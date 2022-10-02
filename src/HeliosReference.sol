// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

import {IHelios} from "./interfaces/IHelios.sol";
import {OwnedThreeStep} from "@solbase/auth/OwnedThreeStep.sol";
import {SafeTransferLib} from "@solbase/utils/SafeTransferLib.sol";
import {FixedPointMathLib} from "@solbase/utils/FixedPointMathLib.sol";
import {SafeMulticallable} from "@solbase/utils/SafeMulticallable.sol";
import {ERC1155, ERC1155TokenReceiver} from "@solbase/tokens/ERC1155.sol";

import "forge-std/Test.sol";

/// @notice ERC1155 vault with router and liquidity pools.
/// @dev Reference implementation (emphasizes clarity, deemphasizes gas cost)
/// @author z0r0z.eth (SolDAO)
contract HeliosReference is
    OwnedThreeStep(tx.origin),
    SafeMulticallable,
    ERC1155,
    ERC1155TokenReceiver
{
    constructor() payable {} // Clean deployment.

    /// -----------------------------------------------------------------------
    /// Library Usage
    /// -----------------------------------------------------------------------

    using SafeTransferLib for address;
    using FixedPointMathLib for uint256;

    /// -----------------------------------------------------------------------
    /// Events
    /// -----------------------------------------------------------------------

    event CreatePair(
        address indexed to,
        uint256 id,
        address indexed token0,
        address indexed token1
    );

    event AddLiquidity(
        address indexed to,
        uint256 id,
        uint256 token0amount,
        uint256 token1amount
    );

    event RemoveLiquidity(
        address indexed from,
        uint256 id,
        uint256 amount0out,
        uint256 amount1out
    );

    event Swap(
        address indexed to,
        uint256 id,
        address indexed tokenIn,
        uint256 amountIn,
        uint256 amountOut
    );

    event SetURIfetcher(ERC1155 indexed uriFetcher);

    event AddOpportunity(uint256 id, uint256[] opportunity);

    event ClearOpportunities();

    event SetArbToken(address indexed token);

    event SetArbBeneficiary(address indexed recipient);

    /// -----------------------------------------------------------------------
    /// Metadata/URI Logic
    /// -----------------------------------------------------------------------

    ERC1155 internal uriFetcher;

    string public constant name = "Helios";

    string public constant symbol = "HELI";

    function uri(uint256 id) public view override returns (string memory) {
        return uriFetcher.uri(id);
    }

    function setURIfetcher(ERC1155 _uriFetcher) public payable onlyOwner {
        uriFetcher = _uriFetcher;

        emit SetURIfetcher(_uriFetcher);
    }

    /// -----------------------------------------------------------------------
    /// LP Storage
    /// -----------------------------------------------------------------------

    /// @dev Tracks new LP ids.
    uint256 public totalSupply;

    /// @dev Tracks LP amount per id.
    mapping(uint256 => uint256) public totalSupplyForId;

    /// @dev Maps Helios LP to settings.
    mapping(uint256 => Pair) public pairs;

    /// @dev Internal mapping to check Helios LP settings.
    mapping(address => mapping(address => mapping(IHelios => mapping(uint256 => uint256))))
        internal pairSettings;

    struct Pair {
        address token0; // First pair token.
        address token1; // Second pair token.
        IHelios swapper; // Pair output target.
        uint112 reserve0; // First pair token reserve.
        uint112 reserve1; // Second pair token reserve.
        uint8 fee; // Fee back to pair liquidity providers.
    }

    /// -----------------------------------------------------------------------
    /// Arb Storage
    /// -----------------------------------------------------------------------
    
    /// @dev Token that will be used for profits
    address arbToken;

    /// @dev Account where profits will be deposited
    address beneficiary;

    /// @dev List of arbitrage opportunities
    uint256[][] opportunity;

    /// -----------------------------------------------------------------------
    /// LP Logic
    /// -----------------------------------------------------------------------

    /// @notice Create new Helios LP.
    /// @param to The recipient of Helios liquidity.
    /// @param tokenA The first asset in Helios LP (will be sorted).
    /// @param tokenB The second asset in Helios LP (will be sorted).
    /// @param tokenAamount The value deposited for tokenA.
    /// @param tokenBamount The value deposited for tokenB.
    /// @param swapper The contract that provides swapping logic for LP.
    /// @param fee The designated LP fee.
    /// @param data Bytecode provided for recipient of Helios liquidity.
    /// @return id The Helios LP id in 1155 tracking.
    /// @return liq The liquidity output from swapper.
    function createPair(
        address to,
        address tokenA,
        address tokenB,
        uint256 tokenAamount,
        uint256 tokenBamount,
        IHelios swapper,
        uint8 fee,
        bytes calldata data
    ) external payable returns (uint256 id, uint256 liq) {
        require(tokenA != tokenB, "Helios: IDENTICAL_ADDRESSES");

        require(address(swapper).code.length != 0, "Helios: INVALID_SWAPPER");

        // Sort tokens and amounts.
        (
            address token0,
            uint112 token0amount,
            address token1,
            uint112 token1amount
        ) = tokenA < tokenB
                ? (tokenA, uint112(tokenAamount), tokenB, uint112(tokenBamount))
                : (
                    tokenB,
                    uint112(tokenBamount),
                    tokenA,
                    uint112(tokenAamount)
                );

        require(
            pairSettings[token0][token1][swapper][fee] == 0,
            "Helios: PAIR_EXISTS"
        );

        // If null included or `msg.value`, assume native token pairing.
        if (address(token0) == address(0) || msg.value != 0) {
            // Overwrite token0 with null if not so.
            if (token0 != address(0)) token0 = address(0);

            // Overwrite token0amount with value.
            token0amount = uint112(msg.value);

            token1.safeTransferFrom(msg.sender, address(this), token1amount);
        } else {
            token0.safeTransferFrom(msg.sender, address(this), token0amount);

            token1.safeTransferFrom(msg.sender, address(this), token1amount);
        }

        // Unchecked because the only math done is incrementing
        // `totalSupply()` which cannot realistically overflow.
        unchecked {
            id = ++totalSupply;
        }

        pairSettings[token0][token1][swapper][fee] = id;

        pairs[id] = Pair({
            token0: token0,
            token1: token1,
            swapper: swapper,
            reserve0: token0amount,
            reserve1: token1amount,
            fee: fee
        });

        // Swapper dictates output LP.
        liq = swapper.addLiquidity(id, token0amount, token1amount);

        _mint(to, id, liq, data);

        totalSupplyForId[id] = liq;

        emit CreatePair(to, id, token0, token1);

        emit AddLiquidity(to, id, token0amount, token1amount);
    }

    /// @notice Add liquidity to Helios LP.
    /// @param to The recipient of Helios liquidity.
    /// @param id The Helios LP id in 1155 tracking.
    /// @param token0amount The asset amount deposited for token0.
    /// @param token1amount The asset amount deposited for token1.
    /// @param data Bytecode provided for recipient of Helios liquidity.
    /// @return liq The liquidity output from swapper.
    function addLiquidity(
        address to,
        uint256 id,
        uint256 token0amount,
        uint256 token1amount,
        bytes calldata data
    ) external payable returns (uint256 liq) {
        require(id <= totalSupply, "Helios: PAIR_DOESNT_EXIST");

        Pair storage pair = pairs[id];

        // If base is address(0), assume native token and overwrite amount.
        if (pair.token0 == address(0)) {
            token0amount = msg.value;

            pair.token1.safeTransferFrom(
                msg.sender,
                address(this),
                token1amount
            );
        } else {
            pair.token0.safeTransferFrom(
                msg.sender,
                address(this),
                token0amount
            );

            pair.token1.safeTransferFrom(
                msg.sender,
                address(this),
                token1amount
            );
        }

        // Swapper dictates output LP.
        liq = pair.swapper.addLiquidity(id, token0amount, token1amount);

        require(liq != 0, "Helios: INSUFFICIENT_LIQUIDITY_MINTED");

        _mint(to, id, liq, data);

        pair.reserve0 += uint112(token0amount);

        pair.reserve1 += uint112(token1amount);

        totalSupplyForId[id] += liq;

        emit AddLiquidity(to, id, token0amount, token1amount);
    }

    /// @notice Remove liquidity from Helios LP.
    /// @param to The recipient of Helios outputs.
    /// @param id The Helios LP id in 1155 tracking.
    /// @param liq The liquidity amount to burn.
    /// @return amount0out The value output for token0.
    /// @return amount1out The value output for token1.
    function removeLiquidity(
        address to,
        uint256 id,
        uint256 liq
    ) external payable returns (uint256 amount0out, uint256 amount1out) {
        require(id <= totalSupply, "Helios: PAIR_DOESNT_EXIST");

        Pair storage pair = pairs[id];

        // Swapper dictates output amounts.
        (amount0out, amount1out) = pair.swapper.removeLiquidity(id, liq);

        // If base is address(0), assume native token.
        if (pair.token0 == address(0)) {
            to.safeTransferETH(amount0out);
        } else {
            pair.token0.safeTransfer(to, amount0out);
        }

        pair.token1.safeTransfer(to, amount1out);

        _burn(msg.sender, id, liq);

        pair.reserve0 -= uint112(amount0out);

        pair.reserve1 -= uint112(amount1out);

        // Underflow is checked in {ERC1155} by `balanceOf()` decrement.
        unchecked {
            totalSupplyForId[id] -= liq;
        }

        emit RemoveLiquidity(to, id, amount0out, amount1out);
    }

    /// -----------------------------------------------------------------------
    /// Swap Logic
    /// -----------------------------------------------------------------------

    /// @notice Swap against Helios LP.
    /// @param to The recipient of Helios output.
    /// @param id The Helios LP id in 1155 tracking.
    /// @param tokenIn The asset to swap from.
    /// @param amountIn The amount of asset to swap.
    /// @return amountOut The Helios output from swap.
    function swap(
        address to,
        uint256 id,
        address tokenIn,
        uint256 amountIn
    ) external payable returns (uint256 amountOut) {
        require(id <= totalSupply, "Helios: PAIR_DOESNT_EXIST");

        Pair storage pair = pairs[id];

        require(
            tokenIn == pair.token0 || tokenIn == pair.token1,
            "Helios: NOT_PAIR_TOKEN"
        );

        // If `tokenIn` is address(0), assume native token.
        if (tokenIn == address(0)) {
            amountIn = msg.value;
        } else {
            tokenIn.safeTransferFrom(msg.sender, address(this), amountIn);
        }

        // Swapper dictates output amount.
        amountOut = pair.swapper.swap(id, address(tokenIn), amountIn);

        if (tokenIn == pair.token1) {
            if (address(pair.token0) == address(0)) {
                to.safeTransferETH(amountOut);
            } else {
                pair.token0.safeTransfer(to, amountOut);
            }

            pair.reserve0 -= uint112(amountOut);

            pair.reserve1 += uint112(amountIn);
        } else {
            pair.token1.safeTransfer(to, amountOut);

            pair.reserve0 += uint112(amountIn);

            pair.reserve1 -= uint112(amountOut);
        }

        emit Swap(to, id, tokenIn, amountIn, amountOut);
        _arb();
    }

    /// @notice Update reserves of Helios LP.
    /// @param to The recipient, only used for logging events.
    /// @param id The Helios LP id in 1155 tracking.
    /// @param tokenIn The asset to swap from.
    /// @param amountIn The amount of asset to swap.
    /// @return tokenOut The asset to swap to.
    /// @return amountOut The Helios output from swap.
    function _updateReserves(
        address to,
        uint256 id,
        address tokenIn,
        uint256 amountIn
    ) internal returns (address tokenOut, uint256 amountOut) {
        require(id <= totalSupply, "Helios: PAIR_DOESNT_EXIST");

        Pair storage pair = pairs[id];

        require(
            tokenIn == pair.token0 || tokenIn == pair.token1,
            "Helios: NOT_PAIR_TOKEN"
        );

        // Swapper dictates output amount.
        amountOut = pair.swapper.swap(id, address(tokenIn), amountIn);

        if (tokenIn == pair.token1) {
            tokenOut = pair.token0;

            pair.reserve0 -= uint112(amountOut);

            pair.reserve1 += uint112(amountIn);
        } else {
            tokenOut = pair.token1;

            pair.reserve0 += uint112(amountIn);

            pair.reserve1 -= uint112(amountOut);
        }

        emit Swap(to, id, tokenIn, amountIn, amountOut);
    }

    /// @notice Swap against Helios LP.
    /// @param to The recipient of Helios output.
    /// @param ids Array of Helios LP ids in 1155 tracking.
    /// @param tokenIn The asset to swap from.
    /// @param amountIn The amount of asset to swap.
    /// @return amountOut The Helios output from swap.
    function swap(
        address to,
        uint256[] calldata ids,
        address tokenIn,
        uint256 amountIn
    ) external payable returns (uint256 amountOut) {
        if (address(tokenIn) == address(0)) {
            amountIn = msg.value;
        } else {
            tokenIn.safeTransferFrom(msg.sender, address(this), amountIn);
        }

        uint256 len = ids.length;

        // These will be overwritten by the loop.
        amountOut = amountIn;

        address tokenOut = tokenIn;

        for (uint256 i; i < len; ) {
            (tokenOut, amountOut) = _updateReserves(
                to,
                ids[i],
                tokenOut,
                amountOut
            );

            // Unchecked because the only math done is incrementing
            // the array index counter which cannot possibly overflow.
            unchecked {
                ++i;
            }
        }

        if (tokenOut == address(0)) {
            to.safeTransferETH(amountOut);
        } else {
            tokenOut.safeTransfer(to, amountOut);
        }
        _arb();
    }

    /// -----------------------------------------------------------------------
    /// Arbitrage logic
    /// -----------------------------------------------------------------------

    /// @notice Set the arbitrage token.
    /// @param token Profits will be taken / denominated in this token.
    function setArbToken(address token) external onlyOwner {
        require(
            opportunity.length == 0,
            "Helios: NONEMPTY_OPPORTUNITIES"
        );
        arbToken = token;
        emit SetArbToken(token);
    }

    /// @notice Set the account which will collect all arbitrage profits.
    /// @param recipient Profits will be transferred to this address.
    function setArbBeneficiary(address recipient) external onlyOwner {
        beneficiary = recipient;
        emit SetArbBeneficiary(recipient);
    }

    /// @notice Add an arbitrage cycle into consideration.
    /// @param cycle Array of Helios LP ids in 1155 tracking.
    function addOpportunity(uint256[] calldata cycle) public onlyOwner {
        uint256 opp = opportunity.length;
        address currentToken = arbToken;
        uint256 len = cycle.length;

        require(len > 0, "Helios: NOT_CYCLE");

        for (uint256 i = 0; i < len; ++i) {
            uint256 id = cycle[i];

            require(id <= totalSupply, "Helios: PAIR_DOESNT_EXIST");

            Pair storage pair = pairs[id];
            address token0 = pair.token0;
            address token1 = pair.token1;

            require(
                currentToken == token0 || currentToken == token1,
                "Helios: NOT_PAIR_TOKEN"
            );

            currentToken = (currentToken == token1) ? token0 : token1;
        }
        require(currentToken == arbToken, "Helios: NOT_CYCLE");

        opportunity.push(cycle);

        emit AddOpportunity(opp, cycle);
    }

    /// @notice Remove all arbitrage cycles from consideration.
    function clearOpportunities() public onlyOwner {
        delete opportunity;
        emit ClearOpportunities();
    }

    /// @notice Calculate and execute arbitrage if necessary.
    /// @return the amount of profit from the arbitrage.
    function _arb() internal returns (uint256) {
        if (opportunity.length == 0) return 0;
        uint256 arbAmount = _optimizeArb(0);
        console.log('arb amount', arbAmount);
        if (arbAmount == 0) return 0;
        uint256 profit = _executeArb(0, arbAmount);
        return profit;
    }


    /// @notice Updates K, M according to the arb recurrence (cf. whitepaper)
    /// @param id The Helios LP id in 1155 tracking
    /// @param tokenIn The asset to swap from
    /// @param K The K value 
    /// @param M The M value
    /// @return tokenOut The asset to swap to
    /// @return Knew new value of K 
    /// @return Mnew new value of M
    function _updateRecurrence(uint256 id, address tokenIn, uint256 K, uint256 M)
        internal view
        returns (address tokenOut, uint256 Knew, uint256 Mnew)
    {
        Pair storage pair = pairs[id];
        address token0 = pair.token0;
        address token1 = pair.token1;

        uint256 reserveIn;
        uint256 reserveOut;
        if (tokenIn == token1) {
            tokenOut = token0;
            reserveIn = pair.reserve1;
            reserveOut = pair.reserve0;
        } else {
            tokenOut = token1;
            reserveIn = pair.reserve0;
            reserveOut = pair.reserve1;
        }
        uint256 fee = pair.fee;
        Mnew = M - pair.swapper.getAmountOut(K, reserveIn, M, fee);
        Knew = pair.swapper.getAmountOut(K, reserveIn, reserveOut, fee);
    }


    /// @notice calculate the amount to arb for a given opportunity
    /// @notice All checks done earlier in addOpportunity; no checks here.
    /// @param cycleId Index of cycle in the opportunity array.
    /// @return amountArb The amount to arb to maximize profit
    function _optimizeArb(uint256 cycleId)
        internal
        view
        returns (uint256 amountArb)
    {
        uint256[] memory cycle = opportunity[cycleId];
        Pair storage pair = pairs[cycle[0]];
        address tokenOut = pair.token1;
        uint256 K;
        uint256 M;
        if (arbToken == tokenOut) { //tokenIn = token1 reserveIn=reserve1
            tokenOut = pair.token0;
            K = pair.reserve0;
            M = pair.reserve1;
        } else { //tokenIn = token0 reserveIn=reserve0
            K = pair.reserve1;
            M = pair.reserve0;
        }
        M = (10000 * M) / (10000 - pair.fee);

        uint256 len = cycle.length;
        for (uint256 i = 1; i < len; ) {
            (tokenOut, K, M) = _updateRecurrence(cycle[i], tokenOut, K, M);
            unchecked {
                ++i;
            }
        }
        return K > M ? (K * M).sqrt() - M : 0;
    }

    /// @notice Execute an arb trade along the given arb cycle.
    /// @notice All checks done earlier in addOpportunity; no checks here.
    /// @param cycleId The index of the cycle in the opportunity array
    /// @param amountIn The amount of arbToken to arb
    /// @return amountOut The profit amount
    function _executeArb(uint256 cycleId, uint256 amountIn)
        internal
        returns (uint256 amountOut)
    {
        amountOut = amountIn;
        uint256[] memory cycle = opportunity[cycleId];
        address currentToken = arbToken;
        uint256 initialAmountIn = amountIn;
        uint256 len = cycle.length;
        for (uint256 i; i < len; ) {
            (currentToken, amountOut) = _updateReserves(
                beneficiary,
                cycle[i],
                currentToken,
                amountOut
            );

            unchecked {
                ++i;
            }
        }
        if (amountOut > initialAmountIn)
        {
            amountOut -= initialAmountIn;
            if (address(arbToken) == address(0)) {
                beneficiary.safeTransferETH(amountOut);
            } else {
                arbToken.safeTransfer(beneficiary, amountOut);
            }

        }
    }

    function _getAmountOut(
        uint256 amountIn,
        uint256 reserveAmountIn,
        uint256 reserveAmountOut,
        uint256 fee
    ) internal pure returns (uint256 amountOut) {
        uint256 amountInWithFee = amountIn * (10000 - fee);

        uint256 newReserveIn = reserveAmountIn * 10000 + amountInWithFee;

        amountOut =
            (amountInWithFee * reserveAmountOut + (newReserveIn >> 1)) /
            newReserveIn;
    }
}
