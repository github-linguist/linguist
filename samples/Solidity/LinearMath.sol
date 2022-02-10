// SPDX-License-Identifier: GPL-3.0-or-later
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

pragma solidity ^0.7.0;

import "@balancer-labs/v2-solidity-utils/contracts/math/Math.sol";
import "@balancer-labs/v2-solidity-utils/contracts/math/FixedPoint.sol";

// These functions start with an underscore, as if they were part of a contract and not a library. At some point this
// should be fixed.
// solhint-disable private-vars-leading-underscore

library LinearMath {
    using FixedPoint for uint256;

    // A thorough derivation of the formulas and derivations found here exceeds the scope of this file, so only
    // introductory notions will be presented.

    // A Linear Pool holds three tokens: the main token, the wrapped token, and the Pool share token (BPT). It is
    // possible to exchange any of these tokens for any of the other two (so we have three trading pairs) in both
    // directions (the first token of each pair can be bought or sold for the second) and by specifying either the input
    // or output amount (typically referred to as 'given in' or 'given out'). A full description thus requires
    // 3*2*2 = 12 functions.
    // Wrapped tokens have a known, trusted exchange rate to main tokens. All functions here assume such a rate has
    // already been applied, meaning main and wrapped balances can be compared as they are both expressed in the same
    // units (those of main token).
    // Additionally, Linear Pools feature a lower and upper target that represent the desired range of values for the
    // main token balance. Any action that moves the main balance away from this range is charged a proportional fee,
    // and any action that moves it towards this range is incentivized by paying the actor using these collected fees.
    // The collected fees are not stored in a separate data structure: they are a function of the current main balance,
    // targets and fee percentage. The main balance sans fees is known as the 'nominal balance', which is always smaller
    // than the real balance except when the real balance is within the targets.
    // The rule under which Linear Pools conduct trades between main and wrapped tokens is by keeping the sum of nominal
    // main balance and wrapped balance constant: this value is known as the 'invariant'. BPT is backed by nominal
    // reserves, meaning its supply is proportional to the invariant. As the wrapped token appreciates in value and its
    // exchange rate to the main token increases, so does the invariant and thus the value of BPT (in main token units).

    struct Params {
        uint256 fee;
        uint256 lowerTarget;
        uint256 upperTarget;
    }

    function _calcBptOutPerMainIn(
        uint256 mainIn,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount out, so we round down overall.

        if (bptSupply == 0) {
            // BPT typically grows in the same ratio the invariant does. The first time liquidity is added however, the
            // BPT supply is initialized to equal the invariant (which in this case is just the nominal main balance as
            // there is no wrapped balance).
            return _toNominal(mainIn, params);
        }

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 afterNominalMain = _toNominal(mainBalance.add(mainIn), params);
        uint256 deltaNominalMain = afterNominalMain.sub(previousNominalMain);
        uint256 invariant = _calcInvariant(previousNominalMain, wrappedBalance);
        return Math.divDown(Math.mul(bptSupply, deltaNominalMain), invariant);
    }

    function _calcBptInPerMainOut(
        uint256 mainOut,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount in, so we round up overall.

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 afterNominalMain = _toNominal(mainBalance.sub(mainOut), params);
        uint256 deltaNominalMain = previousNominalMain.sub(afterNominalMain);
        uint256 invariant = _calcInvariant(previousNominalMain, wrappedBalance);
        return Math.divUp(Math.mul(bptSupply, deltaNominalMain), invariant);
    }

    function _calcWrappedOutPerMainIn(
        uint256 mainIn,
        uint256 mainBalance,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount out, so we round down overall.

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 afterNominalMain = _toNominal(mainBalance.add(mainIn), params);
        return afterNominalMain.sub(previousNominalMain);
    }

    function _calcWrappedInPerMainOut(
        uint256 mainOut,
        uint256 mainBalance,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount in, so we round up overall.

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 afterNominalMain = _toNominal(mainBalance.sub(mainOut), params);
        return previousNominalMain.sub(afterNominalMain);
    }

    function _calcMainInPerBptOut(
        uint256 bptOut,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount in, so we round up overall.

        if (bptSupply == 0) {
            // BPT typically grows in the same ratio the invariant does. The first time liquidity is added however, the
            // BPT supply is initialized to equal the invariant (which in this case is just the nominal main balance as
            // there is no wrapped balance).
            return _fromNominal(bptOut, params);
        }

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 invariant = _calcInvariant(previousNominalMain, wrappedBalance);
        uint256 deltaNominalMain = Math.divUp(Math.mul(invariant, bptOut), bptSupply);
        uint256 afterNominalMain = previousNominalMain.add(deltaNominalMain);
        uint256 newMainBalance = _fromNominal(afterNominalMain, params);
        return newMainBalance.sub(mainBalance);
    }

    function _calcMainOutPerBptIn(
        uint256 bptIn,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount out, so we round down overall.

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 invariant = _calcInvariant(previousNominalMain, wrappedBalance);
        uint256 deltaNominalMain = Math.divDown(Math.mul(invariant, bptIn), bptSupply);
        uint256 afterNominalMain = previousNominalMain.sub(deltaNominalMain);
        uint256 newMainBalance = _fromNominal(afterNominalMain, params);
        return mainBalance.sub(newMainBalance);
    }

    function _calcMainOutPerWrappedIn(
        uint256 wrappedIn,
        uint256 mainBalance,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount out, so we round down overall.

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 afterNominalMain = previousNominalMain.sub(wrappedIn);
        uint256 newMainBalance = _fromNominal(afterNominalMain, params);
        return mainBalance.sub(newMainBalance);
    }

    function _calcMainInPerWrappedOut(
        uint256 wrappedOut,
        uint256 mainBalance,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount in, so we round up overall.

        uint256 previousNominalMain = _toNominal(mainBalance, params);
        uint256 afterNominalMain = previousNominalMain.add(wrappedOut);
        uint256 newMainBalance = _fromNominal(afterNominalMain, params);
        return newMainBalance.sub(mainBalance);
    }

    function _calcBptOutPerWrappedIn(
        uint256 wrappedIn,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount out, so we round down overall.

        if (bptSupply == 0) {
            // BPT typically grows in the same ratio the invariant does. The first time liquidity is added however, the
            // BPT supply is initialized to equal the invariant (which in this case is just the wrapped balance as
            // there is no main balance).
            return wrappedIn;
        }

        uint256 nominalMain = _toNominal(mainBalance, params);
        uint256 previousInvariant = _calcInvariant(nominalMain, wrappedBalance);

        uint256 newWrappedBalance = wrappedBalance.add(wrappedIn);
        uint256 newInvariant = _calcInvariant(nominalMain, newWrappedBalance);

        uint256 newBptBalance = Math.divDown(Math.mul(bptSupply, newInvariant), previousInvariant);

        return newBptBalance.sub(bptSupply);
    }

    function _calcBptInPerWrappedOut(
        uint256 wrappedOut,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount in, so we round up overall.

        uint256 nominalMain = _toNominal(mainBalance, params);
        uint256 previousInvariant = _calcInvariant(nominalMain, wrappedBalance);

        uint256 newWrappedBalance = wrappedBalance.sub(wrappedOut);
        uint256 newInvariant = _calcInvariant(nominalMain, newWrappedBalance);

        uint256 newBptBalance = Math.divDown(Math.mul(bptSupply, newInvariant), previousInvariant);

        return bptSupply.sub(newBptBalance);
    }

    function _calcWrappedInPerBptOut(
        uint256 bptOut,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount in, so we round up overall.

        if (bptSupply == 0) {
            // BPT typically grows in the same ratio the invariant does. The first time liquidity is added however, the
            // BPT supply is initialized to equal the invariant (which in this case is just the wrapped balance as
            // there is no main balance).
            return bptOut;
        }

        uint256 nominalMain = _toNominal(mainBalance, params);
        uint256 previousInvariant = _calcInvariant(nominalMain, wrappedBalance);

        uint256 newBptBalance = bptSupply.add(bptOut);
        uint256 newWrappedBalance = Math.divUp(Math.mul(newBptBalance, previousInvariant), bptSupply).sub(nominalMain);

        return newWrappedBalance.sub(wrappedBalance);
    }

    function _calcWrappedOutPerBptIn(
        uint256 bptIn,
        uint256 mainBalance,
        uint256 wrappedBalance,
        uint256 bptSupply,
        Params memory params
    ) internal pure returns (uint256) {
        // Amount out, so we round down overall.

        uint256 nominalMain = _toNominal(mainBalance, params);
        uint256 previousInvariant = _calcInvariant(nominalMain, wrappedBalance);

        uint256 newBptBalance = bptSupply.sub(bptIn);
        uint256 newWrappedBalance = Math.divUp(Math.mul(newBptBalance, previousInvariant), bptSupply).sub(nominalMain);

        return wrappedBalance.sub(newWrappedBalance);
    }

    function _calcInvariant(uint256 nominalMainBalance, uint256 wrappedBalance) internal pure returns (uint256) {
        return nominalMainBalance.add(wrappedBalance);
    }

    function _toNominal(uint256 real, Params memory params) internal pure returns (uint256) {
        // Fees are always rounded down: either direction would work but we need to be consistent, and rounding down
        // uses less gas.

        if (real < params.lowerTarget) {
            uint256 fees = (params.lowerTarget - real).mulDown(params.fee);
            return real.sub(fees);
        } else if (real <= params.upperTarget) {
            return real;
        } else {
            uint256 fees = (real - params.upperTarget).mulDown(params.fee);
            return real.sub(fees);
        }
    }

    function _fromNominal(uint256 nominal, Params memory params) internal pure returns (uint256) {
        // Since real = nominal + fees, rounding down fees is equivalent to rounding down real.

        if (nominal < params.lowerTarget) {
            return (nominal.add(params.fee.mulDown(params.lowerTarget))).divDown(FixedPoint.ONE.add(params.fee));
        } else if (nominal <= params.upperTarget) {
            return nominal;
        } else {
            return (nominal.sub(params.fee.mulDown(params.upperTarget)).divDown(FixedPoint.ONE.sub(params.fee)));
        }
    }

    function _calcTokensOutGivenExactBptIn(
        uint256[] memory balances,
        uint256 bptAmountIn,
        uint256 bptTotalSupply,
        uint256 bptIndex
    ) internal pure returns (uint256[] memory) {
        /**********************************************************************************************
        // exactBPTInForTokensOut                                                                    //
        // (per token)                                                                               //
        // aO = tokenAmountOut             /        bptIn         \                                  //
        // b = tokenBalance      a0 = b * | ---------------------  |                                 //
        // bptIn = bptAmountIn             \     bptTotalSupply    /                                 //
        // bpt = bptTotalSupply                                                                      //
        **********************************************************************************************/

        // Since we're computing an amount out, we round down overall. This means rounding down on both the
        // multiplication and division.

        uint256 bptRatio = bptAmountIn.divDown(bptTotalSupply);

        uint256[] memory amountsOut = new uint256[](balances.length);
        for (uint256 i = 0; i < balances.length; i++) {
            // BPT is skipped as those tokens are not the LPs, but rather the preminted and undistributed amount.
            if (i != bptIndex) {
                amountsOut[i] = balances[i].mulDown(bptRatio);
            }
        }

        return amountsOut;
    }
}
