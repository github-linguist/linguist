import "@stdlib/tvm-dicts"
import "@stdlib/tvm-lowlevel"
import "@stdlib/gas-payments"

// Restricted wallet initialized by a third party (a variant of restricted-wallet3-code.fc)
// Allows to add more locked budget after initialization

@pure
fun `err:wrong_signature`(): int
    asm "31 PUSHINT";
@pure
fun `err:wrong_config_signature`(): int
    asm "32 PUSHINT";
@pure
fun `err:value_is_too_small`(): int
    asm "33 PUSHINT";
@pure
fun `err:wrong_seqno`(): int
    asm "34 PUSHINT";
@pure
fun `err:wrong_subwallet_id`(): int
    asm "35 PUSHINT";
@pure
fun `err:replay_protection`(): int
    asm "36 PUSHINT";
@pure
fun `err:unknown_op`(): int
    asm "40 PUSHINT";
@pure
fun `err:unknown_cmd`(): int
    asm "41 PUSHINT";

@pure
fun `op:rwallet_op`(): int
    asm "0x82eaf9c4 PUSHINT";
@pure
fun `cmd:restricted_transfer`(): int
    asm "0x373aa9f4 PUSHINT";

fun isWhitelisted(addr: auto, allowedDestinations: auto) {
    var (_, _, _, found: int) = allowedDestinations.prefixDictGet(addr.getRemainingBitsCount(), addr);
    return found;
}

fun loadUint32(mutate self: slice): int {
    return self.loadUint(32);
}

@inline_ref
fun checkMessageDestination(msg: auto, allowedDestinations: auto) {
    var cs = msg.beginParse();
    var flags = cs.loadUint(4);
    if (flags & 8) {
        // external messages are always valid
        return true;
    }
    var (sAddr, dAddr) = (cs.loadAddress(), cs.loadAddress());

    return isWhitelisted(dAddr, allowedDestinations);
}

fun unpackData() {
    var cs = getContractData().beginParse();
    var res = (
        cs.loadUint32(),
        cs.loadUint(32),
        cs.loadUint(256),
        cs.loadUint(256),
        cs.loadDict(),
        cs.loadCoins(),
        cs.loadDict(),
        cs.loadCoins(),
        cs.loadDict()
    );
    cs.assertEndOfSlice();
    return res;
}

fun packData(seqno: int, subwalletId: int, publicKey: int, configPublicKey: int, allowedDestinations: cell, totalLockedValue: int, locked: cell, totalRestrictedValue: int, restricted: cell) {
    return beginCell()
        .storeInt(seqno, 32)
        .storeInt(subwalletId, 32)
        .storeUint(publicKey, 256)
        .storeUint(configPublicKey, 256)
        .storeDict(allowedDestinations)
        .storeCoins(totalLockedValue)
        .storeDict(locked)
        .storeCoins(totalRestrictedValue)
        .storeDict(restricted).endCell();
}

fun lockGrams(locked: cell, total: int, ts: int, value: int): (cell, int) {
    total += value;
    var (foundCs: slice, found: auto) = locked.uDictGet(32, ts);
    if (found) {
        var foundValue = foundCs.loadCoins();
        foundCs.assertEndOfSlice();
        value += foundValue;
    }
    locked.uDictSetBuilder(32, ts, beginCell().storeCoins(value));
    locked.uDictDeleteFirstAndGet(32);
    return (locked, total);
}

fun unlockGrams(locked: cell, total: int, nowTs: int): (cell, int) {
    do {
        var (`locked'`, ts, valueCs, f) = locked.uDictDeleteFirstAndGet(32);
        f.stackMoveToTop();
        if (f) {
            f = ts <= nowTs;
        }
        if (f) {
            locked = `locked'`;
            var value: int = valueCs.loadCoins();
            valueCs.assertEndOfSlice();
            total -= value;
        }
    } while (!(~ f));
    return (locked, total);
}

fun onInternalMessage(msgValue: int, inMsgCell: cell, inMsg: slice) {
    var cs = inMsgCell.beginParse();
    var flags = cs.loadUint(4);  // int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool
    if (flags & 1) {
        // ignore all bounced messages
        return;
    }
    var sAddr = cs.loadAddress();
    if (inMsg.isEndOfSlice()) {
        return;
    }
    var op: int = inMsg.loadUint32();
    if (op <= 1) {
        // simple transfer with comment, return
        return;
    }
    var (storedSeqno, storedSubwallet, publicKey, configPublicKey, allowedDestinations, totalLockedValue, locked,
        totalRestrictedValue, restricted) = unpackData();

    if (isWhitelisted(sAddr, allowedDestinations) & (op != `op:rwallet_op`())) {
        return;
    }

    assert(op == `op:rwallet_op`()) throw `err:unknown_op`();
    assert(msgValue >= 1000000000) throw `err:value_is_too_small`();



    var signature = inMsg.loadBits(512);
    assert(isSignatureValid(sliceHash(inMsg), signature, configPublicKey)) throw `err:wrong_config_signature`();
    var cmd: int = inMsg.loadUint(32);
    assert(cmd == `cmd:restricted_transfer`()) throw `err:unknown_cmd`();
    var (onlyRestrict, ts) = (inMsg.loadUint(1), inMsg.loadUint32());
    if (onlyRestrict) {
        (restricted, totalRestrictedValue) = lockGrams(restricted, totalRestrictedValue, ts, msgValue);
    } else if(true /* false */) {
        (locked, totalLockedValue) = lockGrams(locked, totalLockedValue, ts, msgValue);
    }

    setContractData(packData(storedSeqno, storedSubwallet, publicKey, configPublicKey, allowedDestinations, totalLockedValue, locked,
        totalRestrictedValue, restricted));
}

fun onExternalMessage(inMsg: slice) {
    var signature = inMsg.loadBits(512);
    var cs = inMsg;
    var (subwalletId, validUntil, msgSeqno) = (cs.loadUint(32), cs.loadUint32(), cs.loadUint(32));
    assert(!(validUntil <= now())) throw `err:replay_protection`();
    var (storedSeqno, storedSubwallet, publicKey, configPublicKey, allowedDestinations, totalLockedValue, locked,
        totalRestrictedValue, restricted) = unpackData();
    assert(msgSeqno == storedSeqno) throw `err:wrong_seqno`();
    assert(subwalletId == storedSubwallet) throw `err:wrong_subwallet_id`();
    assert(isSignatureValid(sliceHash(inMsg), signature, publicKey)) throw `err:wrong_signature`();
    startPayingGasFromMyBalance();

    (restricted, totalRestrictedValue) = unlockGrams(restricted, totalRestrictedValue, now());
    (locked, totalLockedValue) = unlockGrams(locked, totalLockedValue, now());
    var effectivelyLocked: int = totalLockedValue;
    var canUseRestricted: int = 1;
    var csCopy = cs;
    while (csCopy.getRemainingRefsCount()) {
        var mode = csCopy.loadUint(8);
        var msg = csCopy.loadRef();
        canUseRestricted &= checkMessageDestination(msg, allowedDestinations);
    }

    if (!canUseRestricted) {
        effectivelyLocked += totalRestrictedValue;
    }
    reserveToncoinsOnBalance(effectivelyLocked, 2);

    cs.stackMoveToTop();
    while (cs.getRemainingRefsCount()) {
        var mode = cs.loadUint(8);
        var msg = cs.loadRef();
        sendMessage(msg, mode);
    }
    cs.assertEndOfSlice();

    setContractData(packData(storedSeqno + 1, storedSubwallet, publicKey, configPublicKey, allowedDestinations, totalLockedValue, locked,
        totalRestrictedValue, restricted));
}

// Get methods

get seqno(): int {
    return getContractData().beginParse().preloadUint(32);
}

get wallet_id(): int {
    var ds = getContractData().beginParse();
    ds.loadUint32();
    return ds.preloadUint(32);
}

get get_public_key(): int {
    var ds = getContractData().beginParse();
    ds.loadUint(32 + 32);
    return ds.preloadUint(256);
}

// the next three methods are mostly for testing

get get_balances_at(time: int): (int, int, int) {
    var (storedSeqno, storedSubwallet, publicKey, configPublicKey, allowedDestinations, totalLockedValue, locked,
        totalRestrictedValue, restricted) = unpackData();
    (restricted, totalRestrictedValue) = unlockGrams(restricted, totalRestrictedValue, time);
    (locked, totalLockedValue) = unlockGrams(locked, totalLockedValue, time);
    var tonBalance: int = getMyStartBalance();
    try { } catch { }
    return ( tonBalance,
        totalRestrictedValue,
        totalLockedValue );
}

@method_id(123)
fun get_balances(): (int, int, int) {
    return get_balances_at(now());
}

get check_destination(destination: slice): int {
    var (storedSeqno, storedSubwallet, publicKey, configPublicKey, allowedDestinations, totalLockedValue, locked,
        totalRestrictedValue, restricted) = unpackData();
    return isWhitelisted(destination, allowedDestinations);
}
