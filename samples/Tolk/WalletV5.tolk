import "@stdlib/gas-payments"
import "c5-register-validation"
import "errors"
import "messages"
import "storage"

contract WalletV5 {
    author: "The Tolk Team"
    version: "5.1"
    description: "Wallet V5. Extensible wallet with plugins support"
    storage: Storage
    incomingMessages: AllowedMessageToWalletV5
    incomingExternal: AllowedExternalMessageToWalletV5
}

const SIZE_SIGNATURE = 512

@inline_ref
fun processExtraActions(extraActions: SnakedExtraActions, isExtension: bool) {
    while (true) {
        val action = lazy ExtraAction.fromSlice(extraActions);
        match (action) {
            AddExtensionExtraAction => {
                val (extensionWorkchain, extensionAddrHash) = action.addr.getWorkchainAndHash();
                val myWorkchain = contract.getAddress().getWorkchain();
                assert (myWorkchain == extensionWorkchain) throw Errors.ExtensionWrongWorkchain;

                var storage = lazy Storage.load();
                val inserted = storage.extensions.addIfNotExists(extensionAddrHash, true);
                assert (inserted) throw Errors.AddExtension;
                storage.save();
            }

            RemoveExtensionExtraAction => {
                val (extensionWorkchain, extensionAddrHash) = action.addr.getWorkchainAndHash();
                val myWorkchain = contract.getAddress().getWorkchain();
                assert (myWorkchain == extensionWorkchain) throw Errors.ExtensionWrongWorkchain;

                var storage = lazy Storage.load();
                val removed = storage.extensions.delete(extensionAddrHash);
                assert (removed) throw Errors.RemoveExtension;
                if (!storage.isSignatureAllowed) {
                    assert (
                        !storage.extensions.isEmpty()
                    ) throw Errors.RemoveLastExtensionWhenSignatureDisabled;
                }
                storage.save();
            }

            SetSignatureAllowedExtraAction => {
                assert (isExtension) throw Errors.OnlyExtensionCanChangeSignatureMode;

                var storage = lazy Storage.load();
                assert (
                    storage.isSignatureAllowed != action.allowSignature
                ) throw Errors.ThisSignatureModeAlreadySet;
                if (!action.allowSignature) {
                    assert (
                        !storage.extensions.isEmpty()
                    ) throw Errors.DisableSignatureWhenExtensionsIsEmpty;
                }
                storage.isSignatureAllowed = action.allowSignature;
                storage.save();
            }

            else => throw Errors.UnsupportedAction,
        }
        if (!extraActions.hasNext()) {
            return;
        }
        extraActions = extraActions.getNext();
    }
}

fun processActions(
    outActions: OutActionsCell?,
    hasExtraActions: bool,
    extraActions: SnakedExtraActions,
    isExternal: bool,
    isExtension: bool,
) {
    if (outActions != null) {
        // Simply set the C5 register with all pre-computed actions after verification:
        setTvmRegisterC5(outActions.verifyC5Actions(isExternal));
    }
    if (hasExtraActions) {
        processExtraActions(extraActions, isExtension);
    }
}

type AllowedMessageToWalletV5 = ExtensionActionRequest | InternalSignedRequest

type AllowedExternalMessageToWalletV5 = ExternalSignedRequest

fun onExternalMessage(inMsgBody: slice) {
    val signature = inMsgBody.getLastBits(SIZE_SIGNATURE);
    val signedSlice = inMsgBody.removeLastBits(SIZE_SIGNATURE);

    val msg = AllowedExternalMessageToWalletV5.fromSlice(
        signedSlice,
        { throwIfOpcodeDoesNotMatch: Errors.InvalidMessageOperation },
    );

    var storage = lazy Storage.load();

    assert (
        isSignatureValid(signedSlice.hash(), signature, storage.publicKey)
    ) throw Errors.InvalidSignature;
    assert (
        storage.isSignatureAllowed | storage.extensions.isEmpty()
    ) throw Errors.SignatureDisabled;
    assert (msg.seqno == storage.seqno) throw Errors.InvalidSeqno;
    assert (msg.walletId == storage.subwalletId) throw Errors.InvalidWalletId;
    assert (msg.validUntil > blockchain.now()) throw Errors.Expired;

    acceptExternalMessage();

    // Store and commit the seqno increment to prevent replays even if the subsequent requests fail.
    storage.isSignatureAllowed = true;
    storage.seqno += 1;
    storage.save();

    commitContractDataAndActions();
    processActions(msg.outActions, msg.hasExtraActions, msg.extraActions, true, false);
}

@on_bounced_policy("manual")
fun onInternalMessage(in: InMessage) {
    val msg = lazy AllowedMessageToWalletV5.fromSlice(in.body);

    match (msg) {
        InternalSignedRequest => {
            val bodyLongEnough = in.body.remainingBitsCount() >= SIZE_SIGNATURE + 128;
            if (!bodyLongEnough) {
                return; // don't try to parse it (it will fail), just ignore
            }

            val signature = in.body.getLastBits(SIZE_SIGNATURE);
            val signedSlice = in.body.removeLastBits(SIZE_SIGNATURE);

            var storage = lazy Storage.load();
            if (!isSignatureValid(signedSlice.hash(), signature, storage.publicKey)) {
                return;
            }

            assert (
                storage.isSignatureAllowed | storage.extensions.isEmpty()
            ) throw Errors.SignatureDisabled;
            assert (msg.seqno == storage.seqno) throw Errors.InvalidSeqno;
            assert (msg.walletId == storage.subwalletId) throw Errors.InvalidWalletId;
            assert (msg.validUntil > blockchain.now()) throw Errors.Expired;

            // Store and commit the seqno increment to prevent replays even if the subsequent requests fail.
            storage.isSignatureAllowed = true;
            storage.seqno += 1;
            storage.save();

            processActions(msg.outActions, msg.hasExtraActions, msg.extraActions, false, false);
        }

        ExtensionActionRequest => {
            val (senderWorkchain, senderAddrHash) = in.senderAddress.getWorkchainAndHash();
            val myWorkchain = contract.getAddress().getWorkchain();
            if (myWorkchain != senderWorkchain) {
                return;
            }

            val storage = lazy Storage.load();
            if (!storage.extensions.exists(senderAddrHash)) {
                // Note that some random contract may have deposited funds with this prefix,
                // so we accept the funds silently instead of throwing an error (wallet v4 does the same).
                return;
            }

            processActions(msg.outActions, msg.hasExtraActions, msg.extraActions, false, true);
        }

        else => {
            // if opcode is strange or body is empty at all (for instance, bounced messages)
            // just ignore, do not throw
        }
    }
}

get fun is_signature_allowed(): bool {
    val storage = lazy Storage.load();
    return storage.isSignatureAllowed;
}

get fun seqno(): int {
    val storage = lazy Storage.load();
    return storage.seqno;
}

get fun get_subwallet_id(): int {
    val storage = lazy Storage.load();
    return storage.subwalletId;
}

get fun get_public_key(): int {
    val storage = lazy Storage.load();
    return storage.publicKey;
}

// Returns raw dictionary (or null if empty) where keys are address hashes. Workchains of extensions are same with wallet smart contract workchain
get fun get_extensions(): map<uint256, bool> {
    val storage = lazy Storage.load();
    return storage.extensions;
}
