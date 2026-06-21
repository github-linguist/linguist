// Standard library for Tolk (LGPL licence).
// It contains common functions that are available out of the box, the user doesn't have to import anything.
// More specific functions are required to be imported explicitly, like "@stdlib/tvm-dicts".
tolk 0.6

/**
  Tuple manipulation primitives.
  Elements of a tuple can be of arbitrary type.
  Note that atomic type `tuple` can't be cast to composite tuple type (e.g. `[int, cell]`) and vise versa.
 */

/// Creates a tuple with zero elements.
@pure
fun createEmptyTuple(): tuple
    asm "NIL";

/// Appends a value to tuple, resulting in `Tuple t' = (x1, ..., xn, value)`.
/// If its size exceeds 255, throws a type check exception.
@pure
fun tuplePush<X>(mutate self: tuple, value: X): void
    asm "TPUSH";

/// Returns the first element of a non-empty tuple.
@pure
fun tupleFirst<X>(t: tuple): X
    asm "FIRST";

/// Returns the [`index`]-th element of a tuple.
@pure
fun tupleAt<X>(t: tuple, index: int): X
    builtin;

/// Returns the size of a tuple (elements count in it).
@pure
fun tupleSize(t: tuple): int
    asm "TLEN";

/// Returns the last element of a non-empty tuple.
@pure
fun tupleLast(t: tuple): int
    asm "LAST";


/**
    Mathematical primitives.
 */

/// Computes the minimum of two integers.
@pure
fun min(x: int, y: int): int
    asm "MIN";

/// Computes the maximum of two integers.
@pure
fun max(x: int, y: int): int
    asm "MAX";

/// Sorts two integers.
@pure
fun minMax(x: int, y: int): (int, int)
    asm "MINMAX";

/// Computes the absolute value of an integer.
@pure
fun abs(x: int): int
    asm "ABS";

/// Returns the sign of an integer: `-1` if x < 0, `0` if x == 0, `1` if x > 0.
@pure
fun sign(x: int): int
    asm "SGN";

/// Computes the quotient and remainder of [x] / [y]. Example: divMod(112,3) = (37,1)
@pure
fun divMod(x: int, y: int): (int, int)
    asm "DIVMOD";

/// Computes the remainder and quotient of [x] / [y]. Example: modDiv(112,3) = (1,37)
@pure
fun modDiv(x: int, y: int): (int, int)
    asm(-> 1 0) "DIVMOD";

/// Computes multiple-then-divide: floor([x] * [y] / [z]).
/// The intermediate result is stored in a 513-bit integer to prevent precision loss.
@pure
fun mulDivFloor(x: int, y: int, z: int): int
    builtin;

/// Similar to `mulDivFloor`, but rounds the result: round([x] * [y] / [z]).
@pure
fun mulDivRound(x: int, y: int, z: int): int
    builtin;

/// Similar to `mulDivFloor`, but ceils the result: ceil([x] * [y] / [z]).
@pure
fun mulDivCeil(x: int, y: int, z: int): int
    builtin;

/// Computes the quotient and remainder of ([x] * [y] / [z]). Example: mulDivMod(112,3,10) = (33,6)
@pure
fun mulDivMod(x: int, y: int, z: int): (int, int)
    builtin;


/**
    Global getters of environment and contract state.
 */

const MASTERCHAIN = -1;
const BASECHAIN = 0;

/// Returns current Unix timestamp (in seconds).
@pure
fun now(): int
    asm "NOW";

/// Returns the internal address of the current smart contract as a Slice with a `MsgAddressInt`.
/// If necessary, it can be parsed further using primitives such as [parseStandardAddress].
@pure
fun getMyAddress(): slice
    asm "MYADDR";

/// Returns the balance (in nanotoncoins) of the smart contract at the start of Computation Phase.
/// Note that RAW primitives such as [sendMessage] do not update this field.
@pure
fun getMyStartBalance(): int
    asm "BALANCE" "FIRST";

/// Same as [getMyStartBalance], but returns a tuple:
/// `int` — balance in nanotoncoins;
/// `cell` — a dictionary with 32-bit keys representing the balance of "extra currencies".
@pure
fun getMyStartBalanceWithExtraCurrencies(): [int, cell]
    asm "BALANCE";

/// Returns the logical time of the current transaction.
@pure
fun getLogicalTime(): int
    asm "LTIME";

/// Returns the starting logical time of the current block.
@pure
fun getCurrentBlockLogicalTime(): int
    asm "BLOCKLT";

/// Returns the value of the global configuration parameter with integer index `i` as a `cell` or `null` value.
@pure
fun getBlockchainConfigParam(x: int): cell
    asm "CONFIGOPTPARAM";

/// Returns a tuple related to blockchain fee (config #14): price of gas units and similar.
fun getBlockchainFeeConfigParams(): tuple
    asm "UNPACKEDCONFIGTUPLE";

/// Returns the persistent contract storage cell. It can be parsed or modified with slice and builder primitives later.
@pure
fun getContractData(): cell
    asm "c4 PUSH";

/// Sets `cell` [c] as persistent contract data. You can update persistent contract storage with this primitive.
fun setContractData(c: cell): void
    asm "c4 POP";

/// Retrieves code of smart-contract from c7
fun getContractCode(): cell
    asm "MYCODE";

/// Creates an output action that would change this smart contract code to that given by cell [newCode].
/// Notice that this change will take effect only after the successful termination of the current run of the smart contract.
fun setContractCodePostponed(newCode: cell): void
    asm "SETCODE";

/// Commits the current state of registers `c4` (“persistent data”) and `c5` (“actions”)
/// so that the current execution is considered “successful” with the saved values even if an exception
/// in Computation Phase is thrown later.
fun commitContractDataAndActions(): void
    asm "COMMIT";


/**
  Signature checks, hashing, cryptography.
 */

/// Computes the representation hash of a `cell` [c] and returns it as a 256-bit unsigned integer `x`.
/// Useful for signing and checking signatures of arbitrary entities represented by a tree of cells.
@pure
fun cellHash(c: cell): int
    asm "HASHCU";

/// Computes the hash of a `slice s` and returns it as a 256-bit unsigned integer `x`.
/// The result is the same as if an ordinary cell containing only data and references from `s` had been created
/// and its hash computed by [cellHash].
@pure
fun sliceHash(s: slice): int
    asm "HASHSU";

/// Computes sha256 of the data bits of `slice` [s]. If the bit length of `s` is not divisible by eight,
/// throws a cell underflow exception. The hash value is returned as a 256-bit unsigned integer `x`.
@pure
fun stringHash(s: slice): int
    asm "SHA256U";

/// Checks the Ed25519-`signature` of a `hash` (a 256-bit unsigned integer, usually computed as the hash of some data)
/// using [publicKey] (also represented by a 256-bit unsigned integer).
/// The signature must contain at least 512 data bits; only the first 512 bits are used.
/// The result is `−1` if the signature is valid, `0` otherwise.
/// Note that `CHKSIGNU` creates a 256-bit slice with the hash and calls `CHKSIGNS`.
/// That is, if [hash] is computed as the hash of some data, these data are hashed twice,
/// the second hashing occurring inside `CHKSIGNS`.
@pure
fun isSignatureValid(hash: int, signature: slice, publicKey: int): int
    asm "CHKSIGNU";

/// Checks whether [signature] is a valid Ed25519-signature of the data portion of `slice data` using `publicKey`,
/// similarly to [isSignatureValid].
/// If the bit length of [data] is not divisible by eight, throws a cell underflow exception.
/// The verification of Ed25519 signatures is the standard one,
/// with sha256 used to reduce [data] to the 256-bit number that is actually signed.
@pure
fun isSliceSignatureValid(data: slice, signature: slice, publicKey: int): int
    asm "CHKSIGNS";

/// Generates a new pseudo-random unsigned 256-bit integer x.
fun random(): int
    asm "RANDU256";

/// Generates a new pseudo-random integer z in the range 0..range−1 (or range..−1, if range < 0).
/// More precisely, an unsigned random value x is generated as in random; then z := x * range / 2^256 is computed.
fun randomRange(range: int): int
    asm "RAND";

/// Returns the current random seed as an unsigned 256-bit integer.
@pure
fun randomGetSeed(): int
    asm "RANDSEED";

/// Sets the random seed to unsigned 256-bit seed.
fun randomSetSeed(seed: int): void
    asm "SETRAND";

/// Initializes (mixes) random seed with unsigned 256-bit integer x.
fun randomizeBy(x: int): void
    asm "ADDRAND";

/// Initializes random seed using current time. Don't forget to call this before calling `random`!
fun randomizeByLogicalTime(): void
    asm "LTIME" "ADDRAND";


/**
  Size computation primitives.
  They may be useful for computing storage fees of user-provided data.
 */

/// Returns `(x, y, z, -1)` or `(null, null, null, 0)`.
/// Recursively computes the count of distinct cells `x`, data bits `y`, and cell references `z`
/// in the DAG rooted at `cell` [c], effectively returning the total storage used by this DAG taking into account
/// the identification of equal cells.
/// The values of `x`, `y`, and `z` are computed by a depth-first traversal of this DAG,
/// with a hash table of visited cell hashes used to prevent visits of already-visited cells.
/// The total count of visited cells `x` cannot exceed non-negative [maxCells];
/// otherwise the computation is aborted before visiting the `(maxCells + 1)`-st cell and
/// a zero flag is returned to indicate failure. If [c] is `null`, returns `x = y = z = 0`.
@pure
fun calculateCellSize(c: cell, maxCells: int): (int, int, int, int)
    asm "CDATASIZEQ NULLSWAPIFNOT2 NULLSWAPIFNOT";

/// Similar to [calculateCellSize], but accepting a `slice` [s] instead of a `cell`.
/// The returned value of `x` does not take into account the cell that contains the `slice` [s] itself;
/// however, the data bits and the cell references of [s] are accounted for in `y` and `z`.
@pure
fun calculateSliceSize(s: slice, maxCells: int): (int, int, int, int)
    asm "SDATASIZEQ NULLSWAPIFNOT2 NULLSWAPIFNOT";

/// A non-quiet version of [calculateCellSize] that throws a cell overflow exception (`8`) on failure.
fun calculateCellSizeStrict(c: cell, maxCells: int): (int, int, int)
    asm "CDATASIZE";

/// A non-quiet version of [calculateSliceSize] that throws a cell overflow exception (`8`) on failure.
fun calculateSliceSizeStrict(s: slice, maxCells: int): (int, int, int)
    asm "SDATASIZE";

/// Returns the depth of `cell` [c].
/// If [c] has no references, then return `0`;
/// otherwise the returned value is one plus the maximum of depths of cells referred to from [c].
/// If [c] is a `null` instead of a cell, returns zero.
@pure
fun getCellDepth(c: cell): int
    asm "CDEPTH";

/// Returns the depth of `slice` [s].
/// If [s] has no references, then returns `0`;
/// otherwise the returned value is one plus the maximum of depths of cells referred to from [s].
@pure
fun getSliceDepth(s: slice): int
    asm "SDEPTH";

/// Returns the depth of `builder` [b].
/// If no cell references are stored in [b], then returns 0;
/// otherwise the returned value is one plus the maximum of depths of cells referred to from [b].
@pure
fun getBuilderDepth(b: builder): int
    asm "BDEPTH";


/**
  Debug primitives.
  Only works for local TVM execution with debug level verbosity.
 */

/// Dump a variable [x] to the debug log.
fun debugPrint<X>(x: X): void
    builtin;

/// Dump a string [x] to the debug log.
fun debugPrintString<X>(x: X): void
    builtin;

/// Dumps the stack (at most the top 255 values) and shows the total stack depth.
fun debugDumpStack(): void
    builtin;


/**
  Slice primitives: parsing cells.
  When you _load_ some data, you mutate the slice (shifting an internal pointer on the stack).
  When you _preload_ some data, you just get the result without mutating the slice.
 */

/// Converts a `cell` [c] into a `slice`. Notice that [c] must be either an ordinary cell,
/// or an exotic cell (see [TVM.pdf](https://ton-blockchain.github.io/docs/tvm.pdf), 3.1.2)
/// which is automatically loaded to yield an ordinary cell `c'`, converted into a `slice` afterwards.
@pure
fun beginParse(c: cell): slice
    asm "CTOS";

/// Checks if slice is empty. If not, throws an exception.
fun assertEndOfSlice(self: slice): void
    asm "ENDS";

/// Loads the next reference from the slice.
@pure
fun loadRef(mutate self: slice): cell
    asm( -> 1 0) "LDREF";

/// Preloads the next reference from the slice.
@pure
fun preloadRef(self: slice): cell
    asm "PLDREF";

/// Loads a signed [len]-bit integer from a slice.
@pure
fun loadInt(mutate self: slice, len: int): int
    builtin;

/// Loads an unsigned [len]-bit integer from a slice.
@pure
fun loadUint(mutate self: slice, len: int): int
    builtin;

/// Loads the first `0 ≤ len ≤ 1023` bits from slice [s] into a separate slice `s''`.
@pure
fun loadBits(mutate self: slice, len: int): slice
    builtin;

/// Preloads a signed [len]-bit integer from a slice.
@pure
fun preloadInt(self: slice, len: int): int
    builtin;

/// Preloads an unsigned [len]-bit integer from a slice.
@pure
fun preloadUint(self: slice, len: int): int
    builtin;

/// Preloads the first `0 ≤ len ≤ 1023` bits from slice [s] into a separate slice.
@pure
fun preloadBits(self: slice, len: int): slice
    builtin;

/// Loads serialized amount of Toncoins (any unsigned integer up to `2^120 - 1`).
@pure
fun loadCoins(mutate self: slice): int
    asm( -> 1 0) "LDGRAMS";

/// Loads bool (-1 or 0) from a slice
fun loadBool(mutate self: slice): int
    asm( -> 1 0) "1 LDI";

/// Shifts a slice pointer to [len] bits forward, mutating the slice.
@pure
fun skipBits(mutate self: slice, len: int): self
    asm "SDSKIPFIRST";

/// Returns the first `0 ≤ len ≤ 1023` bits of a slice.
@pure
fun getFirstBits(self: slice, len: int): slice
    asm "SDCUTFIRST";

/// Returns all but the last `0 ≤ len ≤ 1023` bits of a slice.
@pure
fun removeLastBits(mutate self: slice, len: int): self
    asm "SDSKIPLAST";

/// Returns the last `0 ≤ len ≤ 1023` bits of a slice.
@pure
fun getLastBits(self: slice, len: int): slice
    asm "SDCUTLAST";

/// Loads a dictionary (TL HashMapE structure, represented as TVM cell) from a slice.
/// Returns `null` if `nothing` constructor is used.
@pure
fun loadDict(mutate self: slice): cell
    asm( -> 1 0) "LDDICT";

/// Preloads a dictionary (cell) from a slice.
@pure
fun preloadDict(self: slice): cell
    asm "PLDDICT";

/// Loads a dictionary as [loadDict], but returns only the remainder of the slice.
@pure
fun skipDict(mutate self: slice): self
    asm "SKIPDICT";

/// Loads (Maybe ^Cell) from a slice.
/// In other words, loads 1 bit: if it's true, loads the first ref, otherwise returns `null`.
@pure
fun loadMaybeRef(mutate self: slice): cell
    asm( -> 1 0) "LDOPTREF";

/// Preloads (Maybe ^Cell) from a slice.
@pure
fun preloadMaybeRef(self: slice): cell
    asm "PLDOPTREF";

/// Loads (Maybe ^Cell), but returns only the remainder of the slice.
@pure
fun skipMaybeRef(mutate self: slice): self
    asm "SKIPOPTREF";

/**
  Builder primitives: constructing cells.
  When you _store_ some data, you mutate the builder (shifting an internal pointer on the stack).
  All the primitives below first check whether there is enough space in the `builder`,
  and only then check the range of the value being serialized.
 */

/// Creates a new empty builder.
@pure
fun beginCell(): builder
    asm "NEWC";

/// Converts a builder into an ordinary `cell`.
@pure
fun endCell(self: builder): cell
    asm "ENDC";

/// Stores a reference to a cell into a builder.
@pure
fun storeRef(mutate self: builder, c: cell): self
    asm(c self) "STREF";

/// Stores a signed [len]-bit integer into a builder (`0 ≤ len ≤ 257`).
@pure
fun storeInt(mutate self: builder, x: int, len: int): self
    builtin;

/// Stores an unsigned [len]-bit integer into a builder (`0 ≤ len ≤ 256`).
@pure
fun storeUint(mutate self: builder, x: int, len: int): self
    builtin;

/// Stores a slice into a builder.
@pure
fun storeSlice(mutate self: builder, s: slice): self
    asm "STSLICER";

/// Stores amount of Toncoins into a builder.
@pure
fun storeCoins(mutate self: builder, x: int): self
    asm "STGRAMS";

/// Stores bool (-1 or 0) into a builder.
/// Attention: true value is `-1`, not 1! If you pass `1` here, TVM will throw an exception.
@pure
fun storeBool(mutate self: builder, x: int): self
    asm(x self) "1 STI";

/// Stores dictionary (represented by TVM `cell` or `null`) into a builder.
/// In other words, stores a `1`-bit and a reference to [c] if [c] is not `null` and `0`-bit otherwise.
@pure
fun storeDict(mutate self: builder, c: cell): self
    asm(c self) "STDICT";

/// Stores (Maybe ^Cell) into a builder.
/// In other words, if cell is `null`, store '0' bit; otherwise, store '1' and a ref to [c].
@pure
fun storeMaybeRef(mutate self: builder, c: cell): self
    asm(c self) "STOPTREF";

/// Concatenates two builders.
@pure
fun storeBuilder(mutate self: builder, from: builder): self
    asm "STBR";

/// Stores a slice representing TL addr_none$00 (two `0` bits).
@pure
fun storeAddressNone(mutate self: builder): self
    asm "0 PUSHINT" "SWAP" "2 STU";


/**
  Slice size primitives.
 */

/// Returns the number of references in a slice.
@pure
fun getRemainingRefsCount(self: slice): int
    asm "SREFS";

/// Returns the number of data bits in a slice.
@pure
fun getRemainingBitsCount(self: slice): int
    asm "SBITS";

/// Returns both the number of data bits and the number of references in a slice.
@pure
fun getRemainingBitsAndRefsCount(self: slice): (int, int)
    asm "SBITREFS";

/// Checks whether a slice is empty (i.e., contains no bits of data and no cell references).
@pure
fun isEndOfSlice(self: slice): int
    asm "SEMPTY";

/// Checks whether a slice has no bits of data.
@pure
fun isEndOfSliceBits(self: slice): int
    asm "SDEMPTY";

/// Checks whether a slice has no references.
@pure
fun isEndOfSliceRefs(self: slice): int
    asm "SREMPTY";

/// Checks whether data parts of two slices coinside.
@pure
fun isSliceBitsEqual(self: slice, b: slice): int
    asm "SDEQ";

/// Returns the number of cell references already stored in a builder.
@pure
fun getBuilderRefsCount(self: builder): int
    asm "BREFS";

/// Returns the number of data bits already stored in a builder.
@pure
fun getBuilderBitsCount(self: builder): int
    asm "BBITS";


/**
  Address manipulation primitives.
  The address manipulation primitives listed below serialize and deserialize values according to the following TL-B scheme:
  ```TL-B
  addr_none$00 = MsgAddressExt;
  addr_extern$01 len:(## 8) external_address:(bits len)
               = MsgAddressExt;
  anycast_info$_ depth:(#<= 30) { depth >= 1 }
    rewrite_pfx:(bits depth) = Anycast;
  addr_std$10 anycast:(Maybe Anycast)
    workchain_id:int8 address:bits256 = MsgAddressInt;
  addr_var$11 anycast:(Maybe Anycast) addr_len:(## 9)
    workchain_id:int32 address:(bits addr_len) = MsgAddressInt;
  _ _:MsgAddressInt = MsgAddress;
  _ _:MsgAddressExt = MsgAddress;

  int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool
    src:MsgAddress dest:MsgAddressInt
    value:CurrencyCollection ihr_fee:Grams fwd_fee:Grams
    created_lt:uint64 created_at:uint32 = CommonMsgInfoRelaxed;
  ext_out_msg_info$11 src:MsgAddress dest:MsgAddressExt
    created_lt:uint64 created_at:uint32 = CommonMsgInfoRelaxed;
  ```
  A deserialized `MsgAddress` is represented by a tuple `t` as follows:

  - `addr_none` is represented by `t = (0)`,
    i.e., a tuple containing exactly one integer equal to zero.
  - `addr_extern` is represented by `t = (1, s)`,
    where slice `s` contains the field `external_address`. In other words, `
    t` is a pair (a tuple consisting of two entries), containing an integer equal to one and slice `s`.
  - `addr_std` is represented by `t = (2, u, x, s)`,
    where `u` is either a `null` (if `anycast` is absent) or a slice `s'` containing `rewrite_pfx` (if anycast is present).
    Next, integer `x` is the `workchain_id`, and slice `s` contains the address.
  - `addr_var` is represented by `t = (3, u, x, s)`,
    where `u`, `x`, and `s` have the same meaning as for `addr_std`.
 */

/// Loads from slice [s] the only prefix that is a valid `MsgAddress`,
/// and returns both this prefix `s'` and the remainder `s''` of [s] as slices.
@pure
fun loadAddress(mutate self: slice): slice
    asm( -> 1 0) "LDMSGADDR";

/// Decomposes slice [s] containing a valid `MsgAddress` into a `tuple t` with separate fields of this `MsgAddress`.
/// If [s] is not a valid `MsgAddress`, a cell deserialization exception is thrown.
@pure
fun parseAddress(s: slice): tuple
    asm "PARSEMSGADDR";

/// Parses slice [s] containing a valid `MsgAddressInt` (usually a `msg_addr_std`),
/// applies rewriting from the anycast (if present) to the same-length prefix of the address,
/// and returns both the workchain and the 256-bit address as integers.
/// If the address is not 256-bit, or if [s] is not a valid serialization of `MsgAddressInt`,
/// throws a cell deserialization exception.
@pure
fun parseStandardAddress(s: slice): (int, int)
    asm "REWRITESTDADDR";

/// Creates a slice representing TL addr_none$00 (two `0` bits).
fun createAddressNone(): slice
    asm "<b 0 2 u, b> <s PUSHSLICE";

/// Returns if a slice pointer contains an empty address (`-1` for true, `0` for false, as always).
/// In other words, a slice starts with two `0` bits (TL addr_none$00).
fun addressIsNone(s: slice): int
    asm "2 PLDU" "0 EQINT";

/**
  Messages sending and parsing primitives.
  Working with messages is low-level right now, but still, every contract should do that.

  `Message` structure, its header and so on are specified in TL-B scheme, particularly:
  int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool ... = CommonMsgInfo;
 */

/// 0b011000 tag - 0, ihr_disabled - 1, bounce - 1, bounced - 0, src = adr_none$00
const BOUNCEABLE = 0x18;
/// 0b010000 tag - 0, ihr_disabled - 1, bounce - 0, bounced - 0, src = adr_none$00
const NON_BOUNCEABLE = 0x10;

/// Load msgFlags from in message body (4 bits).
fun loadMsgFlags(mutate self: slice): int
    asm( -> 1 0) "4 LDU";

/// Having msgFlags (4 bits), check that a message is bounced.
/// Effectively, it's `msgFlags & 1` (the lowest bit present).
fun isMessageBounced(msgFlags: int): int
    asm "1 PUSHINT" "AND";

/// Skip 0xFFFFFFFF prefix (when a message is bounced).
fun skipBouncedPrefix(mutate self: slice): self
    asm "32 PUSHINT" "SDSKIPFIRST";

/// The guideline recommends to start the body of an internal message with uint32 `op` and uint64 `queryId`.
fun loadMessageOp(mutate self: slice): int
    asm( -> 1 0) "32 LDU";

fun skipMessageOp(mutate self: slice): self
    asm "32 PUSHINT" "SDSKIPFIRST";

fun storeMessageOp(mutate self: builder, op: int): self
    asm(op self) "32 STU";

/// The guideline recommends that uint64 `queryId` should follow uint32 `op`.
fun loadMessageQueryId(mutate self: slice): int
    asm( -> 1 0) "64 LDU";

fun skipMessageQueryId(mutate self: slice): self
    asm "64 PUSHINT" "SDSKIPFIRST";

fun storeMessageQueryId(mutate self: builder, queryId: int): self
    asm(queryId self) "64 STU";

/// SEND MODES - https://docs.ton.org/tvm.pdf page 137, SENDRAWMSG

/// mode = 0 is used for ordinary messages; the gas fees are deducted from the senging amount; action phaes should NOT be ignored.
const SEND_MODE_REGULAR = 0;
/// +1 means that the sender wants to pay transfer fees separately.
const SEND_MODE_PAY_FEES_SEPARATELY = 1;
/// +2 means that any errors arising while processing this message during the action phase should be ignored.
const SEND_MODE_IGNORE_ERRORS = 2;
/// mode = 32 means that the current account must be destroyed if its resulting balance is zero.
const SEND_MODE_DESTROY = 32;
/// mode = 64 is used for messages that carry all the remaining value of the inbound message in addition to the value initially indicated in the new message.
const SEND_MODE_CARRY_ALL_REMAINING_MESSAGE_VALUE = 64;
/// mode = 128 is used for messages that are to carry all the remaining balance of the current smart contract (instead of the value originally indicated in the message).
const SEND_MODE_CARRY_ALL_BALANCE = 128;
/// in the case of action fail - bounce transaction. No effect if SEND_MODE_IGNORE_ERRORS (+2) is used. TVM UPGRADE 2023-07. https://docs.ton.org/learn/tvm-instructions/tvm-upgrade-2023-07#sending-messages
const SEND_MODE_BOUNCE_ON_ACTION_FAIL = 16;
/// do not create an action, only estimate fee. TVM UPGRADE 2023-07. https://docs.ton.org/learn/tvm-instructions/tvm-upgrade-2023-07#sending-messages
const SEND_MODE_ESTIMATE_FEE_ONLY = 1024;
/// Other modes affect the fee calculation as follows:
/// +64 substitutes the entire balance of the incoming message as an outcoming value (slightly inaccurate, gas expenses that cannot be estimated before the computation is completed are not taken into account).
/// +128 substitutes the value of the entire balance of the contract before the start of the computation phase (slightly inaccurate, since gas expenses that cannot be estimated before the completion of the computation phase are not taken into account).

/// Sends a raw message — a correctly serialized TL object `Message X`.
/// For `mode`, see constants above.
fun sendMessage(msg: cell, mode: int): void
    asm "SENDMSG";
