/**********************************************************************************************************************/

program credits.aleo;

/**********************************************************************************************************************/

/// The `committee` mapping contains the active validator set and their corresponding stake.
mapping committee:
    // The key represents the address of the validator.
    key as address.public;
    // The value represents the committee state of the validator.
    value as committee_state.public;

// The `committee_state` struct tracks the total stake of the validator, and whether they are open to stakers.
struct committee_state:
    // The amount of microcredits bonded to the validator, by the validator and its delegators.
    microcredits as u64;
    // The boolean flag indicating if the validator is open to stakers.
    is_open as boolean;

/**********************************************************************************************************************/

// The `bonded` mapping represents the amount of microcredits that are currently bonded.
mapping bonded:
    // The key represents the address of the staker, which includes the validators and their delegators.
    key as address.public;
    // The value represents the bond state.
    value as bond_state.public;

// The `bond_state` struct tracks the amount of microcredits that are currently bonded to the specified validator.
struct bond_state:
    // The address of the validator.
    validator as address;
    // The amount of microcredits that are currently bonded to the specified validator.
    microcredits as u64;

/**********************************************************************************************************************/

// The `unbonding` mapping contains a set of stakers with their unbonding microcredits and unlock height.
mapping unbonding:
    // The key represents the address of the staker, which includes the validators and their delegators.
    key as address.public;
    // The value represents the unbond state.
    value as unbond_state.public;

// The `unbond_state` struct tracks the microcredits that are currently unbonding, along with the unlock height.
struct unbond_state:
    // The amount of microcredits that are currently unbonding.
    microcredits as u64;
    // The block height at which the unbonding will be complete, and can be claimed.
    height as u32;

/**********************************************************************************************************************/

// The `account` mapping is used to store credits publicly.
mapping account:
    // The key represents the address of the owner.
    key as address.public;
    // The value represents the amount of public microcredits that belong to the specified owner.
    value as u64.public;

/**********************************************************************************************************************/

// The `credits` record is used to store credits privately.
record credits:
    // The address of the owner.
    owner as address.private;
    // The amount of private microcredits that belong to the specified owner.
    microcredits as u64.private;

/**********************************************************************************************************************/

// This function allows any staker to bond their microcredits to a validator.
// The corresponding functions for 'bond_public' are 'unbond_public' and 'claim_unbond_public'.
function bond_public:
    // Input the validator's address.
    input r0 as address.public;
    // Input the amount of microcredits to bond.
    input r1 as u64.public;

    // Determine if the amount is at least one credit.
    gte r1 1_000_000u64 into r2;
    // Enforce the amount is at least one credit.
    assert.eq r2 true;

    // Bond the specified amount of microcredits to the specified validator.
    async bond_public self.caller r0 r1 into r3;
    output r3 as credits.aleo/bond_public.future;

finalize bond_public:
    // Input the staker's address.
    input r0 as address.public;
    // Input the validator's address.
    input r1 as address.public;
    // Input the amount of microcredits to bond.
    input r2 as u64.public;

    // Determine whether the caller is a validator.
    is.eq r0 r1 into r3;
    // If the caller is a validator, jump to the `bond_validator` logic.
    branch.eq r3 true to bond_validator;
    // If the caller is not a validator, jump to the `bond_delegator` logic.
    branch.eq r3 false to bond_delegator;

    /******* Bond Validator *******/

    // Starts the `bond_validator` logic.
    position bond_validator;

    /* Committee */

    // Construct the initial committee state.
    // Note: We set the initial 'is_open' state to 'true'.
    cast 0u64 true into r4 as committee_state;
    // Retrieve the committee state of the specified validator.
    get.or_use committee[r0] r4 into r5;
    // Ensure that the validator is open to stakers.
    assert.eq r5.is_open true;

    // Increment the stake for the specified validator.
    add r5.microcredits r2 into r6;
    // Construct the updated committee state.
    cast r6 r5.is_open into r7 as committee_state;

    /* Bonded */

    // Construct the initial bond state.
    cast r1 0u64 into r8 as bond_state;
    // Get the bond state for the caller, or default to the initial bond state.
    get.or_use bonded[r0] r8 into r9;
    // Enforce the validator matches in the bond state.
    assert.eq r9.validator r1;

    // Increment the microcredits in the bond state.
    add r9.microcredits r2 into r10;
    // Determine if the amount is at least one million credits.
    gte r10 1_000_000_000_000u64 into r11;
    // Enforce the amount is at least one million credits.
    assert.eq r11 true;

    // Construct the updated bond state.
    cast r1 r10 into r12 as bond_state;

    /* Account */

    // Get the balance of the caller.
    // If the account does not exist, this finalize scope will fail.
    get account[r0] into r13;
    // Decrement the balance of the caller.
    sub r13 r2 into r14;

    /* Writes */

    // Update the committee state of the specified validator.
    set r7 into committee[r0];
    // Update the bond state for the caller.
    set r12 into bonded[r0];
    // Update the balance of the caller.
    set r14 into account[r0];

    // Ends the `bond_validator` logic.
    branch.eq true true to end;

    /******* Bond Delegator *******/

    // Starts the `bond_delegator` logic.
    position bond_delegator;

    /* Committee */

    // Check if the caller is a validator.
    contains committee[r0] into r15;
    // Enforce the caller is *not* a validator.
    assert.eq r15 false;

    // Get the stake for the specified validator.
    // If the validator does not exist, this finalize scope will fail.
    get committee[r1] into r16;
    // Ensure that the validator is open to stakers.
    assert.eq r16.is_open true;

    // Increment the stake for the specified validator.
    add r16.microcredits r2 into r17;
    // Construct the updated committee state.
    cast r17 r16.is_open into r18 as committee_state;

    /* Bonded */

    // Construct the initial bond state.
    cast r1 0u64 into r19 as bond_state;
    // Get the bond state for the caller, or default to the initial bond state.
    get.or_use bonded[r0] r19 into r20;
    // Enforce the validator matches in the bond state.
    assert.eq r20.validator r1;

    // Increment the microcredits in the bond state.
    add r20.microcredits r2 into r21;
    // Determine if the amount is at least 10 credits.
    gte r21 10_000_000u64 into r22;
    // Enforce the amount is at least 10 credits.
    assert.eq r22 true;

    // Construct the updated bond state.
    cast r1 r21 into r23 as bond_state;

    /* Account */

    // Get the balance of the caller.
    // If the account does not exist, this finalize scope will fail.
    get account[r0] into r24;
    // Decrement the balance of the caller.
    sub r24 r2 into r25;

    /* Writes */

    // Update the committee state for the specified validator.
    set r18 into committee[r1];
    // Update the bond state for the caller.
    set r23 into bonded[r0];
    // Update the balance of the caller.
    set r25 into account[r0];

    // The terminus.
    position end;

/**********************************************************************************************************************/

// This function allows any staker to unbond their microcredits from a validator.
// The corresponding functions for 'unbond_public' is 'claim_unbond_public'.
function unbond_public:
    // Input the amount of microcredits to unbond.
    input r0 as u64.public;

    // Unbond the specified amount of microcredits to the caller.
    async unbond_public self.caller r0 into r1;
    output r1 as credits.aleo/unbond_public.future;

finalize unbond_public:
    // Input the staker's address.
    input r0 as address.public;
    // Input the amount of microcredits to unbond.
    input r1 as u64.public;

    // Construct the initial unbond state.
    cast 0u64 0u32 into r2 as unbond_state;
    // Get the unbond state for the caller, or default to the initial unbond state.
    get.or_use unbonding[r0] r2 into r3;

    // Compute the height at which the unbonding will be complete, starting from the current block.
    // Note: Calling unbond across multiple blocks before the unbonding is complete will reset the height each time.
    add block.height 720u32 into r4;

    // Determine if the caller is a validator or delegator.
    contains committee[r0] into r5;

    // If the caller is a validator, jump to the `unbond_validator` logic.
    branch.eq r5 true to unbond_validator;
    // If the caller is not a validator, jump to the `unbond_delegator` logic.
    branch.eq r5 false to unbond_delegator;

    /******* Unbond Validator *******/

    // Starts the `unbond_validator` logic.
    position unbond_validator;

    /* Committee */

    // Get the committee state for the specified validator.
    get committee[r0] into r6;
    // Decrement the stake for the specified validator.
    sub r6.microcredits r1 into r7;

    /* Bonded */

    // Get the bond state for the validator, or fail if it does not exist.
    get bonded[r0] into r8;
    // Ensure that the validator matches in the bond state.
    assert.eq r8.validator r0;
    // Decrement the microcredits in the bond state.
    sub r8.microcredits r1 into r9;

    // Determine if the remaining bond is at least one million credits.
    gte r9 1_000_000_000_000u64 into r10;

    // If the remaining balance is at least 1 million credits, jump to the `decrement_validator` logic.
    branch.eq r10 true to decrement_validator;
    // If the remaining balance is less than 1 million credits, jump to the `remove_validator` logic.
    branch.eq r10 false to remove_validator;

    /*** Decrement Validator ***/

    // Starts the `decrement_validator` logic.
    position decrement_validator;

    /* Committee */

    // Construct the updated committee state.
    cast r7 r6.is_open into r11 as committee_state;
    // Update the committee state for the validator.
    set r11 into committee[r0];

    /* Bonded */

    // Construct the updated bond state.
    cast r0 r9 into r12 as bond_state;
    // Update the bond state for the validator.
    set r12 into bonded[r0];

    /* Unbonding */

    // Increment the microcredits in the unbond state.
    add r3.microcredits r1 into r13;

    // Construct the updated unbond state.
    cast r13 r4 into r14 as unbond_state;
    // Update the unbond state for the caller.
    set r14 into unbonding[r0];

    // Ends the `decrement_validator` logic.
    branch.eq true true to end;

    /*** Remove Validator ***/

    // Starts the `remove_validator` logic.
    position remove_validator;

    // Ensure that the validator has no delegators.
    assert.eq r6.microcredits r8.microcredits;

    /* Committee */

    // Remove the validator from the committee.
    remove committee[r0];

    /* Bonded */

    // Remove the bond state for the validator.
    remove bonded[r0];

    /* Unbonding */

    // Increment the microcredits in the unbond state.
    add r3.microcredits r8.microcredits into r15;

    // Construct the updated unbond state.
    cast r15 r4 into r16 as unbond_state;
    // Update the unbond state for the caller.
    set r16 into unbonding[r0];

    // Ends the `remove_validator` logic.
    branch.eq true true to end;

    /******* Unbond Delegator *******/

    // Starts the `unbond_delegator` logic.
    position unbond_delegator;

    // Get the bond state for the caller, or fail if it does not exist.
    get bonded[r0] into r17;
    // Decrement the microcredits in the bond state.
    sub r17.microcredits r1 into r18;

    // Determine if the remaining bond is at least 10 credits.
    gte r18 10_000_000u64 into r19;

    // If the remaining balance is at least 10 credits, jump to the `decrement_delegator` logic.
    branch.eq r19 true to decrement_delegator;
    // If the remaining balance is less than 10 credits, jump to the `remove_delegator` logic.
    branch.eq r19 false to remove_delegator;

    /*** Decrement Delegator ***/

    // Starts the `decrement_delegator` logic.
    position decrement_delegator;

    /* Committee */

    // Get the stake for the specified validator.
    // If the validator does not exist, this finalize scope will fail.
    get committee[r17.validator] into r20;
    // Decrement the stake for the specified validator.
    sub r20.microcredits r1 into r21;
    // Construct the updated committee state.
    cast r21 r20.is_open into r22 as committee_state;
    // Update the stake for the specified validator.
    set r22 into committee[r17.validator];

    /* Bonded */

    // Construct the updated bond state.
    cast r17.validator r18 into r23 as bond_state;
    // Update the bond state for the caller.
    set r23 into bonded[r0];

    /* Unbonding */

    // Increment the microcredits in the unbond state.
    add r3.microcredits r1 into r24;

    // Construct the updated unbond state.
    cast r24 r4 into r25 as unbond_state;
    // Update the unbond state for the caller.
    set r25 into unbonding[r0];

    // Ends the `decrement_delegator` logic.
    branch.eq true true to end;

    /*** Remove Delegator ***/

    // Starts the `remove_delegator` logic.
    position remove_delegator;

    /* Committee */

    // Get the stake for the specified validator.
    // If the validator does not exist, this finalize scope will fail.
    get committee[r17.validator] into r26;
    // Decrement the stake for the specified validator.
    sub r26.microcredits r17.microcredits into r27;
    // Construct the updated committee state.
    cast r27 r26.is_open into r28 as committee_state;
    // Update the stake for the specified validator.
    set r28 into committee[r17.validator];

    /* Bonded */

    // Remove the caller from the bonded mapping.
    remove bonded[r0];

    /* Unbonding */

    // Increment the microcredits in the unbond state.
    add r3.microcredits r17.microcredits into r29;

    // Construct the updated unbond state.
    cast r29 r4 into r30 as unbond_state;
    // Update the unbond state for the caller.
    set r30 into unbonding[r0];

    // The terminus.
    position end;

/**********************************************************************************************************************/

function unbond_delegator_as_validator:
    // Input the delegator's address.
    input r0 as address.public;

    // Unbond the delegator as the validator.
    async unbond_delegator_as_validator self.caller r0 into r1;
    output r1 as credits.aleo/unbond_delegator_as_validator.future;

finalize unbond_delegator_as_validator:
    // Input the validator's address.
    input r0 as address.public;
    // Input the delegator's address.
    input r1 as address.public;

    /* Start Committee */

    // Get the committee state for the specified validator.
    // If the validator does not exist, this finalize scope will fail.
    get committee[r0] into r2;
    // Enforce that the validator is closed to stakers.
    assert.eq r2.is_open false;

    // Check if the delegator is a validator.
    contains committee[r1] into r3;
    // Enforce the delegator is *not* a validator.
    assert.eq r3 false;

    /* End Committee */

    /* Start Bonded */

    // Get the bond state for the delegator, or fail if it does not exist.
    get bonded[r1] into r4;
    // Enforce that the delegator is bonded to the validator.
    assert.eq r4.validator r0;

    /* End Bonded */

    /* Start Committee */

    // Decrement the stake for the specified validator.
    sub r2.microcredits r4.microcredits into r5;
    // Construct the updated committee state.
    cast r5 r2.is_open into r6 as committee_state;

    /* End Committee */

    /* Start Unbond */

    // Construct the initial unbond state.
    cast 0u64 0u32 into r7 as unbond_state;
    // Get the unbond state for the delegator, or default to the initial unbond state.
    get.or_use unbonding[r1] r7 into r8;

    // Increment the microcredits in the unbond state.
    add r8.microcredits r4.microcredits into r9;
    // Compute the height at which the unbonding will be complete, starting from the current block.
    // Note: Calling unbond across multiple blocks before the unbonding is complete will reset the height each time.
    add block.height 720u32 into r10;

    // Construct the updated unbond state.
    cast r9 r10 into r11 as unbond_state;

    /* End Unbond */

    /* Start Writes */

    // Update the committee state for the specified validator.
    set r6 into committee[r0];
    // Remove the bond state for the delegator.
    remove bonded[r1];
    // Update the unbond state for the delegator.
    set r11 into unbonding[r1];

    /* End Writes */

/**********************************************************************************************************************/

// This function allows any staker to claim their microcredits after the unbonding period.
function claim_unbond_public:
    // Claim the unbonded microcredits.
    async claim_unbond_public self.caller into r0;
    output r0 as credits.aleo/claim_unbond_public.future;

finalize claim_unbond_public:
    // Input the staker's address.
    input r0 as address.public;

    // Get the unbond state for the caller, or fail if it does not exist.
    get unbonding[r0] into r1;
    // Determine if unbonding is complete.
    gte block.height r1.height into r2;
    // Enforce the unbonding is complete.
    assert.eq r2 true;

    // Add the unbonded amount to the stakers's public balance.
    // Increments `account[r0]` by `r1`.
    // If `account[r0]` does not exist, 0u64 is used.
    // If `account[r0] + r2` overflows, `claim_unbond_public` is reverted.
    get.or_use account[r0] 0u64 into r3;
    add r1.microcredits r3 into r4;
    set r4 into account[r0];

    // Remove the unbond state for the caller.
    remove unbonding[r0];

/**********************************************************************************************************************/

// This function allows a validator to set their state to be either opened or closed to stakers.
// When the validator is open to stakers, any staker (including the validator) can bond or unbond from the validator.
// When the validator is closed to stakers, all stakers can only unbond from the validator.
//
// This function serves two primary purposes:
// 1. Allow a validator to leave the committee, by closing themselves to stakers and then unbonding all of their stakers.
// 2. Allow a validator to maintain their % of stake, by closing themselves to allowing more stakers to bond to them.
function set_validator_state:
    // Input the 'is_open' state.
    input r0 as boolean.public;
    // Set the validator to be either open or closed to stakers.
    async set_validator_state self.caller r0 into r1;
    output r1 as credits.aleo/set_validator_state.future;

finalize set_validator_state:
    // Input the validator's address.
    input r0 as address.public;
    // Input the 'is_open' state.
    input r1 as boolean.public;

    // Get the committee state for the specified validator.
    // If the validator does not exist, this finalize scope will fail.
    get committee[r0] into r2;

    // Construct the updated committee state.
    cast r2.microcredits r1 into r3 as committee_state;
    // Update the committee state for the specified validator.
    set r3 into committee[r0];

/**********************************************************************************************************************/

// The `transfer_public` function sends the specified amount
// from the sender's `account` to the receiver's `account`.
function transfer_public:
    // Input the receiver.
    input r0 as address.public;
    // Input the amount.
    input r1 as u64.public;
    // Transfer the credits publicly.
    async transfer_public self.caller r0 r1 into r2;
    output r2 as credits.aleo/transfer_public.future;

finalize transfer_public:
    // Input the sender.
    input r0 as address.public;
    // Input the receiver.
    input r1 as address.public;
    // Input the amount.
    input r2 as u64.public;
    // Decrements `account[r0]` by `r2`.
    // If `account[r0]` does not exist, 0u64 is used.
    // If `account[r0] - r2` underflows, `transfer_public` is reverted.
    get.or_use account[r0] 0u64 into r3;
    sub r3 r2 into r4;
    set r4 into account[r0];
    // Increments `account[r1]` by `r2`.
    // If `account[r1]` does not exist, 0u64 is used.
    // If `account[r1] + r2` overflows, `transfer_public` is reverted.
    get.or_use account[r1] 0u64 into r5;
    add r5 r2 into r6;
    set r6 into account[r1];

/**********************************************************************************************************************/

// The `transfer_private` function sends the specified amount
// from the sender's record to the receiver in a record.
function transfer_private:
    // Input the sender's record.
    input r0 as credits.record;
    // Input the receiver.
    input r1 as address.private;
    // Input the amount.
    input r2 as u64.private;
    // Checks the given record has a sufficient amount.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs. The destination register `r3` holds
    // the change amount to be returned to the sender.
    sub r0.microcredits r2 into r3;
    // Construct a record for the specified receiver.
    cast r1 r2 into r4 as credits.record;
    // Construct a record with the change amount for the sender.
    cast r0.owner r3 into r5 as credits.record;
    // Output the receiver's record.
    output r4 as credits.record;
    // Output the sender's change record.
    output r5 as credits.record;

/**********************************************************************************************************************/

// The `transfer_private_to_public` function turns a specified amount
// from a record into public credits for the specified receiver.
//
// This function preserves privacy for the sender's record, however
// it publicly reveals the receiver and the amount.
function transfer_private_to_public:
    // Input the sender's record.
    input r0 as credits.record;
    // Input the receiver.
    input r1 as address.public;
    // Input the amount.
    input r2 as u64.public;
    // Checks the given record has a sufficient amount.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs. The destination register `r3` holds
    // the change amount for the sender.
    sub r0.microcredits r2 into r3;
    // Construct a record with the change amount for the sender.
    cast r0.owner r3 into r4 as credits.record;
    // Increment the amount publicly for the receiver.
    async transfer_private_to_public r1 r2 into r5;
    // Output the sender's change record.
    output r4 as credits.record;
    output r5 as credits.aleo/transfer_private_to_public.future;

finalize transfer_private_to_public:
    // Input the receiver.
    input r0 as address.public;
    // Input the amount.
    input r1 as u64.public;
    // Retrieve the balance of the sender.
    // If `account[r0]` does not exist, 0u64 is used.
    get.or_use account[r0] 0u64 into r2;
    // Increments `account[r0]` by `r1`.
    // If `r1 + r2` overflows, `transfer_private_to_public` is reverted.
    add r1 r2 into r3;
    // Updates the balance of the sender.
    set r3 into account[r0];

/**********************************************************************************************************************/

// The `transfer_public_to_private` function turns a specified amount
// from the mapping `account` into a record for the specified receiver.
//
// This function preserves privacy for the receiver's record, however
// it publicly reveals the sender and the specified amount.
function transfer_public_to_private:
    // Input the receiver.
    input r0 as address.private;
    // Input the amount.
    input r1 as u64.public;
    // Construct a record for the receiver.
    cast r0 r1 into r2 as credits.record;
    // Decrement the balance of the sender publicly.
    async transfer_public_to_private self.caller r1 into r3;
    // Output the record of the receiver.
    output r2 as credits.record;
    output r3 as credits.aleo/transfer_public_to_private.future;

finalize transfer_public_to_private:
    // Input the sender.
    input r0 as address.public;
    // Input the amount.
    input r1 as u64.public;
    // Retrieve the balance of the sender.
    // If `account[r0]` does not exist, 0u64 is used.
    get.or_use account[r0] 0u64 into r2;
    // Decrements `account[r0]` by `r1`.
    // If `r2 - r1` underflows, `transfer_public_to_private` is reverted.
    sub r2 r1 into r3;
    // Updates the balance of the sender.
    set r3 into account[r0];

/**********************************************************************************************************************/

// The `join` function combines two records into one.
function join:
    // Input the first record.
    input r0 as credits.record;
    // Input the second record.
    input r1 as credits.record;
    // Combines the amount of the first record and the second record.
    // This `add` operation is safe, and the proof will fail
    // if an overflow occurs.
    add r0.microcredits r1.microcredits into r2;
    // Construct a record with the combined amount.
    cast r0.owner r2 into r3 as credits.record;
    // Output the record.
    output r3 as credits.record;

/**********************************************************************************************************************/

// The `split` function splits a record into two records. The given input amount will be stored in the first record,
// and the remaining amount will be stored in the second record, with the fee deducted from the remaining amount.
// If the caller executes a transaction that contains only a call to this function, then the transaction does not
// require a fee, unless the caller wishes to provide an additional fee. Transactions that contain multiple transitions
// (that include one or more calls to this function) will require a fee as per standard consensus rules.
function split:
    // Input the record.
    input r0 as credits.record;
    // Input the amount to split.
    input r1 as u64.private;
    // Checks the given record has a sufficient amount to split.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs.
    sub r0.microcredits r1 into r2;
    // Checks the given record has a sufficient fee to remove.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs.
    sub r2 10_000u64 into r3;
    // Construct the first record.
    cast r0.owner r1 into r4 as credits.record;
    // Construct the second record.
    cast r0.owner r3 into r5 as credits.record;
    // Output the first record.
    output r4 as credits.record;
    // Output the second record.
    output r5 as credits.record;

/**********************************************************************************************************************/

// The `fee_private` function charges the specified amount from the sender's record.
function fee_private:
    // Input the sender's record.
    input r0 as credits.record;
    // Input the amount.
    input r1 as u64.public;
    // Input the deployment or execution root.
    input r2 as field.public;
    // Ensure the amount is nonzero.
    assert.neq r1 0u64;
    // Ensure the deployment or execution root is nonzero.
    assert.neq r2 0field;
    // Checks the given record has a sufficient amount.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs. The destination register `r3` holds
    // the change amount for the sender.
    sub r0.microcredits r1 into r3;
    // Construct a record with the change amount for the sender.
    cast r0.owner r3 into r4 as credits.record;
    // Output the sender's change record.
    output r4 as credits.record;

/**********************************************************************************************************************/

// The `fee_public` function charges the specified amount from the sender's account.
function fee_public:
    // Input the amount.
    input r0 as u64.public;
    // Input the deployment or execution root.
    input r1 as field.public;
    // Ensure the amount is nonzero.
    assert.neq r0 0u64;
    // Ensure the deployment or execution root is nonzero.
    assert.neq r1 0field;
    // Decrement the balance of the sender publicly.
    async fee_public self.caller r0 into r2;
    output r2 as credits.aleo/fee_public.future;

finalize fee_public:
    // Input the sender's address.
    input r0 as address.public;
    // Input the amount.
    input r1 as u64.public;
    // Retrieve the balance of the sender.
    // If `account[r0]` does not exist, `fee_public` is reverted.
    get account[r0] into r2;
    // Decrements `account[r0]` by `r1`.
    // If `r2 - r1` underflows, `fee_public` is reverted.
    sub r2 r1 into r3;
    // Updates the balance of the sender.
    set r3 into account[r0];

/**********************************************************************************************************************/
