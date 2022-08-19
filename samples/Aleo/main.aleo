// The 'token.aleo' program.
program token.aleo;

// On-chain storage of an `account` map, with `owner` as the key,
// and `amount` as the value.
mapping account:
    // The token owner.
    key owner as address.public;
    // The token amount.
    value amount as u64.public;

record token:
    // The token owner.
    owner as address.private;
    // The Aleo balance (in gates).
    gates as u64.private;
    // The token amount.
    amount as u64.private;

/* Mint */

// The function `mint_public` issues the specified token amount
// for the token receiver publicly on the network.
function mint_public:
    // Input the token receiver.
    input r0 as address.public;
    // Input the token amount.
    input r1 as u64.public;
    // Mint the tokens publicly.
    finalize r0 r1;

// The finalize scope of `mint_public` increments the
// `account` of the token receiver by the specified amount.
finalize mint_public:
    // Input the token receiver.
    input r0 as address.public;
    // Input the token amount.
    input r1 as u64.public;

    // Increments `account[r0]` by `r1`.
    // If `account[r0]` does not exist, it will be created.
    // If `account[r0] + r1` overflows, `mint_public` is reverted.
    increment account[r0] by r1;

// The function `mint_private` initializes a new record with the
// specified amount of tokens in `r1` for the receiver in `r0`.
function mint_private:
    // Input the token receiver.
    input r0 as address.private;
    // Input the token amount.
    input r1 as u64.private;

    // Produces a token record for the token receiver.
    cast r0 0u64 r1 into r2 as token.record;

    // Output the receiver's record.
    output r2 as token.record;

/* Transfer */

function transfer_public:
    // Input the token receiver.
    input r0 as address.public;
    // Input the token amount.
    input r1 as u64.public;
    // Transfer the tokens publicly.
    finalize self.caller r0 r1;

finalize transfer_public:
    // Input the token sender.
    input r0 as address.public;
    // Input the token receiver.
    input r1 as address.public;
    // Input the token amount.
    input r2 as u64.public;

    // Decrements `account[r0]` by `r2`.
    // If `account[r0]` does not exist, it will be created.
    // If `account[r0] - r2` underflows, `transfer_public` is reverted.
    decrement account[r0] by r2;

    // Increments `account[r1]` by `r2`.
    // If `account[r1]` does not exist, it will be created.
    // If `account[r1] + r2` overflows, `transfer_public` is reverted.
    increment account[r1] by r2;

// The function `transfer_private` sends the specified token amount
// to the token receiver from the specified token record.
function transfer_private:
    // Input the sender's record.
    input r0 as token.record;
    // Input the token receiver.
    input r1 as address.private;
    // Input the token amount.
    input r2 as u64.private;

    // Checks the given token record has a sufficient token amount.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs. The destination register `r3` holds
    // the change amount to be returned to the sender.
    sub r0.amount r2 into r3;

    // Produces a token record for the specified receiver.
    cast r1 0u64 r2 into r4 as token.record;

    // Produces a token record with the change amount for the sender.
    cast r0.owner r0.gates r3 into r5 as token.record;

    // Output the receiver's record.
    output r4 as token.record;
    // Output the sender's change record.
    output r5 as token.record;

// The function `transfer_private_to_public` turns a specified token amount
// from a token record into public tokens for the specified receiver.
//
// This function preserves privacy for the caller's record, however
// it publicly reveals the token receiver and the token amount.
function transfer_private_to_public:
    // Input the caller's record.
    input r0 as token.record;
    // Input the token receiver.
    input r1 as address.public;
    // Input the token amount.
    input r2 as u64.public;

    // Checks the given token record has a sufficient token amount.
    // This `sub` operation is safe, and the proof will fail
    // if an underflow occurs. The destination register `r3` holds
    // the change amount for the caller.
    sub r0.amount r2 into r3;

    // Produces a token record with the change amount for the caller.
    cast r0.owner r0.gates r3 into r4 as token.record;

    // Output the caller's change record.
    output r4 as token.record;

    // Increment the token amount publicly for the token receiver.
    finalize r1 r2;

finalize transfer_private_to_public:
    // Input the token receiver.
    input r0 as address.public;
    // Input the token amount.
    input r1 as u64.public;

    // Increments `account[r0]` by `r1`.
    // If `account[r0]` does not exist, it will be created.
    // If `account[r0] + r1` overflows, `transfer_private_to_public` is reverted.
    increment account[r0] by r1;

// The function `transfer_public_to_private` turns a specified token amount
// from `account` into a token record for the specified receiver.
// 
// This function preserves privacy for the receiver's record, however
// it publicly reveals the caller and the specified token amount.
function transfer_public_to_private:
    // Input the token receiver.
    input r0 as address.public;
    // Input the token amount.
    input r1 as u64.public;

    // Produces a token record for the token receiver.
    cast r0 0u64 r1 into r2 as token.record;

    // Output the receiver's record.
    output r2 as token.record;

    // Decrement the token amount of the caller publicly.
    finalize self.caller r1;

finalize transfer_public_to_private:
    // Input the token owner.
    input r0 as address.public;
    // Input the token amount.
    input r1 as u64.public;

    // Decrements `account[r0]` by `r1`.
    // If `account[r0]` does not exist, it will be created.
    // If `account[r0] - r1` underflows, `transfer_public_to_private` is reverted.
    decrement account[r0] by r1;
