/* This program follows the license outlined, here: https://github.com/AleoHQ/snarkVM/blob/testnet3/LICENSE.md */

program mapping_operations.aleo;

mapping data:
    key as u8.public;
    value as u8.public;

function insert_contains_remove:
    input r0 as u8.public;
    input r1 as u8.public;
    async insert_contains_remove r0 r1 into r2;
    output r2 as mapping_operations.aleo/insert_contains_remove.future;

finalize insert_contains_remove:
    input r0 as u8.public;
    input r1 as u8.public;
    contains data[r0] into r2;
    assert.eq r2 false;
    set r1 into data[r0];
    contains data[r0] into r3;
    assert.eq r3 true;
    remove data[r0];
    contains data[r0] into r4;
    assert.eq r4 false;

function empty_remove:
    input r0 as u8.public;
    async empty_remove r0 into r1;
    output r1 as mapping_operations.aleo/empty_remove.future;

finalize empty_remove:
    input r0 as u8.public;
    remove data[r0];
