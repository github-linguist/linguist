/* This program follows the GPUv3 license outlined, here: https://github.com/AleoHQ/snarkVM/blob/testnet3/LICENSE.md */

program arrays_in_finalize.aleo;

struct tree:
    left as [[boolean; 4u32]; 1u32];
    right as [[boolean; 4u32]; 1u32];

function test_arrays:
    input r0 as [[boolean; 4u32]; 1u32].public;
    input r1 as [[boolean; 4u32]; 1u32].public;
    cast r0 r1 into r2 as tree;
    and r2.left[0u32][0u32] r2.right[0u32][0u32] into r3;
    and r2.left[0u32][1u32] r2.right[0u32][1u32] into r4;
    and r2.left[0u32][2u32] r2.right[0u32][2u32] into r5;
    and r2.left[0u32][3u32] r2.right[0u32][3u32] into r6;
    cast r3 r4 r5 r6 into r7 as [boolean; 4u32];
    cast r7 into r8 as [[boolean; 4u32]; 1u32];
    async test_arrays r0 r1 into r9;
    output r0 as [[boolean; 4u32]; 1u32].public;
    output r1 as [[boolean; 4u32]; 1u32].public;
    output r8 as [[boolean; 4u32]; 1u32].public;
    output r2 as tree.private;
    output r9 as arrays_in_finalize.aleo/test_arrays.future;

finalize test_arrays:
    input r0 as [[boolean; 4u32]; 1u32].public;
    input r1 as [[boolean; 4u32]; 1u32].public;
    and r0[0u32][0u32] r1[0u32][0u32] into r2;
    and r0[0u32][1u32] r1[0u32][1u32] into r3;
    and r0[0u32][2u32] r1[0u32][2u32] into r4;
    and r0[0u32][3u32] r1[0u32][3u32] into r5;
    cast r2 r3 r4 r5 into r6 as [boolean; 4u32];
    cast r6 into r7 as [[boolean; 4u32]; 1u32];
    assert.eq r7[0u32][0u32] false;
    assert.eq r7[0u32][1u32] false;
    assert.eq r7[0u32][2u32] false;
    assert.eq r7[0u32][3u32] false;
