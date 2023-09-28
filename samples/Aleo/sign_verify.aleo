/* This program follows the GPUv3 license outlined, here: https://github.com/AleoHQ/snarkVM/blob/testnet3/LICENSE.md */

program sign_verify.aleo;

function verify:
    input r0 as signature.private;
    input r1 as address.private;
    input r2 as field.private;
    sign.verify r0 r1 r2 into r3;
    output r3 as boolean.private;

function verify_constant_signature:
    input r0 as address.private;
    input r1 as field.private;
    sign.verify sign169ju4e8s66unu25celqycvsv3k9chdyz4n4sy62tx6wxj0u25vqp58hgu9hwyqc63qzxvjwesf2wz0krcvvw9kd9x0rsk4lwqn2acqhp9v0pdkhx6gvkanuuwratqmxa3du7l43c05253hhed9eg6ppzzfnjt06fpzp6msekdjxd36smjltndmxjndvv9x2uecsgngcwsc2qkns4afd r0 r1 into r2;
    output r2 as boolean.private;

function literal_ops_on_signatures:
    input r0 as boolean.private;
    input r1 as signature.private;
    input r2 as signature.private;
    assert.eq r1 r1;
    assert.eq r2 r2;
    is.eq r1 r2 into r3;
    is.neq r1 r2 into r4;
    ternary r0 r1 r2 into r5;
    output r3 as boolean.private;
    output r4 as boolean.private;
    output r5 as signature.private;
