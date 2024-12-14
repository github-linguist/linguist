# Declare this file as a StarkNet contract and set the required
# builtins.
%lang starknet
%builtins pedersen range_check ecdsa

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.hash import hash2
from starkware.cairo.common.math import assert_lt_felt
from starkware.starknet.common.storage import Storage
from starkware.cairo.common.cairo_builtins import SignatureBuiltin
from starkware.cairo.common.signature import verify_ecdsa_signature
from starkware.cairo.common.alloc import alloc

# Phases:
#   * 0 - coordinator initializes
#   * 1 - users send drop commitments and coordinator disables sending
#   * 2 - coordinator submits key
#   * 3 - users cast votes
#   * 4 - voting period ends

@storage_var
func coordinator() -> (res : felt):
end

@storage_var
func phase() -> (res : felt):
end

@storage_var
func ayes() -> (res : felt):
end

@storage_var
func nays() -> (res : felt):
end

@storage_var
func commitments(user : felt) -> (res : felt):
end

@storage_var
func voprf_key() -> (res : felt):
end

@storage_var
func paid(user : felt) -> (res : felt):
end

@storage_var
func voprf_key_bits(bit : felt) -> (res : felt):
end

@external
func initialize{storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, range_check_ptr}(user : felt):
    let (current_phase) = phase.read()
    assert current_phase = 0
    phase.write(1)
    coordinator.write(user)
    return ()
end

@external
func commit{
        storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, ecdsa_ptr : SignatureBuiltin*,
        range_check_ptr}(user : felt, commitment : felt, sig_r : felt, sig_s : felt):
    let (current_phase) = phase.read()
    assert current_phase = 1
    verify_ecdsa_signature(
        message=commitment, public_key=user, signature_r=sig_r, signature_s=sig_s)
    commitments.write(user, commitment)
    return ()
end

@external
func end_commitment_phase{
        storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, ecdsa_ptr : SignatureBuiltin*,
        range_check_ptr}(sig_r : felt, sig_s : felt):
    let (current_phase) = phase.read()
    assert current_phase = 1
    let (the_coordinator) = coordinator.read()
    verify_ecdsa_signature(
        message=current_phase, public_key=the_coordinator, signature_r=sig_r, signature_s=sig_s)
    phase.write(2)
    return ()
end

@external
func submit_key{
        storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, ecdsa_ptr : SignatureBuiltin*,
        range_check_ptr}(key : felt, sig_r : felt, sig_s : felt):
    let (current_phase) = phase.read()
    assert current_phase = 2
    let (the_coordinator) = coordinator.read()
    verify_ecdsa_signature(
        message=key, public_key=the_coordinator, signature_r=sig_r, signature_s=sig_s)
    phase.write(3)
    voprf_key.write(key)
    return ()
end

@external
func end_voting_phase{
        storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, ecdsa_ptr : SignatureBuiltin*,
        range_check_ptr}(sig_r : felt, sig_s : felt):
    let (current_phase) = phase.read()
    assert current_phase = 3
    let (the_coordinator) = coordinator.read()
    verify_ecdsa_signature(
        message=current_phase, public_key=the_coordinator, signature_r=sig_r, signature_s=sig_s)
    phase.write(4)
    return ()
end

func double_and_add_2_128(arr : felt*, index : felt) -> (res : felt):
    if index == 128:
        return (0)
    else:
        assert arr[index] * (1 - arr[index]) = 0
        let (res) = double_and_add_2_128(arr, index + 1)
        return (arr[index] + 2 * res)
    end
end

func double_ec(x_in, y_in) -> (x_out : felt, y_out : felt):
    let lambda = (3 * x_in * x_in + 1) / (2 * y_in)
    let x = lambda * lambda - 2 * x_in
    let y = lambda * (x_in - x) - y_in
    return (x, y)
end

func double_and_add_ec(
        x_in : felt, y_in : felt, x_base : felt, y_base : felt, infinity_in : felt, arr : felt*,
        index : felt) -> (res : felt):
    alloc_locals
    local x
    local y
    local infinity
    if arr[index] == 1:
        if x_base == x_in:
            if y_base == y_in:
                infinity = 0
                let (x_ret, y_ret) = double_ec(x_in, y_in)
                x = x_ret
                y = y_ret
            else:
                infinity = 1
                x = 0
                y = 0
            end
        else:
            if infinity_in == 1:
                infinity = 0
                x = x_base
                y = y_base
            else:
                infinity = 0
                local lambda = (y_base - y_in) / (x_base - x_in)
                local x_tmp = lambda * lambda - x_in - x_base
                local y_tmp = lambda * (x_in - x_tmp) - y_in
                x = x_tmp
                y = y_tmp
            end
        end
    else:
        infinity = infinity_in
        x = x_in
        y = y_in
    end
    if index == 0:
        return (x)
    else:
        local x_double
        local y_double
        if infinity == 0:
            let (x_ret, y_ret) = double_ec(x, y)
            x_double = x_ret
            y_double = y_ret
        else:
            x_double = 0
            y_double = 0
        end
        return double_and_add_ec(x_double, y_double, x_base, y_base, infinity, arr, index - 1)
    end
end

@external
func cast_vote{
        storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, range_check_ptr,
        ecdsa_ptr : SignatureBuiltin*}(
        user : felt, vote : felt, t_hash_y : felt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
        a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28,
        a29, a30, a31, a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46,
        a47, a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63, a64,
        a65, a66, a67, a68, a69, a70, a71, a72, a73, a74, a75, a76, a77, a78, a79, a80, a81, a82,
        a83, a84, a85, a86, a87, a88, a89, a90, a91, a92, a93, a94, a95, a96, a97, a98, a99, a100,
        a101, a102, a103, a104, a105, a106, a107, a108, a109, a110, a111, a112, a113, a114, a115,
        a116, a117, a118, a119, a120, a121, a122, a123, a124, a125, a126, a127):
    alloc_locals

    let (current_phase) = phase.read()
    assert current_phase = 3

    let (local key) = voprf_key.read()
    let (commitment) = commitments.read(user)
    local commitment = commitment
    let (local t_hash) = hash2{hash_ptr=pedersen_ptr}(user, 0)
    assert t_hash_y * t_hash_y = t_hash * t_hash * t_hash + t_hash + 3141592653589793238462643383279502884197169399375105820974944592307816406665

    let (paid_commitment) = paid.read(user)
    assert paid_commitment = 0
    paid.write(user, 1)

    # Make sure vote is either 0 (No) or 1 (Yes)
    assert vote * (1 - vote) = 0

    if vote == 1:
        let (num_yes) = ayes.read()
        ayes.write(num_yes + 1)
        tempvar storage_ptr : Storage* = storage_ptr
        tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
        tempvar range_check_ptr = range_check_ptr
    else:
        if vote == 0:
            let (num_no) = nays.read()
            nays.write(num_no + 1)
            tempvar storage_ptr : Storage* = storage_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
        else:
            tempvar storage_ptr : Storage* = storage_ptr
            tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
            tempvar range_check_ptr = range_check_ptr
        end
        tempvar storage_ptr : Storage* = storage_ptr
        tempvar pedersen_ptr : HashBuiltin* = pedersen_ptr
        tempvar range_check_ptr = range_check_ptr
    end

    local storage_ptr : Storage* = storage_ptr
    local pedersen_ptr : HashBuiltin* = pedersen_ptr
    local range_check_ptr = range_check_ptr

    let (local key_bits : felt*) = alloc()
    key_bits[0] = a0
    key_bits[1] = a1
    key_bits[2] = a2
    key_bits[3] = a3
    key_bits[4] = a4
    key_bits[5] = a5
    key_bits[6] = a6
    key_bits[7] = a7
    key_bits[8] = a8
    key_bits[9] = a9
    key_bits[10] = a10
    key_bits[11] = a11
    key_bits[12] = a12
    key_bits[13] = a13
    key_bits[14] = a14
    key_bits[15] = a15
    key_bits[16] = a16
    key_bits[17] = a17
    key_bits[18] = a18
    key_bits[19] = a19
    key_bits[20] = a20
    key_bits[21] = a21
    key_bits[22] = a22
    key_bits[23] = a23
    key_bits[24] = a24
    key_bits[25] = a25
    key_bits[26] = a26
    key_bits[27] = a27
    key_bits[28] = a28
    key_bits[29] = a29
    key_bits[30] = a30
    key_bits[31] = a31
    key_bits[32] = a32
    key_bits[33] = a33
    key_bits[34] = a34
    key_bits[35] = a35
    key_bits[36] = a36
    key_bits[37] = a37
    key_bits[38] = a38
    key_bits[39] = a39
    key_bits[40] = a40
    key_bits[41] = a41
    key_bits[42] = a42
    key_bits[43] = a43
    key_bits[44] = a44
    key_bits[45] = a45
    key_bits[46] = a46
    key_bits[47] = a47
    key_bits[48] = a48
    key_bits[49] = a49
    key_bits[50] = a50
    key_bits[51] = a51
    key_bits[52] = a52
    key_bits[53] = a53
    key_bits[54] = a54
    key_bits[55] = a55
    key_bits[56] = a56
    key_bits[57] = a57
    key_bits[58] = a58
    key_bits[59] = a59
    key_bits[60] = a60
    key_bits[61] = a61
    key_bits[62] = a62
    key_bits[63] = a63
    key_bits[64] = a64
    key_bits[65] = a65
    key_bits[66] = a66
    key_bits[67] = a67
    key_bits[68] = a68
    key_bits[69] = a69
    key_bits[70] = a70
    key_bits[71] = a71
    key_bits[72] = a72
    key_bits[73] = a73
    key_bits[74] = a74
    key_bits[75] = a75
    key_bits[76] = a76
    key_bits[77] = a77
    key_bits[78] = a78
    key_bits[79] = a79
    key_bits[80] = a80
    key_bits[81] = a81
    key_bits[82] = a82
    key_bits[83] = a83
    key_bits[84] = a84
    key_bits[85] = a85
    key_bits[86] = a86
    key_bits[87] = a87
    key_bits[88] = a88
    key_bits[89] = a89
    key_bits[90] = a90
    key_bits[91] = a91
    key_bits[92] = a92
    key_bits[93] = a93
    key_bits[94] = a94
    key_bits[95] = a95
    key_bits[96] = a96
    key_bits[97] = a97
    key_bits[98] = a98
    key_bits[99] = a99
    key_bits[100] = a100
    key_bits[101] = a101
    key_bits[102] = a102
    key_bits[103] = a103
    key_bits[104] = a104
    key_bits[105] = a105
    key_bits[106] = a106
    key_bits[107] = a107
    key_bits[108] = a108
    key_bits[109] = a109
    key_bits[110] = a110
    key_bits[111] = a111
    key_bits[112] = a112
    key_bits[113] = a113
    key_bits[114] = a114
    key_bits[115] = a115
    key_bits[116] = a116
    key_bits[117] = a117
    key_bits[118] = a118
    key_bits[119] = a119
    key_bits[120] = a120
    key_bits[121] = a121
    key_bits[122] = a122
    key_bits[123] = a123
    key_bits[124] = a124
    key_bits[125] = a125
    key_bits[126] = a126
    key_bits[127] = a127
    let (key_computed) = double_and_add_2_128(key_bits, 0)
    assert key_computed = key

    let (commitment_computed) = double_and_add_ec(0, 0, t_hash, t_hash_y, 1, key_bits, 127)
    assert commitment_computed = commitment
    return ()
end

@view
func get_result{storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (
        num_yes : felt, num_no : felt):
    let (current_phase) = phase.read()
    assert current_phase = 4
    let (num_yes) = ayes.read()
    let (num_no) = nays.read()
    return (num_yes, num_no)
end

@view
func get_phase{storage_ptr : Storage*, pedersen_ptr : HashBuiltin*, range_check_ptr}() -> (
        res : felt):
    let (res) = phase.read()
    return (res)
end
