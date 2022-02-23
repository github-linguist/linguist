from starkware.cairo.common.memcpy import memcpy
from starkware.cairo.common.alloc import alloc

func concat_arr{range_check_ptr}(
    acc: felt*,
    acc_len: felt,
    arr: felt*,
    arr_len: felt
    ) -> (res: felt*, res_len: felt):
    alloc_locals
    let (local acc_cpy: felt*) = alloc()

    memcpy(acc_cpy, acc, acc_len)

    memcpy(acc_cpy + acc_len, arr, arr_len)
    return (acc_cpy, acc_len + arr_len)
end
