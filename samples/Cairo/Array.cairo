from starkware.cairo.common.memcpy import memcpy
from starkware.cairo.common.alloc import alloc

func concat_arr{range_check_ptr}(
    arr1_len: felt,
    arr1: felt*,
    arr2_len: felt,
    arr2: felt*,
  ) -> (res: felt*, res_len: felt):
  alloc_locals

  let (local res: felt*) = alloc()

  memcpy(res, arr1, arr1_len)
  memcpy(res + arr1_len, arr2, arr2_len)

  return (res, arr1_len + arr2_len)
end
