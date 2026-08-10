use cairo_lib::utils::types::words64::{Words64, Words64Trait, reverse_endianness_u64, pow2};
use cairo_lib::utils::types::byte::Byte;
use cairo_lib::utils::array::span_contains;

// @notice Enum with all possible RLP types
// For more info: https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/
#[derive(Drop, PartialEq)]
enum RLPType {
    String: (),
    StringShort: (),
    StringLong: (),
    ListShort: (),
    ListLong: (),
}

#[generate_trait]
impl RLPTypeImpl of RLPTypeTrait {
    // @notice Returns RLPType from the leading byte
    // For more info: https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/
    // @param byte Leading byte
    // @return Result with RLPType
    fn from_byte(byte: Byte) -> Result<RLPType, felt252> {
        if byte <= 0x7f {
            Result::Ok(RLPType::String(()))
        } else if byte <= 0xb7 {
            Result::Ok(RLPType::StringShort(()))
        } else if byte <= 0xbf {
            Result::Ok(RLPType::StringLong(()))
        } else if byte <= 0xf7 {
            Result::Ok(RLPType::ListShort(()))
        } else if byte <= 0xff {
            Result::Ok(RLPType::ListLong(()))
        } else {
            Result::Err('Invalid byte')
        }
    }
}

// @notice Represent a RLP item
#[derive(Drop, PartialEq)]
enum RLPItem {
    Bytes: (Words64, usize),
    // Should be Span<RLPItem> to allow for any depth/recursion, not yet supported by the compiler
    List: Span<(Words64, usize)>
}

// @notice RLP decodes a rlp encoded byte array
// For more info: https://ethereum.org/en/developers/docs/data-structures-and-encoding/rlp/
// @param input RLP encoded input, in little endian 64 bits words
// @return Result with RLPItem and size of the encoded item
fn rlp_decode(input: Words64) -> Result<(RLPItem, usize), felt252> {
    // It's guaranteed to fid in 32 bits, as we are masking with 0xff
    let prefix: u32 = (*input.at(0) & 0xff).try_into().unwrap();

    // It's guaranteed to be a valid RLPType, as we are masking with 0xff
    let rlp_type = RLPTypeTrait::from_byte(prefix.try_into().unwrap()).unwrap();
    match rlp_type {
        RLPType::String(()) => {
            let mut arr = array![prefix.into()];
            Result::Ok((RLPItem::Bytes((arr.span(), 1)), 1))
        },
        RLPType::StringShort(()) => {
            let len = prefix.into() - 0x80;
            let res = input.slice_le(6, len);

            Result::Ok((RLPItem::Bytes((res, len)), 1 + len))
        },
        RLPType::StringLong(()) => {
            let len_len = prefix - 0xb7;
            let len_span = input.slice_le(6, len_len);
            // Enough to store 4.29 GB (fits in u32)
            assert(len_span.len() == 1 && *len_span.at(0) <= 0xffffffff, 'Len of len too big');

            // len fits in 32 bits, confirmed by previous assertion
            let len: u32 = reverse_endianness_u64(*len_span.at(0), Option::Some(len_len.into()))
                .try_into()
                .unwrap();
            let res = input.slice_le(6 - len_len, len);

            Result::Ok((RLPItem::Bytes((res, len)), 1 + len_len + len))
        },
        RLPType::ListShort(()) => {
            let mut len = prefix - 0xc0;
            let mut in = input.slice_le(6, len);
            let res = rlp_decode_list(ref in, len)?;
            Result::Ok((RLPItem::List(res), 1 + len))
        },
        RLPType::ListLong(()) => {
            let len_len = prefix - 0xf7;
            let len_span = input.slice_le(6, len_len);
            // Enough to store 4.29 GB (fits in u32)
            assert(len_span.len() == 1 && *len_span.at(0) <= 0xffffffff, 'Len of len too big');

            // len fits in 32 bits, confirmed by previous assertion
            let len: u32 = reverse_endianness_u64(*len_span.at(0), Option::Some(len_len.into()))
                .try_into()
                .unwrap();
            let mut in = input.slice_le(6 - len_len, len);
            let res = rlp_decode_list(ref in, len)?;

            Result::Ok((RLPItem::List(res), 1 + len_len + len))
        }
    }
}

// @notice RLP decodes into RLPItem::List
// @param input RLP encoded input, in little endian 64 bits words
// @param len Length of the input
// @return Result with a span of the decoded items and the decoded size of each
fn rlp_decode_list(ref input: Words64, len: usize) -> Result<Span<(Words64, usize)>, felt252> {
    let mut i = 0;
    let mut output = ArrayTrait::new();
    let mut total_len = len;

    loop {
        if i >= len {
            break Result::Ok(output.span());
        }

        let (decoded, decoded_len) = match rlp_decode(input) {
            Result::Ok((d, dl)) => (d, dl),
            Result::Err(e) => {
                break Result::Err(e);
            }
        };
        match decoded {
            RLPItem::Bytes(b) => {
                output.append(b);
                let word = decoded_len / 8;
                let reversed = 7 - (decoded_len % 8);
                let next_start = word * 8 + reversed;
                if (total_len - decoded_len != 0) {
                    input = input.slice_le(next_start, total_len - decoded_len);
                }
                total_len -= decoded_len;
            },
            RLPItem::List(_) => {
                panic_with_felt252('Recursive list not supported');
            }
        }
        i += decoded_len;
    }
}

fn rlp_decode_list_lazy(input: Words64, lazy: Span<usize>) -> Result<(RLPItem, usize), felt252> {
    let mut output = ArrayTrait::new();
    let mut lazy_index = 0;

    let list_prefix: u32 = (*input.at(0) & 0xff).try_into().unwrap();
    let list_type = RLPTypeTrait::from_byte(list_prefix.try_into().unwrap()).unwrap();
    let (mut current_input_index, len) = match list_type {
        RLPType::String(()) => {
            return Result::Err('Not a list');
        },
        RLPType::StringShort(()) => {
            return Result::Err('Not a list');
        },
        RLPType::StringLong(()) => {
            return Result::Err('Not a list');
        },
        RLPType::ListShort(()) => (1, list_prefix - 0xc0),
        RLPType::ListLong(()) => {
            let len_len = list_prefix - 0xf7;
            let len_span = input.slice_le(6, len_len);
            // Enough to store 4.29 GB (fits in u32)
            assert(len_span.len() == 1 && *len_span.at(0) <= 0xffffffff, 'Len of len too big');

            // len fits in 32 bits, confirmed by previous assertion
            let len = reverse_endianness_u64(*len_span.at(0), Option::Some(len_len.into()))
                .try_into()
                .unwrap();
            (1 + len_len, len)
        }
    };

    let rlp_byte_len = current_input_index + len;

    loop {
        if output.len() == lazy.len() {
            break Result::Ok((RLPItem::List(output.span()), rlp_byte_len));
        }

        if current_input_index >= rlp_byte_len {
            break Result::Err('Too many items to decode');
        }

        let current_word = current_input_index / 8;

        let pow2_shift = pow2((current_input_index % 8) * 8);
        let prefix = (*input.at(current_word) / pow2_shift) & 0xff;

        let rlp_type = RLPTypeTrait::from_byte(prefix.try_into().unwrap()).unwrap();
        let (item_start_skip, item_len) = match rlp_type {
            RLPType::String(()) => {
                (0, 1)
            },
            RLPType::StringShort(()) => {
                let len = prefix - 0x80;
                (1, len)
            },
            RLPType::StringLong(()) => {
                let len_len = prefix - 0xb7;

                let current_word = (current_input_index + 1) / 8;
                let current_word_offset = 7 - ((current_input_index + 1) % 8);

                let len_span = input
                    .slice_le(current_word * 8 + current_word_offset, len_len.try_into().unwrap());
                // Enough to store 4.29 GB (fits in u32)
                assert(len_span.len() == 1 && *len_span.at(0) <= 0xffffffff, 'Len of len too big');

                // len fits in 32 bits, confirmed by previous assertion
                let len: u32 = reverse_endianness_u64(
                    *len_span.at(0), Option::Some(len_len.try_into().unwrap())
                )
                    .try_into()
                    .unwrap();

                (1 + len_len, len.into())
            },
            RLPType::ListShort(()) => {
                panic_with_felt252('Recursive list not supported')
            },
            RLPType::ListLong(()) => {
                panic_with_felt252('Recursive list not supported')
            }
        };

        current_input_index += item_start_skip.try_into().unwrap();
        if span_contains(lazy, lazy_index) {
            let current_word = current_input_index / 8;
            let current_word_offset = 7 - (current_input_index % 8);
            let start = current_word * 8 + current_word_offset;

            let item_len = item_len.try_into().unwrap();
            let decoded = input.slice_le(start, item_len);
            output.append((decoded, item_len));
        }

        current_input_index += item_len.try_into().unwrap();

        lazy_index += 1;
    }
}
