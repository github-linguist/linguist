%lang starknet

// Starkware dependencies
from starkware.cairo.common.alloc import alloc
from starkware.cairo.common.registers import get_fp_and_pc
from starkware.cairo.common.cairo_builtins import HashBuiltin, BitwiseBuiltin
from starkware.cairo.common.math import assert_nn_le, unsigned_div_rem
from starkware.cairo.common.math_cmp import is_le_felt
from starkware.cairo.common.memcpy import memcpy
from starkware.cairo.common.memset import memset
from starkware.cairo.common.pow import pow
from starkware.cairo.common.bool import FALSE

// Internal dependencies
from utils.sha_256.packed_sha256 import (
    BLOCK_SIZE,
    compute_message_schedule,
    sha2_compress,
    get_round_constants,
)
from utils.utils import Helpers

// @title SHA2-256 Precompile related functions.
// @notice This file contains the logic required to run the SHA2-256 precompile
// @author @ftupas
// @custom:namespace PrecompileSHA256
namespace PrecompileSHA256 {
    const PRECOMPILE_ADDRESS = 0x02;
    const GAS_COST_SHA256 = 60;

    // @notice Run the precompile.
    // @param input_len The length of input array.
    // @param input The input array.
    // @return output_len The output length.
    // @return output The output array.
    // @return gas_used The gas usage of precompile.
    func run{
        syscall_ptr: felt*,
        pedersen_ptr: HashBuiltin*,
        range_check_ptr,
        bitwise_ptr: BitwiseBuiltin*,
    }(_address: felt, input_len: felt, input: felt*) -> (
        output_len: felt, output: felt*, gas_used: felt, reverted: felt
    ) {
        alloc_locals;

        // Copy input array
        let (arr: felt*) = alloc();
        memcpy(arr, input, input_len);

        // Zero-pad bytes array
        // ex. 'rld\x00'
        let (q: felt, r: felt) = unsigned_div_rem(input_len, 4);
        if (r != 0) {
            // Append zero elements at the end of array
            memset(arr + input_len, 0, 4 - r);
            tempvar arr_len = (q + 1) * 4;
        } else {
            tempvar arr_len = q * 4;
        }

        // Prepare input bytes array to words of 32 bits (big endian).
        let (prepared_input: felt*) = alloc();
        let (prepared_input_len, prepared_input: felt*) = Helpers.bytes_to_bytes4_array(
            arr_len, arr, 0, prepared_input
        );

        // Compute hash, use input_len for number of bytes
        let (local sha256_ptr: felt*) = alloc();
        let sha256_ptr_start = sha256_ptr;
        let (hash) = SHA256.sha256{sha256_ptr=sha256_ptr}(prepared_input, input_len);

        // Finalize hash
        SHA256.finalize_sha256(sha256_ptr_start=sha256_ptr_start, sha256_ptr_end=sha256_ptr);

        // Split words and return bytes hash code.
        let (hash_bytes_array: felt*) = alloc();
        let (_, hash_bytes_array: felt*) = Helpers.bytes4_array_to_bytes(
            8, hash, 0, hash_bytes_array
        );
        let (minimum_word_size) = Helpers.minimum_word_count(input_len);
        return (32, hash_bytes_array, 12 * minimum_word_size + GAS_COST_SHA256, 0);
    }
}

namespace SHA256 {
    const SHA256_INPUT_CHUNK_SIZE_FELTS = 16;
    const SHA256_INPUT_CHUNK_SIZE_BYTES = 64;
    const SHA256_STATE_SIZE_FELTS = 8;
    // Each instance consists of 16 words of message, 8 words for the input state and 8 words
    // for the output state.
    const SHA256_INSTANCE_SIZE = SHA256_INPUT_CHUNK_SIZE_FELTS + 2 * SHA256_STATE_SIZE_FELTS;

    // Computes SHA256 of 'input'. Inputs of arbitrary length are supported.
    // To use this function, split the input into (up to) 14 words of 32 bits (big endian).
    // For example, to compute sha256('Hello world'), use:
    //   input = [1214606444, 1864398703, 1919706112]
    // where:
    //   1214606444 == int.from_bytes(b'Hell', 'big')
    //   1864398703 == int.from_bytes(b'o wo', 'big')
    //   1919706112 == int.from_bytes(b'rld\x00', 'big')  # Note the '\x00' padding.
    //
    // block layout:
    // 0 - 15: Message
    // 16 - 23: Input State
    // 24 - 32: Output
    //
    // output is an array of 8 32-bit words (big endian).
    //
    // Note: You must call finalize_sha2() at the end of the program. Otherwise, this function
    // is not sound and a malicious prover may return a wrong result.
    // Note: the interface of this function may change in the future.
    func sha256{range_check_ptr, sha256_ptr: felt*}(data: felt*, n_bytes: felt) -> (output: felt*) {
        alloc_locals;

        // Set the initial input state to IV.
        assert sha256_ptr[16] = 0x6A09E667;
        assert sha256_ptr[17] = 0xBB67AE85;
        assert sha256_ptr[18] = 0x3C6EF372;
        assert sha256_ptr[19] = 0xA54FF53A;
        assert sha256_ptr[20] = 0x510E527F;
        assert sha256_ptr[21] = 0x9B05688C;
        assert sha256_ptr[22] = 0x1F83D9AB;
        assert sha256_ptr[23] = 0x5BE0CD19;

        sha256_inner(data=data, n_bytes=n_bytes, total_bytes=n_bytes);

        // Set `output` to the start of the final state.
        let output = sha256_ptr;
        // Set `sha256_ptr` to the end of the output state.
        let sha256_ptr = sha256_ptr + SHA256_STATE_SIZE_FELTS;
        return (output,);
    }

    // Computes the sha256 hash of the input chunk from `message` to `message + SHA256_INPUT_CHUNK_SIZE_FELTS`
    func _sha256_chunk{range_check_ptr, sha256_start: felt*, state: felt*, output: felt*}() {
        %{
            from starkware.cairo.common.cairo_sha256.sha256_utils import (
                compute_message_schedule, sha2_compress_function)

            _sha256_input_chunk_size_felts = int(ids.SHA256_INPUT_CHUNK_SIZE_FELTS)
            assert 0 <= _sha256_input_chunk_size_felts < 100
            _sha256_state_size_felts = int(ids.SHA256_STATE_SIZE_FELTS)
            assert 0 <= _sha256_state_size_felts < 100
            w = compute_message_schedule(memory.get_range(
                ids.sha256_start, _sha256_input_chunk_size_felts))
            new_state = sha2_compress_function(memory.get_range(ids.state, _sha256_state_size_felts), w)
            segments.write_arg(ids.output, new_state)
        %}
        return ();
    }

    // Inner loop for sha256. `sha256_ptr` points to the start of the block.
    func sha256_inner{range_check_ptr, sha256_ptr: felt*}(
        data: felt*, n_bytes: felt, total_bytes: felt
    ) {
        alloc_locals;

        let message = sha256_ptr;
        let state = sha256_ptr + SHA256_INPUT_CHUNK_SIZE_FELTS;
        let output = state + SHA256_STATE_SIZE_FELTS;

        let zero_bytes = is_le_felt(n_bytes, 0);
        let zero_total_bytes = is_le_felt(total_bytes, 0);

        // If the previous message block was full we are still missing "1" at the end of the message
        let (_, r_div_by_64) = unsigned_div_rem(total_bytes, 64);
        let missing_bit_one = is_le_felt(r_div_by_64, 0);

        // This works for 0 total bytes too, because zero_chunk will be -1 and, therefore, not 0.
        let zero_chunk = zero_bytes - zero_total_bytes - missing_bit_one;

        let is_last_block = is_le_felt(n_bytes, 55);
        if (is_last_block == FALSE) {
            let (q, r) = unsigned_div_rem(n_bytes, SHA256_INPUT_CHUNK_SIZE_BYTES);
            let is_remainder_block = is_le_felt(q, 0);
            if (is_remainder_block == FALSE) {
                _sha256_input(
                    data, SHA256_INPUT_CHUNK_SIZE_BYTES, SHA256_INPUT_CHUNK_SIZE_FELTS, 0
                );
                _sha256_chunk{sha256_start=message, state=state, output=output}();

                let sha256_ptr = sha256_ptr + SHA256_STATE_SIZE_FELTS;
                memcpy(
                    output + SHA256_STATE_SIZE_FELTS + SHA256_INPUT_CHUNK_SIZE_FELTS,
                    output,
                    SHA256_STATE_SIZE_FELTS,
                );
                let sha256_ptr = sha256_ptr + SHA256_STATE_SIZE_FELTS;

                return sha256_inner(
                    data=data + SHA256_INPUT_CHUNK_SIZE_FELTS,
                    n_bytes=n_bytes - SHA256_INPUT_CHUNK_SIZE_BYTES,
                    total_bytes=total_bytes,
                );
            } else {
                _sha256_input(data, r, SHA256_INPUT_CHUNK_SIZE_FELTS, 0);
                _sha256_chunk{sha256_start=message, state=state, output=output}();

                let sha256_ptr = sha256_ptr + SHA256_STATE_SIZE_FELTS;
                memcpy(
                    output + SHA256_STATE_SIZE_FELTS + SHA256_INPUT_CHUNK_SIZE_FELTS,
                    output,
                    SHA256_STATE_SIZE_FELTS,
                );
                let sha256_ptr = sha256_ptr + SHA256_STATE_SIZE_FELTS;

                return sha256_inner(data=data, n_bytes=n_bytes - r, total_bytes=total_bytes);
            }
        }

        _sha256_input(data, n_bytes, SHA256_INPUT_CHUNK_SIZE_FELTS - 2, zero_chunk);
        // Append the original message length at the end of the message block as a 64-bit big-endian integer.
        assert sha256_ptr[0] = 0;
        assert sha256_ptr[1] = total_bytes * 8;
        let sha256_ptr = sha256_ptr + 2;
        _sha256_chunk{sha256_start=message, state=state, output=output}();
        let sha256_ptr = sha256_ptr + SHA256_STATE_SIZE_FELTS;

        return ();
    }

    // 1. Encode the input to binary using UTF-8 and append a single '1' to it.
    // 2. Prepend that binary to the message block.
    func _sha256_input{range_check_ptr, sha256_ptr: felt*}(
        input: felt*, n_bytes: felt, n_words: felt, pad_chunk: felt
    ) {
        alloc_locals;

        local full_word;
        %{ ids.full_word = int(ids.n_bytes >= 4) %}

        if (full_word != 0) {
            assert sha256_ptr[0] = input[0];
            let sha256_ptr = sha256_ptr + 1;
            return _sha256_input(
                input=input + 1, n_bytes=n_bytes - 4, n_words=n_words - 1, pad_chunk=pad_chunk
            );
        }

        if (n_words == 0) {
            return ();
        }

        if (n_bytes == 0 and pad_chunk == 1) {
            // Add zeros between the encoded message and the length integer so that the message block is a multiple of 512.
            memset(dst=sha256_ptr, value=0, n=n_words);
            let sha256_ptr = sha256_ptr + n_words;
            return ();
        }

        if (n_bytes == 0) {
            // This is the last input word, so we should add a byte '0x80' at the end and fill the rest with zeros.
            assert sha256_ptr[0] = 0x80000000;
            // Add zeros between the encoded message and the length integer so that the message block is a multiple of 512.
            memset(dst=sha256_ptr + 1, value=0, n=n_words - 1);
            let sha256_ptr = sha256_ptr + n_words;
            return ();
        }

        assert_nn_le(n_bytes, 3);
        let (padding) = pow(256, 3 - n_bytes);
        local range_check_ptr = range_check_ptr;

        assert sha256_ptr[0] = input[0] + padding * 0x80;

        memset(dst=sha256_ptr + 1, value=0, n=n_words - 1);
        let sha256_ptr = sha256_ptr + n_words;
        return ();
    }

    // Handles n blocks of BLOCK_SIZE SHA256 instances.
    // Taken from: https://github.com/starkware-libs/cairo-examples/blob/0d88b41bffe3de112d98986b8b0afa795f9d67a0/sha256/sha256.cairo#L102
    func _finalize_sha256_inner{range_check_ptr, bitwise_ptr: BitwiseBuiltin*}(
        sha256_ptr: felt*, n: felt, round_constants: felt*
    ) {
        if (n == 0) {
            return ();
        }

        alloc_locals;

        local MAX_VALUE = 2 ** 32 - 1;

        let sha256_start = sha256_ptr;

        let (local message_start: felt*) = alloc();
        let (local input_state_start: felt*) = alloc();

        // Handle message.

        tempvar message = message_start;
        tempvar sha256_ptr = sha256_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar m = SHA256_INPUT_CHUNK_SIZE_FELTS;

        message_loop:
        tempvar x0 = sha256_ptr[0 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 0] = x0;
        assert [range_check_ptr + 1] = MAX_VALUE - x0;
        tempvar x1 = sha256_ptr[1 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 2] = x1;
        assert [range_check_ptr + 3] = MAX_VALUE - x1;
        tempvar x2 = sha256_ptr[2 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 4] = x2;
        assert [range_check_ptr + 5] = MAX_VALUE - x2;
        tempvar x3 = sha256_ptr[3 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 6] = x3;
        assert [range_check_ptr + 7] = MAX_VALUE - x3;
        tempvar x4 = sha256_ptr[4 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 8] = x4;
        assert [range_check_ptr + 9] = MAX_VALUE - x4;
        tempvar x5 = sha256_ptr[5 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 10] = x5;
        assert [range_check_ptr + 11] = MAX_VALUE - x5;
        tempvar x6 = sha256_ptr[6 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 12] = x6;
        assert [range_check_ptr + 13] = MAX_VALUE - x6;
        assert message[0] = x0 + 2 ** 35 * x1 + 2 ** (35 * 2) * x2 + 2 ** (35 * 3) * x3 + 2 ** (
            35 * 4
        ) * x4 + 2 ** (35 * 5) * x5 + 2 ** (35 * 6) * x6;

        tempvar message = message + 1;
        tempvar sha256_ptr = sha256_ptr + 1;
        tempvar range_check_ptr = range_check_ptr + 14;
        tempvar m = m - 1;
        jmp message_loop if m != 0;

        // Handle input state.

        tempvar input_state = input_state_start;
        tempvar sha256_ptr = sha256_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar m = SHA256_STATE_SIZE_FELTS;

        input_state_loop:
        tempvar x0 = sha256_ptr[0 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 0] = x0;
        assert [range_check_ptr + 1] = MAX_VALUE - x0;
        tempvar x1 = sha256_ptr[1 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 2] = x1;
        assert [range_check_ptr + 3] = MAX_VALUE - x1;
        tempvar x2 = sha256_ptr[2 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 4] = x2;
        assert [range_check_ptr + 5] = MAX_VALUE - x2;
        tempvar x3 = sha256_ptr[3 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 6] = x3;
        assert [range_check_ptr + 7] = MAX_VALUE - x3;
        tempvar x4 = sha256_ptr[4 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 8] = x4;
        assert [range_check_ptr + 9] = MAX_VALUE - x4;
        tempvar x5 = sha256_ptr[5 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 10] = x5;
        assert [range_check_ptr + 11] = MAX_VALUE - x5;
        tempvar x6 = sha256_ptr[6 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 12] = x6;
        assert [range_check_ptr + 13] = MAX_VALUE - x6;
        assert input_state[0] = x0 + 2 ** 35 * x1 + 2 ** (35 * 2) * x2 + 2 ** (35 * 3) * x3 + 2 ** (
            35 * 4
        ) * x4 + 2 ** (35 * 5) * x5 + 2 ** (35 * 6) * x6;

        tempvar input_state = input_state + 1;
        tempvar sha256_ptr = sha256_ptr + 1;
        tempvar range_check_ptr = range_check_ptr + 14;
        tempvar m = m - 1;
        jmp input_state_loop if m != 0;

        // Run sha256 on the 7 instances.

        local sha256_ptr: felt* = sha256_ptr;
        local range_check_ptr = range_check_ptr;
        compute_message_schedule(message_start);
        let (outputs) = sha2_compress(input_state_start, message_start, round_constants);
        local bitwise_ptr: BitwiseBuiltin* = bitwise_ptr;

        // Handle outputs.

        tempvar outputs = outputs;
        tempvar sha256_ptr = sha256_ptr;
        tempvar range_check_ptr = range_check_ptr;
        tempvar m = SHA256_STATE_SIZE_FELTS;

        output_loop:
        tempvar x0 = sha256_ptr[0 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr] = x0;
        assert [range_check_ptr + 1] = MAX_VALUE - x0;
        tempvar x1 = sha256_ptr[1 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 2] = x1;
        assert [range_check_ptr + 3] = MAX_VALUE - x1;
        tempvar x2 = sha256_ptr[2 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 4] = x2;
        assert [range_check_ptr + 5] = MAX_VALUE - x2;
        tempvar x3 = sha256_ptr[3 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 6] = x3;
        assert [range_check_ptr + 7] = MAX_VALUE - x3;
        tempvar x4 = sha256_ptr[4 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 8] = x4;
        assert [range_check_ptr + 9] = MAX_VALUE - x4;
        tempvar x5 = sha256_ptr[5 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 10] = x5;
        assert [range_check_ptr + 11] = MAX_VALUE - x5;
        tempvar x6 = sha256_ptr[6 * SHA256_INSTANCE_SIZE];
        assert [range_check_ptr + 12] = x6;
        assert [range_check_ptr + 13] = MAX_VALUE - x6;

        assert outputs[0] = x0 + 2 ** 35 * x1 + 2 ** (35 * 2) * x2 + 2 ** (35 * 3) * x3 + 2 ** (
            35 * 4
        ) * x4 + 2 ** (35 * 5) * x5 + 2 ** (35 * 6) * x6;

        tempvar outputs = outputs + 1;
        tempvar sha256_ptr = sha256_ptr + 1;
        tempvar range_check_ptr = range_check_ptr + 14;
        tempvar m = m - 1;
        jmp output_loop if m != 0;

        return _finalize_sha256_inner(
            sha256_ptr=sha256_start + SHA256_INSTANCE_SIZE * BLOCK_SIZE,
            n=n - 1,
            round_constants=round_constants,
        );
    }

    // Verifies that the results of sha256() are valid.
    // Taken from: https://github.com/starkware-libs/cairo-examples/blob/0d88b41bffe3de112d98986b8b0afa795f9d67a0/sha256/sha256.cairo#L246
    func finalize_sha256{range_check_ptr, bitwise_ptr: BitwiseBuiltin*}(
        sha256_ptr_start: felt*, sha256_ptr_end: felt*
    ) {
        alloc_locals;

        let (__fp__, _) = get_fp_and_pc();

        let (round_constants) = get_round_constants();

        // We reuse the output state of the previous chunk as input to the next.
        tempvar n = (sha256_ptr_end - sha256_ptr_start) / SHA256_INSTANCE_SIZE;
        if (n == 0) {
            return ();
        }

        %{
            # Add dummy pairs of input and output.
            from starkware.cairo.common.cairo_sha256.sha256_utils import (
                IV, compute_message_schedule, sha2_compress_function)

            _block_size = int(ids.BLOCK_SIZE)
            assert 0 <= _block_size < 20
            _sha256_input_chunk_size_felts = int(ids.SHA256_INPUT_CHUNK_SIZE_FELTS)
            assert 0 <= _sha256_input_chunk_size_felts < 100

            message = [0] * _sha256_input_chunk_size_felts
            w = compute_message_schedule(message)
            output = sha2_compress_function(IV, w)
            padding = (message + IV + output) * (_block_size - 1)
            segments.write_arg(ids.sha256_ptr_end, padding)
        %}

        // Compute the amount of blocks (rounded up).
        let (local q, r) = unsigned_div_rem(n + BLOCK_SIZE - 1, BLOCK_SIZE);
        _finalize_sha256_inner(sha256_ptr_start, n=q, round_constants=round_constants);
        return ();
    }
}
