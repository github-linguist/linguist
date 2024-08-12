use aiken/builtin
use aiken/bytearray
use aiken/dict
use aiken/hash.{blake2b_256, sha2_256}
use aiken/interval.{Finite, Interval, IntervalBound}
use aiken/list
use aiken/math.{pow2}
use aiken/merkle_patricia_forestry.{Proof, from_root, insert}
use aiken/transaction.{
  InlineDatum, Mint, Output, ScriptContext, Spend, Transaction,
} as tx
use aiken/transaction/credential.{Address, Inline, ScriptCredential}
use aiken/transaction/value.{from_minted_value}
use fortuna
use fortuna/parameters.{
  epoch_number, halving_number, initial_payout, miner_threshold,
  miner_voting_period, supply_threshold, vote_threshold,
}
use fortuna/types.{
  BurnToken, FinalizeNomination, Genesis, MineTuna, Mining, NominateUpgrade,
  Nominated, Redeem, Statev2, TunaAction, TunaUpgradeProcess, VotingToken,
}
use fortuna/utils.{get_inline_datum, integer_to_bytes, resolve_output_reference}
use fortunav2.{
  big_tuna_length, counter_length, expect_first, flip_hash, genesis_v2,
  quantity_of, tokens, vote, voting_period,
}
use hardfork.{calculate_emission}
use hardfork/hftypes.{Lock, NftForkAction}

type TunaSpendAction {
  TokenVoteFor
  TokenVoteAgainst
  MinerVoteFor { output_index: Int, block_number: Int }
  TransitionState { block_number: Int }
}

type Miner {
  Pkh(ByteArray, Data)
  Nft { policy: ByteArray, name: ByteArray, output_index: Int, extra: Data }
}

type MineAction {
  MinePow(ByteArray, Miner, Proof)
  Upgrade
}

type TargetState {
  nonce: ByteArray,
  miner: ByteArray,
  block_number: Int,
  current_hash: ByteArray,
  leading_zeros: Int,
  target_number: Int,
  epoch_time: Int,
}

validator(fortuna_v1_hash: Data, fork_script_hash: Data) {
  fn tuna(redeemer: TunaAction, ctx: ScriptContext) -> Bool {
    expect ScriptContext { transaction: tx, purpose: Mint(own_policy) } = ctx

    // TODO: replace value.tokens with faster function
    when redeemer is {
      Genesis -> genesis_v2(tx, own_policy, fortuna_v1_hash, fork_script_hash)

      Redeem -> {
        let Transaction { mint, redeemers, .. } = tx

        let withdraw_purpose =
          tx.WithdrawFrom(
            Inline(ScriptCredential(fork_script_hash |> builtin.un_b_data)),
          )

        // Check fork contract for locked amount of v1 TUNA
        expect Lock { locking_amount, .. }: NftForkAction =
          expect_first(redeemers, withdraw_purpose)

        expect [Pair(mint_name, mint_amount)] =
          mint
            |> value.from_minted_value
            |> tokens(own_policy)

        and {
          mint_name == fortuna.token_name,
          // Allow for minting that same amount
          mint_amount == locking_amount,
          // redundant but cheap enough check that I'll just leave it in
          mint_amount > 0,
        }
      }

      MineTuna(input_ref, block_number) -> {
        let Transaction { inputs, mint, .. } = tx

        let minted_names = mint |> from_minted_value |> tokens(own_policy)

        // Fortuna input regardless of version
        let Output { address: spend_address, value: spend_value, .. } =
          resolve_output_reference(inputs, input_ref)

        // Get whatever the current spend cred is
        expect ScriptCredential(spend_cred) = spend_address.payment_credential

        let big_tuna_name =
          bytearray.concat(fortunav2.big_tuna_prefix, spend_cred)

        expect [Pair(token1, 1), Pair(token2, 1)] =
          spend_value |> tokens(own_policy)

        let in_token_name =
          // By ledger rules we should only hit this first since token names
          // are lexographically sorted
          if token2 == big_tuna_name {
            token1
          } else if token1 == big_tuna_name {
            token2
          } else {
            fail @"Big Tuna not found in input value"
          }

        let expected_in_token_name =
          integer_to_bytes(block_number, "")
            |> bytearray.concat(fortunav2.counter_prefix, _)

        let expected_out_token_name =
          integer_to_bytes(block_number + 1, "")
            |> bytearray.concat(fortunav2.counter_prefix, _)

        // Minted amount of TUNA halves every halving number
        let halving_exponent = block_number / halving_number

        // We don't check the output because we expect the spend script to take care of 
        // transporting the tokens to the next output
        // As a backup in case of a spend script exploit, we still check
        // that the input has the correct 2 tokens (Lord Tuna and counter) and the payment credential is contained 
        // within the Lord Tuna token name.
        let expected_mint = {
          let tail =
            if halving_exponent > 32 {
              []
            } else {
              [
                Pair(fortuna.token_name,
                initial_payout / pow2(halving_exponent)),
              ]
            }

          // Here we burn the previous counter and mint the new one
          // Thus ensuring the minting policy is always aware of the current block number
          if
          expected_in_token_name
            |> builtin.less_than_bytearray(expected_out_token_name){
          
            [
              Pair(expected_in_token_name, -1),
              Pair(expected_out_token_name, 1),
              ..tail
            ]
          } else {
            [
              Pair(expected_out_token_name, 1),
              Pair(expected_in_token_name, -1),
              ..tail
            ]
          }
        }

        and {
          // input value check
          in_token_name == expected_in_token_name,
          // minted counter check
          minted_names == expected_mint,
        }
      }

      NominateUpgrade(script_hash, output_index) -> {
        let Transaction { inputs, mint, outputs, validity_range, .. } = tx

        let valid_cred = ScriptCredential(script_hash)

        // Mint nomination token
        expect [Pair(nominated_contract_token, 1)] =
          mint |> from_minted_value |> tokens(own_policy)

        let expected_value =
          value.from_asset(own_policy, nominated_contract_token, 1)

        let anti_script_hash = flip_hash(script_hash)

        expect Interval {
          upper_bound: IntervalBound { bound_type: Finite(upper_range), .. },
          lower_bound: IntervalBound { bound_type: Finite(lower_range), .. },
        } = validity_range

        let expected_datum =
          InlineDatum(
            Nominated {
              script_hash,
              for_count: 0,
              anti_script_hash,
              against_count: 0,
              deadline: lower_range + voting_period(),
            },
          )

        let Output {
          address,
          value: nominated_value,
          datum: nominated_datum,
          ..
        } = utils.list_at(outputs, output_index)

        and {
          nominated_contract_token == bytearray.concat(
            fortunav2.nominated_prefix,
            script_hash,
          ),
          address.payment_credential == ScriptCredential(own_policy),
          expected_value == value.without_lovelace(nominated_value),
          expected_datum == nominated_datum,
          upper_range - lower_range <= 180000,
          // Check contract is executable
          list.any(
            inputs,
            fn(input) { input.output.address.payment_credential == valid_cred },
          ),
        }
      }

      VotingToken(ref_input) -> {
        let Transaction { reference_inputs, mint, .. } = tx

        let Output {
          address: spend_address,
          value: spend_value,
          datum: spend_datum,
          ..
        } = resolve_output_reference(reference_inputs, ref_input)

        expect Nominated { script_hash, anti_script_hash, .. }: TunaUpgradeProcess =
          get_inline_datum(spend_datum)

        expect [Pair(voting_token, 1)] =
          mint |> from_minted_value |> tokens(own_policy)

        and {
          // Is from this contract
          spend_address.payment_credential == ScriptCredential(own_policy),
          // Has NFT for the nominated contract
          quantity_of(
            spend_value |> value.to_dict |> dict.to_pairs,
            own_policy,
            bytearray.concat(fortunav2.nominated_prefix, script_hash),
          ) == 1,
          or {
            // Is for the nominated contract
            voting_token == script_hash,
            // Is against the nominated contract
            voting_token == anti_script_hash,
          },
        }
      }

      BurnToken -> {
        let tuna_and_master_tokens_burned =
          fn(name) { or {
              and {
                builtin.slice_bytearray(0, big_tuna_length, name) == fortuna.token_name,
                or {
                  builtin.length_of_bytearray(name) == 4,
                  builtin.length_of_bytearray(name) == 32,
                },
              },
              builtin.slice_bytearray(0, counter_length, name) == fortunav2.counter_prefix,
            } }

        tx.mint
          |> from_minted_value
          |> tokens(own_policy)
          |> list.all(
              fn(pair) {
                let Pair(name, quantity) = pair
                quantity < 0 && !tuna_and_master_tokens_burned(name)
              },
            )
      }

      FinalizeNomination(own_input, tuna_input, output_index, block_number) -> {
        let Transaction { inputs, outputs, mint, .. } = tx

        let Output { address: in_address, value: in_value, datum: in_datum, .. } =
          resolve_output_reference(inputs, own_input)

        let Output {
          address: tuna_in_address,
          value: tuna_in_value,
          datum: in_tuna_datum,
          ..
        } = resolve_output_reference(inputs, tuna_input)

        expect ScriptCredential(current_spend_validator) =
          tuna_in_address.payment_credential

        expect Mining {
          script_hash,
          miner_support_count,
          block_height_deadline,
        }: TunaUpgradeProcess = get_inline_datum(in_datum)

        let Output {
          value: out_value,
          datum: out_datum,
          address: out_address,
          ..
        } = utils.list_at(outputs, output_index)

        let block_number_as_bytes = integer_to_bytes(block_number, "")
        let expected_miner_votes = epoch_number * miner_threshold / 1000

        let expected_in_value =
          value.from_asset(
            own_policy,
            bytearray.concat(fortunav2.nominated_prefix, script_hash),
            1,
          )

        let expected_in_tuna_value =
          value.from_asset(
            own_policy,
            bytearray.concat(fortunav2.big_tuna_prefix, current_spend_validator),
            1,
          )
            |> value.add(
                own_policy,
                bytearray.concat(
                  fortunav2.counter_prefix,
                  block_number_as_bytes,
                ),
                1,
              )

        let expected_output_value =
          value.from_asset(
            own_policy,
            bytearray.concat(fortunav2.big_tuna_prefix, script_hash),
            1,
          )
            |> value.add(
                own_policy,
                bytearray.concat(
                  fortunav2.counter_prefix,
                  block_number_as_bytes,
                ),
                1,
              )

        let expected_mint = {
          let minted = bytearray.concat(fortunav2.big_tuna_prefix, script_hash)
          let burned =
            bytearray.concat(fortunav2.big_tuna_prefix, current_spend_validator)

          if builtin.less_than_bytearray(minted, burned) {
            [
              Pair(bytearray.concat(fortunav2.nominated_prefix, script_hash),
              -1),
              Pair(minted, 1),
              Pair(burned, -1),
            ]
          } else {
            [
              Pair(bytearray.concat(fortunav2.nominated_prefix, script_hash),
              -1),
              Pair(burned, -1),
              Pair(minted, 1),
            ]
          }
        }

        and {
          (in_address.payment_credential == ScriptCredential(own_policy))?,
          (out_address.payment_credential == ScriptCredential(script_hash))?,
          (out_address.stake_credential == None)?,
          (value.without_lovelace(tuna_in_value) == expected_in_tuna_value)?,
          (value.without_lovelace(in_value) == expected_in_value)?,
          (value.without_lovelace(out_value) == expected_output_value)?,
          (block_height_deadline < block_number)?,
          (miner_support_count >= expected_miner_votes)?,
          ((
            mint
              |> from_minted_value
              |> tokens(own_policy)
          ) == expected_mint)?,
          (out_datum == in_tuna_datum)?,
        }
      }
    }
  }

  fn govern(
    dat: TunaUpgradeProcess,
    rdmr: TunaSpendAction,
    ctx: ScriptContext,
  ) -> Bool {
    expect ScriptContext { transaction: tx, purpose: Spend(own_ref) } = ctx
    when rdmr is {
      TokenVoteFor -> vote(True, own_ref, tx, dat)
      TokenVoteAgainst -> vote(False, own_ref, tx, dat)
      MinerVoteFor(output_index, block_number) -> {
        let Transaction { inputs, outputs, mint, redeemers, .. } = tx

        expect Mining {
          script_hash,
          miner_support_count,
          block_height_deadline,
        } = dat

        let block_number_as_bytes = integer_to_bytes(block_number, "")

        let Output { address: in_address, value: in_value, .. } =
          resolve_output_reference(inputs, own_ref)

        expect ScriptCredential(own_hash) = in_address.payment_credential

        let mint_purpose = Mint(own_hash)

        let Output {
          address: out_address,
          value: out_value,
          datum: out_datum,
          ..
        } = utils.list_at(outputs, output_index)

        let expected_datum =
          InlineDatum(
            Mining {
              script_hash,
              miner_support_count: miner_support_count + 1,
              block_height_deadline,
            },
          )

        // Expect Minter is running MineTuna action
        expect MineTuna { .. }: TunaAction =
          expect_first(redeemers, mint_purpose)

        and {
          in_address == out_address,
          block_height_deadline >= block_number,
          (
            mint
              |> from_minted_value
              |> value.to_dict
              |> dict.to_pairs
              |> quantity_of(
                  own_hash,
                  bytearray.concat(
                    fortunav2.counter_prefix,
                    block_number_as_bytes,
                  ),
                )
          ) == 1,
          expected_datum == out_datum,
          value.without_lovelace(in_value) == value.without_lovelace(out_value),
        }
      }

      TransitionState(block_number) -> {
        let Transaction {
          inputs,
          reference_inputs,
          outputs,
          validity_range,
          mint,
          redeemers,
          ..
        } = tx

        let Output { address: in_address, value: in_value, .. } =
          resolve_output_reference(inputs, own_ref)

        expect ScriptCredential(own_script_hash) = in_address.payment_credential

        let block_number_as_bytes = integer_to_bytes(block_number, "")

        let counter_token_name =
          bytearray.concat(fortunav2.counter_prefix, block_number_as_bytes)

        let common_checks =
          fn(script_hash) { and {
              // checks the input has the correct NFT
              quantity_of(
                in_value |> value.to_dict |> dict.to_pairs,
                own_script_hash,
                bytearray.concat(fortunav2.nominated_prefix, script_hash),
              ) == 1,
              // checks for correctly referencing the latest fortuna block
              // mostly to just check the block number in the redeemer is correct
              list.any(
                reference_inputs,
                fn(input) {
                  let Output { value, address: Address(payment_cred, _), .. } =
                    input.output
                  when payment_cred is {
                    ScriptCredential(hash) -> {
                      let tuna_name =
                        bytearray.concat(fortunav2.big_tuna_prefix, hash)
                      and {
                        quantity_of(
                          value |> value.to_dict |> dict.to_pairs,
                          own_script_hash,
                          counter_token_name,
                        ) == 1,
                        quantity_of(
                          value |> value.to_dict |> dict.to_pairs,
                          own_script_hash,
                          tuna_name,
                        ) == 1,
                      }
                    }
                    _ -> False
                  }
                },
              ),
            } }

        when dat is {
          Nominated { script_hash, for_count, against_count, deadline, .. } -> {
            expect Finite(lower_bound) = validity_range.lower_bound.bound_type

            let Output {
              address: out_address,
              value: out_value,
              datum: out_datum,
              ..
            } = utils.list_at(outputs, 0)

            let expected_datum =
              InlineDatum(
                Mining {
                  script_hash,
                  miner_support_count: 0,
                  block_height_deadline: block_number + miner_voting_period,
                },
              )

            let emitted_supply = calculate_emission(block_number)

            and {
              or {
                and {
                  in_address == out_address,
                  value.without_lovelace(in_value) == value.without_lovelace(
                    out_value,
                  ),
                  out_datum == expected_datum,
                  for_count >= ( for_count + against_count ) * vote_threshold / 1000,
                  for_count + against_count >= emitted_supply * supply_threshold / 1000,
                },
                and {
                  (
                    mint
                      |> from_minted_value
                      |> value.to_dict
                      |> dict.to_pairs
                      |> quantity_of(
                          own_script_hash,
                          bytearray.concat(
                            fortunav2.nominated_prefix,
                            script_hash,
                          ),
                        )
                  ) == -1,
                  or {
                    for_count < ( for_count + against_count ) * vote_threshold / 1000,
                    for_count + against_count < emitted_supply * supply_threshold / 1000,
                  },
                },
              },
              deadline < lower_bound,
              common_checks(script_hash),
            }
          }

          // Validate passed in block number is real
          Mining { script_hash, miner_support_count, block_height_deadline } ->
            or {
              and {
                miner_support_count < epoch_number * miner_threshold / 1000,
                (
                  mint
                    |> from_minted_value
                    |> value.to_dict
                    |> dict.to_pairs
                    |> quantity_of(
                        own_script_hash,
                        bytearray.concat(
                          fortunav2.nominated_prefix,
                          script_hash,
                        ),
                      )
                ) == -1,
                common_checks(script_hash),
                block_height_deadline < block_number,
              },
              // Purposely fails in this block if FinalizeNomination is not present
              {
                let upgrade_rdmr =
                  expect_first(redeemers, Mint(own_script_hash))

                expect FinalizeNomination(_, _, output_index, _): TunaAction =
                  upgrade_rdmr

                let Output { address: out_address, .. } =
                  utils.list_at(outputs, output_index)

                and {
                  quantity_of(
                    in_value |> value.to_dict |> dict.to_pairs,
                    own_script_hash,
                    bytearray.concat(fortunav2.nominated_prefix, script_hash),
                  ) == 1,
                  out_address.payment_credential == ScriptCredential(
                    script_hash,
                  ),
                }
              },
            }
        }
      }
    }
  }
}

validator(tunav2_minting_policy: ByteArray) {
  fn mine(datum: Statev2, redeemer: MineAction, ctx: ScriptContext) -> Bool {
    when redeemer is {
      MinePow(nonce, miner, merkle_proof_list) -> {
        let Statev2 {
          block_number,
          current_hash,
          leading_zeros,
          target_number,
          epoch_time,
          current_posix_time,
          merkle_root,
        } = datum

        let ScriptContext { transaction, purpose } = ctx

        expect Spend(own_reference) = purpose

        let Transaction { inputs, outputs, validity_range, .. } = transaction

        let Output { address: in_address, value: in_value, .. } =
          resolve_output_reference(inputs, own_reference)

        expect ScriptCredential(own_script_hash) = in_address.payment_credential

        // Spend(0) requirement: Contract has only one output with the master token going back to itself
        expect Some(own_output) =
          list.find(
            outputs,
            fn(output: Output) { output.address == in_address },
          )

        let Output { datum: out_datum, value: out_value, .. } = own_output

        // Time Range Span is 3 minutes or less 
        // We have a constant expectation of the transaction time range
        expect Interval {
          upper_bound: IntervalBound {
            bound_type: Finite(upper_range),
            is_inclusive: upper_is_inclusive,
          },
          lower_bound: IntervalBound {
            bound_type: Finite(lower_range),
            is_inclusive: lower_is_inclusive,
          },
        } = validity_range

        let averaged_current_time =
          ( upper_range - lower_range ) / 2 + lower_range

        let serialized_miner = builtin.serialise_data(miner)

        // Target state now includes a miner credential
        let target =
          TargetState {
            nonce,
            block_number,
            epoch_time,
            current_hash,
            leading_zeros,
            target_number,
            miner: blake2b_256(serialized_miner),
          }

        let found_bytearray =
          target
            |> builtin.serialise_data()
            |> sha2_256()
            |> sha2_256()

        let (found_target_number, found_leading_zeros) =
          fortuna.format_found_bytearray(found_bytearray)

        // Check output datum contains correct epoch time, block number, hash, and leading zeros
        // Check for every divisible by 2016 block: 
        // - Epoch time resets
        // - leading zeros is adjusted based on percent of hardcoded target time for 2016 blocks vs epoch time
        // Spend(8) requirement: Expect Output Datum to be of type Statev2
        expect Statev2 {
          epoch_time: out_epoch_time,
          block_number: out_block_number,
          current_hash: out_current_hash,
          leading_zeros: out_leading_zeros,
          target_number: out_target_number,
          current_posix_time: out_current_posix_time,
          merkle_root: out_merkle,
        }: Statev2 = get_inline_datum(out_datum)

        let block_number_as_bytes = integer_to_bytes(out_block_number, "")

        let expected_output_value =
          value.from_asset(
            tunav2_minting_policy,
            bytearray.concat(fortunav2.big_tuna_prefix, own_script_hash),
            1,
          )
            |> value.add(
                tunav2_minting_policy,
                bytearray.concat(
                  fortunav2.counter_prefix,
                  block_number_as_bytes,
                ),
                1,
              )

        and {
          // Posix time is in milliseconds
          // Spend(1) requirement: Time range span is 3 minutes or less and inclusive
          !upper_is_inclusive,
          lower_is_inclusive,
          upper_range - lower_range <= 180000,
          // In case you are wondering here is what enables pools
          // A miner can be a pkh or an nft
          // Nfts can come from any input, even validators
          // So any validator logic can be enforced to run along with fortuna
          // Spend(2) requirement: Validate miner is made the tx
          when miner is {
            Pkh(signer, _) -> list.has(transaction.extra_signatories, signer)
            Nft { policy: nft_policy, name: nft_name, output_index, .. } -> {
              let quantity =
                utils.list_at(outputs, output_index).value
                  |> value.to_dict
                  |> dict.to_pairs
                  |> quantity_of(nft_policy, nft_name)

              quantity == 1
            }
          },
          // Mining Difficulty Met
          // Spend(3) requirement: Found difficulty is less than or equal to the current difficulty
          // We do this by checking the leading zeros and the difficulty number
          or {
            found_leading_zeros > leading_zeros,
            and {
              found_leading_zeros == leading_zeros,
              found_target_number < target_number,
            },
          },
          // Spend(4) requirement: Input has master token
          quantity_of(
            in_value |> value.to_dict |> dict.to_pairs,
            tunav2_minting_policy,
            bytearray.concat(fortunav2.big_tuna_prefix, own_script_hash),
          ) == 1,
          // Spend(7) requirement: Output has same tokens as input
          expected_output_value == value.without_lovelace(out_value),
          // Spend(9) requirement: Check output has correct difficulty number, leading zeros, and epoch time
          if block_number % epoch_number == 0 && block_number > 0 {
            // use total epoch time with target epoch time to get difficulty adjustment ratio
            // ratio maxes out at 4/1 and mins to 1/4
            let total_epoch_time =
              epoch_time + averaged_current_time - current_posix_time
            let (adjustment_numerator, adjustment_denominator) =
              fortuna.get_difficulty_adjustment(total_epoch_time)
            // Now use ratio to find new leading zeros difficulty
            let (new_difficulty, new_leading_zeroes) =
              fortuna.get_new_difficulty(
                target_number,
                leading_zeros,
                adjustment_numerator,
                adjustment_denominator,
              )

            and {
              new_leading_zeroes == out_leading_zeros,
              new_difficulty == out_target_number,
              0 == out_epoch_time,
            }
          } else {
            let new_epoch_time =
              epoch_time + averaged_current_time - current_posix_time

            and {
              leading_zeros == out_leading_zeros,
              target_number == out_target_number,
              new_epoch_time == out_epoch_time,
            }
          },
          // Spend(10) requirement: Output posix time is the averaged current time
          out_current_posix_time == averaged_current_time,
          // Spend(11) requirement: Output block number is the input block number + 1 
          block_number + 1 == out_block_number,
          // Spend(12) requirement: Output current hash is the found hash
          out_current_hash == found_bytearray,
          // Spend(13) requirement: Check output merkle is correct
          insert(
            merkle_root |> from_root,
            found_bytearray |> blake2b_256,
            found_bytearray,
            merkle_proof_list,
          ) == from_root(out_merkle),
          // Spend(14) requirement: Data size doesn't exceed 1/4 tx size
          serialized_miner
            |> builtin.length_of_bytearray()
            |> builtin.less_than_equals_integer(4096),
          builtin.length_of_bytearray(nonce) <= 32,
        }
      }

      Upgrade -> {
        let ScriptContext { transaction, purpose } = ctx

        expect Spend(own_reference) = purpose

        let Transaction { redeemers, inputs, .. } = transaction

        let Output { address, value, .. } =
          resolve_output_reference(inputs, own_reference)

        let upgrade_rdmr = expect_first(redeemers, Mint(tunav2_minting_policy))

        expect FinalizeNomination { .. }: TunaAction = upgrade_rdmr

        expect ScriptCredential(spend_hash) = address.payment_credential

        let name = bytearray.concat(fortunav2.big_tuna_prefix, spend_hash)

        value.quantity_of(value, tunav2_minting_policy, name) == 1
      }
    }
  }
}
