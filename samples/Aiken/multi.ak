use aiken/builtin
use aiken/bytearray
use aiken/dict
use aiken/hash.{blake2b_256}
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, ScriptContext, Spend, Transaction,
} as tx
use aiken/transaction/credential.{Address, PaymentCredential, ScriptCredential}
use aiken/transaction/value

type Action {
  Mint(Int)
  Burn
}

type SpendTokenName =
  ByteArray

validator(creator: ByteArray) {
  fn redeem(
    // Each spend input checks for a token name matching the datum being burned
    datum: SpendTokenName,
    _r: Data,
    ctx: ScriptContext,
  ) {
    let ScriptContext { transaction, purpose } = ctx

    let Transaction { inputs, mint, .. } = transaction

    expect Spend(own_ref) = purpose

    expect Some(own_input) =
      list.find(inputs, fn(input) { input.output_reference == own_ref })

    let Input {
      output: Output { address: Address { payment_credential, .. }, .. },
      ..
    } = own_input

    expect ScriptCredential(own_validator_hash) = payment_credential

    (
      mint
        |> value.from_minted_value
        |> value.quantity_of(own_validator_hash, datum)
    ) == -1
  }

  fn gift_card(rdmr: Action, ctx: ScriptContext) -> Bool {
    // get values from transaction and purpose
    let ScriptContext { transaction, purpose } = ctx

    expect tx.Mint(policy_id) = purpose

    let Transaction { inputs, mint, extra_signatories, outputs, .. } =
      transaction

    let minted_assets =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_pairs()

    when rdmr is {
      Mint(total) -> {
        expect [input, ..] = inputs
        // Base is created from serializing a utxo ref being spent. Thus this guarantees a unique base
        let base = builtin.serialise_data(input.output_reference)
        // Create a list of expected token names
        let expected_minted_token_names =
          create_expected_minted_nfts(base, total, [])
        // Check contract creator is a signer of this tx
        let signature_check =
          list.any(extra_signatories, fn(n) { creator == n })
        // Support multiple gift card creation by allowing a
        // 'number of tokens minted' == 'outputs with datum being token name'
        signature_check && check_mint_and_outputs(
          minted_assets,
          outputs,
          expected_minted_token_names,
          ScriptCredential(policy_id),
        )
      }
      Burn ->
        list.all(
          minted_assets,
          fn(asset) {
            let Pair(_, amount) = asset
            amount == -1
          },
        )
    }
  }
}

fn insert(self: List<a>, e: a, compare: fn(a, a) -> Ordering) -> List<a> {
  when self is {
    [] ->
      [e]
    [x, ..xs] ->
      if compare(e, x) == Less {
        [e, ..self]
      } else {
        [x, ..insert(xs, e, compare)]
      }
  }
}

// Check each minted token name is in the expected list, has quantity of 1,
// and has a corresponding ouput with datum containing token name.
// Otherwise fail
fn check_mint_and_outputs(
  minted_assets: Pairs<ByteArray, Int>,
  outputs: List<Output>,
  expected_assets: List<ByteArray>,
  validator_cred: PaymentCredential,
) -> Bool {
  when minted_assets is {
    [] -> True
    [Pair(minted_asset_name, quantity), ..rest_assets] -> {
      expect
        list.any(
          expected_assets,
          fn(expected_asset) { expected_asset == minted_asset_name },
        )

      expect
        list.any(
          outputs,
          fn(output) {
            let Output { address, datum, .. } = output
            datum == InlineDatum(minted_asset_name) && address.payment_credential == validator_cred
          },
        )

      quantity == 1 && check_mint_and_outputs(
        rest_assets,
        outputs,
        expected_assets,
        validator_cred,
      )
    }
  }
}

fn create_expected_minted_nfts(
  base: ByteArray,
  counter: Int,
  accum: List<ByteArray>,
) -> List<ByteArray> {
  if counter == 0 {
    accum
  } else {
    let token_name = blake2b_256(bytearray.push(base, counter))

    let accum =
      [token_name, ..accum]

    create_expected_minted_nfts(base, counter - 1, accum)
  }
}
