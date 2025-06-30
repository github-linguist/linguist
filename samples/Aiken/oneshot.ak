use aiken/dict
use aiken/list
use aiken/transaction.{OutputReference, ScriptContext, Transaction} as tx
use aiken/transaction/value

type Action {
  Mint
  Burn
}

validator(token_name: ByteArray, utxo_ref: OutputReference) {
  fn gift_card(rdmr: Action, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx

    expect tx.Mint(policy_id) = purpose

    let Transaction { inputs, mint, .. } = transaction

    expect [Pair(asset_name, amount)] =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_pairs()

    when rdmr is {
      Mint -> {
        expect Some(_input) =
          list.find(inputs, fn(input) { input.output_reference == utxo_ref })
        amount == 1 && asset_name == token_name
      }
      Burn -> amount == -1 && asset_name == token_name
    }
  }
}

validator(token_name: ByteArray, policy_id: ByteArray) {
  fn redeem(_d: Data, _r: Data, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, .. } = ctx

    let Transaction { mint, .. } = transaction

    expect [Pair(asset_name, amount)] =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_pairs()

    amount == -1 && asset_name == token_name
  }
}
