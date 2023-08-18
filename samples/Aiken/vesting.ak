use aiken/interval.{Finite}
use aiken/list.{and, or}
use aiken/transaction.{Transaction, ScriptContext, Spend, ValidityRange}
 
validator {
  fn vesting(datum: Datum, _redeemer: Void, ctx: ScriptContext) {
    // In principle, scripts can be used for different purpose (e.g. minting
    // assets). Here we make sure it's only used when 'spending' from a eUTxO
    when ctx.purpose is {
      Spend(_) ->
        or([
          must_be_signed_by(ctx.transaction, datum.owner),
          and([
            must_be_signed_by(ctx.transaction, datum.beneficiary),
            must_start_after(ctx.transaction.validity_range, datum.lock_until),
          ])
        ])
      _ ->
      False
    }
  }
}
 
fn must_be_signed_by(transaction: Transaction, vk: VerificationKeyHash) {
  list.has(transaction.extra_signatories, vk)
}
 
fn must_start_after(range: ValidityRange, lower_bound: POSIXTime) {
  when range.lower_bound.bound_type is {
    Finite(now) -> now >= lower_bound
    _ -> False
  }
}
