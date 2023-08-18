validator {
  fn hello_world(datum, redeemer, context) -> Bool {
    let must_say_hello =
      redeemer == "Hello, World!"

    let must_be_signed =
      list.has(
        context.transaction.extra_signatories,
        datum.owner,
      )

    must_say_hello? && must_be_signed?
  }
}
