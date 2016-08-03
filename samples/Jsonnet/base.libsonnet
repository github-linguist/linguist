local factorial(n) = if n == 0 then 1 else n * factorial(n-1),
      factorials(range) = [{ [std.toString(n)]: factorial(n) } for n in range];

{
    range:: error "to be overridden",
    factorial: factorials(self.range),

    hidden:: 'hidden field',
    another:: 'hidden field',

    nested: {
        deeply_nested: {
            field: 1,
        },
    },
}
