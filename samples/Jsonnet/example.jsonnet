local objectJoin(arr) = std.foldl(
    function(prev, item) prev + item,
    arr,
    {},
);

(import "base.libsonnet") {
    range: std.range(1, 10),

    local fibonacci(n) =
        if n == 0 || n == 1 then 1 else fibonacci(n-1) + fibonacci(n-2),
    fibonacci: objectJoin([
        {["%d" % n]: fibonacci(n) for n in std.range(0, 5)}
    ]),

    /* expose hidden field */
    another::: 'exposed hidden field',

    nested+: {
        deeply_nested+: {
            another: self.field + 1,
        },
    },

    foo: 'bar',
    while: true,
    this: |||
        is a
        multi-line string
    |||,
    refers: |||
        a field %(name)s = %(value)d
    ||| % {
        name: '$.fibonacci["5"]',
        value: $.fibonacci["5"],
    },

    // this is an inline comment
    here: 'is another', // inline comment

    /* this is a block comment
       that continues on another line */

    half: 0.5,
    delta: +10,
    negative: -10,

    finally: 'a trailing comma',
    oh: [
        "we shouldn't forget",
        'arrays can have',
        'trailing commas too',
    ],
}
