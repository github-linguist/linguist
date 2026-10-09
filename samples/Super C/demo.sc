|| Super C syntax demo

|| Variables and OUT ($...$;)
$$x -> 10;
$$label -> "Super C";
$$singleQuote -> 'single quoted string';
$"Welcome to", label$;
$singleQuote$;
$"x =", x$;

|| Math: +, -, *, /, %, **, and parentheses
$$sum -> x + 5;
$$difference -> x - 3;
$$product -> x * 2;
$$quotient -> x / 2;
$$remainder -> x % 3;
$$power -> 2 ** 3;
$$grouped -> (x + 2) * 3;
$"math:", sum, difference, product, quotient, remainder, power, grouped$;

|| Arrays: size and initial values; indexes start at 0
$[2] name = {"DATA1", "DATA2"};
$name[0];
name[1];

|| Array alias: use alias[index] after this declaration
$$alias: $name;
alias[1];

|| Numeric array values can appear in OUT expressions
$[2] numbers = {1, 2};
$"array math =", $numbers[0] + 5$;

|| Functions use $$parameters and must end with };
$f addOne($$n) {
    $$r n + 1;
};
$$result -> addOne(5);
$"function result =", result$;

|| For loop: initialization; condition; update
$$f ($$i = 0; i < 3; i++) {
    $"for =", i$;
};

|| Continue skips to the next iteration; break exits the current loop
$$f ($$j = 0; j < 5 && !false; j++) {
    $$i (j == 1) {
        $$c;
    }
    $$i (j == 3) {
        $$b;
    }
    $"loop control =", j$;
};

|| While loop
$$count -> 0;
$$w (count < 2) {
    $"while =", count$;
    count++;
}

|| If, else-if, and else
$$i (x > 10) {
    $"x > 10"$;
} $$ei (x == 10 || count == 0) {
    $"x == 10"$;
} $$e {
    $"x < 10"$;
}
