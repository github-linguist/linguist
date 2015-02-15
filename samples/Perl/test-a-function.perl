# ptest.t
use strict;
use warnings;

use Test;

my %tests;
BEGIN {
    # plan tests before loading Palindrome.pm
    %tests = (
        'A man, a plan, a canal: Panama.'           => 1,
        'My dog has fleas'                          => 0,
        "Madam, I'm Adam."                          => 1,
        '1 on 1'                                    => 0,
        'In girum imus nocte et consumimur igni'    => 1,
        ''                                          => 1,
    );

    # plan 4 tests per string
    plan tests => (keys(%tests) * 4);
}

use Palindrome;

for my $key (keys %tests) {
    $_ = lc $key;  # convert to lowercase
    s/[\W_]//g;    # keep only alphanumeric characters

    my $expect = $tests{$key};
    my $note = ("\"$key\" should " . ($expect ? '' : 'not ') .
                "be a palindrome.");

    ok palindrome == $expect, 1, "palindrome: $note";
    ok palindrome_c == $expect, 1, "palindrome_c: $note";
    ok palindrome_r == $expect, 1, "palindrome_r: $note";
    ok palindrome_e == $expect, 1, "palindrome_e: $note";
}
