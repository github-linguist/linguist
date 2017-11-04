use v6;

#?pugs emit #
use MONKEY_TYPING;

use Test;

=begin description

Tests the "for" statement

This attempts to test as many variations of the
for statement as possible

=end description

plan 77;

## No foreach
# L<S04/The C<for> statement/"no foreach statement any more">
{
    my $times_run = 0;
    eval_dies_ok 'foreach 1..10 { $times_run++ }; 1', "foreach is gone";
    eval_dies_ok 'foreach (1..10) { $times_run++}; 1',
        "foreach is gone, even with parens";
    is $times_run, 0, "foreach doesn't work";
}

## for with plain old range operator w/out parens

{
    my $a = "";
    for 0 .. 5 { $a = $a ~ $_; };
    is($a, '012345', 'for 0..5 {} works');
}

# ... with pointy blocks

{
    my $b = "";
    for 0 .. 5 -> $_ { $b = $b ~ $_; };
    is($b, '012345', 'for 0 .. 5 -> {} works');
}

#?pugs todo 'slice context'
#?niecza skip 'slice context'
{
    my $str;
    my @a = 1..3;
    my @b = 4..6;
    for zip(@a; @b) -> $x, $y {
        $str ~= "($x $y)";
    }
    is $str, "(1 4)(2 5)(3 6)", 'for zip(@a; @b) -> $x, $y works';
}

# ... with referential sub
{
    my $d = "";
    for -2 .. 2 { $d ~= .sign };
    is($d, '-1-1011', 'for 0 .. 5 { .some_sub } works');
}

## and now with parens around the range operator
{
    my $e = "";
    for (0 .. 5) { $e = $e ~ $_; };
    is($e, '012345', 'for () {} works');
}

# ... with pointy blocks
{
    my $f = "";
    for (0 .. 5) -> $_ { $f = $f ~ $_; };
    is($f, '012345', 'for () -> {} works');
}

# ... with implicit topic

{
    $_ = "GLOBAL VALUE";
    for "INNER VALUE" {
        is( .lc, "inner value", "Implicit default topic is seen by lc()");
    };
    is($_,"GLOBAL VALUE","After the loop the implicit topic gets restored");
}

{
    # as statement modifier
    $_ = "GLOBAL VALUE";
    is( .lc, "inner value", "Implicit default topic is seen by lc()" )
        for "INNER VALUE";
    #?pugs todo
    is($_,"GLOBAL VALUE","After the loop the implicit topic gets restored");
}

## and now for with 'topical' variables

# ... w/out parens

my $i = "";
for 0 .. 5 -> $topic { $i = $i ~ $topic; };
is($i, '012345', 'for 0 .. 5 -> $topic {} works');

# ... with parens

my $j = "";
for (0 .. 5) -> $topic { $j = $j ~ $topic; };
is($j, '012345', 'for () -> $topic {} works');


## for with @array operator w/out parens

my @array_k = (0 .. 5);
my $k = "";
for @array_k { $k = $k ~ $_; };
is($k, '012345', 'for @array {} works');

# ... with pointy blocks

my @array_l = (0 .. 5);
my $l = "";
for @array_l -> $_ { $l = $l ~ $_; };
is($l, '012345', 'for @array -> {} works');

## and now with parens around the @array

my @array_o = (0 .. 5);
my $o = "";
for (@array_o) { $o = $o ~ $_; };
is($o, '012345', 'for (@array) {} works');

# ... with pointy blocks
{
    my @array_p = (0 .. 5);
    my $p = "";
    for (@array_p) -> $_ { $p = $p ~ $_; };
    is($p, '012345', 'for (@array) -> {} works');
}

my @elems = <a b c d e>;

{
    my @a;
    for (@elems) {
        push @a, $_;
    }
    my @e = <a b c d e>;
    is(@a, @e, 'for (@a) { ... $_ ... } iterates all elems');
}

{
    my @a;
        for (@elems) -> $_ { push @a, $_ };
    my @e = @elems;
    is(@a, @e, 'for (@a)->$_ { ... $_ ... } iterates all elems' );
}

{
    my @a;
    for (@elems) { push @a, $_, $_; }
    my @e = <a a b b c c d d e e>;
    is(@a, @e, 'for (@a) { ... $_ ... $_ ... } iterates all elems, not just odd');
}

# "for @a -> $var" is ro by default.
#?pugs skip 'parsefail'
{
    my @a = <1 2 3 4>;

    eval_dies_ok('for @a -> $elem {$elem = 5}', '-> $var is ro by default');

    for @a <-> $elem {$elem++;}
    is(@a, <2 3 4 5>, '<-> $var is rw');

    for @a <-> $first, $second {$first++; $second++}
    is(@a, <3 4 5 6>, '<-> $var, $var2 works');
}

# for with "is rw"
{
    my @array_s = (0..2);
    my @s = (1..3);
    for @array_s { $_++ };
    is(@array_s, @s, 'for @array { $_++ }');
}

{
  my @array = <a b c d>;
  for @array { $_ ~= "c" }
  is ~@array, "ac bc cc dc",
    'mutating $_ in for works';
}

{
    my @array_t = (0..2);
    my @t = (1..3);
    for @array_t -> $val is rw { $val++ };
    is(@array_t, @t, 'for @array -> $val is rw { $val++ }');
}

#?pugs skip "Can't modify const item"
{
    my @array_v = (0..2);
    my @v = (1..3);
    for @array_v.values -> $val is rw { $val++ };
    is(@array_v, @v, 'for @array.values -> $val is rw { $val++ }');
}

#?pugs skip "Can't modify const item"
{
    my @array_kv = (0..2);
    my @kv = (1..3);
    for @array_kv.kv -> $key, $val is rw { $val++ };
    is(@array_kv, @kv, 'for @array.kv -> $key, $val is rw { $val++ }');
}

#?pugs skip "Can't modify const item"
{
    my %hash_v = ( a => 1, b => 2, c => 3 );
    my %v = ( a => 2, b => 3, c => 4 );
    for %hash_v.values -> $val is rw { $val++ };
    is(%hash_v, %v, 'for %hash.values -> $val is rw { $val++ }');
}

#?pugs todo
{
    my %hash_kv = ( a => 1, b => 2, c => 3 );
    my %kv = ( a => 2, b => 3, c => 4 );
    try { for %hash_kv.kv -> $key, $val is rw { $val++ }; };
    is( %hash_kv, %kv, 'for %hash.kv -> $key, $val is rw { $val++ }');
}

# .key //= ++$i for @array1;
class TestClass{ has $.key is rw  };

{
   my @array1 = (TestClass.new(:key<1>),TestClass.new());
   my $i = 0;
   for @array1 { .key //= ++$i }
   my $sum1 = [+] @array1.map: { $_.key };
   is( $sum1, 2, '.key //= ++$i for @array1;' );

}

# .key = 1 for @array1;
{
   my @array1 = (TestClass.new(),TestClass.new(:key<2>));

   .key = 1 for @array1;
   my $sum1 = [+] @array1.map: { $_.key };
   is($sum1, 2, '.key = 1 for @array1;');
}

# $_.key = 1 for @array1;
{
   my @array1 = (TestClass.new(),TestClass.new(:key<2>));

   $_.key = 1 for @array1;
   my $sum1 = [+] @array1.map: { $_.key };
   is( $sum1, 2, '$_.key = 1 for @array1;');

}

# rw scalars
#L<S04/The C<for> statement/implicit parameter to block read/write "by default">
{
    my ($a, $b, $c) = 0..2;
    try { for ($a, $b, $c) { $_++ } };
    is( [$a,$b,$c], [1,2,3], 'for ($a,$b,$c) { $_++ }');

    ($a, $b, $c) = 0..2;
    try { for ($a, $b, $c) -> $x is rw { $x++ } };
    is( [$a,$b,$c], [1,2,3], 'for ($a,$b,$c) -> $x is rw { $x++ }');
}

# list context

{
    my $a = '';
    my $b = '';
    for 1..3, 4..6 { $a ~= $_.WHAT.gist ; $b ~= Int.gist };
    is($a, $b, 'List context');

    $a = '';
    for [1..3, 4..6] { $a ~= $_.WHAT.gist };
    is($a, Array.gist, 'List context');

    $a = '';
    $b = '';
    for [1..3], [4..6] { $a ~= $_.WHAT.gist ; $b ~= Array.gist };
    is($a, $b, 'List context');
}

{
    # this was a rakudo bug with mixed 'for' and recursion, which seems to 
    # confuse some lexical pads or the like, see RT #58392
    my $gather = '';
    sub f($l) {
        if $l <= 0 {
            return $l;
        }
        $gather ~= $l;
        for 1..3 {
        f($l-1);
            $gather ~= '.';
        }
    }
    f(2);

    is $gather, '21....1....1....', 'Can mix recursion and for';
}

# another variation
{
    my $t = '';
    my $c;
    sub r($x) {
        my $h = $c++;
        r $x-1 if $x;
        for 1 { $t ~= $h };
    };
    r 3;
    is $t, '3210', 'can mix recursion and for (RT 103332)';
}

# grep and sort in for - these were pugs bugs once, so let's
# keep them as regression tests

{
  my @array = <1 2 3 4>;
  my $output = '';

  for (grep { 1 }, @array) -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "grep works in for";
}

{
  my @array = <1 2 3 4>;
  my $output = '';

  for @array.sort -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "sort works in for";
}

{
  my @array = <1 2 3 4>;
  my $output = '';

  for (grep { 1 }, @array.sort) -> $elem {
    $output ~= "$elem,";
  }

  is $output, "1,2,3,4,", "grep and sort work in for";
}

# L<S04/Statement parsing/keywords require whitespace>
eval_dies_ok('for(0..5) { }','keyword needs at least one whitespace after it');

# looping with more than one loop variables
{
  my @a = <1 2 3 4>;
  my $str = '';
  for @a -> $x, $y { 
    $str ~= $x+$y;
  }
  is $str, "37", "for loop with two variables";
}

{
  #my $str = '';
  eval_dies_ok('for 1..5 ->  $x, $y { $str ~= "$x$y" }', 'Should throw exception, no value for parameter $y');
  #is $str, "1234", "loop ran before throwing exception";
  #diag ">$str<";
}

#?rakudo skip 'optional variable in for loop (RT #63994)'
#?niecza 2 todo 'NYI'
{
  my $str = '';
  for 1..5 -> $x, $y? {
    $str ~= " " ~ $x*$y;
  }
  is $str, " 2 12 0";
}

{
  my $str = '';
  for 1..5 -> $x, $y = 7 {
    $str ~= " " ~ $x*$y;
  }
  is $str, " 2 12 35", 'default values in for-loops';
}

#?pugs todo
{
  my @a = <1 2 3>;
  my @b = <4 5 6>;
  my $res = '';
  for @a Z @b -> $x, $y {
    $res ~= " " ~ $x * $y;
  }
  is $res, " 4 10 18", "Z -ed for loop";
}

#?pugs todo
{
  my @a = <1 2 3>;
  my $str = '';

  for @a Z @a Z @a Z @a Z @a -> $q, $w, $e, $r, $t {
    $str ~= " " ~ $q*$w*$e*$r*$t;
  }
  is $str, " 1 {2**5} {3**5}", "Z-ed for loop with 5 arrays";
}

{
  eval_dies_ok 'for 1.. { };', "Please use ..* for indefinite range";
  eval_dies_ok 'for 1... { };', "1... does not exist";
}

{
  my $c;
  for 1..8 {
    $c = $_;
    last if $_ == 6;
  }
  is $c, 6, 'for loop ends in time using last';
}

{
  my $c;
  for 1..* {
    $c = $_;
    last if $_ == 6;
  }
  is $c, 6, 'infinte for loop ends in time using last';
}

{
  my $c;
  for 1..Inf {
    $c = $_;
    last if $_ == 6;
  }
  is $c, 6, 'infinte for loop ends in time using last';
}

# RT #62478
#?pugs todo
{
    try { EVAL('for (my $ii = 1; $ii <= 3; $ii++) { say $ii; }') };
    ok "$!" ~~ /C\-style/,   'mentions C-style';
    ok "$!" ~~ /for/,        'mentions for';
    ok "$!" ~~ /loop/,       'mentions loop';
}

# RT #65212
#?pugs todo
{
    my $parsed = 0;
    try { EVAL '$parsed = 1; for (1..3)->$n { last }' };
    ok ! $parsed, 'for (1..3)->$n   fails to parse';
}

# RT #71268
{
    sub rt71268 { for ^1 {} }
    #?pugs todo
    lives_ok { ~(rt71268) }, 'can stringify "for ^1 {}" without death';
    #?pugs skip 'Cannot cast from VList to VCode'
    ok rt71268() ~~ (), 'result of "for ^1 {}" is ()';
}

# RT 62478
{
    eval_dies_ok 'for (my $i; $i <=3; $i++) { $i; }', 'Unsupported use of C-style "for (;;)" loop; in Perl 6 please use "loop (;;)"';
}

#?pugs todo
{
    try { EVAL 'for (my $x; $x <=3; $x++) { $i; }'; diag($!) };
    ok $! ~~ / 'C-style' /, 'Sensible error message';
}

# RT #64886
#?rakudo skip 'maybe bogus, for loops are not supposed to be lazy?'
{
    my $a = 0;
    for 1..10000000000 {
        $a++;
        last;
    }
    is $a, 1, 'for on Range with huge max value is lazy and enters block';
}

# RT #60780
lives_ok {
    for 1 .. 5 -> $x, $y? { }
}, 'Iteration variables do not need to add up if one is optional';

# RT #78232
{
    my $a = 0;
    for 1, 2, 3 { sub foo {}; $a++ }
    is $a, 3, 'RT #78232';
}

# http://irclog.perlgeek.de/perl6/2011-12-29#i_4892285
# (Niecza bug)
{
    my $x = 0;
    for 1 .. 2 -> $a, $b { $x = $b } #OK not used
    is $x, 2, 'Lazy lists interact properly with multi-element for loops';
}

# RT #71270
# list comprehension
#?pugs skip 'Cannot cast from VList to VCode'
{
    sub f() { for ^1 { } };
    is ~f(), '', 'empty for-loop returns empty list';
}

# RT #74060
# more list comprehension
#?pugs skip 'parsefail'
#?niecza todo "https://github.com/sorear/niecza/issues/180"
{
    my @s = ($_ * 2 if $_ ** 2 > 3 for 0 .. 5);
    is ~@s, '4 6 8 10', 'Can use statement-modifying "for" in list comprehension';
}

# RT 113026
#?rakudo todo 'RT 113026 array iterator does not track a growing array'
#?niecza todo 'array iterator does not track a growing array'
#?pugs todo
{
    my @rt113026 = 1 .. 10;
    my $iter = 0;
    for @rt113026 -> $n {
	$iter++;
	if $iter % 2 {
	    @rt113026.push: $n;
	}
    }
    is $iter, 20, 'iterating over an expanding list';
    is @rt113026, <1 2 3 4 5 6 7 8 9 10 1 3 5 7 9 1 5 9 5 5>,
       'array expanded in for loop is expanded';
}

# RT #78406
{
    my $c = 0;
    dies_ok { for ^8 { .=fmt('%03b'); $c++ } }, '$_ is read-only here';
    is $c, 0, '... and $_ is *always* read-only here';
}

dies_ok
    {
        my class Foo {
            has @.items;
            method check_items { for @.items -> $item { die "bad" if $item == 2 } }
            method foo { self.check_items; .say for @.items }
        }
        Foo.new(items => (1, 2, 3, 4)).foo
    }, 'for in called method runs (was a sink context bug)';

# RT #77460
#?pugs todo
{
    my @a = 1;
    for 1..10 {
        my $last = @a[*-1];
        push @a, (sub ($s) { $s + 1 })($last)
    };
    is @a, [1, 2, 3, 4, 5, 6, 7, 8,9, 10, 11];
}

# vim: ft=perl6
