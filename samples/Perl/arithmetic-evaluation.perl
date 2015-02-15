sub ev
# Evaluates an arithmetic expression like "(1+3)*7" and returns
# its value.
 {my $exp = shift;
  # Delete all meaningless characters. (Scientific notation,
  # infinity, and not-a-number aren't supported.)
  $exp =~ tr {0-9.+-/*()} {}cd;
  return ev_ast(astize($exp));}

 {my $balanced_paren_regex;
  $balanced_paren_regex = qr
     {\( ( [^()]+ | (??{$balanced_paren_regex}) )+ \)}x;
  # ??{ ... } interpolates lazily (only when necessary),
  # permitting recursion to arbitrary depths.

  sub astize
  # Constructs an abstract syntax tree by recursively
  # transforming textual arithmetic expressions into array
  # references of the form [operator, left oprand, right oprand].
   {my $exp = shift;
    # If $exp is just a number, return it as-is.
    $exp =~ /[^0-9.]/ or return $exp;
    # If parentheses surround the entire expression, get rid of
    # them.
    $exp = substr($exp, 1, -1)
        while $exp =~ /\A($balanced_paren_regex)\z/;
    # Replace stuff in parentheses with placeholders.
    my @paren_contents;
    $exp =~ s {($balanced_paren_regex)}
              {push(@paren_contents, $1);
               "[p$#paren_contents]"}eg;
    # Scan for operators in order of increasing precedence,
    # preferring the rightmost.
    $exp =~ m{(.+) ([+-]) (.+)}x or
        $exp =~ m{(.+) ([*/]) (.+)}x or
        # The expression must've been malformed somehow.
        # (Note that unary minus isn't supported.)
        die "Eh?: [$exp]\n";
    my ($op, $lo, $ro) = ($2, $1, $3);
    # Restore the parenthetical expressions.
    s {\[p(\d+)\]} {($paren_contents[$1])}eg
        foreach $lo, $ro;
    # And recurse.
    return [$op, astize($lo), astize($ro)];}}

 {my %ops =
     ('+' => sub {$_[0] + $_[1]},
      '-' => sub {$_[0] - $_[1]},
      '*' => sub {$_[0] * $_[1]},
      '/' => sub {$_[0] / $_[1]});

  sub ev_ast
  # Evaluates an abstract syntax tree of the form returned by
  # &astize.
   {my $ast = shift;
    # If $ast is just a number, return it as-is.
    ref $ast or return $ast;
    # Otherwise, recurse.
    my ($op, @operands) = @$ast;
    $_ = ev_ast($_) foreach @operands;
    return $ops{$op}->(@operands);}}
