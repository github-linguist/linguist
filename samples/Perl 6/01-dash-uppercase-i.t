use v6;

use Test;

=begin pod

Test handling of -I.

Multiple C<-I> switches are supposed to
prepend left-to-right:

  -Ifoo -Ibar

should make C<@*INC> look like:

  foo
  bar
  ...

Duplication of directories on the command line is mirrored
in the C<@*INC> variable, so C<pugs -Ilib -Ilib> will have B<two>
entries C<lib/> in C<@*INC>.

=end pod

# L<S19/Reference/"Prepend directories to">

my $fragment = '-e "@*INC.perl.say"';

my @tests = (
    'foo',
    'foo$bar',
    'foo bar$baz',
    'foo$foo',
);

plan @tests*2;

diag "Running under $*OS";

my ($pugs,$redir) = ($*EXECUTABLE_NAME, ">");

if $*OS eq any <MSWin32 mingw msys cygwin> {
  $pugs = 'pugs.exe';
  $redir = '>';
};

sub nonce () { return (".{$*PID}." ~ (1..1000).pick) }

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output" ~ nonce;
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  run $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

for @tests -> $t {
  my @dirs = split('$',$t);
  my $command;
  # This should be smarter about quoting
  # (currently, this should work for WinNT and Unix shells)
  $command = join " ", map { qq["-I$_"] }, @dirs;
  my $got = run_pugs( $command ~ " $fragment" );
  $got .= chomp;

  if (substr($got,0,1) ~~ "[") {
    # Convert from arrayref to array
    $got = substr($got, 1, -1);
  };

  my @got = EVAL $got;
  @got = @got[ 0..@dirs-1 ];
  my @expected = @dirs;

  is @got, @expected, "'" ~ @dirs ~ "' works";

  $command = join " ", map { qq[-I "$_"] }, @dirs;
  $got = run_pugs( $command ~ " $fragment" );
  
  $got .= chomp;
  if (substr($got,0,1) ~~ "[") {
    # Convert from arrayref to array
    $got = substr($got, 1, -1);
  };
  
  @got = EVAL $got;
  @got = @got[ 0..@dirs-1 ];
  @expected = @dirs;

  is @got, @expected, "'" ~ @dirs ~ "' works (with a space delimiting -I)";
}


# vim: ft=perl6
