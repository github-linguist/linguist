use v6;

use Test;

# L<S02/Whitespace and Comments>

=begin kwid

= DESCRIPTION

Tests that the List quoting parser properly
ignores whitespace in lists. This becomes important
if your line endings are \x0d\x0a.

Characters that should be ignored are:

    \t
    \r
    \n
    \x20

Most likely there are more. James tells me that
the maximum Unicode char is \x10FFFF , so maybe
we should simply (re)construct the whitespace
list via IsSpace or \s on the fly.

Of course, in the parsed result, no item should
contain whitespace.

C<\xA0> is specifically an I<nonbreaking> whitespace
character and thus should B<not> break the list.

=end kwid

#?pugs emit if $?PUGS_BACKEND ne "BACKEND_PUGS" {
#?pugs emit   skip_rest "PIL2JS and PIL-Run do not support EVAL() yet.";
#?pugs emit   exit;
#?pugs emit }

my @list = <a b c d>;
my @separators = ("\t","\r","\n"," ");
my @nonseparators = (",","/","\\",";","\xa0");

plan +@separators + @nonseparators + 3;

for @separators -> $sep {
  my $str = "<$sep" ~ @list.join("$sep$sep") ~ "$sep>";
  my @res = EVAL $str;

  my $vis = sprintf "%02x", ord $sep;
  is( @res, @list, "'\\x$vis\\x$vis' is properly parsed as list whitespace")
};

for @nonseparators -> $sep {
  my $ex = @list.join($sep);
  my $str = "<" ~$ex~ ">";
  my @res = EVAL $str;

  my $vis = sprintf "%02x", ord $sep;
  #?rakudo emit if $sep eq "\xa0" {
  #?rakudo emit      todo('\xa0 should not be a separator for list quotes');
  #?rakudo emit };
  #?niecza emit if $sep eq "\xa0" {
  #?niecza emit      todo('\xa0 should not be a separator for list quotes');
  #?niecza emit };
  is( @res, [@list.join($sep)], "'\\x$vis' does not split in a whitespace quoted list")
};

is < foo  
	    >, 'foo', 'various combinations of whitespaces are stripped';

# RT #73772
isa_ok < >, Parcel, '< > (only whitespaces) is empty Parcel';
is < >.elems, 0, ".. and it's really empty";

# vim: ft=perl6
