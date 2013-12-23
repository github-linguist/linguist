#!/usr/bin/env perl6

use v6;

my $string = 'I look like a # comment!';

if $string eq 'foo' {
    say 'hello';
}

regex http-verb {
      'GET'
    | 'POST'
    | 'PUT'
    | 'DELETE'
    | 'TRACE'
    | 'OPTIONS'
    | 'HEAD'
}

# a sample comment

say 'Hello from Perl 6!'


#`{
multi-line comment!
}

say 'here';

#`(
multi-line comment!
)

say 'here';

#`{{{
I'm a special comment!
}}}

say 'there';

#`{{
I'm { even } specialer!
}}

say 'there';

#`{{
does {{nesting}} work?
}}

#`«<
trying mixed delimiters
»

my $string = qq<Hooray, arbitrary delimiter!>;
my $string = qq«Hooray, arbitrary delimiter!»;
my $string = q <now with whitespace!>;
my $string = qq<<more strings>>;

my %hash := Hash.new;

=begin pod

Here's some POD!  Wooo

=end pod

=for Testing
    This is POD (see? role isn't highlighted)

say('this is not!');

=table
    Of role things

say('not in your table');
#= A single line declarator "block" (with a keyword like role)
#| Another single line declarator "block" (with a keyword like role)
#={
    A declarator block (with a keyword like role)
  }
#|{
    Another declarator block (with a keyword like role)
  }
#= { A single line declarator "block" with a brace (with a keyword like role)
#=«
    More declarator blocks! (with a keyword like role)
  »
#|«
    More declarator blocks! (with a keyword like role)
  »

say 'Moar code!';

my $don't = 16;

sub don't($x) {
    !$x
}

say don't 'foo';

my %hash = (
    :foo(1),
);

say %hash<foo>;
say %hash<<foo>>;
say %hash«foo»;

say %*hash<foo>;
say %*hash<<foo>>;
say %*hash«foo»;

say $<todo>;
say $<todo>;

for (@A Z @B) -> $a, $b {
    say $a + $b;
}

Q:PIR {
    .loadlib "somelib"
}

my $longstring = q/
    lots
    of
    text
/;

my $heredoc = q:to/END_SQL/;
SELECT * FROM Users
WHERE first_name = 'Rob'
END_SQL
my $hello;

# Fun with regexen

if 'food' ~~ /foo/ {
    say 'match!'
}

my $re  = /foo/;
my $re2 = m/ foo /;
my $re3 = m:i/ FOO /;

call-a-sub(/ foo /);
call-a-sub(/ foo \/ bar /);

my $re4    = rx/something | something-else/;
my $result = ms/regexy stuff/;
my $sub0   = s/regexy stuff/more stuff/;
my $sub    = ss/regexy stuff/more stuff/;
my $trans  = tr/regexy stuff/more stuff/;

my @values = <a b c d>;
call-sub(<a b c d>);
call-sub <a b c d>;

my $result = $a < $b;

for <a b c d> -> $letter {
    say $letter;
}

sub test-sub {
    say @_;
    say $!;
    say $/;
    say $0;
    say $1;
    say @*ARGS;
    say $*ARGFILES;
    say &?BLOCK;
    say ::?CLASS;
    say $?CLASS;
    say @=COMMENT;
    say %?CONFIG;
    say $*CWD;
    say $=data;
    say %?DEEPMAGIC;
    say $?DISTRO;
    say $*DISTRO;
    say $*EGID;
    say %*ENV;
    say $*ERR;
    say $*EUID;
    say $*EXECUTABLE_NAME;
    say $?FILE;
    say $?GRAMMAR;
    say $*GID;
    say $*IN;
    say @*INC;
    say %?LANG;
    say $*LANG;
    say $?LINE;
    say %*META-ARGS;
    say $?MODULE;
    say %*OPTS;
    say %*OPT;
    say $?KERNEL;
    say $*KERNEL;
    say $*OUT;
    say $?PACKAGE;
    say $?PERL;
    say $*PERL;
    say $*PID;
    say %=pod;
    say $*PROGRAM_NAME;
    say %*PROTOCOLS;
    say ::?ROLE;
    say $?ROLE;
    say &?ROUTINE;
    say $?SCOPE;
    say $*TZ;
    say $*UID;
    say $?USAGE;
    say $?VM;
    say $?XVM;
}

say <a b c>;

my $perl5_re = m:P5/ fo{2} /;
my $re5      = rx«something | something-else»;

my $M := %*COMPILING<%?OPTIONS><M>;

say $M;

sub regex-name { ... }
my $pair = role-name => 'foo';
$pair = rolesque => 'foo';

my sub something(Str:D $value) { ... }

my $s = q«<
some
string
stuff
»;

my $regex = m«< some chars »;
# after

say $/<foo><bar>;

roleq;
