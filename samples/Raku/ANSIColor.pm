use v6;

module Term::ANSIColor;

# these will be macros one day, yet macros can't be exported so far
sub RESET         is export { "\e[0m"  }
sub BOLD          is export { "\e[1m"  }
sub UNDERLINE     is export { "\e[4m"  }
sub INVERSE       is export { "\e[7m"  }
sub BOLD_OFF      is export { "\e[22m" }
sub UNDERLINE_OFF is export { "\e[24m" }
sub INVERSE_OFF   is export { "\e[27m" }

my %attrs = 
	reset      => "0",
	bold       => "1",
	underline  => "4",
	inverse    => "7",
	black      => "30",
	red        => "31",
	green      => "32",
	yellow     => "33",
	blue       => "34",
	magenta    => "35",
	cyan       => "36",
	white      => "37",
	default    => "39",
	on_black   => "40",
	on_red     => "41",
	on_green   => "42",
	on_yellow  => "43",
	on_blue    => "44",
	on_magenta => "45",
	on_cyan    => "46",
	on_white   => "47",
	on_default => "49";

sub color (Str $what) is export {
	my @res;
	my @a = $what.split(' ');
	for @a -> $attr {
		if %attrs.exists($attr) {
			@res.push: %attrs{$attr}
		} else {
			die("Invalid attribute name '$attr'")
		}
	}
	return "\e[" ~ @res.join(';') ~ "m";
}

sub colored (Str $what, Str $how) is export {
	color($how) ~ $what ~ color('reset');
}

sub colorvalid (*@a) is export {
	for @a -> $el {
		return False unless %attrs.exists($el)
	}
	return True;
}

sub colorstrip (*@a) is export {
	my @res;
	for @a -> $str {
		@res.push: $str.subst(/\e\[ <[0..9;]>+ m/, '', :g);
	}
	return @res.join;
}

sub uncolor (Str $what) is export {
	my @res;
	my @list = $what.comb(/\d+/);
	for @list -> $elem {
		if %attrs.reverse.exists($elem) {
			@res.push: %attrs.reverse{$elem}
		} else {
			die("Bad escape sequence: {'\e[' ~ $elem ~ 'm'}")
		}
	}
	return @res.join(' ');
}

=begin pod

=head1 NAME

Term::ANSIColor - Color screen output using ANSI escape sequences

=head1 SYNOPSIS

	use Term::ANSIColor;
	say color('bold'), "this is in bold", color('reset');
	say colored('underline red on_green', 'what a lovely colours!');
	say BOLD, 'good to be fat!', BOLD_OFF;
	say 'ok' if colorvalid('magenta', 'on_black', 'inverse');
	say '\e[36m is ', uncolor('\e36m');
	say colorstrip("\e[1mThis is bold\e[0m");

=head1 DESCRIPTION

Term::ANSIColor provides an interface for using colored output
in terminals. The following functions are available:

=head2 C<color()>

Given a string with color names, the output produced by C<color()>
sets the terminal output so the text printed after it will be colored
as specified. The following color names are recognised:

	reset bold underline inverse black red green yellow blue
	magenta cyan white default on_black on_red on_green on_yellow
	on_blue on_magenta on_cyan on_white on_default

The on_* family of colors correspond to the background colors.

=head2 C<colored()>

C<colored()> is similar to C<color()>. It takes two Str arguments,
where the first is the colors to be used, and the second is the string
to be colored. The C<reset> sequence is automagically placed after
the string.

=head2 C<colorvalid()>

C<colorvalid()> gets an array of color specifications (like those
passed to C<color()>) and returns true if all of them are valid,
false otherwise.

=head2 C<colorstrip()>

C<colorstrip>, given a string, removes all the escape sequences
in it, leaving the plain text without effects.

=head2 C<uncolor()>

Given escape sequences, C<uncolor()> returns a string with readable
color names. E.g. passing "\e[36;44m" will result in "cyan on_blue".

=head1 Constants

C<Term::ANSIColor> provides constants which are just strings of
appropriate escape sequences. The following constants are available:

	RESET BOLD UNDERLINE INVERSE BOLD_OFF UNDERLINE_OFF INVERSE_OFF

=end pod

# vim: ft=perl6
