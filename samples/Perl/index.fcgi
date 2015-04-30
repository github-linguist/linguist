#!/usr/bin/perl

use strict;
use warnings;

use CGI::Fast;
use XML::Hash::XS;
use File::Spec;
use FindBin qw($Bin);

#use lib File::Spec->catdir($Bin, qw(.. lib));

my $_stop = 0;
my $request;
$SIG{'PIPE'} = 'IGNORE';

$SIG{'INT'} = $SIG{'TERM'} = sub {
	$_stop = 1;

	exit(0) unless defined($request);
};

unless ($ENV{'SERVER_SOFTWARE'}) { # for nginx external fcgi
	$CGI::Fast::Ext_Request = FCGI::Request(
		\*STDIN, \*STDOUT, \*STDERR,
		\%ENV, int($ARGV[0] || 0), 1
	);
}

my $conv = XML::Hash::XS->new(
	use_attr => 1,
	indent   => 2,
	output   => \*STDOUT,
	xml_decl => 1
);

my $tmpl_path = File::Spec->catdir($Bin, qw|.. tmpl|);


my $data = {
	name => {
		nick => 'cono'
	},
	tree => {
		example => [
			{ name => 'Encyclopaedia', parent => 0, id => 1 },
			{ name => 'Science',       parent => 1, id => 2 },
			{ name => 'Culture',       parent => 1, id => 3 },
			{ name => 'Art',           parent => 3, id => 4 },
			{ name => 'Craft',         parent => 3, id => 5 }
		],
	},
	third_party => {
		results => [
			{ artist_name => 'Madonna', venue => 'Kitchen',      event => 'cooking',      date => '2013-04-21' },
			{ artist_name => 'cono',    venue => 'Provectus-IT', event => 'presentation', date => '2013-04-20' },
		]
	}
};

while (1) {
	eval {
		$request = CGI::Fast->new();
		unless ($request) {
			$_stop = 1;

			return;
		}

		my ($tmpl, $output) = ('nick');
		if ($ENV{'REQUEST_URI'} =~ m|/([a-zA-Z0-9_]+)|) {
			$tmpl = $1;
			$output = $data->{$tmpl} if exists $data->{$tmpl};
		}

		die "Bad request" unless $output;

		if (-e File::Spec->catfile($tmpl_path, "$tmpl.xslt")) {
			print "X-Xslt-Stylesheet: /$tmpl.xslt\r\n";
		}

		print qq(Content-type:application/xml;charset=utf-8\r\n\r\n);
		$conv->hash2xml($output);

	};

	if ($@) {
		print qq(Content-type:text/html;charset=utf-8\r\n\r\nError: $@);
	}

	$request = undef;
	last if $_stop;

	if (-M $0 < 0) {
		unless ($ENV{'SERVER_SOFTWARE'}) { # for nginx external fcgi
			system("$0 ". int($ARGV[0] || 0).' &');
		}
		last;
	}
}

exit(0);
