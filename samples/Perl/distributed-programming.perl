use Data::Dumper;
use IO::Socket::INET;
use Safe;

sub get_data {
	my $sock = new IO::Socket::INET
		LocalHost =>	"localhost",
		LocalPort =>	"10000",
		Proto =>	"tcp",
		Listen =>	1,
		Reuse =>	1;
	unless ($sock) { die "Socket creation failure" }
	my $cli = $sock->accept();

	# of course someone may be tempted to send you 'system("rm -rf /")',
	# to be safe(r), use Safe::
	my $safe = new Safe;
	my $x = $safe->reval(join("", <$cli>));
	close $cli;
	close $sock;
	return $x;
}

sub send_data {
	my $host = shift;
	my $data = shift;
	my $sock = new IO::Socket::INET
		PeerAddr =>	"$host:10000",
		Proto =>	"tcp",
		Reuse =>	1;

	unless ($sock) { die "Socket creation failure" }

	print $sock Data::Dumper->Dump([$data]);
	close $sock;
}

if (@ARGV) {
	my $x = get_data();
	print "Got data\n", Data::Dumper->Dump([$x]);
} else {
	send_data('some_host', { a=>100, b=>[1 .. 10] });
}
