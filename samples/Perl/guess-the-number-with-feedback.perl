sub prompt {
	my $prompt = shift;
	while (1) {
		print "\n", $prompt, ": ";
		# type ^D, q, quit, quagmire, etc to quit
		defined($_ = <STDIN>) and !/^\s*q/ or exit;

		return $_ if /^\s*\d+\s*$/s;
		$prompt = "Please give a non-negative integer";
	}
}

my $tgt = int(rand prompt("Hola! Please tell me the upper bound") + 1);
my $tries = 1;

$tries++, print "You guessed too ", ($_ == -1 ? "high" : "low"), ".\n"
	while ($_ = $tgt <=> prompt "Your guess");

print "Correct! You guessed it after $tries tries.\n";
