use Fcntl ':flock';

INIT
{
	die "Not able to open $0\n" unless (open ME, $0);
	die "I'm already running !!\n" unless(flock ME, LOCK_EX|LOCK_NB);
}

sleep 60; # then your code goes here
