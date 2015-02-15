use Thread qw'async';
use threads::shared;

my ($lock1, $lock2, $resource1, $resource2) :shared = (0) x 4;

sub use_resource {
        {       # curly provides lexical scope, exiting which causes lock to release
                lock $lock1;
                $resource1 --;          # acquire resource
                sleep(int rand 3);      # artifical delay to pretend real work
                $resource1 ++;          # release resource
                print "In thread ", threads->tid(), ": ";
                print "Resource1 is $resource1\n";
        }
        {
                lock $lock2;
                $resource2 --;
                sleep(int rand 3);
                $resource2 ++;
                print "In thread ", threads->tid(), ": ";
                print "Resource2 is $resource2\n";
        }
}

# create 9 threads and clean up each after they are done.
for ( map async{ use_resource }, 1 .. 9) {
        $_->join
}
