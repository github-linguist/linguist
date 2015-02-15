use threads;
use Thread::Queue qw();

my $q1 = Thread::Queue->new;
my $q2 = Thread::Queue->new;

my $reader = threads->create(sub {
     my $q1 = shift;
     my $q2 = shift;

     open my $fh, '<', 'input.txt';
     $q1->enqueue($_) while <$fh>;
     close $fh;
     $q1->enqueue(undef);

     print $q2->dequeue;
}, $q1, $q2);

my $printer = threads->create(sub {
     my $q1 = shift;
     my $q2 = shift;

     my $count;
     while (my $line = $q1->dequeue) {
         print $line;
         $count++;
     };

     $q2->enqueue($count);
}, $q1, $q2);

$reader->join;
$printer->join;
