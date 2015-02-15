use strict;
use warnings;

use Safe;

sub hail_next {
    my $n = shift;
    return 1 if $n == 1;
    return $n * 3 + 1 if $n % 2;
    $n / 2;
};

my @enviornments;
for my $initial ( 1..12 ) {
   my $env = Safe->new;
   ${ $env->varglob('value') } = $initial;
   ${ $env->varglob('count') } = 0;
   $env->share('&hail_next');
   $env->reval(q{
      sub task {
         return if $value == 1;
         $value = hail_next( $value );
         ++$count;
      }
   });
   push @enviornments, $env;
}

my @value_refs = map $_->varglob('value'), @enviornments;
my @tasks = map $_->varglob('task'), @enviornments;
while( grep { $$_ != 1 } @value_refs ) {
    printf "%4s", $$_ for @value_refs;
    print "\n";
    $_->() for @tasks;
}

print "Counts\n";

printf "%4s", ${$_->varglob('count')} for @enviornments;
print "\n";
