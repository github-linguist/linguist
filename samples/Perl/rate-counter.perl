use Benchmark;

timethese COUNT,{ 'Job1' => &job1, 'Job2' => &job2 };

sub job1
{
	...job1 code...
}
sub job2
{
	...job2 code...
}
