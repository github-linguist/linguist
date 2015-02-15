my @my_list = (2,3,6,23,13,5,7,3,4,5);
heap_sort(\@my_list);
print "@my_list\n";
exit;

sub heap_sort
{
        my($list) = @_;
        my $count = scalar @$list;
        heapify($count,$list);

        my $end = $count - 1;
        while($end > 0)
        {
                @$list[0,$end] = @$list[$end,0];
                sift_down(0,$end-1,$list);
                $end--;
        }
}
sub heapify
{
        my ($count,$list) = @_;
        my $start = ($count - 2) / 2;
        while($start >= 0)
        {
                sift_down($start,$count-1,$list);
                $start--;
        }
}
sub sift_down
{
        my($start,$end,$list) = @_;

        my $root = $start;
        while($root * 2 + 1 <= $end)
        {
                my $child = $root * 2 + 1;
                $child++ if($child + 1 <= $end && $list->[$child] < $list->[$child+1]);
                if($list->[$root] < $list->[$child])
                {
                        @$list[$root,$child] = @$list[$child,$root];
                        $root = $child;
                }else{ return }
        }
}
