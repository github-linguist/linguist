function print_logic($a, $b)
{
    echo "a and b is ", $a && $b ? 'True' : 'False', "\n";
    echo "a or b is ", $a || $b ? 'True' : 'False', "\n";
    echo "not a is ", ! $a ? 'True' : 'False', "\n";
}
