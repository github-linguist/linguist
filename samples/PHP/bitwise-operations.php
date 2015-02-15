function bitwise($a, $b)
{
    echo '$a AND $b: ', $a & $b, "\n";
    echo '$a OR $b: ', $a | $b, "\n";
    echo '$a XOR $b: ', $a ^ $b, "\n";
    echo 'NOT $a: ', ~$a, "\n";
    echo '$a << $b: ', $a << $b, "\n"; // left shift
    echo '$a >> $b: ', $a >> $b, "\n"; // arithmetic right shift
}
