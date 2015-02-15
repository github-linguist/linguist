function is_perfect($number)
{
    $sum = 0;
    for($i = 1; $i < $number; $i++)
    {
        if($number % $i == 0)
            $sum += $i;
    }
    return $sum == $number;
}

echo "Perfect numbers from 1 to 33550337:" . PHP_EOL;
for($num = 1; $num < 33550337; $num++)
{
    if(is_perfect($num))
        echo $num . PHP_EOL;
}
