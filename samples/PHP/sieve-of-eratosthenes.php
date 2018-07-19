function iprimes_upto($limit)
{
    for ($i = 2; $i < $limit; $i++)
    {
	$primes[$i] = true;
    }

    for ($n = 2; $n < $limit; $n++)
    {
	if ($primes[$n])
	{
	    for ($i = $n*$n; $i < $limit; $i += $n)
	    {
		$primes[$i] = false;
	    }
	}
    }

    return $primes;
}
