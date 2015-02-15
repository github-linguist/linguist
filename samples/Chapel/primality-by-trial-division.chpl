proc is_prime(n)
{
    if n == 2 then
        return true;
    if n <= 1 || n % 2 == 0 then
        return false;
    for i in 3..floor(sqrt(n)):int by 2 do
        if n % i == 0 then
            return false;
    return true;
}
