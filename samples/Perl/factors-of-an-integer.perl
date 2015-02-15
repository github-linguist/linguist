sub factors
{
        my($n) = @_;
        return grep { $n % $_ == 0 }(1 .. $n);
}
print join ' ',factors(64);
