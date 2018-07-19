class RosettaDemo
{
    static public function main()
    {
        Sys.print(ackermann(3, 4));
    }

    static function ackermann(m : Int, n : Int)
    {
        if (m == 0)
        {
            return n + 1;
        }
        else if (n == 0)
        {
            return ackermann(m-1, 1);
        }
        return ackermann(m-1, ackermann(m, n-1));
    }
}
