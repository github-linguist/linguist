public function move(n:int, from:int, to:int, via:int):void
{
    if (n > 0)
    {
        move(n - 1, from, via, to);
        trace("Move disk from pole " + from + " to pole " + to);
        move(n - 1, via, to, from);
    }
}
