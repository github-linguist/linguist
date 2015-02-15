public class LogicPuzzle
{
    boolean S[] = new boolean[13];
    int Count = 0;

    public boolean check2 ()
    {
        int count = 0;
        for (int k = 7; k <= 12; k++)
            if (S[k]) count++;
        return S[2] == (count == 3);
    }

    public boolean check3 ()
    {
        int count = 0;
        for (int k = 2; k <= 12; k += 2)
            if (S[k]) count++;
        return S[3] == (count == 2);
    }

    public boolean check4 ()
    {
        return S[4] == ( !S[5] || S[6] && S[7]);
    }

    public boolean check5 ()
    {
        return S[5] == ( !S[2] && !S[3] && !S[4]);
    }

    public boolean check6 ()
    {
        int count = 0;
        for (int k = 1; k <= 11; k += 2)
            if (S[k]) count++;
        return S[6] == (count == 4);
    }

    public boolean check7 ()
    {
        return S[7] == ((S[2] || S[3]) && !(S[2] && S[3]));
    }

    public boolean check8 ()
    {
        return S[8] == ( !S[7] || S[5] && S[6]);
    }

    public boolean check9 ()
    {
        int count = 0;
        for (int k = 1; k <= 6; k++)
            if (S[k]) count++;
        return S[9] == (count == 3);
    }

    public boolean check10 ()
    {
        return S[10] == (S[11] && S[12]);
    }

    public boolean check11 ()
    {
        int count = 0;
        for (int k = 7; k <= 9; k++)
            if (S[k]) count++;
        return S[11] == (count == 1);
    }

    public boolean check12 ()
    {
        int count = 0;
        for (int k = 1; k <= 11; k++)
            if (S[k]) count++;
        return S[12] == (count == 4);
    }

    public void check ()
    {
        if (check2() && check3() && check4() && check5() && check6()
            && check7() && check8() && check9() && check10() && check11()
            && check12())
        {
            for (int k = 1; k <= 12; k++)
                if (S[k]) System.out.print(k + " ");
            System.out.println();
            Count++;
        }
    }

    public void recurseAll (int k)
    {
        if (k == 13)
            check();
        else
        {
            S[k] = false;
            recurseAll(k + 1);
            S[k] = true;
            recurseAll(k + 1);
        }
    }

    public static void main (String args[])
    {
        LogicPuzzle P = new LogicPuzzle();
        P.S[1] = true;
        P.recurseAll(2);
        System.out.println();
        System.out.println(P.Count + " Solutions found.");
    }
}
