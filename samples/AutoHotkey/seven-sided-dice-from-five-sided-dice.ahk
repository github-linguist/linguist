dice5()
{  Random, v, 1, 5
   Return, v
}

dice7()
{  Loop
   {  v := 5 * dice5() + dice5() - 6
      IfLess v, 21, Return, (v // 3) + 1
   }
}
