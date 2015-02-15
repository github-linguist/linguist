import java.math.BigInteger;

class DigitalRoot
{
  public static int[] calcDigitalRoot(String number, int base)
  {
    BigInteger bi = new BigInteger(number, base);
    int additivePersistence = 0;
    if (bi.signum() < 0)
      bi = bi.negate();
    BigInteger biBase = BigInteger.valueOf(base);
    while (bi.compareTo(biBase) >= 0)
    {
      number = bi.toString(base);
      bi = BigInteger.ZERO;
      for (int i = 0; i < number.length(); i++)
        bi = bi.add(new BigInteger(number.substring(i, i + 1), base));
      additivePersistence++;
    }
    return new int[] { additivePersistence, bi.intValue() };
  }

  public static void main(String[] args)
  {
    for (String arg : args)
    {
      int[] results = calcDigitalRoot(arg, 10);
      System.out.println(arg + " has additive persistence " + results[0] + " and digital root of " + results[1]);
    }
  }
}
