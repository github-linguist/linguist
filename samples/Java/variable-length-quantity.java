public class VLQCode
{
  public static byte[] encode(long n)
  {
    int numRelevantBits = 64 - Long.numberOfLeadingZeros(n);
    int numBytes = (numRelevantBits + 6) / 7;
    if (numBytes == 0)
      numBytes = 1;
    byte[] output = new byte[numBytes];
    for (int i = numBytes - 1; i >= 0; i--)
    {
      int curByte = (int)(n & 0x7F);
      if (i != (numBytes - 1))
        curByte |= 0x80;
      output[i] = (byte)curByte;
      n >>>= 7;
    }
    return output;
  }

  public static long decode(byte[] b)
  {
    long n = 0;
    for (int i = 0; i < b.length; i++)
    {
      int curByte = b[i] & 0xFF;
      n = (n << 7) | (curByte & 0x7F);
      if ((curByte & 0x80) == 0)
        break;
    }
    return n;
  }

  public static String byteArrayToString(byte[] b)
  {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < b.length; i++)
    {
      if (i > 0)
        sb.append(", ");
      String s = Integer.toHexString(b[i] & 0xFF);
      if (s.length() < 2)
        s = "0" + s;
      sb.append(s);
    }
    return sb.toString();
  }

  public static void main(String[] args)
  {
    long[] testNumbers = { 2097152, 2097151, 1, 127, 128, 589723405834L };
    for (long n : testNumbers)
    {
      byte[] encoded = encode(n);
      long decoded = decode(encoded);
      System.out.println("Original input=" + n + ", encoded = [" + byteArrayToString(encoded) + "], decoded=" + decoded + ", " + ((n == decoded) ? "OK" : "FAIL"));
    }
  }
}
