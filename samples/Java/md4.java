import org.bouncycastle.crypto.digests.MD4Digest;
import org.bouncycastle.util.encoders.Hex;

public class RosettaMD4
{
    public static void main (String[] argv) throws Exception
    {
        byte[] r = "Rosetta Code".getBytes("US-ASCII");
        MD4Digest d = new MD4Digest();
        d.update (r, 0, r.length);
        byte[] o = new byte[d.getDigestSize()];
        d.doFinal (o, 0);
        Hex.encode (o, System.out);
        System.out.println();
    }
}
