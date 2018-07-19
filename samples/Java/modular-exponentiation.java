import java.math.BigInteger;

public class PowMod {
    public static void main(String[] args){
        BigInteger a = new BigInteger(
      "2988348162058574136915891421498819466320163312926952423791023078876139");
        BigInteger b = new BigInteger(
      "2351399303373464486466122544523690094744975233415544072992656881240319");
        BigInteger m = new BigInteger("10000000000000000000000000000000000000000");

        System.out.println(a.modPow(b, m));
    }
}
