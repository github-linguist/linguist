public class Kaprekar {
    private static String[] splitAt(String str, int idx){
        String[] ans = new String[2];
        ans[0] = str.substring(0, idx);
        if(ans[0].equals("")) ans[0] = "0"; //parsing "" throws an exception
        ans[1] = str.substring(idx);
        return ans;
    }

    public static void main(String[] args){
        int count = 0;
        int base = (args.length > 0) ? Integer.parseInt(args[0]) : 10;
        for(long i = 1; i <= 1000000; i++){
            String sqrStr = Long.toString(i * i, base);
            for(int j = 0; j < sqrStr.length() / 2 + 1; j++){
                String[] parts = splitAt(sqrStr, j);
                long firstNum = Long.parseLong(parts[0], base);
                long secNum = Long.parseLong(parts[1], base);
                //if the right part is all zeroes, then it will be forever, so break
                if(secNum == 0) break;
                if(firstNum + secNum == i){
                    System.out.println(i + "\t" + Long.toString(i, base) +
                            "\t" + sqrStr + "\t" + parts[0] + " + " + parts[1]);
                    count++;
                    break;
                }
            }
        }
        System.out.println(count + " Kaprekar numbers < 1000000 (base 10) in base "+base);
    }
}
