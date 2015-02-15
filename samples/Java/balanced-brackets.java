public class Brackets {
    public static boolean checkBrackets(String str){
        int mismatchedBrackets = 0;
        for(char ch:str.toCharArray()){
            if(ch == '['){
                mismatchedBrackets++;
            }else if(ch == ']'){
                mismatchedBrackets--;
            }else{
                return false; //non-bracket chars
            }
            if(mismatchedBrackets < 0){ //close bracket before open bracket
                return false;
            }
        }
        return mismatchedBrackets == 0;
    }

    public static String generate(int n){
        if(n % 2 == 1){ //if n is odd we can't match brackets
            return null;
        }
        String ans = "";
        int openBracketsLeft = n / 2;
        int unclosed = 0;
        while(ans.length() < n){
            if(Math.random() >= .5 && openBracketsLeft > 0 || unclosed == 0){
                ans += '[';
                openBracketsLeft--;
                unclosed++;
            }else{
                ans += ']';
                unclosed--;
            }
        }
        return ans;
    }

    public static void main(String[] args){
        String[] tests = {"", "[]", "][", "[][]", "][][", "[[][]]", "[]][[]"};
        for(int i = 0; i <= 16; i+=2){
            String bracks = generate(i);
            System.out.println(bracks + ": " + checkBrackets(bracks));
        }

        for(String test: tests){
            System.out.println(test + ": " + checkBrackets(test));
        }
    }
}
