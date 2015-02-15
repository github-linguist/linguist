public class Range{
	public static void main(String[] args){
		System.out.println(compress2Range("-6, -3, -2, -1, 0, 1, 3, 4, 5, 7," +
				" 8, 9, 10, 11, 14, 15, 17, 18, 19, 20"));
        System.out.println(compress2Range(
                "0,  1,  2,  4,  6,  7,  8, 11, 12, 14, " +
                "15, 16, 17, 18, 19, 20, 21, 22, 23, 24," +
                "25, 27, 28, 29, 30, 31, 32, 33, 35, 36," +
                "37, 38, 39"));
    }

    private static String compress2Range(String expanded){
        StringBuilder result = new StringBuilder();
        String[] nums = expanded.replace(" ", "").split(",");
        int firstNum = Integer.parseInt(nums[0]);
        int rangeSize = 0;
        for(int i = 1; i < nums.length; i++){
            int thisNum = Integer.parseInt(nums[i]);
            if(thisNum - firstNum - rangeSize == 1){
                rangeSize++;
            }else{
                if(rangeSize != 0){
                    result.append(firstNum).append((rangeSize == 1) ? ",": "-")
                            .append(firstNum+rangeSize).append(",");
                    rangeSize = 0;
                }else{
                    result.append(firstNum).append(",");
                }
                firstNum = thisNum;
            }
        }
        if(rangeSize != 0){
            result.append(firstNum).append((rangeSize == 1) ? "," : "-").
                    append(firstNum + rangeSize);
            rangeSize = 0;
        } else {
            result.append(firstNum);
        }
        return result.toString();
    }
}
