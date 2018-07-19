public class RMS {
    public static double rms(double[] nums){
        double ms = 0;
        for (int i = 0; i < nums.length; i++)
            ms += nums[i] * nums[i];
        ms /= nums.length;
        return Math.sqrt(ms);
    }

    public static	void main(String[] args){
        double[] nums = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
        System.out.println("The RMS of the numbers from 1 to 10 is " + rms(nums));
    }
}
