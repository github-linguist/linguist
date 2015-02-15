import java.util.LinkedList;
public class StdDev {
    LinkedList<Double> nums;
    public StdDev() {
        nums = new LinkedList<Double>();
    }
    public static void main(String[] args) {
        double[] testData = {2,4,4,4,5,5,7,9};
        StdDev sd = new StdDev();

        for (double x : testData) {
            sd.newNum(x);
            System.out.println(sd.getSD());
        }
    }

    public void newNum(double num) {
        nums.add(num);
    }

    public double getAvg() {
        if (nums.isEmpty()) return 0;
        double ret = 0;
        double sum = 0;
        for (double num : nums) {
           sum += num;
        }
        return sum / nums.size();
    }

    public double getSD() {
        double sqDiffs = 0;
        double avg = getAvg();
        for (double num : nums) {
           sqDiffs += (num - avg) * (num - avg);
        }
        return Math.sqrt(sqDiffs / nums.size());
    }
}
