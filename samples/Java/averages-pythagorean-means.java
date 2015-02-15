import java.util.Arrays;
import java.util.List;

public class PythagoreanMeans {
    public static double arithmeticMean(List<Double> numbers) {
        if (numbers.isEmpty()) return Double.NaN;
        double mean = 0.0;
        for (Double number : numbers) {
            mean += number;
        }
        return mean / numbers.size();
    }

    public static double geometricMean(List<Double> numbers) {
        if (numbers.isEmpty()) return Double.NaN;
        double mean = 1.0;
        for (Double number : numbers) {
            mean *= number;
        }
        return Math.pow(mean, 1.0 / numbers.size());
    }

    public static double harmonicMean(List<Double> numbers) {
        if (numbers.isEmpty() || numbers.contains(0.0)) return Double.NaN;
        double mean = 0.0;
        for (Double number : numbers) {
            mean += (1.0 / number);
        }
        return numbers.size() / mean;
    }

    public static void main(String[] args) {
        Double[] array = {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0};
        List<Double> list = Arrays.asList(array);
        double arithmetic = arithmeticMean(list);
        double geometric = geometricMean(list);
        double harmonic = harmonicMean(list);
        System.out.format("A = %f  G = %f  H = %f%n", arithmetic, geometric, harmonic);
        System.out.format("A >= G is %b, G >= H is %b%n", (arithmetic >= geometric), (geometric >= harmonic));
    }
}
