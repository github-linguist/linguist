import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Horner {
    public static void main(String[] args){
        List<Double> coeffs = new ArrayList<Double>();
        coeffs.add(-19.0);
        coeffs.add(7.0);
        coeffs.add(-4.0);
        coeffs.add(6.0);
        System.out.println(polyEval(coeffs, 3));
    }

    public static double polyEval(List<Double> coefficients, double x) {
        Collections.reverse(coefficients);
        Double accumulator = coefficients.get(0);
        for (int i = 1; i < coefficients.size(); i++) {
            accumulator = (accumulator * x) + (Double) coefficients.get(i);
        }
        return accumulator;
    }
}
