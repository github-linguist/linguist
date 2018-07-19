import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class RAvgMeanAngle {

  private static final List<List<Double>> samples;

  static {
    samples = new ArrayList<>();
    samples.add(Arrays.asList(350.0, 10.0));
    samples.add(Arrays.asList(90.0, 180.0, 270.0, 360.0));
    samples.add(Arrays.asList(10.0, 20.0, 30.0));
    samples.add(Arrays.asList(370.0));
    samples.add(Arrays.asList(180.0));
  }

  public RAvgMeanAngle() {

    return;
  }

  public double getMeanAngle(List<Double> sample) {

    double x_component = 0.0;
    double y_component = 0.0;
    double avg_d, avg_r;

    for (double angle_d : sample) {
      double angle_r;
      angle_r = Math.toRadians(angle_d);
      x_component += Math.cos(angle_r);
      y_component += Math.sin(angle_r);
    }
    x_component /= sample.size();
    y_component /= sample.size();
    avg_r = Math.atan2(y_component, x_component);
    avg_d = Math.toDegrees(avg_r);

    return avg_d;
  }

  public static void main(String[] args) {

    runSample(args);

    return;
  }

  public static void runSample(String[] args) {

    RAvgMeanAngle main = new RAvgMeanAngle();
    for (List<Double> sample : samples) {
      double meanAngle = main.getMeanAngle(sample);
      System.out.printf("The mean angle of %s is:%n%12.6f%n%n", sample, meanAngle);
    }

    return;
  }
}
