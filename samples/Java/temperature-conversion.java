public class TemperatureConversion {
    public static void main(String args[]) {
        if (args.length == 1) {
            try {
                double kelvin = Double.parseDouble(args[0]);
                if (kelvin >= 0) {
                    System.out.printf("K  %2.2f\n", kelvin);
                    System.out.printf("C  %2.2f\n", kelvinToCelsius(kelvin));
                    System.out.printf("F  %2.2f\n", kelvinToFahrenheit(kelvin));
                    System.out.printf("R  %2.2f\n", kelvinToRankine(kelvin));
                } else {
                    System.out.printf("%2.2f K is below absolute zero", kelvin);
                }
            } catch (NumberFormatException e) {
                System.out.println(e);
            }
        }
    }

    public static double kelvinToCelsius(double k) {
        return k + 273.15;
    }

    public static double kelvinToFahrenheit(double k) {
        return k * 1.8 - 459.67;
    }

    public static double kelvinToRankine(double k) {
        return k * 1.8;
    }
}
