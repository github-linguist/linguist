import java.util.Scanner;
public class Sundial {
    public static void main(String[] args) {
        double lat, slat, lng, ref;
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter latitude: ");
        lat = sc.nextDouble();
        System.out.print("Enter longitude: ");
        lng = sc.nextDouble();
        System.out.print("Enter legal meridian: ");
        ref = sc.nextDouble();
        System.out.println();

        slat = Math.sin(Math.toRadians(lat));
        System.out.printf("sine of latitude: %.3f\n", slat);
        System.out.printf("diff longitude: %.3f\n\n", lng - ref);

        System.out.printf("Hour, sun hour angle, dial hour line angle from 6am to 6pm\n");

        for (int h = -6; h <= 6; h++) {
            double hla, hra;
            hra = 15.0 * h;
            hra = hra - lng + ref;
            hraRad = Math.toRadians(hra);
            hla = Math.toDegrees(Math.atan2(Math.sin(hraRad)*Math.sin(Math.toRadians(lat)), Math.cos(hraRad)));
            System.out.printf("HR= %3d;  \t  HRA=%7.3f;  \t  HLA= %7.3f\n",
                    h, hra, hla);
        }
    }
}
