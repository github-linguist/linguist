public class Trig {
        public static void main(String[] args) {
                //Pi / 4 is 45 degrees. All answers should be the same.
                double radians = Math.PI / 4;
                double degrees = 45.0;
                //sine
                System.out.println(Math.sin(radians) + " " + Math.sin(Math.toRadians(degrees)));
                //cosine
                System.out.println(Math.cos(radians) + " " + Math.cos(Math.toRadians(degrees)));
                //tangent
                System.out.println(Math.tan(radians) + " " + Math.tan(Math.toRadians(degrees)));
                //arcsine
                double arcsin = Math.asin(Math.sin(radians));
                System.out.println(arcsin + " " + Math.toDegrees(arcsin));
                //arccosine
                double arccos = Math.acos(Math.cos(radians));
                System.out.println(arccos + " " + Math.toDegrees(arccos));
                //arctangent
                double arctan = Math.atan(Math.tan(radians));
                System.out.println(arctan + " " + Math.toDegrees(arctan));
        }
}
