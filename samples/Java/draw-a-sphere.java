public class Sphere{
    static char[] shades = {'.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@'};

    static double[] light = { 30, 30, -50 };
    private static void normalize(double[] v){
        double len = Math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
        v[0] /= len; v[1] /= len; v[2] /= len;
    }

    private static double dot(double[] x, double[] y){
        double d = x[0]*y[0] + x[1]*y[1] + x[2]*y[2];
        return d < 0 ? -d : 0;
    }

    public static void drawSphere(double R, double k, double ambient){
        double[] vec = new double[3];
        for(int i = (int)Math.floor(-R); i <= (int)Math.ceil(R); i++){
            double x = i + .5;
            for(int j = (int)Math.floor(-2 * R); j <= (int)Math.ceil(2 * R); j++){
                double y = j / 2. + .5;
                if(x * x + y * y <= R * R) {
                    vec[0] = x;
                    vec[1] = y;
                    vec[2] = Math.sqrt(R * R - x * x - y * y);
                    normalize(vec);
                    double b = Math.pow(dot(light, vec), k) + ambient;
                    int intensity = (b <= 0) ?
                                shades.length - 2 :
                                (int)Math.max((1 - b) * (shades.length - 1), 0);
                    System.out.print(shades[intensity]);
                } else
                    System.out.print(' ');
            }
            System.out.println();
        }
    }

    public static void main(String[] args){
        normalize(light);
        drawSphere(20, 4, .1);
        drawSphere(10, 2, .4);
    }
}
