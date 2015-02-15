public class BoxingTheCompass{
    private static String[] points = new String[32];

    public static void main(String[] args){
        buildPoints();

        double heading = 0;

        for(int i = 0; i<= 32;i++){
            heading = i * 11.25;
            switch(i % 3){
                case 1:
                    heading += 5.62;
                    break;
                case 2:
                    heading -= 5.62;
                    break;
                default:
            }

            System.out.printf("%s\t%18s\t%s°\n",(i % 32) + 1, initialUpper(getPoint(heading)), heading);
        }
    }

    private static void buildPoints(){
        String[] cardinal = {"north", "east", "south", "west"};
        String[] pointDesc = {"1", "1 by 2", "1-C", "C by 1", "C", "C by 2", "2-C", "2 by 1"};

        String str1, str2, strC;

        for(int i = 0;i <= 3;i++){
            str1 = cardinal[i];
            str2 = cardinal[(i + 1) % 4];
            strC = (str1.equals("north") || str1.equals("south")) ? (str1 + str2): (str2 + str1);
            for(int j = 0;j <= 7;j++){
                points[i * 8 + j] = pointDesc[j].replace("1", str1).replace("2", str2).replace("C", strC);
            }
        }
    }

    private static String initialUpper(String s){
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    private static String getPoint(double degrees){
        double testD = (degrees / 11.25) + 0.5;
        return points[(int)Math.floor(testD % 32)];
    }
}
