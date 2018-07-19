import java.util.ArrayList;

public class Josephus {
    public static int execute(int n, int k){
        int killIdx = 0;
        ArrayList<Integer> prisoners = new ArrayList<Integer>(n);
        for(int i = 0;i < n;i++){
            prisoners.add(i);
        }
        System.out.println("Prisoners executed in order:");
        while(prisoners.size() > 1){
            killIdx = (killIdx + k - 1) % prisoners.size();
            System.out.print(prisoners.get(killIdx) + " ");
            prisoners.remove(killIdx);
        }
        System.out.println();
        return prisoners.get(0);
    }

    public static ArrayList<Integer> executeAllButM(int n, int k, int m){
        int killIdx = 0;
        ArrayList<Integer> prisoners = new ArrayList<Integer>(n);
        for(int i = 0;i < n;i++){
            prisoners.add(i);
        }
        System.out.println("Prisoners executed in order:");
        while(prisoners.size() > m){
            killIdx = (killIdx + k - 1) % prisoners.size();
            System.out.print(prisoners.get(killIdx) + " ");
            prisoners.remove(killIdx);
        }
        System.out.println();
        return prisoners;
    }

    public static void main(String[] args){
        System.out.println("Survivor: " + execute(41, 3));
        System.out.println("Survivors: " + executeAllButM(41, 3, 3));
    }
}
