import java.util.Random;

public class EvoAlgo {
  static final String target = "METHINKS IT IS LIKE A WEASEL";
  static final char[] possibilities = "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".toCharArray();
  static int C = 100; //number of spawn per generation
  static double minMutateRate = 0.09;
  static int perfectFitness = target.length();
  private static String parent;
  static Random rand = new Random();

  private static int fitness(String trial){
    int retVal = 0;
    for(int i = 0;i < trial.length(); i++){
      if (trial.charAt(i) == target.charAt(i)) retVal++;
    }
    return retVal;
  }

  private static double newMutateRate(){
    return (((double)perfectFitness - fitness(parent)) / perfectFitness * (1 - minMutateRate));
  }

  private static String mutate(String parent, double rate){
    String retVal = "";
    for(int i = 0;i < parent.length(); i++){
      retVal += (rand.nextDouble() <= rate) ?
        possibilities[rand.nextInt(possibilities.length)]:
        parent.charAt(i);
    }
    return retVal;
  }

  public static void main(String[] args){
    parent = mutate(target, 1);
    int iter = 0;
    while(!target.equals(parent)){
      double rate = newMutateRate();
      iter++;
      if(iter % 100 == 0){
        System.out.println(iter +": "+parent+ ", fitness: "+fitness(parent)+", rate: "+rate);
      }
      String bestSpawn = null;
      int bestFit = 0;
      for(int i = 0; i < C; i++){
        String spawn = mutate(parent, rate);
        int fitness = fitness(spawn);
        if(fitness > bestFit){
          bestSpawn = spawn;
          bestFit = fitness;
        }
      }
      parent = bestFit > fitness(parent) ? bestSpawn : parent;
    }
    System.out.println(parent+", "+iter);
  }

}
