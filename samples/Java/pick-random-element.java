import java.util.Random;
...
int[] array = {1,2,3};
return array[new Random().nextInt(array.length)]; // if done multiple times, the Random object should be re-used
