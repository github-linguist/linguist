...
double sum = 0;
for(double i : nums){
  sum += i;
}
System.out.println("The mean is: " + ((nums.length != 0) ? (sum / nums.length) : 0));
...
