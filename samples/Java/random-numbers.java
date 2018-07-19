double[] list = new double[1000];
double mean = 1.0, std = 0.5;
Random rng = new Random();
for(int i = 0;i<list.length;i++) {
  list[i] = mean + std * rng.nextGaussian();
}
