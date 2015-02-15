num mean(List<num> l) => l.reduce((num p, num n) => p + n) / l.length;

void main(){
  print(mean([1,2,3,4,5,6,7]));
}
