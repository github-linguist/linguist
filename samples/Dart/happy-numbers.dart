main() {
  HashMap<int,bool> happy=new HashMap<int,bool>();
  happy[1]=true;

  int count=0;
  int i=0;

  while(count<8) {
    if(happy[i]==null) {
      int j=i;
      Set<int> sequence=new Set<int>();
      while(happy[j]==null && !sequence.contains(j)) {
        sequence.add(j);
        int sum=0;
        int val=j;
        while(val>0) {
          int digit=val%10;
          sum+=digit*digit;
          val=(val/10).toInt();
        }
        j=sum;
      }
      bool sequenceHappy=happy[j];
      Iterator<int> it=sequence.iterator();
      while(it.hasNext()) {
        happy[it.next()]=sequenceHappy;
      }
    }
    if(happy[i]) {
      print(i);
      count++;
    }
    i++;
  }
}
