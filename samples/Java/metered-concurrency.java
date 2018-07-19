public class CountingSemaphore{
   private int lockCount = 0;
   private int maxCount;

   CountingSemaphore(int Max){
      maxCount = Max;
   }

   public synchronized void acquire() throws InterruptedException{
      while( lockCount >= maxCount){
         wait();
      }
      lockCount++;
   }
   public synchronized void release(){
      if (lockCount > 0)
      {
         lockCount--;
         notifyAll();
      }
   }
   public synchronized int getCount(){
      return lockCount;
   }
}

public class Worker extends Thread{
   private CountingSemaphore lock;
   private int id;

   Worker(CountingSemaphore coordinator, int num){
      lock = coordinator;
      id = num;
   }
   Worker(){
   }
   public void run(){
      try{
         lock.acquire();
         System.out.println("Worker " + id + " has acquired the lock.");
         sleep(2000);
      }
      catch (InterruptedException e){
      }
      finally{
         lock.release();
      }
   }
   public static void main(String[] args){
      CountingSemaphore lock = new CountingSemaphore(3);
      Worker crew[];
      crew = new Worker[5];
      for (int i = 0; i < 5; i++){
         crew[i] = new Worker(lock, i);
         crew[i].start();
      }

   }
}
