public class MulTable{
    public static void main(String args[]){
        int i,j;
        for(i=1;i<=12;i++)
        {
            System.out.print("\t"+i);
        }

        System.out.println("");
        for(i=0;i<100;i++)
            System.out.print("-");
        System.out.println("");
        for(i=1;i<=12;i++){
            System.out.print(""+i+"|");
            for(j=1;j<=12;j++){
                if(j<i)
                    System.out.print("\t");
                else
                    System.out.print("\t"+i*j);
            }
            System.out.println("");
        }
    }
}
