public class Complex{
   public final double real;
   public final double imag;

   public Complex(){this(0,0)}//default values to 0...force of habit
   public Complex(double r, double i){real = r; imag = i;}

   public Complex add(Complex b){
      return new Complex(this.real + b.real, this.imag + b.imag);
   }

   public Complex mult(Complex b){
      //FOIL of (a+bi)(c+di) with i*i = -1
      return new Complex(this.real * b.real - this.imag * b.imag, this.real * b.imag + this.imag * b.real);
   }

   public Complex inv(){
      //1/(a+bi) * (a-bi)/(a-bi) = 1/(a+bi) but it's more workable
      double denom = real * real + imag * imag;
      return new Complex(real/denom,-imag/denom);
   }

   public Complex neg(){
      return new Complex(-real, -imag);
   }

   public Complex conj(){
      return new Complex(real, -imag);
   }

   public String toString(){ //override Object's toString
      return real + " + " + imag + " * i";
   }

   public static void main(String[] args){
      Complex a = new Complex(Math.PI, -5) //just some numbers
      Complex b = new Complex(-1, 2.5);
      System.out.println(a.neg());
      System.out.println(a.add(b));
      System.out.println(a.inv());
      System.out.println(a.mult(b));
      System.out.println(a.conj());
   }
}
