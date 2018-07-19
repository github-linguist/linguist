class complex {

  num real=0;
  num imag=0;

  complex(num r,num i){
    this.real=r;
    this.imag=i;
  }


  complex add(complex b){
    return new complex(this.real + b.real, this.imag + b.imag);
  }

  complex mult(complex b){
    //FOIL of (a+bi)(c+di) with i*i = -1
    return new complex(this.real * b.real - this.imag * b.imag, this.real * b.imag + this.imag * b.real);
  }

  complex inv(){
    //1/(a+bi) * (a-bi)/(a-bi) = 1/(a+bi) but it's more workable
    num denom = real * real + imag * imag;
    double r =real/denom;
    double i= -imag/denom;
    return new complex( r,-i);
  }

  complex neg(){
    return new complex(-real, -imag);
  }

  complex conj(){
    return new complex(real, -imag);
  }


String toString(){
  return    this.real.toString()+' + '+ this.imag.toString()+'*i';
}
}
void main() {
  var cl= new complex(1,2);
  var cl2= new complex(3,-1);
  print(cl.toString());
  print(cl2.toString());
  print(cl.inv().toString());
  print(cl2.mult(cl).toString());

}
