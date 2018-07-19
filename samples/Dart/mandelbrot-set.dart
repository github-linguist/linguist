class Complex {
  double _r,_i;

  Complex(this._r,this._i);
  double get r() => _r;
  double get i() => _i;
  String toString() => "($r,$i)";

  Complex operator +(Complex other) => new Complex(r+other.r,i+other.i);
  Complex operator *(Complex other) =>
      new Complex(r*other.r-i*other.i,r*other.i+other.r*i);
  double abs() => r*r+i*i;
}

void main() {
  double start_x=-1.5;
  double start_y=-1.0;
  double step_x=0.03;
  double step_y=0.1;

  for(int y=0;y<20;y++) {
    String line="";
    for(int x=0;x<70;x++) {
      Complex c=new Complex(start_x+step_x*x,start_y+step_y*y);
      Complex z=new Complex(0,0);
      for(int i=0;i<100;i++) {
        z=z*(z)+c;
        if(z.abs()>2) {
          break;
        }
      }
      line+=z.abs()>2 ? " " : "*";
    }
    print(line);
  }
}
