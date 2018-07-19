import 'dart:math' as Math;
cube(x) => x*x*x;
cuberoot(x)  => Math.pow(x, 1/3);
compose(f,g) => ((x)=>f(g(x)));
main(){
  var functions = [Math.sin, Math.exp, cube];
  var inverses = [Math.asin, Math.log, cuberoot];
  for (int i = 0; i < 3; i++){
    print(compose(functions[i], inverses[i])(0.5));
  }
}
