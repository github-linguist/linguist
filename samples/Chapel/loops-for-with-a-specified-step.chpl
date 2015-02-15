// Can be set on commandline via --N=x
config const N = 3;

for i in 1 .. 10 by N {
  writeln(i);
}
