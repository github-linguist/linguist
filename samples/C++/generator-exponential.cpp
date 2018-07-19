#include <iostream>
using namespace std;

template<class T>
class Generator
{
public:
  virtual T operator()() = 0;
};

// Does nothing unspecialized
template<class T, T P>
class PowersGenerator: Generator<T> {};

// Specialize with other types, or provide a generic version of pow
template<int P>
class PowersGenerator<int, P>: Generator<int>
{
public:
  int i;
  PowersGenerator() { i = 1; }
  virtual int operator()()
  {
    int o = 1;
    for(int j = 0; j < P; ++j) o *= i;
    ++i;
    return o;
  }
};

// Only works with non-decreasing generators
template<class T, class G, class F>
class Filter: Generator<T>
{
public:
  G gen;
  F filter;
  T lastG, lastF;

  Filter() { lastG = gen(); lastF = filter(); }

  virtual T operator()()
  {
    while(lastG >= lastF)
    {
      if(lastG == lastF)
        lastG = gen();
      lastF = filter();
    }

    T out = lastG;
    lastG = gen();
    return out;
  }
};

int main()
{
  Filter<int, PowersGenerator<int, 2>, PowersGenerator<int, 3>> gen;

  for(int i = 0; i < 20; ++i)
    gen();

  for(int i = 20; i < 30; ++i)
    cout << i << ": " << gen() << endl;
}
