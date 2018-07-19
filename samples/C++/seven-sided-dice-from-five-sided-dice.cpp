template<typename F> class fivetoseven
{
public:
  fivetoseven(F f): d5(f), rem(0), max(1) {}
  int operator()();
private:
  F d5;
  int rem, max;
};

template<typename F>
 int fivetoseven<F>::operator()()
{
  while (rem/7 == max/7)
  {
    while (max < 7)
    {
      int rand5 = d5()-1;
      max *= 5;
      rem = 5*rem + rand5;
    }

    int groups = max / 7;
    if (rem >= 7*groups)
    {
      rem -= 7*groups;
      max -= 7*groups;
    }
  }

  int result = rem % 7;
  rem /= 7;
  max /= 7;
  return result+1;
}

int d5()
{
  return 5.0*std::rand()/(RAND_MAX + 1.0) + 1;
}

fivetoseven<int(*)()> d7(d5);

int main()
{
  srand(time(0));
  test_distribution(d5, 1000000, 0.001);
  test_distribution(d7, 1000000, 0.001);
}
