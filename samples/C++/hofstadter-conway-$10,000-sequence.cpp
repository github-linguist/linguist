#include <deque>
#include <iostream>

int hcseq(int n)
{
  static std::deque<int> seq(2, 1);
  while (seq.size() < n)
  {
    int x = seq.back();
    seq.push_back(seq[x-1] + seq[seq.size()-x]);
  }
  return seq[n-1];
}

int main()
{
  int pow2 = 1;
  for (int i = 0; i < 20; ++i)
  {
    int pow2next = 2*pow2;
    double max = 0;
    for (int n = pow2; n < pow2next; ++n)
    {
      double anon = hcseq(n)/double(n);
      if (anon > max)
        max = anon;
    }
    std::cout << "maximum of a(n)/n between 2^" << i
              << " (" << pow2 << ") and 2^" << i+1
              << " (" << pow2next << ") is " << max << "\n";
    pow2 = pow2next;
  }
}
