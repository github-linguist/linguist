#include <random>
#include <iostream>
#include <stack>
#include <set>
#include <string>
#include <functional>
using namespace std;

class RPNParse
{
public:
  stack<double> stk;
  multiset<int> digits;

  void op(function<double(double,double)> f)
  {
    if(stk.size() < 2)
      throw "Improperly written expression";
    int b = stk.top(); stk.pop();
    int a = stk.top(); stk.pop();
    stk.push(f(a, b));
  }

  void parse(char c)
  {
    if(c >= '0' && c <= '9')
    {
      stk.push(c - '0');
      digits.insert(c - '0');
    }
    else if(c == '+')
      op([](double a, double b) {return a+b;});
    else if(c == '-')
      op([](double a, double b) {return a-b;});
    else if(c == '*')
      op([](double a, double b) {return a*b;});
    else if(c == '/')
      op([](double a, double b) {return a/b;});
  }

  void parse(string s)
  {
    for(int i = 0; i < s.size(); ++i)
      parse(s[i]);
  }

  double getResult()
  {
    if(stk.size() != 1)
      throw "Improperly written expression";
    return stk.top();
  }
};

int main()
{
  random_device seed;
  mt19937 engine(seed());
  uniform_int_distribution<> distribution(1, 9);
  auto rnd = bind(distribution, engine);

  multiset<int> digits;
  cout << "Make 24 with the digits: ";
  for(int i = 0; i < 4; ++i)
  {
    int n = rnd();
    cout << " " << n;
    digits.insert(n);
  }
  cout << endl;

  RPNParse parser;

  try
  {
    string input;
    getline(cin, input);
    parser.parse(input);

    if(digits != parser.digits)
      cout << "Error: Not using the given digits" << endl;
    else
    {
      double r = parser.getResult();
      cout << "Result: " << r << endl;

      if(r > 23.999 && r < 24.001)
        cout << "Good job!" << endl;
      else
        cout << "Try again." << endl;
    }
  }
  catch(char* e)
  {
    cout << "Error: " << e << endl;
  }
  return 0;
}
