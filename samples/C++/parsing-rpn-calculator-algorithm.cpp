#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <iterator>
#include <cstdlib>

double rpn(const std::string &expr){
  std::istringstream iss(expr);
  std::vector<double> stack;
  std::cout << "Input\tOperation\tStack after" << std::endl;
  std::string token;
  while (iss >> token) {
    std::cout << token << "\t";
    double tokenNum;
    if (std::istringstream(token) >> tokenNum) {
      std::cout << "Push\t\t";
      stack.push_back(tokenNum);
    } else {
      std::cout << "Operate\t\t";
      double secondOperand = stack.back();
      stack.pop_back();
      double firstOperand = stack.back();
      stack.pop_back();
      if (token == "*")
	stack.push_back(firstOperand * secondOperand);
      else if (token == "/")
	stack.push_back(firstOperand / secondOperand);
      else if (token == "-")
	stack.push_back(firstOperand - secondOperand);
      else if (token == "+")
	stack.push_back(firstOperand + secondOperand);
      else if (token == "^")
	stack.push_back(std::pow(firstOperand, secondOperand));
      else { //just in case
	std::cerr << "Error" << std::endl;
	std::exit(1);
      }
    }
    std::copy(stack.begin(), stack.end(), std::ostream_iterator<double>(std::cout, " "));
    std::cout << std::endl;
  }
  return stack.back();
}

int main() {
  std::string s = " 3 4 2 * 1 5 - 2 3 ^ ^ / + ";
  std::cout << "Final answer: " << rpn(s) << std::endl;

  return 0;
}
