void print_logic(bool a, bool b)
{
  std::cout << std::boolalpha; // so that bools are written as "true" and "false"
  std::cout << "a and b is " << (a && b) << "\n";
  std::cout << "a or b is " << (a || b) << "\n";
  std::cout << "not a is " << (!a) << "\n";
}
