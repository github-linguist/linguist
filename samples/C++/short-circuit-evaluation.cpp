#include <iostream>

bool a(bool in)
{
    std::cout << "a" << std::endl;
    return in;
}

bool b(bool in)
{
    std::cout << "b" << std::endl;
    return in;
}

void test(bool i, bool j) {
    std::cout << std::boolalpha << i << " and " << j << " = " << (a(i) && b(j)) << std::endl;
    std::cout << std::boolalpha << i << " or " << j << " = " << (a(i) || b(j)) << std::endl;
}

int main()
{
    test(false, false);
    test(false, true);
    test(true, false);
    test(true, true);
    return 0;
}
