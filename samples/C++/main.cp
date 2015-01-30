#include <iostream>
using namespace std

class myClass
{
	void operator!(){}

};

void operator!(myClass){}
int main()
{
	myClass a;
	!a;
}
