#include <iostream>
using namespace std;
long ackermann(long x, long y)
{
     if (x == 0)        return y+1;
     else if (y == 0)        return ackermann(x-1, 1);
     else return ackermann(x-1, ackermann(x, y-1));
}

int main()
{
        long x,y;
        cout << "x ve y..:";
        cin>>x;
        cin>>y;
        cout<<ackermann(x,y);
        return 0;
}
