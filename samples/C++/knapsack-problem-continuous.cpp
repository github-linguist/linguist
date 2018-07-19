#include<iostream>
#include<algorithm>
#include<string.h>

using namespace std;
double result;
double capacity = 15;
int NumberOfItems;
int number;

struct items
{
    char name[32];
    double weight;
    double price;
    double m;
} item[256];

bool cmp(items a,items b)
{
    return a.price/a.weight > b.price/b.weight; // the compare function for the sorting algorithm
}

int main()
{
NumberOfItems=9;
strcpy(item[1].name,"beef");
item[1].weight=3.8;
item[1].price=36;

strcpy(item[2].name,"pork");
item[2].weight=5.4;
item[2].price=43;

strcpy(item[3].name,"ham");
item[3].weight=3.6;
item[3].price=90;

strcpy(item[4].name,"greaves");
item[4].weight=2.4;
item[4].price=45;

strcpy(item[5].name,"flitch");
item[5].weight=4.0;
item[5].price=30;

strcpy(item[6].name,"brawn");
item[6].weight=2.5;
item[6].price=56;

strcpy(item[7].name,"welt");
item[7].weight=3.7;
item[7].price=67;

strcpy(item[8].name,"salami");
item[8].weight=3.0;
item[8].price=95;

strcpy(item[9].name,"sausage");
item[9].weight=5.9;
item[9].price=98;


sort(item+1,item+NumberOfItems+1,cmp); // We'll sort using Introsort from STL

 number = 1;
 while(capacity>0&&number<=NumberOfItems)
 {
  if(item[number].weight<=capacity)
    {
        result+=item[number].price;
        capacity-=item[number].weight;
        item[number].m=1;
    }
  else
  {
      result+=(item[number].price)*(capacity/item[number].weight);
      item[number].m=(capacity/item[number].weight);
      capacity=0;

  }
  ++number;
 }

cout<<"Total Value = "<<result<<'\n';
cout<<"Total Weight = "<<(double)15-capacity<<'\n';
cout<<"Items Used:\n";
for(int i=1;i<=NumberOfItems;++i)
    if(item[i].m)
    {
       cout<<"We took "<<item[i].m*item[i].weight<<"kg of \""<<item[i].name<<"\" and the value it brought is "<<item[i].price*item[i].m<<"\n";
    }

return 0;
}
