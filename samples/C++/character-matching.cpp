#include <string>
using namespace std;

string s1="abcd";
string s2="abab";
string s3="ab";
//Beginning
s1.compare(0,s3.size(),s3)==0;
//End
s1.compare(s1.size()-s3.size(),s3.size(),s3)==0;
//Anywhere
s1.find(s2)//returns string::npos
int loc=s2.find(s3)//returns 0
loc=s2.find(s3,loc+1)//returns 2
