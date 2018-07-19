#include <iostream>
#include <fstream>

int main(){
  std::ofstream lprFile;
  lprFile.open( "/dev/lp0" );
  lprFile << "Hello World!\n";
  lprFile.close();
  return 0;
}
