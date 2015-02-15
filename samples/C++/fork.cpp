#include<iostream>
#include<unistd.h>

int main()
{
  pid_t pid = fork();

  if (pid == 0)
  {
    std::cout << "This is the new process\n";
  }
  else if (pid > 0)
  {
    std::cout << "This is the original process\n";
  }
  else
  {
    std::cerr << "ERROR: Something went wrong\n";
  }

  return 0;
}
