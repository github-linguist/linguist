#include <iostream>

class T
{
public:
  virtual void identify() { std::cout << "I am a genuine T" << std::endl; }
  virtual T* clone() { return new T(*this); }
  virtual ~T() {}
};

class S: public T
{
public:
  virtual void identify() { std::cout << "I am an S" << std::endl; }
  virtual S* clone() { return new S(*this); }
};

class X // the class of the object which contains a T or S
{
public:
  // by getting the object through a pointer to T, X cannot know if it's an S or a T
  X(T* t): member(t) {}

  // copy constructor
  X(X const& other): member(other.member->clone()) {}

  // copy assignment operator
  X& operator=(X const& other)
  {
    T* new_member = other.member->clone();
    delete member;
    member = new_member;
  }

  // destructor
  ~X() { delete member; }

  // check what sort of object it contains
  void identify_member() { member->identify(); }

private:
  T* member;
};

int main()
{
  X original(new S);      // construct an X and give it an S,
  X copy = original;      // copy it,
  copy.identify_member(); // and check what type of member it contains
}
