class animal {
public:
  virtual void bark() // concrete virtual, not pure
  {
    throw "implement me: do not know how to bark";
  }
};

class elephant : public animal // does not implement bark()
{
};

int main()
{
  elephant e;
  e.bark();  // throws exception
}
