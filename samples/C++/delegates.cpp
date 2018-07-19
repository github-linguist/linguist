#include <tr1/memory>
#include <string>
#include <iostream>
#include <tr1/functional>

using namespace std;
using namespace std::tr1;
using std::tr1::function;

// interface for all delegates
class IDelegate
{
public:
    virtual ~IDelegate() {}
};

//interface for delegates supporting thing
class IThing
{
public:
    virtual ~IThing() {}
    virtual std::string Thing() = 0;
};

// Does not handle Thing
class DelegateA : virtual public IDelegate
{
};

// Handles Thing
class DelegateB : public IThing, public IDelegate
{
    std::string Thing()
    {
        return "delegate implementation";
    }
};

class Delegator
{
public:
    std::string Operation()
    {
        if(Delegate) //have delegate
           if (IThing * pThing = dynamic_cast<IThing*>(Delegate.get()))
            //delegate provides IThing interface
            return pThing->Thing();

        return "default implementation";
    }

    shared_ptr<IDelegate> Delegate;
};

int main()
{
    shared_ptr<DelegateA> delegateA(new DelegateA());
    shared_ptr<DelegateB> delegateB(new DelegateB());
    Delegator delegator;

    // No delegate
    std::cout << delegator.Operation() << std::endl;

    // Delegate doesn't handle "Thing"
    delegator.Delegate = delegateA;
    std::cout << delegator.Operation() << std::endl;

    // Delegate handles "Thing"
    delegator.Delegate = delegateB;
    std::cout << delegator.Operation() << std::endl;

/*
Prints:

  default implementation
  default implementation
  delegate implementation
 */
}
