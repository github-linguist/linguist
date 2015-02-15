#include <iterator>
#include <algorithm>
#include <iostream>

template<typename Struct, typename MemberType> class less_member
{
public:
  less_member(MemberType Struct::*m):
    member(m)
  {
  }
  bool operator()(Struct const& s1, Struct const& s2) const
  {
    return s1.*member < s2.*member;
  }
private:
  MemberType Struct::*const member;
};

template<typename Struct, typename MemberType>
 less_member<Struct, MemberType> make_less_member(MemberType Struct::* m)
{
  return m;
}

template<typename FwdIter, typename MemberPtrType>
 void sort_for_member(FwdIter first, FwdIter last, MemberPtrType m)
{
  std::sort(first, last, make_less_member(m));
}

struct entry
{
  std::string name;
  std::string value;
};

int main()
{
  entry array[] = { { "grass",  "green" },
                    { "snow",   "white" },
                    { "sky",    "blue"  },
                    { "cherry", "red"   } };
  std::cout << "before sorting:\n\n";
  for (int i = 0; i < 4; ++i)
    std::cout << "index: " << i << ", name: " << array[i].name
              << ", value: " << array[i].value << "\n";

  sort_for_member(array, array+4, &entry::name);

  std::cout << "\nafter sorting:\n\n";
  for (int i = 0; i < 4; ++i)
    std::cout << "index: " << i << ", name: " << array[i].name
              << ", value: " << array[i].value << "\n";
  return 0;
}
