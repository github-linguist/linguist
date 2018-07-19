template<bool Condition, typename ThenType, typename Elsetype> struct ifthenelse;

template<typename ThenType, typename ElseType> struct ifthenelse<true, ThenType, ElseType>
{
  typedef ThenType type;
};

template<typename ThenType, typename ElseType> struct ifthenelse<false, ThenType, ElseType>
{
  typedef ElseType type;
};

// example usage: select type based on size
ifthenelse<INT_MAX == 32767, // 16 bit int?
           long int,         // in that case, we'll need a long int
           int>              // otherwise an int will do
  ::type myvar;              // define variable myvar with that type
