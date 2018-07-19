import List

data Person = P String Int deriving Eq
instance Show Person where
    show (P name val) = "Person "++name++" with value "++(show val)
instance Ord Person where
    compare (P n1 _) (P n2 _) = compare n1 n2

people = [(P "Joe" 12), (P "Bob" 8), (P "Alice" 9), (P "Harry" 2)]
sortedPeople = sort people
sortedPeopleByVal = sortBy (\(P _ v1) (P _ v2)->compare v1 v2) people
