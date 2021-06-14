/*
    Example code - use without restriction.  
*/

Layout_Person := RECORD
    UNSIGNED1 PersonID;
    STRING15  FirstName;
    STRING25  LastName;
END;

Person := DATASET([ {1, 'Fred', 'Smith'}, 
                    {2, 'Joe', 'Blow'}, 
                    {3, 'Jane', 'Smith'}], Layout_Person);

SortedPerson := SORT(Person, LastName, FirstName);

//  Outputs  ---
SortedPerson;
