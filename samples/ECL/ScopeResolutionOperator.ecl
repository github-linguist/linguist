//This example shows the qualification order necessary to reach a specific attribute/parameter:
ds := DATASET([1], { INTEGER SomeValue });

INTEGER SomeValue := 10; //local attribute

myModule(INTEGER SomeValue) := MODULE

      EXPORT anotherFunction(INTEGER SomeValue) := FUNCTION
            tbl := TABLE(ds,
                         { SUM(GROUP, someValue),     // 1  - DATASET field
                           SUM(GROUP, ^.someValue),   // 84 - FUNCTION parmameter
                           SUM(GROUP, ^^.someValue),  // 42 - MODULE parmameter
                           SUM(GROUP, ^^^.someValue), // 10 - local attribute
                           0 });
            RETURN tbl;
      END; //FUNCTION

      EXPORT result := anotherFunction(84);

END; //MODULE

OUTPUT(myModule(42).result);
