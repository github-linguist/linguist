example = Import["test.txt", "XML"];
Cases[example, XMLElement["item", _ , _] , Infinity] // First
Cases[example, XMLElement["price", _, List[n_]] -> n, Infinity] // Column
Cases[example, XMLElement["name", _, List[n_]] -> n, Infinity] // Column
