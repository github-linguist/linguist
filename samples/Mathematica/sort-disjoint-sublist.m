Values = { 7, 6, 5, 4, 3, 2, 1, 0} ; Indices = { 7, 2, 8 };
Values[[Sort[Indices]]] = Sort[Values[[Indices]]];

Values
-> { 7, 0, 5, 4, 3, 2, 1, 6 }
