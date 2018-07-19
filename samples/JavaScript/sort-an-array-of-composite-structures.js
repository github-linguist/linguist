var arr = [
  {id: 3, value: "foo"},
  {id: 2, value: "bar"},
  {id: 4, value: "baz"},
  {id: 1, value: 42},
  {id: 5, something: "another string"} // Works with any object declaring 'id' as a number.
];
arr = arr.sort(function(a, b) {return a.id - b.id}); // Sort with comparator checking the id.
