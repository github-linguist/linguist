public array function powerset(required array data)
{
  var ps = [""];
  var d = arguments.data;
  var lenData = arrayLen(d);
  var lenPS = 0;
  for (var i=1; i LTE lenData; i++)
  {
    lenPS = arrayLen(ps);
    for (var j = 1; j LTE lenPS; j++)
    {
      arrayAppend(ps, listAppend(ps[j], d[i]));
    }
  }
  return ps;
}

var res = powerset([1,2,3,4]);
