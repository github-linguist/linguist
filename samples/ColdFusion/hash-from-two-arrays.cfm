<cfscript>
function makeHash(keyArray, valueArray) {
  var x = 1;
  var result = {};
  for( ; x <= ArrayLen(keyArray); x ++ ) {
    result[keyArray[x]] = valueArray[x];
  }
  return result;
}

keyArray = ['a', 'b', 'c'];
valueArray = [1, 2, 3];
map = makeHash(keyArray, valueArray);
</cfscript>
