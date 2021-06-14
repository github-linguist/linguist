MyRec := RECORD
    STRING1 Value1;
    STRING1 Value2;
END;

SomeFile  := DATASET([{'C', 'G'}, 
                      {'C', 'C'}], MyRec);
OtherFile := DATASET([{'A', 'X'}, 
                      {'B', 'G'}, 
                      {'A', 'B'}], MyRec);

MAC_AddCat(AttrName, FirstArg, SecondArg) := MACRO
  AttrName := FirstArg + SecondArg;
  OUTPUT(AttrName);
ENDMACRO;

MAC_AddCat(AddValues, 5, 10)
/*generates this code:
  AddValues := 5 + 10;
  OUTPUT(AddValues);
  
which results in: 15
*/

MAC_AddCat(CatValues, '5', '10')
/*generates this code:
  CatValues := '5' + '10';
  OUTPUT(CatValues);

which results in: '510'
*/

MAC_AddCat(JoinFiles, SORT(SomeFile, Value1, Value2), SORT(OtherFile, Value1, Value2));
/*generates this code:
  JoinFiles := SORT(SomeFile, Value1, Value2) + SORT(OtherFile, Value1, Value2);
  OUTPUT(JoinFiles);
  
which results in:
  Rec#    Value1  Value2
  1       C       C
  2       C       G
  3       A       B
  4       A       X
  5       B       G
*/
