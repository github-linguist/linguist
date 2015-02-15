   mycmp=: 1 :'/:u'
   length_and_lex =: (-@:# ; lower)&>
   strings=: 'Here';'are';'some';'sample';'strings';'to';'be';'sorted'
   length_and_lex mycmp strings
+-------+------+------+----+----+---+--+--+
|strings|sample|sorted|Here|some|are|be|to|
+-------+------+------+----+----+---+--+--+
