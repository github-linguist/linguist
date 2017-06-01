pvt:{[t;k;p;v;f;w]
  // t is source table; k is list of key columns; p is list of pivot columns
  // v is col that provides values for result
  // f is aggregate function to be applied 
  //  when pivot col entries are not unique by key value; defaults to first
  // w is string containing valid where clause for t
  //  allow empty list or empty char list
  / make first the default aggregate
  f:$[()~f;first;f];
  / dictionary for grouping in functional queries
  dK:(K)!K:k,();
  / constrain t by w
  t:$[""~"c"$w; t; ?[t;enlist parse w;0b;()]];
  / consolidate pivot col symbol values using "_" separator
  /  prepend "X" to those starting with non-aplha so they are select accessible
  t:t,'flip(enlist`pc)!enlist`${$[any(first v:"_"sv string each x)within("AZ";"az");v;"X_",v]}each t[;(),p];
  / sort unique symbols from consolidated pivot col
  pu:asc distinct t`pc;
  / use f to aggregate v grouped by pivot col, key col
  t:?[t;();(`pc,K)!(`pc,K);(enlist v)!enlist (f;v)];
  / construct pivot vectors as rectangular null-filled fields
  /  select dictionary mapping pivot entries to values; 
  /  rectangularize by extracting all cols from dict
  t:?[t;();dK;(enlist`P)!enlist((!;`pc;v);`pu)];
  / flip row vectors to column vectors and reconstruct keyed table map
  (key t)!flip pu!flip first value flip value t}

\
Here is a pivot function that mimics Excel.  You specify a q table, a
list of columns to use as the (possibly compound) key, a list of columns
on which to pivot, a column that supplies the values, an aggregate
function to be used when the key values are not unique and a where
clause to filter the table.  In the case of multiple pivot columns, the
resulting compound column names are separated by "_".  In the case of a
pivot column value starting with a non-alpha character, an "X_" is
prepended to the corresponding column name. (While the column can be
created without the "X_", it will not be accessible in a select)

Note: The function is not optimized.  The meat is in the last two
expressions.

JAB 

Usage:
======
t:([]k:1 2 3 2 3 1;p:`a1`a2`a1`a3`a2`a1;v:100 200 300 400 500 1000) 

tKP:([]k1:1 2 3 2 3 1 1;k2:10 20 30 40 50 60 10; p1:(`a1;`a2;`a1;`a3;2;`a1;`a1);p2:1 2 1 2 1 2 1; v:100 200 300 400 500 1000 10000)

show pvt[t;`k;`p;`v;();""]
k| a1  a2  a3 
-| -----------
1| 100        
2|     200 400
3| 300 500    

show pvt[tKP;`k1`k2;`p1`p2;`v;sum;""] 
k1 k2| X_2_1 a1_1  a1_2 a2_2 a3_2
-----| --------------------------
1  10|       10100               
1  60|             1000          
2  20|                  200      
2  40|                       400 
3  30|       300                 
3  50| 500     

show pvt[tKP;`k1`k2;`p1`p2;`v;sum;"p2<>2"] 
k1 k2| X_2_1 a1_1 
-----| -----------
1  10|       10100
3  30|       300  
3  50| 500      
