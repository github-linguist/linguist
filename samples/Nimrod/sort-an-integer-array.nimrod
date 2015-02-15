import algorithm

var a: array[0..8,int] = [2,3,5,8,4,1,6,9,7]
a.sort(system.cmp[int], Ascending)
for x in a:
   echo(x)
