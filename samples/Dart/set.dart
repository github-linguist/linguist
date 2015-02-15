void main(){
  //Set Creation
  Set A = new Set.from([1,2,3]);
  Set B = new Set.from([1,2,3,4,5]);
  Set C = new Set.from([1,2,4,5]);
	
  print('Set A = $A');
  print('Set B = $B');
  print('Set C = $C');
  print('');
  //Test if element is in set
  int m = 3;
  print('m = 5');
  print('m in A = ${A.contains(m)}');
  print('m in B = ${B.contains(m)}');
  print('m in C = ${C.contains(m)}');
  print('');
  //Union of two sets
  Set AC = A.union(C);
  print('Set AC = Union of A and C = $AC');
  print('');
  //Intersection of two sets
  Set A_C = A.intersection(C);
  print('Set A_C = Intersection of A and C = $A_C');
  print('');
  //Difference of two sets
  Set A_diff_C = A.difference(C);
  print('Set A_diff_C = Difference between A and C = $A_diff_C');
  print('');
  //Test if set is subset of another set
  print('A is a subset of B = ${B.containsAll(A)}');
  print('C is a subset of B = ${B.containsAll(C)}');
  print('A is a subset of C = ${C.containsAll(A)}');
  print('');
  //Test if two sets are equal
  print('A is equal to B  = ${B.containsAll(A) && A.containsAll(B)}');
  print('B is equal to AC = ${B.containsAll(AC) && AC.containsAll(B)}');
}
