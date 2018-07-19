num dot(List<num> A, List<num> B){
  if (A.length != B.length){
    throw new Exception('Vectors must be of equal size');
  }
  num result = 0;
  for (int i = 0; i < A.length; i++){
    result += A[i] * B[i];
  }
  return result;
}

void main(){
  var l = [1,3,-5];
  var k = [4,-2,-1];
  print(dot(l,k));
}
