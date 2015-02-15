int[] array_concat(int[]a,int[]b){	
	int[] c = new int[a.length + b.length];
	Memory.copy(c, a, a.length * sizeof(int));
	Memory.copy(&c[a.length], b, b.length * sizeof(int));
	return c;
}
void main(){
	int[] a = {1,2,3,4,5};
	int[] b = {6,7,8};
	int[] c = array_concat(a,b);
	foreach(int i in c){
		stdout.printf("%d\n",i);
	}
}
