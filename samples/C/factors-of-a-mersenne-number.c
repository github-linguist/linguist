int isPrime(int n){
	if (n%2==0) return n==2;
	if (n%3==0) return n==3;
	int d=5;
	while(d*d<=n){
		if(n%d==0) return 0;
		d+=2;
		if(n%d==0) return 0;
		d+=4;}
	return 1;}

main() {int i,d,p,r,q=929;
	if (!isPrime(q)) return 1;
	r=q;
	while(r>0) r<<=1;
	d=2*q+1;
	do { 	for(p=r, i= 1; p; p<<= 1){
			i=((long long)i * i) % d;
			if (p < 0) i *= 2;
			if (i > d) i -= d;}
		if (i != 1) d += 2*q;
		else break;
	} while(1);
	printf("2^%d - 1 = 0 (mod %d)\n", q, d);}
