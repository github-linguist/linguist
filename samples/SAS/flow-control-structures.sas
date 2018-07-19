/* GOTO: as in other languages
   STOP: to stop current data step */
data _null_;
	n=1;
	p=1;
L1:
	put n p;
	n=n+1;
	if n<=p then goto L1;
	p=p+1;
	n=1;
	if p>10 then stop;
	goto L1;

run;

/* LINK: equivalent of GOSUB in BASIC
   RETURN: after a LINK, or to return to the beginning of data step */
data _null_;
input a b;
link gcd;
put a b gcd;
return;

gcd:
	_a=a;
	_b=b;
	do while(_b>0);
	_r=mod(_a,_b);
	_a=_b;
	_b=_r;
	end;
	gcd=_a;
	return;

cards;
2 15
533 221
8 44
;
run;
