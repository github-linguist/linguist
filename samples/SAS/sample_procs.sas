/* This is a comment */
* So is this;
/* Commonly used PROCs in SAS as examples */

proc freq data=work.work;
	Tables Variable1*Variable2;
run;

proc logistic data=work.work;
	model Variable1 = Variable2;
run;

proc univariate data=work.work2;
	var Variable3;
	Histogram;
run;