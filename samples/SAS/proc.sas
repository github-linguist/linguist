/* PROC examples for Linguist */

proc surveyselect data=work.data out=work.boot method=urs reps=20000 seed=2156 sampsize=28 outhits;
	samplingunit Site;	
run;

PROC MI data=work.boot out=work.bootmi nimpute=30 seed=5686 round = 1;
	By Replicate;
	VAR Variable1 Variable2;
run;

proc logistic data=work.bootmi descending;
	By Replicate _Imputation_;
	model Outcome = Variable1 Variable2 / risklimits;
run;