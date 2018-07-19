*--Load the test data;
data test1;
   input x @@;
   obs=_n_;
datalines;
2 4 4 4 5 5 7 9
;
run;

*--Create a dataset with the cummulative data for each set of data for which the SD should be calculated;
data test2 (drop=i obs);
   set test1;
   y=x;
   do i=1 to n;
      set test1 (rename=(obs=setid)) nobs=n point=i;
      if obs<=setid then output;
   end;
proc sort;
   by setid;
run;

*--Calulate the standards deviation (and mean) using PROC MEANS;
proc means data=test2 vardef=n noprint; *--use vardef=n option to calculate the population SD;
   by setid;
   var y;
   output out=stat1 n=n mean=mean std=sd;
run;

*--Output the calculated standard deviations;
proc print data=stat1 noobs;
   var n sd /*mean*/;
run;
