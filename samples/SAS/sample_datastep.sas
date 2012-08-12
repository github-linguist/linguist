/* This is a comment */
/* Code to generate truncated normal distribution */
/* From www.confounding.net */

data work.trunc_norm;
	mean = 0;
	sd = 1;
	max = 2;
	min = -2;
	do i = 1 to 10000;
		random = max+1;
		do while (random<min or random>max);
			random=rannor(-1)*sd+mean;
		end;
		output;
	end;
run;
