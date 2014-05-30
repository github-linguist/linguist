/* Example DATA step code for linguist */

libname source 'C:\path\to\file'

data work.working_copy;
	set source.original_file.sas7bdat;
run;

data work.working_copy;
	set work.working_copy;
	if Purge = 1 then delete;
run;

data work.working_copy;
	set work.working_copy;
	if ImportantVariable = . then MissingFlag = 1;
run;