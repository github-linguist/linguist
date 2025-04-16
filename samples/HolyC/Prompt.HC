U0 PromptDemo()
{
	F64 d;
	I64 i;
	U8 *name;
	CDate cdt;

	"\n\nNow prompting from the cmd line.\n\n"
	"$$GREEN$$<ENTER>$$FG$$ keeps default.\n\n";

	i=GetI64("Enter int expression(%d):",123);
 	"Int=%d\n",i;

	d=GetF64("Enter float expression(%12g):",1.2e3);
	"Float=%e\n",d;

	name=GetStr("Name (%s):","John");
	"Name:%s\n",name;
	Free(name);

	cdt=GetDate("Date (%D):",Now);
	"Date=%D\n",cdt;
}

PromptDemo;
