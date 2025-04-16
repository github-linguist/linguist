U0 RandPts()
{
	I64 i;
	F64 t0=tS;
	do {
		for (i=0;i<1024;i++) {
			gr.dc->color=RandU16&15;
			GrPlot(gr.dc,RandU32%GR_WIDTH,RandU32%GR_HEIGHT);
		}
		Yield;
	} while (tS-t0<1);
	DCFill;
}

U0 RandDemo()
{
	I64 i;

	"Seed(0) will do timer XORed random numbers.  "
	"A task starts with $LK,"Seed(0)",A="FF:::/Kernel/KTask.HC,Seed("$ by dft.\n";
	PressAKey;
	Seed;
	RandPts;
	DCFill;

	"\nSeed(num) will do deterministic, non-timer random numbers starting at "
	"a specific seed.  You can run a specific video game over and over "
	"by typing Seed(num) at the cmd line before the game.\n";
	PressAKey;
	Seed(1);
	RandPts;
	DCFill;

	"\n\n\nWatch when we specify specific seeds:";

	"\n\nSeed(1);\n";
	Seed(1);
	for (i=0;i<5;i++)
		"%d: %d\n",i,RandI16;

	"\nSeed(2); Different, right?\n";
	Seed(2);
	for (i=0;i<5;i++)
		"%d: %d\n",i,RandI16;

	"\nSeed(1); Repeats earlier values.\n";
	Seed(1);
	for (i=0;i<5;i++)
		"%d: %d\n",i,RandI16;

	"\nSeed(0); Zero is special.\n";
	Seed(0);
	for (i=0;i<5;i++)
		"%d: %d\n",i,RandI16;

	"\nSeed(0); Non-repeating.\n";
	Seed(0);
	for (i=0;i<5;i++)
		"%d: %d\n",i,RandI16;

	Seed; // Dft should be kept to zero.
}

RandDemo;
