#include "molclockBootstrap.bf";

RESTORE_GLOBALS 	= 1;
_DO_TREE_REBALANCE_ = 0;
VERBOSITY_LEVEL     = -1;

function RestoreGlobalValues (lfIndex)
{
	if (lfIndex==0)
	{
		for (i=0;i<SAVE_GLOBALS;i=i+1)
		{
			SetParameter (lf,i,globalSpoolMatrix[i]);
		}
	}
	if (lfIndex==1)
	{
		for (i=0;i<SAVE_GLOBALS2;i=i+1)
		{
			SetParameter (lfConstrained,i,globalSpoolMatrix2[i]);
		}
	}
	return 0;
}



fprintf(stdout,"\n ---- RUNNING MOLECULAR CLOCK ANALYSIS ---- \n");

ChoiceList (dataType,"Data type",1,SKIP_NONE,"Nucleotide/Protein","Nucleotide or amino-acid (protein).",
				     "Codon","Codon (several available genetic codes).");

if (dataType<0) 
{
	return;
}
if (dataType)
{
	NICETY_LEVEL = 3;
	#include "TemplateModels/chooseGeneticCode.def";
}

SetDialogPrompt ("Choose the data file:");

DataSet ds = ReadDataFile (PROMPT_FOR_FILE);

fprintf (stdout,"The following data was read:\n",ds,"\n");

if (dataType)
{
	DataSetFilter filteredData = CreateFilter (ds,3,"","",GeneticCodeExclusions);
}
else
{
	DataSetFilter filteredData = CreateFilter (ds,1);
}

SelectTemplateModel(filteredData);

#include "queryTree.bf";

global RelRatio;

RelRatio = 1.0;

relationString = ":=RelRatio*";

parameter2Constrain = 0;

if (Rows("LAST_MODEL_PARAMETER_LIST")>1)
{
	ChoiceList (parameter2Constrain, "Parameter(s) to constrain:",1,SKIP_NONE,LAST_MODEL_PARAMETER_LIST);

	if (parameter2Constrain<0)
	{
		return;
	}
	if (parameter2Constrain==0)
	{
		parameter2ConstrainString = "";
		for (parameter2Constrain=Rows("LAST_MODEL_PARAMETER_LIST")-1; parameter2Constrain; parameter2Constrain = parameter2Constrain-1)
		{
			GetString (funnyString,LAST_MODEL_PARAMETER_LIST,parameter2Constrain);
			parameter2ConstrainString = parameter2ConstrainString + funnyString + ",";
		}
		GetString (funnyString,LAST_MODEL_PARAMETER_LIST,0);
		parameter2ConstrainString = parameter2ConstrainString + funnyString;
	}
	else
	{
		GetString (parameter2ConstrainString,LAST_MODEL_PARAMETER_LIST,parameter2Constrain-1);
	}
}
else
{
	GetString (parameter2ConstrainString,LAST_MODEL_PARAMETER_LIST,0);
}

timer = Time(0);

LikelihoodFunction lf = (filteredData,givenTree);

Optimize (res,lf);

separator = "*-----------------------------------------------------------*";

fprintf (stdout, "\n", separator, "\nRESULTS WITHOUT THE CLOCK:\n",lf);

fullModelLik = res[1][0];

fullVars = res[1][1];

/* now specify the constraint */

Tree clockTree = treeString;

ExecuteCommands ("MolecularClock (clockTree,"+parameter2ConstrainString+");");

LikelihoodFunction lfConstrained = (filteredData, clockTree);

USE_LAST_RESULTS = 1;
Optimize (res1,lfConstrained);
USE_LAST_RESULTS = 0;

SAVE_GLOBALS = res1[1][2];

if (SAVE_GLOBALS)
{
	globalSpoolMatrix = {1,SAVE_GLOBALS};

	for (i=0;i<SAVE_GLOBALS;i=i+1)
	{
		globalSpoolMatrix[i]=res1[0][i];
	}
}

fprintf (stdout, "\n", separator,"\n\nRESULTS WITH THE CLOCK:\n",lfConstrained);

lnLikDiff = 2(fullModelLik-res1[1][0]);

degFDiff = fullVars - res1[1][1];

fprintf (stdout, "\n", separator,"\n\n-2(Ln Likelihood Ratio)=",lnLikDiff,"\n","Constrained parameters:",Format(degFDiff,0,0));

fprintf (stdout, "\nP-Value:",1-CChi2(lnLikDiff,degFDiff));

fprintf (stdout, "\nCPU time taken: ", Time(0)-timer, " seconds.\n");
