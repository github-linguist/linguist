if (Rows (modelMatrixList) == 0)
{
	modelMatrixList = 
	{
	{"Equal Input", "EIAA.mdl", "19"}
	{"Dayhoff","Dayhoff.mdl","0"}
	{"Dayhoff+F","Dayhoff_F.mdl","19"}
	{"JTT", "Jones.mdl", "0"}
	{"JTT+F", "Jones_F.mdl", "19"}
	{"WAG", "WAG.mdl", "0"}
	{"WAG+F", "WAG_F.mdl", "19"}
	{"rtREV", "rtREV.mdl", "0"}
	{"rtREV+F", "rtREV_F.mdl", "19"}
	{"mtMAM", 		  "mtMAM.mdl", "0"}
	{"mtMAM+F", 	  "mtMAM_F.mdl", "19"}
	{"mtREV 24",      "mtREV_24.mdl", "0"}
	{"mtREV 24+F",    "mtREV_24_F.mdl", "19"}
	{"HIV within",    "HIVwithin.mdl", "0"}
	{"HIV within+F",  "HIVwithin+F.mdl", "19"}
	{"HIV between",   "HIVbetween.mdl", "0"}
	{"HIV between+F", "HIVbetween+F.mdl", "19"}
	{"REV-1 step", "reducedREV.mdl", "19"}
	{"REV",   "mtREV.mdl",    "19"}
	};
}

/*___________________________________________________________________________________________________________*/

function runAModel (modelID, fileName, xtraP, midx)
{
	ExecuteCommands ("#include \"TemplateModels/"+fileName+"\";"); 
	Tree 					givenTree 			= treeString;
	LikelihoodFunction 		lf 					= (filteredData,givenTree);
	
	GetString (lf_info, lf, -1);
	locals = lf_info["Local Independent"];

	if (Columns (branchLengthStash))
	{
		USE_LAST_RESULTS = 1;
		for (_iv = 0; _iv < Columns (locals); _iv = _iv+1)
		{
			ExecuteCommands (locals[_iv] + "=1;\n");
		}
		currentBL = BranchLength (givenTree,0);
		currentBN = BranchName	 (givenTree,-1);
		for (_iv = 0; _iv < Columns (currentBN); _iv = _iv+1)
		{
			ExecuteCommands ("givenTree."+currentBN[_iv]+".t="+branchLengthStash[_iv]/currentBL+";");
			
		}
	}
	else
	{
		for (_iv = 0; _iv < Columns (locals); _iv = _iv+1)
		{
			ExecuteCommands (locals[_iv] + "=0.1;\n");
		}
		USE_LAST_RESULTS = 1;
	}

	Optimize (res,lf);
	
	fprintf (stdout, "| ", modelID);
	for (k=0; k<maxModelWidth-Abs(modelID)-1; k=k+1)
	{
		fprintf (stdout, " ");
	}

	params = res[1][1]+xtraP;
	AIC    =  2(-res[1][0]+params);
	
	if (filteredData.sites-params>1)
	{
		cAIC   = 2(-res[1][0]+params*(filteredData.sites/(filteredData.sites-params-1)));
	}
	else
	{
		cAIC = 0;
	}
	
	branchLengths = BranchLength (givenTree,-1);
	TL = 0;
	for (k=Rows(branchLengths)*Columns(branchLengths)-1; k>=0; k=k-1)				  
	{
		TL = TL + branchLengths[k];
	}

	fprintf (stdout, "| ", Format (res[1][0],14,3), " | ", Format (params,5,0), " | ",
						   Format (AIC, 9,3), " | ",);
					
	if (cAIC > 0)
	{
		 fprintf (stdout, Format (cAIC,11,3), " | ");
	}
	else
	{
		 fprintf (stdout, "    N/A     | ");
	}
		   
	fprintf (stdout, Format (TL,11,3), " |\n", sepString);
	
	resultMatrix[midx][0] = res[1][0];
	resultMatrix[midx][1] = params;
	resultMatrix[midx][2] = AIC;
	resultMatrix[midx][3] = cAIC;
	resultMatrix[midx][4] = TL;
	
	if (AIC < bestAIC)
	{
		bestAIC 	= AIC;
		bestAICidx  = midx;
		branchLengthStash = BranchLength (givenTree,-1);
	}
	
	if (cAIC > 0)
	{
		if (bestCAIC > cAIC)
		{
			bestCAIC = cAIC;
			bestCAICidx = midx;
			
		}
	}
	
	return 0;
}


/*___________________________________________________________________________________________________________*/



maxModelWidth = 7;
skipCodeSelectionStep = 0;

ChoiceList (doREV, "Include REV?", 1, SKIP_NONE, "Yes", "Include REV and reduced REV models. CAUTION: these models take a long time to fit.",
												 "No", "Only use empirical models");
												 
if (doREV < 0)
{
	return 0;
}

if (doREV == 0)
{
	#include "TemplateModels/chooseGeneticCode.def";
	skipCodeSelectionStep = 1;
}

modelCount    = Rows (modelMatrixList) - 2*doREV;

for (k=0; k<modelCount; k=k+1)
{
	maxModelWidth = Max(maxModelWidth,Abs (modelMatrixList[k][0])+2);
}

sepString = "";
capString = "";
sepString * 256;
sepString * "+";

capString * 256;
capString * "| Model";

for (k=0; k<maxModelWidth; k=k+1)
{
	sepString * "-";
}

for (k=0; k<maxModelWidth-6; k=k+1)
{
	capString * " ";
}

capString * "| Log Likelihood | #prms | AIC Score | c-AIC Score | Tree Length |\n";
sepString * "+----------------+-------+-----------+-------------+-------------+\n";
sepString * 0;
capString * 0;

branchLengthStash = 0;

SKIP_MODEL_PARAMETER_LIST = 0;

#include "TemplateModels/modelParameters2.mdl";
if (modelType == 1)
{
	#include "TemplateModels/defineGamma.mdl";
}

if (modelType == 2)
{
	#include "TemplateModels/defineHM.mdl";
}
SKIP_MODEL_PARAMETER_LIST = 1;

SetDialogPrompt ("Please load an amino-acid data file:");

DataSet ds = ReadDataFile (PROMPT_FOR_FILE);
DataSetFilter filteredData = CreateFilter (ds,1);

fprintf (stdout,"\nRunning aminoacid model comparisons on ", LAST_FILE_PATH, "\n\nThe alignment has ",ds.species, " sequences and ", ds.sites, " sites\n");

_DO_TREE_REBALANCE_ = 1;

#include "queryTree.bf";

resultMatrix = {modelCount, 5};

fprintf (stdout, "\n",sepString,capString,sepString);

bestAIC 	= 1e100;
bestCAIC	= 1e100;
bestAICidx	= 0;
bestCAICidx = -1;

for (mid=0; mid<modelCount; mid=mid+1)
{
	runAModel (modelMatrixList[mid][0], modelMatrixList[mid][1], 0+modelMatrixList[mid][2], mid);
}	

fprintf (stdout, "\n\nBest AIC model:\n\t", modelMatrixList[bestAICidx][0], " with the score of ", bestAIC);

if (bestCAICidx>=0)
{
	fprintf (stdout, "\n\nBest c-AIC model:\n\t", modelMatrixList[bestCAICidx][0], " with the score of ", bestCAIC);
}

labelMatrix  = {{"Log-likelihood","Parameters","AIC","c-AIC","Total tree length",""}};

aaString = "Model";

for (fC = 0; fC < modelCount; fC = fC+1)
{
	aaString = aaString + ";" + modelMatrixList[fC][0];
}

USE_LAST_RESULTS = 0;

labelMatrix[5] = aaString;
skipCodeSelectionStep = 0;
OpenWindow (CHARTWINDOW,{{"Model Fits"}
						   {"labelMatrix"},
						   {"resultMatrix"},
						   {"Bar Chart"},
						   {"Index"},
						   {"c-AIC"},
						   {"Model Index"},
						   {""},
						   {"AIC"}
						   },
						   "SCREEN_WIDTH-60;SCREEN_HEIGHT-60;30;30");
