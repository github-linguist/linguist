/* Written by SL Kosakovsky Pond (sergeilkp@mac.com) October 2006 */
/* This analysis uses a 4 bin general bivariate discrete distributions, 
   with 2 negatively selected classes, a positively selected class and a neutral class,
   to compare the distributions of dS and dN in two data sets.
   
   The tests done:
   
   - Are the distributions the same?
   - Are there the same proportions of sites with dN>dS in the data sets?
   - Do the sites under positive selection share the same dN/dS?
   - Two previous tests combined
*/

RequireVersion ("0.9920061001");

/*---------------------------------------------------------------------------------------------------------------------------------------------------*/

function BuildCodonFrequencies (obsF)
{
	PIStop = 1.0;
	result = {ModelMatrixDimension,1};
	hshift = 0;

	for (h=0; h<64; h=h+1)
	{
		first = h$16;
		second = h%16$4;
		third = h%4;
		if (_Genetic_Code[h]==10) 
		{
			hshift = hshift+1;
			PIStop = PIStop-obsF[first][0]*obsF[second][1]*obsF[third][2];
			continue; 
		}
		result[h-hshift][0]=obsF[first][0]*obsF[second][1]*obsF[third][2];
	}
	return result*(1.0/PIStop);
}

/*---------------------------------------------------------------------------------------------------------------------------------------------------*/

function ReportDistributionString (rc,freqStrMx,infix, skip0)
{
	distroString = "";
	distroString * 1024;
	
	reportMx = {rc,4};
	
	distroString * ("   dN/dS      dS      dN      Prob\n");
	for (mi=0; mi<rc; mi=mi+1)
	{
		ExecuteCommands ("reportMx[mi][0]=S_"+infix+mi+"/c_"+infix+"scale;");
		ExecuteCommands ("reportMx[mi][1]=NS_"+infix+mi+"/c_"+infix+"scale;");
		reportMx[mi][2] = reportMx[mi][1]/reportMx[mi][0];
		ExecuteCommands ("reportMx[mi][3]="+freqStrMx[mi]+";");
		if (skip0 == 0 || reportMx[mi][3] > 0)
		{
			distroString * (Format(reportMx[mi][2],8,3)+Format(reportMx[mi][0],8,3)+Format(reportMx[mi][1],8,3)+Format(reportMx[mi][3],10,3)+"\n");
		}
	}								   
	
	distroString * 0;
	return distroString;
}

/*---------------------------------------------------------------------------------------------------------------------------------------------------*/

function DefineNucleotideBiases (dummy)
{
	ModelTitle 				= "MG94x"+modelDesc[0];
	modelConstraintString = "";

	for (customLoopCounter2=0; customLoopCounter2<6; customLoopCounter2=customLoopCounter2+1)
	{
		if (rateBiasTerms[customLoopCounter2] != "1")
		{
			ExecuteCommands ("global " + rateBiasTerms[customLoopCounter2] + "=1;");
		}
	}

	for (customLoopCounter2=1; customLoopCounter2<6; customLoopCounter2=customLoopCounter2+1)
	{
		for (customLoopCounter=0; customLoopCounter<customLoopCounter2; customLoopCounter=customLoopCounter+1)
		{
			if (modelDesc[customLoopCounter2]==modelDesc[customLoopCounter])
			{
				ModelTitle  = ModelTitle+modelDesc[customLoopCounter2];	
				if (rateBiasTerms[customLoopCounter2] == "1")
				{
					modelConstraintString = modelConstraintString + rateBiasTerms[customLoopCounter]+":="+rateBiasTerms[customLoopCounter2]+";";
				}
				else
				{
					modelConstraintString = modelConstraintString + rateBiasTerms[customLoopCounter2]+":="+rateBiasTerms[customLoopCounter]+";";			
				}
				break;
			}
		}
		if (customLoopCounter==customLoopCounter2)
		{
			ModelTitle = ModelTitle+modelDesc[customLoopCounter2];	
		}
	}	


	if (Abs(modelConstraintString))
	{
		ExecuteCommands (modelConstraintString);
	}
	return 0;
}

/*---------------------------------------------------------------------------------------------------------------------------------------------------*/

function 		defineRateDistribution (infix)
{
	ExecuteCommands ("global S_"+infix+"0  = 0.5;S_"+infix+"0:>0.0000001;global NS_"+infix+"0 = 0.1;");

	for (mi=1; mi<resp; mi=mi+1)
	{
		categDef1 * ("global S_"+infix+mi+"=0.5;S_"+infix+mi+":>0.0000001;\nglobal NS_"+infix+mi+";\n");
		if (randomizeInitValues)
		{
			categDef1*("global P_"+infix+mi+" = Random(0.05,0.95);\nP_"+infix+mi+":<1;\n");	
			categDef1*("global S_"+infix+mi+" = Random(0.05,1);");	
		}
		else
		{
			categDef1*("global P_"+infix+mi+" = 1/"+(resp+1-mi)+";\nP_"+infix+mi+":<1;\n");
		}
	}

	freqStrMx    = {resp,1};
	if (resp>1)
	{
		freqStrMx[0] = "P_"+infix+"1";

		for (mi=1; mi<resp-1; mi=mi+1)
		{
			freqStrMx[mi] = "";
			for (mi2=1;mi2<=mi;mi2=mi2+1)
			{
				freqStrMx[mi] = freqStrMx[mi]+"(1-P_"+infix+mi2+")";		
			}
			freqStrMx[mi] = freqStrMx[mi]+"P_"+infix+(mi+1);	
		}	
		freqStrMx[mi] = "";

		for (mi2=1;mi2<mi;mi2=mi2+1)
		{
			freqStrMx[mi] = freqStrMx[mi]+"(1-P_"+infix+mi2+")";		
		}
		freqStrMx[mi] = freqStrMx[mi]+"(1-P_"+infix+mi+")";	
	}
	else
	{
		freqStrMx[0] = "1";
	}

	categDef1*( "\n\nglobal c_"+infix+"scale:=S_"+infix+"0*" + freqStrMx[0]);

	for (mi=1; mi<resp; mi=mi+1)
	{
		categDef1*( "+S_"+infix+mi+"*" + freqStrMx[mi]);
	}

	categDef1 * ";";

	for (mi=0; mi<resp; mi=mi+1)
	{
		categDef1 * ("\nglobal R_"+infix+mi+"=1;NS_"+infix+mi+":=R_"+infix+mi+"*S_"+infix+mi+";\n");
	}

	if (stratDNDS)
	{
		for (mi=respP; mi<respN+respP; mi=mi+1)
		{
			categDef1 * ("\nR_"+infix+mi+":=1;");
		}

		if (randomizeInitValues)
		{
			for (mi=respP+respN; mi<resp; mi=mi+1)
			{
				categDef1 * ("\nR_"+infix+mi+":<1;R_"+infix+mi+"="+Random(0.05,0.95)+";");
			}

			for (mi=0; mi<respP; mi=mi+1)
			{
				categDef1 * ("\nR_"+infix+mi+":>1;R_"+infix+mi+"="+Random(1.05,10)+";");
			}
		}
		else
		{
			for (mi=respN+respP; mi<resp; mi=mi+1)
			{
				categDef1 * ("\nR_"+infix+mi+":<1;R_"+infix+mi+"=1/"+(2+mi-respN-respP)+";");
			}

			for (mi=0; mi<respP; mi=mi+1)
			{
				categDef1 * ("\nR_"+infix+mi+":>1;R_"+infix+mi+"=2+"+mi+";");
			}
		}
	}
	else
	{
		if (randomizeInitValues)
		{
			for (mi=0; mi<resp; mi=mi+1)
			{
				categDef1 * ("\nR_"+infix+mi+"="+Random(0.05,1.75)+";");
			}
		}
		else
		{
			for (mi=0; mi<resp; mi=mi+1)
			{
				categDef1 * ("\nR_"+infix+mi+"="+(0.1+0.3*mi)+";");
			}	
		}

	}

	categDef1 * 0;
	return 0;
}
/*---------------------------------------------------------------------------------------------------------------------------------------------------*/

ChoiceList (branchLengths,"Branch Lengths",1,SKIP_NONE,
			"Codon Model","Jointly optimize rate parameters and branch lengths (slow and thorough)",
			"Nucleotide Model","Estimate branch lengths once, using an appropriate nucleotide model (quick and dirty)."
		    );

if (branchLengths<0)
{
	return;
}

fileCount	= 2;
#include "TemplateModels/chooseGeneticCode.def";

ExecuteAFile 		    ("2RatesAnalyses/MG94xREVxBivariate.mdl");
fprintf 				(stdout, "\nReading input file 1/2\n");
SetDialogPrompt 		("Please specify codon data #1:");
DataSet					ds_1 			= ReadDataFile (PROMPT_FOR_FILE);
fprintf					(stdout, "\nData file 1: ", LAST_FILE_PATH, "\n\t", ds_1.species, " sequences and ", ds_1.sites, " nucleotides\n");
DataSetFilter 			filteredData_1 	= CreateFilter (ds_1,3,"","",GeneticCodeExclusions);
HarvestFrequencies		(observedFreq_1,filteredData_1,3,1,1);
HarvestFrequencies		(observedFreqSingle_1,filteredData_1,1,1,1);
done = 0;
while (!done)
{
	fprintf (stdout,"\nPlease enter a 6 character model designation (e.g:010010 defines HKY85):");
	fscanf  (stdin,"String", modelDesc);
	if (Abs(modelDesc)==6)
	{	
		done = 1;
	}
}		
ModelTitle 				= "MG94x"+modelDesc[0];
rateBiasTerms	 		= {{"AC_1","1","AT_1","CG_1","CT_1","GT_1"}};
DefineNucleotideBiases (0);
modelConstraintString_1 = modelConstraintString;
ModelTitle_1			= ModelTitle;	
fprintf					(stdout, "Using model ", ModelTitle_1, " on alignment 1\n\n");
#include				"queryTree.bf";
treeString_1			= treeString;			


fprintf 				(stdout, "\nReading input file 2/2\n");
SetDialogPrompt 		("Please specify codon data #2:");
DataSet					ds_2 			= ReadDataFile (PROMPT_FOR_FILE);
fprintf					(stdout, "\nData file 2: ", LAST_FILE_PATH, "\n\t", ds_2.species, " sequences and ", ds_2.sites, " nucleotides\n");
DataSetFilter 			filteredData_2 	= CreateFilter (ds_2,3,"","",GeneticCodeExclusions);
HarvestFrequencies		(observedFreq_2,filteredData_2,3,1,1);
HarvestFrequencies		(observedFreqSingle_2,filteredData_2,1,1,1);
done = 0;
while (!done)
{
	fprintf (stdout,"\nPlease enter a 6 character model designation (e.g:010010 defines HKY85):");
	fscanf  (stdin,"String", modelDesc);
	if (Abs(modelDesc)==6)
	{	
		done = 1;
	}
}		
ModelTitle 				= "MG94x"+modelDesc[0];
rateBiasTerms	 		= {{"AC_2","1","AT_2","CG_2","CT_2","GT_2"}};
DefineNucleotideBiases (0);
modelConstraintString_2 = modelConstraintString;
ModelTitle_2			= ModelTitle;	
fprintf					(stdout, "Using model ", ModelTitle_2, " on alignment 2\n\n");
#include				"queryTree.bf";
treeString_2			= treeString;			
						  
totalCodonCount			= filteredData_1.sites + filteredData_2.sites;

ChoiceList (randomizeInitValues, "Initial Value Options",1,SKIP_NONE,
			"Default",	 "Use default inital values for rate distribution parameters.",
			"Randomized",	 "Select initial values for rate distribution parameters at random.");


if (randomizeInitValues < 0)
{
	return;
}

stratDNDS = 1;
resp  	  = 4;
respM 	  = 2;
respN 	  = 1;
respP 	  = 1;

fprintf (stdout, "\nUsing\n\t", respM, " negatively selected classes\n\t", respN, " neutrally evolving classes\n\t", 
					respP, " positively selected classes\n\n");

categDef1 = "";
categDef1 * 1024;

lfDef	  = {};

for (fileID = 0; fileID < fileCount; fileID = fileID + 1)
{
	lfDef[fileID] = "";
	lfDef[fileID] * 1024;
	lfDef[fileID] * "Log(";
}

defineRateDistribution ("1_");
categDef_1  = categDef1;
freqStrMx_1 = freqStrMx;

for (mi=0; mi<resp; mi=mi+1)
{
	fileID = 0;
	if (mi)
	{
		lfDef[fileID] * "+";
	}
	lfDef[fileID]*(freqStrMx_1[mi]+"*SITE_LIKELIHOOD["+(fileID*resp+mi)+"]");		
}

defineRateDistribution ("2_");
categDef_2  = categDef1;
freqStrMx_2 = freqStrMx;

for (mi=0; mi<resp; mi=mi+1)
{
	fileID = 1;
	if (mi)
	{
		lfDef[fileID] * "+";
	}
	lfDef[fileID]*(freqStrMx_2[mi]+"*SITE_LIKELIHOOD["+(fileID*resp+mi)+"]");		
}

lfDef1 = "";
lfDef1 * 128;
lfDef1 * "\"";

for (fileID = 0; fileID < fileCount; fileID = fileID + 1)
{
	lfDef[fileID] * ")";
	lfDef[fileID] * 0;
	lfDef1 * lfDef[fileID];
	if (fileID < fileCount - 1)
	{
		lfDef1 * "+";
	}
}

lfDef1 	  * "\"";
lfDef1	  * 0;

ExecuteCommands (categDef_1);
ExecuteCommands (categDef_2);

ModelMatrixDimension = 64;
for (h = 0 ;h<64; h=h+1)
{
	if (_Genetic_Code[h]==10)
	{
		ModelMatrixDimension = ModelMatrixDimension-1;
	}
}

SetDialogPrompt ("Save resulting fits to:");
fprintf 		(PROMPT_FOR_FILE, CLEAR_FILE);
resToPath 		= LAST_FILE_PATH;

vectorOfFrequencies_1 = BuildCodonFrequencies (observedFreq_1);
vectorOfFrequencies_2 = BuildCodonFrequencies (observedFreq_2);

fprintf			(stdout, "Fitting a nucleotide model to approximate branch lengths and get starting nucleotide bias estimates...\n");

nucModelMatrix_1 			= {{*,AC_1*t,t,AT_1*t}{AC_1*t,*,CG_1*t,CT_1*t}{t,CG_1*t,*,GT_1*t}{AT_1*t,CT_1*t,GT_1*t,*}};
Model nucModel_1 			= (nucModelMatrix_1,observedFreqSingle_1);
DataSetFilter 	nucFilter_1 = CreateFilter (filteredData_1,1);
Tree  			nucTree_1 	= treeString_1;

nucModelMatrix_2 = {{*,AC_2*t,t,AT_2*t}{AC_2*t,*,CG_2*t,CT_2*t}{t,CG_2*t,*,GT_2*t}{AT_2*t,CT_2*t,GT_2*t,*}};
Model nucModel_2 = (nucModelMatrix_2,observedFreqSingle_2);
DataSetFilter 	nucFilter_2 = CreateFilter (filteredData_2,1);
Tree  			nucTree_2 	= treeString_2;

LikelihoodFunction nuc_lf = (nucFilter_1, nucTree_1, nucFilter_2, nucTree_2);
Optimize (nuc_res, nuc_lf);

global 	 codonFactor_1 = 0.33;
global 	 codonFactor_2 = 0.33;

lfParts	= "";
lfParts * 128;
lfParts * "LikelihoodFunction lf = (filteredData_1,tree_1_0";

for (fileID = 1; fileID <= fileCount; fileID = fileID + 1)
{
	rateMultipliers 	= {{""   ,"AC_"+fileID+"*",""   ,"AT_"+fileID+"*"}
						   {"AC_"+fileID+"*",""   ,"CG_"+fileID+"*","CT_"+fileID+"*"}
						   {""   ,"CG_"+fileID+"*",""   ,"GT_"+fileID+"*"}
						   {"AT_"+fileID+"*","CT_"+fileID+"*","GT_"+fileID+"*","" }
						  };

	for (part = 0; part < resp; part = part + 1)
	{
		ExecuteCommands ("PopulateModelMatrix(\"rate_matrix_"+fileID+"_"+part+"\",observedFreq_"+fileID+",\"S_"+fileID+"_"+part+"/c_"+fileID+"_scale\",\"NS_"+fileID+"_"+part+"/c_"+fileID+"_scale\");");
		ExecuteCommands ("Model MG94model_"+fileID+"_"+part+"= (rate_matrix_"+fileID+"_"+part+",vectorOfFrequencies_"+fileID+",0);");
		
		treeID = "tree_"+fileID+"_"+part;
		ExecuteCommands ("Tree "+treeID+"=treeString_" + fileID +";");
		if (branchLengths)
		{
			ExecuteCommands ("ReplicateConstraint (\"this1.?.synRate:=this2.?.t__/codonFactor_"+fileID+"\","+treeID+",nucTree_"+fileID+");");
		}
		else
		{
			if (part == 0)
			{	
				ExecuteCommands ("bnames = BranchName(nucTree_" + fileID + ",-1);");
				nlfDef = "";
				nlfDef * 128;
				for (lc = 0; lc < Columns (bnames); lc=lc+1)
				{
					nlfDef * (treeID + "." + bnames[lc] + ".synRate = nucTree_" + fileID + "." + bnames[lc] + ".t/codonFactor_"+fileID+";");
				}
				nlfDef * 0;
				ExecuteCommands (nlfDef);
			}
			else
			{
				ExecuteCommands ("ReplicateConstraint (\"this1.?.synRate:=this2.?.synRate\","+treeID+",tree_"+fileID+"_0);");
			}
		}
		if (part || fileID > 1)
		{
			lfParts = lfParts + ",filteredData_" + fileID + "," + treeID;
		}
	}
}

lfParts * 0;
ExecuteCommands (lfParts + "," + lfDef1 + ");");

sumPath = resToPath + ".summary";

treeBranchParameters = 0;
for (fileID = 1; fileID <= fileCount; fileID = fileID + 1)
{
	ExecuteCommands ("treeBranchParameters = treeBranchParameters + BranchCount(tree_" + fileID + "_0) + TipCount(tree_" + fileID + "_0);");
}	

/*-------------- INDEPENDENT DISTRIBUTIONS  ------------------*/

sop = Max(OPTIMIZATION_PRECISION,0.001);

fprintf (stdout, "Running simpler distribution approximations to ensure good convergence...\n");

OPTIMIZATION_PRECISION = 0.1;

P_1_1 := 0;P_1_2 := 0;P_1_3 := 0;
P_2_1 := 0;P_2_2 := 0;P_2_3 := 0;

S_1_0 := 1;S_1_1 := 1;S_1_2 := 1;S_1_3 := 1;
S_2_0 := 1;S_2_1 := 1;S_2_2 := 1;S_2_3 := 1;
R_1_0 := 1;R_1_1 := 1;R_1_2 := 1;
R_2_0 := 1;R_2_1 := 1;R_2_2 := 1;

Optimize (res,lf);
USE_LAST_RESULTS 		= 1;
SKIP_CONJUGATE_GRADIENT = 1;

d1 = ReportDistributionString(4,freqStrMx_1,"1_",1);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",1);

fprintf (stdout,  "\n*** Done with pass 1. Log(L) = ", Format(res[1][0],10,3), " *** \n");
fprintf (stdout,  "Approximate rates for data set 1:\n", d1);
fprintf (stdout,  "Approximate rates for data set 2:\n", d2);

codonFactor_1 := codonFactor_1__;
codonFactor_2 := codonFactor_2__;

AC_1  := AC_1__; AT_1  := AT_1__; CG_1  := CG_1__; CT_1  := CT_1__; GT_1  := GT_1__;
AC_2  := AC_2__; AT_2  := AT_2__; CG_2  := CG_2__; CT_2  := CT_2__; GT_2  := GT_2__;



fprintf (stdout, "\nGateaux sampling positively selected directions\n");

P_1_1 = 2/filteredData_1.sites;
S_1_3 = 1;

baseLineLL  = res[1][0];
LFCompute 	(lf,LF_START_COMPUTE);
bestDiff 	= -1e100;
bestAlpha 	= 1;
bestBeta  	= 1;
bestLL		= -1e100;

step  		= 0.1;
step2 		= 0.25;
v1 			= 0.05;

for (v1c = 0; v1c < 10; v1c = v1c + 1)
{
	v2 = v1+step; 
	for (v2c = 0; v2c < 20; v2c = v2c+1)
	{
		checkASample ("1_0");	
		v2 = v2 + step2;
	}
	v1 = v1+step;
} 

S_1_0 = bestAlpha;
R_1_0 = bestBeta;
bestAlpha 	= 1;
bestBeta  	= 1;

if (bestDiff <= 0)
{
	P_1_1 = 0;
}
saveBD = bestDiff;

P_2_1 = 1/filteredData_2.sites;
S_2_3 = 1;

v1 			= 0.05;
for (v1c = 0; v1c < 10; v1c = v1c + 1)
{
	v2 = v1+step; 
	for (v2c = 0; v2c < 20; v2c = v2c+1)
	{
		checkASample ("2_0");	
		v2 = v2 + step2;
	}
	v1 = v1+step;
} 

LFCompute (lf,LF_DONE_COMPUTE);

S_2_0 = bestAlpha;
R_2_0 = bestBeta;

if (bestDiff <= saveBD)
{
	P_2_1 = 0;
}

if (bestDiff > 0)
{
	fprintf (stdout, "\nFound a likelihood improvement in the direction (", S_1_0, ",", S_1_0*R_1_0, "), (", S_2_0, ",", S_2_0*R_2_0, "), ",bestDiff," likelihood points\n");
}


Optimize (res,lf);

d1 = ReportDistributionString(4,freqStrMx_1,"1_",1);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",1);

fprintf (stdout,  "\n*** Done with pass 2. Log(L) = ", Format(res[1][0],10,3), " *** \n");
fprintf (stdout,  "Approximate rates for data set 1:\n", d1);
fprintf (stdout,  "Approximate rates for data set 2:\n", d2);


fprintf (stdout, "\nGateaux sampling neutral directions\n");

baseLineLL  = res[1][0];
LFCompute 	(lf,LF_START_COMPUTE);

P_1_2 = 1/(filteredData_1.sites*(1-P_1_1));
bestDiff 	= -1e100;
bestAlpha 	= 1;
step = 0.02;
v1 = 0;
for (v1c = 0; v1c < 50; v1c = v1c + 1)
{
	v1 = v1 + step;
	v2 = v1;
	
	checkASample ("1_1");
}	


S_1_1 = bestAlpha;
bestAlpha 	= 1;

if (bestDiff <= 0)
{
	P_1_2 = 0;
}
saveBD = bestDiff;
R_1_1 := 1;

P_2_2 = 1/(filteredData_2.sites*(1-P_2_1));

v1 = 0;
for (v1c = 0; v1c < 50; v1c = v1c + 1)
{
	v1 = v1 + step;
	v2 = v1;
	
	checkASample ("2_1");
}	


LFCompute (lf,LF_DONE_COMPUTE);

S_2_1 = bestAlpha;
R_2_1 := 1;

if (bestDiff <= saveBD)
{
	P_2_2 = 0;
}

if (bestDiff > 0)
{
	fprintf (stdout, "\nFound a likelihood improvement in the direction (", S_1_1, ",", S_1_1, "), (", S_2_1, ",", S_2_1, "), ",bestDiff," likelihood points\n");
}

Optimize (res,lf);
d1 = ReportDistributionString(4,freqStrMx_1,"1_",1);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",1);

fprintf (stdout,  "\n*** Done with pass 3. Log(L) = ", Format(res[1][0],10,3), " *** \n");
fprintf (stdout,  "Approximate rates for data set 1:\n", d1);
fprintf (stdout,  "Approximate rates for data set 2:\n", d2);

fprintf (stdout, "\nGateaux sampling negative selected directions\n");


baseLineLL  = res[1][0];
LFCompute 	(lf,LF_START_COMPUTE);
bestDiff 	= -1e100;
bestAlpha 	= 1;
bestBeta  	= 1;


step = 1/16;
v1 = 0;
P_1_3 = 2/(filteredData_1.sites*(1-P_1_1)*(1-P_1_2));
S_1_2 = 1;
for (v1c = 0; v1c < 15; v1c = v1c + 1)
{
	v1 = v1+step;
	v2 = step/2; 
	for (v2c = 0; v2c < v1c; v2c = v2c+1)
	{
		checkASample ("1_2");	
		v2 = v2 + step;
	}
} 


S_1_2 = bestAlpha;
R_1_2 = bestBeta;
bestAlpha 	= 1;
bestBeta  	= 1;

if (bestDiff <= 0)
{
	P_1_3 = 0;
}
saveBD = bestDiff;


v1 = 0;
P_2_3 = 2/(filteredData_2.sites*(1-P_2_1)*(1-P_2_2));
S_2_2 = 1;
for (v1c = 0; v1c < 15; v1c = v1c + 1)
{
	v1 = v1+step;
	v2 = step/2; 
	for (v2c = 0; v2c < v1c; v2c = v2c+1)
	{
		checkASample ("2_2");	
		v2 = v2 + step;
	}
} 

LFCompute (lf,LF_DONE_COMPUTE);

S_2_2 = bestAlpha;
R_2_2 = bestBeta;

if (bestDiff <= saveBD)
{
	P_2_2 = 0;
}

if (bestDiff > 0)
{
	fprintf (stdout, "\nFound a likelihood improvement in the direction (", S_1_2, ",", S_1_2*R_1_2, "), (", S_2_2, ",", S_2_2*R_2_2, "), ",bestDiff," likelihood points\n");
}

AC_1  = AC_1; AT_1  = AT_1; CG_1  = CG_1; CT_1  = CT_1; GT_1  = GT_1;
AC_2  = AC_2; AT_2  = AT_2; CG_2  = CG_2; CT_2  = CT_2; GT_2  = GT_2;

if (Abs(modelConstraintString_1))
{
	ExecuteCommands (modelConstraintString_1);
}
if (Abs(modelConstraintString_2))
{
	ExecuteCommands (modelConstraintString_2);
}

codonFactor_1 = codonFactor_1;
codonFactor_2 = codonFactor_2;


GetString (paramList, lf, -1);
degF = Columns(paramList["Global Independent"]) + 14 - 2*branchLengths + treeBranchParameters; 

OPTIMIZATION_PRECISION = sop;
fprintf (stdout, "Running an independent distributions model fit (", degF, " parameters)...\n");
Optimize (res,lf);
LogL = res[1][0];

AIC	 = 2*(degF-LogL);
AICc = 2*(degF*totalCodonCount/(totalCodonCount-degF-1) - LogL);

fprintf (stdout, "\n\nIndependent distributions model fit summary\n", 
				 "\nLog likelihood:", Format (LogL, 15, 5),
				 "\nParameters    :", Format (degF, 15, 0),
				 "\nAIC           :", Format (AIC,  15, 5),
				 "\nc-AIC         :", Format (AICc,  15, 5),"\n"
);

fprintf (sumPath,CLEAR_FILE,"Independent distributions model fit summary\n", 
				 "\nLog likelihood:", Format (LogL, 15, 5),
				 "\nParameters    :", Format (degF, 15, 0),
				 "\nAIC           :", Format (AIC,  15, 5),
				 "\nc-AIC         :", Format (AICc,  15, 5),"\n");
				 
d1 = ReportDistributionString(4,freqStrMx_1,"1_",0);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",0);

fprintf (stdout,  "Inferred rates for data set 1:\n", d1);
fprintf (sumPath, "Inferred rates for data set 1:\n", d1);
fprintf (stdout,  "Inferred rates for data set 2:\n", d2);
fprintf (sumPath, "Inferred rates for data set 2:\n", d2);

LIKELIHOOD_FUNCTION_OUTPUT = 6;
fprintf (resToPath,CLEAR_FILE,lf);


/*-------------- SHARED POSITIVE SELECTION STRENGTHS  ------------------*/

for (k=0; k<respP; k=k+1)
{
	ExecuteCommands ("R_1_"+k+"=0.5(R_1_"+k+"+R_2_"+k+");");
	ExecuteCommands ("R_2_"+k+":=R_1_"+k+";");
}

GetString (paramList, lf, -1);
degFPSS = Columns(paramList["Global Independent"]) + 14 - 2*branchLengths + treeBranchParameters; 
	
fprintf (stdout, "\nRunning a shared positive selection strengths model (",degFPSS," parameters)...\n");
Optimize (res_PSS,lf);
LogLPSS = res_PSS[1][0];

AICPSS	 = 2*(degFPSS-LogLPSS);
AICcPSS    = 2*(degFPSS*totalCodonCount/(totalCodonCount-degFPSS-1) - LogLPSS);

fprintf (stdout, "\n\nShared positive selection strengths model fit summary\n", 
				 "\nLog likelihood:", Format (LogLPSS, 15, 5),
				 "\nParameters    :", Format (degFPSS, 15, 0),
				 "\nAIC           :", Format (AICPSS,  15, 5),
				 "\nc-AIC         :", Format (AICcPSS,  15, 5),"\n"
);

fprintf (sumPath, "\n\nShared positive strengths regime model fit summary\n", 
				 "\nLog likelihood:", Format (LogLPSS, 15, 5),
				 "\nParameters    :", Format (degFPSS, 15, 0),
				 "\nAIC           :", Format (AICPSS,  15, 5),
				 "\nc-AIC         :", Format (AICcPSS,  15, 5),"\n"
);

d1 = ReportDistributionString(4,freqStrMx_1,"1_",0);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",0);

fprintf (stdout,  "Inferred rates for data set 1:\n", d1);
fprintf (sumPath, "Inferred rates for data set 1:\n", d1);
fprintf (stdout,  "Inferred rates for data set 2:\n", d2);
fprintf (sumPath, "Inferred rates for data set 2:\n", d2);
fpath = resToPath + ".SharedStrength";
LIKELIHOOD_FUNCTION_OUTPUT = 6;
fprintf (fpath,CLEAR_FILE,lf);

/*-------------- SHARED POSITIVE SELECTION PROPORTIONS  ------------------*/

/* RELOAD INDEPENDENT FIT */
DeleteObject (lf);
ExecuteAFile (resToPath);

for (k=1; k<=respP; k=k+1)
{
	ExecuteCommands ("P_1_"+k+"=0.5(P_1_"+k+"+P_2_"+k+");");
	ExecuteCommands ("P_2_"+k+":=P_1_"+k+";");
}

GetString (paramList, lf, -1);
degFPSP = Columns(paramList["Global Independent"]) + 14 - 2*branchLengths + treeBranchParameters; 
fprintf (stdout, "\nRunning a shared positive selection proportions model (",degFPSP," parameters) ...\n");
Optimize (res_PSP,lf);
LogLPSP = res_PSP[1][0];


AICPSP	 = 2*(degFPSP-LogLPSP);
AICcPSP    = 2*(degFPSP*totalCodonCount/(totalCodonCount-degFPSP-1) - LogLPSP);

fprintf (stdout, "\n\nShared positive selection proportions model fit summary\n", 
				 "\nLog likelihood:", Format (LogLPSP, 15, 5),
				 "\nParameters    :", Format (degFPSP, 15, 0),
				 "\nAIC           :", Format (AICPSP,  15, 5),
				 "\nc-AIC         :", Format (AICcPSP,  15, 5),"\n"
);

fprintf (sumPath, "\n\nShared positive proportions regime model fit summary\n", 
				 "\nLog likelihood:", Format (LogLPSP, 15, 5),
				 "\nParameters    :", Format (degFPSP, 15, 0),
				 "\nAIC           :", Format (AICPSP,  15, 5),
				 "\nc-AIC         :", Format (AICcPSP,  15, 5),"\n"
);

d1 = ReportDistributionString(4,freqStrMx_1,"1_",0);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",0);

fprintf (stdout,  "Inferred rates for data set 1:\n", d1);
fprintf (sumPath,  "Inferred rates for data set 1:\n", d1);
fprintf (stdout,  "Inferred rates for data set 2:\n", d2);
fprintf (sumPath,  "Inferred rates for data set 2:\n", d2);
fpath = resToPath + ".SharedProportion";
LIKELIHOOD_FUNCTION_OUTPUT = 6;
fprintf (fpath,CLEAR_FILE,lf);


/*-------------- SHARED POSITIVE SELECTION REGIME  ------------------*/

/* RELOAD INDEPENDENT FIT */
DeleteObject (lf);
ExecuteAFile (resToPath);

for (k=0; k<respP; k=k+1)
{
	ExecuteCommands ("R_1_"+k+"=0.5(R_1_"+k+"+R_2_"+k+");");
	ExecuteCommands ("R_2_"+k+":=R_1_"+k+";");
}
for (k=1; k<=respP; k=k+1)
{
	ExecuteCommands ("P_1_"+k+"=0.5(P_1_"+k+"+P_2_"+k+");");
	ExecuteCommands ("P_2_"+k+":=P_1_"+k+";");
}

GetString (paramList, lf, -1);
degFPSH = Columns(paramList["Global Independent"]) + 14 - 2*branchLengths + treeBranchParameters; 

fprintf (stdout, "\nRunning a shared positive selection regime model (", degFPSH, " parameters)...\n");
Optimize (res_PSH,lf);
LogLPSH = res_PSH[1][0];

AICPSH	 = 2*(degFPSH-LogLPSH);
AICcPSH    = 2*(degFPSH*totalCodonCount/(totalCodonCount-degFPSH-1) - LogLPSH);

fprintf (stdout, "\n\nShared positive selection regime model fit summary\n", 
				 "\nLog likelihood:", Format (LogLPSH, 15, 5),
				 "\nParameters    :", Format (degFPSH, 15, 0),
				 "\nAIC           :", Format (AICPSH,  15, 5),
				 "\nc-AIC         :", Format (AICcPSH,  15, 5),"\n"
);

fprintf (sumPath, "\n\nShared positive selection regime model fit summary\n", 
				 "\nLog likelihood:", Format (LogLPSH, 15, 5),
				 "\nParameters    :", Format (degFPSH, 15, 0),
				 "\nAIC           :", Format (AICPSH,  15, 5),
				 "\nc-AIC         :", Format (AICcPSH,  15, 5),"\n"
);

d1 = ReportDistributionString(4,freqStrMx_1,"1_",0);
d2 = ReportDistributionString(4,freqStrMx_2,"2_",0);

fprintf (stdout,  "Inferred rates for data set 1:\n", d1);
fprintf (sumPath,  "Inferred rates for data set 1:\n", d1);
fprintf (stdout,  "Inferred rates for data set 2:\n", d2);
fprintf (sumPath,  "Inferred rates for data set 2:\n", d2);
fpath = resToPath + ".SharedPositiveSelection";
LIKELIHOOD_FUNCTION_OUTPUT = 6;
fprintf (fpath,CLEAR_FILE,lf);

/*-------------- SHARED MODEL ------------------*/

DeleteObject (lf);
ExecuteAFile (resToPath);

for (k=0; k<resp; k=k+1)
{
	ExecuteCommands ("S_1_"+k+"=0.5(S_1_"+k+"+S_2_"+k+");");
	if (k<respP || k>=respP+respN)
	{
		ExecuteCommands ("R_1_"+k+"=0.5(R_1_"+k+"+R_2_"+k+");");
	}
	ExecuteCommands ("S_2_"+k+":=S_1_"+k+";R_2_"+k+":=R_1_"+k+";");
}
for (k=1; k<=resp; k=k+1)
{
	ExecuteCommands ("P_1_"+k+"=0.5(P_1_"+k+"+P_2_"+k+");");
	ExecuteCommands ("P_2_"+k+":=P_1_"+k+";");
}

GetString (paramList, lf, -1);
degFJ = Columns(paramList["Global Independent"]) + 14 - 2*branchLengths + treeBranchParameters; 
fprintf (stdout, "\nRunning a shared distributions model fit (", degFJ, " parameters)...\n");

Optimize (res_J,lf);
LogLJ = res_J[1][0];

AICJ	 = 2*(degFJ-LogLJ);
AICcJ    = 2*(degFJ*totalCodonCount/(totalCodonCount-degFJ-1) - LogLJ);

fprintf (stdout, "\n\nShared distributions model fit summary\n", 
				 "\nLog likelihood:", Format (LogLJ, 15, 5),
				 "\nParameters    :", Format (degFJ, 15, 0),
				 "\nAIC           :", Format (AICJ,  15, 5),
				 "\nc-AIC         :", Format (AICcJ,  15, 5),"\n"
);

fprintf (sumPath, "\n\nShared distributions model fit summary\n", 
				 "\nLog likelihood:", Format (LogLJ, 15, 5),
				 "\nParameters    :", Format (degFJ, 15, 0),
				 "\nAIC           :", Format (AICJ,  15, 5),
				 "\nc-AIC         :", Format (AICcJ,  15, 5),"\n"
);

d1 = ReportDistributionString(4,freqStrMx_1,"1_",0);

fprintf (stdout,  "Inferred joint rates:\n", d1);
fprintf (sumPath, "Inferred joint rates:\n", d1);

fpath = resToPath + ".JointAll";
LIKELIHOOD_FUNCTION_OUTPUT = 6;
fprintf (fpath,CLEAR_FILE,lf);

USE_LAST_RESULTS 			= 0;
SKIP_CONJUGATE_GRADIENT 	= 0;
LIKELIHOOD_FUNCTION_OUTPUT	= 2;

fprintf (stdout, "\nDistribution comparison tests\n",
				 "\n\tAre the distributions different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLJ),10,3), 
				 	  " DF = ", degF-degFJ, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLJ), degF-degFJ),8,3), "\n");

fprintf (stdout, "\n\tAre selective regimes (dN/dS and proportions) different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLPSH),10,3), 
				 	  " DF = ", degF-degFPSH, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLPSH), degF-degFPSH),8,3), "\n");

fprintf (stdout, "\n\tAre selection strengths (dN/dS) different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLPSS),10,3), 
				 	  " DF = ", degF-degFPSS, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLPSS), degF-degFPSH),8,3), "\n");

fprintf (stdout, "\n\tAre the proportions of codons under selection different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLPSP),10,3), 
				 	  " DF = ", degF-degFPSP, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLPSP), degF-degFPSP),8,3), "\n");

fprintf (sumPath, "\n\nDistribution comparison tests\n",
				 "\n\tAre the distributions different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLJ),10,3), 
				 	  " DF = ", degF-degFJ, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLJ), degF-degFJ),8,3), "\n");

fprintf (sumPath, "\n\tAre selective regimes (dN/dS and proportions) different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLPSH),10,3), 
				 	  " DF = ", degF-degFPSH, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLPSH), degF-degFPSH),8,3), "\n");

fprintf (sumPath, "\n\tAre selection strengths (dN/dS) different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLPSS),10,3), 
				 	  " DF = ", degF-degFPSS, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLPSS), degF-degFPSH),8,3), "\n");

fprintf (sumPath, "\n\tAre the proportions of codons under selection different?",
				 "\n\t\tLR = ", Format (2*(LogL-LogLPSP),10,3), 
				 	  " DF = ", degF-degFPSP, 
				 	  " p = ", Format(1-CChi2(2*(LogL-LogLPSP), degF-degFPSP),8,3), "\n");
/*------------------------------------------------------------------------------------------------------------*/

function checkASample (whichRate)
{
	ExecuteCommands ("S_"+whichRate+"=v1;R_"+whichRate+"=v2/v1;");
	LFCompute (lf,res_n);
	localDiff = res_n-baseLineLL;
	if (localDiff > bestDiff)
	{
		bestDiff 	= localDiff;
		bestAlpha 	= v1;
		bestBeta  	= v2/v1;
	}
	return 0;
}
