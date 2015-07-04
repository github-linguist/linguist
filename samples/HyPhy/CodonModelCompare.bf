/* 
	This file takes a nucleotide data set and a tree (either from the data file or from a separate 	file) and computes maximum likelihood estimates for every possible 4x4 reversible model on that data and tree.

		We use the string (v1,v2,v3,v4,v5,v6), where and v1..6 = 0..5
   to encode a 4x4 symmetric transition matrix with entries
   [*  v1  v2  v3]
   [-  *   v4  v5]
   [-  -   *   v6]
   [-  -   -   * ]
   
   		For instance: (010010) encodes HKY85.
   		
   		For each model the following information is reported:
   		- Model string. (e.g. (012345) for the GRM)
   		- Number of model parameters
   		- Max ln-likelihood for the model
   		- Likelihood ratio statistic (as a sub-model of the GRM)
   		- AIC
   		- P-Value for the Likelihood Ratio Test.
   		
   
   Sergei L. Kosakovsky Pond, Summer 2002.
   
*/ 

function ReceiveJobs (sendOrNot)
{
	if (MPI_NODE_COUNT>1)
	{
		MPIReceive (-1, fromNode, result_String);
		jobModelNum = MPINodeState[fromNode-1][1];
		vv1 = MPINodeState[fromNode-1][2];
		vv2 = MPINodeState[fromNode-1][3];
		vv3 = MPINodeState[fromNode-1][4];
		vv4 = MPINodeState[fromNode-1][5];
		vv5 = MPINodeState[fromNode-1][6];
		vv6 = MPINodeState[fromNode-1][7];
		if (sendOrNot)
		{
			MPISend (fromNode,lf);
			MPINodeState[fromNode-1][1] = modelNum;		
			MPINodeState[fromNode-1][2] = v1;
			MPINodeState[fromNode-1][3] = v2;
			MPINodeState[fromNode-1][4] = v3;
			MPINodeState[fromNode-1][5] = v4;
			MPINodeState[fromNode-1][6] = v5;
			MPINodeState[fromNode-1][7] = v6;
		}
		else
		{
			MPINodeState[fromNode-1][0] = 0;
			MPINodeState[fromNode-1][1] = -1;		
		}
		
		ExecuteCommands (result_String);
	}
	else
	{
		jobModelNum = modelNum;
	}
	
	if (jobModelNum == 0)
	{
		stdl = lf_MLES[1][0];
		fullnp = lf_MLES[1][1]+addOn;
		fprintf(stdout,"\n(012345) Full Model ln-lik =  ",stdl,". Parameter Count=",Format(fullnp,0,0)," AIC = ", 2*(fullnp-stdl),"\n\n");


		resultCache [0][0] = 1;
		resultCache [0][1] = 2;
		resultCache [0][2] = 3;
		resultCache [0][3] = 4;
		resultCache [0][4] = 5;
		resultCache [0][5] = lf_MLES[1][0];
		resultCache [0][6] = lf_MLES[1][1]+addOn;
		resultCache [0][7] = 0;
		resultCache [0][8] = 0;
		
		fprintf (stdout,"\n#   |  Model   | # prm |    lnL    |      LRT       |    AIC     |   P-Value        |");   
		fprintf (stdout,"\n----|----------|-------|-----------|----------------|------------|------------------|");   

		if (MPI_NODE_COUNT>1)
		{
			for (h=1; h<203; h=h+1)
			{
				lnL = resultCache[h][5];
				
				if (lnL<0)
				{
					np = resultCache[h][6];
					LRT = -2*(lnL-stdl);
					if (LRT<0)
					{
						LRT = 0;
					}
					AIC = -2*lnL+2*np;
					PRINT_DIGITS = 3;
					fprintf (stdout,"\n",h);
					PRINT_DIGITS = 1;
					fprintf (stdout," | (",0, resultCache[h][0], resultCache[h][1], resultCache[h][2], resultCache[h][3], resultCache[h][4],") | ");
					fprintf (stdout,Format (np,5,0));
					PRINT_DIGITS = 8;
					fprintf (stdout, " |  ",lnL," | ",Format(LRT,14,3), " |  ", AIC, "  |  ", );
					
					PRINT_DIGITS = 15;
					if (LRT==0)
					{
						pValue = 1;					
					}
					else
					{
						pValue = 1-CChi2(LRT,fullnp-np);
					}
					fprintf (stdout,pValue," |");
					resultCache [jobModelNum][7] = pValue;
					if (pValue<rejectAt)
					{
						rejectCount = rejectCount+1;
						resultCache [jobModelNum][8] = 0;
					}
					else
					{
						resultCache [jobModelNum][8] = 1;					
					}
					
					if (pValue<rejectAt)
					{
						fprintf (stdout,"(*)");
					}				
				}
			}
		}
		
		return fromNode-1;
	}
	else
	{
		if ((MPI_NODE_COUNT>1)&&(resultCache[0][5]>=0))
		{
			resultCache [jobModelNum][0] = vv2;
			resultCache [jobModelNum][1] = vv3;
			resultCache [jobModelNum][2] = vv4;
			resultCache [jobModelNum][3] = vv5;
			resultCache [jobModelNum][4] = vv6;
			resultCache [jobModelNum][5] = lf_MLES[1][0];
			resultCache [jobModelNum][6] = lf_MLES[1][1]+addOn;
			return fromNode - 1;
		}
	}

	np = lf_MLES[1][1]+addOn;
	lnL = lf_MLES[1][0];
	LRT = -2*(lnL-stdl);
	if (LRT<0)
	{
		LRT = 0;
	}
	AIC = -2*lnL+2*np;
	PRINT_DIGITS = 3;
	fprintf (stdout,"\n",jobModelNum);
	PRINT_DIGITS = 1;
	fprintf (stdout," | (",vv1,vv2,vv3,vv4,vv5,vv6,") | ");
	fprintf (stdout,Format (np,5,0));
	PRINT_DIGITS = 8;
	fprintf (stdout, " |  ",lnL," | ",Format(LRT,14,3), " |  ", AIC, "  |  ", );
	
	
	PRINT_DIGITS = 15;
	if (LRT==0)
	{
		pValue = 1;					
	}
	else
	{
		pValue = 1-CChi2(LRT,fullnp-np);
	}
	fprintf (stdout,pValue," |");
	
	resultCache [jobModelNum][0] = vv2;
	resultCache [jobModelNum][1] = vv3;
	resultCache [jobModelNum][2] = vv4;
	resultCache [jobModelNum][3] = vv5;
	resultCache [jobModelNum][4] = vv6;
	resultCache [jobModelNum][5] = lf_MLES[1][0];
	resultCache [jobModelNum][6] = lf_MLES[1][1]+addOn;
	resultCache [jobModelNum][7] = pValue;
	if (pValue<rejectAt)
	{
		rejectCount = rejectCount+1;
		resultCache [jobModelNum][8] = 0;
	}
	else
	{
		resultCache [jobModelNum][8] = 1;					
	}
	
	if (pValue<rejectAt)
	{
		fprintf (stdout,"(*)");
	}
			
	return fromNode-1;
}

function PopulateModelMatrix (ModelMatrixName&, EFV)
{
	if (!ModelMatrixDimension)
	{
		ModelMatrixDimension = 64;
		for (h = 0 ;h<64; h=h+1)
		{
			if (_Genetic_Code[h]==10)
			{
				ModelMatrixDimension = ModelMatrixDimension-1;
			}
		}
	}
	
	ModelMatrixName = {ModelMatrixDimension,ModelMatrixDimension}; 

	hshift = 0;

	if (modelType == 0)
	{
		for (h=0; h<64; h=h+1)
		{
			if (_Genetic_Code[h]==10) 
			{
				hshift = hshift+1;
				continue; 
			}
			vshift = hshift;
			for (v = h+1; v<64; v=v+1)
			{
				diff = v-h;
				if (_Genetic_Code[v]==10) 
				{
					vshift = vshift+1;
					continue; 
				}
				nucPosInCodon = 2;
			  	if ((h$4==v$4)||((diff%4==0)&&(h$16==v$16))||(diff%16==0))
			  	{
			  		if (h$4==v$4)
			  		{
			  			transition = v%4;
			  			transition2= h%4;
			  		}
			  		else
			  		{
			  			if(diff%16==0)
			  			{
			  				transition = v$16;
			  				transition2= h$16;
							nucPosInCodon = 0;
			  			}
			  			else
			  			{
			  				transition = v%16$4;
			  				transition2= h%16$4;
							nucPosInCodon = 1;
			  			}
			  		}
			  		
			  		rateType = mSpecMatrix[transition][transition2];
			  		
			  		if (rateType == 1)
			  		{
				  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
				  		{
				  			ModelMatrixName[h-hshift][v-vshift] := AC*synRate*EFV__[transition__][nucPosInCodon__];
				  			ModelMatrixName[v-vshift][h-hshift] := AC*synRate*EFV__[transition2__][nucPosInCodon__];
					  	}
				  		else
				  		{
					  		ModelMatrixName[h-hshift][v-vshift] := AC*R*synRate*EFV__[transition__][nucPosInCodon__];
				  			ModelMatrixName[v-vshift][h-hshift] := AC*R*synRate*EFV__[transition2__][nucPosInCodon__];
			  			}
			  		}
			  		else
			  		{
				  		if (rateType == 2)
				  		{
					  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
					  		{
					  			ModelMatrixName[h-hshift][v-vshift] := synRate*EFV__[transition__][nucPosInCodon__];
					  			ModelMatrixName[v-vshift][h-hshift] := synRate*EFV__[transition2__][nucPosInCodon__];
						  	}
					  		else
					  		{
						  		ModelMatrixName[h-hshift][v-vshift] := R*synRate*EFV__[transition__][nucPosInCodon__];
					  			ModelMatrixName[v-vshift][h-hshift] := R*synRate*EFV__[transition2__][nucPosInCodon__];
				  			}
				  		}
				  		else
				  		{
					  		if (rateType == 3)
					  		{
						  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
						  		{
						  			ModelMatrixName[h-hshift][v-vshift] := AT*synRate*EFV__[transition__][nucPosInCodon__];
						  			ModelMatrixName[v-vshift][h-hshift] := AT*synRate*EFV__[transition2__][nucPosInCodon__];
							  	}
						  		else
						  		{
							  		ModelMatrixName[h-hshift][v-vshift] := AT*R*synRate*EFV__[transition__][nucPosInCodon__];
						  			ModelMatrixName[v-vshift][h-hshift] := AT*R*synRate*EFV__[transition2__][nucPosInCodon__];
					  			}
					  		}
					  		else
					  		{
						  		if (rateType == 4)
						  		{
							  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
							  		{
							  			ModelMatrixName[h-hshift][v-vshift] := CG*synRate*EFV__[transition__][nucPosInCodon__];
							  			ModelMatrixName[v-vshift][h-hshift] := CG*synRate*EFV__[transition2__][nucPosInCodon__];
								  	}
							  		else
							  		{
								  		ModelMatrixName[h-hshift][v-vshift] := CG*R*synRate*EFV__[transition__][nucPosInCodon__];
							  			ModelMatrixName[v-vshift][h-hshift] := CG*R*synRate*EFV__[transition2__][nucPosInCodon__];
						  			}
						  		}
						  		else
						  		{
							  		if (rateType == 5)
							  		{
								  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
								  		{
								  			ModelMatrixName[h-hshift][v-vshift] := CT*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := CT*synRate*EFV__[transition2__][nucPosInCodon__];
									  	}
								  		else
								  		{
									  		ModelMatrixName[h-hshift][v-vshift] := CT*R*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := CT*R*synRate*EFV__[transition2__][nucPosInCodon__];
							  			}
							  		}
							  		else
							  		{
								  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
								  		{
								  			ModelMatrixName[h-hshift][v-vshift] := GT*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := GT*synRate*EFV__[transition2__][nucPosInCodon__];
									  	}
								  		else
								  		{
									  		ModelMatrixName[h-hshift][v-vshift] := GT*R*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := GT*R*synRate*EFV__[transition2__][nucPosInCodon__];
							  			}
							  		}
							  	}
						  	}
					  	}
				  	}
			  	}
			}
		}	
	}
	else
	{
		for (h=0; h<64; h=h+1)
		{
			if (_Genetic_Code[h]==10) 
			{
				hshift = hshift+1;
				continue; 
			}
			vshift = hshift;
			for (v = h+1; v<64; v=v+1)
			{
				diff = v-h;
				if (_Genetic_Code[v]==10) 
				{
					vshift = vshift+1;
					continue; 
				}
				nucPosInCodon = 2;
			  	if ((h$4==v$4)||((diff%4==0)&&(h$16==v$16))||(diff%16==0))
			  	{
			  		if (h$4==v$4)
			  		{
			  			transition = v%4;
			  			transition2= h%4;
			  		}
			  		else
			  		{
			  			if(diff%16==0)
			  			{
			  				transition = v$16;
			  				transition2= h$16;
							nucPosInCodon = 0;
			  			}
			  			else
			  			{
			  				transition = v%16$4;
			  				transition2= h%16$4;
							nucPosInCodon = 1;
			  			}
			  		}
			  		
			  		rateType = mSpecMatrix[transition][transition2];
			  		
					if (rateType == 1)
			  		{
				  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
				  		{
				  			ModelMatrixName[h-hshift][v-vshift] := c*AC*synRate*EFV__[transition__][nucPosInCodon__];
				  			ModelMatrixName[v-vshift][h-hshift] := c*AC*synRate*EFV__[transition2__][nucPosInCodon__];
					  	}
				  		else
				  		{
					  		ModelMatrixName[h-hshift][v-vshift] := c*AC*R*synRate*EFV__[transition__][nucPosInCodon__];
				  			ModelMatrixName[v-vshift][h-hshift] := c*AC*R*synRate*EFV__[transition2__][nucPosInCodon__];
			  			}
			  		}
			  		else
			  		{
				  		if (rateType == 2)
				  		{
					  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
					  		{
					  			ModelMatrixName[h-hshift][v-vshift] := c*synRate*EFV__[transition__][nucPosInCodon__];
					  			ModelMatrixName[v-vshift][h-hshift] := c*synRate*EFV__[transition2__][nucPosInCodon__];
						  	}
					  		else
					  		{
						  		ModelMatrixName[h-hshift][v-vshift] := c*R*synRate*EFV__[transition__][nucPosInCodon__];
					  			ModelMatrixName[v-vshift][h-hshift] := c*R*synRate*EFV__[transition2__][nucPosInCodon__];
				  			}
				  		}
				  		else
				  		{
					  		if (rateType == 3)
					  		{
						  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
						  		{
						  			ModelMatrixName[h-hshift][v-vshift] := c*AT*synRate*EFV__[transition__][nucPosInCodon__];
						  			ModelMatrixName[v-vshift][h-hshift] := c*AT*synRate*EFV__[transition2__][nucPosInCodon__];
							  	}
						  		else
						  		{
							  		ModelMatrixName[h-hshift][v-vshift] := c*AT*R*synRate*EFV__[transition__][nucPosInCodon__];
						  			ModelMatrixName[v-vshift][h-hshift] := c*AT*R*synRate*EFV__[transition2__][nucPosInCodon__];
					  			}
					  		}
					  		else
					  		{
						  		if (rateType == 4)
						  		{
							  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
							  		{
							  			ModelMatrixName[h-hshift][v-vshift] := c*CG*synRate*EFV__[transition__][nucPosInCodon__];
							  			ModelMatrixName[v-vshift][h-hshift] := c*CG*synRate*EFV__[transition2__][nucPosInCodon__];
								  	}
							  		else
							  		{
								  		ModelMatrixName[h-hshift][v-vshift] := c*CG*R*synRate*EFV__[transition__][nucPosInCodon__];
							  			ModelMatrixName[v-vshift][h-hshift] := c*CG*R*synRate*EFV__[transition2__][nucPosInCodon__];
						  			}
						  		}
						  		else
						  		{
							  		if (rateType == 5)
							  		{
								  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
								  		{
								  			ModelMatrixName[h-hshift][v-vshift] := c*CT*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := c*CT*synRate*EFV__[transition2__][nucPosInCodon__];
									  	}
								  		else
								  		{
									  		ModelMatrixName[h-hshift][v-vshift] := c*CT*R*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := c*CT*R*synRate*EFV__[transition2__][nucPosInCodon__];
							  			}
							  		}
							  		else
							  		{
								  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
								  		{
								  			ModelMatrixName[h-hshift][v-vshift] := c*GT*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := c*GT*synRate*EFV__[transition2__][nucPosInCodon__];
									  	}
								  		else
								  		{
									  		ModelMatrixName[h-hshift][v-vshift] := c*GT*R*synRate*EFV__[transition__][nucPosInCodon__];
								  			ModelMatrixName[v-vshift][h-hshift] := c*GT*R*synRate*EFV__[transition2__][nucPosInCodon__];
							  			}
							  		}
							  	}
						  	}
					  	}
				  	}				  	
				  }
			}
		}	
	}	
	return 0;
}


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

function  setElement (h,v,cc)
{	
	mSpecMatrix[h][v]=cc+1;
	mSpecMatrix[v][h]=cc+1;
	return 1;
}


function printModelMatrix (modelString)
{
	
	mstrConv = "1";
	for (v2 = 1; v2 < 6; v2 = v2+1)
	{
		if (modelString[v2]=="0")
		{
			mstrConv = mstrConv+"1";
		}
		else
		{
			if (modelString[v2]=="1")
			{
				mstrConv = mstrConv+"B";
			}
			else
			{
				if (modelString[v2]=="2")
				{
					mstrConv = mstrConv+"C";
				}
				else
				{
					if (modelString[v2]=="3")
					{
						mstrConv = mstrConv+"D";
					}
					else
					{
						if (modelString[v2]=="4")
						{
							mstrConv = mstrConv+"E";
						}
						else
						{
							mstrConv = mstrConv+"F";
						}
					}
				}
			}
		}
	}
	sep = "+---+-----+-----+-----+-----+\n";
	fprintf (stdout, sep,
					 "|   |  A  |  C  |  G  |  T  |\n",
					 sep,
					 "| A |  *  | ", mstrConv[0], "*t | ", mstrConv[1], "*t | ", mstrConv[2], "*t |\n",
					 sep,
					 "| C | ", mstrConv[0], "*t |  *  | ", mstrConv[3], "*t | ", mstrConv[4], "*t |\n",
					 sep,
					 "| G | ", mstrConv[1], "*t | " , mstrConv[3], "*t |  *  | ", mstrConv[5], "*t |\n",
					 sep,
					 "| T | ", mstrConv[2], "*t | " , mstrConv[4], "*t | ", mstrConv[5], "*t |  *  |\n",
					 sep, "\nt = synRate for synonymous substitutions, and t=R*synRate for non-synonumous ones.\n");
						 
	return 1;
}

#include 				   "TemplateModels/chooseGeneticCode.def";
SetDialogPrompt 		   ("Please specify a codon data file:");

DataSet 				   ds 			= ReadDataFile (PROMPT_FOR_FILE);
DataSetFilter 			   filteredData = CreateFilter (ds,3,"","",GeneticCodeExclusions);

fprintf 				   (stdout,"\n______________READ THE FOLLOWING DATA______________\n",ds);


HarvestFrequencies         (observedFreq,filteredData,3,1,1);

mSpecMatrix 			   = {{*,1,1,1}{1,*,1,1}{1,1,*,1}{1,1,1,*}};
ModelMatrixDimension       = 0;

_DO_TREE_REBALANCE_ = 1;

#include "queryTree.bf";
#include "TemplateModels/modelParameters5.mdl";

if (modelType > 0)
{
	#include "TemplateModels/defineGamma.mdl";
}

ChoiceList (branchLengths,"Estimate Branch Lengths",1,SKIP_NONE,
			"Every Time","Branch lengths are reestimated for every model.",
			"Once","Branch lenghts obtained from the nucleotide GTR model and reused for subsequent models."
	       );

if (branchLengths<0)
{
	return;
}

rejectAt = 0;

while ((rejectAt<=0)||(rejectAt>=1))
{
	fprintf (stdout, "\nModel rejection level (e.g. 0.05):");
	fscanf  (stdin,"Number", rejectAt);
}

SetDialogPrompt ("Save results to:"); 
 
fprintf (PROMPT_FOR_FILE, CLEAR_FILE); 
BASE_PATH = LAST_FILE_PATH; 

KEEP_OPTIMAL_ORDER = 1;
MESSAGE_LOGGING = 0;

global AC=1;
global AT=1;
global CG=1;
global CT=1;
global GT=1;
global R=1;


r = setElement (0,2,1);
r = setElement (0,3,2);
r = setElement (1,2,3);
r = setElement (1,3,4);
r = setElement (2,3,5);

MG94custom = 0;
MULTIPLY_BY_FREQS     = PopulateModelMatrix ("MG94custom", observedFreq);
vectorOfFrequencies   = BuildCodonFrequencies (observedFreq);
Model MG94customModel = (MG94custom,vectorOfFrequencies,0);

USE_POSITION_SPECIFIC_FREQS = 1;
Tree tr = treeString;

addOn = 0;

if (branchLengths)
{
	global TreeScaler = 1;
	GTRMatrix = {{*,AC*nt,nt,AT*nt}{AC*nt,*,CG*nt,CT*nt}{nt,CG*nt,*,GT*nt}{AT*nt,CT*nt,GT*nt,*}};
	DataSetFilter 	nucFilter = CreateFilter (filteredData,1);
	HarvestFrequencies         (nucFreq,nucFilter,1,1,1);
	Model GTRModel = (GTRMatrix,nucFreq,1);
	givenTreeString = Format (tr,0,0);
	Tree nucTree = givenTreeString;
	LikelihoodFunction lfn = (nucFilter, nucTree);
	Optimize (nres,lfn);
	ReplicateConstraint ("this1.?.synRate:=this2.?.nt__/TreeScaler",tr,nucTree);
	addOn = nres[1][1]-nres[1][2]-1;
}

LikelihoodFunction lf = (filteredData, tr);

resultCache = {203,9};

modelNum	= 0;
rejectCount = 0;

if (MPI_NODE_COUNT>1)
{
	MPINodeState = {MPI_NODE_COUNT-1,8};
	OPTIMIZE_SUMMATION_ORDER = 0;
	MPISend (1,lf);
	MPINodeState[0][0] = 1;
	MPINodeState[0][1] = modelNum;
}
else
{
	Optimize (lf_MLES,lf);
	vv1 = 0;
	vv2 = 0;
	vv3 = 0;
	vv4 = 0;
	vv5 = 0;
	vv6 = 0;
	dummy = ReceiveJobs (0);
}

rateBiasTerms = {{"AC","1","AT","CG","CT","GT"}};

for (v2=0; v2<=1; v2=v2+1)
{
	for (v3=0; v3<=v2+1; v3=v3+1)
	{
		if (v3>v2)
		{
			ub4 = v3;
		}
		else
		{
			ub4 = v2;
		}
		for (v4=0; v4<=ub4+1; v4=v4+1)
		{
			if (v4>=ub4)
			{
				ub5 = v4;
			}
			else
			{
				ub5 = ub4;
			}
			for (v5=0; v5<=ub5+1; v5=v5+1)
			{
				if (v5>ub5)
				{
					ub6 = v5;
				}
				else
				{
					ub6 = ub5;
				}
				for (v6=0; v6<=ub6+1; v6=v6+1)
				{
					if (v6==5)
					{
						break;
					}

					R = 1;

					paramCount	  = 0;

					modelDesc = "0"+Format(v2,1,0);
					modelDesc = modelDesc+Format(v3,1,0);
					modelDesc = modelDesc+Format(v4,1,0);
					modelDesc = modelDesc+Format(v5,1,0);
					modelDesc = modelDesc+Format(v6,1,0);
					
					modelConstraintString = "";
					
					AC = 1;
					AT = 1;
					CG = 1;
					CT = 1;
					GT = 1;

					for (customLoopCounter2=1; customLoopCounter2<6; customLoopCounter2=customLoopCounter2+1)
					{
						for (customLoopCounter=0; customLoopCounter<customLoopCounter2; customLoopCounter=customLoopCounter+1)
						{
							if (modelDesc[customLoopCounter2]==modelDesc[customLoopCounter])
							{
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
					}	

					if (Abs(modelConstraintString))
					{
						ExecuteCommands (modelConstraintString);
					}					
										

					modelNum = modelNum+1;
					if (MPI_NODE_COUNT>1)
					{
						for (mpiNode = 0; mpiNode < MPI_NODE_COUNT-1; mpiNode = mpiNode+1)
						{
							if (MPINodeState[mpiNode][0]==0)
							{
								break;	
							}
						}
						
						if (mpiNode==MPI_NODE_COUNT-1)
						/* all nodes busy */
						{
							mpiNode = ReceiveJobs (1);
						}
						else
						{
							MPISend (mpiNode+1,lf);
							MPINodeState[mpiNode][0] = 1;
							MPINodeState[mpiNode][1] = modelNum;
							MPINodeState[mpiNode][2] = v1;
							MPINodeState[mpiNode][3] = v2;
							MPINodeState[mpiNode][4] = v3;
							MPINodeState[mpiNode][5] = v4;
							MPINodeState[mpiNode][6] = v5;
							MPINodeState[mpiNode][7] = v6;
						}
					}
					else
					{
						Optimize (lf_MLES,lf);
						vv1 = v1;
						vv2 = v2;
						vv3 = v3;
						vv4 = v4;
						vv5 = v5;
						vv6 = v6;
						dummy = ReceiveJobs (0);
					}
				}
			}
		}
	}

}

if (MPI_NODE_COUNT>1)
{
	while (1)
	{
		for (nodeCounter = 0; nodeCounter < MPI_NODE_COUNT-1; nodeCounter = nodeCounter+1)
		{
			if (MPINodeState[nodeCounter][0]==1)
			{
				fromNode = ReceiveJobs (0);
				break;	
			}
		}
		if (nodeCounter == MPI_NODE_COUNT-1)
		{
			break;
		}
	}	
	OPTIMIZE_SUMMATION_ORDER = 1;
}

function checkEmbedding (_m1, _m2)
{
	for (r=0; r<6; r=r+1)
	{
		if (_m2[r]<_m1[r])
		{
			/*fprintf (stdout,_m1," ", _m2, " Reject 1 at position ",r,"\n");*/
			return 0;
		}
		if (_m2[r]>_m1[r])
		{
			for (r2 = 0; r2 < 6; r2 = r2+1)
			{
				if ((_m2[r2]==_m2[r])&&(_m1[r2]!=_m1[r]))
				{
					/*fprintf (stdout,_m1," ", _m2, " Reject 2 at positions ",r,r2,"\n");*/
					return 0;
				}
			}
		}
	}
	return 1;
}

PRINT_DIGITS = 0;

fprintf (stdout, "\n\n--------------------------\n   (*) => p-Value < ", rejectAt, "\nRejected ", rejectCount, " models.\n");


if (rejectCount<202)
{

	fprintf (stdout, "\nPerforming nested tests on the remaining models...\n");

	done = 0;
	while (!done)
	{
		done = 1;
		for (v2=1; v2<203; v2=v2+1)
		{
			if (resultCache[v2][8])
			{
				modelString = "0";
				for (v3 = 0; v3<5; v3=v3+1)
				{
					modelString = modelString + resultCache [v2][v3];
				}
				for (v3 = v2+1; v3<203; v3 = v3+1)
				{
					if (resultCache[v3][8])
					{
						modelString2 = "0";
						for (v4 = 0; v4<5; v4=v4+1)
						{
							modelString2 = modelString2 + resultCache [v3][v4];
						}	
						if (checkEmbedding (modelString, modelString2))
						{
							fprintf (stdout,"H: (", modelString,") A: (", modelString2, "). ");
							done = 0;
							LRT = 2*(resultCache[v3][5]-resultCache[v2][5]);
							npd = resultCache[v3][6]-resultCache[v2][6];
							if (LRT<0)
							{
								pValue = 1;
							}
							else
							{
								pValue = 1-CChi2(LRT,npd);
							}
							fprintf (stdout," P-Value=", Format (pValue,10,3));
							if (pValue<rejectAt)
							{
								fprintf (stdout,". Rejected H.\n");
								resultCache[v2][8] = 0;
								break;
							}
							else
							{
								fprintf (stdout,". Failed to reject H. Discarding A.\n");
								resultCache[v3][8] = 0;
							}
						}
					}
				}
			}
		}
	}

	fprintf (stdout,"\n\nRemaining models:\n\n#   |  Model   | # prm |    lnL    |      LRT       |    AIC     |   P-Value        |");   
	fprintf (stdout,"\n----|----------|-------|-----------|----------------|------------|------------------|"); 
	fprintf (BASE_PATH,"\n\nRemaining models:\n\n#   |  Model   | # prm |    lnL    |      LRT       |    AIC     |   P-Value        |");   
	fprintf (BASE_PATH,"\n----|----------|-------|-----------|----------------|------------|------------------|"); 
	
	modelNum = 0;  
	v5 = 1e10;
	v4 = 0;

	for (v2=1; v2<203; v2=v2+1)
	{
		if (resultCache[v2][8])
		{
			modelNum = 0;
			modelString = "0";
			for (v3 = 0; v3<5; v3=v3+1)
			{
				modelString = modelString + resultCache [v2][v3];
			}
			np  = resultCache[v2][6];
			lnL = resultCache[v2][5];
			LRT = -2*(lnL-stdl);
			if (LRT<0)
			{
				LRT = 0;
			}
			AIC = -2*lnL+2*np;
			modelNum = modelNum + 1;
			PRINT_DIGITS = 3;
			fprintf (stdout,"\n",v2);
			fprintf (BASE_PATH,"\n",v2);
			PRINT_DIGITS = 1;
			fprintf (stdout," | (",0,resultCache[v2][0],resultCache[v2][1],resultCache[v2][2],resultCache[v2][3],resultCache[v2][4],") | ");
			fprintf (stdout,Format (np,5,0));
			fprintf (BASE_PATH," | (",0,resultCache[v2][0],resultCache[v2][1],resultCache[v2][2],resultCache[v2][3],resultCache[v2][4],") | ");
			fprintf (BASE_PATH,Format (np,5,0));
			PRINT_DIGITS = 8;
			fprintf (stdout, " |  ",lnL," | ",Format(LRT,14,3), " |  ", AIC, "  |  ", );
			fprintf (BASE_PATH, " |  ",lnL," | ",Format(LRT,14,3), " |  ", AIC, "  |  ", );
			PRINT_DIGITS = 15;
			if (LRT==0)
			{
				pValue = 1;					
			}
			else
			{
				pValue = 1-CChi2(LRT,fullnp-np);
			}
			if (AIC<v5)
			{
				v5 = AIC;
				v4 = v2;
			}
			fprintf (stdout,pValue," |");
			fprintf (BASE_PATH,pValue," |");
			
		}
	}
	
	PRINT_DIGITS = 0;
	modelString = "0";
	for (v3 = 0; v3<5; v3=v3+1)
	{
		modelString = modelString + Format(resultCache [v4][v3],0,0);
	}
	
	fprintf (stdout, "\n\nAIC based winner: (", modelString, ") with AIC = ", v5, "\n\n");
	fprintf (BASE_PATH, "\n\nAIC based winner: (", modelString, ") with AIC = ", v5, "\n\n");
	
	dummy = printModelMatrix (modelString);
	
	modelString2 = "";
	if (modelString == "000000")
	{
		modelString2 = "F81";
	}
	if (modelString == "010010")
	{
		modelString2 = "HKY85";
	}
	if (modelString == "010020")
	{
		modelString2 = "TrN";
	}
	if (Abs(modelString2))
	{
		fprintf (stdout, "\nThis model is better known as:", modelString2, "\n");
		fprintf (BASE_PATH, "\nThis model is better known as:", modelString2, "\n");
	}

}
else
{
	fprintf (stdout, "\nGeneral Reversible Model is the winner!\n");
	fprintf (BASE_PATH, "\nGeneral Reversible Model is the winner!\n");
}



