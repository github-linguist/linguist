ModelNames = {{"Neutral",
	  		  "Selection",
			  "Discrete",
			  "Freqs",
			  "Gamma",
			  "2 Gamma",
			  "Beta",
			  "Beta & w",
			  "Beta & Gamma",
			  "Beta & (Gamma+1)",
			  "Beta & (Normal>1)",
			  "0 & 2 (Normal>1)",
			  "3 Normal",
			  "RE: Lognormal",
			  "RE: Gamma",
			  "RE: Discrete"}};
			  
ParameterCount = {{0,
	  		  	   1,
			  	   3,
			  	   4,
			  	   2,
			  	   4,
			       2,
			  	   4,
			  	   5,
			       5,
			  	   5,
			       5,
			       6,
			       1,
			       2,
			       4
			       }};
			       
MAXIMUM_ITERATIONS_PER_VARIABLE = 2000;
OPTIMIZATION_PRECISION = 0.001;
			  
function SetWDistribution (resp)
{
	if (rateType == 0)
	{
		global P = .5;
		P:<1;
		categFreqMatrix = {{P,1-P}};
		categRateMatrix = {{0,1}};
		category c = (2, categFreqMatrix , MEAN, ,categRateMatrix, 0, 1e25);
	}
	else
	{
		if (rateType == 1)
		{
			global P1 = 1/3;
			global P2 = 0;
			
			P1:<1;
			P2:<1;
			
			global W = 1;
			categFreqMatrix = {{P1,(1-P1)*P2, (1-P1)*(1-P2)}} ;
			categRateMatrix = {{0,1,W}};
			category c = (3, categFreqMatrix , MEAN, ,categRateMatrix, 0, 1e25);
		}		
		else
		{
			if (rateType == 2)
			{
				global P1 = 1/3;
				global P2 = .5;
				P1:<1;
				P2:<1;
				global W1 = .25;
				global R1 = 4;
				global R2 = 3;
				R1:>1;
				R2:>1;
				categFreqMatrix = {{P1,(1-P1)*P2, (1-P1)*(1-P2)}} ;
				categRateMatrix = {{W1,W1*R1,W1*R1*R2}};
				category c = (3, categFreqMatrix , MEAN, ,categRateMatrix, 0, 1e25);				
			}
			else
			{
				if (rateType == 3)
				{
					global P1 = 1/5;
					global P2 = 1/4;
					global P3 = 1/3;
					global P4 = 1/2;
					
					P1:<1;
					P2:<1;
					P3:<1;
					P4:<1;
					
					categFreqMatrix = {{P1,
										(1-P1)P2,
										(1-P1)(1-P2)*P3,
										(1-P1)(1-P2)(1-P3)P4,
										(1-P1)(1-P2)(1-P3)(1-P4)}} ;
					categRateMatrix = {{0,1/3,2/3,1,3}};
					category c = (5, categFreqMatrix , MEAN, ,categRateMatrix, 0, 1e25);				
				}
				else
				{
					if (rateType == 4)
					{
						global alpha = .5;
						global beta = 1;
						alpha:>0.01;alpha:<100;
						beta:>0.01;
						beta:<200;
						category c = (resp, EQUAL, MEAN, GammaDist(_x_,alpha,beta), CGammaDist(_x_,alpha,beta), 0 , 
				 			 		  1e25,CGammaDist(_x_,alpha+1,beta)*alpha/beta);
					}
					else
					{
						if (rateType == 5)
						{
							global alpha = .5;
							global beta  =  1;
							global alpha2=  .75;
							global P	 = .5; 
							alpha:>0.01;alpha:<100;
							beta:>0.01;
							beta:<200;
							P:<1;
							alpha2:>0.01;alpha2:<100;
							category c = (resp, EQUAL, MEAN, P*GammaDist(_x_,alpha,beta) + (1-P)*GammaDist(_x_,alpha2,alpha2)
														   , P*CGammaDist(_x_,alpha,beta) + (1-P)*CGammaDist(_x_,alpha2,alpha2), 
														   0 , 1e25,
														   P*CGammaDist(_x_,alpha+1,beta)*alpha/beta + (1-P)*CGammaDist(_x_,alpha2+1,alpha2));
						}
						else
						{
							if (rateType == 6)
							{
								global betaP = 1;
								global betaQ = 1;
								betaP:>0.05;betaP:<85;
								betaQ:>0.05;betaQ:<85;
								category c = (resp, EQUAL, MEAN, _x_^(betaP-1)*(1-_x_)^(betaQ-1)/Beta(betaP,betaQ), IBeta(_x_,betaP,betaQ), 0 , 
						 			 		  1,IBeta(_x_,betaP+1,betaQ)*betaP/(betaP+betaQ));
							}
							else
							{
								if (rateType == 7)
								{
									global W = 2;
									/*W:>1;*/
									global P	 = 1-1/(resp+1);
									global betaP = 1;
									global betaQ = 2;
									betaP:>0.05;
									betaQ:>0.05;
									betaP:<85;
									betaQ:<85;
									P:>0.0000001;
									P:<0.9999999;
									categFreqMatrix = {resp+1,1};
									for (k=0; k<resp; k=k+1)
									{
										categFreqMatrix[k]:=P/resp__;
									}
									categFreqMatrix[resp]:=(1-P);
									category c = (resp+1, categFreqMatrix, MEAN, 
													P*_x_^(betaP-1)*(1-Min(_x_,1))^(betaQ-1)/Beta(betaP,betaQ)+W-W, 
													P*IBeta(Min(_x_,1),betaP,betaQ)+(1-P)*(_x_>=W), 
													0,1e25,
													P*IBeta(Min(_x_,1),betaP+1,betaQ)*betaP/(betaP+betaQ)+(1-P)*W*(_x_>=W));
								}
								else
								{
									if (rateType == 8)
									{
										global P	 = .5;
										global betaP = 1;
										global betaQ = 2;
										betaP:>0.05;betaP:<85;
										betaQ:>0.05;betaQ:<85;
										global alpha = .5;
										global beta  = 1;
										alpha:>0.01;alpha:<100;
										beta:>0.01;										
										beta:<200;
										P:<1;
										category c = (resp, EQUAL, MEAN, 
															P*_x_^(betaP-1)*(1-Min(_x_,1))^(betaQ-1)/Beta(betaP,betaQ)+(1-P)*GammaDist(_x_,alpha,beta), 
															P*IBeta(Min(_x_,1),betaP,betaQ)+(1-P)*CGammaDist(_x_,alpha,beta), 
															0,1e25,
															P*betaP/(betaP+betaQ)*IBeta(Min(_x_,1),betaP+1,betaQ)+(1-P)*alpha/beta*CGammaDist(_x_,alpha+1,beta));
									}	
									else
									{
										if (rateType == 9)
										{
											global P	 = .5;
											P:<1;
											global betaP = 1;
											betaP:>0.05;betaP:<85;
											global betaQ = 2;
											betaQ:>0.05;betaQ:<85;
											global alpha = .5;
											alpha:>0.01;alpha:<100;
											global beta  = 1;
											beta:>0.01;beta:<500;
											category c = (resp, EQUAL, MEAN, 
																P*_x_^(betaP-1)*(1-Min(_x_,1))^(betaQ-1)/Beta(betaP,betaQ)+(1-P)*(_x_>1)*GammaDist(Max(1e-20,_x_-1),alpha,beta), 
																P*IBeta(Min(_x_,1),betaP,betaQ)+(1-P)*CGammaDist(Max(_x_-1,0),alpha,beta), 
																0,1e25,
																P*betaP/(betaP+betaQ)*IBeta(Min(_x_,1),betaP+1,betaQ)+
																		(1-P)*(alpha/beta*CGammaDist(Max(0,_x_-1),alpha+1,beta)+CGammaDist(Max(0,_x_-1),alpha,beta)));
										}				
										else
										{
											if (rateType == 10)
											{
												global P	 = .5;
												global betaP = 1;
												global betaQ = 2;
												betaP:>0.05;
												betaQ:>0.05;
												betaP:<85;
												betaQ:<85;
												global mu = 3;
												global sigma  = .01;
												sigma:>0.0001;
												sqrt2pi = Sqrt(8*Arctan(1));
												P:<1;

												category c = (resp, EQUAL, MEAN, 
																P*_x_^(betaP-1)*(1-Min(_x_,1))^(betaQ-1)/Beta(betaP,betaQ)+
																	(1-P)*(_x_>=1)*Exp(-(_x_-mu)(_x_-mu)/(2*sigma*sigma))/(sqrt2pi__*sigma)/ZCDF((mu-1)/sigma), 
																P*IBeta(Min(_x_,1),betaP,betaQ)+(1-P)*(_x_>=1)*(1-ZCDF((mu-_x_)/sigma)/ZCDF((mu-1)/sigma)), 
																0,1e25,
																P*betaP/(betaP+betaQ)*IBeta(Min(_x_,1),betaP+1,betaQ)+
																(1-P)*(_x_>=1)*(mu*(1-ZCDF((1-mu)/sigma)-ZCDF((mu-_x_)/sigma))+
																sigma*(Exp((mu-1)(1-mu)/(2*sigma*sigma))-Exp((_x_-mu)(mu-_x_)/(2*sigma*sigma)))/sqrt2pi__)/ZCDF((mu-1)/sigma));
											}				
											else
											{
												if (rateType == 11)
												{
													global P	 = 1/3;
													global P1    = .5;

													global mu = 3;
													global sigma  = .5;
													sigma:>0.0001;
													global sigma1  = 1;
													sigma1:>0.0001;

													sqrt2pi = Sqrt(8*Arctan(1));
													P:<1;
													P1:<1;
													
													categFreqMatrix = {resp+1,1};
													for (k=1; k<=resp; k=k+1)
													{
														categFreqMatrix[k]:=(1-P)/resp__;
													}
													categFreqMatrix[0]:=P;

													category c = (resp+1, categFreqMatrix, MEAN,
																	(1-P)((1-P1)*Exp(-(_x_-mu)(_x_-mu)/(2*sigma1*sigma1))/(sqrt2pi__*sigma1)/ZCDF(mu/sigma1)+
																			  P1*Exp(-(_x_-1)(_x_-1)/(2*sigma*sigma))/(sqrt2pi__*sigma)/ZCDF(1/sigma)), 
																	P+(1-P)(_x_>1e-20)((1-P1)(1-ZCDF((mu-_x_)/sigma1)/ZCDF(mu/sigma1))+
																						P1*(1-ZCDF((1-_x_)/sigma)/ZCDF(1/sigma))), 
																	0,1e25,
																	(1-P)((1-P1)(mu*(1-ZCDF(-mu/sigma1)-ZCDF((mu-_x_)/sigma1))+
																	sigma1*(Exp(-mu*mu/(2*sigma1*sigma1))-Exp((_x_-mu)(mu-_x_)/(2*sigma1*sigma1)))/sqrt2pi__)/ZCDF(mu/sigma1)+
																	P(1-ZCDF(-1/sigma)-ZCDF((1-_x_)/sigma)+
																	sigma*(Exp(-1/(2*sigma*sigma))-Exp((_x_-1)(1-_x_)/(2*sigma*sigma)))/sqrt2pi__)/ZCDF(1/sigma))
																 );
												}
												else		
												{
													if (rateType == 12)
													{
														global P	 = 1/3;
														global P1    = .5;

														global mu = 3;
														global sigma  = .25;
														global sigma1 = .5;
														global sigma2 = 1;
														sigma:>0.0001;
														sigma1:>0.0001;
														sigma2:>0.0001;

														sqrt2pi = Sqrt(8*Arctan(1));
														P:<1;
														P1:<1;

														category c = (resp, EQUAL , MEAN,
																		2*P*Exp(-_x_^2/(2*sigma*sigma))+
																		(1-P)((1-P1)*Exp((_x_-mu)(mu-_x_)/(2*sigma2*sigma2))/(sqrt2pi__*sigma2)/ZCDF(mu/sigma2)+
																			  P1*Exp((1-_x_)(_x_-1)/(2*sigma1*sigma1))/(sqrt2pi__*sigma1)/ZCDF(1/sigma1)), 
																		P*(1-2*ZCDF(-_x_/sigma))+
																		(1-P)((1-P1)(1-ZCDF((mu-_x_)/sigma2)/ZCDF(mu/sigma2))+
																			   P1*(1-ZCDF((1-_x_)/sigma1)/ZCDF(1/sigma1))), 
																		0,1e25,
																		2*P*sigma*(1-Exp(-_x_*_x_/(2*sigma*sigma)))/sqrt2pi__+
																		(1-P)((1-P1)(mu*(1-ZCDF(-mu/sigma2)-ZCDF((mu-_x_)/sigma2))+
																		sigma2*(Exp(-mu*mu/(2*sigma2*sigma2))-Exp((_x_-mu)(mu-_x_)/(2*sigma2*sigma2)))/sqrt2pi__)/ZCDF(mu/sigma2)+
																		P1(1-ZCDF(-1/sigma1)-ZCDF((1-_x_)/sigma1)+
																		sigma1*(Exp(-1/(2*sigma1*sigma1))-Exp((_x_-1)(1-_x_)/(2*sigma1*sigma1)))/sqrt2pi__)/ZCDF(mu/sigma1))
																		);
													}	
													else
													{
														if (rateType == 13)
														{
																global sigma = .1;
																sigma:>0.0001;sigma:<10;
																sqrt2pi = Sqrt(8*Arctan(1));
																global _x_:<1e200;
																category c = (resp, EQUAL, MEAN, 
																				Exp (-Log(_x_)*Log(_x_) / (2*sigma*sigma)) / (_x_*sigma*sqrt2pi__), /*density*/
																				ZCDF (Log(_x_)/sigma), /*CDF*/
																				1e-200, 			   /*left bound*/
																				1e200, 			       /*right bound*/
																		  	    Exp (.5*sigma^2)*ZCDF (Log(_x_)/sigma-sigma),
																		  	    CONSTANT_ON_PARTITION
																		  	 );														
														}
														else
														{
															if (rateType == 14)
															{
																global alpha = .5;
																global beta = 1;
																alpha:>0.01;alpha:<100;
																beta:>0.01;
																beta:<200;
																category c = (resp, EQUAL, MEAN, GammaDist(_x_,alpha,beta), CGammaDist(_x_,alpha,beta), 0 , 
														 			 		  1e25,CGammaDist(_x_,alpha+1,beta)*alpha/beta,CONSTANT_ON_PARTITION);
															}
															else
															{
																global P1 = 1/3;
																global P2 = .5;
																P1:<1;
																P2:<1;
																global W1 = .25;
																global R1 = 4;
																global R2 = 3;
																R1:>1;
																R2:>1;
																categFreqMatrix = {{P1,(1-P1)*P2, (1-P1)*(1-P2)}} ;
																categRateMatrix = {{W1,W1*R1,W1*R1*R2}};
																category c = (3, categFreqMatrix , MEAN, ,categRateMatrix, 0, 1e25, ,CONSTANT_ON_PARTITION);				
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
					}				
				}	
			}	
		}	
	}		
	return 0;
}

/* ____________________________________________________________________________________________________________________*/

function FrameText (frameChar,vertChar,parOff,theText)
{
	h = Abs (theText)+4;
	fprintf (stdout,"\n");	
	for (k=0; k<parOff; k=k+1)
	{
		fprintf (stdout," ");
	}
	for (k=0; k<h;k=k+1)
	{
		fprintf (stdout,frameChar);
	}
	fprintf (stdout,"\n");	
	for (k=0; k<parOff; k=k+1)
	{
		fprintf (stdout," ");
	}
	fprintf (stdout,vertChar," ",theText," ",vertChar,"\n");
	for (k=0; k<parOff; k=k+1)
	{
		fprintf (stdout," ");
	}
	for (k=0; k<h;k=k+1)
	{
		fprintf (stdout,frameChar);
	}
	fprintf (stdout,"\n");	
	return 0;
}

/* ____________________________________________________________________________________________________________________*/

function BuildCodonFrequencies4 (obsF)
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
			PIStop = PIStop-obsF[first][0]*obsF[second][0]*obsF[third][0];
			continue; 
		}
		result[h-hshift][0]=obsF[first][0]*obsF[second][0]*obsF[third][0];
	}
	return result*(1.0/PIStop);
}

/* ____________________________________________________________________________________________________________________*/

function BuildCodonFrequencies12 (obsF)
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
/* ____________________________________________________________________________________________________________________*/


function GetDistributionParameters (sigLevel)
{
	GetInformation (distrInfo,c);
	D = Columns(distrInfo);
	E = 0.0;
	T = 0.0;
	sampleVar = 0.0;
	for (k=0; k<D; k=k+1)
	{
		T = distrInfo[0][k]*distrInfo[1][k];
		E = E+T;
		sampleVar = T*distrInfo[0][k]+sampleVar;
	}
	sampleVar = sampleVar-E*E;
	fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\ndN/dS = ",E, " (sample variance = ",sampleVar,")\n");
	for (k=0; k<D; k=k+1)
	{
		fprintf (LAST_FILE_PATH,"\nRate[",Format(k+1,0,0),"]=",
				 Format(distrInfo[0][k],12,8), " (weight=", Format(distrInfo[1][k],9,7),")");
	}
	
	for (k=0; k<D; k=k+1)
	{
		if (distrInfo[0][k]>1) break;
	}
	if (k<D)
	/* have rates > 1 */
	{
		ConstructCategoryMatrix(marginals,lf,COMPLETE);
		
		CC = Columns (marginals);
		if (rateType>=13)
		/* subset rate variation */
		{
			CC  = CC/numberOfSubsets;
			
			subsetMarginals = {D,numberOfSubsets};
			for (v=0; v<numberOfSubsets; v=v+1)
			{
				for (l=0; l<D; l=l+1)
				{
					for (h=0; h<CC; h=h+1)
					{
						subsetMarginals[l][v] = subsetMarginals[l][v] + marginals[l][v*CC+h];
					}
				}
			}
			marginals = subsetMarginals;
			subsetMarginals = 0;
			fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\n Subsets with dN/dS>1 (Posterior cutoff = ",sigLevel,")\n\n");
			for (v=0; v<numberOfSubsets; v=v+1)
			{
				sampleVar = 0;
				for (h=0; h<D; h=h+1)
				{
					sampleVar = sampleVar+distrInfo[1][h]*marginals[h][v];
				}
				positiveProb = 0;
				for (l=k; l<D; l=l+1)
				{
					positiveProb = positiveProb+distrInfo[1][l]*marginals[l][v];
				}
				positiveProb = positiveProb/sampleVar;
				marginals[0][v] = positiveProb;
				if (positiveProb>=sigLevel)
				{
					fprintf (LAST_FILE_PATH,Format (v+1,0,0)," (",positiveProb,")\n");
				}
			}
			fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\n Subsets with dN/dS<=1 (Posterior cutoff = ",sigLevel,")\n\n");
			for (v=0; v<numberOfSubsets; v=v+1)
			{
				if (marginals[0][v]<sigLevel)
				{
					fprintf (LAST_FILE_PATH,Format (v+1,0,0)," (",marginals[0][v],")\n");
				}
			}
		}
		else
		{
			fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\n Sites with dN/dS>1 (Posterior cutoff = ",sigLevel,")\n\n");
			for (v=0; v<CC; v=v+1)
			{
				sampleVar = 0;
				for (h=0; h<D; h=h+1)
				{
					sampleVar = sampleVar+distrInfo[1][h]*marginals[h][v];
				}
				positiveProb = 0;
				for (l=k; l<D; l=l+1)
				{
					positiveProb = positiveProb+distrInfo[1][l]*marginals[l][v];
				}
				positiveProb = positiveProb/sampleVar;
				marginals[0][v] = positiveProb;
				if (positiveProb>=sigLevel)
				{
					fprintf (LAST_FILE_PATH,Format (v+1,0,0)," (",positiveProb,")\n");
				}
			}
			fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\n Sites with dN/dS<=1 (Posterior cutoff = ",sigLevel,")\n\n");
			for (v=0; v<CC; v=v+1)
			{
				if (marginals[0][v]<sigLevel)
				{
					fprintf (LAST_FILE_PATH,Format (v+1,0,0)," (",marginals[0][v],")\n");
				}
			}
		}
		marginals = 0;
	}
	else
	{
		fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\n No rate classes with dN/dS>1.");
	}
	fprintf  (LAST_FILE_PATH,"\n\n------------------------------------------------\n\n");
	return E;
}

/* ____________________________________________________________________________________________________________________*/

function PopulateModelMatrix (ModelMatrixName&, EFV)
{
	ModelMatrixName = {ModelMatrixDimension,ModelMatrixDimension}; 

	hshift = 0;
	
	if (modelType==0)
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
			  			}
			  			else
			  			{
			  				transition = v%16$4;
			  				transition2= h%16$4;
			  			}
			  		}
			  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
			  		{
			  			ModelMatrixName[h-hshift][v-vshift] := t*EFV__[transition__];
			  			ModelMatrixName[v-vshift][h-hshift] := t*EFV__[transition2__];
				  	}
			  		else
			  		{
				  		ModelMatrixName[h-hshift][v-vshift] := c*t*EFV__[transition__];
			  			ModelMatrixName[v-vshift][h-hshift] := c*t*EFV__[transition2__];
		  			}
			  	}
			  }
		}
	}
	else
	{
		if (modelType==1)
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
				  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
				  		{
				  			ModelMatrixName[h-hshift][v-vshift] := t*EFV__[transition__][nucPosInCodon__];
				  			ModelMatrixName[v-vshift][h-hshift] := t*EFV__[transition2__][nucPosInCodon__];
					  	}
				  		else
				  		{
					  		ModelMatrixName[h-hshift][v-vshift] := c*t*EFV__[transition__][nucPosInCodon__];
				  			ModelMatrixName[v-vshift][h-hshift] := c*t*EFV__[transition2__][nucPosInCodon__];
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
				  			}
				  			else
				  			{
				  				transition = v%16$4;
				  				transition2= h%16$4;
				  			}
				  		}
				  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
				  		{
				  			if (Abs(transition-transition2)%2)
				  			{
				  				ModelMatrixName[h-hshift][v-vshift] := kappa*t;
				  				ModelMatrixName[v-vshift][h-hshift] := kappa*t;
				  			}
				  			else
				  			{
				  				ModelMatrixName[h-hshift][v-vshift] := t;
				  				ModelMatrixName[v-vshift][h-hshift] := t;
				  			}
				  			
					  	}
				  		else
				  		{
				  			if (Abs(transition-transition2)%2)
				  			{
				  				ModelMatrixName[h-hshift][v-vshift] := kappa*c*t;
				  				ModelMatrixName[v-vshift][h-hshift] := kappa*c*t;
				  			}
				  			else
				  			{
				  				ModelMatrixName[h-hshift][v-vshift] := c*t;
				  				ModelMatrixName[v-vshift][h-hshift] := c*t;
				  			}
					  	}
				  	}	
				 }
			}	
		}
	 }
	 return (modelType>1);
}

/* ____________________________________________________________________________________________________________________*/

function PopulateModelMatrix2 (ModelMatrixName&, EFV)
{
	ModelMatrixName = {ModelMatrixDimension,ModelMatrixDimension}; 

	hshift = 0;
	
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
		  			}
		  			else
		  			{
		  				transition = v%16$4;
		  				transition2= h%16$4;
		  			}
		  		}
		  		if (_Genetic_Code[0][h]==_Genetic_Code[0][v]) 
		  		{
		  			if (Abs(transition-transition2)%2)
		  			{
		  				ExecuteCommands ("ModelMatrixName[h-hshift][v-vshift] := kappa"+l+"*t;ModelMatrixName[v-vshift][h-hshift] := kappa"+l+"*t;");
		  			}
		  			else
		  			{
		  				ModelMatrixName[h-hshift][v-vshift] := t;
		  				ModelMatrixName[v-vshift][h-hshift] := t;
		  			}
		  			
			  	}
		  		else
		  		{
		  			if (Abs(transition-transition2)%2)
		  			{
		  				ExecuteCommands ("ModelMatrixName[h-hshift][v-vshift] := c*kappa"+l+"*t;ModelMatrixName[v-vshift][h-hshift] := c*kappa"+l+"*t;");
		  			}
		  			else
		  			{
		  				ModelMatrixName[h-hshift][v-vshift] := c*t;
		  				ModelMatrixName[v-vshift][h-hshift] := c*t;
		  			}
			  	}
		  	}	
		 }
	}	
	return 1;
}
/* ____________________________________________________________________________________________________________________*/

function spawnLikelihood (kappaSharedOrNot)
{
	if (kappaSharedOrNot)
	{
		for (l=0; l<numberOfSubsets;l=l+1)
		{
			if (modelType<=1)
			{
				ExecuteCommands ("modelMatrix"+l+" = 0;MULTIPLY_BY_FREQS = PopulateModelMatrix (\"modelMatrix"+l+"\", observedFreq"+l+");");
			}
			else
			{
				ExecuteCommands ("modelMatrix"+l+" = 0;MULTIPLY_BY_FREQS = PopulateModelMatrix2 (\"modelMatrix"+l+"\", observedFreq"+l+");");
			}			
			ExecuteCommands ("Model theModel"+l+" = (modelMatrix"+l+",vectorOfFrequencies"+l+",MULTIPLY_BY_FREQS);");
			partitionTreeString=partitionTrees[l];
			ExecuteCommands ("Tree subsetTree"+l+"=partitionTreeString;");
		}	
	}
	else
	{
		MULTIPLY_BY_FREQS = PopulateModelMatrix ("modelMatrix", observedFreq);
		Model theModel = (modelMatrix,vectorOfFrequencies,MULTIPLY_BY_FREQS);
		for (v=0; v<numberOfSubsets;v=v+1)
		{
			partitionTreeString=partitionTrees[v];
			ExecuteCommands ("Tree subsetTree"+v+"=partitionTreeString;");
		}
	}
	lfSpawnString = "LikelihoodFunction lf = (";
	for (v=0; v<numberOfSubsets;v=v+1)
	{
		if (v)
		{
			lfSpawnString = lfSpawnString+",";
		}
		lfSpawnString = lfSpawnString+"subsetFilter"+v+",subsetTree"+v;
	}
	lfSpawnString = lfSpawnString+");";
	ExecuteCommands (lfSpawnString);
	return 0;
}


/* ____________________________________________________________________________________________________________________*/

NICETY_LEVEL = 3;

#include "TemplateModels/chooseGeneticCode.def";

ModelMatrixDimension = 64;
for (h = 0 ;h<64; h=h+1)
{
	if (_Genetic_Code[h]==10)
	{
		ModelMatrixDimension = ModelMatrixDimension-1;
	}
}

#include "MFPSreader.def";

/* now spawn the dataset filters */


lowerSeqBound = 0;
for (modelType=0; modelType<numberOfSubsets; modelType = modelType+1)
{
	ExecuteCommands ("DataSetFilter   subsetFilter"+modelType+"= CreateFilter (ds,3,\"\",(speciesIndex>=lowerSeqBound)&&(speciesIndex<lowerSeqBound+partitionLengths[modelType]),GeneticCodeExclusions);");
	lowerSeqBound = lowerSeqBound+partitionLengths[modelType];
}

chosenModelList = {17,1};

ChoiceList (modelType,"Distributions",1,SKIP_NONE,
			"Run All","Run all available dN/dS distributions",
			"Run Custom","Choose from available dN/dS distributions.");
			
if (modelType<0)
{
	return;
}

if (modelType==0)
{
	for (rateType = 0; rateType<17; rateType=rateType+1)
	{
		chosenModelList[rateType][0] = 1;
	}
}
else
{
	ChoiceList (modelTypes,"Distributions",0,SKIP_NONE,
				"Single Rate","Single Rate",
				"Neutral","Neutral",
	  			"Selection","Selection",
			    "Discrete","Discrete",
			    "Freqs","Freqs",
			    "Gamma","Gamma",
			    "2 Gamma","2 Gamma",
			    "Beta","Beta",
			    "Beta & w","Beta & w",
			    "Beta & Gamma","Beta & Gamma",
			    "Beta & (Gamma+1)","Beta & (Gamma+1)",
			    "Beta & (Normal>1)","Beta & (Normal>1)",
			    "0 & 2 (Normal>1)","0 & 2 (Normal>1)",
			    "3 Normal","3 Normal",
			    "RE:Log normal","Random Effects: Log normal",
			    "RE:Gamma","Random Effects: Gamma",
			    "RE:Discrete","Random Effects: 3 bin Discrete");
			    
	if (modelTypes[0]<0)
	{
		return;
	}
	for (rateType = 0; rateType < Rows(modelTypes)*Columns(modelTypes); rateType = rateType + 1)
	{
		modelType = modelTypes[rateType];
		chosenModelList[modelType] = 1;
	}
}

ChoiceList (shareType,"Choose parameter sharing mode",1,SKIP_NONE,
			"All","Share dN/dS, transversion/transition ratio (if applicable) and base frequencies for all subsets.",
			"dN/dS Only","Share only dN/dS. Transversion/transition ratio (if applicable) and base frequencies are separate for each subset."
);

if (shareType<0)
{
	return;
}

ChoiceList (modelType,"Choose a model",1,SKIP_NONE,
			"MG94 1x4","Muse-Gaut 94 model with 4(-1) nucleotide frequency parameters (intra-codon position independent).",
			"MG94 3x4","Muse-Gaut 94 model with 12(-3) nucleotide frequency parameters (intra-codon position specific).",
			"GY94 1x4","Goldman-Yang 94 model with 4(-1) nucleotide frequency parameters (intra-codon position independent).",
			"GY94 3x4","Goldman-Yang 94 model with 12(-3) nucleotide frequency parameters (intra-codon position specific)."
);

if (modelType<0)
{
	return;
}

if ((modelType==0)||(modelType==2))
{
	if (shareType==0)
	{
		HarvestFrequencies (observedFreq,filteredData,1,1,0);
		vectorOfFrequencies = BuildCodonFrequencies4 (observedFreq);
	}
	else
	{
		for (v=0; v<numberOfSubsets;v=v+1)
		{
			ExecuteCommands ("global kappa"+v+"=2.;HarvestFrequencies (observedFreq"+v+",subsetFilter"+v+",1,1,0);vectorOfFrequencies"+v+"= BuildCodonFrequencies4 (observedFreq"+v+");");
		}		
	}
}
else
{
	if (shareType==0)
	{
		HarvestFrequencies (observedFreq,filteredData,3,1,1);
		vectorOfFrequencies = BuildCodonFrequencies12 (observedFreq);
	}
	else
	{
		for (v=0; v<numberOfSubsets;v=v+1)
		{
			ExecuteCommands ("global kappa"+v+"=2.;HarvestFrequencies (observedFreq"+v+",subsetFilter"+v+",3,1,1);vectorOfFrequencies"+v+"= BuildCodonFrequencies12 (observedFreq"+v+");");
		}		
	}
}

if (modelType>1)
{	
	global kappa = 2.;
}

fprintf (stdout, "\n\n\nChoose the cutoff (0 to 1) for posterior of dN/dS>1 for a site to be considered under selective pressure:");
fscanf  (stdin, "Number",psigLevel);
if ((psigLevel <= 0)||(psigLevel>1))
{
	psigLevel = .95;
}
fprintf (stdout, "\n>Using ", psigLevel , " cutoff\n");

fprintf (stdout, "\nChoose the number of categories in discretized distributions:");
fscanf  (stdin, "Number",categCount);
categCount = categCount$1;
if (categCount<=0)
{
	categCount = 8;
}

fprintf (stdout, "\n>Using ", Format (categCount,0,0), " categories.\n");

SetDialogPrompt ("Write detailed results to:");

fprintf (PROMPT_FOR_FILE,CLEAR_FILE);

global c = 1.;

dummyVar = FrameText ("-","|",2,"SUMMARY TABLE");
tableSeparator =  "+-------------------------+----------------+---------------+-----+\n";
fprintf (stdout, "\n\"p\" is the number of parameters in addition to the branch lengths.\nDetailed results including sites with dN/dS>1 will be written to\n",LAST_FILE_PATH,"\n\n");
fprintf (stdout, tableSeparator,
				 "| MODEL (Number & Desc)   | Log likelihood | 	   dN/dS     |  p  |\n",
				 tableSeparator);
				 
cachedBranchLengths = {{-1,-1}};
				 
if (chosenModelList[0]>0)
{
	timer = Time(1);
	fprintf (LAST_FILE_PATH,"\n*** RUNNING SINGLE RATE MODEL ***\n#################################\n");
	dummy = spawnLikelihood (shareType);
	Optimize (res,lf);
	fprintf (LAST_FILE_PATH,"\n>Done in ", Time(1)-timer, " seconds \n\n");
	fprintf (LAST_FILE_PATH,lf,"\n\n-----------------------------------\n\ndN/dS = ",c,"\n\n");

	fprintf (stdout, "|  0. Single Rate Model   | ",Format (res[1][0],14,6)," | ",Format (c,13,8)," |  0  |\n",
					 tableSeparator);
					 
	timer = res[1][1]-res[1][2];
	cachedBranchLengths = {timer,1};
	
	for (rateType = timer; rateType < Columns(cachedBranchLengths); rateType = rateType+1)
	{
		cachedBranchLengths[rateType-timer][0] = res [0][rateType];
	}
}

for (rateType = 0; rateType < 16; rateType = rateType + 1)
{
	if (chosenModelList[rateType+1]==0)
	{
		continue;
	}
	timer = Time(1);
	dummy = SetWDistribution (categCount);
	dummy = spawnLikelihood (shareType);
	
	fprintf (LAST_FILE_PATH,"\n*** RUNNING MODEL ", Format(rateType+1,0,0), " (",ModelNames[rateType],") ***\n######################################\n");
	/*if (cachedBranchLengths[0][0]>=0.0)
	{
		v = ParameterCount[rateType];
		if (modelType>1)
		{
			v=v+1;
		}
		for (h=0; h<Rows(cachedBranchLengths); h=h+1)
		{
			SetParameter (lf,h+v,cachedBranchLengths[h][0]);
		}
	}*/
	Optimize (res,lf);
	fprintf (LAST_FILE_PATH,"\n>Done in ",Time(1)-timer, " seconds \n\n", lf);
	fprintf (stdout, "| ");
	if (rateType<9)
	{
		fprintf (stdout," ");
	}
	fprintf (stdout, Format (rateType+1,0,0), ". ", ModelNames[rateType]);
	for (dummy = Abs(ModelNames[rateType])+5; dummy<25; dummy = dummy+1)
	{
		fprintf (stdout," ");
	}
	dummy = GetDistributionParameters(psigLevel);
	fprintf (stdout,"| ",Format (res[1][0],14,6)," | ",Format (dummy,13,8)," |  ",
						 Format(ParameterCount[rateType],0,0),"  |\n",tableSeparator);

	if (modelType>1)
	{	
		kappa = 2.;
	}
}
