# Mini Manitoba Hydro LP Model
# Written for zimpl 3.0.0a
# Licensed under the MIT license
# Developed for fun using publicly available sources.
# This software comes with no guarantees and no claims that it is fit for any purpose.
# john howard, 1 Dec 2009

# Inflows:
#  CR churchill river
#  LT lower nelson tribs
#  SL south indian lake
#  RR red river
#  WR winnipeg river
#
# Major Storages:
#  LW lake winnipeg
#  CL cedar lake
#
# Controlled Channels:
#  MF missi falls
#  NT notigi
#  EC east channel
#  WC west channel
#
# Generation Projects:
#  GR grand rapids
#  JP jenpeg
#  KE kelsey
#  LN lower nelson projects

# variables and constraints are named the following way:
# Type Subtype 2LetterName Kind Modifier
# (v|k) [c] (..) (q|i|o|p|v|e|s|hk|ss|tw|mb|rc|os|ur) [ Start | End | Max | Min | Adj | Fact | Offset ]

param kTS := 662;
set sTime := { 1 .. kTS };

# for 5 day averaging requires indices spaced 3 apart
set sTime5 := { <t> in sTime with t < (kTS - 1) and t mod 3 == 0 };

param kCMSd2KCFSdFact := 35.315 / 1000;

# column 8 is month
param vMONTH[ sTime ] := read "historical.csv" as "8n" skip 1;
set sMonth := { 1 .. 12 };

#########################
### Inital Conditions ###

# South Indian Lake
param kSLeMin := 840.0;
param kSLeMax := 847.9;
param kSLssFactor := 283.7;
param kSLvMax := (kSLeMax - kSLeMin) * kSLssFactor;

# Grand Rapids Pond (Cedar Lake)
param kGReMin := 830.0;
param kGReMax := 841.5;
param kGRssFactor := 330.9;
param kGRvMax := (kGReMax - kGReMin) * kGRssFactor;

# Lake Winnipeg
param kLWeMin := 709.0;
param kLWeMax := 714.75;
param kLWssFactor := 3040.0;
param kLWvMax := (kLWeMax - kLWeMin) * kLWssFactor;

# Kelsey Pond
param kKEeMin := 0.0;
param kKEeMax := 0.1; # as modelled
param kKEssFactor := 902.0;
param kKEvMax := (kKEeMax - kKEeMin) * kKEssFactor;

# Storage at aggregate Lower Nelson Projects
param kLNeMin := 0.0;
param kLNeMax := 3.0; # as modelled
param kLNssFactor := 200.0;
param kLNvMax := (kLNeMax - kLNeMin) * kLNssFactor;
param kLNvStart := kLNvMax / 2;

####################################################################
### Churchill River through South Indian Lake and Notigi Control ###

param kNToMin := 15; # control discharge limits (KCFS)
param kNToMax := 35;

param kMFoMin := 0; # control discharge limits (KCFS)
param kMFoMax := 10; # as modelled
param kMFoFact := 0.001; # adjustments (calibrated for 92-94 peroid)

var vMFo[ sTime ] >= 1 <= kMFoMax; # Missi Falls acts as spill
var vNTo[ sTime ] >= kNToMin <= kNToMax;
var vSLv[ sTime ] >= 0 <= kSLvMax;

# 2nd column for Churchill River
param vCRq[ sTime ] := read "historical.csv" as "2n" skip 1;

param kSLiAdj := 2.3; # adjustments (calibrated for 92-94 peroid)
param kSLvStart := kSLvMax / 2;

# refill
subto kcSLvEnd: vSLv[ kTS ] >= kSLvStart;

# change-in-storage + outflow == inflow
subto vcSLmb:
	forall <t> in sTime do
		if ( t == 1 ) then	vSLv[ 1 ] - kSLvStart + vMFo[ 1 ] +  vNTo[ 1 ]
		else				vSLv[ t ] - vSLv[ t - 1 ] + vMFo[ t ] +  vNTo[ t ]
		end
		== kCMSd2KCFSdFact * vCRq[ t ] + kSLiAdj;

# Notigi within-week outflow shaping
param kNTosFact := 0; # as modelled
subto vcNTosA: forall<t> in sTime5 do vNTo[ t - 2 ] >= vNTo[ t ] - kNTosFact;
subto vcNTosB: forall<t> in sTime5 do vNTo[ t - 2 ] <= vNTo[ t ] + kNTosFact;
subto vcNTosC: forall<t> in sTime5 do vNTo[ t - 1 ] >= vNTo[ t ] - kNTosFact;
subto vcNTosD: forall<t> in sTime5 do vNTo[ t - 1 ] <= vNTo[ t ] + kNTosFact;
subto vcNTosE: forall<t> in sTime5 do vNTo[ t + 1 ] >= vNTo[ t ] - kNTosFact;
subto vcNTosF: forall<t> in sTime5 do vNTo[ t + 1 ] <= vNTo[ t ] + kNTosFact;
subto vcNTosG: forall<t> in sTime5 do vNTo[ t + 2 ] >= vNTo[ t ] - kNTosFact;
subto vcNTosH: forall<t> in sTime5 do vNTo[ t + 2 ] <= vNTo[ t ] + kNTosFact;

###########################################################
### Sask River into Cedar Lake and through Grand Rapids ###

param kGRoMin := 5; # plant/control discharge limits (KCFS)
param kGRoMax := 53;
param kGRsMin := 0;
param kGRsMax := 40;
param kGRpMax := 472; # generation limits (MW)

var vGRv[ sTime ] >= 0 <= kGRvMax;
var vGRs[ sTime ] >= kGRsMin <= kGRsMax;
var vGRo[ sTime ] >= kGRoMin <= kGRoMax;
var vGRp[ sTime ] >= 0 <= kGRpMax;

# 1st column for Sask River
param vSKRq[ sTime ] := read "historical.csv" as "1n" skip 1;

param kGRhk := 9.2; # plant HK factors (MW/KCFS)
param kGRiAdj := -0.6; # adjustments (calibrated for 92-94 peroid)
param kGRvStart := kGRvMax / 2;

# refill
subto kcGRvEnd: vGRv[ kTS ] >= kGRvStart;

# change-in-storage + outflow == inflow
subto vcGRmb:
	forall <t> in sTime do
		if ( t == 1 ) then	vGRv[ 1 ] - kGRvStart + vGRo[ 1 ] + vGRs[ 1 ]
		else				vGRv[ t ] - vGRv[ t - 1 ] + vGRo[ t ] + vGRs[ t ]
		end
		== kCMSd2KCFSdFact * vSKRq[ t ] + kGRiAdj;

# compute power from discharge
subto vcGRp:
	forall <t> in sTime do
		vGRp[ t ] == kGRhk * vGRo[ t ];

# Grand Rapids within-week outflow shaping
param kGRosFact := 10; # as modelled
subto vcGRosA: forall<t> in sTime5 do vGRo[ t - 2 ] >= vGRo[ t ] - kGRosFact;
subto vcGRosB: forall<t> in sTime5 do vGRo[ t - 2 ] <= vGRo[ t ] + kGRosFact;
subto vcGRosC: forall<t> in sTime5 do vGRo[ t - 1 ] >= vGRo[ t ] - kGRosFact;
subto vcGRosD: forall<t> in sTime5 do vGRo[ t - 1 ] <= vGRo[ t ] + kGRosFact;
subto vcGRosE: forall<t> in sTime5 do vGRo[ t + 1 ] >= vGRo[ t ] - kGRosFact;
subto vcGRosF: forall<t> in sTime5 do vGRo[ t + 1 ] <= vGRo[ t ] + kGRosFact;
subto vcGRosG: forall<t> in sTime5 do vGRo[ t + 2 ] >= vGRo[ t ] - kGRosFact;
subto vcGRosH: forall<t> in sTime5 do vGRo[ t + 2 ] <= vGRo[ t ] + kGRosFact;

##############################################################################
### Lake Winnipeg Storage as operated by JENPEG and effect of Each Channel ###

param kJPoMin := 0; # plant/control discharge limits (KCFS)
param kJPoMax := 93;
param kJPsMin := 0; # plant/control discharge limits (KCFS)
param kJPsMax := 9e9; # as modelled
param kJPpMax := 97; # generation limits (MW)

var vLWv[ sTime ] >= 0 <= kLWvMax;
var vECo[ sTime ] >= 0; # upper bound determined by rating curve
var vJPo[ sTime ] >= kJPoMin <= kJPoMax;
var vJPs[ sTime ] >= kJPsMin <= kJPsMax;
var vJPp[ sTime ] >= 0 <= kJPpMax;

# 3rd column for Red River and 4th column for Winnipeg River
param vRRq[ sTime ] := read "historical.csv" as "3n" skip 1;
param vWRq[ sTime ] := read "historical.csv" as "4n" skip 1;

param kLWiAdj := 11.7; # adjustments (calibrated for 92-94 peroid)
param kLWvStart := kLWvMax / 2;

param kJPhk := 1; # plant HK factors (MW/KCFS)
param kJPoNovMaxFact := 0; # curves
param kJPoNovMaxOffset := 0;
param kJPtwFact := 0; # TODO
param kJPtwOffset := 0;

# refill
subto kcLWvEnd: vLWv[ kTS ] >= kLWvStart;

# change-in-storage + outflow == inflow
subto vcLWmb:
	forall <t> in sTime do
		if ( t == 1 ) then	vLWv[ 1 ] - kLWvStart + vJPo[ 1 ] + vJPs[ 1 ] + vECo[ 1 ]
		else				vLWv[ t ] - vLWv[ t - 1 ] + vJPo[ t ] + vJPs[ t ] + vECo[ t ]
		end
		== kCMSd2KCFSdFact * vRRq[ t ] + kCMSd2KCFSdFact * vWRq[ t ] + vGRo[ t ] + vGRs[ t ] + kLWiAdj;

# compute power from discharge
subto vcJPp:
	forall <t> in sTime do
		vJPp[ t ] == kJPhk * vJPo[ t ];

# West Channel Max Discharge
param vWCoMax[ sMonth ] := <1> 8.6440678, <2> 7.79661017, <3> 7.11864407, <4> 6.61016949, <11> 10.3389831, <12> 9.3220339 default 12.5423729;
subto vJPoA:
	forall <t,m> in sTime cross sMonth with m == vMONTH[ t ] do
		vJPo[ t ] <= vLWv[ t ] * vWCoMax[ m ] / kLWssFactor;

# East Channel Discharge
subto vcECo:
	forall <t> in sTime do
		vECo[ t ] == vLWv[ t ] * 4.67463938 / kLWssFactor; # convert q/ft (from historical 92-94 period data) to q/v

# Jenpeg within-week outflow shaping
param kJPosFact := 2; # as modelled
subto vcJPosA: forall<t> in sTime5 do vJPo[ t - 2 ] >= vJPo[ t ] - kJPosFact;
subto vcJPosB: forall<t> in sTime5 do vJPo[ t - 2 ] <= vJPo[ t ] + kJPosFact;
subto vcJPosC: forall<t> in sTime5 do vJPo[ t - 1 ] >= vJPo[ t ] - kJPosFact;
subto vcJPosD: forall<t> in sTime5 do vJPo[ t - 1 ] <= vJPo[ t ] + kJPosFact;
subto vcJPosE: forall<t> in sTime5 do vJPo[ t + 1 ] >= vJPo[ t ] - kJPosFact;
subto vcJPosF: forall<t> in sTime5 do vJPo[ t + 1 ] <= vJPo[ t ] + kJPosFact;
subto vcJPosG: forall<t> in sTime5 do vJPo[ t + 2 ] >= vJPo[ t ] - kJPosFact;
subto vcJPosH: forall<t> in sTime5 do vJPo[ t + 2 ] <= vJPo[ t ] + kJPosFact;

# Jenpeg intra-week shaping
subto vcJPosI: forall<t> in sTime5 without { kTS - 2 } do vJPo[ t + 3 ] >= vJPo[ t ] - kJPosFact * 2;
subto vcJPosJ: forall<t> in sTime5 without { kTS - 2 } do vJPo[ t + 3 ] <= vJPo[ t ] + kJPosFact * 2;

#########################
### Kelsey Operations ###

param kKEoMin := 0; # plant/control discharge limits (KCFS)
param kKEoMax := 55.4;
param kKEsMin := 0; # as modelled
param kKEsMax := 9e9; # as modelled
param kKEpMax := 211; # generation limits (MW)

var vKEv[ sTime ] >= 0 <= kKEvMax;
var vKEs[ sTime ] >= kKEsMin <= kKEsMax;
var vKEo[ sTime ] >= kKEoMin <= kKEoMax;
var vKEp[ sTime ] >= 0 <= kKEpMax;

# 5th column for Gunisao River
param vGUNq[ sTime ] := read "historical.csv" as "5n" skip 1;

param kKEhk := 3.8; # plant HK factors (MW/KCFS)
param kKEiFact := 1; # adjustments (calibrated for 92-94 peroid)
param kKEtwFact := 0; # curves
param kKEtwOffset := 0;
param kKEiAdj := 3; # adjustments (calibrated for 92-94 peroid)
param kKEvStart := kKEvMax / 2;

# refill
subto kcKEvEnd: vKEv[ kTS ] >= kKEvStart;

# change-in-storage + outflow == inflow
subto vcKEmb:
	forall <t> in sTime do
		if ( t == 1 ) then	vKEv[ 1 ] - kKEvStart + vKEo[ 1 ] + vKEs[ 1 ]
		else				vKEv[ t ] - vKEv[ t - 1 ] + vKEo[ t ] + vKEs[ t ]
		end
		== kKEiFact * kCMSd2KCFSdFact * vGUNq[ t ] + kKEiAdj + vECo[ t ] +	vJPo[ t ] + vJPs[ t ];

# compute power from discharge
subto vcKEp:
	forall <t> in sTime do
		vKEp[ t ] == kKEhk * vKEo[ t ];

#########################################################################
### Lower Nelson Operations with inflows from Upper Nelson and Notigi ###

param kLNoMin := 0; # plant/control discharge limits (KCFS)
param kLNoMax := 165.7;
param kLNsMin := 0; # plant/control discharge limits (KCFS)
param kLNsMax := 150; # kpill limits (KCFS)
param kLNpMax := 3583; # generation limits (MW)

var vLNv[ sTime ] >= 0 <= kLNvMax;
var vLNs[ sTime ] >= kLNsMin <= kLNsMax;
var vLNo[ sTime ] >= kLNoMin <= kLNoMax;
var vLNp[ sTime ] >= 0 <= kLNpMax;

# 6th column for Lower Nelson Tribs
param vLTq[ sTime ] := read "historical.csv" as "6n" skip 1;

param kLNhk := 21.7; # plant HK factors (MW/KCFS)
param kLNiAdj := 5; # adjustments (calibrated for 92-94 peroid)
param kLTiFact := 3; # adjustments (calibrated for 92-94 peroid)

# refill
subto kcLNvEnd: vLNv[ kTS ] >= kLNvStart;

# Routed discharges from Notigi to Lower Nelson
var vNTor[ sTime ] >= 0;
set sUIR := { 1, 2, 3, 4 };
param kNTur[ sUIR ] := <1> 0.0, <2> 0.05, <3> 0.80, <4> 0.15; # as modelled
subto vcNTorA:
	forall <t> in sTime without { 1, 2, 3 } do
		vNTor[ t ] == vNTo[ t - 0 ] * kNTur[ 1 ] + vNTo[ t - 1 ] * kNTur[ 2 ] + vNTo[ t - 2 ] * kNTur[ 3 ] + vNTo[ t - 3 ] * kNTur[ 4 ];

# change-in-storage + outflow == inflow
subto vcLNmb:
	forall <t> in sTime do
		if ( t == 1 ) then	vLNv[ 1 ] - kLNvStart + vLNo[ 1 ] + vLNs[ 1 ]
		else				vLNv[ t ] - vLNv[ t - 1 ] + vLNo[ t ] + vLNs[ t ]
		end
		== kLTiFact * kCMSd2KCFSdFact * vLTq[ t ] + kLNiAdj + vNTor[ t ] + vKEo[ t ] + vKEs[ t ];

# compute power from discharge
subto vcLNp:
	forall <t> in sTime do
		vLNp[ t ] == kLNhk * vLNo[ t ];

# Lowern Nelson within-week outflow shaping
param kLNosFact := 10; # as modelled
subto vcLNosA: forall<t> in sTime5 do vLNo[ t - 2 ] >= vLNo[ t ] - kLNosFact;
subto vcLNosB: forall<t> in sTime5 do vLNo[ t - 2 ] <= vLNo[ t ] + kLNosFact;
subto vcLNosC: forall<t> in sTime5 do vLNo[ t - 1 ] >= vLNo[ t ] - kLNosFact;
subto vcLNosD: forall<t> in sTime5 do vLNo[ t - 1 ] <= vLNo[ t ] + kLNosFact;
subto vcLNosE: forall<t> in sTime5 do vLNo[ t + 1 ] >= vLNo[ t ] - kLNosFact;
subto vcLNosF: forall<t> in sTime5 do vLNo[ t + 1 ] <= vLNo[ t ] + kLNosFact;
subto vcLNosG: forall<t> in sTime5 do vLNo[ t + 2 ] >= vLNo[ t ] - kLNosFact;
subto vcLNosH: forall<t> in sTime5 do vLNo[ t + 2 ] <= vLNo[ t ] + kLNosFact;

# Lowern Nelson inter-week outflow shaping
subto vcLNosI: forall<t> in sTime5 without { kTS - 2 } do vLNo[ t + 3 ] >= vLNo[ t ] - kLNosFact * 2;
subto vcLNosJ: forall<t> in sTime5 without { kTS - 2 } do vLNo[ t + 3 ] <= vLNo[ t ] + kLNosFact * 2;

########################################################

# 7th column for Load
param vLOAD[ sTime ] := read "historical.csv" as "7n" skip 1;
var sLOAD >= 0;
subto vcLOAD:
	forall <t> in sTime do
		vGRp[ t ] + vJPp[ t ] + vKEp[ t ] + vLNp[ t ] >= vLOAD[ t ];

minimize kSPILL:
	sum <t> in sTime do vMFo[ t ] +
	sum <t> in sTime do vGRs[ t ] +
	sum <t> in sTime do vJPs[ t ] +
	sum <t> in sTime do vKEs[ t ] +
	sum <t> in sTime do vLNs[ t ];
