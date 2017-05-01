INTEGRATION_PRECISION_FACTOR = 5.0e-6;
END_OF_FILE = 0;
LIKELIHOOD_FUNCTION_OUTPUT = 5;
ACCEPT_BRANCH_LENGTHS = 1;
#include "/home/oashenbe/.local/lib/python2.7/site-packages/phyloExpCM/data//NTsCodonsAAs.ibf";
fprintf(stdout, "Running HYPHY script hyphy_cmds.bf...\n");
DataSet data = ReadDataFile("_codenames_Aligned_NPs_Swine.fasta");
assert(data.sites % 3 == 0, "Sequence lengths not multiples of 3");
totalcodons = data.sites $ 3;
fprintf(stdout, "Read from _codenames_Aligned_NPs_Swine.fasta a set of ", data.species, " sequences consisting of ", data.sites, " nucleotides corresponding to ", totalcodons, " codons each.\n");
fprintf(stdout, "The analysis will include the following 498 codon positions (sequential numbering starting with 1):\n1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 496, 497, 498\n");
assert(totalcodons >= 498, "Largest included site exceeds sequence length");
DataSetFilter codonfilter = CreateFilter(data, 3, "0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,505,506,507,508,509,510,511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,529,530,531,532,533,534,535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,553,554,555,556,557,558,559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600,601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624,625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648,649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672,673,674,675,676,677,678,679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696,697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720,721,722,723,724,725,726,727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744,745,746,747,748,749,750,751,752,753,754,755,756,757,758,759,760,761,762,763,764,765,766,767,768,769,770,771,772,773,774,775,776,777,778,779,780,781,782,783,784,785,786,787,788,789,790,791,792,793,794,795,796,797,798,799,800,801,802,803,804,805,806,807,808,809,810,811,812,813,814,815,816,817,818,819,820,821,822,823,824,825,826,827,828,829,830,831,832,833,834,835,836,837,838,839,840,841,842,843,844,845,846,847,848,849,850,851,852,853,854,855,856,857,858,859,860,861,862,863,864,865,866,867,868,869,870,871,872,873,874,875,876,877,878,879,880,881,882,883,884,885,886,887,888,889,890,891,892,893,894,895,896,897,898,899,900,901,902,903,904,905,906,907,908,909,910,911,912,913,914,915,916,917,918,919,920,921,922,923,924,925,926,927,928,929,930,931,932,933,934,935,936,937,938,939,940,941,942,943,944,945,946,947,948,949,950,951,952,953,954,955,956,957,958,959,960,961,962,963,964,965,966,967,968,969,970,971,972,973,974,975,976,977,978,979,980,981,982,983,984,985,986,987,988,989,990,991,992,993,994,995,996,997,998,999,1000,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011,1012,1013,1014,1015,1016,1017,1018,1019,1020,1021,1022,1023,1024,1025,1026,1027,1028,1029,1030,1031,1032,1033,1034,1035,1036,1037,1038,1039,1040,1041,1042,1043,1044,1045,1046,1047,1048,1049,1050,1051,1052,1053,1054,1055,1056,1057,1058,1059,1060,1061,1062,1063,1064,1065,1066,1067,1068,1069,1070,1071,1072,1073,1074,1075,1076,1077,1078,1079,1080,1081,1082,1083,1084,1085,1086,1087,1088,1089,1090,1091,1092,1093,1094,1095,1096,1097,1098,1099,1100,1101,1102,1103,1104,1105,1106,1107,1108,1109,1110,1111,1112,1113,1114,1115,1116,1117,1118,1119,1120,1121,1122,1123,1124,1125,1126,1127,1128,1129,1130,1131,1132,1133,1134,1135,1136,1137,1138,1139,1140,1141,1142,1143,1144,1145,1146,1147,1148,1149,1150,1151,1152,1153,1154,1155,1156,1157,1158,1159,1160,1161,1162,1163,1164,1165,1166,1167,1168,1169,1170,1171,1172,1173,1174,1175,1176,1177,1178,1179,1180,1181,1182,1183,1184,1185,1186,1187,1188,1189,1190,1191,1192,1193,1194,1195,1196,1197,1198,1199,1200,1201,1202,1203,1204,1205,1206,1207,1208,1209,1210,1211,1212,1213,1214,1215,1216,1217,1218,1219,1220,1221,1222,1223,1224,1225,1226,1227,1228,1229,1230,1231,1232,1233,1234,1235,1236,1237,1238,1239,1240,1241,1242,1243,1244,1245,1246,1247,1248,1249,1250,1251,1252,1253,1254,1255,1256,1257,1258,1259,1260,1261,1262,1263,1264,1265,1266,1267,1268,1269,1270,1271,1272,1273,1274,1275,1276,1277,1278,1279,1280,1281,1282,1283,1284,1285,1286,1287,1288,1289,1290,1291,1292,1293,1294,1295,1296,1297,1298,1299,1300,1301,1302,1303,1304,1305,1306,1307,1308,1309,1310,1311,1312,1313,1314,1315,1316,1317,1318,1319,1320,1321,1322,1323,1324,1325,1326,1327,1328,1329,1330,1331,1332,1333,1334,1335,1336,1337,1338,1339,1340,1341,1342,1343,1344,1345,1346,1347,1348,1349,1350,1351,1352,1353,1354,1355,1356,1357,1358,1359,1360,1361,1362,1363,1364,1365,1366,1367,1368,1369,1370,1371,1372,1373,1374,1375,1376,1377,1378,1379,1380,1381,1382,1383,1384,1385,1386,1387,1388,1389,1390,1391,1392,1393,1394,1395,1396,1397,1398,1399,1400,1401,1402,1403,1404,1405,1406,1407,1408,1409,1410,1411,1412,1413,1414,1415,1416,1417,1418,1419,1420,1421,1422,1423,1424,1425,1426,1427,1428,1429,1430,1431,1432,1433,1434,1435,1436,1437,1438,1439,1440,1441,1442,1443,1444,1445,1446,1447,1448,1449,1450,1451,1452,1453,1454,1455,1456,1457,1458,1459,1460,1461,1462,1463,1464,1465,1466,1467,1468,1469,1470,1471,1472,1473,1474,1475,1476,1477,1478,1479,1480,1481,1482,1483,1484,1485,1486,1487,1488,1489,1490,1491,1492,1493", "", "TAA,TAG,TGA");
assert(data.species == codonfilter.species, "species number mismatch");
assert(codonfilter.sites == 498, "Codon filtered data does not contain the right number of sites");
fprintf(stdout, "Created a codon filter of ", codonfilter.sites, " sites.\n");
assert(totalcodons - (totalcodons - 498) - 0 == codonfilter.sites, "Codon filtered data is not the expected length. Do sequences contain stop codons?");
CheckCodonFilter("codonfilter");
fprintf(stdout, "Reading tree string from _codenames_codonphyml_Swine_tree.newick.\n");
fscanf("_codenames_codonphyml_Swine_tree.newick", String, treestring);
fprintf(stdout, "Using the Goldman Yang 1994 (GY94) codon model...\n");
#include "/home/oashenbe/.local/lib/python2.7/site-packages/phyloExpCM/data//CF3x4.ibf";
#include "/home/oashenbe/.local/lib/python2.7/site-packages/phyloExpCM/data//GY94.ibf";
CreateGY94Model("CF3x4", "global", "global", 4, 4, 1);
UseModel(model);
ExecuteCommands("Tree tree = treestring;")
assert(codonfilter.species == TipCount(tree), "Number of species and number of tips differ");
LikelihoodFunction likelihood = (codonfilter, tree);
fprintf(stdout, "\nNow optimizing the likelihood function...\n");
Optimize(mlestimates, likelihood)
fprintf(stdout, "Completed likelihood optimization. Optimized ", mlestimates[1][1], " indpendent parameters and ", mlestimates[1][2], " shared parameters to obtain a log likelihood of ", mlestimates[1][0], ".\n");
fprintf(stdout, "Writing the results to hyphy_output.txt.\n");
fprintf("hyphy_output.txt", "Log likelihood: ", mlestimates[1][0], "\nindependent parameters (includes branch lengths): ", mlestimates[1][1], "\nshared parameters: ", mlestimates[1][2], "\nnumber of branch lengths: ", TipCount(tree) + BranchCount(tree), "\nnumber of tip nodes: ", TipCount(tree), "\nnumber of internal branches: ", BranchCount(tree), "\n",likelihood);
fprintf(stdout, "\nNow computing per-site likelihoods.\n");
fprintf(stdout, "\nFirst fixing all global variables to the maximum-likelihood values estimated on the entire tree.\n");
GetString(associativearray, likelihood, -1);
globalindependentvariables = associativearray["Global Independent"];
for (ivariable=0; ivariable<Columns(globalindependentvariables); ivariable=ivariable+1) {
  variable = globalindependentvariables[ivariable];
  cmdstring = variable + " := " + Format(variable, 0, 30) + ";";
  fprintf(stdout, "\nFixing variable as follows: ", cmdstring, "\n");
  ExecuteCommands(cmdstring);
}
persitelikelihoods = "sitelikelihoods.txt";
fprintf(stdout, "\nNow computing per-site likelihoods and writing to ", persitelikelihoods, "...\n");
fprintf(persitelikelihoods, "#SITE\tSITE_LOG_LIKELIHOODS\n");
fprintf(stdout, "\nComputing likelihood for site 1...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "0,1,2", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 1");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "1\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 2...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "3,4,5", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 2");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "2\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 3...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "6,7,8", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 3");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "3\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 4...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "9,10,11", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 4");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "4\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 5...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "12,13,14", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 5");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "5\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 6...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "15,16,17", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 6");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "6\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 7...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "18,19,20", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 7");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "7\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 8...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "21,22,23", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 8");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "8\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 9...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "24,25,26", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 9");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "9\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 10...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "27,28,29", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 10");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "10\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 11...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "30,31,32", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 11");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "11\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 12...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "33,34,35", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 12");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "12\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 13...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "36,37,38", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 13");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "13\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 14...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "39,40,41", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 14");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "14\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 15...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "42,43,44", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 15");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "15\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 16...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "45,46,47", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 16");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "16\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 17...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "48,49,50", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 17");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "17\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 18...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "51,52,53", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 18");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "18\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 19...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "54,55,56", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 19");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "19\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 20...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "57,58,59", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 20");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "20\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 21...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "60,61,62", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 21");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "21\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 22...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "63,64,65", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 22");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "22\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 23...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "66,67,68", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 23");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "23\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 24...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "69,70,71", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 24");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "24\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 25...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "72,73,74", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 25");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "25\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 26...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "75,76,77", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 26");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "26\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 27...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "78,79,80", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 27");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "27\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 28...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "81,82,83", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 28");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "28\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 29...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "84,85,86", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 29");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "29\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 30...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "87,88,89", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 30");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "30\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 31...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "90,91,92", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 31");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "31\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 32...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "93,94,95", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 32");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "32\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 33...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "96,97,98", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 33");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "33\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 34...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "99,100,101", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 34");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "34\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 35...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "102,103,104", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 35");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "35\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 36...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "105,106,107", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 36");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "36\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 37...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "108,109,110", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 37");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "37\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 38...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "111,112,113", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 38");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "38\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 39...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "114,115,116", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 39");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "39\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 40...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "117,118,119", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 40");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "40\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 41...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "120,121,122", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 41");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "41\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 42...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "123,124,125", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 42");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "42\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 43...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "126,127,128", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 43");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "43\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 44...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "129,130,131", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 44");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "44\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 45...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "132,133,134", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 45");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "45\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 46...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "135,136,137", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 46");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "46\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 47...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "138,139,140", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 47");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "47\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 48...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "141,142,143", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 48");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "48\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 49...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "144,145,146", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 49");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "49\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 50...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "147,148,149", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 50");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "50\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 51...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "150,151,152", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 51");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "51\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 52...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "153,154,155", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 52");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "52\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 53...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "156,157,158", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 53");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "53\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 54...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "159,160,161", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 54");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "54\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 55...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "162,163,164", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 55");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "55\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 56...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "165,166,167", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 56");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "56\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 57...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "168,169,170", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 57");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "57\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 58...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "171,172,173", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 58");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "58\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 59...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "174,175,176", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 59");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "59\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 60...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "177,178,179", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 60");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "60\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 61...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "180,181,182", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 61");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "61\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 62...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "183,184,185", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 62");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "62\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 63...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "186,187,188", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 63");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "63\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 64...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "189,190,191", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 64");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "64\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 65...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "192,193,194", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 65");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "65\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 66...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "195,196,197", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 66");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "66\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 67...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "198,199,200", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 67");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "67\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 68...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "201,202,203", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 68");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "68\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 69...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "204,205,206", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 69");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "69\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 70...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "207,208,209", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 70");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "70\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 71...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "210,211,212", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 71");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "71\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 72...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "213,214,215", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 72");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "72\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 73...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "216,217,218", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 73");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "73\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 74...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "219,220,221", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 74");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "74\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 75...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "222,223,224", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 75");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "75\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 76...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "225,226,227", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 76");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "76\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 77...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "228,229,230", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 77");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "77\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 78...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "231,232,233", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 78");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "78\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 79...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "234,235,236", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 79");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "79\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 80...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "237,238,239", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 80");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "80\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 81...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "240,241,242", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 81");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "81\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 82...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "243,244,245", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 82");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "82\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 83...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "246,247,248", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 83");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "83\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 84...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "249,250,251", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 84");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "84\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 85...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "252,253,254", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 85");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "85\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 86...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "255,256,257", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 86");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "86\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 87...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "258,259,260", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 87");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "87\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 88...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "261,262,263", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 88");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "88\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 89...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "264,265,266", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 89");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "89\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 90...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "267,268,269", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 90");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "90\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 91...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "270,271,272", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 91");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "91\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 92...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "273,274,275", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 92");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "92\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 93...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "276,277,278", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 93");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "93\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 94...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "279,280,281", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 94");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "94\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 95...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "282,283,284", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 95");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "95\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 96...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "285,286,287", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 96");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "96\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 97...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "288,289,290", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 97");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "97\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 98...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "291,292,293", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 98");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "98\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 99...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "294,295,296", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 99");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "99\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 100...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "297,298,299", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 100");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "100\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 101...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "300,301,302", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 101");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "101\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 102...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "303,304,305", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 102");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "102\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 103...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "306,307,308", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 103");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "103\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 104...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "309,310,311", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 104");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "104\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 105...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "312,313,314", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 105");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "105\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 106...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "315,316,317", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 106");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "106\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 107...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "318,319,320", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 107");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "107\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 108...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "321,322,323", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 108");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "108\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 109...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "324,325,326", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 109");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "109\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 110...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "327,328,329", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 110");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "110\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 111...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "330,331,332", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 111");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "111\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 112...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "333,334,335", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 112");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "112\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 113...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "336,337,338", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 113");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "113\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 114...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "339,340,341", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 114");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "114\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 115...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "342,343,344", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 115");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "115\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 116...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "345,346,347", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 116");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "116\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 117...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "348,349,350", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 117");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "117\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 118...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "351,352,353", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 118");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "118\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 119...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "354,355,356", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 119");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "119\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 120...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "357,358,359", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 120");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "120\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 121...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "360,361,362", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 121");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "121\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 122...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "363,364,365", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 122");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "122\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 123...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "366,367,368", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 123");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "123\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 124...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "369,370,371", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 124");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "124\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 125...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "372,373,374", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 125");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "125\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 126...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "375,376,377", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 126");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "126\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 127...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "378,379,380", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 127");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "127\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 128...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "381,382,383", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 128");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "128\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 129...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "384,385,386", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 129");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "129\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 130...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "387,388,389", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 130");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "130\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 131...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "390,391,392", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 131");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "131\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 132...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "393,394,395", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 132");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "132\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 133...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "396,397,398", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 133");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "133\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 134...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "399,400,401", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 134");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "134\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 135...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "402,403,404", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 135");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "135\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 136...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "405,406,407", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 136");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "136\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 137...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "408,409,410", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 137");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "137\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 138...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "411,412,413", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 138");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "138\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 139...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "414,415,416", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 139");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "139\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 140...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "417,418,419", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 140");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "140\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 141...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "420,421,422", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 141");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "141\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 142...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "423,424,425", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 142");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "142\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 143...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "426,427,428", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 143");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "143\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 144...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "429,430,431", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 144");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "144\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 145...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "432,433,434", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 145");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "145\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 146...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "435,436,437", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 146");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "146\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 147...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "438,439,440", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 147");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "147\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 148...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "441,442,443", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 148");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "148\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 149...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "444,445,446", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 149");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "149\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 150...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "447,448,449", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 150");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "150\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 151...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "450,451,452", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 151");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "151\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 152...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "453,454,455", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 152");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "152\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 153...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "456,457,458", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 153");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "153\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 154...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "459,460,461", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 154");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "154\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 155...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "462,463,464", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 155");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "155\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 156...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "465,466,467", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 156");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "156\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 157...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "468,469,470", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 157");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "157\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 158...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "471,472,473", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 158");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "158\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 159...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "474,475,476", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 159");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "159\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 160...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "477,478,479", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 160");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "160\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 161...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "480,481,482", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 161");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "161\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 162...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "483,484,485", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 162");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "162\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 163...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "486,487,488", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 163");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "163\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 164...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "489,490,491", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 164");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "164\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 165...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "492,493,494", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 165");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "165\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 166...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "495,496,497", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 166");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "166\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 167...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "498,499,500", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 167");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "167\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 168...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "501,502,503", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 168");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "168\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 169...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "504,505,506", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 169");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "169\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 170...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "507,508,509", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 170");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "170\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 171...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "510,511,512", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 171");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "171\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 172...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "513,514,515", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 172");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "172\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 173...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "516,517,518", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 173");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "173\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 174...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "519,520,521", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 174");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "174\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 175...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "522,523,524", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 175");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "175\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 176...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "525,526,527", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 176");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "176\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 177...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "528,529,530", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 177");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "177\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 178...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "531,532,533", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 178");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "178\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 179...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "534,535,536", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 179");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "179\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 180...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "537,538,539", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 180");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "180\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 181...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "540,541,542", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 181");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "181\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 182...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "543,544,545", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 182");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "182\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 183...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "546,547,548", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 183");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "183\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 184...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "549,550,551", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 184");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "184\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 185...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "552,553,554", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 185");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "185\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 186...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "555,556,557", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 186");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "186\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 187...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "558,559,560", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 187");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "187\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 188...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "561,562,563", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 188");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "188\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 189...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "564,565,566", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 189");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "189\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 190...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "567,568,569", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 190");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "190\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 191...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "570,571,572", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 191");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "191\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 192...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "573,574,575", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 192");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "192\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 193...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "576,577,578", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 193");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "193\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 194...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "579,580,581", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 194");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "194\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 195...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "582,583,584", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 195");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "195\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 196...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "585,586,587", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 196");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "196\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 197...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "588,589,590", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 197");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "197\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 198...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "591,592,593", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 198");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "198\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 199...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "594,595,596", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 199");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "199\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 200...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "597,598,599", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 200");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "200\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 201...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "600,601,602", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 201");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "201\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 202...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "603,604,605", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 202");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "202\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 203...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "606,607,608", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 203");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "203\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 204...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "609,610,611", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 204");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "204\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 205...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "612,613,614", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 205");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "205\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 206...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "615,616,617", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 206");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "206\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 207...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "618,619,620", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 207");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "207\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 208...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "621,622,623", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 208");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "208\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 209...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "624,625,626", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 209");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "209\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 210...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "627,628,629", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 210");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "210\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 211...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "630,631,632", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 211");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "211\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 212...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "633,634,635", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 212");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "212\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 213...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "636,637,638", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 213");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "213\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 214...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "639,640,641", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 214");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "214\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 215...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "642,643,644", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 215");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "215\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 216...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "645,646,647", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 216");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "216\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 217...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "648,649,650", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 217");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "217\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 218...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "651,652,653", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 218");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "218\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 219...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "654,655,656", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 219");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "219\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 220...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "657,658,659", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 220");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "220\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 221...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "660,661,662", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 221");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "221\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 222...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "663,664,665", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 222");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "222\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 223...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "666,667,668", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 223");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "223\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 224...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "669,670,671", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 224");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "224\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 225...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "672,673,674", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 225");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "225\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 226...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "675,676,677", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 226");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "226\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 227...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "678,679,680", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 227");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "227\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 228...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "681,682,683", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 228");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "228\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 229...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "684,685,686", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 229");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "229\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 230...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "687,688,689", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 230");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "230\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 231...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "690,691,692", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 231");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "231\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 232...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "693,694,695", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 232");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "232\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 233...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "696,697,698", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 233");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "233\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 234...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "699,700,701", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 234");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "234\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 235...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "702,703,704", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 235");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "235\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 236...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "705,706,707", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 236");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "236\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 237...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "708,709,710", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 237");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "237\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 238...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "711,712,713", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 238");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "238\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 239...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "714,715,716", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 239");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "239\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 240...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "717,718,719", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 240");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "240\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 241...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "720,721,722", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 241");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "241\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 242...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "723,724,725", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 242");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "242\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 243...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "726,727,728", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 243");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "243\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 244...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "729,730,731", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 244");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "244\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 245...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "732,733,734", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 245");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "245\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 246...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "735,736,737", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 246");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "246\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 247...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "738,739,740", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 247");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "247\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 248...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "741,742,743", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 248");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "248\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 249...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "744,745,746", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 249");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "249\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 250...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "747,748,749", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 250");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "250\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 251...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "750,751,752", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 251");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "251\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 252...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "753,754,755", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 252");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "252\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 253...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "756,757,758", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 253");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "253\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 254...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "759,760,761", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 254");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "254\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 255...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "762,763,764", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 255");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "255\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 256...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "765,766,767", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 256");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "256\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 257...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "768,769,770", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 257");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "257\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 258...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "771,772,773", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 258");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "258\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 259...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "774,775,776", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 259");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "259\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 260...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "777,778,779", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 260");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "260\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 261...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "780,781,782", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 261");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "261\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 262...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "783,784,785", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 262");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "262\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 263...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "786,787,788", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 263");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "263\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 264...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "789,790,791", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 264");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "264\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 265...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "792,793,794", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 265");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "265\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 266...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "795,796,797", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 266");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "266\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 267...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "798,799,800", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 267");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "267\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 268...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "801,802,803", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 268");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "268\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 269...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "804,805,806", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 269");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "269\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 270...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "807,808,809", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 270");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "270\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 271...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "810,811,812", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 271");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "271\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 272...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "813,814,815", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 272");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "272\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 273...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "816,817,818", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 273");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "273\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 274...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "819,820,821", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 274");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "274\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 275...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "822,823,824", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 275");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "275\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 276...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "825,826,827", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 276");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "276\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 277...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "828,829,830", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 277");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "277\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 278...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "831,832,833", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 278");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "278\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 279...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "834,835,836", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 279");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "279\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 280...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "837,838,839", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 280");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "280\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 281...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "840,841,842", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 281");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "281\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 282...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "843,844,845", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 282");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "282\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 283...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "846,847,848", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 283");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "283\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 284...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "849,850,851", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 284");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "284\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 285...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "852,853,854", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 285");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "285\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 286...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "855,856,857", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 286");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "286\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 287...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "858,859,860", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 287");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "287\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 288...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "861,862,863", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 288");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "288\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 289...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "864,865,866", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 289");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "289\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 290...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "867,868,869", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 290");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "290\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 291...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "870,871,872", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 291");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "291\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 292...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "873,874,875", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 292");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "292\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 293...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "876,877,878", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 293");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "293\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 294...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "879,880,881", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 294");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "294\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 295...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "882,883,884", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 295");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "295\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 296...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "885,886,887", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 296");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "296\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 297...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "888,889,890", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 297");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "297\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 298...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "891,892,893", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 298");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "298\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 299...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "894,895,896", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 299");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "299\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 300...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "897,898,899", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 300");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "300\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 301...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "900,901,902", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 301");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "301\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 302...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "903,904,905", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 302");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "302\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 303...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "906,907,908", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 303");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "303\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 304...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "909,910,911", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 304");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "304\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 305...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "912,913,914", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 305");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "305\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 306...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "915,916,917", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 306");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "306\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 307...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "918,919,920", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 307");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "307\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 308...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "921,922,923", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 308");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "308\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 309...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "924,925,926", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 309");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "309\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 310...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "927,928,929", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 310");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "310\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 311...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "930,931,932", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 311");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "311\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 312...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "933,934,935", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 312");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "312\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 313...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "936,937,938", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 313");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "313\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 314...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "939,940,941", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 314");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "314\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 315...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "942,943,944", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 315");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "315\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 316...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "945,946,947", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 316");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "316\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 317...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "948,949,950", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 317");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "317\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 318...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "951,952,953", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 318");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "318\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 319...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "954,955,956", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 319");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "319\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 320...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "957,958,959", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 320");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "320\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 321...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "960,961,962", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 321");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "321\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 322...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "963,964,965", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 322");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "322\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 323...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "966,967,968", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 323");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "323\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 324...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "969,970,971", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 324");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "324\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 325...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "972,973,974", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 325");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "325\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 326...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "975,976,977", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 326");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "326\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 327...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "978,979,980", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 327");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "327\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 328...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "981,982,983", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 328");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "328\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 329...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "984,985,986", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 329");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "329\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 330...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "987,988,989", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 330");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "330\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 331...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "990,991,992", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 331");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "331\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 332...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "993,994,995", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 332");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "332\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 333...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "996,997,998", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 333");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "333\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 334...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "999,1000,1001", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 334");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "334\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 335...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1002,1003,1004", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 335");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "335\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 336...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1005,1006,1007", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 336");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "336\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 337...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1008,1009,1010", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 337");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "337\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 338...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1011,1012,1013", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 338");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "338\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 339...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1014,1015,1016", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 339");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "339\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 340...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1017,1018,1019", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 340");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "340\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 341...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1020,1021,1022", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 341");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "341\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 342...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1023,1024,1025", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 342");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "342\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 343...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1026,1027,1028", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 343");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "343\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 344...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1029,1030,1031", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 344");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "344\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 345...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1032,1033,1034", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 345");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "345\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 346...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1035,1036,1037", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 346");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "346\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 347...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1038,1039,1040", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 347");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "347\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 348...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1041,1042,1043", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 348");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "348\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 349...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1044,1045,1046", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 349");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "349\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 350...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1047,1048,1049", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 350");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "350\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 351...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1050,1051,1052", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 351");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "351\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 352...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1053,1054,1055", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 352");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "352\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 353...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1056,1057,1058", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 353");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "353\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 354...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1059,1060,1061", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 354");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "354\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 355...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1062,1063,1064", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 355");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "355\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 356...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1065,1066,1067", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 356");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "356\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 357...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1068,1069,1070", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 357");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "357\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 358...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1071,1072,1073", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 358");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "358\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 359...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1074,1075,1076", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 359");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "359\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 360...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1077,1078,1079", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 360");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "360\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 361...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1080,1081,1082", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 361");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "361\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 362...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1083,1084,1085", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 362");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "362\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 363...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1086,1087,1088", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 363");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "363\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 364...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1089,1090,1091", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 364");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "364\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 365...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1092,1093,1094", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 365");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "365\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 366...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1095,1096,1097", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 366");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "366\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 367...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1098,1099,1100", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 367");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "367\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 368...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1101,1102,1103", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 368");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "368\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 369...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1104,1105,1106", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 369");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "369\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 370...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1107,1108,1109", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 370");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "370\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 371...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1110,1111,1112", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 371");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "371\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 372...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1113,1114,1115", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 372");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "372\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 373...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1116,1117,1118", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 373");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "373\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 374...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1119,1120,1121", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 374");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "374\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 375...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1122,1123,1124", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 375");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "375\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 376...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1125,1126,1127", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 376");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "376\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 377...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1128,1129,1130", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 377");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "377\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 378...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1131,1132,1133", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 378");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "378\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 379...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1134,1135,1136", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 379");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "379\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 380...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1137,1138,1139", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 380");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "380\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 381...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1140,1141,1142", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 381");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "381\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 382...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1143,1144,1145", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 382");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "382\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 383...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1146,1147,1148", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 383");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "383\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 384...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1149,1150,1151", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 384");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "384\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 385...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1152,1153,1154", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 385");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "385\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 386...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1155,1156,1157", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 386");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "386\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 387...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1158,1159,1160", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 387");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "387\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 388...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1161,1162,1163", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 388");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "388\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 389...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1164,1165,1166", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 389");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "389\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 390...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1167,1168,1169", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 390");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "390\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 391...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1170,1171,1172", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 391");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "391\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 392...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1173,1174,1175", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 392");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "392\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 393...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1176,1177,1178", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 393");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "393\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 394...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1179,1180,1181", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 394");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "394\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 395...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1182,1183,1184", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 395");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "395\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 396...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1185,1186,1187", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 396");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "396\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 397...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1188,1189,1190", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 397");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "397\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 398...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1191,1192,1193", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 398");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "398\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 399...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1194,1195,1196", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 399");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "399\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 400...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1197,1198,1199", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 400");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "400\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 401...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1200,1201,1202", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 401");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "401\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 402...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1203,1204,1205", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 402");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "402\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 403...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1206,1207,1208", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 403");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "403\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 404...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1209,1210,1211", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 404");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "404\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 405...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1212,1213,1214", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 405");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "405\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 406...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1215,1216,1217", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 406");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "406\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 407...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1218,1219,1220", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 407");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "407\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 408...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1221,1222,1223", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 408");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "408\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 409...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1224,1225,1226", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 409");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "409\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 410...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1227,1228,1229", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 410");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "410\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 411...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1230,1231,1232", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 411");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "411\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 412...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1233,1234,1235", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 412");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "412\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 413...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1236,1237,1238", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 413");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "413\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 414...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1239,1240,1241", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 414");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "414\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 415...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1242,1243,1244", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 415");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "415\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 416...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1245,1246,1247", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 416");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "416\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 417...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1248,1249,1250", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 417");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "417\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 418...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1251,1252,1253", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 418");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "418\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 419...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1254,1255,1256", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 419");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "419\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 420...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1257,1258,1259", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 420");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "420\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 421...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1260,1261,1262", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 421");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "421\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 422...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1263,1264,1265", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 422");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "422\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 423...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1266,1267,1268", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 423");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "423\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 424...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1269,1270,1271", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 424");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "424\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 425...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1272,1273,1274", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 425");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "425\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 426...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1275,1276,1277", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 426");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "426\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 427...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1278,1279,1280", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 427");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "427\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 428...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1281,1282,1283", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 428");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "428\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 429...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1284,1285,1286", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 429");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "429\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 430...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1287,1288,1289", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 430");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "430\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 431...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1290,1291,1292", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 431");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "431\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 432...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1293,1294,1295", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 432");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "432\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 433...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1296,1297,1298", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 433");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "433\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 434...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1299,1300,1301", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 434");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "434\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 435...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1302,1303,1304", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 435");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "435\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 436...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1305,1306,1307", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 436");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "436\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 437...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1308,1309,1310", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 437");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "437\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 438...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1311,1312,1313", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 438");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "438\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 439...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1314,1315,1316", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 439");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "439\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 440...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1317,1318,1319", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 440");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "440\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 441...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1320,1321,1322", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 441");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "441\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 442...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1323,1324,1325", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 442");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "442\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 443...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1326,1327,1328", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 443");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "443\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 444...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1329,1330,1331", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 444");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "444\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 445...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1332,1333,1334", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 445");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "445\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 446...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1335,1336,1337", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 446");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "446\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 447...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1338,1339,1340", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 447");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "447\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 448...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1341,1342,1343", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 448");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "448\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 449...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1344,1345,1346", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 449");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "449\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 450...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1347,1348,1349", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 450");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "450\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 451...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1350,1351,1352", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 451");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "451\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 452...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1353,1354,1355", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 452");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "452\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 453...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1356,1357,1358", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 453");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "453\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 454...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1359,1360,1361", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 454");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "454\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 455...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1362,1363,1364", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 455");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "455\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 456...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1365,1366,1367", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 456");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "456\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 457...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1368,1369,1370", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 457");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "457\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 458...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1371,1372,1373", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 458");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "458\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 459...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1374,1375,1376", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 459");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "459\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 460...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1377,1378,1379", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 460");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "460\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 461...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1380,1381,1382", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 461");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "461\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 462...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1383,1384,1385", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 462");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "462\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 463...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1386,1387,1388", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 463");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "463\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 464...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1389,1390,1391", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 464");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "464\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 465...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1392,1393,1394", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 465");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "465\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 466...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1395,1396,1397", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 466");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "466\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 467...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1398,1399,1400", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 467");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "467\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 468...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1401,1402,1403", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 468");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "468\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 469...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1404,1405,1406", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 469");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "469\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 470...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1407,1408,1409", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 470");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "470\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 471...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1410,1411,1412", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 471");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "471\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 472...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1413,1414,1415", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 472");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "472\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 473...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1416,1417,1418", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 473");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "473\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 474...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1419,1420,1421", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 474");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "474\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 475...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1422,1423,1424", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 475");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "475\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 476...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1425,1426,1427", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 476");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "476\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 477...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1428,1429,1430", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 477");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "477\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 478...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1431,1432,1433", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 478");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "478\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 479...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1434,1435,1436", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 479");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "479\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 480...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1437,1438,1439", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 480");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "480\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 481...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1440,1441,1442", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 481");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "481\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 482...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1443,1444,1445", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 482");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "482\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 483...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1446,1447,1448", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 483");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "483\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 484...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1449,1450,1451", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 484");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "484\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 485...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1452,1453,1454", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 485");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "485\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 486...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1455,1456,1457", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 486");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "486\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 487...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1458,1459,1460", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 487");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "487\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 488...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1461,1462,1463", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 488");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "488\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 489...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1464,1465,1466", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 489");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "489\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 490...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1467,1468,1469", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 490");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "490\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 491...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1470,1471,1472", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 491");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "491\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 492...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1473,1474,1475", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 492");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "492\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 493...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1476,1477,1478", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 493");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "493\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 494...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1479,1480,1481", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 494");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "494\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 495...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1482,1483,1484", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 495");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "495\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 496...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1485,1486,1487", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 496");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "496\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 497...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1488,1489,1490", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 497");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "497\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "\nComputing likelihood for site 498...\n");
DataSetFilter sitecodonfilter = CreateFilter(data, 3, "1491,1492,1493", "", "TAA,TAG,TGA");
assert(sitecodonfilter.sites == 1, "Codon filtered data does not have one site for 498");
CheckCodonFilter("sitecodonfilter");
UseModel(model);
ExecuteCommands("Tree sitetree = treestring;");
assert(sitecodonfilter.species == TipCount(sitetree), "Number of species and number of tips differ");
assert(TipCount(tree) == TipCount(sitetree), "Number of tips differ");
for (ibranch=0; ibranch<BranchCount(tree); ibranch=ibranch+1) {
  branchname = BranchName(tree, ibranch);
  ExecuteCommands("branchlength = tree." + branchname + ".t;");
  ExecuteCommands("sitetree." + branchname + ".t := " + Format(branchlength, 0, 30) + ";");
}
for (itip=0; itip<TipCount(tree); itip=itip+1) {
  tipname = TipName(tree, itip);
  ExecuteCommands("tiplength = tree." + tipname + ".t;");
  ExecuteCommands("sitetree." + tipname + ".t := " + Format(tiplength, 0, 30) + ";");
}
LikelihoodFunction sitelikelihood = (sitecodonfilter, sitetree);
Optimize(sitemlestimates, sitelikelihood);
assert(sitemlestimates[1][1] == 0, "Found a variable optimized. Either a model or branch parameter must have not been fixed");
fprintf(persitelikelihoods, "498\t", sitemlestimates[1][0], "\n");
fprintf(stdout, "Completed HYPHY script hyphy_cmds.bf.\n");