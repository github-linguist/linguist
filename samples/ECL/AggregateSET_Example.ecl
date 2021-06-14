SetStuff := [1,2,3,4,5];

COUNT(SetStuff);    //result is 5
SUM(SetStuff);      //result is 15
MIN(SetStuff);      //result is 1
MAX(SetStuff);      //result is 5
AVE(SetStuff);      //result is 3.0


//******************************************************
ds := DATASET([{5,1},{4,2},{3,3},{2,4},{1,5}],
              {UNSIGNED1 Nbr1, UNSIGNED1 Nbr2});

COUNT(ds);              //result is 5
SUM(ds,Nbr1 + Nbr2);    //result is 30
MIN(ds,Nbr1 + Nbr2);    //result is 6
MAX(ds,Nbr1 + Nbr2);    //result is 6
AVE(ds,Nbr1 + Nbr2);    //result is 6.0
