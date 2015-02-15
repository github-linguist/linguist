keys=DownValues[#,Sort->False][[All,1,1,1]]&;
hashes=#/@keys[#]&;

a[2]="string";a["sometext"]=23;
keys[a]
->{2,sometext}
hashes[a]
->{string,23}
