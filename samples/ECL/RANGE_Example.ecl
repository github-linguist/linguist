r := {STRING1 Letter};
ds1 := DATASET([{'A'}, {'B'}, {'C'}, {'D'}, {'E'}], r);
ds2 := DATASET([{'F'}, {'G'}, {'H'}, {'I'}, {'J'}], r);
ds3 := DATASET([{'K'}, {'L'}, {'M'}, {'N'}, {'O'}], r);
ds4 := DATASET([{'P'}, {'Q'}, {'R'}, {'S'}, {'T'}], r);
ds5 := DATASET([{'U'}, {'V'}, {'W'}, {'X'}, {'Y'}], r);

SetDS := [ds1, ds2, ds3, ds4, ds5];
outDS := RANGE(setDS, [1, 3]);  //use only 1st and 3rd elements

OUTPUT(outDS[1]);               //results in A, B, C, D, E
OUTPUT(outDS[2]);               //results in K, L, M, N, O
