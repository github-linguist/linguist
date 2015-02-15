HammingList[N_] := Module[{A, B, C}, {A, B, C} = (N^(1/3))*{2.8054745679851933, 1.7700573778298891, 1.2082521307023026} - {1, 1, 1};
 Take[ Sort@Flatten@Table[ 2^x * 3^y * 5^z ,
{x, 0, A}, {y, 0, (-B/A)*x + B}, {z, 0, C - (C/A)*x - (C/B)*y}], N]];
