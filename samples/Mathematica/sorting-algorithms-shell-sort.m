shellSort[ lst_ ] := Module[ {list = lst, incr, temp, i, j},
 incr = Round[Length[list]/2];
 While[incr > 0,

  For[i = incr + 1, i < Length[list], i++,

   temp = list[[i]]; j = i;

   While[(j >= (incr + 1)) && (list[[j - incr]] > temp) ,
    list[[j]] = list[[j - incr]];  j = j-incr;
   ];

   list[[j]] = temp;];
   If[incr == 2, incr = 1, incr = Round[incr/2.2]]
];  list
]
