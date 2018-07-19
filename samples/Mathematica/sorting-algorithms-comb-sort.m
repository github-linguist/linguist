combSort[list_] := Module[{ gap = 0, listSize = 0, swaps = True},
        gap = listSize = Length[list];
        While[ !((gap <= 1) && (swaps == False)),

            gap = Floor@Divide[gap, 1.25];
            If[ gap < 1, gap = 1]; i = 1; swaps = False;

            While[ ! ((i + gap - 1) >= listSize),
                If[ list[[i]] > list[[i + gap]], swaps = True;
                list[[i ;; i + gap]] = list[[i + gap ;; i ;; -1]];
                ];
            i++;
            ]
        ]
]
