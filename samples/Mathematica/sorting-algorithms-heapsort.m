siftDown[list_,root_,theEnd_]:=
 While[(root*2) <= theEnd,
  child = root*2;
  If[(child+1 <= theEnd)&&(list[[child]] < list[[child+1]]), child++;];
  If[list[[root]] < list[[child]],
   list[[{root,child}]] = list[[{child,root}]]; root = child;,
   Break[];
  ]
 ]

heapSort[list_] := Module[{ count, start},
 count = Length[list]; start = Floor[count/2];
 While[start >= 1,list = siftDown[list,start,count];
  start--;
 ]
 While[count > 1, list[[{count,1}]] = list[[{1,count}]];
  count--; list = siftDown[list,1,count];
 ]
]
