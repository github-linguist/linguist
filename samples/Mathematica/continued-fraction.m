sqrt2=Function[n,{1,Transpose@{Array[2&,n],Array[1&,n]}}];
napier=Function[n,{2,Transpose@{Range[n],Prepend[Range[n-1],1]}}];
pi=Function[n,{3,Transpose@{Array[6&,n],Array[(2#-1)^2&,n]}}];
approx=Function[l,
	N[Divide@@First@Fold[{{#2.#[[;;,1]],#2.#[[;;,2]]},#[[1]]}&,{{l[[2,1,1]]l[[1]]+l[[2,1,2]],l[[2,1,1]]},{l[[1]],1}},l[[2,2;;]]],10]];
r2=approx/@{sqrt2@#,napier@#,pi@#}&@10000;r2//TableForm
