f=Function[n,
	Most/@(Range@@@Partition[FindSequenceFunction[{1,2,4,7,11}]/@Range[n+1],2,1])]
TableForm[f@5,TableAlignments->Right,TableSpacing->{1,1}]
TableForm[f@14,TableAlignments->Right,TableSpacing->{1,1}]
