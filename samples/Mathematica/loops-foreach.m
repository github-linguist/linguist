s = (StringSplit@Import["ExampleData/USConstitution.txt"])[[1;;7]];
Do[
 Print@i,
 {i, s}
]
