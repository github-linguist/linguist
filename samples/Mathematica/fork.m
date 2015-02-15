commandstring =  First[$CommandLine] <> " -noprompt -run \"Put[Factorial[20],ToFileName[$TemporaryDirectory,ToString[temp1]]];Quit[]\""
->"MathKernel -noprompt -run \"Put[Factorial[20],ToFileName[$TemporaryDirectory,ToString[temp1]]];Quit[]\""

Run[commandstring]
->0
