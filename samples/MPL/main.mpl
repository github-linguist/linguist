"CommandLine" use
"String"      use
"algorithm"   use
"control"     use
"file"        use

"format" use

{argc: Int32; argv: Natx;} Int32 {} [
  commands: toCommandLine2;
  [commands.size 2 =] "Usage:\n  program <source>" ensure

  print: [([isCombined] [String same ~] [StringView same ~]) meetsAll] [printList] pfunc overload;

  filename: 1 commands.at;
  input:  filename loadString; [input.success    ] ("Unable to load file \"" filename "\""   LF) ensure
  output: input.data format;   [output.error "" =] ("Unable to process input: " output.error LF) ensure

  input.data output.data = ~ [
    error: output.data filename saveFile;
    [error "" =] ("Unable to store the file: " error) ensure
  ] when

  0
] "main" exportFunction 