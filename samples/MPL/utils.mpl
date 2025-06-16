"Array"     use
"Span"      use
"String"    use
"algorithm" use
"control"   use

+=: [
  x: y:;;
  @y @x + @y set
];

-=: [
  x: y:;;
  @y @x - @y set
];

last: [isBuiltinTuple] [
  source:;
  source fieldCount 1 - @source @
] pfunc;

batchIter: [{
  private batchSize: new;
  private source: toIter;

  next: [
    valid: TRUE;
    (batchSize [@source.next !valid] times) valid
  ];
}];

removeTabs: [
  text:;
  result: String;

  text [9n8 = ~] filter [
    unit:;
    (unit 1) toStringView @result.cat
  ] each

  result
];

removeTrailingSpace: [
  text:;
  count: text toSpan .iterReverse [32n8 = ~] findOrdinal;
  count -1 = [StringView] [text count untail] if
];

longestLen: [
  strings:;
  res: 0;
  strings [
    str:;
    str count res > [str count !res] when
  ] each
  res
];

smallestStringIdx: [
  strings:;
  idx: 0;
  strings 0 enumerate [
    i: string: unwrap;;
    string idx strings.at < [i new !idx] when
  ] each
  idx
];

sorted: [
  strings:;
  res: String Array;

  strings count [
    idx: strings smallestStringIdx;
    idx strings.at @res.append
    idx @strings.erase
  ] times
  res
];

normalizeImportNames: [
  importNames: comments:;;

  paddedSize: importNames longestLen;

  (importNames comments) wrapIter [
    name: comment: unwrap;;
    res: String;
    name @res.cat
    paddedSize name count - [
      " " @res.cat
    ] times
    " use" @res.cat
    comment "" = ~ [
      " " @res.cat
      comment removeTabs removeTrailingSpace @res.cat
    ] when
    res
  ] [String] map toArray sorted
]; 