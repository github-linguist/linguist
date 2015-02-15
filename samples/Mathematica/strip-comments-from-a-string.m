a = "apples, pears # and bananas
  apples, pears ; and bananas";
b = StringReplace[a, RegularExpression["[ ]+[#;].+[\n]"] -> "\n"];
StringReplace[b, RegularExpression["[ ]+[#;].+$"] -> ""] // FullForm
