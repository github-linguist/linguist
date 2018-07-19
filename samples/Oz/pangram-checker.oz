declare
  fun {IsPangram Xs}
     {List.sub
      {List.number &a &z 1}
      {Sort {Map Xs Char.toLower} Value.'<'}}
  end
in
  {Show {IsPangram "The quick brown fox jumps over the lazy dog."}}
