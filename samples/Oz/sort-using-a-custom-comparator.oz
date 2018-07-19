declare
  fun {LexicographicLessThan Xs Ys}
     for
        X in {Map Xs Char.toLower}
        Y in {Map Ys Char.toLower}
        return:Return
        default:{Length Xs}<{Length Ys}
     do
        if X < Y then {Return true} end
     end
  end

  fun {LessThan Xs Ys}
     {Length Xs} > {Length Ys}
     orelse
     {Length Xs} == {Length Ys} andthen {LexicographicLessThan Xs Ys}
  end

  Strings = ["Here" "are" "some" "sample" "strings" "to" "be" "sorted"]
in
  {ForAll {Sort Strings LessThan} System.showInfo}
