declare
  local
     Cache = {Dictionary.new}
     Cache.1 := 1
     Cache.2 := 1
  in
     fun {A N}
        if {Not {Dictionary.member Cache N}} then
           Cache.N := {A {A N-1}} + {A N-{A N-1}}
        end
        Cache.N
     end
  end

  Float = Int.toFloat

  for I in 0..19 do
     Range = {List.number {Pow 2 I} {Pow 2 I+1} 1}
     RelativeValues = {Map Range
                       fun {$ N}
                          {Float {A N}}
                          / {Float N}
                       end}
     Maximum = {FoldL RelativeValues Max 0.0}
  in
     {System.showInfo "Max. between 2^"#I#" and 2^"#I+1#": "#Maximum}
  end
