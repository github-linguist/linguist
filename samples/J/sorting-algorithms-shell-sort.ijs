gaps      =: [: }: 1 (1+3*])^:(> {:)^:a:~ #
insert    =: (I.~ {. ]) , [ , ] }.~ I.~
gapinss   =: #@] {. ,@|:@(] insert//.~ #@] $ i.@[)
shellSort =: [: ; gapinss &.>/@(< ,~ ]&.>@gaps)
