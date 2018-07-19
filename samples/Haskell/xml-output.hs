import Text.XML.Light

characterRemarks :: [String] -> [String] -> String
characterRemarks names remarks = showElement $ Element
    (unqual "CharacterRemarks")
    []
    (zipWith character names remarks)
    Nothing
  where character name remark = Elem $ Element
            (unqual "Character")
            [Attr (unqual "name") name]
            [Text $ CData CDataText remark Nothing]
            Nothing
