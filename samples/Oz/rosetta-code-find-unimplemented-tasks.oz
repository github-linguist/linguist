declare
  [HTTPClient] = {Link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [XMLParser] = {Link ['x-oz://system/xml/Parser.ozf']}

  fun {FindUnimplementedTasks Language}
     AllTasks = {FindCategory "Programming Tasks"}
     LangTasks = {FindCategory Language}
  in
     {ListDiff AllTasks LangTasks}
  end

  fun {FindCategory Cat}
     CatUrl = "http://www.rosettacode.org/mw/api.php?action=query"
     #"&list=categorymembers"
     #"&cmtitle=Category:"#{PercentEncode Cat}
     #"&cmlimit=500&format=xml"

     fun {Loop CMContinue}
        [_ Doc] = {Parse {GetPage CatUrl#CMContinue}}
        Titles = {XPath Doc
                  [api query categorymembers cm {Attribute title}]}
     in
        case {XPath Doc
              [api 'query-continue' categorymembers {Attribute cmcontinue}]}
        of nil then Titles
        [] [NewCMContinueAtom] then
           NewCMContinue = {PercentEncode {Atom.toString NewCMContinueAtom}}
        in
           {Append Titles
            {Loop "&cmcontinue="#NewCMContinue}}
        end
     end
  in
     {Loop nil}
  end


  %% XPath emulation
  fun {XPath Doc Path}
     P|Pr = Path
  in
     Doc.name = P %% assert
     {FoldL Pr XPathStep [Doc]}
  end

  Nothing = {NewName}
  fun {NotNothing X} X \= Nothing end

  fun {XPathStep Elements P}
     if {Atom.is P} then
        {FilteredChildren Elements P}
     elseif {Procedure.is P} then
        {Filter {Map Elements P} NotNothing}
     end
  end

  %% A flat list of all Type-children of all Elements.
  fun {FilteredChildren Elements Type}
     {Flatten
      {Map Elements
       fun {$ E}
          {Filter E.children
           fun {$ X}
              case X of element(name:!Type ...) then true
              else false
              end
           end}
       end}}
  end

  fun {Attribute Attr}
     fun {$ Element}
        case {Filter Element.attributes fun {$ A} A.name == Attr end}
        of [A] then A.value
        else Nothing
        end
     end
  end


  %% GetPage
  Client = {New HTTPClient.urlGET init(inPrms(toFile:false toStrm:true) _)}
  fun {GetPage RawUrl}
     Url = {VirtualString.toString RawUrl}
     OutParams
  in
     {Client getService(Url ?OutParams ?_)}
     OutParams.sOut
  end

  fun {PercentEncode Xs}
     case Xs of nil then nil
     [] X|Xr then
        if {Char.isDigit X} orelse {Member X [&- &_ &.  &~]}
           orelse X >= &a andthen X =< &z
           orelse X >= &z andthen X =< &Z then
           X|{PercentEncode Xr}
        else
           {Append &%|{ToHex2 X} {PercentEncode Xr}}
        end
     end
  end

  fun {ToHex2 X}
     [{ToHex1 X div 16} {ToHex1 X mod 16}]
  end

  fun {ToHex1 X}
     if X >= 0 andthen X =< 9 then &0 + X
     elseif X >= 10 andthen X =< 15 then &A + X - 10
     end
  end


  %% Parse
  local
     Parser = {New XMLParser.parser init}
  in
     fun {Parse Xs} {Parser parseVS(Xs $)} end
  end

  fun {ListDiff Xs Ys}
     {FoldL Ys List.subtract Xs}
  end
in
  %% show tasks not implemented in Oz
  {ForAll {FindUnimplementedTasks "Oz"} System.showInfo}
