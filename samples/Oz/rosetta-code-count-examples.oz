declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [XMLParser] = {Module.link ['x-oz://system/xml/Parser.ozf']}
  [StringX] = {Module.link ['x-oz://system/String.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  AllTasksUrl = "http://rosettacode.org/mw/api.php?action=query&list="#
  "categorymembers&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml"

  proc {Main}
     AllTasks = {Parse {GetPage AllTasksUrl}}
     TaskTitles = {GetTitles AllTasks}
     Total = {NewCell 0}
  in
     for Task in TaskTitles do
        TaskPage = {GetPage {TaskUrl Task}}
        RE = {Regex.compile "{{header\\|" [extended newline icase]}
        NumMatches = {Length {Regex.allMatches RE TaskPage}}
     in
        {System.showInfo Task#": "#NumMatches#" examples."}
        Total := @Total + NumMatches
     end
     {System.showInfo "Total: "#@Total#" examples."}
  end

  fun {TaskUrl Task}
     "http://rosettacode.org/mw/index.php?"#
     "title="#{PercentEncode {StringX.replace Task " " "_"}}#
     "&action=raw"
  end

  %% GetPage
  local
     Client = {New HTTPClient.urlGET init(inPrms(toFile:false toStrm:true) _)}
  in
     fun {GetPage RawUrl}
        Url = {VirtualString.toString RawUrl}
        OutParams
        HttpResponseParams
     in
        {Client getService(Url ?OutParams ?HttpResponseParams)}
        OutParams.sOut
     end
  end

  %% Parse
  local
     Parser = {New XMLParser.parser init}
  in
     fun {Parse Xs} {Parser parseVS(Xs $)} end
  end

  fun {GetTitles Doc}
     CMs = Doc.2.1.children.1.children.1.children
     fun {Attributes element(attributes:As ...)} As end
     fun {IsTitle attribute(name:N ...)} N == title end
  in
     {Map {Filter {Flatten {Map CMs Attributes}} IsTitle}
      fun {$ A} {Atom.toString A.value} end}
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
in
  {Main}
