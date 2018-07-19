declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  fun {GetPage RawUrl}
     Client = {New HTTPClient.urlGET init(inPrms(toFile:false toStrm:true) _)}
     Url = {VirtualString.toString RawUrl}
     OutParams
     HttpResponseParams
  in
     {Client getService(Url ?OutParams ?HttpResponseParams)}
     {Client closeAll(true)}
     OutParams.sOut
  end

  fun {GetCategories Doc}
     {Map {Regex.allMatches "<li><a[^>]+>([^<]+)</a> \\(([0-9]+) member" Doc}
      fun {$ Match}
	 Category = {Regex.group 1 Match Doc}
	 Count = {String.toInt {ByteString.toString {Regex.group 2 Match Doc}}}
      in
	 Category#Count
      end
     }
  end

  Url = "http://www.rosettacode.org/mw/index.php?title=Special:Categories&limit=5000"

  {System.showInfo "Retrieving..."}
  Doc = {GetPage Url}

  {System.showInfo "Parsing..."}
  Cs = {GetCategories Doc}
in
  for
     Cat#Count in {Sort Cs fun {$ _#C1 _#C2} C1 > C2 end}
     I in 1..20
  do
     {System.showInfo I#". "#Count#" - "#Cat}
  end
