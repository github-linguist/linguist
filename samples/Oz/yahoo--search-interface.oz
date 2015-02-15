declare
  [HTTPClient] = {Module.link ['x-ozlib://mesaros/net/HTTPClient.ozf']}
  [StringX] = {Module.link ['x-oz://system/String.ozf']}
  [Regex] = {Module.link ['x-oz://contrib/regex']}

  %% Displays page 1 and 3 of the search results.
  %% The user can request and display more with context menu->Actions->Make Needed.
  proc {ExampleUsage}
     Pages = {YahooSearch "Rosetta code"}
  in
     {Inspector.configure widgetShowStrings true}
     {ForAll {Nth Pages 1} Value.makeNeeded}
     {ForAll {Nth Pages 3} Value.makeNeeded}
     %% Display the infinite list of search result pages.
     {Inspect Pages}
  end

  %% Returns a lazy list of pages.
  %% A page is a lazy list of entries like this: result(url:U title:T content:C).
  fun {YahooSearch Query}
     FetchURL = {CreateURLFetcher}

     fun {Page Nr}
	StartResult = (Nr-1)*10+1
	%% only retrieve it when really needed
	Doc = {Value.byNeed fun {$}
			       {FetchURL "http://search.yahoo.com/search"
				["p"#Query "b"#{Int.toString StartResult}]}
			    end}
	RE = "<a class=\"yschttl spt\" href="
     in
	%% Lazily returns results.
	%% In this way it is possible to build the pages list structure
	%% without creating the single elements
	%% (e.g. retrieve page 1 and 3 but not 2).
	for Match in {Regex.allMatches RE Doc} yield:Yield do
	   Xs = {List.drop Doc Match.0.2}
	in
	   {Yield {ParseEntry Xs}}	
	end
     end
  in
     for PageNr in 1;PageNr+1 yield:Yield do
	{Yield {Page PageNr}}
     end
  end

  fun {CreateURLFetcher}
     Client = {New HTTPClient.cgiGET
	       init(inPrms(toFile:false toStrm:true)
		    httpReqPrms
		   )}
     %% close when no longer used
     {Finalize.register Client proc {$ C} {C closeAll(true)} end}

     fun {FetchURL Url Params}
	OutParams
     in
	{Client getService(Url Params ?OutParams ?_)}
	OutParams.sOut
     end
  in
     FetchURL
  end

  %% Xs: String containing HtmL
  %% Result: "result(url:U title:T content:C)" or "parseError"
  fun {ParseEntry Xs}
     proc {Parse Root}
	R1 R2 R3 R4 R4 R5 R6 R7
	Url = {Fix {QuotedString Xs R1}}
	{Const ">" R1 R2}
	Title = {Fix {Until "</a>" R2 R3}}
	{Const "</h3></div>" R3 R4}
	choice
	   %% "enchanted" result?
	   {Const "<div class=\"sm-bd sm-nophoto\" id=\"sm-bd-4-1\">" R4 R5}
	   {Until "</div>" R5 R6 _}
	[] %% result with links into document
	   {Const "<div class=\"sm-bd sm-r\" id=\"sm-bd-8-1\">" R4 R5}
	   {Until "</ul></div>" R5 R6 _}
	[] %% PDF file
	   {Const "<div class=\"format\">" R4 R5}
	   {Until "</a></div>" R5 R6 _}
	[] %% With Review
	   {Const "<div class=\"sm-bd sm-r\" id=\"sm-bd-9-1\">" R4 R5}
	   R6 = nil %% no nice abstract when a review is there
	[] %% normal result
	   R6 = R4
	end
	Abstract =
	choice
	   {Const "<div class=\"abstr\">" R6 R7}
	   {Fix {Until "</div>" R7 _}}
	[] {Const "<div class=\"sm-abs\">" R6 R7}
	   {Fix {Until "</div>" R7 _}}
	[] ""
	end
     in
	Root = result(url:Url title:Title content:Abstract)
     end
  in
     {CondSelect {SearchOne Parse} 1 parseError}
  end

  %% Result: contents of Xs until M is found.
  %% Xs = {Append M Yr}
  fun {Until M Xs ?Yr}
     L R
  in
     {List.takeDrop Xs {Length M} L R}
     if L == M then Yr = R nil
     elsecase Xs of X|Xr then X|{Until M Xr Yr}
     [] nil then Yr = nil nil
     end
  end

  %% Asserts that Xs starts with C. Returns the remainder in Ys.
  proc {Const C Xs ?Ys}
     {List.takeDrop Xs {Length C} C Ys}
  end

  %% Assert that a quoted string follows.
  %% Returns the unquoted string and binds Ys to the remainder of Xs.
  fun {QuotedString &"|Xs ?Ys}
     fun {Loop Xs Ys}
	case Xs of &\\|&"|Xr then  &\\|&"|{Loop Xr Ys}
	[] &"|Xr then Ys = Xr nil
	[] X|Xr then X|{Loop Xr Ys}
	end
     end
  in
     {Loop Xs Ys}
  end

  %% Remove formatting tags.
  fun {Fix Xs}
     {Until "</a></h3>"
      {FoldL ["<b>" "</b>" "<wbr />" "<wbr>" "<b>...</b>"]
       fun {$ Ys Z}
	  {StringX.replace Ys Z ""}
       end
       Xs}
      _}
  end
in
  {ExampleUsage}
