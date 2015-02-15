declare

[Roads] = {Module.link ['x-ozlib://wmeyer/roads/Roads.ozf']}

fun {Table Session}
   html(
      head(title("Show a table with row and column headings")
	   style(type:"text/css"
		 css(td 'text-align':center)
		))
      body(
	 {TagFromList table
	  tr(th th("X") th("Y") th("Z"))
	  |
	  {CreateRows 3 5}
	 }))
end

fun {CreateRows NumCols NumRows}
   {List.map {List.number 1 NumRows 1}
    fun {$ Row}
       {TagFromList tr
	td( {Int.toString Row} )
	|
	{List.map {List.number 1 NumCols 1}
	 fun {$ Col}
	    SequentialNumber = (Row-1)*NumCols + Col
	 in
	    td( {Int.toString SequentialNumber} )
	 end
	}}
    end
   }
end

TagFromList = List.toTuple

in

{Roads.registerFunction table Table}
{Roads.run}
