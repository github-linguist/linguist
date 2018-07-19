declare
  %% Lines: list of strings
  %% Alignment: function like fun {Left Txt ExtraSpace} ... end
  %% Returns: list of aligned (virtual) strings
  fun {Align Lines Alignment}
     ParsedLines = {Map Lines ParseLine}
     NumColumns = {Maximum {Map ParsedLines Record.width}}
     %% maps column index to column width:
     WidthOfColumn = {Record.map {TupleRange NumColumns}
                      fun {$ ColumnIndex}
                         fun {LengthOfThisColumn ParsedLine}
                            {Length {CondSelect ParsedLine ColumnIndex nil}}
                         end
                      in
                         {Maximum {Map ParsedLines LengthOfThisColumn}}
                      end}
  in
     {Map ParsedLines
      fun {$ Columns}
         {Record.mapInd Columns
          fun {$ ColumnIndex ColumnText}
             Extra = WidthOfColumn.ColumnIndex - {Length ColumnText}
          in
             {Alignment ColumnText Extra}#" "
          end}
      end}
  end

  %% A parsed line is a tuple of columns.
  %% "a$b$c" -> '#'(1:"a" 2:"b" 3:"c")
  fun {ParseLine Line}
     {List.toTuple '#' {String.tokens Line &$}}
  end

  %% possible alignments:

  fun {Left Txt Extra}
     Txt#{Spaces Extra}
  end

  fun {Right Txt Extra}
     {Spaces Extra}#Txt
  end

  fun {Center Txt Extra}
     Half = Extra div 2
  in
     {Spaces Half}#Txt#{Spaces Half + Extra mod 2}
  end

  %% helpers:

  %% 3 -> unit(1 2 3)
  fun {TupleRange Max}
     {List.toTuple unit {List.number 1 Max 1}}
  end

  fun {Maximum X|Xr}
     {FoldL Xr Value.max X}
  end

  fun {Spaces N}
     case N of 0 then nil
     else & |{Spaces N-1}
     end
  end

  Lines = ["Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
           "are$delineated$by$a$single$'dollar'$character,$write$a$program"
           "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
           "column$are$separated$by$at$least$one$space."
           "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
           "justified,$right$justified,$or$center$justified$within$its$column."]
in
  {ForAll {Align Lines Left} System.showInfo}
