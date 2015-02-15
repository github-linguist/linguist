 Identification division.
 Program-id. rc-1d-cell.

 Data division.
 Working-storage section.

*> "Constants."
 01 max-gens            pic  999  value   9.
 01 state-width         pic   99  value  20.
 01 state-table-init    pic x(20) value ".@@@.@@.@.@.@.@..@..".
 01 alive               pic    x  value "@".
 01 dead                pic    x  value ".".

*> The current state.
 01 state-gen           pic  999  value   0.
 01 state-row.
    05 state-row-gen   pic zz9.
    05 filler          pic  xx   value ": ".
    05 state-table.
        10 state-cells pic   x   occurs 20 times.

*> The new state.
 01 new-state-table.
    05 new-state-cells pic   x   occurs 20 times.

*> Pointer into cell table during generational production.
 01 cell-index          pic   99.
    88 at-beginning    value  1.
    88 is-inside       values 2 thru 19.
    88 at-end          value 20.

*> The cell's neighborhood.
 01 neighbor-count-def.
   03 neighbor-count      pic   9.
     88 is-comfy        value 1.
     88 is-ripe         value 2.

 Procedure division.
     Perform Init-state-table.
     Perform max-gens times
         perform Display-row
         perform Next-state
     end-perform.
     Perform Display-row.
     Stop run.

 Display-row.
     Move state-gen to state-row-gen.
     Display state-row.

*> Determine who lives and who dies.
 Next-state.
     Add 1 to state-gen.
     Move state-table to new-state-table.

     Perform with test after
         varying cell-index from 1 by 1
         until at-end
         perform Count-neighbors
         perform Die-off
         perform New-births
     end-perform

     move new-state-table to state-table.

*> Living cell with wrong number of neighbors...
 Die-off.
     if state-cells(cell-index) =
     alive and not is-comfy
         then move dead to new-state-cells(cell-index)
     end-if
     .

*> Empty cell with exactly two neighbors are...
 New-births.
     if state-cells(cell-index) = dead and is-ripe
         then move alive to new-state-cells(cell-index)
     end-if
    .
*> How many living neighbors does a cell have?
 Count-neighbors.
     Move 0 to neighbor-count
     if at-beginning or at-end then
         add 1 to neighbor-count
     else
       if is-inside and state-cells(cell-index - 1) = alive
       then
           add 1 to neighbor-count
       end-if
       if is-inside and state-cells(cell-index + 1) = alive
       then
           add 1 to neighbor-count
       end-if
     end-if
     .

*> String is easier to enter, but table is easier to work with,
*> so move each character of the initialization string to the
*> state table.

 Init-state-table.
     Perform with test after
         varying cell-index from 1 by 1
         until at-end
         move state-table-init(cell-index:1)
           to state-cells(cell-index)
      end-perform
      .
