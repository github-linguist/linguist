USING: grouping kernel math random sequences ;

: sorted? ( seq -- ? ) 2 <clumps> [ first2 <= ] all? ;
: bogosort ( seq -- newseq ) [ dup sorted? ] [ randomize ] until ;
