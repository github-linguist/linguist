: qsort ( seq -- seq )
    dup empty? [
      unclip [ [ < ] curry partition [ qsort ] bi@ ] keep
      prefix append
    ] unless ;
