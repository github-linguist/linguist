   require'web/gethttp'
   dict=: gethttp'http://www.puzzlers.org/pub/wordlists/unixdict.txt'
   oWords=: (#~ ] = /:~L:0) <;._2 dict-.CR
   ;:inv (#~ (= >./)@:(#@>))oWords
abbott accent accept access accost almost bellow billow biopsy chilly choosy choppy effort floppy glossy knotty
