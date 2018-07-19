   load 'dates'                                    NB. provides verb 'weekday'
   xmasSunday=: #~ 0 = [: weekday 12 25 ,~"1 0 ]   NB. returns years where 25 Dec is a Sunday
   xmasSunday 2008 + i.114                         NB. check years from 2008 to 2121
2011 2016 2022 2033 2039 2044 2050 2061 2067 2072 2078 2089 2095 2101 2107 2112 2118
