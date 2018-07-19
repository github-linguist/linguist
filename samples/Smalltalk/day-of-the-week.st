2008 to: 2121 do: [ :year | |date|
     date := Date newDay: 25 monthIndex: 12 year: year.
     date dayName = #Sunday
       ifTrue: [ date displayNl ]
]
