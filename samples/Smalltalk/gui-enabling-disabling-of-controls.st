|top input vh incButton decButton|

vh := ValueHolder with:0.

top := StandardSystemView label:'Rosetta GUI interaction'.
top extent:300@100.
top add:((Label label:'Value:') origin: 0 @ 10 corner: 100 @ 40).
top add:(input := EditField origin: 102 @ 10 corner: 1.0 @ 40).
input model:(TypeConverter onNumberValue:vh).
input enableChannel:(BlockValue with:[:v | v = 0] argument:vh).

top add:((incButton := Button label:'Inc') origin: 10 @ 50 corner: 100 @ 80).
top add:((decButton := Button label:'Dec') origin: 110 @ 50 corner: 210 @ 80).

incButton action:[ vh value: (vh value + 1) ].
incButton enableChannel:(BlockValue with:[:v | v < 10] argument:vh).
decButton action:[ vh value: (vh value - 1) ].
decButton enableChannel:(BlockValue with:[:v | v > 0] argument:vh).

top open
