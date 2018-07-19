|top clickCount lh button|

clickCount := 0.
lh := ValueHolder with:'There have been no clicks yet'.

top := StandardSystemView label:'Rosetta Simple Window'.
top extent:300@100.
top add:((Label new labelChannel:lh) origin: 0 @ 10 corner: 1.0 @ 40).
top add:((button := Button label:'Eat Me') origin: 10 @ 50 corner: 100 @ 80).

button action:[
        clickCount := clickCount + 1.
        lh value: ('number of clicks: %1' bindWith:clickCount)
       ].

top open
