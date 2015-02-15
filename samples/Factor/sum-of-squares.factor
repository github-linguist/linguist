USE: math sequences ;

: sum-of-squares ( seq -- n ) [ sq ] map-sum ;

{ 1.0 2.0 4.0 8.0 16.0 } sum-of-squares
