new( Data ) -> erlang:spawn( fun() -> loop( Data, nonext ) end ).
