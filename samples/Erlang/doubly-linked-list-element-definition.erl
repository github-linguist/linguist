new( Data ) -> erlang:spawn( fun() -> loop( Data, noprevious, nonext ) end ).
