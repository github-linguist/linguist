-module( dice ).

-export( [dice5/0, dice7/0, task/0] ).

dice5() -> random:uniform( 5 ).

dice7() ->
	dice7_small_enough( dice5() * 5 + dice5() - 6 ). % 0 - 24

task() ->
       verify_distribution_uniformity:naive( fun dice7/0, 1000000, 1 ).



dice7_small_enough( N ) when N < 21 -> N div 3 + 1;
dice7_small_enough( _N ) -> dice7().
