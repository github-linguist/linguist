-module( four_bit_adder ).

-export( [add_bits/3, create/1, task/0] ).

add_bits( Adder, A_bits, B_bits ) ->
	Adder ! {erlang:self(), lists:reverse(A_bits), lists:reverse(B_bits)},
	receive
	{Adder, Sum, Carry} -> {Sum, Carry}
	end.

create( How_many_bits ) ->
	Full_adders = connect_full_adders( [full_adder_create() || _X <- lists:seq(1, How_many_bits)] ),
	erlang:spawn_link( fun() -> bit_adder_loop( Full_adders ) end ).

task() ->
	Adder = create( 4 ),
	add_bits( Adder, [0,0,1,0], [0,0,1,1] ).



bit_adder_loop( Full_adders ) ->
	receive
	{Pid, As, Bs} ->
		Sum = [full_adder_sum(Adder, A, B) || {Adder, A, B} <- lists:zip3(Full_adders, As, Bs)],
		Carry = receive
			{carry, C} -> C
			end,
		Pid ! {erlang:self(), lists:reverse(Sum), Carry},
		bit_adder_loop( Full_adders )
	end.

connect_full_adders( [Full_adder | T]=Full_adders ) ->
	lists:foldl( fun connect_full_adders/2, Full_adder, T ),
	Full_adders.

connect_full_adders( Full_adder, Previous_full_adder ) ->
	Previous_full_adder ! {carry_to, Full_adder},
	Full_adder.

half_adder( A, B ) -> {z_xor(A, B), A band B}.

full_adder( A, B, Carry ) ->
	{Sum1, Carry1} = half_adder( A, Carry),
	{Sum, Carry2} = half_adder( B, Sum1),
	{Sum, Carry1 bor Carry2}.

full_adder_create( ) -> erlang:spawn( fun() -> full_adder_loop({0, no_carry_pid}) end ).

full_adder_loop( {Carry, Carry_to} ) ->
	receive
	{carry, New_carry} -> full_adder_loop( {New_carry, Carry_to} );
	{carry_to, Pid} -> full_adder_loop( {Carry, Pid} );
	{add, Pid, A, B} ->
		{Sum, New_carry} = full_adder( A, B, Carry ),
		Pid ! {sum, erlang:self(), Sum},
		full_adder_loop_carry_pid( Carry_to, Pid ) ! {carry, New_carry},
		full_adder_loop( {New_carry, Carry_to} )
	end.

full_adder_loop_carry_pid( no_carry_pid, Pid ) -> Pid;
full_adder_loop_carry_pid( Pid, _Pid ) -> Pid.

full_adder_sum( Pid, A, B ) ->
	Pid ! {add, erlang:self(), A, B},
	receive
	{sum, Pid, S} -> S
	end.

%% xor exists, this is another implementation.
z_xor( A, B ) -> (A band (2+bnot B)) bor ((2+bnot A) band B).
