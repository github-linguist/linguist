#using scripts\codescripts\struct;

#using scripts\shared\array_shared;
#using scripts\shared\flag_shared;
#using scripts\shared\flagsys_shared;
#using scripts\shared\util_shared;

#insert scripts\shared\shared.gsh;

#namespace array;

/@
@/
function filter( &array, b_keep_keys, func_filter, arg1, arg2, arg3, arg4, arg5  )
{
	a_new = [];
	
	foreach ( key, val in array )
	{
		if ( util::single_func( self, func_filter, val, arg1, arg2, arg3, arg4, arg5 ) )
		{
			if ( IsString( key ) || IsWeapon( key ) )
			{
				// by default, string and weapon object keys are kept
				if ( isdefined( b_keep_keys ) && !b_keep_keys )
				{
					a_new[ a_new.size ] = val;
				}
				else
				{
					a_new[ key ] = val;
				}
			}
			else
			{
				// by default, int keys are not kept
				if ( IS_TRUE( b_keep_keys ) )
				{
					a_new[ key ] = val;
				}
				else
				{
					a_new[ a_new.size ] = val;
				}
			}
		}
	}
	
	return a_new;
}

/@
@/
function remove_dead( &array, b_keep_keys )
{
	return filter( array, b_keep_keys, &_filter_dead );
}

function _filter_undefined( val )
{
	return isdefined( val );
}

/@
@/
function remove_undefined( &array, b_keep_keys )
{
	return filter( array, b_keep_keys, &_filter_undefined );
}

// This function crashes if called but seems like it should work and would be useful
function cleanup( &array, b_keep_empty_arrays = false )
{
	a_keys = GetArrayKeys( array );
	
	for ( i = a_keys.size - 1; i >= 0; i-- )
	{
		key = a_keys[ i ];
		
		if ( IsArray( array[ key ] ) && array[ key ].size )
		{
			cleanup( array[ key ], b_keep_empty_arrays );
		}		
		else if ( !isdefined( array[ key ] )
		    || ( !b_keep_empty_arrays && IsArray( array[ key ] ) && !array[ key ].size ) )
		{
			ArrayRemoveIndex( array, key );
		}
	}
}

/@
@/
function filter_classname( &array, b_keep_keys, str_classname )
{
	return filter( array, b_keep_keys, &_filter_classname, str_classname );
}

function get_touching( &array, b_keep_keys )
{
	return filter( array, b_keep_keys, &IsTouching );
}

/@
"Name: array::remove_index( <array>, <index>, [b_keep_keys]  )"
"Summary: Removes a specified index from an array, returns a new array with the specified index removed."
"Module: Array"
"MandatoryArg: <array> : The array we will remove an index from."
"MandatoryArg: <index> : The index we will remove from the array."
"OptionalArg:  [b_keep_keys] : If true, retain existing keys. If false or undefined, existing keys of original array will be replaced by ints."
"Example: a_new = array::remove_index( array, 3 );"
"SPMP: both"
@/
function remove_index( array, index, b_keep_keys )
{
	a_new = [];
	
	foreach ( key, val in array )
	{
		if( key == index )
		{
			continue;	
		}
		else
		{
			if ( IS_TRUE( b_keep_keys ) )
			{
				a_new[ key ] = val;
			}
			else
			{
				a_new[ a_new.size ] = val;
			}	
		}
	}
	
	return a_new;
}

/@
"Name: array::delete_all( <array> )"
"Summary: Delete all the elements in an array"
"Module: Array"
"MandatoryArg: <array> : The array whose elements to delete."
"Example: array::delete_all( GetAITeamArray( "axis" ) );"
"SPMP: both"
@/
function delete_all( &array, is_struct )
{
	foreach ( ent in array )
	{
		if ( isdefined( ent ) )
		{
			if ( IS_TRUE( is_struct ) )
			{
				ent struct::delete();
			}
			else if ( isdefined( ent.__vtable ) )
			{
				DELETE( ent );	// class
			}
			else
			{
				ent Delete();
			}
		}
	}
}

/@
"Name: array::notify_all( <array>, <notify> )"
"Summary: Sends a notify to every element within the array"
"Module: Utility"
"MandatoryArg: <array>: the array of entities to wait on"
"MandatoryArg: <notify>: the string notify sent to the elements"
"Example: array::notify_all( soldiers, "fire" );"
"SPMP: both"
@/
function notify_all( &array, str_notify )
{
	foreach ( elem in array )
	{
		elem notify( str_notify );
	}
}

/@
"Name: thread_all( <entities>, <func>, [arg1], [arg2], [arg3], [arg4], [arg5], [arg6] )"
"Summary: Threads the < func > function on every entity in the < entities > array. The entity will become "self" in the specified function."
"Module: Array"
"CallOn: "
"MandatoryArg: entities : array of entities to thread the function"
"MandatoryArg: func : pointer to a script function"
"OptionalArg: arg1: parameter 1 to pass to the func"
"OptionalArg: arg2 : parameter 2 to pass to the func"
"OptionalArg: arg3 : parameter 3 to pass to the func"
"OptionalArg: arg4 : parameter 4 to pass to the func"
"OptionalArg: arg5 : parameter 5 to pass to the func"
"OptionalArg: arg6 : parameter 6 to pass to the func"
"Example: array::thread_all( GetAITeamArray( "allies" ), &set_ignoreme, false );"
"SPMP: both"
@/
function thread_all( &entities, func, arg1, arg2, arg3, arg4, arg5, arg6 )
{
	Assert( isdefined( entities ), "Undefined entity array passed to array::thread_all" );
	Assert( isdefined( func ), "Undefined function passed to array::thread_all" );

	if ( IsArray( entities ) )
	{
		if ( isdefined( arg6 ) )
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]( arg1, arg2, arg3, arg4, arg5, arg6 );
			}
		}
		else if ( isdefined( arg5 ) )
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]( arg1, arg2, arg3, arg4, arg5 );
			}
		}
		else if ( isdefined( arg4 ) )
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]( arg1, arg2, arg3, arg4 );
			}
		}
		else if ( isdefined( arg3 ) )
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]( arg1, arg2, arg3 );
			}
		}
		else if ( isdefined( arg2 ) )
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]( arg1, arg2 );
			}
		}
		else if ( isdefined( arg1 ) )
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]( arg1 );
			}
		}
		else
		{
			foreach ( ent in entities )
			{
				ent thread [[ func ]]();
			}
		}
	}
	else
	{
		util::single_thread( entities, func, arg1, arg2, arg3, arg4, arg5, arg6 );
	}
}

/@
"Name: thread_all_ents( <entities>, <func>, [arg1], [arg2], [arg3], [arg4], [arg5] )"
"Summary: Threads the <func> function on self for every entity in the <entities> array, passing the entity has the first argument."
"Module: Array"
"CallOn: NA"
"MandatoryArg: entities : array of entities to thread the function"
"MandatoryArg: func : pointer to a script function"
"OptionalArg: arg1 : parameter 1 to pass to the func (after the entity)"
"OptionalArg: arg2 : parameter 2 to pass to the func (after the entity)"
"OptionalArg: arg3 : parameter 3 to pass to the func (after the entity)"
"OptionalArg: arg4 : parameter 4 to pass to the func (after the entity)"
"OptionalArg: arg5 : parameter 5 to pass to the func (after the entity)"
"Example: array::thread_all_ents( GetAITeamArray( "allies" ), &do_something, false );"
"SPMP: both"
@/
function thread_all_ents( &entities, func, arg1, arg2, arg3, arg4, arg5 )
{
	Assert( isdefined( entities ), "Undefined entity array passed to util::array_ent_thread" );
	Assert( isdefined( func ), "Undefined function passed to util::array_ent_thread" );
	
	if ( IsArray( entities ) )
	{
		if ( entities.size )
		{
			keys = GetArrayKeys( entities );
			for ( i = 0; i < keys.size; i++ )
			{
				util::single_thread( self, func, entities[keys[i]], arg1, arg2, arg3, arg4, arg5 );
			}
		}
	}
	else
	{
		util::single_thread( self, func, entities, arg1, arg2, arg3, arg4, arg5 );
	}
}

/@
"Name: run_all( <entities>, <func>, [arg1], [arg2], [arg3], [arg4], [arg5], [arg6] )"
"Summary: Runs the < func > function on every entity in the < entities > array. The entity will become "self" in the specified function."
"Module: Array"
"CallOn: "
"MandatoryArg: entities : array of entities to run the function"
"MandatoryArg: func : pointer to a script function"
"OptionalArg: arg1: parameter 1 to pass to the func"
"OptionalArg: arg2 : parameter 2 to pass to the func"
"OptionalArg: arg3 : parameter 3 to pass to the func"
"OptionalArg: arg4 : parameter 4 to pass to the func"
"OptionalArg: arg5 : parameter 5 to pass to the func"
"OptionalArg: arg6 : parameter 6 to pass to the func"
"Example: array::run_all( GetAITeamArray( "allies" ), &set_ignoreme, false );"
"SPMP: both"
@/
function run_all( &entities, func, arg1, arg2, arg3, arg4, arg5, arg6 )
{
	Assert( isdefined( entities ), "Undefined entity array passed to array::run_all" );
	Assert( isdefined( func ), "Undefined function passed to array::run_all" );

	if ( IsArray( entities ) )
	{
		if ( isdefined( arg6 ) )
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]( arg1, arg2, arg3, arg4, arg5, arg6 );
			}
		}
		else if ( isdefined( arg5 ) )
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]( arg1, arg2, arg3, arg4, arg5 );
			}
		}
		else if ( isdefined( arg4 ) )
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]( arg1, arg2, arg3, arg4 );
			}
		}
		else if ( isdefined( arg3 ) )
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]( arg1, arg2, arg3 );
			}
		}
		else if ( isdefined( arg2 ) )
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]( arg1, arg2 );
			}
		}
		else if ( isdefined( arg1 ) )
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]( arg1 );
			}
		}
		else
		{
			foreach ( ent in entities )
			{
				ent [[ func ]]();
			}
		}
	}
	else
	{
		util::single_func( entities, func, arg1, arg2, arg3, arg4, arg5, arg6 );
	}
}

/@
"Name: array::exclude( <array> , <array_exclude> )"
"Summary: Returns an array excluding all members of < array_exclude > "
"Module: Array"
"CallOn: "
"MandatoryArg: <array> : Array containing all items"
"MandatoryArg: <array_exclude> : Array containing all items to remove or individual entity"
"Example: newArray = array::exclude( array1, array2 );"
"SPMP: both"
@/
function exclude( array, array_exclude )// returns "array" minus all members of array_exclude
{
	newarray = array;
	
	if ( IsArray( array_exclude ) )
	{
		foreach ( exclude_item in array_exclude )
		{
			ArrayRemoveValue( newarray, exclude_item );
		}
	}
	else
	{
		ArrayRemoveValue( newarray, array_exclude );
	}
  
	return newarray;
}

/@
"Name: array::add( <array> , <item>, <allow_dupes> )"
"Summary: Adds <item> to <array>.  Will not add the new value if undefined."
"Module: Array"
"CallOn: "
"MandatoryArg:	<array> The array to add <item> to."
"MandatoryArg:	<item> The item to be added. This can be anything."
"OptionalArg:	<allow_dupes> If true, will add the new value if it already exists."
"Example: array::add( nodes, new_node );"
"SPMP: both"
@/
function add( &array, item, allow_dupes = true )
{
	if ( isdefined( item ) )
	{
		if ( allow_dupes || !IsInArray( array, item ) )
		{
			array[ array.size ] = item;
		}
	}
}

/@
"Name: array::add_sorted( <array> , <item>, <allow_dupes> )"
"Summary: Adds <item> to <array> in sorted order from smallest to biggest.  Will not add the new value if undefined."
"Module: Array"
"CallOn: "
"MandatoryArg:	<array> The array to add <item> to."
"MandatoryArg:	<item> The item to be added. This can be anything."
"OptionalArg:	<allow_dupes> If true, will add the new value if it already exists."
"Example: array::add_sorted( a_numbers, 4 );"
"SPMP: both"
@/
function add_sorted( &array, item, allow_dupes = true )
{
	if ( isdefined( item ) )
	{
		if ( allow_dupes || !IsInArray( array, item ) )
		{
			for ( i = 0; i <= array.size; i++ )
			{
				if ( ( i == array.size ) || ( item <= array[i] ) )
				{
					ArrayInsert( array, item, i );
					break;
				}
			}
		}
	}
}

/@
"Name: array::wait_till( <array>, <str_notify>, [n_timeout] )"
"Summary: waits for every entry in the <array> to recieve the notify, die, or n_timeout"
"Module: Utility"
"MandatoryArg: <array>: the array of entities to wait on"
"MandatoryArg: <notifies>: the notify each array entity will wait on.  Can also be an array of notifies (will wait for *any* of the notifies)."
"OptionalArg: [n_timeout]: n_timeout to kill the wait prematurely"
"Example: array::wait_till( guys, "at the hq" );"
"SPMP: both"
@/
function wait_till( &array, notifies, n_timeout )
{
	TIMEOUT( n_timeout );
	
	s_tracker = SpawnStruct();
	s_tracker._wait_count = 0;
	
	foreach ( ent in array )
	{
		if ( isdefined( ent ) )
		{
			ent thread util::timeout( n_timeout, &util::_waitlogic, s_tracker, notifies );
		}
	}
	
	if ( s_tracker._wait_count > 0 )
	{
		s_tracker waittill( "waitlogic_finished" );
	}
}

/@
"Name: array::wait_till_match( <array>, <str_notify>, <str_match>, [n_timeout] )"
"Summary: waits for every entry in the <array> to recieve the notify with a match parameter, die, or n_timeout"
"Module: Utility"
"MandatoryArg: <array>: the array of entities to wait on"
"MandatoryArg: <str_notify>: the notify each array entity will wait on.  Can also be an array of notifies (will wait for *any* of the notifies)."
"MandatoryArg: <str_match>: the notify match value to wait for."
"OptionalArg: <n_timeout>: n_timeout to kill the wait prematurely"
"Example: array::wait_till( guys, "at the hq" );"
"SPMP: both"
@/
function wait_till_match( &array, str_notify, str_match, n_timeout )
{
	TIMEOUT( n_timeout );
	
	s_tracker = SpawnStruct();
	s_tracker._array_wait_count = 0;
	
	foreach ( ent in array )
	{
		if ( isdefined( ent ) )
		{
			s_tracker._array_wait_count++;
			ent thread util::timeout( n_timeout, &_waitlogic_match, s_tracker, str_notify, str_match );
			ent thread util::timeout( n_timeout, &_waitlogic_death, s_tracker );
		}
	}
	
	if ( s_tracker._array_wait_count > 0 )
	{
		s_tracker waittill( "array_wait" );
	}
}

function _waitlogic_match( s_tracker, str_notify, str_match )
{
	self endon( "death" );
	self waittillmatch( str_notify, str_match );
	update_waitlogic_tracker( s_tracker );
}

function _waitlogic_death( s_tracker )
{
	self waittill( "death" );
	update_waitlogic_tracker( s_tracker );
}

function update_waitlogic_tracker( s_tracker )
{
	s_tracker._array_wait_count--;
	if ( s_tracker._array_wait_count == 0 )
	{
		s_tracker notify( "array_wait" );
	}
}

/@
@/
function flag_wait( &array, str_flag )
{
	do
	{
		recheck = false;
		
		for ( i = 0; i < array.size; i++ )
		{
			ent = array[ i ];
			
			if ( isdefined( ent ) && !ent flag::get( str_flag ) )
			{
				ent util::waittill_either( "death", str_flag );
				recheck = true;
				break;
			}
		}
	}
	while ( recheck );
}

/@
@/
function flagsys_wait( &array, str_flag )
{
	do
	{
		recheck = false;
	
		for ( i = 0; i < array.size; i++ )
		{
			ent = array[ i ];
	
			if ( isdefined( ent ) && !ent flagsys::get( str_flag ) )
			{
				ent util::waittill_either( "death", str_flag );
				recheck = true;
				break;
			}
		}
	}
	while ( recheck );
}


/@
@/
function flagsys_wait_any_flag( &array, ... )
{
	do
	{
		recheck = false;
		
		for ( i = 0; i < array.size; i++ )
		{
			ent = array[i];
	
			if ( isdefined( ent ) )
			{
				b_flag_set = false;
				foreach ( str_flag in vararg )
				{
					if ( ent flagsys::get( str_flag ) )
					{
						b_flag_set = true;
						break;					
					}
				}
				
				if ( !b_flag_set )
				{			
					ent util::waittill_any_array( vararg );
					recheck = true;
				}
			}
		}
	} while( recheck );
}

/@
@/
function flagsys_wait_any( &array, str_flag )
{
	foreach ( ent in array )
	{
		if ( ent flagsys::get( str_flag ) )
		{
			return ent;
		}
	}

	wait_any( array, str_flag );
}

/@
@/
function flag_wait_clear( &array, str_flag )
{
	do
	{
		recheck = false;
	
		for ( i = 0; i < array.size; i++ )
		{
			ent = array[i];		
			if ( ent flag::get( str_flag ) )
			{
				ent waittill( str_flag );
				recheck = true;
			}
		}	
	} while( recheck );
}

/@
@/
function flagsys_wait_clear( &array, str_flag, n_timeout )
{
	TIMEOUT( n_timeout );
	
	do
	{
		recheck = false;
	
		for ( i = 0; i < array.size; i++ )
		{
			ent = array[i];		
			if ( IsDefined( ent ) && ent flagsys::get( str_flag ) )
			{
				ent waittill( str_flag );
				recheck = true;
			}
		}
	} while( recheck );
}

/@
"Name: wait_any( <array>, <msg>, <n_timeout> )"
"Summary: waits for any entry in the <array> to recieve the <msg> notify, die, or n_timeout"
"Module: Utility"
"MandatoryArg: <array>: the array of entities to wait on"
"MandatoryArg: <msg>: the msg each array entity will wait on"
"OptionalArg: <n_timeout>: n_timeout to kill the wait prematurely"
"Example: array_wait_any( guys, "at the hq" );"
"SPMP: both"
@/
function wait_any( array, msg, n_timeout )
{
	TIMEOUT( n_timeout );
	
	s_tracker = SpawnStruct();

	foreach ( ent in array )
	{
		if ( isdefined( ent ) )
		{
			level thread util::timeout( n_timeout, &_waitlogic2, s_tracker, ent, msg );
		}
	}
	
	s_tracker endon( "array_wait" );
		
	wait_till( array, "death" );
}

function _waitlogic2( s_tracker, ent, msg )
{
	s_tracker endon( "array_wait" );
	
	if( msg != "death" )
	{
		ent endon( "death" );
	}
	
	ent util::waittill_any_array( msg );
	s_tracker notify( "array_wait" );
}

function flag_wait_any( array, str_flag )
{
	self endon( "death" );
	
	foreach ( ent in array )
	{
		if ( ent flag::get( str_flag ) )
		{
			return ent;
		}
	}

	wait_any( array, str_flag );
}

/@
"Name: random( <array> )"
"Summary: returns a random element from the passed in array "
"Module: Array"
"Example: random_spawner = random( event_1_spawners );"
"MandatoryArg: <array> : the array from which to pluck a random element"
"SPMP: both"
@/
function random( array )
{
	if ( array.size > 0 )
	{
		keys = GetArrayKeys( array );
		return array[ keys[RandomInt( keys.size )] ];
	}
}

/@
"Name: randomize( <array> )"
"Summary: returns a randomized new array from the passed in array "
"Module: Array"
"Example: a_spawn_pos = randomize( a_spawn_pos );"
"MandatoryArg: <array> : the array from which to create the new random array from"
"SPMP: both"
@/
function randomize( array )
{
	for ( i = 0; i < array.size; i++ )
	{
		j = RandomInt( array.size );
		temp = array[ i ];
		array[ i ] = array[ j ];
		array[ j ] = temp;
	}

	return array;
}

/@
"Name: clamp_size( <array>, <n_size> )"
"Summary: returns a chopped off version of the array with only n_count number of elements."
"Example: a_spawn_pos = clamp_size( a_spawn_pos, 255 );"
"MandatoryArg: <array> : the array from which to create the new array from"
"MandatoryArg: <n_size> : the size of the array to return"
@/
function clamp_size( array, n_size )
{
	a_ret = [];
	for ( i = 0; i < n_size; i++ )
	{
		a_ret[ i ] = array[ i ];
	}
	
	return a_ret;
}

/@
"Name: array::reverse( <array> )"
"Summary: Reverses the order of the array and returns the new array."
"Module: Array"
"CallOn: "
"MandatoryArg: <array> : Array to be reversed."
"Example: patrol_nodes = array::reverse( patrol_nodes );"
"SPMP: both"
@/
function reverse( array )
{
	a_array2 = [];
	for ( i = array.size - 1; i >= 0; i-- )
	{
		a_array2[ a_array2.size ] = array[ i ];
	}

	return a_array2;
}

/@
@/
function remove_keys( array )
{
	a_new = [];
	
	foreach ( _, val in array )
	{
		if ( isdefined( val ) )
		{
			a_new[ a_new.size ] = val;
		}
	}
	
	return a_new;
}

/@
@/
function swap( &array, index1, index2 )
{
	temp = array[ index1 ];
	array[ index1 ] = array[ index2 ];
	array[ index2 ] = temp;
}

function pop( &array, index, b_keep_keys = true )
{
	if ( array.size > 0 )
	{
		if ( !isdefined( index ) )
		{
			keys = GetArrayKeys( array );
			index = keys[ 0 ];
		}
		
		if ( isdefined( array[index] ) )
		{
			ret = array[ index ];
			
			ArrayRemoveIndex( array, index, b_keep_keys );
			
			return ret;
		}	
	}
}

function pop_front( &array, b_keep_keys = true )
{
	keys = GetArrayKeys( array );
	index = keys[ keys.size - 1 ];
	return pop( array, index, b_keep_keys );
}

function push( &array, val, index )
{
	if ( !isdefined( index ) )
	{
		// use max free integer as index
		index = 0;
		foreach ( key in GetArrayKeys( array ) )
		{
			if ( IsInt( key ) && ( key >= index ) )
			{
				index = key + 1;
			}
		}
	}
	
	ArrayInsert( array, val, index );
}

function push_front( &array, val )
{
	push( array, val, 0 );
}

/@
"Name: get_closest( <org> , <array> , <dist> )"
"Summary: Returns the closest entity in < array > to location < org > "
"Module: Distance"
"CallOn: "
"MandatoryArg: <org> : Origin to be closest to."
"MandatoryArg: <array> : Array of entities to check distance on"
"OptionalArg: <dist> : Minimum distance to check"
"Example: friendly = array::get_closest( GetPlayers()[0].origin, allies );"
"SPMP: singleplayer"
@/
function get_closest( org, &array, dist )
{
	assert( 0, "Deprecated function. Use 'ArrayGetClosest' instead." );
}

/@
"Name: getFarthest( <org> , <array> , <dist> )"
"Summary: Returns the farthest entity in < array > to location < org > "
"Module: Distance"
"CallOn: "
"MandatoryArg: <org> : Origin to be farthest from."
"MandatoryArg: <array> : Array of entities to check distance on"
"OptionalArg: <dist> : Maximum distance to check"
"Example: target = getFarthest( level.player.origin, targets );"
"SPMP: singleplayer"
@/ 
function get_farthest( org, &array, dist = undefined )
{
	assert( 0, "Deprecated function. Use 'ArrayGetFarthest' instead." );
}

function closerFunc( dist1, dist2 )
{
	return dist1 >= dist2;
}

function fartherFunc( dist1, dist2 )
{
	return dist1 <= dist2;
}

/@
"Name: get_all_farthest( <org> , <array> , [a_exclude] , [n_max], [n_maxdist] )"
"Summary: Returns an array of all the entities in < array > sorted in order of farthest to closest."
"MandatoryArg: <org> : Origin to be farthest from."
"MandatoryArg: <array> : Array of entities (anything that contain .origin) to check distance on."
"OptionalArg: <a_exclude> : Array of entities to exclude from the check."
"OptionalArg: <n_max> : max size of the array to return"
"OptionalArg: <n_maxdist> : max distance from the origin to return acceptable entities"
"Example: allies_sort = get_all_farthest( originFC1.origin, allies );"
@/
function get_all_farthest( org, &array, a_exclude, n_max, n_maxdist )
{
	DEFAULT( n_max, array.size );
	
	a_ret = exclude( array, a_exclude );
	
	if ( isdefined( n_maxdist ) )
	{
		a_ret = ArraySort( a_ret, org, false, n_max, n_maxdist );
	}
	else
	{
		a_ret = ArraySort( a_ret, org, false, n_max );
	}
	
	return a_ret;
}

/@
"Name: get_all_closest( <org> , <array> , [a_exclude] , [n_max], [n_maxdist] )"
"Summary: Returns an array of all the entities in < array > sorted in order of closest to farthest."
"CallOn: "
"MandatoryArg: <org> : Origin to be closest to."
"MandatoryArg: <array> : Array of entities to check distance on."
"OptionalArg: <a_exclude> : Array of entities to exclude from the check."
"OptionalArg: <n_max> : max size of the array to return"
"OptionalArg: <n_maxdist> : max distance from the origin to return acceptable entities"
"Example: allies_sort = get_all_closest( originFC1.origin, allies );"
@/ 
function get_all_closest( org, &array, a_exclude, n_max, n_maxdist )
{
	DEFAULT( n_max, array.size );
	
	a_ret = exclude( array, a_exclude );
	
	if ( isdefined( n_maxdist ) )
	{
		a_ret = ArraySort( a_ret, org, true, n_max, n_maxdist );
	}
	else
	{
		a_ret = ArraySort( a_ret, org, true, n_max );
	}
	
	return a_ret;
}

function alphabetize( &array )
{
	return sort_by_value( array, true );
}

/@
function Name: sort_by_value( array, b_lowest_first = true )
Summary: sorts a list of ents by their value
Module: Utility
CallOn: n/a
ManditoryArg: <array>: array of values to sort
OptionalArg: [b_lowest_first]: sort from lowest to highest
function Example: list = array::sort_by_value( array );
SPMP: singleplayer
@/
//Use ArraySort for distance based sorting of entities
function sort_by_value( &array, b_lowest_first = false )
{
	return merge_sort( array, &_sort_by_value_compare_func, b_lowest_first );
}

function _sort_by_value_compare_func( val1, val2, b_lowest_first )
{
	if ( b_lowest_first )
	{
		return val1 < val2;
	}
	else
	{
		return val1 > val2;
	}
}

/@
function Name: sort_by_script_int( a_ents, b_lowest_first = true )
Summary: sorts a list of ents by their script_int value
Module: Utility
CallOn: n/a
ManditoryArg: <a_ents>: array of entities to sort
OptionalArg: [b_lowest_first]: sort from lowest to highest
function Example: list = array::sort_by_script_int( a_ents );
SPMP: singleplayer
@/
//Use ArraySort for distance based sorting of entities
function sort_by_script_int( &a_ents, b_lowest_first = false )
{
	return merge_sort( a_ents, &_sort_by_script_int_compare_func, b_lowest_first );
}

function _sort_by_script_int_compare_func( e1, e2, b_lowest_first )
{
	if ( b_lowest_first )
	{
		return e1.script_int < e2.script_int;
	}
	else
	{
		return e1.script_int > e2.script_int;
	}
}

function merge_sort( &current_list, func_sort, param )
{
	if ( current_list.size <= 1 )
	{
		return current_list;
	}
		
	left = [];
	right = [];
	
	middle = current_list.size / 2;
	
	for ( x = 0; x < middle; x++ )
	{
		ARRAY_ADD( left, current_list[ x ] );
	}
	
	for ( ; x < current_list.size; x++ )
	{
		ARRAY_ADD( right, current_list[ x ] );
	}
	
	left = merge_sort( left, func_sort, param );
	right = merge_sort( right, func_sort, param );
	
	result = merge( left, right, func_sort, param );

	return result;
}

function merge( left, right, func_sort, param )
{
	result = [];

	li = 0;
	ri = 0;
	while ( li < left.size && ri < right.size )
	{
		b_result = undefined;
		
		if ( isdefined( param ) )
		{
			b_result = [[ func_sort ]]( left[ li ], right[ ri ], param );
		}
		else
		{
			b_result = [[ func_sort ]]( left[ li ], right[ ri ] );
		}
		
		if ( b_result )
		{
			result[ result.size ] = left[ li ];
			li++;
		}
		else
		{
			result[ result.size ] = right[ ri ];
			ri++;
		}
	}

	while ( li < left.size )
	{
		result[ result.size ] = left[ li ];
		li++;
	}

	while ( ri < right.size )
	{
		result[ result.size ] = right[ ri ];
		ri++;
	}

	return result;
}

/@
"Name: insertion_sort( <array>, <compareFunc> , <val> )"
"Summary: performs and insertion sort into <array> of the value <val> based on result of <compareFunc>.  input <array> must already be sorted!  <array> must have integer keys.
"Module: Array"
"CallOn: "
"MandatoryArg: <array> : array to operate on, works in place"
"MandatoryArg: <compareFunc> : foo(a,b);  function that returns the results of comparison, <0 if a<b, 0 if a==b, >0 if a>b"
"MadatoryArg: <val> : the value to insert"
"Example: array::insertion_sort( a, foo, b );"
"SPMP: Both"
@/

function insertion_sort( &a, compareFunc, val )
{
	if (!IsDefined(a))
	{
		a=[];
		a[0]=val;
		return ;
	}
	
	for (i=0;i<a.size;i++)
	{
		if ([[compareFunc]](a[i],val)<=0)
		{
			ArrayInsert(a,val,i);
			return ;
		}
	}
	a[a.size]=val;
}

/@
"Name: spread_all( <entities> , <process> , <var1> , <var2> , <var3> )"
"Summary: Threads the < process > function on every entity in the < entities > array. Each thread is started 1 network frame apart from the next."
"Module: Array"
"CallOn: "
"MandatoryArg: <entities> : array of entities to thread the process"
"MandatoryArg: <process> : pointer to a script function"
"OptionalArg: <var1> : parameter 1 to pass to the process"
"OptionalArg: <var2> : parameter 2 to pass to the process"
"OptionalArg: <var3> : parameter 3 to pass to the process"
"Example: array::spread_all( GetAITeamArray( "allies" ),&set_ignoreme, false );"
"SPMP: Both"
@/

function spread_all( &entities, func, arg1, arg2, arg3, arg4, arg5 )
{
	Assert( isdefined( entities ), "Undefined entity array passed to array::spread_all_ents" );
	Assert( isdefined( func ), "Undefined function passed to array::spread_all_ents" );
	
	if ( IsArray( entities ) )
	{
		foreach ( ent in entities )
		{
			if( isdefined(ent) )
			{
				util::single_thread( ent, func, arg1, arg2, arg3, arg4, arg5 );
			}
			WAIT_ABOUT( .1 );
		}
	}
	else
	{
		util::single_thread( entities, func, arg1, arg2, arg3, arg4, arg5 );
		WAIT_ABOUT( .1 );
	}
}

/@
"Name: wait_till_touching( <array> , <volume> )"
"Summary: Waits until all entities in <array> are touching <volume> at the same time"
"Module: array"
"CallOn: "
"MandatoryArg:	<array> The array of entities."
"MandatoryArg:	<volume> The volume to check for touching"
"Example: array::wait_till_touching(a_group_of_dudes, e_volume_to_wait_for);"
@/
function wait_till_touching( &a_ents, e_volume )
{
	while ( !is_touching( a_ents, e_volume ) )
	{
		wait 0.05;
	}	
}

/@
"Name: is_touching( <array> , <volume> )"
"Summary: Returns true if all entities in <array> are touching <volume>"
"Module: array"
"CallOn: "
"MandatoryArg:	<array> The array of entities."
"MandatoryArg:	<volume> The volume to check for touching"
"Example: array::is_touching(a_group_of_dudes, e_volume);"
@/
function is_touching( &a_ents, e_volume )
{
	foreach ( e_ent in a_ents )
	{
		if ( !e_ent IsTouching( e_volume ) )
	    {
	        return false;
	    }
	}
	
	return true;
}

/@
"Name: contains( <array> , <value> )"
"Summary: Returns true if <value> is found in <array>"
"Module: array"
"MandatoryArg:	<array> The array of possible values. can be a single value."
"MandatoryArg:	<value> The value to search for"
"Example: array::contains(array_numbers_1_to_10, 5) || array::contains(8, 8);"
@/
function contains( array_or_val, value )
{
	if ( isArray ( array_or_val ) )
	{
		foreach( element in array_or_val )
		{
			if ( element === value )
			{
				return true;
			}
		}
		
		return false;
	}
	
	return array_or_val === value;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function _filter_dead( val )
{
	return IsAlive( val );
}

function _filter_classname( val, arg )
{
	return IsSubStr( val.classname, arg );
}

// Quick Sort - pass it an array it will come back sorted
function quickSort(array, compare_func) 
{
    return quickSortMid(array, 0, array.size -1, compare_func);     
}

function quickSortMid(array, start, end, compare_func)
{
	i = start;
	k = end;

    if(!IsDefined(compare_func))
        compare_func = &quickSort_compare;
    
	if (end - start >= 1)
    {
        pivot = array[start];  

        while (k > i)         
        {
            while ( [[ compare_func ]](array[i], pivot) && i <= end && k > i)
	        	i++;                                 
            while ( ![[ compare_func ]](array[k], pivot) && k >= start && k >= i)
	            k--;                                      
	        if (k > i)                                 
	           swap(array, i, k);                    
        }
        swap(array, start, k);                                               
        array = quickSortMid(array, start, k - 1, compare_func); 
        array = quickSortMid(array, k + 1, end, compare_func);   
    }
	else
    	return array;
    
    return array;
}

function quicksort_compare(left, right)
{
    return left<=right;
}