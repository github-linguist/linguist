#using scripts\shared\util_shared;

#insert scripts\shared\shared.gsh;

#namespace math;

function cointoss()
{
	return RandomInt( 100 ) >= 50;
}

/@
"Name: clamp(val, val_min, val_max)"
"Summary: Clamps a value between a min and max value."
"Module: Math"
"MandatoryArg: val: the value to clamp."
"MandatoryArg: val_min: the min value to clamp to."
"MandatoryArg: val_max: the mac value to clamp to."
"Example: clamped_val = clamp(8, 0, 5); // returns 5	*	clamped_val = clamp(-1, 0, 5); // returns 0"
"SPMP: both"
@/
function clamp( val, val_min, val_max )
{
	DEFAULT( val_max, val );
	
	if (val < val_min)
	{
		val = val_min;
	}
	else if (val > val_max)
	{
		val = val_max;
	}

	return val;
}

/@
"Name: linear_map(val, min_a, max_a, min_b, max_b)"
"Summary: Maps a value within one range to a value in another range."
"Module: Math"
"MandatoryArg: val: the value to map."
"MandatoryArg: min_a: the min value of the range in which <val> exists."
"MandatoryArg: max_a: the max value of the range in which <val> exists."
"MandatoryArg: min_b: the min value of the range in which the return value should exist."
"MandatoryArg: max_b: the max value of the range in which the return value should exist."
"Example: fov = linear_map(speed, min_speed, max_speed, min_fov, max_fov);"
"SPMP: both"
@/
function linear_map(num, min_a, max_a, min_b, max_b)
{
	return clamp(( (num - min_a) / (max_a - min_a) * (max_b - min_b) + min_b ), min_b, max_b);
}

/@
"Name: lag(desired, curr, k, dt)"
"Summary: Changes a value from current to desired using 1st order differential lag."
"Module: Math"
"MandatoryArg: desired: desired value."
"MandatoryArg: curr: the current value."
"MandatoryArg: k: the strength of the lag ( lower = slower, higher = faster)."
"MandatoryArg: dt: time step to lag over ( usually 1 server frame )."
"Example: speed = lag(max_speed, speed, 1, 0.05);"
"SPMP: both"
@/
function lag(desired, curr, k, dt)
{
    r = 0.0;

    if (((k * dt) >= 1.0) || (k <= 0.0))
    {
        r = desired;
    }
    else
    {
        err = desired - curr;
        r = curr + k * err * dt;
    }

    return r;
}

function find_box_center( mins, maxs )
{
	center = ( 0, 0, 0 );
	center = maxs - mins;
	center = ( center[0]/2, center[1]/2, center[2]/2 ) + mins;
	return center;
}

function expand_mins( mins, point )
{
	if ( mins[0] > point[0] )
	{
		mins = ( point[0], mins[1], mins[2] );
	}

	if ( mins[1] > point[1] )
	{
		mins = ( mins[0], point[1], mins[2] );
	}

	if ( mins[2] > point[2] )
	{
		mins = ( mins[0], mins[1], point[2] );
	}

	return mins;
}

function expand_maxs( maxs, point )
{
	if ( maxs[0] < point[0] )
	{
		maxs = ( point[0], maxs[1], maxs[2] );
	}

	if ( maxs[1] < point[1] )
	{
		maxs = ( maxs[0], point[1], maxs[2] );
	}

	if ( maxs[2] < point[2] )
	{
		maxs = ( maxs[0], maxs[1], point[2] );
	}

	return maxs;
}

// ----------------------------------------------------------------------------------------------------
// -- Vectors -----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------

/@
"Name: vector_compare( <vec1>, <vec2> )"
"Summary: For 3D vectors.  Returns true if the vectors are the same"
"MandatoryArg: <vec1> : A 3D vector (origin)"
"MandatoryArg: <vec2> : A 3D vector (origin)"
"Example: if (vector_compare(self.origin, node.origin){print(\"yay, i'm on the node!\");}"
"SPMP: both"
@/
function vector_compare(vec1, vec2)
{
	return (abs(vec1[0] - vec2[0]) < .001) && (abs(vec1[1] - vec2[1]) < .001) && (abs(vec1[2] - vec2[2]) < .001);
}

function random_vector(max_length)
{
	return (RandomFloatRange(-1 * max_length, max_length), RandomFloatRange(-1 * max_length, max_length), RandomFloatRange(-1 * max_length, max_length));
}

function angle_dif(oldangle, newangle)
{
	outvalue = ( oldangle - newangle ) % 360;
	
	if ( outvalue < 0 )
	{
		outvalue+=360;
	}
	
	if ( outvalue > 180 )
	{
		outvalue=(outvalue-360)*-1;
	}
	
	return outvalue;		
}

function sign( x )
{
	return ( x >= 0 ? 1 : -1 );
}

function randomSign()
{
	return ( RandomIntRange( -1, 1 ) >= 0 ? 1 : -1 );
}

/@
"Name: get_dot_direction( <v_point>, [b_ignore_z], [b_normalize], [str_direction], [ b_use_eye] )"
"Summary: Calculates and returns dot between an entity's directional vector and a point."
"Module: Math"
"CallOn: Entity. Must have origin and angles parameters."
"MandatoryArg: <v_point> vector position to check against entity origin and angles"
"OptionalArg: <b_ignore_z> specify if get_dot should consider 2d or 3d dot. Defaults to false for 3d dot."
"OptionalArg: <str_direction> specify which vector type to use on angles. Valid options are "forward", "backward", "right", "left", "up" and "down". Defaults to "forward"."
"OptionalArg: <b_normalize> specify if the function should normalize the vector to target point. Defaults to true."
"OptionalArg: <b_use_eye> if self a player or AI, use tag_eye rather than .angles. Defaults to true on players, defaults to false on everything else.
"Example: n_dot = player get_dot_direction( woods.origin );"
"SPMP: singleplayer"
@/
function get_dot_direction( v_point, b_ignore_z, b_normalize, str_direction, b_use_eye )
{
	assert( isdefined( v_point ), "v_point is a required parameter for get_dot" );
	
	if ( !isdefined( b_ignore_z ) )
	{
		b_ignore_z = false;
	}
	
	if ( !isdefined( b_normalize ) )
	{
		b_normalize = true;
	}
	
	if ( !isdefined( str_direction ) )
	{
		str_direction = "forward";
	}
	
	if ( !isdefined( b_use_eye ) )
	{
		b_use_eye = false;
		
		if ( IsPlayer( self ) )
		{
			b_use_eye = true;
		}
	}
	
	v_angles = self.angles;
	v_origin = self.origin;
	
	if ( b_use_eye )
	{
		v_origin = self util::get_eye();
	}
	
	if ( IsPlayer( self ) )
	{
		v_angles = self GetPlayerAngles();
		if ( level.wiiu )
		{
			v_angles = self GetGunAngles();
		}
	}
	
	if ( b_ignore_z )
	{
		v_angles = ( v_angles[ 0 ], v_angles[ 1 ], 0 );
		v_point = ( v_point[ 0 ], v_point[ 1 ], 0 );
		v_origin = ( v_origin[ 0 ], v_origin[ 1 ], 0 );
	}
	
	switch ( str_direction )
	{
		case "forward":
			v_direction = AnglesToForward( v_angles );
			break;
		
		case "backward":
			v_direction = AnglesToForward( v_angles ) * ( -1 );
			break;
			
		case "right":
			v_direction = AnglesToRight( v_angles );
			break;
			
		case "left":
			v_direction = AnglesToRight( v_angles ) * ( -1 );
			break;
			
		case "up":
			v_direction = AnglesToUp( v_angles );
			break;
		
		case "down":
			v_direction = AnglesToUp( v_angles ) * ( -1 );
			break;
			
		default:
			AssertMsg( str_direction + " is not a valid str_direction for get_dot!" );
			v_direction = AnglesToForward( v_angles );   // have to initialize variable for default case
			break;
	}
	
	v_to_point = v_point - v_origin;
	
	if ( b_normalize )
	{
		v_to_point = VectorNormalize( v_to_point );
	}
	
	n_dot = VectorDot( v_direction, v_to_point );
	
	return n_dot;
}

/@
"Name: get_dot_right( <v_point>, [b_ignore_z], [b_normalize] )"
"Summary: Calculates and returns dot between an entity's right vector and a point."
"Module: Math"
"CallOn: Entity. Must have origin and angles parameters."
"MandatoryArg: <v_point> vector position to check against entity origin and angles"
"OptionalArg: <b_ignore_z> specify if get_dot should consider 2d or 3d dot. Defaults to false for 3d dot."
"OptionalArg: <b_normalize> specify if the function should normalize the vector to target point. Defaults to true."
"Example: n_dot = player get_dot_direction( woods.origin );"
"SPMP: singleplayer"
@/
function get_dot_right( v_point, b_ignore_z, b_normalize )
{
	// get_dot will assert if missing, but scripter should know it's coming from get_dot_right
	assert( isdefined( v_point ), "v_point is a required parameter for get_dot_right" );
	
	n_dot = get_dot_direction( v_point, b_ignore_z, b_normalize, "right" );
	
	return n_dot;
}

/@
"Name: get_dot_up( <v_point>, [b_ignore_z], [b_normalize] )"
"Summary: Calculates and returns dot between an entity's up vector and a point."
"Module: Math"
"CallOn: Entity. Must have origin and angles parameters."
"MandatoryArg: <v_point> vector position to check against entity origin and angles"
"OptionalArg: <b_ignore_z> specify if get_dot should consider 2d or 3d dot. Defaults to false for 3d dot."
"OptionalArg: <b_normalize> specify if the function should normalize the vector to target point. Defaults to true."
"Example: n_dot = player get_dot_direction( woods.origin );"
"SPMP: singleplayer"
@/
function get_dot_up( v_point, b_ignore_z, b_normalize )
{
	// get_dot will assert if missing, but scripter should know it's coming from get_dot_up
	assert( isdefined( v_point ), "v_point is a required parameter for get_dot_up" );
	
	n_dot = get_dot_direction( v_point, b_ignore_z, b_normalize, "up" );
	
	return n_dot;
}

/@
"Name: get_dot_forward( <v_point>, [b_ignore_z], [b_normalize] )"
"Summary: Calculates and returns dot between an entity's forward vector and a point."
"Module: Math"
"CallOn: Entity. Must have origin and angles parameters."
"MandatoryArg: <v_point> vector position to check against entity origin and angles"
"OptionalArg: <b_ignore_z> specify if get_dot should consider 2d or 3d dot. Defaults to false for 3d dot."
"OptionalArg: <b_normalize> specify if the function should normalize the vector to target point. Defaults to true."
"Example: n_dot = player get_dot_direction( woods.origin );"
"SPMP: singleplayer"
@/
function get_dot_forward( v_point, b_ignore_z, b_normalize )
{
	// get_dot will assert if missing, but scripter should know it's coming from get_dot_forward
	assert( isdefined( v_point ), "v_point is a required parameter for get_dot_forward" );
	
	n_dot = get_dot_direction( v_point, b_ignore_z, b_normalize, "forward" );
	
	return n_dot;
}

/@
"Name: get_dot_from_eye( <v_point>, [b_ignore_z], [b_normalize], [str_direction] )"
"Summary: Calculates and returns dot between an entity's forward vector and a point based on tag_eye. Only use on players or AI"
"Module: Math"
"CallOn: Entity. Must have origin and angles parameters."
"MandatoryArg: <v_point> vector position to check against entity origin and angles"
"OptionalArg: [b_ignore_z] specify if get_dot should consider 2d or 3d dot. Defaults to false for 3d dot."
"OptionalArg: [b_normalize] specify if the function should normalize the vector to target point. Defaults to true."
"OptionalArg: [str_direction] specify which vector type to use on angles. Valid options are "forward", "backward", "right", "left", "up" and "down". Defaults to "forward"."
"Example: n_dot = player get_dot_from_eye( woods.origin );"
"SPMP: singleplayer"
@/
function get_dot_from_eye( v_point, b_ignore_z, b_normalize, str_direction )
{
	assert( isdefined( v_point ), "v_point is a required parameter for get_dot_forward" );
	Assert( ( IsPlayer( self ) || IsAI( self ) ), "get_dot_from_eye was used on a " + self.classname + ". Valid ents are players and AI, since they have tag_eye." );
	
	n_dot = get_dot_direction( v_point, b_ignore_z, b_normalize, str_direction, true );
	
	return n_dot;
}

// ----------------------------------------------------------------------------------------------------
// -- Arrays ------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------

/@
"Name: array_average( <array> )"
"Summary: Given an array of numbers, returns the average (mean) value of the array"
"Module: Utility"
"MandatoryArg: <array>: the array of numbers which will be averaged"
"Example: array_average( numbers );"
"SPMP: both"
@/
function array_average( array )
{
	assert( IsArray( array ) );
	assert( array.size > 0 );

	total = 0;

	for ( i = 0; i < array.size; i++ )
	{
		total += array[i];
	}

	return ( total / array.size );
}

/@
"Name: array_std_deviation( <array>, <mean> )"
"Summary: Given an array of numbers and the average of the array, returns the standard deviation value of the array"
"Module: Utility"
"MandatoryArg: <array>: the array of numbers"
"MandatoryArg: <mean>: the average (mean) value of the array"
"Example: array_std_deviation( numbers, avg );"
"SPMP: both"
@/
function array_std_deviation( array, mean )
{
	assert( IsArray( array ) );
	assert( array.size > 0 );

	tmp = [];
	for ( i = 0; i < array.size; i++ )
	{
		tmp[i] = ( array[i] - mean ) * ( array[i] - mean );
	}

	total = 0;
	for ( i = 0; i < tmp.size; i++ )
	{
		total = total + tmp[i];
	}

	return Sqrt( total / array.size );
}

/@
"Name: random_normal_distribution( <mean>, <std_deviation>, <lower_bound>, <upper_bound> )"
"Summary: Given the mean and std deviation of a set of numbers, returns a random number from the normal distribution"
"Module: Utility"
"MandatoryArg: <mean>: the average (mean) value of the array"
"MandatoryArg: <std_deviation>: the standard deviation value of the array"
"OptionalArg: <lower_bound> the minimum value that will be returned"
"OptionalArg: <upper_bound> the maximum value that will be returned"
"Example: random_normal_distribution( avg, std_deviation );"
"SPMP: both"
@/
function random_normal_distribution( mean, std_deviation, lower_bound, upper_bound )
{
	//pixbeginevent( "random_normal_distribution" );

	// implements the Box-Muller transform for Gaussian random numbers (http://en.wikipedia.org/wiki/Box-Muller_transform)
	x1 = 0;
	x2 = 0;
	w = 1;
	y1 = 0;

	while ( w >= 1 )
	{
		x1 = 2 * RandomFloatRange( 0, 1 ) - 1;
		x2 = 2 * RandomFloatRange( 0, 1 ) - 1;
		w = x1 * x1 + x2 * x2;
	}

	w = Sqrt( ( -2.0 * Log( w ) ) / w );
	y1 = x1 * w;

	number = mean + y1 * std_deviation;

	if ( isdefined( lower_bound ) && number < lower_bound )
	{
		number = lower_bound;
	}

	if ( isdefined( upper_bound ) && number > upper_bound )
	{
		number = upper_bound;
	}

	//pixendevent();

	return( number );
}

function closest_point_on_line( point, lineStart, lineEnd )
{
	lineMagSqrd = lengthsquared(lineEnd - lineStart);
 
  t =	( ( ( point[0] - lineStart[0] ) * ( lineEnd[0] - lineStart[0] ) ) +
			( ( point[1] - lineStart[1] ) * ( lineEnd[1] - lineStart[1] ) ) +
			( ( point[2] - lineStart[2] ) * ( lineEnd[2] - lineStart[2] ) ) ) /
			( lineMagSqrd );
 
  if( t < 0.0  )
	{
		return lineStart;
	}
	else if( t > 1.0 )
	{
		return lineEnd;
	}

	start_x = lineStart[0] + t * ( lineEnd[0] - lineStart[0] );
	start_y = lineStart[1] + t * ( lineEnd[1] - lineStart[1] );
	start_z = lineStart[2] + t * ( lineEnd[2] - lineStart[2] );
	
	return (start_x,start_y,start_z);
}

function get_2d_yaw( start, end )
{
	vector = (end[0] - start[0], end[1] - start[1], 0);

	return vec_to_angles( vector );
}

function vec_to_angles( vector )
{
	yaw = 0;
	
	vecX = vector[0];
	vecY = vector[1];
	
	if ( vecX == 0 && vecY == 0 )
		return 0;
		
	if ( vecY < 0.001 && vecY > -0.001 )
		vecY = 0.001;

	yaw = atan( vecX / vecY );
	
	if ( vecY < 0 )
		yaw += 180;

	return ( 90 - yaw );
}

function pow( base, exp )
{
	if( exp == 0 )
	{
		return 1;
	}

	result = base;
	for( i = 0; i < ( exp - 1 ); i++ )
	{
		result  *= base;
	}
	
	return result;
}
