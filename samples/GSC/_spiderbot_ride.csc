#include clientscripts\_utility; 
#include clientscripts\_filter;

#using_animtree("player");


init()
{
	clientscripts\_driving_fx::add_vehicletype_callback( "spiderbot_large", ::drive_spiderbot );
}


//
//
drive_spiderbot( localClientNum )
{
	self endon( "entityshutdown" );
	
	while( 1 )
	{
		self waittill( "enter_vehicle", player );
		
		init_filter_karma_spiderbot( player );
		enable_filter_karma_spiderbot( player, 0 );
                SetSavedDvar( "r_stereo3DEyeSeparationScaler", 0.01);

	
		//TODO: Get this to work in the vehicle GDT, somehow overcoming the lack of wheels
		if( isdefined( level._audio_spiderbot_override ) )
		{
			self thread [[level._audio_spiderbot_override]](player);
		}

		// This compass isn't working they way it needs to.		
		// Create the compass
//		spider_compass = Spawn( player GetLocalClientNumber(), player GetOrigin(), "script_model" );
//		spider_compass SetModel( "p6_3d_gizmo" );
//		spider_compass SetViewmodelRenderflag( true );
//		spider_compass LinkToCamera( 5, (16, 0, 12) );	// in/out, left/right, up/down

		self waittill( "exit_vehicle" );

		disable_filter_karma_spiderbot( player, 0 );
                SetSavedDvar( "r_stereo3DEyeSeparationScaler", 1);

//		spider_compass delete();
	}
}
