/*
*	This is a sample script.
*/

#include "BotManagerInterface.acs"

BotManager::BotManager g_BotManager( @CreateDumbBot );

CConCommand@ m_pAddBot;

void PluginInit()
{
	g_BotManager.PluginInit();
	
	@m_pAddBot = @CConCommand( "addbot", "Adds a new bot with the given name", @AddBotCallback );
}

void AddBotCallback( const CCommand@ args )
{
	if( args.ArgC() < 2 )
	{
		g_Game.AlertMessage( at_console, "Usage: addbot <name>" );
		return;
	}
	
	BotManager::BaseBot@ pBot = g_BotManager.CreateBot( args[ 1 ] );
	
	if( pBot !is null )
	{
		g_Game.AlertMessage( at_console, "Created bot " + args[ 1 ] + "\n" );
	}
	else
	{
		g_Game.AlertMessage( at_console, "Could not create bot\n" );
	}
}

final class DumbBot : BotManager::BaseBot
{	
	DumbBot( CBasePlayer@ pPlayer )
	{
		super( pPlayer );
	}
	
	void Think()
	{
		BotManager::BaseBot::Think();
		
		// If the bot is dead and can be respawned, send a button press
		if( Player.pev.deadflag >= DEAD_RESPAWNABLE )
		{
			Player.pev.button |= IN_ATTACK;
		}
		else
			Player.pev.button &= ~IN_ATTACK;
		
		KeyValueBuffer@ pInfoBuffer = g_EngineFuncs.GetInfoKeyBuffer( Player.edict() );
		
		pInfoBuffer.SetValue( "topcolor", Math.RandomLong( 0, 255 ) );
		pInfoBuffer.SetValue( "bottomcolor", Math.RandomLong( 0, 255 ) );
		
		if( Math.RandomLong( 0, 100 ) > 10 )
			Player.pev.button |= IN_ATTACK;
		else
			Player.pev.button &= ~IN_ATTACK;
			
		for( uint uiIndex = 0; uiIndex < 3; ++uiIndex )
		{
			m_vecVelocity[ uiIndex ] = Math.RandomLong( -50, 50 );
		}
	}
}

BotManager::BaseBot@ CreateDumbBot( CBasePlayer@ pPlayer )
{
	return @DumbBot( pPlayer );
}
