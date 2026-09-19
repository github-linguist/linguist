//this script should be run automatically after the king is liberated. which ends hardcore/ronin restrictions

import <scripts/ttpack/util/tt_util.ash>

void changeSettingsForAftercore()
{
	//change assorted mafia settings for aftercore.
	print("Configuring recovery settings for aftercore", "blue");
	set_property("recoveryScript", "scripts\\Universal_recovery.ash");
	set_property("hpAutoRecovery", "0.7");
	set_property("hpAutoRecoveryTarget", "1.0");
	set_property("mpAutoRecovery", "0.1");
	set_property("mpAutoRecoveryTarget", "0.15");
	set_property("manaBurningTrigger", "1.0");
	set_property("manaBurningThreshold", "0.5");
	cli_execute("ccs aftercore");
}

void main()
{
	if(!inAftercore())
	{
		abort("This script should only be run after the king was liberated");
	}
	print("Running tt_kingliberated script", "blue");
	
	changeSettingsForAftercore();			//change assorted mafia settings for aftercore.
	
	print("Pulling all items from hangk's ancestral storage", "blue");
	cli_execute("pull all");				//pull all hangk items
	
	cli_execute("tt_login.ash");			//run login script
	cli_execute("tt_fortune.ash");			//reply and ask for zatara fortunes.
	cli_execute("pvprotect.ash");			//closet pvp stealable items
	tt_snapshot();							//run cc_snapshot if applicable to showoff your greenboxes

	if(get_property("lastDesertUnlock").to_int() != my_ascensions())	//do not have desert access
	{
		print("acquire bitchin\' meatcar", "blue");
		retrieve_item(1, $item[Bitchin\' Meatcar]);
	}

	if(pvp_attacks_left() > 0)
	{
		cli_execute("maximize food drop");	//prepare outfit
		cli_execute("pvp flowers 0");		//burn remaining pvp fights. set for average season.
	}

	print("tt_kingliberated script finished", "green");
}