script "newDG.ash"

import <aen_utils.ash>
import <aen_bastille.ash>
import <aen_pantogram.ash>
import <aen_fortune.ash>

if(!get_property("noDG").to_boolean() && user_confirm("Would you like to tell Aenimus that you're using this script? It would be nice to know, but feel free to say no.")) {
	notify aenimus;
	set_property("noDG", "true");
} else {
	set_property("noDG", "true");
}

visit_url("main.php");
if(last_choice() == 1343) {
	run_choice(1);
}	

visit_url("main.php");
if(last_choice() == 1342) {
	if(!get_property("_newDGSkills").to_boolean()) {
		if(user_confirm("If you have the maximum HP for all the DG skills, click yes. If not, please click no to allocate skills manually.")) {
			if(user_confirm("Do you want the muscle-equalizing skill? Without: scaling fights = easier, but less fast healing and requires reset if you have level 13 muscle test.")) {
			print("Taking muscle equalizer and savage bite.");
				visit_url("choice.php?whichchoice=1342&option=2&pwd=" + my_hash() + "&sk[]=10&sk[]=11&sk[]=12&sk[]=13&sk[]=14&sk[]=15&sk[]=16&sk[]=17&sk[]=18&sk[]=22&sk[]=23&sk[]=24&sk[]=25&sk[]=26&sk[]=27&sk[]=28&sk[]=30&sk[]=31&sk[]=32&sk[]=33&sk[]=34&sk[]=35&sk[]=36&sk[]=37&sk[]=38");
				visit_url("choice.php?whichchoice=1342&option=1&pwd=" + my_hash());
				set_property("_newDGSkills", "true");
			} else {
			print("Not taking muscle equalizer and savage bite.");			
				visit_url("choice.php?whichchoice=1342&option=2&pwd=" + my_hash() + "&sk[]=11&sk[]=12&sk[]=13&sk[]=14&sk[]=16&sk[]=17&sk[]=18&sk[]=22&sk[]=23&sk[]=24&sk[]=25&sk[]=26&sk[]=27&sk[]=28&sk[]=30&sk[]=31&sk[]=32&sk[]=33&sk[]=34&sk[]=35&sk[]=36&sk[]=37&sk[]=38");
				visit_url("choice.php?whichchoice=1342&option=1&pwd=" + my_hash());
				set_property("_newDGSkills", "true");
			}

		} else {
			set_property("_newDGSkills", "true");
			abort("Please allocate your skills manually, confirm, rest for millennia and then re-run.");
		}
	}
}

if(DG()) {

	// Stuffies
	if(get_clan_rumpus() contains "Mr. Klaw \"Skill\" Crane Game") {
		while(get_property("_klawSummons").to_int() < 3) {
			print("Collecting rumpus stuffies.", "blue");
			visit_url("clan_rumpus.php?action=click&spot=3&furni=3");
		}
	}

	if(have($item[Clan VIP Lounge key])) {
		while(get_property("_deluxeKlawSummons").to_int() < 3) {
			print("Collecting VIP stuffies.", "blue");
			visit_url("clan_viplounge.php?action=klaw");
		}
	}

	// Toot Oriole stolen from Bale
	if(!get_property("_tootLetter").to_boolean()) {
		print("Collecting the Toot Oriole letter.", "blue");
		visit_url("tutorial.php?action=toot&pwd");
		item tootletter = $item[letter from King Ralph XI];
		if(my_path() == "Actually Ed the Undying") {
			tootletter = $item[letter to Ed the Undying];
		}
		if(have(tootletter)) {
			print("Using the Toot Oriole letter.", "blue");
			use(1, tootletter);
		}
		set_property("_tootLetter", "true");
	}
		
	// Collect stones
	if(have(sack)) {
		print("Collecting stones.", "blue");
		use(1, sack);
		if(have(por)) {
			print("You received " + amt(bac) + " baconstones, " + amt(ham) + " hamethysts and " + amt(por) + " porquoises (precioussss).", "blue");
		} else {
			print("You received " + amt(bac) + " baconstones, " + amt(ham) + " hamethysts but OMG no porquoises! Drop to casual! ", "red");
		}	
	}
	
	// PANTOGRAM
	if(pantogram_can_summon()) {
		
		if(knoll_available() && amt(por) < 1) {
			print("Buying a taco shell for Pantogram because we have no porquoises (Gollum! GOLLUM!).", "blue");
			buyUntil(1, $item[taco shell]);
		}
		
		pantogram_summon();
		wield(pantogram_pants);
		
	}
	

	if(!get_property("_aen_ppants").to_boolean() && user_confirm("Safety check that your porquoise is safely in your pantogram pants.")) {
		set_property("_aen_ppants", "true");
	} else {
		abort("Please do your pantogram pants manually.");
		set_property("_aen_ppants", "true");
	}
	
	// AUTOSELLING
	if(!get_property("aen_stuffies").to_boolean() && user_confirm("Do you want to give the script permanent permission to sell your in-run klaw stuffies?")) {
		set_property("aen_stuffies", "true");
	}
	if(get_property("aen_stuffies").to_boolean()) {
		foreach junk in $items[club necklace, diamond necklace, spade necklace, rubber WWBD? bracelet, rubber WWJD? bracelet, rubber WWSPD? bracelet, rubber WWtNSD? bracelet,
		stuffed angry cow, stuffed astral badger, stuffed baby gravy fairy, stuffed Cheshire bitten, stuffed cocoabo, stuffed flaming gravy fairy, stuffed frozen gravy fairy, stuffed hand turkey,
		stuffed MagiMechTech MicroMechaMech, stuffed mind flayer, stuffed scary death orb, stuffed sleazy gravy fairy, stuffed snowy owl, stuffed spooky gravy fairy, stuffed stinky gravy fairy,
		stuffed undead elbow macaroni, stuffed Meat, Newbiesport&trade; tent]
	
		autosell(amt(junk), junk);
	}
	
	if(!get_property("_aen_stones").to_boolean()) {
	
		if(have(por) && user_confirm("Do you want to sell your porquoises?")) {
			autosell(amt(por), por);
		}
		
		if(have(bac)) {
			if(in_mysticality_sign() && user_confirm("Do you want to sell your baconstones? Can potentially be used for a +50% myst potion in gnome sign.")) {
				autosell(amt(bac), bac);
			} else if(!in_mysticality_sign() && user_confirm("Do you want to sell your baconstones?")) {
				autosell(amt(bac), bac);
			}
		}
				
		if(have(ham) && user_confirm("Do you want to sell your hamethysts?")) {
			autosell(amt(ham), ham);
		}
		
		set_property("_aen_stones", "true");

	}
	
	// BLOOD BAGS maybe check for collection?
	if(amt($item[blood bag]) < 5) {
		visit_url("place.php?whichplace=town_right&action=town_bloodbank");
	}
	
	// BOXING DAYCARE
	if(get_property("daycareOpen").to_boolean() && get_property("_daycareGymScavenges").to_int() == 0) {
	
		visit_url("place.php?whichplace=town_wrong&action=townwrong_boxingdaycare");
		if(last_choice() == 1334) {
			if(!get_property("_daycareNap").to_boolean()) {
				run_choice(1);
			}
			run_choice(3);
			if(last_choice() == 1336) {
				run_choice(2);
				run_choice(5);
			}
		}
		
		if(last_choice() == 1334) {
			run_choice(4);
		}
	
		if(have($item[body spradium])) {
			print("You received a body spradium! Congratulations!", "green");
		} else {
			print("You did not receive a body spradium! Drop to casual!", "red");
		}
		
	}
	
	// Buy and set the radio
	if(in_muscle_sign() && amt($item[Detuned Radio]) == 0) {
		print("Buying and using a detuned radio.", "blue");
		buyUntil(1, $item[Detuned Radio]);
		change_mcd(10);
	}
	
	// FANTASYREALM
	if(get_property("frAlways").to_boolean() && !get_property("_aen_lyleHat").to_boolean()) {
		print("Collecting Lyle FantasyRealm hat.", "blue");
		item lyleHat;
		if(my_primestat() == $stat[muscle]) {
			// Lyle hat
			cli_execute("make FantasyRealm Warrior's Helm");
			lyleHat = $item[FantasyRealm Warrior's Helm];
			
			/* Mummery Muscle substats on the XO Skeleton
			if(my_familiar() != $familiar[XO Skeleton]) {
				cli_execute("mummery muscle");
			}*/
		} else if(my_primestat() == $stat[mysticality]) {
			cli_execute("make FantasyRealm Mage's Hat");
			lyleHat = $item[FantasyRealm Mage's Hat];
		} else if(my_primestat() == $stat[moxie]) {
			cli_execute("make FantasyRealm Rogue's Mask");
			lyleHat = $item[FantasyRealm Rogue's Mask];			
		}
		wield(lyleHat);
		set_property("_aen_lyleHat", "true");
	}
	
	if(have_effect($effect[Ceaseless Snarling]) < 1) {
		use_skill(1, $skill[Ceaseless Snarl]);
	}
	
	wield($item[vampyric cloake]);
	if(have($item[January's Garbage Tote])) {
		cli_execute("fold tinsel tights");
		wield($item[tinsel tights]);
	} else {
		wield($item[old sweatpants]);
	}
	wield(saber);
	wield($item[latte lovers member's mug]);
	wield($slot[acc1], $item[Kremlin's Greatest Briefcase]);
	if(!wield($slot[acc2], $item[Draftsman's driving gloves])) {
		wield($slot[acc2], $item[astral belt]);
		wield($slot[acc2], $item[astral mask]);
	}
	wield($item[Lil' Doctor&trade; bag]);
	
	// Seal Tooth
	if(!have($item[seal tooth]) && user_confirm("Do you want to fish for a seal tooth? The script won't spend more than 150 meat at at time without permission.")) {
		int spentMeat = 0;
		while(!have($item[seal tooth])) {
			int meat = my_meat();
			while(!have($item[worthless trinket]) && !have($item[worthless gewgaw]) && !have($item[worthless knick-knack]) && my_meat() > meat - 150) {
				buyUntil(1, $item[chewing gum on a string]);
				spentMeat++;
				use(1, $item[chewing gum on a string]);
			}
			if(have($item[worthless trinket]) || have($item[worthless gewgaw]) || have($item[worthless knick-knack])) {
				cli_execute("hermit seal tooth");
				print("You spent " + spentMeat * 50 + " meat to acquire a seal tooth.");
			} else if(!user_confirm("You have already spent " + spentMeat * 50 + " meat so far to purchase a seal tooth. Do you wish to continue?")) {
				print("You did not acquire a seal tooth.", "red");
				break;
			}
		}
	}
	
	// Bastille
	if(get_property("_bastilleGames").to_int() == 0 && have(bastille)) {
		print("Using bastille now so we don't die while looking for a goblin. RIP possible 1 turn of buff.", "blue");
		if(user_confirm("Do you want the +meat% drop bastille buff?")) {
			bastilleBatallion(1, 2, 3, 0);
		} else if(user_confirm("Do you want the +hp regen bastille buff?")) {
			bastilleBatallion(1, 2, 1, 0);
		} else if(user_confirm("Do you want the +myst bastille buff?")) {
			bastilleBatallion(1, 2, 2, 0);
		} else {
			abort("You did not pick any of the buff options.");
		}
	}
	
	if(!avail($item[continuum transfunctioner])) {
	print("Acquiring the continuum transfunctioner.", "blue");
		visit_url("council.php");
		visit_url("place.php?whichplace=forestvillage&action=fv_mystic");
		run_choice(1);
		run_choice(1);
		run_choice(1);
	}
	
	// Set SongBoom song to meat
	if(have($item[SongBoom&trade; BoomBox]) && get_property("boomBoxSong") != "Total Eclipse of Your Meat") {
		if(user_confirm("Do you want to set the SongBoom BoomBox to +meat%?")) {
			print("Setting and adding a change counter for the SongBoom BoomBox.", "blue");
			cli_execute("boombox meat");
			cli_execute("counters add " + (10 - get_property("_boomBoxFights").to_int()) + " BoomBox reward");
		}
	}
	
	print("Please manually put KGB onto ML and collect 3 safety cigars.", "red");
	
	if(user_confirm("First go to the outskirts of cobb's knob with 80ML, cast dark feast and then ensorcel on any goblin, then click yes.")) {
	} else {
		abort("First go to the outskirts of cobb's knob, cast dark feast and then ensorcel on any goblin, then rerun.");
	}
	// Set SongBoom song to meat
	if(have($item[SongBoom&trade; BoomBox])) {
	if(get_property("_boomBoxFights").to_int() != 9) {
		print("It would appear you were unlucky. Recalibrating the BoomBox counter.", "blue");
		stop_counter("BoomBox reward");
		cli_execute("counters add " + (10 - get_property("_boomBoxFights").to_int()) + " BoomBox reward");
	}
	
	if(avail(saber) && !get_property("_saberUpgrade").to_boolean()) {
			visit_url("main.php?action=may4");
		if(user_confirm("Do you want the +ML saber upgrade?")) {
			print("Setting the saber to +ML.", "blue");
			run_choice(2);
		} else if(user_confirm("Do you want the +res saber upgrade?")) {
			print("Setting the saber to +res.", "blue");
			run_choice(3);
		} else {
			print("Please pick your saber upgrade manually. Weirdo.", "red");
		}
		set_property("_saberUpgrade", "true");
	}
	
	// Horsery
	if(get_property("horseryAvailable").to_boolean() && get_property("_horsery") == "") {
		if(user_confirm("Do you want the dark horse for +meat and +NC?")) {
			print("Taking the dark horse for meat and +NC.", "blue");
			if(get_property("_horsery") != "dark horse") {
				string temp = visit_url("place.php?whichplace=town_right&action=town_horsery");
				temp = visit_url("choice.php?pwd=&whichchoice=1266&option=2");
			}
		} else if(!user_confirm("Take the horse of your choice and then click yes.")) {
			abort("Take the horse of your choice and then rerun.");
		}
	}
	
	// Consults
	if(get_property("_clanFortuneConsultUse").to_int() < 3 && have($item[Clan VIP Lounge key])) {
		print("Obtaining Fortune Teller buff and items.", "blue");
		cli_execute("fortune buff mys");
	}
		while(clanmateConsult(""));
	}
	
	// Counters
	if(get_property("voteAlways").to_boolean()) {
		print("Setting some vote wanderer counters.", "blue");
		cli_execute("counters add " + to_string((12 - (total_turns_played() % 11)) % 11) + " Democratic Wanderer");
		set_property("trackVoteMonster", "true");
	}

	// Miscellaneous
	if(!get_property("_defectiveTokenChecked").to_boolean()) {
		visit_url("place.php?whichplace=arcade&action=arcade_plumber", false);
	}
	
	if(have_effect($effect[Glittering Eyelashes]) < 1) {
		buyUntil(1, $item[glittery mascara]);
		use(1, $item[glittery mascara]);
	}
	
	if(get_campground() contains $item[packet of tall grass seeds]) {
		visit_url("/campground.php?action=garden&pwd=" + my_hash());
	}
	
	if(get_campground() contains $item[potted tea tree] && !get_property("_pottedTeaTreeUsed").to_boolean()) {
		if(user_confirm("Do you want to shake your tree? +90 meat, but aftercore losses.")) {
			visit_url("/campground.php?action=teatree");
			run_choice(1);
		}
	}
	
	if(get_property("daycareOpen").to_boolean() && !get_property("_daycareSpa").to_boolean() && user_confirm("Do you want to acquire your daycare mainstat buff (probably yes)?")) {
		cli_execute("daycare mysticality");
	}
	
	if(avail($item[Daily Affirmation: Keep Free Hate in your Heart])){
		if(user_confirm("Do you want to use your Affirmation +ML buff (probably yes)?")) {
			use(1, $item[Daily Affirmation: Keep Free Hate in your Heart]);
		}
	}

if(get_property("telescopeUpgrades").to_int() > 1) {
	cli_execute("telescope");
	string t1;
	if(contains_text(get_property("telescope1"), "muscles")) {
		t1 = "muscle";
	} else if(contains_text(get_property("telescope1"), "puzzles")) {
		t1 = "mysticality";
	} else if(contains_text(get_property("telescope1"), "sunglasses")) {
		t1 = "moxie";
	}
	string t2 = "impossible to determine";
	if(contains_text(get_property("telescope2"), "fire")) {
		t2 = "hot (8-bit realm semi-rare)";
	} else if(contains_text(get_property("telescope2"), "eldritch")) {
		t2 = "spooky (spooky forest semi-rare)";
	} else if(contains_text(get_property("telescope2"), "skulking")) {
		t2 = "sleaze (whitey's grove semi-rare)";
	} else if(contains_text(get_property("telescope2"), "garbage")) {
		t2 = "stench (guano junction semi-rare)";
	} else if(contains_text(get_property("telescope2"), "igloo")) {
		t2 = "cold (haunted kitchen semi-rare)";
	}
	print("Your level 13 quest tests are " + t1 + " and " + t2 + ".", "purple");
}
	print("Remember to check today's bounties, KGB tab(s) and prepare PirateRealm and vote for your intrinsics/badge if you have it!", "red");
}