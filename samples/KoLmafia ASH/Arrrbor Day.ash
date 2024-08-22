since r20028;

import <vprops.ash>;

// Run this script on Arrrbor Day and it will collect the prize you
// earned for planting saplings last Arrrbor Day and will adventure
// sufficiently to get the prize of your choice next Arrrbor Day.

// You can either specify the prize you desire (a potion or an outfit
// piece from one of three kinds of tree) or let this script pick for
// you using the following rules:
//
// If you do not have the outfit (in inventory, closet, storage or your
// display case), pick a missing piece of equipment and spend 100+ turns
// adventuring in the Arrrboretum to set it up. (Optional; if you don't
// have the outfit and don't care, you can disable this.)
//
// Otherwise, pick the potion that you have the least of in those
// locations and spend 3 turns in the Arrboretum setting it up.

// ***************************
// *     Configuration       *
// ***************************

// If you are collecting a particular item, we can get it.

// A potion after 3 turns
//	handful of Crotchety Pine needles
//	lump of Saccharine Maple sap
//	handful of Laughing Willow bark
//
// Equipment after 100 turns
//	crotchety pants
//	Saccharine Maple pendant
//	willowy bonnet
//
// If you want this script to choose for you:
//	none

item target_item = define_property( "VAD.TargetItem", "item", "none" ).to_item();

// If you have no target item, we can adventure for 3 turns and get a potion
// or for 102 turns to get an outfit piece.
//
// We'll either pick an outfit piece you don't have or the potion you have the fewest of.
//
// If you don't want to worry about the outfit, set this to false.

boolean should_complete_outfit = define_property( "VAD.CompleteOutfit", "boolean", "true" ).to_boolean();

// If you want to test what this script will recommend for you, given your current
// collection of rewards and sacks of saplings, set this parameter to "true"

boolean testing = define_property( "VAD.Testing", "boolean", "false" ).to_boolean();

// ***************************
// *      Validation         *
// ***************************

static item NO_ITEM = $item[ none ];

static item_set valid_prizes = $items[
    handful of Crotchety Pine needles,
    lump of Saccharine Maple sap,
    handful of Laughing Willow bark,
    crotchety pants,
    Saccharine Maple pendant,
    willowy bonnet,
];

void validate_configuration()
{
    print( "Validating configuration." );

    boolean valid = true;

    if ( target_item != NO_ITEM &&
	 !( valid_prizes contains target_item ) ) {
	print( "VAD.TargetItem: '" + target_item + "' can't be found in the Arrrboretum.", "red" );
	valid = false;
    }

    if ( !valid ) {
	abort( "Correct those errors and try again." );
    }

    print( "All is well!" );
}

// ***************************
// *      Constants          *
// ***************************

static string ARRRBOR_DAY = "Arrrbor Day";
static location ARRRBORETUM = $location[ The Arrrboretum ];
static slot WEAPON = $slot[ weapon ];
static slot OFFHAND = $slot[ off-hand ];

static string COMPLETION_PROPERTY = "_VAD.Completed";

static int MAX_INT = 16777215;	// Not really, but maximum item count

record tree
{
    string name;	// Name of the tree
    int decision;	// choice option to get sack of saplings
    item sack;		// The sack you equip to get this kind of reward next time
    item potion;	// The potion you get with 1-99 saplings planted
    item equipment;	// The outfit piece you get with 100+ saplings planted
};

static tree[] trees = {
    new tree( "Crotchety Pine", 1,
	      $item[ bag of Crotchety Pine saplings ],
	      $item[ handful of Crotchety Pine needles ],
	      $item[ crotchety pants ] ),
    new tree( "Saccharine Maple", 2,
	      $item[ bag of Saccharine Maple saplings ],
	      $item[ lump of Saccharine Maple sap ],
	      $item[ Saccharine Maple pendant ] ),
    new tree( "Laughing Willow", 3,
	      $item[ bag of Laughing Willow saplings ],
	      $item[ handful of Laughing Willow bark ],
	      $item[ willowy bonnet ] ),
};

static tree [item] item_to_tree;
static item_set all_sacks;

static
{
    foreach i, t in trees {
	all_sacks[ t.sack ] = true;
	item_to_tree[ t.sack ] = t;
	item_to_tree[ t.potion ] = t;
	item_to_tree[ t.equipment ] = t;
    }
}

// ***************************
// *     Initialization      *
// ***************************

typedef int [item] item_to_int_map;	// get_inventory(), etc.

item_to_int_map sacks;
item_to_int_map potions;
item_to_int_map equipment;

void load_item_maps()
{
    int amount( item it )
    {
	return item_amount( it ) + closet_amount( it ) + storage_amount( it ) + display_amount( it );
    }

    foreach i, t in trees {
	// Sacks of saplings are quest items
	sacks[ t.sack ] = item_amount( t.sack );
	potions[ t.potion ] = amount( t.potion );
	equipment[ t.equipment ] = amount( t.equipment ) + equipped_amount( t.equipment );
    }
}

// ***************************
// *   Utility Functions     *
// ***************************

item rarest_item( item_to_int_map map )
{
    item make = NO_ITEM;
    int count = MAX_INT;
    foreach it, num in map {
	if ( num < count ) {
	    make = it;
	    count = num;
	}
    }
    return make;
}

item first_missing_item( item_to_int_map map )
{
    foreach it, num in map {
	if ( num == 0 ) {
	    return it;
	}
    }
    return NO_ITEM;
}

void print_item_map( item_to_int_map mapd, string label )
{
    print( label );
    foreach it, count in mapd {
	print( "\u00A0\u00A0\u00A0\u00A0" + it + " -> " + count );
    }
}

// ***************************
// *     Master Control      *
// ***************************

void main()
{
    if ( !testing ) {
	if ( !holiday().contains_text( ARRRBOR_DAY ) ) {
	    print( `Today is not {ARRRBOR_DAY}.` );
	    return;
	}
	if ( get_property( COMPLETION_PROPERTY ).to_boolean() ) {
	    print( "You have already celebrated Arrrbor Day today." );
	    return;
	}
    }

    // Validate configuration
    validate_configuration();

    item desired_prize;

    // Based on configration and items, choose desired prize
    // We could consider what you did last time. Or not.
    item choose_prize( item last_prize )
    {
	// Choose the best prize given configuration

	if ( target_item != NO_ITEM ) {
	    print( "Target item = " + target_item );
	    return target_item;
	}

	item outfit = equipment.first_missing_item();
	if ( should_complete_outfit && outfit != NO_ITEM ) {
	    print( "First missing outfit piece = " + outfit );
	    return outfit;
	}

	item potion = potions.rarest_item();
	print( "Rarest potion = " + potion );
	return potion;
    }

    item sack_needed( item prize )
    {
	return item_to_tree[ prize ].sack;
    }

    item choose_available_prize()
    {
	// Choose the best prize given configuration and available
	// sacks of saplings

	if ( target_item != NO_ITEM ) {
	    item sack = target_item.sack_needed();
	    if ( sacks[ sack ] > 0 ) {
		print( "Target item = " + target_item );
		return target_item;
	    }
	}

	if ( should_complete_outfit ) {
	    foreach it, num in equipment {
		if ( num > 0 ) {
		    continue;
		}
		item sack = it.sack_needed();
		if ( sacks[ sack ] > 0 ) {
		    print( "Missing outfit piece = " + it );
		    return it;
		}
	    }
	}

	item potion = NO_ITEM;
	int count = MAX_INT;
	foreach it, num in potions {
	    item sack = it.sack_needed();
	    if ( sacks[ sack ] > 0  && num < count ) {
		potion = it;
		count = num;
	    }
	}

	if ( potion != NO_ITEM ) {
	    print( "Rarest available potion = " + potion );
	    return potion;
	}

	print( "No sacks of saplings available" );
	return NO_ITEM;
    }

    int sapling_option( item prize )
    {
	return item_to_tree[ prize ].decision;
    }

    int saplings_needed( item prize )
    {
	if ( equipment contains prize ) {
	    print( `Plant 100 saplings today to get that item next {ARRRBOR_DAY}.` );
	    return 100;
	}

	if ( potions contains prize ) {
	    print( `Plant 1 sapling today to get that item next {ARRRBOR_DAY}.` );
	    return 1;
	}

	// Not expected; validation should have rejected anything else.
	return 0;
    }

    void assess_prizes()
    {
	load_item_maps();

	print( "" );
	print_item_map( sacks, "Saplings" );
	print_item_map( potions, "Potions" );
	print_item_map( equipment, "Equipment" );

	print( "" );
	print( "Choosing best prize" );
	item prize = choose_prize( NO_ITEM );
	int turns = prize.saplings_needed();

	print( "" );
	print( "Choosing best prize using available saplings" );
	prize = choose_available_prize();
	turns = prize.saplings_needed();
    }

    // Print informative logging
    assess_prizes();

    if ( testing ) {
	print( "" );
	print( "Done testing" );
	return;
    }

    if ( my_inebriety() > inebriety_limit() ) {
	print( "You are too drunk to plant saplings." );
	return;
    }

    string url = ARRRBORETUM.to_url();

    // If we have a sack of saplings equipped, unequip it.
    // We will be offered (and required to take) a new sack.
    if ( all_sacks contains equipped_item( OFFHAND ) ) {
	equip( OFFHAND, NO_ITEM );
    }

    item last_prize;
    item sack;
    int turns;

    string page = visit_url( url );

    void plant_a_tree()
    {
	// The sacks of saplings in inventory are available to us to
	// use. Pick a prize that is consistent with those.
	load_item_maps();

	// Choose the prize we would have picked, given available saplings
	desired_prize = choose_available_prize();

	int saplings_needed = desired_prize.saplings_needed();
	int saplings_planted = get_property( "_saplingsPlanted" ).to_int();

	if ( saplings_planted >= saplings_needed ) {
	    print( "You've already participated in Arrrbor Day this year." );
	    return;
	}

	sack = desired_prize.sack_needed();
	turns = saplings_needed - saplings_planted;
    }

    void stumped()
    {
	// Stumped
	page = run_choice( 1 );

	// Extract items for the page and see what we got.
	item_to_int_map map = page.extract_items();
	foreach it in map {
	    // We expect at most one item
	    last_prize = it;
	}

	// The item is now in our inventory. Load the maps that
	// tally our current holdings.
	load_item_maps();

	// Visit the Arrrboretum again to get Timbarrr!
	page = visit_url( url );
    }

    void timbarrr()
    {
	// Based on configuration and inventory, chooose prize
	desired_prize = choose_prize( last_prize );
	turns = desired_prize.saplings_needed();

	// Submit the choice and get the sack of saplings
	int decision = desired_prize.sapling_option();
	page = run_choice( decision );

	// Extract items for the page and see what we got.
	item_to_int_map map = page.extract_items();
	foreach it in map {
	    // We expect at most one item
	    sack = it;
	}
    }

    // Run through expected (and unexpected) choice adventures
    while ( handling_choice() ) {
	switch ( last_choice() ) {
	case 210:
	    stumped();
	    break;
	case 209:
	    timbarrr();
	    break;
	default:
	    // I've seen a June cleaver adventure
	    page = run_choice( -1 );
	    // Fetch the next encounter
	    page = visit_url( url );
	    break;
	}
    }

    if ( page.contains_text( "Plant a Tree, Plant a Tree!" ) ) {
	// We've been through the choice adventures and have an
	// appropriate sack of saplings, but are not carrying it.
	//
	// Perhaps we are all done for the day.
	plant_a_tree();
    }

    // Adventure 2 times to get a potion or 100 times to get an outfit piece
    int available = my_adventures();
    if ( available < turns ) {
	abort( "You need " + turns + " but only have " + available + " left." );
    }

    try {
	cli_execute( "checkpoint" );

	// Since the sack of saplings is an off-hand item, we must
	// unequip a 2-handed weapon before holding it.
	if ( equipped_item( WEAPON ).weapon_hands() > 1 ) {
	    equip( WEAPON, NO_ITEM );
	}

	equip( sack );

	adventure( turns, ARRRBORETUM );

	// Remember that we've completed this today
	set_property( COMPLETION_PROPERTY, "true" );
    } finally {
	cli_execute( "outfit checkpoint" );
    }
}