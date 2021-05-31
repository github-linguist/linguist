since r17612;
/*
	Sweet Synthesis.ash
	Simplifies casting the sweet synthesis skill, which takes two candies and one spleen and gives a thirty-turn buff.
	
	Usage:
	synthesis [effect_wanted] - item, meat, etc.
	
	Written by Ezandora. This script is in the public domain.
*/
string __synthesis_version = "1.0.5";

//Expensive items that are never allowed for use, as a safeguard:
//Well, I'm sure there's that totally elite in-run strategy where you use two UMSBs for +50% moxie gain, but aside from that...
boolean [item] __blacklist = $items[ultra mega sour ball,radio button candy,powdered candy sushi set,gummi ammonite,gummi trilobite,fudge bunny,chocolate cigar,vitachoconutriment capsule];


static
{
	//boolean [item] __simple_candy = $items[1702, 1962, 4341, 913, 1652, 2942, 3455, 3449, 3454, 3453, 3452, 3451, 4152, 1501, 5455, 5478, 5476, 5477, 1344, 5188, 4340, 1161, 912, 4342, 5454, 2941, 1346, 4192, 1494, 5456, 617, 3496, 2734, 933, 908, 3450, 1783, 2088, 2576, 907, 1767, 906, 911, 540, 263, 909, 905, 5180, 2309, 300, 2307, 298, 1163, 2306, 299, 2305, 297, 2304, 2308, 5892, 6792, 5435, 7677, 7785];
	//boolean [item] __complex_candy = $items[5495, 5496, 5494, 5458, 5421, 4851, 2197, 1382, 4334, 4333, 5424, 3269, 5422, 921, 5425, 5423, 3091, 2955, 5416, 5419, 5418, 5417, 5420, 5381, 5319, 5400, 4330, 4332, 5406, 5405, 4818, 5402, 5318, 5384, 4331, 5320, 5382, 5398, 5401, 5397, 5317, 5385, 5321, 5383, 3290, 3760, 2193, 5413, 5459, 5483, 3584, 5395, 5396, 5482, 4256, 5484, 2943, 4329, 3054, 4758, 4163, 4466, 4464, 4465, 4462, 4467, 4463, 4623, 5157, 4395, 4394, 4393, 4518, 5189, 4151, 5023, 3428, 3423, 3424, 5457, 3425, 5480, 5474, 5479, 5473, 5481, 5475, 4164, 3631, 4853, 5414, 5415, 3046, 5345, 1345, 5103, 2220, 4746, 4389, 3125, 4744, 4273, 3422, 1999, 3426, 4181, 4180, 4176, 4183, 4179, 4191, 4182, 4178, 4745, 5526, 6835, 6852, 6833, 6834, 6836, 7499, 7915, 8257, 7914, 6837, 8151, 3124, 8149, 8154, 7919, 6840, 5736, 6831, 7917, 8150, 6404, 6841, 6904, 6903, 7918, 7710, 6399, 9146, 8537, 6405, 6843, 7474, 6172, 9252, 5913];
	boolean [item] __simple_candy;
	boolean [item] __complex_candy;
	void initialiseCandy()
	{
		foreach it in $items[]
		{
			if (!it.candy) continue;
			if (it.candy_type == "simple")
				__simple_candy[it] = true;
			else if (it.candy_type == "complex")
				__complex_candy[it] = true;
		}
	}
	initialiseCandy();
}

//Utility:
//Use prefix to allow including in another script. Bad solution, but...
void slistAppend(item [int] list, item entry)
{
	int position = list.count();
	while (list contains position)
		position += 1;
	list[position] = entry;
}

void slistAppend(item [int][int] list, item [int] entry)
{
	int position = list.count();
	while (list contains position)
		position += 1;
	list[position] = entry;
}

item [int] slistMake(item e1, item e2)
{
	item [int] result;
	result.slistAppend(e1);
	result.slistAppend(e2);
	return result;
}

boolean smafiaIsPastRevision(int revision_number)
{
    if (get_revision() <= 0) //get_revision reports zero in certain cases; assume they're on a recent version
        return true;
    return (get_revision() >= revision_number);
}

//Statics:

static
{
    string [effect] __buff_descriptions;
    __buff_descriptions[$effect[Synthesis: Hot]] = "+9 hot res";
    __buff_descriptions[$effect[Synthesis: Cold]] = "+9 cold res";
    __buff_descriptions[$effect[Synthesis: Pungent]] = "+9 stench res";
    __buff_descriptions[$effect[Synthesis: Scary]] = "+9 spooky res";
    __buff_descriptions[$effect[Synthesis: Greasy]] = "+9 sleaze res";
    
    __buff_descriptions[$effect[Synthesis: Strong]] = "+300% muscle";
    __buff_descriptions[$effect[Synthesis: Smart]] = "+300% myst";
    __buff_descriptions[$effect[Synthesis: Cool]] = "+300% moxie";
    __buff_descriptions[$effect[Synthesis: Hardy]] = "+300% max HP";
    __buff_descriptions[$effect[Synthesis: Energy]] = "+300% max MP";
    
    __buff_descriptions[$effect[Synthesis: Greed]] = "+300% meat";
    __buff_descriptions[$effect[Synthesis: Collection]] = "+150% item";
    __buff_descriptions[$effect[Synthesis: Movement]] = "+50% muscle gain";
    __buff_descriptions[$effect[Synthesis: Learning]] = "+50% myst gain";
    __buff_descriptions[$effect[Synthesis: Style]] = "+50% moxie gain";
    
	int [effect] __buff_tiers;
    __buff_tiers[$effect[Synthesis: Hot]] = 1;
    __buff_tiers[$effect[Synthesis: Cold]] = 1;
    __buff_tiers[$effect[Synthesis: Pungent]] = 1;
    __buff_tiers[$effect[Synthesis: Scary]] = 1;
    __buff_tiers[$effect[Synthesis: Greasy]] = 1;
    
    __buff_tiers[$effect[Synthesis: Strong]] = 2;
    __buff_tiers[$effect[Synthesis: Smart]] = 2;
    __buff_tiers[$effect[Synthesis: Cool]] = 2;
    __buff_tiers[$effect[Synthesis: Hardy]] = 2;
    __buff_tiers[$effect[Synthesis: Energy]] = 2;
    
    __buff_tiers[$effect[Synthesis: Greed]] = 3;
    __buff_tiers[$effect[Synthesis: Collection]] = 3;
    __buff_tiers[$effect[Synthesis: Movement]] = 3;
    __buff_tiers[$effect[Synthesis: Learning]] = 3;
    __buff_tiers[$effect[Synthesis: Style]] = 3;
    
    int [effect] __buff_subid;
    __buff_subid[$effect[Synthesis: Hot]] = 1;
    __buff_subid[$effect[Synthesis: Cold]] = 2;
    __buff_subid[$effect[Synthesis: Pungent]] = 3;
    __buff_subid[$effect[Synthesis: Scary]] = 4;
    __buff_subid[$effect[Synthesis: Greasy]] = 5;
    
    __buff_subid[$effect[Synthesis: Strong]] = 1;
    __buff_subid[$effect[Synthesis: Smart]] = 2;
    __buff_subid[$effect[Synthesis: Cool]] = 3;
    __buff_subid[$effect[Synthesis: Hardy]] = 4;
    __buff_subid[$effect[Synthesis: Energy]] = 5;
    
    __buff_subid[$effect[Synthesis: Greed]] = 1;
    __buff_subid[$effect[Synthesis: Collection]] = 2;
    __buff_subid[$effect[Synthesis: Movement]] = 3;
    __buff_subid[$effect[Synthesis: Learning]] = 4;
    __buff_subid[$effect[Synthesis: Style]] = 5;
}


item [int][int] calculateSweetSynthesisCandyCombinations(int tier, int subid)
{
    item [int][int] result;
    boolean [item] candy_1;
    boolean [item] candy_2;
    
    if (tier == 1)
    {
    	candy_1 = __simple_candy;
    	candy_2 = __simple_candy;
    }
    else if (tier == 2)
    {
    	candy_1 = __simple_candy;
    	candy_2 = __complex_candy;
    }
    else if (tier == 3)
    {
    	candy_1 = __complex_candy;
    	candy_2 = __complex_candy;
    }
    foreach item_1 in candy_1
    {
        int item_1_id = item_1.to_int();
        if (__blacklist contains item_1)
        	continue;
        
        foreach item_2 in candy_2
        {
        	if (__blacklist contains item_2)
        		continue;
            int item_2_id = item_2.to_int();
            if ((item_1_id + item_2_id) % 5 != (subid - 1))
                continue;
            result.slistAppend(slistMake(item_1, item_2));
        }
    }
    return result;
}

float [item] __mall_price_speculation_percent_increase;
int mall_price_speculation(item it)
{
	int price = it.mall_price();
	if (__mall_price_speculation_percent_increase contains it)
		price *= (1.0 + __mall_price_speculation_percent_increase[it]);
	return price;
}

int historical_price_speculation(item it)
{
	int price = it.historical_price();
	if (__mall_price_speculation_percent_increase contains it)
		price *= (1.0 + __mall_price_speculation_percent_increase[it]);
	return price;
}

int synthesis_price(item it)
{
    if (!it.tradeable)
        return 999999999;
    int price = it.historical_price_speculation();
    if ((it.historical_age() > 7.0 && price < 50000) || it.historical_age() >= 60.0)
    {
    	//Initiate re-search of everything if their knowledge is too old. once a week, I guess? that seems like a good idea?
    	//I think mafia has some built-in database of this. 
    	return it.mall_price_speculation();
    }
    if (price <= 0)
        return 999999999;
    return price;
}

item [int] pickBestCandyCombinationFromCombinations(item [int][int] combinations)
{
	if (!can_interact())
	{
		//Ronin:
		item [int][int] combinations_2;
		foreach key in combinations
		{
			item item_1 = combinations[key][0];
			item item_2 = combinations[key][1];
			if (item_1.item_amount() == 0 || item_2.item_amount() == 0)
				continue;
			combinations_2.slistAppend(slistMake(item_1, item_2));
		}
		sort combinations_2 by value[0].synthesis_price() + value[1].synthesis_price();
		item [int][int] final_combinations;
		foreach key in combinations_2
		{
			if (key > 4)
				break;
			final_combinations.slistAppend(combinations_2[key]);
		}
		sort final_combinations by value[0].mall_price_speculation() + value[1].mall_price_speculation();
		return final_combinations[0];
	}
	else
	{
		//Mall access:
		sort combinations by (value[0].synthesis_price() + value[1].synthesis_price());
		//Take cheapest ones, sort by mall price:
		item [int][int] final_combinations;
		foreach key in combinations
		{
			if (key > 40)
				break;
			final_combinations.slistAppend(combinations[key]);
		}
		sort final_combinations by value[0].mall_price_speculation() + value[1].mall_price_speculation();
		return final_combinations[0];
	}
}

//Returns best choice for two candies.
item [int] pickBestCandyCombinationForEffect(effect requested_effect)
{
	item [int][int] combinations = calculateSweetSynthesisCandyCombinations(__buff_tiers[requested_effect], __buff_subid[requested_effect]);
	//Pick cheapest combination obtainable:
	
	return pickBestCandyCombinationFromCombinations(combinations);
}

//Useful in an external script; my farming script will run this to calculate whether to run +item or +meat.
//3 * meat_drop - costToGainEffect($effect[Synthesis: Greed]) / 30.0
//vs
//1.5 * item_drop * item_price - costToGainEffect($effect[Synthesis: Collection]) / 30.0
int costToGainEffect(effect requested_effect)
{
	if (!can_interact())
		return -1; //-1 or 999999999?
	item [int] picks = pickBestCandyCombinationForEffect(requested_effect);
	int price = 0;
	foreach key, it in picks
		price += it.mall_price_speculation(); //don't think candy is NPCable
	return price;
}

//Abstracted for external scripts.
void synthesiseCandy(effect requested_effect, boolean show_output)
{
	//Locked into an effect, let's try it:
	if (show_output)
		print("Requested effect: " + requested_effect + " (" + __buff_descriptions[requested_effect] + ")");
	int breakout3 = 100;
	item final_candy_1 = $item[none];
	item final_candy_2 = $item[none];
	while (breakout3 > 0)
	{
		//Candy picking loop:
		breakout3 -= 1;
		item [int] candy_picks = pickBestCandyCombinationForEffect(requested_effect);
	
		if (candy_picks.count() < 2)
		{
			if (show_output)
				print("Umm... no candy? Nooooo....", "red");
			return;
		}
	
		final_candy_1 = candy_picks[0];
		final_candy_2 = candy_picks[1];
	
		if (!can_interact())
		{
			boolean yes = user_confirm("Allow consuming " + final_candy_1 + " + " + final_candy_2 + "?");
			if (!yes)
			{
				if (show_output)
					print("Canceled.");
				return;
			}
		}
	
		if (final_candy_1 == $item[none] || final_candy_2 == $item[none])
		{
			if (show_output)
				print("Internal error, sorry.", "red");
			return;
		}
		int mall_price_limit = 100000;
		if (can_interact())
			mall_price_limit = 20000;
		if (final_candy_1.mall_price_speculation() >= mall_price_limit || final_candy_2.mall_price_speculation() >= mall_price_limit) //failsafe
		{
			if (show_output)
				print("This candy is too expensive, bailing out.", "red");
			return;
		}
		
		int [item] required_items;
		required_items[final_candy_1] += 1;
		required_items[final_candy_2] += 1;
		boolean need_to_recalculate_candy = false;
		foreach it, amount in required_items
		{
			if (need_to_recalculate_candy)
				break;
			if (it.item_amount() < amount)
			{
				if (can_interact())
				{
					//Can't use retrieve_item(), because that won't let us set a price limit.
					//So, use buy():
					if (it.item_amount() < amount && it.available_amount() > it.item_amount())
					{
						//retrieve what we already have:
						//(this generally means pulling)
						retrieve_item(MIN(MIN(2, amount), it.available_amount()), it);
					}
					int breakout = 2;
					while (breakout > 0 && it.item_amount() < amount)
					{
						breakout -= 1;
						int amount_bought = buy(1, it, MIN(it.mall_price_speculation(), MIN(20000, mall_price_limit))); //the single most dangerous command mafia can ever use
						if (amount_bought < 1)
						{
							if (!smafiaIsPastRevision(17704)) //old versions of mafia won't track mall_price() against disabled/ignored stores
								__mall_price_speculation_percent_increase[it] += 1.0;
							else //new versions will
								__mall_price_speculation_percent_increase[it] += 0.05; //very, very small amount, just in case something weird happens
							need_to_recalculate_candy = true;
							break;
						}
					}
				}
				else
				{
					if (show_output)
						print("Can't acquire this candy.", "red");
					return;
				}
			}
		}
		if (need_to_recalculate_candy)
			continue;
		else
			break;
	}
	
	if (show_output)
		print("Chosen candy: " + final_candy_1 + " + " + final_candy_2);
	if (final_candy_1.item_amount() == 0 || final_candy_2.item_amount() == 0)
	{
		if (show_output)
			print("Can't acquire the candy for some reason.", "red");
		return;
	}
	if (__blacklist contains final_candy_1 || __blacklist contains final_candy_2) //shouldn't show up
	{
		if (show_output)
			print("Blacklisted candy! No can do.", "red");
		return;
	}
	
	visit_url("runskillz.php?action=Skillz&whichskill=166&targetplayer=" + my_id() + "&quantity=1");
	visit_url("choice.php?a=" + final_candy_1.to_int() + "&b=" + final_candy_2.to_int() + "&whichchoice=1217&option=1");
	if (!smafiaIsPastRevision(17700)) //no idea when this was changed; approximating
		cli_execute("refresh inventory");
}

void main(string arguments)
{
	print("Sweet Synthesis.ash version " + __synthesis_version);
	
	if (!$skill[sweet synthesis].have_skill())
	{
		print("You don't seem to have the Sweet Synthesis skill.", "red");
		return;
	}
	if (spleen_limit() - my_spleen_use() <= 0)
	{
		print("You need free spleen space to use this.", "red");
		return;
	}
	
	//Parse arguments:
	effect requested_effect;
	arguments = arguments.to_lower_case();
	if (arguments.contains_text("meat") || arguments.contains_text("greed"))
		requested_effect = $effect[Synthesis: Greed]; //+300% meat
	else if (arguments.contains_text("item") || arguments.contains_text("collection"))
		requested_effect = $effect[Synthesis: Collection]; //+150% item
	else if (arguments.contains_text("muscle gain") || arguments.contains_text("muscle exp") || arguments.contains_text("movement"))
		requested_effect = $effect[Synthesis: Movement]; //+50% muscle gain
	else if (arguments.contains_text("myst gain") || arguments.contains_text("myst exp") || arguments.contains_text("mysticality gain") || arguments.contains_text("mysticality exp") || arguments.contains_text("learning"))
		requested_effect = $effect[Synthesis: Learning]; //+50% myst gain
	else if (arguments.contains_text("moxie gain") || arguments.contains_text("moxie exp") || arguments.contains_text("style"))
		requested_effect = $effect[Synthesis: Style]; //+50% moxie gain
	else if (arguments.contains_text("muscle") || arguments.contains_text("strong"))
		requested_effect = $effect[Synthesis: Strong]; //+300% muscle
	else if (arguments.contains_text("myst") || arguments.contains_text("smart"))
		requested_effect = $effect[Synthesis: Smart]; //+300% myst
	else if (arguments.contains_text("moxie") || arguments.contains_text("cool"))
		requested_effect = $effect[Synthesis: Cool]; //+300% moxie
	else if (arguments.contains_text("hp") || arguments.contains_text("hardy"))
		requested_effect = $effect[Synthesis: Hardy]; //+300% max HP
	else if (arguments.contains_text("mp") || arguments.contains_text("energy"))
		requested_effect = $effect[Synthesis: Energy]; //+300% max MP
	else if (arguments.contains_text("hot") || arguments.contains_text("hot"))
		requested_effect = $effect[Synthesis: Hot]; //+9 hot res
	else if (arguments.contains_text("cold") || arguments.contains_text("cold"))
		requested_effect = $effect[Synthesis: Cold]; //+9 cold res
	else if (arguments.contains_text("stench") || arguments.contains_text("pungent"))
		requested_effect = $effect[Synthesis: Pungent]; //+9 stench res
	else if (arguments.contains_text("spooky") || arguments.contains_text("scary"))
		requested_effect = $effect[Synthesis: Scary]; //+9 spooky res
	else if (arguments.contains_text("sleaze") || arguments.contains_text("greasy"))
		requested_effect = $effect[Synthesis: Greasy]; //+9 sleaze res
	int desired_casts = 1;
	string [int] arguments_list = arguments.split_string(" ");
	foreach key, s in arguments_list
	{
		if (s == "") continue;
		if (s.is_integer())
		{
			int value = s.to_int();
			if (value == 0)
			{
				print_html("Successfully casted the skill zero times.");
				return;
			}
			else if (value < 0)
			{
				print_html("I can't uncast the skill.");
				return;
			}
			desired_casts = MAX(1, value);
		}
		if (s == "*")
		{
			int target_turns = my_adventures();
			int turns_have = requested_effect.have_effect();
			int turn_delta = target_turns - turns_have;
			int casts_needed = ceil(to_float(turn_delta) / 30.0);
			desired_casts = MAX(0, casts_needed);
		}
	}
		
	if (arguments == "" || requested_effect == $effect[none])
	{
		//Print help:
		print_html(" ");
		print_html("Sweet Synthesis.ash will give you thirty turns of a buff using candy, costing a single spleen.");
		if (can_interact())
			print_html("Outside of ronin, it will automatically acquire the cheapest candy from the mall.");
		else
			print_html("In ronin, it will use whatever candy you have in inventory, and request to confirm it.");
		print_html(" ");
		print_html("Usage: synthesis requested_effect");
		print_html("Use one of the following bolded names:");
		print_html("<b>meat</b>: +300% meat");
		print_html("<b>item</b>: +150% item");
		print_html("<b>muscle exp</b>: +50% muscle gain");
		print_html("<b>myst exp</b>: +50% mysticality gain");
		print_html("<b>moxie exp</b>: +50% moxie gain");
		print_html("<b>muscle</b>: +300% muscle");
		print_html("<b>myst</b>: +300% mysticality");
		print_html("<b>moxie</b>: +300% moxie");
		print_html("<b>HP</b>: +300% maximum HP");
		print_html("<b>MP</b>: +300% maximum MP");
		print_html("<b>hot</b>: +9 hot res");
		print_html("<b>cold</b>: +9 cold res");
		print_html("<b>stench</b>: +9 stench res");
		print_html("<b>spooky</b>: +9 spooky res");
		print_html("<b>sleaze</b>: +9 sleaze res");
		print_html("");
		print_html("\"synthesis 10 meat\" will cast ten times, for 300 turns of a +meat buff.");
		print_html("\"synthesis * meat\" will try to extend the meat effect to the number of adventures you have left. In other words, you can cast it before starting your farming.");
		return;
	}
	if (desired_casts <= 0)
		return;
	if (desired_casts > 1)
		print_html("Casting " + desired_casts + " times.");
	for i from 1 to desired_casts
	{
		if (spleen_limit() - my_spleen_use() <= 0)
			break;
		synthesiseCandy(requested_effect, true);
	}
}