/**
 * SOURCE: https://github.com/ADawesomeguy/tw3-modAchievementStatTrak
 * LICENSE: https://github.com/ADawesomeguy/tw3-modAchievementStatTrak/blob/master/LICENSE
 */

// TODO: Multiple functions: one returns array of stuff, another formats, and one final exec function

// Function to get the stats in a formatted string
function getAchievementStats() : array<EStatistic>
{
    // Integer to be later used in the for loop
    var i : int;

    // Array to store the achievement stats
    var stats : array<EStatistic>;

    // String where all the achievements go
    var achievementString : string;
    
    // Set achievement string empty to be later added on to
    achievementString = "";

    // Add all stats to the array
    stats.PushBack(ES_CharmedNPCKills);
    stats.PushBack(ES_AardFallKills);
    stats.PushBack(ES_EnvironmentKills);
    stats.PushBack(ES_CounterattackChain);
    stats.PushBack(ES_DragonsDreamTriggers);
    stats.PushBack(ES_KnownPotionRecipes);
    stats.PushBack(ES_KnownBombRecipes);
    stats.PushBack(ES_ReadBooks);
    stats.PushBack(ES_HeadShotKills);
    stats.PushBack(ES_BleedingBurnedPoisoned);
    stats.PushBack(ES_DestroyedNests);
    stats.PushBack(ES_FundamentalsFirstKills);
    stats.PushBack(ES_FinesseKills);
    stats.PushBack(ES_SelfArrowKills);
    stats.PushBack(ES_ActivePotions);
    stats.PushBack(ES_KilledCows);
    stats.PushBack(ES_SlideTime);

    // Return the array of stats
    return stats;
}

function getAchievementStatVal(statEnum : EStatistic) : int
{
    // Get value from enum
    return theGame.GetGamerProfile().GetStatValue(statEnum);
}

function getAchievementStatNameRaw(statEnum : EStatistic) : string
{
    // Get raw name from enum
    return StatisticEnumToName(statEnum);
}

function getAchievmentStatName(rawStatName : string) : string
{
    var formattedStatName : string;

    // Set Steam/GOG achievement name from name
    switch (rawStatName)
    {
        case "statistic_charmed_kills":
            formattedStatName = "The Enemy of My Enemy";
            break;
        
        case "statistic_aardfall_kills":
            formattedStatName = "Humpty Dumpty";
            break;
        
        case "statistic_environment_kills":
            formattedStatName = "Environmentally Unfriendly";
            break;

        case "statistic_bleed_burn_poison":
            formattedStatName = "Overkill";
            break;
        
        case "statistic_counterattack_chain":
            formattedStatName = "Kaer Morhen Trained";
            break;

        case "statistic_burning_gas_triggers":
            formattedStatName = "That Is the Evilest Thing";
            break;
        
        case "statistic_known_potions":
            formattedStatName = "Let's Cook!";
            break;

        case "statistic_known_bombs":
            formattedStatName = "Bombardier";
            break;

        case "statistic_head_shot_kills":
            formattedStatName = "Master Marksman";
            break;

        case "statistic_read_books":
            formattedStatName = "Bookworm";
            break;

        case "statistic_destroyed_nests":
            formattedStatName = "Fire in the Hole";
            break;
        
        case "statistic_fundamentals_kills":
            formattedStatName = ""; // Don't know what achievement this corresponds to
            break;

        case "statistic_finesse_kills":
            formattedStatName = ""; // Don't know what achievement this corresponds to
            break;

        case "statistic_self_arrow_kills":
            formattedStatName = "Return to Sender";
            break;

        case "statistic_active_potions":
            formattedStatName = "Can Quit Anytime I Want";
            break;

        case "statistic_killed_cows":
            formattedStatName = "Moo-rderer";
            break;

        case "statistic_slide_time":
            formattedStatName = "Rad Steez, Bro!";
            break;
        
        default:
            formattedStatName = "";
            break;
    }

    // Return Steam/GOG achievement name
    return formattedStatName;
}

function getFormattedAchievementStats(statList : array<EStatistic>) : string
{
    // Vars for formatted string
    var achievementString : string;
    var currentRawName : string;
    var currentName : string;
    var currentVal : int;
    var i : int;
    achievementString = "";

    // Loop through each stat to create a final string that goes in the Gwent book
    for (i = 0; i < statList.Size(); i += 1)
    {
        // Use other functions to get data
        currentRawName = getAchievementStatNameRaw(statList[i]);
        currentName = getAchievmentStatName(currentRawName);
        currentVal = getAchievementStatVal(statList[i]);

        // If allowing spoilers was not toggled on
        if (!theGame.GetInGameConfigWrapper().GetVarValue('ModStatTrak', 'AllowSpoilers'))
        {
            if (currentName == "Rad Steez, Bro!" || currentName == "Moo-rderer")
            {
                // Don't print and continue the loop
                continue;
            }
        }

        if (i != 0)
        {
            achievementString += "<br><br>" + currentRawName + ": " + "<font color='#00ff00'>" + currentVal + "</font>" + "<br>" + "Achievement: " + "<font color='#0088ff'>" + currentName + "</font>";
        }
        else
        {
            // Don't add beginning newline for first element
            achievementString += currentRawName + ": " + "<font color='#00ff00'>" + currentVal + "</font>" + "<br>" + "Achievement: " + "<font color='#0088ff'>" + currentName + "</font>";
        }
    }

    // Return final formatted string
    return achievementString;
}

//------------------------
// EXEC FUNCTIONS
//------------------------
exec function getAllAchievementStats()
{
    // Define vars for adding notification for all achievement stats
    var statList : array<EStatistic>;
    var formattedStats : string;
    
    // Get data
    statList = getAchievementStats();
    formattedStats = getFormattedAchievementStats(statList);

    // Add notification
    theGame.GetGuiManager().ShowNotification(formattedStats);
}

// TODO: Fix getting a single achievement stat
/*exec function getAchievementStat(statEnum : EStatistic)
{
    // Define vars for adding notification for specific stat
    var rawName : string;
    var achName : string;
    var val : int;

    // Get data
    rawName = getAchievementStatNameRaw(statEnum);
    achName = getAchievementStatVal(statEnum);
    val = getAchievementStatVal(statEnum);

    // Print to notification
    theGame.GetGuiManager().ShowNotification(rawName + ": " + "<font color='#00ff00'>" + val + "</font>" + "<br>" + "Achievement: " + "<font color='#0088ff'>" + val);
}*/
