#loader crafttweaker
#priority 100

import crafttweaker.event.CommandEvent;
import crafttweaker.player.IPlayer;

/* 
 * We want to register a command handler that prints a message to the player's chat if they execue the "/ct syntax" command.
 * This handler then also gives them Regeneration III for 21 seconds.
 * Command handlers are functions with a specific event type that needs to be provided.
 * Here we use the commandEvent, hence the "as CommandEvent".
 * To actually subscribe to the commandEvents we use "events.onCommand" and supply the command handler.
 * Important, the CommandEvent type needs to be imported, otherwise we'd have to use "as crafttweaker.event.CommandEvent"
 */
events.onCommand(function(event as CommandEvent) {
    val command = event.command;

    if(isNull(command) || (command.name != "crafttweaker") || (event.parameters.length == 0) || (event.parameters[0] != "syntax")) {
        return;
    }

    //Checks if the one sending the command is a player
    if(event.commandSender instanceof IPlayer) {
        val player as IPlayer = event.commandSender;
        
        player.sendChat("You executed /ct syntax");

        #Applies a regeneration III effect for 22 seconds (21 * 20 ticks) to the player.
        #You could also directly write 420, but this way may be clearer for novices.
        player.addPotionEffect(<potion:minecraft:regeneration>.makePotionEffect(21 * 20, 3));
    }
});


recipes.addShapedRecipe("RandomName", <minecraft:diamond>, [[<minecraft:wool:*>], [<minecraft:diamond_sword>.anyDamage()], [<ore:ingotIron>]]);

function variance(inputNumbers as double[]) as void {
    if(inputNumbers.length == 0) {
        return;
    }


    var sum = 0.0D;

    for number in inputNumbers {
        sum += number;
    }

    val average = sum / inputNumbers.length;



    var varianceSum = 0.0D;

    for number in inputNumbers {
        varianceSum += (number - average) * (number - average);
    }

    val variance = varianceSum / (inputNumbers.length - 1);

    print("Average: " ~ average);
    print("Variance: " ~ variance);
}

print("Testing Variance, expecting Avg 2.0 and Variance 1.0");
variance([1.0, 2.0, 3.0]); 

function fibonacci(amount as int) as long[] {
    var fibonacci = [1, 1] as long[];

    for n in 2 .. amount {
        fibonacci += fibonacci[n - 1] + fibonacci[n - 2];
    }

    return fibonacci;
}

print("Testing Fibonacci");
for fib in fibonacci(10) { //1,1,2,3,5,8,13,21,34,55
    print(fib);
}

function bubblesort(list as int[]) as int[] {
    var manipulated = true;

    while(manipulated) {
    	manipulated = false;
        for i in 1 .. list.length {
            if(list[i - 1] > list[i]) {
                manipulated = true;
                //Swap
                val temp = list[i - 1];
                list[i - 1] = list[i];
                list[i] = temp;
            }
        }
    }

    return list;
}


print("Checking bubblesort");

for i in bubblesort([5, 3, 1, 2]) {
	print(i);
}


static plankLogPairs as IIngredient[][IItemStack] = {
	<abyssalcraft:dltplank:0>: [
		<abyssalcraft:dltlog:0>
	],
	<abyssalcraft:dreadplanks:0>: [
		<abyssalcraft:dreadlog:0>
	],
	<betterwithaddons:planks_mulberry:0>: [
		<betterwithaddons:log_mulberry:0>
	],
	<betterwithaddons:planks_sakura:0>: [
		<betterwithaddons:log_sakura:0>
	],
	<minecraft:planks:1>: [
		<thebetweenlands:log_sap:0>,
		<minecraft:log:1>,
		<primal:logs_stripped:1>,
		<twilightforest:magic_log:0>,
		<twilightforest:twilight_log:1>,
		<twilightforest:twilight_log:3>
	]
};

/*
	This array listing should only contain logs which don't have planks from the mod or don't make sense to convert to Vanilla Planks.

	This will then remove the log from processig recipes (in higher tech) to not turn out as chopping blocks.
*/
static logsToRemove as IItemStack[] = [
	<natura:redwood_logs:2>,
	<natura:redwood_logs:0>,
	<thebetweenlands:log_nibbletwig:0>,
	<twilightforest:magic_log:1>
];
