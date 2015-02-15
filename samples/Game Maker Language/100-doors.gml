var doors,a,i;
//Sets up the array for all of the doors.
for (i = 1; i<=100; i += 1)
    {
    doors[i]=0;
    }

//This first for loop goes through and passes the interval down to the next for loop.
for (i = 1; i <= 100; i += 1;)
    {
    //This for loop opens or closes the doors and uses the interval(if interval is 2 it only uses every other etc..)
    for (a = 0; a <= 100; a += i;)
        {
        //Opens or closes a door.
        doors[a] = !doors[a];
        }
    }
open_doors = '';

//This for loop goes through the array and checks for open doors.
//If the door is open it adds it to the string then displays the string.
for (i = 1; i <= 100; i += 1;)
    {
    if (doors[i] == 1)
        {
        open_doors += "Door Number "+string(i)+" is open#";
        }
    }
show_message(open_doors);
game_end();
