//Evidence of the Monty Hall solution.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define GAMES 3000000

int main(void){
    unsigned i, j, k, choice, winsbyswitch=0, door[3];

    srand(time(NULL));                                                          //initialize random seed.
    for(i=0; i<GAMES; i++){
        door[0] = (!(rand()%2)) ? 1: 0;                                         //give door 1 either a car or a goat randomly.
        if(door[0]) door[1]=door[2]=0;                                          //if 1st door has car, give other doors goats.
        else{ door[1] = (!(rand()%2)) ? 1: 0; door[2] = (!door[1]) ? 1: 0; }    //else, give 2nd door car or goat, give 3rd door what's left.
        choice = rand()%3;                                                      //choose a random door.

        //if the next door has a goat, and the following door has a car, or vice versa, you'd win if you switch.
        if(((!(door[((choice+1)%3)])) && (door[((choice+2)%3)])) || (!(door[((choice+2)%3)]) && (door[((choice+1)%3)]))) winsbyswitch++;
    }
    printf("\nAfter %u games, I won %u by switching.  That is %f%%. ", GAMES, winsbyswitch, (float)winsbyswitch*100.0/(float)i);
}
