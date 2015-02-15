#include <stdio.h> //printf()
#include <stdlib.h> //srand(), rand(), RAND_MAX, qsort()
#include <stdbool.h> //true, false
#include <time.h> //time()

#define NUMBALLS 5 //NUMBALLS>1

int compar(const void *a, const void *b){
	char c1=*(const char*)a, c2=*(const char*)b; //first cast void* to char*, then dereference
	return c1-c2;
}

_Bool issorted(char *balls){
	int i,state;
	state=0;
	for(i=0;i<NUMBALLS;i++){
		if(balls[i]<state)return false;
		if(balls[i]>state)state=balls[i];
	}
	return true;
}

void printout(char *balls){
	int i;
	char str[NUMBALLS+1];
	for(i=0;i<NUMBALLS;i++)str[i]=balls[i]==0?'r':balls[i]==1?'w':'b';
	printf("%s\n",str);
}

int main(void) {
	char balls[NUMBALLS]; //0=r, 1=w, 2=b
	int i;
	srand(time(NULL)); //not a good seed but good enough for the example
	rand(); //rand() always starts with the same values for certain seeds, making
	        //  testing pretty irritating
	// Generate balls
	for(i=0;i<NUMBALLS;i++)balls[i]=(double)rand()/RAND_MAX*3;
	while(issorted(balls)){ //enforce that we start with non-sorted balls
		printf("Accidentally still sorted: ");
		printout(balls);
		for(i=0;i<NUMBALLS;i++)balls[i]=(double)rand()/RAND_MAX*3;
	}
	printf("Non-sorted: ");
	printout(balls);
	qsort(balls,NUMBALLS,sizeof(char),compar); //sort them using quicksort (stdlib)
	if(issorted(balls)){ //unnecessary check but task enforces it
		printf("Sorted: ");
		printout(balls);
	} else {
		printf("Sort failed: ");
		printout(balls);
	}
	return 0;
}
