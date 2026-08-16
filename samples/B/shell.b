cmd[512];

lchar(string, i, char){
	*(string + i) = char & 255;
}

starts_with(s1, s2){
	extrn memcmp, strlen;
	if(strlen(s1) < strlen(s2)) return(0);
	return(!memcmp(s1, s2, strlen(s2)));
}

main(){
	extrn stdin, fflush, fgets, strlen, strcmp, memset, printf, chdir, system;

	while(1){
		printf("$ ");
		fflush(stdin);

		memset(cmd, 0, 512);
		if(!fgets(cmd, 512, stdin)) goto quit;
		lchar(cmd, strlen(cmd) - 1, 0); /* remove newline */

		if(strcmp(cmd, "exit") == 0){
			goto quit;
		} else if(starts_with(cmd, "cd ")){
			chdir(cmd + 3);
		} else system(cmd);
	}
	quit:
}
