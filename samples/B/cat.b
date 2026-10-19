EOF 4294967295 /* -1 */;

from_stdin(){
	extrn getchar, putchar;
	auto c;
	while((c = getchar()) != EOF){
		putchar(c);
	}
	return(0);
}

from_args(argc, argv){
	extrn perror, fopen, fclose, putchar, fgetc;
	auto i, fp, c;
	i = 1;
	while(i < argc){
		fp = fopen(argv[i], "r");
		if(!fp){
			perror("fopen");
			return(1);
		}

		while((c = fgetc(fp)) != EOF){
			putchar(c);
		}

		fclose(fp);

		i++;
	}
	return(0);
}

main(argc, argv){
	if(argc <= 1){
		return(from_stdin());
	} else {
		return(from_args(argc, argv));
	}
}
