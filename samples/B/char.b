char(string, i){
	return(*(string + i) & 255);
}

lchar(string, i, char){
	*(string + i) = char & 255;
}
