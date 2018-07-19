public static void main(){
	var file = FileStream.open("foo.txt", "r");
	
	string line = file.read_line();
	while (line != null){
		stdout.printf("%s\n", line);
		line = file.read_line();
	}
}
