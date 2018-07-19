using Gee;

void main(string[] args){
    string filename = args[1];
    var file = FileStream.open(filename, "r");

    var	counter	= new HashMap<char, int>();

    string line = file.read_line();
    while (line != null){
        for (int x = 0;	x < line.length; x++){
            counter[line[x]] = counter[line[x]] + 1;
	}
        line = file.read_line();
    }

    foreach (var elem in counter.entries){
	stdout.printf("%c occured %d times\n", elem.key, elem.value);
    }
}
