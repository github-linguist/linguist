using Gee;

/* function to sum the square of the digits */
int sum(int input){
	// convert input int to string
	string input_str = input.to_string();
	int total = 0;
	// read through each character in string, square them and add to total
	for (int x = 0; x < input_str.length; x++){
		// char.digit_value converts char to the decimal value the char it represents holds
		int digit = input_str[x].digit_value();
		total += (digit * digit);
	}

	return total;
} // end sum

/* function to decide if a number is a happy number */
bool is_happy(int total){
	var past = new HashSet<int>();
	while(true){
		total = sum(total);
		if (total == 1){
			return true;}
		
		if (total in past){
			return false;}
		
		past.add(total);
	} // end while loop
} // end happy

public static void main(){
	var happynums = new ArrayList<int>();
	int x = 1;
	
	while (happynums.size < 8){
		if (is_happy(x) == true)
			happynums.add(x);
		x++;
	}
	
	foreach(int num in happynums)
		stdout.printf("%d ", num);
	stdout.printf("\n");
} // end main
