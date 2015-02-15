using Gee;

ArrayList<int> primes(int limit){
	var sieve = new ArrayList<bool>();
	var prime_list = new ArrayList<int>();
	
	for(int i = 0; i <= limit; i++){
		sieve.add(true);
	}
	
	sieve[0] = false;
	sieve[1] = false;
	
	for (int i = 2; i <= limit/2; i++){
		if (sieve[i] != false){
			for (int j = 2; i*j <= limit; j++){
				sieve[i*j] = false;
			}
		}
	}

	for (int i = 0; i < sieve.size; i++){
		if (sieve[i] != false){
			prime_list.add(i);
		}
	}
	
	return prime_list;
} // end primes

public static void main(){
	var prime_list = primes(50);
	
	foreach(var prime in prime_list)
		stdout.printf("%s ", prime.to_string());
	
	stdout.printf("\n");
}
