using Gee;

void main(){
    // declare HashMap
    var map = new HashMap<string, double?>();

    // set 3 entries
    map["pi"] = 3.14;
    map["e"] = 2.72;
    map["golden"] = 1.62;

    // iterate over (key,value) pair
    foreach (var elem in map.entries){
        string name = elem.key;
        double num = elem.value;

	stdout.printf("%s,%f\n", name, num);
    }

    // iterate over keys
    foreach (string key in map.keys){
	stdout.printf("%s\n", key);
    }

    // iterate over values
    foreach (double num in map.values){
	stdout.printf("%f\n", num);
    }
}
