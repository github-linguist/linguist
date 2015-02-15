using Gee;

void main(){
    var	map = new HashMap<string, int>(); // creates a HashMap with keys of type string, and values of type int

    // two methods to set key,value pair
    map["one"] = 1;
    map["two"] = 2;

    map.set("four", 4);
    map.set("five", 5);

    // two methods of getting key,value pair
    stdout.printf("%d\n", map["one"]);

    stdout.printf("%d\n", map.get("two"));
}
