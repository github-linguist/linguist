using Gee;

void main(){
    // mostly copied from C# example
    var hashmap = new HashMap<string, string>();

    string[] arg_keys = {"foo", "bar", "val"};
    string[] arg_values = {"little", "miss", "muffet"};

    if (arg_keys.length	== arg_values.length ){
	for (int i = 0;	i < arg_keys.length; i++){
            hashmap[arg_keys[i]] = arg_values[i];
	}
    }
}
