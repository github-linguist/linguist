int main() {
	// Decoding
	string json = "{\"cake\":[\"desu\",1,2.3],\"foo\":1}";
	write("%O\n", Standards.JSON.decode(json));
	
	// Encoding
	mapping m = ([
		"foo": ({ 1, 2, 3 }),
		"bar": "hello"
	]);
	
	write("%s\n", Standards.JSON.encode(m));
}
