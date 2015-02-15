void main() {
	import std.range, std.conv;

	string s1 = "hello"; // UTF-8
	assert(s1.retro.text == "olleh");

	wstring s2 = "hello"w; // UTF-16
	assert(s2.retro.wtext == "olleh"w);

	dstring s3 = "hello"d; // UTF-32
	assert(s3.retro.dtext == "olleh"d);

	// without using std.range:
	dstring s4 = "hello"d;
	assert(s4.dup.reverse == "olleh"d); // simple but inefficient (copies first, then reverses)
}
