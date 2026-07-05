/* https://www.nokia.com/bell-labs/about/dennis-m-ritchie/bref.html */
/* This function replaces each upper case character in the input
string s by its lower case equivalent.   It uses the fact that
the ascii alphabetic characters are contiguous. */

lower(s) {
	auto c,i;
	i = -1;
	while( (c=char(s,++i)) != '*e'  )
	if( c >= 'A' & c <= 'Z' ) lchar(s~i~c-'A'+'a');
}
