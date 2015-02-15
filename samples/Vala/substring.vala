string s = "Hello, world!";
int n = 1;
int m = 3;
// start at n and go m letters
string s_n_to_m = s[n:n+m];
// start at n and go to end
string s_n_to_end = s[n:s.length];
// start at beginning and show all but last
string s_notlast = s[0:s.length - 1];
// start from known letter and then go m letters
int index_of_l = s.index_of("l");
string s_froml_for_m = s[index_of_l:index_of_l + m];
// start from known substring then go m letters
int index_of_lo = s.index_of("lo");
string s_fromlo_for_m = s[index_of_lo:index_of_lo + m];
