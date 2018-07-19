array flatten(array a) {
	array r = ({ });
	
	foreach (a, mixed n) {
		if (arrayp(n)) r += flatten(n);
		else r += ({ n });
	}
	
	return r;
}
