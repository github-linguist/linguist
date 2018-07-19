<html><head><title>Catalan</title></head>
<body><pre id='x'></pre><script type="application/javascript">
function disp(x) {
	var e = document.createTextNode(x + '\n');
	document.getElementById('x').appendChild(e);
}

var fc = [], c2 = [], c3 = [];
function fact(n) { return fc[n] ? fc[n] : fc[n] = (n ? n * fact(n - 1) : 1); }
function cata1(n) { return Math.floor(fact(2 * n) / fact(n + 1) / fact(n) + .5); }
function cata2(n) {
	if (n == 0) return 1;
	if (!c2[n]) {
		var s = 0;
		for (var i = 0; i < n; i++) s += cata2(i) * cata2(n - i - 1);
		c2[n] = s;
	}
	return c2[n];
}
function cata3(n) {
	if (n == 0) return 1;
	return c3[n] ? c3[n] : c3[n] = (4 * n - 2) * cata3(n - 1) / (n + 1);
}

disp("       meth1   meth2   meth3");
for (var i = 0; i <= 15; i++)
	disp(i + '\t' + cata1(i) + '\t' + cata2(i) + '\t' + cata3(i));

</script></body></html>
