<html><head><title>Table maker</title><script type="application/javascript">

// normally, don't do this: at least name it something other than "a"
Node.prototype.a = function (e) { this.appendChild(e); return this }

function ce(tag, txt) {
	var x = document.createElement(tag);
	x.textContent = (txt === undefined) ? '' : txt;
	return x;
}

function make_table(cols, rows) {
	var tbl = ce('table', ''), tr = ce('tr'), th;

	tbl.a(tr.a(ce('th')));

	var z = 'Z'.charCodeAt(0);
	for (var l = z - cols + 1; l <= z; l++)
		tr.a(ce('th', String.fromCharCode(l)));

	for (var r = 1; r <= rows; r++) {
		tbl.a(tr = ce('tr').a(ce('th', r)));
		for (var c = 0; c < cols; c++)
			tr.a(ce('td', Math.floor(Math.random() * 10000)));
	}

	document.body
		.a(ce('style',
			'td, th {border: 1px solid #696;' +
			'padding:.4ex} td {text-align: right }' +
			'table { border-collapse: collapse}'))
		.a(tbl);
}
</script></head>
<body><script>make_table(5, 4)</script></body></html>
