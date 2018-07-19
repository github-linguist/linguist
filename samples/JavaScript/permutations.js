<html><head><title>Permutations</title></head>
<body><pre id="result"></pre>
<script type="text/javascript">
var d = document.getElementById('result');

function perm(list, ret)
{
	if (list.length == 0) {
		var row = document.createTextNode(ret.join(' ') + '\n');
		d.appendChild(row);
		return;
	}
	for (var i = 0; i < list.length; i++) {
		var x = list.splice(i, 1);
		ret.push(x);
		perm(list, ret);
		ret.pop();
		list.splice(i, 0, x);
	}
}

perm([1, 2, 'A', 4], []);
</script></body></html>
