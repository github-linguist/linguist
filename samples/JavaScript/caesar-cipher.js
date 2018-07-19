<html><head><title>Caesar</title></head>
<body><pre id='x'></pre>
<script type="application/javascript">
function disp(x) {
	var e = document.createTextNode(x + '\n');
	document.getElementById('x').appendChild(e);
}

function trans(msg, rot) {
	return msg.replace(/([a-z])/ig,
		function($1) {
			var c = $1.charCodeAt(0);
			return String.fromCharCode(
				c >= 97 ? (c + rot + 26 - 97) % 26 + 97
					: (c + rot + 26 - 65) % 26 + 65);
		});
}

var msg = "The quick brown f0x Jumped over the lazy Dog 123";
var enc = trans(msg,  3);
var dec = trans(enc, -3);

disp("Original:" + msg + "\nEncoded: " + enc + "\nDecoded: " + dec);
</script></body></html>
