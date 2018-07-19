<html><head><title>Vigenère</title></head>
<body><pre id='x'></pre>
<script type="application/javascript">
function disp(x) {
	var e = document.createTextNode(x + '\n');
	document.getElementById('x').appendChild(e);
}

function ord(x) { return x.charCodeAt(0) }
function chr(x) { return String.fromCharCode(x) }
function rot(a, b, decode) {
	return decode	? chr((26 + ord(a) - ord(b)) % 26 + ord('A'))
			: chr((26 + ord(a) + ord(b) - ord('A') * 2) % 26 + ord('A')) }

function trans(msg, key, decode) {
	var i = 0;
	key = key.toUpperCase();
	msg = msg.toUpperCase().replace(/[^A-Z]/g, '');
	return msg.replace(/([A-Z])/g,
		function($1) { return rot($1, key[i++ % key.length], decode) });
}

var msg = "The quick brown fox Jumped over the lazy Dog the lazy dog lazy dog dog";
var key = 'VIGENERECIPHER';
var enc = trans(msg, key);
var dec = trans(enc, key, 'decipher');

disp("Original:" + msg + "\nEncoded: " + enc + "\nDecoded: " + dec);
</script></body></html>
