(function() {
	var orig= document.body.innerHTML
	document.body.innerHTML= '';
	setTimeout(function() {
		document.body.innerHTML= 'something';
		setTimeout(function() {
			document.body.innerHTML= orig;
		}, 1000);
	}, 1000);
})();
