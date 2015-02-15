var R = [null, 1];
var S = [null, 2];

var extend_sequences = function (n) {
	var current = Math.max(R[R.length-1],S[S.length-1]);
	var i;
	while (R.length <= n || S.length <= n) {
		i = Math.min(R.length, S.length) - 1;
		current += 1;
		if (current === R[i] + S[i]) {
			R.push(current);
		} else {
			S.push(current);
		}
	}
}

var ffr = function(n) {
	extend_sequences(n);
	return R[n];
};

var ffs = function(n) {
	extend_sequences(n);
	return S[n];
};

for (var i = 1; i <=10; i += 1) {
   console.log('R('+ i +') = ' + ffr(i));
}

var int_array = [];

for (var i = 1; i <= 40; i += 1) {
	int_array.push(ffr(i));
}
for (var i = 1; i <= 960; i += 1) {
	int_array.push(ffs(i));
}

int_array.sort(function(a,b){return a-b;});

for (var i = 1; i <= 1000; i += 1) {
	if (int_array[i-1] !== i) {
		throw "Something's wrong!"
	} else { console.log("1000 integer check ok."); }
}
