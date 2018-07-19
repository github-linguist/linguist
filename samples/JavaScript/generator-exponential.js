function PowersGenerator(m) {
	var n=0;
	while(1) {
		yield Math.pow(n, m);
		n += 1;	
	}
}

function FilteredGenerator(g, f){
	var value = g.next();
	var filter = f.next();
	
	while(1) {
		if( value < filter ) {
			yield value;
			value = g.next();
		} else if ( value > filter ) {
			filter = f.next();
		} else {
			value = g.next();
			filter = f.next();
		}
	}	
}



var squares = PowersGenerator(2);
var cubes = PowersGenerator(3);

var filtered = FilteredGenerator(squares, cubes);



for( var x = 0; x < 20; x++ ) filtered.next()
for( var x = 20; x < 30; x++ ) console.logfiltered.next());
