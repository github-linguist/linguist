// Pascal's triangle object
function pascalTriangle (rows) {

	// Number of rows the triangle contains
	this.rows = rows;

	// The 2D array holding the rows of the triangle
	this.triangle = new Array();
	for (var r = 0; r < rows; r++) {
		this.triangle[r] = new Array();
		for (var i = 0; i <= r; i++) {
			if (i == 0 || i == r)
				this.triangle[r][i] = 1;
			else
				this.triangle[r][i] = this.triangle[r-1][i-1]+this.triangle[r-1][i];
		}
	}

	// Method to print the triangle
	this.print = function(base) {
		if (!base)
			base = 10;

		// Private method to calculate digits in number
		var digits = function(n,b) {
			var d = 0;
			while (n >= 1) {
				d++;
				n /= b;
			}
			return d;
		}

		// Calculate max spaces needed
		var spacing = digits(this.triangle[this.rows-1][Math.round(this.rows/2)],base);

		// Private method to add spacing between numbers
		var insertSpaces = function(s) {
			var buf = "";
			while (s > 0) {
				s--;
				buf += " ";
			}
			return buf;
		}

		// Print the triangle line by line
		for (var r = 0; r < this.triangle.length; r++) {
			var l = "";
			for (var s = 0; s < Math.round(this.rows-1-r); s++) {
				l += insertSpaces(spacing);
			}
			for (var i = 0; i < this.triangle[r].length; i++) {
				if (i != 0)
					l += insertSpaces(spacing-Math.ceil(digits(this.triangle[r][i],base)/2));
				l += this.triangle[r][i].toString(base);
				if (i < this.triangle[r].length-1)
					l += insertSpaces(spacing-Math.floor(digits(this.triangle[r][i],base)/2));
			}
			print(l);
		}
	}

}

// Display 4 row triangle in base 10
var tri = new pascalTriangle(4);
tri.print();
// Display 8 row triangle in base 16
tri = new pascalTriangle(8);
tri.print(16);
