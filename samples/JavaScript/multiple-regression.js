// modifies the matrix "in place"
Matrix.prototype.inverse = function() {
    if (this.height != this.width) {
        throw "can't invert a non-square matrix";
    }

    var I = new IdentityMatrix(this.height);
    for (var i = 0; i < this.height; i++)
        this.mtx[i] = this.mtx[i].concat(I.mtx[i])
    this.width *= 2;

    this.toReducedRowEchelonForm();

    for (var i = 0; i < this.height; i++)
        this.mtx[i].splice(0, this.height);
    this.width /= 2;

    return this;
}

function ColumnVector(ary) {
    return new Matrix(ary.map(function(v) {return [v]}))
}
ColumnVector.prototype = Matrix.prototype

Matrix.prototype.regression_coefficients = function(x) {
    var x_t = x.transpose();
    return x_t.mult(x).inverse().mult(x_t).mult(this);
}

// the Ruby example
var y = new ColumnVector([1,2,3,4,5]);
var x = new ColumnVector([2,1,3,4,5]);
print(y.regression_coefficients(x));
print();

// the Tcl example
y = new ColumnVector([
    52.21, 53.12, 54.48, 55.84, 57.20, 58.57, 59.93, 61.29,
    63.11, 64.47, 66.28, 68.10, 69.92, 72.19, 74.46
]);
x = new Matrix(
    [1.47,1.50,1.52,1.55,1.57,1.60,1.63,1.65,1.68,1.70,1.73,1.75,1.78,1.80,1.83].map(
        function(v) {return [Math.pow(v,0), Math.pow(v,1), Math.pow(v,2)]}
    )
);
print(y.regression_coefficients(x));
