// modifies the matrix in-place
Matrix.prototype.toReducedRowEchelonForm = function() {
    var lead = 0;
    for (var r = 0; r < this.rows(); r++) {
        if (this.columns() <= lead) {
            return;
        }
        var i = r;
        while (this.mtx[i][lead] == 0) {
            i++;
            if (this.rows() == i) {
                i = r;
                lead++;
                if (this.columns() == lead) {
                    return;
                }
            }
        }

        var tmp = this.mtx[i];
        this.mtx[i] = this.mtx[r];
        this.mtx[r] = tmp;

        var val = this.mtx[r][lead];
        for (var j = 0; j < this.columns(); j++) {
            this.mtx[r][j] /= val;
        }

        for (var i = 0; i < this.rows(); i++) {
            if (i == r) continue;
            val = this.mtx[i][lead];
            for (var j = 0; j < this.columns(); j++) {
                this.mtx[i][j] -= val * this.mtx[r][j];
            }
        }
        lead++;
    }
    return this;
}

var m = new Matrix([
  [ 1, 2, -1, -4],
  [ 2, 3, -1,-11],
  [-2, 0, -3, 22]
]);
print(m.toReducedRowEchelonForm());
print();

m = new Matrix([
  [ 1, 2, 3, 7],
  [-4, 7,-2, 7],
  [ 3, 3, 0, 7]
]);
print(m.toReducedRowEchelonForm());
