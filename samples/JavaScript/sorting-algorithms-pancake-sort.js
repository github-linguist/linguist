Array.prototype.pancake_sort = function () {
    for (var i = this.length - 1; i >= 1; i--) {
        // find the index of the largest element not yet sorted
        var max_idx = 0;
        var max = this[0];
        for (var j = 1; j <= i; j++) {
            if (this[j] > max) {
                max = this[j];
                max_idx = j;
            }
        }

        if (max_idx == i)
            continue; // element already in place

        var new_slice;

        // flip this max element to index 0
        if (max_idx > 0) {
            new_slice = this.slice(0, max_idx+1).reverse();
            for (var j = 0; j <= max_idx; j++)
                this[j] = new_slice[j];
        }

        // then flip the max element to its place
        new_slice = this.slice(0, i+1).reverse();
        for (var j = 0; j <= i; j++)
            this[j] = new_slice[j];
    }
    return this;
}
ary = [7,6,5,9,8,4,3,1,2,0]
sorted = ary.concat().pancake_sort();
