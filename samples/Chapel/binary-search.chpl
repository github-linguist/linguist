proc binsearch(A:[], value) {
        var low = A.domain.dim(1).low;
        var high = A.domain.dim(1).high;
        while (low <= high) {
                var mid = (low + high) / 2;

                if A(mid) > value then
                        high = mid - 1;
                else if A(mid) < value then
                        low = mid + 1;
                else
                        return mid;
        }
        return 0;
}

writeln(binsearch([3, 4, 6, 9, 11], 9));
