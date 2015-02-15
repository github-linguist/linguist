function erasthenes(limit) {
    var primes = [];
    if (limit >= 2) {
        var nums = new Array(limit-1);
        for (var i = 2; i <= limit; i++)
            nums[i-2] = i;

        var last_prime;
        var idx = 0;
        while ((last_prime = nums[idx]) <= Math.sqrt(limit)) {
            if (last_prime != null)
                for (var i = idx + last_prime; i < limit - 1; i += last_prime)
                    nums[i] = null;
            idx++;
        }
        for (var i = 0; i < nums.length; i++)
            if (nums[i] != null)
                primes.push(nums[i]);
    }
    return primes;
}

var primes = erasthenes(100);

if (typeof print == "undefined")
    print = (typeof WScript != "undefined") ? WScript.Echo : alert;
print(primes);
