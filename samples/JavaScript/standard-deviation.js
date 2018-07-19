function running_stddev() {
    var n = 0;
    var sum = 0.0;
    var sum_sq = 0.0;
    return function(num) {
        n++;
        sum += num;
        sum_sq += num*num;
        return Math.sqrt( (sum_sq / n) - Math.pow(sum / n, 2) );
    }
}

var sd = running_stddev();
var nums = [2,4,4,4,5,5,7,9];
var stddev = [];
for (var i in nums)
    stddev.push( sd(nums[i]) );

// using WSH
WScript.Echo(stddev.join(', ');
