import std.stdio, std.algorithm;

T median(T)(T[] nums) /*pure nothrow*/ {
    nums.sort();
    if (nums.length & 1)
        return nums[$ / 2];
    else
        return (nums[$ / 2 - 1] + nums[$ / 2]) / 2.0;
}

void main() {
    auto a1 = [5.1, 2.6, 6.2, 8.8, 4.6, 4.1];
    writeln("Even median: ", median(a1));

    auto a2 = [5.1, 2.6, 8.8, 4.6, 4.1];
    writeln("Odd median:  ", median(a2));
}
