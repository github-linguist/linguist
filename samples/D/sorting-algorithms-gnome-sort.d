import std.stdio, std.algorithm;

void gnomeSort(T)(T arr) {
    int i = 1, j = 2;
    while (i < arr.length) {
        if (arr[i-1] <= arr[i]) {
            i = j;
            j++;
        } else {
            swap(arr[i-1], arr[i]);
            i--;
            if (i == 0) {
                i = j;
                j++;
            }
        }
    }
}

void main() {
    auto a = [3,4,2,5,1,6];
    gnomeSort(a);
    writeln(a);
}
