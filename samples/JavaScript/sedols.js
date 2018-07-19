function sedol(input) {
    return input + sedol_check_digit(input);
}

var weight = [1, 3, 1, 7, 3, 9, 1];
function sedol_check_digit(char6) {
    if (char6.search(/^[0-9BCDFGHJKLMNPQRSTVWXYZ]{6}$/) == -1)
        throw "Invalid SEDOL number '" + char6 + "'";
    var sum = 0;
    for (var i = 0; i < char6.length; i++)
        sum += weight[i] * parseInt(char6.charAt(i), 36);
    var check = (10 - sum%10) % 10;
    return check.toString();
}

var input = [
    '710889', 'B0YBKJ', '406566', 'B0YBLH', '228276',
    'B0YBKL', '557910', 'B0YBKR', '585284', 'B0YBKT',
    "BOATER" , "12345", "123456", "1234567"
];

var expected = [
    '7108899', 'B0YBKJ7', '4065663', 'B0YBLH2', '2282765',
    'B0YBKL9', '5579107', 'B0YBKR5', '5852842', 'B0YBKT7',
    null, null, '1234563', null
];

for (var i in input) {
    try {
        var sedolized = sedol(input[i]);
        if (sedolized == expected[i])
            print(sedolized);
        else
            print("error: calculated sedol for input " + input[i] +
                  " is " + sedolized + ", but it should be " + expected[i]
            );
    }
    catch (e) {
        print("error: " + e);
    }
}
