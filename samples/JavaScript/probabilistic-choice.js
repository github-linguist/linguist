var probabilities = {
    aleph:  1/5.0,
    beth:   1/6.0,
    gimel:  1/7.0,
    daleth: 1/8.0,
    he:     1/9.0,
    waw:    1/10.0,
    zayin:  1/11.0,
    heth:   1759/27720
};

var sum = 0;
var iterations = 1000000;
var cumulative = {};
var randomly = {};
for (var name in probabilities) {
    sum += probabilities[name];
    cumulative[name] = sum;
    randomly[name] = 0;
}
for (var i = 0; i < iterations; i++) {
    var r = Math.random();
    for (var name in cumulative) {
        if (r <= cumulative[name]) {
            randomly[name]++;
            break;
        }
    }
}
for (var name in probabilities)
    // using WSH
    WScript.Echo(name + "\t" + probabilities[name] + "\t" + randomly[name]/iterations);
