var gold = { 'value': 2500, 'weight': 2.0, 'volume': 0.002 },
    panacea = { 'value': 3000, 'weight': 0.3, 'volume': 0.025 },
    ichor = { 'value': 1800, 'weight': 0.2, 'volume': 0.015 },

    items = [gold, panacea, ichor],
    knapsack = {'weight': 25, 'volume': 0.25},
    max_val = 0,
    solutions = [],
    g, p, i, item, val;

for (i = 0; i < items.length; i += 1) {
    item = items[i];
    item.max = Math.min(
        Math.floor(knapsack.weight / item.weight),
        Math.floor(knapsack.volume / item.volume)
    );
}

for (g = 0; g <= gold.max; g += 1) {
    for (p = 0; p <= panacea.max; p += 1) {
        for (i = 0; i <= ichor.max; i += 1) {
            if (i * ichor.weight + g * gold.weight + p * panacea.weight > knapsack.weight) {
                continue;
            }
            if (i * ichor.volume + g * gold.volume + p * panacea.volume > knapsack.volume) {
                continue;
            }
            val = i * ichor.value + g * gold.value + p * panacea.value;
            if (val > max_val) {
                solutions = [];
                max_val = val;
            }
            if (val === max_val) {
                solutions.push([g, p, i]);
            }
        }
    }
}

document.write("maximum value: " + max_val + '<br>');
for (i = 0; i < solutions.length; i += 1) {
    item = solutions[i];
    document.write("(gold: " + item[0] + ", panacea: " + item[1] + ", ichor: " + item[2] + ")<br>");
}

output:
<pre>maximum value: 54500
(gold: 11, panacea: 0, ichor: 15)
(gold: 11, panacea: 3, ichor: 10)
(gold: 11, panacea: 6, ichor: 5)
(gold: 11, panacea: 9, ichor: 0)</pre>
