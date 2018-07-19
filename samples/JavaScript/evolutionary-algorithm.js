// ------------------------------------- Cross-browser Compatibility -------------------------------------

/* Compatibility code to reduce an array
 * Source: https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Reduce
 */
if (!Array.prototype.reduce) {
    Array.prototype.reduce = function (fun /*, initialValue */ ) {
        "use strict";

        if (this === void 0 || this === null) throw new TypeError();

        var t = Object(this);
        var len = t.length >>> 0;
        if (typeof fun !== "function") throw new TypeError();

        // no value to return if no initial value and an empty array
        if (len == 0 && arguments.length == 1) throw new TypeError();

        var k = 0;
        var accumulator;
        if (arguments.length >= 2) {
            accumulator = arguments[1];
        } else {
            do {
                if (k in t) {
                    accumulator = t[k++];
                    break;
                }

                // if array contains no values, no initial value to return
                if (++k >= len) throw new TypeError();
            }
            while (true);
        }

        while (k < len) {
            if (k in t) accumulator = fun.call(undefined, accumulator, t[k], k, t);
            k++;
        }

        return accumulator;
    };
}

/* Compatibility code to map an array
 * Source: https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/Map
 */
if (!Array.prototype.map) {
    Array.prototype.map = function (fun /*, thisp */ ) {
        "use strict";

        if (this === void 0 || this === null) throw new TypeError();

        var t = Object(this);
        var len = t.length >>> 0;
        if (typeof fun !== "function") throw new TypeError();

        var res = new Array(len);
        var thisp = arguments[1];
        for (var i = 0; i < len; i++) {
            if (i in t) res[i] = fun.call(thisp, t[i], i, t);
        }

        return res;
    };
}

/* ------------------------------------- Generator -------------------------------------
 * Generates a fixed length gene sequence via a gene strategy object.
 * The gene strategy object must have two functions:
 *	- "create": returns create a new gene
 *	- "mutate(existingGene)": returns mutation of an existing gene
 */
function Generator(length, mutationRate, geneStrategy) {
    this.size = length;
    this.mutationRate = mutationRate;
    this.geneStrategy = geneStrategy;
}

Generator.prototype.spawn = function () {
    var genes = [],
        x;
    for (x = 0; x < this.size; x += 1) {
        genes.push(this.geneStrategy.create());
    }
    return genes;
};

Generator.prototype.mutate = function (parent) {
    return parent.map(function (char) {
        if (Math.random() > this.mutationRate) {
            return char;
        }
        return this.geneStrategy.mutate(char);
    }, this);
};

/* ------------------------------------- Population -------------------------------------
 * Helper class that holds and spawns a new population.
 */
function Population(size, generator) {
    this.size = size;
    this.generator = generator;

    this.population = [];
    // Build initial popuation;
    for (var x = 0; x < this.size; x += 1) {
        this.population.push(this.generator.spawn());
    }
}

Population.prototype.spawn = function (parent) {
    this.population = [];
    for (var x = 0; x < this.size; x += 1) {
        this.population.push(this.generator.mutate(parent));
    }
};

/* ------------------------------------- Evolver -------------------------------------
 * Attempts to converge a population based a fitness strategy object.
 * The fitness strategy object must have three function
 *	- "score(individual)": returns a score for an individual.
 *	- "compare(scoreA, scoreB)": return true if scoreA is better (ie more fit) then scoreB
 *	- "done( score )": return true if score is acceptable (ie we have successfully converged).
 */
function Evolver(size, generator, fitness) {
    this.done = false;
    this.fitness = fitness;
    this.population = new Population(size, generator);
}

Evolver.prototype.getFittest = function () {
    return this.population.population.reduce(function (best, individual) {
        var currentScore = this.fitness.score(individual);
        if (best === null || this.fitness.compare(currentScore, best.score)) {
            return {
                score: currentScore,
                individual: individual
            };
        } else {
            return best;
        }
    }, null);
};

Evolver.prototype.doGeneration = function () {
    this.fittest = this.getFittest();
    this.done = this.fitness.done(this.fittest.score);
    if (!this.done) {
        this.population.spawn(this.fittest.individual);
    }
};

Evolver.prototype.run = function (onCheckpoint, checkPointFrequency) {
    checkPointFrequency = checkPointFrequency || 10; // Default to Checkpoints every 10 generations
    var generation = 0;
    while (!this.done) {
        this.doGeneration();
        if (generation % checkPointFrequency === 0) {
            onCheckpoint(generation, this.fittest);
        }
        generation += 1;
    }
    onCheckpoint(generation, this.fittest);
    return this.fittest;
};

// ------------------------------------- Exports -------------------------------------
window.Generator = Generator;
window.Evolver = Evolver;


// helper utitlity to combine elements of two arrays.
Array.prototype.zip = function (b, func) {
    var result = [],
        max = Math.max(this.length, b.length),
        x;
    for (x = 0; x < max; x += 1) {
        result.push(func(this[x], b[x]));
    }
    return result;
};

var target = "METHINKS IT IS LIKE A WEASEL", geneStrategy, fitness, target, generator, evolver, result;

geneStrategy = {
    // The allowed character set (as an array)
    characterSet: "ABCDEFGHIJKLMNOPQRSTUVWXYZ ".split(""),

    /*
        Pick a random character from the characterSet
    */
    create: function getRandomGene() {
        var randomNumber = Math.floor(Math.random() * this.characterSet.length);
        return this.characterSet[randomNumber];
    }
};
geneStrategy.mutate = geneStrategy.create; // Our mutation stragtegy is to simply get a random gene
fitness = {
    // The target (as an array of characters)
    target: target.split(""),
    equal: function (geneA, geneB) {
        return (geneA === geneB ? 0 : 1);
    },
    sum: function (runningTotal, value) {
        return runningTotal + value;
    },

    /*
        We give one point to for each corect letter
    */
    score: function (genes) {
        var diff = genes.zip(this.target, this.equal); // create an array of ones and zeros
        return diff.reduce(this.sum, 0); // Sum the array values together.
    },
    compare: function (scoreA, scoreB) {
        return scoreA <= scoreB; // Lower scores are better
    },
    done: function (score) {
        return score === 0; // We have matched the target string.
    }
};

generator = new Generator(target.length, 0.05, geneStrategy);
evolver = new Evolver(100, generator, fitness);

function showProgress(generation, fittest) {
    document.write("Generation: " + generation + ", Best: " + fittest.individual.join("") + ", fitness:" + fittest.score + "<br>");
}
result = evolver.run(showProgress);
