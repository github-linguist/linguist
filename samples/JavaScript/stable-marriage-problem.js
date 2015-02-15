function Person(name) {

    var candidateIndex = 0;

    this.name = name;
    this.fiance = null;
    this.candidates = [];

    this.rank = function(p) {
        for (i = 0; i < this.candidates.length; i++)
            if (this.candidates[i] === p) return i;
        return this.candidates.length + 1;
    }

    this.prefers = function(p) {
        return this.rank(p) < this.rank(this.fiance);
    }

    this.nextCandidate = function() {
        if (candidateIndex >= this.candidates.length) return null;
        return this.candidates[candidateIndex++];
    }

    this.engageTo = function(p) {
        if (p.fiance) p.fiance.fiance = null;
        p.fiance = this;
        if (this.fiance) this.fiance.fiance = null;
        this.fiance = p;
    }

    this.swapWith = function(p) {
        console.log("%s & %s swap partners", this.name, p.name);
        var thisFiance = this.fiance;
        var pFiance = p.fiance;
        this.engageTo(pFiance);
        p.engageTo(thisFiance);
    }
}

function isStable(guys, gals) {
    for (var i = 0; i < guys.length; i++)
        for (var j = 0; j < gals.length; j++)
            if (guys[i].prefers(gals[j]) && gals[j].prefers(guys[i]))
                return false;
    return true;
}

function engageEveryone(guys) {
    var done;
    do {
        done = true;
        for (var i = 0; i < guys.length; i++) {
            var guy = guys[i];
            if (!guy.fiance) {
                done = false;
                var gal = guy.nextCandidate();
                if (!gal.fiance || gal.prefers(guy))
                    guy.engageTo(gal);
            }
        }
    } while (!done);
}

function doMarriage() {

    var abe  = new Person("Abe");
    var bob  = new Person("Bob");
    var col  = new Person("Col");
    var dan  = new Person("Dan");
    var ed   = new Person("Ed");
    var fred = new Person("Fred");
    var gav  = new Person("Gav");
    var hal  = new Person("Hal");
    var ian  = new Person("Ian");
    var jon  = new Person("Jon");
    var abi  = new Person("Abi");
    var bea  = new Person("Bea");
    var cath = new Person("Cath");
    var dee  = new Person("Dee");
    var eve  = new Person("Eve");
    var fay  = new Person("Fay");
    var gay  = new Person("Gay");
    var hope = new Person("Hope");
    var ivy  = new Person("Ivy");
    var jan  = new Person("Jan");

    abe.candidates  = [abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay];
    bob.candidates  = [cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay];
    col.candidates  = [hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan];
    dan.candidates  = [ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi];
    ed.candidates   = [jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay];
    fred.candidates = [bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay];
    gav.candidates  = [gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay];
    hal.candidates  = [abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee];
    ian.candidates  = [hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve];
    jon.candidates  = [abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope];
    abi.candidates  = [bob, fred, jon, gav, ian, abe, dan, ed, col, hal];
    bea.candidates  = [bob, abe, col, fred, gav, dan, ian, ed, jon, hal];
    cath.candidates = [fred, bob, ed, gav, hal, col, ian, abe, dan, jon];
    dee.candidates  = [fred, jon, col, abe, ian, hal, gav, dan, bob, ed];
    eve.candidates  = [jon, hal, fred, dan, abe, gav, col, ed, ian, bob];
    fay.candidates  = [bob, abe, ed, ian, jon, dan, fred, gav, col, hal];
    gay.candidates  = [jon, gav, hal, fred, bob, abe, col, ed, dan, ian];
    hope.candidates = [gav, jon, bob, abe, ian, dan, hal, ed, col, fred];
    ivy.candidates  = [ian, col, hal, gav, fred, bob, abe, ed, jon, dan];
    jan.candidates  = [ed, hal, gav, abe, bob, jon, col, ian, fred, dan];

    var guys = [abe, bob, col, dan, ed, fred, gav, hal, ian, jon];
    var gals = [abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan];

    engageEveryone(guys);

    for (var i = 0; i < guys.length; i++) {
        console.log("%s is engaged to %s", guys[i].name, guys[i].fiance.name);
    }
    console.log("Stable = %s", isStable(guys, gals) ? "Yes" : "No");
    jon.swapWith(fred);
    console.log("Stable = %s", isStable(guys, gals) ? "Yes" : "No");
}

doMarriage();
