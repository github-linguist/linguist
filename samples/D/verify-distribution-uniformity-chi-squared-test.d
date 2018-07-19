import std.stdio, std.algorithm, std.mathspecial;

real x2Dist(T)(in T[] data) pure nothrow {
    immutable avg = data.sum / data.length;
    immutable sqs = reduce!((a, b) => a + (b - avg) ^^ 2)(0.0L, data);
    return sqs / avg;
}

real x2Prob(in real dof, in real distance) /*pure nothrow*/ {
    return gammaIncompleteCompl(dof / 2, distance / 2);
}

bool x2IsUniform(T)(in T[] data, in real significance=0.05L)
/*pure nothrow*/ {
    return x2Prob(data.length - 1.0L, x2Dist(data)) > significance;
}

void main() {
    immutable dataSets = [[199809, 200665, 199607, 200270, 199649],
                          [522573, 244456, 139979,  71531,  21461]];
    writefln(" %4s %12s  %12s %8s   %s",
             "dof", "distance", "probability", "Uniform?", "dataset");
    foreach (immutable ds; dataSets) {
        immutable dof = ds.length - 1;
        immutable dist = ds.x2Dist;
        immutable prob = x2Prob(dof, dist);
        writefln("%4d %12.3f  %12.8f    %5s    %6s",
                 dof, dist, prob, ds.x2IsUniform ? "YES" : "NO", ds);
    }
}
