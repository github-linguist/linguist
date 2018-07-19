use("benchmark")

func = method((1..50000) reduce(+))

Benchmark report(1, 1, func)
