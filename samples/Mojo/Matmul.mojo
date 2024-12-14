from autotune import autotune, search
from algorithm import parallelize, Static2DTileUnitFunc as Tile2DFunc, vectorize_unroll
from benchmark import Benchmark
from sys.intrinsics import strided_load
from utils.list import VariadicList
from math import div_ceil, min
from memory import memset_zero
from memory.unsafe import DTypePointer, Pointer
from random import rand, random_float64
from sys.info import simdwidthof
from time import now


struct Matrix:
    var data: DTypePointer[DType.float32]
    var rows: Int
    var cols: Int

    fn __init__(inout self, rows: Int, cols: Int):
        self.data = DTypePointer[DType.float32].alloc(rows * cols)
        rand(self.data, rows * cols)
        self.rows = rows
        self.cols = cols

    fn __del__(owned self):
        self.data.free()

    fn zero(inout self):
        memset_zero(self.data, self.rows * self.cols)

    @always_inline
    fn __getitem__(self, y: Int, x: Int) -> Float32:
        return self.load[1](y, x)

    @always_inline
    fn load[nelts: Int](self, y: Int, x: Int) -> SIMD[DType.float32, nelts]:
        return self.data.simd_load[nelts](y * self.cols + x)

    @always_inline
    fn __setitem__(self, y: Int, x: Int, val: Float32):
        return self.store[1](y, x, val)

    @always_inline
    fn store[nelts: Int](self, y: Int, x: Int, val: SIMD[DType.float32, nelts]):
        self.data.simd_store[nelts](y * self.cols + x, val)


alias matmul_fn_sig_type = fn (Matrix, Matrix, Matrix) -> None


# Perform 2D tiling on the iteration space defined by end_x and end_y.
fn tile[tiled_fn: Tile2DFunc, tile_x: Int, tile_y: Int](end_x: Int, end_y: Int):
    # Note: this assumes that ends are multiples of the tiles.
    for y in range(0, end_y, tile_y):
        for x in range(0, end_x, tile_x):
            tiled_fn[tile_x, tile_y](x, y)


alias nelts = simdwidthof[DType.float32]()  # The SIMD vector width.


# Autotune the tile size used in the matmul.
@adaptive
fn matmul_autotune_impl(C: Matrix, A: Matrix, B: Matrix):
    @parameter
    fn calc_row(m: Int):
        @parameter
        fn calc_tile[tile_x: Int, tile_y: Int](x: Int, y: Int):
            for k in range(y, y + tile_y):

                @parameter
                fn dot[
                    nelts: Int,
                ](n: Int):
                    C.store[nelts](
                        m,
                        n + x,
                        C.load[nelts](m, n + x) + A[m, k] * B.load[nelts](k, n + x),
                    )

                vectorize_unroll[nelts, tile_x // nelts, dot](tile_x)

        # Instead of hardcoding to tile_size = 4, search for the fastest
        # tile size by evaluting this function as tile size varies.
        alias tile_size = autotune(1, 2, 4, 8, 16, 32)
        tile[calc_tile, nelts * tile_size, tile_size](A.cols, C.cols)

    parallelize[calc_row](C.rows)


fn matmul_evaluator(funcs: Pointer[matmul_fn_sig_type], size: Int) -> Int:
    print("matmul_evaluator, number of candidates: ", size)

    let eval_begin: Int = now()

    # This size is picked at random, in real code we could use a real size
    # distribution here.
    let M = 512
    let N = 512
    let K = 512
    print("Optimizing for size:", M, "x", N, "x", K)

    var best_idx: Int = -1
    var best_time: Int = -1

    alias eval_iterations = 10
    alias eval_samples = 10

    var C = Matrix(M, N)
    var A = Matrix(M, K)
    var B = Matrix(K, N)
    let Cptr = Pointer[Matrix].address_of(C).address
    let Aptr = Pointer[Matrix].address_of(A).address
    let Bptr = Pointer[Matrix].address_of(B).address

    # Find the function that's the fastest on the size we're optimizing for
    for f_idx in range(size):
        let func = funcs.load(f_idx)

        @always_inline
        @parameter
        fn wrapper():
            func(C, A, B)

        let cur_time = Benchmark(1, 100_000, 500_000_000, 1000_000_000).run[wrapper]()

        if best_idx < 0:
            best_idx = f_idx
            best_time = cur_time
        if best_time > cur_time:
            best_idx = f_idx
            best_time = cur_time

    let eval_end: Int = now()
    # Prevent matrices from being destroyed before we finished benchmarking them.
    _ = A.data
    _ = B.data
    _ = C.data
    print("Time spent in matmul_evaluator, ms:", (eval_end - eval_begin) // 1000000)
    print("Best candidate idx:", best_idx)
    return best_idx


fn matmul_autotune(C: Matrix, A: Matrix, B: Matrix):
    alias best_impl: matmul_fn_sig_type
    search[
        matmul_fn_sig_type,
        VariadicList(matmul_autotune_impl.__adaptive_set),
        matmul_evaluator -> best_impl
    ]()
    # Run the best candidate
    return best_impl(C, A, B)
