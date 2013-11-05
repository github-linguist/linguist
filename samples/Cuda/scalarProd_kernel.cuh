__global__ void scalarProdGPU(
    float *d_C,
    float *d_A,
    float *d_B,
    int vectorN,
    int elementN
)
{
    //Accumulators cache
    __shared__ float accumResult[ACCUM_N];

    ////////////////////////////////////////////////////////////////////////////
    // Cycle through every pair of vectors,
    // taking into account that vector counts can be different
    // from total number of thread blocks
    ////////////////////////////////////////////////////////////////////////////
    for (int vec = blockIdx.x; vec < vectorN; vec += gridDim.x)
    {
        int vectorBase = IMUL(elementN, vec);
        int vectorEnd  = vectorBase + elementN;

        ////////////////////////////////////////////////////////////////////////
        // Each accumulator cycles through vectors with
        // stride equal to number of total number of accumulators ACCUM_N
        // At this stage ACCUM_N is only preferred be a multiple of warp size
        // to meet memory coalescing alignment constraints.
        ////////////////////////////////////////////////////////////////////////
        for (int iAccum = threadIdx.x; iAccum < ACCUM_N; iAccum += blockDim.x)
        {
            float sum = 0;

            for (int pos = vectorBase + iAccum; pos < vectorEnd; pos += ACCUM_N)
                sum += d_A[pos] * d_B[pos];

            accumResult[iAccum] = sum;
        }

        ////////////////////////////////////////////////////////////////////////
        // Perform tree-like reduction of accumulators' results.
        // ACCUM_N has to be power of two at this stage
        ////////////////////////////////////////////////////////////////////////
        for (int stride = ACCUM_N / 2; stride > 0; stride >>= 1)
        {
            __syncthreads();

            for (int iAccum = threadIdx.x; iAccum < stride; iAccum += blockDim.x)
                accumResult[iAccum] += accumResult[stride + iAccum];
        }

        if (threadIdx.x == 0) d_C[vec] = accumResult[0];
    }
}