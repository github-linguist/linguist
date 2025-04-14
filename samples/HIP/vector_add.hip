#include <hip/hip_runtime.h>

__global__ void vector_add(const float* A, const float* B, float* C, int N) {
    int i = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
    if (i < N) {
        C[i] = A[i] + B[i];
    }
}
