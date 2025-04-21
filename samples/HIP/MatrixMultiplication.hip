// MIT License
//
// Original source:
//   https://github.com/ROCm/rocm-examples
//   Copyright (c) 2022-2023 Advanced Micro Devices, Inc.
//
// Modified and reduced for Linguist sample use by Young-TW
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <hip/hip_runtime.h>
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

constexpr unsigned int BlockSize = 16;

__global__ void matrix_multiplication_kernel(const float* A,
                                             const float* B,
                                             float* C,
                                             const unsigned int a_cols) {
    unsigned int tx = threadIdx.x;
    unsigned int ty = threadIdx.y;
    unsigned int bx = blockIdx.x;
    unsigned int by = blockIdx.y;
    unsigned int b_cols = blockDim.x * gridDim.x;
    unsigned int steps = a_cols / BlockSize;
    float thread_result = 0.0F;

    for (unsigned int step = 0; step < steps; ++step)
    {
        __shared__ float a_values[BlockSize][BlockSize];
        __shared__ float b_values[BlockSize][BlockSize];

        unsigned int a_idx = BlockSize * (a_cols * by + step);
        unsigned int b_idx = BlockSize * (b_cols * step + bx);

        a_values[ty][tx] = A[a_idx + a_cols * ty + tx];
        b_values[ty][tx] = B[b_idx + b_cols * ty + tx];

        __syncthreads();

        for (unsigned int i = 0; i < BlockSize; ++i)
        {
            thread_result += a_values[ty][i] * b_values[i][tx];
        }

        __syncthreads();
    }

    unsigned int block_offset = b_cols * BlockSize * by + BlockSize * bx;
    C[block_offset + b_cols * ty + tx] = thread_result;
}

int main() {
    constexpr unsigned int a_rows = 256, a_cols = 128, b_cols = 128;
    constexpr unsigned int b_rows = a_cols;
    constexpr unsigned int c_rows = a_rows, c_cols = b_cols;
    const size_t a_bytes = sizeof(float) * a_rows * a_cols;
    const size_t b_bytes = sizeof(float) * b_rows * b_cols;
    const size_t c_bytes = sizeof(float) * c_rows * c_cols;

    std::vector<float> A(a_rows * a_cols, 1.0f);
    std::vector<float> B(b_rows * b_cols, 0.5f);
    std::vector<float> C(c_rows * c_cols);

    float *d_A, *d_B, *d_C;
    hipMalloc(&d_A, a_bytes);
    hipMalloc(&d_B, b_bytes);
    hipMalloc(&d_C, c_bytes);
    hipMemcpy(d_A, A.data(), a_bytes, hipMemcpyHostToDevice);
    hipMemcpy(d_B, B.data(), b_bytes, hipMemcpyHostToDevice);

    dim3 block_dim(BlockSize, BlockSize);
    dim3 grid_dim(c_cols / BlockSize, c_rows / BlockSize);
    matrix_multiplication_kernel<<<grid_dim, block_dim>>>(d_A, d_B, d_C, a_cols);
    hipDeviceSynchronize();

    hipMemcpy(C.data(), d_C, c_bytes, hipMemcpyDeviceToHost);

    hipFree(d_A); hipFree(d_B); hipFree(d_C);

    if (std::abs(C[0] - a_cols * 0.5f) < 1e-3f) {
        std::cout << "Validation passed.\n";
    } else {
        std::cout << "Validation failed.\n";
    }
}
