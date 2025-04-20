/**********************************************************************
Copyright ©2015 Advanced Micro Devices, Inc. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or
 other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
********************************************************************/


#include <hip/hip_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "../include/HIPUtil.hpp"

#define SAMPLE_VERSION "HIP-Examples-Application-v1.0"

using namespace appsdk;

/**
 * MatrixMultiplication
 * Class implements HIP Matrix Multiplication sample
 */

class MatrixMultiplication
{
        unsigned int
        seed;                  /**< Seed value for random number generation */
        double
        setupTime;                  /**< Time for setting up OpenCL */
        double
        appTime;                  /**< Time for transfer + kernel execution */
        double
        kernelTime;                  /**< Time for kernel execution */
        float              *input0;                  /**< Input array */
        int                 width0;                  /**< width of input Array */
        int                height0;                  /**< height of input Array */
        float              *input1;                  /**< Input array */
        int                 width1;                  /**< width of Input Array */
        int                height1;                  /**< height of Input Array */
        float              *output;                  /**< Output Array */
        float  *verificationOutput;                  /**< Output array for reference implementation */
         unsigned int
        blockSize;                  /**< Size of the block used for shared memory */

        float*
        inputBuffer0;                  /**<  memory buffer  for matrix input0*/
        float*
        inputBuffer1;                  /**<  memory buffer  for matrix input1*/
        float*
        outputBuffer;                  /**<  memory buffer  for storing the output*/

        bool
        lds;                  /**< Local data store availability */

        int
        n;                  /**< n height of matrix A and width of matrixB */
        int                      m;                  /**< m width of matrix A */
        int                      k;                  /**< k height of matrix B */
        size_t       globalThreads[2];                  /**< global NDRange */
        size_t        localThreads[2];                  /**< local Threads */
        unsigned long availableLocalMemory;                  /**< Total Local Memory available for the kernel */
         unsigned long
        neededLocalMemory;                  /**< Total Local Memory needed by the kernel */
        int
        iterations;                  /**< Number of iterations for kernel execution */

        bool eAppGFLOPS;

        SDKTimer *sampleTimer;      /**< SDKTimer object */

    public:

        HIPCommandArgs   *sampleArgs;   /**< CLCommand argument class */

        /**
         * Constructor
         * Initialize member variables
         */
        MatrixMultiplication()
        {
            sampleArgs = new HIPCommandArgs();
            sampleTimer = new SDKTimer();
            sampleArgs->sampleVerStr = SAMPLE_VERSION;
            seed   = 123;
            input0 = NULL;
            input1 = NULL;
            output = NULL;
            verificationOutput = NULL;
            n = 64;
            m = 64;
            k = 64;
            blockSize = 8;
            setupTime = 0;
            appTime = 0;
            iterations = 1;
            lds = 0;
            eAppGFLOPS = false;
        }

        /**
         * Allocate and initialize host memory array with random values
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */
        int setupMatrixMultiplication();

        /**
         * Function to compute local WorkGroup Size based on inputs
         * and device properties
         */
        int setWorkGroupSize();

        /**
         * Override from SDKSample, Generate binary image of given kernel
         * and exit application
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */

        int setupHIP();

        /**
         * Set values for kernels' arguments, enqueue calls to the kernels
         * on to the command queue, wait till end of kernel execution.
         * Get kernel start and end time if timing is enabled
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */
        int runKernels();

        /**
         * Reference CPU implementation of Matrix Multiplication
         * @param output stores the output of the multiplied matrices depthxheight
         * @param input0 input matrix of size width x height
         * @param input1 input matrix of size depth x width
         * @param height height of the output matrix
         * @param width  length of the common dimension of the matrices input0 and input1
         * @param depth  width  of the output matrix
         */
        void matrixMultiplicationCPUReference(
            float * output,
            float * input0,
            float * input1,
            const  unsigned int height,
            const  unsigned int width,
            const  unsigned int depth);

        /**
         * Override from SDKSample. Print sample stats.
         */
        void printStats();

        /**
         * Override from SDKSample. Initialize
         * command line parser, add custom options
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */
        int initialize();

        /**
         * Override from SDKSample, adjust width and height
         * of execution domain, perform all sample setup
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */
        int setup();

        /**
         * Override from SDKSample
         * Run OpenCL Matrix Multiplication
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */
        int run();

        /**
         * Override from SDKSample
         * Cleanup memory allocations
         * @return SDK_SUCCESS on success and SDK_FAILURE on failure
         */
        int cleanup();

        int verifyResults();
};

#define TILEX 4
#define TILEX_SHIFT 2
#define TILEY 4
#define TILEY_SHIFT 2

/* Output tile size : 4x4 = Each thread computes 16 float values*/
/* Required global threads = (widthC / 4, heightC / 4) */
/* This kernel runs on 7xx and CPU as they don't have hardware local memory */
__global__ void mmmKernel(
			 float4 *matrixA,
                         float4 *matrixB,
                         float4* matrixC,
            uint widthA, uint widthB)
{

    int2 pos = make_int2(hipThreadIdx_x+hipBlockDim_x*hipBlockIdx_x, hipThreadIdx_y+hipBlockDim_y*hipBlockIdx_y);


    float4 sum0 = make_float4(0,0,0,0);
    float4 sum1 = make_float4(0,0,0,0);
    float4 sum2 = make_float4(0,0,0,0);
    float4 sum3 = make_float4(0,0,0,0);

    /* Vectorization of input Matrices reduces their width by a factor of 4 */
    widthB /= 4;

    for(int i = 0; i < widthA; i=i+4)
    {
        float4 tempA0 = matrixA[i/4 + (pos.y << TILEY_SHIFT) * (widthA / 4)];
        float4 tempA1 = matrixA[i/4 + ((pos.y << TILEY_SHIFT) + 1) * (widthA / 4)];
        float4 tempA2 = matrixA[i/4 + ((pos.y << TILEY_SHIFT) + 2) * (widthA / 4)];
        float4 tempA3 = matrixA[i/4 + ((pos.y << TILEY_SHIFT) + 3) * (widthA / 4)];

        //Matrix B is not transposed
        float4 tempB0 = matrixB[pos.x + i * widthB];
        float4 tempB1 = matrixB[pos.x + (i + 1) * widthB];
        float4 tempB2 = matrixB[pos.x + (i + 2) * widthB];
        float4 tempB3 = matrixB[pos.x + (i + 3) * widthB];

        sum0.x += tempA0.x * tempB0.x + tempA0.y * tempB1.x + tempA0.z * tempB2.x + tempA0.w * tempB3.x;
        sum0.y += tempA0.x * tempB0.y + tempA0.y * tempB1.y + tempA0.z * tempB2.y + tempA0.w * tempB3.y;
        sum0.z += tempA0.x * tempB0.z + tempA0.y * tempB1.z + tempA0.z * tempB2.z + tempA0.w * tempB3.z;
        sum0.w += tempA0.x * tempB0.w + tempA0.y * tempB1.w + tempA0.z * tempB2.w + tempA0.w * tempB3.w;

        sum1.x += tempA1.x * tempB0.x + tempA1.y * tempB1.x + tempA1.z * tempB2.x + tempA1.w * tempB3.x;
        sum1.y += tempA1.x * tempB0.y + tempA1.y * tempB1.y + tempA1.z * tempB2.y + tempA1.w * tempB3.y;
        sum1.z += tempA1.x * tempB0.z + tempA1.y * tempB1.z + tempA1.z * tempB2.z + tempA1.w * tempB3.z;
        sum1.w += tempA1.x * tempB0.w + tempA1.y * tempB1.w + tempA1.z * tempB2.w + tempA1.w * tempB3.w;

        sum2.x += tempA2.x * tempB0.x + tempA2.y * tempB1.x + tempA2.z * tempB2.x + tempA2.w * tempB3.x;
        sum2.y += tempA2.x * tempB0.y + tempA2.y * tempB1.y + tempA2.z * tempB2.y + tempA2.w * tempB3.y;
        sum2.z += tempA2.x * tempB0.z + tempA2.y * tempB1.z + tempA2.z * tempB2.z + tempA2.w * tempB3.z;
        sum2.w += tempA2.x * tempB0.w + tempA2.y * tempB1.w + tempA2.z * tempB2.w + tempA2.w * tempB3.w;

        sum3.x += tempA3.x * tempB0.x + tempA3.y * tempB1.x + tempA3.z * tempB2.x + tempA3.w * tempB3.x;
        sum3.y += tempA3.x * tempB0.y + tempA3.y * tempB1.y + tempA3.z * tempB2.y + tempA3.w * tempB3.y;
        sum3.z += tempA3.x * tempB0.z + tempA3.y * tempB1.z + tempA3.z * tempB2.z + tempA3.w * tempB3.z;
        sum3.w += tempA3.x * tempB0.w + tempA3.y * tempB1.w + tempA3.z * tempB2.w + tempA3.w * tempB3.w;
    }
    matrixC[pos.x + ((pos.y <<  TILEY_SHIFT) + 0) * widthB] = sum0;
    matrixC[pos.x + ((pos.y <<  TILEY_SHIFT) + 1) * widthB] = sum1;
    matrixC[pos.x + ((pos.y <<  TILEY_SHIFT) + 2) * widthB] = sum2;
    matrixC[pos.x + ((pos.y <<  TILEY_SHIFT) + 3) * widthB] = sum3;

}

int
MatrixMultiplication::setupMatrixMultiplication()
{
    // allocate and init memory used by host  input0[width0][height0]
    unsigned int inputSizeBytes0 = width0 * height0 * sizeof(float);

    input0 = (float *) malloc(inputSizeBytes0);
    CHECK_ALLOCATION(input0, "Failed to allocate host memory. (input0)");

    // allocate and init memory used by host input1[width1][height1]
    unsigned int inputSizeBytes1 = width1 * height1 * sizeof(float);

    input1 = (float *) malloc(inputSizeBytes1);
    CHECK_ALLOCATION(input1, "Failed to allocate host memory. (input1)");

    // random initialisation of input
    fillRandom<float>(input0, width0, height0, 0, 10);
    fillRandom<float>(input1, width1, height1, 0, 10);

    // allocate memory for output[width1][height0]
    unsigned int outputSizeBytes = height0 * width1 * sizeof(float);

    output = (float *) malloc(outputSizeBytes);
    CHECK_ALLOCATION(output, "Failed to allocate host memory. (output)");

    // allocate memory for output[width1][height0] of reference implementation
    if(sampleArgs->verify)
    {
        verificationOutput = (float *) malloc(outputSizeBytes);
        CHECK_ALLOCATION(verificationOutput,
                         "Failed to allocate host memory. (verificationOutput)");
        memset(verificationOutput, 0, outputSizeBytes);
    }

    // Unless quiet mode has been enabled, print the INPUT arrays
    if(!sampleArgs->quiet)
    {
        printArray<float>(
            "Input0",
            input0,
            width0,
            1);
        printArray<float>(
            "Input1",
            input1,
            width1,
            1);
    }

    return SDK_SUCCESS;
}




int
MatrixMultiplication::setupHIP(void)
{

    return SDK_SUCCESS;
}



int
MatrixMultiplication::runKernels(void)
{
float *din0, *din1, *dout;
    // Set input data to matrix A and matrix B
hipHostMalloc((void**)&inputBuffer0, width0 * height0 * sizeof(float), hipHostMallocDefault);
hipHostMalloc((void**)&inputBuffer1, width1 * height1 * sizeof(float), hipHostMallocDefault);
hipHostMalloc((void**)&outputBuffer, width1 * height0 * sizeof(float), hipHostMallocDefault);

hipHostGetDevicePointer((void**)&din0, inputBuffer0,0);
hipHostGetDevicePointer((void**)&din1, inputBuffer1,0);
hipHostGetDevicePointer((void**)&dout, outputBuffer,0);

hipMemcpy(din0, input0,width0 * height0 * sizeof(float), hipMemcpyHostToDevice);
hipMemcpy(din1, input1,width1 * height1 * sizeof(float), hipMemcpyHostToDevice);


hipLaunchKernelGGL(mmmKernel,
                  dim3((width1/4)/blockSize,(height0/4)/blockSize),
                  dim3(blockSize,blockSize),
                  0, 0,
                  (float4*)inputBuffer0 ,(float4*)inputBuffer1 ,(float4*)outputBuffer, width0, width1 );

hipMemcpy(output, dout, width1 * height0 * sizeof(float), hipMemcpyDeviceToHost);

    return SDK_SUCCESS;
}


/*
 * This is a naive O(N^3) CPU implementation of matrix multiplication
 */
void
MatrixMultiplication::matrixMultiplicationCPUReference(
    float * output,
    float * input0,
    float * input1,
    const unsigned int y,
    const unsigned int x,
    const unsigned int z)
{
    for(unsigned int i = 0; i < y; i++)
    {
        for(unsigned int j = 0; j < z; j++)
        {
            for(unsigned int k = 0; k < x; k++)
            {
                output[i * z + j] += (input0[i * x + k] * input1[k * z + j]);
            }
        }
    }
}

int
MatrixMultiplication::initialize()
{
    // Call base class Initialize to get default configuration
    if(sampleArgs->initialize() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    // add an option for getting blockSize from commandline
    Option* xParam = new Option;
    CHECK_ALLOCATION(xParam, "Memory Allocation error.\n");
    xParam->_sVersion = "x";
    xParam->_lVersion = "height0";
    xParam->_description = "height of matrix A";
    xParam->_type     = CA_ARG_INT;
    xParam->_value    = &n;
    sampleArgs->AddOption(xParam);
    delete xParam;

    Option* yParam = new Option;
    CHECK_ALLOCATION(yParam, "Memory Allocation error.\n");
    yParam->_sVersion = "y";
    yParam->_lVersion = "width0";
    yParam->_description = "width of matrix A and Height of matrix B";
    yParam->_type     = CA_ARG_INT;
    yParam->_value    = &m;
    sampleArgs->AddOption(yParam);
    delete yParam;

    Option* zParam = new Option;
    CHECK_ALLOCATION(zParam, "Memory Allocation error.\n");
    zParam->_sVersion = "z";
    zParam->_lVersion = "width1";
    zParam->_description = "width of matrix B";
    zParam->_type     = CA_ARG_INT;
    zParam->_value    = &k;
    sampleArgs->AddOption(zParam);
    delete zParam;

    Option* blockSizeParam = new Option;
    CHECK_ALLOCATION(blockSizeParam, "Memory Allocation error.\n");
    blockSizeParam->_sVersion = "b";
    blockSizeParam->_lVersion = "blockSize";
    blockSizeParam->_description =
        "Use local memory of dimensions blockSize x blockSize";
    blockSizeParam->_type     = CA_ARG_INT;
    blockSizeParam->_value    = &blockSize;
    sampleArgs->AddOption(blockSizeParam);
    delete blockSizeParam;

    Option* num_iterations = new Option;
    CHECK_ALLOCATION(num_iterations, "Memory Allocation error.\n");
    num_iterations->_sVersion = "i";
    num_iterations->_lVersion = "iterations";
    num_iterations->_description = "Number of iterations for kernel execution";
    num_iterations->_type = CA_ARG_INT;
    num_iterations->_value = &iterations;
    sampleArgs->AddOption(num_iterations);
    delete num_iterations;

    Option* appGflops_option = new Option;
    CHECK_ALLOCATION(appGflops_option, "Memory Allocation error.\n");
    appGflops_option->_sVersion = "";
    appGflops_option->_lVersion = "eAppGflops";
    appGflops_option->_description =
        "Prints GFLOPS calculated from transfer + kernel time";
    appGflops_option->_type = CA_NO_ARGUMENT;
    appGflops_option->_value = &eAppGFLOPS;
    sampleArgs->AddOption(appGflops_option);
    delete appGflops_option;

    return SDK_SUCCESS;
}

int
MatrixMultiplication::setup()
{
    // Validation of input values
    if((n == 0) || (m == 0) || (k == 0))
    {
        std::cout << "Error: Matrix dimensions can not be 0" << std::endl;
        return SDK_FAILURE;
    }

    // Make sure the dimensions are multiples of blockSize
    const int vectorSize = 4;
    if(n % (blockSize * vectorSize) != 0)
    {
        n = (n / (blockSize * vectorSize) + 1) * (blockSize * vectorSize);
    }

    if(m % (blockSize * vectorSize) != 0)
    {
        m = (m / (blockSize * vectorSize) + 1) * (blockSize * vectorSize);
    }

    if(k % (blockSize * vectorSize) != 0)
    {
        k = (k / (blockSize * vectorSize) + 1) * (blockSize * vectorSize);
    }

    width0  = m;
    height0 = n;

    width1  = k;
    height1 = m;

    if(setupMatrixMultiplication()!=SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    int timer = sampleTimer->createTimer();
    sampleTimer->resetTimer(timer);
    sampleTimer->startTimer(timer);

    if(setupHIP()!=SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    sampleTimer->stopTimer(timer);

    setupTime = (double)sampleTimer->readTimer(timer);

    return SDK_SUCCESS;
}


int
MatrixMultiplication::run()
{
    // Warm up
    for(int i = 0; i < 2 && iterations != 1; i++)
    {
        // Arguments are set and execution call is enqueued on command buffer
        if(runKernels() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }
    }

    int timer = sampleTimer->createTimer();
    sampleTimer->resetTimer(timer);
    sampleTimer->startTimer(timer);

    std::cout << "Executing kernel for " << iterations << " iterations" <<
              std::endl;
    std::cout << "-------------------------------------------" << std::endl;

    kernelTime = 0;
    for(int i = 0; i < iterations; i++)
    {
        // Arguments are set and execution call is enqueued on command buffer
        int kernelRun = runKernels();
        if(kernelRun != SDK_SUCCESS)
        {
            return kernelRun;
        }
    }

    sampleTimer->stopTimer(timer);
    appTime = (double)(sampleTimer->readTimer(timer)) / iterations;
    kernelTime = kernelTime / iterations;

    if(!sampleArgs->quiet)
    {
        printArray<float>("Output", output, width1, 1);
    }

    return SDK_SUCCESS;
}

int
MatrixMultiplication::verifyResults()
{
    if(sampleArgs->verify)
    {
        // reference implementation
        matrixMultiplicationCPUReference(verificationOutput, input0, input1, height0,
                                         width0,  width1);

        // compare the results and see if they match
        if(compare(output, verificationOutput, height0*width1))
        {
            std::cout<<"Passed!\n" << std::endl;
            return SDK_SUCCESS;
        }
        else
        {
            std::cout<<"Failed\n" << std::endl;
            return SDK_FAILURE;
        }
    }

    return SDK_SUCCESS;
}

void
MatrixMultiplication::printStats()
{
    if(sampleArgs->timing)
    {
        if(eAppGFLOPS)
        {
            std::string strArray[4] = {"MatrixA", "MatrixB", "Time(sec)", "[Transfer+kernel]Time(sec)"};
            std::string stats[4];

            double flops = 2 * width0 * width1;
            double perf = (flops / appTime) * height0 * 1e-9;
            if(sampleArgs->timing)
            {
                std::cout << "GFlops achieved : " << perf << std::endl << std::endl;
            }

            sampleTimer->totalTime = setupTime + appTime;

            stats[0]  = toString(height0, std::dec)
                        +"x"+toString(width0, std::dec);
            stats[1]  = toString(height1, std::dec)
                        +"x"+toString(width1, std::dec);
            stats[2]  = toString(sampleTimer->totalTime, std::dec);
            stats[3]  = toString(appTime, std::dec);

            printStatistics(strArray, stats, 4);
        }
        else
        {
            std::string strArray[4] = {"MatrixA", "MatrixB", "Time(sec)", "kernelTime(sec)"};
            std::string stats[4];

            double flops = 2 * width0 * width1;
            double perf = (flops / kernelTime) * height0 * 1e-9;
            if(sampleArgs->timing)
            {
                std::cout << "GFlops achieved : " << perf << std::endl << std::endl;
            }

            sampleTimer->totalTime = setupTime + kernelTime;

            stats[0]  = toString(height0, std::dec)
                        +"x"+toString(width0, std::dec);
            stats[1]  = toString(height1, std::dec)
                        +"x"+toString(width1, std::dec);
            stats[2]  = toString(sampleTimer->totalTime, std::dec);
            stats[3]  = toString(kernelTime, std::dec);

            printStatistics(strArray, stats, 4);
        }
    }
}

int
MatrixMultiplication::cleanup()
{

    hipFree(inputBuffer0);

    hipFree(inputBuffer1);

    hipFree(outputBuffer);

    FREE(input0);
    FREE(input1);
    FREE(output);
    FREE(verificationOutput);

    return SDK_SUCCESS;
}

int
main(int argc, char * argv[])
{
    MatrixMultiplication hipMatrixMultiplication;

    if(hipMatrixMultiplication.initialize() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    if(hipMatrixMultiplication.sampleArgs->parseCommandLine(argc,
            argv) != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    {
        // Setup
        if(hipMatrixMultiplication.setup() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }
        // Run
        if(hipMatrixMultiplication.run() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }

        // VerifyResults
        if(hipMatrixMultiplication.verifyResults() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }

        // Cleanup
        if(hipMatrixMultiplication.cleanup() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }

        hipMatrixMultiplication.printStats();
    }

    return SDK_SUCCESS;
}