/*
Copyright (c) 2015-2016 Advanced Micro Devices, Inc. All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

#include <hip/hip_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "../include/HIPUtil.hpp"

using namespace appsdk;
using namespace std;



#define SAMPLE_VERSION "HIP-Examples-Application-v1.0"

/**
 * PrefixSum
 * Class implements Prefix Sum sample
 */

class PrefixSum
{
        unsigned int
        seed;      /**< Seed value for random number generation */
        double           setupTime;      /**< Time for setting up Open*/
        double          kernelTime;      /**< Time for kernel execution */
        unsigned int                 length;     /**< length of the input array */
        float               *input;      /**< Input array */
        float  *verificationOutput;      /**< Output array for reference implementation */
        float*            inputBuffer;      /**< memory buffer */
        float*           outputBuffer;      /**< memory output Buffer */
	float*               out;
        int
        iterations;      /**< Number of iterations for kernel execution */
        SDKTimer *sampleTimer;      /**< SDKTimer object */
	  float *din, *dout;
    public:

        HIPCommandArgs   *sampleArgs;   /**< HIPCommand argument class */

        /**
        *******************************************************************************
        * @fn Constructor
        * @brief Initialize member variables
        *
        *******************************************************************************
        */
        PrefixSum()
            : seed(123),
              setupTime(0),
              kernelTime(0),
              length(64),
              input(NULL),
              verificationOutput(NULL),
              iterations(1)
        {
            sampleArgs = new HIPCommandArgs();
            sampleTimer = new SDKTimer();
            sampleArgs->sampleVerStr = SAMPLE_VERSION;
        }

        /**
        *******************************************************************************
        * @fn Destructor
        * @brief Cleanup the member objects.
        *******************************************************************************
        */
        ~PrefixSum()
        {
            // release program resources
            FREE(input);
            FREE(verificationOutput);
        }

        /**
        *******************************************************************************
        * @fn setupPrefixSum
        * @brief Allocate and initialize host memory array with random values
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int setupPrefixSum();

        /**
        *******************************************************************************
        * @fn setup
        * @brief Openrelated initialisations. Set up Context, Device list,
        *        Command Queue, Memory buffers Build kernel program executable.
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int setupHIP();

        /**
        *******************************************************************************
        * @fn runCLKernels
        * @brief Set values for kernels' arguments, enqueue calls to the kernels
        *        on to the command queue, wait till end of kernel execution.
        *        Get kernel start and end time if timing is enabled.
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int runKernels();

        /**
        *******************************************************************************
        * @fn prefixSumCPUReference
        * @brief Reference CPU implementation of Prefix Sum.
        *
        * @param output the array that stores the prefix sum
        * @param input the input array
        * @param length length of the input array
        *******************************************************************************
        */
        void prefixSumCPUReference(float * output,
                                   float * input,
                                   const unsigned int length);

        /**
        *******************************************************************************
        * @fn printStats
        * @brief Override from SDKSample. Print sample stats.
        *******************************************************************************
        */
        void printStats();

        /**
        *******************************************************************************
        * @fn initialize
        * @brief Override from SDKSample. Initialize command line parser, add custom options.
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int initialize();


        /**
        *******************************************************************************
        * @fn setup
        * @brief Override from SDKSample, adjust width and height
        *        of execution domain, perform all sample setup
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int setup();

        /**
        *******************************************************************************
        * @fn run
        * @brief Run OpenFastWalsh Transform. Override from SDKSample.
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int run();

        /**
        *******************************************************************************
        * @fn cleanup
        * @brief Cleanup memory allocations. Override from SDKSample.
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int cleanup();

        /**
        *******************************************************************************
        * @fn verifyResults
        * @brief Verify against reference implementation
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int verifyResults();


        /**
        *******************************************************************************
        * @fn runGroupKernel
        * @brief Run group prefixsum kernel. The kernel make prefix sum on individual work groups.
        *
        * @param[in] offset : Distance between two consecutive index.
        *
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int runGroupKernel(unsigned int offset);

        /**
        *******************************************************************************
        * @fn runGlobalKernel
        * @brief Run global prefixsum kernel. The kernel updates all elements.
        *
        * @param[in] offset : Distance between two consecutive index.
        *
        * @return SDK_SUCCESS on success and SDK_FAILURE on failure
        *******************************************************************************
        */
        int runGlobalKernel(unsigned int offset);
};

__global__
void group_prefixSum( float * output,
		      float * input,
		      const unsigned int length,
		      const unsigned int idxOffset) {
	int localId = hipThreadIdx_x;
	int localSize = hipBlockDim_x;
	int globalIdx = hipBlockIdx_x;
	__shared__  float  block[64];

	// Cache the computational window in shared memory
	globalIdx = (idxOffset *(2 *(globalIdx*localSize + localId) +1)) - 1;
	if(globalIdx < length)             { block[2*localId]     = input[globalIdx];				}
    if(globalIdx + idxOffset < length) { block[2*localId + 1] = input[globalIdx + idxOffset];	}

	// Build up tree
	int offset = 1;
	for(int l = length>>1; l > 0; l >>= 1)
	{
	  __syncthreads();
	  if(localId < l) {
            int ai = offset*(2*localId + 1) - 1;
            int bi = offset*(2*localId + 2) - 1;
            block[bi] += block[ai];
         }
         offset <<= 1;
	}

	if (length > 2)
	{
		if(offset < length) { offset <<= 1; }

		// Build down tree
		int maxThread = offset>>1;
		for(int d = 0; d < maxThread; d<<=1)
		{
			d += 1;
			offset >>=1;
	                __syncthreads();

			if(localId < d) {
				int ai = offset*(localId + 1) - 1;
				int bi = ai + (offset>>1);
				block[bi] += block[ai];
			}
		}
	}
	  __syncthreads();

    // write the results back to global memory
    if(globalIdx < length)           { output[globalIdx]             = block[2*localId];		}
    if(globalIdx+idxOffset < length) { output[globalIdx + idxOffset] = block[2*localId + 1];	}
}

/*
 * Work-efficient compute implementation of scan, one thread per 2 elements
 * O(log(n)) stepas and O(n) adds using shared memory
 * Uses a balanced tree algorithm. See Belloch, 1990 "Prefix Sums
 * and Their Applications"
 * @param buffer	input/output data
 * @param offset	Multiple of Offset positions are already updated by group_prefixSum kernel
 * @param length	lenght of the input data
 */
__global__
void global_prefixSum( 
		       float * buffer,
                       unsigned int offset,
		       unsigned int length) {
	int localSize = hipBlockDim_x;
    int groupIdx  = hipBlockIdx_x;

	int sortedLocalBlocks = offset / localSize;		// sorted groups per block
	// Map the gids to unsorted local blocks.
	int gidToUnsortedBlocks = groupIdx + (groupIdx / ((offset<<1) - sortedLocalBlocks) +1) * sortedLocalBlocks;

	// Get the corresponding global index
    int globalIdx = (gidToUnsortedBlocks*localSize + hipThreadIdx_x);
	if(((globalIdx+1) % offset != 0) && (globalIdx < length))
		buffer[globalIdx] += buffer[globalIdx - (globalIdx%offset + 1)];
}


int PrefixSum::setupPrefixSum()
{
    if(length < 2)
    {
        length = 2;
    }
    // allocate and init memory used by host
    unsigned int sizeBytes = length * sizeof(float);

    input = (float *) malloc(sizeBytes);
    CHECK_ALLOCATION(input, "Failed to allocate host memory. (input)");
    // random initialisation of input
    fillRandom<float>(input, length, 1, 0, 10);
    out = (float *) malloc(sizeBytes);

    if(sampleArgs->verify)
    {
        verificationOutput = (float *) malloc(sizeBytes);
        CHECK_ALLOCATION(verificationOutput,
                         "Failed to allocate host memory. (verificationOutput)");
        memset(verificationOutput, 0, sizeBytes);
    }
    // Unless quiet mode has been enabled, print the INPUT array
    if(!sampleArgs->quiet)
    {
        printArray<float>("Input : ", input, length, 1);
    }

    return SDK_SUCCESS;
}

int
PrefixSum::setupHIP(void)
{

    return SDK_SUCCESS;
}

int
PrefixSum::runGroupKernel(unsigned int offset)
{

    float* buf;
    unsigned int dataSize = length/offset;
    unsigned int localThreads = 128;
    unsigned int globalThreads = (dataSize+1) / 2;    // Actual threads needed
    // Set global thread size multiple of local thread size.
    globalThreads = ((globalThreads + localThreads - 1) / localThreads) *
                    localThreads;

buf=(offset>1) ? outputBuffer  : inputBuffer;
hipLaunchKernelGGL(group_prefixSum,
                  dim3(globalThreads/localThreads),
                  dim3(localThreads),
                  0, 0,
                  outputBuffer ,buf,length,offset);


    return SDK_SUCCESS;
}

int
PrefixSum::runGlobalKernel(unsigned int offset)
{
    unsigned int localThreads = 128;
    unsigned int localDataSize = localThreads << 1;   // Each thread work on 2 elements

    // Set number of threads needed for global_kernel.
    unsigned int globalThreads = length - offset;
    globalThreads -= (globalThreads / (offset * localDataSize)) * offset;

    // Set global thread size multiple of local thread size.
    globalThreads = ((globalThreads + localThreads - 1) / localThreads) *
                    localThreads;

hipLaunchKernelGGL(global_prefixSum,
                  dim3(globalThreads/localThreads),
                  dim3(localThreads),
                  0, 0,
                  outputBuffer ,offset, length);

    return SDK_SUCCESS;
}

int
PrefixSum::runKernels(void)
{
    hipMemcpy(din, input, sizeof(float) * length, hipMemcpyHostToDevice);
    unsigned int localThreads = 128;
    unsigned int localDataSize = localThreads << 1;   // Each thread work on 2 elements


    for(unsigned int offset=1; offset<length; offset *= localDataSize)
    {
        if ((length/offset) > 1)  // Need atlest 2 element for process the kernel
        {
            if(runGroupKernel(offset) != SDK_SUCCESS)
            {
                return SDK_FAILURE;
            }
        }

        // Call global_kernel for update all elements
        if(offset > 1)
        {
            if(runGlobalKernel(offset) != SDK_SUCCESS)
            {
                return SDK_FAILURE;
            }
        }
    }

    return SDK_SUCCESS;
}

void
PrefixSum::prefixSumCPUReference(
    float * output,
    float * input,
    const unsigned int length)
{
    output[0] = input[0];

    for(unsigned int i = 1; i< length; ++i)
    {
        output[i] = input[i] + output[i-1];
    }
}

int PrefixSum::initialize()
{
    // Call base class Initialize to get default configuration
    if(sampleArgs->initialize() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    Option* array_length = new Option;
    CHECK_ALLOCATION(array_length, "Memory allocation error. (array_length)");

    array_length->_sVersion = "x";
    array_length->_lVersion = "length";
    array_length->_description = "Length of the input array";
    array_length->_type = CA_ARG_INT;
    array_length->_value = &length;
    sampleArgs->AddOption(array_length);
    delete array_length;

    Option* num_iterations = new Option;
    CHECK_ALLOCATION(num_iterations, "Memory allocation error. (num_iterations)");

    num_iterations->_sVersion = "i";
    num_iterations->_lVersion = "iterations";
    num_iterations->_description = "Number of iterations for kernel execution";
    num_iterations->_type = CA_ARG_INT;
    num_iterations->_value = &iterations;

    sampleArgs->AddOption(num_iterations);
    delete num_iterations;

    return SDK_SUCCESS;
}

int PrefixSum::setup()
{
    if(!isPowerOf2(length))
    {
        length = roundToPowerOf2(length);
    }
    if(setupPrefixSum() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    int timer = sampleTimer->createTimer();
    sampleTimer->resetTimer(timer);
    sampleTimer->startTimer(timer);

    if (setupHIP() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    sampleTimer->stopTimer(timer);
    setupTime = (double)sampleTimer->readTimer(timer);
    return SDK_SUCCESS;
}


int PrefixSum::run()
{
  hipHostMalloc((void**)&inputBuffer, sizeof(float) * length,hipHostMallocDefault);
  hipHostMalloc((void**)&outputBuffer, sizeof(float) * length,hipHostMallocDefault);

  hipHostGetDevicePointer((void**)&din, inputBuffer,0);
  hipHostGetDevicePointer((void**)&dout, outputBuffer,0);


    // Warm up
    for(int i = 0; i < 2 && iterations != 1; i++)
    {
        // Arguments are set and execution call is enqueued on command buffer
        if(runKernels() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }
    }

    std::cout << "Executing kernel for " << iterations
              << " iterations" << std::endl;
    std::cout << "-------------------------------------------" << std::endl;

    int timer = sampleTimer->createTimer();
    sampleTimer->resetTimer(timer);
    sampleTimer->startTimer(timer);

    for(int i = 0; i < iterations; i++)
    {
        // Arguments are set and execution call is enqueued on command buffer
        if(runKernels() != SDK_SUCCESS)
        {
            return SDK_FAILURE;
        }
    }
      hipMemcpy(out, dout, sizeof(float) * length, hipMemcpyDeviceToHost);
      printArray<float>("Output : ", out, length, 1);

    sampleTimer->stopTimer(timer);
    kernelTime = (double)(sampleTimer->readTimer(timer));

    return SDK_SUCCESS;
}

int PrefixSum::verifyResults()
{
    int status = SDK_SUCCESS;
    if(sampleArgs->verify)
    {
        // Read the device output buffer


        // reference implementation
        prefixSumCPUReference(verificationOutput, input, length);

        // compare the results and see if they match
        float epsilon = length * 1e-7f;
        if(compare(out, verificationOutput, length, epsilon))
        {
            std::cout << "Passed!\n" << std::endl;
            status = SDK_SUCCESS;
        }
        else
        {
            std::cout << "Failed\n" << std::endl;
            status = SDK_FAILURE;
        }

        if(!sampleArgs->quiet)
        {
            printArray<float>("Output : ", out, length, 1);
        }

    }
    return status;
}

void PrefixSum::printStats()
{
    if(sampleArgs->timing)
    {
        std::string strArray[4] =
        {
            "Samples",
            "Setup Time(sec)",
            "Avg. kernel time (sec)",
            "Samples used /sec"
        };
        std::string stats[4];
        double avgKernelTime = kernelTime / iterations;

        stats[0] = toString(length, std::dec);
        stats[1] = toString(setupTime, std::dec);
        stats[2] = toString(avgKernelTime, std::dec);
        stats[3] = toString((length/avgKernelTime), std::dec);

        printStatistics(strArray, stats, 4);
    }
}

int PrefixSum::cleanup()
{
    hipFree(inputBuffer);
    hipFree(outputBuffer);

    return SDK_SUCCESS;
}

int
main(int argc, char * argv[])
{
  hipDeviceProp_t devProp;
  hipGetDeviceProperties(&devProp, 0);
  cout << " System minor " << devProp.minor << endl;
  cout << " System major " << devProp.major << endl;
  cout << " agent prop name " << devProp.name << endl;
  cout << " totalConstMem " << devProp.totalConstMem << endl;
  cout << " sharedMemPerBlock " << devProp.sharedMemPerBlock << endl;



    PrefixSum hipPrefixSum;
    // Initialize
    if(hipPrefixSum.initialize() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    if(hipPrefixSum.sampleArgs->parseCommandLine(argc, argv) != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    // Setup
    if(hipPrefixSum.setup() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    // Run
    if(hipPrefixSum.run() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    // VerifyResults
    if(hipPrefixSum.verifyResults() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    // Cleanup
    if (hipPrefixSum.cleanup() != SDK_SUCCESS)
    {
        return SDK_FAILURE;
    }

    hipPrefixSum.printStats();
    return SDK_SUCCESS;
}
