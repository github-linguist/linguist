void foo()
{
  cudaArray* cu_array;
  texture<float, 2, cudaReadModeElementType> tex;

  // Allocate array
  cudaChannelFormatDesc description = cudaCreateChannelDesc<float>();
  cudaMallocArray(&cu_array, &description, width, height);

  // Copy image data to array
  cudaMemcpyToArray(cu_array, image, width*height*sizeof(float), cudaMemcpyHostToDevice);

  // Set texture parameters (default)
  tex.addressMode[0] = cudaAddressModeClamp;
  tex.addressMode[1] = cudaAddressModeClamp;
  tex.filterMode = cudaFilterModePoint;
  tex.normalized = false; // do not normalize coordinates

  // Bind the array to the texture
  cudaBindTextureToArray(tex, cu_array);

  // Run kernel
  dim3 blockDim(16, 16, 1);
  dim3 gridDim((width + blockDim.x - 1)/ blockDim.x, (height + blockDim.y - 1) / blockDim.y, 1);
  kernel<<< gridDim, blockDim, 0 >>>(d_data, height, width);

  // Unbind the array from the texture
  cudaUnbindTexture(tex);
} //end foo()

__global__ void kernel(float* odata, int height, int width)
{
   unsigned int x = blockIdx.x*blockDim.x + threadIdx.x;
   unsigned int y = blockIdx.y*blockDim.y + threadIdx.y;
   if (x < width && y < height) {
      float c = tex2D(tex, x, y);
      odata[y*width+x] = c;
   }
}
