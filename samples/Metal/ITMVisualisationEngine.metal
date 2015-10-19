// Copyright 2014 Isis Innovation Limited and the authors of InfiniTAM

#include <metal_stdlib>

#include "../../DeviceAgnostic/ITMSceneReconstructionEngine.h"
#include "../../DeviceAgnostic/ITMVisualisationEngine.h"
#include "ITMVisualisationEngine_Metal.h"

using namespace metal;

kernel void genericRaycastVH_device(DEVICEPTR(Vector4f) *pointsRay                                  [[ buffer(0) ]],
                                    const CONSTPTR(ITMVoxel) *voxelData                             [[ buffer(1) ]],
                                    const CONSTPTR(typename ITMVoxelIndex::IndexData) *voxelIndex   [[ buffer(2) ]],
                                    const CONSTPTR(Vector2f) *minmaxdata                            [[ buffer(3) ]],
                                    const CONSTPTR(CreateICPMaps_Params) *params                    [[ buffer(4) ]],
                                    uint2 threadIdx                                                 [[ thread_position_in_threadgroup ]],
                                    uint2 blockIdx                                                  [[ threadgroup_position_in_grid ]],
                                    uint2 blockDim                                                  [[ threads_per_threadgroup ]])
{
    int x = threadIdx.x + blockIdx.x * blockDim.x, y = threadIdx.y + blockIdx.y * blockDim.y;
    
    if (x >= params->imgSize.x || y >= params->imgSize.y) return;
    
    int locId = x + y * params->imgSize.x;
    int locId2 = (int)floor((float)x / minmaximg_subsample) + (int)floor((float)y / minmaximg_subsample) * params->imgSize.x;
    
    castRay<ITMVoxel, ITMVoxelIndex>(pointsRay[locId], x, y, voxelData, voxelIndex, params->invM, params->invProjParams,
                                     params->voxelSizes.y, params->lightSource.w, minmaxdata[locId2]);
}

kernel void genericRaycastVGMissingPoints_device(DEVICEPTR(Vector4f) *forwardProjection                         [[ buffer(0) ]],
                                                 const CONSTPTR(int) *fwdProjMissingPoints                      [[ buffer(1) ]],
                                                 const CONSTPTR(ITMVoxel) *voxelData                            [[ buffer(2) ]],
                                                 const CONSTPTR(typename ITMVoxelIndex::IndexData) *voxelIndex  [[ buffer(3) ]],
                                                 const CONSTPTR(Vector2f) *minmaxdata                           [[ buffer(4) ]],
                                                 const CONSTPTR(CreateICPMaps_Params) *params                   [[ buffer(5) ]],
                                                 uint2 threadIdx                                                [[ thread_position_in_threadgroup ]],
                                                 uint2 blockIdx                                                 [[ threadgroup_position_in_grid ]],
                                                 uint2 blockDim                                                 [[ threads_per_threadgroup ]])
{
    int pointId = threadIdx.x + blockIdx.x * blockDim.x;
    
    if (pointId >= params->imgSize.z) return;
    
    int locId = fwdProjMissingPoints[pointId];
    int y = locId / params->imgSize.x, x = locId - y * params->imgSize.x;
    int locId2 = (int)floor((float)x / minmaximg_subsample) + (int)floor((float)y / minmaximg_subsample) * params->imgSize.x;
    
    castRay<ITMVoxel, ITMVoxelIndex>(forwardProjection[locId], x, y, voxelData, voxelIndex, params->invM, params->invProjParams,
                                     params->voxelSizes.y, params->lightSource.w, minmaxdata[locId2]);
}

kernel void renderICP_device(const CONSTPTR(Vector4f) *pointsRay            [[ buffer(0) ]],
                             DEVICEPTR(Vector4f) *pointsMap                 [[ buffer(1) ]],
                             DEVICEPTR(Vector4f) *normalsMap                [[ buffer(2) ]],
                             DEVICEPTR(Vector4u) *outRendering              [[ buffer(3) ]],
                             const CONSTPTR(CreateICPMaps_Params) *params   [[ buffer(4) ]],
                             uint2 threadIdx                                [[ thread_position_in_threadgroup ]],
                             uint2 blockIdx                                 [[ threadgroup_position_in_grid ]],
                             uint2 blockDim                                 [[ threads_per_threadgroup ]])
{
    int x = threadIdx.x + blockIdx.x * blockDim.x, y = threadIdx.y + blockIdx.y * blockDim.y;
    
    if (x >= params->imgSize.x || y >= params->imgSize.y) return;
    
    processPixelICP<false>(outRendering, pointsMap, normalsMap, pointsRay, params->imgSize.xy, x, y, params->voxelSizes.x, TO_VECTOR3(params->lightSource));
}

kernel void renderForward_device(DEVICEPTR(Vector4u) *outRendering              [[ buffer(0) ]],
                                 const CONSTPTR(Vector4f) *pointsRay            [[ buffer(1) ]],
                                 const CONSTPTR(CreateICPMaps_Params) *params   [[ buffer(2) ]],
                                 uint2 threadIdx                                [[ thread_position_in_threadgroup ]],
                                 uint2 blockIdx                                 [[ threadgroup_position_in_grid ]],
                                 uint2 blockDim                                 [[ threads_per_threadgroup ]])
{
    int x = threadIdx.x + blockIdx.x * blockDim.x, y = threadIdx.y + blockIdx.y * blockDim.y;
    
    if (x >= params->imgSize.x || y >= params->imgSize.y) return;
    
    processPixelForwardRender<false>(outRendering, pointsRay, params->imgSize.xy, x, y, params->voxelSizes.x, TO_VECTOR3(params->lightSource));
}

kernel void forwardProject_device(DEVICEPTR(Vector4f) *forwardProjection         [[ buffer(0) ]],
                                  const CONSTPTR(Vector4f) *pointsRay            [[ buffer(1) ]],
                                  const CONSTPTR(CreateICPMaps_Params) *params   [[ buffer(2) ]],
                                  uint2 threadIdx                                [[ thread_position_in_threadgroup ]],
                                  uint2 blockIdx                                 [[ threadgroup_position_in_grid ]],
                                  uint2 blockDim                                 [[ threads_per_threadgroup ]])
{
    int x = (threadIdx.x + blockIdx.x * blockDim.x), y = (threadIdx.y + blockIdx.y * blockDim.y);
    
    if (x >= params->imgSize.x || y >= params->imgSize.y) return;
    
    int locId = x + y * params->imgSize.x;
    Vector4f pixel = pointsRay[locId];
    
    int locId_new = forwardProjectPixel(pixel * params->voxelSizes.x, params->M, params->projParams, params->imgSize.xy);
    if (locId_new >= 0) forwardProjection[locId_new] = pixel;
}