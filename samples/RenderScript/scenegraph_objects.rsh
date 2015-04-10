// Copyright (C) 2011-2012 The Android Open Source Project
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#pragma version(1)

#pragma rs java_package_name(com.android.scenegraph)

#ifndef _TRANSFORM_DEF_
#define _TRANSFORM_DEF_

#include "rs_graphics.rsh"

#define TRANSFORM_NONE      0
#define TRANSFORM_TRANSLATE 1
#define TRANSFORM_ROTATE    2
#define TRANSFORM_SCALE     3

#define CULL_FRUSTUM 0
#define CULL_ALWAYS  2

#define LIGHT_POINT       0
#define LIGHT_DIRECTIONAL 1

// Shader params that involve only data
#define SHADER_PARAM_DATA_ONLY                 10000
#define SHADER_PARAM_FLOAT4_DATA               10001
#define SHADER_PARAM_TRANSFORM_DATA            10002
#define SHADER_PARAM_TRANSFORM_MODEL           10003

// Shader params that involve camera
#define SHADER_PARAM_CAMERA                    1000
#define SHADER_PARAM_FLOAT4_CAMERA_POS         1001
#define SHADER_PARAM_FLOAT4_CAMERA_DIR         1002
#define SHADER_PARAM_TRANSFORM_VIEW            1003
#define SHADER_PARAM_TRANSFORM_PROJ            1004
#define SHADER_PARAM_TRANSFORM_VIEW_PROJ       1005
#define SHADER_PARAM_TRANSFORM_MODEL_VIEW      1006
#define SHADER_PARAM_TRANSFORM_MODEL_VIEW_PROJ 1007

// Shader Params that only involve lights
#define SHADER_PARAM_LIGHT                     100
#define SHADER_PARAM_FLOAT4_LIGHT_COLOR        103
#define SHADER_PARAM_FLOAT4_LIGHT_POS          104
#define SHADER_PARAM_FLOAT4_LIGHT_DIR          105

#define SHADER_PARAM_TEXTURE                   10

#define TEXTURE_NONE          0
#define TEXTURE_2D            1
#define TEXTURE_CUBE          2
#define TEXTURE_RENDER_TARGET 3

typedef struct TransformComponent_s {
    float4 value;
    int type;
    rs_allocation name;
} SgTransformComponent;

typedef struct __attribute__((packed, aligned(4))) SgTransform {
    rs_matrix4x4 globalMat;
    rs_matrix4x4 localMat;

    rs_allocation components;
    int isDirty;

    rs_allocation children;
    rs_allocation name;

    // Used to check whether transform params need to be updated
    uint32_t timestamp;
} SgTransform;

typedef struct VertexShader_s {
    rs_program_vertex program;
    // Buffer with vertex constant data
    rs_allocation shaderConst;
    // ShaderParam's that populate data
    rs_allocation shaderConstParams;
    // location of the per object constants on the buffer
    int objectConstIndex;
} SgVertexShader;

typedef struct FragmentShader_s {
    rs_program_fragment program;
    // Buffer with vertex constant data
    rs_allocation shaderConst;
    // ShaderParam's that populate data
    rs_allocation shaderConstParams;
    // ShaderParam's that set textures
    rs_allocation shaderTextureParams;
    // location of the per object constants on the buffer
    int objectConstIndex;
} SgFragmentShader;

typedef struct RenderState_s {
    rs_allocation pv; // VertexShader struct
    rs_allocation pf; // FragmentShader struct
    rs_program_store ps;
    rs_program_raster pr;
} SgRenderState;

typedef struct Renderable_s {
    rs_allocation render_state;
    // Buffer with vertex constant data
    rs_allocation pv_const;
    // ShaderParam's that populate data
    rs_allocation pv_constParams;
    // Buffer with fragment constant data
    rs_allocation pf_const;
    // ShaderParam's that populate data
    rs_allocation pf_constParams;
    rs_allocation pf_textures[8];
    int pf_num_textures;
    rs_mesh mesh;
    int meshIndex;
    rs_allocation transformMatrix;
    rs_allocation name;
    float4 boundingSphere;
    float4 worldBoundingSphere;
    int bVolInitialized;
    int cullType; // specifies whether to frustum cull
    int isVisible;
} SgRenderable;

typedef struct RenderPass_s {
    rs_allocation color_target;
    rs_allocation depth_target;
    rs_allocation camera;
    rs_allocation objects;

    float4 clear_color;
    float clear_depth;
    bool should_clear_color;
    bool should_clear_depth;
} SgRenderPass;

typedef struct Camera_s {
    rs_matrix4x4 proj;
    rs_matrix4x4 view;
    rs_matrix4x4 viewProj;
    float4 position;
    float near;
    float far;
    float horizontalFOV;
    float aspect;
    rs_allocation name;
    rs_allocation transformMatrix;
    float4 frustumPlanes[6];

    int isDirty;
    // Timestamp of the camera itself to signal params if anything changes
    uint32_t timestamp;
    // Timestamp of our transform
    uint32_t transformTimestamp;
} SgCamera;

typedef struct Light_s {
    float4 position;
    float4 color;
    float intensity;
    int type;
    rs_allocation name;
    rs_allocation transformMatrix;
} SgLight;

// This represents the shader parameter data needed to set a float or transform data
typedef struct ShaderParamData_s {
    int type;
    float4 float_value;
    uint32_t timestamp;
    rs_allocation paramName;
    rs_allocation camera;
    rs_allocation light;
    rs_allocation transform;
    rs_allocation texture;
} SgShaderParamData;

// This represents a shader parameter that knows how to update itself for a given
// renderable or shader and contains a timestamp for the last time this buffer was updated
typedef struct ShaderParam_s {
    // Used to check whether transform params need to be updated
    uint32_t transformTimestamp;
    // Used to check whether data params need to be updated
    // These are used when somebody set the matrix of float value directly in java
    uint32_t dataTimestamp;
    // Specifies where in the constant buffer data gets written to
    int bufferOffset;
    // An instance of SgShaderParamData that could be shared by multiple objects
    rs_allocation data;
    // How many components of the vector we need to write
    int float_vecSize;
} SgShaderParam;

// This represents a texture object
typedef struct Texture_s {
    uint32_t type;
    rs_allocation texture;
} SgTexture;

static void printName(rs_allocation name) {
    if (!rsIsObject(name)) {
        rsDebug("no name", 0);
        return;
    }

    rsDebug((const char*)rsGetElementAt(name, 0), 0);
}

static void printCameraInfo(const SgCamera *cam) {
    rsDebug("***** Camera information. ptr:", cam);
    printName(cam->name);
    const SgTransform *camTransform = (const SgTransform *)rsGetElementAt(cam->transformMatrix, 0);
    rsDebug("Transform name:", camTransform);
    printName(camTransform->name);

    rsDebug("Aspect: ", cam->aspect);
    rsDebug("Near: ", cam->near);
    rsDebug("Far: ", cam->far);
    rsDebug("Fov: ", cam->horizontalFOV);
    rsDebug("Position: ", cam->position);
    rsDebug("Proj: ", &cam->proj);
    rsDebug("View: ", &cam->view);
}

static void printLightInfo(const SgLight *light) {
    rsDebug("***** Light information. ptr:", light);
    printName(light->name);
    const SgTransform *lTransform = (const SgTransform *)rsGetElementAt(light->transformMatrix, 0);
    rsDebug("Transform name:", lTransform);
    printName(lTransform->name);

    rsDebug("Position: ", light->position);
    rsDebug("Color : ", light->color);
    rsDebug("Intensity: ", light->intensity);
    rsDebug("Type: ", light->type);
}

static void getCameraRay(const SgCamera *cam, int screenX, int screenY, float3 *pnt, float3 *vec) {
    rsDebug("=================================", screenX);
    rsDebug("Point X", screenX);
    rsDebug("Point Y", screenY);

    rs_matrix4x4 mvpInv;
    rsMatrixLoad(&mvpInv, &cam->viewProj);
    rsMatrixInverse(&mvpInv);

    float width = (float)rsgGetWidth();
    float height = (float)rsgGetHeight();

    float4 pos = {(float)screenX, height - (float)screenY, 0.0f, 1.0f};

    pos.x /= width;
    pos.y /= height;

    rsDebug("Pre Norm X", pos.x);
    rsDebug("Pre Norm Y", pos.y);

    pos.xy = pos.xy * 2.0f - 1.0f;

    rsDebug("Norm X", pos.x);
    rsDebug("Norm Y", pos.y);

    pos = rsMatrixMultiply(&mvpInv, pos);
    float oneOverW = 1.0f / pos.w;
    pos.xyz *= oneOverW;

    rsDebug("World X", pos.x);
    rsDebug("World Y", pos.y);
    rsDebug("World Z", pos.z);

    rsDebug("Cam X", cam->position.x);
    rsDebug("Cam Y", cam->position.y);
    rsDebug("Cam Z", cam->position.z);

    *vec = normalize(pos.xyz - cam->position.xyz);
    rsDebug("Vec X", vec->x);
    rsDebug("Vec Y", vec->y);
    rsDebug("Vec Z", vec->z);
    *pnt = cam->position.xyz;
}

static bool intersect(const SgRenderable *obj, float3 pnt, float3 vec) {
    // Solving for t^2 + Bt + C = 0
    float3 originMinusCenter = pnt - obj->worldBoundingSphere.xyz;
    float B = dot(originMinusCenter, vec) * 2.0f;
    float C = dot(originMinusCenter, originMinusCenter) -
              obj->worldBoundingSphere.w * obj->worldBoundingSphere.w;

    float discriminant = B * B - 4.0f * C;
    if (discriminant < 0.0f) {
        return false;
    }
    discriminant = sqrt(discriminant);

    float t0 = (-B - discriminant) * 0.5f;
    float t1 = (-B + discriminant) * 0.5f;

    if (t0 > t1) {
        float temp = t0;
        t0 = t1;
        t1 = temp;
    }

    // The sphere is behind us
    if (t1 < 0.0f) {
        return false;
    }
    return true;
}


#endif // _TRANSFORM_DEF_
