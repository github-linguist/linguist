// Copyright (c) 2012 The Chromium Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

// To compile these two shaders:
// fxc /E pixelMain /T ps_2_0 accelerated_surface_win.hlsl
// fxc /E vertexMain /T vs_2_0 accelerated_surface_win.hlsl
//
// fxc is in the DirectX SDK.

struct Vertex {
  float4 position : POSITION;
  float2 texCoord : TEXCOORD0;
};

texture t;
sampler s;

// Passes a position and texture coordinate to the pixel shader.
Vertex vertexMain(Vertex input) {
  return input;
};

// Samples a texture at the given texture coordinate and returns the result.
float4 pixelMain(float2 texCoord : TEXCOORD0) : COLOR0 {
  return tex2D(s, texCoord);
};
