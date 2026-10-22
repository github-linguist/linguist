/*
Source: https://github.com/MicBosi/VisualizationLibrary/blob/47d0b4e30c9375ee3e8096a973e13c107f2b9ccc/data/glsl/smooth_triangle.gs
License: https://github.com/MicBosi/VisualizationLibrary/blob/47d0b4e30c9375ee3e8096a973e13c107f2b9ccc/LICENSE.md

Visualization Library

http://VisualizationLibrary.org 

Copyright (c) 2005-2020, Michele Bosi
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this 
list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this 
list of conditions and the following disclaimer in the documentation and/or 
other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/**************************************************************************************/
/*                                                                                    */
/*  Copyright (c) 2005-2020, Michele Bosi.                                            */
/*  All rights reserved.                                                              */
/*                                                                                    */
/*  This file is part of Visualization Library                                        */
/*  http://visualizationlibrary.org                                                   */
/*                                                                                    */
/*  Released under the OSI approved Simplified BSD License                            */
/*  http://www.opensource.org/licenses/bsd-license.php                                */
/*                                                                                    */
/**************************************************************************************/

#version 400 compatibility

layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;

in vec3 tesPosition[3];
in vec4 tesColor[3];

vec3 shrink(vec3 v, vec3 center, float amount)
{
	return normalize(center-v)*amount + v;
}

void main(void)
{
	vec4 A = gl_ModelViewMatrix * vec4( tesPosition[0], 1.0 );
	vec4 B = gl_ModelViewMatrix * vec4( tesPosition[1], 1.0 );
	vec4 C = gl_ModelViewMatrix * vec4( tesPosition[2], 1.0 );
	vec3 N = normalize( cross( B.xyz-A.xyz, C.xyz-A.xyz) );
	vec3 center = ( A.xyz + B.xyz + C.xyz ) / 3.0;
	A.xyz = shrink(A.xyz, center, 0.01);
	B.xyz = shrink(B.xyz, center, 0.01);
	C.xyz = shrink(C.xyz, center, 0.01);

	// simple flat shading from the center of the triangle
	vec3 L = normalize( gl_LightSource[0].position.xyz - center );
	vec4 NdotL = vec4( max(0.0, dot(N, L) ) );

	gl_FrontColor = tesColor[0] * NdotL;
	gl_Position = gl_ProjectionMatrix * A; EmitVertex();

	gl_FrontColor = tesColor[1] * NdotL;
	gl_Position = gl_ProjectionMatrix * B; EmitVertex();

	gl_FrontColor = tesColor[2] * NdotL;
	gl_Position = gl_ProjectionMatrix * C; EmitVertex();

	EndPrimitive();
}
