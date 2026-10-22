/*
Source: https://github.com/MicBosi/VisualizationLibrary/blob/47d0b4e30c9375ee3e8096a973e13c107f2b9ccc/data/glsl/examples/triangle_fur.gs
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

#version 150 compatibility

#extension GL_ARB_geometry_shader4 : enable

layout(triangles) in;
layout(triangle_strip, max_vertices = 18) out;


void main(void)
{
	vec4 A = gl_PositionIn[0];
	vec4 B = gl_PositionIn[1];
	vec4 C = gl_PositionIn[2];
	vec4 D = (A+B) * 0.5;
	vec4 E = (B+C) * 0.5;
	vec4 F = (C+A) * 0.5;
	vec4 G = (A+B+C) * 0.33333;
	vec3 N = normalize(cross(B.xyz-A.xyz, C.xyz-A.xyz)) * length(B.xyz-A.xyz);
	G += vec4(-2.0*N,0);

	gl_Position    = A; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = C; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = B; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = G; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = F; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = G; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();

	gl_Position    = E; gl_FrontColor  = gl_FrontColorIn[0]; EmitVertex();
	gl_Position    = G; gl_FrontColor  = gl_FrontColorIn[2]; EmitVertex();
	gl_Position    = D; gl_FrontColor  = gl_FrontColorIn[1]; EmitVertex();
	EndPrimitive();
}
