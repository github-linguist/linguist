#version 330 core

layout(points) in ;
layout(points, max_vertices = 1) out;

// 直通的几何着色器 原样输出
void main()
{
	gl_Position = gl_in[0].gl_Position;
	gl_PointSize = gl_in[0].gl_PointSize;
	EmitVertex();
	EndPrimitive();
}