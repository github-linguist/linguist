#version 330 core

layout(points) in ;
layout(points, max_vertices = 1) out;

// ֱͨ�ļ�����ɫ�� ԭ�����
void main()
{
	gl_Position = gl_in[0].gl_Position;
	gl_PointSize = gl_in[0].gl_PointSize;
	EmitVertex();
	EndPrimitive();
}