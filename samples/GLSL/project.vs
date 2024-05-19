#version 100
attribute vec2 pos;
attribute vec3 rayvtx;
varying vec3 Ray;
uniform vec4 split;

void main()
{
	Ray = rayvtx;
	gl_Position = vec4(pos * vec2(split.z, split.w) + vec2(split.x, split.y),
					   0.0, 1.0);
}