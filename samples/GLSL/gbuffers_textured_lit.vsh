#version 120

varying vec3 color;
varying vec2 texcoord;

void main() {
	color    = gl_Color.rgb;
	texcoord = gl_MultiTexCoord0.st;
	
	gl_Position = ftransform();
}