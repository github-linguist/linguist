#version 120

uniform sampler2D texture;

varying vec3 color;
varying vec2 texcoord;

vec4 GetDiffuse() {
	vec4 diffuse = vec4(color.rgb, 1.0);
	     diffuse *= texture2D(texture, texcoord);
	
	return diffuse;
}


void main() {
	vec4 diffuse = GetDiffuse();
	
	gl_FragData[0] = diffuse;
}