#version 120

#define SCREEN 0
#define DODGE 1
#define BURN 2
#define OVERLAY 3
#define MULTIPLY 4
#define ADD 5
#define DIVIDE 6
#define GRAIN_EXTRACT 7
#define GRAIN_MERGE 8

// grayscale
uniform sampler2D t_Lena;
// rgba
uniform sampler2D t_Tint;

uniform int i_Blend;

varying vec2 v_Uv;

void main() {
    // we sample from both textures using the same uv coordinates. since our
    // lena image is grayscale, we only get the first component.
    vec3 lena = vec3(texture2D(t_Lena, v_Uv).r);
    vec3 tint = texture2D(t_Tint, v_Uv).rgb;
   
    vec3 result = vec3(0.0);

    // normally you'd have a shader program per technique, but for the sake of
    // simplicity we'll just branch on it here.
    if (i_Blend == SCREEN) {
        result = vec3(1.0) - ((vec3(1.0) - lena) * (vec3(1.0) - tint));
    } else if (i_Blend == DODGE) {
        result = lena / (vec3(1.0) - tint);
    } else if (i_Blend == BURN) {
        result = vec3(1.0) - ((vec3(1.0) - lena) / lena);
    } else if (i_Blend == OVERLAY) {
        result = lena * (lena + (tint * 2) * (vec3(1.0) - lena));
    } else if (i_Blend == MULTIPLY) {
        result = lena * tint;
    } else if (i_Blend == ADD) {
        result = lena + tint;
    } else if (i_Blend == DIVIDE) {
        result = lena / tint;
    } else if (i_Blend == GRAIN_EXTRACT) {
        result = lena - tint + 0.5;
    } else if (i_Blend == GRAIN_MERGE) {
        result = lena + tint - 0.5;
    }

    gl_FragColor = vec4(result, 1.0);
}
