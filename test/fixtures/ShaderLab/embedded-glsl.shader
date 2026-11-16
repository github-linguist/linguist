Shader "Effects/Desaturate" {
    Properties {
        _MainTex ("Texture", 2D) = "white" {}
    }
    SubShader {
        Pass {
            GLSLPROGRAM
            #version 120
            #ifdef VERTEX
            void main() {
                gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
                gl_TexCoord[0] = gl_MultiTexCoord0;
            }
            #endif
            #ifdef FRAGMENT
            uniform sampler2D _MainTex;
            void main() {
                vec4 color = texture2D(_MainTex, gl_TexCoord[0].xy);
                float luminance = dot(color.rgb, vec3(0.2126, 0.7152, 0.0722));
                gl_FragColor = vec4(vec3(luminance), color.a);
            }
            #endif
            ENDGLSL
        }
    }
}
