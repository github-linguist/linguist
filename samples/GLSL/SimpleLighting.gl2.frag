static const char* SimpleFragmentShader = STRINGIFY(

varying vec4 FrontColor;

void main(void)
{
    gl_FragColor = FrontColor;
}
);
