// Taken directly from Godot Engine documentation
// https://docs.godotengine.org/en/stable/tutorials/shaders/shader_reference/shader_preprocessor.html

// fancy_color.gdshaderinc

// While technically allowed, there is usually no `shader_type` declaration in include files.

vec3 get_fancy_color() {
    return vec3(0.3, 0.6, 0.9);
}