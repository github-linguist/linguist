// Taken from https://github.com/gdquest-demos/godot-shaders/blob/main/godot/Shaders/gaussian_blur.gdshader

shader_type canvas_item;

uniform vec2 blur_scale = vec2(1, 0);

const float SAMPLES = 71.0;
const float TAU = 6.283185307179586476925286766559;

float gaussian(float x) {
	float x_squared = x * x;
	float width = 1.0 / sqrt(TAU * SAMPLES);

	return width * exp((x_squared / (2.0 * SAMPLES)) * -1.0);
}

void fragment() {
	vec2 scale = TEXTURE_PIXEL_SIZE * blur_scale;
	
	float total_weight = 0.0;
	vec4 color = vec4(0.0);
	
	for (int i = -int(SAMPLES) / 2; i < int(SAMPLES) / 2; ++i) {
		float weight = gaussian(float(i));
		color += texture(TEXTURE, UV + scale * vec2(float(i))) * weight;
		total_weight += weight;
	}
	
	COLOR = color / total_weight;
}