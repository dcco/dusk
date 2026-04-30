#version 330
precision mediump float;
precision mediump sampler2DArray;

in float vTexId;
in vec2 vTex;

uniform sampler2DArray uSampler;

out vec4 gColor;

void main()
{
	float alpha = texture(uSampler, vec3(vTex, vTexId)).a;
	if (alpha == 0.0) discard;
	gColor = vec4(0.6, 0.6, 0.6, 1.0);
}