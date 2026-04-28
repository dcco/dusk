#version 300 es
precision mediump float;
precision mediump sampler2DArray;

out vec4 gColor;

in float vTexId;
in vec2 vTex;

uniform sampler2DArray uSampler;

void main()
{
	float alpha = texture(uSampler, vec3(vTex, vTexId)).a;
	if (alpha == 0.0) discard;
}