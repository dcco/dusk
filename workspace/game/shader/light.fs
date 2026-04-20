#version 300 es
precision mediump float;
precision mediump sampler2DArray;

in float vTexId;
in vec2 vTexCoord;

uniform sampler2DArray uSampler;

out vec4 FragColor;

void main()
{
	FragColor = texture(uSampler, vec3(vTexCoord, vTexId));
}