#version 300 es
precision highp float;

layout (location = 0) in vec3 bPos;
layout (location = 1) in vec2 bTexCoord;
layout (location = 2) in vec3 aPos;
layout (location = 3) in float aTexId;
layout (location = 4) in vec2 aTexUV;
layout (location = 5) in vec2 aTexSize;

uniform mat4 uPMat;

out float vTexId;
out vec2 vTexCoord;

void main()
{
	vec3 xPos = bPos + aPos;
	vTexId = aTexId;
	vTexCoord = (bTexCoord * aTexSize) + aTexUV;
	gl_Position = uPMat * vec4(xPos, 1.0);
}