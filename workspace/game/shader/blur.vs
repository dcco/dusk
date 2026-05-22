#version 330
precision highp float;

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTex;

out vec2 xTex;

void main()
{
	gl_Position = vec4(aPos, 1.0);
	xTex = aTex;
}
