#version 300 es
precision highp float;

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTex;
layout (location = 2) in vec3 iPos;
layout (location = 3) in float iTexId;
layout (location = 4) in vec2 iUVPos;
layout (location = 5) in vec2 iUVSize;

uniform mat4 uPMat;
uniform mat4 uMVMat;
uniform mat4 uLightMat;

//uniform float spFlag;

out float vTexId;
out vec2 vTex;

void main()
{
	// adjustment for sprites
	vec4 vPos = vec4(aPos + iPos, 1.0);
	//if (spFlag > 0.5) vPos.z = vPos.z - 0.1;
	// calculate position
	gl_Position = uPMat * uLightMat * uMVMat * vPos; 
	vTex = aTex;
}