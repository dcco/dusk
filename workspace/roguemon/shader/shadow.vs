#version 330
precision highp float;

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTex;
layout (location = 3) in vec3 iPos;
layout (location = 4) in float iTexId;
layout (location = 5) in vec2 iUVPos;
layout (location = 6) in vec2 iUVSize;

layout (location = 7) in vec4 aSkew;
layout (location = 8) in float spFlag;

uniform mat4 uPMat;
uniform mat4 uMVMat;
uniform mat4 uLightMat;

//uniform float spFlag;

out float vTexId;
out vec2 vTex;

void main()
{
	vec4 vPos = vec4(aPos + iPos, 1.0);
	float s1 = aPos.z < 0.5 ? aSkew.x : aSkew.z;
	float s2 = aPos.z < 0.5 ? aSkew.y : aSkew.w;
	vPos.y = vPos.y + (aPos.x < 0.5 ? s1 : s2);
	// adjustment for sprites
	if (spFlag > 0.5) vPos.z = vPos.z - 0.1;
	// calculate position
	gl_Position = uPMat * uLightMat * uMVMat * vPos; 
	vTexId = iTexId;
	vTex = (aTex * iUVSize) + iUVPos;
}