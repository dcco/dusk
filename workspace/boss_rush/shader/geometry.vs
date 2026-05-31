#version 330
precision highp float;

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTex;
layout (location = 2) in vec3 aNorm;
layout (location = 3) in vec3 iPos;
layout (location = 4) in float iTexId;
layout (location = 5) in vec2 iUVPos;
layout (location = 6) in vec2 iUVSize;

uniform mat4 uPMat;
uniform mat4 uMVMat;
// uniform mat3 uLightPMat;
uniform mat4 uLightMat;

out vec3 vPos;
out vec3 vNorm;
out float vTexId;
out vec2 vTex;
out vec4 fragLightPos;

void main(void) {
	// calculate position + send to fragment shader
	vec4 xPos = vec4(aPos + iPos, 1.0);
	// -- assumes shadow uses same perspective matrix
	vec4 vPos4 = uMVMat * xPos;
	fragLightPos = uPMat * uLightMat * vPos4;
	gl_Position = uPMat * vPos4;
	vPos = vPos4.xyz;

	// pass on the vertex normals for interpolation in the fragment shader
	// -- originally multiplied by uObjMat but not needed for only translation
	// -- apparently we can simply skip transpose-inverse if only rotation-translation is present
	mat3 normMat = mat3(uMVMat);
	vNorm = normMat * aNorm;

	// calculate texture coordinate
	vTexId = iTexId;
	vTex = (aTex * iUVSize) + iUVPos;
}
