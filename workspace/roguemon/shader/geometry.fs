#version 330
precision mediump float;
precision mediump sampler2DArray;

layout (location = 0) out vec4 gPos;
layout (location = 1) out vec4 gNorm;
layout (location = 2) out vec4 gColor;
layout (location = 3) out vec4 gSpec;

uniform sampler2DArray uSampler;
uniform sampler2D uShadowMap;

in vec3 vPos;
in vec3 vNorm;
in float vTexId;
in vec2 vTex;
in vec4 fragLightPos;

const float shadowBias = 0.000005;
const float shadowRes = 4096.0;
const float shadowP = 1.0 / shadowRes;

float shadowCalc(vec4 fragLightPos) {
	// calculate position on shadow texture
	vec3 projPos = fragLightPos.xyz / fragLightPos.w;
	projPos = (projPos * 0.5) + 0.5;
	// calculate where the point is relative to other pixels of the shadow map
	float fx = float(int((projPos.x + shadowP) * shadowRes)) / shadowRes;
	float fy = float(int((projPos.y + shadowP) * shadowRes)) / shadowRes;
	float cx = (projPos.x + shadowP - fx) / shadowP;
	float cy = (projPos.y + shadowP - fy) / shadowP;
	float d1 = texture(uShadowMap, vec2(fx - shadowP, fy - shadowP)).r * cx * cy;
	float d2 = texture(uShadowMap, vec2(fx, fy - shadowP)).r * (1.0 - cx) * cy;
	float d3 = texture(uShadowMap, vec2(fx - shadowP, fy)).r * cx * (1.0 - cy);
	float d4 = texture(uShadowMap, vec2(fx, fy)).r * (1.0 - cx) * (1.0 - cy);
	// decide whether the depth is 
	float closestDepth = d1 + d2 + d3 + d4;
	float currentDepth = projPos.z;
	float shadow = currentDepth - shadowBias > closestDepth ? 1.0 : 0.0;
	return shadow;
}

void main(void) {
	vec4 vColor = texture(uSampler, vec3(vTex, vTexId));
	float alpha = vColor.a;
	if (alpha == 0.0) discard;

	gPos = vec4(vPos, alpha);
	gNorm = vec4(normalize(vNorm), alpha);
	gColor = vColor;
	float shadow = shadowCalc(fragLightPos);

	gSpec = vec4(0.0, shadow, gl_FragCoord.z / gl_FragCoord.w, alpha);
}