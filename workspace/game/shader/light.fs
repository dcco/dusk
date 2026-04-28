#version 300 es
precision mediump float;
precision mediump sampler2DArray;

in vec4 tPos;
in vec4 fragLightPos;
in float vTexId;
in vec2 vTex;

uniform sampler2DArray uSampler;
uniform sampler2D uShadowMap;

out vec4 gColor;

const float shadowBias = 0.000005;
const float shadowRes = 2048.0;
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

void main()
{
	float shadow = shadowCalc(fragLightPos);
	vec4 vColor = texture(uSampler, vec3(vTex, vTexId));
	gColor = vec4((1.0 - (shadow * 0.4)) * vColor.rgb, vColor.a);
}