#version 330
precision mediump float;

in vec3 xPos;
in vec2 xTex;

uniform sampler2D gPos;
uniform sampler2D gNorm;
uniform sampler2D gColor;
uniform sampler2D gSpec;

out vec4 finalColor;

const vec4 cDarkColor = vec4(0.7, 0.7, 0.8, 1.0);
const vec3 cFogColor = vec3(0.4, 0.45, 0.6);

const vec3 cLightPos = vec3(-9.0, -15.0, -25.0);
const vec3 cDiffColor = vec3(0.3, 0.3, 0.3);
const vec3 cSpecColor = vec3(0.4, 0.3, 0.5);

const vec3 centerPos = vec3(0.0, -1.0, -10.0);

const float ox = 2.5 / 960.0;
const float oy = 2.5 / 640.0;

const vec2 poissonDisk[8] = vec2[](
	vec2(-0.942, -0.399),
	vec2( 0.945, -0.768),
	vec2(-0.094, -0.929),
	vec2( 0.344,  0.293),
	vec2(-0.915,  0.457),
	vec2( 0.791,  0.598),
	vec2(-0.488, -0.649),
	vec2( 0.087, -0.005)
);

	// math functions

vec3 squareVec(vec3 v) {
	return vec3(v.r * v.r, v.g * v.g, v.b * v.b);
}

vec3 sqrtVec(vec3 v) {
	return vec3(sqrt(v.r), sqrt(v.g), sqrt(v.b));
}

void main()
{
	vec3 vPos = texture(gPos, xTex).rgb;
	vec3 vNorm = texture(gNorm, xTex).rgb;
	vec4 vColor = texture(gColor, xTex);
	vec4 vSpec = texture(gSpec, xTex);

	float distance = length(centerPos - vPos);
	float attenuation = 1.0 / (1.0 + 0.3 * pow(distance, 1.7));
	vColor = vColor + (vColor * attenuation * 0.8);

	// calculate shadow by averaging locations nearby
	/*float shadow = 0.0;
	for (int i = -1; i <= 1; i++)
	{
		for (int j = -2; j <= 0; j++)
		{
			float zx = xTex.x + (ox * float(i));
			float zy = xTex.y + (oy * float(i));
			float shadowP = texture(gSpec, vec2(zx, zy)).g;
			shadow = shadow + shadowP;
		}
	}
	shadow = (shadow * 0.4) / 9.0;*/

	// calculate shadow by averaging locations nearby
	float shadAngle = fract(sin(dot(vPos.xy, vec2(12.9898, 78.233))) * 43758.5453) * 6.2831853;
	mat2 shadRotate = mat2(cos(shadAngle), -sin(shadAngle), sin(shadAngle), cos(shadAngle));
	float shadow = 0.0;
	for (int i = 0; i < 8; i++)
	{
		vec2 offset = shadRotate * poissonDisk[i];
		float zx = xTex.x + offset.x * ox;
		float zy = xTex.y + (offset.y - 0.8) * oy;
		float shadowP = texture(gSpec, vec2(zx, zy)).g;
		shadow = shadow + shadowP;
	}
	shadow = (shadow * 0.4) / 8.0;

	// calculate the specular reflection
	vec3 lightDir = normalize(cLightPos - vPos);
	float lambert = dot(vNorm, lightDir);
	float spec = 0.0;
	if (lambert > 0.0)
	{
		vec3 viewDir = normalize(-vPos);
		vec3 halfDir = normalize(lightDir + viewDir);
		float specAngle = dot(halfDir, vNorm);
		spec = pow(specAngle, 4.0);
	}

	// combine the ambient, diffuse, and specular light to get the base color
	// -- coloring given used as ambient light value
	float alpha = vColor.a;
	vec4 texColor = vColor * cDarkColor;
	texColor = vec4((1.0 - shadow) * texColor.rgb, texColor.a);
	// -- lambert multiplier makes things lighter
	vec4 lightColor = vec4((lambert * 0.3 * cDiffColor) + (spec * 0.5 * cSpecColor), alpha);
	vec4 baseColor = texColor + (lightColor * 0.7);
	//vec4 baseColor = texColor;
	// fog factor
	float LOG2 = 1.442695;
	float fogCoord = vSpec.z - 3.0;

	float fogDensity = 0.07;
	float fogFactor = exp2(-fogDensity * fogDensity * fogCoord * fogCoord * LOG2);
	fogFactor = clamp(fogFactor, 0.0, 1.0);
	vec4 fogColor = mix(vec4(cFogColor, 1.0), baseColor, fogFactor);

	finalColor = fogColor;	
}