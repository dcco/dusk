#version 330
precision mediump float;

in vec2 xTex;

uniform sampler2D gPos;
uniform sampler2D gNorm;
uniform sampler2D texNoise;

uniform vec3 samples[64];
uniform mat4 uPMat;

out vec4 gColor;

const int kernelSize = 64;
const float radius = 0.3;
//const float bias = 0.4;
const float bias = 0.2;
const vec2 noiseScale = vec2(960.0 / 4.0, 640.0 / 4.0); 

void main()
{
	// SSAO inputs
	vec4 vPosX = texture(gPos, xTex);
	vec3 vPos = vPosX.rgb;
	vec3 vNormal = normalize(texture(gNorm, xTex).rgb);
	vec3 randomVec = normalize(texture(texNoise, xTex * noiseScale).rgb);

	// TBN change of basis matrix
	vec3 tan = normalize(randomVec - (vNormal * dot(randomVec, vNormal)));
	vec3 bitan = cross(vNormal, tan);
	mat3 tbnMat = mat3(tan, bitan, vNormal);

	// sample kernel iteration to calculate occlusion
	float occlusion = 0.0;
	for (int i = 0; i < kernelSize; i++)
	{
		// from the point, calculate a sample point
		vec3 _samplePos = tbnMat * samples[i];
		vec3 samplePos = vPos + (_samplePos * radius);
			
		// calculate the sample point back into view space, and then into a 0.0-1.0 range
		vec4 offset = vec4(samplePos, 1.0);
		offset = uPMat * offset;
		offset.xyz = ((offset.xyz / offset.w) * 0.5) + 0.5;
		
		// at the given xy coordinate, calculate the depth
		float sampleDepth = texture(gPos, offset.xy).z;

		// strengthen bias based on x offset
		float biasX = bias;// * (1.0 - (0.7 * sqrt(abs(_samplePos.x))));
		
		// if the depth > original pos by a certain amount, add to the occlusion
		float rangeCheck = smoothstep(0.0, 1.0, radius / abs(vPos.z - sampleDepth));
		float sampleEx = (sampleDepth >= samplePos.z + biasX ? 1.0 : 0.0);
		occlusion = occlusion + (sampleEx * rangeCheck);
	}
	occlusion = 1.0 - (occlusion / float(kernelSize));
	//occlusion = pow(occlusion, 1.5);
	//occlusion = occlusion * occlusion;

	gColor = vec4(occlusion, 0.0, 0.0, 1.0);
}