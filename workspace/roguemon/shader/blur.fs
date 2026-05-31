#version 330
precision highp float;

in vec2 xTex;

uniform sampler2D gColor;

out vec4 FragColor;

const float weight[5] = float[] (0.227027, 0.1945946, 0.1216216, 0.054054, 0.016216);

void main()
{
	vec4 vColor = texture(gColor, xTex);
	ivec2 texSize = textureSize(gColor, 0);
	vec2 texOffset = vec2(1.0 / float(texSize.x), 1.0 / float(texSize.y));
	float result = vColor.r * weight[0];
	for (int i = 1; i < 5; i++) {
		result = result + texture(gColor, xTex + vec2(texOffset.x * float(i), 0.0)).r * weight[i];
		result = result + texture(gColor, xTex - vec2(texOffset.x * float(i), 0.0)).r * weight[i];
	}
	/*for (int i = 1; i < 5; i++) {
		result = result + texture(gColor, xTex + vec2(0.0, texOffset.y * float(i))).r * weight[i] * 0.5;
		result = result + texture(gColor, xTex - vec2(0.0, texOffset.y * float(i))).r * weight[i] * 0.5;	
	}*/
	if (result < vColor.r) result = vColor.r;
	FragColor = vec4(result, vColor.g, vColor.b, vColor.a);
}