#ifndef SULF_SHADER2D_H
#define SULF_SHADER2D_H

	/*
		builtin 2d shader
	*/

const char *BASE2_VS = "#version 300 es \n\
precision highp float; \n\
layout (location = 0) in vec3 bPos; \n\
layout (location = 1) in vec2 bTex; \n\
layout (location = 2) in vec3 aPos; \n\
layout (location = 3) in vec2 aSize; \n\
layout (location = 4) in uint aColor; \n\
layout (location = 5) in float aTexId; \n\
layout (location = 6) in vec2 aTexUVPos; \n\
layout (location = 7) in vec2 aTexUVSize; \n\
uniform mat4 uPMat; \n\
uniform int uTotal; \n\
out vec2 vTex; \n\
out float vTexId; \n\
out vec3 vColor; \n\
vec3 unpackRGB(uint c) \n\
{ \n\
	float r = float((c >> 16) & 0xFFu) / 255.0; \n\
	float g = float((c >> 8) & 0xFFu) / 255.0; \n\
	float b = float(c & 0xFFu) / 255.0; \n\
	return vec3(r, g, b); \n\
} \n\
void main(void) { \n\
	vec2 pos = (bPos.xy * aSize) + aPos.xy; \n\
	gl_Position = uPMat * vec4(pos, 0.0, 1.0); \n\
	gl_Position.z = aPos.z / float(uTotal); \n\
	vTex = (bTex * aTexUVSize) + aTexUVPos; \n\
	vTexId = aTexId; \n\
	vColor = unpackRGB(aColor); \n\
}";

const char *BASE2_FS = "#version 300 es \n\
precision mediump float; \n\
precision mediump sampler2DArray; \n\
in vec2 vTex; \n\
in float vTexId; \n\
in vec3 vColor; \n\
uniform sampler2DArray uSampler; \n\
out vec4 FragColor; \n\
void main(void) { \n\
	vec4 texColor = texture(uSampler, vec3(vTex, vTexId)); \n\
	if (texColor.a == 0.0) discard; \n\
	vec3 mColor = texColor.rgb * vColor.rgb; \n\
	FragColor = vec4(mColor, texColor.a); \n\
}";

//uniform vec4 uColor; \n\
//FragColor = vec4(texColor.rgb * uColor.rgb, texColor.a * uColor.a); \n\

	/*
		default 2d draw data:
			defines one object sent to be drawn to the 2d shader.
			(objects sent to the 3d shader are compiled based on the pipeline definition) 
	*/

typedef struct draw_dat2d {
	float aPos[3];
	float aSize[2];
	uint32_t aColor;
	float aTexId;
	float aTexUVPos[2];
	float aTexUVSize[2];
} draw_dat2d_t;

	/*
		builtin 2d shader's definition
	*/

const struct shader_attr_def BASE2_ATTR_LIST[8] = {
	{ 1, 3, GL_FLOAT, (void*) offsetof(vertex_t, pos) },
	{ 3, 2, GL_FLOAT, (void*) offsetof(vertex_t, uv) },
	{ 0, 3, GL_FLOAT, (void*) offsetof(draw_dat2d_t, aPos) },
	{ 0, 2, GL_FLOAT, (void*) offsetof(draw_dat2d_t, aSize) },
	{ 0, 1, GL_UNSIGNED_INT, (void*) offsetof(draw_dat2d_t, aColor) },
	{ 0, 1, GL_FLOAT, (void*) offsetof(draw_dat2d_t, aTexId) },
	{ 0, 2, GL_FLOAT, (void*) offsetof(draw_dat2d_t, aTexUVPos) },
	{ 0, 2, GL_FLOAT, (void*) offsetof(draw_dat2d_t, aTexUVSize) }
};

const struct shader_def BASE2_DEF = {
	8, sizeof(draw_dat2d_t), BASE2_ATTR_LIST, 0, NULL, "uSampler", "uTotal", "uPMat"
};

#endif 