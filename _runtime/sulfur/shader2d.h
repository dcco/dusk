#ifndef SULF_SHADER2D_H
#define SULF_SHADER2D_H

	/*
		builtin 2d shader
	*/

const char *BASE2_VS = "#version 300 es \n\
precision highp float; \n\
layout (location = 0) in vec3 bPos; \n\
layout (location = 1) in vec2 bTex; \n\
layout (location = 2) in vec2 aPos; \n\
layout (location = 3) in vec2 aSize; \n\
layout (location = 4) in float aTexId; \n\
layout (location = 5) in vec2 aTexUVPos; \n\
layout (location = 6) in vec2 aTexUVSize; \n\
uniform mat4 uPMat; \n\
out vec2 vTex; \n\
out float vTexId; \n\
void main(void) { \n\
	vec2 pos = (bPos.xy * aSize) + aPos; \n\
	gl_Position = uPMat * vec4(pos, 0.0, 1.0); \n\
	vTex = (bTex * aTexUVSize) + aTexUVPos; \n\
	vTexId = aTexId; \n\
}";

const char *BASE2_FS = "#version 300 es \n\
precision mediump float; \n\
precision mediump sampler2DArray; \n\
in vec2 vTex; \n\
in float vTexId; \n\
uniform sampler2DArray uSampler; \n\
out vec4 FragColor; \n\
void main(void) { \n\
	vec4 texColor = texture(uSampler, vec3(vTex, vTexId)); \n\
	if (texColor.a == 0.0) discard; \n\
	FragColor = vec4(texColor.rgb, texColor.a); \n\
}";

	/*
		default 2d draw data:
			defines one object sent to be drawn to the 2d shader.
			(objects sent to the 3d shader are compiled based on the pipeline definition) 
	*/

typedef struct draw_dat2d {
	float aPos[2];
	float aSize[2];
	float aTexId;
	float aTexUVPos[2];
	float aTexUVSize[2];
} draw_dat2d_t;

	/*
		builtin 2d shader's definition
	*/

const struct shader_attr_def BASE2_ATTR_LIST[7] = {
	{ 1, 3, (void*) offsetof(vertex_t, pos) },
	{ 3, 2, (void*) offsetof(vertex_t, uv) },
	{ 0, 2, (void*) offsetof(draw_dat2d_t, aPos) },
	{ 0, 2, (void*) offsetof(draw_dat2d_t, aSize) },
	{ 0, 1, (void*) offsetof(draw_dat2d_t, aTexId) },
	{ 0, 2, (void*) offsetof(draw_dat2d_t, aTexUVPos) },
	{ 0, 2, (void*) offsetof(draw_dat2d_t, aTexUVSize) }
};

const struct shader_def BASE2_DEF = {
	7, sizeof(draw_dat2d_t), BASE2_ATTR_LIST, "uSampler", "uPMat"
};

//uniform vec4 uColor; \n\
//FragColor = vec4(texColor.rgb * uColor.rgb, texColor.a * uColor.a); \n\

	/*
		default 2d draw data:
			defines one object sent to be drawn to the 2d shader.
			(objects sent to the 3d shader are compiled based on the pipeline definition) 
	*/
/*
typedef struct draw_dat2d {
	float bPos[2];
	float bTex[2];
	float aPos[2];
	float aSize[2];
	float aTexId;
	float aSize[2];
	float aTexUVPos[2];
	float aTexUVSize[2];
} draw_dat2d_t;
*/
	/*
		shader 2d:
			special shader object for the main 2d shader


typedef struct shader2d {
	GLuint prog;
} shader2d_t;	*/

	/* shader constructors / destructors

shader_t* initShader2d() {
	// compile shaders
	GLuint frag = makeShaderSrc(BASE2_FS, GL_FRAGMENT_SHADER);
	GLuint vert = makeShaderSrc(BASE2_VS, GL_VERTEX_SHADER);

	// build shader program
	GLuint prog = glCreateProgram();
	if (prog == 0) exit_log("shader.h - Could not create shader program.", "");
	glAttachShader(prog, vert);
	glAttachShader(prog, frag);
	glLinkProgram(prog);

	// check validity
	GLint status;
	glGetProgramiv(prog, GL_LINK_STATUS, &status);
	if (!status) exit_log("shader.h - Could not link shaders.", "");
	glUseProgram(prog);

	// create shader object
	shader_t* shader = (shader_t*) malloc(sizeof(shader_t));
	shader->prog = prog;
	shader->uColor = -1;

	// create vertex array object
	glGenVertexArrays(1, &shader->vao);
	glBindVertexArray(shader->vao);

	// add main attributes
	addAttribShader(prog, &shader->aPos, def->aPos);
	addAttribShader(prog, &shader->aTex, def->aTex);
	addUniformShader(prog, &shader->uPers, def->uPers);
	addUniformShader(prog, &shader->uObj, def->uObj);
	if (def->uColor != NULL) addUniformShader(prog, &shader->uColor, def->uColor);

	return shader;
}

void delShader2d(shader2d_t* s) {
	free(s);
} */

#endif 