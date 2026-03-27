#ifndef SULF_SHADER_H
#define SULF_SHADER_H

	/* default shaders */

const char *BASE2_VS = "#version 300 es \n\
precision highp float; \n\
layout (location = 0) in vec2 bPos; \n\
layout (location = 1) in vec2 bTex; \n\
layout (location = 2) in vec2 aPos; \n\
layout (location = 3) in vec2 aSize; \n\
layout (location = 4) in float aTexId; \n\
layout (location = 5) in vec2 aTexUVPos; \n\
layout (location = 6) in vec2 aTexUVSize; \n\
uniform mat4 uPMat; \n\
out vec2 vTex; \n\
void main(void) { \n\
	vec2 pos = (bPos * aSize) + aPos; \n\
	gl_Position = uPMat * vec4(pos, 0.0, 1.0); \n\
	vTex = (bTex * bSize) + aTexUVPos; \n\
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

//uniform vec4 uColor; \n\
//FragColor = vec4(texColor.rgb * uColor.rgb, texColor.a * uColor.a); \n\

	/*
		default 2d draw data:
			defines one object sent to be drawn to the 2d shader.
			(objects sent to the 3d shader are compiled based on the pipeline definition) 
	*/

typedef struct draw_dat2d {
	float aPos[2];
	float aTex[2];
} draw_dat2d_t;

	/* shader main attribute def */

typedef struct shader_attr_def {
	const char* aPos;
	const char* aTex;
	const char* uPers;
	const char* uObj;
	const char* uColor; /* NULL-able */
} shader_attr_def_t;

const shader_attr_def_t BASE2_ATTR_DEF = {
		"aPos", "aTex", "uPMat", "uMVMat", "uColor" 
	};

	/* shader definition + auxiliary initialization functions */

typedef struct shader {
	GLuint prog;
	GLint vao;
	GLint aPos;
	GLint aTex;
	GLint uPers;
	GLint uObj;
	GLint uColor; /* NULL-able */
} shader_t;

GLuint makeShaderSrc(const char *src, GLenum sType) {
	GLuint s = glCreateShader(sType);
	glShaderSource(s, 1, &src, NULL);
	glCompileShader(s);

	GLint status;
	glGetShaderiv(s, GL_COMPILE_STATUS, &status);
	if (!status) {
		int errLen;
		glGetShaderiv(s, GL_INFO_LOG_LENGTH, &errLen);
		char errBuf[errLen];
		glGetShaderInfoLog(s, errLen, &errLen, errBuf);
		exit_log("shader.h - Could not compile shaders:\n%s", errBuf);
	}
	return s;
}

void addAttribShader(GLuint prog, GLint* loc, const char* name) {
	GLint l = glGetAttribLocation(prog, name);
	if (l < 0) exit_log("shader.h - Could not load shader attribute %s", name);
	glEnableVertexAttribArray(l);
	*loc = l;
}

void addUniformShader(GLuint prog, GLint* loc, const char* name) {
	GLint l = glGetUniformLocation(prog, name);
	if (l < 0) exit_log("shader.h - Could not load shader uniform %s", name);
	*loc = l;
}

	/* shader constructors / destructors */

shader_t* initShader(const char* vs, const char* fs, const shader_attr_def_t* def) {
	// compile shaders
	GLuint frag = makeShaderSrc(fs, GL_FRAGMENT_SHADER);
	GLuint vert = makeShaderSrc(vs, GL_VERTEX_SHADER);

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

void delShader(shader_t* s) {
	free(s);
}

	/* render functions */

void _setAttrib(GLint loc, buffer_t* buf, GLint size) {
	glBindBuffer(GL_ARRAY_BUFFER, buf->id);
	glVertexAttribPointer(loc, size, GL_FLOAT, 0, 0, 0);
}

void drawMeshShader(shader_t* shader) {
}

/*
void drawMeshShader(shader_t* shader, GLfloat* oMat, mesh_t* mesh, buffer_t* tBuf, tex_image_t* image) {
	_setAttrib(shader->aPos, &mesh->vBuf, VBUF_SIZE);
	_setAttrib(shader->aTex, tBuf, TBUF_SIZE);

	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, image->texId);

	glUniformMatrix4fv(shader->uObj, 1, 0, oMat);
	if (shader->uColor >= 0) glUniform4f(shader->uColor, 1.0f, 1.0f, 1.0f, 1.0f);
	
	glDrawArrays(GL_TRIANGLES, 0, mesh->vBuf.numItems);
}*/

#endif /* SULF_SHADER_H */