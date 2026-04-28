#ifndef SULF_SHADER_H
#define SULF_SHADER_H

	/*
		##### GLSL (shader file) compilation #####
	*/

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

	/*
		##### SHADERS #####
		the shader class is designed to be flexible enough to accomodate any
		shader from our custom shader language, while providing a concrete API
		for the compiled language to call. the primary operations to support are:
			- add a sprite/mesh to the render list
			- draw the render list
		
		data is fed into the render list through a VBO in an unstructured (void*) buffer
		the VAO is used to specify the layout of the VBO.
	*/

	/*
		shader_def:
			an object specifying the layout of the attributes in a shader.
			(generated during compilation by our shader pipeline language)
	*/

typedef struct shader_attr_def {
	int vertAttrFlag;
	int arity;
	GLenum glType;
	GLvoid* offset;
} shader_attr_def_t;

typedef struct shader_uniform_def {
	const char* name;
	int arity;
	GLenum glType;
} shader_uniform_def_t;

typedef struct shader_def {
	int attrTotal;
	GLsizei instSize;		// size of a unit in the render list
	const shader_attr_def_t* attrList;
	int uniformTotal;
	const shader_uniform_def_t* uniformList;
	const char* uSampler;
	const char* uTotal;		// name of uniform for total (can be NULL)
	const char* uPers;		// name of perspective matrix (can be NULL)
	int uTexTotal;
	char** uTexList;
} shader_def_t;

	/*
		shader:
			stores the shader program and its associated VAO + VBO bindings
			- stride stores how big a unit for the render list / VBO should be
	*/

typedef struct shader_uniform {
	GLint loc;
	shader_uniform_def_t uDef;
} shader_uniform_t;

typedef struct shader {
	GLuint prog;
	GLint vao;
	GLint vertexVBO;
	GLint instVBO;
	GLsizei instSize;
	GLint uSampler;
	GLint uTotal;	// total, may be -1
	GLint uPers;	// reference to perspective uniform, may be -1
	int uniformTotal;
	shader_uniform_t* uniformList;
	gl_val_t** uniformBuffer;
} shader_t;

	/* shader constructors / destructors */

shader_t* initShader(const char* vs, const char* fs, const shader_def_t* sDef) {
	// compile GLSL shader files
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
	glDeleteShader(vert);
	glDeleteShader(frag);

	// create shader object
	shader_t* shader = (shader_t*) malloc(sizeof(shader_t));
	shader->prog = prog;
	
	// create VAO + VBO
	glGenVertexArrays(1, &shader->vao);
	glGenBuffers(1, &shader->vertexVBO);
	glGenBuffers(1, &shader->instVBO);

	glBindVertexArray(shader->vao);

	// initialize VAO based on given shader definition
	shader->instSize = sDef->instSize;
	for (int i = 0; i < sDef->attrTotal; i++) {
		glEnableVertexAttribArray(i);
		shader_attr_def_t attr = sDef->attrList[i];
		// instance attribute
		if (attr.vertAttrFlag == 0) {
			glBindBuffer(GL_ARRAY_BUFFER, shader->instVBO);
			if (attr.glType == GL_UNSIGNED_INT) glVertexAttribIPointer(i, attr.arity, attr.glType, sDef->instSize, attr.offset);
			else glVertexAttribPointer(i, attr.arity, attr.glType, GL_FALSE, sDef->instSize, attr.offset);
			glVertexAttribDivisor(i, 1);
		// vertex attribute
		} else {
			GLvoid* offset;
			if (attr.vertAttrFlag == 1) offset = (void*) offsetof(vertex_t, pos);
			else if (attr.vertAttrFlag == 2) offset = (void*) offsetof(vertex_t, normal);
			else offset = (void*) offsetof(vertex_t, uv);
			glBindBuffer(GL_ARRAY_BUFFER, shader->vertexVBO);
			if (attr.glType == GL_UNSIGNED_INT) glVertexAttribIPointer(i, attr.arity, attr.glType, sizeof(vertex_t), offset);
			else glVertexAttribPointer(i, attr.arity, attr.glType, GL_FALSE, sizeof(vertex_t), offset);
		}
	}

	// initialize texture sampler
	shader->uSampler = glGetUniformLocation(prog, sDef->uSampler);
	if (shader->uSampler < 0) exit_log("shader.h - Could not load shader uniform %s", sDef->uSampler);

	// initialize total uniform when relevant
	const char* uTotal = sDef->uTotal;
	if (uTotal == NULL) {
		shader->uTotal = -1;
	} else {
		shader->uTotal = glGetUniformLocation(prog, uTotal);
		if (shader->uTotal < 0) exit_log("shader.h - Could not load shader uniform %s", uTotal);
	}

	// initialize perspective uniform when relevant
	const char* uPers = sDef->uPers;
	if (uPers == NULL) {
		shader->uPers = -1;
	} else {
		shader->uPers = glGetUniformLocation(prog, uPers);
		if (shader->uPers < 0) exit_log("shader.h - Could not load shader uniform %s", uPers);
	}

	// initialize extra uniforms
	if (sDef->uniformList == NULL) {
		shader->uniformTotal = 0;
		shader->uniformList = NULL;
		shader->uniformBuffer = NULL;
	} else {
		shader->uniformTotal = sDef->uniformTotal;
		shader->uniformList = (shader_uniform_t*) malloc(sizeof(shader_uniform_t) * sDef->uniformTotal);
		shader->uniformBuffer = (gl_val_t**) malloc(sizeof(gl_val_t*) * sDef->uniformTotal);
		for (int i = 0; i < sDef->uniformTotal; i++) {
			shader->uniformList[i].uDef = sDef->uniformList[i];
			shader->uniformList[i].loc = glGetUniformLocation(prog, sDef->uniformList[i].name);
			shader->uniformBuffer[i] = NULL;
		}
	}

	// initialize extra uniform textures
	if (sDef->uTexList != NULL) {
		for (int i = 0; i < sDef->uTexTotal; i++) {
			GLint uLoc = glGetUniformLocation(prog, sDef->uTexList[i]);
			glUniform1i(uLoc, i);
		}
	}
	/*if (sDef->uTexList == NULL) { 
		shader->uTexList = NULL;
	} else {
		shader->uTexList = (GLint*) malloc(sizeof(GLint) * sDef->uTexTotal);
		for (int i = 0; i < sDef->uTexTotal; i++) {
			shader->uTexList[i] = glGetUniformLocation(prog, sDef->uTexList[i]);
			glUniform1i(shader->uTexList[i], i);
		}
	}*/

	return shader;
}

void delShader(shader_t* s) {
	free(s);
}

	/* shader uniform assignment */

void setUniformsShader(shader_t* shader) {
	if (shader->uniformList == NULL) return;
	for (int i = 0; i < shader->uniformTotal; i++) {
		shader_uniform_t* uniform = &shader->uniformList[i];
		//gl_val_t* v = ((gl_val_t**) uData->data)[i];
		gl_val_t* v = shader->uniformBuffer[i];
		if (v == NULL) continue;
		if (uniform->uDef.glType == GL_FLOAT_MAT4) {
			glUniformMatrix4fv(uniform->loc, 1, GL_FALSE, ((gl_mat4_val_t*) v)->mat);
		}
	}
}

/*void setUniformShader(shader_t* shader, int32_t i, gc_val_t* uniform) {
	if (shader->uniformList == NULL) return;
	shader_uniform_t* uniform = &shader->uniformList[i];
	gl_val_t* v = ((gl_val_t**) uData->data)[i];
	if (uniform->uDef.glType == GL_FLOAT_MAT4) {
		glUniformMatrix4fv(uniform->loc, 1, GL_FALSE, ((gl_mat4_val_t*) v)->mat);
	}
}*/

	/* shader draw function */

void drawDataShader(shader_t* shader, mesh_t* mesh, tex_array_t* texArr, int total, void* data) {
	glBindVertexArray(shader->vao);

	glBindBuffer(GL_ARRAY_BUFFER, shader->vertexVBO);
	glBufferData(GL_ARRAY_BUFFER, mesh->vertexTotal * sizeof(vertex_t), mesh->data, GL_DYNAMIC_DRAW);

	glBindBuffer(GL_ARRAY_BUFFER, shader->instVBO);
	glBufferData(GL_ARRAY_BUFFER, total * shader->instSize, data, GL_DYNAMIC_DRAW);

	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D_ARRAY, texArr->id);
	glUniform1i(shader->uSampler, 0);
	if (shader->uTotal != -1) glUniform1i(shader->uTotal, total);

	glDrawArraysInstanced(GL_TRIANGLES, 0, mesh->vertexTotal, total);
}	

#endif /* SULF_SHADER_H */