#ifndef RENDER_VAR_H
#define RENDER_VAR_H

	/*
		supported attr/uniform types:
		- GL_UNSIGNED_INT (1 + v)
		- GL_FLOAT (1-4 + v)
		- GL_FLOAT_MAT4
	*/

typedef int8_t C_GL_TYPE;
enum { G_GL_NULL = 0, G_GL_UINT = 1, G_GL_FLOAT = 2, G_GL_MAT4 = 3 };

extern const int8_t C_GL_UINT;
extern const int8_t C_GL_FLOAT;
extern const int8_t C_GL_MAT4;

const int8_t C_GL_UINT = G_GL_UINT;
const int8_t C_GL_FLOAT = G_GL_FLOAT;
const int8_t C_GL_MAT4 = G_GL_MAT4;

GLint readGLType(C_GL_TYPE type) {
	if (type == C_GL_UINT) return GL_UNSIGNED_INT;
	else if (type == C_GL_FLOAT) return GL_FLOAT;
	else if (type == C_GL_MAT4) return GL_FLOAT_MAT4;
}

typedef struct gl_val {
	int8_t type;
	int8_t c[15];
} gl_val_t;

typedef struct gl_float_val {
	int8_t type;
	float f;
} gl_float_val_t;

typedef struct gl_mat4_val {
	int8_t type;
	float* mat;
} gl_mat4_val_t;

void copyRenderVar(gl_val_t* dst, gl_val_t* src) {
	dst->type = src->type;
	if (src->type == C_GL_FLOAT) {
		((gl_float_val_t*) dst)->f = ((gl_float_val_t*) src)->f;
	} else if (src->type == C_GL_MAT4) {
		((gl_mat4_val_t*) dst)->mat = rawCopyMat4(((gl_mat4_val_t*) src)->mat);
	}
}

#endif