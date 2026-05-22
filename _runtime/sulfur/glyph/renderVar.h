#ifndef RENDER_VAR_H
#define RENDER_VAR_H

	/*
		supported attr/uniform types:
		- GL_UNSIGNED_INT (1 + v)
		- GL_FLOAT (1-4 + v)
		- GL_FLOAT_MAT4
	*/

typedef tag_type C_GL_TYPE;
enum { G_GL_NULL = 0, G_GL_UINT = 1,
	G_GL_FLOAT = 2, G_GL_FLOAT3 = 3, G_GL_FLOAT4 = 4,
	G_GL_MAT4 = 5, G_GL_FLOAT_V3 = 6 };

extern const tag_type C_GL_UINT;
extern const tag_type C_GL_FLOAT;
extern const tag_type C_GL_FLOAT3;
extern const tag_type C_GL_FLOAT4;
extern const tag_type C_GL_MAT4;
extern const tag_type C_GL_FLOAT_V3;

const tag_type C_GL_UINT = G_GL_UINT;
const tag_type C_GL_FLOAT = G_GL_FLOAT;
const tag_type C_GL_FLOAT3 = G_GL_FLOAT3;
const tag_type C_GL_FLOAT4 = G_GL_FLOAT4;
const tag_type C_GL_MAT4 = G_GL_MAT4;
const tag_type C_GL_FLOAT_V3 = G_GL_FLOAT_V3;

GLint readGLType(C_GL_TYPE type) {
	if (type == C_GL_UINT) return GL_UNSIGNED_INT;
	else if (type == C_GL_FLOAT || type == C_GL_FLOAT3 || type == C_GL_FLOAT4) return GL_FLOAT;
	else if (type == C_GL_MAT4) return GL_FLOAT_MAT4;
	else if (type == C_GL_FLOAT_V3) return GL_FLOAT_VEC3;
}

int readGLArity(C_GL_TYPE type) {
	if (type == C_GL_FLOAT3) return 3;
	else if (type == C_GL_FLOAT4) return 4;
	return 1;
}

typedef struct gl_val {
	C_GL_TYPE type;
	int8_t c[GL_VAL_SIZE - sizeof(C_GL_TYPE)];
} gl_val_t;

typedef struct gl_float_val {
	C_GL_TYPE type;
	float f;
} gl_float_val_t;

typedef struct gl_float3_val {
	C_GL_TYPE type;
	float f1;
	float f2;
	float f3;
} gl_float3_val_t;

typedef struct gl_float4_val {
	C_GL_TYPE type;
	float f1;
	float f2;
	float f3;
	float f4;
} gl_float4_val_t;

typedef struct gl_fv3_val {
	C_GL_TYPE type;
	gc_array_t* arr;
} gl_fv3_val_t;

typedef struct gl_mat4_val {
	C_GL_TYPE type;
	float* mat;
} gl_mat4_val_t;

void clearRenderVar(gl_val_t* v) {
	if (v->type == C_GL_MAT4) {
		free(((gl_mat4_val_t*) v)->mat);
	} else if (v->type == C_GL_FLOAT_V3) {
		raw_free_array(((gl_fv3_val_t*) v)->arr);
	}
}

void copyRenderVar(gl_val_t* dst, gl_val_t* src) {
	// free if changing type
	C_GL_TYPE oldType = dst->type;
	if (src->type != oldType) {
		clearRenderVar(dst);
	}
	dst->type = src->type;
	// copy (init memory if changing type)
	if (src->type == C_GL_FLOAT) {
		((gl_float_val_t*) dst)->f = ((gl_float_val_t*) src)->f;
	} else if (src->type == C_GL_FLOAT3) {
		gl_float3_val_t* a = (gl_float3_val_t*) dst;
		gl_float3_val_t* b = (gl_float3_val_t*) src;
		a->f1 = b->f1;
		a->f2 = b->f2;
		a->f3 = b->f3;
	} else if (src->type == C_GL_FLOAT4) {
		gl_float4_val_t* a = (gl_float4_val_t*) dst;
		gl_float4_val_t* b = (gl_float4_val_t*) src;
		a->f1 = b->f1;
		a->f2 = b->f2;
		a->f3 = b->f3;
		a->f4 = b->f4;
	} else if (src->type == C_GL_MAT4) {
		gl_mat4_val_t* m = (gl_mat4_val_t*) dst;
		if (oldType != src->type) m->mat = rawNewMat4();
		memcpy(m->mat, ((gl_mat4_val_t*) src)->mat, sizeof(float) * 16);
	} else if (src->type == C_GL_FLOAT_V3) {
		gl_fv3_val_t* a = (gl_fv3_val_t*) dst;
		gl_fv3_val_t* b = (gl_fv3_val_t*) src;
		if (oldType != src->type) a->arr = raw_alloc_1d_array(sizeof(float), b->arr->size);
		memcpy(a->arr->data, b->arr->data, sizeof(float) * b->arr->size);
	} else if (src->type != G_GL_NULL) {
		exit_log("Encountered unknown render variable type during copy.", "");
	}
}

size_t sizeofRenderVar(gl_val_t* v) {
	if (v->type == C_GL_FLOAT) return sizeof(float);
	else if (v->type == C_GL_FLOAT3) return sizeof(float) * 3;
	else if (v->type == C_GL_FLOAT4) return sizeof(float) * 4;
	else if (v->type == C_GL_MAT4) return sizeof(float) * 16;
	else if (v->type == C_GL_FLOAT_V3) {
		exit_log("Encountered vector type while reading size (vector types invalid for attribute data).", "");
	}
	exit_log("Encountered unknown render variable type while reading size.", "");
}

void writeRenderVar(void* dat, gl_val_t* v) {
	if (v->type == C_GL_FLOAT) {
		*((float*) dat) = ((gl_float_val_t*) v)->f;
	} else if (v->type == C_GL_FLOAT3) {
		float* a = (float*) dat;
		gl_float3_val_t* b = (gl_float3_val_t*) v;
		a[0] = b->f1;
		a[1] = b->f2;
		a[2] = b->f3;
	} else if (v->type == C_GL_FLOAT4) {
		float* a = (float*) dat;
		gl_float4_val_t* b = (gl_float4_val_t*) v;
		a[0] = b->f1;
		a[1] = b->f2;
		a[2] = b->f3;
		a[3] = b->f4;
	} else if (v->type == C_GL_MAT4) {
		memcpy(dat, ((gl_mat4_val_t*) v)->mat, sizeof(float) * 16);	
	} else if (v->type == C_GL_FLOAT_V3) {
		exit_log("Encountered vector type while writing (vector types invalid for attribute data).", "");
	} else {
		exit_log("Encountered unknown render variable type while writing.", "");
	}
}

	/*
		render variable pool:
			list of render variables that automatically increases in size when needed
	*/

typedef struct gl_var_list {
	int32_t total;
	gl_val_t* list;
} gl_var_list_t;

gl_var_list_t* newRenderVarList(int8_t strict) {
	gl_var_list_t* vl = (gl_var_list_t*) malloc(sizeof(gl_var_list_t));
	vl->total = strict ? 0 : 10;
	vl->list = malloc(sizeof(gl_val_t) * vl->total);
	for (int i = 0; i < vl->total; i++) {
		vl->list[i].type = G_GL_NULL;
	}
	return vl;
}

void addRenderVar(gl_var_list_t* vl, int32_t i, gl_val_t* v) {
	if (i >= vl->total) {
		vl->list = realloc(vl->list, sizeof(gl_val_t) * (i + 1));
		for (int k = vl->total; k <= i; k++) {
			vl->list[k].type = G_GL_NULL;
		}
		vl->total = i + 1;
	}
	copyRenderVar(&vl->list[i], v);
}

size_t sizeofRVarList(gl_var_list_t* vl) {
	size_t sum = 0;
	for (int i = 0; i < vl->total; i++) {
		sum = sum + sizeofRenderVar(&vl->list[i]);
	}
	return sum;
}

void writeRVarList(int8_t* dst, gl_var_list_t* vl) {
	for (int i = 0; i < vl->total; i++) {
		writeRenderVar(dst, &vl->list[i]);
		dst = dst + sizeofRenderVar(&vl->list[i]);
	}
}

#endif