#ifndef SULFUR_DATA_H
#define SULFUR_DATA_H

	/*
		functions for converting dusk values into
			data usable in the pipeline
	*/

void* map_gc_array(gc_array_t* list, size_t oldSize, size_t newSize, void (*f)(void* dst, void* src))
{
	if (list->size <= 0) return NULL;
	int32_t total = list->size;
	int8_t* newList = (int8_t*) malloc(newSize * total);
	int8_t* dstPtr = newList;
	int8_t* srcPtr = (int8_t*) list->data;
	for (int i = 0; i < total; i++) {
		f(dstPtr, srcPtr);
		srcPtr = srcPtr + oldSize;
		dstPtr = dstPtr + newSize;
	}
	return (void*) newList;
}

	/* uniforms */

typedef struct raw_enum {
	int8_t type;
} raw_enum_t;

typedef struct raw_uniform_def {
	dusk_string_t* name;
	raw_enum_t* g;
	int32_t arity;
} raw_uniform_def_t;

void read_uniform(void* dst, void* src)
{
	shader_uniform_def_t* uniform = (shader_uniform_def_t*) dst;
	raw_uniform_def_t* rawUniform = *((raw_uniform_def_t**) src);
	uniform->name = &rawUniform->name->start;
	uniform->glType = readGLType(rawUniform->g->type);
	uniform->arity = rawUniform->arity;
}

void read_string(void* dst, void* src)
{
	char** s = (char**) dst;
	dusk_string_t* raw = *((dusk_string_t**) src);
	*s = &raw->start;
}

	/* layers */

void read_layer(void* dst, void* src)
{
	fbo_layer_def_t* layer = (fbo_layer_def_t*) dst;
	raw_enum_t* e = *((raw_enum_t**) src);
	layer->type = e->type;
}

#endif