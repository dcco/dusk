#ifndef SULFUR_BINDINGS_H
#define SULFUR_BINDINGS_H

#include "sulfur.h"
#include "initRom.h"

sulfur_t* sulfur = NULL;

	/* main bindings */

extern void _Glyph_Sys_Sulfur_draw(int8_t raw[32])
{
	renderList_t* rl = &sulfur->back_buffer->list2d;
	addGlyphRList(rl, (glyph_t*) raw);
}

extern void _none_Sys_Sulfur_refresh()
{
	swapBackBuffer(sulfur);
}

extern void _Glyph3d_Sys_Sulfur_draw(int8_t raw[32])
{
	#ifdef PIPELINE_DEF
		renderTable_t* rt = &sulfur->back_buffer->table3d;
		addGlyph3dRTable(sulfur->r3d, rt, (glyph3d_t*) raw);
	#endif
}

	/* special pipeline bindings */

#ifdef PIPELINE_DEF

typedef struct raw_gltype {
	int8_t type;
} raw_gltype_t;

typedef struct raw_uniform_def {
	dusk_string_t* name;
	raw_gltype_t* g;
	int32_t arity;
} raw_uniform_def_t;

extern void* _String_Sys_Sulfur_newShader(dusk_string_t* _vs, dusk_string_t* _fs, gc_array_t* attrs, gc_array_t* uniforms)
{
	// convert uniform list into usable data structure
	int32_t uniformTotal = 0;
	shader_uniform_def_t* uniformList = NULL;
	if (uniforms->size > 0) {
		uniformTotal = uniforms->size;
		uniformList = (shader_uniform_def_t*) malloc(sizeof(shader_uniform_def_t) * uniformTotal);
		raw_uniform_def_t** rawUList = (raw_uniform_def_t**) uniforms->data;
		for (int i = 0; i < uniformTotal; i++) {
			raw_uniform_def_t* rawUniform = rawUList[i];
			uniformList[i].name = &rawUniform->name->start;
			uniformList[i].glType = readGLType(rawUniform->g->type);
			uniformList[i].arity = rawUniform->arity;
		}
	}

	// build shader definition
	const struct shader_def BASE3_DEF = {
		6, sizeof(draw_dat3d_t), BASE3_ATTR_LIST,
		uniformTotal, uniformList, "uSampler", NULL, "uPMat"
	};

	// compile shader
	const char* vs = (char*) (&_vs->start);
	const char* fs = (char*) (&_fs->start);
	shader_t* shader = initShader(vs, fs, &BASE3_DEF);

	// cleanup
	if (uniformList != NULL) free(uniformList);
	return (void*) shader;
}

float P_MAT_X[16] = {
    1.3333333f, 0.0f,        0.0f,  0.0f,
    0.0f,       -2.0f,        0.0f,  0.0f,
    0.0f,       0.0f,       -1.0020020f, -1.0f,
    0.0f,       0.0f,       -0.2002002f,  0.0f
};

extern void _Shader_Sys_Sulfur_setUniform(void* _shader, int32_t i, gl_val_t* v)
{
	shader_t* shader = (shader_t*) _shader;
	if (shader->uniformBuffer == NULL || i < 0 || i >= shader->uniformTotal) return;
	shader->uniformBuffer[i] = v;
}

extern void _Shader_Sys_Sulfur_render(void* _shader, renderData_t* rd)
{
	// enable shader
	shader_t* shader = (shader_t*) _shader;
	glUseProgram(shader->prog);
	glUniformMatrix4fv(shader->uPers, 1, 0, P_MAT_X);
	// assign argument uniforms
	setUniformsShader(shader);

	// render draw data
	renderTable_t* rt = &rd->table3d;
	renderNode_t* rNode = rt->activeHead;
	while (rNode != NULL) {
		renderList_t* rl = &rNode->list;
		int32_t len = lenRList(rl);
		if (rNode->mesh != NULL) drawDataShader((shader_t*) shader, rNode->mesh, sulfur->rom->texArr, len, rl->data);
		rNode = rNode->next;
	}
}

extern renderData_t* _none_Sys_Sulfur_renderData() {
	return sulfur->back_buffer;
}

extern gl_val_t* _RenderData_Sys_Sulfur_get(renderData_t* rd, int32_t i) {
	return &rd->varList[i];
}

extern void _RenderData_Sys_Sulfur_set(renderData_t* rd, int32_t i, gl_val_t* v) {
	return addVarRData(rd, i, v);
}

#endif /* PIPELINE_DEF */

#endif /* SULFUR_BINDINGS_H */
