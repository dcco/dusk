#ifndef SULFUR_BINDINGS_H
#define SULFUR_BINDINGS_H

#include "sulfur.h"
#include "initRom.h"

sulfur_t* sulfur = NULL;

	/* main bindings */

extern void _Glyph_Sys_Sulfur_draw(int8_t raw[32])
{
	renderList_t* rl = sulfur->back_buffer;
	addGlyphRList(rl, (glyph_t*) raw);
}

extern void _none_Sys_Sulfur_refresh()
{
	swapBackBuffer(sulfur);
}

extern void _Glyph3d_Sys_Sulfur_draw(int8_t raw[32])
{
	renderList_t* rl = sulfur->back_buffer + 1;
	addGlyph3dRList(rl, (glyph3d_t*) raw);
}

	/* special pipeline bindings */

#ifdef PIPELINE_DEF

extern void* _String_Sys_Sulfur_newShader(dusk_string_t* _vs, dusk_string_t* _fs, gc_array_t* args)
{
	const char* vs = (char*) (&_vs->start);
	const char* fs = (char*) (&_fs->start);

	const struct shader_def BASE3_DEF = {
		6, sizeof(draw_dat3d_t), BASE3_ATTR_LIST, "uSampler", NULL, "uPMat"
	};

	shader_t* shader = initShader(vs, fs, &BASE3_DEF);
	return (void*) shader;
}

float P_MAT_X[16] = {
    1.3333333f, 0.0f,        0.0f,  0.0f,
    0.0f,       2.0f,        0.0f,  0.0f,
    0.0f,       0.0f,       -1.0020020f, -1.0f,
    0.0f,       0.0f,       -0.2002002f,  0.0f
};

extern void _Shader_Sys_Sulfur_render(void* _shader, void* rl)
{
	// enable shader
	printf("shader ptr: %x\n", _shader);
	shader_t* shader = *((shader_t**) _shader);
	glUseProgram(shader->prog);
	printf("%d\n", shader->prog);
	
	glUniformMatrix4fv(shader->uPers, 1, 0, P_MAT_X); 

	// render draw data
	renderList_t* r_buffer = sulfur->front_buffer + 1;
	int32_t len = lenRList(r_buffer);
	drawDataShader((shader_t*) shader, &sulfur->sf2d->defQuad, sulfur->rom->texArr, len, r_buffer->data);
}

#endif

#endif
