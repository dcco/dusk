#ifndef SULFUR_BINDINGS_H
#define SULFUR_BINDINGS_H

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#include "sulfur.h"
#include "sulfurData.h"
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

/*
extern void _Glyph3d_Sys_Sulfur_draw(int8_t raw[32])
{
	#ifdef PIPELINE_DEF
		renderTable_t* rt = &sulfur->back_buffer->table3d;
		addGlyph3dRTable(sulfur->r3d, rt, (glyph3d_t*) raw);
	#endif
}*/

extern void _Float_Sys_Sulfur_drawQuadX(float x, float y, float z, sprite_t* spritePtr, int32_t frame)
{
	#ifdef PIPELINE_DEF
		renderTable_t* rt = &sulfur->back_buffer->table3d;
		addQuad3dRTable(sulfur->r3d, rt, 0, x, y, z, spritePtr, frame);
	#endif
}

extern void _Float_Sys_Sulfur_drawQuadY(float x, float y, float z, sprite_t* spritePtr, int32_t frame)
{
	#ifdef PIPELINE_DEF
		renderTable_t* rt = &sulfur->back_buffer->table3d;
		addQuad3dRTable(sulfur->r3d, rt, 1, x, y, z, spritePtr, frame);
	#endif
}

extern void _Float_Sys_Sulfur_drawQuadZ(float x, float y, float z, sprite_t* spritePtr, int32_t frame)
{
	#ifdef PIPELINE_DEF
		renderTable_t* rt = &sulfur->back_buffer->table3d;
		addQuad3dRTable(sulfur->r3d, rt, 2, x, y, z, spritePtr, frame);
	#endif
}

extern void _Float_Sys_Sulfur_drawSprite(float x, float y, float z, sprite_t* spritePtr, int32_t frame)
{
	#ifdef PIPELINE_DEF
		renderTable_t* rt = &sulfur->back_buffer->table3d;
		addSprite3dRTable(sulfur->r3d, rt, x, y, z, spritePtr, frame);
	#endif
}

extern void _none_Sys_Sulfur_waitRom()
{
	waitRom(sulfur->rom);
}

	/* resource bindings */

extern uint32_t _Image_Sys_Sulfur_pixel(tex_image_t* imagePtr, int32_t x, int32_t y)
{
	int i = ((y * imagePtr->width) + x) * 4;
	uint32_t c = imagePtr->data[i] << 24 | imagePtr->data[i + 1] << 16 |
		imagePtr->data[i + 2] << 8 | imagePtr->data[i + 3];
	return c;
}

	/* special pipeline bindings */

#ifdef PIPELINE_DEF
/*
shader_uniform_def_t* convert_uniforms(gc_array_t* uniforms)
{
	shader_uniform_def_t* uniformList = NULL;
	if (uniforms->size > 0) {
		int32_t uniformTotal = uniforms->size;
		uniformList = (shader_uniform_def_t*) malloc(sizeof(shader_uniform_def_t) * uniformTotal);
		raw_uniform_def_t** rawUList = (raw_uniform_def_t**) uniforms->data;
		for (int i = 0; i < uniformTotal; i++) {
			raw_uniform_def_t* rawUniform = rawUList[i];
			uniformList[i].name = &rawUniform->name->start;
			uniformList[i].glType = readGLType(rawUniform->g->type);
			uniformList[i].arity = rawUniform->arity;
		}
	}
	return uniformList;
}*/

extern shader_t* _String_Sys_Sulfur_newShader(dusk_string_t* _vs, dusk_string_t* _fs,
	gc_array_t* attrs, dusk_string_t* _uPMat, gc_array_t* uniforms, gc_array_t* uniformTexs)
{
	// convert uniform list into usable data structure
	int32_t uniformTotal = uniforms->size;
	shader_uniform_def_t* uniformList = map_gc_array(uniforms,
		sizeof(raw_uniform_def_t*), sizeof(shader_uniform_def_t), &read_uniform);

	int32_t uTexTotal = uniformTexs->size;
	char** uniformTexList = map_gc_array(uniformTexs, sizeof(dusk_string_t*), sizeof(char*), &read_string);
	//convert_uniforms(uniforms);

	// read pmat / sampler names
	const char* uPMat = NULL;
	read_string(&uPMat, &_uPMat); 
	if (strcmp(uPMat, "null") == 0) uPMat = NULL;

	const char* uSampler = NULL;
	if (uniformList != NULL) {
		if (strcmp(uniformList[0].name, "null") != 0) uSampler = uniformList[0].name;
	}

	// build shader definition
	const struct shader_def BASE3_DEF = {
		BASE3_ATTR_TOTAL, sizeof(draw_dat3d_t), BASE3_ATTR_LIST,
		uniformTotal, uniformList, uSampler, NULL, uPMat,
		uTexTotal, uniformTexList
	};

	// compile shader
	const char* vs = (char*) (&_vs->start);
	const char* fs = (char*) (&_fs->start);
	shader_t* shader = initShader(vs, fs, &BASE3_DEF);

	// cleanup
	if (uniformList != NULL) free(uniformList);
	if (uniformTexList != NULL) free(uniformTexList);
	return shader;
}
/*
fbo_layer_def_t* convert_layers(gc_array_t* layers)
{
	fbo_layer_def_t* layerList = NULL;
	if (layers->size > 0) {
		int32_t layersTotal = layers->size;
		layerList = (fbo_layer_def_t*) malloc(sizeof(fbo_layer_def_t) * layersTotal);
		raw_enum_t** rawLList = (raw_enum_t**) layers->data;
		for (int i = 0; i < layersTotal; i++) {
			raw_enum_t* rawLayer = rawLList[i];
			layerList[i].type = rawLayer->type;
		}
	}
	return layerList;
}*/

extern frameBuffer_t* _String_Sys_Sulfur_newFrameBuffer(dusk_string_t* _vs, dusk_string_t* _fs, int32_t w, int32_t h,
	gc_array_t* layers, dusk_string_t* _uPMat, gc_array_t* uniforms, gc_array_t* uniformTexs)
{
	// initialize shader + fbo
	shader_t* shader = _String_Sys_Sulfur_newShader(_vs, _fs, NULL, _uPMat, uniforms, uniformTexs);
	fbo_layer_def_t* layerList = map_gc_array(layers, sizeof(tag_type), sizeof(fbo_layer_def_t), &read_layer);
	frameBuffer_t* buffer = newFrameBuffer(shader, w, h, layers->size, layerList);

	// cleanup
	if (layerList != NULL) free(layerList);
	return buffer;
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

extern void _FrameBuffer_Sys_Sulfur_setUniform(void* _fbo, int32_t i, gl_val_t* v)
{
	_Shader_Sys_Sulfur_setUniform(((frameBuffer_t*) _fbo)->shader, i, v);
}

extern void _Shader_Sys_Sulfur_loadTexture(void* _shader, int32_t i, void* _fbo, int32_t j)
{
	shader_t* shader = (shader_t*) _shader;
	frameBuffer_t* fbo = (frameBuffer_t*) _fbo;
	glActiveTexture(GL_TEXTURE0 + i);
	glBindTexture(GL_TEXTURE_2D, fbo->texList[j]);
}

extern void _FrameBuffer_Sys_Sulfur_loadTexture(void* _dst, int32_t i, void* _src, int32_t j)
{
	_Shader_Sys_Sulfur_loadTexture(((frameBuffer_t*) _dst)->shader, i, _src, j);
}

void shader_render(shader_t* shader, renderData_t* rd)
{
	// assign argument uniforms
	if (shader->uPers != -1) glUniformMatrix4fv(shader->uPers, 1, 0, P_MAT_X);
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

extern void _Shader_Sys_Sulfur_render(void* _shader, renderData_t* rd)
{
	// enable shader
	shader_t* shader = (shader_t*) _shader;
	glUseProgram(shader->prog);
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	// clear shader
	glViewport(0, 0, sulfur->width, sulfur->height);
	glClearColor(0.1f, 0.15f, 0.15f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	// render
	shader_render(_shader, rd);
}

extern void _FrameBuffer_Sys_Sulfur_render(void* _fbo, renderData_t* rd)
{
	// enable frame buffer
	frameBuffer_t* fbo = (frameBuffer_t*) _fbo;
	glUseProgram(fbo->shader->prog);
	glBindFramebuffer(GL_FRAMEBUFFER, fbo->fbo);
	// clear frame buffer
	glViewport(0, 0, fbo->width, fbo->height);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	// render
	shader_render(fbo->shader, rd);
	//_Shader_Sys_Sulfur_render(((frameBuffer_t*) _fbo)->shader, rd);
}

void shader_renderQuad(shader_t* shader)
{
	// assign argument uniforms
	glUniformMatrix4fv(shader->uPers, 1, 0, P_MAT_X);
	setUniformsShader(shader);

	// render draw data
	drawDataShader((shader_t*) shader, sulfur->screenQuad, sulfur->rom->texArr, 1, sulfur->screenDrawDat);
}

extern void _Shader_Sys_Sulfur_renderQuad(void* _shader)
{
	// enable shader
	shader_t* shader = (shader_t*) _shader;
	glUseProgram(shader->prog);
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	// clear shader
	glViewport(0, 0, sulfur->width, sulfur->height);
	glClearColor(0.1f, 0.15f, 0.15f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	// render
	shader_renderQuad(_shader);
}

extern void _FrameBuffer_Sys_Sulfur_renderQuad(void* _fbo)
{
	// enable frame buffer
	frameBuffer_t* fbo = (frameBuffer_t*) _fbo;
	glUseProgram(fbo->shader->prog);
	glBindFramebuffer(GL_FRAMEBUFFER, fbo->fbo);
	// clear frame buffer
	glViewport(0, 0, fbo->width, fbo->height);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	// render
	shader_renderQuad(fbo->shader);
}

extern renderData_t* _none_Sys_Sulfur_renderData() {
	return sulfur->back_buffer;
}

extern void _RenderData_Sys_Sulfur_get(gl_val_t* ret, renderData_t* rd, int32_t i) {
	if (i >= rd->varTotal) {
		ret->type = G_GL_NULL;
		return;
	}
	*ret = rd->varList[i];
}

extern void _RenderData_Sys_Sulfur_set(renderData_t* rd, int32_t i, gl_val_t* v) {
	return addVarRData(rd, i, v);
}

#endif /* PIPELINE_DEF */

#endif /* SULFUR_BINDINGS_H */
