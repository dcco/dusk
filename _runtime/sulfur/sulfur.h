#ifndef SULFUR_H
#define SULFUR_H

#include <pthread.h>
#include "glfw3.h"
#include "exit_log.h"

	// materials
#include "vertex.h"
#include "mesh.h"
#include "texArray.h"
#include "texImage.h"
#include "sprite.h"

	// shader code
#include "mat4.h"
#include "renderVar.h"
#include "shader.h"
#include "shader2d.h"
#include "drawDat3d.h"

	// glyph / render list code
#include "renderList.h"
#include "r3d.h"
#include "renderTable.h"
#include "renderData.h"
#include "glyph.h"
#include "glyph3d.h"

	// rom + main code
#include "resLoadList.h"
#include "sulfurRom.h"
#include "sf2d.h"
#include "captureCard.h"

#define PIPELINE_DEF 1

const int ZOOM = 2;

void orthoMat(GLfloat* m, float l, float r, float b, float t, float n, float f) {
	float lr = 1.0f / (l - r);
	float bt = 1.0f / (b - t);
	float nf = 1.0f / (n - f);
	m[0] = -2.0f * lr;
	m[1] = 0.0f;
	m[2] = 0.0f;
	m[3] = 0.0f;
	m[4] = 0.0f;
	m[5] = -2.0f * bt;
	m[6] = 0.0f;
	m[7] = 0.0f;
	m[8] = 0.0f;
	m[9] = 0.0f;
	m[10] = 2.0f * nf;
	m[11] = 0.0f;
	m[12] = (l + r) * lr;
	m[13] = (t + b) * bt;
	m[14] = (f + n) * nf;
	m[15] = 1.0f;
}

	/*
		external bindings required for pipeline rendering
	*/

extern void _none_Sys_Sulfur_initShaderFront();
extern void _RenderData_Sys_Sulfur_passRenderVars(renderData_t* rd);

extern void _none_pipeline_initShaderBack();
extern void _RenderData_pipeline_runShader(renderData_t* rd);

	/* main sulfur runtime */

typedef struct sulfur {
	sf2d_t* sf2d;
	cc_t* cc;
	int width;
	int height;
	GLfloat pMat[16];
		/* render thread buffer */
	pthread_mutex_t bufferMutex;
	int8_t dirty;
	r3d_t* r3d;
	renderData_t* back_buffer;
	renderData_t* swap_buffer;
	renderData_t* front_buffer;
		/* media rom */
	sf_rom_t* rom;
} sulfur_t;

sulfur_t* initSulfur(int width, int height) {
	// initialize 2d rendering
	sulfur_t* self = (sulfur_t*) malloc(sizeof(sulfur_t));
	self->sf2d = initSf2d();
	self->cc = initCC(ZOOM, width, height);
	self->width = width;
	self->height = height;
	orthoMat(self->pMat, 0.0f, (float) width, (float) height, 0.0f, -1.0f, 1.0f);
	pthread_mutex_init(&self->bufferMutex, NULL);

	// initialize 2d + 3d render buffers
	#ifdef PIPELINE_DEF
		size_t size3d = sizeof(draw_dat3d_t);
	#else
		size_t size3d = 0;
	#endif
	self->r3d = initR3d();
	self->back_buffer = newRData(sizeof(draw_dat2d_t), size3d);
	self->swap_buffer = newRData(sizeof(draw_dat2d_t), size3d);
	self->front_buffer = newRData(sizeof(draw_dat2d_t), size3d);

	#ifdef PIPELINE_DEF
		_none_pipeline_initShaderBack();
		_none_Sys_Sulfur_initShaderFront();
	#endif
	self->rom = initSfRom();

	// misc init
	glClearColor(0.1f, 0.15f, 0.15f, 1.0f);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);     
	return self;
}

	/*
		currently unused (sulfur exists during the whole runtime lifespan), but exists for posterity
		- currently incomplete bc lazy
	*/
void delSulfur(sulfur_t* self) {
	delSf2d(self->sf2d);
	free(self);
}

	/* rendering / glyph datatype */

void swapBackBuffer(sulfur_t* sulfur) {
	pthread_mutex_lock(&sulfur->bufferMutex);
	// swap render lists
	renderData_t* temp = sulfur->back_buffer;
	sulfur->back_buffer = sulfur->swap_buffer;
	sulfur->swap_buffer = temp;
	clearRData(sulfur->back_buffer);
	// dirty bits
	sulfur->dirty = 1;
	pthread_mutex_unlock(&sulfur->bufferMutex);
}

void _clear(sulfur_t* sulfur) {
	shader_t* shader = sulfur->sf2d->shader;
	glViewport(0, 0, sulfur->width, sulfur->height);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glUseProgram(shader->prog);
	glUniformMatrix4fv(shader->uPers, 1, 0, sulfur->pMat); 
}

int8_t render(sulfur_t* sulfur) {
	// check if buffer is dirty
	int8_t dirty = 0;
	pthread_mutex_lock(&sulfur->bufferMutex);
	if (sulfur->dirty) {
		// swap render lists
		renderData_t* temp = sulfur->front_buffer;
		sulfur->front_buffer = sulfur->swap_buffer;
		sulfur->swap_buffer = temp;
		// dirty bits
		sulfur->dirty = 0;
		if (sulfur->rom->texArr != NULL) dirty = 1;
	}
	pthread_mutex_unlock(&sulfur->bufferMutex);
	if (!dirty) return 0;

	// clear and do initial 2d render
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	_clear(sulfur);
	shader_t* shader = sulfur->sf2d->shader;

	int32_t len = lenRList(&sulfur->front_buffer->list2d);
	drawDataShader(shader, &sulfur->sf2d->defQuad, sulfur->rom->texArr, len, sulfur->front_buffer->list2d.data);
	
	// copy to capture card for future
	copyCC(sulfur->cc);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	
	// pipeline render call
	#ifdef PIPELINE_DEF
		_RenderData_pipeline_runShader(sulfur->front_buffer);
		//glUseProgram(sulfur->sf2d->shader->prog);
	#endif

	// 2d re-render
	//drawDataShader(shader, &sulfur->sf2d->defQuad, sulfur->cc->capArr, 1, (void*) sulfur->cc->data);

	return 1;
}

void updateRom(sulfur_t* sulfur) {
	_updateRom(sulfur->rom);
}

#endif /* SULFUR_H */