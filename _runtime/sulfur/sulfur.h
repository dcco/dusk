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
#include "shader.h"
#include "shader2d.h"

	// glyph / render list code
#include "renderList.h"
#include "glyph.h"

	// rom + main code
#include "resLoadList.h"
#include "sulfurRom.h"
#include "sf2d.h"
#include "captureCard.h"

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
	renderList_t* back_buffer;
	renderList_t* swap_buffer;
	renderList_t* front_buffer;
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
	self->back_buffer = newRList(sizeof(draw_dat2d_t));
	self->swap_buffer = newRList(sizeof(draw_dat2d_t));
	self->front_buffer = newRList(sizeof(draw_dat2d_t));
	self->rom = initSfRom();

	// misc init
	glClearColor(0.1f, 0.15f, 0.15f, 1.0f);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LESS);     
	return self;
}

	/* currently unused (sulfur exists during the whole runtime lifespan), but exists for posterity */
void delSulfur(sulfur_t* self) {
	delSf2d(self->sf2d);
	free(self);
}

	/* rendering / glyph datatype */

void swapBackBuffer(sulfur_t* sulfur) {
	pthread_mutex_lock(&sulfur->bufferMutex);
	renderList_t* temp = sulfur->back_buffer;
	sulfur->back_buffer = sulfur->swap_buffer;
	sulfur->swap_buffer = temp;
	clearRList(sulfur->back_buffer);
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
		renderList_t* temp = sulfur->front_buffer;
		sulfur->front_buffer = sulfur->swap_buffer;
		sulfur->swap_buffer = temp;
		sulfur->dirty = 0;
		if (sulfur->rom->texArr != NULL) dirty = 1;
	}
	pthread_mutex_unlock(&sulfur->bufferMutex);
	if (!dirty) return 0;

	// clear and do initial 2d render
	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	_clear(sulfur);
	shader_t* shader = sulfur->sf2d->shader;

	int32_t len = lenRList(sulfur->front_buffer);
	drawDataShader(shader, &sulfur->sf2d->defQuad, sulfur->rom->texArr, len, sulfur->front_buffer->data);
	
	// copy to capture card and re-render
	copyCC(sulfur->cc);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	drawDataShader(shader, &sulfur->sf2d->defQuad, sulfur->cc->capArr, 1, (void*) sulfur->cc->data);

	return 1;
}

void updateRom(sulfur_t* sulfur) {
	_updateRom(sulfur->rom);
}

#endif /* SULFUR_H */