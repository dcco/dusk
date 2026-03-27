#ifndef SULFUR_H
#define SULFUR_H

#include <string.h>
#include <pthread.h>
#include "glfw3.h"
#include "exit_log.h"

#include "buffer.h"
#include "mesh.h"
#include "texArray.h"
#include "texImage.h"
#include "sprite.h"
#include "res_load_list.h"
#include "shader.h"
#include "sf2d.h"
#include "glyph.h"

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
	int width;
	int height;
	GLfloat pMat[16];
		/* permanent resources */
	tex_array_t* texArr;
		/* render thread buffer */
	pthread_mutex_t bufferMutex;
	int8_t dirty;
	glyphList_t* back_buffer;
	glyphList_t* swap_buffer;
	glyphList_t* front_buffer;
		/* resource loading buffer */
	pthread_mutex_t loadMutex;
	resLoadList_t* res_list;
} sulfur_t;

sulfur_t* initSulfur(int width, int height) {
	// initialize 2d rendering
	sulfur_t* self = (sulfur_t*) malloc(sizeof(sulfur_t));
	self->sf2d = initSf2d();
	self->width = width;
	self->height = height;
	orthoMat(self->pMat, 0.0f, (float) width, (float) height, 0.0f, -1.0f, 1.0f);
	self->texArr = NULL;
	pthread_mutex_init(&self->bufferMutex, NULL);
	self->back_buffer = newGList();
	self->swap_buffer = newGList();
	self->front_buffer = newGList();
	pthread_mutex_init(&self->loadMutex, NULL);
	self->res_list = newResList();

	// misc init
	glClearColor(0.1f, 0.15f, 0.15f, 1.0f);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glDisable(GL_DEPTH_TEST);
	return self;
}

	/* currently unused (sulfur exists during the whole runtime lifespan), but exists for posterity */
void delSulfur(sulfur_t* self) {
	delSf2d(self->sf2d);
	free(self);
}

	/* rendering / glyph datatype */

void _clear(sulfur_t* sulfur) {
	shader_t* shader = sulfur->sf2d->shader;
	glUseProgram(shader->prog);
	glViewport(0, 0, sulfur->width, sulfur->height);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glUniformMatrix4fv(shader->uPers, 1, 0, sulfur->pMat); 
}

void swapBackBuffer(sulfur_t* sulfur) {
	pthread_mutex_lock(&sulfur->bufferMutex);
	glyphList_t* temp = sulfur->back_buffer;
	sulfur->back_buffer = sulfur->swap_buffer;
	sulfur->swap_buffer = temp;
	clearGList(sulfur->back_buffer);
	sulfur->dirty = 1;
	pthread_mutex_unlock(&sulfur->bufferMutex);
}

void render(GLfloat *oMat, sulfur_t* sulfur) {
	int8_t dirty = 0;
	pthread_mutex_lock(&sulfur->bufferMutex);
	if (sulfur->dirty) {
		glyphList_t* temp = sulfur->front_buffer;
		sulfur->front_buffer = sulfur->swap_buffer;
		sulfur->swap_buffer = temp;
		sulfur->dirty = 0;
		dirty = 1;
	}
	pthread_mutex_unlock(&sulfur->bufferMutex);
	if (!dirty) return;

	_clear(sulfur);
	shader_t* shader = sulfur->sf2d->shader;

	int32_t len = lenGList(sulfur->front_buffer);
	for (int i = 0; i < len; i++) {
		glyph_t* g = getGList(sulfur->front_buffer, i);
		if (g->type == C_BOX) {
			box_glyph_t* bg = (box_glyph_t*) g;
			//mesh_t* mesh = tempBoxSf2d(sulfur->sf2d, bg->x, bg->y, bg->w, bg->h);
			//drawMeshShader(shader, oMat, mesh, &sulfur->sf2d->defTBuf, &sulfur->sf2d->defTex);
		} else if (g->type == C_SPRITE) {
			sprite_glyph_t* sg = (sprite_glyph_t*) g;
			if (sg->spritePtr == NULL) continue;
			sprite_t* spritePtr = (sprite_t*) sg->spritePtr;
			if (spritePtr->image == NULL) continue;
			tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
			//mesh_t* mesh = tempBoxSf2d(sulfur->sf2d, 0, 0, imagePtr->width, imagePtr->height);
			//drawMeshShader(shader, oMat, mesh, &sulfur->sf2d->defTBuf, imagePtr);
		}
	}
}

void flushResList(sulfur_t* sulfur) {
	pthread_mutex_lock(&sulfur->loadMutex);
	resLoadItem_t* nextRes = takeResList(sulfur->res_list);
	if (nextRes != NULL) {
		if (nextRes->type == R_IMAGE) {
			tex_image_t* imageData = (tex_image_t*) malloc(sizeof(tex_image_t));
			//initTexImage(imageData, nextRes->a, nextRes->b, (char*) nextRes->xArgs);
			*nextRes->storePtr = (void*) imageData;
		} else if (nextRes->type == R_SPRITE) {
			int* i_args = nextRes->iArgs;
			tex_image_t* imgPtr = *((tex_image_t**) nextRes->xArgs);
			sprite_t* sprite = NULL;
			//sprite_t* sprite = initSprite(imgPtr, i_args[0], i_args[1], i_args[2], 1, 1);
			*nextRes->storePtr = (void*) sprite;
		}
	}
	pthread_mutex_unlock(&sulfur->loadMutex);
}

#endif /* SULFUR_H */