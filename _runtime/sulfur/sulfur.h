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
#include "shader2d.h"
#include "sf2d.h"
#include "captureCard.h"
#include "glyph.h"
#include "renderList.h"

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
		/* permanent resources */
	tex_array_t* texArr;
		/* render thread buffer */
	pthread_mutex_t bufferMutex;
	int8_t dirty;
	renderList_t* back_buffer;
	renderList_t* swap_buffer;
	renderList_t* front_buffer;
		/* resource loading buffer */
	pthread_mutex_t loadMutex;
	resLoadList_t* res_list;
} sulfur_t;

sulfur_t* initSulfur(int width, int height) {
	// initialize 2d rendering
	sulfur_t* self = (sulfur_t*) malloc(sizeof(sulfur_t));
	self->sf2d = initSf2d();
	self->cc = initCC(ZOOM, width, height);
	self->width = width;
	self->height = height;
	orthoMat(self->pMat, 0.0f, (float) width, (float) height, 0.0f, -1.0f, 1.0f);
	self->texArr = NULL;
	pthread_mutex_init(&self->bufferMutex, NULL);
	self->back_buffer = newRList(sizeof(draw_dat2d_t));
	self->swap_buffer = newRList(sizeof(draw_dat2d_t));
	self->front_buffer = newRList(sizeof(draw_dat2d_t));
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

void swapBackBuffer(sulfur_t* sulfur) {
	pthread_mutex_lock(&sulfur->bufferMutex);
	renderList_t* temp = sulfur->back_buffer;
	sulfur->back_buffer = sulfur->swap_buffer;
	sulfur->swap_buffer = temp;
	clearRList(sulfur->back_buffer);
	sulfur->dirty = 1;
	pthread_mutex_unlock(&sulfur->bufferMutex);
}

void addGlyph(sulfur_t* sulfur, glyph_t* g) {
	renderList_t* rl = sulfur->back_buffer;
	if (g->type == C_SPRITE) {
		// obtain sprite + image data
		sprite_glyph_t* sg = (sprite_glyph_t*) g;
		if (sg->spritePtr == NULL) return;
		sprite_t* spritePtr = (sprite_t*) sg->spritePtr;
		if (spritePtr->image == NULL) return;
		tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
		// write into render list
		draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
		dat->aPos[0] = (float) sg->x;
		dat->aPos[1] = (float) sg->y;
		dat->aSize[0] = (float) spritePtr->tw;
		dat->aSize[1] = (float) spritePtr->th;
		dat->aTexId = imagePtr->index;
		dat->aTexUVPos[0] = spritePtr->fx + ((sg->frame % spritePtr->spanWidth) * spritePtr->fw);
		dat->aTexUVPos[1] = spritePtr->fy + ((sg->frame / spritePtr->spanWidth) * spritePtr->fh);
		dat->aTexUVSize[0] = spritePtr->fw;
		dat->aTexUVSize[1] = spritePtr->fh;
	}
}

void _clear(sulfur_t* sulfur) {
	shader_t* shader = sulfur->sf2d->shader;
	glUseProgram(shader->prog);
	glViewport(0, 0, sulfur->width, sulfur->height);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glUniformMatrix4fv(shader->uPers, 1, 0, sulfur->pMat); 
}

void render(GLfloat *oMat, sulfur_t* sulfur) {
	// check if buffer is dirty
	int8_t dirty = 0;
	pthread_mutex_lock(&sulfur->bufferMutex);
	if (sulfur->dirty) {
		renderList_t* temp = sulfur->front_buffer;
		sulfur->front_buffer = sulfur->swap_buffer;
		sulfur->swap_buffer = temp;
		sulfur->dirty = 0;
		if (sulfur->texArr != NULL) dirty = 1;
	}
	pthread_mutex_unlock(&sulfur->bufferMutex);
	if (!dirty) return;

	// clear and do initial 2d render
	_clear(sulfur);
	shader_t* shader = sulfur->sf2d->shader;

	int32_t len = lenRList(sulfur->front_buffer);
	drawDataShader(shader, &sulfur->sf2d->defQuad, sulfur->texArr, len, sulfur->front_buffer->data);

	// copy to capture card and re-render
	copyCC(sulfur->cc);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	drawDataShader(shader, &sulfur->sf2d->defQuad, sulfur->cc->capArr, 1, (void*) sulfur->cc->data);

	/*for (int i = 0; i < len; i++) {
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
			//drawDataShader(shader, mesh, sulfur->texArr, total, data) {
		}
	}*/
}

void flushResList(sulfur_t* sulfur) {
	// initialize sulfur's texture array if applicable
	if (sulfur->texArr == NULL && sulfur->res_list->meta.init) {
		resListMeta_t* meta = &sulfur->res_list->meta;
		sulfur->texArr = initTexArray(meta->total, meta->width, meta->height);
	}
	// load remaining resources
	pthread_mutex_lock(&sulfur->loadMutex);
	resLoadItem_t* nextRes = takeResList(sulfur->res_list);
	if (nextRes != NULL) {
		if (nextRes->type == R_IMAGE) {
			tex_image_t* imageData = (tex_image_t*) malloc(sizeof(tex_image_t));
			initTexImage(sulfur->texArr, imageData, nextRes->storeId, (char*) nextRes->xArgs);
			//initTexImage(imageData, nextRes->a, nextRes->b, (char*) nextRes->xArgs);
			*nextRes->storePtr = (void*) imageData;
		} else if (nextRes->type == R_SPRITE) {
			int* i_args = nextRes->iArgs;
			tex_image_t* imgPtr = *((tex_image_t**) nextRes->xArgs);
			sprite_t* sprite = initSprite(imgPtr, i_args[0], i_args[1], i_args[2], 1, 1);
			*nextRes->storePtr = (void*) sprite;
		}
	}
	pthread_mutex_unlock(&sulfur->loadMutex);
}

#endif /* SULFUR_H */