#ifndef GLYPH_H
#define GLYPH_H

	/* glyph datatype */

typedef int8_t GLYPH_TYPE;
enum { G_NOP = 0, G_BOX = 1, G_IMAGE = 2, G_SPRITE = 3 };

extern const int8_t C_NOP;
extern const int8_t C_BOX;
extern const int8_t C_IMAGE;
extern const int8_t C_SPRITE;

const int8_t C_NOP = G_NOP;
const int8_t C_BOX = G_BOX;
const int8_t C_IMAGE = G_IMAGE;
const int8_t C_SPRITE = G_SPRITE;

typedef struct glyph {
	GLYPH_TYPE type;
	int8_t raw[31];
} glyph_t;

typedef struct box_glyph {
	GLYPH_TYPE type;
	int32_t x;
	int32_t y;
	int32_t w;
	int32_t h;
} box_glyph_t;

typedef struct sprite_glyph {
	GLYPH_TYPE type;
	int32_t x;
	int32_t y;
	sprite_t* spritePtr;
	int32_t frame;
} sprite_glyph_t;

	/* add to render list */

void addGlyphRList(renderList_t* rl, glyph_t* g) {
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
		dat->aPos[2] = (float) -lenRList(rl);
		dat->aSize[0] = (float) spritePtr->tw;
		dat->aSize[1] = (float) spritePtr->th;
		dat->aTexId = imagePtr->index;
		dat->aTexUVPos[0] = spritePtr->fx + ((sg->frame % spritePtr->spanWidth) * spritePtr->fw);
		dat->aTexUVPos[1] = spritePtr->fy + ((sg->frame / spritePtr->spanWidth) * spritePtr->fh);
		dat->aTexUVSize[0] = spritePtr->fw;
		dat->aTexUVSize[1] = spritePtr->fh;
	}
}


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

#endif