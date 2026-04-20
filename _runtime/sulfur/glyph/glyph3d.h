#ifndef GLYPH_3D_H
#define GLYPH_3D_H

	/* glyph datatype */

typedef int8_t GLYPH_3D_TYPE;
enum { G3_NOP = 0, G3_TEST = 1 };

extern const int8_t C3_NOP;
extern const int8_t C3_TEST;

const int8_t C3_NOP = G3_NOP;
const int8_t C3_TEST = G3_TEST;

typedef struct glyph3d {
	GLYPH_3D_TYPE type;
	int8_t raw[31];
} glyph3d_t;

typedef struct test_glyph3d {
	GLYPH_TYPE type;
	float x;
	float y;
	float z;
	sprite_t* spritePtr;
	int32_t frame;
} test_glyph3d_t;

	/* add to render list */

void addGlyph3dRList(renderList_t* rl, glyph3d_t* g) {
	if (g->type == G3_TEST) {
		test_glyph3d_t* tg = (test_glyph3d_t*) g;
		if (tg->spritePtr == NULL) return;
		sprite_t* spritePtr = (sprite_t*) tg->spritePtr;
		if (spritePtr->image == NULL) return;
		tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
		// write into render list
		draw_dat3d_t* dat = (draw_dat3d_t*) nextRList(rl);
		dat->aPos[0] = tg->x;
		dat->aPos[1] = tg->y;
		dat->aPos[2] = tg->z;
		dat->aTexId = imagePtr->index;
		dat->aTexUVPos[0] = spritePtr->fx + ((tg->frame % spritePtr->spanWidth) * spritePtr->fw);
		dat->aTexUVPos[1] = spritePtr->fy + ((tg->frame / spritePtr->spanWidth) * spritePtr->fh);
		dat->aTexUVSize[0] = spritePtr->fw;
		dat->aTexUVSize[1] = spritePtr->fh;
	}
}

#endif