#ifndef GLYPH_3D_H
#define GLYPH_3D_H

	/* glyph datatype */

typedef int8_t GLYPH_3D_TYPE;
enum { G3_NOP = 0, G3_QX = 1, G3_QY = 2, G3_QZ = 3 };

extern const int8_t C3_NOP;
extern const int8_t C3_TEST;

const int8_t C3_NOP = G3_NOP;
const int8_t C3_QX = G3_QX;
const int8_t C3_QY = G3_QY;
const int8_t C3_QZ = G3_QZ;

typedef struct glyph3d {
	GLYPH_3D_TYPE type;
	int8_t raw[31];
} glyph3d_t;

typedef struct quad_glyph3d {
	GLYPH_TYPE type;
	float x;
	float y;
	float z;
	sprite_t* spritePtr;
	int32_t frame;
} quad_glyph3d_t;

	/* add to render list */

void addGlyph3dRTable(r3d_t* cont, renderTable_t* rt, glyph3d_t* g) {
	if (g->type >= G3_QX && g->type <= G3_QZ) {
		quad_glyph3d_t* tg = (quad_glyph3d_t*) g;
		if (tg->spritePtr == NULL) return;
		sprite_t* spritePtr = (sprite_t*) tg->spritePtr;
		if (spritePtr->image == NULL) return;
		tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
		// write into render list
		mesh_t* mesh = &cont->defQuad[g->type - 1];
		draw_dat3d_t* dat = (draw_dat3d_t*) nextRTable(cont, rt, mesh);
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