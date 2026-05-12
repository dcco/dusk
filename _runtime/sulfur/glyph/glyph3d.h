#ifndef GLYPH_3D_H
#define GLYPH_3D_H

	/* glyph datatype */

/*typedef tag_type GLYPH_3D_TYPE;
enum { G3_NOP = 0, G3_QX = 1, G3_QY = 2, G3_QZ = 3, G3_SPRITE = 4 };

extern const tag_type C3_NOP;
extern const tag_type C3_TEST;

const tag_type C3_NOP = G3_NOP;
const tag_type C3_QX = G3_QX;
const tag_type C3_QY = G3_QY;
const tag_type C3_QZ = G3_QZ;

typedef struct glyph3d {
	GLYPH_3D_TYPE type;
	int8_t raw[GLYPH_3D_SIZE - sizeof(GLYPH_TYPE)];
} glyph3d_t;

typedef struct quad_glyph3d {
	GLYPH_3D_TYPE type;
	float x;
	float y;
	float z;
	sprite_t* spritePtr;
	int32_t frame;
} quad_glyph3d_t;

typedef struct sprite_glyph3d {
	GLYPH_3D_TYPE type;
	float x;
	float y;
	float z;
	sprite_t* spritePtr;
	int32_t frame;
} sprite_glyph3d_t;*/

	/* add to render list */
/*
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
}*/

void addQuad3dRTable(r3d_t* cont, renderTable_t* rt, int axis, float x, float y, float z, sprite_t* spritePtr, int32_t frame) {
	if (spritePtr == NULL) return;
	if (spritePtr->image == NULL) return;
	tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
	// write into render list
	mesh_t* mesh = &cont->defQuad[axis];
	draw_dat3d_t* dat = (draw_dat3d_t*) nextRTable(cont, rt, mesh);
	dat->aPos[0] = x;
	dat->aPos[1] = y;
	dat->aPos[2] = z;
	dat->aTexId = imagePtr->index;
	dat->aTexUVPos[0] = spritePtr->fx + ((frame % spritePtr->spanWidth) * spritePtr->fw);
	dat->aTexUVPos[1] = spritePtr->fy + ((frame / spritePtr->spanWidth) * spritePtr->fh);
	dat->aTexUVSize[0] = spritePtr->fw;
	dat->aTexUVSize[1] = spritePtr->fh;
}

void addSprite3dRTable(r3d_t* cont, renderTable_t* rt, float x, float y, float z, sprite_t* spritePtr, int32_t frame) {
	if (spritePtr == NULL) return;
	if (spritePtr->image == NULL) return;
	tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
	// find appropriately sized sprite mesh
	// write into render list
	mesh_t* mesh = getSpriteMesh(cont, spritePtr->rw, spritePtr->rh);
	draw_dat3d_t* dat = (draw_dat3d_t*) nextRTable(cont, rt, mesh);
	dat->aPos[0] = x - spritePtr->offX;
	dat->aPos[1] = y - spritePtr->offY;
	dat->aPos[2] = z;
	dat->aTexId = imagePtr->index;
	dat->aTexUVPos[0] = spritePtr->fx + ((frame % spritePtr->spanWidth) * spritePtr->fw);
	dat->aTexUVPos[1] = spritePtr->fy + ((frame / spritePtr->spanWidth) * spritePtr->fh);
	dat->aTexUVSize[0] = spritePtr->fw;
	dat->aTexUVSize[1] = spritePtr->fh;
}

#endif