#ifndef GLYPH_H
#define GLYPH_H

	/* glyph datatype */
/*
typedef tag_type GLYPH_TYPE;
enum { G_NOP = 0, G_BOX = 1, G_SPRITE = 3, G_TEXT = 4 };

extern const tag_type C_NOP;
extern const tag_type C_BOX;
extern const tag_type C_SPRITE;
extern const tag_type C_TEXT;

const tag_type C_NOP = G_NOP;
const tag_type C_BOX = G_BOX;
const tag_type C_SPRITE = G_SPRITE;
const tag_type C_TEXT = G_TEXT;

typedef struct glyph {
	GLYPH_TYPE type;
	int8_t raw[GLYPH_SIZE - sizeof(GLYPH_TYPE)];
} glyph_t;

typedef struct box_glyph {
	GLYPH_TYPE type;
	int32_t c;
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
	int8_t facing;
} sprite_glyph_t;

typedef struct text_glyph {
	GLYPH_TYPE type;
	sprite_t* fontPtr;
	int32_t x;
	int32_t y;
	int32_t kw;
	dusk_string_t* text;
} text_glyph_t;*/

	/* add to render list */

/*
void addGlyphRList(renderList_t* rl, glyph_t* g) {
	if (g->type == C_BOX) {
		box_glyph_t* bg = (box_glyph_t*) g;
		draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
		dat->aPos[0] = (float) bg->x;
		dat->aPos[1] = (float) bg->y;
		dat->aPos[2] = (float) -lenRList(rl);
		dat->aSize[0] = (float) bg->w;
		dat->aSize[1] = (float) bg->h;
		dat->aColor = (uint32_t) bg->c;
		dat->aTexId = 0;
		dat->aTexUVPos[0] = 0.0;
		dat->aTexUVPos[1] = 0.0;
		dat->aTexUVSize[0] = 1.0;
		dat->aTexUVSize[1] = 1.0;
	} else if (g->type == C_SPRITE) {
		// obtain sprite + image data
		sprite_glyph_t* sg = (sprite_glyph_t*) g;
		if (sg->spritePtr == NULL) return;
		sprite_t* spritePtr = (sprite_t*) sg->spritePtr;
		if (spritePtr->image == NULL) return;
		tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
		// write into render list
		draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
		float flipX = 0.0;
		dat->aPos[0] = (float) sg->x;
		dat->aPos[1] = (float) sg->y;
		dat->aPos[2] = (float) -lenRList(rl);
		dat->aSize[0] = (float) spritePtr->tw;
		dat->aSize[1] = (float) spritePtr->th;
		dat->aColor = 0xFFFFFFFF;
		dat->aTexId = imagePtr->index;
		float offX = (sg->frame % spritePtr->spanWidth) * spritePtr->fw;
		if (!sg->facing) {
			dat->aTexUVPos[0] = spritePtr->fx + offX;
			dat->aTexUVSize[0] = spritePtr->fw;
		} else {
			dat->aTexUVPos[0] = spritePtr->fx + offX + spritePtr->fw;
			dat->aTexUVSize[0] = -spritePtr->fw;
		}
		//dat->aTexUVPos[0] = spritePtr->fx + ((sg->frame % spritePtr->spanWidth) * spritePtr->fw);
		//dat->aTexUVSize[0] = spritePtr->fw;
		dat->aTexUVPos[1] = spritePtr->fy + ((sg->frame / spritePtr->spanWidth) * spritePtr->fh);
		dat->aTexUVSize[1] = spritePtr->fh;
	} else if (g->type == C_TEXT) {
		// obtain sprite + image data
		text_glyph_t* tg = (text_glyph_t*) g;
		if (tg->fontPtr == NULL) return;
		sprite_t* spritePtr = (sprite_t*) tg->fontPtr;
		if (spritePtr->image == NULL) return;
		tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
		// write every character in the string
		dusk_string_t* text = tg->text;
		char* data = &text->start;
		for (int i = 0; i < text->len; i++) {
			char c = data[i];
			if (c > 32 && c <= 96) {
				drawChar(rl, spritePtr, imagePtr, tg, i, c - 32);
			}
		}
	}
}*/

void addBoxRList(renderList_t* rl, int32_t c, int32_t x, int32_t y, int32_t w, int32_t h) {
	draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
	dat->aPos[0] = (float) x;
	dat->aPos[1] = (float) y;
	dat->aPos[2] = (float) -lenRList(rl);
	dat->aSize[0] = (float) w;
	dat->aSize[1] = (float) h;
	dat->aColor = (uint32_t) c;
	dat->aTexId = 0;
	dat->aTexUVPos[0] = 0.0;
	dat->aTexUVPos[1] = 0.0;
	dat->aTexUVSize[0] = 1.0;
	dat->aTexUVSize[1] = 1.0;
}

void addSpriteRList(renderList_t* rl, int32_t x, int32_t y, sprite_t* spritePtr, int32_t frame, int8_t facing) {
	// obtain sprite + image data
	if (spritePtr == NULL) return;
	if (spritePtr->image == NULL) return;
	tex_image_t* imagePtr = (tex_image_t*) spritePtr->image;
	// write into render list
	draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
	float flipX = 0.0;
	dat->aPos[0] = (float) x;
	dat->aPos[1] = (float) y;
	dat->aPos[2] = (float) -lenRList(rl);
	dat->aSize[0] = (float) spritePtr->tw;
	dat->aSize[1] = (float) spritePtr->th;
	dat->aColor = 0xFFFFFFFF;
	dat->aTexId = imagePtr->index;
	float offX = (frame % spritePtr->spanWidth) * spritePtr->fw;
	if (!facing) {
		dat->aTexUVPos[0] = spritePtr->fx + offX;
		dat->aTexUVSize[0] = spritePtr->fw;
	} else {
		dat->aTexUVPos[0] = spritePtr->fx + offX + spritePtr->fw;
		dat->aTexUVSize[0] = -spritePtr->fw;
	}
	dat->aTexUVPos[1] = spritePtr->fy + ((frame / spritePtr->spanWidth) * spritePtr->fh);
	dat->aTexUVSize[1] = spritePtr->fh;
}

inline static void drawChar(renderList_t* rl, sprite_t* spritePtr, tex_image_t* imagePtr,
	int32_t x, int32_t y, int32_t kw, int i, int frame)
{
	draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
	dat->aPos[0] = (float) (x + kw * i);
	dat->aPos[1] = (float) y;
	dat->aPos[2] = (float) -lenRList(rl);
	dat->aSize[0] = (float) spritePtr->tw;
	dat->aSize[1] = (float) spritePtr->th;
	dat->aColor = 0xFFFFFFFF;
	dat->aTexId = imagePtr->index;
	dat->aTexUVPos[0] = spritePtr->fx + ((frame % spritePtr->spanWidth) * spritePtr->fw);
	dat->aTexUVPos[1] = spritePtr->fy + ((frame / spritePtr->spanWidth) * spritePtr->fh);
	dat->aTexUVSize[0] = spritePtr->fw;
	dat->aTexUVSize[1] = spritePtr->fh;
}

void addTextRList(renderList_t* rl, sprite_t* fontPtr, int32_t x, int32_t y, int32_t kw, dusk_string_t* text) {
	// obtain sprite + image data
	if (fontPtr == NULL) return;
	if (fontPtr->image == NULL) return;
	tex_image_t* imagePtr = (tex_image_t*) fontPtr->image;
	// write every character in the string
	char* data = &text->start;
	for (int i = 0; i < text->len; i++) {
		char c = data[i];
		if (c > 32 && c <= 96) {
			drawChar(rl, fontPtr, imagePtr, x, y, kw, i, c - 32);
		}
	}
}

#endif