#ifndef GLYPH_H
#define GLYPH_H

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

void addTextfRList(renderList_t* rl, fontFamily_t* familyPtr, int32_t x, int32_t y, dusk_string_t* text) {
	// obtain font family data
	if (familyPtr == NULL) return;
	fontData_t* fontPtr = &familyPtr->data[0];
	int texId = fontPtr->index;
	// create cursor
	float fx = (float) x;
	float fy = (float) y;
	// write every character in the string
	char* data = &text->start;
	for (int i = 0; i < text->len; i++) {
		char c = data[i];
		if (c >= familyPtr->minC && c < familyPtr->maxC) {
			// read data for specific glyph
			int ii = c - familyPtr->minC;
			stbtt_bakedchar* charData = &fontPtr->glyphs[ii];
			// draw char glyph
			draw_dat2d_t* dat = (draw_dat2d_t*) nextRList(rl);
			dat->aPos[0] = fx + charData->xoff;
			dat->aPos[1] = fy + charData->yoff;
			dat->aPos[2] = (float) -lenRList(rl);
			dat->aSize[0] = charData->x1 - charData->x0;
			dat->aSize[1] = charData->y1 - charData->y0;
			dat->aColor = 0xFFFFFFFF;
			dat->aTexId = texId;
			dat->aTexUVPos[0] = (float) charData->x0 / familyPtr->width;
			dat->aTexUVPos[1] = (float) charData->y0 / familyPtr->height;
			dat->aTexUVSize[0] = (float) (charData->x1 - charData->x0) / familyPtr->width;
			dat->aTexUVSize[1] = (float) (charData->y1 - charData->y0) / familyPtr->height;
			// move cursor
			fx = fx + charData->xadvance;
		}
	}
}

#endif