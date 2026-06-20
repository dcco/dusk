#ifndef FONT_H
#define FONT_H

typedef struct fontData {
	int index;
	stbtt_bakedchar* glyphs;
} fontData_t;

typedef struct fontFamily {
	int width;
	int height;
	int total;
	int* sizeList;
	fontData_t* data;
} fontFamily_t;

fontFamily_t* newFontFamily(tex_array_t* texArr, int startIndex, int32_t* iArgs, char* ttfData) {
	// initialize space for font family
	fontFamily_t* fontData = (fontFamily_t*) malloc(sizeof(fontFamily_t));
	fontData->total = iArgs[0];
	fontData->sizeList = (int*) malloc(sizeof(int) * iArgs[0]);
	fontData->data = (fontData_t*) malloc(sizeof(fontData_t) * iArgs[0]);
	// alloc space to draw font
	int32_t w = texArr->width;
	int32_t h = texArr->height;
	fontData->width = w;
	fontData->height = h;
	char* bmpData = malloc(sizeof(char) * w * h);
	memset(bmpData, 0, sizeof(char) * w * h);
	// draw font at each size
	for (int i = 0; i < iArgs[0]; i++) {
		// - read font size
		int32_t printSize = iArgs[i + 1];
		fontData->sizeList[i] = printSize;
		// - draw glyphs to bitmap
		stbtt_bakedchar* glyphs = (stbtt_bakedchar*) malloc(sizeof(stbtt_bakedchar) * 96);
		int res = stbtt_BakeFontBitmap(
			ttfData, 0, printSize,
			bmpData, w, h,
			32, 96, glyphs
		);
		if (res <= 0) {
			exit_log("Error rasterizing font. (Maybe texture atlas was too small?)", "");
		}
		// - upload texture data to array
		glBindTexture(GL_TEXTURE_2D_ARRAY, texArr->id);
		glTexSubImage3D(
			GL_TEXTURE_2D_ARRAY, 0,
			0, 0, startIndex + i,
			w, h, 1,
			GL_RED, GL_UNSIGNED_BYTE, bmpData
		);
		// - add data to font family
		fontData->data[i].index = startIndex + i;
		fontData->data[i].glyphs = glyphs;
	}
	// cleanup
	free(bmpData);
	return fontData;
}

#endif