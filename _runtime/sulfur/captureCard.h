#ifndef CAPTURE_CARD_H
#define CAPTURE_CARD_H

	/*
		capture card:
			a special part of sulfur dedicated to up-scaling the 2d graphical display
	*/

typedef struct cc {
	int8_t zoom;
	int32_t width;
	int32_t height;
	int32_t windowHeight;
	int32_t focusHeight;
	tex_array_t* capArr;
	draw_dat2d_t* data;
} cc_t;

int nextPow2(int n) {
	int sz = 2;
	while (sz < n) {
		sz = sz * 2;
		if (sz >= 4096) return 4096;
	}
	return sz;
}

cc_t* initCC(int zoom, int w, int h) {
		// initialize capture card + zoom level
	cc_t* cc = malloc(sizeof(cc_t));
	cc->zoom = (int8_t) zoom;
	if (cc->zoom <= 0) cc->zoom = 1;
	else if (cc->zoom > 8) cc->zoom = 8;
		// calculate capture card size
	cc->windowHeight = h;
	cc->focusHeight = ((h - 1) / cc->zoom) + 1;
	cc->width = nextPow2(w / cc->zoom);
	cc->height = nextPow2(cc->focusHeight);
		// initialize texture + draw info
	cc->capArr = initTexArray(1, cc->width, cc->height);
	draw_dat2d_t* data = malloc(sizeof(draw_dat2d_t));
	data->aPos[0] = 0.0;
	data->aPos[1] = (float) cc->focusHeight * cc->zoom;
	data->aSize[0] = (float) cc->width * cc->zoom;
	data->aSize[1] = (float) -cc->height * cc->zoom;
	data->aTexId = 0.0;
	data->aTexUVPos[0] = 0.0;
	data->aTexUVPos[1] = 0.0;
	data->aTexUVSize[0] = 1.0;
	data->aTexUVSize[1] = 1.0;
	cc->data = data;
	return cc;
}

void copyCC(cc_t* cc) {
	glBindTexture(GL_TEXTURE_2D_ARRAY, cc->capArr->id);
	glCopyTexSubImage3D(
		GL_TEXTURE_2D_ARRAY, 0,
		0, 0, 0,
		0, cc->windowHeight - cc->focusHeight,
		cc->width, cc->height
	);
}

#endif