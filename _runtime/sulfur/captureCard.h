#ifndef CAPTURE_CARD_H
#define CAPTURE_CARD_H

	/*
		capture card:
			a special part of sulfur dedicated to up-scaling the 2d graphical display
	*/

typedef struct updateFlag {
	int8_t flag;
	int32_t newZoom;
	int32_t newWidth;
	int32_t newHeight;
} updateFlag_t;

typedef struct cc {
	int8_t zoom;
	int32_t width;
	int32_t height;
	int32_t windowWidth;
	int32_t windowHeight;
	int32_t focusHeight;
	tex_array_t* capArr;
	draw_dat2d_t* data;
	updateFlag_t uf;
} cc_t;

int nextPow2(int n) {
	int sz = 2;
	while (sz < n) {
		sz = sz * 2;
		if (sz >= 8192) return 8192;
	}
	return sz;
}

const int32_t MAX_ZOOM = 8;

cc_t* initCC(int zoom, int w, int h) {
		// initialize capture card + zoom level
	cc_t* cc = malloc(sizeof(cc_t));
	cc->zoom = (int8_t) zoom;
	if (cc->zoom <= 0) cc->zoom = 1;
	else if (cc->zoom > MAX_ZOOM) cc->zoom = MAX_ZOOM;
		// calculate capture card size
	cc->windowWidth = w;
	cc->windowHeight = h;
	cc->focusHeight = ((h - 1) / cc->zoom) + 1;
	cc->width = nextPow2(w / cc->zoom);
	cc->height = nextPow2(cc->focusHeight);
		// initialize texture + draw info
	cc->capArr = initTexArray(1, cc->width, cc->height);
	draw_dat2d_t* data = malloc(sizeof(draw_dat2d_t));
	data->aPos[0] = 0.0;
	data->aPos[1] = (float) cc->focusHeight * cc->zoom;
	data->aPos[2] = 0.0;
	data->aSize[0] = (float) cc->width * cc->zoom;
	data->aSize[1] = (float) -cc->height * cc->zoom;
	data->aColor = 0xFFFFFFFF;
	data->aTexId = 0.0;
	data->aTexUVPos[0] = 0.0;
	data->aTexUVPos[1] = 0.0;
	data->aTexUVSize[0] = 1.0;
	data->aTexUVSize[1] = 1.0;
	cc->data = data;
	cc->uf.flag = 0;
	return cc;
}

void askResizeCC(cc_t* cc, int zoom, int w, int h) {
	cc->uf.flag = 1;
	cc->uf.newZoom = zoom;
	cc->uf.newWidth = w;
	cc->uf.newHeight = h;
}

void resizeCC(cc_t* cc, int zoom, int w, int h) {
	if (zoom <= 0) zoom = 1;
	else if (zoom > MAX_ZOOM) zoom = MAX_ZOOM;
	cc->zoom = (int8_t) zoom;
	cc->windowWidth = w;
	cc->windowHeight = h;
	cc->focusHeight = ((h - 1) / zoom) + 1;
	cc->width = nextPow2(w / zoom);
	cc->height = nextPow2(cc->focusHeight);
	resizeTexArray(cc->capArr, 1, (int) cc->width, (int) cc->height);
	draw_dat2d_t* data = cc->data;
	data->aPos[1] = (float) cc->focusHeight * cc->zoom;
	data->aSize[0] = (float) cc->width * cc->zoom;
	data->aSize[1] = (float) -cc->height * cc->zoom;
}

void updateCC(cc_t* cc) {
	if (cc->uf.flag) {
		cc->uf.flag = 0;
		resizeCC(cc, cc->uf.newZoom, cc->uf.newWidth, cc->uf.newHeight);
	}
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