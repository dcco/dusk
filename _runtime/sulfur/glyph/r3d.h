#ifndef R3D_H
#define R3D_H

	/* 3d render context */

typedef struct r3d {
	int32_t lastIndex;
	mesh_t defQuad[3];
	int32_t cacheWidth;
	int32_t cacheHeight;
	mesh_t*** spriteCache;
} r3d_t;

r3d_t* initR3d() {
	// initialize basic context
	r3d_t* self = (r3d_t*) malloc(sizeof(r3d_t));
	self->lastIndex = 0;
	// initialize default quads
	float coords[18];
	squareXVertices(coords, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f);
	initMesh(&self->defQuad[0], 6, coords, SQUARE_TEX_COORDS);
	squareYVertices(coords, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f);
	initMesh(&self->defQuad[1], 6, coords, SQUARE_TEX_COORDS);
	squareZVertices(coords, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f);
	initMesh(&self->defQuad[2], 6, coords, SQUARE_TEX_COORDS);
	// initialize sprite mesh cache
	self->cacheWidth = 4;
	self->cacheHeight = 4;
	self->spriteCache = (mesh_t***) malloc(sizeof(mesh_t**) * 4);
	for (int i = 0; i < 4; i++) {
		self->spriteCache[i] = (mesh_t**) malloc(sizeof(mesh_t*) * 4);
		for (int j = 0; j < 4; j++) {
			self->spriteCache[i][j] = NULL;
		}
	}
	return self;
}

mesh_t* getSpriteMesh(r3d_t* cont, int i, int j) {
	if (i >= cont->cacheHeight) {
		cont->cacheHeight = i + 1;
		for (int k = 0; k < cont->cacheWidth; k++) {
			cont->spriteCache[k] = (mesh_t**) realloc(cont->spriteCache[k], sizeof(mesh_t*) * cont->cacheHeight);
		}
	}
	if (i >= cont->cacheWidth) {
		cont->spriteCache = (mesh_t***) realloc(cont->spriteCache, sizeof(mesh_t**) * (i + 1));
		for (int k = cont->cacheWidth; k <= i; k++) {
			cont->spriteCache[k] = (mesh_t**) malloc(sizeof(mesh_t*) * cont->cacheHeight);
			for (int l = 0; l <= cont->cacheHeight; l++) {
				cont->spriteCache[k][l] = NULL;
			}
		}
		cont->cacheWidth = i + 1;
	}
	if (cont->spriteCache[i][j] == NULL) {
		cont->spriteCache[i][j] = malloc(sizeof(mesh_t));
		float coords[18];
		squareZVertices(coords, 0.0f, 0.0f, (float) i, (float) j, 0.0f);
		initMesh(cont->spriteCache[i][j], 6, coords, SQUARE_TEX_COORDS);
	}
	return cont->spriteCache[i][j];
}

#endif