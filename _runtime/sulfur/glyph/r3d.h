#ifndef R3D_H
#define R3D_H

	/* 3d render context */

typedef struct r3d {
	int32_t lastIndex;
	mesh_t defQuad[3];
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
	return self;
}

#endif