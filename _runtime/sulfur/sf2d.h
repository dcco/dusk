#ifndef SF2D_H
#define SF2D_H

	/* sulfur 2d main */

typedef struct sf2d {
	shader_t* shader;
	tex_image_t defTex;
	buffer_t defTBuf;
	mesh_t defBox;	
} sf2d_t;

sf2d_t* initSf2d() {
	// load the base shader
	sf2d_t* self = (sf2d_t*) malloc(sizeof(sf2d_t));
	self->shader = initShader(BASE2_VS, BASE2_FS, &BASE2_ATTR_DEF);
	// create default materials
	initTexImage(&self->defTex, 2, 2, DEF_TEX_DATA);
	initBuffer(&self->defTBuf, sizeof(SQUARE_TEX_COORDS), SQUARE_TEX_COORDS, 6);
	float zCoords[18];
	squareZVertices(zCoords, 0.0f, 0.0f, 1.0f, 1.0f, 0.0f);
	initMesh(&self->defBox, zCoords, sizeof(float), 6);
	return self;
}

void delSf2d(sf2d_t* self) {
	delShader(self->shader);
	free(self);
}

mesh_t* tempBoxSf2d(sf2d_t* sf2d, int x, int y, int w, int h) {
	mesh_t* mesh = &sf2d->defBox;
	float zCoords[18];
	squareZVertices(zCoords, (float) x, (float) y, (float) w, (float) h, 0.0f);
	updateMesh(mesh, zCoords, sizeof(float), 6);
	return mesh;
}

#endif
