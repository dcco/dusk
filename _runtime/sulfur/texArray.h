#ifndef TEX_ARRAY_H
#define TEX_ARRAY_H

typedef struct tex_array {
	GLuint id;
	int width;
	int height;
} tex_array_t;

tex_array_t* initTexArray(int total, int w, int h) {
	// create texture array
	GLuint arrayId;
	glGenTextures(1, &arrayId);
	if (arrayId < 0) exit_log("Could not generate texture array.", "");

	// initialize texture array
	glBindTexture(GL_TEXTURE_2D_ARRAY, arrayId);
	glTexImage3D(
		GL_TEXTURE_2D_ARRAY, 0, GL_RGBA,
		w, h, total, 0,
		GL_RGBA, GL_UNSIGNED_BYTE, NULL
	);

	// store object data
	tex_array_t* arr = (tex_array_t*) malloc(sizeof(tex_array_t));
	arr->id = arrayId;
	arr->width = w;
	arr->height = h;
	return arr;
}

#endif