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
	if (arrayId == 0) exit_log("Could not generate texture array.", "");

	// initialize texture array
	glBindTexture(GL_TEXTURE_2D_ARRAY, arrayId);
	glTexImage3D(
		GL_TEXTURE_2D_ARRAY, 0, GL_RGBA,
		w, h, total + 1, 0,
		GL_RGBA, GL_UNSIGNED_BYTE, NULL
	);

	// other texture parameters
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D_ARRAY, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	// fill layer 0 with an all-white image
	unsigned char* defData = malloc(w * h * 4);
	for (int i = 0; i < w * h * 4; i++) {
		defData[i] = 255;
	}
	glTexSubImage3D(
		GL_TEXTURE_2D_ARRAY, 0,
		0, 0, 0,
		w, h, 1,
		GL_RGBA, GL_UNSIGNED_BYTE, defData
	);
	free(defData);

	// store object data
	tex_array_t* arr = (tex_array_t*) malloc(sizeof(tex_array_t));
	arr->id = arrayId;
	arr->width = w;
	arr->height = h;
	return arr;
}

#endif