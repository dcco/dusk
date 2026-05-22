#ifndef TEX_SINGLE_IMAGE_H
#define TEX_SINGLE_IMAGE_H

typedef struct tex_simage {
	GLuint texId;
	int width;
	int height;
} tex_simage_t;

void initTexImageFloat(tex_simage_t* image, int w, int h, float* data) {
	// create texture data
	glGenTextures(1, &image->texId);

	// initialize texture data
	glBindTexture(GL_TEXTURE_2D, image->texId);
	glTexImage2D(
		GL_TEXTURE_2D, 0,
		GL_RGBA16F, w, h, 0,
		GL_RGBA, GL_FLOAT, data
	);

	// other texture parameters
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	// store object data
	image->width = w;
	image->height = h;
}

#endif