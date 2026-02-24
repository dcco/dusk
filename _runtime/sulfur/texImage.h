#ifndef TEX_IMAGE_H
#define TEX_IMAGE_H

typedef struct tex_image {
	GLuint texId;
	int width;
	int height;
} tex_image_t;

void initTexImage(tex_image_t* image, int w, int h, const char* data) {
	// create texture
	GLuint id;
	glGenTextures(1, &id);
	if (id < 0) exit_log("Could not generate texture.", "");

	// load texture data
	glBindTexture(GL_TEXTURE_2D, id);
	glTexImage2D(
		GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0,
		GL_RGBA, GL_UNSIGNED_BYTE, data
	);

	// other texture parameters
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	// store object data
	image->texId = id;
	image->width = w;
	image->height = h;
}

const char DEF_TEX_DATA[] = {
	255, 255, 255, 255, 255, 255, 255, 255, 
	255, 255, 255, 255, 255, 255, 255, 255
};

#endif /* TEX_IMAGE_H */