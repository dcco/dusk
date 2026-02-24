#ifndef SULF_BUFFER_H
#define SULF_BUFFER_H

typedef struct buffer {
	GLuint id;
	int numItems;
} buffer_t;

const GLint VBUF_SIZE = 3;
const GLint TBUF_SIZE = 2;

void initBuffer(buffer_t* buffer, GLsizeiptr size, const void* data, int numItems) {
	glGenBuffers(1, &buffer->id);
	glBindBuffer(GL_ARRAY_BUFFER, buffer->id);
	glBufferData(GL_ARRAY_BUFFER, size, data, GL_STATIC_DRAW);
	buffer->numItems = numItems;
}

void updateBuffer(buffer_t* buffer, GLsizeiptr size, void* data, int numItems, GLenum dType) {
	glBindBuffer(GL_ARRAY_BUFFER, buffer->id);
	glBufferData(GL_ARRAY_BUFFER, size, data, dType);
	buffer->numItems = numItems;
}

	/* common coordinate/vertex lists */

const float SQUARE_TEX_COORDS[] = {
	0.0f, 0.0f, 1.0f, 1.0f, 1.0f, 0.0f,
	0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f
};

void squareZVertices(float* data, float l, float t, float w, float h, float z) {
	float r = l + w;
	float b = t + h;
	/*
		1 2  returned in CCW order: [1, 4, 2] [1, 3, 4]
		3 4
	*/
	float newData[] = {
		l, t, z, r, b, z,
		r, t, z, l, t, z,
		l, b, z, r, b, z
	};
	memcpy(data, newData, sizeof(newData));
}

#endif /* SULF_BUFFER_H */