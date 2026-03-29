#ifndef SULF_VERTEX_H
#define SULF_VERTEX_H

	/*
		vertex:
			defines an individual vertex in a uniform way, to be passed into shaders
	*/

typedef struct vertex {
	float pos[3];
	float normal[3];
	float uv[2];
} vertex_t;

const float SQUARE_TEX_COORDS[] = {
	0.0f, 0.0f, 1.0f, 1.0f, 1.0f, 0.0f,
	0.0f, 0.0f, 0.0f, 1.0f, 1.0f, 1.0f
};

	/* common coordinate/vertex lists */

void squareZVertices(float* data, float l, float t, float w, float h, float z) {
	float r = l + w;
	float b = t + h;
	/*
		1 2  returned in CCW order: [1, 4, 2] [1, 3, 4]
		3 4
	*/
	data[0] = l; data[1] = t; data[2] = z;
	data[3] = r; data[4] = b; data[5] = z;
	data[6] = r; data[7] = l; data[8] = z;
	data[9] = l; data[10] = t; data[11] = z;
	data[12] = l; data[13] = b; data[14] = z;
	data[15] = r; data[16] = b; data[17] = z;
	/*float newData[] = {
		l, t, z, r, b, z,
		r, t, z, l, t, z,
		l, b, z, r, b, z
	};
	memcpy(data, newData, sizeof(newData));*/
}

#endif