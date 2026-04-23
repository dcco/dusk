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

void squareXVertices(float* data, float l, float t, float w, float h, float x) {
	float r = l + w;
	float b = t + h;
	/*
		1 2  returned in CCW order: [1, 4, 2] [1, 3, 4]
		3 4
	*/
	data[0] = x; data[1] = t; data[2] = l;
	data[3] = x; data[4] = b; data[5] = r;
	data[6] = x; data[7] = l; data[8] = r; 
	data[9] = x; data[10] = t; data[11] = l; 
	data[12] = x; data[13] = b; data[14] = l; 
	data[15] = x; data[16] = b; data[17] = r; 
}

void squareYVertices(float* data, float l, float t, float w, float h, float y) {
	float r = l + w;
	float b = t + h;
	/*
		1 2  returned in CCW order: [1, 4, 2] [1, 3, 4]
		3 4
	*/
	data[0] = l; data[1] = y; data[2] = t;
	data[3] = r; data[4] = y; data[5] = b;
	data[6] = r; data[7] = y; data[8] = l;
	data[9] = l; data[10] = y; data[11] = t;
	data[12] = l; data[13] = y; data[14] = b;
	data[15] = r; data[16] = y; data[17] = b;
}

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