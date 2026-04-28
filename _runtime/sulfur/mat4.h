#ifndef MAT4_H
#define MAT4_H

const float M4_EPSILON = 0.000001f;

	/*
		VEC 3
	*/

typedef struct vec3 {
	float x;
	float y;
	float z;
} vec3_t;

	/*
		MAT 4
	*/

extern void _Mat4_Sys_Sulfur_idMat4(float* m) {
	memset(m, 0, sizeof(float) * 16);
	m[0] = 1.0f;
	m[5] = 1.0f;
	m[10] = 1.0f;
	m[15] = 1.0f;
}

extern void* _none_Sys_Sulfur_newMat4() {
	float* m = (float*) gc_alloc(sizeof(float) * 16, NULL);
	_Mat4_Sys_Sulfur_idMat4(m);
	return (void*) m;
}

float* rawCopyMat4(float* m) {
	float* f = (float*) malloc(sizeof(float) * 16);
	memcpy(f, m, sizeof(float) * 16);
	return f;
}

extern void _Mat4_Sys_Sulfur_translate(float* m, float x, float y, float z) {
	m[12] = m[0] * x + m[4] * y + m[8] * z + m[12];
	m[13] = m[1] * x + m[5] * y + m[9] * z + m[13];
	m[14] = m[2] * x + m[6] * y + m[10] * z + m[14];
	m[15] = m[3] * x + m[7] * y + m[11] * z + m[15];
}

extern void _Mat4_Sys_Sulfur_rotateX(float* m, float r) {
	float s = sin(r);
	float c = cos(r);

	float v10 = m[4];
	float v11 = m[5];
	float v12 = m[6];
	float v13 = m[7];
	float v20 = m[8];
	float v21 = m[9];
	float v22 = m[10];
	float v23 = m[11];

	m[4] = v10 * c + v20 * s;
	m[5] = v11 * c + v21 * s;
	m[6] = v12 * c + v22 * s;
	m[7] = v13 * c + v23 * s;
	m[8] = v20 * c - v10 * s;
	m[9] = v21 * c - v11 * s;
	m[10] = v22 * c - v12 * s;
	m[11] = v23 * c - v13 * s;
}

extern void _Mat4_Sys_Sulfur_lookAt(float* m, vec3_t* start, vec3_t* end, vec3_t* up) {
	float d0 = start->x - end->x;
	float d1 = start->y - end->y;
	float d2 = start->z - end->z;

	if (fabsf(d0) < M4_EPSILON || fabsf(d1) < M4_EPSILON || fabsf(d2) < M4_EPSILON)
	{
		_Mat4_Sys_Sulfur_idMat4(m);
		return;
	}

	float dMag = sqrt(d0 * d0 + d1 * d1 + d2 * d2);
	d0 = d0 / dMag;
	d1 = d1 / dMag;
	d2 = d2 / dMag;

	float x0 = up->y * d2 - up->z * d1;
	float x1 = up->z * d0 - up->x * d2;
	float x2 = up->x * d1 - up->y * d0;
	float xMag = sqrt(x0 * x0 + x1 * x1 + x2 * x2);
	if (xMag < M4_EPSILON) {
		x0 = 0.0f; x1 = 0.0f; x2 = 0.0f;
	} else {
		x0 = x0 / xMag;
		x1 = x1 / xMag;
		x2 = x2 / xMag;
	}

	float y0 = d1 * x2 - d2 * x1;
	float y1 = d2 * x0 - d0 * x2;
	float y2 = d0 * x1 - d1 * x0;
	float yMag = sqrt(y0 * y0 + y1 * y1 + y2 * y2);
	if (yMag < M4_EPSILON) {
		y0 = 0.0f; y1 = 0.0f; y2 = 0.0f;
	} else {
		y0 = y0 / yMag;
		y1 = y1 / yMag;
		y2 = y2 / yMag;
	}

	m[0] = x0;
	m[1] = y0;
	m[2] = d0;
	m[3] = 0.0f;
	m[4] = x1;
	m[5] = y1;
	m[6] = d1;
	m[7] = 0.0f;
	m[8] = x2;
	m[9] = y2;
	m[10] = d2;
	m[11] = 0.0f;
	m[12] = -(x0 * start->x + x1 * start->y + x2 * start->z);
	m[13] = -(y0 * start->x + y1 * start->y + y2 * start->z);
	m[14] = -(d0 * start->x + d1 * start->y + d2 * start->z);
	m[15] = 1.0f;
}

#endif