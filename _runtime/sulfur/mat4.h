#ifndef MAT4_H
#define MAT4_H

extern void _Mat4_Sys_Sulfur_idMat4(float* m) {
	memset(m, 0, sizeof(float) * 16);
	m[0] = 1.0f;
	m[5] = 1.0f;
	m[10] = 1.0f;
	m[15] = 1.0f;
}

	/*
		TODO: change back to a gc_alloc once we have global initialization order working
	*/

extern void* _none_Sys_Sulfur_newMat4() {
	float* fData = (float*) malloc(sizeof(float) * 16);
	_Mat4_Sys_Sulfur_idMat4(fData);
	return (void*) fData;
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


#endif