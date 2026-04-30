#ifndef SULF_MESH_H
#define SULF_MESH_H

	/*
		mesh:
		- rIndex: which render list to be used (-1 means un-assigned)
		- vertexTotal: vertex total
		- data: raw mesh data
	*/

typedef struct mesh {
	int rIndex;
	int vertexTotal;
	void* data;
} mesh_t;

void getNorm(float* n, const float* v) {
	float a0 = v[6] - v[0];
	float a1 = v[7] - v[1];
	float a2 = v[8] - v[2];
	float b0 = v[3] - v[0];
	float b1 = v[4] - v[1];
	float b2 = v[5] - v[2];
	// normalize
	float n0 = a1 * b2 - a2 * b1;
	float n1 = a2 * b0 - a0 * b2;
	float n2 = a0 * b1 - a1 * b0;
	float mag = sqrt(n0 * n0 + n1 * n1 + n2 * n2);
	n[0] = n1 / mag; n[1] = n1 / mag; n[2] = n2 / mag;
}

void initMesh(mesh_t* mesh, int vertexTotal, const float* data, const float* uv) {
	mesh->rIndex = -1;
	mesh->vertexTotal = vertexTotal;
	vertex_t* vertData = (vertex_t*) malloc(vertexTotal * sizeof(vertex_t));
	for (int i = 0; i < vertexTotal; i++) {
		memcpy(&vertData[i].pos, &data[i * 3], 3 * sizeof(float));
		memcpy(&vertData[i].uv, &uv[i * 2], 2 * sizeof(float));
	}
	if (vertexTotal % 3 != 0) exit_log("mesh.h - Attempted to create mesh with vertex count indivisible by 3.", "");
	for (int i = 0; i < vertexTotal / 3; i++) {
		int ix = i * 3;
		int iv = i * 9;
		getNorm(vertData[ix].normal, &data[iv]);
		memcpy(&vertData[ix + 1].normal, &vertData[ix].normal, 3 * sizeof(float));
		memcpy(&vertData[ix + 2].normal, &vertData[ix].normal, 3 * sizeof(float));
	}
	mesh->data = vertData;
	//memcpy(mesh->data, data, vertexTotal * sizeof(vertex_t));
	//initBuffer(&mesh->vBuf, elemSize * VBUF_SIZE * numItems, data, numItems);
}

void freeMesh(mesh_t* mesh) {
	free(mesh->data);
}

/*void updateMesh(mesh_t* mesh, const float* data) {
	memcpy(mesh->data, data, mesh->vertexTotal * sizeof(vertex_t));
}*/

/*
typedef struct mesh {
	buffer_t vBuf;
} mesh_t;

void initMesh(mesh_t* mesh, const void* data, int elemSize, int numItems) {
	initBuffer(&mesh->vBuf, elemSize * VBUF_SIZE * numItems, data, numItems);
}

void updateMesh(mesh_t* mesh, void* data, int elemSize, int numItems) {
	updateBuffer(&mesh->vBuf, elemSize * VBUF_SIZE * numItems, data, numItems, GL_DYNAMIC_DRAW);
}*/

#endif /* MESH_H */
