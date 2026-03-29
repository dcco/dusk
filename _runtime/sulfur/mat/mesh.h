#ifndef SULF_MESH_H
#define SULF_MESH_H

	/*
		mesh:
	*/

typedef struct mesh {
	int vertexTotal;
	void* data;
} mesh_t;

void initMesh(mesh_t* mesh, int vertexTotal, const float* data, const float* uv) {
	mesh->vertexTotal = vertexTotal;
	vertex_t* vertData = (vertex_t*) malloc(vertexTotal * sizeof(vertex_t));
	for (int i = 0; i < vertexTotal; i++) {
		memcpy(&vertData[i].pos, &data[i * 3], 3 * sizeof(float));
		memcpy(&vertData[i].uv, &uv[i * 2], 2 * sizeof(float));
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
