#ifndef SULF_MESH_H
#define SULF_MESH_H

typedef struct mesh {
	buffer_t vBuf;
} mesh_t;

void initMesh(mesh_t* mesh, const void* data, int elemSize, int numItems) {
	initBuffer(&mesh->vBuf, elemSize * VBUF_SIZE * numItems, data, numItems);
}

void updateMesh(mesh_t* mesh, void* data, int elemSize, int numItems) {
	updateBuffer(&mesh->vBuf, elemSize * VBUF_SIZE * numItems, data, numItems, GL_DYNAMIC_DRAW);
}

#endif /* MESH_H */
