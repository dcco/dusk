#ifndef RENDER_TABLE_H
#define RENDER_TABLE_H

	/*
		render node:
			- mesh: pointer to mesh associated with list
			- list: raw data
			- next: pointer to link render lists together
				(so that only active ones will be in use)
	*/

typedef struct renderNode {
	mesh_t* mesh;
	renderList_t list;
	struct renderNode* next;
} renderNode_t;

void initRNode(renderNode_t* rn, size_t unitSize) {
	initRList(&rn->list, unitSize);
	rn->mesh = NULL;
	rn->next = NULL;
}

	/*
		render table:
			stores per-mesh render lists
	*/

typedef struct renderTable {
	int32_t capacity;
	int32_t total;
	size_t unitSize;
	renderNode_t* lists;
	renderNode_t* activeHead;
} renderTable_t;

void initRTable(renderTable_t* rt, size_t unitSize) {
	rt->capacity = 20;
	rt->total = 0;
	rt->unitSize = unitSize;
	rt->lists = (renderNode_t*) malloc(20 * sizeof(renderNode_t));
	for (int i = 0; i < 20; i++) {
		initRNode(rt->lists + i, unitSize);
	}
	rt->activeHead = NULL;
}

renderTable_t* newRTable(size_t unitSize) {
	renderTable_t* rt = (renderTable_t*) malloc(sizeof(renderTable_t));
	initRTable(rt, unitSize);
	return rt;
}

void clearRTable(renderTable_t* table) {
	renderNode_t* curNode = table->activeHead;
	while (curNode != NULL) {
		renderNode_t* nextNode = curNode->next;
		clearRList(&curNode->list);
		curNode->next = NULL;
		curNode = nextNode;
	}
	table->activeHead = NULL;
}

/*int32_t _addListRTable(renderTable_t* table) {
	// add space if necessary
	if (table->total >= table->capacity) {
		int32_t newCap = (table->capacity * 5) / 4;
		table->lists = realloc(table->lists, newCap * sizeof(renderNode_t));
		table->capacity = newCap;
	}
	// add the new list
	int32_t i = table->total;
	initRNode(table->lists + i, table->unitSize);
	table->total = i + 1;
	return i;
}*/

void _extendUptoRTable(renderTable_t* table, int32_t newIndex) {
	if (newIndex < table->total) return;
	// add space if necessary
	int32_t oldSize = table->total;
	if (newIndex > table->capacity) {
		int32_t newCap = ((newIndex + 1) * 5) / 4;
		table->lists = realloc(table->lists, newCap * sizeof(renderNode_t));
		table->capacity = newCap;
	}
	// add new lists
	for (int i = oldSize; i <= newIndex; i++) {
		initRNode(table->lists + i, table->unitSize);
	}
	table->total = newIndex + 1;	
}

void* nextRTable(r3d_t* cont, renderTable_t* table, mesh_t* mesh) {
	// get render list index from mesh
	int32_t rIndex = mesh->rIndex;
	if (rIndex == -1) {
		// get new index if none exists
		rIndex = cont->lastIndex;
		cont->lastIndex = cont->lastIndex + 1;
		_extendUptoRTable(table, rIndex);
		mesh->rIndex = rIndex;
		//rIndex = _addListRTable(table);
		//table->lists[rIndex].mesh = mesh;
		//mesh->rIndex = rIndex;
	}
	// extend table if not enough space
	if (rIndex >= table->total) _extendUptoRTable(table, rIndex);
	// link mesh to list if unlinked
	if (table->lists[rIndex].mesh == NULL) table->lists[rIndex].mesh = mesh;
	// get relevant render list, if not active, make active
	renderNode_t* rNode = table->lists + rIndex;
	if (rNode->list.length == 0) {
		rNode->next = table->activeHead;
		table->activeHead = rNode;
	}
	// get next memory location within list
	return nextRList(&rNode->list);
}

#endif