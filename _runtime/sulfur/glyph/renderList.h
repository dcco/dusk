#ifndef RENDER_LIST_H
#define RENDER_LIST_H

	/*
		render list:
			stores render objects of variable size
	*/

typedef struct renderList {
	int32_t capacity;
	int32_t length;
	size_t unitSize;
	void* data;
} renderList_t;

void initRList(renderList_t* rl, size_t unitSize) {
	rl->capacity = 100;
	rl->length = 0;
	rl->unitSize = unitSize;
	rl->data = (renderList_t*) malloc(100 * unitSize);
}

renderList_t* newRList(size_t unitSize) {
	renderList_t* rl = (renderList_t*) malloc(sizeof(renderList_t));
	initRList(rl, unitSize);
	return rl;
}

void clearRList(renderList_t* list) {
	list->length = 0;
}

void* nextRList(renderList_t* list) {
	if (list->length >= list->capacity) {
		int32_t newCap = (list->capacity * 5) / 4;
		list->data = realloc(list->data, newCap * list->unitSize);
		list->capacity = newCap;
	}
	void* newRef = list->data + (list->length * list->unitSize);
	list->length = list->length + 1;
	return newRef;
}
/*
void addRList(renderList_t* list, void* g) {
	if (list->length >= list->capacity) {
		int32_t newCap = (list->capacity * 5) / 4;
		list->data = realloc(list->data, newCap * list->unitSize);
		list->capacity = newCap;
	}
	memcpy(list->data + (list->length * list->unitSize), g, list->unitSize);
	list->length = list->length + 1;
}*/

int32_t lenRList(renderList_t* list) {
	return list->length;
}
/*
glyph_t* getRList(renderList_t* list, int i) {
	return list->data + i];
}*/

#endif