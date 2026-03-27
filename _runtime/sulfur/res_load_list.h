#ifndef RES_STACK_H
#define RES_STACK_H

	/* resource loading info */

typedef enum { R_IMAGE, R_SPRITE } R_LOAD_TYPE;

typedef struct resLoadItem {
	R_LOAD_TYPE type;
	void** storePtr;
	int a;
	int b;
	int* iArgs;
	void* xArgs;
} resLoadItem_t;

	/* resource load queue */

typedef struct resLoadList {
	int32_t capacity;
	int32_t length;
	int32_t head;
	resLoadItem_t* data;
} resLoadList_t;

resLoadList_t* newResList() {
	resLoadList_t* rl = (resLoadList_t*) malloc(sizeof(resLoadList_t));
	rl->capacity = 30;
	rl->length = 0;
	rl->head = 0;
	rl->data = (resLoadItem_t*) malloc(30 * sizeof(resLoadItem_t));
	return rl;
}

int emptyResList(resLoadList_t* list) {
	return list->head >= list->length;
}

void addResList(resLoadList_t* list, resLoadItem_t* v) {
	if (list->length >= list->capacity) {
		int32_t newCap = (list->capacity * 3) / 2;
		list->data = (resLoadItem_t*) realloc(list->data, newCap * sizeof(resLoadItem_t));
		list->capacity = newCap;
	}
	list->data[list->length] = *v;
	list->length = list->length + 1;
}

resLoadItem_t* takeResList(resLoadList_t* list) {
	if (list->head >= list->length) return NULL;
	int i = list->head;
	list->head = list->head + 1;
	return &list->data[i];
}

#endif