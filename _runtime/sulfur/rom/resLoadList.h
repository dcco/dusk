#ifndef RES_LOAD_LIST_H
#define RES_LOAD_LIST_H

	/*
		resource load item: a generic datatype containing information
			for either loading OR processing a resource
	*/

typedef tag_type R_LOAD_TYPE;
enum { R_IMAGE = 0, R_FONT = 1, R_SPRITE = 10 };

extern const tag_type RC_IMAGE;
extern const tag_type RC_FONT;
extern const tag_type RC_SPRITE;

const tag_type RC_IMAGE = R_IMAGE;
const tag_type RC_FONT = R_FONT;
const tag_type RC_SPRITE = R_SPRITE;

typedef struct resLoadItem {
	R_LOAD_TYPE type;
	void** storePtr;
	int storeId;
	int a;
	int b;
	int* iArgs;
	void* xArgs;
} resLoadItem_t;

	/*
		resource load [queue]: a list containing a "queue" of resources to be processed
			keeps track of a "head" pointer. instead of "popping" values out of the queue,
			the head pointer simply moves forward.
			this is so the items may be used in-place.
	*/

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

void freeResList(resLoadList_t* list) {
	free(list->data);
	free(list);
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