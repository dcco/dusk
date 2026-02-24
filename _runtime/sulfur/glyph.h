
	/* glyph datatype */

typedef enum : int8_t { G_NOP = 0, G_BOX = 1, G_IMAGE = 2, G_SPRITE = 3 } GLYPH_TYPE;

extern const int8_t C_NOP;
extern const int8_t C_BOX;
extern const int8_t C_IMAGE;
extern const int8_t C_SPRITE;

const int8_t C_NOP = G_NOP;
const int8_t C_BOX = G_BOX;
const int8_t C_IMAGE = G_IMAGE;
const int8_t C_SPRITE = G_SPRITE;

typedef struct glyph {
	GLYPH_TYPE type;
	int32_t a;
	int32_t b;
	int32_t c;
	int32_t d;
} glyph_t;

	/*
		glyph lists
	*/

typedef struct glyphList {
	int32_t capacity;
	int32_t length;
	glyph_t* data;
} glyphList_t;

glyphList_t* newGList() {
	glyphList_t* gl = (glyphList_t*) malloc(sizeof(glyphList_t));
	gl->capacity = 100;
	gl->length = 0;
	gl->data = (glyph_t*) malloc(100 * sizeof(glyph_t));
	return gl;
}

void clearGList(glyphList_t* list) {
	list->length = 0;
}

void addGList(glyphList_t* list, glyph_t* g) {
	if (list->length >= list->capacity) {
		int32_t newCap = (list->capacity * 5) / 4;
		list->data = (glyph_t*) realloc(list->data, newCap * sizeof(glyph_t));
		list->capacity = newCap;
	}
	list->data[list->length] = *g;
	list->length = list->length + 1;
}

int32_t lenGList(glyphList_t* list) {
	return list->length;
}

glyph_t* getGList(glyphList_t* list, int i) {
	return &list->data[i];
}