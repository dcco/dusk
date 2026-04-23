#ifndef RENDER_DATA_H
#define RENDER_DATA_H

	/*
		render_data: a dual 2d + 3d render object buffer
	*/

typedef struct renderData {
	int8_t active3d;
	renderList_t list2d;
	renderTable_t table3d;
	int32_t varTotal;
	gl_val_t* varList;
} renderData_t;

renderData_t* newRData(size_t unitSize2d, size_t unitSize3d) {
	renderData_t* rd = (renderData_t*) malloc(sizeof(renderData_t));
	initRList(&rd->list2d, unitSize2d);
	rd->active3d = 0;
	if (unitSize3d != 0) {
		initRTable(&rd->table3d, unitSize3d);
		rd->active3d = 1;
	}
	rd->varTotal = 10;
	rd->varList = malloc(sizeof(gl_val_t) * 10);
	return rd;
}

void clearRData(renderData_t* rd) {
	clearRList(&rd->list2d);
	if (rd->active3d) clearRTable(&rd->table3d);
}

void addVarRData(renderData_t* rd, int32_t i, gl_val_t* v) {
	if (i >= rd->varTotal) {
		rd->varTotal = i + 1;
		rd->varList = realloc(rd->varList, sizeof(gl_val_t) * (i + 1));
	}
	copyRenderVar(&rd->varList[i], v);
}

#endif