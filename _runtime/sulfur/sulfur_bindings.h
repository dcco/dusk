#ifndef SULFUR_BINDINGS_H
#define SULFUR_BINDINGS_H

#include "sulfur.h"
#include "sulfur_rom.h"

sulfur_t* sulfur = NULL;

extern void _Glyph_Sys_Sulfur_draw(int8_t raw[20])
{
	addGlyph(sulfur, (glyph_t*) raw);
	//addRList(sulfur->back_buffer, g);
}
/*
extern void _Int_Sys_Sulfur_drawBox(int x, int y, int w, int h)
{
	glyph_t g;
	g.type = C_BOX;
	g.a = x;
	g.b = y;
	g.c = w;
	g.d = h;

	addGList(sulfur->back_buffer, &g);
}*/

typedef struct {
	int32_t a;
	int32_t b;
	int32_t c;
	int32_t d;
} box_t;
/*
extern void _t4_Sys_Sulfur_drawTX(box_t* b)
{
	glyph_t g;
	g.type = C_BOX;
	g.a = b->a;
	g.b = b->b;
	g.c = b->c;
	g.d = b->d;

	addGList(sulfur->back_buffer, &g);
}*/

extern void _none_Sys_Sulfur_refresh()
{
	swapBackBuffer(sulfur);
}

#endif