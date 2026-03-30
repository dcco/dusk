#ifndef GC_H
#define GC_H

extern void* gc_alloc(int32_t size) {
	return malloc(size);
}

#endif