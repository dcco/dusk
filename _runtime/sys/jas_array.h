#ifndef JAS_ARRAY_H
#define JAS_ARRAY_H

	/*
		array functions
	*/

typedef struct _JasArray {
	int32_t length;
	void* data;
} JasArray;

typedef struct _JasTensor {
	int32_t* dims;
	void* data;
} JasTensor;

extern JasArray* _builtin_Int_newArray(int32_t length) {
	JasArray* a = (JasArray*) malloc(sizeof(JasArray));
	a->length = length;
	a->data = malloc(sizeof(int32_t) * length);
	return a;
}

extern JasTensor* _builtin_Int_newTensor(int32_t dim, int32_t size) {
	JasTensor* a = (JasTensor*) malloc(sizeof(JasTensor));
	a->dims = malloc(sizeof(int32_t) * dim);
	a->data = malloc(size);
	return a;
}

#endif