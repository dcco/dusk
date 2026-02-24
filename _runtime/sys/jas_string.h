#ifndef JAS_STRING_H
#define JAS_STRING_H

	/*
		jas_string: custom struct for storing strings
		- lazy evaluation strategy for string concatenation
	*/

const int8_t STR_DYNAMIC = 0x01;

typedef struct _JasString {
		// byte 0 - permanent / dynamically allocated 
	int8_t flag;
	int32_t len;
		// data != NULL --> c1, c2 == NULL
		// (c1 != NULL <-> c2 != NULL) --> data = <undefined>
	struct _JasString* c1;
	struct _JasString* c2;
	char* data;
} JasString;

char* _flatten_string(char* buf, JasString* cur) {
	if (cur->c1 != NULL) {
		buf = _flatten_string(buf, cur->c1);
		if (cur->c1->flag & STR_DYNAMIC) {
			free(cur->c1->data);
			free(cur->c1);
		}
		buf = _flatten_string(buf, cur->c2);
		if (cur->c2->flag & STR_DYNAMIC) {
			free(cur->c2->data);
			free(cur->c2);
		}
		cur->c1 = NULL;
		cur->c2 = NULL;
		return buf;
	} else {
		int copyLen = sizeof(char) * cur->len;
		memcpy(buf, cur->data, copyLen);
		return buf + copyLen;
	}
}

void flatten_string(JasString* s) {
	char* data = malloc(sizeof(char) * (s->len + 1));
	_flatten_string(data, s);
	data[s->len] = 0;
	s->data = data;
}

extern JasString* _String_builtin_add(void* _s1, void* _s2) {
	JasString* s1 = (JasString*) _s1;
	JasString* s2 = (JasString*) _s2;
	JasString* sx = (JasString*) malloc(sizeof(JasString));
	sx->flag = STR_DYNAMIC;
	sx->len = s1->len + s2->len;
	sx->c1 = s1;
	sx->c2 = s2;
	return sx;
}

extern int32_t _String_builtin_length(void* s) {
	return (int32_t) ((JasString*) s)->len;
}

	/*
		string conversion functions
	*/

extern JasString* _Int_builtin_toString(int32_t i) {
	int length = snprintf(NULL, 0, "%d", i);
	char *data = malloc(sizeof(char) * (length + 1));
	snprintf(data, length + 1, "%d", i);
	JasString* s = (JasString*) malloc(sizeof(JasString));
	s->flag = STR_DYNAMIC;
	s->len = length;
	s->c1 = NULL;
	s->c2 = NULL;
	s->data = data;
	return s;
}

extern JasString* _Float_builtin_toString(float f) {
	int length = snprintf(NULL, 0, "%f", f);
	char *data = malloc(sizeof(char) * (length + 1));
	snprintf(data, length + 1, "%f", f);
	JasString* s = (JasString*) malloc(sizeof(JasString));
	s->flag = STR_DYNAMIC;
	s->len = length;
	s->c1 = NULL;
	s->c2 = NULL;
	s->data = data;
	return s;
}

#endif