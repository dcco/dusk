#ifndef OS_H
#define OS_H

	/*
		os - console / printing functions
	*/

extern void _String_Sys_Os_print(void* _s) {
	JasString* s = (JasString*) _s;
	if (s->c1 != NULL) flatten_string(s);
	printf("%s\n", s->data);
}

	/*
		os - randomization
	*/

extern int32_t _Int_Sys_Os_randomInt(int32_t i) {
	return rand() % i;
}

extern float _none_Sys_Os_randomFloat() {
	return ((float) rand()) / ((float) RAND_MAX);
}

	/*
		os - time
	*/

extern int64_t _none_Sys_Os_time() {
	return time_ns();
}

#endif