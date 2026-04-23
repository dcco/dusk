#ifndef OS_H
#define OS_H
	
	/*
		basic functions
	*/

extern float _Float_builtin_expo(float f, float z) {
	return powf(f, z);
}

extern float _Float_builtin_sqrt(float f) {
	return sqrtf(f);
}

extern float _Float_builtin_abs(float f) {
	return fabsf(f);
}

extern float _Float_builtin_floor(float f) {
	return floorf(f);
}

extern float _Int_builtin_toRadians(int32_t i) {
	return ((float) i) * M_PI / 180.0f;
}

	/*
		os - console / printing functions
	*/

extern void _String_Sys_Os_print(void* _s) {
	dusk_string_t* s = (dusk_string_t*) _s;
	printf("%s\n", &s->start);
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
		os - prng object
	*/

extern xoshiro_state_t* _Int_Sys_Os_newPRNG(int32_t seed) {
	xoshiro_state_t* prng = (xoshiro_state_t*) gc_alloc(sizeof(xoshiro_state_t), NULL);
	xoshiro_seed(prng, (uint64_t) seed);
	return prng;
}

extern int32_t _PRNG_Sys_Os_randomInt(xoshiro_state_t* prng, int32_t i) {
	return (int32_t) (xoshiro_next(prng) % (uint64_t) i);
}
extern float _PRNG_Sys_Os_randomFloat(xoshiro_state_t* prng) {
	uint32_t bits = (uint32_t) (xoshiro_next(prng) >> 41) | 0x3F800000u; // exponent = 127
	float f = *((float*) &bits);
	return f - 1.0f; 
}

	/*
		os - time
	*/

extern int64_t _none_Sys_Os_time() {
	return time_ns();
}

#endif