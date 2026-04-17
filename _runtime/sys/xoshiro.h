#ifndef XOSHIRO_H
#define XOSHIRO_H

typedef struct xoshiro_state {
	uint64_t s[4];
} xoshiro_state_t;

static uint64_t splitmix64_next(uint64_t* x) {
	uint64_t z = (*x += 0x9E3779B97f4A7C15ULL);
	z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
	z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
	return z ^ (z >> 31);
}

void xoshiro_seed(xoshiro_state_t *state, uint64_t seed) {
	uint64_t s_reg = seed;
	for (int i = 0; i < 4; i++) {
		state->s[i] = splitmix64_next(&s_reg);
	}
}

static inline uint64_t rotate_left(const uint64_t x, int k) {
	return (x << k) | (x >> (64 - k));
}

uint64_t xoshiro_next(xoshiro_state_t* state) {
	const uint64_t n = rotate_left(state->s[0] + state->s[3], 23) + state->s[0];
	const uint64_t t = state->s[1] << 17;

	state->s[2] ^= state->s[0];
	state->s[3] ^= state->s[1];
	state->s[1] ^= state->s[2];
	state->s[0] ^= state->s[3];

	state->s[2] ^= t;
	state->s[3] = rotate_left(state->s[3], 45);

	return n;
}

#endif
