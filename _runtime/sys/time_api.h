#ifndef TIME_API_H
#define TIME_API_H

inline static int64_t time_ns() {
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (int64_t) ts.tv_sec * 1000000000LL + ts.tv_nsec;
}

#endif