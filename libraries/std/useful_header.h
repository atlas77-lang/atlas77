#ifndef PORTABLE_TIMER_H
#define PORTABLE_TIMER_H

/* Minimal uint64_t for old compilers */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <stdint.h>
#else
typedef unsigned long long uint64_t;
#endif

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static uint64_t timer_now_ns(void) {
    LARGE_INTEGER freq;
    LARGE_INTEGER cnt;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&cnt);
    return (uint64_t)((cnt.QuadPart * 1000000000ULL) / freq.QuadPart);
}

#else /* POSIX / macOS */

#include <time.h>

#if defined(__MACH__) && !defined(CLOCK_MONOTONIC)
/* macOS pre-10.12 fallback using mach_absolute_time */
#include <mach/mach_time.h>

static uint64_t timer_now_ns(void) {
    static mach_timebase_info_data_t tb = {0,0};
    if (tb.denom == 0) mach_timebase_info(&tb);
    uint64_t v = mach_absolute_time();
    return (v * (uint64_t)tb.numer) / (uint64_t)tb.denom;
}

#else
/* POSIX clock_gettime (Linux, modern macOS) */
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#endif
static uint64_t timer_now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
#endif

#endif /* _WIN32 */

static uint64_t timer_elapsed_ns(uint64_t start_ns) {
    return timer_now_ns() - start_ns;
}

static double timer_elapsed_s(uint64_t start_ns) {
    return (timer_elapsed_ns(start_ns)) / 1e9;
}

#endif /* PORTABLE_TIMER_H */
