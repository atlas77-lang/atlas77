#ifndef ATLAS77_H
#define ATLAS77_H

/* Minimal uint64_t for old compilers */
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <stdint.h>
#else
#include <limits.h>

/* 64-bits types */
typedef signed long long int64_t;
typedef unsigned long long uint64_t;

#define INT64_MAX 9223372036854775807LL
#define INT64_MIN (-INT64_MAX - 1LL)
#define UINT64_MAX 18446744073709551615ULL

/* 8-bit */
typedef signed char int8_t;
typedef unsigned char uint8_t;

/* 16-bit */
#if INT_MAX == 32767
typedef signed int int16_t;
typedef unsigned int uint16_t;
#else
typedef signed short int16_t;
typedef unsigned short uint16_t;
#endif

/* 32-bit */
#if INT_MAX == 2147483647L
typedef signed int int32_t;
typedef unsigned int uint32_t;
#else
typedef signed long int32_t;
typedef unsigned long uint32_t;
#endif

#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#if defined(_WIN32)
#include <windows.h>
#endif

/* Define the noreturn macro based on the compiler */
#if defined(_MSC_VER)
/* Microsoft Visual C++ */
#define PANIC_NORETURN __declspec(noreturn)
#elif defined(__GNUC__) || defined(__clang__) || defined(__TINYC__) || defined(__INTEL_COMPILER)
/* GCC, Clang, TCC, and Intel CC all support GNU-style attributes */
#define PANIC_NORETURN __attribute__((noreturn))
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
/* Fallback for any other standard C11 compiler */
#define PANIC_NORETURN _Noreturn
#else
/* Fallback for unknown compilers */
#define PANIC_NORETURN
#endif

/* Aborts the process after printing a panic message. */
PANIC_NORETURN static inline void panic(const char *message)
{
    fprintf(stderr, "PANIC: %s\n", message);
    exit(1);
}

extern inline uint64_t atlas77_input_impl(uint8_t *buf, uint64_t size)
{
    uint64_t len;
    char *raw;

    if (buf == NULL || size == 0)
    {
        return 0;
    }

    raw = (char *)(void *)buf;
    if (fgets(raw, (int)size, stdin) == NULL)
    {
        return 0;
    }

    len = (uint64_t)strlen(raw);

    /* Trim trailing newline(s) so returned length matches captured content. */
    while (len > 0 && (raw[len - 1] == '\n' || raw[len - 1] == '\r'))
    {
        raw[len - 1] = '\0';
        len--;
    }

    return len;
}

int64_t clocks_per_sec(void)
{
    return CLOCKS_PER_SEC;
}

// Return the amount of nanoseconds since the Unix epoch (January 1, 1970).
int64_t atlas77_instant_now(void)
{
#if defined(_WIN32)
    FILETIME ft;
    ULARGE_INTEGER ticks;
    /* Windows epoch (1601) to Unix epoch (1970), in 100ns units. */
    const uint64_t EPOCH_DIFF_100NS = 116444736000000000ULL;

#if defined(_WIN32_WINNT) && _WIN32_WINNT >= 0x0602
    GetSystemTimePreciseAsFileTime(&ft);
#else
    GetSystemTimeAsFileTime(&ft);
#endif

    ticks.LowPart = ft.dwLowDateTime;
    ticks.HighPart = ft.dwHighDateTime;
    if (ticks.QuadPart < EPOCH_DIFF_100NS)
    {
        panic("Failed to get current time");
    }
    return (int64_t)((ticks.QuadPart - EPOCH_DIFF_100NS) * 100ULL);
#elif defined(CLOCK_REALTIME)
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
    {
        panic("Failed to get current time");
    }
    return (int64_t)ts.tv_sec * 1000000000LL + (int64_t)ts.tv_nsec;
#elif defined(TIME_UTC)
    struct timespec ts;
    if (timespec_get(&ts, TIME_UTC) == 0)
    {
        panic("Failed to get current time");
    }
    return (int64_t)ts.tv_sec * 1000000000LL + (int64_t)ts.tv_nsec;
#else
    /* Last-resort fallback: process CPU time, not wall-clock time. */
    clock_t c = clock();
    if (c == (clock_t)-1)
    {
        panic("Failed to get current time");
    }
    return ((int64_t)c * 1000000000LL) / (int64_t)CLOCKS_PER_SEC;
#endif
}

static uint64_t atlas77_string_hash(const char *s)
{
    if (s == NULL)
    {
        return UINT64_C(0);
    }
    uint64_t hash = UINT64_C(1469598103934665603);
    while (*s != '\0')
    {
        hash ^= (uint64_t)(unsigned char)(*s);
        hash *= UINT64_C(1099511628211);
        ++s;
    }
    return hash;
}

static uint64_t c_errno()
{
    return errno;
}

#endif /* ATLAS77_H */
