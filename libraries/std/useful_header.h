#ifndef ATLAS77_USEFUL_HEADER_H
#define ATLAS77_USEFUL_HEADER_H

#ifndef ATLAS77_NS_INTEGER_TYPES
#define ATLAS77_NS_INTEGER_TYPES

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

#endif /* ATLAS77_NS_INTEGER_TYPES */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifndef ATLAS77_NS_TIME
#define ATLAS77_NS_TIME

typedef struct
{
    int64_t sec;
    int64_t nsec;
} atlas77_time_layout;

/* Returns the current wall-clock time split into seconds and nanoseconds. */
static inline atlas77_time_layout atlas77_now_layout(void)
{
    atlas77_time_layout t;
#ifdef _WIN32
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;

    /* FILETIME epoch is 1601-01-01 in 100ns ticks. */
    {
        uint64_t unix_ticks = uli.QuadPart - 116444736000000000ULL;
        uint64_t ns = unix_ticks * 100ULL;
        t.sec = (int64_t)(ns / 1000000000ULL);
        t.nsec = (int64_t)(ns % 1000000000ULL);
    }
#else
    struct timespec ts;
#if defined(CLOCK_REALTIME)
    clock_gettime(CLOCK_REALTIME, &ts);
#else
    ts.tv_sec = time(NULL);
    ts.tv_nsec = 0;
#endif
    t.sec = (int64_t)ts.tv_sec;
    t.nsec = (int64_t)ts.tv_nsec;
#endif
    return t;
}

/* Returns a stable pointer to a freshly updated time snapshot. */
static inline const atlas77_time_layout *atlas77_now_layout_ptr(void)
{
    static atlas77_time_layout t;
    t = atlas77_now_layout();
    return &t;
}

/* Allocates and copies bytes from a null-terminated C string as uint8 data. */
static inline uint8_t *atlas77_strdup_u8(const char *s)
{
    size_t len;
    char *out;

    if (s == NULL)
    {
        return NULL;
    }

    len = strlen(s);
    out = (char *)malloc(len + 1);
    if (out == NULL)
    {
        return NULL;
    }
    memcpy(out, s, len + 1);
    return (uint8_t *)(void *)out;
}

/* Formats a time value using a strftime format string and returns heap-allocated text. */
static inline uint8_t *atlas77_format_time_impl(const void *time_raw, const uint8_t *fmt_raw)
{
    const atlas77_time_layout *t = (const atlas77_time_layout *)time_raw;
    const char *fmt = (const char *)(const void *)fmt_raw;
    time_t sec;
    struct tm tm_buf;
    struct tm *tm_ptr;
    char temp[256];
    size_t len;
    char *out;

    if (t == NULL)
    {
        return atlas77_strdup_u8("<null-time>");
    }

    if (fmt == NULL || fmt[0] == '\0')
    {
        fmt = "%Y-%m-%d %H:%M:%S";
    }

    sec = (time_t)t->sec;
#ifdef _WIN32
    if (localtime_s(&tm_buf, &sec) != 0)
    {
        return atlas77_strdup_u8("<invalid-time>");
    }
    tm_ptr = &tm_buf;
#else
    tm_ptr = localtime_r(&sec, &tm_buf);
    if (tm_ptr == NULL)
    {
        return atlas77_strdup_u8("<invalid-time>");
    }
#endif

    len = strftime(temp, sizeof(temp), fmt, tm_ptr);
    if (len == 0)
    {
        return atlas77_strdup_u8("<format-error>");
    }

    out = (char *)malloc(len + 1);
    if (out == NULL)
    {
        return NULL;
    }
    memcpy(out, temp, len + 1);
    return (uint8_t *)(void *)out;
}

/* Formats a time value using an ISO-like default pattern. */
static inline uint8_t *atlas77_format_time_iso_impl(const void *time_raw)
{
    return atlas77_format_time_impl(
        time_raw,
        (const uint8_t *)(const void *)"%Y-%m-%dT%H:%M:%S");
}

/* Suspends execution for the duration represented by the provided time value. */
static inline void atlas77_sleep_impl(const void *time_raw)
{
    const atlas77_time_layout *t = (const atlas77_time_layout *)time_raw;
    int64_t sec;
    int64_t nsec;

    if (t == NULL)
    {
        return;
    }

    sec = t->sec;
    nsec = t->nsec;
    if (sec < 0)
    {
        sec = 0;
    }
    if (nsec < 0)
    {
        nsec = 0;
    }

#ifdef _WIN32
    {
        uint64_t ms = (uint64_t)sec * 1000ULL + (uint64_t)nsec / 1000000ULL;
        if (ms > 0xFFFFFFFFULL)
        {
            ms = 0xFFFFFFFFULL;
        }
        Sleep((DWORD)ms);
    }
#else
    {
        struct timespec req;
        req.tv_sec = (time_t)sec;
        req.tv_nsec = (long)(nsec % 1000000000LL);
        nanosleep(&req, NULL);
    }
#endif
}

/*
 * Export a symbol-compatible shim for `extern fun atlas77_now_impl() -> Time`
 * without introducing global libc-name collisions.
 */
#define atlas77_now_impl() (*(Time *)(void *)atlas77_now_layout_ptr())

#endif /* ATLAS77_NS_TIME */

#ifndef ATLAS77_NS_MEMORY
#define ATLAS77_NS_MEMORY

// TODO: Once size_of<T> is implemented in Atlas77, we can make this more general
// because we will know the size at compile time.
/* Swaps two 64-bit values in place. */
static inline void __atlas77_c_swap(void *a, void *b)
{
    uint64_t temp = *(uint64_t *)a;
    *(uint64_t *)a = *(uint64_t *)b;
    *(uint64_t *)b = temp;
}

/* Aborts the process after printing a panic message. */
static inline void panic(const char *message)
{
    fprintf(stderr, "PANIC: %s\n", message);
    exit(1);
}

#endif /* ATLAS77_NS_MEMORY */

#ifndef ATLAS77_NS_STRING
#define ATLAS77_NS_STRING

#include <ctype.h>

#endif /* ATLAS77_NS_STRING */

#ifndef ATLAS77_NS_VECTOR
#define ATLAS77_NS_VECTOR
/* Reserved section for vector runtime hooks. */
#endif /* ATLAS77_NS_VECTOR */

#ifndef ATLAS77_NS_OPTIONAL
#define ATLAS77_NS_OPTIONAL
/* Reserved section for optional runtime hooks. */
#endif /* ATLAS77_NS_OPTIONAL */

#ifndef ATLAS77_NS_EXPECTED
#define ATLAS77_NS_EXPECTED
/* Reserved section for expected runtime hooks. */
#endif /* ATLAS77_NS_EXPECTED */

#ifndef ATLAS77_NS_MATH
#define ATLAS77_NS_MATH
/* Reserved section for math runtime hooks. */
#endif /* ATLAS77_NS_MATH */

#ifndef ATLAS77_NS_IO
#define ATLAS77_NS_IO
/* Reserved section for io runtime hooks */

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
#endif /* ATLAS77_NS_IO */

// TODO: This should get removed once we get the build system.
// There should be some kind of "c_library" field and another one for linking
// To ease the developers and streamline the process
#include "raylib.h"

#endif /* ATLAS77_USEFUL_HEADER_H */
