#ifndef ATLAS77_COMPILER_RUNTIME_H
#define ATLAS77_COMPILER_RUNTIME_H

/*
 * Minimal, `std`-independent C compatibility shim: guarantees fixed-width integer
 * types and a couple of universally useful headers for every Atlas77-generated
 * program, regardless of whether it imports `std`. Everything else that used to
 * live in this file (panic, I/O, time, directory listing, ...) is a `std` runtime
 * concern and lives in the `std` package's own `include/atlas77.h` instead, pulled
 * in via its `[c]` config only when a project actually depends on `std`.
 */
#include <limits.h>
#include <stdarg.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#include <stdint.h>
#else

/* 64-bit types */
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

#endif /* ATLAS77_COMPILER_RUNTIME_H */
