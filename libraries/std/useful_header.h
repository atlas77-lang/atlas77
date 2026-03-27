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
// #include <raylib.h>

int64_t clocks_per_sec()
{
    return CLOCKS_PER_SEC;
}

#endif /* ATLAS77_USEFUL_HEADER_H */
