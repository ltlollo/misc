// gcc self $cflags -fopenmp -Wall -Wextra -pedantic -o banana

#define _GNU_SOURCE
#include <assert.h>
#include <err.h>
#include <omp.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define ALGN 32
#define align(x, n) ((x + n - 1) & ~(n - 1))
#define __aligned(x, n)                                                       \
    __builtin_assume_aligned(                                                 \
            (void *)((uintptr_t)((char *)x + n - 1) & ~(uintptr_t)(n - 1)),   \
            n)

#define swap(a, b, tmp)                                                       \
    do {                                                                      \
        tmp = a;                                                              \
        a = b;                                                                \
        b = tmp;                                                              \
    } while (0)

typedef struct BwtBuf {
    size_t size;
    void *mem;
} BwtBuf;

int
bwcmp(const void *p, const void *q, void *sp) {
    return memcmp(*(char **)p, *(char **)q, *(size_t *)sp);
}

char **
idx_of(struct BwtBuf *b) {
    return __aligned(b->mem, ALGN);
}

char *
buf_of(struct BwtBuf *b) {
    return __aligned(idx_of(b) + b->size * sizeof(char *), ALGN);
}

void
qsort_s(void *base,
        size_t nmemb,
        size_t size,
        int (*cmp)(const void *, const void *, void *),
        void *arg) {
    qsort_r(base, nmemb, size, cmp, arg);
}

void *
bsearch_s(const void *key,
          const void *base,
          size_t nmemb,
          size_t size,
          int (*cmp)(const void *, const void *, void *),
          void *arg) {
    void *res = NULL;
    size_t lb = 0;
    size_t rb = nmemb;
    size_t i;
    int ord;

    while (lb < rb) {
        i = (lb + rb) / 2;
        res = (char *)base + (i * size);
        ord = cmp(key, res, arg);
        if (ord < 0) {
            rb = i;
        } else if (ord > 0) {
            lb = i + 1;
        } else {
            return res;
        }
    }
    return NULL;
}

long long
bwt(struct BwtBuf *b, char *msg, size_t size) {
    long long res;
    char **idx = b->mem;
    char **msgpos;
    char *mm;

    if (size == 0) {
        return -2;
    }
    if (size > b->size) {
        if ((idx = realloc(b->mem,
                           2 * size + sizeof(char *) * size + 3 * ALGN)) ==
            NULL) {
            return -1;
        }
        b->mem = idx;
        b->size = size;
    }
    idx = idx_of(b);
    mm = buf_of(b);
#pragma omp task
    memcpy(mm, msg, size);
#pragma omp task
    memcpy(mm + size, msg, size);
#pragma omp taskwait
#pragma omp for simd aligned(idx, mm : ALGN)
    for (size_t i = 0; i < size; ++i) {
        idx[i] = mm + i;
    }
    qsort_s(idx, size, sizeof(char *), bwcmp, &size);
    msgpos = bsearch_s(&msg, idx, size, sizeof(char *), bwcmp, &size);
    res = msgpos - idx;
#pragma omp for simd aligned(idx, mm : ALGN)
    for (size_t i = 0; i < size; ++i) {
        mm[i] = idx[i][size - 1];
    }
    return res;
}

int
ibwt(char *dst, struct BwtBuf *b, char *arr, size_t size, size_t n) {
    char **idx = b->mem;
    size_t one = 1;
    char *mm, *mms, *tmp;
    size_t j;

    if (size == 0) {
        return 0;
    }
    if (size > b->size) {
        if ((idx = realloc(b->mem,
                           2 * size + sizeof(char *) * size + 3 * ALGN)) ==
            NULL) {
            return -1;
        }
        b->mem = idx;
        b->size = size;
    }
    mm = buf_of(b);
    mms = __aligned(mm + size, ALGN);
#pragma omp task
    memcpy(mm, arr, size);
#pragma omp task
    memcpy(mm + size, arr, size);
#pragma omp taskwait
#pragma omp parallel
    {
#pragma omp for simd aligned(idx, mm, mms : ALGN)
        for (size_t i = 0; i < size; ++i) {
            idx[i] = arr + i;
        }
#pragma omp single
        qsort_s(idx, size, sizeof(char *), bwcmp, &one);
#pragma omp for simd aligned(idx, mm, mms : ALGN)
        for (size_t i = 0; i < size; ++i) {
            mm[i] = *idx[i];
        }
#pragma omp for simd aligned(idx, mm, mms : ALGN)
        for (size_t i = 0; i < size; ++i) {
            idx[i] -= (ptrdiff_t)arr;
        }
    }

    for (j = 0;;) {
        *dst++ = mm[n];
        if (++j == size) {
            break;
        }
#pragma omp for simd aligned(idx, mm, mms : ALGN)
        for (size_t i = 0; i < size; ++i) {
            mms[i] = *(mm + (ptrdiff_t)idx[i]);
        }
        swap(mm, mms, tmp);
    }
    return 0;
}

#undef __aligned
#undef align
#undef swap

int
main(int argc, char *argv[]) {
    char *mm;
    long long pos;
    long long size;
    char out[1 << 20] = { 0 };
    char in[1 << 20];

    (void)argc;
    (void)argv;

    BwtBuf b = { 0, NULL };

    size = fread(in, 1, 1 << 20, stdin);
    assert(size > 0 && size < (1 << 20));
    pos = bwt(&b, in, size);
    assert(pos > 0);

    fprintf(stderr, "p:%lld s: %lld\n", pos, size);

    mm = buf_of(&b);
    memcpy(in, mm, size);

    ibwt(out, &b, in, size, pos);
    fwrite(out, 1, size, stdout);

    return 0;
}

