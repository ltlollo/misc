// musl-gcc self $cflags

#include <assert.h>
#include <err.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef uint64_t num_t;
typedef struct { num_t lo, hi, count; } Prob;
typedef struct { int o2, o1; } Model;

static const int NB = 4;
static const int EOM = 256;

static const num_t HI = 0xffffffffu;
static const num_t MD = 0x80000000u;
static const num_t HM = 0xc0000000u;
static const num_t ML = 0x40000000u;
static num_t cum[257 * 257 * 258];
static unsigned short bmap[258][8];

static void usage(void);
static inline num_t *at(Model *, int);
static inline void updatep(Model *, int);
static inline Prob getp(Model *, int);
static inline void init(Model *);
static inline int getch(Model *, num_t, Prob *);
static inline int get(void);
static inline int putb(unsigned short);
static inline int getb(unsigned short *);
static inline int flushbs(void);
static inline int putbs(unsigned short, int *);

extern char *__progname;

int encode(void);
int decode(void);

int
main(int argc, char *argv[]) {
    if (argc - 1 < 1) {
        warnx("not enough arguments");
        usage();
        return 1;
    }
    if (strcmp(argv[1], "-h") == 0) {
        usage();
        return 0;
    }
    if (argc - 1 > 1) {
        if (freopen(argv[2], "r", stdin) == NULL) {
            err(1, "freopen");
        }
    }
    if (argc - 1 > 2) {
        if (freopen(argv[3], "w", stdout) == NULL) {
            err(1, "freopen");
        }
    }
    if (strcmp(argv[1], "e") == 0) {
        if (encode() == -1) {
            err(1, "encode");
        }
        return 0;
    } else if (strcmp(argv[1], "d") == 0) {
        if (decode() == -1) {
            err(1, "decode");
        }
        return 0;
    }
    usage();
    return 1;
}

int
encode(void) {
    int c, pending = 0;
    num_t hi = HI, lo = 0;
    Model m;
    init(&m);
    do {
        c = get();
        Prob p = getp(&m, c);
        num_t range = hi - lo + 1;
        hi = lo + (range * p.hi / p.count) - 1;
        lo = lo + (range * p.lo / p.count);
        while (1) {
            if (hi < MD) {
                if (putbs(0, &pending)) {
                    return -1;
                }
            } else if (lo >= MD) {
                if (putbs(1, &pending)) {
                    return -1;
                }
            } else if (lo >= ML && hi < HM) {
                ++pending;
                lo -= ML;
                hi -= ML;
            } else {
                break;
            }
            hi = ((hi << 1) | 1) & HI;
            lo = ((lo << 1) | 0) & HI;
        }
    } while (c != EOM);
    pending += NB * 8;
    if (lo < ML) {
        if (putbs(0, &pending)) {
            return -1;
        }
    } else {
        if (putbs(1, &pending)) {
            return -1;
        }
    }
    return flushbs();
}

int
decode(void) {
    int c;
    unsigned short b;
    Prob p;
    num_t hi = HI, lo = 0, va = 0;
    Model m;
    init(&m);
    for (unsigned i = 0; i < 258; ++i) {
        for (unsigned j = 0; j < 8; ++j) {
            bmap[i][j] = (i >> (7 - j)) & 1;
        }
    }
    for (int i = 0; i < NB; ++i) {
        if ((c = getchar()) == EOF) {
            while (i--) {
                putchar((va >> (i * 8)) & 0xff);
            }
            return 0;
        }
        va = (va << 8) | c;
    }
    while (1) {
        num_t range = hi - lo + 1;
        num_t scaled = ((va - lo + 1) * *at(&m, 257) - 1) / range;
        if ((c = getch(&m, scaled, &p)) == -1) {
            return -1;
        }
        if (c == EOM) {
            break;
        }
        if (putchar(c) == -1) {
            return -1;
        }
        hi = lo + (range * p.hi / p.count) - 1;
        lo = lo + (range * p.lo / p.count);
        while (1) {
            if (hi < MD) {
            } else if (lo >= MD) {
                va -= MD;
                lo -= MD;
                hi -= MD;
            } else if (lo >= ML && hi < HM) {
                va -= ML;
                lo -= ML;
                hi -= ML;
            } else {
                break;
            }
            if (getb(&b) == -1) {
                return -1;
            }
            hi = ((hi << 1) | 1) & HI;
            lo = ((lo << 1) | 0) & HI;
            va = ((va << 1) | b) & HI;
        }
    }
    return 0;
}

static inline void
init(Model *m) {
    m->o2 = EOM;
    m->o1 = EOM;
    for (int i = 0; i < 257 * 257 * 258; ++i) {
        cum[i] = i % 258;
    }
}

static inline Prob
getp(Model *m, int pos) {
    Prob res = {*at(m, pos), *at(m, pos + 1), *at(m, 257)};
    updatep(m, pos);
    return res;
}

static inline num_t *
at(Model *m, int pos) {
    return cum + 258 * 257 * m->o2 + 258 * m->o1 + pos;
}

static inline void
updatep(Model *m, int pos) {
    if (*at(m, 257) != HI) {
        for (int i = pos + 1; i < 258; ++i) {
            ++*at(m, i);
        }
    }
    m->o2 = m->o1;
    m->o1 = pos;
}

static inline int
getch(Model *m, num_t scale, Prob *p) {
    for (int i = 0; i < 257; ++i) {
        if (scale < *at(m, i + 1)) {
            p->lo = *at(m, i);
            p->hi = *at(m, i + 1);
            p->count = *at(m, 257);
            updatep(m, i);
            return i;
        }
    }
    return -1;
}

static inline int
get(void) {
    int c = getchar();
    if (c != EOF) {
        return c;
    }
    return EOM;
}

static inline int
putb(unsigned short b) {
    static unsigned short c = 0, count = 0, bits[8];
    bits[count++ % 8] = b;
    if (count % 8 == 0) {
        c = (bits[0] << 7) | (bits[1] << 6) | (bits[2] << 5) | (bits[3] << 4) |
            (bits[4] << 3) | (bits[5] << 2) | (bits[6] << 1) | (bits[7] << 0);
        return putchar(c);
    }
    return 0;
}

static inline int
getb(unsigned short *bit) {
    static int c = 0, count = 0;
    if ((count & 7) == 0) {
        if ((c = getchar()) == EOF) {
            return -1;
        }
    }
    *bit = bmap[c][count++ & 7];
    return 0;
}

static inline int
flushbs(void) {
    for (int i = 0; i < 7; ++i) {
        if (putb(0) == -1) {
            return -1;
        }
    }
    return 0;
}

static inline int
putbs(unsigned short bit, int *pending) {
    if (putb(bit) == -1) {
        return -1;
    }
    for (int i = 0; i < *pending; ++i) {
        if (putb(!bit) == -1) {
            return -1;
        }
    }
    *pending = 0;
    return 0;
}

static void
usage(void) {
    fprintf(stderr, "Usage:\t%s {e|d} [in] [out]"
                    "\n\t\te: encode"
                    "\n\t\td: decode"
                    "\n\t\tin: input file (default: stdin)"
                    "\n\t\tout: output file (default: stdout)"
                    "\n\t%s\t-h"
                    "\n\t\t-h: this message"
                    "\nScope:\tSimple, inefficient implementation of "
                    "Markoff-2 algebraic encoding"
                    "\n",
            __progname, __progname);
}
