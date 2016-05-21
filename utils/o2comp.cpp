// gpp self $cflags

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef uint64_t num_t;
typedef struct { num_t lo, hi, count; } Prob;
typedef struct {
    unsigned o2, o1;
    num_t *cum;
} Model;

static const unsigned NB = 4;
static const unsigned EOM = 256;

static const num_t HI = 0xffffffffu;
static const num_t MD = 0x80000000u;
static const num_t HM = 0xc0000000u;
static const num_t ML = 0x40000000u;

num_t *
at(Model *m, unsigned pos) {
    return m->cum + 258 * m->o1 * m->o2 + 258 * m->o1 + pos;
}

Prob
getp(Model *m, unsigned pos) {
    Prob res = {*at(m, pos), *at(m, pos + 1), *at(m, 257)};
    if (*at(m, 257) != HI) {
        for (unsigned i = pos + 1; i < 258; ++i) {
            ++*at(m, i);
        }
    }
    m->o2 = m->o1;
    m->o1 = pos;
    return res;
}

void
update(Model *m, unsigned pos) {
    if (*at(m, 257) != HI) {
        for (unsigned i = pos + 1; i < 258; ++i) {
            ++*at(m, i);
        }
    }
    m->o2 = m->o1;
    m->o1 = pos;
}

int
init(Model *m) {
    m->o2 = EOM;
    m->o1 = EOM;
    if ((m->cum = (num_t *)malloc(sizeof(num_t) * 258 * 258 * 258)) == NULL) {
        return -1;
    }
    for (unsigned i = 0; i < 258 * 258 * 258; ++i) {
        m->cum[i] = i % 258;
    }
    return 0;
}


int
getch(Model *m, num_t scale, Prob *p) {
    for (unsigned i = 0; i < 257; ++i) {
        if (scale < *at(m, i + 1)) {
            p->lo = *at(m, i);
            p->hi = *at(m, i + 1);
            p->count = *at(m, 257);
            if (*at(m, 257) != HI) {
                for (unsigned j = i + 1; j < 258; ++j) {
                    ++*at(m, j);
                }
            }
            m->o2 = m->o1;
            m->o1 = i;
            return i;
        }
    }
    return -1;
}

unsigned
get() {
    int c = getchar();
    if (c != EOF) {
        return c;
    }
    return EOM;
}

int
putb(bool b) {
    static unsigned c = 0;
    static unsigned count = 0;
    c = (c << 1) | b;
    if ((++count) % 8 == 0) {
        return putchar(c);
    }
    return 0;
}
int
getb(bool *bit) {
    static int c = 0;
    static unsigned count = 0;
    if (count % 8 == 0) {
        if ((c = getchar()) == EOF) {
            return -1;
        }
    }
    *bit = ((c >> (7 - (count++ % 8)) & 1));
    return 0;
}

int
flushbs() {
    for (unsigned i = 0; i < 7; ++i) {
        if (putb(0) == -1) {
            return -1;
        }
    }
    return 0;
}

int
putbs(bool bit, unsigned *pending) {
    if (putb(bit) == -1) {
        return -1;
    }
    for (unsigned i = 0; i < *pending; ++i) {
        if (putb(!bit) == -1) {
            return -1;
        }
    }
    *pending = 0;
    return 0;
}

int
encode() {
    num_t hi = HI;
    num_t lo = 0;
    unsigned pending = 0;
    Model m;
    if (init(&m) == -1) {
        return -1;
    }
    int c;
    do {
        c = get();
        Prob p = getp(&m, c);
        num_t range = hi - lo + 1;
        hi = lo + (range * p.hi / p.count) - 1;
        lo = lo + (range * p.lo / p.count);
        while (true) {
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
    for (unsigned i = 0; i < 3; ++i) {
        Prob p = getp(&m, c);
        num_t range = hi - lo + 1;
        hi = lo + (range * p.hi / p.count) - 1;
        lo = lo + (range * p.lo / p.count);
        while (true) {
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

    }
    ++pending;
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
decode() {
    num_t hi = HI;
    num_t lo = 0;
    num_t va = 0;
    int c;
    bool b;
    Prob p;
    Model m;
    if (init(&m) == -1) {
        return -1;
    }
    for (unsigned i = 0; i < NB; ++i) {
        if ((c = getchar()) == EOF) {
            while (i--) {
                putchar((va >> (i * 8)) & 0xff);
            }
            return 0;
        }
        va = (va << 8) | c;
    }
    while (true) {
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
        while (true) {
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

int
main(int argc, char *argv[]) {
    if (argc - 1 < 1) {
        return 1;
    }
    if (argc - 1 > 1) {
        if (freopen(argv[2], "r", stdin) == NULL) {
            return 1;
        }
    }
    if (argc - 1 > 2) {
        if (freopen(argv[3], "w", stdout) == NULL) {
            return 1;
        }
    }
    if (strcmp(argv[1], "e") == 0) {
        return encode();
    } else if (strcmp(argv[1], "d") == 0) {
        return decode();
    }
    return 1;
}
