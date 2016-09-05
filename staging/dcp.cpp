// gpp self $cppflags

#include <err.h>
#include <fcntl.h>
#include <immintrin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

// cpp

#include <boost/functional/hash.hpp>
#include <unordered_map>

struct Num;
struct Hash;
struct Range;
template <typename T> struct Ring;
template <typename T> struct RingBuf;

using map_t = std::unordered_map<Range, Num, Hash>;
using num_t = uint16_t;
using num_fast_t = uint_fast16_t;

static constexpr auto max_num = (num_t)0xffffffffffffffff;
static constexpr auto include_base_syms = false;
static constexpr auto advance_step_ratio = 1;
static constexpr unsigned char alphabet[0xff + 32] = {
    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,
    15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
    30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
    45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
    60,  61,  62,  63,  64,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,
    75,  76,  77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,
    90,  91,  92,  93,  94,  95,  96,  97,  98,  99,  100, 101, 102, 103, 104,
    105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
    135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
    165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
    180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
    195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
    210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224,
    225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
    255
};

static inline const char *encode_sym(const char *, const char *, num_t *,
                                     unsigned *, map_t &);
static inline bool memeq(const void *, const void *, std::size_t);
static const char *mkdict(const char *, const char *, map_t &);
template <typename T> static inline unsigned find_min_pos(const Ring<T> &);
template <typename T> static inline void write_out(T *beg, std::size_t size);
template <typename T> static inline void write_out(T ele);

struct Num {
    num_t data = max_num;
    constexpr bool operator==(Num lhs) const { return data == lhs.data; }
    constexpr operator num_t() const { return data; }
    void operator=(num_fast_t lhs) { data = (num_t)lhs; }
};

static constexpr auto max_ele = Num{ max_num - 2 };
static constexpr auto lit = Num{ max_num - 1 };
static constexpr auto nxt = Num{ max_num - 0 };

struct Range {
    const char *beg;
    num_t size;
    constexpr Range(const char *beg, num_fast_t size)
        : beg{ beg }, size{ (num_t)size } {}
    bool operator==(const Range &rhs) const {
        if (rhs.size != size) {
            return false;
        }
        return memeq(beg, rhs.beg, size);
    }
};

struct Hash {
    std::size_t operator()(const Range &range) const {
        return boost::hash_range(range.beg, range.beg + range.size);
    }
};

template <typename T> struct Ring {
    unsigned N;
    T *data, *beg;
    unsigned size = 0;

    auto &operator[](int pos) const {
        return data[((beg - data) + N + pos) % N];
    }
    Ring(unsigned size) : N{ size }, data{ new T[size] }, beg{ data } {}
    Ring(unsigned size, unsigned filled, T ele) : Ring(size) {
        this->size = filled;
        for (unsigned i = 0; i < filled; ++i) {
            data[i] = ele;
        }
    }
    ~Ring() { delete[] data; }
    void advance(int n) { beg = &(*this)[n]; }
    template <typename F> void advance(unsigned n, F f) {
        for (unsigned i = 0; i < n; ++i) {
            f(beg[i]);
        }
        beg = &(*this)[n];
    }
    void populate(unsigned beg, unsigned times, T ele) {
        for (unsigned i = beg; i < times; ++i) {
            (*this)[i] = ele;
        }
    }
};

template <typename T> struct RingBuf {
    unsigned nrings, bufsize;
    T *data, *beg;
    RingBuf(unsigned nrings, unsigned bufsize)
        : nrings{ nrings }, bufsize{ bufsize },
          data{ new T[nrings * bufsize] }, beg{ data } {}
    ~RingBuf() { delete[] data; }
    T *operator[](int n) const {
        return data +
               (((beg - data) / bufsize + nrings + n) % nrings) * bufsize;
    }
    auto advance(int n) { return beg = (*this)[n]; }
};

int
encode(const char *beg_orig, std::size_t size_orig, const char *beg_new,
       std::size_t size_new, unsigned stop_point = 4, unsigned ndicts = 2) {
    if (size_orig <= 32) {
        return -1;
    }
    if (size_new <= 32) {
        write_out(beg_new, size_new);
    }
    const char *end_orig = beg_orig + size_orig - 32;
    const char *end_new = beg_new + size_new - 32;
    Ring<map_t> dicts(ndicts);

    auto populate_dicts = [&]() {
        while (beg_orig != end_orig && dicts.size != ndicts) {
            map_t dict;
            if (include_base_syms && sizeof(num_t) > 1) {
                for (unsigned i = 0; i < 0xff; ++i) {
                    dict[Range((const char *)alphabet + i, 1)] = i;
                }
            }
            const char *needle_orig;
            if ((needle_orig = mkdict(beg_orig, end_orig, dict)) == nullptr) {
                beg_orig = needle_orig;
                break;
            }
            beg_orig += (needle_orig - beg_orig) / advance_step_ratio;
            dicts[dicts.size++] = std::move(dict);
        }
    };
    populate_dicts();

    RingBuf<num_t> buf(ndicts, stop_point * 2);

    Ring<unsigned> bufsizes(ndicts, dicts.size, 0);
    Ring<const char *> needles_new(ndicts, dicts.size, beg_new);

    auto populate_buf = [&](unsigned di) {
        auto &dict = dicts[di];
        auto &beg_new = needles_new[di];
        unsigned *buf_size = &bufsizes[di];
        num_t *buf_beg = buf[di];
        for (unsigned pi = 0; pi < stop_point; ++pi) {
            if ((beg_new = encode_sym(beg_new, end_new, buf_beg, buf_size,
                                      dict)) == end_new) {
                break;
            }
        }
    };
    do {
        assert(beg_new <= end_new);
        bufsizes.populate(0, dicts.size, 0);
        needles_new.populate(0, dicts.size, beg_new);
        for (unsigned di = 0; di < dicts.size; ++di) {
            populate_buf(di);
        }
    RING_MOVED:
        auto min_pos = find_min_pos(bufsizes);
        if (min_pos != 0) {
            auto orig_size = dicts.size;
            dicts.advance(min_pos, [](auto &dict) { dict.clear(); });
            bufsizes.advance(min_pos);
            needles_new.advance(min_pos);
            buf.advance(min_pos);
            for (unsigned i = 0; i < min_pos; ++i) {
                write_out(nxt);
            }
            if (orig_size == ndicts) {
                orig_size = dicts.size;
                populate_dicts();
                bufsizes.populate(orig_size, dicts.size, 0);
                needles_new.populate(orig_size, dicts.size, beg_new);
                for (unsigned i = orig_size; i < dicts.size; ++i) {
                    populate_buf(i);
                }
                if (dicts.size == ndicts) {
                    goto RING_MOVED;
                }
            }
        } else {
            write_out(buf[0], bufsizes[0]);
            beg_new = needles_new[0];
        }
    } while (beg_new != end_new);
    write_out(end_new, 32);
    return 0;
}

static inline const char *
encode_sym(const char *beg_new, const char *end_new, num_t *out_buf,
           unsigned *out_size, map_t &dict) {
    num_fast_t look_ahead = 1;
    auto end = dict.end();
    auto it = end;
    auto it_old = it;
    while ((it = dict.find(Range{ beg_new, look_ahead })) != end &&
           beg_new + look_ahead != end_new) {
        ++look_ahead;
        it_old = it;
    }
    if (look_ahead - 1 == 0) {
        out_buf[*out_size] = lit;
        out_buf[*out_size + 1] = beg_new[0];
        *out_size += 2;
    } else {
        out_buf[*out_size] = it_old->second;
        *out_size += 1;
    }
    return beg_new + look_ahead;
}

template <typename T>
static inline unsigned
find_min_pos(const Ring<T> &ring) {
    assert(ring.size != 0);
    T curr = *ring.beg;
    unsigned min_pos = 0;
    for (unsigned i = 1; i < ring.size; ++i) {
        if (ring[i] < curr) {
            curr = ring[i];
            min_pos = i;
        }
    }
    return min_pos;
}

template <typename T>
static inline void
write_out(T *beg, std::size_t size) {
    fwrite(beg, sizeof(T), size, stdout);
}

template <typename T>
static inline void
write_out(T ele) {
    fwrite(&ele, sizeof(T), 1, stdout);
}

static inline bool
memeq(const void *f, const void *s, std::size_t size) {
    // requirements:
    //      size must be non zero
    //      input buffers must be 32 byte padded
    //      little endian
    __m256i *fp = (__m256i *)f, *sp = (__m256i *)s;
    std::size_t times = size / 32;
    unsigned rest = size % 32;
    for (unsigned i = 0; i < times; ++i) {
        __m256i fr = _mm256_lddqu_si256(fp++);
        __m256i sr = _mm256_lddqu_si256(sp++);
        if (_mm256_movemask_epi8(_mm256_cmpeq_epi8(fr, sr)) != ~0) {
            return false;
        }
    }
    if (rest) {
        __m256i fr = _mm256_lddqu_si256(fp);
        __m256i sr = _mm256_lddqu_si256(sp);
        int res = _mm256_movemask_epi8(_mm256_cmpeq_epi8(fr, sr));
        unsigned mask = 0xffffffff << rest;
        return (res | mask) == 0xffffffff;
    }
    return true;
}

static const char *
mkdict(const char *beg, const char *end, map_t &dict) {
    if (end - beg == 0) {
        return nullptr;
    }
    num_fast_t count = 0, look_ahead;
    do {
        look_ahead = 1;
        while (beg + look_ahead != end) {
            auto &dict_ele = dict[Range(beg, look_ahead)];
            if (dict_ele == Num{}) {
                dict_ele = count++;
                beg += look_ahead;
                break;
            } else {
                ++look_ahead;
            }
        }
    } while (beg + look_ahead != end && count != max_ele);
    return beg + look_ahead;
}

// TODO: add BTW in? Huffman out?
// TODO: write decode procedure
// TODO: cache min bufsize in the encoder

int
main(int argc, char *argv[]) {
    if (argc - 1 != 2) {
        err(1, "NEA");
    }
    const char *addrs[2];
    std::size_t sizes[2];

    for (unsigned i = 0; i < 2; ++i) {
        int fd = open(argv[i + 1], O_RDONLY);
        if (fd == -1) {
            err(1, "open");
        }
        struct stat sb;
        if (fstat(fd, &sb) == -1) {
            err(1, "fstat");
        }
        sizes[i] = sb.st_size;
        addrs[i] = (const char *)mmap(NULL, sizes[i], PROT_READ,
                                      MAP_PRIVATE | MAP_POPULATE, fd, 0);
        if (addrs[i] == MAP_FAILED) {
            err(1, "mmap");
        }
    }
    encode(addrs[0], sizes[0], addrs[1], sizes[1], 128, 2);
    return 0;
}

