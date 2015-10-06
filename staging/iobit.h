#ifndef IOBIT_H
#define IOBIT_H

#include <stdio.h>
#include <inttypes.h>

#define likely(x)    __builtin_expect((x), 1)
#define unlikely(x)  __builtin_expect((x), 0)

namespace io {

namespace {
struct Res { int8_t err; uint64_t value; };
template<typename> struct make_ls;
template<> struct make_ls<uint8_t > { using type = int16_t; };
template<> struct make_ls<uint16_t> { using type = int32_t; };
template<> struct make_ls<uint32_t> { using type = int64_t; };
template<> struct make_ls<uint64_t> { using type = Res;     };
template<typename T> using make_ls_t = typename make_ls<T>::type;
template<typename> struct n_bits;
template<> struct n_bits<uint8_t > { static constexpr int8_t value = 8;  };
template<> struct n_bits<uint16_t> { static constexpr int8_t value = 16; };
template<> struct n_bits<uint32_t> { static constexpr int8_t value = 32; };
template<> struct n_bits<uint64_t> { static constexpr int8_t value = 64; };
template<typename T> struct max_bits{
    static constexpr T value = (T)0xFFFFFFFFFFFFFFFF;
};
template<typename T> constexpr auto n_bits_v = n_bits<T>::value;
template<typename T> constexpr T max_bits_v = max_bits<T>::value;
}

template<typename T> class ibuf {
    using It = uint8_t;
    It pos{0};
    int8_t bpos{n_bits_v<T>-1};
    It max;
    T buf[max_bits_v<It>];
    FILE* f;
    inline void fill() {
        max = (It)(fread(buf, 1, max_bits_v<It>*sizeof(T), f)/sizeof(T));
    }
    inline bool fail_refill() {
        if (unlikely(pos == max)) {
            if (unlikely(feof(f))) {
                return true;
            }
            fill();
        }
        return false;
    }
    public:
    ibuf(FILE* stream) : f{stream} {
        fill();
    }
    make_ls_t<T> get();
    int8_t get_bit() {
        if (unlikely(bpos == -1)) {
            if (unlikely(fail_refill())) {
                return -1;
            }
            bpos = n_bits_v<T>-1;
            pos++;
        }
        return (buf[pos]>>bpos--)&1;
    }
};

template<typename T> inline make_ls_t<T> ibuf<T>::get() {
        if (unlikely(fail_refill())) {
            return -1;
        }
        return buf[pos++];
}
template<> inline make_ls_t<uint64_t> ibuf<uint64_t>::get() {
    if (unlikely(fail_refill())) {
        return Res{-1, {}};
    }
    return Res{{}, buf[pos++]};
}

template<typename T, T eom={}> class obuf {
    using It = uint8_t;
    It pos{0};
    int8_t bpos{0};
    T buf[max_bits_v<It>];
    FILE* f;
    public:
    obuf(FILE* stream) : f{stream} {
    }
    void put_bit(bool b) {
        if (unlikely(bpos == n_bits_v<T>)) {
            if (unlikely(pos == max_bits_v<It> || buf[pos] == eom)) {
                flush();
            }
            bpos = 0;
            pos++;
        }
        buf[pos] = T((buf[pos]<<1)|b);
        bpos++;
    }
    inline void flush() {
        if (unlikely(bpos == 0 && pos == 0)) {
            return;
        }
        buf[pos] = T(bpos << (n_bits_v<T>-bpos));
        fwrite(buf, 1, pos*sizeof(T)+(bpos != 0), f);
    }
};

using ibuf8  = ibuf<uint8_t >;
using ibuf16 = ibuf<uint16_t>;
using ibuf32 = ibuf<uint32_t>;
using ibuf64 = ibuf<uint64_t>;
using obuf8  = obuf<uint8_t , 0>;
using obuf16 = obuf<uint16_t, 0>;
using obuf32 = obuf<uint32_t, 0>;

}

#undef unlikely
#undef likely

#endif // IOBIT_H
