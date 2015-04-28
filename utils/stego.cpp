#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <random>
#include <unistd.h>
#include <limits.h>
#include <png++/png.hpp>
#include <extra/utils.h>

// gpp stego.cpp -lpng

using namespace std;
using namespace png;

using png_t = image<rgb_pixel>;

template<typename T>
using mat = vector<vector<T>>;

constexpr int dcenter_c{4};
static_assert(dcenter_c >= 0, "must be a positive integer");

constexpr int slope_c{2*dcenter_c+1};
static_assert(slope_c >= 2*dcenter_c + 1 && slope_c <= 255, "invalid slope");

template<typename T, typename U>
constexpr T round(const T& size, const U& mul) {
    return size - size%mul;
}

template<typename T>
auto make_mat(const png_t& img, const T& ele) {
    if (img.get_height() < 10 || img.get_width() < 10) {
        throw runtime_error("unsufficient size");
    }
    return mat<T>(img.get_height(), vector<T>(img.get_width(), ele));
}

enum Candidate { None = 0, Min = 1, Ok = 1, Max = 2, Mid = 3 };

struct changes_t {
    Candidate red, blue, green;
};

vector<bool> to_bitstream(const vector<char>& in) {
    auto res = vector<bool>();
    res.reserve(in.size() * CHAR_BIT);
    for(const auto& c: in) {
        for (uint8_t i = 0; i < CHAR_BIT; ++i) {
            res.push_back((c>>i)&1);
        }
    }
    return res;
}

vector<char> to_stream(const vector<bool>& in) {
    auto rs = round(in.size(), CHAR_BIT);
    auto res = vector<char>();
    res.reserve(rs);
    for (auto it = begin(in); it < begin(in) + rs; it += CHAR_BIT) {
        char c = 0;
        for (uint8_t i = 0; i < CHAR_BIT; ++i) {
            c |= (char(*(it+i))<<i);
        }
        res.push_back(c);
    }
    return res;
}

template<typename T>
inline bool inside(size_t i, size_t j, const mat<T>& mat) {
    return (i < mat.size()) && (j < mat[0].size());
}

template<typename T, typename F>
void for_insides(mat<T>& m, F&& f) {
    auto rs = round(m.size(), 2);
    auto cs = round(m[0].size(), 2);
    for (size_t i = 1; i < rs-1; i+=2) {
        for (size_t j = 1; j < cs-1; j+=2) {
            f(m, i, j);
        }
    }
    for (size_t i = 2; i < rs-2; i+=2) {
        for (size_t j = 2; j < cs-2; j+=2) {
            f(m, i, j);
        }
    }
}

#define COLORS \
    FOR(red)   \
    FOR(blue)  \
    FOR(green)

#define CROSS(op, img, color, i, j)              \
    op(op(img[i][j-1].color, img[i][j+1].color), \
       op(img[i-1][j].color, img[i+1][j].color))

#define IF_CANDIDATE(color)                             \
    auto max##color  = CROSS(max, img, color, i, j);    \
    auto min##color  = CROSS(min, img, color, i, j);    \
    if((max##color - min##color >= slope_c) &&          \
       (img[i][j].color >= (min##color + dcenter_c)) && \
       (img[i][j].color <= (max##color - dcenter_c)))

#define IF_(op, color, off) \
    if(img[i][j].color == CROSS(op, img, color, i, j) + (off))

#define IF_MIN(color) IF_(min, color, +dcenter_c)
#define IF_MAX(color) IF_(max, color, -dcenter_c)

#define SET_TYPE(color) do {              \
    IF_MIN(color) {                       \
        mat[i][j].color = Candidate::Min; \
    } else { IF_MAX(color) {              \
        mat[i][j].color = Candidate::Max; \
    } else {                              \
        mat[i][j].color = Candidate::Mid; \
    }}                                    \
} while(0)

auto enc_spots(const png_t& img) {
    auto mat = make_mat(img, changes_t{None, None, None});
    for_insides(mat, [&](auto& mat, size_t i, size_t j) {
            #define FOR(color) IF_CANDIDATE(color) { SET_TYPE(color); }
                COLORS
            #undef FOR
    });
    return mat;
}

auto dec_spots(const png_t& img) {
    auto mat = make_mat(img, changes_t{None, None, None});
    for_insides(mat, [&](auto& mat, size_t i, size_t j) {
            #define FOR(color) \
                IF_CANDIDATE(color) { mat[i][j].color = Candidate::Ok; }
                COLORS
            #undef FOR
    });
    return mat;
}

#define SET(ele, color, img) do {             \
    if (ele.color == Candidate::Min) {        \
        img[i][j].color += 1;                 \
    } else if (ele.color == Candidate::Max) { \
        img[i][j].color -= 1;                 \
    } else {                                  \
        if (d(gen)) {                         \
            img[i][j].color += 1;             \
        } else {                              \
            img[i][j].color -= 1;             \
        }                                     \
    }                                         \
} while(0)

#define ENC_MSG(color) do {                                                  \
    if (fst_ele.color || snd_ele.color) {                                    \
        if (bool((img_f[i][j].color ^ img_s[i][j].color)&1) != msg[count]) { \
            if (snd_ele.color == Candidate::None) {                          \
                SET(fst_ele, color, img_f);                                  \
            } else if (fst_ele.color == Candidate::None) {                   \
                SET(snd_ele, color, img_s);                                  \
            } else {                                                         \
               if(d(gen)) {                                                  \
                    SET(fst_ele, color, img_f);                              \
                } else {                                                     \
                    SET(snd_ele, color, img_s);                              \
                }                                                            \
            }                                                                \
        }                                                                    \
        if (++count == msg.size()) {                                         \
            return count;                                                    \
        }                                                                    \
    }                                                                        \
} while(0)

#define DEC_MSG(color) do {                                             \
    if(mat_f[i][j].color || mat_s[i][j].color) {                        \
        msg.push_back(bool((img_f[i][j].color ^ img_s[i][j].color)&1)); \
    }                                                                   \
} while(0)

auto enc(const vector<bool>& msg, png_t& img_f, png_t& img_s) {
    size_t count = 0;
    if (msg.empty()) {
        return count;
    }
    random_device rd;
    default_random_engine gen(rd());
    discrete_distribution<> d({50, 50});
    auto mat_f = enc_spots(img_f);
    auto mat_s = enc_spots(img_s);
    size_t min_r = min(mat_f.size(), mat_s.size()),
           min_c = min(mat_f[0].size(), mat_s[0].size());
    for (size_t i = 0; i < min_r; ++i) {
        for (size_t j = 0; j < min_c; ++j) {
            auto& fst_ele = mat_f[i][j];
            auto& snd_ele = mat_s[i][j];
            #define FOR(color) ENC_MSG(color);
                COLORS
            #undef FOR
        }
    }
    return count;
}

auto dec(const png_t& img_f, const png_t& img_s) {
    auto msg = vector<bool>();
    auto mat_f = dec_spots(img_f);
    auto mat_s = dec_spots(img_s);
    size_t min_r = min(mat_f.size(), mat_s.size()),
           min_c = min(mat_f[0].size(), mat_s[0].size());
    for (size_t i = 0; i < min_r; ++i) {
        for (size_t j = 0; j < min_c; ++j) {
            #define FOR(color) DEC_MSG(color);
                COLORS
            #undef FOR
        }
    }
    return to_stream(msg);
}

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
         cerr << "Usage:\t" << argv[0] << " -f fst -s snd {-e|-d}"
         "\n\t-f fst<string>: file name of the fist png image"
         "\n\t-s snd<string>: file name of the second png image"
         "\n\t-e: perform encoding"
         "\n\t-d: perform decoding"
         "\nScope:\tsteganography on two images' LSB edges, (d)ecoding only,"
         "\n\tit doesn't (d)encript/(de)compress/(de)serialize the message"
         << endl;
    };
    char* ifname_f{nullptr},* ifname_s{nullptr};
    enum { None = 0, Enc, Dec} op = None;
    int opt;
    while ((opt = getopt(argc, argv, "edhvf:s:")) != -1) {
        switch (opt) {
        case 'f':           // first image filename
            ifname_f = optarg;
            break;
        case 's':           // second image filename
            ifname_s = optarg;
            break;
        case 'e':           // encode operation
            op = Enc;
            break;
        case 'd':           // decode operation
            op = Dec;
            break;
        case 'h':           // print help and exit
            print_help();
            return 0;
        default:            // print help and die
            print_help();
            return 1;
        }
    }
    if (ifname_f == nullptr || ifname_s == nullptr || op == None) {
        print_help();
        return 1;
    }
    auto img_f = png_t(ifname_f);
    auto img_s = png_t(ifname_s);
    if (op == Enc) {
        vector<char> in(istreambuf_iterator<char>{cin}, {});
        auto msg = to_bitstream(in);
        auto esize = enc(msg, img_f, img_s);
        img_f.write(string(ifname_f) + ".enc.png"s);
        img_s.write(string(ifname_s) + ".enc.png"s);
        cerr << "[I]: bits encoded " << esize << endl;
    } else if (op == Dec) {
        auto msg = dec(img_f, img_s);
        copy(begin(msg), end(msg), ostreambuf_iterator<char>{cout});
    } else {
        print_help();
        return 1;
    }
    return 0;
}
