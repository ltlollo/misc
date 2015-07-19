// gpp schlock.cpp -lpng

#include <vector>
#include <string>
#include <iostream>
#include <png++/png.hpp>
#include <stdlib.h>

using namespace std;
using px_t = png::rgb_pixel;
using png_t = png::image<px_t>;

inline bool bit_val(const px_t& px) {
    return (px.red ^ px.blue ^ px.green)&1;
}

struct bound { size_t hlb, hrb, vub, vlb; };

inline void swap_bits(px_t& f, px_t& s) noexcept {
    bool tmp = f.red&1;
    f.red = (s.red&1)|(f.red&0xFE);
    s.red = tmp | (s.red&0xFE);
    tmp = f.blue&1;
    f.blue = (s.blue&1)|(f.blue&0xFE);
    s.blue = tmp | (s.blue&0xFE);
    tmp = f.green&1;
    f.green = (s.green&1)|(f.green&0xFE);
    s.green = tmp | (s.green&0xFE);
}

enum Count { Left = 1, Up = 1, Equal = 2, Right = 0, Low = 0 };

inline void flip_horiz(png_t& img, bound b) noexcept {
    for (size_t i = b.vub; i < b.vlb+1; ++i) {
        while(b.hlb <= b.hrb) {
            swap_bits(img[i][b.hlb++], img[i][b.hrb--]);
        }
    }
}

inline void flip_vert(png_t& img, bound b) noexcept {
   while(b.vub <= b.vlb) {
       for (size_t i = b.hlb; i < b.hrb+1; ++i) {
           swap_bits(img[b.vub][i], img[b.vlb][i]);
       }
       b.vub++;
       b.vlb--;
   }
}

// the bounds are choosen this way in order to minimize phase shift in {F}
// probably makes very little difference
void bound_producer(const size_t bpos, vector<bound>& bounds) {
    size_t hlb = bounds[bpos].hlb, hrb = bounds[bpos].hrb,
           vub = bounds[bpos].vub, vlb = bounds[bpos].vlb;
    hlb = (hlb+hrb)/2+1;
    hrb = hrb - (hrb-bounds[bpos].hlb)/2-1;
    vub = (vub+vlb)/2+1;
    vlb = vlb - (vlb-bounds[bpos].vub)/2-1;

    if (bounds[bpos].hlb+2 >= bounds[bpos].hrb ||
        bounds[bpos].vub+2 >= bounds[bpos].vlb) {
        return;
    }
    bounds.emplace_back(bound{bounds[bpos].hlb,hlb-1,bounds[bpos].vub,vub-1});
    bounds.emplace_back(bound{hrb+1,bounds[bpos].hrb,bounds[bpos].vub,vub-1});
    bounds.emplace_back(bound{bounds[bpos].hlb,hlb-1,vlb+1,bounds[bpos].vlb});
    bounds.emplace_back(bound{hrb+1,bounds[bpos].hrb,vlb+1,bounds[bpos].vlb});
}

// return the bound vector associated to (image, message),
// it's sized as message size/2u, but can be samller depending
// on image geometry
auto make_bounds(const png_t& img, const vector<bool>& msg) {
    vector<bound> bounds;
    // last bit of msg ignored for simplicity if msg size is odd
    size_t bpos = 0, expected_size = msg.size()/2;
    if (!expected_size) {
        return bounds;
    }
    bounds.reserve(expected_size+5);
    bounds.reserve(msg.size());
    bounds.emplace_back(bound{0, img.get_width()-1, 0, img.get_height()-1});
    while (bounds.size() < expected_size && bpos < bounds.size()) {
        bound_producer(bpos++, bounds);
    }
    if (bounds.size() > expected_size) {
        bounds.resize(expected_size);
    }
    return bounds;
}

// return the number of bits of message used for the marking
size_t mark(png_t& img, const vector<bool>& msg) {
    auto bounds = make_bounds(img, msg);
    size_t mpos = 0, bpos = 0;
    while(bpos != bounds.size()) {
        if (msg[mpos++]) {
            flip_horiz(img, bounds[bpos]);
        }
        if (msg[mpos++]) {
            flip_vert(img, bounds[bpos]);
        }
        ++bpos;
    }
    return bounds.size()*2;
}

// return the number of bits of message used for the unmarking
size_t unmark(png_t& img, const vector<bool>& msg) {
    auto bounds = make_bounds(img, msg);
    size_t mpos = bounds.size()*2-1;
    for (auto it = bounds.crbegin(); it != bounds.crend(); ++it) {
        if (msg[mpos--]) {
            flip_vert(img, *it);
        }
        if (msg[mpos--]) {
            flip_horiz(img, *it);
        }
    }
    return bounds.size()*2;
}

template<typename T> vector<bool> to_bitvec(T&& in) {
    auto res = vector<bool>();
    res.reserve(in.size() * CHAR_BIT);
    for(const auto& c: in) {
        for (uint8_t i = 0; i < CHAR_BIT; ++i) {
            res.push_back((c>>i)&1);
        }
    }
    return res;
}

// the idea:
// if represented as non inplace function (un)mark :: Png -> Msg -> Png, then
// f b a = unmark (mark a b) b => f b = id foreach b

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
         cerr << "Usage:\t" << argv[0]
              << " -f img {-m|-u} [-p pattern]"
         "\n\t-i img<string>: file name of the png image"
         "\n\t-m: perform marking"
         "\n\t-u: perform unmarking"
         "\n\t-p pattern<string>: provide the marking pattern"
         " (defaults to stdin)"
         "\nScope:\t(un)mark an image according to a user provided pattern"
         << endl;
    };
    char* ifname{nullptr}, *pattern{nullptr};
    enum { None = 0, Mark, Unmark} op = None;
    int opt;
    while ((opt = getopt(argc, argv, "muhi:p:")) != -1) {
        switch (opt) {
        case 'i':           // image filename
            ifname = optarg;
            break;
        case 'p':           // pattern
            pattern = optarg;
            break;
        case 'm':           // mark operation
            op = Mark;
            break;
        case 'u':           // unmark operation
            op = Unmark;
            break;
        case 'h':           // print help and exit
            print_help();
            return 0;
        default:            // print help and die
            print_help();
            return 1;
        }
    }
    if (ifname == nullptr || op == None) {
        print_help();
        return 1;
    }
    auto img = png_t(ifname);
    auto msg = pattern ? to_bitvec(string(pattern)) :
        to_bitvec(vector<char>(istreambuf_iterator<char>{cin}, {}));
    auto esize = (op == Mark) ? mark(img, msg) : unmark(img, msg);
    if (op == Mark) {
        img.write(string(ifname) + ".mark.png"s);
    } else {
        img.write(string(ifname) + ".unmark.png"s);
    }
    cerr << "[I]: msg bits used " << esize << endl;
    return 0;
}
