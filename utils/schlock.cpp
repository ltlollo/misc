// gpp schlock.cpp -lpng

#include <vector>
#include <string>
#include <png++/png.hpp>
#include <stdlib.h>

using namespace std;
using px_t = png::rgb_pixel;
using png_t = png::image<px_t>;

inline bool bit_val(const px_t& px) {
    return (px.red ^ px.blue ^ px.green)&1;
}

inline void swap_bits(px_t& f, px_t& s) {
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

Count lgtr(const png_t& img, size_t hlb, size_t hrb, size_t vub, size_t vlb) {
    size_t right_count = 0, letf_count = 0;
    for (size_t i = vub; i < vlb+1; ++i) {
        while(hlb <= hrb) {
            letf_count += bit_val(img[i][hlb++]);
            right_count += bit_val(img[i][hrb--]);
        }
    }
    if (letf_count > right_count) {
        return Left;
    } else if (letf_count < right_count) {
        return Right;
    } else {
        return Equal;
    }
}

Count ugtd(const png_t& img, size_t hlb, size_t hrb, size_t vub, size_t vlb) {
    size_t upper_count = 0, lower_count = 0;

    while(vub <= vlb) {
        for (size_t i = hlb; i < hrb+1; ++i) {
            upper_count += bit_val(img[vub][i]);
            lower_count += bit_val(img[vlb][i]);
        }
        vub++;
        vlb--;
    }
    if (upper_count > lower_count) {
        return Up;
    } else if (lower_count < upper_count) {
        return Low;
    } else {
        return Equal;
    }
}

void encode(const vector<bool>& msg, size_t& pos,
            png_t& img, size_t hlb, size_t hrb, size_t vub, size_t vlb) {
    if (pos >= msg.size()) {
        return;
    }
    size_t hlb_orig = hlb, hrb_orig = hrb, vub_orig = vub, vlb_orig = vlb;
    auto vcount = lgtr(img, hlb, hrb, vub, vlb);
    if (vcount != Equal && vcount != msg[pos++]) {
            for (size_t i = vub; i < vlb+1; ++i) {
                while(hlb <= hrb) {
                    swap_bits(img[i][hlb++], img[i][hrb--]);
                }
            }
    } else {
        hlb = (hlb+hrb)/2+1;
        hrb = hrb - (hrb-hlb_orig)/2-1;
    }
    if (pos >= msg.size()) {
        return;
    }
    vcount = ugtd(img, hlb_orig, hrb_orig, vub, vlb);
    if (vcount != Equal && vcount != msg[pos++]) {
            while(vub <= vlb) {
                for (size_t i = hlb_orig; i < hrb_orig+1; ++i) {
                    swap_bits(img[vub][i], img[vlb][i]);
                }
                vub++;
                vlb--;
            }
    } else {
        vub = (vub+vlb)/2+1;
        vlb = vlb - (vlb-vub_orig)/2-1;
    }
    if (hlb_orig+2 >= hrb_orig || vub_orig+2 >= vlb_orig) {
        return;
    }
    encode(msg, pos, img, hlb_orig, hlb-1, vub_orig, vub-1);
    encode(msg, pos, img, hrb+1, hrb_orig, vub_orig, vub-1);
    encode(msg, pos, img, hlb_orig, hlb-1, vlb+1, vlb_orig);
    encode(msg, pos, img, hrb+1, hrb_orig, vlb+1, vlb_orig);
}


int main(int , char *[]) {
    auto msg = vector<bool>(20, true);
    auto img = png_t(string(getenv("HOME")) + "/Images/45.png");
    size_t size = 0;
    encode(msg, size, img, 0, img.get_width()-1, 0, img.get_height()-1);
    return 0;
}