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

Count lgtr(const png_t& img, bound b) {
    size_t right_count = 0, letf_count = 0;
    for (size_t i = b.vub; i < b.vlb+1; ++i) {
        while(b.hlb <= b.hrb) {
            letf_count += bit_val(img[i][b.hlb++]);
            right_count += bit_val(img[i][b.hrb--]);
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

Count ugtd(const png_t& img, bound b) {
    size_t upper_count = 0, lower_count = 0;
    while(b.vub <= b.vlb) {
        for (size_t i = b.hlb; i < b.hrb+1; ++i) {
            upper_count += bit_val(img[b.vub][i]);
            lower_count += bit_val(img[b.vlb][i]);
        }
        b.vub++;
        b.vlb--;
    }
    if (upper_count > lower_count) {
        return Up;
    } else if (lower_count < upper_count) {
        return Low;
    } else {
        return Equal;
    }
}

void encoder(const vector<bool>& msg, size_t& mpos,
            png_t& img, const size_t bpos, vector<bound>& bounds) {
    if (mpos >= msg.size()) {
        return;
    }
    size_t hlb = bounds[bpos].hlb, hrb = bounds[bpos].hrb,
           vub = bounds[bpos].vub, vlb = bounds[bpos].vlb;
    auto vcount = lgtr(img, bounds[bpos]);
    if (vcount != Equal && vcount != msg[mpos++]) {
        for (size_t i = vub; i < vlb+1; ++i) {
            while(hlb <= hrb) {
                swap_bits(img[i][hlb++], img[i][hrb--]);
            }
        }
    } else {
        hlb = (hlb+hrb)/2+1;
        hrb = hrb - (hrb-bounds[bpos].hlb)/2-1;
    }
    if (mpos >= msg.size()) {
        return;
    }
    vcount = ugtd(img, bounds[bpos]);
    if (vcount != Equal && vcount != msg[mpos++]) {
        while(vub <= vlb) {
            for (size_t i = bounds[bpos].hlb; i < bounds[bpos].hrb+1; ++i) {
                swap_bits(img[vub][i], img[vlb][i]);
            }
            vub++;
            vlb--;
        }
    } else {
        vub = (vub+vlb)/2+1;
        vlb = vlb - (vlb-bounds[bpos].vub)/2-1;
    }
    if (bounds[bpos].hlb+2 >= bounds[bpos].hrb ||
        bounds[bpos].vub+2 >= bounds[bpos].vlb) {
        return;
    }
    bounds.emplace_back(bound{bounds[bpos].hlb,hlb-1,bounds[bpos].vub,vub-1});
    bounds.emplace_back(bound{hrb+1,bounds[bpos].hrb,bounds[bpos].vub,vub-1});
    bounds.emplace_back(bound{bounds[bpos].hlb,hlb-1,vlb+1,bounds[bpos].vlb});
    bounds.emplace_back(bound{hrb+1,bounds[bpos].hrb,vlb+1,bounds[bpos].vlb});
}

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

size_t encode(png_t& img, const vector<bool>& msg) {
    if (msg.empty()) {
        return 0;
    }
    size_t mpos = 0, bpos = 0;
    vector<bound> bounds;
    bounds.reserve(msg.size());
    bounds.emplace_back(bound{0, img.get_width()-1, 0, img.get_height()-1});
    do {
        encoder(msg, mpos, img, bpos++, bounds);
    } while (mpos != msg.size() || bpos != bounds.size());
    return mpos;
}

bool encoder_producer(const size_t bpos, vector<bound>& bounds) {
    size_t hlb = bounds[bpos].hlb, hrb = bounds[bpos].hrb,
           vub = bounds[bpos].vub, vlb = bounds[bpos].vlb;

        hlb = (hlb+hrb)/2+1;
        hrb = hrb - (hrb-bounds[bpos].hlb)/2-1;

        vub = (vub+vlb)/2+1;
        vlb = vlb - (vlb-bounds[bpos].vub)/2-1;
    if (bounds[bpos].hlb+2 >= bounds[bpos].hrb ||
        bounds[bpos].vub+2 >= bounds[bpos].vlb) {
        return false;
    }
    bounds.emplace_back(bound{bounds[bpos].hlb,hlb-1,bounds[bpos].vub,vub-1});
    bounds.emplace_back(bound{hrb+1,bounds[bpos].hrb,bounds[bpos].vub,vub-1});
    bounds.emplace_back(bound{bounds[bpos].hlb,hlb-1,vlb+1,bounds[bpos].vlb});
    bounds.emplace_back(bound{hrb+1,bounds[bpos].hrb,vlb+1,bounds[bpos].vlb});
    return true;
}

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
    while (bounds.size() < expected_size) {
        if (encoder_producer(bpos++, bounds)) {
            break;
        }
    }
    if (bounds.size() > expected_size) {
        bounds.resize(expected_size);
    }
    return bounds;
}

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

// the idea:
// if represented as non inplace function (un)mark :: Png -> Msg -> Png, then
// f b a = unmark (mark a b) b => f b = id foreach b


int main(int , char *[]) {
    auto msg = vector<bool>{
        true, false,
        true, false,
        true, true,
        false, true
    };
    auto img = png_t(string(getenv("HOME")) + "/Images/45.png.enc.png");
    //auto size = encode(img, msg);
    //cerr << "[I]: bits encoded " << size << '\n';
    mark(img, msg);
    img.write("marked.png");
    img = png_t("marked.png");
    unmark(img, msg);
    img.write("unmarked.png");
    return 0;
}
