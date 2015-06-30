#include <vector>
#include <stdexcept>
#include <iostream>
#include <random>
#include <string.h>
#include <png++/png.hpp>

//gpp cautoma.cpp -lpng

using px_t = png::rgb_pixel;
using img_t = png::image<px_t>;
using rules_t = uint32_t;

constexpr bool wrap = true, red = true, green = true, blue = true;

std::random_device rd;
std::default_random_engine gen(rd());
std::uniform_int_distribution<> col_d(0, 7);
std::uniform_int_distribution<> srule_d(0, 0xffffff);

inline px_t next(px_t* curr, const rules_t rules) {
    px_t ret;
    uint8_t r = 0, g = 0, b = 0;
    if (red) {
        r = (((curr-1)->red&1)<<2)+((curr->red&1)<<1)+((curr+1)->red&1);
        if ((((rules>>0)&0xff)>>r)&1) { ret.red = 255; }
    }
    if (green) {
        g = (((curr-1)->green&1)<<2)+((curr->green&1)<<1)+((curr+1)->green&1);
        if ((((rules>>8)&0xff)>>g)&1) { ret.green = 255; }
    }
    if (blue) {
        b = (((curr-1)->blue&1)<<2)+((curr->blue&1)<<1)+((curr+1)->blue&1);
        if ((((rules>>16)&0xff)>>b)&1) { ret.blue = 255; }
    }
    return ret;
}

inline void wrap_line(img_t& img, size_t r) {
    img[r][0] = img[r][img.get_width()-2];
    img[r][img.get_width()-1] = img[r][1];
}

void populate(img_t& img, const uint32_t rule) {
    for (size_t i = 1; i < img.get_height(); ++i) {
        for (size_t j = 1;  j < img.get_width()-1; ++j) {
            img[i][j] = next(&img[i-1][j], rule);
        }
        if (wrap) {
            wrap_line(img, i);
        }
    }
}

template<typename T>
auto new_img(size_t x, size_t y, T&& lambda) {
    if (x < 20 || x < 20) {
        throw std::runtime_error("Image too small");
    }
    auto ret = img_t(x+2,y+2);
    for (size_t i = 1; i < ret.get_width()-1; ++i) {
        ret[0][i] = lambda(i-1);
    }
    if (wrap) {
        wrap_line(ret, 0);
    }
    return ret;
}

px_t random_col() {
    png::rgb_pixel ret;
    auto rnd = col_d(gen);
    if ((rnd>>0)&1) { ret.red   = 255; }
    if ((rnd>>1)&1) { ret.green = 255; }
    if ((rnd>>2)&1) { ret.blue  = 255; }
    return ret;
}

uint32_t random_rules() {
    return srule_d(gen);
}

auto random_start(size_t x, size_t y) {
    return new_img(x, y, [](size_t) { return random_col(); });
}

// rules is uint32_t(0b________xxxxxxxxxxxxxxxxxxxxxxxx)
//                     \ pad  /\ blue /\green /\ red  /
//  where bitpos repr a state of the top neighbors, and the value the
//  transformed state eg red: 11111110 (0: cell dead, cell alive)
//                    bitpos: 76543210 - 000 -> 0, 001 -> 1, 010 -> 1, ...
//  or 000, 001, 010, ...
//      0    1    1   ...

int main(int , char *[]) {
    auto img = random_start(1024, 1024);
    auto rules = random_rules();
    std::cerr << "rules: " << rules << '\n';
    populate(img, rules);
    img.write("/tmp/out.png");
    return 0;
}

