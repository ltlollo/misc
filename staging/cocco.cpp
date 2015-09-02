// gpp self -lpng

#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <png++/png.hpp>

using namespace png;
using namespace std;

using px_t = png::rgb_pixel;
using png_t = png::image<px_t>;

constexpr bool ptrun{true};
constexpr float pi { acos(-1.f) };
constexpr float rdiv{ pi/4 };

static_assert(rdiv < pi/2, "must be less than pi/2");

constexpr bool hex(char c) noexcept {
    return (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F');
}
constexpr uint8_t to_hex(char c) noexcept {
    if (c >= '0' && c <= '9') { return uint8_t(c%48);    }
    if (c >= 'a' && c <= 'f') { return uint8_t(c%97+10); }
  /*if (c >= 'A' && c <= 'F')*/ return uint8_t(c%65+10);
}

auto parse_num(const string& str) {
    px_t res = {0,0,0};
    if (str.empty()) { throw std::runtime_error("can't be empty"); }
    if (str.length()%2) { throw std::runtime_error("must be even in size"); }
    if (str.length()>6) { throw std::runtime_error("max 6 chars"); }
    for (const auto& c: str) {
        if (!(hex(c))) {
            throw std::runtime_error("not a number");
        }
    }
    res.red = uint8_t(to_hex(str[0])*16+to_hex(str[1]));
    if (str.size() == 2) { return res; }
    res.green = uint8_t(to_hex(str[2])*16+to_hex(str[3]));
    if (str.size() == 4) { return res; }
    res.blue = uint8_t(to_hex(str[4])*16+to_hex(str[5]));
    return res;
}

struct CVers { float red, green, blue; };

struct CVBound {
    float min_red_vers
        , max_red_vers
        , min_green_vers
        , max_green_vers
        , min_blue_vers
        , max_blue_vers
        ;
};

size_t area(const png_t& img) noexcept {
    return img.get_width()*img.get_height();
}
float norm2sq(const px_t& col) noexcept {
    return float(col.red*col.red+col.green*col.green+col.blue*col.blue);
}

CVBound ptrun_bounds(const px_t& col) {
    auto norm = sqrt(norm2sq(col));
    if (norm == 0) { throw std::runtime_error("cannot use black"); }
    CVers res = { col.red/norm, col.green/norm, col.blue/norm };
    // z = r, g = x, b = y
    float phi = acos(res.red), th = atan2(res.blue, res.green);
    if (phi > pi/2-rdiv/2) { phi = pi/2-rdiv/2; }
    else if (phi < rdiv/2) { phi = rdiv/2;      }
    if (th > pi/2-rdiv/2)  { th = pi/2-rdiv/2;  }
    else if (th < rdiv/2)  { th = rdiv/2;       }
    res = { cos(phi), sin(phi)*cos(th), sin(phi)*sin(th) };
    return CVBound {
          cos(phi+rdiv/2)
        , cos(phi-rdiv/2)
        , sin(phi)*cos(th+rdiv/2)
        , sin(phi)*cos(th-rdiv/2)
        , sin(phi)*sin(th-rdiv/2)
        , sin(phi)*sin(th+rdiv/2)
    };
}

int main(int argc, char *argv[]) {
    auto print_help =[&] () {
        cerr << "Usage:\t" << argv[0] << " img col"
            "\n\timg<png>: input image"
            "\n\tcol<hh[hh][hh]>: hex color (where h in 0..F)"
            "\nScope:\tdoes something"
            <<  endl;
    };
    if (argc-1 < 2) {
        print_help();
        return 1;
    }
    auto img = png_t(argv[1]);
    auto cv = parse_num(argv[2]);
    auto bounds = ptrun_bounds(cv);
    size_t count = 0;
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            auto& col = img[i][j];
            auto norm = sqrt(norm2sq(col));
            CVers res = { col.red/norm, col.green/norm, col.blue/norm };
            if (res.red   < bounds.min_red_vers   ||
                res.red   > bounds.max_red_vers   ||
                res.blue  < bounds.min_blue_vers  ||
                res.blue  > bounds.max_blue_vers  ||
                res.green < bounds.min_green_vers ||
                res.green > bounds.max_green_vers) { continue; }
            ++count;
        }
    }
    cout << (count >= area(img)/2 ? 'y' : 'n') << endl;
    return 0;
}

