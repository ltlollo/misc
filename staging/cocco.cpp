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
constexpr float rd_div{ 2.0 };
constexpr float rd_divsq{ rd_div*rd_div };
constexpr double pop_rat{ 0.5 };

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
    if (str.empty())    {
        throw std::runtime_error("color can't be empty");
    }
    if (str.length()%2) {
        throw std::runtime_error("color must be even in size");
    }
    if (str.length()>6) {
        throw std::runtime_error("color max 6 chars");
    }
    for (const auto& c: str) {
        if (!(hex(c))) {
            throw std::runtime_error("color is not a hex number");
        }
    }
    res.red = uint8_t(to_hex(str[0])*16+to_hex(str[1]));
    if (str.size() == 2) { return res; }
    res.green = uint8_t(to_hex(str[2])*16+to_hex(str[3]));
    if (str.size() == 4) { return res; }
    res.blue = uint8_t(to_hex(str[4])*16+to_hex(str[5]));
    return res;
}

struct CVers { float r, g, b; };
struct Vdiff { int16_t r, g, b; };
struct CVBound {
    float min_r_vers
        , max_r_vers
        , min_g_vers
        , max_g_vers
        , min_b_vers
        , max_b_vers
        ;
};

size_t area(const png_t& img) noexcept {
    return img.get_width()*img.get_height();
}
float norm2sq(const px_t& col) noexcept {
    return float(col.red*col.red+col.green*col.green+col.blue*col.blue);
}
float norm2sq(const Vdiff& vd) noexcept {
    return float(vd.r*vd.r+vd.g*vd.g+vd.b*vd.b);
}

CVBound ptrun_bounds(const px_t& col) {
    auto norm = sqrt(norm2sq(col));
    if (norm == 0) { throw std::runtime_error("cannot use black"); }
    CVers cv = { col.red/norm, col.green/norm, col.blue/norm };
    // z = r, g = x, b = y
    float phi = acos(cv.r), th = atan2(cv.b, cv.g);
    if (phi > pi/2-rdiv/2) { phi = pi/2-rdiv/2; }
    else if (phi < rdiv/2) { phi = rdiv/2;      }
    if (th > pi/2-rdiv/2)  { th  = pi/2-rdiv/2; }
    else if (th < rdiv/2)  { th  = rdiv/2;      }
    return CVBound {
          cos(phi+rdiv/2)
        , cos(phi-rdiv/2)
        , sin(phi)*cos(th+rdiv/2)
        , sin(phi)*cos(th-rdiv/2)
        , sin(phi)*sin(th-rdiv/2)
        , sin(phi)*sin(th+rdiv/2)
    };
}

CVBound bounds(const px_t& col) {
    CVBound res;
    auto norm = sqrt(norm2sq(col));
    if (norm == 0) { throw std::runtime_error("color cannot be black"); }
    CVers cv = { col.red/norm, col.green/norm, col.blue/norm };
    // z = r, g = x, b = y
    float phi = acos(cv.r), th = atan2(cv.b, cv.g);
    if (phi > pi/2-rdiv/2) {
        res.min_r_vers =             0.0;
        res.max_r_vers = cos(phi-rdiv/2);
    } else if (phi < rdiv/2) {
        res.min_r_vers = cos(phi+rdiv/2);
        res.max_r_vers =             1.0;
    } else {
        res.min_r_vers = cos(phi+rdiv/2);
        res.max_r_vers = cos(phi-rdiv/2);
    }
    if (th > pi/2-rdiv/2)  {
        res.min_g_vers =                     0.0;
        res.max_g_vers = sin(phi)*cos(th-rdiv/2);
        res.min_b_vers = sin(phi)*sin(th-rdiv/2);
        res.max_b_vers                     = 1.0;
    } else if (th < rdiv/2)  {
        res.min_g_vers = sin(phi)*cos(th+rdiv/2);
        res.max_g_vers =                     1.0;
        res.min_b_vers                     = 0.0;
        res.max_b_vers = sin(phi)*sin(th+rdiv/2);
    } else {
        res.min_g_vers = sin(phi)*cos(th+rdiv/2);
        res.max_g_vers = sin(phi)*cos(th-rdiv/2);
        res.min_b_vers = sin(phi)*sin(th-rdiv/2);
        res.max_b_vers = sin(phi)*sin(th+rdiv/2);
    }
    return res;
}

Vdiff operator-(const px_t& lhs, const px_t& rhs) noexcept {
    Vdiff res = { lhs.red, lhs.green, lhs.blue };
    res.r = int16_t(res.r - rhs.red);
    res.g = int16_t(res.g - rhs.green);
    res.b = int16_t(res.b - rhs.blue);
    return res;
}

bool scasim(const png_t& img, const px_t cv) {
    auto cvn2sq = norm2sq(cv);
    auto esq = cvn2sq/rd_divsq;
    size_t count = 0;
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            if (norm2sq(cv-img[i][j]) <= esq) {
                ++count;
            }
        }
    }
    return count >= double(area(img))*pop_rat;
}

bool inbsim(const png_t img, const px_t cv) {
    auto b = ptrun ? ptrun_bounds(cv) : bounds(cv);
    size_t count = 0;
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            auto& col = img[i][j];
            auto norm = sqrt(norm2sq(col));
            CVers res = { col.red/norm, col.green/norm, col.blue/norm };
            if (res.r < b.min_r_vers || res.r > b.max_r_vers ||
                res.b < b.min_b_vers || res.b > b.max_b_vers ||
                res.g < b.min_g_vers || res.g > b.max_g_vers) { continue; }
            ++count;
        }
    }
    return count >= double(area(img))*pop_rat;
}

int main(int argc, char *argv[]) {
    auto print_help =[&]() {
        cerr << "Usage:\t" << argv[0] << " img col"
            "\n\timg<png>: input png filename"
            "\n\tcol<hh[hh][hh]>: hex color (where h in 0..F), col not 0"
            "\nScope:\tsome"
            <<  endl;
    };
    if (argc-1 < 2) {
        print_help();
        return 1;
    }
    try {
        auto img = png_t(argv[1]);
        auto cv = parse_num(argv[2]);
        cout << (inbsim(img, cv) ? 'y' : 'n') << endl;
    } catch (std::runtime_error& e) {
        cerr << "[E]: " << e.what() << endl;
        return 1;
    } catch(...) {
        return 1;
    }
    return 0;
}

