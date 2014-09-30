#include <iostream>
#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <png++/png.hpp>
#include <png++/image.hpp>
#include <png++/rgb_pixel.hpp>

using namespace std;

constexpr unsigned w_size{2}, n_features{2000};
constexpr double ets{5000};

struct feature {
    struct pt { size_t x, y;} p;
    double e;
};

struct g {
    long gxx, gyy, gxy;
};

using mgrad = std::vector<std::vector<g>>;

mgrad gradient(const png::image<png::gray_pixel>& img) {
    size_t h{img.get_height()}, w{img.get_width()};
    mgrad grad(h, std::vector<g>(w, g()));
    if (h < 3 || w < 3) {
        return grad;
    }
    for (size_t i{1}; i < h-1; ++i) {
        for (size_t j{1}; j < w-1; ++j) {
            grad[i][j].gxx = {-img[i][j-1] + img[i][j+1]}; // gx
        }
    }
    for (size_t i{1}; i < h-1; ++i) {
        for (size_t j{1}; j < w-1; ++j) {
            grad[i][j].gyy = {-img[i-1][j] + img[i+1][j]}; // gy
            grad[i][j].gxy = {grad[i][j].gyy*grad[i][j].gxx};
            grad[i][j].gyy *= grad[i][j].gyy;
            grad[i][j].gxx *= grad[i][j].gxx;
        }
    }
    return grad;
}

inline double patch_interest(const mgrad& grad, unsigned y, unsigned x) {
    g sum{0, 0, 0};
    for (unsigned k{0}; k < w_size; ++k) {
        for (unsigned z{0}; z < w_size; ++z) {
            sum.gxx += grad[y+k][x+z].gxx;
            sum.gyy += grad[y+k][x+z].gyy;
            sum.gxy += grad[y+k][x+z].gxy;
        }
    }
    double tr = sum.gxx+sum.gyy;
    if (!tr) {
        return 0;
    }
    int det = sum.gxx*sum.gyy-sum.gxy*sum.gxy;
    return det/tr;
}

int main(int argc, char *argv[]) {
    assert(argc > 1);
    png::image<png::gray_pixel> img(argv[1]);
    size_t h{img.get_height()}, w{img.get_width()};
    assert(h > 2*w_size && w > 2*w_size);
    std::vector<feature> res;
    png::image<png::gray_pixel> out(w, h);
    auto grad = gradient(img);
    for (size_t i{1}; i < h-w_size-1; ++i) {
        for (size_t j{1}; j < w-w_size-1; ++j) {
            auto e = patch_interest(grad, i, j);
            if (e > ets) {
                res.emplace_back(feature{{j+w_size/2,i+w_size/2}, e});
            }
        }
    }
    std::sort(res.begin(), res.end(), [](const feature& f, const feature& s) {
        return f.e > s.e;
    });
    for_each(res.begin(), res.size() > n_features ?
                 res.begin()+n_features : res.end(), [&](const feature& fe) {
        out[fe.p.y][fe.p.x] = 255;
    });
    out.write(string(argv[1])+"-out.png");
    return 0;
}

