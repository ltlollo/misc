#include <vector>
#include <algorithm>
#include <png++/png.hpp>
#include <extra/func.h>

constexpr unsigned w_size{2}, n_features{500}, d_win{1};
constexpr double ets{1000};
constexpr int smooth_win{1};

static_assert(d_win > 0, "invalid d_win");
static_assert(n_features > 0, "invalid n_features");
static_assert(smooth_win > 0, "invalid smooth_win");
static_assert(w_size > d_win, "invalid w_size");
static_assert(ets >= 0, "invalid ets");

struct feature {
    double e;
    struct pt { size_t x, y;} p;
};

struct pderiv {
    int gxx, gyy, gxy;
};

using Features = std::vector<feature>;
using RGrad = std::vector<pderiv>;
using MGrad = std::vector<RGrad>;
using GImage = png::image<png::gray_pixel>;

MGrad gradient(const GImage& img) {
    size_t h{img.get_height()}, w{img.get_width()};
    MGrad grad(h, RGrad(w, pderiv{0, 0, 0}));
    if (h < 3 || w < 3) {
        return grad;
    }
    for (size_t i{d_win}; i < h-d_win; ++i) {
        for (size_t j{d_win}; j < w-d_win; ++j) {
            grad[i][j].gxx = img[i][j-d_win] - img[i][j+d_win]; // gx
        }
    }
    for (size_t i{d_win}; i < h-d_win; ++i) {
        for (size_t j{d_win}; j < w-d_win; ++j) {
            grad[i][j].gyy = img[i-d_win][j] - img[i+d_win][j]; // gy
            grad[i][j].gxy = grad[i][j].gyy * grad[i][j].gxx;
            grad[i][j].gyy *= grad[i][j].gyy;
            grad[i][j].gxx *= grad[i][j].gxx;
        }
    }
    return grad;
}

void blur(GImage& img) {
    size_t h{img.get_height()}, w{img.get_width()};
    GImage tmp(w, h);
    for (size_t i{smooth_win}; i < h-smooth_win; ++i) {
        for (size_t j{smooth_win}; j < w-smooth_win; ++j) {
            for (int k{-smooth_win}; k < smooth_win+1; ++k) {
                tmp[i][j] += img[i][j+k]/((2*smooth_win+1)*(2*smooth_win+1));
            }
        }
    }
    img = GImage(w, h);
    for (size_t i{smooth_win}; i < h-smooth_win; ++i) {
        for (size_t j{smooth_win}; j < w-smooth_win; ++j) {
            for (int k{-smooth_win}; k < smooth_win+1; ++k) {
                img[i][j] += tmp[i+k][j];
            }
        }
    }
}

inline double patch_eval(const MGrad& grad, unsigned y, unsigned x) noexcept {
    pderiv sum{0, 0, 0};
    for (unsigned k{0}; k < w_size; ++k) {
        for (unsigned z{0}; z < w_size; ++z) {
            sum.gxx += grad[y+k][x+z].gxx;
            sum.gyy += grad[y+k][x+z].gyy;
            sum.gxy += grad[y+k][x+z].gxy;
        }
    }
    double tr = sum.gxx+sum.gyy;
    if (!tr) {
        return 0.0;
    }
    int det{sum.gxx*sum.gyy-sum.gxy*sum.gxy};
    return det/tr;
}

Features features(const MGrad& grad, double ets = 0) {
    std::vector<feature> res;
    if (grad.empty() || grad[0].empty()) {
        return res;
    }
    size_t h{grad.size()}, w{grad[0].size()};
    for (size_t i{d_win}; i < h-w_size-d_win; ++i) {
        for (size_t j{d_win}; j < w-w_size-d_win; ++j) {
            double e = patch_eval(grad, i, j);
            if (e > ets) {
                res.emplace_back(feature{e, {j+w_size/2,i+w_size/2}});
            }
        }
    }
    return res;
}

int main(int argc, char *argv[]) {
    assert(argc > 1);
    GImage img(argv[1]);
    size_t h{img.get_height()}, w{img.get_width()};
    assert(h > w_size+d_win && w > w_size+d_win);
    GImage out(w, h);
    fun::measure("processing", [&]() noexcept {
        auto res = features(gradient(img), ets);
        std::partial_sort(res.begin(), res.size() > n_features
                          ? res.begin()+n_features : res.end(), res.end(),
                        [](const feature& f, const feature& s) {
            return f.e > s.e;
        });
        std::for_each(res.begin(), res.size() > n_features
                      ? res.begin()+n_features : res.end(), [&](const feature& fe) {
            out[fe.p.y][fe.p.x] = 255;
        });
        blur(out);
    });
    out.write(std::string(argv[1])+"-out.png");
    return 0;
}

