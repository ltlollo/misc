#include <vector>
#include <algorithm>
#include <png++/png.hpp>

constexpr unsigned w_size{2}, n_features{500}, d_win{1};
constexpr double ets{5000};

static_assert(d_win > 0, "invalid d_win");
static_assert(w_size > d_win, "invalid w_size");

struct feature {
    struct pt { size_t x, y;} p;
    double e;
};

struct pderiv {
    int gxx, gyy, gxy;
};

using mgrad = std::vector<std::vector<pderiv>>;

mgrad gradient(const png::image<png::gray_pixel>& img) {
    size_t h{img.get_height()}, w{img.get_width()};
    mgrad grad(h, std::vector<pderiv>(w, pderiv{0, 0, 0}));
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

inline double patch_eval(const mgrad& grad, unsigned y, unsigned x) noexcept {
    pderiv sum{0, 0, 0};
    for (unsigned k{0}; k < w_size; ++k) {
        for (unsigned z{0}; z < w_size; ++z) {
            sum.gxx += grad[y+k][x+z].gxx;
            sum.gyy += grad[y+k][x+z].gyy;
            sum.gxy += grad[y+k][x+z].gxy;
        }
    }
    double tr{sum.gxx+sum.gyy};
    if (!tr) {
        return 0.0;
    }
    int det{sum.gxx*sum.gyy-sum.gxy*sum.gxy};
    return det/tr;
}

int main(int argc, char *argv[]) {
    assert(argc > 1);
    png::image<png::gray_pixel> img(argv[1]);
    size_t h{img.get_height()}, w{img.get_width()};
    assert(h > w_size+d_win && w > w_size+d_win);
    std::vector<feature> res;
    png::image<png::gray_pixel> out(w, h);
    auto grad = gradient(img);
    for (size_t i{d_win}; i < h-w_size-d_win; ++i) {
        for (size_t j{d_win}; j < w-w_size-d_win; ++j) {
            double e = patch_eval(grad, i, j);
            if (e > ets) {
                res.emplace_back(feature{{j+w_size/2,i+w_size/2}, e});
            }
        }
    }
    std::sort(res.begin(), res.end(), [](const feature& f, const feature& s) {
        return f.e > s.e;
    });
    std::for_each(res.begin(), res.size() > n_features
                  ? res.begin()+n_features : res.end(), [&](const feature& fe) {
        out[fe.p.y][fe.p.x] = 255;
    });
    out.write(std::string(argv[1])+"-out.png");
    return 0;
}

