#include <vector>
#include <string>
#include <iostream>
#include <thread>
#include <regex>
#include "work/workers.h"
#include <png++/png.hpp>

using namespace std;
using u = size_t;

void populate(u* mat, u size, u rawsize) noexcept {
    if (size <= 1) {
        return;
    }
    for (u i{0}; i < size; ++i) {
        for (u j{0}; j < size; ++j) {
            *(mat + j + i*rawsize) *= 10;
            *(mat + j + i*rawsize) +=
                    (i < size/2) ? ((j < size/2) ? 2 : 1)
                                 : ((j < size/2) ? 3 : 4);
        }
    }
    populate(mat,                    size/2, rawsize);
    populate(mat+size/2,             size/2, rawsize);
    populate(mat+size/2*rawsize,     size/2, rawsize);
    populate(mat+size/2*(rawsize+1), size/2, rawsize);
}

using col = png::rgb_pixel;
using px = uint8_t;

class Img {
    vector<col> pxs;
public:
    template<typename T> Img(T&& pxs) : pxs{forward<T>(pxs)} {}
    void save(const string& out) {
        u size{(u)sqrt(pxs.size())};
        png::image<png::rgb_pixel> image(size, size);
        for (u i = 0; i < size; ++i) {
            for (u j = 0; j < size; ++j) {
                image[i][j] = pxs[i*size+j];
            }
        }
        image.write(out);
    }
};

class Mat {
    u size;
    vector<string> data;

public:
    Mat(uint8_t n) : size{(u)pow(2, n)} {
        vector<u> pd(size*size, 0);
        populate(&pd[0], size, size);
        data.reserve(size*size);
        for (const auto& it: pd) {
            data.emplace_back(to_string(it));
        }
    }
    Img apply(const string& restr) {
        vector<const string*> d;
        d.reserve(data.size());
        for (u i{0}; i < data.size(); ++i) {
            d.emplace_back(&data[i]);
        }
        const regex re(restr, regex::optimize);
        function<col(const string*)> f = [re](const string* s) {
            cmatch sm;
            if (!regex_match(s->c_str(), sm, re)) {
                return col(255, 255, 255);
            }
            px r = (sm.length(1) > 0) ? 255u/s->size()*sm.length(1) : 0;
            px b = (sm.length(2) > 0) ? 255u/s->size()*sm.length(2) : 0;
            px g = (sm.length(3) > 0) ? 255u/s->size()*sm.length(3) : 0;
            return col(r, b, g);
        };
        auto res = work::static_work_balancer(d, f, work::Num<4>());
        return Img(move(res));
    }
};


int main(int argc, char *argv[]) {
    auto print_help = [&]() {
        cerr << "USAGE\t" << argv[0] << " depth regex\n"
             << "\tdepth<unsigned>: recursion depth of the canvas generator, determines the image size (size: 2^depth, 2^10 is 1024)\n"
             << "\tregex<string>: self explanatory\n"
             << "SCOPE: http://www.reddit.com/r/dailyprogrammer/comments/2fkh8u/9052014_challenge_178_hard_regular_expression/\n"
             << endl;
    };
    if (argc < 3) {
        print_help();
        return 1;
    }
    Mat(atoi(argv[1])).apply(argv[2]).save("out.png");
    return 0;
}
