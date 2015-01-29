#include <vector>
#include <string>
#include <iostream>
#include <thread>
#include <regex>
#include <png++/png.hpp>
#include <extra/task.h>
#include <stdlib.h>
#include <extra/utils.h>

/* comp: gpp distrx -lpng
 * use: ./distrx 10 "2*3?(4+1)+"
 */

using namespace std;

using u = size_t;
using col = png::rgb_pixel;
using px = uint8_t;
using rgb_t = px[3];
using grad_t = rgb_t[2];

col to_col(const grad_t& grad, double norm_m) {
    auto comp = [](double norm, px snd, px fst){
        if(fst > snd) {
            norm = 1 - norm;
        }
        return min(snd, fst) + norm * abs(snd - fst);
    };
    return col(comp(norm_m, grad[0][0], grad[1][0]),
               comp(norm_m, grad[0][1], grad[1][1]),
               comp(norm_m, grad[0][2], grad[1][2]));
}

void populate(u* mat, u size, u rawsize) noexcept {
    if (size <= 1) {
        return;
    }
    for (u i{0}; i < size; ++i) {
        for (u j{0}; j < size; ++j) {
            *(mat + j + i*rawsize) *= 10;
            *(mat + j + i*rawsize) +=
                    (i < size/2) ? ((j < size/2) ? 1 : 2)
                                 : ((j < size/2) ? 3 : 4);
        }
    }
    populate(mat,                    size/2, rawsize);
    populate(mat+size/2,             size/2, rawsize);
    populate(mat+size/2*rawsize,     size/2, rawsize);
    populate(mat+size/2*(rawsize+1), size/2, rawsize);
}

unsigned n_edits(u fst, u snd) {
    unsigned dist = 0;
    while(fst) {
        if (fst%10 == snd%10) {
            dist++;
        }
        fst /= 10;
        snd /= 10;
    }
    return dist;
}

auto min_distance(u px, const vector<u>& matches, unsigned depth) {
    unsigned min = depth;
    for (const auto& it: matches) {
        auto distance = depth - n_edits(it, px);
        if (distance == 0) {
            return 0u;
        } else if (distance < min) {
            min = distance;
        }
    }
    return min;
}

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
    vector<u> data;
    unsigned depth;

public:
    Mat(uint8_t n) : depth{n}, size{(u)pow(2, n)},
        data(size*size, 0) {
        populate(&data[0], size, size);
    }
    Img apply(const string& restr, const grad_t& grad) {
        const regex re(restr, regex::optimize);
        auto s_priv = string(depth, '0');
        auto filter = [s_priv, re](const u& val) mutable noexcept {
            s_priv = to_string(val);
            cmatch sm;
            return regex_match(s_priv.c_str(), sm, re);
        };
        auto identity = [](auto it) { return it; };
        auto dzero = vector<u>{};
        task::map_reduce(data, identity, filter, [&](auto& it){
                    dzero.insert(end(dzero),
                       make_move_iterator(begin(it.result)),                    
                       make_move_iterator(end(it.result))                       
                      );}, task::Threads<4>());
        auto res = vector<col>{};
        task::map_reduce(data,
            [&](u it) { return to_col(grad, min_distance(it, dzero, depth)/double(depth)); },
            [&](auto& it){                 
                    res.insert(end(res),                                    
                       make_move_iterator(begin(it.result)),                    
                       make_move_iterator(end(it.result))                       
                      );}, task::Threads<4>()); 
        return Img(move(res));
    }
};

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
        cerr << "USAGE\t" << argv[0] << " depth regex [grad_params]\n"
             << "\tdepth<unsigned>: recursion depth of the canvas generator, "
                "determines the image size (size: 2^depth, 2^10 is 1024)\n"
             << "\tregex<string>: self explanatory\n"
             << "SCOPE: https://ssodelta.wordpress.com/2015/01/26/gradient"
             "-images-from-regular-expressions/\n"
             << endl;
    };
    if (argc < 3) {
        print_help();
        return 1;
    }
    auto depth = atoi(argv[1]);
    auto rx = argv[2];
    grad_t grad{{0x0, 0x0, 0x0}, {0xFF, 0xFF, 0xFF}};
    if (argc == 9) {
        grad[0][0] = atoi(argv[3]);
        grad[0][1] = atoi(argv[4]);
        grad[0][2] = atoi(argv[5]);
        grad[1][0] = atoi(argv[6]);
        grad[1][1] = atoi(argv[7]);
        grad[1][2] = atoi(argv[8]);
    }
    Mat(depth).apply(rx, grad).save("out.png");
    return 0;
}
