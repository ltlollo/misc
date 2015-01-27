#include <vector>
#include <string>
#include <iostream>
#include <thread>
#include <regex>
#include <png++/png.hpp>
#include <extra/task.h>

/* comp: gpp distrx -lpng
 * use: ./distrx 10 "2*3?(4+1)+"
 */

using namespace std;

using u = size_t;
using col = png::rgb_pixel;
using px = uint8_t;
using pxg = px[3];

constexpr pxg fst_c{0, 100, 50}, snd_c{255, 50, 70};

col translate(const pxg& snd, const pxg& fst, double norm_m) {
    auto comp = [](double norm, px snd, px fst){
        if(fst > snd) {
            norm = 1 - norm;
        }
        return min(snd, fst) + norm * abs(snd - fst);
    };
    return col(comp(norm_m, snd[0], fst[0]),
               comp(norm_m, snd[1], fst[1]),
               comp(norm_m, snd[2], fst[2]));
}

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

unsigned distance(u fst, u snd) {
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

auto morph(u px, const vector<u> matches, unsigned depth) {
    unsigned min = depth;

    for (const auto& it: matches) {
        auto matching = depth - distance(it, px);
        if (matching == 0) {
            break;
        } else if (matching < min) {
            min = matching;
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
    Img apply(const string& restr) {
        const regex re(restr, regex::optimize);
        auto f = [re](const u& val) noexcept {
            cmatch sm;
            string s{to_string(val)};
            return regex_match(s.c_str(), sm, re);
        };

        auto pres = vector<u>{};
        task::map_reduce(data, [](auto it) { return it; }, f, [&](auto& it){
                    pres.insert(end(pres),                                                
                       make_move_iterator(begin(it.result)),                    
                       make_move_iterator(end(it.result))                       
                      );}, task::Threads<4>());
        auto res = vector<col>{};
        res.reserve(data.size());
        for (const auto& it: data) {
            auto m = morph(it, pres, depth)/double(depth);
            res.emplace_back(translate(snd_c, fst_c,  m));
        }
        return Img(move(res));
    }
};

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
        cerr << "USAGE\t" << argv[0] << " depth regex\n"
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
    Mat(atoi(argv[1])).apply(argv[2]).save("out.png");
    return 0;
}
