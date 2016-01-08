// gpp self

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <locale>
#include <random>
#include <unistd.h>
#include <thread>
#include <stdio.h>
#include "mat.h"

using namespace std;

random_device rd;
default_random_engine gen(rd());

bool bitmap[512];

#define UNSAFE_ACCESS(x) const_cast<volatile decltype(x)>(x)

void init_bitmap() noexcept {
    auto d = bernoulli_distribution(generate_canonical<double, 8>(gen));
    for (auto& e: bitmap) { UNSAFE_ACCESS(e) = d(gen); }
}

wchar_t rk() { return uniform_int_distribution<wchar_t>(0x4E00, 0x9FC0)(gen); }

constexpr double avg_luma() { return 0.299*82+0.587*204+0.114*82; }

int main(int argc, char *argv[]) {
    if (argc-1 < 1) {
        cerr << "[E]: wrong number of argument"
            "\nUsage:\t" << argv[0] <<" w h f"
            "\n\tw<unsigned>: output width"
            "\n\th<unsigned>: output height"
            "\n\tf<unsigned>: reinit freq"
            "\nScope:\trandomized cellular automatons"
            "\n";
        return 1;
    }
    unsigned w = (argc > 1) ? atoi(argv[1]) : 180,
             h = (argc > 2) ? atoi(argv[2]) : 50,
             f = (argc > 3) ? atoi(argv[3]) : 25;
    locale::global(locale(""));
    constexpr wchar_t dead_sym = 0x3000;
    auto is_alive = [](const wchar_t c) { return c != dead_sym; };
    auto clear = []() {
        wcout << "\033[2J\033[1;1H\033[38;2;82;204;82;48;2;0;0;0m";
    };
    auto print = [](auto& mat) {
        for (size_t i = 1; i < mat.height-1 ; ++i) {
            for (size_t j = 1; j < mat.width-1 ; ++j) {
                wcout << mat[i][j];
            }
            wcout << '\n';
        }
    };
    init_bitmap();
    auto mat = Mat<wchar_t>(w+2, h+2);
    fill(mat.data, mat.data+mat.size(), dead_sym);
    auto next = Mat<wchar_t>(mat.width, mat.height);
    fill(next.data, next.data+next.size(), dead_sym);
    auto ipop_rat = [&](const auto& m) {
        size_t n = 1;
        m.for_each([&](auto* ele) { if (is_alive(*ele)) ++n; });
        return m.size()/double(n);
    };
    auto nclear = [&](const auto& m) {
        auto  ip = ipop_rat(m);
        if (ip > 3) ip = 3;
        unsigned r = (50*(0.23*ip))/0.23,
                 g = (100*(0.54*ip))/0.54,
                 b = (50*(0.31*ip))/0.31;
        if (r>255) r = 255; if (g>255) g = 255; if (b>255) b = 255;
        wcout << "\033[2J\033[1;1H\033[38;2;" << r << ";" << g << ";" << b
            << ";48;2;0;0;0m";
    };
    auto loop = [&]() {
        zip(mat, next, {1, mat.width-2, 1, mat.height-2},
            [&](auto* mat_ele, auto* next_ele) {
            auto vw = mat.view(mat_ele);
            uint16_t pattern = 0;
            uint8_t count = 0, alive = 0;
            for (int8_t ri = -1; ri <= 1; ++ri) {
                for (int8_t rj = -1; rj <= 1; ++rj) {
                    if (is_alive(vw[ri][rj])) { ++alive; }
                    pattern = uint16_t(pattern|
                             (uint16_t(is_alive(vw[ri][rj]))<<(count++)));
                }
            }
            if (UNSAFE_ACCESS(bitmap[pattern])) {
                if (alive) {
                    alive = uniform_int_distribution<uint8_t>(1, alive)(gen);
                    count = 0;
                    for (int8_t ri = -1; ri <= 1; ++ri) {
                        for (int8_t rj = -1; rj <= 1; ++rj) {
                            if (!is_alive(vw[ri][rj])) continue;
                            if (++count == alive) {
                                *next_ele = vw[ri][rj];
                                break;
                            }
                        }
                    }
                } else {
                    *next_ele = rk();
                }
            } else {
                *next_ele = dead_sym;
            }
        });
        if (argc < 5) { wrap(next); }
        nclear(next);
        swap(mat.data, next.data);
        print(mat);
        usleep(50000);
    };
    if (f) {
        for (unsigned i = 0;;) {
            loop();
            if (!(++i%f)) { init_bitmap(); }
        }
    } else {
        std::thread([](){
            for(int c = 0;;) {
                if ((c = getchar()) == '\n') {
                    init_bitmap();
                }
            }
        }).detach();
        while(true) {
            loop();
        }
    }
}

