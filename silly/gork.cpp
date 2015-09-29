// gpp self

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <locale>
#include <random>
#include <unistd.h>
#include "mat.h"

using namespace std;

random_device rd;
default_random_engine gen(rd());

bool bitmap[512];

void init_bitmap() noexcept {
    auto d = bernoulli_distribution(std::generate_canonical<double, 8>(gen));
    for (auto& e: bitmap) { e = d(gen); }
}

wchar_t rk() { return uniform_int_distribution<wchar_t>(0x4E00, 0x9FC0)(gen); }

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
    fill(mat.data, mat.data+mat.width*mat.height, dead_sym);
    auto next = Mat<wchar_t>(mat.width, mat.height);
    fill(next.data, next.data+next.height*next.width, dead_sym);
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
            if (bitmap[pattern]) {
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
        wrap(next);
        clear();
        swap(mat.data, next.data);
        print(mat);
        usleep(50000);
    };
    for (unsigned i = 0;;) {
        loop();
        if (f && !(++i%f)) { init_bitmap(); }
    }
}

