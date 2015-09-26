// gpp self

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <random>
#include <unistd.h>
#include "mat.h"

using namespace std;

random_device rd;
default_random_engine gen(rd());

int main(int argc, char *argv[]) {
    if (argc-1 != 1) {
        cerr << "[E]: wrong number of argument"
            "\nUsage:\t" << argv[0] <<" file"
            "\n\tfile<sintrg>: the input file"
            "\nScope:\tr/dailyprogrammer 2015-09-23 on toroidal space"
            "\n";
        return 1;
    }
    constexpr char dead_sym = ' ';
    auto is_alive = [](const char c) { return c != dead_sym; };
    auto clear = []() { cout << "\033[2J\033[1;1H"; };
    auto mat = [&]() {
        size_t dim = 0;
        vector<string> in;
        ifstream f(argv[1]);
        for (string tmp; getline(f, tmp);) {
            if (tmp.size() > dim) {
                dim = tmp.size();
            }
            in.push_back(move(tmp));
        }
        auto m = Mat<char>(dim+2, in.size()+2);
        fill(m.data, m.data+m.width*m.height, dead_sym);
        for (size_t i = 0; i < in.size(); ++i) {
            for (size_t j = 0; j < in[i].size(); ++j) {
                m[i+1][j+1] = in[i][j];
            }
        }
        return m;
    }();
    auto print = [](auto& mat) {
        for (size_t i = 1; i < mat.height-1; ++i) {
            for (size_t j = 1; j < mat.width-1; ++j) {
                cout << mat[i][j];
            }
            cout << '\n';
        }
    };
    auto next = Mat<char>(mat.width, mat.height);
    copy(mat, next);
    wrap(mat);
    auto loop = [&]() {
        zip(mat, next, {1, mat.width-2, 1, mat.height-2},
            [&](auto* mat_ele, auto* next_ele) {
            uint8_t alive = 0;
            auto vw = mat.view(mat_ele);
            vw.for_each([&](auto* e) { if(is_alive(*e)) ++alive; },
                        Bound<int8_t>{-1, 1, -1, 1}, 0b111101111);
            if (is_alive(*mat_ele)) {
                if (alive == 2 || alive == 3) {
                    *next_ele = *mat_ele;
                } else {
                    *next_ele = dead_sym;
                }
            } else {
                if (alive == 3) {
                    alive = uniform_int_distribution<uint8_t>(1, alive)(gen);
                    uint8_t count = 0;
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
                    *next_ele = dead_sym;
                }
            }
        });
        clear();
        print(mat);
        wrap(next);
        swap(mat.data, next.data);
    };
    while (true) {
        loop();
        usleep(50000);
    }
}

