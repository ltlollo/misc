// gpp quinalli.cpp

#include <vector>
#include <iostream>
#include <algorithm>
#include <png++/png.hpp>

using namespace std;
using px_t = png::rgb_pixel;
using png_t = png::image<px_t>;
constexpr char lo = 32, mid = 46,  hi = 120;
double luma(const px_t& p) {return 0.2126*p.red+0.7152*p.green+0.0722*p.blue; }
char to_ch(double i) { return (i >= 98) ? hi : (i > 68 && i < 98) ? mid : lo; }

int main(int argc, char *argv[]) {
    if (argc < 3) {
        cerr << "Usage:\t" << argv[0] << " PNG SIZE\nScope:\tprints pgm/bash"
            " grayscale quines from PNG image of size SIZE\n";
        return 1;
    }
    png_t img(argv[1]);
    double s = stoll(argv[2]);
    size_t w = img.get_width(), h = img.get_height();
    if (!w || !h) {
        cerr << argv[1] << "too small\n";
        return 1;
    }
    printf("P5 %ld %ld %d", size_t(s+1), size_t(s/w*h), unsigned(hi));
    for (size_t i = 0; i < s/w*h; ++i) {
        cout << '\n';
        for (size_t j = 0; j < s; ++j) {
            cout << to_ch(luma(img[i/s*w][j/s*w]));
        }
    }
    cout << endl;
    return 0;
}

