// gpp quinalli.cpp

#include <vector>
#include <iostream>
#include <algorithm>
#include <png++/png.hpp>

using namespace std;
using px_t = png::rgb_pixel;
using png_t = png::image<px_t>;
constexpr char lo = 32, lm = 46, hm = 58, hi = 120;
double luma(const px_t& p) {return 0.2126*p.red+0.7152*p.green+0.0722*p.blue; }
char to_ch(double i) {return(i>=123)?hi:(i>=98&&i<123)?hm:(i>=68&&i<98)?lm:lo;}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        cerr << "Usage:\t" << argv[0] << " PNG SIZE\nScope:\tprints pgm/bash"
            " grayscale quines from PNG image of size SIZE\n";
        return 1;
    }
    png_t img(argv[1]);
    double s = stol(argv[2]), w = img.get_width(), h = img.get_height();
    if (!w || !h || !s) {
        cerr << "[E]: " << (s ? argv[1] : "SIZE") << " too small\n";
        return 1;
    }
    printf("P5 %ld %ld %d", size_t(s+1), size_t(s*(h/w)), unsigned(hi));
    for (double i = 0; i < s*(h/w); ++i) {
        cout << '\n';
    for (double j = 0; j < s; ++j) {
            cout << to_ch(luma(img[i/s*w][j/s*w]));
        }
    }
    cout << endl;
    return 0;
}

