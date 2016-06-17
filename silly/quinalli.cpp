// gpp self -lpng

#include <algorithm>
#include <err.h>
#include <png++/png.hpp>
#include <stdio.h>
#include <string>
#include <vector>

using namespace std;
using px_t = png::rgb_pixel;
using png_t = png::image<px_t>;
constexpr auto lmap = "   ..:ccccooocc:....   ";

unsigned
lu(px_t &p) {
    return unsigned(0.299 * p.red + 0.587 * p.green + 0.114 * p.blue) / 20;
}

int
main(int argc, char *argv[]) {
    if (argc < 3) {
        err(1, "Usage: png size [-i]\nScope:\tprints pgm/ascii grayscale "
               "polyglot from a png image given a size"
               "\n\timg<string>: png input file"
               "\n\tsize<unsigned>: width of the printed output (required > 0)"
               "\n\t-i: inverts the output");
    }
    bool inv = false;
    if (argc > 3 && argv[3] == "-i"s) {
        inv = true;
    }
    png_t img(argv[1]);
    size_t s = stol(argv[2]), w = img.get_width(), h = img.get_height();
    if (!w || !h || !(s + 1) || !(s * h / w)) {
        err(1, "size too small");
    }
    printf("P5 %ld %ld %d", s + 1, s * h / w, 'o');
    for (size_t i = 0; i < s * h / w; ++i) {
        putchar('\n');
        for (size_t j = 0; j < s; ++j) {
            putchar(lmap[lu(img[i * w / s][j * w / s]) + (inv ? 10 : 0)]);
        }
    }
    putchar('\n');
    return 0;
}

