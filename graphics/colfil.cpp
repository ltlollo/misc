#include <iostream>
#include <png++/png.hpp>

// gpp colfil -lpng

using namespace std;
using img_t = png::image<png::rgb_pixel>;

int main(int argc, char *argv[]) {
    auto print_help = [&](){
        cerr << "Usage:"<< argv[0] << " -i in -o -<col> [-<col>]"
                "\n\t-i <string>: input png file"
                "\n\t-o <string>: output png file"
                "\n\t-<col>: {r|g|b},  color to filter"
                "\nScope: filter out red/green/blue color in a png"
             << endl;
    };
    int opt;
    char* inname = nullptr, *outname = nullptr;
    bool b = false, r = false, g = false;
    while ((opt = getopt(argc, argv, "rgbhi:o:")) != -1) {
        switch (opt) {
        case 'i':
            inname = optarg;
            break;
        case 'o':
            outname = optarg;
            break;
        case 'r':
            r = true;
            break;
        case 'g':
            g = true;
            break;
        case 'b':
            b = true;
            break;
        case 'h':
            print_help();
            return 0;
        default:
            print_help();
            return 1;
        }
    }
    if (inname == nullptr || outname == nullptr || !(r|g|b)) {
        print_help();
        return 1;
    }
    auto img = img_t(inname);
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            if (r) { img[i][j].red   = 0; }
            if (g) { img[i][j].green = 0; }
            if (b) { img[i][j].blue  = 0; }
        }
    }
    img.write(outname);
    return 0;
}

