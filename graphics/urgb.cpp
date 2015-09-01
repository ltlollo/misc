// gpp self -lpng

#include <strings.h>
#include <png++/png.hpp>

using namespace std;
using img_t = png::image<png::rgb_pixel>;

constexpr uint8_t red = 1, green = 2, blue = 4;

void rm(img_t& img, uint8_t c) {
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            if (c&red)   { img[i][j].red   = 0; }
            if (c&green) { img[i][j].green = 0; }
            if (c&blue)  { img[i][j].blue  = 0; }
        }
    }
}

void cp(img_t& img, uint8_t src, uint8_t dst) {
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            if (src&red) {
                if (dst&green) { img[i][j].green = img[i][j].red; }
                if (dst&blue)  { img[i][j].blue  = img[i][j].red; }
            } else if (src&green) {
                if (dst&red)   { img[i][j].red   = img[i][j].green; }
                if (dst&blue)  { img[i][j].blue  = img[i][j].green; }
            } else if (src&blue) {
                if (dst&red)   { img[i][j].red   = img[i][j].blue; }
                if (dst&green) { img[i][j].green = img[i][j].blue; }
            }
        }
    }
}

void mv(img_t& img, uint8_t src, uint8_t dst) {
    for (size_t i = 0; i < img.get_height(); ++i) {
        for (size_t j = 0; j < img.get_width(); ++j) {
            if (src&red) {
                if (dst&green)  { img[i][j].green = img[i][j].red; }
                if (dst&blue)   { img[i][j].blue  = img[i][j].red; }
                if (!(dst&red)) { img[i][j].red = 0; }
            } else if (src&green) {
                if (dst&red)      { img[i][j].red   = img[i][j].green; }
                if (dst&blue)     { img[i][j].blue  = img[i][j].green; }
                if (!(dst&green)) { img[i][j].green = 0; }
            } else if (src&blue) {
                if (dst&red)     { img[i][j].red   = img[i][j].blue; }
                if (dst&green)   { img[i][j].green = img[i][j].blue; }
                if (!(dst&blue)) { img[i][j].blue = 0; }
            }
        }
    }
}

template<typename T> constexpr bool cmp(T) { return false; }
template<size_t N, typename... Ts>
bool cmp(const char* f, const char (&s)[N],  const Ts&... argv) {
    return (strcmp(f, s) == 0) || cmp(f, argv...);
}

uint8_t parse_cols(char* s) {
    uint8_t r = 0;
    while (*s != '\0') {
        switch(*s++) {
        case 'r':
            r |= red;
            break;
        case 'g':
            r |= green;
            break;
        case 'b':
            r |= blue;
            break;
        default:
            return 0;
        }
    }
    return r;
}

bool uniq(uint8_t c) {
    switch(c) {
        case red: return true;
        case green: return true;
        case blue: return true;
    }
    return false;
}

void hint_cmdline(int argc, char** argv, int n) {
    printf("    ");
    for (int i = 0; i < argc; ++i) {
        printf("%*s ", 1, argv[i]);
    }
    printf("\n    ");
    for (int i = 0; i < argc; ++i) {
        printf("%-*c ", int(strlen(argv[i])), (i == n) ? '^':' ');
    }
    printf("\n");
}

int main(int argc, char **argv) {
    auto err = [&](){
        fprintf(stderr, "Usage: %s cmd in out"
                "\n\tcmd: rm cols"
                "\n\t   | mv col cols"
                "\n\t   | cp col cols"
                "\n\tcol: r | g | b"
                "\n\tcols: col[col...]"
                "\n\tin: input png file name"
                "\n\tout: output png file name"
                "\nScope: move, remove, copy rgb pixels\n"
               , argv[0]);
        exit(1);
    };
    auto hint = [&](auto&& e, int h) {
        fprintf(stderr, "Error: %s\n", e);
        hint_cmdline(argc, argv, h);
        err();
    };
    uint8_t f, s;
    img_t img;
    switch (argc) {
    case 5:
        if (!cmp(argv[1], "rm")) {
            hint("did you mean rm?", 1);
        }
        f = parse_cols(argv[2]);
        if (!f) {
            hint("must be col[col...], where col: r|g|b", 2);
        }
        try {
            img = img_t(argv[3]);
        } catch(std::runtime_error& e) {
            hint(e.what(), 3);
        }
        rm(img, f);
        img.write(argv[4]);
        return 0;
    case 6:
        if (!cmp(argv[1], "cp", "mv")) {
            hint("did you mean cp/mv?", 1);
        }
        f = parse_cols(argv[2]);
        if (!f) {
            hint("must be r|g|b", 2);
        }
        if (!uniq(f)) {
            hint("non unique color", 2);
        }
        s = parse_cols(argv[3]);
        if (!s) {
            hint("must be col[col...], where col: r|g|b", 3);
        }
        try {
            img = img_t(argv[4]);
        } catch(std::runtime_error& e) {
            hint(e.what(), 4);
        }
        if (cmp(argv[1], "cp")) {
            cp(img, f, s);
        } else {
            mv(img, f, s);
        }
        img.write(argv[5]);
        return 0;
    default:
        err();
    }
}

