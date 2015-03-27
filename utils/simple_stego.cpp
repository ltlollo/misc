#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <unistd.h>
#include <limits.h>
#include <png++/png.hpp>

// gpp simple_stego.cpp -lpng

using namespace std;
using namespace png;

using png_t = image<rgb_pixel>;

vector<bool> to_bitstream(const vector<char>& in) {
    auto res = vector<bool>();
    res.reserve(in.size() * CHAR_BIT);
    for(const auto& c: in) {
        for (uint8_t i = 0; i < CHAR_BIT; ++i) {
            res.push_back((c>>i)&1);
        }
    }
    return res;
}

vector<char> to_stream(const vector<bool>& in) {
    auto rs = in.size()/CHAR_BIT;
    auto res = vector<char>();
    res.reserve(rs);
    for (auto it = begin(in); it < begin(in) + rs*CHAR_BIT; it += CHAR_BIT) {
        char c = 0;
        for (uint8_t i = 0; i < CHAR_BIT; ++i) {
            c |= (char(*(it+i))<<i);
        }
        res.push_back(c);
    }
    return res;
}

#define ENC_MSG(color) \
    do { \
         if (bool((fst_ele.color ^ snd_ele.color)&1) != msg[count]) { \
             auto& px = d(gen) ? fst_ele : snd_ele; \
             if (px.color == 0) { \
                 px.color = 1; \
             } else if (px.color == 255) { \
                 px.color = 254; \
             } else { \
                 px.color += d(gen) ? +1 : -1; \
             } \
         } \
         if (++count == msg.size()) { \
             return count; \
         } \
    } while(0)

#define DEC_MSG(color) \
    do { \
        msg.emplace_back(bool((img_f[i][j].color ^ img_s[i][j].color)&1)); \
    } while(0)

auto enc(const vector<bool> msg, png_t& img_f, png_t& img_s) {
    size_t count = 0;
    if (msg.empty()) {
        return count;
    }
    random_device rd;
    default_random_engine gen(rd());
    discrete_distribution<> d({50, 50});
    size_t min_r = min(img_f.get_height(), img_s.get_height()),
           min_c = min(img_f.get_width(), img_s.get_width());
    for (size_t i = 0; i < min_r; ++i) {
        for (size_t j = 0; j < min_c; ++j) {
            auto& fst_ele = img_f[i][j];
            auto& snd_ele = img_s[i][j];
            ENC_MSG(red);
            ENC_MSG(blue);
            ENC_MSG(green);
        }
    }
    return count;
}

auto dec(const png_t& img_f, const png_t& img_s) {
    auto msg = vector<bool>();
    size_t min_r = min(img_f.get_height(), img_s.get_height()),
           min_c = min(img_f.get_width(), img_s.get_width());
    for (size_t i = 0; i < min_r; ++i) {
        for (size_t j = 0; j < min_c; ++j) {
            DEC_MSG(red);
            DEC_MSG(blue);
            DEC_MSG(green);
        }
    }
    return to_stream(msg);
}

void check_sizes(const auto& img) {
    if (img.get_height() < 10 || img.get_width() < 10) {
        throw runtime_error("unsufficient size");
    }
}

/*
 * The original implementation is uses some filters to find the best spots
 * in order to minimize sig alteration, but it'not yet correct/complete
 * NOTE: 1px bigger erosion block
 */

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
         cerr << "Usage: " << argv[0] << " -f fst -s snd {-e|-d}"
              << "\n\t-f fst<string>: file name of the fist png image"
              << "\n\t-s snd<string>: file name of the second png image"
              << "\n\t-e: perform encoding"
              << "\n\t-e: perform decoding"
              << "\nScope: simple LSB gtegonograpy using two images"
              << endl;
    };
    char* ifname_f{nullptr},* ifname_s{nullptr};
    enum { None, Enc, Dec} op = None;
    int opt;
    while ((opt = getopt(argc, argv, "edhvf:s:")) != -1) {
        switch (opt) {
        case 'f':           // first image filename
            ifname_f = optarg;
            break;
        case 's':           // second image filename
            ifname_s = optarg;
            break;
        case 'e':           // encode operation
            op = Enc;
            break;
        case 'd':           // decode operation
            op = Dec;
            break;
        case 'h':           // print help and exit
            print_help();
            return 0;
        default:            // print help and die
            print_help();
            return 1;
        }
    }
    if (ifname_f == nullptr || ifname_s == nullptr || op == None) {
        print_help();
        return 1;
    }
    auto img_f = png_t(ifname_f);
    auto img_s = png_t(ifname_s);
    check_sizes(img_f);
    check_sizes(img_s);
    if (op == Enc) {
        vector<char> in;
        copy(istreambuf_iterator<char>{cin}, {}, back_inserter(in));
        auto msg = to_bitstream(in);
        auto esize = enc(msg, img_f, img_s);
        img_f.write(string(ifname_f) + ".enc.png"s);
        img_s.write(string(ifname_s) + ".enc.png"s);
        cerr << "[I]: bits encoded " << esize << endl;
    } else if (op == Dec) {
        auto msg = dec(img_f, img_s);
        copy(begin(msg), end(msg), ostreambuf_iterator<char>{cout});
    } else {
        print_help();
        return 1;
    }
    return 0;
}
