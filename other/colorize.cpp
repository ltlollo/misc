#include <vector>
#include <iostream>
#include <locale>
#include <err.h>

using namespace std;

template<size_t N, typename T> constexpr auto size(const T(&)[N]) { return N; }
using color_t = uint32_t;
static size_t ringpos = 0;
static constexpr color_t colring[] = {
      0xffffff
    , 0xff00ff
    , 0x88ff00
    , 0x0044ff
    , 0x44ffff
    , 0xff8844
};
static_assert(size(colring) > 0, "size cannot be empty");
static size_t prev() {
    ringpos = (size(colring)+ringpos-1)%size(colring);
    return ringpos;
}
static size_t next() {
    ringpos = (ringpos+1)%size(colring);
    return ringpos;
}
struct Encl {
    enum { Paren, Bracket, Scope} encl;
};
auto to_encl(wchar_t c) {
    switch (c) {
    case '(': case ')': return Encl::Paren;
    case '[': case ']': return Encl::Bracket;
    case '{': case '}': return Encl::Scope;
    default: err(1, "not enclosure");
    }
}
void raw_set_col(const color_t col) {
        std::wcout << "\033[38;2;"
            << ((col>> 0)&0xff) << ";"
            << ((col>> 8)&0xff) << ";"
            << ((col>>16)&0xff) << "m"
            ;

}

void set_color(vector<Encl>& vs, const wchar_t c) {
    vs.push_back({ to_encl(c) });
    raw_set_col(colring[next()]);
}

void restore_color(vector<Encl>& vs, const wchar_t c) {
    if (vs.empty()) {
        return;
    }
    if (vs[vs.size()-1].encl != to_encl(c)) {
        return;
    }
    raw_set_col(colring[prev()]);
    vs.resize(vs.size()-1);
}


int main(int, char *[]) {
    auto l = std::locale("");
    std::locale::global(l);
    auto vstack = std::vector<Encl>();
    auto beg = std::istreambuf_iterator<wchar_t>(std::wcin);
    auto end = std::istreambuf_iterator<wchar_t>();
    raw_set_col(colring[ringpos]);
    while (beg != end) {
        switch (*beg) {
        case '\\':
            std::wcout << *beg++;
            if (beg != end) {
                std::wcout << *beg++;
            } break;
        case '(': case '[': case '{':
            std::wcout << *beg;
            set_color(vstack, *beg++);
            break;
        case ')': case ']': case '}':
            restore_color(vstack, *beg);
            std::wcout << *beg++;
            break;
        case '\'':
            std::wcout << *beg++;
            raw_set_col(colring[next()]);
            while (beg != end) {
                if (*beg == '\\') {
                    std::wcout << *beg++;
                    if (beg != end) {
                        std::wcout << *beg++;
                    }
                    continue;
                }
                if (*beg == '\'') {
                    raw_set_col(colring[prev()]);
                    std::wcout << *beg++;
                    break;
                }
                std::wcout << *beg++;
            } break;
        case '\"':
            std::wcout << *beg++;
            raw_set_col(colring[next()]);
            while (beg != end) {
                if (*beg == '\\') {
                    std::wcout << *beg++;
                    if (beg != end) {
                        std::wcout << *beg++;
                    }
                    continue;
                }
                if (*beg == '\"') {
                    raw_set_col(colring[prev()]);
                    std::wcout << *beg++;
                    break;
                }
                std::wcout << *beg++;
            } break;
        default:
            std::wcout << *beg++;
        }
    }
    return 0;
}
