// gpp compa

#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>

using namespace std;

inline bool is_spacing(wchar_t c) {
    return c == L' ' || c == L'\n' || c == L'\t';
}

int main(int argc, char *argv[]) {
    enum { More, Less } opt = (argc > 1 && argv[1] == "-l"s) ? Less : More;
    locale::global(locale(""));
    vector<wchar_t> in(istreambuf_iterator<wchar_t>{wcin}, {});
    in.erase(remove_if(in.begin(), in.end(), [](const auto& i){
        return i == L'\r'; }), in.end());
    if (opt == More) {
        auto it = in.cbegin();
        if (it == in.cend()) { return 0; }
        if (is_spacing(*it)) { wcout << *it; }
        if (++it  == in.cend()) { return 0; }
        for (; it < in.cend(); ++it) {
            if (!is_spacing(*(it-1)) || !is_spacing(*it) ) {
                wcout << *it;
            }
        }
    } else {
        unique(in.begin(), in.end(), [](const auto& i, const auto& j){
            return i == j && is_spacing(i);
        });
        copy(in.begin(), in.end(), ostreambuf_iterator<wchar_t>{wcout});
    }
    return 0;
}
