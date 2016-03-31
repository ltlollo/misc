// gpp self

#include <algorithm>
#include <codecvt>
#include <experimental/string_view>
#include <iostream>
#include <locale>
#include <string.h>
#include <string>
#include <vector>

using namespace std::experimental;

int main(int argc, char *argv[]) {
    auto print_usage = [&]() {
        std::wcerr << "Usage:\t" << argv[0]
                   << L" N S\nScope:\tpresent"
                      " text using N runes per line, using S to indicate line"
                      " wrap.\n\tN<uint+>: runes per line (default: 79)\n\t"
                      "S<str>: newline indicator (default ' ↵')"
                   << std::endl;
    };
    std::locale::global(std::locale(""));
    std::wstring_convert<std::codecvt_utf8<wchar_t>> converter;
    std::ostreambuf_iterator<wchar_t> wc{std::wcout};
    size_t max = (argc > 1) ? [&]() {
        size_t res;
        char* end;
        if ((res = std::strtoll(argv[1], &end, 10)) != 0 &&
            end == argv[1] + strlen(argv[1])) { // OOR is not an error
            return res;
        }
        std::wcerr << "[E]: " << converter.from_bytes(argv[1]) << "; 0/NaN\n";
        print_usage();
        exit(EXIT_FAILURE);
    }() : 79;
    std::wstring sepa = (argc > 2) ? converter.from_bytes(argv[2]) : L" ↵";
    sepa += '\n';
    if (max <= sepa.length()) {
        std::copy(std::istreambuf_iterator<wchar_t>{std::wcin}, {}, wc);
        std::wcerr << "[E]: separator size must be greater than line size\n";
        exit(EXIT_FAILURE);
    }
    for (std::wstring s; std::getline(std::wcin, s);) {
        auto it = std::begin(s);
        auto se = std::find_if_not(it, std::end(s), std::iswspace);
        auto d = size_t(se - it);
        auto m = (se != std::end(s) && max > d) ? max - d : max;
        auto fst = true;
        while (size_t(distance(it, std::end(s))) > max) {
            auto rpos = wstring_view(&*it, m).find_last_of(' ');
            if (rpos == std::wstring::npos) {
                if (m != max && !fst) {
                    std::copy(std::begin(s), se, wc);
                }
                std::copy(it, it + m - sepa.length() - 1, wc);
                std::copy(std::begin(sepa), std::end(sepa), wc);
                it += m - sepa.length() - 1;
            } else {
                *(it + rpos) = '\n';
                if (m != max && !fst) {
                    std::copy(std::begin(s), se, wc);
                }
                std::copy(it, it + rpos + 1, wc);
                it += rpos + 1;
            }
            fst = false;
        }
        if (m != max && !fst) {
            std::copy(std::begin(s), se, wc);
        }
        std::copy(it, std::end(s), wc);
        std::wcout << std::endl;
    }
    return 0;
}

