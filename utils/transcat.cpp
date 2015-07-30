// gpp transcat.cpp

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <locale>
#include <stdlib.h>
#include <stdio.h>
#include <wchar.h>

int main(int argc, char *argv[]) {
    std::locale::global(std::locale(""));
    if (argc < 2) {
        fprintf(stderr, "Usage:\t%s dict [1]\nScope:\ttranslate @prefixes into"
                " words from a dictionary\n\t1: 1-based index\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    std::vector<std::wstring> dict;
    if (argc > 2 && argv[2] == std::string("1")) {
        dict.push_back(L"");
    }
    {
        std::wifstream df(argv[1]);
        for (std::wstring s; std::getline(df, s);) {
            dict.push_back(std::move(s));
        }
    }
    wint_t c;
    while ((c = fgetwc(stdin)) != WEOF) {
        if (c != '@') {
            fputwc(c, stdout);
            continue;
        }
        size_t n = 0;
        bool esc = false;
        while ((c = fgetwc(stdin)) != WEOF && c >= '0' && c <= '9') {
            n = n*10 + c%48;
            esc = true;
        }
        if (esc && n < dict.size()) {
            fwprintf(stdout, L"%ls", dict[n].c_str());
        }
        if (c == WEOF) {
            break;
        }
        ungetwc(c, stdin);
    }
    return 0;
}
