#include <vector>
#include <stdexcept>
#include <fstream>
#include <ncursesw/curses.h>

// gpp yougits.cpp -lncursesw

using namespace std;

void initterm() {
    initscr();
    erase();
    refresh();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    scrollok(stdscr, TRUE);
}

int main(int argc, char *argv[]) {
    if (argc - 1 < 1) {
        printf("USAGE: %s file\nSCOPE: 攻殻機動隊 typing sim\n", argv[0]);
        return 1;
    }
    auto l = locale("");
    locale::global(l);
    wifstream file(argv[1]);
    file.imbue(l);
    noskipws(file);
    if (!file.is_open()) {
        throw runtime_error("open");
    }
    initterm();
    vector<wchar_t> buf(istreambuf_iterator<wchar_t>{file}, {});
    cchar_t c;
    for (size_t i = 0; getch(); i = (i+1)%buf.size()) {
        c = { 0, buf[i] };
        add_wch(&c);
    }
    return 0;
}

