#include <vector>
#include <stdexcept>
#include <fstream>
#include <ncursesw/curses.h>

// gpp yougits.cpp -lncursesw

using namespace std;

struct CursesWin {
    CursesWin() {
        initscr();
        erase();
        refresh();
        cbreak();
        noecho();
        keypad(stdscr, TRUE);
        scrollok(stdscr, TRUE);
    }
    ~CursesWin() { endwin(); }
};

void print_forever(const string& fname) {
    auto l = locale("");
    locale::global(l);
    wifstream file(fname);
    file.imbue(l);
    noskipws(file);
    if (!file.is_open()) {
        throw runtime_error("open");
    }
    vector<wchar_t> buf(istreambuf_iterator<wchar_t>{file}, {});
    auto win = CursesWin();
    cchar_t c;
    for (size_t i = 0; getch(); i = (i+1)%buf.size()) {
        c = { 0, buf[i] };
        add_wch(&c);
    }
}

int main(int argc, char *argv[]) {
    if (argc - 1 < 1) {
        printf("USAGE: %s file\nSCOPE: 攻殻機動隊 typing sim\n", argv[0]);
        return 1;
    }
    try {
        print_forever(string(argv[1]));
    } catch (runtime_error& e) {
        printf(e.what());
    } catch (...) {
        return 1;
    }
    return 0;
}

