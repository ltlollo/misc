#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <curses.h>

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
        cerr << "USAGE" << argv[0] << " file\n"
             << "SCOPE: not much\n";
        return 1;
    }
    ifstream f(argv[1], ios::in|ios::binary|ios::ate);
    if (!f.is_open()) {
        throw std::runtime_error("open");
    }
    size_t size = f.tellg();
    if (size == 0) {
        throw runtime_error("empty");
    }
    f.seekg(0, ios::beg);
    vector<char> buf(size);
    f.read(&buf[0], size);
    initterm();
    size = 0;
    while(getch()) {
        addch(buf[size]);
        size = (size+1)%buf.size();
    }
    return 0;
}

