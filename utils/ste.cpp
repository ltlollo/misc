#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <atomic>
#include <unistd.h>
#include <termio.h>
#include <err.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>

static int in  = STDIN_FILENO;
static int out = STDOUT_FILENO;
std::atomic<bool> need_redraw;

static inline void clear()     { printf("\033[2J"); }
static inline void clearline() { printf("\033[2K"); }
static inline void clearr()    { printf("\033[0K"); }
static inline void move(unsigned y, unsigned x) {
    printf("\033[%d;%dH", y+1, x+1);
}
static inline void handlesig(int) {
    signal(SIGWINCH, SIG_IGN);
    need_redraw = true;
}
static inline void rstscr() {
    clear();
    move(0, 0);
}

static void setattrs(const bool rst) {
    static termios old, curr;
    if (!rst) {
        rstscr();
        tcsetattr(in, TCSANOW, &old);
        return;
    }
    tcgetattr(in, &old);
    curr = old;
    curr.c_cc[VTIME] = 0; curr.c_cc[VMIN] = 1;
    curr.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    curr.c_cflag |=  (CS8);
    curr.c_oflag &= ~(OPOST);
    curr.c_lflag &= ~(ICANON | IEXTEN | ECHO | ECHOE | ECHOK | ECHONL);
    tcsetattr(in, TCSANOW, &curr);
    setvbuf(stdin, NULL, _IONBF, 0);
    setvbuf(stdout, NULL, _IONBF, 0);
}

static inline int readch(const int fd) {
    unsigned char c;
    if (read(fd, &c, 1) == 1) {
        return c;
    }
    return -1;
}

enum key :int {
    tab = '\t',
    nl = 13,
    del = 126,
    back,
    up = -999,
    down,
    right,
    left,
    home,
    insert,
    dead,
    end,
    pgup,
    pgdown,
    f1,
    f2,
    f3,
    f4,
    f5,
    f6,
    f7,
    f8,
    f9,
    f10,
    f11,
    f12,
    shup,
    shdown,
    shright,
    shleft,
};

static int getk() {
    int k, x;
    if ((k = readch(in)) != 033) {
        return k;
    }
    if ((k = readch(in)) == 79) {
        if ((k = readch(in)) >= 80 && k <= 83) {
            return f1 + (k-80);
        }
        return 0;
    } else if (k == '[') {
        if ((k = readch(in)) >= 'A' && k <= 'D') {
            return up + (k - 'A');
        } else if (k >= '1' && k <= '6' && k != '3') {
            if ((x = readch(in)) == 126) {
                return home + (k-'1');
            } else if (x == ';') {
                if (k == '1' && (x = readch(in)) == '2'
                    && (k = readch(in)) >= 'A' && k <= 'D') {
                    return shup + (k - 'A');
                }
                return 0;
            }
            return 0;
        }
        return 0;
    }
    return 0;
}


struct Ed {
    unsigned cols, rows, pgstep, tstep;
    unsigned pagey  = 0, pagex  = 0;
    unsigned y      = 0, x      = 0;
    unsigned ry     = 0, rx     = 0;
    bool restore = true;
    std::vector<std::vector<char>> t;
    Ed(unsigned c, unsigned r) : cols{c}, rows{r}, pgstep{rows/2},
        tstep{cols/4 > 4 ? 4 : cols/4} {
        rstscr();
    }
    unsigned absx() const { return pagex + x; }
    unsigned absy() const { return pagey + y; }
    void rst() { if (restore) { y = ry; x = rx; } }
    void sav() { if (restore) { ry = y; rx = x; } }

    void refreshline(unsigned py, unsigned px, unsigned y, unsigned x = 0) {
        move(y, x);
        auto i = t.begin() + py + y;
        if (i >= t.end() || i->empty()) {
            clearr();
            return;
        }
        for (auto j = i->begin() + px + x;
             j < i->begin() + px + cols; ++j) {
            if (j >= i->end()) {
                clearr();
                break;
            }
            if (*j && *j != '\t') {
                putchar(*j);
            } else {
                putchar(' ');
            }
        }
    }
    void refresh(unsigned py, unsigned px, unsigned y = 0) {
        move(y, 0);
        for (unsigned i = 0; i < rows; ++i) {
            refreshline(py, px, i);
        }
    }
    void mvd(unsigned step = 1) {
        if (y+step > rows-1) {
            refresh(pagey+=step, pagex);
            move(y, x);
            return;
        }
        move(y+=step, x);
    }
    void mvu(unsigned step = 1) {
        if (absy() < step) {
            t.insert(t.begin(), step, std::vector<char>());
            refresh(pagey, pagex);
            move(y, x);
            return;
        }
        if (y < step) {
            refresh(pagey-=step, pagex);
            move(y, x);
            return;
        }
        move(y-=step, x);
    }
    void mvl(unsigned step = 1) {
        if (absx() < step) {
            move(y, x = 0);
            return;
        }
        if (x < step && pagex) {
            refresh(pagey, pagex -= cols);
            move(y, x = cols - step + x);
            sav();
            return;
        }
        move(y, x-=step);
    }
    void mvr(unsigned step = 1) {
        if (cols - 1 < x + step) {
            refresh(pagey, pagex += cols);
            move(y, x = x + step - cols);
            sav();
            return;
        }
        move(y, x += step);
    }
    void insert(char c) {
        if (t.size() <= absy()) {
            t.resize(absy()+1);
        }
        auto& l = t[absy()];
        if (l.size() <= absx()) {
            l.resize(absx());
        }
        l.insert(l.begin() + absx(), c);
        refreshline(pagey, pagex, y, x);
        mvr();
    }
    void del() {
        if (t.size() <= absy()) {
            return;
        }
        auto l = t.begin() + absy();
        if (std::all_of(l->begin(), l->end(), [](auto& c) {return c == 0; })) {
            t.erase(l);
            refresh(pagey, pagex, y);
            move(y, x);
            return;
        }
        if (absx() == 0) {
            return;
        }
        if (l->size() <= absx() - 1) {
           mvl();
           return;
        }
        auto it = l->begin() + absx() -1;
        if (*it == '\t') {
            auto rit = std::make_reverse_iterator(it+1);
            auto re = std::find_if(rit, l->rend(), [](auto& c) {
                return c != '\t';
            });
            unsigned d = (unsigned)std::distance(rit, re);
            l->erase(re.base(), rit.base());
            refreshline(pagey, pagex, y);
            mvl(d);
            return;
        }
        l->erase(it);
        refreshline(pagey, pagex, y, x - 1);
        mvl();
    }
    void rm() {
        if (t.size() <= absy()) {
            return;
        }
        auto l = t.begin() + absy();
        if (std::all_of(l->begin(), l->end(), [](auto& c) {return c == 0; })) {
            t.erase(l);
            refresh(pagey, pagex, y);
            move(y, x);
            return;
        }
        if (l->size() <= absx()) {
           return;
        }
        l->erase(l->begin() + absx());
        refreshline(pagey, pagex, y, x);
        move(y, x);
    }
    void nl() {
        if (t.size() <= absy()+1) {
            t.resize(absy()+1);
        }
        auto v = std::vector<char>();
        if (absx() < t[absy()].size()) {
            auto b = std::make_move_iterator(t[absy()].begin()+absx());
            auto e = std::make_move_iterator(t[absy()].end());
            v.resize(absx());
            v.insert(v.end(), b, e);
            t[absy()].resize(absx());
        }
        t.insert(t.begin()+absy()+1, std::move(v));
        refresh(pagey, pagex, y);
        rst();
        mvd();
    }
    void refresh() {
        refresh(pagey, pagex);
        move(y, x);
    }

    void handlecmd() {
        int k;
        move(cols-1, 0);
        clearline();
        while ((k = getk()) && std::isprint(k)) {
            putchar(k);
        }
        move(y, x);
    }
};

#define cb(x) break; case (x)
#define r4(x) do{ x; x; x; x; } while(0)

void start() {
    setattrs(true);
    need_redraw = false;
    winsize w;
    ioctl(out, TIOCGWINSZ, &w);
    Ed fred(w.ws_col, w.ws_row-1);
    signal(SIGWINCH, handlesig);
    signal(SIGINT, SIG_IGN);
    int ch = 0;
    do {
        if (need_redraw) {
            ioctl(out, TIOCGWINSZ, &w);
            fred.cols = w.ws_col;
            fred.rows = w.ws_row-1;
            fred.pgstep = fred.cols/2;
            fred.tstep = w.ws_col/4 > 4 ? 4 : w.ws_col/4;
            need_redraw = false;
            signal(SIGWINCH, handlesig);
            continue;
        }
        ch = getk();
        if (ch) {
            switch(ch) {
            default:            fred.insert((char)ch);
            cb(key::up):        fred.rst(); fred.mvu();             fred.sav();
            cb(key::down):      fred.rst(); fred.mvd();             fred.sav();
            cb(key::left):      fred.mvl();                         fred.sav();
            cb(key::right):     fred.mvr();                         fred.sav();
            cb(key::shup):      fred.mvu(fred.tstep);                fred.sav();
            cb(key::shdown):    fred.mvd(fred.tstep);                fred.sav();
            cb(key::shright):   fred.mvr(fred.tstep);                fred.sav();
            cb(key::shleft):    fred.mvl(fred.tstep);                fred.sav();
            cb(key::pgup):      fred.x = 0; fred.mvu(fred.pgstep);  fred.sav();
            cb(key::pgdown):    fred.x = 0; fred.mvd(fred.pgstep);  fred.sav();
            cb(key::back):      fred.del();
            cb(key::nl):;       fred.nl();                          fred.sav();
            cb(key::del):       fred.rm();
            cb('$'):            fred.refresh();
            cb('@'):            fred.handlecmd();
            cb(key::tab):       r4(fred.insert('\t'));              fred.sav();
            }
        }
    } while (ch != key::f1);
    setattrs(false);
}

int main() {
    start();
}
