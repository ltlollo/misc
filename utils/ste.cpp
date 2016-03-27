#include <algorithm>
#include <atomic>
#include <err.h>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <signal.h>
#include <stdexcept>
#include <stdio.h>
#include <string>
#include <termio.h>
#include <thread>
#include <unistd.h>
#include <unordered_map>
#include <vector>

using Text = std::vector<std::vector<char>>;

struct Session {
    std::string fname;
    Text text;
    unsigned y, x;
    unsigned pagey, pagex;
};

static int in = STDIN_FILENO;
static int out = STDOUT_FILENO;
std::atomic<bool> need_redraw;

static inline void clear() { printf("\033[2J"); }
static inline void clearline() { printf("\033[2K"); }
static inline void clearr() { printf("\033[0K"); }
static inline void handlesig(int) {
    signal(SIGWINCH, SIG_IGN);
    need_redraw = true;
}
static inline void rstscr() {
    clear();
    printf("\033[1;1H");
}

static void setattrs(const bool rstcurs) {
    static termios old, curr;
    if (!rstcurs) {
        rstscr();
        tcsetattr(in, TCSANOW, &old);
        return;
    }
    tcgetattr(in, &old);
    curr = old;
    curr.c_cc[VTIME] = 0;
    curr.c_cc[VMIN] = 1;
    curr.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    curr.c_cflag |= (CS8);
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

enum key : int {
    tab = '\t',
    nl = '\n',
    del = 126,
    back,
    up = -999,
    down,
    right,
    left,
    home,
    ins,
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
        if (k == 13) {
            return nl;
        }
        return k;
    }
    if ((k = readch(in)) == 79) {
        if ((k = readch(in)) >= 80 && k <= 83) {
            return f1 + (k - 80);
        }
        return 0;
    } else if (k == '[') {
        if ((k = readch(in)) >= 'A' && k <= 'D') {
            return up + (k - 'A');
        } else if (k >= '1' && k <= '6' && k != '3') {
            if ((x = readch(in)) == 126) {
                return home + (k - '1');
            } else if (x == ';') {
                if (k == '1' && (x = readch(in)) == '2' &&
                    (k = readch(in)) >= 'A' && k <= 'D') {
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

struct CurrText {
    std::string name;
    std::string fname;
    Text &text;
    unsigned y = 0;
    unsigned x = 0;
    unsigned pagey = 0;
    unsigned pagex = 0;
    CurrText(std::string n, Session &s)
        : name{n}, fname{s.fname}, text{s.text}, y{s.y}, x{s.x} {}
};
struct Ed {
    using Map = std::unordered_map<std::string, Session>;
    unsigned cols, rows, pgstep, tstep;
    unsigned ry = 0, rx = 0;
    unsigned winofy = 0, winofx = 0;
    bool restore = true;
    Map workspace;
    CurrText curr;
    Ed() : curr{"", workspace[""]} {}
    Ed(unsigned cols, unsigned rows, unsigned winofy, unsigned winofx)
        : cols{cols}, rows{rows}, winofy{winofx}, winofx{winofx},
          curr{"", workspace[""]} {}
};
static inline void move(const Ed &ed, unsigned y, unsigned x) {
    printf("\033[%d;%dH", ed.winofy + y + 1, ed.winofx + x + 1);
}

void init(Ed &ed) {
    winsize w;
    ioctl(out, TIOCGWINSZ, &w);
    ed.cols = w.ws_col;
    ed.rows = w.ws_row - 1;
    ed.pgstep = ed.cols / 2;
    ed.tstep = w.ws_col / 4 > 4 ? 4 : w.ws_col / 4;
}

void rstcurs(Ed &ed) {
    if (ed.restore) {
        ed.curr.y = ed.ry;
        ed.curr.x = ed.rx;
    }
}
void savecurs(Ed &ed) {
    if (ed.restore) {
        ed.ry = ed.curr.y;
        ed.rx = ed.curr.x;
    }
}
unsigned absx(const Ed &ed) { return ed.curr.pagex + ed.curr.x; }
unsigned absy(const Ed &ed) { return ed.curr.pagey + ed.curr.y; }
void refreshline(const Ed &ed, unsigned py, unsigned px, unsigned y,
                 unsigned x = 0) {
    move(ed, y, x);
    auto i = ed.curr.text.begin() + py + y;
    if (i >= ed.curr.text.end() || i->empty()) {
        clearr();
        return;
    }
    for (auto j = i->begin() + px + x; j < i->begin() + px + ed.cols; ++j) {
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
void refresh(const Ed &ed, unsigned py, unsigned px, unsigned y = 0) {
    move(ed, y, 0);
    for (unsigned i = 0; i < ed.rows; ++i) {
        refreshline(ed, py, px, i);
    }
}
void refresh(const Ed &ed) {
    refresh(ed, ed.curr.pagey, ed.curr.pagex);
    move(ed, ed.curr.y, ed.curr.x);
}
void rmch(Ed &ed) {
    if (ed.curr.text.size() <= absy(ed)) {
        return;
    }
    auto l = ed.curr.text.begin() + absy(ed);
    if (std::all_of(l->begin(), l->end(), [](auto &c) { return c == 0; })) {
        ed.curr.text.erase(l);
        refresh(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    if (l->size() <= absx(ed)) {
        return;
    }
    l->erase(l->begin() + absx(ed));
    refreshline(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y, ed.curr.x);
    move(ed, ed.curr.y, ed.curr.x);
}
void mvleft(Ed &ed, unsigned step = 1) {
    if (absx(ed) < step) {
        move(ed, ed.curr.y, ed.curr.x = 0);
        return;
    }
    if (ed.curr.x < step && ed.curr.pagex) {
        refresh(ed, ed.curr.pagey, ed.curr.pagex -= ed.cols);
        move(ed, ed.curr.y, ed.curr.x = ed.cols - step + ed.curr.x);
        savecurs(ed);
        return;
    }
    move(ed, ed.curr.y, ed.curr.x -= step);
}
void mvright(Ed &ed, unsigned step = 1) {
    if (ed.cols - 1 < ed.curr.x + step) {
        refresh(ed, ed.curr.pagey, ed.curr.pagex += ed.cols);
        move(ed, ed.curr.y, ed.curr.x = ed.curr.x + step - ed.cols);
        savecurs(ed);
        return;
    }
    move(ed, ed.curr.y, ed.curr.x += step);
}
void mvdown(Ed &ed, unsigned step = 1) {
    if (ed.curr.y + step > ed.rows - 1) {
        refresh(ed, ed.curr.pagey += step, ed.curr.pagex);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    move(ed, ed.curr.y += step, ed.curr.x);
}
void mvup(Ed &ed, unsigned step = 1) {
    if (absy(ed) < step) {
        ed.curr.text.insert(ed.curr.text.begin(), step, std::vector<char>());
        refresh(ed, ed.curr.pagey, ed.curr.pagex);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    if (ed.curr.y < step) {
        refresh(ed, ed.curr.pagey -= step, ed.curr.pagex);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    move(ed, ed.curr.y -= step, ed.curr.x);
}
void nocnl(Ed &ed) {
    if (ed.curr.text.size() <= absy(ed) + 1) {
        ed.curr.text.resize(absy(ed) + 1);
    }
    ed.curr.text.insert(ed.curr.text.begin() + absy(ed) + 1,
                        std::vector<char>());
    refresh(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y);
    ed.curr.x = 0;
    mvdown(ed);
}
void delch(Ed &ed) {
    if (ed.curr.text.size() <= absy(ed)) {
        return;
    }
    auto l = ed.curr.text.begin() + absy(ed);
    if (std::all_of(l->begin(), l->end(), [](auto &c) { return c == 0; })) {
        ed.curr.text.erase(l);
        refresh(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    if (absx(ed) == 0) {
        return;
    }
    if (l->size() <= absx(ed) - 1) {
        mvleft(ed);
        return;
    }
    auto it = l->begin() + absx(ed) - 1;
    if (*it == '\t') {
        auto rit = std::make_reverse_iterator(it + 1);
        auto re =
            std::find_if(rit, l->rend(), [](auto &c) { return c != '\t'; });
        unsigned d = (unsigned)std::distance(rit, re);
        l->erase(re.base(), rit.base());
        refreshline(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y);
        mvleft(ed, d);
        return;
    }
    l->erase(it);
    refreshline(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y, ed.curr.x - 1);
    mvleft(ed);
}
void showmsg(const Ed &ed, const char *msg) {
    move(ed, ed.cols - 1, 0);
    clearline();
    printf("%s", msg);
    move(ed, ed.curr.y, ed.curr.x);
}
void insert(Ed &ed, char c, unsigned times = 1) {
    if (ed.curr.text.size() <= absy(ed)) {
        ed.curr.text.resize(absy(ed) + 1);
    }
    auto &l = ed.curr.text[absy(ed)];
    if (l.size() <= absx(ed)) {
        l.resize(absx(ed));
    }
    l.insert(l.begin() + absx(ed), times, c);
    refreshline(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y, ed.curr.x);
    mvright(ed, times);
}
void nobreaknl(Ed &ed) {
    if (ed.curr.text.size() <= absy(ed) + 1) {
        ed.curr.text.resize(absy(ed) + 1);
    }
    auto v = std::vector<char>();
    if (absx(ed) < ed.curr.text[absy(ed)].size()) {
        auto &l = ed.curr.text[absy(ed)];
        auto b = std::make_move_iterator(l.begin() + absx(ed));
        auto e = std::make_move_iterator(l.end());
        v.resize(absx(ed));
        v.insert(v.end(), b, e);
        ed.curr.text[absy(ed)].resize(absx(ed));
    }
    ed.curr.text.insert(ed.curr.text.begin() + absy(ed) + 1, std::move(v));
    refresh(ed, ed.curr.pagey, ed.curr.pagex, ed.curr.y);
    rstcurs(ed);
    mvdown(ed);
}
void stream(Ed &ed, FILE *p) {
    auto restore = ed.restore;
    ed.restore = false;
    int c;
    while ((c = fgetc(p)) != EOF) {
        if (std::isprint(c)) {
            insert(ed, (char)c);
            continue;
        }
        switch (c) {
        default:
            insert(ed, ' ');
            break;
        case (key::nl):
            nocnl(ed);
            break;
        case (key::tab):
            insert(ed, '\t', 4);
        }
    }
    ed.restore = restore;
    savecurs(ed);
}
bool isemptycurr(const Ed &ed) {
    if (ed.curr.text.begin() + absy(ed) >= ed.curr.text.end()) {
        return true;
    }
    auto l = ed.curr.text.begin() + absy(ed);
    if (std::all_of(l->begin(), l->end(), [](auto &c) { return c == 0; })) {
        return true;
    }
    if (absx(ed) == 0) {
        return true;
    }
    return false;
}

void exec(Ed &ed, const unsigned y, unsigned x) {
    static std::vector<char> cmd;
    const auto &text = ed.curr.text;
    if (y >= text.size()) {
        return;
    }
    if (x >= text[y].size()) {
        return;
    }
    auto restore = ed.restore;
    ed.restore = false;
    cmd.resize(0);
    for (const auto &c : text[y]) {
        if (c == 0) {
            cmd.push_back(' ');
        } else {
            cmd.push_back(c);
        }
    }
    cmd.push_back(0);
    showmsg(ed, cmd.data());
    auto p = popen(cmd.data(), "r");
    if (p != nullptr) {
        nocnl(ed);
        stream(ed, p);
        fclose(p);
    }
    ed.restore = restore;
    savecurs(ed);
}
void exec(Ed &ed) {
    rstcurs(ed);
    return exec(ed, absy(ed), absx(ed));
}
void mvupbound(Ed &ed, unsigned step = 1) {
    if (absy(ed) < step) {
        return;
    }
    if (ed.curr.y < step) {
        refresh(ed, ed.curr.pagey -= step, ed.curr.pagex);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    move(ed, ed.curr.y -= step, ed.curr.x);
}
void mvdownbound(Ed &ed, unsigned step = 1) {
    if (absy(ed) + step < ed.curr.text.size()) {
        refresh(ed, ed.curr.pagey += step, ed.curr.pagex);
        move(ed, ed.curr.y, ed.curr.x);
        return;
    }
    if (ed.curr.y + step > ed.rows - 1) {
        return;
    }
    move(ed, ed.curr.y += step, ed.curr.x);
}
void fillstr(const Text &text, std::vector<char> &str) {
    if (text.size() < 2) {
        str.push_back(0);
        return;
    }
    for (const auto &ch : text[text.size() - 2]) {
        if (std::isprint(ch)) {
            str.push_back(ch);
        } else {
            str.push_back(' ');
        }
    }
    str.push_back(0);
}
void savesession(Ed &ed) {
    Session &s = ed.workspace[ed.curr.name];
    s.text = ed.curr.text;
    s.y = ed.curr.y;
    s.x = ed.curr.x;
    s.pagey = ed.curr.pagey;
    s.pagex = ed.curr.pagex;
    s.fname = ed.curr.fname;
}
void opensession(Ed &ed, const std::string &name) {
    savesession(ed);
    Session &s = ed.workspace[name];
    ed.curr.text = s.text;
    ed.curr.y = s.y;
    ed.curr.x = s.x;
    ed.curr.pagey = s.pagey;
    ed.curr.pagex = s.pagex;
    ed.curr.fname = s.fname;
    ed.curr.name = name;
    refresh(ed);
    savecurs(ed);
}
void handlecmd(Ed &ed) {
    static Ed line;
    static std::vector<char> cmd;
    char str[121];
    line.restore = false;
    line.cols = ed.cols;
    line.rows = 1;
    line.winofy = ed.cols - 2;
    line.winofx = 0;
    line.cols = ed.cols;
    int ch;
    move(ed, line.winofy, line.winofx);
    auto err = [&] {
        while ((ch = getk())) {
            switch (ch) {
            default:
                insert(line, (char)ch);
                break;
            case (key::up):
                mvupbound(line);
                break;
            case (key::down):
                mvdownbound(line);
                break;
            case (key::left):
                mvleft(line);
                break;
            case (key::right):
                mvright(line);
                break;
            case (key::back):
                delch(line);
                break;
            case (key::nl):
                if (!isemptycurr(line)) {
                    nocnl(line);
                    return 0;
                }
                return -1;
            case (key::del):
                rmch(line);
                break;
            case (key::tab):
                insert(line, '\t', 4);
            case (key::shright):
                mvright(line, 4);
                break;
            case (key::shleft):
                mvleft(line, 4);
                break;
            case (key::f1):
                return -1;
            }
        }
        return -1;
    }();
    if (err == 0) {
        cmd.resize(0);
        fillstr(line.curr.text, cmd);
        move(ed, 0, 0);
        if (sscanf(cmd.data(), "open %120s", str) == 1) {
            opensession(ed, str);
        }
    }
    move(ed, ed.curr.y, ed.curr.x);
}
void start() {
    setattrs(true);
    need_redraw = false;
    winsize w;
    ioctl(out, TIOCGWINSZ, &w);
    Ed ed;
    init(ed);
    rstscr();
    signal(SIGWINCH, handlesig);
    signal(SIGINT, SIG_IGN);
    int ch = 0;
    do {
        if (need_redraw) {
            init(ed);
            need_redraw = false;
            signal(SIGWINCH, handlesig);
            continue;
        }
        ch = getk();
        if (ch) {
            switch (ch) {
            default:
                insert(ed, (char)ch);
                break;
            case (key::up):
                rstcurs(ed);
                mvup(ed);
                savecurs(ed);
                break;
            case (key::down):
                rstcurs(ed);
                mvdown(ed);
                savecurs(ed);
                break;
            case (key::left):
                mvleft(ed);
                savecurs(ed);
                break;
            case (key::right):
                mvright(ed);
                savecurs(ed);
                break;
            case (key::shup):
                mvup(ed, ed.tstep);
                savecurs(ed);
                break;
            case (key::shdown):
                mvdown(ed, ed.tstep);
                savecurs(ed);
                break;
            case (key::shright):
                mvright(ed, ed.tstep);
                savecurs(ed);
                break;
            case (key::shleft):
                mvleft(ed, ed.tstep);
                savecurs(ed);
                break;
            case (key::pgup):
                ed.curr.x = 0;
                mvup(ed, ed.pgstep);
                savecurs(ed);
                break;
            case (key::pgdown):
                ed.curr.x = 0;
                mvdown(ed, ed.pgstep);
                savecurs(ed);
                break;
            case (key::back):
                delch(ed);
                break;
            case (key::nl):
                nobreaknl(ed);
                savecurs(ed);
                break;
            case (key::del):
                rmch(ed);
                break;
            case ('$'):
                exec(ed);
                break;
            case ('@'):
                handlecmd(ed);
                break;
            case (key::tab):
                insert(ed, '\t', 4);
                savecurs(ed);
            }
        }
    } while (ch != key::f1);
    setattrs(false);
}

int main() { start(); }
