// gpp self

#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <regex>
#include <unistd.h>

using namespace std;

bool needs_escape(const char c) noexcept {
    return c == '(' || c == ')' || c == ' ' || c == '\'' || c == '"'
        || c == '&';
}

auto escape(const string& s) {
    string res;
    res.reserve(s.size());
    for (auto it = s.begin(); it != s.end(); ++it) {
        if (needs_escape(*it)) {
            res += '\\';
        }
        res += *it;
    }
    return res;
}

auto escape(int argc, char* argv[], int start=1) {
    string res;
    if (start >= argc) return res;
    for (int i = start; i < argc; ++i) {
        for(char* it = argv[i]; *it != '\0'; ++it) {
            if (needs_escape(*it)) {
                res += '\\';
            }
            res += *it;
        }
        if (i < argc-1) res += ' ';
    }
    return res;
}

static auto operator""_r(const char* str, size_t) {
    return regex(str, regex::optimize);
}

struct StrNum{ string str{}; unsigned num{}; };
struct Rule{regex r; vector<StrNum> strnunms; string rest{}; };

Rule rules[] = {
    {"(.+\\.(?:cbz|pdf)$)"_r, {{"okular "}}},
    {"(.+\\.(?:cbr)$)"_r, {{"qcomicbook "}}},
    {"(.+\\.(?:3g2|3gp|avi|flv|mkv|mpg|wmv|mp4)$)"_r ,{{"vlc "}}},
    {"(.+\\.(?:m3u|mp3|ogg|wav)$)"_r, {{"vlc "}}},
    {"(.+\\.(?:h|cpp|c|cc|rs)):([0-9]+):([0-9]+):?$"_r ,
        {{"kate -l ", 1}, {" -c ", 2}, {" ", 0}}},
    {"(.+\\.(?:h|cpp|c|cc|rs)):([0-9]+):?$"_r, {{"kate -l ", 1}, {" ", 0}}},
    {"(.+\\.(?:h|cpp|c|cc|rs)):?$"_r, {{"kate ",0}}},
    {"(.+\\.(?:test|exp)$)"_r, {{"cat -> ", 0}}},
    {"(.+\\.(?:loop)$)"_r, {{"poppe ", 0}}, " -"},
    {"(.+(?:youtube).+)"_r, {{"youtube-dl -q ", 0}}, " &>/dev/null"},
    {"(www.*)"_r, {{"ping "}}},
};

void call(const string& str) {
    size_t i = 0;
    for (const auto& rule: rules) {
        smatch what;
        if (regex_match(str, what, rule.r) &&
            what.size() == rule.strnunms.size() + 1) {
            string cmd;
            for (const auto& strnum: rule.strnunms) {
                cmd += strnum.str;
                if (strnum.num+1 >= what.size()) {
                    cerr << "[W]: position outside range in rule number " << i;
                }
                cmd += what[strnum.num+1];
            }
            cmd += rule.rest;
            execl("/bin/sh", "/bin/sh", "-c", cmd.c_str(), nullptr);
        }
        ++i;
    }
}

int main(int argc, char *argv[]) {
    call(escape(argc, argv));
    return 1;
}

