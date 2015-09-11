// gpp self

#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>

using namespace std;

template<typename L, typename T, typename U>
void rparse_next_vec(const L& line, string& out, T& it, T& sep, const U& end,
                     bool top=true) {
    string str, num;
    if (!top) {
        out += ", ";
    }
    str = string(it, sep);
    out += "{\"" + move(str) + "\"";
    if (++sep == end) {
        cerr << "[E]: no cannot parse number after \\ in\n\t" << *line << endl;
        exit(EXIT_FAILURE);
    }
    it = sep;
    while (sep != end && *sep >= '0' && *sep <= '9') {
        ++sep;
    }
    num = string(it, sep);
    if (num.empty()) {
        cerr << "[E]: no cannot parse number after \\ in\n\t" << *line << endl;
        exit(EXIT_FAILURE);
    }
    if (stoul(num)) {
        out += ", " + move(num);
    }
    out += "}";
    it = sep;
    if ((sep = find(it, end, '\\')) != end) {
        rparse_next_vec(line, out, it, sep, end, false);
    }
}
template<typename L, typename T, typename U>
void parse_vecnum_rest(const L& line, string& out, T& it, const U& end) {
    string rx, rest;
    auto it_cp = it;
    out += "{";
    if ((it = find(it, end, '\\')) == end) {
        rx = string(it_cp, it);
        out += "{\"" + move(rx) + "\"}";
    } else {
        rparse_next_vec(line, out, it_cp, it, end);
    }
    out += "}";
    rest = string(it, end);
    if (!rest.empty()) {
        out += ", \"" + move(rest) + "\"";
    }
}

/*
 * Grammar:
 *  Separator Nl Rules
 * Separator:
 *  Ignore Chars Nl Ignore
 * Rules:
 *  (Ignore Rule Rules Inore) || Nil
 * Rule:
 *  Rx Spaces Sepa StrNum
 * Ignore: ("#" (Chars || Nil) Nl) || Nil
 * Nl: "\n"
 * Nil: ""
 */

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
        cerr << "Usage:\t" << argv[0] << " grammar"
            "\n\tgrammar<file>: file containing the regex"
            "\nScope:\tgenerates the Rules struct to be used by soppe"
             << endl;
    };
    if (argc-1 != 1) {
        print_help();
        return 1;
    }
    auto f = ifstream(argv[1]);
    vector<string> lines;
    for (string line; getline(f, line);) {
        if (line.size() && line[0] != '#') {
            lines.push_back(move(line));
        }
    }
    if (lines.empty()) {
        cerr << "[E]: must contain the separator" << endl;
        exit(EXIT_FAILURE);
    }
    string& sepa = lines[0];
    string out;
    out += "Rule rules[] = {\n";
    for (auto line = lines.begin()+1; line != lines.end(); ++line) {
        auto end = line->end();
        auto it = search(line->begin(), end, sepa.begin(), sepa.end());
        if (it == line->end() || it == line->begin()) {
            cerr << "[E]: line does not contain a separator\n\t"
                 << *line << endl;
            exit(EXIT_FAILURE);
        }
        auto it_cp = it;
        while (*(it_cp-1) == ' ') {
            --it_cp;
        }
        out += "    {\"" + string(line->begin(), it_cp) + "\"_r, ";
        it += sepa.size();
        while (it != line->end() && *it == ' ') {
            ++it;
        }
        parse_vecnum_rest(line, out, it, end);
        out += (line+1 != lines.end() ? "},\n" : "}\n");
    }
    out += "};\n";
    cout << out;
    return 0;
}

