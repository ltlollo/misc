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
    if (!top) { out += ", "; }
    out += "{\"" + string(it, sep) + "\"";
    if (++sep == end) {
        cerr << "[E]: no cannot parse number after \\ in\n\t" << *line << endl;
        exit(EXIT_FAILURE);
    }
    it = sep;
    while (sep != end && *sep >= '0' && *sep <= '9') { ++sep; }
    string num = string(it, sep);
    if (num.empty()) {
        cerr << "[E]: no cannot parse number after \\ in\n\t" << *line << endl;
        exit(EXIT_FAILURE);
    }
    if (stoul(num)) { out += ", " + move(num); }
    out += "}";
    it = sep;
    if ((sep = find(it, end, '\\')) != end) {
        rparse_next_vec(line, out, it, sep, end, false);
    }
}
template<typename L, typename T, typename U>
void parse_vecnum_rest(const L& line, string& out, T& it, const U& end) {
    auto it_cp = it;
    out += "{";
    if ((it = find(it, end, '\\')) == end) {
        out += "{\"" + string(it_cp, it) + "\"}";
    } else {
        rparse_next_vec(line, out, it_cp, it, end);
    }
    out += "}";
    auto rest = string(it, end);
    if (!rest.empty()) {
        out += ", \"" + move(rest) + "\"";
    }
}

int main(int argc, char *argv[]) {
    auto print_help = [&]() {
        cerr << "Usage:\nScope\n" << endl;
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

