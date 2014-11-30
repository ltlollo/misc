#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <array>
#include <extra/utils.h>
#include <work/workers.h>

using namespace std;

using usize = size_t;
struct Data {
    string str;
};
template<typename T> struct mrange {
    T fst, end;
};
using DataVec = vector<Data>;
using Range = mrange<usize>;
using RangeVec = vector<Range>;

RangeVec len_map(const DataVec& vd) {
    if (!vd.size()) { return vector<Range>{}; }
    usize len = vd[0].str.size();
    auto res = vector<Range>(len+1, Range{vd.size(), vd.size()});
    usize pos = 0;
    for (usize i = 0; i < vd.size(); ++i) {
        if (vd[i].str.size() == len) { continue; }
        res[len] = Range{pos, i};
        pos = i;
        len = vd[i].str.size();
    }
    res[len] = Range{pos, vd.size()};
    return res;
}

auto rnear(usize ss, const RangeVec& lmap, usize len) {
    for (usize i = ss-1; i > 1; --i) {
        if (lmap[i].fst != len) { return lmap[i].fst; }
    }
    return len;
}

auto fmatch(const string& s, const DataVec& vd, const RangeVec& lmap) {
    auto res = vector<usize>{};
    if (s.size() < 2 || vd.empty()) { return res; }
    auto pos = s.size() > vd[0].str.size() ? 0 : rnear(s.size(), lmap, vd.size());

    auto nth = [](const Data&, size_t i) noexcept { return i; };
    auto filter = [&, s, pos](const Data& cs, size_t i) noexcept -> bool {
        return  i >= pos && s.find(cs.str) != string::npos;
    };
    res = work::igen_work_balancer(vd, nth, filter);
    if (s.size() <= vd[0].str.size()) {
        for (auto i = lmap[s.size()].fst; i < lmap[s.size()].end; ++i) {
            if (s == vd[i].str) {
                res.push_back(i);
                break;
            }
        }
    }
    return res;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        cout << "USAGE: " << argv[0] << " wordlist\n"
            "SCOPE: i entirelly misread the description of "
            "https://www.reddit.com/r/dailyprogrammer/comments"
            "/2nihz6/20141126_challenge_190_intermediate_words_inside/\n"
            "so now this is a perf test case for mutation vs non mutation, spoiler: non mut wins";
            return 1;
    }
    vector<Data> vd;
    ifstream in(argv[1]);
    string line;
    while (in >> line) {
        if (line.empty()) {
            continue;
        }
        vd.emplace_back(Data{move(line)});
    }
    sort(begin(vd), end(vd), [](const Data& f, const Data& s) {
            return f.str.size() > s.str.size();
    });
    vector<Range> lmap = len_map(vd);
    for(string uin;;) {
        cout << "% ";
        cin >> uin;
        transform(begin(uin), end(uin), begin(uin), ::tolower);
        auto res = fun::rmeasure("bench", fmatch, uin, vd, lmap);
        cout << "looking for: " << uin << endl;
        usize count{0};
        for_each(begin(res), end(res), [&](const auto& it){
                cout << vd[it].str << " ";
                ++count;
        });
        cout << '(' << count << ')' << endl;
    }
    return 0;
}
