#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

using usize = size_t;

struct Data {
    string str;
    bool found;
    vector<usize> ss;
};

template<typename T>
struct range {
    T fst, end;
};

using DataVec = vector<Data>;
using Range = range<usize>;
using RangeVec = vector<Range>;

RangeVec len_map(const DataVec& vd) {
    if (!vd.size()) return vector<Range>{};

    usize len = vd[0].str.size();
    auto res = vector<Range>(len+1, Range{0,0});
    usize pos = 0;

    for (usize i = 0; i < vd.size(); ++i) {
        if (vd[i].str.size() == len) continue;
        res[len] = Range{pos, i};
        pos = i;
        len = vd[i].str.size();
    }
    res[len] = Range{pos, vd.size()};
    return res;
}


auto ccont(Data& p, const DataVec& vd, RangeVec& lmap, usize rpos) {
    for (usize j = rpos; j > 1; --j) {
        for (usize i = lmap[j].fst; i < lmap[j].end; ++i) {
            if (p.str.find(vd[i].str) != string::npos) {
                p.ss.push_back(i);
            }
        }
    }
}

constexpr usize opt[] = {6};
constexpr usize delta = 500;

auto op(DataVec& vd, RangeVec& lmap) {
    if (lmap.size() < 2) return;
    // ineff : A in B, B in C => A in C
    auto l = lmap.size()-1;
    for (size_t j = l; j > 1; --j) {
        if (find(begin(opt), end(opt), j)!= end(opt) ||
            lmap[j].end - lmap[j].fst < delta)
            for (auto i = lmap[j].fst; i < lmap[j].end; ++i) {
                ccont(vd[i], vd, lmap, j-1);
            }
        cout << j << endl;
    }
    cout << "ready\n";
}

auto get_graph(usize node, DataVec& vd) {
    auto res = vector<usize>{vd[node].ss};
    if (res.empty()) return res;

    for (const auto& it : vd[node].ss) {
        vd[it].found = true;
        auto nodes = get_graph(it, vd);
        res.insert(end(res), begin(nodes), end(nodes));
    }
    return res;
}

auto fss(const string& s, DataVec& vd, const RangeVec& lmap) {
    auto res = vector<usize>{};
    if (s.size() < 2 || vd.empty()) return res;
    auto pos = s.size() > vd[0].str.size() ? 0 : lmap[s.size()-1].fst;
    for (usize i = pos; i < vd.size(); ++i) {
        vd[i].found = false;
    }
    for (auto i = pos; i < vd.size(); ++i) {
        if (!vd[i].found) {
            if (s.find(vd[i].str) != string::npos) {
                res.push_back(i);
                for (auto& it: vd[i].ss) {
                    if (!vd[it].found) {
                        vd[it].found = true;
                        res.push_back(it);
                    }
                }
            }
        }
    }
    return res;
}


int main(int argc, char *argv[]) {
    auto help = [&](){
        cout << "USAGE: " << argv[0] << " file\n"
            "SCOPE: https://www.reddit.com/r/dailyprogrammer/comments"
            "/2nihz6/20141126_challenge_190_intermediate_words_inside/\n";
    };
    if (argc != 2) {
        help();
        return 1;
    }
    vector<Data> vd;
    ifstream in(argv[1]);
    string line;
    while (in >> line) {
        if (line.empty()) continue;
        if(*line.rbegin() == '\r') {
            line.erase(line.length()-1, 1);
        }
        vd.emplace_back(Data{move(line), false, {}});
    }
    sort(begin(vd), end(vd), [](const Data& f, const Data& s) {
            return f.str.size() > s.str.size();
    });
    vector<Range> lmap = len_map(vd);
    op(vd, lmap);
    string uin;
    for(;;) {
        getline(cin, uin);
        cout << "lf: " << uin << endl;
        auto res = fss(uin, vd, lmap);
        for (const auto& it : res) {
            cout << vd[it].str << " ";
        }
        cout << endl;
    }
    return 0;
}
