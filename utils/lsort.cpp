// gpp lsort

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <iterator>

using namespace std;

auto non_lazy() {
    vector<string> vd{};
    string s;
    cin >> noskipws;
    while (getline(cin, s)) {
        vd.emplace_back(move(s));
    }
    sort(begin(vd), end(vd), [](const auto& f, const auto& s) noexcept {
        return f.size() > s.size();
    });
    for (const auto& it: vd) {
        cout << it << '\n';
    }
}

int main(int argc, char *argv[]) {
    cin.sync_with_stdio(false);
    non_lazy();
    return 0;
}

