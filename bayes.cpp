#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <sstream>   // std::istringstream
#include <cmath>     // std::abs
#include <algorithm> // std::for_each

namespace stat {

constexpr double laplace_eps{0.01}, delta_influence{0.1};

static_assert(laplace_eps > 0 && laplace_eps < 1, "invalid");
static_assert(delta_influence > 0 && delta_influence < 1, "invalid");

using Count = std::size_t;
using Pred = std::string;
using Text = std::map<Pred, Count>;

struct Counts {
    Count goods, bads;
    void operator+=(const Counts& rhs) noexcept {
        goods += rhs.goods;
        bads += rhs.bads;
    }
};

struct Data {
    Pred pred;
    Counts counts;
};

struct Good {};
struct Bad {};

static inline double pgoods(Count goods, Count bads) noexcept {
    return goods/(goods + bads);
}

static inline double pbads(Count goods, Count bads) noexcept {
    return 1.0 - pgoods(goods, bads);
}

Text parse(const std::string& orig) {
    Text text;
    if (orig.empty()) {
        return text;
    }
    std::istringstream iss(orig);
    Pred pred;
    while (iss >> pred) {
        if (text.find(pred) == text.end()) {
            text[pred] = 1;
        } else {
            text[pred]++;
        }
    }
    return text;
}

class Bayes {
    std::vector<Data> stats;
    Count goods{0}, bads{0};
    double influence(const Counts& counts) const noexcept {
        return std::abs(pgoods(counts.goods, counts.bads) - pgoods(goods, bads));
    }
    bool influencing(const Counts& counts) const noexcept {
        return influence(counts) > delta_influence;
    }
    double psmoothed(double w, double c) const noexcept {
        return (w + laplace_eps)/(c + 2*laplace_eps);
    }
    double pcond(const Text& text, Good) const noexcept {
        double prob {1.};
        for (const auto& stat : stats) {
            if (!influencing(stat.counts)) {
                return prob;
            }
            if (text.find(stat.pred) != text.end()) {
                prob *= (1. - psmoothed(stat.counts.goods, goods));
            } else {
                prob *= psmoothed(stat.counts.goods, goods);
            }
        }
        return prob;
    }
    double pcond(const Text& text, Bad) const noexcept {
        double prob {1.};
        std::for_each(std::begin(stats), std::end(stats),
                      [&](const Data& stat) {
            if (text.find(stat.pred) != text.end()) {
                prob *= (1. - psmoothed(stat.counts.bads, bads));
            } else {
                prob *= psmoothed(stat.counts.bads, bads);
            }
        });
        return prob;
    }
public:
    double pcond(Good, const Text& text) const noexcept {
        double pg{pcond(text, Good())}, pb{pcond(text, Bad())},
        pgs{pgoods(goods, bads)};
        return pg*pgs/(pg*pgs + pb*(1 - pgs));
    }
    double pcond(Bad, const Text& text) const noexcept {
        return 1. - pcond(Good(), text);
    }
    void train(Good, const Text& text) {
        if (text.empty()) {
            return;
        }
        for (const auto& it: text) {
            auto pos = std::find_if(std::begin(stats), std::end(stats),
                                    [&](const Data& data) {
                return data.pred == it.first; });
            if (pos != std::end(stats)) {
                pos->counts += Counts{it.second, 0};
            } else {
                stats.push_back(Data{it.first, Counts{it.second, 0}});
            }
            goods += it.second;
        }
        std::sort(std::begin(stats), std::end(stats),
                  [&](const Data& fst, const Data& snd){
            return influence(fst.counts) > influence(snd.counts);
        });
    }
    void train(Bad, const Text& text) {
        if (text.empty()) {
            return;
        }
        for (const auto& it: text) {
            auto pos = std::find_if(std::begin(stats), std::end(stats),
                                    [&](const Data& data) {
                return data.pred == it.first; });
            if (pos != std::end(stats)) {
                pos->counts += Counts{0, it.second};
            } else {
                stats.push_back(Data{it.first, Counts{0, it.second}});
            }
            bads += it.second;
        }
        std::sort(std::begin(stats), std::end(stats),
                  [&](const Data& fst, const Data& snd){
            return influence(fst.counts) > influence(snd.counts);
        });
    }
};

}

int main(int, char **) {
    return 0;
}
