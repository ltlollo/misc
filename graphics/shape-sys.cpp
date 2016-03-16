// gpp self $(pkg-config --libs sfml-all)
// ./shape-sys

#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <algorithm>
#include <cmath>
#include <iostream>
#include <iterator>
#include <unordered_map>
#include <vector>

constexpr float off{0.f};
constexpr unsigned wh{1000}, ww{1000};

using Vertex = sf::Vector2f;
using Shape = std::vector<Vertex>;
using Shapes = std::vector<Shape>;

auto mid(const Vertex &f, const Vertex &s) { return (s - f) / 2.f; }
auto divvec(const Vertex &f, const Vertex &s, float p = 1.f, float n = 2.f) {
    return (s - f) * (p / n) + f;
}
auto join(const auto &seq, auto &&ele, auto p) {
    using Res = decltype(p(std::cbegin(seq)));
    Res res;
    if (seq.empty()) {
        return res;
    }
    auto ei = std::cbegin(seq);
    for (size_t i = 0; i < std::size(seq) - 1; ++i, ++ei) {
        auto te = p(ei);
        std::copy(std::cbegin(te), std::cend(te), std::back_inserter(res));
        std::copy(std::cbegin(ele), std::cend(ele), std::back_inserter(res));
    }
    auto te = p(ei);
    std::copy(std::cbegin(te), std::cend(te), std::back_inserter(res));
    return res;
}
auto join(const auto &seq, auto &&ele) {
    return join(seq, ele, [](auto it) { return *it; });
}
void draw_shape(sf::RenderWindow &win, const Shape &shape) {
    sf::Vertex arr[2];
    for (unsigned i = 0; i < shape.size() - 1; ++i) {
        arr[0].position = shape[i];
        arr[1].position = shape[i + 1];
        win.draw(arr, 2, sf::Lines);
    }
    if (shape.size() > 2) {
        arr[0].position = shape[shape.size() - 1];
        arr[1].position = shape[0];
        win.draw(arr, 2, sf::Lines);
    }
}
void draw_shapes(sf::RenderWindow &win, const Shapes &shapes) {
    for (const auto &shape : shapes) {
        draw_shape(win, shape);
    }
}

auto calc_center(const Shape &shape) {
    Vertex c{0.f, 0.f};
    for (const auto &it : shape) {
        c += it;
    }
    return c /= float(shape.size());
}

bool is_vertex(char it) { return (it >= 'A' && it <= 'Z'); }

bool is_mid(char it) { return (it >= 'a' && it <= 'z'); }

auto remove_spaces(const std::string &str) {
    auto res = str;
    res.erase(std::remove_if(begin(res), end(res),
                             [](const auto &it) { return it == ' '; }),
              end(res));
    return res;
}

struct Rule {
    bool identity{false}, opt_noadjmids{true}, opt_nocenter{true};
    size_t type{0}, self_cycle;
    std::string lhs;
    std::vector<std::string> vrhs;
    std::unordered_map<char, Vertex> vmap;
    Rule() : identity{true} {
        /* The identity rule Rule().apply(shape) is \shape -> [shape]
         * used by Grammar::map<n-of-gons, Rule> when n-gon is not in the map
         * this means that if there's no parser associated to the shape
         * shape is retuned in the form of [shape] (by apply).
         */
    }
    Rule(const std::string &rule_copy) {
        std::string rule = remove_spaces(rule_copy);
        auto it = std::find(begin(rule), end(rule), '>');
        if (it > end(rule) - 1) {
            throw std::runtime_error("No sparator\nHint: lhs>[rhs]");
        }
        std::string rhs = {it + 1, end(rule)};
        lhs = {begin(rule), it};
        type = std::count_if(begin(lhs), end(lhs), is_vertex);
        for (const auto &it : rhs) {
            if (std::none_of(begin(lhs), end(lhs), [&](const auto &s) {
                    return (s == it) || it == ',' || it == '.';
                })) {
                throw std::runtime_error("Unknown symbol: " + std::string{it});
            }
            if (it == '.') {
                opt_nocenter = false;
            }
        }
        auto adj_mids =
            std::adjacent_find(begin(lhs), end(lhs), [](char f, char s) {
                return is_mid(f) && is_mid(s);
            });
        if (adj_mids != end(lhs)) {
            this->opt_noadjmids = false;
        }
        if (type < 2 && lhs.size() != type) {
            throw std::runtime_error("Points cannot be devided");
        }
        if (!type && !opt_nocenter) {
            throw std::runtime_error("Center can't be calculated");
        }
        std::string curr;
        for (auto it = begin(rhs); it != end(rhs); ++it) {
            if (*it == ',') {
                vrhs.push_back(move(curr));
                curr = {};
            } else {
                curr.push_back(*it);
            }
        }
        vrhs.push_back(move(curr));
        vrhs.erase(std::remove_if(begin(vrhs), end(vrhs),
                                  [](const auto &it) { return it.empty(); }),
                   end(vrhs));
        self_cycle = std::distance(
            std::find_if(std::begin(vrhs), std::end(vrhs),
                         [&](const auto &s) {
                             return s.size() == type &&
                                    std::all_of(std::begin(s), std::end(s),
                                                is_vertex);
                         }),
            std::begin(vrhs));
        if (!lhs.empty()) {
            if (!is_vertex(lhs[0])) {
                throw std::runtime_error("Must start with a vertex");
            }
            // this is a microopt, see(*)
            lhs.reserve(lhs.size() + 1);
            lhs.push_back(lhs[0]);
        }
    }
    Shapes apply(sf::RenderWindow &win, const Shape &shape);
    void calc_mids();

    auto to_string() const {
        using str = char[1];
        return lhs + '>' + join(vrhs, str{','});
    }
};

void Rule::calc_mids() {
    if (opt_noadjmids) {
        // optimized mid point calculation, if there's only one
        // it's halfway between the adjacent vertices
        for (auto it = begin(lhs); it != end(lhs); ++it) {
            if (is_mid(*it)) {
                vmap[*it] = mid(vmap[*(it - 1)], vmap[*(it + 1)]);
            }
        }
    } else {
        for (auto it = begin(lhs); it != end(lhs); ++it) {
            auto it_mb = it;
            while (is_mid(*it)) {
                ++it;
            }
            size_t n_mids = std::distance(it_mb, it);
            for (size_t i = 0; i < n_mids; ++i) {
                vmap[*(it_mb + i)] = divvec(vmap[*(it_mb - 1)], vmap[*(it)],
                                            float(i + 1), float(n_mids + 1));
            }
        }
    }
}

Shapes Rule::apply(sf::RenderWindow &win, const Shape &shape) {
    if (identity) {
        return {shape};
    }
    auto res = Shapes{};
    if (vrhs.empty()) {
        return res;
    }
    res.reserve(vrhs.size());
    unsigned i = 0;
    for (const auto &it : shape) {
        while (!is_vertex(lhs[i])) {
            ++i;
        }
        vmap[lhs[i]] = it;
        ++i;
    }
    calc_mids();
    if (!opt_nocenter) {
        vmap['.'] = calc_center(shape);
    }
    for (size_t i = 0; i < vrhs.size(); ++i) {
        auto curr_shape = Shape{};
        curr_shape.reserve(vrhs[i].size());
        for (const auto &sv : vrhs[i]) {
            curr_shape.push_back(vmap[sv]);
        }
        if (i != self_cycle) {
            res.push_back(move(curr_shape));
        } else {
            draw_shape(win, curr_shape);
        }
    }
    return res;
}

struct Grammar {
    std::unordered_map<size_t, Rule> pmap;
    Grammar(const std::vector<Rule> &rules) {
        for (auto it_f = begin(rules); it_f < end(rules) - 1; ++it_f) {
            for (auto it_s = it_f + 1; it_s != end(rules); ++it_s) {
                if (it_f->type == it_s->type) {
                    throw std::runtime_error("Non unique rule: " + it_f->lhs +
                                             " " + it_s->lhs);
                }
            }
        }
        for (const auto &it : rules) {
            pmap[it.type] = it;
        }
    }
    Shapes next(sf::RenderWindow &win, const Shape &shape) {
        return pmap[shape.size()].apply(win, shape);
    }
    Shapes iterate(sf::RenderWindow &win, const Shapes &state) {
        auto shapes = Shapes{};
        auto curr = Shapes{};
        for (const auto &it : state) {
            curr = this->next(win, it);
            shapes.insert(end(shapes), std::make_move_iterator(begin(curr)),
                          std::make_move_iterator(end(curr)));
        }
        return shapes;
    }
    Shapes iterate(sf::RenderWindow &win, const Shapes &state,
                   unsigned depth) {
        auto res = state;
        for (unsigned i = 0; i < depth; ++i) {
            res = move(this->iterate(win, res));
        }
        return res;
    }
    auto to_string() const {
        using str = char[1];
        return join(pmap, str{';'},
                    [](auto r) { return (r->second.to_string()); });
    }
};

/* grammar explanation
 * def: RULE := LHS '>' RHS
 *      LHS := [A-Z][:alpha:]
 *      RHS := "" | [:alpha:] | "." | RHS ',' RHS
 * ex: AbCdEf>ACE,bdf
 *      - AbCdEf>_ instructs the parser to match an ACE shaped plygon,
 *        introducing b,d,f points between it's vertices
 *      - _>aBc instucts the parser to form a new aBc polygon with using the
 *        vertices introduced in LHS
 *      - Old vertices must be uppercase, new ones lowercase.
 *      - The LHS definition wraps arownd, therfore in "ABCd", d is considered
 *        between A and C (*)
 *      - '.' introduces the center of the polygon
 * def: RULES := RULE | RULE, RULES
 *      - rules LHS must match unique polygons
 *        ( ex: "ABC>", "AdBC>" is not allowed )
 */

int main(int, char *[]) {
    sf::VideoMode vmode{ww, wh};
    sf::RenderWindow window(vmode, "", sf::Style::Titlebar);
    window.clear();
    window.display();

    sf::Event event;
    auto g = Grammar{{{{"AabBcdCefDgh>ABCD,hABcc,AafDD,bBCee,dCDgg"}},
                      {{"ABabCDEcd>ABad,cbCE,dabc"}}}};
    auto gc = Grammar{{{{"ABCDE>"}}}};
    auto first = Shapes{{{0.f + off, 0.f + off},
                         {0.f + off, wh - off},
                         {ww - off, wh - off},
                         {ww - off, 0.f + off}}};

    {
        auto shapes = g.iterate(window, first, 12);
        draw_shapes(window, shapes);
    }
    while (window.isOpen()) {
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            } else if (event.type == sf::Event::KeyPressed)
                switch (event.key.code) {
                case sf::Keyboard::Q:
                case sf::Keyboard::Escape:
                    window.close();
                    break;
                case sf::Keyboard::S:
                    window.capture().saveToFile(g.to_string() + ".png");
                    break;
                default:
                    break;
                }
        }
        window.display();
    }
    return 0;
}
