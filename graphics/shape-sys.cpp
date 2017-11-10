// gpp self $(pkg-config --libs sfml-all) -lboost_timer -lboost_system
// ./shape-sys

#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <algorithm>
#include <boost/container/flat_map.hpp>
#include <boost/container/small_vector.hpp>
#include <boost/timer/timer.hpp>
#include <iterator>
#include <iostream>
#include <unistd.h>

constexpr float off{0.f};
constexpr unsigned wh{1000}, ww{1000};
constexpr unsigned epilepsy_factor = 2;

template <typename T, typename U> using Map = boost::container::flat_map<T, U>;
template <typename T> using UVec = boost::container::small_vector<T, 8>;
using Vertex = sf::Vector2f;
using Shape = boost::container::vector<Vertex>;
using Shapes = boost::container::vector<Shape>;

auto mid(const Vertex &f, const Vertex &s) { return (s - f) / 2.f; }
auto div(const Vertex &f, const Vertex &s, float p = 1.f, float n = 2.f) {
    return (s - f) * (p / n) + f;
}
auto join_seq(auto &&s, auto &&ele, auto p) {
    auto seq = std::move(s);
    using Res = decltype(p(std::begin(seq)));
    Res res;
    if (std::size(seq) == 0) {
        return res;
    }
    auto ei = std::begin(seq);
    for (size_t i = 0; i < std::size(seq) - 1; ++i, ++ei) {
        auto te = p(ei);
        res.insert(std::end(res), std::make_move_iterator(std::begin(te)),
                   std::make_move_iterator(std::end(te)));
        std::copy(std::cbegin(ele), std::cend(ele), std::back_inserter(res));
    }
    auto te = p(ei);
    res.insert(std::end(res), std::make_move_iterator(std::begin(te)),
               std::make_move_iterator(std::end(te)));
    return res;
}
auto join_seq(auto &&seq, auto &&ele) {
    return join_seq(seq, ele, [](auto it) { return *it; });
}
auto join(auto &&s, auto &&ele, auto p) {
    auto seq = std::move(s);
    using Res = decltype(p(std::begin(seq)));
    Res res;
    if (std::size(seq) == 0) {
        return res;
    }
    auto ei = std::begin(seq);
    for (size_t i = 0; i < std::size(seq) - 1; ++i, ++ei) {
        auto te = p(ei);
        res.insert(std::end(res), std::make_move_iterator(std::begin(te)),
                   std::make_move_iterator(std::end(te)));
        res.push_back(ele);
    }
    auto te = p(ei);
    res.insert(std::end(res), std::make_move_iterator(std::begin(te)),
               std::make_move_iterator(std::end(te)));
    return res;
}
auto join(auto &&seq, auto &&ele) {
    return join(seq, ele, [](auto it) { return *it; });
}
auto split(auto &&v, auto &&at, auto p) {
    auto seq = std::move(v);
    using It = decltype(std::begin(seq));
    using Res = decltype(p(std::begin(seq), std::begin(seq)));
    std::vector<Res> res;
    It b, e = std::begin(seq);
    do {
        if ((b = e) == std::end(seq)) {
            break;
        }
        e = std::find(b, std::end(seq), at);
        res.emplace_back(
            p(std::make_move_iterator(b), std::make_move_iterator(e)));
    } while (++e < std::end(seq));
    return res;
}
auto split_seq(auto &&v, auto &&at, auto p) {
    auto seq = std::move(v);
    using It = decltype(std::begin(seq));
    using Res = decltype(p(std::begin(seq), std::begin(seq)));
    std::vector<Res> res;
    It b, e = std::begin(seq);
    do {
        if ((b = e) == std::end(seq)) {
            break;
        }
        e = std::search(b, std::end(seq), std::begin(at), std::end(at));
        res.emplace_back(
            p(std::make_move_iterator(b), std::make_move_iterator(e)));
    } while (e += std::size(at) < std::end(seq));
    return res;
}
auto to_str(const UVec<char> &v) {
    std::string res;
    std::copy(std::begin(v), std::end(v), std::back_inserter(res));
    return res;
}
void draw(sf::RenderWindow &win, const Shape &shape) {
    sf::Vertex arr[2];
	arr[0].color = sf::Color::White;
	arr[1].color = sf::Color::White;
	static float step = 0.0;

    for (unsigned i = 0; i < shape.size() - 1; ++i) {
        arr[0].position = shape[i];
        arr[1].position = shape[i + 1];
#ifndef NCOLOR
        arr[0].color -= sf::Color((uint8_t)step, (uint8_t)(0.5*step), (uint8_t)(0.1*step), 0);
        arr[1].color -= sf::Color((uint8_t)step, (uint8_t)(0.5*step), (uint8_t)(0.1*step), 0);
#endif
        win.draw(arr, 2, sf::Lines);
    }
    if (shape.size() > 2) {
        arr[0].position = shape[shape.size() - 1];
        arr[1].position = shape[0];
#ifndef NCOLOR
        arr[0].color -= sf::Color((uint8_t)step, (uint8_t)(0.5*step), (uint8_t)(0.1*step), 0);
        arr[1].color -= sf::Color((uint8_t)step, (uint8_t)(0.5*step), (uint8_t)(0.1*step), 0);
#endif
        win.draw(arr, 2, sf::Lines);
    }
	step = step > epilepsy_factor * 255 ? 0 : step + 0.5f;
}
void draw(sf::RenderWindow &win, const Shapes &shapes) {
    for (const auto &shape : shapes) {
        draw(win, shape);
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
    res.erase(std::remove_if(std::begin(res), std::end(res),
                             [](const auto &it) { return it == ' '; }),
              std::end(res));
    return res;
}

struct Rule {
    bool identity{false}, opt_noadjmids{true}, opt_nocenter{true};
    size_t poly_type{0}, self_cycle;
    UVec<char> lhs;
    UVec<UVec<char>> vrhs;
    Map<char, Vertex> vmap;
    Rule() : identity{true} {
        /* The identity rule Rule().apply(shape) is \shape -> [shape]
         * used by Grammar::map<n-of-gons, Rule> when n-gon is not in the map
         * this means that if there's no parser associated to the shape
         * shape is retuned in the form of [shape] (by apply).
         */
    }
    Rule(const std::string &rule_copy);
    Shapes apply(sf::RenderWindow &win, const Shape &shape);
    void calc_mids();
};
auto to_str(const Rule &r) {
    return to_str(r.lhs) + '>' + to_str(join(r.vrhs, ','));
}
Rule::Rule(const std::string &rule_copy) {
    std::string rule = remove_spaces(rule_copy);
    auto it = std::find(std::begin(rule), std::end(rule), '>');
    if (it > end(rule) - 1) {
        throw std::runtime_error("No sparator\nHint: lhs>[rhs]");
    }
    std::string rhs = {it + 1, std::end(rule)};
    std::copy(std::begin(rule), it, std::back_inserter(lhs));
    poly_type = std::count_if(std::begin(lhs), std::end(lhs), is_vertex);
    for (const auto &it : rhs) {
        if (std::none_of(std::begin(lhs), std::end(lhs), [&](const auto &s) {
                return (s == it) || it == ',' || it == '.';
            })) {
            throw std::runtime_error("Unknown symbol: " + std::string{it});
        }
        if (it == '.') {
            opt_nocenter = false;
        }
    }
    auto adj_mids =
        std::adjacent_find(std::begin(lhs), std::end(lhs), [](char f, char s) {
            return is_mid(f) && is_mid(s);
        });
    if (adj_mids != std::end(lhs)) {
        this->opt_noadjmids = false;
    }
    if (poly_type < 2 && lhs.size() != poly_type) {
        throw std::runtime_error("Points cannot be devided");
    }
    if (!poly_type && !opt_nocenter) {
        throw std::runtime_error("Center can't be calculated");
    }
    UVec<char> curr;
    for (auto it = std::begin(rhs); it != std::end(rhs); ++it) {
        if (*it == ',') {
            vrhs.push_back(std::move(curr));
            curr = {};
        } else {
            curr.push_back(*it);
        }
    }
    vrhs.push_back(std::move(curr));
    vrhs.erase(std::remove_if(std::begin(vrhs), std::end(vrhs),
                              [](const auto &it) { return it.empty(); }),
               std::end(vrhs));
    self_cycle = std::distance(
        std::find_if(std::begin(vrhs), std::end(vrhs),
                     [&](const auto &s) {
                         return s.size() == poly_type &&
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
void Rule::calc_mids() {
    if (opt_noadjmids) {
        // optimized mid point calculation, if there's only one
        // it's halfway between the adjacent vertices
        for (auto it = std::begin(lhs); it != std::end(lhs); ++it) {
            if (is_mid(*it)) {
                vmap[*it] = mid(vmap[*(it - 1)], vmap[*(it + 1)]);
            }
        }
    } else {
        for (auto it = std::begin(lhs); it != std::end(lhs); ++it) {
            auto it_mb = it;
            while (is_mid(*it)) {
                ++it;
            }
            size_t n_mids = std::distance(it_mb, it);
            for (size_t i = 0; i < n_mids; ++i) {
                vmap[*(it_mb + i)] = div(vmap[*(it_mb - 1)], vmap[*(it)],
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
    size_t i = 0;
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
            res.push_back(std::move(curr_shape));
        } else {
            draw(win, curr_shape);
        }
    }
    return res;
}

struct Grammar {
    Map<unsigned, Rule> pmap;
    Grammar(const std::string &str)
        : Grammar(split(str, ';', [](auto b, auto e) {
              return Rule(std::string(b, e));
          })) {}
    Grammar(const std::vector<Rule> &rules) {
        for (auto it = std::begin(rules); it < std::end(rules) - 1; ++it) {
            for (auto it_ = it + 1; it_ != std::end(rules); ++it_) {
                if (it->poly_type == it_->poly_type) {
                    throw std::runtime_error("Non unique rule: " +
                                             to_str(it->lhs) + " " +
                                             to_str(it_->lhs));
                }
            }
        }
        for (const auto &it : rules) {
            pmap[it.poly_type] = it;
        }
    }
    Shapes next(sf::RenderWindow &win, const Shape &shape) {
        return pmap[shape.size()].apply(win, shape);
    }
    Shapes iterate(sf::RenderWindow &win, const Shapes &state) {
        auto shapes = Shapes{};
        auto curr = Shapes{};
        for (const auto &it : state) {
            curr = next(win, it);
            shapes.insert(std::end(shapes),
                          std::make_move_iterator(std::begin(curr)),
                          std::make_move_iterator(std::end(curr)));
        }
        return shapes;
    }
    Shapes iterate(sf::RenderWindow &win, const Shapes &state,
                   unsigned depth) {
        auto res = state;
        for (unsigned i = 0; i < depth; ++i) {
            res = std::move(iterate(win, res));
        }
        return res;
    }
};
auto to_str(const Grammar &g) {
    return join(g.pmap, ';', [](auto r) { return to_str(r->second); });
}

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

int main(int argc, char *argv[]) {
    sf::VideoMode vmode{ww, wh};
    sf::RenderWindow window(vmode, argv[0], sf::Style::Default);
    sf::Event event;
    window.clear();
    window.display();
	window.setFramerateLimit(60);

	int nsteps = 7;
	const char *gram = "ABCD>AB.,BC.,CD.,DA.;AaBnnnnncnCndnnnnn>acd,Aad,aBc,dcC";
	int poly = 4;
	int move_i = 0;

	if (argc-1 > 0) {
		gram = argv[1];
	}
	if (argc-2 > 0) {
		nsteps = atoi(argv[2]);
	}
	if (argc-3 > 0) {
		poly = atoi(argv[3]);
	}
	if (argc-4 > 0) {
		move_i = atoi(argv[4]);
	}
	Vertex generator = {ww/2 * (1-0.1), 0.0};
	float rot = 360.f / poly;
	sf::Transform rotm = sf::Transform::Identity;
	rotm.rotate(rot);
    auto first = Shapes{{}};

	for (int i = 0; i < poly; i++) {
		first[0].emplace_back(generator + Vertex(ww/2.f, wh/2.f));
		generator = rotm * generator;
	}
    auto g = Grammar(gram);
	Shapes shapes;
    {
        boost::timer::auto_cpu_timer measure(std::cerr);
        shapes = g.iterate(window, first, nsteps);
    }
    sf::Vector2u windowSize = window.getSize();
    sf::Texture texture;
    texture.create(windowSize.x, windowSize.y);
    texture.update(window);
    sf::Image screenshot = texture.copyToImage();
    window.display();

    while (window.isOpen()) {
		window.clear();
        draw(window, shapes);
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
                    texture.update(window);
                    texture.copyToImage().saveToFile(std::string(gram) + ".png");
                    break;
                default:
                    break;
                }
        }
		for (auto &s : shapes) {
			auto c = Vertex(0, 0);
			for (const auto &p : s) {
				c += p;
			}
			c.x /= s.size();
			c.y /= s.size();
			for (int i = move_i; i < s.size(); i++) {
				sf::Transform id = sf::Transform::Identity;
				s[i] = id.rotate(1.5, c) * s[i];
			}
		}
	    window.display();
    }
    return 0;
}
