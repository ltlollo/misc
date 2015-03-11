#include <vector>
#include <algorithm>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <iterator>
#include <iostream>
#include <map>
#include <cmath>

// gpp shape-sys.cpp $(pkg-config --libs sfml-all) && ./shape-sys

constexpr unsigned wh{1200}, ww{1200};

using namespace std;

using Shape = vector<sf::Vertex>;
using Shapes = vector<Shape>;

sf::Vertex mid(const sf::Vertex& fst, const sf::Vertex& snd) {
    return {{
         (fst.position.x+snd.position.x)/2.f
        ,(fst.position.y+snd.position.y)/2.f
    }};
}

sf::Vertex divseg(const sf::Vertex& fst, const sf::Vertex& snd,
        float of = 1.f, float n = 2.f) {
    float mx = std::min(fst.position.x, snd.position.x)
        ,Mx = std::max(fst.position.x, snd.position.x)
        ,my = std::min(fst.position.y, snd.position.y)
        ,My = std::max(fst.position.y, snd.position.y);
    return {{
          fst.position.x == mx ? (Mx-mx)*of/n + mx : (Mx-mx)*(n-of)/n + mx
         ,fst.position.y == my ? (My-my)*of/n + my : (My-my)*(n-of)/n + my
    }};
}

void drawShapes(sf::RenderWindow& win, const Shapes& shapes) {
    for(const auto& shape: shapes) {
        for (unsigned i = 0; i < shape.size() - 1; ++i) {
            win.draw(&shape[i], 2, sf::Lines);
        } if (shape.size() > 2) {
            auto last = Shape{shape[shape.size()-1], shape[0]};
            win.draw(&last[0], 2, sf::Lines);
        }
    }
}

bool is_vertex(char it) {
    return (it >= 'A' && it <= 'Z');
}

bool is_mid(char it) {
    return (it >= 'a' && it <= 'z');
}

struct Parser {
    bool do_nothing{false}, opt_noadjmids{true};
    unsigned type{0};
    string lhs;
    vector<string> vrhs;
    std::map<char, sf::Vertex> vmap;
    Parser() : do_nothing{true} {
        // the identity parser p().apply(shape) is \shape -> [shape]
        // used by Grammar::map<?, Parser> when ? is not in the map
        // this means that if there's no parser associated to the shape
        // shape is retuned in the form of [shape].
    }

    Parser(const string& rule) {
        string rhs;
        auto it = std::find(begin(rule), end(rule), '>');
        if (it > end(rule)-1) {
            throw std::runtime_error("No sparator\nHint: lhs>[rhs]");
        }
        lhs = {begin(rule), it};
        rhs = {it+1, end(rule)};
        type = std::count_if(begin(lhs), end(lhs), is_vertex);
        for (const auto& it: rhs) {
            if (std::none_of(begin(lhs), end(lhs), [&](const auto& s){
                return (s == it) || it == ',';
            })) {
                throw std::runtime_error("Unknown symbol: " + string{it});
            }
        }
        auto adj_mids = std::adjacent_find(begin(lhs), end(lhs),
                                           [](char f, char s){
            return is_mid(f) && is_mid(s);
        });
        if (adj_mids != end(lhs)) {
            this->opt_noadjmids = false;
        }
        if (type < 2 && lhs.size() != type) {
            throw std::runtime_error("Points cannot be devided");
        }
        string curr;
        for (auto it = begin(rhs); it != end(rhs); ++it) {
            if (*it == ',') {
                vrhs.push_back(move(curr));
                curr = string{};
            } else {
                curr.push_back(*it);
            }
        }
        vrhs.push_back(move(curr));
        vrhs.erase(std::remove_if(begin(vrhs), end(vrhs), [](const auto& it){
            return it.empty();
        }), end(vrhs));

        if (!lhs.empty()) {
            if (!is_vertex(lhs[0])) {
                throw std::runtime_error("Must start with a vertex");
            }
            // this is a microopt, see(*)
            lhs.push_back(lhs[0]);
        }
    }
    Shapes apply(const Shape& shape);
    void calc_mids();
};

void Parser::calc_mids() {
    if (opt_noadjmids) {
        // optimized mid point calculation, if there's only one
        // it's halfway between the adjacent vertices
        for(auto it = begin(lhs); it != end(lhs); ++it) {
            if (is_mid(*it)) {
                vmap[*it] = mid(vmap[*(it-1)], vmap[*(it+1)]);
            }
        }
    } else {
        for(auto it = begin(lhs); it != end(lhs); ++it) {
            auto it_mb = it;
            while (is_mid(*it)) {
                ++it;
            }
            unsigned n_mids = std::distance(it_mb, it);
            for (unsigned i = 0; i < n_mids; ++i) {
                vmap[*(it_mb+i)] = divseg(vmap[*(it_mb-1)], vmap[*(it)], (i+1),
                        (n_mids+1));
            }

        }
    }
}

Shapes Parser::apply(const Shape& shape) {
    if (do_nothing) {
        return {shape};
    }
    auto res = Shapes{};
    if (vrhs.empty()) {
        return res;
    }
    res.reserve(vrhs.size());
    unsigned i = 0;
    for(const auto& it: shape) {
        while(!is_vertex(lhs[i])) {
            ++i;
        }
        vmap[lhs[i]] = it;
        ++i;
    }
    calc_mids();
    for (const auto& it: vrhs) {
        auto curr_shape = Shape{};
        curr_shape.reserve(it.size());
        for (const auto& sv: it) {
            curr_shape.push_back(vmap[sv]);
        }
        res.push_back(move(curr_shape));
    }
    return res;
}

struct Grammar {
    std::map<unsigned, Parser> pmap;
    Grammar(const vector<Parser>& rules) {
        for (auto it_f = begin(rules); it_f != end(rules); ++it_f) {
            for (auto it_s = it_f+1; it_s != end(rules); ++it_s) {
                if (it_f->type == it_s->type) {
                    throw std::runtime_error("Non unique rule: " + it_f->lhs +
                    " " + it_s->lhs);
                }
            }
        }
        for (const auto& it: rules) {
            pmap[it.type] = it;
        }
    }
    Shapes next(const Shape& shape) {
        return pmap[shape.size()].apply(shape);
    }
    Shapes iterate(const Shapes& state) {
        auto shapes = Shapes{};
        auto curr = Shapes{};
        for (const auto& it: state) {
            curr = this->next(it);
            shapes.insert(end(shapes)
                    ,std::make_move_iterator(begin(curr))
                    ,std::make_move_iterator(end(curr))
                    );
        }
        return shapes;
    }
    Shapes iterate(const Shapes& state, unsigned depth) {
        auto res = state;
        for (unsigned i = 0; i < depth; ++i) {
            res = move(this->iterate(res));
        }
        return res;
    }
};

/* grammar explanation
 * def: RULE := LHS '>' RHS
 *      LHS := [A-Z][:alpha:]
 *      RHS := "" | [:alpha:] | RHS ',' RHS
 * ex: AbCdEf>ACE,bdf
 *      - AbCdEf>? instructs the parser to match an ACE shaped plygon,
 *        introducing b,d,f points between it's vertices
 *      - ?>aBc instucts the parser to form a new aBc polygon with using the
 *        vertices introduced in LHS
 *      - Old vertices must be uppercase, new ones lowercase.
 *      - The LHS definition wraps arownd, therfore in "ABCd", d is considered
 *        between A and B (*)
 * def: RULES := RULE | RULE, RULES
 *      - rules LHS must match unique polygons
 *        ( ex: "ABC>", "AdBC>" is not allowed )
 */

int main(int argc, char *argv[]) {
    sf::RenderWindow window{{ww, wh}, "shapes"};
    sf::Event event;
    auto g = Grammar{{
         Parser("AaBbCcDd>abcd,aAb,bBc,cCd,dDa")
        ,Parser("AabBcCd>abcd")
    }};
    auto first = Shapes{{
         {{10., 10.}, sf::Color::Red}
        ,{{1010.,10.}, sf::Color::Yellow}
        ,{{1010.,1010.}, sf::Color::Blue}
        ,{{10.,1010.}, sf::Color::Green}
    }};
    auto shapes = g.iterate(first, 10);

    window.clear();
    drawShapes(window, shapes);
    while(window.isOpen()) {
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            }
            else if (event.type == sf::Event::KeyPressed)
                switch (event.key.code) {
                case sf::Keyboard::Escape:
                    window.close();
                    break;
                default:
                    break;
                }
        }
        window.display();
    }
    return 0;
}
