#include <vector>
#include <string>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>

/* gpp lsys.cpp $(pkg-config --libs sfml-all) */

using namespace std;

constexpr unsigned wh{900}, ww{1600};
constexpr float pi = 3.141592653589793;

using State = vector<char>;
using Sym = char;

struct Rule {
    Sym sym;
    State expand;
};

using uint  = unsigned;
using Rules = vector<Rule>;
using Position = sf::Vector2f;

State next(const State& state, const vector<Rule>& rules) {
    State nstate;
    for (const auto& sym: state) {
        auto it = find_if(begin(rules), end(rules),
                          [&](const auto& rule){ return rule.sym == sym; });
        if (it == end(rules)) {
            nstate.push_back(sym);
        } else {
            nstate.insert(end(nstate), begin(it->expand), end(it->expand));
        }
    }
    return nstate;
}

enum AngT{ Rad, Grad };
template<AngT> struct Angle {
    float value;
    constexpr Angle(float value) : value{value} {}
};

constexpr Angle<Rad> to_rad(Angle<Grad> ang) {
    return Angle<Rad>(ang.value*pi/180);
}

struct Config {
    float len;
    Angle<Rad> ang;
    constexpr Config(float len, Angle<Rad> ang)
    : len{len}, ang{ang} {}
};

struct System {
    State state;
    Config conf;
    System(const State& start, const vector<Rule>& rules,
           const Config& conf, uint depth)
        : state{start}, conf{conf} {
        for (uint i = 0; i < depth; ++i) {
            state = next(state, rules);
        }
    }
};

struct GraphState {
    sf::Vertex line[2];
    float angle, len;
    sf::RenderWindow& win;
};

void mutate(Sym sym, GraphState& state, const Config& conf) {
    switch (sym) {
    case 'F':
    case 'G':
        state.line[1] = sf::Vertex({state.line[0].position.x
                                    - state.len*cos(state.angle),
                                    state.line[0].position.y
                                    - state.len*sin(state.angle)});
        state.win.draw(state.line, 2, sf::Lines);
        state.line[0] = state.line[1];
        break;
    case '-':
        state.angle -= conf.ang.value;
        break;
    case '+':
        state.angle += conf.ang.value;
        break;
    case 'S':
        state.len = sqrtf(state.len);
        break;
    case 'p':
        state.len *= state.len;
        break;
    case 's':
        state.len /= sqrtf(2);
        break;
    default:
        break;
    }
}

void consumer(const System& sys, auto& it, GraphState state) {
    while(it != sys.state.cend()) {
        if (*it == '[') {
            consumer(sys, ++it, state);
        } else if (*it == ']') {
            ++it; break;
        } else {
            mutate(*it++, state, sys.conf);
        }
    }
}

void drawGraph(sf::RenderWindow& win, const System& sys) {
    auto it = sys.state.cbegin();
    auto startPos = sf::Vertex(sf::Vector2f(win.getSize().x * 0.5,
                                            win.getSize().y * 0.5));
    consumer(sys, it,
            GraphState{{startPos, startPos}, pi/2, sys.conf.len, win});
}

auto to_vec(const string& str) {
    return vector<Sym>(begin(str), end(str));
}

int main() {
    sf::RenderWindow window{{ww, wh}, "graph"};
    sf::Event event;
    float speed = 0.001f;
    auto sys = System(
                   State{to_vec("F")},
                   Rules{
                       {'F', to_vec("++I[+++++F][-----F]")}
                      ,{'I', to_vec("Gs")}
                   },
                   Config{130, to_rad(Angle<Grad>{0.0})},
                   15);
    drawGraph(window, sys);

    while(window.isOpen()) {
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            }
            else if (event.type == sf::Event::KeyPressed)
                switch (event.key.code) {
                case sf::Keyboard::M:
                    speed *= 10;
                    break;
                case sf::Keyboard::N:
                    speed /= 10;
                    break;
                case sf::Keyboard::Q:
                case sf::Keyboard::Escape:
                    window.close();
                    break;
                default:
                    break;
                }
        }
        sys.conf.ang.value += speed;
        window.clear();
        drawGraph(window, sys);
        window.display();
    }
    return 0;
}
