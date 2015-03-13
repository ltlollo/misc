#include <vector>
#include <string>
#include <iostream>
#include <cmath>
#include <algorithm>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>

/* gpp lsys.cpp $(pkg-config --libs sfml-all) */

using namespace std;

constexpr unsigned wh{900}, ww{900};
constexpr float pi = 3.141592653589793;

using uint  = unsigned;
using Sym = char;
using State = vector<Sym>;

struct Rule {
    Sym sym;
    State expand;
};

using Rules = vector<Rule>;
using Position = sf::Vector2f;

State next(const State& state, const Rules& rules) {
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
            ++it;
            break;
        } else {
            mutate(*it++, state, sys.conf);
        }
    }
}

void drawGraph(sf::RenderWindow& win, const System& sys,
        float x = 0.5, float y = 0.5) {
    auto it = sys.state.cbegin();
    auto startPos = sf::Vertex(sf::Vector2f(win.getSize().x * x,
                                            win.getSize().y * y));
    consumer(sys, it, GraphState{{startPos, startPos}, pi/2, sys.conf.len,
            win});
}

State to_vec(const string& str) {
    return State(begin(str), end(str));
}

int main() {
    sf::RenderWindow window{{ww, wh}, "graph"};
    sf::Event event;
    float speed = 0.0001f, size = 70.0f, x = 0.5f, y = 0.5f;
    auto sys = System(
                   State{to_vec("F")},
                   Rules{
                       {'F', to_vec("GFs[+F][-F]")}
                       ,{'G', to_vec("+")}
                   },
                   Config{size, to_rad(Angle<Grad>{60.0})},
                   9);
    drawGraph(window, sys, x, y);

    while(window.isOpen()) {
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            }
            else if (event.type == sf::Event::KeyPressed)
                switch (event.key.code) {
                case sf::Keyboard::W:
                    y += 0.1;
                    break;
                case sf::Keyboard::S:
                    y -= 0.1;
                    break;
                case sf::Keyboard::A:
                    x += 0.1;
                    break;
                case sf::Keyboard::D:
                    x -= 0.1;
                    break;
                case sf::Keyboard::E:
                    sys.conf.len += 5;
                    break;
                case sf::Keyboard::Q:
                    sys.conf.len -= 5;
                    break;
                case sf::Keyboard::X:
                    speed *= 5;
                    break;
                case sf::Keyboard::Z:
                    speed /= 5;
                    break;
                case sf::Keyboard::B:
                    speed *= (-1);
                    break;
                case sf::Keyboard::Escape:
                    window.close();
                    break;
                default:
                    break;
                }
        }
        sys.conf.ang.value += speed;
        window.clear();
        drawGraph(window, sys, x, y);
        window.display();
    }
    return 0;
}
