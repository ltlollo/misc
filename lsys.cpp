#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <math.h>

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

struct Config {
    float len;
    float ang;
};

struct System {
    State state;
    Config conf;
    System(const State& start, const vector<Rule>& rules, const Config& conf,
           uint depth)
        : state{start} {
        this->conf = conf;
        for (uint i = 0; i < depth; ++i) {
            state = next(state, rules);
        }
    }
};

struct GraphState {
    sf::Vertex line[2];
    float angle;
    sf::RenderWindow& win;
};

void mutate(Sym sym, GraphState& state, const Config& conf) {
    switch (sym) {
    case 'F':
    case 'G':
        state.line[1] = sf::Vertex({state.line[0].position.x
                                    - conf.len*cos(state.angle),
                                    state.line[0].position.y
                                    - conf.len*sin(state.angle)});
        state.win.draw(state.line, 2, sf::Lines);
        state.line[0] = state.line[1];
        break;
    case '-':
        state.angle -= conf.ang*pi/180.0;
        break;
    case '+':
        state.angle += conf.ang*pi/180.0;
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
    auto startPos = sf::Vertex(sf::Vector2f(win.getSize().x/2,
                                            win.getSize().y));
    consumer(sys, it, GraphState{{startPos, startPos}, pi/2, win});
}

auto to_vec(const string& str) {
    return vector<Sym>(begin(str), end(str));
}

int main() {
    sf::RenderWindow window{{ww, wh}, "graph"};
    sf::Event event;
    auto sys = System(
                   State{to_vec("F")},
                   Rules{
                       {'F', to_vec("G+F+G")}
                      ,{'G', to_vec("F-G-F")}
                   },
                   Config{5, 60}, 6);
    drawGraph(window, sys);

    while(window.isOpen()) {
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                window.close();
            }
            else if (event.type == sf::Event::KeyPressed)
                switch (event.key.code) {
                case sf::Keyboard::Q: // Quit the program
                case sf::Keyboard::Escape:
                    window.close();
                    break;
                default: // Do nothing
                    break;
                }
        }
        window.display();
    }
    return 0;
}
