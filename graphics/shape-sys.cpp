#include <vector>
#include <algorithm>
#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <iterator>

// gpp shape-sys.cpp $(pkg-config --libs sfml-all) && ./shape-sys

constexpr unsigned wh{600}, ww{800};

using namespace std;

enum PolyT { None = 0, Point, Line, Tri, Quad, Penta, Hexa };

struct Shape : public vector<sf::Vertex> {
    using vector<sf::Vertex>::vector;
    PolyT type() const {
        return (PolyT)this->size();
    }
};

using Shapes = vector<Shape>;

sf::Vertex mid(const sf::Vertex& fst, const sf::Vertex& snd) {
    return {{
         (fst.position.x+snd.position.x)/2.f
        ,(fst.position.y+snd.position.y)/2.f
    }};
}

template<PolyT T> struct Rule {
    static Shapes next(const Shape& s) {
        return {s};
    }
};

template<> struct Rule<PolyT::Tri> {
    static Shapes next(const Shape& shape) {
        auto res = Shapes{};
        auto intersec_fst = mid(shape[0], shape[1]);
        auto intersec_snd = mid(shape[0], shape[2]);
        auto intersec_trd = mid(shape[1], shape[2]);
        res = Shapes{
              {shape[0], intersec_fst, intersec_snd}
             ,{shape[1], intersec_fst, intersec_trd}
             ,{shape[2], intersec_snd, intersec_trd}
        };
        return res;
    }
};

template<> struct Rule<PolyT::Quad> {
    static Shapes next(const Shape& shape) {
        auto res = Shapes{};
        auto intersec_fst = mid(shape[1], shape[2]);
        res = Shapes{
             {shape[0], shape[1], intersec_fst}
            ,{shape[0], intersec_fst, shape[3]}
            ,{intersec_fst, shape[3], shape[2]}
        };
        return res;
    }
};

Shapes match(const Shape& shape) {
    switch (shape.type()) {
        case PolyT::Tri:
            return Rule<PolyT::Tri>::next(shape);
        case PolyT::Quad:
            return Rule<PolyT::Quad>::next(shape);
        case PolyT::Penta:
            return Rule<PolyT::Penta>::next(shape);
        default:
            return Rule<None>::next(shape);
    }
}

Shapes iterate(const Shapes& state) {
    auto shapes = Shapes{};
    auto next = Shapes{};
    for (const auto& it: state) {
        next = move(match(it));
        shapes.insert(end(shapes)
                ,std::make_move_iterator(begin(next))
                ,std::make_move_iterator(end(next))
                );
    }
    return shapes;
}

Shapes iterate(const Shapes& state, unsigned depth) {
    auto res = state;
    for (unsigned i = 0; i < depth; ++i) {
        res = move(iterate(res));
    }
    return res;
}

void drawShapes(sf::RenderWindow& win, const Shapes& shapes) {
    for(const auto& shape: shapes) {
        for (unsigned i = 0; i < shape.size() - 1; ++i) {
            win.draw(&shape[i], 2, sf::Lines);
        } if (shape.type() > PolyT::Line) {
            auto last = Shape{shape[shape.size()-1], shape[0]};
            win.draw(&last[0], 2, sf::Lines);
        }
    }
}

/*   A_e_B
 * h_|   | _f
 *   |_ _|
 *   D g C
 * How could a grammr rule be introduced: LHS "}A{e}B{f}C{g}D{h" -> RHS "Adf,Bde,Cef,.."
 */


int main(int argc, char *argv[]) {
    sf::RenderWindow window{{ww, wh}, "shapes"};
    sf::Event event;

    auto first = Shapes{{
        {{300.,100.}}, {{600.,400.}}, {{100.,400.}}
    }};
    auto shapes = iterate(first, 7);

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
        window.clear();
        drawShapes(window, shapes);
        window.display();
    }
    return 0;
}
