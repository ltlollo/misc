// gpp self $cppflags $(pkg-config --libs sfml-all)

#include <assert.h>
#include <err.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// cpp

#include <algorithm>
#include <fstream>
#include <iostream>
#include <random>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>

using namespace std;
using namespace sf;

template <size_t N> struct Stack {
    static constexpr size_t size = N;
    union {
        float f;
        uint32_t u;
        uint32_t i;
    } buf[N];
};

enum Instr {
    IAdd,
    ISub,
    IMul,
    FAdd,
    FSub,
    FMul,
    UAdd,
    USub,
    UMul,

    Exp,
    Log,

    FDiv,
    FMod,
    IDiv,
    IMod,
    UDiv,
    UMod,

    Sin,
    Cos,

    EOI,

    Not,

    Tan,

    Asinh,
    Acosh,
    Atanh,

    Sinh,
    Cosh,
    Tanh,

    Asin,
    Acos,
    Atan,

};

template <size_t N> struct Prog {
    static constexpr size_t size = N;
    Instr icache[N];
};

template <size_t StackSize, size_t ProgSize, int nb = 1> struct Cell {
    Stack<StackSize> stack;
    Prog<ProgSize> prog;
    static constexpr unsigned nbs = (2 * nb + 1) * (2 * nb + 1);
    uint64_t NbPos[nbs];
    Cell() {
        static_assert(StackSize && ProgSize && nb > 0);
        std::random_device r;
        std::default_random_engine rd(r());
        std::uniform_int_distribution<unsigned> idist(0, EOI - 1);
        for (auto &i : prog.icache) {
            i = (Instr)idist(rd);
        }
        std::uniform_int_distribution<uint32_t> cdist(0, StackSize - 1);
        for (auto &c : NbPos) {
            c = cdist(rd);
        }
    }
    void populate(const auto &mat, size_t pi, size_t pj) {
        unsigned c = 0;
        for (int i = -nb; i <= nb; ++i) {
            for (int j = -nb; j <= nb; ++j) {
                auto col = mat.getPixel(pj + j, pi + i).toInteger();
                stack.buf[NbPos[c++]].u = col;
            }
        }
    }
    auto run(const auto &mat, size_t pi, size_t pj) {
        populate(mat, pi, pj);
        size_t sp = 0;
        float f;
        unsigned u;
        int i;
        for (const auto &instr : prog.icache) {
            switch (instr) {
            default:
                break;
            case Sin:
                if ((f = sin(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Cos:
                if ((f = cos(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Tan:
                if ((f = tan(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Asin:
                if ((f = asin(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Acos:
                if ((f = acos(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Atan:
                if ((f = atan(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Sinh:
                if ((f = sinh(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Cosh:
                if ((f = cosh(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Tanh:
                if ((f = tanh(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Asinh:
                if ((f = asinh(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Acosh:
                if ((f = acosh(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Atanh:
                if ((f = atanh(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Exp:
                if ((f = exp(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Log:
                if ((f = log(stack.buf[sp].f)) != NAN) {
                    stack.buf[sp].f = f;
                }
                break;
            case Not:
                stack.buf[sp].u = !stack.buf[sp].u;
                break;
            case FAdd:
                f = stack.buf[sp].f;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].f += f;
                break;
            case FSub:
                f = stack.buf[sp].f;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].f -= f;
                break;
            case FMul:
                f = stack.buf[sp].f;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].f *= f;
                break;
            case FDiv:
                if ((f = stack.buf[sp].f) == 0) {
                    break;
                }
                sp = (sp + 1) % StackSize;
                stack.buf[sp].f /= f;
                break;
            case FMod:
                if ((f = stack.buf[sp].f) == 0) {
                    break;
                }
                sp = (sp + 1) % StackSize;
                stack.buf[sp].f = fmod(stack.buf[sp].f, f);
                break;
            case UAdd:
                u = stack.buf[sp].u;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].u += u;
                break;
            case USub:
                u = stack.buf[sp].u;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].u -= u;
                break;
            case UMul:
                u = stack.buf[sp].u;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].u *= u;
                break;
            case UDiv:
                if ((u = stack.buf[sp].u) == 0) {
                    break;
                }
                sp = (sp + 1) % StackSize;
                stack.buf[sp].u /= u;
                break;
            case UMod:
                if ((u = stack.buf[sp].u) == 0) {
                    break;
                }
                sp = (sp + 1) % StackSize;
                stack.buf[sp].u %= u;
                break;
            case IAdd:
                i = stack.buf[sp].i;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].i += i;
                break;
            case ISub:
                i = stack.buf[sp].i;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].i -= i;
                break;
            case IMul:
                i = stack.buf[sp].i;
                sp = (sp + 1) % StackSize;
                stack.buf[sp].i *= i;
                break;
            case IDiv:
                if ((i = stack.buf[sp].i) == 0) {
                    break;
                }
                sp = (sp + 1) % StackSize;
                stack.buf[sp].i /= i;
                break;
            case IMod:
                if ((i = stack.buf[sp].i) == 0) {
                    break;
                }
                sp = (sp + 1) % StackSize;
                stack.buf[sp].i %= i;
                break;
            }
        }
        return stack.buf[sp].u;
    }
};

void
wrap(auto &img) {
    auto size = img.getSize();
    size_t x = size.x, y = size.y;
    for (size_t i = 1; i < y - 1; ++i) {
        auto c = img.getPixel(x - 2, i);
        img.setPixel(0, i, c);
    }
    for (size_t i = 1; i < y - 1; ++i) {
        auto c = img.getPixel(1, i);
        img.setPixel(x - 1, i, c);
    }
    for (size_t i = 1; i < x - 1; ++i) {
        auto c = img.getPixel(i, y - 2);
        img.setPixel(i, 0, c);
    }
    for (size_t i = 1; i < x - 1; ++i) {
        auto c = img.getPixel(i, 1);
        img.setPixel(i, y - 1, c);
    }
}

void
apply(auto &prog, auto &fimg, const auto &simg, size_t x, size_t y,
      unsigned times = 1) {
    while (times--)
        for (size_t i = 1; i < y - 1; ++i) {
            for (size_t j = 1; j < x - 1; ++j) {
                auto c = Color(prog.run(simg, i, j));
                fimg.setPixel(j, i, c);
            }
        }
}

int
main(int argc, char *argv[]) {
    if (argc - 1 < 1) {
        err(1, "not eno args");
    }

    Texture texture;
    if (!texture.loadFromFile(argv[1])) {
        err(1, "imga loading failed");
    }
    auto fimg = texture.copyToImage();
    auto simg = texture.copyToImage();
    Sprite sprite(texture);

    wrap(fimg);
    wrap(simg);

    auto size = fimg.getSize();
    size_t x = size.x, y = size.y;

    if (x < 20 || y < 20) {
        err(1, "size too small");
    }

    Cell<16, 2> prog;

    RenderWindow window(VideoMode(800, 600), argv[0]);
    while (window.isOpen()) {
        Event event;
        while (window.pollEvent(event)) {
            if (event.type == Event::Closed ||
                (event.type == Event::KeyPressed &&
                 (event.key.code == Keyboard::Escape ||
                  event.key.code == Keyboard::Q))) {
                window.close();
            }
        }
        window.clear();
        apply(prog, fimg, simg, x, y);
        wrap(fimg);
        texture.update(fimg);
        window.draw(sprite);
        window.display();
        swap(fimg, simg);
    }
    return 0;
}

