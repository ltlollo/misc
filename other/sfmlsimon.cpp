// gpp sfmlsimon.cpp -lsfml-system -lsfml-graphics -lsfml-window -lsfml-audio

#include <SFML/Window.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>
#include <SFML/System.hpp>
#include <vector>
#include <array>
#include <cmath>
#include <random>

constexpr unsigned winSize{600}, sampRate{44100}, vol{30000},
          samples{sampRate}, base{3}, gridSize{base*base}, speedUp{10};
constexpr double audioIncr{330.0/sampRate}, // 330Hz, speakers' min freq
    twoPi{6.283185307179586}, angSpd{audioIncr*twoPi}, tileBase{winSize/base};
constexpr sf::Uint8 colorIncr{20}, mul{0x33};

static std::random_device rd;
static std::default_random_engine gen(rd());
static std::discrete_distribution<sf::Uint8> colorU8{20, 10, 5, 10, 20};
static std::uniform_int_distribution<sf::Uint8> position(0, gridSize-1);
static sf::Int16 tonebuf[samples];

static inline double tone(const unsigned t, const unsigned scale) {
    return sin(scale * angSpd * t);
}

static inline int8_t sign(const unsigned t, const unsigned scale) {
    double x = tone(t, scale);
    return x > 0 ? 1 : x < 0 ? -1 : 0;
}

class AudioPools {
    std::vector<sf::SoundBuffer> tonePool;
    sf::Sound sound;
public:
    AudioPools(const unsigned size) {
        tonePool.reserve(size);
        sf::SoundBuffer sbuf;
        for (unsigned i = 1; i < size+1; ++i) {
            for (unsigned j= 0; j < samples; ++j) {
                tonebuf[j] = sf::Int16(vol * sign(i, j));
            }
            sbuf.loadFromSamples(tonebuf, samples, 1, sampRate);
            tonePool.push_back(std::move(sbuf));
        }
    }
    void playSample(const unsigned pos) {
        if (pos < tonePool.size()) {
            sound.setBuffer(tonePool[pos]);
            sound.setLoop(false);
            sound.play();
        }
    }
    void stop() {
        sound.stop();
    }
};

class Tile {
    const unsigned number;
    const sf::Color origColor, alternColor;
    sf::RectangleShape shape;
public:
    Tile(const unsigned pos, const double x, const double y)
        : number{pos},
          origColor(sf::Uint8(mul*colorU8(gen)),
                    sf::Uint8(mul*colorU8(gen)),
                    sf::Uint8(mul*colorU8(gen))
                    ),
          alternColor(sf::Uint8((origColor.r + colorIncr)%255),
                      sf::Uint8((origColor.b + colorIncr)%255),
                      sf::Uint8((origColor.g + colorIncr)%255)
                      ) {
        shape.setPosition(float(x), float(y));
        shape.setSize({tileBase, tileBase});
        shape.setOrigin(tileBase/2., tileBase/2.);
        shape.setFillColor(origColor);
    }
    const sf::RectangleShape& getShape() const {
        return shape;
    }
    void press(sf::RenderWindow& rw, AudioPools& ap, const unsigned ms) {
        shape.setFillColor(alternColor);
        ap.playSample(number);
        rw.draw(shape);
        rw.display();
        sleep(sf::milliseconds(ms));
        shape.setFillColor(origColor);
        rw.draw(shape);
        rw.display();
        ap.stop();
    }
};

int main() {
    sf::Event event;
    sf::RenderWindow window{{winSize, winSize}, "Simon Says"};
    AudioPools audio(gridSize);
    std::vector<Tile> tiles;
    std::vector<unsigned> simonSays, playerInput;
    bool playerTurn{false}, stillWinning{true};
    unsigned speed{200};

    for(unsigned i = 0; i < base; ++i) {
        for(unsigned j = 0; j < base; ++j) {
            tiles.emplace_back(Tile(j + i*base, tileBase/2. + j*tileBase,
                                    tileBase/2. + i*tileBase));
        }
    }
    while(stillWinning) {
        window.clear(sf::Color::Black);
        if(sf::Keyboard::isKeyPressed(sf::Keyboard::Key::Escape)) {
            break;
        }
        for (const auto& tile: tiles) {
            window.draw(tile.getShape());
        }
        window.display();
        if (playerTurn) {
            while (window.pollEvent(event)) {
                switch (event.type) {
                case sf::Event::KeyPressed:
                    switch (event.key.code) {
                    case sf::Keyboard::Q:
                        tiles[0].press(window, audio, speed);
                        playerInput.push_back(0);
                        break;
                    case sf::Keyboard::W:
                        tiles[1].press(window, audio, speed);
                        playerInput.push_back(1);
                        break;
                    case sf::Keyboard::E:
                        tiles[2].press(window, audio, speed);
                        playerInput.push_back(2);
                        break;
                    case sf::Keyboard::A:
                        tiles[3].press(window, audio, speed);
                        playerInput.push_back(3);
                        break;
                    case sf::Keyboard::S:
                        tiles[4].press(window, audio, speed);
                        playerInput.push_back(4);
                        break;
                    case sf::Keyboard::D:
                        tiles[5].press(window, audio, speed);
                        playerInput.push_back(5);
                        break;
                    case sf::Keyboard::Z:
                        tiles[6].press(window, audio, speed);
                        playerInput.push_back(6);
                        break;
                    case sf::Keyboard::X:
                        tiles[7].press(window, audio, speed);
                        playerInput.push_back(7);
                        break;
                    case sf::Keyboard::C:
                        tiles[8].press(window, audio, speed);
                        playerInput.push_back(8);
                        break;
                    default:
                        break;
                    }
                    for (unsigned i{0}; i < playerInput.size(); ++i) {
                        if (playerInput.at(i) != simonSays.at(i)) {
                            stillWinning = false;
                        }
                    }
                    if (playerInput.size() == simonSays.size()) {
                        playerInput.clear();
                        if (speed > speedUp) {
                            speed -= speedUp;
                        }
                        playerTurn = false;
                    }
                    break;
                case sf::Event::Closed:
                    stillWinning = false;
                    window.close();
                    break;
                default:
                    break;
                }
            }
        }
        else {
            sleep(sf::milliseconds(200));
            simonSays.emplace_back(position(gen));
            for (const auto& pos: simonSays) {
                tiles[pos].press(window, audio, speed);
            }
            playerTurn = true;
        }
    }
    return 0;
}
