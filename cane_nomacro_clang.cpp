#include <random>

using namespace std;
using ui = size_t;

std::random_device rd;
std::mt19937 gen(rd());

template<unsigned N, unsigned M> constexpr unsigned size(const char (&arr)[N][M]) {
        return N;
}

struct Root{}; struct Name{}; struct Adj{}; struct Conj{};
template<typename T> struct Male{};
template<typename T> struct Female{};
template<typename T> struct Animal{};

char mnames[][6]{
    "Gesù"
    , "Dio"
};
char anames[][7]{
    "cane"
    , "porco"
    , "maiale"
};
char fnames[][8]{
    "Madonna"
    , "Maremma"
};
char madj[][11]{
    "sudicio"
    , "infiammato"
    , "beduino"
    , "buco"
    , "laido"
};
char fadjs[][11]{
    "puttana"
    , "infiammata"
    , "laida"
    , "sudicia"
    , "porca"
    , "troia"
    , "maiala"
    , "appestata"
    , "impestata"
    , "vipera"
    , "velenosa"
    , "ladra"
};

template<typename T, ui... Ns> struct Word {};

template<ui... Ns> struct Word<Root, Ns...>{
    void operator()() {
        std::uniform_int_distribution<> dist(0,1);
        switch(dist(gen)) {
            case 0: Word<Male<Name>, Ns...>()(); break;
            case 1: Word<Female<Name>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Male<Name>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(mnames)-1);
        printf("%s ", mnames[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 5);
        switch(ndist(gen)) {
            default: Word<Male<Adj>, Ns...>()(); break;
            case 1: Word<Male<Conj>, Ns...>()(); break;
            case 2: Word<Female<Conj>, Ns...>()(); break;
            case 3: Word<Animal<Conj>, Ns...>()(); break;
            case 4: Word<Animal<Name>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Animal<Name>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(anames)-1);
        printf("%s ", anames[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Male<Adj>, Ns...>()(); break;
            case 1: Word<Male<Conj>, Ns...>()(); break;
            case 2: Word<Female<Conj>, Ns...>()(); break;
            case 3: Word<Animal<Conj>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Female<Name>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(fnames)-1);
        printf("%s ", fnames[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Female<Adj>, Ns...>()(); break;
            case 1: Word<Male<Conj>, Ns...>()(); break;
            case 2: Word<Animal<Conj>, Ns...>()(); break;
            case 3: Word<Female<Conj>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Male<Adj>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(madj)-1);
        printf("%s ", madj[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 3);
        switch(ndist(gen)) {
            default: Word<Male<Adj>, Ns...>()(); break;
            case 1: Word<Male<Conj>, Ns...>()(); break;
            case 2: Word<Female<Conj>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Female<Adj>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(fadjs)-1);
        printf("%s ", fadjs[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Female<Adj>, Ns...>()(); break;
            case 1: Word<Female<Conj>, Ns...>()(); break;
            case 2: Word<Male<Conj>, Ns...>()(); break;
            case 3: Word<Animal<Conj>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Female<Conj>, N, Ns...> {
    void operator()() { printf("della "); Word<Female<Name>, Ns...>()(); }
};


template<ui N, ui... Ns> struct Word<Animal<Conj>, N, Ns...> {
    void operator()() { printf("del "); Word<Animal<Name>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Male<Conj>, N, Ns...> {
    void operator()() { printf("di "); Word<Male<Name>, Ns...>()(); }
};

template<typename N> struct Word<N> { void operator()() { printf("\n"); } };
template<ui N>struct Word<Female<Conj>, N> { void operator()() { Word<Female<Conj>, N, N>()(); }};
template<ui N>struct Word<Male<Conj>,   N> { void operator()() { Word<Male<Conj>,   N, N>()(); }};
template<ui N>struct Word<Animal<Conj>, N> { void operator()() { Word<Animal<Conj>, N, N>()(); }};
template<ui N>struct Word<Animal<Name>, N> { void operator()() { Word<Animal<Name>, N, N>()(); }};

template<ui... Ns> struct Num {};
template<ui N, typename T> struct GenGraph {
    template<ui... Ns> void operator()(Num<Ns...> n){ return GenGraph<N-1, T>()(Num<Ns..., N>()); }
};

template<typename T> struct GenGraph<0, T> {
    template<ui... Ns> void operator()(Num<Ns...> n){ return Word<T, Ns...>()(); }
};

template<ui N> void Graph() { GenGraph<N, Root>()(Num<>()); }

int main(int argc, char *argv[]) {
    Graph<10>();
    return 0;
}
