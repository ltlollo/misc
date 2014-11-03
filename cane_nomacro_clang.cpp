#include <random>

using namespace std;
using ui = size_t;

std::random_device rd;
std::mt19937 gen(rd());

template<unsigned N, unsigned M> constexpr unsigned size(const char (&arr)[N][M]) {
        return N;
}

struct Root{};
template<typename T> struct Name{};
template<typename T> struct Adj{};
template<typename T> struct Conj{};
template<typename T> struct Proper{};
template<typename T> struct Common{};
struct Male{};
struct Female{};

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
            case 0: Word<Name<Proper<Male>>, Ns...>()(); break;
            case 1: Word<Name<Proper<Female>>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Name<Proper<Male>>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(mnames)-1);
        printf("%s ", mnames[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 5);
        switch(ndist(gen)) {
            default: Word<Adj<Male>, Ns...>()(); break;
            case 1: Word<Conj<Male>, Ns...>()(); break;
            case 2: Word<Conj<Female>, Ns...>()(); break;
            case 3: Word<Conj<Common<Male>>, Ns...>()(); break;
            case 4: Word<Name<Common<Male>>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Name<Common<Male>>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(anames)-1);
        printf("%s ", anames[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Adj<Male>, Ns...>()(); break;
            case 1: Word<Conj<Male>, Ns...>()(); break;
            case 2: Word<Conj<Female>, Ns...>()(); break;
            case 3: Word<Conj<Common<Male>>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Name<Proper<Female>>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(fnames)-1);
        printf("%s ", fnames[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Adj<Female>, Ns...>()(); break;
            case 1: Word<Conj<Male>, Ns...>()(); break;
            case 2: Word<Conj<Common<Male>>, Ns...>()(); break;
            case 3: Word<Conj<Female>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Adj<Male>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(madj)-1);
        printf("%s ", madj[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 3);
        switch(ndist(gen)) {
            default: Word<Adj<Male>, Ns...>()(); break;
            case 1: Word<Conj<Male>, Ns...>()(); break;
            case 2: Word<Conj<Female>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Adj<Female>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, size(fadjs)-1);
        printf("%s ", fadjs[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Adj<Female>, Ns...>()(); break;
            case 1: Word<Conj<Female>, Ns...>()(); break;
            case 2: Word<Conj<Male>, Ns...>()(); break;
            case 3: Word<Conj<Common<Male>>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Conj<Female>, N, Ns...> {
    void operator()() { printf("della "); Word<Name<Proper<Female>>, Ns...>()(); }
};


template<ui N, ui... Ns> struct Word<Conj<Common<Male>>, N, Ns...> {
    void operator()() { printf("del "); Word<Name<Common<Male>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Male>, N, Ns...> {
    void operator()() { printf("di "); Word<Name<Proper<Male>>, Ns...>()(); }
};

template<typename N> struct Word<N> { void operator()() { printf("\n"); } };
template<ui N>struct Word<Conj<Female>, N> { void operator()() { Word<Conj<Female>, N, N>()(); }};
template<ui N>struct Word<Conj<Male>,   N> { void operator()() { Word<Conj<Male>,   N, N>()(); }};
template<ui N>struct Word<Conj<Common<Male>>, N> { void operator()() { Word<Conj<Common<Male>>, N, N>()(); }};
template<ui N>struct Word<Name<Common<Male>>, N> { void operator()() { Word<Name<Common<Male>>, N, N>()(); }};

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
