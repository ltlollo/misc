#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <random>
#define ty typename
#define pc(pos, name) case pos: printf("%s ", name); break
#define rand std::uniform_int_distribution<>
#define drand(dist, arr) \
std::uniform_int_distribution<> dist(0, sizeof(arr)/sizeof(arr[0])-1)

#define cont(case) \
template<ui N>\
struct Word<case, N> {\
    void operator()() {\
        Word<case, N, N>()();\
    }\
}

using namespace std;
using ui = size_t;

std::random_device rd;
std::mt19937 gen(rd());

struct Root{};
template<ty T> struct Male{};
template<ty T> struct Female{};
template<ty T> struct Animal{};
struct Name{};
struct Adj{};
struct Conj{};

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
};

template<typename T, ui... Ns> struct Word {};

template<ui... Ns> struct Word<Root, Ns...>{
    void operator()() {
        rand dist(0,1);
        switch(dist(gen)) {
            case 0: Word<Male<Name>, Ns...>()(); break;
            case 1: Word<Female<Name>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Male<Name>, N, Ns...> {
    void operator()() {
        drand(cdist, mnames);
        printf("%s ", mnames[cdist(gen)]);
        rand ndist(0, 5);
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
        drand(cdist, anames);
        printf("%s ", anames[cdist(gen)]);
        rand ndist(0, 4);
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
        drand(cdist, fnames);
        printf("%s ", fnames[cdist(gen)]);
        rand ndist(0, 4);
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
        drand(cdist, madj);
        printf("%s ", madj[cdist(gen)]);
        rand ndist(0, 3);
        switch(ndist(gen)) {
            default: Word<Male<Adj>, Ns...>()(); break;
            case 1: Word<Male<Conj>, Ns...>()(); break;
            case 2: Word<Female<Conj>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Female<Adj>, N, Ns...> {
    void operator()() {
        drand(cdist, fadjs);
        printf("%s ", fadjs[cdist(gen)]);
        rand ndist(0, 4);
        switch(ndist(gen)) {
            default: Word<Female<Adj>, Ns...>()(); break;
            case 1: Word<Female<Conj>, Ns...>()(); break;
            case 2: Word<Male<Conj>, Ns...>()(); break;
            case 3: Word<Animal<Conj>, Ns...>()(); break;
        }
    }
};

template<ui N, ui... Ns> struct Word<Female<Conj>, N, Ns...> {
    void operator()() {
        printf("della ");
        Word<Female<Name>, Ns...>()();
    }
};


template<ui N, ui... Ns> struct Word<Animal<Conj>, N, Ns...> {
    void operator()() {
        printf("del ");
        Word<Animal<Name>, Ns...>()();
    }
};

template<ui N, ui... Ns> struct Word<Male<Conj>, N, Ns...> {
    void operator()() {
        printf("di ");
        Word<Male<Name>, Ns...>()();
    }
};

template<ty N> struct Word<N> {
    void operator()() {
        printf("\n");
    }
};

cont(Female<Conj>);
cont(Male<Conj>);
cont(Animal<Conj>);
cont(Animal<Name>);

template<ui... Ns> struct Num {};
template<ui N, typename T> struct GenGraph {
    template<ui... Ns> void operator()(Num<Ns...> n){
        return GenGraph<N-1, T>()(Num<Ns..., N>());
    }
};

template<typename T> struct GenGraph<0, T> {
    template<ui... Ns> void operator()(Num<Ns...> n){
        return Word<T, Ns...>()();
    }
};

template<ui N> void Graph() {
    GenGraph<N, Root>()(Num<>());
}

int main(int argc, char *argv[]) {
    Graph<100>();
    return 0;
}

