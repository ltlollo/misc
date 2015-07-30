// cat cane.cpp | transcat -d /usr/share/dict/italian | g++ -std=c++14 -x c++ - -o cane

#include <random>

#define d(...) default: Word<__VA_ARGS__>{}(); break
#define c(pos,...) case pos: Word<__VA_ARGS__>{}(); break
#define pc(pos, name) case pos: printf("%s ", name); break
#define rand std::uniform_int_distribution<>
#define base(case) \
template<>\
struct Word<0, case> {\
    void operator()(){ printf("\n"); }\
}
#define cont(case) \
template<>\
struct Word<1, case> {\
    void operator()() {\
        Word<2, case>()();\
    }\
}

using namespace std;
using ui = size_t;

std::random_device rd;
std::mt19937 gen(rd());

struct Root{};
template<typename T> struct Male{};
template<typename T> struct Female{};
template<typename T> struct Animal{};
struct Name{};
struct Adj{};
struct Conj{};


template<ui N, typename T> struct Word {};

template<ui N> struct Word<N, Root> {
    void operator()() {
        rand dist(0,1);
        switch(dist(gen)) {
            c(0, N, Male<Name>);
            c(1, N, Female<Name>);
        }
    }
};

template<ui N> struct Word<N, Male<Name>> {
    void operator()() {
        rand cdist(0, 1);
        switch(cdist(gen)) {
            pc(0, "@g@e@102369");
            pc(1, "@33081");
        }
        rand ndist(0, 5);
        switch(ndist(gen)) {
            d(N-1, Male<Adj>);
            c(1, N-1, Male<Conj>);
            c(2, N-1, Female<Conj>);
            c(3, N-1, Animal<Conj>);
            c(4, N-1, Animal<Name>);
        }
    }
};

template<ui N> struct Word<N, Animal<Name>> {
    void operator()() {
        rand cdist(0, 2);
        switch(cdist(gen)) {
            pc(0, "@68519");
            pc(1, "@17969");
            pc(2, "@56642");
        }
        rand ndist(0, 4);
        switch(ndist(gen)) {
            d(N-1, Male<Adj>);
            c(1, N-1, Male<Conj>);
            c(2, N-1, Female<Conj>);
            c(3, N-1, Animal<Conj>);
        }
    }
};

template<ui N> struct Word<N, Female<Name>> {
    void operator()() {
        rand dist(0, 1);
        switch(dist(gen)) {
            pc(0, "@56558");
            pc(1, "@57352@m@m@a");
        }
        rand ndist(0, 4);
        switch(ndist(gen)) {
            d(N-1, Female<Adj>);
            c(1, N-1, Male<Conj>);
            c(2, N-1, Animal<Conj>);
            c(3, N-1, Female<Conj>);
        }
    }
};

template<ui N> struct Word<N, Male<Adj>> {
    void operator()() {
        rand cdist(0, 4);
        switch(cdist(gen)) {
            pc(0, "@102660");
            pc(1, "@49088");
            pc(2, "@b@e@d@u@i@61568");
            pc(3, "@16904");
            pc(4, "@54400");
        }
        rand ndist(0, 3);
        switch(ndist(gen)) {
            d(N-1, Male<Adj>);
            c(1, N-1, Male<Conj>);
            c(2, N-1, Female<Conj>);
        }
    }
};

template<ui N> struct Word<N, Female<Adj>> {
    void operator()() {
        rand cdist(0, 10);
        switch(cdist(gen)) {
            pc(0, "@73131");
            pc(1, "@49086");
            pc(2, "@54397");
            pc(3, "@102658");
            pc(4, "@68507");
            pc(5, "@111224");
            pc(6, "@56641@6023");
            pc(7, "@10253");
            pc(8, "@i@m@67223");
            pc(9, "@115802");
            pc(10, "@114636");
        }
        rand ndist(0, 4);
        switch(ndist(gen)) {
            d(N-1, Female<Adj>);
            c(1, N-1, Female<Conj>);
            c(2, N-1, Male<Conj>);
            c(3, N-1, Animal<Conj>);
        }
    }
};

template<ui N> struct Word<N, Female<Conj>> {
    void operator()() {
        printf("@30473 ");
        Word<N-1, Female<Name>>()();
    }
};


template<ui N> struct Word<N, Animal<Conj>> {
    void operator()() {
        printf("@29078 ");
        Word<N-1, Animal<Name>>()();
    }
};

template<ui N> struct Word<N, Male<Conj>> {
    void operator()() {
        printf("@31807 ");
        Word<N-1, Male<Name>>()();
    }
};

cont(Female<Conj>);
cont(Male<Conj>);
cont(Animal<Conj>);
cont(Animal<Name>);

base(Male<Adj>);
base(Male<Name>);
base(Female<Name>);
base(Female<Adj>);
base(Animal<Name>);

base(Female<Conj>);
base(Animal<Conj>);
base(Male<Conj>);

int main(int argc, char *argv[]) {
    Word<10, Root>()();
    return 0;
}

