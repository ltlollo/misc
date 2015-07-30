// cat cane_nomacro_clang.cpp | transcat -d /usr/share/dict/italian | g++ -std=c++14 -x c++ - -o cane_nomacro_clang

#include <random>

using namespace std;
using ui = size_t;

std::random_device rd;
std::mt19937 gen(rd());

template<unsigned N, unsigned M>
constexpr unsigned size_of(const char (&arr)[N][M]) {
        return N;
}

struct Root{};
struct Male{};
struct Female{};
template<typename T> using sex_of = typename T::sex;
template<typename T> struct Gender{ using sex = T; };
template<typename T> struct Proper : Gender<T> {};
template<typename T> struct Common : Gender<T> {};
template<typename T> struct Adj : Gender<T> {};
template<typename T> struct Name : T {};
template<typename T> struct Conj : T {};
template<typename T> struct FlipSex{};
template<> struct FlipSex<Male> : Gender<Female> {};
template<> struct FlipSex<Female> : Gender<Male> {};
template<typename T> using flip_sex_of = sex_of<FlipSex<T>>;
template<typename T> struct FlipSex<Proper<T>>{ using sex = Proper<flip_sex_of<T>>; };
template<typename T> struct FlipSex<Common<T>>{ using sex = Common<flip_sex_of<T>>; };
template<typename T> struct FlipType{};
template<typename T> struct FlipType<Proper<T>> { using type = Common<T>; };
template<typename T> struct FlipType<Common<T>> { using type = Proper<T>; };
template<typename T> using flip_type_of = typename FlipType<T>::type;
template<typename T> using flip_any_of = flip_type_of<flip_sex_of<T>>;

char pmnames[][6]{
    "@g@e@102369"
    , "@33081"
};
char cmnames[][10]{
    "@17969"
    , "@68519"
    , "@56642"
    , "@19877"
    , "@15533"
};
char pfnames[][8]{
    "@56558"
    , "@57352@m@m@a"
};
char madjs[][11]{
    "@102660"
    , "@49088"
    , "@b@e@d@u@i@61568"
    , "@16904"
    , "@54400"
    , "@15296"
};
char fadjs[][11]{
    "@49086"
    , "@54397"
    , "@102658"
    , "@68507"
    , "@111224"
    , "@56641@6023"
    , "@10253"
    , "@i@m@67223"
    , "@115802"
    , "@114636"
    , "@54367"
    , "@86884"
    , "@54997@z@z@a"
};
char cfnames[][11]{
    "@73131"
    , "@68507"
    , "@111224"
    , "@56641@6023"
    , "@115802"
    , "@114636"
};



template<typename T> struct Data{};
template<> struct Data<Name<Proper<Male>>>   {
    static constexpr auto words = pmnames; static
    constexpr ui size = size_of(pmnames);
};
template<> struct Data<Name<Proper<Female>>> {
    static constexpr auto words = pfnames;
    static constexpr ui size = size_of(pfnames);
};
template<> struct Data<Name<Common<Male>>>   {
    static constexpr auto words = cmnames;
    static constexpr ui size = size_of(cmnames);
};
template<> struct Data<Name<Common<Female>>> {
    static constexpr auto words = cfnames;
    static constexpr ui size = size_of(cfnames);
};
template<> struct Data<Adj<Male>>            {
    static constexpr auto words = madjs;
    static   constexpr ui size = size_of(madjs);
};
template<> struct Data<Adj<Female>>          {
    static constexpr auto words = fadjs;
    static   constexpr ui size = size_of(fadjs);
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

template<typename T, ui N, ui... Ns> struct Word<Name<T>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, Data<Name<T>>::size-1);
        printf("%s ", Data<Name<T>>::words[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 7);
        switch(ndist(gen)) {
            default: Word<Adj<sex_of<T>>, Ns...>()(); break;
            case 1: Word<Conj<T>, Ns...>()(); break;
            case 2: Word<Conj<flip_sex_of<T>>, Ns...>()(); break;
            case 3: Word<Conj<flip_type_of<T>>, Ns...>()(); break;
            case 4: Word<Conj<flip_any_of<T>>, Ns...>()(); break;
            case 5: Word<Name<Common<sex_of<T>>>, Ns...>()(); break;
        }
    }
};

template<typename T, ui N, ui... Ns> struct Word<Adj<T>, N, Ns...> {
    void operator()() {
        std::uniform_int_distribution<> cdist(0, Data<Adj<T>>::size-1);
        printf("%s ", Data<Adj<T>>::words[cdist(gen)]);
        std::uniform_int_distribution<> ndist(0, 3);
        switch(ndist(gen)) {
            default: Word<Adj<T>, Ns...>()(); break;
            case 1: Word<Conj<Proper<T>>, Ns...>()(); break;
            case 2: Word<Conj<Proper<flip_sex_of<T>>>, Ns...>()(); break;
            case 3: Word<Conj<Common<flip_sex_of<T>>>, Ns...>()(); break;

        }
    }
};
template<ui N, ui... Ns> struct Word<Conj<Proper<Female>>, N, Ns...> {
    void operator()() { printf("@30473 "); Word<Name<Proper<Female>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Common<Male>>, N, Ns...> {
    void operator()() { printf("@29078 "); Word<Name<Common<Male>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Common<Female>>, N, Ns...> {
    void operator()() { printf("@30473 "); Word<Name<Common<Female>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Proper<Male>>, N, Ns...> {
    void operator()() { printf("@31807 "); Word<Name<Proper<Male>>, Ns...>()(); }
};

template<typename N> struct Word<N>{
    void operator()() { printf("\n"); }
};

template<ui N> struct Word<Conj<Common<Male>>, N> {
    void operator()() { Word<Conj<Common<Male>>, N, N>()(); }
};
template<ui N> struct Word<Conj<Common<Female>>, N> {
    void operator()() { Word<Conj<Common<Female>>, N, N>()(); }
};
template<ui N> struct Word<Conj<Proper<Male>>, N> {
    void operator()() { Word<Conj<Proper<Male>>, N, N>()(); }
};
template<ui N> struct Word<Conj<Proper<Female>>, N> {
    void operator()() { Word<Conj<Proper<Female>>, N, N>()(); }
};
template<typename T, ui N>struct Word<Name<Common<T>>,   N> {
    void operator()() { Word<Name<Common<T>>, N, N>()(); }
};

template<ui... Ns> struct Num {};
template<ui N, typename T> struct GenGraph {
    template<ui... Ns> void operator()(Num<Ns...> n){ GenGraph<N-1, T>()(Num<Ns..., N>()); }
};

template<typename T> struct GenGraph<0, T> {
    template<ui... Ns> void operator()(Num<Ns...> n){ Word<T, Ns...>()(); }
};

template<ui N> void Graph() { GenGraph<N, Root>()(Num<>()); }

int main(int argc, char *argv[]) {
    Graph<10>();
    return 0;
}
