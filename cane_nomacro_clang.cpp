#include <random>

using namespace std;
using ui = size_t;

std::random_device rd;
std::mt19937 gen(rd());

template<unsigned N, unsigned M> constexpr unsigned size_of(const char (&arr)[N][M]) {
        return N;
}

struct Root{};
struct Male{};
struct Female{};

template<typename T> struct oppGender{};
template<> struct oppGender<Male> { using type = Female; };
template<> struct oppGender<Female> { using type = Male; };

template<typename T> using oG = typename oppGender<T>::type;
template<typename T> struct Proper{ using sex = T; };
template<typename T> struct Common{ using sex = T; };
template<typename T> struct Name{};
template<typename T> struct Adj{};
template<typename T> struct Conj{};

template<typename T> struct oppGender<Proper<T>>{ using type = Proper<oG<T>>; };
template<typename T> struct oppGender<Common<T>>{ using type = Common<oG<T>>; };
template<typename T> struct oppType{};
template<typename T> struct oppType<Proper<T>> { using type = Common<T>; };
template<typename T> struct oppType<Common<T>> { using type = Proper<T>; };
template<typename T> using oT = typename oppType<T>::type;
template<typename T> using sex = typename T::sex;

char pmnames[][6]{
    "Gesù"
    , "Dio"
};
char cmnames[][10]{
    "cane"
    , "porco"
    , "maiale"
    , "cinghiale"
    , "becco"
};
char pfnames[][8]{
    "Madonna"
    , "Maremma"
};
char madjs[][11]{
    "sudicio"
    , "infiammato"
    , "beduino"
    , "buco"
    , "laido"
};
char fadjs[][11]{
    "infiammata"
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
    , "schifosa"
    , "lezza"
};
char cfnames[][11]{
    "puttana"
    , "porca"
    , "troia"
    , "maiala"
    , "vipera"
    , "velenosa"
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
        std::uniform_int_distribution<> ndist(0, 6);
        switch(ndist(gen)) {
            default: Word<Adj<sex<T>>, Ns...>()(); break;
            case 1: Word<Conj<T>, Ns...>()(); break;
            case 2: Word<Conj<oG<T>>, Ns...>()(); break;
            case 3: Word<Conj<oT<T>>, Ns...>()(); break;
            case 4: Word<Name<Common<sex<T>>>, Ns...>()(); break;
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
            case 2: Word<Conj<Proper<oG<T>>>, Ns...>()(); break;
            case 3: Word<Conj<Common<oG<T>>>, Ns...>()(); break;

        }
    }
};
template<ui N, ui... Ns> struct Word<Conj<Proper<Female>>, N, Ns...> {
    void operator()() { printf("della "); Word<Name<Proper<Female>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Common<Male>>, N, Ns...> {
    void operator()() { printf("del "); Word<Name<Common<Male>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Common<Female>>, N, Ns...> {
    void operator()() { printf("della "); Word<Name<Common<Female>>, Ns...>()(); }
};

template<ui N, ui... Ns> struct Word<Conj<Proper<Male>>, N, Ns...> {
    void operator()() { printf("di "); Word<Name<Proper<Male>>, Ns...>()(); }
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
