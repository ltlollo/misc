#include <vector>
#include <stdexcept>
#include <string>
#include <iostream>
#include <fstream>
#include <thread>
#include <algorithm>
#include <tuple>
#include <pthread.h>
#include <unistd.h>

using namespace std;

template<size_t...> struct Seq{};
template<size_t N, size_t... Ns> struct GenSeq : GenSeq<N-1, N-1, Ns...>{};
template<size_t... Ns> struct GenSeq<0, Ns...>{ using type = Seq<Ns...>; };
template<size_t N> using make_seq_t = typename GenSeq<N>::type;
template<typename Ret, typename... Args> using fn_t = Ret(*)(Args...);
template<typename T, size_t... N> const constexpr void caller(T* t) {
    t->call(Seq<N...>{});
}

template<typename T> struct Function : public Function<decltype(&T::operator())>{};
template<typename T, typename Ret, typename... Args>
struct Function<Ret(T::*)(Args...) const> {
    using return_t = Ret;
    using ptr_t = fn_t<Ret, Args...>;
};

template<typename R, typename F, typename... Args>
struct FunStore {
    F f;
    std::tuple<Args...> args;
    pthread_t t;

    R operator()() {
        return call(make_seq_t<sizeof...(Args)>{});
    }
    template<size_t... Ns> R call(Seq<Ns...>) {
        return f(std::get<Ns>(args)...);
    }
    template<size_t... Ns>
    void call_async(Seq<Ns...>) {
        pthread_create(&t, nullptr,
            (fn_t<void*, void*>)caller<FunStore<R, F, Args...>, Ns...>,
            (void*)this);
    }
    void async() {
        return call_async(make_seq_t<sizeof...(Args)>{});
    }
    void join() {
        pthread_join(t, nullptr);
    }
};

template<typename F, typename... Args>
const constexpr auto make_function(F f, Args... args) {
    using fun_t = typename Function<decltype(f)>::ptr_t;
    using ret_t = typename Function<decltype(f)>::return_t;
    return FunStore<ret_t, fun_t, Args...>{(fun_t)f, make_tuple(args...)};
}

struct Some { string s; void print() {  for(;;) cout << s << endl; } };

int main(int argc, char *argv[]) {
    Some a1{"first"}, a2{"second"}, a3{"third"};
    auto f = [](Some& i) { i.print(); };
    auto t1 = make_function(f, a1);
    auto t2 = make_function(f, a2);
    auto t3 = make_function(f, a3);

    t1.async();
    t2.async();
    t3.async();

    t1.join();

    return 0;
}

