#ifndef PIPE_H
#define PIPE_H

#include <algorithm>
#include <type_traits>
#include "extra/fntraits.h"

/* Scope:
 * Tiny pipe/signal lib
 *
 * Example(code):
 *
 * void f() { std::cout << "Hello "; }
 * void g() { std::cout << " world\n"; }
 * int h(int i) { return i+1; }
 * void i(int j) { std::cout << "j is " << j << '\n'; }
 * ...
 * auto sigf = connect(f, g);                       sigf();
 * auto d = sigf.disconnect();                      sigf();
 * sigf.reconnect([](){ std::cout << " what? "; }); sigf();
 * sigf.reconnect(d);                               sigf();
 * auto hh = connect(h, h); {
 * auto hhi = connect(hh, i);
 * auto d = hhi.disconnect();  hhi(3);
 * hhi.reconnect(d);           hhi(3); } {
 * auto hhi = pipe(h) | h | i; hhi(3);
 * }
 * auto hhi = pipe(hh) | i;    hhi(3); pipe(2) | hhi ; pipe(2) | hh | i;
 * auto onekhi = pipe(repeat(1000, h)) | i;         onekhi(1);
 */

template<typename T> struct Compose {
    template<typename G, typename F, typename... Args>
    static constexpr auto call(G g, F f, Args&&... args) {
       return g(f(std::forward<Args>(args)...));
    }
};

template<> struct Compose<void> {
    template<typename G, typename F, typename... Args>
    static constexpr auto call(G g, F f, Args&&... args) {
       f(std::forward<Args>(args)...);
       return g();
    }
};

template<typename F, typename G, typename P> struct Sig;
template<typename F, typename G, typename... Args>
struct Sig<F, G, Pack<Args...>> {
    F f;
    G g;
    using args_t = Pack<Args...>;
    using RetF = ret_of_t<F>;
    using RetG = ret_of_t<G>;
    using ArgsG = args_of_t<G>;
    static_assert(std::is_same<RetF, void>() ||
                  ArgsG::size == 1, "wrong arity");
    constexpr auto operator()(Args&&... args) {
        return Compose<RetF>::call(g, f, std::forward<Args>(args)...);
    }
    constexpr auto disconnect() noexcept {
        auto gc = std::move(g);
        g = MakeFn<RetG, ArgsG>::get();
        return gc;
    }
    constexpr auto reconnect(G val) noexcept {
        g = val;
    }
};

template<typename F, typename G>
static constexpr auto connect(F f, G g) noexcept {
    return Sig<F, G, args_of_t<F>>{ f, g };
}

template<typename F, typename P> struct Con;
template<typename F, typename... Args> struct Con<F, Pack<Args...>> {
    F f;
    template<typename G> auto operator|(G g) noexcept {
        return Con<Sig<F, G, Pack<Args...>>, Pack<Args...>>{
            Sig<F, G, Pack<Args...>>{ f, g } } ;
    }
    constexpr auto operator()(Args&&... args) {
        return f(std::forward<Args>(args)...);
    }
    constexpr auto disconnect() noexcept {
        return f.disconnect();
    }
    template<typename T> constexpr auto reconnect(T t) noexcept {
        return f.reconnect(t);
    }
};

template<template<class...> class M, typename... Ret>
struct Constr {
    template<typename F, typename... Args>
    constexpr static auto make(F f, Args&&... args) {
        return M<Ret...> { f(std::forward<Args>(args)...) };
    }
};

template<template<class...> class M>
struct Constr<M, void> {
    template<typename F, typename... Args>
    constexpr static auto make(F f, Args&&... args) {
        f(std::forward<Args>(args)...);
        return M<void>{};
    }
};

template<typename... T> struct Lazy;
template<typename T> struct Lazy<T> {
    T val;
    constexpr auto operator()() noexcept {
        return val;
    }
    template<typename G> constexpr auto operator|(G g) {
        using Res = decltype(g(std::forward<T>(val)));
        return Constr<Lazy, Res>::make(g, std::forward<T>(val));
    }
};

template<> struct Lazy<void> {
    constexpr auto operator()() noexcept {}
};

template<typename Ret, typename F> struct Chain {
    size_t N;
    F f;
    static_assert(std::is_same<Ret, t_of<Unpack<0, args_of_t<F>>>>(),
                  "f cannot be composed with itself");
    constexpr Ret operator()(Ret in) {
        for (size_t i = 0; i < N; ++i) {
            in = f(in);
        }
        return in;
    }
};
template<typename F> struct Chain<void, F> {
    size_t N;
    F f;
    constexpr void operator()() {
        for (size_t i = 0; i < N; ++i) {
            f();
        }
    }
};

template<bool, typename... T> struct Pipe;
template<typename T> struct Pipe<true, T> {
    template<typename F> constexpr auto operator()(F f) noexcept {
        return Con<F, args_of_t<F>>{ f };
    }
};
template<typename T> struct Pipe<false, T> {
    template<typename F> constexpr auto operator()(F f) noexcept {
        return Lazy<F>{ f };
    }
};

template<typename F> constexpr auto pipe(F arg) noexcept {
    return Pipe<is_callable<F>::value, F>{}(arg);
}

/*TODO: remove one*/
template<typename F> constexpr static auto repeat(size_t n, F f) noexcept {
    return Chain<ret_of_t<F>, F>{ n, f };
}
template<typename F> constexpr static auto repeat(F f, size_t n) noexcept {
    return repeat(n, f);
}

#endif // PIPE_H
