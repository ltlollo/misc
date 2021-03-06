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

namespace Either {
    struct Left{};
    struct Right{};
    template<bool> struct FromBool;
    template<> struct FromBool<true> { static Either::Left value; };
    template<> struct FromBool<false> { static Either::Right value; };
    template<bool b> static constexpr auto from_bool() {
        return FromBool<b>::value;
    }
}

template<template<typename...> typename M1,
         template<typename...> typename... Ms1>
struct EitherT {
    template<template<typename...> typename M2,
    template<typename...> typename... Ms2>
    struct OrT {
        template<typename T, typename... Ts>
        constexpr static auto match_fst(Either::Right, T t, Ts... ts) {
            return M2<T, Ms2<T>...>{ t,  ts... };
        }
        template<typename T, typename... Ts>
        constexpr static auto match_fst(Either::Left, T t, Ts... ts) {
            return M1<T, Ms1<T>...>{ t,  ts... };
        }
        template<typename... Ts>
        constexpr static auto match(Either::Right, Ts... ts) {
            return M2<Ts..., Ms2<Ts...>...>{ ts... };
        }
        template<typename... Ts>
        constexpr static auto match(Either::Left, Ts... ts) {
            return M1<Ts..., Ms1<Ts...>...>{ ts... };
        }
    };
    template<typename T> struct Or {
        template<typename... Ts>
        constexpr static auto match(Either::Right, Ts... ts) {
            return T { ts... };
        }
        template<typename... Ts>
        constexpr static auto match(Either::Left, Ts... ts) {
            return M1<T, Ms1<T>...>{ ts... };
        }
    };
};

template<template<typename...> typename M1, template<typename...> typename M2>
struct FuseT { template<typename T> using typeT = M1<M2<T>>; };

template<template<typename...> typename M, typename... Ret>
struct ConstrT {
    template<typename F, typename... Args>
    constexpr static auto make(F f, Args&&... args) {
        return M<Ret...>{ f(std::forward<Args>(args)...) };
    }
};

template<template<typename...> typename M>
struct ConstrT<M, void> {
    template<typename F, typename... Args>
    constexpr static auto make(F f, Args&&... args) {
        f(std::forward<Args>(args)...);
        return M<void>{};
    }
};

template<typename T> struct Stream {
    T val;
    constexpr auto operator()() noexcept {
        return val;
    }
    template<typename G> constexpr auto operator|(G g) {
        using Res = decltype(g(std::forward<T>(val)));
        return ConstrT<Stream, Res>::make(g, std::forward<T>(val));
    }
    constexpr operator T() {
        return val;
    }
};

template<> struct Stream<void> {
    constexpr auto operator()() noexcept {}
    template<typename G> constexpr auto operator|(G g) {
        g();
        return *this;
    }
};

template<typename F, typename G, typename P> struct Con;
template<typename F, typename G, typename... Args>
struct Con<F, G, Pack<Args...>> {
    F f;
    G g;
    using args_t = Pack<Args...>;
    using RetF = ret_of_t<F>;
    using RetG = ret_of_t<G>;
    using ArgsG = args_of_t<G>;
    static_assert(std::is_same<RetF, void>() ||
                  ArgsG::size == 1, "wrong arity");
    constexpr auto operator()(Args&&... args) {
        return ConstrT<Stream, RetF>::make(f, std::forward<Args>(args)...) | g;
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
    return Con<F, G, args_of_t<F>>{ f, g };
}

template<typename F, typename P> struct Chan;
template<typename F, typename... Args> struct Chan<F, Pack<Args...>> {
    F f;
    template<typename G> constexpr auto operator|(G g) noexcept {
        return Chan<Con<F, G, Pack<Args...>>, Pack<Args...>>{
            Con<F, G, Pack<Args...>>{ f, g }
        };
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

template<typename F, typename Ret> struct Chain {
    F f;
    size_t N;
    static_assert(std::is_same<Ret, t_of<Unpack<0, args_of_t<F>>>>(),
                  "f cannot be composed with itself");
    constexpr Ret operator()(Ret in) {
        for (size_t i = 0; i < N; ++i) {
            in = f(in);
        }
        return in;
    }
};

template<typename F> struct Chain<F, Stream<void>> {
    F f;
    size_t N;
    constexpr void operator()() {
        for (size_t i = 0; i < N; ++i) {
            f();
        }
    }
};

template<typename F> constexpr static auto pipe(F arg) noexcept {
    return EitherT<Chan, args_of_t>::OrT<Stream>
        ::match_fst(Either::from_bool<is_callable<F>::value>(), arg);
}

/*TODO: remove one*/
template<typename F> constexpr static auto repeat(size_t n, F f) noexcept {
    constexpr bool ret_void = std::is_void<ret_of_t<F>>::value;
    return EitherT<Chain, FuseT<Stream, ret_of_t>::typeT>::OrT<Chain, ret_of_t>
        ::match_fst(Either::from_bool<ret_void>(), f, n);
}
template<typename F> constexpr static auto repeat(F f, size_t n) noexcept {
    return repeat(n, f);
}

#endif // PIPE_H
