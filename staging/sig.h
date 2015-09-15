#ifndef PIPE_H
#define PIPE_H

#include <algorithm>
#include <type_traits>
#include "extra/fntraits.h"

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
    static constexpr std::size_t Garity = Function<G>::args_t::size;
    using RetF = typename Function<F>::return_t;
    static_assert(std::is_same<RetF, void>() || Garity == 1, "wrong arity");
    constexpr auto operator()(Args&&... args) {
        return Compose<RetF>::call(g, f, std::forward<Args>(args)...);
    }
};

template<typename F, typename G>
static constexpr auto connect(F f, G g) noexcept {
    return Sig<F, G, typename Function<F>::args_t>{ f, g };
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
};

template<typename T> struct Lazy {
    T val;
    constexpr auto operator()() noexcept {
        return val;
    }
    template<typename G> constexpr auto operator|(G g) {
        return g(val);
    }
};

template<typename... T> struct is_sig {
    static constexpr bool value = false;
};
template<typename... T> struct is_sig<Sig<T...>> {
    static constexpr bool value = true;
};

template<bool> struct Pipe;
template<> struct Pipe<true> {
    template<typename F> constexpr auto operator()(F f) {
        return Con<F, typename Function<F>::args_t>{ f };
    }
};

template<> struct Pipe<false> {
    template<typename F> constexpr auto operator()(F f) {
        return Lazy<F>{ f };
    }
};

template<typename F> constexpr auto pipe(F arg) {
    return Pipe<is_callable<F>::value || is_sig<F>::value>{}(arg);
}

#endif // PIPE_H
