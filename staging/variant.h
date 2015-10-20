#ifndef VARIANT_H
#define VARIANT_H

#include <vector>
#include <tuple>
#include <cstddef>

template<std::size_t N, typename T, typename... Ts> struct Adder;
template<std::size_t N, typename T, typename... Ts>
struct Adder<N, T, T, Ts...> {
    template<typename V, typename U> static auto add(V& tp, U&& val) {
        std::get<N>(tp).emplace_back(std::forward<U>(val));
    }
};
template<std::size_t N, typename T, typename TT, typename... Ts>
struct Adder<N, T, TT, Ts...> {
    template<typename V, typename U> static auto add(V& tp, U&& val) {
        Adder<N+1, T, Ts...>::add(tp, std::forward<U>(val));
    }
};

template<std::size_t N, std::size_t Size> struct Apply {
    template<typename F, typename T> static auto call(T& tp, F f) {
        auto& vref = std::get<N>(tp);
        for (auto& ele: vref) {
            f(ele);
        }
        Apply<N+1, Size>::call(tp, f);
    }
};
template<std::size_t N> struct Apply<N, N> {
    template<typename F, typename T> static auto call(T&, F) {
    }
};

template<typename... Ts> struct Unique;
template<typename T, typename... Ts> struct Unique<T, T, Ts...> {
    static constexpr bool value = false;
};
template<typename T, typename U, typename... Ts> struct Unique<T, U, Ts...>{
    static constexpr bool value =
        Unique<T, Ts...>::value && Unique<U, Ts...>::value;
};
template<typename T> struct Unique<T>{
    static constexpr bool value = true;
};

template<typename... Ts>
struct vec_variant {
    static_assert(Unique<Ts...>::value, "non unique types");
    std::tuple<std::vector<Ts>...> data;
    template<typename F> auto foreach(F f) {
        Apply<0, sizeof...(Ts)>::call(data, f);
    }
    template<typename T> auto push(T&& val) {
        Adder<0, T, Ts...>::add(data, std::forward<T>(val));
    }

};

#endif // VARIANT_H
