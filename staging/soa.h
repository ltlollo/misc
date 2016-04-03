#ifndef SOA_H
#define SOA_H

#include <tuple>

template <std::size_t I> struct Apply {
    template <std::size_t N, typename T, typename... Ts,
              typename Tp = std::tuple<Ts...>>
    static constexpr auto call(T &tp, const Tp (&arr)[N]) {
        auto &vref = std::get<std::tuple_size<T>::value - I>(tp);
        for (std::size_t i = 0; i != N; ++i) {
            vref[i] = std::get<std::tuple_size<T>::value - I>(arr[i]);
        }
        Apply<I - 1>::call(tp, arr);
    }
};
template <> struct Apply<1> {
    template <std::size_t N, typename T, typename... Ts,
              typename Tp = std::tuple<Ts...>>
    static constexpr auto call(T &tp, const Tp (&arr)[N]) {
        auto &vref = std::get<std::tuple_size<T>::value - 1>(tp);
        for (std::size_t i = 0; i != N; ++i) {
            vref[i] = std::get<std::tuple_size<T>::value - 1>(arr[i]);
        }
    }
};
template <typename T, typename U> constexpr auto polupate(T &tp, const U &ts) {
    return Apply<std::tuple_size<T>::value>::call(tp, ts);
}

template <std::size_t N, typename... Ts> struct Soa {
    std::tuple<Ts[N]...> data;
};
template <typename T> struct WrapT { using type = T; };
template <std::size_t N, typename... Ts>
auto soaT(const std::tuple<Ts...> (&)[N]) {
    return WrapT<std::tuple<Ts[N]...>>{};
}

template <typename T>
using soa_t = typename decltype(soaT(std::declval<T>()))::type;

// #include <iostream>
//
// int main() {
//     constexpr auto v = std::make_tuple("hallo", "world");
//     decltype(v) aos[] = {v, v, v, v, v};
//     soa_t<decltype(aos)> data;
//     polupate(data, aos);
//     for (const auto &e : std::get<0>(data)) {
//         std::cout << e << std::endl;
//     }
//     for (const auto &e : std::get<1>(data)) {
//         std::cout << e << std::endl;
//     }
//     return 0;
// }

#endif // SOA_H
