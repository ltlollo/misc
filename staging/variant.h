#ifndef VARIANT_H
#define VARIANT_H

#include <vector>
#include <tuple>
#include <functional>

/* Scope
 * Provide multi-typed container with the fastest trasversal given
 * ammortized relocation/insertion by-type.
 *
 * Usage (code example)
 * ```
 * MultiVec<int, double, string> v;
 * v.push(1);
 * v.push(2.3);
 * v.push(string("hi"));
 * v.foreach([](auto &val) { cout << val << endl; });
 * v.view<int, double>()
 *  .foreach([](auto &val) { cout << val << endl; });
 * ```
 */

template<typename T, typename... Ts> struct Find;
template<std::size_t N, typename T, typename... Ts> struct Find_helper;
template<std::size_t N> struct Apply_helper;
template<typename T, typename F> static auto constexpr apply(T& tp, F f);
template<typename... Ts> struct Unique;
template<typename... Ts> struct MultiVec;
template<typename... Ts> struct MultiVec_view;

template<typename... Ts> struct MultiVec {
    static_assert(Unique<Ts...>::value, "non unique types");
    std::tuple<std::vector<Ts>...> data;
    template<typename F> auto foreach(F f) {
        return map([=](auto &vec){
            for (auto &ele: vec) {
                f(ele);
            }
        });
    }
    template<typename T> auto push(T&& val) {
        return std::get<Find<T, Ts...>::value>(data).emplace_back(val);
    }
    template<typename T> constexpr auto& get() {
        return std::get<Find<T, Ts...>::value>(data);
    }
    template<typename T> constexpr auto* get_ptr() {
        return &std::get<Find<T, Ts...>::value>(data);
    }
    template<typename... Us> auto view() {
        return MultiVec_view<Us...> { std::make_tuple(get_ptr<Us>()...) };
    }
    template<typename F> constexpr auto map(F f) {
        return apply(data, f);
    }
};

template<typename... Ts> struct MultiVec_view {
    std::tuple<std::vector<Ts>*...> data;
    template<typename F> constexpr auto map(F f) {
        return apply(data, f);
    }
    template<typename F> auto foreach(F f) {
        return map([=](auto &vec){
            for (auto &ele: *vec) {
                f(ele);
            }
        });
    }
};

template<typename T, typename... Ts> struct Find {
    static constexpr size_t value = Find_helper<0, T, Ts...>::value;
    static_assert(value != sizeof...(Ts), "type not in variant");
};

template<std::size_t N, typename T, typename... Ts>
struct Find_helper<N, T, T, Ts...> {
    static constexpr size_t value = N;
};
template<std::size_t N, typename T, typename TT, typename... Ts>
struct Find_helper<N, T, TT, Ts...> {
    static constexpr size_t value = Find_helper<N+1, T, Ts...>::value;
};
template<std::size_t N, typename T> struct Find_helper<N, T> {
    static constexpr size_t value = N;
};

template<typename T, typename F> static auto constexpr apply(T& tp, F f) {
    return Apply_helper<std::tuple_size<T>::value>::call(tp, f);
}

template<std::size_t N> struct Apply_helper {
    template<typename F, typename T> static constexpr auto call(T& tp, F f) {
        auto& vref = std::get<std::tuple_size<T>::value-N>(tp);
        f(vref);
        Apply_helper<N-1>::call(tp, f);
    }
};
template<> struct Apply_helper<1> {
    template<typename F, typename T> static constexpr auto call(T& tp, F f) {
        auto& vref = std::get<std::tuple_size<T>::value-1>(tp);
        f(vref);
    }
};

template<typename T, typename... Ts> struct Unique<T, T, Ts...> {
    static constexpr bool value = false;
};
template<typename T, typename U, typename... Ts> struct Unique<T, U, Ts...> {
    static constexpr bool value =
        Unique<T, Ts...>::value && Unique<U, Ts...>::value;
};
template<typename T> struct Unique<T> {
    static constexpr bool value = true;
};


#endif // VARIANT_H
