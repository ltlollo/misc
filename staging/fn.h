#ifndef FN_H
#define FN_H

#include <utility>

template<typename R, typename... As> using fn_t = R(*)(As...);
template<typename T> struct Fn : public Fn<decltype(&T::operator())> {};
template<typename T, typename R>
struct Fn<R(T::*)()const> { using ptr_t = fn_t<R>; };
template<typename T, typename R, typename... As>
struct Fn<R(T::*)(const As&...)const> { using ptr_t = fn_t<R, const As&...>; };
template<typename T, typename R, typename... As>
struct Fn<R(T::*)(const As...)const> { using ptr_t = fn_t<R, const As...>; };
template<typename T, typename R, typename... As>
struct Fn<R(T::*)(const As&&...)const> { using ptr_t = fn_t<R, const As&&...>; };
template<typename R> struct Fn<R(*)()> { using ptr_t = fn_t<R>; };
template<typename R, typename... As>
struct Fn<R(*)(const As&...)> { using ptr_t = fn_t<R, const As&...>; };
template<typename R, typename... As>
struct Fn<R(*)(const As...)> { using ptr_t = fn_t<R, const As...>; };
template<typename R, typename... As>
struct Fn<R(*)(const As&&...)> { using ptr_t = fn_t<R, const As&&...>; };
template<typename C>
struct is_pure {
    template<typename T> static typename Fn<T>::ptr_t has(T);
    template<typename T> static int test(decltype(has(std::declval<C>())));
    template<typename> static bool test(...);
    static constexpr bool value = std::is_same<decltype(test<C>(std::declval<C>())), int>();
};
template<typename T> constexpr bool is_pure_v = is_pure<T>::value;

#endif // FN_H
