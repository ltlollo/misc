#ifndef IOBUFFER_H
#define IOBUFFER_H

#include <utility>

/* Scope
 *
 * Given a configuration parameter `Conf<type, size>`, a group constructor
 * function, and an optional group destruction function, the `make_iobuffer`
 * function provides a stack allocated buffer of `size` elements of `type`.
 * Errors must be reported returing a non zero value in the group construtor;
 * if they occur, contruction gets stopped at the current element; it's
 * position gets returned with error (construction can be resumed via the first
 * optional paramenter of the `group_cnstr` member function).
 * Group destruction must not fail and has to be called when done.
 * All exceptions are fatal.
 * `init` and `cycle` methods are provided in order to create the fist and the
 * next batch of elements, destructors are called and errors forwarded if any.
 *
 * Usage (code example)
 * ```
 *   auto show = [](const auto& buf) { for (const auto& i: buf.storage) {
 *       cout << i << '\n';
 *   }};
 *   int s = 0;
 *   auto buf = make_iobuffer(Conf<int, 1024>{}, [&](auto& val) { val = s++; });
 *   buf.init();
 *   show(buf);
 *   buf.cycle();
 *   show(buf);
 * ```
 * Note: This version uses the type in the Conf instead of infering it via
 * funtion traits. The main advantage of this approach is to accept polymophic/
 * overloaded lambdas(auto& ...). The other approach would allow to not specify
 * the type in Conf, and to use T* as proxy type instead of just T&. But this
 * version is more composable.
 */

template<typename T, std::size_t N>
struct Conf {
    using Type = T;
    static constexpr std::size_t size = N;
};

template<typename T>
struct Err {
    T err;
    std::size_t pos;
};

template<typename Cfg, typename Cn, typename Ds, typename Ret>
struct IOBuffer {
    static constexpr std::size_t size = Cfg::size;
    using Ele = typename Cfg::Type;
    Ele storage[size]{};
    Cn cnstr;
    Ds destr;
    Ret err;
    constexpr IOBuffer(Cn c, Ds d) noexcept : cnstr{c}, destr{d} {
    }
    auto group_cnstr(const std::size_t pos = 0) noexcept {
        for (std::size_t i = pos; i < size; ++i) {
            if ((err = cnstr(storage[i]))) {
                return Err<Ret>{ err, i };
            }
        }
        return Err<Ret>{ {}, size };
    }
    auto group_destr() noexcept {
        for (std::size_t i = 0; i < size; ++i) {
            destr(storage[i]);
        }
    }
    auto init() noexcept {
        return group_cnstr();
    }
    auto cycle() noexcept {
        group_destr();
        return group_cnstr();
    }
};

template<typename Cfg, typename Cn, typename Ds>
struct IOBuffer<Cfg, Cn, Ds, void> {
    static constexpr std::size_t size = Cfg::size;
    using Ele = typename Cfg::Type;
    Ele storage[size]{};
    Cn cnstr;
    Ds destr;
    constexpr IOBuffer(Cn c, Ds d) noexcept : cnstr{c}, destr{d} {
    }
    auto group_cnstr(const std::size_t pos = 0) noexcept {
        for (std::size_t i = pos; i < size; ++i) {
            cnstr(storage[i]);
        }
    }
    auto group_destr() noexcept {
        for (std::size_t i = 0; i < size; ++i) {
            destr(storage[i]);
        }
    }
    auto init() noexcept {
        return group_cnstr();
    }
    auto cycle() noexcept {
        group_destr();
        return group_cnstr();
    }
};

template<typename Cfg, typename Cn, typename Ret>
struct IOBuffer<Cfg, Cn, void, Ret> {
    static constexpr std::size_t size = Cfg::size;
    using Ele = typename Cfg::Type;
    Ele storage[size];
    Cn cnstr;
    Ret err;
    constexpr IOBuffer(Cn c) noexcept : cnstr{c} {
    }
    auto group_cnstr(const std::size_t pos = 0) noexcept {
        for (std::size_t i = pos; i < size; ++i) {
            if ((err = cnstr(storage[i]))) {
                return Err<Ret>{ err, i };
            }
        }
        return Err<Ret>{ {}, size };
    }
    auto init() noexcept {
        return group_cnstr();
    }
    auto cycle() noexcept {
        return group_cnstr();
    }
};

template<typename Cfg, typename Cn>
struct IOBuffer<Cfg, Cn, void, void> {
    static constexpr std::size_t size = Cfg::size;
    using Ele = typename Cfg::Type;
    Ele storage[size];
    Cn cnstr;
    constexpr IOBuffer(Cn c) noexcept : cnstr{c} {
    }
    auto group_cnstr(const std::size_t pos = 0) noexcept {
        for (std::size_t i = pos; i < size; ++i) {
            cnstr(storage[i]);
        }
    }
    auto init() noexcept {
        return group_cnstr();
    }
    auto cycle() noexcept {
        return group_cnstr();
    }
};

template<typename C, typename F, typename G, typename P = typename C::Type&>
constexpr auto make_iobuffer(C, F f, G g) noexcept {
    using Ret = decltype(f(std::declval<P>()));
    return IOBuffer<C, F, G, Ret>{f, g};
}

template<typename C, typename F, typename P = typename C::Type&>
constexpr auto make_iobuffer(C, F f) noexcept {
    using Ret = decltype(f(std::declval<P>()));
    return IOBuffer<C, F, void, Ret>{f};
}

#endif // IOBUFFER_H
