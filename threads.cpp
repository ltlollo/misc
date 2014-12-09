#include <vector>
#include <iostream>
#include <tuple>
#include <pthread.h>

using namespace std;

template<size_t... Ns> struct Seq{};
template<size_t N, size_t... Ns> struct GenSeq : GenSeq<N-1, N-1, Ns...>{};
template<size_t... Ns> struct GenSeq<0, Ns...>{ using type = Seq<Ns...>; };
template<size_t N> using make_seq_t = typename GenSeq<N>::type;
template<typename Ret, typename... Args> using fn_t = Ret(*)(Args...);
template<typename T, size_t... Ns> constexpr auto caller(T* t) {
    t->result = t->call(Seq<Ns...>{});
}
template<typename T, size_t... Ns> constexpr void vcaller(T* t) {
    t->call(Seq<Ns...>{});
}

template<typename T> struct Function : public Function<decltype(&T::operator())>{};
template<typename T, typename Ret, typename... Args>
struct Function<Ret(T::*)(Args...) const> {
    using return_t = Ret;
    using ptr_t = fn_t<Ret, Args...>;
};

template<typename Ret, typename Fun, typename... Args>
struct FunStore {
    Fun f;
    std::tuple<Args...> args;
    pthread_t t;
    Ret result;

    Ret operator()() {
        return call(make_seq_t<sizeof...(Args)>{});
    }
    template<size_t... Ns> Ret call(Seq<Ns...>) {
        return f(std::get<Ns>(args)...);
    }
    template<size_t... Ns> void call_async(Seq<Ns...>) {
        pthread_create(&t, nullptr,
            (fn_t<void*, void*>)caller<FunStore<Ret, Fun, Args...>, Ns...>,
            (void*)this
        );
    }
    void async() {
        return call_async(make_seq_t<sizeof...(Args)>{});
    }
    void join() {
        pthread_join(t, nullptr);
    }
};

template<typename Fun, typename... Args>
struct FunStore<void, Fun, Args...> {
    Fun f;
    std::tuple<Args...> args;
    pthread_t t;

    void operator()() {
        call(make_seq_t<sizeof...(Args)>{});
    }
    template<size_t... Ns> void call(Seq<Ns...>) {
        f(std::get<Ns>(args)...);
    }
    template<size_t... Ns> void call_async(Seq<Ns...>) {
        pthread_create(&t, nullptr,
            (fn_t<void*, void*>)vcaller<FunStore<void, Fun, Args...>, Ns...>,
            (void*)this
        );
    }
    void async() {
        return call_async(make_seq_t<sizeof...(Args)>{});
    }
    void join() {
        pthread_join(t, nullptr);
    }
};

template<typename Fun, typename... Args>
const constexpr auto make_function(Fun f, Args... args) {
    using fun_t = typename Function<decltype(f)>::ptr_t;
    using ret_t = typename Function<decltype(f)>::return_t;
    return FunStore<ret_t, fun_t, Args...>{(fun_t)f, make_tuple(args...), (pthread_t)0, {}};
}

template<typename T, typename Fun, typename Fil>
auto split(const vector<T>& vec, Fun fun, Fil filter, unsigned Nth, unsigned N) {
    auto range_beg = vec.size()*(Nth)/N;
    auto range_end = vec.size()*(Nth+1)/N;
    auto len = range_end - range_beg;
    vector<std::result_of_t<Fun(T)>> res;
    if (!len) {
        return res;
    }
    res.reserve(len);
    for (size_t i = range_beg; i < range_end; ++i) {
        if (filter(i)) {
            res.emplace_back(fun(vec[i]));
        }
    }
    return res;
}

struct Caller {
    template<typename T, typename... TT> Caller(T& t, TT&... tt) {
        t.async();
        Caller(tt...);
        t.join();
    }
    template<typename T> Caller(T& t) { t.async(); t.join(); }
};

struct Foreach {
    template<typename F, typename T, typename... TT> Foreach(F&& f, T& t, TT&... tt) {
        f(t);
        Foreach(std::forward<F>(f), tt...);
    }
    template<typename F, typename T> Foreach(F&& f, T& t) { f(t); }
};

template<typename T, typename Fun, typename Fil, unsigned... Ns>
auto compute(const vector<T>& vec, Fun fun, Fil filter, std::integer_sequence<unsigned, Ns...>) {
    auto f = [](const vector<T>& vec, Fun fun, Fil filter, unsigned nth, unsigned of) {
        return split(vec, fun, filter, nth, of);
    };
    auto res = make_tuple(make_function(f, vec, fun, filter, Ns, sizeof...(Ns))...);
    Caller(std::get<Ns>(res)...);
    Foreach([](const auto& t){
        for (const auto& it: t.result) {
            cout << it << ' ';
        }
        cout << '\n';
    }, std::get<Ns>(res)...);
}

int main(int, char *[]) {
    vector<int> vec(40, 0);
    compute(vec,
            [](const auto& it){ return it+1; },
            [](const auto&){ return true; },
            std::make_integer_sequence<unsigned, 4>{});
    return 0;
}
