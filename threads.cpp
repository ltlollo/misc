#include <vector>
#include <tuple>
#include <pthread.h>

namespace task {
enum Err { Ok, Create, Join, Except, Cancel };
template<size_t... Ns> struct Seq{};
template<size_t N, size_t... Ns> struct GenSeq : GenSeq<N-1, N-1, Ns...>{};
template<size_t... Ns> struct GenSeq<0, Ns...>{ using type = Seq<Ns...>; };
template<size_t N> using make_seq_t = typename GenSeq<N>::type;
template<typename Ret, typename... Args> using fn_t = Ret(*)(Args...);
template<typename T, size_t... Ns> constexpr auto caller(T* t) {
    t->err = Except;
    t->result = t->call(Seq<Ns...>{});
    t->err = Ok;
}
template<typename T, size_t... Ns> constexpr void vcaller(T* t) {
    t->err = Except;
    t->call(Seq<Ns...>{});
    t->err = Ok;
}
template<unsigned N> using Threads = std::make_integer_sequence<unsigned, N>;
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
    Err err{Ok};

    FunStore(Fun f, const std::tuple<Args...>& args) :
        f{f}, args{args} {
    }
    Ret operator()() {
        return call(make_seq_t<sizeof...(Args)>{});
    }
    template<size_t... Ns> Ret call(Seq<Ns...>) {
        return f(std::get<Ns>(args)...);
    }
    template<size_t... Ns> void call_async(Seq<Ns...>) {
        auto e = pthread_create(&t, nullptr,
            (fn_t<void*, void*>)caller<FunStore<Ret, Fun, Args...>, Ns...>,
            (void*)this
        );
        if (e < 0) { err = Create; }
    }
    void async() {
        return call_async(make_seq_t<sizeof...(Args)>{});
    }
    void join() {
        if ( err == Ok && pthread_join(t, nullptr) < 0) { err = Join; }
    }
    void cancel() {
        if ( err == Ok && pthread_cancel(t) < 0) { err = Cancel; }
    }
};

template<typename Fun, typename... Args>
struct FunStore<void, Fun, Args...> {
    Fun f;
    std::tuple<Args...> args;
    pthread_t t;
    Err err{Ok};

    FunStore(Fun f, const std::tuple<Args...>& args) :
        f{f}, args{args} {
    }
    void operator()() {
        call(make_seq_t<sizeof...(Args)>{});
    }
    template<size_t... Ns> void call(Seq<Ns...>) {
        f(std::get<Ns>(args)...);
    }
    template<size_t... Ns> void call_async(Seq<Ns...>) {
        auto e = pthread_create(&t, nullptr,
            (fn_t<void*, void*>)vcaller<FunStore<void, Fun, Args...>, Ns...>,
            (void*)this
        );
        if (err < 0) { err = Create; }
    }
    void async() {
        return call_async(make_seq_t<sizeof...(Args)>{});
    }
    void join() {
        if ( err == Ok && pthread_join(t, nullptr) < 0) { err = Join; }
    }
    void cancel() {
        if ( err == Ok && pthread_cancel(t) < 0) { err = Cancel; }
    }
};

template<typename Fun, typename... Args>
const constexpr auto make_function(Fun f, Args... args) {
    using fun_t = typename Function<decltype(f)>::ptr_t;
    using ret_t = typename Function<decltype(f)>::return_t;
    return FunStore<ret_t, fun_t, Args...>((fun_t)f, std::make_tuple(args...));
}

template<typename T, typename Fun, typename Fil>
auto split(const std::vector<T>* vec, Fun fun, Fil filter, unsigned Nth, unsigned N) {
    auto range_beg = vec->size()*(Nth)/N;
    auto range_end = vec->size()*(Nth+1)/N;
    auto len = range_end - range_beg;
    std::vector<std::result_of_t<Fun(T, size_t&)>> res;
    if (!len) {
        return res;
    }
    res.reserve(len);
    for (size_t i = range_beg; i < range_end; ++i) {
        if (filter((*vec)[i], i)) {
            res.emplace_back(fun((*vec)[i], i));
        }
    }
    return res;
}

struct Foreach {
    template<typename F, typename T, typename... TT> Foreach(F f, T& t, TT&... tt) {
        f(t);
        Foreach(f, tt...);
    }
    template<typename F, typename T> Foreach(F f, T& t) { f(t); }
};

template<typename T, typename Fun, typename Fil, unsigned... Ns>
auto compute(const std::vector<T>& vec, Fun fun, Fil filter, std::integer_sequence<unsigned, Ns...>) {
    auto f = [](const std::vector<T>* vec, Fun fun, Fil filter, unsigned nth, unsigned of) {
        return split(vec, fun, filter, nth, of);
    };
    auto res = std::make_tuple(make_function(f, &vec, fun, filter, Ns, sizeof...(Ns))...);
    Foreach([](auto& t){ t.async(); }, std::get<Ns>(res)...);
    Foreach([](auto& t){ t.join(); }, std::get<Ns>(res)...);
}
}

int main(int, char *[]) {
    std::vector<int> vec(100000000, 0);
    task::compute(vec,
            [](const auto& it, ...){ return it+1; },
            [](const auto&, ...){ return true; },
            task::Threads<4>{});
    return 0;
}
