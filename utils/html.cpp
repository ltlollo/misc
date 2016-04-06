#include <algorithm>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

#define assert(expr)                                                          \
    do {                                                                      \
        assert_entry((expr), "" #expr);                                       \
    } while (0)

void entry() {}

void assert_entry(bool expr, const char *str) {
    if (!expr) {
        entry();
    }
}

using namespace std;

static char nil[] = {'\0'};

struct Tag {
    char *first = &*nil;
    unsigned size = 0;
};

struct Str {
    const char *data;
    unsigned size;
    template <unsigned N>
    constexpr Str(const char (&s)[N]) : data{s}, size{N - 1} {}
};
struct Ignore {
    Str beg, end;
};
enum Type { Mark, Text };

namespace utilfuns {
auto type(Tag &t) { return *t.first == '<' ? Mark : Text; }
auto first(Tag &mark) { return mark.first + 1; }
auto size(Tag &mark) { return mark.size; }
unsigned narrow(auto b, const auto e) { return (unsigned)(&*e - &*b); }
auto addr(auto &i) { return &*i; }
bool eq(auto b, const auto e, const Str &s) {
    return std::equal(s.data, s.data + s.size, b);
}
auto sea(auto b, const auto e, const Str s) {
    return std::search(b, e, s.data, s.data + s.size);
}
}

using namespace utilfuns;

struct Element {
    Tag content;
    std::vector<Element> inners;
    Element() : content{}, inners{} {};
    Element(Tag t) : content{t.size ? t : Tag{}} {}
};
auto closes(Element *ele, auto b, const auto e) {
    if (ele->content.size == 0 || e == b) {
        return b;
    }
    assert(*(b + 1) != '>');
    if (*(b + 1) != '/') {
        return b;
    }
    const auto eb = first(ele->content);
    auto sz = size(ele->content);
    auto cb = b + 2;
    if (!std::equal(eb, eb + sz, cb, cb + sz)) {
        auto te = std::find(cb, e, '>');
        return te + 1;
    }
    assert(*(cb + sz) == '>');
    return cb + sz + 1;
}
auto consume_comment(auto b, const auto e) {
    static Str cbeg = {"<!--"}, cend = {"-->"};
    if (!eq(b, e, cbeg)) {
        return b;
    }
    auto ce = sea(b, e, cend);
    assert(ce != e);
    return ce + cend.size;
}
auto consume_ignoresingletonlist(auto b, const auto e) {
    static constexpr Ignore il[] = {{{"<!--"}, {"-->"}}, {{"<!"}, {">"}}};
    for (const auto &i : il) {
        if (eq(b, e, i.beg)) {
            auto ce = sea(b, e, i.end);
            assert(ce != e);
            return ce + i.end.size;
        }
    }
    return b;
}
void insert_sled(std::vector<char> &v, unsigned size) {
    v.resize(v.size() + size, '\0');
}
auto consume_ignorelist(auto b, const auto e) {
    static constexpr Ignore il[] = {{{"<script"}, {"</script>"}},
                                    {{"<style"}, {"</style>"}}};
    for (const auto & i:il) {
        if (eq(b, e, i.beg)) {
            auto bs = std::find(b, e, '>');
            assert(bs != e);
            auto ce = sea(bs, e, i.end);
            assert(ce != e);
            return ce + i.end.size;
        }
    }
    return b;
}
template <typename T> auto &last(std::vector<T> &v) {
    assert(v.size());
    return v[v.size() - 1];
}
template <typename T> auto backtrack(std::vector<T> &v) {
    assert(v.size());
    v.resize(v.size() - 1);
    return last(v);
}
template <typename T> auto pop(std::vector<T> &v) {
    auto r = last(v);
    v.resize(v.size() - 1);
    return r;
}
template <typename T> auto rpushv(std::vector<T> &w, auto &v) {
    for (auto i = rbegin(v); i != rend(v); ++i) {
        if (type(i->content) == Text || i->inners.size())
            w.push_back(&*i);
    }
}
auto init_mark(Element *ele, auto b, const auto e) {
    auto me = std::find(b, e, '>');
    assert(me != e);
    ele->content.first = addr(b);
    auto te = std::find(b, me, ' ');
    assert((ele->content.size = narrow(b + 1, te)) != 0);
    return me + 1;
}
auto push_notnil(std::vector<Element> &v, Tag &&t) {
    if (t.size) {
        v.emplace_back(Element(t));
    }
}
auto *push_new(std::vector<Element> &v) {
    v.emplace_back(Element{});
    return &v[v.size() - 1];
}
auto consume_singleton(Element *curr, auto b, const auto e) {
    static constexpr Str stags[] = {
        {"<area"}, {"<base"},  {"<br"},     {"<col"},   {"<command"},
        {"<embed"}, {"<hr"},    {"<img"},    {"<input"}, {"<link"},
        {"<meta"},  {"<param"}, {"<source"},
    };
    auto me = std::find(b, e, '>');
    assert(me != e);
    auto te = std::find(b, me, ' ');
    for (const auto &t : stags) {
        if (eq(b, e, t)) {
            curr->inners.emplace_back(
                Element(Tag{addr(b), narrow(b + 1, te)}));
            return me + 1;
        }
    }
    return b;
}
auto traverse(Element *root) {
    std::vector<Element *> stack{root};
    auto curr = last(stack);
    do {
        curr = pop(stack);
        if (curr->inners.size()) {
            rpushv(stack, curr->inners);
            curr = last(stack);
        } else {
            printf("%.*s", curr->content.size, curr->content.first);
        }
    } while (stack.size());
    printf("\n");
}
auto parse(Element *root, std::ifstream &ih) {
    unsigned sled = 10;
    std::vector<char> in(std::istreambuf_iterator<char>(ih), {});
    insert_sled(in, sled);
    auto i = in.data();
    const auto e = in.data() + in.size() - sled;
    std::vector<Element *> stack{root};
    auto curr = last(stack);
    if (i == e) {
        return;
    }
    do {
        auto s = std::find(i, e, '<');
        push_notnil(curr->inners, {addr(i), narrow(i, s)});
        i = s;
        if ((s = consume_ignorelist(i, e)) != i) {
            i = s;
        } else if ((s = consume_ignoresingletonlist(i, e)) != i) {
            i = s;
        } else if ((s = closes(curr, i, e)) != i) {
            i = s;
            curr = backtrack(stack);
        } else if ((s = consume_singleton(curr, i, e)) != i) {
            i = s;
        } else {
            curr = push_new(curr->inners);
            i = init_mark(curr, i, e);
            stack.push_back(curr);
        }
    } while (i < e);
}

int main() {
    std::ifstream ih("out.html");
    Element root;
    parse(&root, ih);
    traverse(&root);
    return 0;
}
