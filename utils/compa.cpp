// gpp self

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

inline bool is_spacing(wchar_t c) {
    return c == ' ' || c == '\n' || c == '\t';
}

int main(int argc, char *argv[]) {
    enum { More, Mid, Less } opt = (argc > 1) ? [&](){
        if (argv[1] == std::string("-l"))  return Mid;
        if (argv[1] == std::string("-ll")) return Less;
        std::cerr << "[E]: unknown option: " << argv[1] << "\nUsage: "
        << argv[0] << " [-l[l]]\n\tScope: removes unecesary spacing\n";
        exit(EXIT_FAILURE);
    }() : More;
    std::locale::global(std::locale(""));
    std::vector<wchar_t> in;
    std::copy_if(std::istreambuf_iterator<wchar_t>{std::wcin}, {},
                 std::back_inserter(in), [](const auto& it){
                    return it != '\r';
    });
    if (opt == More) {
        auto it = in.cbegin();
        if (it == in.cend()) { return 0; }
        if (is_spacing(*it)) { std::wcout << *it; }
        if (++it  == in.cend()) { return 0; }
        for (; it < in.cend(); ++it) {
            if (!is_spacing(*(it-1)) || !is_spacing(*it) ) {
                std::wcout << *it;
            }
        }
    } else {
        std::unique(in.begin(), in.end(), [=](const auto& i, const auto& j){
            return (opt == Mid) ? is_spacing(j) && is_spacing(i) :
                i == j && is_spacing(i);
        });
        std::copy(in.begin(), in.end(),
                  std::ostreambuf_iterator<wchar_t>{std::wcout});
    }
    return 0;
}
