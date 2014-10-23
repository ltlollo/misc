#include "extra/func_meta.h"

using namespace std;
using namespace fun;
using namespace fun::cond;
using Cond = cond::True;

#define if If<
#define then >{}(
#define else , Else{},
#define fi )

auto print = [](auto x, auto y) {
    std::cout << x << ' ' << y << '\n';
};

int main(int argc, char *argv[]) {
    if Cond then
        print, "Hallo", "True"
    else
        print, "Hallo", "False"
    fi;
    return 0;
}
