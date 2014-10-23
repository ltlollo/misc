#include "extra/func_meta.h"

using namespace std;
using namespace fun;
using namespace fun::cond;
using Cond = cond::False;

constexpr auto add = [](auto&& x, auto&& y) {
    return x + y;
};

auto printnl = [](auto&& x) {
    cout << x << '\n';
};

#define if If<
#define do >
#define else Else(),
#define exec(...) int main(int argc, char *argv[]) { __VA_ARGS__ ; return 0;}

exec(
  printnl(
    if Cond do() (
      add, 2, 3,
        else
      add, 2, 2
    )
  )
)

