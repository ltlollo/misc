#include "extra/func_meta.h"

using namespace std;
using namespace fun;
using namespace fun::cond;

#define exp(...) __VA_ARGS__;
#define if If<
#define do >()
#define else Else(),
#define exec(...) int main(int argc, char *argv[]) { exp(__VA_ARGS__) exp(return 0) }
#define set(x, y) using x = y;
#define lambda(fun, ...) [](__VA_ARGS__) { exp(fun) }
#define rlambda(fun, ...) lambda(return fun, __VA_ARGS__)
#define def(name, fun, ...) exp(constexpr auto name = lambda(fun, __VA_ARGS__))
#define rdef(name, fun, ...) def(name, return fun, __VA_ARGS__)

def(print, cout<<x<<'\n', auto&& x)
rdef(add, x + y, auto&& x, auto&& y)
rdef(one, 1)
rdef(other, add(1, one()))
rdef(str, to_string(x), auto&& x)
rdef(hallo, "hallo"s)

set(Cond, True)

exec(
  print(
    if Cond do (
      rlambda(1),
        else
      rlambda(add(hallo(), str(other())))
    )
  )
)

