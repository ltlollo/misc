#include "extra/func_meta.h"

using namespace std;
using namespace fun;
using namespace fun::cond;

#define exp(...) __VA_ARGS__;
#define if If<
#define do >
#define else Else(),
#define exec(...) int main(int argc, char *argv[]) { exp(__VA_ARGS__) exp(return 0) }
#define set(x, y) using x = y;
#define lambda(fun, ...) [](__VA_ARGS__) { exp(fun) }
#define def(name, fun, ...) exp(constexpr auto name = lambda(fun, __VA_ARGS__))

def(add, return x + y, auto&& x, auto&& y)
def(one, return 1)
def(printnl, cout<<x<<'\n', auto&& x)
def(other, return add(1, one()))

set(Cond, False)
exec(
  printnl(
    if Cond do() (
      lambda(return 1),
        else
      add, one(), other()
    )
  )
)

