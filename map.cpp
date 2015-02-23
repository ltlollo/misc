#include <string>
#include <cstdlib>
#include <cstdio>

int main(int argc, char *argv[]) {
    auto args = argv+1;
    auto args_size = argc - 1;

    if (args_size < 2) {
        fprintf(stderr,
                "USAGE: %s f x [xs]\n"
                "SCOPE: executes f x [ && %s f xs]\n",
                argv[0], argv[0]);
        return 1;
    }

    int ret;
    auto cmd = args[0];
    std::string cmdline;
    for(auto arg_end = (args++) + args_size; args != arg_end; ++args) {
        cmdline = cmd;
        cmdline.push_back(' ');
        cmdline.insert(cmdline.size(), *args);
        ret = std::system(cmdline.c_str());
        if (ret != 0) {
            fprintf(stderr, "fail: %s\n", cmdline.c_str());
            return ret;
        }
    }

    return 0;
}

