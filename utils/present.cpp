#include <vector>
#include <string>
#include <stdio.h>

using namespace std;

struct str {
    char* leak;
    size_t n;
};

int main(int argc, char* argv[]) {
    auto print_help = [&]() {
        fprintf(stderr, "USAGE:\t%s n\n"
        "SCOPE:\tpresent lines in a tabular form [line; n]\n"
        "\t\twhen n is 0 returns 0", argv[0]);
    };
    if (argc-1 != 1) {
        print_help();
        return 1;
    }
    size_t n = stoul(argv[1]), i = 0, max = 0;
    ssize_t s;
    vector<str> vstr;
    str in{NULL, 0};
    if (!n) {
        return 0;
    }
    while((s = getline(&in.leak, &in.n, stdin)) != -1) {
        if ((size_t)s > max) {
            max = s;
        }
        in.leak[s-1] = ' ';
        vstr.push_back(in);
        in.leak = NULL;
        in.n = 0;
    }
    for (const auto& str: vstr) {
        printf("%-*s", (int)max, str.leak);
        if (!(++i%n)) {
            printf("\n");
        }
    }
    if (i%n) {
        printf("\n");
    };
    return 0;
}
