#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#include <sys/mman.h>


void
help(int val) {
    int val1 = val + 1;
    printf("val is %d\n", val1);
}

#define PGSIZE 0x1000

struct StackSpace {
    void *mm;
    int npages;
};

int
stack_init(struct StackSpace *s, int npages) {
    char *stk = mmap(NULL, (npages + 1) * PGSIZE, PROT_WRITE | PROT_READ,
                     MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);
    char *stkbeg = stk + (npages + 1) * PGSIZE;

    if (stk == MAP_FAILED) {
        return -1;
    }
    s->mm = stk;
    s->npages = npages;

    mprotect(stk, 1, PROT_NONE);
    asm("mov %%rsp, %%r15\n\t"
        "mov %0, %%rsp\n\t"
        "push %%r15"
        : :"r"(stkbeg) :"%r15", "%rsp");
    return 0;
}

void *
curr_stkp(void) {
    void *stk;
    asm("mov %%rsp, %0":"=r"(stk)::);
    return stk;
}

void *
stack_top(struct StackSpace *s) {
    char *beg = (char *)s->mm + (s->npages + 1) * PGSIZE;
    return ((void **)beg)[-1];
}


void
stack_delete(struct StackSpace *s) {
    void *old_stk = stack_top(s);
    int npages = s->npages;

    asm("mov %0, %%rsp"::"r"(old_stk):"%rsp");
    memset(s->mm + PGSIZE, 0, npages * PGSIZE);
    asm("mfence" :::);
    munmap(s->mm, (npages) * PGSIZE);
}

int
main() {
    //int npages = 2;
    //char *stk = mmap(NULL, (npages + 1) * PGSIZE, PROT_WRITE | PROT_READ,
    //                 MAP_PRIVATE | MAP_ANONYMOUS, 0, 0);

    //assert(stk != MAP_FAILED);
    //mprotect(stk, 1, PROT_NONE);
    //char *stkbeg = stk + (npages + 1) * PGSIZE;

    //asm("mov %%rsp, %%r15\n\t"
    //    "mov %0, %%rsp\n\t"
    //    "push %%r15"
    //    : :"r"(stkbeg) :"%r15", "%rsp");
    //asm("mov %0, %%rsp"::"r"(((void**)stkbeg)[-1]):"%rsp");
    //memset(stk + PGSIZE, 0, npages * PGSIZE);
    //asm("mfence" :::);
    //munmap(stk, (npages + 1) * PGSIZE);
    //
    printf("DBG: sp %p\n", curr_stkp());
    struct StackSpace s;
    stack_init(&s, 2);

    help(3);
    printf("DBG: s.mm %p\n", s.mm);
    printf("DBG: sp %p\n", curr_stkp());

    stack_delete(&s);
    printf("DBG: sp %p\n", curr_stkp());
    //asm("mov %0, %%rsp"::"r"(((void**)stkbeg)[-1]):"%rsp");
    //memset(stk + PGSIZE, 0, npages * PGSIZE);
    //asm("mfence" :::);
    //munmap(stk, (npages + 1) * PGSIZE);

    return 0;
}
