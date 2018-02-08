extern void
clear_stk(void) {
    asm volatile(
        "mov rcx, rsp;"
        "mov rax, rsp;"
        "mov rbx, 0;"
        "and rax, -4096;"
        "add rax, -4096;"
        "1: push rbx;"
        "cmp rsp, rax;"
        "jne 1b;"
        "mov rsp, rcx;"
        "mfence;"
        :
        :
        : "rax", "rbx", "rcx", "memory"
    );
}

int main() {
    clear_stk();
    return 0;
}
