.data
msg: .string "sqrt(%2d) = %2d\n"

.text
.globl main

isqrt:
    pushq %rbp
    movq %rsp, %rbp
    subq $24, %rsp
    movq $0, -8(%rbp) # c = 0
    movq $1, -16(%rbp) # s = 1
    movq $1, -24(%rbp) # inc = 1

loop:
    movq -16(%rbp), %rax
    cmpq %rdi, %rax
    jg end

    addq $1, -8(%rbp) # c++
    addq $2, -24(%rbp) # inc += 2
    movq -24(%rbp), %rax
    addq %rax, -16(%rbp) # s += inc
    jmp loop

end:
    movq -8(%rbp), %rax
    addq $24, %rsp
    popq %rbp
    ret

main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $0, -8(%rbp) # n = 0

loop_main:
    movq -8(%rbp), %rax 
    cmpq $21, %rax # n < 21
    jge end_main
    movq %rax, %rdi 
    call isqrt
    movq %rax, %rdx # 3rd argument
    movq -8(%rbp), %rsi # 2nd argument
    movq $msg, %rdi
    movq $0, %rax
    call printf

    addq $1, -8(%rbp) # n++
    jmp loop_main

end_main:
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    ret 
