.data
msg: .string "sqrt(%2d) = %2d\n"

.text
.globl main

isqrt:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $0, -8(%rbp) # c = 0
    movq $1, -16(%rbp) # s = 1

loop:
    movq -16(%rbp), %rax
    cmpq %rdi, %rax
    jge end

    addq $1, -8(%rbp)
    movq -8(%rbp), %rax
    imul $2, %rax
    addq $1, %rax
    addq %rax, -16(%rbp)
    jmp loop

end:
    movq -8(%rbp), %rax
    addq $16, %rsp
    popq %rbp
    ret

main:
    pushq %rbp
    movq %rsp, %rbp
    subq $8, %rsp
    movq $0, -8(%rbp) # n = 0

loop_main:
    movq -8(%rbp), %rax 
    cmpq $20, %rax # n < 20
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
    addq $8, %rsp
    popq %rbp
    ret 
