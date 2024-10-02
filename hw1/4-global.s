.data
x: .quad 2
y: .quad 0
msg: .string "%d\n"

.text

.globl main

main:
    pushq %rbp
    movq %rsp, %rbp
    # load x into rax
    movq x(%rip), %rax
    # cal x * x
    imul %rax, %rax
    # store the result in y
    movq %rax, y(%rip)

    # print y + x
    movq y(%rip), %rax
    add x(%rip), %rax

    movq $msg, %rdi
    movq %rax, %rsi
    call printf
    
    popq %rbp
    ret
