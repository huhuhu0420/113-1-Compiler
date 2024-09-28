.data
output: .string "%d\n"

.text

.globl main

first:
    # print (let x = 3 in x* x)
    pushq %rbp
    movq %rsp, %rbp

    pushq $3
    popq %rax
    imul %rax, %rax

    movq $output, %rdi
    movq %rax, %rsi
    call printf

    xor %rax, %rax
    popq %rbp
    ret

second:
    # print (let x = 3 in (let y = x + x in x * y) + (let z = x + 3 in z / z))
    pushq %rbp
    movq %rsp, %rbp
    subq $24, %rsp  # Adjust stack to keep it 16-byte aligned

    # x = 3
    movq $3, -8(%rbp)
    # y = x + x
    movq -8(%rbp), %rax
    addq %rax, %rax
    movq %rax, -16(%rbp)
    # z = x + 3
    movq -8(%rbp), %rax
    addq $3, %rax
    movq %rax, -24(%rbp)
    # z / z
    movq -24(%rbp), %rax
    xor %rdx, %rdx
    movq %rax, %rbx
    idiv %rbx
    movq %rax, %rbx
    # x * y
    movq -8(%rbp), %rax
    imul -16(%rbp), %rax
    # (x * y) + (z / z)
    addq %rbx, %rax

    # Prepare arguments for printf
    movq $output, %rdi
    movq %rax, %rsi
    movb $0, %al  # No SSE registers used
    call printf

    addq $24, %rsp
    popq %rbp
    ret

main:
    pushq %rbp
    movq %rsp, %rbp

    call first
    call second

    popq %rbp
    ret
