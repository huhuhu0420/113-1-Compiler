.data
msg: .string "n = %d\n"

.text

.globl main

print_n:
    pushq %rbp
    movq %rsp, %rbp
    movq %rdi, %rsi
    movq $msg, %rdi
    call printf
    movq $0, %rax
    popq %rbp
    ret

first:
    # 4 + 6
    pushq %rbp
    movq %rsp, %rbp
    movq $4, %rax
    add $6, %rax
    popq %rbp
    ret

second:
    # 21 * 2
    pushq %rbp
    movq %rsp, %rbp
    movq $21, %rax
    imul $2, %rax
    popq %rbp
    ret

third:
    # 4 + 7 / 2
    pushq %rbp
    movq %rsp, %rbp
    movq $7, %rax
    movq $2, %rbx
    idiv %rbx
    add $4, %rax
    popq %rbp
    ret

forth:
    # 3 - 6 * (10 / 5)
    pushq %rbp
    movq %rsp, %rbp

    # 10 / 5
    movq $10, %rax # dividend
    xor %rdx, %rdx
    movq $5, %rbx # divisor
    div %rbx
    imul $6, %rax
    movq $3, %rbx
    sub %rax, %rbx
    movq %rbx, %rax

    popq %rbp
    ret

main:
    pushq %rbp       # save the old base pointer
    movq %rsp, %rbp  # set the new base pointer

    call first
    movq %rax, %rdi  
    call print_n   

    call second 
    movq %rax, %rdi  
    call print_n   

    call third 
    movq %rax, %rdi  
    call print_n   

    call forth 
    movq %rax, %rdi  
    call print_n   

    popq %rbp        # restore the old base pointer
    ret              # return    
