.data
true: .string "true\n"
false: .string "false\n"
msg: .string "%d\n"

.text

.globl main

print_bool:
    pushq %rbp
    movq %rsp, %rbp
    cmp $0, %rdi
    je print_false
    jne print_true

print_true:
    movq $true, %rdi
    call printf
    movq $0, %rax
    popq %rbp
    ret

print_false:
    movq $false, %rdi
    call printf
    movq $0, %rax
    popq %rbp
    ret

first:
    # true && false
    pushq %rbp
    movq %rsp, %rbp
    movq $1, %rax
    movq $0, %rbx
    and %rbx, %rax
    popq %rbp
    ret

second:
    # if 3 <> 4 then 10 * 2 else 14
    pushq %rbp
    movq %rsp, %rbp
    movq $3, %rax
    cmp $4, %rax
    je second_else
    movq $10, %rax
    imul $2, %rax
    popq %rbp
    ret

second_else:
    movq $14, %rax
    popq %rbp
    ret

third:
    # (2 = 3) || (4 <= 2 * 3)
    pushq %rbp
    movq %rsp, %rbp
    movq $2, %rax
    cmp $3, %rax
    je third_true
    movq $4, %rax
    movq $2, %rbx
    imul $3, %rbx
    cmp %rbx, %rax
    jle third_true
    movq $0, %rax
    popq %rbp
    ret

third_true:
    movq $1, %rax
    popq %rbp
    ret

main:
    pushq %rbp       # save the old base pointer
    movq %rsp, %rbp  # set the new base pointer
    
    call first
    movq %rax, %rdi
    call print_bool

    call second
    movq $msg, %rdi
    movq %rax, %rsi
    call printf
    
    call third
    movq %rax, %rdi
    call print_bool

    popq %rbp        # restore the old base pointer
    ret              # return    
