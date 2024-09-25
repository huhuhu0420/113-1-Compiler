.data
msg: .string "n = %d\n"

.text

.globl main

main:
    pushq %rbp       # save the old base pointer
    movq %rsp, %rbp  # set the new base pointer
    movq $msg, %rdi  # set the first argument
    movq $42, %rsi   # set the second argument
    call printf      # call printf
    movq $0, %rax    # return 0
    popq %rbp        # restore the old base pointer
    ret              # return    
