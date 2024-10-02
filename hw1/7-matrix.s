
.text
.globl  main

f:
    pushq   %rbp                  # Save base pointer
    movq    %rsp, %rbp            # Set stack frame
    subq    $32, %rsp             # Allocate space for local variables (8 integers)

    cmpl    $15, %edi             # Compare i with N (15)
    jne     .Lnot_equal_N         # If i != N, proceed
    movl    $0, %eax              # Return 0
    jmp     .Lexit_f              # Exit function

.Lnot_equal_N:
    movl    %esi, %edx            # Move c to %edx
    shll    $4, %edx              # key = c << 4
    orl     %edi, %edx            # key |= i
    movl    %edx, -4(%rbp)        # Store key on stack

    leaq    memo(,%rdx,4), %rax   # Compute address of memo[key]
    movl    (%rax), %ecx          # Load memo[key] into %ecx
    cmpl    $0, %ecx              # Compare memo[key] with 0
    je      .Lr_is_zero           # If memo[key] == 0, proceed to compute
    subl    $1, %ecx              # r = memo[key] - 1
    movl    %ecx, %eax            # Return r
    jmp     .Lexit_f              # Exit function

.Lr_is_zero:
    movl    $0, -12(%rbp)         # Initialize s = 0
    movl    $0, -16(%rbp)         # Initialize j = 0

.Lloop_start:
    cmpl    $15, -16(%rbp)        # Compare j with N (15)
    jge     .Lloop_end            # If j >= N, exit loop

    movl    $1, %edx              # Set col = 1
    movl    -16(%rbp), %ecx       # Load j
    shll    %cl, %edx             # col <<= j (col = 1 << j)
    movl    %edx, -20(%rbp)       # Store col

    movl    %esi, %eax            # Load c
    andl    %edx, %eax            # Check if c & col == 0
    testl   %eax, %eax
    je      .Lnext_iteration      # If zero, continue loop

    movl    %esi, %ecx            # Compute c - col
    subl    %edx, %ecx            # ecx = c - col

    leal    1(%edi), %edx         # edx = i + 1

    # Calculate m[i][j]
    movl    %edi, %eax            # %eax = i
    imull   $15, %eax, %eax       # %eax = i * 15
    addl    -16(%rbp), %eax       # %eax = i * 15 + j

    leaq    m(,%rax,4), %rax      # Address of m[i][j]
    movl    (%rax), %eax          # Load m[i][j]
    movl    %eax, -24(%rbp)       # Store x = m[i][j]

    # Save %esi and %edi before the call
    pushq   %rsi                  # Save %rsi (c)
    pushq   %rdi                  # Save %rdi (i)

    # Prepare arguments for recursive call f(i + 1, c - col)
    movl    %edx, %edi            # i + 1
    movl    %ecx, %esi            # c - col
    call    f

    # Restore %edi and %esi after the call
    popq    %rdi                  # Restore %rdi (i)
    popq    %rsi                  # Restore %rsi (c)

    movl    -24(%rbp), %edx       # Retrieve x = m[i][j]
    addl    %eax, %edx            # x += f(i + 1, c - col)

    movl    -12(%rbp), %ecx       # Load s
    cmpl    %ecx, %edx            # Compare x and s
    jle     .Lafter_if            # If x <= s, skip update
    movl    %edx, -12(%rbp)       # Update s = x

.Lafter_if:
.Lnext_iteration:
    incl    -16(%rbp)             # Increment j
    jmp     .Lloop_start          # Loop back

.Lloop_end:
    movl    -4(%rbp), %edx        # Load key
    leaq    memo(,%rdx,4), %rax   # Address of memo[key]
    movl    -12(%rbp), %ecx       # Load s
    addl    $1, %ecx              # s += 1
    movl    %ecx, (%rax)          # memo[key] = s + 1

    movl    -12(%rbp), %ecx       # Load s
    movl    %ecx, %eax            # Return s

.Lexit_f:
    addq    $32, %rsp             # Deallocate space for local variables
    popq    %rbp                  # Restore base pointer
    ret                           # Return from function

main:
    pushq   %rbp                  # Save base pointer
    movq    %rsp, %rbp            # Set stack frame

    movl    $0, %edi              # i = 0
    movl    $32767, %esi          # c = (1 << 15) - 1 = 32767
    call    f                     # Call f(0, 32767)

    subq    $8, %rsp              # Align stack to 16 bytes before calling printf
    movl    %eax, %esi            # Move result to %esi (second argument)
    movq    $output, %rdi         # Load address of output format string
    xorq    %rax, %rax            # Clear %rax
    call    printf                # Call printf
    addq    $8, %rsp              # Restore stack pointer

    movl    $0, %eax              # Return 0 from main
    popq    %rbp                  # Restore base pointer
    ret                           # Return from main

.data
output: .string "%d\n"
N:     .long   15
L:     .long   4
m:
    .long   7, 53, 183, 439, 863, 497, 383, 563, 79, 973, 287, 63, 343, 169, 583
    .long   627, 343, 773, 959, 943, 767, 473, 103, 699, 303, 957, 703, 583, 639, 913
    .long   447, 283, 463, 29, 23, 487, 463, 993, 119, 883, 327, 493, 423, 159, 743
    .long   217, 623, 3, 399, 853, 407, 103, 983, 89, 463, 290, 516, 212, 462, 350
    .long   960, 376, 682, 962, 300, 780, 486, 502, 912, 800, 250, 346, 172, 812, 350
    .long   870, 456, 192, 162, 593, 473, 915, 45, 989, 873, 823, 965, 425, 329, 803
    .long   973, 965, 905, 919, 133, 673, 665, 235, 509, 613, 673, 815, 165, 992, 326
    .long   322, 148, 972, 962, 286, 255, 941, 541, 265, 323, 925, 281, 601, 95, 973
    .long   445, 721, 11, 525, 473, 65, 511, 164, 138, 672, 18, 428, 154, 448, 848
    .long   414, 456, 310, 312, 798, 104, 566, 520, 302, 248, 694, 976, 430, 392, 198
    .long   184, 829, 373, 181, 631, 101, 969, 613, 840, 740, 778, 458, 284, 760, 390
    .long   821, 461, 843, 513, 17, 901, 711, 993, 293, 157, 274, 94, 192, 156, 574
    .long   34, 124, 4, 878, 450, 476, 712, 914, 838, 669, 875, 299, 823, 329, 699
    .long   815, 559, 813, 459, 522, 788, 168, 586, 966, 232, 308, 833, 251, 631, 107
    .long   813, 883, 451, 509, 615, 77, 281, 613, 459, 205, 380, 274, 302, 35, 805
.bss
    .align 4
memo:
    .space  2097152               # 524,288 ints * 4 bytes per int
