	.file	"ranstuff.c.bc"
	.text
	.globl	initialize_prn
	.align	16, 0x90
	.type	initialize_prn,@function
initialize_prn:                         # @initialize_prn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$3, %rcx
	leaq	69607(,%rdx,8), %r8
	shlq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movabsq	$4294967040, %r9        # imm = 0xFFFFFF00
	movq	%rsi, %r10
	andq	%r9, %r10
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movq	%rsi, %rax
	andq	%r9, %rax
	movq	$8, %rcx
	shrq	%cl, %rax
	movq	$8, %rcx
	shrq	%cl, %r10
	movq	%r10, (%rdi)
	movq	%rax, 8(%rdi)
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movq	%rsi, %r10
	andq	%r9, %r10
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movq	%rsi, %rax
	andq	%r9, %rax
	movq	$8, %rcx
	shrq	%cl, %rax
	movq	$8, %rcx
	shrq	%cl, %r10
	movq	%r10, 16(%rdi)
	movq	%rax, 24(%rdi)
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movq	%rsi, %r10
	andq	%r9, %r10
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movq	%rsi, %rax
	andq	%r9, %rax
	movq	$8, %rcx
	shrq	%cl, %rax
	movq	$8, %rcx
	shrq	%cl, %r10
	movq	%r10, 32(%rdi)
	movq	%rax, 40(%rdi)
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	andq	%rsi, %r9
	movq	$8, %rcx
	shrq	%cl, %r9
	movq	%r9, 48(%rdi)
	imulq	%r8, %rsi
	addq	$12345, %rsi            # imm = 0x3039
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 72(%rdi)
	movabsq	$429518204436480, %rax  # imm = 0x186A500000000
	addq	%rdx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 56(%rdi)
	movq	$12345, 64(%rdi)        # imm = 0x3039
	movabsq	$4499096027743125504, %rax # imm = 0x3E70000000000000
	movq	%rax, 80(%rdi)
	retq
.Ltmp0:
	.size	initialize_prn, .Ltmp0-initialize_prn
	.cfi_endproc

	.globl	myrand
	.align	16, 0x90
	.type	myrand,@function
myrand:                                 # @myrand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbx, -16
	movq	32(%rdi), %r8
	movq	40(%rdi), %rsi
	movq	$7, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	movq	48(%rdi), %rbx
	movq	$17, %rcx
	shlq	%cl, %rbx
	orq	%rdx, %rbx
	movq	$1, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	movq	$23, %rcx
	movq	%rsi, %rdx
	movq	%rsi, 48(%rdi)
	movq	%r8, 40(%rdi)
	movq	16(%rdi), %r8
	movq	24(%rdi), %rsi
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movq	%rsi, 32(%rdi)
	movq	%r8, 24(%rdi)
	movq	(%rdi), %rax
	xorq	%rbx, %rdx
	movq	8(%rdi), %rcx
	movq	%rcx, 16(%rdi)
	movq	%rax, 8(%rdi)
	andq	$16777215, %rdx         # imm = 0xFFFFFF
	movq	%rdx, (%rdi)
	movq	72(%rdi), %rax
	movq	56(%rdi), %rsi
	movq	64(%rdi), %rcx
	imulq	%rax, %rsi
	addq	%rcx, %rsi
	movabsq	$4294967040, %rax       # imm = 0xFFFFFF00
	andq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 72(%rdi)
	movq	80(%rdi), %rbx
	movq	$8, %rcx
	shrq	%cl, %rax
	xorq	%rdx, %rax
	movq	%rax, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	popq	%rbx
	retq
.Ltmp5:
	.size	myrand, .Ltmp5-myrand
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
