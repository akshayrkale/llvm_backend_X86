	.file	"make_ahmat.c.bc"
	.text
	.globl	make_anti_hermitian
	.align	16, 0x90
	.type	make_anti_hermitian,@function
make_anti_hermitian:                    # @make_anti_hermitian
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp8:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp9:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp10:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
.Ltmp12:
	.cfi_offset %rbx, -48
.Ltmp13:
	.cfi_offset %r12, -40
.Ltmp14:
	.cfi_offset %r13, -32
.Ltmp15:
	.cfi_offset %r14, -24
.Ltmp16:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %r15
	movq	72(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r14
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	136(%rbx), %r13
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	float64_add
	movabsq	$4599676419361018586, %rsi # imm = 0x3FD5555551C112DA
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, 48(%r14)
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, 56(%r14)
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, 64(%r14)
	movq	16(%rbx), %rdi
	movq	48(%rbx), %rsi
	callq	float64_sub
	movabsq	$4602678819172646912, %r15 # imm = 0x3FE0000000000000
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, (%r14)
	movq	32(%rbx), %rdi
	movq	96(%rbx), %rsi
	callq	float64_sub
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, 16(%r14)
	movq	80(%rbx), %rdi
	movq	112(%rbx), %rsi
	callq	float64_sub
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, 32(%r14)
	movq	24(%rbx), %rdi
	movq	56(%rbx), %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, 8(%r14)
	movq	40(%rbx), %rdi
	movq	104(%rbx), %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, 24(%r14)
	movq	88(%rbx), %rdi
	movq	120(%rbx), %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, 40(%r14)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	make_anti_hermitian, .Ltmp17-make_anti_hermitian
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
