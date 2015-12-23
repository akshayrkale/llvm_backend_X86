	.file	"rand_ahmat.c.bc"
	.text
	.globl	random_anti_hermitian
	.align	16, 0x90
	.type	random_anti_hermitian,@function
random_anti_hermitian:                  # @random_anti_hermitian
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, %r12
	movabsq	$4603375528459645724, %rsi # imm = 0x3FE279A74590331C
	movq	%r12, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 48(%rbx)
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	float64_sub
	movq	%rax, 56(%rbx)
	movabsq	$-4615492908767759588, %rsi # imm = 0xBFF279A74590331C
	movq	%r12, %rdi
	callq	float64_mul
	movq	%rax, 64(%rbx)
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, (%rbx)
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, 16(%rbx)
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, 32(%rbx)
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, 8(%rbx)
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, 24(%rbx)
	movq	%r14, %rdi
	callq	gaussian_rand_no
	movq	%rax, 40(%rbx)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	random_anti_hermitian, .Ltmp16-random_anti_hermitian
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
