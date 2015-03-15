	.file	"su3_rdot.c.bc"
	.text
	.globl	su3_rdot
	.align	16, 0x90
	.type	su3_rdot,@function
su3_rdot:                               # @su3_rdot
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
	movq	(%rbx), %rdi
	movq	8(%rbx), %r15
	movq	(%r14), %rsi
	movq	8(%r14), %r12
	callq	float64_mul
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	16(%rbx), %rdi
	movq	16(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	24(%rbx), %rdi
	movq	24(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	32(%rbx), %rdi
	movq	32(%r14), %rsi
	movq	%rax, %r15
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	40(%rbx), %rdi
	movq	40(%r14), %rsi
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	su3_rdot, .Ltmp16-su3_rdot
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
