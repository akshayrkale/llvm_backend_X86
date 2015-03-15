	.file	"trace_su3.c.bc"
	.text
	.globl	trace_su3
	.align	16, 0x90
	.type	trace_su3,@function
trace_su3:                              # @trace_su3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	64(%rbx), %rsi
	movq	(%rbx), %rdi
	movq	8(%rbx), %r14
	callq	float64_add
	movq	72(%rbx), %rsi
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	float64_add
	movq	128(%rbx), %rsi
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	float64_add
	movq	136(%rbx), %rsi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	float64_add
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rcx, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	trace_su3, .Ltmp10-trace_su3
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
