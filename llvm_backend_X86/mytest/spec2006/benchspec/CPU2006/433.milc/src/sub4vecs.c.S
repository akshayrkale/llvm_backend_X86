	.file	"sub4vecs.c.bc"
	.text
	.globl	sub_four_su3_vecs
	.align	16, 0x90
	.type	sub_four_su3_vecs,@function
sub_four_su3_vecs:                      # @sub_four_su3_vecs
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	movq	%rsi, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	movq	(%rax), %rsi
	movq	%rax, %r15
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, %r13
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r12
	movq	%r12, 8(%rsp)           # 8-byte Spill
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movq	8(%rbx), %rdi
	movq	8(%r15), %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 8(%rbx)
	movq	(%r12), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movq	8(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 8(%rbx)
	movq	(%r13), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, (%rbx)
	movq	8(%r13), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 8(%rbx)
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	(%r13), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, (%rbx)
	movq	8(%r13), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, 8(%rbx)
	movq	16(%rbx), %rdi
	movq	(%rsp), %r15            # 8-byte Reload
	movq	16(%r15), %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	24(%rbx), %rdi
	movq	24(%r15), %rsi
	movq	%r15, %r13
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 24(%rbx)
	movq	16(%r12), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	24(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 24(%rbx)
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	16(%r12), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, 16(%rbx)
	movq	24(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 24(%rbx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rsi
	movq	24(%rax), %r12
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, 24(%rbx)
	movq	%r14, 16(%rbx)
	movq	32(%rbx), %rdi
	movq	32(%r13), %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, 32(%rbx)
	movq	40(%rbx), %rdi
	movq	40(%r13), %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 40(%rbx)
	movq	8(%rsp), %r12           # 8-byte Reload
	movq	32(%r12), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, 32(%rbx)
	movq	40(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 40(%rbx)
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	32(%r12), %rsi
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, 32(%rbx)
	movq	40(%r12), %rsi
	movq	%r15, %rdi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 40(%rbx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rsi
	movq	40(%rax), %r12
	movq	%r14, %rdi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, 40(%rbx)
	movq	%r14, 32(%rbx)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	sub_four_su3_vecs, .Ltmp17-sub_four_su3_vecs
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
