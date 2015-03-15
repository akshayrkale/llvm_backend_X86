	.file	"su3_dot.c.bc"
	.text
	.globl	su3_dot
	.align	16, 0x90
	.type	su3_dot,@function
su3_dot:                                # @su3_dot
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
	subq	$48, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 96
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
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	(%rdi), %rbx
	movq	8(%rdi), %r12
	movq	(%rsi), %r15
	movq	8(%rsi), %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	16(%r14), %r15
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	24(%r14), %r13
	movq	24(%rbx), %r14
	movq	%rax, %r12
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_add
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %r13
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	float64_mul
	movq	40(%r14), %r14
	movq	40(%rbx), %r15
	movq	%rax, %r12
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, %rcx
	movq	%rbx, %rax
	movq	%rcx, %rdx
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	su3_dot, .Ltmp17-su3_dot
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
