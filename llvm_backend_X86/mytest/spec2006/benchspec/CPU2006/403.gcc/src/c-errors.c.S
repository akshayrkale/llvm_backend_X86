	.file	"c-errors.c.bc"
	.text
	.globl	pedwarn_c99
	.align	16, 0x90
	.type	pedwarn_c99,@function
pedwarn_c99:                            # @pedwarn_c99
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp3:
	.cfi_def_cfa_offset 464
.Ltmp4:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	464(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	input_filename(%rip), %rcx
	movl	flag_isoc99(%rip), %edx
	movl	lineno(%rip), %r8d
	movq	$8, 48(%rsp)
	testq	%rdx, %rdx
	movq	$1, %r9
	je	.LBB0_4
# BB#1:                                 # %lor.rhs
	movl	flag_pedantic_errors(%rip), %edx
	testq	%rdx, %rdx
	jne	.LBB0_2
# BB#3:                                 # %lor.rhs
	movabsq	$1, %r9
	jmp	.LBB0_4
.LBB0_2:
	movabsq	$0, %r9
.LBB0_4:                                # %lor.end
	leaq	72(%rsp), %rbx
	leaq	48(%rsp), %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_diagnostic_context
	movq	%rbx, %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	retq
.Ltmp5:
	.size	pedwarn_c99, .Ltmp5-pedwarn_c99
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
