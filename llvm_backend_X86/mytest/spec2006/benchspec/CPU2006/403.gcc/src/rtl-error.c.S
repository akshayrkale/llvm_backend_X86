	.file	"rtl-error.c.bc"
	.text
	.globl	error_for_asm
	.align	16, 0x90
	.type	error_for_asm,@function
error_for_asm:                          # @error_for_asm
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
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	464(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	72(%rsp), %rdi
	leaq	48(%rsp), %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	callq	set_diagnostic_context
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	je	.LBB0_5
# BB#1:                                 # %entry
	cmpq	$41, %rcx
	je	.LBB0_8
# BB#2:                                 # %entry
	cmpq	$47, %rcx
	jne	.LBB0_4
# BB#3:                                 # %land.lhs.true.i.i
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	je	.LBB0_8
	jmp	.LBB0_4
.LBB0_5:                                # %land.lhs.true18.i.i
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	je	.LBB0_8
# BB#6:                                 # %land.lhs.true18.i.i
	cmpq	$47, %rcx
	jne	.LBB0_4
# BB#7:                                 # %if.then25.i.i
	movq	16(%rax), %rax
.LBB0_8:                                # %if.end56.i.i
	testq	%rax, %rax
	je	.LBB0_4
# BB#9:                                 # %if.then57.i.i
	movq	48(%rax), %rcx
	movq	%rcx, 408(%rsp)
	addq	$56, %rax
	jmp	.LBB0_10
.LBB0_4:                                # %if.else62.i.i
	movq	input_filename(%rip), %rax
	movq	%rax, 408(%rsp)
	movabsq	$lineno, %rax
.LBB0_10:                               # %diagnostic_for_asm.exit
	movl	(%rax), %eax
	movl	%eax, 416(%rsp)
	leaq	72(%rsp), %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	retq
.Ltmp5:
	.size	error_for_asm, .Ltmp5-error_for_asm
	.cfi_endproc

	.globl	warning_for_asm
	.align	16, 0x90
	.type	warning_for_asm,@function
warning_for_asm:                        # @warning_for_asm
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 16
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp9:
	.cfi_def_cfa_offset 464
.Ltmp10:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	464(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	leaq	72(%rsp), %rdi
	leaq	48(%rsp), %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	$1, %r9
	callq	set_diagnostic_context
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	je	.LBB1_5
# BB#1:                                 # %entry
	cmpq	$41, %rcx
	je	.LBB1_8
# BB#2:                                 # %entry
	cmpq	$47, %rcx
	jne	.LBB1_4
# BB#3:                                 # %land.lhs.true.i.i
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	je	.LBB1_8
	jmp	.LBB1_4
.LBB1_5:                                # %land.lhs.true18.i.i
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	je	.LBB1_8
# BB#6:                                 # %land.lhs.true18.i.i
	cmpq	$47, %rcx
	jne	.LBB1_4
# BB#7:                                 # %if.then25.i.i
	movq	16(%rax), %rax
.LBB1_8:                                # %if.end56.i.i
	testq	%rax, %rax
	je	.LBB1_4
# BB#9:                                 # %if.then57.i.i
	movq	48(%rax), %rcx
	movq	%rcx, 408(%rsp)
	addq	$56, %rax
	jmp	.LBB1_10
.LBB1_4:                                # %if.else62.i.i
	movq	input_filename(%rip), %rax
	movq	%rax, 408(%rsp)
	movabsq	$lineno, %rax
.LBB1_10:                               # %diagnostic_for_asm.exit
	movl	(%rax), %eax
	movl	%eax, 416(%rsp)
	leaq	72(%rsp), %rdi
	callq	report_diagnostic
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	retq
.Ltmp11:
	.size	warning_for_asm, .Ltmp11-warning_for_asm
	.cfi_endproc

	.globl	_fatal_insn
	.align	16, 0x90
	.type	_fatal_insn,@function
_fatal_insn:                            # @_fatal_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp21:
	.cfi_def_cfa_offset 48
.Ltmp22:
	.cfi_offset %rbx, -40
.Ltmp23:
	.cfi_offset %r12, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %rcx
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	error
	movq	global_dc(%rip), %rax
	movl	60(%rax), %ecx
	decq	%rcx
	movl	%ecx, 60(%rax)
	movq	%rbx, %rdi
	callq	debug_rtx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	fancy_abort
.Ltmp26:
	.size	_fatal_insn, .Ltmp26-_fatal_insn
	.cfi_endproc

	.globl	_fatal_insn_not_found
	.align	16, 0x90
	.type	_fatal_insn_not_found,@function
_fatal_insn_not_found:                  # @_fatal_insn_not_found
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp28:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movslq	40(%rax), %rdi
	movq	%rcx, %r8
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	testq	%rdi, %rdi
	jns	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str1, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn
.LBB3_2:                                # %if.else
	movabsq	$.L.str2, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn
.Ltmp29:
	.size	_fatal_insn_not_found, .Ltmp29-_fatal_insn_not_found
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%s"
	.size	.L.str, 3

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"unrecognizable insn:"
	.size	.L.str1, 21

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"insn does not satisfy its constraints:"
	.size	.L.str2, 39


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
