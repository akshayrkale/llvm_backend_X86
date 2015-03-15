	.file	"xstrdup.c.bc"
	.text
	.globl	xstrdup
	.align	16, 0x90
	.type	xstrdup,@function
xstrdup:                                # @xstrdup
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
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	strlen
	movq	%rax, %rbx
	incq	%rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	xstrdup, .Ltmp10-xstrdup
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
