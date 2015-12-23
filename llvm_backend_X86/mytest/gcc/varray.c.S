	.file	"varray.c.bc"
	.text
	.globl	varray_init
	.align	16, 0x90
	.type	varray_init,@function
varray_init:                            # @varray_init
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
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r15, %rax
	imulq	%rbx, %rax
	leaq	32(%rax), %rdi
	movq	$1, %rsi
	callq	xcalloc
	movq	%rbx, (%rax)
	movq	$0, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	varray_init, .Ltmp10-varray_init
	.cfi_endproc

	.globl	varray_grow
	.align	16, 0x90
	.type	varray_grow,@function
varray_grow:                            # @varray_grow
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
.Ltmp22:
	.cfi_offset %rbx, -48
.Ltmp23:
	.cfi_offset %r12, -40
.Ltmp24:
	.cfi_offset %r13, -32
.Ltmp25:
	.cfi_offset %r14, -24
.Ltmp26:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %r13
	movq	%rsi, %rbx
	cmpq	%rbx, %r13
	je	.LBB1_3
# BB#1:                                 # %if.then
	movq	16(%r15), %r12
	movq	%r12, %r14
	imulq	%rbx, %r14
	leaq	32(%r14), %rsi
	movq	%r15, %rdi
	callq	xrealloc
	movq	%rax, %r15
	cmpq	%rbx, %r13
	movq	%rbx, (%r15)
	jae	.LBB1_3
# BB#2:                                 # %if.then5
	imulq	%r13, %r12
	leaq	32(%r12,%r15), %rdi
	subq	%r12, %r14
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	memset
.LBB1_3:                                # %if.end6
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp27:
	.size	varray_grow, .Ltmp27-varray_grow
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
