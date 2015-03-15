	.file	"r_su2_hit_a.c.bc"
	.text
	.globl	right_su2_hit_a
	.align	16, 0x90
	.type	right_su2_hit_a,@function
right_su2_hit_a:                        # @right_su2_hit_a
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 48
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$4, %rcx
	shlq	%cl, %rbx
	leaq	(%r14,%rbx), %rsi
	movq	$4, %rcx
	shlq	%cl, %r12
	leaq	(%r14,%r12), %rdx
                                        # kill: RDI<def> R15<kill>
	callq	mult_su2_mat_vec_elem_a
	leaq	48(%r14,%rbx), %rsi
	leaq	48(%r14,%r12), %rdx
	movq	%r15, %rdi
	callq	mult_su2_mat_vec_elem_a
	leaq	96(%r14,%rbx), %rsi
	leaq	96(%r14,%r12), %rdx
	movq	%r15, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	mult_su2_mat_vec_elem_a  # TAILCALL
.Ltmp14:
	.size	right_su2_hit_a, .Ltmp14-right_su2_hit_a
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
