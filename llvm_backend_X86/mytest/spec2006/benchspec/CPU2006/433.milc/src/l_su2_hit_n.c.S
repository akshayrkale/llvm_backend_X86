	.file	"l_su2_hit_n.c.bc"
	.text
	.globl	left_su2_hit_n
	.align	16, 0x90
	.type	left_su2_hit_n,@function
left_su2_hit_n:                         # @left_su2_hit_n
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
	movq	%rcx, %r15
	movq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rsi,%rsi,2), %r12
	movq	$4, %rcx
	shlq	%cl, %r12
	leaq	(%r15,%r12), %rsi
	leaq	(%rdx,%rdx,2), %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	leaq	(%r15,%rbx), %rdx
                                        # kill: RDI<def> R14<kill>
	callq	mult_su2_mat_vec_elem_n
	leaq	16(%r15,%r12), %rsi
	leaq	16(%r15,%rbx), %rdx
	movq	%r14, %rdi
	callq	mult_su2_mat_vec_elem_n
	leaq	32(%r15,%r12), %rsi
	leaq	32(%r15,%rbx), %rdx
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	mult_su2_mat_vec_elem_n  # TAILCALL
.Ltmp14:
	.size	left_su2_hit_n, .Ltmp14-left_su2_hit_n
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
