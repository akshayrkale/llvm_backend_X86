	.file	"m_mv_s_4dir.c.bc"
	.text
	.globl	mult_su3_mat_vec_sum_4dir
	.align	16, 0x90
	.type	mult_su3_mat_vec_sum_4dir,@function
mult_su3_mat_vec_sum_4dir:              # @mult_su3_mat_vec_sum_4dir
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
	movq	%r9, %r13
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%r13, %rdx
	callq	mult_su3_mat_vec
	leaq	144(%rbx), %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_mat_vec_sum
	leaq	288(%rbx), %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	mult_su3_mat_vec_sum
	addq	$432, %rbx              # imm = 0x1B0
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	mult_su3_mat_vec_sum  # TAILCALL
.Ltmp16:
	.size	mult_su3_mat_vec_sum_4dir, .Ltmp16-mult_su3_mat_vec_sum_4dir
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
