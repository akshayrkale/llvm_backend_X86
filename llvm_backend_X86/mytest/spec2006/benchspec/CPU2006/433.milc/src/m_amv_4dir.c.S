	.file	"m_amv_4dir.c.bc"
	.text
	.globl	mult_adj_su3_mat_vec_4dir
	.align	16, 0x90
	.type	mult_adj_su3_mat_vec_4dir,@function
mult_adj_su3_mat_vec_4dir:              # @mult_adj_su3_mat_vec_4dir
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
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
                                        # kill: RDX<def> R15<kill>
	callq	mult_adj_su3_mat_vec
	leaq	144(%rbx), %rdi
	leaq	48(%r15), %rdx
	movq	%r14, %rsi
	callq	mult_adj_su3_mat_vec
	leaq	288(%rbx), %rdi
	leaq	96(%r15), %rdx
	movq	%r14, %rsi
	callq	mult_adj_su3_mat_vec
	addq	$432, %rbx              # imm = 0x1B0
	addq	$144, %r15
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	mult_adj_su3_mat_vec  # TAILCALL
.Ltmp10:
	.size	mult_adj_su3_mat_vec_4dir, .Ltmp10-mult_adj_su3_mat_vec_4dir
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
