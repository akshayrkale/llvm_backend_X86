	.file	"m_amat_hwvec.c.bc"
	.text
	.globl	mult_adj_su3_mat_hwvec
	.align	16, 0x90
	.type	mult_adj_su3_mat_hwvec,@function
mult_adj_su3_mat_hwvec:                 # @mult_adj_su3_mat_hwvec
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
	movq	%rsi, %rbx
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> RBX<kill>
                                        # kill: RDX<def> R14<kill>
	callq	mult_adj_su3_mat_vec
	addq	$48, %rbx
	addq	$48, %r14
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	mult_adj_su3_mat_vec  # TAILCALL
.Ltmp10:
	.size	mult_adj_su3_mat_hwvec, .Ltmp10-mult_adj_su3_mat_hwvec
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
