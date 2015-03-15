	.file	"m_amv_4vec.c.bc"
	.text
	.globl	mult_adj_su3_mat_4vec
	.align	16, 0x90
	.type	mult_adj_su3_mat_4vec,@function
mult_adj_su3_mat_4vec:                  # @mult_adj_su3_mat_4vec
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
	movq	%r9, %r14
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rsi, %r13
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R13<kill>
	callq	mult_adj_su3_mat_vec
	leaq	144(%rbx), %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	mult_adj_su3_mat_vec
	leaq	288(%rbx), %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	mult_adj_su3_mat_vec
	addq	$432, %rbx              # imm = 0x1B0
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	mult_adj_su3_mat_vec  # TAILCALL
.Ltmp16:
	.size	mult_adj_su3_mat_4vec, .Ltmp16-mult_adj_su3_mat_4vec
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
