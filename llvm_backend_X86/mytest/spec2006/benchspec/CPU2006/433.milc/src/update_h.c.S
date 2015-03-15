	.file	"update_h.c.bc"
	.text
	.globl	update_h
	.align	16, 0x90
	.type	update_h,@function
update_h:                               # @update_h
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	callq	rephase
	movq	$1840, %rsi             # imm = 0x730
	movq	%rbx, %rdi
	callq	imp_gauge_force
	movq	$1, %rdi
	callq	rephase
	movq	$2336, %rdi             # imm = 0x920
	movq	$2336, %rsi             # imm = 0x920
	movq	$1, %rdx
	callq	dslash_fn
	movl	nflavors(%rip), %esi
	movq	$2336, %rdx             # imm = 0x920
	movq	%rbx, %rdi
	popq	%rbx
	jmp	eo_fermion_force  # TAILCALL
.Ltmp4:
	.size	update_h, .Ltmp4-update_h
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
