	.file	"update.c.bc"
	.text
	.globl	update
	.align	16, 0x90
	.type	update,@function
update:                                 # @update
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
.Ltmp12:
	.cfi_offset %rbx, -48
.Ltmp13:
	.cfi_offset %r12, -40
.Ltmp14:
	.cfi_offset %r13, -32
.Ltmp15:
	.cfi_offset %r14, -24
.Ltmp16:
	.cfi_offset %r15, -16
	callq	ranmom
	movslq	steps(%rip), %rax
	testq	%rax, %rax
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	jle	.LBB0_5
# BB#1:
	xorq	%rbx, %rbx
	movq	$1, %r14
	movabsq	$4593671619917905920, %r12 # imm = 0x3FC0000000000000
	movabsq	$4602678819172646912, %r15 # imm = 0x3FE0000000000000
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	epsilon(%rip), %r13
	movl	nflavors(%rip), %edi
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	update_u
	movq	mass(%rip), %rsi
	movq	$2192, %rdi             # imm = 0x890
	movq	$2, %rdx
	callq	grsource_imp
	movq	epsilon(%rip), %rbx
	movl	nflavors(%rip), %edi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	update_u
	movq	rsqmin(%rip), %r8
	movl	niter(%rip), %ecx
	movq	mass(%rip), %rdx
	movq	$2192, %rdi             # imm = 0x890
	movq	$2336, %rsi             # imm = 0x920
	movq	$2, %r9
	leaq	24(%rsp), %rax
	movq	%rax, (%rsp)
	callq	ks_congrad
	movq	epsilon(%rip), %rdi
	movq	%rax, %rbx
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rbx
	callq	update_h
	movq	epsilon(%rip), %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	update_u
	xorq	%rdi, %rdi
	callq	rephase
	callq	reunitarize
	movq	$1, %rdi
	callq	rephase
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movslq	steps(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r14
	cmpq	%rsi, %rax
	jl	.LBB0_2
# BB#3:                                 # %for.end
	testq	%rsi, %rsi
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	jle	.LBB0_5
# BB#4:                                 # %if.then
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rax
	cqto
	idivq	%rsi
.LBB0_5:                                # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	update, .Ltmp17-update
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
