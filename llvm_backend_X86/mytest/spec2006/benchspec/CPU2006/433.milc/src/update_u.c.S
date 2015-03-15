	.file	"update_u.c.bc"
	.text
	.globl	update_u
	.align	16, 0x90
	.type	update_u,@function
update_u:                               # @update_u
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
	subq	$512, %rsp              # imm = 0x200
.Ltmp11:
	.cfi_def_cfa_offset 560
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
	movq	%rdi, %rbx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movabsq	$4602678819172646912, %rsi # imm = 0x3FE0000000000000
                                        # kill: RDI<def> RBX<kill>
	callq	float64_mul
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$4613937818241073152, %rsi # imm = 0x4008000000000000
	movq	%rbx, %rdi
	callq	float64_div
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$4598175219545276416, %rsi # imm = 0x3FD0000000000000
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4617315517961601024, %rsi # imm = 0x4014000000000000
	movq	%rbx, %rdi
	callq	float64_div
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$4618441417868443648, %rsi # imm = 0x4018000000000000
	movq	%rbx, %rdi
	callq	float64_div
	movslq	sites_on_node(%rip), %rcx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB0_5
# BB#1:                                 # %for.cond5.preheader.lr.ph
	movq	lattice(%rip), %rdx
	leaq	112(%rdx), %rcx
	xorq	%rdi, %rdi
	leaq	80(%rsp), %r12
	leaq	368(%rsp), %r13
	leaq	224(%rsp), %r8
	.align	16, 0x90
.LBB0_2:                                # %for.cond5.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	$1840, %r15             # imm = 0x730
	movq	%rcx, %rbx
	movq	%r12, %r14
	movq	%r8, %r12
	.align	16, 0x90
.LBB0_3:                                # %for.body7
                                        #   Parent Loop BB0_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r15), %rdi
	movq	%r14, %rsi
	callq	uncompress_anti_hermitian
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_matrix
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_matrix
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_matrix
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_matrix
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_matrix
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_matrix
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	su3mat_copy
	movq	72(%rsp), %rdx          # 8-byte Reload
	addq	$80, %r15
	addq	$144, %rbx
	cmpq	$2160, %r15             # imm = 0x870
	jne	.LBB0_3
# BB#4:                                 # %for.inc11
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%r12, %r8
	movq	8(%rsp), %rdi           # 8-byte Reload
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	movslq	sites_on_node(%rip), %rbx
	sarq	%cl, %rsi
	addq	$3200, %rdx             # imm = 0xC80
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	$3200, %rcx             # imm = 0xC80
	cmpq	%rbx, %rsi
	movq	%r14, %r12
	jl	.LBB0_2
.LBB0_5:                                # %for.end13
	xorq	%rax, %rax
	movl	%eax, valid_longlinks(%rip)
	movl	%eax, valid_fatlinks(%rip)
	addq	$512, %rsp              # imm = 0x200
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	update_u, .Ltmp17-update_u
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
