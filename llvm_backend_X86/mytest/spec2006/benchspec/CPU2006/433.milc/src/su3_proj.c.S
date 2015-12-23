	.file	"su3_proj.c.bc"
	.text
	.globl	su3_projector
	.align	16, 0x90
	.type	su3_projector,@function
su3_projector:                          # @su3_projector
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
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	%r14, 24(%rsp)          # 8-byte Spill
	addq	$40, %rbx
	movq	$8, %r13
	.align	16, 0x90
.LBB0_1:                                # %for.cond1.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	-8(%r14,%r13), %rdi
	movq	(%r14,%r13), %r14
	movq	(%r15), %rsi
	movq	8(%r15), %r12
	callq	float64_mul
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, -40(%rbx)
	movq	(%r15), %rsi
	movq	%r14, %rdi
	callq	float64_mul
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	-8(%rcx,%r13), %r14
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, -32(%rbx)
	movq	16(%r15), %rsi
	movq	24(%r15), %r12
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	callq	float64_mul
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%r13), %r14
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, -24(%rbx)
	movq	16(%r15), %rsi
	movq	%r14, %rdi
	callq	float64_mul
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	-8(%r12,%r13), %r14
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r14, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, -16(%rbx)
	movq	32(%r15), %rsi
	movq	40(%r15), %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	callq	float64_mul
	movq	(%r12,%r13), %r14
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rax, -8(%rcx)
	movq	32(%r15), %rsi
	movq	%r14, %rdi
	callq	float64_mul
	movq	%r12, %rcx
	movq	-8(%rcx,%r13), %rdi
	movq	%rcx, %r14
	movq	%rax, %r12
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, (%rbx)
	addq	$16, %r13
	addq	$48, %rbx
	cmpq	$56, %r13
	jne	.LBB0_1
# BB#2:                                 # %for.end48
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	su3_projector, .Ltmp17-su3_projector
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
