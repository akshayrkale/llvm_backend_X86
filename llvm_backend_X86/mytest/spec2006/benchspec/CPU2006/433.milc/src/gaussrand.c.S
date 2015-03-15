	.file	"gaussrand.c.bc"
	.text
	.globl	gaussian_rand_no
	.align	16, 0x90
	.type	gaussian_rand_no,@function
gaussian_rand_no:                       # @gaussian_rand_no
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
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
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
	movzbl	gaussian_rand_no.iset(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_1
# BB#4:                                 # %if.else
	movq	gaussian_rand_no.gset(%rip), %rax
	xorq	%rcx, %rcx
	movb	%cl, gaussian_rand_no.iset(%rip)
	jmp	.LBB0_5
.LBB0_1:
	movq	%rdi, %r13
	movabsq	$-4616189618054758400, %r14 # imm = 0xBFF0000000000000
	.align	16, 0x90
.LBB0_2:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %r15
	movq	%r15, %rdi
	callq	myrand
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%r15, %r13
	callq	myrand
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r14
	callq	float64_add
	movq	%rax, %r15
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%r12, %rdi
	movabsq	$4607182418800017408, %rsi # imm = 0x3FF0000000000000
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB0_2
# BB#3:                                 # %do.end
	movq	%r12, %rdi
	callq	log
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	xorq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_div
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %rbx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, gaussian_rand_no.gset(%rip)
	movq	$1, %rax
	movb	%al, gaussian_rand_no.iset(%rip)
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
.LBB0_5:                                # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	gaussian_rand_no, .Ltmp17-gaussian_rand_no
	.cfi_endproc

	.type	gaussian_rand_no.iset,@object # @gaussian_rand_no.iset
	.local	gaussian_rand_no.iset
	.comm	gaussian_rand_no.iset,1,1
	.type	gaussian_rand_no.gset,@object # @gaussian_rand_no.gset
	.local	gaussian_rand_no.gset
	.comm	gaussian_rand_no.gset,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
