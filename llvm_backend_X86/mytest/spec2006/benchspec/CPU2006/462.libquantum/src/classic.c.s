	.file	"mytest/spec2006/benchspec/CPU2006/462.libquantum/src/classic.c.bc"
	.text
	.globl	quantum_ipow
	.align	16, 0x90
	.type	quantum_ipow,@function
quantum_ipow:                           # @quantum_ipow
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rax
	jle	.LBB0_3
# BB#1:
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rsi
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	incq	%rdx
	imulq	%rdi, %rax
	movq	%rdx, %rcx
	andq	%r8, %rcx
	cmpq	%rsi, %rcx
	jne	.LBB0_2
.LBB0_3:                                # %for.end
	retq
.Ltmp0:
	.size	quantum_ipow, .Ltmp0-quantum_ipow
	.cfi_endproc

	.globl	quantum_gcd
	.align	16, 0x90
	.type	quantum_gcd,@function
quantum_gcd:                            # @quantum_gcd
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rdx
	movq	%rdi, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rdx
	je	.LBB1_1
	.align	16, 0x90
.LBB1_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	testq	%r8, %rdx
	movq	%rdi, %rax
	jne	.LBB1_2
# BB#3:     