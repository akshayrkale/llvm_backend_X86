	.file	"byterevn.c.bc"
	.text
	.globl	byterevn
	.align	16, 0x90
	.type	byterevn,@function
byterevn:                               # @byterevn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_3
# BB#1:
	movq	$1, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rsi
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	-4(%rdi,%rax,4), %edx
	bswapq	%rdx
	movq	$32, %rcx
	shrq	%cl, %rdx
	movl	%edx, -4(%rdi,%rax,4)
	movq	%rax, %rcx
	andq	%r8, %rcx
	incq	%rax
	cmpq	%rsi, %rcx
	jne	.LBB0_2
.LBB0_3:                                # %for.end
	retq
.Ltmp0:
	.size	byterevn, .Ltmp0-byterevn
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
