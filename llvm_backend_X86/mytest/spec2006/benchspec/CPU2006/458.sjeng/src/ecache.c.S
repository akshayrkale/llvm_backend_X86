	.file	"ecache.c.bc"
	.text
	.globl	storeECache
	.align	16, 0x90
	.type	storeECache,@function
storeECache:                            # @storeECache
	.cfi_startproc
# BB#0:                                 # %entry
	movl	hash(%rip), %esi
	movl	ECacheSize(%rip), %ecx
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%rcx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	ECache(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rcx
	movl	%esi, (%rax,%rcx,4)
	movl	hold_hash(%rip), %edx
	movl	%edx, 4(%rax,%rcx,4)
	movl	%edi, 8(%rax,%rcx,4)
	retq
.Ltmp0:
	.size	storeECache, .Ltmp0-storeECache
	.cfi_endproc

	.globl	checkECache
	.align	16, 0x90
	.type	checkECache,@function
checkECache:                            # @checkECache
	.cfi_startproc
# BB#0:                                 # %entry
	movl	ECacheProbes(%rip), %eax
	incq	%rax
	movl	%eax, ECacheProbes(%rip)
	movl	hash(%rip), %r8d
	movl	ECacheSize(%rip), %ecx
	xorq	%rdx, %rdx
	movq	%r8, %rax
	divq	%rcx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	ECache(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rcx
	movl	(%rax,%rcx,4), %edx
	cmpq	%r8, %rdx
	jne	.LBB1_3
# BB#1:                                 # %land.lhs.true
	movl	4(%rax,%rcx,4), %r8d
	movl	hold_hash(%rip), %edx
	cmpq	%rdx, %r8
	jne	.LBB1_3
# BB#2:                                 # %if.then
	movl	ECacheHits(%rip), %edx
	incq	%rdx
	movl	%edx, ECacheHits(%rip)
	movq	$1, %rdx
	movl	%edx, (%rsi)
	movl	8(%rax,%rcx,4), %eax
	movl	%eax, (%rdi)
.LBB1_3:                                # %if.end
	retq
.Ltmp1:
	.size	checkECache, .Ltmp1-checkECache
	.cfi_endproc

	.globl	reset_ecache
	.align	16, 0x90
	.type	reset_ecache,@function
reset_ecache:                           # @reset_ecache
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp3:
	.cfi_def_cfa_offset 16
	movslq	ECacheSize(%rip), %rax
	movq	ECache(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rdx
	xorq	%rsi, %rsi
	callq	memset
	popq	%rax
	retq
.Ltmp4:
	.size	reset_ecache, .Ltmp4-reset_ecache
	.cfi_endproc

	.globl	alloc_ecache
	.align	16, 0x90
	.type	alloc_ecache,@function
alloc_ecache:                           # @alloc_ecache
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp6:
	.cfi_def_cfa_offset 16
	movslq	ECacheSize(%rip), %rax
	movq	$2, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rdi
	callq	malloc
	testq	%rax, %rax
	movq	%rax, ECache(%rip)
	je	.LBB3_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB3_2:                                # %if.then
	movabsq	$.Lstr, %rax
	movq	%rax, %rdi
	callq	puts
	movq	$1, %rdi
	callq	exit
.Ltmp7:
	.size	alloc_ecache, .Ltmp7-alloc_ecache
	.cfi_endproc

	.globl	free_ecache
	.align	16, 0x90
	.type	free_ecache,@function
free_ecache:                            # @free_ecache
	.cfi_startproc
# BB#0:                                 # %entry
	movq	ECache(%rip), %rdi
	jmp	free  # TAILCALL
.Ltmp8:
	.size	free_ecache, .Ltmp8-free_ecache
	.cfi_endproc

	.type	ECache,@object          # @ECache
	.comm	ECache,8,8
	.type	ECacheProbes,@object    # @ECacheProbes
	.comm	ECacheProbes,4,4
	.type	ECacheHits,@object      # @ECacheHits
	.comm	ECacheHits,4,4
	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"Out of memory allocating ECache."
	.size	.Lstr, 33


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
