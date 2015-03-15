	.file	"xstrerror.c.bc"
	.text
	.globl	xstrerror
	.align	16, 0x90
	.type	xstrerror,@function
xstrerror:                              # @xstrerror
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	strerror
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$xstrerror_buf, %rbx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	sprintf
.LBB0_2:                                # %if.end
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	xstrerror, .Ltmp8-xstrerror
	.cfi_endproc

	.type	xstrerror_buf,@object   # @xstrerror_buf
	.local	xstrerror_buf
	.comm	xstrerror_buf,43,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"undocumented error #%d"
	.size	.L.str, 23


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
