	.file	"getpwd.c.bc"
	.text
	.globl	getpwd
	.align	16, 0x90
	.type	getpwd,@function
getpwd:                                 # @getpwd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	subq	$296, %rsp              # imm = 0x128
.Ltmp9:
	.cfi_def_cfa_offset 336
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movq	getpwd.pwd(%rip), %r15
	testq	%r15, %r15
	jne	.LBB0_14
# BB#1:                                 # %land.lhs.true
	movl	getpwd.failure_errno(%rip), %ebx
	callq	__errno_location
	movq	%rax, %r14
	xorq	%r15, %r15
	testq	%rbx, %rbx
	movl	%ebx, (%r14)
	jne	.LBB0_14
# BB#2:                                 # %if.then
	movabsq	$.L.str, %rdi
	callq	getenv
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB0_8
# BB#3:                                 # %land.lhs.true3
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	jne	.LBB0_8
# BB#4:                                 # %land.lhs.true6
	leaq	8(%rsp), %rsi
	movq	%r15, %rdi
	callq	stat
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB0_8
# BB#5:                                 # %land.lhs.true10
	movabsq	$.L.str1, %rdi
	leaq	152(%rsp), %rsi
	callq	stat
	testq	%rbx, %rax
	jne	.LBB0_8
# BB#6:                                 # %land.lhs.true14
	movq	160(%rsp), %rax
	movq	16(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_8
# BB#7:                                 # %land.lhs.true18
	movq	152(%rsp), %rax
	movq	8(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB0_13
.LBB0_8:                                # %for.cond.preheader
	movq	$100, %rdi
	movq	$100, %rbx
	callq	xmalloc
	movq	%rax, %r15
	movq	$100, %rsi
	movq	%r15, %rdi
	jmp	.LBB0_10
	.align	16, 0x90
.LBB0_9:                                # %for.cond
                                        #   in Loop: Header=BB0_10 Depth=1
	addq	%rbx, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
.LBB0_10:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	callq	getcwd
	testq	%rax, %rax
	jne	.LBB0_13
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB0_10 Depth=1
	movl	(%r14), %r12d
	movq	%r15, %rdi
	callq	free
	cmpq	$34, %r12
	je	.LBB0_9
# BB#12:                                # %if.then29
	movl	%r12d, getpwd.failure_errno(%rip)
	movl	%r12d, (%r14)
	xorq	%r15, %r15
.LBB0_13:                               # %if.end31
	movq	%r15, getpwd.pwd(%rip)
.LBB0_14:                               # %if.end32
	movq	%r15, %rax
	addq	$296, %rsp              # imm = 0x128
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	getpwd, .Ltmp14-getpwd
	.cfi_endproc

	.type	getpwd.pwd,@object      # @getpwd.pwd
	.local	getpwd.pwd
	.comm	getpwd.pwd,8,8
	.type	getpwd.failure_errno,@object # @getpwd.failure_errno
	.local	getpwd.failure_errno
	.comm	getpwd.failure_errno,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"PWD"
	.size	.L.str, 4

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"."
	.size	.L.str1, 2


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
