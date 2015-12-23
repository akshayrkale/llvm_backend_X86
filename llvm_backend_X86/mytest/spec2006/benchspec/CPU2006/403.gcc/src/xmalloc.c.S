	.file	"xmalloc.c.bc"
	.text
	.globl	xmalloc_set_program_name
	.align	16, 0x90
	.type	xmalloc_set_program_name,@function
xmalloc_set_program_name:               # @xmalloc_set_program_name
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, name(%rip)
	retq
.Ltmp0:
	.size	xmalloc_set_program_name, .Ltmp0-xmalloc_set_program_name
	.cfi_endproc

	.globl	xmalloc_failed
	.align	16, 0x90
	.type	xmalloc_failed,@function
xmalloc_failed:                         # @xmalloc_failed
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp2:
	.cfi_def_cfa_offset 16
	movq	name(%rip), %rdx
	movzbl	(%rdx), %eax
	movq	stderr(%rip), %r9
	movq	%rdi, %r8
	testq	%rax, %rax
	je	.LBB1_2
# BB#1:
	movabsq	$.L.str1, %rcx
	jmp	.LBB1_3
.LBB1_2:                                # %select.mid
	movabsq	$.L.str2, %rcx
.LBB1_3:                                # %select.end
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%r9, %rdi
	callq	fprintf
	movq	$1, %rdi
	callq	xexit
.Ltmp3:
	.size	xmalloc_failed, .Ltmp3-xmalloc_failed
	.cfi_endproc

	.globl	xmalloc
	.align	16, 0x90
	.type	xmalloc,@function
xmalloc:                                # @xmalloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 16
.Ltmp7:
	.cfi_offset %rbx, -16
	testq	%rdi, %rdi
	movq	$1, %rbx
	je	.LBB2_2
# BB#1:                                 # %entry
	movq	%rdi, %rbx
.LBB2_2:                                # %entry
	movq	%rbx, %rdi
	callq	malloc
	testq	%rax, %rax
	je	.LBB2_4
# BB#3:                                 # %if.end2
	popq	%rbx
	retq
.LBB2_4:                                # %if.then1
	movq	%rbx, %rdi
	callq	xmalloc_failed
.Ltmp8:
	.size	xmalloc, .Ltmp8-xmalloc
	.cfi_endproc

	.globl	xcalloc
	.align	16, 0x90
	.type	xcalloc,@function
xcalloc:                                # @xcalloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp12:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp14:
	.cfi_def_cfa_offset 32
.Ltmp15:
	.cfi_offset %rbx, -24
.Ltmp16:
	.cfi_offset %r14, -16
	testq	%rdi, %rdi
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB3_2
# BB#1:                                 # %entry
	movq	%rdx, %rcx
.LBB3_2:                                # %entry
	testq	%rsi, %rsi
	jne	.LBB3_4
# BB#3:                                 # %entry
	movq	%rdx, %rax
.LBB3_4:                                # %entry
	orq	%rax, %rcx
	movq	$1, %r14
	movq	$1, %rbx
	jne	.LBB3_6
# BB#5:                                 # %entry
	movq	%rsi, %rbx
.LBB3_6:                                # %entry
	jne	.LBB3_8
# BB#7:                                 # %entry
	movq	%rdi, %r14
.LBB3_8:                                # %entry
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	calloc
	testq	%rax, %rax
	je	.LBB3_10
# BB#9:                                 # %if.end3
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB3_10:                               # %if.then2
	imulq	%rbx, %r14
	movq	%r14, %rdi
	callq	xmalloc_failed
.Ltmp17:
	.size	xcalloc, .Ltmp17-xcalloc
	.cfi_endproc

	.globl	xrealloc
	.align	16, 0x90
	.type	xrealloc,@function
xrealloc:                               # @xrealloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 16
.Ltmp21:
	.cfi_offset %rbx, -16
	testq	%rsi, %rsi
	movq	$1, %rbx
	je	.LBB4_2
# BB#1:                                 # %entry
	movq	%rsi, %rbx
.LBB4_2:                                # %entry
	testq	%rdi, %rdi
	je	.LBB4_3
# BB#4:                                 # %if.else
	movq	%rbx, %rsi
	callq	realloc
	jmp	.LBB4_5
.LBB4_3:                                # %if.then1
	movq	%rbx, %rdi
	callq	malloc
.LBB4_5:                                # %if.end3
	testq	%rax, %rax
	je	.LBB4_7
# BB#6:                                 # %if.end6
	popq	%rbx
	retq
.LBB4_7:                                # %if.then5
	movq	%rbx, %rdi
	callq	xmalloc_failed
.Ltmp22:
	.size	xrealloc, .Ltmp22-xrealloc
	.cfi_endproc

	.type	name,@object            # @name
	.data
	.align	8
name:
	.quad	.L.str2
	.size	name, 8

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\n%s%sCannot allocate %lu bytes\n"
	.size	.L.str, 32

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	": "
	.size	.L.str1, 3

	.type	.L.str2,@object         # @.str2
.L.str2:
	.zero	1
	.size	.L.str2, 1


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
