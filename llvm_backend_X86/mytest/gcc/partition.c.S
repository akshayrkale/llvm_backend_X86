	.file	"partition.c.bc"
	.text
	.globl	partition_new
	.align	16, 0x90
	.type	partition_new,@function
partition_new:                          # @partition_new
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
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rbx, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	-1(%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	leaq	32(,%rax,8), %rdi
	callq	xmalloc
	testq	%r14, %r14
	movl	%ebx, (%rax)
	jle	.LBB0_3
# BB#1:                                 # %for.body.lr.ph
	leaq	8(%rax), %rcx
	movq	$1, %r8
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rbx
	movq	$1, %rdi
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rdi), %rdx
	movl	%edx, (%rcx)
	movq	%rcx, 8(%rcx)
	movl	%r8d, 16(%rcx)
	movq	%rdi, %rdx
	andq	%rsi, %rdx
	incq	%rdi
	addq	$24, %rcx
	cmpq	%rbx, %rdx
	jne	.LBB0_2
.LBB0_3:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	partition_new, .Ltmp8-partition_new
	.cfi_endproc

	.globl	partition_delete
	.align	16, 0x90
	.type	partition_delete,@function
partition_delete:                       # @partition_delete
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	free  # TAILCALL
.Ltmp9:
	.size	partition_delete, .Ltmp9-partition_delete
	.cfi_endproc

	.globl	partition_union
	.align	16, 0x90
	.type	partition_union,@function
partition_union:                        # @partition_union
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %r9
	movl	8(%rdi,%r9,8), %r8d
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %rcx
	movl	8(%rdi,%rcx,8), %eax
	cmpq	%rax, %r8
	jne	.LBB2_2
# BB#1:
	movq	%r8, %rax
	retq
.LBB2_2:                                # %if.end
	movl	24(%rdi,%r9,8), %r9d
	movl	24(%rdi,%rcx,8), %ecx
	cmpq	%rcx, %r9
	movq	%rsi, %r9
	jb	.LBB2_4
# BB#3:                                 # %if.end
	movq	%rdx, %r9
.LBB2_4:                                # %if.end
	jb	.LBB2_6
# BB#5:                                 # %if.end
	movq	%rsi, %rdx
.LBB2_6:                                # %if.end
	jb	.LBB2_8
# BB#7:                                 # %if.end
	movq	%r8, %rax
.LBB2_8:                                # %if.end
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	leaq	(%r9,%r9,2), %r9
	movslq	8(%rdi,%r9,8), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	movl	24(%rdi,%rcx,8), %r8d
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %rcx
	movl	24(%rdi,%rcx,8), %esi
	addq	%r8, %rsi
	movl	%esi, 24(%rdi,%rcx,8)
	movl	%eax, 8(%rdi,%r9,8)
	movq	16(%rdi,%r9,8), %r8
	leaq	8(%rdi,%r9,8), %rcx
	cmpq	%rcx, %r8
	je	.LBB2_11
# BB#9:
	movq	%r8, %rsi
	.align	16, 0x90
.LBB2_10:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, (%rsi)
	movq	8(%rsi), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB2_10
.LBB2_11:                               # %for.end
	leaq	(%rdx,%rdx,2), %rcx
	movq	16(%rdi,%rcx,8), %rdx
	leaq	16(%rdi,%r9,8), %rsi
	movq	%r8, 16(%rdi,%rcx,8)
	movq	%rdx, (%rsi)
	retq
.Ltmp10:
	.size	partition_union, .Ltmp10-partition_union
	.cfi_endproc

	.globl	partition_print
	.align	16, 0x90
	.type	partition_print,@function
partition_print:                        # @partition_print
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 112
.Ltmp23:
	.cfi_offset %rbx, -48
.Ltmp24:
	.cfi_offset %r12, -40
.Ltmp25:
	.cfi_offset %r13, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movslq	(%r12), %r13
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%r13, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%r13, %rdx
	callq	memset
	movq	$2, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$91, %rdi
	movq	%r14, %rsi
	callq	fputc
	xorq	%rdx, %rdx
	movq	%r12, %r10
	movq	%r15, %r8
	testq	%r13, %r13
	jle	.LBB3_12
# BB#1:                                 # %for.body.lr.ph
	leaq	8(%r10), %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$1, %r9
	movabsq	$-6148914691236517205, %rsi # imm = 0xAAAAAAAAAAAAAAAB
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_4 Depth 2
                                        #     Child Loop BB3_6 Depth 2
	movzbl	(%r8,%rdx), %eax
	testq	%rax, %rax
	jne	.LBB3_11
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r14, %r15
	leaq	(%rdx,%rdx,2), %rax
	movslq	8(%r10,%rax,8), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	24(%r10,%rax,8), %r14
	testq	%r14, %r14
	movq	$1, %rax
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	jle	.LBB3_9
	.align	16, 0x90
.LBB3_4:                                # %for.body15
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%edx, -4(%rdi,%rax,4)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movb	%r9b, (%r8,%rdx)
	leaq	(%rdx,%rdx,2), %rcx
	movq	16(%r10,%rcx,8), %rdx
	subq	%rbx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	imulq	%rsi, %rdx
	movq	%rax, %rcx
	andq	%r13, %rcx
	movq	%r14, %r12
	andq	%r13, %r12
	incq	%rax
	cmpq	%r12, %rcx
	jne	.LBB3_4
# BB#5:                                 # %for.end
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$4, %rdx
	movq	%r14, %rsi
	movabsq	$elem_compare, %rcx
	callq	specqsort
	movq	$40, %rdi
	movq	%r15, %rsi
	movq	%rsi, %rbx
	callq	fputc
	xorq	%r15, %r15
	testq	%r14, %r14
	movq	%rbx, %r14
	jle	.LBB3_10
	.align	16, 0x90
.LBB3_6:                                # %for.body27
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r15
	movabsq	$.L.str, %rsi
	je	.LBB3_8
# BB#7:                                 # %for.body27
                                        #   in Loop: Header=BB3_6 Depth=2
	movabsq	$.L.str1, %rsi
.LBB3_8:                                # %for.body27
                                        #   in Loop: Header=BB3_6 Depth=2
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	(%rax,%r15,4), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	incq	%r15
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	%r12, %rax
	jne	.LBB3_6
	jmp	.LBB3_10
	.align	16, 0x90
.LBB3_9:                                # %for.end35.critedge
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$4, %rdx
	movq	%r14, %rsi
	movabsq	$elem_compare, %rcx
	callq	specqsort
	movq	$40, %rdi
	movq	%r15, %r14
	movq	%r14, %rsi
	callq	fputc
.LBB3_10:                               # %for.end35
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$41, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	$1, %r9
	movabsq	$-6148914691236517205, %rsi # imm = 0xAAAAAAAAAAAAAAAB
.LBB3_11:                               # %for.inc37
                                        #   in Loop: Header=BB3_2 Depth=1
	incq	%rdx
	movq	%rdx, %rax
	andq	%r13, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_2
.LBB3_12:                               # %for.end39
	movq	%r8, %rbx
	movq	$93, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	%rbx, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp28:
	.size	partition_print, .Ltmp28-partition_print
	.cfi_endproc

	.align	16, 0x90
	.type	elem_compare,@function
elem_compare:                           # @elem_compare
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	(%rdi), %rax
	movslq	(%rsi), %rcx
	cmpq	%rcx, %rax
	jge	.LBB4_2
# BB#1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.LBB4_2:                                # %if.else
	jle	.LBB4_3
# BB#4:                                 # %if.else
	movabsq	$1, %rax
	retq
.LBB4_3:
	movabsq	$0, %rax
	retq
.Ltmp29:
	.size	elem_compare, .Ltmp29-elem_compare
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d"
	.size	.L.str, 3

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	" %d"
	.size	.L.str1, 4


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
