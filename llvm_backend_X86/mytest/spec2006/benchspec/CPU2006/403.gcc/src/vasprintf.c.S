	.file	"vasprintf.c.bc"
	.text
	.globl	vasprintf
	.align	16, 0x90
	.type	vasprintf,@function
vasprintf:                              # @vasprintf
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
	subq	$64, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 112
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
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r12
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%r15, 56(%rsp)
	movq	%r15, %rdi
	callq	strlen
	movq	16(%rbx), %rcx
	movq	%rax, %r14
	movq	%rcx, 48(%rsp)
	movq	(%rbx), %rax
	incq	%r14
	movq	8(%rbx), %rcx
	movq	%rcx, 40(%rsp)
	movq	%rax, 32(%rsp)
	movabsq	$.L.str1, %r13
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_41:                               # %sw.epilog.i
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%r15
	movq	%r15, 56(%rsp)
	movq	24(%rsp), %r12          # 8-byte Reload
.LBB0_1:                                # %while.cond.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_2 Depth 2
                                        #     Child Loop BB0_5 Depth 2
                                        #     Child Loop BB0_22 Depth 2
	movq	%r15, %rax
	.align	16, 0x90
.LBB0_2:                                # %while.cond.i
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB0_42
# BB#3:                                 # %while.body.i
                                        #   in Loop: Header=BB0_2 Depth=2
	leaq	1(%rax), %r15
	movq	%r15, 56(%rsp)
	movzbl	(%rax), %eax
	cmpq	$37, %rax
	movq	%r15, %rax
	jne	.LBB0_2
# BB#4:                                 # %while.cond6.preheader.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movsbq	(%r15), %rbx
	movq	$6, %rdx
	movabsq	$.L.str, %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	memchr
	testq	%rax, %rax
	je	.LBB0_7
	.align	16, 0x90
.LBB0_5:                                # %while.body9.i
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movsbq	1(%r15), %rbx
	incq	%r15
	movq	$6, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	memchr
	testq	%rax, %rax
	jne	.LBB0_5
# BB#6:                                 # %while.cond6.while.end_crit_edge.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, 56(%rsp)
.LBB0_7:                                # %while.end.i
                                        #   in Loop: Header=BB0_1 Depth=1
	andq	$255, %rbx
	cmpq	$42, %rbx
	jne	.LBB0_12
# BB#8:                                 # %if.then14.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%rsp), %rax
	incq	%r15
	movq	%r15, 56(%rsp)
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$40, %rcx
	ja	.LBB0_10
# BB#9:                                 # %vaarg.in_reg.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	48(%rsp), %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rsi, %rdx
	addq	$8, %rax
	movl	%eax, 32(%rsp)
	jmp	.LBB0_11
	.align	16, 0x90
.LBB0_12:                               # %if.else.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$10, %rdx
	movq	%r15, %rdi
	leaq	56(%rsp), %rsi
	callq	strtoul
	movq	56(%rsp), %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	addq	%rax, %r14
	movq	%r14, %rbx
	jmp	.LBB0_13
.LBB0_10:                               # %vaarg.in_mem.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %rdx
	leaq	8(%rdx), %rax
	movq	%rax, 40(%rsp)
.LBB0_11:                               # %vaarg.end.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	(%rdx), %rbx
	movq	$31, %rcx
	movq	%rbx, %rax
	sarq	%cl, %rax
	addq	%rax, %rbx
	xorq	%rax, %rbx
	addq	%r14, %rbx
.LBB0_13:                               # %if.end.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	(%r15), %eax
	cmpq	$46, %rax
	jne	.LBB0_21
# BB#14:                                # %if.then26.i
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	1(%r15), %rdi
	movq	%rdi, 56(%rsp)
	movzbl	1(%r15), %eax
	cmpq	$42, %rax
	jne	.LBB0_19
# BB#15:                                # %if.then31.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%rsp), %rax
	addq	$2, %r15
	movq	%r15, 56(%rsp)
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$40, %rcx
	ja	.LBB0_17
# BB#16:                                # %vaarg.in_reg37.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	48(%rsp), %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rsi, %rdx
	addq	$8, %rax
	movl	%eax, 32(%rsp)
	jmp	.LBB0_18
.LBB0_19:                               # %if.else47.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$10, %rdx
	leaq	56(%rsp), %rsi
	callq	strtoul
	movq	56(%rsp), %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	jmp	.LBB0_20
.LBB0_17:                               # %vaarg.in_mem39.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %rdx
	leaq	8(%rdx), %rax
	movq	%rax, 40(%rsp)
.LBB0_18:                               # %vaarg.end43.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	(%rdx), %rax
	movq	$31, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	addq	%rdx, %rax
	xorq	%rdx, %rax
.LBB0_20:                               # %while.cond54.preheader.i
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	%rax, %rbx
.LBB0_21:                               # %while.cond54.preheader.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movsbq	(%r15), %r12
	movq	$4, %rdx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	memchr
	testq	%rax, %rax
	je	.LBB0_24
	.align	16, 0x90
.LBB0_22:                               # %while.body58.i
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movsbq	1(%r15), %r12
	incq	%r15
	movq	$4, %rdx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	memchr
	testq	%rax, %rax
	jne	.LBB0_22
# BB#23:                                # %while.cond54.while.end60_crit_edge.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%r15, 56(%rsp)
.LBB0_24:                               # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	30(%rbx), %r14
	cmpq	$87, %r12
	jg	.LBB0_29
# BB#25:                                # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$69, %r12
	je	.LBB0_27
# BB#26:                                # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$71, %r12
	jne	.LBB0_41
	jmp	.LBB0_27
	.align	16, 0x90
.LBB0_29:                               # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$98, %r12
	jg	.LBB0_33
# BB#30:                                # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$88, %r12
	jne	.LBB0_41
	jmp	.LBB0_31
.LBB0_33:                               # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$-99, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$21, %r12
	ja	.LBB0_41
# BB#34:                                # %while.end60.i
                                        #   in Loop: Header=BB0_1 Depth=1
	jmpq	*.LJTI0_0(,%r12,8)
.LBB0_31:                               # %while.end60.sw.bb_crit_edge.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	32(%rsp), %eax
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$40, %rcx
	ja	.LBB0_32
# BB#40:                                # %vaarg.in_reg67.i
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$8, %rax
	movl	%eax, 32(%rsp)
	jmp	.LBB0_41
.LBB0_27:                               # %while.end60.sw.bb75_crit_edge172.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	36(%rsp), %eax
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$160, %rcx
	ja	.LBB0_35
# BB#28:                                # %vaarg.in_reg77.i
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$16, %rax
	addq	$337, %rbx              # imm = 0x151
	movl	%eax, 36(%rsp)
	movq	%rbx, %r14
	jmp	.LBB0_41
.LBB0_35:                               # %vaarg.in_mem79.i
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$8, 40(%rsp)
	addq	$337, %rbx              # imm = 0x151
	movq	%rbx, %r14
	jmp	.LBB0_41
.LBB0_32:                               # %vaarg.in_mem110.i
                                        #   in Loop: Header=BB0_1 Depth=1
	addq	$8, 40(%rsp)
	jmp	.LBB0_41
.LBB0_36:                               # %sw.bb86.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	32(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB0_38
# BB#37:                                # %vaarg.in_reg91.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	48(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 32(%rsp)
	jmp	.LBB0_39
.LBB0_38:                               # %vaarg.in_mem93.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 40(%rsp)
.LBB0_39:                               # %vaarg.end97.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	(%rcx), %rdi
	callq	strlen
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	addq	%rax, %r14
	jmp	.LBB0_41
.LBB0_42:                               # %while.end118.i
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, %rdi
	callq	malloc
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, (%r12)
	je	.LBB0_44
# BB#43:                                # %if.then125.i
	movq	%rcx, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	vsprintf
.LBB0_44:                               # %int_vasprintf.exit
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	vasprintf, .Ltmp17-vasprintf
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_31
	.quad	.LBB0_31
	.quad	.LBB0_27
	.quad	.LBB0_27
	.quad	.LBB0_27
	.quad	.LBB0_41
	.quad	.LBB0_31
	.quad	.LBB0_41
	.quad	.LBB0_41
	.quad	.LBB0_41
	.quad	.LBB0_41
	.quad	.LBB0_31
	.quad	.LBB0_31
	.quad	.LBB0_31
	.quad	.LBB0_41
	.quad	.LBB0_41
	.quad	.LBB0_36
	.quad	.LBB0_41
	.quad	.LBB0_31
	.quad	.LBB0_41
	.quad	.LBB0_41
	.quad	.LBB0_31

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"-+ #0"
	.size	.L.str, 6

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"hlL"
	.size	.L.str1, 4


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
