	.file	"mkdeps.c.bc"
	.text
	.globl	deps_init
	.align	16, 0x90
	.type	deps_init,@function
deps_init:                              # @deps_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	popq	%rdx
	retq
.Ltmp2:
	.size	deps_init, .Ltmp2-deps_init
	.cfi_endproc

	.globl	deps_free
	.align	16, 0x90
	.type	deps_free,@function
deps_free:                              # @deps_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp9:
	.cfi_def_cfa_offset 32
.Ltmp10:
	.cfi_offset %rbx, -32
.Ltmp11:
	.cfi_offset %r14, -24
.Ltmp12:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB1_5
# BB#1:                                 # %for.cond.preheader
	movl	8(%r14), %eax
	testq	%rax, %rax
	je	.LBB1_4
# BB#2:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	(%rdi,%rax,8), %rdi
	callq	free
	incq	%rbx
	movl	8(%r14), %eax
	movq	(%r14), %rdi
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	jb	.LBB1_3
.LBB1_4:                                # %for.end
	callq	free
.LBB1_5:                                # %if.end
	movq	16(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB1_10
# BB#6:                                 # %for.cond5.preheader
	movl	24(%r14), %eax
	testq	%rax, %rax
	je	.LBB1_9
# BB#7:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_8:                                # %for.body7
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	(%rdi,%rax,8), %rdi
	callq	free
	incq	%rbx
	movl	24(%r14), %eax
	movq	16(%r14), %rdi
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	jb	.LBB1_8
.LBB1_9:                                # %for.end13
	callq	free
.LBB1_10:                               # %if.end15
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp13:
	.size	deps_free, .Ltmp13-deps_free
	.cfi_endproc

	.globl	deps_add_target
	.align	16, 0x90
	.type	deps_add_target,@function
deps_add_target:                        # @deps_add_target
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp20:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp21:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp22:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp23:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 48
.Ltmp25:
	.cfi_offset %rbx, -48
.Ltmp26:
	.cfi_offset %r12, -40
.Ltmp27:
	.cfi_offset %r13, -32
.Ltmp28:
	.cfi_offset %r14, -24
.Ltmp29:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	8(%r14), %eax
	movl	12(%r14), %ecx
	movq	%rdx, %r15
	movq	%rsi, %r13
	cmpq	%rcx, %rax
	jne	.LBB2_2
# BB#1:                                 # %if.then
	leaq	4(%rax,%rax), %rax
	movl	%eax, 12(%r14)
	movq	(%r14), %rdi
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	andq	%rax, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, (%r14)
.LBB2_2:                                # %if.end
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %r15
	je	.LBB2_28
# BB#3:                                 # %if.then6
	movzbl	(%r13), %ebx
	testq	%rbx, %rbx
	movq	$1, %rdi
	je	.LBB2_15
# BB#4:                                 # %for.body.i.preheader
	leaq	-1(%r13), %rax
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	.align	16, 0x90
.LBB2_5:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_9 Depth 2
	movq	$56, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	andq	$255, %rbx
	cmpq	$9, %rbx
	je	.LBB2_6
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB2_5 Depth=1
	andq	%r12, %rdi
	cmpq	$36, %rdi
	jne	.LBB2_8
# BB#12:                                # %sw.bb8.i
                                        #   in Loop: Header=BB2_5 Depth=1
	incq	%rsi
	movq	%rsi, %rdi
	jmp	.LBB2_13
	.align	16, 0x90
.LBB2_6:                                #   in Loop: Header=BB2_5 Depth=1
	movq	%rax, %rcx
	movq	%rsi, %rdi
	jmp	.LBB2_9
	.align	16, 0x90
.LBB2_8:                                # %for.body.i
                                        #   in Loop: Header=BB2_5 Depth=1
	cmpq	$32, %rdi
	movq	%rax, %rcx
	movq	%rsi, %rdi
	jne	.LBB2_13
	.align	16, 0x90
.LBB2_9:                                # %for.cond1.i
                                        #   Parent Loop BB2_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r13, %rcx
	jb	.LBB2_11
# BB#10:                                # %land.rhs.i
                                        #   in Loop: Header=BB2_9 Depth=2
	movzbl	(%rcx), %esi
	incq	%rdi
	decq	%rcx
	cmpq	$92, %rsi
	je	.LBB2_9
	jmp	.LBB2_13
	.align	16, 0x90
.LBB2_11:                               # %for.end.critedge.i
                                        #   in Loop: Header=BB2_5 Depth=1
	incq	%rdi
.LBB2_13:                               # %for.inc10.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movzbl	1(%rdx), %ebx
	leaq	1(%rdi), %rsi
	incq	%rdx
	incq	%rax
	testq	%rbx, %rbx
	jne	.LBB2_5
# BB#14:                                # %for.cond.for.end13_crit_edge.i
	addq	$2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
.LBB2_15:                               # %for.end13.i
	callq	xmalloc
	movzbl	(%r13), %edx
	testq	%rdx, %rdx
	movq	%rax, %rsi
	je	.LBB2_27
# BB#16:                                # %for.body17.i.preheader
	leaq	-1(%r13), %r10
	movq	$92, %r9
	movq	$36, %r8
	movq	%rax, %rsi
	movq	%r13, %rbx
	.align	16, 0x90
.LBB2_17:                               # %for.body17.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_21 Depth 2
	movq	$56, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	andq	$255, %rdx
	cmpq	$9, %rdx
	je	.LBB2_18
# BB#19:                                # %for.body17.i
                                        #   in Loop: Header=BB2_17 Depth=1
	andq	%r12, %rdi
	cmpq	$36, %rdi
	jne	.LBB2_20
# BB#24:                                # %sw.bb35.i
                                        #   in Loop: Header=BB2_17 Depth=1
	movb	%r8b, (%rsi)
	jmp	.LBB2_25
	.align	16, 0x90
.LBB2_18:                               #   in Loop: Header=BB2_17 Depth=1
	movq	%r10, %rcx
	jmp	.LBB2_21
	.align	16, 0x90
.LBB2_20:                               # %for.body17.i
                                        #   in Loop: Header=BB2_17 Depth=1
	cmpq	$32, %rdi
	movq	%r10, %rcx
	jne	.LBB2_26
	.align	16, 0x90
.LBB2_21:                               # %for.cond21.i
                                        #   Parent Loop BB2_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r13, %rcx
	jb	.LBB2_23
# BB#22:                                # %land.rhs24.i
                                        #   in Loop: Header=BB2_21 Depth=2
	movzbl	(%rcx), %edx
	decq	%rcx
	cmpq	$92, %rdx
	movb	%r9b, (%rsi)
	leaq	1(%rsi), %rsi
	je	.LBB2_21
	jmp	.LBB2_26
	.align	16, 0x90
.LBB2_23:                               # %for.end33.critedge.i
                                        #   in Loop: Header=BB2_17 Depth=1
	movb	%r9b, (%rsi)
.LBB2_25:                               # %sw.epilog37.i
                                        #   in Loop: Header=BB2_17 Depth=1
	incq	%rsi
.LBB2_26:                               # %sw.epilog37.i
                                        #   in Loop: Header=BB2_17 Depth=1
	movzbl	(%rbx), %ecx
	movb	%cl, (%rsi)
	movzbl	1(%rbx), %edx
	incq	%rsi
	incq	%rbx
	incq	%r10
	testq	%rdx, %rdx
	jne	.LBB2_17
.LBB2_27:                               # %munge.exit
	xorq	%rcx, %rcx
	movb	%cl, (%rsi)
	jmp	.LBB2_29
.LBB2_28:                               # %if.else
	movq	%r13, %rdi
	callq	xstrdup
.LBB2_29:                               # %if.end9
	movl	8(%r14), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 8(%r14)
	movq	(%r14), %rdx
	movq	%rax, (%rdx,%rcx,8)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp30:
	.size	deps_add_target, .Ltmp30-deps_add_target
	.cfi_endproc

	.globl	deps_add_default_target
	.align	16, 0x90
	.type	deps_add_default_target,@function
deps_add_default_target:                # @deps_add_default_target
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp34:
	.cfi_def_cfa_offset 16
.Ltmp35:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp36:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	pushq	%rax
.Ltmp37:
	.cfi_offset %rbx, -40
.Ltmp38:
	.cfi_offset %r14, -32
.Ltmp39:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movl	8(%r14), %eax
	testq	%rax, %rax
	jne	.LBB3_7
# BB#1:                                 # %if.end
	movzbl	(%rsi), %eax
	testq	%rax, %rax
	je	.LBB3_2
# BB#3:                                 # %if.else
	movq	%rsi, %rdi
	callq	lbasename
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	strlen
	movq	%rsp, %rbx
	addq	$18, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	$46, %rsi
	movq	%rbx, %rdi
	callq	strrchr
	testq	%rax, %rax
	jne	.LBB3_5
# BB#4:                                 # %if.then8
	movq	%rbx, %rdi
	callq	strlen
	addq	%rbx, %rax
.LBB3_5:                                # %if.end10
	xorq	%rcx, %rcx
	movb	%cl, 2(%rax)
	movq	$28462, %rcx            # imm = 0x6F2E
	movw	%cx, (%rax)
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	jmp	.LBB3_6
.LBB3_2:                                # %if.then2
	movabsq	$.L.str, %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
.LBB3_6:                                # %if.end12
	callq	deps_add_target
.LBB3_7:                                # %if.end12
	leaq	-24(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp40:
	.size	deps_add_default_target, .Ltmp40-deps_add_default_target
	.cfi_endproc

	.globl	deps_add_dep
	.align	16, 0x90
	.type	deps_add_dep,@function
deps_add_dep:                           # @deps_add_dep
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp46:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp48:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp50:
	.cfi_def_cfa_offset 48
.Ltmp51:
	.cfi_offset %rbx, -40
.Ltmp52:
	.cfi_offset %r12, -32
.Ltmp53:
	.cfi_offset %r14, -24
.Ltmp54:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	(%r12), %edx
	movq	%rdi, %r14
	testq	%rdx, %rdx
	movq	$1, %rdi
	je	.LBB4_12
# BB#1:                                 # %for.body.i.preheader
	leaq	-1(%r12), %rax
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r12, %rsi
	.align	16, 0x90
.LBB4_2:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_6 Depth 2
	movq	$56, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	andq	$255, %rdx
	cmpq	$9, %rdx
	je	.LBB4_3
# BB#4:                                 # %for.body.i
                                        #   in Loop: Header=BB4_2 Depth=1
	andq	%r8, %rdi
	cmpq	$36, %rdi
	jne	.LBB4_5
# BB#9:                                 # %sw.bb8.i
                                        #   in Loop: Header=BB4_2 Depth=1
	incq	%rbx
	movq	%rbx, %rdi
	jmp	.LBB4_10
	.align	16, 0x90
.LBB4_3:                                #   in Loop: Header=BB4_2 Depth=1
	movq	%rax, %rcx
	movq	%rbx, %rdi
	jmp	.LBB4_6
	.align	16, 0x90
.LBB4_5:                                # %for.body.i
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$32, %rdi
	movq	%rax, %rcx
	movq	%rbx, %rdi
	jne	.LBB4_10
	.align	16, 0x90
.LBB4_6:                                # %for.cond1.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r12, %rcx
	jb	.LBB4_8
# BB#7:                                 # %land.rhs.i
                                        #   in Loop: Header=BB4_6 Depth=2
	movzbl	(%rcx), %edx
	incq	%rdi
	decq	%rcx
	cmpq	$92, %rdx
	je	.LBB4_6
	jmp	.LBB4_10
	.align	16, 0x90
.LBB4_8:                                # %for.end.critedge.i
                                        #   in Loop: Header=BB4_2 Depth=1
	incq	%rdi
.LBB4_10:                               # %for.inc10.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movzbl	1(%rsi), %edx
	leaq	1(%rdi), %rbx
	incq	%rsi
	incq	%rax
	testq	%rdx, %rdx
	jne	.LBB4_2
# BB#11:                                # %for.cond.for.end13_crit_edge.i
	addq	$2, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
.LBB4_12:                               # %for.end13.i
	callq	xmalloc
	movzbl	(%r12), %esi
	movq	%rax, %r15
	testq	%rsi, %rsi
	movq	%r15, %rdx
	je	.LBB4_24
# BB#13:                                # %for.body17.i.preheader
	leaq	-1(%r12), %rax
	movq	$92, %r10
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	$36, %r8
	movq	%r15, %rdx
	movq	%r12, %rbx
	.align	16, 0x90
.LBB4_14:                               # %for.body17.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_18 Depth 2
	movq	$56, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	andq	$255, %rsi
	cmpq	$9, %rsi
	je	.LBB4_15
# BB#16:                                # %for.body17.i
                                        #   in Loop: Header=BB4_14 Depth=1
	andq	%r9, %rdi
	cmpq	$36, %rdi
	jne	.LBB4_17
# BB#21:                                # %sw.bb35.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movb	%r8b, (%rdx)
	jmp	.LBB4_22
	.align	16, 0x90
.LBB4_15:                               #   in Loop: Header=BB4_14 Depth=1
	movq	%rax, %rcx
	jmp	.LBB4_18
	.align	16, 0x90
.LBB4_17:                               # %for.body17.i
                                        #   in Loop: Header=BB4_14 Depth=1
	cmpq	$32, %rdi
	movq	%rax, %rcx
	jne	.LBB4_23
	.align	16, 0x90
.LBB4_18:                               # %for.cond21.i
                                        #   Parent Loop BB4_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r12, %rcx
	jb	.LBB4_20
# BB#19:                                # %land.rhs24.i
                                        #   in Loop: Header=BB4_18 Depth=2
	movzbl	(%rcx), %esi
	decq	%rcx
	cmpq	$92, %rsi
	movb	%r10b, (%rdx)
	leaq	1(%rdx), %rdx
	je	.LBB4_18
	jmp	.LBB4_23
	.align	16, 0x90
.LBB4_20:                               # %for.end33.critedge.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movb	%r10b, (%rdx)
.LBB4_22:                               # %sw.epilog37.i
                                        #   in Loop: Header=BB4_14 Depth=1
	incq	%rdx
.LBB4_23:                               # %sw.epilog37.i
                                        #   in Loop: Header=BB4_14 Depth=1
	movzbl	(%rbx), %ecx
	movb	%cl, (%rdx)
	movzbl	1(%rbx), %esi
	incq	%rdx
	incq	%rbx
	incq	%rax
	testq	%rsi, %rsi
	jne	.LBB4_14
.LBB4_24:                               # %munge.exit
	xorq	%rax, %rax
	movb	%al, (%rdx)
	movl	24(%r14), %ecx
	movl	28(%r14), %eax
	cmpq	%rax, %rcx
	jne	.LBB4_25
# BB#26:                                # %if.then
	leaq	8(%rcx,%rcx), %rax
	movl	%eax, 28(%r14)
	movq	16(%r14), %rdi
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	andq	%rax, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, 16(%r14)
	movl	24(%r14), %ecx
	jmp	.LBB4_27
.LBB4_25:                               # %munge.exit.if.end_crit_edge
	movq	16(%r14), %rax
.LBB4_27:                               # %if.end
	leaq	1(%rcx), %rdx
	movl	%edx, 24(%r14)
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	%r15, (%rax,%rdx,8)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp55:
	.size	deps_add_dep, .Ltmp55-deps_add_dep
	.cfi_endproc

	.globl	deps_write
	.align	16, 0x90
	.type	deps_write,@function
deps_write:                             # @deps_write
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp64:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp65:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp67:
	.cfi_def_cfa_offset 80
.Ltmp68:
	.cfi_offset %rbx, -48
.Ltmp69:
	.cfi_offset %r12, -40
.Ltmp70:
	.cfi_offset %r13, -32
.Ltmp71:
	.cfi_offset %r14, -24
.Ltmp72:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdx, %rax
	andq	%rdi, %rax
	je	.LBB5_1
# BB#2:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB5_3
.LBB5_1:
	movabsq	$0, %rcx
.LBB5_3:                                # %entry
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	cmpq	$34, %rax
	sbbq	%rax, %rax
	testq	%rcx, %rax
	movq	$34, %rax
	jne	.LBB5_5
# BB#4:                                 # %entry
	movq	%rdx, %rax
.LBB5_5:                                # %entry
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	8(%r12), %eax
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	%rdi, %r15
	je	.LBB5_19
# BB#6:                                 # %for.body.lr.ph
	xorq	%r13, %r13
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r15, %rax
	je	.LBB5_7
# BB#10:
	andq	%r15, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB5_11:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rax
	movq	%r13, %r12
	andq	%r15, %r12
	movq	(%rax,%r12,8), %rdi
	callq	strlen
	movq	%rax, %rbx
	addq	%rbx, %r14
	movq	%r14, %rax
	andq	%r15, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB5_13
# BB#12:                                # %if.then6.us
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	$4, %rsi
	movq	$1, %rdx
	movabsq	$.L.str2, %rdi
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	fwrite
	incq	%rbx
	movq	%rbx, %r14
.LBB5_13:                               # %if.end9.us
                                        #   in Loop: Header=BB5_11 Depth=1
	testq	%r15, %r13
	jne	.LBB5_15
# BB#14:                                #   in Loop: Header=BB5_11 Depth=1
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB5_16
	.align	16, 0x90
.LBB5_15:                               # %if.then11.us
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	$32, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	putc
	incq	%r14
.LBB5_16:                               # %if.end13.us
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	(%rax,%r12,8), %rdi
	movq	%rcx, %r12
	movq	%rbx, %rsi
	callq	fputs
	movl	8(%r12), %eax
	incq	%r13
	movq	%r13, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_11
	jmp	.LBB5_19
.LBB5_7:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB5_8:                                # %if.end9
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	movq	(%rax,%rcx,8), %r12
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r14
	addq	%r13, %r14
	movq	%rbx, %r13
	andq	%r15, %r13
	jne	.LBB5_17
# BB#9:                                 #   in Loop: Header=BB5_8 Depth=1
	movq	24(%rsp), %r15          # 8-byte Reload
	jmp	.LBB5_18
	.align	16, 0x90
.LBB5_17:                               # %if.then11
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$32, %rdi
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	putc
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	(%rax,%r13,8), %r12
	incq	%r14
.LBB5_18:                               # %if.end13
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	fputs
	movq	16(%rsp), %rdx          # 8-byte Reload
	movl	8(%rdx), %eax
	incq	%rbx
	movq	%rbx, %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	movq	%r14, %r13
	movq	%rdx, %r12
	jb	.LBB5_8
.LBB5_19:                               # %for.end
	movq	$58, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	putc
	movq	$32, %rdi
	movq	%rbx, %rsi
	callq	putc
	movl	24(%r12), %eax
	testq	%rax, %rax
	movq	%r15, %r13
	movq	%rbx, %r15
	je	.LBB5_32
# BB#20:                                # %for.body25.lr.ph
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r13, %rax
	je	.LBB5_21
# BB#25:                                # %for.body25.us.preheader
	addq	$2, %r14
	xorq	%rbx, %rbx
	andq	%r13, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB5_26:                               # %for.body25.us
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r12), %rax
	movq	%rbx, %r12
	andq	%r13, %r12
	movq	(%rax,%r12,8), %rdi
	callq	strlen
	movq	%r13, %rcx
	movq	%rax, %r13
	addq	%r13, %r14
	movq	%r14, %rax
	andq	%rcx, %rax
	movq	%rcx, %r15
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB5_28
# BB#27:                                # %if.then35.us
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	$4, %rsi
	movq	$1, %rdx
	movabsq	$.L.str2, %rdi
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	fwrite
	incq	%r13
	movq	%r13, %r14
.LBB5_28:                               # %if.end38.us
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	%r15, %r13
	testq	%r13, %rbx
	jne	.LBB5_30
# BB#29:                                #   in Loop: Header=BB5_26 Depth=1
	movq	24(%rsp), %r15          # 8-byte Reload
	jmp	.LBB5_31
	.align	16, 0x90
.LBB5_30:                               # %if.then40.us
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	$32, %rdi
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	putc
	incq	%r14
.LBB5_31:                               # %if.end43.us
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rax
	movq	(%rax,%r12,8), %rdi
	movq	%rcx, %r12
	movq	%r15, %rsi
	callq	fputs
	movl	24(%r12), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_26
	jmp	.LBB5_32
.LBB5_21:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB5_22:                               # %if.end38
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %r14
	andq	%r13, %r14
	je	.LBB5_24
# BB#23:                                # %if.then40
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	$32, %rdi
	movq	%r15, %rsi
	callq	putc
.LBB5_24:                               # %if.end43
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	16(%r12), %rax
	movq	(%rax,%r14,8), %rdi
	movq	%r15, %rsi
	callq	fputs
	movl	24(%r12), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_22
.LBB5_32:                               # %for.end50
	movq	$10, %rdi
	movq	%r15, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	putc  # TAILCALL
.Ltmp73:
	.size	deps_write, .Ltmp73-deps_write
	.cfi_endproc

	.globl	deps_phony_targets
	.align	16, 0x90
	.type	deps_phony_targets,@function
deps_phony_targets:                     # @deps_phony_targets
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp83:
	.cfi_def_cfa_offset 48
.Ltmp84:
	.cfi_offset %rbx, -40
.Ltmp85:
	.cfi_offset %r12, -32
.Ltmp86:
	.cfi_offset %r14, -24
.Ltmp87:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	24(%r14), %eax
	movq	%rsi, %r15
	cmpq	$2, %rax
	jb	.LBB6_3
# BB#1:                                 # %for.body.lr.ph
	movq	$1, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	movq	16(%r14), %rax
	movq	%rbx, %rcx
	andq	%r12, %rcx
	movq	(%rax,%rcx,8), %rdi
	movq	%r15, %rsi
	callq	fputs
	movq	$58, %rdi
	movq	%r15, %rsi
	callq	putc
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	movl	24(%r14), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r12, %rcx
	cmpq	%rax, %rcx
	jb	.LBB6_2
.LBB6_3:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp88:
	.size	deps_phony_targets, .Ltmp88-deps_phony_targets
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"-"
	.size	.L.str, 2

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	".o"
	.size	.L.str1, 3

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	" \\\n "
	.size	.L.str2, 5


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
