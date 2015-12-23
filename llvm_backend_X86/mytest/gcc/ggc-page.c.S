	.file	"ggc-page.c.bc"
	.text
	.globl	debug_print_page_list
	.align	16, 0x90
	.type	debug_print_page_list,@function
debug_print_page_list:                  # @debug_print_page_list
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
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	G(,%rbx,8), %rsi
	movq	G+272(,%rbx,8), %rdx
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	G(,%rbx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB0_3
# BB#1:
	movabsq	$.L.str1, %r14
	.align	16, 0x90
.LBB0_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	40(%rbx), %edx
	movzbl	42(%rbx), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	printf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_2
.LBB0_3:                                # %while.end
	movabsq	$.Lstr, %rdi
	callq	puts
	movq	stdout(%rip), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fflush  # TAILCALL
.Ltmp8:
	.size	debug_print_page_list, .Ltmp8-debug_print_page_list
	.cfi_endproc

	.globl	ggc_alloc
	.align	16, 0x90
	.type	ggc_alloc,@function
ggc_alloc:                              # @ggc_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp17:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp20:
	.cfi_def_cfa_offset 112
.Ltmp21:
	.cfi_offset %rbx, -48
.Ltmp22:
	.cfi_offset %r12, -40
.Ltmp23:
	.cfi_offset %r13, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	cmpq	$256, %rdi              # imm = 0x100
	ja	.LBB1_1
# BB#49:                                # %if.then
	movzbl	size_lookup(%rdi), %r12d
	jmp	.LBB1_3
.LBB1_1:
	movq	$9, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_2:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %r12
                                        # kill: RCX<def> R12<kill>
	andq	%rax, %rcx
	movq	object_size_table(,%rcx,8), %rdx
	leaq	1(%r12), %rcx
	cmpq	%rdi, %rdx
	jb	.LBB1_2
.LBB1_3:                                # %if.end
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r12, %r13
	andq	%r8, %r13
	movq	G(,%r13,8), %rbx
	testq	%rbx, %rbx
	je	.LBB1_5
# BB#4:                                 # %lor.lhs.false
	movzbl	42(%rbx), %eax
	testq	%rax, %rax
	je	.LBB1_5
# BB#37:                                # %if.else21
	movzbl	44(%rbx), %edi
	movq	$5, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	movq	%rdi, %rax
	movq	48(%rbx,%rdx,8), %r9
	andq	$31, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%r9, %rsi
	je	.LBB1_43
# BB#38:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB1_39:                               # %while.cond27
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
                                        # kill: RAX<def> RDX<kill>
	andq	%r8, %rax
	movq	48(%rbx,%rax,8), %rsi
	leaq	1(%rdx), %rax
	cmpq	$-1, %rsi
	je	.LBB1_39
# BB#40:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB1_41:                               # %while.cond36
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	$1, %rdi
                                        # kill: RCX<def> RAX<kill>
	shlq	%cl, %rdi
	leaq	1(%rax), %rcx
	testq	%rsi, %rdi
	jne	.LBB1_41
# BB#42:                                # %while.end46
	movq	$5, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	addq	%rax, %rdi
.LBB1_43:                               # %if.end47
	leaq	1(%rdi), %rcx
	movw	%cx, 44(%rbx)
	movq	object_size_table(,%r13,8), %rsi
	imulq	%rdi, %rsi
	andq	%r8, %rsi
	jmp	.LBB1_44
.LBB1_5:                                # %if.then12
	movl	objects_per_page_table(,%r13,4), %edx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	object_size_table(,%r13,8), %rax
	movq	G+2592(%rip), %r15
	leaq	32(%rdx), %rcx
	shrq	$5, %rcx
	imulq	%rdx, %rax
	cmpq	%r15, %rax
	movq	%r15, %r14
	jb	.LBB1_7
# BB#6:                                 # %if.then12
	movq	%rax, %r14
.LBB1_7:                                # %if.then12
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	leaq	48(,%rcx,8), %rdx
	movabsq	$G+2640, %rbx
	.align	16, 0x90
.LBB1_8:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB1_12
# BB#9:                                 # %for.body.i
                                        #   in Loop: Header=BB1_8 Depth=1
	movq	8(%rbx), %rcx
	cmpq	%r14, %rcx
	jne	.LBB1_8
# BB#10:                                # %if.then14.i
	movq	(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	16(%rbx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movzbl	46(%rbx), %eax
	movq	24(%rbx), %r15
	cmpq	%r13, %rax
	jne	.LBB1_11
# BB#51:                                # %if.end89.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	memset
	jmp	.LBB1_32
.LBB1_12:                               # %if.else24.i
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r13, 24(%rsp)          # 8-byte Spill
	cmpq	%r15, %r14
	jne	.LBB1_14
# BB#13:                                # %if.then28.i
	movq	$4, %rcx
	movq	%r15, %r13
	shlq	%cl, %r13
	jmp	.LBB1_15
.LBB1_14:                               # %if.else30.i
	leaq	-1(%r15,%r14), %r13
.LBB1_15:                               # %if.end33.i
	movq	%r13, %rdi
	callq	xmalloc
	movq	G+2592(%rip), %rdx
	leaq	-1(%rdx,%rax), %rcx
	movq	%rdx, %r8
	negq	%r8
	andq	%r8, %rcx
	movq	%rcx, %rdi
	subq	%rax, %rdi
	movq	%r15, %r9
	cmpq	%r15, %r14
	jne	.LBB1_17
# BB#16:                                # %if.then38.i
	leaq	-1(%rdx), %rbx
	leaq	(%rax,%r13), %rsi
	andq	%rbx, %rsi
	jmp	.LBB1_18
.LBB1_17:                               # %if.else42.i
	movq	%r13, %rsi
	subq	%r14, %rsi
	subq	%rdi, %rsi
.LBB1_18:                               # %if.end45.i
	movq	%r13, %rbx
	movq	%r13, %r10
	subq	%rsi, %rbx
	leaq	(%rax,%rbx), %r13
	cmpq	$32, %rdi
	jb	.LBB1_20
# BB#19:                                # %if.then49.i
	leaq	-32(%rcx), %r15
	jmp	.LBB1_24
.LBB1_20:                               # %if.else51.i
	testq	%rsi, %rsi
	jne	.LBB1_22
# BB#21:                                # %if.then54.i
	subq	%rdx, %rbx
	addq	%rax, %rbx
	movq	%rbx, %r13
	movq	%rdx, %rsi
.LBB1_22:                               # %if.end58.i
	cmpq	$31, %rsi
	jbe	.LBB1_50
# BB#23:                                # %if.end62.i
	movq	%r13, %r15
.LBB1_24:                               # %if.end64.i
	movq	G+2648(%rip), %rdx
	movq	%rdx, (%r15)
	movq	%rax, 8(%r15)
	movq	%r10, 16(%r15)
	xorq	%rax, %rax
	movl	%eax, 24(%r15)
	movq	%r15, G+2648(%rip)
	addq	%r10, G+2624(%rip)
	cmpq	%r9, %r14
	jne	.LBB1_25
# BB#26:                                # %if.then70.i
	movq	%r15, %rdx
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	G+2640(%rip), %rbx
	addq	%r8, %r13
	cmpq	%rcx, %r13
	je	.LBB1_27
# BB#28:                                # %for.body76.lr.ph.i
	movq	%rcx, %r15
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB1_29:                               # %for.body76.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdi
	movq	%rsi, %r14
	callq	xcalloc
	movq	%r14, %rsi
	movb	%r12b, 46(%rax)
	movq	G+2592(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%r13, 16(%rax)
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, 24(%rax)
	movq	%rbx, (%rax)
	subq	%rcx, %r13
	cmpq	%r15, %r13
	movq	%rax, %rbx
	jne	.LBB1_29
	jmp	.LBB1_30
.LBB1_25:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB1_31
.LBB1_11:                               # %if.else.i
	movq	%rbx, %rdi
	movq	%rdx, %rbx
	callq	free
	movq	%rbx, %rsi
	jmp	.LBB1_31
.LBB1_27:
	movq	%rbx, %rax
	movq	%rcx, %r15
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rdx, 56(%rsp)          # 8-byte Spill
.LBB1_30:                               # %for.end87.i
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%rax, G+2640(%rip)
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	56(%rsp), %r15          # 8-byte Reload
.LBB1_31:                               # %if.then92.i
	movq	$1, %rdi
	callq	xcalloc
	movq	%rax, %rbx
.LBB1_32:                               # %if.end94.i
	movq	%r14, 8(%rbx)
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	%r8, 16(%rbx)
	movzbl	G+2632(%rip), %eax
	movw	%ax, 40(%rbx)
	movb	%r12b, 46(%rbx)
	movq	40(%rsp), %rsi          # 8-byte Reload
	movw	%si, 42(%rbx)
	movq	$1, %rdi
	movw	%di, 44(%rbx)
	movq	%r15, 24(%rbx)
	movq	G+2600(%rip), %rcx
	movq	8(%r15), %rdx
	movq	%r8, %rax
	subq	%rdx, %rax
	shrq	%cl, %rax
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movl	24(%r15), %eax
	orq	%rdx, %rax
	movl	%eax, 24(%r15)
	movq	%rsi, %rcx
	andq	$31, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	shrq	$5, %rsi
	movq	%rax, 48(%rbx,%rsi,8)
	movq	$24, %rdx
	movq	$24, %rcx
	movq	%r8, %r14
	movq	G+2600(%rip), %rax
	shrq	%cl, %r14
	andq	$255, %r14
	movq	%rax, %rcx
	shrq	%cl, %r8
	subq	%rax, %rdx
	movq	$1, %r15
	movq	%rdx, %rcx
	shlq	%cl, %r15
	decq	%r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	G+544(,%r14,8), %rax
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	%r8, %r15
	testq	%rax, %rax
	movq	48(%rsp), %r8           # 8-byte Reload
	jne	.LBB1_34
# BB#33:                                # %if.then.i.i
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	movq	$8, %rsi
	movq	%r8, %r12
	callq	xcalloc
	movq	%r12, %r8
	movq	%rax, G+544(,%r14,8)
.LBB1_34:                               # %alloc_page.exit
	testq	%r8, %r8
	movq	%rbx, (%rax,%r15,8)
	jne	.LBB1_36
# BB#35:                                # %if.then15
	movq	%rbx, G+272(,%r13,8)
.LBB1_36:                               # %if.end18
	movq	%r8, (%rbx)
	movq	%rbx, G(,%r13,8)
	movq	$1, %rax
	movw	%ax, 44(%rbx)
	xorq	%rax, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB1_44:                               # %if.end56
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	andq	%r8, %rdx
	orq	%rdi, 48(%rbx,%rdx,8)
	movzbl	42(%rbx), %eax
	decq	%rax
	testq	$65535, %rax            # imm = 0xFFFF
	movw	%ax, 42(%rbx)
	jne	.LBB1_48
# BB#45:                                # %land.lhs.true
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB1_48
# BB#46:                                # %land.lhs.true68
	movzbl	42(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB1_48
# BB#47:                                # %if.then74
	movq	%rax, G(,%r13,8)
	movq	$0, (%rbx)
	movq	G+272(,%r13,8), %rax
	movq	%rbx, (%rax)
	movq	%rbx, G+272(,%r13,8)
.LBB1_48:                               # %if.end84
	movq	object_size_table(,%r13,8), %rcx
	movq	16(%rbx), %rax
	addq	%rcx, G+2608(%rip)
	addq	%rsi, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_50:                               # %if.then61.i
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.alloc_page, %rcx
	movq	$680, %rsi              # imm = 0x2A8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp26:
	.size	ggc_alloc, .Ltmp26-ggc_alloc
	.cfi_endproc

	.globl	ggc_set_mark
	.align	16, 0x90
	.type	ggc_set_mark,@function
ggc_set_mark:                           # @ggc_set_mark
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$21, %rcx
	movq	%rdi, %rdx
	movq	G+2600(%rip), %rax
	shrq	%cl, %rdx
	movq	%rdi, %rsi
	movq	%rax, %rcx
	shrq	%cl, %rsi
	movq	$24, %rcx
	subq	%rax, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	andq	$2040, %rdx             # imm = 0x7F8
	movq	G+544(%rdx), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	andq	%rsi, %rax
	movq	(%rdx,%rax,8), %r8
	movq	16(%r8), %rax
	movzbl	46(%r8), %ecx
	subq	%rax, %rdi
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	object_size_table(,%rcx,8)
	movq	%rax, %rdx
	movq	$1, %rax
	movq	$5, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$134217727, %rdi        # imm = 0x7FFFFFF
	movq	48(%r8,%rdi,8), %r9
	andq	$31, %rdx
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	testq	%r9, %rsi
	jne	.LBB2_2
# BB#1:                                 # %if.end
	orq	%r9, %rsi
	movq	%rsi, 48(%r8,%rdi,8)
	movzbl	42(%r8), %eax
	decq	%rax
	movw	%ax, 42(%r8)
	xorq	%rax, %rax
.LBB2_2:                                # %return
	retq
.Ltmp27:
	.size	ggc_set_mark, .Ltmp27-ggc_set_mark
	.cfi_endproc

	.globl	ggc_marked_p
	.align	16, 0x90
	.type	ggc_marked_p,@function
ggc_marked_p:                           # @ggc_marked_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$21, %rcx
	movq	%rdi, %rdx
	movq	G+2600(%rip), %rax
	shrq	%cl, %rdx
	movq	%rdi, %rsi
	movq	%rax, %rcx
	shrq	%cl, %rsi
	movq	$24, %rcx
	subq	%rax, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	andq	$2040, %rdx             # imm = 0x7F8
	movq	G+544(%rdx), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	andq	%rsi, %rax
	movq	(%rdx,%rax,8), %rsi
	movq	16(%rsi), %rax
	movzbl	46(%rsi), %ecx
	subq	%rax, %rdi
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	object_size_table(,%rcx,8)
	movq	$2, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$1073741816, %rdx       # imm = 0x3FFFFFF8
	movq	48(%rsi,%rdx), %rdx
	movq	$1, %rsi
	andq	$31, %rax
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	je	.LBB3_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB3_1:
	movabsq	$0, %rax
	retq
.Ltmp28:
	.size	ggc_marked_p, .Ltmp28-ggc_marked_p
	.cfi_endproc

	.globl	ggc_get_size
	.align	16, 0x90
	.type	ggc_get_size,@function
ggc_get_size:                           # @ggc_get_size
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$21, %rcx
	movq	%rdi, %rdx
	movq	G+2600(%rip), %rax
	shrq	%cl, %rdx
	movq	%rax, %rcx
	shrq	%cl, %rdi
	movq	$24, %rcx
	subq	%rax, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	andq	$2040, %rdx             # imm = 0x7F8
	movq	G+544(%rdx), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	andq	%rdi, %rax
	movq	(%rdx,%rax,8), %rax
	movzbl	46(%rax), %eax
	movq	object_size_table(,%rax,8), %rax
	retq
.Ltmp29:
	.size	ggc_get_size, .Ltmp29-ggc_get_size
	.cfi_endproc

	.globl	init_ggc
	.align	16, 0x90
	.type	init_ggc,@function
init_ggc:                               # @init_ggc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp31:
	.cfi_def_cfa_offset 16
	callq	getpagesize
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, G+2592(%rip)
	movq	%rax, %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	stdout(%rip), %rcx
	movq	%rax, G+2600(%rip)
	movq	%rcx, G+2656(%rip)
	movq	$4194304, G+2616(%rip)  # imm = 0x400000
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB5_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movq	%rax, object_size_table(,%rcx,8)
	incq	%rcx
	cmpq	$32, %rcx
	jne	.LBB5_1
# BB#2:                                 # %for.cond4.preheader
	movq	$208, object_size_table+256(%rip)
	movq	G+2592(%rip), %rcx
	movq	$48, object_size_table+264(%rip)
	movq	$-136, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_3:                                # %for.body21
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%rcx, %rax
	divq	object_size_table+272(%rsi,%rsi)
	testq	%rdi, %rax
	movq	$1, %rdx
	je	.LBB5_5
# BB#4:                                 # %for.body21
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	%rax, %rdx
.LBB5_5:                                # %for.body21
                                        #   in Loop: Header=BB5_3 Depth=1
	movl	%edx, objects_per_page_table+136(%rsi)
	addq	$4, %rsi
	jne	.LBB5_3
# BB#6:                                 # %for.body55.lr.ph
	movzbl	size_lookup+208(%rip), %eax
	movabsq	$size_lookup+208, %rcx
	movq	$32, %rdx
	andq	$255, %rax
	.align	16, 0x90
.LBB5_7:                                # %for.body55
                                        # =>This Inner Loop Header: Depth=1
	movb	%dl, (%rcx)
	movzbl	-1(%rcx), %esi
	decq	%rcx
	cmpq	%rax, %rsi
	je	.LBB5_7
# BB#8:                                 # %for.body55.lr.ph.1
	movzbl	size_lookup+48(%rip), %eax
	movabsq	$size_lookup+48, %rcx
	movq	$33, %rdx
	andq	$255, %rax
	.align	16, 0x90
.LBB5_9:                                # %for.body55.1
                                        # =>This Inner Loop Header: Depth=1
	movb	%dl, (%rcx)
	movzbl	-1(%rcx), %esi
	decq	%rcx
	cmpq	%rax, %rsi
	je	.LBB5_9
# BB#10:                                # %for.inc61.1
	popq	%rax
	retq
.Ltmp32:
	.size	init_ggc, .Ltmp32-init_ggc
	.cfi_endproc

	.globl	ggc_push_context
	.align	16, 0x90
	.type	ggc_push_context,@function
ggc_push_context:                       # @ggc_push_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp34:
	.cfi_def_cfa_offset 16
	movzbl	G+2632(%rip), %eax
	incq	%rax
	testq	$65535, %rax            # imm = 0xFFFF
	movw	%ax, G+2632(%rip)
	je	.LBB6_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB6_2:                                # %if.then
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.ggc_push_context, %rcx
	movq	$1156, %rsi             # imm = 0x484
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp35:
	.size	ggc_push_context, .Ltmp35-ggc_push_context
	.cfi_endproc

	.globl	ggc_pop_context
	.align	16, 0x90
	.type	ggc_pop_context,@function
ggc_pop_context:                        # @ggc_pop_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
.Ltmp47:
	.cfi_offset %rbx, -48
.Ltmp48:
	.cfi_offset %r12, -40
.Ltmp49:
	.cfi_offset %r13, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movzbl	G+2632(%rip), %r14d
	decq	%r14
	movw	%r14w, G+2632(%rip)
	movq	$2, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r14, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	.align	16, 0x90
.LBB7_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_2 Depth 2
                                        #       Child Loop BB7_9 Depth 3
                                        #         Child Loop BB7_11 Depth 4
	movq	G(,%r15,8), %rbx
	jmp	.LBB7_2
	.align	16, 0x90
.LBB7_4:                                # %if.then
                                        #   in Loop: Header=BB7_2 Depth=2
	movw	%r14w, 40(%rbx)
	movq	(%rbx), %rbx
.LBB7_2:                                # %for.body
                                        #   Parent Loop BB7_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_9 Depth 3
                                        #         Child Loop BB7_11 Depth 4
	testq	%rbx, %rbx
	je	.LBB7_19
# BB#3:                                 # %for.body6
                                        #   in Loop: Header=BB7_2 Depth=2
	movzbl	40(%rbx), %eax
	cmpq	%r12, %rax
	ja	.LBB7_4
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB7_2 Depth=2
	jne	.LBB7_18
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_2 Depth=2
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB7_18
# BB#7:                                 # %if.then16
                                        #   in Loop: Header=BB7_2 Depth=2
	movzbl	46(%rbx), %eax
	movl	objects_per_page_table(,%rax,4), %eax
	incq	%rax
	movq	%rax, %r8
	andq	%r13, %r8
	movw	%ax, 42(%rbx)
	leaq	31(%r8), %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB7_16
# BB#8:                                 #   in Loop: Header=BB7_2 Depth=2
	movq	$1, %rsi
	xorq	%rcx, %rcx
	jmp	.LBB7_9
	.align	16, 0x90
.LBB7_14:                               # %for.inc21.i.for.body.i_crit_edge
                                        #   in Loop: Header=BB7_9 Depth=3
	movq	32(%rbx), %rdi
	incq	%rsi
.LBB7_9:                                # %for.body.i
                                        #   Parent Loop BB7_1 Depth=1
                                        #     Parent Loop BB7_2 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_11 Depth 4
	movq	(%rdi,%rcx,8), %rax
	movq	48(%rbx,%rcx,8), %rdi
	orq	%rax, %rdi
	movq	%rdi, 48(%rbx,%rcx,8)
	je	.LBB7_13
# BB#10:                                # %for.body16.lr.ph.i
                                        #   in Loop: Header=BB7_9 Depth=3
	movzbl	42(%rbx), %eax
	.align	16, 0x90
.LBB7_11:                               # %for.body16.i
                                        #   Parent Loop BB7_1 Depth=1
                                        #     Parent Loop BB7_2 Depth=2
                                        #       Parent Loop BB7_9 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rdi, %rcx
	andq	$1, %rcx
	andq	$65535, %rax            # imm = 0xFFFF
	subq	%rcx, %rax
	movq	$1, %rcx
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	jne	.LBB7_11
# BB#12:                                # %for.cond15.for.inc21_crit_edge.i
                                        #   in Loop: Header=BB7_9 Depth=3
	movw	%ax, 42(%rbx)
.LBB7_13:                               # %for.inc21.i
                                        #   in Loop: Header=BB7_9 Depth=3
	movq	%rsi, %rcx
	andq	%r13, %rcx
	cmpq	%rdx, %rcx
	jb	.LBB7_14
# BB#15:                                # %for.cond.for.end22_crit_edge.i
                                        #   in Loop: Header=BB7_2 Depth=2
	movzbl	42(%rbx), %eax
.LBB7_16:                               # %for.end22.i
                                        #   in Loop: Header=BB7_2 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	%r8, %rax
	jae	.LBB7_21
# BB#17:                                # %ggc_recalculate_in_use_p.exit
                                        #   in Loop: Header=BB7_2 Depth=2
	movq	32(%rbx), %rdi
	callq	free
	movq	$0, 32(%rbx)
.LBB7_18:                               # %for.inc
                                        #   in Loop: Header=BB7_2 Depth=2
	movq	(%rbx), %rbx
	jmp	.LBB7_2
	.align	16, 0x90
.LBB7_19:                               # %for.inc20
                                        #   in Loop: Header=BB7_1 Depth=1
	incq	%r15
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$34, %rax
	jb	.LBB7_1
# BB#20:                                # %for.end21
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_21:                               # %if.then.i
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.ggc_recalculate_in_use_p, %rcx
	movq	$1194, %rsi             # imm = 0x4AA
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp52:
	.size	ggc_pop_context, .Ltmp52-ggc_pop_context
	.cfi_endproc

	.globl	ggc_collect
	.align	16, 0x90
	.type	ggc_collect,@function
ggc_collect:                            # @ggc_collect
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp61:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp62:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp63:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp64:
	.cfi_def_cfa_offset 80
.Ltmp65:
	.cfi_offset %rbx, -48
.Ltmp66:
	.cfi_offset %r12, -40
.Ltmp67:
	.cfi_offset %r13, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	G+2608(%rip), %rdi
	callq	int64_to_float64
	movq	G+2616(%rip), %rdi
	movq	%rax, %rbx
	callq	int64_to_float64
	movabsq	$4608533498688228557, %rsi # imm = 0x3FF4CCCCCCCCCCCD
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB8_65
# BB#1:                                 # %if.end
	movq	$1, %rdi
	callq	timevar_push
	movl	quiet_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_2
.LBB8_3:                                # %if.end4
	movq	$0, G+2608(%rip)
	movq	G+2640(%rip), %rax
	testq	%rax, %rax
	je	.LBB8_9
# BB#4:
	movabsq	$G+2640, %rbx
.LBB8_6:                                # %while.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_7 Depth 2
	movq	%rax, %rdi
	.align	16, 0x90
.LBB8_7:                                # %while.body.i
                                        #   Parent Loop BB8_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rdi), %rax
	movl	24(%rax), %ecx
	movq	(%rdi), %rax
	testq	%rcx, %rcx
	jne	.LBB8_5
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB8_7 Depth=2
	movq	%rax, (%rbx)
	callq	free
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB8_7
	jmp	.LBB8_9
	.align	16, 0x90
.LBB8_5:                                # %while.cond.outer.loopexit.i
                                        #   in Loop: Header=BB8_6 Depth=1
	testq	%rax, %rax
	movq	%rdi, %rbx
	jne	.LBB8_6
.LBB8_9:                                # %while.cond3.preheader.i
	movq	G+2648(%rip), %rax
	testq	%rax, %rax
	movq	$2, %rcx
	je	.LBB8_16
# BB#10:
	movabsq	$G+2648, %rbx
	movq	%rax, %rcx
	jmp	.LBB8_11
.LBB8_13:                               # %while.body5.lr.ph.i
                                        #   in Loop: Header=BB8_11 Depth=1
	movq	%rax, %rcx
	.align	16, 0x90
.LBB8_11:                               # %while.body5.i
                                        # =>This Inner Loop Header: Depth=1
	movl	24(%rcx), %edx
	movq	(%rcx), %rax
	testq	%rdx, %rdx
	jne	.LBB8_12
# BB#14:                                # %if.then8.i
                                        #   in Loop: Header=BB8_11 Depth=1
	movq	%rax, (%rbx)
	movq	16(%rcx), %rax
	subq	%rax, G+2624(%rip)
	movq	8(%rcx), %rdi
	callq	free
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB8_11
	jmp	.LBB8_15
.LBB8_12:                               # %while.cond3.outer.loopexit.i
                                        #   in Loop: Header=BB8_11 Depth=1
	testq	%rax, %rax
	movq	%rcx, %rbx
	jne	.LBB8_13
.LBB8_15:
	movq	$2, %rcx
	.align	16, 0x90
.LBB8_16:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_18 Depth 2
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movl	objects_per_page_table(,%rcx,4), %edx
	movq	G(,%rcx,8), %rbx
	leaq	32(%rdx), %r14
	movq	$2, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	je	.LBB8_24
# BB#17:                                # %for.body9.lr.ph.i
                                        #   in Loop: Header=BB8_16 Depth=1
	movabsq	$4611686018427387896, %rax # imm = 0x3FFFFFFFFFFFFFF8
	andq	%rax, %r14
	movq	%rdx, %rcx
	andq	$31, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$5, %rcx
	movq	%rdx, %r12
	movq	%rdx, %r15
	shrq	%cl, %r12
	.align	16, 0x90
.LBB8_18:                               # %for.body9.i
                                        #   Parent Loop BB8_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	40(%rbx), %eax
	movzbl	G+2632(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB8_19
# BB#20:                                # %if.then.i15
                                        #   in Loop: Header=BB8_18 Depth=2
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB8_22
# BB#21:                                # %if.then14.i
                                        #   in Loop: Header=BB8_18 Depth=2
	movq	%r14, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	movq	%rdi, 32(%rbx)
.LBB8_22:                               # %if.end.i
                                        #   in Loop: Header=BB8_18 Depth=2
	leaq	48(%rbx), %r13
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	memcpy
	jmp	.LBB8_23
	.align	16, 0x90
.LBB8_19:                               # %for.body9.if.end17_crit_edge.i
                                        #   in Loop: Header=BB8_18 Depth=2
	leaq	48(%rbx), %r13
.LBB8_23:                               # %if.end17.i
                                        #   in Loop: Header=BB8_18 Depth=2
	movw	%r15w, 42(%rbx)
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%r14, %rdx
	callq	memset
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 48(%rbx,%r12,8)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB8_18
.LBB8_24:                               # %for.inc23.i
                                        #   in Loop: Header=BB8_16 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$34, %rcx
	jne	.LBB8_16
# BB#25:                                # %clear_marks.exit
	callq	ggc_mark_roots
	movq	$2, %r10
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB8_26:                               # %for.body.i21
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_28 Depth 2
                                        #     Child Loop BB8_50 Depth 2
                                        #       Child Loop BB8_53 Depth 3
                                        #         Child Loop BB8_55 Depth 4
	movq	G(,%r10,8), %rbx
	testq	%rbx, %rbx
	je	.LBB8_61
# BB#27:                                # %do.body.preheader.i
                                        #   in Loop: Header=BB8_26 Depth=1
	movq	%r10, %r8
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	G+272(,%r8,8), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movl	objects_per_page_table(,%r8,4), %edi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB8_28:                               # %do.body.i
                                        #   Parent Loop BB8_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %r12
	movzbl	42(%r12), %ecx
	movq	object_size_table(,%r8,8), %rax
	movq	(%r12), %rbx
	movq	%rdi, %rdx
	subq	%rcx, %rdx
	imulq	%rdx, %rax
	addq	%rax, G+2608(%rip)
	movzbl	40(%r12), %eax
	movzbl	G+2632(%rip), %edx
	cmpq	%rdx, %rax
	movq	%r12, %rax
	jb	.LBB8_47
# BB#29:                                # %if.else.i
                                        #   in Loop: Header=BB8_28 Depth=2
	cmpq	%rcx, %rdi
	jne	.LBB8_38
# BB#30:                                # %if.then22.i
                                        #   in Loop: Header=BB8_28 Depth=2
	testq	%r15, %r15
	jne	.LBB8_32
# BB#31:                                # %if.then23.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%rbx, G(,%r8,8)
	jmp	.LBB8_33
	.align	16, 0x90
.LBB8_38:                               # %if.else37.i
                                        #   in Loop: Header=BB8_28 Depth=2
	testq	%rcx, %rcx
	jne	.LBB8_43
# BB#39:                                # %if.then42.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	G+272(,%r8,8), %rax
	cmpq	%rax, %r12
	movq	%r12, %rax
	je	.LBB8_47
# BB#40:                                # %if.then47.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	$0, (%r12)
	movq	G+272(,%r8,8), %rax
	movq	%r12, (%rax)
	movq	%r12, G+272(,%r8,8)
	testq	%r15, %r15
	jne	.LBB8_42
# BB#41:                                # %if.then55.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%rbx, G(,%r8,8)
	xorq	%rax, %rax
	jmp	.LBB8_47
.LBB8_32:                               # %if.else26.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%rbx, (%r15)
.LBB8_33:                               # %if.end28.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	G+272(,%r8,8), %rax
	cmpq	%rax, %r12
	jne	.LBB8_35
# BB#34:                                # %if.then33.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%r15, G+272(,%r8,8)
.LBB8_35:                               # %if.end36.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	16(%r12), %rdi
	movq	$24, %rcx
	movq	%rdi, %r14
	shrq	%cl, %r14
	movq	G+2600(%rip), %rsi
	andq	$255, %r14
	movq	%rsi, %rcx
	shrq	%cl, %rdi
	movq	$24, %rdx
	subq	%rsi, %rdx
	movq	$1, %r13
	movq	%rdx, %rcx
	shlq	%cl, %r13
	decq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	G+544(,%r14,8), %rax
	movq	$32, %rcx
	sarq	%cl, %r13
	andq	%rdi, %r13
	testq	%rax, %rax
	jne	.LBB8_37
# BB#36:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	$1, %rdi
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	8(%rsp), %r8            # 8-byte Reload
	movq	%rax, G+544(,%r14,8)
	movq	G+2600(%rip), %rsi
.LBB8_37:                               # %free_page.exit.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	$0, (%rax,%r13,8)
	movq	24(%r12), %rdx
	movq	16(%r12), %rax
	movq	8(%rdx), %rcx
	subq	%rcx, %rax
	movq	%rsi, %rcx
	shrq	%cl, %rax
	movl	24(%rdx), %esi
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	notq	%rdi
	andq	%rdi, %rsi
	movl	%esi, 24(%rdx)
	movq	G+2640(%rip), %rax
	movq	%rax, (%r12)
	movq	%r12, G+2640(%rip)
	movq	%r15, %rax
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB8_47
.LBB8_43:                               # %if.else62.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	G(,%r8,8), %rax
	cmpq	%rax, %r12
	movq	%r12, %rax
	je	.LBB8_47
# BB#44:                                # %if.then67.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%rbx, (%r15)
	movq	G(,%r8,8), %rax
	movq	%rax, (%r12)
	movq	%r12, G(,%r8,8)
	movq	G+272(,%r8,8), %rax
	cmpq	%r12, %rax
	jne	.LBB8_46
# BB#45:                                # %if.then79.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%r15, G+272(,%r8,8)
	jmp	.LBB8_46
.LBB8_42:                               # %if.else58.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%rbx, (%r15)
.LBB8_46:                               # %if.end86.i
                                        #   in Loop: Header=BB8_28 Depth=2
	movq	%r15, %rax
	.align	16, 0x90
.LBB8_47:                               # %if.end86.i
                                        #   in Loop: Header=BB8_28 Depth=2
	cmpq	%rsi, %r12
	movq	%rax, %r15
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	jne	.LBB8_28
# BB#48:                                # %for.cond90.preheader.i
                                        #   in Loop: Header=BB8_26 Depth=1
	movq	G(,%r8,8), %rax
	movq	%r8, %r10
	testq	%rax, %rax
	je	.LBB8_61
# BB#49:                                # %for.body92.lr.ph.i
                                        #   in Loop: Header=BB8_26 Depth=1
	movzbl	G+2632(%rip), %r8d
	.align	16, 0x90
.LBB8_50:                               # %for.body92.i
                                        #   Parent Loop BB8_26 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB8_53 Depth 3
                                        #         Child Loop BB8_55 Depth 4
	movzbl	40(%rax), %ecx
	movq	%r8, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	%rdx, %rcx
	je	.LBB8_60
# BB#51:                                # %if.then98.i
                                        #   in Loop: Header=BB8_50 Depth=2
	movzbl	46(%rax), %ecx
	movl	objects_per_page_table(,%rcx,4), %edx
	incq	%rdx
	movq	%rdx, %r9
	andq	%r11, %r9
	movw	%dx, 42(%rax)
	leaq	31(%r9), %rdi
	movq	$5, %rcx
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	je	.LBB8_59
# BB#52:                                # %for.body.lr.ph.i.i
                                        #   in Loop: Header=BB8_50 Depth=2
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB8_53:                               # %for.body.i.i
                                        #   Parent Loop BB8_26 Depth=1
                                        #     Parent Loop BB8_50 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB8_55 Depth 4
	movq	32(%rax), %rdx
	movq	(%rdx,%rcx,8), %rsi
	movq	48(%rax,%rcx,8), %rdx
	orq	%rsi, %rdx
	movq	%rdx, 48(%rax,%rcx,8)
	je	.LBB8_57
# BB#54:                                # %for.body16.lr.ph.i.i
                                        #   in Loop: Header=BB8_53 Depth=3
	movzbl	42(%rax), %esi
	.align	16, 0x90
.LBB8_55:                               # %for.body16.i.i
                                        #   Parent Loop BB8_26 Depth=1
                                        #     Parent Loop BB8_50 Depth=2
                                        #       Parent Loop BB8_53 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rdx, %rcx
	andq	$1, %rcx
	andq	$65535, %rsi            # imm = 0xFFFF
	subq	%rcx, %rsi
	movq	$1, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB8_55
# BB#56:                                # %for.cond15.for.inc21_crit_edge.i.i
                                        #   in Loop: Header=BB8_53 Depth=3
	movw	%si, 42(%rax)
.LBB8_57:                               # %for.inc21.i.i
                                        #   in Loop: Header=BB8_53 Depth=3
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r11, %rcx
	cmpq	%rdi, %rcx
	jb	.LBB8_53
# BB#58:                                # %for.cond.for.end22_crit_edge.i.i
                                        #   in Loop: Header=BB8_50 Depth=2
	movzbl	42(%rax), %edx
.LBB8_59:                               # %for.end22.i.i
                                        #   in Loop: Header=BB8_50 Depth=2
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	%r9, %rdx
	jae	.LBB8_66
.LBB8_60:                               # %for.inc.i
                                        #   in Loop: Header=BB8_50 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB8_50
.LBB8_61:                               # %for.inc101.i
                                        #   in Loop: Header=BB8_26 Depth=1
	incq	%r10
	movq	%r10, %rax
	andq	%r11, %rax
	cmpq	$34, %rax
	jb	.LBB8_26
# BB#62:                                # %sweep_pages.exit
	movq	G+2608(%rip), %rax
	cmpq	$4194304, %rax          # imm = 0x400000
	movq	$4194304, %rcx          # imm = 0x400000
	jb	.LBB8_64
# BB#63:                                # %sweep_pages.exit
	movq	%rax, %rcx
.LBB8_64:                               # %sweep_pages.exit
	movq	%rcx, G+2616(%rip)
	movq	$1, %rdi
	callq	timevar_pop
	movl	quiet_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_67
.LBB8_65:                               # %if.end13
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_67:                               # %if.then10
	movq	stderr(%rip), %rdi
	movq	G+2608(%rip), %rdx
	movq	$10, %rcx
	shrq	%cl, %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB8_2:                                # %if.then3
	movq	G+2608(%rip), %rdx
	movq	stderr(%rip), %rdi
	movq	$10, %rcx
	shrq	%cl, %rdx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB8_3
.LBB8_66:                               # %if.then.i.i
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.ggc_recalculate_in_use_p, %rcx
	movq	$1194, %rsi             # imm = 0x4AA
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp70:
	.size	ggc_collect, .Ltmp70-ggc_collect
	.cfi_endproc

	.globl	ggc_print_statistics
	.align	16, 0x90
	.type	ggc_print_statistics,@function
ggc_print_statistics:                   # @ggc_print_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp79:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp80:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 48
	subq	$6192, %rsp             # imm = 0x1830
.Ltmp82:
	.cfi_def_cfa_offset 6240
.Ltmp83:
	.cfi_offset %rbx, -48
.Ltmp84:
	.cfi_offset %r12, -40
.Ltmp85:
	.cfi_offset %r13, -32
.Ltmp86:
	.cfi_offset %r14, -24
.Ltmp87:
	.cfi_offset %r15, -16
	leaq	24(%rsp), %rbx
	xorq	%rsi, %rsi
	movq	$6168, %rdx             # imm = 0x1818
	movq	%rbx, %rdi
	callq	memset
	movq	stderr(%rip), %rdi
	movq	$0, G+2616(%rip)
	movq	%rbx, %rsi
	callq	ggc_print_common_statistics
	movq	G+2640(%rip), %rax
	testq	%rax, %rax
	je	.LBB9_6
# BB#1:
	movabsq	$G+2640, %rbx
.LBB9_3:                                # %while.body.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_4 Depth 2
	movq	%rax, %rdi
	.align	16, 0x90
.LBB9_4:                                # %while.body.i
                                        #   Parent Loop BB9_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rdi), %rax
	movl	24(%rax), %ecx
	movq	(%rdi), %rax
	testq	%rcx, %rcx
	jne	.LBB9_2
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB9_4 Depth=2
	movq	%rax, (%rbx)
	callq	free
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB9_4
	jmp	.LBB9_6
	.align	16, 0x90
.LBB9_2:                                # %while.cond.outer.loopexit.i
                                        #   in Loop: Header=BB9_3 Depth=1
	testq	%rax, %rax
	movq	%rdi, %rbx
	jne	.LBB9_3
.LBB9_6:                                # %while.cond3.preheader.i
	movq	G+2648(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB9_12
# BB#7:
	movabsq	$G+2648, %rbx
	movq	%rcx, %rax
	jmp	.LBB9_8
.LBB9_10:                               # %while.body5.lr.ph.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movq	%rcx, %rax
	.align	16, 0x90
.LBB9_8:                                # %while.body5.i
                                        # =>This Inner Loop Header: Depth=1
	movl	24(%rax), %edx
	movq	(%rax), %rcx
	testq	%rdx, %rdx
	jne	.LBB9_9
# BB#11:                                # %if.then8.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movq	%rcx, (%rbx)
	movq	16(%rax), %rcx
	subq	%rcx, G+2624(%rip)
	movq	8(%rax), %rdi
	callq	free
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB9_8
	jmp	.LBB9_12
.LBB9_9:                                # %while.cond3.outer.loopexit.i
                                        #   in Loop: Header=BB9_8 Depth=1
	testq	%rcx, %rcx
	movq	%rax, %rbx
	jne	.LBB9_10
.LBB9_12:                               # %release_pages.exit
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str7, %rdx
	movabsq	$.L.str8, %rcx
	movabsq	$.L.str9, %r8
	movabsq	$.L.str10, %r9
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	stderr(%rip), %rdi
	callq	fprintf
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	.align	16, 0x90
.LBB9_13:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_15 Depth 2
	movq	G(,%r13,8), %rdi
	testq	%rdi, %rdi
	je	.LBB9_36
# BB#14:                                # %for.body6.lr.ph
                                        #   in Loop: Header=BB9_13 Depth=1
	movl	objects_per_page_table(,%r13,4), %r8d
	movq	object_size_table(,%r13,8), %rdx
	leaq	32(%r8), %r10
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %r10
	movq	$2, %rcx
	shrq	%cl, %r10
	xorq	%r15, %r15
	xorq	%r9, %r9
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB9_15:                               # %for.body6
                                        #   Parent Loop BB9_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rcx
	movq	%r9, %rax
	movq	8(%rdi), %rbx
	movzbl	42(%rdi), %esi
	movq	(%rdi), %rdi
	addq	%rcx, %rbx
	movq	%r8, %r9
	subq	%rsi, %r9
	andq	%r14, %r9
	imulq	%rdx, %r9
	addq	%rax, %r9
	leaq	48(%r15,%r10), %r15
	testq	%rdi, %rdi
	jne	.LBB9_15
# BB#16:                                # %for.end
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$32, %rsi
	cmpq	$10240, %rbx            # imm = 0x2800
	jae	.LBB9_18
# BB#17:                                #   in Loop: Header=BB9_13 Depth=1
	movq	$32, %r8
	jmp	.LBB9_23
	.align	16, 0x90
.LBB9_18:                               # %cond.false
                                        #   in Loop: Header=BB9_13 Depth=1
	cmpq	$10485760, %rbx         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB9_20
# BB#19:                                # %cond.false
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$20, %rcx
.LBB9_20:                               # %cond.false
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$107, %r8
	jb	.LBB9_22
# BB#21:                                # %cond.false
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$77, %r8
.LBB9_22:                               # %cond.false
                                        #   in Loop: Header=BB9_13 Depth=1
	shrq	%cl, %rbx
.LBB9_23:                               # %cond.end42
                                        #   in Loop: Header=BB9_13 Depth=1
	cmpq	$10240, %r9             # imm = 0x2800
	jb	.LBB9_29
# BB#24:                                # %cond.false47
                                        #   in Loop: Header=BB9_13 Depth=1
	cmpq	$10485760, %r9          # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB9_26
# BB#25:                                # %cond.false47
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$20, %rcx
.LBB9_26:                               # %cond.false47
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$107, %rsi
	jb	.LBB9_28
# BB#27:                                # %cond.false47
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$77, %rsi
.LBB9_28:                               # %cond.false47
                                        #   in Loop: Header=BB9_13 Depth=1
	shrq	%cl, %r9
.LBB9_29:                               # %cond.end65
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$32, %rax
	cmpq	$10240, %r15            # imm = 0x2800
	movq	%r15, %rdi
	jb	.LBB9_35
# BB#30:                                # %cond.false70
                                        #   in Loop: Header=BB9_13 Depth=1
	cmpq	$10485760, %r15         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB9_32
# BB#31:                                # %cond.false70
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$20, %rcx
.LBB9_32:                               # %cond.false70
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$107, %rax
	jb	.LBB9_34
# BB#33:                                # %cond.false70
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	$77, %rax
.LBB9_34:                               # %cond.false70
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	%r15, %rdi
	shrq	%cl, %rdi
.LBB9_35:                               # %cond.end88
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	%rax, 16(%rsp)
	movq	%rdi, 8(%rsp)
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movq	stderr(%rip), %rdi
	movabsq	$.L.str11, %rsi
	movq	%rbx, %rcx
	callq	fprintf
	addq	%r15, %r12
.LBB9_36:                               # %for.inc92
                                        #   in Loop: Header=BB9_13 Depth=1
	incq	%r13
	cmpq	$34, %r13
	jne	.LBB9_13
# BB#37:                                # %for.end93
	movq	G+2624(%rip), %rbx
	cmpq	$10240, %rbx            # imm = 0x2800
	movq	$32, %rdi
	jae	.LBB9_39
# BB#38:
	movq	$32, %r8
	jmp	.LBB9_44
.LBB9_39:                               # %cond.false111
	cmpq	$10485760, %rbx         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB9_41
# BB#40:                                # %cond.false111
	movq	$20, %rcx
.LBB9_41:                               # %cond.false111
	movq	$107, %r8
	jb	.LBB9_43
# BB#42:                                # %cond.false111
	movq	$77, %r8
.LBB9_43:                               # %cond.false111
	shrq	%cl, %rbx
.LBB9_44:                               # %cond.end115
	movq	G+2608(%rip), %r9
	cmpq	$10240, %r9             # imm = 0x2800
	jb	.LBB9_50
# BB#45:                                # %cond.false134
	cmpq	$10485760, %r9          # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB9_47
# BB#46:                                # %cond.false134
	movq	$20, %rcx
.LBB9_47:                               # %cond.false134
	movq	$107, %rdi
	jb	.LBB9_49
# BB#48:                                # %cond.false134
	movq	$77, %rdi
.LBB9_49:                               # %cond.false134
	shrq	%cl, %r9
.LBB9_50:                               # %cond.end138
	movq	$32, %rax
	cmpq	$10240, %r12            # imm = 0x2800
	jb	.LBB9_56
# BB#51:                                # %cond.false143
	cmpq	$10485760, %r12         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB9_53
# BB#52:                                # %cond.false143
	movq	$20, %rcx
.LBB9_53:                               # %cond.false143
	movq	$107, %rax
	jb	.LBB9_55
# BB#54:                                # %cond.false143
	movq	$77, %rax
.LBB9_55:                               # %cond.false143
	shrq	%cl, %r12
.LBB9_56:                               # %cond.end161
	movq	%rax, 16(%rsp)
	movq	%r12, 8(%rsp)
	movabsq	$.L.str12, %rsi
	movabsq	$.L.str13, %rdx
	xorq	%rax, %rax
	movq	%rdi, (%rsp)
	movq	stderr(%rip), %rdi
	movq	%rbx, %rcx
	callq	fprintf
	addq	$6192, %rsp             # imm = 0x1830
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp88:
	.size	ggc_print_statistics, .Ltmp88-ggc_print_statistics
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Head=%p, Tail=%p:\n"
	.size	.L.str, 19

	.type	G,@object               # @G
	.local	G
	.comm	G,2664,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"%p(%1d|%3d) -> "
	.size	.L.str1, 16

	.type	size_lookup,@object     # @size_lookup
	.data
	.align	16
size_lookup:
	.ascii	"\003\003\003\003\003\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
	.size	size_lookup, 257

	.type	object_size_table,@object # @object_size_table
	.local	object_size_table
	.comm	object_size_table,272,16
	.type	objects_per_page_table,@object # @objects_per_page_table
	.local	objects_per_page_table
	.comm	objects_per_page_table,136,16
	.type	.L.str3,@object         # @.str3
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str3:
	.asciz	"ggc-page.c"
	.size	.L.str3, 11

	.type	.L__FUNCTION__.ggc_push_context,@object # @__FUNCTION__.ggc_push_context
.L__FUNCTION__.ggc_push_context:
	.asciz	"ggc_push_context"
	.size	.L__FUNCTION__.ggc_push_context, 17

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	" {GC %luk -> "
	.size	.L.str4, 14

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%luk}"
	.size	.L.str5, 6

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\n%-5s %10s  %10s  %10s\n"
	.size	.L.str6, 24

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Size"
	.size	.L.str7, 5

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Allocated"
	.size	.L.str8, 10

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Used"
	.size	.L.str9, 5

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Overhead"
	.size	.L.str10, 9

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"%-5d %10ld%c %10ld%c %10ld%c\n"
	.size	.L.str11, 30

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"%-5s %10ld%c %10ld%c %10ld%c\n"
	.size	.L.str12, 30

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Total"
	.size	.L.str13, 6

	.type	.L__FUNCTION__.ggc_recalculate_in_use_p,@object # @__FUNCTION__.ggc_recalculate_in_use_p
.L__FUNCTION__.ggc_recalculate_in_use_p:
	.asciz	"ggc_recalculate_in_use_p"
	.size	.L__FUNCTION__.ggc_recalculate_in_use_p, 25

	.type	.L__FUNCTION__.alloc_page,@object # @__FUNCTION__.alloc_page
.L__FUNCTION__.alloc_page:
	.asciz	"alloc_page"
	.size	.L__FUNCTION__.alloc_page, 11

	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	"NULL"
	.size	.Lstr, 5


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
