	.file	"Base64.c.bc"
	.text
	.globl	XS_MIME__Base64_encode_base64
	.align	16, 0x90
	.type	XS_MIME__Base64_encode_base64,@function
XS_MIME__Base64_encode_base64:          # @XS_MIME__Base64_encode_base64
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
	subq	$96, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 144
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r15
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rdx, %r15
	cmpq	$7, %r15
	jg	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB0_2:                                # %if.end
	movq	8(%rax,%rbx,8), %r14
	leaq	1(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_utf8_downgrade
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB0_4
# BB#3:                                 # %cond.true
	movq	(%r14), %rax
	movq	8(%rax), %r14
	movq	%r14, 80(%rsp)
	movq	(%rax), %r13
	jmp	.LBB0_5
.LBB0_4:                                # %cond.false
	leaq	80(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	80(%rsp), %r14
	movq	%rax, %r13
.LBB0_5:                                # %cond.end
	cmpq	$9, %r15
	jl	.LBB0_10
# BB#6:                                 # %land.lhs.true
	movq	PL_stack_base(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	16(%rax,%rcx,8), %rdi
	movq	16(%rdi), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB0_10
# BB#7:                                 # %if.then14
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB0_9
# BB#8:                                 # %cond.true20
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 88(%rsp)
	movq	(%rax), %rsi
	jmp	.LBB0_11
.LBB0_10:                               # %if.else
	movq	$1, 88(%rsp)
	movabsq	$.L.str1, %rsi
.LBB0_11:                               # %if.end35
	leaq	2(%r14), %rax
	movabsq	$6148914691236517206, %rcx # imm = 0x5555555555555556
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	addq	%rdx, %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	movq	%rdi, 80(%rsp)
	leaq	4(%r14), %rax
	movq	%rsi, %rbx
	cmpq	$5, %rax
	jb	.LBB0_13
# BB#12:                                # %if.then38
	leaq	-1(%rdi), %rax
	movabsq	$-2912643801112034465, %rcx # imm = 0xD79435E50D79435F
	mulq	%rcx
	movq	88(%rsp), %rax
	movq	$6, %rcx
	shrq	%cl, %rdx
	incq	%rdx
	imulq	%rdx, %rax
	addq	%rax, %rdi
	movq	%rdi, 80(%rsp)
.LBB0_13:                               # %if.end43
	testq	%rdi, %rdi
	jne	.LBB0_15
# BB#14:                                # %if.end43
	movq	$1, %rdi
.LBB0_15:                               # %if.end43
	callq	Perl_newSV
	movq	%rax, 40(%rsp)          # 8-byte Spill
	orq	$67371008, 16(%rax)     # imm = 0x4040000
	movq	80(%rsp), %rdx
	movq	(%rax), %rcx
	movq	%rdx, 8(%rcx)
	movq	(%rcx), %r9
	movq	%rbx, %r10
	testq	%r14, %r14
	jle	.LBB0_44
# BB#16:                                # %for.body.lr.ph
	movq	%r10, 16(%rsp)          # 8-byte Spill
	leaq	1(%r10), %r11
	movq	%r11, 64(%rsp)          # 8-byte Spill
	movq	%r10, %rax
	notq	%rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r10, %rax
	negq	%rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$15677, %r12            # imm = 0x3D3D
	.align	16, 0x90
.LBB0_17:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_30 Depth 2
                                        #     Child Loop BB0_33 Depth 2
	movq	%rdi, %rax
	andq	%r15, %rax
	cmpq	$19, %rax
	jne	.LBB0_35
# BB#18:                                # %if.then57
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	88(%rsp), %r8
	xorq	%rdi, %rdi
	testq	%r8, %r8
	jle	.LBB0_35
# BB#19:                                # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	addq	%r10, %r8
	cmpq	%r11, %r8
	movq	%r8, %rax
	ja	.LBB0_21
# BB#20:                                # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	%r11, %rax
.LBB0_21:                               # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
	cmpq	%r11, %r8
	movq	%r8, %rcx
	ja	.LBB0_23
# BB#22:                                # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	%r11, %rcx
.LBB0_23:                               # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	%rcx, %r12
	subq	%r10, %r12
	cmpq	%r11, %r8
	movq	%r8, %rax
	ja	.LBB0_25
# BB#24:                                # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	%r11, %rax
.LBB0_25:                               # %while.body.lr.ph
                                        #   in Loop: Header=BB0_17 Depth=1
	xorq	%r15, %r15
	movq	%r12, %rsi
	andq	$-4, %rsi
	jne	.LBB0_27
# BB#26:                                #   in Loop: Header=BB0_17 Depth=1
	movq	%r10, %rax
	movq	%r9, %rdx
	jmp	.LBB0_32
.LBB0_27:                               # %vector.memcheck
                                        #   in Loop: Header=BB0_17 Depth=1
	leaq	-1(%rax), %rdx
	cmpq	%rdx, %r9
	ja	.LBB0_29
# BB#28:                                # %vector.memcheck
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	32(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rax
	addq	%r9, %rax
	cmpq	%rax, %r10
	movq	%r10, %rax
	movq	%r9, %rdx
	jbe	.LBB0_32
.LBB0_29:                               # %vector.body.preheader
                                        #   in Loop: Header=BB0_17 Depth=1
	leaq	(%r10,%rsi), %rax
	leaq	(%r9,%rsi), %rdx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rsi           # 8-byte Reload
	addq	%rsi, %rcx
	andq	$-4, %rcx
	movq	%r10, %rsi
	movq	%r9, %rdi
	.align	16, 0x90
.LBB0_30:                               # %vector.body
                                        #   Parent Loop BB0_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rsi), %r15d
	movzbl	2(%rsi), %ebx
	movzbl	1(%rsi), %r11d
	movzbl	3(%rsi), %r10d
	movb	%r10b, 3(%rdi)
	movb	%bl, 2(%rdi)
	movb	%r11b, 1(%rdi)
	movb	%r15b, (%rdi)
	addq	$4, %rdi
	addq	$4, %rsi
	addq	$-4, %rcx
	jne	.LBB0_30
# BB#31:                                #   in Loop: Header=BB0_17 Depth=1
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	16(%rsp), %r10          # 8-byte Reload
.LBB0_32:                               # %middle.block
                                        #   in Loop: Header=BB0_17 Depth=1
	cmpq	%r15, %r12
	movq	64(%rsp), %r11          # 8-byte Reload
	je	.LBB0_34
	.align	16, 0x90
.LBB0_33:                               # %while.body
                                        #   Parent Loop BB0_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	leaq	1(%rax), %rax
	cmpq	%r8, %rax
	movb	%cl, (%rdx)
	leaq	1(%rdx), %rdx
	jb	.LBB0_33
.LBB0_34:                               # %while.cond.if.end64.loopexit_crit_edge
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	subq	%r10, %rax
	addq	%rax, %r9
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$15677, %r12            # imm = 0x3D3D
.LBB0_35:                               # %if.end64
                                        #   in Loop: Header=BB0_17 Depth=1
	movzbl	(%r13), %edx
	cmpq	$2, %r14
	jl	.LBB0_36
# BB#37:                                # %cond.true67
                                        #   in Loop: Header=BB0_17 Depth=1
	movzbl	1(%r13), %ebx
	addq	$2, %r13
	jmp	.LBB0_38
	.align	16, 0x90
.LBB0_36:                               #   in Loop: Header=BB0_17 Depth=1
	xorq	%rbx, %rbx
	incq	%r13
.LBB0_38:                               # %cond.end70
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	$2, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	$4, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	basis_64(%rsi), %ecx
	andq	$48, %rdx
	orq	%rdx, %rax
	movzbl	basis_64(%rax), %eax
	movb	%cl, (%r9)
	movb	%al, 1(%r9)
	cmpq	$3, %r14
	jl	.LBB0_40
# BB#39:                                # %if.then87
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	$2, %rcx
	shlq	%cl, %rbx
	movzbl	(%r13), %eax
	andq	$60, %rbx
	movq	$6, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	orq	%rbx, %rdx
	movzbl	basis_64(%rdx), %ecx
	andq	$63, %rax
	movzbl	basis_64(%rax), %eax
	incq	%r13
	movb	%cl, 2(%r9)
	movb	%al, 3(%r9)
	jmp	.LBB0_43
	.align	16, 0x90
.LBB0_40:                               # %if.else104
                                        #   in Loop: Header=BB0_17 Depth=1
	cmpq	$2, %r14
	je	.LBB0_41
# BB#42:                                # %if.else115
                                        #   in Loop: Header=BB0_17 Depth=1
	movw	%r12w, 2(%r9)
.LBB0_43:                               # %for.inc
                                        #   in Loop: Header=BB0_17 Depth=1
	addq	$4, %r9
	addq	$-3, %r14
	incq	%rdi
	testq	%r14, %r14
	jg	.LBB0_17
	jmp	.LBB0_44
.LBB0_41:                               # %for.inc.thread
	andq	$15, %rbx
	movzbl	basis_64(,%rbx,4), %eax
	movb	%al, 2(%r9)
	movq	$61, %rax
	movb	%al, 3(%r9)
	addq	$4, %r9
.LBB0_44:                               # %for.end
	cmpq	$0, 80(%rsp)
	je	.LBB0_60
# BB#45:                                # %if.then122
	movq	88(%rsp), %r14
	testq	%r14, %r14
	jle	.LBB0_60
# BB#46:                                # %while.body131.lr.ph
	addq	%r10, %r14
	leaq	1(%r10), %rdx
	cmpq	%rdx, %r14
	movq	%r14, %rdi
	ja	.LBB0_48
# BB#47:                                # %while.body131.lr.ph
	movq	%rdx, %rdi
.LBB0_48:                               # %while.body131.lr.ph
	cmpq	%rdx, %r14
	movq	%r14, %rax
	ja	.LBB0_50
# BB#49:                                # %while.body131.lr.ph
	movq	%rdx, %rax
.LBB0_50:                               # %while.body131.lr.ph
	movq	%rax, %r11
	subq	%r10, %r11
	xorq	%rsi, %rsi
	movq	%r10, %rcx
	movq	%r11, %r10
	andq	$-4, %r10
	je	.LBB0_51
# BB#52:                                # %vector.memcheck288
	movq	%rcx, %r8
	leaq	-1(%rax), %rdx
	cmpq	%rdx, %r9
	ja	.LBB0_54
# BB#53:                                # %vector.memcheck288
	movq	%r8, %rdx
	notq	%rdx
	addq	%rdx, %rax
	addq	%r9, %rax
	cmpq	%rax, %r8
	movq	%r8, %rax
	movq	%r9, %rdx
	jbe	.LBB0_57
.LBB0_54:                               # %vector.body267.preheader
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	leaq	(%r8,%r10), %rax
	leaq	(%r9,%r10), %rdx
	movq	%r11, %rsi
	andq	$-4, %rsi
	movq	%r8, %rbx
	movq	%r9, %rdi
	.align	16, 0x90
.LBB0_55:                               # %vector.body267
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %r15d
	movzbl	2(%rbx), %r13d
	movzbl	1(%rbx), %r12d
	movzbl	3(%rbx), %ecx
	movb	%cl, 3(%rdi)
	movb	%r13b, 2(%rdi)
	movb	%r12b, 1(%rdi)
	movb	%r15b, (%rdi)
	addq	$4, %rdi
	addq	$4, %rbx
	addq	$-4, %rsi
	jne	.LBB0_55
# BB#56:
	movq	%r10, %rsi
	movq	72(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB0_57
.LBB0_51:
	movq	%rcx, %rax
	movq	%rcx, %r8
	movq	%r9, %rdx
.LBB0_57:                               # %middle.block268
	cmpq	%rsi, %r11
	je	.LBB0_59
	.align	16, 0x90
.LBB0_58:                               # %while.body131
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	leaq	1(%rax), %rax
	cmpq	%r14, %rax
	movb	%cl, (%rdx)
	leaq	1(%rdx), %rdx
	jb	.LBB0_58
.LBB0_59:                               # %while.cond128.if.end135.loopexit_crit_edge
	subq	%r8, %rdi
	addq	%rdi, %r9
.LBB0_60:                               # %if.end135
	xorq	%rax, %rax
	movb	%al, (%r9)
	movq	PL_stack_base(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, 8(%rax,%rcx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rcx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_9:                                # %cond.false29
	leaq	88(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rsi
	jmp	.LBB0_11
.Ltmp17:
	.size	XS_MIME__Base64_encode_base64, .Ltmp17-XS_MIME__Base64_encode_base64
	.cfi_endproc

	.globl	XS_MIME__Base64_decode_base64
	.align	16, 0x90
	.type	XS_MIME__Base64_decode_base64,@function
XS_MIME__Base64_decode_base64:          # @XS_MIME__Base64_decode_base64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 96
.Ltmp30:
	.cfi_offset %rbx, -48
.Ltmp31:
	.cfi_offset %r12, -40
.Ltmp32:
	.cfi_offset %r13, -32
.Ltmp33:
	.cfi_offset %r14, -24
.Ltmp34:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB1_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	movq	16(%rdi), %rax
	leaq	1(%rbx), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	andq	$537133056, %rax        # imm = 0x20040000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB1_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %r15
	movq	%r15, 40(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB1_5
.LBB1_4:                                # %cond.false
	leaq	40(%rsp), %rsi
	callq	Perl_sv_2pvbyte
	movq	40(%rsp), %r15
	movq	%rax, %rbx
.LBB1_5:                                # %cond.end
	leaq	(%r15,%r15,2), %rdi
	movq	$2, %rcx
	shrq	%cl, %rdi
	testq	%rdi, %rdi
	jne	.LBB1_7
# BB#6:                                 # %cond.end
	movq	$1, %rdi
.LBB1_7:                                # %cond.end
	callq	Perl_newSV
	movq	%rax, 8(%rsp)           # 8-byte Spill
	orq	$67371008, 16(%rax)     # imm = 0x4040000
	movq	(%rax), %rax
	movq	(%rax), %r14
	testq	%r15, %r15
	jle	.LBB1_31
# BB#8:                                 # %do.body.preheader.lr.ph
	leaq	(%rbx,%r15), %r13
	leaq	-1(%rbx,%r15), %r15
	.align	16, 0x90
.LBB1_9:                                # %do.body.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_10 Depth 2
	xorq	%r12, %r12
	.align	16, 0x90
.LBB1_10:                               # %do.body
                                        #   Parent Loop BB1_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	movzbl	index_64(%rax), %eax
	cmpq	$255, %rax
	je	.LBB1_12
# BB#11:                                # %if.then22
                                        #   in Loop: Header=BB1_10 Depth=2
	movq	$32, %rcx
	leaq	1(%r12), %rdx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movb	%al, 36(%rsp,%r12)
	movq	%rdx, %r12
.LBB1_12:                               # %if.end25
                                        #   in Loop: Header=BB1_10 Depth=2
	cmpq	%rbx, %r15
	je	.LBB1_13
# BB#21:                                # %do.cond
                                        #   in Loop: Header=BB1_10 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB1_10
	jmp	.LBB1_22
	.align	16, 0x90
.LBB1_13:                               # %if.then28
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	movq	%r13, %rbx
	jg	.LBB1_22
# BB#14:                                # %if.then31
                                        #   in Loop: Header=BB1_9 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB1_31
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_17
# BB#16:                                # %if.then36
                                        #   in Loop: Header=BB1_9 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str3, %rdi
	callq	Perl_warn
.LBB1_17:                               # %if.end37
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB1_31
# BB#18:                                # %if.end41
                                        #   in Loop: Header=BB1_9 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$2, %r12
	jne	.LBB1_20
# BB#19:                                # %if.then44
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	$254, %rax
	movb	%al, 38(%rsp)
.LBB1_20:                               # %if.end46
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	$254, %rax
	movb	%al, 39(%rsp)
	movq	%r13, %rbx
.LBB1_22:                               # %do.end
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	36(%rsp), %eax
	cmpq	$254, %rax
	je	.LBB1_24
# BB#23:                                # %lor.lhs.false
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	37(%rsp), %edx
	cmpq	$254, %rdx
	je	.LBB1_24
# BB#26:                                # %if.end66
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	$2, %rcx
	shlq	%cl, %rax
	movq	$4, %rcx
	shrq	%cl, %rdx
	andq	$3, %rdx
	orq	%rax, %rdx
	movb	%dl, (%r14)
	movzbl	38(%rsp), %eax
	cmpq	$254, %rax
	je	.LBB1_27
# BB#28:                                # %if.end80
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	37(%rsp), %edx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	$2, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	orq	%rdx, %rax
	movb	%al, 1(%r14)
	movzbl	39(%rsp), %eax
	cmpq	$254, %rax
	je	.LBB1_29
# BB#30:                                # %if.end97
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	38(%rsp), %edx
	movq	$6, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movb	%dl, 2(%r14)
	addq	$3, %r14
	cmpq	%r13, %rbx
	jb	.LBB1_9
	jmp	.LBB1_31
.LBB1_24:                               # %if.then60
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_31
# BB#25:                                # %if.then64
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
	jmp	.LBB1_31
.LBB1_27:
	incq	%r14
	jmp	.LBB1_31
.LBB1_29:
	addq	$2, %r14
.LBB1_31:                               # %thats_it
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	(%rsi), %rax
	movq	(%rax), %rcx
	movq	%r14, %rdx
	subq	%rcx, %rdx
	movq	%rdx, 8(%rax)
	xorq	%rax, %rax
	movb	%al, (%r14)
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rsi, 8(%rax,%rcx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rcx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	XS_MIME__Base64_decode_base64, .Ltmp35-XS_MIME__Base64_decode_base64
	.cfi_endproc

	.globl	XS_MIME__QuotedPrint_encode_qp
	.align	16, 0x90
	.type	XS_MIME__QuotedPrint_encode_qp,@function
XS_MIME__QuotedPrint_encode_qp:         # @XS_MIME__QuotedPrint_encode_qp
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
	subq	$80, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 128
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	subq	%rdx, %rbx
	cmpq	$7, %rbx
	jg	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB2_2:                                # %if.end
	movq	8(%rax,%r15,8), %r14
	leaq	1(%r15), %r12
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_utf8_downgrade
	cmpq	$9, %rbx
	jl	.LBB2_7
# BB#3:                                 # %land.lhs.true
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%r15,8), %rdi
	movq	16(%rdi), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB2_7
# BB#4:                                 # %if.then9
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB2_6
# BB#5:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movq	(%rax), %rax
	jmp	.LBB2_8
.LBB2_7:                                # %if.else
	movq	$1, 72(%rsp)
	movabsq	$.L.str1, %rax
.LBB2_8:                                # %if.end23
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	cmpq	$17, %rbx
	jl	.LBB2_30
# BB#9:                                 # %land.rhs
	movq	PL_stack_base(%rip), %rax
	movq	24(%rax,%r15,8), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB2_26
# BB#10:                                # %cond.false29
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB2_17
# BB#11:                                # %cond.true35
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB2_26
# BB#12:                                # %land.rhs40
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB2_26
# BB#13:                                # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB2_26
# BB#14:                                # %land.rhs45
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB2_15
# BB#16:                                # %land.rhs45
	movabsq	$1, %rax
	jmp	.LBB2_26
.LBB2_6:                                # %cond.false
	leaq	72(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	jmp	.LBB2_8
.LBB2_17:                               # %cond.false51
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB2_21
# BB#18:                                # %cond.true57
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB2_19
# BB#20:                                # %cond.true57
	movabsq	$1, %rax
	jmp	.LBB2_26
.LBB2_21:                               # %cond.false63
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB2_25
# BB#22:                                # %cond.true69
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB2_23
# BB#24:                                # %cond.true69
	movabsq	$1, %rax
	jmp	.LBB2_26
.LBB2_19:
	movabsq	$0, %rax
	jmp	.LBB2_26
.LBB2_15:
	movabsq	$0, %rax
	jmp	.LBB2_26
.LBB2_25:                               # %cond.false75
	callq	Perl_sv_2bool
	jmp	.LBB2_26
.LBB2_23:
	movabsq	$0, %rax
.LBB2_26:                               # %cond.end86
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_27
# BB#28:                                # %cond.end86
	movabsq	$1, %rax
	jmp	.LBB2_29
.LBB2_27:
	movabsq	$0, %rax
.LBB2_29:                               # %cond.end86
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB2_30:                               # %land.end89
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB2_32
# BB#31:                                # %cond.true94
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	(%r14), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 64(%rsp)
	movq	(%rax), %r14
	jmp	.LBB2_33
.LBB2_32:                               # %cond.false99
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r15, 16(%rsp)          # 8-byte Spill
	leaq	64(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	64(%rsp), %rdi
	movq	%rax, %r14
.LBB2_33:                               # %cond.end101
	movq	%r14, 24(%rsp)          # 8-byte Spill
	leaq	(%r14,%rdi), %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	incq	%rdi
	callq	Perl_newSV
	movq	%rax, %r15
	movabsq	$.L.str6, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_setpv
	movq	%rbx, %rdi
	xorq	%r12, %r12
	movq	%r14, %rbx
                                        # kill: R14<def> RBX<kill>
	jmp	.LBB2_34
.LBB2_76:                               # %cond.true250
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	%rdi, %rbx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_grow
	movq	%rbx, %rdi
	movq	%r13, %r14
	.align	16, 0x90
.LBB2_34:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_35 Depth 2
                                        #     Child Loop BB2_65 Depth 2
                                        #     Child Loop BB2_51 Depth 2
	cmpq	%rdi, %r14
	movq	%r14, %r13
	jae	.LBB2_40
.LBB2_35:                               # %land.rhs109
                                        #   Parent Loop BB2_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	cmpq	$9, %rax
	je	.LBB2_39
# BB#36:                                # %lor.rhs113
                                        #   in Loop: Header=BB2_35 Depth=2
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$32, %rdx
	jl	.LBB2_40
# BB#37:                                # %switch.early.test
                                        #   in Loop: Header=BB2_35 Depth=2
	cmpq	$61, %rax
	je	.LBB2_40
# BB#38:                                # %switch.early.test
                                        #   in Loop: Header=BB2_35 Depth=2
	cmpq	$127, %rax
	je	.LBB2_40
.LBB2_39:                               # %while.body130
                                        #   in Loop: Header=BB2_35 Depth=2
	incq	%r13
	cmpq	%rdi, %r13
	jb	.LBB2_35
	.align	16, 0x90
.LBB2_40:                               # %while.end
                                        #   in Loop: Header=BB2_34 Depth=1
	cmpq	%rdi, %r13
	je	.LBB2_64
# BB#41:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_34 Depth=1
	movzbl	(%r13), %eax
	cmpq	$10, %rax
	jne	.LBB2_42
.LBB2_64:                               # %while.cond138.preheader
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	%r15, %rbx
	jmp	.LBB2_65
	.align	16, 0x90
.LBB2_68:                               # %while.cond138.backedge
                                        #   in Loop: Header=BB2_65 Depth=2
	decq	%r13
.LBB2_65:                               # %while.cond138.preheader
                                        #   Parent Loop BB2_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %r13
	jbe	.LBB2_43
# BB#66:                                # %land.rhs141
                                        #   in Loop: Header=BB2_65 Depth=2
	movzbl	-1(%r13), %eax
	cmpq	$9, %rax
	je	.LBB2_68
# BB#67:                                # %land.rhs141
                                        #   in Loop: Header=BB2_65 Depth=2
	cmpq	$32, %rax
	je	.LBB2_68
	jmp	.LBB2_43
.LBB2_42:                               #   in Loop: Header=BB2_34 Depth=1
	movq	%r15, %rbx
	.align	16, 0x90
.LBB2_43:                               # %if.end157
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	%r13, %r15
	subq	%r14, %r15
	je	.LBB2_57
# BB#44:                                # %if.then162
                                        #   in Loop: Header=BB2_34 Depth=1
	cmpq	$0, 72(%rsp)
	je	.LBB2_54
# BB#45:                                # %if.then164
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	$76, 48(%rsp)           # 8-byte Folded Spill
	cmpq	%rdi, %r13
	je	.LBB2_50
# BB#46:                                # %lor.lhs.false167
                                        #   in Loop: Header=BB2_34 Depth=1
	movzbl	(%r13), %eax
	cmpq	$10, %rax
	je	.LBB2_50
# BB#47:                                # %cond.false172
                                        #   in Loop: Header=BB2_34 Depth=1
	leaq	1(%r13), %rax
	cmpq	%rdi, %rax
	movq	$73, 48(%rsp)           # 8-byte Folded Spill
	je	.LBB2_50
# BB#48:                                # %lor.rhs176
                                        #   in Loop: Header=BB2_34 Depth=1
	movzbl	(%rax), %eax
	cmpq	$10, %rax
	movq	$73, 48(%rsp)           # 8-byte Folded Spill
	je	.LBB2_50
# BB#49:                                # %select.mid
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	$72, 48(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_50:                               # %cond.end183
                                        #   in Loop: Header=BB2_34 Depth=1
	leaq	(%r12,%r15), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB2_54
	.align	16, 0x90
.LBB2_51:                               # %while.body190
                                        #   Parent Loop BB2_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$75, %rax
	subq	%r12, %rax
	cmpq	%r15, %rax
	movq	%r15, %r12
	ja	.LBB2_53
# BB#52:                                # %while.body190
                                        #   in Loop: Header=BB2_51 Depth=2
	movq	%rax, %r12
.LBB2_53:                               # %while.body190
                                        #   in Loop: Header=BB2_51 Depth=2
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	Perl_sv_catpvn_flags
	addq	%r12, %r14
	subq	%r12, %r15
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str7, %rsi
	callq	Perl_sv_catpvn_flags
	movq	72(%rsp), %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	Perl_sv_catpvn_flags
	xorq	%r12, %r12
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r15
	ja	.LBB2_51
.LBB2_54:                               # %if.end198
                                        #   in Loop: Header=BB2_34 Depth=1
	testq	%r15, %r15
	je	.LBB2_56
# BB#55:                                # %if.then200
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	Perl_sv_catpvn_flags
	addq	%r15, %r12
.LBB2_56:                               # %if.end203
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
.LBB2_57:                               # %if.end203
                                        #   in Loop: Header=BB2_34 Depth=1
	cmpq	%rdi, %r13
	je	.LBB2_77
# BB#58:                                # %if.else207
                                        #   in Loop: Header=BB2_34 Depth=1
	movzbl	(%r13), %edx
	cmpq	$10, %rdx
	movabsq	$0, %rcx
	je	.LBB2_60
# BB#59:                                # %if.else207
                                        #   in Loop: Header=BB2_34 Depth=1
	movabsq	$1, %rcx
.LBB2_60:                               # %if.else207
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	72(%rsp), %rax
	testq	%rax, %rax
	movabsq	$0, %rsi
	jne	.LBB2_62
# BB#61:                                # %if.else207
                                        #   in Loop: Header=BB2_34 Depth=1
	movabsq	$1, %rsi
.LBB2_62:                               # %if.else207
                                        #   in Loop: Header=BB2_34 Depth=1
	orq	%rsi, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	orq	%rsi, %rcx
	movq	%rbx, %r15
	je	.LBB2_63
# BB#69:                                # %if.else217
                                        #   in Loop: Header=BB2_34 Depth=1
	testq	%rax, %rax
	je	.LBB2_72
# BB#70:                                # %if.else217
                                        #   in Loop: Header=BB2_34 Depth=1
	cmpq	$73, %r12
	jb	.LBB2_72
# BB#71:                                # %if.then222
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movabsq	$.L.str7, %rsi
	callq	Perl_sv_catpvn_flags
	movq	72(%rsp), %rdx
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	Perl_sv_catpvn_flags
	movzbl	(%r13), %edx
	xorq	%r12, %r12
.LBB2_72:                               # %if.end223
                                        #   in Loop: Header=BB2_34 Depth=1
	andq	$255, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str8, %rsi
	callq	Perl_sv_catpvf
	addq	$3, %r12
	jmp	.LBB2_73
	.align	16, 0x90
.LBB2_63:                               # %if.then215
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	Perl_sv_catpvn_flags
	xorq	%r12, %r12
.LBB2_73:                               # %if.end228
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	(%r15), %rax
	movq	16(%rax), %rcx
	incq	%r13
	cmpq	$81, %rcx
	movq	%r13, %r14
	movq	40(%rsp), %rdi          # 8-byte Reload
	jb	.LBB2_34
# BB#74:                                # %land.lhs.true232
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	8(%rax), %rdx
	movq	%rcx, %rax
	subq	%rdx, %rax
	cmpq	$2, %rax
	movq	%r13, %r14
	ja	.LBB2_34
# BB#75:                                # %if.then240
                                        #   in Loop: Header=BB2_34 Depth=1
	movq	64(%rsp), %rax
	imulq	%rdx, %rax
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	subq	%rdx, %rsi
	xorq	%rdx, %rdx
	divq	%rsi
	cmpq	%rax, %rcx
	movq	%r13, %r14
	jae	.LBB2_34
	jmp	.LBB2_76
.LBB2_77:                               # %while.end258
	movq	(%rbx), %rax
	cmpq	$0, 8(%rax)
	je	.LBB2_81
# BB#78:                                # %while.end258
	movq	72(%rsp), %rax
	testq	%rax, %rax
	je	.LBB2_81
# BB#79:                                # %while.end258
	testq	%r12, %r12
	je	.LBB2_81
# BB#80:                                # %if.then266
	movabsq	$.L.str7, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	callq	Perl_sv_catpvn_flags
	movq	72(%rsp), %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	Perl_sv_catpvn_flags
.LBB2_81:                               # %if.end267
	movq	PL_stack_base(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, 8(%rax,%rcx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rcx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	XS_MIME__QuotedPrint_encode_qp, .Ltmp53-XS_MIME__QuotedPrint_encode_qp
	.cfi_endproc

	.globl	XS_MIME__QuotedPrint_decode_qp
	.align	16, 0x90
	.type	XS_MIME__QuotedPrint_decode_qp,@function
XS_MIME__QuotedPrint_decode_qp:         # @XS_MIME__QuotedPrint_decode_qp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 80
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB3_2:                                # %if.end
	movq	8(%rax,%r12,8), %rdi
	movq	16(%rdi), %rax
	leaq	1(%r12), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	andq	$537133056, %rax        # imm = 0x20040000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB3_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %r13
	movq	%r13, 24(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB3_5
.LBB3_4:                                # %cond.false
	leaq	24(%rsp), %rsi
	callq	Perl_sv_2pvbyte
	movq	24(%rsp), %r13
	movq	%rax, %rbx
.LBB3_5:                                # %cond.end
	testq	%r13, %r13
	movq	%r13, %rdi
	jne	.LBB3_7
# BB#6:                                 # %cond.end
	movq	$1, %rdi
.LBB3_7:                                # %cond.end
	callq	Perl_newSV
	movq	%rax, (%rsp)            # 8-byte Spill
	orq	$67371008, 16(%rax)     # imm = 0x4040000
	movq	(%rax), %rax
	movq	(%rax), %r15
	testq	%r13, %r13
	jle	.LBB3_52
# BB#8:                                 # %while.body.lr.ph
	addq	%rbx, %r13
	xorq	%r14, %r14
	jmp	.LBB3_9
.LBB3_43:                               # %if.then155
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	%rcx
	xorq	%r14, %r14
	movq	%rcx, %rbx
	jmp	.LBB3_48
	.align	16, 0x90
.LBB3_9:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_23 Depth 2
                                        #     Child Loop BB3_34 Depth 2
	movsbq	(%rbx), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$31, %rax
	jg	.LBB3_16
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$9, %rcx
	je	.LBB3_17
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$10, %rcx
	jne	.LBB3_12
# BB#20:                                # %if.then45
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	%rbx
	movq	$10, %rax
	jmp	.LBB3_46
	.align	16, 0x90
.LBB3_16:                               # %while.body
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$32, %rcx
	jne	.LBB3_21
.LBB3_17:                               # %if.then23
                                        #   in Loop: Header=BB3_9 Depth=1
	testq	%r14, %r14
	movq	%rbx, %rax
	je	.LBB3_19
# BB#18:                                # %if.then23
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	%r14, %rax
.LBB3_19:                               # %if.then23
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	%rbx
	movq	%rax, %r14
	jmp	.LBB3_48
.LBB3_12:                               # %while.body
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	$13, %rcx
	jne	.LBB3_21
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_9 Depth=1
	leaq	1(%rbx), %rcx
	cmpq	%r13, %rcx
	movq	$13, %rax
	jae	.LBB3_21
# BB#14:                                # %land.lhs.true34
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB3_21
# BB#15:                                #   in Loop: Header=BB3_9 Depth=1
	movq	%rcx, %rbx
	jmp	.LBB3_48
	.align	16, 0x90
.LBB3_21:                               # %if.else48
                                        #   in Loop: Header=BB3_9 Depth=1
	testq	%r14, %r14
	je	.LBB3_25
# BB#22:                                # %if.else48
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	%rbx, %r14
	jae	.LBB3_25
	.align	16, 0x90
.LBB3_23:                               # %while.body54
                                        #   Parent Loop BB3_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %eax
	incq	%r14
	movb	%al, (%r15)
	incq	%r15
	cmpq	%r14, %rbx
	jne	.LBB3_23
# BB#24:                                # %while.cond51.if.end57.loopexit_crit_edge
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rbx), %eax
.LBB3_25:                               # %if.end57
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$61, %rcx
	jne	.LBB3_45
# BB#26:                                # %if.then61
                                        #   in Loop: Header=BB3_9 Depth=1
	leaq	2(%rbx), %rsi
	leaq	1(%rbx), %rax
	cmpq	%r13, %rsi
	jae	.LBB3_33
# BB#27:                                # %land.lhs.true65
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rax), %edx
	leaq	-48(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB3_30
# BB#28:                                # %switch.early.test
                                        #   in Loop: Header=BB3_9 Depth=1
	leaq	-65(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	ja	.LBB3_33
# BB#29:                                # %switch.early.test
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	movabsq	$270582939711, %rcx     # imm = 0x3F0000003F
	testq	%rcx, %rdi
	je	.LBB3_33
.LBB3_30:                               # %land.lhs.true95
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rsi), %ecx
	leaq	-48(%rcx), %rsi
	andq	$255, %rsi
	cmpq	$10, %rsi
	jb	.LBB3_53
# BB#31:                                # %switch.early.test297
                                        #   in Loop: Header=BB3_9 Depth=1
	addq	$-65, %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	ja	.LBB3_33
# BB#32:                                # %switch.early.test297
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$270582939711, %rcx     # imm = 0x3F0000003F
	testq	%rcx, %rsi
	je	.LBB3_33
.LBB3_53:                               # %if.then125
                                        #   in Loop: Header=BB3_9 Depth=1
	movb	%dl, 21(%rsp)
	movzbl	2(%rbx), %eax
	addq	$3, %rbx
	movb	%al, 22(%rsp)
	xorq	%r14, %r14
	movb	%r14b, 23(%rsp)
	xorq	%rsi, %rsi
	movq	$16, %rdx
	leaq	21(%rsp), %rdi
	callq	strtol
	movb	%al, (%r15)
	incq	%r15
	jmp	.LBB3_48
.LBB3_45:                               # %if.else178
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	%rbx
.LBB3_46:                               # %while.cond.backedge
                                        #   in Loop: Header=BB3_9 Depth=1
	movb	%al, (%r15)
	incq	%r15
.LBB3_47:                               # %while.cond.backedge
                                        #   in Loop: Header=BB3_9 Depth=1
	xorq	%r14, %r14
	jmp	.LBB3_48
.LBB3_33:                               # %while.cond137.preheader
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	%r13, %rax
	movq	%rax, %rcx
	jae	.LBB3_39
	.align	16, 0x90
.LBB3_34:                               # %land.rhs
                                        #   Parent Loop BB3_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rcx), %edx
	cmpq	$9, %rdx
	je	.LBB3_37
# BB#35:                                # %land.rhs
                                        #   in Loop: Header=BB3_34 Depth=2
	cmpq	$10, %rdx
	je	.LBB3_43
# BB#36:                                # %land.rhs
                                        #   in Loop: Header=BB3_34 Depth=2
	cmpq	$32, %rdx
	jne	.LBB3_38
.LBB3_37:                               # %while.cond137.backedge
                                        #   in Loop: Header=BB3_34 Depth=2
	incq	%rcx
	cmpq	%r13, %rcx
	jb	.LBB3_34
.LBB3_38:                               # %while.cond137.backedge.if.else157.loopexit_crit_edge
                                        #   in Loop: Header=BB3_9 Depth=1
	leaq	-1(%rcx), %rbx
.LBB3_39:                               # %if.else157
                                        #   in Loop: Header=BB3_9 Depth=1
	leaq	2(%rbx), %rdx
	cmpq	%r13, %rdx
	jae	.LBB3_44
# BB#40:                                # %land.lhs.true161
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rcx), %ecx
	cmpq	$13, %rcx
	jne	.LBB3_44
# BB#41:                                # %land.lhs.true165
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rdx), %ecx
	cmpq	$10, %rcx
	jne	.LBB3_44
# BB#42:                                # %if.then170
                                        #   in Loop: Header=BB3_9 Depth=1
	addq	$3, %rbx
	jmp	.LBB3_47
.LBB3_44:                               # %if.else172
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$61, %rcx
	movb	%cl, (%r15)
	incq	%r15
	xorq	%r14, %r14
	movq	%rax, %rbx
	.align	16, 0x90
.LBB3_48:                               # %while.cond.backedge
                                        #   in Loop: Header=BB3_9 Depth=1
	cmpq	%r13, %rbx
	jb	.LBB3_9
# BB#49:                                # %while.end185
	testq	%r14, %r14
	je	.LBB3_52
# BB#50:                                # %while.end185
	cmpq	%rbx, %r14
	jae	.LBB3_52
	.align	16, 0x90
.LBB3_51:                               # %while.body191
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	incq	%r14
	movb	%al, (%r15)
	incq	%r15
	cmpq	%r14, %rbx
	jne	.LBB3_51
.LBB3_52:                               # %if.end195
	xorq	%rax, %rax
	movb	%al, (%r15)
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	(%rdx), %rax
	movq	(%rax), %rcx
	subq	%rcx, %r15
	movq	%r15, 8(%rax)
	movq	PL_stack_base(%rip), %rax
	movq	%rdx, 8(%rax,%r12,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r12,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	XS_MIME__QuotedPrint_decode_qp, .Ltmp71-XS_MIME__QuotedPrint_decode_qp
	.cfi_endproc

	.globl	boot_MIME__Base64
	.align	16, 0x90
	.type	boot_MIME__Base64,@function
boot_MIME__Base64:                      # @boot_MIME__Base64
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp76:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp77:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp78:
	.cfi_def_cfa_offset 32
.Ltmp79:
	.cfi_offset %rbx, -32
.Ltmp80:
	.cfi_offset %r14, -24
.Ltmp81:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movabsq	$.L.str11, %rdi
	movabsq	$XS_MIME__Base64_encode_base64, %rsi
	movabsq	$.L.str10, %rbx
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str12, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str13, %rdi
	movabsq	$XS_MIME__Base64_decode_base64, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str14, %r14
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str15, %rdi
	movabsq	$XS_MIME__QuotedPrint_encode_qp, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str16, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str17, %rdi
	movabsq	$XS_MIME__QuotedPrint_decode_qp, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%r15,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp82:
	.size	boot_MIME__Base64, .Ltmp82-boot_MIME__Base64
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: MIME::Base64::encode_base64(sv, ...)"
	.size	.L.str, 44

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\n"
	.size	.L.str1, 2

	.type	basis_64,@object        # @basis_64
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
basis_64:
	.asciz	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
	.size	basis_64, 65

	.type	.L.str2,@object         # @.str2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str2:
	.asciz	"Usage: MIME::Base64::decode_base64(sv)"
	.size	.L.str2, 39

	.type	index_64,@object        # @index_64
	.section	.rodata,"a",@progbits
	.align	16
index_64:
	.ascii	"\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377>\377\377\377?456789:;<=\377\377\377\376\377\377\377\000\001\002\003\004\005\006\007\b\t\n\013\f\r\016\017\020\021\022\023\024\025\026\027\030\031\377\377\377\377\377\377\032\033\034\035\036\037 !\"#$%&'()*+,-./0123\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377\377"
	.size	index_64, 256

	.type	.L.str3,@object         # @.str3
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str3:
	.asciz	"Premature end of base64 data"
	.size	.L.str3, 29

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Premature padding of base64 data"
	.size	.L.str4, 33

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Usage: MIME::QuotedPrint::encode_qp(sv, ...)"
	.size	.L.str5, 45

	.type	.L.str6,@object         # @.str6
.L.str6:
	.zero	1
	.size	.L.str6, 1

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"="
	.size	.L.str7, 2

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"=%02X"
	.size	.L.str8, 6

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Usage: MIME::QuotedPrint::decode_qp(sv)"
	.size	.L.str9, 40

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Base64.c"
	.size	.L.str10, 9

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"MIME::Base64::encode_base64"
	.size	.L.str11, 28

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"$;$"
	.size	.L.str12, 4

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"MIME::Base64::decode_base64"
	.size	.L.str13, 28

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"$"
	.size	.L.str14, 2

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"MIME::QuotedPrint::encode_qp"
	.size	.L.str15, 29

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"$;$$"
	.size	.L.str16, 5

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"MIME::QuotedPrint::decode_qp"
	.size	.L.str17, 29


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
