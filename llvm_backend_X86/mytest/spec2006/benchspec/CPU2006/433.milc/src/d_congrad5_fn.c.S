	.file	"d_congrad5_fn.c.bc"
	.text
	.globl	ks_congrad
	.align	16, 0x90
	.type	ks_congrad,@function
ks_congrad:                             # @ks_congrad
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
	subq	$416, %rsp              # imm = 0x1A0
.Ltmp11:
	.cfi_def_cfa_offset 464
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
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r9, %rax
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB0_1
# BB#2:                                 # %entry
	movabsq	$1, %r15
	jmp	.LBB0_3
.LBB0_1:
	movabsq	$0, %r15
.LBB0_3:                                # %entry
	movabsq	$4616189618054758400, %rsi # imm = 0x4010000000000000
	movq	%r14, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movl	valid_longlinks(%rip), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	jne	.LBB0_5
# BB#4:                                 # %if.then5
	xorq	%rax, %rax
	callq	load_longlinks
.LBB0_5:                                # %if.end6
	movl	valid_fatlinks(%rip), %eax
	movq	%r15, %r12
	xorq	$1, %r12
	testq	%rax, %rax
	jne	.LBB0_7
# BB#6:                                 # %if.then8
	xorq	%rax, %rax
	callq	load_fatlinks
.LBB0_7:                                # %start.preheader
	incq	%r12
	movq	%r12, 32(%rsp)          # 8-byte Spill
	incq	%r15
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	xorq	%rax, %r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,4), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_8:                                # %start.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_9 Depth 2
                                        #       Child Loop BB0_17 Depth 3
                                        #       Child Loop BB0_44 Depth 3
                                        #         Child Loop BB0_52 Depth 4
                                        #         Child Loop BB0_58 Depth 4
                                        #         Child Loop BB0_72 Depth 4
                                        #       Child Loop BB0_20 Depth 3
                                        #         Child Loop BB0_38 Depth 4
                                        #         Child Loop BB0_40 Depth 4
                                        #         Child Loop BB0_34 Depth 4
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB0_9:                                # %start
                                        #   Parent Loop BB0_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_17 Depth 3
                                        #       Child Loop BB0_44 Depth 3
                                        #         Child Loop BB0_52 Depth 4
                                        #         Child Loop BB0_58 Depth 4
                                        #         Child Loop BB0_72 Depth 4
                                        #       Child Loop BB0_20 Depth 3
                                        #         Child Loop BB0_38 Depth 4
                                        #         Child Loop BB0_40 Depth 4
                                        #         Child Loop BB0_34 Depth 4
	movq	%rdx, %r12
	movq	80(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB0_11
# BB#10:                                # %if.then11
                                        #   in Loop: Header=BB0_9 Depth=2
	leaq	256(%rsp), %rdi
	leaq	128(%rsp), %rsi
	callq	cleanup_gathers
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
.LBB0_11:                               # %if.end13
                                        #   in Loop: Header=BB0_9 Depth=2
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rbx
	andq	%r13, %rbx
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	$0, 392(%rsp)
	movq	$0, 408(%rsp)
	movq	$1232, %rsi             # imm = 0x4D0
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rdx
	callq	dslash_fn
	movq	$1232, %rdi             # imm = 0x4D0
	movq	$1232, %rsi             # imm = 0x4D0
	movq	%r14, %rdx
	callq	dslash_fn
	movl	even_sites_on_node(%rip), %r14d
	cmpq	$2, %rbx
	movq	%r14, %rax
	je	.LBB0_13
# BB#12:                                # %if.end13
                                        #   in Loop: Header=BB0_9 Depth=2
	movl	sites_on_node(%rip), %eax
.LBB0_13:                               # %if.end13
                                        #   in Loop: Header=BB0_9 Depth=2
	movq	$32, %rcx
	movq	%rax, %rdx
                                        # kill: RAX<def> RDX<kill>
	movq	%rdx, %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB0_15
# BB#14:                                # %if.end13
                                        #   in Loop: Header=BB0_9 Depth=2
	xorq	%r14, %r14
.LBB0_15:                               # %if.end13
                                        #   in Loop: Header=BB0_9 Depth=2
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%rsi, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	jge	.LBB0_18
# BB#16:                                # %for.body.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=2
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$113, %rsi, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	leaq	1088(%rax,%rsi), %rbx
	.align	16, 0x90
.LBB0_17:                               # %for.body
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1088(%rbx,%r12), %rsi
	movq	%r12, %r15
	leaq	144(%rbx), %r12
	movq	%r12, %rdi
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_vector
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	-1088(%rbx,%rax), %r13
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %r12
	movq	%rbx, %rdx
	callq	add_su3_vector
	movq	40(%rbx), %rax
	movq	%rax, 88(%rbx)
	movq	32(%rbx), %rax
	movq	%rax, 80(%rbx)
	movq	24(%rbx), %rax
	movq	%rax, 72(%rbx)
	movq	16(%rbx), %rax
	movq	%rax, 64(%rbx)
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	%rcx, 56(%rbx)
	movq	%rax, 48(%rbx)
	movq	%r13, %rdi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	callq	magsq_su3vec
	movq	392(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, 392(%rsp)
	movq	%rbx, %rdi
	callq	magsq_su3vec
	movq	408(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%rax, 408(%rsp)
	incq	%r14
	movq	%r14, %rax
	andq	%r13, %rax
	andq	%r13, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_17
.LBB0_18:                               # %for.end
                                        #   in Loop: Header=BB0_9 Depth=2
	leaq	392(%rsp), %rdi
	callq	g_doublesum
	leaq	408(%rsp), %rdi
	callq	g_doublesum
	movl	total_iters(%rip), %eax
	movq	392(%rsp), %rdi
	incq	%rax
	movl	%eax, total_iters(%rip)
	incq	104(%rsp)               # 8-byte Folded Spill
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_mul
	movq	408(%rsp), %rbx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_41
# BB#19:                                # %do.body.preheader
                                        #   in Loop: Header=BB0_9 Depth=2
	cmpq	$2, 112(%rsp)           # 8-byte Folded Reload
	movq	56(%rsp), %rdx          # 8-byte Reload
	jne	.LBB0_44
	jmp	.LBB0_20
	.align	16, 0x90
.LBB0_36:                               # %scalar_mult_add_latvec.exit.us.do.body.us_crit_edge
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	408(%rsp), %rbx
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB0_20:                               # %do.body.us
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_38 Depth 4
                                        #         Child Loop BB0_40 Depth 4
                                        #         Child Loop BB0_34 Depth 4
	movq	$0, 400(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	$1136, %rdi             # imm = 0x470
	movq	$1232, %rsi             # imm = 0x4D0
	testq	%r13, %rax
	jne	.LBB0_21
# BB#22:                                # %if.then64.us
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	$1, %r8
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	leaq	128(%rsp), %rcx
	callq	dslash_fn_special
	movq	$1232, %rdi             # imm = 0x4D0
	movq	$1232, %rsi             # imm = 0x4D0
	movq	$2, %rdx
	movq	$1, %r8
	jmp	.LBB0_23
	.align	16, 0x90
.LBB0_21:                               # %if.else.us
                                        #   in Loop: Header=BB0_20 Depth=3
	xorq	%r8, %r8
	leaq	128(%rsp), %rcx
	callq	dslash_fn_special
	movq	$1232, %rdi             # imm = 0x4D0
	movq	$1232, %rsi             # imm = 0x4D0
	movq	$2, %rdx
	xorq	%r8, %r8
.LBB0_23:                               # %if.end125.us
                                        #   in Loop: Header=BB0_20 Depth=3
	leaq	256(%rsp), %rcx
	callq	dslash_fn_special
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movslq	even_sites_on_node(%rip), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	movq	$0, 400(%rsp)
	movq	%rax, %r15
	je	.LBB0_25
# BB#24:                                # %if.end125.us
                                        #   in Loop: Header=BB0_20 Depth=3
	xorq	%r15, %r15
.LBB0_25:                               # %if.end125.us
                                        #   in Loop: Header=BB0_20 Depth=3
	cmpq	%rax, %r15
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %r8
	jge	.LBB0_26
# BB#37:                                # %for.body144.lr.ph.us
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	imulq	$113, %rdi, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	leaq	1136(%rax,%rdi), %rbx
	.align	16, 0x90
.LBB0_38:                               # %for.body144.us
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_20 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	96(%rbx), %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rcx
	movq	%r8, %r14
	movq	%rdx, %r13
	callq	scalar_mult_add_su3_vector
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	su3_rdot
	movq	400(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%r13, %rdx
	movq	%r14, %r8
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 400(%rsp)
	incq	%r15
	movq	%r15, %rax
	andq	%r8, %rax
	andq	%r8, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_38
.LBB0_26:                               # %for.end156.us
                                        #   in Loop: Header=BB0_20 Depth=3
	leaq	400(%rsp), %rdi
	callq	g_doublesum
	movl	total_iters(%rip), %eax
	movq	408(%rsp), %rsi
	incq	%rax
	movl	%eax, total_iters(%rip)
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	callq	float64_sub
	movq	400(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_div
	movslq	even_sites_on_node(%rip), %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	movq	$0, 408(%rsp)
	movq	%rcx, %r15
	je	.LBB0_28
# BB#27:                                # %for.end156.us
                                        #   in Loop: Header=BB0_20 Depth=3
	xorq	%r15, %r15
.LBB0_28:                               # %for.end156.us
                                        #   in Loop: Header=BB0_20 Depth=3
	cmpq	%rcx, %r15
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rdx
	jge	.LBB0_29
# BB#39:                                # %for.body178.lr.ph.us
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$113, %rsi, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	leaq	1232(%rax,%rsi), %rbx
	.align	16, 0x90
.LBB0_40:                               # %for.body178.us
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_20 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-96(%rbx), %rsi
	movq	%rdi, %r14
	leaq	-1232(%rbx,%r14), %rdi
	movq	%rdi, %rcx
	movq	%rdx, %r13
	callq	scalar_mult_add_su3_vector
	leaq	-144(%rbx), %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_vector
	movq	%r12, %rdi
	callq	magsq_su3vec
	movq	408(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%r13, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	movq	%r14, %rdi
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 408(%rsp)
	incq	%r15
	movq	%r15, %rax
	andq	%rsi, %rax
	andq	%rsi, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_40
.LBB0_29:                               # %for.end193.us
                                        #   in Loop: Header=BB0_20 Depth=3
	incq	104(%rsp)               # 8-byte Folded Spill
	leaq	408(%rsp), %rdi
	callq	g_doublesum
	movq	408(%rsp), %r13
	movq	%r13, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_64
# BB#30:                                # %if.end207.us
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	%r13, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
	movslq	even_sites_on_node(%rip), %r14
	movq	%rax, %r12
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	movq	%r14, %r15
	je	.LBB0_32
# BB#31:                                # %if.end207.us
                                        #   in Loop: Header=BB0_20 Depth=3
	xorq	%r15, %r15
.LBB0_32:                               # %if.end207.us
                                        #   in Loop: Header=BB0_20 Depth=3
	cmpq	%r14, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jge	.LBB0_35
# BB#33:                                # %for.body.lr.ph.i.us
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$113, %rdx, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	leaq	1136(%rax,%rdx), %rbx
	.align	16, 0x90
.LBB0_34:                               # %for.body.i.us
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_20 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-48(%rbx), %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	scalar_mult_add_su3_vector
	incq	%r15
	movq	%r15, %rax
	andq	%r13, %rax
	movq	%r14, %rcx
	andq	%r13, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_34
.LBB0_35:                               # %scalar_mult_add_latvec.exit.us
                                        #   in Loop: Header=BB0_20 Depth=3
	movq	$32, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	40(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	testq	%r13, %rdx
	jne	.LBB0_36
	jmp	.LBB0_74
	.align	16, 0x90
.LBB0_43:                               # %scalar_mult_add_latvec.exit.do.body_crit_edge
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	408(%rsp), %rbx
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB0_44:                               # %do.body
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_52 Depth 4
                                        #         Child Loop BB0_58 Depth 4
                                        #         Child Loop BB0_72 Depth 4
	movq	$0, 400(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	$1136, %rdi             # imm = 0x470
	movq	$1232, %rsi             # imm = 0x4D0
	testq	%r13, %rax
	jne	.LBB0_46
# BB#45:                                # %if.then64
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	$1, %r8
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	leaq	128(%rsp), %rcx
	callq	dslash_fn_special
	movq	$1232, %rdi             # imm = 0x4D0
	movq	$1232, %rsi             # imm = 0x4D0
	movq	$1, %r8
	jmp	.LBB0_47
	.align	16, 0x90
.LBB0_46:                               # %if.else
                                        #   in Loop: Header=BB0_44 Depth=3
	xorq	%r8, %r8
	leaq	128(%rsp), %rcx
	callq	dslash_fn_special
	movq	$1232, %rdi             # imm = 0x4D0
	movq	$1232, %rsi             # imm = 0x4D0
	xorq	%r8, %r8
.LBB0_47:                               # %if.end125
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	32(%rsp), %rdx          # 8-byte Reload
	leaq	256(%rsp), %rcx
	callq	dslash_fn_special
	movslq	sites_on_node(%rip), %r8
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	movq	$0, 400(%rsp)
	je	.LBB0_48
# BB#49:                                # %if.end125
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	jmp	.LBB0_50
	.align	16, 0x90
.LBB0_48:                               #   in Loop: Header=BB0_44 Depth=3
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movl	even_sites_on_node(%rip), %r15d
.LBB0_50:                               # %if.end125
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	movq	96(%rsp), %rdx          # 8-byte Reload
	jge	.LBB0_53
# BB#51:                                # %for.body144.lr.ph
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	imulq	$113, %rdi, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	leaq	1136(%rax,%rdi), %rbx
	.align	16, 0x90
.LBB0_52:                               # %for.body144
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	96(%rbx), %r12
	movq	%r12, %rdi
	movq	%r8, %r13
	movq	%rbx, %rsi
	movq	%r12, %rcx
	movq	%rdx, %r14
	callq	scalar_mult_add_su3_vector
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	su3_rdot
	movq	400(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%r14, %rdx
	movq	%r13, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	movq	%rax, 400(%rsp)
	incq	%r15
	movq	%r15, %rax
	andq	%rsi, %rax
	movq	%r8, %rcx
	andq	%rsi, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_52
.LBB0_53:                               # %for.end156
                                        #   in Loop: Header=BB0_44 Depth=3
	leaq	400(%rsp), %rdi
	callq	g_doublesum
	movl	total_iters(%rip), %eax
	movq	408(%rsp), %rsi
	incq	%rax
	movl	%eax, total_iters(%rip)
	incq	104(%rsp)               # 8-byte Folded Spill
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	callq	float64_sub
	movq	400(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_div
	movslq	sites_on_node(%rip), %rsi
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	movq	$0, 408(%rsp)
	movq	%rax, %rdx
	je	.LBB0_54
# BB#55:                                # %for.end156
                                        #   in Loop: Header=BB0_44 Depth=3
	xorq	%r15, %r15
	jmp	.LBB0_56
	.align	16, 0x90
.LBB0_54:                               #   in Loop: Header=BB0_44 Depth=3
	movl	even_sites_on_node(%rip), %r15d
.LBB0_56:                               # %for.end156
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	jge	.LBB0_59
# BB#57:                                # %for.body178.lr.ph
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$113, %rsi, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	leaq	1232(%rax,%rsi), %rbx
	.align	16, 0x90
.LBB0_58:                               # %for.body178
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-96(%rbx), %rsi
	movq	%rdi, %r14
	leaq	-1232(%rbx,%r14), %rdi
	movq	%rdi, %rcx
	movq	%rdx, %r13
	callq	scalar_mult_add_su3_vector
	leaq	-144(%rbx), %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	scalar_mult_add_su3_vector
	movq	%r12, %rdi
	callq	magsq_su3vec
	movq	408(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%r13, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	movq	%r14, %rdi
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 408(%rsp)
	incq	%r15
	movq	%r15, %rax
	andq	%rsi, %rax
	andq	%rsi, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_58
.LBB0_59:                               # %for.end193
                                        #   in Loop: Header=BB0_44 Depth=3
	leaq	408(%rsp), %rdi
	callq	g_doublesum
	movq	408(%rsp), %r13
	movq	%r13, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %rbx
	je	.LBB0_61
# BB#60:                                # %for.end193
                                        #   in Loop: Header=BB0_44 Depth=3
	movabsq	$1, %rbx
.LBB0_61:                               # %for.end193
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	%r13, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jle	.LBB0_63
# BB#62:                                # %for.end193
                                        #   in Loop: Header=BB0_44 Depth=3
	movabsq	$1, %rax
.LBB0_63:                               # %for.end193
                                        #   in Loop: Header=BB0_44 Depth=3
	orq	%rax, %rbx
	je	.LBB0_64
# BB#67:                                # %if.end207
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	%r13, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
	movslq	sites_on_node(%rip), %r14
	movq	%rax, %r12
	cmpq	$1, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB0_68
# BB#69:                                # %if.end207
                                        #   in Loop: Header=BB0_44 Depth=3
	xorq	%r15, %r15
	jmp	.LBB0_70
	.align	16, 0x90
.LBB0_68:                               #   in Loop: Header=BB0_44 Depth=3
	movl	even_sites_on_node(%rip), %r15d
.LBB0_70:                               # %if.end207
                                        #   in Loop: Header=BB0_44 Depth=3
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r14, %rax
	jge	.LBB0_73
# BB#71:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$113, %rdx, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	leaq	1136(%rax,%rdx), %rbx
	.align	16, 0x90
.LBB0_72:                               # %for.body.i
                                        #   Parent Loop BB0_8 Depth=1
                                        #     Parent Loop BB0_9 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-48(%rbx), %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	scalar_mult_add_su3_vector
	incq	%r15
	movq	%r15, %rax
	andq	%r13, %rax
	movq	%r14, %rcx
	andq	%r13, %rcx
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB0_72
.LBB0_73:                               # %scalar_mult_add_latvec.exit
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	$32, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	40(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	testq	%r13, %rdx
	jne	.LBB0_43
.LBB0_74:                               #   in Loop: Header=BB0_9 Depth=2
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	$32, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	64(%rsp), %rdx          # 8-byte Reload
	jl	.LBB0_9
# BB#75:                                # %if.end236
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	movq	$2, %r15
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	cmpq	$3, %rax
	movq	$2, 24(%rsp)            # 8-byte Folded Spill
	je	.LBB0_8
	jmp	.LBB0_76
	.align	16, 0x90
.LBB0_64:                               # %if.then196
                                        #   in Loop: Header=BB0_8 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	movq	$2, %r15
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	cmpq	$3, %rcx
	movq	$2, 24(%rsp)            # 8-byte Folded Spill
	movq	64(%rsp), %rdx          # 8-byte Reload
	je	.LBB0_8
	jmp	.LBB0_65
.LBB0_41:                               # %if.then56
                                        #   in Loop: Header=BB0_8 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	movq	$2, %r15
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	cmpq	$3, %rax
	movq	$2, 24(%rsp)            # 8-byte Folded Spill
	movq	64(%rsp), %rdx          # 8-byte Reload
	je	.LBB0_8
# BB#42:                                # %if.end60
	movq	464(%rsp), %rax
	movq	%rbx, (%rax)
	jmp	.LBB0_81
.LBB0_65:                               # %if.end200
	movq	464(%rsp), %rax
	movq	%r13, (%rax)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_81
# BB#66:                                # %if.then203
	leaq	256(%rsp), %rdi
	leaq	128(%rsp), %rsi
	callq	cleanup_gathers
.LBB0_81:                               # %cleanup
	movq	104(%rsp), %rax         # 8-byte Reload
	addq	$416, %rsp              # imm = 0x1A0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_76:                               # %if.end240
	movq	408(%rsp), %rax
	movq	464(%rsp), %rcx
	movq	%rax, (%rcx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_78
# BB#77:                                # %if.then243
	leaq	256(%rsp), %rdi
	leaq	128(%rsp), %rsi
	callq	cleanup_gathers
.LBB0_78:                               # %if.end246
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_80
# BB#79:                                # %if.then249
	movq	408(%rsp), %rdx
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	callq	printf
.LBB0_80:                               # %if.end251
	movq	stdout(%rip), %rdi
	callq	fflush
	jmp	.LBB0_81
.Ltmp17:
	.size	ks_congrad, .Ltmp17-ks_congrad
	.cfi_endproc

	.globl	scalar_mult_add_latvec
	.align	16, 0x90
	.type	scalar_mult_add_latvec,@function
scalar_mult_add_latvec:                 # @scalar_mult_add_latvec
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
	subq	$16, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 64
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
	movl	even_sites_on_node(%rip), %r10d
	movq	%rcx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r8
	cmpq	$2, %r8
	movq	%r10, %r9
	je	.LBB1_2
# BB#1:                                 # %entry
	movl	sites_on_node(%rip), %r9d
.LBB1_2:                                # %entry
	movq	%rdx, %r11
	movq	$32, %rcx
	movq	%r9, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	$1, %r8
	je	.LBB1_4
# BB#3:                                 # %entry
	xorq	%r10, %r10
.LBB1_4:                                # %entry
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r14, %rdx
	movq	%r11, %rdx
	jge	.LBB1_7
# BB#5:                                 # %for.body.lr.ph
	movq	lattice(%rip), %r8
	movq	$32, %rcx
	movq	%r10, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	imulq	$1808, %r11, %r11       # imm = 0x710
	addq	%r8, %r11
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%r11, %rbx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%r11, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%r11, %rdi
	andq	%rax, %r9
	.align	16, 0x90
.LBB1_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rcx
	movq	%r9, %r15
	movq	%r10, %r12
	movq	%rsi, %r13
	movq	%rdx, %r14
	callq	scalar_mult_add_su3_vector
	movq	%r14, %rdx
	movq	%r13, %rsi
	movq	%r12, %r10
	movq	%r15, %r9
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	incq	%r10
	movq	%r10, %rax
	andq	%rcx, %rax
	addq	$1808, %rbx             # imm = 0x710
	addq	$1808, %rsi             # imm = 0x710
	addq	$1808, %rdi             # imm = 0x710
	cmpq	%r9, %rax
	jne	.LBB1_6
.LBB1_7:                                # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	scalar_mult_add_latvec, .Ltmp35-scalar_mult_add_latvec
	.cfi_endproc

	.globl	clear_latvec
	.align	16, 0x90
	.type	clear_latvec,@function
clear_latvec:                           # @clear_latvec
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rsi, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	je	.LBB2_6
# BB#1:                                 # %entry
	andq	%rax, %rsi
	cmpq	$3, %rsi
	jne	.LBB2_2
# BB#9:                                 # %sw.bb36
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_12
# BB#10:                                # %for.body39.lr.ph
	movq	lattice(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	16(%rdi,%rdx), %rdx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB2_11:                               # %for.body39
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, -8(%rdx)
	movq	$0, -16(%rdx)
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	movq	$0, 24(%rdx)
	movq	$0, 16(%rdx)
	movq	$0, 8(%rdx)
	movq	$0, (%rdx)
	leaq	1808(%rdx), %rdx
	jl	.LBB2_11
	jmp	.LBB2_12
.LBB2_6:                                # %sw.bb10
	movslq	even_sites_on_node(%rip), %rax
	movslq	sites_on_node(%rip), %rdx
	cmpq	%rdx, %rax
	jge	.LBB2_12
# BB#7:                                 # %for.body15.lr.ph
	movq	lattice(%rip), %rcx
	imulq	$1808, %rax, %rsi       # imm = 0x710
	addq	%rcx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	16(%rdi,%rsi), %rsi
	.align	16, 0x90
.LBB2_8:                                # %for.body15
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, -8(%rsi)
	movq	$0, -16(%rsi)
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdx, %rdi
	movq	$0, 24(%rsi)
	movq	$0, 16(%rsi)
	movq	$0, 8(%rsi)
	movq	$0, (%rsi)
	leaq	1808(%rsi), %rsi
	jl	.LBB2_8
	jmp	.LBB2_12
.LBB2_2:                                # %entry
	cmpq	$2, %rsi
	jne	.LBB2_12
# BB#3:                                 # %sw.bb
	movslq	even_sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_12
# BB#4:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	16(%rdi,%rdx), %rdx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB2_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, -8(%rdx)
	movq	$0, -16(%rdx)
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	movq	$0, 24(%rdx)
	movq	$0, 16(%rdx)
	movq	$0, 8(%rdx)
	movq	$0, (%rdx)
	leaq	1808(%rdx), %rdx
	jl	.LBB2_5
.LBB2_12:                               # %sw.epilog
	retq
.Ltmp36:
	.size	clear_latvec, .Ltmp36-clear_latvec
	.cfi_endproc

	.globl	copy_latvec
	.align	16, 0x90
	.type	copy_latvec,@function
copy_latvec:                            # @copy_latvec
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rdx, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	je	.LBB3_9
# BB#1:                                 # %entry
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jne	.LBB3_2
# BB#6:                                 # %for.cond.preheader
	movslq	even_sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB3_12
# BB#7:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %r9
	.align	16, 0x90
.LBB3_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	lattice(%rip), %rcx
	movq	40(%rcx,%rdi), %rdx
	movq	%rdx, 40(%rcx,%rsi)
	movq	32(%rcx,%rdi), %rdx
	movq	%rdx, 32(%rcx,%rsi)
	movq	24(%rcx,%rdi), %rdx
	movq	%rdx, 24(%rcx,%rsi)
	movq	16(%rcx,%rdi), %rdx
	movq	%rdx, 16(%rcx,%rsi)
	movq	(%rcx,%rdi), %r8
	movq	8(%rcx,%rdi), %rdx
	movq	%rdx, 8(%rcx,%rsi)
	movq	%r8, (%rcx,%rsi)
	movslq	even_sites_on_node(%rip), %rdx
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r9
	addq	$1808, %rsi             # imm = 0x710
	addq	$1808, %rdi             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB3_8
	jmp	.LBB3_12
.LBB3_9:                                # %sw.bb3
	movslq	even_sites_on_node(%rip), %r9
	movslq	sites_on_node(%rip), %rcx
	cmpq	%rcx, %r9
	jge	.LBB3_12
# BB#10:                                # %for.body8.lr.ph
	imulq	$1808, %r9, %rdx        # imm = 0x710
	incq	%r9
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%rdx, %rdi
	.align	16, 0x90
.LBB3_11:                               # %for.body8
                                        # =>This Inner Loop Header: Depth=1
	movq	lattice(%rip), %rcx
	movq	40(%rcx,%rdi), %rdx
	movq	%rdx, 40(%rcx,%rsi)
	movq	32(%rcx,%rdi), %rdx
	movq	%rdx, 32(%rcx,%rsi)
	movq	24(%rcx,%rdi), %rdx
	movq	%rdx, 24(%rcx,%rsi)
	movq	16(%rcx,%rdi), %rdx
	movq	%rdx, 16(%rcx,%rsi)
	movq	(%rcx,%rdi), %r8
	movq	8(%rcx,%rdi), %rdx
	movq	%rdx, 8(%rcx,%rsi)
	movq	%r8, (%rcx,%rsi)
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r9
	addq	$1808, %rsi             # imm = 0x710
	addq	$1808, %rdi             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB3_11
	jmp	.LBB3_12
.LBB3_2:                                # %entry
	cmpq	$3, %rdx
	jne	.LBB3_12
# BB#3:                                 # %for.cond20.preheader
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB3_12
# BB#4:                                 # %for.body22.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %r9
	.align	16, 0x90
.LBB3_5:                                # %for.body22
                                        # =>This Inner Loop Header: Depth=1
	movq	lattice(%rip), %rcx
	movq	40(%rcx,%rdi), %rdx
	movq	%rdx, 40(%rcx,%rsi)
	movq	32(%rcx,%rdi), %rdx
	movq	%rdx, 32(%rcx,%rsi)
	movq	24(%rcx,%rdi), %rdx
	movq	%rdx, 24(%rcx,%rsi)
	movq	16(%rcx,%rdi), %rdx
	movq	%rdx, 16(%rcx,%rsi)
	movq	(%rcx,%rdi), %r8
	movq	8(%rcx,%rdi), %rdx
	movq	%rdx, 8(%rcx,%rsi)
	movq	%r8, (%rcx,%rsi)
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r9
	addq	$1808, %rsi             # imm = 0x710
	addq	$1808, %rdi             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB3_5
.LBB3_12:                               # %sw.epilog
	retq
.Ltmp37:
	.size	copy_latvec, .Ltmp37-copy_latvec
	.cfi_endproc

	.globl	scalar2_mult_add_su3_vector
	.align	16, 0x90
	.type	scalar2_mult_add_su3_vector,@function
scalar2_mult_add_su3_vector:            # @scalar2_mult_add_su3_vector
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp44:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp45:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp46:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp47:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp49:
	.cfi_def_cfa_offset 80
.Ltmp50:
	.cfi_offset %rbx, -48
.Ltmp51:
	.cfi_offset %r12, -40
.Ltmp52:
	.cfi_offset %r13, -32
.Ltmp53:
	.cfi_offset %r14, -24
.Ltmp54:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %r14
	movq	8(%rbx), %rax
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	(%r12), %r14
	movq	8(%r12), %rsi
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_add
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx)
	movq	%r14, (%rcx)
	movq	16(%rbx), %r14
	movq	24(%rbx), %rsi
	movq	%r13, %rdi
	callq	float64_mul
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	16(%r12), %r14
	movq	24(%r12), %rsi
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_add
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 24(%rcx)
	movq	%r14, 16(%rcx)
	movq	32(%rbx), %r14
	movq	40(%rbx), %rsi
	movq	%r13, %rdi
	callq	float64_mul
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	32(%r12), %rbx
	movq	40(%r12), %rsi
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_add
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 40(%rcx)
	movq	%rbx, 32(%rcx)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp55:
	.size	scalar2_mult_add_su3_vector, .Ltmp55-scalar2_mult_add_su3_vector
	.cfi_endproc

	.globl	scalar2_mult_add_latvec
	.align	16, 0x90
	.type	scalar2_mult_add_latvec,@function
scalar2_mult_add_latvec:                # @scalar2_mult_add_latvec
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
	subq	$64, %rsp
.Ltmp67:
	.cfi_def_cfa_offset 112
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
	movl	even_sites_on_node(%rip), %ebx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r9
	cmpq	$2, %r9
	movq	%rbx, %rax
	je	.LBB5_2
# BB#1:                                 # %entry
	movl	sites_on_node(%rip), %eax
.LBB5_2:                                # %entry
	movq	%rsi, %r11
	movq	%rcx, %r10
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	movq	%rsi, %r15
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %r9
	je	.LBB5_4
# BB#3:                                 # %entry
	xorq	%rbx, %rbx
.LBB5_4:                                # %entry
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	movq	%r10, 16(%rsp)          # 8-byte Spill
	movq	%r11, %r9
	movq	%r9, 56(%rsp)           # 8-byte Spill
	movq	%r15, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jge	.LBB5_7
# BB#5:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$1808, %rsi, %rsi       # imm = 0x710
	addq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	leaq	32(%r8,%rsi), %r15
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	32(%rdx,%rsi), %r14
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	32(%rdi,%rsi), %r12
	.align	16, 0x90
.LBB5_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	-32(%r12), %rbx
	movq	-24(%r12), %rsi
	movq	%r9, %rdi
	callq	float64_mul
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_mul
	movq	-32(%r14), %r13
	movq	-24(%r14), %rsi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, -24(%r15)
	movq	%r13, -32(%r15)
	movq	-16(%r12), %r13
	movq	-8(%r12), %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	float64_mul
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	-16(%r14), %r13
	movq	-8(%r14), %rsi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, -8(%r15)
	movq	%r13, -16(%r15)
	movq	(%r12), %r13
	movq	8(%r12), %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	float64_mul
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	(%r14), %r13
	movq	8(%r14), %rsi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	8(%rsp), %rsi           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	movq	56(%rsp), %r9           # 8-byte Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rax, 8(%r15)
	movq	%r13, (%r15)
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	addq	$1808, %r15             # imm = 0x710
	addq	$1808, %r14             # imm = 0x710
	addq	$1808, %r12             # imm = 0x710
	cmpq	%rcx, %rax
	jne	.LBB5_6
.LBB5_7:                                # %for.end
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp73:
	.size	scalar2_mult_add_latvec, .Ltmp73-scalar2_mult_add_latvec
	.cfi_endproc

	.globl	scalar_mult_latvec
	.align	16, 0x90
	.type	scalar_mult_latvec,@function
scalar_mult_latvec:                     # @scalar_mult_latvec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp80:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp81:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp82:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp83:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp84:
	.cfi_def_cfa_offset 48
.Ltmp85:
	.cfi_offset %rbx, -48
.Ltmp86:
	.cfi_offset %r12, -40
.Ltmp87:
	.cfi_offset %r13, -32
.Ltmp88:
	.cfi_offset %r14, -24
.Ltmp89:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rax, %rdx
	cmpq	$1, %rdx
	je	.LBB6_6
# BB#1:                                 # %entry
	andq	%rax, %rcx
	cmpq	$3, %rcx
	jne	.LBB6_2
# BB#9:                                 # %sw.bb15
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB6_12
# BB#10:                                # %for.body18.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB6_11:                               # %for.body18
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	scalar_mult_su3_vector
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	sites_on_node(%rip), %rdx
	sarq	%cl, %rax
	addq	$1808, %r15             # imm = 0x710
	addq	$1808, %r13             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB6_11
	jmp	.LBB6_12
.LBB6_6:                                # %sw.bb3
	movslq	even_sites_on_node(%rip), %r12
	movslq	sites_on_node(%rip), %rax
	cmpq	%rax, %r12
	jge	.LBB6_12
# BB#7:                                 # %for.body6.lr.ph
	movq	lattice(%rip), %rax
	imulq	$1808, %r12, %rdx       # imm = 0x710
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rdx, %r15
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rdx, %r13
	.align	16, 0x90
.LBB6_8:                                # %for.body6
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	scalar_mult_su3_vector
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	sites_on_node(%rip), %rdx
	sarq	%cl, %rax
	addq	$1808, %r15             # imm = 0x710
	addq	$1808, %r13             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB6_8
	jmp	.LBB6_12
.LBB6_2:                                # %entry
	cmpq	$2, %rcx
	jne	.LBB6_12
# BB#3:                                 # %sw.bb
	movslq	even_sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB6_12
# BB#4:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %r13
	xorq	%r12, %r12
	.align	16, 0x90
.LBB6_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	scalar_mult_su3_vector
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	even_sites_on_node(%rip), %rdx
	sarq	%cl, %rax
	addq	$1808, %r15             # imm = 0x710
	addq	$1808, %r13             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB6_5
.LBB6_12:                               # %sw.epilog
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp90:
	.size	scalar_mult_latvec, .Ltmp90-scalar_mult_latvec
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"CG not converged after %d iterations, res. = %e wanted %e\n"
	.size	.L.str, 59


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
