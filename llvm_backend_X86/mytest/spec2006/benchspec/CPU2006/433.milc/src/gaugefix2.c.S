	.file	"gaugefix2.c.bc"
	.text
	.globl	accum_gauge_hit
	.align	16, 0x90
	.type	accum_gauge_hit,@function
accum_gauge_hit:                        # @accum_gauge_hit
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
	movl	even_sites_on_node(%rip), %edx
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rsi, %rbx
	andq	%rcx, %rbx
	cmpq	$1, %rbx
	movq	%rdx, %rsi
	je	.LBB0_2
# BB#1:                                 # %entry
	xorq	%rsi, %rsi
.LBB0_2:                                # %entry
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	sites_on_node(%rip), %r8d
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$2, %rbx
	movq	%rdx, %rbx
	je	.LBB0_4
# BB#3:                                 # %entry
	movq	%r8, %rbx
.LBB0_4:                                # %entry
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdi
	jge	.LBB0_15
# BB#5:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$3200, %rsi, %r14       # imm = 0xC80
	addq	%rax, %r14
	movq	$4, %rcx
	leaq	1(%rsi), %rbx
	shlq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %r15
	leaq	(%rsi,%rsi,8), %r12
	movq	8(%rsp), %r13           # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	.align	16, 0x90
.LBB0_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	diffmat_offset(%rip), %rdi
	testq	%rdi, %rdi
	js	.LBB0_8
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB0_6 Depth=1
	addq	%r14, %rdi
	jmp	.LBB0_9
	.align	16, 0x90
.LBB0_8:                                # %if.else
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	diffmatp(%rip), %rdi
	addq	%r12, %rdi
.LBB0_9:                                # %if.end
                                        #   in Loop: Header=BB0_6 Depth=1
	callq	clear_su3mat
	movslq	sumvec_offset(%rip), %rdi
	testq	%rdi, %rdi
	js	.LBB0_11
# BB#10:                                # %if.then11
                                        #   in Loop: Header=BB0_6 Depth=1
	addq	%r14, %rdi
	jmp	.LBB0_12
	.align	16, 0x90
.LBB0_11:                               # %if.else14
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	sumvecp(%rip), %rdi
	addq	%r15, %rdi
.LBB0_12:                               # %for.inc
                                        #   in Loop: Header=BB0_6 Depth=1
	callq	clearvec
	movl	even_sites_on_node(%rip), %edx
	movl	sites_on_node(%rip), %r8d
	cmpq	$2, %r13
	movq	%rdx, %rsi
	je	.LBB0_14
# BB#13:                                # %for.inc
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	%r8, %rsi
.LBB0_14:                               # %for.inc
                                        #   in Loop: Header=BB0_6 Depth=1
	addq	$3200, %r14             # imm = 0xC80
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rbx
	addq	$48, %r15
	addq	$144, %r12
	cmpq	%rsi, %rdi
	jl	.LBB0_6
.LBB0_15:                               # %for.end
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	andq	%r13, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$1, %rcx
	movq	%rdx, %rbx
	je	.LBB0_17
# BB#16:                                # %for.end
	xorq	%rbx, %rbx
.LBB0_17:                               # %for.end
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, 8(%rsp)             # 8-byte Folded Reload
	movq	%rdx, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	je	.LBB0_19
# BB#18:                                # %for.end
	movq	%r8, %rdi
.LBB0_19:                               # %for.end
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rax
	jge	.LBB0_34
# BB#20:                                # %for.cond33.preheader.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$3200, %rbx, %rcx       # imm = 0xC80
	leaq	(%rax,%rcx), %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	leaq	112(%rax,%rcx), %rcx
	.align	16, 0x90
.LBB0_21:                               # %for.cond33.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_22 Depth 2
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movq	%rcx, %rbx
	.align	16, 0x90
.LBB0_22:                               # %for.body35
                                        #   Parent Loop BB0_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rax
	andq	%r13, %rax
	movq	%rsi, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB0_30
# BB#23:                                # %if.then37
                                        #   in Loop: Header=BB0_22 Depth=2
	movslq	diffmat_offset(%rip), %rdi
	testq	%rdi, %rdi
	js	.LBB0_25
# BB#24:                                # %if.then41
                                        #   in Loop: Header=BB0_22 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_26
	.align	16, 0x90
.LBB0_25:                               # %if.else46
                                        #   in Loop: Header=BB0_22 Depth=2
	movq	diffmatp(%rip), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rcx,8), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
.LBB0_26:                               # %if.end51
                                        #   in Loop: Header=BB0_22 Depth=2
	addq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rdi, %rdx
	callq	sub_su3_matrix
	movslq	sumvec_offset(%rip), %r14
	testq	%r14, %r14
	js	.LBB0_27
# BB#28:                                # %for.cond54.preheader
                                        #   in Loop: Header=BB0_22 Depth=2
	movq	(%rbx), %rdi
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, (%r12,%r14)
	movq	8(%rbx), %rdi
	movq	8(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, 8(%r12,%r14)
	movq	64(%rbx), %rdi
	movq	16(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, 16(%r12,%r14)
	movq	72(%rbx), %rdi
	movq	24(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, 24(%r12,%r14)
	movq	128(%rbx), %rdi
	movq	32(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, 32(%r12,%r14)
	movq	136(%rbx), %rdi
	movq	40(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, 40(%r12,%r14)
	jmp	.LBB0_29
	.align	16, 0x90
.LBB0_27:                               # %for.cond82.preheader
                                        #   in Loop: Header=BB0_22 Depth=2
	movq	sumvecp(%rip), %r12
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,2), %r14
	movq	$4, %rcx
	shlq	%cl, %r14
	movq	(%rbx), %rdi
	movq	8(%rbx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	(%r12,%r14), %rsi
	movq	8(%r12,%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 8(%r12,%r14)
	movq	%r13, (%r12,%r14)
	movq	64(%rbx), %rdi
	movq	72(%rbx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	16(%r12,%r14), %rsi
	movq	24(%r12,%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 24(%r12,%r14)
	movq	%r13, 16(%r12,%r14)
	movq	128(%rbx), %rdi
	movq	136(%rbx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	32(%r12,%r14), %rsi
	movq	40(%r12,%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 40(%r12,%r14)
	movq	%r13, 32(%r12,%r14)
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB0_29:                               # %for.inc116
                                        #   in Loop: Header=BB0_22 Depth=2
	movq	56(%rsp), %rsi          # 8-byte Reload
.LBB0_30:                               # %for.inc116
                                        #   in Loop: Header=BB0_22 Depth=2
	incq	%r15
	addq	$144, %rbx
	cmpq	$4, %r15
	jne	.LBB0_22
# BB#31:                                # %for.inc119
                                        #   in Loop: Header=BB0_21 Depth=1
	movl	even_sites_on_node(%rip), %edx
	movl	sites_on_node(%rip), %r8d
	movq	48(%rsp), %rbx          # 8-byte Reload
	incq	%rbx
	cmpq	$2, 8(%rsp)             # 8-byte Folded Reload
	movq	%rdx, %rax
	je	.LBB0_33
# BB#32:                                # %for.inc119
                                        #   in Loop: Header=BB0_21 Depth=1
	movq	%r8, %rax
.LBB0_33:                               # %for.inc119
                                        #   in Loop: Header=BB0_21 Depth=1
	addq	$3200, 40(%rsp)         # 8-byte Folded Spill
                                        # imm = 0xC80
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	$3200, %rcx             # imm = 0xC80
	cmpq	%rax, %rdi
	jl	.LBB0_21
.LBB0_34:                               # %for.end122
	cmpq	$1, 8(%rsp)             # 8-byte Folded Reload
	movq	%rdx, %rax
	je	.LBB0_36
# BB#35:                                # %for.end122
	xorq	%rax, %rax
.LBB0_36:                               # %for.end122
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	lattice(%rip), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	cmpq	$2, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB0_38
# BB#37:                                # %for.end122
	movq	%r8, %rdx
.LBB0_38:                               # %for.end122
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, 48(%rsp)          # 8-byte Folded Reload
	jge	.LBB0_64
# BB#39:                                # %for.cond138.preheader.lr.ph
	movq	48(%rsp), %rax          # 8-byte Reload
	imulq	$3200, %rax, %rax       # imm = 0xC80
	addq	%rax, 40(%rsp)          # 8-byte Folded Spill
	cmpq	$2, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB0_52
# BB#40:
	andq	%r13, %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_41:                               # %for.cond138.preheader.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_42 Depth 2
	xorq	%r14, %r14
	.align	16, 0x90
.LBB0_42:                               # %for.body140.us
                                        #   Parent Loop BB0_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	%rsi, %rax
	je	.LBB0_49
# BB#43:                                # %if.then142.us
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	gen_pt(,%r14,8), %rax
	movslq	diffmat_offset(%rip), %rdi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	(%rax,%rbx,8), %r15
	testq	%rdi, %rdi
	js	.LBB0_44
# BB#45:                                # %if.then148.us
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_46
	.align	16, 0x90
.LBB0_44:                               # %if.else153.us
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	diffmatp(%rip), %rax
	leaq	(%rbx,%rbx,8), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
.LBB0_46:                               # %if.end158.us
                                        #   in Loop: Header=BB0_42 Depth=2
	addq	%rax, %rdi
	movq	%r15, %rsi
	movq	%rdi, %rdx
	callq	add_su3_matrix
	movslq	sumvec_offset(%rip), %r12
	testq	%r12, %r12
	js	.LBB0_51
# BB#47:                                # %for.cond161.preheader.us
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	(%r15), %rdi
	movq	8(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx,%r12), %rsi
	movq	8(%rbx,%r12), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 8(%rbx,%r12)
	movq	%r13, (%rbx,%r12)
	movq	64(%r15), %rdi
	movq	72(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	16(%rbx,%r12), %rsi
	movq	24(%rbx,%r12), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 24(%rbx,%r12)
	movq	%r13, 16(%rbx,%r12)
	movq	128(%r15), %rdi
	movq	136(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	32(%rbx,%r12), %rsi
	movq	40(%rbx,%r12), %r13
	callq	float64_add
	movq	%rax, %r15
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 40(%rbx,%r12)
	movq	%r15, 32(%rbx,%r12)
	jmp	.LBB0_48
	.align	16, 0x90
.LBB0_51:                               # %for.cond194.preheader.us
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	sumvecp(%rip), %r12
	leaq	(%rbx,%rbx,2), %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	movq	(%r15), %rdi
	movq	8(%r15), %rbx
	movq	(%r12,%r13), %rsi
	movq	8(%r12,%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 8(%r12,%r13)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r12,%r13)
	movq	64(%r15), %rdi
	movq	72(%r15), %rbx
	movq	16(%r12,%r13), %rsi
	movq	24(%r12,%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 24(%r12,%r13)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r12,%r13)
	movq	128(%r15), %rdi
	movq	136(%r15), %r15
	movq	32(%r12,%r13), %rsi
	movq	40(%r12,%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 40(%r12,%r13)
	movq	%rbx, 32(%r12,%r13)
.LBB0_48:                               # %for.inc228.us
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	56(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB0_49:                               # %for.inc228.us
                                        #   in Loop: Header=BB0_42 Depth=2
	incq	%r14
	cmpq	$4, %r14
	jne	.LBB0_42
# BB#50:                                # %for.inc231.us
                                        #   in Loop: Header=BB0_41 Depth=1
	movq	48(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	addq	$3200, 40(%rsp)         # 8-byte Folded Spill
                                        # imm = 0xC80
	movslq	even_sites_on_node(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_41
	jmp	.LBB0_64
.LBB0_52:
	andq	%r13, %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_53:                               # %for.cond138.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_54 Depth 2
	xorq	%r14, %r14
	.align	16, 0x90
.LBB0_54:                               # %for.body140
                                        #   Parent Loop BB0_53 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	%rsi, %rax
	je	.LBB0_62
# BB#55:                                # %if.then142
                                        #   in Loop: Header=BB0_54 Depth=2
	movq	gen_pt(,%r14,8), %rax
	movslq	diffmat_offset(%rip), %rdi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	(%rax,%rbx,8), %r15
	testq	%rdi, %rdi
	js	.LBB0_57
# BB#56:                                # %if.then148
                                        #   in Loop: Header=BB0_54 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_58
	.align	16, 0x90
.LBB0_57:                               # %if.else153
                                        #   in Loop: Header=BB0_54 Depth=2
	movq	diffmatp(%rip), %rax
	leaq	(%rbx,%rbx,8), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
.LBB0_58:                               # %if.end158
                                        #   in Loop: Header=BB0_54 Depth=2
	addq	%rax, %rdi
	movq	%r15, %rsi
	movq	%rdi, %rdx
	callq	add_su3_matrix
	movslq	sumvec_offset(%rip), %r12
	testq	%r12, %r12
	js	.LBB0_59
# BB#60:                                # %for.cond161.preheader
                                        #   in Loop: Header=BB0_54 Depth=2
	movq	(%r15), %rdi
	movq	8(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx,%r12), %rsi
	movq	8(%rbx,%r12), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 8(%rbx,%r12)
	movq	%r13, (%rbx,%r12)
	movq	64(%r15), %rdi
	movq	72(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	16(%rbx,%r12), %rsi
	movq	24(%rbx,%r12), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %r13
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 24(%rbx,%r12)
	movq	%r13, 16(%rbx,%r12)
	movq	128(%r15), %rdi
	movq	136(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	32(%rbx,%r12), %rsi
	movq	40(%rbx,%r12), %r13
	callq	float64_add
	movq	%rax, %r15
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, 40(%rbx,%r12)
	movq	%r15, 32(%rbx,%r12)
	jmp	.LBB0_61
	.align	16, 0x90
.LBB0_59:                               # %for.cond194.preheader
                                        #   in Loop: Header=BB0_54 Depth=2
	movq	sumvecp(%rip), %r12
	leaq	(%rbx,%rbx,2), %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	movq	(%r15), %rdi
	movq	8(%r15), %rbx
	movq	(%r12,%r13), %rsi
	movq	8(%r12,%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 8(%r12,%r13)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r12,%r13)
	movq	64(%r15), %rdi
	movq	72(%r15), %rbx
	movq	16(%r12,%r13), %rsi
	movq	24(%r12,%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 24(%r12,%r13)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r12,%r13)
	movq	128(%r15), %rdi
	movq	136(%r15), %r15
	movq	32(%r12,%r13), %rsi
	movq	40(%r12,%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	float64_add
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, 40(%r12,%r13)
	movq	%rbx, 32(%r12,%r13)
.LBB0_61:                               # %for.inc228
                                        #   in Loop: Header=BB0_54 Depth=2
	movq	56(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB0_62:                               # %for.inc228
                                        #   in Loop: Header=BB0_54 Depth=2
	incq	%r14
	cmpq	$4, %r14
	jne	.LBB0_54
# BB#63:                                # %for.inc231
                                        #   in Loop: Header=BB0_53 Depth=1
	movq	48(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	addq	$3200, 40(%rsp)         # 8-byte Folded Spill
                                        # imm = 0xC80
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_53
.LBB0_64:                               # %for.end234
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	accum_gauge_hit, .Ltmp17-accum_gauge_hit
	.cfi_endproc

	.globl	do_hit
	.align	16, 0x90
	.type	do_hit,@function
do_hit:                                 # @do_hit
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
	subq	$368, %rsp              # imm = 0x170
.Ltmp29:
	.cfi_def_cfa_offset 416
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
	movq	%r9, %r14
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%rcx, %r13
	movq	%r13, 120(%rsp)         # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rsi, %r12
	andq	%r15, %r12
	callq	accum_gauge_hit
	movl	even_sites_on_node(%rip), %eax
	cmpq	$1, %r12
	movq	%rax, %rsi
	je	.LBB1_2
# BB#1:                                 # %entry
	xorq	%rsi, %rsi
.LBB1_2:                                # %entry
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %r12
	je	.LBB1_4
# BB#3:                                 # %entry
	movl	sites_on_node(%rip), %eax
.LBB1_4:                                # %entry
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	movq	%rsi, %r14
	jge	.LBB1_27
# BB#5:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$3200, %r14, %r12       # imm = 0xC80
	addq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%r13, 136(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB1_6:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_14 Depth 2
                                        #     Child Loop BB1_20 Depth 2
	movslq	sumvec_offset(%rip), %rax
	testq	%rax, %rax
	js	.LBB1_8
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB1_6 Depth=1
	addq	%r12, %rax
	movq	$4, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	(%rdx,%rax), %rdi
	movq	$4, %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	(%rdx,%rax), %rsi
	jmp	.LBB1_9
	.align	16, 0x90
.LBB1_8:                                # %if.else
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	sumvecp(%rip), %rax
	movq	$4, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	leaq	(%r14,%r14,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	addq	%rax, %rsi
	movq	(%rdx,%rsi), %rdi
	movq	$4, %rcx
	movq	136(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	(%rax,%rsi), %rsi
.LBB1_9:                                # %if.end
                                        #   in Loop: Header=BB1_6 Depth=1
	callq	float64_add
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movslq	diffmat_offset(%rip), %rax
	testq	%rax, %rax
	js	.LBB1_11
# BB#10:                                # %if.then30
                                        #   in Loop: Header=BB1_6 Depth=1
	addq	%r12, %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rsi,2), %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	$4, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %r13
	shlq	%cl, %r13
	movq	(%r13,%rbx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	8(%r13,%rbx), %rdi
	movq	%r14, 104(%rsp)         # 8-byte Spill
	leaq	(%rdx,%rdx,2), %r14
	movq	$4, %rcx
	shlq	%cl, %r14
	addq	%rax, %r14
	jmp	.LBB1_12
	.align	16, 0x90
.LBB1_11:                               # %if.else80
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	diffmatp(%rip), %rax
	movq	136(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rsi,2), %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	leaq	(%r14,%r14,8), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	addq	%rax, %rdx
	addq	%rdx, %rbx
	movq	$4, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, %r13
	shlq	%cl, %r13
	movq	(%r13,%rbx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	8(%r13,%rbx), %rdi
	movq	%r14, 104(%rsp)         # 8-byte Spill
	leaq	(%rax,%rax,2), %r14
	movq	$4, %rcx
	shlq	%cl, %r14
	addq	%rdx, %r14
.LBB1_12:                               # %if.end133
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$4, %rcx
	movq	%r12, 128(%rsp)         # 8-byte Spill
	movq	%rsi, %r12
	shlq	%cl, %r12
	movq	(%r12,%r14), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	8(%r12,%r14), %rsi
	callq	float64_add
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	float64_sub
	movq	8(%r13,%r14), %rdi
	movq	8(%r12,%rbx), %rsi
	movq	%rax, %r14
	callq	float64_sub
	movq	%rax, %r12
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	%r14, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %r14
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movq	%rax, %r12
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_div
	movq	%rax, %r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r13
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	cmplx
	movq	%rax, 304(%rsp)
	movq	%rdx, 312(%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	cmplx
	movq	%rax, 320(%rsp)
	movq	%rdx, 328(%rsp)
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	movq	%rax, %r14
	xorq	%r14, %rbx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	cmplx
	movq	%rax, 336(%rsp)
	movq	%rdx, 344(%rsp)
	xorq	%r14, %r12
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	cmplx
	movq	%rax, 352(%rsp)
	movq	%rdx, 360(%rsp)
	movq	128(%rsp), %r12         # 8-byte Reload
	leaq	112(%r12), %rcx
	leaq	304(%rsp), %rbx
	movq	%rbx, %rdi
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdx
	callq	left_su2_hit_n
	leaq	256(%r12), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	left_su2_hit_n
	leaq	400(%r12), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	left_su2_hit_n
	leaq	544(%r12), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	left_su2_hit_n
	movq	gen_pt(%rip), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	right_su2_hit_a
	movq	gen_pt+8(%rip), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	right_su2_hit_a
	movq	gen_pt+16(%rip), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	right_su2_hit_a
	movq	gen_pt+24(%rip), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	%r13, %rdx
	callq	right_su2_hit_a
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	movq	416(%rsp), %r13
	jle	.LBB1_18
# BB#13:                                # %for.body188.lr.ph
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$1, %rbx
	.align	16, 0x90
.LBB1_14:                               # %for.body188
                                        #   Parent Loop BB1_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	424(%rsp), %rax
	movl	-4(%rax,%rbx,4), %eax
	cmpq	$3, %rax
	je	.LBB1_16
# BB#15:                                # %for.body188
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	152(%rsp), %rcx         # 8-byte Reload
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_17
.LBB1_16:                               # %if.then195
                                        #   in Loop: Header=BB1_14 Depth=2
	movslq	-4(%r13,%rbx,4), %rax
	addq	%r12, %rax
	movq	$4, %rcx
	movq	144(%rsp), %rsi         # 8-byte Reload
	shlq	%cl, %rsi
	addq	%rax, %rsi
	movq	$4, %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	addq	%rax, %rdx
	leaq	304(%rsp), %rdi
	callq	mult_su2_mat_vec_elem_n
.LBB1_17:                               # %for.inc211
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	%r14, %rcx
	andq	%r15, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB1_14
.LBB1_18:                               # %for.cond214.preheader
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$32, %rcx
	movq	432(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	448(%rsp), %r14
	movq	%r14, %rdx
	jle	.LBB1_24
# BB#19:                                # %for.body216.lr.ph
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$1, %rbx
	.align	16, 0x90
.LBB1_20:                               # %for.body216
                                        #   Parent Loop BB1_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	-4(%rdx,%rbx,4), %eax
	cmpq	$3, %rax
	je	.LBB1_22
# BB#21:                                # %for.body216
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	152(%rsp), %rcx         # 8-byte Reload
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_23
.LBB1_22:                               # %if.then224
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	440(%rsp), %rax
	movslq	-4(%rax,%rbx,4), %rdi
	addq	%r12, %rdi
	leaq	160(%rsp), %r13
	movq	%r13, %rsi
	callq	uncompress_anti_hermitian
	leaq	304(%rsp), %r14
	movq	%r14, %rdi
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	left_su2_hit_n
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	right_su2_hit_a
	movq	440(%rsp), %rax
	movslq	-4(%rax,%rbx,4), %rsi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	128(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rsi
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	make_anti_hermitian
	movq	448(%rsp), %rdx
.LBB1_23:                               # %for.inc234
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	432(%rsp), %rcx
	andq	%r15, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB1_20
.LBB1_24:                               # %for.inc237
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	andq	%r15, %rax
	movq	104(%rsp), %r14         # 8-byte Reload
	incq	%r14
	cmpq	$2, %rax
	movabsq	$even_sites_on_node, %rax
	je	.LBB1_26
# BB#25:                                # %for.inc237
                                        #   in Loop: Header=BB1_6 Depth=1
	movabsq	$sites_on_node, %rax
.LBB1_26:                               # %for.inc237
                                        #   in Loop: Header=BB1_6 Depth=1
	addq	$3200, %r12             # imm = 0xC80
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB1_6
.LBB1_27:                               # %for.end239
	addq	$368, %rsp              # imm = 0x170
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	do_hit, .Ltmp35-do_hit
	.cfi_endproc

	.globl	get_gauge_fix_action
	.align	16, 0x90
	.type	get_gauge_fix_action,@function
get_gauge_fix_action:                   # @get_gauge_fix_action
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
	subq	$16, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, (%rsp)            # 8-byte Spill
	movl	even_sites_on_node(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$1, %rsi
	movq	$0, 8(%rsp)
	movq	%rax, %r13
	je	.LBB2_2
# BB#1:                                 # %entry
	xorq	%r13, %r13
.LBB2_2:                                # %entry
	movq	$32, %rcx
	movq	%r13, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	$2, %rsi
	je	.LBB2_4
# BB#3:                                 # %entry
	movl	sites_on_node(%rip), %eax
.LBB2_4:                                # %entry
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r12
	jge	.LBB2_20
# BB#5:                                 # %for.cond7.preheader.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$3, %rcx
	imulq	$3200, %r13, %rdx       # imm = 0xC80
	shlq	%cl, %r13
	leaq	544(%rax,%rdx), %rbx
	cmpq	$2, %rsi
	jne	.LBB2_6
# BB#13:                                # %for.cond7.preheader.us.preheader
	movq	(%rsp), %r14            # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	.align	16, 0x90
.LBB2_14:                               # %for.cond7.preheader.us
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_16
# BB#15:                                # %for.inc.us
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	gen_pt(%rip), %rax
	movq	(%rax,%r13), %r15
	leaq	-432(%rbx), %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	cmpq	$1, %r14
	je	.LBB2_17
.LBB2_16:                               # %for.inc.us.1
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	gen_pt+8(%rip), %rax
	movq	(%rax,%r13), %r15
	leaq	-288(%rbx), %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	cmpq	$2, %r14
	je	.LBB2_18
.LBB2_17:                               # %for.inc.us.2
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	gen_pt+16(%rip), %rax
	movq	(%rax,%r13), %r15
	leaq	-144(%rbx), %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	cmpq	$3, %r14
	je	.LBB2_19
.LBB2_18:                               # %if.then.us.3
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	gen_pt+24(%rip), %rax
	movq	(%rax,%r13), %r15
	movq	%rbx, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
.LBB2_19:                               # %for.inc.us.3
                                        #   in Loop: Header=BB2_14 Depth=1
	incq	%r12
	movslq	even_sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %r13
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rax, %rdx
	jl	.LBB2_14
	jmp	.LBB2_20
.LBB2_6:                                # %for.cond7.preheader.preheader
	movq	(%rsp), %r14            # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	.align	16, 0x90
.LBB2_7:                                # %for.cond7.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_9
# BB#8:                                 # %for.inc
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	gen_pt(%rip), %rax
	movq	(%rax,%r13), %r15
	leaq	-432(%rbx), %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	cmpq	$1, %r14
	je	.LBB2_10
.LBB2_9:                                # %for.inc.1
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	gen_pt+8(%rip), %rax
	movq	(%rax,%r13), %r15
	leaq	-288(%rbx), %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	cmpq	$2, %r14
	je	.LBB2_11
.LBB2_10:                               # %for.inc.2
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	gen_pt+16(%rip), %rax
	movq	(%rax,%r13), %r15
	leaq	-144(%rbx), %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	cmpq	$3, %r14
	je	.LBB2_12
.LBB2_11:                               # %if.then.3
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	gen_pt+24(%rip), %rax
	movq	(%rax,%r13), %r15
	movq	%rbx, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	movq	%r15, %rdi
	callq	trace_su3
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
.LBB2_12:                               # %for.inc.3
                                        #   in Loop: Header=BB2_7 Depth=1
	incq	%r12
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %r13
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rax, %rdx
	jl	.LBB2_7
.LBB2_20:                               # %for.cond24.preheader
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, (%rsp)            # 8-byte Folded Spill
	movabsq	$0, %rbx
	movabsq	$1, %rax
	movq	%rbx, %rdx
	je	.LBB2_22
# BB#21:                                # %for.cond24.preheader
	movq	%rax, %rdx
.LBB2_22:                               # %for.cond24.preheader
	cmpq	$1, (%rsp)              # 8-byte Folded Reload
	movq	%rbx, %rcx
	je	.LBB2_24
# BB#23:                                # %for.cond24.preheader
	movq	%rax, %rcx
.LBB2_24:                               # %for.cond24.preheader
	addq	%rdx, %rcx
	cmpq	$2, (%rsp)              # 8-byte Folded Reload
	movq	%rbx, %rdx
	je	.LBB2_26
# BB#25:                                # %for.cond24.preheader
	movq	%rax, %rdx
.LBB2_26:                               # %for.cond24.preheader
	addq	%rcx, %rdx
	cmpq	$3, (%rsp)              # 8-byte Folded Reload
	je	.LBB2_28
# BB#27:                                # %for.cond24.preheader
	movq	%rax, %rbx
.LBB2_28:                               # %for.cond24.preheader
	addq	%rdx, %rbx
	leaq	8(%rsp), %rdi
	callq	g_doublesum
	movl	nx(%rip), %eax
	imulq	%rbx, %rax
	movl	ny(%rip), %ecx
	imulq	%rax, %rcx
	movl	nz(%rip), %eax
	movl	nt(%rip), %edx
	imulq	%rcx, %rax
	movq	8(%rsp), %rbx
	imulq	%rax, %rdx
	addq	%rdx, %rdx
	leaq	(%rdx,%rdx,2), %rax
	movabsq	$4294967294, %rdi       # imm = 0xFFFFFFFE
	andq	%rax, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	get_gauge_fix_action, .Ltmp53-get_gauge_fix_action
	.cfi_endproc

	.globl	gaugefixstep
	.align	16, 0x90
	.type	gaugefixstep,@function
gaugefixstep:                           # @gaugefixstep
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
	subq	$192, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 240
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
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	%r8, 96(%rsp)           # 8-byte Spill
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	$0, (%rsi)
	xorq	%rax, %rax
	callq	g_sync
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$1, %rcx
	.align	16, 0x90
.LBB3_1:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_2 Depth 2
                                        #       Child Loop BB3_8 Depth 3
                                        #       Child Loop BB3_24 Depth 3
                                        #       Child Loop BB3_26 Depth 3
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	gen_pt(%rip), %r8
	movq	$112, %rdi
	movq	$144, %rsi
	movq	$7, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	start_gather
	movq	gen_pt+8(%rip), %r8
	movq	%rax, %r14
	movq	%r14, 128(%rsp)
	movq	$256, %rdi              # imm = 0x100
	movq	$144, %rsi
	movq	$6, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	start_gather
	movq	gen_pt+16(%rip), %r8
	movq	%rax, %r13
	movq	%r13, 136(%rsp)
	movq	$400, %rdi              # imm = 0x190
	movq	$144, %rsi
	movq	$5, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	start_gather
	movq	gen_pt+24(%rip), %r8
	movq	%rax, %r12
	movq	%r12, 144(%rsp)
	movq	$544, %rdi              # imm = 0x220
	movq	$144, %rsi
	movq	$4, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	start_gather
	movq	%rax, %rbx
	movq	%rbx, 152(%rsp)
	movq	%r14, %rdi
	callq	wait_gather
	movq	%r13, %rdi
	callq	wait_gather
	movq	%r12, %rdi
	callq	wait_gather
	movq	%rbx, %rdi
	callq	wait_gather
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	get_gauge_fix_action
	movq	256(%rsp), %rax
	movq	%rax, %r13
	movq	%r13, 32(%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	80(%rsp), %r15          # 8-byte Reload
	movq	%r15, %r8
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %r9
	callq	do_hit
	movq	%r13, 32(%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %r8
	movq	%r14, %r9
	callq	do_hit
	movq	%r13, 32(%rsp)
	movq	248(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$2, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %r8
	movq	%r14, %r9
	callq	do_hit
	movq	%r12, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	get_gauge_fix_action
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rax, (%rbx)
	movq	%rcx, %rax
	xorq	$3, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
	.align	16, 0x90
.LBB3_2:                                # %for.body25
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_8 Depth 3
                                        #       Child Loop BB3_24 Depth 3
                                        #       Child Loop BB3_26 Depth 3
	movq	%rcx, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	xorq	%rax, %rax
	callq	g_sync
	movl	even_sites_on_node(%rip), %edx
	cmpq	$1, %r13
	movq	%rdx, %rax
	je	.LBB3_4
# BB#3:                                 # %for.body25
                                        #   in Loop: Header=BB3_2 Depth=2
	xorq	%rax, %rax
.LBB3_4:                                # %for.body25
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$2, %r13
	je	.LBB3_6
# BB#5:                                 # %for.body25
                                        #   in Loop: Header=BB3_2 Depth=2
	movl	sites_on_node(%rip), %edx
.LBB3_6:                                # %for.body25
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	jge	.LBB3_14
# BB#7:                                 # %for.body39.lr.ph
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	lattice(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$3200, %rax, %r14       # imm = 0xC80
	addq	%rdx, %r14
	movq	$4, %rcx
	leaq	1(%rax), %rbx
	shlq	%cl, %rax
	leaq	(%rax,%rax,8), %r15
	.align	16, 0x90
.LBB3_8:                                # %for.body39
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	gen_pt(,%r12,8), %rax
	movslq	diffmat_offset(%rip), %rsi
	movq	-8(%rax,%rbx,8), %rdi
	testq	%rsi, %rsi
	js	.LBB3_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB3_8 Depth=3
	addq	%r14, %rsi
	jmp	.LBB3_11
	.align	16, 0x90
.LBB3_10:                               # %if.else
                                        #   in Loop: Header=BB3_8 Depth=3
	movq	diffmatp(%rip), %rsi
	addq	%r15, %rsi
.LBB3_11:                               # %for.inc52
                                        #   in Loop: Header=BB3_8 Depth=3
	callq	su3mat_copy
	cmpq	$2, %r13
	movabsq	$even_sites_on_node, %rax
	je	.LBB3_13
# BB#12:                                # %for.inc52
                                        #   in Loop: Header=BB3_8 Depth=3
	movabsq	$sites_on_node, %rax
.LBB3_13:                               # %for.inc52
                                        #   in Loop: Header=BB3_8 Depth=3
	addq	$3200, %r14             # imm = 0xC80
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$144, %r15
	cmpq	%rax, %rdx
	jl	.LBB3_8
.LBB3_14:                               # %for.end54
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	128(%rsp,%r12,8), %rdi
	callq	cleanup_gather
	xorq	%rax, %rax
	callq	g_sync
	movslq	diffmat_offset(%rip), %rdi
	testq	%rdi, %rdi
	js	.LBB3_16
# BB#15:                                # %if.then59
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	gen_pt(,%r12,8), %r8
	movq	$144, %rsi
	movq	%r12, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	start_gather
	jmp	.LBB3_17
	.align	16, 0x90
.LBB3_16:                               # %if.else65
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	diffmatp(%rip), %rdi
	movq	gen_pt(,%r12,8), %r8
	movq	$144, %rsi
	movq	%r12, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	start_gather_from_temp
.LBB3_17:                               # %if.end72
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	%rax, %r14
	movq	%r14, 128(%rsp,%r12,8)
	movq	%r14, %rdi
	callq	wait_gather
	movl	even_sites_on_node(%rip), %eax
	cmpq	$2, %r13
	movq	%rax, %r15
	je	.LBB3_19
# BB#18:                                # %if.end72
                                        #   in Loop: Header=BB3_2 Depth=2
	xorq	%r15, %r15
.LBB3_19:                               # %if.end72
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %r13
	je	.LBB3_21
# BB#20:                                # %if.end72
                                        #   in Loop: Header=BB3_2 Depth=2
	movl	sites_on_node(%rip), %eax
.LBB3_21:                               # %if.end72
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB3_27
# BB#22:                                # %for.body94.lr.ph
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	(%r12,%r12,8), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	imulq	$3200, %r15, %rcx       # imm = 0xC80
	incq	%r15
	addq	%rcx, %rax
	cmpq	$1, %r13
	jne	.LBB3_23
# BB#25:                                # %for.body94.us.preheader
                                        #   in Loop: Header=BB3_2 Depth=2
	leaq	112(%rdx,%rax), %rbx
	.align	16, 0x90
.LBB3_26:                               # %for.body94.us
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	gen_pt(,%r12,8), %rax
	movq	-8(%rax,%r15,8), %rdi
	movq	%rbx, %rsi
	callq	su3mat_copy
	movslq	even_sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rax, %rdx
	jl	.LBB3_26
	jmp	.LBB3_27
	.align	16, 0x90
.LBB3_23:                               # %for.body94.preheader
                                        #   in Loop: Header=BB3_2 Depth=2
	leaq	112(%rdx,%rax), %r13
	.align	16, 0x90
.LBB3_24:                               # %for.body94
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	gen_pt(,%r12,8), %rax
	movq	-8(%rax,%r15,8), %rdi
	movq	%r13, %rsi
	callq	su3mat_copy
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rax, %rdx
	jl	.LBB3_24
.LBB3_27:                               # %for.end105
                                        #   in Loop: Header=BB3_2 Depth=2
	movq	%r14, %rdi
	callq	cleanup_gather
	incq	%r12
	cmpq	$4, %r12
	movq	120(%rsp), %rcx         # 8-byte Reload
	jne	.LBB3_2
# BB#28:                                # %for.inc111
                                        #   in Loop: Header=BB3_1 Depth=1
	incq	%rcx
	movq	%rcx, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$3, %rax
	jne	.LBB3_1
# BB#29:                                # %for.end113
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	gaugefixstep, .Ltmp71-gaugefixstep
	.cfi_endproc

	.globl	gaugefixscratch
	.align	16, 0x90
	.type	gaugefixscratch,@function
gaugefixscratch:                        # @gaugefixscratch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp74:
	.cfi_def_cfa_offset 16
.Ltmp75:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movl	%edi, diffmat_offset(%rip)
	movq	$0, diffmatp(%rip)
	jns	.LBB4_5
# BB#1:                                 # %if.then
	movslq	sites_on_node(%rip), %rdi
	movq	$144, %rsi
	callq	calloc
	testq	%rax, %rax
	movq	%rax, diffmatp(%rip)
	jne	.LBB4_5
# BB#2:                                 # %if.then3
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_4
# BB#3:                                 # %if.then6
	movabsq	$.Lstr3, %rdi
	callq	puts
.LBB4_4:                                # %if.end
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$1, %rdi
	callq	terminate
.LBB4_5:                                # %if.end10
	movl	%ebx, sumvec_offset(%rip)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	$0, sumvecp(%rip)
	jns	.LBB4_10
# BB#6:                                 # %if.then13
	movslq	sites_on_node(%rip), %rdi
	movq	$48, %rsi
	callq	calloc
	testq	%rax, %rax
	movq	%rax, sumvecp(%rip)
	je	.LBB4_7
.LBB4_10:                               # %if.end26
	popq	%rbx
	retq
.LBB4_7:                                # %if.then18
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_9
# BB#8:                                 # %if.then21
	movabsq	$.Lstr, %rdi
	callq	puts
.LBB4_9:                                # %if.end23
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$1, %rdi
	popq	%rbx
	jmp	terminate  # TAILCALL
.Ltmp76:
	.size	gaugefixscratch, .Ltmp76-gaugefixscratch
	.cfi_endproc

	.globl	gaugefix
	.align	16, 0x90
	.type	gaugefix,@function
gaugefix:                               # @gaugefix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp83:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp84:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp85:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp86:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp88:
	.cfi_def_cfa_offset 112
.Ltmp89:
	.cfi_offset %rbx, -48
.Ltmp90:
	.cfi_offset %r12, -40
.Ltmp91:
	.cfi_offset %r13, -32
.Ltmp92:
	.cfi_offset %r14, -24
.Ltmp93:
	.cfi_offset %r15, -16
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r8, %rdi
	movq	%r9, %rsi
	callq	gaugefixscratch
	xorq	%rbx, %rbx
	testq	%r14, %r14
	jle	.LBB5_8
	.align	16, 0x90
.LBB5_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %r12
	movq	152(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	136(%rsp), %rax
	movq	%rax, (%rsp)
	movq	40(%rsp), %rdi          # 8-byte Reload
	leaq	56(%rsp), %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	112(%rsp), %rcx
	movq	120(%rsp), %r8
	movq	128(%rsp), %r9
	callq	gaugefixstep
	movq	56(%rsp), %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB5_5
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %r14
	movq	%r14, %rdi
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	andq	%rax, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB5_6
# BB#3:                                 # %if.end4
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rax
	movabsq	$7378697629483820647, %rcx # imm = 0x6666666666666667
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$3, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movq	$2, %rcx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	subq	%rax, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$19, %rsi
	jne	.LBB5_5
# BB#4:                                 # %if.then6
                                        #   in Loop: Header=BB5_1 Depth=1
	callq	reunitarize
.LBB5_5:                                # %for.inc
                                        #   in Loop: Header=BB5_1 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB5_1
.LBB5_6:                                # %for.end
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rax
	movabsq	$7378697629483820647, %rcx # imm = 0x6666666666666667
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$3, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	movq	$2, %rcx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	subq	%rax, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB5_8
# BB#7:                                 # %if.then10
	callq	reunitarize
.LBB5_8:                                # %if.end11
	movslq	diffmat_offset(%rip), %rax
	testq	%rax, %rax
	jns	.LBB5_10
# BB#9:                                 # %if.then13
	movq	diffmatp(%rip), %rdi
	callq	free
.LBB5_10:                               # %if.end14
	movslq	sumvec_offset(%rip), %rax
	testq	%rax, %rax
	jns	.LBB5_12
# BB#11:                                # %if.then16
	movq	sumvecp(%rip), %rdi
	callq	free
.LBB5_12:                               # %if.end17
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_14
# BB#13:                                # %if.end21
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_14:                               # %if.then19
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	printf  # TAILCALL
.Ltmp94:
	.size	gaugefix, .Ltmp94-gaugefix
	.cfi_endproc

	.type	diffmat_offset,@object  # @diffmat_offset
	.comm	diffmat_offset,4,4
	.type	diffmatp,@object        # @diffmatp
	.comm	diffmatp,8,8
	.type	sumvec_offset,@object   # @sumvec_offset
	.comm	sumvec_offset,4,4
	.type	sumvecp,@object         # @sumvecp
	.comm	sumvecp,8,8
	.type	.L.str2,@object         # @.str2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str2:
	.asciz	"GFIX: Ended at step %d. Av gf action %.8e, delta %.3e\n"
	.size	.L.str2, 55

	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"gaugefix: Can't malloc sumvec"
	.size	.Lstr, 30

	.type	.Lstr3,@object          # @str3
	.align	16
.Lstr3:
	.asciz	"gaugefix: Can't malloc diffmat"
	.size	.Lstr3, 31


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
