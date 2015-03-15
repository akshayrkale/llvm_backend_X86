	.file	"dependence.c.bc"
	.text
	.globl	init_dependence_analysis
	.align	16, 0x90
	.type	init_dependence_analysis,@function
init_dependence_analysis:               # @init_dependence_analysis
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
	subq	$2896, %rsp             # imm = 0xB50
.Ltmp11:
	.cfi_def_cfa_offset 2944
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
	movq	%rdi, %rbx
	movabsq	$.L.str, %rdx
	movq	$50, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, def_use_chain(%rip)
	movabsq	$.L.str1, %rdx
	movq	$50, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, dep_chain(%rip)
	movabsq	$.L.str2, %rdx
	movq	$50, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, loop_chain(%rip)
	movabsq	$.L.str3, %rdx
	movq	$50, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, induction_chain(%rip)
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	build_def_use
	movq	loop_chain(%rip), %rax
	movq	40(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB0_8
# BB#1:                                 # %land.rhs.lr.ph.i
	movq	32(%rax), %rdi
	movq	$1, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_2:                                # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rbx
	movq	%rdx, %rcx
	andq	%r8, %rcx
	cmpq	%rbx, %rcx
	jae	.LBB0_8
# BB#3:                                 # %for.body.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	(%rdi), %rcx
	movq	(%rsi), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB0_4
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	16(%rsi), %ecx
	movl	16(%rdi), %ebx
	decq	%rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rbx
	movq	%rsi, %rbx
	jne	.LBB0_7
# BB#6:                                 # %if.then12.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%rsi, 24(%rdi)
	movq	loop_chain(%rip), %rax
	movq	%rsi, %rbx
	jmp	.LBB0_7
	.align	16, 0x90
.LBB0_4:                                #   in Loop: Header=BB0_2 Depth=1
	movq	%rdi, %rbx
.LBB0_7:                                # %for.inc.i
                                        #   in Loop: Header=BB0_2 Depth=1
	incq	%rdx
	movq	%rdx, %rcx
	andq	%r8, %rcx
	movq	32(%rax,%rcx,8), %rsi
	testq	%rsi, %rsi
	movq	%rbx, %rdi
	jne	.LBB0_2
.LBB0_8:                                # %link_loops.exit
	movq	def_use_chain(%rip), %rdi
	movq	32(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB0_210
# BB#9:                                 # %land.rhs.lr.ph.i24
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%rsi, %rsi
.LBB0_10:                               # %land.rhs.i28
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_13 Depth 2
                                        #       Child Loop BB0_15 Depth 3
                                        #       Child Loop BB0_18 Depth 3
                                        #       Child Loop BB0_26 Depth 3
                                        #       Child Loop BB0_30 Depth 3
                                        #       Child Loop BB0_40 Depth 3
                                        #         Child Loop BB0_41 Depth 4
                                        #           Child Loop BB0_46 Depth 5
                                        #       Child Loop BB0_54 Depth 3
                                        #         Child Loop BB0_57 Depth 4
                                        #         Child Loop BB0_60 Depth 4
                                        #         Child Loop BB0_68 Depth 4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_74 Depth 6
                                        #         Child Loop BB0_84 Depth 4
                                        #         Child Loop BB0_91 Depth 4
                                        #         Child Loop BB0_105 Depth 4
                                        #           Child Loop BB0_106 Depth 5
                                        #         Child Loop BB0_119 Depth 4
                                        #           Child Loop BB0_121 Depth 5
                                        #             Child Loop BB0_128 Depth 6
                                        #             Child Loop BB0_148 Depth 6
                                        #             Child Loop BB0_158 Depth 6
                                        #         Child Loop BB0_171 Depth 4
                                        #           Child Loop BB0_172 Depth 5
                                        #         Child Loop BB0_170 Depth 4
                                        #         Child Loop BB0_177 Depth 4
                                        #         Child Loop BB0_191 Depth 4
                                        #         Child Loop BB0_195 Depth 4
	movq	%rsi, 208(%rsp)         # 8-byte Spill
	movq	(%rdi), %rax
	movq	%rsi, %rdx
	andq	%r13, %rdx
	movq	%rdx, 192(%rsp)         # 8-byte Spill
	cmpq	%rax, %rdx
	jae	.LBB0_209
# BB#11:                                # %for.body.i29
                                        #   in Loop: Header=BB0_10 Depth=1
	movl	36(%rcx), %eax
	cmpq	$1, %rax
	jne	.LBB0_207
# BB#12:                                # %if.then.i30
                                        #   in Loop: Header=BB0_10 Depth=1
	testq	%rcx, %rcx
	movq	$7, %rdx
	movl	%edx, 388(%rsp)
	movabsq	$0, %rax
	movl	%eax, 1076(%rsp)
	movl	%edx, 440(%rsp)
	movl	%eax, 1128(%rsp)
	movl	%edx, 492(%rsp)
	movl	%eax, 1180(%rsp)
	movl	%edx, 544(%rsp)
	movl	%eax, 1232(%rsp)
	movl	%edx, 596(%rsp)
	movl	%eax, 1284(%rsp)
	movl	%edx, 648(%rsp)
	movl	%eax, 1336(%rsp)
	movl	%edx, 700(%rsp)
	movl	%eax, 1388(%rsp)
	movl	%edx, 752(%rsp)
	movl	%eax, 1440(%rsp)
	movl	%edx, 804(%rsp)
	movl	%eax, 1492(%rsp)
	movl	%edx, 856(%rsp)
	movl	%eax, 1544(%rsp)
	movl	%edx, 908(%rsp)
	movl	%eax, 1596(%rsp)
	movl	%edx, 960(%rsp)
	movl	%eax, 1648(%rsp)
	movq	%rcx, %r12
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	je	.LBB0_207
	.align	16, 0x90
.LBB0_13:                               # %for.body6.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_15 Depth 3
                                        #       Child Loop BB0_18 Depth 3
                                        #       Child Loop BB0_26 Depth 3
                                        #       Child Loop BB0_30 Depth 3
                                        #       Child Loop BB0_40 Depth 3
                                        #         Child Loop BB0_41 Depth 4
                                        #           Child Loop BB0_46 Depth 5
                                        #       Child Loop BB0_54 Depth 3
                                        #         Child Loop BB0_57 Depth 4
                                        #         Child Loop BB0_60 Depth 4
                                        #         Child Loop BB0_68 Depth 4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_74 Depth 6
                                        #         Child Loop BB0_84 Depth 4
                                        #         Child Loop BB0_91 Depth 4
                                        #         Child Loop BB0_105 Depth 4
                                        #           Child Loop BB0_106 Depth 5
                                        #         Child Loop BB0_119 Depth 4
                                        #           Child Loop BB0_121 Depth 5
                                        #             Child Loop BB0_128 Depth 6
                                        #             Child Loop BB0_148 Depth 6
                                        #             Child Loop BB0_158 Depth 6
                                        #         Child Loop BB0_171 Depth 4
                                        #           Child Loop BB0_172 Depth 5
                                        #         Child Loop BB0_170 Depth 4
                                        #         Child Loop BB0_177 Depth 4
                                        #         Child Loop BB0_191 Depth 4
                                        #         Child Loop BB0_195 Depth 4
	movq	%r12, 216(%rsp)         # 8-byte Spill
	movl	32(%r12), %eax
	testq	%rax, %rax
	jne	.LBB0_205
# BB#14:                                # %if.end.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	16(%r12), %rax
	movzbl	16(%rax), %ecx
	xorq	%r15, %r15
	jmp	.LBB0_15
	.align	16, 0x90
.LBB0_218:                              # %for.body.i.i.i
                                        #   in Loop: Header=BB0_15 Depth=3
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	incq	%r15
.LBB0_15:                               # %if.end.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	$43, %rcx
	je	.LBB0_218
# BB#16:                                # %for.cond1.preheader.i.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	xorq	%rcx, %rcx
	movl	%ecx, 1712(%rsp)
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	movl	%edx, 1716(%rsp)
	movl	%edx, 1720(%rsp)
	leaq	1728(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1748(%rsp)
	movl	%edx, 1752(%rsp)
	leaq	1760(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1780(%rsp)
	movl	%edx, 1784(%rsp)
	leaq	1792(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1812(%rsp)
	movl	%edx, 1816(%rsp)
	leaq	1824(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1844(%rsp)
	movl	%edx, 1848(%rsp)
	leaq	1856(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1876(%rsp)
	movl	%edx, 1880(%rsp)
	leaq	1888(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1908(%rsp)
	movl	%edx, 1912(%rsp)
	leaq	1920(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1940(%rsp)
	movl	%edx, 1944(%rsp)
	leaq	1952(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 1972(%rsp)
	movl	%edx, 1976(%rsp)
	leaq	1984(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2004(%rsp)
	movl	%edx, 2008(%rsp)
	leaq	2016(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2036(%rsp)
	movl	%edx, 2040(%rsp)
	leaq	2048(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2068(%rsp)
	movl	%edx, 2072(%rsp)
	leaq	2080(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2100(%rsp)
	movl	%edx, 2104(%rsp)
	leaq	2112(%rsp), %rax
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	16(%r12), %r14
	movzbl	16(%r14), %eax
	cmpq	$43, %rax
	jne	.LBB0_22
# BB#17:                                # %for.body22.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	leaq	1712(%rsp,%rax), %rbx
	.align	16, 0x90
.LBB0_18:                               # %for.body22.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	40(%r14), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB0_20
# BB#19:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB0_18 Depth=3
	movq	32(%rdi), %rax
	movl	%eax, 8(%rbx)
	jmp	.LBB0_21
	.align	16, 0x90
.LBB0_20:                               # %if.else.i.i.i
                                        #   in Loop: Header=BB0_18 Depth=3
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	get_one_coefficient
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_205
.LBB0_21:                               # %if.end46.i.i.i
                                        #   in Loop: Header=BB0_18 Depth=3
	movq	32(%r14), %r14
	movzbl	16(%r14), %eax
	addq	$-32, %rbx
	cmpq	$43, %rax
	je	.LBB0_18
.LBB0_22:                               # %get_coefficients.exit.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_205
# BB#23:                                # %if.end10.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	loop_chain(%rip), %r9
	movq	32(%r9), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	jne	.LBB0_25
# BB#24:                                #   in Loop: Header=BB0_13 Depth=2
	xorq	%rbx, %rbx
	jmp	.LBB0_28
.LBB0_25:                               # %land.rhs.lr.ph.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	(%r9), %rdx
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_26:                               # %land.rhs.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rsi
	andq	%r13, %rsi
	cmpq	%rdx, %rsi
	jae	.LBB0_27
# BB#43:                                # %for.body17.i.i
                                        #   in Loop: Header=BB0_26 Depth=3
	movq	(%rax), %rsi
	movq	(%r12), %rdi
	cmpq	%rdi, %rsi
	je	.LBB0_27
# BB#44:                                # %for.inc23.i.i
                                        #   in Loop: Header=BB0_26 Depth=3
	incq	%rbx
	movq	%rbx, %rsi
	andq	%r13, %rsi
	movq	32(%r9,%rsi,8), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	jne	.LBB0_26
	jmp	.LBB0_28
.LBB0_27:                               #   in Loop: Header=BB0_13 Depth=2
	movq	%rax, %rcx
.LBB0_28:                               # %for.cond30.preheader.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	movq	%rax, 264(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_37
# BB#29:                                # %land.rhs32.lr.ph.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	(%r9), %r10
	xorq	%r8, %r8
	movq	264(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdi
	.align	16, 0x90
.LBB0_30:                               # %land.rhs32.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rcx
	andq	%r13, %rcx
	cmpq	%r10, %rcx
	jae	.LBB0_36
# BB#31:                                # %for.body38.i.i
                                        #   in Loop: Header=BB0_30 Depth=3
	movq	(%rdi), %rcx
	movq	(%r12), %rdx
	movq	%r12, %rax
	cmpq	%rdx, %rcx
	jne	.LBB0_35
# BB#32:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_30 Depth=3
	movl	20(%rdi), %ecx
	cmpq	$1, %rcx
	movq	$1, %rdi
	je	.LBB0_34
# BB#33:                                # %select.mid
                                        #   in Loop: Header=BB0_30 Depth=3
	movq	%r8, %rdi
.LBB0_34:                               # %select.end
                                        #   in Loop: Header=BB0_30 Depth=3
	movq	%rdi, %r8
.LBB0_35:                               # %for.inc47.i.i
                                        #   in Loop: Header=BB0_30 Depth=3
	movq	%rax, %r12
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r13, %rcx
	movq	32(%r9,%rcx,8), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_30
.LBB0_36:                               # %for.end53.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	%rsi, 264(%rsp)         # 8-byte Spill
	testq	%r13, %r8
	jne	.LBB0_205
.LBB0_37:                               # %if.end56.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$2147483648, %r10       # imm = 0x80000000
	jle	.LBB0_53
# BB#38:                                # %if.end56.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	cmpq	$0, 200(%rsp)           # 8-byte Folded Reload
	je	.LBB0_53
# BB#39:                                # %for.cond1.preheader.lr.ph.for.cond1.preheader.lr.ph.split_crit_edge.i.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	incq	%r15
	movq	$1, %r8
	.align	16, 0x90
.LBB0_40:                               # %for.body2.lr.ph.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_41 Depth 4
                                        #           Child Loop BB0_46 Depth 5
	movq	$5, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	leaq	1728(%rsp,%rax), %rcx
	leaq	1716(%rsp,%rax), %r9
	leaq	1720(%rsp,%rax), %rsi
	movq	264(%rsp), %rdi         # 8-byte Reload
	.align	16, 0x90
.LBB0_41:                               # %for.body2.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_40 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_46 Depth 5
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB0_51
# BB#42:                                # %for.body5.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_41 Depth=4
	movq	(%rcx), %rax
	.align	16, 0x90
.LBB0_46:                               # %for.body5.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_40 Depth=3
                                        #         Parent Loop BB0_41 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	(%rbx), %rdx
	cmpq	%rdx, %rax
	je	.LBB0_47
# BB#45:                                # %for.cond3.i.i.i
                                        #   in Loop: Header=BB0_46 Depth=5
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_46
	jmp	.LBB0_51
	.align	16, 0x90
.LBB0_47:                               # %if.then.i403.i.i
                                        #   in Loop: Header=BB0_41 Depth=4
	movslq	12(%rbx), %rdx
	movslq	16(%rbx), %rax
	cmpq	%rax, %rdx
	jge	.LBB0_49
# BB#48:                                # %if.then9.i.i.i
                                        #   in Loop: Header=BB0_41 Depth=4
	movl	(%r9), %eax
	movl	(%rsi), %ebx
	imulq	%rdx, %rax
	addq	%rax, %rbx
	movl	%ebx, (%rsi)
	jmp	.LBB0_51
.LBB0_49:                               # %if.else.i407.i.i
                                        #   in Loop: Header=BB0_41 Depth=4
	movq	%rax, %rdx
	andq	%r13, %rdx
	cmpq	%r10, %rdx
	je	.LBB0_51
# BB#50:                                # %if.then17.i.i.i
                                        #   in Loop: Header=BB0_41 Depth=4
	movl	(%r9), %edx
	imulq	%rdx, %rax
	movl	%eax, (%rsi)
	negq	%rdx
	movl	%edx, (%r9)
	.align	16, 0x90
.LBB0_51:                               # %for.inc35.i.i.i
                                        #   in Loop: Header=BB0_41 Depth=4
	movq	24(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_41
# BB#52:                                # %for.inc37.i.i.i
                                        #   in Loop: Header=BB0_40 Depth=3
	incq	%r8
	movq	%r8, %rax
	andq	%r13, %rax
	movq	%r15, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_40
.LBB0_53:                               # %for.body60.lr.ph.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	176(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_54
.LBB0_193:                              # %if.then207.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$1, %rcx
	movl	%ecx, 16(%rbx)
	movl	(%rdx), %ecx
	movabsq	$0, %rsi
	movl	%esi, (%rax)
	negq	%rcx
	movl	%ecx, (%rdx)
	jmp	.LBB0_194
	.align	16, 0x90
.LBB0_54:                               # %for.body60.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_57 Depth 4
                                        #         Child Loop BB0_60 Depth 4
                                        #         Child Loop BB0_68 Depth 4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_74 Depth 6
                                        #         Child Loop BB0_84 Depth 4
                                        #         Child Loop BB0_91 Depth 4
                                        #         Child Loop BB0_105 Depth 4
                                        #           Child Loop BB0_106 Depth 5
                                        #         Child Loop BB0_119 Depth 4
                                        #           Child Loop BB0_121 Depth 5
                                        #             Child Loop BB0_128 Depth 6
                                        #             Child Loop BB0_148 Depth 6
                                        #             Child Loop BB0_158 Depth 6
                                        #         Child Loop BB0_171 Depth 4
                                        #           Child Loop BB0_172 Depth 5
                                        #         Child Loop BB0_170 Depth 4
                                        #         Child Loop BB0_177 Depth 4
                                        #         Child Loop BB0_191 Depth 4
                                        #         Child Loop BB0_195 Depth 4
	cmpq	%rcx, %r12
	je	.LBB0_204
# BB#55:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	(%r12), %rax
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	cmpq	%rcx, %rax
	movq	%rdx, %rcx
	jne	.LBB0_204
# BB#56:                                # %if.end68.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	xorq	%rdx, %rdx
	movl	%edx, 36(%r12)
	movl	%edx, 36(%rcx)
	movq	16(%rcx), %rax
	movq	%rcx, 248(%rsp)         # 8-byte Spill
	movzbl	16(%rax), %ecx
	jmp	.LBB0_57
	.align	16, 0x90
.LBB0_219:                              # %for.body.i542.i.i
                                        #   in Loop: Header=BB0_57 Depth=4
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	incq	%rdx
.LBB0_57:                               # %if.end68.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	$43, %rcx
	je	.LBB0_219
# BB#58:                                # %for.cond1.preheader.i531.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%rdx, 256(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	movl	%ecx, 2128(%rsp)
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	movl	%edx, 2132(%rsp)
	movl	%edx, 2136(%rsp)
	leaq	2144(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2164(%rsp)
	movl	%edx, 2168(%rsp)
	leaq	2176(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2196(%rsp)
	movl	%edx, 2200(%rsp)
	leaq	2208(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2228(%rsp)
	movl	%edx, 2232(%rsp)
	leaq	2240(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2260(%rsp)
	movl	%edx, 2264(%rsp)
	leaq	2272(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2292(%rsp)
	movl	%edx, 2296(%rsp)
	leaq	2304(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2324(%rsp)
	movl	%edx, 2328(%rsp)
	leaq	2336(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2356(%rsp)
	movl	%edx, 2360(%rsp)
	leaq	2368(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2388(%rsp)
	movl	%edx, 2392(%rsp)
	leaq	2400(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2420(%rsp)
	movl	%edx, 2424(%rsp)
	leaq	2432(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2452(%rsp)
	movl	%edx, 2456(%rsp)
	leaq	2464(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2484(%rsp)
	movl	%edx, 2488(%rsp)
	leaq	2496(%rsp), %rax
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 2516(%rsp)
	movl	%edx, 2520(%rsp)
	leaq	2528(%rsp), %rax
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %r14
	movzbl	16(%r14), %eax
	cmpq	$43, %rax
	movabsq	$0, %rbx
	jne	.LBB0_63
# BB#59:                                # %for.body22.lr.ph.i549.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$32, %rcx
	movq	256(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	leaq	2128(%rsp,%rax), %r15
	.align	16, 0x90
.LBB0_60:                               # %for.body22.i566.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	40(%r14), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB0_71
# BB#61:                                # %if.then.i571.i.i
                                        #   in Loop: Header=BB0_60 Depth=4
	movq	32(%rdi), %rax
	movl	%eax, 8(%r15)
	jmp	.LBB0_62
	.align	16, 0x90
.LBB0_71:                               # %if.else.i575.i.i
                                        #   in Loop: Header=BB0_60 Depth=4
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	248(%rsp), %rdx         # 8-byte Reload
	callq	get_one_coefficient
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rcx
	js	.LBB0_72
.LBB0_62:                               # %if.end46.i583.i.i
                                        #   in Loop: Header=BB0_60 Depth=4
	movq	32(%r14), %r14
	movzbl	16(%r14), %eax
	addq	$-32, %r15
	cmpq	$43, %rax
	je	.LBB0_60
.LBB0_63:                               # %get_coefficients.exit585.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$32, %rcx
	movq	256(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %rcx
	movabsq	$1, %r14
	jg	.LBB0_65
# BB#64:                                # %get_coefficients.exit585.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%r14, %rcx
.LBB0_65:                               # %get_coefficients.exit585.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movabsq	$2147483648, %r11       # imm = 0x80000000
	movq	$5, %r8
	jle	.LBB0_82
# BB#66:                                # %get_coefficients.exit585.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	cmpq	$0, 200(%rsp)           # 8-byte Folded Reload
	je	.LBB0_82
# BB#67:                                # %for.cond1.preheader.lr.ph.for.cond1.preheader.lr.ph.split_crit_edge.i478.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%r14, %r15
	movq	%rbx, %r14
	movq	256(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %r8
	movq	$1, %r9
	.align	16, 0x90
.LBB0_68:                               # %for.body2.lr.ph.i485.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_69 Depth 5
                                        #             Child Loop BB0_74 Depth 6
	movq	$5, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	leaq	2144(%rsp,%rax), %rcx
	leaq	2132(%rsp,%rax), %r10
	leaq	2136(%rsp,%rax), %rdi
	movq	264(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB0_69:                               # %for.body2.i490.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB0_74 Depth 6
	movq	32(%rbx), %rax
	testq	%rax, %rax
	je	.LBB0_79
# BB#70:                                # %for.body5.lr.ph.i491.i.i
                                        #   in Loop: Header=BB0_69 Depth=5
	movq	(%rcx), %rdx
	.align	16, 0x90
.LBB0_74:                               # %for.body5.i499.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_68 Depth=4
                                        #           Parent Loop BB0_69 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movq	(%rax), %rsi
	cmpq	%rsi, %rdx
	je	.LBB0_75
# BB#73:                                # %for.cond3.i495.i.i
                                        #   in Loop: Header=BB0_74 Depth=6
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB0_74
	jmp	.LBB0_79
	.align	16, 0x90
.LBB0_75:                               # %if.then.i503.i.i
                                        #   in Loop: Header=BB0_69 Depth=5
	movslq	12(%rax), %rdx
	movslq	16(%rax), %rax
	cmpq	%rax, %rdx
	jge	.LBB0_77
# BB#76:                                # %if.then9.i508.i.i
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	(%r10), %eax
	movl	(%rdi), %esi
	imulq	%rdx, %rax
	addq	%rax, %rsi
	movl	%esi, (%rdi)
	jmp	.LBB0_79
.LBB0_77:                               # %if.else.i510.i.i
                                        #   in Loop: Header=BB0_69 Depth=5
	movq	%rax, %rdx
	andq	%r13, %rdx
	cmpq	%r11, %rdx
	je	.LBB0_79
# BB#78:                                # %if.then17.i515.i.i
                                        #   in Loop: Header=BB0_69 Depth=5
	movl	(%r10), %edx
	imulq	%rdx, %rax
	movl	%eax, (%rdi)
	negq	%rdx
	movl	%edx, (%r10)
	.align	16, 0x90
.LBB0_79:                               # %for.inc35.i518.i.i
                                        #   in Loop: Header=BB0_69 Depth=5
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_69
# BB#80:                                # %for.inc37.i522.i.i
                                        #   in Loop: Header=BB0_68 Depth=4
	incq	%r9
	movq	%r9, %rax
	andq	%r13, %rax
	movq	%r8, %rdx
	andq	%r13, %rdx
	xorq	%rcx, %rcx
	cmpq	%rdx, %rax
	jne	.LBB0_68
# BB#81:                                #   in Loop: Header=BB0_54 Depth=3
	movq	$5, %r8
	movq	%r14, %rbx
	movq	%r15, %r14
	jmp	.LBB0_82
.LBB0_72:                               #   in Loop: Header=BB0_54 Depth=3
	movq	%r13, 256(%rsp)         # 8-byte Spill
	movq	$5, %r8
	movabsq	$1, %r14
	.align	16, 0x90
.LBB0_82:                               # %normalize_coefficients.exit523.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$0, 2880(%rsp)
	movq	$0, 2872(%rsp)
	movq	$0, 2864(%rsp)
	movq	$0, 2856(%rsp)
	movq	$0, 2848(%rsp)
	movq	$0, 2840(%rsp)
	movq	$0, 2832(%rsp)
	movq	$0, 2824(%rsp)
	movq	$0, 2816(%rsp)
	movq	$0, 2808(%rsp)
	movq	$0, 2800(%rsp)
	movq	$0, 2792(%rsp)
	movq	$0, 2784(%rsp)
	movq	$0, 2768(%rsp)
	movq	$0, 2760(%rsp)
	movq	$0, 2752(%rsp)
	movq	$0, 2744(%rsp)
	movq	$0, 2736(%rsp)
	movq	$0, 2728(%rsp)
	movq	$0, 2720(%rsp)
	movq	$0, 2712(%rsp)
	movq	$0, 2704(%rsp)
	movq	$0, 2696(%rsp)
	movq	$0, 2688(%rsp)
	movq	$0, 2680(%rsp)
	movq	$0, 2672(%rsp)
	movabsq	$0, %rax
	movl	%eax, 2592(%rsp)
	movq	$0, 2584(%rsp)
	movq	$0, 2576(%rsp)
	movq	$0, 2568(%rsp)
	movq	$0, 2560(%rsp)
	movq	$0, 2552(%rsp)
	movq	$0, 2544(%rsp)
	movl	%eax, 2656(%rsp)
	movq	$0, 2648(%rsp)
	movq	$0, 2640(%rsp)
	movq	$0, 2632(%rsp)
	movq	$0, 2624(%rsp)
	movq	$0, 2616(%rsp)
	movq	$0, 2608(%rsp)
	movq	$1, %r10
	testq	%rcx, %rcx
	jne	.LBB0_117
# BB#83:                                # %for.body.lr.ph.i459.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%rbx, %r9
	movq	256(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %rdx
	movq	$2, %rbx
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_84:                               # %for.body.i461.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	2176(%rsp,%rdi,8), %rax
	testq	%rax, %rax
	je	.LBB0_87
# BB#85:                                # %if.then.i463.i.i
                                        #   in Loop: Header=BB0_84 Depth=4
	cmpq	$0, 2792(%rsp,%rdi,2)
	jne	.LBB0_87
# BB#86:                                # %if.then4.i.i.i
                                        #   in Loop: Header=BB0_84 Depth=4
	movl	2164(%rsp,%rdi,8), %ecx
	movq	%rax, 2792(%rsp,%rdi,2)
	movl	%ecx, 2548(%rsp,%rdi)
.LBB0_87:                               # %if.end14.i.i.i
                                        #   in Loop: Header=BB0_84 Depth=4
	movq	1760(%rsp,%rdi,8), %rax
	testq	%rax, %rax
	je	.LBB0_90
# BB#88:                                # %if.then19.i.i.i
                                        #   in Loop: Header=BB0_84 Depth=4
	cmpq	$0, 2680(%rsp,%rdi,2)
	jne	.LBB0_90
# BB#89:                                # %if.then23.i.i.i
                                        #   in Loop: Header=BB0_84 Depth=4
	movl	1748(%rsp,%rdi,8), %ecx
	movq	%rax, 2680(%rsp,%rdi,2)
	movl	%ecx, 2612(%rsp,%rdi)
.LBB0_90:                               # %for.inc.i.i.i
                                        #   in Loop: Header=BB0_84 Depth=4
	movq	%rbx, %rsi
	andq	%r13, %rsi
	movq	%rdx, %rax
	andq	%r13, %rax
	incq	%rbx
	addq	$4, %rdi
	xorq	%rcx, %rcx
	cmpq	%rax, %rsi
	jne	.LBB0_84
	.align	16, 0x90
.LBB0_91:                               # %for.body38.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	2792(%rsp,%rcx,8), %rsi
	movq	2680(%rsp,%rcx,8), %rdi
	movq	%rsi, %rdx
	orq	%rdi, %rdx
	jne	.LBB0_93
# BB#92:                                # %if.then45.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movabsq	$0, %rdx
	movl	%edx, 276(%rsp,%rcx,4)
	jmp	.LBB0_103
	.align	16, 0x90
.LBB0_93:                               # %if.else.i467.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movl	2548(%rsp,%rcx,4), %edx
	cmpq	%rdi, %rsi
	jne	.LBB0_99
# BB#94:                                # %if.then53.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movl	2612(%rsp,%rcx,4), %esi
	movq	%rdx, %rdi
	andq	%r13, %rdi
	cmpq	%rsi, %rdi
	jne	.LBB0_96
# BB#95:                                # %if.then59.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movq	$1, %rdx
	movl	%edx, 276(%rsp,%rcx,4)
	jmp	.LBB0_103
	.align	16, 0x90
.LBB0_99:                               # %if.else76.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	testq	%r13, %rdx
	je	.LBB0_101
# BB#100:                               # %lor.lhs.false.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movl	2612(%rsp,%rcx,4), %edx
	testq	%rdx, %rdx
	jne	.LBB0_102
.LBB0_101:                              # %if.then83.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movq	$3, %rdx
	movl	%edx, 276(%rsp,%rcx,4)
	jmp	.LBB0_103
.LBB0_96:                               # %if.else62.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	addq	%rsi, %rdx
	testq	%r13, %rdx
	jne	.LBB0_98
# BB#97:                                # %if.then68.i470.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movq	$4, %rdx
	movl	%edx, 276(%rsp,%rcx,4)
	jmp	.LBB0_103
.LBB0_102:                              # %if.else86.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movl	%r8d, 276(%rsp,%rcx,4)
	jmp	.LBB0_103
.LBB0_98:                               # %if.else71.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	movq	$2, %rdx
	movl	%edx, 276(%rsp,%rcx,4)
	.align	16, 0x90
.LBB0_103:                              # %for.inc92.i.i.i
                                        #   in Loop: Header=BB0_91 Depth=4
	leaq	2(%rcx), %rdx
	andq	%r13, %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	movq	$1, %r10
	jne	.LBB0_91
# BB#104:                               #   in Loop: Header=BB0_54 Depth=3
	movq	$1, %rcx
	movq	%r9, %r8
	.align	16, 0x90
.LBB0_105:                              # %for.body100.lr.ph.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_106 Depth 5
	movq	$1, %rdx
	.align	16, 0x90
.LBB0_106:                              # %for.body100.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_105 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%rcx, %rsi
	andq	%r13, %rsi
	movq	%rdx, %rdi
	andq	%r13, %rdi
	cmpq	%rdi, %rsi
	je	.LBB0_115
# BB#107:                               # %land.lhs.true102.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	movq	2784(%rsp,%rcx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB0_115
# BB#108:                               # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	movq	2672(%rsp,%rdx,8), %rbx
	testq	%rbx, %rbx
	movq	%r8, %rsi
	je	.LBB0_110
# BB#109:                               # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	movq	%r14, %rsi
.LBB0_110:                              # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	cmpq	%rbx, %rdi
	movq	%r8, %rdi
	jne	.LBB0_112
# BB#111:                               # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	movq	%r14, %rdi
.LBB0_112:                              # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	xorq	%rbx, %rbx
	testq	%rdi, %rsi
	jne	.LBB0_114
# BB#113:                               # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	movq	%r10, %rbx
.LBB0_114:                              # %land.lhs.true106.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	movq	%rbx, %r10
.LBB0_115:                              # %for.inc118.i.i.i
                                        #   in Loop: Header=BB0_106 Depth=5
	incq	%rdx
	movq	%rdx, %rsi
	andq	%r13, %rsi
	cmpq	%rax, %rsi
	jne	.LBB0_106
# BB#116:                               # %for.inc121.i.i.i
                                        #   in Loop: Header=BB0_105 Depth=4
	incq	%rcx
	movq	%rcx, %rdx
	andq	%r13, %rdx
	cmpq	%rax, %rdx
	jne	.LBB0_105
.LBB0_117:                              # %classify_dependence.exit.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	cmpq	$0, 264(%rsp)           # 8-byte Folded Reload
	movq	248(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_204
# BB#118:                               # %for.cond80.preheader.lr.ph.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	256(%rsp), %rdx         # 8-byte Reload
	leaq	1(%rdx), %r8
	movq	$1, %rdi
	movq	264(%rsp), %r11         # 8-byte Reload
	movq	$2147483647, %r15       # imm = 0x7FFFFFFF
	.align	16, 0x90
.LBB0_119:                              # %for.cond80.preheader.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_121 Depth 5
                                        #             Child Loop BB0_128 Depth 6
                                        #             Child Loop BB0_148 Depth 6
                                        #             Child Loop BB0_158 Depth 6
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %r9
	jle	.LBB0_166
# BB#120:                               # %for.body83.lr.ph.i.i
                                        #   in Loop: Header=BB0_119 Depth=4
	movq	$1, %r14
	jmp	.LBB0_121
.LBB0_143:                              # %if.else66.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$32, %rcx
	shlq	%cl, %rax
	movslq	16(%r11), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$52, %rdx, %rcx
	testq	%rax, %rax
	leaq	336(%rsp,%rcx), %rax
	leaq	(%rax,%r14,4), %rax
	jle	.LBB0_145
# BB#144:                               # %if.then68.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movabsq	$0, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB0_164
.LBB0_145:                              # %if.else74.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$2, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB0_164
	.align	16, 0x90
.LBB0_121:                              # %for.body83.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_119 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB0_128 Depth 6
                                        #             Child Loop BB0_148 Depth 6
                                        #             Child Loop BB0_158 Depth 6
	imulq	$52, %rdi, %rax
	testq	%r13, %r10
	leaq	336(%rsp,%rax), %rcx
	movq	$5, %rdx
	movl	%edx, (%rcx,%r14,4)
	leaq	1024(%rsp,%rax), %rax
	movl	%r15d, (%rax,%r14,4)
	je	.LBB0_146
# BB#122:                               # %land.lhs.true93.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movl	272(%rsp,%r14,4), %eax
	leaq	-3(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB0_126
# BB#123:                               # %land.lhs.true93.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_124
.LBB0_126:                              # %if.then119.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	%rdi, %r9
	movq	32(%r11), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB0_133
# BB#127:                               # %for.body.lr.ph.i.i427.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$5, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	2144(%rsp,%rdx), %r12
	movq	1728(%rsp,%rdx), %rdx
	xorq	%rsi, %rsi
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_128:                              # %for.body.i.i439.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_119 Depth=4
                                        #           Parent Loop BB0_121 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movq	(%rax), %r15
	movq	$1, %rbx
	cmpq	%r15, %r12
	movq	$1, %rcx
	je	.LBB0_130
# BB#129:                               # %select.mid487
                                        #   in Loop: Header=BB0_128 Depth=6
	movq	%rdi, %rcx
.LBB0_130:                              # %select.end486
                                        #   in Loop: Header=BB0_128 Depth=6
	cmpq	%r15, %rdx
	je	.LBB0_132
# BB#131:                               # %select.mid601
                                        #   in Loop: Header=BB0_128 Depth=6
	movq	%rsi, %rbx
.LBB0_132:                              # %select.end600
                                        #   in Loop: Header=BB0_128 Depth=6
	movq	24(%rax), %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdi
	jne	.LBB0_128
.LBB0_133:                              # %check_subscript_induction.exit.i443.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	orq	%rcx, %rbx
	testq	%r13, %rbx
	movq	$2147483647, %r15       # imm = 0x7FFFFFFF
	je	.LBB0_164
# BB#134:                               # %if.end.i447.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$5, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movl	2136(%rsp,%rsi), %edx
	movl	1720(%rsp,%rsi), %eax
	movl	1716(%rsp,%rsi), %edi
	movl	2132(%rsp,%rsi), %esi
	movabsq	$2147483648, %rbx       # imm = 0x80000000
	cmpq	%rbx, %rdi
	je	.LBB0_139
# BB#135:                               # %if.else.i448.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	%rsi, %rcx
	andq	%r13, %rcx
	cmpq	%rbx, %rcx
	je	.LBB0_138
# BB#136:                               # %if.else23.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	leaq	(%rdi,%rsi), %rcx
	testq	%r13, %rcx
	jne	.LBB0_139
# BB#137:                               # %if.then31.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$63, %rcx
	sarq	%cl, %rsi
	addq	%rsi, %rdi
	xorq	%rsi, %rdi
	addq	%rdi, %rdi
.LBB0_138:                              # %if.end43.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	%rdi, %rsi
.LBB0_139:                              # %if.end43.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	subq	%rdx, %rax
	movq	%rax, %rdi
	andq	%r13, %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cqto
	idivq	%rbx
	movslq	16(%r11), %rcx
	imulq	%rax, %rsi
	andq	%r13, %rsi
	cmpq	%rdi, %rsi
	jne	.LBB0_140
# BB#141:                               # %if.else53.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	imulq	$52, %rcx, %rcx
	leaq	1024(%rsp,%rcx), %rcx
	movl	%eax, (%rcx,%r14,4)
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB0_143
# BB#142:                               # %if.then60.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movslq	16(%r11), %rax
	imulq	$52, %rax, %rax
	leaq	336(%rsp,%rax), %rax
	movq	$3, %rcx
	jmp	.LBB0_163
.LBB0_124:                              # %land.lhs.true93.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	testq	%r13, %rax
	jne	.LBB0_146
# BB#125:                               # %if.then98.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	%rdi, %r9
	movq	$5, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movl	1720(%rsp,%rax), %ecx
	movl	2136(%rsp,%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB0_161
	jmp	.LBB0_164
	.align	16, 0x90
.LBB0_146:                              # %if.else124.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	%rdi, %r9
	movq	32(%r11), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB0_153
# BB#147:                               # %for.body.lr.ph.i.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$5, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	2144(%rsp,%rdx), %r12
	movq	1728(%rsp,%rdx), %r15
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_148:                              # %for.body.i.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_119 Depth=4
                                        #           Parent Loop BB0_121 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movq	(%rax), %rdx
	movq	$1, %rsi
	cmpq	%rdx, %r12
	movq	$1, %rcx
	je	.LBB0_150
# BB#149:                               # %select.mid502
                                        #   in Loop: Header=BB0_148 Depth=6
	movq	%rdi, %rcx
.LBB0_150:                              # %select.end501
                                        #   in Loop: Header=BB0_148 Depth=6
	cmpq	%rdx, %r15
	je	.LBB0_152
# BB#151:                               # %select.mid603
                                        #   in Loop: Header=BB0_148 Depth=6
	movq	%rbx, %rsi
.LBB0_152:                              # %select.end602
                                        #   in Loop: Header=BB0_148 Depth=6
	movq	24(%rax), %rax
	testq	%rax, %rax
	movq	%rsi, %rbx
	movq	%rcx, %rdi
	jne	.LBB0_148
.LBB0_153:                              # %check_subscript_induction.exit.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	orq	%rcx, %rsi
	testq	%r13, %rsi
	movq	$2147483647, %r15       # imm = 0x7FFFFFFF
	je	.LBB0_164
# BB#154:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$5, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movl	2132(%rsp,%rdx), %eax
	testq	%rax, %rax
	je	.LBB0_164
# BB#155:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movl	1716(%rsp,%rdx), %esi
	testq	%r13, %rsi
	je	.LBB0_164
# BB#156:                               # %if.else10.i.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$63, %rcx
	sarq	%cl, %rdx
	addq	%rdx, %rax
	xorq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$63, %rcx
	sarq	%cl, %rdx
	addq	%rdx, %rsi
	xorq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cqto
	idivq	%rdi
	jmp	.LBB0_158
	.align	16, 0x90
.LBB0_157:                              # %while.body.i.i.i.i
                                        #   in Loop: Header=BB0_158 Depth=6
	movq	%rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rsi, %rax
	cqto
	idivq	%rbx
	movq	%rdi, %rsi
.LBB0_158:                              # %while.body.i.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_119 Depth=4
                                        #           Parent Loop BB0_121 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	testq	%r13, %rdx
	jne	.LBB0_157
# BB#159:                               # %find_gcd.exit.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_164
# BB#160:                               # %if.then9.i419.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movq	$5, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movl	2136(%rsp,%rdx), %eax
	movl	1720(%rsp,%rdx), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	testq	%r13, %rdx
	je	.LBB0_164
.LBB0_161:                              # %if.then17.i420.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movslq	16(%r11), %rax
	imulq	$52, %rax, %rax
	jmp	.LBB0_162
.LBB0_140:                              # %if.then48.i.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	imulq	$52, %rcx, %rax
.LBB0_162:                              # %for.inc131.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	leaq	336(%rsp,%rax), %rax
	movq	$6, %rcx
.LBB0_163:                              # %for.inc131.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	movl	%ecx, (%rax,%r14,4)
.LBB0_164:                              # %for.inc131.i.i
                                        #   in Loop: Header=BB0_121 Depth=5
	incq	%r14
	movq	%r14, %rax
	andq	%r13, %rax
	movq	%r8, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	movq	%r9, %rdi
	jne	.LBB0_121
# BB#165:                               #   in Loop: Header=BB0_119 Depth=4
	movq	%r8, %r9
	movq	256(%rsp), %rdx         # 8-byte Reload
.LBB0_166:                              # %for.end133.i.i
                                        #   in Loop: Header=BB0_119 Depth=4
	movq	24(%r11), %r11
	incq	%rdi
	testq	%r11, %r11
	jne	.LBB0_119
# BB#167:                               # %for.end136.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	movq	216(%rsp), %r12         # 8-byte Reload
	movq	248(%rsp), %rcx         # 8-byte Reload
	jl	.LBB0_204
# BB#168:                               # %for.body.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	leaq	-1(%rdi), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rsi
	cmpq	$1, %rdx
	leaq	1080(%rsp), %rax
	movq	%rax, %rbx
	leaq	392(%rsp), %rax
	jg	.LBB0_171
# BB#169:                               #   in Loop: Header=BB0_54 Depth=3
	movq	$2, %rax
	leaq	1076(%rsp), %rcx
	leaq	388(%rsp), %rdx
	movq	$5, %r8
	movq	$2147483647, %r9        # imm = 0x7FFFFFFF
	.align	16, 0x90
.LBB0_170:                              # %for.end.us.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rdi, %rsi
	andq	%r13, %rsi
	movl	%r9d, (%rcx)
	movl	%r8d, (%rdx)
	movq	%rdi, %rbx
	movq	%rax, %rdi
	andq	%r13, %rdi
	incq	%rax
	addq	$52, %rdx
	addq	$52, %rcx
	cmpq	%rsi, %rdi
	movq	%rbx, %rdi
	jne	.LBB0_170
	jmp	.LBB0_176
	.align	16, 0x90
.LBB0_171:                              # %for.body7.lr.ph.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_172 Depth 5
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	%rbx, 232(%rsp)         # 8-byte Spill
	movq	%rsi, 256(%rsp)         # 8-byte Spill
	movq	%rdi, 240(%rsp)         # 8-byte Spill
	imulq	$52, %rsi, %rcx
	leaq	1024(%rsp,%rcx), %r11
	movq	$2147483647, %rsi       # imm = 0x7FFFFFFF
	movl	%esi, 1024(%rsp,%rcx)
	leaq	336(%rsp,%rcx), %rdi
	movq	$5, %rdx
	movl	%edx, 336(%rsp,%rcx)
	movq	$2, %r15
	movq	%rbx, %r8
	movq	%rax, %r10
	movq	$1, %r12
	.align	16, 0x90
.LBB0_172:                              # %for.body7.i.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        #         Parent Loop BB0_171 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movslq	(%r8), %rbx
	movq	$31, %rcx
	movq	%rbx, %rax
	sarq	%cl, %rax
	leaq	(%rbx,%rax), %rcx
	xorq	%rax, %rcx
	testq	%rbx, %rbx
	movq	$-1, %r14
	js	.LBB0_174
# BB#173:                               # %for.body7.i.i.i
                                        #   in Loop: Header=BB0_172 Depth=5
	movq	%r12, %r14
.LBB0_174:                              # %for.body7.i.i.i
                                        #   in Loop: Header=BB0_172 Depth=5
	andq	%rcx, %rsi
	movl	%esi, (%r11)
	movslq	(%r10), %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	merge_dependencies.direction_merge(%rdx,%rax,4), %edx
	movl	%edx, (%rdi)
	movq	%r15, %rax
	andq	%r13, %rax
	movq	%r9, %rcx
	andq	%r13, %rcx
	incq	%r15
	addq	$4, %r10
	addq	$4, %r8
	cmpq	%rcx, %rax
	movq	%r14, %r12
	jne	.LBB0_172
# BB#175:                               # %for.end.i.i.i
                                        #   in Loop: Header=BB0_171 Depth=4
	movq	240(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rdx
	andq	%r13, %rdx
	imulq	%r14, %rsi
	movl	%esi, (%r11)
	movq	256(%rsp), %rsi         # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rcx
	andq	%r13, %rcx
	movq	224(%rsp), %rax         # 8-byte Reload
	addq	$52, %rax
	movq	232(%rsp), %rbx         # 8-byte Reload
	addq	$52, %rbx
	cmpq	%rdx, %rcx
	jne	.LBB0_171
.LBB0_176:                              #   in Loop: Header=BB0_54 Depth=3
	movq	$1, %rax
	xorq	%r8, %r8
	leaq	388(%rsp), %rsi
	movq	184(%rsp), %rdx         # 8-byte Reload
	.align	16, 0x90
.LBB0_177:                              # %for.body144.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	(%rsi), %ecx
	cmpq	$6, %rcx
	je	.LBB0_179
# BB#178:                               # %select.mid507
                                        #   in Loop: Header=BB0_177 Depth=4
	movq	$1, %r8
.LBB0_179:                              # %select.end506
                                        #   in Loop: Header=BB0_177 Depth=4
	movq	$32, %rcx
	leaq	1(%rax), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	$52, %rsi
	cmpq	%rbx, %rax
	movq	%rdi, %rax
	jl	.LBB0_177
# BB#180:                               # %for.end154.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	testq	%r13, %r8
	movq	216(%rsp), %r12         # 8-byte Reload
	movq	248(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_204
# BB#181:                               # %do.body.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%rdx, %r15
	movq	%rcx, %r14
	movq	dep_chain(%rip), %rdi
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB0_183
# BB#182:                               # %if.then161.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	addq	%rsi, %rsi
	callq	varray_grow
	movq	%rax, dep_chain(%rip)
.LBB0_183:                              # %if.end164.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$136, %rdi
	callq	xmalloc
	movq	dep_chain(%rip), %rcx
	movq	8(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	%rax, 32(%rcx,%rdx,8)
	movq	dep_chain(%rip), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rbx
	movq	16(%r14), %rax
	movq	%rax, (%rbx)
	movq	16(%r12), %rax
	cmpq	%r14, %r12
	movq	%rax, 8(%rbx)
	movq	$0, 128(%rbx)
	jae	.LBB0_186
# BB#184:                               # %land.lhs.true179.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movl	32(%r14), %eax
	cmpq	$1, %rax
	jne	.LBB0_186
# BB#185:                               # %if.then183.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%r12, %r9
	movabsq	$0, %rax
	movl	%eax, 16(%rbx)
	movq	$1, %rsi
	leaq	388(%rsp), %rax
	leaq	1076(%rsp), %rdx
	movq	%r15, %r10
	jmp	.LBB0_191
.LBB0_186:                              # %if.else184.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	cmpq	%r14, %r12
	movq	%r12, %r9
	movq	%r15, %r10
	jbe	.LBB0_189
# BB#187:                               # %land.lhs.true187.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movl	32(%r14), %eax
	cmpq	$1, %rax
	jne	.LBB0_189
# BB#188:                               # %if.then191.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$1, %rsi
	movl	%esi, 16(%rbx)
	jmp	.LBB0_190
.LBB0_189:                              # %if.else193.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$2, %rax
	movl	%eax, 16(%rbx)
	movq	$1, %rsi
.LBB0_190:                              # %for.body201.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	leaq	388(%rsp), %rax
	leaq	1076(%rsp), %rdx
.LBB0_191:                              # %for.body201.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB0_198
# BB#192:                               # %for.body201.i.i
                                        #   in Loop: Header=BB0_191 Depth=4
	cmpq	$3, %rcx
	je	.LBB0_193
# BB#199:                               # %for.inc229.i.i
                                        #   in Loop: Header=BB0_191 Depth=4
	movq	$32, %rcx
	leaq	1(%rsi), %r8
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$52, %rdx
	addq	$52, %rax
	cmpq	%rdi, %rsi
	movq	%r8, %rsi
	jl	.LBB0_191
	jmp	.LBB0_194
.LBB0_198:                              # %if.then225.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movabsq	$0, %rax
	movl	%eax, 16(%rbx)
.LBB0_194:                              # %for.cond232.preheader.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movl	388(%rsp), %eax
	movl	%eax, 24(%rbx)
	movl	1076(%rsp), %eax
	movl	%eax, 76(%rbx)
	movl	440(%rsp), %eax
	movl	%eax, 28(%rbx)
	movl	1128(%rsp), %eax
	movl	%eax, 80(%rbx)
	movl	492(%rsp), %eax
	movl	%eax, 32(%rbx)
	movl	1180(%rsp), %eax
	movl	%eax, 84(%rbx)
	movl	544(%rsp), %eax
	movl	%eax, 36(%rbx)
	movl	1232(%rsp), %eax
	movl	%eax, 88(%rbx)
	movl	596(%rsp), %eax
	movl	%eax, 40(%rbx)
	movl	1284(%rsp), %eax
	movl	%eax, 92(%rbx)
	movl	648(%rsp), %eax
	movl	%eax, 44(%rbx)
	movl	1336(%rsp), %eax
	movl	%eax, 96(%rbx)
	movl	700(%rsp), %eax
	movl	%eax, 48(%rbx)
	movl	1388(%rsp), %eax
	movl	%eax, 100(%rbx)
	movl	752(%rsp), %eax
	movl	%eax, 52(%rbx)
	movl	1440(%rsp), %eax
	movl	%eax, 104(%rbx)
	movl	804(%rsp), %eax
	movl	%eax, 56(%rbx)
	movl	1492(%rsp), %eax
	movl	%eax, 108(%rbx)
	movl	856(%rsp), %eax
	movl	%eax, 60(%rbx)
	movl	1544(%rsp), %eax
	movl	%eax, 112(%rbx)
	movl	908(%rsp), %eax
	movl	%eax, 64(%rbx)
	movl	1596(%rsp), %eax
	movl	%eax, 116(%rbx)
	movl	960(%rsp), %eax
	movl	%eax, 68(%rbx)
	movl	1648(%rsp), %eax
	movl	%eax, 120(%rbx)
	movq	%r9, %r12
	movq	48(%r12), %rcx
	.align	16, 0x90
.LBB0_195:                              # %for.cond251.i.i
                                        #   Parent Loop BB0_10 Depth=1
                                        #     Parent Loop BB0_13 Depth=2
                                        #       Parent Loop BB0_54 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rcx, %rax
	testq	%rax, %rax
	je	.LBB0_200
# BB#196:                               # %land.rhs253.i.i
                                        #   in Loop: Header=BB0_195 Depth=4
	movq	128(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_195
# BB#197:                               # %if.else292.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%rbx, 128(%rax)
	jmp	.LBB0_203
.LBB0_200:                              # %do.body263.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	dep_chain(%rip), %rdi
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB0_202
# BB#201:                               # %if.then268.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	addq	%rsi, %rsi
	callq	varray_grow
	movq	%rax, dep_chain(%rip)
.LBB0_202:                              # %if.end272.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	$136, %rdi
	callq	xmalloc
	movq	dep_chain(%rip), %rcx
	movq	8(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	%rax, 32(%rcx,%rdx,8)
	movq	dep_chain(%rip), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rax
	movq	$0, (%rax)
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$3, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 128(%rax)
	movq	%rbx, 48(%r12)
.LBB0_203:                              # %for.inc295.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	%r14, %rcx
	.align	16, 0x90
.LBB0_204:                              # %for.inc295.i.i
                                        #   in Loop: Header=BB0_54 Depth=3
	movq	40(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_54
	.align	16, 0x90
.LBB0_205:                              # %for.inc298.i.i
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	40(%r12), %r12
	testq	%r12, %r12
	movq	208(%rsp), %rsi         # 8-byte Reload
	jne	.LBB0_13
# BB#206:                               # %for.inc.loopexit.i
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	def_use_chain(%rip), %rdi
	.align	16, 0x90
.LBB0_207:                              # %for.inc.i32
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	32(%rdi,%rax,8), %rcx
	incq	%rsi
	testq	%rcx, %rcx
	jne	.LBB0_10
	jmp	.LBB0_209
.LBB0_208:                              # %for.body
	movq	%rax, %rdi
	callq	free
.LBB0_209:                              # %for.body
	movq	def_use_chain(%rip), %rdi
.LBB0_210:                              # %for.body
	movq	8(%rdi), %rcx
	movq	24(%rdi,%rcx,8), %rax
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 8(%rdi)
	jne	.LBB0_208
# BB#211:                               # %do.body
	testq	%rdi, %rdi
	je	.LBB0_213
# BB#212:                               # %if.then
	callq	free
	movq	$0, def_use_chain(%rip)
.LBB0_213:                              # %do.body11
	movq	loop_chain(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_215
# BB#214:                               # %if.then13
	callq	free
	movq	$0, loop_chain(%rip)
.LBB0_215:                              # %do.body16
	movq	induction_chain(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_217
# BB#216:                               # %if.then18
	callq	free
	movq	$0, induction_chain(%rip)
.LBB0_217:                              # %do.end20
	addq	$2896, %rsp             # imm = 0xB50
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	init_dependence_analysis, .Ltmp17-init_dependence_analysis
	.cfi_endproc

	.align	16, 0x90
	.type	build_def_use,@function
build_def_use:                          # @build_def_use
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
	subq	$64, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 112
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
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rsi, %rax
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB1_2
# BB#1:                                 # %if.then
	movq	$0, build_def_use.outer_loop(%rip)
	xorq	%rax, %rax
	movl	%eax, build_def_use.nloop(%rip)
	movl	%eax, build_def_use.du_idx(%rip)
	jmp	.LBB1_2
	.align	16, 0x90
.LBB1_122:                              # %while.cond.backedge
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	(%rbx), %rbx
.LBB1_2:                                # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_25 Depth 2
                                        #     Child Loop BB1_37 Depth 2
                                        #     Child Loop BB1_81 Depth 2
                                        #       Child Loop BB1_87 Depth 3
	testq	%rbx, %rbx
	je	.LBB1_113
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$40, %rax
	jg	.LBB1_6
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$2, %rax
	jne	.LBB1_115
# BB#5:                                 # %sw.bb2
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rbx), %rdi
.LBB1_120:                              # %if.then197
                                        #   in Loop: Header=BB1_2 Depth=1
	xorq	%rsi, %rsi
	jmp	.LBB1_121
	.align	16, 0x90
.LBB1_6:                                # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$151, %rax
	jg	.LBB1_11
# BB#7:                                 # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$52, %rax
	jg	.LBB1_15
# BB#8:                                 # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$48, %rax
	jne	.LBB1_9
# BB#77:                                # %sw.bb29
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	build_def_use.loop_def(%rip), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	32(%rbx), %r15
	testq	%rax, %rax
	je	.LBB1_90
# BB#78:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_2 Depth=1
	movzbl	16(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB1_90
# BB#79:                                # %land.lhs.true39
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	loop_chain(%rip), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_90
# BB#80:                                # %land.rhs.lr.ph.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	72(%r15), %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	32(%rcx), %r12
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	induction_chain(%rip), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%r13, %r13
.LBB1_81:                               # %land.rhs.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_87 Depth 3
	movq	%r13, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jae	.LBB1_90
# BB#82:                                # %for.body.i
                                        #   in Loop: Header=BB1_81 Depth=2
	movq	(%rax), %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB1_85
# BB#83:                                # %if.then.i
                                        #   in Loop: Header=BB1_81 Depth=2
	movq	32(%rax), %r14
	testq	%r14, %r14
	je	.LBB1_85
# BB#84:                                # %land.rhs7.lr.ph.i
                                        #   in Loop: Header=BB1_81 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB1_85
	.align	16, 0x90
.LBB1_87:                               # %for.body13.i
                                        #   Parent Loop BB1_2 Depth=1
                                        #     Parent Loop BB1_81 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%r14), %rdi
	movq	%r12, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_88
# BB#86:                                # %for.cond5.i
                                        #   in Loop: Header=BB1_87 Depth=3
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB1_87
.LBB1_85:                               # %for.inc17.i
                                        #   in Loop: Header=BB1_81 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx,%rax,8), %rax
	testq	%rax, %rax
	jne	.LBB1_81
	jmp	.LBB1_90
.LBB1_11:                               # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$165, %rax
	jg	.LBB1_114
# BB#12:                                # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	leaq	-152(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$4, %rcx
	ja	.LBB1_115
# BB#13:                                # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	jmpq	*.LJTI1_0(,%rcx,8)
.LBB1_119:                              # %sw.bb188
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$48, %rax
	jne	.LBB1_122
	jmp	.LBB1_120
.LBB1_15:                               # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$53, %rax
	jne	.LBB1_115
	jmp	.LBB1_122
.LBB1_114:                              # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$166, %rax
	je	.LBB1_122
	jmp	.LBB1_115
.LBB1_9:                                # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$41, %rax
	je	.LBB1_92
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$43, %rax
	je	.LBB1_94
	.align	16, 0x90
.LBB1_115:                              # %sw.default
                                        #   in Loop: Header=BB1_2 Depth=1
	movzbl	tree_code_type(%rax), %eax
	cmpq	$50, %rax
	jne	.LBB1_113
# BB#116:                               # %if.then212
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rbx), %rdi
	movq	$1, %rsi
.LBB1_117:                              # %while.cond.backedge
                                        #   in Loop: Header=BB1_2 Depth=1
	callq	build_def_use
.LBB1_118:                              # %while.cond.backedge
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	40(%rbx), %rdi
	movq	$1, %rsi
.LBB1_121:                              # %while.cond.backedge
                                        #   in Loop: Header=BB1_2 Depth=1
	callq	build_def_use
	jmp	.LBB1_122
.LBB1_14:                               # %sw.bb
                                        #   in Loop: Header=BB1_2 Depth=1
	addq	$32, %rbx
	movq	(%rbx), %rbx
	jmp	.LBB1_2
.LBB1_16:                               # %sw.bb7
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	build_def_use.nloop(%rip), %r14d
	testq	%r14, %r14
	jne	.LBB1_17
# BB#18:                                # %if.then9
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%rbx, build_def_use.outer_loop(%rip)
	movq	%rbx, %r15
	jmp	.LBB1_19
.LBB1_88:                               # %have_induction_variable.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	js	.LBB1_90
# BB#89:                                # %if.then45
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$1, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 20(%rcx)
.LBB1_90:                               # %if.end47
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	16(%r15), %eax
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$43, %rax
	jne	.LBB1_118
# BB#91:                                # %if.then64
                                        #   in Loop: Header=BB1_2 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	jmp	.LBB1_117
.LBB1_17:                               # %sw.bb7.if.end10_crit_edge
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	build_def_use.outer_loop(%rip), %r15
.LBB1_19:                               # %if.end10
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%r14
	movq	loop_chain(%rip), %rdi
	movl	%r14d, build_def_use.nloop(%rip)
	movq	%rbx, build_def_use.current_loop(%rip)
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB1_21
# BB#20:                                # %if.then.i283
                                        #   in Loop: Header=BB1_2 Depth=1
	addq	%rsi, %rsi
	callq	varray_grow
	movq	%rax, loop_chain(%rip)
.LBB1_21:                               # %add_loop.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$40, %rdi
	callq	xmalloc
	movq	loop_chain(%rip), %rcx
	movq	8(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	%rax, 32(%rcx,%rdx,8)
	movq	loop_chain(%rip), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %r12
	movq	%r15, (%r12)
	movq	%rbx, 8(%r12)
	movl	%r14d, 16(%r12)
	movabsq	$0, %rax
	movl	%eax, 36(%r12)
	movq	$0, 28(%r12)
	movq	$0, 20(%r12)
	movq	%r12, build_def_use.loop_def(%rip)
	movq	32(%rbx), %r13
	testq	%r13, %r13
	je	.LBB1_75
# BB#22:                                # %add_loop.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	48(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_75
# BB#23:                                # %add_loop.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.LBB1_75
# BB#24:                                # %while.cond.preheader.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	16(%rcx), %esi
	movq	%rcx, %rdx
.LBB1_25:                               # %while.cond.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rsi
	cmpq	$47, %rsi
	jne	.LBB1_26
# BB#27:                                # %while.body.i
                                        #   in Loop: Header=BB1_25 Depth=2
	movq	32(%rdx), %r14
	movl	16(%r14), %esi
	movq	%rsi, %rdi
	andq	$253, %rdi
	cmpq	$129, %rdi
	je	.LBB1_35
# BB#28:                                # %while.body.i
                                        #   in Loop: Header=BB1_25 Depth=2
	andq	$255, %rsi
	cmpq	$130, %rsi
	je	.LBB1_35
# BB#29:                                # %while.body.i
                                        #   in Loop: Header=BB1_25 Depth=2
	cmpq	$132, %rsi
	je	.LBB1_35
# BB#30:                                # %if.end19.i
                                        #   in Loop: Header=BB1_25 Depth=2
	movq	40(%rdx), %rdx
	movl	16(%rdx), %esi
	movq	%rsi, %rdi
	andq	$253, %rdi
	cmpq	$129, %rdi
	je	.LBB1_31
# BB#32:                                # %if.end19.i
                                        #   in Loop: Header=BB1_25 Depth=2
	movq	%rsi, %rdi
	andq	$255, %rdi
	cmpq	$130, %rdi
	je	.LBB1_33
# BB#34:                                # %if.end19.i
                                        #   in Loop: Header=BB1_25 Depth=2
	cmpq	$132, %rdi
	movq	%rdx, %r14
	jne	.LBB1_25
	jmp	.LBB1_35
.LBB1_26:                               #   in Loop: Header=BB1_2 Depth=1
	movq	%rcx, %r14
	jmp	.LBB1_35
.LBB1_31:                               #   in Loop: Header=BB1_2 Depth=1
	movq	%rdx, %r14
	jmp	.LBB1_35
.LBB1_33:                               #   in Loop: Header=BB1_2 Depth=1
	movq	%rdx, %r14
.LBB1_35:                               # %while.end.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rax), %r8
	movzbl	16(%r8), %ecx
	addq	$-91, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB1_45
# BB#36:                                # %while.body53.lr.ph.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%r8, %rcx
.LBB1_37:                               # %while.body53.i
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rsi
	movq	32(%rsi), %rcx
	movzbl	16(%rcx), %edx
	movzbl	tree_code_type(%rdx), %edi
	cmpq	$60, %rdi
	jne	.LBB1_41
# BB#38:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB1_37 Depth=2
	movq	32(%rcx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$34, %rax
	jne	.LBB1_41
# BB#39:                                # %land.lhs.true76.i
                                        #   in Loop: Header=BB1_37 Depth=2
	movq	32(%r14), %rax
	movq	72(%rdi), %rdi
	movq	72(%rax), %rax
	movq	32(%rdi), %rdi
	movq	32(%rax), %rax
	cmpq	%rax, %rdi
	je	.LBB1_40
.LBB1_41:                               # %if.end97.i
                                        #   in Loop: Header=BB1_37 Depth=2
	movq	40(%rsi), %r15
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB1_44
# BB#42:                                # %land.lhs.true110.i
                                        #   in Loop: Header=BB1_37 Depth=2
	movq	32(%r15), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$34, %rax
	jne	.LBB1_44
# BB#43:                                # %land.lhs.true123.i
                                        #   in Loop: Header=BB1_37 Depth=2
	movq	32(%r14), %rax
	movq	72(%rsi), %rsi
	movq	72(%rax), %rax
	movq	32(%rsi), %rsi
	movq	32(%rax), %rax
	cmpq	%rax, %rsi
	je	.LBB1_46
.LBB1_44:                               # %if.end151.i
                                        #   in Loop: Header=BB1_37 Depth=2
	addq	$-91, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jb	.LBB1_37
.LBB1_45:                               #   in Loop: Header=BB1_2 Depth=1
	movq	%r8, %r15
.LBB1_46:                               # %while.end155.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$253, %rcx
	cmpq	$129, %rcx
	je	.LBB1_49
# BB#47:                                # %while.end155.i
                                        #   in Loop: Header=BB1_2 Depth=1
	andq	$255, %rax
	cmpq	$130, %rax
	je	.LBB1_49
# BB#48:                                # %while.end155.i
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$132, %rax
	jne	.LBB1_75
.LBB1_49:                               # %land.lhs.true171.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB1_75
# BB#50:                                # %do.body.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	induction_chain(%rip), %rdi
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB1_52
# BB#51:                                # %if.then226.i
                                        #   in Loop: Header=BB1_2 Depth=1
	addq	%rsi, %rsi
	callq	varray_grow
	movq	%rax, induction_chain(%rip)
.LBB1_52:                               # %if.end228.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$32, %rdi
	callq	xmalloc
	movq	induction_chain(%rip), %rcx
	movq	8(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	%rax, 32(%rcx,%rdx,8)
	movq	induction_chain(%rip), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rax
	movq	%rax, 32(%r12)
	movq	32(%r14), %rcx
	movq	72(%rcx), %rcx
	movq	32(%rcx), %r8
	movq	%r8, (%rax)
	movq	40(%r14), %rcx
	movq	32(%rcx), %r9
	movl	%r9d, 8(%rax)
	movl	16(%r14), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$131, %rcx
	jne	.LBB1_54
# BB#53:                                # %if.then262.i
                                        #   in Loop: Header=BB1_2 Depth=1
	negq	%r9
	movl	%r9d, 8(%rax)
.LBB1_54:                               # %if.end266.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movabsq	$2147483648, %r10       # imm = 0x80000000
	movl	16(%r13), %esi
	movq	%rsi, %rcx
	andq	$255, %rcx
	cmpq	$166, %rcx
	jne	.LBB1_57
# BB#55:                                # %if.end.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	movq	%r10, %rsi
	je	.LBB1_65
# BB#56:                                # %if.end.while.cond.preheader_crit_edge.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	16(%r13), %esi
.LBB1_57:                               # %while.cond.preheader.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%rsi, %rcx
	andq	$255, %rcx
	cmpq	$152, %rcx
	jne	.LBB1_58
# BB#60:                                # %if.then34.split.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%r13), %r13
	movl	16(%r13), %esi
.LBB1_61:                               # %if.end38.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	andq	$255, %rsi
	cmpq	$48, %rsi
	movq	%r10, %rsi
	jne	.LBB1_65
# BB#62:                                # %land.lhs.true44.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%r13), %rdi
	movzbl	16(%rdi), %ecx
	cmpq	$34, %rcx
	movq	%r10, %rsi
	jne	.LBB1_65
# BB#63:                                # %land.lhs.true53.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	72(%rdi), %rcx
	movq	32(%rcx), %rcx
	cmpq	%r8, %rcx
	movq	%r10, %rsi
	jne	.LBB1_65
# BB#64:                                # %if.then63.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	40(%r13), %rcx
	movq	32(%rcx), %rsi
.LBB1_65:                               # %get_low_bound.exit.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	%esi, 12(%rax)
	movq	32(%r15), %rsi
	movzbl	16(%rsi), %edi
	cmpq	$34, %rdi
	jne	.LBB1_69
# BB#66:                                # %land.lhs.true278.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	72(%rsi), %rcx
	movq	32(%rcx), %rcx
	cmpq	%r8, %rcx
	jne	.LBB1_69
# BB#67:                                # %if.then290.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	40(%r15), %rcx
	movzbl	16(%rcx), %esi
	cmpq	$25, %rsi
	jne	.LBB1_73
# BB#68:                                # %if.then300.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rcx), %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	jmp	.LBB1_76
.LBB1_75:                               # %if.then22
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$1, %rax
	movl	%eax, 20(%r12)
.LBB1_76:                               # %if.end23
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	56(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	build_def_use
	movl	build_def_use.nloop(%rip), %eax
	decq	%rax
	movl	%eax, build_def_use.nloop(%rip)
	movq	$0, build_def_use.current_loop(%rip)
	movq	(%rbx), %rbx
	jmp	.LBB1_2
.LBB1_69:                               # %if.else313.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	40(%r15), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$34, %rdx
	jne	.LBB1_74
# BB#70:                                # %land.lhs.true323.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	72(%rcx), %rcx
	movq	32(%rcx), %rcx
	cmpq	%r8, %rcx
	jne	.LBB1_74
# BB#71:                                # %if.then335.i
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$25, %rdi
	jne	.LBB1_73
# BB#72:                                # %if.then345.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	32(%rsi), %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	jmp	.LBB1_76
.LBB1_40:                               #   in Loop: Header=BB1_2 Depth=1
	movq	%rcx, %r15
	jmp	.LBB1_46
.LBB1_73:                               # %if.else354.i
                                        #   in Loop: Header=BB1_2 Depth=1
	andq	%r10, %r9
	movq	$31, %rcx
	shrq	%cl, %r9
	addq	$2147483647, %r9        # imm = 0x7FFFFFFF
	movl	%r9d, 16(%rax)
.LBB1_74:                               # %find_induction_variable.exit
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	$0, 24(%rax)
	jmp	.LBB1_76
.LBB1_58:                               # %while.cond.preheader.i.i
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$47, %rcx
	jne	.LBB1_61
.LBB1_59:                               # %while.cond.us.i.i
                                        # =>This Inner Loop Header: Depth=1
	jmp	.LBB1_59
.LBB1_92:                               # %sw.bb74
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB1_113
# BB#93:                                # %lor.lhs.false79
	movzbl	16(%rbx), %eax
	cmpq	$43, %rax
	jne	.LBB1_113
.LBB1_94:                               # %sw.bb93
	movl	build_def_use.nloop(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_113
# BB#95:                                # %if.then95
	movq	def_use_chain(%rip), %rdi
	xorq	%rax, %rax
	movb	%al, 63(%rsp)
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB1_97
# BB#96:                                # %if.then97
	addq	%rsi, %rsi
	callq	varray_grow
	movq	%rax, def_use_chain(%rip)
.LBB1_97:                               # %if.end100
	movq	$56, %rdi
	callq	xmalloc
	movq	def_use_chain(%rip), %rcx
	movq	8(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	%rax, 32(%rcx,%rdx,8)
	movslq	build_def_use.du_idx(%rip), %rax
	movq	def_use_chain(%rip), %rcx
	leaq	1(%rax), %r12
	movl	%r12d, build_def_use.du_idx(%rip)
	movq	32(%rcx,%rax,8), %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 32(%rdx)
	movq	$1, %rcx
	movl	%ecx, 36(%rdx)
	movq	build_def_use.outer_loop(%rip), %rcx
	movq	%rcx, (%rdx)
	movq	build_def_use.current_loop(%rip), %rcx
	movq	%rcx, 8(%rdx)
	movq	%rbx, 16(%rdx)
	leaq	63(%rsp), %rcx
	movq	%rcx, 24(%rdx)
	leaq	40(%rdx), %rsi
	movq	$0, 48(%rdx)
	movq	$0, 40(%rdx)
	jmp	.LBB1_98
.LBB1_100:                              # %for.inc
                                        #   in Loop: Header=BB1_98 Depth=1
	movq	32(%rbx), %rbx
.LBB1_98:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$39, %rcx
	je	.LBB1_101
# BB#99:                                # %for.cond
                                        #   in Loop: Header=BB1_98 Depth=1
	cmpq	$43, %rcx
	je	.LBB1_100
	jmp	.LBB1_103
.LBB1_101:                              # %if.then124
	movq	40(%rbx), %rbx
	movzbl	16(%rbx), %ecx
	cmpq	$37, %rcx
	jne	.LBB1_113
# BB#102:                               # %land.lhs.true133
	movq	8(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$18, %rcx
	jne	.LBB1_113
.LBB1_103:                              # %if.end143
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	testq	%rax, %rax
	js	.LBB1_107
# BB#104:                               # %land.rhs.lr.ph
	movq	72(%rbx), %rax
	movq	32(%rax), %r14
	movq	def_use_chain(%rip), %r15
	xorq	%r13, %r13
.LBB1_106:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15,%r13,8), %rax
	movq	24(%rax), %rsi
	movq	%r14, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_107
# BB#105:                               # %for.cond144
                                        #   in Loop: Header=BB1_106 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB1_106
.LBB1_107:                              # %for.end161
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	andq	%r13, %rax
	cmpq	%r12, %rax
	jne	.LBB1_108
# BB#111:                               # %if.else
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	$0, (%rax)
	movq	48(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB1_112
.LBB1_108:                              # %if.then163
	movq	$32, %rcx
	movq	def_use_chain(%rip), %rax
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	32(%rax,%r13,8), %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
.LBB1_109:                              # %for.cond168
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	40(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_109
# BB#110:                               # %for.end174
	movq	%rdx, 40(%rax)
.LBB1_112:                              # %if.end177
	movq	72(%rbx), %rax
	movq	32(%rax), %rax
	movq	%rax, 24(%rdx)
.LBB1_113:                              # %while.end
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	build_def_use, .Ltmp35-build_def_use
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_119
	.quad	.LBB1_14
	.quad	.LBB1_122
	.quad	.LBB1_115
	.quad	.LBB1_16

	.text
	.globl	search_dependence
	.align	16, 0x90
	.type	search_dependence,@function
search_dependence:                      # @search_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	movq	dep_chain(%rip), %r9
	xorq	%rax, %rax
	testq	%r9, %r9
	je	.LBB2_14
# BB#1:                                 # %if.then
	movzbl	16(%rdi), %edx
	cmpq	$41, %rdx
	jne	.LBB2_6
# BB#2:                                 # %land.lhs.true
	movq	40(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB2_6
# BB#3:                                 # %land.lhs.true2
	movzbl	16(%rdx), %esi
	cmpq	$43, %rsi
	je	.LBB2_5
# BB#4:                                 # %land.lhs.true2
	movq	%rdi, %rdx
.LBB2_5:                                # %land.lhs.true2
	movq	%rdx, %rdi
.LBB2_6:                                # %if.end
	movq	32(%r9), %rsi
	testq	%rsi, %rsi
	je	.LBB2_14
# BB#7:                                 # %for.body.lr.ph
	movq	dest_to_remember(%rip), %r8
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB2_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rax
	cmpq	%rax, %rdi
	jne	.LBB2_10
# BB#9:                                 # %land.lhs.true18
                                        #   in Loop: Header=BB2_8 Depth=1
	movq	8(%rsi), %rcx
	cmpq	%rcx, %r8
	je	.LBB2_12
.LBB2_10:                               # %lor.lhs.false
                                        #   in Loop: Header=BB2_8 Depth=1
	testq	%rax, %rax
	jne	.LBB2_13
# BB#11:                                # %land.lhs.true22
                                        #   in Loop: Header=BB2_8 Depth=1
	movq	8(%rsi), %rax
	cmpq	%rax, %rdi
	je	.LBB2_12
.LBB2_13:                               # %for.inc
                                        #   in Loop: Header=BB2_8 Depth=1
	movq	32(%r9,%rdx,8), %rsi
	incq	%rdx
	xorq	%rax, %rax
	testq	%rsi, %rsi
	jne	.LBB2_8
.LBB2_14:                               # %return
	retq
.LBB2_12:                               # %if.then25
	incq	%rdx
	movq	%rdx, %rax
	retq
.Ltmp36:
	.size	search_dependence, .Ltmp36-search_dependence
	.cfi_endproc

	.globl	remember_dest_for_dependence
	.align	16, 0x90
	.type	remember_dest_for_dependence,@function
remember_dest_for_dependence:           # @remember_dest_for_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB3_7
# BB#1:                                 # %if.then
	movzbl	16(%rdi), %eax
	cmpq	$41, %rax
	jne	.LBB3_6
# BB#2:                                 # %land.lhs.true
	movq	40(%rdi), %rax
	testq	%rax, %rax
	je	.LBB3_6
# BB#3:                                 # %land.lhs.true2
	movzbl	16(%rax), %ecx
	cmpq	$43, %rcx
	je	.LBB3_5
# BB#4:                                 # %land.lhs.true2
	movq	%rdi, %rax
.LBB3_5:                                # %land.lhs.true2
	movq	%rax, %rdi
.LBB3_6:                                # %if.end
	movq	%rdi, dest_to_remember(%rip)
.LBB3_7:                                # %if.end15
	retq
.Ltmp37:
	.size	remember_dest_for_dependence, .Ltmp37-remember_dest_for_dependence
	.cfi_endproc

	.globl	have_dependence_p
	.align	16, 0x90
	.type	have_dependence_p,@function
have_dependence_p:                      # @have_dependence_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rax
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	xorq	%rax, %rax
	cmpq	$66, %rdx
	movabsq	$0, %rdx
	jne	.LBB4_2
# BB#1:                                 # %if.then
	movq	24(%rcx), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	addq	%rcx, %rdx
.LBB4_2:                                # %if.end
	movq	32(%rsi), %rcx
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$66, %rsi
	jne	.LBB4_4
# BB#3:                                 # %if.then22
	movq	24(%rcx), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	addq	%rcx, %rax
.LBB4_4:                                # %if.end34
	movq	%rax, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jns	.LBB4_9
# BB#5:                                 # %if.end40
	negq	%rdx
	movq	$32, %rcx
	movq	dep_chain(%rip), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%rsi,%rdx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB4_9
# BB#6:                                 # %for.body.lr.ph
	negq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rsi,%rax,8), %rcx
	movq	$1, %rax
	.align	16, 0x90
.LBB4_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rcx, %rdx
	je	.LBB4_10
# BB#8:                                 # %for.inc
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	128(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB4_7
.LBB4_9:
	xorq	%rax, %rax
.LBB4_10:                               # %return
	retq
.Ltmp38:
	.size	have_dependence_p, .Ltmp38-have_dependence_p
	.cfi_endproc

	.globl	end_dependence_analysis
	.align	16, 0x90
	.type	end_dependence_analysis,@function
end_dependence_analysis:                # @end_dependence_analysis
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp40:
	.cfi_def_cfa_offset 16
	movq	dep_chain(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_2
# BB#1:                                 # %if.then
	callq	free
	movq	$0, dep_chain(%rip)
.LBB5_2:                                # %do.end
	popq	%rax
	retq
.Ltmp41:
	.size	end_dependence_analysis, .Ltmp41-end_dependence_analysis
	.cfi_endproc

	.align	16, 0x90
	.type	get_one_coefficient,@function
get_one_coefficient:                    # @get_one_coefficient
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp50:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp51:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp52:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp53:
	.cfi_def_cfa_offset 96
.Ltmp54:
	.cfi_offset %rbx, -48
.Ltmp55:
	.cfi_offset %r12, -40
.Ltmp56:
	.cfi_offset %r13, -32
.Ltmp57:
	.cfi_offset %r14, -24
.Ltmp58:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	movq	%rdx, %r15
	movq	%rsi, %r14
	testq	%rcx, %rcx
	je	.LBB6_2
# BB#1:                                 # %if.then
	movl	%eax, (%rcx)
.LBB6_2:                                # %if.end
	xorq	%r12, %r12
	cmpq	$115, %rax
	jg	.LBB6_27
# BB#3:                                 # %if.end
	cmpq	$58, %rax
	jg	.LBB6_8
# BB#4:                                 # %if.end
	cmpq	$25, %rax
	jne	.LBB6_5
# BB#26:                                # %if.then15
	movq	32(%rbx), %r12
	jmp	.LBB6_39
.LBB6_27:                               # %if.end
	cmpq	$116, %rax
	jne	.LBB6_39
# BB#28:                                # %if.then20
	movq	32(%rbx), %rdi
	leaq	44(%rsp), %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	get_one_coefficient
	movq	%rax, %r12
	jmp	.LBB6_39
.LBB6_8:                                # %if.end
	cmpq	$59, %rax
	je	.LBB6_29
# BB#9:                                 # %if.end
	cmpq	$60, %rax
	jne	.LBB6_10
# BB#33:                                # %if.then47
	movq	32(%rbx), %rdi
	leaq	44(%rsp), %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	get_one_coefficient
	movl	44(%rsp), %ecx
	cmpq	$25, %rcx
	jne	.LBB6_35
# BB#34:                                # %if.then54
	movl	%eax, 8(%r14)
.LBB6_35:                               # %if.end56
	movq	40(%rbx), %rdi
	leaq	44(%rsp), %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	get_one_coefficient
	movl	44(%rsp), %ecx
	cmpq	$25, %rcx
	jne	.LBB6_39
# BB#36:                                # %if.then63
	negq	%rax
	movl	%eax, 8(%r14)
	jmp	.LBB6_39
.LBB6_5:                                # %if.end
	cmpq	$34, %rax
	jne	.LBB6_39
# BB#6:                                 # %if.then1
	movq	loop_chain(%rip), %rdx
	movq	32(%rdx), %rax
	testq	%rax, %rax
	je	.LBB6_7
# BB#13:                                # %land.rhs.lr.ph.i
	movq	72(%rbx), %rcx
	movq	(%r15), %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	32(%rcx), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	(%rdx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	induction_chain(%rip), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_14:                               # %land.rhs.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_22 Depth 2
	movq	%rdx, %rbx
	movq	%r12, %rcx
	andq	%r15, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jae	.LBB6_15
# BB#16:                                # %for.body.i
                                        #   in Loop: Header=BB6_14 Depth=1
	movq	(%rax), %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB6_19
# BB#17:                                # %if.then.i
                                        #   in Loop: Header=BB6_14 Depth=1
	movq	32(%rax), %r13
	testq	%r13, %r13
	je	.LBB6_19
# BB#18:                                # %land.rhs7.lr.ph.i
                                        #   in Loop: Header=BB6_14 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	$0, (%rax)
	je	.LBB6_19
	.align	16, 0x90
.LBB6_22:                               # %for.body13.i
                                        #   Parent Loop BB6_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	strcmp
	testq	%r15, %rax
	je	.LBB6_23
# BB#21:                                # %for.cond5.i
                                        #   in Loop: Header=BB6_22 Depth=2
	movq	24(%r13), %r13
	testq	%r13, %r13
	jne	.LBB6_22
	.align	16, 0x90
.LBB6_19:                               # %for.inc17.i
                                        #   in Loop: Header=BB6_14 Depth=1
	incq	%r12
	movq	%r12, %rax
	andq	%r15, %rax
	movq	%rbx, %rdx
	movq	32(%rdx,%rax,8), %rax
	testq	%rax, %rax
	jne	.LBB6_14
# BB#20:
	movq	%r15, %r12
	jmp	.LBB6_39
.LBB6_29:                               # %if.then25
	movq	32(%rbx), %rdi
	leaq	44(%rsp), %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	get_one_coefficient
	movl	44(%rsp), %ecx
	cmpq	$25, %rcx
	jne	.LBB6_31
# BB#30:                                # %if.then32
	movl	%eax, 8(%r14)
.LBB6_31:                               # %if.end34
	movq	40(%rbx), %rdi
	leaq	44(%rsp), %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	get_one_coefficient
	movl	44(%rsp), %ecx
	cmpq	$25, %rcx
	jne	.LBB6_39
# BB#32:                                # %if.then41
	movl	%eax, 8(%r14)
	jmp	.LBB6_39
.LBB6_10:                               # %if.end
	cmpq	$61, %rax
	jne	.LBB6_39
# BB#11:                                # %if.then69
	movq	32(%rbx), %rdi
	leaq	44(%rsp), %r13
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	get_one_coefficient
	movq	40(%rbx), %rdi
	movq	%rax, %rbx
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	get_one_coefficient
	movl	44(%rsp), %ecx
	cmpq	$34, %rcx
	jne	.LBB6_37
# BB#12:                                # %if.then87
	movl	%eax, 4(%r14)
	jmp	.LBB6_39
.LBB6_7:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB6_39
.LBB6_37:                               # %if.else89
	movl	44(%rsp), %eax
	cmpq	$34, %rax
	jne	.LBB6_39
# BB#38:                                # %if.then91
	movl	%ebx, 4(%r14)
	jmp	.LBB6_39
.LBB6_23:                               # %have_induction_variable.exit
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB6_39
# BB#24:                                # %if.then3
	movl	%r12d, (%r14)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r14)
	movq	$1, %rax
	movl	%eax, 4(%r14)
	movl	8(%r14), %eax
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	cmpq	%rcx, %rax
	jne	.LBB6_39
# BB#25:                                # %if.then10
	xorq	%rax, %rax
	movl	%eax, 8(%r14)
	jmp	.LBB6_39
.LBB6_15:
	movq	%r15, %r12
.LBB6_39:                               # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp59:
	.size	get_one_coefficient, .Ltmp59-get_one_coefficient
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"def_use_chain"
	.size	.L.str, 14

	.type	def_use_chain,@object   # @def_use_chain
	.local	def_use_chain
	.comm	def_use_chain,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"dep_chain"
	.size	.L.str1, 10

	.type	dep_chain,@object       # @dep_chain
	.local	dep_chain
	.comm	dep_chain,8,8
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"loop_chain"
	.size	.L.str2, 11

	.type	loop_chain,@object      # @loop_chain
	.local	loop_chain
	.comm	loop_chain,8,8
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"induction_chain"
	.size	.L.str3, 16

	.type	induction_chain,@object # @induction_chain
	.local	induction_chain
	.comm	induction_chain,8,8
	.type	dest_to_remember,@object # @dest_to_remember
	.local	dest_to_remember
	.comm	dest_to_remember,8,8
	.type	merge_dependencies.direction_merge,@object # @merge_dependencies.direction_merge
	.section	.rodata,"a",@progbits
	.align	16
merge_dependencies.direction_merge:
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	0                       # 0x0
	.long	6                       # 0x6
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	1                       # 0x1
	.long	6                       # 0x6
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	2                       # 0x2
	.long	6                       # 0x6
	.long	2                       # 0x2
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	4                       # 0x4
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	3                       # 0x3
	.long	6                       # 0x6
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	6                       # 0x6
	.long	4                       # 0x4
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	6                       # 0x6
	.long	5                       # 0x5
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.size	merge_dependencies.direction_merge, 256

	.type	build_def_use.outer_loop,@object # @build_def_use.outer_loop
	.local	build_def_use.outer_loop
	.comm	build_def_use.outer_loop,8,8
	.type	build_def_use.nloop,@object # @build_def_use.nloop
	.local	build_def_use.nloop
	.comm	build_def_use.nloop,4,4
	.type	build_def_use.current_loop,@object # @build_def_use.current_loop
	.local	build_def_use.current_loop
	.comm	build_def_use.current_loop,8,8
	.type	build_def_use.du_idx,@object # @build_def_use.du_idx
	.local	build_def_use.du_idx
	.comm	build_def_use.du_idx,4,4
	.type	build_def_use.loop_def,@object # @build_def_use.loop_def
	.local	build_def_use.loop_def
	.comm	build_def_use.loop_def,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
