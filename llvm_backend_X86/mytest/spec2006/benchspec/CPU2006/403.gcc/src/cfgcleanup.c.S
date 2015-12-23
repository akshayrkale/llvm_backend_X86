	.file	"cfgcleanup.c.bc"
	.text
	.globl	cleanup_cfg
	.align	16, 0x90
	.type	cleanup_cfg,@function
cleanup_cfg:                            # @cleanup_cfg
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
	subq	$240, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 288
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
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	$4, %rdi
	movq	$4, %r15
	callq	timevar_push
	callq	find_unreachable_blocks
	movslq	n_basic_blocks(%rip), %rax
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	testq	%rax, %rax
	jle	.LBB0_5
	.align	16, 0x90
.LBB0_1:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%r15,8), %rbx
	movzbl	108(%rbx), %ecx
	testq	$1, %rcx
	jne	.LBB0_3
# BB#2:                                 # %if.then.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rbx, %rdi
	callq	flow_delete_block_noexpunge
	movq	%rbx, %rdi
	callq	expunge_block_nocompact
	movq	$1, %rsi
	jmp	.LBB0_4
	.align	16, 0x90
.LBB0_3:                                # %if.else.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rbx, 32(%rax,%rdx,8)
	movl	%r14d, 88(%rbx)
	incq	%r14
.LBB0_4:                                # %for.inc.i
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%r15), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rax, %rdx
	jl	.LBB0_1
.LBB0_5:                                # %for.end.i
	movl	%r14d, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	basic_block_info(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, (%rax)
	andq	$1, %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	je	.LBB0_7
# BB#6:                                 # %if.then7.i
	callq	tidy_fallthru_edges
.LBB0_7:                                # %delete_unreachable_blocks.exit
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	$2, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	je	.LBB0_9
# BB#8:                                 # %if.then.i5
	callq	add_noreturn_fake_exit_edges
.LBB0_9:                                # %for.cond.preheader.i
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_14
# BB#10:
	movq	$4, %rbx
	movabsq	$4294967293, %r15       # imm = 0xFFFFFFFD
	.align	16, 0x90
.LBB0_11:                               # %for.body.i10
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %r14
	movq	%r14, %rdi
	callq	forwarder_block_p
	movq	80(%r14), %rcx
	andq	%r15, %rcx
	testq	%rax, %rax
	je	.LBB0_13
# BB#12:                                #   in Loop: Header=BB0_11 Depth=1
	orq	$2, %rcx
.LBB0_13:                               # %for.body.i10
                                        #   in Loop: Header=BB0_11 Depth=1
	movq	%rcx, 80(%r14)
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB0_11
.LBB0_14:                               # %for.end.i12
	xorq	%rbx, %rbx
	callq	*targetm+336(%rip)
	testq	%rax, %rax
	jne	.LBB0_328
# BB#15:                                # %do.body.preheader.i
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rcx
	movq	%rax, %rdx
	movq	%rax, %rsi
	movq	%rax, %rdi
	movabsq	$.L.str1, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	andq	$8, %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	andq	$1, %rdx
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	andq	$64, %rsi
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	andq	$32, %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	andq	$16, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB0_16:                               # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_18 Depth 2
                                        #       Child Loop BB0_19 Depth 3
                                        #         Child Loop BB0_21 Depth 4
                                        #         Child Loop BB0_30 Depth 4
                                        #         Child Loop BB0_46 Depth 4
                                        #         Child Loop BB0_66 Depth 4
                                        #           Child Loop BB0_76 Depth 5
                                        #           Child Loop BB0_91 Depth 5
                                        #           Child Loop BB0_96 Depth 5
                                        #         Child Loop BB0_156 Depth 4
                                        #           Child Loop BB0_157 Depth 5
                                        #             Child Loop BB0_162 Depth 6
                                        #               Child Loop BB0_191 Depth 7
                                        #               Child Loop BB0_198 Depth 7
                                        #               Child Loop BB0_203 Depth 7
                                        #                 Child Loop BB0_207 Depth 8
                                        #               Child Loop BB0_214 Depth 7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
                                        #               Child Loop BB0_240 Depth 7
                                        #               Child Loop BB0_256 Depth 7
                                        #           Child Loop BB0_282 Depth 5
                                        #             Child Loop BB0_301 Depth 6
	movq	rtl_dump_file(%rip), %rdi
	incq	40(%rsp)                # 8-byte Folded Spill
	testq	%rdi, %rdi
	movabsq	$0, %rbx
	movabsq	$0, %r12
	je	.LBB0_18
# BB#17:                                # %if.then4.i
                                        #   in Loop: Header=BB0_16 Depth=1
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movabsq	$.L.str, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	fprintf
	xorq	%r12, %r12
	jmp	.LBB0_18
	.align	16, 0x90
.LBB0_321:                              # %if.then185.i
                                        #   in Loop: Header=BB0_18 Depth=2
	movl	88(%r13), %r12d
	incq	%r12
	movq	112(%rsp), %rbx         # 8-byte Reload
.LBB0_18:                               # %for.cond7.outer.i
                                        #   Parent Loop BB0_16 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_19 Depth 3
                                        #         Child Loop BB0_21 Depth 4
                                        #         Child Loop BB0_30 Depth 4
                                        #         Child Loop BB0_46 Depth 4
                                        #         Child Loop BB0_66 Depth 4
                                        #           Child Loop BB0_76 Depth 5
                                        #           Child Loop BB0_91 Depth 5
                                        #           Child Loop BB0_96 Depth 5
                                        #         Child Loop BB0_156 Depth 4
                                        #           Child Loop BB0_157 Depth 5
                                        #             Child Loop BB0_162 Depth 6
                                        #               Child Loop BB0_191 Depth 7
                                        #               Child Loop BB0_198 Depth 7
                                        #               Child Loop BB0_203 Depth 7
                                        #                 Child Loop BB0_207 Depth 8
                                        #               Child Loop BB0_214 Depth 7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
                                        #               Child Loop BB0_240 Depth 7
                                        #               Child Loop BB0_256 Depth 7
                                        #           Child Loop BB0_282 Depth 5
                                        #             Child Loop BB0_301 Depth 6
	movq	%r12, 128(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB0_19:                               # %for.cond7.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_21 Depth 4
                                        #         Child Loop BB0_30 Depth 4
                                        #         Child Loop BB0_46 Depth 4
                                        #         Child Loop BB0_66 Depth 4
                                        #           Child Loop BB0_76 Depth 5
                                        #           Child Loop BB0_91 Depth 5
                                        #           Child Loop BB0_96 Depth 5
                                        #         Child Loop BB0_156 Depth 4
                                        #           Child Loop BB0_157 Depth 5
                                        #             Child Loop BB0_162 Depth 6
                                        #               Child Loop BB0_191 Depth 7
                                        #               Child Loop BB0_198 Depth 7
                                        #               Child Loop BB0_203 Depth 7
                                        #                 Child Loop BB0_207 Depth 8
                                        #               Child Loop BB0_214 Depth 7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
                                        #               Child Loop BB0_240 Depth 7
                                        #               Child Loop BB0_256 Depth 7
                                        #           Child Loop BB0_282 Depth 5
                                        #             Child Loop BB0_301 Depth 6
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB0_322
# BB#20:                                # %for.body9.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	basic_block_info(%rip), %rsi
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	32(%rsi,%rax,8), %r12
	movq	32(%r12), %rax
	testq	%rax, %rax
	jne	.LBB0_25
	jmp	.LBB0_21
	.align	16, 0x90
.LBB0_358:                              # %if.end23.while.body_crit_edge.i
                                        #   in Loop: Header=BB0_21 Depth=4
	movq	basic_block_info(%rip), %rsi
.LBB0_21:                               #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r12, %rbx
	movl	88(%rbx), %edx
	leaq	-1(%rdx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	rtl_dump_file(%rip), %rdi
	movq	32(%rsi,%rax,8), %r12
	testq	%rdi, %rdi
	je	.LBB0_23
# BB#22:                                # %if.then20.i
                                        #   in Loop: Header=BB0_21 Depth=4
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.LBB0_23:                               # %if.end23.i
                                        #   in Loop: Header=BB0_21 Depth=4
	movq	%rbx, %rdi
	callq	flow_delete_block
	movq	32(%r12), %rax
	testq	%rax, %rax
	je	.LBB0_358
# BB#24:                                #   in Loop: Header=BB0_19 Depth=3
	movq	$1, %rbx
.LBB0_25:                               #   in Loop: Header=BB0_19 Depth=3
	leaq	32(%r12), %r14
	cmpq	$0, (%rax)
	jne	.LBB0_47
# BB#26:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movl	48(%rax), %ecx
	andq	$15, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_47
# BB#27:                                # %land.lhs.true35.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	(%r12), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$36, %rcx
	jne	.LBB0_47
# BB#28:                                # %land.lhs.true37.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	176(%rsp), %rcx         # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB0_33
# BB#29:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	tail_recursion_label_list(%rip), %rcx
	jmp	.LBB0_30
	.align	16, 0x90
.LBB0_32:                               # %for.inc.i.i
                                        #   in Loop: Header=BB0_30 Depth=4
	movq	16(%rcx), %rcx
.LBB0_30:                               # %lor.lhs.false.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rcx, %rcx
	je	.LBB0_33
# BB#31:                                # %for.body.i.i
                                        #   in Loop: Header=BB0_30 Depth=4
	movq	8(%rcx), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB0_32
	jmp	.LBB0_47
.LBB0_33:                               # %land.lhs.true42.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	16(%rax), %rax
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	je	.LBB0_43
# BB#34:                                # %lor.lhs.false45.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB0_43
# BB#35:                                # %lor.lhs.false51.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	64(%rax), %rax
	cmpq	%rdi, %rax
	je	.LBB0_47
# BB#36:                                # %if.end.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	testq	%rax, %rax
	je	.LBB0_43
# BB#37:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_43
# BB#38:                                # %land.lhs.true6.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB0_43
# BB#39:                                # %land.lhs.true8.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	32(%rax), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	jne	.LBB0_43
# BB#40:                                # %if.then18.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$45, %rcx
	movabsq	$0, %rcx
	jne	.LBB0_42
# BB#41:                                # %if.then18.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movabsq	$1, %rcx
.LBB0_42:                               # %if.then18.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	8(%rax,%rcx,8), %rax
	movslq	(%rax), %r8
	testq	%r8, %r8
	movq	$1, %rdx
	jle	.LBB0_43
.LBB0_46:                               # %for.body.i346.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%rax,%rdx,8), %rcx
	movq	8(%rcx), %rcx
	cmpq	%rdi, %rcx
	je	.LBB0_47
# BB#45:                                # %for.cond.i.i
                                        #   in Loop: Header=BB0_46 Depth=4
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%r8, %rsi
	jl	.LBB0_46
.LBB0_43:                               # %if.then57.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	24(%rdi), %rax
	movq	%rax, (%r12)
	movq	%rdi, %rsi
	callq	delete_insn_chain
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_47
# BB#44:                                # %if.then63.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movl	88(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str2, %rsi
	callq	fprintf
	.align	16, 0x90
.LBB0_47:                               # %if.end67.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	(%r14), %rax
	cmpq	$0, (%rax)
	jne	.LBB0_48
# BB#49:                                # %land.lhs.true71.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movzbl	48(%rax), %eax
	testq	$1, %rax
	jne	.LBB0_51
# BB#50:                                #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	jmp	.LBB0_64
	.align	16, 0x90
.LBB0_48:                               #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	jmp	.LBB0_64
	.align	16, 0x90
.LBB0_51:                               # %land.lhs.true76.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$36, %rax
	jne	.LBB0_53
# BB#52:                                #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	jmp	.LBB0_64
.LBB0_53:                               # %land.lhs.true81.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movzbl	80(%r12), %eax
	testq	$2, %rax
	jne	.LBB0_55
# BB#54:                                #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	jmp	.LBB0_64
.LBB0_55:                               # %land.lhs.true84.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rax
	movl	48(%rax), %eax
	testq	$1, %rax
	je	.LBB0_56
# BB#57:                                # %land.lhs.true84.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movl	n_basic_blocks(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_58
# BB#59:                                # %if.then90.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_61
# BB#60:                                # %if.then92.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movl	88(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	callq	fprintf
.LBB0_61:                               # %if.end95.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movl	88(%r12), %eax
	testq	%rax, %rax
	movq	$1, 112(%rsp)           # 8-byte Folded Spill
	movq	$1, %rcx
	je	.LBB0_63
# BB#62:                                # %cond.true.i
                                        #   in Loop: Header=BB0_19 Depth=3
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, %rcx
.LBB0_63:                               # %cond.end.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	basic_block_info(%rip), %rax
	movq	40(%r12), %rdx
	movq	32(%rax,%rcx,8), %rbx
	movq	(%r14), %rdi
	movq	24(%rdx), %rsi
	callq	redirect_edge_succ_nodup
	movq	%r12, %rdi
	callq	flow_delete_block
	movq	%rbx, %r12
	jmp	.LBB0_64
.LBB0_56:                               #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	jmp	.LBB0_64
.LBB0_58:                               #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB0_64:                               # %while.cond109.preheader.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rbx
	movq	$0, 224(%rsp)           # 8-byte Folded Spill
	testq	%rbx, %rbx
	je	.LBB0_130
# BB#65:                                # %land.lhs.true112.lr.ph.i
                                        #   in Loop: Header=BB0_19 Depth=3
	leaq	8(%r12), %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
	movq	$0, 224(%rsp)           # 8-byte Folded Spill
.LBB0_66:                               # %land.lhs.true112.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_76 Depth 5
                                        #           Child Loop BB0_91 Depth 5
                                        #           Child Loop BB0_96 Depth 5
	cmpq	$0, 8(%rbx)
	jne	.LBB0_130
# BB#67:                                # %land.lhs.true114.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movzbl	48(%rbx), %eax
	testq	$14, %rax
	jne	.LBB0_130
# BB#68:                                # %land.lhs.true118.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	24(%rbx), %r13
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r13
	je	.LBB0_130
# BB#69:                                # %land.lhs.true121.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	32(%r13), %rax
	cmpq	$0, (%rax)
	jne	.LBB0_130
# BB#70:                                # %land.lhs.true121.i
                                        #   in Loop: Header=BB0_66 Depth=4
	cmpq	%r13, %r12
	je	.LBB0_130
# BB#71:                                # %land.lhs.true127.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB0_73
# BB#72:                                # %lor.lhs.false132.i
                                        #   in Loop: Header=BB0_66 Depth=4
	callq	onlyjump_p
	testq	%r15, %rax
	je	.LBB0_130
.LBB0_73:                               # %land.rhs.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	testq	%r15, %rax
	je	.LBB0_79
# BB#74:                                # %land.lhs.true.i352.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB0_79
# BB#75:                                # %land.lhs.true1.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	tail_recursion_label_list(%rip), %rcx
	jmp	.LBB0_76
	.align	16, 0x90
.LBB0_78:                               # %for.inc.i.i.i
                                        #   in Loop: Header=BB0_76 Depth=5
	movq	16(%rcx), %rcx
.LBB0_76:                               # %land.lhs.true1.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_66 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	testq	%rcx, %rcx
	je	.LBB0_79
# BB#77:                                # %for.body.i.i.i
                                        #   in Loop: Header=BB0_76 Depth=5
	movq	8(%rcx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB0_78
	jmp	.LBB0_130
	.align	16, 0x90
.LBB0_79:                               # %if.end.i353.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movzbl	48(%rbx), %eax
	testq	$1, %rax
	jne	.LBB0_80
# BB#87:                                # %if.else.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	cmpq	$0, 168(%rsp)           # 8-byte Folded Reload
	je	.LBB0_147
# BB#88:                                # %if.then22.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movzbl	80(%r12), %eax
	testq	$2, %rax
	jne	.LBB0_131
# BB#89:                                # %lor.lhs.false26.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movzbl	80(%r13), %eax
	testq	$2, %rax
	jne	.LBB0_131
# BB#90:                                # %if.end31.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	40(%r13), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	movabsq	$0, %rax
	je	.LBB0_94
	.align	16, 0x90
.LBB0_91:                               # %for.body.i358.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_66 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movzbl	48(%rcx), %edx
	movq	$1, %rax
	testq	$1, %rdx
	jne	.LBB0_94
# BB#92:                                # %for.cond.i357.i
                                        #   in Loop: Header=BB0_91 Depth=5
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_91
# BB#93:                                #   in Loop: Header=BB0_66 Depth=4
	xorq	%rax, %rax
.LBB0_94:                               # %for.end.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	32(%r12), %rcx
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB0_98
	.align	16, 0x90
.LBB0_96:                               # %for.body42.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_66 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movzbl	48(%rcx), %esi
	movq	$1, %rdx
	testq	$1, %rsi
	jne	.LBB0_97
# BB#95:                                # %for.cond40.i.i
                                        #   in Loop: Header=BB0_96 Depth=5
	movq	(%rcx), %rcx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	jne	.LBB0_96
	jmp	.LBB0_98
	.align	16, 0x90
.LBB0_80:                               # %if.then5.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%r12, %rbx
	movl	88(%rbx), %r12d
	movzbl	80(%r13), %eax
	movl	88(%r13), %edx
	testq	$1, %rax
	jne	.LBB0_82
# BB#81:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$33, %rax
	jne	.LBB0_83
.LBB0_82:                               # %if.then12.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	80(%rbx), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, 80(%rbx)
.LBB0_83:                               # %if.end15.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%rdx, 224(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	merge_blocks_nomove
	movq	%rbx, %rdi
	callq	forwarder_block_p
	movq	80(%rbx), %rcx
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	andq	%rdx, %rcx
	testq	%rax, %rax
	je	.LBB0_85
# BB#84:                                #   in Loop: Header=BB0_66 Depth=4
	orq	$2, %rcx
.LBB0_85:                               # %if.end15.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%rbx, %r14
	movq	%rcx, 80(%r14)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_129
# BB#86:                                # %if.then17.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	xorq	%rax, %rax
	movabsq	$.L.str13, %rsi
	movq	%r12, %rdx
	movq	224(%rsp), %rcx         # 8-byte Reload
	callq	fprintf
	jmp	.LBB0_129
.LBB0_97:                               #   in Loop: Header=BB0_66 Depth=4
	movq	%rcx, %rbx
.LBB0_98:                               # %for.end49.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%r12, %r14
	testq	%rax, %rax
	je	.LBB0_99
# BB#114:                               # %if.end55.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%r15, %r12
	testq	%rdx, %rdx
	je	.LBB0_120
# BB#115:                               # %if.then57.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	16(%rbx), %rax
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	movq	%r12, %r15
	movq	%r14, %r12
	je	.LBB0_130
# BB#116:                               # %if.end61.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%r12, %r14
	movq	%r15, %r12
	movq	%rbx, %rdi
	callq	force_nonfallthru
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB0_117
# BB#119:                               # %if.else65.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	16(%rbx), %rax
	movq	80(%rax), %rcx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rdx, %rcx
	orq	$1, %rcx
	movq	%rcx, 80(%rax)
	jmp	.LBB0_120
.LBB0_99:                               # %if.then54.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	8(%r13), %rbx
	movq	24(%rbx), %rdi
	leaq	8(%r13), %r12
	testq	%rdi, %rdi
	je	.LBB0_109
# BB#100:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB0_107
# BB#101:                               # %land.lhs.true2.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	24(%rdi), %rax
	testq	%rax, %rax
	je	.LBB0_105
# BB#102:                               # %land.lhs.true7.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB0_105
# BB#103:                               # %land.lhs.true14.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	32(%rax), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	jne	.LBB0_105
# BB#104:                               # %if.then.i120.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%rax, (%r12)
	movq	24(%rax), %rdi
.LBB0_105:                              # %if.end.i121.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	testq	%rdi, %rdi
	je	.LBB0_109
# BB#106:                               # %if.end.land.lhs.true42_crit_edge.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movl	(%rdi), %eax
.LBB0_107:                              # %land.lhs.true42.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$35, %rax
	jne	.LBB0_109
# BB#108:                               # %if.then46.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	callq	delete_insn
.LBB0_109:                              # %if.end47.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	squeeze_notes
	testq	%rax, %rax
	jne	.LBB0_110
# BB#112:                               # %if.end51.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	(%r13), %rdi
	movq	8(%r13), %rsi
	movq	8(%r14), %rdx
	callq	reorder_insns_nobb
	movq	%rbx, 8(%r13)
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	merge_blocks_nomove
	movq	80(%r14), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, 80(%r14)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_129
# BB#113:                               # %if.then58.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movl	88(%r14), %ecx
	movl	88(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str15, %rsi
	callq	fprintf
	jmp	.LBB0_129
.LBB0_117:                              # %if.end.i132.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	80(%r15), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, 80(%r15)
	movq	%r15, %rdi
	callq	forwarder_block_p
	testq	%rax, %rax
	je	.LBB0_120
# BB#118:                               # %if.then2.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	80(%r15), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	andq	%rcx, %rax
	orq	$2, %rax
	movq	%rax, 80(%r15)
.LBB0_120:                              # %if.end73.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	(%rbx), %rdi
	callq	next_nonnote_insn
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	jne	.LBB0_121
# BB#122:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	%rax, %rdi
	callq	delete_insn
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	squeeze_notes
	testq	%rax, %rax
	jne	.LBB0_123
# BB#124:                               # %if.end5.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	(%r13), %rax
	movq	(%rbx), %rsi
	movq	16(%rax), %rdx
	cmpq	%rdx, %rsi
	movq	%r12, %r15
	movq	%r14, %rbx
	je	.LBB0_126
# BB#125:                               # %if.then9.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	(%rbx), %rdi
	callq	reorder_insns_nobb
.LBB0_126:                              # %if.end16.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	80(%rbx), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, 80(%rbx)
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_128
# BB#127:                               # %if.then18.i.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movl	88(%r13), %ecx
	movl	88(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str14, %rsi
	callq	fprintf
.LBB0_128:                              # %merge_blocks_move_predecessor_nojumps.exit.i.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movslq	88(%rbx), %rax
	movq	basic_block_info(%rip), %rcx
	movq	%r13, 32(%rcx,%rax,8)
	movslq	88(%r13), %rcx
	movq	basic_block_info(%rip), %rdx
	movq	%rbx, 32(%rdx,%rcx,8)
	movl	%ecx, 88(%rbx)
	movl	%eax, 88(%r13)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	merge_blocks_nomove
	movq	%rbx, %r14
.LBB0_129:                              # %while.cond109.backedge.i
                                        #   in Loop: Header=BB0_66 Depth=4
	movq	40(%r14), %rbx
	testq	%rbx, %rbx
	movq	$1, 224(%rsp)           # 8-byte Folded Spill
	movq	%r14, %r12
	jne	.LBB0_66
	.align	16, 0x90
.LBB0_130:                              # %while.end138.i
                                        #   in Loop: Header=BB0_19 Depth=3
	cmpq	$0, 168(%rsp)           # 8-byte Folded Reload
	je	.LBB0_147
.LBB0_131:                              # %land.lhs.true141.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB0_155
# BB#132:                               # %lor.lhs.false.i363.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_147
# BB#133:                               # %lor.lhs.false3.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	cmpq	$0, 8(%rax)
	jne	.LBB0_147
# BB#134:                               # %if.end.i368.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	8(%r12), %r13
	movq	%r13, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB0_147
# BB#135:                               # %if.end10.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rbx
	movzbl	48(%rbx), %ecx
	movq	8(%rbx), %rax
	andq	$1, %rcx
	movq	%rax, %rdx
	je	.LBB0_137
# BB#136:                               # %if.end10.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, %rdx
.LBB0_137:                              # %if.end10.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	testq	%rcx, %rcx
	je	.LBB0_139
# BB#138:                               # %if.end10.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rax, %rbx
.LBB0_139:                              # %if.end10.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	24(%rdx), %r14
	movq	32(%r14), %rax
	cmpq	$0, (%rax)
	jne	.LBB0_147
# BB#140:                               # %lor.lhs.false28.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rdx, 232(%rsp)         # 8-byte Spill
	movl	n_basic_blocks(%rip), %eax
	movl	88(%r14), %ecx
	decq	%rax
	andq	%r15, %rax
	cmpq	%rax, %rcx
	je	.LBB0_147
# BB#141:                               # %lor.lhs.false29.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movzbl	80(%r14), %eax
	testq	$2, %rax
	je	.LBB0_147
# BB#142:                               # %if.end33.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r14), %rax
	movq	24(%rax), %rax
	movq	%rax, 208(%rsp)         # 8-byte Spill
	movq	24(%rbx), %rsi
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	callq	can_fallthru
	testq	%rax, %rax
	je	.LBB0_147
# BB#143:                               # %if.end39.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	block_label
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	invert_jump
	testq	%r15, %rax
	je	.LBB0_147
# BB#144:                               # %if.end44.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_146
# BB#145:                               # %if.then46.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	8(%r14), %rax
	movl	8(%r13), %edx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str12, %rsi
	callq	fprintf
.LBB0_146:                              # %if.then143.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, %rdi
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	redirect_edge_succ_nodup
	movq	%rax, %rbx
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	redirect_edge_succ_nodup
	movl	48(%rbx), %ecx
	orq	$1, %rcx
	movl	%ecx, 48(%rbx)
	movl	48(%rax), %ecx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	movq	%rdx, %r13
	andq	%r13, %rcx
	movl	%ecx, 48(%rax)
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	update_br_prob_note
	movq	%r14, %rdi
	callq	flow_delete_block
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	200(%rsp), %rdx         # 8-byte Reload
	callq	tidy_fallthru_edge
	movq	80(%rbx), %rax
	andq	%r13, %rax
	orq	$1, %rax
	movq	%rax, 80(%rbx)
	movq	%rbx, %r12
	movq	$1, 224(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_147:                              # %if.end146.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB0_155
# BB#148:                               # %land.lhs.true149.i
                                        #   in Loop: Header=BB0_19 Depth=3
	cmpq	$0, 8(%rax)
	jne	.LBB0_155
# BB#149:                               # %land.lhs.true153.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	24(%rax), %rax
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	je	.LBB0_155
# BB#150:                               # %land.lhs.true158.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	8(%r12), %rdi
	callq	onlyjump_p
	testq	%r15, %rax
	je	.LBB0_155
# BB#151:                               # %land.lhs.true162.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rdi
	movq	24(%rdi), %rsi
	callq	redirect_edge_and_branch
	testq	%rax, %rax
	je	.LBB0_155
# BB#152:                               # %if.then168.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	80(%r12), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, 80(%r12)
	movq	%r12, %rdi
	callq	forwarder_block_p
	movq	80(%r12), %rcx
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	andq	%rdx, %rcx
	testq	%rax, %rax
	je	.LBB0_154
# BB#153:                               #   in Loop: Header=BB0_19 Depth=3
	orq	$2, %rcx
.LBB0_154:                              # %if.then168.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rcx, 80(%r12)
	movq	$1, 224(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_155:                              # %if.end173.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	40(%r12), %rbx
	xorq	%r14, %r14
	xorq	%rax, %rax
	jmp	.LBB0_156
.LBB0_311:                              #   in Loop: Header=BB0_156 Depth=4
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	200(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB0_156:                              # %for.cond.outer.outer.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_157 Depth 5
                                        #             Child Loop BB0_162 Depth 6
                                        #               Child Loop BB0_191 Depth 7
                                        #               Child Loop BB0_198 Depth 7
                                        #               Child Loop BB0_203 Depth 7
                                        #                 Child Loop BB0_207 Depth 8
                                        #               Child Loop BB0_214 Depth 7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
                                        #               Child Loop BB0_240 Depth 7
                                        #               Child Loop BB0_256 Depth 7
                                        #           Child Loop BB0_282 Depth 5
                                        #             Child Loop BB0_301 Depth 6
	movq	%rax, 184(%rsp)         # 8-byte Spill
	jmp	.LBB0_157
.LBB0_267:                              # %if.then117.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movl	88(%r10), %edx
	xorq	%rax, %rax
	movabsq	$.L.str9, %rsi
	callq	fprintf
	.align	16, 0x90
.LBB0_157:                              # %for.cond.i389.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB0_162 Depth 6
                                        #               Child Loop BB0_191 Depth 7
                                        #               Child Loop BB0_198 Depth 7
                                        #               Child Loop BB0_203 Depth 7
                                        #                 Child Loop BB0_207 Depth 8
                                        #               Child Loop BB0_214 Depth 7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
                                        #               Child Loop BB0_240 Depth 7
                                        #               Child Loop BB0_256 Depth 7
	movq	%rbx, %r13
	testq	%r13, %r13
	je	.LBB0_312
# BB#158:                               # %for.body.i393.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movzbl	48(%r13), %eax
	movq	8(%r13), %rbx
	testq	$14, %rax
	jne	.LBB0_157
# BB#159:                               # %if.end.i395.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	%r12, 216(%rsp)         # 8-byte Spill
	movq	%r15, %r12
	movslq	n_basic_blocks(%rip), %rax
	movq	24(%r13), %r9
	movq	%r9, 208(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	testq	%rax, %rax
	jle	.LBB0_160
# BB#161:                               # %while.body.lr.ph.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r9, %rdx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
.LBB0_162:                              # %while.body.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_191 Depth 7
                                        #               Child Loop BB0_198 Depth 7
                                        #               Child Loop BB0_203 Depth 7
                                        #                 Child Loop BB0_207 Depth 8
                                        #               Child Loop BB0_214 Depth 7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
                                        #               Child Loop BB0_240 Depth 7
                                        #               Child Loop BB0_256 Depth 7
	movzbl	80(%rdx), %ecx
	testq	$2, %rcx
	je	.LBB0_167
# BB#163:                               # %land.lhs.true.i397.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	40(%rdx), %rcx
	movq	24(%rcx), %r10
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %r10
	jne	.LBB0_164
.LBB0_167:                              # %if.else.i398.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r14, %r15
	movq	%rdx, %r14
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB0_168
# BB#169:                               # %if.then17.i400.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	16(%r13), %rax
	movq	40(%rax), %rcx
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB0_168
# BB#170:                               # %lor.lhs.false.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	cmpq	$0, 8(%rcx)
	jne	.LBB0_168
# BB#171:                               # %if.end.i.i402.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	40(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB0_168
# BB#172:                               # %lor.lhs.false8.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB0_168
# BB#173:                               # %lor.lhs.false12.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	cmpq	$0, 8(%rcx)
	jne	.LBB0_168
# BB#174:                               # %if.end18.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r8, 152(%rsp)          # 8-byte Spill
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	movq	8(%rax), %rdi
	callq	any_condjump_p
	testq	%r12, %rax
	je	.LBB0_175
# BB#176:                               # %lor.lhs.false21.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	8(%r14), %rdi
	callq	any_condjump_p
	testq	%r12, %rax
	je	.LBB0_175
# BB#177:                               # %lor.lhs.false25.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	8(%r14), %rdi
	callq	onlyjump_p
	testq	%r12, %rax
	je	.LBB0_175
# BB#178:                               # %if.end30.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	movq	%r15, 192(%rsp)         # 8-byte Spill
	movq	16(%r13), %rax
	movq	8(%rax), %rdi
	callq	pc_set
	movq	8(%r14), %rdi
	movq	%r14, 232(%rsp)         # 8-byte Spill
	movq	%rax, %rbx
	callq	pc_set
	movq	16(%rbx), %rcx
	movq	16(%rcx), %rdx
	movq	global_rtl(%rip), %rsi
	movq	8(%rcx), %r14
	movq	%rax, %rcx
	cmpq	%rsi, %rdx
	movabsq	$0, %rax
	jne	.LBB0_180
# BB#179:                               # %if.end30.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movabsq	$1, %rax
.LBB0_180:                              # %if.end30.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	16(%rcx), %rcx
	movzbl	48(%r13), %edx
	movq	8(%rcx), %rbx
	andq	$1, %rdx
	xorq	%rax, %rdx
	cmpq	$1, %rdx
	jne	.LBB0_182
# BB#181:                               # %if.then58.i.i407.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	16(%r13), %rax
	movq	8(%rax), %rsi
	movq	%r14, %rdi
	callq	reversed_comparison_code
	movq	%rax, %r15
	jmp	.LBB0_183
	.align	16, 0x90
.LBB0_182:                              # %if.else.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movzbl	(%r14), %r15d
.LBB0_183:                              # %if.end62.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movzbl	(%rbx), %r12d
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	callq	reversed_comparison_code
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	comparison_dominates_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB0_187
# BB#184:                               # %land.lhs.true.i.i410.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r15, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	comparison_dominates_p
	testq	%r12, %rax
	je	.LBB0_185
.LBB0_187:                              # %if.end72.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	8(%r14), %rdi
	movq	8(%rbx), %rsi
	callq	rtx_equal_p
	testq	%r12, %rax
	movq	%r12, %r15
	je	.LBB0_188
# BB#189:                               # %lor.lhs.false81.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	16(%r14), %rdi
	movq	16(%rbx), %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB0_188
# BB#190:                               # %if.end91.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	232(%rsp), %r14         # 8-byte Reload
	movq	(%r14), %rcx
	movq	8(%r14), %rax
	movq	24(%rcx), %rbx
	jmp	.LBB0_191
	.align	16, 0x90
.LBB0_196:                              # %for.cond.backedge.i.i.i
                                        #   in Loop: Header=BB0_191 Depth=7
	movq	24(%rbx), %rbx
.LBB0_191:                              # %if.end91.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	movq	24(%rax), %rcx
	cmpq	%rcx, %rbx
	je	.LBB0_197
# BB#192:                               # %for.body.i.i412.i
                                        #   in Loop: Header=BB0_191 Depth=7
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_196
# BB#193:                               # %land.lhs.true107.i.i.i
                                        #   in Loop: Header=BB0_191 Depth=7
	movq	32(%rbx), %rdi
	callq	side_effects_p
	testq	%r15, %rax
	jne	.LBB0_194
# BB#195:                               # %land.lhs.true107.for.cond.backedge_crit_edge.i.i.i
                                        #   in Loop: Header=BB0_191 Depth=7
	movq	8(%r14), %rax
	jmp	.LBB0_196
	.align	16, 0x90
.LBB0_197:                              # %for.end.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	callq	cselib_init
	movq	16(%r13), %rax
	movq	(%rax), %rcx
	movq	8(%rax), %rdx
	movq	24(%rcx), %rbx
	movq	24(%rdx), %rcx
	jmp	.LBB0_198
	.align	16, 0x90
.LBB0_200:                              # %for.cond123.backedge.i.i.i
                                        #   in Loop: Header=BB0_198 Depth=7
	movq	8(%rax), %rcx
	movq	24(%rbx), %rbx
	movq	24(%rcx), %rcx
.LBB0_198:                              # %for.end.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	cmpq	%rcx, %rbx
	je	.LBB0_202
# BB#199:                               # %for.body131.i.i.i
                                        #   in Loop: Header=BB0_198 Depth=7
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_200
# BB#201:                               # %if.then139.i.i.i
                                        #   in Loop: Header=BB0_198 Depth=7
	movq	%rbx, %rdi
	callq	cselib_process_insn
	movq	16(%r13), %rax
	jmp	.LBB0_200
	.align	16, 0x90
.LBB0_202:                              # %for.end145.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, %rdi
	movq	%rax, %r13
	callq	bitmap_clear
	movq	(%r14), %rax
	movq	8(%r14), %rcx
	movq	24(%rax), %rbx
	movq	24(%rcx), %rax
	xorq	%r14, %r14
	cmpq	%rax, %rbx
	je	.LBB0_211
	.align	16, 0x90
.LBB0_203:                              # %land.rhs.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        # =>            This Loop Header: Depth=7
                                        #                 Child Loop BB0_207 Depth 8
	testq	$1, %r14
	jne	.LBB0_210
# BB#204:                               # %for.body160.i.i.i
                                        #   in Loop: Header=BB0_203 Depth=7
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_209
# BB#205:                               # %if.then168.i.i.i
                                        #   in Loop: Header=BB0_203 Depth=7
	movq	32(%rbx), %r15
	movzbl	(%r15), %eax
	cmpq	$39, %rax
	jne	.LBB0_208
# BB#206:                               # %for.cond177.preheader.i.i.i
                                        #   in Loop: Header=BB0_203 Depth=7
	movq	8(%r15), %rdx
	movslq	(%rdx), %rax
	testq	%rax, %rax
	movq	$1, %r12
	jle	.LBB0_209
	.align	16, 0x90
.LBB0_207:                              # %for.body182.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        #               Parent Loop BB0_203 Depth=7
                                        # =>              This Inner Loop Header: Depth=8
	movq	(%rdx,%r12,8), %rdi
	movq	%r13, %rsi
	callq	mark_effect
	andq	$1, %r14
	movq	8(%r15), %rdx
	orq	%rax, %r14
	movslq	(%rdx), %rax
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rax, %rsi
	jl	.LBB0_207
	jmp	.LBB0_209
	.align	16, 0x90
.LBB0_208:                              # %if.else195.i.i.i
                                        #   in Loop: Header=BB0_203 Depth=7
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	mark_effect
	movq	%rax, %r14
.LBB0_209:                              # %if.end204.i.i.i
                                        #   in Loop: Header=BB0_203 Depth=7
	movq	%rbx, %rdi
	callq	cselib_process_insn
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	24(%rbx), %rbx
	movq	24(%rax), %rax
	cmpq	%rax, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jne	.LBB0_203
.LBB0_210:                              # %for.end209.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	$1, %r14
	jne	.LBB0_231
.LBB0_211:                              # %if.end212.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	movq	%r13, %r14
	je	.LBB0_213
# BB#212:                               # %if.then215.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	72(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	bitmap_operation
.LBB0_213:                              # %do.body.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	(%r14), %rax
	testq	%rax, %rax
	movq	104(%rsp), %rbx         # 8-byte Reload
	je	.LBB0_220
.LBB0_214:                              # %for.cond236.preheader.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        # =>            This Loop Header: Depth=7
                                        #                 Child Loop BB0_215 Depth 8
                                        #                   Child Loop BB0_216 Depth 9
	movq	%r14, %r13
	xorq	%rdx, %rdx
.LBB0_215:                              # %for.body239.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        #               Parent Loop BB0_214 Depth=7
                                        # =>              This Loop Header: Depth=8
                                        #                   Child Loop BB0_216 Depth 9
	movq	24(%rax,%rdx,8), %rsi
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB0_218
	.align	16, 0x90
.LBB0_216:                              # %for.body248.i.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        #               Parent Loop BB0_214 Depth=7
                                        #                 Parent Loop BB0_215 Depth=8
                                        # =>                This Inner Loop Header: Depth=9
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rsi, %rdi
	jne	.LBB0_231
# BB#217:                               # %for.inc258.i.i.i
                                        #   in Loop: Header=BB0_216 Depth=9
	incq	%rcx
	movq	%rcx, %rdi
	andq	%r15, %rdi
	cmpq	$64, %rdi
	jb	.LBB0_216
.LBB0_218:                              # %if.end261.i.i.i
                                        #   in Loop: Header=BB0_215 Depth=8
	incq	%rdx
	movq	%rdx, %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB0_215
# BB#219:                               # %for.end264.i.i.i
                                        #   in Loop: Header=BB0_214 Depth=7
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%r13, %r14
	jne	.LBB0_214
	.align	16, 0x90
.LBB0_220:                              # %do.body268.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	%r14, %r14
	je	.LBB0_222
# BB#221:                               # %if.then270.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r14, %rdi
	callq	bitmap_clear
	movq	%r14, %rdi
	callq	free
.LBB0_222:                              # %do.end272.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	callq	cselib_finish
	movq	%rbx, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	comparison_dominates_p
	testq	%r15, %rax
	movabsq	$0, %rax
	je	.LBB0_224
# BB#223:                               # %do.end272.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movabsq	$1, %rax
.LBB0_224:                              # %do.end272.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	global_rtl(%rip), %rdx
	cmpq	%rdx, %rcx
	movabsq	$0, %rcx
	movq	192(%rsp), %r14         # 8-byte Reload
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %r13          # 8-byte Reload
	jne	.LBB0_226
# BB#225:                               # %do.end272.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movabsq	$1, %rcx
.LBB0_226:                              # %do.end272.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	232(%rsp), %rdx         # 8-byte Reload
	movq	40(%rdx), %rbx
	movl	48(%rbx), %edx
	andq	$1, %rdx
	xorq	%rcx, %rax
	je	.LBB0_229
# BB#227:                               # %if.then286.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	%rdx, %rdx
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	jne	.LBB0_234
# BB#228:                               #   in Loop: Header=BB0_162 Depth=6
	movq	%r15, %r12
	jmp	.LBB0_235
	.align	16, 0x90
.LBB0_164:                              # %if.then7.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	cmpq	%r10, %rdx
	movq	%rdx, 232(%rsp)         # 8-byte Spill
	movq	%rax, %rcx
	je	.LBB0_166
# BB#165:                               # %if.then7.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%rbx, %rcx
.LBB0_166:                              # %if.then7.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	xorq	%rsi, %rsi
	movq	%rcx, %rbx
	jmp	.LBB0_247
.LBB0_229:                              # %if.else294.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	%rdx, %rdx
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	je	.LBB0_234
# BB#230:                               #   in Loop: Header=BB0_162 Depth=6
	movq	%r15, %r12
	jmp	.LBB0_235
.LBB0_234:                              # %cond.true.i.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%r15, %r12
	movq	8(%rbx), %rbx
.LBB0_235:                              # %thread_jump.exit.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	%rbx, %rbx
	je	.LBB0_236
# BB#237:                               # %if.then19.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	%r14, %r14
	jne	.LBB0_238
# BB#242:                               # %if.then21.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movslq	n_basic_blocks(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	movq	%r9, %r14
	movq	%r8, 152(%rsp)          # 8-byte Spill
	callq	xmalloc
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	%r14, %r9
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %r14
	jmp	.LBB0_243
.LBB0_238:                              # %for.cond24.preheader.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	jle	.LBB0_243
	.align	16, 0x90
.LBB0_240:                              # %for.body27.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	movq	-8(%r14,%rax,8), %rcx
	cmpq	%rbx, %rcx
	je	.LBB0_241
# BB#239:                               # %for.cond24.i.i
                                        #   in Loop: Header=BB0_240 Depth=7
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rax
	cmpq	%rsi, %rdx
	jl	.LBB0_240
	.align	16, 0x90
.LBB0_243:                              # %if.end36.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	24(%rbx), %rax
	movq	216(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB0_236
# BB#244:                               # %if.end41.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB0_245
# BB#246:                               # %if.end45.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	$32, %rcx
	leaq	1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rbx, (%r14,%rdi,8)
	movq	24(%rbx), %r10
	movq	$1, %rsi
	movq	%rdx, %rdi
	movq	144(%rsp), %rbx         # 8-byte Reload
.LBB0_247:                              # %if.end52.i418.i
                                        #   in Loop: Header=BB0_162 Depth=6
	testq	%r10, %r10
	je	.LBB0_248
# BB#249:                               # %if.end55.i419.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	je	.LBB0_264
# BB#250:                               # %if.then58.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	movq	232(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rax
	movzbl	48(%rax), %eax
	testq	$1, %rax
	jne	.LBB0_251
# BB#252:                               # %cond.false.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	movq	%r10, %r15
	movq	%rdi, %rbx
	movq	8(%rcx), %rdi
	movq	%r8, 152(%rsp)          # 8-byte Spill
	callq	prev_nonnote_insn
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	jmp	.LBB0_253
.LBB0_251:                              # %cond.true.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	movq	%r10, %r15
	movq	%rdi, %rbx
	movq	(%rcx), %rax
.LBB0_253:                              # %cond.end.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$37, %rdx
	movq	%rbx, %rdi
	movq	%r15, %r10
	je	.LBB0_255
# BB#254:                               # %if.then66.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	24(%rax), %rax
	movl	(%rax), %ecx
.LBB0_255:                              # %land.lhs.true71.i.preheader.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movq	144(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_256
	.align	16, 0x90
.LBB0_260:                              # %for.inc95.i.i
                                        #   in Loop: Header=BB0_256 Depth=7
	movq	24(%rax), %rax
	movl	(%rax), %ecx
.LBB0_256:                              # %land.lhs.true71.i.preheader.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_157 Depth=5
                                        #             Parent Loop BB0_162 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$36, %rdx
	je	.LBB0_261
# BB#257:                               # %land.rhs.i.i
                                        #   in Loop: Header=BB0_256 Depth=7
	andq	%r12, %rdx
	movzbl	rtx_class(%rdx), %esi
	cmpq	$105, %rsi
	je	.LBB0_261
# BB#258:                               # %for.body83.i.i
                                        #   in Loop: Header=BB0_256 Depth=7
	cmpq	$37, %rdx
	jne	.LBB0_260
# BB#259:                               # %land.lhs.true88.i.i
                                        #   in Loop: Header=BB0_256 Depth=7
	movl	40(%rax), %edx
	movabsq	$4294967200, %rsi       # imm = 0xFFFFFFA0
	cmpq	%rsi, %rdx
	jne	.LBB0_260
	.align	16, 0x90
.LBB0_261:                              # %for.end99.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	je	.LBB0_262
# BB#263:                               # %for.end99.if.end106_crit_edge.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	movl	n_basic_blocks(%rip), %eax
	movq	192(%rsp), %rsi         # 8-byte Reload
.LBB0_264:                              # %if.end106.i.i
                                        #   in Loop: Header=BB0_162 Depth=6
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	andq	$1, %r8
	orq	%rsi, %r8
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	movq	%r10, %rdx
	jl	.LBB0_162
	jmp	.LBB0_265
.LBB0_160:                              #   in Loop: Header=BB0_157 Depth=5
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%r9, %r10
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	jmp	.LBB0_265
.LBB0_231:                              # %do.body306.i.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	%r15, %r12
	movq	%r13, %rbx
	testq	%rbx, %rbx
	je	.LBB0_233
# BB#232:                               # %if.then308.i.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	%rbx, %rdi
	callq	bitmap_clear
	movq	%rbx, %rdi
	callq	free
.LBB0_233:                              # %do.end310.i.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	callq	cselib_finish
	movq	232(%rsp), %r10         # 8-byte Reload
	movq	192(%rsp), %r14         # 8-byte Reload
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	96(%rsp), %r13          # 8-byte Reload
	jmp	.LBB0_265
.LBB0_168:                              #   in Loop: Header=BB0_157 Depth=5
	movq	%r14, %r10
	movq	%r15, %r14
	jmp	.LBB0_265
.LBB0_175:                              #   in Loop: Header=BB0_157 Depth=5
	movq	%r14, %r10
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r14
	jmp	.LBB0_265
.LBB0_188:                              #   in Loop: Header=BB0_157 Depth=5
	movq	%r15, %r12
.LBB0_185:                              #   in Loop: Header=BB0_157 Depth=5
	movq	232(%rsp), %r10         # 8-byte Reload
.LBB0_186:                              # %while.end.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	192(%rsp), %r14         # 8-byte Reload
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_265
.LBB0_241:                              # %if.then34.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movl	n_basic_blocks(%rip), %ebx
	movq	232(%rsp), %r10         # 8-byte Reload
	jmp	.LBB0_265
.LBB0_236:                              #   in Loop: Header=BB0_157 Depth=5
	movq	232(%rsp), %r10         # 8-byte Reload
	movq	144(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_265
.LBB0_248:                              #   in Loop: Header=BB0_157 Depth=5
	movq	232(%rsp), %r10         # 8-byte Reload
	jmp	.LBB0_265
.LBB0_194:                              #   in Loop: Header=BB0_157 Depth=5
	movq	%r15, %r12
	movq	%r14, %r10
	jmp	.LBB0_186
.LBB0_262:                              #   in Loop: Header=BB0_157 Depth=5
	movq	232(%rsp), %r10         # 8-byte Reload
.LBB0_265:                              # %while.end.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	movq	200(%rsp), %rbx         # 8-byte Reload
	jge	.LBB0_266
# BB#268:                               # %if.else120.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	cmpq	%r9, %r10
	movq	%r12, %r15
	movq	216(%rsp), %r12         # 8-byte Reload
	je	.LBB0_157
# BB#269:                               # %if.else124.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	56(%r13), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movl	52(%r13), %eax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r10
	movabsq	$0, %rax
	je	.LBB0_271
# BB#270:                               # %if.else124.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movabsq	$1, %rax
.LBB0_271:                              # %if.else124.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	%r9, 208(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	andq	%rax, %r8
	cmpq	$1, %r8
	je	.LBB0_272
# BB#289:                               # %if.else136.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	%r13, %rdi
	movq	%r10, %rsi
	movq	%r10, 232(%rsp)         # 8-byte Spill
	callq	redirect_edge_and_branch
	testq	%rax, %rax
	jne	.LBB0_277
# BB#290:                               # %if.then138.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	movq	200(%rsp), %rbx         # 8-byte Reload
	movq	232(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_157
# BB#291:                               # %if.then140.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	24(%r13), %rax
	movl	88(%rcx), %r8d
	movl	88(%r12), %edx
	movl	88(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str11, %rsi
	callq	fprintf
	jmp	.LBB0_157
.LBB0_266:                              # %if.then115.i.i
                                        #   in Loop: Header=BB0_157 Depth=5
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	movq	%r12, %r15
	movq	216(%rsp), %r12         # 8-byte Reload
	je	.LBB0_157
	jmp	.LBB0_267
.LBB0_272:                              # %if.then130.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	%r14, 192(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	movq	%r10, %rsi
	movq	%r10, 232(%rsp)         # 8-byte Spill
	callq	redirect_edge_and_branch_force
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB0_275
# BB#273:                               # %if.end.i375.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	80(%r14), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	orq	$1, %rax
	movq	%rax, 80(%r14)
	movq	%r14, %rdi
	callq	forwarder_block_p
	testq	%rax, %rax
	je	.LBB0_275
# BB#274:                               # %if.then2.i.i432.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	80(%r14), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	andq	%rcx, %rax
	orq	$2, %rax
	movq	%rax, 80(%r14)
.LBB0_275:                              # %notice_new_block.exit.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	movq	192(%rsp), %r14         # 8-byte Reload
	je	.LBB0_277
# BB#276:                               # %if.then133.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	$23, %rsi
	movq	$1, %rdx
	movabsq	$.L.str10, %rdi
	callq	fwrite
.LBB0_277:                              # %if.end148.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movl	104(%r12), %eax
	movq	144(%rsp), %rcx         # 8-byte Reload
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$3777893186295716171, %rcx # imm = 0x346DC5D63886594B
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %r13
	movq	80(%r12), %rsi
	shrq	%cl, %r13
	movq	$11, %rcx
	sarq	%cl, %rdx
	testq	$2, %rsi
	jne	.LBB0_281
# BB#278:                               # %land.lhs.true153.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	%rdx, %r15
	movq	%r12, %rdi
	callq	forwarder_block_p
	movq	80(%r12), %rsi
	testq	%rax, %rax
	je	.LBB0_280
# BB#279:                               # %if.then156.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	andq	%rax, %rsi
	orq	$2, %rsi
	movq	%rsi, 80(%r12)
.LBB0_280:                              # %if.end161.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	movq	%r15, %rdx
.LBB0_281:                              # %if.end161.i.i
                                        #   in Loop: Header=BB0_156 Depth=4
	addq	%r13, %rdx
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %rsi
	orq	$1, %rsi
	movq	%rsi, 80(%r12)
	imulq	$10000, %rdx, %rax      # imm = 0x2710
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%r11, %r11
	movq	%rbx, %r8
	movq	%r8, 160(%rsp)          # 8-byte Spill
	movq	208(%rsp), %rbx         # 8-byte Reload
	movq	232(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %rdi         # 8-byte Reload
	.align	16, 0x90
.LBB0_282:                              # %do.body.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB0_301 Depth 6
	movq	96(%rbx), %rax
	subq	%rdi, %rax
	movabsq	$0, %rcx
	js	.LBB0_284
# BB#283:                               # %do.body.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%rax, %rcx
.LBB0_284:                              # %do.body.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r14, 192(%rsp)         # 8-byte Spill
	movq	%rcx, 96(%rbx)
	movl	104(%rbx), %esi
	subq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r10
	js	.LBB0_286
# BB#285:                               # %do.body.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%rsi, %r10
.LBB0_286:                              # %do.body.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movl	%r10d, 104(%rbx)
	movq	40(%rbx), %r15
	movq	8(%r15), %r14
	movq	$32, %rcx
	testq	%r14, %r14
	jne	.LBB0_287
# BB#303:                               # %if.else239.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jge	.LBB0_304
# BB#305:                               # %land.lhs.true242.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	192(%rsp), %r14         # 8-byte Reload
	movq	(%r14,%rax,8), %rax
	movq	16(%rax), %rax
	cmpq	%rax, %rbx
	movabsq	$0, %rax
	jne	.LBB0_307
# BB#306:                               # %land.lhs.true242.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movabsq	$1, %rax
.LBB0_307:                              # %land.lhs.true242.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	addq	%rax, %r11
	jmp	.LBB0_308
	.align	16, 0x90
.LBB0_287:                              # %if.then185.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jge	.LBB0_288
# BB#292:                               # %if.end190.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r15, %rdx
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %r15
	movq	16(%r15), %rax
	cmpq	%rbx, %rax
	jne	.LBB0_293
# BB#294:                               # %if.end197.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	xorq	%rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r10
	movq	%rdx, %r8
	je	.LBB0_296
# BB#295:                               # %if.then200.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	$32, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
.LBB0_296:                              # %if.end205.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%rbx, 208(%rsp)         # 8-byte Spill
	movslq	52(%r15), %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	%rdx, %rcx
	jg	.LBB0_298
# BB#297:                               # %if.end205.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%rax, %rcx
.LBB0_298:                              # %if.end205.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r12, 216(%rsp)         # 8-byte Spill
	incq	%r11
	subq	%rcx, %rdx
	movl	%edx, 52(%r15)
	movq	$10000, %rdi            # imm = 0x2710
	subq	%rcx, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_299
# BB#357:                               # %if.then217.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r11, 184(%rsp)         # 8-byte Spill
	movq	$10000, %rax            # imm = 0x2710
	movl	%eax, 52(%r8)
	xorq	%rax, %rax
	movl	%eax, 52(%r14)
	jmp	.LBB0_302
	.align	16, 0x90
.LBB0_304:                              #   in Loop: Header=BB0_282 Depth=5
	movq	192(%rsp), %r14         # 8-byte Reload
	jmp	.LBB0_308
	.align	16, 0x90
.LBB0_299:                              # %for.cond225.preheader.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%r11, 184(%rsp)         # 8-byte Spill
	testq	%r8, %r8
	movq	%r8, %rbx
	movabsq	$4294967280, %r13       # imm = 0xFFFFFFF0
	je	.LBB0_302
# BB#300:                               # %for.body227.for.body227_crit_edge.i.lr.ph.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	callq	int32_to_float64
	movl	52(%rbx), %ecx
	movq	%rax, %r12
	imulq	$10000, %rcx, %rdi      # imm = 0x2710
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_div
	movq	%rax, %rdi
	callq	float64_to_int64
	movl	%eax, 52(%rbx)
	.align	16, 0x90
.LBB0_301:                              # %for.body227.for.body227_crit_edge.i.i
                                        #   Parent Loop BB0_16 Depth=1
                                        #     Parent Loop BB0_18 Depth=2
                                        #       Parent Loop BB0_19 Depth=3
                                        #         Parent Loop BB0_156 Depth=4
                                        #           Parent Loop BB0_282 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movl	52(%r14), %eax
	movq	8(%r14), %rbx
	imulq	$10000, %rax, %rdi      # imm = 0x2710
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_div
	movq	%rax, %rdi
	callq	float64_to_int64
	testq	%rbx, %rbx
	movl	%eax, 52(%r14)
	movq	%rbx, %r14
	jne	.LBB0_301
.LBB0_302:                              # %if.end238.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	update_br_prob_note
	movq	216(%rsp), %r12         # 8-byte Reload
	movq	192(%rsp), %r14         # 8-byte Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	232(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	184(%rsp), %r11         # 8-byte Reload
.LBB0_308:                              # %if.end252.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	56(%r15), %rcx
	xorq	%rax, %rax
	subq	%rdi, %rcx
	js	.LBB0_310
# BB#309:                               # %if.end252.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%rcx, %rax
.LBB0_310:                              # %if.end252.i.i
                                        #   in Loop: Header=BB0_282 Depth=5
	movq	%rax, 56(%r15)
	movq	24(%r15), %rbx
	cmpq	%r9, %rbx
	movq	$1, %rax
	jne	.LBB0_282
	jmp	.LBB0_311
	.align	16, 0x90
.LBB0_312:                              # %for.end267.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%r12, %r13
	testq	%r14, %r14
	movq	%r14, %rdi
	movabsq	$.L.str1, %r14
	movq	128(%rsp), %r12         # 8-byte Reload
	je	.LBB0_314
# BB#313:                               # %if.then269.i.i
                                        #   in Loop: Header=BB0_19 Depth=3
	callq	free
.LBB0_314:                              # %try_forward_edges.exit.i
                                        #   in Loop: Header=BB0_19 Depth=3
	testq	$255, 184(%rsp)         # 8-byte Folded Reload
	movq	$1, %rbx
	jne	.LBB0_316
# BB#315:                               # %try_forward_edges.exit.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	224(%rsp), %rbx         # 8-byte Reload
.LBB0_316:                              # %try_forward_edges.exit.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	testq	%r15, %rax
	je	.LBB0_320
# BB#317:                               # %land.lhs.true179.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	try_crossjump_bb
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB0_319
# BB#318:                               # %land.lhs.true179.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rbx, %rax
.LBB0_319:                              # %land.lhs.true179.i
                                        #   in Loop: Header=BB0_19 Depth=3
	movq	%rax, %rbx
.LBB0_320:                              # %if.end183.i
                                        #   in Loop: Header=BB0_19 Depth=3
	testq	$1, %rbx
	movq	$1, %rbx
	jne	.LBB0_19
	jmp	.LBB0_321
	.align	16, 0x90
.LBB0_322:                              # %for.end188.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	testq	%r15, %rax
	je	.LBB0_326
# BB#323:                               # %land.lhs.true191.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$entry_exit_blocks+112, %rsi
	callq	try_crossjump_bb
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB0_325
# BB#324:                               # %land.lhs.true191.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	%rbx, %rax
.LBB0_325:                              # %land.lhs.true191.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	%rax, %rbx
.LBB0_326:                              # %if.end195.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	%rbx, %rax
	andq	$1, %rax
	orq	%rax, 32(%rsp)          # 8-byte Folded Spill
	andq	$1, %rbx
	jne	.LBB0_16
# BB#327:                               # %if.end203.loopexit.i
	movq	32(%rsp), %rbx          # 8-byte Reload
	andq	$1, %rbx
.LBB0_328:                              # %if.end203.i
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	120(%rsp), %rax         # 8-byte Reload
	testq	%r15, %rax
	je	.LBB0_330
# BB#329:                               # %if.then206.i
	callq	remove_fake_edges
.LBB0_330:                              # %if.end207.i
	testq	$32, 64(%rsp)           # 8-byte Folded Reload
	je	.LBB0_340
# BB#331:                               # %if.end207.i
	testq	%rbx, %rbx
	je	.LBB0_340
# BB#332:                               # %if.then213.i
	movl	n_basic_blocks(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	sbitmap_zero
	movslq	n_basic_blocks(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB0_339
# BB#333:                               # %for.body218.lr.ph.i
	movq	%rbx, %r8
	movq	basic_block_info(%rip), %rsi
	xorq	%rbx, %rbx
	andq	%r15, %rdx
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_334:                              # %for.body218.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rsi,%rbx,8), %rdi
	movzbl	80(%rdi), %edi
	testq	$1, %rdi
	je	.LBB0_336
# BB#335:                               # %if.then226.i
                                        #   in Loop: Header=BB0_334 Depth=1
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	movq	$3, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	andq	$536870904, %rax        # imm = 0x1FFFFFF8
	orq	%rdi, 16(%r14,%rax)
	movq	$1, %rcx
.LBB0_336:                              # %for.inc231.i
                                        #   in Loop: Header=BB0_334 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%rdx, %rax
	jne	.LBB0_334
# BB#337:                               # %for.end233.i
	testq	$1, %rcx
	movq	%r8, %rbx
	je	.LBB0_339
# BB#338:                               # %if.then235.i
	movq	$1, %rsi
	movq	$25, %rdx
	movq	%r14, %rdi
	callq	update_life_info
.LBB0_339:                              # %if.end236.i
	movq	%r14, %rdi
	callq	free
.LBB0_340:                              # %for.cond238.preheader.i
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_343
# BB#341:
	movq	$4, %rcx
	andq	%r15, %rax
	.align	16, 0x90
.LBB0_342:                              # %for.body241.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rdx
	movq	(%rdx,%rcx,8), %rdx
	movq	$0, 80(%rdx)
	leaq	-3(%rcx), %rdx
	andq	%r15, %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jne	.LBB0_342
.LBB0_343:                              # %try_optimize_cfg.exit
	testq	%rbx, %rbx
	je	.LBB0_353
# BB#344:                               # %if.then
	callq	find_unreachable_blocks
	movslq	n_basic_blocks(%rip), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jle	.LBB0_345
# BB#346:
	movq	$4, %rbx
	xorq	%r14, %r14
	.align	16, 0x90
.LBB0_347:                              # %for.body.i28
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rdx
	movq	(%rdx,%rbx,8), %r15
	movzbl	108(%r15), %ecx
	testq	$1, %rcx
	jne	.LBB0_349
# BB#348:                               # %if.then.i30
                                        #   in Loop: Header=BB0_347 Depth=1
	movq	%r15, %rdi
	callq	flow_delete_block_noexpunge
	movq	%r15, %rdi
	callq	expunge_block_nocompact
	movq	$1, %rax
	jmp	.LBB0_350
	.align	16, 0x90
.LBB0_349:                              # %if.else.i35
                                        #   in Loop: Header=BB0_347 Depth=1
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r15, 32(%rdx,%rsi,8)
	movl	%r14d, 88(%r15)
	incq	%r14
.LBB0_350:                              # %for.inc.i40
                                        #   in Loop: Header=BB0_347 Depth=1
	movslq	n_basic_blocks(%rip), %rdx
	leaq	-3(%rbx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB0_347
	jmp	.LBB0_351
.LBB0_345:
	xorq	%r14, %r14
.LBB0_351:                              # %for.end.i46
	movl	%r14d, n_basic_blocks(%rip)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	sarq	%cl, %r14
	movq	%r14, (%rdx)
	testq	$1, %rax
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	je	.LBB0_353
# BB#352:                               # %if.then7.i47
	callq	tidy_fallthru_edges
.LBB0_353:                              # %if.end
	movabsq	$label_value_list, %rdi
	callq	free_EXPR_LIST_list
	movabsq	$tail_recursion_label_list, %rdi
	callq	free_EXPR_LIST_list
	movq	$4, %rdi
	callq	timevar_pop
	testq	$255, 24(%rsp)          # 8-byte Folded Reload
	je	.LBB0_354
# BB#355:                               # %if.end
	movabsq	$1, %rax
	jmp	.LBB0_356
.LBB0_354:
	movabsq	$0, %rax
.LBB0_356:                              # %if.end
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_110:                              # %if.then50.i.i.i
	movabsq	$.L.str8, %rax
	movabsq	$.L__FUNCTION__.merge_blocks_move_successor_nojumps, %rcx
	movq	$724, %rsi              # imm = 0x2D4
	jmp	.LBB0_111
.LBB0_121:                              # %if.then.i.i.i
	movabsq	$.L.str8, %rax
	movabsq	$.L__FUNCTION__.merge_blocks_move_predecessor_nojumps, %rcx
	movq	$650, %rsi              # imm = 0x28A
	jmp	.LBB0_111
.LBB0_123:                              # %if.then4.i.i.i
	movabsq	$.L.str8, %rax
	movabsq	$.L__FUNCTION__.merge_blocks_move_predecessor_nojumps, %rcx
	movq	$661, %rsi              # imm = 0x295
	jmp	.LBB0_111
.LBB0_245:                              # %if.then44.i.i
	movabsq	$.L.str8, %rax
	movabsq	$.L__FUNCTION__.try_forward_edges, %rcx
	movq	$441, %rsi              # imm = 0x1B9
	jmp	.LBB0_111
.LBB0_288:                              # %if.then189.i.i
	movabsq	$.L.str8, %rax
	movabsq	$.L__FUNCTION__.try_forward_edges, %rcx
	movq	$539, %rsi              # imm = 0x21B
	jmp	.LBB0_111
.LBB0_293:                              # %if.then196.i.i
	movabsq	$.L.str8, %rax
	movabsq	$.L__FUNCTION__.try_forward_edges, %rcx
	movq	$542, %rsi              # imm = 0x21E
.LBB0_111:                              # %if.then50.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	cleanup_cfg, .Ltmp17-cleanup_cfg
	.cfi_endproc

	.align	16, 0x90
	.type	try_crossjump_bb,@function
try_crossjump_bb:                       # @try_crossjump_bb
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
	subq	$32, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 80
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
	movq	32(%rsi), %r13
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	testq	%r13, %r13
	je	.LBB1_32
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, (%r13)
	je	.LBB1_32
# BB#2:
	movq	%r13, %rax
	movabsq	$0, %r12
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	48(%rax), %ecx
	movq	$1, %r14
	testq	$1, %rcx
	jne	.LBB1_4
# BB#5:                                 # %for.inc
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	(%rax), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	jne	.LBB1_3
	jmp	.LBB1_6
.LBB1_4:
	movq	%rax, %r12
.LBB1_6:                                # %for.body12.lr.ph.lr.ph
	xorq	%rax, %rax
.LBB1_7:                                # %for.body12.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_22 Depth 2
                                        #       Child Loop BB1_30 Depth 3
                                        #     Child Loop BB1_8 Depth 2
                                        #       Child Loop BB1_12 Depth 3
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%r14, %r14
	je	.LBB1_22
	.align	16, 0x90
.LBB1_8:                                # %if.then15.us
                                        #   Parent Loop BB1_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_12 Depth 3
	movq	%r13, %rbx
	movq	(%rbx), %r13
	cmpq	%r12, %rbx
	je	.LBB1_20
# BB#9:                                 # %if.end17.us
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	try_crossjump_to_edge
	testq	%rax, %rax
	jne	.LBB1_17
# BB#10:                                # %if.end21.us
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	16(%rbx), %rax
	movq	40(%rax), %rax
	cmpq	%rbx, %rax
	jne	.LBB1_20
# BB#11:                                # %if.end24.us
                                        #   in Loop: Header=BB1_8 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r15
.LBB1_12:                               # %if.end24.us
                                        #   Parent Loop BB1_7 Depth=1
                                        #     Parent Loop BB1_8 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r15, %r15
	je	.LBB1_20
# BB#13:                                # %for.body28.us
                                        #   in Loop: Header=BB1_12 Depth=3
	movq	%r15, %rdx
	movq	(%rdx), %r15
	cmpq	%rbx, %rdx
	je	.LBB1_12
# BB#14:                                # %for.body28.us
                                        #   in Loop: Header=BB1_12 Depth=3
	cmpq	%r12, %rdx
	je	.LBB1_12
# BB#15:                                # %if.end35.us
                                        #   in Loop: Header=BB1_12 Depth=3
	movq	16(%rbx), %rax
	movq	16(%rdx), %rcx
	movslq	88(%rax), %rax
	movslq	88(%rcx), %rcx
	cmpq	%rcx, %rax
	jg	.LBB1_12
# BB#16:                                # %if.end41.us
                                        #   in Loop: Header=BB1_12 Depth=3
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	try_crossjump_to_edge
	testq	%rax, %rax
	je	.LBB1_12
	jmp	.LBB1_17
	.align	16, 0x90
.LBB1_20:                               # %for.cond10.backedge.us
                                        #   in Loop: Header=BB1_8 Depth=2
	testq	%r13, %r13
	jne	.LBB1_8
	jmp	.LBB1_21
	.align	16, 0x90
.LBB1_22:                               # %if.end21
                                        #   Parent Loop BB1_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_30 Depth 3
	movq	%r13, %r15
	movq	16(%r15), %rax
	movq	40(%rax), %rax
	movq	(%r15), %r13
	cmpq	%r15, %rax
	jne	.LBB1_23
# BB#25:                                # %if.end24
                                        #   in Loop: Header=BB1_22 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rbx
	.align	16, 0x90
.LBB1_30:                               # %for.cond26.backedge
                                        #   Parent Loop BB1_7 Depth=1
                                        #     Parent Loop BB1_22 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB1_23
# BB#26:                                # %for.body28
                                        #   in Loop: Header=BB1_30 Depth=3
	movq	%rbx, %rdx
	movq	(%rdx), %rbx
	cmpq	%r15, %rdx
	je	.LBB1_30
# BB#27:                                # %for.body28
                                        #   in Loop: Header=BB1_30 Depth=3
	cmpq	%r12, %rdx
	je	.LBB1_30
# BB#28:                                # %if.end35
                                        #   in Loop: Header=BB1_30 Depth=3
	movq	16(%r15), %rax
	movq	16(%rdx), %rcx
	movslq	88(%rax), %rax
	movslq	88(%rcx), %rcx
	cmpq	%rcx, %rax
	jg	.LBB1_30
# BB#29:                                # %if.end41
                                        #   in Loop: Header=BB1_30 Depth=3
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	try_crossjump_to_edge
	testq	%rax, %rax
	je	.LBB1_30
	jmp	.LBB1_17
	.align	16, 0x90
.LBB1_23:                               # %for.cond10.backedge
                                        #   in Loop: Header=BB1_22 Depth=2
	testq	%r13, %r13
	jne	.LBB1_22
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_17:                               # %for.cond10.outer.backedge
                                        #   in Loop: Header=BB1_7 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r13
	testq	%r13, %r13
	movq	$1, %rax
	jne	.LBB1_7
	jmp	.LBB1_18
.LBB1_21:
	movq	8(%rsp), %rax           # 8-byte Reload
	jmp	.LBB1_18
.LBB1_24:
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB1_18:                               # %for.end49
	testq	$255, %rax
	je	.LBB1_19
# BB#31:                                # %for.end49
	movabsq	$1, %rax
	jmp	.LBB1_32
.LBB1_19:
	movabsq	$0, %rax
.LBB1_32:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	try_crossjump_bb, .Ltmp35-try_crossjump_bb
	.cfi_endproc

	.align	16, 0x90
	.type	try_crossjump_to_edge,@function
try_crossjump_to_edge:                  # @try_crossjump_to_edge
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
	movq	16(%rsi), %r14
	movq	32(%r14), %rax
	movq	16(%rdx), %r12
	testq	%rax, %rax
	je	.LBB2_3
# BB#1:                                 # %land.lhs.true
	movzbl	80(%r14), %ecx
	testq	$2, %rcx
	je	.LBB2_3
# BB#2:                                 # %if.then
	movq	16(%rax), %r14
	movq	%rax, %rsi
.LBB2_3:                                # %if.end
	movq	32(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_6
# BB#4:                                 # %land.lhs.true9
	movzbl	80(%r12), %ecx
	testq	$2, %rcx
	je	.LBB2_6
# BB#5:                                 # %if.then13
	movq	16(%rax), %r12
	movq	%rax, %rdx
.LBB2_6:                                # %if.end16
	movabsq	$entry_exit_blocks, %rax
	xorq	%r13, %r13
	cmpq	%rax, %r14
	je	.LBB2_212
# BB#7:                                 # %if.end16
	cmpq	%rax, %r12
	je	.LBB2_212
# BB#8:                                 # %if.end16
	cmpq	%r12, %r14
	je	.LBB2_212
# BB#9:                                 # %if.end22
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	24(%rsi), %rax
	movzbl	80(%rax), %ecx
	testq	$2, %rcx
	je	.LBB2_11
# BB#10:                                # %land.lhs.true26
	movq	40(%rax), %rax
	movq	24(%rax), %rax
	movzbl	80(%rax), %eax
	testq	$2, %rax
	jne	.LBB2_212
.LBB2_11:                               # %if.end33
	movq	24(%rdx), %rax
	movzbl	80(%rax), %ecx
	testq	$2, %rcx
	je	.LBB2_13
# BB#12:                                # %land.lhs.true38
	movq	40(%rax), %rax
	movq	24(%rax), %rax
	movzbl	80(%rax), %eax
	testq	$2, %rax
	jne	.LBB2_212
.LBB2_13:                               # %if.end46
	cmpq	$0, 32(%r14)
	je	.LBB2_212
# BB#14:                                # %lor.lhs.false49
	cmpq	$0, 32(%r12)
	je	.LBB2_212
# BB#15:                                # %if.end53
	movq	40(%r14), %rax
	leaq	40(%r14), %rbx
	testq	%rax, %rax
	je	.LBB2_83
# BB#16:                                # %land.lhs.true.i
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB2_17
# BB#22:                                # %land.lhs.true21.i
	cmpq	$0, 8(%rcx)
	jne	.LBB2_83
# BB#23:                                # %land.lhs.true26.i
	movq	8(%r14), %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB2_83
# BB#24:                                # %land.lhs.true28.i
	leaq	8(%r14), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	(%rax), %rdi
	callq	onlyjump_p
	testq	%r15, %rax
	je	.LBB2_83
# BB#25:                                # %if.then32.i
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_212
# BB#26:                                # %lor.lhs.false.i
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB2_212
# BB#27:                                # %lor.lhs.false38.i
	cmpq	$0, 8(%rax)
	jne	.LBB2_212
# BB#28:                                # %lor.lhs.false43.i
	movq	8(%r12), %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB2_212
# BB#29:                                # %lor.lhs.false47.i
	movq	8(%r12), %rdi
	callq	onlyjump_p
	testq	%r15, %rax
	je	.LBB2_212
# BB#30:                                # %if.end52.i
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movl	92(%r14), %eax
	movl	92(%r12), %ecx
	cmpq	%rcx, %rax
	jne	.LBB2_212
# BB#31:                                # %if.end56.i
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movl	48(%rax), %edx
	andq	$1, %rdx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	je	.LBB2_33
# BB#32:                                # %cond.true.i
	movq	8(%rax), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
.LBB2_33:                               # %cond.end.i
	movq	40(%r12), %rcx
	movl	48(%rcx), %esi
	andq	$1, %rsi
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	je	.LBB2_35
# BB#34:                                # %cond.true68.i
	movq	8(%rcx), %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
.LBB2_35:                               # %cond.end73.i
	testq	%r15, %rdx
	jne	.LBB2_37
# BB#36:                                # %cond.false81.i
	movq	8(%rax), %rax
.LBB2_37:                               # %cond.end84.i
	testq	%r15, %rsi
	jne	.LBB2_39
# BB#38:                                # %cond.false92.i
	movq	8(%rcx), %rcx
.LBB2_39:                               # %cond.end95.i
	movq	24(%rax), %rdx
	movzbl	80(%rdx), %esi
	testq	$2, %rsi
	je	.LBB2_41
# BB#40:                                # %if.then99.i
	movq	40(%rdx), %rax
.LBB2_41:                               # %if.end102.i
	movq	24(%rcx), %rdx
	movzbl	80(%rdx), %esi
	testq	$2, %rsi
	je	.LBB2_43
# BB#42:                                # %if.then107.i
	movq	40(%rdx), %rcx
.LBB2_43:                               # %if.end110.i
	movq	24(%rax), %rax
	movzbl	80(%rax), %edx
	testq	$2, %rdx
	jne	.LBB2_212
# BB#44:                                # %lor.lhs.false115.i
	movq	24(%rcx), %rcx
	movzbl	80(%rcx), %edx
	testq	$2, %rdx
	jne	.LBB2_212
# BB#45:                                # %lor.lhs.false120.i
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	24(%rdx), %rdx
	movzbl	80(%rdx), %esi
	testq	$2, %rsi
	jne	.LBB2_212
# BB#46:                                # %lor.lhs.false125.i
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsi), %rsi
	movzbl	80(%rsi), %edi
	testq	$2, %rdi
	jne	.LBB2_212
# BB#47:                                # %if.end131.i
	cmpq	%rcx, %rax
	jne	.LBB2_49
# BB#48:                                # %if.end131.i
	xorq	%rbx, %rbx
	cmpq	%rsi, %rdx
	je	.LBB2_51
.LBB2_49:                               # %if.else.i
	xorq	%r13, %r13
	cmpq	%rsi, %rax
	jne	.LBB2_212
# BB#50:                                # %if.else.i
	movq	$1, %rbx
	cmpq	%rcx, %rdx
	jne	.LBB2_212
.LBB2_51:                               # %if.end150.i
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, %rdi
	callq	pc_set
	movq	8(%r12), %rcx
	movq	%rax, %r13
	movq	%rcx, %rdi
	callq	pc_set
	movq	16(%r13), %rcx
	movq	%rax, %rdi
	movq	16(%rcx), %rax
	movq	global_rtl(%rip), %rsi
	movq	8(%rcx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	cmpq	%rsi, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB2_53
# BB#52:                                # %if.end150.i
	movq	%rdx, %rcx
.LBB2_53:                               # %if.end150.i
	movq	16(%rdi), %rdi
	movq	16(%rdi), %r8
	movq	8(%rdi), %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	cmpq	%rsi, %r8
	jne	.LBB2_55
# BB#54:                                # %if.end150.i
	movq	%rdx, %rax
.LBB2_55:                               # %if.end150.i
	movq	24(%rsp), %rdx          # 8-byte Reload
	movzbl	(%rdx), %edx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	xorq	%rax, %rcx
	cmpq	%rcx, %rbx
	jne	.LBB2_56
# BB#57:                                # %if.else188.i
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %ebx
	jmp	.LBB2_58
.LBB2_17:                               # %land.lhs.true3.i
	movzbl	48(%rax), %eax
	testq	$30, %rax
	je	.LBB2_18
.LBB2_83:                               # %if.end283.i
	movq	8(%r14), %rsi
	movq	8(%r12), %rdx
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	insns_match_p
	testq	%rax, %rax
	je	.LBB2_212
# BB#84:                                # %if.end288.i
	movq	40(%r12), %rax
	movq	(%rbx), %r11
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movabsq	$0, %rdi
	movabsq	$1, %rsi
	movq	%rdi, %rcx
	je	.LBB2_86
# BB#85:                                # %if.end288.i
	movq	%rsi, %rcx
.LBB2_86:                               # %if.end288.i
	xorq	%r15, %r15
	testq	%r11, %r11
	leaq	8(%r14), %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	je	.LBB2_88
# BB#87:                                # %if.end288.i
	movq	%rsi, %rdi
.LBB2_88:                               # %if.end288.i
	je	.LBB2_89
# BB#90:                                # %if.end288.i
	testq	%rax, %rax
	je	.LBB2_89
# BB#91:
	movabsq	$0, %r9
	movabsq	$1, %r8
	xorq	%r10, %r10
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_92:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	48(%r11), %edx
	movl	48(%rax), %esi
	movq	$3, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movq	$3, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	testq	$1, %rdx
	je	.LBB2_94
# BB#93:                                # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	movq	%r11, 56(%rsp)          # 8-byte Spill
.LBB2_94:                               # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	testq	$1, %rsi
	je	.LBB2_96
# BB#95:                                # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB2_96:                               # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	movq	8(%rax), %rax
	movq	8(%r11), %r11
	andq	$1, %rbx
	andq	$1, %rdi
	testq	%rax, %rax
	movq	%r9, %rcx
	je	.LBB2_98
# BB#97:                                # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	movq	%r8, %rcx
.LBB2_98:                               # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	addq	%rbx, %r15
	addq	%rdi, %r10
	testq	%r11, %r11
	movq	%r9, %rdi
	je	.LBB2_100
# BB#99:                                # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	movq	%r8, %rdi
.LBB2_100:                              # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	je	.LBB2_102
# BB#101:                               # %for.body.i
                                        #   in Loop: Header=BB2_92 Depth=1
	testq	%rax, %rax
	jne	.LBB2_92
	jmp	.LBB2_102
.LBB2_89:
	xorq	%r10, %r10
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
.LBB2_102:                              # %for.end.i
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %r10
	movq	%r15, %rax
	andq	%rdx, %rax
	xorq	%r13, %r13
	cmpq	%r10, %rax
	jne	.LBB2_212
# BB#103:                               # %for.end.i
	orq	%rcx, %rdi
	xorq	$1, %rdi
	je	.LBB2_212
# BB#104:                               # %lor.lhs.false324.i
	movq	56(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB2_106
# BB#105:                               # %lor.lhs.false324.i
	movq	%rdx, %rcx
.LBB2_106:                              # %lor.lhs.false324.i
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB2_108
# BB#107:                               # %lor.lhs.false324.i
	movq	%rdx, %rax
.LBB2_108:                              # %lor.lhs.false324.i
	xorq	%rax, %rcx
	jne	.LBB2_212
# BB#109:                               # %if.end332.i
	testq	%rbx, %rbx
	je	.LBB2_116
# BB#110:                               # %if.then334.i
	movq	24(%rbx), %rdi
	callq	forwarder_block_p
	movq	24(%rbx), %rcx
	testq	%rax, %rax
	je	.LBB2_111
# BB#112:                               # %cond.true338.i
	movq	40(%rcx), %rax
	movq	24(%rax), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	jmp	.LBB2_113
.LBB2_18:                               # %if.then.i
	movq	40(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_212
# BB#19:                                # %land.lhs.true8.i
	cmpq	$0, 8(%rax)
	jne	.LBB2_212
# BB#20:                                # %land.rhs.i
	movzbl	48(%rax), %eax
	testq	$30, %rax
	jne	.LBB2_212
# BB#21:                                # %land.rhs.i.if.end55_crit_edge
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	leaq	8(%r14), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jmp	.LBB2_118
.LBB2_111:
	movq	%rcx, 56(%rsp)          # 8-byte Spill
.LBB2_113:                              # %cond.end344.i
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	24(%rbx), %rdi
	callq	forwarder_block_p
	movq	24(%rbx), %rcx
	testq	%rax, %rax
	je	.LBB2_115
# BB#114:                               # %cond.true349.i
	movq	40(%rcx), %rax
	movq	24(%rax), %rcx
.LBB2_115:                              # %cond.end355.i
	cmpq	%rcx, 56(%rsp)          # 8-byte Folded Reload
	jne	.LBB2_212
.LBB2_116:                              # %if.end361.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB2_118
# BB#117:                               # %if.then363.i
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movq	$23, %rsi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	callq	find_reg_note
	movq	8(%r12), %rdi
	movq	%rax, %rbx
	movq	$23, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	8(%rbx), %rcx
	movq	8(%rax), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_212
	jmp	.LBB2_118
.LBB2_56:                               # %if.then185.i
	movq	8(%r12), %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	reversed_comparison_code
	movq	%rax, %rbx
.LBB2_58:                               # %if.end191.i
	xorq	%r13, %r13
	testq	%r15, %rbx
	je	.LBB2_212
# BB#59:                                # %if.end195.i
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%rax, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB2_62
# BB#60:                                # %land.lhs.true198.i
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_renumbered_equal_p
	testq	%r15, %rax
	je	.LBB2_62
# BB#61:                                # %land.lhs.true207.i
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_renumbered_equal_p
	testq	%r15, %rax
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	jne	.LBB2_68
.LBB2_62:                               # %lor.rhs.i
	movq	%rbx, %rdi
	callq	swap_condition
	andq	%r15, %rax
	cmpq	%rax, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB2_212
# BB#63:                                # %land.lhs.true219.i
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_renumbered_equal_p
	testq	%r15, %rax
	je	.LBB2_212
# BB#64:                                # %land.rhs228.i
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_renumbered_equal_p
	testq	%r15, %rax
	je	.LBB2_65
# BB#66:                                # %land.rhs228.i
	movabsq	$1, %rax
	jmp	.LBB2_67
.LBB2_65:
	movabsq	$0, %rax
.LBB2_67:                               # %land.rhs228.i
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB2_68:                               # %lor.end.i
	movl	optimize_size(%rip), %ecx
	movq	32(%rsp), %rax          # 8-byte Reload
	xorq	$1, %rax
	testq	%rcx, %rcx
	jne	.LBB2_77
# BB#69:                                # %lor.end.i
	testq	%rax, %rax
	jne	.LBB2_77
# BB#70:                                # %land.lhs.true243.i
	movslq	104(%r14), %rcx
	cmpq	$11, %rcx
	jl	.LBB2_77
# BB#71:                                # %land.lhs.true246.i
	movslq	104(%r12), %rcx
	cmpq	$11, %rcx
	jl	.LBB2_77
# BB#72:                                # %if.then250.i
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	24(%rcx), %rcx
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsi), %rdx
	movl	52(%rsi), %ebx
	cmpq	%rdx, %rcx
	je	.LBB2_74
# BB#73:                                # %select.mid
	movq	$10000, %rcx            # imm = 0x2710
	subq	%rbx, %rcx
	movq	%rcx, %rbx
.LBB2_74:                               # %select.end
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	52(%rcx), %r8d
	movq	%r8, %rdx
	subq	%rbx, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$63, %rcx
	sarq	%cl, %rsi
	addq	%rsi, %rdx
	xorq	%rsi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$5001, %rdx             # imm = 0x1389
	jl	.LBB2_77
# BB#75:                                # %if.then264.i
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB2_212
# BB#76:                                # %if.then266.i
	movl	88(%r12), %r9d
	movl	88(%r14), %edx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%rcx, %rdi
	movq	%r9, %rcx
	movq	%rbx, %r9
	callq	fprintf
	jmp	.LBB2_212
.LBB2_77:                               # %if.end272.i
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_78
# BB#79:                                # %if.end272.i
	movabsq	$1, %rdx
	jmp	.LBB2_80
.LBB2_78:
	movabsq	$0, %rdx
.LBB2_80:                               # %if.end272.i
	orq	%rax, %rdx
	jne	.LBB2_82
# BB#81:                                # %if.then277.i
	movl	88(%r12), %ebx
	movl	88(%r14), %edx
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%rcx, %rdi
	movq	%rbx, %rcx
	callq	fprintf
.LBB2_82:                               # %outgoing_edges_match.exit
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB2_212
.LBB2_118:                              # %if.end55
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r13
	movq	%r13, %rdi
	callq	onlyjump_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB2_121
# BB#119:                               # %lor.lhs.false.i338
	movq	%r13, %rdi
	callq	returnjump_p
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	testq	%r15, %rax
	je	.LBB2_122
# BB#120:                               # %land.lhs.true.i341
	movq	32(%r13), %rdi
	callq	side_effects_p
	testq	%r15, %rax
	jne	.LBB2_122
.LBB2_121:                              # %if.then.i342
	movq	16(%r13), %rax
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%rax, %r13
.LBB2_122:                              # %if.end.i
	movq	8(%r12), %rbx
	movq	%rbx, %rdi
	callq	onlyjump_p
	testq	%r15, %rax
	jne	.LBB2_127
# BB#123:                               # %lor.lhs.false11.i
	movq	%rbx, %rdi
	callq	returnjump_p
	testq	%r15, %rax
	je	.LBB2_124
# BB#125:                               # %land.lhs.true14.i
	movq	32(%rbx), %rdi
	callq	side_effects_p
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	testq	%r15, %rax
	je	.LBB2_127
# BB#126:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB2_134
.LBB2_127:                              # %if.then20.i
	movq	%rbx, %rdi
	callq	simplejump_p
	xorq	%rdx, %rdx
	testq	%r15, %rax
	jne	.LBB2_133
# BB#128:                               # %land.lhs.true23.i
	movq	%rbx, %rdi
	callq	returnjump_p
	testq	%r15, %rax
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %rdx
	jne	.LBB2_130
# BB#129:                               # %land.lhs.true23.i
	movq	%rcx, %rdx
.LBB2_130:                              # %land.lhs.true23.i
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB2_132
# BB#131:                               # %land.lhs.true23.i
	movq	%rcx, %rax
.LBB2_132:                              # %land.lhs.true23.i
	andq	%rax, %rdx
.LBB2_133:                              # %if.end29.i
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rax
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	jmp	.LBB2_134
.LBB2_124:
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB2_134
.LBB2_213:                              # %while.body37.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	16(%r13), %r13
.LBB2_134:                              # %while.cond34.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_136 Depth 2
	movq	%r13, %rdi
	callq	active_insn_p
	testq	%r15, %rax
	jne	.LBB2_136
# BB#135:                               # %land.rhs.i347
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	(%r14), %rax
	cmpq	%rax, %r13
	jne	.LBB2_213
	jmp	.LBB2_136
.LBB2_138:                              # %while.body48.i
                                        #   in Loop: Header=BB2_136 Depth=2
	movq	16(%rbx), %rbx
.LBB2_136:                              # %while.cond41.preheader.i
                                        #   Parent Loop BB2_134 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	active_insn_p
	testq	%r15, %rax
	jne	.LBB2_139
# BB#137:                               # %land.rhs44.i
                                        #   in Loop: Header=BB2_136 Depth=2
	movq	(%r12), %rax
	cmpq	%rax, %rbx
	jne	.LBB2_138
.LBB2_139:                              # %while.end52.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	(%r14), %rax
	cmpq	%rax, %r13
	je	.LBB2_156
# BB#140:                               # %lor.lhs.false55.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	(%r12), %rax
	cmpq	%rax, %rbx
	je	.LBB2_156
# BB#141:                               # %if.end59.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	insns_match_p
	testq	%rax, %rax
	je	.LBB2_156
# BB#142:                               # %if.end62.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	%r13, %rdi
	callq	active_insn_p
	testq	%r15, %rax
	je	.LBB2_155
# BB#143:                               # %if.then65.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	%r13, %rdi
	callq	find_reg_equal_equiv_note
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	find_reg_equal_equiv_note
	movq	64(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB2_146
# BB#144:                               # %if.then65.i
                                        #   in Loop: Header=BB2_134 Depth=1
	testq	%rax, %rax
	jne	.LBB2_146
# BB#145:                               # %if.then71.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	%r13, %rdi
	jmp	.LBB2_153
.LBB2_146:                              # %if.else.i349
                                        #   in Loop: Header=BB2_134 Depth=1
	testq	%rsi, %rsi
	jne	.LBB2_149
# BB#147:                               # %if.else.i349
                                        #   in Loop: Header=BB2_134 Depth=1
	testq	%rax, %rax
	je	.LBB2_149
# BB#148:                               # %if.then75.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	jmp	.LBB2_153
.LBB2_149:                              # %if.else76.i
                                        #   in Loop: Header=BB2_134 Depth=1
	testq	%rsi, %rsi
	je	.LBB2_154
# BB#150:                               # %if.else76.i
                                        #   in Loop: Header=BB2_134 Depth=1
	testq	%rax, %rax
	je	.LBB2_154
# BB#151:                               # %land.lhs.true80.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	8(%rsi), %rdi
	movq	8(%rax), %rsi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	rtx_equal_p
	testq	%r15, %rax
	movq	64(%rsp), %rsi          # 8-byte Reload
	jne	.LBB2_154
# BB#152:                               # %if.then89.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	%r13, %rdi
	callq	remove_note
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
.LBB2_153:                              # %if.end92.i
                                        #   in Loop: Header=BB2_134 Depth=1
	callq	remove_note
.LBB2_154:                              # %if.end92.i
                                        #   in Loop: Header=BB2_134 Depth=1
	incq	56(%rsp)                # 8-byte Folded Spill
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
.LBB2_155:                              # %if.end94.i
                                        #   in Loop: Header=BB2_134 Depth=1
	movq	16(%r13), %r13
	movq	16(%rbx), %rbx
	jmp	.LBB2_134
.LBB2_156:                              # %while.end101.i
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	movabsq	$0, %r13
	je	.LBB2_212
# BB#157:                               # %while.cond.preheader.i
	movq	(%r14), %rax
	movq	64(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB2_158
.LBB2_160:                              # %while.body113.i
                                        #   in Loop: Header=BB2_158 Depth=1
	movq	16(%rbx), %rbx
	movq	(%r14), %rax
.LBB2_158:                              # %while.cond.preheader.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rax, %rbx
	je	.LBB2_165
# BB#159:                               # %land.rhs106.i
                                        #   in Loop: Header=BB2_158 Depth=1
	movq	16(%rbx), %rdi
	callq	active_insn_p
	testq	%r15, %rax
	je	.LBB2_160
# BB#161:                               # %while.end117.i
	movq	(%r14), %rax
	cmpq	%rax, %rbx
	je	.LBB2_165
# BB#162:                               # %land.lhs.true120.i
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB2_164
# BB#163:                               # %land.lhs.true120.i
	movq	%rbx, %rax
.LBB2_164:                              # %land.lhs.true120.i
	movq	%rax, %rbx
.LBB2_165:                              # %while.cond130.preheader.i
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r15, %r13
	movq	(%r12), %rax
	movq	32(%rsp), %rbx          # 8-byte Reload
	cmpq	%rax, %rbx
	movq	%r12, %r15
	je	.LBB2_176
.LBB2_166:                              # %land.rhs133.i
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rdi
	callq	active_insn_p
	testq	%r13, %rax
	jne	.LBB2_169
# BB#167:                               # %while.body141.i
                                        #   in Loop: Header=BB2_166 Depth=1
	movq	16(%rbx), %rbx
	movq	(%r12), %rax
	cmpq	%rax, %rbx
	jne	.LBB2_166
# BB#168:
	movq	%r12, %r15
	jmp	.LBB2_176
.LBB2_169:                              # %while.end145.i
	movq	(%r12), %rax
	cmpq	%rax, %rbx
	movq	%r12, %r15
	je	.LBB2_176
# BB#170:                               # %if.end59
	movq	%rbx, %rdx
	movq	16(%rdx), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$36, %rcx
	je	.LBB2_172
# BB#171:                               # %if.end59
	movq	%rdx, %rbx
.LBB2_172:                              # %if.end59
	cmpq	%rax, %rbx
	movq	%r12, %r15
	je	.LBB2_176
# BB#173:                               # %if.else
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_175
# BB#174:                               # %if.then63
	movl	88(%r12), %edx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	fprintf
.LBB2_175:                              # %if.end65
	movq	16(%rbx), %rsi
	movq	%r12, %rdi
	callq	split_block
	movq	24(%rax), %r15
.LBB2_176:                              # %if.end68
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_178
# BB#177:                               # %if.then70
	movl	88(%r12), %ecx
	movl	88(%r14), %edx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	fprintf
.LBB2_178:                              # %if.end74
	movq	96(%r14), %rax
	addq	%rax, 96(%r15)
	movl	104(%r14), %eax
	movl	104(%r15), %ecx
	addq	%rax, %rcx
	movl	%ecx, 104(%r15)
	movq	40(%r15), %r10
	testq	%r10, %r10
	je	.LBB2_199
# BB#179:
	movabsq	$2147483648, %r8        # imm = 0x80000000
	movabsq	$3777893186295716171, %r9 # imm = 0x346DC5D63886594B
.LBB2_180:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_183 Depth 2
	movq	24(%r10), %r11
	movq	80(%r11), %rax
	andq	$2, %rax
	movq	%r11, %rcx
	je	.LBB2_182
# BB#181:                               # %if.then84
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	40(%r11), %rcx
	movq	24(%rcx), %rcx
.LBB2_182:                              # %if.end87
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	48(%rsp), %rdx          # 8-byte Reload
	.align	16, 0x90
.LBB2_183:                              # %for.cond89
                                        #   Parent Loop BB2_180 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %r12
	movq	24(%r12), %rdi
	movq	80(%rdi), %rsi
	andq	$2, %rsi
	movq	%rdi, %rbx
	je	.LBB2_185
# BB#184:                               # %if.then94
                                        #   in Loop: Header=BB2_183 Depth=2
	movq	40(%rdi), %rdx
	movq	24(%rdx), %rbx
.LBB2_185:                              # %if.end97
                                        #   in Loop: Header=BB2_183 Depth=2
	leaq	8(%r12), %rdx
	cmpq	%rbx, %rcx
	jne	.LBB2_183
# BB#186:                               # %for.end
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	56(%r12), %rcx
	addq	%rcx, 56(%r10)
	testq	%rax, %rax
	je	.LBB2_188
# BB#187:                               # %if.then108
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	56(%r12), %rax
	movq	40(%r11), %rcx
	addq	%rax, 56(%rcx)
	movq	56(%r12), %rax
	addq	%rax, 96(%r11)
	movq	16(%r10), %rax
	movl	104(%rax), %ecx
	movl	52(%r10), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r9
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movl	104(%r11), %ebx
	movq	$11, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	addq	%rdx, %rbx
	movl	%ebx, 104(%r11)
.LBB2_188:                              # %if.end124
                                        #   in Loop: Header=BB2_180 Depth=1
	testq	%rsi, %rsi
	je	.LBB2_195
# BB#189:                               # %if.then129
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	40(%rdi), %rax
	movq	56(%r12), %rdx
	movq	56(%rax), %rcx
	xorq	%r11, %r11
	subq	%rdx, %rcx
	movabsq	$0, %rdx
	js	.LBB2_191
# BB#190:                               # %if.then129
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	%rcx, %rdx
.LBB2_191:                              # %if.then129
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	%rdx, 56(%rax)
	movq	56(%r12), %rax
	movq	96(%rdi), %rsi
	subq	%rax, %rsi
	movq	%rsi, 96(%rdi)
	movq	16(%r10), %rax
	movl	104(%rax), %ecx
	movl	52(%r10), %eax
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r9
	movq	$63, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movq	$11, %rcx
	movl	104(%rdi), %eax
	sarq	%cl, %rdx
	addq	%rbx, %rdx
	subq	%rdx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB2_193
# BB#192:                               # %if.then129
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	%rax, %r11
.LBB2_193:                              # %if.then129
                                        #   in Loop: Header=BB2_180 Depth=1
	testq	%rsi, %rsi
	movl	%r11d, 104(%rdi)
	jns	.LBB2_195
# BB#194:                               # %if.then166
                                        #   in Loop: Header=BB2_180 Depth=1
	movq	$0, 96(%rdi)
.LBB2_195:                              # %if.end170
                                        #   in Loop: Header=BB2_180 Depth=1
	movl	104(%r15), %edx
	movl	104(%r14), %esi
	movl	52(%r10), %ecx
	movq	%rdx, %rax
	orq	%rsi, %rax
	testq	%r13, %rax
	jne	.LBB2_197
# BB#196:                               # %if.then176
                                        #   in Loop: Header=BB2_180 Depth=1
	movl	52(%r12), %edx
	addq	%rcx, %rdx
	movq	%rdx, %rax
	andq	%r8, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$33, %rcx
	sarq	%cl, %rax
	jmp	.LBB2_198
.LBB2_197:                              # %if.else182
                                        #   in Loop: Header=BB2_180 Depth=1
	movl	52(%r12), %eax
	imulq	%rdx, %rcx
	imulq	%rsi, %rax
	addq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
.LBB2_198:                              # %for.inc196
                                        #   in Loop: Header=BB2_180 Depth=1
	movl	%eax, 52(%r10)
	movq	8(%r10), %r10
	testq	%r10, %r10
	jne	.LBB2_180
.LBB2_199:                              # %for.end198
	movq	%r15, %rdi
	callq	update_br_prob_note
	movq	64(%rsp), %rdx          # 8-byte Reload
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB2_201
# BB#200:                               # %if.then200
	movq	24(%rdx), %rdx
	movl	(%rdx), %eax
.LBB2_201:                              # %if.end204
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$37, %rax
	jne	.LBB2_203
# BB#202:                               # %if.then208
	movq	24(%rdx), %rdx
.LBB2_203:                              # %if.end212
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	(%r13), %r12
	movq	%r15, %rdi
	callq	block_label
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	gen_jump
	movq	(%r13), %rsi
	movq	%rax, %rdi
	callq	emit_jump_insn_after
	movq	(%r13), %rax
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	delete_insn_chain
	movq	(%r13), %rdi
	movq	%r13, %rbx
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB2_205
# BB#204:                               # %lor.lhs.false226
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	je	.LBB2_206
.LBB2_205:                              # %if.then230
	movq	(%rbx), %rdi
	callq	emit_barrier_after
.LBB2_206:                              # %while.cond.preheader
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB2_208
.LBB2_207:                              # %while.body
                                        #   in Loop: Header=BB2_208 Depth=1
	callq	remove_edge
.LBB2_208:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB2_207
# BB#209:                               # %while.end
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	make_single_succ_edge
	movq	80(%r14), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	orq	$1, %rcx
	movq	%rcx, 80(%r14)
	movq	%r14, %rdi
	callq	forwarder_block_p
	movq	80(%r14), %rdx
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	andq	%rdx, %rcx
	testq	%rax, %rax
	je	.LBB2_211
# BB#210:
	orq	$2, %rcx
.LBB2_211:                              # %while.end
	movq	%rcx, 80(%r14)
	movq	$1, %r13
.LBB2_212:                              # %return
	movq	%r13, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	try_crossjump_to_edge, .Ltmp53-try_crossjump_to_edge
	.cfi_endproc

	.align	16, 0x90
	.type	insns_match_p,@function
insns_match_p:                          # @insns_match_p
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
	movq	%rdx, %r12
	movq	%rsi, %r13
	movzbl	(%r13), %eax
	movzbl	(%r12), %ecx
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	jne	.LBB3_48
# BB#1:                                 # %if.end
	movq	32(%r13), %rsi
	movq	32(%r12), %r15
	movl	(%rsi), %ecx
	movl	(%r15), %edx
	xorq	%rcx, %rdx
	testq	$65535, %rdx            # imm = 0xFFFF
	jne	.LBB3_48
# BB#2:                                 # %if.end12
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	cmpq	$34, %rax
	jne	.LBB3_4
# BB#3:                                 # %land.lhs.true
	movq	64(%r13), %rdi
	movq	64(%r12), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_48
.LBB3_4:                                # %if.end23
	testq	$4, %r14
	je	.LBB3_21
# BB#5:                                 # %land.lhs.true25
	movq	%r13, %rdi
	callq	stack_regs_mentioned
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_21
# BB#6:                                 # %if.then28
	movq	56(%r13), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	je	.LBB3_13
# BB#7:
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB3_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsi), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$65536, %rcx            # imm = 0x10000
	jne	.LBB3_12
# BB#9:                                 # %land.lhs.true36
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	8(%rsi), %rcx
	movzbl	(%rcx), %ebx
	cmpq	$61, %rbx
	jne	.LBB3_12
# BB#10:                                # %land.lhs.true43
                                        #   in Loop: Header=BB3_8 Depth=1
	movl	8(%rcx), %ecx
	movq	%rcx, %rbx
	andq	%rdi, %rbx
	cmpq	$8, %rbx
	jne	.LBB3_12
# BB#11:                                # %if.then58
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %rbx
	shlq	%cl, %rbx
	orq	%rbx, %rdx
	.align	16, 0x90
.LBB3_12:                               # %for.inc
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB3_8
.LBB3_13:                               # %for.end
	movq	56(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB3_20
# BB#14:
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB3_15:                               # %for.body74
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsi), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$65536, %rcx            # imm = 0x10000
	jne	.LBB3_19
# BB#16:                                # %land.lhs.true79
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	8(%rsi), %rcx
	movzbl	(%rcx), %ebx
	cmpq	$61, %rbx
	jne	.LBB3_19
# BB#17:                                # %land.lhs.true86
                                        #   in Loop: Header=BB3_15 Depth=1
	movl	8(%rcx), %ecx
	movq	%rcx, %rbx
	andq	%rdi, %rbx
	cmpq	$8, %rbx
	jne	.LBB3_19
# BB#18:                                # %if.then102
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	$1, %rbx
	shlq	%cl, %rbx
	orq	%rbx, %rax
	.align	16, 0x90
.LBB3_19:                               # %for.inc113
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB3_15
.LBB3_20:                               # %for.end117
	xorq	%rbx, %rbx
	cmpq	%rax, %rdx
	jne	.LBB3_48
.LBB3_21:                               # %if.end121
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_23
# BB#22:                                # %cond.true
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	rtx_renumbered_equal_p
	jmp	.LBB3_24
.LBB3_23:                               # %cond.false
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	rtx_equal_p
.LBB3_24:                               # %cond.false
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rbx
	jne	.LBB3_48
# BB#25:                                # %if.then127
	movq	%r13, %rdi
	callq	find_reg_equal_equiv_note
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	find_reg_equal_equiv_note
	movq	%rax, %rdx
	testq	%rbx, %rbx
	je	.LBB3_47
# BB#26:                                # %if.then127
	testq	%rdx, %rdx
	je	.LBB3_47
# BB#27:                                # %land.lhs.true133
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_34
# BB#28:                                # %lor.lhs.false
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$133, %rax
	jg	.LBB3_31
# BB#29:                                # %lor.lhs.false
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB3_47
# BB#30:                                # %lor.lhs.false
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB3_33
	jmp	.LBB3_47
.LBB3_31:                               # %lor.lhs.false
	cmpq	$134, %rax
	je	.LBB3_33
# BB#32:                                # %lor.lhs.false
	cmpq	$140, %rax
	jne	.LBB3_47
.LBB3_33:                               # %land.lhs.true190
	movq	8(%rdx), %rsi
	movq	%rdx, %r14
	callq	rtx_equal_p
	movq	%r14, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_47
.LBB3_34:                               # %if.then199
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB3_38
# BB#35:                                # %cond.true205
	movq	%rdx, %r14
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB3_37
# BB#36:                                # %cond.false217
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB3_37:                               # %cond.end223
	movq	%r14, %rdx
.LBB3_38:                               # %cond.end223
	movzbl	(%r12), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB3_47
# BB#39:                                # %cond.true232
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	movq	%rdx, %r14
	cmpq	$47, %rcx
	je	.LBB3_41
# BB#40:                                # %cond.false244
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB3_41:                               # %cond.end252
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB3_47
# BB#42:                                # %cond.end252
	testq	%rax, %rax
	je	.LBB3_47
# BB#43:                                # %land.lhs.true259
	movq	8(%rcx), %rdi
	movq	8(%rax), %rsi
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r14
	callq	rtx_renumbered_equal_p
	movq	%r14, %r8
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB3_47
# BB#44:                                # %if.then268
	movq	%rcx, %r14
	movq	8(%rbx), %rdx
	addq	$16, %rsi
	movq	$1, %rcx
	movq	$1, %rbx
	movq	%r13, %rdi
	movq	%r8, %r13
	callq	validate_change
	movq	8(%r14), %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	addq	$16, %r13
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	validate_change
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	rtx_renumbered_equal_p
	testq	%r14, %rax
	je	.LBB3_45
# BB#46:                                # %if.else
	callq	apply_change_group
	testq	%r14, %rax
	jne	.LBB3_48
	jmp	.LBB3_47
.LBB3_45:                               # %if.then285
	xorq	%rdi, %rdi
	callq	cancel_changes
.LBB3_47:                               # %if.end292
	xorq	%rbx, %rbx
.LBB3_48:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	insns_match_p, .Ltmp71-insns_match_p
	.cfi_endproc

	.align	16, 0x90
	.type	mark_effect,@function
mark_effect:                            # @mark_effect
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 48
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
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	movq	%rsi, %r14
	xorq	%r12, %r12
	cmpq	$47, %rax
	jne	.LBB4_1
# BB#20:                                # %sw.bb69
	movq	8(%rbx), %rdi
	movq	16(%rbx), %rsi
	callq	rtx_equal_for_cselib_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB4_40
# BB#21:                                # %if.end78
	movq	8(%rbx), %rbx
	movq	global_rtl(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB4_40
# BB#22:                                # %if.end85
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	movq	$1, %r12
	jne	.LBB4_41
# BB#23:                                # %if.end91
	movslq	8(%rbx), %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	bitmap_set_bit
	cmpq	$52, %r15
	jg	.LBB4_40
# BB#24:                                # %if.then97
	movzbl	2(%rbx), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r15, %rcx
	cmpq	$8, %rcx
	je	.LBB4_28
# BB#25:                                # %if.then97
	leaq	-21(%r15), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_28
# BB#26:                                # %if.then97
	leaq	-45(%r15), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_28
# BB#27:                                # %if.then97
	leaq	-29(%r15), %rcx
	andq	%r13, %rcx
	cmpq	$7, %rcx
	jbe	.LBB4_28
# BB#33:                                # %cond.false140
	cmpq	$24, %rax
	jne	.LBB4_34
# BB#36:                                # %cond.true156
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB4_39
.LBB4_1:                                # %entry
	cmpq	$49, %rax
	jne	.LBB4_40
# BB#2:                                 # %sw.bb
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB4_41
# BB#3:                                 # %if.then
	movslq	8(%rbx), %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	bitmap_clear_bit
	cmpq	$52, %r15
	jg	.LBB4_41
# BB#4:                                 # %if.then9
	movzbl	2(%rbx), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r15, %rcx
	cmpq	$8, %rcx
	je	.LBB4_8
# BB#5:                                 # %if.then9
	leaq	-21(%r15), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB4_8
# BB#6:                                 # %if.then9
	leaq	-45(%r15), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB4_8
# BB#7:                                 # %if.then9
	leaq	-29(%r15), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB4_8
# BB#13:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB4_14
# BB#16:                                # %cond.true44
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB4_18
.LBB4_8:                                # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$1, %rbx
	je	.LBB4_19
# BB#9:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB4_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %rbx
	incq	%rbx
	jmp	.LBB4_18
.LBB4_14:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB4_17
# BB#15:                                # %cond.true37
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB4_18
.LBB4_28:                               # %cond.true122
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$1, %rbx
	je	.LBB4_38
# BB#29:                                # %lor.rhs130
	cmpq	$6, %rax
	jne	.LBB4_30
# BB#31:                                # %lor.rhs130
	movabsq	$1, %rbx
	incq	%rbx
	jmp	.LBB4_39
.LBB4_10:
	movabsq	$0, %rbx
	incq	%rbx
	jmp	.LBB4_18
.LBB4_17:                               # %cond.false48
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rbx
	jmp	.LBB4_18
.LBB4_34:                               # %cond.false140
	cmpq	$18, %rax
	jne	.LBB4_37
# BB#35:                                # %cond.true146
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB4_39
.LBB4_30:
	movabsq	$0, %rbx
	incq	%rbx
	jmp	.LBB4_39
.LBB4_37:                               # %cond.false160
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rbx
	jmp	.LBB4_39
.LBB4_18:                               # %while.cond.preheader
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_41
.LBB4_19:                               # %while.body
	leaq	(%rbx,%r15), %rsi
	movq	%r14, %rdi
	callq	bitmap_clear_bit
	jmp	.LBB4_18
.LBB4_38:                               # %while.body186
	leaq	(%rbx,%r15), %rsi
	movq	%r14, %rdi
	callq	bitmap_set_bit
.LBB4_39:                               # %while.body186
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB4_38
.LBB4_40:
	xorq	%r12, %r12
.LBB4_41:                               # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp88:
	.size	mark_effect, .Ltmp88-mark_effect
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\n\ntry_optimize_cfg iteration %i\n\n"
	.size	.L.str, 34

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Deleting block %i.\n"
	.size	.L.str1, 20

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Deleted label in block %i.\n"
	.size	.L.str2, 28

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Deleting fallthru block %i.\n"
	.size	.L.str3, 29

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Splitting bb %i before %i insns\n"
	.size	.L.str4, 33

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Cross jumping from bb %i to bb %i; %i common insns\n"
	.size	.L.str5, 52

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Outcomes of branch in bb %i and %i differs to much (%i %i)\n"
	.size	.L.str6, 60

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Conditionals in bb %i and %i match.\n"
	.size	.L.str7, 37

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"cfgcleanup.c"
	.size	.L.str8, 13

	.type	.L__FUNCTION__.try_forward_edges,@object # @__FUNCTION__.try_forward_edges
.L__FUNCTION__.try_forward_edges:
	.asciz	"try_forward_edges"
	.size	.L__FUNCTION__.try_forward_edges, 18

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Infinite loop in BB %i.\n"
	.size	.L.str9, 25

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Conditionals threaded.\n"
	.size	.L.str10, 24

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Forwarding edge %i->%i to %i failed.\n"
	.size	.L.str11, 38

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Simplifying condjump %i around jump %i\n"
	.size	.L.str12, 40

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Merged %d and %d without moving.\n"
	.size	.L.str13, 34

	.type	.L__FUNCTION__.merge_blocks_move_predecessor_nojumps,@object # @__FUNCTION__.merge_blocks_move_predecessor_nojumps
.L__FUNCTION__.merge_blocks_move_predecessor_nojumps:
	.asciz	"merge_blocks_move_predecessor_nojumps"
	.size	.L__FUNCTION__.merge_blocks_move_predecessor_nojumps, 38

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Moved block %d before %d and merged.\n"
	.size	.L.str14, 38

	.type	.L__FUNCTION__.merge_blocks_move_successor_nojumps,@object # @__FUNCTION__.merge_blocks_move_successor_nojumps
.L__FUNCTION__.merge_blocks_move_successor_nojumps:
	.asciz	"merge_blocks_move_successor_nojumps"
	.size	.L__FUNCTION__.merge_blocks_move_successor_nojumps, 36

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Moved block %d after %d and merged.\n"
	.size	.L.str15, 37


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
