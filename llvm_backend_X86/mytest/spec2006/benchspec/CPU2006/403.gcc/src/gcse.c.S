	.file	"gcse.c.bc"
	.text
	.globl	gcse_main
	.align	16, 0x90
	.type	gcse_main,@function
gcse_main:                              # @gcse_main
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
	subq	$208, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 256
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
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	xorq	%r14, %r14
	testq	$16, %rax
	jne	.LBB0_457
# BB#1:                                 # %if.end
	movq	%rdi, %r15
	movq	stderr(%rip), %rax
	xorq	%r14, %r14
	movb	%r14b, run_jump_opt_after_gcse(%rip)
	movq	%rax, debug_stderr(%rip)
	movq	%rsi, gcse_file(%rip)
	movq	%rsi, %rbx
	callq	max_reg_num
	movq	%rbx, %rdi
	testq	%rdi, %rdi
	movl	%eax, max_gcse_regno(%rip)
	je	.LBB0_3
# BB#2:                                 # %if.then2
	movq	%rdi, %rbx
	callq	dump_flow_info
	movq	%rbx, %rdi
.LBB0_3:                                # %if.end3
	movslq	n_basic_blocks(%rip), %rsi
	cmpq	$2, %rsi
	jl	.LBB0_457
# BB#4:                                 # %if.end5
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	cmpq	$1001, %rsi             # imm = 0x3E9
	jl	.LBB0_9
# BB#5:                                 # %land.lhs.true
	movslq	n_edges(%rip), %rax
	cqto
	idivq	%rsi
	movq	%rax, %rdx
	movq	$32, %rcx
                                        # kill: RAX<def> RDX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$20, %rax
	jl	.LBB0_9
# BB#6:                                 # %if.then8
	movl	warn_disabled_optimization(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_457
# BB#7:                                 # %if.then10
	movabsq	$.L.str, %rdi
	jmp	.LBB0_8
.LBB0_9:                                # %if.end13
	movl	max_gcse_regno(%rip), %edx
	leaq	63(%rdx), %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$6, %rcx
	movq	compiler_params(%rip), %rdi
	shrq	%cl, %rax
	imulq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movslq	104(%rdi), %rdi
	andq	%rcx, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	cmpq	%rdi, %rax
	jbe	.LBB0_12
# BB#10:                                # %if.then19
	movl	warn_disabled_optimization(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_457
# BB#11:                                # %if.then21
	movabsq	$.L.str1, %rdi
.LBB0_8:                                # %return
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	warning
	jmp	.LBB0_457
.LBB0_12:                               # %if.end23
	movzbl	can_copy_init_p(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_19
# BB#13:                                # %if.then25
	xorq	%rbx, %rbx
	movb	%bl, can_copy_p+58(%rip)
	movw	%bx, can_copy_p+56(%rip)
	movq	$0, can_copy_p+48(%rip)
	movq	$0, can_copy_p+40(%rip)
	movq	$0, can_copy_p+32(%rip)
	movq	$0, can_copy_p+24(%rip)
	movq	$0, can_copy_p+16(%rip)
	movq	$0, can_copy_p+8(%rip)
	movq	$0, can_copy_p(%rip)
	callq	start_sequence
	movq	$1, %r14
	.align	16, 0x90
.LBB0_14:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	mode_class(,%rbx,4), %eax
	cmpq	$4, %rax
	jne	.LBB0_16
# BB#15:                                # %if.then.i
                                        #   in Loop: Header=BB0_14 Depth=1
	movq	$58, %rsi
	movq	%rbx, %rdi
	callq	gen_rtx_REG
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_17
.LBB0_16:                               # %if.else.i
                                        #   in Loop: Header=BB0_14 Depth=1
	movb	%r14b, can_copy_p(%rbx)
.LBB0_17:                               # %for.inc.i
                                        #   in Loop: Header=BB0_14 Depth=1
	incq	%rbx
	cmpq	$59, %rbx
	jne	.LBB0_14
# BB#18:                                # %compute_can_copy.exit
	callq	end_sequence
	movq	$1, %rax
	movb	%al, can_copy_init_p(%rip)
.LBB0_19:                               # %if.end26
	movabsq	$gcse_obstack, %rdi
	callq	gcc_obstack_init
	xorq	%rax, %rax
	movl	%eax, bytes_used(%rip)
	callq	init_alias_analysis
	movl	max_gcse_regno(%rip), %eax
	leaq	100(%rax), %rbx
	movl	bytes_used(%rip), %ecx
	movl	%ebx, reg_set_table_size(%rip)
	leaq	800(%rcx,%rax,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rbx        # imm = 0x1FFFFFFF
	shlq	$3, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, reg_set_table(%rip)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rbx, %rdx
	callq	memset
	movabsq	$reg_set_obstack, %rdi
	callq	gcc_obstack_init
	testq	%r15, %r15
	je	.LBB0_24
# BB#20:
	movabsq	$record_set_info, %r14
	movq	%r15, %rbx
	.align	16, 0x90
.LBB0_21:                               # %for.body.i162
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_23
# BB#22:                                # %if.then.i164
                                        #   in Loop: Header=BB0_21 Depth=1
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	note_stores
.LBB0_23:                               # %for.inc.i165
                                        #   in Loop: Header=BB0_21 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_21
.LBB0_24:                               # %compute_sets.exit
	movq	gcse_obstack+32(%rip), %rcx
	movq	gcse_obstack+24(%rip), %rax
	subq	%rax, %rcx
	testq	%rcx, %rcx
	jg	.LBB0_26
# BB#25:                                # %if.then.i167
	movabsq	$gcse_obstack, %rdi
	movq	$1, %rsi
	callq	_obstack_newchunk
	movq	gcse_obstack+24(%rip), %rax
.LBB0_26:                               # %if.end.i
	incq	%rax
	movq	%rax, gcse_obstack+24(%rip)
	movq	gcse_obstack+16(%rip), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	cmpq	%rcx, %rax
	movq	%r15, %r14
	movq	%r14, 40(%rsp)          # 8-byte Spill
	jne	.LBB0_28
# BB#27:                                # %if.then9.i
	movzbl	gcse_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, gcse_obstack+80(%rip)
.LBB0_28:                               # %if.end10.i
	movslq	gcse_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, gcse_obstack+24(%rip)
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB0_30
# BB#29:                                # %if.then30.i
	movq	%rax, gcse_obstack+24(%rip)
	movq	%rax, %rcx
.LBB0_30:                               # %gcse_alloc.exit
	movq	%rcx, gcse_obstack+16(%rip)
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_31:                               # %land.rhs
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_42 Depth 2
                                        #       Child Loop BB0_44 Depth 3
                                        #         Child Loop BB0_47 Depth 4
                                        #           Child Loop BB0_50 Depth 5
                                        #         Child Loop BB0_57 Depth 4
                                        #           Child Loop BB0_63 Depth 5
                                        #         Child Loop BB0_69 Depth 4
                                        #     Child Loop BB0_74 Depth 2
                                        #     Child Loop BB0_75 Depth 2
                                        #       Child Loop BB0_77 Depth 3
                                        #     Child Loop BB0_83 Depth 2
                                        #       Child Loop BB0_84 Depth 3
                                        #         Child Loop BB0_87 Depth 4
                                        #     Child Loop BB0_92 Depth 2
                                        #       Child Loop BB0_95 Depth 3
                                        #         Child Loop BB0_96 Depth 4
                                        #     Child Loop BB0_106 Depth 2
                                        #       Child Loop BB0_107 Depth 3
                                        #         Child Loop BB0_116 Depth 4
                                        #         Child Loop BB0_122 Depth 4
                                        #     Child Loop BB0_172 Depth 2
                                        #       Child Loop BB0_174 Depth 3
                                        #         Child Loop BB0_182 Depth 4
                                        #         Child Loop BB0_195 Depth 4
                                        #     Child Loop BB0_208 Depth 2
                                        #       Child Loop BB0_209 Depth 3
                                        #         Child Loop BB0_212 Depth 4
                                        #           Child Loop BB0_214 Depth 5
                                        #     Child Loop BB0_227 Depth 2
                                        #     Child Loop BB0_241 Depth 2
                                        #       Child Loop BB0_242 Depth 3
                                        #     Child Loop BB0_240 Depth 2
                                        #       Child Loop BB0_248 Depth 3
                                        #     Child Loop BB0_254 Depth 2
                                        #       Child Loop BB0_256 Depth 3
                                        #     Child Loop BB0_259 Depth 2
                                        #       Child Loop BB0_260 Depth 3
                                        #         Child Loop BB0_263 Depth 4
                                        #     Child Loop BB0_282 Depth 2
                                        #       Child Loop BB0_284 Depth 3
                                        #         Child Loop BB0_289 Depth 4
                                        #           Child Loop BB0_294 Depth 5
                                        #     Child Loop BB0_309 Depth 2
                                        #       Child Loop BB0_310 Depth 3
                                        #         Child Loop BB0_312 Depth 4
                                        #           Child Loop BB0_315 Depth 5
                                        #     Child Loop BB0_344 Depth 2
                                        #     Child Loop BB0_350 Depth 2
                                        #     Child Loop BB0_365 Depth 2
                                        #       Child Loop BB0_368 Depth 3
                                        #         Child Loop BB0_369 Depth 4
                                        #     Child Loop BB0_377 Depth 2
                                        #       Child Loop BB0_380 Depth 3
                                        #     Child Loop BB0_389 Depth 2
                                        #       Child Loop BB0_391 Depth 3
                                        #     Child Loop BB0_394 Depth 2
                                        #       Child Loop BB0_396 Depth 3
                                        #         Child Loop BB0_399 Depth 4
                                        #       Child Loop BB0_421 Depth 3
                                        #         Child Loop BB0_424 Depth 4
                                        #           Child Loop BB0_430 Depth 5
	movq	compiler_params(%rip), %rax
	movslq	128(%rax), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB0_451
# BB#32:                                # %while.body
                                        #   in Loop: Header=BB0_31 Depth=1
	incq	%rsi
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	movq	%rsi, %rbx
	je	.LBB0_34
# BB#33:                                # %if.then34
                                        #   in Loop: Header=BB0_31 Depth=1
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str2, %rsi
	movq	%rbx, %rdx
	callq	fprintf
.LBB0_34:                               # %if.end37
                                        #   in Loop: Header=BB0_31 Depth=1
	movl	bytes_used(%rip), %eax
	movl	%eax, bytes_used(%rip)
	callq	max_reg_num
	movl	%eax, max_gcse_regno(%rip)
	movq	%r14, %rdi
	callq	alloc_gcse_mem
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	callq	one_cprop_pass
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movl	optimize_size(%rip), %ebx
	movl	max_cuid(%rip), %edx
	xorq	%rax, %rax
	movl	%eax, gcse_subst_count(%rip)
	movl	%eax, gcse_create_count(%rip)
	movq	$1, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	cmpq	$22, %rdx
	movq	$11, %rdi
	jb	.LBB0_36
# BB#35:                                # %if.end37
                                        #   in Loop: Header=BB0_31 Depth=1
	orq	$1, %rax
	movq	%rax, %rdi
.LBB0_36:                               # %if.end37
                                        #   in Loop: Header=BB0_31 Depth=1
	movl	bytes_used(%rip), %eax
	movl	%edi, expr_hash_table_size(%rip)
	leaq	(%rax,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	movq	%rax, expr_hash_table(%rip)
	jne	.LBB0_37
# BB#170:                               # %if.else
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r15, 48(%rsp)          # 8-byte Spill
	callq	add_noreturn_fake_exit_edges
	movl	flag_gcse_lm(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_207
# BB#171:                               # %if.then.i188
                                        #   in Loop: Header=BB0_31 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	movq	$0, pre_ldst_mems(%rip)
	xorq	%r13, %r13
	testq	%rax, %rax
	jle	.LBB0_207
	.align	16, 0x90
.LBB0_172:                              # %for.body.i.i194
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_174 Depth 3
                                        #         Child Loop BB0_182 Depth 4
                                        #         Child Loop BB0_195 Depth 4
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r13,8), %rcx
	movq	(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB0_206
# BB#173:                               # %land.rhs.i.preheader.i195
                                        #   in Loop: Header=BB0_172 Depth=2
	movq	8(%rcx), %rcx
	movq	24(%rcx), %rcx
	cmpq	%rcx, %rbx
	je	.LBB0_206
	.align	16, 0x90
.LBB0_174:                              # %for.body9.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_182 Depth 4
                                        #         Child Loop BB0_195 Depth 4
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_203
# BB#175:                               # %if.then.i.i200
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB0_202
# BB#176:                               # %if.then21.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	16(%rdi), %r14
	movl	(%r14), %eax
	movq	8(%rdi), %r15
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB0_188
# BB#177:                               # %land.lhs.true.i.i201
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$66, %rcx
	jne	.LBB0_188
# BB#178:                               # %land.lhs.true.i.i201
                                        #   in Loop: Header=BB0_174 Depth=3
	andq	$16711680, %rax         # imm = 0xFF0000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB0_188
# BB#179:                               # %simple_mem.exit.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	8(%r14), %rdi
	xorq	%rsi, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_188
# BB#180:                               # %if.then39.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	pre_ldst_mems(%rip), %r12
	jmp	.LBB0_182
	.align	16, 0x90
.LBB0_181:                              # %for.cond.i.i.i
                                        #   in Loop: Header=BB0_182 Depth=4
	movq	32(%r12), %r12
.LBB0_182:                              # %for.cond.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        #       Parent Loop BB0_174 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%r12, %r12
	je	.LBB0_184
# BB#183:                               # %for.body.i.i.i206
                                        #   in Loop: Header=BB0_182 Depth=4
	movq	8(%r12), %rdi
	movq	%r14, %rsi
	callq	expr_equiv_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_181
	jmp	.LBB0_185
	.align	16, 0x90
.LBB0_202:                              # %if.else75.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	callq	invalidate_any_buried_refs
	jmp	.LBB0_203
	.align	16, 0x90
.LBB0_188:                              # %if.else48.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	%r14, %rdi
	callq	invalidate_any_buried_refs
.LBB0_189:                              # %if.end49.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB0_203
# BB#190:                               # %land.lhs.true54.i.i210
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$66, %rcx
	jne	.LBB0_203
# BB#191:                               # %land.lhs.true54.i.i210
                                        #   in Loop: Header=BB0_174 Depth=3
	andq	$16711680, %rax         # imm = 0xFF0000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB0_203
# BB#192:                               # %simple_mem.exit123.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	8(%r15), %rdi
	xorq	%rsi, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_203
# BB#193:                               # %if.then57.i.i211
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	pre_ldst_mems(%rip), %r12
	jmp	.LBB0_195
	.align	16, 0x90
.LBB0_194:                              # %for.cond.i129.i.i
                                        #   in Loop: Header=BB0_195 Depth=4
	movq	32(%r12), %r12
.LBB0_195:                              # %for.cond.i129.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_172 Depth=2
                                        #       Parent Loop BB0_174 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%r12, %r12
	je	.LBB0_197
# BB#196:                               # %for.body.i134.i.i
                                        #   in Loop: Header=BB0_195 Depth=4
	movq	8(%r12), %rdi
	movq	%r15, %rsi
	callq	expr_equiv_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_194
	jmp	.LBB0_198
.LBB0_184:                              # %if.then4.i.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	$64, %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	pre_ldst_mems(%rip), %rax
	movq	%rax, 32(%r12)
	movq	$0, (%r12)
	movq	%r14, 8(%r12)
	movq	$0, 56(%r12)
	movq	$0, 40(%r12)
	xorq	%rax, %rax
	movl	%eax, 48(%r12)
	movq	$0, 24(%r12)
	movq	$0, 16(%r12)
	movq	%r12, pre_ldst_mems(%rip)
.LBB0_185:                              # %ldst_entry.exit.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB0_187
# BB#186:                               # %if.then45.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	16(%r12), %rsi
	movq	%rbx, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 16(%r12)
	jmp	.LBB0_189
.LBB0_197:                              # %if.then4.i144.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	$64, %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	pre_ldst_mems(%rip), %rax
	movq	%rax, 32(%r12)
	movq	$0, (%r12)
	movq	%r15, 8(%r12)
	movq	$0, 56(%r12)
	movq	$0, 40(%r12)
	xorq	%rax, %rax
	movl	%eax, 48(%r12)
	movq	$0, 24(%r12)
	movq	$0, 16(%r12)
	movq	%r12, pre_ldst_mems(%rip)
.LBB0_198:                              # %ldst_entry.exit146.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movzbl	(%r14), %eax
	cmpq	$41, %rax
	je	.LBB0_201
# BB#199:                               # %ldst_entry.exit146.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	cmpq	$66, %rax
	jne	.LBB0_200
.LBB0_201:                              # %if.else71.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	$1, %rax
	movl	%eax, 40(%r12)
	jmp	.LBB0_203
.LBB0_187:                              # %if.else.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	$1, %rax
	movl	%eax, 40(%r12)
	jmp	.LBB0_189
.LBB0_200:                              # %if.then68.i.i
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	24(%r12), %rsi
	movq	%rbx, %rdi
	callq	alloc_INSN_LIST
	movq	%rax, 24(%r12)
	.align	16, 0x90
.LBB0_203:                              # %for.inc.i.i214
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB0_205
# BB#204:                               # %for.inc.land.rhs_crit_edge.i.i219
                                        #   in Loop: Header=BB0_174 Depth=3
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rbx
	jne	.LBB0_174
.LBB0_205:                              # %for.inc84.i.loopexit.i.loopexit
                                        #   in Loop: Header=BB0_172 Depth=2
	movl	n_basic_blocks(%rip), %eax
.LBB0_206:                              # %for.inc84.i.i
                                        #   in Loop: Header=BB0_172 Depth=2
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_172
.LBB0_207:                              # %if.end.i222
                                        #   in Loop: Header=BB0_31 Depth=1
	xorq	%r14, %r14
	movl	expr_hash_table_size(%rip), %edx
	movq	expr_hash_table(%rip), %rdi
	movl	%r14d, n_exprs(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	xorq	%rdi, %rdi
	callq	compute_hash_table
	movq	pre_ldst_mems(%rip), %rax
	testq	%rax, %rax
	je	.LBB0_222
	jmp	.LBB0_208
.LBB0_37:                               # %if.then42
                                        #   in Loop: Header=BB0_31 Depth=1
	movl	max_cuid(%rip), %r14d
	movl	n_basic_blocks(%rip), %ebx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, rd_kill(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, rd_gen(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, reaching_defs(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, rd_out(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movl	expr_hash_table_size(%rip), %edx
	movq	expr_hash_table(%rip), %rdi
	xorq	%rax, %rax
	movl	%eax, n_exprs(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	xorq	%rdi, %rdi
	callq	compute_hash_table
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_39
# BB#38:                                # %if.then.i172
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %rdx
	movl	n_exprs(%rip), %r8d
	movl	expr_hash_table_size(%rip), %ecx
	movabsq	$.L.str15, %rsi
	callq	dump_hash_table
.LBB0_39:                               # %if.end.i174
                                        #   in Loop: Header=BB0_31 Depth=1
	movslq	n_exprs(%rip), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	jle	.LBB0_167
# BB#40:                                # %if.then1.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movslq	n_basic_blocks(%rip), %r15
	xorq	%r14, %r14
	testq	%r15, %r15
	jle	.LBB0_75
# BB#41:                                # %for.cond1.preheader.lr.ph.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$0, 184(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_42:                               # %for.cond1.preheader.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_44 Depth 3
                                        #         Child Loop BB0_47 Depth 4
                                        #           Child Loop BB0_50 Depth 5
                                        #         Child Loop BB0_57 Depth 4
                                        #           Child Loop BB0_63 Depth 5
                                        #         Child Loop BB0_69 Depth 4
	movq	$32, %rcx
	movl	max_cuid(%rip), %eax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_73
# BB#43:                                # %for.body3.lr.ph.i.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	rd_gen(%rip), %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
	.align	16, 0x90
.LBB0_44:                               # %for.body3.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_47 Depth 4
                                        #           Child Loop BB0_50 Depth 5
                                        #         Child Loop BB0_57 Depth 4
                                        #           Child Loop BB0_63 Depth 5
                                        #         Child Loop BB0_69 Depth 4
	movq	$3, %rcx
	movq	%r12, %rax
	shrq	%cl, %rax
	andq	$536870904, %rax        # imm = 0x1FFFFFF8
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx,%rax), %rax
	movq	$1, %rdx
	movq	%r12, %rcx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	je	.LBB0_72
# BB#45:                                # %if.then.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	cuid_insn(%rip), %rax
	movq	(%rax,%r12,8), %r14
	movzbl	(%r14), %eax
	movq	32(%r14), %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	cmpq	$34, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jne	.LBB0_54
# BB#46:                                # %for.cond11.preheader.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	xorq	%r10, %r10
	.align	16, 0x90
.LBB0_47:                               # %for.body13.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_50 Depth 5
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rsi
	movq	%r10, %rcx
	shlq	%cl, %rsi
	testq	%rax, %rsi
	je	.LBB0_53
# BB#48:                                # %if.then17.i.i
                                        #   in Loop: Header=BB0_47 Depth=4
	movq	reg_set_table(%rip), %rax
	movq	(%rax,%r10,8), %rdi
	testq	%rdi, %rdi
	je	.LBB0_53
# BB#49:                                # %for.body.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_47 Depth=4
	movq	basic_block_for_insn(%rip), %r11
	movslq	8(%r14), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%r11,%rax,8), %rax
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	32(%rcx,%rsi,8), %r9
	movl	88(%rax), %r8d
	.align	16, 0x90
.LBB0_50:                               # %for.body.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        #         Parent Loop BB0_47 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%rdi), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%r11,%rcx,8), %rax
	movl	88(%rax), %eax
	movq	%r8, %r13
	andq	%rdx, %r13
	cmpq	%r13, %rax
	je	.LBB0_52
# BB#51:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB0_50 Depth=5
	movq	uid_cuid(%rip), %rax
	movslq	88(%r9), %rsi
	movq	%rdx, %rbx
	movq	rd_kill(%rip), %rdx
	movl	(%rax,%rcx,4), %ecx
	movq	(%rdx,%rsi,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rax,%rcx,8)
	movq	%rbx, %rdx
.LBB0_52:                               # %for.inc.i.i.i
                                        #   in Loop: Header=BB0_50 Depth=5
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_50
.LBB0_53:                               # %for.inc.i.i
                                        #   in Loop: Header=BB0_47 Depth=4
	incq	%r10
	cmpq	$53, %r10
	jne	.LBB0_47
.LBB0_54:                               # %if.end21.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$47, %rax
	jne	.LBB0_55
# BB#66:                                # %land.lhs.true74.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	8(%rcx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_72
# BB#67:                                # %if.then81.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movslq	8(%rax), %rax
	movq	reg_set_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rdx
	testq	%rdx, %rdx
	je	.LBB0_72
# BB#68:                                # %for.body.lr.ph.i145.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	basic_block_for_insn(%rip), %rsi
	movslq	8(%r14), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%rsi,%rax,8), %rax
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	32(%rcx,%rdi,8), %r9
	movl	88(%rax), %r8d
	.align	16, 0x90
.LBB0_69:                               # %for.body.i154.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rdx), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%rsi,%rcx,8), %rbx
	movl	88(%rbx), %ebx
	movq	%r8, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	%rax, %rbx
	je	.LBB0_71
# BB#70:                                # %if.then.i165.i.i
                                        #   in Loop: Header=BB0_69 Depth=4
	movq	uid_cuid(%rip), %rax
	movslq	88(%r9), %rbx
	movq	rd_kill(%rip), %rdi
	movl	(%rax,%rcx,4), %ecx
	movq	(%rdi,%rbx,8), %rax
	movq	$1, %rdi
	shlq	%cl, %rdi
	shrq	$6, %rcx
	orq	%rdi, 16(%rax,%rcx,8)
.LBB0_71:                               # %for.inc.i169.i.i
                                        #   in Loop: Header=BB0_69 Depth=4
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB0_69
	jmp	.LBB0_72
	.align	16, 0x90
.LBB0_55:                               # %if.end21.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	cmpq	$39, %rax
	jne	.LBB0_72
# BB#56:                                # %if.then25.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	movq	8(%rcx), %r11
	movslq	(%r11), %r8
	testq	%r8, %r8
	jle	.LBB0_72
	.align	16, 0x90
.LBB0_57:                               # %for.body30.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_63 Depth 5
	movq	(%r11,%r8,8), %rax
	movzbl	(%rax), %ecx
	decq	%r8
	cmpq	$47, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB0_60
# BB#58:                                # %for.body30.i.i
                                        #   in Loop: Header=BB0_57 Depth=4
	cmpq	$49, %rcx
	jne	.LBB0_59
.LBB0_60:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_57 Depth=4
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_59
# BB#61:                                # %if.then52.i.i
                                        #   in Loop: Header=BB0_57 Depth=4
	movslq	8(%rax), %rax
	movq	reg_set_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rdi
	testq	%rdi, %rdi
	je	.LBB0_59
# BB#62:                                # %for.body.lr.ph.i187.i.i
                                        #   in Loop: Header=BB0_57 Depth=4
	movq	basic_block_for_insn(%rip), %r13
	movslq	8(%r14), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%r13,%rax,8), %rax
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	32(%rcx,%rsi,8), %r10
	movl	88(%rax), %r9d
	.align	16, 0x90
.LBB0_63:                               # %for.body.i196.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_44 Depth=3
                                        #         Parent Loop BB0_57 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%rdi), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%r13,%rcx,8), %rbx
	movl	88(%rbx), %ebx
	movq	%r9, %rax
	andq	%rdx, %rax
	cmpq	%rax, %rbx
	je	.LBB0_65
# BB#64:                                # %if.then.i207.i.i
                                        #   in Loop: Header=BB0_63 Depth=5
	movq	uid_cuid(%rip), %rax
	movslq	88(%r10), %rbx
	movq	%rdx, %rsi
	movq	rd_kill(%rip), %rdx
	movl	(%rax,%rcx,4), %ecx
	movq	(%rdx,%rbx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rax,%rcx,8)
	movq	%rsi, %rdx
.LBB0_65:                               # %for.inc.i211.i.i
                                        #   in Loop: Header=BB0_63 Depth=5
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_63
	.align	16, 0x90
.LBB0_59:                               # %for.cond28.backedge.i.i
                                        #   in Loop: Header=BB0_57 Depth=4
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_57
	.align	16, 0x90
.LBB0_72:                               # %for.inc95.i.i
                                        #   in Loop: Header=BB0_44 Depth=3
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movl	max_cuid(%rip), %ecx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_44
.LBB0_73:                               # %for.inc98.i.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	incq	%rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	%r15, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	movq	$1, %rbx
	jne	.LBB0_42
	.align	16, 0x90
.LBB0_74:                               # %for.body.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	rd_out(%rip), %rax
	movq	rd_gen(%rip), %rcx
	movq	-8(%rax,%rbx,8), %rdi
	movq	-8(%rcx,%rbx,8), %rsi
	callq	sbitmap_copy
	movslq	n_basic_blocks(%rip), %r15
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	xorq	%r14, %r14
	cmpq	%r15, %rax
	jl	.LBB0_74
	.align	16, 0x90
.LBB0_75:                               # %for.cond3.preheader.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_77 Depth 3
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$0, %rax
	jle	.LBB0_76
	.align	16, 0x90
.LBB0_77:                               # %for.body5.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_75 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	reaching_defs(%rip), %rcx
	movq	(%rcx,%rbx,8), %rdi
	movq	rd_out(%rip), %rsi
	movq	%rax, %r15
	movq	%rbx, %rdx
	callq	sbitmap_union_of_preds
	movq	rd_out(%rip), %rsi
	movq	rd_gen(%rip), %rcx
	movq	reaching_defs(%rip), %rdx
	movq	rd_kill(%rip), %rax
	movq	(%rsi,%rbx,8), %rdi
	movq	(%rcx,%rbx,8), %rsi
	movq	(%rdx,%rbx,8), %rdx
	movq	(%rax,%rbx,8), %rcx
	callq	sbitmap_union_of_diff
	orq	%r15, %rax
	incq	%rbx
	movslq	n_basic_blocks(%rip), %r15
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r15, %rdx
	jl	.LBB0_77
# BB#78:                                # %for.end18.i.i
                                        #   in Loop: Header=BB0_75 Depth=2
	incq	%r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_75
	jmp	.LBB0_79
.LBB0_220:                              # %if.else.i20.i
                                        #   in Loop: Header=BB0_208 Depth=2
	movq	pre_ldst_mems(%rip), %rax
	movq	32(%rax), %rax
	movq	%rax, pre_ldst_mems(%rip)
	leaq	16(%r15), %rdi
	callq	free_INSN_LIST_list
	leaq	24(%r15), %rdi
	callq	free_INSN_LIST_list
	movq	%r15, %rdi
	callq	free
	xorq	%r15, %r15
	movabsq	$pre_ldst_mems, %rax
	jmp	.LBB0_221
	.align	16, 0x90
.LBB0_208:                              # %while.body.lr.ph.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_209 Depth 3
                                        #         Child Loop BB0_212 Depth 4
                                        #           Child Loop BB0_214 Depth 5
	movq	%rax, %r15
.LBB0_209:                              # %while.body.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_208 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_212 Depth 4
                                        #           Child Loop BB0_214 Depth 5
	movl	40(%r15), %eax
	testq	%rax, %rax
	jne	.LBB0_218
# BB#210:                               # %while.body.i.i
                                        #   in Loop: Header=BB0_209 Depth=3
	movl	expr_hash_table_size(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_218
# BB#211:                               # %for.body.lr.ph.i.i223
                                        #   in Loop: Header=BB0_209 Depth=3
	xorq	%r12, %r12
.LBB0_212:                              # %for.body.i18.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_208 Depth=2
                                        #       Parent Loop BB0_209 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_214 Depth 5
	movq	expr_hash_table(%rip), %rcx
	movq	%r12, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	movq	(%rcx,%rdx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB0_217
	.align	16, 0x90
.LBB0_214:                              # %for.body5.i.i227
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_208 Depth=2
                                        #       Parent Loop BB0_209 Depth=3
                                        #         Parent Loop BB0_212 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	(%rbx), %rdi
	movq	8(%r15), %rsi
	callq	expr_equiv_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_215
# BB#213:                               # %for.cond3.i.i
                                        #   in Loop: Header=BB0_214 Depth=5
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_214
# BB#216:                               # %for.inc10.i.loopexit.i
                                        #   in Loop: Header=BB0_212 Depth=4
	movl	expr_hash_table_size(%rip), %eax
.LBB0_217:                              # %for.inc10.i.i
                                        #   in Loop: Header=BB0_212 Depth=4
	incq	%r12
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rcx
	movq	%rax, %rdx
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB0_212
	.align	16, 0x90
.LBB0_218:                              # %if.then14.i.i
                                        #   in Loop: Header=BB0_209 Depth=3
	testq	%r14, %r14
	je	.LBB0_220
# BB#219:                               # %if.then16.i.i
                                        #   in Loop: Header=BB0_209 Depth=3
	movq	32(%r15), %rax
	movq	%rax, 32(%r14)
	leaq	16(%r15), %rdi
	callq	free_INSN_LIST_list
	leaq	24(%r15), %rdi
	callq	free_INSN_LIST_list
	movq	%r15, %rdi
	callq	free
	movq	32(%r14), %r15
	testq	%r15, %r15
	jne	.LBB0_209
	jmp	.LBB0_222
	.align	16, 0x90
.LBB0_215:                              # %if.else21.i.i
                                        #   in Loop: Header=BB0_208 Depth=2
	movq	%rbx, (%r15)
	leaq	32(%r15), %rax
.LBB0_221:                              # %while.cond.outer.backedge.i.i
                                        #   in Loop: Header=BB0_208 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%r15, %r14
	jne	.LBB0_208
.LBB0_222:                              # %while.end.i.i230
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	gcse_file(%rip), %r14
	testq	%r14, %r14
	je	.LBB0_223
# BB#224:                               # %while.end.i.i230
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	pre_ldst_mems(%rip), %rax
	testq	%rax, %rax
	je	.LBB0_225
# BB#226:                               # %if.then27.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$12, %rsi
	movq	$1, %rdx
	movabsq	$.L.str25, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	pre_ldst_mems(%rip), %rbx
	testq	%rbx, %rbx
	movq	48(%rsp), %r15          # 8-byte Reload
	je	.LBB0_234
	.align	16, 0x90
.LBB0_227:                              # %for.body.i.i25.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	44(%rbx), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str26, %rsi
	callq	fprintf
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	print_rtl
	movq	$11, %rsi
	movq	$1, %rdx
	movabsq	$.L.str27, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	16(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB0_228
# BB#229:                               # %if.else.i.i.i
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str28, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB0_230
	.align	16, 0x90
.LBB0_228:                              # %if.then.i.i.i231
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	%r14, %rdi
	callq	print_rtl
.LBB0_230:                              # %if.end.i.i.i232
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	$11, %rsi
	movq	$1, %rdx
	movabsq	$.L.str29, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	24(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB0_231
# BB#232:                               # %if.else10.i.i.i
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str28, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB0_233
	.align	16, 0x90
.LBB0_231:                              # %if.then8.i.i.i
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	%r14, %rdi
	callq	print_rtl
.LBB0_233:                              # %if.end12.i.i.i
                                        #   in Loop: Header=BB0_227 Depth=2
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str30, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_227
.LBB0_234:                              # %print_ldst_list.exit.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	gcse_file(%rip), %r14
	jmp	.LBB0_235
.LBB0_76:                               # %for.end18.thread.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	incq	%r14
.LBB0_79:                               # %while.end.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_81
# BB#80:                                # %if.then.i18.i
                                        #   in Loop: Header=BB0_31 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str50, %rsi
	movq	%r14, %rdx
	callq	fprintf
	movl	n_basic_blocks(%rip), %r15d
.LBB0_81:                               # %compute_rd.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movl	n_exprs(%rip), %ebx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, ae_kill(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, ae_gen(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, ae_in(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, ae_out(%rip)
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	movl	expr_hash_table_size(%rip), %r10d
	testq	%r10, %r10
	je	.LBB0_90
# BB#82:                                # %for.body.lr.ph.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %r8
	xorq	%r9, %r9
	.align	16, 0x90
.LBB0_83:                               # %for.body.i26.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_84 Depth 3
                                        #         Child Loop BB0_87 Depth 4
	movq	(%r8,%r9,8), %r11
	jmp	.LBB0_84
	.align	16, 0x90
.LBB0_88:                               # %for.inc15.i.i
                                        #   in Loop: Header=BB0_84 Depth=3
	movq	16(%r11), %r11
.LBB0_84:                               # %for.body.i26.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_83 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_87 Depth 4
	testq	%r11, %r11
	je	.LBB0_89
# BB#85:                                # %for.body3.i27.i
                                        #   in Loop: Header=BB0_84 Depth=3
	movq	32(%r11), %rsi
	testq	%rsi, %rsi
	je	.LBB0_88
# BB#86:                                # %for.body6.lr.ph.i.i
                                        #   in Loop: Header=BB0_84 Depth=3
	movl	8(%r11), %eax
	movq	basic_block_for_insn(%rip), %rdi
	movq	ae_gen(%rip), %rdx
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	movq	$6, %rcx
	shrq	%cl, %rax
	.align	16, 0x90
.LBB0_87:                               # %for.body6.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_83 Depth=2
                                        #       Parent Loop BB0_84 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rsi), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%rdi,%rcx,8), %rcx
	movslq	88(%rcx), %rcx
	movq	(%rdx,%rcx,8), %rcx
	orq	%rbx, 16(%rcx,%rax,8)
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB0_87
	jmp	.LBB0_88
	.align	16, 0x90
.LBB0_89:                               # %for.inc17.i.i
                                        #   in Loop: Header=BB0_83 Depth=2
	incq	%r9
	movq	%r9, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r10, %rax
	jne	.LBB0_83
.LBB0_90:                               # %compute_ae_gen.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	ae_gen(%rip), %r15
	movslq	n_basic_blocks(%rip), %rax
	movq	ae_kill(%rip), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	testq	%rax, %rax
	movq	%r10, %rsi
	jle	.LBB0_91
	.align	16, 0x90
.LBB0_92:                               # %for.cond1.preheader.i42.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_95 Depth 3
                                        #         Child Loop BB0_96 Depth 4
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rsi
	jne	.LBB0_94
# BB#93:                                #   in Loop: Header=BB0_92 Depth=2
	xorq	%rsi, %rsi
	jmp	.LBB0_103
	.align	16, 0x90
.LBB0_94:                               # %for.body3.lr.ph.i44.i
                                        #   in Loop: Header=BB0_92 Depth=2
	xorq	%r12, %r12
	.align	16, 0x90
.LBB0_95:                               # %for.body3.i47.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_92 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_96 Depth 4
	movq	expr_hash_table(%rip), %rax
	movq	%r12, %rcx
	andq	%r14, %rcx
	movq	(%rax,%rcx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB0_101
	.align	16, 0x90
.LBB0_96:                               # %for.body5.i53.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_92 Depth=2
                                        #       Parent Loop BB0_95 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	8(%rbx), %ecx
	movq	(%r15,%r13,8), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB0_99
# BB#97:                                # %if.end.i.i
                                        #   in Loop: Header=BB0_96 Depth=4
	movq	basic_block_info(%rip), %rax
	movq	(%rbx), %rdi
	movq	32(%rax,%r13,8), %rsi
	callq	expr_killed_p
	testq	%r14, %rax
	je	.LBB0_99
# BB#98:                                # %if.then17.i61.i
                                        #   in Loop: Header=BB0_96 Depth=4
	movl	8(%rbx), %ecx
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%r13,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rax,%rcx,8)
.LBB0_99:                               # %for.inc.i65.i
                                        #   in Loop: Header=BB0_96 Depth=4
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_96
# BB#100:                               # %for.cond4.for.inc29_crit_edge.i.i
                                        #   in Loop: Header=BB0_95 Depth=3
	movl	expr_hash_table_size(%rip), %r10d
.LBB0_101:                              # %for.inc29.i.i
                                        #   in Loop: Header=BB0_95 Depth=3
	incq	%r12
	movq	%r12, %rax
	andq	%r14, %rax
	movq	%r10, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jb	.LBB0_95
# BB#102:                               # %for.cond1.for.inc31_crit_edge.i.i
                                        #   in Loop: Header=BB0_92 Depth=2
	movl	n_basic_blocks(%rip), %eax
	movq	%r10, %rsi
.LBB0_103:                              # %for.inc31.i.i
                                        #   in Loop: Header=BB0_92 Depth=2
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rdx
	jl	.LBB0_92
# BB#104:                               # %compute_ae_kill.exit.loopexit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	ae_gen(%rip), %r15
	movq	ae_kill(%rip), %rsi
	jmp	.LBB0_105
.LBB0_91:                               #   in Loop: Header=BB0_31 Depth=1
	movq	184(%rsp), %rsi         # 8-byte Reload
.LBB0_105:                              # %compute_ae_kill.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	ae_out(%rip), %rdx
	movq	ae_in(%rip), %rcx
	movq	%r15, %rdi
	callq	compute_available
	movslq	n_basic_blocks(%rip), %rax
	xorq	%r13, %r13
	cmpq	$2, %rax
	movq	$1, %r15
	jl	.LBB0_166
	.align	16, 0x90
.LBB0_106:                              # %for.body.i74.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_107 Depth 3
                                        #         Child Loop BB0_116 Depth 4
                                        #         Child Loop BB0_122 Depth 4
	movq	reg_set_bitmap(%rip), %rdi
	callq	bitmap_clear
	callq	clear_modify_mem_tables
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB0_165
	jmp	.LBB0_107
	.align	16, 0x90
.LBB0_164:                              # %for.inc.land.rhs_crit_edge.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
.LBB0_107:                              # %land.rhs.i.preheader.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_106 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_116 Depth 4
                                        #         Child Loop BB0_122 Depth 4
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB0_165
# BB#108:                               # %for.body10.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB0_161
# BB#109:                               # %land.lhs.true.i81.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_161
# BB#110:                               # %land.lhs.true18.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB0_161
# BB#111:                               # %land.lhs.true28.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movl	8(%rcx), %ecx
	cmpq	$53, %rcx
	jb	.LBB0_161
# BB#112:                               # %if.then.i85.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	16(%rax), %r14
	movq	%r14, %rdi
	callq	want_to_gcse_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_161
# BB#113:                               # %land.lhs.true44.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movl	expr_hash_table_size(%rip), %r12d
	movzbl	2(%r14), %esi
	xorq	%rax, %rax
	movl	%eax, 196(%rsp)
	movq	%r14, %rdi
	leaq	196(%rsp), %rdx
	callq	hash_expr_1
	movl	196(%rsp), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_161
# BB#114:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %r12
	xorq	%rdx, %rdx
	divq	%r12
	movq	expr_hash_table(%rip), %rax
	movq	(%rax,%rdx,8), %r12
	jmp	.LBB0_116
.LBB0_115:                              # %while.cond.i.i.i
                                        #   in Loop: Header=BB0_116 Depth=4
	movq	16(%r12), %r12
.LBB0_116:                              # %while.cond.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_106 Depth=2
                                        #       Parent Loop BB0_107 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%r12, %r12
	je	.LBB0_161
# BB#117:                               # %land.rhs.i.i.i
                                        #   in Loop: Header=BB0_116 Depth=4
	movq	(%r12), %rdi
	movq	%r14, %rsi
	callq	expr_equiv_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_115
# BB#118:                               # %land.lhs.true47.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	ae_in(%rip), %rax
	movl	8(%r12), %ecx
	movq	(%rax,%r15,8), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB0_161
# BB#119:                               # %land.lhs.true54.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	oprs_not_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_161
# BB#120:                               # %if.then57.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movslq	8(%rbx), %rcx
	movq	basic_block_for_insn(%rip), %rax
	movq	32(%r12), %r13
	movq	32(%rax,%rcx,8), %rsi
	movq	%rsi, 184(%rsp)         # 8-byte Spill
	cmpq	$0, (%r13)
	jne	.LBB0_121
# BB#125:                               # %if.then.i.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	8(%r13), %rdx
	movslq	8(%rdx), %rcx
	movq	32(%rax,%rcx,8), %rax
	xorq	%rcx, %rcx
	cmpq	%rsi, %rax
	je	.LBB0_160
# BB#126:                               # %if.then.i.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	testq	%rdx, %rdx
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	jne	.LBB0_127
	jmp	.LBB0_160
.LBB0_121:                              # %for.cond.preheader.i.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	$0, 168(%rsp)           # 8-byte Folded Spill
	testq	%r13, %r13
	movabsq	$0, %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	jne	.LBB0_122
	jmp	.LBB0_141
.LBB0_139:                              # %for.inc.for.body_crit_edge.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movq	basic_block_for_insn(%rip), %rax
.LBB0_122:                              # %for.body.i.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_106 Depth=2
                                        #       Parent Loop BB0_107 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%r13), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%rax,%rcx,8), %rax
	cmpq	%rsi, %rax
	movq	%rsi, %r14
	jne	.LBB0_134
# BB#123:                               # %if.then27.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movslq	8(%rbx), %rax
	movq	uid_cuid(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	movslq	(%rdx,%rcx,4), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_124
# BB#132:                               # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movslq	n_basic_blocks(%rip), %rdi
	movq	$1, %rsi
	callq	xcalloc
	movq	%rax, %r14
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	184(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %r8
	callq	expr_reaches_here_p_work
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	callq	free
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	176(%rsp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB0_136
# BB#133:                               #   in Loop: Header=BB0_122 Depth=4
	movq	184(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB0_138
.LBB0_134:                              # %if.else46.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movslq	n_basic_blocks(%rip), %rdi
	movq	$1, %rsi
	callq	xcalloc
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %r8
	movq	%rax, %r14
	callq	expr_reaches_here_p_work
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	callq	free
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	176(%rsp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB0_136
# BB#135:                               #   in Loop: Header=BB0_122 Depth=4
	movq	184(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB0_138
.LBB0_136:                              # %if.then49.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movq	$32, %rcx
	movq	168(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	184(%rsp), %rsi         # 8-byte Reload
	jg	.LBB0_160
# BB#137:                               # %if.end53.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movq	8(%r13), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	incq	168(%rsp)               # 8-byte Folded Spill
	jmp	.LBB0_138
.LBB0_124:                              #   in Loop: Header=BB0_122 Depth=4
	movq	184(%rsp), %rsi         # 8-byte Reload
.LBB0_138:                              # %for.inc.i.i.i.i
                                        #   in Loop: Header=BB0_122 Depth=4
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB0_139
# BB#140:                               # %for.end.i.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	cmpq	$0, 160(%rsp)           # 8-byte Folded Reload
	je	.LBB0_141
.LBB0_127:                              # %if.end.i98.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	160(%rsp), %rdi         # 8-byte Reload
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_131
# BB#128:                               # %cond.true.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	32(%rdi), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB0_130
# BB#129:                               # %cond.false.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	%r14, %rsi
	callq	single_set_2
	movq	%rax, %r14
.LBB0_130:                              # %cond.end17.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	testq	%r14, %r14
	je	.LBB0_131
# BB#143:                               # %if.end20.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_147
# BB#144:                               # %if.then28.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movl	8(%rax), %eax
	movl	max_gcse_regno(%rip), %ecx
	leaq	16(%r14), %r12
	cmpq	%rcx, %rax
	movabsq	$.L.str43, %rcx
	jae	.LBB0_151
# BB#145:                               # %lor.lhs.false.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	reg_set_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movq	%rax, 200(%rsp)
	cmpq	$0, (%rax)
	movabsq	$.L.str43, %rax
	movq	%rax, %rcx
	je	.LBB0_151
# BB#146:                               # %lor.lhs.false40.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	leaq	200(%rsp), %rdi
	movq	%rbx, %rsi
	callq	can_disregard_other_sets
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$.L.str43, %rax
	movq	%rax, %rcx
	jne	.LBB0_151
.LBB0_147:                              # %if.then47.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	addq	$8, %r14
	movq	(%r14), %rax
	movl	8(%rax), %eax
	movl	max_gcse_regno(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB0_148
# BB#149:                               # %if.end58.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	reg_set_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movq	%rax, 200(%rsp)
	cmpq	$0, (%rax)
	movabsq	$.L.str44, %rax
	movq	%rax, %rcx
	movq	%r14, %r12
	je	.LBB0_151
# BB#150:                               # %lor.lhs.false64.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	leaq	200(%rsp), %rdi
	movq	%rbx, %rsi
	callq	can_disregard_other_sets
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$.L.str44, %rax
	movq	%rax, %rcx
	movq	%r14, %r12
	jne	.LBB0_151
# BB#154:                               # %if.else107.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	(%r14), %rax
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	(%r14), %rcx
	movq	%rax, %r14
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	160(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	callq	emit_insn_after
	movl	8(%r14), %edi
	movq	%rax, %rsi
	callq	record_one_set
	movl	gcse_create_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, gcse_create_count(%rip)
	testq	%rdi, %rdi
	je	.LBB0_156
# BB#155:                               # %if.then125.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	24(%r12), %rax
	movq	32(%rax), %rcx
	movq	16(%rcx), %rcx
	movl	8(%rax), %edx
	movl	8(%rcx), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str45, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	8(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str46, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	8(%r14), %edx
	xorq	%rax, %rax
	movabsq	$.L.str47, %rsi
	callq	fprintf
.LBB0_156:                              # %if.end153.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	24(%r12), %rax
	movq	%r12, %r13
	movq	32(%rax), %rax
	movq	32(%rbx), %rsi
	movq	8(%rax), %rdx
	addq	$16, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	validate_change
	xorq	%rcx, %rcx
	movq	%rax, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB0_160
# BB#157:                               # %if.then171.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movl	gcse_subst_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, gcse_subst_count(%rip)
	testq	%rdi, %rdi
	je	.LBB0_159
# BB#158:                               # %if.then175.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	24(%r13), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	movl	8(%rbx), %edx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str48, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	8(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str49, %rsi
	callq	fprintf
	jmp	.LBB0_159
.LBB0_151:                              # %if.end83.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	%rcx, %r14
	movq	32(%rbx), %rsi
	movq	(%r12), %r13
	addq	$16, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rdx
	callq	validate_change
	xorq	%rcx, %rcx
	movq	%rax, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB0_160
# BB#152:                               # %if.then89.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movl	gcse_subst_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, gcse_subst_count(%rip)
	testq	%rdi, %rdi
	je	.LBB0_159
# BB#153:                               # %if.then92.i.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str41, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movq	160(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %r8d
	movl	8(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str42, %rsi
	movq	%r14, %rcx
	callq	fprintf
.LBB0_159:                              # %handle_avail_expr.exit.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	%r12, %rcx
.LBB0_160:                              # %handle_avail_expr.exit.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	96(%rsp), %r13          # 8-byte Reload
	orq	%rcx, %r13
	.align	16, 0x90
.LBB0_161:                              # %if.end59.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_163
# BB#162:                               # %if.then66.i.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	%rbx, %rdi
	callq	mark_oprs_set
.LBB0_163:                              # %for.inc.i96.i
                                        #   in Loop: Header=BB0_107 Depth=3
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_164
	.align	16, 0x90
.LBB0_165:                              # %for.inc71.i.i
                                        #   in Loop: Header=BB0_106 Depth=2
	incq	%r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_106
.LBB0_166:                              # %classic_gcse.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	ae_kill(%rip), %rdi
	callq	free
	movq	ae_gen(%rip), %rdi
	callq	free
	movq	ae_in(%rip), %rdi
	callq	free
	movq	ae_out(%rip), %rdi
	callq	free
	movq	48(%rsp), %r15          # 8-byte Reload
.LBB0_167:                              # %if.end2.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	rd_kill(%rip), %rdi
	callq	free
	movq	rd_gen(%rip), %rdi
	callq	free
	movq	reaching_defs(%rip), %rdi
	callq	free
	movq	rd_out(%rip), %rdi
	callq	free
	movq	expr_hash_table(%rip), %rdi
	callq	free
	movq	gcse_file(%rip), %rsi
	testq	%rsi, %rsi
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB0_169
# BB#168:                               # %if.then4.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$10, %rdi
	callq	fputc
	movq	cfun(%rip), %rax
	movq	gcse_file(%rip), %rdi
	movq	40(%rax), %rdx
	movl	gcse_subst_count(%rip), %r9d
	movl	bytes_used(%rip), %r8d
	xorq	%rax, %rax
	movabsq	$.L.str39, %rsi
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	gcse_create_count(%rip), %edx
	xorq	%rax, %rax
	movabsq	$.L.str40, %rsi
	callq	fprintf
.LBB0_169:                              # %one_classic_gcse_pass.exit
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r13
	jmp	.LBB0_354
.LBB0_223:                              #   in Loop: Header=BB0_31 Depth=1
	movq	48(%rsp), %r15          # 8-byte Reload
	jmp	.LBB0_235
.LBB0_225:                              #   in Loop: Header=BB0_31 Depth=1
	movq	48(%rsp), %r15          # 8-byte Reload
.LBB0_235:                              # %trim_ld_motion_mems.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	testq	%r14, %r14
	je	.LBB0_237
# BB#236:                               # %if.then2.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %rdx
	movl	n_exprs(%rip), %r8d
	movl	expr_hash_table_size(%rip), %ecx
	movq	%r14, %rdi
	movabsq	$.L.str15, %rsi
	callq	dump_hash_table
.LBB0_237:                              # %if.end3.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movslq	n_exprs(%rip), %rbx
	xorq	%r13, %r13
	testq	%rbx, %rbx
	jle	.LBB0_344
# BB#238:                               # %if.then4.i238
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movl	n_basic_blocks(%rip), %r14d
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, transp(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, comp(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, antloc(%rip)
	movq	$0, pre_insert_map(%rip)
	movq	$0, pre_delete_map(%rip)
	movq	$0, ae_in(%rip)
	movq	$0, ae_out(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	transp(%rip), %rdi
	movq	comp(%rip), %rsi
	movq	antloc(%rip), %rdx
	movq	%rax, ae_kill(%rip)
	xorq	%rcx, %rcx
	xorq	%r15, %r15
	callq	compute_local_properties
	movq	ae_kill(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	callq	sbitmap_vector_zero
	movl	n_exprs(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	sbitmap_zero
	movl	expr_hash_table_size(%rip), %eax
	testq	%rax, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB0_239
	.align	16, 0x90
.LBB0_241:                              # %for.body.i32.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_242 Depth 3
	movq	expr_hash_table(%rip), %rcx
	movq	%r15, %rdx
	andq	%r12, %rdx
	movq	(%rcx,%rdx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB0_246
	.align	16, 0x90
.LBB0_242:                              # %for.body3.i.i239
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_241 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rbx), %rdi
	callq	may_trap_p
	testq	%r12, %rax
	je	.LBB0_244
# BB#243:                               # %if.then.i37.i
                                        #   in Loop: Header=BB0_242 Depth=3
	movl	8(%rbx), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	shrq	$6, %rcx
	orq	%rax, 16(%r14,%rcx,8)
.LBB0_244:                              # %for.inc.i39.i
                                        #   in Loop: Header=BB0_242 Depth=3
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_242
# BB#245:                               # %for.cond1.for.inc8_crit_edge.i.i
                                        #   in Loop: Header=BB0_241 Depth=2
	movl	expr_hash_table_size(%rip), %eax
.LBB0_246:                              # %for.inc8.i.i
                                        #   in Loop: Header=BB0_241 Depth=2
	incq	%r15
	movq	%r15, %rcx
	andq	%r12, %rcx
	movq	%rax, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB0_241
.LBB0_239:                              # %for.cond10.preheader.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r12, %r15
	movslq	n_basic_blocks(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB0_252
	.align	16, 0x90
.LBB0_240:                              # %for.body12.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_248 Depth 3
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	32(%rax), %rax
	jmp	.LBB0_248
	.align	16, 0x90
.LBB0_247:                              # %for.cond16.i.i
                                        #   in Loop: Header=BB0_248 Depth=3
	movq	(%rax), %rax
.LBB0_248:                              # %for.cond16.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_240 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	je	.LBB0_251
# BB#249:                               # %for.body18.i.i
                                        #   in Loop: Header=BB0_248 Depth=3
	movzbl	48(%rax), %ecx
	testq	$2, %rcx
	je	.LBB0_247
# BB#250:                               # %if.then20.i.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	antloc(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	%rdi, %rsi
	movq	%r14, %rdx
	callq	sbitmap_difference
	movq	transp(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	%rdi, %rsi
	movq	%r14, %rdx
	callq	sbitmap_difference
.LBB0_251:                              # %for.end31.i.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	ae_kill(%rip), %rax
	movq	transp(%rip), %rcx
	movq	comp(%rip), %rdx
	movq	(%rax,%rbx,8), %rdi
	movq	(%rcx,%rbx,8), %rsi
	movq	(%rdx,%rbx,8), %rdx
	callq	sbitmap_a_or_b
	movq	ae_kill(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	%rdi, %rsi
	callq	sbitmap_not
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_240
.LBB0_252:                              # %compute_pre_data.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	gcse_file(%rip), %rdi
	movq	transp(%rip), %rdx
	movq	comp(%rip), %rcx
	movq	antloc(%rip), %r8
	movq	ae_kill(%rip), %r9
	movl	n_exprs(%rip), %esi
	movq	$pre_delete_map, 8(%rsp)
	movq	$pre_insert_map, (%rsp)
	callq	pre_edge_lcm
	movq	antloc(%rip), %rdi
	movq	%rax, edge_list(%rip)
	callq	free
	movq	ae_kill(%rip), %rdi
	movq	$0, antloc(%rip)
	callq	free
	movq	$0, ae_kill(%rip)
	movq	%r14, %rdi
	callq	free
	movslq	n_exprs(%rip), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movl	expr_hash_table_size(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB0_258
# BB#253:                               # %for.body.lr.ph.i51.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %rbx
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_254:                              # %for.body.i54.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_256 Depth 3
	movq	(%rbx,%rdx,8), %rsi
	jmp	.LBB0_256
	.align	16, 0x90
.LBB0_255:                              # %for.body5.i59.i
                                        #   in Loop: Header=BB0_256 Depth=3
	movslq	8(%rsi), %rdi
	movq	%rsi, (%rax,%rdi,8)
	movq	16(%rsi), %rsi
.LBB0_256:                              # %for.body5.i59.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_254 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rsi, %rsi
	jne	.LBB0_255
# BB#257:                               # %for.inc8.i62.i
                                        #   in Loop: Header=BB0_254 Depth=2
	incq	%rdx
	movq	%rdx, %rsi
	andq	%r15, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB0_254
.LBB0_258:                              # %for.end9.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movl	max_cuid(%rip), %edi
	callq	sbitmap_alloc
	movq	%rax, pre_redundant_insns(%rip)
	movq	%rax, %rdi
	callq	sbitmap_zero
	movl	expr_hash_table_size(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rsi
	movabsq	$0, %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	%r15, %rbx
	je	.LBB0_280
	.align	16, 0x90
.LBB0_259:                              # %for.body.i.i64.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_260 Depth 3
                                        #         Child Loop BB0_263 Depth 4
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	movq	expr_hash_table(%rip), %rcx
	movq	%rsi, %rdx
	andq	%rbx, %rdx
	movq	(%rcx,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB0_279
	.align	16, 0x90
.LBB0_260:                              # %for.body3.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_259 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_263 Depth 4
	movq	24(%rcx), %r15
	testq	%r15, %r15
	jne	.LBB0_262
# BB#261:                               #   in Loop: Header=BB0_260 Depth=3
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	jmp	.LBB0_277
	.align	16, 0x90
.LBB0_262:                              # %for.body6.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_260 Depth=3
	movl	8(%rcx), %edx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movq	%rdx, %r13
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %r13
	movq	$6, %rcx
	shrq	%cl, %r13
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	movq	%rsi, 176(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB0_263:                              # %for.body6.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_259 Depth=2
                                        #       Parent Loop BB0_260 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%r15), %r12
	movslq	8(%r12), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %r14
	movslq	88(%r14), %rax
	movq	pre_delete_map(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movq	16(%rax,%r13,8), %rax
	testq	%rsi, %rax
	je	.LBB0_276
# BB#264:                               # %if.then.i.i68.i
                                        #   in Loop: Header=BB0_263 Depth=4
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_268
# BB#265:                               # %cond.true.i.i.i257
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	32(%r12), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB0_267
# BB#266:                               # %cond.false.i.i.i258
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB0_267:                              # %cond.end34.i.i.i
                                        #   in Loop: Header=BB0_263 Depth=4
	testq	%rbx, %rbx
	je	.LBB0_268
# BB#269:                               # %if.end.i.i71.i
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB0_271
# BB#270:                               # %if.then40.i.i.i
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	8(%rbx), %rax
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	%rax, %rdx
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	%rdx, 40(%rax)
.LBB0_271:                              # %if.end48.i.i.i
                                        #   in Loop: Header=BB0_263 Depth=4
	addq	$16, %rbx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_273
# BB#272:                               # %if.then55.i.i.i
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	$1, %rax
	movb	%al, 16(%r15)
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	movslq	8(%r12), %rax
	movq	uid_cuid(%rip), %rcx
	movl	(%rcx,%rax,4), %ecx
	movq	pre_redundant_insns(%rip), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rax,%rcx,8)
	movl	gcse_subst_count(%rip), %eax
	incq	%rax
	movl	%eax, gcse_subst_count(%rip)
.LBB0_273:                              # %if.end72.i.i.i
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_275
# BB#274:                               # %if.then74.i.i.i
                                        #   in Loop: Header=BB0_263 Depth=4
	movl	8(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str23, %rsi
	movq	184(%rsp), %rbx         # 8-byte Reload
	movl	8(%rbx), %ecx
	callq	fprintf
	movq	40(%rbx), %rax
	movq	gcse_file(%rip), %rdi
	movl	88(%r14), %edx
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str24, %rsi
	callq	fprintf
.LBB0_275:                              # %for.inc.i.i.i268
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	176(%rsp), %rsi         # 8-byte Reload
.LBB0_276:                              # %for.inc.i.i.i268
                                        #   in Loop: Header=BB0_263 Depth=4
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB0_263
.LBB0_277:                              # %for.inc86.i.i.i
                                        #   in Loop: Header=BB0_260 Depth=3
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_260
# BB#278:                               # %for.cond1.for.inc88_crit_edge.i.i.i
                                        #   in Loop: Header=BB0_259 Depth=2
	movl	expr_hash_table_size(%rip), %eax
.LBB0_279:                              # %for.inc88.i.i.i
                                        #   in Loop: Header=BB0_259 Depth=2
	movq	168(%rsp), %rsi         # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB0_259
.LBB0_280:                              # %pre_delete.exit.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	pre_insert_map(%rip), %rax
	movq	edge_list(%rip), %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	movl	4(%rax), %eax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movslq	4(%rcx), %r14
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movl	n_exprs(%rip), %esi
	movq	%r14, %rdi
	callq	sbitmap_vector_alloc
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_zero
	testq	%r14, %r14
	movq	%rbx, %rdx
	movabsq	$0, %r15
	jle	.LBB0_308
# BB#281:                               # %for.body.lr.ph.i.i.i272
                                        #   in Loop: Header=BB0_31 Depth=1
	xorq	%r8, %r8
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_282:                              # %for.body.i21.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_284 Depth 3
                                        #         Child Loop BB0_289 Depth 4
                                        #           Child Loop BB0_294 Depth 5
	movq	%r8, 176(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	128(%rsp), %r9          # 8-byte Reload
	jle	.LBB0_307
# BB#283:                               # %for.body5.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_282 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	(%rax,%r8,8), %rax
	movq	16(%rax), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movl	n_exprs(%rip), %eax
	xorq	%rbx, %rbx
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_284:                              # %for.body5.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_282 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_289 Depth 4
                                        #           Child Loop BB0_294 Depth 5
	movq	pre_insert_map(%rip), %rcx
	movq	(%rcx,%r8,8), %rcx
	movq	16(%rcx,%rbx,8), %r12
	testq	%r12, %r12
	je	.LBB0_285
# BB#286:                               # %for.body5.i.i.i
                                        #   in Loop: Header=BB0_284 Depth=3
	movq	%rbx, 112(%rsp)         # 8-byte Spill
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
	cmpq	%rdx, %rsi
	jge	.LBB0_287
# BB#288:                               # %for.body12.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_284 Depth=3
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	movq	%rdi, %r14
	.align	16, 0x90
.LBB0_289:                              # %for.body12.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_282 Depth=2
                                        #       Parent Loop BB0_284 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_294 Depth 5
	testq	$1, %r12
	je	.LBB0_304
# BB#290:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB0_289 Depth=4
	movq	(%r9,%rbx,8), %rcx
	cmpq	$0, 40(%rcx)
	je	.LBB0_304
# BB#291:                               # %if.then.i27.i.i
                                        #   in Loop: Header=BB0_289 Depth=4
	movq	%rbx, %rdx
	movq	24(%rcx), %rbx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	testq	%rbx, %rbx
	jne	.LBB0_293
# BB#292:                               #   in Loop: Header=BB0_289 Depth=4
	movq	128(%rsp), %r9          # 8-byte Reload
	movq	%rdx, %rbx
	jmp	.LBB0_304
.LBB0_293:                              # %for.body21.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_289 Depth=4
	movq	$6, %rcx
	movq	%rdx, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, %r13
	shrq	%cl, %r13
	andq	$67108863, %r13         # imm = 0x3FFFFFF
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_294:                              # %for.body21.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_282 Depth=2
                                        #       Parent Loop BB0_284 Depth=3
                                        #         Parent Loop BB0_289 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_302
# BB#295:                               # %if.end.i32.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	(%rcx,%rdx,8), %rax
	movq	16(%rax,%r13,8), %rax
	testq	%rsi, %rax
	jne	.LBB0_302
# BB#296:                               # %if.then31.i.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	(%rax,%rdx,8), %r15
	movzbl	48(%r15), %eax
	testq	$2, %rax
	jne	.LBB0_297
# BB#298:                               # %if.else.i.i74.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	184(%rsp), %r13         # 8-byte Reload
	movq	(%r13), %rdi
	movq	40(%r13), %rsi
	callq	process_insert_insn
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%r13, %r15
	movq	136(%rsp), %r13         # 8-byte Reload
	callq	insert_insn_on_edge
	jmp	.LBB0_299
.LBB0_297:                              # %if.then37.i33.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	184(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	insert_insn_end_bb
.LBB0_299:                              # %if.end43.i.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_301
# BB#300:                               # %if.then45.i.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rax
	movq	24(%rax), %rax
	movl	88(%rax), %ecx
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	88(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str21, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	8(%r15), %edx
	xorq	%rax, %rax
	movabsq	$.L.str22, %rsi
	callq	fprintf
.LBB0_301:                              # %if.end52.i.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	%r15, %rdi
	movq	%r15, 184(%rsp)         # 8-byte Spill
	callq	update_ld_motion_stores
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	(%rcx,%rdx,8), %rax
	movq	152(%rsp), %rsi         # 8-byte Reload
	orq	%rsi, 16(%rax,%r13,8)
	movl	gcse_create_count(%rip), %eax
	incq	%rax
	movl	%eax, gcse_create_count(%rip)
	movq	$1, %r15
.LBB0_302:                              # %for.inc.i39.i.i
                                        #   in Loop: Header=BB0_294 Depth=5
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_294
# BB#303:                               # %for.inc63.i.loopexit.i.i
                                        #   in Loop: Header=BB0_289 Depth=4
	movq	%rdx, %r8
	movl	n_exprs(%rip), %eax
	movq	128(%rsp), %r9          # 8-byte Reload
	movq	120(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB0_304:                              # %for.inc63.i.i.i
                                        #   in Loop: Header=BB0_289 Depth=4
	movq	$1, %rcx
	shrq	%cl, %r12
	testq	%r12, %r12
	je	.LBB0_306
# BB#305:                               # %for.inc63.i.i.i
                                        #   in Loop: Header=BB0_289 Depth=4
	incq	%r14
	incq	%rbx
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB0_289
	jmp	.LBB0_306
	.align	16, 0x90
.LBB0_285:                              #   in Loop: Header=BB0_284 Depth=3
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	jmp	.LBB0_306
	.align	16, 0x90
.LBB0_287:                              #   in Loop: Header=BB0_284 Depth=3
	movq	%rdi, 104(%rsp)         # 8-byte Spill
.LBB0_306:                              # %for.inc67.i.i.i
                                        #   in Loop: Header=BB0_284 Depth=3
	movq	112(%rsp), %rbx         # 8-byte Reload
	incq	%rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	addq	$64, %rdi
	movq	%rbx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB0_284
.LBB0_307:                              # %for.inc70.i.i.i
                                        #   in Loop: Header=BB0_282 Depth=2
	movq	%r9, 128(%rsp)          # 8-byte Spill
	incq	%r8
	movq	%r8, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_282
.LBB0_308:                              # %pre_edge_insert.exit.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r15, 168(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rdx, %rbx
	callq	free
	movl	expr_hash_table_size(%rip), %eax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB0_332
	.align	16, 0x90
.LBB0_309:                              # %for.body.i43.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_310 Depth 3
                                        #         Child Loop BB0_312 Depth 4
                                        #           Child Loop BB0_315 Depth 5
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	movq	expr_hash_table(%rip), %rcx
	andq	%rbx, %rdx
	movq	(%rcx,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB0_331
	.align	16, 0x90
.LBB0_310:                              # %for.body3.i45.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_309 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_312 Depth 4
                                        #           Child Loop BB0_315 Depth 5
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	cmpq	$0, 40(%rcx)
	je	.LBB0_329
# BB#311:                               # %if.end.i47.i.i
                                        #   in Loop: Header=BB0_310 Depth=3
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rax
	jmp	.LBB0_312
	.align	16, 0x90
.LBB0_328:                              # %for.inc48.i.i.i
                                        #   in Loop: Header=BB0_312 Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
.LBB0_312:                              # %if.end.i47.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_309 Depth=2
                                        #       Parent Loop BB0_310 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_315 Depth 5
	testq	%rax, %rax
	je	.LBB0_329
# BB#313:                               # %for.body7.i.i.i
                                        #   in Loop: Header=BB0_312 Depth=4
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movzbl	16(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_328
# BB#314:                               # %if.end9.i.i.i
                                        #   in Loop: Header=BB0_312 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r14
	jmp	.LBB0_315
	.align	16, 0x90
.LBB0_327:                              # %for.inc.i62.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	(%r14), %r14
.LBB0_315:                              # %if.end9.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_309 Depth=2
                                        #       Parent Loop BB0_310 Depth=3
                                        #         Parent Loop BB0_312 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	testq	%r14, %r14
	je	.LBB0_328
# BB#316:                               # %for.body12.i51.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movzbl	17(%r14), %eax
	testq	%rax, %rax
	jne	.LBB0_327
# BB#317:                               # %if.end16.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	8(%r14), %r15
	movslq	8(%r15), %rax
	movq	uid_cuid(%rip), %rcx
	movl	(%rcx,%rax,4), %ecx
	movq	pre_redundant_insns(%rip), %rdx
	movq	%rcx, %rsi
	shrq	$6, %rsi
	movq	16(%rdx,%rsi,8), %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	jne	.LBB0_327
# BB#318:                               # %if.end29.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movslq	8(%rcx), %rcx
	movq	32(%rdx,%rax,8), %r13
	movq	32(%rdx,%rcx,8), %r12
	movslq	n_basic_blocks(%rip), %rdi
	movq	$1, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	pre_expr_reaches_here_p_work
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	free
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB0_327
# BB#319:                               # %if.end46.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	176(%rsp), %rbx         # 8-byte Reload
	jne	.LBB0_323
# BB#320:                               # %cond.true.i.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	32(%r15), %rax
	movq	40(%rbx), %r12
	movl	8(%r12), %r13d
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_322
# BB#321:                               # %cond.false.i.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_322:                              # %cond.end17.i.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	testq	%rax, %rax
	je	.LBB0_323
# BB#324:                               # %if.end.i.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	8(%rax), %rsi
	movq	%r12, %rdi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_insn_after
	movq	%rax, %r12
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	record_one_set
	movl	gcse_create_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, gcse_create_count(%rip)
	testq	%rdi, %rdi
	je	.LBB0_326
# BB#325:                               # %if.then25.i.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movslq	8(%r15), %r9
	movq	basic_block_for_insn(%rip), %rax
	movq	32(%rax,%r9,8), %rax
	movl	88(%rax), %edx
	movl	8(%r12), %ecx
	xorq	%rax, %rax
	movq	%r13, (%rsp)
	movabsq	$.L.str18, %rsi
	movl	8(%rbx), %r8d
	callq	fprintf
.LBB0_326:                              # %pre_insert_copy_insn.exit.i.i.i
                                        #   in Loop: Header=BB0_315 Depth=5
	movq	%rbx, %rdi
	callq	update_ld_motion_stores
	movq	$1, %rax
	movb	%al, 17(%r14)
	jmp	.LBB0_327
	.align	16, 0x90
.LBB0_329:                              # %for.inc51.i.i.i
                                        #   in Loop: Header=BB0_310 Depth=3
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	movq	168(%rsp), %r15         # 8-byte Reload
	jne	.LBB0_310
# BB#330:                               # %for.cond1.for.inc53_crit_edge.i.i.i
                                        #   in Loop: Header=BB0_309 Depth=2
	movl	expr_hash_table_size(%rip), %eax
.LBB0_331:                              # %for.inc53.i.i.i
                                        #   in Loop: Header=BB0_309 Depth=2
	movq	160(%rsp), %rsi         # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	%rdx, %rcx
	movq	%rsi, %rdx
	jb	.LBB0_309
.LBB0_332:                              # %pre_insert_copies.exit.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	testq	%rbx, %r15
	movq	96(%rsp), %r13          # 8-byte Reload
	je	.LBB0_334
# BB#333:                               # %if.then.i77.i
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	commit_edge_insertions
	movq	$1, %r13
.LBB0_334:                              # %pre_gcse.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	pre_redundant_insns(%rip), %rdi
	callq	free
	movq	edge_list(%rip), %rdi
	callq	free_edge_list
	movq	transp(%rip), %rdi
	callq	free
	movq	comp(%rip), %rdi
	callq	free
	movq	pre_insert_map(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_336
# BB#335:                               # %if.then5.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	free
.LBB0_336:                              # %if.end6.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	pre_delete_map(%rip), %rdi
	testq	%rdi, %rdi
	movq	48(%rsp), %r15          # 8-byte Reload
	je	.LBB0_338
# BB#337:                               # %if.then8.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	free
.LBB0_338:                              # %if.end9.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	ae_in(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_340
# BB#339:                               # %if.then11.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	free
.LBB0_340:                              # %if.end12.i78.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	ae_out(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_342
# BB#341:                               # %if.then14.i79.i
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	free
.LBB0_342:                              # %free_pre_mem.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$0, comp(%rip)
	movq	$0, transp(%rip)
	movq	$0, pre_delete_map(%rip)
	movq	$0, pre_insert_map(%rip)
	movq	$0, ae_out(%rip)
	movq	$0, ae_in(%rip)
	jmp	.LBB0_344
	.align	16, 0x90
.LBB0_343:                              # %while.body.i84.i
                                        #   in Loop: Header=BB0_344 Depth=2
	movq	32(%rbx), %rax
	movq	%rax, pre_ldst_mems(%rip)
	leaq	16(%rbx), %rdi
	callq	free_INSN_LIST_list
	leaq	24(%rbx), %rdi
	callq	free_INSN_LIST_list
	movq	%rbx, %rdi
	callq	free
.LBB0_344:                              # %while.body.i84.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	pre_ldst_mems(%rip), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_343
# BB#345:                               # %free_ldst_mems.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$0, pre_ldst_mems(%rip)
	callq	remove_fake_edges
	movq	expr_hash_table(%rip), %rdi
	callq	free
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_347
# BB#346:                               # %if.then7.i288
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movl	bytes_used(%rip), %r8d
	xorq	%rax, %rax
	movabsq	$.L.str16, %rsi
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	gcse_create_count(%rip), %ecx
	movl	gcse_subst_count(%rip), %edx
	xorq	%rax, %rax
	movabsq	$.L.str17, %rsi
	callq	fprintf
.LBB0_347:                              # %one_pre_gcse_pass.exit
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB0_349
# BB#348:                               # %if.then49
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	clear_modify_mem_tables
	movq	modify_mem_list(%rip), %rdi
	callq	free
	movq	canon_modify_mem_list(%rip), %rdi
	callq	free
	movq	$0, modify_mem_list(%rip)
	movl	n_basic_blocks(%rip), %edi
	movl	bytes_used(%rip), %eax
	movq	$0, canon_modify_mem_list(%rip)
	leaq	(%rax,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %edi
	movl	bytes_used(%rip), %ecx
	movq	%rax, modify_mem_list(%rip)
	leaq	(%rcx,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	modify_mem_list(%rip), %rdi
	movq	%rax, canon_modify_mem_list(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movslq	n_basic_blocks(%rip), %rdx
	movq	canon_modify_mem_list(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
.LBB0_349:                              # %if.end62
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	reg_set_table(%rip), %rdi
	callq	free
	xorq	%rsi, %rsi
	movabsq	$reg_set_obstack, %r14
	movq	%r14, %rdi
	callq	obstack_free
	callq	max_reg_num
	leaq	100(%rax), %rbx
	movl	bytes_used(%rip), %ecx
	movl	%ebx, reg_set_table_size(%rip)
	leaq	800(%rcx,%rax,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rbx        # imm = 0x1FFFFFFF
	shlq	$3, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, reg_set_table(%rip)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rbx, %rdx
	callq	memset
	movq	%r14, %rdi
	callq	gcc_obstack_init
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %rbx
	movq	%rax, %r14
	je	.LBB0_353
	.align	16, 0x90
.LBB0_350:                              # %for.body.i307
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_352
# BB#351:                               # %if.then.i310
                                        #   in Loop: Header=BB0_350 Depth=2
	movq	32(%rbx), %rdi
	movabsq	$record_set_info, %rsi
	movq	%rbx, %rdx
	callq	note_stores
.LBB0_352:                              # %for.inc.i314
                                        #   in Loop: Header=BB0_350 Depth=2
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_350
.LBB0_353:                              # %compute_sets.exit315
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$1, %rax
	movb	%al, run_jump_opt_after_gcse(%rip)
.LBB0_354:                              # %if.end64
                                        #   in Loop: Header=BB0_31 Depth=1
	movslq	bytes_used(%rip), %rbx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jl	.LBB0_356
# BB#355:                               # %if.end64
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r15, %rbx
.LBB0_356:                              # %if.end64
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	free_gcse_mem
	movl	optimize_size(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_358
# BB#357:                               #   in Loop: Header=BB0_31 Depth=1
	movq	%rbx, %r15
	jmp	.LBB0_415
.LBB0_358:                              # %if.then70
                                        #   in Loop: Header=BB0_31 Depth=1
	callq	max_reg_num
	movl	%eax, max_gcse_regno(%rip)
	movq	%r14, %rdi
	callq	alloc_gcse_mem
	movl	max_cuid(%rip), %edx
	movq	$1, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	cmpq	$22, %rdx
	movq	$11, %rdi
	jb	.LBB0_360
# BB#359:                               # %if.then70
                                        #   in Loop: Header=BB0_31 Depth=1
	orq	$1, %rax
	movq	%rax, %rdi
.LBB0_360:                              # %if.then70
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movl	bytes_used(%rip), %eax
	movl	%edi, expr_hash_table_size(%rip)
	leaq	(%rax,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	movl	expr_hash_table_size(%rip), %edx
	movq	%rax, expr_hash_table(%rip)
	xorq	%rcx, %rcx
	movl	%ecx, n_exprs(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	memset
	xorq	%rdi, %rdi
	callq	compute_hash_table
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_362
# BB#361:                               # %if.then.i326
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %rdx
	movl	n_exprs(%rip), %r8d
	movl	expr_hash_table_size(%rip), %ecx
	movabsq	$.L.str8, %rsi
	callq	dump_hash_table
.LBB0_362:                              # %if.end.i328
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movslq	n_exprs(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB0_413
# BB#363:                               # %if.then1.i335
                                        #   in Loop: Header=BB0_31 Depth=1
	movl	n_basic_blocks(%rip), %r14d
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, antloc(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, transp(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, comp(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, hoist_vbein(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, hoist_vbeout(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, hoist_exprs(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, transpout(%rip)
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movq	transp(%rip), %rdi
	movq	comp(%rip), %rsi
	movq	antloc(%rip), %rdx
	movq	%rax, dominators(%rip)
	xorq	%rcx, %rcx
	callq	compute_local_properties
	movq	transpout(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	callq	sbitmap_vector_ones
	movslq	n_basic_blocks(%rip), %rsi
	testq	%rsi, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jle	.LBB0_376
# BB#364:                               # %for.body.lr.ph.i.i.i338
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	basic_block_info(%rip), %r8
	xorq	%r11, %r11
	.align	16, 0x90
.LBB0_365:                              # %for.body.i.i.i344
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_368 Depth 3
                                        #         Child Loop BB0_369 Depth 4
	movq	32(%r8,%r11,8), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$34, %rax
	jne	.LBB0_375
# BB#366:                               # %for.cond3.preheader.i.i.i
                                        #   in Loop: Header=BB0_365 Depth=2
	movl	expr_hash_table_size(%rip), %r9d
	testq	%r9, %r9
	je	.LBB0_375
# BB#367:                               # %for.body5.lr.ph.i.i.i345
                                        #   in Loop: Header=BB0_365 Depth=2
	movq	expr_hash_table(%rip), %r10
	xorq	%r14, %r14
	.align	16, 0x90
.LBB0_368:                              # %for.body5.i.i.i348
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_365 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_369 Depth 4
	movq	(%r10,%r14,8), %rdi
	jmp	.LBB0_369
	.align	16, 0x90
.LBB0_373:                              # %for.inc.i.i.i361
                                        #   in Loop: Header=BB0_369 Depth=4
	movq	16(%rdi), %rdi
.LBB0_369:                              # %for.body5.i.i.i348
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_365 Depth=2
                                        #       Parent Loop BB0_368 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rdi, %rdi
	je	.LBB0_374
# BB#370:                               # %for.body9.i.i.i
                                        #   in Loop: Header=BB0_369 Depth=4
	movq	(%rdi), %rcx
	movzbl	(%rcx), %ebx
	cmpq	$66, %rbx
	jne	.LBB0_373
# BB#371:                               # %if.then14.i.i.i
                                        #   in Loop: Header=BB0_369 Depth=4
	movq	8(%rcx), %rcx
	movl	(%rcx), %ecx
	andq	$67174399, %rcx         # imm = 0x400FFFF
	cmpq	$67108932, %rcx         # imm = 0x4000044
	je	.LBB0_373
# BB#372:                               # %if.end28.i.i.i
                                        #   in Loop: Header=BB0_369 Depth=4
	movq	transpout(%rip), %rbx
	movl	8(%rdi), %ecx
	movq	%rdx, %rax
	movq	$1, %rdx
	movq	(%rbx,%r11,8), %rbx
	shlq	%cl, %rdx
	notq	%rdx
	shrq	$6, %rcx
	andq	%rdx, 16(%rbx,%rcx,8)
	movq	%rax, %rdx
	jmp	.LBB0_373
	.align	16, 0x90
.LBB0_374:                              # %for.inc35.i.i.i
                                        #   in Loop: Header=BB0_368 Depth=3
	incq	%r14
	movq	%r14, %rcx
	andq	%rdx, %rcx
	cmpq	%r9, %rcx
	jne	.LBB0_368
.LBB0_375:                              # %for.inc37.i.i.i
                                        #   in Loop: Header=BB0_365 Depth=2
	incq	%r11
	movq	%r11, %rax
	andq	%rdx, %rax
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_365
.LBB0_376:                              # %compute_transpout.exit.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	hoist_vbeout(%rip), %rdi
	callq	sbitmap_vector_zero
	movq	hoist_vbein(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	callq	sbitmap_vector_zero
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_377:                              # %while.body.i.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_380 Depth 3
	movslq	n_basic_blocks(%rip), %r8
	testq	%r8, %r8
	jle	.LBB0_378
# BB#379:                               # %for.body.lr.ph.i1.i.i
                                        #   in Loop: Header=BB0_377 Depth=2
	movq	%rdx, 176(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	movq	%r8, %rbx
	.align	16, 0x90
.LBB0_380:                              # %for.body.i5.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_377 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r13, %r14
	movq	hoist_vbein(%rip), %rsi
	movq	antloc(%rip), %rcx
	movq	hoist_vbeout(%rip), %rdx
	movq	transp(%rip), %rax
	movq	-8(%rsi,%rbx,8), %rdi
	movq	-8(%rcx,%rbx,8), %rsi
	movq	-8(%rdx,%rbx,8), %rdx
	movq	-8(%rax,%rbx,8), %rcx
	leaq	-1(%rbx), %r15
	movq	%r8, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	decq	%r8
	movq	%r8, 184(%rsp)          # 8-byte Spill
	callq	sbitmap_a_or_b_and_c
	movl	n_basic_blocks(%rip), %ecx
	movq	%rax, %r13
	orq	%r14, %r13
	cmpq	%rcx, %r12
	je	.LBB0_381
# BB#458:                               # %if.then.i.i.i368
                                        #   in Loop: Header=BB0_380 Depth=3
	movq	hoist_vbeout(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	hoist_vbein(%rip), %rsi
	movq	%r15, %rdx
	callq	sbitmap_intersection_of_succs
.LBB0_381:                              # %for.body.for.cond.backedge_crit_edge.i.i.i
                                        #   in Loop: Header=BB0_380 Depth=3
	movq	%r15, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r15, %rbx
	movq	184(%rsp), %r8          # 8-byte Reload
	jg	.LBB0_380
# BB#382:                               # %for.end.i.i.i
                                        #   in Loop: Header=BB0_377 Depth=2
	movq	176(%rsp), %rdx         # 8-byte Reload
	incq	%rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB0_377
	jmp	.LBB0_383
.LBB0_378:                              # %for.end.thread.i.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	incq	%rdx
.LBB0_383:                              # %while.end.i.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_385
# BB#384:                               # %if.then12.i.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str12, %rsi
	callq	fprintf
.LBB0_385:                              # %compute_code_hoist_vbeinout.exit.i.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	dominators(%rip), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	calculate_dominance_info
	movq	gcse_file(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB0_387
# BB#386:                               # %if.then.i.i371
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$10, %rdi
	callq	fputc
.LBB0_387:                              # %compute_code_hoist_data.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	hoist_exprs(%rip), %rdi
	movl	n_basic_blocks(%rip), %esi
	callq	sbitmap_vector_zero
	movslq	n_exprs(%rip), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movl	expr_hash_table_size(%rip), %ecx
	movq	%rax, %r14
	movq	%r14, 112(%rsp)         # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB0_393
# BB#388:                               # %for.body.lr.ph.i.i372
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB0_389:                              # %for.body.i.i375
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_391 Depth 3
	movq	(%rax,%rdx,8), %rsi
	jmp	.LBB0_391
	.align	16, 0x90
.LBB0_390:                              # %for.body5.i.i382
                                        #   in Loop: Header=BB0_391 Depth=3
	movslq	8(%rsi), %rdi
	movq	%rsi, (%r14,%rdi,8)
	movq	16(%rsi), %rsi
.LBB0_391:                              # %for.body5.i.i382
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_389 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rsi, %rsi
	jne	.LBB0_390
# BB#392:                               # %for.inc8.i.i385
                                        #   in Loop: Header=BB0_389 Depth=2
	incq	%rdx
	movq	%rdx, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB0_389
.LBB0_393:                              # %for.cond10.preheader.i.i373
                                        #   in Loop: Header=BB0_31 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	xorq	%r8, %r8
	testq	%rax, %rax
	jle	.LBB0_412
	.align	16, 0x90
.LBB0_394:                              # %for.cond14.preheader.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_396 Depth 3
                                        #         Child Loop BB0_399 Depth 4
                                        #       Child Loop BB0_421 Depth 3
                                        #         Child Loop BB0_424 Depth 4
                                        #           Child Loop BB0_430 Depth 5
	movq	%r8, 144(%rsp)          # 8-byte Spill
	movq	hoist_vbeout(%rip), %rcx
	movq	(%rcx,%r8,8), %rdx
	movl	(%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_411
# BB#395:                               # %for.body19.lr.ph.i.i
                                        #   in Loop: Header=BB0_394 Depth=2
	movq	$6, %rcx
	movq	%r8, %r9
	shrq	%cl, %r9
	andq	$67108863, %r9          # imm = 0x3FFFFFF
	movq	%r9, 168(%rsp)          # 8-byte Spill
	movq	$1, %r10
	movq	%r8, %rcx
	shlq	%cl, %r10
	movq	%r10, 176(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_396:                              # %for.body19.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_394 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_399 Depth 4
	movq	%r13, %rbx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rbx
	movq	$6, %rcx
	shrq	%cl, %rbx
	movq	16(%rdx,%rbx,8), %rdx
	movq	$1, %r12
	movq	%r13, %rcx
	shlq	%cl, %r12
	testq	%rdx, %r12
	je	.LBB0_408
# BB#397:                               # %land.lhs.true.i.i390
                                        #   in Loop: Header=BB0_396 Depth=3
	movq	transpout(%rip), %rcx
	movq	(%rcx,%r8,8), %rcx
	movq	16(%rcx,%rbx,8), %rcx
	testq	%r12, %rcx
	je	.LBB0_408
# BB#398:                               # %land.lhs.true.i.i390
                                        #   in Loop: Header=BB0_396 Depth=3
	xorq	%r15, %r15
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	jle	.LBB0_408
	.align	16, 0x90
.LBB0_399:                              # %for.body38.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_394 Depth=2
                                        #       Parent Loop BB0_396 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r8, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rcx
	movq	%r15, %rdx
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	je	.LBB0_405
# BB#400:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB0_399 Depth=4
	movq	dominators(%rip), %rcx
	movq	(%rcx,%r15,8), %rcx
	movq	16(%rcx,%r9,8), %rcx
	testq	%r10, %rcx
	je	.LBB0_405
# BB#401:                               # %if.end.i.i391
                                        #   in Loop: Header=BB0_399 Depth=4
	movq	antloc(%rip), %rcx
	movq	(%rcx,%r15,8), %rcx
	movq	16(%rcx,%rbx,8), %rcx
	testq	%r12, %rcx
	je	.LBB0_405
# BB#402:                               # %if.end65.i.i
                                        #   in Loop: Header=BB0_399 Depth=4
	movq	%r8, %r14
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	hoist_expr_reaches_here_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rcx
	je	.LBB0_404
# BB#403:                               # %if.end65.i.i
                                        #   in Loop: Header=BB0_399 Depth=4
	movabsq	$1, %rcx
.LBB0_404:                              # %if.end65.i.i
                                        #   in Loop: Header=BB0_399 Depth=4
	movl	n_basic_blocks(%rip), %eax
	addq	%rcx, 184(%rsp)         # 8-byte Folded Spill
	movq	%r14, %r8
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	176(%rsp), %r10         # 8-byte Reload
	.align	16, 0x90
.LBB0_405:                              # %for.inc78.i.i
                                        #   in Loop: Header=BB0_399 Depth=4
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_399
# BB#406:                               # %for.end80.i.i
                                        #   in Loop: Header=BB0_396 Depth=3
	movq	$32, %rcx
	movq	184(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB0_408
# BB#407:                               # %if.then83.i.i
                                        #   in Loop: Header=BB0_396 Depth=3
	movq	hoist_exprs(%rip), %rcx
	movq	(%rcx,%r8,8), %rcx
	orq	%r12, 16(%rcx,%rbx,8)
	movq	$1, 160(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_408:                              # %for.inc94.i.i
                                        #   in Loop: Header=BB0_396 Depth=3
	movq	hoist_vbeout(%rip), %rcx
	movq	(%rcx,%r8,8), %rdx
	movl	(%rdx), %ecx
	incq	%r13
	movq	%r13, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	%rcx, %rsi
	jb	.LBB0_396
# BB#409:                               # %for.end96.i.i
                                        #   in Loop: Header=BB0_394 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	160(%rsp), %rsi         # 8-byte Reload
	testq	%rcx, %rsi
	jne	.LBB0_420
# BB#410:                               #   in Loop: Header=BB0_394 Depth=2
	movq	112(%rsp), %r14         # 8-byte Reload
	jmp	.LBB0_411
	.align	16, 0x90
.LBB0_420:                              # %for.cond100.preheader.i.i
                                        #   in Loop: Header=BB0_394 Depth=2
	movq	hoist_exprs(%rip), %rsi
	movq	(%rsi,%r8,8), %rcx
	movl	(%rcx), %ecx
	xorq	%r12, %r12
	testq	%rcx, %rcx
	movq	112(%rsp), %r14         # 8-byte Reload
	je	.LBB0_411
	jmp	.LBB0_421
	.align	16, 0x90
.LBB0_448:                              # %for.inc237.for.body106_crit_edge.i.i
                                        #   in Loop: Header=BB0_421 Depth=3
	movq	hoist_vbeout(%rip), %rcx
	movq	(%rcx,%r8,8), %rdx
.LBB0_421:                              # %for.body106.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_394 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_424 Depth 4
                                        #           Child Loop BB0_430 Depth 5
	movq	%r12, 136(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdi
	movq	$6, %rcx
	shrq	%cl, %rdi
	movq	%rdi, 184(%rsp)         # 8-byte Spill
	movq	16(%rdx,%rdi,8), %rdx
	movq	$1, %rdi
	movq	%r12, %rcx
	shlq	%cl, %rdi
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	testq	%rdx, %rdi
	je	.LBB0_447
# BB#422:                               # %for.body106.i.i
                                        #   in Loop: Header=BB0_421 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_447
# BB#423:                               # %for.body122.lr.ph.i.i
                                        #   in Loop: Header=BB0_421 Depth=3
	movq	%r12, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_424:                              # %for.body122.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_394 Depth=2
                                        #       Parent Loop BB0_421 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_430 Depth 5
	movq	%r8, %rax
	andq	%rbx, %rax
	movq	%r13, %r15
	andq	%rbx, %r15
	cmpq	%r15, %rax
	je	.LBB0_445
# BB#425:                               # %lor.lhs.false125.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	dominators(%rip), %rax
	movq	(%rax,%r13,8), %rax
	movq	16(%rax,%r9,8), %rax
	testq	%r10, %rax
	je	.LBB0_445
# BB#426:                               # %if.end138.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	antloc(%rip), %rax
	movq	(%rax,%r13,8), %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	16(%rax,%rcx,8), %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB0_445
# BB#427:                               # %if.end151.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r13,8), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	hoist_expr_reaches_here_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	jne	.LBB0_429
# BB#428:                               #   in Loop: Header=BB0_424 Depth=4
	movq	%rdx, %rbx
	movq	144(%rsp), %r8          # 8-byte Reload
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	176(%rsp), %r10         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_429:                              # %if.then162.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	(%r14,%rax,8), %rcx
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movq	basic_block_for_insn(%rip), %rax
	leaq	24(%rcx), %rbx
	.align	16, 0x90
.LBB0_430:                              # %while.cond.i.i
                                        #   Parent Loop BB0_31 Depth=1
                                        #     Parent Loop BB0_394 Depth=2
                                        #       Parent Loop BB0_421 Depth=3
                                        #         Parent Loop BB0_424 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	(%rbx), %rbx
	movq	8(%rbx), %r14
	testq	%rbx, %rbx
	je	.LBB0_432
# BB#431:                               # %while.cond.i.i
                                        #   in Loop: Header=BB0_430 Depth=5
	movslq	8(%r14), %rcx
	movq	32(%rax,%rcx,8), %rcx
	movl	88(%rcx), %ecx
	andq	%rdx, %rcx
	cmpq	%r15, %rcx
	jne	.LBB0_430
.LBB0_432:                              # %while.end.i.i400
                                        #   in Loop: Header=BB0_424 Depth=4
	testq	%rbx, %rbx
	je	.LBB0_433
# BB#434:                               # %if.end177.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_438
# BB#435:                               # %cond.true.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	32(%r14), %r12
	movzbl	(%r12), %eax
	movq	%rdx, %r15
	cmpq	$47, %rax
	je	.LBB0_437
# BB#436:                               # %cond.false.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	single_set_2
	movq	%rax, %r12
.LBB0_437:                              # %cond.end199.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	testq	%r12, %r12
	je	.LBB0_438
# BB#439:                               # %if.end203.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB0_441
# BB#440:                               # %if.then206.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	8(%r12), %rax
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	%rax, %rdx
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%rdx, 40(%rax)
.LBB0_441:                              # %if.end214.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	addq	$16, %r12
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	validate_change
	testq	%r15, %rax
	jne	.LBB0_443
# BB#442:                               #   in Loop: Header=BB0_424 Depth=4
	movq	%r15, %rbx
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	144(%rsp), %r8          # 8-byte Reload
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	176(%rsp), %r10         # 8-byte Reload
	movq	136(%rsp), %r12         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_443:                              # %if.then221.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	$1, %rax
	movb	%al, 16(%rbx)
	movq	120(%rsp), %rax         # 8-byte Reload
	testq	%r15, %rax
	movq	%r15, %rbx
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	144(%rsp), %r8          # 8-byte Reload
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	176(%rsp), %r10         # 8-byte Reload
	movq	136(%rsp), %r12         # 8-byte Reload
	jne	.LBB0_445
# BB#444:                               # %if.then223.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r8,8), %rsi
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r8, %r15
	movq	%r9, %r12
	callq	insert_insn_end_bb
	movq	176(%rsp), %r10         # 8-byte Reload
	movq	%r12, %r9
	movq	136(%rsp), %r12         # 8-byte Reload
	movq	%r15, %r8
	movq	$1, 120(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_445:                              # %for.inc233.i.i
                                        #   in Loop: Header=BB0_424 Depth=4
	incq	%r13
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_424
# BB#446:                               # %for.cond119.for.inc237.loopexit_crit_edge.i.i
                                        #   in Loop: Header=BB0_421 Depth=3
	movq	hoist_exprs(%rip), %rsi
.LBB0_447:                              # %for.inc237.i.i
                                        #   in Loop: Header=BB0_421 Depth=3
	movq	(%rsi,%r8,8), %rcx
	movl	(%rcx), %ecx
	incq	%r12
	movq	%r12, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	cmpq	%rcx, %rdx
	jb	.LBB0_448
	.align	16, 0x90
.LBB0_411:                              # %for.inc240.i.i
                                        #   in Loop: Header=BB0_394 Depth=2
	incq	%r8
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_394
.LBB0_412:                              # %hoist_code.exit.i
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%r14, %rdi
	callq	free
	movq	antloc(%rip), %rdi
	callq	free
	movq	transp(%rip), %rdi
	callq	free
	movq	comp(%rip), %rdi
	callq	free
	movq	hoist_vbein(%rip), %rdi
	callq	free
	movq	hoist_vbeout(%rip), %rdi
	callq	free
	movq	hoist_exprs(%rip), %rdi
	callq	free
	movq	transpout(%rip), %rdi
	callq	free
	movq	dominators(%rip), %rdi
	callq	free
.LBB0_413:                              # %one_code_hoisting_pass.exit
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	expr_hash_table(%rip), %rdi
	callq	free
	callq	free_gcse_mem
	movslq	bytes_used(%rip), %r15
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	96(%rsp), %r13          # 8-byte Reload
	jl	.LBB0_415
# BB#414:                               # %one_code_hoisting_pass.exit
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%rdx, %r15
.LBB0_415:                              # %one_code_hoisting_pass.exit
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	80(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB0_417
# BB#416:                               # %if.then80
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	$10, %rdi
	movq	%rsi, %rbx
	callq	fputc
	movq	%rbx, %rdi
	callq	fflush
	movq	%rbx, %rsi
.LBB0_417:                              # %if.end83
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	gcse_obstack+8(%rip), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rax, %rcx
	jbe	.LBB0_449
# BB#418:                               # %if.end83
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	gcse_obstack+32(%rip), %rax
	cmpq	%rax, %rcx
	jae	.LBB0_449
# BB#419:                               # %if.then89
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rcx, gcse_obstack+16(%rip)
	movq	%rcx, gcse_obstack+24(%rip)
	jmp	.LBB0_450
.LBB0_449:                              # %if.else90
                                        #   in Loop: Header=BB0_31 Depth=1
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movabsq	$gcse_obstack, %rdi
	movq	%rcx, %rsi
	callq	obstack_free
.LBB0_450:                              # %while.cond.backedge
                                        #   in Loop: Header=BB0_31 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	64(%rsp), %rsi          # 8-byte Reload
	testq	%rax, %r13
	jne	.LBB0_31
.LBB0_451:                              # %while.end
	movq	%rsi, %rbx
	callq	max_reg_num
	movl	%eax, max_gcse_regno(%rip)
	movq	%r14, %rdi
	callq	alloc_gcse_mem
	leaq	1(%rbx), %rdi
	movq	$1, %rsi
	callq	one_cprop_pass
	callq	free_gcse_mem
	movq	80(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB0_456
# BB#452:                               # %if.then96
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movl	n_basic_blocks(%rip), %ecx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%rdi, %r14
	callq	fprintf
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	movq	%rbx, %rdx
	jg	.LBB0_453
# BB#454:                               # %if.then96
	movabsq	$.L.str7, %rcx
	jmp	.LBB0_455
.LBB0_453:
	movabsq	$.L.str6, %rcx
.LBB0_455:                              # %if.then96
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %r8
	callq	fprintf
.LBB0_456:                              # %if.else114
	movabsq	$gcse_obstack, %rdi
	xorq	%rsi, %rsi
	callq	obstack_free
	movq	reg_set_table(%rip), %rdi
	callq	free
	movabsq	$reg_set_obstack, %rdi
	xorq	%rsi, %rsi
	callq	obstack_free
	callq	end_alias_analysis
	callq	max_reg_num
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	allocate_reg_info
	movzbl	run_jump_opt_after_gcse(%rip), %r14d
.LBB0_457:                              # %return
	movq	%r14, %rax
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_141:                              # %if.then59.i.i.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.computing_insn, %rcx
	movq	$3353, %rsi             # imm = 0xD19
	jmp	.LBB0_142
.LBB0_131:                              # %if.then19.i.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.handle_avail_expr, %rcx
	movq	$3465, %rsi             # imm = 0xD89
	jmp	.LBB0_142
.LBB0_148:                              # %if.then57.i.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.handle_avail_expr, %rcx
	movq	$3501, %rsi             # imm = 0xDAD
	jmp	.LBB0_142
.LBB0_268:                              # %if.then37.i.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.pre_delete, %rcx
	movq	$4969, %rsi             # imm = 0x1369
	jmp	.LBB0_142
.LBB0_323:                              # %if.then.i.i.i.i281
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.pre_insert_copy_insn, %rcx
	movq	$4859, %rsi             # imm = 0x12FB
	jmp	.LBB0_142
.LBB0_433:                              # %if.then176.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.hoist_code, %rcx
	movq	$5801, %rsi             # imm = 0x16A9
	jmp	.LBB0_142
.LBB0_438:                              # %if.then202.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.hoist_code, %rcx
	movq	$5807, %rsi             # imm = 0x16AF
.LBB0_142:                              # %if.then59.i.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	gcse_main, .Ltmp17-gcse_main
	.cfi_endproc

	.align	16, 0x90
	.type	alloc_gcse_mem,@function
alloc_gcse_mem:                         # @alloc_gcse_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp27:
	.cfi_def_cfa_offset 48
.Ltmp28:
	.cfi_offset %rbx, -40
.Ltmp29:
	.cfi_offset %r12, -32
.Ltmp30:
	.cfi_offset %r14, -24
.Ltmp31:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	callq	get_max_uid
	movl	bytes_used(%rip), %ecx
	leaq	4(,%rax,4), %r15
	leaq	4(%rcx,%rax,4), %rax
	movl	%eax, bytes_used(%rip)
	movabsq	$4294967292, %rdi       # imm = 0xFFFFFFFC
	andq	%r15, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r14, uid_cuid(%rip)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	memset
	testq	%rbx, %rbx
	movabsq	$0, %rax
	je	.LBB1_6
# BB#1:
	movq	%rbx, %rcx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %edx
	movzbl	rtx_class(%rdx), %edx
	cmpq	$105, %rdx
	jne	.LBB1_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB1_2 Depth=1
	movslq	8(%rcx), %rdx
	movl	%eax, (%r14,%rdx,4)
	incq	%rax
	jmp	.LBB1_5
	.align	16, 0x90
.LBB1_4:                                # %if.else
                                        #   in Loop: Header=BB1_2 Depth=1
	movslq	8(%rcx), %rdx
	movl	%eax, (%r14,%rdx,4)
.LBB1_5:                                # %for.inc
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_2
.LBB1_6:                                # %for.end
	movl	%eax, max_cuid(%rip)
	movl	bytes_used(%rip), %ecx
	leaq	8(,%rax,8), %r14
	leaq	8(%rcx,%rax,8), %rax
	movl	%eax, bytes_used(%rip)
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%r14, %rdi
	callq	xmalloc
	movq	%rax, cuid_insn(%rip)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r14, %rdx
	callq	memset
	jmp	.LBB1_7
	.align	16, 0x90
.LBB1_10:                               # %for.inc37
                                        #   in Loop: Header=BB1_7 Depth=1
	movq	24(%rbx), %rbx
.LBB1_7:                                # %for.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB1_11
# BB#8:                                 # %for.body24
                                        #   in Loop: Header=BB1_7 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_10
# BB#9:                                 # %if.then32
                                        #   in Loop: Header=BB1_7 Depth=1
	movq	$32, %rcx
	leaq	1(%r12), %rax
	movq	cuid_insn(%rip), %rdx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%rbx, (%rdx,%r12,8)
	movq	%rax, %r12
	jmp	.LBB1_10
.LBB1_11:                               # %for.end41
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movl	max_gcse_regno(%rip), %esi
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, reg_set_bitmap(%rip)
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movl	bytes_used(%rip), %ecx
	movq	%rax, reg_set_in_block(%rip)
	leaq	(%rcx,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	movl	n_basic_blocks(%rip), %edi
	movl	bytes_used(%rip), %ecx
	movq	%rax, modify_mem_list(%rip)
	leaq	(%rcx,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	modify_mem_list(%rip), %rdi
	movq	%rax, canon_modify_mem_list(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movslq	n_basic_blocks(%rip), %rdx
	movq	canon_modify_mem_list(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, modify_mem_list_set(%rip)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, canon_modify_mem_list_set(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp32:
	.size	alloc_gcse_mem, .Ltmp32-alloc_gcse_mem
	.cfi_endproc

	.align	16, 0x90
	.type	one_cprop_pass,@function
one_cprop_pass:                         # @one_cprop_pass
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp39:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp40:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp41:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp42:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp44:
	.cfi_def_cfa_offset 128
.Ltmp45:
	.cfi_offset %rbx, -48
.Ltmp46:
	.cfi_offset %r12, -40
.Ltmp47:
	.cfi_offset %r13, -32
.Ltmp48:
	.cfi_offset %r14, -24
.Ltmp49:
	.cfi_offset %r15, -16
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	movl	%ebx, const_prop_count(%rip)
	movslq	max_cuid(%rip), %rdx
	movl	%ebx, copy_prop_count(%rip)
	movq	$61, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$3, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$34, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$11, %rcx
	movq	$11, %rdi
	jb	.LBB2_2
# BB#1:                                 # %entry
	orq	$1, %rax
	movq	%rax, %rdi
.LBB2_2:                                # %entry
	movl	bytes_used(%rip), %eax
	movl	%edi, set_hash_table_size(%rip)
	leaq	(%rax,%rdi,8), %rax
	movl	%eax, bytes_used(%rip)
	andq	$536870911, %rdi        # imm = 0x1FFFFFFF
	shlq	$3, %rdi
	callq	xmalloc
	movl	set_hash_table_size(%rip), %edx
	movq	%rax, set_hash_table(%rip)
	movl	%ebx, n_sets(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	memset
	movq	$1, %rdi
	callq	compute_hash_table
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_4
# BB#3:                                 # %if.then
	movq	set_hash_table(%rip), %rdx
	movl	n_sets(%rip), %r8d
	movl	set_hash_table_size(%rip), %ecx
	movabsq	$.L.str31, %rsi
	callq	dump_hash_table
.LBB2_4:                                # %if.end
	movslq	n_sets(%rip), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	jle	.LBB2_87
# BB#5:                                 # %if.then1
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movl	n_basic_blocks(%rip), %r14d
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, cprop_pavloc(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, cprop_absaltered(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, cprop_avin(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	cprop_absaltered(%rip), %rdi
	movq	cprop_pavloc(%rip), %rsi
	movq	%rax, cprop_avout(%rip)
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%rdx, %rdx
	movq	$1, %rcx
	callq	compute_local_properties
	movq	cprop_pavloc(%rip), %rdi
	movq	cprop_absaltered(%rip), %rsi
	movq	cprop_avout(%rip), %rdx
	movq	cprop_avin(%rip), %rcx
	callq	compute_available
	movslq	n_basic_blocks(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB2_84
# BB#6:                                 # %for.body.lr.ph.i
	movq	$1, %rbx
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_7:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_8 Depth 2
                                        #       Child Loop BB2_14 Depth 3
                                        #         Child Loop BB2_21 Depth 4
                                        #           Child Loop BB2_22 Depth 5
                                        #           Child Loop BB2_29 Depth 5
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	reg_set_bitmap(%rip), %rdi
	callq	bitmap_clear
	callq	clear_modify_mem_tables
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rcx
	movq	(%rcx), %r13
	testq	%r13, %r13
	je	.LBB2_83
	jmp	.LBB2_8
	.align	16, 0x90
.LBB2_82:                               # %for.inc.land.rhs_crit_edge.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rcx
.LBB2_8:                                # %land.rhs.i.preheader
                                        #   Parent Loop BB2_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_14 Depth 3
                                        #         Child Loop BB2_21 Depth 4
                                        #           Child Loop BB2_22 Depth 5
                                        #           Child Loop BB2_29 Depth 5
	movq	8(%rcx), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %r13
	je	.LBB2_83
# BB#9:                                 # %for.body10.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_81
# BB#10:                                # %if.end.i.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movl	%eax, reg_use_count(%rip)
	leaq	32(%r13), %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	xorq	%rdx, %rdx
	movabsq	$find_used_regs, %rsi
	callq	note_uses
	movq	%r13, %rdi
	callq	find_reg_equal_equiv_note
	testq	%rax, %rax
	je	.LBB2_12
# BB#11:                                # %if.then3.i.i
                                        #   in Loop: Header=BB2_8 Depth=2
	addq	$8, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	find_used_regs
.LBB2_12:                               # %for.condthread-pre-split.i.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movslq	reg_use_count(%rip), %rax
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	jle	.LBB2_79
# BB#13:                                # %for.body.lr.ph.i.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movabsq	$reg_use_table, %r14
	.align	16, 0x90
.LBB2_14:                               # %for.body.i.i
                                        #   Parent Loop BB2_7 Depth=1
                                        #     Parent Loop BB2_8 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB2_21 Depth 4
                                        #           Child Loop BB2_22 Depth 5
                                        #           Child Loop BB2_29 Depth 5
	movq	(%r14), %rdi
	movl	8(%rdi), %r12d
	movl	max_gcse_regno(%rip), %eax
	cmpq	%rax, %r12
	jae	.LBB2_78
# BB#15:                                # %if.end15.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	%r13, %rsi
	callq	oprs_not_set_p
	movq	%r15, %rbx
	testq	%rbx, %rax
	jne	.LBB2_17
# BB#16:                                #   in Loop: Header=BB2_14 Depth=3
	movq	%rbx, %r15
	jmp	.LBB2_78
	.align	16, 0x90
.LBB2_17:                               # %if.end20.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	set_hash_table_size(%rip), %ecx
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rcx
	movq	set_hash_table(%rip), %rax
	movq	(%rax,%rdx,8), %r15
	xorq	%rax, %rax
	testq	%r15, %r15
	movq	%r12, %rcx
	jne	.LBB2_21
# BB#18:                                #   in Loop: Header=BB2_14 Depth=3
	movq	%rbx, %r15
	jmp	.LBB2_78
	.align	16, 0x90
.LBB2_21:                               # %land.rhs7.i.i.preheader.i.i
                                        #   Parent Loop BB2_7 Depth=1
                                        #     Parent Loop BB2_8 Depth=2
                                        #       Parent Loop BB2_14 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB2_22 Depth 5
                                        #           Child Loop BB2_29 Depth 5
	movq	%rax, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB2_22:                               # %land.rhs7.i.i.i.i
                                        #   Parent Loop BB2_7 Depth=1
                                        #     Parent Loop BB2_8 Depth=2
                                        #       Parent Loop BB2_14 Depth=3
                                        #         Parent Loop BB2_21 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	(%r15), %rax
	movq	8(%rax), %rdx
	movl	8(%rdx), %esi
	movq	%rcx, %rdx
	andq	%rbx, %rdx
	cmpq	%rdx, %rsi
	je	.LBB2_23
# BB#19:                                # %while.cond5.i.i.i.i
                                        #   in Loop: Header=BB2_22 Depth=5
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB2_22
	jmp	.LBB2_20
	.align	16, 0x90
.LBB2_23:                               # %while.body2.lr.ph.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movq	%rbx, %r8
	movslq	8(%r13), %rcx
	movq	basic_block_for_insn(%rip), %rsi
	movq	32(%rsi,%rcx,8), %rcx
	movslq	88(%rcx), %rsi
	movq	cprop_avin(%rip), %rdi
	movl	8(%r15), %ecx
	movq	(%rdi,%rsi,8), %r9
	movq	%rcx, %rdi
	shrq	$6, %rdi
	movq	16(%r9,%rdi,8), %rdi
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	%rdi, %rbx
	jne	.LBB2_24
# BB#27:                                # %do.body.i.i.i.preheader.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movq	16(%r15), %r15
	testq	%r15, %r15
	movq	%r8, %rbx
	je	.LBB2_28
	.align	16, 0x90
.LBB2_29:                               # %land.rhs.i.i.i.i
                                        #   Parent Loop BB2_7 Depth=1
                                        #     Parent Loop BB2_8 Depth=2
                                        #       Parent Loop BB2_14 Depth=3
                                        #         Parent Loop BB2_21 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movl	8(%rcx), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB2_31
# BB#30:                                # %while.body2.i.loopexit.i.i
                                        #   in Loop: Header=BB2_29 Depth=5
	movl	8(%r15), %ecx
	movq	%rcx, %rdi
	shrq	$6, %rdi
	movq	16(%r9,%rdi,8), %rdi
	movq	%rbx, %rsi
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	%rdi, %rbx
	movq	%rsi, %rbx
	jne	.LBB2_25
.LBB2_31:                               # %do.body.i.i.i.backedge.i
                                        #   in Loop: Header=BB2_29 Depth=5
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB2_29
	jmp	.LBB2_32
	.align	16, 0x90
.LBB2_24:                               #   in Loop: Header=BB2_21 Depth=4
	movq	%r8, %rbx
.LBB2_25:                               # %while.end.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	testq	%r15, %r15
	je	.LBB2_26
# BB#33:                                # %if.end12.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB2_34
# BB#36:                                # %if.end15.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movq	16(%rax), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_39
# BB#37:                                # %if.end15.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_41
# BB#38:                                # %if.end15.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB2_42
	jmp	.LBB2_41
.LBB2_39:                               # %if.end15.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	cmpq	$134, %rcx
	je	.LBB2_42
# BB#40:                                # %if.end15.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	cmpq	$140, %rcx
	je	.LBB2_42
.LBB2_41:                               # %lor.lhs.false49.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	oprs_not_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_43
.LBB2_42:                               # %if.then52.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movq	%r15, 72(%rsp)          # 8-byte Spill
.LBB2_43:                               # %if.end53.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB2_44
# BB#45:                                # %if.end58.i.i.i
                                        #   in Loop: Header=BB2_21 Depth=4
	movl	8(%rbx), %ecx
	movl	set_hash_table_size(%rip), %esi
	xorq	%rdx, %rdx
	movq	%rcx, %rax
	divq	%rsi
	movq	set_hash_table(%rip), %rax
	movq	(%rax,%rdx,8), %r15
	testq	%r15, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	jne	.LBB2_21
	jmp	.LBB2_46
.LBB2_20:                               #   in Loop: Header=BB2_14 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB2_46
.LBB2_32:                               #   in Loop: Header=BB2_14 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB2_46:                               # %find_avail_set.exit.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	%rbx, %r15
	testq	%rax, %rax
	je	.LBB2_78
# BB#47:                                # %if.end24.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB2_48
# BB#49:                                # %if.end30.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	16(%rax), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_58
# BB#50:                                # %if.end30.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_78
# BB#51:                                # %if.end30.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB2_60
# BB#52:                                # %if.end30.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	cmpq	$7, %rcx
	jne	.LBB2_78
# BB#53:                                # %land.lhs.true112.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	8(%rbx), %eax
	cmpq	$53, %rax
	jb	.LBB2_78
# BB#54:                                # %land.lhs.true112.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	cmpq	%r12, %rax
	je	.LBB2_78
# BB#55:                                # %if.then124.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	(%r14), %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	try_replace_reg
	testq	%r15, %rax
	je	.LBB2_78
# BB#56:                                # %if.then128.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	copy_prop_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, copy_prop_count(%rip)
	testq	%rdi, %rdi
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	je	.LBB2_78
# BB#57:                                # %if.then132.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	8(%r13), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str36, %rsi
	movq	%r12, %rdx
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str37, %rsi
	callq	fprintf
	jmp	.LBB2_78
.LBB2_58:                               # %if.end30.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	cmpq	$134, %rcx
	je	.LBB2_60
# BB#59:                                # %if.end30.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	cmpq	$140, %rcx
	jne	.LBB2_78
.LBB2_60:                               # %if.then72.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movzbl	(%r13), %eax
	cmpq	$32, %rax
	jne	.LBB2_64
# BB#61:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	(%r14), %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	try_replace_reg
	testq	%r15, %rax
	jne	.LBB2_62
.LBB2_64:                               # %if.else.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB2_78
# BB#65:                                # %land.lhs.true91.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movzbl	(%r13), %eax
	cmpq	$33, %rax
	jne	.LBB2_78
# BB#66:                                # %land.lhs.true96.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	%r13, %rdi
	callq	condjump_p
	testq	%r15, %rax
	je	.LBB2_78
# BB#67:                                # %land.lhs.true99.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	%r13, %rdi
	callq	simplejump_p
	testq	%r15, %rax
	jne	.LBB2_78
# BB#68:                                # %if.then102.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r12
	movq	(%r14), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	16(%r12), %rdi
	movq	%rbx, %rdx
	callq	simplify_replace_rtx
	movq	16(%r12), %rsi
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	rtx_equal_p
	xorq	%rcx, %rcx
	testq	%r15, %rax
	jne	.LBB2_77
# BB#69:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	global_rtl(%rip), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rax, %rcx
	movq	%rcx, %rdx
	jne	.LBB2_71
# BB#70:                                # %if.then8.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	%r13, %rdi
	callq	delete_insn
	jmp	.LBB2_74
.LBB2_62:                               # %if.then80.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	const_prop_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, const_prop_count(%rip)
	testq	%rdi, %rdi
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	je	.LBB2_78
# BB#63:                                # %if.then83.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	xorq	%rax, %rax
	movabsq	$.L.str34, %rsi
	movq	%r12, %rdx
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	8(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str35, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	print_rtl
	movq	gcse_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	jmp	.LBB2_78
.LBB2_26:                               #   in Loop: Header=BB2_14 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB2_46
.LBB2_44:                               #   in Loop: Header=BB2_14 Depth=3
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB2_46
.LBB2_28:                               #   in Loop: Header=BB2_14 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB2_46
.LBB2_71:                               # %if.else.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	addq	$16, %r12
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	validate_change
	xorq	%rcx, %rcx
	testq	%r15, %rax
	je	.LBB2_77
# BB#72:                                # %if.end16.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB2_74
# BB#73:                                # %if.then21.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	%r13, %rdi
	callq	emit_barrier_after
.LBB2_74:                               # %if.end24.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	const_prop_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, const_prop_count(%rip)
	movq	$1, %rax
	movb	%al, run_jump_opt_after_gcse(%rip)
	testq	%rdi, %rdi
	je	.LBB2_76
# BB#75:                                # %if.then26.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	8(%r13), %ecx
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str38, %rsi
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	print_rtl
	movq	gcse_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB2_76:                               # %if.end33.i.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	purge_dead_edges
	movq	$1, %rcx
.LBB2_77:                               # %cprop_jump.exit.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	orq	%rcx, 64(%rsp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_78:                               # %for.inc.i.i
                                        #   in Loop: Header=BB2_14 Depth=3
	movl	reg_use_count(%rip), %eax
	addq	$8, %r14
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, reg_use_count(%rip)
	jg	.LBB2_14
.LBB2_79:                               # %cprop_insn.exit.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movzbl	(%r13), %eax
	movq	64(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, 48(%rsp)          # 8-byte Folded Spill
	cmpq	$37, %rax
	movq	56(%rsp), %rbx          # 8-byte Reload
	je	.LBB2_81
# BB#80:                                # %if.then23.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	%r13, %rdi
	callq	mark_oprs_set
.LBB2_81:                               # %for.inc.i
                                        #   in Loop: Header=BB2_8 Depth=2
	movq	24(%r13), %r13
	testq	%r13, %r13
	jne	.LBB2_82
	.align	16, 0x90
.LBB2_83:                               # %for.inc28.i
                                        #   in Loop: Header=BB2_7 Depth=1
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB2_7
.LBB2_84:                               # %for.end29.i
	movq	gcse_file(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB2_86
# BB#85:                                # %if.then32.i
	movq	$10, %rdi
	callq	fputc
.LBB2_86:                               # %cprop.exit
	movq	cprop_pavloc(%rip), %rdi
	callq	free
	movq	cprop_absaltered(%rip), %rdi
	callq	free
	movq	cprop_avin(%rip), %rdi
	callq	free
	movq	cprop_avout(%rip), %rdi
	callq	free
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	48(%rsp), %r12          # 8-byte Reload
.LBB2_87:                               # %if.end2
	movq	set_hash_table(%rip), %rdi
	callq	free
	movq	gcse_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB2_89
# BB#88:                                # %if.then4
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movl	bytes_used(%rip), %r8d
	movabsq	$.L.str32, %rsi
	xorq	%rax, %rax
	movq	%r14, %rcx
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movl	copy_prop_count(%rip), %ecx
	movl	const_prop_count(%rip), %edx
	movabsq	$.L.str33, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB2_89:                               # %if.end7
	movq	%r12, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_34:                               # %if.then14.i.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.find_avail_set, %rcx
	movq	$4040, %rsi             # imm = 0xFC8
	jmp	.LBB2_35
.LBB2_48:                               # %if.then29.i.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.cprop_insn, %rcx
	movq	$4202, %rsi             # imm = 0x106A
.LBB2_35:                               # %if.then14.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp50:
	.size	one_cprop_pass, .Ltmp50-one_cprop_pass
	.cfi_endproc

	.align	16, 0x90
	.type	free_gcse_mem,@function
free_gcse_mem:                          # @free_gcse_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp52:
	.cfi_def_cfa_offset 16
	movq	uid_cuid(%rip), %rdi
	callq	free
	movq	cuid_insn(%rip), %rdi
	callq	free
	movq	reg_set_bitmap(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_2
# BB#1:                                 # %if.then
	callq	bitmap_clear
	movq	reg_set_bitmap(%rip), %rdi
	callq	free
	movq	$0, reg_set_bitmap(%rip)
.LBB3_2:                                # %do.end
	movq	reg_set_in_block(%rip), %rdi
	callq	free
	callq	clear_modify_mem_tables
	movq	modify_mem_list(%rip), %rdi
	callq	free
	movq	canon_modify_mem_list(%rip), %rdi
	callq	free
	movq	modify_mem_list_set(%rip), %rdi
	movq	$0, modify_mem_list(%rip)
	movq	$0, canon_modify_mem_list(%rip)
	testq	%rdi, %rdi
	je	.LBB3_4
# BB#3:                                 # %if.then3
	callq	bitmap_clear
	movq	modify_mem_list_set(%rip), %rdi
	callq	free
	movq	$0, modify_mem_list_set(%rip)
.LBB3_4:                                # %do.body6
	movq	canon_modify_mem_list_set(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_6
# BB#5:                                 # %if.then8
	callq	bitmap_clear
	movq	canon_modify_mem_list_set(%rip), %rdi
	callq	free
	movq	$0, canon_modify_mem_list_set(%rip)
.LBB3_6:                                # %do.end10
	popq	%rax
	retq
.Ltmp53:
	.size	free_gcse_mem, .Ltmp53-free_gcse_mem
	.cfi_endproc

	.globl	delete_null_pointer_checks
	.align	16, 0x90
	.type	delete_null_pointer_checks,@function
delete_null_pointer_checks:             # @delete_null_pointer_checks
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
	subq	$112, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 160
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
	movslq	n_basic_blocks(%rip), %rcx
	cmpq	$2, %rcx
	jl	.LBB4_44
# BB#1:                                 # %if.end
	cmpq	$1001, %rcx             # imm = 0x3E9
	jl	.LBB4_3
# BB#2:                                 # %land.lhs.true
	movslq	n_edges(%rip), %rax
	cqto
	idivq	%rcx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$19, %rax
	jg	.LBB4_44
.LBB4_3:                                # %if.end4
	callq	max_reg_num
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r13
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	$34, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
                                        # kill: RAX<def> R13<kill>
	addq	$63, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$6, %rcx
	shrq	%cl, %rax
	imulq	%rsi, %rax
	cmpq	$10485761, %rax         # imm = 0xA00001
	movq	%r13, %rbx
	jb	.LBB4_5
# BB#4:                                 # %if.end.i
	leaq	10485759(%rsi), %rax
	xorq	%rdx, %rdx
	divq	%rsi
	movq	%rax, %rbx
	movq	$6, %rcx
	shlq	%cl, %rbx
.LBB4_5:                                # %get_bitmap_width.exit
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 88(%rsp)
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 96(%rsp)
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$4, %rsi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rsi
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	jle	.LBB4_17
# BB#6:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	leaq	64(%rsp), %r14
	.align	16, 0x90
.LBB4_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	8(%rax), %r15
	movzbl	(%r15), %eax
	cmpq	$33, %rax
	jne	.LBB4_16
# BB#8:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r12, %rax
	je	.LBB4_16
# BB#9:                                 # %lor.lhs.false18
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%r15, %rdi
	callq	onlyjump_p
	testq	%r12, %rax
	je	.LBB4_16
# BB#10:                                # %if.end22
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	get_condition
	testq	%rax, %rax
	je	.LBB4_16
# BB#11:                                # %lor.lhs.false25
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	(%rax), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$102, %rcx
	jne	.LBB4_16
# BB#12:                                # %lor.lhs.false35
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	16(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$54, %rcx
	jne	.LBB4_16
# BB#13:                                # %lor.lhs.false41
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	8(%rax), %rax
	movl	(%rax), %esi
	movq	$13, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	andq	$2040, %rdi             # imm = 0x7F8
	movq	const_tiny_rtx(%rdi), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB4_16
# BB#14:                                # %lor.lhs.false41
                                        #   in Loop: Header=BB4_7 Depth=1
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB4_16
# BB#15:                                # %if.end64
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	8(%rax), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movl	%eax, (%rcx,%rbx,4)
	.align	16, 0x90
.LBB4_16:                               # %for.inc
                                        #   in Loop: Header=BB4_7 Depth=1
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rsi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB4_7
.LBB4_17:                               # %for.cond69.preheader
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$54, %rax
	jl	.LBB4_43
# BB#18:                                # %for.body72.lr.ph
	movq	$53, %rdi
	leaq	72(%rsp), %r14
	.align	16, 0x90
.LBB4_19:                               # %for.body72
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_22 Depth 2
                                        #       Child Loop BB4_23 Depth 3
                                        #     Child Loop BB4_45 Depth 2
	movl	%edi, 76(%rsp)
	movq	(%rsp), %rax            # 8-byte Reload
	addq	%rax, %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movq	%rdi, %rax
	jle	.LBB4_21
# BB#20:                                # %for.body72
                                        #   in Loop: Header=BB4_19 Depth=1
	movq	%r13, %rax
.LBB4_21:                               # %for.body72
                                        #   in Loop: Header=BB4_19 Depth=1
	movl	%eax, 80(%rsp)
	movq	88(%rsp), %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	sbitmap_vector_zero
	movl	n_basic_blocks(%rip), %esi
	movq	%rbx, %rdi
	callq	sbitmap_vector_zero
	movslq	n_basic_blocks(%rip), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	jle	.LBB4_41
	.align	16, 0x90
.LBB4_22:                               # %for.body.i
                                        #   Parent Loop BB4_19 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_23 Depth 3
	movq	basic_block_info(%rip), %rcx
	movl	%r13d, 72(%rsp)
	movq	32(%rcx,%r13,8), %rcx
	movq	8(%rcx), %rdx
	movq	(%rcx), %r12
	movq	24(%rdx), %r15
	cmpq	%r15, %r12
	je	.LBB4_40
	.align	16, 0x90
.LBB4_23:                               # %for.body12.i
                                        #   Parent Loop BB4_19 Depth=1
                                        #     Parent Loop BB4_22 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB4_38
# BB#24:                                # %cond.true.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	32(%r12), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB4_26
# BB#25:                                # %cond.false.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB4_26:                               # %cond.end39.i
                                        #   in Loop: Header=BB4_23 Depth=3
	testq	%rbx, %rbx
	jne	.LBB4_28
# BB#27:                                # %if.then41.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	32(%r12), %rdi
	movabsq	$invalidate_nonnull_info, %rsi
	movq	%r14, %rdx
	callq	note_stores
	jmp	.LBB4_38
	.align	16, 0x90
.LBB4_28:                               # %if.end45.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB4_33
# BB#29:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_33
# BB#30:                                # %land.lhs.true63.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movl	8(%rax), %eax
	movl	76(%rsp), %edx
	movq	%rax, %rcx
	subq	%rdx, %rcx
	jb	.LBB4_33
# BB#31:                                # %land.lhs.true63.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movl	80(%rsp), %edx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rdx, %rax
	jae	.LBB4_33
# BB#32:                                # %if.then74.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	%rcx, %rax
	movabsq	$4294967232, %rdx       # imm = 0xFFFFFFC0
	andq	%rdx, %rax
	movq	$1, %rdx
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	(%rsi,%r13,8), %rsi
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%rsi,%rax)
	.align	16, 0x90
.LBB4_33:                               # %if.end88.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	32(%r12), %rdi
	movabsq	$invalidate_nonnull_info, %rsi
	movq	%r14, %rdx
	callq	note_stores
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB4_38
# BB#34:                                # %land.lhs.true99.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_38
# BB#35:                                # %land.lhs.true110.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movl	8(%rax), %eax
	movl	76(%rsp), %edx
	movq	%rax, %rcx
	subq	%rdx, %rcx
	jb	.LBB4_38
# BB#36:                                # %land.lhs.true110.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movl	80(%rsp), %edx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rdx, %rax
	jae	.LBB4_38
# BB#37:                                # %if.then124.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	%rcx, %rax
	movabsq	$4294967232, %rdx       # imm = 0xFFFFFFC0
	andq	%rdx, %rax
	movq	$1, %rdx
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	(%rsi,%r13,8), %rsi
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%rsi,%rax)
	.align	16, 0x90
.LBB4_38:                               # %for.inc.i
                                        #   in Loop: Header=BB4_23 Depth=3
	movq	24(%r12), %r12
	cmpq	%r15, %r12
	jne	.LBB4_23
# BB#39:                                # %for.inc149.i.loopexit
                                        #   in Loop: Header=BB4_22 Depth=2
	movl	n_basic_blocks(%rip), %eax
.LBB4_40:                               # %for.inc149.i
                                        #   in Loop: Header=BB4_22 Depth=2
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB4_22
.LBB4_41:                               # %for.end150.i
                                        #   in Loop: Header=BB4_19 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	compute_available
	movslq	n_basic_blocks(%rip), %rsi
	xorq	%r15, %r15
	testq	%rsi, %rsi
	jle	.LBB4_42
	.align	16, 0x90
.LBB4_45:                               # %for.body154.i
                                        #   Parent Loop BB4_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	(%rax,%r15,4), %ebx
	movl	76(%rsp), %eax
	cmpq	%rax, %rbx
	jb	.LBB4_56
# BB#46:                                # %for.body154.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movl	80(%rsp), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	jae	.LBB4_56
# BB#47:                                # %if.end171.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movq	8(%rax), %r12
	movq	%r12, %rdi
	leaq	104(%rsp), %rsi
	callq	get_condition
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB4_56
# BB#48:                                # %if.end175.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movl	76(%rsp), %eax
	subq	%rax, %rbx
	movq	%rbx, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%r15,8), %rdx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	16(%rdx,%rax), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	je	.LBB4_56
# BB#49:                                # %if.end194.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	104(%rsp), %rbx
	cmpq	%r12, %rbx
	movq	$1, %rcx
	je	.LBB4_51
# BB#50:                                # %if.else.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	%r12, %rdi
	callq	prev_nonnote_insn
	movq	$2, %rcx
	cmpq	%rax, %rbx
	jne	.LBB4_56
.LBB4_51:                               # %if.end204.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	%rcx, %rbx
	movzbl	(%r13), %eax
	cmpq	$102, %rax
	jne	.LBB4_53
# BB#52:                                # %if.then209.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	64(%r12), %rdi
	callq	gen_jump
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	emit_jump_insn_before
	movq	64(%r12), %rcx
	movq	%rcx, 64(%rax)
	movl	32(%rcx), %edx
	incq	%rdx
	movl	%edx, 32(%rcx)
	movq	%rax, %rdi
	callq	emit_barrier_after
.LBB4_53:                               # %if.end228.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	%r12, %rdi
	callq	delete_insn
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	cmpq	$2, %rbx
	jne	.LBB4_55
# BB#54:                                # %if.then232.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	104(%rsp), %rdi
	callq	delete_insn
.LBB4_55:                               # %if.end234.i
                                        #   in Loop: Header=BB4_45 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r15,8), %rdi
	callq	purge_dead_edges
	movq	48(%rsp), %rax          # 8-byte Reload
	xorq	%rcx, %rcx
	movl	%ecx, (%rax,%r15,4)
	.align	16, 0x90
.LBB4_56:                               # %for.inc242.i
                                        #   in Loop: Header=BB4_45 Depth=2
	incq	%r15
	movslq	n_basic_blocks(%rip), %rsi
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB4_45
.LBB4_42:                               # %for.cond69.backedge
                                        #   in Loop: Header=BB4_19 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %r13           # 8-byte Reload
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB4_19
.LBB4_43:                               # %for.end79
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	88(%rsp), %rdi
	callq	free
	movq	96(%rsp), %rdi
	callq	free
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	free
.LBB4_44:                               # %return
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	delete_null_pointer_checks, .Ltmp71-delete_null_pointer_checks
	.cfi_endproc

	.align	16, 0x90
	.type	dump_hash_table,@function
dump_hash_table:                        # @dump_hash_table
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
	subq	$32, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 80
.Ltmp84:
	.cfi_offset %rbx, -48
.Ltmp85:
	.cfi_offset %r12, -40
.Ltmp86:
	.cfi_offset %r13, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, %r12
	movq	$2, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %r15
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	jle	.LBB5_6
# BB#1:
	xorq	%rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_4 Depth 2
	movq	(%r14,%rax,8), %rdx
	jmp	.LBB5_4
	.align	16, 0x90
.LBB5_3:                                # %for.body7
                                        #   in Loop: Header=BB5_4 Depth=2
	movslq	8(%rdx), %rsi
	movq	%rdx, (%r12,%rsi,8)
	movl	%eax, (%r15,%rsi,4)
	movq	16(%rdx), %rdx
.LBB5_4:                                # %for.body7
                                        #   Parent Loop BB5_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdx, %rdx
	jne	.LBB5_3
# BB#5:                                 # %for.inc13
                                        #   in Loop: Header=BB5_2 Depth=1
	incq	%rax
	movq	%rax, %rdx
	andq	%rcx, %rdx
	movq	%r13, %rsi
	andq	%rcx, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB5_2
.LBB5_6:                                # %for.end14
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r13, %rcx
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	%r13, %r8
	callq	fprintf
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_11
# BB#7:
	movq	$1, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r13
	movq	%r13, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB5_8:                                # %for.body19
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r12,%rbx,8), %r14
	testq	%r14, %r14
	je	.LBB5_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB5_8 Depth=1
	movl	-4(%r15,%rbx,4), %ecx
	movl	8(%r14), %edx
	xorq	%rax, %rax
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movabsq	$.L.str14, %rsi
	callq	fprintf
	movq	(%r14), %rsi
	movq	%r13, %rdi
	callq	print_rtl
	movq	$10, %rdi
	movq	%r13, %rsi
	movq	16(%rsp), %r13          # 8-byte Reload
	callq	fputc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
.LBB5_10:                               # %for.inc32
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%rbx, %rax
	andq	%rcx, %rax
	incq	%rbx
	cmpq	%r13, %rax
	jne	.LBB5_8
.LBB5_11:                               # %for.end34
	movq	$10, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	fputc
	movq	%r12, %rdi
	callq	free
	movq	%r15, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp89:
	.size	dump_hash_table, .Ltmp89-dump_hash_table
	.cfi_endproc

	.align	16, 0x90
	.type	hoist_expr_reaches_here_p,@function
hoist_expr_reaches_here_p:              # @hoist_expr_reaches_here_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp96:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp97:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp98:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp99:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp101:
	.cfi_def_cfa_offset 80
.Ltmp102:
	.cfi_offset %rbx, -48
.Ltmp103:
	.cfi_offset %r12, -40
.Ltmp104:
	.cfi_offset %r13, -32
.Ltmp105:
	.cfi_offset %r14, -24
.Ltmp106:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	xorq	%rbx, %rbx
	testq	%rdx, %rdx
	jne	.LBB6_2
# BB#1:                                 # %if.then
	movslq	n_basic_blocks(%rip), %rdi
	movq	$1, %rsi
	movq	$1, %rbx
	callq	xcalloc
	movq	%rax, %rdx
.LBB6_2:                                # %if.end
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967232, %rdi       # imm = 0xFFFFFFC0
	andq	%r12, %rdi
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	32(%r14), %r12
	movq	$6, %rcx
	shrq	%cl, %rdi
	testq	%r12, %r12
	movq	$1, %r15
	je	.LBB6_10
# BB#3:                                 # %for.body.lr.ph
	movq	$1, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$entry_exit_blocks, %r14
	.align	16, 0x90
.LBB6_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r12), %rsi
	xorq	%r15, %r15
	cmpq	%r14, %rsi
	je	.LBB6_10
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB6_4 Depth=1
	movslq	88(%rsi), %rax
	movzbl	(%rdx,%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB6_9
# BB#6:                                 # %if.else9
                                        #   in Loop: Header=BB6_4 Depth=1
	movq	comp(%rip), %rcx
	movq	(%rcx,%rax,8), %rcx
	movq	16(%rcx,%rdi,8), %rcx
	movq	16(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rcx
	jne	.LBB6_10
# BB#7:                                 # %if.else17
                                        #   in Loop: Header=BB6_4 Depth=1
	movq	transp(%rip), %rcx
	movq	(%rcx,%rax,8), %rcx
	movq	16(%rcx,%rdi,8), %rcx
	testq	%rbx, %rcx
	je	.LBB6_10
# BB#8:                                 # %if.else31
                                        #   in Loop: Header=BB6_4 Depth=1
	movq	$1, %rcx
	movb	%cl, (%rdx,%rax)
	movq	%rdi, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %r13
	callq	hoist_expr_reaches_here_p
	movq	%rbx, %rdi
	movq	%r13, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_10
.LBB6_9:                                # %for.inc
                                        #   in Loop: Header=BB6_4 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	movq	$1, %r15
	jne	.LBB6_4
.LBB6_10:                               # %for.end
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB6_12
# BB#11:                                # %if.then44
	movq	%rdx, %rdi
	callq	free
.LBB6_12:                               # %if.end45
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp107:
	.size	hoist_expr_reaches_here_p, .Ltmp107-hoist_expr_reaches_here_p
	.cfi_endproc

	.align	16, 0x90
	.type	insert_insn_end_bb,@function
insert_insn_end_bb:                     # @insert_insn_end_bb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp114:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp115:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp116:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp117:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp119:
	.cfi_def_cfa_offset 80
.Ltmp120:
	.cfi_offset %rbx, -48
.Ltmp121:
	.cfi_offset %r12, -40
.Ltmp122:
	.cfi_offset %r13, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	40(%rdi), %rax
	movq	%rsi, %r14
	movq	8(%r14), %r13
	movq	(%rdi), %rdi
	movl	8(%rax), %ecx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rax, %rsi
	callq	process_insert_insn
	movzbl	(%r13), %ecx
	movq	%rax, %rbx
	cmpq	$34, %rcx
	je	.LBB7_10
# BB#1:                                 # %entry
	cmpq	$33, %rcx
	je	.LBB7_5
# BB#2:                                 # %entry
	cmpq	$32, %rcx
	jne	.LBB7_18
# BB#3:                                 # %land.lhs.true
	movq	40(%r14), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB7_5
# BB#4:                                 # %lor.lhs.false4
	movzbl	48(%rax), %eax
	testq	$2, %rax
	je	.LBB7_9
.LBB7_5:                                # %if.end
	movq	32(%r13), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB7_7
# BB#6:                                 # %if.then47
	movq	%r13, %rdi
	callq	prev_real_insn
	movq	%rax, %r13
.LBB7_7:                                # %if.end49
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	jmp	.LBB7_19
.LBB7_9:                                # %if.else
	cmpq	$34, %rcx
	jne	.LBB7_18
.LBB7_10:                               # %land.lhs.true54
	movq	40(%r14), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB7_12
# BB#11:                                # %lor.lhs.false58
	movzbl	48(%rax), %eax
	testq	$2, %rax
	je	.LBB7_18
.LBB7_12:                               # %if.end96
	movq	(%r14), %rsi
	movq	%r13, %rdi
	callq	find_first_parameter_load
	movabsq	$4294967216, %rcx       # imm = 0xFFFFFFB0
	jmp	.LBB7_13
	.align	16, 0x90
.LBB7_16:                               # %while.body
                                        #   in Loop: Header=BB7_13 Depth=1
	movq	24(%rax), %rax
.LBB7_13:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %edx
	cmpq	$36, %rdx
	je	.LBB7_16
# BB#14:                                # %while.cond
                                        #   in Loop: Header=BB7_13 Depth=1
	cmpq	$37, %rdx
	jne	.LBB7_17
# BB#15:                                # %land.rhs
                                        #   in Loop: Header=BB7_13 Depth=1
	movl	40(%rax), %edx
	cmpq	%rcx, %rdx
	je	.LBB7_16
.LBB7_17:                               # %while.end
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_insn_before
	jmp	.LBB7_19
.LBB7_18:                               # %if.else111
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	emit_insn_after
.LBB7_19:                               # %if.end114
	movq	%rax, %r13
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB7_25
# BB#20:                                # %for.cond.preheader
	movq	8(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB7_26
# BB#21:
	movq	$1, %r12
	movabsq	$record_set_info, %r14
	.align	16, 0x90
.LBB7_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %r15
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_24
# BB#23:                                # %if.then134
                                        #   in Loop: Header=BB7_22 Depth=1
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	callq	add_label_notes
.LBB7_24:                               # %if.end138
                                        #   in Loop: Header=BB7_22 Depth=1
	movq	32(%r15), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	note_stores
	movq	8(%rbx), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB7_22
	jmp	.LBB7_26
.LBB7_25:                               # %if.else142
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	add_label_notes
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	record_one_set
.LBB7_26:                               # %if.end143
	movl	gcse_create_count(%rip), %eax
	movq	gcse_file(%rip), %rdi
	incq	%rax
	movl	%eax, gcse_create_count(%rip)
	testq	%rdi, %rdi
	je	.LBB7_27
# BB#28:                                # %if.then146
	movl	8(%r13), %ecx
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	88(%rax), %edx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	gcse_file(%rip), %rdi
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %edx
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB7_27:                               # %if.end154
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	insert_insn_end_bb, .Ltmp125-insert_insn_end_bb
	.cfi_endproc

	.align	16, 0x90
	.type	process_insert_insn,@function
process_insert_insn:                    # @process_insert_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp130:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp131:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp132:
	.cfi_def_cfa_offset 32
.Ltmp133:
	.cfi_offset %rbx, -32
.Ltmp134:
	.cfi_offset %r14, -24
.Ltmp135:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	callq	copy_rtx
	movq	%rax, %rbx
	callq	start_sequence
	movzbl	2(%r14), %esi
	movq	%rbx, %rdi
	callq	general_operand
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB8_2
# BB#1:                                 # %if.then
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB8_3
.LBB8_2:                                # %if.else
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %rdi
	callq	insn_invalid_p
	testq	%r15, %rax
	jne	.LBB8_4
.LBB8_3:                                # %if.end9
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB8_4:                                # %if.then8
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.process_insert_insn, %rcx
	movq	$4610, %rsi             # imm = 0x1202
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp136:
	.size	process_insert_insn, .Ltmp136-process_insert_insn
	.cfi_endproc

	.align	16, 0x90
	.type	add_label_notes,@function
add_label_notes:                        # @add_label_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp143:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp144:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp145:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp146:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp147:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp148:
	.cfi_def_cfa_offset 64
.Ltmp149:
	.cfi_offset %rbx, -48
.Ltmp150:
	.cfi_offset %r12, -40
.Ltmp151:
	.cfi_offset %r13, -32
.Ltmp152:
	.cfi_offset %r14, -24
.Ltmp153:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movl	(%r13), %eax
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$67, %rcx
	jne	.LBB9_3
# BB#1:                                 # %if.then
	movq	8(%r13), %rdx
	movq	56(%r14), %rcx
	movq	$4, %rdi
	movq	$13, %rsi
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%r14)
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB9_8
# BB#2:                                 # %if.then15
	movl	32(%rax), %ecx
	incq	%rcx
	movl	%ecx, 32(%rax)
	jmp	.LBB9_8
.LBB9_3:                                # %if.end21
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB9_8
# BB#4:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB9_5:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_11 Depth 2
	movq	%r15, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r15
	cmpq	$69, %rax
	jne	.LBB9_6
# BB#10:                                # %if.then42
                                        #   in Loop: Header=BB9_5 Depth=1
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %r12
	testq	%r12, %r12
	jle	.LBB9_7
	jmp	.LBB9_11
	.align	16, 0x90
.LBB9_12:                               # %for.body50.for.body50_crit_edge
                                        #   in Loop: Header=BB9_11 Depth=2
	movq	(%r13,%rbx,8), %rax
.LBB9_11:                               # %for.body50.lr.ph
                                        #   Parent Loop BB9_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r12
	movq	8(%rax,%r12,8), %rdi
	movq	%r14, %rsi
	callq	add_label_notes
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_12
	jmp	.LBB9_7
	.align	16, 0x90
.LBB9_6:                                # %for.body
                                        #   in Loop: Header=BB9_5 Depth=1
	cmpq	$101, %rax
	jne	.LBB9_7
# BB#9:                                 # %if.then32
                                        #   in Loop: Header=BB9_5 Depth=1
	movq	(%r13,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	add_label_notes
.LBB9_7:                                # %for.cond.backedge
                                        #   in Loop: Header=BB9_5 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_5
.LBB9_8:                                # %for.end61
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp154:
	.size	add_label_notes, .Ltmp154-add_label_notes
	.cfi_endproc

	.align	16, 0x90
	.type	record_set_info,@function
record_set_info:                        # @record_set_info
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB10_2
# BB#1:                                 # %land.lhs.true
	movl	8(%rdi), %edi
	cmpq	$53, %rdi
	jb	.LBB10_2
# BB#3:                                 # %if.then
	movq	%rdx, %rsi
	jmp	record_one_set  # TAILCALL
.LBB10_2:                               # %if.end
	retq
.Ltmp155:
	.size	record_set_info, .Ltmp155-record_set_info
	.cfi_endproc

	.align	16, 0x90
	.type	record_one_set,@function
record_one_set:                         # @record_one_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp161:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp162:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp163:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp165:
	.cfi_def_cfa_offset 48
.Ltmp166:
	.cfi_offset %rbx, -40
.Ltmp167:
	.cfi_offset %r12, -32
.Ltmp168:
	.cfi_offset %r14, -24
.Ltmp169:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	movslq	reg_set_table_size(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jg	.LBB11_2
# BB#1:                                 # %if.then
	leaq	100(%r12), %r15
	movq	reg_set_table(%rip), %rdi
	movq	%r15, %rsi
	andq	$536870911, %rsi        # imm = 0x1FFFFFFF
	shlq	$3, %rsi
	callq	xrealloc
	movslq	reg_set_table_size(%rip), %rcx
	movq	%rax, reg_set_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%r15, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%r15d, reg_set_table_size(%rip)
.LBB11_2:                               # %if.end
	movq	reg_set_obstack+32(%rip), %rax
	movq	reg_set_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB11_4
# BB#3:                                 # %if.then7
	movabsq	$reg_set_obstack, %rdi
	movq	$16, %rsi
	callq	_obstack_newchunk
	movq	reg_set_obstack+24(%rip), %rcx
.LBB11_4:                               # %if.end8
	addq	$16, %rcx
	movq	%rcx, reg_set_obstack+24(%rip)
	movq	reg_set_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB11_6
# BB#5:                                 # %if.then17
	movzbl	reg_set_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, reg_set_obstack+80(%rip)
.LBB11_6:                               # %if.end18
	movslq	reg_set_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, reg_set_obstack+24(%rip)
	movq	reg_set_obstack+32(%rip), %rbx
	movq	reg_set_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB11_8
# BB#7:                                 # %if.then39
	movq	%rbx, reg_set_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB11_8:                               # %if.end42
	movl	bytes_used(%rip), %ecx
	movq	%rdx, reg_set_obstack+16(%rip)
	addq	$16, %rcx
	movl	%ecx, bytes_used(%rip)
	movq	%r14, 8(%rax)
	movq	$32, %rcx
	movq	reg_set_table(%rip), %rdx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	(%rdx,%r12,8), %rcx
	movq	%rcx, (%rax)
	movq	reg_set_table(%rip), %rcx
	movq	%rax, (%rcx,%r12,8)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp170:
	.size	record_one_set, .Ltmp170-record_one_set
	.cfi_endproc

	.align	16, 0x90
	.type	compute_local_properties,@function
compute_local_properties:               # @compute_local_properties
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp177:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp178:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp179:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp180:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp181:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp182:
	.cfi_def_cfa_offset 96
.Ltmp183:
	.cfi_offset %rbx, -48
.Ltmp184:
	.cfi_offset %r12, -40
.Ltmp185:
	.cfi_offset %r13, -32
.Ltmp186:
	.cfi_offset %r14, -24
.Ltmp187:
	.cfi_offset %r15, -16
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, %r13
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	je	.LBB12_4
# BB#1:                                 # %if.then
	movl	n_basic_blocks(%rip), %esi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	%rbx, %rdi
                                        # kill: RBX<def> RDI<kill>
	je	.LBB12_3
# BB#2:                                 # %if.then2
	callq	sbitmap_vector_zero
	jmp	.LBB12_4
.LBB12_3:                               # %if.else
	callq	sbitmap_vector_ones
.LBB12_4:                               # %if.end3
	testq	%r13, %r13
	je	.LBB12_6
# BB#5:                                 # %if.then5
	movl	n_basic_blocks(%rip), %esi
	movq	%r13, %rdi
	callq	sbitmap_vector_zero
.LBB12_6:                               # %if.end6
	testq	%r15, %r15
	je	.LBB12_8
# BB#7:                                 # %if.then8
	movl	n_basic_blocks(%rip), %esi
	movq	%r15, %rdi
	callq	sbitmap_vector_zero
.LBB12_8:                               # %if.end9
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	jne	.LBB12_9
# BB#10:                                # %if.end9
	movabsq	$expr_hash_table, %rax
	jmp	.LBB12_11
.LBB12_9:
	movabsq	$set_hash_table, %rax
.LBB12_11:                              # %if.end9
	testq	%rcx, %rcx
	jne	.LBB12_12
# BB#13:                                # %if.end9
	movabsq	$expr_hash_table_size, %rcx
	jmp	.LBB12_14
.LBB12_12:
	movabsq	$set_hash_table_size, %rcx
.LBB12_14:                              # %if.end9
	movq	%rbx, %rdx
	movl	(%rcx), %ecx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB12_30
# BB#15:
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	.align	16, 0x90
.LBB12_16:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_17 Depth 2
                                        #       Child Loop BB12_23 Depth 3
                                        #       Child Loop BB12_27 Depth 3
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%rsi,8), %r12
	jmp	.LBB12_17
	.align	16, 0x90
.LBB12_28:                              # %if.end64
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	$0, 40(%r12)
	movq	16(%r12), %r12
.LBB12_17:                              # %for.body
                                        #   Parent Loop BB12_16 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_23 Depth 3
                                        #       Child Loop BB12_27 Depth 3
	testq	%r12, %r12
	je	.LBB12_29
# BB#18:                                # %for.body18
                                        #   in Loop: Header=BB12_17 Depth=2
	movl	8(%r12), %esi
	testq	%rdx, %rdx
	je	.LBB12_20
# BB#19:                                # %if.then20
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	(%r12), %rdi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	callq	compute_transp
	movq	%rbx, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
.LBB12_20:                              # %if.end22
                                        #   in Loop: Header=BB12_17 Depth=2
	testq	%r15, %r15
	je	.LBB12_24
# BB#21:                                # %if.then24
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	24(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_24
# BB#22:                                # %for.body27.lr.ph
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	%rsi, %rbx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rbx
	movq	$1, %r8
	movq	basic_block_for_insn(%rip), %rdi
	movq	%rsi, %rcx
	shlq	%cl, %r8
	movq	$6, %rcx
	shrq	%cl, %rbx
	.align	16, 0x90
.LBB12_23:                              # %for.body27
                                        #   Parent Loop BB12_16 Depth=1
                                        #     Parent Loop BB12_17 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rax), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%rdi,%rcx,8), %rcx
	movslq	88(%rcx), %rcx
	movq	(%r15,%rcx,8), %rcx
	orq	%r8, 16(%rcx,%rbx,8)
	movb	%r14b, 16(%rax)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB12_23
.LBB12_24:                              # %if.end35
                                        #   in Loop: Header=BB12_17 Depth=2
	testq	%r13, %r13
	je	.LBB12_28
# BB#25:                                # %if.then37
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	32(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_28
# BB#26:                                # %for.body40.lr.ph
                                        #   in Loop: Header=BB12_17 Depth=2
	movq	%rsi, %rbx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rbx
	movq	$1, %r8
	movq	basic_block_for_insn(%rip), %rdi
	movq	%rsi, %rcx
	shlq	%cl, %r8
	movq	$6, %rcx
	shrq	%cl, %rbx
	.align	16, 0x90
.LBB12_27:                              # %for.body40
                                        #   Parent Loop BB12_16 Depth=1
                                        #     Parent Loop BB12_17 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rax), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%rdi,%rcx,8), %rcx
	movslq	88(%rcx), %rcx
	movq	(%r13,%rcx,8), %rcx
	orq	%r8, 16(%rcx,%rbx,8)
	movb	%r14b, 17(%rax)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB12_27
	jmp	.LBB12_28
	.align	16, 0x90
.LBB12_29:                              # %for.inc67
                                        #   in Loop: Header=BB12_16 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB12_16
.LBB12_30:                              # %for.end68
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp188:
	.size	compute_local_properties, .Ltmp188-compute_local_properties
	.cfi_endproc

	.align	16, 0x90
	.type	compute_transp,@function
compute_transp:                         # @compute_transp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp195:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp196:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp197:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp198:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp199:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp200:
	.cfi_def_cfa_offset 96
.Ltmp201:
	.cfi_offset %rbx, -48
.Ltmp202:
	.cfi_offset %r12, -40
.Ltmp203:
	.cfi_offset %r13, -32
.Ltmp204:
	.cfi_offset %r14, -24
.Ltmp205:
	.cfi_offset %r15, -16
	movq	%rcx, %r9
	movq	%rdx, %r10
	movq	%r10, 40(%rsp)          # 8-byte Spill
	movq	%rdi, %r8
	testq	%r8, %r8
	je	.LBB13_25
# BB#1:                                 # %if.end.lr.ph
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	movq	%rsi, %rcx
	andq	%rcx, %rax
	movq	$1, %rdx
	movq	%rcx, %r14
	shlq	%cl, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	notq	%rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	$6, %rcx
	shrq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB13_2:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_35 Depth 2
                                        #       Child Loop BB13_36 Depth 3
                                        #     Child Loop BB13_8 Depth 2
                                        #       Child Loop BB13_9 Depth 3
                                        #     Child Loop BB13_46 Depth 2
                                        #       Child Loop BB13_52 Depth 3
	movzbl	(%r8), %eax
	leaq	-44(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$25, %rcx
	ja	.LBB13_44
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$58776579, %rdx         # imm = 0x380DC03
	jne	.LBB13_25
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB13_2 Depth=1
	cmpq	$17, %rcx
	je	.LBB13_15
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB13_2 Depth=1
	cmpq	$22, %rcx
	jne	.LBB13_44
# BB#6:                                 # %for.cond128.preheader
                                        #   in Loop: Header=BB13_2 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	movq	%r9, %rbx
	jle	.LBB13_42
# BB#7:                                 # %for.body130.lr.ph
                                        #   in Loop: Header=BB13_2 Depth=1
	xorq	%r15, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB13_35
	.align	16, 0x90
.LBB13_8:                               # %for.body130.us
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB13_9 Depth 3
	movq	canon_modify_mem_list(%rip), %rax
	movq	(%rax,%r15,8), %rax
	testq	%rax, %rax
	je	.LBB13_14
	.align	16, 0x90
.LBB13_9:                               # %while.body.us
                                        #   Parent Loop BB13_2 Depth=1
                                        #     Parent Loop BB13_8 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rax), %rdi
	movl	(%rdi), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	je	.LBB13_13
# BB#10:                                # %if.end165.us
                                        #   in Loop: Header=BB13_9 Depth=3
	movq	%rbx, %r13
	movq	16(%rax), %rbx
	movq	8(%rbx), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r8, %rcx
	movq	%r8, %r12
	movabsq	$rtx_addr_varies_p, %r8
	callq	canon_true_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_11
# BB#12:                                # %if.end204.us
                                        #   in Loop: Header=BB13_9 Depth=3
	movq	16(%rbx), %rax
	testq	%rax, %rax
	movq	%r13, %rbx
	movq	%r12, %r8
	jne	.LBB13_9
	jmp	.LBB13_14
.LBB13_13:                              # %if.else152.us
                                        #   in Loop: Header=BB13_8 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, 16(%rax,%rdx,8)
	jmp	.LBB13_14
.LBB13_11:                              # %if.else191.us
                                        #   in Loop: Header=BB13_8 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, 16(%rax,%rdx,8)
	movq	%r13, %rbx
	movq	%r12, %r8
	.align	16, 0x90
.LBB13_14:                              # %for.inc208.us
                                        #   in Loop: Header=BB13_8 Depth=2
	incq	%r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB13_8
	jmp	.LBB13_42
	.align	16, 0x90
.LBB13_44:                              # %sw.epilog
                                        #   in Loop: Header=BB13_2 Depth=1
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB13_25
# BB#45:                                # %for.body222.lr.ph
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB13_46:                              # %for.body222
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB13_52 Depth 3
	movq	%rdx, %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%rdi), %eax
	leaq	-1(%rdi), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB13_47
# BB#51:                                # %for.cond248.preheader
                                        #   in Loop: Header=BB13_46 Depth=2
	movq	(%r8,%rdi,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	movq	%r9, %r13
	jle	.LBB13_48
	.align	16, 0x90
.LBB13_52:                              # %for.body254
                                        #   Parent Loop BB13_2 Depth=1
                                        #     Parent Loop BB13_46 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdi, %r12
	movq	(%rax,%r15,8), %rdi
	movq	%r14, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r8, %rbx
	callq	compute_transp
	movq	%r12, %rdi
	movq	%rbx, %r8
	movq	(%r8,%rdi,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB13_52
	jmp	.LBB13_48
	.align	16, 0x90
.LBB13_47:                              # %for.body222
                                        #   in Loop: Header=BB13_46 Depth=2
	cmpq	$101, %rax
	movq	%r9, %r13
	jne	.LBB13_48
# BB#49:                                # %if.then228
                                        #   in Loop: Header=BB13_46 Depth=2
	movq	%r13, %r9
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	(%r8,%rdi,8), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB13_43
# BB#50:                                # %if.end236
                                        #   in Loop: Header=BB13_46 Depth=2
	movq	%r14, %rsi
	movq	%r10, %rdx
	movq	%r9, %r13
	movq	%r13, %rcx
	movq	%r8, %rbx
	callq	compute_transp
	movq	%rbx, %r8
.LBB13_48:                              # %for.cond219.backedge
                                        #   in Loop: Header=BB13_46 Depth=2
	movq	%r13, %r9
	movq	$32, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_46
	jmp	.LBB13_25
	.align	16, 0x90
.LBB13_35:                              # %for.body130
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB13_36 Depth 3
	movq	canon_modify_mem_list(%rip), %rax
	movq	(%rax,%r15,8), %rax
	testq	%rax, %rax
	je	.LBB13_41
	.align	16, 0x90
.LBB13_36:                              # %while.body
                                        #   Parent Loop BB13_2 Depth=1
                                        #     Parent Loop BB13_35 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rax), %rdi
	movl	(%rdi), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	je	.LBB13_37
# BB#38:                                # %if.end165
                                        #   in Loop: Header=BB13_36 Depth=3
	movq	%rbx, %r13
	movq	16(%rax), %rbx
	movq	8(%rbx), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r8, %rcx
	movq	%r8, %r12
	movabsq	$rtx_addr_varies_p, %r8
	callq	canon_true_dependence
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_39
# BB#40:                                # %if.end204
                                        #   in Loop: Header=BB13_36 Depth=3
	movq	16(%rbx), %rax
	testq	%rax, %rax
	movq	%r13, %rbx
	movq	%r12, %r8
	jne	.LBB13_36
	jmp	.LBB13_41
.LBB13_37:                              # %if.then141
                                        #   in Loop: Header=BB13_35 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	orq	%rcx, 16(%rax,%rdx,8)
	jmp	.LBB13_41
.LBB13_39:                              # %if.then180
                                        #   in Loop: Header=BB13_35 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	orq	%rcx, 16(%rax,%rdx,8)
	movq	%r13, %rbx
	movq	%r12, %r8
	.align	16, 0x90
.LBB13_41:                              # %for.inc208
                                        #   in Loop: Header=BB13_35 Depth=2
	incq	%r15
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB13_35
.LBB13_42:                              # %for.end210
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%rbx, %r9
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	8(%r8), %rdi
.LBB13_43:                              # %repeat.backedge
                                        #   in Loop: Header=BB13_2 Depth=1
	testq	%rdi, %rdi
	movq	%rdi, %r8
	jne	.LBB13_2
	jmp	.LBB13_25
.LBB13_15:                              # %sw.bb
	movl	8(%r8), %r8d
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r9
	je	.LBB13_26
# BB#16:                                # %if.then1
	cmpq	$52, %r8
	ja	.LBB13_22
# BB#17:                                # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %r9
	testq	%r9, %r9
	jle	.LBB13_25
# BB#18:                                # %for.body.lr.ph
	movq	$6, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	movq	reg_set_in_block(%rip), %r11
	movq	$1, %rdx
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
.LBB13_19:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r11,%rdx,8), %rcx
	movq	16(%rcx,%rsi,8), %rcx
	testq	%rax, %rcx
	je	.LBB13_21
# BB#20:                                # %if.then15
                                        #   in Loop: Header=BB13_19 Depth=1
	movq	-8(%r10,%rdx,8), %rcx
	movq	16(%rsp), %rbx          # 8-byte Reload
	orq	%rdi, 16(%rcx,%rbx,8)
.LBB13_21:                              # %for.inc
                                        #   in Loop: Header=BB13_19 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rdx
	cmpq	%r9, %rbx
	jl	.LBB13_19
	jmp	.LBB13_25
.LBB13_26:                              # %if.else51
	cmpq	$52, %r8
	ja	.LBB13_32
# BB#27:                                # %for.cond57.preheader
	movslq	n_basic_blocks(%rip), %r9
	testq	%r9, %r9
	jle	.LBB13_25
# BB#28:                                # %for.body59.lr.ph
	movq	$6, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	movq	reg_set_in_block(%rip), %r11
	movq	$1, %rdx
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	movq	(%rsp), %rdi            # 8-byte Reload
.LBB13_29:                              # %for.body59
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r11,%rdx,8), %rcx
	movq	16(%rcx,%rsi,8), %rcx
	testq	%rax, %rcx
	je	.LBB13_31
# BB#30:                                # %if.then77
                                        #   in Loop: Header=BB13_29 Depth=1
	movq	-8(%r10,%rdx,8), %rcx
	movq	16(%rsp), %rbx          # 8-byte Reload
	andq	%rdi, 16(%rcx,%rbx,8)
.LBB13_31:                              # %for.inc89
                                        #   in Loop: Header=BB13_29 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rdx
	cmpq	%r9, %rbx
	jl	.LBB13_29
	jmp	.LBB13_25
.LBB13_22:                              # %if.else
	movq	reg_set_table(%rip), %rax
	movq	(%rax,%r8,8), %rax
	testq	%rax, %rax
	je	.LBB13_25
# BB#23:                                # %for.body32.lr.ph
	movq	basic_block_for_insn(%rip), %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rdi           # 8-byte Reload
.LBB13_24:                              # %for.body32
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rdx
	movslq	8(%rdx), %rdx
	movq	32(%rcx,%rdx,8), %rdx
	movslq	88(%rdx), %rdx
	movq	(%r10,%rdx,8), %rdx
	orq	%rdi, 16(%rdx,%rsi,8)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB13_24
	jmp	.LBB13_25
.LBB13_32:                              # %if.else92
	movq	reg_set_table(%rip), %rax
	movq	(%rax,%r8,8), %rax
	testq	%rax, %rax
	je	.LBB13_25
# BB#33:                                # %for.body100.lr.ph
	movq	basic_block_for_insn(%rip), %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	(%rsp), %rdi            # 8-byte Reload
.LBB13_34:                              # %for.body100
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rdx
	movslq	8(%rdx), %rdx
	movq	32(%rcx,%rdx,8), %rdx
	movslq	88(%rdx), %rdx
	movq	(%r10,%rdx,8), %rdx
	andq	%rdi, 16(%rdx,%rsi,8)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB13_34
.LBB13_25:                              # %for.end267
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp206:
	.size	compute_transp, .Ltmp206-compute_transp
	.cfi_endproc

	.align	16, 0x90
	.type	compute_hash_table,@function
compute_hash_table:                     # @compute_hash_table
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp213:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp214:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp215:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp216:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp217:
	.cfi_def_cfa_offset 48
.Ltmp218:
	.cfi_offset %rbx, -48
.Ltmp219:
	.cfi_offset %r12, -40
.Ltmp220:
	.cfi_offset %r13, -32
.Ltmp221:
	.cfi_offset %r14, -24
.Ltmp222:
	.cfi_offset %r15, -16
	movq	reg_set_in_block(%rip), %rax
	movl	n_basic_blocks(%rip), %esi
	movq	%rdi, %r15
	movq	%rax, %rdi
	callq	sbitmap_vector_zero
	callq	clear_modify_mem_tables
	movl	max_gcse_regno(%rip), %eax
	movq	$2, %rcx
	shlq	%cl, %rax
	movl	bytes_used(%rip), %ecx
	leaq	(%rax,%rax,2), %rax
	addq	%rax, %rcx
	movl	%ecx, bytes_used(%rip)
	movabsq	$4294967292, %rdi       # imm = 0xFFFFFFFC
	andq	%rax, %rdi
	callq	xmalloc
	movl	max_gcse_regno(%rip), %ecx
	movq	%rax, reg_avail_info(%rip)
	testq	%rcx, %rcx
	je	.LBB14_3
# BB#1:
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	andq	%rsi, %rdi
	leaq	(%rdi,%rdi,2), %rdi
	incq	%rdx
	movl	%esi, (%rax,%rdi,4)
	movq	%rdx, %rdi
	andq	%rsi, %rdi
	cmpq	%rcx, %rdi
	jb	.LBB14_2
.LBB14_3:                               # %for.cond3.preheader
	movslq	n_basic_blocks(%rip), %rcx
	xorq	%rdx, %rdx
	movl	%edx, current_bb(%rip)
	testq	%rcx, %rcx
	jle	.LBB14_41
# BB#4:                                 # %for.body6.lr.ph
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_5:                               # %for.body6
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_6 Depth 2
                                        #       Child Loop BB14_11 Depth 3
                                        #     Child Loop BB14_20 Depth 2
                                        #       Child Loop BB14_30 Depth 3
	movq	$32, %rcx
	movq	%rdx, %rax
	movq	basic_block_info(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rsi,%rax,8), %rax
	movq	(%rax), %r14
	jmp	.LBB14_6
	.align	16, 0x90
.LBB14_18:                              # %for.inc44
                                        #   in Loop: Header=BB14_6 Depth=2
	movslq	current_bb(%rip), %rdx
	movq	basic_block_info(%rip), %rax
	movq	24(%r14), %r14
	movq	32(%rax,%rdx,8), %rax
.LBB14_6:                               # %for.body6
                                        #   Parent Loop BB14_5 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB14_11 Depth 3
	testq	%r14, %r14
	je	.LBB14_19
# BB#7:                                 # %land.rhs
                                        #   in Loop: Header=BB14_6 Depth=2
	movq	8(%rax), %rcx
	movq	24(%rcx), %rcx
	cmpq	%rcx, %r14
	je	.LBB14_19
# BB#8:                                 # %for.body17
                                        #   in Loop: Header=BB14_6 Depth=2
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB14_18
# BB#9:                                 # %if.end
                                        #   in Loop: Header=BB14_6 Depth=2
	cmpq	$34, %rax
	jne	.LBB14_17
# BB#10:                                # %for.cond28.preheader
                                        #   in Loop: Header=BB14_6 Depth=2
	movq	reg_avail_info(%rip), %rdx
	movq	uid_cuid(%rip), %r9
	movq	reg_set_in_block(%rip), %r8
	addq	$8, %rdx
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB14_11:                              # %lor.lhs.false
                                        #   Parent Loop BB14_5 Depth=1
                                        #     Parent Loop BB14_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	regs_invalidated_by_call(%rip), %rsi
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	testq	%rdi, %rsi
	je	.LBB14_14
# BB#12:                                # %if.then35
                                        #   in Loop: Header=BB14_11 Depth=3
	movslq	8(%r14), %rcx
	movl	(%r9,%rcx,4), %ecx
	movl	%ecx, (%rdx)
	movl	-8(%rdx), %eax
	movl	current_bb(%rip), %esi
	cmpq	%rsi, %rax
	je	.LBB14_14
# BB#13:                                # %if.then.i
                                        #   in Loop: Header=BB14_11 Depth=3
	movl	%esi, -8(%rdx)
	movl	%ecx, -4(%rdx)
	movslq	current_bb(%rip), %rcx
	movq	(%r8,%rcx,8), %rsi
	movq	$3, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	andq	$536870904, %rax        # imm = 0x1FFFFFF8
	orq	%rdi, 16(%rsi,%rax)
.LBB14_14:                              # %for.inc37
                                        #   in Loop: Header=BB14_11 Depth=3
	incq	%rbx
	addq	$12, %rdx
	cmpq	$53, %rbx
	jne	.LBB14_11
# BB#15:                                # %for.end39
                                        #   in Loop: Header=BB14_6 Depth=2
	movzbl	3(%r14), %eax
	testq	$4, %rax
	jne	.LBB14_17
# BB#16:                                # %if.then.i130
                                        #   in Loop: Header=BB14_6 Depth=2
	movq	%r14, %rdi
	callq	record_last_mem_set_info
.LBB14_17:                              # %if.end40
                                        #   in Loop: Header=BB14_6 Depth=2
	movq	32(%r14), %rdi
	movabsq	$record_last_set_info, %rsi
	movq	%r14, %rdx
	callq	note_stores
	jmp	.LBB14_18
	.align	16, 0x90
.LBB14_19:                              # %for.end48
                                        #   in Loop: Header=BB14_5 Depth=1
	movq	(%rax), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB14_39
	jmp	.LBB14_20
	.align	16, 0x90
.LBB14_38:                              # %for.inc93.land.rhs56_crit_edge
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rdx,8), %rax
.LBB14_20:                              # %land.rhs56
                                        #   Parent Loop BB14_5 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB14_30 Depth 3
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB14_39
# BB#21:                                # %for.body68
                                        #   in Loop: Header=BB14_20 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB14_37
# BB#22:                                # %if.then76
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	$1, %r12
	jne	.LBB14_34
# BB#23:                                # %if.else
                                        #   in Loop: Header=BB14_20 Depth=2
	testq	%r13, %r15
	jne	.LBB14_24
# BB#26:                                # %if.end85
                                        #   in Loop: Header=BB14_20 Depth=2
	testq	%r13, %r14
	jne	.LBB14_35
	jmp	.LBB14_27
.LBB14_24:                              # %land.lhs.true
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB14_27
# BB#25:                                # %land.lhs.true
                                        #   in Loop: Header=BB14_20 Depth=2
	testq	%r13, %r14
	jne	.LBB14_37
.LBB14_27:                              # %if.end.i
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	32(%rbx), %r14
	movzbl	(%r14), %eax
	xorq	%r12, %r12
	cmpq	$47, %rax
	jne	.LBB14_28
# BB#42:                                # %if.then1.i
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	hash_scan_set
	jmp	.LBB14_34
.LBB14_28:                              # %if.end.i
                                        #   in Loop: Header=BB14_20 Depth=2
	cmpq	$39, %rax
	jne	.LBB14_34
# BB#29:                                # %for.cond.preheader.i
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r13
	jle	.LBB14_33
	.align	16, 0x90
.LBB14_30:                              # %for.body.i
                                        #   Parent Loop BB14_5 Depth=1
                                        #     Parent Loop BB14_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rax,%r13,8), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$47, %rcx
	jne	.LBB14_32
# BB#31:                                # %if.then16.i
                                        #   in Loop: Header=BB14_30 Depth=3
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	hash_scan_set
	movq	8(%r14), %rax
.LBB14_32:                              # %for.inc.i
                                        #   in Loop: Header=BB14_30 Depth=3
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	cmpq	%rdx, %rsi
	jl	.LBB14_30
.LBB14_33:                              #   in Loop: Header=BB14_20 Depth=2
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_34:                              # %hash_scan_insn.exit
                                        #   in Loop: Header=BB14_20 Depth=2
	testq	%r13, %r15
	movq	%r12, %r14
	jne	.LBB14_37
.LBB14_35:                              # %land.lhs.true87
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB14_37
# BB#36:                                # %land.lhs.true87
                                        #   in Loop: Header=BB14_20 Depth=2
	xorq	%r14, %r14
	.align	16, 0x90
.LBB14_37:                              # %for.inc93
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	24(%rbx), %rbx
	movslq	current_bb(%rip), %rdx
	testq	%rbx, %rbx
	jne	.LBB14_38
.LBB14_39:                              # %for.inc98
                                        #   in Loop: Header=BB14_5 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movslq	n_basic_blocks(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%edx, current_bb(%rip)
	cmpq	%rsi, %rax
	jl	.LBB14_5
# BB#40:                                # %for.cond3.for.end100_crit_edge
	movq	reg_avail_info(%rip), %rax
.LBB14_41:                              # %for.end100
	movq	%rax, %rdi
	callq	free
	movq	$0, reg_avail_info(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp223:
	.size	compute_hash_table, .Ltmp223-compute_hash_table
	.cfi_endproc

	.align	16, 0x90
	.type	clear_modify_mem_tables,@function
clear_modify_mem_tables:                # @clear_modify_mem_tables
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp230:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp231:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp232:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp233:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp234:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp235:
	.cfi_def_cfa_offset 64
.Ltmp236:
	.cfi_offset %rbx, -48
.Ltmp237:
	.cfi_offset %r12, -40
.Ltmp238:
	.cfi_offset %r13, -32
.Ltmp239:
	.cfi_offset %r14, -24
.Ltmp240:
	.cfi_offset %r15, -16
	movq	modify_mem_list_set(%rip), %rdi
	movq	(%rdi), %r14
	testq	%r14, %r14
	je	.LBB15_12
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB15_2:                               # %for.cond6.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_3 Depth 2
                                        #     Child Loop BB15_7 Depth 2
	movq	24(%r14), %r12
	testq	%r12, %r12
	movq	$1, %r13
	je	.LBB15_6
	.align	16, 0x90
.LBB15_3:                               # %for.body13
                                        #   Parent Loop BB15_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r13), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	%r12, %rbx
	je	.LBB15_5
# BB#4:                                 # %if.then15
                                        #   in Loop: Header=BB15_3 Depth=2
	notq	%rbx
	movl	16(%r14), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	modify_mem_list(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rdx,%rax,8), %rdi
	callq	free_INSN_LIST_list
	andq	%rbx, %r12
	je	.LBB15_6
.LBB15_5:                               # %for.inc
                                        #   in Loop: Header=BB15_3 Depth=2
	movq	%r13, %rax
	andq	%r15, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB15_3
.LBB15_6:                               # %if.end24
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	32(%r14), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB15_10
	.align	16, 0x90
.LBB15_7:                               # %for.body13.1
                                        #   Parent Loop BB15_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %r13
	movq	%rbx, %rcx
	shlq	%cl, %r13
	testq	%r12, %r13
	je	.LBB15_9
# BB#8:                                 # %if.then15.1
                                        #   in Loop: Header=BB15_7 Depth=2
	notq	%r13
	movl	16(%r14), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%rbx,%rax), %rax
	movq	modify_mem_list(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rdx,%rax,8), %rdi
	callq	free_INSN_LIST_list
	andq	%r13, %r12
	je	.LBB15_10
.LBB15_9:                               # %for.inc.1
                                        #   in Loop: Header=BB15_7 Depth=2
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB15_7
.LBB15_10:                              # %if.end24.1
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB15_2
# BB#11:                                # %for.cond.do.end_crit_edge
	movq	modify_mem_list_set(%rip), %rdi
.LBB15_12:                              # %do.end
	callq	bitmap_clear
	movq	canon_modify_mem_list_set(%rip), %rdi
	movq	(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB15_27
	.align	16, 0x90
.LBB15_13:                              # %for.cond55.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_14 Depth 2
                                        #       Child Loop BB15_16 Depth 3
                                        #     Child Loop BB15_22 Depth 2
                                        #       Child Loop BB15_24 Depth 3
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	24(%rdx), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB15_21
	.align	16, 0x90
.LBB15_14:                              # %for.body66
                                        #   Parent Loop BB15_13 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB15_16 Depth 3
	movq	$1, %r13
	movq	%rbx, %rcx
	shlq	%cl, %r13
	testq	%r12, %r13
	je	.LBB15_20
# BB#15:                                # %if.then72
                                        #   in Loop: Header=BB15_14 Depth=2
	movl	16(%rdx), %r15d
	movq	$7, %rcx
	shlq	%cl, %r15
	addq	%rbx, %r15
	movq	canon_modify_mem_list(%rip), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	(%rax,%r15,8), %rdi
	testq	%rdi, %rdi
	je	.LBB15_19
	.align	16, 0x90
.LBB15_16:                              # %for.body.i
                                        #   Parent Loop BB15_13 Depth=1
                                        #     Parent Loop BB15_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rdi), %eax
	movq	16(%rdi), %r14
	cmpq	$3, %rax
	jne	.LBB15_33
# BB#17:                                # %if.then.i
                                        #   in Loop: Header=BB15_16 Depth=3
	callq	free_EXPR_LIST_node
	jmp	.LBB15_18
	.align	16, 0x90
.LBB15_33:                              # %if.else.i
                                        #   in Loop: Header=BB15_16 Depth=3
	callq	free_INSN_LIST_node
.LBB15_18:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB15_16 Depth=3
	testq	%r14, %r14
	movq	%r14, %rdi
	jne	.LBB15_16
.LBB15_19:                              # %free_insn_expr_list_list.exit
                                        #   in Loop: Header=BB15_14 Depth=2
	notq	%r13
	andq	%r13, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movq	$0, (%rax,%r15,8)
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB15_21
.LBB15_20:                              # %for.inc86
                                        #   in Loop: Header=BB15_14 Depth=2
	incq	%rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB15_14
.LBB15_21:                              # %if.end89
                                        #   in Loop: Header=BB15_13 Depth=1
	movq	32(%rdx), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB15_32
	.align	16, 0x90
.LBB15_22:                              # %for.body66.1
                                        #   Parent Loop BB15_13 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB15_24 Depth 3
	movq	$1, %r13
	movq	%rbx, %rcx
	shlq	%cl, %r13
	testq	%r12, %r13
	je	.LBB15_31
# BB#23:                                # %if.then72.1
                                        #   in Loop: Header=BB15_22 Depth=2
	movl	16(%rdx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%rbx,%rax), %r15
	movq	canon_modify_mem_list(%rip), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	(%rax,%r15,8), %rdi
	testq	%rdi, %rdi
	je	.LBB15_30
	.align	16, 0x90
.LBB15_24:                              # %for.body.i.1
                                        #   Parent Loop BB15_13 Depth=1
                                        #     Parent Loop BB15_22 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rdi), %eax
	movq	16(%rdi), %r14
	cmpq	$3, %rax
	jne	.LBB15_25
# BB#28:                                # %if.then.i.1
                                        #   in Loop: Header=BB15_24 Depth=3
	callq	free_EXPR_LIST_node
	jmp	.LBB15_29
	.align	16, 0x90
.LBB15_25:                              # %if.else.i.1
                                        #   in Loop: Header=BB15_24 Depth=3
	callq	free_INSN_LIST_node
.LBB15_29:                              # %for.cond.backedge.i.1
                                        #   in Loop: Header=BB15_24 Depth=3
	testq	%r14, %r14
	movq	%r14, %rdi
	jne	.LBB15_24
.LBB15_30:                              # %free_insn_expr_list_list.exit.1
                                        #   in Loop: Header=BB15_22 Depth=2
	notq	%r13
	andq	%r13, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movq	$0, (%rax,%r15,8)
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB15_32
.LBB15_31:                              # %for.inc86.1
                                        #   in Loop: Header=BB15_22 Depth=2
	incq	%rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB15_22
.LBB15_32:                              # %if.end89.1
                                        #   in Loop: Header=BB15_13 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB15_13
# BB#26:                                # %for.cond52.do.end96_crit_edge
	movq	canon_modify_mem_list_set(%rip), %rdi
.LBB15_27:                              # %do.end96
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	bitmap_clear  # TAILCALL
.Ltmp241:
	.size	clear_modify_mem_tables, .Ltmp241-clear_modify_mem_tables
	.cfi_endproc

	.align	16, 0x90
	.type	record_last_set_info,@function
record_last_set_info:                   # @record_last_set_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 16
.Ltmp245:
	.cfi_offset %rbx, -16
	movl	(%rdi), %esi
	movq	%rdx, %rbx
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB16_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movl	(%rdi), %esi
.LBB16_2:                               # %if.end
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB16_3
# BB#6:                                 # %land.lhs.true
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	push_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB16_7
# BB#8:                                 # %if.then12
	movq	%rbx, %rdi
	popq	%rbx
	jmp	record_last_mem_set_info  # TAILCALL
.LBB16_3:                               # %if.end
	cmpq	$61, %rax
	jne	.LBB16_7
# BB#4:                                 # %if.then4
	movslq	8(%rbx), %rdx
	movq	uid_cuid(%rip), %rsi
	movslq	8(%rdi), %rcx
	movq	reg_avail_info(%rip), %rax
	movl	(%rsi,%rdx,4), %edx
	leaq	(%rcx,%rcx,2), %rsi
	movl	%edx, 8(%rax,%rsi,4)
	movl	(%rax,%rsi,4), %ebx
	movl	current_bb(%rip), %edi
	cmpq	%rdi, %rbx
	jne	.LBB16_5
.LBB16_7:                               # %if.end14
	popq	%rbx
	retq
.LBB16_5:                               # %if.then.i
	leaq	(%rax,%rsi,4), %rsi
	movl	%edi, (%rsi)
	leaq	(%rcx,%rcx,2), %rsi
	movl	%edx, 4(%rax,%rsi,4)
	movslq	current_bb(%rip), %rax
	movq	reg_set_in_block(%rip), %rdx
	movabsq	$4294967232, %rsi       # imm = 0xFFFFFFC0
	andq	%rcx, %rsi
	movq	$1, %rdi
	movq	(%rdx,%rax,8), %rax
	shlq	%cl, %rdi
	movq	$3, %rcx
	shrq	%cl, %rsi
	orq	%rdi, 16(%rax,%rsi)
	popq	%rbx
	retq
.Ltmp246:
	.size	record_last_set_info, .Ltmp246-record_last_set_info
	.cfi_endproc

	.align	16, 0x90
	.type	hash_scan_set,@function
hash_scan_set:                          # @hash_scan_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp253:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp254:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp255:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp256:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp257:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp258:
	.cfi_def_cfa_offset 80
.Ltmp259:
	.cfi_offset %rbx, -48
.Ltmp260:
	.cfi_offset %r12, -40
.Ltmp261:
	.cfi_offset %r13, -32
.Ltmp262:
	.cfi_offset %r14, -24
.Ltmp263:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	16(%r13), %r15
	movzbl	(%r15), %eax
	movq	%rdx, %r12
	cmpq	$50, %rax
	je	.LBB17_129
# BB#1:                                 # %if.else
	movq	8(%r13), %r9
	movzbl	(%r9), %eax
	cmpq	$61, %rax
	jne	.LBB17_129
# BB#2:                                 # %if.then7
	movl	8(%r9), %ebx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %r12
	je	.LBB17_3
# BB#4:                                 # %land.lhs.true
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r9, %r15
	movq	%r8, %r14
	movq	%rsi, %rdi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	callq	find_reg_equal_equiv_note
	testq	%rax, %rax
	je	.LBB17_5
# BB#6:                                 # %land.lhs.true11
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	movq	%r14, %r8
	jg	.LBB17_9
# BB#7:                                 # %land.lhs.true11
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	movq	%r15, %r9
	ja	.LBB17_12
# BB#8:                                 # %land.lhs.true11
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	movq	8(%rsp), %r15           # 8-byte Reload
	jne	.LBB17_11
	jmp	.LBB17_13
.LBB17_3:
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	jmp	.LBB17_13
.LBB17_5:
	movq	%r14, %r8
	movq	%r15, %r9
	jmp	.LBB17_12
.LBB17_9:                               # %land.lhs.true11
	cmpq	$134, %rcx
	movq	%r15, %r9
	movq	8(%rsp), %r15           # 8-byte Reload
	je	.LBB17_11
# BB#10:                                # %land.lhs.true11
	cmpq	$140, %rcx
	jne	.LBB17_13
.LBB17_11:                              # %if.then66
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r9, %rdx
	movq	%rax, %rcx
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%rax, 8(%rsp)           # 8-byte Spill
	callq	gen_rtx_fmt_ee
	movq	%r15, %r9
	movq	%r14, %r8
	movq	%rax, %r13
.LBB17_12:                              # %if.end
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB17_13:                              # %if.end
	testq	%r8, %r12
	jne	.LBB17_80
# BB#14:                                # %if.end
	movq	%rbx, %rax
	andq	%r8, %rax
	cmpq	$53, %rax
	jb	.LBB17_80
# BB#15:                                # %land.lhs.true74
	movzbl	2(%r9), %eax
	movzbl	can_copy_p(%rax), %eax
	testq	%rax, %rax
	je	.LBB17_80
# BB#16:                                # %land.lhs.true79
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %r14
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r9, %r15
	callq	can_throw_internal
	movq	%r15, %r9
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	%r14, %r8
	testq	%rax, %rax
	jne	.LBB17_80
# BB#17:                                # %land.lhs.true81
	movq	%r15, %rdi
	movq	%r8, %r14
	movq	%r9, %r15
	callq	want_to_gcse_p
	movq	%r15, %r9
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	%r14, %r8
	testq	%r8, %rax
	je	.LBB17_80
# BB#18:                                # %land.lhs.true84
	movq	%r13, %rdi
	movq	%r8, %r14
	movq	%r9, %r15
	callq	set_noop_p
	movq	%r15, %r9
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	%r14, %r8
	testq	%r8, %rax
	jne	.LBB17_80
# BB#19:                                # %land.lhs.true87
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	$0, (%rsp)              # 8-byte Folded Spill
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %r14
	movq	%r9, %r15
	callq	find_reg_note
	movq	%r15, %r9
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	%r14, %r8
	testq	%rax, %rax
	je	.LBB17_21
# BB#20:                                # %lor.lhs.false91
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB17_21
.LBB17_80:                              # %if.else142
	testq	%r8, %r12
	movq	16(%rsp), %rdi          # 8-byte Reload
	je	.LBB17_129
# BB#81:                                # %if.else142
	andq	%r8, %rbx
	cmpq	$53, %rbx
	jb	.LBB17_129
# BB#82:                                # %land.lhs.true147
	movzbl	(%r15), %eax
	cmpq	$133, %rax
	jg	.LBB17_92
# BB#83:                                # %land.lhs.true147
	leaq	-54(%rax), %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_129
# BB#84:                                # %land.lhs.true147
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB17_94
# BB#85:                                # %land.lhs.true147
	cmpq	$7, %rcx
	jne	.LBB17_129
# BB#86:                                # %land.lhs.true152
	movl	8(%r15), %edx
	cmpq	$53, %rdx
	jb	.LBB17_129
# BB#87:                                # %land.lhs.true158
	movzbl	2(%r9), %esi
	movzbl	can_copy_p(%rsi), %esi
	testq	%rsi, %rsi
	je	.LBB17_89
# BB#88:                                # %land.lhs.true158
	cmpq	%rbx, %rdx
	jne	.LBB17_94
.LBB17_89:                              # %lor.lhs.false172
	cmpq	$133, %rax
	jg	.LBB17_92
# BB#90:                                # %lor.lhs.false172
	cmpq	$14, %rcx
	ja	.LBB17_129
# BB#91:                                # %lor.lhs.false172
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB17_94
	jmp	.LBB17_129
.LBB17_92:                              # %lor.lhs.false172
	cmpq	$134, %rax
	je	.LBB17_94
# BB#93:                                # %lor.lhs.false172
	cmpq	$140, %rax
	jne	.LBB17_129
.LBB17_94:                              # %land.lhs.true212
	movslq	8(%rdi), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movslq	88(%rax), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	8(%rax), %rax
	cmpq	%rdi, %rax
	je	.LBB17_97
# BB#95:                                # %lor.lhs.false223
	movq	%r8, %rbx
	callq	next_nonnote_insn
	movq	%rbx, %rcx
	testq	%rax, %rax
	je	.LBB17_129
# BB#96:                                # %land.lhs.true227
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rbx
	callq	oprs_unchanged_p
	movq	%rbx, %r8
	testq	%r8, %rax
	je	.LBB17_129
.LBB17_97:                              # %if.then230
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	jne	.LBB17_130
# BB#98:                                # %lor.lhs.false.i
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB17_130
# BB#99:                                # %if.end.i
	movq	%r8, %r12
	movl	8(%rax), %eax
	movl	set_hash_table_size(%rip), %ecx
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rdx, %r14
	movq	set_hash_table(%rip), %rax
	movq	(%rax,%r14,8), %rax
	xorq	%r15, %r15
	jmp	.LBB17_100
.LBB17_110:                             # %while.cond.i
                                        #   in Loop: Header=BB17_100 Depth=1
	movq	%rsi, %r12
	movq	16(%r15), %rax
.LBB17_100:                             # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB17_101
# BB#109:                               # %land.rhs.i
                                        #   in Loop: Header=BB17_100 Depth=1
	movq	%rax, %r15
	movq	(%r15), %rdi
	movq	%r13, %rsi
	callq	expr_equiv_p
	movq	%r12, %rsi
	testq	%rsi, %rax
	je	.LBB17_110
	jmp	.LBB17_113
.LBB17_101:                             # %if.then13.i
	movq	gcse_obstack+32(%rip), %rcx
	movq	gcse_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$47, %rcx
	jg	.LBB17_103
# BB#102:                               # %if.then.i106.i
	movabsq	$gcse_obstack, %rdi
	movq	$48, %rsi
	callq	_obstack_newchunk
	movq	gcse_obstack+24(%rip), %rax
.LBB17_103:                             # %if.end.i109.i
	addq	$48, %rax
	movq	%rax, gcse_obstack+24(%rip)
	movq	gcse_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB17_105
# BB#104:                               # %if.then9.i112.i
	movzbl	gcse_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, gcse_obstack+80(%rip)
.LBB17_105:                             # %if.end10.i126.i
	movslq	gcse_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, gcse_obstack+24(%rip)
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB17_107
# BB#106:                               # %if.then30.i127.i
	movq	%rax, gcse_obstack+24(%rip)
	movq	%rax, %rcx
.LBB17_107:                             # %gcse_alloc.exit128.i
	movl	bytes_used(%rip), %edx
	movq	set_hash_table(%rip), %rax
	movq	%rcx, gcse_obstack+16(%rip)
	addq	$48, %rdx
	movl	%edx, bytes_used(%rip)
	cmpq	$0, (%rax,%r14,8)
	je	.LBB17_108
# BB#111:                               # %if.else.i
	movq	%rbx, 16(%r15)
	jmp	.LBB17_112
.LBB17_108:                             # %if.then20.i
	movq	%rbx, (%rax,%r14,8)
.LBB17_112:                             # %if.end24.i
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, (%rbx)
	movl	n_sets(%rip), %eax
	leaq	1(%rax), %rcx
	movl	%ecx, n_sets(%rip)
	movl	%eax, 8(%rbx)
	movq	$0, 32(%rbx)
	movq	$0, 24(%rbx)
	movq	$0, 16(%rbx)
	movq	%rbx, %r15
	movq	%r12, %rsi
.LBB17_113:                             # %if.end28.i
	movq	32(%r15), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	movq	16(%rsp), %r12          # 8-byte Reload
	je	.LBB17_118
# BB#114:                               # %land.rhs32.lr.ph.i
	movq	basic_block_for_insn(%rip), %rax
	movslq	8(%r12), %rdx
	movq	32(%rax,%rdx,8), %rdx
	movl	88(%rdx), %edx
	andq	%rsi, %rdx
	.align	16, 0x90
.LBB17_116:                             # %land.rhs32.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movq	8(%rbx), %rcx
	movslq	8(%rcx), %rcx
	movq	32(%rax,%rcx,8), %rcx
	movl	88(%rcx), %ecx
	cmpq	%rdx, %rcx
	je	.LBB17_117
# BB#115:                               # %while.cond30.i
                                        #   in Loop: Header=BB17_116 Depth=1
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB17_116
.LBB17_118:                             # %if.else56.i
	movq	%r12, %r14
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB17_120
# BB#119:                               # %if.then.i.i
	movabsq	$gcse_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	gcse_obstack+24(%rip), %rcx
.LBB17_120:                             # %if.end.i.i
	addq	$24, %rcx
	movq	%rcx, gcse_obstack+24(%rip)
	movq	gcse_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB17_122
# BB#121:                               # %if.then9.i.i
	movzbl	gcse_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, gcse_obstack+80(%rip)
.LBB17_122:                             # %if.end10.i.i
	movslq	gcse_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, gcse_obstack+24(%rip)
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rax, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB17_124
# BB#123:                               # %if.then30.i.i
	movq	%rax, gcse_obstack+24(%rip)
	movq	%rax, %rdx
.LBB17_124:                             # %gcse_alloc.exit.i
	movl	bytes_used(%rip), %eax
	movq	%rdx, gcse_obstack+16(%rip)
	addq	$24, %rax
	movl	%eax, bytes_used(%rip)
	cmpq	$0, 32(%r15)
	je	.LBB17_125
# BB#126:                               # %if.else66.i
	movq	%r8, (%rbx)
	jmp	.LBB17_127
.LBB17_117:                             # %if.then54.i
	movq	%r12, 8(%rbx)
	jmp	.LBB17_129
.LBB17_125:                             # %if.then64.i
	movq	%r8, 32(%r15)
.LBB17_127:                             # %if.end68.i
	movq	%r14, 8(%r8)
.LBB17_128:                             # %if.end234
	movq	$0, (%r8)
.LBB17_129:                             # %if.end234
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_21:                              # %if.then99
	movq	%r9, %r12
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r8, %r14
	callq	oprs_unchanged_p
	movq	%r14, %r8
	movq	%rbx, %rsi
	testq	%r8, %rax
	je	.LBB17_29
# BB#22:                                # %land.rhs
	movzbl	(%rsi), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB17_25
# BB#23:                                # %cond.true
	movq	32(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB17_25
# BB#24:                                # %cond.false
	movq	%rsi, %rdi
	movq	%rsi, %r14
	movq	%rax, %rsi
	movq	%r8, %rbx
	callq	single_set_2
	movq	%rbx, %r8
	movq	%r14, %rsi
.LBB17_25:                              # %cond.end126
	testq	%rax, %rax
	je	.LBB17_26
# BB#27:                                # %cond.end126
	movabsq	$1, %rax
	jmp	.LBB17_28
.LBB17_26:
	movabsq	$0, %rax
.LBB17_28:                              # %cond.end126
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB17_29:                              # %land.end
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rsi, %rbx
	movq	%r8, %r14
	callq	oprs_unchanged_p
	xorq	%rcx, %rcx
	testq	%r14, %rax
	movabsq	$0, %r13
	je	.LBB17_33
# BB#30:                                # %land.rhs132
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	je	.LBB17_31
# BB#32:                                # %land.rhs132
	movabsq	$1, %r13
	jmp	.LBB17_33
.LBB17_31:
	movabsq	$0, %r13
.LBB17_33:                              # %land.end137
	movzbl	2(%r12), %esi
	movl	expr_hash_table_size(%rip), %r12d
	movl	%ecx, 28(%rsp)
	leaq	28(%rsp), %rdx
	movq	%r15, %rdi
	callq	hash_expr_1
	movl	28(%rsp), %ecx
	testq	%rcx, %rcx
	jne	.LBB17_129
# BB#34:                                # %if.end.i297
	andq	%r14, %rax
	andq	%r14, %r12
	movq	%r14, %rbx
	xorq	%rdx, %rdx
	divq	%r12
	movq	%rdx, %r12
	movq	expr_hash_table(%rip), %rax
	movq	(%rax,%r12,8), %rax
	xorq	%r14, %r14
	jmp	.LBB17_35
.LBB17_44:                              # %while.cond.i300
                                        #   in Loop: Header=BB17_35 Depth=1
	movq	16(%r14), %rax
.LBB17_35:                              # %if.end.i297
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB17_36
# BB#45:                                # %land.rhs.i303
                                        #   in Loop: Header=BB17_35 Depth=1
	movq	%rax, %r14
	movq	(%r14), %rdi
	movq	%r15, %rsi
	callq	expr_equiv_p
	testq	%rbx, %rax
	je	.LBB17_44
# BB#46:
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB17_49
.LBB17_36:                              # %if.then4.i
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$47, %rax
	jg	.LBB17_38
# BB#37:                                # %if.then.i.i310
	movabsq	$gcse_obstack, %rdi
	movq	$48, %rsi
	callq	_obstack_newchunk
	movq	gcse_obstack+24(%rip), %rcx
.LBB17_38:                              # %if.end.i.i313
	addq	$48, %rcx
	movq	%rcx, gcse_obstack+24(%rip)
	movq	gcse_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB17_40
# BB#39:                                # %if.then9.i.i316
	movzbl	gcse_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, gcse_obstack+80(%rip)
.LBB17_40:                              # %if.end10.i.i330
	movslq	gcse_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, gcse_obstack+24(%rip)
	movq	gcse_obstack+32(%rip), %r8
	movq	gcse_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB17_42
# BB#41:                                # %if.then30.i.i331
	movq	%r8, gcse_obstack+24(%rip)
	movq	%r8, %rdx
.LBB17_42:                              # %gcse_alloc.exit.i334
	movl	bytes_used(%rip), %esi
	movq	expr_hash_table(%rip), %rcx
	movq	%rdx, gcse_obstack+16(%rip)
	addq	$48, %rsi
	movl	%esi, bytes_used(%rip)
	cmpq	$0, (%rcx,%r12,8)
	je	.LBB17_43
# BB#47:                                # %if.else.i335
	movq	%rax, 16(%r14)
	jmp	.LBB17_48
.LBB17_43:                              # %if.then11.i
	movq	%rax, (%rcx,%r12,8)
.LBB17_48:                              # %if.end15.i
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, (%rax)
	movl	n_exprs(%rip), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, n_exprs(%rip)
	movl	%ecx, 8(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	%rax, %r14
.LBB17_49:                              # %if.end20.i
	xorq	%r12, %r12
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	je	.LBB17_64
# BB#50:                                # %if.then22.i
	movq	24(%r14), %rdx
	xorq	%r15, %r15
	testq	%rdx, %rdx
	je	.LBB17_54
# BB#51:                                # %land.rhs26.lr.ph.i
	movq	basic_block_for_insn(%rip), %rax
	movslq	8(%rsi), %rcx
	movq	32(%rax,%rcx,8), %rcx
	movl	88(%rcx), %ecx
	xorq	%r15, %r15
	andq	%rbx, %rcx
.LBB17_53:                              # %land.rhs26.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %r12
	movq	%rdx, %r15
	movq	8(%r15), %rdx
	movslq	8(%rdx), %rdx
	movq	32(%rax,%rdx,8), %rdx
	movl	88(%rdx), %edx
	cmpq	%rcx, %rdx
	je	.LBB17_64
# BB#52:                                # %while.cond24.i
                                        #   in Loop: Header=BB17_53 Depth=1
	movq	(%r15), %rdx
	testq	%rdx, %rdx
	jne	.LBB17_53
.LBB17_54:                              # %if.else48.i
	movq	%rsi, %r12
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB17_56
# BB#55:                                # %if.then.i161.i
	movabsq	$gcse_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	gcse_obstack+24(%rip), %rcx
.LBB17_56:                              # %if.end.i164.i
	addq	$24, %rcx
	movq	%rcx, gcse_obstack+24(%rip)
	movq	gcse_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB17_58
# BB#57:                                # %if.then9.i167.i
	movzbl	gcse_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, gcse_obstack+80(%rip)
.LBB17_58:                              # %if.end10.i181.i
	movslq	gcse_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, gcse_obstack+24(%rip)
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rax, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB17_60
# BB#59:                                # %if.then30.i182.i
	movq	%rax, gcse_obstack+24(%rip)
	movq	%rax, %rdx
.LBB17_60:                              # %gcse_alloc.exit183.i
	movl	bytes_used(%rip), %ecx
	movq	%rdx, gcse_obstack+16(%rip)
	addq	$24, %rcx
	movl	%ecx, bytes_used(%rip)
	cmpq	$0, 24(%r14)
	movq	%r12, %rsi
	je	.LBB17_61
# BB#62:                                # %if.else58.i
	movq	%r8, (%r15)
	jmp	.LBB17_63
.LBB17_61:                              # %if.then56.i
	movq	%r8, 24(%r14)
.LBB17_63:                              # %if.end60.i
	movq	%rsi, 8(%r8)
	movq	$0, (%r8)
	movq	%r15, %r12
.LBB17_64:                              # %if.end64.i
	testq	%r13, %r13
	je	.LBB17_129
# BB#65:                                # %if.then66.i
	movq	32(%r14), %rdx
	testq	%rdx, %rdx
	je	.LBB17_70
# BB#66:                                # %land.rhs70.lr.ph.i
	movq	basic_block_for_insn(%rip), %rax
	movslq	8(%rsi), %rcx
	movq	32(%rax,%rcx,8), %rcx
	movl	88(%rcx), %ecx
	andq	%rbx, %rcx
.LBB17_68:                              # %land.rhs70.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r12
	movq	8(%r12), %rdx
	movslq	8(%rdx), %rdx
	movq	32(%rax,%rdx,8), %rdx
	movl	88(%rdx), %edx
	cmpq	%rcx, %rdx
	je	.LBB17_69
# BB#67:                                # %while.cond68.i
                                        #   in Loop: Header=BB17_68 Depth=1
	movq	(%r12), %rdx
	testq	%rdx, %rdx
	jne	.LBB17_68
.LBB17_70:                              # %if.else99.i
	movq	%rsi, %r15
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB17_72
# BB#71:                                # %if.then.i189.i
	movabsq	$gcse_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	gcse_obstack+24(%rip), %rcx
.LBB17_72:                              # %if.end.i192.i
	addq	$24, %rcx
	movq	%rcx, gcse_obstack+24(%rip)
	movq	gcse_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB17_74
# BB#73:                                # %if.then9.i195.i
	movzbl	gcse_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, gcse_obstack+80(%rip)
.LBB17_74:                              # %if.end10.i209.i
	movslq	gcse_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, gcse_obstack+24(%rip)
	movq	gcse_obstack+32(%rip), %rax
	movq	gcse_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rax, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB17_76
# BB#75:                                # %if.then30.i210.i
	movq	%rax, gcse_obstack+24(%rip)
	movq	%rax, %rdx
.LBB17_76:                              # %gcse_alloc.exit211.i
	movl	bytes_used(%rip), %eax
	movq	%rdx, gcse_obstack+16(%rip)
	addq	$24, %rax
	movl	%eax, bytes_used(%rip)
	cmpq	$0, 32(%r14)
	je	.LBB17_77
# BB#78:                                # %if.else109.i
	movq	%r8, (%r12)
	movq	%r15, 8(%r8)
	jmp	.LBB17_128
.LBB17_69:                              # %if.then97.i
	movq	%rsi, 8(%r12)
	jmp	.LBB17_129
.LBB17_77:                              # %if.then107.i
	movq	%r8, 32(%r14)
	movq	%r15, 8(%r8)
	jmp	.LBB17_128
.LBB17_130:                             # %if.then.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.insert_set_in_table, %rcx
	movq	$2093, %rsi             # imm = 0x82D
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp264:
	.size	hash_scan_set, .Ltmp264-hash_scan_set
	.cfi_endproc

	.align	16, 0x90
	.type	want_to_gcse_p,@function
want_to_gcse_p:                         # @want_to_gcse_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp268:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp269:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp270:
	.cfi_def_cfa_offset 32
.Ltmp271:
	.cfi_offset %rbx, -24
.Ltmp272:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%rax, %rax
	movl	%eax, 4(%rsp)
	movl	(%rbx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	ja	.LBB18_2
# BB#1:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$-6790302338167865344, %rcx # imm = 0xA1C4000000000000
	testq	%rcx, %rdx
	jne	.LBB18_11
.LBB18_2:                               # %sw.epilog
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%rbx, %rdi
	callq	general_operand
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB18_11
# BB#3:                                 # %if.else
	movl	(%rbx), %edx
	testq	$16711680, %rdx         # imm = 0xFF0000
	movabsq	$0, %rax
	je	.LBB18_11
# BB#4:                                 # %if.end7
	movq	want_to_gcse_p.test_insn(%rip), %rax
	testq	%rax, %rax
	jne	.LBB18_6
# BB#5:                                 # %if.then9
	movl	word_mode(%rip), %edi
	movq	$106, %rsi
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rcx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	make_insn_raw
	movq	%rax, want_to_gcse_p.test_insn(%rip)
	movq	$0, 16(%rax)
	movq	want_to_gcse_p.test_insn(%rip), %rax
	movabsq	$want_to_gcse_p.test_insn, %rdi
	movq	$0, 24(%rax)
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movl	(%rbx), %edx
	movq	want_to_gcse_p.test_insn(%rip), %rax
.LBB18_6:                               # %if.end16
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 2(%rax)
	movq	want_to_gcse_p.test_insn(%rip), %rax
	movq	32(%rax), %rax
	movq	%rbx, 16(%rax)
	movq	want_to_gcse_p.test_insn(%rip), %rsi
	movq	32(%rsi), %rdi
	leaq	4(%rsp), %rdx
	callq	recog
	movq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	js	.LBB18_11
# BB#7:                                 # %land.rhs
	movl	4(%rsp), %eax
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB18_11
# BB#8:                                 # %lor.rhs
	movq	%rdx, %rdi
	callq	added_clobbers_hard_reg_p
	testq	%r14, %rax
	jne	.LBB18_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB18_11
.LBB18_9:
	movabsq	$0, %rax
.LBB18_11:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp273:
	.size	want_to_gcse_p, .Ltmp273-want_to_gcse_p
	.cfi_endproc

	.align	16, 0x90
	.type	expr_equiv_p,@function
expr_equiv_p:                           # @expr_equiv_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp280:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp281:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp282:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp283:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp284:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp285:
	.cfi_def_cfa_offset 64
.Ltmp286:
	.cfi_offset %rbx, -48
.Ltmp287:
	.cfi_offset %r12, -40
.Ltmp288:
	.cfi_offset %r13, -32
.Ltmp289:
	.cfi_offset %r14, -24
.Ltmp290:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	cmpq	%r14, %rbx
	je	.LBB19_1
# BB#2:                                 # %if.end
	testq	%rbx, %rbx
	je	.LBB19_72
# BB#3:                                 # %if.end
	testq	%r14, %r14
	je	.LBB19_72
# BB#4:                                 # %if.end5
	movl	(%rbx), %eax
	movl	(%r14), %edx
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	%rsi, %rcx
	jne	.LBB19_72
# BB#5:                                 # %if.end11
	movq	%rdx, %rsi
	xorq	%rax, %rsi
	testq	$16711680, %rsi         # imm = 0xFF0000
	jne	.LBB19_72
# BB#6:                                 # %if.end20
	cmpq	$101, %rcx
	jg	.LBB19_39
# BB#7:                                 # %if.end20
	cmpq	$53, %rcx
	jg	.LBB19_16
# BB#8:                                 # %if.end20
	cmpq	$41, %rcx
	jne	.LBB19_37
# BB#9:                                 # %sw.bb109
	orq	%rax, %rdx
	testq	$134217728, %rdx        # imm = 0x8000000
	jne	.LBB19_72
# BB#10:                                # %lor.lhs.false129
	movq	8(%rbx), %rdi
	movq	8(%r14), %rsi
	callq	strcmp
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB19_72
# BB#11:                                # %lor.lhs.false138
	movq	16(%rbx), %rdi
	movq	16(%r14), %rsi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB19_72
# BB#12:                                # %lor.lhs.false147
	movl	24(%rbx), %eax
	movl	24(%r14), %ecx
	cmpq	%rcx, %rax
	jne	.LBB19_72
# BB#13:                                # %lor.lhs.false155
	movq	32(%rbx), %rax
	movq	32(%r14), %rdx
	movl	(%rax), %eax
	movq	$32, %rcx
	movq	%rax, %r12
	movl	(%rdx), %edx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	%rdx, %rax
	jne	.LBB19_72
# BB#14:                                # %if.end165
	testq	%rax, %rax
	je	.LBB19_15
.LBB19_46:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_47
# BB#48:                                # %for.body
                                        #   in Loop: Header=BB19_46 Depth=1
	movq	32(%rbx), %rax
	movq	32(%r14), %rcx
	movq	(%rax,%r12,8), %rax
	movq	(%rcx,%r12,8), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	expr_equiv_p
	testq	%r15, %rax
	je	.LBB19_72
# BB#49:                                # %lor.lhs.false190
                                        #   in Loop: Header=BB19_46 Depth=1
	movq	40(%rbx), %rax
	movq	40(%r14), %rcx
	movq	(%rax,%r12,8), %rax
	movq	(%rcx,%r12,8), %rcx
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	leaq	-1(%r12), %r12
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB19_46
	jmp	.LBB19_72
.LBB19_1:
	movq	$1, %rax
	jmp	.LBB19_73
.LBB19_39:                              # %if.end20
	leaq	-102(%rcx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jb	.LBB19_40
	jmp	.LBB19_37
.LBB19_16:                              # %if.end20
	cmpq	$58, %rcx
	jg	.LBB19_20
# BB#17:                                # %if.end20
	cmpq	$54, %rcx
	jne	.LBB19_37
# BB#18:                                # %sw.bb23
	movq	8(%rbx), %rax
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_19
# BB#22:                                # %sw.bb23
	movabsq	$1, %rax
	jmp	.LBB19_73
.LBB19_20:                              # %if.end20
	leaq	-59(%rcx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$26, %rdx
	ja	.LBB19_37
# BB#21:                                # %if.end20
	xorq	%rax, %rax
	jmpq	*.LJTI19_0(,%rdx,8)
.LBB19_40:                              # %sw.bb78
	movq	8(%rbx), %rdi
	movq	8(%r14), %rsi
	callq	expr_equiv_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB19_42
# BB#41:                                # %land.lhs.true
	movq	16(%rbx), %rdi
	movq	16(%r14), %rsi
	callq	expr_equiv_p
	testq	%r15, %rax
	movq	$1, %rax
	jne	.LBB19_73
.LBB19_42:
	leaq	16(%r14), %rax
	movq	8(%rbx), %rdi
	movq	(%rax), %rsi
	callq	expr_equiv_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r15, %rcx
	je	.LBB19_73
# BB#43:                                # %land.rhs
	movq	16(%rbx), %rdi
	movq	8(%r14), %rsi
	callq	expr_equiv_p
	testq	%r15, %rax
	je	.LBB19_44
# BB#45:                                # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB19_73
.LBB19_44:
	movabsq	$0, %rax
	jmp	.LBB19_73
.LBB19_19:
	movabsq	$0, %rax
	jmp	.LBB19_73
.LBB19_29:                              # %sw.bb45
	movl	8(%rbx), %eax
	movl	8(%r14), %ecx
	cmpq	%rcx, %rax
	jne	.LBB19_30
# BB#31:                                # %sw.bb45
	movabsq	$1, %rax
	jmp	.LBB19_73
.LBB19_32:                              # %sw.bb53
	movq	16(%rbx), %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movabsq	$0, %rsi
	je	.LBB19_34
# BB#33:                                # %cond.false
	movq	(%rax), %rsi
.LBB19_34:                              # %cond.end
	movq	16(%r14), %rax
	testq	%rax, %rax
	je	.LBB19_36
# BB#35:                                # %cond.false67
	movq	(%rax), %rdx
.LBB19_36:                              # %cond.end72
	xorq	%rax, %rax
	cmpq	%rdx, %rsi
	jne	.LBB19_73
.LBB19_37:                              # %sw.epilog
	movzbl	rtx_length(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB19_38
# BB#50:                                # %for.body223.lr.ph
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB19_51:                              # %for.body223
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_70 Depth 2
	movq	%rdx, %r15
	movq	(%rsp), %rax            # 8-byte Reload
	movsbq	-1(%rax,%r15), %rax
	leaq	-1(%r15), %rcx
	cmpq	$68, %rax
	jg	.LBB19_55
# BB#52:                                # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	andq	%r12, %rax
	cmpq	$48, %rax
	je	.LBB19_53
	jmp	.LBB19_74
.LBB19_55:                              # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	cmpq	$100, %rax
	jg	.LBB19_59
# BB#56:                                # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	andq	%r12, %rax
	cmpq	$69, %rax
	jne	.LBB19_74
# BB#57:                                # %sw.bb240
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	(%rbx,%r15,8), %rax
	movq	(%r14,%r15,8), %rdx
	movl	(%rax), %esi
	movl	(%rdx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB19_72
# BB#58:                                # %for.cond255.preheader
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %r13
	testq	%rsi, %rsi
	jle	.LBB19_53
	jmp	.LBB19_70
	.align	16, 0x90
.LBB19_69:                              # %for.cond255.for.body263_crit_edge
                                        #   in Loop: Header=BB19_70 Depth=2
	movq	(%r14,%r15,8), %rdx
	incq	%r13
.LBB19_70:                              # %for.body263
                                        #   Parent Loop BB19_51 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r13,8), %rdi
	movq	(%rdx,%r13,8), %rsi
	callq	expr_equiv_p
	testq	%r12, %rax
	je	.LBB19_72
# BB#68:                                # %for.cond255
                                        #   in Loop: Header=BB19_70 Depth=2
	movq	(%rbx,%r15,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB19_69
	jmp	.LBB19_53
.LBB19_59:                              # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$114, %rax
	jg	.LBB19_64
# BB#60:                                # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	andq	%r12, %rax
	cmpq	$101, %rax
	jne	.LBB19_61
# BB#67:                                # %sw.bb227
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	(%rbx,%r15,8), %rdi
	movq	(%r14,%r15,8), %rsi
	callq	expr_equiv_p
	testq	%r12, %rax
	jne	.LBB19_53
	jmp	.LBB19_72
.LBB19_64:                              # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	andq	%r12, %rax
	cmpq	$115, %rax
	jne	.LBB19_65
# BB#71:                                # %sw.bb284
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	(%rbx,%r15,8), %rdi
	movq	(%r14,%r15,8), %rsi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB19_53
	jmp	.LBB19_72
.LBB19_61:                              # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	cmpq	$105, %rax
	jne	.LBB19_74
# BB#62:                                # %sw.bb297
                                        #   in Loop: Header=BB19_51 Depth=1
	movl	(%rbx,%r15,8), %eax
	movl	(%r14,%r15,8), %ecx
	jmp	.LBB19_63
.LBB19_65:                              # %for.body223
                                        #   in Loop: Header=BB19_51 Depth=1
	cmpq	$119, %rax
	jne	.LBB19_74
# BB#66:                                # %sw.bb310
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	(%rbx,%r15,8), %rax
	movq	(%r14,%r15,8), %rcx
.LBB19_63:                              # %sw.bb297
                                        #   in Loop: Header=BB19_51 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB19_72
.LBB19_53:                              # %for.cond220.backedge
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB19_51
# BB#54:
	movq	$1, %rax
	jmp	.LBB19_73
.LBB19_72:
	xorq	%rax, %rax
.LBB19_73:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_38:
	movq	$1, %rax
	jmp	.LBB19_73
.LBB19_23:                              # %sw.bb29
	movq	8(%rbx), %rax
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_24
# BB#25:                                # %sw.bb29
	movabsq	$1, %rax
	jmp	.LBB19_73
.LBB19_26:                              # %sw.bb37
	movq	8(%rbx), %rax
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_27
# BB#28:                                # %sw.bb37
	movabsq	$1, %rax
	jmp	.LBB19_73
.LBB19_30:
	movabsq	$0, %rax
	jmp	.LBB19_73
.LBB19_24:
	movabsq	$0, %rax
	jmp	.LBB19_73
.LBB19_27:
	movabsq	$0, %rax
	jmp	.LBB19_73
.LBB19_15:
	movq	$1, %rax
	jmp	.LBB19_73
.LBB19_47:
	movq	$1, %rax
	jmp	.LBB19_73
.LBB19_74:                              # %sw.default323
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.expr_equiv_p, %rcx
	movq	$1936, %rsi             # imm = 0x790
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp291:
	.size	expr_equiv_p, .Ltmp291-expr_equiv_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_73
	.quad	.LBB19_37
	.quad	.LBB19_29
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_32
	.quad	.LBB19_23
	.quad	.LBB19_26
	.quad	.LBB19_73
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_40
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_40
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_37
	.quad	.LBB19_40
	.quad	.LBB19_40
	.quad	.LBB19_40

	.text
	.align	16, 0x90
	.type	hash_expr_1,@function
hash_expr_1:                            # @hash_expr_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp298:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp299:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp300:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp301:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp302:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp303:
	.cfi_def_cfa_offset 96
.Ltmp304:
	.cfi_offset %rbx, -48
.Ltmp305:
	.cfi_offset %r12, -40
.Ltmp306:
	.cfi_offset %r13, -32
.Ltmp307:
	.cfi_offset %r14, -24
.Ltmp308:
	.cfi_offset %r15, -16
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rdi, %r9
	testq	%r9, %r9
	je	.LBB20_1
# BB#2:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	xorq	%rbx, %rbx
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	jmp	.LBB20_3
.LBB20_1:
	xorq	%r12, %r12
	jmp	.LBB20_77
.LBB20_28:                              # %cond.end
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	8(%r9), %r9
	andq	%rax, %rbx
	addq	%rdx, %rbx
	.align	16, 0x90
.LBB20_3:                               # %repeat
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_36 Depth 2
                                        #     Child Loop BB20_40 Depth 2
                                        #     Child Loop BB20_45 Depth 2
                                        #       Child Loop BB20_48 Depth 3
                                        #     Child Loop BB20_53 Depth 2
                                        #     Child Loop BB20_57 Depth 2
                                        #       Child Loop BB20_68 Depth 3
                                        #       Child Loop BB20_60 Depth 3
	movl	(%r9), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$95, %rdx
	jg	.LBB20_29
# BB#4:                                 # %repeat
                                        #   in Loop: Header=BB20_3 Depth=1
	leaq	-41(%rdx), %rcx
	andq	%rax, %rcx
	cmpq	$28, %rcx
	ja	.LBB20_30
# BB#5:                                 # %repeat
                                        #   in Loop: Header=BB20_3 Depth=1
	jmpq	*.LJTI20_0(,%rcx,8)
.LBB20_24:                              # %sw.bb72
                                        #   in Loop: Header=BB20_3 Depth=1
	testq	$134217728, %rdi        # imm = 0x8000000
	jne	.LBB20_25
# BB#26:                                # %if.end78
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	16(%r9), %rcx
	addq	$66, %rbx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB20_28
# BB#27:                                # %cond.false
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	(%rcx), %rdx
	jmp	.LBB20_28
	.align	16, 0x90
.LBB20_29:                              # %repeat
                                        #   in Loop: Header=BB20_3 Depth=1
	leaq	-96(%rdx), %rcx
	andq	%rax, %rcx
	cmpq	$4, %rcx
	jb	.LBB20_25
.LBB20_30:                              # %sw.epilog
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	movzbl	rtx_length(%rdx), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	andq	$255, %rdi
	addq	%rdx, %rbx
	addq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB20_31
# BB#56:                                # %for.body197.lr.ph
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	rtx_format(,%rdx,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB20_57:                              # %for.body197
                                        #   Parent Loop BB20_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB20_68 Depth 3
                                        #       Child Loop BB20_60 Depth 3
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movsbq	-1(%rax,%rcx), %rax
	leaq	-1(%rcx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	cmpq	$100, %rax
	jg	.LBB20_62
# BB#58:                                # %for.body197
                                        #   in Loop: Header=BB20_57 Depth=2
	andq	$255, %rax
	cmpq	$69, %rax
	jne	.LBB20_75
# BB#59:                                # %for.cond228.preheader
                                        #   in Loop: Header=BB20_57 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%r9,%rax,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	movq	%rbx, %r14
	jle	.LBB20_73
	.align	16, 0x90
.LBB20_60:                              # %for.body236
                                        #   Parent Loop BB20_3 Depth=1
                                        #     Parent Loop BB20_57 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rax,%r15,8), %rdi
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r9, %r13
	callq	hash_expr_1
	movq	%r13, %r9
	movl	(%rbx), %ecx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	jne	.LBB20_77
# BB#61:                                # %for.inc249
                                        #   in Loop: Header=BB20_60 Depth=3
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%r9,%rax,8), %rax
	addq	%r14, %rbx
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	movq	%rbx, %r14
	jl	.LBB20_60
	jmp	.LBB20_73
	.align	16, 0x90
.LBB20_62:                              # %for.body197
                                        #   in Loop: Header=BB20_57 Depth=2
	andq	$255, %rax
	cmpq	$101, %rax
	je	.LBB20_69
# BB#63:                                # %for.body197
                                        #   in Loop: Header=BB20_57 Depth=2
	cmpq	$105, %rax
	jne	.LBB20_64
# BB#74:                                # %if.then271
                                        #   in Loop: Header=BB20_57 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	(%r9,%rax,8), %eax
	jmp	.LBB20_72
.LBB20_69:                              # %if.then203
                                        #   in Loop: Header=BB20_57 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%r9,%rax,8), %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rsi, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB20_70
# BB#71:                                # %if.end211
                                        #   in Loop: Header=BB20_57 Depth=2
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r9, %r15
	callq	hash_expr_1
	movq	%r15, %r9
	movl	(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB20_72
	jmp	.LBB20_77
.LBB20_64:                              # %for.body197
                                        #   in Loop: Header=BB20_57 Depth=2
	cmpq	$115, %rax
	jne	.LBB20_75
# BB#65:                                # %if.then258
                                        #   in Loop: Header=BB20_57 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%r9,%rax,8), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB20_72
# BB#66:                                # %while.cond.preheader.i
                                        #   in Loop: Header=BB20_57 Depth=2
	movzbl	(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB20_72
# BB#67:                                # %while.body.i.preheader
                                        #   in Loop: Header=BB20_57 Depth=2
	incq	%rcx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB20_68:                              # %while.body.i
                                        #   Parent Loop BB20_3 Depth=1
                                        #     Parent Loop BB20_57 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	$255, %rdx
	addq	%rdx, %rax
	movzbl	(%rcx), %edx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB20_68
	.align	16, 0x90
.LBB20_72:                              #   in Loop: Header=BB20_57 Depth=2
	addq	%rax, %rbx
.LBB20_73:                              # %for.cond194.backedge
                                        #   in Loop: Header=BB20_57 Depth=2
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r12
	jg	.LBB20_57
	jmp	.LBB20_77
.LBB20_70:                              #   in Loop: Header=BB20_3 Depth=1
	movq	%rdi, %r9
	movq	8(%rsp), %rsi           # 8-byte Reload
	jmp	.LBB20_3
.LBB20_32:                              # %sw.bb94
                                        #   in Loop: Header=BB20_3 Depth=1
	testq	$134217728, %rdi        # imm = 0x8000000
	jne	.LBB20_25
# BB#33:                                # %if.else100
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	$16, %rcx
	movq	8(%r9), %rdx
	shrq	%cl, %rdi
	xorq	%r8, %r8
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB20_37
# BB#34:                                # %while.cond.preheader.i389
                                        #   in Loop: Header=BB20_3 Depth=1
	movzbl	(%rdx), %esi
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB20_37
# BB#35:                                # %while.body.i396.preheader
                                        #   in Loop: Header=BB20_3 Depth=1
	incq	%rdx
	xorq	%rcx, %rcx
.LBB20_36:                              # %while.body.i396
                                        #   Parent Loop BB20_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rsi
	addq	%rsi, %rcx
	movzbl	(%rdx), %esi
	incq	%rdx
	testq	%rsi, %rsi
	jne	.LBB20_36
.LBB20_37:                              # %hash_string_1.exit398
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	16(%r9), %rdx
	movq	%rax, %r15
	testq	%rdx, %rdx
	je	.LBB20_41
# BB#38:                                # %while.cond.preheader.i401
                                        #   in Loop: Header=BB20_3 Depth=1
	movzbl	(%rdx), %eax
	testq	%rax, %rax
	je	.LBB20_41
# BB#39:                                # %while.body.i408.preheader
                                        #   in Loop: Header=BB20_3 Depth=1
	incq	%rdx
	xorq	%r8, %r8
.LBB20_40:                              # %while.body.i408
                                        #   Parent Loop BB20_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	addq	%rax, %r8
	movzbl	(%rdx), %eax
	incq	%rdx
	testq	%rax, %rax
	jne	.LBB20_40
.LBB20_41:                              # %hash_string_1.exit410
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	32(%r9), %rdx
	andq	$255, %rdi
	movl	24(%r9), %esi
	addq	%rdi, %rbx
	movl	(%rdx), %edi
	addq	%rcx, %rbx
	addq	%r8, %rbx
	leaq	41(%rsi,%rbx), %rax
	testq	%rdi, %rdi
	je	.LBB20_42
# BB#43:                                # %for.cond126.preheader
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %r12
	cmpq	$1, %rdi
	jle	.LBB20_78
# BB#44:                                #   in Loop: Header=BB20_3 Depth=1
	movq	%rax, %r14
	movq	%r9, %rbx
.LBB20_45:                              # %for.body133
                                        #   Parent Loop BB20_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB20_48 Depth 3
	movq	8(%rdx,%r12,8), %rdi
	movzbl	2(%rdi), %esi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	hash_expr_1
	movq	40(%rbx), %r8
	movq	8(%r8,%r12,8), %rcx
	movq	8(%rcx), %rcx
	xorq	%rdi, %rdi
	testq	%rcx, %rcx
	movq	%rbx, %rsi
	je	.LBB20_49
# BB#46:                                # %while.cond.preheader.i425
                                        #   in Loop: Header=BB20_45 Depth=2
	movzbl	(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB20_49
# BB#47:                                # %while.body.i432.preheader
                                        #   in Loop: Header=BB20_45 Depth=2
	incq	%rcx
	xorq	%rdi, %rdi
.LBB20_48:                              # %while.body.i432
                                        #   Parent Loop BB20_3 Depth=1
                                        #     Parent Loop BB20_45 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	andq	$255, %rdx
	addq	%rdx, %rdi
	movzbl	(%rcx), %edx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB20_48
.LBB20_49:                              # %hash_string_1.exit434
                                        #   in Loop: Header=BB20_45 Depth=2
	addq	%r14, %rax
	movq	32(%rsi), %rdx
	addq	%rdi, %rax
	incq	%r12
	movslq	(%rdx), %rbx
	movq	$32, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rbx, %rdi
	movq	%rax, %r14
	movq	%rsi, %rbx
	jl	.LBB20_45
	jmp	.LBB20_50
.LBB20_78:                              # %for.cond126.preheader.for.end164_crit_edge
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	40(%r9), %r8
.LBB20_50:                              # %for.end164
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	8(%r8), %rcx
	movq	8(%rcx), %rsi
	xorq	%rbx, %rbx
	testq	%rsi, %rsi
	je	.LBB20_54
# BB#51:                                # %while.cond.preheader.i413
                                        #   in Loop: Header=BB20_3 Depth=1
	movzbl	(%rsi), %ecx
	testq	%rcx, %rcx
	je	.LBB20_54
# BB#52:                                # %while.body.i420.preheader
                                        #   in Loop: Header=BB20_3 Depth=1
	incq	%rsi
	xorq	%rbx, %rbx
.LBB20_53:                              # %while.body.i420
                                        #   Parent Loop BB20_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	addq	%rcx, %rbx
	movzbl	(%rsi), %ecx
	incq	%rsi
	testq	%rcx, %rcx
	jne	.LBB20_53
.LBB20_54:                              # %hash_string_1.exit422
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	8(%rdx), %r9
	movzbl	2(%r9), %esi
	addq	%rax, %rbx
	movq	%r15, %rax
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	jmp	.LBB20_3
.LBB20_25:                              # %if.then77
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	%eax, (%rcx)
	xorq	%r12, %r12
	jmp	.LBB20_77
.LBB20_31:
	movq	%rbx, %r12
.LBB20_77:                              # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_6:                               # %sw.bb2
	movl	8(%r9), %eax
	addq	%rsi, %rbx
	leaq	6912(%rax,%rbx), %r12
	jmp	.LBB20_77
.LBB20_7:                               # %sw.bb8
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	leaq	55(%rbx,%rdi), %rdx
	je	.LBB20_12
# BB#8:                                 # %for.cond.preheader
	movq	%rdx, %rcx
	movzbl	rtx_length+55(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB20_9
# BB#10:                                # %for.body.lr.ph
	movq	$3, %rdx
	movq	%rcx, %r12
.LBB20_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r9,%rdx,8), %ecx
	addq	%rcx, %r12
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB20_11
	jmp	.LBB20_77
.LBB20_13:                              # %sw.bb38
	movq	8(%r9), %rax
	movslq	(%rax), %r12
	testq	%r12, %r12
	jle	.LBB20_14
# BB#15:
	movq	%r9, %r13
	movq	$1, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r12
	jmp	.LBB20_16
.LBB20_18:                              # %for.body44.for.body44_crit_edge
                                        #   in Loop: Header=BB20_16 Depth=1
	movq	8(%r13), %rax
	incq	%r15
.LBB20_16:                              # %for.body44
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r15,8), %rdi
	movzbl	2(%rdi), %esi
	movq	%rbx, %r14
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	hash_expr_1
	movq	%rax, %rbx
	addq	%r14, %rbx
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r12, %rax
	jne	.LBB20_18
# BB#17:
	movq	%rbx, %r12
	jmp	.LBB20_77
.LBB20_76:                              # %sw.bb
	movl	8(%r9), %eax
	leaq	7808(%rbx,%rax), %r12
	jmp	.LBB20_77
.LBB20_19:                              # %sw.bb57
	movq	8(%r9), %rax
	movl	48(%rax), %eax
	leaq	8576(%rbx,%rax), %r12
	jmp	.LBB20_77
.LBB20_20:                              # %sw.bb64
	movq	8(%r9), %rax
	movzbl	(%rax), %ecx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB20_23
# BB#21:                                # %while.body.preheader
	incq	%rax
	xorq	%rdx, %rdx
.LBB20_22:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	imulq	$129, %rdx, %rdx
	addq	%rcx, %rdx
	movzbl	(%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB20_22
.LBB20_23:                              # %while.end
	leaq	8704(%rbx,%rdx), %r12
	jmp	.LBB20_77
.LBB20_12:                              # %if.else
	movl	16(%r9), %eax
	movl	24(%r9), %ecx
	addq	%rdx, %rax
	addq	%rcx, %rax
	movq	%rax, %r12
	jmp	.LBB20_77
.LBB20_14:
	movq	%rbx, %r12
	jmp	.LBB20_77
.LBB20_9:
	movq	%rcx, %r12
	jmp	.LBB20_77
.LBB20_75:                              # %if.else277
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.hash_expr_1, %rcx
	movq	$1776, %rsi             # imm = 0x6F0
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB20_42:
	movq	%rax, %r12
	jmp	.LBB20_77
.Ltmp309:
	.size	hash_expr_1, .Ltmp309-hash_expr_1
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI20_0:
	.quad	.LBB20_32
	.quad	.LBB20_30
	.quad	.LBB20_25
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_25
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_6
	.quad	.LBB20_7
	.quad	.LBB20_13
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_25
	.quad	.LBB20_30
	.quad	.LBB20_76
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_30
	.quad	.LBB20_24
	.quad	.LBB20_19
	.quad	.LBB20_20
	.quad	.LBB20_25

	.text
	.align	16, 0x90
	.type	oprs_unchanged_p,@function
oprs_unchanged_p:                       # @oprs_unchanged_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp316:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp317:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp318:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp319:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp320:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp321:
	.cfi_def_cfa_offset 80
.Ltmp322:
	.cfi_offset %rbx, -48
.Ltmp323:
	.cfi_offset %r12, -40
.Ltmp324:
	.cfi_offset %r13, -32
.Ltmp325:
	.cfi_offset %r14, -24
.Ltmp326:
	.cfi_offset %r15, -16
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	testq	%r12, %r12
	movq	$1, %rbx
	je	.LBB21_32
# BB#1:                                 # %if.end.lr.ph
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB21_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	xorq	%rbx, %rbx
	cmpq	$95, %rax
	jg	.LBB21_31
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB21_2 Depth=1
	leaq	-44(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$25, %rcx
	ja	.LBB21_18
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$58776579, %rdx         # imm = 0x380DC03
	jne	.LBB21_5
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB21_2 Depth=1
	cmpq	$17, %rcx
	je	.LBB21_10
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB21_2 Depth=1
	cmpq	$22, %rcx
	jne	.LBB21_18
# BB#8:                                 # %sw.bb18
                                        #   in Loop: Header=BB21_2 Depth=1
	movslq	current_bb(%rip), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movslq	8(%rcx), %rcx
	movq	uid_cuid(%rip), %rdx
	movl	(%rdx,%rcx,4), %esi
	movl	88(%rax), %edi
	movq	%r12, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	load_killed_in_block_p
	testq	%r15, %rax
	jne	.LBB21_32
# BB#9:                                 # %if.else28
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	8(%r12), %r12
	testq	%r12, %r12
	movq	$1, %rbx
	jne	.LBB21_2
	jmp	.LBB21_32
.LBB21_31:                              # %if.end
	leaq	-96(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jae	.LBB21_18
.LBB21_32:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB21_18:                              # %sw.epilog
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB21_19
# BB#20:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB21_21:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_30 Depth 2
	movq	%rdx, %r13
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB21_22
# BB#25:                                # %for.cond72.preheader
                                        #   in Loop: Header=BB21_21 Depth=1
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r14
	jle	.LBB21_23
	.align	16, 0x90
.LBB21_30:                              # %for.body78
                                        #   Parent Loop BB21_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	oprs_unchanged_p
	xorq	%rbx, %rbx
	testq	%r15, %rax
	je	.LBB21_32
# BB#29:                                # %for.cond72
                                        #   in Loop: Header=BB21_30 Depth=2
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB21_30
	jmp	.LBB21_23
	.align	16, 0x90
.LBB21_22:                              # %for.body
                                        #   in Loop: Header=BB21_21 Depth=1
	cmpq	$101, %rax
	jne	.LBB21_23
# BB#26:                                # %if.then46
                                        #   in Loop: Header=BB21_21 Depth=1
	movq	(%r12,%r13,8), %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	oprs_unchanged_p
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB21_27
# BB#28:                                # %if.else55
                                        #   in Loop: Header=BB21_21 Depth=1
	xorq	%rbx, %rbx
	testq	%r15, %rax
	je	.LBB21_32
.LBB21_23:                              # %for.cond.backedge
                                        #   in Loop: Header=BB21_21 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB21_21
# BB#24:
	movq	$1, %rbx
	jmp	.LBB21_32
.LBB21_19:
	movq	$1, %rbx
	jmp	.LBB21_32
.LBB21_5:
	movq	$1, %rbx
	jmp	.LBB21_32
.LBB21_10:                              # %sw.bb
	movl	8(%r12), %ecx
	movq	reg_avail_info(%rip), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movl	(%rax,%rcx,4), %edx
	movl	current_bb(%rip), %esi
	cmpq	%rsi, %rdx
	movq	$1, %rbx
	jne	.LBB21_32
# BB#11:                                # %if.end4
	movq	24(%rsp), %rdx          # 8-byte Reload
	testq	%r15, %rdx
	je	.LBB21_15
# BB#12:                                # %if.then5
	movq	16(%rsp), %rdx          # 8-byte Reload
	movslq	8(%rdx), %rdx
	movq	uid_cuid(%rip), %rsi
	movslq	8(%rax,%rcx,4), %rax
	movslq	(%rsi,%rdx,4), %rcx
	cmpq	%rcx, %rax
	jge	.LBB21_13
# BB#14:                                # %if.then5
	movabsq	$1, %rbx
	jmp	.LBB21_32
.LBB21_27:
	movq	%rax, %rbx
	jmp	.LBB21_32
.LBB21_15:                              # %if.else
	movq	16(%rsp), %rdx          # 8-byte Reload
	movslq	8(%rdx), %rdx
	movq	uid_cuid(%rip), %rsi
	movslq	4(%rax,%rcx,4), %rax
	movslq	(%rsi,%rdx,4), %rcx
	cmpq	%rcx, %rax
	jl	.LBB21_16
# BB#17:                                # %if.else
	movabsq	$1, %rbx
	jmp	.LBB21_32
.LBB21_13:
	movabsq	$0, %rbx
	jmp	.LBB21_32
.LBB21_16:
	movabsq	$0, %rbx
	jmp	.LBB21_32
.Ltmp327:
	.size	oprs_unchanged_p, .Ltmp327-oprs_unchanged_p
	.cfi_endproc

	.align	16, 0x90
	.type	load_killed_in_block_p,@function
load_killed_in_block_p:                 # @load_killed_in_block_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp334:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp335:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp336:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp337:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp338:
	.cfi_def_cfa_offset 48
.Ltmp339:
	.cfi_offset %rbx, -48
.Ltmp340:
	.cfi_offset %r12, -40
.Ltmp341:
	.cfi_offset %r13, -32
.Ltmp342:
	.cfi_offset %r14, -24
.Ltmp343:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	$32, %rcx
	movq	modify_mem_list(%rip), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	(%rbx,%rdi,8), %rbx
	movq	%rdx, %r14
	movq	%rsi, %r15
	xorq	%r13, %r13
	testq	%rbx, %rbx
	je	.LBB22_10
# BB#1:                                 # %while.body.lr.ph
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movabsq	$mems_conflict_for_gcse_p, %r12
	testq	%rcx, %rax
	je	.LBB22_2
	.align	16, 0x90
.LBB22_6:                               # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movq	uid_cuid(%rip), %rcx
	movslq	8(%rax), %rdx
	movslq	(%rcx,%rdx,4), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB22_7
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB22_6 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	movq	$1, %r13
	je	.LBB22_10
# BB#9:                                 # %if.end26
                                        #   in Loop: Header=BB22_6 Depth=1
	movq	%r14, gcse_mem_operand(%rip)
	xorq	%rcx, %rcx
	movb	%cl, gcse_mems_conflict_p(%rip)
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	note_stores
	movzbl	gcse_mems_conflict_p(%rip), %eax
	testq	%rax, %rax
	jne	.LBB22_10
.LBB22_7:                               # %while.cond.backedge
                                        #   in Loop: Header=BB22_6 Depth=1
	movq	16(%rbx), %rbx
	xorq	%r13, %r13
	testq	%rbx, %rbx
	jne	.LBB22_6
	jmp	.LBB22_10
	.align	16, 0x90
.LBB22_2:                               # %land.lhs.true8.us
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movq	uid_cuid(%rip), %rcx
	movslq	8(%rax), %rdx
	movslq	(%rcx,%rdx,4), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jg	.LBB22_5
# BB#3:                                 # %if.end.us
                                        #   in Loop: Header=BB22_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	movq	$1, %r13
	je	.LBB22_10
# BB#4:                                 # %if.end26.us
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	%r14, gcse_mem_operand(%rip)
	xorq	%rcx, %rcx
	movb	%cl, gcse_mems_conflict_p(%rip)
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	note_stores
	movzbl	gcse_mems_conflict_p(%rip), %eax
	testq	%rax, %rax
	jne	.LBB22_10
.LBB22_5:                               # %while.cond.backedge.us
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	16(%rbx), %rbx
	xorq	%r13, %r13
	testq	%rbx, %rbx
	jne	.LBB22_2
.LBB22_10:                              # %return
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp344:
	.size	load_killed_in_block_p, .Ltmp344-load_killed_in_block_p
	.cfi_endproc

	.align	16, 0x90
	.type	mems_conflict_for_gcse_p,@function
mems_conflict_for_gcse_p:               # @mems_conflict_for_gcse_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp349:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp350:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp351:
	.cfi_def_cfa_offset 32
.Ltmp352:
	.cfi_offset %rbx, -32
.Ltmp353:
	.cfi_offset %r14, -24
.Ltmp354:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_4:                               # %while.body
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rbx), %rbx
.LBB23_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jb	.LBB23_4
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$66, %rax
	je	.LBB23_5
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB23_1 Depth=1
	addq	$-132, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	jbe	.LBB23_4
	jmp	.LBB23_13
.LBB23_5:                               # %if.end
	movq	gcse_mem_operand(%rip), %rdx
	cmpq	%rdx, %rbx
	jne	.LBB23_11
# BB#6:                                 # %if.end
	movq	pre_ldst_mems(%rip), %r15
	testq	%r15, %r15
	je	.LBB23_11
	.align	16, 0x90
.LBB23_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rdi
	movq	%rbx, %rsi
	callq	expr_equiv_p
	testq	%r14, %rax
	je	.LBB23_9
# BB#8:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB23_7 Depth=1
	movl	40(%r15), %eax
	testq	%rax, %rax
	je	.LBB23_10
.LBB23_9:                               # %for.inc.i
                                        #   in Loop: Header=BB23_7 Depth=1
	movq	32(%r15), %r15
	testq	%r15, %r15
	jne	.LBB23_7
	jmp	.LBB23_12
.LBB23_11:                              # %if.end19
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movabsq	$rtx_addr_varies_p, %rcx
	movq	%rbx, %rdi
	callq	true_dependence
	testq	%r14, %rax
	jne	.LBB23_12
	jmp	.LBB23_13
.LBB23_10:                              # %find_rtx_in_ldst.exit
	testq	%r15, %r15
	jne	.LBB23_13
.LBB23_12:                              # %if.then24
	movq	$1, %rax
	movb	%al, gcse_mems_conflict_p(%rip)
.LBB23_13:                              # %if.end25
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp355:
	.size	mems_conflict_for_gcse_p, .Ltmp355-mems_conflict_for_gcse_p
	.cfi_endproc

	.align	16, 0x90
	.type	record_last_mem_set_info,@function
record_last_mem_set_info:               # @record_last_mem_set_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp359:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp360:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp361:
	.cfi_def_cfa_offset 32
.Ltmp362:
	.cfi_offset %rbx, -24
.Ltmp363:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movslq	8(%r14), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movslq	88(%rax), %rbx
	movq	modify_mem_list(%rip), %rax
	movq	(%rax,%rbx,8), %rsi
                                        # kill: RDI<def> R14<kill>
	callq	alloc_INSN_LIST
	movq	modify_mem_list(%rip), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	modify_mem_list_set(%rip), %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movzbl	(%r14), %eax
	cmpq	$34, %rax
	jne	.LBB24_2
# BB#1:                                 # %if.then
	movq	canon_modify_mem_list(%rip), %rax
	movq	(%rax,%rbx,8), %rsi
	movq	%r14, %rdi
	callq	alloc_INSN_LIST
	movq	canon_modify_mem_list(%rip), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	canon_modify_mem_list_set(%rip), %rdi
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	bitmap_set_bit  # TAILCALL
.LBB24_2:                               # %if.else
	movq	32(%r14), %rdi
	movabsq	$canon_list_insert, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	note_stores  # TAILCALL
.Ltmp364:
	.size	record_last_mem_set_info, .Ltmp364-record_last_mem_set_info
	.cfi_endproc

	.align	16, 0x90
	.type	canon_list_insert,@function
canon_list_insert:                      # @canon_list_insert
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp368:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp369:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp370:
	.cfi_def_cfa_offset 32
.Ltmp371:
	.cfi_offset %rbx, -24
.Ltmp372:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rdi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB25_1
	.align	16, 0x90
.LBB25_4:                               # %while.body
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	8(%rbx), %rbx
.LBB25_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	leaq	-63(%rcx), %rdx
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jb	.LBB25_4
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB25_1 Depth=1
	cmpq	$66, %rcx
	je	.LBB25_5
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB25_1 Depth=1
	addq	$-132, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jbe	.LBB25_4
# BB#6:                                 # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB25_5:                               # %if.end
	movq	8(%rbx), %rdi
	callq	get_addr
	movq	%rax, %rdi
	callq	canon_rtx
	movslq	8(%r14), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
	movslq	88(%rcx), %r14
	movq	canon_modify_mem_list(%rip), %rcx
	movq	(%rcx,%r14,8), %rdx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	alloc_EXPR_LIST
	movq	canon_modify_mem_list(%rip), %rcx
	movq	%rax, (%rcx,%r14,8)
	movq	canon_modify_mem_list(%rip), %rax
	movq	(%rax,%r14,8), %rdx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	alloc_EXPR_LIST
	movq	canon_modify_mem_list(%rip), %rcx
	movq	%rax, (%rcx,%r14,8)
	movq	canon_modify_mem_list_set(%rip), %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	bitmap_set_bit  # TAILCALL
.Ltmp373:
	.size	canon_list_insert, .Ltmp373-canon_list_insert
	.cfi_endproc

	.align	16, 0x90
	.type	invalidate_nonnull_info,@function
invalidate_nonnull_info:                # @invalidate_nonnull_info
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB26_1
	.align	16, 0x90
.LBB26_3:                               # %while.body
                                        #   in Loop: Header=BB26_1 Depth=1
	movq	8(%rdi), %rdi
.LBB26_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	je	.LBB26_4
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB26_1 Depth=1
	cmpq	$63, %rax
	je	.LBB26_3
	jmp	.LBB26_7
.LBB26_4:                               # %lor.lhs.false
	movl	8(%rdi), %eax
	movl	4(%rdx), %esi
	movq	%rax, %rcx
	subq	%rsi, %rcx
	jb	.LBB26_7
# BB#5:                                 # %lor.lhs.false7
	movl	8(%rdx), %esi
	cmpq	%rsi, %rax
	jae	.LBB26_7
# BB#6:                                 # %if.end
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	movslq	(%rdx), %r9
	movq	16(%rdx), %r8
	andq	%rcx, %rax
	movq	$1, %rdi
	shlq	%cl, %rdi
	movq	%rdi, %rsi
	movq	(%r8,%r9,8), %r8
	notq	%rsi
	movq	$3, %rcx
	shrq	%cl, %rax
	andq	%rsi, 16(%r8,%rax)
	movq	24(%rdx), %rcx
	movq	(%rcx,%r9,8), %rcx
	orq	%rdi, 16(%rcx,%rax)
.LBB26_7:                               # %return
	retq
.Ltmp374:
	.size	invalidate_nonnull_info, .Ltmp374-invalidate_nonnull_info
	.cfi_endproc

	.align	16, 0x90
	.type	update_ld_motion_stores,@function
update_ld_motion_stores:                # @update_ld_motion_stores
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp381:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp382:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp383:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp384:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp385:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp386:
	.cfi_def_cfa_offset 64
.Ltmp387:
	.cfi_offset %rbx, -48
.Ltmp388:
	.cfi_offset %r12, -40
.Ltmp389:
	.cfi_offset %r13, -32
.Ltmp390:
	.cfi_offset %r14, -24
.Ltmp391:
	.cfi_offset %r15, -16
	movq	pre_ldst_mems(%rip), %rbx
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB27_5
# BB#1:
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB27_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%r15, %rsi
	callq	expr_equiv_p
	testq	%r14, %rax
	je	.LBB27_4
# BB#3:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB27_2 Depth=1
	movl	40(%rbx), %eax
	testq	%rax, %rax
	je	.LBB27_6
.LBB27_4:                               # %for.inc.i
                                        #   in Loop: Header=BB27_2 Depth=1
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB27_2
	jmp	.LBB27_5
.LBB27_6:                               # %find_rtx_in_ldst.exit
	testq	%rbx, %rbx
	je	.LBB27_5
# BB#7:                                 # %if.then
	movq	24(%rbx), %r15
	jmp	.LBB27_8
	.align	16, 0x90
.LBB27_13:                              # %for.inc
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	16(%r15), %r15
.LBB27_8:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB27_5
# BB#9:                                 # %for.body
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	8(%r15), %r13
	movq	32(%r13), %r12
	movq	16(%r12), %rsi
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	40(%rax), %rbx
	cmpq	%rsi, %rbx
	je	.LBB27_13
# BB#10:                                # %if.end
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	gcse_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB27_12
# BB#11:                                # %if.then12
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	$38, %rsi
	movq	$1, %rdx
	movabsq	$.L.str19, %rdi
	callq	fwrite
	movq	gcse_file(%rip), %rdi
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	40(%rax), %rsi
	callq	print_rtl
	movq	gcse_file(%rip), %rcx
	movq	$3, %rsi
	movq	$1, %rdx
	movabsq	$.L.str20, %rdi
	callq	fwrite
	movq	gcse_file(%rip), %rdi
	movq	$8, %rdx
	movq	%r13, %rsi
	callq	print_inline_rtx
	movq	gcse_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	16(%r12), %rsi
.LBB27_12:                              # %if.end17
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	%rbx, %rdi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	movl	8(%rbx), %edi
	movq	%rax, %rsi
	callq	record_one_set
	movq	%rbx, 16(%r12)
	movl	%r14d, 40(%r13)
	movl	gcse_create_count(%rip), %eax
	incq	%rax
	movl	%eax, gcse_create_count(%rip)
	jmp	.LBB27_13
.LBB27_5:                               # %if.end33
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp392:
	.size	update_ld_motion_stores, .Ltmp392-update_ld_motion_stores
	.cfi_endproc

	.align	16, 0x90
	.type	pre_expr_reaches_here_p_work,@function
pre_expr_reaches_here_p_work:           # @pre_expr_reaches_here_p_work
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp399:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp400:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp401:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp402:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp403:
	.cfi_def_cfa_offset 48
.Ltmp404:
	.cfi_offset %rbx, -48
.Ltmp405:
	.cfi_offset %r12, -40
.Ltmp406:
	.cfi_offset %r13, -32
.Ltmp407:
	.cfi_offset %r14, -24
.Ltmp408:
	.cfi_offset %r15, -16
	movq	32(%rdx), %rbx
	movq	%rcx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB28_10
# BB#1:                                 # %for.body.lr.ph
	movabsq	$entry_exit_blocks, %r13
	movq	$1, %r10
	.align	16, 0x90
.LBB28_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rdx
	cmpq	%r13, %rdx
	je	.LBB28_9
# BB#3:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB28_2 Depth=1
	movslq	88(%rdx), %rsi
	movzbl	(%r12,%rsi), %eax
	testq	%rax, %rax
	jne	.LBB28_9
# BB#4:                                 # %if.else
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	comp(%rip), %rax
	movl	8(%r14), %r8d
	movq	(%rax,%rsi,8), %rax
	movq	$6, %rcx
	movq	%r8, %rdi
	shrq	%cl, %rdi
	movq	16(%rax,%rdi,8), %r9
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	testq	%rax, %r9
	jne	.LBB28_5
# BB#7:                                 # %if.else18
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	transp(%rip), %rcx
	movq	(%rcx,%rsi,8), %rcx
	movq	16(%rcx,%rdi,8), %rcx
	testq	%rax, %rcx
	movb	%r10b, (%r12,%rsi)
	je	.LBB28_9
# BB#8:                                 # %if.else37
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rcx
	callq	pre_expr_reaches_here_p_work
	movq	$1, %r10
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	je	.LBB28_9
	jmp	.LBB28_11
.LBB28_5:                               # %if.then11
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	$1, %rax
	cmpq	%r15, %rdx
	je	.LBB28_11
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB28_2 Depth=1
	movb	%r10b, (%r12,%rsi)
	.align	16, 0x90
.LBB28_9:                               # %for.inc
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB28_2
.LBB28_10:
	xorq	%rax, %rax
.LBB28_11:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp409:
	.size	pre_expr_reaches_here_p_work, .Ltmp409-pre_expr_reaches_here_p_work
	.cfi_endproc

	.align	16, 0x90
	.type	invalidate_any_buried_refs,@function
invalidate_any_buried_refs:             # @invalidate_any_buried_refs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp416:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp417:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp418:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp419:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp420:
	.cfi_def_cfa_offset 48
.Ltmp421:
	.cfi_offset %rbx, -48
.Ltmp422:
	.cfi_offset %r12, -40
.Ltmp423:
	.cfi_offset %r13, -32
.Ltmp424:
	.cfi_offset %r14, -24
.Ltmp425:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB29_10
# BB#1:                                 # %land.lhs.true
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$66, %rcx
	jne	.LBB29_10
# BB#2:                                 # %land.lhs.true
	andq	$16711680, %rax         # imm = 0xFF0000
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB29_10
# BB#3:                                 # %simple_mem.exit
	movq	8(%r12), %rdi
	xorq	%rsi, %rsi
	callq	rtx_varies_p
	testq	%rbx, %rax
	jne	.LBB29_10
# BB#4:                                 # %if.then
	movq	pre_ldst_mems(%rip), %r14
	jmp	.LBB29_5
.LBB29_7:                               # %for.cond.i
                                        #   in Loop: Header=BB29_5 Depth=1
	movq	32(%r14), %r14
.LBB29_5:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB29_6
# BB#8:                                 # %for.body.i
                                        #   in Loop: Header=BB29_5 Depth=1
	movq	8(%r14), %rdi
	movq	%r12, %rsi
	callq	expr_equiv_p
	testq	%rbx, %rax
	je	.LBB29_7
	jmp	.LBB29_9
.LBB29_6:                               # %if.then4.i
	movq	$64, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	pre_ldst_mems(%rip), %rax
	movq	%rax, 32(%r14)
	movq	$0, (%r14)
	movq	%r12, 8(%r14)
	movq	$0, 56(%r14)
	movq	$0, 40(%r14)
	xorq	%rax, %rax
	movl	%eax, 48(%r14)
	movq	$0, 24(%r14)
	movq	$0, 16(%r14)
	movq	%r14, pre_ldst_mems(%rip)
.LBB29_9:                               # %ldst_entry.exit
	movq	$1, %rax
	movl	%eax, 40(%r14)
.LBB29_10:                              # %if.end
	movzbl	(%r12), %eax
	movzbl	rtx_length(%rax), %r14d
	testq	%r14, %r14
	je	.LBB29_15
# BB#11:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r15
	.align	16, 0x90
.LBB29_12:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_18 Depth 2
	movq	%r14, %r13
	movzbl	-1(%r15,%r13), %eax
	leaq	-1(%r13), %r14
	cmpq	$69, %rax
	jne	.LBB29_13
# BB#17:                                # %if.then23
                                        #   in Loop: Header=BB29_12 Depth=1
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB29_14
	jmp	.LBB29_18
	.align	16, 0x90
.LBB29_19:                              # %for.body31.for.body31_crit_edge
                                        #   in Loop: Header=BB29_18 Depth=2
	movq	(%r12,%r13,8), %rax
.LBB29_18:                              # %for.body31.lr.ph
                                        #   Parent Loop BB29_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdi
	callq	invalidate_any_buried_refs
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB29_19
	jmp	.LBB29_14
	.align	16, 0x90
.LBB29_13:                              # %for.body
                                        #   in Loop: Header=BB29_12 Depth=1
	cmpq	$101, %rax
	jne	.LBB29_14
# BB#16:                                # %if.then15
                                        #   in Loop: Header=BB29_12 Depth=1
	movq	(%r12,%r13,8), %rdi
	callq	invalidate_any_buried_refs
.LBB29_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB29_12 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB29_12
.LBB29_15:                              # %for.end42
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp426:
	.size	invalidate_any_buried_refs, .Ltmp426-invalidate_any_buried_refs
	.cfi_endproc

	.align	16, 0x90
	.type	mark_oprs_set,@function
mark_oprs_set:                          # @mark_oprs_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp433:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp434:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp435:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp436:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp437:
	.cfi_def_cfa_offset 48
.Ltmp438:
	.cfi_offset %rbx, -48
.Ltmp439:
	.cfi_offset %r12, -40
.Ltmp440:
	.cfi_offset %r13, -32
.Ltmp441:
	.cfi_offset %r14, -24
.Ltmp442:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	32(%r14), %r15
	movzbl	(%r15), %eax
	cmpq	$46, %rax
	jg	.LBB30_13
# BB#1:                                 # %entry
	cmpq	$39, %rax
	jne	.LBB30_42
# BB#2:                                 # %for.cond.preheader
	movq	8(%r15), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB30_42
# BB#3:                                 # %for.body.lr.ph
	xorq	%r12, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB30_8 Depth 2
                                        #     Child Loop BB30_38 Depth 2
	movq	8(%rax,%r12,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$50, %rax
	je	.LBB30_36
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB30_4 Depth=1
	cmpq	$49, %rax
	jne	.LBB30_6
	.align	16, 0x90
.LBB30_38:                              # %while.cond.i85
                                        #   Parent Loop BB30_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	leaq	-63(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB30_38
# BB#39:                                # %while.end.i
                                        #   in Loop: Header=BB30_4 Depth=1
	cmpq	$61, %rax
	jne	.LBB30_37
# BB#40:                                # %if.then.i87
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	reg_set_bitmap(%rip), %rdi
	movl	8(%rbx), %esi
	callq	bitmap_set_bit
	jmp	.LBB30_41
	.align	16, 0x90
.LBB30_6:                               # %for.body
                                        #   in Loop: Header=BB30_4 Depth=1
	cmpq	$47, %rax
	jne	.LBB30_41
# BB#7:                                 # %if.then15
                                        #   in Loop: Header=BB30_4 Depth=1
	leaq	8(%rbx), %rax
	jmp	.LBB30_8
	.align	16, 0x90
.LBB30_33:                              # %while.body.i66
                                        #   in Loop: Header=BB30_8 Depth=2
	addq	$8, %rax
.LBB30_8:                               # %while.cond.i63
                                        #   Parent Loop BB30_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$131, %rcx
	jg	.LBB30_32
# BB#9:                                 # %while.cond.i63
                                        #   in Loop: Header=BB30_8 Depth=2
	leaq	-63(%rcx), %rdx
	andq	%r13, %rdx
	cmpq	$2, %rdx
	jb	.LBB30_33
	jmp	.LBB30_10
	.align	16, 0x90
.LBB30_32:                              # %while.cond.i63
                                        #   in Loop: Header=BB30_8 Depth=2
	addq	$-132, %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	jbe	.LBB30_33
	jmp	.LBB30_35
.LBB30_10:                              # %while.cond.i63
                                        #   in Loop: Header=BB30_4 Depth=1
	cmpq	$61, %rcx
	jne	.LBB30_11
# BB#34:                                # %if.then.i69
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	reg_set_bitmap(%rip), %rdi
	movl	8(%rax), %esi
	callq	bitmap_set_bit
	jmp	.LBB30_35
.LBB30_11:                              # %while.cond.i63
                                        #   in Loop: Header=BB30_4 Depth=1
	cmpq	$66, %rcx
	jne	.LBB30_35
# BB#12:                                # %if.then22.i70
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	%r14, %rdi
	callq	record_last_mem_set_info
.LBB30_35:                              # %if.end23.i76
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$50, %rax
	jne	.LBB30_41
	.align	16, 0x90
.LBB30_36:                              # %if.then25
                                        #   in Loop: Header=BB30_4 Depth=1
	movzbl	3(%r14), %eax
	testq	$4, %rax
	jne	.LBB30_41
.LBB30_37:                              # %if.then.i90
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	%r14, %rdi
	callq	record_last_mem_set_info
.LBB30_41:                              # %for.inc
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	8(%r15), %rax
	incq	%r12
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB30_4
	jmp	.LBB30_42
.LBB30_13:                              # %entry
	cmpq	$47, %rax
	je	.LBB30_19
# BB#14:                                # %entry
	cmpq	$49, %rax
	jne	.LBB30_29
# BB#15:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_16:                              # %while.cond.i98
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %r15
	movzbl	(%r15), %ecx
	leaq	-63(%rcx), %rdx
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jb	.LBB30_16
# BB#17:                                # %while.end.i100
	cmpq	$61, %rcx
	jne	.LBB30_31
# BB#18:                                # %if.then.i103
	movq	reg_set_bitmap(%rip), %rdi
	movl	8(%r15), %esi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	bitmap_set_bit  # TAILCALL
.LBB30_19:                              # %if.then
	leaq	8(%r15), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB30_20
	.align	16, 0x90
.LBB30_26:                              # %while.body.i
                                        #   in Loop: Header=BB30_20 Depth=1
	addq	$8, %rax
.LBB30_20:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	movzbl	(%rax), %edx
	cmpq	$131, %rdx
	jg	.LBB30_25
# BB#21:                                # %while.cond.i
                                        #   in Loop: Header=BB30_20 Depth=1
	leaq	-63(%rdx), %rsi
	andq	%rcx, %rsi
	cmpq	$2, %rsi
	jb	.LBB30_26
	jmp	.LBB30_22
	.align	16, 0x90
.LBB30_25:                              # %while.cond.i
                                        #   in Loop: Header=BB30_20 Depth=1
	addq	$-132, %rdx
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	jbe	.LBB30_26
	jmp	.LBB30_28
.LBB30_22:                              # %while.cond.i
	cmpq	$61, %rdx
	jne	.LBB30_23
# BB#27:                                # %if.then.i
	movq	reg_set_bitmap(%rip), %rdi
	movl	8(%rax), %esi
	callq	bitmap_set_bit
	jmp	.LBB30_28
.LBB30_23:                              # %while.cond.i
	cmpq	$66, %rdx
	jne	.LBB30_28
# BB#24:                                # %if.then22.i
	movq	%r14, %rdi
	callq	record_last_mem_set_info
.LBB30_28:                              # %if.end23.i
	movq	16(%r15), %rax
	movzbl	(%rax), %eax
.LBB30_29:                              # %if.end23.i
	cmpq	$50, %rax
	jne	.LBB30_42
# BB#30:                                # %if.then30.i
	movzbl	3(%r14), %eax
	testq	$4, %rax
	je	.LBB30_31
.LBB30_42:                              # %if.end41
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_31:                              # %if.then.i.i
	movq	%r14, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	record_last_mem_set_info  # TAILCALL
.Ltmp443:
	.size	mark_oprs_set, .Ltmp443-mark_oprs_set
	.cfi_endproc

	.align	16, 0x90
	.type	find_used_regs,@function
find_used_regs:                         # @find_used_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp450:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp451:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp452:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp453:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp454:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp455:
	.cfi_def_cfa_offset 64
.Ltmp456:
	.cfi_offset %rbx, -48
.Ltmp457:
	.cfi_offset %r12, -40
.Ltmp458:
	.cfi_offset %r13, -32
.Ltmp459:
	.cfi_offset %r14, -24
.Ltmp460:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	movq	%rsi, %r14
	jmp	.LBB31_1
.LBB31_15:                              # %if.then23
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	8(%rbx), %rbx
	.align	16, 0x90
.LBB31_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB31_7 Depth 2
                                        #       Child Loop BB31_13 Depth 3
	testq	%rbx, %rbx
	je	.LBB31_10
# BB#2:                                 # %if.end
                                        #   in Loop: Header=BB31_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB31_5
# BB#3:                                 # %if.then4
                                        #   in Loop: Header=BB31_1 Depth=1
	movl	reg_use_count(%rip), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$8, %rdx
	je	.LBB31_10
# BB#4:                                 # %if.end7
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	%rbx, reg_use_table(,%rsi,8)
	incq	%rdx
	movl	%edx, reg_use_count(%rip)
.LBB31_5:                               # %if.end8
                                        #   in Loop: Header=BB31_1 Depth=1
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB31_10
# BB#6:                                 # %for.body.lr.ph
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB31_7:                               # %for.body
                                        #   Parent Loop BB31_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB31_13 Depth 3
	movq	%rdx, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r12), %eax
	leaq	-1(%r12), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB31_8
# BB#11:                                # %for.cond36.preheader
                                        #   in Loop: Header=BB31_7 Depth=2
	movq	(%rbx,%r12,8), %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB31_9
# BB#12:                                #   in Loop: Header=BB31_7 Depth=2
	movq	$8, %r13
	movq	$1, %r15
	.align	16, 0x90
.LBB31_13:                              # %for.body42
                                        #   Parent Loop BB31_1 Depth=1
                                        #     Parent Loop BB31_7 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	addq	%r13, %rdi
	movq	%r14, %rsi
	callq	find_used_regs
	movq	(%rbx,%r12,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$8, %r13
	cmpq	%rax, %rdx
	jl	.LBB31_13
	jmp	.LBB31_9
	.align	16, 0x90
.LBB31_8:                               # %for.body
                                        #   in Loop: Header=BB31_7 Depth=2
	cmpq	$101, %rax
	jne	.LBB31_9
# BB#14:                                # %if.then20
                                        #   in Loop: Header=BB31_7 Depth=2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB31_15
# BB#16:                                # %if.end25
                                        #   in Loop: Header=BB31_7 Depth=2
	leaq	8(%rbx,%rcx,8), %rdi
	movq	%r14, %rsi
	callq	find_used_regs
.LBB31_9:                               # %for.cond.backedge
                                        #   in Loop: Header=BB31_7 Depth=2
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB31_7
.LBB31_10:                              # %for.end53
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp461:
	.size	find_used_regs, .Ltmp461-find_used_regs
	.cfi_endproc

	.align	16, 0x90
	.type	oprs_not_set_p,@function
oprs_not_set_p:                         # @oprs_not_set_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp468:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp469:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp470:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp471:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp472:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp473:
	.cfi_def_cfa_offset 80
.Ltmp474:
	.cfi_offset %rbx, -48
.Ltmp475:
	.cfi_offset %r12, -40
.Ltmp476:
	.cfi_offset %r13, -32
.Ltmp477:
	.cfi_offset %r14, -24
.Ltmp478:
	.cfi_offset %r15, -16
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r12
	testq	%r12, %r12
	movq	$1, %rbx
	je	.LBB32_32
# BB#1:                                 # %if.end.lr.ph
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB32_2:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_9 Depth 2
	movzbl	(%r12), %eax
	leaq	-44(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$25, %rcx
	ja	.LBB32_19
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB32_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$58776579, %rdx         # imm = 0x380DC03
	jne	.LBB32_4
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB32_2 Depth=1
	cmpq	$17, %rcx
	je	.LBB32_16
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB32_2 Depth=1
	cmpq	$22, %rcx
	jne	.LBB32_19
# BB#7:                                 # %sw.bb1
                                        #   in Loop: Header=BB32_2 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movslq	88(%rcx), %rcx
	movq	modify_mem_list(%rip), %rdx
	movq	(%rdx,%rcx,8), %r13
	testq	%r13, %r13
	je	.LBB32_14
# BB#8:                                 #   in Loop: Header=BB32_2 Depth=1
	movq	uid_cuid(%rip), %rcx
	movl	(%rcx,%rax,4), %r14d
	jmp	.LBB32_9
	.align	16, 0x90
.LBB32_13:                              # %while.cond.backedge.us.i.land.lhs.true8.us.i_crit_edge
                                        #   in Loop: Header=BB32_9 Depth=2
	movq	uid_cuid(%rip), %rcx
.LBB32_9:                               # %land.lhs.true8.us.i
                                        #   Parent Loop BB32_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r13), %rax
	movslq	8(%rax), %rdx
	movslq	(%rcx,%rdx,4), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jg	.LBB32_12
# BB#10:                                # %if.end.us.i
                                        #   in Loop: Header=BB32_9 Depth=2
	movzbl	(%rax), %ecx
	xorq	%rbx, %rbx
	cmpq	$34, %rcx
	je	.LBB32_32
# BB#11:                                # %if.end26.us.i
                                        #   in Loop: Header=BB32_9 Depth=2
	movq	%r12, gcse_mem_operand(%rip)
	xorq	%rbx, %rbx
	movb	%bl, gcse_mems_conflict_p(%rip)
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movabsq	$mems_conflict_for_gcse_p, %rsi
	callq	note_stores
	movzbl	gcse_mems_conflict_p(%rip), %eax
	testq	%rax, %rax
	jne	.LBB32_32
.LBB32_12:                              # %while.cond.backedge.us.i
                                        #   in Loop: Header=BB32_9 Depth=2
	movq	16(%r13), %r13
	testq	%r13, %r13
	jne	.LBB32_13
.LBB32_14:                              # %if.else
                                        #   in Loop: Header=BB32_2 Depth=1
	movq	8(%r12), %r12
	testq	%r12, %r12
	jne	.LBB32_2
# BB#15:
	movq	$1, %rbx
	jmp	.LBB32_32
.LBB32_4:
	movq	$1, %rbx
	jmp	.LBB32_32
.LBB32_19:                              # %sw.epilog
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB32_20
# BB#21:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB32_22:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_31 Depth 2
	movq	%rdx, %r13
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB32_23
# BB#26:                                # %for.cond53.preheader
                                        #   in Loop: Header=BB32_22 Depth=1
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r14
	jle	.LBB32_24
	.align	16, 0x90
.LBB32_31:                              # %for.body59
                                        #   Parent Loop BB32_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	oprs_not_set_p
	xorq	%rbx, %rbx
	testq	%r15, %rax
	je	.LBB32_32
# BB#30:                                # %for.cond53
                                        #   in Loop: Header=BB32_31 Depth=2
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB32_31
	jmp	.LBB32_24
	.align	16, 0x90
.LBB32_23:                              # %for.body
                                        #   in Loop: Header=BB32_22 Depth=1
	cmpq	$101, %rax
	jne	.LBB32_24
# BB#27:                                # %if.then28
                                        #   in Loop: Header=BB32_22 Depth=1
	movq	(%r12,%r13,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	oprs_not_set_p
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB32_28
# BB#29:                                # %if.end37
                                        #   in Loop: Header=BB32_22 Depth=1
	xorq	%rbx, %rbx
	testq	%r15, %rax
	je	.LBB32_32
.LBB32_24:                              # %for.cond.backedge
                                        #   in Loop: Header=BB32_22 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB32_22
# BB#25:
	movq	$1, %rbx
	jmp	.LBB32_32
.LBB32_20:
	movq	$1, %rbx
.LBB32_32:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB32_16:                              # %sw.bb12
	movq	reg_set_bitmap(%rip), %rdi
	movl	8(%r12), %esi
	callq	bitmap_bit_p
	testq	%r15, %rax
	jne	.LBB32_17
# BB#18:                                # %sw.bb12
	movabsq	$1, %rbx
	jmp	.LBB32_32
.LBB32_17:
	movabsq	$0, %rbx
	jmp	.LBB32_32
.LBB32_28:
	movq	%rax, %rbx
	jmp	.LBB32_32
.Ltmp479:
	.size	oprs_not_set_p, .Ltmp479-oprs_not_set_p
	.cfi_endproc

	.align	16, 0x90
	.type	try_replace_reg,@function
try_replace_reg:                        # @try_replace_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp486:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp487:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp488:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp489:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp490:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp491:
	.cfi_def_cfa_offset 80
.Ltmp492:
	.cfi_offset %rbx, -48
.Ltmp493:
	.cfi_offset %r12, -40
.Ltmp494:
	.cfi_offset %r13, -32
.Ltmp495:
	.cfi_offset %r14, -24
.Ltmp496:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	find_reg_equal_equiv_note
	movzbl	(%rbx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	movq	%rax, %r14
	xorq	%r15, %r15
	cmpq	$105, %rcx
	movabsq	$0, %r12
	jne	.LBB33_3
# BB#1:                                 # %cond.true
	movq	32(%rbx), %r12
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB33_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	single_set_2
	movq	%rax, %r12
.LBB33_3:                               # %cond.end16
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	validate_replace_src
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB33_11
# BB#4:                                 # %cond.end16
	testq	%r12, %r12
	je	.LBB33_11
# BB#5:                                 # %if.then
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r14, (%rsp)            # 8-byte Spill
	movq	16(%r12), %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %r15
	callq	simplify_replace_rtx
	movq	16(%r12), %rsi
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	rtx_equal_p
	xorq	%r13, %r13
	testq	%r15, %rax
	movq	%r15, %rax
	je	.LBB33_7
# BB#6:
	movq	%r14, %r15
	jmp	.LBB33_10
.LBB33_7:                               # %land.lhs.true30
	leaq	16(%r12), %rsi
	xorq	%rcx, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rdx
	movq	%rax, %rbx
	callq	validate_change
	testq	%rbx, %rax
	movq	%rbx, %rax
	movq	%r14, %r15
	je	.LBB33_8
# BB#9:                                 # %land.lhs.true30
	movabsq	$1, %r13
	jmp	.LBB33_10
.LBB33_8:
	movabsq	$0, %r13
.LBB33_10:                              # %land.lhs.true30
	movq	(%rsp), %r14            # 8-byte Reload
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB33_11:                              # %if.end37
	testq	%rax, %r13
	jne	.LBB33_14
# BB#12:                                # %if.end37
	testq	%r14, %r14
	jne	.LBB33_14
# BB#13:                                # %if.end37
	testq	%r12, %r12
	je	.LBB33_14
# BB#16:                                # %if.end59
	movq	%r15, %rdi
	callq	copy_rtx
	movq	$4, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB33_20
# BB#17:                                # %if.end59.land.lhs.true61_crit_edge
	movq	8(%r14), %rax
	jmp	.LBB33_18
.LBB33_14:                              # %if.else
	testq	%r14, %r14
	je	.LBB33_20
# BB#15:                                # %if.end59.thread101
	movq	8(%r14), %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	simplify_replace_rtx
	movq	%rax, 8(%r14)
.LBB33_18:                              # %land.lhs.true61
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB33_20
# BB#19:                                # %if.then69
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	remove_note
.LBB33_20:                              # %if.end70
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp497:
	.size	try_replace_reg, .Ltmp497-try_replace_reg
	.cfi_endproc

	.align	16, 0x90
	.type	can_disregard_other_sets,@function
can_disregard_other_sets:               # @can_disregard_other_sets
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp504:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp505:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp506:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp507:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp508:
	.cfi_def_cfa_offset 48
.Ltmp509:
	.cfi_offset %rbx, -48
.Ltmp510:
	.cfi_offset %r12, -40
.Ltmp511:
	.cfi_offset %r13, -32
.Ltmp512:
	.cfi_offset %r14, -24
.Ltmp513:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %r12
	movq	%rsi, %r15
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB34_18
# BB#1:                                 # %for.body.lr.ph
	xorq	%r13, %r13
.LBB34_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%r15), %r9
	movq	basic_block_for_insn(%rip), %r11
	movq	8(%r12), %r8
	movq	32(%r11,%r9,8), %rax
	movslq	8(%r8), %rsi
	movq	uid_cuid(%rip), %r10
	movslq	88(%rax), %rbx
	movq	reaching_defs(%rip), %rcx
	movl	(%r10,%rsi,4), %edi
	movq	(%rcx,%rbx,8), %rcx
	movq	%rdi, %rax
	shrq	$6, %rax
	movq	16(%rcx,%rax,8), %rax
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB34_10
# BB#3:                                 # %if.end.i
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	32(%r11,%rsi,8), %rax
	movl	88(%rax), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rax, %rbx
	jne	.LBB34_17
# BB#4:                                 # %if.then34.i
                                        #   in Loop: Header=BB34_2 Depth=1
	movslq	(%r10,%r9,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jge	.LBB34_17
# BB#5:                                 # %if.then46.i
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	32(%r8), %rax
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	je	.LBB34_10
# BB#6:                                 # %if.then46.i
                                        #   in Loop: Header=BB34_2 Depth=1
	cmpq	$47, %rcx
	je	.LBB34_8
# BB#7:                                 # %if.then46.i
                                        #   in Loop: Header=BB34_2 Depth=1
	cmpq	$49, %rcx
	jne	.LBB34_19
.LBB34_8:                               # %def_reaches_here_p.exit
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	8(%rax), %rdi
	movq	24(%r8), %rsi
	movq	%r15, %rdx
	callq	reg_set_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB34_17
# BB#9:                                 # %def_reaches_here_p.exit.if.then_crit_edge
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	8(%r12), %r8
	.align	16, 0x90
.LBB34_10:                              # %if.then
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	32(%r8), %rax
	movzbl	(%rax), %ecx
	xorq	%rbx, %rbx
	cmpq	$39, %rcx
	je	.LBB34_18
# BB#11:                                # %if.then
                                        #   in Loop: Header=BB34_2 Depth=1
	cmpq	$49, %rcx
	je	.LBB34_18
# BB#12:                                # %lor.lhs.false
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	32(%r15), %rcx
	movq	16(%rax), %rdi
	movq	16(%rcx), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB34_18
# BB#13:                                # %if.end29
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB34_16
# BB#14:                                # %if.then31
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	8(%r12), %rax
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	je	.LBB34_18
# BB#15:                                # %if.else
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	32(%r15), %rcx
	movq	16(%rax), %rdi
	movq	16(%rcx), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB34_18
.LBB34_16:                              # %if.end58
                                        #   in Loop: Header=BB34_2 Depth=1
	incq	%r13
	movq	%r12, (%r14)
.LBB34_17:                              # %for.inc
                                        #   in Loop: Header=BB34_2 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	movq	%r13, %rbx
	jne	.LBB34_2
.LBB34_18:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB34_19:                              # %if.else78.i
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.def_reaches_here_p, %rcx
	movq	$3382, %rsi             # imm = 0xD36
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp514:
	.size	can_disregard_other_sets, .Ltmp514-can_disregard_other_sets
	.cfi_endproc

	.align	16, 0x90
	.type	expr_reaches_here_p_work,@function
expr_reaches_here_p_work:               # @expr_reaches_here_p_work
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp521:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp522:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp523:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp524:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp525:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp526:
	.cfi_def_cfa_offset 64
.Ltmp527:
	.cfi_offset %rbx, -48
.Ltmp528:
	.cfi_offset %r12, -40
.Ltmp529:
	.cfi_offset %r13, -32
.Ltmp530:
	.cfi_offset %r14, -24
.Ltmp531:
	.cfi_offset %r15, -16
	movq	%rdx, %r11
	movq	%r11, 8(%rsp)           # 8-byte Spill
	movq	32(%r11), %r15
	movq	%r8, %r12
	movq	%rcx, %r9
	movq	%rdi, (%rsp)            # 8-byte Spill
	jmp	.LBB35_1
	.align	16, 0x90
.LBB35_12:                              # %for.inc
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	(%r15), %r15
.LBB35_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB35_13
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	16(%r15), %rdx
	movslq	88(%rdx), %r10
	movzbl	(%r12,%r10), %eax
	testq	%rax, %rax
	jne	.LBB35_12
# BB#3:                                 # %if.else
                                        #   in Loop: Header=BB35_1 Depth=1
	cmpq	%r11, %rdx
	jne	.LBB35_8
# BB#4:                                 # %if.then3
                                        #   in Loop: Header=BB35_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r9
	je	.LBB35_7
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	ae_gen(%rip), %rax
	movl	8(%rsi), %ecx
	movq	(%rax,%r10,8), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB35_7
# BB#6:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	8(%rdi), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %eax
	movq	%r10, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	movq	$1, %r13
	jne	.LBB35_7
	jmp	.LBB35_14
.LBB35_8:                               # %if.else24
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	ae_kill(%rip), %rcx
	movl	8(%rsi), %r8d
	movq	(%rcx,%r10,8), %rbx
	movq	$6, %rcx
	movq	%r8, %r14
	shrq	%cl, %r14
	movq	16(%rbx,%r14,8), %rax
	movq	$1, %rbx
	movq	%r8, %rcx
	shlq	%cl, %rbx
	testq	%rbx, %rax
	jne	.LBB35_7
# BB#9:                                 # %if.else43
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	ae_gen(%rip), %rax
	movq	(%rax,%r10,8), %rax
	movq	16(%rax,%r14,8), %rax
	testq	%rbx, %rax
	jne	.LBB35_10
# BB#11:                                # %if.else76
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	$1, %r13
	movb	%r13b, (%r12,%r10)
	movq	%r9, %rcx
	movq	%r12, %r8
	movq	%r9, %rbx
	movq	%rsi, %r14
	callq	expr_reaches_here_p_work
	movq	%r14, %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rbx, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	8(%rsp), %r11           # 8-byte Reload
	je	.LBB35_12
	jmp	.LBB35_14
.LBB35_10:                              # %if.then58
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	8(%rdi), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %eax
	movq	%r10, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	movq	$1, %r13
	movq	8(%rsp), %r11           # 8-byte Reload
	je	.LBB35_14
	.align	16, 0x90
.LBB35_7:                               # %if.end
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	$1, %rax
	movb	%al, (%r12,%r10)
	movq	(%r15), %r15
	jmp	.LBB35_1
.LBB35_13:
	xorq	%r13, %r13
.LBB35_14:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp532:
	.size	expr_reaches_here_p_work, .Ltmp532-expr_reaches_here_p_work
	.cfi_endproc

	.align	16, 0x90
	.type	expr_killed_p,@function
expr_killed_p:                          # @expr_killed_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp539:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp540:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp541:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp542:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp543:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp544:
	.cfi_def_cfa_offset 80
.Ltmp545:
	.cfi_offset %rbx, -48
.Ltmp546:
	.cfi_offset %r12, -40
.Ltmp547:
	.cfi_offset %r13, -32
.Ltmp548:
	.cfi_offset %r14, -24
.Ltmp549:
	.cfi_offset %r15, -16
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	testq	%r15, %r15
	movq	$1, %rbx
	je	.LBB36_27
# BB#1:                                 # %if.end.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB36_2:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_9 Depth 2
	movzbl	(%r15), %eax
	leaq	-44(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$25, %rcx
	ja	.LBB36_14
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$58776579, %rdx         # imm = 0x380DC03
	jne	.LBB36_4
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB36_2 Depth=1
	cmpq	$17, %rcx
	je	.LBB36_28
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB36_2 Depth=1
	cmpq	$22, %rcx
	jne	.LBB36_14
# BB#7:                                 # %sw.bb7
                                        #   in Loop: Header=BB36_2 Depth=1
	callq	get_max_uid
	movq	24(%rsp), %rcx          # 8-byte Reload
	movslq	88(%rcx), %rcx
	movq	modify_mem_list(%rip), %rdx
	movq	(%rdx,%rcx,8), %r14
	movq	%rax, %r13
	testq	%r14, %r14
	je	.LBB36_13
# BB#8:                                 #   in Loop: Header=BB36_2 Depth=1
	incq	%r13
	.align	16, 0x90
.LBB36_9:                               # %land.lhs.true8.us.i
                                        #   Parent Loop BB36_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r14), %rax
	movq	uid_cuid(%rip), %rcx
	movslq	8(%rax), %rdx
	movslq	(%rcx,%rdx,4), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jg	.LBB36_12
# BB#10:                                # %if.end.us.i
                                        #   in Loop: Header=BB36_9 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	movq	$1, %rbx
	je	.LBB36_27
# BB#11:                                # %if.end26.us.i
                                        #   in Loop: Header=BB36_9 Depth=2
	movq	%r15, gcse_mem_operand(%rip)
	xorq	%rcx, %rcx
	movb	%cl, gcse_mems_conflict_p(%rip)
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	movabsq	$mems_conflict_for_gcse_p, %rsi
	callq	note_stores
	movzbl	gcse_mems_conflict_p(%rip), %eax
	testq	%rax, %rax
	jne	.LBB36_27
.LBB36_12:                              # %while.cond.backedge.us.i
                                        #   in Loop: Header=BB36_9 Depth=2
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB36_9
.LBB36_13:                              # %if.else
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	8(%r15), %r15
	testq	%r15, %r15
	movq	$1, %rbx
	jne	.LBB36_2
	jmp	.LBB36_27
.LBB36_4:
	xorq	%rbx, %rbx
	jmp	.LBB36_27
.LBB36_14:                              # %sw.epilog
	movq	%r12, %r14
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB36_15
# BB#16:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB36_17:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_26 Depth 2
	movq	%rdx, %r13
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB36_18
# BB#21:                                # %for.cond52.preheader
                                        #   in Loop: Header=BB36_17 Depth=1
	movq	(%r15,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	movq	$1, %r12
	jle	.LBB36_19
	.align	16, 0x90
.LBB36_26:                              # %for.body58
                                        #   Parent Loop BB36_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r12,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	expr_killed_p
	testq	%r14, %rax
	jne	.LBB36_27
# BB#25:                                # %for.cond52
                                        #   in Loop: Header=BB36_26 Depth=2
	movq	(%r15,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB36_26
	jmp	.LBB36_19
	.align	16, 0x90
.LBB36_18:                              # %for.body
                                        #   in Loop: Header=BB36_17 Depth=1
	cmpq	$101, %rax
	jne	.LBB36_19
# BB#22:                                # %if.then26
                                        #   in Loop: Header=BB36_17 Depth=1
	movq	(%r15,%r13,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	expr_killed_p
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%r14, %rcx
	je	.LBB36_23
# BB#24:                                # %if.else35
                                        #   in Loop: Header=BB36_17 Depth=1
	movq	$1, %rbx
	testq	%r14, %rax
	jne	.LBB36_27
.LBB36_19:                              # %for.cond.backedge
                                        #   in Loop: Header=BB36_17 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB36_17
# BB#20:
	xorq	%rbx, %rbx
	jmp	.LBB36_27
.LBB36_15:
	xorq	%rbx, %rbx
	jmp	.LBB36_27
.LBB36_28:                              # %sw.bb
	movq	24(%rsp), %rax          # 8-byte Reload
	movslq	88(%rax), %rax
	movq	reg_set_in_block(%rip), %rdx
	movl	8(%r15), %ecx
	movq	(%rdx,%rax,8), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rbx
	andq	$63, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	jmp	.LBB36_27
.LBB36_23:
	movq	%rax, %rbx
.LBB36_27:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp550:
	.size	expr_killed_p, .Ltmp550-expr_killed_p
	.cfi_endproc

	.type	run_jump_opt_after_gcse,@object # @run_jump_opt_after_gcse
	.local	run_jump_opt_after_gcse
	.comm	run_jump_opt_after_gcse,1,1
	.type	debug_stderr,@object    # @debug_stderr
	.local	debug_stderr
	.comm	debug_stderr,8,8
	.type	gcse_file,@object       # @gcse_file
	.local	gcse_file
	.comm	gcse_file,8,8
	.type	max_gcse_regno,@object  # @max_gcse_regno
	.local	max_gcse_regno
	.comm	max_gcse_regno,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"GCSE disabled: %d > 1000 basic blocks and %d >= 20 edges/basic block"
	.size	.L.str, 69

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"GCSE disabled: %d basic blocks and %d registers"
	.size	.L.str1, 48

	.type	can_copy_init_p,@object # @can_copy_init_p
	.local	can_copy_init_p
	.comm	can_copy_init_p,1,1
	.type	gcse_obstack,@object    # @gcse_obstack
	.local	gcse_obstack
	.comm	gcse_obstack,88,8
	.type	bytes_used,@object      # @bytes_used
	.local	bytes_used
	.comm	bytes_used,4,4
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"GCSE pass %d\n\n"
	.size	.L.str2, 15

	.type	modify_mem_list,@object # @modify_mem_list
	.local	modify_mem_list
	.comm	modify_mem_list,8,8
	.type	canon_modify_mem_list,@object # @canon_modify_mem_list
	.local	canon_modify_mem_list
	.comm	canon_modify_mem_list,8,8
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"GCSE of %s: %d basic blocks, "
	.size	.L.str4, 30

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%d pass%s, %d bytes\n\n"
	.size	.L.str5, 22

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"es"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.zero	1
	.size	.L.str7, 1

	.type	modify_mem_list_set,@object # @modify_mem_list_set
	.comm	modify_mem_list_set,8,8
	.type	canon_modify_mem_list_set,@object # @canon_modify_mem_list_set
	.comm	canon_modify_mem_list_set,8,8
	.type	max_cuid,@object        # @max_cuid
	.local	max_cuid
	.comm	max_cuid,4,4
	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Code Hosting Expressions"
	.size	.L.str8, 25

	.type	expr_hash_table,@object # @expr_hash_table
	.local	expr_hash_table
	.comm	expr_hash_table,8,8
	.type	expr_hash_table_size,@object # @expr_hash_table_size
	.local	expr_hash_table_size
	.comm	expr_hash_table_size,4,4
	.type	n_exprs,@object         # @n_exprs
	.local	n_exprs
	.comm	n_exprs,4,4
	.type	antloc,@object          # @antloc
	.local	antloc
	.comm	antloc,8,8
	.type	transp,@object          # @transp
	.local	transp
	.comm	transp,8,8
	.type	comp,@object            # @comp
	.local	comp
	.comm	comp,8,8
	.type	hoist_vbein,@object     # @hoist_vbein
	.local	hoist_vbein
	.comm	hoist_vbein,8,8
	.type	hoist_vbeout,@object    # @hoist_vbeout
	.local	hoist_vbeout
	.comm	hoist_vbeout,8,8
	.type	hoist_exprs,@object     # @hoist_exprs
	.local	hoist_exprs
	.comm	hoist_exprs,8,8
	.type	transpout,@object       # @transpout
	.local	transpout
	.comm	transpout,8,8
	.type	dominators,@object      # @dominators
	.local	dominators
	.comm	dominators,8,8
	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"gcse.c"
	.size	.L.str9, 7

	.type	.L__FUNCTION__.hoist_code,@object # @__FUNCTION__.hoist_code
.L__FUNCTION__.hoist_code:
	.asciz	"hoist_code"
	.size	.L__FUNCTION__.hoist_code, 11

	.type	gcse_create_count,@object # @gcse_create_count
	.local	gcse_create_count
	.comm	gcse_create_count,4,4
	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"PRE/HOIST: end of bb %d, insn %d, "
	.size	.L.str10, 35

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"copying expression %d to reg %d\n"
	.size	.L.str11, 33

	.type	reg_set_table_size,@object # @reg_set_table_size
	.local	reg_set_table_size
	.comm	reg_set_table_size,4,4
	.type	reg_set_table,@object   # @reg_set_table
	.local	reg_set_table
	.comm	reg_set_table,8,8
	.type	reg_set_obstack,@object # @reg_set_obstack
	.local	reg_set_obstack
	.comm	reg_set_obstack,88,8
	.type	.L__FUNCTION__.process_insert_insn,@object # @__FUNCTION__.process_insert_insn
.L__FUNCTION__.process_insert_insn:
	.asciz	"process_insert_insn"
	.size	.L__FUNCTION__.process_insert_insn, 20

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"hoisting vbeinout computation: %d passes\n"
	.size	.L.str12, 42

	.type	set_hash_table_size,@object # @set_hash_table_size
	.local	set_hash_table_size
	.comm	set_hash_table_size,4,4
	.type	set_hash_table,@object  # @set_hash_table
	.local	set_hash_table
	.comm	set_hash_table,8,8
	.type	reg_set_in_block,@object # @reg_set_in_block
	.local	reg_set_in_block
	.comm	reg_set_in_block,8,8
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"%s hash table (%d buckets, %d entries)\n"
	.size	.L.str13, 40

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Index %d (hash value %d)\n  "
	.size	.L.str14, 28

	.type	reg_avail_info,@object  # @reg_avail_info
	.local	reg_avail_info
	.comm	reg_avail_info,8,8
	.type	current_bb,@object      # @current_bb
	.local	current_bb
	.comm	current_bb,4,4
	.type	can_copy_p,@object      # @can_copy_p
	.local	can_copy_p
	.comm	can_copy_p,59,16
	.type	.L__FUNCTION__.insert_set_in_table,@object # @__FUNCTION__.insert_set_in_table
.L__FUNCTION__.insert_set_in_table:
	.asciz	"insert_set_in_table"
	.size	.L__FUNCTION__.insert_set_in_table, 20

	.type	n_sets,@object          # @n_sets
	.local	n_sets
	.comm	n_sets,4,4
	.type	.L__FUNCTION__.expr_equiv_p,@object # @__FUNCTION__.expr_equiv_p
.L__FUNCTION__.expr_equiv_p:
	.asciz	"expr_equiv_p"
	.size	.L__FUNCTION__.expr_equiv_p, 13

	.type	.L__FUNCTION__.hash_expr_1,@object # @__FUNCTION__.hash_expr_1
.L__FUNCTION__.hash_expr_1:
	.asciz	"hash_expr_1"
	.size	.L__FUNCTION__.hash_expr_1, 12

	.type	uid_cuid,@object        # @uid_cuid
	.local	uid_cuid
	.comm	uid_cuid,8,8
	.type	gcse_mem_operand,@object # @gcse_mem_operand
	.local	gcse_mem_operand
	.comm	gcse_mem_operand,8,8
	.type	gcse_mems_conflict_p,@object # @gcse_mems_conflict_p
	.local	gcse_mems_conflict_p
	.comm	gcse_mems_conflict_p,1,1
	.type	pre_ldst_mems,@object   # @pre_ldst_mems
	.local	pre_ldst_mems
	.comm	pre_ldst_mems,8,8
	.type	want_to_gcse_p.test_insn,@object # @want_to_gcse_p.test_insn
	.local	want_to_gcse_p.test_insn
	.comm	want_to_gcse_p.test_insn,8,8
	.type	gcse_subst_count,@object # @gcse_subst_count
	.local	gcse_subst_count
	.comm	gcse_subst_count,4,4
	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Expression"
	.size	.L.str15, 11

	.type	edge_list,@object       # @edge_list
	.local	edge_list
	.comm	edge_list,8,8
	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"\nPRE GCSE of %s, pass %d: %d bytes needed, "
	.size	.L.str16, 44

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"%d substs, %d insns created\n"
	.size	.L.str17, 29

	.type	pre_insert_map,@object  # @pre_insert_map
	.local	pre_insert_map
	.comm	pre_insert_map,8,8
	.type	pre_delete_map,@object  # @pre_delete_map
	.local	pre_delete_map
	.comm	pre_delete_map,8,8
	.type	ae_in,@object           # @ae_in
	.local	ae_in
	.comm	ae_in,8,8
	.type	ae_out,@object          # @ae_out
	.local	ae_out
	.comm	ae_out,8,8
	.type	pre_redundant_insns,@object # @pre_redundant_insns
	.local	pre_redundant_insns
	.comm	pre_redundant_insns,8,8
	.type	.L__FUNCTION__.pre_insert_copy_insn,@object # @__FUNCTION__.pre_insert_copy_insn
.L__FUNCTION__.pre_insert_copy_insn:
	.asciz	"pre_insert_copy_insn"
	.size	.L__FUNCTION__.pre_insert_copy_insn, 21

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"PRE: bb %d, insn %d, copy expression %d in insn %d to reg %d\n"
	.size	.L.str18, 62

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"PRE:  store updated with reaching reg "
	.size	.L.str19, 39

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	":\n\t"
	.size	.L.str20, 4

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"PRE/HOIST: edge (%d,%d), "
	.size	.L.str21, 26

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"copy expression %d\n"
	.size	.L.str22, 20

	.type	.L__FUNCTION__.pre_delete,@object # @__FUNCTION__.pre_delete
.L__FUNCTION__.pre_delete:
	.asciz	"pre_delete"
	.size	.L__FUNCTION__.pre_delete, 11

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"PRE: redundant insn %d (expression %d) in "
	.size	.L.str23, 43

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"bb %d, reaching reg is %d\n"
	.size	.L.str24, 27

	.type	ae_kill,@object         # @ae_kill
	.local	ae_kill
	.comm	ae_kill,8,8
	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"LDST list: \n"
	.size	.L.str25, 13

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"  Pattern (%3d): "
	.size	.L.str26, 18

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"\n\t Loads : "
	.size	.L.str27, 12

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"(nil)"
	.size	.L.str28, 6

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"\n\tStores : "
	.size	.L.str29, 12

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"\n\n"
	.size	.L.str30, 3

	.type	const_prop_count,@object # @const_prop_count
	.local	const_prop_count
	.comm	const_prop_count,4,4
	.type	copy_prop_count,@object # @copy_prop_count
	.local	copy_prop_count
	.comm	copy_prop_count,4,4
	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"SET"
	.size	.L.str31, 4

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"CPROP of %s, pass %d: %d bytes needed, "
	.size	.L.str32, 40

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"%d const props, %d copy props\n\n"
	.size	.L.str33, 32

	.type	cprop_pavloc,@object    # @cprop_pavloc
	.local	cprop_pavloc
	.comm	cprop_pavloc,8,8
	.type	cprop_absaltered,@object # @cprop_absaltered
	.local	cprop_absaltered
	.comm	cprop_absaltered,8,8
	.type	cprop_avin,@object      # @cprop_avin
	.local	cprop_avin
	.comm	cprop_avin,8,8
	.type	cprop_avout,@object     # @cprop_avout
	.local	cprop_avout
	.comm	cprop_avout,8,8
	.type	reg_set_bitmap,@object  # @reg_set_bitmap
	.local	reg_set_bitmap
	.comm	reg_set_bitmap,8,8
	.type	reg_use_count,@object   # @reg_use_count
	.local	reg_use_count
	.comm	reg_use_count,4,4
	.type	reg_use_table,@object   # @reg_use_table
	.local	reg_use_table
	.comm	reg_use_table,64,16
	.type	.L__FUNCTION__.cprop_insn,@object # @__FUNCTION__.cprop_insn
.L__FUNCTION__.cprop_insn:
	.asciz	"cprop_insn"
	.size	.L__FUNCTION__.cprop_insn, 11

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"CONST-PROP: Replacing reg %d in "
	.size	.L.str34, 33

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"insn %d with constant "
	.size	.L.str35, 23

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"COPY-PROP: Replacing reg %d in insn %d"
	.size	.L.str36, 39

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	" with reg %d\n"
	.size	.L.str37, 14

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"CONST-PROP: Replacing reg %d in insn %d with constant "
	.size	.L.str38, 55

	.type	.L__FUNCTION__.find_avail_set,@object # @__FUNCTION__.find_avail_set
.L__FUNCTION__.find_avail_set:
	.asciz	"find_avail_set"
	.size	.L__FUNCTION__.find_avail_set, 15

	.type	ae_gen,@object          # @ae_gen
	.local	ae_gen
	.comm	ae_gen,8,8
	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"GCSE of %s, pass %d: %d bytes needed, %d substs,"
	.size	.L.str39, 49

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"%d insns created\n"
	.size	.L.str40, 18

	.type	rd_kill,@object         # @rd_kill
	.local	rd_kill
	.comm	rd_kill,8,8
	.type	rd_gen,@object          # @rd_gen
	.local	rd_gen
	.comm	rd_gen,8,8
	.type	reaching_defs,@object   # @reaching_defs
	.local	reaching_defs
	.comm	reaching_defs,8,8
	.type	rd_out,@object          # @rd_out
	.local	rd_out
	.comm	rd_out,8,8
	.type	.L__FUNCTION__.handle_avail_expr,@object # @__FUNCTION__.handle_avail_expr
.L__FUNCTION__.handle_avail_expr:
	.asciz	"handle_avail_expr"
	.size	.L__FUNCTION__.handle_avail_expr, 18

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"GCSE: Replacing the source in insn %d with"
	.size	.L.str41, 43

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	" reg %d %s insn %d\n"
	.size	.L.str42, 20

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"from"
	.size	.L.str43, 5

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"set in"
	.size	.L.str44, 7

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"GCSE: Creating insn %d to copy value of reg %d"
	.size	.L.str45, 47

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	", computed in insn %d,\n"
	.size	.L.str46, 24

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"      into newly allocated reg %d\n"
	.size	.L.str47, 35

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"GCSE: Replacing the source in insn %d with reg %d "
	.size	.L.str48, 51

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"set in insn %d\n"
	.size	.L.str49, 16

	.type	.L__FUNCTION__.def_reaches_here_p,@object # @__FUNCTION__.def_reaches_here_p
.L__FUNCTION__.def_reaches_here_p:
	.asciz	"def_reaches_here_p"
	.size	.L__FUNCTION__.def_reaches_here_p, 19

	.type	.L__FUNCTION__.computing_insn,@object # @__FUNCTION__.computing_insn
.L__FUNCTION__.computing_insn:
	.asciz	"computing_insn"
	.size	.L__FUNCTION__.computing_insn, 15

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"reaching def computation: %d passes\n"
	.size	.L.str50, 37

	.type	cuid_insn,@object       # @cuid_insn
	.local	cuid_insn
	.comm	cuid_insn,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
