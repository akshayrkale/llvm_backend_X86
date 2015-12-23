	.file	"regmove.c.bc"
	.text
	.globl	regmove_optimize
	.align	16, 0x90
	.type	regmove_optimize,@function
regmove_optimize:                       # @regmove_optimize
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
	subq	$672, %rsp              # imm = 0x2A0
.Ltmp11:
	.cfi_def_cfa_offset 720
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
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	get_max_uid
	movl	flag_non_call_exceptions(%rip), %ecx
	movq	%rax, %r12
	movq	%r12, 152(%rsp)         # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB0_604
# BB#1:                                 # %if.end
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	%r14, 136(%rsp)         # 8-byte Spill
	movl	word_mode(%rip), %edi
	movq	$10000, %rsi            # imm = 0x2710
	callq	gen_rtx_REG
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$2, %rsi
	xorq	%rbx, %rbx
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_add3_insn
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_19
# BB#2:                                 # %if.end
	cmpq	$39, %rcx
	jne	.LBB0_13
# BB#3:                                 # %if.then6.i
	movq	8(%rax), %rax
	movl	(%rax), %ecx
	cmpq	$2, %rcx
	jne	.LBB0_4
# BB#5:                                 # %if.end.i
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	jne	.LBB0_6
# BB#7:                                 # %if.end18.i
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB0_8
# BB#14:                                # %discover_flags_reg.exit
	movl	8(%rdi), %eax
	cmpq	$53, %rax
	jb	.LBB0_16
# BB#15:                                # %select.mid21
	xorq	%rdi, %rdi
.LBB0_16:                               # %select.end20
	testq	%rdi, %rdi
	je	.LBB0_18
# BB#17:                                # %select.end20
	movq	global_rtl(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB0_18
# BB#67:                                # %if.end.i1204
	movl	8(%rdi), %esi
	movq	%rsi, 176(%rsp)         # 8-byte Spill
	movzbl	2(%rdi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	je	.LBB0_71
# BB#68:                                # %if.end.i1204
	leaq	-21(%rsi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB0_71
# BB#69:                                # %if.end.i1204
	leaq	-45(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB0_71
# BB#70:                                # %if.end.i1204
	leaq	-29(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB0_71
# BB#76:                                # %cond.false.i
	cmpq	$24, %rax
	jne	.LBB0_77
# BB#79:                                # %cond.true42.i
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB0_81
.LBB0_13:                               # %if.end47.i
	movq	global_rtl(%rip), %rbx
	jmp	.LBB0_19
.LBB0_4:                                # %if.then9.i
	movq	global_rtl(%rip), %rbx
	jmp	.LBB0_19
.LBB0_6:                                # %if.then17.i
	movq	global_rtl(%rip), %rbx
	jmp	.LBB0_19
.LBB0_8:                                # %if.end18.i
	cmpq	$63, %rax
	jne	.LBB0_19
# BB#9:                                 # %land.lhs.true.i
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_19
# BB#10:                                # %land.lhs.true30.i
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jae	.LBB0_12
# BB#11:
	movq	global_rtl(%rip), %rbx
	jmp	.LBB0_19
.LBB0_18:
	movq	%rdi, %rbx
	jmp	.LBB0_19
.LBB0_71:                               # %cond.true.i
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB0_81
# BB#72:                                # %lor.rhs.i
	movq	%rsi, %rbx
	cmpq	$6, %rcx
	jne	.LBB0_73
# BB#74:                                # %lor.rhs.i
	movabsq	$1, %rax
	jmp	.LBB0_75
.LBB0_77:                               # %cond.false.i
	cmpq	$18, %rax
	jne	.LBB0_80
# BB#78:                                # %cond.true34.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB0_81
.LBB0_12:                               # %select.mid
	xorq	%rbx, %rbx
.LBB0_19:                               # %if.then.i
	testq	%rbx, %rbx
	je	.LBB0_21
# BB#20:                                # %if.then.i
	movq	$196608, %rbx           # imm = 0x30000
.LBB0_21:                               # %if.then.i
	callq	get_insns
	testq	%rax, %rax
	je	.LBB0_24
# BB#22:
	movabsq	$4278255615, %rcx       # imm = 0xFF00FFFF
	.align	16, 0x90
.LBB0_23:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %edx
	andq	%rcx, %rdx
	orq	%rbx, %rdx
	movl	%edx, (%rax)
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB0_23
	jmp	.LBB0_24
.LBB0_73:
	movabsq	$0, %rax
.LBB0_75:                               # %lor.rhs.i
	incq	%rax
	movq	%rbx, %rsi
	jmp	.LBB0_81
.LBB0_80:                               # %cond.false46.i
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB0_81:                               # %cond.end61.i
	movslq	n_basic_blocks(%rip), %rcx
	movq	%rdi, flags_set_1_rtx(%rip)
	testq	%rcx, %rcx
	jle	.LBB0_24
# BB#82:                                # %for.body67.lr.ph.i
	movq	%rcx, %rdi
	movq	$32, %rcx
	movq	%rax, %rdx
                                        # kill: RAX<def> RDX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_83
# BB#84:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	movq	%rdi, %rcx
.LBB0_101:                              # %for.body79.lr.ph.us.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_103 Depth 2
                                        #     Child Loop BB0_86 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rcx,8), %rax
	movq	(%rax), %r13
	movq	8(%rax), %rdx
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	64(%rax), %rdi
	leaq	-1(%rcx), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rcx, %r12
	callq	bitmap_bit_p
	movq	%rax, %r14
	cmpq	$1, 160(%rsp)           # 8-byte Folded Reload
	je	.LBB0_85
# BB#102:                               #   in Loop: Header=BB0_101 Depth=1
	movq	$1, %rbx
	movq	%r12, 168(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB0_103:                              # %for.body79.us.for.body79.us_crit_edge.i
                                        #   Parent Loop BB0_101 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	basic_block_info(%rip), %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rax
	movq	64(%rax), %rdi
	movq	%r14, %r12
	movq	176(%rsp), %rax         # 8-byte Reload
	leaq	(%rbx,%rax), %rsi
	callq	bitmap_bit_p
	movq	%rax, %r14
	incq	%rbx
	orq	%r12, %r14
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB0_103
.LBB0_85:                               #   in Loop: Header=BB0_101 Depth=1
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB0_86
	.align	16, 0x90
.LBB0_99:                               # %if.end129.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	24(%r13), %r13
	movq	%r12, %r14
.LBB0_86:                               # %while.body.us.i
                                        #   Parent Loop BB0_101 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_87
# BB#90:                                # %if.then95.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	xorq	%r12, %r12
	testq	%rdx, %r14
	je	.LBB0_94
# BB#91:                                # %land.lhs.true97.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	%rdx, %rbx
	movq	$1, %rsi
	movq	%r13, %rdi
	movq	176(%rsp), %rdx         # 8-byte Reload
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB0_93
# BB#92:                                # %land.lhs.true97.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	xorq	%r14, %r14
.LBB0_93:                               # %land.lhs.true97.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movl	(%r13), %eax
	movq	%r14, %r12
	movq	%rbx, %rdx
.LBB0_94:                               # %if.end101.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	%r12, %rcx
	andq	%rdx, %rcx
	movq	%rdx, %rbx
	je	.LBB0_96
# BB#95:                                # %if.end101.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	$196608, %rcx           # imm = 0x30000
.LBB0_96:                               # %if.end101.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, (%r13)
	xorq	%rax, %rax
	movb	%al, flags_set_1_set(%rip)
	movq	32(%r13), %rdi
	xorq	%rdx, %rdx
	movabsq	$flags_set_1, %rsi
	callq	note_stores
	movzbl	flags_set_1_set(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_98
# BB#97:                                # %if.then113.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	$2, %rax
	movb	%al, 2(%r13)
	movq	$1, %r12
	jmp	.LBB0_98
	.align	16, 0x90
.LBB0_87:                               # %if.else.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	%r14, %rcx
	andq	%rdx, %rcx
	je	.LBB0_89
# BB#88:                                # %if.else.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	$196608, %rcx           # imm = 0x30000
.LBB0_89:                               # %if.else.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	%rdx, %rbx
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, (%r13)
	movq	%r14, %r12
.LBB0_98:                               # %if.end125.us.i
                                        #   in Loop: Header=BB0_86 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r13
	movq	%rbx, %rdx
	jne	.LBB0_99
# BB#100:                               # %for.cond64.loopexit.us.i
                                        #   in Loop: Header=BB0_101 Depth=1
	movq	$32, %rcx
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %rcx
	movq	152(%rsp), %r12         # 8-byte Reload
	movq	176(%rsp), %rsi         # 8-byte Reload
	jg	.LBB0_101
	jmp	.LBB0_24
.LBB0_83:
	movq	%rdi, %rdx
.LBB0_105:                              # %for.body67.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_106 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdx,8), %r12
	movq	8(%r12), %r14
	decq	%rdx
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	jmp	.LBB0_106
	.align	16, 0x90
.LBB0_119:                              # %if.end129.i
                                        #   in Loop: Header=BB0_106 Depth=2
	addq	$24, %r12
	movq	%r13, %rbx
.LBB0_106:                              # %while.body.i
                                        #   Parent Loop BB0_105 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r12), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB0_115
# BB#107:                               # %if.then95.i
                                        #   in Loop: Header=BB0_106 Depth=2
	xorq	%r13, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	je	.LBB0_111
# BB#108:                               # %land.lhs.true97.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	176(%rsp), %rdx         # 8-byte Reload
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB0_110
# BB#109:                               # %land.lhs.true97.i
                                        #   in Loop: Header=BB0_106 Depth=2
	xorq	%rbx, %rbx
.LBB0_110:                              # %land.lhs.true97.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movl	(%r12), %eax
	movq	%rbx, %r13
.LBB0_111:                              # %if.end101.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	%r13, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	je	.LBB0_113
# BB#112:                               # %if.end101.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	$196608, %rcx           # imm = 0x30000
.LBB0_113:                              # %if.end101.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, (%r12)
	xorq	%rax, %rax
	movb	%al, flags_set_1_set(%rip)
	movq	32(%r12), %rdi
	xorq	%rdx, %rdx
	movabsq	$flags_set_1, %rsi
	callq	note_stores
	movzbl	flags_set_1_set(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_118
# BB#114:                               # %if.then113.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	$2, %rax
	movb	%al, 2(%r12)
	movq	$1, %r13
	jmp	.LBB0_118
	.align	16, 0x90
.LBB0_115:                              # %if.else.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	%rbx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	je	.LBB0_117
# BB#116:                               # %if.else.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	$196608, %rcx           # imm = 0x30000
.LBB0_117:                              # %if.else.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, (%r12)
	movq	%rbx, %r13
.LBB0_118:                              # %if.end125.i
                                        #   in Loop: Header=BB0_106 Depth=2
	cmpq	%r14, %r12
	jne	.LBB0_119
# BB#104:                               # %for.cond64.loopexit.i
                                        #   in Loop: Header=BB0_105 Depth=1
	movq	$32, %rcx
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	152(%rsp), %r12         # 8-byte Reload
	jg	.LBB0_105
.LBB0_24:                               # %mark_flags_life_zones.exit
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$2, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	movq	%rax, regno_src_regno(%rip)
	movq	136(%rsp), %r14         # 8-byte Reload
	jle	.LBB0_27
# BB#25:                                # %for.body.preheader
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	decq	%r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_26:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, (%rax,%r15,4)
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%r15
	testq	%rsi, %rsi
	jg	.LBB0_26
.LBB0_27:                               # %for.end
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	1(%r12), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	movq	%rax, regmove_bb_head(%rip)
	js	.LBB0_30
# BB#28:                                # %for.body10.lr.ph
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_29:                               # %for.body10
                                        # =>This Inner Loop Header: Depth=1
	movl	%esi, (%rax,%rdx,4)
	movq	$32, %rcx
	leaq	-1(%rdx), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rdi, %rdx
	jg	.LBB0_29
.LBB0_30:                               # %for.cond15.preheader
	movslq	n_basic_blocks(%rip), %rcx
	testq	%rcx, %rcx
	jle	.LBB0_33
# BB#31:                                # %for.body18.lr.ph
	movq	basic_block_info(%rip), %rdx
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_32:                               # %for.body18
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdx,%rbx,8), %rcx
	movq	(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movl	%ebx, (%rax,%rcx,4)
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rdi
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	jl	.LBB0_32
.LBB0_33:                               # %for.cond26.preheader
	xorq	%r8, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_34:                               # %for.body29
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_42 Depth 2
                                        #       Child Loop BB0_57 Depth 3
                                        #       Child Loop BB0_127 Depth 3
                                        #       Child Loop BB0_146 Depth 3
                                        #       Child Loop BB0_160 Depth 3
                                        #       Child Loop BB0_198 Depth 3
                                        #       Child Loop BB0_212 Depth 3
                                        #       Child Loop BB0_233 Depth 3
                                        #         Child Loop BB0_293 Depth 4
                                        #         Child Loop BB0_314 Depth 4
                                        #       Child Loop BB0_362 Depth 3
                                        #       Child Loop BB0_386 Depth 3
	movq	%r8, 160(%rsp)          # 8-byte Spill
	movl	flag_regmove(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_36
# BB#35:                                # %for.body29
                                        #   in Loop: Header=BB0_34 Depth=1
	movl	flag_expensive_optimizations(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB0_603
.LBB0_36:                               # %if.end34
                                        #   in Loop: Header=BB0_34 Depth=1
	testq	%r14, %r14
	je	.LBB0_40
# BB#37:                                # %if.then36
                                        #   in Loop: Header=BB0_34 Depth=1
	testq	%rbx, %r8
	movq	%r8, %r15
	movabsq	$.L.str1, %rdx
	jne	.LBB0_39
# BB#38:                                # %if.then36
                                        #   in Loop: Header=BB0_34 Depth=1
	movabsq	$.L.str2, %rdx
.LBB0_39:                               # %if.then36
                                        #   in Loop: Header=BB0_34 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str, %rsi
	callq	fprintf
	movq	%r15, %r8
.LBB0_40:                               # %if.end39
                                        #   in Loop: Header=BB0_34 Depth=1
	testq	%rbx, %r8
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, %r13
	je	.LBB0_42
# BB#41:                                # %cond.true
                                        #   in Loop: Header=BB0_34 Depth=1
	movq	%r8, %r14
	callq	get_last_insn
	movq	%r14, %r8
	movq	%rax, %r13
	jmp	.LBB0_42
	.align	16, 0x90
.LBB0_423:                              #   in Loop: Header=BB0_42 Depth=2
	addq	$16, %r13
	movq	(%r13), %r13
.LBB0_42:                               # %for.cond43.preheader
                                        #   Parent Loop BB0_34 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_57 Depth 3
                                        #       Child Loop BB0_127 Depth 3
                                        #       Child Loop BB0_146 Depth 3
                                        #       Child Loop BB0_160 Depth 3
                                        #       Child Loop BB0_198 Depth 3
                                        #       Child Loop BB0_212 Depth 3
                                        #       Child Loop BB0_233 Depth 3
                                        #         Child Loop BB0_293 Depth 4
                                        #         Child Loop BB0_314 Depth 4
                                        #       Child Loop BB0_362 Depth 3
                                        #       Child Loop BB0_386 Depth 3
	testq	%r13, %r13
	je	.LBB0_426
# BB#43:                                # %for.body45
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r13, 176(%rsp)         # 8-byte Spill
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_422
# BB#44:                                # %cond.true51
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	32(%r13), %r9
	movzbl	(%r9), %eax
	cmpq	$47, %rax
	je	.LBB0_46
# BB#45:                                # %cond.false62
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r13, %rdi
	movq	%r9, %rsi
	movq	%r8, %r14
	callq	single_set_2
	movq	%r14, %r8
	movq	%rax, %r9
.LBB0_46:                               # %cond.end70
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%r9, %r9
	je	.LBB0_422
# BB#47:                                # %if.end74
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_135
# BB#48:                                # %if.end74
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%rbx, %r8
	jne	.LBB0_135
# BB#49:                                # %land.lhs.true78
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%r9), %r12
	movl	(%r12), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$120, %rax
	jne	.LBB0_135
# BB#50:                                # %land.lhs.true93
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	8(%r12), %r14
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB0_135
# BB#51:                                # %land.lhs.true104
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	8(%r9), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_135
# BB#52:                                # %if.then112
                                        #   in Loop: Header=BB0_42 Depth=2
	movslq	8(%r14), %r15
	cmpq	$53, %r15
	jl	.LBB0_135
# BB#53:                                # %if.then112
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	8(%rax), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB0_135
# BB#54:                                # %lor.lhs.false7.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$1, %rsi
	movq	%r13, %rdi
	movq	%r14, %rdx
	movq	%r8, %rbx
	movq	%r9, 144(%rsp)          # 8-byte Spill
	callq	find_reg_note
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%rbx, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rax, %rax
	je	.LBB0_135
# BB#55:                                # %lor.lhs.false8.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movl	24(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_135
# BB#56:                                # %lor.lhs.false11.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	12(%rax), %eax
	cmpq	$1, %rax
	movq	%r13, %rsi
	jne	.LBB0_135
.LBB0_57:                               # %for.cond.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB0_135
# BB#58:                                # %land.rhs.i1216
                                        #   in Loop: Header=BB0_57 Depth=3
	movq	%r14, %rdi
	movq	%rsi, 184(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	callq	reg_set_p
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	movq	184(%rsp), %rsi         # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB0_64
# BB#59:                                # %for.body.i1217
                                        #   in Loop: Header=BB0_57 Depth=3
	movzbl	(%rsi), %eax
	cmpq	$36, %rax
	je	.LBB0_64
# BB#60:                                # %for.body.i1217
                                        #   in Loop: Header=BB0_57 Depth=3
	cmpq	$33, %rax
	je	.LBB0_64
# BB#61:                                # %for.body.i1217
                                        #   in Loop: Header=BB0_57 Depth=3
	cmpq	$34, %rax
	jne	.LBB0_63
# BB#62:                                # %sw.bb1.i.i
                                        #   in Loop: Header=BB0_57 Depth=3
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_64
.LBB0_63:                               # %perhaps_ends_bb_p.exit.i
                                        #   in Loop: Header=BB0_57 Depth=3
	movq	%rsi, %rdi
	movq	%r8, %r15
	callq	can_throw_internal
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	movq	184(%rsp), %rsi         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB0_57
.LBB0_64:                               # %if.end32.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movzbl	(%rsi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_135
# BB#65:                                # %cond.true.i1220
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	32(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_120
# BB#66:                                #   in Loop: Header=BB0_42 Depth=2
	movq	%rsi, %r15
	jmp	.LBB0_121
.LBB0_120:                              # %cond.false.i1221
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%rsi, %rdi
	movq	%rsi, %r15
	movq	%rax, %rsi
	movq	%r8, %rbx
	movq	%r9, 144(%rsp)          # 8-byte Spill
	callq	single_set_2
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%rbx, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB0_121:                              # %cond.end53.i
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%rax, %rax
	movq	%r15, %rdi
	je	.LBB0_135
# BB#122:                               # %lor.lhs.false56.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$66, %rcx
	jne	.LBB0_135
# BB#123:                               # %lor.lhs.false64.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$3, %rsi
	movq	%rax, 168(%rsp)         # 8-byte Spill
	xorq	%rdx, %rdx
	movq	%rdi, 184(%rsp)         # 8-byte Spill
	movq	%r9, %r15
	callq	find_reg_note
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %r9
	movq	184(%rsp), %r15         # 8-byte Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB0_135
# BB#124:                               # %lor.lhs.false67.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	8(%rsi), %rax
	cmpq	%r14, %rax
	jne	.LBB0_135
# BB#125:                               # %if.end74.i
                                        #   in Loop: Header=BB0_42 Depth=2
	addq	$16, %rsi
	movq	(%rsi), %rax
	movzbl	3(%rax), %eax
	testq	$8, %rax
	jne	.LBB0_135
# BB#126:                               # %if.end82.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r9, 144(%rsp)          # 8-byte Spill
	movl	(%r12), %eax
	movzbl	2(%r14), %ebx
	movq	%rbx, 168(%rsp)         # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rax
	movb	%al, 2(%r14)
	movq	(%rsi), %rax
	movq	%rax, 8(%r12)
	movq	$1, %rcx
	movq	%r15, %rdi
	movq	%r12, %rdx
	callq	validate_change
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_lowpart_SUBREG
	movq	24(%r15), %r15
	movq	%rax, %rbx
	jmp	.LBB0_127
.LBB0_130:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB0_127 Depth=3
	movq	24(%r15), %r15
.LBB0_127:                              # %if.end82.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r13, %r15
	je	.LBB0_131
# BB#128:                               # %while.body.i1223
                                        #   in Loop: Header=BB0_127 Depth=3
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_130
# BB#129:                               # %if.end131.i
                                        #   in Loop: Header=BB0_127 Depth=3
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	validate_replace_rtx_group
	jmp	.LBB0_130
.LBB0_131:                              # %while.end.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	validate_replace_rtx_group
	callq	apply_change_group
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB0_133
# BB#132:                               # %if.then134.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movb	%al, 2(%r14)
	movq	%r14, 8(%r12)
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	jmp	.LBB0_135
.LBB0_133:                              # %if.else.i1224
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	je	.LBB0_135
# BB#134:                               # %if.then145.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%r8, %r14
	movq	%r9, %r15
	callq	remove_note
	movq	%r15, %r9
	movq	%r14, %r8
.LBB0_135:                              # %if.end119
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_229
# BB#136:                               # %if.end119
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%rbx, %r8
	jne	.LBB0_229
# BB#137:                               # %land.lhs.true123
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%r9), %rdx
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB0_229
# BB#138:                               # %land.lhs.true131
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	8(%r9), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB0_229
# BB#139:                               # %if.then139
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$1, %rsi
	movq	%r13, %rdi
	movq	%r8, %r14
	movq	%r9, %r15
	callq	find_reg_note
	movq	%r15, %r9
	movq	%r14, %r8
	testq	%rax, %rax
	jne	.LBB0_195
# BB#140:                               # %lor.lhs.false145
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%r9), %r12
	movq	8(%r9), %rdx
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	movl	8(%r12), %esi
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	8(%rdx), %edx
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	cmpq	%rdx, %rsi
	je	.LBB0_229
# BB#141:                               # %lor.lhs.false145
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$32, %rcx
	movl	8(%r12), %eax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB0_229
# BB#142:                               # %lor.lhs.false145
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$32, %rcx
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB0_229
# BB#143:                               # %lor.lhs.false145
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	8(%r12), %eax
	cmpq	$7, %rax
	je	.LBB0_229
# BB#144:                               # %lor.lhs.false145
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$7, %rax
	je	.LBB0_229
# BB#145:                               # %for.cond.preheader.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	24(%r13), %r14
	testq	%r14, %r14
	je	.LBB0_229
.LBB0_146:                              # %for.body.i1235
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	je	.LBB0_229
# BB#147:                               # %for.body.i1235
                                        #   in Loop: Header=BB0_146 Depth=3
	cmpq	$33, %rax
	je	.LBB0_229
# BB#148:                               # %for.body.i1235
                                        #   in Loop: Header=BB0_146 Depth=3
	cmpq	$34, %rax
	jne	.LBB0_150
# BB#149:                               # %sw.bb1.i.i1238
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_229
.LBB0_150:                              # %perhaps_ends_bb_p.exit.i1240
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	%r14, %rdi
	movq	%r14, 184(%rsp)         # 8-byte Spill
	movq	%r12, %r14
	movq	%r8, %r12
	movq	%r9, %r15
	callq	can_throw_internal
	movq	%r15, %r9
	movq	%r12, %r8
	movq	%r14, %r12
	movq	184(%rsp), %r14         # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB0_229
# BB#151:                               # %if.else.i1244
                                        #   in Loop: Header=BB0_146 Depth=3
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_605
# BB#152:                               # %if.end20.i
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r9, %r15
	callq	reg_set_p
	movq	%r15, %r9
	testq	%rbx, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	jne	.LBB0_229
# BB#153:                               # %lor.lhs.false23.i
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r12, %r14
	movq	%r8, %r12
	movq	%r9, %r15
	callq	reg_set_p
	movq	%r15, %r9
	movq	%r12, %r8
	movq	%r14, %r12
	movq	184(%rsp), %r14         # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB0_229
# BB#154:                               # %lor.lhs.false26.i
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB0_156
# BB#155:                               # %land.lhs.true.i1248
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	8(%rax), %rsi
	movq	%r12, %rdi
	movq	%r12, %r14
	movq	%r8, %r12
	movq	%r9, %r15
	callq	reg_overlap_mentioned_p
	movq	%r15, %r9
	movq	%r12, %r8
	movq	%r14, %r12
	movq	184(%rsp), %r14         # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB0_229
.LBB0_156:                              # %if.end43.i
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	$1, %rsi
	movq	%r14, %rdi
	movl	8(%r12), %edx
	movq	%r9, %r15
	callq	find_regno_note
	movq	%r15, %r9
	testq	%rax, %rax
	je	.LBB0_605
# BB#157:                               # %land.lhs.true47.i
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movl	(%r12), %ecx
	xorq	%rdx, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB0_158
.LBB0_605:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB0_146 Depth=3
	movq	24(%r14), %r14
	testq	%r14, %r14
	movq	160(%rsp), %r8          # 8-byte Reload
	jne	.LBB0_146
	jmp	.LBB0_229
.LBB0_158:                              # %if.then58.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	%r9, 144(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	next_real_insn
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	next_real_insn
	xorq	%r12, %r12
	cmpq	%rax, %r15
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	je	.LBB0_180
# BB#159:                               # %for.body64.lr.ph.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
.LBB0_160:                              # %for.body64.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	32(%r15), %rsi
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	%r15, %r12
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	reg_overlap_mentioned_p
	testq	%rbx, %rax
	jne	.LBB0_162
# BB#161:                               #   in Loop: Header=BB0_160 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %r15
	jmp	.LBB0_167
.LBB0_162:                              # %if.else80.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	callq	validate_replace_rtx
	testq	%rbx, %rax
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %r15
	je	.LBB0_166
# BB#163:                               # %land.lhs.true83.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	$32, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movl	8(%rdi), %eax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB0_165
# BB#164:                               #   in Loop: Header=BB0_160 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_167
.LBB0_165:                              # %lor.lhs.false86.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	32(%r15), %rsi
	movq	%rdx, %r12
	callq	reg_overlap_mentioned_p
	movq	%r12, %rdx
	testq	%rbx, %rax
	movq	72(%rsp), %rax          # 8-byte Reload
	je	.LBB0_167
.LBB0_166:                              # %if.else93.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rdx, %r12
	movq	%r15, %rdx
	callq	validate_replace_rtx
	movq	%r12, %rdx
	movq	$1, %rax
.LBB0_167:                              # %if.end97.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	movabsq	$0, %rax
	movq	%rax, %rcx
	je	.LBB0_169
# BB#168:                               # %if.end97.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movabsq	$1, %rax
	movq	%rax, %rcx
.LBB0_169:                              # %if.end97.i
                                        #   in Loop: Header=BB0_160 Depth=3
	cmpq	%r14, %r15
	je	.LBB0_172
# BB#170:                               # %land.lhs.true104.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB0_172
# BB#171:                               # %if.then109.i
                                        #   in Loop: Header=BB0_160 Depth=3
	incq	32(%rsp)                # 8-byte Folded Spill
	addq	%rcx, 40(%rsp)          # 8-byte Folded Spill
.LBB0_172:                              # %if.end115.i
                                        #   in Loop: Header=BB0_160 Depth=3
	testq	%rdx, %rdx
	jne	.LBB0_177
# BB#173:                               # %land.lhs.true118.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%r15, %rdi
	movq	168(%rsp), %r12         # 8-byte Reload
	movl	8(%r12), %edx
	callq	find_regno_note
	testq	%rax, %rax
	jne	.LBB0_175
# BB#174:                               #   in Loop: Header=BB0_160 Depth=3
	movq	88(%rsp), %rsi          # 8-byte Reload
	xorq	%rdx, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB0_178
.LBB0_175:                              # %if.then122.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movl	(%r12), %ecx
	xorq	%rdx, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	movq	$1, %rsi
	movabsq	$0, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	jne	.LBB0_178
# BB#176:                               # %if.else135.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	%r15, %rdi
	movq	%rax, %r12
	movq	%r12, %rsi
	callq	remove_note
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %rdx
.LBB0_177:                              # %for.inc.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	88(%rsp), %rsi          # 8-byte Reload
.LBB0_178:                              # %for.inc.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	incq	48(%rsp)                # 8-byte Folded Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rcx
	movq	%r15, %rdi
	movq	%rcx, %r12
	callq	next_real_insn
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	next_real_insn
	cmpq	%rax, %r15
	jne	.LBB0_160
# BB#179:                               # %for.end.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r12, 56(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movq	88(%rsp), %r12          # 8-byte Reload
	jne	.LBB0_187
.LBB0_180:                              # %if.then140.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB0_186
# BB#181:                               # %if.then143.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movslq	28(%rax), %rdx
	testq	%rdx, %rdx
	js	.LBB0_185
# BB#182:                               # %if.then148.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	48(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$2, %rsi
	movq	$2, %rcx
	jl	.LBB0_184
# BB#183:                               # %if.then148.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%rdx, %rcx
.LBB0_184:                              # %if.then148.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	%ecx, 28(%rax)
.LBB0_185:                              # %if.end168.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	32(%rax), %ecx
	movq	32(%rsp), %rdx          # 8-byte Reload
	subq	%rdx, %rcx
	movl	%ecx, 32(%rax)
.LBB0_186:                              # %if.end174.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r14, %rdi
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	remove_note
	movq	56(%r13), %rax
	movq	%rax, 16(%r15)
	movq	%r15, 56(%r13)
	movq	$1, %r15
.LBB0_187:                              # %if.end184.i
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%r12, %r12
	jne	.LBB0_190
# BB#188:                               # %land.lhs.true186.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$10, %rsi
	movq	%r13, %rdi
	movq	120(%rsp), %rdx         # 8-byte Reload
	callq	find_regno_note
	movq	%rax, %r12
	testq	%r12, %r12
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	je	.LBB0_194
# BB#189:                               # %if.then189.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$1, %rax
	movb	%al, 2(%r12)
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	remove_note
.LBB0_190:                              # %if.then194.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	56(%r14), %rax
	movq	%rax, 16(%r12)
	movq	%r12, 56(%r14)
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	jl	.LBB0_194
# BB#191:                               # %if.then206.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movslq	28(%rax), %rcx
	testq	%rcx, %rcx
	js	.LBB0_193
# BB#192:                               # %if.then214.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	56(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rcx
	movl	%ecx, 28(%rax)
.LBB0_193:                              # %if.end220.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	32(%rax), %ecx
	movq	40(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rcx
	movl	%ecx, 32(%rax)
.LBB0_194:                              # %optimize_reg_copy_1.exit
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%r15, %r15
	je	.LBB0_229
.LBB0_195:                              # %land.lhs.true154
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	8(%r9), %r15
	movslq	8(%r15), %r12
	cmpq	$53, %r12
	jb	.LBB0_229
# BB#196:                               # %if.then162
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%r9), %r14
	movq	%r9, 144(%rsp)          # 8-byte Spill
	movslq	8(%r14), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	cmpq	$53, %rax
	jb	.LBB0_224
# BB#197:                               # %if.then171
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	24(%r13), %rbx
	jmp	.LBB0_198
.LBB0_222:                              # %for.cond.backedge.i1301
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	24(%rbx), %rbx
.LBB0_198:                              # %if.then171
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_224
# BB#199:                               # %for.body.i1270
                                        #   in Loop: Header=BB0_198 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB0_224
# BB#200:                               # %for.body.i1270
                                        #   in Loop: Header=BB0_198 Depth=3
	cmpq	$33, %rax
	je	.LBB0_224
# BB#201:                               # %for.body.i1270
                                        #   in Loop: Header=BB0_198 Depth=3
	cmpq	$34, %rax
	jne	.LBB0_203
# BB#202:                               # %sw.bb1.i.i1273
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_224
.LBB0_203:                              # %perhaps_ends_bb_p.exit.i1275
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	%rbx, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB0_224
# BB#204:                               # %if.else.i1279
                                        #   in Loop: Header=BB0_198 Depth=3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_222
# BB#205:                               # %cond.true.i1282
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_207
# BB#206:                               # %cond.false.i1283
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_207:                              # %cond.end34.i
                                        #   in Loop: Header=BB0_198 Depth=3
	testq	%rax, %rax
	je	.LBB0_219
# BB#208:                               # %land.lhs.true.i1286
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	16(%rax), %rcx
	cmpq	%r15, %rcx
	jne	.LBB0_219
# BB#209:                               # %land.lhs.true42.i
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	8(%rax), %rax
	cmpq	%r14, %rax
	jne	.LBB0_219
# BB#210:                               # %land.lhs.true48.i
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_211
.LBB0_219:                              # %if.end111.i
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	reg_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_224
# BB#220:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_224
# BB#221:                               # %lor.lhs.false116.i
                                        #   in Loop: Header=BB0_198 Depth=3
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB0_222
# BB#223:                               # %land.lhs.true121.i
                                        #   in Loop: Header=BB0_198 Depth=3
	movq	reg_n_info(%rip), %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_222
	jmp	.LBB0_224
.LBB0_211:                              # %for.cond52.preheader.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	24(%rbx), %rax
	cmpq	%r13, %rax
	movq	%r13, %rsi
	je	.LBB0_218
.LBB0_212:                              # %for.body58.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rsi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_217
# BB#213:                               # %if.then66.i
                                        #   in Loop: Header=BB0_212 Depth=3
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rsi
	movq	%r15, %rdi
	callq	reg_mentioned_p
	movq	168(%rsp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_215
# BB#214:                               # %if.then72.i
                                        #   in Loop: Header=BB0_212 Depth=3
	movq	32(%rsi), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	replace_rtx
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, 32(%rsi)
.LBB0_215:                              # %if.end80.i
                                        #   in Loop: Header=BB0_212 Depth=3
	movzbl	(%rsi), %eax
	cmpq	$34, %rax
	jne	.LBB0_217
# BB#216:                               # %if.then85.i
                                        #   in Loop: Header=BB0_212 Depth=3
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r12,8), %rcx
	movl	32(%rcx), %edx
	decq	%rdx
	movl	%edx, 32(%rcx)
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movl	32(%rax), %ecx
	incq	%rcx
	movl	%ecx, 32(%rax)
.LBB0_217:                              # %for.inc.i1293
                                        #   in Loop: Header=BB0_212 Depth=3
	movq	24(%rsi), %rsi
	movq	24(%rbx), %rax
	cmpq	%rax, %rsi
	jne	.LBB0_212
.LBB0_218:                              # %for.end.i1296
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	find_reg_note
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	remove_note
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r12,8), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	movq	$1, %rsi
	movq	%r13, %rdi
	movq	%r14, %rdx
	callq	find_reg_note
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	remove_note
	movq	reg_n_info(%rip), %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
.LBB0_224:                              # %if.end178
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	8(%r9), %rdx
	movl	8(%rdx), %eax
	movq	regno_src_regno(%rip), %rcx
	movslq	(%rcx,%rax,4), %rsi
	testq	%rsi, %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	160(%rsp), %r8          # 8-byte Reload
	jns	.LBB0_229
# BB#225:                               # %land.lhs.true189
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%r9), %rsi
	cmpq	%rdx, %rsi
	je	.LBB0_229
# BB#226:                               # %if.then198
                                        #   in Loop: Header=BB0_42 Depth=2
	movslq	8(%rsi), %rsi
	movslq	(%rcx,%rsi,4), %rdx
	testq	%rdx, %rdx
	jns	.LBB0_228
# BB#227:                               # %if.then198
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%rsi, %rdx
.LBB0_228:                              # %if.then198
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	%edx, (%rcx,%rax,4)
.LBB0_229:                              # %if.end223
                                        #   in Loop: Header=BB0_42 Depth=2
	movl	flag_regmove(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_422
# BB#230:                               # %if.end226
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r13, %rdi
	leaq	192(%rsp), %rsi
	movq	%r8, %r14
	movq	%r9, %r15
	movq	%r15, 144(%rsp)         # 8-byte Spill
	callq	find_matches
	movq	%r15, %r9
	movq	%r14, %r8
	testq	%rbx, %rax
	je	.LBB0_422
# BB#231:                               # %if.end226
                                        #   in Loop: Header=BB0_42 Depth=2
	movzbl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_422
# BB#232:                               # %for.body235.lr.ph
                                        #   in Loop: Header=BB0_42 Depth=2
	leaq	16(%r9), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
	.align	16, 0x90
.LBB0_233:                              # %for.body235
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_293 Depth 4
                                        #         Child Loop BB0_314 Depth 4
	movslq	192(%rsp,%r12,4), %r14
	movq	%r14, 184(%rsp)         # 8-byte Spill
	testq	%r14, %r14
	js	.LBB0_246
# BB#234:                               # %if.end241
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	recog_data(,%r12,8), %r11
	movzbl	(%r11), %eax
	cmpq	$61, %rax
	jne	.LBB0_246
# BB#235:                               # %if.end251
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	recog_data(,%r14,8), %r10
	movl	(%r10), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB0_236
# BB#237:                               # %land.lhs.true256
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%r10), %rax
	movq	$16, %rcx
	movq	%rdx, %rsi
	movzbl	2(%rax), %edi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %eax
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rax
	jae	.LBB0_239
# BB#238:                               #   in Loop: Header=BB0_233 Depth=3
	movq	%r11, 168(%rsp)         # 8-byte Spill
	jmp	.LBB0_240
.LBB0_236:                              #   in Loop: Header=BB0_233 Depth=3
	movq	%r11, 168(%rsp)         # 8-byte Spill
	jmp	.LBB0_241
.LBB0_239:                              # %if.then273
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	16(%r10), %edx
	movq	%r11, %rsi
	movq	%r14, 184(%rsp)         # 8-byte Spill
	movq	%r9, %r14
	movq	%r10, %r15
	movq	%r11, %r13
	callq	gen_rtx_SUBREG
	movq	%r13, %r11
	movq	%r15, %r10
	movq	%r14, %r9
	movq	184(%rsp), %r14         # 8-byte Reload
	movq	176(%rsp), %r13         # 8-byte Reload
	movq	8(%r10), %r10
	movl	(%r10), %edx
	movq	%rax, 168(%rsp)         # 8-byte Spill
.LBB0_240:                              # %if.end287
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	160(%rsp), %r8          # 8-byte Reload
.LBB0_241:                              # %if.end287
                                        #   in Loop: Header=BB0_233 Depth=3
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB0_246
# BB#242:                               # %lor.lhs.false292
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	8(%r10), %eax
	cmpq	$53, %rax
	jb	.LBB0_246
# BB#243:                               # %if.end299
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	8(%r11), %edx
	cmpq	$52, %rdx
	ja	.LBB0_247
# BB#244:                               # %if.then305
                                        #   in Loop: Header=BB0_233 Depth=3
	movslq	432(%rsp,%r12,4), %rsi
	movq	$32, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jge	.LBB0_246
# BB#245:                               # %if.then310
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	regno_src_regno(%rip), %rcx
	movl	%edx, (%rcx,%rax,4)
	jmp	.LBB0_246
.LBB0_247:                              # %if.end320
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%rdx,8), %rax
	movslq	28(%rax), %rax
	testq	%rax, %rax
	js	.LBB0_246
# BB#248:                               # %if.end330
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	312(%rsp,%r12,4), %eax
	testq	%rax, %rax
	jne	.LBB0_246
# BB#249:                               # %lor.lhs.false335
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	312(%rsp,%r14,4), %eax
	cmpq	$1, %rax
	jne	.LBB0_246
# BB#250:                               # %if.end342
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	552(%rsp,%r14,4), %eax
	testq	%rax, %rax
	je	.LBB0_252
# BB#251:                               # %land.lhs.true346
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	32(%r13), %rdi
	xorq	%rdx, %rdx
	movq	%r11, %rsi
	movq	%r8, %r15
	movq	%r14, 184(%rsp)         # 8-byte Spill
	movq	%r9, %r14
	movq	%r10, %r13
	movq	%r11, 112(%rsp)         # 8-byte Spill
	callq	count_occurrences
	movq	112(%rsp), %r11         # 8-byte Reload
	movq	%r13, %r10
	movq	%r14, %r9
	movq	184(%rsp), %r14         # 8-byte Reload
	movq	176(%rsp), %r13         # 8-byte Reload
	movq	%r15, %r8
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jg	.LBB0_246
.LBB0_252:                              # %if.end354
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	recog_data(,%r14,8), %rax
	movq	8(%r9), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_246
# BB#253:                               # %if.end363
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r11, %rdi
	movq	%r10, %rsi
	movq	%r8, %r15
	movq	%r10, 104(%rsp)         # 8-byte Spill
	movq	%r11, 112(%rsp)         # 8-byte Spill
	callq	operands_match_p
	movq	112(%rsp), %r10         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rbx, %rax
	jne	.LBB0_246
# BB#254:                               # %if.end367
                                        #   in Loop: Header=BB0_233 Depth=3
	movslq	432(%rsp,%r12,4), %rax
	testq	%rax, %rax
	js	.LBB0_269
# BB#255:                               # %if.then373
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	recog_data(,%rax,8), %rdi
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	callq	operands_match_p
	movq	112(%rsp), %r10         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rbx, %rax
	je	.LBB0_269
# BB#256:                               # %land.lhs.true381
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rdx          # 8-byte Reload
	movzbl	(%rdx), %ecx
	xorq	%rax, %rax
	cmpq	$61, %rcx
	movabsq	$0, %rcx
	jne	.LBB0_262
# BB#257:                               # %if.end.i1311
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	8(%rdx), %edx
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rdx,8), %rcx
	movslq	28(%rcx), %rcx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	js	.LBB0_262
# BB#258:                               # %if.end5.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	regno_src_regno(%rip), %rcx
	movslq	(%rcx,%rdx,4), %rdx
	testq	%rdx, %rdx
	movq	$3, %rcx
	js	.LBB0_262
# BB#259:                               # %if.end13.i
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$53, %rdx
	movabsq	$0, %rcx
	jge	.LBB0_261
# BB#260:                               # %if.end13.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movabsq	$1, %rcx
.LBB0_261:                              # %if.end13.i
                                        #   in Loop: Header=BB0_233 Depth=3
	xorq	$1, %rcx
	incq	%rcx
.LBB0_262:                              # %replacement_quality.exit
                                        #   in Loop: Header=BB0_233 Depth=3
	movzbl	(%r10), %edx
	cmpq	$61, %rdx
	jne	.LBB0_268
# BB#263:                               # %if.end.i1324
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	8(%r10), %edx
	movq	reg_n_info(%rip), %rdi
	movq	32(%rdi,%rdx,8), %rdi
	movslq	28(%rdi), %rdi
	testq	%rdi, %rdi
	js	.LBB0_268
# BB#264:                               # %if.end5.i1327
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	regno_src_regno(%rip), %rax
	movslq	(%rax,%rdx,4), %rdx
	testq	%rdx, %rdx
	movq	$3, %rax
	js	.LBB0_268
# BB#265:                               # %if.end13.i1330
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$53, %rdx
	movabsq	$0, %rax
	jge	.LBB0_267
# BB#266:                               # %if.end13.i1330
                                        #   in Loop: Header=BB0_233 Depth=3
	movabsq	$1, %rax
.LBB0_267:                              # %if.end13.i1330
                                        #   in Loop: Header=BB0_233 Depth=3
	xorq	$1, %rax
	incq	%rax
.LBB0_268:                              # %replacement_quality.exit1332
                                        #   in Loop: Header=BB0_233 Depth=3
	andq	%rbx, %rax
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	jae	.LBB0_246
.LBB0_269:                              # %if.end388
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r10, 112(%rsp)         # 8-byte Spill
	movl	8(%r10), %edi
	movq	%rsi, %r15
	callq	reg_preferred_class
	movl	8(%r15), %edi
	movq	%rax, 96(%rsp)          # 8-byte Spill
	andq	%rbx, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	callq	reg_preferred_class
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%rax, %rsi
	andq	%rbx, %rsi
	cmpq	%rsi, 88(%rsp)          # 8-byte Folded Reload
	je	.LBB0_280
# BB#270:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %r15
	movq	%r15, %rsi
	movq	%r8, %rbx
	callq	reg_class_subset_p
	movq	%r15, %rdi
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%rbx, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB0_278
# BB#271:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	cmpq	$1, %rax
	je	.LBB0_278
# BB#272:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$2, %rax
	je	.LBB0_278
# BB#273:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$3, %rax
	je	.LBB0_278
# BB#274:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$4, %rax
	je	.LBB0_278
# BB#275:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$7, %rax
	je	.LBB0_278
# BB#276:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$5, %rax
	je	.LBB0_278
# BB#277:                               # %lor.lhs.false.i1340
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$6, %rax
	jne	.LBB0_280
.LBB0_278:                              # %lor.rhs.i1341
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r15
	callq	reg_class_subset_p
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rbx, %rax
	je	.LBB0_246
# BB#279:                               # %lor.rhs.i1341
                                        #   in Loop: Header=BB0_233 Depth=3
	decq	%rsi
	andq	%rbx, %rsi
	cmpq	$7, %rsi
	jb	.LBB0_246
.LBB0_280:                              # %if.end400
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	(%rdx), %eax
	movl	(%rcx), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB0_246
# BB#281:                               # %if.end410
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	$1, %rsi
	movq	%r13, %rdi
	movq	%r8, %r15
	callq	find_reg_note
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	movzbl	3(%rsi), %ecx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	$4, %rcx
	jne	.LBB0_246
# BB#282:                               # %if.end.i1348
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_283
# BB#284:                               # %if.then2.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	$37, 72(%rsp)           # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movzbl	(%rdi), %r8d
	leaq	-75(%r8), %rcx
	andq	%rbx, %rcx
	cmpq	$15, %rcx
	ja	.LBB0_289
# BB#285:                               # %if.then2.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$53249, %rax            # imm = 0xD001
	je	.LBB0_289
# BB#286:                               # %land.lhs.true.i1352
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%rdi), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB0_289
# BB#287:                               # %land.lhs.true17.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %eax
	cmpq	$54, %rax
	jne	.LBB0_289
# BB#288:                               # %if.then27.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%rcx), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r8, 72(%rsp)           # 8-byte Spill
	jmp	.LBB0_290
.LBB0_283:                              #   in Loop: Header=BB0_233 Depth=3
	movq	$37, 72(%rsp)           # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB0_290
.LBB0_289:                              # %if.else.i1353
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r14, 184(%rsp)         # 8-byte Spill
	movq	%r9, %r14
	movq	%rdx, %r15
	callq	stable_and_no_regs_but_for_p
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %r9
	movq	184(%rsp), %r14         # 8-byte Reload
	testq	%rbx, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	je	.LBB0_246
.LBB0_290:                              # %if.end45.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%rsi, %r15
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB0_292
# BB#291:                               # %if.then47.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	8(%r13), %ecx
	xorq	%rax, %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str4, %rsi
	movq	%r12, %rdx
	movq	%r14, %r8
	callq	fprintf
.LBB0_292:                              # %if.end51.i
                                        #   in Loop: Header=BB0_233 Depth=3
	callq	get_insns
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	reg_is_remote_constant_p
	movq	%r15, %rdx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	testq	%rbx, %rax
	movabsq	$0, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r13, %r11
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %r10         # 8-byte Reload
	jne	.LBB0_246
	jmp	.LBB0_293
.LBB0_373:                              # %if.end467.i
                                        #   in Loop: Header=BB0_293 Depth=4
	incq	40(%rsp)                # 8-byte Folded Spill
	addq	%rax, 32(%rsp)          # 8-byte Folded Spill
	movq	%rcx, %rbx
	movq	%rdx, %r8
	movq	%rsi, %r9
	movq	%rdi, %r14
	movq	%r10, %r13
	movq	%r11, %r10
	movq	%r15, %rdx
	movq	96(%rsp), %r11          # 8-byte Reload
.LBB0_293:                              # %for.cond.i1361
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_233 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	24(%r11), %r11
	testq	%r11, %r11
	je	.LBB0_246
# BB#294:                               # %for.body.i1362
                                        #   in Loop: Header=BB0_293 Depth=4
	movl	(%r11), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB0_296
# BB#295:                               # %if.then64.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movl	8(%r10), %esi
	leaq	64(%r11), %rdi
	movq	%r11, %rcx
	movq	%r8, %r15
	movq	%r11, 96(%rsp)          # 8-byte Spill
	callq	replace_in_call_usage
	movq	96(%rsp), %r11          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	movl	(%r11), %eax
.LBB0_296:                              # %if.end70.i
                                        #   in Loop: Header=BB0_293 Depth=4
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$36, %rax
	je	.LBB0_246
# BB#297:                               # %if.end70.i
                                        #   in Loop: Header=BB0_293 Depth=4
	cmpq	$33, %rax
	je	.LBB0_246
# BB#298:                               # %if.end70.i
                                        #   in Loop: Header=BB0_293 Depth=4
	cmpq	$34, %rax
	jne	.LBB0_300
# BB#299:                               # %sw.bb1.i.i1369
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_246
.LBB0_300:                              # %perhaps_ends_bb_p.exit.i1371
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	%r11, %rdi
	movq	%r8, %r15
	movq	%r11, 96(%rsp)          # 8-byte Spill
	callq	can_throw_internal
	movq	96(%rsp), %r11          # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	104(%rsp), %r10         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rax, %rax
	jne	.LBB0_246
# BB#301:                               # %if.else74.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movzbl	(%r11), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_293
# BB#302:                               # %if.end82.i1373
                                        #   in Loop: Header=BB0_293 Depth=4
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	movabsq	$0, %rax
	je	.LBB0_304
# BB#303:                               # %if.end82.i1373
                                        #   in Loop: Header=BB0_293 Depth=4
	movabsq	$1, %rax
.LBB0_304:                              # %if.end82.i1373
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rdx, %rdi
	movq	%r11, %rsi
	movq	%r11, %r15
	callq	reg_set_p
	movq	%r15, %rsi
	testq	%rbx, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %rdi         # 8-byte Reload
	jne	.LBB0_246
# BB#305:                               # %lor.lhs.false89.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	%r8, %r15
	callq	reg_set_p
	movq	96(%rsp), %r11          # 8-byte Reload
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	104(%rsp), %r10         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rbx, %rax
	jne	.LBB0_246
# BB#306:                               # %lor.lhs.false92.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	32(%r11), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB0_308
# BB#307:                               # %land.lhs.true100.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	8(%rax), %rsi
	movq	%r8, %r15
	callq	reg_overlap_mentioned_p
	movq	96(%rsp), %r11          # 8-byte Reload
	movq	104(%rsp), %r10         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rbx, %rax
	jne	.LBB0_246
.LBB0_308:                              # %if.end110.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	%r11, 96(%rsp)          # 8-byte Spill
	movl	8(%r10), %edx
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	%rax, 56(%rsp)          # 8-byte Folded Spill
	incq	64(%rsp)                # 8-byte Folded Spill
	movq	$1, %rsi
	movq	%r11, %rdi
	movq	%r10, %r15
	callq	find_regno_note
	movq	%r15, %rdi
	testq	%rax, %rax
	je	.LBB0_365
# BB#309:                               # %land.lhs.true116.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movl	(%rdi), %ecx
	xorq	%rdx, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB0_310
.LBB0_365:                              # %if.end433.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rsi
	movq	%rdi, %rbx
	callq	reg_overlap_mentioned_p
	movq	%r15, %r11
	movq	%rbx, %r10
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	112(%rsp), %rdi         # 8-byte Reload
	jne	.LBB0_246
# BB#366:                               # %if.end440.i
                                        #   in Loop: Header=BB0_293 Depth=4
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_369
# BB#367:                               # %land.lhs.true442.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	32(%r11), %rsi
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%r10, %r13
	callq	reg_overlap_mentioned_p
	movq	96(%rsp), %r11          # 8-byte Reload
	movq	%r13, %r10
	movq	%r14, %r9
	movq	184(%rsp), %r14         # 8-byte Reload
	movq	176(%rsp), %r13         # 8-byte Reload
	movq	%r15, %r8
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	testq	%rbx, %rax
	je	.LBB0_369
# BB#368:                               # %if.then448.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	$1, %rsi
	movq	%r11, %rdi
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%r8, %r15
	callq	find_reg_note
	movq	96(%rsp), %r11          # 8-byte Reload
	movq	104(%rsp), %r10         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%r11, 48(%rsp)          # 8-byte Spill
.LBB0_369:                              # %if.end450.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movzbl	(%r11), %eax
	cmpq	$34, %rax
	movq	112(%rsp), %rdx         # 8-byte Reload
	jne	.LBB0_293
# BB#370:                               # %if.then455.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movl	8(%rdx), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_246
# BB#371:                               # %if.end467.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movq	%r11, 96(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%r10, %r11
	movq	%r13, %r10
	movq	%r14, %rdi
	movq	%r9, %rsi
	movq	%r8, %rdx
	movq	%rbx, %rcx
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	movabsq	$0, %rax
	je	.LBB0_373
# BB#372:                               # %if.end467.i
                                        #   in Loop: Header=BB0_293 Depth=4
	movabsq	$1, %rax
	jmp	.LBB0_373
.LBB0_310:                              # %if.then128.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%rax, 24(%rsp)          # 8-byte Spill
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rbx
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	96(%rsp), %rax          # 8-byte Reload
	je	.LBB0_312
# BB#311:                               # %land.lhs.true130.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	16(%rax), %rax
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_246
.LBB0_312:                              # %if.end140.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	movq	%r9, %r15
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_355
# BB#313:                               # %if.then142.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	movq	96(%rsp), %rdi          # 8-byte Reload
	testq	%rbx, %rax
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r9
	movq	104(%rsp), %rdx         # 8-byte Reload
	jne	.LBB0_246
.LBB0_314:                              # %for.body152.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        #       Parent Loop BB0_233 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$36, %rax
	je	.LBB0_323
# BB#315:                               # %for.body152.i
                                        #   in Loop: Header=BB0_314 Depth=4
	cmpq	$33, %rax
	je	.LBB0_323
# BB#316:                               # %for.body152.i
                                        #   in Loop: Header=BB0_314 Depth=4
	cmpq	$34, %rax
	jne	.LBB0_318
# BB#317:                               # %sw.bb1.i1246.i
                                        #   in Loop: Header=BB0_314 Depth=4
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_323
.LBB0_318:                              # %perhaps_ends_bb_p.exit1251.i
                                        #   in Loop: Header=BB0_314 Depth=4
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %r15
	callq	can_throw_internal
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r8
	testq	%rax, %rax
	jne	.LBB0_323
# BB#319:                               # %if.else156.i
                                        #   in Loop: Header=BB0_314 Depth=4
	movq	%r9, %r15
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_322
# BB#320:                               # %if.else165.i
                                        #   in Loop: Header=BB0_314 Depth=4
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	testq	%rbx, %rax
	jne	.LBB0_329
# BB#321:                               # %lor.lhs.false171.i
                                        #   in Loop: Header=BB0_314 Depth=4
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	reg_set_p
	testq	%rbx, %rax
	jne	.LBB0_329
.LBB0_322:                              # %for.inc.i1380
                                        #   in Loop: Header=BB0_314 Depth=4
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r9
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	jne	.LBB0_314
	jmp	.LBB0_323
.LBB0_329:                              # %for.end.i1381
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r9
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	je	.LBB0_323
# BB#330:                               # %if.then182.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_323
# BB#331:                               # %cond.true.i1382
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r8, %r15
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movzbl	(%rax), %eax
	cmpq	$47, %rax
	je	.LBB0_333
# BB#332:                               # %cond.false.i1383
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	single_set_2
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB0_333:                              # %if.end208.i
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	movq	%r15, %r8
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	je	.LBB0_323
# BB#334:                               # %lor.lhs.false212.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rax), %rcx
	movzbl	(%rcx), %eax
	movq	%rcx, %r10
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB0_323
# BB#335:                               # %lor.lhs.false220.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%r10), %rax
	cmpq	%rsi, %rax
	jne	.LBB0_323
# BB#336:                               # %lor.lhs.false229.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	16(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB0_323
# BB#337:                               # %lor.lhs.false240.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r9, %r15
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	8(%rcx), %rcx
	cmpq	%rsi, %rcx
	movq	%rsi, %rcx
	je	.LBB0_340
# BB#338:                               # %land.lhs.true246.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rax
	movq	%rax, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r9
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	jne	.LBB0_339
.LBB0_323:                              # %if.then249.i
                                        #   in Loop: Header=BB0_233 Depth=3
	cmpq	$75, 72(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_246
# BB#324:                               # %if.then249.i
                                        #   in Loop: Header=BB0_233 Depth=3
	testq	%rbx, %r8
	je	.LBB0_246
# BB#325:                               # %land.lhs.true256.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movl	8(%rdx), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_350
# BB#326:                               # %land.lhs.true263.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_350
# BB#327:                               # %cond.true271.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r8, %r15
	movq	32(%rdi), %rax
	movq	%rdi, %rsi
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$47, %rdx
	jne	.LBB0_342
# BB#328:                               # %cond.true279.i
                                        #   in Loop: Header=BB0_233 Depth=3
	testq	%rax, %rax
	movq	%r15, %r8
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%rsi, %rdi
	jne	.LBB0_344
	jmp	.LBB0_350
.LBB0_339:                              # %land.lhs.true246.if.else375_crit_edge.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r9, %r15
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rax), %rcx
	movq	16(%rcx), %rax
	movq	%rcx, %r10
.LBB0_340:                              # %if.else375.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%rax), %rax
	movq	80(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
	cmpq	$75, 72(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_352
# BB#341:                               #   in Loop: Header=BB0_233 Depth=3
	movq	$75, 72(%rsp)           # 8-byte Folded Spill
	movq	%r15, %r9
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB0_354
.LBB0_342:                              # %cond.false284.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
	movq	96(%rsp), %rdi          # 8-byte Reload
	testq	%rax, %rax
	movq	%r15, %r8
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	je	.LBB0_350
# BB#343:                               # %cond.false284.land.lhs.true291_crit_edge.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	32(%rdi), %rax
	movl	(%rax), %ecx
.LBB0_344:                              # %land.lhs.true291.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r8, %r15
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	je	.LBB0_346
# BB#345:                               # %cond.false311.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_346:                              # %cond.end319.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	%r15, %r8
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	jne	.LBB0_350
# BB#347:                               # %land.lhs.true328.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r8, %r15
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_349
# BB#348:                               # %cond.false348.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_349:                              # %cond.end356.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	8(%rax), %rax
	movl	8(%rax), %eax
	cmpq	$53, %rax
	movq	%r15, %r8
	movq	144(%rsp), %r9          # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	jb	.LBB0_246
.LBB0_350:                              # %land.lhs.true366.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movzbl	2(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB0_246
# BB#351:                               # %if.then372.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r10
	movq	80(%rsp), %rax          # 8-byte Reload
	negq	%rax
	movq	$76, 72(%rsp)           # 8-byte Folded Spill
	movq	%r13, 16(%rsp)          # 8-byte Spill
	jmp	.LBB0_354
.LBB0_352:                              # %land.lhs.true388.i
                                        #   in Loop: Header=BB0_233 Depth=3
	testq	%rax, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r9
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	js	.LBB0_246
# BB#353:                               # %lor.lhs.false391.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movzbl	2(%r10), %ecx
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	cmpq	%rcx, %rax
	jae	.LBB0_246
.LBB0_354:                              # %if.end416.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%r9, %r15
	addq	$16, 8(%rsp)            # 8-byte Folded Spill
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
.LBB0_355:                              # %for.end478.i
                                        #   in Loop: Header=BB0_233 Depth=3
	movq	recog_data+240(,%r14,8), %rax
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	96(%rsp), %rdx          # 8-byte Reload
	callq	validate_replace_rtx
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rbx, %rax
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r9
	jne	.LBB0_356
	.align	16, 0x90
.LBB0_246:                              # %for.inc415
                                        #   in Loop: Header=BB0_233 Depth=3
	incq	%r12
	movsbq	recog_data+996(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_233
	jmp	.LBB0_422
.LBB0_356:                              # %if.end481.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r13, %r15
	movq	%rcx, %rdi
	movq	%rcx, %r13
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	remove_note
	cmpq	$76, 72(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_358
# BB#357:                               # %if.then484.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	32(%r15), %rax
	movq	%rax, %rdi
	callq	copy_rtx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_after
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	addq	$16, 168(%rsp)          # 8-byte Folded Spill
	xorq	%rdi, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	movq	112(%rsp), %rsi         # 8-byte Reload
	movl	8(%rsi), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	12(%rax), %edx
	incq	%rdx
	movl	%edx, 12(%rax)
	movl	8(%rsi), %eax
	movq	32(%rcx,%rax,8), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
.LBB0_358:                              # %if.end514.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	movq	%r15, 168(%rsp)         # 8-byte Spill
	je	.LBB0_375
# BB#359:                               # %if.then516.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	32(%r15), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB0_361
# BB#360:                               # %if.then521.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	remove_note
.LBB0_361:                              # %if.end522.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	56(%r15), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	16(%r13), %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	emit_insn_after_with_line_notes
	movq	%r15, %rdi
	callq	delete_insn
	movq	%r13, 168(%rsp)         # 8-byte Spill
	jmp	.LBB0_362
.LBB0_364:                              # %while.body.i1392
                                        #   in Loop: Header=BB0_362 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
.LBB0_362:                              # %while.cond.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_364
# BB#363:                               # %lor.rhs.i1391
                                        #   in Loop: Header=BB0_362 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB0_364
# BB#374:                               # %while.end.i1393
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 56(%rax)
.LBB0_375:                              # %if.end548.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	movabsq	$0, %rax
	jne	.LBB0_377
# BB#376:                               # %if.end548.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movabsq	$1, %rax
.LBB0_377:                              # %if.end548.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	72(%rsp), %rcx          # 8-byte Reload
	addq	$-75, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	ja	.LBB0_410
# BB#378:                               # %if.end548.i
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%rax, %rax
	je	.LBB0_410
# BB#379:                               # %if.then556.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	find_reg_note
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_410
# BB#380:                               # %land.lhs.true561.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	cmpq	$133, %rax
	movq	$1, 144(%rsp)           # 8-byte Folded Spill
	jg	.LBB0_383
# BB#381:                               # %land.lhs.true561.i
                                        #   in Loop: Header=BB0_42 Depth=2
	addq	$-54, %rax
	andq	%rbx, %rax
	cmpq	$14, %rax
	ja	.LBB0_410
# BB#382:                               # %land.lhs.true561.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)          # 8-byte Spill
	jne	.LBB0_386
	jmp	.LBB0_410
.LBB0_383:                              # %land.lhs.true561.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$134, %rax
	jne	.LBB0_385
# BB#384:                               #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)          # 8-byte Spill
	jmp	.LBB0_386
.LBB0_385:                              # %land.lhs.true561.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$140, %rax
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)          # 8-byte Spill
	jne	.LBB0_410
	jmp	.LBB0_386
.LBB0_405:                              # %if.end686.i
                                        #   in Loop: Header=BB0_386 Depth=3
	addq	%rax, 80(%rsp)          # 8-byte Folded Spill
	incq	144(%rsp)               # 8-byte Folded Spill
.LBB0_386:                              # %for.cond629.i
                                        #   Parent Loop BB0_34 Depth=1
                                        #     Parent Loop BB0_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_410
# BB#387:                               # %for.body631.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$36, %rax
	je	.LBB0_410
# BB#388:                               # %for.body631.i
                                        #   in Loop: Header=BB0_386 Depth=3
	cmpq	$33, %rax
	je	.LBB0_410
# BB#389:                               # %for.body631.i
                                        #   in Loop: Header=BB0_386 Depth=3
	cmpq	$34, %rax
	jne	.LBB0_391
# BB#390:                               # %sw.bb1.i1256.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_410
.LBB0_391:                              # %perhaps_ends_bb_p.exit1261.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB0_410
# BB#392:                               # %if.else635.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_386
# BB#393:                               # %if.end645.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	reg_set_p
	testq	%rbx, %rax
	jne	.LBB0_394
# BB#402:                               # %if.end679.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	testq	%rbx, %rax
	jne	.LBB0_410
# BB#403:                               # %if.end686.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movzbl	(%r13), %eax
	cmpq	$34, %rax
	movabsq	$0, %rax
	jne	.LBB0_405
# BB#404:                               # %if.end686.i
                                        #   in Loop: Header=BB0_386 Depth=3
	movabsq	$1, %rax
	jmp	.LBB0_405
.LBB0_394:                              # %if.then649.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_410
# BB#395:                               # %cond.true657.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_397
# BB#396:                               # %cond.false669.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_397:                              # %for.end698.i
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%rax, %rax
	je	.LBB0_410
# BB#398:                               # %land.lhs.true702.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	8(%rax), %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB0_410
# BB#399:                               # %land.lhs.true708.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$133, %rax
	jg	.LBB0_406
# BB#400:                               # %land.lhs.true708.i
                                        #   in Loop: Header=BB0_42 Depth=2
	addq	$-54, %rax
	andq	%rbx, %rax
	cmpq	$14, %rax
	ja	.LBB0_410
# BB#401:                               # %land.lhs.true708.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB0_408
	jmp	.LBB0_410
.LBB0_406:                              # %land.lhs.true708.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$134, %rax
	je	.LBB0_408
# BB#407:                               # %land.lhs.true708.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$140, %rax
	jne	.LBB0_410
.LBB0_408:                              # %land.lhs.true772.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	xorq	%rcx, %rcx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	testq	%rbx, %rax
	je	.LBB0_410
# BB#409:                               # %if.then781.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	movq	112(%rsp), %rsi         # 8-byte Reload
	movl	8(%rsi), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	12(%rax), %edx
	decq	%rdx
	movl	%edx, 12(%rax)
	movl	8(%rsi), %eax
	movq	32(%rcx,%rax,8), %rax
	movl	32(%rax), %edx
	movq	80(%rsp), %rdi          # 8-byte Reload
	subq	%rdi, %rdx
	movl	%edx, 32(%rax)
	movl	8(%rsi), %eax
	movq	32(%rcx,%rax,8), %rax
	movl	28(%rax), %ecx
	movq	144(%rsp), %rdx         # 8-byte Reload
	subq	%rdx, %rcx
	movl	%ecx, 28(%rax)
.LBB0_410:                              # %if.end811.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	testq	%rbx, %rax
	je	.LBB0_412
# BB#411:                               # %if.then891.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	56(%rdx), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 16(%rcx)
	movq	%rcx, 56(%rdx)
.LBB0_412:                              # %if.end901.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_414
# BB#413:                               # %if.end901.if.end924_crit_edge.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	reg_n_info(%rip), %r8
	jmp	.LBB0_417
.LBB0_414:                              # %if.then903.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_416
# BB#415:                               # %if.then905.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	remove_note
.LBB0_416:                              # %if.end906.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	%r13, %rdx
	movq	56(%rdx), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 16(%rcx)
	movq	%rcx, 56(%rdx)
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	movq	reg_n_info(%rip), %r8
	movq	32(%r8,%rcx,8), %rcx
	movl	32(%rcx), %edx
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdx
	movl	%edx, 32(%rcx)
.LBB0_417:                              # %if.end924.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	112(%rsp), %rdi         # 8-byte Reload
	movl	8(%rdi), %ecx
	movq	32(%r8,%rcx,8), %rcx
	movl	12(%rcx), %edx
	incq	%rdx
	movl	%edx, 12(%rcx)
	movq	104(%rsp), %rsi         # 8-byte Reload
	movl	8(%rsi), %ecx
	movq	32(%r8,%rcx,8), %rcx
	movl	12(%rcx), %edx
	decq	%rdx
	movl	%edx, 12(%rcx)
	movl	8(%rsi), %ecx
	movq	32(%r8,%rcx,8), %rcx
	movl	32(%rcx), %edx
	movq	40(%rsp), %rax          # 8-byte Reload
	subq	%rax, %rdx
	movl	%edx, 32(%rcx)
	movl	8(%rdi), %ecx
	movq	32(%r8,%rcx,8), %rcx
	movl	28(%rcx), %edx
	movq	56(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdx
	movl	%edx, 28(%rcx)
	movl	8(%rsi), %ecx
	movq	32(%r8,%rcx,8), %rcx
	movslq	28(%rcx), %rdx
	testq	%rdx, %rdx
	js	.LBB0_420
# BB#418:                               # %if.then971.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	64(%rsp), %rdi          # 8-byte Reload
	subq	%rdi, %rdx
	movl	%edx, 28(%rcx)
	movl	8(%rsi), %ecx
	movq	32(%r8,%rcx,8), %rax
	movslq	28(%rax), %rcx
	cmpq	$1, %rcx
	jg	.LBB0_420
# BB#419:                               # %if.then991.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	$2, %rcx
	movl	%ecx, 28(%rax)
.LBB0_420:                              # %if.end1001.i
                                        #   in Loop: Header=BB0_42 Depth=2
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	160(%rsp), %r8          # 8-byte Reload
	movq	%r15, %r13
	je	.LBB0_422
# BB#421:                               # %if.then1003.i
                                        #   in Loop: Header=BB0_42 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str5, %rdx
	movq	%rdx, %rsi
	movq	%r12, %rdx
	movq	%r8, %r15
	movq	%r14, %r8
	callq	fprintf
	movq	%r15, %r8
	.align	16, 0x90
.LBB0_422:                              # %for.inc418
                                        #   in Loop: Header=BB0_42 Depth=2
	testq	%rbx, %r8
	jne	.LBB0_423
# BB#424:                               # %for.inc418
                                        #   in Loop: Header=BB0_42 Depth=2
	addq	$24, %r13
	movq	(%r13), %r13
	jmp	.LBB0_42
	.align	16, 0x90
.LBB0_426:                              # %for.inc431
                                        #   in Loop: Header=BB0_34 Depth=1
	incq	%r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	movq	152(%rsp), %r13         # 8-byte Reload
	movq	136(%rsp), %r14         # 8-byte Reload
	jl	.LBB0_34
# BB#427:                               # %for.end433
	testq	%r14, %r14
	je	.LBB0_429
# BB#428:                               # %if.then435
	movabsq	$.L.str3, %rdi
	movq	$26, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
.LBB0_429:                              # %if.end437
	callq	get_last_insn
	movq	%rax, %rbx
	jmp	.LBB0_430
	.align	16, 0x90
.LBB0_595:                              # %for.inc904
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	16(%rbx), %rbx
.LBB0_430:                              # %if.end437
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_439 Depth 2
                                        #       Child Loop BB0_546 Depth 3
                                        #       Child Loop BB0_467 Depth 3
                                        #     Child Loop BB0_576 Depth 2
                                        #       Child Loop BB0_577 Depth 3
	testq	%rbx, %rbx
	je	.LBB0_431
# BB#435:                               # %for.body441
                                        #   in Loop: Header=BB0_430 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_595
# BB#436:                               # %if.then449
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%rbx, %rdi
	leaq	192(%rsp), %rsi
	callq	find_matches
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_595
# BB#437:                               # %if.then449
                                        #   in Loop: Header=BB0_430 Depth=1
	movzbl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_595
# BB#438:                               # %for.body460.lr.ph
                                        #   in Loop: Header=BB0_430 Depth=1
	leaq	32(%rbx), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%rbx, 184(%rsp)         # 8-byte Spill
	xorq	%r15, %r15
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB0_439
.LBB0_500:                              # %if.end647
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rax, %rdi
	callq	reg_preferred_class
	movl	8(%r14), %edi
	movq	%rax, 112(%rsp)         # 8-byte Spill
	callq	reg_preferred_class
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_501
.LBB0_454:                              #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
	movq	152(%rsp), %r13         # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB0_518
.LBB0_501:                              # %if.end659
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r13, 160(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rdx
	andq	%rcx, %rdx
	cmpq	%rax, %rdx
	movq	%rdi, %rax
	je	.LBB0_534
# BB#502:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rax, %r13
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	reg_class_subset_p
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	120(%rsp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_510
# BB#503:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$1, %rsi
	je	.LBB0_510
# BB#504:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$2, %rsi
	je	.LBB0_510
# BB#505:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$3, %rsi
	je	.LBB0_510
# BB#506:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$4, %rsi
	je	.LBB0_510
# BB#507:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$7, %rsi
	je	.LBB0_510
# BB#508:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$5, %rsi
	je	.LBB0_510
# BB#509:                               # %lor.lhs.false.i1466
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$6, %rsi
	jne	.LBB0_534
.LBB0_510:                              # %lor.rhs.i1469
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%r13, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %r13
	callq	reg_class_subset_p
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	testq	%rcx, %rax
	je	.LBB0_512
# BB#511:                               # %lor.rhs.i1469
                                        #   in Loop: Header=BB0_439 Depth=2
	decq	%rdx
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	jbe	.LBB0_512
.LBB0_534:                              # %if.end666
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rsi
	movq	%r14, %rdi
	movq	%rcx, %r13
	callq	reg_overlap_mentioned_p
	movq	%r13, %r8
	testq	%r8, %rax
	jne	.LBB0_535
# BB#540:                               # %if.end676
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	128(%rsp), %rdx         # 8-byte Reload
	movq	%r8, %r13
	callq	reg_is_remote_constant_p
	movq	%r13, %rcx
	testq	%rcx, %rax
	jne	.LBB0_541
# BB#544:                               # %if.end683
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	184(%rsp), %r13         # 8-byte Reload
	movabsq	$0, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	je	.LBB0_546
# BB#545:                               # %if.then685
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	184(%rsp), %r13         # 8-byte Reload
	movl	8(%r13), %ecx
	xorq	%rax, %rax
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	movq	136(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str4, %rdx
	movq	%rdx, %rsi
	movq	%r15, %rdx
	movq	%r12, %r8
	callq	fprintf
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
.LBB0_546:                              # %for.cond694
                                        #   Parent Loop BB0_430 Depth=1
                                        #     Parent Loop BB0_439 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r13, %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rax, %r13
	je	.LBB0_547
# BB#548:                               # %for.body696
                                        #   in Loop: Header=BB0_546 Depth=3
	movzbl	(%r13), %eax
	cmpq	$36, %rax
	je	.LBB0_547
# BB#549:                               # %for.body696
                                        #   in Loop: Header=BB0_546 Depth=3
	cmpq	$33, %rax
	je	.LBB0_547
# BB#550:                               # %for.body696
                                        #   in Loop: Header=BB0_546 Depth=3
	cmpq	$34, %rax
	jne	.LBB0_552
# BB#551:                               # %sw.bb1.i
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_547
.LBB0_552:                              # %perhaps_ends_bb_p.exit
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	%r13, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB0_547
# BB#553:                               # %if.else
                                        #   in Loop: Header=BB0_546 Depth=3
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_546
# BB#554:                               # %if.end709
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	32(%r13), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movzbl	(%rax), %eax
	cmpq	$47, %rax
	je	.LBB0_556
# BB#555:                               # %cond.false730
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	%r13, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	single_set_2
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB0_556:                              # %cond.end738
                                        #   in Loop: Header=BB0_546 Depth=3
	incq	144(%rsp)               # 8-byte Folded Spill
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB0_561
# BB#557:                               # %land.lhs.true741
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB0_558
.LBB0_561:                              # %if.end764
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	32(%r13), %rax
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_547
# BB#562:                               # %lor.lhs.false770
                                        #   in Loop: Header=BB0_546 Depth=3
	movq	32(%r13), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_547
# BB#563:                               # %if.end777
                                        #   in Loop: Header=BB0_546 Depth=3
	movzbl	(%r13), %eax
	cmpq	$34, %rax
	jne	.LBB0_546
# BB#564:                               # %if.then782
                                        #   in Loop: Header=BB0_546 Depth=3
	movl	8(%r14), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	32(%rax), %eax
	incq	120(%rsp)               # 8-byte Folded Spill
	testq	%rax, %rax
	jne	.LBB0_546
	jmp	.LBB0_547
.LBB0_512:                              # %if.then662
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rcx, %rax
	cmpq	$0, 176(%rsp)           # 8-byte Folded Reload
	je	.LBB0_514
# BB#513:                               # %if.then662
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
.LBB0_514:                              # %if.then662
                                        #   in Loop: Header=BB0_439 Depth=2
	je	.LBB0_516
	jmp	.LBB0_515
.LBB0_535:                              # %if.then672
                                        #   in Loop: Header=BB0_439 Depth=2
	cmpq	$0, 176(%rsp)           # 8-byte Folded Reload
	je	.LBB0_537
# BB#536:                               # %if.then672
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
.LBB0_537:                              # %if.then672
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	160(%rsp), %rax         # 8-byte Reload
	je	.LBB0_539
# BB#538:                               # %if.then672
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	176(%rsp), %rax         # 8-byte Reload
.LBB0_539:                              # %if.then672
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	152(%rsp), %r13         # 8-byte Reload
	jmp	.LBB0_518
.LBB0_541:                              # %if.then679
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rcx, %rax
	cmpq	$0, 176(%rsp)           # 8-byte Folded Reload
	je	.LBB0_543
# BB#542:                               # %if.then679
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
.LBB0_543:                              # %if.then679
                                        #   in Loop: Header=BB0_439 Depth=2
	je	.LBB0_516
.LBB0_515:                              # %if.then662
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 160(%rsp)         # 8-byte Spill
.LBB0_516:                              # %if.then662
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movq	152(%rsp), %r13         # 8-byte Reload
	jmp	.LBB0_517
.LBB0_558:                              # %if.then747
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	184(%rsp), %rdx         # 8-byte Reload
	callq	validate_replace_rtx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_547
# BB#559:                               # %if.then750
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	addq	$8, %rax
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_565
# BB#560:                               # %if.else757
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%r14, %rdi
	movq	160(%rsp), %rsi         # 8-byte Reload
	movq	184(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdx
	callq	validate_replace_rtx
	movq	recog_data+240(,%r12,8), %rax
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	validate_change
.LBB0_547:                              #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
	movq	152(%rsp), %r13         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB0_517:                              # %for.inc894
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rax, %r8
	jmp	.LBB0_518
	.align	16, 0x90
.LBB0_439:                              # %for.body460
                                        #   Parent Loop BB0_430 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_546 Depth 3
                                        #       Child Loop BB0_467 Depth 3
	movslq	192(%rsp,%r15,4), %r12
	testq	%r12, %r12
	jns	.LBB0_441
# BB#440:                               #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
	jmp	.LBB0_518
	.align	16, 0x90
.LBB0_441:                              # %if.end472
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	recog_data(,%r15,8), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB0_442
# BB#443:                               # %if.end482
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	recog_data(,%r12,8), %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB0_442
# BB#444:                               # %lor.lhs.false487
                                        #   in Loop: Header=BB0_439 Depth=2
	movl	8(%r14), %ecx
	cmpq	$53, %rcx
	jb	.LBB0_442
# BB#445:                               # %lor.lhs.false493
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	reg_n_info(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
	movslq	28(%rcx), %rcx
	testq	%rcx, %rcx
	js	.LBB0_442
# BB#446:                               # %lor.lhs.false493
                                        #   in Loop: Header=BB0_439 Depth=2
	andq	$67108864, %rax         # imm = 0x4000000
	testq	%r8, %rax
	jne	.LBB0_442
# BB#447:                               # %if.end510
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	movq	%r14, %rsi
	movq	%r8, %r13
	callq	operands_match_p
	movq	%r13, %r8
	testq	%r8, %rax
	jne	.LBB0_442
# BB#448:                               # %if.end514
                                        #   in Loop: Header=BB0_439 Depth=2
	movslq	432(%rsp,%r15,4), %rax
	testq	%rax, %rax
	js	.LBB0_450
# BB#449:                               # %if.then520
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	recog_data(,%rax,8), %rdi
	movq	%r14, %rsi
	movq	%r8, %r13
	callq	operands_match_p
	movq	%r13, %r8
	testq	%r8, %rax
	jne	.LBB0_442
.LBB0_450:                              # %if.end531
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	184(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_442
# BB#451:                               # %cond.true539
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	movq	160(%rsp), %r13         # 8-byte Reload
	je	.LBB0_453
# BB#452:                               # %cond.false551
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_453:                              # %cond.end559
                                        #   in Loop: Header=BB0_439 Depth=2
	testq	%rax, %rax
	je	.LBB0_454
# BB#455:                               # %if.end563
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	reg_set_p
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	jne	.LBB0_442
# BB#456:                               # %if.end567
                                        #   in Loop: Header=BB0_439 Depth=2
	movl	312(%rsp,%r15,4), %eax
	testq	%rax, %rax
	jne	.LBB0_442
# BB#457:                               # %lor.lhs.false573
                                        #   in Loop: Header=BB0_439 Depth=2
	movl	312(%rsp,%r12,4), %eax
	cmpq	$1, %rax
	jne	.LBB0_442
# BB#458:                               # %if.end580
                                        #   in Loop: Header=BB0_439 Depth=2
	movl	552(%rsp,%r12,4), %eax
	testq	%rax, %rax
	je	.LBB0_460
# BB#459:                               # %land.lhs.true585
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	movq	%r8, %r13
	callq	count_occurrences
	movq	%r13, %r8
	movq	160(%rsp), %r13         # 8-byte Reload
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jg	.LBB0_442
.LBB0_460:                              # %if.end593
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	recog_data(,%r12,8), %rax
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	8(%rdx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_442
# BB#461:                               # %if.end602
                                        #   in Loop: Header=BB0_439 Depth=2
	movl	8(%r13), %eax
	cmpq	$52, %rax
	ja	.LBB0_500
# BB#462:                               # %if.then608
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jne	.LBB0_442
# BB#463:                               # %land.lhs.true616
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	16(%rcx), %rsi
	movzbl	(%rsi), %edx
	cmpq	$54, %rdx
	jne	.LBB0_442
# BB#464:                               # %land.lhs.true627
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	8(%rcx), %rcx
	cmpq	%r13, %rcx
	jne	.LBB0_442
# BB#465:                               # %land.lhs.true636
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	$1, %rsi
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	jne	.LBB0_454
# BB#466:                               # %for.cond.preheader.i1407
                                        #   in Loop: Header=BB0_439 Depth=2
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movq	184(%rsp), %r12         # 8-byte Reload
.LBB0_467:                              # %for.cond.i1415
                                        #   Parent Loop BB0_430 Depth=1
                                        #     Parent Loop BB0_439 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%r12), %r12
	testq	%r12, %r12
	je	.LBB0_547
# BB#468:                               # %for.body.i1418
                                        #   in Loop: Header=BB0_467 Depth=3
	movzbl	(%r12), %eax
	cmpq	$36, %rax
	je	.LBB0_547
# BB#469:                               # %for.body.i1418
                                        #   in Loop: Header=BB0_467 Depth=3
	cmpq	$33, %rax
	je	.LBB0_547
# BB#470:                               # %for.body.i1418
                                        #   in Loop: Header=BB0_467 Depth=3
	cmpq	$34, %rax
	jne	.LBB0_472
# BB#471:                               # %sw.bb1.i.i1421
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	cfun(%rip), %rax
	cmpq	$0, 176(%rax)
	jne	.LBB0_547
.LBB0_472:                              # %perhaps_ends_bb_p.exit.i1423
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	%r12, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB0_547
# BB#473:                               # %if.else.i1428
                                        #   in Loop: Header=BB0_467 Depth=3
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_467
# BB#474:                               # %if.end11.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movl	8(%r14), %eax
	movq	$1, %rsi
	movq	%r12, %rcx
	movq	%rcx, %rdi
                                        # kill: R12<def> RCX<kill>
	movq	%rax, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB0_476
# BB#475:                               # %if.end11.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	%r12, 104(%rsp)         # 8-byte Spill
.LBB0_476:                              # %if.end11.i
                                        #   in Loop: Header=BB0_467 Depth=3
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	jne	.LBB0_478
# BB#477:                               # %if.end11.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movabsq	$1, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
.LBB0_478:                              # %if.end11.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	movq	96(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, 112(%rsp)         # 8-byte Folded Spill
	cmpq	$105, %rax
	jne	.LBB0_493
# BB#479:                               # %cond.true.i1431
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_481
# BB#480:                               # %cond.false.i1432
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_481:                              # %cond.end45.i
                                        #   in Loop: Header=BB0_467 Depth=3
	testq	%rax, %rax
	je	.LBB0_493
# BB#482:                               # %land.lhs.true.i1435
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	8(%rax), %rcx
	cmpq	%r14, %rcx
	jne	.LBB0_493
# BB#483:                               # %land.lhs.true53.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB0_493
# BB#484:                               # %land.lhs.true61.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	8(%rax), %rcx
	cmpq	%r13, %rcx
	jne	.LBB0_493
# BB#485:                               # %land.lhs.true70.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB0_493
# BB#486:                               # %if.then81.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	144(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	8(%rax), %rax
	subq	%rax, %rcx
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_add3_insn
	testq	%rax, %rax
	je	.LBB0_493
# BB#487:                               # %land.lhs.true96.i
                                        #   in Loop: Header=BB0_467 Depth=3
	xorq	%rcx, %rcx
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_488
.LBB0_493:                              # %if.end131.i1444
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	32(%r12), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	reg_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_547
# BB#494:                               # %if.end138.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movzbl	(%r12), %eax
	cmpq	$34, %rax
	je	.LBB0_495
# BB#498:                               # %if.else171.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	32(%r12), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	reg_set_p
	jmp	.LBB0_499
.LBB0_495:                              # %if.then143.i1446
                                        #   in Loop: Header=BB0_467 Depth=3
	movl	8(%r13), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	je	.LBB0_547
# BB#496:                               # %if.end159.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movl	8(%r14), %eax
	movzbl	call_used_regs(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_547
# BB#497:                               # %lor.lhs.false.i1447
                                        #   in Loop: Header=BB0_467 Depth=3
	movq	96(%rsp), %rax          # 8-byte Reload
	addq	%rax, 120(%rsp)         # 8-byte Folded Spill
	movq	$49, %rsi
	movq	%r12, %rdi
	movq	%r14, %rdx
	callq	find_reg_fusage
.LBB0_499:                              # %if.else171.i
                                        #   in Loop: Header=BB0_467 Depth=3
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_467
	jmp	.LBB0_547
	.align	16, 0x90
.LBB0_442:                              #   in Loop: Header=BB0_439 Depth=2
	movq	%rbx, %r14
	movq	152(%rsp), %r13         # 8-byte Reload
.LBB0_518:                              # %for.inc894
                                        #   in Loop: Header=BB0_439 Depth=2
	incq	%r15
	movsbq	recog_data+996(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%r14, %rbx
	jl	.LBB0_439
	jmp	.LBB0_519
.LBB0_565:                              # %if.then802
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	remove_note
	movq	32(%r13), %rax
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_567
# BB#566:                               # %if.then808
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%r13, %rdx
	movq	56(%rdx), %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 16(%rcx)
	movq	%rcx, 56(%rdx)
                                        # kill: R13<def> RDX<kill>
.LBB0_567:                              # %if.end818
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	$1, %rsi
	movq	%r13, %rdi
	movq	%r14, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_569
# BB#568:                               # %if.then821
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB0_569:                              # %if.end822
                                        #   in Loop: Header=BB0_430 Depth=1
	movslq	8(%r14), %rax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movl	12(%rdx), %eax
	movq	160(%rsp), %rsi         # 8-byte Reload
	movslq	8(%rsi), %rsi
	incq	%rax
	movl	%eax, 12(%rdx)
	movq	32(%rcx,%rsi,8), %rax
	movl	12(%rax), %ecx
	decq	%rcx
	movl	%ecx, 12(%rax)
	movl	32(%rdx), %ecx
	movq	120(%rsp), %rsi         # 8-byte Reload
	addq	%rsi, %rcx
	movl	%ecx, 32(%rdx)
	movl	32(%rax), %ecx
	subq	%rsi, %rcx
	movl	%ecx, 32(%rax)
	movl	28(%rdx), %ecx
	movq	144(%rsp), %rsi         # 8-byte Reload
	addq	%rsi, %rcx
	movl	%ecx, 28(%rdx)
	movslq	28(%rax), %rdx
	testq	%rdx, %rdx
	js	.LBB0_573
# BB#570:                               # %if.then864
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	144(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$2, %rsi
	movq	$2, %rcx
	jl	.LBB0_572
# BB#571:                               # %if.then864
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%rdx, %rcx
.LBB0_572:                              # %if.then864
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	%ecx, 28(%rax)
.LBB0_573:                              # %if.end885
                                        #   in Loop: Header=BB0_430 Depth=1
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	152(%rsp), %r13         # 8-byte Reload
	movq	184(%rsp), %rbx         # 8-byte Reload
	je	.LBB0_595
# BB#574:                               # %if.then887
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	8(%rbx), %ecx
	xorq	%rax, %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str5, %rdx
	movq	%rdx, %rsi
	movq	%r15, %rdx
	movq	%r12, %r8
	callq	fprintf
	movq	16(%rbx), %rbx
	jmp	.LBB0_430
.LBB0_488:                              # %if.then102.i
                                        #   in Loop: Header=BB0_430 Depth=1
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	je	.LBB0_490
# BB#489:                               # %if.then104.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	8(%r14), %eax
	movq	%rax, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	remove_death
	movl	8(%r14), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	28(%rax), %edx
	movq	112(%rsp), %rsi         # 8-byte Reload
	addq	%rsi, %rdx
	movl	%edx, 28(%rax)
	movl	8(%r14), %eax
	movq	32(%rcx,%rax,8), %rax
	movl	32(%rax), %ecx
	movq	120(%rsp), %rdx         # 8-byte Reload
	addq	%rdx, %rcx
	movl	%ecx, 32(%rax)
.LBB0_490:                              # %if.end123.i
                                        #   in Loop: Header=BB0_430 Depth=1
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB0_491
# BB#492:                               # %if.then125.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str6, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
.LBB0_491:                              #   in Loop: Header=BB0_430 Depth=1
	movq	%rbx, %r14
	movq	152(%rsp), %r13         # 8-byte Reload
.LBB0_519:                              # %for.end896
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	176(%rsp), %r15         # 8-byte Reload
	testq	%r15, %r15
	movq	184(%rsp), %rbx         # 8-byte Reload
	je	.LBB0_595
# BB#520:                               # %if.then901
                                        #   in Loop: Header=BB0_430 Depth=1
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB0_595
# BB#521:                               # %land.lhs.true.i1489
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	8(%r15), %ecx
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%rcx,8), %rcx
	movslq	28(%rcx), %rcx
	testq	%rcx, %rcx
	jle	.LBB0_595
# BB#522:                               # %land.lhs.true3.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	(%r14), %ecx
	andq	$67174399, %rcx         # imm = 0x400FFFF
	cmpq	$61, %rcx
	jne	.LBB0_595
# BB#523:                               # %land.lhs.true10.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	8(%r14), %ecx
	movq	32(%rax,%rcx,8), %rax
	movslq	28(%rax), %rax
	testq	%rax, %rax
	jle	.LBB0_595
# BB#524:                               # %land.lhs.true20.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_595
# BB#525:                               # %cond.true.i1494
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_527
# BB#526:                               # %cond.false.i1496
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_527:                              # %cond.end41.i
                                        #   in Loop: Header=BB0_430 Depth=1
	testq	%rax, %rax
	je	.LBB0_595
# BB#528:                               # %land.lhs.true45.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	16(%rax), %rsi
	movq	%r14, %rdi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_595
# BB#529:                               # %land.lhs.true51.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	(%r15), %eax
	movl	(%r14), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB0_595
# BB#530:                               # %if.then.i1501
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rax), %eax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	callq	start_sequence
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	callq	gen_sequence
	movq	%r15, %r12
	movq	%rax, %r15
	callq	end_sequence
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movl	(%rax), %ecx
	movq	168(%rsp), %rdx         # 8-byte Reload
	cmpq	%rcx, %rdx
	movq	%rdx, %rcx
	jne	.LBB0_533
# BB#531:                               # %lor.lhs.false.i1502
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %r12
	movq	%r12, %rdx
	callq	validate_replace_rtx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_575
# BB#532:                               # %lor.lhs.false.if.then68_crit_edge.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%r12, %rbx
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
.LBB0_533:                              # %if.then68.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	%ecx, (%rax)
	movq	16(%rbx), %rbx
	jmp	.LBB0_430
.LBB0_575:                              # %if.end.i1503
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_insn_before
	movq	16(%r12), %r8
	movq	56(%r12), %rbx
	leaq	56(%r8), %r10
	leaq	56(%r12), %rdx
	testq	%rbx, %rbx
	je	.LBB0_581
.LBB0_576:                              # %for.body.lr.ph.i1504
                                        #   Parent Loop BB0_430 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_577 Depth 3
	movq	%r12, %r9
	movq	%r13, %r12
	movq	%rbx, %rsi
	movq	176(%rsp), %rcx         # 8-byte Reload
.LBB0_577:                              # %for.body.i1506
                                        #   Parent Loop BB0_430 Depth=1
                                        #     Parent Loop BB0_576 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rsi), %rax
	movq	16(%rsi), %rbx
	leaq	16(%rsi), %rdi
	cmpq	%rcx, %rax
	je	.LBB0_578
# BB#579:                               # %if.else.i1508
                                        #   in Loop: Header=BB0_577 Depth=3
	movq	%rsi, (%rdx)
	testq	%rbx, %rbx
	movq	%rbx, %rsi
	movq	%rdi, %rdx
	jne	.LBB0_577
	jmp	.LBB0_580
.LBB0_578:                              # %if.then94.i
                                        #   in Loop: Header=BB0_576 Depth=2
	movq	%rsi, (%r10)
	testq	%rbx, %rbx
	movq	%rdi, %r10
	movq	%r12, %r13
	movq	%r9, %r12
	jne	.LBB0_576
	jmp	.LBB0_581
.LBB0_580:                              #   in Loop: Header=BB0_430 Depth=1
	movq	%rdi, %rdx
	movq	%r12, %r13
	movq	%r9, %r12
.LBB0_581:                              # %for.end.i1512
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	$0, (%r10)
	movq	$0, (%rdx)
	movslq	8(%r12), %rbx
	movq	%r12, %r15
	movq	$32, %rcx
	movq	%r13, %rax
	movq	%r13, %r12
	shlq	%cl, %rax
	movl	8(%r8), %ecx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB0_584
# BB#582:                               # %if.then109.i1513
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	regmove_bb_head(%rip), %rax
	movslq	(%rax,%rbx,4), %rcx
	testq	%rcx, %rcx
	js	.LBB0_584
# BB#583:                               # %if.then114.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rcx,8), %rax
	movq	%r8, (%rax)
	movq	regmove_bb_head(%rip), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, (%rax,%rbx,4)
.LBB0_584:                              # %if.end122.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movslq	8(%r14), %rax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movl	12(%rcx), %eax
	incq	%rax
	movl	%eax, 12(%rcx)
	movl	28(%rcx), %eax
	incq	%rax
	movl	%eax, 28(%rcx)
	movl	(%rcx), %eax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rbx
	cmpq	%rbx, %rax
	jne	.LBB0_586
# BB#585:                               # %if.then142.i1518
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	8(%r8), %eax
	movl	%eax, (%rcx)
.LBB0_586:                              # %if.end148.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	176(%rsp), %rax         # 8-byte Reload
	movslq	8(%rax), %r14
	movq	$1, %rsi
	movq	%r8, %rdi
	movq	%rax, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB0_587
# BB#588:                               # %if.then154.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	jmp	.LBB0_589
.LBB0_587:                              # %if.end148.if.end161_crit_edge.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
.LBB0_589:                              # %if.end161.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	(%rax), %ecx
	cmpq	%rbx, %rcx
	jne	.LBB0_591
# BB#590:                               # %if.then169.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, (%rax)
.LBB0_591:                              # %if.end175.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	4(%rax), %ecx
	cmpq	%rbx, %rcx
	jne	.LBB0_593
# BB#592:                               # %if.then182.i1520
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, 4(%rax)
.LBB0_593:                              # %if.end188.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movl	8(%rax), %ecx
	cmpq	%rbx, %rcx
	movq	%r12, %r13
	movq	%r15, %rbx
	jne	.LBB0_595
# BB#594:                               # %if.then195.i
                                        #   in Loop: Header=BB0_430 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, 8(%rax)
	jmp	.LBB0_595
.LBB0_431:                              # %for.cond909.preheader
	movslq	n_basic_blocks(%rip), %r9
	testq	%r9, %r9
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jle	.LBB0_603
# BB#432:
	xorq	%rdi, %rdi
	leaq	-1(%r9), %r8
	andq	%r14, %r8
	.align	16, 0x90
.LBB0_433:                              # %for.body912
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_599 Depth 2
                                        #     Child Loop BB0_597 Depth 2
	movq	basic_block_info(%rip), %r11
	movq	32(%r11,%rdi,8), %r10
	movq	8(%r10), %rax
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_596
# BB#434:                               #   in Loop: Header=BB0_433 Depth=1
	movq	%rax, %rbx
	jmp	.LBB0_602
	.align	16, 0x90
.LBB0_596:                              # %land.lhs.true923.lr.ph
                                        #   in Loop: Header=BB0_433 Depth=1
	movq	%rdi, %rdx
	andq	%r14, %rdx
	cmpq	%r8, %rdx
	movq	%rax, %rbx
	jne	.LBB0_599
	.align	16, 0x90
.LBB0_597:                              # %land.lhs.true923.us
                                        #   Parent Loop BB0_433 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	%rcx, %rax
	movslq	8(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_602
# BB#598:                               # %while.cond.backedge.us
                                        #   in Loop: Header=BB0_597 Depth=2
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	movq	%rax, %rbx
	jne	.LBB0_597
	jmp	.LBB0_602
	.align	16, 0x90
.LBB0_599:                              # %land.lhs.true923
                                        #   Parent Loop BB0_433 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rax
	movslq	8(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_602
# BB#600:                               # %lor.rhs
                                        #   in Loop: Header=BB0_599 Depth=2
	movq	40(%r11,%rdi,8), %rcx
	movq	(%rcx), %rcx
	cmpq	%rax, %rcx
	je	.LBB0_602
# BB#601:                               # %while.cond.backedge
                                        #   in Loop: Header=BB0_599 Depth=2
	movq	24(%rax), %rcx
	testq	%rcx, %rcx
	movq	%rax, %rbx
	jne	.LBB0_599
	.align	16, 0x90
.LBB0_602:                              # %while.end
                                        #   in Loop: Header=BB0_433 Depth=1
	incq	%rdi
	movq	%rbx, 8(%r10)
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r9, %rax
	jl	.LBB0_433
.LBB0_603:                              # %done
	movq	regno_src_regno(%rip), %rdi
	callq	free
	movq	regmove_bb_head(%rip), %rdi
	callq	free
.LBB0_604:                              # %cleanup
	addq	$672, %rsp              # imm = 0x2A0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	regmove_optimize, .Ltmp17-regmove_optimize
	.cfi_endproc

	.align	16, 0x90
	.type	find_matches,@function
find_matches:                           # @find_matches
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
	subq	$160, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 208
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
	movq	%rsi, %r14
	callq	extract_insn
	xorq	%rdi, %rdi
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	callq	constrain_operands
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB1_42
# BB#1:                                 # %if.end
	movsbq	recog_data+996(%rip), %rax
	testq	%rax, %rax
	jle	.LBB1_42
# BB#2:                                 # %for.body.lr.ph
	leaq	-1(%rax), %rdx
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%ebx, 240(%r14,%rdx,4)
	movl	%ebx, (%r14,%rdx,4)
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rdx
	testq	%rsi, %rsi
	jg	.LBB1_3
# BB#4:                                 # %for.cond4.preheader
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	jle	.LBB1_42
# BB#5:
	xorq	%r10, %r10
	movabsq	$0, %r15
	movabsq	$1, %r11
	xorq	%r13, %r13
	.align	16, 0x90
.LBB1_6:                                # %for.body8
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_18 Depth 2
                                        #     Child Loop BB1_14 Depth 2
	movq	recog_data+480(,%r13,8), %rdi
	movl	%r10d, 32(%rsp,%r13,4)
	movl	%r10d, 120(%r14,%r13,4)
	movl	%r10d, 360(%r14,%r13,4)
	movzbl	(%rdi), %eax
	movq	%rbx, %r9
	cmpq	$43, %rax
	jne	.LBB1_7
# BB#9:                                 # %if.then27
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$2, %rax
	jmp	.LBB1_10
	.align	16, 0x90
.LBB1_7:                                # %for.body8
                                        #   in Loop: Header=BB1_6 Depth=1
	cmpq	$61, %rax
	jne	.LBB1_11
# BB#8:                                 # %if.then20
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	$1, %rax
.LBB1_10:                               # %for.cond33.preheader
                                        #   in Loop: Header=BB1_6 Depth=1
	movl	%eax, 120(%r14,%r13,4)
.LBB1_11:                               # %for.cond33.preheader
                                        #   in Loop: Header=BB1_6 Depth=1
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB1_13
# BB#12:                                # %for.cond33.preheader
                                        #   in Loop: Header=BB1_6 Depth=1
	movl	which_alternative(%rip), %r8d
	xorq	%rbx, %rbx
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB1_13
	.align	16, 0x90
.LBB1_18:                               # %for.body38
                                        #   Parent Loop BB1_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rax
	cmpq	$44, %rax
	movq	%r15, %rcx
	jne	.LBB1_20
# BB#19:                                # %for.body38
                                        #   in Loop: Header=BB1_18 Depth=2
	movq	%r11, %rcx
.LBB1_20:                               # %for.body38
                                        #   in Loop: Header=BB1_18 Depth=2
	movzbl	1(%rdi), %eax
	incq	%rdi
	testq	%rax, %rax
	je	.LBB1_13
# BB#21:                                # %for.body38
                                        #   in Loop: Header=BB1_18 Depth=2
	addq	%rcx, %rbx
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB1_18
.LBB1_13:                               # %while.cond.preheader
                                        #   in Loop: Header=BB1_6 Depth=1
	leaq	1(%r13), %r12
	movq	%r9, %rbx
	jmp	.LBB1_14
	.align	16, 0x90
.LBB1_40:                               # %while.cond.backedge
                                        #   in Loop: Header=BB1_14 Depth=2
	movzbl	(%rdx), %eax
	movq	%rdx, %rdi
	movabsq	$1, %r11
.LBB1_14:                               # %while.cond
                                        #   Parent Loop BB1_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	$255, %rax
	je	.LBB1_41
# BB#15:                                # %while.cond
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$44, %rcx
	je	.LBB1_41
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB1_14 Depth=2
	leaq	1(%rdi), %rdx
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	addq	$-37, %rsi
	andq	%rbx, %rsi
	cmpq	$85, %rsi
	ja	.LBB1_40
# BB#17:                                # %while.body
                                        #   in Loop: Header=BB1_14 Depth=2
	jmpq	*.LJTI1_0(,%rsi,8)
.LBB1_30:                               # %sw.bb92
                                        #   in Loop: Header=BB1_14 Depth=2
	leaq	-81(%rax), %rcx
	andq	$255, %rcx
	cmpq	$36, %rcx
	ja	.LBB1_34
# BB#31:                                # %sw.bb92
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$107376279552, %rdi     # imm = 0x1900200000
	testq	%rdi, %rsi
	je	.LBB1_32
# BB#36:                                # %cond.false554
                                        #   in Loop: Header=BB1_14 Depth=2
	addq	$-65, %rax
	andq	$255, %rax
	cmpq	$33, %rax
	movabsq	$0, %r10
	ja	.LBB1_40
# BB#37:                                # %cond.false554
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movabsq	$8590196745, %rax       # imm = 0x200040009
	jmp	.LBB1_38
.LBB1_24:                               # %sw.bb66
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$10, %rdx
	leaq	24(%rsp), %rsi
	callq	strtoul
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movq	24(%rsp), %rdx
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jge	.LBB1_27
# BB#25:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	32(%rsp,%rsi,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_26
.LBB1_27:                               # %if.end75
                                        #   in Loop: Header=BB1_14 Depth=2
	movl	%eax, (%r14,%r13,4)
	movslq	240(%r14,%r13,4), %rcx
	testq	%rcx, %rcx
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	js	.LBB1_28
# BB#29:                                # %if.then84
                                        #   in Loop: Header=BB1_14 Depth=2
	movl	%eax, (%r14,%rcx,4)
	xorq	%r10, %r10
	jmp	.LBB1_40
.LBB1_32:                               # %sw.bb92
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$8589934595, %rdi       # imm = 0x200000003
	testq	%rdi, %rsi
	movabsq	$0, %r10
	jne	.LBB1_40
# BB#33:                                # %sw.bb92
                                        #   in Loop: Header=BB1_14 Depth=2
	cmpq	$16, %rcx
	je	.LBB1_39
.LBB1_34:                               # %cond.false244
                                        #   in Loop: Header=BB1_14 Depth=2
	addq	$-65, %rax
	andq	$255, %rax
	cmpq	$35, %rax
	ja	.LBB1_40
# BB#35:                                # %cond.false244
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movabsq	$60129804297, %rax      # imm = 0xE00040009
.LBB1_38:                               # %cond.false554
                                        #   in Loop: Header=BB1_14 Depth=2
	testq	%rax, %rsi
	je	.LBB1_40
.LBB1_39:                               # %if.then1169
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$1, %rax
	movl	%eax, 32(%rsp,%r13,4)
	jmp	.LBB1_40
.LBB1_28:                               #   in Loop: Header=BB1_14 Depth=2
	xorq	%r10, %r10
	jmp	.LBB1_40
.LBB1_26:                               #   in Loop: Header=BB1_14 Depth=2
	xorq	%r10, %r10
	jmp	.LBB1_40
.LBB1_23:                               # %sw.bb58
                                        #   in Loop: Header=BB1_14 Depth=2
	movl	%r12d, 240(%r14,%r13,4)
	movl	%r13d, 244(%r14,%r13,4)
	jmp	.LBB1_40
.LBB1_22:                               # %sw.bb
                                        #   in Loop: Header=BB1_14 Depth=2
	movq	$1, %rax
	movl	%eax, 360(%r14,%r13,4)
	jmp	.LBB1_40
	.align	16, 0x90
.LBB1_41:                               # %for.inc1173
                                        #   in Loop: Header=BB1_6 Depth=1
	movsbq	recog_data+996(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%r12, %r13
	jl	.LBB1_6
.LBB1_42:                               # %cleanup
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	find_matches, .Ltmp35-find_matches
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_23
	.quad	.LBB1_22
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_24
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30
	.quad	.LBB1_30

	.text
	.align	16, 0x90
	.type	reg_is_remote_constant_p,@function
reg_is_remote_constant_p:               # @reg_is_remote_constant_p
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
	movq	%rdi, %r15
	movl	8(%r15), %eax
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	12(%rax), %eax
	movq	%rdx, %r13
	movq	%rsi, %r12
	xorq	%r14, %r14
	cmpq	$1, %rax
	jne	.LBB2_23
# BB#1:                                 # %if.end
	movq	48(%r12), %rbx
	jmp	.LBB2_2
	.align	16, 0x90
.LBB2_20:                               # %for.inc
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	16(%rbx), %rbx
.LBB2_2:                                # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB2_3
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB2_2 Depth=1
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB2_20
# BB#14:                                # %if.end7
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_20
# BB#15:                                # %cond.true
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB2_17
# BB#16:                                # %cond.false
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%rax, %rsi
	callq	single_set_2
.LBB2_17:                               # %cond.end44
                                        #   in Loop: Header=BB2_2 Depth=1
	testq	%rax, %rax
	je	.LBB2_20
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_20
# BB#19:                                # %land.lhs.true55
                                        #   in Loop: Header=BB2_2 Depth=1
	movl	8(%rax), %eax
	movl	8(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB2_20
	jmp	.LBB2_23
	.align	16, 0x90
.LBB2_21:                               # %for.inc144
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	24(%r13), %r13
.LBB2_3:                                # %for.cond72.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r13
	je	.LBB2_23
# BB#4:                                 # %for.cond72.preheader
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	%r12, %r13
	je	.LBB2_23
# BB#5:                                 # %for.body76
                                        #   in Loop: Header=BB2_3 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_21
# BB#6:                                 # %cond.true94
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB2_8
# BB#7:                                 # %cond.false106
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB2_8:                                # %cond.end114
                                        #   in Loop: Header=BB2_3 Depth=1
	testq	%rax, %rax
	je	.LBB2_21
# BB#9:                                 # %land.lhs.true118
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_21
# BB#10:                                # %land.lhs.true126
                                        #   in Loop: Header=BB2_3 Depth=1
	movl	8(%rax), %eax
	movl	8(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB2_21
# BB#11:                                # %if.then138
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB2_12
# BB#22:                                # %if.then138
	movabsq	$1, %r14
	jmp	.LBB2_23
.LBB2_12:
	movabsq	$0, %r14
.LBB2_23:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	reg_is_remote_constant_p, .Ltmp52-reg_is_remote_constant_p
	.cfi_endproc

	.globl	combine_stack_adjustments
	.align	16, 0x90
	.type	combine_stack_adjustments,@function
combine_stack_adjustments:              # @combine_stack_adjustments
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
	subq	$80, %rsp
.Ltmp64:
	.cfi_def_cfa_offset 128
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
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB3_59
# BB#1:                                 # %for.body.lr.ph
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_3 Depth 2
                                        #       Child Loop BB3_20 Depth 3
                                        #       Child Loop BB3_25 Depth 3
                                        #       Child Loop BB3_51 Depth 3
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rsi,8), %r15
	movq	(%r15), %r14
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	xorq	%r13, %r13
	jmp	.LBB3_3
.LBB3_15:                               # %cond.true52.i
                                        #   in Loop: Header=BB3_3 Depth=2
	testq	%rax, %rax
	js	.LBB3_23
# BB#16:                                # %if.then58.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%rax, %r15
	leaq	(%r14,%r15), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rcx
	negq	%rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	try_apply_stack_adjustment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_17
.LBB3_21:                               # %if.end67.i
                                        #   in Loop: Header=BB3_3 Depth=2
	testq	%r15, %r15
	jne	.LBB3_23
# BB#22:                                # %if.then72.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%r12, %rdi
	callq	delete_insn
.LBB3_23:                               # %if.end74.i
                                        #   in Loop: Header=BB3_3 Depth=2
	testq	%r13, %r13
	je	.LBB3_24
	.align	16, 0x90
.LBB3_25:                               # %for.body.i311.i
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%r13), %r15
	movq	%r13, %rdi
	callq	free
	testq	%r15, %r15
	movq	%r15, %r13
	jne	.LBB3_25
.LBB3_24:                               #   in Loop: Header=BB3_3 Depth=2
	xorq	%r13, %r13
.LBB3_10:                               #   in Loop: Header=BB3_3 Depth=2
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	%r14, 32(%rsp)          # 8-byte Spill
	jmp	.LBB3_11
.LBB3_17:                               # %if.then62.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	testq	%r13, %r13
	je	.LBB3_18
	.align	16, 0x90
.LBB3_20:                               # %for.body.i.i
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%r13), %r14
	movq	%r13, %rdi
	callq	free
	testq	%r14, %r14
	movq	%r14, %r13
	jne	.LBB3_20
.LBB3_18:                               #   in Loop: Header=BB3_3 Depth=2
	xorq	%r13, %r13
	movq	%r12, 56(%rsp)          # 8-byte Spill
.LBB3_19:                               # %for.inc.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB3_11:                               # %for.inc.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%rbx, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB3_53
.LBB3_41:                               # %if.then184.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
	xorq	%r13, %r13
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	jmp	.LBB3_11
	.align	16, 0x90
.LBB3_3:                                # %for.body.i
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_20 Depth 3
                                        #       Child Loop BB3_25 Depth 3
                                        #       Child Loop BB3_51 Depth 3
	movq	%r14, %r12
	movzbl	(%r12), %eax
	movq	8(%r15), %rbx
	movzbl	rtx_class(%rax), %eax
	movq	24(%r12), %r14
	cmpq	$105, %rax
	jne	.LBB3_54
# BB#4:                                 # %if.end.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	%r15, %rbx
	movq	%r12, %rdi
	callq	single_set_for_csa
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB3_31
# BB#5:                                 # %if.then5.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%r14), %r15
	movq	global_rtl+16(%rip), %rdi
	movq	16(%r14), %rsi
	cmpq	%rdi, %r15
	jne	.LBB3_26
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movzbl	(%rsi), %eax
	cmpq	$75, %rax
	jne	.LBB3_26
# BB#7:                                 # %land.lhs.true18.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%rsi), %rax
	cmpq	%r15, %rax
	jne	.LBB3_26
# BB#8:                                 # %land.lhs.true24.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	16(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB3_26
# BB#9:                                 # %if.then32.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%rax), %r14
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	movq	32(%rsp), %rax          # 8-byte Reload
	je	.LBB3_10
# BB#12:                                # %cond.true.i
                                        #   in Loop: Header=BB3_3 Depth=2
	testq	%r14, %r14
	jg	.LBB3_15
# BB#13:                                # %if.then45.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%rax, %r15
	leaq	(%r14,%r15), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%r14, %rcx
	callq	try_apply_stack_adjustment
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_21
# BB#14:                                # %if.then48.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%r12, %rdi
	callq	delete_insn
	jmp	.LBB3_19
	.align	16, 0x90
.LBB3_26:                               # %if.end75.i
                                        #   in Loop: Header=BB3_3 Depth=2
	testq	%r13, %r13
	jne	.LBB3_31
# BB#27:                                # %land.lhs.true78.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movl	(%r15), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB3_31
# BB#28:                                # %land.lhs.true83.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	addq	$8, %r14
	cmpq	$100, %rcx
	jne	.LBB3_29
# BB#33:                                # %land.lhs.true106.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	16(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB3_31
# BB#34:                                # %land.lhs.true117.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%rcx), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB3_31
# BB#35:                                # %land.lhs.true129.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB3_31
# BB#36:                                # %land.lhs.true143.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%rcx), %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rcx
	jne	.LBB3_31
	jmp	.LBB3_37
.LBB3_29:                               # %land.lhs.true83.i
                                        #   in Loop: Header=BB3_3 Depth=2
	cmpq	$96, %rcx
	jne	.LBB3_31
# BB#30:                                # %land.lhs.true91.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, 32(%rsp)          # 8-byte Folded Reload
	jne	.LBB3_31
.LBB3_37:                               # %land.lhs.true159.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	8(%rax), %rax
	cmpq	%rdi, %rax
	jne	.LBB3_31
# BB#38:                                # %land.lhs.true168.i
                                        #   in Loop: Header=BB3_3 Depth=2
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_31
# BB#39:                                # %land.lhs.true171.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	global_rtl+16(%rip), %rsi
	movzbl	2(%r15), %edi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_31
# BB#40:                                # %land.lhs.true177.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	global_rtl+16(%rip), %rsi
	movq	%r15, %rdi
	callq	replace_equiv_address
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_41
	.align	16, 0x90
.LBB3_31:                               # %if.end187.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	%r12, 64(%rsp)
	movq	%r13, 72(%rsp)
	movzbl	(%r12), %eax
	cmpq	$34, %rax
	je	.LBB3_32
# BB#42:                                # %if.end187.i
                                        #   in Loop: Header=BB3_3 Depth=2
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	movq	%rbx, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB3_44
# BB#43:                                # %land.lhs.true196.i
                                        #   in Loop: Header=BB3_3 Depth=2
	leaq	32(%r12), %rdi
	movabsq	$record_stack_memrefs, %rsi
	leaq	64(%rsp), %rdx
	callq	for_each_rtx
	movq	72(%rsp), %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_53
	jmp	.LBB3_44
.LBB3_32:                               #   in Loop: Header=BB3_3 Depth=2
	movq	%rbx, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
.LBB3_44:                               # %if.end204.i
                                        #   in Loop: Header=BB3_3 Depth=2
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
	je	.LBB3_45
# BB#46:                                # %land.lhs.true207.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movzbl	(%r12), %eax
	cmpq	$34, %rax
	je	.LBB3_48
# BB#47:                                # %lor.lhs.false212.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	global_rtl+16(%rip), %rdi
	movq	32(%r12), %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_54
.LBB3_48:                               # %if.then218.i
                                        #   in Loop: Header=BB3_3 Depth=2
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB3_50
# BB#49:                                # %if.then223.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
.LBB3_50:                               # %if.end225.i
                                        #   in Loop: Header=BB3_3 Depth=2
	testq	%r13, %r13
	je	.LBB3_52
	.align	16, 0x90
.LBB3_51:                               # %for.body.i323.i
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%r13), %rbx
	movq	%r13, %rdi
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %r13
	jne	.LBB3_51
.LBB3_52:                               #   in Loop: Header=BB3_3 Depth=2
	xorq	%r13, %r13
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
.LBB3_53:                               # %for.inc.i
                                        #   in Loop: Header=BB3_3 Depth=2
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB3_54
.LBB3_45:                               #   in Loop: Header=BB3_3 Depth=2
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB3_54:                               # %for.inc.i
                                        #   in Loop: Header=BB3_3 Depth=2
	cmpq	%rbx, %r12
	jne	.LBB3_3
# BB#55:                                # %for.end.i
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB3_58
# BB#56:                                # %for.end.i
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB3_58
# BB#57:                                # %if.then231.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insn
.LBB3_58:                               # %combine_stack_adjustments_for_block.exit
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB3_2
.LBB3_59:                               # %for.end
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp70:
	.size	combine_stack_adjustments, .Ltmp70-combine_stack_adjustments
	.cfi_endproc

	.align	16, 0x90
	.type	single_set_for_csa,@function
single_set_for_csa:                     # @single_set_for_csa
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 16
.Ltmp74:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	(%rbx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB4_5
# BB#1:                                 # %cond.true
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB4_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB4_3:                                # %cond.end15
	testq	%rax, %rax
	jne	.LBB4_15
# BB#4:                                 # %cond.end15.if.end_crit_edge
	movl	(%rbx), %ecx
.LBB4_5:                                # %if.end
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$32, %rcx
	jne	.LBB4_15
# BB#6:                                 # %lor.lhs.false
	movq	32(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$39, %rdx
	jne	.LBB4_15
# BB#7:                                 # %if.end29
	movq	8(%rcx), %rsi
	movq	8(%rsi), %r8
	movzbl	(%r8), %ecx
	cmpq	$47, %rcx
	jne	.LBB4_15
# BB#8:                                 # %for.cond.preheader
	movslq	(%rsi), %r9
	cmpq	$2, %r9
	jl	.LBB4_14
# BB#9:
	movq	$2, %rbx
	.align	16, 0x90
.LBB4_10:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi,%rbx,8), %rdx
	movl	(%rdx), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB4_12
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_10 Depth=1
	movq	8(%rdx), %rcx
	movq	16(%rdx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB4_13
.LBB4_12:                               # %if.else
                                        #   in Loop: Header=BB4_10 Depth=1
	andq	$65534, %rdi            # imm = 0xFFFE
	cmpq	$48, %rdi
	jne	.LBB4_15
.LBB4_13:                               # %for.inc
                                        #   in Loop: Header=BB4_10 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%r9, %rdx
	jl	.LBB4_10
.LBB4_14:
	movq	%r8, %rax
.LBB4_15:                               # %return
	popq	%rbx
	retq
.Ltmp75:
	.size	single_set_for_csa, .Ltmp75-single_set_for_csa
	.cfi_endproc

	.align	16, 0x90
	.type	try_apply_stack_adjustment,@function
try_apply_stack_adjustment:             # @try_apply_stack_adjustment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp82:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp83:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp84:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp85:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp86:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp87:
	.cfi_def_cfa_offset 64
.Ltmp88:
	.cfi_offset %rbx, -48
.Ltmp89:
	.cfi_offset %r12, -40
.Ltmp90:
	.cfi_offset %r13, -32
.Ltmp91:
	.cfi_offset %r14, -24
.Ltmp92:
	.cfi_offset %r15, -16
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	callq	single_set_for_csa
	movq	16(%rax), %r13
	addq	$16, %r13
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	validate_change
	testq	%r14, %r14
	je	.LBB5_3
# BB#1:
	movq	%r14, %r15
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r15), %rbx
	movq	(%r15), %rsi
	movq	8(%r15), %r12
	movq	(%rbx), %r13
	movq	global_rtl+16(%rip), %rdi
	movq	8(%rsp), %rax           # 8-byte Reload
	subq	%rax, %rsi
	callq	plus_constant_wide
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	replace_equiv_address_nv
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	validate_change
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB5_2
.LBB5_3:                                # %for.end
	movq	8(%rsp), %rbx           # 8-byte Reload
	callq	apply_change_group
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	je	.LBB5_4
# BB#5:                                 # %for.end
	movabsq	$1, %rax
	jmp	.LBB5_6
.LBB5_4:
	movabsq	$0, %rax
.LBB5_6:                                # %for.end
	testq	%rcx, %rcx
	je	.LBB5_10
# BB#7:                                 # %for.end
	testq	%r14, %r14
	je	.LBB5_10
# BB#8:
	movq	$1, %rax
	.align	16, 0x90
.LBB5_9:                                # %for.body15
                                        # =>This Inner Loop Header: Depth=1
	subq	%rbx, (%r14)
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB5_9
.LBB5_10:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp93:
	.size	try_apply_stack_adjustment, .Ltmp93-try_apply_stack_adjustment
	.cfi_endproc

	.align	16, 0x90
	.type	record_stack_memrefs,@function
record_stack_memrefs:                   # @record_stack_memrefs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp100:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp102:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp103:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp104:
	.cfi_def_cfa_offset 48
.Ltmp105:
	.cfi_offset %rbx, -48
.Ltmp106:
	.cfi_offset %r12, -40
.Ltmp107:
	.cfi_offset %r13, -32
.Ltmp108:
	.cfi_offset %r14, -24
.Ltmp109:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rbx
	movq	%rsi, %r14
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB6_16
# BB#1:                                 # %if.end
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB6_2
# BB#14:                                # %sw.bb10
	movl	8(%rbx), %eax
	cmpq	$7, %rax
	movq	$1, %rax
	je	.LBB6_16
	jmp	.LBB6_15
.LBB6_2:                                # %if.end
	cmpq	$66, %rax
	jne	.LBB6_15
# BB#3:                                 # %sw.bb
	movq	global_rtl+16(%rip), %rdi
	movq	%rbx, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB6_4
# BB#5:                                 # %if.end3
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	movq	$1, %rax
	jne	.LBB6_16
# BB#6:                                 # %if.end.i
	movq	8(%rbx), %rcx
	movq	global_rtl+16(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB6_10
# BB#7:                                 # %if.end3.i
	movzbl	(%rcx), %esi
	cmpq	$75, %rsi
	jne	.LBB6_16
# BB#8:                                 # %land.lhs.true.i
	movq	8(%rcx), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB6_16
# BB#9:                                 # %land.lhs.true11.i
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB6_16
.LBB6_10:                               # %if.then6
	movq	(%r14), %rbx
	movq	8(%r14), %r13
	movq	$32, %rdi
	callq	xmalloc
	movq	(%r15), %rcx
	movq	8(%rcx), %rcx
	movq	global_rtl+16(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB6_11
# BB#12:                                # %if.else.i
	movq	16(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, (%rax)
	jmp	.LBB6_13
.LBB6_15:                               # %sw.epilog
	xorq	%rax, %rax
.LBB6_16:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_4:
	movq	%r12, %rax
	jmp	.LBB6_16
.LBB6_11:                               # %if.then.i
	movq	$0, (%rax)
.LBB6_13:                               # %record_one_stack_memref.exit
	movq	%rbx, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r13, 24(%rax)
	movq	%rax, 8(%r14)
	movq	%r12, %rax
	jmp	.LBB6_16
.Ltmp110:
	.size	record_stack_memrefs, .Ltmp110-record_stack_memrefs
	.cfi_endproc

	.align	16, 0x90
	.type	stable_and_no_regs_but_for_p,@function
stable_and_no_regs_but_for_p:           # @stable_and_no_regs_but_for_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp121:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp122:
	.cfi_def_cfa_offset 64
.Ltmp123:
	.cfi_offset %rbx, -48
.Ltmp124:
	.cfi_offset %r12, -40
.Ltmp125:
	.cfi_offset %r13, -32
.Ltmp126:
	.cfi_offset %r14, -24
.Ltmp127:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movzbl	(%r13), %eax
	movsbq	rtx_class(%rax), %rcx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r15
	addq	$-49, %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$62, %rcx
	ja	.LBB7_18
# BB#1:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$1688849860265991, %rsi # imm = 0x6000000000807
	testq	%rsi, %rdx
	je	.LBB7_2
# BB#10:                                # %sw.bb
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB7_11
# BB#12:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r12
	decq	%rbx
	.align	16, 0x90
.LBB7_13:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12,%rbx), %eax
	cmpq	$101, %rax
	jne	.LBB7_15
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB7_13 Depth=1
	movq	8(%r13,%rbx,8), %rdi
	movq	%r15, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	stable_and_no_regs_but_for_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	je	.LBB7_21
.LBB7_15:                               # %for.cond.backedge
                                        #   in Loop: Header=BB7_13 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB7_13
# BB#16:
	movq	$1, %rax
	jmp	.LBB7_21
.LBB7_11:
	movq	$1, %rax
	jmp	.LBB7_21
.LBB7_2:                                # %entry
	cmpq	$62, %rcx
	jne	.LBB7_18
# BB#3:                                 # %sw.bb14
	cmpq	$66, %rax
	jne	.LBB7_4
# BB#17:                                # %land.lhs.true25
	movq	8(%r13), %rdi
	movq	%r15, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	stable_and_no_regs_but_for_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	jne	.LBB7_18
	jmp	.LBB7_21
.LBB7_4:                                # %sw.bb14
	cmpq	$61, %rax
	jne	.LBB7_18
# BB#5:                                 # %if.then17
	cmpq	%r15, %r13
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB7_7
# BB#6:                                 # %if.then17
	movq	%rdx, %rax
.LBB7_7:                                # %if.then17
	movq	8(%rsp), %rsi           # 8-byte Reload
	cmpq	%rsi, %r13
	jne	.LBB7_9
# BB#8:                                 # %if.then17
	movq	%rdx, %rcx
.LBB7_9:                                # %if.then17
	orq	%rcx, %rax
	jmp	.LBB7_21
.LBB7_18:                               # %sw.default
	movq	%r13, %rdi
	callq	rtx_unstable_p
	testq	%r14, %rax
	jne	.LBB7_19
# BB#20:                                # %sw.default
	movabsq	$1, %rax
	jmp	.LBB7_21
.LBB7_19:
	movabsq	$0, %rax
.LBB7_21:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp128:
	.size	stable_and_no_regs_but_for_p, .Ltmp128-stable_and_no_regs_but_for_p
	.cfi_endproc

	.align	16, 0x90
	.type	replace_in_call_usage,@function
replace_in_call_usage:                  # @replace_in_call_usage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp137:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp138:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp139:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp140:
	.cfi_def_cfa_offset 80
.Ltmp141:
	.cfi_offset %rbx, -48
.Ltmp142:
	.cfi_offset %r12, -40
.Ltmp143:
	.cfi_offset %r13, -32
.Ltmp144:
	.cfi_offset %r14, -24
.Ltmp145:
	.cfi_offset %r15, -16
	movq	%rdi, %r9
	movq	(%r9), %rdi
	movq	%rcx, %r8
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	movq	%rdi, %rcx
	je	.LBB8_12
# BB#1:                                 # %if.end
	movq	%rcx, %rbx
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	movq	%rsi, %rax
	jne	.LBB8_4
# BB#2:                                 # %if.then1
	movl	8(%rbx), %ecx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	%rax, %rcx
	jne	.LBB8_12
# BB#3:                                 # %if.end4
	movq	$1, %rcx
	movq	%r8, %rdi
	movq	%r9, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	validate_change  # TAILCALL
.LBB8_4:                                # %if.end5
	movq	%rbx, %rdi
	movq	%rax, %r9
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movzbl	rtx_length(%rcx), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rax, %rax
	je	.LBB8_12
# BB#5:                                 # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %rsi
	xorq	%r10, %r10
	.align	16, 0x90
.LBB8_6:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_10 Depth 2
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movzbl	(%rsi), %eax
	cmpq	$101, %rax
	jne	.LBB8_7
# BB#13:                                # %if.then14
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rdi, %rbx
	leaq	8(%rbx,%r10,8), %rdi
	movq	%r9, %r13
	movq	%r13, %rsi
	movq	%r8, %rcx
	movq	%rdx, %r15
	movq	%r8, %r14
	movq	%r10, %r12
	callq	replace_in_call_usage
	movq	%r12, %r10
	movq	%r14, %r8
	movq	%r15, %rdx
	jmp	.LBB8_11
	.align	16, 0x90
.LBB8_7:                                # %for.body
                                        #   in Loop: Header=BB8_6 Depth=1
	cmpq	$69, %rax
	movq	%r9, %r13
	movq	%rdi, %rbx
	jne	.LBB8_11
# BB#8:                                 # %for.cond22.preheader
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	8(%rbx,%r10,8), %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB8_11
# BB#9:                                 #   in Loop: Header=BB8_6 Depth=1
	movq	$8, %r15
	movq	$1, %r12
	.align	16, 0x90
.LBB8_10:                               # %for.body28
                                        #   Parent Loop BB8_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r15, %rdi
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %rcx
	movq	%r8, %r14
	movq	%r10, %r13
	callq	replace_in_call_usage
	movq	%r13, %r10
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	%r14, %r8
	movq	8(%rbx,%r10,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$8, %r15
	cmpq	%rax, %rdx
	movq	24(%rsp), %rdx          # 8-byte Reload
	jl	.LBB8_10
	.align	16, 0x90
.LBB8_11:                               # %for.inc37
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rbx, %rdi
	movq	%r13, %r9
	incq	%r10
	movq	8(%rsp), %rsi           # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB8_6
.LBB8_12:                               # %for.end39
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp146:
	.size	replace_in_call_usage, .Ltmp146-replace_in_call_usage
	.cfi_endproc

	.align	16, 0x90
	.type	flags_set_1,@function
flags_set_1:                            # @flags_set_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp148:
	.cfi_def_cfa_offset 16
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	jne	.LBB9_3
# BB#1:                                 # %land.lhs.true
	movq	flags_set_1_rtx(%rip), %rsi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_3
# BB#2:                                 # %if.then
	movq	$1, %rax
	movb	%al, flags_set_1_set(%rip)
.LBB9_3:                                # %if.end
	popq	%rax
	retq
.Ltmp149:
	.size	flags_set_1, .Ltmp149-flags_set_1
	.cfi_endproc

	.type	regno_src_regno,@object # @regno_src_regno
	.local	regno_src_regno
	.comm	regno_src_regno,8,8
	.type	regmove_bb_head,@object # @regmove_bb_head
	.local	regmove_bb_head
	.comm	regmove_bb_head,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Starting %s pass...\n"
	.size	.L.str, 21

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"backward"
	.size	.L.str1, 9

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"forward"
	.size	.L.str2, 8

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Starting backward pass...\n"
	.size	.L.str3, 27

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Could fix operand %d of insn %d matching operand %d.\n"
	.size	.L.str4, 54

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Fixed operand %d of insn %d matching operand %d.\n"
	.size	.L.str5, 50

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Fixed operand of insn %d.\n"
	.size	.L.str6, 27

	.type	flags_set_1_rtx,@object # @flags_set_1_rtx
	.local	flags_set_1_rtx
	.comm	flags_set_1_rtx,8,8
	.type	flags_set_1_set,@object # @flags_set_1_set
	.local	flags_set_1_set
	.comm	flags_set_1_set,1,1

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
