	.file	"global.c.bc"
	.text
	.globl	global_alloc
	.align	16, 0x90
	.type	global_alloc,@function
global_alloc:                           # @global_alloc
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
	subq	$80, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 128
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
	movl	flag_omit_frame_pointer(%rip), %eax
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_2
# BB#1:                                 # %lor.lhs.false
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$64, %rax
	je	.LBB0_3
.LBB0_2:                                # %for.end.thread
	xorq	%rax, %rax
	movq	eliminable_regset(%rip), %rcx
	movl	%eax, max_allocno(%rip)
	movq	$1114112, no_global_alloc_regs(%rip) # imm = 0x110000
	orq	$1114176, %rcx          # imm = 0x110040
	movq	%rcx, eliminable_regset(%rip)
	movq	$1114176, %rax          # imm = 0x110040
.LBB0_5:                                # %if.then19
	movq	%rax, no_global_alloc_regs(%rip)
.LBB0_6:                                # %if.end21
	movq	$0, regs_used_so_far(%rip)
	xorq	%rax, %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_7:                                # %for.body24
                                        # =>This Inner Loop Header: Depth=1
	movzbl	regs_ever_live(%rcx), %edx
	testq	%rdx, %rdx
	jne	.LBB0_9
# BB#8:                                 # %lor.lhs.false27
                                        #   in Loop: Header=BB0_7 Depth=1
	movzbl	call_used_regs(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB0_10
.LBB0_9:                                # %if.then31
                                        #   in Loop: Header=BB0_7 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdx, %rax
	movq	%rax, regs_used_so_far(%rip)
.LBB0_10:                               # %for.inc35
                                        #   in Loop: Header=BB0_7 Depth=1
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB0_7
# BB#11:                                # %for.cond38.preheader
	movslq	max_regno(%rip), %rdi
	cmpq	$54, %rdi
	jb	.LBB0_16
# BB#12:                                # %for.body42.lr.ph
	movq	reg_renumber(%rip), %rdx
	movq	$53, %rsi
	.align	16, 0x90
.LBB0_13:                               # %for.body42
                                        # =>This Inner Loop Header: Depth=1
	movswq	(%rdx,%rsi,2), %rcx
	testq	%rcx, %rcx
	js	.LBB0_15
# BB#14:                                # %if.then47
                                        #   in Loop: Header=BB0_13 Depth=1
	movq	$1, %rbx
	shlq	%cl, %rbx
	orq	%rbx, %rax
	movq	%rax, regs_used_so_far(%rip)
.LBB0_15:                               # %for.inc54
                                        #   in Loop: Header=BB0_13 Depth=1
	incq	%rsi
	cmpq	%rdi, %rsi
	jb	.LBB0_13
.LBB0_16:                               # %for.end56
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, reg_allocno(%rip)
	movq	$255, %rsi
	movq	$212, %rdx
	movq	%rax, %rdi
	callq	memset
	movslq	max_regno(%rip), %rdi
	movq	$4, %rsi
	callq	xcalloc
	movq	regs_may_share(%rip), %rdx
	movq	%rax, reg_may_share(%rip)
	jmp	.LBB0_17
	.align	16, 0x90
.LBB0_29:                               # %if.then86
                                        #   in Loop: Header=BB0_17 Depth=1
	movl	%esi, (%rax,%rdx,4)
	movq	16(%rcx), %rdx
.LBB0_17:                               # %for.end56
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rdx
	je	.LBB0_18
# BB#28:                                # %for.body71
                                        #   in Loop: Header=BB0_17 Depth=1
	movq	16(%rdx), %rcx
	movq	8(%rdx), %rdx
	movq	8(%rcx), %rsi
	movslq	8(%rdx), %rdx
	movslq	8(%rsi), %rsi
	cmpq	%rsi, %rdx
	jg	.LBB0_29
# BB#30:                                # %if.else
                                        #   in Loop: Header=BB0_17 Depth=1
	movl	%edx, (%rax,%rsi,4)
	movq	16(%rcx), %rdx
	jmp	.LBB0_17
.LBB0_18:                               # %for.cond99.preheader
	movl	max_regno(%rip), %ecx
	cmpq	$54, %rcx
	jb	.LBB0_37
# BB#19:                                # %for.body103.lr.ph
	movq	reg_n_info(%rip), %rcx
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_20:                               # %for.body103
                                        # =>This Inner Loop Header: Depth=1
	movq	456(%rcx,%rsi,8), %rdi
	movl	16(%rdi), %ebx
	testq	%rbx, %rbx
	je	.LBB0_35
# BB#21:                                # %land.lhs.true107
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	28(%rdi), %ebx
	cmpq	%r9, %rbx
	je	.LBB0_35
# BB#22:                                # %land.lhs.true113
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	cfun(%rip), %rbx
	movzbl	425(%rbx), %ebx
	testq	$1, %rbx
	je	.LBB0_24
# BB#23:                                # %lor.lhs.false118
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	32(%rdi), %ebx
	testq	%rbx, %rbx
	jne	.LBB0_35
.LBB0_24:                               # %if.then124
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	reg_renumber(%rip), %rbx
	movswq	106(%rbx,%rsi,2), %rbx
	testq	%rbx, %rbx
	jns	.LBB0_32
# BB#25:                                # %land.lhs.true129
                                        #   in Loop: Header=BB0_20 Depth=1
	movslq	212(%rax,%rsi,4), %rbx
	testq	%rbx, %rbx
	je	.LBB0_32
# BB#26:                                # %land.lhs.true132
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	reg_allocno(%rip), %r8
	movslq	(%r8,%rbx,4), %rbx
	testq	%rbx, %rbx
	js	.LBB0_32
# BB#27:                                # %if.then138
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	%ebx, 212(%r8,%rsi,4)
	jmp	.LBB0_33
	.align	16, 0x90
.LBB0_35:                               # %if.else155
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	reg_allocno(%rip), %rdx
	movl	%r9d, 212(%rdx,%rsi,4)
	jmp	.LBB0_36
.LBB0_32:                               # %if.else143
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	max_allocno(%rip), %r8d
	movq	reg_allocno(%rip), %rdx
	leaq	1(%r8), %rbx
	movl	%ebx, max_allocno(%rip)
	movl	%r8d, 212(%rdx,%rsi,4)
.LBB0_33:                               # %if.end146
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	28(%rdi), %edx
	testq	%rdx, %rdx
	je	.LBB0_34
	.align	16, 0x90
.LBB0_36:                               # %for.inc158
                                        #   in Loop: Header=BB0_20 Depth=1
	movslq	max_regno(%rip), %rdx
	leaq	1(%rsi), %rdi
	addq	$54, %rsi
	cmpq	%rdx, %rsi
	movq	%rdi, %rsi
	jb	.LBB0_20
.LBB0_37:                               # %for.end160
	movslq	max_allocno(%rip), %rdi
	movq	$64, %rsi
	callq	xcalloc
	movl	max_regno(%rip), %r15d
	movq	%rax, %r9
	movq	%r9, allocno(%rip)
	movq	$32, %rcx
	movq	%r15, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	$53, %r15
	jbe	.LBB0_38
# BB#39:                                # %for.body167.lr.ph
	movq	reg_allocno(%rip), %r8
	movq	$53, %rbx
	.align	16, 0x90
.LBB0_40:                               # %for.body167
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%r8,%rbx,4), %rdi
	testq	%rdi, %rdi
	js	.LBB0_43
# BB#41:                                # %if.then171
                                        #   in Loop: Header=BB0_40 Depth=1
	movq	cfun(%rip), %rax
	movq	$6, %rcx
	shlq	%cl, %rdi
	movl	%ebx, (%r9,%rdi)
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	2(%rax), %eax
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	reg_n_info(%rip), %rcx
	movl	%eax, 4(%r9,%rdi)
	movq	32(%rcx,%rbx,8), %rax
	movl	32(%rax), %ecx
	movl	8(%r9,%rdi), %edx
	addq	%rcx, %rdx
	movl	%edx, 8(%r9,%rdi)
	movl	16(%rax), %ecx
	movl	12(%r9,%rdi), %edx
	addq	%rcx, %rdx
	movl	%edx, 12(%r9,%rdi)
	movl	20(%rax), %ecx
	movl	16(%r9,%rdi), %edx
	addq	%rcx, %rdx
	movl	%edx, 16(%r9,%rdi)
	movslq	20(%r9,%rdi), %rcx
	movslq	28(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB0_43
# BB#42:                                # %if.then221
                                        #   in Loop: Header=BB0_40 Depth=1
	leaq	20(%r9,%rdi), %rcx
	movl	%eax, (%rcx)
.LBB0_43:                               # %for.inc231
                                        #   in Loop: Header=BB0_40 Depth=1
	incq	%rbx
	cmpq	%r14, %rbx
	jb	.LBB0_40
# BB#44:                                # %for.end233
	movabsq	$local_reg_live_length, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	movabsq	$local_reg_n_refs, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	movabsq	$local_reg_freq, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	cmpq	$54, %r15
	movq	$-53, %r12
	jb	.LBB0_63
# BB#45:                                # %for.body238.lr.ph
	movq	reg_renumber(%rip), %r8
	movq	$53, %r15
	movabsq	$0, %r9
	movabsq	$1, %r10
	.align	16, 0x90
.LBB0_46:                               # %for.body238
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_61 Depth 2
	movswq	(%r8,%r15,2), %rbx
	testq	%rbx, %rbx
	js	.LBB0_62
# BB#47:                                # %if.then243
                                        #   in Loop: Header=BB0_46 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r15,8), %rax
	movzbl	2(%rax), %eax
	movq	%rbx, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB0_51
# BB#48:                                # %if.then243
                                        #   in Loop: Header=BB0_46 Depth=1
	leaq	-21(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB0_51
# BB#49:                                # %if.then243
                                        #   in Loop: Header=BB0_46 Depth=1
	leaq	-45(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB0_51
# BB#50:                                # %if.then243
                                        #   in Loop: Header=BB0_46 Depth=1
	leaq	-29(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB0_51
# BB#55:                                # %cond.false294
                                        #   in Loop: Header=BB0_46 Depth=1
	cmpq	$24, %rax
	jne	.LBB0_56
# BB#58:                                # %cond.true316
                                        #   in Loop: Header=BB0_46 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB0_60
	.align	16, 0x90
.LBB0_51:                               # %cond.true269
                                        #   in Loop: Header=BB0_46 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB0_60
# BB#52:                                # %lor.rhs280
                                        #   in Loop: Header=BB0_46 Depth=1
	cmpq	$6, %rcx
	movq	%r9, %rax
	jne	.LBB0_54
# BB#53:                                # %lor.rhs280
                                        #   in Loop: Header=BB0_46 Depth=1
	movq	%r10, %rax
.LBB0_54:                               # %lor.rhs280
                                        #   in Loop: Header=BB0_46 Depth=1
	incq	%rax
	jmp	.LBB0_60
.LBB0_56:                               # %cond.false294
                                        #   in Loop: Header=BB0_46 Depth=1
	cmpq	$18, %rax
	jne	.LBB0_59
# BB#57:                                # %cond.true303
                                        #   in Loop: Header=BB0_46 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB0_60
.LBB0_59:                               # %cond.end342
                                        #   in Loop: Header=BB0_46 Depth=1
	movl	target_flags(%rip), %r11d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r11
	andq	$4, %r11
	leaq	3(%rax,%r11), %rax
	addq	$4, %r11
	cqto
	idivq	%r11
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_62
	.align	16, 0x90
.LBB0_60:                               # %for.body348.lr.ph
                                        #   in Loop: Header=BB0_46 Depth=1
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%r15,8), %rdx
	addq	%rbx, %rax
	incq	%rbx
	.align	16, 0x90
.LBB0_61:                               # %for.body348
                                        #   Parent Loop BB0_46 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	16(%rdx), %ecx
	movl	local_reg_n_refs-4(,%rbx,4), %edi
	addq	%rcx, %rdi
	movl	%edi, local_reg_n_refs-4(,%rbx,4)
	movl	20(%rdx), %ecx
	movl	local_reg_freq-4(,%rbx,4), %edi
	addq	%rcx, %rdi
	movl	%edi, local_reg_freq-4(,%rbx,4)
	movl	28(%rdx), %ecx
	movl	local_reg_live_length-4(,%rbx,4), %edi
	addq	%rcx, %rdi
	movl	%edi, local_reg_live_length-4(,%rbx,4)
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rsi, %rdi
	jl	.LBB0_61
.LBB0_62:                               # %for.inc374
                                        #   in Loop: Header=BB0_46 Depth=1
	incq	%r15
	cmpq	%r14, %r15
	jb	.LBB0_46
	jmp	.LBB0_63
.LBB0_38:                               # %for.end233.thread
	movabsq	$local_reg_live_length, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	movabsq	$local_reg_n_refs, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	movabsq	$local_reg_freq, %rdi
	xorq	%rsi, %rsi
	movq	$212, %rdx
	callq	memset
	movq	$-53, %r12
	.align	16, 0x90
.LBB0_63:                               # %for.body380
                                        # =>This Inner Loop Header: Depth=1
	movzbl	regs_ever_live+53(%r12), %eax
	testq	%rax, %rax
	je	.LBB0_65
# BB#64:                                # %if.then383
                                        #   in Loop: Header=BB0_63 Depth=1
	xorq	%rax, %rax
	movl	%eax, local_reg_n_refs+212(,%r12,4)
	movl	%eax, local_reg_freq+212(,%r12,4)
.LBB0_65:                               # %for.inc387
                                        #   in Loop: Header=BB0_63 Depth=1
	incq	%r12
	jne	.LBB0_63
# BB#66:                                # %for.end389
	movl	max_allocno(%rip), %eax
	leaq	63(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$57, %rcx
	shrq	%cl, %rdx
	andq	$63, %rdx
	leaq	63(%rax,%rdx), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$38, %rcx
	sarq	%cl, %rdi
	movl	%edi, allocno_row_words(%rip)
	imulq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movslq	allocno_row_words(%rip), %rdi
	movq	%rax, conflicts(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	max_allocno(%rip), %rcx
	movq	%rax, allocnos_live(%rip)
	testq	%rcx, %rcx
	jle	.LBB0_316
# BB#67:                                # %if.then401
	movslq	max_parallel(%rip), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	max_allocno(%rip), %rdi
	movq	%rax, regs_set(%rip)
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rcx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB0_213
# BB#68:
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB0_69:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_73 Depth 2
                                        #       Child Loop BB0_74 Depth 3
                                        #         Child Loop BB0_76 Depth 4
                                        #           Child Loop BB0_94 Depth 5
                                        #     Child Loop BB0_103 Depth 2
                                        #     Child Loop BB0_106 Depth 2
                                        #     Child Loop BB0_110 Depth 2
                                        #       Child Loop BB0_111 Depth 3
                                        #     Child Loop BB0_116 Depth 2
                                        #       Child Loop BB0_117 Depth 3
                                        #     Child Loop BB0_122 Depth 2
                                        #       Child Loop BB0_123 Depth 3
                                        #     Child Loop BB0_128 Depth 2
                                        #       Child Loop BB0_129 Depth 3
                                        #     Child Loop BB0_134 Depth 2
                                        #       Child Loop BB0_135 Depth 3
                                        #     Child Loop BB0_140 Depth 2
                                        #       Child Loop BB0_141 Depth 3
                                        #     Child Loop BB0_146 Depth 2
                                        #       Child Loop BB0_147 Depth 3
                                        #     Child Loop BB0_152 Depth 2
                                        #       Child Loop BB0_153 Depth 3
                                        #     Child Loop BB0_157 Depth 2
                                        #       Child Loop BB0_160 Depth 3
                                        #       Child Loop BB0_167 Depth 3
                                        #         Child Loop BB0_174 Depth 4
                                        #           Child Loop BB0_175 Depth 5
                                        #         Child Loop BB0_207 Depth 4
                                        #       Child Loop BB0_168 Depth 3
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movslq	allocno_row_words(%rip), %rdx
	movq	%rsi, %r14
	movq	allocnos_live(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
	movq	64(%rax), %r15
	movq	$0, hard_regs_live(%rip)
	movabsq	$hard_regs_live, %rdi
	movq	%r15, %rsi
	callq	reg_set_to_hard_reg_set
	movq	(%r15), %r8
	testq	%r8, %r8
	movq	48(%rsp), %rdi          # 8-byte Reload
	je	.LBB0_104
# BB#70:                                # %for.cond22.preheader.lr.ph.i
                                        #   in Loop: Header=BB0_69 Depth=1
	movl	16(%r8), %eax
	testq	%rax, %rax
	movq	$53, %r12
	movq	%rdi, %r14
	je	.LBB0_72
# BB#71:                                # %select.mid
                                        #   in Loop: Header=BB0_69 Depth=1
	xorq	%r12, %r12
.LBB0_72:                               # %select.end
                                        #   in Loop: Header=BB0_69 Depth=1
	xorq	%r9, %r9
	.align	16, 0x90
.LBB0_73:                               # %for.cond22.preheader.i
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_74 Depth 3
                                        #         Child Loop BB0_76 Depth 4
                                        #           Child Loop BB0_94 Depth 5
	xorq	%r10, %r10
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_74:                               # %for.body25.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_73 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_76 Depth 4
                                        #           Child Loop BB0_94 Depth 5
	movq	24(%r8,%r10,8), %r11
	testq	%r11, %r11
	je	.LBB0_99
# BB#75:                                # %for.body34.preheader.i
                                        #   in Loop: Header=BB0_74 Depth=3
	movq	$6, %rcx
	movq	%r10, %r15
	shlq	%cl, %r15
	movq	%r12, %r13
	andq	%rsi, %r13
	.align	16, 0x90
.LBB0_76:                               # %for.body34.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_73 Depth=2
                                        #       Parent Loop BB0_74 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_94 Depth 5
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%r11, %rdx
	je	.LBB0_98
# BB#77:                                # %if.then37.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movl	16(%r8), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	(%r13,%r15), %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	reg_allocno(%rip), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movslq	(%rax,%rbx,4), %rax
	notq	%rdx
	andq	%rdx, %r11
	testq	%rax, %rax
	js	.LBB0_79
# BB#78:                                # %if.then47.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movq	%rax, %rdx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdx
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	movq	allocnos_live(%rip), %rbx
	movq	$3, %rcx
	shrq	%cl, %rdx
	orq	%rdi, (%rbx,%rdx)
	movq	$32, %rcx
	leaq	1(%r9), %rdx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movl	%eax, (%r14,%r9,4)
	movq	%rdx, %r9
	jmp	.LBB0_97
	.align	16, 0x90
.LBB0_79:                               # %if.else.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rdi
	testq	%rdi, %rdi
	js	.LBB0_97
# BB#80:                                # %if.then59.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	2(%rax), %eax
	movq	%rdi, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB0_84
# BB#81:                                # %if.then59.i
                                        #   in Loop: Header=BB0_76 Depth=4
	leaq	-21(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_84
# BB#82:                                # %if.then59.i
                                        #   in Loop: Header=BB0_76 Depth=4
	leaq	-45(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_84
# BB#83:                                # %if.then59.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movq	%rdi, %rcx
	addq	$-29, %rcx
	andq	%rsi, %rcx
	cmpq	$7, %rcx
	jbe	.LBB0_84
# BB#88:                                # %cond.false.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	cmpq	$24, %rax
	jne	.LBB0_89
# BB#92:                                # %cond.true22.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB0_93
.LBB0_84:                               # %cond.true.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB0_93
# BB#85:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB0_87
# BB#86:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movabsq	$1, %rax
.LBB0_87:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	incq	%rax
	jmp	.LBB0_93
.LBB0_89:                               # %cond.false.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	cmpq	$18, %rax
	jne	.LBB0_91
# BB#90:                                # %cond.true18.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB0_93
.LBB0_91:                               # %cond.end38.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_96
.LBB0_93:                               # %while.body.lr.ph.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movq	hard_regs_live(%rip), %rdx
	addq	%rdi, %rax
	.align	16, 0x90
.LBB0_94:                               # %while.body.i.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_73 Depth=2
                                        #       Parent Loop BB0_74 Depth=3
                                        #         Parent Loop BB0_76 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%rdx, %rbx
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	orq	%rbx, %rdx
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rbx
	jl	.LBB0_94
# BB#95:                                # %while.cond.while.end_crit_edge.i.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movq	%rdx, hard_regs_live(%rip)
.LBB0_96:                               # %if.end63.i
                                        #   in Loop: Header=BB0_76 Depth=4
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB0_97:                               # %if.end63.i
                                        #   in Loop: Header=BB0_76 Depth=4
	testq	%r11, %r11
	je	.LBB0_99
.LBB0_98:                               # %for.inc.i
                                        #   in Loop: Header=BB0_76 Depth=4
	incq	%r12
	movq	%r12, %rax
	andq	%rsi, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB0_76
.LBB0_99:                               # %if.end70.i
                                        #   in Loop: Header=BB0_74 Depth=3
	incq	%r10
	xorq	%r12, %r12
	cmpq	$2, %r10
	jne	.LBB0_74
# BB#100:                               # %for.end73.i
                                        #   in Loop: Header=BB0_73 Depth=2
	movq	(%r8), %r8
	xorq	%r12, %r12
	testq	%r8, %r8
	jne	.LBB0_73
# BB#101:                               # %do.end77.i
                                        #   in Loop: Header=BB0_69 Depth=1
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %rdi
	jle	.LBB0_104
# BB#102:                               # %while.body.lr.ph.i327.i
                                        #   in Loop: Header=BB0_69 Depth=1
	movq	allocno(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	decq	%r9
	.align	16, 0x90
.LBB0_103:                              # %while.body.i330.i
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	hard_regs_live(%rip), %rdx
	movslq	(%rdi,%r9,4), %rsi
	movq	$6, %rcx
	shlq	%cl, %rsi
	orq	%rdx, 24(%rax,%rsi)
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%r9
	testq	%rdx, %rdx
	jg	.LBB0_103
.LBB0_104:                              # %record_conflicts.exit.i
                                        #   in Loop: Header=BB0_69 Depth=1
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %r11
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	32(%r11,%rsi,8), %r14
	movq	32(%r14), %rax
	jmp	.LBB0_106
	.align	16, 0x90
.LBB0_105:                              # %for.cond82.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movq	(%rax), %rax
.LBB0_106:                              # %for.cond82.i
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB0_157
# BB#107:                               # %for.body83.i
                                        #   in Loop: Header=BB0_106 Depth=2
	movzbl	48(%rax), %ecx
	testq	$2, %rcx
	je	.LBB0_105
# BB#108:                               # %for.cond93.preheader.i
                                        #   in Loop: Header=BB0_69 Depth=1
	movslq	allocno_row_words(%rip), %r9
	testq	%r9, %r9
	jle	.LBB0_157
# BB#109:                               # %for.body.lr.ph.i868
                                        #   in Loop: Header=BB0_69 Depth=1
	movq	%rsi, %r15
	movq	allocnos_live(%rip), %r8
	movq	allocno(%rip), %r10
	leaq	24(%r10), %rax
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_110:                              # %for.body.i875
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_111 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rax, %rbx
	je	.LBB0_114
	.align	16, 0x90
.LBB0_111:                              # %for.body3.i880
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_110 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_113
# BB#112:                               # %if.then5.i883
                                        #   in Loop: Header=BB0_111 Depth=3
	orq	$256, (%rbx)            # imm = 0x100
.LBB0_113:                              # %for.inc.i887
                                        #   in Loop: Header=BB0_111 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rbx
	testq	%rdi, %rdi
	jne	.LBB0_111
.LBB0_114:                              # %for.inc6.i890
                                        #   in Loop: Header=BB0_110 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$4096, %rax             # imm = 0x1000
	testq	%rdi, %rdi
	jg	.LBB0_110
# BB#115:                               # %for.body.i781.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	leaq	24(%r10), %rbx
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_116:                              # %for.body.i781
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_117 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB0_120
	.align	16, 0x90
.LBB0_117:                              # %for.body3.i786
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_116 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_119
# BB#118:                               # %if.then5.i789
                                        #   in Loop: Header=BB0_117 Depth=3
	orq	$512, (%rax)            # imm = 0x200
.LBB0_119:                              # %for.inc.i793
                                        #   in Loop: Header=BB0_117 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rax
	testq	%rdi, %rdi
	jne	.LBB0_117
.LBB0_120:                              # %for.inc6.i796
                                        #   in Loop: Header=BB0_116 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %rbx             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_116
# BB#121:                               # %for.body.i756.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	leaq	24(%r10), %rbx
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_122:                              # %for.body.i756
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_123 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB0_126
	.align	16, 0x90
.LBB0_123:                              # %for.body3.i761
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_122 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_125
# BB#124:                               # %if.then5.i764
                                        #   in Loop: Header=BB0_123 Depth=3
	orq	$1024, (%rax)           # imm = 0x400
.LBB0_125:                              # %for.inc.i768
                                        #   in Loop: Header=BB0_123 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rax
	testq	%rdi, %rdi
	jne	.LBB0_123
.LBB0_126:                              # %for.inc6.i771
                                        #   in Loop: Header=BB0_122 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %rbx             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_122
# BB#127:                               # %for.body.i731.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	leaq	24(%r10), %rbx
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_128:                              # %for.body.i731
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_129 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB0_132
	.align	16, 0x90
.LBB0_129:                              # %for.body3.i736
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_128 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_131
# BB#130:                               # %if.then5.i739
                                        #   in Loop: Header=BB0_129 Depth=3
	orq	$2048, (%rax)           # imm = 0x800
.LBB0_131:                              # %for.inc.i743
                                        #   in Loop: Header=BB0_129 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rax
	testq	%rdi, %rdi
	jne	.LBB0_129
.LBB0_132:                              # %for.inc6.i746
                                        #   in Loop: Header=BB0_128 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %rbx             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_128
# BB#133:                               # %for.body.i706.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	leaq	24(%r10), %rbx
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_134:                              # %for.body.i706
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_135 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB0_138
	.align	16, 0x90
.LBB0_135:                              # %for.body3.i711
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_134 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_137
# BB#136:                               # %if.then5.i714
                                        #   in Loop: Header=BB0_135 Depth=3
	orq	$4096, (%rax)           # imm = 0x1000
.LBB0_137:                              # %for.inc.i718
                                        #   in Loop: Header=BB0_135 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rax
	testq	%rdi, %rdi
	jne	.LBB0_135
.LBB0_138:                              # %for.inc6.i721
                                        #   in Loop: Header=BB0_134 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %rbx             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_134
# BB#139:                               # %for.body.i681.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	leaq	24(%r10), %rbx
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_140:                              # %for.body.i681
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_141 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB0_144
	.align	16, 0x90
.LBB0_141:                              # %for.body3.i686
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_140 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_143
# BB#142:                               # %if.then5.i689
                                        #   in Loop: Header=BB0_141 Depth=3
	orq	$8192, (%rax)           # imm = 0x2000
.LBB0_143:                              # %for.inc.i693
                                        #   in Loop: Header=BB0_141 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rax
	testq	%rdi, %rdi
	jne	.LBB0_141
.LBB0_144:                              # %for.inc6.i696
                                        #   in Loop: Header=BB0_140 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %rbx             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_140
# BB#145:                               # %for.body.i656.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	leaq	24(%r10), %rbx
	movq	%r9, %rsi
	movq	%r8, %rdx
	.align	16, 0x90
.LBB0_146:                              # %for.body.i656
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_147 Depth 3
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rax
	je	.LBB0_150
	.align	16, 0x90
.LBB0_147:                              # %for.body3.i661
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_146 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdi
	je	.LBB0_149
# BB#148:                               # %if.then5.i664
                                        #   in Loop: Header=BB0_147 Depth=3
	orq	$16384, (%rax)          # imm = 0x4000
.LBB0_149:                              # %for.inc.i668
                                        #   in Loop: Header=BB0_147 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rax
	testq	%rdi, %rdi
	jne	.LBB0_147
.LBB0_150:                              # %for.inc6.i671
                                        #   in Loop: Header=BB0_146 Depth=2
	decq	%rsi
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %rbx             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_146
# BB#151:                               # %for.body.i642.preheader
                                        #   in Loop: Header=BB0_69 Depth=1
	addq	$24, %r10
	movq	%r15, %rsi
	.align	16, 0x90
.LBB0_152:                              # %for.body.i642
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_153 Depth 3
	movq	(%r8), %rdx
	testq	%rdx, %rdx
	movq	%r10, %rax
	je	.LBB0_156
	.align	16, 0x90
.LBB0_153:                              # %for.body3.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_152 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rdx
	je	.LBB0_155
# BB#154:                               # %if.then5.i
                                        #   in Loop: Header=BB0_153 Depth=3
	orq	$32768, (%rax)          # imm = 0x8000
.LBB0_155:                              # %for.inc.i646
                                        #   in Loop: Header=BB0_153 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rdx
	addq	$64, %rax
	testq	%rdx, %rdx
	jne	.LBB0_153
.LBB0_156:                              # %for.inc6.i
                                        #   in Loop: Header=BB0_152 Depth=2
	decq	%r9
	addq	$8, %r8
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %r10             # imm = 0x1000
	testq	%rax, %rax
	jg	.LBB0_152
	jmp	.LBB0_157
	.align	16, 0x90
.LBB0_211:                              # %if.end249.i
                                        #   in Loop: Header=BB0_157 Depth=2
	addq	$24, %r14
.LBB0_157:                              # %while.body106.i
                                        #   Parent Loop BB0_69 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_160 Depth 3
                                        #       Child Loop BB0_167 Depth 3
                                        #         Child Loop BB0_174 Depth 4
                                        #           Child Loop BB0_175 Depth 5
                                        #         Child Loop BB0_207 Depth 4
                                        #       Child Loop BB0_168 Depth 3
	movq	(%r14), %r14
	movl	(%r14), %eax
	xorq	%rcx, %rcx
	movl	%ecx, n_regs_set(%rip)
	movq	%rax, %rcx
	andq	$65533, %rcx            # imm = 0xFFFD
	cmpq	$32, %rcx
	je	.LBB0_159
# BB#158:                               # %while.body106.i
                                        #   in Loop: Header=BB0_157 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB0_210
.LBB0_159:                              # %if.then116.i
                                        #   in Loop: Header=BB0_157 Depth=2
	movq	32(%r14), %rdi
	xorq	%rdx, %rdx
	movabsq	$mark_reg_clobber, %rsi
	callq	note_stores
	movq	56(%r14), %rbx
	jmp	.LBB0_160
	.align	16, 0x90
.LBB0_163:                              # %for.inc134.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	16(%rbx), %rbx
.LBB0_160:                              # %if.then116.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_157 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_164
# BB#161:                               # %for.body123.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB0_163
# BB#162:                               # %if.then129.i
                                        #   in Loop: Header=BB0_160 Depth=3
	movq	8(%rbx), %rdi
	callq	mark_reg_death
	jmp	.LBB0_163
	.align	16, 0x90
.LBB0_164:                              # %for.end138.i
                                        #   in Loop: Header=BB0_157 Depth=2
	movq	32(%r14), %rdi
	xorq	%rdx, %rdx
	movabsq	$mark_reg_store, %rsi
	callq	note_stores
	movq	32(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$39, %rax
	jne	.LBB0_168
# BB#165:                               # %land.lhs.true149.i
                                        #   in Loop: Header=BB0_157 Depth=2
	movq	%r14, %rdi
	callq	multiple_sets
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_168
# BB#166:                               # %for.cond156.preheader.i
                                        #   in Loop: Header=BB0_157 Depth=2
	movq	56(%r14), %r12
	jmp	.LBB0_167
	.align	16, 0x90
.LBB0_208:                              # %for.inc218.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	16(%r12), %r12
.LBB0_167:                              # %for.cond156.preheader.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_157 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_174 Depth 4
                                        #           Child Loop BB0_175 Depth 5
                                        #         Child Loop BB0_207 Depth 4
	testq	%r12, %r12
	je	.LBB0_168
# BB#171:                               # %for.body158.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movl	(%r12), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB0_208
# BB#172:                               # %if.then164.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	32(%r14), %rax
	movq	8(%rax), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB0_208
# BB#173:                               #   in Loop: Header=BB0_167 Depth=3
	movq	8(%r12), %r15
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
.LBB0_174:                              # %for.body177.lr.ph.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_157 Depth=2
                                        #       Parent Loop BB0_167 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_175 Depth 5
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	.align	16, 0x90
.LBB0_175:                              # %for.body177.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_157 Depth=2
                                        #       Parent Loop BB0_167 Depth=3
                                        #         Parent Loop BB0_174 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	32(%r14), %rax
	movq	8(%rax), %rax
	movq	8(%rax,%rbx,8), %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	jne	.LBB0_178
# BB#176:                               # %land.lhs.true190.i
                                        #   in Loop: Header=BB0_175 Depth=5
	movq	8(%r13), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	je	.LBB0_178
# BB#177:                               # %land.lhs.true198.i
                                        #   in Loop: Header=BB0_175 Depth=5
	movq	%r15, %rdi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_179
.LBB0_178:                              # %for.cond174.backedge.i
                                        #   in Loop: Header=BB0_175 Depth=5
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB0_175
	jmp	.LBB0_182
	.align	16, 0x90
.LBB0_179:                              # %land.lhs.true204.i
                                        #   in Loop: Header=BB0_174 Depth=4
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	8(%r13), %rsi
	movq	%rax, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%r15, %rdi
                                        # kill: R15<def> RDI<kill>
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_181
# BB#180:                               # %land.lhs.true204.i
                                        #   in Loop: Header=BB0_174 Depth=4
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
.LBB0_181:                              # %land.lhs.true204.i
                                        #   in Loop: Header=BB0_174 Depth=4
	testq	%r13, %r13
	jg	.LBB0_174
.LBB0_182:                              # %for.end213.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB0_208
# BB#183:                               # %if.then215.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB0_185
# BB#184:                               # %if.then.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	8(%r15), %r15
	movl	(%r15), %eax
.LBB0_185:                              # %if.end.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB0_208
# BB#186:                               # %if.end5.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movslq	8(%r15), %rbx
	cmpq	$53, %rbx
	jl	.LBB0_189
# BB#187:                               # %if.then9.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	reg_allocno(%rip), %rax
	movslq	(%rax,%rbx,4), %rax
	testq	%rax, %rax
	js	.LBB0_189
# BB#188:                               # %if.then12.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	%rbx, %rdi
	callq	record_one_conflict
.LBB0_189:                              # %if.end14.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %r13
	testq	%r13, %r13
	jns	.LBB0_191
# BB#190:                               # %if.end14.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	%rbx, %r13
.LBB0_191:                              # %if.end14.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	cmpq	$52, %r13
	jg	.LBB0_208
# BB#192:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	fixed_regs(%rax), %eax
	testq	%rax, %rax
	jne	.LBB0_208
# BB#193:                               # %if.then28.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movzbl	2(%r15), %eax
	movq	%r13, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB0_197
# BB#194:                               # %if.then28.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	leaq	-21(%r13), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_197
# BB#195:                               # %if.then28.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	leaq	-45(%r13), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_197
# BB#196:                               # %if.then28.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	leaq	-29(%r13), %rcx
	andq	%rdx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB0_197
# BB#201:                               # %cond.false.i339.i
                                        #   in Loop: Header=BB0_167 Depth=3
	cmpq	$24, %rax
	jne	.LBB0_202
# BB#204:                               # %cond.true78.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB0_206
.LBB0_197:                              # %cond.true.i336.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rbx
	je	.LBB0_206
# BB#198:                               # %lor.rhs.i338.i
                                        #   in Loop: Header=BB0_167 Depth=3
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, %rbx
	jne	.LBB0_200
# BB#199:                               # %lor.rhs.i338.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movabsq	$1, %rax
	movq	%rax, %rbx
.LBB0_200:                              # %lor.rhs.i338.i
                                        #   in Loop: Header=BB0_167 Depth=3
	incq	%rbx
	jmp	.LBB0_206
.LBB0_202:                              # %cond.false.i339.i
                                        #   in Loop: Header=BB0_167 Depth=3
	cmpq	$18, %rax
	jne	.LBB0_205
# BB#203:                               # %cond.true69.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB0_206
.LBB0_205:                              # %cond.end98.i.i
                                        #   in Loop: Header=BB0_167 Depth=3
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
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_208
.LBB0_206:                              # %while.body.lr.ph.i344.i
                                        #   in Loop: Header=BB0_167 Depth=3
	addq	%r13, %rbx
	.align	16, 0x90
.LBB0_207:                              # %while.body.i346.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_157 Depth=2
                                        #       Parent Loop BB0_167 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r13, %rdi
	callq	record_one_conflict
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB0_207
	jmp	.LBB0_208
	.align	16, 0x90
.LBB0_170:                              # %if.then235.i
                                        #   in Loop: Header=BB0_168 Depth=3
	movq	8(%rax), %rdi
	callq	mark_reg_death
.LBB0_168:                              # %while.cond224.preheader.i
                                        #   Parent Loop BB0_69 Depth=1
                                        #     Parent Loop BB0_157 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	n_regs_set(%rip), %rax
	testq	%rax, %rax
	leaq	-1(%rax), %rax
	movl	%eax, n_regs_set(%rip)
	jle	.LBB0_209
# BB#169:                               # %while.body228.i
                                        #   in Loop: Header=BB0_168 Depth=3
	movq	$32, %rcx
	movq	regs_set(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rax
	movl	8(%rax), %edx
	movq	$10, %rsi
	movq	%r14, %rdi
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB0_168
	jmp	.LBB0_170
	.align	16, 0x90
.LBB0_209:                              # %if.end241.loopexit.i
                                        #   in Loop: Header=BB0_157 Depth=2
	movq	basic_block_info(%rip), %r11
	movq	56(%rsp), %rsi          # 8-byte Reload
.LBB0_210:                              # %if.end241.i
                                        #   in Loop: Header=BB0_157 Depth=2
	movq	32(%r11,%rsi,8), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %r14
	jne	.LBB0_211
# BB#212:                               # %for.inc254.i
                                        #   in Loop: Header=BB0_69 Depth=1
	incq	%rsi
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_69
.LBB0_213:                              # %global_conflicts.exit
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	regs_set(%rip), %rdi
	callq	free
	movslq	max_allocno(%rip), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB0_228
# BB#214:                               # %for.body.lr.ph.i798
	movslq	allocno_row_words(%rip), %rsi
	testq	%rsi, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	jle	.LBB0_229
# BB#215:                               # %for.body.lr.ph.split.us.i
	movq	conflicts(%rip), %rax
	leaq	-1(%rsi), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	incq	%rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	$38, %rcx
	movq	%rsi, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	movq	%rsi, %rdx
	movq	%rsi, %r10
	shlq	%cl, %rdx
	movq	$1, %rcx
	movq	%rdi, %r11
	movq	%rax, %r15
	.align	16, 0x90
.LBB0_223:                              # %for.body4.lr.ph.us.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_216 Depth 2
                                        #       Child Loop BB0_217 Depth 3
	decq	%r11
	testq	%rcx, %rcx
	leaq	8(%rax), %r14
	je	.LBB0_225
# BB#224:                               # %for.body4.lr.ph.us.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	%rax, %r14
.LBB0_225:                              # %for.body4.lr.ph.us.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	$1, %r13
	je	.LBB0_227
# BB#226:                               # %for.body4.lr.ph.us.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	%rcx, %r13
.LBB0_227:                              # %for.body4.lr.ph.us.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	%r14, %rax
	movq	%r10, %r9
	movq	%r15, %rdi
	.align	16, 0x90
.LBB0_216:                              # %for.body4.us.i
                                        #   Parent Loop BB0_223 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_217 Depth 3
	movq	(%rdi), %rbx
	xorq	%r8, %r8
	jmp	.LBB0_217
	.align	16, 0x90
.LBB0_220:                              # %for.inc.us.i
                                        #   in Loop: Header=BB0_217 Depth=3
	movq	$1, %rcx
	shrq	%cl, %rbx
	addq	%rdx, %r8
.LBB0_217:                              # %for.body4.us.i
                                        #   Parent Loop BB0_223 Depth=1
                                        #     Parent Loop BB0_216 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB0_221
# BB#218:                               # %for.body8.us.i
                                        #   in Loop: Header=BB0_217 Depth=3
	testq	$1, %rbx
	je	.LBB0_220
# BB#219:                               # %if.then10.us.i
                                        #   in Loop: Header=BB0_217 Depth=3
	orq	%r13, (%rax,%r8)
	jmp	.LBB0_220
	.align	16, 0x90
.LBB0_221:                              # %for.inc12.us.i
                                        #   in Loop: Header=BB0_216 Depth=2
	decq	%r9
	addq	$8, %rdi
	movq	$32, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%r12, %rax
	testq	%rsi, %rsi
	jg	.LBB0_216
# BB#222:                               # %for.inc16.us.i
                                        #   in Loop: Header=BB0_223 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	leaq	(%r15,%rax,8), %r15
	addq	%r13, %r13
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r13, %rcx
	movq	%r14, %rax
	jg	.LBB0_223
.LBB0_228:                              # %for.cond402.preheader
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rdi          # 8-byte Reload
	testq	%rax, %rdi
	je	.LBB0_231
.LBB0_229:                              # %for.body406.lr.ph
	movq	allocno(%rip), %rax
	addq	$40, %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_230:                              # %for.body406
                                        # =>This Inner Loop Header: Depth=1
	movq	eliminable_regset(%rip), %rdx
	notq	%rdx
	andq	%rdx, -16(%rax)
	movq	eliminable_regset(%rip), %rdx
	notq	%rdx
	andq	%rdx, (%rax)
	movq	eliminable_regset(%rip), %rdx
	notq	%rdx
	andq	%rdx, -8(%rax)
	incq	%rcx
	addq	$64, %rax
	cmpq	%rdi, %rcx
	jb	.LBB0_230
.LBB0_231:                              # %for.end417
	callq	get_insns
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB0_251
# BB#232:
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_233:                              # %for.body.i805
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_240 Depth 2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_250
# BB#234:                               # %cond.true.i
                                        #   in Loop: Header=BB0_233 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_236
# BB#235:                               # %cond.false.i
                                        #   in Loop: Header=BB0_233 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_236:                              # %cond.end23.i
                                        #   in Loop: Header=BB0_233 Depth=1
	testq	%rax, %rax
	je	.LBB0_250
# BB#237:                               # %land.lhs.true27.i
                                        #   in Loop: Header=BB0_233 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB0_250
# BB#238:                               # %land.lhs.true35.i
                                        #   in Loop: Header=BB0_233 Depth=1
	movl	8(%rcx), %ecx
	movq	reg_allocno(%rip), %r9
	movslq	(%r9,%rcx,4), %r8
	testq	%r8, %r8
	js	.LBB0_250
# BB#239:                               # %if.then.i
                                        #   in Loop: Header=BB0_233 Depth=1
	movq	56(%rbx), %rdi
	jmp	.LBB0_240
	.align	16, 0x90
.LBB0_249:                              # %for.inc.i817
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	16(%rdi), %rdi
.LBB0_240:                              # %if.then.i
                                        #   Parent Loop BB0_233 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rdi, %rdi
	je	.LBB0_250
# BB#241:                               # %for.body50.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movl	(%rdi), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$65536, %rcx            # imm = 0x10000
	jne	.LBB0_249
# BB#242:                               # %land.lhs.true55.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	8(%rdi), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_249
# BB#243:                               # %land.lhs.true63.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movl	8(%rsi), %ecx
	movslq	(%r9,%rcx,4), %r10
	testq	%r10, %r10
	js	.LBB0_249
# BB#244:                               # %land.lhs.true74.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movl	allocno_row_words(%rip), %r11d
	movq	%r10, %rdx
	andq	%r14, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	conflicts(%rip), %rcx
	imulq	%r8, %r11
	addq	%r11, %rdx
	andq	%r15, %rdx
	movq	(%rcx,%rdx,8), %r11
	movq	$1, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %r11
	jne	.LBB0_249
# BB#245:                               # %if.then102.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	16(%rax), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB0_246
# BB#247:                               # %if.then127.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	allocno(%rip), %rsi
	movq	$6, %rcx
	movq	%r10, %r12
	shlq	%cl, %r12
	movq	40(%rsi,%r12), %r11
	movq	$6, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	40(%rsi,%rdx), %rcx
	orq	%r11, %rcx
	movq	%rcx, 40(%rsi,%rdx)
	orq	%rcx, 40(%rsi,%r12)
	jmp	.LBB0_248
.LBB0_246:                              # %if.then102.if.end_crit_edge.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	allocno(%rip), %rsi
.LBB0_248:                              # %if.end.i
                                        #   in Loop: Header=BB0_240 Depth=2
	movq	$6, %rcx
	shlq	%cl, %r10
	movq	32(%rsi,%r10), %r11
	movq	$6, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	32(%rsi,%rdx), %rcx
	orq	%r11, %rcx
	movq	%rcx, 32(%rsi,%rdx)
	orq	%rcx, 32(%rsi,%r10)
	movq	48(%rsi,%r10), %r11
	movq	48(%rsi,%rdx), %rcx
	orq	%r11, %rcx
	movq	%rcx, 48(%rsi,%rdx)
	orq	%rcx, 48(%rsi,%r10)
	jmp	.LBB0_249
	.align	16, 0x90
.LBB0_250:                              # %for.inc171.i
                                        #   in Loop: Header=BB0_233 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_233
.LBB0_251:                              # %expand_preferences.exit
	movslq	max_allocno(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movl	max_allocno(%rip), %edx
	movq	%rax, allocno_order(%rip)
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	je	.LBB0_260
	.align	16, 0x90
.LBB0_252:                              # %for.body425
                                        # =>This Inner Loop Header: Depth=1
	movl	%esi, (%rax,%rsi,4)
	incq	%rsi
	cmpq	%rdi, %rsi
	jb	.LBB0_252
# BB#253:                               # %for.cond431.preheader
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	je	.LBB0_260
# BB#254:                               # %for.body435.lr.ph
	movq	allocno(%rip), %rcx
	addq	$20, %rcx
	xorq	%rdx, %rdx
	movq	$1, %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_255:                              # %for.body435
                                        # =>This Inner Loop Header: Depth=1
	movl	-16(%rcx), %esi
	testq	%rsi, %rsi
	jne	.LBB0_257
# BB#256:                               # %if.then440
                                        #   in Loop: Header=BB0_255 Depth=1
	movl	%edi, -16(%rcx)
.LBB0_257:                              # %if.end443
                                        #   in Loop: Header=BB0_255 Depth=1
	movl	(%rcx), %esi
	testq	%rsi, %rsi
	jne	.LBB0_259
# BB#258:                               # %if.then448
                                        #   in Loop: Header=BB0_255 Depth=1
	movl	%ebx, (%rcx)
.LBB0_259:                              # %for.inc452
                                        #   in Loop: Header=BB0_255 Depth=1
	movslq	max_allocno(%rip), %rsi
	incq	%rdx
	addq	$64, %rcx
	cmpq	%rsi, %rdx
	jb	.LBB0_255
.LBB0_260:                              # %for.end454
	movabsq	$allocno_compare, %rcx
	movq	$4, %rdx
	movq	%rax, %rdi
	callq	specqsort
	movslq	max_allocno(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	max_allocno(%rip), %r14
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	jle	.LBB0_270
# BB#261:                               # %for.body.lr.ph.i823
	movq	allocno(%rip), %rdi
	decq	%r14
	.align	16, 0x90
.LBB0_262:                              # %for.body.i829
                                        # =>This Inner Loop Header: Depth=1
	movq	allocno_order(%rip), %rax
	movslq	(%rax,%r14,4), %r15
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	%r14d, (%rax,%r15,4)
	movq	$6, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movl	8(%rdi,%rax), %ecx
	movq	24(%rdi,%rax), %rax
	testq	%rcx, %rcx
	jne	.LBB0_264
# BB#263:                               #   in Loop: Header=BB0_262 Depth=1
	movq	fixed_reg_set(%rip), %rbx
	jmp	.LBB0_265
	.align	16, 0x90
.LBB0_264:                              # %select.mid231
                                        #   in Loop: Header=BB0_262 Depth=1
	movq	call_used_reg_set(%rip), %rbx
.LBB0_265:                              # %select.end230
                                        #   in Loop: Header=BB0_262 Depth=1
	movq	$32, %rcx
	movq	%r14, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$6, %rcx
	shlq	%cl, %r15
	movl	(%rdi,%r15), %edi
	orq	%rax, %rbx
	callq	reg_preferred_class
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	allocno(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reg_class_contents(,%rax,8), %rax
	movq	32(%rdi,%r15), %rcx
	notq	%rbx
	andq	%rax, %rbx
	andq	%rbx, %rcx
	andq	%rbx, 40(%rdi,%r15)
	movq	%rcx, 32(%rdi,%r15)
	andq	%rbx, 48(%rdi,%r15)
	decq	%r14
	testq	%r13, %r13
	jg	.LBB0_262
# BB#266:                               # %for.end.i
	movslq	max_allocno(%rip), %r15
	testq	%r15, %r15
	jle	.LBB0_270
# BB#267:                               # %for.body32.lr.ph.i
	movslq	allocno_row_words(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	allocno_order(%rip), %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB0_268
# BB#326:                               # %for.body41.lr.ph.us.i.preheader
	movq	conflicts(%rip), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	48(%rdi), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB0_327:                              # %for.body41.lr.ph.us.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_317 Depth 2
                                        #       Child Loop BB0_318 Depth 3
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movslq	-4(%rdx,%r15,4), %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	decq	%r15
	movq	%rdx, %rax
	movq	24(%rsp), %rbx          # 8-byte Reload
	imulq	%rbx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rax,8), %rsi
	movq	$6, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	leaq	4(%rdi,%rax), %rdx
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	56(%rsp), %r11          # 8-byte Reload
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%rbx, %r9
	xorq	%r13, %r13
	.align	16, 0x90
.LBB0_317:                              # %for.body41.us.i
                                        #   Parent Loop BB0_327 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_318 Depth 3
	movq	(%rsi), %r10
	testq	%r10, %r10
	movq	%r11, %rdi
	movq	%r14, %r8
	je	.LBB0_324
	.align	16, 0x90
.LBB0_318:                              # %for.body43.us.i
                                        #   Parent Loop BB0_327 Depth=1
                                        #     Parent Loop BB0_317 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %r10
	je	.LBB0_323
# BB#319:                               # %if.then46.us.i
                                        #   in Loop: Header=BB0_318 Depth=3
	movq	$32, %rcx
	movq	%r15, %rax
	movslq	(%rdi), %r12
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r12
	jle	.LBB0_323
# BB#320:                               # %if.then51.us.i
                                        #   in Loop: Header=BB0_318 Depth=3
	movslq	-44(%r8), %rax
	movslq	(%rdx), %rbx
	movq	(%r8), %rcx
	cmpq	%rbx, %rax
	jle	.LBB0_321
# BB#322:                               # %if.else64.us.i
                                        #   in Loop: Header=BB0_318 Depth=3
	orq	%rcx, 72(%rsp)          # 8-byte Folded Spill
	jmp	.LBB0_323
.LBB0_321:                              # %if.then59.us.i
                                        #   in Loop: Header=BB0_318 Depth=3
	orq	%rcx, %r13
	.align	16, 0x90
.LBB0_323:                              # %for.inc72.us.i
                                        #   in Loop: Header=BB0_318 Depth=3
	movq	$1, %rcx
	shrq	%cl, %r10
	addq	$64, %r8
	addq	$4, %rdi
	testq	%r10, %r10
	jne	.LBB0_318
.LBB0_324:                              # %for.inc74.us.i
                                        #   in Loop: Header=BB0_317 Depth=2
	decq	%r9
	addq	$8, %rsi
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4096, %r14             # imm = 0x1000
	addq	$256, %r11              # imm = 0x100
	testq	%rax, %rax
	jg	.LBB0_317
# BB#325:                               # %do.end.us.i
                                        #   in Loop: Header=BB0_327 Depth=1
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$6, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	48(%rdi,%rax), %rcx
	notq	%rcx
	andq	%rcx, %r13
	movq	72(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %r13
	testq	%rdx, %rdx
	movq	%r13, 56(%rdi,%rax)
	movq	32(%rsp), %rdx          # 8-byte Reload
	jg	.LBB0_327
	jmp	.LBB0_270
.LBB0_3:                                # %lor.end
	callq	ix86_frame_pointer_required
	xorq	%rcx, %rcx
	movl	%ecx, max_allocno(%rip)
	movl	frame_pointer_needed(%rip), %esi
	orq	%rax, %rsi
	movq	eliminable_regset(%rip), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rsi
	movq	$0, no_global_alloc_regs(%rip)
	je	.LBB0_328
# BB#4:                                 # %for.end
	orq	$1114176, %rcx          # imm = 0x110040
	testq	%rdx, %rax
	movq	$1114112, no_global_alloc_regs(%rip) # imm = 0x110000
	movq	%rcx, eliminable_regset(%rip)
	movq	$1114176, %rax          # imm = 0x110040
	jne	.LBB0_5
	jmp	.LBB0_6
.LBB0_328:                              # %for.inc.3
	orq	$1114176, %rcx          # imm = 0x110040
	testq	%rdx, %rax
	movq	%rcx, eliminable_regset(%rip)
	movq	$64, %rax
	jne	.LBB0_5
	jmp	.LBB0_6
.LBB0_268:                              # %do.end.i.preheader
	decq	%r15
	.align	16, 0x90
.LBB0_269:                              # %do.end.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rdx,%r15,4), %rax
	movq	$6, %rcx
	shlq	%cl, %rax
	movq	$0, 56(%rdi,%rax)
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r15
	testq	%rax, %rax
	jg	.LBB0_269
.LBB0_270:                              # %prune_preferences.exit
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	free
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB0_307
# BB#271:                               # %if.then456
	movslq	max_allocno(%rip), %r10
	xorq	%r14, %r14
	testq	%r10, %r10
	movabsq	$0, %rdx
	jle	.LBB0_274
# BB#272:                               # %for.body.lr.ph.i837
	movq	allocno_order(%rip), %r8
	movq	allocno(%rip), %rdi
	xorq	%rdx, %rdx
	movq	reg_renumber(%rip), %rbx
	movq	$1, %rsi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %r10
	.align	16, 0x90
.LBB0_273:                              # %for.body.i841
                                        # =>This Inner Loop Header: Depth=1
	movslq	-4(%r8,%rsi,4), %rax
	movq	$6, %rcx
	shlq	%cl, %rax
	movslq	(%rdi,%rax), %rax
	movzbl	(%rbx,%rax,2), %eax
	movq	$15, %rcx
	shrq	%cl, %rax
	addq	%rax, %rdx
	movq	%rsi, %rax
	andq	%r9, %rax
	incq	%rsi
	cmpq	%r10, %rax
	jne	.LBB0_273
.LBB0_274:                              # %for.end.i843
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	fprintf
	movslq	max_allocno(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_285
# BB#275:
	movabsq	$.L.str5, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$.L.str8, %r13
	.align	16, 0x90
.LBB0_276:                              # %for.body11.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_278 Depth 2
	movq	allocno_order(%rip), %rax
	movslq	(%rax,%r14,4), %rax
	movq	allocno(%rip), %rdx
	movq	$6, %rcx
	shlq	%cl, %rax
	movslq	(%rdx,%rax), %rdx
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rdx,2), %rax
	testq	%rax, %rax
	jns	.LBB0_284
# BB#277:                               # %if.end23.i
                                        #   in Loop: Header=BB0_276 Depth=1
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	fprintf
	movslq	max_regno(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_282
	.align	16, 0x90
.LBB0_278:                              # %for.body33.i
                                        #   Parent Loop BB0_276 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	reg_allocno(%rip), %rcx
	movq	allocno_order(%rip), %rsi
	movl	(%rcx,%rbx,4), %edi
	movq	$32, %rcx
	movq	%rdi, %rdx
	movl	(%rsi,%r14,4), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdi
	jne	.LBB0_281
# BB#279:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB0_278 Depth=2
	movq	allocno(%rip), %rsi
	movq	$6, %rcx
	shlq	%cl, %rdx
	movl	(%rsi,%rdx), %ecx
	movq	%rbx, %rdx
	andq	%r12, %rdx
	cmpq	%rcx, %rdx
	je	.LBB0_281
# BB#280:                               # %if.then47.i848
                                        #   in Loop: Header=BB0_278 Depth=2
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movl	max_regno(%rip), %eax
.LBB0_281:                              # %for.inc50.i
                                        #   in Loop: Header=BB0_278 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_278
.LBB0_282:                              # %for.end52.i
                                        #   in Loop: Header=BB0_276 Depth=1
	movq	allocno_order(%rip), %rax
	movslq	(%rax,%r14,4), %rax
	movq	allocno(%rip), %rdx
	movq	$6, %rcx
	shlq	%cl, %rax
	movl	4(%rdx,%rax), %edx
	cmpq	$1, %rdx
	je	.LBB0_284
# BB#283:                               # %if.then59.i850
                                        #   in Loop: Header=BB0_276 Depth=1
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str9, %rsi
	callq	fprintf
.LBB0_284:                              # %for.inc67.i
                                        #   in Loop: Header=BB0_276 Depth=1
	incq	%r14
	movslq	max_allocno(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_276
.LBB0_285:                              # %for.end69.i
	movq	$10, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	fputc
	movslq	max_allocno(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_306
# BB#286:
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_287:                              # %for.body74.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_289 Depth 2
                                        #     Child Loop BB0_292 Depth 2
                                        #     Child Loop BB0_296 Depth 2
                                        #     Child Loop BB0_301 Depth 2
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movl	(%rax,%rdx), %edx
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str10, %rsi
	callq	fprintf
	movslq	max_allocno(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %r13
	jle	.LBB0_292
# BB#288:                               # %for.body83.lr.ph.i
                                        #   in Loop: Header=BB0_287 Depth=1
	movq	%r15, %rdi
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdi
	movq	$6, %rcx
	shrq	%cl, %rdi
	movq	$1, %r14
	movq	$1, %r12
	movq	%r15, %rcx
	shlq	%cl, %r12
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_289:                              # %for.body83.i860
                                        #   Parent Loop BB0_287 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	allocno_row_words(%rip), %ecx
	leaq	-1(%r14), %rdx
	movq	conflicts(%rip), %rsi
	imulq	%rcx, %rdx
	addq	%rdi, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	(%rsi,%rdx,8), %rcx
	testq	%r12, %rcx
	je	.LBB0_291
# BB#290:                               # %if.then86.i
                                        #   in Loop: Header=BB0_289 Depth=2
	movq	allocno(%rip), %rax
	movl	(%rax,%rbx), %edx
	xorq	%rax, %rax
	movq	%rdi, %r13
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str5, %rsi
	callq	fprintf
	movq	%r13, %rdi
	movl	max_allocno(%rip), %eax
.LBB0_291:                              # %for.inc92.i
                                        #   in Loop: Header=BB0_289 Depth=2
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	addq	$64, %rbx
	xorq	%r13, %r13
	cmpq	%rsi, %rdx
	jl	.LBB0_289
	.align	16, 0x90
.LBB0_292:                              # %for.body98.i
                                        #   Parent Loop BB0_287 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	24(%rax,%rdx), %rax
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB0_294
# BB#293:                               # %if.then105.i
                                        #   in Loop: Header=BB0_292 Depth=2
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str5, %rsi
	movq	%r13, %rdx
	callq	fprintf
.LBB0_294:                              # %for.inc108.i
                                        #   in Loop: Header=BB0_292 Depth=2
	incq	%r13
	cmpq	$53, %r13
	jne	.LBB0_292
# BB#295:                               # %for.end110.i
                                        #   in Loop: Header=BB0_287 Depth=1
	movq	$10, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	fputc
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	32(%rax,%rdx), %rdx
	xorq	%rcx, %rcx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB0_296:                              # %for.body115.i
                                        #   Parent Loop BB0_287 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB0_298
# BB#297:                               # %for.body115.i
                                        #   in Loop: Header=BB0_296 Depth=2
	movq	$1, %rsi
.LBB0_298:                              # %for.body115.i
                                        #   in Loop: Header=BB0_296 Depth=2
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB0_296
# BB#299:                               # %for.end126.i
                                        #   in Loop: Header=BB0_287 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	je	.LBB0_305
# BB#300:                               # %if.end129.i
                                        #   in Loop: Header=BB0_287 Depth=1
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movl	(%rax,%rdx), %edx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str11, %rsi
	callq	fprintf
	.align	16, 0x90
.LBB0_301:                              # %for.body137.i
                                        #   Parent Loop BB0_287 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	32(%rax,%rdx), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB0_303
# BB#302:                               # %if.then145.i
                                        #   in Loop: Header=BB0_301 Depth=2
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str5, %rsi
	movq	%rbx, %rdx
	callq	fprintf
.LBB0_303:                              # %for.inc148.i
                                        #   in Loop: Header=BB0_301 Depth=2
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB0_301
# BB#304:                               # %for.end150.i
                                        #   in Loop: Header=BB0_287 Depth=1
	movq	$10, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	fputc
.LBB0_305:                              # %for.inc152.i
                                        #   in Loop: Header=BB0_287 Depth=1
	incq	%r15
	movslq	max_allocno(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB0_287
.LBB0_306:                              # %dump_conflicts.exit
	movq	$10, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	fputc
.LBB0_307:                              # %for.cond458.preheader
	movl	max_allocno(%rip), %eax
	movq	allocno_order(%rip), %rdi
	testq	%rax, %rax
	je	.LBB0_315
# BB#308:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_309:                              # %for.body462
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%rdi,%rbx,4), %rdi
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movslq	(%rax,%rdx), %rax
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB0_314
# BB#310:                               # %land.lhs.true472
                                        #   in Loop: Header=BB0_309 Depth=1
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movslq	28(%rax), %rax
	testq	%rax, %rax
	js	.LBB0_314
# BB#311:                               # %if.then484
                                        #   in Loop: Header=BB0_309 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	callq	find_reg
	movq	allocno_order(%rip), %rax
	movslq	(%rax,%rbx,4), %rax
	movq	allocno(%rip), %rdx
	movq	$6, %rcx
	shlq	%cl, %rax
	movslq	(%rdx,%rax), %rdi
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rdi,2), %rax
	testq	%rax, %rax
	jns	.LBB0_314
# BB#312:                               # %if.end496
                                        #   in Loop: Header=BB0_309 Depth=1
	callq	reg_alternate_class
	testq	%r14, %rax
	je	.LBB0_314
# BB#313:                               # %if.then504
                                        #   in Loop: Header=BB0_309 Depth=1
	movq	allocno_order(%rip), %rax
	movl	(%rax,%rbx,4), %edi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	callq	find_reg
	.align	16, 0x90
.LBB0_314:                              # %for.inc508
                                        #   in Loop: Header=BB0_309 Depth=1
	movslq	max_allocno(%rip), %rax
	movq	allocno_order(%rip), %rdi
	incq	%rbx
	cmpq	%rax, %rbx
	jb	.LBB0_309
.LBB0_315:                              # %for.end510
	callq	free
.LBB0_316:                              # %if.end511
	callq	get_insns
	movq	%rax, %rdi
	callq	build_insn_chain
	callq	get_insns
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	reload
	movq	reg_allocno(%rip), %rdi
	movq	%rax, %rbx
	callq	free
	movq	reg_may_share(%rip), %rdi
	callq	free
	movq	allocno(%rip), %rdi
	callq	free
	movq	conflicts(%rip), %rdi
	callq	free
	movq	allocnos_live(%rip), %rdi
	callq	free
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_34:                               # %if.then153
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.global_alloc, %rcx
	movq	$441, %rsi              # imm = 0x1B9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	global_alloc, .Ltmp17-global_alloc
	.cfi_endproc

	.align	16, 0x90
	.type	allocno_compare,@function
allocno_compare:                        # @allocno_compare
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
	movslq	(%rdi), %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	shlq	%cl, %rbx
	movslq	12(%rax,%rbx), %rdi
	movslq	(%rsi), %r15
	movq	%r15, 16(%rsp)          # 8-byte Spill
	callq	floor_log2_wide
	movq	allocno(%rip), %r12
	movl	16(%r12,%rbx), %edi
	imulq	%rax, %rdi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	20(%r12,%rbx), %edi
	movq	%rax, %r14
	callq	int32_to_float64
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movl	4(%r12,%rbx), %edi
	movq	%rax, %rbx
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movq	$6, %rcx
	shlq	%cl, %r15
	movslq	12(%r12,%r15), %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r12
	andq	%r13, %r12
	callq	floor_log2_wide
	movq	allocno(%rip), %r14
	movl	16(%r14,%r15), %edi
	imulq	%rax, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movl	20(%r14,%r15), %edi
	movq	%rax, %rbx
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4621819117588971520, %rsi # imm = 0x4024000000000000
	callq	float64_mul
	movl	4(%r14,%r15), %edi
	movq	%rax, %rbx
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	andq	%rax, %r13
	cmpq	%r12, %r13
	je	.LBB1_1
# BB#2:                                 # %entry
	movq	8(%rsp), %rcx           # 8-byte Reload
	jmp	.LBB1_3
.LBB1_1:
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
.LBB1_3:                                # %entry
	subq	%rcx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	allocno_compare, .Ltmp35-allocno_compare
	.cfi_endproc

	.align	16, 0x90
	.type	find_reg,@function
find_reg:                               # @find_reg
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
	subq	$128, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 176
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
	movq	%rcx, %r15
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	allocno(%rip), %rax
	sarq	%cl, %rbx
	movq	$6, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movl	(%rax,%rdi), %edi
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rdx
	je	.LBB2_2
# BB#1:                                 # %cond.true
	callq	reg_alternate_class
	jmp	.LBB2_3
.LBB2_2:                                # %cond.false
	callq	reg_preferred_class
.LBB2_3:                                # %cond.end
	movq	cfun(%rip), %rcx
	movq	allocno(%rip), %rdx
	movq	24(%rcx), %rsi
	movq	$6, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movslq	(%rdx,%rdi), %rcx
	movq	80(%rsi), %rsi
	movq	(%rsi,%rcx,8), %rcx
	movzbl	2(%rcx), %r9d
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r12, %r8
	testq	%r8, %r15
	je	.LBB2_5
# BB#4:
	movabsq	$call_fixed_reg_set, %rcx
	jmp	.LBB2_8
.LBB2_5:                                # %if.else
	movq	$6, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movl	8(%rdx,%rsi), %ecx
	testq	%rcx, %rcx
	je	.LBB2_6
# BB#7:                                 # %select.mid
	movabsq	$call_used_reg_set, %rcx
	jmp	.LBB2_8
.LBB2_6:
	movabsq	$fixed_reg_set, %rcx
.LBB2_8:                                # %if.end15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	(%rcx), %rdi
	movq	no_global_alloc_regs(%rip), %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reg_class_contents(,%rax,8), %rax
	notq	%rax
	orq	%r14, %rdi
	movq	%r14, 8(%rsp)           # 8-byte Spill
	orq	%rsi, %rdi
	orq	%rax, %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	$6, %rcx
	movq	%rbx, %rax
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	24(%rdx,%rax), %rsi
	movq	56(%rdx,%rax), %rcx
	orq	%rdi, %rsi
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	movq	regs_used_so_far(%rip), %rax
	notq	%rax
	orq	%rsi, %rax
	orq	%rcx, %rax
	xorq	%rdx, %rdx
	movq	%r8, %rcx
	.align	16, 0x90
.LBB2_9:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_12 Depth 2
                                        #       Child Loop BB2_28 Depth 3
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movq	%rdx, %rcx
	andq	%r8, %rcx
	cmpq	$1, %rcx
	movq	96(%rsp), %r14          # 8-byte Reload
	je	.LBB2_11
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%rax, %r14
.LBB2_11:                               # %for.body
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	xorq	%r15, %r15
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_12:                               # %for.body38
                                        #   Parent Loop BB2_9 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_28 Depth 3
	movl	reg_alloc_order(,%r15,4), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r14, %rax
	jne	.LBB2_31
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_12 Depth=2
	movq	%rcx, %rdi
	movq	%r9, %rsi
	movq	%r9, %r13
	movq	%rcx, %r14
	movq	%r8, %r12
	callq	ix86_hard_regno_mode_ok
	movq	%r12, %r8
	movq	%r14, %r12
	movq	120(%rsp), %r14         # 8-byte Reload
	movq	%r13, %r9
	testq	%r8, %rax
	je	.LBB2_31
# BB#14:                                # %land.lhs.true44
                                        #   in Loop: Header=BB2_12 Depth=2
	movq	%r12, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB2_18
# BB#15:                                # %land.lhs.true44
                                        #   in Loop: Header=BB2_12 Depth=2
	leaq	-21(%r12), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB2_18
# BB#16:                                # %land.lhs.true44
                                        #   in Loop: Header=BB2_12 Depth=2
	leaq	-45(%r12), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB2_18
# BB#17:                                # %land.lhs.true44
                                        #   in Loop: Header=BB2_12 Depth=2
	leaq	-29(%r12), %rax
	andq	%r8, %rax
	cmpq	$7, %rax
	jbe	.LBB2_18
# BB#22:                                # %cond.false75
                                        #   in Loop: Header=BB2_12 Depth=2
	cmpq	$24, %r9
	jne	.LBB2_23
# BB#25:                                # %cond.true83
                                        #   in Loop: Header=BB2_12 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_27
.LBB2_18:                               # %cond.true67
                                        #   in Loop: Header=BB2_12 Depth=2
	movl	mode_class(,%r9,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_27
# BB#19:                                # %lor.rhs
                                        #   in Loop: Header=BB2_12 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_21
# BB#20:                                # %lor.rhs
                                        #   in Loop: Header=BB2_12 Depth=2
	movabsq	$1, %rax
.LBB2_21:                               # %lor.rhs
                                        #   in Loop: Header=BB2_12 Depth=2
	incq	%rax
	jmp	.LBB2_27
.LBB2_23:                               # %cond.false75
                                        #   in Loop: Header=BB2_12 Depth=2
	cmpq	$18, %r9
	jne	.LBB2_26
# BB#24:                                # %cond.true77
                                        #   in Loop: Header=BB2_12 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_27
.LBB2_26:                               # %cond.false87
                                        #   in Loop: Header=BB2_12 Depth=2
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r9), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	.align	16, 0x90
.LBB2_27:                               # %cond.end100
                                        #   in Loop: Header=BB2_12 Depth=2
	addq	%r12, %rax
	movq	%r12, %rdx
	.align	16, 0x90
.LBB2_28:                               # %for.cond104
                                        #   Parent Loop BB2_9 Depth=1
                                        #     Parent Loop BB2_12 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jge	.LBB2_30
# BB#29:                                # %land.rhs107
                                        #   in Loop: Header=BB2_28 Depth=3
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	testq	%r14, %rsi
	je	.LBB2_28
.LBB2_30:                               # %for.end
                                        #   in Loop: Header=BB2_12 Depth=2
	andq	%r8, %rax
	andq	%r8, %rdx
	cmpq	%rax, %rdx
	je	.LBB2_33
.LBB2_31:                               # %for.inc119
                                        #   in Loop: Header=BB2_12 Depth=2
	incq	%r15
	incq	%rbx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB2_12
# BB#32:                                #   in Loop: Header=BB2_9 Depth=1
	movq	%rbx, %r15
	movq	104(%rsp), %r12         # 8-byte Reload
.LBB2_33:                               # %for.inc122
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	112(%rsp), %rdx         # 8-byte Reload
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jg	.LBB2_35
# BB#34:                                # %for.inc122
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$52, %r15
	movq	%r12, %rcx
	movq	%r14, %rax
	jg	.LBB2_9
.LBB2_35:                               # %for.end124
	movq	allocno(%rip), %rdx
	movq	$6, %rcx
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	40(%rdx,%rsi), %rax
	movq	%r14, %rdi
	notq	%rdi
	andq	%rdi, %rax
	movq	%rax, 40(%rdx,%rsi)
	movq	reg_class_contents(%rip), %rcx
	notq	%rcx
	testq	%rcx, %rax
	je	.LBB2_67
# BB#36:                                # %for.end124
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB2_67
# BB#37:                                # %for.cond141.preheader
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	xorq	%r10, %r10
	movq	$2, %rbx
	jmp	.LBB2_38
	.align	16, 0x90
.LBB2_65:                               # %for.inc304.for.body144_crit_edge
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	40(%rax,%rdx), %rax
	incq	%rbx
.LBB2_38:                               # %for.body144
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_58 Depth 2
	movq	$1, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB2_64
# BB#39:                                # %land.lhs.true152
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	%r10, %rdi
	movq	%r9, %rsi
	movq	%r10, 112(%rsp)         # 8-byte Spill
	movq	%rbx, %r13
	movq	%r9, %rbx
	callq	ix86_hard_regno_mode_ok
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r8
	movq	%rbx, %r9
	movq	%r13, %rbx
	movq	112(%rsp), %r10         # 8-byte Reload
	testq	%r8, %rax
	je	.LBB2_64
# BB#40:                                # %land.lhs.true155
                                        #   in Loop: Header=BB2_38 Depth=1
	movl	regclass_map(,%r10,4), %edi
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	regclass_map(,%rax,4), %esi
	cmpq	%rsi, %rdi
	je	.LBB2_43
# BB#41:                                # %lor.lhs.false171
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	movq	%rdi, %r13
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	callq	reg_class_subset_p
	movq	%r13, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r8
	movq	%rbx, %r9
	movq	%r15, %rcx
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	112(%rsp), %r10         # 8-byte Reload
	testq	%r8, %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	jne	.LBB2_43
# BB#42:                                # %lor.lhs.false178
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	%rbx, %r13
	movq	%rcx, %r15
	movq	%r9, %rbx
	callq	reg_class_subset_p
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r8
	movq	%rbx, %r9
	movq	%r13, %rbx
	movq	112(%rsp), %r10         # 8-byte Reload
	testq	%r8, %rax
	je	.LBB2_64
	.align	16, 0x90
.LBB2_43:                               # %if.then185
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r10, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB2_47
# BB#44:                                # %if.then185
                                        #   in Loop: Header=BB2_38 Depth=1
	movabsq	$4294967275, %rax       # imm = 0xFFFFFFEB
	leaq	(%r10,%rax), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB2_47
# BB#45:                                # %if.then185
                                        #   in Loop: Header=BB2_38 Depth=1
	movabsq	$4294967251, %rax       # imm = 0xFFFFFFD3
	leaq	(%r10,%rax), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB2_47
# BB#46:                                # %if.then185
                                        #   in Loop: Header=BB2_38 Depth=1
	movabsq	$4294967267, %rax       # imm = 0xFFFFFFE3
	leaq	(%r10,%rax), %rax
	andq	%r8, %rax
	cmpq	$7, %rax
	jbe	.LBB2_47
# BB#51:                                # %cond.false223
                                        #   in Loop: Header=BB2_38 Depth=1
	cmpq	$24, %r9
	jne	.LBB2_52
# BB#54:                                # %cond.true233
                                        #   in Loop: Header=BB2_38 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_56
.LBB2_47:                               # %cond.true211
                                        #   in Loop: Header=BB2_38 Depth=1
	movl	mode_class(,%r9,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_56
# BB#48:                                # %lor.rhs216
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	%r8, %rdi
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_50
# BB#49:                                # %lor.rhs216
                                        #   in Loop: Header=BB2_38 Depth=1
	movabsq	$1, %rax
.LBB2_50:                               # %lor.rhs216
                                        #   in Loop: Header=BB2_38 Depth=1
	incq	%rax
	movq	%rdi, %r8
	jmp	.LBB2_56
.LBB2_52:                               # %cond.false223
                                        #   in Loop: Header=BB2_38 Depth=1
	cmpq	$18, %r9
	jne	.LBB2_55
# BB#53:                                # %cond.true226
                                        #   in Loop: Header=BB2_38 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_56
.LBB2_55:                               # %cond.false237
                                        #   in Loop: Header=BB2_38 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r9), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	.align	16, 0x90
.LBB2_56:                               # %cond.end254
                                        #   in Loop: Header=BB2_38 Depth=1
	addq	%r10, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	1(%r10), %rbx
	movq	%r10, 112(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB2_63
# BB#57:                                # %land.lhs.true261.lr.ph
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	%r12, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	subq	%rax, %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	80(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB2_58:                               # %land.lhs.true261
                                        #   Parent Loop BB2_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r15), %rbx
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%r14, %rax
	jne	.LBB2_63
# BB#59:                                # %land.rhs266
                                        #   in Loop: Header=BB2_58 Depth=2
	movl	regclass_map-4(,%r15,4), %edi
	movq	96(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	regclass_map(,%rax,4), %esi
	cmpq	%rsi, %rdi
	je	.LBB2_62
# BB#60:                                # %lor.lhs.false275
                                        #   in Loop: Header=BB2_58 Depth=2
	movq	%rdi, %r13
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	callq	reg_class_subset_p
	movq	%r13, %rsi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	movq	88(%rsp), %rdi          # 8-byte Reload
	jne	.LBB2_62
# BB#61:                                # %lor.rhs284
                                        #   in Loop: Header=BB2_58 Depth=2
	movq	%r8, %r13
	callq	reg_class_subset_p
	movq	%r13, %r8
	testq	%r8, %rax
	je	.LBB2_63
.LBB2_62:                               # %for.cond258.backedge
                                        #   in Loop: Header=BB2_58 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r15, %rbx
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rdx, %rax
	jl	.LBB2_58
.LBB2_63:                               # %for.end298
                                        #   in Loop: Header=BB2_38 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
	andq	%r8, %rax
	andq	%r8, %rbx
	cmpq	%rax, %rbx
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	112(%rsp), %r10         # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	je	.LBB2_98
.LBB2_64:                               # %for.inc304
                                        #   in Loop: Header=BB2_38 Depth=1
	incq	%r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB2_65
# BB#66:                                # %no_copy_prefs.loopexit
	movq	allocno(%rip), %rdx
	movq	(%rsp), %rdi            # 8-byte Reload
.LBB2_67:                               # %no_copy_prefs
	movq	$6, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	32(%rdx,%rsi), %rax
	andq	%rdi, %rax
	movq	%rax, 32(%rdx,%rsi)
	movq	reg_class_contents(%rip), %rcx
	notq	%rcx
	testq	%rcx, %rax
	je	.LBB2_97
# BB#68:                                # %no_copy_prefs
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB2_97
# BB#69:                                # %for.cond324.preheader
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	xorq	%r10, %r10
	movq	$2, %rbx
	movq	%r14, 120(%rsp)         # 8-byte Spill
	jmp	.LBB2_70
	.align	16, 0x90
.LBB2_175:                              # %for.inc487.for.body327_crit_edge
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	32(%rax,%rdx), %rax
	incq	%rbx
.LBB2_70:                               # %for.body327
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_90 Depth 2
	movq	$1, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB2_96
# BB#71:                                # %land.lhs.true335
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	%r10, %rdi
	movq	%r9, %rsi
	movq	%r10, %r14
	movq	%rbx, %r13
	movq	%r9, %rbx
	callq	ix86_hard_regno_mode_ok
	movq	%rbx, %r9
	movq	%r13, %rbx
	movq	%r14, %r10
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_96
# BB#72:                                # %land.lhs.true338
                                        #   in Loop: Header=BB2_70 Depth=1
	movl	regclass_map(,%r10,4), %edi
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	regclass_map(,%rax,4), %esi
	cmpq	%rsi, %rdi
	je	.LBB2_75
# BB#73:                                # %lor.lhs.false354
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	%r10, %r13
	movq	%rbx, %r14
	movq	%r9, %rbx
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	callq	reg_class_subset_p
	movq	%rbx, %r9
	movq	%r14, %rbx
	movq	%r15, %rcx
	movq	%r13, %r10
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	jne	.LBB2_75
# BB#74:                                # %lor.lhs.false361
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	%r10, %r13
	movq	%rcx, %r15
	movq	%rbx, %r14
	movq	%r9, %rbx
	callq	reg_class_subset_p
	movq	%rbx, %r9
	movq	%r14, %rbx
	movq	%r13, %r10
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_96
	.align	16, 0x90
.LBB2_75:                               # %if.then368
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r10, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	je	.LBB2_79
# BB#76:                                # %if.then368
                                        #   in Loop: Header=BB2_70 Depth=1
	movabsq	$4294967275, %rax       # imm = 0xFFFFFFEB
	leaq	(%r10,%rax), %rax
	andq	%r14, %rax
	cmpq	$8, %rax
	jb	.LBB2_79
# BB#77:                                # %if.then368
                                        #   in Loop: Header=BB2_70 Depth=1
	movabsq	$4294967251, %rax       # imm = 0xFFFFFFD3
	leaq	(%r10,%rax), %rax
	andq	%r14, %rax
	cmpq	$8, %rax
	jb	.LBB2_79
# BB#78:                                # %if.then368
                                        #   in Loop: Header=BB2_70 Depth=1
	movabsq	$4294967267, %rax       # imm = 0xFFFFFFE3
	leaq	(%r10,%rax), %rax
	andq	%r14, %rax
	cmpq	$7, %rax
	jbe	.LBB2_79
# BB#83:                                # %cond.false406
                                        #   in Loop: Header=BB2_70 Depth=1
	cmpq	$24, %r9
	jne	.LBB2_84
# BB#86:                                # %cond.true416
                                        #   in Loop: Header=BB2_70 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_88
.LBB2_79:                               # %cond.true394
                                        #   in Loop: Header=BB2_70 Depth=1
	movl	mode_class(,%r9,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_88
# BB#80:                                # %lor.rhs399
                                        #   in Loop: Header=BB2_70 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_82
# BB#81:                                # %lor.rhs399
                                        #   in Loop: Header=BB2_70 Depth=1
	movabsq	$1, %rax
.LBB2_82:                               # %lor.rhs399
                                        #   in Loop: Header=BB2_70 Depth=1
	incq	%rax
	jmp	.LBB2_88
.LBB2_84:                               # %cond.false406
                                        #   in Loop: Header=BB2_70 Depth=1
	cmpq	$18, %r9
	jne	.LBB2_87
# BB#85:                                # %cond.true409
                                        #   in Loop: Header=BB2_70 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_88
.LBB2_87:                               # %cond.false420
                                        #   in Loop: Header=BB2_70 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r9), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	.align	16, 0x90
.LBB2_88:                               # %cond.end437
                                        #   in Loop: Header=BB2_70 Depth=1
	addq	%r10, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r10, 112(%rsp)         # 8-byte Spill
	leaq	1(%r10), %r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jge	.LBB2_95
# BB#89:                                # %land.lhs.true444.lr.ph
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	%r12, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	subq	%rax, %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB2_90:                               # %land.lhs.true444
                                        #   Parent Loop BB2_70 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rbx), %r15
	movq	$1, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB2_95
# BB#91:                                # %land.rhs449
                                        #   in Loop: Header=BB2_90 Depth=2
	movl	regclass_map-4(,%rbx,4), %edi
	movq	96(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r15), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	regclass_map(,%rax,4), %r13d
	cmpq	%r13, %rdi
	je	.LBB2_94
# BB#92:                                # %lor.lhs.false458
                                        #   in Loop: Header=BB2_90 Depth=2
	movq	%r13, %rsi
	movq	%rdi, %r14
	callq	reg_class_subset_p
	movq	%r14, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_94
# BB#93:                                # %lor.rhs467
                                        #   in Loop: Header=BB2_90 Depth=2
	movq	%r13, %rdi
	callq	reg_class_subset_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_95
.LBB2_94:                               # %for.cond441.backedge
                                        #   in Loop: Header=BB2_90 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, %r15
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB2_90
.LBB2_95:                               # %for.end481
                                        #   in Loop: Header=BB2_70 Depth=1
	movq	104(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	andq	%rax, %r15
	cmpq	%r14, %r15
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	112(%rsp), %r10         # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
	je	.LBB2_98
.LBB2_96:                               # %for.inc487
                                        #   in Loop: Header=BB2_70 Depth=1
	incq	%r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB2_175
.LBB2_97:
	movq	%r12, %r10
.LBB2_98:                               # %no_prefs
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB2_105
# BB#99:                                # %no_prefs
	movl	flag_caller_saves(%rip), %ecx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB2_105
# BB#100:                               # %no_prefs
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB2_105
# BB#101:                               # %land.lhs.true497
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %edx
	testq	%rdx, %rdx
	je	.LBB2_105
# BB#102:                               # %land.lhs.true503
	movq	$34, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$6, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movslq	12(%rax,%rsi), %rax
	cmpq	%rax, %rdx
	jge	.LBB2_105
# BB#103:                               # %if.then511
	movq	losing_caller_save_reg_set(%rip), %rsi
	movq	8(%rsp), %rax           # 8-byte Reload
	orq	%rax, %rsi
	movq	$1, %rcx
	movq	$1, %r14
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r10, %r12
	movq	%r9, %rbx
	callq	find_reg
	movq	%rbx, %r9
	movq	%r12, %r10
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movslq	(%rax,%rdx), %rax
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rax
	testq	%rax, %rax
	js	.LBB2_105
# BB#104:                               # %if.then525
	movl	%r14d, caller_save_needed(%rip)
	jmp	.LBB2_174
.LBB2_105:                              # %if.end528
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB2_141
# BB#106:                               # %if.end528
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB2_141
# BB#107:                               # %land.lhs.true533
	movq	allocno(%rip), %rax
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movl	4(%rax,%rdx), %eax
	cmpq	$1, %rax
	jne	.LBB2_141
# BB#108:
	movq	%r10, 112(%rsp)         # 8-byte Spill
	movq	%r15, 72(%rsp)          # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	$52, %rbx
	movq	48(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB2_109:                              # %for.body542
                                        # =>This Inner Loop Header: Depth=1
	movslq	reg_alloc_order(,%rbx,4), %r12
	movl	local_reg_n_refs(,%r12,4), %eax
	testq	%rax, %rax
	je	.LBB2_139
# BB#110:                               # %land.lhs.true550
                                        #   in Loop: Header=BB2_109 Depth=1
	movq	%r12, %rcx
	andq	%r13, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r14, %rax
	jne	.LBB2_139
# BB#111:                               # %land.lhs.true555
                                        #   in Loop: Header=BB2_109 Depth=1
	movq	%r12, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB2_139
# BB#112:                               # %land.lhs.true558
                                        #   in Loop: Header=BB2_109 Depth=1
	movq	allocno(%rip), %r14
	movl	local_reg_freq(,%r12,4), %edi
	callq	int32_to_float64
	movl	local_reg_live_length(,%r12,4), %edi
	movq	%rax, %r15
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	$6, %rcx
	movq	72(%rsp), %r13          # 8-byte Reload
	shlq	%cl, %r13
	movl	16(%r14,%r13), %edi
	movq	%rax, %r15
	callq	int32_to_float64
	movl	20(%r14,%r13), %edi
	movq	%rax, %r13
	callq	int32_to_float64
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r14
	je	.LBB2_114
# BB#113:                               # %land.lhs.true558
                                        #   in Loop: Header=BB2_109 Depth=1
	movabsq	$1, %r14
.LBB2_114:                              # %land.lhs.true558
                                        #   in Loop: Header=BB2_109 Depth=1
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	js	.LBB2_116
# BB#115:                               # %land.lhs.true558
                                        #   in Loop: Header=BB2_109 Depth=1
	movabsq	$1, %rax
.LBB2_116:                              # %land.lhs.true558
                                        #   in Loop: Header=BB2_109 Depth=1
	orq	%rax, %r14
	movq	48(%rsp), %r14          # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	je	.LBB2_117
	.align	16, 0x90
.LBB2_139:                              # %for.inc717
                                        #   in Loop: Header=BB2_109 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rax, %rbx
	jg	.LBB2_109
# BB#140:
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	112(%rsp), %r10         # 8-byte Reload
.LBB2_141:                              # %if.end719
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_174
# BB#142:                               # %if.then722
	movq	allocno(%rip), %r12
	movq	$6, %rcx
	shlq	%cl, %r15
	movslq	(%r12,%r15), %rcx
	movq	reg_renumber(%rip), %r8
	movw	%r10w, (%r8,%rcx,2)
	movq	reg_may_share(%rip), %rdx
	movl	(%rdx,%rcx,4), %ecx
	testq	%rcx, %rcx
	je	.LBB2_148
# BB#143:                               # %for.cond738.preheader
	movslq	max_regno(%rip), %rdx
	cmpq	$54, %rdx
	jl	.LBB2_148
# BB#144:                               # %for.body741.lr.ph
	movq	reg_allocno(%rip), %rsi
	movq	$54, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rbx
	.align	16, 0x90
.LBB2_145:                              # %for.body741
                                        # =>This Inner Loop Header: Depth=1
	movl	-4(%rsi,%rax,4), %ecx
	cmpq	%rbx, %rcx
	jne	.LBB2_147
# BB#146:                               # %if.then746
                                        #   in Loop: Header=BB2_145 Depth=1
	movw	%r10w, -2(%r8,%rax,2)
.LBB2_147:                              # %for.inc751
                                        #   in Loop: Header=BB2_145 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rdx, %rdi
	jl	.LBB2_145
.LBB2_148:                              # %if.end754
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r10, %rax
	cmpq	$8, %rax
	je	.LBB2_152
# BB#149:                               # %if.end754
	leaq	-21(%r10), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_152
# BB#150:                               # %if.end754
	leaq	-45(%r10), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_152
# BB#151:                               # %if.end754
	leaq	-29(%r10), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB2_152
# BB#157:                               # %cond.false790
	cmpq	$24, %r9
	jne	.LBB2_158
# BB#160:                               # %cond.true800
	movzbl	target_flags+3(%rip), %r15d
	andq	$2, %r15
	xorq	$6, %r15
	jmp	.LBB2_162
.LBB2_152:                              # %cond.true778
	movl	mode_class(,%r9,4), %eax
	cmpq	$5, %rax
	movq	$2, %r15
	je	.LBB2_162
# BB#153:                               # %lor.rhs783
	cmpq	$6, %rax
	jne	.LBB2_154
# BB#155:                               # %lor.rhs783
	movabsq	$1, %r15
	incq	%r15
	jmp	.LBB2_162
.LBB2_158:                              # %cond.false790
	cmpq	$18, %r9
	jne	.LBB2_161
# BB#159:                               # %cond.true793
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	xorq	$3, %r15
	jmp	.LBB2_162
.LBB2_154:
	movabsq	$0, %r15
	incq	%r15
	jmp	.LBB2_162
.LBB2_161:                              # %cond.end821
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r9), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	jle	.LBB2_167
.LBB2_162:                              # %for.body827.lr.ph
	addq	%r10, %r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	1(%r10), %rax
	movq	%r10, %r8
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	movq	regs_used_so_far(%rip), %r14
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	leaq	local_reg_n_refs(,%rbx,4), %rdi
	movq	%r15, %rcx
	jge	.LBB2_164
# BB#163:                               # %for.body827.lr.ph
	movq	%rax, %rcx
.LBB2_164:                              # %for.body827.lr.ph
	decq	%rcx
	subq	%r8, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	leaq	4(,%rax,4), %rdx
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	callq	memset
	leaq	local_reg_freq(,%rbx,4), %rdi
	xorq	%rsi, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	callq	memset
	incq	%rbx
	.align	16, 0x90
.LBB2_165:                              # %for.body827
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rax
	leaq	-1(%rbx), %rcx
	movq	$1, %r14
	shlq	%cl, %r14
	orq	%r14, %r13
	orq	%rax, %r14
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB2_165
# BB#166:                               # %for.cond824.for.end840_crit_edge
	movq	%r14, regs_used_so_far(%rip)
.LBB2_167:                              # %for.end840
	movslq	allocno_row_words(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_174
# BB#168:                               # %for.body846.lr.ph
	movq	%rax, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	imulq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	conflicts(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rsi,%rdx,8), %rdx
	addq	$24, %r12
	.align	16, 0x90
.LBB2_169:                              # %for.body846
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_170 Depth 2
	movq	(%rdx), %rsi
	testq	%rsi, %rsi
	movq	%r12, %rdi
	je	.LBB2_173
	.align	16, 0x90
.LBB2_170:                              # %for.body849
                                        #   Parent Loop BB2_169 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	$1, %rsi
	je	.LBB2_172
# BB#171:                               # %if.then852
                                        #   in Loop: Header=BB2_170 Depth=2
	orq	%r13, (%rdi)
.LBB2_172:                              # %for.inc858
                                        #   in Loop: Header=BB2_170 Depth=2
	movq	$1, %rcx
	shrq	%cl, %rsi
	addq	$64, %rdi
	testq	%rsi, %rsi
	jne	.LBB2_170
.LBB2_173:                              # %for.inc861
                                        #   in Loop: Header=BB2_169 Depth=1
	decq	%rax
	addq	$8, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$4096, %r12             # imm = 0x1000
	testq	%rsi, %rsi
	jg	.LBB2_169
.LBB2_174:                              # %if.end865
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_117:                              # %for.cond585.preheader
	movslq	max_regno(%rip), %r11
	testq	%r11, %r11
	jle	.LBB2_118
# BB#119:                               # %for.body588.lr.ph
	movq	$1, %rdi
	movq	reg_renumber(%rip), %r14
	movabsq	$0, %r8
	movabsq	$1, %r9
	movq	$65535, %r10            # imm = 0xFFFF
	movq	72(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB2_120:                              # %for.body588
                                        # =>This Inner Loop Header: Depth=1
	movswq	-2(%r14,%rdi,2), %rsi
	testq	%rsi, %rsi
	js	.LBB2_137
# BB#121:                               # %if.then594
                                        #   in Loop: Header=BB2_120 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	-8(%rax,%rdi,8), %rax
	movzbl	2(%rax), %eax
	movq	%rsi, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	je	.LBB2_125
# BB#122:                               # %if.then594
                                        #   in Loop: Header=BB2_120 Depth=1
	leaq	-21(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB2_125
# BB#123:                               # %if.then594
                                        #   in Loop: Header=BB2_120 Depth=1
	leaq	-45(%rsi), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$8, %rcx
	jb	.LBB2_125
# BB#124:                               # %if.then594
                                        #   in Loop: Header=BB2_120 Depth=1
	movq	%rsi, %rcx
	addq	$-29, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$7, %rcx
	jbe	.LBB2_125
# BB#129:                               # %cond.false647
                                        #   in Loop: Header=BB2_120 Depth=1
	cmpq	$24, %rax
	jne	.LBB2_130
# BB#132:                               # %cond.true671
                                        #   in Loop: Header=BB2_120 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_134
.LBB2_125:                              # %cond.true621
                                        #   in Loop: Header=BB2_120 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_134
# BB#126:                               # %lor.rhs633
                                        #   in Loop: Header=BB2_120 Depth=1
	movq	%r15, %rbx
	cmpq	$6, %rcx
	movq	%r8, %rax
	jne	.LBB2_128
# BB#127:                               # %lor.rhs633
                                        #   in Loop: Header=BB2_120 Depth=1
	movq	%r9, %rax
.LBB2_128:                              # %lor.rhs633
                                        #   in Loop: Header=BB2_120 Depth=1
	incq	%rax
	movq	%rbx, %r15
	jmp	.LBB2_134
.LBB2_130:                              # %cond.false647
                                        #   in Loop: Header=BB2_120 Depth=1
	cmpq	$18, %rax
	jne	.LBB2_133
# BB#131:                               # %cond.true657
                                        #   in Loop: Header=BB2_120 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_134
.LBB2_133:                              # %cond.false675
                                        #   in Loop: Header=BB2_120 Depth=1
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	.align	16, 0x90
.LBB2_134:                              # %cond.end699
                                        #   in Loop: Header=BB2_120 Depth=1
	cmpq	%rsi, %r12
	jl	.LBB2_137
# BB#135:                               # %cond.end699
                                        #   in Loop: Header=BB2_120 Depth=1
	addq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r12
	jge	.LBB2_137
# BB#136:                               # %if.then707
                                        #   in Loop: Header=BB2_120 Depth=1
	movw	%r10w, -2(%r14,%rdi,2)
.LBB2_137:                              # %for.inc712
                                        #   in Loop: Header=BB2_120 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdi
	cmpq	%r11, %rax
	jl	.LBB2_120
# BB#138:
	movq	%r12, %r10
	movq	64(%rsp), %r9           # 8-byte Reload
	jmp	.LBB2_141
.LBB2_118:
	movq	%r12, %r10
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	72(%rsp), %r15          # 8-byte Reload
	jmp	.LBB2_141
.Ltmp53:
	.size	find_reg, .Ltmp53-find_reg
	.cfi_endproc

	.globl	build_insn_chain
	.align	16, 0x90
	.type	build_insn_chain,@function
build_insn_chain:                       # @build_insn_chain
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
	subq	$64, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 112
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
	movq	%rdi, %r12
	leaq	40(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, live_relevant_regs(%rip)
	movabsq	$reload_insn_chain, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB3_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_4 Depth 2
                                        #       Child Loop BB3_6 Depth 3
                                        #       Child Loop BB3_14 Depth 3
                                        #     Child Loop BB3_23 Depth 2
                                        #     Child Loop BB3_32 Depth 2
	testq	%r12, %r12
	je	.LBB3_48
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	32(%rax,%rdx,8), %rax
	movq	(%rax), %rax
	cmpq	%rax, %r12
	jne	.LBB3_17
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	live_relevant_regs(%rip), %rdi
	callq	bitmap_clear
	movq	basic_block_info(%rip), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movq	64(%rax), %rax
	movq	(%rax), %r15
	jmp	.LBB3_4
	.align	16, 0x90
.LBB3_55:                               # %if.end47.1
                                        #   in Loop: Header=BB3_4 Depth=2
	movq	(%r15), %r15
.LBB3_4:                                # %if.then
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_6 Depth 3
                                        #       Child Loop BB3_14 Depth 3
	testq	%r15, %r15
	je	.LBB3_17
# BB#5:                                 # %for.cond15.preheader
                                        #   in Loop: Header=BB3_4 Depth=2
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	movq	$1, %r14
	je	.LBB3_13
	.align	16, 0x90
.LBB3_6:                                # %for.body24
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%r14), %rax
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%rbx, %rdx
	je	.LBB3_12
# BB#7:                                 # %if.then26
                                        #   in Loop: Header=BB3_6 Depth=3
	notq	%rdx
	movl	16(%r15), %esi
	andq	%rdx, %rbx
	movq	$7, %rcx
	shlq	%cl, %rsi
	addq	%rsi, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$52, %rdx
	jg	.LBB3_9
# BB#8:                                 # %cond.true
                                        #   in Loop: Header=BB3_6 Depth=3
	movq	eliminable_regset(%rip), %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	je	.LBB3_10
	jmp	.LBB3_11
	.align	16, 0x90
.LBB3_9:                                # %cond.false
                                        #   in Loop: Header=BB3_6 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	reg_renumber(%rip), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	(%rsi,%rdx,2), %rcx
	testq	%rcx, %rcx
	js	.LBB3_11
.LBB3_10:                               # %if.then40
                                        #   in Loop: Header=BB3_6 Depth=3
	movq	live_relevant_regs(%rip), %rdi
	movq	%rax, %rsi
	callq	bitmap_set_bit
.LBB3_11:                               # %if.end41
                                        #   in Loop: Header=BB3_6 Depth=3
	testq	%rbx, %rbx
	je	.LBB3_13
.LBB3_12:                               # %for.inc
                                        #   in Loop: Header=BB3_6 Depth=3
	movq	%r14, %rax
	andq	%r13, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB3_6
.LBB3_13:                               # %if.end47
                                        #   in Loop: Header=BB3_4 Depth=2
	movq	32(%r15), %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB3_55
	.align	16, 0x90
.LBB3_14:                               # %for.body24.1
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%r14, %rax
	je	.LBB3_54
# BB#15:                                # %if.then26.1
                                        #   in Loop: Header=BB3_14 Depth=3
	notq	%rax
	movl	16(%r15), %edx
	andq	%rax, %r14
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rbx,%rdx), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$53, %rdx
	jge	.LBB3_16
# BB#51:                                # %cond.true.1
                                        #   in Loop: Header=BB3_14 Depth=3
	movq	eliminable_regset(%rip), %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	jne	.LBB3_53
	jmp	.LBB3_52
	.align	16, 0x90
.LBB3_16:                               # %cond.false.1
                                        #   in Loop: Header=BB3_14 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	reg_renumber(%rip), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	(%rsi,%rdx,2), %rcx
	testq	%rcx, %rcx
	js	.LBB3_53
.LBB3_52:                               # %if.then40.1
                                        #   in Loop: Header=BB3_14 Depth=3
	movq	live_relevant_regs(%rip), %rdi
	movq	%rax, %rsi
	callq	bitmap_set_bit
.LBB3_53:                               # %if.end41.1
                                        #   in Loop: Header=BB3_14 Depth=3
	testq	%r14, %r14
	je	.LBB3_55
.LBB3_54:                               # %for.inc.1
                                        #   in Loop: Header=BB3_14 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$64, %rax
	jb	.LBB3_14
	jmp	.LBB3_55
	.align	16, 0x90
.LBB3_17:                               # %if.end54
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	(%r12), %eax
	cmpq	$35, %rax
	jne	.LBB3_19
# BB#18:                                #   in Loop: Header=BB3_1 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB3_38
	.align	16, 0x90
.LBB3_19:                               # %if.end54
                                        #   in Loop: Header=BB3_1 Depth=1
	cmpq	$37, %rax
	jne	.LBB3_21
# BB#20:                                #   in Loop: Header=BB3_1 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB3_38
	.align	16, 0x90
.LBB3_21:                               # %if.then62
                                        #   in Loop: Header=BB3_1 Depth=1
	callq	new_insn_chain
	movq	%rax, %r15
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 8(%r15)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%r15, (%rax)
	movq	%r12, 32(%r15)
	movq	24(%rsp), %r14          # 8-byte Reload
	movl	%r14d, 24(%r15)
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_29
# BB#22:                                # %if.then73
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	56(%r12), %rbx
	jmp	.LBB3_23
.LBB3_29:                               # %if.else
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	live_relevant_regs(%rip), %rsi
	leaq	40(%r15), %rdi
	callq	bitmap_copy
	jmp	.LBB3_30
	.align	16, 0x90
.LBB3_27:                               # %for.inc103
                                        #   in Loop: Header=BB3_23 Depth=2
	movq	16(%rbx), %rbx
.LBB3_23:                               # %if.then73
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB3_28
# BB#24:                                # %for.body77
                                        #   in Loop: Header=BB3_23 Depth=2
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB3_27
# BB#25:                                # %land.lhs.true82
                                        #   in Loop: Header=BB3_23 Depth=2
	movq	8(%rbx), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB3_27
# BB#26:                                # %if.then90
                                        #   in Loop: Header=BB3_23 Depth=2
	movl	8(%rax), %edi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rdx
	callq	reg_dies
	jmp	.LBB3_27
.LBB3_28:                               # %for.end107
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	live_relevant_regs(%rip), %rsi
	leaq	40(%r15), %rdi
	callq	bitmap_copy
	movq	32(%r12), %rdi
	leaq	64(%r15), %rdx
	movabsq	$reg_becomes_live, %rsi
	callq	note_stores
.LBB3_30:                               # %if.end112
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_37
# BB#31:                                # %if.then120
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	56(%r12), %rbx
	jmp	.LBB3_32
	.align	16, 0x90
.LBB3_36:                               # %for.inc155
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	16(%rbx), %rbx
.LBB3_32:                               # %if.then120
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB3_37
# BB#33:                                # %for.body127
                                        #   in Loop: Header=BB3_32 Depth=2
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$655360, %rax           # imm = 0xA0000
	jne	.LBB3_36
# BB#34:                                # %land.lhs.true133
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	8(%rbx), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB3_36
# BB#35:                                # %if.then141
                                        #   in Loop: Header=BB3_32 Depth=2
	movl	8(%rax), %edi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rdx
	callq	reg_dies
	jmp	.LBB3_36
	.align	16, 0x90
.LBB3_37:                               #   in Loop: Header=BB3_1 Depth=1
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r15, 16(%rsp)          # 8-byte Spill
.LBB3_38:                               # %if.end161
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movq	8(%rax), %rax
	cmpq	%rax, %r12
	movabsq	$0, %rax
	jne	.LBB3_40
# BB#39:                                # %if.end161
                                        #   in Loop: Header=BB3_1 Depth=1
	movabsq	$1, %rax
.LBB3_40:                               # %if.end161
                                        #   in Loop: Header=BB3_1 Depth=1
	addq	%rax, %r14
	movl	n_basic_blocks(%rip), %eax
	movq	24(%r12), %r12
	movq	%r14, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	jne	.LBB3_1
	jmp	.LBB3_41
	.align	16, 0x90
.LBB3_47:                               # %for.inc222
                                        #   in Loop: Header=BB3_41 Depth=1
	movq	24(%r12), %r12
.LBB3_41:                               # %for.cond177.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB3_48
# BB#42:                                # %for.body179
                                        #   in Loop: Header=BB3_41 Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_47
# BB#43:                                # %land.lhs.true187
                                        #   in Loop: Header=BB3_41 Depth=1
	movq	32(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB3_47
# BB#44:                                # %land.lhs.true187
                                        #   in Loop: Header=BB3_41 Depth=1
	addq	$-44, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	ja	.LBB3_56
# BB#45:                                # %land.lhs.true210
                                        #   in Loop: Header=BB3_41 Depth=1
	movq	%r12, %rdi
	callq	prev_real_insn
	testq	%rax, %rax
	je	.LBB3_56
# BB#46:                                # %land.lhs.true214
                                        #   in Loop: Header=BB3_41 Depth=1
	movq	%r12, %rdi
	callq	prev_real_insn
	movzbl	(%rax), %eax
	cmpq	$33, %rax
	je	.LBB3_47
.LBB3_56:                               # %if.then220
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.build_insn_chain, %rcx
	movq	$1898, %rsi             # imm = 0x76A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB3_48:                               # %do.body233
	movq	live_relevant_regs(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_50
# BB#49:                                # %if.then235
	callq	bitmap_clear
	movq	$0, live_relevant_regs(%rip)
.LBB3_50:                               # %do.end237
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	$0, (%rax)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	build_insn_chain, .Ltmp71-build_insn_chain
	.cfi_endproc

	.globl	retry_global_alloc
	.align	16, 0x90
	.type	retry_global_alloc,@function
retry_global_alloc:                     # @retry_global_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp79:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp80:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp81:
	.cfi_def_cfa_offset 48
.Ltmp82:
	.cfi_offset %rbx, -40
.Ltmp83:
	.cfi_offset %r12, -32
.Ltmp84:
	.cfi_offset %r14, -24
.Ltmp85:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	movq	reg_allocno(%rip), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movslq	(%rax,%rbx,4), %r12
	movq	%rsi, %r15
	testq	%r12, %r12
	js	.LBB4_5
# BB#1:                                 # %if.then
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	find_reg
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rax
	testq	%rax, %rax
	jns	.LBB4_4
# BB#2:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	reg_alternate_class
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_4
# BB#3:                                 # %if.then7
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	find_reg
.LBB4_4:                                # %if.end
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rax
	testq	%rax, %rax
	js	.LBB4_5
# BB#6:                                 # %if.then13
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rbx,8), %rcx
	movl	%eax, 8(%rcx)
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	mark_home_live  # TAILCALL
.LBB4_5:                                # %if.end21
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp86:
	.size	retry_global_alloc, .Ltmp86-retry_global_alloc
	.cfi_endproc

	.globl	mark_elimination
	.align	16, 0x90
	.type	mark_elimination,@function
mark_elimination:                       # @mark_elimination
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp95:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp96:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp97:
	.cfi_def_cfa_offset 48
.Ltmp98:
	.cfi_offset %rbx, -48
.Ltmp99:
	.cfi_offset %r12, -40
.Ltmp100:
	.cfi_offset %r13, -32
.Ltmp101:
	.cfi_offset %r14, -24
.Ltmp102:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rax, %rax
	jle	.LBB5_5
# BB#1:
	movq	$4, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	(%rax,%rbx,8), %rax
	movq	64(%rax), %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	bitmap_bit_p
	testq	%r13, %rax
	je	.LBB5_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	bitmap_clear_bit
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	bitmap_set_bit
.LBB5_4:                                # %for.inc
                                        #   in Loop: Header=BB5_2 Depth=1
	movslq	n_basic_blocks(%rip), %rax
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB5_2
.LBB5_5:                                # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp103:
	.size	mark_elimination, .Ltmp103-mark_elimination
	.cfi_endproc

	.align	16, 0x90
	.type	reg_dies,@function
reg_dies:                               # @reg_dies
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp108:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp109:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp110:
	.cfi_def_cfa_offset 32
.Ltmp111:
	.cfi_offset %rbx, -32
.Ltmp112:
	.cfi_offset %r14, -24
.Ltmp113:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB6_19
# BB#1:                                 # %if.then
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rbx, %rax
	cmpq	$8, %rax
	je	.LBB6_5
# BB#2:                                 # %if.then
	leaq	-21(%rbx), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_5
# BB#3:                                 # %if.then
	leaq	-45(%rbx), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_5
# BB#4:                                 # %if.then
	leaq	-29(%rbx), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB6_5
# BB#10:                                # %cond.false
	andq	%rsi, %rax
	cmpq	$24, %rax
	jne	.LBB6_11
# BB#13:                                # %cond.true23
	movzbl	target_flags+3(%rip), %r15d
	andq	$2, %r15
	xorq	$6, %r15
	jmp	.LBB6_15
.LBB6_19:                               # %if.else
	movq	live_relevant_regs(%rip), %rdi
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	reg_renumber(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movswq	(%rdx,%rax,2), %rax
	testq	%rax, %rax
	js	.LBB6_20
# BB#21:                                # %if.then52
	addq	$64, %r14
	movq	%r14, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	bitmap_set_bit  # TAILCALL
.LBB6_5:                                # %cond.true
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$5, %rax
	movq	$2, %r15
	je	.LBB6_15
# BB#6:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB6_7
# BB#8:                                 # %lor.rhs
	movabsq	$1, %r15
	incq	%r15
	jmp	.LBB6_15
.LBB6_11:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB6_14
# BB#12:                                # %cond.true19
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	xorq	$3, %r15
	jmp	.LBB6_15
.LBB6_7:
	movabsq	$0, %r15
	incq	%r15
	jmp	.LBB6_15
.LBB6_14:                               # %while.cond.preheader
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movl	target_flags(%rip), %edi
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB6_20
.LBB6_15:                               # %while.body.lr.ph
	addq	$64, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB6_16:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	live_relevant_regs(%rip), %rdi
	decq	%r15
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
	movzbl	fixed_regs(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB6_18
# BB#17:                                # %if.then46
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
.LBB6_18:                               # %if.end
                                        #   in Loop: Header=BB6_16 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB6_16
.LBB6_20:                               # %if.end55
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp114:
	.size	reg_dies, .Ltmp114-reg_dies
	.cfi_endproc

	.align	16, 0x90
	.type	reg_becomes_live,@function
reg_becomes_live:                       # @reg_becomes_live
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp119:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp120:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp121:
	.cfi_def_cfa_offset 32
.Ltmp122:
	.cfi_offset %rbx, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movl	(%rdi), %eax
	movq	%rdx, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB7_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movl	(%rdi), %eax
.LBB7_2:                                # %if.end
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB7_23
# BB#3:                                 # %if.end5
	movslq	8(%rdi), %rbx
	cmpq	$52, %rbx
	jg	.LBB7_22
# BB#4:                                 # %if.then9
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rbx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB7_8
# BB#5:                                 # %if.then9
	leaq	-21(%rbx), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB7_8
# BB#6:                                 # %if.then9
	leaq	-45(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB7_8
# BB#7:                                 # %if.then9
	movq	%rbx, %rdx
	addq	$-29, %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB7_8
# BB#13:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB7_14
# BB#16:                                # %cond.true44
	movzbl	target_flags+3(%rip), %r15d
	andq	$2, %r15
	xorq	$6, %r15
	jmp	.LBB7_19
.LBB7_22:                               # %if.else
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rax
	testq	%rax, %rax
	js	.LBB7_23
# BB#24:                                # %if.then77
	movq	live_relevant_regs(%rip), %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movq	%r14, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	bitmap_set_bit  # TAILCALL
.LBB7_8:                                # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r15
	je	.LBB7_19
# BB#9:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB7_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %r15
	incq	%r15
	jmp	.LBB7_19
.LBB7_14:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB7_17
# BB#15:                                # %cond.true37
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	xorq	$3, %r15
	jmp	.LBB7_19
.LBB7_10:
	movabsq	$0, %r15
	incq	%r15
	jmp	.LBB7_19
.LBB7_17:                               # %while.cond.preheader
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r15
.LBB7_18:                               # %while.cond.preheader
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_23
.LBB7_19:                               # %while.body
	movq	live_relevant_regs(%rip), %rdi
	decq	%r15
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movzbl	fixed_regs(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB7_21
# BB#20:                                # %if.then70
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
.LBB7_21:                               # %if.end71
	incq	%rbx
	jmp	.LBB7_18
.LBB7_23:                               # %if.end79
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	reg_becomes_live, .Ltmp125-reg_becomes_live
	.cfi_endproc

	.globl	dump_global_regs
	.align	16, 0x90
	.type	dump_global_regs,@function
dump_global_regs:                       # @dump_global_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp134:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp135:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp136:
	.cfi_def_cfa_offset 48
.Ltmp137:
	.cfi_offset %rbx, -48
.Ltmp138:
	.cfi_offset %r12, -40
.Ltmp139:
	.cfi_offset %r13, -32
.Ltmp140:
	.cfi_offset %r14, -24
.Ltmp141:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$.L.str1, %rdi
	movq	$26, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movslq	max_regno(%rip), %rax
	cmpq	$54, %rax
	jl	.LBB8_6
# BB#1:
	xorq	%r12, %r12
	movq	$53, %rbx
	movabsq	$3074457345618258603, %r13 # imm = 0x2AAAAAAAAAAAAAAB
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rcx
	testq	%rcx, %rcx
	js	.LBB8_5
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB8_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str2, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rax
	imulq	%r13
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	addq	%rdx, %rax
	addq	%rax, %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rsi
	testq	%r15, %rsi
	jne	.LBB8_5
# BB#4:                                 # %if.then9
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
.LBB8_5:                                # %for.inc
                                        #   in Loop: Header=BB8_2 Depth=1
	incq	%rbx
	movslq	max_regno(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB8_2
.LBB8_6:                                # %for.end
	movabsq	$.L.str4, %rdi
	movq	$21, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	xorq	%rbx, %rbx
	movabsq	$.L.str5, %r15
	.align	16, 0x90
.LBB8_7:                                # %for.body17
                                        # =>This Inner Loop Header: Depth=1
	movzbl	regs_ever_live(%rbx), %eax
	testq	%rax, %rax
	je	.LBB8_9
# BB#8:                                 # %if.then20
                                        #   in Loop: Header=BB8_7 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	fprintf
.LBB8_9:                                # %for.inc23
                                        #   in Loop: Header=BB8_7 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB8_7
# BB#10:                                # %for.end25
	movabsq	$.L.str6, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.Ltmp142:
	.size	dump_global_regs, .Ltmp142-dump_global_regs
	.cfi_endproc

	.align	16, 0x90
	.type	record_one_conflict,@function
record_one_conflict:                    # @record_one_conflict
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB9_8
# BB#1:                                 # %do.body
	movslq	allocno_row_words(%rip), %r8
	testq	%r8, %r8
	jle	.LBB9_11
# BB#2:                                 # %for.body.lr.ph
	movq	allocno(%rip), %rsi
	movq	allocnos_live(%rip), %r9
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	addq	$24, %rsi
	.align	16, 0x90
.LBB9_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_4 Depth 2
	movq	(%r9), %rdi
	testq	%rdi, %rdi
	movq	%rsi, %rdx
	je	.LBB9_7
	.align	16, 0x90
.LBB9_4:                                # %for.body3
                                        #   Parent Loop BB9_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	$1, %rdi
	je	.LBB9_6
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB9_4 Depth=2
	orq	%rax, (%rdx)
.LBB9_6:                                # %for.inc
                                        #   in Loop: Header=BB9_4 Depth=2
	movq	$1, %rcx
	shrq	%cl, %rdi
	addq	$64, %rdx
	testq	%rdi, %rdi
	jne	.LBB9_4
.LBB9_7:                                # %for.inc6
                                        #   in Loop: Header=BB9_3 Depth=1
	decq	%r8
	addq	$8, %r9
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$4096, %rsi             # imm = 0x1000
	testq	%rdx, %rdx
	jg	.LBB9_3
	jmp	.LBB9_11
.LBB9_8:                                # %if.else
	movq	$32, %rcx
	movq	reg_allocno(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movslq	(%rax,%rdi,4), %rdi
	movq	hard_regs_live(%rip), %r8
	movq	allocno(%rip), %rax
	movslq	allocno_row_words(%rip), %rdx
	movq	$6, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	orq	%r8, 24(%rax,%rsi)
	testq	%rdx, %rdx
	jle	.LBB9_11
# BB#9:                                 # %for.body17.lr.ph
	movq	allocnos_live(%rip), %rax
	movq	conflicts(%rip), %rsi
	imulq	%rdx, %rdi
	leaq	-1(%rdx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	(%rsi,%rdi,8), %rsi
	.align	16, 0x90
.LBB9_10:                               # %for.body17
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rdx,8), %rcx
	orq	%rcx, (%rsi,%rdx,8)
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rdx
	testq	%rdi, %rdi
	jg	.LBB9_10
.LBB9_11:                               # %if.end27
	retq
.Ltmp143:
	.size	record_one_conflict, .Ltmp143-record_one_conflict
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reg_clobber,@function
mark_reg_clobber:                       # @mark_reg_clobber
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rsi), %eax
	cmpq	$49, %rax
	jne	.LBB10_1
# BB#2:                                 # %if.then
	jmp	mark_reg_store  # TAILCALL
.LBB10_1:                               # %if.end
	retq
.Ltmp144:
	.size	mark_reg_clobber, .Ltmp144-mark_reg_clobber
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reg_death,@function
mark_reg_death:                         # @mark_reg_death
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	8(%rdi), %rax
	cmpq	$53, %rax
	jl	.LBB11_3
# BB#1:                                 # %if.then
	movq	reg_allocno(%rip), %rcx
	movslq	(%rcx,%rax,4), %rcx
	testq	%rcx, %rcx
	js	.LBB11_3
# BB#2:                                 # %if.then3
	movabsq	$4294967232, %rdx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	movq	allocnos_live(%rip), %r8
	notq	%rsi
	movq	$3, %rcx
	shrq	%cl, %rdx
	andq	%rsi, (%r8,%rdx)
.LBB11_3:                               # %if.end10
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %r8
	testq	%r8, %r8
	jns	.LBB11_5
# BB#4:                                 # %if.end10
	movq	%rax, %r8
.LBB11_5:                               # %if.end10
	cmpq	$52, %r8
	jg	.LBB11_24
# BB#6:                                 # %land.lhs.true
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	fixed_regs(%rax), %eax
	testq	%rax, %rax
	jne	.LBB11_24
# BB#7:                                 # %if.then24
	movzbl	2(%rdi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r8, %rcx
	cmpq	$8, %rcx
	je	.LBB11_11
# BB#8:                                 # %if.then24
	leaq	-21(%r8), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB11_11
# BB#9:                                 # %if.then24
	leaq	-45(%r8), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB11_11
# BB#10:                                # %if.then24
	leaq	-29(%r8), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB11_11
# BB#16:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB11_17
# BB#19:                                # %cond.true73
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_21
.LBB11_11:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_21
# BB#12:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB11_13
# BB#14:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB11_21
.LBB11_17:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB11_20
# BB#18:                                # %cond.true63
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_21
.LBB11_13:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB11_21
.LBB11_20:                              # %cond.end94
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB11_24
.LBB11_21:                              # %while.body.lr.ph
	movq	hard_regs_live(%rip), %rdx
	addq	%r8, %rax
	.align	16, 0x90
.LBB11_22:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	movq	$1, %rdx
	movq	%r8, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdi, %rdx
	incq	%r8
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdi
	jl	.LBB11_22
# BB#23:                                # %while.cond.if.end103.loopexit_crit_edge
	movq	%rdx, hard_regs_live(%rip)
.LBB11_24:                              # %if.end103
	retq
.Ltmp145:
	.size	mark_reg_death, .Ltmp145-mark_reg_death
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reg_store,@function
mark_reg_store:                         # @mark_reg_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp152:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp153:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp154:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp155:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp156:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp157:
	.cfi_def_cfa_offset 64
.Ltmp158:
	.cfi_offset %rbx, -48
.Ltmp159:
	.cfi_offset %r12, -40
.Ltmp160:
	.cfi_offset %r13, -32
.Ltmp161:
	.cfi_offset %r14, -24
.Ltmp162:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB12_2
# BB#1:                                 # %if.then
	movq	8(%r15), %r15
	movl	(%r15), %eax
.LBB12_2:                               # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB12_95
# BB#3:                                 # %if.end5
	movslq	n_regs_set(%rip), %rax
	movq	regs_set(%rip), %rcx
	leaq	1(%rax), %rdx
	movl	%edx, n_regs_set(%rip)
	movq	%r15, (%rcx,%rax,8)
	testq	%rsi, %rsi
	je	.LBB12_72
# BB#4:                                 # %land.lhs.true
	movzbl	(%rsi), %eax
	cmpq	$49, %rax
	je	.LBB12_72
# BB#5:                                 # %if.then10
	movq	16(%rsi), %r13
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	rtx_format(,%rcx,8), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$101, %rcx
	jne	.LBB12_6
# BB#7:                                 # %if.then.i
	movq	8(%r13), %r13
	movl	(%r13), %eax
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB12_8
.LBB12_6:
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
.LBB12_8:                               # %if.end.i
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB12_9
# BB#11:                                # %land.lhs.true.i
	movq	8(%r13), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB12_72
# BB#12:                                # %if.then22.i
	movl	8(%rcx), %r12d
	cmpq	$52, %r12
	ja	.LBB12_14
# BB#13:                                # %if.then37.i
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r13), %edx
	shrq	%cl, %rax
	andq	$255, %rsi
	andq	$255, %rax
	movq	%r12, %rdi
	movq	%rax, %rcx
	callq	subreg_regno_offset
	jmp	.LBB12_15
.LBB12_9:                               # %if.end.i
	cmpq	$61, %rcx
	jne	.LBB12_72
# BB#10:                                # %if.then8.i
	movl	8(%r13), %r12d
	xorq	%r9, %r9
	jmp	.LBB12_16
.LBB12_14:                              # %if.else55.i
	movl	16(%r13), %eax
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	xorq	%rdx, %rdx
	divq	%rcx
.LBB12_15:                              # %if.end63.i
	movq	%rax, %r9
.LBB12_16:                              # %if.end63.i
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB12_17
# BB#19:                                # %land.lhs.true77.i
	movq	8(%r15), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB12_72
# BB#20:                                # %if.then85.i
	movl	8(%rcx), %ebx
	cmpq	$52, %rbx
	ja	.LBB12_22
# BB#21:                                # %if.then100.i
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r15), %edx
	shrq	%cl, %rax
	andq	$255, %rsi
	andq	$255, %rax
	movq	%rbx, %rdi
	movq	%rax, %rcx
	movq	%r9, %r14
	callq	subreg_regno_offset
	movq	%r14, %r9
	jmp	.LBB12_23
.LBB12_17:                              # %if.end63.i
	cmpq	$61, %rcx
	jne	.LBB12_72
# BB#18:                                # %if.then68.i
	movl	8(%r15), %ebx
	jmp	.LBB12_24
.LBB12_22:                              # %if.else120.i
	movl	16(%r15), %eax
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	xorq	%rdx, %rdx
	divq	%rcx
.LBB12_23:                              # %if.end132.i
	subq	%rax, %r9
.LBB12_24:                              # %if.end132.i
	movq	reg_renumber(%rip), %rax
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%r12, %rcx
	andq	%r11, %rcx
	movswq	(%rax,%rcx,2), %r8
	testq	%r8, %r8
	jns	.LBB12_26
# BB#25:                                # %if.end132.i
	movq	%r12, %r8
.LBB12_26:                              # %if.end132.i
	movq	%rbx, %rcx
	andq	%r11, %rcx
	movswq	(%rax,%rcx,2), %rdi
	testq	%rdi, %rdi
	jns	.LBB12_28
# BB#27:                                # %if.end132.i
	movq	%rbx, %rdi
.LBB12_28:                              # %if.end132.i
	movq	%rdi, %rax
	andq	%r11, %rax
	cmpq	$52, %rax
	ja	.LBB12_50
# BB#29:                                # %if.end132.i
	movq	%r8, %rax
	andq	%r11, %rax
	cmpq	$53, %rax
	jb	.LBB12_50
# BB#30:                                # %land.lhs.true158.i
	movq	reg_allocno(%rip), %rcx
	movslq	(%rcx,%rax,4), %rbx
	testq	%rbx, %rbx
	js	.LBB12_50
# BB#31:                                # %if.then163.i
	subq	%r9, %rdi
	movq	%rdi, %rcx
	andq	%r11, %rcx
	cmpq	$52, %rcx
	ja	.LBB12_50
# BB#32:                                # %if.then167.i
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB12_33
# BB#34:                                # %if.then169.i
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	$1, %rsi
	movq	allocno(%rip), %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rsi
	movq	$6, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	orq	%rsi, 40(%rdx,%rax)
	jmp	.LBB12_35
.LBB12_33:                              # %if.then167.if.end174_crit_edge.i
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	allocno(%rip), %rdx
	movq	$1, %rsi
	movq	%rdi, %rcx
	shlq	%cl, %rsi
.LBB12_35:                              # %if.end174.i
	movq	$6, %rcx
	shlq	%cl, %rbx
	orq	%rsi, 32(%rdx,%rbx)
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	movabsq	$0, %r9
	movabsq	$1, %r14
	movq	%r9, %rcx
	jne	.LBB12_37
# BB#36:                                # %if.end174.i
	movq	%r14, %rcx
.LBB12_37:                              # %if.end174.i
	leaq	-21(%rdi), %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-45(%rdi), %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-29(%rdi), %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	sbbq	%rsi, %rsi
	andq	$1, %rsi
	orq	%rcx, %rsi
	leaq	48(%rdx,%rbx), %r12
	movq	%rdi, %rbx
	jmp	.LBB12_38
.LBB12_96:                              # %for.body.i
                                        #   in Loop: Header=BB12_38 Depth=1
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rax, (%r12)
	incq	%rbx
.LBB12_38:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2(%r15), %eax
	testq	%rsi, %rsi
	je	.LBB12_43
# BB#39:                                # %cond.true.i
                                        #   in Loop: Header=BB12_38 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_48
# BB#40:                                # %lor.rhs.i
                                        #   in Loop: Header=BB12_38 Depth=1
	cmpq	$6, %rcx
	movq	%r9, %rax
	jne	.LBB12_42
# BB#41:                                # %lor.rhs.i
                                        #   in Loop: Header=BB12_38 Depth=1
	movq	%r14, %rax
.LBB12_42:                              # %lor.rhs.i
                                        #   in Loop: Header=BB12_38 Depth=1
	incq	%rax
	jmp	.LBB12_48
.LBB12_43:                              # %cond.false.i
                                        #   in Loop: Header=BB12_38 Depth=1
	cmpq	$24, %rax
	jne	.LBB12_44
# BB#46:                                # %cond.true234.i
                                        #   in Loop: Header=BB12_38 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_48
.LBB12_44:                              # %cond.false.i
                                        #   in Loop: Header=BB12_38 Depth=1
	cmpq	$18, %rax
	jne	.LBB12_47
# BB#45:                                # %cond.true224.i
                                        #   in Loop: Header=BB12_38 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_48
.LBB12_47:                              # %cond.false238.i
                                        #   in Loop: Header=BB12_38 Depth=1
	movl	target_flags(%rip), %r10d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r10
	andq	$4, %r10
	leaq	3(%rax,%r10), %rax
	addq	$4, %r10
	cqto
	idivq	%r10
.LBB12_48:                              # %cond.end257.i
                                        #   in Loop: Header=BB12_38 Depth=1
	addq	%rdi, %rax
	andq	%r11, %rax
	movq	%rbx, %rcx
	andq	%r11, %rcx
	cmpq	%rax, %rcx
	jb	.LBB12_96
# BB#49:
	movq	(%rsp), %r9             # 8-byte Reload
.LBB12_50:                              # %if.end270.i
	movq	%r8, %rax
	andq	%r11, %rax
	cmpq	$52, %rax
	ja	.LBB12_72
# BB#51:                                # %if.end270.i
	andq	%r11, %rdi
	cmpq	$53, %rdi
	jb	.LBB12_72
# BB#52:                                # %land.lhs.true276.i
	movq	reg_allocno(%rip), %rax
	movslq	(%rax,%rdi,4), %rax
	testq	%rax, %rax
	js	.LBB12_72
# BB#53:                                # %if.then281.i
	addq	%r9, %r8
	movq	%r8, %rcx
	andq	%r11, %rcx
	cmpq	$52, %rcx
	ja	.LBB12_72
# BB#54:                                # %if.then285.i
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB12_55
# BB#56:                                # %if.then287.i
	movq	$1, %rsi
	movq	allocno(%rip), %rdx
	movq	%r8, %rcx
	shlq	%cl, %rsi
	movq	$6, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	orq	%rsi, 40(%rdx,%rdi)
	jmp	.LBB12_57
.LBB12_55:                              # %if.then285.if.end296_crit_edge.i
	movq	allocno(%rip), %rdx
	movq	$1, %rsi
	movq	%r8, %rcx
	shlq	%cl, %rsi
.LBB12_57:                              # %if.end296.i
	movq	$6, %rcx
	shlq	%cl, %rax
	orq	%rsi, 32(%rdx,%rax)
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r8, %rcx
	cmpq	$8, %rcx
	movabsq	$0, %r10
	movabsq	$1, %r9
	movq	%r10, %rcx
	jne	.LBB12_59
# BB#58:                                # %if.end296.i
	movq	%r9, %rcx
.LBB12_59:                              # %if.end296.i
	leaq	-21(%r8), %rsi
	andq	%r11, %rsi
	cmpq	$8, %rsi
	sbbq	%rsi, %rsi
	andq	$1, %rsi
	orq	%rcx, %rsi
	leaq	-45(%r8), %rcx
	andq	%r11, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rsi, %rcx
	leaq	-29(%r8), %rsi
	andq	%r11, %rsi
	cmpq	$8, %rsi
	sbbq	%rbx, %rbx
	andq	$1, %rbx
	orq	%rcx, %rbx
	leaq	48(%rdx,%rax), %r14
	movq	%r8, %rdi
	jmp	.LBB12_60
.LBB12_71:                              # %for.body392.i
                                        #   in Loop: Header=BB12_60 Depth=1
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	orq	%rax, (%r14)
	incq	%rdi
.LBB12_60:                              # %for.cond305.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2(%r13), %eax
	testq	%rbx, %rbx
	je	.LBB12_65
# BB#61:                                # %cond.true329.i
                                        #   in Loop: Header=BB12_60 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_70
# BB#62:                                # %lor.rhs337.i
                                        #   in Loop: Header=BB12_60 Depth=1
	cmpq	$6, %rcx
	movq	%r10, %rax
	jne	.LBB12_64
# BB#63:                                # %lor.rhs337.i
                                        #   in Loop: Header=BB12_60 Depth=1
	movq	%r9, %rax
.LBB12_64:                              # %lor.rhs337.i
                                        #   in Loop: Header=BB12_60 Depth=1
	incq	%rax
	jmp	.LBB12_70
.LBB12_65:                              # %cond.false347.i
                                        #   in Loop: Header=BB12_60 Depth=1
	cmpq	$24, %rax
	jne	.LBB12_66
# BB#68:                                # %cond.true363.i
                                        #   in Loop: Header=BB12_60 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_70
.LBB12_66:                              # %cond.false347.i
                                        #   in Loop: Header=BB12_60 Depth=1
	cmpq	$18, %rax
	jne	.LBB12_69
# BB#67:                                # %cond.true353.i
                                        #   in Loop: Header=BB12_60 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_70
.LBB12_69:                              # %cond.false367.i
                                        #   in Loop: Header=BB12_60 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB12_70:                              # %cond.end387.i
                                        #   in Loop: Header=BB12_60 Depth=1
	addq	%r8, %rax
	andq	%r11, %rax
	movq	%rdi, %rcx
	andq	%r11, %rcx
	cmpq	%rax, %rcx
	jb	.LBB12_71
.LBB12_72:                              # %if.end14
	movslq	8(%r15), %r14
	cmpq	$53, %r14
	jl	.LBB12_75
# BB#73:                                # %if.then18
	movq	reg_allocno(%rip), %rax
	movslq	(%rax,%r14,4), %rcx
	testq	%rcx, %rcx
	js	.LBB12_75
# BB#74:                                # %if.then22
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$1, %rdx
	movq	allocnos_live(%rip), %rsi
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, (%rsi,%rax)
	movq	%r14, %rdi
	callq	record_one_conflict
.LBB12_75:                              # %if.end30
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rbx
	testq	%rbx, %rbx
	jns	.LBB12_77
# BB#76:                                # %if.end30
	movq	%r14, %rbx
.LBB12_77:                              # %if.end30
	cmpq	$52, %rbx
	jg	.LBB12_95
# BB#78:                                # %land.lhs.true42
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	fixed_regs(%rax), %eax
	testq	%rax, %rax
	jne	.LBB12_95
# BB#79:                                # %if.then46
	movzbl	2(%r15), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	je	.LBB12_83
# BB#80:                                # %if.then46
	leaq	-21(%rbx), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB12_83
# BB#81:                                # %if.then46
	leaq	-45(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB12_83
# BB#82:                                # %if.then46
	leaq	-29(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB12_83
# BB#88:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB12_89
# BB#91:                                # %cond.true96
	movzbl	target_flags+3(%rip), %r14d
	andq	$2, %r14
	xorq	$6, %r14
	jmp	.LBB12_93
.LBB12_83:                              # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r14
	je	.LBB12_93
# BB#84:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB12_85
# BB#86:                                # %lor.rhs
	movabsq	$1, %r14
	incq	%r14
	jmp	.LBB12_93
.LBB12_89:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB12_92
# BB#90:                                # %cond.true87
	movl	target_flags(%rip), %r14d
	movq	$25, %rcx
	shrq	%cl, %r14
	andq	$1, %r14
	xorq	$3, %r14
	jmp	.LBB12_93
.LBB12_85:
	movabsq	$0, %r14
	incq	%r14
	jmp	.LBB12_93
.LBB12_92:                              # %cond.end117
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r14
	movq	$32, %rcx
                                        # kill: RAX<def> R14<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB12_95
.LBB12_93:                              # %while.body.lr.ph
	addq	%rbx, %r14
	.align	16, 0x90
.LBB12_94:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	record_one_conflict
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rax, hard_regs_live(%rip)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB12_94
.LBB12_95:                              # %if.end126
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp163:
	.size	mark_reg_store, .Ltmp163-mark_reg_store
	.cfi_endproc

	.type	max_allocno,@object     # @max_allocno
	.local	max_allocno
	.comm	max_allocno,4,4
	.type	no_global_alloc_regs,@object # @no_global_alloc_regs
	.local	no_global_alloc_regs
	.comm	no_global_alloc_regs,8,8
	.type	eliminable_regset,@object # @eliminable_regset
	.local	eliminable_regset
	.comm	eliminable_regset,8,8
	.type	regs_used_so_far,@object # @regs_used_so_far
	.local	regs_used_so_far
	.comm	regs_used_so_far,8,8
	.type	reg_allocno,@object     # @reg_allocno
	.local	reg_allocno
	.comm	reg_allocno,8,8
	.type	reg_may_share,@object   # @reg_may_share
	.local	reg_may_share
	.comm	reg_may_share,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"global.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.global_alloc,@object # @__FUNCTION__.global_alloc
.L__FUNCTION__.global_alloc:
	.asciz	"global_alloc"
	.size	.L__FUNCTION__.global_alloc, 13

	.type	allocno,@object         # @allocno
	.local	allocno
	.comm	allocno,8,8
	.type	local_reg_live_length,@object # @local_reg_live_length
	.local	local_reg_live_length
	.comm	local_reg_live_length,212,16
	.type	local_reg_n_refs,@object # @local_reg_n_refs
	.local	local_reg_n_refs
	.comm	local_reg_n_refs,212,16
	.type	local_reg_freq,@object  # @local_reg_freq
	.local	local_reg_freq
	.comm	local_reg_freq,212,16
	.type	allocno_row_words,@object # @allocno_row_words
	.local	allocno_row_words
	.comm	allocno_row_words,4,4
	.type	conflicts,@object       # @conflicts
	.local	conflicts
	.comm	conflicts,8,8
	.type	allocnos_live,@object   # @allocnos_live
	.local	allocnos_live
	.comm	allocnos_live,8,8
	.type	allocno_order,@object   # @allocno_order
	.local	allocno_order
	.comm	allocno_order,8,8
	.type	live_relevant_regs,@object # @live_relevant_regs
	.local	live_relevant_regs
	.comm	live_relevant_regs,8,8
	.type	.L__FUNCTION__.build_insn_chain,@object # @__FUNCTION__.build_insn_chain
.L__FUNCTION__.build_insn_chain:
	.asciz	"build_insn_chain"
	.size	.L__FUNCTION__.build_insn_chain, 17

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	";; Register dispositions:\n"
	.size	.L.str1, 27

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%d in %d  "
	.size	.L.str2, 11

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"\n\n;; Hard regs used: "
	.size	.L.str4, 22

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" %d"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\n\n"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	";; %d regs to allocate:"
	.size	.L.str7, 24

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"+%d"
	.size	.L.str8, 4

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	" (%d)"
	.size	.L.str9, 6

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	";; %d conflicts:"
	.size	.L.str10, 17

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	";; %d preferences:"
	.size	.L.str11, 19

	.type	regs_set,@object        # @regs_set
	.local	regs_set
	.comm	regs_set,8,8
	.type	hard_regs_live,@object  # @hard_regs_live
	.local	hard_regs_live
	.comm	hard_regs_live,8,8
	.type	n_regs_set,@object      # @n_regs_set
	.local	n_regs_set
	.comm	n_regs_set,4,4

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
