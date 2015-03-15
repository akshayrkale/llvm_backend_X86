	.file	"dwarf2out.c.bc"
	.text
	.globl	dwarf2out_do_frame
	.align	16, 0x90
	.type	dwarf2out_do_frame,@function
dwarf2out_do_frame:                     # @dwarf2out_do_frame
	.cfi_startproc
# BB#0:                                 # %entry
	movl	flag_unwind_tables(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB0_6
# BB#1:                                 # %switch.early.test
	movl	write_symbols(%rip), %ecx
	cmpq	$4, %rcx
	je	.LBB0_6
# BB#2:                                 # %switch.early.test
	cmpq	$7, %rcx
	je	.LBB0_6
# BB#3:                                 # %lor.rhs
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_4
# BB#5:                                 # %lor.rhs
	movabsq	$1, %rax
.LBB0_6:                                # %lor.end
	retq
.LBB0_4:
	movabsq	$0, %rax
	retq
.Ltmp0:
	.size	dwarf2out_do_frame, .Ltmp0-dwarf2out_do_frame
	.cfi_endproc

	.globl	default_eh_frame_section
	.align	16, 0x90
	.type	default_eh_frame_section,@function
default_eh_frame_section:               # @default_eh_frame_section
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str, %rdi
	movq	$512, %rsi              # imm = 0x200
	jmp	named_section_flags  # TAILCALL
.Ltmp1:
	.size	default_eh_frame_section, .Ltmp1-default_eh_frame_section
	.cfi_endproc

	.globl	expand_builtin_dwarf_fp_regnum
	.align	16, 0x90
	.type	expand_builtin_dwarf_fp_regnum,@function
expand_builtin_dwarf_fp_regnum:         # @expand_builtin_dwarf_fp_regnum
	.cfi_startproc
# BB#0:                                 # %entry
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB2_1
# BB#2:                                 # %entry
	movabsq	$svr4_dbx_register_map+24, %rax
	jmp	.LBB2_3
.LBB2_1:
	movabsq	$dbx64_register_map+24, %rax
.LBB2_3:                                # %entry
	movslq	(%rax), %rsi
	xorq	%rdi, %rdi
	jmp	gen_rtx_CONST_INT  # TAILCALL
.Ltmp2:
	.size	expand_builtin_dwarf_fp_regnum, .Ltmp2-expand_builtin_dwarf_fp_regnum
	.cfi_endproc

	.globl	expand_builtin_init_dwarf_reg_sizes
	.align	16, 0x90
	.type	expand_builtin_init_dwarf_reg_sizes,@function
expand_builtin_init_dwarf_reg_sizes:    # @expand_builtin_init_dwarf_reg_sizes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp9:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp10:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp11:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp12:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp14:
	.cfi_def_cfa_offset 64
.Ltmp15:
	.cfi_offset %rbx, -48
.Ltmp16:
	.cfi_offset %r12, -40
.Ltmp17:
	.cfi_offset %r13, -32
.Ltmp18:
	.cfi_offset %r14, -24
.Ltmp19:
	.cfi_offset %r15, -16
	movq	integer_types(%rip), %rax
	movl	60(%rax), %r15d
	movq	$9, %rcx
	shrq	%cl, %r15
	andq	$127, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	callq	expand_expr
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$svr4_dbx_register_map, %r14
	.align	16, 0x90
.LBB3_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	movq	%r14, %rax
	je	.LBB3_3
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB3_1 Depth=1
	movabsq	$dbx64_register_map, %rax
.LBB3_3:                                # %for.body
                                        #   in Loop: Header=BB3_1 Depth=1
	movslq	(%rax,%rbx,4), %rax
	cmpq	$16, %rax
	jg	.LBB3_6
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	mode_size(%r15), %edx
	imulq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_6
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB3_1 Depth=1
	movslq	reg_raw_mode(,%rbx,4), %rax
	movq	$32, %rcx
	movzbl	mode_size(%rax), %r12d
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r15, %rsi
	callq	adjust_address_1
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB3_6:                                # %for.inc
                                        #   in Loop: Header=BB3_1 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB3_1
# BB#7:                                 # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp20:
	.size	expand_builtin_init_dwarf_reg_sizes, .Ltmp20-expand_builtin_init_dwarf_reg_sizes
	.cfi_endproc

	.globl	dwarf2out_cfi_label
	.align	16, 0x90
	.type	dwarf2out_cfi_label,@function
dwarf2out_cfi_label:                    # @dwarf2out_cfi_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 16
.Ltmp24:
	.cfi_offset %rbx, -16
	movq	dwarf2out_cfi_label.label_num(%rip), %rcx
	leaq	1(%rcx), %rax
	movq	%rax, dwarf2out_cfi_label.label_num(%rip)
	movabsq	$dwarf2out_cfi_label.label, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp25:
	.size	dwarf2out_cfi_label, .Ltmp25-dwarf2out_cfi_label
	.cfi_endproc

	.globl	dwarf2out_def_cfa
	.align	16, 0x90
	.type	dwarf2out_def_cfa,@function
dwarf2out_def_cfa:                      # @dwarf2out_def_cfa
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp27:
	.cfi_def_cfa_offset 48
	xorq	%rax, %rax
	movl	%eax, 32(%rsp)
	movq	$0, 24(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	movq	%rax, 8(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	8(%rsp), %rsi
	callq	def_cfa_1
	addq	$40, %rsp
	retq
.Ltmp28:
	.size	dwarf2out_def_cfa, .Ltmp28-dwarf2out_def_cfa
	.cfi_endproc

	.align	16, 0x90
	.type	def_cfa_1,@function
def_cfa_1:                              # @def_cfa_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp40:
	.cfi_def_cfa_offset 80
.Ltmp41:
	.cfi_offset %rbx, -48
.Ltmp42:
	.cfi_offset %r12, -40
.Ltmp43:
	.cfi_offset %r13, -32
.Ltmp44:
	.cfi_offset %r14, -24
.Ltmp45:
	.cfi_offset %r15, -16
	movq	24(%rsi), %rax
	movq	%rax, cfa+24(%rip)
	movq	16(%rsi), %rax
	movq	%rax, cfa+16(%rip)
	movq	(%rsi), %rax
	movq	8(%rsi), %rcx
	movq	%rcx, cfa+8(%rip)
	movq	%rax, cfa(%rip)
	movq	(%rsi), %r9
	movq	8(%rsi), %rcx
	movq	16(%rsi), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	cfa_store(%rip), %rax
	movl	24(%rsi), %esi
	movq	%rdi, (%rsp)            # 8-byte Spill
	cmpq	%r9, %rax
	jne	.LBB6_3
# BB#1:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	jne	.LBB6_3
# BB#2:                                 # %if.then
	movq	%rcx, cfa_store+8(%rip)
.LBB6_3:                                # %if.end
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB6_4
# BB#5:                                 # %if.end
	movabsq	$dbx64_register_map, %r8
	jmp	.LBB6_6
.LBB6_4:
	movabsq	$svr4_dbx_register_map, %r8
.LBB6_6:                                # %if.end
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	cie_cfi_head(%rip), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	movq	$-1, %r13
	je	.LBB6_7
# BB#8:
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movabsq	$.L.str226, %r12
	xorq	%r10, %r10
	xorq	%r14, %r14
	.align	16, 0x90
.LBB6_9:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_15 Depth 2
	movl	8(%rbx), %eax
	addq	$-12, %rax
	andq	%r11, %rax
	cmpq	$3, %rax
	ja	.LBB6_103
# BB#10:                                # %for.body.i
                                        #   in Loop: Header=BB6_9 Depth=1
	jmpq	*.LJTI6_0(,%rax,8)
.LBB6_13:                               # %sw.bb3.i102
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	16(%rbx), %r13
	movq	24(%rbx), %r14
	jmp	.LBB6_103
.LBB6_12:                               # %sw.bb1.i97
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	16(%rbx), %r13
	jmp	.LBB6_103
.LBB6_11:                               # %sw.bb.i94
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	16(%rbx), %r14
	jmp	.LBB6_103
.LBB6_14:                               # %sw.bb9.i110
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	16(%rbx), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	movq	$-1, %r13
	movabsq	$0, %r10
	movabsq	$0, %r14
	movabsq	$0, %r15
	je	.LBB6_103
	jmp	.LBB6_15
.LBB6_21:                               # %sw.bb22.i.i128
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	24(%rax), %r14
	jmp	.LBB6_101
.LBB6_100:                              # %sw.bb2.i.i118
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	24(%rax), %r13
	jmp	.LBB6_101
.LBB6_20:                               # %sw.bb12.i.i125
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	24(%rax), %r13
	movq	48(%rax), %r10
.LBB6_101:                              # %for.inc.i.i210
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	%rdi, %r15
	jmp	.LBB6_102
	.align	16, 0x90
.LBB6_15:                               # %for.body.i.i113
                                        #   Parent Loop BB6_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	8(%rax), %ecx
	leaq	-3(%rcx), %rdx
	andq	%r11, %rdx
	cmpq	$147, %rdx
	ja	.LBB6_93
# BB#16:                                # %for.body.i.i113
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	$1, %r15
	jmpq	*.LJTI6_1(,%rdx,8)
.LBB6_17:                               # %sw.bb.i.i116
                                        #   in Loop: Header=BB6_15 Depth=2
	addq	$-80, %rcx
	jmp	.LBB6_18
	.align	16, 0x90
.LBB6_19:                               # %sw.bb4.i.i122
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	24(%rax), %r10
	addq	$-112, %rcx
.LBB6_18:                               # %for.inc.i.i210
                                        #   in Loop: Header=BB6_15 Depth=2
	andq	%r11, %rcx
	movq	%rdi, %r15
	movq	%rcx, %r13
.LBB6_102:                              # %for.inc.i.i210
                                        #   in Loop: Header=BB6_15 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%r15, %rdi
	jne	.LBB6_15
	.align	16, 0x90
.LBB6_103:                              # %lookup_cfa_1.exit211
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_9
	jmp	.LBB6_104
.LBB6_7:
	xorq	%r10, %r10
	xorq	%r14, %r14
.LBB6_104:                              # %for.end.i
	movl	fde_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_200
# BB#105:                               # %if.then.i
	decq	%rax
	movq	fde_table(%rip), %rcx
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	andq	%r11, %rax
	leaq	(%rax,%rax,4), %rax
	movq	24(%rcx,%rax,8), %rcx
	testq	%rcx, %rcx
	je	.LBB6_200
# BB#106:
	movabsq	$.L.str226, %r12
	.align	16, 0x90
.LBB6_107:                              # %for.body4.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_113 Depth 2
	movl	8(%rcx), %eax
	addq	$-12, %rax
	andq	%r11, %rax
	cmpq	$3, %rax
	ja	.LBB6_199
# BB#108:                               # %for.body4.i
                                        #   in Loop: Header=BB6_107 Depth=1
	jmpq	*.LJTI6_2(,%rax,8)
.LBB6_111:                              # %sw.bb3.i
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	16(%rcx), %r13
	movq	24(%rcx), %r14
	jmp	.LBB6_199
.LBB6_110:                              # %sw.bb1.i
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	16(%rcx), %r13
	jmp	.LBB6_199
.LBB6_109:                              # %sw.bb.i
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	16(%rcx), %r14
	jmp	.LBB6_199
.LBB6_112:                              # %sw.bb9.i
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	16(%rcx), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	movq	$-1, %r13
	movabsq	$0, %r10
	movabsq	$0, %r14
	movabsq	$0, %r15
	je	.LBB6_199
	jmp	.LBB6_113
.LBB6_119:                              # %sw.bb22.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	24(%rax), %r14
	jmp	.LBB6_197
.LBB6_196:                              # %sw.bb2.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	24(%rax), %r13
	jmp	.LBB6_197
.LBB6_118:                              # %sw.bb12.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	24(%rax), %r13
	movq	48(%rax), %r10
.LBB6_197:                              # %for.inc.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	%rdi, %r15
	jmp	.LBB6_198
	.align	16, 0x90
.LBB6_113:                              # %for.body.i.i
                                        #   Parent Loop BB6_107 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	8(%rax), %ebx
	leaq	-3(%rbx), %rdx
	andq	%r11, %rdx
	cmpq	$147, %rdx
	ja	.LBB6_191
# BB#114:                               # %for.body.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	$1, %r15
	jmpq	*.LJTI6_3(,%rdx,8)
.LBB6_115:                              # %sw.bb.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	addq	$-80, %rbx
	jmp	.LBB6_116
	.align	16, 0x90
.LBB6_117:                              # %sw.bb4.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	24(%rax), %r10
	addq	$-112, %rbx
.LBB6_116:                              # %for.inc.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	andq	%r11, %rbx
	movq	%rdi, %r15
	movq	%rbx, %r13
.LBB6_198:                              # %for.inc.i.i
                                        #   in Loop: Header=BB6_113 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%r15, %rdi
	jne	.LBB6_113
	.align	16, 0x90
.LBB6_199:                              # %lookup_cfa_1.exit
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB6_107
.LBB6_200:                              # %lookup_cfa.exit
	movslq	(%r8,%r9,4), %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rsi
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB6_202
# BB#201:                               # %lookup_cfa.exit
	movq	%rdx, %rcx
.LBB6_202:                              # %lookup_cfa.exit
	cmpq	%r10, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_204
# BB#203:                               # %lookup_cfa.exit
	movq	%rdx, %rax
.LBB6_204:                              # %lookup_cfa.exit
	cmpq	%r13, %r12
	jne	.LBB6_208
# BB#205:                               # %lookup_cfa.exit
	cmpq	%r14, 16(%rsp)          # 8-byte Folded Reload
	jne	.LBB6_208
# BB#206:                               # %lookup_cfa.exit
	andq	%rbx, %r15
	movq	%rsi, %rdx
	andq	%rbx, %rdx
	cmpq	%r15, %rdx
	jne	.LBB6_208
# BB#207:                               # %lookup_cfa.exit
	orq	%rax, %rcx
	je	.LBB6_208
# BB#234:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_93:                               # %sw.default.i.i.i203
	movabsq	$.L.str365, %r12
	jmp	.LBB6_99
.LBB6_208:                              # %if.end28
	movq	$32, %rdi
	movq	%rsi, %r15
	callq	xmalloc
	movq	%r15, %rcx
	movq	%rax, %r15
	cmpq	%r13, %r12
	movq	$0, (%r15)
	movq	$0, 24(%r15)
	movq	$0, 16(%r15)
	jne	.LBB6_211
# BB#209:                               # %if.end28
	testq	%rbx, %rcx
	jne	.LBB6_211
# BB#210:                               # %if.then36
	movq	$14, %rax
	movl	%eax, 8(%r15)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r15)
	jmp	.LBB6_233
.LBB6_191:                              # %sw.default.i.i.i
	movabsq	$.L.str365, %r12
.LBB6_99:                               # %dwarf_stack_op_name.exit.i.i207
	movabsq	$.L.str497, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	internal_error
.LBB6_211:                              # %if.else
	movq	%r15, %rdx
	addq	$16, %rdx
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%r14, %rax
	movq	%rax, %r14
	jne	.LBB6_215
# BB#212:                               # %if.else
	cmpq	$-1, %r13
	je	.LBB6_215
# BB#213:                               # %land.lhs.true46
	testq	%rbx, %rcx
	jne	.LBB6_217
# BB#214:                               # %if.then49
	movq	$13, %rax
	movl	%eax, 8(%r15)
	movq	%r12, 16(%r15)
	jmp	.LBB6_233
.LBB6_215:                              # %if.else53
	testq	%rbx, %rcx
	je	.LBB6_216
.LBB6_217:                              # %if.else64
	movq	%rdx, %r13
	movq	$15, %rax
	movl	%eax, 8(%r15)
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB6_221
# BB#218:                               # %if.then1.i
	andq	%r12, %rbx
	cmpq	$31, %rbx
	ja	.LBB6_220
# BB#219:                               # %if.then3.i
	addq	$112, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movl	%r12d, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 24(%rbx)
	jmp	.LBB6_224
.LBB6_221:                              # %if.else10.i
	andq	%r12, %rbx
	cmpq	$31, %rbx
	ja	.LBB6_223
# BB#222:                               # %if.then14.i
	addq	$80, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movl	%r12d, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	$0, 24(%rbx)
	jmp	.LBB6_224
.LBB6_220:                              # %if.else.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$146, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r12, 24(%rbx)
	movl	%eax, 40(%rbx)
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 48(%rbx)
	jmp	.LBB6_225
.LBB6_216:                              # %if.then57
	movq	$12, %rax
	movl	%eax, 8(%r15)
	movq	%r12, 16(%r15)
	movq	%r14, 24(%r15)
	jmp	.LBB6_233
.LBB6_223:                              # %if.else19.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$144, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r12, 24(%rbx)
.LBB6_224:                              # %if.end23.i
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
.LBB6_225:                              # %if.end23.i
	movq	%rbx, 24(%rsp)
	movq	$5, %rax
	movl	%eax, 16(%rbx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	testq	%rbx, %rbx
	movq	$6, %rcx
	movl	%ecx, 8(%rax)
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	je	.LBB6_226
	.align	16, 0x90
.LBB6_227:                              # %for.cond.i.for.cond.i_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rcx
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_227
	jmp	.LBB6_228
.LBB6_226:
	leaq	24(%rsp), %rcx
.LBB6_228:                              # %add_loc_descr.exit.i
	movq	%rax, (%rcx)
	testq	%r14, %r14
	je	.LBB6_232
# BB#229:                               # %if.then27.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$35, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	24(%rsp), %rcx
	.align	16, 0x90
.LBB6_230:                              # %for.cond.i82.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB6_230
# BB#231:                               # %add_loc_descr.exit83.i
	movq	%rax, (%rdx)
.LBB6_232:                              # %build_cfa_loc.exit
	movq	24(%rsp), %rax
	movq	%rax, (%r13)
.LBB6_233:                              # %if.end70
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r15, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_fde_cfi  # TAILCALL
.LBB6_94:                               # %dwarf_stack_op_name.exit.loopexit.i.i204
	movabsq	$.L.str220, %r12
	jmp	.LBB6_99
.LBB6_95:                               # %dwarf_stack_op_name.exit.i.loopexit.i205
	movabsq	$.L.str222, %r12
	jmp	.LBB6_99
.LBB6_96:                               # %dwarf_stack_op_name.exit.i.i207.loopexit
	movabsq	$.L.str223, %r12
	jmp	.LBB6_99
.LBB6_97:                               # %dwarf_stack_op_name.exit.i.i207.loopexit288
	movabsq	$.L.str224, %r12
	jmp	.LBB6_99
.LBB6_22:                               # %sw.bb7.i.i.i132
	movabsq	$.L.str227, %r12
	jmp	.LBB6_99
.LBB6_23:                               # %sw.bb8.i.i.i133
	movabsq	$.L.str228, %r12
	jmp	.LBB6_99
.LBB6_24:                               # %sw.bb9.i.i.i134
	movabsq	$.L.str229, %r12
	jmp	.LBB6_99
.LBB6_25:                               # %sw.bb10.i.i.i135
	movabsq	$.L.str230, %r12
	jmp	.LBB6_99
.LBB6_26:                               # %sw.bb11.i.i.i136
	movabsq	$.L.str231, %r12
	jmp	.LBB6_99
.LBB6_27:                               # %sw.bb12.i.i.i137
	movabsq	$.L.str232, %r12
	jmp	.LBB6_99
.LBB6_28:                               # %sw.bb13.i.i.i138
	movabsq	$.L.str233, %r12
	jmp	.LBB6_99
.LBB6_29:                               # %sw.bb14.i.i.i139
	movabsq	$.L.str234, %r12
	jmp	.LBB6_99
.LBB6_30:                               # %sw.bb15.i.i.i140
	movabsq	$.L.str235, %r12
	jmp	.LBB6_99
.LBB6_31:                               # %sw.bb16.i.i.i141
	movabsq	$.L.str236, %r12
	jmp	.LBB6_99
.LBB6_32:                               # %sw.bb17.i.i.i142
	movabsq	$.L.str237, %r12
	jmp	.LBB6_99
.LBB6_33:                               # %sw.bb18.i.i.i143
	movabsq	$.L.str238, %r12
	jmp	.LBB6_99
.LBB6_34:                               # %sw.bb19.i.i.i144
	movabsq	$.L.str239, %r12
	jmp	.LBB6_99
.LBB6_35:                               # %sw.bb20.i.i.i145
	movabsq	$.L.str240, %r12
	jmp	.LBB6_99
.LBB6_36:                               # %sw.bb21.i.i.i146
	movabsq	$.L.str241, %r12
	jmp	.LBB6_99
.LBB6_37:                               # %sw.bb22.i.i.i147
	movabsq	$.L.str242, %r12
	jmp	.LBB6_99
.LBB6_38:                               # %sw.bb23.i.i.i148
	movabsq	$.L.str243, %r12
	jmp	.LBB6_99
.LBB6_39:                               # %sw.bb24.i.i.i149
	movabsq	$.L.str244, %r12
	jmp	.LBB6_99
.LBB6_40:                               # %sw.bb25.i.i.i150
	movabsq	$.L.str245, %r12
	jmp	.LBB6_99
.LBB6_41:                               # %sw.bb26.i.i.i151
	movabsq	$.L.str246, %r12
	jmp	.LBB6_99
.LBB6_42:                               # %sw.bb27.i.i.i152
	movabsq	$.L.str247, %r12
	jmp	.LBB6_99
.LBB6_43:                               # %sw.bb28.i.i.i153
	movabsq	$.L.str248, %r12
	jmp	.LBB6_99
.LBB6_44:                               # %sw.bb30.i.i.i154
	movabsq	$.L.str250, %r12
	jmp	.LBB6_99
.LBB6_45:                               # %sw.bb31.i.i.i155
	movabsq	$.L.str251, %r12
	jmp	.LBB6_99
.LBB6_46:                               # %sw.bb32.i.i.i156
	movabsq	$.L.str252, %r12
	jmp	.LBB6_99
.LBB6_47:                               # %sw.bb33.i.i.i157
	movabsq	$.L.str253, %r12
	jmp	.LBB6_99
.LBB6_48:                               # %sw.bb34.i.i.i158
	movabsq	$.L.str254, %r12
	jmp	.LBB6_99
.LBB6_49:                               # %sw.bb35.i.i.i159
	movabsq	$.L.str255, %r12
	jmp	.LBB6_99
.LBB6_50:                               # %sw.bb36.i.i.i160
	movabsq	$.L.str256, %r12
	jmp	.LBB6_99
.LBB6_51:                               # %sw.bb37.i.i.i161
	movabsq	$.L.str257, %r12
	jmp	.LBB6_99
.LBB6_52:                               # %sw.bb38.i.i.i162
	movabsq	$.L.str258, %r12
	jmp	.LBB6_99
.LBB6_53:                               # %sw.bb39.i.i.i163
	movabsq	$.L.str259, %r12
	jmp	.LBB6_99
.LBB6_54:                               # %sw.bb40.i.i.i164
	movabsq	$.L.str260, %r12
	jmp	.LBB6_99
.LBB6_55:                               # %sw.bb41.i.i.i165
	movabsq	$.L.str261, %r12
	jmp	.LBB6_99
.LBB6_56:                               # %sw.bb42.i.i.i166
	movabsq	$.L.str262, %r12
	jmp	.LBB6_99
.LBB6_57:                               # %sw.bb43.i.i.i167
	movabsq	$.L.str263, %r12
	jmp	.LBB6_99
.LBB6_58:                               # %sw.bb44.i.i.i168
	movabsq	$.L.str264, %r12
	jmp	.LBB6_99
.LBB6_59:                               # %sw.bb45.i.i.i169
	movabsq	$.L.str265, %r12
	jmp	.LBB6_99
.LBB6_60:                               # %sw.bb46.i.i.i170
	movabsq	$.L.str266, %r12
	jmp	.LBB6_99
.LBB6_61:                               # %sw.bb47.i.i.i171
	movabsq	$.L.str267, %r12
	jmp	.LBB6_99
.LBB6_62:                               # %sw.bb48.i.i.i172
	movabsq	$.L.str268, %r12
	jmp	.LBB6_99
.LBB6_63:                               # %sw.bb49.i.i.i173
	movabsq	$.L.str269, %r12
	jmp	.LBB6_99
.LBB6_64:                               # %sw.bb50.i.i.i174
	movabsq	$.L.str270, %r12
	jmp	.LBB6_99
.LBB6_65:                               # %sw.bb51.i.i.i175
	movabsq	$.L.str271, %r12
	jmp	.LBB6_99
.LBB6_66:                               # %sw.bb52.i.i.i176
	movabsq	$.L.str272, %r12
	jmp	.LBB6_99
.LBB6_67:                               # %sw.bb53.i.i.i177
	movabsq	$.L.str273, %r12
	jmp	.LBB6_99
.LBB6_68:                               # %sw.bb54.i.i.i178
	movabsq	$.L.str274, %r12
	jmp	.LBB6_99
.LBB6_69:                               # %sw.bb55.i.i.i179
	movabsq	$.L.str275, %r12
	jmp	.LBB6_99
.LBB6_70:                               # %sw.bb56.i.i.i180
	movabsq	$.L.str276, %r12
	jmp	.LBB6_99
.LBB6_71:                               # %sw.bb57.i.i.i181
	movabsq	$.L.str277, %r12
	jmp	.LBB6_99
.LBB6_72:                               # %sw.bb58.i.i.i182
	movabsq	$.L.str278, %r12
	jmp	.LBB6_99
.LBB6_73:                               # %sw.bb59.i.i.i183
	movabsq	$.L.str279, %r12
	jmp	.LBB6_99
.LBB6_74:                               # %sw.bb60.i.i.i184
	movabsq	$.L.str280, %r12
	jmp	.LBB6_99
.LBB6_75:                               # %sw.bb61.i.i.i185
	movabsq	$.L.str281, %r12
	jmp	.LBB6_99
.LBB6_76:                               # %sw.bb62.i.i.i186
	movabsq	$.L.str282, %r12
	jmp	.LBB6_99
.LBB6_77:                               # %sw.bb63.i.i.i187
	movabsq	$.L.str283, %r12
	jmp	.LBB6_99
.LBB6_78:                               # %sw.bb64.i.i.i188
	movabsq	$.L.str284, %r12
	jmp	.LBB6_99
.LBB6_79:                               # %sw.bb65.i.i.i189
	movabsq	$.L.str285, %r12
	jmp	.LBB6_99
.LBB6_80:                               # %sw.bb66.i.i.i190
	movabsq	$.L.str286, %r12
	jmp	.LBB6_99
.LBB6_81:                               # %sw.bb67.i.i.i191
	movabsq	$.L.str287, %r12
	jmp	.LBB6_99
.LBB6_82:                               # %sw.bb68.i.i.i192
	movabsq	$.L.str288, %r12
	jmp	.LBB6_99
.LBB6_83:                               # %sw.bb69.i.i.i193
	movabsq	$.L.str289, %r12
	jmp	.LBB6_99
.LBB6_84:                               # %sw.bb70.i.i.i194
	movabsq	$.L.str290, %r12
	jmp	.LBB6_99
.LBB6_85:                               # %sw.bb71.i.i.i195
	movabsq	$.L.str291, %r12
	jmp	.LBB6_99
.LBB6_86:                               # %sw.bb72.i.i.i196
	movabsq	$.L.str292, %r12
	jmp	.LBB6_99
.LBB6_87:                               # %sw.bb73.i.i.i197
	movabsq	$.L.str293, %r12
	jmp	.LBB6_99
.LBB6_88:                               # %sw.bb139.i.i.i198
	movabsq	$.L.str359, %r12
	jmp	.LBB6_99
.LBB6_89:                               # %sw.bb141.i.i.i199
	movabsq	$.L.str361, %r12
	jmp	.LBB6_99
.LBB6_90:                               # %sw.bb142.i.i.i200
	movabsq	$.L.str362, %r12
	jmp	.LBB6_99
.LBB6_91:                               # %sw.bb143.i.i.i201
	movabsq	$.L.str363, %r12
	jmp	.LBB6_99
.LBB6_92:                               # %sw.bb144.i.i.i202
	movabsq	$.L.str364, %r12
	jmp	.LBB6_99
.LBB6_192:                              # %dwarf_stack_op_name.exit.loopexit.i.i
	movabsq	$.L.str220, %r12
	jmp	.LBB6_99
.LBB6_193:                              # %dwarf_stack_op_name.exit.i.loopexit.i
	movabsq	$.L.str222, %r12
	jmp	.LBB6_99
.LBB6_194:                              # %dwarf_stack_op_name.exit.i.i.loopexit
	movabsq	$.L.str223, %r12
	jmp	.LBB6_99
.LBB6_195:                              # %dwarf_stack_op_name.exit.i.i.loopexit287
	movabsq	$.L.str224, %r12
	jmp	.LBB6_99
.LBB6_98:                               # %dwarf_stack_op_name.exit.i.i207.loopexit2
	movabsq	$.L.str225, %r12
	jmp	.LBB6_99
.LBB6_120:                              # %sw.bb7.i.i.i
	movabsq	$.L.str227, %r12
	jmp	.LBB6_99
.LBB6_121:                              # %sw.bb8.i.i.i
	movabsq	$.L.str228, %r12
	jmp	.LBB6_99
.LBB6_122:                              # %sw.bb9.i.i.i
	movabsq	$.L.str229, %r12
	jmp	.LBB6_99
.LBB6_123:                              # %sw.bb10.i.i.i
	movabsq	$.L.str230, %r12
	jmp	.LBB6_99
.LBB6_124:                              # %sw.bb11.i.i.i
	movabsq	$.L.str231, %r12
	jmp	.LBB6_99
.LBB6_125:                              # %sw.bb12.i.i.i
	movabsq	$.L.str232, %r12
	jmp	.LBB6_99
.LBB6_126:                              # %sw.bb13.i.i.i
	movabsq	$.L.str233, %r12
	jmp	.LBB6_99
.LBB6_127:                              # %sw.bb14.i.i.i
	movabsq	$.L.str234, %r12
	jmp	.LBB6_99
.LBB6_128:                              # %sw.bb15.i.i.i
	movabsq	$.L.str235, %r12
	jmp	.LBB6_99
.LBB6_129:                              # %sw.bb16.i.i.i
	movabsq	$.L.str236, %r12
	jmp	.LBB6_99
.LBB6_130:                              # %sw.bb17.i.i.i
	movabsq	$.L.str237, %r12
	jmp	.LBB6_99
.LBB6_131:                              # %sw.bb18.i.i.i
	movabsq	$.L.str238, %r12
	jmp	.LBB6_99
.LBB6_132:                              # %sw.bb19.i.i.i
	movabsq	$.L.str239, %r12
	jmp	.LBB6_99
.LBB6_133:                              # %sw.bb20.i.i.i
	movabsq	$.L.str240, %r12
	jmp	.LBB6_99
.LBB6_134:                              # %sw.bb21.i.i.i
	movabsq	$.L.str241, %r12
	jmp	.LBB6_99
.LBB6_135:                              # %sw.bb22.i.i.i
	movabsq	$.L.str242, %r12
	jmp	.LBB6_99
.LBB6_136:                              # %sw.bb23.i.i.i
	movabsq	$.L.str243, %r12
	jmp	.LBB6_99
.LBB6_137:                              # %sw.bb24.i.i.i
	movabsq	$.L.str244, %r12
	jmp	.LBB6_99
.LBB6_138:                              # %sw.bb25.i.i.i
	movabsq	$.L.str245, %r12
	jmp	.LBB6_99
.LBB6_139:                              # %sw.bb26.i.i.i
	movabsq	$.L.str246, %r12
	jmp	.LBB6_99
.LBB6_140:                              # %sw.bb27.i.i.i
	movabsq	$.L.str247, %r12
	jmp	.LBB6_99
.LBB6_141:                              # %sw.bb28.i.i.i
	movabsq	$.L.str248, %r12
	jmp	.LBB6_99
.LBB6_142:                              # %sw.bb30.i.i.i
	movabsq	$.L.str250, %r12
	jmp	.LBB6_99
.LBB6_143:                              # %sw.bb31.i.i.i
	movabsq	$.L.str251, %r12
	jmp	.LBB6_99
.LBB6_144:                              # %sw.bb32.i.i.i
	movabsq	$.L.str252, %r12
	jmp	.LBB6_99
.LBB6_145:                              # %sw.bb33.i.i.i
	movabsq	$.L.str253, %r12
	jmp	.LBB6_99
.LBB6_146:                              # %sw.bb34.i.i.i
	movabsq	$.L.str254, %r12
	jmp	.LBB6_99
.LBB6_147:                              # %sw.bb35.i.i.i
	movabsq	$.L.str255, %r12
	jmp	.LBB6_99
.LBB6_148:                              # %sw.bb36.i.i.i
	movabsq	$.L.str256, %r12
	jmp	.LBB6_99
.LBB6_149:                              # %sw.bb37.i.i.i
	movabsq	$.L.str257, %r12
	jmp	.LBB6_99
.LBB6_150:                              # %sw.bb38.i.i.i
	movabsq	$.L.str258, %r12
	jmp	.LBB6_99
.LBB6_151:                              # %sw.bb39.i.i.i
	movabsq	$.L.str259, %r12
	jmp	.LBB6_99
.LBB6_152:                              # %sw.bb40.i.i.i
	movabsq	$.L.str260, %r12
	jmp	.LBB6_99
.LBB6_153:                              # %sw.bb41.i.i.i
	movabsq	$.L.str261, %r12
	jmp	.LBB6_99
.LBB6_154:                              # %sw.bb42.i.i.i
	movabsq	$.L.str262, %r12
	jmp	.LBB6_99
.LBB6_155:                              # %sw.bb43.i.i.i
	movabsq	$.L.str263, %r12
	jmp	.LBB6_99
.LBB6_156:                              # %sw.bb44.i.i.i
	movabsq	$.L.str264, %r12
	jmp	.LBB6_99
.LBB6_157:                              # %sw.bb45.i.i.i
	movabsq	$.L.str265, %r12
	jmp	.LBB6_99
.LBB6_158:                              # %sw.bb46.i.i.i
	movabsq	$.L.str266, %r12
	jmp	.LBB6_99
.LBB6_159:                              # %sw.bb47.i.i.i
	movabsq	$.L.str267, %r12
	jmp	.LBB6_99
.LBB6_160:                              # %sw.bb48.i.i.i
	movabsq	$.L.str268, %r12
	jmp	.LBB6_99
.LBB6_161:                              # %sw.bb49.i.i.i
	movabsq	$.L.str269, %r12
	jmp	.LBB6_99
.LBB6_162:                              # %sw.bb50.i.i.i
	movabsq	$.L.str270, %r12
	jmp	.LBB6_99
.LBB6_163:                              # %sw.bb51.i.i.i
	movabsq	$.L.str271, %r12
	jmp	.LBB6_99
.LBB6_164:                              # %sw.bb52.i.i.i
	movabsq	$.L.str272, %r12
	jmp	.LBB6_99
.LBB6_165:                              # %sw.bb53.i.i.i
	movabsq	$.L.str273, %r12
	jmp	.LBB6_99
.LBB6_166:                              # %sw.bb54.i.i.i
	movabsq	$.L.str274, %r12
	jmp	.LBB6_99
.LBB6_167:                              # %sw.bb55.i.i.i
	movabsq	$.L.str275, %r12
	jmp	.LBB6_99
.LBB6_168:                              # %sw.bb56.i.i.i
	movabsq	$.L.str276, %r12
	jmp	.LBB6_99
.LBB6_169:                              # %sw.bb57.i.i.i
	movabsq	$.L.str277, %r12
	jmp	.LBB6_99
.LBB6_170:                              # %sw.bb58.i.i.i
	movabsq	$.L.str278, %r12
	jmp	.LBB6_99
.LBB6_171:                              # %sw.bb59.i.i.i
	movabsq	$.L.str279, %r12
	jmp	.LBB6_99
.LBB6_172:                              # %sw.bb60.i.i.i
	movabsq	$.L.str280, %r12
	jmp	.LBB6_99
.LBB6_173:                              # %sw.bb61.i.i.i
	movabsq	$.L.str281, %r12
	jmp	.LBB6_99
.LBB6_174:                              # %sw.bb62.i.i.i
	movabsq	$.L.str282, %r12
	jmp	.LBB6_99
.LBB6_175:                              # %sw.bb63.i.i.i
	movabsq	$.L.str283, %r12
	jmp	.LBB6_99
.LBB6_176:                              # %sw.bb64.i.i.i
	movabsq	$.L.str284, %r12
	jmp	.LBB6_99
.LBB6_177:                              # %sw.bb65.i.i.i
	movabsq	$.L.str285, %r12
	jmp	.LBB6_99
.LBB6_178:                              # %sw.bb66.i.i.i
	movabsq	$.L.str286, %r12
	jmp	.LBB6_99
.LBB6_179:                              # %sw.bb67.i.i.i
	movabsq	$.L.str287, %r12
	jmp	.LBB6_99
.LBB6_180:                              # %sw.bb68.i.i.i
	movabsq	$.L.str288, %r12
	jmp	.LBB6_99
.LBB6_181:                              # %sw.bb69.i.i.i
	movabsq	$.L.str289, %r12
	jmp	.LBB6_99
.LBB6_182:                              # %sw.bb70.i.i.i
	movabsq	$.L.str290, %r12
	jmp	.LBB6_99
.LBB6_183:                              # %sw.bb71.i.i.i
	movabsq	$.L.str291, %r12
	jmp	.LBB6_99
.LBB6_184:                              # %sw.bb72.i.i.i
	movabsq	$.L.str292, %r12
	jmp	.LBB6_99
.LBB6_185:                              # %sw.bb73.i.i.i
	movabsq	$.L.str293, %r12
	jmp	.LBB6_99
.LBB6_186:                              # %sw.bb139.i.i.i
	movabsq	$.L.str359, %r12
	jmp	.LBB6_99
.LBB6_187:                              # %sw.bb141.i.i.i
	movabsq	$.L.str361, %r12
	jmp	.LBB6_99
.LBB6_188:                              # %sw.bb142.i.i.i
	movabsq	$.L.str362, %r12
	jmp	.LBB6_99
.LBB6_189:                              # %sw.bb143.i.i.i
	movabsq	$.L.str363, %r12
	jmp	.LBB6_99
.LBB6_190:                              # %sw.bb144.i.i.i
	movabsq	$.L.str364, %r12
	jmp	.LBB6_99
.Ltmp46:
	.size	def_cfa_1, .Ltmp46-def_cfa_1
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI6_0:
	.quad	.LBB6_13
	.quad	.LBB6_12
	.quad	.LBB6_11
	.quad	.LBB6_14
.LJTI6_1:
	.quad	.LBB6_94
	.quad	.LBB6_93
	.quad	.LBB6_93
	.quad	.LBB6_102
	.quad	.LBB6_93
	.quad	.LBB6_95
	.quad	.LBB6_96
	.quad	.LBB6_97
	.quad	.LBB6_98
	.quad	.LBB6_99
	.quad	.LBB6_22
	.quad	.LBB6_23
	.quad	.LBB6_24
	.quad	.LBB6_25
	.quad	.LBB6_26
	.quad	.LBB6_27
	.quad	.LBB6_28
	.quad	.LBB6_29
	.quad	.LBB6_30
	.quad	.LBB6_31
	.quad	.LBB6_32
	.quad	.LBB6_33
	.quad	.LBB6_34
	.quad	.LBB6_35
	.quad	.LBB6_36
	.quad	.LBB6_37
	.quad	.LBB6_38
	.quad	.LBB6_39
	.quad	.LBB6_40
	.quad	.LBB6_41
	.quad	.LBB6_42
	.quad	.LBB6_43
	.quad	.LBB6_21
	.quad	.LBB6_44
	.quad	.LBB6_45
	.quad	.LBB6_46
	.quad	.LBB6_47
	.quad	.LBB6_48
	.quad	.LBB6_49
	.quad	.LBB6_50
	.quad	.LBB6_51
	.quad	.LBB6_52
	.quad	.LBB6_53
	.quad	.LBB6_54
	.quad	.LBB6_55
	.quad	.LBB6_56
	.quad	.LBB6_57
	.quad	.LBB6_58
	.quad	.LBB6_59
	.quad	.LBB6_60
	.quad	.LBB6_61
	.quad	.LBB6_62
	.quad	.LBB6_63
	.quad	.LBB6_64
	.quad	.LBB6_65
	.quad	.LBB6_66
	.quad	.LBB6_67
	.quad	.LBB6_68
	.quad	.LBB6_69
	.quad	.LBB6_70
	.quad	.LBB6_71
	.quad	.LBB6_72
	.quad	.LBB6_73
	.quad	.LBB6_74
	.quad	.LBB6_75
	.quad	.LBB6_76
	.quad	.LBB6_77
	.quad	.LBB6_78
	.quad	.LBB6_79
	.quad	.LBB6_80
	.quad	.LBB6_81
	.quad	.LBB6_82
	.quad	.LBB6_83
	.quad	.LBB6_84
	.quad	.LBB6_85
	.quad	.LBB6_86
	.quad	.LBB6_87
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_17
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_19
	.quad	.LBB6_100
	.quad	.LBB6_88
	.quad	.LBB6_20
	.quad	.LBB6_89
	.quad	.LBB6_90
	.quad	.LBB6_91
	.quad	.LBB6_92
.LJTI6_2:
	.quad	.LBB6_111
	.quad	.LBB6_110
	.quad	.LBB6_109
	.quad	.LBB6_112
.LJTI6_3:
	.quad	.LBB6_192
	.quad	.LBB6_191
	.quad	.LBB6_191
	.quad	.LBB6_198
	.quad	.LBB6_191
	.quad	.LBB6_193
	.quad	.LBB6_194
	.quad	.LBB6_195
	.quad	.LBB6_98
	.quad	.LBB6_99
	.quad	.LBB6_120
	.quad	.LBB6_121
	.quad	.LBB6_122
	.quad	.LBB6_123
	.quad	.LBB6_124
	.quad	.LBB6_125
	.quad	.LBB6_126
	.quad	.LBB6_127
	.quad	.LBB6_128
	.quad	.LBB6_129
	.quad	.LBB6_130
	.quad	.LBB6_131
	.quad	.LBB6_132
	.quad	.LBB6_133
	.quad	.LBB6_134
	.quad	.LBB6_135
	.quad	.LBB6_136
	.quad	.LBB6_137
	.quad	.LBB6_138
	.quad	.LBB6_139
	.quad	.LBB6_140
	.quad	.LBB6_141
	.quad	.LBB6_119
	.quad	.LBB6_142
	.quad	.LBB6_143
	.quad	.LBB6_144
	.quad	.LBB6_145
	.quad	.LBB6_146
	.quad	.LBB6_147
	.quad	.LBB6_148
	.quad	.LBB6_149
	.quad	.LBB6_150
	.quad	.LBB6_151
	.quad	.LBB6_152
	.quad	.LBB6_153
	.quad	.LBB6_154
	.quad	.LBB6_155
	.quad	.LBB6_156
	.quad	.LBB6_157
	.quad	.LBB6_158
	.quad	.LBB6_159
	.quad	.LBB6_160
	.quad	.LBB6_161
	.quad	.LBB6_162
	.quad	.LBB6_163
	.quad	.LBB6_164
	.quad	.LBB6_165
	.quad	.LBB6_166
	.quad	.LBB6_167
	.quad	.LBB6_168
	.quad	.LBB6_169
	.quad	.LBB6_170
	.quad	.LBB6_171
	.quad	.LBB6_172
	.quad	.LBB6_173
	.quad	.LBB6_174
	.quad	.LBB6_175
	.quad	.LBB6_176
	.quad	.LBB6_177
	.quad	.LBB6_178
	.quad	.LBB6_179
	.quad	.LBB6_180
	.quad	.LBB6_181
	.quad	.LBB6_182
	.quad	.LBB6_183
	.quad	.LBB6_184
	.quad	.LBB6_185
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_115
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_117
	.quad	.LBB6_196
	.quad	.LBB6_186
	.quad	.LBB6_118
	.quad	.LBB6_187
	.quad	.LBB6_188
	.quad	.LBB6_189
	.quad	.LBB6_190

	.text
	.globl	dwarf2out_window_save
	.align	16, 0x90
	.type	dwarf2out_window_save,@function
dwarf2out_window_save:                  # @dwarf2out_window_save
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 16
.Ltmp50:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$45, %rcx
	movl	%ecx, 8(%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	popq	%rbx
	jmp	add_fde_cfi  # TAILCALL
.Ltmp51:
	.size	dwarf2out_window_save, .Ltmp51-dwarf2out_window_save
	.cfi_endproc

	.align	16, 0x90
	.type	add_fde_cfi,@function
add_fde_cfi:                            # @add_fde_cfi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp58:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp59:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp60:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp61:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp62:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp63:
	.cfi_def_cfa_offset 64
.Ltmp64:
	.cfi_offset %rbx, -48
.Ltmp65:
	.cfi_offset %r12, -40
.Ltmp66:
	.cfi_offset %r13, -32
.Ltmp67:
	.cfi_offset %r14, -24
.Ltmp68:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB8_1
# BB#4:                                 # %if.then
	movl	fde_table_in_use(%rip), %ebx
	decq	%rbx
	movzbl	(%r13), %eax
	movq	fde_table(%rip), %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	testq	%rax, %rax
	jne	.LBB8_6
# BB#5:                                 # %if.then2
	movq	dwarf2out_cfi_label.label_num(%rip), %rcx
	leaq	1(%rcx), %rax
	movq	%rax, dwarf2out_cfi_label.label_num(%rip)
	movabsq	$dwarf2out_cfi_label.label, %r13
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%r13, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB8_6:                                # %if.end
	leaq	(%rbx,%rbx,4), %r12
	movq	8(%r15,%r12,8), %rsi
	testq	%rsi, %rsi
	je	.LBB8_9
# BB#7:                                 # %lor.lhs.false
	movq	%r13, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_8
.LBB8_9:                                # %if.then9
	movq	%r14, 8(%rsp)           # 8-byte Spill
	leaq	8(%r15,%r12,8), %r14
	movq	%r13, %rdi
	callq	xstrdup
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$4, %rcx
	movl	%ecx, 8(%rax)
	leaq	24(%r15,%r12,8), %rcx
	movq	%rbx, 16(%rax)
	movq	%rcx, %rsi
	.align	16, 0x90
.LBB8_10:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	movq	(%rdx), %rsi
	testq	%rsi, %rsi
	jne	.LBB8_10
# BB#11:                                # %add_cfi.exit
	movq	%rax, (%rdx)
	movq	8(%rsp), %r14           # 8-byte Reload
	jmp	.LBB8_12
.LBB8_1:
	movabsq	$cie_cfi_head, %rax
	.align	16, 0x90
.LBB8_2:                                # %for.cond.i37
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB8_2
# BB#3:                                 # %add_cfi.exit38
	movq	%r14, (%rcx)
	jmp	.LBB8_14
.LBB8_8:                                # %lor.lhs.false.if.end13_crit_edge
	leaq	24(%r15,%r12,8), %rcx
	.align	16, 0x90
.LBB8_12:                               # %for.cond.i32
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB8_12
# BB#13:                                # %add_cfi.exit33
	movq	%r14, (%rax)
.LBB8_14:                               # %if.end15
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp69:
	.size	add_fde_cfi, .Ltmp69-add_fde_cfi
	.cfi_endproc

	.globl	dwarf2out_args_size
	.align	16, 0x90
	.type	dwarf2out_args_size,@function
dwarf2out_args_size:                    # @dwarf2out_args_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp73:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp74:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp75:
	.cfi_def_cfa_offset 32
.Ltmp76:
	.cfi_offset %rbx, -24
.Ltmp77:
	.cfi_offset %r14, -16
	movq	old_args_size(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	cmpq	%rbx, %rax
	jne	.LBB9_2
# BB#1:                                 # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB9_2:                                # %if.end
	movq	%rbx, old_args_size(%rip)
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$46, %rcx
	movl	%ecx, 8(%rax)
	movq	%rbx, 16(%rax)
	movq	%r14, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	add_fde_cfi  # TAILCALL
.Ltmp78:
	.size	dwarf2out_args_size, .Ltmp78-dwarf2out_args_size
	.cfi_endproc

	.globl	dwarf2out_reg_save
	.align	16, 0x90
	.type	dwarf2out_reg_save,@function
dwarf2out_reg_save:                     # @dwarf2out_reg_save
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp84:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp85:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp86:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp88:
	.cfi_def_cfa_offset 48
.Ltmp89:
	.cfi_offset %rbx, -40
.Ltmp90:
	.cfi_offset %r12, -32
.Ltmp91:
	.cfi_offset %r14, -24
.Ltmp92:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %eax
	movq	%rdx, %r15
	movq	%rdi, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rsi, %rbx
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB10_1
# BB#2:                                 # %entry
	movabsq	$dbx64_register_map, %r12
	jmp	.LBB10_3
.LBB10_1:
	movabsq	$svr4_dbx_register_map, %r12
.LBB10_3:                               # %entry
	movq	$32, %rdi
	callq	xmalloc
	movl	(%r12,%rbx,4), %ecx
	movq	%rax, %rsi
	movq	$0, (%rsi)
	cmpq	$63, %rcx
	movq	%rcx, 16(%rsi)
	movq	$5, %rdi
	ja	.LBB10_5
# BB#4:                                 # %entry
	movq	$128, %rdi
.LBB10_5:                               # %entry
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	movq	$-4, %rcx
	subq	%rax, %rcx
	movq	%r15, %rax
	cqto
	idivq	%rcx
	testq	%rax, %rax
	movq	$17, %rcx
	js	.LBB10_7
# BB#6:                                 # %entry
	movq	%rdi, %rcx
.LBB10_7:                               # %entry
	movl	%ecx, 8(%rsi)
	movq	%rax, 24(%rsi)
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	add_fde_cfi  # TAILCALL
.Ltmp93:
	.size	dwarf2out_reg_save, .Ltmp93-dwarf2out_reg_save
	.cfi_endproc

	.globl	dwarf2out_return_save
	.align	16, 0x90
	.type	dwarf2out_return_save,@function
dwarf2out_return_save:                  # @dwarf2out_return_save
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp98:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp99:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 32
.Ltmp101:
	.cfi_offset %rbx, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %ebx
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$22, %rcx
	shrq	%cl, %rbx
	andq	$8, %rbx
	addq	$8, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	%rax, %rsi
	movq	$0, (%rsi)
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rbx, %rax
	movq	%rax, 16(%rsi)
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	movq	$-4, %rcx
	subq	%rax, %rcx
	movq	%r15, %rax
	cqto
	idivq	%rcx
	testq	%rax, %rax
	movq	$17, %rcx
	js	.LBB11_2
# BB#1:                                 # %entry
	movq	$128, %rcx
.LBB11_2:                               # %entry
	movl	%ecx, 8(%rsi)
	movq	%rax, 24(%rsi)
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	add_fde_cfi  # TAILCALL
.Ltmp104:
	.size	dwarf2out_return_save, .Ltmp104-dwarf2out_return_save
	.cfi_endproc

	.globl	dwarf2out_return_reg
	.align	16, 0x90
	.type	dwarf2out_return_reg,@function
dwarf2out_return_reg:                   # @dwarf2out_return_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp113:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 48
.Ltmp116:
	.cfi_offset %rbx, -48
.Ltmp117:
	.cfi_offset %r12, -40
.Ltmp118:
	.cfi_offset %r13, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movl	target_flags(%rip), %ebx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r15, %r13
	andq	%r12, %r13
	movq	$22, %rcx
	shrq	%cl, %rbx
	andq	$8, %rbx
	addq	$8, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rbx, %rcx
	cmpq	%r12, %r13
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	%rcx, 16(%rax)
	je	.LBB12_1
# BB#2:                                 # %if.else12.i
	andq	%r12, %r15
	andq	%r12, %rbx
	cmpq	%r15, %rbx
	jne	.LBB12_3
# BB#5:                                 # %reg_save.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_1:                               # %if.then.i
	movq	$128, %rcx
	movl	%ecx, 8(%rax)
	movq	$0, 24(%rax)
	jmp	.LBB12_4
.LBB12_3:                               # %if.else16.i
	movq	$9, %rcx
	movl	%ecx, 8(%rax)
	movq	%r15, 24(%rax)
.LBB12_4:                               # %if.end22.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_fde_cfi  # TAILCALL
.Ltmp121:
	.size	dwarf2out_return_reg, .Ltmp121-dwarf2out_return_reg
	.cfi_endproc

	.globl	dwarf2out_frame_debug
	.align	16, 0x90
	.type	dwarf2out_frame_debug,@function
dwarf2out_frame_debug:                  # @dwarf2out_frame_debug
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp126:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp127:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp128:
	.cfi_def_cfa_offset 32
.Ltmp129:
	.cfi_offset %rbx, -32
.Ltmp130:
	.cfi_offset %r14, -24
.Ltmp131:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB13_1
# BB#200:                               # %if.end4
	movzbl	(%r15), %eax
	cmpq	$32, %rax
	jne	.LBB13_205
# BB#201:                               # %lor.lhs.false
	movq	queued_reg_saves(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB13_206
# BB#202:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB13_204:                             # %for.body.i36
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%r15, %rsi
	callq	modified_in_p
	testq	%r14, %rax
	jne	.LBB13_205
# BB#203:                               # %for.cond.i
                                        #   in Loop: Header=BB13_204 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_204
	jmp	.LBB13_206
.LBB13_205:                             # %if.then8
	callq	flush_queued_reg_saves
.LBB13_206:                             # %if.end9
	movslq	(%r15), %rax
	testq	%rax, %rax
	js	.LBB13_238
# BB#207:                               # %if.then12
	movzbl	target_flags+1(%rip), %ecx
	testq	$16, %rcx
	jne	.LBB13_243
# BB#208:                               # %if.then15
	movl	flag_asynchronous_unwind_tables(%rip), %ecx
	andq	$65535, %rax            # imm = 0xFFFF
	testq	%rcx, %rcx
	jne	.LBB13_220
# BB#209:                               # %land.lhs.true.i
	cmpq	$34, %rax
	jne	.LBB13_219
# BB#210:                               # %if.then.i38
	movq	32(%r15), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	jne	.LBB13_212
# BB#211:                               # %if.then4.i
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB13_212:                             # %if.end.i
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$47, %rdx
	jne	.LBB13_214
# BB#213:                               # %if.then11.i
	movq	16(%rax), %rax
	movl	(%rax), %ecx
.LBB13_214:                             # %if.end15.i
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$50, %rcx
	jne	.LBB13_215
# BB#216:                               # %if.end20.i
	movq	16(%rax), %rax
	movq	8(%rax), %rbx
	movq	old_args_size(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB13_243
# BB#217:                               # %if.end.i.i
	movq	%rbx, old_args_size(%rip)
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$46, %rcx
	movl	%ecx, 8(%rax)
	movabsq	$.L.str440, %rdi
	jmp	.LBB13_218
.LBB13_1:                               # %if.then
	callq	flush_queued_reg_saves
	movq	$-1, cfa(%rip)
	xorq	%r9, %r9
	movl	%r9d, cfa+24(%rip)
	movq	cie_cfi_head(%rip), %rdx
	movq	$0, cfa+16(%rip)
	movq	$0, cfa+8(%rip)
	testq	%rdx, %rdx
	movq	$-1, %r8
	je	.LBB13_98
# BB#2:
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movabsq	$.L.str226, %rsi
	.align	16, 0x90
.LBB13_3:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_9 Depth 2
	movl	8(%rdx), %eax
	addq	$-12, %rax
	andq	%rdi, %rax
	cmpq	$3, %rax
	ja	.LBB13_97
# BB#4:                                 # %for.body.i
                                        #   in Loop: Header=BB13_3 Depth=1
	jmpq	*.LJTI13_0(,%rax,8)
.LBB13_7:                               # %sw.bb3.i53
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	16(%rdx), %r8
	movq	%r8, cfa(%rip)
	movq	24(%rdx), %rax
	movq	%rax, cfa+8(%rip)
	jmp	.LBB13_97
.LBB13_6:                               # %sw.bb1.i50
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	16(%rdx), %r8
	movq	%r8, cfa(%rip)
	jmp	.LBB13_97
.LBB13_5:                               # %sw.bb.i48
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	16(%rdx), %rax
	movq	%rax, cfa+8(%rip)
	jmp	.LBB13_97
.LBB13_8:                               # %sw.bb9.i57
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	16(%rdx), %rbx
	testq	%rbx, %rbx
	movl	%r9d, cfa+24(%rip)
	movq	$0, cfa+16(%rip)
	movq	$0, cfa+8(%rip)
	movq	$-1, cfa(%rip)
	movq	$-1, %r8
	je	.LBB13_97
	jmp	.LBB13_9
.LBB13_14:                              # %sw.bb20.i.i73
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	$1, %rax
	movl	%eax, cfa+24(%rip)
	jmp	.LBB13_96
.LBB13_15:                              # %sw.bb22.i.i75
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	24(%rbx), %rax
	movq	%rax, cfa+8(%rip)
	jmp	.LBB13_96
.LBB13_11:                              # %sw.bb2.i.i65
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	24(%rbx), %r8
	movq	%r8, cfa(%rip)
	jmp	.LBB13_96
.LBB13_13:                              # %sw.bb12.i.i72
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	24(%rbx), %r8
	movq	%r8, cfa(%rip)
	movq	48(%rbx), %rax
	movq	%rax, cfa+16(%rip)
	jmp	.LBB13_96
	.align	16, 0x90
.LBB13_9:                               # %for.body.i.i60
                                        #   Parent Loop BB13_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	8(%rbx), %eax
	leaq	-3(%rax), %rcx
	andq	%rdi, %rcx
	cmpq	$147, %rcx
	ja	.LBB13_87
# BB#10:                                # %for.body.i.i60
                                        #   in Loop: Header=BB13_9 Depth=2
	jmpq	*.LJTI13_1(,%rcx,8)
.LBB13_94:                              # %sw.bb.i.i63
                                        #   in Loop: Header=BB13_9 Depth=2
	addq	$-80, %rax
	andq	%rdi, %rax
	movq	%rax, cfa(%rip)
	jmp	.LBB13_95
	.align	16, 0x90
.LBB13_12:                              # %sw.bb4.i.i69
                                        #   in Loop: Header=BB13_9 Depth=2
	addq	$-112, %rax
	andq	%rdi, %rax
	movq	%rax, cfa(%rip)
	movq	24(%rbx), %rcx
	movq	%rcx, cfa+16(%rip)
.LBB13_95:                              # %for.inc.i.i157
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	%rax, %r8
.LBB13_96:                              # %for.inc.i.i157
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_9
	.align	16, 0x90
.LBB13_97:                              # %lookup_cfa_1.exit158
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB13_3
.LBB13_98:                              # %for.end.i
	movl	fde_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_194
# BB#99:                                # %if.then.i
	decq	%rax
	movq	fde_table(%rip), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	leaq	(%rax,%rax,4), %rax
	movq	24(%rdx,%rax,8), %rdx
	testq	%rdx, %rdx
	je	.LBB13_194
# BB#100:
	xorq	%r9, %r9
	movabsq	$.L.str226, %rsi
	.align	16, 0x90
.LBB13_101:                             # %for.body4.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_107 Depth 2
	movl	8(%rdx), %eax
	addq	$-12, %rax
	andq	%rcx, %rax
	cmpq	$3, %rax
	ja	.LBB13_193
# BB#102:                               # %for.body4.i
                                        #   in Loop: Header=BB13_101 Depth=1
	jmpq	*.LJTI13_2(,%rax,8)
.LBB13_105:                             # %sw.bb3.i
                                        #   in Loop: Header=BB13_101 Depth=1
	movq	16(%rdx), %r8
	movq	%r8, cfa(%rip)
	movq	24(%rdx), %rax
	movq	%rax, cfa+8(%rip)
	jmp	.LBB13_193
.LBB13_104:                             # %sw.bb1.i
                                        #   in Loop: Header=BB13_101 Depth=1
	movq	16(%rdx), %r8
	movq	%r8, cfa(%rip)
	jmp	.LBB13_193
.LBB13_103:                             # %sw.bb.i
                                        #   in Loop: Header=BB13_101 Depth=1
	movq	16(%rdx), %rax
	movq	%rax, cfa+8(%rip)
	jmp	.LBB13_193
.LBB13_106:                             # %sw.bb9.i
                                        #   in Loop: Header=BB13_101 Depth=1
	movq	16(%rdx), %rbx
	testq	%rbx, %rbx
	movl	%r9d, cfa+24(%rip)
	movq	$0, cfa+16(%rip)
	movq	$0, cfa+8(%rip)
	movq	$-1, cfa(%rip)
	movq	$-1, %r8
	je	.LBB13_193
	jmp	.LBB13_107
.LBB13_112:                             # %sw.bb20.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	movq	$1, %rax
	movl	%eax, cfa+24(%rip)
	jmp	.LBB13_192
.LBB13_113:                             # %sw.bb22.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	movq	24(%rbx), %rax
	movq	%rax, cfa+8(%rip)
	jmp	.LBB13_192
.LBB13_109:                             # %sw.bb2.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	movq	24(%rbx), %r8
	movq	%r8, cfa(%rip)
	jmp	.LBB13_192
.LBB13_111:                             # %sw.bb12.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	movq	24(%rbx), %r8
	movq	%r8, cfa(%rip)
	movq	48(%rbx), %rax
	movq	%rax, cfa+16(%rip)
	jmp	.LBB13_192
	.align	16, 0x90
.LBB13_107:                             # %for.body.i.i
                                        #   Parent Loop BB13_101 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	8(%rbx), %eax
	leaq	-3(%rax), %rdi
	andq	%rcx, %rdi
	cmpq	$147, %rdi
	ja	.LBB13_185
# BB#108:                               # %for.body.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	jmpq	*.LJTI13_3(,%rdi,8)
.LBB13_190:                             # %sw.bb.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	addq	$-80, %rax
	andq	%rcx, %rax
	movq	%rax, cfa(%rip)
	jmp	.LBB13_191
	.align	16, 0x90
.LBB13_110:                             # %sw.bb4.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	addq	$-112, %rax
	andq	%rcx, %rax
	movq	%rax, cfa(%rip)
	movq	24(%rbx), %rdi
	movq	%rdi, cfa+16(%rip)
.LBB13_191:                             # %for.inc.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	movq	%rax, %r8
.LBB13_192:                             # %for.inc.i.i
                                        #   in Loop: Header=BB13_107 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_107
	.align	16, 0x90
.LBB13_193:                             # %lookup_cfa_1.exit
                                        #   in Loop: Header=BB13_101 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB13_101
.LBB13_194:                             # %lookup_cfa.exit
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	movq	$1, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB13_195
# BB#196:                               # %lookup_cfa.exit
	movabsq	$svr4_dbx_register_map+28, %rax
	jmp	.LBB13_197
.LBB13_238:                             # %if.end17
	movq	dwarf2out_cfi_label.label_num(%rip), %rcx
	leaq	1(%rcx), %rax
	movq	%rax, dwarf2out_cfi_label.label_num(%rip)
	movabsq	$dwarf2out_cfi_label.label, %r14
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%r14, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	$21, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB13_239
# BB#240:                               # %if.end17
	addq	$8, %rax
	movq	%rax, %r15
	jmp	.LBB13_241
.LBB13_87:                              # %sw.default.i.i.i150
	movabsq	$.L.str365, %rsi
	jmp	.LBB13_93
.LBB13_239:
	addq	$32, %r15
.LBB13_241:                             # %if.end17
	movq	(%r15), %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	dwarf2out_frame_debug_expr  # TAILCALL
.LBB13_195:
	movabsq	$dbx64_register_map+28, %rax
.LBB13_197:                             # %lookup_cfa.exit
	movslq	(%rax), %rax
	cmpq	%rax, %r8
	jne	.LBB13_198
# BB#242:                               # %if.end
	movq	$7, cfa(%rip)
	movq	cfa+24(%rip), %rax
	movq	%rax, cfa_store+24(%rip)
	movq	cfa+16(%rip), %rax
	movq	%rax, cfa_store+16(%rip)
	movq	cfa+8(%rip), %rax
	movq	%rax, cfa_store+8(%rip)
	movq	$7, cfa_store(%rip)
	movq	$-1, cfa_temp.0(%rip)
	movq	$0, cfa_temp.1(%rip)
.LBB13_243:                             # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB13_185:                             # %sw.default.i.i.i
	movabsq	$.L.str365, %rsi
.LBB13_93:                              # %dwarf_stack_op_name.exit.i.i154
	movabsq	$.L.str497, %rdi
	xorq	%rax, %rax
	callq	internal_error
.LBB13_219:                             # %if.else.i
	movq	cfa(%rip), %rcx
	cmpq	$7, %rcx
	jne	.LBB13_243
.LBB13_220:                             # %if.end31.i
	cmpq	$35, %rax
	jne	.LBB13_222
# BB#221:                               # %if.then35.i
	movq	args_size(%rip), %r14
	negq	%r14
	jmp	.LBB13_226
.LBB13_222:                             # %if.else36.i
	movq	32(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$24, %rax
	je	.LBB13_233
# BB#223:                               # %if.else36.i
	cmpq	$39, %rax
	jne	.LBB13_224
.LBB13_233:                             # %if.then60.i
	movq	8(%rdi), %rax
	movl	(%rax), %ebx
	xorq	%r14, %r14
	jmp	.LBB13_234
.LBB13_237:                             # %if.then80.i
                                        #   in Loop: Header=BB13_234 Depth=1
	callq	stack_adjust_offset
	addq	%rax, %r14
.LBB13_234:                             # %for.cond.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_235 Depth 2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB13_235:                             # %for.cond.i42
                                        #   Parent Loop BB13_234 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_226
# BB#236:                               # %for.body.i43
                                        #   in Loop: Header=BB13_235 Depth=2
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movq	(%rax,%rbx,8), %rdi
	movzbl	(%rdi), %eax
	decq	%rbx
	cmpq	$47, %rax
	jne	.LBB13_235
	jmp	.LBB13_237
.LBB13_224:                             # %if.else36.i
	cmpq	$47, %rax
	jne	.LBB13_243
# BB#225:                               # %if.then43.i
	callq	stack_adjust_offset
	movq	%rax, %r14
.LBB13_226:                             # %if.end95.i
	testq	%r14, %r14
	je	.LBB13_243
# BB#227:                               # %if.end98.i
	movq	cfa(%rip), %rax
	cmpq	$7, %rax
	jne	.LBB13_229
# BB#228:                               # %if.then100.i
	addq	%r14, cfa+8(%rip)
.LBB13_229:                             # %if.end102.i
	movq	args_size(%rip), %rax
	addq	%r14, %rax
	movabsq	$0, %rdx
	js	.LBB13_231
# BB#230:                               # %if.end102.i
	movq	%rax, %rdx
.LBB13_231:                             # %if.end102.i
	movq	dwarf2out_cfi_label.label_num(%rip), %rcx
	movq	%rdx, args_size(%rip)
	leaq	1(%rcx), %rax
	movq	%rax, dwarf2out_cfi_label.label_num(%rip)
	movabsq	$dwarf2out_cfi_label.label, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movabsq	$cfa, %rsi
	movq	%rbx, %rdi
	callq	def_cfa_1
	movq	args_size(%rip), %rbx
	movq	old_args_size(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB13_243
# BB#232:                               # %if.end.i134.i
	movq	%rbx, old_args_size(%rip)
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$46, %rcx
	movl	%ecx, 8(%rax)
	movabsq	$dwarf2out_cfi_label.label, %rdi
.LBB13_218:                             # %if.end.i.i
	movq	%rbx, 16(%rax)
	movq	%rax, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	add_fde_cfi  # TAILCALL
.LBB13_215:                             # %if.then19.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_stack_adjust, %rcx
	movq	$1009, %rsi             # imm = 0x3F1
	jmp	.LBB13_199
.LBB13_88:                              # %dwarf_stack_op_name.exit.loopexit.i.i151
	movabsq	$.L.str220, %rsi
	jmp	.LBB13_93
.LBB13_89:                              # %dwarf_stack_op_name.exit.i.loopexit.i152
	movabsq	$.L.str222, %rsi
	jmp	.LBB13_93
.LBB13_90:                              # %dwarf_stack_op_name.exit.i.i154.loopexit
	movabsq	$.L.str223, %rsi
	jmp	.LBB13_93
.LBB13_91:                              # %dwarf_stack_op_name.exit.i.i154.loopexit162
	movabsq	$.L.str224, %rsi
	jmp	.LBB13_93
.LBB13_16:                              # %sw.bb7.i.i.i79
	movabsq	$.L.str227, %rsi
	jmp	.LBB13_93
.LBB13_17:                              # %sw.bb8.i.i.i80
	movabsq	$.L.str228, %rsi
	jmp	.LBB13_93
.LBB13_18:                              # %sw.bb9.i.i.i81
	movabsq	$.L.str229, %rsi
	jmp	.LBB13_93
.LBB13_19:                              # %sw.bb10.i.i.i82
	movabsq	$.L.str230, %rsi
	jmp	.LBB13_93
.LBB13_20:                              # %sw.bb11.i.i.i83
	movabsq	$.L.str231, %rsi
	jmp	.LBB13_93
.LBB13_21:                              # %sw.bb12.i.i.i84
	movabsq	$.L.str232, %rsi
	jmp	.LBB13_93
.LBB13_22:                              # %sw.bb13.i.i.i85
	movabsq	$.L.str233, %rsi
	jmp	.LBB13_93
.LBB13_23:                              # %sw.bb14.i.i.i86
	movabsq	$.L.str234, %rsi
	jmp	.LBB13_93
.LBB13_24:                              # %sw.bb15.i.i.i87
	movabsq	$.L.str235, %rsi
	jmp	.LBB13_93
.LBB13_25:                              # %sw.bb16.i.i.i88
	movabsq	$.L.str236, %rsi
	jmp	.LBB13_93
.LBB13_26:                              # %sw.bb17.i.i.i89
	movabsq	$.L.str237, %rsi
	jmp	.LBB13_93
.LBB13_27:                              # %sw.bb18.i.i.i90
	movabsq	$.L.str238, %rsi
	jmp	.LBB13_93
.LBB13_28:                              # %sw.bb19.i.i.i91
	movabsq	$.L.str239, %rsi
	jmp	.LBB13_93
.LBB13_29:                              # %sw.bb20.i.i.i92
	movabsq	$.L.str240, %rsi
	jmp	.LBB13_93
.LBB13_30:                              # %sw.bb21.i.i.i93
	movabsq	$.L.str241, %rsi
	jmp	.LBB13_93
.LBB13_31:                              # %sw.bb22.i.i.i94
	movabsq	$.L.str242, %rsi
	jmp	.LBB13_93
.LBB13_32:                              # %sw.bb23.i.i.i95
	movabsq	$.L.str243, %rsi
	jmp	.LBB13_93
.LBB13_33:                              # %sw.bb24.i.i.i96
	movabsq	$.L.str244, %rsi
	jmp	.LBB13_93
.LBB13_34:                              # %sw.bb25.i.i.i97
	movabsq	$.L.str245, %rsi
	jmp	.LBB13_93
.LBB13_35:                              # %sw.bb26.i.i.i98
	movabsq	$.L.str246, %rsi
	jmp	.LBB13_93
.LBB13_36:                              # %sw.bb27.i.i.i99
	movabsq	$.L.str247, %rsi
	jmp	.LBB13_93
.LBB13_37:                              # %sw.bb28.i.i.i100
	movabsq	$.L.str248, %rsi
	jmp	.LBB13_93
.LBB13_38:                              # %sw.bb30.i.i.i101
	movabsq	$.L.str250, %rsi
	jmp	.LBB13_93
.LBB13_39:                              # %sw.bb31.i.i.i102
	movabsq	$.L.str251, %rsi
	jmp	.LBB13_93
.LBB13_40:                              # %sw.bb32.i.i.i103
	movabsq	$.L.str252, %rsi
	jmp	.LBB13_93
.LBB13_41:                              # %sw.bb33.i.i.i104
	movabsq	$.L.str253, %rsi
	jmp	.LBB13_93
.LBB13_42:                              # %sw.bb34.i.i.i105
	movabsq	$.L.str254, %rsi
	jmp	.LBB13_93
.LBB13_43:                              # %sw.bb35.i.i.i106
	movabsq	$.L.str255, %rsi
	jmp	.LBB13_93
.LBB13_44:                              # %sw.bb36.i.i.i107
	movabsq	$.L.str256, %rsi
	jmp	.LBB13_93
.LBB13_45:                              # %sw.bb37.i.i.i108
	movabsq	$.L.str257, %rsi
	jmp	.LBB13_93
.LBB13_46:                              # %sw.bb38.i.i.i109
	movabsq	$.L.str258, %rsi
	jmp	.LBB13_93
.LBB13_47:                              # %sw.bb39.i.i.i110
	movabsq	$.L.str259, %rsi
	jmp	.LBB13_93
.LBB13_48:                              # %sw.bb40.i.i.i111
	movabsq	$.L.str260, %rsi
	jmp	.LBB13_93
.LBB13_49:                              # %sw.bb41.i.i.i112
	movabsq	$.L.str261, %rsi
	jmp	.LBB13_93
.LBB13_50:                              # %sw.bb42.i.i.i113
	movabsq	$.L.str262, %rsi
	jmp	.LBB13_93
.LBB13_51:                              # %sw.bb43.i.i.i114
	movabsq	$.L.str263, %rsi
	jmp	.LBB13_93
.LBB13_52:                              # %sw.bb44.i.i.i115
	movabsq	$.L.str264, %rsi
	jmp	.LBB13_93
.LBB13_53:                              # %sw.bb45.i.i.i116
	movabsq	$.L.str265, %rsi
	jmp	.LBB13_93
.LBB13_54:                              # %sw.bb46.i.i.i117
	movabsq	$.L.str266, %rsi
	jmp	.LBB13_93
.LBB13_55:                              # %sw.bb47.i.i.i118
	movabsq	$.L.str267, %rsi
	jmp	.LBB13_93
.LBB13_56:                              # %sw.bb48.i.i.i119
	movabsq	$.L.str268, %rsi
	jmp	.LBB13_93
.LBB13_57:                              # %sw.bb49.i.i.i120
	movabsq	$.L.str269, %rsi
	jmp	.LBB13_93
.LBB13_58:                              # %sw.bb50.i.i.i121
	movabsq	$.L.str270, %rsi
	jmp	.LBB13_93
.LBB13_59:                              # %sw.bb51.i.i.i122
	movabsq	$.L.str271, %rsi
	jmp	.LBB13_93
.LBB13_60:                              # %sw.bb52.i.i.i123
	movabsq	$.L.str272, %rsi
	jmp	.LBB13_93
.LBB13_61:                              # %sw.bb53.i.i.i124
	movabsq	$.L.str273, %rsi
	jmp	.LBB13_93
.LBB13_62:                              # %sw.bb54.i.i.i125
	movabsq	$.L.str274, %rsi
	jmp	.LBB13_93
.LBB13_63:                              # %sw.bb55.i.i.i126
	movabsq	$.L.str275, %rsi
	jmp	.LBB13_93
.LBB13_64:                              # %sw.bb56.i.i.i127
	movabsq	$.L.str276, %rsi
	jmp	.LBB13_93
.LBB13_65:                              # %sw.bb57.i.i.i128
	movabsq	$.L.str277, %rsi
	jmp	.LBB13_93
.LBB13_66:                              # %sw.bb58.i.i.i129
	movabsq	$.L.str278, %rsi
	jmp	.LBB13_93
.LBB13_67:                              # %sw.bb59.i.i.i130
	movabsq	$.L.str279, %rsi
	jmp	.LBB13_93
.LBB13_68:                              # %sw.bb60.i.i.i131
	movabsq	$.L.str280, %rsi
	jmp	.LBB13_93
.LBB13_69:                              # %sw.bb61.i.i.i132
	movabsq	$.L.str281, %rsi
	jmp	.LBB13_93
.LBB13_70:                              # %sw.bb62.i.i.i133
	movabsq	$.L.str282, %rsi
	jmp	.LBB13_93
.LBB13_71:                              # %sw.bb63.i.i.i134
	movabsq	$.L.str283, %rsi
	jmp	.LBB13_93
.LBB13_72:                              # %sw.bb64.i.i.i135
	movabsq	$.L.str284, %rsi
	jmp	.LBB13_93
.LBB13_73:                              # %sw.bb65.i.i.i136
	movabsq	$.L.str285, %rsi
	jmp	.LBB13_93
.LBB13_74:                              # %sw.bb66.i.i.i137
	movabsq	$.L.str286, %rsi
	jmp	.LBB13_93
.LBB13_75:                              # %sw.bb67.i.i.i138
	movabsq	$.L.str287, %rsi
	jmp	.LBB13_93
.LBB13_76:                              # %sw.bb68.i.i.i139
	movabsq	$.L.str288, %rsi
	jmp	.LBB13_93
.LBB13_77:                              # %sw.bb69.i.i.i140
	movabsq	$.L.str289, %rsi
	jmp	.LBB13_93
.LBB13_78:                              # %sw.bb70.i.i.i141
	movabsq	$.L.str290, %rsi
	jmp	.LBB13_93
.LBB13_79:                              # %sw.bb71.i.i.i142
	movabsq	$.L.str291, %rsi
	jmp	.LBB13_93
.LBB13_80:                              # %sw.bb72.i.i.i143
	movabsq	$.L.str292, %rsi
	jmp	.LBB13_93
.LBB13_81:                              # %sw.bb73.i.i.i144
	movabsq	$.L.str293, %rsi
	jmp	.LBB13_93
.LBB13_82:                              # %sw.bb139.i.i.i145
	movabsq	$.L.str359, %rsi
	jmp	.LBB13_93
.LBB13_83:                              # %sw.bb141.i.i.i146
	movabsq	$.L.str361, %rsi
	jmp	.LBB13_93
.LBB13_84:                              # %sw.bb142.i.i.i147
	movabsq	$.L.str362, %rsi
	jmp	.LBB13_93
.LBB13_85:                              # %sw.bb143.i.i.i148
	movabsq	$.L.str363, %rsi
	jmp	.LBB13_93
.LBB13_86:                              # %sw.bb144.i.i.i149
	movabsq	$.L.str364, %rsi
	jmp	.LBB13_93
.LBB13_186:                             # %dwarf_stack_op_name.exit.loopexit.i.i
	movabsq	$.L.str220, %rsi
	jmp	.LBB13_93
.LBB13_187:                             # %dwarf_stack_op_name.exit.i.loopexit.i
	movabsq	$.L.str222, %rsi
	jmp	.LBB13_93
.LBB13_188:                             # %dwarf_stack_op_name.exit.i.i.loopexit
	movabsq	$.L.str223, %rsi
	jmp	.LBB13_93
.LBB13_189:                             # %dwarf_stack_op_name.exit.i.i.loopexit161
	movabsq	$.L.str224, %rsi
	jmp	.LBB13_93
.LBB13_92:                              # %dwarf_stack_op_name.exit.i.i154.loopexit2
	movabsq	$.L.str225, %rsi
	jmp	.LBB13_93
.LBB13_114:                             # %sw.bb7.i.i.i
	movabsq	$.L.str227, %rsi
	jmp	.LBB13_93
.LBB13_115:                             # %sw.bb8.i.i.i
	movabsq	$.L.str228, %rsi
	jmp	.LBB13_93
.LBB13_116:                             # %sw.bb9.i.i.i
	movabsq	$.L.str229, %rsi
	jmp	.LBB13_93
.LBB13_117:                             # %sw.bb10.i.i.i
	movabsq	$.L.str230, %rsi
	jmp	.LBB13_93
.LBB13_118:                             # %sw.bb11.i.i.i
	movabsq	$.L.str231, %rsi
	jmp	.LBB13_93
.LBB13_119:                             # %sw.bb12.i.i.i
	movabsq	$.L.str232, %rsi
	jmp	.LBB13_93
.LBB13_120:                             # %sw.bb13.i.i.i
	movabsq	$.L.str233, %rsi
	jmp	.LBB13_93
.LBB13_121:                             # %sw.bb14.i.i.i
	movabsq	$.L.str234, %rsi
	jmp	.LBB13_93
.LBB13_122:                             # %sw.bb15.i.i.i
	movabsq	$.L.str235, %rsi
	jmp	.LBB13_93
.LBB13_123:                             # %sw.bb16.i.i.i
	movabsq	$.L.str236, %rsi
	jmp	.LBB13_93
.LBB13_124:                             # %sw.bb17.i.i.i
	movabsq	$.L.str237, %rsi
	jmp	.LBB13_93
.LBB13_125:                             # %sw.bb18.i.i.i
	movabsq	$.L.str238, %rsi
	jmp	.LBB13_93
.LBB13_126:                             # %sw.bb19.i.i.i
	movabsq	$.L.str239, %rsi
	jmp	.LBB13_93
.LBB13_127:                             # %sw.bb20.i.i.i
	movabsq	$.L.str240, %rsi
	jmp	.LBB13_93
.LBB13_128:                             # %sw.bb21.i.i.i
	movabsq	$.L.str241, %rsi
	jmp	.LBB13_93
.LBB13_129:                             # %sw.bb22.i.i.i
	movabsq	$.L.str242, %rsi
	jmp	.LBB13_93
.LBB13_130:                             # %sw.bb23.i.i.i
	movabsq	$.L.str243, %rsi
	jmp	.LBB13_93
.LBB13_131:                             # %sw.bb24.i.i.i
	movabsq	$.L.str244, %rsi
	jmp	.LBB13_93
.LBB13_132:                             # %sw.bb25.i.i.i
	movabsq	$.L.str245, %rsi
	jmp	.LBB13_93
.LBB13_133:                             # %sw.bb26.i.i.i
	movabsq	$.L.str246, %rsi
	jmp	.LBB13_93
.LBB13_134:                             # %sw.bb27.i.i.i
	movabsq	$.L.str247, %rsi
	jmp	.LBB13_93
.LBB13_135:                             # %sw.bb28.i.i.i
	movabsq	$.L.str248, %rsi
	jmp	.LBB13_93
.LBB13_136:                             # %sw.bb30.i.i.i
	movabsq	$.L.str250, %rsi
	jmp	.LBB13_93
.LBB13_137:                             # %sw.bb31.i.i.i
	movabsq	$.L.str251, %rsi
	jmp	.LBB13_93
.LBB13_138:                             # %sw.bb32.i.i.i
	movabsq	$.L.str252, %rsi
	jmp	.LBB13_93
.LBB13_139:                             # %sw.bb33.i.i.i
	movabsq	$.L.str253, %rsi
	jmp	.LBB13_93
.LBB13_140:                             # %sw.bb34.i.i.i
	movabsq	$.L.str254, %rsi
	jmp	.LBB13_93
.LBB13_141:                             # %sw.bb35.i.i.i
	movabsq	$.L.str255, %rsi
	jmp	.LBB13_93
.LBB13_142:                             # %sw.bb36.i.i.i
	movabsq	$.L.str256, %rsi
	jmp	.LBB13_93
.LBB13_143:                             # %sw.bb37.i.i.i
	movabsq	$.L.str257, %rsi
	jmp	.LBB13_93
.LBB13_144:                             # %sw.bb38.i.i.i
	movabsq	$.L.str258, %rsi
	jmp	.LBB13_93
.LBB13_145:                             # %sw.bb39.i.i.i
	movabsq	$.L.str259, %rsi
	jmp	.LBB13_93
.LBB13_146:                             # %sw.bb40.i.i.i
	movabsq	$.L.str260, %rsi
	jmp	.LBB13_93
.LBB13_147:                             # %sw.bb41.i.i.i
	movabsq	$.L.str261, %rsi
	jmp	.LBB13_93
.LBB13_148:                             # %sw.bb42.i.i.i
	movabsq	$.L.str262, %rsi
	jmp	.LBB13_93
.LBB13_149:                             # %sw.bb43.i.i.i
	movabsq	$.L.str263, %rsi
	jmp	.LBB13_93
.LBB13_150:                             # %sw.bb44.i.i.i
	movabsq	$.L.str264, %rsi
	jmp	.LBB13_93
.LBB13_151:                             # %sw.bb45.i.i.i
	movabsq	$.L.str265, %rsi
	jmp	.LBB13_93
.LBB13_152:                             # %sw.bb46.i.i.i
	movabsq	$.L.str266, %rsi
	jmp	.LBB13_93
.LBB13_153:                             # %sw.bb47.i.i.i
	movabsq	$.L.str267, %rsi
	jmp	.LBB13_93
.LBB13_154:                             # %sw.bb48.i.i.i
	movabsq	$.L.str268, %rsi
	jmp	.LBB13_93
.LBB13_155:                             # %sw.bb49.i.i.i
	movabsq	$.L.str269, %rsi
	jmp	.LBB13_93
.LBB13_156:                             # %sw.bb50.i.i.i
	movabsq	$.L.str270, %rsi
	jmp	.LBB13_93
.LBB13_157:                             # %sw.bb51.i.i.i
	movabsq	$.L.str271, %rsi
	jmp	.LBB13_93
.LBB13_158:                             # %sw.bb52.i.i.i
	movabsq	$.L.str272, %rsi
	jmp	.LBB13_93
.LBB13_159:                             # %sw.bb53.i.i.i
	movabsq	$.L.str273, %rsi
	jmp	.LBB13_93
.LBB13_160:                             # %sw.bb54.i.i.i
	movabsq	$.L.str274, %rsi
	jmp	.LBB13_93
.LBB13_161:                             # %sw.bb55.i.i.i
	movabsq	$.L.str275, %rsi
	jmp	.LBB13_93
.LBB13_162:                             # %sw.bb56.i.i.i
	movabsq	$.L.str276, %rsi
	jmp	.LBB13_93
.LBB13_163:                             # %sw.bb57.i.i.i
	movabsq	$.L.str277, %rsi
	jmp	.LBB13_93
.LBB13_164:                             # %sw.bb58.i.i.i
	movabsq	$.L.str278, %rsi
	jmp	.LBB13_93
.LBB13_165:                             # %sw.bb59.i.i.i
	movabsq	$.L.str279, %rsi
	jmp	.LBB13_93
.LBB13_166:                             # %sw.bb60.i.i.i
	movabsq	$.L.str280, %rsi
	jmp	.LBB13_93
.LBB13_167:                             # %sw.bb61.i.i.i
	movabsq	$.L.str281, %rsi
	jmp	.LBB13_93
.LBB13_168:                             # %sw.bb62.i.i.i
	movabsq	$.L.str282, %rsi
	jmp	.LBB13_93
.LBB13_169:                             # %sw.bb63.i.i.i
	movabsq	$.L.str283, %rsi
	jmp	.LBB13_93
.LBB13_170:                             # %sw.bb64.i.i.i
	movabsq	$.L.str284, %rsi
	jmp	.LBB13_93
.LBB13_171:                             # %sw.bb65.i.i.i
	movabsq	$.L.str285, %rsi
	jmp	.LBB13_93
.LBB13_172:                             # %sw.bb66.i.i.i
	movabsq	$.L.str286, %rsi
	jmp	.LBB13_93
.LBB13_173:                             # %sw.bb67.i.i.i
	movabsq	$.L.str287, %rsi
	jmp	.LBB13_93
.LBB13_174:                             # %sw.bb68.i.i.i
	movabsq	$.L.str288, %rsi
	jmp	.LBB13_93
.LBB13_175:                             # %sw.bb69.i.i.i
	movabsq	$.L.str289, %rsi
	jmp	.LBB13_93
.LBB13_176:                             # %sw.bb70.i.i.i
	movabsq	$.L.str290, %rsi
	jmp	.LBB13_93
.LBB13_177:                             # %sw.bb71.i.i.i
	movabsq	$.L.str291, %rsi
	jmp	.LBB13_93
.LBB13_178:                             # %sw.bb72.i.i.i
	movabsq	$.L.str292, %rsi
	jmp	.LBB13_93
.LBB13_179:                             # %sw.bb73.i.i.i
	movabsq	$.L.str293, %rsi
	jmp	.LBB13_93
.LBB13_180:                             # %sw.bb139.i.i.i
	movabsq	$.L.str359, %rsi
	jmp	.LBB13_93
.LBB13_181:                             # %sw.bb141.i.i.i
	movabsq	$.L.str361, %rsi
	jmp	.LBB13_93
.LBB13_182:                             # %sw.bb142.i.i.i
	movabsq	$.L.str362, %rsi
	jmp	.LBB13_93
.LBB13_183:                             # %sw.bb143.i.i.i
	movabsq	$.L.str363, %rsi
	jmp	.LBB13_93
.LBB13_184:                             # %sw.bb144.i.i.i
	movabsq	$.L.str364, %rsi
	jmp	.LBB13_93
.LBB13_198:                             # %if.then3
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug, %rcx
	movq	$1626, %rsi             # imm = 0x65A
.LBB13_199:                             # %if.then3
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp132:
	.size	dwarf2out_frame_debug, .Ltmp132-dwarf2out_frame_debug
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_7
	.quad	.LBB13_6
	.quad	.LBB13_5
	.quad	.LBB13_8
.LJTI13_1:
	.quad	.LBB13_88
	.quad	.LBB13_87
	.quad	.LBB13_87
	.quad	.LBB13_14
	.quad	.LBB13_87
	.quad	.LBB13_89
	.quad	.LBB13_90
	.quad	.LBB13_91
	.quad	.LBB13_92
	.quad	.LBB13_93
	.quad	.LBB13_16
	.quad	.LBB13_17
	.quad	.LBB13_18
	.quad	.LBB13_19
	.quad	.LBB13_20
	.quad	.LBB13_21
	.quad	.LBB13_22
	.quad	.LBB13_23
	.quad	.LBB13_24
	.quad	.LBB13_25
	.quad	.LBB13_26
	.quad	.LBB13_27
	.quad	.LBB13_28
	.quad	.LBB13_29
	.quad	.LBB13_30
	.quad	.LBB13_31
	.quad	.LBB13_32
	.quad	.LBB13_33
	.quad	.LBB13_34
	.quad	.LBB13_35
	.quad	.LBB13_36
	.quad	.LBB13_37
	.quad	.LBB13_15
	.quad	.LBB13_38
	.quad	.LBB13_39
	.quad	.LBB13_40
	.quad	.LBB13_41
	.quad	.LBB13_42
	.quad	.LBB13_43
	.quad	.LBB13_44
	.quad	.LBB13_45
	.quad	.LBB13_46
	.quad	.LBB13_47
	.quad	.LBB13_48
	.quad	.LBB13_49
	.quad	.LBB13_50
	.quad	.LBB13_51
	.quad	.LBB13_52
	.quad	.LBB13_53
	.quad	.LBB13_54
	.quad	.LBB13_55
	.quad	.LBB13_56
	.quad	.LBB13_57
	.quad	.LBB13_58
	.quad	.LBB13_59
	.quad	.LBB13_60
	.quad	.LBB13_61
	.quad	.LBB13_62
	.quad	.LBB13_63
	.quad	.LBB13_64
	.quad	.LBB13_65
	.quad	.LBB13_66
	.quad	.LBB13_67
	.quad	.LBB13_68
	.quad	.LBB13_69
	.quad	.LBB13_70
	.quad	.LBB13_71
	.quad	.LBB13_72
	.quad	.LBB13_73
	.quad	.LBB13_74
	.quad	.LBB13_75
	.quad	.LBB13_76
	.quad	.LBB13_77
	.quad	.LBB13_78
	.quad	.LBB13_79
	.quad	.LBB13_80
	.quad	.LBB13_81
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_94
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_11
	.quad	.LBB13_82
	.quad	.LBB13_13
	.quad	.LBB13_83
	.quad	.LBB13_84
	.quad	.LBB13_85
	.quad	.LBB13_86
.LJTI13_2:
	.quad	.LBB13_105
	.quad	.LBB13_104
	.quad	.LBB13_103
	.quad	.LBB13_106
.LJTI13_3:
	.quad	.LBB13_186
	.quad	.LBB13_185
	.quad	.LBB13_185
	.quad	.LBB13_112
	.quad	.LBB13_185
	.quad	.LBB13_187
	.quad	.LBB13_188
	.quad	.LBB13_189
	.quad	.LBB13_92
	.quad	.LBB13_93
	.quad	.LBB13_114
	.quad	.LBB13_115
	.quad	.LBB13_116
	.quad	.LBB13_117
	.quad	.LBB13_118
	.quad	.LBB13_119
	.quad	.LBB13_120
	.quad	.LBB13_121
	.quad	.LBB13_122
	.quad	.LBB13_123
	.quad	.LBB13_124
	.quad	.LBB13_125
	.quad	.LBB13_126
	.quad	.LBB13_127
	.quad	.LBB13_128
	.quad	.LBB13_129
	.quad	.LBB13_130
	.quad	.LBB13_131
	.quad	.LBB13_132
	.quad	.LBB13_133
	.quad	.LBB13_134
	.quad	.LBB13_135
	.quad	.LBB13_113
	.quad	.LBB13_136
	.quad	.LBB13_137
	.quad	.LBB13_138
	.quad	.LBB13_139
	.quad	.LBB13_140
	.quad	.LBB13_141
	.quad	.LBB13_142
	.quad	.LBB13_143
	.quad	.LBB13_144
	.quad	.LBB13_145
	.quad	.LBB13_146
	.quad	.LBB13_147
	.quad	.LBB13_148
	.quad	.LBB13_149
	.quad	.LBB13_150
	.quad	.LBB13_151
	.quad	.LBB13_152
	.quad	.LBB13_153
	.quad	.LBB13_154
	.quad	.LBB13_155
	.quad	.LBB13_156
	.quad	.LBB13_157
	.quad	.LBB13_158
	.quad	.LBB13_159
	.quad	.LBB13_160
	.quad	.LBB13_161
	.quad	.LBB13_162
	.quad	.LBB13_163
	.quad	.LBB13_164
	.quad	.LBB13_165
	.quad	.LBB13_166
	.quad	.LBB13_167
	.quad	.LBB13_168
	.quad	.LBB13_169
	.quad	.LBB13_170
	.quad	.LBB13_171
	.quad	.LBB13_172
	.quad	.LBB13_173
	.quad	.LBB13_174
	.quad	.LBB13_175
	.quad	.LBB13_176
	.quad	.LBB13_177
	.quad	.LBB13_178
	.quad	.LBB13_179
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_190
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_110
	.quad	.LBB13_109
	.quad	.LBB13_180
	.quad	.LBB13_111
	.quad	.LBB13_181
	.quad	.LBB13_182
	.quad	.LBB13_183
	.quad	.LBB13_184

	.text
	.align	16, 0x90
	.type	flush_queued_reg_saves,@function
flush_queued_reg_saves:                 # @flush_queued_reg_saves
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp139:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp140:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp141:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp142:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp143:
	.cfi_def_cfa_offset 48
.Ltmp144:
	.cfi_offset %rbx, -48
.Ltmp145:
	.cfi_offset %r12, -40
.Ltmp146:
	.cfi_offset %r13, -32
.Ltmp147:
	.cfi_offset %r14, -24
.Ltmp148:
	.cfi_offset %r15, -16
	movq	queued_reg_saves(%rip), %r13
	testq	%r13, %r13
	je	.LBB14_8
	.align	16, 0x90
.LBB14_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r13), %rax
	movq	last_reg_save_label(%rip), %r14
	movq	16(%r13), %r15
	movl	target_flags(%rip), %ecx
	movl	8(%rax), %ebx
	testq	$33554432, %rcx         # imm = 0x2000000
	movabsq	$svr4_dbx_register_map, %r12
	je	.LBB14_3
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movabsq	$dbx64_register_map, %r12
.LBB14_3:                               # %for.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	$32, %rdi
	callq	xmalloc
	movl	(%r12,%rbx,4), %ecx
	movq	%rax, %rsi
	movq	$0, (%rsi)
	cmpq	$63, %rcx
	movq	%rcx, 16(%rsi)
	movq	$5, %rdi
	ja	.LBB14_5
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	$128, %rdi
.LBB14_5:                               # %for.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	movq	$-4, %rcx
	subq	%rax, %rcx
	movq	%r15, %rax
	cqto
	idivq	%rcx
	testq	%rax, %rax
	movq	$17, %rcx
	js	.LBB14_7
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	%rdi, %rcx
.LBB14_7:                               # %for.body
                                        #   in Loop: Header=BB14_1 Depth=1
	movl	%ecx, 8(%rsi)
	movq	%rax, 24(%rsi)
	movq	%r14, %rdi
	callq	add_fde_cfi
	movq	(%r13), %rbx
	movq	%r13, %rdi
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %r13
	jne	.LBB14_1
.LBB14_8:                               # %for.end
	movq	$0, queued_reg_saves(%rip)
	movq	$0, last_reg_save_label(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp149:
	.size	flush_queued_reg_saves, .Ltmp149-flush_queued_reg_saves
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_frame_debug_expr,@function
dwarf2out_frame_debug_expr:             # @dwarf2out_frame_debug_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp155:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp156:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp157:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp158:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp159:
	.cfi_def_cfa_offset 48
.Ltmp160:
	.cfi_offset %rbx, -40
.Ltmp161:
	.cfi_offset %r12, -32
.Ltmp162:
	.cfi_offset %r14, -24
.Ltmp163:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	cmpq	$24, %rax
	je	.LBB15_3
# BB#1:                                 # %entry
	cmpq	$47, %rax
	jne	.LBB15_2
# BB#13:                                # %if.end33
	movq	8(%r14), %rax
	movl	(%rax), %edx
	movq	16(%r14), %r14
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB15_14
# BB#85:                                # %sw.bb336
	movzbl	(%r14), %ecx
	cmpq	$61, %rcx
	jne	.LBB15_86
# BB#87:                                # %if.end342
	movq	8(%rax), %rax
	movzbl	(%rax), %esi
	cmpq	$134, %rsi
	jg	.LBB15_112
# BB#88:                                # %if.end342
	cmpq	$95, %rsi
	jg	.LBB15_93
# BB#89:                                # %if.end342
	leaq	-75(%rsi), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB15_113
# BB#90:                                # %if.end342
	cmpq	$61, %rsi
	jne	.LBB15_127
# BB#91:                                # %sw.bb502
	movq	cfa_store(%rip), %rdx
	movl	8(%rax), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB15_122
# BB#92:                                # %if.then512
	movq	cfa_store+8(%rip), %rbx
	negq	%rbx
	jmp	.LBB15_129
.LBB15_2:                               # %entry
	cmpq	$39, %rax
	jne	.LBB15_11
.LBB15_3:                               # %if.then
	movq	8(%r14), %rax
	movslq	(%rax), %r15
	testq	%r15, %r15
	jle	.LBB15_140
# BB#4:
	movq	$1, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r15
	jmp	.LBB15_5
	.align	16, 0x90
.LBB15_10:                              # %for.inc.for.body_crit_edge
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	8(%r14), %rax
	incq	%rbx
.LBB15_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rdi
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB15_9
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_8
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_5 Depth=1
	leaq	-1(%rbx), %rax
	testq	%r12, %rax
	jne	.LBB15_9
.LBB15_8:                               # %if.then21
                                        #   in Loop: Header=BB15_5 Depth=1
	callq	dwarf2out_frame_debug_expr
.LBB15_9:                               # %for.inc
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	%r15, %rax
	jne	.LBB15_10
	jmp	.LBB15_140
.LBB15_14:                              # %if.end33
	cmpq	$61, %rcx
	jne	.LBB15_141
# BB#15:                                # %sw.bb
	movzbl	(%r14), %ecx
	cmpq	$133, %rcx
	jg	.LBB15_22
# BB#16:                                # %sw.bb
	cmpq	$83, %rcx
	jg	.LBB15_73
# BB#17:                                # %sw.bb
	leaq	-75(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$2, %rsi
	jb	.LBB15_24
# BB#18:                                # %sw.bb
	cmpq	$54, %rcx
	jne	.LBB15_19
# BB#72:                                # %sw.bb280
	movl	8(%rax), %eax
	movq	%rax, cfa_temp.0(%rip)
	movq	8(%r14), %rax
	movq	%rax, cfa_temp.1(%rip)
	jmp	.LBB15_83
.LBB15_112:                             # %if.end342
	cmpq	$135, %rsi
	jne	.LBB15_127
.LBB15_113:                             # %sw.bb438
	movq	16(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB15_114
# BB#115:                               # %if.end450
	movq	8(%rcx), %rbx
	cmpq	$76, %rsi
	jne	.LBB15_117
# BB#116:
	negq	%rbx
.LBB15_117:                             # %if.end450
	movq	8(%rax), %rcx
	movq	cfa_store(%rip), %rdx
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB15_119
# BB#118:                               # %if.then482
	movq	cfa_store+8(%rip), %rcx
	subq	%rcx, %rbx
	jmp	.LBB15_129
.LBB15_22:                              # %sw.bb
	cmpq	$134, %rcx
	je	.LBB15_83
# BB#23:                                # %sw.bb
	cmpq	$135, %rcx
	jne	.LBB15_81
.LBB15_24:                              # %sw.bb54
	movq	global_rtl+16(%rip), %rdx
	cmpq	%rdx, %rax
	je	.LBB15_25
# BB#46:                                # %if.else122
	movq	global_rtl+32(%rip), %rsi
	cmpq	%rsi, %rax
	je	.LBB15_47
# BB#56:                                # %if.else171
	cmpq	$76, %rcx
	je	.LBB15_57
# BB#58:                                # %if.end177
	movq	8(%r14), %rsi
	movzbl	(%rsi), %edi
	cmpq	$61, %rdi
	jne	.LBB15_68
# BB#59:                                # %land.lhs.true185
	movl	8(%rsi), %r8d
	movq	cfa(%rip), %rbx
	cmpq	%rbx, %r8
	jne	.LBB15_63
# BB#60:                                # %land.lhs.true195
	movq	16(%r14), %r8
	movzbl	(%r8), %ebx
	cmpq	$54, %rbx
	jne	.LBB15_62
# BB#61:                                # %if.then203
	movq	8(%r8), %rcx
	movq	cfa+8(%rip), %rdx
	subq	%rcx, %rdx
	movq	%rdx, cfa+8(%rip)
	movl	8(%rax), %eax
	movq	%rax, cfa(%rip)
	movq	%rax, cfa_temp.0(%rip)
	movq	%rdx, cfa_temp.1(%rip)
	jmp	.LBB15_83
.LBB15_119:                             # %if.else484
	movq	cfa_temp.0(%rip), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB15_121
# BB#120:                               # %if.then497
	movq	cfa_temp.1(%rip), %rcx
	subq	%rcx, %rbx
	jmp	.LBB15_129
.LBB15_93:                              # %if.end342
	leaq	-96(%rsi), %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$2, %rdi
	jae	.LBB15_94
# BB#104:                               # %sw.bb399
	movq	8(%rax), %rcx
	movl	8(%rcx), %edi
	movq	$16, %rcx
	shrq	%cl, %rdx
	cmpq	$7, %rdi
	jne	.LBB15_106
# BB#105:                               # %sw.bb399
	movq	cfa_store(%rip), %rcx
	cmpq	$7, %rcx
	jne	.LBB15_106
# BB#107:                               # %if.end431
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	$97, %rsi
	jne	.LBB15_109
# BB#108:
	negq	%rcx
.LBB15_109:                             # %if.end431
	movq	cfa_store+8(%rip), %rbx
	movq	cfa(%rip), %rdx
	addq	%rcx, %rbx
	movq	%rbx, cfa_store+8(%rip)
	cmpq	$7, %rdx
	jne	.LBB15_111
# BB#110:                               # %if.then435
	movq	%rbx, cfa+8(%rip)
.LBB15_111:                             # %if.end436
	negq	%rbx
	jmp	.LBB15_129
.LBB15_73:                              # %sw.bb
	cmpq	$84, %rcx
	jne	.LBB15_81
# BB#74:                                # %sw.bb288
	movq	8(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB15_77
# BB#75:                                # %lor.lhs.false296
	movl	8(%rcx), %ecx
	movq	cfa_temp.0(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB15_77
# BB#76:                                # %lor.lhs.false306
	movq	16(%r14), %rdx
	movzbl	(%rdx), %esi
	cmpq	$54, %rsi
	jne	.LBB15_77
# BB#78:                                # %if.end315
	movl	8(%rax), %eax
	cmpq	%rcx, %rax
	je	.LBB15_80
# BB#79:                                # %if.then322
	movq	%rax, cfa_temp.0(%rip)
.LBB15_80:                              # %if.end327
	movq	8(%rdx), %rax
	orq	%rax, cfa_temp.1(%rip)
	jmp	.LBB15_83
.LBB15_25:                              # %if.then57
	movq	16(%r14), %rdx
	movzbl	(%rdx), %esi
	cmpq	$61, %rsi
	jne	.LBB15_26
# BB#31:                                # %sw.bb69
	movl	8(%rdx), %edx
	movq	cfa_temp.0(%rip), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB15_33
# BB#32:
	movabsq	$cfa_temp.1, %rdx
	jmp	.LBB15_28
.LBB15_94:                              # %if.end342
	cmpq	$99, %rsi
	jne	.LBB15_95
# BB#125:                               # %sw.bb529
	movq	8(%rax), %rcx
	movq	cfa_temp.0(%rip), %rsi
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB15_126
# BB#128:                               # %if.end543
	movq	cfa_temp.1(%rip), %rsi
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	movq	%rsi, %rbx
	negq	%rbx
	subq	%rcx, %rsi
	movq	%rsi, cfa_temp.1(%rip)
	jmp	.LBB15_129
.LBB15_47:                              # %if.then125
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_48
# BB#49:                                # %if.end128
	movq	8(%r14), %rax
	movzbl	(%rax), %edx
	cmpq	$61, %rdx
	jne	.LBB15_55
# BB#50:                                # %land.lhs.true136
	movl	8(%rax), %eax
	movq	cfa(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB15_55
# BB#51:                                # %land.lhs.true146
	movq	16(%r14), %rax
	movzbl	(%rax), %edx
	cmpq	$54, %rdx
	jne	.LBB15_55
# BB#52:                                # %if.then154
	movq	8(%rax), %rax
	cmpq	$76, %rcx
	je	.LBB15_54
# BB#53:                                # %if.then154
	negq	%rax
.LBB15_54:                              # %if.then154
	addq	%rax, cfa+8(%rip)
	movq	$6, cfa(%rip)
	jmp	.LBB15_83
.LBB15_26:                              # %if.then57
	cmpq	$54, %rsi
	jne	.LBB15_34
# BB#27:                                # %sw.bb63
	addq	$8, %rdx
.LBB15_28:                              # %sw.epilog
	movq	8(%r14), %rsi
	movq	global_rtl+32(%rip), %rdi
	movq	(%rdx), %rdx
	cmpq	%rdi, %rsi
	je	.LBB15_29
# BB#37:                                # %if.else91
	cmpq	$135, %rcx
	je	.LBB15_40
# BB#38:                                # %if.else91
	cmpq	%rax, %rsi
	jne	.LBB15_39
.LBB15_40:                              # %if.end106
	movq	cfa(%rip), %rax
	cmpq	$76, %rcx
	je	.LBB15_42
# BB#41:                                # %if.end106
	negq	%rdx
.LBB15_42:                              # %if.end106
	cmpq	$7, %rax
	jne	.LBB15_44
	jmp	.LBB15_43
.LBB15_19:                              # %sw.bb
	cmpq	$61, %rcx
	jne	.LBB15_81
# BB#20:                                # %sw.bb43
	movq	cfa(%rip), %rcx
	movl	8(%r14), %edx
	cmpq	%rdx, %rcx
	jne	.LBB15_21
# BB#82:                                # %if.end49
	movl	8(%rax), %eax
	movq	%rax, cfa(%rip)
	movq	cfa+8(%rip), %rcx
	movq	%rax, cfa_temp.0(%rip)
	movq	%rcx, cfa_temp.1(%rip)
	jmp	.LBB15_83
.LBB15_95:                              # %if.end342
	cmpq	$100, %rsi
	jne	.LBB15_127
# BB#96:                                # %sw.bb348
	movq	16(%rax), %rcx
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB15_97
# BB#98:                                # %if.end363
	movq	8(%rax), %rdx
	movl	8(%rdx), %edx
	cmpq	$7, %rdx
	jne	.LBB15_100
# BB#99:                                # %if.end363
	movq	cfa_store(%rip), %rdx
	cmpq	$7, %rdx
	jne	.LBB15_100
# BB#101:                               # %if.end392
	movq	8(%rcx), %rcx
	movq	cfa_store+8(%rip), %rbx
	movq	cfa(%rip), %rdx
	subq	%rcx, %rbx
	movq	%rbx, cfa_store+8(%rip)
	cmpq	$7, %rdx
	jne	.LBB15_103
# BB#102:                               # %if.then396
	movq	%rbx, cfa+8(%rip)
.LBB15_103:                             # %if.end397
	negq	%rbx
	jmp	.LBB15_129
.LBB15_29:                              # %if.then86
	movq	cfa(%rip), %rax
	cmpq	$6, %rax
	jne	.LBB15_30
# BB#35:                                # %if.end106.thread
	movq	$7, cfa(%rip)
	movzbl	(%r14), %eax
	cmpq	$76, %rax
	je	.LBB15_43
# BB#36:                                # %if.end106.thread
	negq	%rdx
.LBB15_43:                              # %if.then115
	addq	%rdx, cfa+8(%rip)
.LBB15_44:                              # %if.end116
	movq	cfa_store(%rip), %rax
	cmpq	$7, %rax
	jne	.LBB15_83
# BB#45:                                # %if.then119
	addq	%rdx, cfa_store+8(%rip)
	jmp	.LBB15_83
.LBB15_122:                             # %if.else514
	movq	cfa_temp.0(%rip), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB15_124
# BB#123:                               # %if.then524
	movq	cfa_temp.1(%rip), %rbx
	negq	%rbx
.LBB15_129:                             # %sw.epilog553
	movl	8(%r14), %ecx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	je	.LBB15_138
# BB#130:                               # %land.lhs.true565
	movq	cfa(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB15_138
# BB#131:                               # %if.then572
	cmpq	$0, cfa+8(%rip)
	je	.LBB15_132
# BB#133:                               # %if.else576
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	je	.LBB15_135
# BB#134:                               # %if.then584
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB15_135:                             # %if.end588
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB15_136
# BB#137:                               # %if.end594
	movl	8(%rax), %eax
	movq	%rax, cfa(%rip)
	movabsq	$cfa, %rsi
	movq	%rbx, cfa+16(%rip)
	movq	$1, %rax
	movabsq	$dwarf2out_cfi_label.label, %rdi
	movl	%eax, cfa+24(%rip)
	jmp	.LBB15_84
.LBB15_138:                             # %if.end599
	movabsq	$dwarf2out_cfi_label.label, %rdi
	movabsq	$cfa, %rsi
	callq	def_cfa_1
.LBB15_139:                             # %sw.epilog601
	movq	$24, %rdi
	callq	xmalloc
	movq	queued_reg_saves(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%r14, 8(%rax)
	movq	%rbx, 16(%rax)
	movq	%rax, queued_reg_saves(%rip)
	movq	$dwarf2out_cfi_label.label, last_reg_save_label(%rip)
.LBB15_140:                             # %sw.epilog601
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB15_132:                             # %if.then575
	movabsq	$dwarf2out_cfi_label.label, %rdi
	movabsq	$cfa, %rsi
	callq	def_cfa_1
	movq	global_rtl+16(%rip), %r14
	jmp	.LBB15_139
.LBB15_62:                              # %if.else216
	cmpq	$61, %rdi
	jne	.LBB15_68
.LBB15_63:                              # %land.lhs.true224
	movl	8(%rsi), %esi
	movq	cfa_temp.0(%rip), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB15_68
# BB#64:                                # %land.lhs.true234
	movq	16(%r14), %rsi
	cmpq	%rdx, %rsi
	je	.LBB15_65
.LBB15_68:                              # %if.else250
	cmpq	$135, %rcx
	jne	.LBB15_71
# BB#69:                                # %land.lhs.true255
	movq	16(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB15_71
# BB#70:                                # %if.then263
	movl	8(%rax), %eax
	movq	%rax, cfa_temp.0(%rip)
	movq	8(%rcx), %rax
	movq	%rax, cfa_temp.1(%rip)
.LBB15_83:                              # %sw.epilog335
	movabsq	$dwarf2out_cfi_label.label, %rdi
	movabsq	$cfa, %rsi
.LBB15_84:                              # %sw.epilog335
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	def_cfa_1  # TAILCALL
.LBB15_65:                              # %if.then240
	movq	cfa(%rip), %rcx
	cmpq	$7, %rcx
	jne	.LBB15_66
# BB#67:                                # %if.end244
	movq	cfa+8(%rip), %rcx
	movq	cfa_temp.1(%rip), %rdx
	movl	8(%rax), %eax
	movq	%rax, cfa_store(%rip)
	subq	%rdx, %rcx
	movq	%rcx, cfa_store+8(%rip)
	jmp	.LBB15_83
.LBB15_86:                              # %if.then341
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1479, %rsi             # imm = 0x5C7
	jmp	.LBB15_12
.LBB15_11:                              # %if.then32
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1309, %rsi             # imm = 0x51D
	jmp	.LBB15_12
.LBB15_141:                             # %sw.default600
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1603, %rsi             # imm = 0x643
	jmp	.LBB15_12
.LBB15_114:                             # %if.then449
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1528, %rsi             # imm = 0x5F8
	jmp	.LBB15_12
.LBB15_57:                              # %if.then176
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1404, %rsi             # imm = 0x57C
	jmp	.LBB15_12
.LBB15_121:                             # %if.else499
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1538, %rsi             # imm = 0x602
	jmp	.LBB15_12
.LBB15_106:                             # %if.then430
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1513, %rsi             # imm = 0x5E9
	jmp	.LBB15_12
.LBB15_77:                              # %if.then314
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1457, %rsi             # imm = 0x5B1
	jmp	.LBB15_12
.LBB15_33:                              # %if.then79
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1353, %rsi             # imm = 0x549
	jmp	.LBB15_12
.LBB15_126:                             # %if.then542
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1555, %rsi             # imm = 0x613
	jmp	.LBB15_12
.LBB15_48:                              # %if.then127
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1386, %rsi             # imm = 0x56A
	jmp	.LBB15_12
.LBB15_55:                              # %if.else169
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1399, %rsi             # imm = 0x577
	jmp	.LBB15_12
.LBB15_34:                              # %sw.default
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1357, %rsi             # imm = 0x54D
	jmp	.LBB15_12
.LBB15_39:                              # %if.then103
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1371, %rsi             # imm = 0x55B
	jmp	.LBB15_12
.LBB15_81:                              # %sw.default334
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1471, %rsi             # imm = 0x5BF
	jmp	.LBB15_12
.LBB15_21:                              # %if.else
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1328, %rsi             # imm = 0x530
	jmp	.LBB15_12
.LBB15_127:                             # %sw.default552
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1561, %rsi             # imm = 0x619
	jmp	.LBB15_12
.LBB15_97:                              # %if.then362
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1490, %rsi             # imm = 0x5D2
	jmp	.LBB15_12
.LBB15_100:                             # %if.then391
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1495, %rsi             # imm = 0x5D7
	jmp	.LBB15_12
.LBB15_30:                              # %if.then89
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1364, %rsi             # imm = 0x554
	jmp	.LBB15_12
.LBB15_124:                             # %if.else526
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1549, %rsi             # imm = 0x60D
	jmp	.LBB15_12
.LBB15_136:                             # %if.then593
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1588, %rsi             # imm = 0x634
	jmp	.LBB15_12
.LBB15_71:                              # %if.else274
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1442, %rsi             # imm = 0x5A2
	jmp	.LBB15_12
.LBB15_66:                              # %if.then243
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_frame_debug_expr, %rcx
	movq	$1429, %rsi             # imm = 0x595
.LBB15_12:                              # %if.then32
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp164:
	.size	dwarf2out_frame_debug_expr, .Ltmp164-dwarf2out_frame_debug_expr
	.cfi_endproc

	.globl	dwarf2out_begin_prologue
	.align	16, 0x90
	.type	dwarf2out_begin_prologue,@function
dwarf2out_begin_prologue:               # @dwarf2out_begin_prologue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp171:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp172:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp173:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp174:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp175:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp176:
	.cfi_def_cfa_offset 80
.Ltmp177:
	.cfi_offset %rbx, -48
.Ltmp178:
	.cfi_offset %r12, -40
.Ltmp179:
	.cfi_offset %r13, -32
.Ltmp180:
	.cfi_offset %r14, -24
.Ltmp181:
	.cfi_offset %r15, -16
	movl	flag_unwind_tables(%rip), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$0, current_function_func_begin_label(%rip)
	testq	%rax, %rax
	jne	.LBB16_4
# BB#1:                                 # %switch.early.test.i
	movl	write_symbols(%rip), %eax
	cmpq	$4, %rax
	je	.LBB16_4
# BB#2:                                 # %switch.early.test.i
	cmpq	$7, %rax
	je	.LBB16_4
# BB#3:                                 # %lor.rhs.i
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_13
.LBB16_4:                               # %if.end
	movl	current_funcdef_number(%rip), %eax
	movq	current_function_decl(%rip), %rdi
	incq	%rax
	movl	%eax, current_funcdef_number(%rip)
	callq	function_section
	movl	current_funcdef_number(%rip), %ecx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str5, %rbx
	leaq	(%rsp), %r12
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movl	current_funcdef_number(%rip), %ecx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movq	%r12, %rdi
	callq	get_identifier
	movl	fde_table_in_use(%rip), %ebx
	movl	fde_table_allocated(%rip), %ecx
	movq	%rax, current_function_func_begin_label(%rip)
	cmpq	%rcx, %rbx
	jne	.LBB16_5
# BB#6:                                 # %if.then7
	addq	$256, %rbx              # imm = 0x100
	movl	%ebx, fde_table_allocated(%rip)
	movq	fde_table(%rip), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,4), %rsi
	callq	xrealloc
	movl	fde_table_in_use(%rip), %ebx
	movq	%rax, %r13
	movq	%r13, fde_table(%rip)
	jmp	.LBB16_7
.LBB16_5:                               # %if.end.if.end9_crit_edge
	movq	fde_table(%rip), %r13
.LBB16_7:                               # %if.end9
	leaq	1(%rbx), %rax
	movl	%eax, fde_table_in_use(%rip)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rbx
	leaq	(%rsp), %rdi
	callq	xstrdup
	leaq	(%rbx,%rbx,4), %rdx
	movq	%rax, (%r13,%rdx,8)
	movq	$0, 24(%r13,%rdx,8)
	movq	$0, 16(%r13,%rdx,8)
	movq	$0, 8(%r13,%rdx,8)
	movl	current_funcdef_number(%rip), %eax
	movl	%eax, 32(%r13,%rdx,8)
	movzbl	current_function_nothrow(%rip), %eax
	movzbl	36(%r13,%rdx,8), %esi
	andq	$1, %rax
	andq	$-2, %rsi
	orq	%rax, %rsi
	movb	%sil, 36(%r13,%rdx,8)
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
	movq	$21, %rcx
	shrq	%cl, %rax
	andq	$2, %rax
	andq	$253, %rsi
	orq	%rax, %rsi
	movb	%sil, 36(%r13,%rdx,8)
	movq	$0, old_args_size(%rip)
	movq	$0, args_size(%rip)
	testq	%r15, %r15
	je	.LBB16_13
# BB#8:                                 # %if.end9
	movl	debug_info_level(%rip), %eax
	andq	%r12, %rax
	cmpq	$2, %rax
	jb	.LBB16_13
# BB#9:                                 # %if.then.i
	movq	current_function_decl(%rip), %rdi
	callq	function_section
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_11
# BB#10:                                # %if.then1.i
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str408, %rsi
	movabsq	$.L.str409, %rdx
	xorq	%rax, %rax
	movq	%r15, %rcx
	movq	%r14, %r8
	callq	fprintf
.LBB16_11:                              # %if.end.i
	movq	%r15, %rdi
	callq	lookup_filename
	movq	asm_out_file(%rip), %rdi
	movq	%rax, %rcx
	movabsq	$.L.str410, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	line_info_table_in_use(%rip), %eax
	movq	current_function_decl(%rip), %rcx
	incq	%rax
	movl	%eax, line_info_table_in_use(%rip)
	cmpq	$0, 128(%rcx)
	je	.LBB16_13
# BB#12:                                # %if.then5.i
	movl	separate_line_info_table_in_use(%rip), %eax
	incq	%rax
	movl	%eax, separate_line_info_table_in_use(%rip)
.LBB16_13:                              # %if.end23
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp182:
	.size	dwarf2out_begin_prologue, .Ltmp182-dwarf2out_begin_prologue
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_source_line,@function
dwarf2out_source_line:                  # @dwarf2out_source_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp187:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp188:
	.cfi_def_cfa_offset 32
.Ltmp189:
	.cfi_offset %rbx, -24
.Ltmp190:
	.cfi_offset %r14, -16
	movl	debug_info_level(%rip), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	cmpq	$2, %rax
	jb	.LBB17_5
# BB#1:                                 # %if.then
	movq	current_function_decl(%rip), %rdi
	callq	function_section
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_3
# BB#2:                                 # %if.then1
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str408, %rsi
	movabsq	$.L.str409, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	fprintf
.LBB17_3:                               # %if.end
	movq	%rbx, %rdi
	callq	lookup_filename
	movq	asm_out_file(%rip), %rdi
	movq	%rax, %rcx
	movabsq	$.L.str410, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	line_info_table_in_use(%rip), %eax
	movq	current_function_decl(%rip), %rcx
	incq	%rax
	movl	%eax, line_info_table_in_use(%rip)
	cmpq	$0, 128(%rcx)
	je	.LBB17_5
# BB#4:                                 # %if.then5
	movl	separate_line_info_table_in_use(%rip), %eax
	incq	%rax
	movl	%eax, separate_line_info_table_in_use(%rip)
.LBB17_5:                               # %if.end8
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp191:
	.size	dwarf2out_source_line, .Ltmp191-dwarf2out_source_line
	.cfi_endproc

	.globl	dwarf2out_end_epilogue
	.align	16, 0x90
	.type	dwarf2out_end_epilogue,@function
dwarf2out_end_epilogue:                 # @dwarf2out_end_epilogue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp196:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp197:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp198:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp199:
	.cfi_def_cfa_offset 64
.Ltmp200:
	.cfi_offset %rbx, -32
.Ltmp201:
	.cfi_offset %r14, -24
.Ltmp202:
	.cfi_offset %r15, -16
	movl	current_funcdef_number(%rip), %ecx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str7, %rdx
	leaq	(%rsp), %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%r14, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	fde_table_in_use(%rip), %eax
	movq	fde_table(%rip), %r15
	decq	%rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%r14, %rdi
	callq	xstrdup
	leaq	(%rbx,%rbx,4), %rcx
	movq	%rax, 16(%r15,%rcx,8)
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp203:
	.size	dwarf2out_end_epilogue, .Ltmp203-dwarf2out_end_epilogue
	.cfi_endproc

	.globl	dwarf2out_frame_init
	.align	16, 0x90
	.type	dwarf2out_frame_init,@function
dwarf2out_frame_init:                   # @dwarf2out_frame_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp209:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp210:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp211:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp212:
	.cfi_def_cfa_offset 40
	subq	$40, %rsp
.Ltmp213:
	.cfi_def_cfa_offset 80
.Ltmp214:
	.cfi_offset %rbx, -40
.Ltmp215:
	.cfi_offset %r12, -32
.Ltmp216:
	.cfi_offset %r14, -24
.Ltmp217:
	.cfi_offset %r15, -16
	movq	$256, %rdi              # imm = 0x100
	movq	$40, %rsi
	movq	$256, %rbx              # imm = 0x100
	callq	xcalloc
	movq	%rax, fde_table(%rip)
	movl	%ebx, fde_table_allocated(%rip)
	xorq	%rax, %rax
	movl	%eax, fde_table_in_use(%rip)
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	movl	%eax, 32(%rsp)
	movq	$0, 24(%rsp)
	movq	$7, 8(%rsp)
	movq	%rcx, 16(%rsp)
	leaq	8(%rsp), %rsi
	xorq	%rdi, %rdi
	callq	def_cfa_1
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	jmp	.LBB19_1
	.align	16, 0x90
.LBB19_23:                              # %if.end55.i
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	8(%rax), %rax
.LBB19_1:                               # %tailrecurse.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB19_2
# BB#21:                                # %sw.bb47.i
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	je	.LBB19_23
# BB#22:                                # %if.then54.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.initial_return_save, %rcx
	movq	$921, %rsi              # imm = 0x399
	jmp	.LBB19_14
.LBB19_2:                               # %tailrecurse.i
	cmpq	$66, %rcx
	jne	.LBB19_3
# BB#8:                                 # %sw.bb7.i
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$76, %rcx
	je	.LBB19_17
# BB#9:                                 # %sw.bb7.i
	cmpq	$75, %rcx
	jne	.LBB19_10
# BB#15:                                # %sw.bb16.i
	movq	8(%rax), %rcx
	movl	8(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB19_16
# BB#25:                                # %if.end25.i
	movq	16(%rax), %rax
	movq	8(%rax), %r14
	jmp	.LBB19_26
.LBB19_3:                               # %tailrecurse.i
	cmpq	$61, %rcx
	jne	.LBB19_24
# BB#4:                                 # %sw.bb.i
	movzbl	target_flags+3(%rip), %ecx
	movl	8(%rax), %eax
	testq	$2, %rcx
	je	.LBB19_5
# BB#6:                                 # %sw.bb.i
	movabsq	$dbx64_register_map, %rcx
	jmp	.LBB19_7
.LBB19_5:
	movabsq	$svr4_dbx_register_map, %rcx
.LBB19_7:                               # %sw.bb.i
	movl	(%rcx,%rax,4), %r15d
	xorq	%r14, %r14
	jmp	.LBB19_27
.LBB19_17:                              # %sw.bb31.i
	movq	8(%rax), %rcx
	movl	8(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB19_18
# BB#19:                                # %if.end40.i
	movq	16(%rax), %rax
	movq	8(%rax), %r14
	negq	%r14
	jmp	.LBB19_26
.LBB19_10:                              # %sw.bb7.i
	cmpq	$61, %rcx
	jne	.LBB19_20
# BB#11:                                # %sw.bb12.i
	movl	8(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB19_13
# BB#12:
	xorq	%r14, %r14
.LBB19_26:                              # %sw.epilog60.i
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB19_27:                              # %sw.epilog60.i
	movl	target_flags(%rip), %ebx
	movq	$22, %rcx
	movq	cfa+8(%rip), %r12
	shrq	%cl, %rbx
	andq	$8, %rbx
	addq	$8, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	%rax, %rsi
	movq	$0, (%rsi)
	movq	$0, 24(%rsi)
	movq	$0, 16(%rsi)
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rbx, %rax
	movq	%rax, 16(%rsi)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	cmpq	%rax, %r15
	je	.LBB19_28
# BB#31:                                # %if.else12.i.i
	andq	%rax, %rbx
	cmpq	%rbx, %r15
	je	.LBB19_36
# BB#32:                                # %if.else16.i.i
	movq	$9, %rax
	movl	%eax, 8(%rsi)
	movq	%r15, 24(%rsi)
	jmp	.LBB19_33
.LBB19_28:                              # %if.then.i.i
	subq	%r12, %r14
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	movq	$-4, %rcx
	subq	%rax, %rcx
	movq	%r14, %rax
	cqto
	idivq	%rcx
	testq	%rax, %rax
	movq	$17, %rcx
	js	.LBB19_30
# BB#29:                                # %if.then.i.i
	movq	$128, %rcx
.LBB19_30:                              # %if.then.i.i
	movl	%ecx, 8(%rsi)
	movq	%rax, 24(%rsi)
.LBB19_33:                              # %for.cond.i37.i.i
	movabsq	$cie_cfi_head, %rax
	.align	16, 0x90
.LBB19_34:                              # %for.cond.i37.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB19_34
# BB#35:                                # %add_fde_cfi.exit.i
	movq	%rsi, (%rcx)
.LBB19_36:                              # %initial_return_save.exit
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB19_16:                              # %if.then24.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.initial_return_save, %rcx
	movq	$900, %rsi              # imm = 0x384
	jmp	.LBB19_14
.LBB19_24:                              # %sw.default59.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.initial_return_save, %rcx
	movq	$926, %rsi              # imm = 0x39E
	jmp	.LBB19_14
.LBB19_18:                              # %if.then39.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.initial_return_save, %rcx
	movq	$906, %rsi              # imm = 0x38A
	jmp	.LBB19_14
.LBB19_20:                              # %sw.default.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.initial_return_save, %rcx
	movq	$911, %rsi              # imm = 0x38F
	jmp	.LBB19_14
.LBB19_13:                              # %if.then.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.initial_return_save, %rcx
	movq	$894, %rsi              # imm = 0x37E
.LBB19_14:                              # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp218:
	.size	dwarf2out_frame_init, .Ltmp218-dwarf2out_frame_init
	.cfi_endproc

	.globl	dwarf2out_frame_finish
	.align	16, 0x90
	.type	dwarf2out_frame_finish,@function
dwarf2out_frame_finish:                 # @dwarf2out_frame_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp220:
	.cfi_def_cfa_offset 16
	movl	write_symbols(%rip), %eax
	cmpq	$4, %rax
	je	.LBB20_2
# BB#1:                                 # %entry
	cmpq	$7, %rax
	jne	.LBB20_3
.LBB20_2:                               # %if.then
	xorq	%rdi, %rdi
	callq	output_call_frame_info
.LBB20_3:                               # %if.end
	movl	flag_unwind_tables(%rip), %eax
	movl	flag_exceptions(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB20_4
# BB#5:                                 # %if.then4
	movq	$1, %rdi
	popq	%rax
	jmp	output_call_frame_info  # TAILCALL
.LBB20_4:                               # %if.end5
	popq	%rax
	retq
.Ltmp221:
	.size	dwarf2out_frame_finish, .Ltmp221-dwarf2out_frame_finish
	.cfi_endproc

	.align	16, 0x90
	.type	output_call_frame_info,@function
output_call_frame_info:                 # @output_call_frame_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp228:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp229:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp230:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp231:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp233:
	.cfi_def_cfa_offset 208
.Ltmp234:
	.cfi_offset %rbx, -48
.Ltmp235:
	.cfi_offset %r12, -40
.Ltmp236:
	.cfi_offset %r13, -32
.Ltmp237:
	.cfi_offset %r14, -24
.Ltmp238:
	.cfi_offset %r15, -16
	xorq	%r10, %r10
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	movq	%rdi, %r15
	je	.LBB21_10
# BB#1:                                 # %if.then
	movl	fde_table_in_use(%rip), %r8d
	movl	flag_asynchronous_unwind_tables(%rip), %edi
	xorq	%r10, %r10
	testq	%r8, %r8
	je	.LBB21_2
# BB#3:                                 # %for.body.lr.ph
	movq	fde_table(%rip), %rcx
	addq	$36, %rcx
	xorq	%rax, %rax
	movq	$1, %rdx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB21_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %esi
	testq	$2, %rsi
	movq	$1, %rbx
	movq	$1, %r10
	jne	.LBB21_8
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB21_4 Depth=1
	testq	$1, %rsi
	movq	$1, %rbx
	je	.LBB21_7
# BB#6:                                 # %if.else
                                        #   in Loop: Header=BB21_4 Depth=1
	movq	%rdi, %rbx
.LBB21_7:                               # %if.else
                                        #   in Loop: Header=BB21_4 Depth=1
	movq	%rax, %r10
.LBB21_8:                               # %for.inc
                                        #   in Loop: Header=BB21_4 Depth=1
	movq	%rdx, %rax
	andq	%r9, %rax
	incq	%rdx
	addq	$40, %rcx
	cmpq	%r8, %rax
	movq	%r10, %rax
	movq	%rbx, %rdi
	jb	.LBB21_4
	jmp	.LBB21_9
.LBB21_2:
	movq	%rdi, %rbx
.LBB21_9:                               # %for.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB21_74
.LBB21_10:                              # %if.end14
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_12
# BB#11:                                # %if.then16
	callq	app_enable
.LBB21_12:                              # %if.end17
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB21_14
# BB#13:                                # %if.then19
	callq	*targetm+144(%rip)
	jmp	.LBB21_15
.LBB21_14:                              # %if.else20
	movabsq	$.L.str441, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
.LBB21_15:                              # %do.body
	movabsq	$.L.str1, %r12
	movabsq	$.L.str442, %rdx
	leaq	64(%rsp), %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r15, %rbx
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %rcx
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%r14, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %r15
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	fwrite
	movabsq	$.L.str443, %rdx
	leaq	128(%rsp), %r13
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rcx
	callq	sprintf
	movabsq	$.L.str444, %rdx
	leaq	96(%rsp), %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rcx
	callq	sprintf
	movabsq	$.L.str445, %rcx
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	dw2_asm_output_delta
	movq	asm_out_file(%rip), %rdi
	movq	%r13, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r15, %rdi
	callq	fwrite
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movabsq	$0, %rsi
	jne	.LBB21_17
# BB#16:                                # %do.body
	movq	%rax, %rsi
.LBB21_17:                              # %do.body
	movabsq	$.L.str446, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$.L.str447, %rdx
	movq	$1, %rdi
	movq	$1, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	testq	%rbx, %rbx
	movabsq	$0, %rbx
	movb	%bl, 58(%rsp)
	movabsq	$0, %r15
	movabsq	$0, %r12
	je	.LBB21_31
# BB#18:                                # %if.then43
	movl	flag_pic(%rip), %r14d
	testq	%r14, %r14
	movq	%r14, %rax
	je	.LBB21_20
# BB#19:                                # %if.then43
	movq	$27, %rax
.LBB21_20:                              # %if.then43
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r14, %r15
	je	.LBB21_22
# BB#21:                                # %if.then43
	movq	$155, %r15
.LBB21_22:                              # %if.then43
	xorq	%r12, %r12
	cmpq	$0, libfunc_table+152(%rip)
	leaq	59(%rsp), %rax
	movq	%rax, %rbx
	movq	%rax, %r13
	je	.LBB21_24
# BB#23:                                # %if.then52
	leaq	60(%rsp), %rbx
	movq	$80, %rax
	movb	%al, 59(%rsp)
	movq	%r15, %rdi
	callq	size_of_encoded_value
	movq	%rax, %r12
	incq	%r12
.LBB21_24:                              # %if.end55
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB21_26
# BB#25:                                # %if.then57
	movq	$76, %rax
	movb	%al, (%rbx)
	incq	%rbx
	incq	%r12
.LBB21_26:                              # %if.end60
	testq	%r14, %r14
	je	.LBB21_28
# BB#27:                                # %if.then63
	movq	$82, %rax
	movb	%al, (%rbx)
	incq	%rbx
	incq	%r12
.LBB21_28:                              # %if.end66
	cmpq	%r13, %rbx
	jbe	.LBB21_30
# BB#29:                                # %if.then71
	movq	$122, %rax
	movb	%al, 58(%rsp)
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB21_30:                              # %if.end109
	movq	16(%rsp), %rbx          # 8-byte Reload
.LBB21_31:                              # %if.end109
	movabsq	$.L.str448, %rdx
	leaq	58(%rsp), %rdi
	movq	$-1, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_nstring
	movabsq	$.L.str449, %rsi
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data_uleb128
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	movq	$-4, %rdi
	subq	%rax, %rdi
	movabsq	$.L.str450, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data_sleb128
	movl	target_flags(%rip), %eax
	movq	$22, %rcx
	shrq	%cl, %rax
	andq	$8, %rax
	addq	$8, %rax
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	andq	%rax, %rsi
	movabsq	$.L.str451, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movzbl	58(%rsp), %eax
	testq	%rax, %rax
	je	.LBB21_38
# BB#32:                                # %if.then122
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movabsq	$.L.str452, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	dw2_asm_output_data_uleb128
	cmpq	$0, libfunc_table+152(%rip)
	je	.LBB21_34
# BB#33:                                # %if.then125
	movq	%rbx, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%r15, %rbx
	movq	%r15, %rdi
	callq	eh_data_format_name
	movq	%rax, %rcx
	movabsq	$.L.str453, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rbx
	callq	dw2_asm_output_data
	movq	libfunc_table+152(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	dw2_asm_output_encoded_addr_rtx
.LBB21_34:                              # %if.end128
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r15, %rax
	je	.LBB21_36
# BB#35:                                # %if.then130
	movq	%rbx, %r14
                                        # kill: RBX<def> R14<kill>
	andq	%r15, %rbx
	movq	%r14, %rdi
	callq	eh_data_format_name
	movq	%rax, %rcx
	movabsq	$.L.str454, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rbx
	callq	dw2_asm_output_data
.LBB21_36:                              # %if.end133
	testq	%r15, %rbx
	je	.LBB21_38
# BB#37:                                # %if.then136
	andq	%rbx, %r15
	movq	%rbx, %rdi
	callq	eh_data_format_name
	movq	%rax, %rcx
	movabsq	$.L.str455, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	dw2_asm_output_data
.LBB21_38:                              # %for.cond141.preheader
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	cie_cfi_head(%rip), %rbx
	testq	%rbx, %rbx
	movq	48(%rsp), %r14          # 8-byte Reload
	je	.LBB21_40
	.align	16, 0x90
.LBB21_39:                              # %for.body144
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	output_cfi
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB21_39
.LBB21_40:                              # %for.end146
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	callq	floor_log2_wide
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB21_42
# BB#41:                                # %if.then161
	movl	target_flags(%rip), %edi
	movq	asm_out_file(%rip), %r15
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	callq	floor_log2_wide
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$.L.str456, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
.LBB21_42:                              # %if.end178
	movq	asm_out_file(%rip), %rdi
	leaq	96(%rsp), %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	fde_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_72
# BB#43:                                # %for.body184.lr.ph
	xorq	%r15, %r15
	testq	%rbx, %r14
	je	.LBB21_59
# BB#44:
	leaq	128(%rsp), %r12
	.align	16, 0x90
.LBB21_45:                              # %for.body184.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_54 Depth 2
	movq	%r15, %rcx
	movl	flag_asynchronous_unwind_tables(%rip), %edx
	movq	fde_table(%rip), %r14
	andq	%rbx, %rcx
	leaq	(%rcx,%rcx,4), %rsi
	testq	%rdx, %rdx
	jne	.LBB21_47
# BB#46:                                # %land.lhs.true190.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movzbl	36(%r14,%rsi,8), %ecx
	andq	$3, %rcx
	cmpq	$1, %rcx
	je	.LBB21_58
.LBB21_47:                              # %if.end254.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movq	asm_out_file(%rip), %rdi
	leaq	(%r14,%rsi,8), %r13
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r15,2), %rbx
	movq	%r15, 32(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str217, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str1, %r15
	movq	%r15, %rsi
	movabsq	$.L.str457, %rdx
	movq	%rbx, %rcx
	callq	sprintf
	xorq	%rax, %rax
	movq	%r14, 24(%rsp)          # 8-byte Spill
	leaq	96(%rsp), %r14
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	40(%rsp), %r15          # 8-byte Reload
	movabsq	$.L.str458, %rdx
	movq	%rbx, %rcx
	callq	sprintf
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r12, %rdx
	movabsq	$.L.str459, %rcx
	callq	dw2_asm_output_delta
	movq	asm_out_file(%rip), %rdi
	movq	%r12, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str3, %rdi
	callq	fwrite
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	leaq	64(%rsp), %rdx
	movabsq	$.L.str460, %rcx
	callq	dw2_asm_output_delta
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	(%r13), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	callq	gen_rtx_fmt_s
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movabsq	$.L.str461, %rdx
	callq	dw2_asm_output_encoded_addr_rtx
	movq	%rbx, %rdi
	callq	size_of_encoded_value
	movq	16(%r14,%r15,8), %rsi
	movq	(%r13), %rdx
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str462, %rcx
	callq	dw2_asm_output_delta
	movzbl	58(%rsp), %eax
	testq	%rax, %rax
	je	.LBB21_53
# BB#48:                                # %if.then257.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_49
# BB#52:                                # %if.else327.us
                                        #   in Loop: Header=BB21_45 Depth=1
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str452, %rsi
	callq	dw2_asm_output_data_uleb128
	jmp	.LBB21_53
.LBB21_49:                              # %if.then259.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movq	%rbx, %rdi
	callq	size_of_encoded_value
	movq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movabsq	$.L.str452, %rsi
	callq	dw2_asm_output_data_uleb128
	movzbl	36(%r14,%r15,8), %eax
	testq	$2, %rax
	jne	.LBB21_50
# BB#51:                                # %if.end324.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movq	%rbx, %rdi
	callq	size_of_encoded_value
	movq	%rax, %rcx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str465, %rdx
	callq	dw2_asm_output_data
	jmp	.LBB21_53
.LBB21_50:                              # %do.body293.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movl	32(%r14,%r15,8), %ecx
	xorq	%rax, %rax
	movq	%r12, %r15
	movq	%r15, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str463, %rdx
	callq	sprintf
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%r15, %rdx
	movq	%r15, %r12
	movq	40(%rsp), %r15          # 8-byte Reload
	callq	gen_rtx_fmt_s
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movabsq	$.L.str464, %rdx
	callq	dw2_asm_output_encoded_addr_rtx
	.align	16, 0x90
.LBB21_53:                              # %if.end329.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movq	(%r13), %rax
	movq	%rax, 8(%r14,%r15,8)
	movq	24(%r14,%r15,8), %rbx
	testq	%rbx, %rbx
	movq	48(%rsp), %r14          # 8-byte Reload
	je	.LBB21_55
	.align	16, 0x90
.LBB21_54:                              # %for.body334.us
                                        #   Parent Loop BB21_45 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	output_cfi
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB21_54
.LBB21_55:                              # %for.end337.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	callq	floor_log2_wide
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movq	32(%rsp), %r15          # 8-byte Reload
	je	.LBB21_57
# BB#56:                                # %if.then355.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movl	target_flags(%rip), %edi
	movq	asm_out_file(%rip), %r14
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	callq	floor_log2_wide
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str456, %rsi
	callq	fprintf
.LBB21_57:                              # %if.end373.us
                                        #   in Loop: Header=BB21_45 Depth=1
	movq	asm_out_file(%rip), %rdi
	leaq	96(%rsp), %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str3, %rdi
	callq	fwrite
	movl	fde_table_in_use(%rip), %eax
.LBB21_58:                              # %for.inc376.us
                                        #   in Loop: Header=BB21_45 Depth=1
	incq	%r15
	movq	%r15, %rcx
	andq	%rbx, %rcx
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB21_45
	jmp	.LBB21_72
.LBB21_59:
	leaq	96(%rsp), %r14
	.align	16, 0x90
.LBB21_60:                              # %if.end254
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_68 Depth 2
	movq	%r15, %rax
	movq	fde_table(%rip), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	asm_out_file(%rip), %rdi
	andq	%rbx, %rax
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	leaq	(%r15,%r15), %r12
	xorq	%rax, %rax
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str217, %rdx
	movq	%r12, %rcx
	callq	fprintf
	xorq	%rax, %rax
	leaq	128(%rsp), %r13
	movq	%r13, %rdi
	movabsq	$.L.str1, %rbx
	movq	%rbx, %rsi
	movabsq	$.L.str457, %rdx
	movq	%r12, %rcx
	callq	sprintf
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str458, %rdx
	movq	%r12, %rcx
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rbx          # 8-byte Reload
	callq	sprintf
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r13, %rdx
	movabsq	$.L.str459, %rcx
	callq	dw2_asm_output_delta
	movq	asm_out_file(%rip), %rdi
	movq	%r13, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str3, %rdi
	movq	%rdi, %r13
	callq	fwrite
	movq	$4, %rdi
	xorq	%rax, %rax
	leaq	64(%rsp), %rsi
	movabsq	$.L.str460, %rdx
	callq	dw2_asm_output_offset
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	movq	(%rbx,%r12,8), %rsi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str461, %rdx
	callq	dw2_asm_output_addr
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	movq	16(%rbx,%r12,8), %rsi
	movq	(%rbx,%r12,8), %rdx
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str462, %rcx
	callq	dw2_asm_output_delta
	movzbl	58(%rsp), %eax
	testq	%rax, %rax
	je	.LBB21_66
# BB#61:                                # %if.then257
                                        #   in Loop: Header=BB21_60 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	16(%rsp), %r14          # 8-byte Reload
	jne	.LBB21_62
# BB#65:                                # %if.else327
                                        #   in Loop: Header=BB21_60 Depth=1
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str452, %rsi
	callq	dw2_asm_output_data_uleb128
	jmp	.LBB21_66
	.align	16, 0x90
.LBB21_62:                              # %if.then259
                                        #   in Loop: Header=BB21_60 Depth=1
	movq	%r14, %rdi
	callq	size_of_encoded_value
	movq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movabsq	$.L.str452, %rsi
	callq	dw2_asm_output_data_uleb128
	movzbl	36(%rbx,%r12,8), %eax
	testq	$2, %rax
	jne	.LBB21_63
# BB#64:                                # %if.end324
                                        #   in Loop: Header=BB21_60 Depth=1
	movq	%r14, %rdi
	callq	size_of_encoded_value
	movq	%rax, %rcx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str465, %rdx
	callq	dw2_asm_output_data
	jmp	.LBB21_66
.LBB21_63:                              # %do.body293
                                        #   in Loop: Header=BB21_60 Depth=1
	movl	32(%rbx,%r12,8), %ecx
	xorq	%rax, %rax
	movq	%rbx, %r12
	leaq	128(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str463, %rdx
	callq	sprintf
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%rbx, %rdx
	movq	%r12, %rbx
	movq	48(%rsp), %r12          # 8-byte Reload
	callq	gen_rtx_fmt_s
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movabsq	$.L.str464, %rdx
	callq	dw2_asm_output_encoded_addr_rtx
	.align	16, 0x90
.LBB21_66:                              # %if.end329
                                        #   in Loop: Header=BB21_60 Depth=1
	movq	%r12, %rcx
	leaq	(%rbx,%rcx,8), %r12
	movq	(%r12), %rax
	movq	%rax, 8(%rbx,%rcx,8)
	movq	24(%rbx,%rcx,8), %rbx
	jmp	.LBB21_68
	.align	16, 0x90
.LBB21_67:                              # %for.body334
                                        #   in Loop: Header=BB21_68 Depth=2
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	output_cfi
	movq	(%rbx), %rbx
.LBB21_68:                              # %for.body334
                                        #   Parent Loop BB21_60 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	jne	.LBB21_67
# BB#69:                                # %for.end337
                                        #   in Loop: Header=BB21_60 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	callq	floor_log2_wide
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movq	%r13, %r12
	je	.LBB21_71
# BB#70:                                # %if.then355
                                        #   in Loop: Header=BB21_60 Depth=1
	movl	target_flags(%rip), %edi
	movq	asm_out_file(%rip), %r14
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	callq	floor_log2_wide
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str456, %rsi
	callq	fprintf
.LBB21_71:                              # %for.inc376
                                        #   in Loop: Header=BB21_60 Depth=1
	movq	asm_out_file(%rip), %rdi
	leaq	96(%rsp), %r14
	movq	%r14, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r12, %rdi
	callq	fwrite
	movl	fde_table_in_use(%rip), %eax
	incq	%r15
	movq	%r15, %rcx
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB21_60
.LBB21_72:                              # %for.end378
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_74
# BB#73:                                # %if.then380
	callq	app_disable
.LBB21_74:                              # %if.end381
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp239:
	.size	output_call_frame_info, .Ltmp239-output_call_frame_info
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_init,@function
dwarf2out_init:                         # @dwarf2out_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp245:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp246:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp247:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp248:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp249:
	.cfi_def_cfa_offset 48
.Ltmp250:
	.cfi_offset %rbx, -40
.Ltmp251:
	.cfi_offset %r12, -32
.Ltmp252:
	.cfi_offset %r14, -24
.Ltmp253:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$64, %rdi
	movq	$8, %rsi
	movq	$64, %r14
	callq	xcalloc
	movq	%rax, file_table.0(%rip)
	movl	%r14d, file_table.1(%rip)
	movq	$1, %r14
	movl	%r14d, file_table.2(%rip)
	xorq	%r15, %r15
	movl	%r15d, file_table.3(%rip)
	movq	%rbx, primary_filename(%rip)
	movq	%rbx, %rdi
	callq	lookup_filename
	movq	$256, %rdi              # imm = 0x100
	movq	$8, %rsi
	movq	$256, %r12              # imm = 0x100
	callq	xcalloc
	movq	%rax, decl_die_table(%rip)
	movl	%r12d, decl_die_table_allocated(%rip)
	movl	%r15d, decl_die_table_in_use(%rip)
	movabsq	$.L.str391, %rdx
	movq	$256, %rdi              # imm = 0x100
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, decl_scope_table(%rip)
	movabsq	$decl_scope_table, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_varray_root
	movq	$256, %rdi              # imm = 0x100
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, abbrev_die_table(%rip)
	movl	%r12d, abbrev_die_table_allocated(%rip)
	movl	%r14d, abbrev_die_table_in_use(%rip)
	movq	$1024, %rdi             # imm = 0x400
	movq	$16, %rsi
	callq	xcalloc
	movq	%rax, line_info_table(%rip)
	movl	%r14d, line_info_table_in_use(%rip)
	movq	%rbx, %rdi
	callq	gen_compile_unit_die
	movq	%rax, comp_unit_die(%rip)
	movabsq	$.L.str392, %rdx
	movq	$64, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, incomplete_types(%rip)
	movabsq	$incomplete_types, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_varray_root
	movabsq	$.L.str393, %rdx
	movq	$32, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, used_rtx_varray(%rip)
	movabsq	$used_rtx_varray, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_varray_root
	movabsq	$limbo_die_list, %rdi
	movabsq	$mark_limbo_die_list, %rcx
	movq	$1, %rsi
	movq	$1, %rdx
	callq	ggc_add_root
	movabsq	$text_end_label, %rdi
	movabsq	$.L.str1, %rbx
	movabsq	$.L.str8, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	sprintf
	movabsq	$abbrev_section_label, %r12
	movabsq	$.L.str394, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	sprintf
	movabsq	$text_section_label, %rdi
	movabsq	$.L.str395, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	sprintf
	movabsq	$debug_info_section_label, %r15
	movabsq	$.L.str396, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	sprintf
	movabsq	$debug_line_section_label, %r14
	movabsq	$.L.str397, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	sprintf
	movabsq	$ranges_section_label, %rdi
	movabsq	$.L.str398, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	sprintf
	movabsq	$.L.str11, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movq	asm_out_file(%rip), %rdi
	movq	%r12, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rbx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	fwrite
	movabsq	$.L.str211, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movq	asm_out_file(%rip), %rdi
	movq	%r15, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	fwrite
	movabsq	$.L.str399, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movq	asm_out_file(%rip), %rdi
	movq	%r14, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	fwrite
	movl	debug_info_level(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB22_2
# BB#1:                                 # %if.then
	movabsq	$.L.str15, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movabsq	$macinfo_section_label, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str400, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB22_2:                               # %if.end
	callq	text_section
	movq	asm_out_file(%rip), %rdi
	movabsq	$text_section_label, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.Ltmp254:
	.size	dwarf2out_init, .Ltmp254-dwarf2out_init
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_finish,@function
dwarf2out_finish:                       # @dwarf2out_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp261:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp262:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp263:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp264:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp265:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp266:
	.cfi_def_cfa_offset 144
.Ltmp267:
	.cfi_offset %rbx, -48
.Ltmp268:
	.cfi_offset %r12, -40
.Ltmp269:
	.cfi_offset %r13, -32
.Ltmp270:
	.cfi_offset %r14, -24
.Ltmp271:
	.cfi_offset %r15, -16
	movq	limbo_die_list(%rip), %rbx
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_32:                              # %if.end72
                                        #   in Loop: Header=BB23_1 Depth=1
	callq	free
.LBB23_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_3 Depth 2
                                        #       Child Loop BB23_5 Depth 3
	testq	%rbx, %rbx
	je	.LBB23_43
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rbx, %rdi
	movq	(%rdi), %rax
	movq	16(%rdi), %rbx
	cmpq	$0, 24(%rax)
	movq	%rax, %rcx
	jne	.LBB23_32
.LBB23_3:                               # %tailrecurse.i.i
                                        #   Parent Loop BB23_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_5 Depth 3
	testq	%rcx, %rcx
	je	.LBB23_11
# BB#4:                                 # %if.then.i.i
                                        #   in Loop: Header=BB23_3 Depth=2
	movq	16(%rcx), %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	je	.LBB23_11
	.align	16, 0x90
.LBB23_5:                               # %for.body.i.i
                                        #   Parent Loop BB23_1 Depth=1
                                        #     Parent Loop BB23_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rdx), %esi
	cmpq	$49, %rsi
	je	.LBB23_19
# BB#6:                                 # %for.body.i.i
                                        #   in Loop: Header=BB23_5 Depth=3
	cmpq	$71, %rsi
	jne	.LBB23_9
# BB#7:                                 # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB23_5 Depth=3
	movl	16(%rdx), %ecx
	cmpq	$10, %rcx
	jne	.LBB23_416
# BB#8:                                 # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB23_5 Depth=3
	movq	24(%rdx), %rcx
.LBB23_9:                               # %for.inc.i.i
                                        #   in Loop: Header=BB23_5 Depth=3
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB23_5
# BB#10:                                # %for.end.i.i
                                        #   in Loop: Header=BB23_3 Depth=2
	testq	%rcx, %rcx
	jne	.LBB23_3
	jmp	.LBB23_11
	.align	16, 0x90
.LBB23_19:                              # %land.lhs.true.i.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	16(%rdx), %ecx
	cmpq	$10, %rcx
	jne	.LBB23_20
# BB#22:                                # %get_AT_ref.exit
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	24(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_23
.LBB23_11:                              # %if.else
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	comp_unit_die(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB23_32
# BB#12:                                # %if.else7
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rdi), %r9
	testq	%r9, %r9
	je	.LBB23_27
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	16(%r9), %esi
	cmpq	$118, %rsi
	jne	.LBB23_27
# BB#14:                                # %land.lhs.true11
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	40(%r9), %rsi
	movl	36(%rsi), %r8d
	movl	decl_die_table_in_use(%rip), %esi
	cmpq	%r8, %rsi
	jbe	.LBB23_27
# BB#15:                                # %lookup_decl_die.exit
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	decl_die_table(%rip), %rsi
	movq	(%rsi,%r8,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB23_16
.LBB23_27:                              # %if.else16
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	global_dc(%rip), %rsi
	movslq	60(%rsi), %rdx
	testq	%rdx, %rdx
	jg	.LBB23_29
# BB#28:                                # %lor.lhs.false
                                        #   in Loop: Header=BB23_1 Depth=1
	movslq	56(%rsi), %rdx
	testq	%rdx, %rdx
	jle	.LBB23_33
.LBB23_29:                              # %if.then23
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rcx, %rcx
	je	.LBB23_32
# BB#30:                                # %if.then23
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rax, %rax
	jne	.LBB23_31
	jmp	.LBB23_32
.LBB23_23:                              # %if.then3
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_24
	jmp	.LBB23_32
.LBB23_33:                              # %if.else24
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%r9, %r9
	je	.LBB23_417
# BB#34:                                # %land.lhs.true27
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	16(%r9), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$116, %rcx
	jne	.LBB23_35
# BB#37:                                # %land.lhs.true51
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	144(%r9), %rcx
	jmp	.LBB23_38
.LBB23_35:                              # %land.lhs.true27
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$100, %rcx
	jne	.LBB23_417
# BB#36:                                # %land.lhs.true36
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	80(%r9), %rcx
.LBB23_38:                              # %land.lhs.true51
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rcx, %rcx
	je	.LBB23_417
# BB#39:                                # %land.lhs.true55
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	16(%rcx), %edx
	cmpq	$30, %rdx
	jne	.LBB23_417
# BB#40:                                # %if.then62
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	36(%rcx), %ecx
	movl	decl_die_table_in_use(%rip), %edx
	cmpq	%rcx, %rdx
	jbe	.LBB23_42
# BB#41:                                # %lookup_decl_die.exit193
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	decl_die_table(%rip), %rdx
	movq	(%rdx,%rcx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB23_42
.LBB23_24:                              # %if.then3
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rax, %rax
	je	.LBB23_32
# BB#25:                                # %if.then.i
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	%rax, %rcx
	je	.LBB23_26
.LBB23_31:                              # %if.end.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rcx, 24(%rax)
	movq	32(%rcx), %rdx
	movq	%rdx, 40(%rax)
	movq	%rax, 32(%rcx)
	jmp	.LBB23_32
.LBB23_16:                              # %if.then15
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rax, %rax
	je	.LBB23_32
# BB#17:                                # %if.then.i170
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	%rax, %rsi
	je	.LBB23_26
# BB#18:                                # %if.end.i175
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rsi, 24(%rax)
	movq	32(%rsi), %rcx
	movq	%rcx, 40(%rax)
	movq	%rax, 32(%rsi)
	callq	free
	jmp	.LBB23_1
.LBB23_43:                              # %for.end
	movq	incomplete_types(%rip), %rax
	movq	$0, limbo_die_list(%rip)
	movq	8(%rax), %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	addq	%r14, %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB23_47
# BB#44:                                # %for.body.lr.ph.i207
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	32(%rax,%r15,8), %rdi
	movq	comp_unit_die(%rip), %rsi
	callq	gen_type_die
	testq	%r15, %r15
	jle	.LBB23_47
# BB#45:                                # %for.body.for.body_crit_edge.i.preheader
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	.align	16, 0x90
.LBB23_46:                              # %for.body.for.body_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	incomplete_types(%rip), %rax
	movq	32(%rax,%rbx,8), %rdi
	movq	comp_unit_die(%rip), %rsi
	callq	gen_type_die
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB23_46
.LBB23_47:                              # %retry_incomplete_types.exit
	movq	comp_unit_die(%rip), %rdi
	callq	reverse_all_dies
	movl	flag_eliminate_dwarf2_dups(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_69
# BB#48:                                # %if.then74
	movq	comp_unit_die(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB23_66
# BB#49:
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	32(%rcx), %r13
	xorq	%r12, %r12
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	movabsq	$37398439002134, %rdi   # imm = 0x220380AC0016
	movabsq	$9007474132746240, %r9  # imm = 0x20004000018000
	jmp	.LBB23_50
	.align	16, 0x90
.LBB23_99:                              # %if.then12.i
                                        #   in Loop: Header=BB23_50 Depth=1
	movq	40(%r12), %r15
	movq	$0, 40(%r12)
.LBB23_98:                              # %for.cond.outer.backedge.i
                                        #   in Loop: Header=BB23_50 Depth=1
	movq	%rbx, %rdi
	callq	remove_children
	movq	%rbx, %rdi
	callq	free
	movq	(%r13), %rax
	testq	%rax, %rax
	movq	%r15, %r12
	movabsq	$4294967294, %rdi       # imm = 0xFFFFFFFE
	movq	%rdi, %r8
	movabsq	$37398439002134, %rdi   # imm = 0x220380AC0016
	movabsq	$9007474132746240, %r9  # imm = 0x20004000018000
	jne	.LBB23_50
	jmp	.LBB23_66
.LBB23_84:                              #   in Loop: Header=BB23_50 Depth=1
	movq	%rbx, %rax
.LBB23_85:                              # %tailrecurse.i.i.i45.i
                                        #   Parent Loop BB23_50 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_87 Depth 3
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB23_97
# BB#86:                                # %if.then.i.i.i49.i
                                        #   in Loop: Header=BB23_85 Depth=2
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rdi
	je	.LBB23_97
	.align	16, 0x90
.LBB23_87:                              # %for.body.i.i.i53.i
                                        #   Parent Loop BB23_50 Depth=1
                                        #     Parent Loop BB23_85 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB23_90
# BB#88:                                # %for.body.i.i.i53.i
                                        #   in Loop: Header=BB23_87 Depth=3
	cmpq	$3, %rdx
	je	.LBB23_94
# BB#89:                                # %for.body.i.i.i53.i
                                        #   in Loop: Header=BB23_87 Depth=3
	cmpq	$71, %rdx
	jne	.LBB23_92
.LBB23_90:                              # %land.lhs.true.i.i.i.i57.i
                                        #   in Loop: Header=BB23_87 Depth=3
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB23_20
# BB#91:                                # %AT_ref.exit.i.i.i61.i
                                        #   in Loop: Header=BB23_87 Depth=3
	movq	24(%rcx), %rax
.LBB23_92:                              # %for.inc.i.i.i66.i
                                        #   in Loop: Header=BB23_87 Depth=3
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_87
# BB#93:                                # %for.end.i.i.i68.i
                                        #   in Loop: Header=BB23_85 Depth=2
	xorq	%rdi, %rdi
	testq	%rax, %rax
	jne	.LBB23_85
	jmp	.LBB23_97
.LBB23_94:                              # %land.lhs.true.i.i.i71.i
                                        #   in Loop: Header=BB23_50 Depth=1
	movl	16(%rcx), %eax
	cmpq	$14, %rax
	jne	.LBB23_95
# BB#96:                                # %AT_string.exit.i.i.i
                                        #   in Loop: Header=BB23_50 Depth=1
	movq	24(%rcx), %rax
	movq	8(%rax), %rdi
.LBB23_97:                              # %push_new_compile_unit.exit.i
                                        #   in Loop: Header=BB23_50 Depth=1
	callq	gen_compile_unit_die
	movq	%rax, %r15
	movq	%r12, 40(%r15)
	jmp	.LBB23_98
	.align	16, 0x90
.LBB23_50:                              # %for.body.lr.ph.i213
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_51 Depth 2
                                        #       Child Loop BB23_53 Depth 3
                                        #         Child Loop BB23_56 Depth 4
                                        #           Child Loop BB23_58 Depth 5
                                        #     Child Loop BB23_85 Depth 2
                                        #       Child Loop BB23_87 Depth 3
	movq	%rax, %rbx
.LBB23_51:                              # %for.body.i214
                                        #   Parent Loop BB23_50 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_53 Depth 3
                                        #         Child Loop BB23_56 Depth 4
                                        #           Child Loop BB23_58 Depth 5
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	$16644, %rcx            # imm = 0x4104
	je	.LBB23_82
# BB#52:                                # %lor.lhs.false3.i
                                        #   in Loop: Header=BB23_51 Depth=2
	testq	%r12, %r12
	movq	%rax, %rcx
	movq	%rbx, %rdx
	jne	.LBB23_53
	jmp	.LBB23_65
	.align	16, 0x90
.LBB23_81:                              # %get_AT_ref.exit.i.tailrecurse.i_crit_edge.i
                                        #   in Loop: Header=BB23_53 Depth=3
	movl	(%rdx), %ecx
.LBB23_53:                              # %tailrecurse.i.i215
                                        #   Parent Loop BB23_50 Depth=1
                                        #     Parent Loop BB23_51 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB23_56 Depth 4
                                        #           Child Loop BB23_58 Depth 5
	andq	%r14, %rcx
	cmpq	$53, %rcx
	ja	.LBB23_65
# BB#54:                                # %tailrecurse.i.i215
                                        #   in Loop: Header=BB23_53 Depth=3
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	jne	.LBB23_82
# BB#55:                                # %tailrecurse.i.i215
                                        #   in Loop: Header=BB23_53 Depth=3
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%r9, %rsi
	je	.LBB23_65
.LBB23_56:                              # %tailrecurse.i.i.i.i
                                        #   Parent Loop BB23_50 Depth=1
                                        #     Parent Loop BB23_51 Depth=2
                                        #       Parent Loop BB23_53 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB23_58 Depth 5
	testq	%rdx, %rdx
	je	.LBB23_65
# BB#57:                                # %if.then.i.i.i.i
                                        #   in Loop: Header=BB23_56 Depth=4
	movq	16(%rdx), %rcx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB23_65
	.align	16, 0x90
.LBB23_58:                              # %for.body.i.i.i.i
                                        #   Parent Loop BB23_50 Depth=1
                                        #     Parent Loop BB23_51 Depth=2
                                        #       Parent Loop BB23_53 Depth=3
                                        #         Parent Loop BB23_56 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movl	(%rcx), %esi
	cmpq	$49, %rsi
	je	.LBB23_61
# BB#59:                                # %for.body.i.i.i.i
                                        #   in Loop: Header=BB23_58 Depth=5
	cmpq	$73, %rsi
	je	.LBB23_79
# BB#60:                                # %for.body.i.i.i.i
                                        #   in Loop: Header=BB23_58 Depth=5
	cmpq	$71, %rsi
	jne	.LBB23_63
.LBB23_61:                              # %land.lhs.true.i.i.i.i.i
                                        #   in Loop: Header=BB23_58 Depth=5
	movl	16(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB23_416
# BB#62:                                # %AT_ref.exit.i.i.i.i
                                        #   in Loop: Header=BB23_58 Depth=5
	movq	24(%rcx), %rdx
.LBB23_63:                              # %for.inc.i.i.i.i
                                        #   in Loop: Header=BB23_58 Depth=5
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_58
# BB#64:                                # %for.end.i.i.i.i
                                        #   in Loop: Header=BB23_56 Depth=4
	testq	%rdx, %rdx
	jne	.LBB23_56
	jmp	.LBB23_65
	.align	16, 0x90
.LBB23_79:                              # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB23_53 Depth=3
	movl	16(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB23_20
# BB#80:                                # %get_AT_ref.exit.i.i
                                        #   in Loop: Header=BB23_53 Depth=3
	movq	24(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.LBB23_81
	jmp	.LBB23_65
	.align	16, 0x90
.LBB23_82:                              # %if.then.i218
                                        #   in Loop: Header=BB23_51 Depth=2
	movq	40(%rbx), %rcx
	movq	%rcx, (%r13)
	cmpq	$16645, %rax            # imm = 0x4105
	je	.LBB23_99
# BB#83:                                # %if.then.i218
                                        #   in Loop: Header=BB23_51 Depth=2
	cmpq	$16644, %rax            # imm = 0x4104
	je	.LBB23_84
# BB#100:                               # %if.else14.i
                                        #   in Loop: Header=BB23_51 Depth=2
	testq	%r12, %r12
	je	.LBB23_102
# BB#101:                               # %if.else14.i
                                        #   in Loop: Header=BB23_51 Depth=2
	testq	%rbx, %rbx
	je	.LBB23_102
# BB#103:                               # %if.then.i.i222
                                        #   in Loop: Header=BB23_51 Depth=2
	cmpq	%rbx, %r12
	je	.LBB23_26
# BB#104:                               # %if.end.i.i224
                                        #   in Loop: Header=BB23_51 Depth=2
	movq	%r12, 24(%rbx)
	movq	32(%r12), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r12)
	movq	(%r13), %rcx
.LBB23_102:                             # %for.cond.backedge.i
                                        #   in Loop: Header=BB23_51 Depth=2
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	jne	.LBB23_51
	jmp	.LBB23_66
	.align	16, 0x90
.LBB23_65:                              # %if.else16.i
                                        #   in Loop: Header=BB23_50 Depth=1
	movq	40(%rbx), %rax
	addq	$40, %rbx
	testq	%rax, %rax
	movq	%rbx, %r13
	jne	.LBB23_50
.LBB23_66:                              # %for.end.i
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	assign_symbol_names
	movq	limbo_die_list(%rip), %rbx
	jmp	.LBB23_68
	.align	16, 0x90
.LBB23_67:                              # %for.body21.i
                                        #   in Loop: Header=BB23_68 Depth=1
	movq	(%rbx), %rdi
	callq	compute_section_prefix
	movq	(%rbx), %rdi
	callq	assign_symbol_names
	movq	16(%rbx), %rbx
.LBB23_68:                              # %for.body21.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB23_67
.LBB23_69:                              # %if.end75
	movq	comp_unit_die(%rip), %rdi
	callq	add_sibling_attributes
	movq	limbo_die_list(%rip), %rbx
	jmp	.LBB23_71
	.align	16, 0x90
.LBB23_70:                              # %for.body78
                                        #   in Loop: Header=BB23_71 Depth=1
	movq	(%rbx), %rdi
	callq	add_sibling_attributes
	movq	16(%rbx), %rbx
.LBB23_71:                              # %for.body78
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB23_70
# BB#72:                                # %for.end82
	callq	text_section
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str8, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	callq	fprintf
	movl	separate_line_info_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_73
# BB#105:                               # %if.else94
	movl	ranges_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_109
# BB#106:                               # %if.then98
	movq	comp_unit_die(%rip), %rbx
	movq	const_int_rtx+512(%rip), %r15
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%rbx, %rbx
	movq	$0, 8(%rax)
	movq	$82, %rdx
	movl	%edx, (%rax)
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	je	.LBB23_109
# BB#107:                               # %if.then98
	testq	%rax, %rax
	je	.LBB23_109
# BB#108:                               # %if.then.i.i255
	movq	16(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rbx)
	jmp	.LBB23_109
.LBB23_73:                              # %if.then93
	movq	comp_unit_die(%rip), %r12
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$17, %rax
	movl	%eax, (%rbx)
	movq	$12, %r15
	movl	%r15d, 16(%rbx)
	movabsq	$text_section_label, %rdi
	callq	xstrdup
	testq	%r12, %r12
	movq	%rax, 24(%rbx)
	je	.LBB23_76
# BB#74:                                # %if.then93
	testq	%rbx, %rbx
	je	.LBB23_76
# BB#75:                                # %if.then.i.i230
	movq	16(%r12), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r12)
.LBB23_76:                              # %add_AT_lbl_id.exit
	movq	comp_unit_die(%rip), %r12
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$18, %rax
	movl	%eax, (%rbx)
	movl	%r15d, 16(%rbx)
	movabsq	$text_end_label, %rdi
	callq	xstrdup
	testq	%r12, %r12
	movq	%rax, 24(%rbx)
	je	.LBB23_109
# BB#77:                                # %add_AT_lbl_id.exit
	testq	%rbx, %rbx
	je	.LBB23_109
# BB#78:                                # %if.then.i.i243
	movq	16(%r12), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r12)
.LBB23_109:                             # %if.end100
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB23_117
# BB#110:                               # %if.then103
	movq	comp_unit_die(%rip), %r15
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$16, %rax
	movl	%eax, (%rbx)
	movq	$13, %rax
	movl	%eax, 16(%rbx)
	movabsq	$debug_line_section_label, %rdi
	callq	xstrdup
	testq	%r15, %r15
	movq	%rax, 24(%rbx)
	je	.LBB23_113
# BB#111:                               # %if.then103
	testq	%rbx, %rbx
	je	.LBB23_113
# BB#112:                               # %if.then.i.i268
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB23_113:                             # %if.end104
	movl	debug_info_level(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB23_117
# BB#114:                               # %if.then107
	movq	comp_unit_die(%rip), %r15
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$67, %rax
	movl	%eax, (%rbx)
	movq	$13, %rax
	movl	%eax, 16(%rbx)
	movabsq	$macinfo_section_label, %rdi
	callq	xstrdup
	testq	%r15, %r15
	movq	%rax, 24(%rbx)
	je	.LBB23_117
# BB#115:                               # %if.then107
	testq	%rbx, %rbx
	je	.LBB23_117
# BB#116:                               # %if.then.i.i281
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB23_117:                             # %for.cond109.preheader
	movq	limbo_die_list(%rip), %rbx
	jmp	.LBB23_119
	.align	16, 0x90
.LBB23_118:                             # %for.body111
                                        #   in Loop: Header=BB23_119 Depth=1
	movq	(%rbx), %rdi
	callq	output_comp_unit
	movq	16(%rbx), %rbx
.LBB23_119:                             # %for.body111
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB23_118
# BB#120:                               # %for.end115
	movq	comp_unit_die(%rip), %rdi
	callq	output_comp_unit
	movabsq	$.L.str11, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movl	abbrev_die_table_in_use(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB23_316
# BB#121:
	movq	$1, %r13
	movabsq	$.L.str37, %rbx
	movabsq	$.L.str59, %r15
	jmp	.LBB23_122
.LBB23_129:                             # %for.body.i285
                                        #   in Loop: Header=BB23_122 Depth=1
	jmpq	*.LJTI23_0(,%rax,8)
.LBB23_177:                             # %sw.bb49.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str204, %rdx
	jmp	.LBB23_183
.LBB23_178:                             # %sw.bb50.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str205, %rdx
	jmp	.LBB23_183
.LBB23_179:                             # %sw.bb51.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str206, %rdx
	jmp	.LBB23_183
.LBB23_180:                             # %sw.bb52.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str207, %rdx
	jmp	.LBB23_183
.LBB23_181:                             # %sw.bb53.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str208, %rdx
	jmp	.LBB23_183
.LBB23_131:                             # %sw.bb2.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str157, %rdx
	jmp	.LBB23_183
.LBB23_132:                             # %sw.bb3.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str158, %rdx
	jmp	.LBB23_183
.LBB23_133:                             # %sw.bb4.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str159, %rdx
	jmp	.LBB23_183
.LBB23_134:                             # %sw.bb5.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str160, %rdx
	jmp	.LBB23_183
.LBB23_135:                             # %sw.bb6.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str161, %rdx
	jmp	.LBB23_183
.LBB23_136:                             # %sw.bb7.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str162, %rdx
	jmp	.LBB23_183
.LBB23_137:                             # %sw.bb8.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str163, %rdx
	jmp	.LBB23_183
.LBB23_138:                             # %sw.bb9.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str164, %rdx
	jmp	.LBB23_183
.LBB23_139:                             # %sw.bb10.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str165, %rdx
	jmp	.LBB23_183
.LBB23_140:                             # %sw.bb11.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str166, %rdx
	jmp	.LBB23_183
.LBB23_141:                             # %sw.bb12.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str167, %rdx
	jmp	.LBB23_183
.LBB23_142:                             # %sw.bb13.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str168, %rdx
	jmp	.LBB23_183
.LBB23_143:                             # %sw.bb14.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str169, %rdx
	jmp	.LBB23_183
.LBB23_144:                             # %sw.bb15.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str170, %rdx
	jmp	.LBB23_183
.LBB23_145:                             # %sw.bb16.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str171, %rdx
	jmp	.LBB23_183
.LBB23_146:                             # %sw.bb17.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str172, %rdx
	jmp	.LBB23_183
.LBB23_147:                             # %sw.bb18.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str173, %rdx
	jmp	.LBB23_183
.LBB23_148:                             # %sw.bb19.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str174, %rdx
	jmp	.LBB23_183
.LBB23_149:                             # %sw.bb20.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str175, %rdx
	jmp	.LBB23_183
.LBB23_150:                             # %sw.bb21.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str176, %rdx
	jmp	.LBB23_183
.LBB23_151:                             # %sw.bb22.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str177, %rdx
	jmp	.LBB23_183
.LBB23_152:                             # %sw.bb23.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str178, %rdx
	jmp	.LBB23_183
.LBB23_153:                             # %sw.bb24.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str179, %rdx
	jmp	.LBB23_183
.LBB23_154:                             # %sw.bb25.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str180, %rdx
	jmp	.LBB23_183
.LBB23_155:                             # %sw.bb26.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str181, %rdx
	jmp	.LBB23_183
.LBB23_156:                             # %sw.bb27.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str182, %rdx
	jmp	.LBB23_183
.LBB23_157:                             # %sw.bb28.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str183, %rdx
	jmp	.LBB23_183
.LBB23_158:                             # %sw.bb29.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str184, %rdx
	jmp	.LBB23_183
.LBB23_159:                             # %sw.bb30.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str185, %rdx
	jmp	.LBB23_183
.LBB23_160:                             # %sw.bb31.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str186, %rdx
	jmp	.LBB23_183
.LBB23_161:                             # %sw.bb32.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str187, %rdx
	jmp	.LBB23_183
.LBB23_162:                             # %sw.bb33.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str188, %rdx
	jmp	.LBB23_183
.LBB23_163:                             # %sw.bb34.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str189, %rdx
	jmp	.LBB23_183
.LBB23_164:                             # %sw.bb35.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str190, %rdx
	jmp	.LBB23_183
.LBB23_165:                             # %sw.bb36.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str191, %rdx
	jmp	.LBB23_183
.LBB23_166:                             # %sw.bb37.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str192, %rdx
	jmp	.LBB23_183
.LBB23_167:                             # %sw.bb38.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str193, %rdx
	jmp	.LBB23_183
.LBB23_168:                             # %sw.bb39.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str194, %rdx
	jmp	.LBB23_183
.LBB23_169:                             # %sw.bb40.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str195, %rdx
	jmp	.LBB23_183
.LBB23_170:                             # %sw.bb41.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str196, %rdx
	jmp	.LBB23_183
.LBB23_171:                             # %sw.bb42.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str197, %rdx
	jmp	.LBB23_183
.LBB23_172:                             # %sw.bb43.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str198, %rdx
	jmp	.LBB23_183
.LBB23_173:                             # %sw.bb44.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str199, %rdx
	jmp	.LBB23_183
.LBB23_174:                             # %sw.bb45.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str200, %rdx
	jmp	.LBB23_183
.LBB23_175:                             # %sw.bb46.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str201, %rdx
	jmp	.LBB23_183
.LBB23_176:                             # %sw.bb47.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str202, %rdx
	jmp	.LBB23_183
	.align	16, 0x90
.LBB23_122:                             # %for.body.i285
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_187 Depth 2
	movq	abbrev_die_table(%rip), %rax
	movq	(%rax,%r13,8), %r12
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str33, %rsi
	callq	dw2_asm_output_data_uleb128
	movslq	(%r12), %rax
	movq	%rax, %rdi
	andq	%r14, %rdi
	cmpq	$16512, %rax            # imm = 0x4080
	jg	.LBB23_125
# BB#123:                               # %for.body.i285
                                        #   in Loop: Header=BB23_122 Depth=1
	cmpq	$53, %rdi
	ja	.LBB23_182
# BB#124:                               # %for.body.i285
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str155, %rdx
	jmpq	*.LJTI23_1(,%rdi,8)
.LBB23_130:                             # %sw.bb1.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str156, %rdx
	jmp	.LBB23_183
	.align	16, 0x90
.LBB23_125:                             # %for.body.i285
                                        #   in Loop: Header=BB23_122 Depth=1
	cmpq	$16640, %rax            # imm = 0x4100
	jg	.LBB23_128
# BB#126:                               # %for.body.i285
                                        #   in Loop: Header=BB23_122 Depth=1
	cmpq	$16513, %rdi            # imm = 0x4081
	jne	.LBB23_182
# BB#127:                               # %sw.bb48.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str203, %rdx
	jmp	.LBB23_183
	.align	16, 0x90
.LBB23_128:                             # %for.body.i285
                                        #   in Loop: Header=BB23_122 Depth=1
	addq	$-16641, %rax           # imm = 0xFFFFFFFFFFFFBEFF
	andq	%r14, %rax
	cmpq	$4, %rax
	jbe	.LBB23_129
.LBB23_182:                             # %sw.default.i.i
                                        #   in Loop: Header=BB23_122 Depth=1
	movabsq	$.L.str209, %rdx
.LBB23_183:                             # %dwarf_tag_name.exit.i
                                        #   in Loop: Header=BB23_122 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str34, %rsi
	callq	dw2_asm_output_data_uleb128
	cmpq	$0, 32(%r12)
	jne	.LBB23_184
# BB#185:                               # %if.else.i289
                                        #   in Loop: Header=BB23_122 Depth=1
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movabsq	$.L.str36, %rdx
	jmp	.LBB23_186
	.align	16, 0x90
.LBB23_184:                             # %if.then.i288
                                        #   in Loop: Header=BB23_122 Depth=1
	movq	$1, %rdi
	movq	$1, %rsi
	xorq	%rax, %rax
	movabsq	$.L.str35, %rdx
.LBB23_186:                             # %if.end.i290
                                        #   in Loop: Header=BB23_122 Depth=1
	callq	dw2_asm_output_data
	movq	16(%r12), %r12
	jmp	.LBB23_187
	.align	16, 0x90
.LBB23_314:                             # %output_value_format.exit.i
                                        #   in Loop: Header=BB23_187 Depth=2
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	dw2_asm_output_data_uleb128
	movq	8(%r12), %r12
.LBB23_187:                             # %if.end.i290
                                        #   Parent Loop BB23_122 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r12, %r12
	je	.LBB23_315
# BB#188:                               # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movslq	(%r12), %rax
	movq	%rax, %rdi
	andq	%r14, %rdi
	cmpq	$8192, %rax             # imm = 0x2000
	jg	.LBB23_191
# BB#189:                               # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	decq	%rax
	andq	%r14, %rax
	cmpq	$88, %rax
	ja	.LBB23_290
# BB#190:                               # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str61, %rdx
	jmpq	*.LJTI23_4(,%rax,8)
.LBB23_197:                             # %sw.bb1.i25.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str62, %rdx
	jmp	.LBB23_291
	.align	16, 0x90
.LBB23_191:                             # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	cmpq	$8704, %rax             # imm = 0x2200
	jg	.LBB23_288
# BB#192:                               # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	cmpq	$8448, %rax             # imm = 0x2100
	jg	.LBB23_195
# BB#193:                               # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	addq	$-8193, %rax            # imm = 0xFFFFFFFFFFFFDFFF
	andq	%r14, %rax
	cmpq	$10, %rax
	ja	.LBB23_290
# BB#194:                               # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	jmpq	*.LJTI23_3(,%rax,8)
.LBB23_270:                             # %sw.bb74.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str135, %rdx
	jmp	.LBB23_291
.LBB23_288:                             # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	cmpq	$8705, %rdi             # imm = 0x2201
	jne	.LBB23_290
# BB#289:                               # %sw.bb92.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str153, %rdx
	jmp	.LBB23_291
.LBB23_195:                             # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	addq	$-8449, %rax            # imm = 0xFFFFFFFFFFFFDEFF
	andq	%r14, %rax
	cmpq	$6, %rax
	jbe	.LBB23_196
	.align	16, 0x90
.LBB23_290:                             # %sw.default.i78.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str154, %rdx
.LBB23_291:                             # %dwarf_attr_name.exit.i
                                        #   in Loop: Header=BB23_187 Depth=2
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	dw2_asm_output_data_uleb128
	movq	%r12, %rdi
	callq	value_format
	movq	%rax, %rdi
	andq	%r14, %rdi
	decq	%rax
	andq	%r14, %rax
	cmpq	$21, %rax
	ja	.LBB23_313
# BB#292:                               # %dwarf_attr_name.exit.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str38, %rdx
	jmpq	*.LJTI23_5(,%rax,8)
	.align	16, 0x90
.LBB23_313:                             # %sw.default.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movq	%r15, %rdx
	jmp	.LBB23_314
.LBB23_196:                             # %for.body9.i
                                        #   in Loop: Header=BB23_187 Depth=2
	jmpq	*.LJTI23_2(,%rax,8)
.LBB23_281:                             # %sw.bb85.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str146, %rdx
	jmp	.LBB23_291
.LBB23_293:                             # %sw.bb1.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str39, %rdx
	jmp	.LBB23_314
.LBB23_294:                             # %sw.bb2.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str40, %rdx
	jmp	.LBB23_314
.LBB23_295:                             # %sw.bb3.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str41, %rdx
	jmp	.LBB23_314
.LBB23_296:                             # %sw.bb4.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str42, %rdx
	jmp	.LBB23_314
.LBB23_297:                             # %sw.bb5.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str43, %rdx
	jmp	.LBB23_314
.LBB23_298:                             # %sw.bb6.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str44, %rdx
	jmp	.LBB23_314
.LBB23_299:                             # %sw.bb7.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str45, %rdx
	jmp	.LBB23_314
.LBB23_300:                             # %sw.bb8.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str46, %rdx
	jmp	.LBB23_314
.LBB23_301:                             # %sw.bb9.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str47, %rdx
	jmp	.LBB23_314
.LBB23_302:                             # %sw.bb10.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str48, %rdx
	jmp	.LBB23_314
.LBB23_303:                             # %sw.bb11.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str49, %rdx
	jmp	.LBB23_314
.LBB23_304:                             # %sw.bb12.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str50, %rdx
	jmp	.LBB23_314
.LBB23_305:                             # %sw.bb13.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str51, %rdx
	jmp	.LBB23_314
.LBB23_306:                             # %sw.bb14.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str52, %rdx
	jmp	.LBB23_314
.LBB23_307:                             # %sw.bb15.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str53, %rdx
	jmp	.LBB23_314
.LBB23_308:                             # %sw.bb16.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str54, %rdx
	jmp	.LBB23_314
.LBB23_309:                             # %sw.bb17.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str55, %rdx
	jmp	.LBB23_314
.LBB23_310:                             # %sw.bb18.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str56, %rdx
	jmp	.LBB23_314
.LBB23_311:                             # %sw.bb19.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str57, %rdx
	jmp	.LBB23_314
.LBB23_312:                             # %sw.bb20.i.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str58, %rdx
	jmp	.LBB23_314
.LBB23_282:                             # %sw.bb86.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str147, %rdx
	jmp	.LBB23_291
.LBB23_283:                             # %sw.bb87.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str148, %rdx
	jmp	.LBB23_291
.LBB23_284:                             # %sw.bb88.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str149, %rdx
	jmp	.LBB23_291
.LBB23_285:                             # %sw.bb89.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str150, %rdx
	jmp	.LBB23_291
.LBB23_286:                             # %sw.bb90.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str151, %rdx
	jmp	.LBB23_291
.LBB23_287:                             # %sw.bb91.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str152, %rdx
	jmp	.LBB23_291
.LBB23_271:                             # %sw.bb75.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str136, %rdx
	jmp	.LBB23_291
.LBB23_272:                             # %sw.bb76.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str137, %rdx
	jmp	.LBB23_291
.LBB23_273:                             # %sw.bb77.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str138, %rdx
	jmp	.LBB23_291
.LBB23_274:                             # %sw.bb78.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str139, %rdx
	jmp	.LBB23_291
.LBB23_275:                             # %sw.bb79.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str140, %rdx
	jmp	.LBB23_291
.LBB23_276:                             # %sw.bb80.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str141, %rdx
	jmp	.LBB23_291
.LBB23_277:                             # %sw.bb81.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str142, %rdx
	jmp	.LBB23_291
.LBB23_278:                             # %sw.bb82.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str143, %rdx
	jmp	.LBB23_291
.LBB23_279:                             # %sw.bb83.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str144, %rdx
	jmp	.LBB23_291
.LBB23_280:                             # %sw.bb84.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str145, %rdx
	jmp	.LBB23_291
.LBB23_198:                             # %sw.bb2.i26.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str63, %rdx
	jmp	.LBB23_291
.LBB23_199:                             # %sw.bb3.i27.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str64, %rdx
	jmp	.LBB23_291
.LBB23_200:                             # %sw.bb4.i28.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str65, %rdx
	jmp	.LBB23_291
.LBB23_201:                             # %sw.bb5.i29.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str66, %rdx
	jmp	.LBB23_291
.LBB23_202:                             # %sw.bb6.i30.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str67, %rdx
	jmp	.LBB23_291
.LBB23_203:                             # %sw.bb7.i31.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str68, %rdx
	jmp	.LBB23_291
.LBB23_204:                             # %sw.bb8.i32.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str69, %rdx
	jmp	.LBB23_291
.LBB23_205:                             # %sw.bb9.i33.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str70, %rdx
	jmp	.LBB23_291
.LBB23_206:                             # %sw.bb10.i34.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str71, %rdx
	jmp	.LBB23_291
.LBB23_207:                             # %sw.bb11.i35.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str72, %rdx
	jmp	.LBB23_291
.LBB23_208:                             # %sw.bb12.i36.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str73, %rdx
	jmp	.LBB23_291
.LBB23_209:                             # %sw.bb13.i37.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str74, %rdx
	jmp	.LBB23_291
.LBB23_210:                             # %sw.bb14.i38.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str75, %rdx
	jmp	.LBB23_291
.LBB23_211:                             # %sw.bb15.i39.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str76, %rdx
	jmp	.LBB23_291
.LBB23_212:                             # %sw.bb16.i40.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str77, %rdx
	jmp	.LBB23_291
.LBB23_213:                             # %sw.bb17.i41.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str78, %rdx
	jmp	.LBB23_291
.LBB23_214:                             # %sw.bb18.i42.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str79, %rdx
	jmp	.LBB23_291
.LBB23_215:                             # %sw.bb19.i43.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str80, %rdx
	jmp	.LBB23_291
.LBB23_216:                             # %sw.bb20.i44.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str81, %rdx
	jmp	.LBB23_291
.LBB23_217:                             # %sw.bb21.i45.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str82, %rdx
	jmp	.LBB23_291
.LBB23_218:                             # %sw.bb22.i46.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str83, %rdx
	jmp	.LBB23_291
.LBB23_219:                             # %sw.bb23.i47.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str84, %rdx
	jmp	.LBB23_291
.LBB23_220:                             # %sw.bb24.i48.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str85, %rdx
	jmp	.LBB23_291
.LBB23_221:                             # %sw.bb25.i49.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str86, %rdx
	jmp	.LBB23_291
.LBB23_222:                             # %sw.bb26.i50.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str87, %rdx
	jmp	.LBB23_291
.LBB23_223:                             # %sw.bb27.i51.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str88, %rdx
	jmp	.LBB23_291
.LBB23_224:                             # %sw.bb28.i52.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str89, %rdx
	jmp	.LBB23_291
.LBB23_225:                             # %sw.bb29.i53.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str90, %rdx
	jmp	.LBB23_291
.LBB23_226:                             # %sw.bb30.i54.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str91, %rdx
	jmp	.LBB23_291
.LBB23_227:                             # %sw.bb31.i55.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str92, %rdx
	jmp	.LBB23_291
.LBB23_228:                             # %sw.bb32.i56.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str93, %rdx
	jmp	.LBB23_291
.LBB23_229:                             # %sw.bb33.i57.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str94, %rdx
	jmp	.LBB23_291
.LBB23_230:                             # %sw.bb34.i58.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str95, %rdx
	jmp	.LBB23_291
.LBB23_231:                             # %sw.bb35.i59.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str96, %rdx
	jmp	.LBB23_291
.LBB23_232:                             # %sw.bb36.i60.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str97, %rdx
	jmp	.LBB23_291
.LBB23_233:                             # %sw.bb37.i61.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str98, %rdx
	jmp	.LBB23_291
.LBB23_234:                             # %sw.bb38.i62.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str99, %rdx
	jmp	.LBB23_291
.LBB23_235:                             # %sw.bb39.i63.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str100, %rdx
	jmp	.LBB23_291
.LBB23_236:                             # %sw.bb40.i64.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str101, %rdx
	jmp	.LBB23_291
.LBB23_237:                             # %sw.bb41.i65.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str102, %rdx
	jmp	.LBB23_291
.LBB23_238:                             # %sw.bb42.i66.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str103, %rdx
	jmp	.LBB23_291
.LBB23_239:                             # %sw.bb43.i67.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str104, %rdx
	jmp	.LBB23_291
.LBB23_240:                             # %sw.bb44.i68.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str105, %rdx
	jmp	.LBB23_291
.LBB23_241:                             # %sw.bb45.i69.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str106, %rdx
	jmp	.LBB23_291
.LBB23_242:                             # %sw.bb46.i70.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str107, %rdx
	jmp	.LBB23_291
.LBB23_243:                             # %sw.bb47.i71.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str108, %rdx
	jmp	.LBB23_291
.LBB23_244:                             # %sw.bb48.i72.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str109, %rdx
	jmp	.LBB23_291
.LBB23_245:                             # %sw.bb49.i73.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str110, %rdx
	jmp	.LBB23_291
.LBB23_246:                             # %sw.bb50.i74.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str111, %rdx
	jmp	.LBB23_291
.LBB23_247:                             # %sw.bb51.i75.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str112, %rdx
	jmp	.LBB23_291
.LBB23_248:                             # %sw.bb52.i76.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str113, %rdx
	jmp	.LBB23_291
.LBB23_249:                             # %sw.bb53.i77.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str114, %rdx
	jmp	.LBB23_291
.LBB23_250:                             # %sw.bb54.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str115, %rdx
	jmp	.LBB23_291
.LBB23_251:                             # %sw.bb55.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str116, %rdx
	jmp	.LBB23_291
.LBB23_252:                             # %sw.bb56.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str117, %rdx
	jmp	.LBB23_291
.LBB23_253:                             # %sw.bb57.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str118, %rdx
	jmp	.LBB23_291
.LBB23_254:                             # %sw.bb58.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str119, %rdx
	jmp	.LBB23_291
.LBB23_255:                             # %sw.bb59.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str120, %rdx
	jmp	.LBB23_291
.LBB23_256:                             # %sw.bb60.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str121, %rdx
	jmp	.LBB23_291
.LBB23_257:                             # %sw.bb61.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str122, %rdx
	jmp	.LBB23_291
.LBB23_258:                             # %sw.bb62.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str123, %rdx
	jmp	.LBB23_291
.LBB23_259:                             # %sw.bb63.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str124, %rdx
	jmp	.LBB23_291
.LBB23_260:                             # %sw.bb64.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str125, %rdx
	jmp	.LBB23_291
.LBB23_261:                             # %sw.bb65.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str126, %rdx
	jmp	.LBB23_291
.LBB23_262:                             # %sw.bb66.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str127, %rdx
	jmp	.LBB23_291
.LBB23_263:                             # %sw.bb67.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str128, %rdx
	jmp	.LBB23_291
.LBB23_264:                             # %sw.bb68.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str129, %rdx
	jmp	.LBB23_291
.LBB23_265:                             # %sw.bb69.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str130, %rdx
	jmp	.LBB23_291
.LBB23_266:                             # %sw.bb70.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str131, %rdx
	jmp	.LBB23_291
.LBB23_267:                             # %sw.bb71.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str132, %rdx
	jmp	.LBB23_291
.LBB23_268:                             # %sw.bb72.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str133, %rdx
	jmp	.LBB23_291
.LBB23_269:                             # %sw.bb73.i.i
                                        #   in Loop: Header=BB23_187 Depth=2
	movabsq	$.L.str134, %rdx
	jmp	.LBB23_291
	.align	16, 0x90
.LBB23_315:                             # %for.end.i296
                                        #   in Loop: Header=BB23_122 Depth=1
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	abbrev_die_table_in_use(%rip), %eax
	incq	%r13
	cmpq	%rax, %r13
	jb	.LBB23_122
.LBB23_316:                             # %output_abbrev_section.exit
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	pubname_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_327
# BB#317:                               # %if.then117
	movabsq	$.L.str12, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movl	pubname_table_in_use(%rip), %r15d
	testq	%r15, %r15
	movq	$14, %r13
	je	.LBB23_321
# BB#318:                               # %for.body.lr.ph.i.i
	movq	pubname_table(%rip), %r12
	addq	$8, %r12
	movq	$10, %r13
	movq	$1, %rbx
	.align	16, 0x90
.LBB23_319:                             # %for.body.i.i298
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rdi
	callq	strlen
	leaq	5(%r13,%rax), %r13
	movq	%rbx, %rax
	andq	%r14, %rax
	incq	%rbx
	addq	$16, %r12
	cmpq	%r15, %rax
	jne	.LBB23_319
# BB#320:                               # %for.cond.for.end_crit_edge.i.i
	addq	$4, %r13
.LBB23_321:                             # %size_of_pubnames.exit.i
	movabsq	$.L.str29, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r13, %rsi
	callq	dw2_asm_output_data
	movabsq	$.L.str22, %rdx
	movq	$2, %rdi
	movq	$2, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$debug_info_section_label, %rsi
	movabsq	$.L.str23, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_offset
	movq	next_die_offset(%rip), %rsi
	movabsq	$.L.str30, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	pubname_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_326
# BB#322:
	movabsq	$.L.str32, %r13
	.align	16, 0x90
.LBB23_323:                             # %for.body.i302
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rbx
	movq	pubname_table(%rip), %r15
	andq	%r14, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	(%r15,%rax), %rax
	movl	64(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB23_324
# BB#325:                               # %if.end.i306
                                        #   in Loop: Header=BB23_323 Depth=1
	movq	48(%rax), %rsi
	movq	$4, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str31, %rdx
	callq	dw2_asm_output_data
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	8(%r15,%rbx), %rdi
	movq	$-1, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdx
	callq	dw2_asm_output_nstring
	movl	pubname_table_in_use(%rip), %eax
	incq	%r12
	movq	%r12, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB23_323
.LBB23_326:                             # %output_pubnames.exit
	movq	$4, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
.LBB23_327:                             # %if.end118
	movl	fde_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_402
# BB#328:                               # %if.then120
	movabsq	$.L.str13, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	leaq	(%rsi,%rsi), %rdi
	leaq	11(%rsi,%rsi), %rax
	cqto
	idivq	%rdi
	movq	$-4, %rax
	subq	%rdx, %rax
	leaq	11(%rax,%rsi,2), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	arange_table_in_use(%rip), %edx
	sarq	%cl, %rax
	imulq	%rdi, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	leaq	(%rax,%rsi,4), %rsi
	addq	%rcx, %rsi
	movabsq	$.L.str21, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	xorq	%r12, %r12
	callq	dw2_asm_output_data
	movabsq	$.L.str22, %rdx
	movq	$2, %rdi
	movq	$2, %rsi
	xorq	%rax, %rax
	movq	$2, %r15
	callq	dw2_asm_output_data
	movabsq	$debug_info_section_label, %rsi
	movabsq	$.L.str23, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_offset
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movabsq	$.L.str24, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$.L.str25, %rdx
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	leaq	11(%rax,%rax), %rsi
	leaq	(%rax,%rax), %rcx
	movq	%rsi, %rax
	cqto
	idivq	%rcx
	subq	%rdx, %rsi
	andq	%r14, %rsi
	cmpq	$12, %rsi
	je	.LBB23_331
# BB#329:                               # %if.then.i314
	movabsq	$.L.str26, %rdx
	movq	$2, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	leaq	(%rsi,%rsi), %rcx
	leaq	11(%rsi,%rsi), %rax
	cqto
	idivq	%rcx
	movq	$-12, %rax
	subq	%rdx, %rax
	leaq	11(%rax,%rsi,2), %rax
	andq	%r14, %rax
	cmpq	$3, %rax
	jb	.LBB23_331
	.align	16, 0x90
.LBB23_330:                             # %for.body.i316
                                        # =>This Inner Loop Header: Depth=1
	movq	$2, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	addq	$2, %r15
	movq	%r15, %rsi
	movl	target_flags(%rip), %edi
	andq	%r14, %rsi
	movq	$20, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	andq	$32, %rbx
	addq	$32, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	leaq	(%rbx,%rbx), %rcx
	leaq	11(%rbx,%rbx), %rax
	cqto
	idivq	%rcx
	movq	$-12, %rax
	subq	%rdx, %rax
	leaq	11(%rax,%rbx,2), %rax
	andq	%r14, %rax
	cmpq	%rax, %rsi
	jb	.LBB23_330
.LBB23_331:                             # %if.end.i317
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	movabsq	$text_section_label, %r15
	movabsq	$.L.str27, %rdx
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	dw2_asm_output_addr
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	movabsq	$text_end_label, %rsi
	movabsq	$.L.str28, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	dw2_asm_output_delta
	movl	arange_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_401
# BB#332:
	movabsq	$.L.str27, %rbx
	movabsq	$4294967294, %r13       # imm = 0xFFFFFFFE
	.align	16, 0x90
.LBB23_333:                             # %for.body57.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_373 Depth 2
                                        #       Child Loop BB23_375 Depth 3
                                        #     Child Loop BB23_387 Depth 2
                                        #       Child Loop BB23_389 Depth 3
                                        #     Child Loop BB23_337 Depth 2
                                        #       Child Loop BB23_339 Depth 3
                                        #     Child Loop BB23_350 Depth 2
                                        #       Child Loop BB23_352 Depth 3
                                        #     Child Loop BB23_361 Depth 2
                                        #       Child Loop BB23_363 Depth 3
	movq	arange_table(%rip), %rax
	movq	%r12, %rcx
	andq	%r14, %rcx
	movq	(%rax,%rcx,8), %r15
	movl	64(%r15), %eax
	testq	%rax, %rax
	je	.LBB23_334
# BB#335:                               # %if.end61.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	(%r15), %eax
	cmpq	$46, %rax
	movq	%r15, %rax
	jne	.LBB23_373
# BB#336:                               # %if.then64.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	movq	%r15, %rax
.LBB23_337:                             # %tailrecurse.i.i124.i
                                        #   Parent Loop BB23_333 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_339 Depth 3
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB23_349
# BB#338:                               # %if.then.i.i128.i
                                        #   in Loop: Header=BB23_337 Depth=2
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rsi
	je	.LBB23_349
	.align	16, 0x90
.LBB23_339:                             # %for.body.i.i132.i
                                        #   Parent Loop BB23_333 Depth=1
                                        #     Parent Loop BB23_337 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB23_342
# BB#340:                               # %for.body.i.i132.i
                                        #   in Loop: Header=BB23_339 Depth=3
	cmpq	$17, %rdx
	je	.LBB23_346
# BB#341:                               # %for.body.i.i132.i
                                        #   in Loop: Header=BB23_339 Depth=3
	cmpq	$71, %rdx
	jne	.LBB23_344
.LBB23_342:                             # %land.lhs.true.i.i.i136.i
                                        #   in Loop: Header=BB23_339 Depth=3
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB23_20
# BB#343:                               # %AT_ref.exit.i.i140.i
                                        #   in Loop: Header=BB23_339 Depth=3
	movq	24(%rcx), %rax
.LBB23_344:                             # %for.inc.i.i145.i
                                        #   in Loop: Header=BB23_339 Depth=3
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_339
# BB#345:                               # %for.end.i.i147.i
                                        #   in Loop: Header=BB23_337 Depth=2
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jne	.LBB23_337
	jmp	.LBB23_349
	.align	16, 0x90
.LBB23_373:                             # %tailrecurse.i.i325
                                        #   Parent Loop BB23_333 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_375 Depth 3
	testq	%rax, %rax
	je	.LBB23_383
# BB#374:                               # %if.then.i.i329
                                        #   in Loop: Header=BB23_373 Depth=2
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB23_383
	.align	16, 0x90
.LBB23_375:                             # %for.body.i.i333
                                        #   Parent Loop BB23_333 Depth=1
                                        #     Parent Loop BB23_373 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB23_378
# BB#376:                               # %for.body.i.i333
                                        #   in Loop: Header=BB23_375 Depth=3
	cmpq	$2, %rdx
	je	.LBB23_382
# BB#377:                               # %for.body.i.i333
                                        #   in Loop: Header=BB23_375 Depth=3
	cmpq	$71, %rdx
	jne	.LBB23_380
.LBB23_378:                             # %land.lhs.true.i.i225.i
                                        #   in Loop: Header=BB23_375 Depth=3
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB23_20
# BB#379:                               # %AT_ref.exit.i.i335
                                        #   in Loop: Header=BB23_375 Depth=3
	movq	24(%rcx), %rax
.LBB23_380:                             # %for.inc.i.i340
                                        #   in Loop: Header=BB23_375 Depth=3
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_375
# BB#381:                               # %for.end.i.i342
                                        #   in Loop: Header=BB23_373 Depth=2
	testq	%rax, %rax
	jne	.LBB23_373
	jmp	.LBB23_383
	.align	16, 0x90
.LBB23_382:                             # %lor.lhs.false.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	16(%rcx), %eax
	cmpq	$2, %rax
	jne	.LBB23_383
# BB#384:                               # %AT_loc.exit.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movq	24(%rcx), %rax
	movl	8(%rax), %ecx
	cmpq	$3, %rcx
	jne	.LBB23_385
# BB#386:                               # %if.end87.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	movq	24(%rax), %rsi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	dw2_asm_output_addr_rtx
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
.LBB23_387:                             # %tailrecurse.i.i.i
                                        #   Parent Loop BB23_333 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_389 Depth 3
	xorq	%rsi, %rsi
	testq	%r15, %r15
	je	.LBB23_399
# BB#388:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB23_387 Depth=2
	movq	16(%r15), %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rsi
	je	.LBB23_399
	.align	16, 0x90
.LBB23_389:                             # %for.body.i.i.i
                                        #   Parent Loop BB23_333 Depth=1
                                        #     Parent Loop BB23_387 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rax), %ecx
	cmpq	$49, %rcx
	je	.LBB23_392
# BB#390:                               # %for.body.i.i.i
                                        #   in Loop: Header=BB23_389 Depth=3
	cmpq	$11, %rcx
	je	.LBB23_396
# BB#391:                               # %for.body.i.i.i
                                        #   in Loop: Header=BB23_389 Depth=3
	cmpq	$71, %rcx
	jne	.LBB23_394
.LBB23_392:                             # %land.lhs.true.i.i.i.i351
                                        #   in Loop: Header=BB23_389 Depth=3
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB23_20
# BB#393:                               # %AT_ref.exit.i.i.i
                                        #   in Loop: Header=BB23_389 Depth=3
	movq	24(%rax), %r15
.LBB23_394:                             # %for.inc.i.i.i
                                        #   in Loop: Header=BB23_389 Depth=3
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB23_389
# BB#395:                               # %for.end.i.i.i
                                        #   in Loop: Header=BB23_387 Depth=2
	xorq	%rsi, %rsi
	testq	%r15, %r15
	jne	.LBB23_387
	jmp	.LBB23_399
	.align	16, 0x90
.LBB23_346:                             # %land.lhs.true.i.i148.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	16(%rcx), %eax
	andq	%r13, %rax
	cmpq	$12, %rax
	jne	.LBB23_347
# BB#348:                               # %AT_lbl.exit.i.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movq	24(%rcx), %rsi
.LBB23_349:                             # %get_AT_low_pc.exit.i
                                        #   in Loop: Header=BB23_333 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	dw2_asm_output_addr
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	movq	%r15, %rax
.LBB23_350:                             # %tailrecurse.i.i153.i
                                        #   Parent Loop BB23_333 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_352 Depth 3
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB23_361
# BB#351:                               # %if.then.i.i157.i
                                        #   in Loop: Header=BB23_350 Depth=2
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rsi
	je	.LBB23_361
	.align	16, 0x90
.LBB23_352:                             # %for.body.i.i161.i
                                        #   Parent Loop BB23_333 Depth=1
                                        #     Parent Loop BB23_350 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB23_355
# BB#353:                               # %for.body.i.i161.i
                                        #   in Loop: Header=BB23_352 Depth=3
	cmpq	$18, %rdx
	je	.LBB23_359
# BB#354:                               # %for.body.i.i161.i
                                        #   in Loop: Header=BB23_352 Depth=3
	cmpq	$71, %rdx
	jne	.LBB23_357
.LBB23_355:                             # %land.lhs.true.i.i.i165.i
                                        #   in Loop: Header=BB23_352 Depth=3
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB23_20
# BB#356:                               # %AT_ref.exit.i.i169.i
                                        #   in Loop: Header=BB23_352 Depth=3
	movq	24(%rcx), %rax
.LBB23_357:                             # %for.inc.i.i174.i
                                        #   in Loop: Header=BB23_352 Depth=3
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_352
# BB#358:                               # %for.end.i.i176.i
                                        #   in Loop: Header=BB23_350 Depth=2
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jne	.LBB23_350
	jmp	.LBB23_361
	.align	16, 0x90
.LBB23_359:                             # %land.lhs.true.i.i180.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	16(%rcx), %eax
	andq	%r13, %rax
	cmpq	$12, %rax
	jne	.LBB23_347
# BB#360:                               # %AT_lbl.exit.i184.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movq	24(%rcx), %rsi
.LBB23_361:                             # %tailrecurse.i.i188.i
                                        #   Parent Loop BB23_333 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB23_363 Depth 3
	xorq	%rdx, %rdx
	testq	%r15, %r15
	je	.LBB23_372
# BB#362:                               # %if.then.i.i192.i
                                        #   in Loop: Header=BB23_361 Depth=2
	movq	16(%r15), %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB23_372
	.align	16, 0x90
.LBB23_363:                             # %for.body.i.i196.i
                                        #   Parent Loop BB23_333 Depth=1
                                        #     Parent Loop BB23_361 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rax), %ecx
	cmpq	$49, %rcx
	je	.LBB23_366
# BB#364:                               # %for.body.i.i196.i
                                        #   in Loop: Header=BB23_363 Depth=3
	cmpq	$17, %rcx
	je	.LBB23_370
# BB#365:                               # %for.body.i.i196.i
                                        #   in Loop: Header=BB23_363 Depth=3
	cmpq	$71, %rcx
	jne	.LBB23_368
.LBB23_366:                             # %land.lhs.true.i.i.i200.i
                                        #   in Loop: Header=BB23_363 Depth=3
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB23_20
# BB#367:                               # %AT_ref.exit.i.i204.i
                                        #   in Loop: Header=BB23_363 Depth=3
	movq	24(%rax), %r15
.LBB23_368:                             # %for.inc.i.i209.i
                                        #   in Loop: Header=BB23_363 Depth=3
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB23_363
# BB#369:                               # %for.end.i.i211.i
                                        #   in Loop: Header=BB23_361 Depth=2
	xorq	%rdx, %rdx
	testq	%r15, %r15
	jne	.LBB23_361
	jmp	.LBB23_372
	.align	16, 0x90
.LBB23_396:                             # %land.lhs.true.i.i.i357
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB23_397
# BB#398:                               # %AT_unsigned.exit.i.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	24(%rax), %esi
.LBB23_399:                             # %get_AT_unsigned.exit.i
                                        #   in Loop: Header=BB23_333 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str28, %rdx
	callq	dw2_asm_output_data
	jmp	.LBB23_400
	.align	16, 0x90
.LBB23_370:                             # %land.lhs.true.i.i215.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	16(%rax), %ecx
	andq	%r13, %rcx
	cmpq	$12, %rcx
	jne	.LBB23_347
# BB#371:                               # %AT_lbl.exit.i219.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movq	24(%rax), %rdx
.LBB23_372:                             # %get_AT_low_pc.exit221.i
                                        #   in Loop: Header=BB23_333 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str28, %rcx
	callq	dw2_asm_output_delta
.LBB23_400:                             # %for.inc99.i
                                        #   in Loop: Header=BB23_333 Depth=1
	movl	arange_table_in_use(%rip), %eax
	incq	%r12
	movq	%r12, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB23_333
.LBB23_401:                             # %output_aranges.exit
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
.LBB23_402:                             # %if.end121
	movl	ranges_table_in_use(%rip), %eax
	testq	%rax, %rax
	movq	%r14, %rbx
	je	.LBB23_411
# BB#403:                               # %if.then123
	movabsq	$.L.str14, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movq	asm_out_file(%rip), %rdi
	movabsq	$ranges_section_label, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	ranges_table_in_use(%rip), %eax
	testq	%rax, %rax
	je	.LBB23_411
# BB#404:
	xorq	%r13, %r13
	movabsq	$.L.str18, %r12
	leaq	64(%rsp), %r14
	.align	16, 0x90
.LBB23_405:                             # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	ranges_table(%rip), %rax
	movq	%r13, %rcx
	andq	%rbx, %rcx
	movl	(%rax,%rcx,4), %r15d
	testq	%r15, %r15
	jne	.LBB23_406
# BB#409:                               # %if.else36.i
                                        #   in Loop: Header=BB23_405 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$.L.str18, %r12
	jmp	.LBB23_410
	.align	16, 0x90
.LBB23_406:                             # %do.body.i
                                        #   in Loop: Header=BB23_405 Depth=1
	movq	%r12, 24(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str1, %rbx
	movq	%rbx, %rsi
	movabsq	$.L.str19, %rdx
	movq	%r15, %rcx
	callq	sprintf
	xorq	%rax, %rax
	leaq	32(%rsp), %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str20, %rdx
	movq	%r15, %rcx
	callq	sprintf
	movl	target_flags(%rip), %edi
	movl	separate_line_info_table_in_use(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	movq	%r13, %rbx
	imulq	%rdi, %rbx
	addq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB23_408
# BB#407:                               # %if.then7.i
                                        #   in Loop: Header=BB23_405 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %r15
	movabsq	$text_section_label, %r12
	movq	%r12, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	dw2_asm_output_delta
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	dw2_asm_output_delta
	xorq	%r12, %r12
	jmp	.LBB23_410
	.align	16, 0x90
.LBB23_408:                             # %if.else.i
                                        #   in Loop: Header=BB23_405 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	dw2_asm_output_addr
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	dw2_asm_output_addr
	xorq	%r12, %r12
.LBB23_410:                             # %for.inc.i
                                        #   in Loop: Header=BB23_405 Depth=1
	movl	ranges_table_in_use(%rip), %eax
	incq	%r13
	movq	%r13, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB23_405
.LBB23_411:                             # %if.end125
	movl	debug_info_level(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB23_413
# BB#412:                               # %if.then128
	movabsq	$.L.str15, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movabsq	$.L.str16, %rdx
	movq	$1, %rdi
	movq	$4, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
.LBB23_413:                             # %if.end129
	movq	debug_str_hash(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB23_415
# BB#414:                               # %if.then131
	movabsq	$output_indirect_string, %rsi
	xorq	%rdx, %rdx
	callq	ht_forall
.LBB23_415:                             # %if.end132
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_416:                             # %if.end.i.i.i
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.AT_ref, %rdx
	movq	$4588, %rsi             # imm = 0x11EC
	callq	fancy_abort
.LBB23_417:                             # %if.else66
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_finish, %rcx
	movq	$12184, %rsi            # imm = 0x2F98
	jmp	.LBB23_21
.LBB23_42:                              # %if.then65
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.dwarf2out_finish, %rcx
	movq	$12180, %rsi            # imm = 0x2F94
	jmp	.LBB23_21
.LBB23_95:                              # %if.end.i.i.i72.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_string, %rcx
	movq	$4518, %rsi             # imm = 0x11A6
	jmp	.LBB23_21
.LBB23_26:                              # %if.then3.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB23_21
.LBB23_324:                             # %if.then.i303
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_pubnames, %rcx
	movq	$6543, %rsi             # imm = 0x198F
	jmp	.LBB23_21
.LBB23_334:                             # %if.then60.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_aranges, %rcx
	movq	$6613, %rsi             # imm = 0x19D5
	jmp	.LBB23_21
.LBB23_383:                             # %if.then81.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_aranges, %rcx
	movq	$6631, %rsi             # imm = 0x19E7
	jmp	.LBB23_21
.LBB23_385:                             # %if.then86.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_aranges, %rcx
	movq	$6635, %rsi             # imm = 0x19EB
	jmp	.LBB23_21
.LBB23_20:                              # %if.end.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB23_21
.LBB23_397:                             # %if.end.i.i.i358
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
	jmp	.LBB23_21
.LBB23_347:                             # %if.end.i.i149.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_lbl, %rcx
	movq	$4785, %rsi             # imm = 0x12B1
.LBB23_21:                              # %if.end.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp272:
	.size	dwarf2out_finish, .Ltmp272-dwarf2out_finish
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI23_0:
	.quad	.LBB23_177
	.quad	.LBB23_178
	.quad	.LBB23_179
	.quad	.LBB23_180
	.quad	.LBB23_181
.LJTI23_1:
	.quad	.LBB23_183
	.quad	.LBB23_130
	.quad	.LBB23_131
	.quad	.LBB23_132
	.quad	.LBB23_133
	.quad	.LBB23_134
	.quad	.LBB23_182
	.quad	.LBB23_182
	.quad	.LBB23_135
	.quad	.LBB23_182
	.quad	.LBB23_136
	.quad	.LBB23_137
	.quad	.LBB23_182
	.quad	.LBB23_138
	.quad	.LBB23_182
	.quad	.LBB23_139
	.quad	.LBB23_140
	.quad	.LBB23_141
	.quad	.LBB23_142
	.quad	.LBB23_143
	.quad	.LBB23_182
	.quad	.LBB23_144
	.quad	.LBB23_145
	.quad	.LBB23_146
	.quad	.LBB23_147
	.quad	.LBB23_148
	.quad	.LBB23_149
	.quad	.LBB23_150
	.quad	.LBB23_151
	.quad	.LBB23_152
	.quad	.LBB23_153
	.quad	.LBB23_154
	.quad	.LBB23_155
	.quad	.LBB23_156
	.quad	.LBB23_157
	.quad	.LBB23_158
	.quad	.LBB23_159
	.quad	.LBB23_160
	.quad	.LBB23_161
	.quad	.LBB23_162
	.quad	.LBB23_163
	.quad	.LBB23_164
	.quad	.LBB23_165
	.quad	.LBB23_166
	.quad	.LBB23_167
	.quad	.LBB23_168
	.quad	.LBB23_169
	.quad	.LBB23_170
	.quad	.LBB23_171
	.quad	.LBB23_172
	.quad	.LBB23_173
	.quad	.LBB23_174
	.quad	.LBB23_175
	.quad	.LBB23_176
.LJTI23_2:
	.quad	.LBB23_281
	.quad	.LBB23_282
	.quad	.LBB23_283
	.quad	.LBB23_284
	.quad	.LBB23_285
	.quad	.LBB23_286
	.quad	.LBB23_287
.LJTI23_3:
	.quad	.LBB23_270
	.quad	.LBB23_271
	.quad	.LBB23_272
	.quad	.LBB23_273
	.quad	.LBB23_274
	.quad	.LBB23_275
	.quad	.LBB23_276
	.quad	.LBB23_277
	.quad	.LBB23_278
	.quad	.LBB23_279
	.quad	.LBB23_280
.LJTI23_4:
	.quad	.LBB23_291
	.quad	.LBB23_197
	.quad	.LBB23_198
	.quad	.LBB23_290
	.quad	.LBB23_290
	.quad	.LBB23_290
	.quad	.LBB23_290
	.quad	.LBB23_290
	.quad	.LBB23_199
	.quad	.LBB23_200
	.quad	.LBB23_201
	.quad	.LBB23_202
	.quad	.LBB23_203
	.quad	.LBB23_290
	.quad	.LBB23_204
	.quad	.LBB23_205
	.quad	.LBB23_206
	.quad	.LBB23_207
	.quad	.LBB23_208
	.quad	.LBB23_209
	.quad	.LBB23_210
	.quad	.LBB23_211
	.quad	.LBB23_212
	.quad	.LBB23_213
	.quad	.LBB23_214
	.quad	.LBB23_215
	.quad	.LBB23_216
	.quad	.LBB23_217
	.quad	.LBB23_218
	.quad	.LBB23_219
	.quad	.LBB23_290
	.quad	.LBB23_220
	.quad	.LBB23_221
	.quad	.LBB23_222
	.quad	.LBB23_290
	.quad	.LBB23_290
	.quad	.LBB23_223
	.quad	.LBB23_290
	.quad	.LBB23_224
	.quad	.LBB23_290
	.quad	.LBB23_290
	.quad	.LBB23_225
	.quad	.LBB23_290
	.quad	.LBB23_226
	.quad	.LBB23_290
	.quad	.LBB23_227
	.quad	.LBB23_228
	.quad	.LBB23_290
	.quad	.LBB23_229
	.quad	.LBB23_230
	.quad	.LBB23_231
	.quad	.LBB23_232
	.quad	.LBB23_233
	.quad	.LBB23_234
	.quad	.LBB23_235
	.quad	.LBB23_236
	.quad	.LBB23_237
	.quad	.LBB23_238
	.quad	.LBB23_239
	.quad	.LBB23_240
	.quad	.LBB23_241
	.quad	.LBB23_242
	.quad	.LBB23_243
	.quad	.LBB23_244
	.quad	.LBB23_245
	.quad	.LBB23_246
	.quad	.LBB23_247
	.quad	.LBB23_248
	.quad	.LBB23_249
	.quad	.LBB23_250
	.quad	.LBB23_251
	.quad	.LBB23_252
	.quad	.LBB23_253
	.quad	.LBB23_254
	.quad	.LBB23_255
	.quad	.LBB23_256
	.quad	.LBB23_257
	.quad	.LBB23_258
	.quad	.LBB23_259
	.quad	.LBB23_260
	.quad	.LBB23_261
	.quad	.LBB23_262
	.quad	.LBB23_263
	.quad	.LBB23_264
	.quad	.LBB23_265
	.quad	.LBB23_266
	.quad	.LBB23_267
	.quad	.LBB23_268
	.quad	.LBB23_269
.LJTI23_5:
	.quad	.LBB23_314
	.quad	.LBB23_313
	.quad	.LBB23_293
	.quad	.LBB23_294
	.quad	.LBB23_295
	.quad	.LBB23_296
	.quad	.LBB23_297
	.quad	.LBB23_298
	.quad	.LBB23_299
	.quad	.LBB23_300
	.quad	.LBB23_301
	.quad	.LBB23_302
	.quad	.LBB23_303
	.quad	.LBB23_304
	.quad	.LBB23_305
	.quad	.LBB23_306
	.quad	.LBB23_307
	.quad	.LBB23_308
	.quad	.LBB23_309
	.quad	.LBB23_310
	.quad	.LBB23_311
	.quad	.LBB23_312

	.text
	.align	16, 0x90
	.type	dwarf2out_define,@function
dwarf2out_define:                       # @dwarf2out_define
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp276:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp277:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp278:
	.cfi_def_cfa_offset 32
.Ltmp279:
	.cfi_offset %rbx, -24
.Ltmp280:
	.cfi_offset %r14, -16
	movl	debug_info_level(%rip), %eax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	cmpq	$3, %rax
	jb	.LBB24_1
# BB#2:                                 # %if.then
	movabsq	$.L.str15, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movabsq	$.L.str404, %rdx
	movq	$1, %rdi
	movq	$1, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	movabsq	$.L.str402, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	dw2_asm_output_data_uleb128
	movabsq	$.L.str403, %rdx
	movq	$-1, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	dw2_asm_output_nstring  # TAILCALL
.LBB24_1:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp281:
	.size	dwarf2out_define, .Ltmp281-dwarf2out_define
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_undef,@function
dwarf2out_undef:                        # @dwarf2out_undef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp285:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp286:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp287:
	.cfi_def_cfa_offset 32
.Ltmp288:
	.cfi_offset %rbx, -24
.Ltmp289:
	.cfi_offset %r14, -16
	movl	debug_info_level(%rip), %eax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	cmpq	$3, %rax
	jb	.LBB25_1
# BB#2:                                 # %if.then
	movabsq	$.L.str15, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movabsq	$.L.str401, %rdx
	movq	$1, %rdi
	movq	$2, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	movabsq	$.L.str402, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	dw2_asm_output_data_uleb128
	movabsq	$.L.str403, %rdx
	movq	$-1, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	dw2_asm_output_nstring  # TAILCALL
.LBB25_1:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp290:
	.size	dwarf2out_undef, .Ltmp290-dwarf2out_undef
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_start_source_file,@function
dwarf2out_start_source_file:            # @dwarf2out_start_source_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp297:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp298:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp299:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp300:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp301:
	.cfi_def_cfa_offset 48
.Ltmp302:
	.cfi_offset %rbx, -48
.Ltmp303:
	.cfi_offset %r12, -40
.Ltmp304:
	.cfi_offset %r13, -32
.Ltmp305:
	.cfi_offset %r14, -24
.Ltmp306:
	.cfi_offset %r15, -16
	movl	flag_eliminate_dwarf2_dups(%rip), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB26_11
# BB#1:                                 # %if.then
	movq	comp_unit_die(%rip), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	testq	%rbx, %rbx
	movq	$16644, %rax            # imm = 0x4104
	movl	%eax, (%r12)
	je	.LBB26_5
# BB#2:                                 # %if.then.i
	testq	%r12, %r12
	je	.LBB26_6
# BB#3:                                 # %if.then.i.i
	cmpq	%rbx, %r12
	je	.LBB26_13
# BB#4:                                 # %if.end.i.i
	movq	%rbx, 24(%r12)
	movq	32(%rbx), %rax
	movq	%rax, 40(%r12)
	movq	%r12, 32(%rbx)
	jmp	.LBB26_6
.LBB26_5:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r12, (%rax)
	movq	$0, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB26_6:                               # %new_die.exit
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r13
	movq	%rax, %rbx
	testq	%r13, %r13
	jne	.LBB26_8
# BB#7:                                 # %if.then.i9
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r13
.LBB26_8:                               # %if.end.i
	movq	%r15, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$3, %rcx
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r12, %r12
	je	.LBB26_11
# BB#9:                                 # %if.end.i
	testq	%rbx, %rbx
	je	.LBB26_11
# BB#10:                                # %if.then.i.i11
	movq	16(%r12), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r12)
.LBB26_11:                              # %if.end
	movl	debug_info_level(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB26_12
# BB#14:                                # %if.then1
	movabsq	$.L.str15, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movabsq	$.L.str405, %rdx
	movq	$1, %rdi
	movq	$3, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r14, %rdi
	andq	%rbx, %rdi
	movabsq	$.L.str406, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	dw2_asm_output_data_uleb128
	movq	%r15, %rdi
	callq	lookup_filename
	andq	%rax, %rbx
	movabsq	$.L.str407, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	dw2_asm_output_data_uleb128  # TAILCALL
.LBB26_12:                              # %if.end4
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_13:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp307:
	.size	dwarf2out_start_source_file, .Ltmp307-dwarf2out_start_source_file
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_end_source_file,@function
dwarf2out_end_source_file:              # @dwarf2out_end_source_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp311:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp312:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp313:
	.cfi_def_cfa_offset 32
.Ltmp314:
	.cfi_offset %rbx, -24
.Ltmp315:
	.cfi_offset %r14, -16
	movl	flag_eliminate_dwarf2_dups(%rip), %eax
	testq	%rax, %rax
	je	.LBB27_6
# BB#1:                                 # %if.then
	movq	comp_unit_die(%rip), %r14
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	testq	%r14, %r14
	movq	$16645, %rax            # imm = 0x4105
	movl	%eax, (%rbx)
	je	.LBB27_5
# BB#2:                                 # %if.then.i
	testq	%rbx, %rbx
	je	.LBB27_6
# BB#3:                                 # %if.then.i.i
	cmpq	%r14, %rbx
	je	.LBB27_8
# BB#4:                                 # %if.end.i.i
	movq	%r14, 24(%rbx)
	movq	32(%r14), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r14)
	jmp	.LBB27_6
.LBB27_5:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	$0, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB27_6:                               # %if.end
	movl	debug_info_level(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB27_7
# BB#9:                                 # %if.then1
	movabsq	$.L.str15, %rdi
	movq	$1024, %rsi             # imm = 0x400
	callq	named_section_flags
	movabsq	$.L.str16, %rdx
	movq	$1, %rdi
	movq	$4, %rsi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	dw2_asm_output_data  # TAILCALL
.LBB27_7:                               # %if.end2
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB27_8:                               # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp316:
	.size	dwarf2out_end_source_file, .Ltmp316-dwarf2out_end_source_file
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_begin_block,@function
dwarf2out_begin_block:                  # @dwarf2out_begin_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp319:
	.cfi_def_cfa_offset 16
.Ltmp320:
	.cfi_offset %rbx, -16
	movq	current_function_decl(%rip), %rdi
	movq	%rsi, %rbx
	callq	function_section
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str19, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rcx
	popq	%rbx
	jmp	fprintf  # TAILCALL
.Ltmp321:
	.size	dwarf2out_begin_block, .Ltmp321-dwarf2out_begin_block
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_end_block,@function
dwarf2out_end_block:                    # @dwarf2out_end_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp324:
	.cfi_def_cfa_offset 16
.Ltmp325:
	.cfi_offset %rbx, -16
	movq	current_function_decl(%rip), %rdi
	movq	%rsi, %rbx
	callq	function_section
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str20, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rcx
	popq	%rbx
	jmp	fprintf  # TAILCALL
.Ltmp326:
	.size	dwarf2out_end_block, .Ltmp326-dwarf2out_end_block
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_ignore_block,@function
dwarf2out_ignore_block:                 # @dwarf2out_ignore_block
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB30_17
# BB#1:
	movabsq	$0, %r9
	movabsq	$1, %r8
	.align	16, 0x90
.LBB30_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rdx), %ecx
	xorq	%rax, %rax
	cmpq	$30, %rcx
	je	.LBB30_18
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB30_2 Depth=1
	cmpq	$33, %rcx
	jne	.LBB30_16
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB30_2 Depth=1
	cmpq	$0, 72(%rdx)
	je	.LBB30_18
# BB#5:                                 # %lor.lhs.false9
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	48(%rdx), %rdi
	testq	$4194304, %rdi          # imm = 0x400000
	je	.LBB30_16
# BB#6:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	8(%rdx), %r10
	movl	16(%r10), %ecx
	movq	%rcx, %rsi
	andq	$254, %rsi
	cmpq	$20, %rsi
	je	.LBB30_9
# BB#7:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB30_2 Depth=1
	andq	$255, %rcx
	cmpq	$22, %rcx
	je	.LBB30_9
# BB#8:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB30_2 Depth=1
	cmpq	$10, %rcx
	jne	.LBB30_16
.LBB30_9:                               # %land.lhs.true17
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	(%r10), %rsi
	cmpq	%rsi, %rdx
	je	.LBB30_18
# BB#10:                                # %lor.lhs.false22
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	112(%rdx), %r10
	testq	%r10, %r10
	je	.LBB30_16
# BB#11:                                # %land.lhs.true25
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	$15, %rcx
	shrq	%cl, %rdi
	cmpq	%rdx, %r10
	movq	%r9, %rcx
	jne	.LBB30_13
# BB#12:                                # %land.lhs.true25
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	%r8, %rcx
.LBB30_13:                              # %land.lhs.true25
                                        #   in Loop: Header=BB30_2 Depth=1
	xorq	%rax, %rax
	testq	%rcx, %rdi
	movabsq	$0, %rcx
	jne	.LBB30_15
# BB#14:                                # %land.lhs.true25
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	%r10, %rcx
.LBB30_15:                              # %land.lhs.true25
                                        #   in Loop: Header=BB30_2 Depth=1
	cmpq	%rsi, %rcx
	je	.LBB30_18
	.align	16, 0x90
.LBB30_16:                              # %for.inc
                                        #   in Loop: Header=BB30_2 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB30_2
.LBB30_17:
	movq	$1, %rax
.LBB30_18:                              # %return
	retq
.Ltmp327:
	.size	dwarf2out_ignore_block, .Ltmp327-dwarf2out_ignore_block
	.cfi_endproc

	.globl	dwarf2out_decl
	.align	16, 0x90
	.type	dwarf2out_decl,@function
dwarf2out_decl:                         # @dwarf2out_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp331:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp332:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp333:
	.cfi_def_cfa_offset 32
.Ltmp334:
	.cfi_offset %rbx, -24
.Ltmp335:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	comp_unit_die(%rip), %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	je	.LBB31_6
# BB#1:                                 # %entry
	cmpq	$33, %rcx
	jne	.LBB31_2
# BB#9:                                 # %sw.bb36
	movq	48(%rbx), %rax
	testq	$256, %rax              # imm = 0x100
	jne	.LBB31_33
# BB#10:                                # %if.end45
	movl	32(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB31_11
# BB#28:                                # %if.end70
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB31_33
	jmp	.LBB31_29
.LBB31_6:                               # %sw.bb17
	movzbl	49(%rbx), %ecx
	testq	$1, %rcx
	je	.LBB31_32
# BB#7:                                 # %land.lhs.true25
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB31_33
# BB#8:                                 # %land.lhs.true25
	movl	debug_info_level(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jae	.LBB31_31
	jmp	.LBB31_33
.LBB31_2:                               # %entry
	cmpq	$30, %rcx
	jne	.LBB31_33
# BB#3:                                 # %sw.bb1
	movq	48(%rbx), %rax
	testq	$256, %rax              # imm = 0x100
	je	.LBB31_5
# BB#4:                                 # %sw.bb1
	andq	$1610612736, %rax       # imm = 0x60000000
	jne	.LBB31_33
.LBB31_5:                               # %if.end
	cmpq	$0, 104(%rbx)
	je	.LBB31_33
.LBB31_29:                              # %if.end73
	movq	%rbx, %rdi
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB31_31
# BB#30:                                # %if.end73
	xorq	%r14, %r14
	jmp	.LBB31_31
.LBB31_32:                              # %if.end32
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB31_33
.LBB31_31:                              # %sw.epilog
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_decl_die  # TAILCALL
.LBB31_11:                              # %tailrecurse.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB31_13 Depth 2
	testq	%r14, %r14
	je	.LBB31_33
# BB#12:                                # %if.then.i.i
                                        #   in Loop: Header=BB31_11 Depth=1
	movq	16(%r14), %rcx
	xorq	%r14, %r14
	testq	%rcx, %rcx
	je	.LBB31_33
	.align	16, 0x90
.LBB31_13:                              # %for.body.i.i
                                        #   Parent Loop BB31_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB31_16
# BB#14:                                # %for.body.i.i
                                        #   in Loop: Header=BB31_13 Depth=2
	cmpq	$19, %rdx
	je	.LBB31_22
# BB#15:                                # %for.body.i.i
                                        #   in Loop: Header=BB31_13 Depth=2
	cmpq	$71, %rdx
	jne	.LBB31_20
.LBB31_16:                              # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB31_13 Depth=2
	movl	16(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB31_17
# BB#19:                                # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB31_13 Depth=2
	movq	24(%rcx), %r14
.LBB31_20:                              # %for.inc.i.i
                                        #   in Loop: Header=BB31_13 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB31_13
# BB#21:                                # %for.end.i.i
                                        #   in Loop: Header=BB31_11 Depth=1
	testq	%r14, %r14
	jne	.LBB31_11
	jmp	.LBB31_33
.LBB31_22:                              # %land.lhs.true.i.i
	movl	16(%rcx), %edx
	cmpq	$6, %rdx
	jne	.LBB31_23
# BB#24:                                # %get_AT_unsigned.exit
	movl	24(%rcx), %ecx
	cmpq	$4, %rcx
	jne	.LBB31_33
# BB#25:                                # %land.lhs.true51
	movq	8(%rbx), %rdi
	movzbl	16(%rdi), %ecx
	cmpq	$11, %rcx
	jne	.LBB31_33
# BB#26:                                # %land.lhs.true51
	andq	$16384, %rax            # imm = 0x4000
	jne	.LBB31_33
# BB#27:                                # %if.then65
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	modified_type_die  # TAILCALL
.LBB31_33:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB31_17:                              # %if.end.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB31_18
.LBB31_23:                              # %if.end.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
.LBB31_18:                              # %if.end.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp336:
	.size	dwarf2out_decl, .Ltmp336-dwarf2out_decl
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_global_decl,@function
dwarf2out_global_decl:                  # @dwarf2out_global_decl
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	cmpq	$30, %rax
	jne	.LBB32_3
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, 104(%rdi)
	je	.LBB32_3
# BB#2:                                 # %if.end
	retq
.LBB32_3:                               # %if.then
	jmp	dwarf2out_decl  # TAILCALL
.Ltmp337:
	.size	dwarf2out_global_decl, .Ltmp337-dwarf2out_global_decl
	.cfi_endproc

	.align	16, 0x90
	.type	dwarf2out_abstract_function,@function
dwarf2out_abstract_function:            # @dwarf2out_abstract_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp342:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp343:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp344:
	.cfi_def_cfa_offset 32
.Ltmp345:
	.cfi_offset %rbx, -32
.Ltmp346:
	.cfi_offset %r14, -24
.Ltmp347:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	112(%rbx), %rax
	movq	48(%rbx), %r15
	testq	%rax, %rax
	je	.LBB33_2
# BB#1:                                 # %entry
	movq	%rax, %rbx
.LBB33_2:                               # %entry
	movl	36(%rbx), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB33_18
# BB#3:                                 # %lookup_decl_die.exit
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	jmp	.LBB33_4
	.align	16, 0x90
.LBB33_5:                               # %tailrecurse.i.i
                                        #   in Loop: Header=BB33_4 Depth=1
	testq	%rax, %rax
	je	.LBB33_18
# BB#6:                                 # %if.then.i.i
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB33_18
	.align	16, 0x90
.LBB33_7:                               # %for.body.i.i
                                        #   Parent Loop BB33_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB33_10
# BB#8:                                 # %for.body.i.i
                                        #   in Loop: Header=BB33_7 Depth=2
	cmpq	$32, %rdx
	je	.LBB33_15
# BB#9:                                 # %for.body.i.i
                                        #   in Loop: Header=BB33_7 Depth=2
	cmpq	$71, %rdx
	jne	.LBB33_14
.LBB33_10:                              # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB33_7 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB33_11
# BB#13:                                # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB33_7 Depth=2
	movq	24(%rcx), %rax
.LBB33_14:                              # %for.inc.i.i
                                        #   in Loop: Header=BB33_7 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB33_7
.LBB33_4:                               # %lookup_decl_die.exit
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_7 Depth 2
	testq	%rax, %rax
	jne	.LBB33_5
	jmp	.LBB33_18
.LBB33_15:                              # %land.lhs.true.i.i
	movl	16(%rcx), %eax
	cmpq	$6, %rax
	jne	.LBB33_16
# BB#17:                                # %get_AT_unsigned.exit
	movl	24(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB33_31
.LBB33_18:                              # %if.end
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB33_28
# BB#19:                                # %if.then8
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB33_21
# BB#20:                                # %lor.lhs.false.i
	cmpq	$0, 184(%rbx)
	je	.LBB33_21
# BB#22:                                # %if.else.i
	movq	8(%rbx), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	addq	$128, %rax
	jmp	.LBB33_23
.LBB33_21:                              # %if.then.i
	leaq	80(%rbx), %rax
.LBB33_23:                              # %if.end.i
	movq	(%rax), %r14
	testq	%r14, %r14
	je	.LBB33_28
# BB#24:                                # %land.lhs.true.i
	movzbl	16(%r14), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB33_28
# BB#25:                                # %if.then11
	movq	%rbx, %rdi
	callq	decl_function_context
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jne	.LBB33_27
# BB#26:                                # %if.then11
	movq	comp_unit_die(%rip), %rdx
.LBB33_27:                              # %if.then11
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_type_die_for_member
.LBB33_28:                              # %if.end19
	movq	current_function_decl(%rip), %r14
	movq	%rbx, current_function_decl(%rip)
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	set_decl_abstract_flags
	movq	%rbx, %rdi
	callq	dwarf2out_decl
	testq	$32768, %r15            # imm = 0x8000
	jne	.LBB33_30
# BB#29:                                # %if.then21
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_decl_abstract_flags
.LBB33_30:                              # %if.end22
	movq	%r14, current_function_decl(%rip)
.LBB33_31:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB33_11:                              # %if.end.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB33_12
.LBB33_16:                              # %if.end.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
.LBB33_12:                              # %if.end.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp348:
	.size	dwarf2out_abstract_function, .Ltmp348-dwarf2out_abstract_function
	.cfi_endproc

	.globl	dwarf2out_set_demangle_name_func
	.align	16, 0x90
	.type	dwarf2out_set_demangle_name_func,@function
dwarf2out_set_demangle_name_func:       # @dwarf2out_set_demangle_name_func
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, demangle_name_func(%rip)
	retq
.Ltmp349:
	.size	dwarf2out_set_demangle_name_func, .Ltmp349-dwarf2out_set_demangle_name_func
	.cfi_endproc

	.globl	debug_dwarf_die
	.align	16, 0x90
	.type	debug_dwarf_die,@function
debug_dwarf_die:                        # @debug_dwarf_die
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rsi
	jmp	print_die  # TAILCALL
.Ltmp350:
	.size	debug_dwarf_die, .Ltmp350-debug_dwarf_die
	.cfi_endproc

	.align	16, 0x90
	.type	print_die,@function
print_die:                              # @print_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp357:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp358:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp359:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp360:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp361:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp362:
	.cfi_def_cfa_offset 64
.Ltmp363:
	.cfi_offset %rbx, -48
.Ltmp364:
	.cfi_offset %r12, -40
.Ltmp365:
	.cfi_offset %r13, -32
.Ltmp366:
	.cfi_offset %r14, -24
.Ltmp367:
	.cfi_offset %r15, -16
	movl	print_indent(%rip), %edx
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movabsq	$.L.str439, %rsi
	movabsq	$.L.str440, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movslq	(%rbx), %rax
	movq	48(%rbx), %rdx
	cmpq	$16512, %rax            # imm = 0x4080
	jg	.LBB36_3
# BB#1:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$53, %rax
	ja	.LBB36_60
# BB#2:                                 # %entry
	movabsq	$.L.str155, %rcx
	jmpq	*.LJTI36_1(,%rax,8)
.LBB36_8:                               # %sw.bb1.i
	movabsq	$.L.str156, %rcx
	jmp	.LBB36_61
.LBB36_3:                               # %entry
	cmpq	$16640, %rax            # imm = 0x4100
	jg	.LBB36_6
# BB#4:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$16513, %rax            # imm = 0x4081
	jne	.LBB36_60
# BB#5:                                 # %sw.bb48.i
	movabsq	$.L.str203, %rcx
	jmp	.LBB36_61
.LBB36_6:                               # %entry
	addq	$-16641, %rax           # imm = 0xFFFFFFFFFFFFBEFF
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$4, %rcx
	jbe	.LBB36_7
.LBB36_60:                              # %sw.default.i
	movabsq	$.L.str209, %rcx
.LBB36_61:                              # %dwarf_tag_name.exit
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str419, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movl	print_indent(%rip), %edx
	movabsq	$.L.str439, %rsi
	movabsq	$.L.str440, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	56(%rbx), %rdx
	movabsq	$.L.str420, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	48(%rbx), %rdx
	movabsq	$.L.str421, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB36_190
# BB#62:
	movabsq	$.L.str439, %r15
	movabsq	$.L.str440, %r12
	movabsq	$.L.str422, %r13
	jmp	.LBB36_63
.LBB36_179:                             # %AT_ref.exit240
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB36_180
# BB#181:                               # %if.else
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	48(%rax), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str434, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_186:                             # %AT_string.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str437, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_157:                             # %sw.bb86.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str147, %rdx
	jmp	.LBB36_166
.LBB36_158:                             # %sw.bb87.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str148, %rdx
	jmp	.LBB36_166
.LBB36_159:                             # %sw.bb88.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str149, %rdx
	jmp	.LBB36_166
.LBB36_160:                             # %sw.bb89.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str150, %rdx
	jmp	.LBB36_166
.LBB36_161:                             # %sw.bb90.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str151, %rdx
	jmp	.LBB36_166
.LBB36_162:                             # %sw.bb91.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str152, %rdx
	jmp	.LBB36_166
.LBB36_180:                             # %if.then38
                                        #   in Loop: Header=BB36_63 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str433, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_146:                             # %sw.bb75.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str136, %rdx
	jmp	.LBB36_166
.LBB36_147:                             # %sw.bb76.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str137, %rdx
	jmp	.LBB36_166
.LBB36_148:                             # %sw.bb77.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str138, %rdx
	jmp	.LBB36_166
.LBB36_149:                             # %sw.bb78.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str139, %rdx
	jmp	.LBB36_166
.LBB36_150:                             # %sw.bb79.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str140, %rdx
	jmp	.LBB36_166
.LBB36_151:                             # %sw.bb80.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str141, %rdx
	jmp	.LBB36_166
.LBB36_152:                             # %sw.bb81.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str142, %rdx
	jmp	.LBB36_166
.LBB36_153:                             # %sw.bb82.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str143, %rdx
	jmp	.LBB36_166
.LBB36_154:                             # %sw.bb83.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str144, %rdx
	jmp	.LBB36_166
.LBB36_155:                             # %sw.bb84.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str145, %rdx
	jmp	.LBB36_166
.LBB36_73:                              # %sw.bb2.i126
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str63, %rdx
	jmp	.LBB36_166
.LBB36_74:                              # %sw.bb3.i127
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str64, %rdx
	jmp	.LBB36_166
.LBB36_75:                              # %sw.bb4.i128
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str65, %rdx
	jmp	.LBB36_166
.LBB36_76:                              # %sw.bb5.i129
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str66, %rdx
	jmp	.LBB36_166
.LBB36_77:                              # %sw.bb6.i130
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str67, %rdx
	jmp	.LBB36_166
.LBB36_78:                              # %sw.bb7.i131
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str68, %rdx
	jmp	.LBB36_166
.LBB36_79:                              # %sw.bb8.i132
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str69, %rdx
	jmp	.LBB36_166
.LBB36_80:                              # %sw.bb9.i133
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str70, %rdx
	jmp	.LBB36_166
.LBB36_81:                              # %sw.bb10.i134
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str71, %rdx
	jmp	.LBB36_166
.LBB36_82:                              # %sw.bb11.i135
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str72, %rdx
	jmp	.LBB36_166
.LBB36_83:                              # %sw.bb12.i136
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str73, %rdx
	jmp	.LBB36_166
.LBB36_84:                              # %sw.bb13.i137
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str74, %rdx
	jmp	.LBB36_166
.LBB36_85:                              # %sw.bb14.i138
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str75, %rdx
	jmp	.LBB36_166
.LBB36_86:                              # %sw.bb15.i139
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str76, %rdx
	jmp	.LBB36_166
.LBB36_87:                              # %sw.bb16.i140
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str77, %rdx
	jmp	.LBB36_166
.LBB36_88:                              # %sw.bb17.i141
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str78, %rdx
	jmp	.LBB36_166
.LBB36_89:                              # %sw.bb18.i142
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str79, %rdx
	jmp	.LBB36_166
.LBB36_90:                              # %sw.bb19.i143
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str80, %rdx
	jmp	.LBB36_166
.LBB36_91:                              # %sw.bb20.i144
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str81, %rdx
	jmp	.LBB36_166
.LBB36_92:                              # %sw.bb21.i145
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str82, %rdx
	jmp	.LBB36_166
.LBB36_93:                              # %sw.bb22.i146
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str83, %rdx
	jmp	.LBB36_166
.LBB36_94:                              # %sw.bb23.i147
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str84, %rdx
	jmp	.LBB36_166
.LBB36_95:                              # %sw.bb24.i148
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str85, %rdx
	jmp	.LBB36_166
.LBB36_96:                              # %sw.bb25.i149
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str86, %rdx
	jmp	.LBB36_166
.LBB36_97:                              # %sw.bb26.i150
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str87, %rdx
	jmp	.LBB36_166
.LBB36_98:                              # %sw.bb27.i151
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str88, %rdx
	jmp	.LBB36_166
.LBB36_99:                              # %sw.bb28.i152
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str89, %rdx
	jmp	.LBB36_166
.LBB36_100:                             # %sw.bb29.i153
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str90, %rdx
	jmp	.LBB36_166
.LBB36_101:                             # %sw.bb30.i154
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str91, %rdx
	jmp	.LBB36_166
.LBB36_102:                             # %sw.bb31.i155
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str92, %rdx
	jmp	.LBB36_166
.LBB36_103:                             # %sw.bb32.i156
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str93, %rdx
	jmp	.LBB36_166
.LBB36_104:                             # %sw.bb33.i157
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str94, %rdx
	jmp	.LBB36_166
.LBB36_105:                             # %sw.bb34.i158
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str95, %rdx
	jmp	.LBB36_166
.LBB36_106:                             # %sw.bb35.i159
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str96, %rdx
	jmp	.LBB36_166
.LBB36_107:                             # %sw.bb36.i160
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str97, %rdx
	jmp	.LBB36_166
.LBB36_108:                             # %sw.bb37.i161
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str98, %rdx
	jmp	.LBB36_166
.LBB36_109:                             # %sw.bb38.i162
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str99, %rdx
	jmp	.LBB36_166
.LBB36_110:                             # %sw.bb39.i163
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str100, %rdx
	jmp	.LBB36_166
.LBB36_111:                             # %sw.bb40.i164
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str101, %rdx
	jmp	.LBB36_166
.LBB36_112:                             # %sw.bb41.i165
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str102, %rdx
	jmp	.LBB36_166
.LBB36_113:                             # %sw.bb42.i166
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str103, %rdx
	jmp	.LBB36_166
.LBB36_114:                             # %sw.bb43.i167
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str104, %rdx
	jmp	.LBB36_166
.LBB36_115:                             # %sw.bb44.i168
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str105, %rdx
	jmp	.LBB36_166
.LBB36_116:                             # %sw.bb45.i169
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str106, %rdx
	jmp	.LBB36_166
.LBB36_117:                             # %sw.bb46.i170
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str107, %rdx
	jmp	.LBB36_166
.LBB36_118:                             # %sw.bb47.i171
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str108, %rdx
	jmp	.LBB36_166
.LBB36_119:                             # %sw.bb48.i172
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str109, %rdx
	jmp	.LBB36_166
.LBB36_120:                             # %sw.bb49.i173
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str110, %rdx
	jmp	.LBB36_166
.LBB36_121:                             # %sw.bb50.i174
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str111, %rdx
	jmp	.LBB36_166
.LBB36_122:                             # %sw.bb51.i175
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str112, %rdx
	jmp	.LBB36_166
.LBB36_123:                             # %sw.bb52.i176
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str113, %rdx
	jmp	.LBB36_166
.LBB36_124:                             # %sw.bb53.i177
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str114, %rdx
	jmp	.LBB36_166
.LBB36_125:                             # %sw.bb54.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str115, %rdx
	jmp	.LBB36_166
.LBB36_126:                             # %sw.bb55.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str116, %rdx
	jmp	.LBB36_166
.LBB36_127:                             # %sw.bb56.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str117, %rdx
	jmp	.LBB36_166
.LBB36_128:                             # %sw.bb57.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str118, %rdx
	jmp	.LBB36_166
.LBB36_129:                             # %sw.bb58.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str119, %rdx
	jmp	.LBB36_166
.LBB36_130:                             # %sw.bb59.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str120, %rdx
	jmp	.LBB36_166
.LBB36_131:                             # %sw.bb60.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str121, %rdx
	jmp	.LBB36_166
.LBB36_132:                             # %sw.bb61.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str122, %rdx
	jmp	.LBB36_166
.LBB36_133:                             # %sw.bb62.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str123, %rdx
	jmp	.LBB36_166
.LBB36_134:                             # %sw.bb63.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str124, %rdx
	jmp	.LBB36_166
.LBB36_135:                             # %sw.bb64.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str125, %rdx
	jmp	.LBB36_166
.LBB36_136:                             # %sw.bb65.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str126, %rdx
	jmp	.LBB36_166
.LBB36_137:                             # %sw.bb66.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str127, %rdx
	jmp	.LBB36_166
.LBB36_138:                             # %sw.bb67.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str128, %rdx
	jmp	.LBB36_166
.LBB36_139:                             # %sw.bb68.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str129, %rdx
	jmp	.LBB36_166
.LBB36_140:                             # %sw.bb69.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str130, %rdx
	jmp	.LBB36_166
.LBB36_141:                             # %sw.bb70.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str131, %rdx
	jmp	.LBB36_166
.LBB36_142:                             # %sw.bb71.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str132, %rdx
	jmp	.LBB36_166
.LBB36_143:                             # %sw.bb72.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str133, %rdx
	jmp	.LBB36_166
.LBB36_144:                             # %sw.bb73.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str134, %rdx
	jmp	.LBB36_166
	.align	16, 0x90
.LBB36_63:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	print_indent(%rip), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	callq	fprintf
	movslq	(%rbx), %rax
	cmpq	$8192, %rax             # imm = 0x2000
	jg	.LBB36_66
# BB#64:                                # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$88, %rax
	ja	.LBB36_165
# BB#65:                                # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str61, %rdx
	jmpq	*.LJTI36_4(,%rax,8)
.LBB36_72:                              # %sw.bb1.i125
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str62, %rdx
	jmp	.LBB36_166
	.align	16, 0x90
.LBB36_66:                              # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	cmpq	$8704, %rax             # imm = 0x2200
	jg	.LBB36_163
# BB#67:                                # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	cmpq	$8448, %rax             # imm = 0x2100
	jg	.LBB36_70
# BB#68:                                # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	addq	$-8193, %rax            # imm = 0xFFFFFFFFFFFFDFFF
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$10, %rax
	ja	.LBB36_165
# BB#69:                                # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	jmpq	*.LJTI36_3(,%rax,8)
.LBB36_145:                             # %sw.bb74.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str135, %rdx
	jmp	.LBB36_166
	.align	16, 0x90
.LBB36_163:                             # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$8705, %rax             # imm = 0x2201
	jne	.LBB36_165
# BB#164:                               # %sw.bb92.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str153, %rdx
	jmp	.LBB36_166
.LBB36_70:                              # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	addq	$-8449, %rax            # imm = 0xFFFFFFFFFFFFDEFF
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$6, %rax
	jbe	.LBB36_71
	.align	16, 0x90
.LBB36_165:                             # %sw.default.i178
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str154, %rdx
.LBB36_166:                             # %dwarf_attr_name.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	fprintf
	movl	16(%rbx), %eax
	cmpq	$14, %rax
	ja	.LBB36_189
# BB#167:                               # %dwarf_attr_name.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	jmpq	*.LJTI36_5(,%rax,8)
.LBB36_183:                             # %land.lhs.true.i242
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	cmpq	$12, %rax
	jne	.LBB36_196
# BB#184:                               # %AT_lbl.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str436, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_71:                              # %for.body
                                        #   in Loop: Header=BB36_63 Depth=1
	jmpq	*.LJTI36_2(,%rax,8)
.LBB36_156:                             # %sw.bb85.i
                                        #   in Loop: Header=BB36_63 Depth=1
	movabsq	$.L.str146, %rdx
	jmp	.LBB36_166
.LBB36_168:                             # %sw.bb
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$7, %rsi
	movq	$1, %rdx
	movabsq	$.L.str423, %rdi
	jmp	.LBB36_188
.LBB36_169:                             # %sw.bb9
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str424, %rdi
	jmp	.LBB36_188
.LBB36_170:                             # %sw.bb11
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$19, %rsi
	movq	$1, %rdx
	movabsq	$.L.str425, %rdi
	jmp	.LBB36_188
.LBB36_171:                             # %AT_loc_list.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rax
	movq	24(%rax), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str426, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_172:                             # %sw.bb16
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$10, %rsi
	movq	$1, %rdx
	movabsq	$.L.str427, %rdi
	jmp	.LBB36_188
.LBB36_173:                             # %AT_int.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str428, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_174:                             # %AT_unsigned.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str429, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_175:                             # %sw.bb24
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rdx
	movq	32(%rbx), %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str430, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_176:                             # %sw.bb29
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$23, %rsi
	movq	$1, %rdx
	movabsq	$.L.str431, %rdi
	jmp	.LBB36_188
.LBB36_177:                             # %AT_flag.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	movzbl	24(%rbx), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str432, %rsi
	callq	fprintf
	jmp	.LBB36_189
.LBB36_178:                             # %AT_ref.exit
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_179
# BB#182:                               # %if.else45
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$13, %rsi
	movq	$1, %rdx
	movabsq	$.L.str435, %rdi
	jmp	.LBB36_188
.LBB36_185:                             # %AT_string.exit189
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	24(%rbx), %rax
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB36_186
# BB#187:                               # %if.else57
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str438, %rdi
.LBB36_188:                             # %sw.epilog
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	%r14, %rcx
	callq	fwrite
	.align	16, 0x90
.LBB36_189:                             # %sw.epilog
                                        #   in Loop: Header=BB36_63 Depth=1
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_63
.LBB36_190:                             # %for.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	32(%rax), %rbx
	movl	print_indent(%rip), %eax
	testq	%rbx, %rbx
	je	.LBB36_194
# BB#191:                               # %for.body66.lr.ph
	addq	$4, %rax
	movl	%eax, print_indent(%rip)
	.align	16, 0x90
.LBB36_192:                             # %for.body66
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	print_die
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_192
# BB#193:                               # %for.end68
	movl	print_indent(%rip), %eax
	addq	$-4, %rax
	movl	%eax, print_indent(%rip)
.LBB36_194:                             # %if.end69
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB36_197
# BB#195:                               # %if.end73
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB36_197:                             # %if.then71
	movq	$10, %rdi
	movq	%r14, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB36_7:                               # %entry
	jmpq	*.LJTI36_0(,%rcx,8)
.LBB36_55:                              # %sw.bb49.i
	movabsq	$.L.str204, %rcx
	jmp	.LBB36_61
.LBB36_56:                              # %sw.bb50.i
	movabsq	$.L.str205, %rcx
	jmp	.LBB36_61
.LBB36_57:                              # %sw.bb51.i
	movabsq	$.L.str206, %rcx
	jmp	.LBB36_61
.LBB36_58:                              # %sw.bb52.i
	movabsq	$.L.str207, %rcx
	jmp	.LBB36_61
.LBB36_59:                              # %sw.bb53.i
	movabsq	$.L.str208, %rcx
	jmp	.LBB36_61
.LBB36_9:                               # %sw.bb2.i
	movabsq	$.L.str157, %rcx
	jmp	.LBB36_61
.LBB36_10:                              # %sw.bb3.i
	movabsq	$.L.str158, %rcx
	jmp	.LBB36_61
.LBB36_11:                              # %sw.bb4.i
	movabsq	$.L.str159, %rcx
	jmp	.LBB36_61
.LBB36_12:                              # %sw.bb5.i
	movabsq	$.L.str160, %rcx
	jmp	.LBB36_61
.LBB36_13:                              # %sw.bb6.i
	movabsq	$.L.str161, %rcx
	jmp	.LBB36_61
.LBB36_14:                              # %sw.bb7.i
	movabsq	$.L.str162, %rcx
	jmp	.LBB36_61
.LBB36_15:                              # %sw.bb8.i
	movabsq	$.L.str163, %rcx
	jmp	.LBB36_61
.LBB36_16:                              # %sw.bb9.i
	movabsq	$.L.str164, %rcx
	jmp	.LBB36_61
.LBB36_17:                              # %sw.bb10.i
	movabsq	$.L.str165, %rcx
	jmp	.LBB36_61
.LBB36_18:                              # %sw.bb11.i
	movabsq	$.L.str166, %rcx
	jmp	.LBB36_61
.LBB36_19:                              # %sw.bb12.i
	movabsq	$.L.str167, %rcx
	jmp	.LBB36_61
.LBB36_20:                              # %sw.bb13.i
	movabsq	$.L.str168, %rcx
	jmp	.LBB36_61
.LBB36_21:                              # %sw.bb14.i
	movabsq	$.L.str169, %rcx
	jmp	.LBB36_61
.LBB36_22:                              # %sw.bb15.i
	movabsq	$.L.str170, %rcx
	jmp	.LBB36_61
.LBB36_23:                              # %sw.bb16.i
	movabsq	$.L.str171, %rcx
	jmp	.LBB36_61
.LBB36_24:                              # %sw.bb17.i
	movabsq	$.L.str172, %rcx
	jmp	.LBB36_61
.LBB36_25:                              # %sw.bb18.i
	movabsq	$.L.str173, %rcx
	jmp	.LBB36_61
.LBB36_26:                              # %sw.bb19.i
	movabsq	$.L.str174, %rcx
	jmp	.LBB36_61
.LBB36_27:                              # %sw.bb20.i
	movabsq	$.L.str175, %rcx
	jmp	.LBB36_61
.LBB36_28:                              # %sw.bb21.i
	movabsq	$.L.str176, %rcx
	jmp	.LBB36_61
.LBB36_29:                              # %sw.bb22.i
	movabsq	$.L.str177, %rcx
	jmp	.LBB36_61
.LBB36_30:                              # %sw.bb23.i
	movabsq	$.L.str178, %rcx
	jmp	.LBB36_61
.LBB36_31:                              # %sw.bb24.i
	movabsq	$.L.str179, %rcx
	jmp	.LBB36_61
.LBB36_32:                              # %sw.bb25.i
	movabsq	$.L.str180, %rcx
	jmp	.LBB36_61
.LBB36_33:                              # %sw.bb26.i
	movabsq	$.L.str181, %rcx
	jmp	.LBB36_61
.LBB36_34:                              # %sw.bb27.i
	movabsq	$.L.str182, %rcx
	jmp	.LBB36_61
.LBB36_35:                              # %sw.bb28.i
	movabsq	$.L.str183, %rcx
	jmp	.LBB36_61
.LBB36_36:                              # %sw.bb29.i
	movabsq	$.L.str184, %rcx
	jmp	.LBB36_61
.LBB36_37:                              # %sw.bb30.i
	movabsq	$.L.str185, %rcx
	jmp	.LBB36_61
.LBB36_38:                              # %sw.bb31.i
	movabsq	$.L.str186, %rcx
	jmp	.LBB36_61
.LBB36_39:                              # %sw.bb32.i
	movabsq	$.L.str187, %rcx
	jmp	.LBB36_61
.LBB36_40:                              # %sw.bb33.i
	movabsq	$.L.str188, %rcx
	jmp	.LBB36_61
.LBB36_41:                              # %sw.bb34.i
	movabsq	$.L.str189, %rcx
	jmp	.LBB36_61
.LBB36_42:                              # %sw.bb35.i
	movabsq	$.L.str190, %rcx
	jmp	.LBB36_61
.LBB36_43:                              # %sw.bb36.i
	movabsq	$.L.str191, %rcx
	jmp	.LBB36_61
.LBB36_44:                              # %sw.bb37.i
	movabsq	$.L.str192, %rcx
	jmp	.LBB36_61
.LBB36_45:                              # %sw.bb38.i
	movabsq	$.L.str193, %rcx
	jmp	.LBB36_61
.LBB36_46:                              # %sw.bb39.i
	movabsq	$.L.str194, %rcx
	jmp	.LBB36_61
.LBB36_47:                              # %sw.bb40.i
	movabsq	$.L.str195, %rcx
	jmp	.LBB36_61
.LBB36_48:                              # %sw.bb41.i
	movabsq	$.L.str196, %rcx
	jmp	.LBB36_61
.LBB36_49:                              # %sw.bb42.i
	movabsq	$.L.str197, %rcx
	jmp	.LBB36_61
.LBB36_50:                              # %sw.bb43.i
	movabsq	$.L.str198, %rcx
	jmp	.LBB36_61
.LBB36_51:                              # %sw.bb44.i
	movabsq	$.L.str199, %rcx
	jmp	.LBB36_61
.LBB36_52:                              # %sw.bb45.i
	movabsq	$.L.str200, %rcx
	jmp	.LBB36_61
.LBB36_53:                              # %sw.bb46.i
	movabsq	$.L.str201, %rcx
	jmp	.LBB36_61
.LBB36_54:                              # %sw.bb47.i
	movabsq	$.L.str202, %rcx
	jmp	.LBB36_61
.LBB36_196:                             # %if.end.i244
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_lbl, %rcx
	movq	$4785, %rsi             # imm = 0x12B1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp368:
	.size	print_die, .Ltmp368-print_die
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI36_0:
	.quad	.LBB36_55
	.quad	.LBB36_56
	.quad	.LBB36_57
	.quad	.LBB36_58
	.quad	.LBB36_59
.LJTI36_1:
	.quad	.LBB36_61
	.quad	.LBB36_8
	.quad	.LBB36_9
	.quad	.LBB36_10
	.quad	.LBB36_11
	.quad	.LBB36_12
	.quad	.LBB36_60
	.quad	.LBB36_60
	.quad	.LBB36_13
	.quad	.LBB36_60
	.quad	.LBB36_14
	.quad	.LBB36_15
	.quad	.LBB36_60
	.quad	.LBB36_16
	.quad	.LBB36_60
	.quad	.LBB36_17
	.quad	.LBB36_18
	.quad	.LBB36_19
	.quad	.LBB36_20
	.quad	.LBB36_21
	.quad	.LBB36_60
	.quad	.LBB36_22
	.quad	.LBB36_23
	.quad	.LBB36_24
	.quad	.LBB36_25
	.quad	.LBB36_26
	.quad	.LBB36_27
	.quad	.LBB36_28
	.quad	.LBB36_29
	.quad	.LBB36_30
	.quad	.LBB36_31
	.quad	.LBB36_32
	.quad	.LBB36_33
	.quad	.LBB36_34
	.quad	.LBB36_35
	.quad	.LBB36_36
	.quad	.LBB36_37
	.quad	.LBB36_38
	.quad	.LBB36_39
	.quad	.LBB36_40
	.quad	.LBB36_41
	.quad	.LBB36_42
	.quad	.LBB36_43
	.quad	.LBB36_44
	.quad	.LBB36_45
	.quad	.LBB36_46
	.quad	.LBB36_47
	.quad	.LBB36_48
	.quad	.LBB36_49
	.quad	.LBB36_50
	.quad	.LBB36_51
	.quad	.LBB36_52
	.quad	.LBB36_53
	.quad	.LBB36_54
.LJTI36_2:
	.quad	.LBB36_156
	.quad	.LBB36_157
	.quad	.LBB36_158
	.quad	.LBB36_159
	.quad	.LBB36_160
	.quad	.LBB36_161
	.quad	.LBB36_162
.LJTI36_3:
	.quad	.LBB36_145
	.quad	.LBB36_146
	.quad	.LBB36_147
	.quad	.LBB36_148
	.quad	.LBB36_149
	.quad	.LBB36_150
	.quad	.LBB36_151
	.quad	.LBB36_152
	.quad	.LBB36_153
	.quad	.LBB36_154
	.quad	.LBB36_155
.LJTI36_4:
	.quad	.LBB36_166
	.quad	.LBB36_72
	.quad	.LBB36_73
	.quad	.LBB36_165
	.quad	.LBB36_165
	.quad	.LBB36_165
	.quad	.LBB36_165
	.quad	.LBB36_165
	.quad	.LBB36_74
	.quad	.LBB36_75
	.quad	.LBB36_76
	.quad	.LBB36_77
	.quad	.LBB36_78
	.quad	.LBB36_165
	.quad	.LBB36_79
	.quad	.LBB36_80
	.quad	.LBB36_81
	.quad	.LBB36_82
	.quad	.LBB36_83
	.quad	.LBB36_84
	.quad	.LBB36_85
	.quad	.LBB36_86
	.quad	.LBB36_87
	.quad	.LBB36_88
	.quad	.LBB36_89
	.quad	.LBB36_90
	.quad	.LBB36_91
	.quad	.LBB36_92
	.quad	.LBB36_93
	.quad	.LBB36_94
	.quad	.LBB36_165
	.quad	.LBB36_95
	.quad	.LBB36_96
	.quad	.LBB36_97
	.quad	.LBB36_165
	.quad	.LBB36_165
	.quad	.LBB36_98
	.quad	.LBB36_165
	.quad	.LBB36_99
	.quad	.LBB36_165
	.quad	.LBB36_165
	.quad	.LBB36_100
	.quad	.LBB36_165
	.quad	.LBB36_101
	.quad	.LBB36_165
	.quad	.LBB36_102
	.quad	.LBB36_103
	.quad	.LBB36_165
	.quad	.LBB36_104
	.quad	.LBB36_105
	.quad	.LBB36_106
	.quad	.LBB36_107
	.quad	.LBB36_108
	.quad	.LBB36_109
	.quad	.LBB36_110
	.quad	.LBB36_111
	.quad	.LBB36_112
	.quad	.LBB36_113
	.quad	.LBB36_114
	.quad	.LBB36_115
	.quad	.LBB36_116
	.quad	.LBB36_117
	.quad	.LBB36_118
	.quad	.LBB36_119
	.quad	.LBB36_120
	.quad	.LBB36_121
	.quad	.LBB36_122
	.quad	.LBB36_123
	.quad	.LBB36_124
	.quad	.LBB36_125
	.quad	.LBB36_126
	.quad	.LBB36_127
	.quad	.LBB36_128
	.quad	.LBB36_129
	.quad	.LBB36_130
	.quad	.LBB36_131
	.quad	.LBB36_132
	.quad	.LBB36_133
	.quad	.LBB36_134
	.quad	.LBB36_135
	.quad	.LBB36_136
	.quad	.LBB36_137
	.quad	.LBB36_138
	.quad	.LBB36_139
	.quad	.LBB36_140
	.quad	.LBB36_141
	.quad	.LBB36_142
	.quad	.LBB36_143
	.quad	.LBB36_144
.LJTI36_5:
	.quad	.LBB36_168
	.quad	.LBB36_169
	.quad	.LBB36_170
	.quad	.LBB36_171
	.quad	.LBB36_172
	.quad	.LBB36_173
	.quad	.LBB36_174
	.quad	.LBB36_175
	.quad	.LBB36_176
	.quad	.LBB36_177
	.quad	.LBB36_178
	.quad	.LBB36_189
	.quad	.LBB36_183
	.quad	.LBB36_183
	.quad	.LBB36_185

	.text
	.globl	debug_dwarf
	.align	16, 0x90
	.type	debug_dwarf,@function
debug_dwarf:                            # @debug_dwarf
	.cfi_startproc
# BB#0:                                 # %entry
	movq	comp_unit_die(%rip), %rdi
	movq	stderr(%rip), %rsi
	xorq	%rax, %rax
	movl	%eax, print_indent(%rip)
	jmp	print_die  # TAILCALL
.Ltmp369:
	.size	debug_dwarf, .Ltmp369-debug_dwarf
	.cfi_endproc

	.globl	dwarf2out_add_library_unit_info
	.align	16, 0x90
	.type	dwarf2out_add_library_unit_info,@function
dwarf2out_add_library_unit_info:        # @dwarf2out_add_library_unit_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp376:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp377:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp378:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp379:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp380:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp381:
	.cfi_def_cfa_offset 64
.Ltmp382:
	.cfi_offset %rbx, -48
.Ltmp383:
	.cfi_offset %r12, -40
.Ltmp384:
	.cfi_offset %r13, -32
.Ltmp385:
	.cfi_offset %r14, -24
.Ltmp386:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB38_23
# BB#1:                                 # %if.then
	movq	comp_unit_die(%rip), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	testq	%rbx, %rbx
	movq	$30, %rax
	movl	%eax, (%r12)
	je	.LBB38_5
# BB#2:                                 # %if.then.i
	testq	%r12, %r12
	je	.LBB38_6
# BB#3:                                 # %if.then.i.i
	cmpq	%rbx, %r12
	je	.LBB38_24
# BB#4:                                 # %if.end.i.i
	movq	%rbx, 24(%r12)
	movq	32(%rbx), %rax
	movq	%rax, 40(%r12)
	movq	%r12, 32(%rbx)
	jmp	.LBB38_6
.LBB38_5:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r12, (%rax)
	movq	$0, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB38_6:                               # %new_die.exit
	movq	%r15, %rdi
	callq	get_identifier
	movq	global_trees+216(%rip), %rdx
	movq	$31, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	16(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	testq	%r15, %r15
	movl	%ecx, 16(%rax)
	je	.LBB38_15
# BB#7:                                 # %land.lhs.true.i
	movzbl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB38_15
# BB#8:                                 # %if.then.i10
	movq	demangle_name_func(%rip), %rcx
	movq	%r14, %rbx
	testq	%rcx, %rcx
	je	.LBB38_10
# BB#9:                                 # %if.then3.i
	movq	%r15, %rdi
	callq	*%rcx
	movq	%rax, %r15
.LBB38_10:                              # %if.end.i
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r14
	movq	%rax, %r13
	testq	%r14, %r14
	jne	.LBB38_12
# BB#11:                                # %if.then.i.i12
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r14
.LBB38_12:                              # %if.end.i.i13
	movq	%r15, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%r13)
	movq	$3, %rcx
	movl	%ecx, (%r13)
	movq	$14, %rcx
	movl	%ecx, 16(%r13)
	movq	%rax, 24(%r13)
	testq	%r12, %r12
	movq	%rbx, %r14
	je	.LBB38_15
# BB#13:                                # %if.end.i.i13
	testq	%r13, %r13
	je	.LBB38_15
# BB#14:                                # %if.then.i.i.i
	movq	16(%r12), %rax
	movq	%rax, 8(%r13)
	movq	%r13, 16(%r12)
.LBB38_15:                              # %add_name_attribute.exit
	movq	%r14, %rdi
	callq	lookup_filename
	movq	%rax, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	testq	%r12, %r12
	je	.LBB38_18
# BB#16:                                # %add_name_attribute.exit
	testq	%rax, %rax
	je	.LBB38_18
# BB#17:                                # %if.then.i.i16
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
.LBB38_18:                              # %add_AT_unsigned.exit
	movq	8(%rsp), %r15           # 8-byte Reload
	movzbl	18(%r15), %eax
	testq	$8, %rax
	je	.LBB38_23
# BB#19:                                # %if.end.i19
	movl	pubname_table_in_use(%rip), %esi
	movl	pubname_table_allocated(%rip), %eax
	cmpq	%rax, %rsi
	jne	.LBB38_20
# BB#21:                                # %if.then1.i
	addq	$64, %rsi
	movl	%esi, pubname_table_allocated(%rip)
	movq	pubname_table(%rip), %rdi
	andq	%r14, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	pubname_table_in_use(%rip), %esi
	movq	%rax, %rbx
	movq	%rbx, pubname_table(%rip)
	jmp	.LBB38_22
.LBB38_20:                              # %if.end.if.end2_crit_edge.i
	movq	pubname_table(%rip), %rbx
.LBB38_22:                              # %if.end2.i
	leaq	1(%rsi), %rax
	movl	%eax, pubname_table_in_use(%rip)
	andq	%rsi, %r14
	movq	$4, %rcx
	shlq	%cl, %r14
	movq	%r12, (%rbx,%r14)
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rdi
	callq	xstrdup
	movq	%rax, 8(%rbx,%r14)
.LBB38_23:                              # %if.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB38_24:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp387:
	.size	dwarf2out_add_library_unit_info, .Ltmp387-dwarf2out_add_library_unit_info
	.cfi_endproc

	.align	16, 0x90
	.type	lookup_filename,@function
lookup_filename:                        # @lookup_filename
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp394:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp395:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp396:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp397:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp398:
	.cfi_def_cfa_offset 48
.Ltmp399:
	.cfi_offset %rbx, -48
.Ltmp400:
	.cfi_offset %r12, -40
.Ltmp401:
	.cfi_offset %r13, -32
.Ltmp402:
	.cfi_offset %r14, -24
.Ltmp403:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$.L.str411, %rsi
                                        # kill: RDI<def> R14<kill>
	callq	strcmp
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB39_12
# BB#1:                                 # %lor.lhs.false
	movabsq	$.L.str412, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB39_12
# BB#2:                                 # %if.end
	movl	file_table.3(%rip), %ebx
	testq	%rbx, %rbx
	je	.LBB39_4
# BB#3:                                 # %if.then4
	movq	file_table.0(%rip), %rax
	movq	(%rax,%rbx,8), %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB39_12
.LBB39_4:                               # %for.cond.preheader
	movl	file_table.2(%rip), %r12d
	cmpq	$2, %r12
	movq	$1, %rbx
	jb	.LBB39_9
# BB#5:                                 # %for.body.lr.ph
	movq	file_table.0(%rip), %r13
	movq	$1, %rbx
	.align	16, 0x90
.LBB39_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13,%rbx,8), %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB39_8
# BB#6:                                 # %for.cond
                                        #   in Loop: Header=BB39_7 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%r12, %rax
	jb	.LBB39_7
.LBB39_9:                               # %for.end
	movl	file_table.1(%rip), %eax
	movq	%rbx, %r12
	andq	%r15, %r12
	cmpq	%rax, %r12
	jne	.LBB39_11
# BB#10:                                # %if.then18
	leaq	64(%rbx), %rax
	movl	%eax, file_table.1(%rip)
	movq	file_table.0(%rip), %rdi
	andq	%rax, %r15
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	%r15, %rsi
	callq	xrealloc
	movq	%rax, file_table.0(%rip)
.LBB39_11:                              # %if.end20
	movq	%r14, %rdi
	callq	xstrdup
	movq	file_table.0(%rip), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	asm_out_file(%rip), %rdi
	leaq	1(%rbx), %rax
	movl	%eax, file_table.2(%rip)
	movl	%ebx, file_table.3(%rip)
	movabsq	$.L.str413, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	jmp	.LBB39_12
.LBB39_8:                               # %if.then15
	movl	%ebx, file_table.3(%rip)
.LBB39_12:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp404:
	.size	lookup_filename, .Ltmp404-lookup_filename
	.cfi_endproc

	.align	16, 0x90
	.type	modified_type_die,@function
modified_type_die:                      # @modified_type_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp411:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp412:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp413:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp414:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp415:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp416:
	.cfi_def_cfa_offset 80
.Ltmp417:
	.cfi_offset %rbx, -48
.Ltmp418:
	.cfi_offset %r12, -40
.Ltmp419:
	.cfi_offset %r13, -32
.Ltmp420:
	.cfi_offset %r14, -24
.Ltmp421:
	.cfi_offset %r15, -16
	movzbl	16(%rdi), %r13d
	movq	%rcx, %rax
	movq	%rsi, %r14
	xorq	%rsi, %rsi
	testq	%r13, %r13
	je	.LBB40_1
# BB#2:                                 # %if.then
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r14
	movabsq	$0, %rsi
	movabsq	$1, %rcx
	movq	%rsi, %rax
	je	.LBB40_4
# BB#3:                                 # %if.then
	movq	%rcx, %rax
.LBB40_4:                               # %if.then
	testq	%rbx, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	je	.LBB40_6
# BB#5:                                 # %if.then
	movq	%rcx, %rsi
.LBB40_6:                               # %if.then
	addq	%rsi, %rsi
	orq	%rax, %rsi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	callq	get_qualified_type
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB40_17
# BB#7:                                 # %if.then5
	movq	88(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB40_105
# BB#8:                                 # %land.lhs.true
	movq	96(%rbx), %rax
	testq	%rax, %rax
	je	.LBB40_17
# BB#9:                                 # %land.lhs.true13
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB40_16
# BB#10:                                # %land.lhs.true21
	movq	96(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB40_16
# BB#11:                                # %if.then25
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB40_12
# BB#13:                                # %if.else
	movl	16(%rax), %r8d
	movq	$12, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB40_15
# BB#14:                                # %lor.lhs.false
	movq	$11, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	jle	.LBB40_16
.LBB40_15:                              # %if.then42
	movq	%r14, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	modified_type_die
	movq	%rax, %rsi
	jmp	.LBB40_16
.LBB40_1:
	movq	%rdi, %rbx
	xorq	%r12, %r12
	xorq	%r15, %r15
	jmp	.LBB40_48
.LBB40_12:                              # %if.then31
	movq	%rbx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	gen_type_die
	movq	88(%rbx), %rsi
.LBB40_16:                              # %if.end48
	xorq	%r15, %r15
	testq	%rsi, %rsi
	movabsq	$0, %r12
	jne	.LBB40_47
.LBB40_17:                              # %if.else51
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB40_27
# BB#18:                                # %if.then53
	movq	%rbx, %r14
	movq	comp_unit_die(%rip), %r15
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	testq	%r15, %r15
	movq	$38, %rax
	movl	%eax, (%rbx)
	je	.LBB40_24
# BB#19:                                # %if.then.i
	testq	%rbx, %rbx
	movq	8(%rsp), %rdi           # 8-byte Reload
	je	.LBB40_25
# BB#20:                                # %if.then.i.i
	cmpq	%r15, %rbx
	je	.LBB40_21
# BB#23:                                # %if.end.i.i
	movq	%r15, 24(%rbx)
	movq	32(%r15), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r15)
	jmp	.LBB40_25
.LBB40_27:                              # %if.else56
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB40_34
# BB#28:                                # %if.then58
	movq	%rbx, %r14
	movq	comp_unit_die(%rip), %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	testq	%r12, %r12
	movq	$53, %rax
	movl	%eax, (%rbx)
	movq	8(%rsp), %r15           # 8-byte Reload
	je	.LBB40_32
# BB#29:                                # %if.then.i180
	testq	%rbx, %rbx
	je	.LBB40_33
# BB#30:                                # %if.then.i.i182
	cmpq	%r12, %rbx
	je	.LBB40_21
# BB#31:                                # %if.end.i.i187
	movq	%r12, 24(%rbx)
	movq	32(%r12), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r12)
	jmp	.LBB40_33
.LBB40_24:                              # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB40_25:                              # %new_die.exit
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	16(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB40_26
.LBB40_34:                              # %if.else61
	cmpq	$15, %r13
	movq	8(%rsp), %r15           # 8-byte Reload
	jne	.LBB40_35
# BB#53:                                # %if.then72
	movq	%rbx, %r14
	movq	comp_unit_die(%rip), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rdx
	testq	%rbx, %rbx
	movq	$16, %rax
	jmp	.LBB40_37
.LBB40_32:                              # %if.else.i192
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	%r15, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB40_33:                              # %new_die.exit193
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r15, %rdi
.LBB40_26:                              # %if.end99
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	modified_type_die
	movq	%rbx, %rsi
	movq	%rax, %r15
.LBB40_46:                              # %if.end99
	movq	%r14, %rbx
.LBB40_47:                              # %if.end99
	testq	%rbx, %rbx
	movq	24(%rsp), %rax          # 8-byte Reload
	je	.LBB40_49
.LBB40_48:                              # %if.then101
	movq	%rsi, 88(%rbx)
.LBB40_49:                              # %if.end102
	movq	%rsi, %rbx
	testq	%r12, %r12
	je	.LBB40_51
# BB#50:                                # %if.then104
	movl	16(%r12), %edx
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r12, %rdi
	movq	%rax, %rcx
	callq	modified_type_die
	movq	%rax, %r15
.LBB40_51:                              # %if.end116
	testq	%r15, %r15
	je	.LBB40_52
# BB#102:                               # %if.then119
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	movq	%rbx, %rsi
	testq	%rsi, %rsi
	je	.LBB40_105
# BB#103:                               # %if.then119
	testq	%rax, %rax
	je	.LBB40_105
# BB#104:                               # %if.then.i.i269
	movq	16(%rsi), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rsi)
	jmp	.LBB40_105
.LBB40_52:
	movq	%rbx, %rsi
.LBB40_105:                             # %return
	movq	%rsi, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB40_35:                              # %if.else61
	cmpq	$13, %r13
	jne	.LBB40_54
# BB#36:                                # %if.then63
	movq	%rbx, %r14
	movq	comp_unit_die(%rip), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rdx
	testq	%rbx, %rbx
	movq	$15, %rax
.LBB40_37:                              # %if.then63
	movl	%eax, (%rdx)
	je	.LBB40_41
# BB#38:                                # %if.then.i198
	testq	%rdx, %rdx
	je	.LBB40_42
# BB#39:                                # %if.then.i.i200
	cmpq	%rbx, %rdx
	je	.LBB40_21
# BB#40:                                # %if.end.i.i205
	movq	%rbx, 24(%rdx)
	movq	32(%rbx), %rax
	movq	%rax, 40(%rdx)
	movq	%rdx, 32(%rbx)
	jmp	.LBB40_42
.LBB40_41:                              # %if.else.i210
	movq	$24, %rdi
	movq	%rdx, %rbx
	callq	xmalloc
	movq	%rbx, %rdx
	movq	limbo_die_list(%rip), %rcx
	movq	%rdx, (%rax)
	movq	%r15, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB40_42:                              # %new_die.exit211
	movl	target_flags(%rip), %ebx
	movq	$20, %rcx
	shrq	%cl, %rbx
	andq	$32, %rbx
	addq	$32, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	$40, %rdi
	movq	%rdx, %r12
	callq	xmalloc
	movq	%r12, %rsi
	movq	$0, 8(%rax)
	movq	$11, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	testq	%rsi, %rsi
	je	.LBB40_45
# BB#43:                                # %new_die.exit211
	testq	%rax, %rax
	je	.LBB40_45
# BB#44:                                # %if.then.i.i214
	movq	16(%rsi), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rsi)
.LBB40_45:                              # %add_AT_unsigned.exit
	movq	8(%r15), %r12
	xorq	%r15, %r15
	jmp	.LBB40_46
.LBB40_54:                              # %if.else81
	movq	%rax, %rdi
	movl	16(%r15), %r13d
	movq	%r13, %rcx
	andq	$255, %rcx
	cmpq	$24, %rcx
	ja	.LBB40_106
# BB#55:                                # %if.else81
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$33547776, %rdx         # imm = 0x1FFE600
	je	.LBB40_56
# BB#97:                                # %if.else86
	movq	%r15, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	gen_type_die
	movq	128(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB40_98
.LBB40_99:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	128(%rcx), %rax
	cmpq	%rax, %rcx
	jne	.LBB40_99
	jmp	.LBB40_100
.LBB40_56:                              # %if.else81
	shlq	%cl, %rax
	testq	$6625, %rax             # imm = 0x19E1
	je	.LBB40_106
# BB#57:                                # %if.then84
	movq	%r15, %rax
	xorq	%r15, %r15
	testq	%rcx, %rcx
	jne	.LBB40_59
# BB#58:
	xorq	%r12, %r12
	xorq	%rsi, %rsi
	jmp	.LBB40_47
.LBB40_98:
	movq	%rax, %rcx
.LBB40_100:                             # %type_main_variant.exit
	movq	88(%rcx), %rsi
	xorq	%r15, %r15
	testq	%rsi, %rsi
	movabsq	$0, %r12
	jne	.LBB40_47
# BB#101:                               # %if.then91
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.modified_type_die, %rcx
	movq	$7573, %rsi             # imm = 0x1D95
	jmp	.LBB40_22
.LBB40_59:                              # %if.then84
	cmpq	$5, %rcx
	movabsq	$0, %r12
	movabsq	$0, %rsi
	je	.LBB40_47
# BB#60:                                # %if.end.i
	movq	%rax, %r15
	movq	96(%r15), %rax
	testq	%rax, %rax
	je	.LBB40_61
# BB#62:                                # %if.then8.i
	movzbl	16(%rax), %edx
	cmpq	$33, %rdx
	jne	.LBB40_64
# BB#63:                                # %if.then14.i
	movq	72(%rax), %rax
.LBB40_64:                              # %if.end16.i
	movq	%rdi, %rdx
	movq	32(%rax), %r14
	jmp	.LBB40_65
.LBB40_106:                             # %sw.default.i
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.is_base_type, %rdx
	movq	$7461, %rsi             # imm = 0x1D25
	callq	fancy_abort
.LBB40_61:
	movabsq	$.L.str416, %r14
	movq	%rdi, %rdx
.LBB40_65:                              # %if.end17.i
	addq	$-6, %rcx
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	ja	.LBB40_75
# BB#66:                                # %if.end17.i
	movq	$4, %r12
	jmpq	*.LJTI40_0(,%rcx,8)
.LBB40_67:                              # %sw.bb.i
	movl	60(%r15), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	$8, %rax
	jne	.LBB40_71
# BB#68:                                # %land.lhs.true.i
	movq	integer_types(%rip), %rax
	cmpq	%r15, %rax
	je	.LBB40_76
# BB#69:                                # %lor.lhs.false27.i
	movabsq	$.L.str417, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%rdx, %r12
	callq	strcmp
	movq	%r12, %rcx
	testq	%rcx, %rax
	je	.LBB40_76
# BB#70:                                # %lor.lhs.false29.i
	movabsq	$.L.str418, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%rcx, %r12
	callq	strcmp
	testq	%r12, %rax
	je	.LBB40_76
.LBB40_71:                              # %if.then32.i
	andq	$8192, %r13             # imm = 0x2000
	movq	$12, %rcx
	shrq	%cl, %r13
	orq	$5, %r13
	jmp	.LBB40_77
.LBB40_72:                              # %sw.bb52.i
	movq	8(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	movq	$3, %r12
	je	.LBB40_78
# BB#73:                                # %sw.bb52.i
	movq	$128, %r12
	jmp	.LBB40_78
.LBB40_74:                              # %sw.bb63.i
	movq	$2, %r12
	jmp	.LBB40_78
.LBB40_76:                              # %sw.bb41.i
	andq	$8192, %r13             # imm = 0x2000
	movq	$12, %rcx
	shrq	%cl, %r13
	addq	$6, %r13
.LBB40_77:                              # %sw.epilog.i
	movq	%r13, %r12
.LBB40_78:                              # %sw.epilog.i
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	comp_unit_die(%rip), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rdx
	testq	%rbx, %rbx
	movq	$36, %rax
	movl	%eax, (%rdx)
	je	.LBB40_82
# BB#79:                                # %if.then.i.i250
	testq	%rdx, %rdx
	je	.LBB40_83
# BB#80:                                # %if.then.i.i.i
	cmpq	%rbx, %rdx
	je	.LBB40_21
# BB#81:                                # %if.end.i.i.i
	movq	%rbx, 24(%rdx)
	movq	32(%rbx), %rax
	movq	%rax, 40(%rdx)
	movq	%rdx, 32(%rbx)
	jmp	.LBB40_83
.LBB40_82:                              # %if.else.i.i
	movq	$24, %rdi
	movq	%rdx, %rbx
	callq	xmalloc
	movq	%rbx, %rdx
	movq	limbo_die_list(%rip), %rcx
	movq	%rdx, (%rax)
	movq	%r15, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB40_83:                              # %new_die.exit.i
	movq	demangle_name_func(%rip), %rcx
	movq	%rdx, %r13
	testq	%rcx, %rcx
	je	.LBB40_85
# BB#84:                                # %if.then66.i
	movq	%r14, %rdi
	callq	*%rcx
	movq	%rax, %r14
.LBB40_85:                              # %if.end68.i
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r15
	movq	%rax, %rbx
	testq	%r15, %r15
	jne	.LBB40_87
# BB#86:                                # %if.then.i93.i
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r15
.LBB40_87:                              # %if.end.i.i251
	movq	%r14, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$3, %rcx
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r13, %r13
	je	.LBB40_90
# BB#88:                                # %if.end.i.i251
	testq	%rbx, %rbx
	je	.LBB40_90
# BB#89:                                # %if.then.i.i95.i
	movq	16(%r13), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r13)
.LBB40_90:                              # %add_AT_string.exit.i
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	int_size_in_bytes
	movq	%rax, %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	%r13, %rdx
	testq	%rdx, %rdx
	movq	$0, 8(%rax)
	movq	$11, %rcx
	movl	%ecx, (%rax)
	movq	$6, %r14
	movl	%r14d, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB40_93
# BB#91:                                # %add_AT_string.exit.i
	testq	%rax, %rax
	je	.LBB40_93
# BB#92:                                # %if.then.i.i106.i
	movq	16(%rdx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rdx)
.LBB40_93:                              # %add_AT_unsigned.exit.i
	movq	$40, %rdi
	movq	%rdx, %rbx
	callq	xmalloc
	movq	%rbx, %rsi
	xorq	%r15, %r15
	testq	%rsi, %rsi
	movq	$0, 8(%rax)
	movq	$62, %rcx
	movl	%ecx, (%rax)
	movl	%r14d, 16(%rax)
	movq	%r12, 24(%rax)
	je	.LBB40_94
# BB#95:                                # %add_AT_unsigned.exit.i
	testq	%rax, %rax
	movabsq	$0, %r12
	movq	(%rsp), %rbx            # 8-byte Reload
	je	.LBB40_47
# BB#96:                                # %if.then.i.i118.i
	movq	16(%rsi), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rsi)
	xorq	%r15, %r15
	xorq	%r12, %r12
	jmp	.LBB40_47
.LBB40_94:
	xorq	%r12, %r12
	movq	(%rsp), %rbx            # 8-byte Reload
	jmp	.LBB40_47
.LBB40_75:                              # %sw.default.i248
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.base_type_die, %rcx
	movq	$7381, %rsi             # imm = 0x1CD5
	jmp	.LBB40_22
.LBB40_21:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
.LBB40_22:                              # %if.then3.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp422:
	.size	modified_type_die, .Ltmp422-modified_type_die
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI40_0:
	.quad	.LBB40_67
	.quad	.LBB40_78
	.quad	.LBB40_72
	.quad	.LBB40_75
	.quad	.LBB40_75
	.quad	.LBB40_74
	.quad	.LBB40_76

	.text
	.align	16, 0x90
	.type	gen_decl_die,@function
gen_decl_die:                           # @gen_decl_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp429:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp430:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp431:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp432:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp433:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp434:
	.cfi_def_cfa_offset 96
.Ltmp435:
	.cfi_offset %rbx, -48
.Ltmp436:
	.cfi_offset %r12, -40
.Ltmp437:
	.cfi_offset %r13, -32
.Ltmp438:
	.cfi_offset %r14, -24
.Ltmp439:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %edx
	movq	%rsi, %r14
	cmpq	$100, %rdx
	jne	.LBB41_2
# BB#1:                                 # %land.lhs.true
	movzbl	49(%rbx), %edx
	testq	$64, %rdx
	jne	.LBB41_298
.LBB41_2:                               # %if.end
	cmpq	$29, %rcx
	jg	.LBB41_5
# BB#3:                                 # %if.end
	testq	%rcx, %rcx
	je	.LBB41_298
	jmp	.LBB41_4
.LBB41_5:                               # %if.end
	addq	$-30, %rcx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rcx
	cmpq	$8, %rcx
	ja	.LBB41_4
# BB#6:                                 # %if.end
	jmpq	*.LJTI41_0(,%rcx,8)
.LBB41_7:                               # %sw.bb
	movq	104(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB41_11
# BB#8:                                 # %land.lhs.true12
	cmpq	$0, 80(%rbx)
	jne	.LBB41_11
# BB#9:                                 # %land.lhs.true16
	cmpq	$0, current_function_decl(%rip)
	je	.LBB41_298
# BB#10:                                # %lor.lhs.false
	movzbl	50(%rbx), %ecx
	testq	$64, %rcx
	jne	.LBB41_298
.LBB41_11:                              # %if.end26
	movq	112(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rcx
	je	.LBB41_13
# BB#12:                                # %if.end26
	movq	%rdi, %rcx
.LBB41_13:                              # %if.end26
	cmpq	%rbx, %rcx
	je	.LBB41_15
# BB#14:                                # %if.then33
	callq	dwarf2out_abstract_function
	jmp	.LBB41_30
.LBB41_168:                             # %sw.bb134
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB41_298
# BB#169:                               # %if.then137
	movq	48(%rbx), %rdx
	movq	112(%rbx), %rax
	movq	$15, %rcx
	shrq	%cl, %rdx
	cmpq	%rbx, %rax
	jne	.LBB41_170
# BB#171:                               # %if.then137
	movabsq	$1, %rcx
	jmp	.LBB41_172
.LBB41_31:                              # %sw.bb79
	movl	debug_info_level(%rip), %ecx
	cmpq	$2, %rcx
	jb	.LBB41_298
# BB#32:                                # %if.end83
	movq	72(%rbx), %r10
	movq	48(%rbx), %rdx
	testq	%r10, %r10
	je	.LBB41_45
# BB#33:                                # %lor.lhs.false87
	testq	$4194304, %rdx          # imm = 0x400000
	je	.LBB41_99
# BB#34:                                # %land.lhs.true95
	movq	8(%rbx), %rcx
	movl	16(%rcx), %edi
	movq	%rdi, %rsi
	andq	$254, %rsi
	cmpq	$20, %rsi
	je	.LBB41_37
# BB#35:                                # %land.lhs.true95
	andq	$255, %rdi
	cmpq	$22, %rdi
	je	.LBB41_37
# BB#36:                                # %land.lhs.true95
	cmpq	$10, %rdi
	jne	.LBB41_97
.LBB41_37:                              # %land.lhs.true100
	movq	(%rcx), %rdi
	cmpq	%rbx, %rdi
	je	.LBB41_45
# BB#38:                                # %lor.lhs.false106
	movq	112(%rbx), %r8
	testq	%r8, %r8
	je	.LBB41_97
# BB#39:                                # %land.lhs.true111
	movq	$15, %rcx
	movq	%rdx, %r9
	shrq	%cl, %r9
	cmpq	%rbx, %r8
	jne	.LBB41_40
# BB#41:                                # %land.lhs.true111
	movabsq	$1, %rcx
	jmp	.LBB41_42
.LBB41_206:                             # %sw.bb139
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB41_298
# BB#207:                               # %if.end143
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB41_209
# BB#208:                               # %lor.lhs.false.i
	cmpq	$0, 184(%rbx)
	je	.LBB41_209
# BB#210:                               # %if.else.i
	movq	8(%rbx), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	addq	$128, %rax
	jmp	.LBB41_211
.LBB41_296:                             # %sw.bb182
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
.LBB41_221:                             # %if.then161
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_formal_parameter_die
	jmp	.LBB41_298
.LBB41_222:                             # %sw.bb165
	cmpq	$0, 72(%rbx)
	jne	.LBB41_224
# BB#223:                               # %lor.lhs.false170
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$21, %rax
	jne	.LBB41_298
.LBB41_224:                             # %if.then179
	movq	96(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB41_226
# BB#225:                               # %cond.false.i
	movq	8(%rbx), %rdi
.LBB41_226:                             # %member_declared_type.exit
	movq	%r14, %rsi
	callq	gen_type_die
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	testq	%r14, %r14
	movq	$13, %rax
	movl	%eax, (%r15)
	je	.LBB41_230
# BB#227:                               # %if.then.i.i442
	testq	%r15, %r15
	je	.LBB41_231
# BB#228:                               # %if.then.i.i.i444
	cmpq	%r14, %r15
	je	.LBB41_68
# BB#229:                               # %if.end.i.i.i449
	movq	%r14, 24(%r15)
	movq	32(%r14), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r14)
	jmp	.LBB41_231
.LBB41_4:                               # %sw.default
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.gen_decl_die, %rdx
	movq	$11553, %rsi            # imm = 0x2D21
	callq	fancy_abort
.LBB41_209:                             # %if.then.i
	leaq	80(%rbx), %rax
.LBB41_211:                             # %if.end.i
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB41_214
# BB#212:                               # %land.lhs.true.i
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB41_214
# BB#213:                               # %if.then149
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_type_die_for_member
.LBB41_214:                             # %if.end150
	movzbl	49(%rbx), %eax
	movq	112(%rbx), %rdx
	movq	$7, %rcx
	shrq	%cl, %rax
	cmpq	%rbx, %rdx
	jne	.LBB41_215
# BB#216:                               # %if.end150
	movabsq	$1, %rsi
	jmp	.LBB41_217
.LBB41_15:                              # %if.else
	movq	48(%rbx), %rcx
	andq	$34816, %rcx            # imm = 0x8800
	cmpq	$2048, %rcx             # imm = 0x800
	jne	.LBB41_20
# BB#16:                                # %land.lhs.true49
	testq	%r14, %r14
	je	.LBB41_18
# BB#17:                                # %land.rhs.i
	movl	(%r14), %ecx
	movabsq	$4294967291, %rdx       # imm = 0xFFFFFFFB
	andq	%rcx, %rdx
	cmpq	$19, %rdx
	je	.LBB41_20
.LBB41_18:                              # %land.lhs.true51
	testq	%rax, %rax
	je	.LBB41_20
# BB#19:                                # %if.then56
	movq	%rbx, %rdi
	callq	dwarf2out_abstract_function
	movq	%rbx, %rdi
	callq	set_decl_origin_self
	jmp	.LBB41_30
.LBB41_230:                             # %if.else.i.i454
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB41_231:                             # %new_die.exit.i456
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	add_name_and_src_coords_attributes
	movq	96(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB41_233
# BB#232:                               # %cond.false.i.i
	movq	8(%rbx), %rdi
.LBB41_233:                             # %member_declared_type.exit.i
	movl	16(%rdi), %eax
	movl	16(%rbx), %r8d
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rax, %rdx
	andq	$254, %rdx
	cmpq	$6, %rdx
	jne	.LBB41_237
# BB#234:                               # %land.lhs.true.i.i462
	movq	8(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB41_237
# BB#235:                               # %land.lhs.true6.i.i464
	cmpq	$0, 96(%rdi)
	jne	.LBB41_237
# BB#236:                               # %if.then.i137.i
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rdx, %rdi
.LBB41_237:                             # %if.end.i138.i
	testq	%rcx, %rcx
	je	.LBB41_247
# BB#238:                               # %if.end.i138.i
	cmpq	$5, %rcx
	je	.LBB41_247
# BB#239:                               # %if.end19.i.i470
	movq	%r8, %rcx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$2048, %r8              # imm = 0x800
	movq	$1, %rdx
	testq	%r13, %rcx
	movq	$1, %rsi
	jne	.LBB41_241
# BB#240:                               # %lor.rhs.i.i475
	movq	%rax, %rsi
	andq	$4096, %rsi             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rsi
.LBB41_241:                             # %lor.end.i.i478
	testq	%r13, %r8
	jne	.LBB41_243
# BB#242:                               # %lor.rhs25.i.i483
	andq	$2048, %rax             # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %rax
	movq	%rax, %rdx
.LBB41_243:                             # %lor.end31.i.i486
	movq	%r14, %rcx
	callq	modified_type_die
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB41_247
# BB#244:                               # %if.then34.i.i489
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB41_247
# BB#245:                               # %if.then34.i.i489
	testq	%rax, %rax
	je	.LBB41_247
# BB#246:                               # %if.then.i.i.i151.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB41_247:                             # %add_type_attribute.exit.i493
	movq	96(%rbx), %r12
	testq	%r12, %r12
	je	.LBB41_286
# BB#248:                               # %if.then.i497
	movzbl	16(%rbx), %eax
	cmpq	$36, %rax
	jg	.LBB41_253
# BB#249:                               # %if.then.i497
	leaq	-20(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$3, %rcx
	jb	.LBB41_252
# BB#250:                               # %if.then.i497
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB41_261
# BB#251:                               # %if.then.i497
	cmpq	$10, %rax
	jne	.LBB41_299
.LBB41_252:                             # %sw.bb1.i.i
	movq	%rbx, %rdi
	callq	int_size_in_bytes
	movq	%rax, %r14
	jmp	.LBB41_261
.LBB41_170:
	movabsq	$0, %rcx
.LBB41_172:                             # %if.then137
	xorq	%r12, %r12
	testq	%rcx, %rdx
	jne	.LBB41_174
# BB#173:                               # %if.then137
	movq	%rax, %r12
.LBB41_174:                             # %if.then137
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$10, %rax
	movl	%eax, (%r15)
	testq	%r14, %r14
	je	.LBB41_178
# BB#175:                               # %if.then.i.i364
	testq	%r15, %r15
	je	.LBB41_179
# BB#176:                               # %if.then.i.i.i366
	cmpq	%r14, %r15
	je	.LBB41_68
# BB#177:                               # %if.end.i.i.i371
	movq	%r14, 24(%r15)
	movq	32(%r14), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r14)
	jmp	.LBB41_179
.LBB41_253:                             # %if.then.i497
	cmpq	$37, %rax
	jne	.LBB41_299
# BB#254:                               # %field_type.exit.i.i
	movzbl	16(%r12), %eax
	testq	%rax, %rax
	je	.LBB41_255
# BB#256:                               # %if.else.i645
	movq	32(%r12), %rdi
	xorq	%r14, %r14
	testq	%rdi, %rdi
	je	.LBB41_260
# BB#257:                               # %if.else5.i
	movq	$1, %rsi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB41_259
# BB#258:                               # %if.then9.i648
	movq	32(%r12), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r14
	jmp	.LBB41_260
.LBB41_215:
	movabsq	$0, %rsi
.LBB41_217:                             # %if.end150
	xorq	%rcx, %rcx
	testq	%rsi, %rax
	jne	.LBB41_219
# BB#218:                               # %if.end150
	movq	%rdx, %rcx
.LBB41_219:                             # %if.end150
	testq	%rcx, %rcx
	je	.LBB41_297
# BB#220:                               # %land.lhs.true154
	movzbl	16(%rcx), %eax
	cmpq	$35, %rax
	je	.LBB41_221
.LBB41_297:                             # %if.else163
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_variable_die
	jmp	.LBB41_298
.LBB41_20:                              # %if.else57
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB41_30
# BB#21:                                # %if.then60
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	cmpq	$0, 184(%rbx)
	je	.LBB41_23
# BB#22:                                # %if.then67
	movq	80(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
.LBB41_23:                              # %if.end70
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB41_25
# BB#24:                                # %lor.lhs.false.i293
	cmpq	$0, 184(%rbx)
	je	.LBB41_25
# BB#26:                                # %if.else.i300
	movq	8(%rbx), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	addq	$128, %rax
	jmp	.LBB41_27
.LBB41_178:                             # %if.else.i.i376
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB41_179:                             # %new_die.exit.i378
	testq	%r12, %r12
	je	.LBB41_194
# BB#180:                               # %if.then.i381
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	je	.LBB41_185
# BB#181:                               # %if.then.i56.i
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%r12, %rdi
	jne	.LBB41_183
# BB#182:                               # %if.then7.i.i385
	movq	(%r12), %rdi
.LBB41_183:                             # %if.end.i59.i
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB41_185
# BB#184:                               # %if.then9.i60.i
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB41_185:                             # %if.end11.i.i391
	movzbl	16(%r12), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB41_186
# BB#189:                               # %if.then32.i.i402
	movq	88(%r12), %r14
	jmp	.LBB41_190
.LBB41_194:                             # %if.else.i408
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	add_name_and_src_coords_attributes
	jmp	.LBB41_195
.LBB41_186:                             # %if.end11.i.i391
	cmpq	$100, %rax
	jne	.LBB41_82
# BB#187:                               # %if.then21.i.i394
	movl	36(%r12), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB41_82
# BB#188:                               # %cond.true.i.i.i397
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
.LBB41_190:                             # %if.end35.i.i405
	testq	%r14, %r14
	je	.LBB41_82
# BB#191:                               # %add_abstract_origin_attribute.exit.i407
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$49, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB41_195
# BB#192:                               # %add_abstract_origin_attribute.exit.i407
	testq	%rax, %rax
	je	.LBB41_195
# BB#193:                               # %if.then.i.i637
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB41_195:                             # %if.end.i413
	movzbl	49(%rbx), %eax
	testq	$128, %rax
	jne	.LBB41_163
# BB#196:                               # %if.else4.i
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB41_198
# BB#197:                               # %cond.false.i433
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB41_198:                             # %cond.end.i
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$36, %rdx
	je	.LBB41_201
# BB#199:                               # %cond.end.i
	cmpq	$37, %rdx
	jne	.LBB41_298
# BB#200:                               # %land.lhs.true.i436
	movl	40(%rax), %edx
	movabsq	$4294967208, %rsi       # imm = 0xFFFFFFA8
	cmpq	%rsi, %rdx
	jne	.LBB41_298
.LBB41_201:                             # %if.then18.i
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB41_202
# BB#203:                               # %do.body.i
	movl	48(%rax), %ecx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str414, %rdx
	leaq	16(%rsp), %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$17, %rax
	movl	%eax, (%rbx)
	movq	$12, %rax
	movl	%eax, 16(%rbx)
	movq	%r14, %rdi
	callq	xstrdup
	movq	%rax, 24(%rbx)
	testq	%r15, %r15
	je	.LBB41_298
# BB#204:                               # %do.body.i
	testq	%rbx, %rbx
	je	.LBB41_298
# BB#205:                               # %if.then.i.i52.i
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
	jmp	.LBB41_298
.LBB41_255:                             # %if.then.i643
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %r14       # imm = 0xFFFFFFE0
	andq	%rax, %r14
	jmp	.LBB41_260
.LBB41_25:                              # %if.then.i295
	leaq	80(%rbx), %rax
.LBB41_27:                              # %if.end.i304
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB41_30
# BB#28:                                # %land.lhs.true.i312
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB41_30
# BB#29:                                # %if.then74
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_type_die_for_member
.LBB41_30:                              # %if.end78
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_subprogram_die
	jmp	.LBB41_298
.LBB41_259:                             # %if.else13.i
	movl	64(%r12), %r14d
.LBB41_260:                             # %simple_type_size_in_bits.exit
	movq	$3, %rcx
	shrq	%cl, %r14
.LBB41_261:                             # %sw.epilog.i.i
	andq	%r13, %r14
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$11, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	testq	%r15, %r15
	je	.LBB41_264
# BB#262:                               # %sw.epilog.i.i
	testq	%rax, %rax
	je	.LBB41_264
# BB#263:                               # %if.then.i.i.i.i509
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB41_264:                             # %add_byte_size_attribute.exit.i
	movzbl	16(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB41_266
# BB#265:                               # %lor.lhs.false.i.i
	cmpq	$0, 96(%rbx)
	je	.LBB41_266
# BB#267:                               # %if.end.i.i511
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB41_271
# BB#268:                               # %if.then4.i.i
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r14
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$13, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	je	.LBB41_271
# BB#269:                               # %if.then4.i.i
	testq	%rax, %rax
	je	.LBB41_271
# BB#270:                               # %if.then.i.i.i79.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB41_271:                             # %add_bit_size_attribute.exit.i
	movq	%rbx, %rdi
	callq	field_byte_offset
	movq	96(%rbx), %r12
	movq	%rax, %r14
	testq	%r12, %r12
	je	.LBB41_273
# BB#272:                               # %lor.lhs.false.i87.i
	movzbl	16(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB41_273
# BB#274:                               # %if.end.i89.i
	movq	%rbx, %rdi
	callq	bit_position
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB41_286
# BB#275:                               # %lor.lhs.false5.i.i
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB41_286
# BB#276:                               # %if.end10.i.i
	movq	%rbx, %rdi
	callq	int_bit_position
	movq	40(%rbx), %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %r14
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movzbl	16(%r12), %ecx
	movq	%rax, %rdx
	testq	%rcx, %rcx
	je	.LBB41_277
# BB#278:                               # %if.else.i.i.i513
	movq	32(%r12), %rdi
	testq	%rdi, %rdi
	movabsq	$0, %rcx
	je	.LBB41_283
# BB#279:                               # %if.else5.i.i.i
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	$1, %rsi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB41_281
# BB#280:                               # %if.then9.i.i.i
	movq	32(%r12), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rcx
	jmp	.LBB41_282
.LBB41_277:                             # %if.then.i.i92.i
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	jmp	.LBB41_283
.LBB41_281:                             # %if.else13.i.i.i
	movl	64(%r12), %ecx
.LBB41_282:                             # %simple_type_size_in_bits.exit.i.i
	movq	(%rsp), %rdx            # 8-byte Reload
.LBB41_283:                             # %simple_type_size_in_bits.exit.i.i
	movq	8(%rsp), %rax           # 8-byte Reload
	subq	%rax, %r14
	subq	%rdx, %r14
	addq	%rcx, %r14
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$12, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	je	.LBB41_286
# BB#284:                               # %simple_type_size_in_bits.exit.i.i
	testq	%rax, %rax
	je	.LBB41_286
# BB#285:                               # %if.then.i.i.i106.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB41_286:                             # %if.end.i518
	movq	80(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$21, %rax
	je	.LBB41_288
# BB#287:                               # %if.then11.i
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	add_data_member_location_attribute
.LBB41_288:                             # %if.end12.i
	movzbl	50(%rbx), %eax
	testq	$64, %rax
	je	.LBB41_292
# BB#289:                               # %if.then18.i527
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r15, %r15
	je	.LBB41_292
# BB#290:                               # %if.then18.i527
	testq	%rax, %rax
	je	.LBB41_292
# BB#291:                               # %if.then.i.i110.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB41_292:                             # %if.end19.i
	movl	16(%rbx), %eax
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB41_294
# BB#293:                               # %if.then25.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$2, 24(%rax)
	jmp	.LBB41_84
.LBB41_294:                             # %if.else.i529
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB41_298
# BB#295:                               # %if.then31.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$3, 24(%rax)
.LBB41_84:                              # %add_abstract_origin_attribute.exit620
	testq	%r15, %r15
	je	.LBB41_298
# BB#85:                                # %add_abstract_origin_attribute.exit620
	testq	%rax, %rax
	je	.LBB41_298
# BB#86:                                # %if.then.i.i690
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	jmp	.LBB41_298
.LBB41_40:
	movabsq	$0, %rcx
.LBB41_42:                              # %land.lhs.true111
	xorq	%rsi, %rsi
	testq	%rcx, %r9
	jne	.LBB41_44
# BB#43:                                # %land.lhs.true111
	movq	%r8, %rsi
.LBB41_44:                              # %land.lhs.true111
	cmpq	%rdi, %rsi
	jne	.LBB41_97
.LBB41_45:                              # %land.lhs.true119
	movq	112(%rbx), %rdi
	movq	$15, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	cmpq	%rbx, %rdi
	jne	.LBB41_46
# BB#47:                                # %land.lhs.true119
	movabsq	$1, %rcx
	jmp	.LBB41_48
.LBB41_46:
	movabsq	$0, %rcx
.LBB41_48:                              # %land.lhs.true119
	testq	%rcx, %rsi
	jne	.LBB41_97
# BB#49:                                # %land.lhs.true119
	testq	%rdi, %rdi
	je	.LBB41_97
# BB#50:                                # %if.then123
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB41_298
# BB#51:                                # %if.then123
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB41_298
# BB#52:                                # %if.end.i321
	movq	128(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB41_53
.LBB41_54:                              # %while.cond.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	128(%rcx), %rax
	cmpq	%rax, %rcx
	jne	.LBB41_54
	jmp	.LBB41_55
.LBB41_97:                              # %if.end126
	testq	%r10, %r10
	je	.LBB41_98
.LBB41_99:                              # %lor.lhs.false.i262
	testq	$4194304, %rdx          # imm = 0x400000
	je	.LBB41_118
# BB#100:                               # %land.lhs.true.i264
	movq	8(%rbx), %rdi
	movl	16(%rdi), %ecx
	movq	%rcx, %rsi
	andq	$254, %rsi
	cmpq	$20, %rsi
	je	.LBB41_103
# BB#101:                               # %land.lhs.true.i264
	andq	$255, %rcx
	cmpq	$22, %rcx
	je	.LBB41_103
# BB#102:                               # %land.lhs.true.i264
	cmpq	$10, %rcx
	jne	.LBB41_111
.LBB41_103:                             # %land.lhs.true4.i
	movq	(%rdi), %r8
	cmpq	%rbx, %r8
	je	.LBB41_117
# BB#104:                               # %lor.lhs.false9.i
	movq	112(%rbx), %r9
	testq	%r9, %r9
	je	.LBB41_111
# BB#105:                               # %land.lhs.true12.i
	movq	$15, %rcx
	shrq	%cl, %rdx
	cmpq	%rbx, %r9
	jne	.LBB41_106
# BB#107:                               # %land.lhs.true12.i
	movabsq	$1, %rcx
	jmp	.LBB41_108
.LBB41_98:                              # %if.end126.if.then129_crit_edge
	movq	8(%rbx), %rdi
.LBB41_117:                             # %if.then129
	movq	%r14, %rsi
	callq	gen_type_die
	jmp	.LBB41_298
.LBB41_53:
	movq	%rax, %rcx
.LBB41_55:                              # %type_main_variant.exit.i
	cmpq	%rbx, %rcx
	jne	.LBB41_56
# BB#58:                                # %if.end4.i
	movzbl	16(%rbx), %eax
	cmpq	$9, %rax
	jg	.LBB41_61
# BB#59:                                # %if.end4.i
	testq	%rax, %rax
	je	.LBB41_298
	jmp	.LBB41_60
.LBB41_61:                              # %if.end4.i
	leaq	-21(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jae	.LBB41_62
# BB#89:                                # %sw.bb6.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$23, %rax
	movl	%eax, (%r15)
	testq	%r14, %r14
	je	.LBB41_93
# BB#90:                                # %if.then.i.i.i
	testq	%r15, %r15
	je	.LBB41_94
# BB#91:                                # %if.then.i.i.i.i
	cmpq	%r14, %r15
	je	.LBB41_68
# BB#92:                                # %if.end.i.i.i.i
	movq	%r14, 24(%r15)
	movq	32(%r14), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r14)
	jmp	.LBB41_94
.LBB41_106:
	movabsq	$0, %rcx
.LBB41_108:                             # %land.lhs.true12.i
	xorq	%rsi, %rsi
	testq	%rcx, %rdx
	jne	.LBB41_110
# BB#109:                               # %land.lhs.true12.i
	movq	%r9, %rsi
.LBB41_110:                             # %land.lhs.true12.i
	cmpq	%r8, %rsi
	je	.LBB41_117
.LBB41_111:                             # %land.lhs.true26.i
	movq	80(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB41_118
# BB#112:                               # %land.lhs.true29.i
	movl	16(%rcx), %esi
	movq	%rsi, %rdx
	andq	$254, %rdx
	cmpq	$20, %rdx
	je	.LBB41_115
# BB#113:                               # %land.lhs.true29.i
	andq	$255, %rsi
	cmpq	$22, %rsi
	je	.LBB41_115
# BB#114:                               # %land.lhs.true29.i
	cmpq	$10, %rsi
	jne	.LBB41_118
.LBB41_115:                             # %land.lhs.true34.i
	movq	96(%rcx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB41_118
# BB#116:                               # %land.lhs.true43.i
	movq	72(%rcx), %rcx
	cmpq	%rcx, %r10
	je	.LBB41_117
.LBB41_118:                             # %if.else132
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB41_298
# BB#119:                               # %if.end.i330
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%rbx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$22, %rax
	movl	%eax, (%r15)
	testq	%r14, %r14
	je	.LBB41_123
# BB#120:                               # %if.then.i.i
	testq	%r15, %r15
	je	.LBB41_124
# BB#121:                               # %if.then.i.i.i331
	cmpq	%r14, %r15
	je	.LBB41_68
# BB#122:                               # %if.end.i.i.i
	movq	%r14, 24(%r15)
	movq	32(%r14), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r14)
	jmp	.LBB41_124
.LBB41_123:                             # %if.else.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB41_124:                             # %new_die.exit.i
	movzbl	49(%rbx), %eax
	movq	112(%rbx), %rdx
	movq	$7, %rcx
	shrq	%cl, %rax
	cmpq	%rbx, %rdx
	jne	.LBB41_125
# BB#126:                               # %new_die.exit.i
	movabsq	$1, %rcx
	jmp	.LBB41_127
.LBB41_125:
	movabsq	$0, %rcx
.LBB41_127:                             # %new_die.exit.i
	xorq	%r12, %r12
	testq	%rcx, %rax
	jne	.LBB41_129
# BB#128:                               # %new_die.exit.i
	movq	%rdx, %r12
.LBB41_129:                             # %new_die.exit.i
	testq	%r12, %r12
	je	.LBB41_142
# BB#130:                               # %if.then6.i
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	je	.LBB41_135
# BB#131:                               # %if.then.i72.i
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%r12, %rdi
	jne	.LBB41_133
# BB#132:                               # %if.then7.i.i
	movq	(%r12), %rdi
.LBB41_133:                             # %if.end.i75.i
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB41_135
# BB#134:                               # %if.then9.i76.i
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB41_135:                             # %if.end11.i.i
	movzbl	16(%r12), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB41_136
# BB#139:                               # %if.then32.i.i
	movq	88(%r12), %r14
	jmp	.LBB41_140
.LBB41_142:                             # %if.else.i341
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	add_name_and_src_coords_attributes
	movq	96(%rbx), %rdi
	movq	8(%rbx), %rax
	testq	%rdi, %rdi
	je	.LBB41_143
# BB#144:                               # %if.then9.i
	cmpq	%rax, %rdi
	je	.LBB41_145
# BB#146:                               # %if.else16.i
	movq	%r15, 88(%rax)
	jmp	.LBB41_147
.LBB41_136:                             # %if.end11.i.i
	cmpq	$100, %rax
	jne	.LBB41_82
# BB#137:                               # %if.then21.i.i
	movl	36(%r12), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB41_82
# BB#138:                               # %cond.true.i.i.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
.LBB41_140:                             # %if.end35.i.i
	testq	%r14, %r14
	je	.LBB41_82
# BB#141:                               # %add_abstract_origin_attribute.exit.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$49, %rcx
.LBB41_159:                             # %if.then34.i.i
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB41_162
# BB#160:                               # %if.then34.i.i
	testq	%rax, %rax
	je	.LBB41_162
# BB#161:                               # %if.then.i.i.i.i349
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	jmp	.LBB41_162
.LBB41_62:                              # %if.end4.i
	cmpq	$20, %rax
	jne	.LBB41_63
# BB#87:                                # %sw.bb5.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$19, %rax
	jmp	.LBB41_65
.LBB41_143:
	movq	%rax, %rdi
.LBB41_147:                             # %if.end23.i
	movl	16(%rdi), %eax
	movl	16(%rbx), %r8d
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rax, %rdx
	andq	$254, %rdx
	cmpq	$6, %rdx
	jne	.LBB41_151
# BB#148:                               # %land.lhs.true.i.i
	movq	8(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB41_151
# BB#149:                               # %land.lhs.true6.i.i
	cmpq	$0, 96(%rdi)
	jne	.LBB41_151
# BB#150:                               # %if.then.i63.i
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rdx, %rdi
.LBB41_151:                             # %if.end.i64.i
	testq	%rcx, %rcx
	je	.LBB41_162
# BB#152:                               # %if.end.i64.i
	cmpq	$5, %rcx
	je	.LBB41_162
# BB#153:                               # %if.end19.i.i
	movq	%r8, %rcx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$2048, %r8              # imm = 0x800
	movq	$1, %rdx
	testq	%r13, %rcx
	movq	$1, %rsi
	jne	.LBB41_155
# BB#154:                               # %lor.rhs.i.i
	movq	%rax, %rsi
	andq	$4096, %rsi             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rsi
.LBB41_155:                             # %lor.end.i.i
	testq	%r13, %r8
	jne	.LBB41_157
# BB#156:                               # %lor.rhs25.i.i
	andq	$2048, %rax             # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %rax
	movq	%rax, %rdx
.LBB41_157:                             # %lor.end31.i.i
	movq	%r14, %rcx
	callq	modified_type_die
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB41_162
# BB#158:                               # %if.then34.i.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	jmp	.LBB41_159
.LBB41_162:                             # %if.end32.i
	movzbl	49(%rbx), %eax
	testq	$128, %rax
	je	.LBB41_298
.LBB41_163:                             # %if.then38.i
	movl	36(%rbx), %r14d
	movl	decl_die_table_allocated(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB41_165
# BB#164:                               # %if.then.i61.i
	leaq	256(%r14), %rax
	movabsq	$4294967040, %rsi       # imm = 0xFFFFFF00
	andq	%rax, %rsi
	movq	%rax, %rbx
	movq	decl_die_table(%rip), %rdi
	andq	$-256, %rbx
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	decl_die_table_allocated(%rip), %ecx
	movq	%rax, decl_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	andq	%r13, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, decl_die_table_allocated(%rip)
.LBB41_165:                             # %if.end.i.i
	movl	decl_die_table_in_use(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB41_167
# BB#166:                               # %if.then9.i.i
	leaq	1(%r14), %rax
	movl	%eax, decl_die_table_in_use(%rip)
.LBB41_167:                             # %equate_decl_number_to_die.exit.i
	movq	decl_die_table(%rip), %rax
	movq	%r15, (%rax,%r14,8)
.LBB41_298:                             # %sw.epilog
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB41_63:                              # %if.end4.i
	cmpq	$10, %rax
	jne	.LBB41_60
# BB#64:                                # %sw.bb.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$4, %rax
.LBB41_65:                              # %sw.bb.i
	movl	%eax, (%r15)
	testq	%r14, %r14
	je	.LBB41_69
# BB#66:                                # %if.then.i.i37.i
	testq	%r15, %r15
	je	.LBB41_70
# BB#67:                                # %if.then.i.i.i39.i
	cmpq	%r14, %r15
	je	.LBB41_68
# BB#88:                                # %if.end.i.i.i44.i
	movq	%r14, 24(%r15)
	movq	32(%r14), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r14)
	jmp	.LBB41_70
.LBB41_93:                              # %if.else.i.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB41_94:                              # %gen_inlined_union_type_die.exit.i
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	je	.LBB41_76
# BB#95:                                # %if.then.i536
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%rbx, %rdi
	jne	.LBB41_74
# BB#96:                                # %if.then7.i
	movq	(%rbx), %rdi
	jmp	.LBB41_74
.LBB41_69:                              # %if.else.i.i49.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB41_70:                              # %gen_inlined_enumeration_type_die.exit.i
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	je	.LBB41_76
# BB#71:                                # %if.then.i591
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%rbx, %rax
	jne	.LBB41_73
# BB#72:                                # %if.then7.i593
	movq	(%rbx), %rax
.LBB41_73:                              # %if.end.i597
	movq	%rax, %rdi
.LBB41_74:                              # %if.end.i597
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB41_76
# BB#75:                                # %if.then9.i598
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB41_76:                              # %if.end11.i603
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB41_77
# BB#80:                                # %if.then32.i615
	movq	88(%rbx), %rbx
	jmp	.LBB41_81
.LBB41_77:                              # %if.end11.i603
	cmpq	$100, %rax
	jne	.LBB41_82
# BB#78:                                # %if.then21.i607
	movl	36(%rbx), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB41_82
# BB#79:                                # %cond.true.i.i610
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB41_81:                              # %if.end35.i618
	testq	%rbx, %rbx
	je	.LBB41_82
# BB#83:                                # %add_abstract_origin_attribute.exit620
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$49, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	jmp	.LBB41_84
.LBB41_299:                             # %sw.default.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_byte_size_attribute, %rcx
	movq	$9320, %rsi             # imm = 0x2468
	jmp	.LBB41_57
.LBB41_202:                             # %if.then23.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_label_die, %rcx
	movq	$10568, %rsi            # imm = 0x2948
	jmp	.LBB41_57
.LBB41_266:                             # %if.then.i66.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_bit_size_attribute, %rcx
	movq	$9405, %rsi             # imm = 0x24BD
	jmp	.LBB41_57
.LBB41_273:                             # %if.then.i88.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_bit_offset_attribute, %rcx
	movq	$9361, %rsi             # imm = 0x2491
	jmp	.LBB41_57
.LBB41_56:                              # %if.then3.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_tagged_type_instantiation_die, %rcx
	movq	$11221, %rsi            # imm = 0x2BD5
	jmp	.LBB41_57
.LBB41_145:                             # %if.then15.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_typedef_die, %rcx
	movq	$11035, %rsi            # imm = 0x2B1B
	jmp	.LBB41_57
.LBB41_60:                              # %sw.default.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_tagged_type_instantiation_die, %rcx
	movq	$11245, %rsi            # imm = 0x2BED
	jmp	.LBB41_57
.LBB41_68:                              # %if.then3.i.i.i40.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB41_57
.LBB41_82:                              # %if.then38.i619
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_abstract_origin_attribute, %rcx
	movq	$9459, %rsi             # imm = 0x24F3
.LBB41_57:                              # %if.then3.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp440:
	.size	gen_decl_die, .Ltmp440-gen_decl_die
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI41_0:
	.quad	.LBB41_7
	.quad	.LBB41_168
	.quad	.LBB41_298
	.quad	.LBB41_31
	.quad	.LBB41_206
	.quad	.LBB41_296
	.quad	.LBB41_4
	.quad	.LBB41_222
	.quad	.LBB41_298

	.text
	.align	16, 0x90
	.type	reverse_all_dies,@function
reverse_all_dies:                       # @reverse_all_dies
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp443:
	.cfi_def_cfa_offset 16
.Ltmp444:
	.cfi_offset %rbx, -16
	movq	16(%rdi), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB42_3
# BB#1:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB42_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	8(%rcx), %rax
	testq	%rax, %rax
	movq	%rdx, 8(%rcx)
	movq	%rcx, %rdx
	jne	.LBB42_2
.LBB42_3:                               # %for.end.i
	movq	%rcx, 16(%rdi)
	movq	32(%rdi), %rax
	testq	%rax, %rax
	je	.LBB42_4
	.align	16, 0x90
.LBB42_5:                               # %for.body5.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rcx
	movq	%rax, %rbx
	movq	40(%rbx), %rax
	testq	%rax, %rax
	movq	%rcx, 40(%rbx)
	jne	.LBB42_5
# BB#6:                                 # %for.body.lr.ph
	movq	%rbx, 32(%rdi)
	.align	16, 0x90
.LBB42_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	reverse_all_dies
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB42_7
# BB#8:                                 # %for.end
	popq	%rbx
	retq
.LBB42_4:                               # %reverse_die_lists.exit.thread
	movq	$0, 32(%rdi)
	popq	%rbx
	retq
.Ltmp445:
	.size	reverse_all_dies, .Ltmp445-reverse_all_dies
	.cfi_endproc

	.align	16, 0x90
	.type	add_sibling_attributes,@function
add_sibling_attributes:                 # @add_sibling_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp449:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp450:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp451:
	.cfi_def_cfa_offset 32
.Ltmp452:
	.cfi_offset %rbx, -24
.Ltmp453:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	cmpq	$17, %rax
	je	.LBB43_6
# BB#1:                                 # %land.lhs.true
	movq	40(%rbx), %r14
	testq	%r14, %r14
	je	.LBB43_6
# BB#2:                                 # %land.lhs.true1
	cmpq	$0, 32(%rbx)
	je	.LBB43_6
# BB#3:                                 # %if.then
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$1, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%rbx, %rbx
	je	.LBB43_6
# BB#4:                                 # %if.then
	testq	%rax, %rax
	je	.LBB43_6
# BB#5:                                 # %if.then.i.i
	movq	16(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rbx)
.LBB43_6:                               # %if.end
	movq	32(%rbx), %rbx
	jmp	.LBB43_8
	.align	16, 0x90
.LBB43_7:                               # %for.body
                                        #   in Loop: Header=BB43_8 Depth=1
	movq	%rbx, %rdi
	callq	add_sibling_attributes
	movq	40(%rbx), %rbx
.LBB43_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB43_7
# BB#9:                                 # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp454:
	.size	add_sibling_attributes, .Ltmp454-add_sibling_attributes
	.cfi_endproc

	.align	16, 0x90
	.type	output_comp_unit,@function
output_comp_unit:                       # @output_comp_unit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp458:
	.cfi_def_cfa_offset 16
.Ltmp459:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp460:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	pushq	%rax
.Ltmp461:
	.cfi_offset %rbx, -40
.Ltmp462:
	.cfi_offset %r14, -32
.Ltmp463:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	mark_dies
	movq	%r14, %rdi
	callq	build_abbrev_table
	movq	$11, next_die_offset(%rip)
	movq	%r14, %rdi
	callq	calc_die_sizes
	movq	8(%r14), %r15
	testq	%r15, %r15
	je	.LBB44_1
# BB#2:                                 # %if.then
	movq	%r15, %rdi
	callq	strlen
	movq	%rsp, %rbx
	addq	$39, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movabsq	$.L.str210, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	sprintf
	movq	$0, 8(%r14)
	jmp	.LBB44_3
.LBB44_1:
	movabsq	$.L.str211, %rbx
.LBB44_3:                               # %if.end
	movq	$1024, %rsi             # imm = 0x400
	movq	%rbx, %rdi
	callq	named_section_flags
	movq	next_die_offset(%rip), %rsi
	addq	$-4, %rsi
	movabsq	$.L.str369, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$.L.str370, %rdx
	movq	$2, %rdi
	movq	$2, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movabsq	$abbrev_section_label, %rsi
	movabsq	$.L.str371, %rdx
	movq	$4, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_offset
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movabsq	$.L.str372, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movq	%r14, %rdi
	callq	output_die
	cmpq	$0, 8(%r14)
	je	.LBB44_5
# BB#4:                                 # %if.then7
	movq	%r14, %rdi
	callq	unmark_dies
.LBB44_5:                               # %if.end8
	leaq	-24(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp464:
	.size	output_comp_unit, .Ltmp464-output_comp_unit
	.cfi_endproc

	.align	16, 0x90
	.type	output_indirect_string,@function
output_indirect_string:                 # @output_indirect_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp467:
	.cfi_def_cfa_offset 16
.Ltmp468:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movl	20(%rbx), %eax
	cmpq	$14, %rax
	jne	.LBB45_2
# BB#1:                                 # %if.then
	movabsq	$.L.str17, %rdi
	movq	$99329, %rsi            # imm = 0x18401
	callq	named_section_flags
	movq	asm_out_file(%rip), %rdi
	movq	24(%rbx), %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	(%rbx), %esi
	movq	8(%rbx), %rdi
	incq	%rsi
	callq	assemble_string
.LBB45_2:                               # %if.end
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp469:
	.size	output_indirect_string, .Ltmp469-output_indirect_string
	.cfi_endproc

	.align	16, 0x90
	.type	value_format,@function
value_format:                           # @value_format
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp473:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp474:
	.cfi_def_cfa_offset 24
	subq	$40, %rsp
.Ltmp475:
	.cfi_def_cfa_offset 64
.Ltmp476:
	.cfi_offset %rbx, -24
.Ltmp477:
	.cfi_offset %r14, -16
	movl	16(%rdi), %ecx
	cmpq	$14, %rcx
	ja	.LBB46_25
# BB#1:                                 # %entry
	movq	$1, %rax
	jmpq	*.LJTI46_0(,%rcx,8)
.LBB46_16:                              # %sw.bb18
	movq	$10, %rax
	jmp	.LBB46_27
.LBB46_2:                               # %AT_loc.exit
	movq	24(%rdi), %rdi
	callq	size_of_locs
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB46_4
# BB#3:                                 # %if.else.i
	movq	%rax, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$60, %rcx
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
.LBB46_4:                               # %constant_size.exit
	callq	floor_log2_wide
	leaq	1(%rax), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$10, %rax
	cmpq	$1, %rcx
	je	.LBB46_27
# BB#5:                                 # %constant_size.exit
	cmpq	$2, %rcx
	jne	.LBB46_7
# BB#6:                                 # %sw.bb7
	movq	$3, %rax
	jmp	.LBB46_27
.LBB46_8:                               # %sw.bb8
	movq	$13, %rax
	jmp	.LBB46_27
.LBB46_9:                               # %AT_unsigned.exit
	movq	24(%rdi), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB46_11
# BB#10:                                # %if.else.i44
	movq	%rax, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$60, %rcx
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
.LBB46_11:                              # %constant_size.exit50
	callq	floor_log2_wide
	leaq	1(%rax), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$7, %rcx
	ja	.LBB46_15
# BB#12:                                # %constant_size.exit50
	movq	$11, %rax
	jmpq	*.LJTI46_1(,%rcx,8)
.LBB46_13:                              # %sw.bb13
	movq	$5, %rax
	jmp	.LBB46_27
.LBB46_26:                              # %sw.bb2
	movq	$6, %rax
.LBB46_27:                              # %return
	addq	$40, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB46_17:                              # %sw.bb19
	movq	$12, %rax
	jmp	.LBB46_27
.LBB46_18:                              # %if.then.i
	movl	32(%rdi), %eax
	testq	%rax, %rax
	movq	$19, %rax
	je	.LBB46_27
# BB#19:                                # %select.mid
	movq	$16, %rax
	jmp	.LBB46_27
.LBB46_20:                              # %if.then.i62
	movq	24(%rdi), %rbx
	movl	20(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB46_27
# BB#21:                                # %if.end.i64
	movl	(%rbx), %eax
	incq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	jb	.LBB46_23
# BB#22:                                # %lor.lhs.false.i
	movl	16(%rbx), %eax
	testq	%rax, %rax
	je	.LBB46_23
# BB#24:                                # %do.body.i
	movl	const_labelno(%rip), %ecx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str60, %rdx
	leaq	(%rsp), %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	movl	const_labelno(%rip), %eax
	incq	%rax
	movl	%eax, const_labelno(%rip)
	movq	%r14, %rdi
	callq	xstrdup
	movq	%rax, 24(%rbx)
	movq	$14, %rax
	movl	%eax, 20(%rbx)
	jmp	.LBB46_27
.LBB46_25:                              # %sw.default27
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.value_format, %rdx
	movq	$6078, %rsi             # imm = 0x17BE
	callq	fancy_abort
.LBB46_23:                              # %if.then7.i
	movq	$8, %rax
	movl	%eax, 20(%rbx)
	jmp	.LBB46_27
.LBB46_14:                              # %sw.bb15
	movq	$7, %rax
	jmp	.LBB46_27
.LBB46_15:                              # %sw.default16
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.value_format, %rdx
	movq	$6055, %rsi             # imm = 0x17A7
	callq	fancy_abort
.LBB46_7:                               # %sw.default
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.value_format, %rcx
	movq	$6039, %rsi             # imm = 0x1797
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp478:
	.size	value_format, .Ltmp478-value_format
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI46_0:
	.quad	.LBB46_27
	.quad	.LBB46_26
	.quad	.LBB46_2
	.quad	.LBB46_26
	.quad	.LBB46_26
	.quad	.LBB46_8
	.quad	.LBB46_9
	.quad	.LBB46_16
	.quad	.LBB46_16
	.quad	.LBB46_17
	.quad	.LBB46_18
	.quad	.LBB46_26
	.quad	.LBB46_27
	.quad	.LBB46_26
	.quad	.LBB46_20
.LJTI46_1:
	.quad	.LBB46_27
	.quad	.LBB46_13
	.quad	.LBB46_15
	.quad	.LBB46_26
	.quad	.LBB46_15
	.quad	.LBB46_15
	.quad	.LBB46_15
	.quad	.LBB46_14

	.text
	.align	16, 0x90
	.type	size_of_locs,@function
size_of_locs:                           # @size_of_locs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp484:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp485:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp486:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp487:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp488:
	.cfi_def_cfa_offset 48
.Ltmp489:
	.cfi_offset %rbx, -40
.Ltmp490:
	.cfi_offset %r12, -32
.Ltmp491:
	.cfi_offset %r14, -24
.Ltmp492:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB47_23
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB47_2
.LBB47_21:                              # %sw.bb69.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	$2, %rax
	jmp	.LBB47_22
.LBB47_13:                              # %sw.bb5.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	$5, %rax
	jmp	.LBB47_22
.LBB47_14:                              # %sw.bb7.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	$9, %rax
	jmp	.LBB47_22
.LBB47_20:                              # %sw.bb50.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	24(%rbx), %rdi
	callq	size_of_uleb128
	movq	%rax, %r15
	movq	$32, %rcx
	movq	48(%rbx), %rdi
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	callq	size_of_sleb128
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	1(%r15,%rax), %rax
	jmp	.LBB47_22
.LBB47_11:                              # %sw.bb.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	orq	$1, %rax
	jmp	.LBB47_22
	.align	16, 0x90
.LBB47_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%r14d, 64(%rbx)
	movslq	8(%rbx), %rcx
	movq	$1, %rax
	cmpq	$111, %rcx
	jg	.LBB47_9
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	cmpq	$46, %rcx
	jg	.LBB47_17
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	cmpq	$34, %rcx
	jg	.LBB47_7
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	addq	$-3, %rcx
	andq	%r12, %rcx
	cmpq	$18, %rcx
	ja	.LBB47_22
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	jmpq	*.LJTI47_1(,%rcx,8)
.LBB47_12:                              # %sw.bb3.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	$3, %rax
	jmp	.LBB47_22
	.align	16, 0x90
.LBB47_9:                               # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	addq	$-112, %rcx
	andq	%r12, %rcx
	cmpq	$37, %rcx
	ja	.LBB47_22
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	jmpq	*.LJTI47_0(,%rcx,8)
.LBB47_19:                              # %sw.bb12.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	24(%rbx), %rdi
	callq	size_of_sleb128
	jmp	.LBB47_16
	.align	16, 0x90
.LBB47_17:                              # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	andq	%r12, %rcx
	cmpq	$47, %rcx
	jne	.LBB47_22
	jmp	.LBB47_18
.LBB47_7:                               # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	andq	%r12, %rcx
	cmpq	$35, %rcx
	jne	.LBB47_8
.LBB47_15:                              # %sw.bb9.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	24(%rbx), %rdi
	callq	size_of_uleb128
.LBB47_16:                              # %size_of_loc_descr.exit
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rax
	jmp	.LBB47_22
.LBB47_8:                               # %for.body
                                        #   in Loop: Header=BB47_2 Depth=1
	cmpq	$40, %rcx
	jne	.LBB47_22
.LBB47_18:                              # %sw.bb27.i
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	$3, %rax
	.align	16, 0x90
.LBB47_22:                              # %size_of_loc_descr.exit
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	(%rbx), %rbx
	addq	%rax, %r14
	testq	%rbx, %rbx
	jne	.LBB47_2
.LBB47_23:                              # %for.end
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp493:
	.size	size_of_locs, .Ltmp493-size_of_locs
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI47_0:
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_19
	.quad	.LBB47_15
	.quad	.LBB47_19
	.quad	.LBB47_20
	.quad	.LBB47_15
	.quad	.LBB47_21
	.quad	.LBB47_21
.LJTI47_1:
	.quad	.LBB47_11
	.quad	.LBB47_22
	.quad	.LBB47_22
	.quad	.LBB47_22
	.quad	.LBB47_22
	.quad	.LBB47_21
	.quad	.LBB47_21
	.quad	.LBB47_12
	.quad	.LBB47_12
	.quad	.LBB47_13
	.quad	.LBB47_13
	.quad	.LBB47_14
	.quad	.LBB47_14
	.quad	.LBB47_15
	.quad	.LBB47_19
	.quad	.LBB47_22
	.quad	.LBB47_22
	.quad	.LBB47_22
	.quad	.LBB47_21

	.text
	.align	16, 0x90
	.type	mark_dies,@function
mark_dies:                              # @mark_dies
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp496:
	.cfi_def_cfa_offset 16
.Ltmp497:
	.cfi_offset %rbx, -16
	movq	$1, %rax
	movl	%eax, 64(%rdi)
	movq	32(%rdi), %rbx
	jmp	.LBB48_2
	.align	16, 0x90
.LBB48_1:                               # %for.body
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	%rbx, %rdi
	callq	mark_dies
	movq	40(%rbx), %rbx
.LBB48_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB48_1
# BB#3:                                 # %for.end
	popq	%rbx
	retq
.Ltmp498:
	.size	mark_dies, .Ltmp498-mark_dies
	.cfi_endproc

	.align	16, 0x90
	.type	build_abbrev_table,@function
build_abbrev_table:                     # @build_abbrev_table
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp505:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp506:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp507:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp508:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp509:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp510:
	.cfi_def_cfa_offset 80
.Ltmp511:
	.cfi_offset %rbx, -48
.Ltmp512:
	.cfi_offset %r12, -40
.Ltmp513:
	.cfi_offset %r13, -32
.Ltmp514:
	.cfi_offset %r14, -24
.Ltmp515:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rax
	testq	%rax, %rax
	je	.LBB49_6
# BB#1:
	movq	$1, %rcx
	.align	16, 0x90
.LBB49_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rax), %edx
	cmpq	$10, %rdx
	jne	.LBB49_31
# BB#3:                                 # %AT_ref.exit
                                        #   in Loop: Header=BB49_2 Depth=1
	movq	24(%rax), %rdx
	movl	64(%rdx), %esi
	testq	%rsi, %rsi
	jne	.LBB49_31
# BB#4:                                 # %AT_ref.exit114
                                        #   in Loop: Header=BB49_2 Depth=1
	cmpq	$0, 8(%rdx)
	je	.LBB49_5
# BB#30:                                # %set_AT_ref_external.exit
                                        #   in Loop: Header=BB49_2 Depth=1
	movl	%ecx, 32(%rax)
.LBB49_31:                              # %for.inc
                                        #   in Loop: Header=BB49_2 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB49_2
.LBB49_6:                               # %for.cond7.preheader
	movl	abbrev_die_table_in_use(%rip), %r8d
	cmpq	$2, %r8
	movq	$1, %r13
	jb	.LBB49_22
# BB#7:                                 # %for.body10.lr.ph
	movq	$1, %r13
	movabsq	$0, %rdi
	movabsq	$1, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB49_8:                               # %for.body10
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB49_15 Depth 2
	movq	abbrev_die_table(%rip), %rcx
	movq	(%rcx,%r13,8), %rcx
	movl	(%rcx), %edx
	movl	(%rbx), %esi
	cmpq	%rsi, %rdx
	jne	.LBB49_21
# BB#9:                                 # %if.then14
                                        #   in Loop: Header=BB49_8 Depth=1
	cmpq	$0, 32(%rcx)
	movq	%rdi, %rdx
	je	.LBB49_11
# BB#10:                                # %if.then14
                                        #   in Loop: Header=BB49_8 Depth=1
	movq	%rax, %rdx
.LBB49_11:                              # %if.then14
                                        #   in Loop: Header=BB49_8 Depth=1
	cmpq	$0, 32(%rbx)
	movq	%rdi, %rsi
	je	.LBB49_13
# BB#12:                                # %if.then14
                                        #   in Loop: Header=BB49_8 Depth=1
	movq	%rax, %rsi
.LBB49_13:                              # %if.then14
                                        #   in Loop: Header=BB49_8 Depth=1
	xorq	%rsi, %rdx
	jne	.LBB49_21
# BB#14:                                # %if.then22
                                        #   in Loop: Header=BB49_8 Depth=1
	movq	16(%rcx), %r15
	movq	16(%rbx), %r12
	jmp	.LBB49_15
	.align	16, 0x90
.LBB49_19:                              # %if.end37
                                        #   in Loop: Header=BB49_15 Depth=2
	movq	8(%r15), %r15
	movq	8(%r12), %r12
.LBB49_15:                              # %if.then22
                                        #   Parent Loop BB49_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %r15
	je	.LBB49_20
# BB#16:                                # %if.then22
                                        #   in Loop: Header=BB49_15 Depth=2
	testq	%r12, %r12
	je	.LBB49_20
# BB#17:                                # %while.body
                                        #   in Loop: Header=BB49_15 Depth=2
	movl	(%r15), %eax
	movl	(%r12), %ecx
	cmpq	%rcx, %rax
	jne	.LBB49_20
# BB#18:                                # %lor.lhs.false
                                        #   in Loop: Header=BB49_15 Depth=2
	movq	%r15, %rdi
	callq	value_format
	movq	%rax, %rbx
	andq	%r14, %rbx
	movq	%r12, %rdi
	callq	value_format
	andq	%r14, %rax
	cmpq	%rax, %rbx
	je	.LBB49_19
	.align	16, 0x90
.LBB49_20:                              # %while.end
                                        #   in Loop: Header=BB49_8 Depth=1
	movl	abbrev_die_table_in_use(%rip), %r8d
	orq	%r15, %r12
	movq	24(%rsp), %rbx          # 8-byte Reload
	movabsq	$0, %rdi
	movabsq	$1, %rax
	je	.LBB49_22
.LBB49_21:                              # %for.inc49
                                        #   in Loop: Header=BB49_8 Depth=1
	incq	%r13
	movq	%r8, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %r13
	jb	.LBB49_8
.LBB49_22:                              # %for.end50
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r8, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %r13
	jb	.LBB49_26
# BB#23:                                # %if.then54
	movl	abbrev_die_table_allocated(%rip), %r15d
	cmpq	%r15, %rcx
	jb	.LBB49_25
# BB#24:                                # %if.then57
	addq	$256, %r15              # imm = 0x100
	movq	abbrev_die_table(%rip), %rdi
	movq	%r15, %rsi
	andq	%r14, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	abbrev_die_table_allocated(%rip), %ecx
	movq	%rax, abbrev_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%r15, %rdx
	subq	%rcx, %rdx
	andq	%r14, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	abbrev_die_table_in_use(%rip), %r8d
	movl	%r15d, abbrev_die_table_allocated(%rip)
.LBB49_25:                              # %if.end63
	movq	abbrev_die_table(%rip), %rcx
	incq	%r8
	movl	%r8d, abbrev_die_table_in_use(%rip)
	movq	%rbx, (%rcx,%r13,8)
.LBB49_26:                              # %if.end66
	movq	%r13, 56(%rbx)
	movq	32(%rbx), %rbx
	jmp	.LBB49_28
	.align	16, 0x90
.LBB49_27:                              # %for.body71
                                        #   in Loop: Header=BB49_28 Depth=1
	movq	%rbx, %rdi
	callq	build_abbrev_table
	movq	40(%rbx), %rbx
.LBB49_28:                              # %for.body71
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB49_27
# BB#29:                                # %for.end73
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB49_5:                               # %if.then5
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.build_abbrev_table, %rcx
	movq	$5770, %rsi             # imm = 0x168A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp516:
	.size	build_abbrev_table, .Ltmp516-build_abbrev_table
	.cfi_endproc

	.align	16, 0x90
	.type	calc_die_sizes,@function
calc_die_sizes:                         # @calc_die_sizes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp523:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp524:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp525:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp526:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp527:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp528:
	.cfi_def_cfa_offset 80
.Ltmp529:
	.cfi_offset %rbx, -48
.Ltmp530:
	.cfi_offset %r12, -40
.Ltmp531:
	.cfi_offset %r13, -32
.Ltmp532:
	.cfi_offset %r14, -24
.Ltmp533:
	.cfi_offset %r15, -16
	movq	next_die_offset(%rip), %rax
	movq	%rdi, %r14
	movq	%rax, 48(%r14)
	movq	56(%r14), %rdi
	callq	size_of_uleb128
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	16(%r14), %r12
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%r12, %r12
	je	.LBB50_27
# BB#1:                                 # %for.body.lr.ph.i
	leaq	(%rsp), %r13
	.align	16, 0x90
.LBB50_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r12), %eax
	cmpq	$14, %rax
	ja	.LBB50_32
# BB#3:                                 # %for.body.i
                                        #   in Loop: Header=BB50_2 Depth=1
	jmpq	*.LJTI50_0(,%rax,8)
.LBB50_17:                              # %sw.bb.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$3, %rcx
	jmp	.LBB50_9
.LBB50_4:                               # %AT_loc.exit.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	24(%r12), %rdi
	callq	size_of_locs
	movq	%rax, %r15
	xorq	%rdi, %rdi
	testq	%r15, %r15
	je	.LBB50_6
# BB#5:                                 # %if.else.i.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	%r15, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$60, %rcx
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
.LBB50_6:                               # %constant_size.exit.i
                                        #   in Loop: Header=BB50_2 Depth=1
	callq	floor_log2_wide
	leaq	1(%rax), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%r15, %rbx
	addq	%rax, %rbx
	jmp	.LBB50_26
.LBB50_7:                               # %AT_int.exit.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	24(%r12), %rdi
	callq	size_of_sleb128
	jmp	.LBB50_8
.LBB50_10:                              # %AT_unsigned.exit.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	24(%r12), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB50_12
# BB#11:                                # %if.else.i117.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	%rax, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$60, %rcx
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
.LBB50_12:                              # %constant_size.exit123.i
                                        #   in Loop: Header=BB50_2 Depth=1
	callq	floor_log2_wide
	leaq	1(%rax), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
.LBB50_8:                               # %for.inc.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
.LBB50_9:                               # %for.inc.i
                                        #   in Loop: Header=BB50_2 Depth=1
	sarq	%cl, %rax
	addq	%rax, %rbx
	jmp	.LBB50_26
.LBB50_13:                              # %sw.bb28.i
                                        #   in Loop: Header=BB50_2 Depth=1
	addq	$9, %rbx
	jmp	.LBB50_26
.LBB50_14:                              # %sw.bb30.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movl	32(%r12), %eax
	leaq	1(,%rax,4), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	jmp	.LBB50_15
.LBB50_16:                              # %sw.bb34.i
                                        #   in Loop: Header=BB50_2 Depth=1
	incq	%rbx
	jmp	.LBB50_26
.LBB50_18:                              # %if.then.i.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	24(%r12), %r15
	movl	20(%r15), %eax
	testq	%rax, %rax
	jne	.LBB50_24
# BB#19:                                # %if.end.i.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movl	(%r15), %eax
	incq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	jb	.LBB50_21
# BB#20:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movl	16(%r15), %eax
	testq	%rax, %rax
	jne	.LBB50_23
.LBB50_21:                              # %AT_string_form.exit.thread125.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	$8, %rax
	movl	%eax, 20(%r15)
	jmp	.LBB50_22
.LBB50_24:                              # %AT_string_form.exit.i
                                        #   in Loop: Header=BB50_2 Depth=1
	cmpq	$14, %rax
	je	.LBB50_25
.LBB50_22:                              # %if.else.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	24(%r12), %rax
	movl	(%rax), %eax
	incq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
.LBB50_15:                              # %for.inc.i
                                        #   in Loop: Header=BB50_2 Depth=1
	andq	%rcx, %rax
	addq	%rax, %rbx
	jmp	.LBB50_26
.LBB50_23:                              # %AT_string_form.exit.thread.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movl	const_labelno(%rip), %ecx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str60, %rdx
	callq	sprintf
	movl	const_labelno(%rip), %eax
	incq	%rax
	movl	%eax, const_labelno(%rip)
	movq	%r13, %rdi
	callq	xstrdup
	movq	%rax, 24(%r15)
	movq	$14, %rax
	movl	%eax, 20(%r15)
	.align	16, 0x90
.LBB50_25:                              # %sw.bb5.i
                                        #   in Loop: Header=BB50_2 Depth=1
	addq	$4, %rbx
.LBB50_26:                              # %for.inc.i
                                        #   in Loop: Header=BB50_2 Depth=1
	movq	8(%r12), %r12
	testq	%r12, %r12
	jne	.LBB50_2
.LBB50_27:                              # %size_of_die.exit
	addq	%rbx, next_die_offset(%rip)
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB50_31
	.align	16, 0x90
.LBB50_28:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	calc_die_sizes
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB50_28
# BB#29:                                # %for.end
	cmpq	$0, 32(%r14)
	je	.LBB50_31
# BB#30:                                # %if.then
	incq	next_die_offset(%rip)
.LBB50_31:                              # %if.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB50_32:                              # %sw.default.i
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.size_of_die, %rdx
	movq	$5914, %rsi             # imm = 0x171A
	callq	fancy_abort
.Ltmp534:
	.size	calc_die_sizes, .Ltmp534-calc_die_sizes
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI50_0:
	.quad	.LBB50_17
	.quad	.LBB50_25
	.quad	.LBB50_4
	.quad	.LBB50_25
	.quad	.LBB50_25
	.quad	.LBB50_7
	.quad	.LBB50_10
	.quad	.LBB50_13
	.quad	.LBB50_14
	.quad	.LBB50_16
	.quad	.LBB50_25
	.quad	.LBB50_25
	.quad	.LBB50_17
	.quad	.LBB50_25
	.quad	.LBB50_18

	.text
	.align	16, 0x90
	.type	output_die,@function
output_die:                             # @output_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp541:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp542:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp543:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp544:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp545:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp546:
	.cfi_def_cfa_offset 144
.Ltmp547:
	.cfi_offset %rbx, -48
.Ltmp548:
	.cfi_offset %r12, -40
.Ltmp549:
	.cfi_offset %r13, -32
.Ltmp550:
	.cfi_offset %r14, -24
.Ltmp551:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB51_4
# BB#1:                                 # %if.end.i
	movabsq	$.L.str366, %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB51_3
# BB#2:                                 # %if.then2.i
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str367, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB51_3:                               # %if.end5.i
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str3, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB51_4:                               # %if.end
	movq	48(%r14), %rdx
	movslq	(%r14), %rax
	movq	56(%r14), %rdi
	cmpq	$16512, %rax            # imm = 0x4080
	jg	.LBB51_7
# BB#5:                                 # %if.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$53, %rax
	ja	.LBB51_64
# BB#6:                                 # %if.end
	movabsq	$.L.str155, %rcx
	jmpq	*.LJTI51_1(,%rax,8)
.LBB51_12:                              # %sw.bb1.i
	movabsq	$.L.str156, %rcx
	jmp	.LBB51_65
.LBB51_7:                               # %if.end
	cmpq	$16640, %rax            # imm = 0x4100
	jg	.LBB51_10
# BB#8:                                 # %if.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$16513, %rax            # imm = 0x4081
	jne	.LBB51_64
# BB#9:                                 # %sw.bb48.i
	movabsq	$.L.str203, %rcx
	jmp	.LBB51_65
.LBB51_10:                              # %if.end
	addq	$-16641, %rax           # imm = 0xFFFFFFFFFFFFBEFF
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$4, %rcx
	jbe	.LBB51_11
.LBB51_64:                              # %sw.default.i
	movabsq	$.L.str209, %rcx
.LBB51_65:                              # %dwarf_tag_name.exit
	movabsq	$.L.str212, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data_uleb128
	movq	16(%r14), %r13
	movq	%r14, 24(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	je	.LBB51_221
# BB#66:                                # %for.body.lr.ph
	movabsq	$.L.str61, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB51_67
	.align	16, 0x90
.LBB51_171:                             # %dwarf_attr_name.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	jmpq	*.LJTI51_5(,%rax,8)
.LBB51_219:                             # %AT_addr.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	movq	24(%r13), %rsi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_addr_rtx
	jmp	.LBB51_220
.LBB51_199:                             # %sw.bb5
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rsi
	jmp	.LBB51_200
.LBB51_173:                             # %AT_loc.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	%r15, %rbx
	movq	24(%r13), %rdi
	callq	size_of_locs
	movq	%rax, %r15
	xorq	%rdi, %rdi
	testq	%r15, %r15
	je	.LBB51_175
# BB#174:                               # %if.else.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	%r15, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$60, %rcx
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
.LBB51_175:                             # %land.lhs.true.i247
                                        #   in Loop: Header=BB51_67 Depth=1
	callq	floor_log2_wide
	leaq	1(%rax), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_data
	movl	16(%r13), %eax
	cmpq	$2, %rax
	jne	.LBB51_176
# BB#178:                               # %AT_loc.exit251
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rdi
	callq	output_loc_sequence
	movq	%rbx, %r15
	jmp	.LBB51_220
.LBB51_190:                             # %AT_loc_list.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rax
	movq	24(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB51_191
# BB#192:                               # %if.end51
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	$4, %rdi
	xorq	%rax, %rax
	movabsq	$loc_section_label, %rdx
	movabsq	$.L.str213, %rcx
	movq	%r12, %r8
	callq	dw2_asm_output_delta
	jmp	.LBB51_220
.LBB51_172:                             # %sw.bb6
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$ranges_section_label, %rbx
	movq	%rbx, %rdi
	callq	strlen
	movq	24(%r13), %rdx
	movq	%r15, %r14
	movq	%rax, %r15
	leaq	ranges_section_label(%r15), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str214, %rsi
	callq	sprintf
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_offset
	xorq	%rax, %rax
	movb	%al, ranges_section_label(%r15)
	movq	%r14, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB51_220
.LBB51_179:                             # %AT_int.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str213, %rsi
	movq	%r12, %rdx
	callq	dw2_asm_output_data_sleb128
	jmp	.LBB51_220
.LBB51_180:                             # %AT_unsigned.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB51_182
# BB#181:                               # %if.else.i268
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	%rax, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$60, %rcx
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
.LBB51_182:                             # %land.lhs.true.i279
                                        #   in Loop: Header=BB51_67 Depth=1
	callq	floor_log2_wide
	movl	16(%r13), %ecx
	cmpq	$6, %rcx
	jne	.LBB51_183
# BB#184:                               # %AT_unsigned.exit282
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rsi
	incq	%rax
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	jmp	.LBB51_201
.LBB51_185:                             # %sw.bb23
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	$1, %rdi
	movq	$8, %rsi
	xorq	%rax, %rax
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_data
	movq	24(%r13), %rbx
	movq	32(%r13), %rsi
	movq	$4, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str215, %rdx
	callq	dw2_asm_output_data
	movq	$4, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	dw2_asm_output_data
	jmp	.LBB51_220
.LBB51_186:                             # %sw.bb29
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	32(%r13), %esi
	andq	$1073741823, %rsi       # imm = 0x3FFFFFFF
	shlq	$2, %rsi
	movq	$1, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_data
	movl	32(%r13), %eax
	testq	%rax, %rax
	je	.LBB51_220
# BB#187:                               # %for.body39.lr.ph
                                        #   in Loop: Header=BB51_67 Depth=1
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB51_188:                             # %for.body39
                                        #   Parent Loop BB51_67 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r13), %rax
	movq	%rbx, %rcx
	andq	%r14, %rcx
	movq	(%rax,%rcx,8), %rsi
	movq	$4, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str216, %rdx
	movq	%rbx, %rcx
	callq	dw2_asm_output_data
	movl	32(%r13), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB51_188
	jmp	.LBB51_220
.LBB51_189:                             # %AT_flag.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movzbl	24(%r13), %esi
	movq	$1, %rdi
	jmp	.LBB51_201
.LBB51_193:                             # %AT_ref.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	32(%r13), %ecx
	movq	24(%r13), %rax
	testq	%rcx, %rcx
	jne	.LBB51_194
# BB#197:                               # %if.else
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	48(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB51_198
.LBB51_200:                             # %AT_ref.exit320
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	$4, %rdi
.LBB51_201:                             # %for.inc102
                                        #   in Loop: Header=BB51_67 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_data
	jmp	.LBB51_220
.LBB51_202:                             # %do.body
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	24(%r13), %ecx
	addq	%rcx, %rcx
	xorq	%rax, %rax
	leaq	32(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str217, %rdx
	callq	sprintf
	movq	$4, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	jmp	.LBB51_206
.LBB51_203:                             # %AT_lbl.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	movq	24(%r13), %rsi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_addr
	jmp	.LBB51_220
.LBB51_204:                             # %AT_lbl.exit335
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rsi
	movq	$4, %rdi
	jmp	.LBB51_205
.LBB51_207:                             # %if.then.i342
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rbx
	movl	20(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB51_214
# BB#208:                               # %if.end.i344
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	(%rbx), %eax
	incq	%rax
	andq	%r14, %rax
	cmpq	$5, %rax
	jb	.LBB51_210
# BB#209:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	16(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB51_213
.LBB51_210:                             # %AT_string_form.exit.thread370
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	$8, %rax
	movl	%eax, 20(%rbx)
	jmp	.LBB51_211
.LBB51_75:                              # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	jmpq	*.LJTI51_2(,%rax,8)
.LBB51_160:                             # %sw.bb85.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str146, %r12
	jmp	.LBB51_170
.LBB51_194:                             # %if.then55
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	8(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB51_195
# BB#196:                               # %if.end62
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
.LBB51_205:                             # %for.inc102
                                        #   in Loop: Header=BB51_67 Depth=1
	xorq	%rax, %rax
.LBB51_206:                             # %for.inc102
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_offset
	jmp	.LBB51_220
.LBB51_214:                             # %AT_string_form.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	cmpq	$14, %rax
	je	.LBB51_215
.LBB51_211:                             # %land.lhs.true.i359
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	16(%r13), %eax
	cmpq	$14, %rax
	jne	.LBB51_216
# BB#212:                               # %AT_string.exit365
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rax
	movq	8(%rax), %rdi
	movq	$-1, %rsi
	xorq	%rax, %rax
	movabsq	$.L.str213, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_nstring
	jmp	.LBB51_220
.LBB51_161:                             # %sw.bb86.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str147, %r12
	jmp	.LBB51_170
.LBB51_162:                             # %sw.bb87.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str148, %r12
	jmp	.LBB51_170
.LBB51_163:                             # %sw.bb88.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str149, %r12
	jmp	.LBB51_170
.LBB51_164:                             # %sw.bb89.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str150, %r12
	jmp	.LBB51_170
.LBB51_165:                             # %sw.bb90.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str151, %r12
	jmp	.LBB51_170
.LBB51_166:                             # %sw.bb91.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str152, %r12
	jmp	.LBB51_170
.LBB51_213:                             # %AT_string_form.exit.thread
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	const_labelno(%rip), %ecx
	xorq	%rax, %rax
	leaq	64(%rsp), %rdx
	movq	%rdx, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str60, %rdx
	callq	sprintf
	movl	const_labelno(%rip), %eax
	incq	%rax
	movl	%eax, const_labelno(%rip)
	leaq	64(%rsp), %rdi
	callq	xstrdup
	movq	%rax, 24(%rbx)
	movq	$14, %rax
	movl	%eax, 20(%rbx)
.LBB51_215:                             # %land.lhs.true.i350
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	16(%r13), %eax
	cmpq	$14, %rax
	jne	.LBB51_216
# BB#217:                               # %AT_string.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	24(%r13), %rax
	movq	8(%rax), %r8
	movq	24(%rax), %rsi
	movq	$4, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str218, %rdx
	movq	%r12, %rcx
	callq	dw2_asm_output_offset
	.align	16, 0x90
.LBB51_220:                             # %for.inc102
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB51_67
	jmp	.LBB51_221
.LBB51_150:                             # %sw.bb75.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str136, %r12
	jmp	.LBB51_170
.LBB51_151:                             # %sw.bb76.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str137, %r12
	jmp	.LBB51_170
.LBB51_152:                             # %sw.bb77.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str138, %r12
	jmp	.LBB51_170
.LBB51_153:                             # %sw.bb78.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str139, %r12
	jmp	.LBB51_170
.LBB51_154:                             # %sw.bb79.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str140, %r12
	jmp	.LBB51_170
.LBB51_155:                             # %sw.bb80.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str141, %r12
	jmp	.LBB51_170
.LBB51_156:                             # %sw.bb81.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str142, %r12
	jmp	.LBB51_170
.LBB51_157:                             # %sw.bb82.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str143, %r12
	jmp	.LBB51_170
.LBB51_158:                             # %sw.bb83.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str144, %r12
	jmp	.LBB51_170
.LBB51_159:                             # %sw.bb84.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str145, %r12
	jmp	.LBB51_170
.LBB51_77:                              # %sw.bb2.i177
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str63, %r12
	jmp	.LBB51_170
.LBB51_78:                              # %sw.bb3.i178
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str64, %r12
	jmp	.LBB51_170
.LBB51_79:                              # %sw.bb4.i179
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str65, %r12
	jmp	.LBB51_170
.LBB51_80:                              # %sw.bb5.i180
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str66, %r12
	jmp	.LBB51_170
.LBB51_81:                              # %sw.bb6.i181
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str67, %r12
	jmp	.LBB51_170
.LBB51_82:                              # %sw.bb7.i182
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str68, %r12
	jmp	.LBB51_170
.LBB51_83:                              # %sw.bb8.i183
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str69, %r12
	jmp	.LBB51_170
.LBB51_84:                              # %sw.bb9.i184
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str70, %r12
	jmp	.LBB51_170
.LBB51_85:                              # %sw.bb10.i185
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str71, %r12
	jmp	.LBB51_170
.LBB51_86:                              # %sw.bb11.i186
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str72, %r12
	jmp	.LBB51_170
.LBB51_87:                              # %sw.bb12.i187
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str73, %r12
	jmp	.LBB51_170
.LBB51_88:                              # %sw.bb13.i188
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str74, %r12
	jmp	.LBB51_170
.LBB51_89:                              # %sw.bb14.i189
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str75, %r12
	jmp	.LBB51_170
.LBB51_90:                              # %sw.bb15.i190
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str76, %r12
	jmp	.LBB51_170
.LBB51_91:                              # %sw.bb16.i191
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str77, %r12
	jmp	.LBB51_170
.LBB51_92:                              # %sw.bb17.i192
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str78, %r12
	jmp	.LBB51_170
.LBB51_93:                              # %sw.bb18.i193
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str79, %r12
	jmp	.LBB51_170
.LBB51_94:                              # %sw.bb19.i194
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str80, %r12
	jmp	.LBB51_170
.LBB51_95:                              # %sw.bb20.i195
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str81, %r12
	jmp	.LBB51_170
.LBB51_96:                              # %sw.bb21.i196
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str82, %r12
	jmp	.LBB51_170
.LBB51_97:                              # %sw.bb22.i197
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str83, %r12
	jmp	.LBB51_170
.LBB51_98:                              # %sw.bb23.i198
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str84, %r12
	jmp	.LBB51_170
.LBB51_99:                              # %sw.bb24.i199
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str85, %r12
	jmp	.LBB51_170
.LBB51_100:                             # %sw.bb25.i200
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str86, %r12
	jmp	.LBB51_170
.LBB51_101:                             # %sw.bb26.i201
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str87, %r12
	jmp	.LBB51_170
.LBB51_102:                             # %sw.bb27.i202
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str88, %r12
	jmp	.LBB51_170
.LBB51_103:                             # %sw.bb28.i203
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str89, %r12
	jmp	.LBB51_170
.LBB51_104:                             # %sw.bb29.i204
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str90, %r12
	jmp	.LBB51_170
.LBB51_105:                             # %sw.bb30.i205
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str91, %r12
	jmp	.LBB51_170
.LBB51_106:                             # %sw.bb31.i206
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str92, %r12
	jmp	.LBB51_170
.LBB51_107:                             # %sw.bb32.i207
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str93, %r12
	jmp	.LBB51_170
.LBB51_108:                             # %sw.bb33.i208
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str94, %r12
	jmp	.LBB51_170
.LBB51_109:                             # %sw.bb34.i209
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str95, %r12
	jmp	.LBB51_170
.LBB51_110:                             # %sw.bb35.i210
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str96, %r12
	jmp	.LBB51_170
.LBB51_111:                             # %sw.bb36.i211
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str97, %r12
	jmp	.LBB51_170
.LBB51_112:                             # %sw.bb37.i212
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str98, %r12
	jmp	.LBB51_170
.LBB51_113:                             # %sw.bb38.i213
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str99, %r12
	jmp	.LBB51_170
.LBB51_114:                             # %sw.bb39.i214
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str100, %r12
	jmp	.LBB51_170
.LBB51_115:                             # %sw.bb40.i215
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str101, %r12
	jmp	.LBB51_170
.LBB51_116:                             # %sw.bb41.i216
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str102, %r12
	jmp	.LBB51_170
.LBB51_117:                             # %sw.bb42.i217
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str103, %r12
	jmp	.LBB51_170
.LBB51_118:                             # %sw.bb43.i218
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str104, %r12
	jmp	.LBB51_170
.LBB51_119:                             # %sw.bb44.i219
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str105, %r12
	jmp	.LBB51_170
.LBB51_120:                             # %sw.bb45.i220
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str106, %r12
	jmp	.LBB51_170
.LBB51_121:                             # %sw.bb46.i221
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str107, %r12
	jmp	.LBB51_170
.LBB51_122:                             # %sw.bb47.i222
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str108, %r12
	jmp	.LBB51_170
.LBB51_123:                             # %sw.bb48.i223
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str109, %r12
	jmp	.LBB51_170
.LBB51_124:                             # %sw.bb49.i224
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str110, %r12
	jmp	.LBB51_170
.LBB51_125:                             # %sw.bb50.i225
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str111, %r12
	jmp	.LBB51_170
.LBB51_126:                             # %sw.bb51.i226
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str112, %r12
	jmp	.LBB51_170
.LBB51_127:                             # %sw.bb52.i227
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str113, %r12
	jmp	.LBB51_170
.LBB51_128:                             # %sw.bb53.i228
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str114, %r12
	jmp	.LBB51_170
.LBB51_129:                             # %sw.bb54.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str115, %r12
	jmp	.LBB51_170
.LBB51_130:                             # %sw.bb55.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str116, %r12
	jmp	.LBB51_170
.LBB51_131:                             # %sw.bb56.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str117, %r12
	jmp	.LBB51_170
.LBB51_132:                             # %sw.bb57.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str118, %r12
	jmp	.LBB51_170
.LBB51_133:                             # %sw.bb58.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str119, %r12
	jmp	.LBB51_170
.LBB51_134:                             # %sw.bb59.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str120, %r12
	jmp	.LBB51_170
.LBB51_135:                             # %sw.bb60.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str121, %r12
	jmp	.LBB51_170
.LBB51_136:                             # %sw.bb61.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str122, %r12
	jmp	.LBB51_170
.LBB51_137:                             # %sw.bb62.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str123, %r12
	jmp	.LBB51_170
.LBB51_138:                             # %sw.bb63.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str124, %r12
	jmp	.LBB51_170
.LBB51_139:                             # %sw.bb64.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str125, %r12
	jmp	.LBB51_170
.LBB51_140:                             # %sw.bb65.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str126, %r12
	jmp	.LBB51_170
.LBB51_141:                             # %sw.bb66.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str127, %r12
	jmp	.LBB51_170
.LBB51_142:                             # %sw.bb67.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str128, %r12
	jmp	.LBB51_170
.LBB51_143:                             # %sw.bb68.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str129, %r12
	jmp	.LBB51_170
.LBB51_144:                             # %sw.bb69.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str130, %r12
	jmp	.LBB51_170
.LBB51_145:                             # %sw.bb70.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str131, %r12
	jmp	.LBB51_170
.LBB51_146:                             # %sw.bb71.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str132, %r12
	jmp	.LBB51_170
.LBB51_147:                             # %sw.bb72.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str133, %r12
	jmp	.LBB51_170
.LBB51_148:                             # %sw.bb73.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str134, %r12
	jmp	.LBB51_170
	.align	16, 0x90
.LBB51_67:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB51_188 Depth 2
	movslq	(%r13), %rax
	cmpq	$8192, %rax             # imm = 0x2000
	jg	.LBB51_70
# BB#68:                                # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	decq	%rax
	andq	%r14, %rax
	cmpq	$88, %rax
	ja	.LBB51_169
# BB#69:                                # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	movq	%r15, %r12
	jmpq	*.LJTI51_4(,%rax,8)
.LBB51_76:                              # %sw.bb1.i176
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str62, %r12
	jmp	.LBB51_170
	.align	16, 0x90
.LBB51_70:                              # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	cmpq	$8704, %rax             # imm = 0x2200
	jg	.LBB51_167
# BB#71:                                # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	cmpq	$8448, %rax             # imm = 0x2100
	jg	.LBB51_74
# BB#72:                                # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	addq	$-8193, %rax            # imm = 0xFFFFFFFFFFFFDFFF
	andq	%r14, %rax
	cmpq	$10, %rax
	ja	.LBB51_169
# BB#73:                                # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	jmpq	*.LJTI51_3(,%rax,8)
.LBB51_149:                             # %sw.bb74.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str135, %r12
	jmp	.LBB51_170
	.align	16, 0x90
.LBB51_167:                             # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	andq	%r14, %rax
	cmpq	$8705, %rax             # imm = 0x2201
	jne	.LBB51_169
# BB#168:                               # %sw.bb92.i
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str153, %r12
	jmp	.LBB51_170
.LBB51_74:                              # %for.body
                                        #   in Loop: Header=BB51_67 Depth=1
	addq	$-8449, %rax            # imm = 0xFFFFFFFFFFFFDEFF
	andq	%r14, %rax
	cmpq	$6, %rax
	jbe	.LBB51_75
	.align	16, 0x90
.LBB51_169:                             # %sw.default.i229
                                        #   in Loop: Header=BB51_67 Depth=1
	movabsq	$.L.str154, %r12
.LBB51_170:                             # %dwarf_attr_name.exit
                                        #   in Loop: Header=BB51_67 Depth=1
	movl	16(%r13), %eax
	cmpq	$14, %rax
	jbe	.LBB51_171
# BB#218:                               # %sw.default
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.output_die, %rdx
	movq	$6411, %rsi             # imm = 0x190B
	callq	fancy_abort
.LBB51_221:                             # %for.end103
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB51_225
	.align	16, 0x90
.LBB51_222:                             # %for.body107
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	output_die
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB51_222
# BB#223:                               # %for.end109
	cmpq	$0, 32(%r14)
	je	.LBB51_225
# BB#224:                               # %if.then113
	movq	48(%r14), %rcx
	movabsq	$.L.str219, %rdx
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
.LBB51_225:                             # %if.end115
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB51_11:                              # %if.end
	jmpq	*.LJTI51_0(,%rcx,8)
.LBB51_59:                              # %sw.bb49.i
	movabsq	$.L.str204, %rcx
	jmp	.LBB51_65
.LBB51_60:                              # %sw.bb50.i
	movabsq	$.L.str205, %rcx
	jmp	.LBB51_65
.LBB51_61:                              # %sw.bb51.i
	movabsq	$.L.str206, %rcx
	jmp	.LBB51_65
.LBB51_62:                              # %sw.bb52.i
	movabsq	$.L.str207, %rcx
	jmp	.LBB51_65
.LBB51_63:                              # %sw.bb53.i
	movabsq	$.L.str208, %rcx
	jmp	.LBB51_65
.LBB51_13:                              # %sw.bb2.i
	movabsq	$.L.str157, %rcx
	jmp	.LBB51_65
.LBB51_14:                              # %sw.bb3.i
	movabsq	$.L.str158, %rcx
	jmp	.LBB51_65
.LBB51_15:                              # %sw.bb4.i
	movabsq	$.L.str159, %rcx
	jmp	.LBB51_65
.LBB51_16:                              # %sw.bb5.i
	movabsq	$.L.str160, %rcx
	jmp	.LBB51_65
.LBB51_17:                              # %sw.bb6.i
	movabsq	$.L.str161, %rcx
	jmp	.LBB51_65
.LBB51_18:                              # %sw.bb7.i
	movabsq	$.L.str162, %rcx
	jmp	.LBB51_65
.LBB51_19:                              # %sw.bb8.i
	movabsq	$.L.str163, %rcx
	jmp	.LBB51_65
.LBB51_20:                              # %sw.bb9.i
	movabsq	$.L.str164, %rcx
	jmp	.LBB51_65
.LBB51_21:                              # %sw.bb10.i
	movabsq	$.L.str165, %rcx
	jmp	.LBB51_65
.LBB51_22:                              # %sw.bb11.i
	movabsq	$.L.str166, %rcx
	jmp	.LBB51_65
.LBB51_23:                              # %sw.bb12.i
	movabsq	$.L.str167, %rcx
	jmp	.LBB51_65
.LBB51_24:                              # %sw.bb13.i
	movabsq	$.L.str168, %rcx
	jmp	.LBB51_65
.LBB51_25:                              # %sw.bb14.i
	movabsq	$.L.str169, %rcx
	jmp	.LBB51_65
.LBB51_26:                              # %sw.bb15.i
	movabsq	$.L.str170, %rcx
	jmp	.LBB51_65
.LBB51_27:                              # %sw.bb16.i
	movabsq	$.L.str171, %rcx
	jmp	.LBB51_65
.LBB51_28:                              # %sw.bb17.i
	movabsq	$.L.str172, %rcx
	jmp	.LBB51_65
.LBB51_29:                              # %sw.bb18.i
	movabsq	$.L.str173, %rcx
	jmp	.LBB51_65
.LBB51_30:                              # %sw.bb19.i
	movabsq	$.L.str174, %rcx
	jmp	.LBB51_65
.LBB51_31:                              # %sw.bb20.i
	movabsq	$.L.str175, %rcx
	jmp	.LBB51_65
.LBB51_32:                              # %sw.bb21.i
	movabsq	$.L.str176, %rcx
	jmp	.LBB51_65
.LBB51_33:                              # %sw.bb22.i
	movabsq	$.L.str177, %rcx
	jmp	.LBB51_65
.LBB51_34:                              # %sw.bb23.i
	movabsq	$.L.str178, %rcx
	jmp	.LBB51_65
.LBB51_35:                              # %sw.bb24.i
	movabsq	$.L.str179, %rcx
	jmp	.LBB51_65
.LBB51_36:                              # %sw.bb25.i
	movabsq	$.L.str180, %rcx
	jmp	.LBB51_65
.LBB51_37:                              # %sw.bb26.i
	movabsq	$.L.str181, %rcx
	jmp	.LBB51_65
.LBB51_38:                              # %sw.bb27.i
	movabsq	$.L.str182, %rcx
	jmp	.LBB51_65
.LBB51_39:                              # %sw.bb28.i
	movabsq	$.L.str183, %rcx
	jmp	.LBB51_65
.LBB51_40:                              # %sw.bb29.i
	movabsq	$.L.str184, %rcx
	jmp	.LBB51_65
.LBB51_41:                              # %sw.bb30.i
	movabsq	$.L.str185, %rcx
	jmp	.LBB51_65
.LBB51_42:                              # %sw.bb31.i
	movabsq	$.L.str186, %rcx
	jmp	.LBB51_65
.LBB51_43:                              # %sw.bb32.i
	movabsq	$.L.str187, %rcx
	jmp	.LBB51_65
.LBB51_44:                              # %sw.bb33.i
	movabsq	$.L.str188, %rcx
	jmp	.LBB51_65
.LBB51_45:                              # %sw.bb34.i
	movabsq	$.L.str189, %rcx
	jmp	.LBB51_65
.LBB51_46:                              # %sw.bb35.i
	movabsq	$.L.str190, %rcx
	jmp	.LBB51_65
.LBB51_47:                              # %sw.bb36.i
	movabsq	$.L.str191, %rcx
	jmp	.LBB51_65
.LBB51_48:                              # %sw.bb37.i
	movabsq	$.L.str192, %rcx
	jmp	.LBB51_65
.LBB51_49:                              # %sw.bb38.i
	movabsq	$.L.str193, %rcx
	jmp	.LBB51_65
.LBB51_50:                              # %sw.bb39.i
	movabsq	$.L.str194, %rcx
	jmp	.LBB51_65
.LBB51_51:                              # %sw.bb40.i
	movabsq	$.L.str195, %rcx
	jmp	.LBB51_65
.LBB51_52:                              # %sw.bb41.i
	movabsq	$.L.str196, %rcx
	jmp	.LBB51_65
.LBB51_53:                              # %sw.bb42.i
	movabsq	$.L.str197, %rcx
	jmp	.LBB51_65
.LBB51_54:                              # %sw.bb43.i
	movabsq	$.L.str198, %rcx
	jmp	.LBB51_65
.LBB51_55:                              # %sw.bb44.i
	movabsq	$.L.str199, %rcx
	jmp	.LBB51_65
.LBB51_56:                              # %sw.bb45.i
	movabsq	$.L.str200, %rcx
	jmp	.LBB51_65
.LBB51_57:                              # %sw.bb46.i
	movabsq	$.L.str201, %rcx
	jmp	.LBB51_65
.LBB51_58:                              # %sw.bb47.i
	movabsq	$.L.str202, %rcx
	jmp	.LBB51_65
.LBB51_176:                             # %if.end.i250
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_loc, %rcx
	movq	$4653, %rsi             # imm = 0x122D
	jmp	.LBB51_177
.LBB51_191:                             # %if.then50
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_die, %rcx
	movq	$6361, %rsi             # imm = 0x18D9
	jmp	.LBB51_177
.LBB51_183:                             # %if.end.i281
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
	jmp	.LBB51_177
.LBB51_198:                             # %if.then71
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_die, %rcx
	movq	$6377, %rsi             # imm = 0x18E9
	jmp	.LBB51_177
.LBB51_195:                             # %if.then61
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_die, %rcx
	movq	$6373, %rsi             # imm = 0x18E5
	jmp	.LBB51_177
.LBB51_216:                             # %if.end.i354
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_string, %rcx
	movq	$4518, %rsi             # imm = 0x11A6
.LBB51_177:                             # %if.end.i250
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp552:
	.size	output_die, .Ltmp552-output_die
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI51_0:
	.quad	.LBB51_59
	.quad	.LBB51_60
	.quad	.LBB51_61
	.quad	.LBB51_62
	.quad	.LBB51_63
.LJTI51_1:
	.quad	.LBB51_65
	.quad	.LBB51_12
	.quad	.LBB51_13
	.quad	.LBB51_14
	.quad	.LBB51_15
	.quad	.LBB51_16
	.quad	.LBB51_64
	.quad	.LBB51_64
	.quad	.LBB51_17
	.quad	.LBB51_64
	.quad	.LBB51_18
	.quad	.LBB51_19
	.quad	.LBB51_64
	.quad	.LBB51_20
	.quad	.LBB51_64
	.quad	.LBB51_21
	.quad	.LBB51_22
	.quad	.LBB51_23
	.quad	.LBB51_24
	.quad	.LBB51_25
	.quad	.LBB51_64
	.quad	.LBB51_26
	.quad	.LBB51_27
	.quad	.LBB51_28
	.quad	.LBB51_29
	.quad	.LBB51_30
	.quad	.LBB51_31
	.quad	.LBB51_32
	.quad	.LBB51_33
	.quad	.LBB51_34
	.quad	.LBB51_35
	.quad	.LBB51_36
	.quad	.LBB51_37
	.quad	.LBB51_38
	.quad	.LBB51_39
	.quad	.LBB51_40
	.quad	.LBB51_41
	.quad	.LBB51_42
	.quad	.LBB51_43
	.quad	.LBB51_44
	.quad	.LBB51_45
	.quad	.LBB51_46
	.quad	.LBB51_47
	.quad	.LBB51_48
	.quad	.LBB51_49
	.quad	.LBB51_50
	.quad	.LBB51_51
	.quad	.LBB51_52
	.quad	.LBB51_53
	.quad	.LBB51_54
	.quad	.LBB51_55
	.quad	.LBB51_56
	.quad	.LBB51_57
	.quad	.LBB51_58
.LJTI51_2:
	.quad	.LBB51_160
	.quad	.LBB51_161
	.quad	.LBB51_162
	.quad	.LBB51_163
	.quad	.LBB51_164
	.quad	.LBB51_165
	.quad	.LBB51_166
.LJTI51_3:
	.quad	.LBB51_149
	.quad	.LBB51_150
	.quad	.LBB51_151
	.quad	.LBB51_152
	.quad	.LBB51_153
	.quad	.LBB51_154
	.quad	.LBB51_155
	.quad	.LBB51_156
	.quad	.LBB51_157
	.quad	.LBB51_158
	.quad	.LBB51_159
.LJTI51_4:
	.quad	.LBB51_170
	.quad	.LBB51_76
	.quad	.LBB51_77
	.quad	.LBB51_169
	.quad	.LBB51_169
	.quad	.LBB51_169
	.quad	.LBB51_169
	.quad	.LBB51_169
	.quad	.LBB51_78
	.quad	.LBB51_79
	.quad	.LBB51_80
	.quad	.LBB51_81
	.quad	.LBB51_82
	.quad	.LBB51_169
	.quad	.LBB51_83
	.quad	.LBB51_84
	.quad	.LBB51_85
	.quad	.LBB51_86
	.quad	.LBB51_87
	.quad	.LBB51_88
	.quad	.LBB51_89
	.quad	.LBB51_90
	.quad	.LBB51_91
	.quad	.LBB51_92
	.quad	.LBB51_93
	.quad	.LBB51_94
	.quad	.LBB51_95
	.quad	.LBB51_96
	.quad	.LBB51_97
	.quad	.LBB51_98
	.quad	.LBB51_169
	.quad	.LBB51_99
	.quad	.LBB51_100
	.quad	.LBB51_101
	.quad	.LBB51_169
	.quad	.LBB51_169
	.quad	.LBB51_102
	.quad	.LBB51_169
	.quad	.LBB51_103
	.quad	.LBB51_169
	.quad	.LBB51_169
	.quad	.LBB51_104
	.quad	.LBB51_169
	.quad	.LBB51_105
	.quad	.LBB51_169
	.quad	.LBB51_106
	.quad	.LBB51_107
	.quad	.LBB51_169
	.quad	.LBB51_108
	.quad	.LBB51_109
	.quad	.LBB51_110
	.quad	.LBB51_111
	.quad	.LBB51_112
	.quad	.LBB51_113
	.quad	.LBB51_114
	.quad	.LBB51_115
	.quad	.LBB51_116
	.quad	.LBB51_117
	.quad	.LBB51_118
	.quad	.LBB51_119
	.quad	.LBB51_120
	.quad	.LBB51_121
	.quad	.LBB51_122
	.quad	.LBB51_123
	.quad	.LBB51_124
	.quad	.LBB51_125
	.quad	.LBB51_126
	.quad	.LBB51_127
	.quad	.LBB51_128
	.quad	.LBB51_129
	.quad	.LBB51_130
	.quad	.LBB51_131
	.quad	.LBB51_132
	.quad	.LBB51_133
	.quad	.LBB51_134
	.quad	.LBB51_135
	.quad	.LBB51_136
	.quad	.LBB51_137
	.quad	.LBB51_138
	.quad	.LBB51_139
	.quad	.LBB51_140
	.quad	.LBB51_141
	.quad	.LBB51_142
	.quad	.LBB51_143
	.quad	.LBB51_144
	.quad	.LBB51_145
	.quad	.LBB51_146
	.quad	.LBB51_147
	.quad	.LBB51_148
.LJTI51_5:
	.quad	.LBB51_219
	.quad	.LBB51_199
	.quad	.LBB51_173
	.quad	.LBB51_190
	.quad	.LBB51_172
	.quad	.LBB51_179
	.quad	.LBB51_180
	.quad	.LBB51_185
	.quad	.LBB51_186
	.quad	.LBB51_189
	.quad	.LBB51_193
	.quad	.LBB51_202
	.quad	.LBB51_203
	.quad	.LBB51_204
	.quad	.LBB51_207

	.text
	.align	16, 0x90
	.type	unmark_dies,@function
unmark_dies:                            # @unmark_dies
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp555:
	.cfi_def_cfa_offset 16
.Ltmp556:
	.cfi_offset %rbx, -16
	xorq	%rax, %rax
	movl	%eax, 64(%rdi)
	movq	32(%rdi), %rbx
	jmp	.LBB52_2
	.align	16, 0x90
.LBB52_1:                               # %for.body
                                        #   in Loop: Header=BB52_2 Depth=1
	movq	%rbx, %rdi
	callq	unmark_dies
	movq	40(%rbx), %rbx
.LBB52_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB52_1
# BB#3:                                 # %for.end
	popq	%rbx
	retq
.Ltmp557:
	.size	unmark_dies, .Ltmp557-unmark_dies
	.cfi_endproc

	.align	16, 0x90
	.type	output_loc_sequence,@function
output_loc_sequence:                    # @output_loc_sequence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp564:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp565:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp566:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp567:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp568:
	.cfi_def_cfa_offset 48
.Ltmp569:
	.cfi_offset %rbx, -48
.Ltmp570:
	.cfi_offset %r12, -40
.Ltmp571:
	.cfi_offset %r13, -32
.Ltmp572:
	.cfi_offset %r14, -24
.Ltmp573:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB53_175
# BB#1:
	movabsq	$.L.str220, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$.L.str365, %r13
	movabsq	$.L.str213, %r14
	jmp	.LBB53_2
.LBB53_4:                               # %sw.bb1.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str221, %rcx
	jmp	.LBB53_149
.LBB53_5:                               # %sw.bb2.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str222, %rcx
	jmp	.LBB53_149
.LBB53_6:                               # %sw.bb3.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str223, %rcx
	jmp	.LBB53_149
.LBB53_7:                               # %sw.bb4.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str224, %rcx
	jmp	.LBB53_149
.LBB53_8:                               # %sw.bb5.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str225, %rcx
	jmp	.LBB53_149
.LBB53_9:                               # %sw.bb6.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str226, %rcx
	jmp	.LBB53_149
.LBB53_10:                              # %sw.bb7.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str227, %rcx
	jmp	.LBB53_149
.LBB53_11:                              # %sw.bb8.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str228, %rcx
	jmp	.LBB53_149
.LBB53_12:                              # %sw.bb9.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str229, %rcx
	jmp	.LBB53_149
.LBB53_13:                              # %sw.bb10.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str230, %rcx
	jmp	.LBB53_149
.LBB53_14:                              # %sw.bb11.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str231, %rcx
	jmp	.LBB53_149
.LBB53_15:                              # %sw.bb12.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str232, %rcx
	jmp	.LBB53_149
.LBB53_16:                              # %sw.bb13.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str233, %rcx
	jmp	.LBB53_149
.LBB53_17:                              # %sw.bb14.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str234, %rcx
	jmp	.LBB53_149
.LBB53_18:                              # %sw.bb15.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str235, %rcx
	jmp	.LBB53_149
.LBB53_19:                              # %sw.bb16.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str236, %rcx
	jmp	.LBB53_149
.LBB53_20:                              # %sw.bb17.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str237, %rcx
	jmp	.LBB53_149
.LBB53_21:                              # %sw.bb18.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str238, %rcx
	jmp	.LBB53_149
.LBB53_22:                              # %sw.bb19.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str239, %rcx
	jmp	.LBB53_149
.LBB53_23:                              # %sw.bb20.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str240, %rcx
	jmp	.LBB53_149
.LBB53_24:                              # %sw.bb21.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str241, %rcx
	jmp	.LBB53_149
.LBB53_25:                              # %sw.bb22.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str242, %rcx
	jmp	.LBB53_149
.LBB53_26:                              # %sw.bb23.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str243, %rcx
	jmp	.LBB53_149
.LBB53_27:                              # %sw.bb24.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str244, %rcx
	jmp	.LBB53_149
.LBB53_28:                              # %sw.bb25.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str245, %rcx
	jmp	.LBB53_149
.LBB53_29:                              # %sw.bb26.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str246, %rcx
	jmp	.LBB53_149
.LBB53_30:                              # %sw.bb27.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str247, %rcx
	jmp	.LBB53_149
.LBB53_31:                              # %sw.bb28.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str248, %rcx
	jmp	.LBB53_149
.LBB53_32:                              # %sw.bb29.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str249, %rcx
	jmp	.LBB53_149
.LBB53_33:                              # %sw.bb30.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str250, %rcx
	jmp	.LBB53_149
.LBB53_34:                              # %sw.bb31.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str251, %rcx
	jmp	.LBB53_149
.LBB53_35:                              # %sw.bb32.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str252, %rcx
	jmp	.LBB53_149
.LBB53_36:                              # %sw.bb33.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str253, %rcx
	jmp	.LBB53_149
.LBB53_37:                              # %sw.bb34.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str254, %rcx
	jmp	.LBB53_149
.LBB53_38:                              # %sw.bb35.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str255, %rcx
	jmp	.LBB53_149
.LBB53_39:                              # %sw.bb36.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str256, %rcx
	jmp	.LBB53_149
.LBB53_40:                              # %sw.bb37.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str257, %rcx
	jmp	.LBB53_149
.LBB53_41:                              # %sw.bb38.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str258, %rcx
	jmp	.LBB53_149
.LBB53_42:                              # %sw.bb39.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str259, %rcx
	jmp	.LBB53_149
.LBB53_43:                              # %sw.bb40.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str260, %rcx
	jmp	.LBB53_149
.LBB53_44:                              # %sw.bb41.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str261, %rcx
	jmp	.LBB53_149
.LBB53_45:                              # %sw.bb42.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str262, %rcx
	jmp	.LBB53_149
.LBB53_46:                              # %sw.bb43.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str263, %rcx
	jmp	.LBB53_149
.LBB53_47:                              # %sw.bb44.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str264, %rcx
	jmp	.LBB53_149
.LBB53_48:                              # %sw.bb45.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str265, %rcx
	jmp	.LBB53_149
.LBB53_49:                              # %sw.bb46.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str266, %rcx
	jmp	.LBB53_149
.LBB53_50:                              # %sw.bb47.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str267, %rcx
	jmp	.LBB53_149
.LBB53_51:                              # %sw.bb48.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str268, %rcx
	jmp	.LBB53_149
.LBB53_52:                              # %sw.bb49.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str269, %rcx
	jmp	.LBB53_149
.LBB53_53:                              # %sw.bb50.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str270, %rcx
	jmp	.LBB53_149
.LBB53_54:                              # %sw.bb51.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str271, %rcx
	jmp	.LBB53_149
.LBB53_55:                              # %sw.bb52.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str272, %rcx
	jmp	.LBB53_149
.LBB53_56:                              # %sw.bb53.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str273, %rcx
	jmp	.LBB53_149
.LBB53_57:                              # %sw.bb54.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str274, %rcx
	jmp	.LBB53_149
.LBB53_58:                              # %sw.bb55.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str275, %rcx
	jmp	.LBB53_149
.LBB53_59:                              # %sw.bb56.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str276, %rcx
	jmp	.LBB53_149
.LBB53_60:                              # %sw.bb57.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str277, %rcx
	jmp	.LBB53_149
.LBB53_61:                              # %sw.bb58.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str278, %rcx
	jmp	.LBB53_149
.LBB53_62:                              # %sw.bb59.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str279, %rcx
	jmp	.LBB53_149
.LBB53_63:                              # %sw.bb60.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str280, %rcx
	jmp	.LBB53_149
.LBB53_64:                              # %sw.bb61.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str281, %rcx
	jmp	.LBB53_149
.LBB53_65:                              # %sw.bb62.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str282, %rcx
	jmp	.LBB53_149
.LBB53_66:                              # %sw.bb63.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str283, %rcx
	jmp	.LBB53_149
.LBB53_67:                              # %sw.bb64.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str284, %rcx
	jmp	.LBB53_149
.LBB53_68:                              # %sw.bb65.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str285, %rcx
	jmp	.LBB53_149
.LBB53_69:                              # %sw.bb66.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str286, %rcx
	jmp	.LBB53_149
.LBB53_70:                              # %sw.bb67.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str287, %rcx
	jmp	.LBB53_149
.LBB53_71:                              # %sw.bb68.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str288, %rcx
	jmp	.LBB53_149
.LBB53_72:                              # %sw.bb69.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str289, %rcx
	jmp	.LBB53_149
.LBB53_73:                              # %sw.bb70.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str290, %rcx
	jmp	.LBB53_149
.LBB53_74:                              # %sw.bb71.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str291, %rcx
	jmp	.LBB53_149
.LBB53_75:                              # %sw.bb72.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str292, %rcx
	jmp	.LBB53_149
.LBB53_76:                              # %sw.bb73.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str293, %rcx
	jmp	.LBB53_149
.LBB53_77:                              # %sw.bb74.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str294, %rcx
	jmp	.LBB53_149
.LBB53_78:                              # %sw.bb75.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str295, %rcx
	jmp	.LBB53_149
.LBB53_79:                              # %sw.bb76.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str296, %rcx
	jmp	.LBB53_149
.LBB53_80:                              # %sw.bb77.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str297, %rcx
	jmp	.LBB53_149
.LBB53_81:                              # %sw.bb78.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str298, %rcx
	jmp	.LBB53_149
.LBB53_82:                              # %sw.bb79.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str299, %rcx
	jmp	.LBB53_149
.LBB53_83:                              # %sw.bb80.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str300, %rcx
	jmp	.LBB53_149
.LBB53_84:                              # %sw.bb81.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str301, %rcx
	jmp	.LBB53_149
.LBB53_85:                              # %sw.bb82.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str302, %rcx
	jmp	.LBB53_149
.LBB53_86:                              # %sw.bb83.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str303, %rcx
	jmp	.LBB53_149
.LBB53_87:                              # %sw.bb84.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str304, %rcx
	jmp	.LBB53_149
.LBB53_88:                              # %sw.bb85.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str305, %rcx
	jmp	.LBB53_149
.LBB53_89:                              # %sw.bb86.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str306, %rcx
	jmp	.LBB53_149
.LBB53_90:                              # %sw.bb87.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str307, %rcx
	jmp	.LBB53_149
.LBB53_91:                              # %sw.bb88.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str308, %rcx
	jmp	.LBB53_149
.LBB53_92:                              # %sw.bb89.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str309, %rcx
	jmp	.LBB53_149
.LBB53_93:                              # %sw.bb90.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str310, %rcx
	jmp	.LBB53_149
.LBB53_94:                              # %sw.bb91.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str311, %rcx
	jmp	.LBB53_149
.LBB53_95:                              # %sw.bb92.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str312, %rcx
	jmp	.LBB53_149
.LBB53_96:                              # %sw.bb93.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str313, %rcx
	jmp	.LBB53_149
.LBB53_97:                              # %sw.bb94.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str314, %rcx
	jmp	.LBB53_149
.LBB53_98:                              # %sw.bb95.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str315, %rcx
	jmp	.LBB53_149
.LBB53_99:                              # %sw.bb96.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str316, %rcx
	jmp	.LBB53_149
.LBB53_100:                             # %sw.bb97.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str317, %rcx
	jmp	.LBB53_149
.LBB53_101:                             # %sw.bb98.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str318, %rcx
	jmp	.LBB53_149
.LBB53_102:                             # %sw.bb99.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str319, %rcx
	jmp	.LBB53_149
.LBB53_103:                             # %sw.bb100.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str320, %rcx
	jmp	.LBB53_149
.LBB53_104:                             # %sw.bb101.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str321, %rcx
	jmp	.LBB53_149
.LBB53_105:                             # %sw.bb102.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str322, %rcx
	jmp	.LBB53_149
.LBB53_106:                             # %sw.bb103.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str323, %rcx
	jmp	.LBB53_149
.LBB53_107:                             # %sw.bb104.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str324, %rcx
	jmp	.LBB53_149
.LBB53_108:                             # %sw.bb105.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str325, %rcx
	jmp	.LBB53_149
.LBB53_109:                             # %sw.bb106.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str326, %rcx
	jmp	.LBB53_149
.LBB53_110:                             # %sw.bb107.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str327, %rcx
	jmp	.LBB53_149
.LBB53_111:                             # %sw.bb108.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str328, %rcx
	jmp	.LBB53_149
.LBB53_112:                             # %sw.bb109.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str329, %rcx
	jmp	.LBB53_149
.LBB53_113:                             # %sw.bb110.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str330, %rcx
	jmp	.LBB53_149
.LBB53_114:                             # %sw.bb111.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str331, %rcx
	jmp	.LBB53_149
.LBB53_115:                             # %sw.bb112.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str332, %rcx
	jmp	.LBB53_149
.LBB53_116:                             # %sw.bb113.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str333, %rcx
	jmp	.LBB53_149
.LBB53_117:                             # %sw.bb114.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str334, %rcx
	jmp	.LBB53_149
.LBB53_118:                             # %sw.bb115.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str335, %rcx
	jmp	.LBB53_149
.LBB53_119:                             # %sw.bb116.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str336, %rcx
	jmp	.LBB53_149
.LBB53_120:                             # %sw.bb117.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str337, %rcx
	jmp	.LBB53_149
.LBB53_121:                             # %sw.bb118.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str338, %rcx
	jmp	.LBB53_149
.LBB53_122:                             # %sw.bb119.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str339, %rcx
	jmp	.LBB53_149
.LBB53_123:                             # %sw.bb120.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str340, %rcx
	jmp	.LBB53_149
.LBB53_124:                             # %sw.bb121.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str341, %rcx
	jmp	.LBB53_149
.LBB53_125:                             # %sw.bb122.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str342, %rcx
	jmp	.LBB53_149
.LBB53_126:                             # %sw.bb123.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str343, %rcx
	jmp	.LBB53_149
.LBB53_127:                             # %sw.bb124.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str344, %rcx
	jmp	.LBB53_149
.LBB53_128:                             # %sw.bb125.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str345, %rcx
	jmp	.LBB53_149
.LBB53_129:                             # %sw.bb126.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str346, %rcx
	jmp	.LBB53_149
.LBB53_130:                             # %sw.bb127.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str347, %rcx
	jmp	.LBB53_149
.LBB53_131:                             # %sw.bb128.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str348, %rcx
	jmp	.LBB53_149
.LBB53_132:                             # %sw.bb129.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str349, %rcx
	jmp	.LBB53_149
.LBB53_133:                             # %sw.bb130.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str350, %rcx
	jmp	.LBB53_149
.LBB53_134:                             # %sw.bb131.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str351, %rcx
	jmp	.LBB53_149
.LBB53_135:                             # %sw.bb132.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str352, %rcx
	jmp	.LBB53_149
.LBB53_136:                             # %sw.bb133.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str353, %rcx
	jmp	.LBB53_149
.LBB53_137:                             # %sw.bb134.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str354, %rcx
	jmp	.LBB53_149
.LBB53_138:                             # %sw.bb135.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str355, %rcx
	jmp	.LBB53_149
.LBB53_139:                             # %sw.bb136.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str356, %rcx
	jmp	.LBB53_149
.LBB53_140:                             # %sw.bb137.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str357, %rcx
	jmp	.LBB53_149
.LBB53_141:                             # %sw.bb138.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str358, %rcx
	jmp	.LBB53_149
.LBB53_142:                             # %sw.bb139.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str359, %rcx
	jmp	.LBB53_149
.LBB53_143:                             # %sw.bb140.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str360, %rcx
	jmp	.LBB53_149
.LBB53_144:                             # %sw.bb141.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str361, %rcx
	jmp	.LBB53_149
.LBB53_145:                             # %sw.bb142.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str362, %rcx
	jmp	.LBB53_149
.LBB53_146:                             # %sw.bb143.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str363, %rcx
	jmp	.LBB53_149
.LBB53_147:                             # %sw.bb144.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movabsq	$.L.str364, %rcx
	jmp	.LBB53_149
.LBB53_172:                             # %sw.bb41.i20
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rsi
	movq	$1, %rdi
	jmp	.LBB53_173
.LBB53_161:                             # %sw.bb3.i7
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rsi
	movq	$4, %rdi
	jmp	.LBB53_173
.LBB53_170:                             # %sw.bb33.i18
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data_uleb128
	movq	48(%rbx), %rdi
	jmp	.LBB53_169
.LBB53_158:                             # %sw.bb.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	movq	24(%rbx), %rsi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_addr_rtx
	jmp	.LBB53_174
	.align	16, 0x90
.LBB53_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	8(%rbx), %esi
	leaq	-3(%rsi), %rax
	andq	%r12, %rax
	cmpq	$147, %rax
	ja	.LBB53_148
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	%r15, %rcx
	jmpq	*.LJTI53_0(,%rax,8)
	.align	16, 0x90
.LBB53_148:                             # %sw.default.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	%r13, %rcx
.LBB53_149:                             # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	dw2_asm_output_data
	movslq	8(%rbx), %rax
	cmpq	$111, %rax
	jg	.LBB53_156
# BB#150:                               # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	cmpq	$46, %rax
	jg	.LBB53_164
# BB#151:                               # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	cmpq	$34, %rax
	jg	.LBB53_154
# BB#152:                               # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	addq	$-3, %rax
	andq	%r12, %rax
	cmpq	$18, %rax
	ja	.LBB53_174
# BB#153:                               # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	jmpq	*.LJTI53_2(,%rax,8)
.LBB53_159:                             # %sw.bb1.i6
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rsi
	movq	$2, %rdi
	jmp	.LBB53_173
	.align	16, 0x90
.LBB53_156:                             # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	addq	$-112, %rax
	andq	%r12, %rax
	cmpq	$37, %rax
	ja	.LBB53_174
# BB#157:                               # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	jmpq	*.LJTI53_1(,%rax,8)
.LBB53_168:                             # %sw.bb15.i12
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rdi
.LBB53_169:                             # %output_loc_operands.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data_sleb128
	jmp	.LBB53_174
	.align	16, 0x90
.LBB53_164:                             # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	andq	%r12, %rax
	cmpq	$47, %rax
	jne	.LBB53_174
	jmp	.LBB53_165
.LBB53_154:                             # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	andq	%r12, %rax
	cmpq	$35, %rax
	jne	.LBB53_155
.LBB53_171:                             # %sw.bb13.i11
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data_uleb128
	jmp	.LBB53_174
.LBB53_155:                             # %dwarf_stack_op_name.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	cmpq	$40, %rax
	jne	.LBB53_174
.LBB53_165:                             # %sw.bb7.i9
                                        #   in Loop: Header=BB53_2 Depth=1
	movl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB53_167
# BB#166:                               # %if.then.i
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	24(%rbx), %rax
	movl	64(%rax), %esi
	movl	64(%rbx), %eax
	addq	$-3, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$2, %rdi
.LBB53_173:                             # %output_loc_operands.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	.align	16, 0x90
.LBB53_174:                             # %output_loc_operands.exit
                                        #   in Loop: Header=BB53_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB53_2
.LBB53_175:                             # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB53_162:                             # %sw.bb6.i8
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_loc_operands, %rcx
	movq	$2778, %rsi             # imm = 0xADA
	jmp	.LBB53_163
.LBB53_167:                             # %if.else.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_loc_operands, %rcx
	movq	$2789, %rsi             # imm = 0xAE5
.LBB53_163:                             # %sw.bb6.i8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp574:
	.size	output_loc_sequence, .Ltmp574-output_loc_sequence
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI53_0:
	.quad	.LBB53_149
	.quad	.LBB53_148
	.quad	.LBB53_148
	.quad	.LBB53_4
	.quad	.LBB53_148
	.quad	.LBB53_5
	.quad	.LBB53_6
	.quad	.LBB53_7
	.quad	.LBB53_8
	.quad	.LBB53_9
	.quad	.LBB53_10
	.quad	.LBB53_11
	.quad	.LBB53_12
	.quad	.LBB53_13
	.quad	.LBB53_14
	.quad	.LBB53_15
	.quad	.LBB53_16
	.quad	.LBB53_17
	.quad	.LBB53_18
	.quad	.LBB53_19
	.quad	.LBB53_20
	.quad	.LBB53_21
	.quad	.LBB53_22
	.quad	.LBB53_23
	.quad	.LBB53_24
	.quad	.LBB53_25
	.quad	.LBB53_26
	.quad	.LBB53_27
	.quad	.LBB53_28
	.quad	.LBB53_29
	.quad	.LBB53_30
	.quad	.LBB53_31
	.quad	.LBB53_32
	.quad	.LBB53_33
	.quad	.LBB53_34
	.quad	.LBB53_35
	.quad	.LBB53_36
	.quad	.LBB53_37
	.quad	.LBB53_38
	.quad	.LBB53_39
	.quad	.LBB53_40
	.quad	.LBB53_41
	.quad	.LBB53_42
	.quad	.LBB53_43
	.quad	.LBB53_44
	.quad	.LBB53_45
	.quad	.LBB53_46
	.quad	.LBB53_47
	.quad	.LBB53_48
	.quad	.LBB53_49
	.quad	.LBB53_50
	.quad	.LBB53_51
	.quad	.LBB53_52
	.quad	.LBB53_53
	.quad	.LBB53_54
	.quad	.LBB53_55
	.quad	.LBB53_56
	.quad	.LBB53_57
	.quad	.LBB53_58
	.quad	.LBB53_59
	.quad	.LBB53_60
	.quad	.LBB53_61
	.quad	.LBB53_62
	.quad	.LBB53_63
	.quad	.LBB53_64
	.quad	.LBB53_65
	.quad	.LBB53_66
	.quad	.LBB53_67
	.quad	.LBB53_68
	.quad	.LBB53_69
	.quad	.LBB53_70
	.quad	.LBB53_71
	.quad	.LBB53_72
	.quad	.LBB53_73
	.quad	.LBB53_74
	.quad	.LBB53_75
	.quad	.LBB53_76
	.quad	.LBB53_77
	.quad	.LBB53_78
	.quad	.LBB53_79
	.quad	.LBB53_80
	.quad	.LBB53_81
	.quad	.LBB53_82
	.quad	.LBB53_83
	.quad	.LBB53_84
	.quad	.LBB53_85
	.quad	.LBB53_86
	.quad	.LBB53_87
	.quad	.LBB53_88
	.quad	.LBB53_89
	.quad	.LBB53_90
	.quad	.LBB53_91
	.quad	.LBB53_92
	.quad	.LBB53_93
	.quad	.LBB53_94
	.quad	.LBB53_95
	.quad	.LBB53_96
	.quad	.LBB53_97
	.quad	.LBB53_98
	.quad	.LBB53_99
	.quad	.LBB53_100
	.quad	.LBB53_101
	.quad	.LBB53_102
	.quad	.LBB53_103
	.quad	.LBB53_104
	.quad	.LBB53_105
	.quad	.LBB53_106
	.quad	.LBB53_107
	.quad	.LBB53_108
	.quad	.LBB53_109
	.quad	.LBB53_110
	.quad	.LBB53_111
	.quad	.LBB53_112
	.quad	.LBB53_113
	.quad	.LBB53_114
	.quad	.LBB53_115
	.quad	.LBB53_116
	.quad	.LBB53_117
	.quad	.LBB53_118
	.quad	.LBB53_119
	.quad	.LBB53_120
	.quad	.LBB53_121
	.quad	.LBB53_122
	.quad	.LBB53_123
	.quad	.LBB53_124
	.quad	.LBB53_125
	.quad	.LBB53_126
	.quad	.LBB53_127
	.quad	.LBB53_128
	.quad	.LBB53_129
	.quad	.LBB53_130
	.quad	.LBB53_131
	.quad	.LBB53_132
	.quad	.LBB53_133
	.quad	.LBB53_134
	.quad	.LBB53_135
	.quad	.LBB53_136
	.quad	.LBB53_137
	.quad	.LBB53_138
	.quad	.LBB53_139
	.quad	.LBB53_140
	.quad	.LBB53_141
	.quad	.LBB53_142
	.quad	.LBB53_143
	.quad	.LBB53_144
	.quad	.LBB53_145
	.quad	.LBB53_146
	.quad	.LBB53_147
.LJTI53_1:
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_168
	.quad	.LBB53_171
	.quad	.LBB53_168
	.quad	.LBB53_170
	.quad	.LBB53_171
	.quad	.LBB53_172
	.quad	.LBB53_172
.LJTI53_2:
	.quad	.LBB53_158
	.quad	.LBB53_174
	.quad	.LBB53_174
	.quad	.LBB53_174
	.quad	.LBB53_174
	.quad	.LBB53_172
	.quad	.LBB53_172
	.quad	.LBB53_159
	.quad	.LBB53_159
	.quad	.LBB53_161
	.quad	.LBB53_161
	.quad	.LBB53_162
	.quad	.LBB53_162
	.quad	.LBB53_171
	.quad	.LBB53_168
	.quad	.LBB53_174
	.quad	.LBB53_174
	.quad	.LBB53_174
	.quad	.LBB53_172

	.text
	.align	16, 0x90
	.type	assign_symbol_names,@function
assign_symbol_names:                    # @assign_symbol_names
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp578:
	.cfi_def_cfa_offset 16
.Ltmp579:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp580:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$264, %rsp              # imm = 0x108
.Ltmp581:
	.cfi_offset %rbx, -40
.Ltmp582:
	.cfi_offset %r14, -32
.Ltmp583:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movl	(%r14), %ecx
	cmpq	$53, %rcx
	movq	%r14, %rax
	ja	.LBB54_2
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$9044941291225110, %rcx # imm = 0x20225380AD8016
	testq	%rcx, %rax
	movq	%r14, %rax
	je	.LBB54_2
.LBB54_16:                              # %if.then
	movq	comdat_symbol_id(%rip), %r15
	testq	%r15, %r15
	je	.LBB54_23
# BB#17:                                # %if.then2
	movq	%r15, %rdi
	callq	strlen
	movq	%rsp, %rbx
	addq	$79, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movl	comdat_symbol_number(%rip), %r8d
	leaq	1(%r8), %rax
	movl	%eax, comdat_symbol_number(%rip)
	movabsq	$.L.str383, %rsi
	movabsq	$.L.str366, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rcx
	callq	sprintf
	jmp	.LBB54_24
	.align	16, 0x90
.LBB54_2:                               # %tailrecurse.i10.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_5 Depth 2
	testq	%rax, %rax
	je	.LBB54_25
# BB#3:                                 # %if.then.i14.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB54_25
# BB#4:                                 #   in Loop: Header=BB54_2 Depth=1
	xorq	%rax, %rax
	.align	16, 0x90
.LBB54_5:                               # %for.body.i19.i
                                        #   Parent Loop BB54_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB54_19
# BB#6:                                 # %for.body.i19.i
                                        #   in Loop: Header=BB54_5 Depth=2
	cmpq	$60, %rdx
	je	.LBB54_7
# BB#18:                                # %for.body.i19.i
                                        #   in Loop: Header=BB54_5 Depth=2
	cmpq	$71, %rdx
	jne	.LBB54_21
.LBB54_19:                              # %land.lhs.true.i.i24.i
                                        #   in Loop: Header=BB54_5 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB54_29
# BB#20:                                # %AT_ref.exit.i28.i
                                        #   in Loop: Header=BB54_5 Depth=2
	movq	24(%rcx), %rax
.LBB54_21:                              # %for.inc.i33.i
                                        #   in Loop: Header=BB54_5 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB54_5
# BB#22:                                # %for.end.i35.i
                                        #   in Loop: Header=BB54_2 Depth=1
	testq	%rax, %rax
	jne	.LBB54_2
	jmp	.LBB54_25
.LBB54_7:
	movq	%r14, %rax
.LBB54_8:                               # %tailrecurse.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_10 Depth 2
	testq	%rax, %rax
	je	.LBB54_16
# BB#9:                                 # %if.then.i.i
                                        #   in Loop: Header=BB54_8 Depth=1
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB54_16
	.align	16, 0x90
.LBB54_10:                              # %for.body.i.i
                                        #   Parent Loop BB54_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$71, %rdx
	je	.LBB54_25
# BB#11:                                # %for.body.i.i
                                        #   in Loop: Header=BB54_10 Depth=2
	cmpq	$49, %rdx
	jne	.LBB54_14
# BB#12:                                # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB54_10 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB54_29
# BB#13:                                # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB54_10 Depth=2
	movq	24(%rcx), %rax
.LBB54_14:                              # %for.inc.i.i
                                        #   in Loop: Header=BB54_10 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB54_10
# BB#15:                                # %for.end.i.i
                                        #   in Loop: Header=BB54_8 Depth=1
	testq	%rax, %rax
	jne	.LBB54_8
	jmp	.LBB54_16
.LBB54_23:                              # %if.else
	movl	gen_internal_sym.label_num(%rip), %ecx
	leaq	1(%rcx), %rax
	movl	%eax, gen_internal_sym.label_num(%rip)
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str384, %rdx
	leaq	-288(%rbp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
.LBB54_24:                              # %if.end8
	movq	%rbx, %rdi
	callq	xstrdup
	movq	%rax, 8(%r14)
.LBB54_25:                              # %if.end8
	movq	32(%r14), %rbx
	jmp	.LBB54_27
	.align	16, 0x90
.LBB54_26:                              # %for.body
                                        #   in Loop: Header=BB54_27 Depth=1
	movq	%rbx, %rdi
	callq	assign_symbol_names
	movq	40(%rbx), %rbx
.LBB54_27:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB54_26
# BB#28:                                # %for.end
	leaq	-24(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB54_29:                              # %if.end.i.i25.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp584:
	.size	assign_symbol_names, .Ltmp584-assign_symbol_names
	.cfi_endproc

	.align	16, 0x90
	.type	compute_section_prefix,@function
compute_section_prefix:                 # @compute_section_prefix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp588:
	.cfi_def_cfa_offset 16
.Ltmp589:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp590:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$184, %rsp
.Ltmp591:
	.cfi_offset %rbx, -56
.Ltmp592:
	.cfi_offset %r12, -48
.Ltmp593:
	.cfi_offset %r13, -40
.Ltmp594:
	.cfi_offset %r14, -32
.Ltmp595:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movq	%r14, %rax
	xorq	%r13, %r13
.LBB55_1:                               # %tailrecurse.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB55_4 Depth 2
	testq	%rax, %rax
	je	.LBB55_2
# BB#3:                                 # %if.then.i.i
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rdi
	je	.LBB55_16
	.align	16, 0x90
.LBB55_4:                               # %for.body.i.i
                                        #   Parent Loop BB55_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB55_7
# BB#5:                                 # %for.body.i.i
                                        #   in Loop: Header=BB55_4 Depth=2
	cmpq	$3, %rdx
	je	.LBB55_13
# BB#6:                                 # %for.body.i.i
                                        #   in Loop: Header=BB55_4 Depth=2
	cmpq	$71, %rdx
	jne	.LBB55_11
.LBB55_7:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB55_4 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB55_8
# BB#10:                                # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB55_4 Depth=2
	movq	24(%rcx), %rax
.LBB55_11:                              # %for.inc.i.i
                                        #   in Loop: Header=BB55_4 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB55_4
# BB#12:                                # %for.end.i.i
                                        #   in Loop: Header=BB55_1 Depth=1
	xorq	%rdi, %rdi
	testq	%rax, %rax
	jne	.LBB55_1
	jmp	.LBB55_16
.LBB55_13:                              # %land.lhs.true.i.i
	movl	16(%rcx), %eax
	cmpq	$14, %rax
	jne	.LBB55_14
# BB#15:                                # %AT_string.exit.i
	movq	24(%rcx), %rax
	movq	8(%rax), %rdi
.LBB55_16:                              # %get_AT_string.exit
	callq	lbasename
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	strlen
	movq	%rsp, %rbx
	addq	$79, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	leaq	-224(%rbp), %r12
	movq	%r12, %rdi
	callq	md5_init_ctx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	die_checksum
	leaq	-64(%rbp), %rsi
	movq	%r12, %rdi
	callq	md5_finish_ctx
	movabsq	$.L.str381, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	sprintf
	movq	%rbx, %rdi
	callq	clean_symbol_name
	movq	%rbx, %rdi
	callq	strlen
	movzbl	-64(%rbp), %edx
	movq	%rax, %r15
	leaq	(%rbx,%r15), %rdi
	movabsq	$.L.str382, %r12
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	sprintf
	movzbl	-63(%rbp), %edx
	leaq	2(%rbx,%r15), %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	sprintf
	movzbl	-62(%rbp), %edx
	leaq	4(%rbx,%r15), %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	sprintf
	movzbl	-61(%rbp), %edx
	leaq	6(%r15,%rbx), %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	sprintf
	movq	%rbx, %rdi
	callq	xstrdup
	movq	%rax, 8(%r14)
	movq	%rax, comdat_symbol_id(%rip)
	movl	%r13d, comdat_symbol_number(%rip)
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB55_2:
	xorq	%rdi, %rdi
	jmp	.LBB55_16
.LBB55_8:                               # %if.end.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB55_9
.LBB55_14:                              # %if.end.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_string, %rcx
	movq	$4518, %rsi             # imm = 0x11A6
.LBB55_9:                               # %if.end.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp596:
	.size	compute_section_prefix, .Ltmp596-compute_section_prefix
	.cfi_endproc

	.align	16, 0x90
	.type	die_checksum,@function
die_checksum:                           # @die_checksum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp602:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp603:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp604:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp605:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp606:
	.cfi_def_cfa_offset 48
.Ltmp607:
	.cfi_offset %rbx, -40
.Ltmp608:
	.cfi_offset %r12, -32
.Ltmp609:
	.cfi_offset %r14, -24
.Ltmp610:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$4, %rsi
                                        # kill: RDI<def> R14<kill>
	movq	%r15, %rdx
	callq	md5_process_bytes
	movq	16(%r14), %rbx
	jmp	.LBB56_1
	.align	16, 0x90
.LBB56_20:                              # %attr_checksum.exit
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	8(%rbx), %rbx
.LBB56_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB56_14 Depth 2
	testq	%rbx, %rbx
	je	.LBB56_21
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	$4, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	md5_process_bytes
	movl	(%rbx), %eax
	cmpq	$37, %rax
	je	.LBB56_20
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB56_1 Depth=1
	cmpq	$58, %rax
	je	.LBB56_20
# BB#4:                                 # %if.end.i
                                        #   in Loop: Header=BB56_1 Depth=1
	movl	16(%rbx), %eax
	cmpq	$14, %rax
	ja	.LBB56_20
# BB#5:                                 # %if.end.i
                                        #   in Loop: Header=BB56_1 Depth=1
	jmpq	*.LJTI56_0(,%rax,8)
.LBB56_12:                              # %sw.bb.i
                                        #   in Loop: Header=BB56_1 Depth=1
	leaq	24(%rbx), %rdi
.LBB56_18:                              # %attr_checksum.exit
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	$8, %rsi
	jmp	.LBB56_19
.LBB56_6:                               # %sw.bb10.i
                                        #   in Loop: Header=BB56_1 Depth=1
	leaq	24(%rbx), %rdi
	movq	$16, %rsi
	jmp	.LBB56_19
.LBB56_9:                               # %AT_addr.exit.i
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	24(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$68, %rcx
	je	.LBB56_10
	jmp	.LBB56_11
.LBB56_13:                              # %AT_loc.exit.i
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	24(%rbx), %r12
	jmp	.LBB56_14
.LBB56_7:                               # %sw.bb13.i
                                        #   in Loop: Header=BB56_1 Depth=1
	leaq	24(%rbx), %rdi
	movq	$1, %rsi
	jmp	.LBB56_19
.LBB56_16:                              # %AT_ref.exit110.i
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	24(%rbx), %rdi
	cmpq	$0, 48(%rdi)
	je	.LBB56_20
# BB#17:                                # %AT_ref.exit.i
                                        #   in Loop: Header=BB56_1 Depth=1
	addq	$48, %rdi
	jmp	.LBB56_18
.LBB56_8:                               # %AT_string.exit87.i
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	24(%rbx), %rax
.LBB56_10:                              # %sw.bb22.i
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	8(%rax), %r12
	movq	%r12, %rdi
	callq	strlen
	movq	%r12, %rdi
	movq	%rax, %rsi
.LBB56_19:                              # %attr_checksum.exit
                                        #   in Loop: Header=BB56_1 Depth=1
	movq	%r15, %rdx
	callq	md5_process_bytes
	jmp	.LBB56_20
.LBB56_15:                              # %for.body.i
                                        #   in Loop: Header=BB56_14 Depth=2
	leaq	8(%r12), %rdi
	movq	$4, %rsi
	movq	%r15, %rdx
	callq	md5_process_bytes
	leaq	16(%r12), %rdi
	movq	$24, %rsi
	movq	%r15, %rdx
	callq	md5_process_bytes
	leaq	40(%r12), %rdi
	movq	$24, %rsi
	movq	%r15, %rdx
	callq	md5_process_bytes
	movq	(%r12), %r12
.LBB56_14:                              # %AT_loc.exit.i
                                        #   Parent Loop BB56_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r12, %r12
	je	.LBB56_20
	jmp	.LBB56_15
.LBB56_21:                              # %for.end
	movq	32(%r14), %rbx
	jmp	.LBB56_23
	.align	16, 0x90
.LBB56_22:                              # %for.body3
                                        #   in Loop: Header=BB56_23 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	die_checksum
	movq	40(%rbx), %rbx
.LBB56_23:                              # %for.body3
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB56_22
# BB#24:                                # %for.end5
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB56_11:                              # %sw.default.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.attr_checksum, %rcx
	movq	$5453, %rsi             # imm = 0x154D
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp611:
	.size	die_checksum, .Ltmp611-die_checksum
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI56_0:
	.quad	.LBB56_9
	.quad	.LBB56_12
	.quad	.LBB56_13
	.quad	.LBB56_20
	.quad	.LBB56_20
	.quad	.LBB56_12
	.quad	.LBB56_12
	.quad	.LBB56_6
	.quad	.LBB56_6
	.quad	.LBB56_7
	.quad	.LBB56_16
	.quad	.LBB56_20
	.quad	.LBB56_20
	.quad	.LBB56_20
	.quad	.LBB56_8

	.text
	.align	16, 0x90
	.type	remove_children,@function
remove_children:                        # @remove_children
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp618:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp619:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp620:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp621:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp622:
	.cfi_def_cfa_offset 48
.Ltmp623:
	.cfi_offset %rbx, -48
.Ltmp624:
	.cfi_offset %r12, -40
.Ltmp625:
	.cfi_offset %r13, -32
.Ltmp626:
	.cfi_offset %r14, -24
.Ltmp627:
	.cfi_offset %r15, -16
	movq	32(%rdi), %r14
	testq	%r14, %r14
	movq	$0, 32(%rdi)
	je	.LBB57_12
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB57_2:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB57_3 Depth 2
	movq	16(%r14), %r13
	movq	40(%r14), %r12
	jmp	.LBB57_3
	.align	16, 0x90
.LBB57_10:                              # %free_AT.exit
                                        #   in Loop: Header=BB57_3 Depth=2
	movq	%rbx, %rdi
	callq	free
.LBB57_3:                               # %while.body
                                        #   Parent Loop BB57_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	je	.LBB57_11
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB57_3 Depth=2
	movq	%r13, %rbx
	movl	16(%rbx), %eax
	movq	8(%rbx), %r13
	leaq	-12(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB57_9
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB57_3 Depth=2
	andq	%r15, %rax
	cmpq	$8, %rax
	jne	.LBB57_6
.LBB57_9:                               # %sw.bb8.i
                                        #   in Loop: Header=BB57_3 Depth=2
	movq	24(%rbx), %rdi
	callq	free
	jmp	.LBB57_10
.LBB57_6:                               # %for.body
                                        #   in Loop: Header=BB57_3 Depth=2
	cmpq	$14, %rax
	jne	.LBB57_10
# BB#7:                                 # %sw.bb.i
                                        #   in Loop: Header=BB57_3 Depth=2
	movq	24(%rbx), %rax
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB57_10
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB57_3 Depth=2
	decq	%rcx
	movl	%ecx, 16(%rax)
	jmp	.LBB57_10
	.align	16, 0x90
.LBB57_11:                              # %for.end
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	%r14, %rdi
	callq	remove_children
	movq	%r14, %rdi
	callq	free
	testq	%r12, %r12
	movq	%r12, %r14
	jne	.LBB57_2
.LBB57_12:                              # %while.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp628:
	.size	remove_children, .Ltmp628-remove_children
	.cfi_endproc

	.align	16, 0x90
	.type	gen_compile_unit_die,@function
gen_compile_unit_die:                   # @gen_compile_unit_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp635:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp636:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp637:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp638:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp639:
	.cfi_def_cfa_offset 48
	subq	$272, %rsp              # imm = 0x110
.Ltmp640:
	.cfi_def_cfa_offset 320
.Ltmp641:
	.cfi_offset %rbx, -48
.Ltmp642:
	.cfi_offset %r12, -40
.Ltmp643:
	.cfi_offset %r13, -32
.Ltmp644:
	.cfi_offset %r14, -24
.Ltmp645:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	callq	getpwd
	movq	%rax, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$17, %rax
	movl	%eax, (%r15)
	movq	$24, %rdi
	callq	xmalloc
	testq	%r12, %r12
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	$0, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	je	.LBB58_9
# BB#1:                                 # %land.lhs.true.i
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB58_9
# BB#2:                                 # %if.then.i
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	demangle_name_func(%rip), %rax
	testq	%rax, %rax
	movq	%r12, %r13
	je	.LBB58_4
# BB#3:                                 # %if.then3.i
	movq	%r12, %rdi
	callq	*%rax
	movq	%rax, %r13
.LBB58_4:                               # %if.end.i
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r14
	movq	%rax, %rbx
	testq	%r14, %r14
	jne	.LBB58_6
# BB#5:                                 # %if.then.i.i
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r14
.LBB58_6:                               # %if.end.i.i
	movq	%r13, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$3, %rcx
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r15, %r15
	movq	8(%rsp), %r13           # 8-byte Reload
	je	.LBB58_9
# BB#7:                                 # %if.end.i.i
	testq	%rbx, %rbx
	je	.LBB58_9
# BB#8:                                 # %if.then.i.i.i
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB58_9:                               # %add_name_attribute.exit
	testq	%r13, %r13
	je	.LBB58_16
# BB#10:                                # %land.lhs.true
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB58_16
# BB#11:                                # %if.then
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r14
	movq	%rax, %rbx
	testq	%r14, %r14
	jne	.LBB58_13
# BB#12:                                # %if.then.i54
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r14
.LBB58_13:                              # %if.end.i55
	movq	%r13, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$27, %rcx
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r15, %r15
	je	.LBB58_16
# BB#14:                                # %if.end.i55
	testq	%rbx, %rbx
	je	.LBB58_16
# BB#15:                                # %if.then.i.i56
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB58_16:                              # %if.end
	movq	lang_hooks(%rip), %r14
	movq	version_string(%rip), %rcx
	movabsq	$.L.str385, %rsi
	leaq	16(%rsp), %rdi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r12
	movq	%rax, %rbx
	testq	%r12, %r12
	jne	.LBB58_18
# BB#17:                                # %if.then.i62
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r12
.LBB58_18:                              # %if.end.i76
	leaq	16(%rsp), %r13
	movq	%r13, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$37, %rcx
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r15, %r15
	je	.LBB58_21
# BB#19:                                # %if.end.i76
	testq	%rbx, %rbx
	je	.LBB58_21
# BB#20:                                # %if.then.i.i78
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB58_21:                              # %add_AT_string.exit79
	movabsq	$.L.str386, %rsi
	movq	%r14, %rdi
	callq	strcmp
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	movq	$4, %rbx
	je	.LBB58_28
# BB#22:                                # %if.else
	movabsq	$.L.str387, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r12, %rax
	movq	$3, %rbx
	je	.LBB58_28
# BB#23:                                # %if.else14
	movabsq	$.L.str388, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r12, %rax
	movq	$7, %rbx
	je	.LBB58_28
# BB#24:                                # %if.else19
	movabsq	$.L.str389, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r12, %rax
	movq	$9, %rbx
	je	.LBB58_28
# BB#25:                                # %if.else24
	movabsq	$.L.str390, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r12, %rax
	movq	$11, %rbx
	je	.LBB58_28
# BB#26:                                # %if.else29
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB58_28
# BB#27:                                # %select.mid
	movq	$2, %rbx
.LBB58_28:                              # %if.end37
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$19, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB58_31
# BB#29:                                # %if.end37
	testq	%rax, %rax
	je	.LBB58_31
# BB#30:                                # %if.then.i.i90
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB58_31:                              # %add_AT_unsigned.exit
	movq	%r15, %rax
	addq	$272, %rsp              # imm = 0x110
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp646:
	.size	gen_compile_unit_die, .Ltmp646-gen_compile_unit_die
	.cfi_endproc

	.align	16, 0x90
	.type	indirect_string_alloc,@function
indirect_string_alloc:                  # @indirect_string_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp648:
	.cfi_def_cfa_offset 16
	movq	$32, %rdi
	callq	xmalloc
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	popq	%rdx
	retq
.Ltmp649:
	.size	indirect_string_alloc, .Ltmp649-indirect_string_alloc
	.cfi_endproc

	.align	16, 0x90
	.type	gen_type_die,@function
gen_type_die:                           # @gen_type_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp656:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp657:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp658:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp659:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp660:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp661:
	.cfi_def_cfa_offset 96
.Ltmp662:
	.cfi_offset %rbx, -48
.Ltmp663:
	.cfi_offset %r12, -40
.Ltmp664:
	.cfi_offset %r13, -32
.Ltmp665:
	.cfi_offset %r14, -24
.Ltmp666:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB60_397
# BB#1:                                 # %entry
	movq	global_trees(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB60_397
# BB#2:                                 # %if.end
	movq	128(%rdi), %rcx
	movl	16(%rcx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$18, %rdx
	jne	.LBB60_3
	.align	16, 0x90
.LBB60_4:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movq	128(%rbx), %rcx
	cmpq	%rcx, %rbx
	jne	.LBB60_4
# BB#5:                                 # %type_main_variant.exit.loopexit
	movl	16(%rbx), %eax
	jmp	.LBB60_6
.LBB60_3:
	movq	%rcx, %rbx
.LBB60_6:                               # %type_main_variant.exit
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB60_397
# BB#7:                                 # %if.end3
	movq	96(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_10
# BB#8:                                 # %land.lhs.true
	movzbl	16(%rdi), %ecx
	cmpq	$33, %rcx
	jne	.LBB60_10
# BB#9:                                 # %land.lhs.true12
	cmpq	$0, 96(%rdi)
	je	.LBB60_10
# BB#398:                               # %if.then16
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%rbx)
	movq	%r14, %rsi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_decl_die  # TAILCALL
.LBB60_10:                              # %if.end23
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$24, %rcx
	ja	.LBB60_394
# BB#11:                                # %if.end23
	jmpq	*.LJTI60_0(,%rcx,8)
.LBB60_104:                             # %sw.bb69
	movq	144(%rbx), %rdi
	xorq	%r15, %r15
	testq	%rdi, %rdi
	je	.LBB60_105
# BB#106:                               # %land.lhs.true72
	movl	16(%rdi), %ecx
	movq	%rcx, %rsi
	andq	$255, %rsi
	addq	$-18, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$4, %rsi
	ja	.LBB60_112
# BB#107:                               # %land.lhs.true72
	andq	$16384, %rcx            # imm = 0x4000
	testq	%rdx, %rcx
	jne	.LBB60_112
# BB#108:                               # %if.then121
	movq	%r14, %rsi
	callq	gen_type_die
	movzbl	17(%rbx), %eax
	testq	$64, %rax
	jne	.LBB60_397
# BB#109:                               # %if.end131
	movq	decl_scope_table(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	movq	144(%rbx), %r14
	cmpq	%rsi, %rcx
	jb	.LBB60_111
# BB#110:                               # %if.then.i218
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, decl_scope_table(%rip)
	movq	8(%rax), %rcx
.LBB60_111:                             # %push_decl_scope.exit
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
	movq	144(%rbx), %rdi
	movq	88(%rdi), %r14
	movl	16(%rbx), %eax
	movq	$1, %r15
	jmp	.LBB60_112
.LBB60_395:                             # %sw.bb
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%rbx)
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	jmp	.LBB60_396
.LBB60_105:
	xorq	%rdi, %rdi
.LBB60_112:                             # %if.end138
	movq	88(%rbx), %r12
	andq	$255, %rax
	cmpq	$10, %rax
	jne	.LBB60_203
# BB#113:                               # %if.then144
	testq	%r12, %r12
	je	.LBB60_114
# BB#133:                               # %if.else.i
	movq	%r15, 32(%rsp)          # 8-byte Spill
	cmpq	$0, 32(%rbx)
	je	.LBB60_391
# BB#134:                               # %if.then.i90.i
	leaq	16(%r12), %rcx
	movq	%r12, %r13
.LBB60_135:                             # %for.cond.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB60_144
# BB#136:                               # %for.body.i.i
                                        #   in Loop: Header=BB60_135 Depth=1
	movl	(%r12), %edx
	leaq	8(%r12), %rcx
	cmpq	$60, %rdx
	jne	.LBB60_135
# BB#137:                               # %if.then5.i.i
	movq	8(%r12), %rcx
	movq	%rcx, (%rax)
	movl	16(%r12), %eax
	leaq	-12(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB60_142
# BB#138:                               # %if.then5.i.i
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB60_139
.LBB60_142:                             # %sw.bb8.i.i.i
	movq	24(%r12), %rdi
	callq	free
.LBB60_143:                             # %free_AT.exit.i.i
	movq	%r12, %rdi
	callq	free
	jmp	.LBB60_144
.LBB60_203:                             # %if.else145
	xorq	%rax, %rax
	cmpq	$0, 32(%rbx)
	je	.LBB60_212
# BB#204:                               # %land.rhs.i
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB60_205
# BB#209:                               # %lor.rhs.i
	movzbl	49(%rax), %eax
	testq	$1, %rax
	jne	.LBB60_210
# BB#211:                               # %lor.rhs.i
	movabsq	$1, %rax
	jmp	.LBB60_212
.LBB60_394:                             # %sw.default
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.gen_type_die, %rdx
	movq	$11200, %rsi            # imm = 0x2BC0
	callq	fancy_abort
.LBB60_12:                              # %sw.bb35
	movq	112(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	movq	%rax, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	testq	%r12, %r12
	movq	$31, %rax
	movl	%eax, (%r15)
	je	.LBB60_18
# BB#13:                                # %if.then.i.i246
	testq	%r15, %r15
	je	.LBB60_19
# BB#14:                                # %if.then.i.i.i248
	cmpq	%r12, %r15
	je	.LBB60_15
# BB#17:                                # %if.end.i.i.i253
	movq	%r12, 24(%r15)
	movq	32(%r12), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r12)
	jmp	.LBB60_19
.LBB60_80:                              # %sw.bb47
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movq	8(%rbx), %r12
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	movq	%rax, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	testq	%r13, %r13
	movq	$21, %rax
	movl	%eax, (%r15)
	je	.LBB60_84
# BB#81:                                # %if.then.i564
	testq	%r15, %r15
	je	.LBB60_85
# BB#82:                                # %if.then.i.i566
	cmpq	%r13, %r15
	je	.LBB60_15
# BB#83:                                # %if.end.i.i571
	movq	%r13, 24(%r15)
	movq	32(%r13), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r13)
	jmp	.LBB60_85
.LBB60_95:                              # %sw.bb50
	movzbl	62(%rbx), %eax
	testq	$1, %rax
	je	.LBB60_103
# BB#96:                                # %land.lhs.true56
	movq	8(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$12, %rax
	jne	.LBB60_103
# BB#97:                                # %if.then64
	movq	%r14, %rsi
	callq	gen_type_die
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	movq	%rax, %r15
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r14
	testq	%r15, %r15
	movq	$18, %rax
	movl	%eax, (%r14)
	je	.LBB60_101
# BB#98:                                # %if.then.i.i224
	testq	%r14, %r14
	je	.LBB60_102
# BB#99:                                # %if.then.i.i.i226
	cmpq	%r15, %r14
	je	.LBB60_15
# BB#100:                               # %if.end.i.i.i231
	movq	%r15, 24(%r14)
	movq	32(%r15), %rax
	movq	%rax, 40(%r14)
	movq	%r14, 32(%r15)
	movq	%r14, 88(%rbx)
	jmp	.LBB60_396
.LBB60_103:                             # %sw.bb68
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_array_type_die
	jmp	.LBB60_396
.LBB60_32:                              # %sw.bb39
	movq	24(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	movq	%rax, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	testq	%r12, %r12
	movq	$32, %rax
	movl	%eax, (%r15)
	je	.LBB60_36
# BB#33:                                # %if.then.i.i290
	testq	%r15, %r15
	je	.LBB60_37
# BB#34:                                # %if.then.i.i.i292
	cmpq	%r12, %r15
	je	.LBB60_15
# BB#35:                                # %if.end.i.i.i297
	movq	%r12, 24(%r15)
	movq	32(%r12), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r12)
	jmp	.LBB60_37
.LBB60_46:                              # %sw.bb44
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movq	8(%rbx), %r12
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	movq	%rax, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	testq	%r13, %r13
	movq	$21, %rax
	movl	%eax, (%r15)
	je	.LBB60_50
# BB#47:                                # %if.then.i499
	testq	%r15, %r15
	je	.LBB60_51
# BB#48:                                # %if.then.i.i501
	cmpq	%r13, %r15
	je	.LBB60_15
# BB#49:                                # %if.end.i.i506
	movq	%r13, 24(%r15)
	movq	32(%r13), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r13)
	jmp	.LBB60_51
.LBB60_114:                             # %if.then.i215
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	movq	%rax, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	testq	%r12, %r12
	movq	$4, %rax
	movl	%eax, (%r13)
	je	.LBB60_118
# BB#115:                               # %if.then.i.i
	movq	%r15, 32(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	je	.LBB60_119
# BB#116:                               # %if.then.i.i78.i
	cmpq	%r12, %r13
	je	.LBB60_15
# BB#117:                               # %if.end.i.i.i
	movq	%r12, 24(%r13)
	movq	32(%r12), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%r12)
	jmp	.LBB60_119
.LBB60_205:                             # %land.end.i.thread
	movq	%r15, 32(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	jne	.LBB60_206
# BB#207:                               # %land.end.i.thread
	movabsq	$1, %r15
	movq	$1, %rax
	jmp	.LBB60_217
.LBB60_18:                              # %if.else.i.i258
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_19:                              # %new_die.exit.i268
	movq	%r15, 88(%rbx)
	movq	112(%rbx), %rax
	movq	88(%rax), %r12
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$29, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%r12, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB60_22
# BB#20:                                # %new_die.exit.i268
	testq	%rax, %rax
	je	.LBB60_22
# BB#21:                                # %if.then.i.i15.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB60_22:                              # %add_AT_die_ref.exit.i
	movq	8(%rbx), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB60_26
# BB#23:                                # %land.lhs.true.i.i272
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB60_26
# BB#24:                                # %land.lhs.true6.i.i
	cmpq	$0, 96(%rdi)
	jne	.LBB60_26
# BB#25:                                # %if.then.i16.i
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
.LBB60_26:                              # %if.end.i.i273
	testq	%rax, %rax
	je	.LBB60_396
# BB#27:                                # %if.end.i.i273
	cmpq	$5, %rax
	je	.LBB60_396
# BB#28:                                # %if.end19.i.i
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r14, %rcx
	callq	modified_type_die
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB60_396
# BB#29:                                # %if.then34.i.i
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$73, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%r14, 24(%rax)
	movl	%ecx, 32(%rax)
	jne	.LBB60_30
	jmp	.LBB60_396
.LBB60_84:                              # %if.else.i576
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_85:                              # %new_die.exit578
	movq	%r15, 88(%rbx)
	movq	comp_unit_die(%rip), %rcx
.LBB60_86:                              # %tailrecurse.i.i.i581
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_88 Depth 2
	testq	%rcx, %rcx
	je	.LBB60_69
# BB#87:                                # %if.then.i.i.i585
                                        #   in Loop: Header=BB60_86 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB60_69
.LBB60_88:                              # %for.body.i.i.i589
                                        #   Parent Loop BB60_86 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB60_91
# BB#89:                                # %for.body.i.i.i589
                                        #   in Loop: Header=BB60_88 Depth=2
	cmpq	$19, %rdx
	je	.LBB60_62
# BB#90:                                # %for.body.i.i.i589
                                        #   in Loop: Header=BB60_88 Depth=2
	cmpq	$71, %rdx
	jne	.LBB60_93
.LBB60_91:                              # %land.lhs.true.i.i.i.i593
                                        #   in Loop: Header=BB60_88 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB60_58
# BB#92:                                # %AT_ref.exit.i.i.i597
                                        #   in Loop: Header=BB60_88 Depth=2
	movq	24(%rax), %rcx
.LBB60_93:                              # %for.inc.i.i.i602
                                        #   in Loop: Header=BB60_88 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB60_88
# BB#94:                                # %for.end.i.i.i604
                                        #   in Loop: Header=BB60_86 Depth=1
	testq	%rcx, %rcx
	jne	.LBB60_86
	jmp	.LBB60_69
.LBB60_36:                              # %if.else.i.i302
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_37:                              # %new_die.exit.i308
	movq	%r15, 88(%rbx)
	movq	8(%rbx), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB60_41
# BB#38:                                # %land.lhs.true.i.i311
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB60_41
# BB#39:                                # %land.lhs.true6.i.i313
	cmpq	$0, 96(%rdi)
	jne	.LBB60_41
# BB#40:                                # %if.then.i8.i
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
.LBB60_41:                              # %if.end.i.i319
	testq	%rax, %rax
	je	.LBB60_396
# BB#42:                                # %if.end.i.i319
	cmpq	$5, %rax
	je	.LBB60_396
# BB#43:                                # %if.end19.i.i331
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r14, %rcx
	callq	modified_type_die
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB60_396
# BB#44:                                # %if.then34.i.i343
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB60_396
.LBB60_30:                              # %if.then34.i.i
	testq	%rax, %rax
	je	.LBB60_396
# BB#31:                                # %if.then.i.i.i.i284
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	jmp	.LBB60_396
.LBB60_50:                              # %if.else.i508
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_51:                              # %new_die.exit
	movq	%r15, 88(%rbx)
	movq	comp_unit_die(%rip), %rcx
.LBB60_52:                              # %tailrecurse.i.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_54 Depth 2
	testq	%rcx, %rcx
	je	.LBB60_69
# BB#53:                                # %if.then.i.i.i512
                                        #   in Loop: Header=BB60_52 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB60_69
.LBB60_54:                              # %for.body.i.i.i514
                                        #   Parent Loop BB60_52 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB60_57
# BB#55:                                # %for.body.i.i.i514
                                        #   in Loop: Header=BB60_54 Depth=2
	cmpq	$19, %rdx
	je	.LBB60_62
# BB#56:                                # %for.body.i.i.i514
                                        #   in Loop: Header=BB60_54 Depth=2
	cmpq	$71, %rdx
	jne	.LBB60_60
.LBB60_57:                              # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB60_54 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB60_58
# BB#59:                                # %AT_ref.exit.i.i.i
                                        #   in Loop: Header=BB60_54 Depth=2
	movq	24(%rax), %rcx
.LBB60_60:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB60_54 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB60_54
# BB#61:                                # %for.end.i.i.i
                                        #   in Loop: Header=BB60_52 Depth=1
	testq	%rcx, %rcx
	jne	.LBB60_52
	jmp	.LBB60_69
.LBB60_62:                              # %land.lhs.true.i.i.i521
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB60_63
# BB#64:                                # %get_AT_unsigned.exit.i
	movl	24(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB60_69
# BB#65:                                # %land.lhs.true.i
	cmpq	$0, 24(%rbx)
	je	.LBB60_69
# BB#66:                                # %if.then.i533
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$39, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r15, %r15
	je	.LBB60_69
# BB#67:                                # %if.then.i533
	testq	%rax, %rax
	je	.LBB60_69
# BB#68:                                # %if.then.i.i5.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB60_69:                              # %add_prototyped_attribute.exit
	movl	16(%r12), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB60_73
# BB#70:                                # %land.lhs.true.i538
	movq	8(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB60_73
# BB#71:                                # %land.lhs.true6.i
	cmpq	$0, 96(%r12)
	jne	.LBB60_73
# BB#72:                                # %if.then.i541
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %r12
.LBB60_73:                              # %if.end.i543
	testq	%rax, %rax
	je	.LBB60_79
# BB#74:                                # %if.end.i543
	cmpq	$5, %rax
	je	.LBB60_79
# BB#75:                                # %if.end19.i
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r12, %rdi
	movq	%r14, %rcx
	callq	modified_type_die
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB60_79
# BB#76:                                # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB60_79
# BB#77:                                # %if.then34.i
	testq	%rax, %rax
	je	.LBB60_79
# BB#78:                                # %if.then.i.i.i558
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB60_79:                              # %add_type_attribute.exit
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_formal_types_die
.LBB60_396:                             # %sw.epilog
	movl	16(%rbx), %eax
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%rbx)
.LBB60_397:                             # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB60_210:
	movabsq	$0, %rax
.LBB60_212:                             # %land.end.i
	movq	%r15, 32(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	jne	.LBB60_213
# BB#214:                               # %land.end.i
	movabsq	$1, %r15
	jmp	.LBB60_215
.LBB60_213:
	movabsq	$0, %r15
.LBB60_215:                             # %land.end.i
	je	.LBB60_217
# BB#216:                               # %land.end.i
	testq	%rax, %rax
	je	.LBB60_391
	jmp	.LBB60_217
.LBB60_118:                             # %if.else.i.i
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r13, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_119:                             # %new_die.exit.i
	movq	%r13, 88(%rbx)
	movq	96(%rbx), %rax
	testq	%rax, %rax
	je	.LBB60_144
# BB#120:                               # %if.then.i80.i
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB60_124
# BB#121:                               # %if.then.i80.i
	cmpq	$33, %rcx
	jne	.LBB60_144
# BB#122:                               # %land.lhs.true.i.i
	movzbl	49(%rax), %ecx
	testq	$64, %rcx
	jne	.LBB60_144
# BB#123:                               # %if.end25.i.i
	movq	72(%rax), %rax
	testq	%rax, %rax
	je	.LBB60_144
.LBB60_124:                             # %if.end29.i.i
	movq	32(%rax), %r12
	testq	%r12, %r12
	je	.LBB60_144
# BB#125:                               # %lor.lhs.false.i.i
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB60_144
# BB#126:                               # %if.then.i85.i
	movq	demangle_name_func(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB60_128
# BB#127:                               # %if.then3.i.i
	movq	%r12, %rdi
	callq	*%rcx
	movq	%rax, %r12
.LBB60_128:                             # %if.end.i.i
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %rcx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB60_130
# BB#129:                               # %if.then.i.i87.i
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %rcx
.LBB60_130:                             # %if.end.i.i88.i
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	testq	%r13, %r13
	movl	%ecx, 16(%rax)
	movq	$0, 8(%r15)
	movq	$3, %rcx
	movl	%ecx, (%r15)
	movq	$14, %rcx
	movl	%ecx, 16(%r15)
	movq	%rax, 24(%r15)
	je	.LBB60_144
# BB#131:                               # %if.end.i.i88.i
	testq	%r15, %r15
	je	.LBB60_144
# BB#132:                               # %if.then.i.i.i.i
	movq	16(%r13), %rax
	movq	%rax, 8(%r15)
	movq	%r15, 16(%r13)
.LBB60_144:                             # %if.end7.i
	cmpq	$0, 32(%rbx)
	je	.LBB60_200
# BB#145:                               # %if.then11.i
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	orq	$16384, %rcx            # imm = 0x4000
	movl	%ecx, 16(%rbx)
	andq	$255, %rax
	cmpq	$36, %rax
	jg	.LBB60_150
# BB#146:                               # %if.then11.i
	leaq	-20(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	jb	.LBB60_149
# BB#147:                               # %if.then11.i
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB60_160
# BB#148:                               # %if.then11.i
	cmpq	$10, %rax
	jne	.LBB60_399
.LBB60_149:                             # %sw.bb1.i.i
	movq	%rbx, %rdi
	callq	int_size_in_bytes
	movq	%rax, %r12
	jmp	.LBB60_160
.LBB60_200:                             # %if.else45.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$60, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r13, %r13
	je	.LBB60_391
# BB#201:                               # %if.else45.i
	testq	%rax, %rax
	je	.LBB60_391
# BB#202:                               # %if.then.i.i.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
	jmp	.LBB60_391
.LBB60_150:                             # %if.then11.i
	cmpq	$37, %rax
	jne	.LBB60_399
# BB#151:                               # %if.end.i.i104.i
	movq	96(%rbx), %r15
	testq	%r15, %r15
	jne	.LBB60_153
# BB#152:                               # %if.then3.i.i105.i
	movq	8(%rbx), %r15
.LBB60_153:                             # %field_type.exit.i.i
	movzbl	16(%r15), %eax
	testq	%rax, %rax
	je	.LBB60_154
# BB#155:                               # %if.else.i214.i
	movq	32(%r15), %rdi
	xorq	%r12, %r12
	testq	%rdi, %rdi
	je	.LBB60_159
# BB#156:                               # %if.else5.i.i
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_158
# BB#157:                               # %if.then9.i.i
	movq	32(%r15), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r12
	jmp	.LBB60_159
.LBB60_206:
	movabsq	$0, %r15
	movq	$1, %rax
.LBB60_217:                             # %if.end.i
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r12, 40(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	testq	%rdi, %rdi
	je	.LBB60_219
# BB#218:                               # %land.lhs.true9.i
	movzbl	16(%rdi), %eax
	addq	$-18, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	sbbq	%r13, %r13
	andq	$1, %r13
.LBB60_219:                             # %if.end48.i
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB60_222
# BB#220:                               # %lor.lhs.false51.i
	testq	%r13, %r13
	je	.LBB60_250
# BB#221:                               # %lor.lhs.false51.i
	movq	comp_unit_die(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB60_250
.LBB60_222:                             # %if.then55.i
	movq	%rax, %r12
	movzbl	16(%rbx), %eax
	cmpq	$20, %rax
	je	.LBB60_223
# BB#224:                               # %if.then55.i
	movabsq	$1, %rax
	jmp	.LBB60_225
.LBB60_223:
	movabsq	$0, %rax
.LBB60_225:                             # %if.then55.i
	leaq	19(,%rax,4), %r15
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	movl	%r15d, (%r13)
	testq	%r12, %r12
	je	.LBB60_229
# BB#226:                               # %if.then.i.i366
	testq	%r13, %r13
	je	.LBB60_230
# BB#227:                               # %if.then.i.i.i368
	cmpq	%r12, %r13
	je	.LBB60_15
# BB#228:                               # %if.end.i.i.i373
	movq	%r12, 24(%r13)
	movq	32(%r12), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%r12)
	jmp	.LBB60_230
.LBB60_250:                             # %if.else66.i
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	jne	.LBB60_261
# BB#251:                               # %if.then.i169.i
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	16(%rax), %rcx
.LBB60_252:                             # %for.cond.i.i435
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %r13
	testq	%r13, %r13
	je	.LBB60_261
# BB#253:                               # %for.body.i.i436
                                        #   in Loop: Header=BB60_252 Depth=1
	movl	(%r13), %edx
	leaq	8(%r13), %rcx
	cmpq	$60, %rdx
	jne	.LBB60_252
# BB#254:                               # %if.then5.i.i439
	movq	8(%r13), %rcx
	movq	%rcx, (%rax)
	movl	16(%r13), %eax
	leaq	-12(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB60_259
# BB#255:                               # %if.then5.i.i439
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB60_256
.LBB60_259:                             # %sw.bb8.i.i.i448
	movq	24(%r13), %rdi
	callq	free
	jmp	.LBB60_260
.LBB60_229:                             # %if.else.i.i378
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r13, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_230:                             # %new_die.exit.i381
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r12          # 8-byte Reload
	testq	%r12, %r12
	movq	%r13, 88(%rbx)
	je	.LBB60_235
# BB#231:                               # %if.then63.i
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r13, %r13
	movq	$0, 8(%rax)
	movq	$71, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%r12, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB60_232
# BB#233:                               # %if.then63.i
	testq	%rax, %rax
	movq	%r13, 40(%rsp)          # 8-byte Spill
	je	.LBB60_261
# BB#234:                               # %if.then.i.i157.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jmp	.LBB60_261
.LBB60_235:                             # %if.else.i392
	movq	96(%rbx), %rax
	testq	%rax, %rax
	movq	%r13, 40(%rsp)          # 8-byte Spill
	je	.LBB60_261
# BB#236:                               # %if.then.i159.i
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB60_240
# BB#237:                               # %if.then.i159.i
	cmpq	$33, %rcx
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jne	.LBB60_261
# BB#238:                               # %land.lhs.true.i.i400
	movzbl	49(%rax), %ecx
	testq	$64, %rcx
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jne	.LBB60_261
# BB#239:                               # %if.end25.i.i403
	movq	72(%rax), %rax
	testq	%rax, %rax
	movq	%r13, 40(%rsp)          # 8-byte Spill
	je	.LBB60_261
.LBB60_240:                             # %if.end29.i.i407
	movq	32(%rax), %r12
	testq	%r12, %r12
	movq	%r13, 40(%rsp)          # 8-byte Spill
	je	.LBB60_261
# BB#241:                               # %lor.lhs.false.i.i409
	movzbl	(%r12), %eax
	testq	%rax, %rax
	movq	%r13, 40(%rsp)          # 8-byte Spill
	je	.LBB60_261
# BB#242:                               # %if.then.i164.i
	movq	demangle_name_func(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB60_244
# BB#243:                               # %if.then3.i.i410
	movq	%r12, %rdi
	callq	*%rcx
	movq	%rax, %r12
.LBB60_244:                             # %if.end.i.i414
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r15
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%r15, %r15
	jne	.LBB60_246
# BB#245:                               # %if.then.i.i166.i
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r15
.LBB60_246:                             # %if.end.i.i167.i
	movq	%r12, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	testq	%r13, %r13
	movl	%ecx, 16(%rax)
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	$0, 8(%rdx)
	movq	$3, %rcx
	movl	%ecx, (%rdx)
	movq	$14, %rcx
	movl	%ecx, 16(%rdx)
	movq	%rax, 24(%rdx)
	movq	%rdx, %rcx
	je	.LBB60_247
# BB#248:                               # %if.end.i.i167.i
	testq	%rcx, %rcx
	movq	%r13, 40(%rsp)          # 8-byte Spill
	je	.LBB60_261
# BB#249:                               # %if.then.i.i.i.i432
	movq	16(%r13), %rax
	movq	%rax, 8(%rcx)
	movq	%rcx, 16(%r13)
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jmp	.LBB60_261
.LBB60_154:                             # %if.then.i212.i
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %r12       # imm = 0xFFFFFFE0
	andq	%rax, %r12
	jmp	.LBB60_159
.LBB60_101:                             # %if.else.i.i236
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r14, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB60_102:                             # %gen_string_type_die.exit
	movq	%r14, 88(%rbx)
	jmp	.LBB60_396
.LBB60_232:
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jmp	.LBB60_261
.LBB60_139:                             # %if.then5.i.i
	cmpq	$14, %rax
	jne	.LBB60_143
# BB#140:                               # %sw.bb.i.i.i
	movq	24(%r12), %rax
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB60_143
# BB#141:                               # %if.then.i.i98.i
	decq	%rcx
	movl	%ecx, 16(%rax)
	jmp	.LBB60_143
.LBB60_158:                             # %if.else13.i.i
	movl	64(%r15), %r12d
.LBB60_159:                             # %simple_type_size_in_bits.exit.i
	movq	$3, %rcx
	shrq	%cl, %r12
.LBB60_160:                             # %sw.epilog.i.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$11, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r12, 24(%rax)
	testq	%r13, %r13
	je	.LBB60_163
# BB#161:                               # %sw.epilog.i.i
	testq	%rax, %rax
	je	.LBB60_163
# BB#162:                               # %if.then.i.i.i116.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB60_163:                             # %add_byte_size_attribute.exit.i
	movq	(%rbx), %r15
	testq	%r15, %r15
	je	.LBB60_170
# BB#164:                               # %if.then14.i
	movq	24(%r15), %rdi
	callq	lookup_filename
	movq	%rax, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	movq	$40, %rdi
	callq	xmalloc
	testq	%r13, %r13
	movq	$0, 8(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r12, 24(%rax)
	je	.LBB60_167
# BB#165:                               # %if.then14.i
	testq	%rax, %rax
	je	.LBB60_167
# BB#166:                               # %if.then.i.i.i129.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB60_167:                             # %add_AT_unsigned.exit.i.i
	movslq	32(%r15), %r15
	movq	$40, %rdi
	callq	xmalloc
	testq	%r13, %r13
	movq	$0, 8(%rax)
	movq	$59, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	je	.LBB60_170
# BB#168:                               # %add_AT_unsigned.exit.i.i
	testq	%rax, %rax
	je	.LBB60_170
# BB#169:                               # %if.then.i.i17.i.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB60_170:                             # %if.end17.i
	cmpq	$0, 24(%r13)
	jne	.LBB60_175
# BB#171:                               # %if.then19.i
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	scope_die_for
	testq	%rax, %rax
	je	.LBB60_175
# BB#172:                               # %if.then19.i
	testq	%r13, %r13
	je	.LBB60_175
# BB#173:                               # %if.then.i132.i
	cmpq	%r13, %rax
	je	.LBB60_15
# BB#174:                               # %if.end.i134.i
	movq	%rax, 24(%r13)
	movq	32(%rax), %rcx
	movq	%rcx, 40(%r13)
	movq	%r13, 32(%rax)
.LBB60_175:                             # %if.end21.i
	movq	24(%rbx), %r15
	jmp	.LBB60_176
.LBB60_199:                             # %for.inc.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	(%r15), %r15
.LBB60_176:                             # %if.end21.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB60_391
# BB#177:                               # %for.body.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r14
	testq	%r13, %r13
	movq	$40, %rax
	movl	%eax, (%r14)
	jne	.LBB60_178
# BB#181:                               # %if.else.i151.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r14, (%rax)
	movq	%r15, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	jmp	.LBB60_182
.LBB60_178:                             # %if.then.i139.i
                                        #   in Loop: Header=BB60_176 Depth=1
	testq	%r14, %r14
	je	.LBB60_182
# BB#179:                               # %if.then.i.i141.i
                                        #   in Loop: Header=BB60_176 Depth=1
	cmpq	%r13, %r14
	je	.LBB60_15
# BB#180:                               # %if.end.i.i146.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	%r13, 24(%r14)
	movq	32(%r13), %rax
	movq	%rax, 40(%r14)
	movq	%r14, 32(%r13)
.LBB60_182:                             # %new_die.exit153.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	24(%r15), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB60_192
# BB#183:                               # %land.lhs.true.i156.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_192
# BB#184:                               # %if.then.i158.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	demangle_name_func(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB60_186
# BB#185:                               #   in Loop: Header=BB60_176 Depth=1
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jmp	.LBB60_187
.LBB60_186:                             # %if.then3.i160.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	*%rcx
	movq	%rax, %rbx
.LBB60_187:                             # %if.end.i164.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r13
	movq	%rax, %r12
	testq	%r13, %r13
	jne	.LBB60_189
# BB#188:                               # %if.then.i.i168.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r13
.LBB60_189:                             # %if.end.i.i182.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	%rbx, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%r12)
	movq	$3, %rcx
	movl	%ecx, (%r12)
	movq	$14, %rcx
	movl	%ecx, 16(%r12)
	movq	%rax, 24(%r12)
	testq	%r14, %r14
	movq	40(%rsp), %r13          # 8-byte Reload
	je	.LBB60_192
# BB#190:                               # %if.end.i.i182.i
                                        #   in Loop: Header=BB60_176 Depth=1
	testq	%r12, %r12
	je	.LBB60_192
# BB#191:                               # %if.then.i.i.i184.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	16(%r14), %rax
	movq	%rax, 8(%r12)
	movq	%r12, 16(%r14)
.LBB60_192:                             # %add_name_attribute.exit185.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_199
# BB#193:                               # %if.then28.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	32(%r15), %rdi
	callq	tree_int_cst_sgn
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	32(%r15), %rdi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r12
	movq	$40, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	movq	$0, 8(%rax)
	movq	$28, %rcx
	movl	%ecx, (%rax)
	jns	.LBB60_195
# BB#194:                               # %if.then33.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	$5, %rcx
	jmp	.LBB60_196
.LBB60_195:                             # %if.else37.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	$6, %rcx
.LBB60_196:                             # %if.else37.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movl	%ecx, 16(%rax)
	movq	%r12, 24(%rax)
	testq	%r14, %r14
	je	.LBB60_199
# BB#197:                               # %if.else37.i
                                        #   in Loop: Header=BB60_176 Depth=1
	testq	%rax, %rax
	je	.LBB60_199
# BB#198:                               # %if.then.i.i207.i
                                        #   in Loop: Header=BB60_176 Depth=1
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
	jmp	.LBB60_199
.LBB60_256:                             # %if.then5.i.i439
	cmpq	$14, %rax
	jne	.LBB60_260
# BB#257:                               # %sw.bb.i.i.i441
	movq	24(%r13), %rax
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB60_260
# BB#258:                               # %if.then.i.i177.i
	decq	%rcx
	movl	%ecx, 16(%rax)
.LBB60_260:                             # %free_AT.exit.i.i449
	movq	%r13, %rdi
	callq	free
.LBB60_261:                             # %if.end67.i
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB60_383
# BB#262:                               # %if.then69.i
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	orq	$16384, %rcx            # imm = 0x4000
	movl	%ecx, 16(%rbx)
	andq	$255, %rax
	cmpq	$36, %rax
	jg	.LBB60_267
# BB#263:                               # %if.then69.i
	leaq	-20(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	jb	.LBB60_266
# BB#264:                               # %if.then69.i
	xorq	%r13, %r13
	testq	%rax, %rax
	je	.LBB60_277
# BB#265:                               # %if.then69.i
	cmpq	$10, %rax
	jne	.LBB60_399
.LBB60_266:                             # %sw.bb1.i.i453
	movq	%rbx, %rdi
	callq	int_size_in_bytes
	movq	%rax, %r13
	jmp	.LBB60_277
.LBB60_383:                             # %if.else91.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$60, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	movq	40(%rsp), %r12          # 8-byte Reload
	testq	%r12, %r12
	je	.LBB60_386
# BB#384:                               # %if.else91.i
	testq	%rax, %rax
	je	.LBB60_386
# BB#385:                               # %if.then.i.i246.i
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
.LBB60_386:                             # %add_AT_flag.exit.i
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_391
# BB#387:                               # %land.lhs.true95.i
	callq	decl_function_context
	testq	%rax, %rax
	jne	.LBB60_391
# BB#388:                               # %do.body.i
	movq	incomplete_types(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB60_390
# BB#389:                               # %if.then102.i
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, incomplete_types(%rip)
	movq	8(%rax), %rcx
.LBB60_390:                             # %if.end105.i
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	jmp	.LBB60_391
.LBB60_267:                             # %if.then69.i
	cmpq	$37, %rax
	jne	.LBB60_399
# BB#268:                               # %if.end.i.i183.i
	movq	96(%rbx), %r15
	testq	%r15, %r15
	jne	.LBB60_270
# BB#269:                               # %if.then3.i.i184.i
	movq	8(%rbx), %r15
.LBB60_270:                             # %field_type.exit.i.i458
	movzbl	16(%r15), %eax
	testq	%rax, %rax
	je	.LBB60_271
# BB#272:                               # %if.else.i702
	movq	32(%r15), %rdi
	xorq	%r13, %r13
	testq	%rdi, %rdi
	je	.LBB60_276
# BB#273:                               # %if.else5.i
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB60_275
# BB#274:                               # %if.then9.i
	movq	32(%r15), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r13
	jmp	.LBB60_276
.LBB60_271:                             # %if.then.i700
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %r13       # imm = 0xFFFFFFE0
	andq	%rax, %r13
	jmp	.LBB60_276
.LBB60_275:                             # %if.else13.i
	movl	64(%r15), %r13d
.LBB60_276:                             # %simple_type_size_in_bits.exit
	movq	$3, %rcx
	shrq	%cl, %r13
.LBB60_277:                             # %sw.epilog.i.i463
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$11, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, 24(%rax)
	movq	40(%rsp), %r13          # 8-byte Reload
	testq	%r13, %r13
	je	.LBB60_280
# BB#278:                               # %sw.epilog.i.i463
	testq	%rax, %rax
	je	.LBB60_280
# BB#279:                               # %if.then.i.i.i195.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB60_280:                             # %add_byte_size_attribute.exit.i464
	movq	(%rbx), %r12
	testq	%r12, %r12
	je	.LBB60_287
# BB#281:                               # %if.then76.i
	movq	24(%r12), %rdi
	callq	lookup_filename
	movq	%r13, %r15
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, 24(%rax)
	movq	%r15, %r13
	je	.LBB60_284
# BB#282:                               # %if.then76.i
	testq	%rax, %rax
	je	.LBB60_284
# BB#283:                               # %if.then.i.i.i208.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB60_284:                             # %add_AT_unsigned.exit.i.i478
	movslq	32(%r12), %r15
	movq	$40, %rdi
	callq	xmalloc
	testq	%r13, %r13
	movq	$0, 8(%rax)
	movq	$59, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	je	.LBB60_287
# BB#285:                               # %add_AT_unsigned.exit.i.i478
	testq	%rax, %rax
	je	.LBB60_287
# BB#286:                               # %if.then.i.i17.i.i480
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB60_287:                             # %if.end79.i
	movq	%r14, 24(%rsp)          # 8-byte Spill
	cmpq	$0, 24(%r13)
	movq	16(%rsp), %rcx          # 8-byte Reload
	jne	.LBB60_292
# BB#288:                               # %if.end79.i
	testq	%rcx, %rcx
	je	.LBB60_292
# BB#289:                               # %if.end79.i
	testq	%r13, %r13
	je	.LBB60_292
# BB#290:                               # %if.then.i211.i
	cmpq	%r13, %rcx
	je	.LBB60_15
# BB#291:                               # %if.end.i213.i
	movq	%rcx, 24(%r13)
	movq	32(%rcx), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%rcx)
.LBB60_292:                             # %if.end82.i
	movq	decl_scope_table(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB60_294
# BB#293:                               # %if.then.i216.i
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, decl_scope_table(%rip)
	movq	8(%rax), %rcx
.LBB60_294:                             # %push_decl_scope.exit.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rsi, %r12
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	movq	136(%rbx), %rax
	testq	%rax, %rax
	movq	%r13, %rsi
	je	.LBB60_299
# BB#295:                               # %land.lhs.true.i249.i
	movq	64(%rax), %rax
	testq	%rax, %rax
	je	.LBB60_299
# BB#296:                               # %if.then.i250.i
	movslq	24(%rax), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB60_299
# BB#297:
	movq	$4, %r15
	andq	%r12, 40(%rsp)          # 8-byte Folded Spill
.LBB60_298:                             # %for.body.i254.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r15,8), %rdi
	movq	%rsi, %r13
	movq	%rax, %r14
	callq	gen_inheritance_die
	movq	%r13, %rsi
	leaq	-3(%r15), %rax
	andq	%r12, %rax
	incq	%r15
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%r14, %rax
	jne	.LBB60_298
.LBB60_299:                             # %if.end.i255.i
	movq	24(%rbx), %r12
	testq	%r12, %r12
	movq	%rsi, %r15
	movq	24(%rsp), %r14          # 8-byte Reload
	je	.LBB60_337
.LBB60_300:                             # %for.body17.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_306 Depth 2
                                        #       Child Loop BB60_308 Depth 3
                                        #     Child Loop BB60_319 Depth 2
                                        #       Child Loop BB60_321 Depth 3
                                        #     Child Loop BB60_332 Depth 2
	movl	36(%r12), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB60_335
# BB#301:                               # %lookup_decl_die.exit.i.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	jne	.LBB60_302
.LBB60_335:                             # %if.else.i263.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	gen_decl_die
	jmp	.LBB60_336
.LBB60_302:                             # %if.then19.i.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	24(%rax), %rcx
	cmpq	%r15, %rcx
	movq	%rax, %rsi
	je	.LBB60_303
.LBB60_306:                             # %tailrecurse.i.i744
                                        #   Parent Loop BB60_300 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB60_308 Depth 3
	xorq	%rcx, %rcx
	testq	%rsi, %rsi
	je	.LBB60_316
# BB#307:                               # %if.then.i.i748
                                        #   in Loop: Header=BB60_306 Depth=2
	movq	16(%rsi), %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	movabsq	$0, %rsi
	je	.LBB60_316
.LBB60_308:                             # %for.body.i.i753
                                        #   Parent Loop BB60_300 Depth=1
                                        #     Parent Loop BB60_306 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rdx), %edi
	cmpq	$71, %rdi
	je	.LBB60_314
# BB#309:                               # %for.body.i.i753
                                        #   in Loop: Header=BB60_308 Depth=3
	cmpq	$49, %rdi
	jne	.LBB60_312
# BB#310:                               # %land.lhs.true.i.i.i758
                                        #   in Loop: Header=BB60_308 Depth=3
	movl	16(%rdx), %esi
	cmpq	$10, %rsi
	jne	.LBB60_58
# BB#311:                               # %AT_ref.exit.i.i762
                                        #   in Loop: Header=BB60_308 Depth=3
	movq	24(%rdx), %rsi
.LBB60_312:                             # %for.inc.i.i767
                                        #   in Loop: Header=BB60_308 Depth=3
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB60_308
# BB#313:                               # %for.end.i.i769
                                        #   in Loop: Header=BB60_306 Depth=2
	testq	%rsi, %rsi
	jne	.LBB60_306
	jmp	.LBB60_316
.LBB60_314:                             # %land.lhs.true.i.i773
                                        #   in Loop: Header=BB60_300 Depth=1
	movl	16(%rdx), %ecx
	cmpq	$10, %rcx
	jne	.LBB60_58
# BB#315:                               # %AT_ref.exit.i777
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	24(%rdx), %rcx
.LBB60_316:                             # %get_AT_ref.exit779
                                        #   in Loop: Header=BB60_300 Depth=1
	testq	%rcx, %rcx
	je	.LBB60_318
# BB#317:                               # %get_AT_ref.exit779
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	%rcx, %rax
.LBB60_318:                             # %get_AT_ref.exit779
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	24(%rax), %rcx
	cmpq	%r15, %rcx
	movq	%r15, %rdi
	jne	.LBB60_319
.LBB60_303:                             #   in Loop: Header=BB60_300 Depth=1
	movq	%r15, %rcx
	jmp	.LBB60_304
.LBB60_319:                             # %tailrecurse.i.i706
                                        #   Parent Loop BB60_300 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB60_321 Depth 3
	xorq	%r8, %r8
	testq	%rdi, %rdi
	je	.LBB60_329
# BB#320:                               # %if.then.i.i710
                                        #   in Loop: Header=BB60_319 Depth=2
	movq	16(%rdi), %rsi
	xorq	%r8, %r8
	testq	%rsi, %rsi
	movabsq	$0, %rdi
	je	.LBB60_329
.LBB60_321:                             # %for.body.i.i715
                                        #   Parent Loop BB60_300 Depth=1
                                        #     Parent Loop BB60_319 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rsi), %edx
	cmpq	$71, %rdx
	je	.LBB60_327
# BB#322:                               # %for.body.i.i715
                                        #   in Loop: Header=BB60_321 Depth=3
	cmpq	$49, %rdx
	jne	.LBB60_325
# BB#323:                               # %land.lhs.true.i.i.i720
                                        #   in Loop: Header=BB60_321 Depth=3
	movl	16(%rsi), %edx
	cmpq	$10, %rdx
	jne	.LBB60_58
# BB#324:                               # %AT_ref.exit.i.i724
                                        #   in Loop: Header=BB60_321 Depth=3
	movq	24(%rsi), %rdi
.LBB60_325:                             # %for.inc.i.i729
                                        #   in Loop: Header=BB60_321 Depth=3
	movq	8(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB60_321
# BB#326:                               # %for.end.i.i731
                                        #   in Loop: Header=BB60_319 Depth=2
	testq	%rdi, %rdi
	jne	.LBB60_319
	jmp	.LBB60_329
.LBB60_327:                             # %land.lhs.true.i.i735
                                        #   in Loop: Header=BB60_300 Depth=1
	movl	16(%rsi), %edx
	cmpq	$10, %rdx
	jne	.LBB60_58
# BB#328:                               # %AT_ref.exit.i739
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	24(%rsi), %r8
.LBB60_329:                             # %get_AT_ref.exit741
                                        #   in Loop: Header=BB60_300 Depth=1
	cmpq	%r8, %rcx
	jne	.LBB60_330
.LBB60_304:                             # %if.end9.i.i.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	32(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB60_334
# BB#305:                               #   in Loop: Header=BB60_300 Depth=1
	addq	$32, %rcx
.LBB60_332:                             # %for.body.i.i.i
                                        #   Parent Loop BB60_300 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rax, %rdx
	je	.LBB60_333
# BB#331:                               # %for.cond.i.i.i
                                        #   in Loop: Header=BB60_332 Depth=2
	movq	%rdx, %rcx
	addq	$40, %rcx
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.LBB60_332
	jmp	.LBB60_334
.LBB60_333:                             # %if.then13.i.i.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	40(%rax), %rdx
	movq	%rdx, (%rcx)
.LBB60_334:                             # %splice_child_die.exit.i.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	32(%r15), %rcx
	movq	%rcx, 40(%rax)
	movq	%rax, 32(%r15)
.LBB60_336:                             # %for.inc21.i.i
                                        #   in Loop: Header=BB60_300 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB60_300
.LBB60_337:                             # %for.end22.i.i
	movq	112(%rbx), %r12
	jmp	.LBB60_338
.LBB60_376:                             # %for.inc35.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	(%r12), %r12
.LBB60_338:                             # %for.end22.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_344 Depth 2
                                        #       Child Loop BB60_346 Depth 3
                                        #     Child Loop BB60_358 Depth 2
                                        #       Child Loop BB60_360 Depth 3
                                        #     Child Loop BB60_372 Depth 2
	testq	%r12, %r12
	je	.LBB60_377
# BB#339:                               # %for.body26.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	cmpq	$0, 112(%r12)
	jne	.LBB60_376
# BB#340:                               # %if.end29.i264.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movl	36(%r12), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB60_375
# BB#341:                               # %lookup_decl_die.exit67.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	jne	.LBB60_342
.LBB60_375:                             # %if.else33.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	gen_decl_die
	jmp	.LBB60_376
.LBB60_342:                             # %if.then32.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	24(%rax), %rcx
	cmpq	%r15, %rcx
	movq	%rax, %rsi
	jne	.LBB60_344
# BB#343:                               #   in Loop: Header=BB60_338 Depth=1
	movq	%r15, %rcx
	jmp	.LBB60_369
.LBB60_344:                             # %tailrecurse.i.i782
                                        #   Parent Loop BB60_338 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB60_346 Depth 3
	xorq	%rcx, %rcx
	testq	%rsi, %rsi
	je	.LBB60_354
# BB#345:                               # %if.then.i.i786
                                        #   in Loop: Header=BB60_344 Depth=2
	movq	16(%rsi), %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	movabsq	$0, %rsi
	je	.LBB60_354
.LBB60_346:                             # %for.body.i.i791
                                        #   Parent Loop BB60_338 Depth=1
                                        #     Parent Loop BB60_344 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rdx), %edi
	cmpq	$71, %rdi
	je	.LBB60_352
# BB#347:                               # %for.body.i.i791
                                        #   in Loop: Header=BB60_346 Depth=3
	cmpq	$49, %rdi
	jne	.LBB60_350
# BB#348:                               # %land.lhs.true.i.i.i796
                                        #   in Loop: Header=BB60_346 Depth=3
	movl	16(%rdx), %esi
	cmpq	$10, %rsi
	jne	.LBB60_58
# BB#349:                               # %AT_ref.exit.i.i800
                                        #   in Loop: Header=BB60_346 Depth=3
	movq	24(%rdx), %rsi
.LBB60_350:                             # %for.inc.i.i805
                                        #   in Loop: Header=BB60_346 Depth=3
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB60_346
# BB#351:                               # %for.end.i.i807
                                        #   in Loop: Header=BB60_344 Depth=2
	testq	%rsi, %rsi
	jne	.LBB60_344
	jmp	.LBB60_354
.LBB60_352:                             # %land.lhs.true.i.i811
                                        #   in Loop: Header=BB60_338 Depth=1
	movl	16(%rdx), %ecx
	cmpq	$10, %rcx
	jne	.LBB60_58
# BB#353:                               # %AT_ref.exit.i815
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	24(%rdx), %rcx
.LBB60_354:                             # %get_AT_ref.exit817
                                        #   in Loop: Header=BB60_338 Depth=1
	testq	%rcx, %rcx
	je	.LBB60_356
# BB#355:                               # %get_AT_ref.exit817
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	%rcx, %rax
.LBB60_356:                             # %get_AT_ref.exit817
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	24(%rax), %rcx
	cmpq	%r15, %rcx
	movq	%r15, %rdi
	jne	.LBB60_358
# BB#357:                               #   in Loop: Header=BB60_338 Depth=1
	movq	%r15, %rcx
	jmp	.LBB60_369
.LBB60_358:                             # %tailrecurse.i.i
                                        #   Parent Loop BB60_338 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB60_360 Depth 3
	xorq	%r8, %r8
	testq	%rdi, %rdi
	je	.LBB60_368
# BB#359:                               # %if.then.i.i678
                                        #   in Loop: Header=BB60_358 Depth=2
	movq	16(%rdi), %rsi
	xorq	%r8, %r8
	testq	%rsi, %rsi
	movabsq	$0, %rdi
	je	.LBB60_368
.LBB60_360:                             # %for.body.i.i681
                                        #   Parent Loop BB60_338 Depth=1
                                        #     Parent Loop BB60_358 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rsi), %edx
	cmpq	$71, %rdx
	je	.LBB60_366
# BB#361:                               # %for.body.i.i681
                                        #   in Loop: Header=BB60_360 Depth=3
	cmpq	$49, %rdx
	jne	.LBB60_364
# BB#362:                               # %land.lhs.true.i.i.i686
                                        #   in Loop: Header=BB60_360 Depth=3
	movl	16(%rsi), %edx
	cmpq	$10, %rdx
	jne	.LBB60_58
# BB#363:                               # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB60_360 Depth=3
	movq	24(%rsi), %rdi
.LBB60_364:                             # %for.inc.i.i
                                        #   in Loop: Header=BB60_360 Depth=3
	movq	8(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB60_360
# BB#365:                               # %for.end.i.i
                                        #   in Loop: Header=BB60_358 Depth=2
	testq	%rdi, %rdi
	jne	.LBB60_358
	jmp	.LBB60_368
.LBB60_366:                             # %land.lhs.true.i.i692
                                        #   in Loop: Header=BB60_338 Depth=1
	movl	16(%rsi), %edx
	cmpq	$10, %rdx
	jne	.LBB60_58
# BB#367:                               # %AT_ref.exit.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	24(%rsi), %r8
.LBB60_368:                             # %get_AT_ref.exit
                                        #   in Loop: Header=BB60_338 Depth=1
	cmpq	%r8, %rcx
	jne	.LBB60_330
.LBB60_369:                             # %if.end9.i87.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	32(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB60_374
# BB#370:                               #   in Loop: Header=BB60_338 Depth=1
	addq	$32, %rcx
.LBB60_372:                             # %for.body.i93.i.i
                                        #   Parent Loop BB60_338 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rax, %rdx
	je	.LBB60_373
# BB#371:                               # %for.cond.i89.i.i
                                        #   in Loop: Header=BB60_372 Depth=2
	movq	%rdx, %rcx
	addq	$40, %rcx
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.LBB60_372
	jmp	.LBB60_374
.LBB60_373:                             # %if.then13.i95.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	40(%rax), %rdx
	movq	%rdx, (%rcx)
.LBB60_374:                             # %splice_child_die.exit100.i.i
                                        #   in Loop: Header=BB60_338 Depth=1
	movq	32(%r15), %rcx
	movq	%rcx, 40(%rax)
	movq	%rax, 32(%r15)
	movq	(%r12), %r12
	jmp	.LBB60_338
.LBB60_377:                             # %gen_member_die.exit.i
	movq	decl_scope_table(%rip), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB60_378
# BB#379:                               # %pop_decl_scope.exit.i
	decq	%rcx
	movq	%rcx, 8(%rax)
	movq	104(%rbx), %rax
	testq	%rax, %rax
	je	.LBB60_391
# BB#380:                               # %if.then85.i
	movq	184(%rax), %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movq	88(%rbx), %rbx
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$29, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB60_391
# BB#381:                               # %if.then85.i
	testq	%rax, %rax
	je	.LBB60_391
# BB#382:                               # %if.then.i.i234.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB60_391:                             # %if.end146
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB60_397
# BB#392:                               # %if.then148
	movq	decl_scope_table(%rip), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB60_378
# BB#393:                               # %pop_decl_scope.exit
	decq	%rcx
	movq	%rcx, 8(%rax)
	jmp	.LBB60_397
.LBB60_45:                              # %sw.bb41
	movq	8(%rbx), %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	gen_type_die
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_type_die, %rcx
	movq	$11117, %rsi            # imm = 0x2B6D
	jmp	.LBB60_16
.LBB60_63:                              # %if.end.i.i.i522
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
	jmp	.LBB60_16
.LBB60_247:
	movq	%r13, 40(%rsp)          # 8-byte Spill
	jmp	.LBB60_261
.LBB60_399:                             # %sw.default.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_byte_size_attribute, %rcx
	movq	$9320, %rsi             # imm = 0x2468
	jmp	.LBB60_16
.LBB60_15:                              # %if.then3.i.i.i249
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB60_16
.LBB60_58:                              # %if.end.i.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB60_16
.LBB60_330:                             # %if.then8.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.splice_child_die, %rcx
	movq	$5055, %rsi             # imm = 0x13BF
	jmp	.LBB60_16
.LBB60_378:                             # %if.then.i220.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.pop_decl_scope, %rcx
	movq	$9553, %rsi             # imm = 0x2551
.LBB60_16:                              # %if.then3.i.i.i249
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp667:
	.size	gen_type_die, .Ltmp667-gen_type_die
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI60_0:
	.quad	.LBB60_396
	.quad	.LBB60_394
	.quad	.LBB60_394
	.quad	.LBB60_394
	.quad	.LBB60_394
	.quad	.LBB60_396
	.quad	.LBB60_396
	.quad	.LBB60_396
	.quad	.LBB60_396
	.quad	.LBB60_103
	.quad	.LBB60_104
	.quad	.LBB60_396
	.quad	.LBB60_396
	.quad	.LBB60_395
	.quad	.LBB60_12
	.quad	.LBB60_395
	.quad	.LBB60_80
	.quad	.LBB60_45
	.quad	.LBB60_95
	.quad	.LBB60_32
	.quad	.LBB60_104
	.quad	.LBB60_104
	.quad	.LBB60_104
	.quad	.LBB60_46
	.quad	.LBB60_396

	.text
	.align	16, 0x90
	.type	gen_array_type_die,@function
gen_array_type_die:                     # @gen_array_type_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp674:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp675:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp676:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp677:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp678:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp679:
	.cfi_def_cfa_offset 80
.Ltmp680:
	.cfi_offset %rbx, -48
.Ltmp681:
	.cfi_offset %r12, -40
.Ltmp682:
	.cfi_offset %r13, -32
.Ltmp683:
	.cfi_offset %r14, -24
.Ltmp684:
	.cfi_offset %r15, -16
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
	callq	scope_die_for
	movq	%rax, %r14
	movq	$1, %rdi
	movq	$72, %rsi
	movq	$1, %rbx
	callq	xcalloc
	movq	%rax, %r13
	testq	%r14, %r14
	movl	%ebx, (%r13)
	je	.LBB61_4
# BB#1:                                 # %if.then.i
	testq	%r13, %r13
	je	.LBB61_5
# BB#2:                                 # %if.then.i.i
	cmpq	%r14, %r13
	je	.LBB61_62
# BB#3:                                 # %if.end.i.i
	movq	%r14, 24(%r13)
	movq	32(%r14), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%r14)
	jmp	.LBB61_5
.LBB61_4:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r13, (%rax)
	movq	%r15, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB61_5:                               # %new_die.exit
	movq	96(%r15), %rax
	testq	%rax, %rax
	je	.LBB61_19
# BB#6:                                 # %if.then.i33
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB61_10
# BB#7:                                 # %if.then.i33
	cmpq	$33, %rcx
	jne	.LBB61_19
# BB#8:                                 # %land.lhs.true.i
	movzbl	49(%rax), %ecx
	testq	$64, %rcx
	jne	.LBB61_19
# BB#9:                                 # %if.end25.i
	movq	72(%rax), %rax
	testq	%rax, %rax
	je	.LBB61_19
.LBB61_10:                              # %if.end29.i
	movq	32(%rax), %r14
	testq	%r14, %r14
	je	.LBB61_19
# BB#11:                                # %lor.lhs.false.i
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB61_19
# BB#12:                                # %if.then.i38
	movq	demangle_name_func(%rip), %rcx
	movq	%r13, %rbx
	testq	%rcx, %rcx
	je	.LBB61_14
# BB#13:                                # %if.then3.i
	movq	%r14, %rdi
	callq	*%rcx
	movq	%rax, %r14
.LBB61_14:                              # %if.end.i
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r13
	movq	%rax, %r12
	testq	%r13, %r13
	jne	.LBB61_16
# BB#15:                                # %if.then.i.i40
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r13
.LBB61_16:                              # %if.end.i.i41
	movq	%r14, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%r12)
	movq	$3, %rcx
	movl	%ecx, (%r12)
	movq	$14, %rcx
	movl	%ecx, 16(%r12)
	movq	%rax, 24(%r12)
	movq	%rbx, %r13
	testq	%r13, %r13
	je	.LBB61_19
# BB#17:                                # %if.end.i.i41
	testq	%r12, %r12
	je	.LBB61_19
# BB#18:                                # %if.then.i.i.i
	movq	16(%r13), %rax
	movq	%rax, 8(%r12)
	movq	%r12, 16(%r13)
.LBB61_19:                              # %add_name_attribute.exit
	movq	%r13, 88(%r15)
	movzbl	16(%r15), %eax
	cmpq	$9, %rax
	jne	.LBB61_23
# BB#20:                                # %if.then
	movq	24(%r15), %rax
	movq	24(%rax), %rax
	movq	8(%rax), %r15
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$8455, %rcx             # imm = 0x2107
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r13, %r13
	je	.LBB61_23
# BB#21:                                # %if.then
	testq	%rax, %rax
	je	.LBB61_23
# BB#22:                                # %if.then.i.i44
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB61_23:                              # %if.end
	movzbl	16(%r15), %eax
	cmpq	$18, %rax
	jne	.LBB61_49
# BB#24:                                # %for.body.i.lr.ph
	movq	%r15, %rbx
	.align	16, 0x90
.LBB61_25:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r14
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	testq	%r13, %r13
	movq	$33, %rax
	movl	%eax, (%r12)
	jne	.LBB61_26
# BB#29:                                # %if.else.i.i
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r12, (%rax)
	movq	$0, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	jmp	.LBB61_30
	.align	16, 0x90
.LBB61_26:                              # %if.then.i.i70
                                        #   in Loop: Header=BB61_25 Depth=1
	testq	%r12, %r12
	je	.LBB61_30
# BB#27:                                # %if.then.i.i.i71
                                        #   in Loop: Header=BB61_25 Depth=1
	cmpq	%r13, %r12
	je	.LBB61_62
# BB#28:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	32(%r13), %rax
	movq	%r13, 24(%r12)
	movq	%rax, 40(%r12)
	movq	%r12, 32(%r13)
.LBB61_30:                              # %new_die.exit.i
                                        #   in Loop: Header=BB61_25 Depth=1
	testq	%r14, %r14
	je	.LBB61_48
# BB#31:                                # %if.then.i75
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	8(%r14), %rdi
	movq	104(%r14), %r8
	movq	112(%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB61_46
# BB#32:                                # %if.then7.i
                                        #   in Loop: Header=BB61_25 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$6, %rax
	jne	.LBB61_36
# BB#33:                                # %land.lhs.true.i76
                                        #   in Loop: Header=BB61_25 Depth=1
	cmpq	$0, 96(%r14)
	jne	.LBB61_36
# BB#34:                                # %land.lhs.true15.i
                                        #   in Loop: Header=BB61_25 Depth=1
	movzbl	16(%rdi), %eax
	cmpq	$6, %rax
	jne	.LBB61_36
# BB#35:                                # %land.lhs.true23.i
                                        #   in Loop: Header=BB61_25 Depth=1
	cmpq	$0, 96(%rdi)
	je	.LBB61_46
	.align	16, 0x90
.LBB61_36:                              # %if.else.i77
                                        #   in Loop: Header=BB61_25 Depth=1
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB61_40
# BB#37:                                # %land.lhs.true.i84
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB61_40
# BB#38:                                # %land.lhs.true6.i86
                                        #   in Loop: Header=BB61_25 Depth=1
	cmpq	$0, 96(%rdi)
	jne	.LBB61_40
# BB#39:                                # %if.then.i90
                                        #   in Loop: Header=BB61_25 Depth=1
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
	.align	16, 0x90
.LBB61_40:                              # %if.end.i93
                                        #   in Loop: Header=BB61_25 Depth=1
	testq	%rax, %rax
	je	.LBB61_46
# BB#41:                                # %if.end.i93
                                        #   in Loop: Header=BB61_25 Depth=1
	cmpq	$5, %rax
	je	.LBB61_46
# BB#42:                                # %if.end19.i94
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r13, %rcx
	movq	%r8, %r14
	callq	modified_type_die
	movq	%r14, %r8
	testq	%rax, %rax
	je	.LBB61_46
# BB#43:                                # %if.then34.i118
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	$40, %rdi
	movq	%r8, %r14
	movq	%rax, 8(%rsp)           # 8-byte Spill
	callq	xmalloc
	movq	%r14, %r8
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r12, %r12
	je	.LBB61_46
# BB#44:                                # %if.then34.i118
                                        #   in Loop: Header=BB61_25 Depth=1
	testq	%rax, %rax
	je	.LBB61_46
# BB#45:                                # %if.then.i.i.i120
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
	.align	16, 0x90
.LBB61_46:                              # %if.end32.i
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	$34, %rsi
	movq	%r12, %rdi
	movq	%r8, %rdx
	callq	add_bound_info
	movq	24(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB61_48
# BB#47:                                # %if.then34.i78
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	$47, %rsi
	movq	%r12, %rdi
	callq	add_bound_info
.LBB61_48:                              # %for.inc.i
                                        #   in Loop: Header=BB61_25 Depth=1
	movq	8(%rbx), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$18, %rax
	je	.LBB61_25
.LBB61_49:
	movq	16(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB61_50:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %r15
	movzbl	16(%r15), %eax
	cmpq	$18, %rax
	je	.LBB61_50
# BB#51:                                # %while.end
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	gen_type_die
	movl	16(%r15), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB61_55
# BB#52:                                # %land.lhs.true.i47
	movq	8(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB61_55
# BB#53:                                # %land.lhs.true6.i
	cmpq	$0, 96(%r15)
	jne	.LBB61_55
# BB#54:                                # %if.then.i48
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %r15
.LBB61_55:                              # %if.end.i49
	testq	%rax, %rax
	je	.LBB61_61
# BB#56:                                # %if.end.i49
	cmpq	$5, %rax
	je	.LBB61_61
# BB#57:                                # %if.end19.i
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rcx
	callq	modified_type_die
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB61_61
# BB#58:                                # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r13, %r13
	je	.LBB61_61
# BB#59:                                # %if.then34.i
	testq	%rax, %rax
	je	.LBB61_61
# BB#60:                                # %if.then.i.i.i61
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB61_61:                              # %add_type_attribute.exit
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB61_62:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp685:
	.size	gen_array_type_die, .Ltmp685-gen_array_type_die
	.cfi_endproc

	.align	16, 0x90
	.type	scope_die_for,@function
scope_die_for:                          # @scope_die_for
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp687:
	.cfi_def_cfa_offset 16
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB62_1
# BB#3:                                 # %if.end
	movq	144(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB62_6
# BB#4:                                 # %land.lhs.true
	movl	16(%rdx), %r9d
	movq	%r9, %rcx
	andq	$255, %rcx
	cmpq	$23, %rcx
	je	.LBB62_6
# BB#5:                                 # %land.lhs.true
	cmpq	$38, %rcx
	jne	.LBB62_7
.LBB62_6:                               # %if.then22
	movq	comp_unit_die(%rip), %rsi
.LBB62_18:                              # %if.end58
	movq	%rsi, %rax
	popq	%rdx
	retq
.LBB62_7:                               # %if.else
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$116, %rcx
	jne	.LBB62_18
# BB#8:                                 # %if.then32
	movq	decl_scope_table(%rip), %rsi
	movq	8(%rsi), %rdi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	addq	%r8, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB62_13
# BB#9:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	.align	16, 0x90
.LBB62_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rsi,%rdi,8), %rax
	movq	$32, %rcx
	cmpq	%rdx, %rax
	je	.LBB62_12
# BB#10:                                # %for.cond
                                        #   in Loop: Header=BB62_11 Depth=1
	leaq	-1(%rdi), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rax, %rdi
	jg	.LBB62_11
	jmp	.LBB62_13
.LBB62_12:                              # %for.end
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	js	.LBB62_13
# BB#17:                                # %if.else54
	movq	88(%rdx), %rsi
	jmp	.LBB62_18
.LBB62_13:                              # %if.then44
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB62_16
# BB#14:                                # %if.then44
	andq	$16384, %r9             # imm = 0x4000
	testq	%r8, %r9
	je	.LBB62_15
.LBB62_16:                              # %if.end53
	movq	comp_unit_die(%rip), %rsi
	jmp	.LBB62_18
.LBB62_1:                               # %if.then
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.scope_die_for, %rcx
	movq	$9575, %rsi             # imm = 0x2567
	jmp	.LBB62_2
.LBB62_15:                              # %if.then52
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.scope_die_for, %rcx
	movq	$9604, %rsi             # imm = 0x2584
.LBB62_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp688:
	.size	scope_die_for, .Ltmp688-scope_die_for
	.cfi_endproc

	.align	16, 0x90
	.type	gen_inheritance_die,@function
gen_inheritance_die:                    # @gen_inheritance_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp693:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp694:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp695:
	.cfi_def_cfa_offset 32
.Ltmp696:
	.cfi_offset %rbx, -32
.Ltmp697:
	.cfi_offset %r14, -24
.Ltmp698:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	testq	%r15, %r15
	movq	$28, %rax
	movl	%eax, (%rbx)
	je	.LBB63_4
# BB#1:                                 # %if.then.i
	testq	%rbx, %rbx
	je	.LBB63_5
# BB#2:                                 # %if.then.i.i
	cmpq	%r15, %rbx
	je	.LBB63_27
# BB#3:                                 # %if.end.i.i
	movq	%r15, 24(%rbx)
	movq	32(%r15), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r15)
	jmp	.LBB63_5
.LBB63_4:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	%r14, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB63_5:                               # %new_die.exit
	movq	8(%r14), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB63_9
# BB#6:                                 # %land.lhs.true.i
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB63_9
# BB#7:                                 # %land.lhs.true6.i
	cmpq	$0, 96(%rdi)
	jne	.LBB63_9
# BB#8:                                 # %if.then.i55
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
.LBB63_9:                               # %if.end.i
	testq	%rax, %rax
	je	.LBB63_15
# BB#10:                                # %if.end.i
	cmpq	$5, %rax
	je	.LBB63_15
# BB#11:                                # %if.end19.i
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r15, %rcx
	callq	modified_type_die
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB63_15
# BB#12:                                # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%rbx, %rbx
	je	.LBB63_15
# BB#13:                                # %if.then34.i
	testq	%rax, %rax
	je	.LBB63_15
# BB#14:                                # %if.then.i.i.i
	movq	16(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rbx)
.LBB63_15:                              # %add_type_attribute.exit
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	add_data_member_location_attribute
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB63_19
# BB#16:                                # %if.then
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$76, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, 24(%rax)
	testq	%rbx, %rbx
	je	.LBB63_19
# BB#17:                                # %if.then
	testq	%rax, %rax
	je	.LBB63_19
# BB#18:                                # %if.then.i.i28
	movq	16(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rbx)
.LBB63_19:                              # %if.end
	movl	16(%r14), %eax
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB63_21
# BB#20:                                # %if.then7
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, 24(%rax)
	jmp	.LBB63_23
.LBB63_21:                              # %if.else
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB63_26
# BB#22:                                # %if.then13
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$2, 24(%rax)
.LBB63_23:                              # %if.then13
	testq	%rbx, %rbx
	je	.LBB63_26
# BB#24:                                # %if.then13
	testq	%rax, %rax
	je	.LBB63_26
# BB#25:                                # %if.then.i.i53
	movq	16(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%rbx)
.LBB63_26:                              # %if.end15
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB63_27:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp699:
	.size	gen_inheritance_die, .Ltmp699-gen_inheritance_die
	.cfi_endproc

	.align	16, 0x90
	.type	add_data_member_location_attribute,@function
add_data_member_location_attribute:     # @add_data_member_location_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp705:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp706:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp707:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp708:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp709:
	.cfi_def_cfa_offset 48
.Ltmp710:
	.cfi_offset %rbx, -40
.Ltmp711:
	.cfi_offset %r12, -32
.Ltmp712:
	.cfi_offset %r14, -24
.Ltmp713:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$0, (%rsp)
	movl	16(%rbx), %eax
	movq	%rdi, %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$3, %rcx
	jne	.LBB64_48
# BB#1:                                 # %if.then
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB64_14
# BB#2:                                 # %land.lhs.true
	movq	comp_unit_die(%rip), %rcx
.LBB64_3:                               # %tailrecurse.i.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB64_5 Depth 2
	testq	%rcx, %rcx
	je	.LBB64_14
# BB#4:                                 # %if.then.i.i.i
                                        #   in Loop: Header=BB64_3 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB64_14
	.align	16, 0x90
.LBB64_5:                               # %for.body.i.i.i
                                        #   Parent Loop BB64_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB64_8
# BB#6:                                 # %for.body.i.i.i
                                        #   in Loop: Header=BB64_5 Depth=2
	cmpq	$19, %rdx
	je	.LBB64_15
# BB#7:                                 # %for.body.i.i.i
                                        #   in Loop: Header=BB64_5 Depth=2
	cmpq	$71, %rdx
	jne	.LBB64_12
.LBB64_8:                               # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB64_5 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB64_9
# BB#11:                                # %AT_ref.exit.i.i.i
                                        #   in Loop: Header=BB64_5 Depth=2
	movq	24(%rax), %rcx
.LBB64_12:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB64_5 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB64_5
# BB#13:                                # %for.end.i.i.i
                                        #   in Loop: Header=BB64_3 Depth=1
	testq	%rcx, %rcx
	jne	.LBB64_3
	jmp	.LBB64_14
.LBB64_48:                              # %if.else20
	movq	%rbx, %rdi
	callq	field_byte_offset
	movq	%rax, %r15
	jmp	.LBB64_43
.LBB64_15:                              # %land.lhs.true.i.i.i
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB64_16
# BB#17:                                # %AT_unsigned.exit.i.i
	movl	24(%rax), %eax
	cmpq	$4, %rax
	jne	.LBB64_14
# BB#18:                                # %add_loc_descr.exit
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$18, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %r15
	movl	%r15d, 16(%rax)
	movq	$0, 24(%rax)
	movl	%r15d, 40(%rax)
	movq	$0, 48(%rax)
	movq	%rax, (%rsp)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r15d, 8(%rax)
	movl	%r15d, 16(%rax)
	movq	$0, 24(%rax)
	movl	%r15d, 40(%rax)
	movq	$0, 48(%rax)
	leaq	(%rsp), %rcx
	.align	16, 0x90
.LBB64_19:                              # %for.cond.i51
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB64_19
# BB#20:                                # %add_loc_descr.exit52
	movq	%rax, (%rdx)
	movq	72(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r15
	testq	%r15, %r15
	jns	.LBB64_21
# BB#22:                                # %if.end
	movq	%r15, %rbx
	negq	%rbx
	js	.LBB64_31
# BB#23:                                # %if.then.i
	cmpq	$31, %rbx
	jg	.LBB64_25
# BB#24:                                # %if.then2.i
	movq	$48, %r12
	subq	%r15, %r12
	jmp	.LBB64_34
.LBB64_14:                              # %if.else
	movq	40(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r15
.LBB64_43:                              # %if.then24
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$35, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r15, 24(%rbx)
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
	movq	%rbx, (%rsp)
.LBB64_44:                              # %if.end27
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$56, %rcx
	movl	%ecx, (%rax)
	movq	$2, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	testq	%r14, %r14
	je	.LBB64_47
# BB#45:                                # %if.end27
	testq	%rax, %rax
	je	.LBB64_47
# BB#46:                                # %if.then.i.i
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB64_47:                              # %add_AT_loc.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB64_31:                              # %if.else18.i
	movq	$9, %r12
	cmpq	$-129, %rbx
	jg	.LBB64_34
# BB#32:                                # %if.else22.i
	cmpq	$-32769, %rbx           # imm = 0xFFFFFFFFFFFF7FFF
	movq	$11, %r12
	jg	.LBB64_34
# BB#33:                                # %if.else22.i
	movq	$17, %r12
	jmp	.LBB64_34
.LBB64_25:                              # %if.else.i
	movq	$8, %r12
	cmpq	$256, %rbx              # imm = 0x100
	jl	.LBB64_34
# BB#26:                                # %if.else6.i
	movq	$10, %r12
	cmpq	$65536, %rbx            # imm = 0x10000
	jl	.LBB64_34
# BB#27:                                # %if.else10.i
	movabsq	$4294967296, %rax       # imm = 0x100000000
	cmpq	%rax, %rbx
	jge	.LBB64_28
# BB#29:                                # %if.else10.i
	movabsq	$1, %rax
	jmp	.LBB64_30
.LBB64_28:
	movabsq	$0, %rax
.LBB64_30:                              # %if.else10.i
	xorq	$1, %rax
	leaq	12(,%rax,4), %r12
.LBB64_34:                              # %int_loc_descriptor.exit
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r12d, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	(%rsp), %rcx
	.align	16, 0x90
.LBB64_35:                              # %for.cond.i61
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB64_35
# BB#36:                                # %add_loc_descr.exit62
	movq	%rax, (%rdx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$28, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	(%rsp), %rcx
	.align	16, 0x90
.LBB64_37:                              # %for.cond.i86
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB64_37
# BB#38:                                # %add_loc_descr.exit87
	movq	%rax, (%rdx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$6, %rcx
	movl	%ecx, 8(%rax)
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	(%rsp), %rcx
	.align	16, 0x90
.LBB64_39:                              # %for.cond.i101
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB64_39
# BB#40:                                # %add_loc_descr.exit102
	movq	%rax, (%rdx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$34, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	(%rsp), %rcx
	.align	16, 0x90
.LBB64_41:                              # %for.cond.i106
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB64_41
# BB#42:                                # %if.end22
	movq	%rax, (%rdx)
	movq	(%rsp), %rbx
	testq	%rbx, %rbx
	jne	.LBB64_44
	jmp	.LBB64_43
.LBB64_9:                               # %if.end.i.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB64_10
.LBB64_16:                              # %if.end.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
	jmp	.LBB64_10
.LBB64_21:                              # %if.then10
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_data_member_location_attribute, %rcx
	movq	$8599, %rsi             # imm = 0x2197
.LBB64_10:                              # %if.end.i.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp714:
	.size	add_data_member_location_attribute, .Ltmp714-add_data_member_location_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	field_byte_offset,@function
field_byte_offset:                      # @field_byte_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp721:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp722:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp723:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp724:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp725:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp726:
	.cfi_def_cfa_offset 64
.Ltmp727:
	.cfi_offset %rbx, -48
.Ltmp728:
	.cfi_offset %r12, -40
.Ltmp729:
	.cfi_offset %r13, -32
.Ltmp730:
	.cfi_offset %r14, -24
.Ltmp731:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movzbl	16(%r13), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB65_25
# BB#1:                                 # %entry
	cmpq	$37, %rax
	jne	.LBB65_26
# BB#2:                                 # %if.end.i
	movq	96(%r13), %r14
	testq	%r14, %r14
	jne	.LBB65_4
# BB#3:                                 # %if.then3.i
	movq	8(%r13), %r14
.LBB65_4:                               # %field_type.exit
	movq	40(%r13), %r15
	testq	%r15, %r15
	jne	.LBB65_6
# BB#5:
	movq	global_trees+136(%rip), %r15
.LBB65_6:                               # %field_type.exit
	movq	%r13, %rdi
	callq	bit_position
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB65_25
# BB#7:                                 # %if.end15
	movq	%r13, %rdi
	callq	int_bit_position
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB65_9
# BB#8:                                 # %if.then19
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	tree_low_cst
	movq	%rax, %r15
	jmp	.LBB65_10
.LBB65_9:                               # %if.else21
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %r15       # imm = 0xFFFFFFE0
	andq	%rax, %r15
.LBB65_10:                              # %if.end23
	movzbl	16(%r14), %eax
	testq	%rax, %rax
	je	.LBB65_11
# BB#12:                                # %if.else.i
	movq	32(%r14), %rdi
	xorq	%r12, %r12
	testq	%rdi, %rdi
	je	.LBB65_16
# BB#13:                                # %if.else5.i
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB65_15
# BB#14:                                # %if.then9.i
	movq	32(%r14), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r12
	jmp	.LBB65_16
.LBB65_11:                              # %if.then.i81
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %r12       # imm = 0xFFFFFFE0
	andq	%rax, %r12
	jmp	.LBB65_16
.LBB65_15:                              # %if.else13.i
	movl	64(%r14), %r12d
.LBB65_16:                              # %simple_type_size_in_bits.exit
	movzbl	16(%r14), %eax
	testq	%rax, %rax
	je	.LBB65_18
# BB#17:                                # %cond.true.i
	movl	64(%r14), %ebx
	jmp	.LBB65_19
.LBB65_18:                              # %cond.false.i
	movl	target_flags(%rip), %ebx
	movq	$20, %rcx
	shrq	%cl, %rbx
	andq	$32, %rbx
	addq	$32, %rbx
.LBB65_19:                              # %simple_type_align_in_bits.exit
	movzbl	16(%r13), %eax
	testq	%rax, %rax
	je	.LBB65_20
# BB#21:                                # %if.end.i74
	movl	56(%r13), %esi
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	movq	%r13, %rdi
	callq	x86_field_alignment
	movq	%rax, %rsi
	jmp	.LBB65_22
.LBB65_20:                              # %if.then.i73
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
.LBB65_22:                              # %simple_field_decl_align_in_bits.exit
	movq	8(%rsp), %r8            # 8-byte Reload
	addq	%r8, %r15
	subq	%r12, %r15
	leaq	-1(%rbx), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	addq	%r15, %rdi
	andq	%rcx, %rbx
	movq	%rdi, %rax
	cqto
	idivq	%rbx
	subq	%rdx, %rdi
	cmpq	%r8, %rdi
	jle	.LBB65_24
# BB#23:                                # %if.then34
	leaq	-1(%rsi), %rdi
	andq	%rcx, %rdi
	addq	%r15, %rdi
	andq	%rcx, %rsi
	movq	%rdi, %rax
	cqto
	idivq	%rsi
	subq	%rdx, %rdi
.LBB65_24:                              # %if.end43
	movq	$63, %rcx
	movq	%rdi, %rbx
	sarq	%cl, %rbx
	movq	$61, %rcx
	shrq	%cl, %rbx
	addq	%rdi, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
.LBB65_25:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB65_26:                              # %if.then6
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.field_byte_offset, %rcx
	movq	$8430, %rsi             # imm = 0x20EE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp732:
	.size	field_byte_offset, .Ltmp732-field_byte_offset
	.cfi_endproc

	.align	16, 0x90
	.type	add_bound_info,@function
add_bound_info:                         # @add_bound_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp739:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp740:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp741:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp742:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp743:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp744:
	.cfi_def_cfa_offset 64
.Ltmp745:
	.cfi_offset %rbx, -48
.Ltmp746:
	.cfi_offset %r12, -40
.Ltmp747:
	.cfi_offset %r13, -32
.Ltmp748:
	.cfi_offset %r14, -24
.Ltmp749:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB66_1
	.align	16, 0x90
.LBB66_62:                              # %sw.bb17
                                        #   in Loop: Header=BB66_1 Depth=1
	movq	32(%rbx), %rbx
.LBB66_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$113, %rax
	jle	.LBB66_2
# BB#10:                                # %tailrecurse
                                        #   in Loop: Header=BB66_1 Depth=1
	leaq	-114(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$4, %rcx
	jb	.LBB66_62
# BB#11:                                # %tailrecurse
	cmpq	$118, %rax
	jne	.LBB66_89
# BB#12:                                # %sw.bb18
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.LBB66_120
# BB#13:                                # %land.lhs.true23
	movl	optimize(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB66_15
# BB#14:                                # %lor.lhs.false25
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB66_120
.LBB66_15:                              # %if.then32
	movq	current_function_decl(%rip), %rax
	movl	36(%rax), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB66_16
# BB#63:                                # %lookup_decl_die.exit
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	testq	%r13, %r13
	movq	$52, %rax
	movl	%eax, (%r12)
	je	.LBB66_17
# BB#64:                                # %if.then.i
	testq	%r12, %r12
	je	.LBB66_68
# BB#65:                                # %if.then.i.i201
	cmpq	%r13, %r12
	je	.LBB66_66
# BB#67:                                # %if.end.i.i
	movq	%r13, 24(%r12)
	movq	32(%r13), %rax
	movq	%rax, 40(%r12)
	movq	%r12, 32(%r13)
.LBB66_68:                              # %new_die.exit
	movq	%r13, 8(%rsp)           # 8-byte Spill
	jmp	.LBB66_69
.LBB66_2:                               # %tailrecurse
	leaq	-34(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jae	.LBB66_3
# BB#86:                                # %sw.bb56
	movl	36(%rbx), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB66_120
# BB#87:                                # %lookup_decl_die.exit230
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
	testq	%rbx, %rbx
	je	.LBB66_120
# BB#88:                                # %if.then60
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movl	%r14d, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	jmp	.LBB66_116
.LBB66_3:                               # %tailrecurse
	testq	%rax, %rax
	je	.LBB66_120
# BB#4:                                 # %tailrecurse
	cmpq	$25, %rax
	jne	.LBB66_89
# BB#5:                                 # %sw.bb1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB66_120
# BB#6:                                 # %lor.lhs.false
	movq	%r14, %rax
	andq	%r12, %rax
	cmpq	$34, %rax
	jne	.LBB66_61
# BB#7:                                 # %land.lhs.true
	movq	comp_unit_die(%rip), %rax
	movq	%rax, %rdx
.LBB66_8:                               # %tailrecurse.i.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_20 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_9
# BB#18:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB66_8 Depth=1
	movq	16(%rdx), %rcx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB66_19
	.align	16, 0x90
.LBB66_20:                              # %for.body.i.i.i
                                        #   Parent Loop BB66_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %esi
	cmpq	$49, %rsi
	je	.LBB66_23
# BB#21:                                # %for.body.i.i.i
                                        #   in Loop: Header=BB66_20 Depth=2
	cmpq	$19, %rsi
	je	.LBB66_30
# BB#22:                                # %for.body.i.i.i
                                        #   in Loop: Header=BB66_20 Depth=2
	cmpq	$71, %rsi
	jne	.LBB66_27
.LBB66_23:                              # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB66_20 Depth=2
	movl	16(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB66_24
# BB#26:                                # %AT_ref.exit.i.i.i
                                        #   in Loop: Header=BB66_20 Depth=2
	movq	24(%rcx), %rdx
.LBB66_27:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB66_20 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_20
# BB#28:                                # %for.end.i.i.i
                                        #   in Loop: Header=BB66_8 Depth=1
	testq	%rdx, %rdx
	jne	.LBB66_8
# BB#29:
	movq	%rax, %rdx
	jmp	.LBB66_34
.LBB66_89:                              # %sw.default
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	loc_descriptor_from_tree
	testq	%rax, %rax
	je	.LBB66_120
# BB#90:                                # %if.end68
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	current_function_decl(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB66_91
# BB#92:                                # %if.else71
	movl	36(%rcx), %eax
	movl	decl_die_table_in_use(%rip), %edx
	cmpq	%rax, %rdx
	jbe	.LBB66_98
# BB#93:                                # %cond.true.i248
	movq	decl_die_table(%rip), %rdx
	leaq	(%rdx,%rax,8), %rax
	jmp	.LBB66_94
.LBB66_30:                              # %land.lhs.true.i.i.i
	movl	16(%rcx), %edx
	cmpq	$6, %rdx
	jne	.LBB66_31
# BB#32:                                # %is_c_family.exit
	movq	24(%rcx), %rcx
	andq	%r12, %rcx
	cmpq	$4, %rcx
	movq	%rax, %rdx
	ja	.LBB66_34
# BB#33:                                # %is_c_family.exit
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$22, %rdx
	movq	%rax, %rdx
	jne	.LBB66_46
	jmp	.LBB66_34
.LBB66_91:
	movabsq	$comp_unit_die, %rax
.LBB66_94:                              # %if.end73
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB66_98
# BB#95:                                # %if.then.i255
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	testq	%r13, %r13
	movq	$52, %rax
	movl	%eax, (%r13)
	je	.LBB66_99
# BB#96:                                # %if.then.i.i257
	cmpq	%r12, %r13
	je	.LBB66_66
# BB#97:                                # %if.end.i.i262
	movq	%r12, 24(%r13)
	movq	32(%r12), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%r12)
	jmp	.LBB66_99
.LBB66_98:                              # %if.else.i267
	movq	8(%rbx), %rsi
	movq	$118, %rdi
	xorq	%r8, %r8
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	movq	$52, %rax
	movl	%eax, (%r13)
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r13, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB66_99:                              # %new_die.exit268
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r13, %r13
	je	.LBB66_102
# BB#100:                               # %new_die.exit268
	testq	%rax, %rax
	je	.LBB66_102
# BB#101:                               # %if.then.i.i279
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB66_102:                             # %add_AT_flag.exit280
	movq	8(%rbx), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB66_106
# BB#103:                               # %land.lhs.true.i
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB66_106
# BB#104:                               # %land.lhs.true6.i
	cmpq	$0, 96(%rdi)
	jne	.LBB66_106
# BB#105:                               # %if.then.i306
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
.LBB66_106:                             # %if.end.i
	testq	%rax, %rax
	je	.LBB66_112
# BB#107:                               # %if.end.i
	cmpq	$5, %rax
	je	.LBB66_112
# BB#108:                               # %if.end19.i
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$1, %rsi
	movq	%r12, %rcx
	callq	modified_type_die
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB66_112
# BB#109:                               # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r13, %r13
	movq	$0, 8(%rax)
	movq	$73, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB66_112
# BB#110:                               # %if.then34.i
	testq	%rax, %rax
	je	.LBB66_112
# BB#111:                               # %if.then.i.i.i312
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB66_112:                             # %add_type_attribute.exit
	movq	$40, %rdi
	callq	xmalloc
	testq	%r13, %r13
	movq	$0, 8(%rax)
	movq	$2, %rcx
	movl	%ecx, (%rax)
	movl	%ecx, 16(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 24(%rax)
	je	.LBB66_115
# BB#113:                               # %add_type_attribute.exit
	testq	%rax, %rax
	je	.LBB66_115
# BB#114:                               # %if.then.i.i291
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB66_115:                             # %add_AT_loc.exit
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movl	%r14d, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, 24(%rax)
.LBB66_116:                             # %add_AT_loc.exit
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	jmp	.LBB66_117
.LBB66_9:
	movq	%rax, %rdx
	jmp	.LBB66_34
.LBB66_19:
	movq	%rax, %rdx
.LBB66_34:                              # %tailrecurse.i.i.i128
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_37 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_48
# BB#35:                                # %if.then.i.i.i132
                                        #   in Loop: Header=BB66_34 Depth=1
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	je	.LBB66_48
# BB#36:                                #   in Loop: Header=BB66_34 Depth=1
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB66_37:                              # %for.body.i.i.i136
                                        #   Parent Loop BB66_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %esi
	cmpq	$49, %rsi
	je	.LBB66_40
# BB#38:                                # %for.body.i.i.i136
                                        #   in Loop: Header=BB66_37 Depth=2
	cmpq	$19, %rsi
	je	.LBB66_44
# BB#39:                                # %for.body.i.i.i136
                                        #   in Loop: Header=BB66_37 Depth=2
	cmpq	$71, %rsi
	jne	.LBB66_42
.LBB66_40:                              # %land.lhs.true.i.i.i.i140
                                        #   in Loop: Header=BB66_37 Depth=2
	movl	16(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB66_24
# BB#41:                                # %AT_ref.exit.i.i.i144
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	24(%rcx), %rdx
.LBB66_42:                              # %for.inc.i.i.i149
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_37
# BB#43:                                # %for.end.i.i.i151
                                        #   in Loop: Header=BB66_34 Depth=1
	testq	%rdx, %rdx
	jne	.LBB66_34
	jmp	.LBB66_48
.LBB66_44:                              # %land.lhs.true.i.i.i155
	movl	16(%rcx), %edx
	cmpq	$6, %rdx
	jne	.LBB66_31
# BB#45:                                # %AT_unsigned.exit.i.i159
	movl	24(%rcx), %ecx
	cmpq	$11, %rcx
	jne	.LBB66_48
.LBB66_46:                              # %land.lhs.true7
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r12, %rax
	jne	.LBB66_120
# BB#47:                                # %land.lhs.true7.lor.lhs.false10_crit_edge
	movq	comp_unit_die(%rip), %rax
.LBB66_48:                              # %tailrecurse.i.i.i163
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_51 Depth 2
	testq	%rax, %rax
	je	.LBB66_61
# BB#49:                                # %if.then.i.i.i167
                                        #   in Loop: Header=BB66_48 Depth=1
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB66_61
# BB#50:                                #   in Loop: Header=BB66_48 Depth=1
	xorq	%rax, %rax
	.align	16, 0x90
.LBB66_51:                              # %for.body.i.i.i171
                                        #   Parent Loop BB66_48 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB66_54
# BB#52:                                # %for.body.i.i.i171
                                        #   in Loop: Header=BB66_51 Depth=2
	cmpq	$19, %rdx
	je	.LBB66_58
# BB#53:                                # %for.body.i.i.i171
                                        #   in Loop: Header=BB66_51 Depth=2
	cmpq	$71, %rdx
	jne	.LBB66_56
.LBB66_54:                              # %land.lhs.true.i.i.i.i175
                                        #   in Loop: Header=BB66_51 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB66_24
# BB#55:                                # %AT_ref.exit.i.i.i179
                                        #   in Loop: Header=BB66_51 Depth=2
	movq	24(%rcx), %rax
.LBB66_56:                              # %for.inc.i.i.i184
                                        #   in Loop: Header=BB66_51 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_51
# BB#57:                                # %for.end.i.i.i186
                                        #   in Loop: Header=BB66_48 Depth=1
	testq	%rax, %rax
	jne	.LBB66_48
	jmp	.LBB66_61
.LBB66_58:                              # %land.lhs.true.i.i.i190
	movl	16(%rcx), %eax
	cmpq	$6, %rax
	jne	.LBB66_31
# BB#59:                                # %AT_unsigned.exit.i.i195
	movl	24(%rcx), %eax
	addq	$-7, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB66_61
# BB#60:                                # %land.lhs.true13
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r12, %rax
	jne	.LBB66_120
.LBB66_61:                              # %if.else
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	movq	%rax, %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movl	%r14d, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
.LBB66_117:                             # %add_AT_loc.exit
	testq	%r15, %r15
	je	.LBB66_120
# BB#118:                               # %add_AT_loc.exit
	testq	%rax, %rax
	je	.LBB66_120
# BB#119:                               # %if.then.i.i304
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB66_120:                             # %sw.epilog
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB66_16:                              # %lookup_decl_die.exit.thread
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	movq	$52, %rax
	movl	%eax, (%r12)
.LBB66_17:                              # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	%r12, (%rax)
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
.LBB66_69:                              # %new_die.exit
	movq	48(%rbx), %r13
	movzbl	(%r13), %eax
	cmpq	$66, %rax
	jne	.LBB66_72
# BB#70:                                # %if.then41
	movq	8(%r13), %rdi
	movq	%rbx, %rsi
	callq	fix_lexical_addr
	movq	8(%r13), %rcx
	cmpq	%rax, %rcx
	je	.LBB66_72
# BB#71:                                # %if.then48
	movzbl	2(%r13), %edi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r13
.LBB66_72:                              # %if.end53
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r12, %r12
	je	.LBB66_75
# BB#73:                                # %if.end53
	testq	%rax, %rax
	je	.LBB66_75
# BB#74:                                # %if.then.i.i212
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
.LBB66_75:                              # %add_AT_flag.exit
	movq	8(%rbx), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB66_79
# BB#76:                                # %land.lhs.true.i318
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB66_79
# BB#77:                                # %land.lhs.true6.i320
	cmpq	$0, 96(%rdi)
	jne	.LBB66_79
# BB#78:                                # %if.then.i324
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
.LBB66_79:                              # %if.end.i327
	testq	%rax, %rax
	je	.LBB66_85
# BB#80:                                # %if.end.i327
	cmpq	$5, %rax
	je	.LBB66_85
# BB#81:                                # %if.end19.i328
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$1, %rsi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	modified_type_die
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB66_85
# BB#82:                                # %if.then34.i348
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r12, %r12
	movq	$0, 8(%rax)
	movq	$73, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB66_85
# BB#83:                                # %if.then34.i348
	testq	%rax, %rax
	je	.LBB66_85
# BB#84:                                # %if.then.i.i.i350
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
.LBB66_85:                              # %add_type_attribute.exit352
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	add_AT_location_description
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movl	%r14d, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r12, 24(%rax)
	jmp	.LBB66_116
.LBB66_66:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB66_25
.LBB66_24:                              # %if.end.i.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB66_25
.LBB66_31:                              # %if.end.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
.LBB66_25:                              # %if.end.i.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp750:
	.size	add_bound_info, .Ltmp750-add_bound_info
	.cfi_endproc

	.align	16, 0x90
	.type	add_AT_location_description,@function
add_AT_location_description:            # @add_AT_location_description
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp755:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp756:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp757:
	.cfi_def_cfa_offset 32
.Ltmp758:
	.cfi_offset %rbx, -32
.Ltmp759:
	.cfi_offset %r14, -24
.Ltmp760:
	.cfi_offset %r15, -16
	movl	(%rsi), %eax
	movq	%rdi, %r14
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	addq	$-61, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$5, %rcx
	ja	.LBB67_12
# BB#1:                                 # %entry
	jmpq	*.LJTI67_0(,%rcx,8)
.LBB67_2:                               # %sw.bb.i
	movq	8(%rsi), %rsi
.LBB67_3:                               # %sw.bb1.i
	movl	8(%rsi), %eax
	cmpq	$52, %rax
	ja	.LBB67_19
# BB#4:                                 # %reg_number.exit.i.i
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB67_5
# BB#6:                                 # %reg_number.exit.i.i
	movabsq	$dbx64_register_map, %rcx
	jmp	.LBB67_7
.LBB67_11:                              # %sw.bb9.i
	movq	8(%rsi), %rdi
	movq	16(%rsi), %rsi
	callq	concat_loc_descriptor
	jmp	.LBB67_14
.LBB67_13:                              # %sw.bb2.i
	movq	8(%rsi), %rdi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rsi
	callq	mem_loc_descriptor
.LBB67_14:                              # %loc_descriptor.exit
	movq	%rax, %rbx
	jmp	.LBB67_15
.LBB67_5:
	movabsq	$svr4_dbx_register_map, %rcx
.LBB67_7:                               # %reg_number.exit.i.i
	movl	(%rcx,%rax,4), %r15d
	cmpq	$31, %r15
	ja	.LBB67_10
# BB#8:                                 # %if.then2.i.i
	addq	$80, %r15
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movl	%r15d, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	$0, 24(%rbx)
	jmp	.LBB67_9
.LBB67_10:                              # %if.else.i.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$144, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r15, 24(%rbx)
.LBB67_9:                               # %loc_descriptor.exit
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
.LBB67_15:                              # %loc_descriptor.exit
	testq	%rbx, %rbx
	je	.LBB67_19
# BB#16:                                # %if.then
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$2, %rcx
	movl	%ecx, (%rax)
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	testq	%r14, %r14
	je	.LBB67_19
# BB#17:                                # %if.then
	testq	%rax, %rax
	je	.LBB67_19
# BB#18:                                # %if.then.i.i
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB67_19:                              # %if.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB67_12:                              # %sw.default.i
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.loc_descriptor, %rdx
	movq	$7976, %rsi             # imm = 0x1F28
	callq	fancy_abort
.Ltmp761:
	.size	add_AT_location_description, .Ltmp761-add_AT_location_description
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI67_0:
	.quad	.LBB67_3
	.quad	.LBB67_12
	.quad	.LBB67_2
	.quad	.LBB67_12
	.quad	.LBB67_11
	.quad	.LBB67_13

	.text
	.align	16, 0x90
	.type	loc_descriptor_from_tree,@function
loc_descriptor_from_tree:               # @loc_descriptor_from_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp768:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp769:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp770:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp771:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp772:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp773:
	.cfi_def_cfa_offset 112
.Ltmp774:
	.cfi_offset %rbx, -48
.Ltmp775:
	.cfi_offset %r12, -40
.Ltmp776:
	.cfi_offset %r13, -32
.Ltmp777:
	.cfi_offset %r14, -24
.Ltmp778:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	8(%r15), %rbx
	movl	16(%rbx), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 52(%rsp)
	movzbl	16(%r15), %ecx
	cmpq	$24, %rcx
	jg	.LBB68_3
# BB#1:                                 # %entry
	testq	%rcx, %rcx
	movabsq	$0, %r13
	jne	.LBB68_2
.LBB68_134:                             # %return
	movq	%r13, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB68_3:                               # %entry
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	addq	$-25, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$96, %rcx
	ja	.LBB68_2
# BB#4:                                 # %entry
	movq	$32, %r14
	movq	$26, %r12
	xorq	%r13, %r13
	jmpq	*.LJTI68_0(,%rcx,8)
.LBB68_21:                              # %sw.bb109
	movq	%rdx, %rbx
	leaq	16(%rsp), %rax
	leaq	32(%rsp), %rsi
	leaq	24(%rsp), %rdx
	leaq	40(%rsp), %rcx
	leaq	20(%rsp), %r8
	leaq	52(%rsp), %r9
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	callq	get_inner_reference
	cmpq	%r15, %rax
	je	.LBB68_22
# BB#23:                                # %if.end114
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	loc_descriptor_from_tree
	testq	%rax, %rax
	movq	%rax, 56(%rsp)
	je	.LBB68_24
# BB#25:                                # %lor.lhs.false117
	movq	24(%rsp), %rax
	testq	$7, %rax
	je	.LBB68_27
# BB#26:
	xorq	%r13, %r13
	jmp	.LBB68_134
	.align	16, 0x90
.LBB68_5:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15), %r15
	movzbl	16(%r15), %eax
	leaq	-114(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$5, %rcx
	jb	.LBB68_5
# BB#6:                                 # %for.cond
	cmpq	$41, %rax
	movabsq	$0, %r13
	jne	.LBB68_134
.LBB68_7:                               # %cond.true
	movq	32(%r15), %rdi
.LBB68_8:                               # %return
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	loc_descriptor_from_tree
	movq	%rax, %r13
	jmp	.LBB68_134
.LBB68_66:                              # %sw.bb151
	movq	$33, %r12
	jmp	.LBB68_90
.LBB68_2:                               # %sw.default
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.loc_descriptor_from_tree, %rdx
	movq	$8292, %rsi             # imm = 0x2064
	callq	fancy_abort
.LBB68_9:                               # %sw.bb45
	movq	%rdx, %rbx
	movq	%r15, %rdi
	callq	rtl_for_decl_location
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB68_10
# BB#11:                                # %if.else
	movl	(%r13), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB68_17
# BB#12:                                # %if.else
	leaq	-54(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	movq	8(%rsp), %r14           # 8-byte Reload
	ja	.LBB68_14
# BB#13:                                # %if.else
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB68_19
	jmp	.LBB68_14
.LBB68_65:                              # %sw.bb150
	movq	$39, %r12
	jmp	.LBB68_90
.LBB68_22:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_51:                              # %sw.bb141
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	host_integerp
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB68_134
# BB#52:                                # %if.then144
	movq	%rcx, %r13
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	tree_low_cst
	movq	%rax, %rbx
	testq	%rbx, %rbx
	js	.LBB68_61
# BB#53:                                # %if.then.i534
	cmpq	$31, %rbx
	jg	.LBB68_55
# BB#54:                                # %if.then2.i537
	leaq	48(%rbx), %r12
	movq	8(%rsp), %r14           # 8-byte Reload
	jmp	.LBB68_64
.LBB68_122:                             # %sw.bb94
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	movq	%rdx, %rbx
	callq	loc_descriptor_from_tree
	movq	%rbx, %rdx
	movq	%rax, 56(%rsp)
	movq	$1, %r12
	movq	8(%rsp), %r14           # 8-byte Reload
	jmp	.LBB68_123
.LBB68_20:                              # %sw.bb99
	movq	40(%r15), %rdi
	jmp	.LBB68_8
.LBB68_73:                              # %sw.bb160
	movq	%rdx, %rbx
	movq	40(%r15), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	movq	$34, %r12
	jne	.LBB68_74
# BB#75:                                # %land.lhs.true
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	callq	host_integerp
	movq	%rbx, %rdx
	testq	%rdx, %rax
	je	.LBB68_90
# BB#76:                                # %if.then174
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	movq	%rdx, %rbx
	callq	loc_descriptor_from_tree
	movq	%rbx, %rcx
	testq	%rax, %rax
	movq	%rax, 56(%rsp)
	je	.LBB68_134
# BB#77:                                # %if.end181
	movq	%rcx, %r13
	movq	40(%r15), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$35, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
.LBB68_78:                              # %for.cond.i585
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_78
# BB#79:                                # %add_loc_descr.exit586
	movq	%rax, (%rdx)
	xorq	%r12, %r12
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r13, %rdx
	jmp	.LBB68_119
.LBB68_68:                              # %sw.bb153
	movq	$28, %r12
	jmp	.LBB68_90
.LBB68_70:                              # %sw.bb155
	movq	$30, %r12
	jmp	.LBB68_90
.LBB68_67:                              # %sw.bb152
	movq	$27, %r12
	jmp	.LBB68_90
.LBB68_69:                              # %sw.bb154
	movq	$29, %r12
	jmp	.LBB68_90
.LBB68_99:                              # %sw.bb262
	movq	$31, %r14
	jmp	.LBB68_100
.LBB68_103:                             # %sw.bb271
	movq	integer_types+40(%rip), %rsi
	movq	32(%r15), %rdx
	movq	40(%r15), %rcx
	movq	$97, %rdi
	xorq	%rax, %rax
	callq	build
	movq	32(%r15), %r8
	movq	40(%r15), %rcx
	movq	%rax, %rdx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build
	movq	%rax, %r15
.LBB68_104:                             # %sw.bb288
	movq	40(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	callq	loc_descriptor_from_tree
	movq	48(%r15), %rdi
	movq	%rax, %r12
	xorq	%rsi, %rsi
	callq	loc_descriptor_from_tree
	movq	32(%r15), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	loc_descriptor_from_tree
	testq	%rax, %rax
	movq	%rax, 56(%rsp)
	je	.LBB68_134
# BB#105:                               # %sw.bb288
	testq	%r12, %r12
	je	.LBB68_134
# BB#106:                               # %sw.bb288
	testq	%rbx, %rbx
	je	.LBB68_134
# BB#107:                               # %if.end307
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	movq	$40, %rax
	movl	%eax, 8(%r13)
	movq	$6, %rax
	movl	%eax, 16(%r13)
	movq	$0, 24(%r13)
	movl	%eax, 40(%r13)
	movq	$0, 48(%r13)
	leaq	56(%rsp), %rax
	.align	16, 0x90
.LBB68_108:                             # %for.cond.i595
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB68_108
# BB#109:                               # %add_loc_descr.exit596
	movq	%r13, (%rcx)
	leaq	56(%rsp), %rax
	.align	16, 0x90
.LBB68_110:                             # %for.cond.i590
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB68_110
# BB#111:                               # %add_loc_descr.exit591
	movq	%rbx, (%rcx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$47, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	$0, 24(%rbx)
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
	leaq	56(%rsp), %rax
	movq	8(%rsp), %r14           # 8-byte Reload
	.align	16, 0x90
.LBB68_112:                             # %for.cond.i474
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB68_112
# BB#113:                               # %add_loc_descr.exit475
	movq	%rbx, (%rcx)
	leaq	56(%rsp), %rax
	.align	16, 0x90
.LBB68_114:                             # %for.cond.i469
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB68_114
# BB#115:                               # %add_loc_descr.exit470
	movq	%r12, (%rcx)
	movq	$2, %rax
	movl	%eax, 16(%r13)
	movq	%r12, 24(%r13)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$150, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
	.align	16, 0x90
.LBB68_116:                             # %for.cond.i454
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_116
# BB#117:                               # %add_loc_descr.exit455
	movq	%rax, (%rdx)
	movq	$2, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	xorq	%r12, %r12
.LBB68_118:                             # %sw.epilog
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB68_119
.LBB68_98:                              # %sw.bb261
	movq	$25, %r14
.LBB68_100:                             # %do_unop
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%rdx, %rbx
	callq	loc_descriptor_from_tree
	movq	%rbx, %rcx
	testq	%rax, %rax
	movq	%rax, 56(%rsp)
	je	.LBB68_134
# BB#101:                               # %if.end269
	movq	%rcx, %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r14d, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
	.align	16, 0x90
.LBB68_102:                             # %for.cond.i620
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_102
	jmp	.LBB68_97
.LBB68_71:                              # %sw.bb156
	movq	$36, %r12
	jmp	.LBB68_90
.LBB68_72:                              # %sw.bb157
	movq	$38, %r12
	subq	%rax, %r12
	jmp	.LBB68_90
.LBB68_84:                              # %sw.bb216
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	movq	$45, %r12
	testq	$32, %rax
	je	.LBB68_90
# BB#85:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_80:                              # %sw.bb188
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	movq	$44, %r12
	testq	$32, %rax
	je	.LBB68_90
# BB#81:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_86:                              # %sw.bb230
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	movq	$43, %r12
	testq	$32, %rax
	je	.LBB68_90
# BB#87:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_82:                              # %sw.bb202
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	movq	$42, %r12
	testq	$32, %rax
	je	.LBB68_90
# BB#83:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_88:                              # %sw.bb244
	movq	$41, %r12
	jmp	.LBB68_90
.LBB68_89:                              # %sw.bb245
	movq	$46, %r12
	jmp	.LBB68_90
.LBB68_24:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_10:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_17:                              # %if.else
	cmpq	$134, %rax
	movq	8(%rsp), %r14           # 8-byte Reload
	je	.LBB68_19
# BB#18:                                # %if.else
	cmpq	$140, %rax
	jne	.LBB68_14
.LBB68_19:                              # %if.then79
	movq	$1, %rdi
	movq	$72, %rsi
	movq	$1, %r12
	callq	xcalloc
	movq	$3, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	movq	%rax, 56(%rsp)
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, 24(%rax)
	movq	%rbx, %rdx
	jmp	.LBB68_123
.LBB68_27:                              # %lor.lhs.false119
	movzbl	32(%rsp), %ecx
	testq	$7, %rcx
	je	.LBB68_29
# BB#28:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_14:                              # %if.else82
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%r12, %r12
	cmpq	$66, %rax
	jne	.LBB68_16
# BB#15:                                # %if.then89
	movq	8(%r13), %r13
	movq	$1, %r12
.LBB68_16:                              # %if.end
	movq	%r13, %rdi
	callq	mem_loc_descriptor
	movq	%rax, 56(%rsp)
	movq	%rbx, %rdx
	jmp	.LBB68_119
.LBB68_74:
	movq	%rbx, %rdx
.LBB68_90:                              # %do_binop
	movq	%rdx, %rbx
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	callq	loc_descriptor_from_tree
	movq	%rax, 56(%rsp)
	movq	40(%r15), %rdi
	xorq	%rsi, %rsi
	callq	loc_descriptor_from_tree
	movq	56(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB68_134
# BB#91:                                # %do_binop
	testq	%rax, %rax
	je	.LBB68_134
# BB#92:
	leaq	56(%rsp), %rdx
	jmp	.LBB68_93
	.align	16, 0x90
.LBB68_94:                              # %for.cond.i600.for.cond.i600_crit_edge
                                        #   in Loop: Header=BB68_93 Depth=1
	movq	%rcx, %rdx
	movq	(%rcx), %rcx
.LBB68_93:                              # %for.cond.i600
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	jne	.LBB68_94
# BB#95:                                # %add_loc_descr.exit601
	movq	%rax, (%rdx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r12d, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
	.align	16, 0x90
.LBB68_96:                              # %for.cond.i615
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_96
.LBB68_97:                              # %add_loc_descr.exit616
	movq	%rax, (%rdx)
	xorq	%r12, %r12
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%rbx, %rdx
.LBB68_119:                             # %sw.epilog
	testq	%rdx, %r14
	je	.LBB68_123
# BB#120:                               # %sw.epilog
	testq	%r12, %r12
	jne	.LBB68_123
# BB#121:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_123:                             # %if.end324
	testq	%rdx, %r14
	jne	.LBB68_133
# BB#124:                               # %if.end324
	testq	%r12, %r12
	jle	.LBB68_133
# BB#125:                               # %if.then328
	movq	8(%r15), %rdi
	callq	int_size_in_bytes
	movl	target_flags(%rip), %edx
	movq	%rax, %rbx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rbx
	jg	.LBB68_126
# BB#127:                               # %if.then328
	cmpq	$-1, %rbx
	movabsq	$0, %r13
	je	.LBB68_134
# BB#128:                               # %if.else342
	cmpq	%rdx, %rbx
	movq	$6, %r14
	movq	$6, %r15
	je	.LBB68_130
# BB#129:                               # %if.else342
	movq	$148, %r15
.LBB68_130:                             # %if.else342
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r15d, 8(%rax)
	movl	%r14d, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%r14d, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
	.align	16, 0x90
.LBB68_131:                             # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_131
# BB#132:                               # %add_loc_descr.exit
	movq	%rax, (%rdx)
.LBB68_133:                             # %if.end355
	movq	56(%rsp), %r13
	jmp	.LBB68_134
.LBB68_126:
	xorq	%r13, %r13
	jmp	.LBB68_134
.LBB68_61:                              # %if.else18.i546
	movq	$9, %r12
	cmpq	$-129, %rbx
	movq	8(%rsp), %r14           # 8-byte Reload
	jg	.LBB68_64
# BB#62:                                # %if.else22.i549
	cmpq	$-32769, %rbx           # imm = 0xFFFFFFFFFFFF7FFF
	movq	$11, %r12
	jg	.LBB68_64
# BB#63:                                # %if.else22.i549
	movq	$17, %r12
	jmp	.LBB68_64
.LBB68_29:                              # %if.end123
	movq	40(%rsp), %rdi
	testq	%rdi, %rdi
	movq	8(%rsp), %r14           # 8-byte Reload
	je	.LBB68_35
# BB#30:                                # %if.then125
	xorq	%rsi, %rsi
	callq	loc_descriptor_from_tree
	leaq	56(%rsp), %rcx
.LBB68_31:                              # %for.cond.i479
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_31
# BB#32:                                # %add_loc_descr.exit480
	movq	%rax, (%rdx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$34, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
.LBB68_33:                              # %for.cond.i494
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_33
# BB#34:                                # %add_loc_descr.exit495
	movq	%rax, (%rdx)
	movq	24(%rsp), %rax
.LBB68_35:                              # %if.end128
	movq	%rbx, %rdx
	testq	%rdx, %r14
	jne	.LBB68_36
# BB#37:                                # %if.end128
	movabsq	$1, %r12
	jmp	.LBB68_38
.LBB68_55:                              # %if.else.i539
	movq	$8, %r12
	cmpq	$256, %rbx              # imm = 0x100
	movq	8(%rsp), %r14           # 8-byte Reload
	jl	.LBB68_64
# BB#56:                                # %if.else6.i541
	movq	$10, %r12
	cmpq	$65536, %rbx            # imm = 0x10000
	jl	.LBB68_64
# BB#57:                                # %if.else10.i544
	movabsq	$4294967296, %rax       # imm = 0x100000000
	cmpq	%rax, %rbx
	jge	.LBB68_58
# BB#59:                                # %if.else10.i544
	movabsq	$1, %rax
	jmp	.LBB68_60
.LBB68_36:
	movabsq	$0, %r12
.LBB68_38:                              # %if.end128
	movq	$63, %rcx
	movq	%rax, %rbx
	sarq	%cl, %rbx
	movq	$61, %rcx
	shrq	%cl, %rbx
	addq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	$8, %rax
	jl	.LBB68_42
# BB#39:                                # %if.then132
	movq	%rdx, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$35, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
.LBB68_40:                              # %for.cond.i509
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_40
# BB#41:                                # %add_loc_descr.exit510
	movq	%rax, (%rdx)
	movq	%r13, %rdx
	jmp	.LBB68_119
.LBB68_42:                              # %if.else134
	cmpq	$-8, %rax
	jg	.LBB68_119
# BB#43:                                # %if.else18.i
	movq	%r14, %r13
	movq	$9, %r14
	cmpq	$-1032, %rax            # imm = 0xFFFFFFFFFFFFFBF8
	jg	.LBB68_46
# BB#44:                                # %if.else22.i
	cmpq	$-262152, %rax          # imm = 0xFFFFFFFFFFFBFFF8
	movq	$11, %r14
	jg	.LBB68_46
# BB#45:                                # %if.else22.i
	movq	$17, %r14
.LBB68_46:                              # %int_loc_descriptor.exit
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r14d, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
.LBB68_47:                              # %for.cond.i515
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_47
# BB#48:                                # %add_loc_descr.exit516
	movq	%rax, (%rdx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$34, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	56(%rsp), %rcx
	movq	%r13, %r14
.LBB68_49:                              # %for.cond.i530
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB68_49
# BB#50:                                # %add_loc_descr.exit531
	movq	%rax, (%rdx)
	jmp	.LBB68_118
.LBB68_58:
	movabsq	$0, %rax
.LBB68_60:                              # %if.else10.i544
	xorq	$1, %rax
	leaq	12(,%rax,4), %r12
.LBB68_64:                              # %int_loc_descriptor.exit561
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r12d, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	movq	%rax, 56(%rsp)
	xorq	%r12, %r12
	movq	%r13, %rdx
	jmp	.LBB68_119
.Ltmp779:
	.size	loc_descriptor_from_tree, .Ltmp779-loc_descriptor_from_tree
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI68_0:
	.quad	.LBB68_51
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_9
	.quad	.LBB68_9
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_21
	.quad	.LBB68_21
	.quad	.LBB68_122
	.quad	.LBB68_2
	.quad	.LBB68_21
	.quad	.LBB68_21
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_20
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_104
	.quad	.LBB68_2
	.quad	.LBB68_134
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_134
	.quad	.LBB68_134
	.quad	.LBB68_73
	.quad	.LBB68_68
	.quad	.LBB68_70
	.quad	.LBB68_67
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_69
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_99
	.quad	.LBB68_2
	.quad	.LBB68_103
	.quad	.LBB68_98
	.quad	.LBB68_2
	.quad	.LBB68_71
	.quad	.LBB68_72
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_66
	.quad	.LBB68_65
	.quad	.LBB68_90
	.quad	.LBB68_2
	.quad	.LBB68_100
	.quad	.LBB68_90
	.quad	.LBB68_66
	.quad	.LBB68_90
	.quad	.LBB68_66
	.quad	.LBB68_65
	.quad	.LBB68_100
	.quad	.LBB68_84
	.quad	.LBB68_80
	.quad	.LBB68_86
	.quad	.LBB68_82
	.quad	.LBB68_88
	.quad	.LBB68_89
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_7
	.quad	.LBB68_7
	.quad	.LBB68_7
	.quad	.LBB68_7
	.quad	.LBB68_7
	.quad	.LBB68_2
	.quad	.LBB68_2
	.quad	.LBB68_5

	.text
	.align	16, 0x90
	.type	rtl_for_decl_location,@function
rtl_for_decl_location:                  # @rtl_for_decl_location
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp784:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp785:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp786:
	.cfi_def_cfa_offset 32
.Ltmp787:
	.cfi_offset %rbx, -32
.Ltmp788:
	.cfi_offset %r14, -24
.Ltmp789:
	.cfi_offset %r15, -16
	movl	reload_completed(%rip), %ecx
	movq	144(%rdi), %rax
	testq	%rcx, %rcx
	je	.LBB69_1
# BB#23:                                # %if.else
	movzbl	16(%rdi), %ecx
	cmpq	$35, %rcx
	jne	.LBB69_39
# BB#24:                                # %if.then104
	testq	%rax, %rax
	je	.LBB69_29
# BB#25:                                # %lor.lhs.false106
	movzbl	(%rax), %edx
	cmpq	$61, %rdx
	movq	%rax, %rcx
	je	.LBB69_28
# BB#26:                                # %lor.lhs.false106
	cmpq	$63, %rdx
	jne	.LBB69_40
# BB#27:                                # %land.rhs.i
	movq	8(%rax), %rcx
.LBB69_28:                              # %is_pseudo_reg.exit
	movl	8(%rcx), %ecx
	cmpq	$53, %rcx
	jb	.LBB69_39
.LBB69_29:                              # %if.then109
	movq	8(%rdi), %rcx
	movq	128(%rcx), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$18, %rcx
	jne	.LBB69_30
	.align	16, 0x90
.LBB69_31:                              # %while.cond.i509
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rcx
	movq	128(%rcx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB69_31
	jmp	.LBB69_32
.LBB69_1:                               # %if.then
	testq	%rax, %rax
	je	.LBB69_9
# BB#2:                                 # %land.lhs.true
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB69_20
# BB#3:                                 # %land.lhs.true
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB69_9
# BB#4:                                 # %land.lhs.true
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB69_22
# BB#5:                                 # %land.lhs.true
	cmpq	$12, %rcx
	jne	.LBB69_9
# BB#6:                                 # %land.lhs.true47
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB69_20
# BB#7:                                 # %land.lhs.true47
	addq	$-54, %rcx
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB69_9
# BB#8:                                 # %land.lhs.true47
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB69_22
	jmp	.LBB69_9
.LBB69_20:                              # %land.lhs.true47
	cmpq	$134, %rcx
	je	.LBB69_22
# BB#21:                                # %land.lhs.true47
	cmpq	$140, %rcx
	jne	.LBB69_9
	jmp	.LBB69_22
.LBB69_30:
	movq	%rdx, %rcx
.LBB69_32:                              # %type_main_variant.exit511
	movq	104(%rdi), %rdx
	movq	128(%rdx), %rsi
	movzbl	16(%rsi), %edx
	cmpq	$18, %rdx
	jne	.LBB69_33
	.align	16, 0x90
.LBB69_34:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	movq	128(%rdx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB69_34
	jmp	.LBB69_35
.LBB69_33:
	movq	%rsi, %rdx
.LBB69_35:                              # %type_main_variant.exit
	cmpq	%rdx, %rcx
	je	.LBB69_38
# BB#36:                                # %if.else117
	movzbl	16(%rcx), %esi
	cmpq	$6, %rsi
	jne	.LBB69_39
# BB#37:                                # %land.lhs.true123
	movl	60(%rcx), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	movl	60(%rdx), %edx
	andq	$127, %rsi
	movzbl	mode_size(%rsi), %esi
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rsi
	ja	.LBB69_39
.LBB69_38:                              # %if.then138
	movq	160(%rdi), %rax
.LBB69_39:                              # %if.end320
	testq	%rax, %rax
	je	.LBB69_9
.LBB69_40:                              # %if.then323
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	eliminate_regs
.LBB69_41:                              # %if.end437
	testq	%rax, %rax
	jne	.LBB69_22
	jmp	.LBB69_42
.LBB69_9:                               # %if.else325
	movzbl	16(%rdi), %eax
	cmpq	$34, %rax
	jne	.LBB69_42
# BB#10:                                # %land.lhs.true332
	movq	104(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB69_42
# BB#11:                                # %if.then336
	movzbl	16(%rbx), %eax
	cmpq	$29, %rax
	jne	.LBB69_43
# BB#12:                                # %land.lhs.true345
	movq	8(%rdi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$18, %rcx
	jne	.LBB69_42
# BB#13:                                # %if.then354
	movq	8(%rax), %rcx
	movl	60(%rcx), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB69_42
# BB#14:                                # %land.lhs.true372
	movzbl	mode_size(%rdx), %ecx
	cmpq	$1, %rcx
	jne	.LBB69_42
# BB#15:                                # %land.lhs.true372
	movq	24(%rax), %r15
	testq	%r15, %r15
	je	.LBB69_42
# BB#16:                                # %land.lhs.true380
	movq	104(%r15), %rdi
	callq	integer_zerop
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB69_42
# BB#17:                                # %land.lhs.true384
	movl	32(%rbx), %esi
	movq	112(%r15), %rdi
	decq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	compare_tree_int
	testq	%r14, %rax
	jne	.LBB69_42
# BB#18:                                # %land.lhs.true391
	movslq	32(%rbx), %r14
	movq	40(%rbx), %rbx
	movq	%rbx, %rdi
	callq	strlen
	incq	%rax
	cmpq	%rax, %r14
	jne	.LBB69_42
# BB#19:                                # %if.then399
	movq	$57, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_s
	jmp	.LBB69_41
.LBB69_43:                              # %if.else404
	addq	$-25, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB69_42
# BB#44:                                # %if.then422
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$3, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	testq	%rax, %rax
	je	.LBB69_42
# BB#45:                                # %land.lhs.true427
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	je	.LBB69_46
.LBB69_22:                              # %if.then100
	movq	%rax, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	i386_simplify_dwarf_addr  # TAILCALL
.LBB69_42:                              # %return
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB69_46:                              # %if.then432
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.rtl_for_decl_location, %rcx
	movq	$8957, %rsi             # imm = 0x22FD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp790:
	.size	rtl_for_decl_location, .Ltmp790-rtl_for_decl_location
	.cfi_endproc

	.align	16, 0x90
	.type	mem_loc_descriptor,@function
mem_loc_descriptor:                     # @mem_loc_descriptor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp795:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp796:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp797:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp798:
	.cfi_def_cfa_offset 48
.Ltmp799:
	.cfi_offset %rbx, -32
.Ltmp800:
	.cfi_offset %r14, -24
.Ltmp801:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	$0, 8(%rsp)
	callq	i386_simplify_dwarf_addr
	movq	%rax, %r15
	movl	(%r15), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$95, %rax
	jg	.LBB70_3
# BB#1:                                 # %entry
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$24, %rcx
	ja	.LBB70_90
# BB#2:                                 # %entry
	jmpq	*.LJTI70_0(,%rcx,8)
.LBB70_26:                              # %sw.bb17
	andq	$67174399, %rsi         # imm = 0x400FFFF
	cmpq	$67108932, %rsi         # imm = 0x4000044
	jne	.LBB70_27
# BB#28:                                # %if.then24
	leaq	7(%rsp), %rsi
	movq	%r15, %rdi
	callq	get_pool_constant_mark
	movq	%rax, %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$68, %rcx
	jne	.LBB70_31
# BB#29:                                # %if.then29
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB70_94
# BB#30:                                # %if.then34
	leaq	7(%rsp), %rsi
	movq	%r14, %rdi
	callq	get_pool_constant_mark
	movq	%r14, %r15
.LBB70_31:                              # %if.end37
	movzbl	7(%rsp), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%r15, %r14
	jne	.LBB70_32
	jmp	.LBB70_93
.LBB70_3:                               # %entry
	leaq	-96(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$5, %rcx
	ja	.LBB70_90
# BB#4:                                 # %entry
	movq	$1, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$44, %rsi
	je	.LBB70_5
.LBB70_8:                               # %sw.bb
	movq	8(%r15), %r15
.LBB70_9:                               # %sw.bb1
	movl	8(%r15), %ecx
	cmpq	$52, %rcx
	ja	.LBB70_92
# BB#10:                                # %reg_number.exit
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB70_11
# BB#12:                                # %reg_number.exit
	movabsq	$dbx64_register_map, %rax
	jmp	.LBB70_13
.LBB70_5:                               # %entry
	shlq	%cl, %rdx
	testq	$3, %rdx
	je	.LBB70_6
# BB#35:                                # %sw.bb56
	movq	8(%r15), %rbx
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_unit_size(%rdx), %esi
	cmpq	$97, %rax
	je	.LBB70_37
# BB#36:                                # %sw.bb56
	negq	%rsi
.LBB70_37:                              # %sw.bb56
	movl	word_mode(%rip), %r15d
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r15
	jmp	.LBB70_38
.LBB70_11:
	movabsq	$svr4_dbx_register_map, %rax
.LBB70_13:                              # %reg_number.exit
	movl	frame_pointer_needed(%rip), %edx
	movl	(%rax,%rcx,4), %ebx
	testq	%rdx, %rdx
	movq	$6, %rcx
	jne	.LBB70_15
# BB#14:                                # %select.mid
	movq	$7, %rcx
.LBB70_15:                              # %select.end
	movl	(%rax,%rcx,4), %eax
	cmpq	%rbx, %rax
	jne	.LBB70_19
# BB#16:                                # %if.then.i207
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$145, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB70_17
.LBB70_19:                              # %if.else.i
	cmpq	$31, %rbx
	ja	.LBB70_21
# BB#20:                                # %if.then8.i
	leaq	112(%rbx), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%ebx, 8(%rax)
.LBB70_17:                              # %sw.epilog
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	jmp	.LBB70_18
.LBB70_6:                               # %entry
	cmpq	$4, %rcx
	jne	.LBB70_90
# BB#7:                                 # %sw.bb52
	movq	16(%r15), %r15
.LBB70_38:                              # %plus
	movzbl	(%r15), %eax
	movq	8(%r15), %rdi
	cmpq	$75, %rax
	jne	.LBB70_54
# BB#39:                                # %land.rhs.i
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB70_54
# BB#40:                                # %land.lhs.true.i
	movl	8(%rdi), %eax
	cmpq	$52, %rax
	ja	.LBB70_54
# BB#41:                                # %is_based_loc.exit
	movq	16(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB70_54
# BB#42:                                # %if.then73
	movl	8(%rdi), %ecx
	cmpq	$53, %rcx
	jae	.LBB70_95
# BB#43:                                # %reg_number.exit249
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB70_44
# BB#45:                                # %reg_number.exit249
	movabsq	$dbx64_register_map, %rax
	jmp	.LBB70_46
.LBB70_54:                              # %if.else84
	movq	%r14, %rsi
	callq	mem_loc_descriptor
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 8(%rsp)
	je	.LBB70_92
# BB#55:                                # %if.end92
	movq	16(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$54, %rax
	jne	.LBB70_60
# BB#56:                                # %land.lhs.true100
	movq	8(%rdi), %r15
	testq	%r15, %r15
	js	.LBB70_60
# BB#57:                                # %if.then109
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$35, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	8(%rsp), %rcx
	jmp	.LBB70_58
	.align	16, 0x90
.LBB70_59:                              # %for.cond.i316.for.cond.i316_crit_edge
                                        #   in Loop: Header=BB70_58 Depth=1
	movq	%rbx, %rcx
	movq	(%rbx), %rbx
.LBB70_58:                              # %for.cond.i316
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB70_59
	jmp	.LBB70_91
.LBB70_60:                              # %if.else117
	movq	%r14, %rsi
	callq	mem_loc_descriptor
	leaq	8(%rsp), %rcx
	jmp	.LBB70_61
	.align	16, 0x90
.LBB70_62:                              # %for.cond.i331.for.cond.i331_crit_edge
                                        #   in Loop: Header=BB70_61 Depth=1
	movq	%rbx, %rcx
	movq	(%rbx), %rbx
.LBB70_61:                              # %for.cond.i331
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB70_62
# BB#63:                                # %add_loc_descr.exit332
	movq	%rax, (%rcx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$34, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	8(%rsp), %rcx
	.align	16, 0x90
.LBB70_64:                              # %for.cond.i346
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB70_64
# BB#65:                                # %add_loc_descr.exit347
	movq	%rax, (%rdx)
	jmp	.LBB70_92
.LBB70_27:
	movq	%r15, %r14
.LBB70_32:                              # %if.end41
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rcx
	movq	$3, %rax
	movl	%eax, 8(%rcx)
	movq	$6, %rax
	movl	%eax, 40(%rcx)
	movq	$0, 48(%rcx)
	movq	%rcx, 8(%rsp)
	xorq	%rdx, %rdx
	movq	used_rtx_varray(%rip), %rax
	movl	%edx, 16(%rcx)
	movq	%r14, 24(%rcx)
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB70_34
# BB#33:                                # %if.then45
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, used_rtx_varray(%rip)
	movq	8(%rax), %rcx
.LBB70_34:                              # %if.end48
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
	jmp	.LBB70_92
.LBB70_90:                              # %sw.default
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.mem_loc_descriptor, %rdx
	movq	$7904, %rsi             # imm = 0x1EE0
	callq	fancy_abort
.LBB70_74:                              # %sw.bb143
	movq	8(%r15), %rbx
	testq	%rbx, %rbx
	js	.LBB70_83
# BB#75:                                # %if.then.i209
	cmpq	$31, %rbx
	jg	.LBB70_77
# BB#76:                                # %if.then2.i
	leaq	48(%rbx), %r14
	jmp	.LBB70_86
.LBB70_22:                              # %sw.bb6
	movq	8(%r15), %rdi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	mem_loc_descriptor
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 8(%rsp)
	je	.LBB70_92
# BB#23:                                # %if.then14
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$6, %rcx
	movl	%ecx, 8(%rax)
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	8(%rsp), %rcx
	jmp	.LBB70_24
	.align	16, 0x90
.LBB70_25:                              # %for.cond.i.for.cond.i_crit_edge
                                        #   in Loop: Header=BB70_24 Depth=1
	movq	%rbx, %rcx
	movq	(%rbx), %rbx
.LBB70_24:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB70_25
.LBB70_91:                              # %add_loc_descr.exit
	movq	%rax, (%rcx)
	jmp	.LBB70_92
.LBB70_88:                              # %sw.bb148
	movq	8(%r15), %rcx
	movzbl	(%rcx), %edx
	xorq	%rax, %rax
	cmpq	$66, %rdx
	jne	.LBB70_93
# BB#89:                                # %if.then156
	movq	8(%rcx), %rdi
	movq	%r14, %rsi
	callq	mem_loc_descriptor
	jmp	.LBB70_93
.LBB70_66:                              # %sw.bb125
	movq	8(%r15), %rdi
	movq	%r14, %rsi
	callq	mem_loc_descriptor
	movq	16(%r15), %rdi
	movq	%rax, %rbx
	movq	%r14, %rsi
	callq	mem_loc_descriptor
	testq	%rbx, %rbx
	je	.LBB70_92
# BB#67:                                # %sw.bb125
	testq	%rax, %rax
	je	.LBB70_92
# BB#68:                                # %if.end141
	movq	%rbx, 8(%rsp)
	leaq	8(%rsp), %rcx
	jmp	.LBB70_69
	.align	16, 0x90
.LBB70_70:                              # %for.cond.i351.for.cond.i351_crit_edge
                                        #   in Loop: Header=BB70_69 Depth=1
	movq	%rbx, %rcx
	movq	(%rbx), %rbx
.LBB70_69:                              # %for.cond.i351
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB70_70
# BB#71:                                # %add_loc_descr.exit352
	movq	%rax, (%rcx)
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$30, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$0, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	8(%rsp), %rcx
	.align	16, 0x90
.LBB70_72:                              # %for.cond.i301
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB70_72
# BB#73:                                # %add_loc_descr.exit302
	movq	%rax, (%rdx)
	jmp	.LBB70_92
.LBB70_21:                              # %if.else10.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$146, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB70_87
.LBB70_83:                              # %if.else18.i
	movq	$9, %r14
	cmpq	$-129, %rbx
	jg	.LBB70_86
# BB#84:                                # %if.else22.i
	cmpq	$-32769, %rbx           # imm = 0xFFFFFFFFFFFF7FFF
	movq	$11, %r14
	jg	.LBB70_86
# BB#85:                                # %if.else22.i
	movq	$17, %r14
	jmp	.LBB70_86
.LBB70_77:                              # %if.else.i212
	movq	$8, %r14
	cmpq	$256, %rbx              # imm = 0x100
	jl	.LBB70_86
# BB#78:                                # %if.else6.i
	movq	$10, %r14
	cmpq	$65536, %rbx            # imm = 0x10000
	jl	.LBB70_86
# BB#79:                                # %if.else10.i214
	movabsq	$4294967296, %rax       # imm = 0x100000000
	cmpq	%rax, %rbx
	jge	.LBB70_80
# BB#81:                                # %if.else10.i214
	movabsq	$1, %rax
	jmp	.LBB70_82
.LBB70_94:                              # %if.end37.thread
	movq	$1, %rax
	movb	%al, 7(%rsp)
	jmp	.LBB70_32
.LBB70_44:
	movabsq	$svr4_dbx_register_map, %rax
.LBB70_46:                              # %reg_number.exit249
	movq	16(%r15), %rdx
	movl	(%rax,%rcx,4), %ebx
	movl	frame_pointer_needed(%rip), %ecx
	movq	8(%rdx), %r14
	testq	%rcx, %rcx
	movq	$6, %rcx
	jne	.LBB70_48
# BB#47:                                # %select.mid4
	movq	$7, %rcx
.LBB70_48:                              # %select.end3
	movl	(%rax,%rcx,4), %eax
	cmpq	%rbx, %rax
	jne	.LBB70_51
# BB#49:                                # %if.then.i268
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$145, %rcx
	movl	%ecx, 8(%rax)
	jmp	.LBB70_50
.LBB70_51:                              # %if.else.i270
	cmpq	$31, %rbx
	ja	.LBB70_53
# BB#52:                                # %if.then8.i282
	leaq	112(%rbx), %rbx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%ebx, 8(%rax)
.LBB70_50:                              # %sw.epilog
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	jmp	.LBB70_18
.LBB70_53:                              # %if.else10.i294
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$146, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	%r14, 48(%rax)
	movq	%rax, 8(%rsp)
	jmp	.LBB70_92
.LBB70_80:
	movabsq	$0, %rax
.LBB70_82:                              # %if.else10.i214
	xorq	$1, %rax
	leaq	12(,%rax,4), %r14
.LBB70_86:                              # %int_loc_descriptor.exit
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movl	%r14d, 8(%rax)
.LBB70_87:                              # %sw.epilog
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
.LBB70_18:                              # %sw.epilog
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	movq	%rax, 8(%rsp)
.LBB70_92:                              # %sw.epilog
	movq	8(%rsp), %rax
.LBB70_93:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB70_95:                              # %if.then.i242
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.reg_number, %rcx
	movq	$7617, %rsi             # imm = 0x1DC1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp802:
	.size	mem_loc_descriptor, .Ltmp802-mem_loc_descriptor
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI70_0:
	.quad	.LBB70_74
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_26
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_9
	.quad	.LBB70_90
	.quad	.LBB70_8
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_22
	.quad	.LBB70_26
	.quad	.LBB70_26
	.quad	.LBB70_90
	.quad	.LBB70_88
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_38
	.quad	.LBB70_90
	.quad	.LBB70_90
	.quad	.LBB70_66

	.text
	.align	16, 0x90
	.type	concat_loc_descriptor,@function
concat_loc_descriptor:                  # @concat_loc_descriptor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp809:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp810:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp811:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp812:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp813:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp814:
	.cfi_def_cfa_offset 64
.Ltmp815:
	.cfi_offset %rbx, -48
.Ltmp816:
	.cfi_offset %r12, -40
.Ltmp817:
	.cfi_offset %r13, -32
.Ltmp818:
	.cfi_offset %r14, -24
.Ltmp819:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	$0, 8(%rsp)
	movl	(%r15), %eax
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-61, %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$5, %rcx
	ja	.LBB71_26
# BB#1:                                 # %entry
	movq	%r15, %rdx
	jmpq	*.LJTI71_0(,%rcx,8)
.LBB71_2:                               # %sw.bb.i42
	movq	8(%r15), %rdx
.LBB71_3:                               # %sw.bb1.i45
	movl	8(%rdx), %eax
	xorq	%rbx, %rbx
	cmpq	$52, %rax
	ja	.LBB71_14
# BB#4:                                 # %reg_number.exit.i73
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB71_5
# BB#6:                                 # %reg_number.exit.i73
	movabsq	$dbx64_register_map, %rcx
	jmp	.LBB71_7
.LBB71_11:                              # %sw.bb9.i57
	movq	8(%r15), %rdi
	movq	16(%r15), %rsi
	callq	concat_loc_descriptor
	jmp	.LBB71_13
.LBB71_12:                              # %sw.bb2.i51
	movq	8(%r15), %rdi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rsi
	callq	mem_loc_descriptor
.LBB71_13:                              # %loc_descriptor.exit60
	movq	%rax, %rbx
	jmp	.LBB71_14
.LBB71_5:
	movabsq	$svr4_dbx_register_map, %rcx
.LBB71_7:                               # %reg_number.exit.i73
	movl	(%rcx,%rax,4), %r13d
	cmpq	$31, %r13
	ja	.LBB71_10
# BB#8:                                 # %if.then2.i85
	addq	$80, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movl	%r13d, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	$0, 24(%rbx)
	jmp	.LBB71_9
.LBB71_10:                              # %if.else.i97
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$144, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r13, 24(%rbx)
.LBB71_9:                               # %loc_descriptor.exit60
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
.LBB71_14:                              # %loc_descriptor.exit60
	movl	(%r14), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	addq	$-61, %rax
	andq	%r12, %rax
	cmpq	$5, %rax
	ja	.LBB71_26
# BB#15:                                # %loc_descriptor.exit60
	movq	%r14, %rcx
	jmpq	*.LJTI71_1(,%rax,8)
.LBB71_16:                              # %sw.bb.i
	movq	8(%r14), %rcx
.LBB71_17:                              # %sw.bb1.i
	movl	8(%rcx), %ecx
	xorq	%rax, %rax
	cmpq	$52, %rcx
	ja	.LBB71_39
# BB#18:                                # %reg_number.exit.i
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB71_19
# BB#20:                                # %reg_number.exit.i
	movabsq	$dbx64_register_map, %rax
	jmp	.LBB71_21
.LBB71_25:                              # %sw.bb9.i
	movq	8(%r14), %rdi
	movq	16(%r14), %rsi
	callq	concat_loc_descriptor
	jmp	.LBB71_28
.LBB71_27:                              # %sw.bb2.i
	movq	8(%r14), %rdi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	mem_loc_descriptor
.LBB71_28:                              # %loc_descriptor.exit
	movq	%rax, %r12
	jmp	.LBB71_29
.LBB71_26:                              # %sw.default.i58
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.loc_descriptor, %rdx
	movq	$7976, %rsi             # imm = 0x1F28
	callq	fancy_abort
.LBB71_19:
	movabsq	$svr4_dbx_register_map, %rax
.LBB71_21:                              # %reg_number.exit.i
	movl	(%rax,%rcx,4), %r13d
	cmpq	$31, %r13
	ja	.LBB71_24
# BB#22:                                # %if.then2.i
	addq	$80, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	movl	%r13d, 8(%r12)
	movq	$6, %rax
	movl	%eax, 16(%r12)
	movq	$0, 24(%r12)
	jmp	.LBB71_23
.LBB71_24:                              # %if.else.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	movq	$144, %rax
	movl	%eax, 8(%r12)
	movq	$6, %rax
	movl	%eax, 16(%r12)
	movq	%r13, 24(%r12)
.LBB71_23:                              # %loc_descriptor.exit
	movl	%eax, 40(%r12)
	movq	$0, 48(%r12)
.LBB71_29:                              # %loc_descriptor.exit
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB71_39
# BB#30:                                # %loc_descriptor.exit
	testq	%r12, %r12
	je	.LBB71_39
# BB#31:                                # %if.end
	movq	%rbx, 8(%rsp)
	movzbl	2(%r15), %eax
	movzbl	mode_size(%rax), %r15d
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$147, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	8(%rsp), %rcx
	jmp	.LBB71_32
	.align	16, 0x90
.LBB71_33:                              # %for.cond.i.for.cond.i_crit_edge
                                        #   in Loop: Header=BB71_32 Depth=1
	movq	%rbx, %rcx
	movq	(%rbx), %rbx
.LBB71_32:                              # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB71_33
# BB#34:                                # %add_loc_descr.exit
	movq	%rax, (%rcx)
	leaq	8(%rsp), %rax
	.align	16, 0x90
.LBB71_35:                              # %for.cond.i20
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB71_35
# BB#36:                                # %add_loc_descr.exit21
	movq	%r12, (%rcx)
	movzbl	2(%r14), %eax
	movzbl	mode_size(%rax), %ebx
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	$147, %rcx
	movl	%ecx, 8(%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 40(%rax)
	movq	$0, 48(%rax)
	leaq	8(%rsp), %rcx
	.align	16, 0x90
.LBB71_37:                              # %for.cond.i35
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB71_37
# BB#38:                                # %add_loc_descr.exit36
	movq	%rax, (%rdx)
	movq	8(%rsp), %rax
.LBB71_39:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp820:
	.size	concat_loc_descriptor, .Ltmp820-concat_loc_descriptor
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI71_0:
	.quad	.LBB71_3
	.quad	.LBB71_26
	.quad	.LBB71_2
	.quad	.LBB71_26
	.quad	.LBB71_11
	.quad	.LBB71_12
.LJTI71_1:
	.quad	.LBB71_17
	.quad	.LBB71_26
	.quad	.LBB71_16
	.quad	.LBB71_26
	.quad	.LBB71_25
	.quad	.LBB71_27

	.text
	.align	16, 0x90
	.type	gen_formal_types_die,@function
gen_formal_types_die:                   # @gen_formal_types_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp827:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp828:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp829:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp830:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp831:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp832:
	.cfi_def_cfa_offset 64
.Ltmp833:
	.cfi_offset %rbx, -48
.Ltmp834:
	.cfi_offset %r12, -40
.Ltmp835:
	.cfi_offset %r13, -32
.Ltmp836:
	.cfi_offset %r14, -24
.Ltmp837:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	16(%r12), %eax
	movq	%rsi, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	xorq	%rbx, %rbx
	cmpq	$30, %rax
	jne	.LBB72_1
# BB#2:                                 # %if.then
	movq	8(%r12), %rax
	movq	88(%r12), %r13
	movq	%rax, %r12
	jmp	.LBB72_3
.LBB72_1:
	xorq	%r13, %r13
.LBB72_3:                               # %if.end
	movq	24(%r12), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rax, %rax
	je	.LBB72_18
# BB#4:                                 # %for.body.lr.ph
	movq	(%rsp), %r14            # 8-byte Reload
	.align	16, 0x90
.LBB72_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rbx
	movq	global_trees+216(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB72_18
# BB#6:                                 # %if.end5
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_formal_parameter_die
	movzbl	16(%r12), %ecx
	movq	%r12, %r15
	movq	%rax, %r12
	cmpq	$16, %rcx
	jne	.LBB72_8
# BB#7:                                 # %if.end5
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB72_12
.LBB72_8:                               # %lor.lhs.false
                                        #   in Loop: Header=BB72_5 Depth=1
	testq	%r13, %r13
	jne	.LBB72_10
# BB#9:                                 # %if.end19.thread
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	(%r14), %r14
	xorq	%rax, %rax
	movq	%r15, %r12
	jmp	.LBB72_17
	.align	16, 0x90
.LBB72_10:                              # %land.lhs.true13
                                        #   in Loop: Header=BB72_5 Depth=1
	movzbl	50(%r13), %eax
	testq	$64, %rax
	jne	.LBB72_12
# BB#11:                                # %if.end19.thread67
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	(%r14), %r14
	movq	%r15, %r12
	jmp	.LBB72_16
	.align	16, 0x90
.LBB72_12:                              # %if.then18
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r12, %r12
	je	.LBB72_15
# BB#13:                                # %if.then18
                                        #   in Loop: Header=BB72_5 Depth=1
	testq	%rax, %rax
	je	.LBB72_15
# BB#14:                                # %if.then.i.i
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
.LBB72_15:                              # %if.end19
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	(%r14), %r14
	xorq	%rax, %rax
	testq	%r13, %r13
	movq	%r15, %r12
	je	.LBB72_17
.LBB72_16:                              # %if.then22
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	(%r13), %rax
.LBB72_17:                              # %for.cond.backedge
                                        #   in Loop: Header=BB72_5 Depth=1
	testq	%r14, %r14
	movq	%rax, %r13
	movq	8(%rsp), %r15           # 8-byte Reload
	jne	.LBB72_5
.LBB72_18:                              # %for.end
	movq	global_trees+216(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB72_24
# BB#19:                                # %if.then27
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$24, %rax
	movl	%eax, (%rbx)
	testq	%r15, %r15
	je	.LBB72_23
# BB#20:                                # %if.then.i.i64
	testq	%rbx, %rbx
	je	.LBB72_24
# BB#21:                                # %if.then.i.i.i
	cmpq	%r15, %rbx
	je	.LBB72_29
# BB#22:                                # %if.end.i.i.i
	movq	%r15, 24(%rbx)
	movq	32(%r15), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r15)
	jmp	.LBB72_24
.LBB72_23:                              # %if.else.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	%r12, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB72_24:                              # %if.end28
	movq	24(%r12), %rbx
	jmp	.LBB72_25
	.align	16, 0x90
.LBB72_27:                              # %for.body36
                                        #   in Loop: Header=BB72_25 Depth=1
	movq	%r15, %rsi
	callq	gen_type_die
	movq	(%rbx), %rbx
.LBB72_25:                              # %if.end28
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB72_28
# BB#26:                                # %land.rhs
                                        #   in Loop: Header=BB72_25 Depth=1
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB72_27
.LBB72_28:                              # %for.end41
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB72_29:                              # %if.then3.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp838:
	.size	gen_formal_types_die, .Ltmp838-gen_formal_types_die
	.cfi_endproc

	.align	16, 0x90
	.type	gen_formal_parameter_die,@function
gen_formal_parameter_die:               # @gen_formal_parameter_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp845:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp846:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp847:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp848:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp849:
	.cfi_def_cfa_offset 48
.Ltmp850:
	.cfi_offset %rbx, -48
.Ltmp851:
	.cfi_offset %r12, -40
.Ltmp852:
	.cfi_offset %r13, -32
.Ltmp853:
	.cfi_offset %r14, -24
.Ltmp854:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	testq	%r15, %r15
	movq	$5, %rax
	movl	%eax, (%r13)
	je	.LBB73_6
# BB#1:                                 # %if.then.i
	testq	%r13, %r13
	je	.LBB73_7
# BB#2:                                 # %if.then.i.i
	cmpq	%r15, %r13
	je	.LBB73_3
# BB#5:                                 # %if.end.i.i
	movq	%r15, 24(%r13)
	movq	32(%r15), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%r15)
	jmp	.LBB73_7
.LBB73_6:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r13, (%rax)
	movq	%r14, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB73_7:                               # %new_die.exit
	movl	16(%r14), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movsbq	tree_code_type(%rax), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$116, %rcx
	jne	.LBB73_8
# BB#53:                                # %sw.bb24
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB73_57
# BB#54:                                # %land.lhs.true.i
	movq	8(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB73_57
# BB#55:                                # %land.lhs.true6.i
	cmpq	$0, 96(%r14)
	jne	.LBB73_57
# BB#56:                                # %if.then.i54
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %r14
.LBB73_57:                              # %if.end.i55
	testq	%rax, %rax
	je	.LBB73_63
# BB#58:                                # %if.end.i55
	cmpq	$5, %rax
	je	.LBB73_63
# BB#59:                                # %if.end19.i
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r14, %rdi
	movq	%r15, %rcx
	callq	modified_type_die
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB73_63
# BB#60:                                # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r13, %r13
	je	.LBB73_63
# BB#61:                                # %if.then34.i
	testq	%rax, %rax
	je	.LBB73_63
# BB#62:                                # %if.then.i.i.i
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
	jmp	.LBB73_63
.LBB73_8:                               # %new_die.exit
	cmpq	$100, %rcx
	jne	.LBB73_64
# BB#9:                                 # %sw.bb
	movzbl	49(%r14), %eax
	movq	112(%r14), %rdx
	movq	$7, %rcx
	shrq	%cl, %rax
	cmpq	%r14, %rdx
	jne	.LBB73_10
# BB#11:                                # %sw.bb
	movabsq	$1, %rcx
	jmp	.LBB73_12
.LBB73_10:
	movabsq	$0, %rcx
.LBB73_12:                              # %sw.bb
	xorq	%rbx, %rbx
	testq	%rcx, %rax
	jne	.LBB73_14
# BB#13:                                # %sw.bb
	movq	%rdx, %rbx
.LBB73_14:                              # %sw.bb
	testq	%rbx, %rbx
	je	.LBB73_28
# BB#15:                                # %if.then
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	je	.LBB73_20
# BB#16:                                # %if.then.i107
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%rbx, %rdi
	jne	.LBB73_18
# BB#17:                                # %if.then7.i
	movq	(%rbx), %rdi
.LBB73_18:                              # %if.end.i110
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB73_20
# BB#19:                                # %if.then9.i111
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB73_20:                              # %if.end11.i
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB73_21
# BB#24:                                # %if.then32.i
	movq	88(%rbx), %rbx
	jmp	.LBB73_25
.LBB73_28:                              # %if.else
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	add_name_and_src_coords_attributes
	movq	8(%r14), %rdi
	movl	16(%rdi), %eax
	movl	16(%r14), %r8d
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rax, %rdx
	andq	$254, %rdx
	cmpq	$6, %rdx
	jne	.LBB73_32
# BB#29:                                # %land.lhs.true.i62
	movq	8(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB73_32
# BB#30:                                # %land.lhs.true6.i64
	cmpq	$0, 96(%rdi)
	jne	.LBB73_32
# BB#31:                                # %if.then.i68
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rdx, %rdi
.LBB73_32:                              # %if.end.i71
	testq	%rcx, %rcx
	je	.LBB73_42
# BB#33:                                # %if.end.i71
	cmpq	$5, %rcx
	je	.LBB73_42
# BB#34:                                # %if.end19.i73
	movq	%r8, %rcx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$2048, %r8              # imm = 0x800
	movq	$1, %rdx
	testq	%r12, %rcx
	movq	$1, %rsi
	jne	.LBB73_36
# BB#35:                                # %lor.rhs.i
	movq	%rax, %rsi
	andq	$4096, %rsi             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rsi
.LBB73_36:                              # %lor.end.i
	testq	%r12, %r8
	jne	.LBB73_38
# BB#37:                                # %lor.rhs25.i
	andq	$2048, %rax             # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %rax
	movq	%rax, %rdx
.LBB73_38:                              # %lor.end31.i
	movq	%r15, %rcx
	callq	modified_type_die
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB73_42
# BB#39:                                # %if.then34.i97
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r13, %r13
	je	.LBB73_42
# BB#40:                                # %if.then34.i97
	testq	%rax, %rax
	je	.LBB73_42
# BB#41:                                # %if.then.i.i.i99
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB73_42:                              # %add_type_attribute.exit101
	movzbl	50(%r14), %eax
	testq	$64, %rax
	je	.LBB73_47
# BB#43:                                # %if.then14
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	jmp	.LBB73_44
.LBB73_21:                              # %if.end11.i
	cmpq	$100, %rax
	jne	.LBB73_26
# BB#22:                                # %if.then21.i
	movl	36(%rbx), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB73_26
# BB#23:                                # %cond.true.i.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB73_25:                              # %if.end35.i
	testq	%rbx, %rbx
	je	.LBB73_26
# BB#27:                                # %add_abstract_origin_attribute.exit
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$49, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
.LBB73_44:                              # %if.then14
	testq	%r13, %r13
	je	.LBB73_47
# BB#45:                                # %if.then14
	testq	%rax, %rax
	je	.LBB73_47
# BB#46:                                # %if.then.i.i49
	movq	16(%r13), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r13)
.LBB73_47:                              # %if.end15
	movl	36(%r14), %r15d
	movl	decl_die_table_allocated(%rip), %eax
	cmpq	%r15, %rax
	ja	.LBB73_49
# BB#48:                                # %if.then.i52
	leaq	256(%r15), %rax
	movabsq	$4294967040, %rsi       # imm = 0xFFFFFF00
	andq	%rax, %rsi
	movq	%rax, %rbx
	movq	decl_die_table(%rip), %rdi
	andq	$-256, %rbx
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	decl_die_table_allocated(%rip), %ecx
	movq	%rax, decl_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	andq	%r12, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, decl_die_table_allocated(%rip)
.LBB73_49:                              # %if.end.i
	movl	decl_die_table_in_use(%rip), %eax
	cmpq	%r15, %rax
	ja	.LBB73_51
# BB#50:                                # %if.then9.i
	leaq	1(%r15), %rax
	movl	%eax, decl_die_table_in_use(%rip)
.LBB73_51:                              # %equate_decl_number_to_die.exit
	movq	decl_die_table(%rip), %rax
	movq	%r13, (%rax,%r15,8)
	movzbl	49(%r14), %eax
	testq	$128, %rax
	jne	.LBB73_63
# BB#52:                                # %if.then22
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	add_location_or_const_value_attribute
.LBB73_63:                              # %sw.epilog
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB73_3:                               # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB73_4
.LBB73_64:                              # %sw.default
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_formal_parameter_die, %rcx
	movq	$10025, %rsi            # imm = 0x2729
	jmp	.LBB73_4
.LBB73_26:                              # %if.then38.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_abstract_origin_attribute, %rcx
	movq	$9459, %rsi             # imm = 0x24F3
.LBB73_4:                               # %if.then3.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp855:
	.size	gen_formal_parameter_die, .Ltmp855-gen_formal_parameter_die
	.cfi_endproc

	.align	16, 0x90
	.type	add_name_and_src_coords_attributes,@function
add_name_and_src_coords_attributes:     # @add_name_and_src_coords_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp862:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp863:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp864:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp865:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp866:
	.cfi_def_cfa_offset 48
.Ltmp867:
	.cfi_offset %rbx, -48
.Ltmp868:
	.cfi_offset %r12, -40
.Ltmp869:
	.cfi_offset %r13, -32
.Ltmp870:
	.cfi_offset %r14, -24
.Ltmp871:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	72(%r15), %rax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB74_32
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 32(%rax)
	je	.LBB74_32
# BB#2:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB74_11
# BB#3:                                 # %land.lhs.true.i
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB74_11
# BB#4:                                 # %if.then.i
	movq	demangle_name_func(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB74_6
# BB#5:                                 # %if.then3.i
	movq	%rbx, %rdi
	callq	*%rcx
	movq	%rax, %rbx
.LBB74_6:                               # %if.end.i
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r13
	movq	%rax, %r12
	testq	%r13, %r13
	jne	.LBB74_8
# BB#7:                                 # %if.then.i.i
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r13
.LBB74_8:                               # %if.end.i.i
	movq	%rbx, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%r12)
	movq	$3, %rcx
	movl	%ecx, (%r12)
	movq	$14, %rcx
	movl	%ecx, 16(%r12)
	movq	%rax, 24(%r12)
	testq	%r14, %r14
	je	.LBB74_11
# BB#9:                                 # %if.end.i.i
	testq	%r12, %r12
	je	.LBB74_11
# BB#10:                                # %if.then.i.i.i
	movq	16(%r14), %rax
	movq	%rax, 8(%r12)
	movq	%r12, 16(%r14)
.LBB74_11:                              # %add_name_attribute.exit
	movzbl	50(%r15), %eax
	testq	$64, %rax
	jne	.LBB74_18
# BB#12:                                # %if.then4
	movq	24(%r15), %rdi
	callq	lookup_filename
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	movq	$6, %r12
	movl	%r12d, 16(%rax)
	movq	%rbx, 24(%rax)
	testq	%r14, %r14
	je	.LBB74_15
# BB#13:                                # %if.then4
	testq	%rax, %rax
	je	.LBB74_15
# BB#14:                                # %if.then.i.i.i77
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB74_15:                              # %add_AT_unsigned.exit.i
	movslq	32(%r15), %rbx
	movq	$40, %rdi
	callq	xmalloc
	testq	%r14, %r14
	movq	$0, 8(%rax)
	movq	$59, %rcx
	movl	%ecx, (%rax)
	movl	%r12d, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB74_18
# BB#16:                                # %add_AT_unsigned.exit.i
	testq	%rax, %rax
	je	.LBB74_18
# BB#17:                                # %if.then.i.i17.i
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB74_18:                              # %if.end
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB74_20
# BB#19:                                # %if.end
	cmpq	$34, %rcx
	jne	.LBB74_32
.LBB74_20:                              # %land.lhs.true13
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB74_32
# BB#21:                                # %land.lhs.true19
	movq	120(%r15), %rax
	testq	%rax, %rax
	jne	.LBB74_23
# BB#22:                                # %cond.false
	movq	%r15, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r15), %rax
.LBB74_23:                              # %cond.end
	movq	72(%r15), %rcx
	cmpq	%rcx, %rax
	je	.LBB74_32
# BB#24:                                # %land.lhs.true27
	movzbl	49(%r15), %ecx
	testq	$128, %rcx
	jne	.LBB74_32
# BB#25:                                # %if.then34
	testq	%rax, %rax
	jne	.LBB74_27
# BB#26:                                # %cond.false39
	movq	%r15, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r15), %rax
.LBB74_27:                              # %cond.end40
	movq	32(%rax), %r15
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r12
	movq	%rax, %rbx
	testq	%r12, %r12
	jne	.LBB74_29
# BB#28:                                # %if.then.i80
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r12
.LBB74_29:                              # %if.end.i82
	movq	%r15, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$8199, %rcx             # imm = 0x2007
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r14, %r14
	je	.LBB74_32
# BB#30:                                # %if.end.i82
	testq	%rbx, %rbx
	je	.LBB74_32
# BB#31:                                # %if.then.i.i83
	movq	16(%r14), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r14)
.LBB74_32:                              # %if.end47
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp872:
	.size	add_name_and_src_coords_attributes, .Ltmp872-add_name_and_src_coords_attributes
	.cfi_endproc

	.align	16, 0x90
	.type	add_location_or_const_value_attribute,@function
add_location_or_const_value_attribute:  # @add_location_or_const_value_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp879:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp880:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp881:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp882:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp883:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp884:
	.cfi_def_cfa_offset 112
.Ltmp885:
	.cfi_offset %rbx, -48
.Ltmp886:
	.cfi_offset %r12, -40
.Ltmp887:
	.cfi_offset %r13, -32
.Ltmp888:
	.cfi_offset %r14, -24
.Ltmp889:
	.cfi_offset %r15, -16
	movzbl	16(%rsi), %eax
	movq	%rdi, %r14
	leaq	-34(%rax), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jae	.LBB75_1
# BB#4:                                 # %if.end12
	movq	%rsi, %rdi
	callq	rtl_for_decl_location
	testq	%rax, %rax
	je	.LBB75_46
# BB#5:                                 # %if.end15
	movq	%rax, %rdi
	callq	avoid_constant_pool_reference
	movq	%rax, %r13
	movl	(%r13), %r15d
	movq	%r15, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-54(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$21, %rcx
	ja	.LBB75_9
# BB#6:                                 # %if.end15
	movq	$1, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$2121755, %rsi          # imm = 0x20601B
	je	.LBB75_7
# BB#10:                                # %sw.bb
	cmpq	$74, %rax
	jg	.LBB75_45
# BB#11:                                # %sw.bb
	cmpq	$14, %rcx
	ja	.LBB75_43
# BB#12:                                # %sw.bb
	jmpq	*.LJTI75_0(,%rcx,8)
.LBB75_37:                              # %sw.bb46.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$28, %rcx
	movl	%ecx, (%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, 24(%rax)
	testq	%r14, %r14
	je	.LBB75_40
# BB#38:                                # %sw.bb46.i
	testq	%rax, %rax
	je	.LBB75_40
# BB#39:                                # %if.then.i.i133.i
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB75_40:                              # %add_AT_addr.exit.i
	movq	used_rtx_varray(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB75_42
# BB#41:                                # %if.then50.i
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, used_rtx_varray(%rip)
	movq	8(%rax), %rcx
.LBB75_42:                              # %if.end54.i
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r13, 32(%rax,%rcx,8)
	jmp	.LBB75_46
.LBB75_1:                               # %entry
	testq	%rax, %rax
	je	.LBB75_46
# BB#2:                                 # %if.then11
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_location_or_const_value_attribute, %rcx
	movq	$8989, %rsi             # imm = 0x231D
	jmp	.LBB75_3
.LBB75_7:                               # %if.end15
	shlq	%cl, %rdx
	testq	$6784, %rdx             # imm = 0x1A80
	je	.LBB75_8
# BB#44:                                # %sw.bb19
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	add_AT_location_description
	jmp	.LBB75_46
.LBB75_45:                              # %sw.bb
	cmpq	$75, %rax
	je	.LBB75_46
.LBB75_43:                              # %sw.default58.i
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.add_const_value_attribute, %rdx
	movq	$8757, %rsi             # imm = 0x2235
	callq	fancy_abort
.LBB75_8:                               # %if.end15
	cmpq	$16, %rcx
	je	.LBB75_46
.LBB75_9:                               # %sw.default
	movabsq	$.L.str4, %rdi
	movabsq	$.L__FUNCTION__.add_location_or_const_value_attribute, %rdx
	movq	$9026, %rsi             # imm = 0x2342
	callq	fancy_abort
.LBB75_13:                              # %sw.bb.i
	movq	8(%r13), %rbx
	movq	$40, %rdi
	callq	xmalloc
	testq	%rbx, %rbx
	movq	$0, 8(%rax)
	movq	$28, %rcx
	movl	%ecx, (%rax)
	js	.LBB75_14
# BB#19:                                # %if.then3.i
	movq	$6, %rcx
	jmp	.LBB75_15
.LBB75_20:                              # %sw.bb6.i
	movq	$16, %rcx
	shrq	%cl, %r15
	andq	$255, %r15
	movl	mode_class(,%r15,4), %eax
	cmpq	$2, %rax
	jne	.LBB75_31
# BB#21:                                # %if.then11.i
	movzbl	mode_size(%r15), %r12d
	movq	$2, %rcx
	shrq	%cl, %r12
	movq	$3, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	32(%r13), %rcx
	movq	%rcx, 56(%rsp)
	movq	16(%r13), %rcx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%r13), %rax
	movq	%rax, 48(%rsp)
	movq	%rcx, 40(%rsp)
	leaq	-17(%r15), %rax
	andq	%rbx, %rax
	cmpq	$2, %rax
	jae	.LBB75_22
# BB#25:                                # %sw.bb22.i
	movzbl	target_flags+3(%rip), %eax
	testq	$1, %rax
	je	.LBB75_26
# BB#27:                                # %cond.true37.i
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	etartdouble
	jmp	.LBB75_30
.LBB75_32:                              # %sw.bb43.i
	movq	8(%r13), %r15
	movq	$40, %rdi
	callq	xmalloc
	movq	debug_str_hash(%rip), %r12
	movq	%rax, %rbx
	testq	%r12, %r12
	jne	.LBB75_34
# BB#33:                                # %if.then.i.i
	movq	$10, %rdi
	callq	ht_create
	movq	%rax, debug_str_hash(%rip)
	movq	$indirect_string_alloc, 96(%rax)
	movq	debug_str_hash(%rip), %r12
.LBB75_34:                              # %if.end.i.i
	movq	%r15, %rdi
	callq	strlen
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	movq	$0, 8(%rbx)
	movq	$28, %rcx
	movl	%ecx, (%rbx)
	movq	$14, %rcx
	movl	%ecx, 16(%rbx)
	movq	%rax, 24(%rbx)
	testq	%r14, %r14
	je	.LBB75_46
# BB#35:                                # %if.end.i.i
	testq	%rbx, %rbx
	je	.LBB75_46
# BB#36:                                # %if.then.i.i122.i
	movq	16(%r14), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r14)
	jmp	.LBB75_46
.LBB75_22:                              # %if.then11.i
	cmpq	$16, %r15
	jne	.LBB75_23
# BB#29:                                # %sw.bb21.i
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	etardouble
	jmp	.LBB75_30
.LBB75_14:                              # %if.then.i
	movq	$5, %rcx
.LBB75_15:                              # %if.then.i
	movl	%ecx, 16(%rax)
	jmp	.LBB75_16
.LBB75_23:                              # %if.then11.i
	cmpq	$15, %r15
	jne	.LBB75_28
# BB#24:                                # %sw.bb18.i
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	etarsingle
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rax, (%rbx)
	jmp	.LBB75_30
.LBB75_26:                              # %cond.true30.i
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	etarldouble
.LBB75_30:                              # %sw.epilog.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$28, %rcx
	movl	%ecx, (%rax)
	movq	$8, %rcx
	movl	%ecx, 16(%rax)
	movl	%r12d, 32(%rax)
.LBB75_16:                              # %if.then.i
	movq	%rbx, 24(%rax)
	testq	%r14, %r14
	je	.LBB75_46
# BB#17:                                # %if.then.i
	testq	%rax, %rax
	je	.LBB75_46
# BB#18:                                # %if.then.i.i.i
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB75_46:                              # %sw.epilog
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB75_31:                              # %if.else41.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_const_value_attribute, %rcx
	movq	$8722, %rsi             # imm = 0x2212
	jmp	.LBB75_3
.LBB75_28:                              # %sw.default.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_const_value_attribute, %rcx
	movq	$8713, %rsi             # imm = 0x2209
.LBB75_3:                               # %if.then11
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp890:
	.size	add_location_or_const_value_attribute, .Ltmp890-add_location_or_const_value_attribute
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI75_0:
	.quad	.LBB75_13
	.quad	.LBB75_20
	.quad	.LBB75_43
	.quad	.LBB75_32
	.quad	.LBB75_37
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_43
	.quad	.LBB75_37
	.quad	.LBB75_37

	.text
	.align	16, 0x90
	.type	mark_limbo_die_list,@function
mark_limbo_die_list:                    # @mark_limbo_die_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp895:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp896:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp897:
	.cfi_def_cfa_offset 32
.Ltmp898:
	.cfi_offset %rbx, -32
.Ltmp899:
	.cfi_offset %r14, -24
.Ltmp900:
	.cfi_offset %r15, -16
	movq	limbo_die_list(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB76_8
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB76_2:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %r14
	testq	%r14, %r14
	je	.LBB76_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB76_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB76_7
# BB#4:                                 # %do.body2
                                        #   in Loop: Header=BB76_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB76_6
# BB#5:                                 # %if.then4
                                        #   in Loop: Header=BB76_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB76_6:                               # %if.end
                                        #   in Loop: Header=BB76_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB76_7:                               # %for.inc
                                        #   in Loop: Header=BB76_2 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB76_2
.LBB76_8:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp901:
	.size	mark_limbo_die_list, .Ltmp901-mark_limbo_die_list
	.cfi_endproc

	.align	16, 0x90
	.type	gen_type_die_for_member,@function
gen_type_die_for_member:                # @gen_type_die_for_member
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp905:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp906:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp907:
	.cfi_def_cfa_offset 32
.Ltmp908:
	.cfi_offset %rbx, -24
.Ltmp909:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%rdx, %rsi
	callq	gen_type_die
	movq	(%rbx), %rax
	movzbl	49(%rax), %eax
	testq	$1, %rax
	je	.LBB77_18
# BB#1:                                 # %land.lhs.true
	movl	36(%r14), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB77_3
# BB#2:                                 # %lookup_decl_die.exit
	movq	decl_die_table(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	jne	.LBB77_18
.LBB77_3:                               # %if.then
	movzbl	49(%r14), %eax
	movq	112(%r14), %rdx
	movq	$7, %rcx
	shrq	%cl, %rax
	cmpq	%r14, %rdx
	jne	.LBB77_4
# BB#5:                                 # %if.then
	movabsq	$1, %rcx
	jmp	.LBB77_6
.LBB77_4:
	movabsq	$0, %rcx
.LBB77_6:                               # %if.then
	testq	%rcx, %rax
	jne	.LBB77_10
# BB#7:                                 # %if.then
	testq	%rdx, %rdx
	jne	.LBB77_8
.LBB77_10:                              # %if.end
	movq	decl_scope_table(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB77_12
# BB#11:                                # %if.then.i21
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, decl_scope_table(%rip)
	movq	8(%rax), %rcx
.LBB77_12:                              # %push_decl_scope.exit
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	movzbl	16(%r14), %eax
	movq	88(%rbx), %rsi
	movq	%r14, %rdi
	cmpq	$30, %rax
	jne	.LBB77_14
# BB#13:                                # %if.then8
	callq	gen_subprogram_die
	jmp	.LBB77_15
.LBB77_14:                              # %if.else
	callq	gen_variable_die
.LBB77_15:                              # %if.end11
	movq	decl_scope_table(%rip), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB77_16
# BB#17:                                # %pop_decl_scope.exit
	decq	%rcx
	movq	%rcx, 8(%rax)
.LBB77_18:                              # %if.end12
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB77_8:                               # %if.then4
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_type_die_for_member, %rcx
	movq	$10121, %rsi            # imm = 0x2789
	jmp	.LBB77_9
.LBB77_16:                              # %if.then.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.pop_decl_scope, %rcx
	movq	$9553, %rsi             # imm = 0x2551
.LBB77_9:                               # %if.then4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp910:
	.size	gen_type_die_for_member, .Ltmp910-gen_type_die_for_member
	.cfi_endproc

	.align	16, 0x90
	.type	gen_subprogram_die,@function
gen_subprogram_die:                     # @gen_subprogram_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp917:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp918:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp919:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp920:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp921:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp922:
	.cfi_def_cfa_offset 112
.Ltmp923:
	.cfi_offset %rbx, -48
.Ltmp924:
	.cfi_offset %r12, -40
.Ltmp925:
	.cfi_offset %r13, -32
.Ltmp926:
	.cfi_offset %r14, -24
.Ltmp927:
	.cfi_offset %r15, -16
	movzbl	49(%rdi), %eax
	movq	112(%rdi), %rdx
	movq	$7, %rcx
	shrq	%cl, %rax
	cmpq	%rdi, %rdx
	jne	.LBB78_1
# BB#2:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB78_3
.LBB78_1:
	movabsq	$0, %rcx
.LBB78_3:                               # %entry
	xorq	%rbx, %rbx
	testq	%rcx, %rax
	movabsq	$0, %r12
	jne	.LBB78_5
# BB#4:                                 # %entry
	movq	%rdx, %r12
.LBB78_5:                               # %entry
	movl	36(%rdi), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB78_7
# BB#6:                                 # %cond.true.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB78_7:                               # %lookup_decl_die.exit
	movq	current_function_decl(%rip), %rax
	cmpq	%rdi, %rax
	movq	$1, %r8
	jne	.LBB78_21
# BB#8:                                 # %lor.rhs
	testq	%rsi, %rsi
	je	.LBB78_9
# BB#18:                                # %land.rhs.i
	movl	(%rsi), %eax
	movabsq	$4294967291, %rcx       # imm = 0xFFFFFFFB
	andq	%rax, %rcx
	cmpq	$19, %rcx
	jne	.LBB78_19
# BB#20:                                # %land.rhs.i
	movabsq	$1, %r8
	jmp	.LBB78_21
.LBB78_9:
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$1, %r14
	xorq	%r8, %r8
	movq	%rbx, %rdx
	jmp	.LBB78_10
.LBB78_19:
	movabsq	$0, %r8
.LBB78_21:                              # %lor.end
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	movabsq	$0, %r14
	movabsq	$1, %rax
	movq	%r14, %rdx
	jne	.LBB78_23
# BB#22:                                # %lor.end
	movq	%rax, %rdx
.LBB78_23:                              # %lor.end
	movq	%r8, %rcx
	xorq	$1, %rcx
	orq	%rdx, %rcx
	testq	%rsi, %rsi
	jne	.LBB78_25
# BB#24:                                # %lor.end
	movq	%rax, %r14
.LBB78_25:                              # %lor.end
	movq	%rbx, %rdx
	je	.LBB78_10
# BB#26:                                # %lor.end
	testq	%rcx, %rcx
	jne	.LBB78_10
# BB#27:                                # %land.rhs.i834
	movl	(%rsi), %eax
	movabsq	$4294967291, %rcx       # imm = 0xFFFFFFFB
	andq	%rax, %rcx
	cmpq	$19, %rcx
	jne	.LBB78_10
# BB#28:                                # %if.then
	movq	%rsi, %r12
	movq	%r8, 16(%rsp)           # 8-byte Spill
	testq	%rdx, %rdx
	movq	%rdx, %r13
	je	.LBB78_71
# BB#29:                                # %if.then9
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_subprogram_die, %rcx
	movq	$10204, %rsi            # imm = 0x27DC
	jmp	.LBB78_30
.LBB78_10:                              # %if.end10
	testq	%r12, %r12
	je	.LBB78_69
# BB#11:                                # %if.then12
	testq	%r8, %r8
	je	.LBB78_31
# BB#12:                                # %land.lhs.true14
	testq	%r14, %r14
	jne	.LBB78_17
# BB#13:
	movq	%rsi, %rax
.LBB78_14:                              # %for.body.i838
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %ecx
	cmpq	$29, %rcx
	je	.LBB78_31
# BB#15:                                # %for.body.i838
                                        #   in Loop: Header=BB78_14 Depth=1
	cmpq	$46, %rcx
	je	.LBB78_31
# BB#16:                                # %for.inc.i
                                        #   in Loop: Header=BB78_14 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB78_14
.LBB78_17:                              # %if.then17
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_subprogram_die, %rcx
	movq	$10210, %rsi            # imm = 0x27E2
	jmp	.LBB78_30
.LBB78_31:                              # %if.end18
	testq	%rdx, %rdx
	je	.LBB78_37
# BB#32:                                # %land.lhs.true20
	cmpq	$0, 24(%rdx)
	jne	.LBB78_37
# BB#33:                                # %land.lhs.true20
	testq	%rsi, %rsi
	je	.LBB78_37
# BB#34:                                # %if.then.i844
	cmpq	%rsi, %rdx
	je	.LBB78_35
# BB#36:                                # %if.end.i846
	movq	%rsi, 24(%rdx)
	movq	32(%rsi), %rax
	movq	%rax, 40(%rdx)
	movq	%rdx, 32(%rsi)
.LBB78_37:                              # %if.end23
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r15
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$46, %rax
	movl	%eax, (%rbx)
	testq	%r14, %r14
	je	.LBB78_38
# BB#41:                                # %if.else.i863
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	%rbx, %r14
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	%r13, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	jmp	.LBB78_42
.LBB78_69:                              # %if.else
	testq	%rdx, %rdx
	je	.LBB78_70
# BB#75:                                # %if.then26
	movq	%rsi, %r12
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rdx, %rbx
	movq	24(%rax), %rdi
	callq	lookup_filename
	movq	%rbx, %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rcx
.LBB78_76:                              # %tailrecurse.i.i906
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_80 Depth 2
	testq	%rcx, %rcx
	je	.LBB78_77
# BB#78:                                # %if.then.i.i910
                                        #   in Loop: Header=BB78_76 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB78_79
	.align	16, 0x90
.LBB78_80:                              # %for.body.i.i914
                                        #   Parent Loop BB78_76 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB78_83
# BB#81:                                # %for.body.i.i914
                                        #   in Loop: Header=BB78_80 Depth=2
	cmpq	$60, %rdx
	je	.LBB78_89
# BB#82:                                # %for.body.i.i914
                                        #   in Loop: Header=BB78_80 Depth=2
	cmpq	$71, %rdx
	jne	.LBB78_86
.LBB78_83:                              # %land.lhs.true.i.i.i918
                                        #   in Loop: Header=BB78_80 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB78_84
# BB#85:                                # %AT_ref.exit.i.i922
                                        #   in Loop: Header=BB78_80 Depth=2
	movq	24(%rax), %rcx
.LBB78_86:                              # %for.inc.i.i927
                                        #   in Loop: Header=BB78_80 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB78_80
# BB#87:                                # %for.end.i.i929
                                        #   in Loop: Header=BB78_76 Depth=1
	testq	%rcx, %rcx
	jne	.LBB78_76
# BB#88:
	movq	%rdi, %rax
	jmp	.LBB78_92
.LBB78_38:                              # %if.then.i851
	testq	%rbx, %rbx
	movq	%rbx, %r14
	movq	24(%rsp), %r13          # 8-byte Reload
	je	.LBB78_42
# BB#39:                                # %if.then.i.i853
	cmpq	%r15, %r14
	je	.LBB78_35
# BB#40:                                # %if.end.i.i858
	movq	%r15, 24(%r14)
	movq	32(%r15), %rax
	movq	%rax, 40(%r14)
	movq	%r14, 32(%r15)
.LBB78_42:                              # %new_die.exit865
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	je	.LBB78_47
# BB#43:                                # %if.then.i1016
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%r12, %rdi
	jne	.LBB78_45
# BB#44:                                # %if.then7.i
	movq	(%r12), %rdi
.LBB78_45:                              # %if.end.i1019
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB78_47
# BB#46:                                # %if.then9.i1020
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB78_47:                              # %if.end11.i
	movzbl	16(%r12), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB78_48
# BB#51:                                # %if.then32.i
	movq	88(%r12), %rbx
	movq	%r14, %r15
	jmp	.LBB78_52
.LBB78_48:                              # %if.end11.i
	cmpq	$100, %rax
	movq	%r14, %r15
	jne	.LBB78_53
# BB#49:                                # %if.then21.i
	movl	36(%r12), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB78_53
# BB#50:                                # %cond.true.i.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB78_52:                              # %if.end35.i
	testq	%rbx, %rbx
	movq	%r13, %r14
	je	.LBB78_53
# BB#54:                                # %add_abstract_origin_attribute.exit
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$49, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB78_55
# BB#67:                                # %add_abstract_origin_attribute.exit
	testq	%rax, %rax
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	jne	.LBB78_68
	jmp	.LBB78_56
.LBB78_89:                              # %land.lhs.true.i.i932
	movl	16(%rax), %ecx
	cmpq	$9, %rcx
	jne	.LBB78_90
# BB#91:                                # %get_AT_flag.exit
	movzbl	24(%rax), %eax
	testq	%rax, %rax
	movq	%rdi, %rax
	jne	.LBB78_107
	jmp	.LBB78_92
.LBB78_55:
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	jmp	.LBB78_56
.LBB78_70:
	movq	%rdx, %r13
	movq	%rsi, %r12
	movq	%r8, 16(%rsp)           # 8-byte Spill
.LBB78_71:                              # %if.else70
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$46, %rax
	movl	%eax, (%r15)
	testq	%r14, %r14
	je	.LBB78_72
# BB#194:                               # %if.else.i675
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	jmp	.LBB78_195
.LBB78_72:                              # %if.then.i670
	testq	%r15, %r15
	movq	24(%rsp), %r14          # 8-byte Reload
	je	.LBB78_195
# BB#73:                                # %if.then.i.i672
	cmpq	%r12, %r15
	je	.LBB78_35
# BB#74:                                # %if.end.i.i673
	movq	%r12, 24(%r15)
	movq	32(%r12), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r12)
.LBB78_195:                             # %new_die.exit
	movzbl	18(%r14), %eax
	testq	$8, %rax
	je	.LBB78_199
# BB#196:                               # %if.then76
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$63, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r15, %r15
	je	.LBB78_199
# BB#197:                               # %if.then76
	testq	%rax, %rax
	je	.LBB78_199
# BB#198:                               # %if.then.i.i665
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_199:                             # %if.end77
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	add_name_and_src_coords_attributes
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB78_226
# BB#200:                               # %if.then80
	movq	8(%r14), %rax
	movq	comp_unit_die(%rip), %rdx
.LBB78_201:                             # %tailrecurse.i.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_203 Depth 2
	testq	%rdx, %rdx
	je	.LBB78_216
# BB#202:                               # %if.then.i.i.i638
                                        #   in Loop: Header=BB78_201 Depth=1
	movq	16(%rdx), %rcx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB78_216
	.align	16, 0x90
.LBB78_203:                             # %for.body.i.i.i
                                        #   Parent Loop BB78_201 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %esi
	cmpq	$49, %rsi
	je	.LBB78_206
# BB#204:                               # %for.body.i.i.i
                                        #   in Loop: Header=BB78_203 Depth=2
	cmpq	$19, %rsi
	je	.LBB78_210
# BB#205:                               # %for.body.i.i.i
                                        #   in Loop: Header=BB78_203 Depth=2
	cmpq	$71, %rsi
	jne	.LBB78_208
.LBB78_206:                             # %land.lhs.true.i.i.i.i
                                        #   in Loop: Header=BB78_203 Depth=2
	movl	16(%rcx), %edx
	cmpq	$10, %rdx
	jne	.LBB78_84
# BB#207:                               # %AT_ref.exit.i.i.i
                                        #   in Loop: Header=BB78_203 Depth=2
	movq	24(%rcx), %rdx
.LBB78_208:                             # %for.inc.i.i.i
                                        #   in Loop: Header=BB78_203 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB78_203
# BB#209:                               # %for.end.i.i.i
                                        #   in Loop: Header=BB78_201 Depth=1
	testq	%rdx, %rdx
	jne	.LBB78_201
	jmp	.LBB78_216
.LBB78_210:                             # %land.lhs.true.i.i.i642
	movl	16(%rcx), %edx
	cmpq	$6, %rdx
	jne	.LBB78_103
# BB#211:                               # %get_AT_unsigned.exit.i
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	jne	.LBB78_216
# BB#212:                               # %land.lhs.true.i
	cmpq	$0, 24(%rax)
	je	.LBB78_216
# BB#213:                               # %if.then.i653
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$39, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r15, %r15
	je	.LBB78_216
# BB#214:                               # %if.then.i653
	testq	%rax, %rax
	je	.LBB78_216
# BB#215:                               # %if.then.i.i5.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_216:                             # %add_prototyped_attribute.exit
	movq	8(%r14), %rax
	movq	8(%rax), %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rdx, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB78_220
# BB#217:                               # %land.lhs.true.i1027
	movq	8(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB78_220
# BB#218:                               # %land.lhs.true6.i
	cmpq	$0, 96(%rdi)
	jne	.LBB78_220
# BB#219:                               # %if.then.i1028
	movl	16(%rcx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	%rcx, %rdi
.LBB78_220:                             # %if.end.i1029
	testq	%rax, %rax
	je	.LBB78_226
# BB#221:                               # %if.end.i1029
	cmpq	$5, %rax
	je	.LBB78_226
# BB#222:                               # %if.end19.i
	movq	$12, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	$11, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r12, %rcx
	callq	modified_type_die
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB78_226
# BB#223:                               # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB78_226
# BB#224:                               # %if.then34.i
	testq	%rax, %rax
	je	.LBB78_226
# BB#225:                               # %if.then.i.i.i1043
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_226:                             # %if.end86
	cmpq	$0, 184(%r14)
	je	.LBB78_238
# BB#227:                               # %if.then.i623
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$76, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, 24(%rax)
	testq	%r15, %r15
	je	.LBB78_230
# BB#228:                               # %if.then.i623
	testq	%rax, %rax
	je	.LBB78_230
# BB#229:                               # %if.then.i.i.i624
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_230:                             # %add_AT_unsigned.exit.i
	movq	184(%r14), %rdi
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB78_234
# BB#231:                               # %if.then4.i
	movq	184(%r14), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$16, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r12, 24(%rbx)
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$77, %rcx
	movl	%ecx, (%rax)
	movq	$2, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB78_234
# BB#232:                               # %if.then4.i
	testq	%rax, %rax
	je	.LBB78_234
# BB#233:                               # %if.then.i.i33.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_234:                             # %if.end.i633
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB78_238
# BB#235:                               # %if.then9.i635
	movq	80(%r14), %rax
	movq	88(%rax), %rbx
	movq	$40, %rdi
	callq	xmalloc
	xorq	%rcx, %rcx
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$29, %rdx
	movl	%edx, (%rax)
	movq	$10, %rdx
	movl	%edx, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%ecx, 32(%rax)
	je	.LBB78_238
# BB#236:                               # %if.then9.i635
	testq	%rax, %rax
	je	.LBB78_238
# BB#237:                               # %if.then.i.i44.i
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_238:                             # %add_pure_or_virtual_attribute.exit
	movzbl	50(%r14), %eax
	testq	$64, %rax
	je	.LBB78_242
# BB#239:                               # %if.then94
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r15, %r15
	je	.LBB78_242
# BB#240:                               # %if.then94
	testq	%rax, %rax
	je	.LBB78_242
# BB#241:                               # %if.then.i.i612
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_242:                             # %if.end95
	movl	16(%r14), %eax
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB78_247
# BB#243:                               # %if.then101
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$2, 24(%rax)
	jne	.LBB78_245
	jmp	.LBB78_244
.LBB78_247:                             # %if.else102
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB78_244
# BB#248:                               # %if.then108
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$3, 24(%rax)
	je	.LBB78_244
.LBB78_245:                             # %if.then101
	testq	%rax, %rax
.LBB78_246:                             # %if.then101
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	%r13, %rsi
	je	.LBB78_56
.LBB78_68:                              # %if.then.i.i1057
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	jmp	.LBB78_56
.LBB78_77:
	movq	%rdi, %rax
	jmp	.LBB78_92
.LBB78_79:
	movq	%rdi, %rax
.LBB78_92:                              # %tailrecurse.i.i938
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_95 Depth 2
	testq	%rax, %rax
	je	.LBB78_105
# BB#93:                                # %if.then.i.i942
                                        #   in Loop: Header=BB78_92 Depth=1
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB78_105
# BB#94:                                #   in Loop: Header=BB78_92 Depth=1
	xorq	%rax, %rax
	.align	16, 0x90
.LBB78_95:                              # %for.body.i.i947
                                        #   Parent Loop BB78_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB78_98
# BB#96:                                # %for.body.i.i947
                                        #   in Loop: Header=BB78_95 Depth=2
	cmpq	$32, %rdx
	je	.LBB78_102
# BB#97:                                # %for.body.i.i947
                                        #   in Loop: Header=BB78_95 Depth=2
	cmpq	$71, %rdx
	jne	.LBB78_100
.LBB78_98:                              # %land.lhs.true.i.i.i952
                                        #   in Loop: Header=BB78_95 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB78_84
# BB#99:                                # %AT_ref.exit.i.i956
                                        #   in Loop: Header=BB78_95 Depth=2
	movq	24(%rcx), %rax
.LBB78_100:                             # %for.inc.i.i961
                                        #   in Loop: Header=BB78_95 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB78_95
# BB#101:                               # %for.end.i.i963
                                        #   in Loop: Header=BB78_92 Depth=1
	testq	%rax, %rax
	jne	.LBB78_92
	jmp	.LBB78_105
.LBB78_102:                             # %land.lhs.true.i.i967
	movl	16(%rcx), %eax
	cmpq	$6, %rax
	jne	.LBB78_103
# BB#104:                               # %get_AT_unsigned.exit973
	movl	24(%rcx), %eax
	testq	%rax, %rax
	je	.LBB78_105
.LBB78_107:                             # %if.end38
	movq	24(%rdi), %rax
	movq	comp_unit_die(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB78_108
# BB#109:                               # %if.end38
	movabsq	$1, %rax
	jmp	.LBB78_110
.LBB78_108:
	movabsq	$0, %rax
.LBB78_110:                             # %if.end38
	orq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB78_154
# BB#111:                               # %land.lhs.true42
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	50(%rax), %eax
	testq	$64, %rax
	jne	.LBB78_138
# BB#112:
	movq	%rdi, %rax
.LBB78_113:                             # %tailrecurse.i.i976
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_115 Depth 2
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB78_124
# BB#114:                               # %if.then.i.i980
                                        #   in Loop: Header=BB78_113 Depth=1
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB78_124
	.align	16, 0x90
.LBB78_115:                             # %for.body.i.i985
                                        #   Parent Loop BB78_113 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB78_118
# BB#116:                               # %for.body.i.i985
                                        #   in Loop: Header=BB78_115 Depth=2
	cmpq	$58, %rdx
	je	.LBB78_122
# BB#117:                               # %for.body.i.i985
                                        #   in Loop: Header=BB78_115 Depth=2
	cmpq	$71, %rdx
	jne	.LBB78_120
.LBB78_118:                             # %land.lhs.true.i.i.i990
                                        #   in Loop: Header=BB78_115 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB78_84
# BB#119:                               # %AT_ref.exit.i.i994
                                        #   in Loop: Header=BB78_115 Depth=2
	movq	24(%rcx), %rax
.LBB78_120:                             # %for.inc.i.i999
                                        #   in Loop: Header=BB78_115 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB78_115
# BB#121:                               # %for.end.i.i1001
                                        #   in Loop: Header=BB78_113 Depth=1
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jne	.LBB78_113
	jmp	.LBB78_124
.LBB78_105:                             # %if.then34
	movq	global_dc(%rip), %rax
	movl	60(%rax), %eax
	testq	%rax, %rax
	jne	.LBB78_344
# BB#106:                               # %if.end37
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.gen_subprogram_die, %rcx
	movq	$10237, %rsi            # imm = 0x27FD
	jmp	.LBB78_30
.LBB78_122:                             # %land.lhs.true.i.i1005
	movl	16(%rcx), %eax
	cmpq	$6, %rax
	jne	.LBB78_103
# BB#123:                               # %AT_unsigned.exit.i1009
	movq	24(%rcx), %rdx
.LBB78_124:                             # %get_AT_unsigned.exit1011
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	andq	%rax, %rcx
	andq	%rax, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB78_154
# BB#125:
	movq	%rdi, %rcx
.LBB78_126:                             # %tailrecurse.i.i868
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_128 Depth 2
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB78_137
# BB#127:                               # %if.then.i.i872
                                        #   in Loop: Header=BB78_126 Depth=1
	movq	16(%rcx), %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	movabsq	$0, %rsi
	je	.LBB78_137
	.align	16, 0x90
.LBB78_128:                             # %for.body.i.i877
                                        #   Parent Loop BB78_126 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rdx), %esi
	cmpq	$49, %rsi
	je	.LBB78_131
# BB#129:                               # %for.body.i.i877
                                        #   in Loop: Header=BB78_128 Depth=2
	cmpq	$59, %rsi
	je	.LBB78_135
# BB#130:                               # %for.body.i.i877
                                        #   in Loop: Header=BB78_128 Depth=2
	cmpq	$71, %rsi
	jne	.LBB78_133
.LBB78_131:                             # %land.lhs.true.i.i.i882
                                        #   in Loop: Header=BB78_128 Depth=2
	movl	16(%rdx), %ecx
	cmpq	$10, %rcx
	jne	.LBB78_84
# BB#132:                               # %AT_ref.exit.i.i886
                                        #   in Loop: Header=BB78_128 Depth=2
	movq	24(%rdx), %rcx
.LBB78_133:                             # %for.inc.i.i891
                                        #   in Loop: Header=BB78_128 Depth=2
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB78_128
# BB#134:                               # %for.end.i.i893
                                        #   in Loop: Header=BB78_126 Depth=1
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	jne	.LBB78_126
	jmp	.LBB78_137
.LBB78_135:                             # %land.lhs.true.i.i897
	movl	16(%rdx), %ecx
	cmpq	$6, %rcx
	jne	.LBB78_103
# BB#136:                               # %AT_unsigned.exit.i901
	movq	24(%rdx), %rsi
.LBB78_137:                             # %get_AT_unsigned.exit903
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	32(%rcx), %ecx
	andq	%rax, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB78_154
.LBB78_138:                             # %if.then.i811
	leaq	16(%rdi), %rcx
	movq	24(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB78_139:                             # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB78_148
# BB#140:                               # %for.body.i
                                        #   in Loop: Header=BB78_139 Depth=1
	movl	(%rbx), %edx
	leaq	8(%rbx), %rcx
	cmpq	$60, %rdx
	jne	.LBB78_139
# BB#141:                               # %if.then5.i
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movl	16(%rbx), %eax
	leaq	-12(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB78_146
# BB#142:                               # %if.then5.i
	andq	%rcx, %rax
	cmpq	$8, %rax
	jne	.LBB78_143
.LBB78_146:                             # %sw.bb8.i.i
	movq	%rdi, %r15
	movq	24(%rbx), %rdi
	callq	free
.LBB78_147:                             # %free_AT.exit.i
	movq	%rbx, %rdi
	callq	free
	movq	%r15, %rdi
.LBB78_148:                             # %remove_AT.exit
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	remove_children
	movq	%rbx, %rsi
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	movq	%rsi, %r15
	jne	.LBB78_57
	jmp	.LBB78_149
.LBB78_154:                             # %if.else53
	movq	%rdi, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$46, %rax
	movl	%eax, (%r15)
	testq	%r14, %r14
	je	.LBB78_155
# BB#158:                               # %if.else.i807
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	jmp	.LBB78_159
.LBB78_155:                             # %if.then.i795
	testq	%r15, %r15
	movq	24(%rsp), %rbx          # 8-byte Reload
	je	.LBB78_159
# BB#156:                               # %if.then.i.i797
	cmpq	%r12, %r15
	je	.LBB78_35
# BB#157:                               # %if.end.i.i802
	movq	%r12, 24(%r15)
	movq	32(%r12), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r12)
.LBB78_159:                             # %new_die.exit809
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$71, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, %rsi
	movq	%rsi, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	movq	%rsi, %rcx
	je	.LBB78_162
# BB#160:                               # %new_die.exit809
	testq	%rax, %rax
	movq	%rsi, %rcx
	je	.LBB78_162
# BB#161:                               # %if.then.i.i790
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	movq	%rsi, %rcx
.LBB78_162:                             # %tailrecurse.i.i743
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_165 Depth 2
	testq	%rcx, %rcx
	je	.LBB78_163
# BB#164:                               # %if.then.i.i747
                                        #   in Loop: Header=BB78_162 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB78_174
	.align	16, 0x90
.LBB78_165:                             # %for.body.i.i752
                                        #   Parent Loop BB78_162 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB78_168
# BB#166:                               # %for.body.i.i752
                                        #   in Loop: Header=BB78_165 Depth=2
	cmpq	$58, %rdx
	je	.LBB78_172
# BB#167:                               # %for.body.i.i752
                                        #   in Loop: Header=BB78_165 Depth=2
	cmpq	$71, %rdx
	jne	.LBB78_170
.LBB78_168:                             # %land.lhs.true.i.i.i757
                                        #   in Loop: Header=BB78_165 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB78_84
# BB#169:                               # %AT_ref.exit.i.i761
                                        #   in Loop: Header=BB78_165 Depth=2
	movq	24(%rax), %rcx
.LBB78_170:                             # %for.inc.i.i766
                                        #   in Loop: Header=BB78_165 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB78_165
# BB#171:                               # %for.end.i.i768
                                        #   in Loop: Header=BB78_162 Depth=1
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	jne	.LBB78_162
	jmp	.LBB78_174
.LBB78_172:                             # %land.lhs.true.i.i772
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB78_103
# BB#173:                               # %AT_unsigned.exit.i776
	movq	24(%rax), %rdx
.LBB78_174:                             # %get_AT_unsigned.exit778
	movq	%rbx, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rax           # 8-byte Reload
	andq	%r14, %rax
	andq	%r14, %rdx
	cmpq	%rax, %rdx
	movq	%rsi, %rcx
	je	.LBB78_178
# BB#175:                               # %if.then57
	movq	%rsi, %rbx
	movq	%rax, %r13
	movq	$40, %rdi
	callq	xmalloc
	movq	%rbx, %rsi
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, 24(%rax)
	movq	%rsi, %rcx
	je	.LBB78_178
# BB#176:                               # %if.then57
	testq	%rax, %rax
	movq	%rsi, %rcx
	je	.LBB78_178
# BB#177:                               # %if.then.i.i739
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	movq	%rsi, %rcx
.LBB78_178:                             # %tailrecurse.i.i692
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_180 Depth 2
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB78_189
# BB#179:                               # %if.then.i.i696
                                        #   in Loop: Header=BB78_178 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB78_189
	.align	16, 0x90
.LBB78_180:                             # %for.body.i.i701
                                        #   Parent Loop BB78_178 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB78_183
# BB#181:                               # %for.body.i.i701
                                        #   in Loop: Header=BB78_180 Depth=2
	cmpq	$59, %rdx
	je	.LBB78_187
# BB#182:                               # %for.body.i.i701
                                        #   in Loop: Header=BB78_180 Depth=2
	cmpq	$71, %rdx
	jne	.LBB78_185
.LBB78_183:                             # %land.lhs.true.i.i.i706
                                        #   in Loop: Header=BB78_180 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB78_84
# BB#184:                               # %AT_ref.exit.i.i710
                                        #   in Loop: Header=BB78_180 Depth=2
	movq	24(%rax), %rcx
.LBB78_185:                             # %for.inc.i.i715
                                        #   in Loop: Header=BB78_180 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB78_180
# BB#186:                               # %for.end.i.i717
                                        #   in Loop: Header=BB78_178 Depth=1
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	jne	.LBB78_178
	jmp	.LBB78_189
.LBB78_187:                             # %land.lhs.true.i.i721
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB78_103
# BB#188:                               # %AT_unsigned.exit.i725
	movq	24(%rax), %rdx
.LBB78_189:                             # %get_AT_unsigned.exit727
	movl	32(%r12), %eax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	%r14, %rdx
	cmpq	%rax, %rdx
	jne	.LBB78_191
# BB#190:
	movq	%r12, %r14
	movq	16(%rsp), %r12          # 8-byte Reload
	jmp	.LBB78_56
.LBB78_191:                             # %if.then64
	movq	%rsi, %r13
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$59, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB78_192
# BB#193:                               # %if.then64
	testq	%rax, %rax
	movq	%r12, %r14
	jmp	.LBB78_246
.LBB78_192:
	movq	%r12, %r14
.LBB78_244:
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	%r13, %rsi
.LBB78_56:                              # %if.end112
	testq	%r12, %r12
	je	.LBB78_149
.LBB78_57:                              # %if.then114
	movq	%r14, 24(%rsp)          # 8-byte Spill
	jmp	.LBB78_58
.LBB78_149:                             # %if.else132
	movq	48(%r14), %rbx
	testq	$32768, %rbx            # imm = 0x8000
	je	.LBB78_271
# BB#150:                               # %if.then140
	testq	$2048, %rbx             # imm = 0x800
	je	.LBB78_262
# BB#151:                               # %if.then140
	movl	flag_no_inline(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_262
# BB#152:                               # %if.then149
	movq	$40, %rdi
	callq	xmalloc
	testq	$262144, %rbx           # imm = 0x40000
	movq	$0, 8(%rax)
	movq	$32, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	je	.LBB78_261
# BB#153:                               # %if.then156
	movq	$3, 24(%rax)
	jmp	.LBB78_263
	.align	16, 0x90
.LBB78_59:                              # %tailrecurse.i.i540
                                        #   in Loop: Header=BB78_58 Depth=1
	testq	%rsi, %rsi
	je	.LBB78_251
# BB#60:                                # %if.then.i.i544
                                        #   in Loop: Header=BB78_58 Depth=1
	movq	16(%rsi), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB78_251
	.align	16, 0x90
.LBB78_61:                              # %for.body.i.i549
                                        #   Parent Loop BB78_58 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %ecx
	cmpq	$49, %rcx
	je	.LBB78_64
# BB#62:                                # %for.body.i.i549
                                        #   in Loop: Header=BB78_61 Depth=2
	cmpq	$32, %rcx
	je	.LBB78_249
# BB#63:                                # %for.body.i.i549
                                        #   in Loop: Header=BB78_61 Depth=2
	cmpq	$71, %rcx
	jne	.LBB78_66
.LBB78_64:                              # %land.lhs.true.i.i.i554
                                        #   in Loop: Header=BB78_61 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB78_84
# BB#65:                                # %AT_ref.exit.i.i558
                                        #   in Loop: Header=BB78_61 Depth=2
	movq	24(%rax), %rsi
.LBB78_66:                              # %for.inc.i.i563
                                        #   in Loop: Header=BB78_61 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB78_61
.LBB78_58:                              # %if.then114
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_61 Depth 2
	testq	%rsi, %rsi
	jne	.LBB78_59
	jmp	.LBB78_251
.LBB78_249:                             # %land.lhs.true.i.i569
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB78_103
# BB#250:                               # %get_AT_unsigned.exit575
	movl	24(%rax), %eax
	testq	%rax, %rax
	movq	$1, %r14
	jne	.LBB78_318
.LBB78_251:                             # %if.then119
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$60, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r15, %r15
	je	.LBB78_254
# BB#252:                               # %if.then119
	testq	%rax, %rax
	je	.LBB78_254
# BB#253:                               # %if.then.i.i537
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_254:                             # %add_AT_flag.exit
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	$0, 80(%rcx)
	jne	.LBB78_256
# BB#255:                               # %lor.lhs.false122
	movzbl	49(%rcx), %eax
	movq	$1, %r14
	testq	$128, %rax
	je	.LBB78_318
.LBB78_256:                             # %if.then129
	movl	36(%rcx), %r14d
	movl	decl_die_table_allocated(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB78_258
# BB#257:                               # %if.then.i519
	leaq	256(%r14), %rax
	movabsq	$4294967040, %rsi       # imm = 0xFFFFFF00
	andq	%rax, %rsi
	movq	%rax, %rbx
	movq	decl_die_table(%rip), %rdi
	andq	$-256, %rbx
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	decl_die_table_allocated(%rip), %ecx
	movq	%rax, decl_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%rbx, %rax
	subq	%rcx, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, decl_die_table_allocated(%rip)
.LBB78_258:                             # %if.end.i521
	movl	decl_die_table_in_use(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB78_260
# BB#259:                               # %if.then9.i523
	leaq	1(%r14), %rax
	movl	%eax, decl_die_table_in_use(%rip)
.LBB78_260:                             # %equate_decl_number_to_die.exit526
	movq	decl_die_table(%rip), %rax
	movq	%r15, (%rax,%r14,8)
	movq	$1, %r14
	jmp	.LBB78_318
.LBB78_271:                             # %if.else161
	movq	%r14, 24(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	testq	$256, %rbx              # imm = 0x100
	jne	.LBB78_318
	jmp	.LBB78_272
.LBB78_262:                             # %if.else159
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$32, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$2, 24(%rax)
	jmp	.LBB78_263
.LBB78_261:                             # %if.else157
	movq	$1, 24(%rax)
.LBB78_263:                             # %if.else159
	testq	%r15, %r15
	je	.LBB78_266
# BB#264:                               # %if.else159
	testq	%rax, %rax
	je	.LBB78_266
# BB#265:                               # %if.then.i.i481
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB78_266:                             # %if.end160
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movl	36(%r14), %r14d
	movl	decl_die_table_allocated(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB78_268
# BB#267:                               # %if.then.i463
	leaq	256(%r14), %rax
	movabsq	$4294967040, %rsi       # imm = 0xFFFFFF00
	andq	%rax, %rsi
	movq	%rax, %rbx
	movq	decl_die_table(%rip), %rdi
	andq	$-256, %rbx
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	decl_die_table_allocated(%rip), %ecx
	movq	%rax, decl_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%rbx, %rax
	subq	%rcx, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, decl_die_table_allocated(%rip)
.LBB78_268:                             # %if.end.i465
	movl	decl_die_table_in_use(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB78_270
# BB#269:                               # %if.then9.i467
	leaq	1(%r14), %rax
	movl	%eax, decl_die_table_in_use(%rip)
.LBB78_270:                             # %equate_decl_number_to_die.exit470
	movq	decl_die_table(%rip), %rax
	movq	%r15, (%rax,%r14,8)
	xorq	%r14, %r14
.LBB78_318:                             # %if.end186
	movl	debug_info_level(%rip), %eax
	cmpq	$2, %rax
	jb	.LBB78_341
# BB#319:                               # %if.else190
	testq	%r14, %r14
	je	.LBB78_321
# BB#320:                               # %if.end233.thread
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	gen_formal_types_die
	jmp	.LBB78_344
.LBB78_321:                             # %if.else193
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB78_333
# BB#322:                               # %for.body.lr.ph
	movabsq	$.L.str415, %r12
	jmp	.LBB78_323
.LBB78_327:                             # %if.then.i.i379
                                        #   in Loop: Header=BB78_323 Depth=1
	testq	%r13, %r13
	je	.LBB78_332
# BB#328:                               # %if.then.i.i.i381
                                        #   in Loop: Header=BB78_323 Depth=1
	cmpq	%r15, %r13
	je	.LBB78_35
# BB#329:                               # %if.end.i.i.i386
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	%r15, 24(%r13)
	movq	32(%r15), %rax
	movq	%rax, 40(%r13)
	movq	%r13, 32(%r15)
	jmp	.LBB78_332
	.align	16, 0x90
.LBB78_323:                             # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$35, %rax
	jne	.LBB78_332
# BB#324:                               # %if.then201
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	72(%rbx), %rax
	testq	%rax, %rax
	je	.LBB78_331
# BB#325:                               # %land.lhs.true204
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	32(%rax), %rdi
	movq	%r12, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_331
# BB#326:                               # %if.then209
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r13
	testq	%r15, %r15
	movq	$24, %rax
	movl	%eax, (%r13)
	jne	.LBB78_327
# BB#330:                               # %if.else.i.i391
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r13, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
	jmp	.LBB78_332
	.align	16, 0x90
.LBB78_331:                             # %if.else210
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_decl_die
.LBB78_332:                             # %for.inc
                                        #   in Loop: Header=BB78_323 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB78_323
.LBB78_333:                             # %for.end
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rax
	movq	24(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB78_339
# BB#334:                               # %if.then219
	callq	tree_last
	movq	32(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB78_335
	jmp	.LBB78_341
.LBB78_339:                             # %if.else225
	cmpq	$0, 104(%rcx)
	jne	.LBB78_341
.LBB78_335:                             # %if.then223
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$24, %rax
	movl	%eax, (%rbx)
	testq	%r15, %r15
	je	.LBB78_340
# BB#336:                               # %if.then.i.i361
	testq	%rbx, %rbx
	je	.LBB78_341
# BB#337:                               # %if.then.i.i.i363
	cmpq	%r15, %rbx
	je	.LBB78_35
# BB#338:                               # %if.end.i.i.i368
	movq	%r15, 24(%rbx)
	movq	32(%r15), %rax
	movq	%rax, 40(%rbx)
	movq	%rbx, 32(%r15)
	jmp	.LBB78_341
.LBB78_273:                             # %tailrecurse.i.i
                                        #   in Loop: Header=BB78_272 Depth=1
	testq	%rsi, %rsi
	je	.LBB78_283
# BB#274:                               # %if.then.i.i446
                                        #   in Loop: Header=BB78_272 Depth=1
	movq	16(%rsi), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB78_283
	.align	16, 0x90
.LBB78_275:                             # %for.body.i.i
                                        #   Parent Loop BB78_272 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %ecx
	cmpq	$49, %rcx
	je	.LBB78_278
# BB#276:                               # %for.body.i.i
                                        #   in Loop: Header=BB78_275 Depth=2
	cmpq	$32, %rcx
	je	.LBB78_281
# BB#277:                               # %for.body.i.i
                                        #   in Loop: Header=BB78_275 Depth=2
	cmpq	$71, %rcx
	jne	.LBB78_280
.LBB78_278:                             # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB78_275 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB78_84
# BB#279:                               # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB78_275 Depth=2
	movq	24(%rax), %rsi
.LBB78_280:                             # %for.inc.i.i
                                        #   in Loop: Header=BB78_275 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB78_275
.LBB78_272:                             # %if.then168
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_275 Depth 2
	testq	%rsi, %rsi
	jne	.LBB78_273
	jmp	.LBB78_283
.LBB78_340:                             # %if.else.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%rbx, (%rax)
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB78_341:                             # %if.end233
	testq	%r14, %r14
	jne	.LBB78_344
# BB#342:                               # %land.lhs.true237
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rdi
	movzbl	16(%rdi), %eax
	testq	%rax, %rax
	je	.LBB78_344
# BB#343:                               # %if.then244
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	decls_for_scope
.LBB78_344:                             # %if.end245
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB78_281:                             # %land.lhs.true.i.i
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB78_103
# BB#282:                               # %get_AT_unsigned.exit
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB78_288
.LBB78_283:                             # %if.then173
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	36(%rax), %r14d
	movl	decl_die_table_allocated(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB78_285
# BB#284:                               # %if.then.i
	leaq	256(%r14), %rax
	movabsq	$4294967040, %rsi       # imm = 0xFFFFFF00
	andq	%rax, %rsi
	movq	%rax, %rbx
	movq	decl_die_table(%rip), %rdi
	andq	$-256, %rbx
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	decl_die_table_allocated(%rip), %ecx
	movq	%rax, decl_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%rbx, %rax
	subq	%rcx, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, decl_die_table_allocated(%rip)
.LBB78_285:                             # %if.end.i443
	movl	decl_die_table_in_use(%rip), %eax
	cmpq	%r14, %rax
	ja	.LBB78_287
# BB#286:                               # %if.then9.i
	leaq	1(%r14), %rax
	movl	%eax, decl_die_table_in_use(%rip)
.LBB78_287:                             # %equate_decl_number_to_die.exit
	movq	decl_die_table(%rip), %rax
	movq	%r15, (%rax,%r14,8)
.LBB78_288:                             # %do.body
	movl	current_funcdef_number(%rip), %ecx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str5, %rdx
	leaq	32(%rsp), %r12
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$17, %rax
	movl	%eax, (%rbx)
	movq	$12, %r14
	movl	%r14d, 16(%rbx)
	movq	%r12, %rdi
	callq	xstrdup
	movq	%rax, 24(%rbx)
	testq	%r15, %r15
	je	.LBB78_291
# BB#289:                               # %do.body
	testq	%rbx, %rbx
	je	.LBB78_291
# BB#290:                               # %if.then.i.i436
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB78_291:                             # %add_AT_lbl_id.exit437
	movl	current_funcdef_number(%rip), %ecx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str7, %rdx
	leaq	32(%rsp), %r12
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$18, %rax
	movl	%eax, (%rbx)
	movl	%r14d, 16(%rbx)
	movq	%r12, %rdi
	callq	xstrdup
	testq	%r15, %r15
	movq	%rax, 24(%rbx)
	je	.LBB78_294
# BB#292:                               # %add_AT_lbl_id.exit437
	testq	%rbx, %rbx
	je	.LBB78_294
# BB#293:                               # %if.then.i.i423
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB78_294:                             # %add_AT_lbl_id.exit
	movq	24(%rsp), %rbx          # 8-byte Reload
	movzbl	18(%rbx), %eax
	testq	$8, %rax
	je	.LBB78_299
# BB#295:                               # %if.end.i404
	movq	%rbx, %r12
	movl	pubname_table_in_use(%rip), %ecx
	movl	pubname_table_allocated(%rip), %eax
	cmpq	%rax, %rcx
	jne	.LBB78_296
# BB#297:                               # %if.then1.i
	addq	$64, %rcx
	movl	%ecx, pubname_table_allocated(%rip)
	movq	pubname_table(%rip), %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	pubname_table_in_use(%rip), %ecx
	movq	%rax, %r14
	movq	%r14, pubname_table(%rip)
	jmp	.LBB78_298
.LBB78_296:                             # %if.end.if.end2_crit_edge.i
	movq	pubname_table(%rip), %r14
.LBB78_298:                             # %if.end2.i
	leaq	1(%rcx), %rax
	movl	%eax, pubname_table_in_use(%rip)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	%r15, (%r14,%rbx)
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rdi
	callq	xstrdup
	movq	%rax, 8(%r14,%rbx)
	movq	%r12, %rbx
.LBB78_299:                             # %add_pubname.exit
	cmpq	$0, 128(%rbx)
	je	.LBB78_304
# BB#300:                               # %if.end.i
	movl	arange_table_in_use(%rip), %ecx
	movl	arange_table_allocated(%rip), %eax
	cmpq	%rax, %rcx
	jne	.LBB78_301
# BB#302:                               # %if.then2.i400
	addq	$64, %rcx
	movl	%ecx, arange_table_allocated(%rip)
	movq	arange_table(%rip), %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	arange_table_in_use(%rip), %ecx
	movq	%rax, arange_table(%rip)
	jmp	.LBB78_303
.LBB78_301:                             # %if.end.if.end3_crit_edge.i
	movq	arange_table(%rip), %rax
.LBB78_303:                             # %if.end3.i
	leaq	1(%rcx), %rdx
	movl	%edx, arange_table_in_use(%rip)
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	%r15, (%rax,%rdx,8)
.LBB78_304:                             # %add_arange.exit
	movl	frame_pointer_needed(%rip), %eax
	testq	%rax, %rax
	je	.LBB78_306
# BB#305:
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	global_rtl+32(%rip), %rax
	jmp	.LBB78_307
.LBB78_306:                             # %select.mid
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	global_rtl+16(%rip), %rax
.LBB78_307:                             # %select.end
	movl	8(%rax), %eax
	xorq	%r14, %r14
	cmpq	$52, %rax
	movabsq	$0, %rbx
	ja	.LBB78_315
# BB#308:                               # %reg_number.exit.i
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB78_309
# BB#310:                               # %reg_number.exit.i
	movabsq	$dbx64_register_map, %rcx
	jmp	.LBB78_311
.LBB78_309:
	movabsq	$svr4_dbx_register_map, %rcx
.LBB78_311:                             # %reg_number.exit.i
	movl	(%rcx,%rax,4), %r12d
	cmpq	$31, %r12
	ja	.LBB78_313
# BB#312:                               # %if.then2.i
	addq	$80, %r12
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movl	%r12d, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	$0, 24(%rbx)
	jmp	.LBB78_314
.LBB78_313:                             # %if.else.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$144, %rax
	movl	%eax, 8(%rbx)
	movq	$6, %rax
	movl	%eax, 16(%rbx)
	movq	%r12, 24(%rbx)
.LBB78_314:                             # %reg_loc_descriptor.exit
	movl	%eax, 40(%rbx)
	movq	$0, 48(%rbx)
.LBB78_315:                             # %reg_loc_descriptor.exit
	movq	$40, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	$0, 8(%rax)
	movq	$64, %rcx
	movl	%ecx, (%rax)
	movq	$2, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB78_318
# BB#316:                               # %reg_loc_descriptor.exit
	testq	%rax, %rax
	je	.LBB78_318
# BB#317:                               # %if.then.i.i394
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
	jmp	.LBB78_318
.LBB78_143:                             # %if.then5.i
	movq	%rdi, %r15
	cmpq	$14, %rax
	jne	.LBB78_147
# BB#144:                               # %sw.bb.i.i
	movq	24(%rbx), %rax
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB78_147
# BB#145:                               # %if.then.i.i820
	decq	%rcx
	movl	%ecx, 16(%rax)
	jmp	.LBB78_147
.LBB78_163:
	xorq	%rdx, %rdx
	jmp	.LBB78_174
.LBB78_53:                              # %if.then38.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_abstract_origin_attribute, %rcx
	movq	$9459, %rsi             # imm = 0x24F3
	jmp	.LBB78_30
.LBB78_90:                              # %if.end.i.i933
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_flag, %rcx
	movq	$4387, %rsi             # imm = 0x1123
	jmp	.LBB78_30
.LBB78_35:                              # %if.then3.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB78_30
.LBB78_84:                              # %if.end.i.i.i919
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB78_30
.LBB78_103:                             # %if.end.i.i968
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
.LBB78_30:                              # %if.then9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp928:
	.size	gen_subprogram_die, .Ltmp928-gen_subprogram_die
	.cfi_endproc

	.align	16, 0x90
	.type	gen_variable_die,@function
gen_variable_die:                       # @gen_variable_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp935:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp936:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp937:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp938:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp939:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp940:
	.cfi_def_cfa_offset 64
.Ltmp941:
	.cfi_offset %rbx, -48
.Ltmp942:
	.cfi_offset %r12, -40
.Ltmp943:
	.cfi_offset %r13, -32
.Ltmp944:
	.cfi_offset %r14, -24
.Ltmp945:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	48(%r13), %rdx
	movq	112(%r13), %rax
	movq	%rsi, %rbx
	movq	$15, %rcx
	shrq	%cl, %rdx
	cmpq	%r13, %rax
	jne	.LBB79_1
# BB#2:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB79_3
.LBB79_1:
	movabsq	$0, %rcx
.LBB79_3:                               # %entry
	xorq	%r12, %r12
	testq	%rcx, %rdx
	jne	.LBB79_5
# BB#4:                                 # %entry
	movq	%rax, %r12
.LBB79_5:                               # %entry
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r14
	testq	%rbx, %rbx
	movq	$52, %rax
	movl	%eax, (%r14)
	je	.LBB79_11
# BB#6:                                 # %if.then.i
	testq	%r14, %r14
	je	.LBB79_12
# BB#7:                                 # %if.then.i.i
	cmpq	%rbx, %r14
	je	.LBB79_8
# BB#10:                                # %if.end.i.i
	movq	%rbx, 24(%r14)
	movq	32(%rbx), %rax
	movq	%rax, 40(%r14)
	movq	%r14, 32(%rbx)
	jmp	.LBB79_12
.LBB79_11:                              # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r14, (%rax)
	movq	%r13, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB79_12:                              # %new_die.exit
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movl	36(%r13), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	xorq	%rbx, %rbx
	cmpq	%rax, %rcx
	jbe	.LBB79_14
# BB#13:                                # %cond.true.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB79_14:                              # %lookup_decl_die.exit
	movzbl	49(%r13), %r15d
	andq	$1, %r15
	jne	.LBB79_19
# BB#15:                                # %lookup_decl_die.exit
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB79_19
# BB#16:                                # %land.rhs.i
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	(%rax), %eax
	movabsq	$4294967291, %rcx       # imm = 0xFFFFFFFB
	andq	%rax, %rcx
	cmpq	$19, %rcx
	jne	.LBB79_17
# BB#18:                                # %land.rhs.i
	movabsq	$1, %r15
	jmp	.LBB79_19
.LBB79_17:
	movabsq	$0, %r15
.LBB79_19:                              # %lor.end
	testq	%r12, %r12
	je	.LBB79_33
# BB#20:                                # %if.then
	movzbl	16(%r12), %eax
	cmpq	$30, %rax
	je	.LBB79_25
# BB#21:                                # %if.then.i376
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%r12, %rdi
	jne	.LBB79_23
# BB#22:                                # %if.then7.i
	movq	(%r12), %rdi
.LBB79_23:                              # %if.end.i379
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB79_25
# BB#24:                                # %if.then9.i380
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB79_25:                              # %if.end11.i
	movzbl	16(%r12), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB79_26
# BB#29:                                # %if.then32.i
	movq	88(%r12), %rbx
	jmp	.LBB79_30
.LBB79_33:                              # %if.else
	testq	%rbx, %rbx
	movq	%r15, %r12
	je	.LBB79_46
# BB#34:                                # %land.lhs.true
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB79_46
# BB#35:
	movq	%rbx, %rcx
.LBB79_36:                              # %tailrecurse.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB79_38 Depth 2
	testq	%rcx, %rcx
	je	.LBB79_46
# BB#37:                                # %if.then.i.i154
                                        #   in Loop: Header=BB79_36 Depth=1
	movq	16(%rcx), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB79_46
	.align	16, 0x90
.LBB79_38:                              # %for.body.i.i
                                        #   Parent Loop BB79_36 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	cmpq	$49, %rdx
	je	.LBB79_41
# BB#39:                                # %for.body.i.i
                                        #   in Loop: Header=BB79_38 Depth=2
	cmpq	$60, %rdx
	je	.LBB79_70
# BB#40:                                # %for.body.i.i
                                        #   in Loop: Header=BB79_38 Depth=2
	cmpq	$71, %rdx
	jne	.LBB79_44
.LBB79_41:                              # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB79_38 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB79_42
# BB#43:                                # %AT_ref.exit.i.i
                                        #   in Loop: Header=BB79_38 Depth=2
	movq	24(%rax), %rcx
.LBB79_44:                              # %for.inc.i.i
                                        #   in Loop: Header=BB79_38 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB79_38
# BB#45:                                # %for.end.i.i
                                        #   in Loop: Header=BB79_36 Depth=1
	testq	%rcx, %rcx
	jne	.LBB79_36
	jmp	.LBB79_46
.LBB79_26:                              # %if.end11.i
	cmpq	$100, %rax
	jne	.LBB79_31
# BB#27:                                # %if.then21.i
	movl	36(%r12), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB79_31
# BB#28:                                # %cond.true.i.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB79_30:                              # %if.end35.i
	testq	%rbx, %rbx
	je	.LBB79_31
# BB#32:                                # %add_abstract_origin_attribute.exit
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$49, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r14, %r14
	movq	%r15, %r12
	jne	.LBB79_110
	jmp	.LBB79_112
.LBB79_70:                              # %land.lhs.true.i.i
	movl	16(%rax), %ecx
	cmpq	$9, %rcx
	jne	.LBB79_71
# BB#72:                                # %get_AT_flag.exit
	movzbl	24(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB79_46
# BB#73:                                # %if.then14
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$71, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r14, %r14
	je	.LBB79_76
# BB#74:                                # %if.then14
	testq	%rax, %rax
	je	.LBB79_76
# BB#75:                                # %if.then.i.i163
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_76:                              # %add_AT_die_ref.exit
	cmpq	$0, 72(%r13)
	je	.LBB79_112
# BB#77:                                # %if.then17
	movq	24(%r13), %rdi
	callq	lookup_filename
	movq	%rax, %r15
	movq	%rbx, %rax
.LBB79_78:                              # %tailrecurse.i.i166
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB79_80 Depth 2
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB79_90
# BB#79:                                # %if.then.i.i170
                                        #   in Loop: Header=BB79_78 Depth=1
	movq	16(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB79_90
	.align	16, 0x90
.LBB79_80:                              # %for.body.i.i175
                                        #   Parent Loop BB79_78 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rcx), %edx
	cmpq	$49, %rdx
	je	.LBB79_83
# BB#81:                                # %for.body.i.i175
                                        #   in Loop: Header=BB79_80 Depth=2
	cmpq	$58, %rdx
	je	.LBB79_87
# BB#82:                                # %for.body.i.i175
                                        #   in Loop: Header=BB79_80 Depth=2
	cmpq	$71, %rdx
	jne	.LBB79_85
.LBB79_83:                              # %land.lhs.true.i.i.i179
                                        #   in Loop: Header=BB79_80 Depth=2
	movl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB79_42
# BB#84:                                # %AT_ref.exit.i.i183
                                        #   in Loop: Header=BB79_80 Depth=2
	movq	24(%rcx), %rax
.LBB79_85:                              # %for.inc.i.i188
                                        #   in Loop: Header=BB79_80 Depth=2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB79_80
# BB#86:                                # %for.end.i.i190
                                        #   in Loop: Header=BB79_78 Depth=1
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jne	.LBB79_78
	jmp	.LBB79_90
.LBB79_46:                              # %if.else33
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	add_name_and_src_coords_attributes
	movq	8(%r13), %rdi
	movl	16(%rdi), %eax
	movl	16(%r13), %ebx
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rax, %rdx
	andq	$254, %rdx
	cmpq	$6, %rdx
	jne	.LBB79_50
# BB#47:                                # %land.lhs.true.i352
	movq	8(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB79_50
# BB#48:                                # %land.lhs.true6.i
	cmpq	$0, 96(%rdi)
	jne	.LBB79_50
# BB#49:                                # %if.then.i355
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movq	%rdx, %rdi
.LBB79_50:                              # %if.end.i356
	testq	%rcx, %rcx
	je	.LBB79_60
# BB#51:                                # %if.end.i356
	cmpq	$5, %rcx
	je	.LBB79_60
# BB#52:                                # %if.end19.i
	movq	%rbx, %rcx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$2048, %rbx             # imm = 0x800
	movq	$1, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rcx
	movq	$1, %rsi
	jne	.LBB79_54
# BB#53:                                # %lor.rhs.i
	movq	%rax, %rsi
	andq	$4096, %rsi             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rsi
.LBB79_54:                              # %lor.end.i
	testq	%r8, %rbx
	jne	.LBB79_56
# BB#55:                                # %lor.rhs25.i
	andq	$2048, %rax             # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %rax
	movq	%rax, %rdx
.LBB79_56:                              # %lor.end31.i
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	modified_type_die
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB79_60
# BB#57:                                # %if.then34.i
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$73, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r14, %r14
	je	.LBB79_60
# BB#58:                                # %if.then34.i
	testq	%rax, %rax
	je	.LBB79_60
# BB#59:                                # %if.then.i.i.i369
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_60:                              # %add_type_attribute.exit
	movzbl	18(%r13), %eax
	testq	$8, %rax
	je	.LBB79_64
# BB#61:                                # %if.then48
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$63, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r14, %r14
	je	.LBB79_64
# BB#62:                                # %if.then48
	testq	%rax, %rax
	je	.LBB79_64
# BB#63:                                # %if.then.i.i268
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_64:                              # %if.end49
	movzbl	50(%r13), %eax
	testq	$64, %rax
	je	.LBB79_68
# BB#65:                                # %if.then56
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$52, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r14, %r14
	je	.LBB79_68
# BB#66:                                # %if.then56
	testq	%rax, %rax
	je	.LBB79_68
# BB#67:                                # %if.then.i.i279
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_68:                              # %if.end57
	movl	16(%r13), %eax
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB79_107
# BB#69:                                # %if.then63
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$2, 24(%rax)
	jmp	.LBB79_109
.LBB79_107:                             # %if.else64
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB79_112
# BB#108:                               # %if.then70
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$50, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	$3, 24(%rax)
.LBB79_109:                             # %if.then70
	testq	%r14, %r14
	jne	.LBB79_110
	jmp	.LBB79_112
.LBB79_87:                              # %land.lhs.true.i.i193
	movl	16(%rcx), %eax
	cmpq	$6, %rax
	jne	.LBB79_88
# BB#89:                                # %AT_unsigned.exit.i
	movq	24(%rcx), %rdx
.LBB79_90:                              # %get_AT_unsigned.exit
	movq	%r12, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r15
	andq	%r12, %rdx
	cmpq	%r15, %rdx
	je	.LBB79_94
# BB#91:                                # %if.then22
	movq	$40, %rdi
	callq	xmalloc
	testq	%r14, %r14
	movq	$0, 8(%rax)
	movq	$58, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 24(%rax)
	je	.LBB79_94
# BB#92:                                # %if.then22
	testq	%rax, %rax
	je	.LBB79_94
# BB#93:                                # %if.then.i.i206
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_94:                              # %tailrecurse.i.i209
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB79_96 Depth 2
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	je	.LBB79_105
# BB#95:                                # %if.then.i.i213
                                        #   in Loop: Header=BB79_94 Depth=1
	movq	16(%rbx), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB79_105
	.align	16, 0x90
.LBB79_96:                              # %for.body.i.i218
                                        #   Parent Loop BB79_94 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %ecx
	cmpq	$49, %rcx
	je	.LBB79_99
# BB#97:                                # %for.body.i.i218
                                        #   in Loop: Header=BB79_96 Depth=2
	cmpq	$59, %rcx
	je	.LBB79_103
# BB#98:                                # %for.body.i.i218
                                        #   in Loop: Header=BB79_96 Depth=2
	cmpq	$71, %rcx
	jne	.LBB79_101
.LBB79_99:                              # %land.lhs.true.i.i.i223
                                        #   in Loop: Header=BB79_96 Depth=2
	movl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB79_42
# BB#100:                               # %AT_ref.exit.i.i227
                                        #   in Loop: Header=BB79_96 Depth=2
	movq	24(%rax), %rbx
.LBB79_101:                             # %for.inc.i.i232
                                        #   in Loop: Header=BB79_96 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB79_96
# BB#102:                               # %for.end.i.i234
                                        #   in Loop: Header=BB79_94 Depth=1
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	jne	.LBB79_94
	jmp	.LBB79_105
.LBB79_103:                             # %land.lhs.true.i.i238
	movl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB79_88
# BB#104:                               # %AT_unsigned.exit.i242
	movq	24(%rax), %rdx
.LBB79_105:                             # %get_AT_unsigned.exit244
	movl	32(%r13), %eax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	%r12, %rdx
	cmpq	%rax, %rdx
	movq	(%rsp), %r12            # 8-byte Reload
	je	.LBB79_112
# BB#106:                               # %if.then27
	movq	$40, %rdi
	callq	xmalloc
	testq	%r14, %r14
	movq	$0, 8(%rax)
	movq	$59, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	je	.LBB79_112
.LBB79_110:                             # %if.then70
	testq	%rax, %rax
	je	.LBB79_112
# BB#111:                               # %if.then.i.i305
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_112:                             # %if.end74
	testq	$1, %r12
	je	.LBB79_116
# BB#113:                               # %if.then76
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$60, %rcx
	movl	%ecx, (%rax)
	movq	$9, %rcx
	movl	%ecx, 16(%rax)
	movq	$1, %rcx
	movb	%cl, 24(%rax)
	testq	%r14, %r14
	je	.LBB79_116
# BB#114:                               # %if.then76
	testq	%rax, %rax
	je	.LBB79_116
# BB#115:                               # %if.then.i.i317
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_116:                             # %if.end77
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%rax, %rax
	je	.LBB79_118
# BB#117:                               # %land.rhs.i321
	movl	(%rax), %eax
	movabsq	$4294967291, %rcx       # imm = 0xFFFFFFFB
	andq	%rax, %rcx
	cmpq	$19, %rcx
	je	.LBB79_119
.LBB79_118:                             # %lor.lhs.false
	movzbl	49(%r13), %eax
	testq	$128, %rax
	je	.LBB79_124
.LBB79_119:                             # %if.then86
	movl	36(%r13), %r15d
	movl	decl_die_table_allocated(%rip), %eax
	cmpq	%r15, %rax
	ja	.LBB79_121
# BB#120:                               # %if.then.i328
	leaq	256(%r15), %rax
	movabsq	$4294967040, %rsi       # imm = 0xFFFFFF00
	andq	%rax, %rsi
	movq	%rax, %rbx
	movq	decl_die_table(%rip), %rdi
	andq	$-256, %rbx
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	decl_die_table_allocated(%rip), %ecx
	movq	%rax, decl_die_table(%rip)
	leaq	(%rax,%rcx,8), %rdi
	movq	%rbx, %rax
	subq	%rcx, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, decl_die_table_allocated(%rip)
.LBB79_121:                             # %if.end.i
	movl	decl_die_table_in_use(%rip), %eax
	cmpq	%r15, %rax
	ja	.LBB79_123
# BB#122:                               # %if.then9.i
	leaq	1(%r15), %rax
	movl	%eax, decl_die_table_in_use(%rip)
.LBB79_123:                             # %equate_decl_number_to_die.exit
	movq	decl_die_table(%rip), %rax
	movq	%r14, (%rax,%r15,8)
.LBB79_124:                             # %if.end87
	testq	$1, %r12
	jne	.LBB79_131
# BB#125:                               # %land.lhs.true89
	movzbl	49(%r13), %eax
	testq	$128, %rax
	je	.LBB79_126
.LBB79_131:                             # %if.else98
	movl	16(%r13), %eax
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB79_142
# BB#132:                               # %land.lhs.true.i
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB79_142
# BB#133:                               # %land.lhs.true.i
	movq	104(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB79_142
# BB#134:                               # %land.lhs.true11.i
	movq	8(%r13), %r15
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	initializer_constant_valid_p
	movq	global_trees+112(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB79_142
# BB#135:                               # %if.end.i343
	movzbl	16(%r15), %eax
	cmpq	$6, %rax
	jne	.LBB79_142
# BB#136:                               # %sw.bb.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB79_138
# BB#137:                               # %if.then17.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	movq	%rax, %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$28, %rcx
	movl	%ecx, (%rax)
	movq	$6, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	jmp	.LBB79_139
.LBB79_126:                             # %if.then97
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	add_location_or_const_value_attribute
	movzbl	18(%r13), %eax
	testq	$8, %rax
	je	.LBB79_142
# BB#127:                               # %if.end.i332
	movl	pubname_table_in_use(%rip), %ecx
	movl	pubname_table_allocated(%rip), %eax
	cmpq	%rax, %rcx
	jne	.LBB79_128
# BB#129:                               # %if.then1.i
	addq	$64, %rcx
	movl	%ecx, pubname_table_allocated(%rip)
	movq	pubname_table(%rip), %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	pubname_table_in_use(%rip), %ecx
	movq	%rax, %r15
	movq	%r15, pubname_table(%rip)
	jmp	.LBB79_130
.LBB79_128:                             # %if.end.if.end2_crit_edge.i
	movq	pubname_table(%rip), %r15
.LBB79_130:                             # %if.end2.i
	leaq	1(%rcx), %rax
	movl	%eax, pubname_table_in_use(%rip)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	%r14, (%r15,%rbx)
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rdi
	callq	xstrdup
	movq	%rax, 8(%r15,%rbx)
	jmp	.LBB79_142
.LBB79_138:                             # %if.else19.i
	movq	32(%rbx), %r15
	movq	40(%rbx), %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$28, %rcx
	movl	%ecx, (%rax)
	movq	$7, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	movq	%r15, 32(%rax)
.LBB79_139:                             # %if.else19.i
	testq	%r14, %r14
	je	.LBB79_142
# BB#140:                               # %if.else19.i
	testq	%rax, %rax
	je	.LBB79_142
# BB#141:                               # %if.then.i.i42.i
	movq	16(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r14)
.LBB79_142:                             # %if.end99
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB79_8:                               # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB79_9
.LBB79_31:                              # %if.then38.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_abstract_origin_attribute, %rcx
	movq	$9459, %rsi             # imm = 0x24F3
	jmp	.LBB79_9
.LBB79_71:                              # %if.end.i.i156
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_flag, %rcx
	movq	$4387, %rsi             # imm = 0x1123
	jmp	.LBB79_9
.LBB79_42:                              # %if.end.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_ref, %rcx
	movq	$4588, %rsi             # imm = 0x11EC
	jmp	.LBB79_9
.LBB79_88:                              # %if.end.i.i194
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.AT_unsigned, %rcx
	movq	$4441, %rsi             # imm = 0x1159
.LBB79_9:                               # %if.then3.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp946:
	.size	gen_variable_die, .Ltmp946-gen_variable_die
	.cfi_endproc

	.align	16, 0x90
	.type	decls_for_scope,@function
decls_for_scope:                        # @decls_for_scope
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp953:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp954:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp955:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp956:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp957:
	.cfi_def_cfa_offset 48
.Ltmp958:
	.cfi_offset %rbx, -48
.Ltmp959:
	.cfi_offset %r12, -40
.Ltmp960:
	.cfi_offset %r13, -32
.Ltmp961:
	.cfi_offset %r14, -24
.Ltmp962:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB80_32
# BB#1:                                 # %lor.lhs.false
	movzbl	18(%r15), %eax
	testq	$1, %rax
	je	.LBB80_32
# BB#2:                                 # %if.end
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB80_29
# BB#3:                                 # %for.body.lr.ph
	movabsq	$0, %r13
	.align	16, 0x90
.LBB80_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB80_5
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB80_4 Depth=1
	cmpq	$0, 72(%rbx)
	jne	.LBB80_10
# BB#9:                                 # %land.lhs.true.if.then39_crit_edge
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	8(%rbx), %rax
.LBB80_21:                              # %if.then39
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	88(%rax), %rax
	jmp	.LBB80_22
	.align	16, 0x90
.LBB80_5:                               # %for.body
                                        #   in Loop: Header=BB80_4 Depth=1
	cmpq	$30, %rax
	jne	.LBB80_27
# BB#6:                                 # %if.then6
                                        #   in Loop: Header=BB80_4 Depth=1
	movl	36(%rbx), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB80_27
# BB#7:                                 # %cond.true.i
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
.LBB80_22:                              # %if.end45
                                        #   in Loop: Header=BB80_4 Depth=1
	testq	%rax, %rax
	je	.LBB80_27
# BB#23:                                # %land.lhs.true47
                                        #   in Loop: Header=BB80_4 Depth=1
	cmpq	$0, 24(%rax)
	jne	.LBB80_27
# BB#24:                                # %if.then49
                                        #   in Loop: Header=BB80_4 Depth=1
	testq	%r12, %r12
	je	.LBB80_28
# BB#25:                                # %if.then.i
                                        #   in Loop: Header=BB80_4 Depth=1
	cmpq	%r12, %rax
	je	.LBB80_33
# BB#26:                                # %if.end.i
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	%r12, 24(%rax)
	movq	32(%r12), %rcx
	movq	%rcx, 40(%rax)
	movq	%rax, 32(%r12)
	jmp	.LBB80_28
	.align	16, 0x90
.LBB80_10:                              # %lor.lhs.false14
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	48(%rbx), %rdx
	testq	$4194304, %rdx          # imm = 0x400000
	je	.LBB80_27
# BB#11:                                # %land.lhs.true20
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	8(%rbx), %rax
	movl	16(%rax), %ecx
	movq	%rcx, %rsi
	andq	$254, %rsi
	cmpq	$20, %rsi
	je	.LBB80_14
# BB#12:                                # %land.lhs.true20
                                        #   in Loop: Header=BB80_4 Depth=1
	andq	$255, %rcx
	cmpq	$22, %rcx
	je	.LBB80_14
# BB#13:                                # %land.lhs.true20
                                        #   in Loop: Header=BB80_4 Depth=1
	cmpq	$10, %rcx
	jne	.LBB80_27
.LBB80_14:                              # %land.lhs.true24
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	(%rax), %rsi
	cmpq	%rsi, %rbx
	je	.LBB80_21
# BB#15:                                # %lor.lhs.false29
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	112(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB80_27
# BB#16:                                # %land.lhs.true32
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	$15, %rcx
	shrq	%cl, %rdx
	cmpq	%rbx, %rdi
	movq	%r13, %rcx
	jne	.LBB80_18
# BB#17:                                # %land.lhs.true32
                                        #   in Loop: Header=BB80_4 Depth=1
	movabsq	$1, %rcx
.LBB80_18:                              # %land.lhs.true32
                                        #   in Loop: Header=BB80_4 Depth=1
	xorq	%r8, %r8
	testq	%rcx, %rdx
	jne	.LBB80_20
# BB#19:                                # %land.lhs.true32
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	%rdi, %r8
.LBB80_20:                              # %land.lhs.true32
                                        #   in Loop: Header=BB80_4 Depth=1
	cmpq	%rsi, %r8
	je	.LBB80_21
	.align	16, 0x90
.LBB80_27:                              # %if.else50
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	gen_decl_die
.LBB80_28:                              # %for.inc
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB80_4
.LBB80_29:                              # %for.end
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB80_32
# BB#30:                                # %for.body58.lr.ph
	incq	%r14
	.align	16, 0x90
.LBB80_31:                              # %for.body58
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	gen_block_die
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB80_31
.LBB80_32:                              # %for.end62
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB80_33:                              # %if.then3.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp963:
	.size	decls_for_scope, .Ltmp963-decls_for_scope
	.cfi_endproc

	.align	16, 0x90
	.type	gen_block_die,@function
gen_block_die:                          # @gen_block_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp970:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp971:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp972:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp973:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp974:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp975:
	.cfi_def_cfa_offset 80
.Ltmp976:
	.cfi_offset %rbx, -48
.Ltmp977:
	.cfi_offset %r12, -40
.Ltmp978:
	.cfi_offset %r13, -32
.Ltmp979:
	.cfi_offset %r14, -24
.Ltmp980:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB81_67
# BB#1:                                 # %lor.lhs.false
	movl	16(%r14), %eax
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB81_67
# BB#2:                                 # %lor.lhs.false1
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB81_4
# BB#3:                                 # %land.lhs.true
	movzbl	24(%r14), %eax
	testq	$2, %rax
	je	.LBB81_67
.LBB81_4:                               # %if.end
	cmpq	$0, 64(%r14)
	je	.LBB81_8
# BB#5:                                 # %if.then13
	movq	40(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB81_67
# BB#6:                                 # %for.body.lr.ph
	incq	%r12
	.align	16, 0x90
.LBB81_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	gen_block_die
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB81_7
	jmp	.LBB81_67
.LBB81_8:                               # %if.end17
	movl	24(%r14), %eax
	movq	56(%r14), %rdx
	andq	$2, %rax
	movq	$1, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	cmpq	%r14, %rdx
	jne	.LBB81_9
# BB#10:                                # %if.end17
	movabsq	$1, %rcx
	jmp	.LBB81_11
.LBB81_9:
	movabsq	$0, %rcx
.LBB81_11:                              # %if.end17
	testq	%rcx, %rsi
	movq	%r14, %rdi
	jne	.LBB81_19
# BB#12:                                # %if.end17
	testq	%rdx, %rdx
	movq	%r14, %rdi
	je	.LBB81_19
	.align	16, 0x90
.LBB81_13:                              # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rcx
	movzbl	16(%rcx), %edx
	cmpq	$4, %rdx
	jne	.LBB81_17
# BB#14:                                # %cond.end.i
                                        #   in Loop: Header=BB81_13 Depth=1
	movq	56(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB81_15
# BB#16:                                # %cond.end.i
                                        #   in Loop: Header=BB81_13 Depth=1
	cmpq	%rcx, %rdx
	movq	%rcx, %rdi
	jne	.LBB81_13
	jmp	.LBB81_19
.LBB81_17:                              # %block_ultimate_origin.exit
	testq	%rcx, %rcx
	movq	%r14, %rdi
	je	.LBB81_19
# BB#18:                                # %cond.end
	cmpq	$30, %rdx
	movq	%rcx, %rdi
	jne	.LBB81_19
# BB#25:                                # %if.then65
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB81_63
# BB#26:                                # %if.then.i
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r15
	movq	$29, %rax
	movl	%eax, (%r15)
	testq	%r13, %r13
	je	.LBB81_32
# BB#27:                                # %if.then.i.i
	testq	%r15, %r15
	je	.LBB81_33
# BB#28:                                # %if.then.i.i.i
	cmpq	%r13, %r15
	je	.LBB81_29
# BB#31:                                # %if.end.i.i.i
	movq	%r13, 24(%r15)
	movq	32(%r13), %rax
	movq	%rax, 40(%r15)
	movq	%r15, 32(%r13)
	jmp	.LBB81_33
.LBB81_15:
	movq	%rcx, %rdi
.LBB81_19:                              # %if.else
	callq	is_body_block
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB81_65
# BB#20:                                # %if.then31
	movl	debug_info_level(%rip), %ecx
	movq	32(%r14), %rax
	cmpq	$1, %rcx
	jbe	.LBB81_21
# BB#64:                                # %if.then33
	testq	%rax, %rax
	jne	.LBB81_63
	jmp	.LBB81_65
.LBB81_24:                              # %for.inc54
                                        #   in Loop: Header=BB81_21 Depth=1
	movq	(%rax), %rax
.LBB81_21:                              # %for.cond39.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB81_65
# BB#22:                                # %for.body42
                                        #   in Loop: Header=BB81_21 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB81_24
# BB#23:                                # %land.lhs.true49
                                        #   in Loop: Header=BB81_21 Depth=1
	cmpq	$0, 104(%rax)
	je	.LBB81_24
.LBB81_63:                              # %if.else66
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	gen_lexical_block_die
	jmp	.LBB81_67
.LBB81_65:                              # %if.else68
	movq	%r14, %rdi
	movq	%r13, %rsi
.LBB81_66:                              # %if.end69
	movq	%r12, %rdx
	callq	decls_for_scope
.LBB81_67:                              # %if.end69
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB81_32:                              # %if.else.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB81_33:                              # %new_die.exit.i
	movzbl	24(%r14), %edx
	movq	56(%r14), %rax
	movq	$1, %rcx
	shrq	%cl, %rdx
	cmpq	%r14, %rax
	jne	.LBB81_34
# BB#35:                                # %new_die.exit.i
	movabsq	$1, %rcx
	jmp	.LBB81_36
.LBB81_34:
	movabsq	$0, %rcx
.LBB81_36:                              # %new_die.exit.i
	xorq	%rbx, %rbx
	testq	%rcx, %rdx
	jne	.LBB81_41
# BB#37:                                # %new_die.exit.i
	testq	%rax, %rax
	je	.LBB81_41
.LBB81_38:                              # %do.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$4, %rax
	jne	.LBB81_41
# BB#39:                                # %cond.end.i.i
                                        #   in Loop: Header=BB81_38 Depth=1
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.LBB81_41
# BB#40:                                # %cond.end.i.i
                                        #   in Loop: Header=BB81_38 Depth=1
	cmpq	%rbx, %rax
	jne	.LBB81_38
.LBB81_41:                              # %block_ultimate_origin.exit.i
	movq	%rbx, %rdi
	callq	dwarf2out_abstract_function
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	je	.LBB81_46
# BB#42:                                # %if.then.i103
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	movq	%rbx, %rdi
	jne	.LBB81_44
# BB#43:                                # %if.then7.i
	movq	(%rbx), %rdi
.LBB81_44:                              # %if.end.i
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB81_46
# BB#45:                                # %if.then9.i
	movq	%rax, %rdi
	callq	dwarf2out_abstract_function
.LBB81_46:                              # %if.end11.i
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB81_47
# BB#50:                                # %if.then32.i
	movq	88(%rbx), %rbx
	jmp	.LBB81_51
.LBB81_47:                              # %if.end11.i
	cmpq	$100, %rax
	jne	.LBB81_52
# BB#48:                                # %if.then21.i
	movl	36(%rbx), %eax
	movl	decl_die_table_in_use(%rip), %ecx
	cmpq	%rax, %rcx
	jbe	.LBB81_52
# BB#49:                                # %cond.true.i.i
	movq	decl_die_table(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
.LBB81_51:                              # %if.end35.i
	testq	%rbx, %rbx
	je	.LBB81_52
# BB#53:                                # %add_abstract_origin_attribute.exit
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$49, %rcx
	movl	%ecx, (%rax)
	movq	$10, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 32(%rax)
	testq	%r15, %r15
	je	.LBB81_56
# BB#54:                                # %add_abstract_origin_attribute.exit
	testq	%rax, %rax
	je	.LBB81_56
# BB#55:                                # %if.then.i.i109
	movq	16(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r15)
.LBB81_56:                              # %add_AT_die_ref.exit
	movl	24(%r14), %ebx
	movq	$2, %rcx
	shrq	%cl, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str19, %rdx
	leaq	(%rsp), %r13
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$17, %rax
	movl	%eax, (%rbx)
	movq	$12, %rax
	movl	%eax, 16(%rbx)
	movq	%r13, %rdi
	callq	xstrdup
	testq	%r15, %r15
	movq	%rax, 24(%rbx)
	je	.LBB81_59
# BB#57:                                # %add_AT_die_ref.exit
	testq	%rbx, %rbx
	je	.LBB81_59
# BB#58:                                # %if.then.i.i32.i
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB81_59:                              # %add_AT_lbl_id.exit.i
	movl	24(%r14), %ebx
	movq	$2, %rcx
	shrq	%cl, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str20, %rdx
	leaq	(%rsp), %r13
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$18, %rax
	movl	%eax, (%rbx)
	movq	$12, %rax
	movl	%eax, 16(%rbx)
	movq	%r13, %rdi
	callq	xstrdup
	testq	%r15, %r15
	movq	%rax, 24(%rbx)
	je	.LBB81_62
# BB#60:                                # %add_AT_lbl_id.exit.i
	testq	%rbx, %rbx
	je	.LBB81_62
# BB#61:                                # %if.then.i.i45.i
	movq	16(%r15), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r15)
.LBB81_62:                              # %add_AT_lbl_id.exit46.i
	movq	%r14, %rdi
	movq	%r15, %rsi
	jmp	.LBB81_66
.LBB81_29:                              # %if.then3.i.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	jmp	.LBB81_30
.LBB81_52:                              # %if.then38.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_abstract_origin_attribute, %rcx
	movq	$9459, %rsi             # imm = 0x24F3
.LBB81_30:                              # %if.then3.i.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp981:
	.size	gen_block_die, .Ltmp981-gen_block_die
	.cfi_endproc

	.align	16, 0x90
	.type	gen_lexical_block_die,@function
gen_lexical_block_die:                  # @gen_lexical_block_die
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp988:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp989:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp990:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp991:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp992:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp993:
	.cfi_def_cfa_offset 96
.Ltmp994:
	.cfi_offset %rbx, -48
.Ltmp995:
	.cfi_offset %r12, -40
.Ltmp996:
	.cfi_offset %r13, -32
.Ltmp997:
	.cfi_offset %r14, -24
.Ltmp998:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r13
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %r12
	testq	%rbx, %rbx
	movq	$11, %rax
	movl	%eax, (%r12)
	je	.LBB82_4
# BB#1:                                 # %if.then.i
	testq	%r12, %r12
	je	.LBB82_5
# BB#2:                                 # %if.then.i.i
	cmpq	%rbx, %r12
	je	.LBB82_30
# BB#3:                                 # %if.end.i.i
	movq	%rbx, 24(%r12)
	movq	32(%rbx), %rax
	movq	%rax, 40(%r12)
	movq	%r12, 32(%rbx)
	jmp	.LBB82_5
.LBB82_4:                               # %if.else.i
	movq	$24, %rdi
	callq	xmalloc
	movq	limbo_die_list(%rip), %rcx
	movq	%r12, (%rax)
	movq	%r13, 8(%rax)
	movq	%rcx, 16(%rax)
	movq	%rax, limbo_die_list(%rip)
.LBB82_5:                               # %new_die.exit
	movl	24(%r13), %ebx
	testq	$2, %rbx
	jne	.LBB82_29
# BB#6:                                 # %if.then
	cmpq	$0, 72(%r13)
	je	.LBB82_23
# BB#7:                                 # %if.then3
	movl	ranges_table_in_use(%rip), %r14d
	movl	ranges_table_allocated(%rip), %eax
	cmpq	%rax, %r14
	jne	.LBB82_9
# BB#8:                                 # %if.then.i45
	leaq	64(%r14), %rax
	movl	%eax, ranges_table_allocated(%rip)
	movq	ranges_table(%rip), %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	$2, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, ranges_table(%rip)
.LBB82_9:                               # %if.end.i
	movq	%r15, 8(%rsp)           # 8-byte Spill
	xorq	%rax, %rax
	testq	%r13, %r13
	je	.LBB82_11
# BB#10:                                # %cond.true.i
	movl	24(%r13), %eax
	movq	$2, %rcx
	shrq	%cl, %rax
.LBB82_11:                              # %add_ranges.exit
	movq	ranges_table(%rip), %rcx
	movl	%eax, (%rcx,%r14,4)
	leaq	1(%r14), %rax
	movl	target_flags(%rip), %ebx
	movl	%eax, ranges_table_in_use(%rip)
	movq	$20, %rcx
	shrq	%cl, %rbx
	andq	$32, %rbx
	addq	$32, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	imulq	%r14, %rbx
	andq	$2147483644, %rbx       # imm = 0x7FFFFFFC
	shlq	$1, %rbx
	movq	$40, %rdi
	callq	xmalloc
	movq	$0, 8(%rax)
	movq	$85, %rcx
	movl	%ecx, (%rax)
	movq	$4, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 24(%rax)
	testq	%r12, %r12
	je	.LBB82_14
# BB#12:                                # %add_ranges.exit
	testq	%rax, %rax
	je	.LBB82_14
# BB#13:                                # %if.then.i.i48
	movq	16(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 16(%r12)
.LBB82_14:                              # %add_AT_range_list.exit
	movq	72(%r13), %r14
	movq	%r13, (%rsp)            # 8-byte Spill
	movl	ranges_table_in_use(%rip), %ebx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB82_15:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movl	ranges_table_allocated(%rip), %eax
	movq	%rbx, %r15
                                        # kill: RBX<def> R15<kill>
	andq	%r13, %rbx
	cmpq	%rax, %rbx
	jne	.LBB82_17
# BB#16:                                # %if.then.i54
                                        #   in Loop: Header=BB82_15 Depth=1
	leaq	64(%r15), %rsi
	movl	%esi, ranges_table_allocated(%rip)
	movq	ranges_table(%rip), %rdi
	andq	%r13, %rsi
	movq	$2, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, ranges_table(%rip)
.LBB82_17:                              # %if.end.i56
                                        #   in Loop: Header=BB82_15 Depth=1
	xorq	%rdx, %rdx
	testq	%r14, %r14
	je	.LBB82_19
# BB#18:                                # %cond.true.i60
                                        #   in Loop: Header=BB82_15 Depth=1
	movl	24(%r14), %edx
	movq	$2, %rcx
	shrq	%cl, %rdx
.LBB82_19:                              # %add_ranges.exit69
                                        #   in Loop: Header=BB82_15 Depth=1
	movq	ranges_table(%rip), %rax
	movl	%edx, (%rax,%rbx,4)
	leaq	1(%r15), %rbx
	movl	%ebx, ranges_table_in_use(%rip)
	movq	72(%r14), %r14
	testq	%r14, %r14
	jne	.LBB82_15
# BB#20:                                # %do.end
	movl	ranges_table_allocated(%rip), %ecx
	andq	%r13, %rbx
	cmpq	%rcx, %rbx
	jne	.LBB82_22
# BB#21:                                # %if.then.i75
	leaq	65(%r15), %rcx
	movl	%ecx, ranges_table_allocated(%rip)
	andq	%rcx, %r13
	movq	$2, %rcx
	shlq	%cl, %r13
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	xrealloc
	movq	%rax, ranges_table(%rip)
.LBB82_22:                              # %add_ranges.exit85
	movq	(%rsp), %r13            # 8-byte Reload
	xorq	%rcx, %rcx
	movl	%ecx, (%rax,%rbx,4)
	addq	$2, %r15
	movl	%r15d, ranges_table_in_use(%rip)
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB82_29
.LBB82_23:                              # %do.body12
	movq	%r13, %r14
	movq	$2, %rcx
	shrq	%cl, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str19, %rdx
	leaq	16(%rsp), %r13
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$17, %rax
	movl	%eax, (%rbx)
	movq	$12, %rax
	movl	%eax, 16(%rbx)
	movq	%r13, %rdi
	callq	xstrdup
	movq	%rax, 24(%rbx)
	testq	%r12, %r12
	je	.LBB82_26
# BB#24:                                # %do.body12
	testq	%rbx, %rbx
	je	.LBB82_26
# BB#25:                                # %if.then.i.i97
	movq	16(%r12), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r12)
.LBB82_26:                              # %add_AT_lbl_id.exit
	movl	24(%r14), %ebx
	movq	$2, %rcx
	shrq	%cl, %rbx
	movabsq	$.L.str1, %rsi
	movabsq	$.L.str20, %rdx
	leaq	16(%rsp), %r13
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	sprintf
	movq	$40, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	$0, 8(%rbx)
	movq	$18, %rax
	movl	%eax, (%rbx)
	movq	$12, %rax
	movl	%eax, 16(%rbx)
	movq	%r13, %rdi
	movq	%r14, %r13
	callq	xstrdup
	testq	%r12, %r12
	movq	%rax, 24(%rbx)
	je	.LBB82_29
# BB#27:                                # %add_AT_lbl_id.exit
	testq	%rbx, %rbx
	je	.LBB82_29
# BB#28:                                # %if.then.i.i110
	movq	16(%r12), %rax
	movq	%rax, 8(%rbx)
	movq	%rbx, 16(%r12)
.LBB82_29:                              # %if.end30
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	decls_for_scope
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB82_30:                              # %if.then3.i.i
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.add_child_die, %rcx
	movq	$5026, %rsi             # imm = 0x13A2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp999:
	.size	gen_lexical_block_die, .Ltmp999-gen_lexical_block_die
	.cfi_endproc

	.align	16, 0x90
	.type	output_cfi,@function
output_cfi:                             # @output_cfi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1004:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1005:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp1006:
	.cfi_def_cfa_offset 32
.Ltmp1007:
	.cfi_offset %rbx, -32
.Ltmp1008:
	.cfi_offset %r14, -24
.Ltmp1009:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	8(%rbx), %r8d
	movq	%rdx, %r15
	movq	%rsi, %r14
	cmpq	$128, %r8
	jne	.LBB83_1
# BB#4:                                 # %if.then7
	movq	16(%rbx), %rcx
	movq	%rcx, %rsi
	andq	$63, %rsi
	orq	$128, %rsi
	movabsq	$.L.str467, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movq	24(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
.LBB83_50:                              # %sw.bb84
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	dw2_asm_output_data_uleb128  # TAILCALL
.LBB83_1:                               # %entry
	cmpq	$64, %r8
	jne	.LBB83_5
# BB#2:                                 # %if.then
	movq	16(%rbx), %rcx
	movq	%rcx, %rsi
	andq	$63, %rsi
	orq	$64, %rsi
	movabsq	$.L.str466, %rdx
	jmp	.LBB83_3
.LBB83_5:                               # %if.else16
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$191, %rax
	jg	.LBB83_8
# BB#6:                                 # %if.else16
	cmpq	$47, %r8
	ja	.LBB83_33
# BB#7:                                 # %if.else16
	movabsq	$.L.str472, %rcx
	jmpq	*.LJTI83_0(,%r8,8)
.LBB83_10:                              # %sw.bb4.i
	movabsq	$.L.str473, %rcx
	jmp	.LBB83_34
.LBB83_8:                               # %if.else16
	cmpq	$192, %r8
	jne	.LBB83_33
# BB#9:                                 # %if.then20
	movq	16(%rbx), %rcx
	movq	%rcx, %rsi
	andq	$63, %rsi
	orq	%r8, %rsi
	movabsq	$.L.str468, %rdx
.LBB83_3:                               # %if.then
	movq	$1, %rdi
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	dw2_asm_output_data  # TAILCALL
.LBB83_33:                              # %sw.default.i
	movabsq	$.L.str496, %rcx
.LBB83_34:                              # %dwarf_cfi_name.exit
	movabsq	$.L.str213, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r8, %rsi
	callq	dw2_asm_output_data
	movl	8(%rbx), %eax
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$46, %rax
	jbe	.LBB83_35
.LBB83_43:                              # %if.end102
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB83_11:                              # %sw.bb5.i
	movabsq	$.L.str474, %rcx
	jmp	.LBB83_34
.LBB83_12:                              # %sw.bb6.i
	movabsq	$.L.str475, %rcx
	jmp	.LBB83_34
.LBB83_13:                              # %sw.bb7.i
	movabsq	$.L.str476, %rcx
	jmp	.LBB83_34
.LBB83_14:                              # %sw.bb8.i
	movabsq	$.L.str477, %rcx
	jmp	.LBB83_34
.LBB83_15:                              # %sw.bb9.i
	movabsq	$.L.str478, %rcx
	jmp	.LBB83_34
.LBB83_16:                              # %sw.bb10.i
	movabsq	$.L.str479, %rcx
	jmp	.LBB83_34
.LBB83_17:                              # %sw.bb11.i
	movabsq	$.L.str480, %rcx
	jmp	.LBB83_34
.LBB83_18:                              # %sw.bb12.i
	movabsq	$.L.str481, %rcx
	jmp	.LBB83_34
.LBB83_19:                              # %sw.bb13.i
	movabsq	$.L.str482, %rcx
	jmp	.LBB83_34
.LBB83_20:                              # %sw.bb14.i
	movabsq	$.L.str483, %rcx
	jmp	.LBB83_34
.LBB83_21:                              # %sw.bb15.i
	movabsq	$.L.str484, %rcx
	jmp	.LBB83_34
.LBB83_22:                              # %sw.bb16.i
	movabsq	$.L.str485, %rcx
	jmp	.LBB83_34
.LBB83_23:                              # %sw.bb17.i
	movabsq	$.L.str486, %rcx
	jmp	.LBB83_34
.LBB83_24:                              # %sw.bb18.i
	movabsq	$.L.str487, %rcx
	jmp	.LBB83_34
.LBB83_25:                              # %sw.bb19.i
	movabsq	$.L.str488, %rcx
	jmp	.LBB83_34
.LBB83_26:                              # %sw.bb20.i
	movabsq	$.L.str489, %rcx
	jmp	.LBB83_34
.LBB83_27:                              # %sw.bb21.i
	movabsq	$.L.str490, %rcx
	jmp	.LBB83_34
.LBB83_28:                              # %sw.bb22.i
	movabsq	$.L.str491, %rcx
	jmp	.LBB83_34
.LBB83_29:                              # %sw.bb23.i
	movabsq	$.L.str492, %rcx
	jmp	.LBB83_34
.LBB83_30:                              # %sw.bb24.i
	movabsq	$.L.str493, %rcx
	jmp	.LBB83_34
.LBB83_31:                              # %sw.bb25.i
	movabsq	$.L.str494, %rcx
	jmp	.LBB83_34
.LBB83_32:                              # %sw.bb26.i
	movabsq	$.L.str495, %rcx
	jmp	.LBB83_34
.LBB83_35:                              # %dwarf_cfi_name.exit
	jmpq	*.LJTI83_1(,%rax,8)
.LBB83_48:                              # %sw.bb84
	movq	16(%rbx), %rcx
	jmp	.LBB83_49
.LBB83_36:                              # %sw.bb
	testq	%rcx, %r15
	je	.LBB83_40
# BB#37:                                # %if.then34
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	movq	$27, %r14
	jne	.LBB83_39
# BB#38:                                # %select.mid
	xorq	%r14, %r14
.LBB83_39:                              # %select.end
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	movq	16(%rbx), %rdx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	$68, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_fmt_s
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	dw2_asm_output_encoded_addr_rtx  # TAILCALL
.LBB83_41:                              # %sw.bb47
	movq	16(%rbx), %rdx
	movq	8(%r14), %r8
	movq	$1, %rdi
	jmp	.LBB83_42
.LBB83_44:                              # %sw.bb53
	movq	16(%rbx), %rdx
	movq	8(%r14), %r8
	movq	$2, %rdi
	jmp	.LBB83_42
.LBB83_45:                              # %sw.bb60
	movq	16(%rbx), %rdx
	movq	8(%r14), %r8
	movq	$4, %rdi
	jmp	.LBB83_42
.LBB83_51:                              # %sw.bb74
	movq	16(%rbx), %rcx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	dw2_asm_output_data_uleb128
	movq	24(%rbx), %rcx
.LBB83_49:                              # %sw.bb84
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	jmp	.LBB83_50
.LBB83_54:                              # %sw.bb98
	movq	16(%rbx), %rbx
	movq	%rbx, %rdi
	callq	size_of_locs
	movq	%rax, %rcx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	dw2_asm_output_data_uleb128
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	output_loc_sequence  # TAILCALL
.LBB83_47:                              # %sw.bb79
	movq	16(%rbx), %rcx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	dw2_asm_output_data_uleb128
	movq	24(%rbx), %rcx
	jmp	.LBB83_53
.LBB83_52:                              # %sw.bb95
	movq	16(%rbx), %rcx
.LBB83_53:                              # %sw.bb95
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	dw2_asm_output_data_sleb128  # TAILCALL
.LBB83_46:                              # %sw.bb67
	movq	16(%rbx), %rdx
	movq	8(%r14), %r8
	movq	$8, %rdi
.LBB83_42:                              # %if.end102
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rdx, %rsi
	movq	%r8, %rdx
	callq	dw2_asm_output_delta
	movq	16(%rbx), %rax
	movq	%rax, 8(%r14)
	jmp	.LBB83_43
.LBB83_55:                              # %sw.bb99
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.output_cfi, %rcx
	movq	$1772, %rsi             # imm = 0x6EC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB83_40:                              # %if.else41
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	movq	16(%rbx), %rbx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	dw2_asm_output_addr  # TAILCALL
.Ltmp1010:
	.size	output_cfi, .Ltmp1010-output_cfi
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI83_0:
	.quad	.LBB83_34
	.quad	.LBB83_10
	.quad	.LBB83_11
	.quad	.LBB83_12
	.quad	.LBB83_13
	.quad	.LBB83_14
	.quad	.LBB83_15
	.quad	.LBB83_16
	.quad	.LBB83_17
	.quad	.LBB83_18
	.quad	.LBB83_19
	.quad	.LBB83_20
	.quad	.LBB83_21
	.quad	.LBB83_22
	.quad	.LBB83_23
	.quad	.LBB83_24
	.quad	.LBB83_25
	.quad	.LBB83_26
	.quad	.LBB83_27
	.quad	.LBB83_28
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_29
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_33
	.quad	.LBB83_30
	.quad	.LBB83_31
	.quad	.LBB83_32
.LJTI83_1:
	.quad	.LBB83_36
	.quad	.LBB83_41
	.quad	.LBB83_44
	.quad	.LBB83_45
	.quad	.LBB83_51
	.quad	.LBB83_48
	.quad	.LBB83_48
	.quad	.LBB83_48
	.quad	.LBB83_51
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_51
	.quad	.LBB83_48
	.quad	.LBB83_48
	.quad	.LBB83_54
	.quad	.LBB83_54
	.quad	.LBB83_47
	.quad	.LBB83_47
	.quad	.LBB83_52
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_46
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_43
	.quad	.LBB83_48
	.quad	.LBB83_55

	.text
	.align	16, 0x90
	.type	stack_adjust_offset,@function
stack_adjust_offset:                    # @stack_adjust_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1012:
	.cfi_def_cfa_offset 16
	movq	8(%rdi), %rsi
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rsi
	je	.LBB84_1
# BB#5:                                 # %if.else
	movl	(%rsi), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rdx
	jne	.LBB84_17
# BB#6:                                 # %if.then27
	movq	8(%rsi), %rsi
	movl	(%rsi), %edx
	movq	%rdx, %r8
	andq	$65535, %r8             # imm = 0xFFFF
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$96, %rdx
	je	.LBB84_8
# BB#7:                                 # %if.then27
	cmpq	$100, %r8
	jne	.LBB84_17
.LBB84_8:                               # %lor.lhs.false37
	movq	8(%rsi), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB84_17
# BB#9:                                 # %if.end43
	cmpq	$100, %r8
	jne	.LBB84_13
# BB#10:                                # %if.then45
	movq	16(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB84_18
# BB#11:                                # %lor.lhs.false58
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB84_18
# BB#12:                                # %if.end74.thread
	movq	8(%rax), %rax
	jmp	.LBB84_16
.LBB84_1:                               # %if.then
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %r8d
	leaq	-75(%r8), %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	xorq	%rax, %rax
	cmpq	$1, %rdi
	ja	.LBB84_17
# BB#2:                                 # %lor.lhs.false6
	movq	8(%rcx), %rdi
	cmpq	%rsi, %rdi
	jne	.LBB84_17
# BB#3:                                 # %lor.lhs.false11
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$54, %rsi
	jne	.LBB84_17
# BB#4:                                 # %if.end
	movq	8(%rcx), %rax
	jmp	.LBB84_14
.LBB84_13:                              # %if.else67
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %eax
.LBB84_14:                              # %if.end74
	cmpq	$75, %r8
	je	.LBB84_16
# BB#15:                                # %if.end74
	cmpq	$97, %r8
	jne	.LBB84_17
.LBB84_16:                              # %if.then80
	negq	%rax
.LBB84_17:                              # %return
	popq	%rdx
	retq
.LBB84_18:                              # %if.then62
	movabsq	$.L.str4, %rax
	movabsq	$.L__FUNCTION__.stack_adjust_offset, %rcx
	movq	$972, %rsi              # imm = 0x3CC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp1013:
	.size	stack_adjust_offset, .Ltmp1013-stack_adjust_offset
	.cfi_endproc

	.type	current_funcdef_number,@object # @current_funcdef_number
	.bss
	.globl	current_funcdef_number
	.align	4
current_funcdef_number:
	.long	0                       # 0x0
	.size	current_funcdef_number, 4

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	".eh_frame"
	.size	.L.str, 10

	.type	dwarf2out_cfi_label.label,@object # @dwarf2out_cfi_label.label
	.local	dwarf2out_cfi_label.label
	.comm	dwarf2out_cfi_label.label,20,16
	.type	dwarf2out_cfi_label.label_num,@object # @dwarf2out_cfi_label.label_num
	.local	dwarf2out_cfi_label.label_num
	.comm	dwarf2out_cfi_label.label_num,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"*.%s%u"
	.size	.L.str1, 7

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"LCFI"
	.size	.L.str2, 5

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	":\n"
	.size	.L.str3, 3

	.type	old_args_size,@object   # @old_args_size
	.local	old_args_size
	.comm	old_args_size,8,8
	.type	cfa,@object             # @cfa
	.local	cfa
	.comm	cfa,32,8
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"dwarf2out.c"
	.size	.L.str4, 12

	.type	.L__FUNCTION__.dwarf2out_frame_debug,@object # @__FUNCTION__.dwarf2out_frame_debug
.L__FUNCTION__.dwarf2out_frame_debug:
	.asciz	"dwarf2out_frame_debug"
	.size	.L__FUNCTION__.dwarf2out_frame_debug, 22

	.type	cfa_store,@object       # @cfa_store
	.local	cfa_store
	.comm	cfa_store,32,8
	.type	cfa_temp.0,@object      # @cfa_temp.0
	.local	cfa_temp.0
	.comm	cfa_temp.0,8,8
	.type	cfa_temp.1,@object      # @cfa_temp.1
	.local	cfa_temp.1
	.comm	cfa_temp.1,8,8
	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"LFB"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	".%s%u:\n"
	.size	.L.str6, 8

	.type	fde_table_in_use,@object # @fde_table_in_use
	.local	fde_table_in_use
	.comm	fde_table_in_use,4,4
	.type	fde_table_allocated,@object # @fde_table_allocated
	.local	fde_table_allocated
	.comm	fde_table_allocated,4,4
	.type	fde_table,@object       # @fde_table
	.local	fde_table
	.comm	fde_table,8,8
	.type	args_size,@object       # @args_size
	.local	args_size
	.comm	args_size,8,8
	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"LFE"
	.size	.L.str7, 4

	.type	dwarf2_debug_hooks,@object # @dwarf2_debug_hooks
	.data
	.globl	dwarf2_debug_hooks
	.align	8
dwarf2_debug_hooks:
	.quad	dwarf2out_init
	.quad	dwarf2out_finish
	.quad	dwarf2out_define
	.quad	dwarf2out_undef
	.quad	dwarf2out_start_source_file
	.quad	dwarf2out_end_source_file
	.quad	dwarf2out_begin_block
	.quad	dwarf2out_end_block
	.quad	dwarf2out_ignore_block
	.quad	dwarf2out_source_line
	.quad	dwarf2out_begin_prologue
	.quad	debug_nothing_int
	.quad	dwarf2out_end_epilogue
	.quad	debug_nothing_tree
	.quad	debug_nothing_int
	.quad	dwarf2out_decl
	.quad	dwarf2out_global_decl
	.quad	debug_nothing_tree
	.quad	dwarf2out_abstract_function
	.quad	debug_nothing_rtx
	.size	dwarf2_debug_hooks, 160

	.type	demangle_name_func,@object # @demangle_name_func
	.local	demangle_name_func
	.comm	demangle_name_func,8,8
	.type	print_indent,@object    # @print_indent
	.local	print_indent
	.comm	print_indent,4,4
	.type	comp_unit_die,@object   # @comp_unit_die
	.local	comp_unit_die
	.comm	comp_unit_die,8,8
	.type	debug_str_hash,@object  # @debug_str_hash
	.comm	debug_str_hash,8,8
	.type	decl_scope_table,@object # @decl_scope_table
	.comm	decl_scope_table,8,8
	.type	incomplete_types,@object # @incomplete_types
	.comm	incomplete_types,8,8
	.type	limbo_die_list,@object  # @limbo_die_list
	.local	limbo_die_list
	.comm	limbo_die_list,8,8
	.type	.L__FUNCTION__.dwarf2out_finish,@object # @__FUNCTION__.dwarf2out_finish
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.dwarf2out_finish:
	.asciz	"dwarf2out_finish"
	.size	.L__FUNCTION__.dwarf2out_finish, 17

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Letext"
	.size	.L.str8, 7

	.type	loc_section_label,@object # @loc_section_label
	.local	loc_section_label
	.comm	loc_section_label,30,16
	.type	separate_line_info_table_in_use,@object # @separate_line_info_table_in_use
	.local	separate_line_info_table_in_use
	.comm	separate_line_info_table_in_use,4,4
	.type	text_section_label,@object # @text_section_label
	.local	text_section_label
	.comm	text_section_label,30,16
	.type	text_end_label,@object  # @text_end_label
	.local	text_end_label
	.comm	text_end_label,30,16
	.type	ranges_table_in_use,@object # @ranges_table_in_use
	.local	ranges_table_in_use
	.comm	ranges_table_in_use,4,4
	.type	debug_line_section_label,@object # @debug_line_section_label
	.local	debug_line_section_label
	.comm	debug_line_section_label,30,16
	.type	macinfo_section_label,@object # @macinfo_section_label
	.local	macinfo_section_label
	.comm	macinfo_section_label,30,16
	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	".debug_abbrev"
	.size	.L.str11, 14

	.type	pubname_table_in_use,@object # @pubname_table_in_use
	.local	pubname_table_in_use
	.comm	pubname_table_in_use,4,4
	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	".debug_pubnames"
	.size	.L.str12, 16

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	".debug_aranges"
	.size	.L.str13, 15

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	".debug_ranges"
	.size	.L.str14, 14

	.type	ranges_section_label,@object # @ranges_section_label
	.local	ranges_section_label
	.comm	ranges_section_label,60,16
	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	".debug_macinfo"
	.size	.L.str15, 15

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"End file"
	.size	.L.str16, 9

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	".debug_str"
	.size	.L.str17, 11

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Offset 0x%x"
	.size	.L.str18, 12

	.type	ranges_table,@object    # @ranges_table
	.local	ranges_table
	.comm	ranges_table,8,8
	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"LBB"
	.size	.L.str19, 4

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"LBE"
	.size	.L.str20, 4

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Length of Address Ranges Info"
	.size	.L.str21, 30

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"DWARF Version"
	.size	.L.str22, 14

	.type	debug_info_section_label,@object # @debug_info_section_label
	.local	debug_info_section_label
	.comm	debug_info_section_label,30,16
	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Offset of Compilation Unit Info"
	.size	.L.str23, 32

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Size of Address"
	.size	.L.str24, 16

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Size of Segment Descriptor"
	.size	.L.str25, 27

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Pad to %d byte boundary"
	.size	.L.str26, 24

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Address"
	.size	.L.str27, 8

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Length"
	.size	.L.str28, 7

	.type	arange_table_in_use,@object # @arange_table_in_use
	.local	arange_table_in_use
	.comm	arange_table_in_use,4,4
	.type	arange_table,@object    # @arange_table
	.local	arange_table
	.comm	arange_table,8,8
	.type	.L__FUNCTION__.output_aranges,@object # @__FUNCTION__.output_aranges
.L__FUNCTION__.output_aranges:
	.asciz	"output_aranges"
	.size	.L__FUNCTION__.output_aranges, 15

	.type	.L__FUNCTION__.AT_loc,@object # @__FUNCTION__.AT_loc
.L__FUNCTION__.AT_loc:
	.asciz	"AT_loc"
	.size	.L__FUNCTION__.AT_loc, 7

	.type	.L__FUNCTION__.AT_ref,@object # @__FUNCTION__.AT_ref
.L__FUNCTION__.AT_ref:
	.asciz	"AT_ref"
	.size	.L__FUNCTION__.AT_ref, 7

	.type	.L__FUNCTION__.AT_lbl,@object # @__FUNCTION__.AT_lbl
.L__FUNCTION__.AT_lbl:
	.asciz	"AT_lbl"
	.size	.L__FUNCTION__.AT_lbl, 7

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Length of Public Names Info"
	.size	.L.str29, 28

	.type	next_die_offset,@object # @next_die_offset
	.local	next_die_offset
	.comm	next_die_offset,8,8
	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Compilation Unit Length"
	.size	.L.str30, 24

	.type	pubname_table,@object   # @pubname_table
	.local	pubname_table
	.comm	pubname_table,8,8
	.type	.L__FUNCTION__.output_pubnames,@object # @__FUNCTION__.output_pubnames
.L__FUNCTION__.output_pubnames:
	.asciz	"output_pubnames"
	.size	.L__FUNCTION__.output_pubnames, 16

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"DIE offset"
	.size	.L.str31, 11

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"external name"
	.size	.L.str32, 14

	.type	abbrev_die_table_in_use,@object # @abbrev_die_table_in_use
	.local	abbrev_die_table_in_use
	.comm	abbrev_die_table_in_use,4,4
	.type	abbrev_die_table,@object # @abbrev_die_table
	.local	abbrev_die_table
	.comm	abbrev_die_table,8,8
	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"(abbrev code)"
	.size	.L.str33, 14

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"(TAG: %s)"
	.size	.L.str34, 10

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"DW_children_yes"
	.size	.L.str35, 16

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"DW_children_no"
	.size	.L.str36, 15

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"(%s)"
	.size	.L.str37, 5

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"DW_FORM_addr"
	.size	.L.str38, 13

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"DW_FORM_block2"
	.size	.L.str39, 15

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"DW_FORM_block4"
	.size	.L.str40, 15

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"DW_FORM_data2"
	.size	.L.str41, 14

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"DW_FORM_data4"
	.size	.L.str42, 14

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"DW_FORM_data8"
	.size	.L.str43, 14

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"DW_FORM_string"
	.size	.L.str44, 15

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"DW_FORM_block"
	.size	.L.str45, 14

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"DW_FORM_block1"
	.size	.L.str46, 15

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"DW_FORM_data1"
	.size	.L.str47, 14

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"DW_FORM_flag"
	.size	.L.str48, 13

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"DW_FORM_sdata"
	.size	.L.str49, 14

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"DW_FORM_strp"
	.size	.L.str50, 13

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"DW_FORM_udata"
	.size	.L.str51, 14

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"DW_FORM_ref_addr"
	.size	.L.str52, 17

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"DW_FORM_ref1"
	.size	.L.str53, 13

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"DW_FORM_ref2"
	.size	.L.str54, 13

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"DW_FORM_ref4"
	.size	.L.str55, 13

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"DW_FORM_ref8"
	.size	.L.str56, 13

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"DW_FORM_ref_udata"
	.size	.L.str57, 18

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"DW_FORM_indirect"
	.size	.L.str58, 17

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"DW_FORM_<unknown>"
	.size	.L.str59, 18

	.type	.L__FUNCTION__.value_format,@object # @__FUNCTION__.value_format
.L__FUNCTION__.value_format:
	.asciz	"value_format"
	.size	.L__FUNCTION__.value_format, 13

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"LC"
	.size	.L.str60, 3

	.type	.L__FUNCTION__.AT_unsigned,@object # @__FUNCTION__.AT_unsigned
.L__FUNCTION__.AT_unsigned:
	.asciz	"AT_unsigned"
	.size	.L__FUNCTION__.AT_unsigned, 12

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"DW_AT_sibling"
	.size	.L.str61, 14

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"DW_AT_location"
	.size	.L.str62, 15

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"DW_AT_name"
	.size	.L.str63, 11

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"DW_AT_ordering"
	.size	.L.str64, 15

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"DW_AT_subscr_data"
	.size	.L.str65, 18

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"DW_AT_byte_size"
	.size	.L.str66, 16

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"DW_AT_bit_offset"
	.size	.L.str67, 17

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"DW_AT_bit_size"
	.size	.L.str68, 15

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"DW_AT_element_list"
	.size	.L.str69, 19

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"DW_AT_stmt_list"
	.size	.L.str70, 16

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"DW_AT_low_pc"
	.size	.L.str71, 13

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"DW_AT_high_pc"
	.size	.L.str72, 14

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"DW_AT_language"
	.size	.L.str73, 15

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"DW_AT_member"
	.size	.L.str74, 13

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"DW_AT_discr"
	.size	.L.str75, 12

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"DW_AT_discr_value"
	.size	.L.str76, 18

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"DW_AT_visibility"
	.size	.L.str77, 17

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"DW_AT_import"
	.size	.L.str78, 13

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"DW_AT_string_length"
	.size	.L.str79, 20

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"DW_AT_common_reference"
	.size	.L.str80, 23

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"DW_AT_comp_dir"
	.size	.L.str81, 15

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"DW_AT_const_value"
	.size	.L.str82, 18

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"DW_AT_containing_type"
	.size	.L.str83, 22

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"DW_AT_default_value"
	.size	.L.str84, 20

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"DW_AT_inline"
	.size	.L.str85, 13

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"DW_AT_is_optional"
	.size	.L.str86, 18

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"DW_AT_lower_bound"
	.size	.L.str87, 18

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"DW_AT_producer"
	.size	.L.str88, 15

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"DW_AT_prototyped"
	.size	.L.str89, 17

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"DW_AT_return_addr"
	.size	.L.str90, 18

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"DW_AT_start_scope"
	.size	.L.str91, 18

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"DW_AT_stride_size"
	.size	.L.str92, 18

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"DW_AT_upper_bound"
	.size	.L.str93, 18

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"DW_AT_abstract_origin"
	.size	.L.str94, 22

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"DW_AT_accessibility"
	.size	.L.str95, 20

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"DW_AT_address_class"
	.size	.L.str96, 20

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"DW_AT_artificial"
	.size	.L.str97, 17

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"DW_AT_base_types"
	.size	.L.str98, 17

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"DW_AT_calling_convention"
	.size	.L.str99, 25

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"DW_AT_count"
	.size	.L.str100, 12

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"DW_AT_data_member_location"
	.size	.L.str101, 27

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"DW_AT_decl_column"
	.size	.L.str102, 18

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"DW_AT_decl_file"
	.size	.L.str103, 16

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"DW_AT_decl_line"
	.size	.L.str104, 16

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"DW_AT_declaration"
	.size	.L.str105, 18

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"DW_AT_discr_list"
	.size	.L.str106, 17

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"DW_AT_encoding"
	.size	.L.str107, 15

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"DW_AT_external"
	.size	.L.str108, 15

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"DW_AT_frame_base"
	.size	.L.str109, 17

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"DW_AT_friend"
	.size	.L.str110, 13

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"DW_AT_identifier_case"
	.size	.L.str111, 22

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"DW_AT_macro_info"
	.size	.L.str112, 17

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"DW_AT_namelist_items"
	.size	.L.str113, 21

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"DW_AT_priority"
	.size	.L.str114, 15

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"DW_AT_segment"
	.size	.L.str115, 14

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"DW_AT_specification"
	.size	.L.str116, 20

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"DW_AT_static_link"
	.size	.L.str117, 18

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"DW_AT_type"
	.size	.L.str118, 11

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"DW_AT_use_location"
	.size	.L.str119, 19

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"DW_AT_variable_parameter"
	.size	.L.str120, 25

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"DW_AT_virtuality"
	.size	.L.str121, 17

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"DW_AT_vtable_elem_location"
	.size	.L.str122, 27

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"DW_AT_allocated"
	.size	.L.str123, 16

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"DW_AT_associated"
	.size	.L.str124, 17

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"DW_AT_data_location"
	.size	.L.str125, 20

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"DW_AT_stride"
	.size	.L.str126, 13

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"DW_AT_entry_pc"
	.size	.L.str127, 15

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"DW_AT_use_UTF8"
	.size	.L.str128, 15

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"DW_AT_extension"
	.size	.L.str129, 16

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"DW_AT_ranges"
	.size	.L.str130, 13

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"DW_AT_trampoline"
	.size	.L.str131, 17

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"DW_AT_call_column"
	.size	.L.str132, 18

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"DW_AT_call_file"
	.size	.L.str133, 16

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"DW_AT_call_line"
	.size	.L.str134, 16

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"DW_AT_MIPS_fde"
	.size	.L.str135, 15

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"DW_AT_MIPS_loop_begin"
	.size	.L.str136, 22

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"DW_AT_MIPS_tail_loop_begin"
	.size	.L.str137, 27

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"DW_AT_MIPS_epilog_begin"
	.size	.L.str138, 24

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"DW_AT_MIPS_loop_unroll_factor"
	.size	.L.str139, 30

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"DW_AT_MIPS_software_pipeline_depth"
	.size	.L.str140, 35

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"DW_AT_MIPS_linkage_name"
	.size	.L.str141, 24

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"DW_AT_MIPS_stride"
	.size	.L.str142, 18

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"DW_AT_MIPS_abstract_name"
	.size	.L.str143, 25

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"DW_AT_MIPS_clone_origin"
	.size	.L.str144, 24

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"DW_AT_MIPS_has_inlines"
	.size	.L.str145, 23

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"DW_AT_sf_names"
	.size	.L.str146, 15

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"DW_AT_src_info"
	.size	.L.str147, 15

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"DW_AT_mac_info"
	.size	.L.str148, 15

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"DW_AT_src_coords"
	.size	.L.str149, 17

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"DW_AT_body_begin"
	.size	.L.str150, 17

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"DW_AT_body_end"
	.size	.L.str151, 15

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"DW_AT_GNU_vector"
	.size	.L.str152, 17

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"DW_AT_VMS_rtnbeg_pd_address"
	.size	.L.str153, 28

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"DW_AT_<unknown>"
	.size	.L.str154, 16

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"DW_TAG_padding"
	.size	.L.str155, 15

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"DW_TAG_array_type"
	.size	.L.str156, 18

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"DW_TAG_class_type"
	.size	.L.str157, 18

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"DW_TAG_entry_point"
	.size	.L.str158, 19

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"DW_TAG_enumeration_type"
	.size	.L.str159, 24

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"DW_TAG_formal_parameter"
	.size	.L.str160, 24

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"DW_TAG_imported_declaration"
	.size	.L.str161, 28

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"DW_TAG_label"
	.size	.L.str162, 13

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"DW_TAG_lexical_block"
	.size	.L.str163, 21

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"DW_TAG_member"
	.size	.L.str164, 14

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"DW_TAG_pointer_type"
	.size	.L.str165, 20

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"DW_TAG_reference_type"
	.size	.L.str166, 22

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"DW_TAG_compile_unit"
	.size	.L.str167, 20

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"DW_TAG_string_type"
	.size	.L.str168, 19

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"DW_TAG_structure_type"
	.size	.L.str169, 22

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"DW_TAG_subroutine_type"
	.size	.L.str170, 23

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"DW_TAG_typedef"
	.size	.L.str171, 15

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"DW_TAG_union_type"
	.size	.L.str172, 18

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"DW_TAG_unspecified_parameters"
	.size	.L.str173, 30

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"DW_TAG_variant"
	.size	.L.str174, 15

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"DW_TAG_common_block"
	.size	.L.str175, 20

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"DW_TAG_common_inclusion"
	.size	.L.str176, 24

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"DW_TAG_inheritance"
	.size	.L.str177, 19

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"DW_TAG_inlined_subroutine"
	.size	.L.str178, 26

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"DW_TAG_module"
	.size	.L.str179, 14

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"DW_TAG_ptr_to_member_type"
	.size	.L.str180, 26

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"DW_TAG_set_type"
	.size	.L.str181, 16

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"DW_TAG_subrange_type"
	.size	.L.str182, 21

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"DW_TAG_with_stmt"
	.size	.L.str183, 17

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"DW_TAG_access_declaration"
	.size	.L.str184, 26

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"DW_TAG_base_type"
	.size	.L.str185, 17

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"DW_TAG_catch_block"
	.size	.L.str186, 19

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"DW_TAG_const_type"
	.size	.L.str187, 18

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"DW_TAG_constant"
	.size	.L.str188, 16

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"DW_TAG_enumerator"
	.size	.L.str189, 18

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"DW_TAG_file_type"
	.size	.L.str190, 17

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"DW_TAG_friend"
	.size	.L.str191, 14

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"DW_TAG_namelist"
	.size	.L.str192, 16

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"DW_TAG_namelist_item"
	.size	.L.str193, 21

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"DW_TAG_packed_type"
	.size	.L.str194, 19

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"DW_TAG_subprogram"
	.size	.L.str195, 18

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"DW_TAG_template_type_param"
	.size	.L.str196, 27

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"DW_TAG_template_value_param"
	.size	.L.str197, 28

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"DW_TAG_thrown_type"
	.size	.L.str198, 19

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"DW_TAG_try_block"
	.size	.L.str199, 17

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"DW_TAG_variant_part"
	.size	.L.str200, 20

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"DW_TAG_variable"
	.size	.L.str201, 16

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"DW_TAG_volatile_type"
	.size	.L.str202, 21

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"DW_TAG_MIPS_loop"
	.size	.L.str203, 17

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"DW_TAG_format_label"
	.size	.L.str204, 20

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"DW_TAG_function_template"
	.size	.L.str205, 25

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"DW_TAG_class_template"
	.size	.L.str206, 22

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"DW_TAG_GNU_BINCL"
	.size	.L.str207, 17

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"DW_TAG_GNU_EINCL"
	.size	.L.str208, 17

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"DW_TAG_<unknown>"
	.size	.L.str209, 17

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	".gnu.linkonce.wi.%s"
	.size	.L.str210, 20

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	".debug_info"
	.size	.L.str211, 12

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"(DIE (0x%lx) %s)"
	.size	.L.str212, 17

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"%s"
	.size	.L.str213, 3

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"+0x%lx"
	.size	.L.str214, 7

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"long long constant"
	.size	.L.str215, 19

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"fp constant word %u"
	.size	.L.str216, 20

	.type	.L__FUNCTION__.output_die,@object # @__FUNCTION__.output_die
.L__FUNCTION__.output_die:
	.asciz	"output_die"
	.size	.L__FUNCTION__.output_die, 11

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"LSFDE"
	.size	.L.str217, 6

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"%s: \"%s\""
	.size	.L.str218, 9

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"end of children of DIE 0x%lx"
	.size	.L.str219, 29

	.type	.L__FUNCTION__.AT_string,@object # @__FUNCTION__.AT_string
.L__FUNCTION__.AT_string:
	.asciz	"AT_string"
	.size	.L__FUNCTION__.AT_string, 10

	.type	.L__FUNCTION__.AT_flag,@object # @__FUNCTION__.AT_flag
.L__FUNCTION__.AT_flag:
	.asciz	"AT_flag"
	.size	.L__FUNCTION__.AT_flag, 8

	.type	.L__FUNCTION__.output_loc_operands,@object # @__FUNCTION__.output_loc_operands
.L__FUNCTION__.output_loc_operands:
	.asciz	"output_loc_operands"
	.size	.L__FUNCTION__.output_loc_operands, 20

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"DW_OP_addr"
	.size	.L.str220, 11

	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"DW_OP_deref"
	.size	.L.str221, 12

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"DW_OP_const1u"
	.size	.L.str222, 14

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"DW_OP_const1s"
	.size	.L.str223, 14

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"DW_OP_const2u"
	.size	.L.str224, 14

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"DW_OP_const2s"
	.size	.L.str225, 14

	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"DW_OP_const4u"
	.size	.L.str226, 14

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"DW_OP_const4s"
	.size	.L.str227, 14

	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"DW_OP_const8u"
	.size	.L.str228, 14

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"DW_OP_const8s"
	.size	.L.str229, 14

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"DW_OP_constu"
	.size	.L.str230, 13

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"DW_OP_consts"
	.size	.L.str231, 13

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"DW_OP_dup"
	.size	.L.str232, 10

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"DW_OP_drop"
	.size	.L.str233, 11

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"DW_OP_over"
	.size	.L.str234, 11

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"DW_OP_pick"
	.size	.L.str235, 11

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"DW_OP_swap"
	.size	.L.str236, 11

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"DW_OP_rot"
	.size	.L.str237, 10

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"DW_OP_xderef"
	.size	.L.str238, 13

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"DW_OP_abs"
	.size	.L.str239, 10

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"DW_OP_and"
	.size	.L.str240, 10

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"DW_OP_div"
	.size	.L.str241, 10

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"DW_OP_minus"
	.size	.L.str242, 12

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"DW_OP_mod"
	.size	.L.str243, 10

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"DW_OP_mul"
	.size	.L.str244, 10

	.type	.L.str245,@object       # @.str245
.L.str245:
	.asciz	"DW_OP_neg"
	.size	.L.str245, 10

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"DW_OP_not"
	.size	.L.str246, 10

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"DW_OP_or"
	.size	.L.str247, 9

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"DW_OP_plus"
	.size	.L.str248, 11

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"DW_OP_plus_uconst"
	.size	.L.str249, 18

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	"DW_OP_shl"
	.size	.L.str250, 10

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	"DW_OP_shr"
	.size	.L.str251, 10

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	"DW_OP_shra"
	.size	.L.str252, 11

	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	"DW_OP_xor"
	.size	.L.str253, 10

	.type	.L.str254,@object       # @.str254
.L.str254:
	.asciz	"DW_OP_bra"
	.size	.L.str254, 10

	.type	.L.str255,@object       # @.str255
.L.str255:
	.asciz	"DW_OP_eq"
	.size	.L.str255, 9

	.type	.L.str256,@object       # @.str256
.L.str256:
	.asciz	"DW_OP_ge"
	.size	.L.str256, 9

	.type	.L.str257,@object       # @.str257
.L.str257:
	.asciz	"DW_OP_gt"
	.size	.L.str257, 9

	.type	.L.str258,@object       # @.str258
.L.str258:
	.asciz	"DW_OP_le"
	.size	.L.str258, 9

	.type	.L.str259,@object       # @.str259
.L.str259:
	.asciz	"DW_OP_lt"
	.size	.L.str259, 9

	.type	.L.str260,@object       # @.str260
.L.str260:
	.asciz	"DW_OP_ne"
	.size	.L.str260, 9

	.type	.L.str261,@object       # @.str261
.L.str261:
	.asciz	"DW_OP_skip"
	.size	.L.str261, 11

	.type	.L.str262,@object       # @.str262
.L.str262:
	.asciz	"DW_OP_lit0"
	.size	.L.str262, 11

	.type	.L.str263,@object       # @.str263
.L.str263:
	.asciz	"DW_OP_lit1"
	.size	.L.str263, 11

	.type	.L.str264,@object       # @.str264
.L.str264:
	.asciz	"DW_OP_lit2"
	.size	.L.str264, 11

	.type	.L.str265,@object       # @.str265
.L.str265:
	.asciz	"DW_OP_lit3"
	.size	.L.str265, 11

	.type	.L.str266,@object       # @.str266
.L.str266:
	.asciz	"DW_OP_lit4"
	.size	.L.str266, 11

	.type	.L.str267,@object       # @.str267
.L.str267:
	.asciz	"DW_OP_lit5"
	.size	.L.str267, 11

	.type	.L.str268,@object       # @.str268
.L.str268:
	.asciz	"DW_OP_lit6"
	.size	.L.str268, 11

	.type	.L.str269,@object       # @.str269
.L.str269:
	.asciz	"DW_OP_lit7"
	.size	.L.str269, 11

	.type	.L.str270,@object       # @.str270
.L.str270:
	.asciz	"DW_OP_lit8"
	.size	.L.str270, 11

	.type	.L.str271,@object       # @.str271
.L.str271:
	.asciz	"DW_OP_lit9"
	.size	.L.str271, 11

	.type	.L.str272,@object       # @.str272
.L.str272:
	.asciz	"DW_OP_lit10"
	.size	.L.str272, 12

	.type	.L.str273,@object       # @.str273
.L.str273:
	.asciz	"DW_OP_lit11"
	.size	.L.str273, 12

	.type	.L.str274,@object       # @.str274
.L.str274:
	.asciz	"DW_OP_lit12"
	.size	.L.str274, 12

	.type	.L.str275,@object       # @.str275
.L.str275:
	.asciz	"DW_OP_lit13"
	.size	.L.str275, 12

	.type	.L.str276,@object       # @.str276
.L.str276:
	.asciz	"DW_OP_lit14"
	.size	.L.str276, 12

	.type	.L.str277,@object       # @.str277
.L.str277:
	.asciz	"DW_OP_lit15"
	.size	.L.str277, 12

	.type	.L.str278,@object       # @.str278
.L.str278:
	.asciz	"DW_OP_lit16"
	.size	.L.str278, 12

	.type	.L.str279,@object       # @.str279
.L.str279:
	.asciz	"DW_OP_lit17"
	.size	.L.str279, 12

	.type	.L.str280,@object       # @.str280
.L.str280:
	.asciz	"DW_OP_lit18"
	.size	.L.str280, 12

	.type	.L.str281,@object       # @.str281
.L.str281:
	.asciz	"DW_OP_lit19"
	.size	.L.str281, 12

	.type	.L.str282,@object       # @.str282
.L.str282:
	.asciz	"DW_OP_lit20"
	.size	.L.str282, 12

	.type	.L.str283,@object       # @.str283
.L.str283:
	.asciz	"DW_OP_lit21"
	.size	.L.str283, 12

	.type	.L.str284,@object       # @.str284
.L.str284:
	.asciz	"DW_OP_lit22"
	.size	.L.str284, 12

	.type	.L.str285,@object       # @.str285
.L.str285:
	.asciz	"DW_OP_lit23"
	.size	.L.str285, 12

	.type	.L.str286,@object       # @.str286
.L.str286:
	.asciz	"DW_OP_lit24"
	.size	.L.str286, 12

	.type	.L.str287,@object       # @.str287
.L.str287:
	.asciz	"DW_OP_lit25"
	.size	.L.str287, 12

	.type	.L.str288,@object       # @.str288
.L.str288:
	.asciz	"DW_OP_lit26"
	.size	.L.str288, 12

	.type	.L.str289,@object       # @.str289
.L.str289:
	.asciz	"DW_OP_lit27"
	.size	.L.str289, 12

	.type	.L.str290,@object       # @.str290
.L.str290:
	.asciz	"DW_OP_lit28"
	.size	.L.str290, 12

	.type	.L.str291,@object       # @.str291
.L.str291:
	.asciz	"DW_OP_lit29"
	.size	.L.str291, 12

	.type	.L.str292,@object       # @.str292
.L.str292:
	.asciz	"DW_OP_lit30"
	.size	.L.str292, 12

	.type	.L.str293,@object       # @.str293
.L.str293:
	.asciz	"DW_OP_lit31"
	.size	.L.str293, 12

	.type	.L.str294,@object       # @.str294
.L.str294:
	.asciz	"DW_OP_reg0"
	.size	.L.str294, 11

	.type	.L.str295,@object       # @.str295
.L.str295:
	.asciz	"DW_OP_reg1"
	.size	.L.str295, 11

	.type	.L.str296,@object       # @.str296
.L.str296:
	.asciz	"DW_OP_reg2"
	.size	.L.str296, 11

	.type	.L.str297,@object       # @.str297
.L.str297:
	.asciz	"DW_OP_reg3"
	.size	.L.str297, 11

	.type	.L.str298,@object       # @.str298
.L.str298:
	.asciz	"DW_OP_reg4"
	.size	.L.str298, 11

	.type	.L.str299,@object       # @.str299
.L.str299:
	.asciz	"DW_OP_reg5"
	.size	.L.str299, 11

	.type	.L.str300,@object       # @.str300
.L.str300:
	.asciz	"DW_OP_reg6"
	.size	.L.str300, 11

	.type	.L.str301,@object       # @.str301
.L.str301:
	.asciz	"DW_OP_reg7"
	.size	.L.str301, 11

	.type	.L.str302,@object       # @.str302
.L.str302:
	.asciz	"DW_OP_reg8"
	.size	.L.str302, 11

	.type	.L.str303,@object       # @.str303
.L.str303:
	.asciz	"DW_OP_reg9"
	.size	.L.str303, 11

	.type	.L.str304,@object       # @.str304
.L.str304:
	.asciz	"DW_OP_reg10"
	.size	.L.str304, 12

	.type	.L.str305,@object       # @.str305
.L.str305:
	.asciz	"DW_OP_reg11"
	.size	.L.str305, 12

	.type	.L.str306,@object       # @.str306
.L.str306:
	.asciz	"DW_OP_reg12"
	.size	.L.str306, 12

	.type	.L.str307,@object       # @.str307
.L.str307:
	.asciz	"DW_OP_reg13"
	.size	.L.str307, 12

	.type	.L.str308,@object       # @.str308
.L.str308:
	.asciz	"DW_OP_reg14"
	.size	.L.str308, 12

	.type	.L.str309,@object       # @.str309
.L.str309:
	.asciz	"DW_OP_reg15"
	.size	.L.str309, 12

	.type	.L.str310,@object       # @.str310
.L.str310:
	.asciz	"DW_OP_reg16"
	.size	.L.str310, 12

	.type	.L.str311,@object       # @.str311
.L.str311:
	.asciz	"DW_OP_reg17"
	.size	.L.str311, 12

	.type	.L.str312,@object       # @.str312
.L.str312:
	.asciz	"DW_OP_reg18"
	.size	.L.str312, 12

	.type	.L.str313,@object       # @.str313
.L.str313:
	.asciz	"DW_OP_reg19"
	.size	.L.str313, 12

	.type	.L.str314,@object       # @.str314
.L.str314:
	.asciz	"DW_OP_reg20"
	.size	.L.str314, 12

	.type	.L.str315,@object       # @.str315
.L.str315:
	.asciz	"DW_OP_reg21"
	.size	.L.str315, 12

	.type	.L.str316,@object       # @.str316
.L.str316:
	.asciz	"DW_OP_reg22"
	.size	.L.str316, 12

	.type	.L.str317,@object       # @.str317
.L.str317:
	.asciz	"DW_OP_reg23"
	.size	.L.str317, 12

	.type	.L.str318,@object       # @.str318
.L.str318:
	.asciz	"DW_OP_reg24"
	.size	.L.str318, 12

	.type	.L.str319,@object       # @.str319
.L.str319:
	.asciz	"DW_OP_reg25"
	.size	.L.str319, 12

	.type	.L.str320,@object       # @.str320
.L.str320:
	.asciz	"DW_OP_reg26"
	.size	.L.str320, 12

	.type	.L.str321,@object       # @.str321
.L.str321:
	.asciz	"DW_OP_reg27"
	.size	.L.str321, 12

	.type	.L.str322,@object       # @.str322
.L.str322:
	.asciz	"DW_OP_reg28"
	.size	.L.str322, 12

	.type	.L.str323,@object       # @.str323
.L.str323:
	.asciz	"DW_OP_reg29"
	.size	.L.str323, 12

	.type	.L.str324,@object       # @.str324
.L.str324:
	.asciz	"DW_OP_reg30"
	.size	.L.str324, 12

	.type	.L.str325,@object       # @.str325
.L.str325:
	.asciz	"DW_OP_reg31"
	.size	.L.str325, 12

	.type	.L.str326,@object       # @.str326
.L.str326:
	.asciz	"DW_OP_breg0"
	.size	.L.str326, 12

	.type	.L.str327,@object       # @.str327
.L.str327:
	.asciz	"DW_OP_breg1"
	.size	.L.str327, 12

	.type	.L.str328,@object       # @.str328
.L.str328:
	.asciz	"DW_OP_breg2"
	.size	.L.str328, 12

	.type	.L.str329,@object       # @.str329
.L.str329:
	.asciz	"DW_OP_breg3"
	.size	.L.str329, 12

	.type	.L.str330,@object       # @.str330
.L.str330:
	.asciz	"DW_OP_breg4"
	.size	.L.str330, 12

	.type	.L.str331,@object       # @.str331
.L.str331:
	.asciz	"DW_OP_breg5"
	.size	.L.str331, 12

	.type	.L.str332,@object       # @.str332
.L.str332:
	.asciz	"DW_OP_breg6"
	.size	.L.str332, 12

	.type	.L.str333,@object       # @.str333
.L.str333:
	.asciz	"DW_OP_breg7"
	.size	.L.str333, 12

	.type	.L.str334,@object       # @.str334
.L.str334:
	.asciz	"DW_OP_breg8"
	.size	.L.str334, 12

	.type	.L.str335,@object       # @.str335
.L.str335:
	.asciz	"DW_OP_breg9"
	.size	.L.str335, 12

	.type	.L.str336,@object       # @.str336
.L.str336:
	.asciz	"DW_OP_breg10"
	.size	.L.str336, 13

	.type	.L.str337,@object       # @.str337
.L.str337:
	.asciz	"DW_OP_breg11"
	.size	.L.str337, 13

	.type	.L.str338,@object       # @.str338
.L.str338:
	.asciz	"DW_OP_breg12"
	.size	.L.str338, 13

	.type	.L.str339,@object       # @.str339
.L.str339:
	.asciz	"DW_OP_breg13"
	.size	.L.str339, 13

	.type	.L.str340,@object       # @.str340
.L.str340:
	.asciz	"DW_OP_breg14"
	.size	.L.str340, 13

	.type	.L.str341,@object       # @.str341
.L.str341:
	.asciz	"DW_OP_breg15"
	.size	.L.str341, 13

	.type	.L.str342,@object       # @.str342
.L.str342:
	.asciz	"DW_OP_breg16"
	.size	.L.str342, 13

	.type	.L.str343,@object       # @.str343
.L.str343:
	.asciz	"DW_OP_breg17"
	.size	.L.str343, 13

	.type	.L.str344,@object       # @.str344
.L.str344:
	.asciz	"DW_OP_breg18"
	.size	.L.str344, 13

	.type	.L.str345,@object       # @.str345
.L.str345:
	.asciz	"DW_OP_breg19"
	.size	.L.str345, 13

	.type	.L.str346,@object       # @.str346
.L.str346:
	.asciz	"DW_OP_breg20"
	.size	.L.str346, 13

	.type	.L.str347,@object       # @.str347
.L.str347:
	.asciz	"DW_OP_breg21"
	.size	.L.str347, 13

	.type	.L.str348,@object       # @.str348
.L.str348:
	.asciz	"DW_OP_breg22"
	.size	.L.str348, 13

	.type	.L.str349,@object       # @.str349
.L.str349:
	.asciz	"DW_OP_breg23"
	.size	.L.str349, 13

	.type	.L.str350,@object       # @.str350
.L.str350:
	.asciz	"DW_OP_breg24"
	.size	.L.str350, 13

	.type	.L.str351,@object       # @.str351
.L.str351:
	.asciz	"DW_OP_breg25"
	.size	.L.str351, 13

	.type	.L.str352,@object       # @.str352
.L.str352:
	.asciz	"DW_OP_breg26"
	.size	.L.str352, 13

	.type	.L.str353,@object       # @.str353
.L.str353:
	.asciz	"DW_OP_breg27"
	.size	.L.str353, 13

	.type	.L.str354,@object       # @.str354
.L.str354:
	.asciz	"DW_OP_breg28"
	.size	.L.str354, 13

	.type	.L.str355,@object       # @.str355
.L.str355:
	.asciz	"DW_OP_breg29"
	.size	.L.str355, 13

	.type	.L.str356,@object       # @.str356
.L.str356:
	.asciz	"DW_OP_breg30"
	.size	.L.str356, 13

	.type	.L.str357,@object       # @.str357
.L.str357:
	.asciz	"DW_OP_breg31"
	.size	.L.str357, 13

	.type	.L.str358,@object       # @.str358
.L.str358:
	.asciz	"DW_OP_regx"
	.size	.L.str358, 11

	.type	.L.str359,@object       # @.str359
.L.str359:
	.asciz	"DW_OP_fbreg"
	.size	.L.str359, 12

	.type	.L.str360,@object       # @.str360
.L.str360:
	.asciz	"DW_OP_bregx"
	.size	.L.str360, 12

	.type	.L.str361,@object       # @.str361
.L.str361:
	.asciz	"DW_OP_piece"
	.size	.L.str361, 12

	.type	.L.str362,@object       # @.str362
.L.str362:
	.asciz	"DW_OP_deref_size"
	.size	.L.str362, 17

	.type	.L.str363,@object       # @.str363
.L.str363:
	.asciz	"DW_OP_xderef_size"
	.size	.L.str363, 18

	.type	.L.str364,@object       # @.str364
.L.str364:
	.asciz	"DW_OP_nop"
	.size	.L.str364, 10

	.type	.L.str365,@object       # @.str365
.L.str365:
	.asciz	"OP_<unknown>"
	.size	.L.str365, 13

	.type	.L.str366,@object       # @.str366
.L.str366:
	.asciz	"DW"
	.size	.L.str366, 3

	.type	.L.str367,@object       # @.str367
.L.str367:
	.asciz	".globl "
	.size	.L.str367, 8

	.type	.L.str369,@object       # @.str369
.L.str369:
	.asciz	"Length of Compilation Unit Info"
	.size	.L.str369, 32

	.type	.L.str370,@object       # @.str370
.L.str370:
	.asciz	"DWARF version number"
	.size	.L.str370, 21

	.type	abbrev_section_label,@object # @abbrev_section_label
	.local	abbrev_section_label
	.comm	abbrev_section_label,30,16
	.type	.L.str371,@object       # @.str371
.L.str371:
	.asciz	"Offset Into Abbrev. Section"
	.size	.L.str371, 28

	.type	.L.str372,@object       # @.str372
.L.str372:
	.asciz	"Pointer Size (in bytes)"
	.size	.L.str372, 24

	.type	.L__FUNCTION__.size_of_die,@object # @__FUNCTION__.size_of_die
.L__FUNCTION__.size_of_die:
	.asciz	"size_of_die"
	.size	.L__FUNCTION__.size_of_die, 12

	.type	.L__FUNCTION__.build_abbrev_table,@object # @__FUNCTION__.build_abbrev_table
.L__FUNCTION__.build_abbrev_table:
	.asciz	"build_abbrev_table"
	.size	.L__FUNCTION__.build_abbrev_table, 19

	.type	abbrev_die_table_allocated,@object # @abbrev_die_table_allocated
	.local	abbrev_die_table_allocated
	.comm	abbrev_die_table_allocated,4,4
	.type	.L.str381,@object       # @.str381
.L.str381:
	.asciz	"%s."
	.size	.L.str381, 4

	.type	.L.str382,@object       # @.str382
.L.str382:
	.asciz	"%.2x"
	.size	.L.str382, 5

	.type	comdat_symbol_id,@object # @comdat_symbol_id
	.local	comdat_symbol_id
	.comm	comdat_symbol_id,8,8
	.type	comdat_symbol_number,@object # @comdat_symbol_number
	.local	comdat_symbol_number
	.comm	comdat_symbol_number,4,4
	.type	.L__FUNCTION__.attr_checksum,@object # @__FUNCTION__.attr_checksum
.L__FUNCTION__.attr_checksum:
	.asciz	"attr_checksum"
	.size	.L__FUNCTION__.attr_checksum, 14

	.type	.L.str383,@object       # @.str383
.L.str383:
	.asciz	"%s.%s.%x"
	.size	.L.str383, 9

	.type	.L.str384,@object       # @.str384
.L.str384:
	.asciz	"LDIE"
	.size	.L.str384, 5

	.type	gen_internal_sym.label_num,@object # @gen_internal_sym.label_num
	.local	gen_internal_sym.label_num
	.comm	gen_internal_sym.label_num,4,4
	.type	.L.str385,@object       # @.str385
.L.str385:
	.asciz	"%s %s"
	.size	.L.str385, 6

	.type	.L.str386,@object       # @.str386
.L.str386:
	.asciz	"GNU C++"
	.size	.L.str386, 8

	.type	.L.str387,@object       # @.str387
.L.str387:
	.asciz	"GNU Ada"
	.size	.L.str387, 8

	.type	.L.str388,@object       # @.str388
.L.str388:
	.asciz	"GNU F77"
	.size	.L.str388, 8

	.type	.L.str389,@object       # @.str389
.L.str389:
	.asciz	"GNU Pascal"
	.size	.L.str389, 11

	.type	.L.str390,@object       # @.str390
.L.str390:
	.asciz	"GNU Java"
	.size	.L.str390, 9

	.type	.L__FUNCTION__.gen_type_die,@object # @__FUNCTION__.gen_type_die
.L__FUNCTION__.gen_type_die:
	.asciz	"gen_type_die"
	.size	.L__FUNCTION__.gen_type_die, 13

	.type	.L__FUNCTION__.pop_decl_scope,@object # @__FUNCTION__.pop_decl_scope
.L__FUNCTION__.pop_decl_scope:
	.asciz	"pop_decl_scope"
	.size	.L__FUNCTION__.pop_decl_scope, 15

	.type	.L__FUNCTION__.splice_child_die,@object # @__FUNCTION__.splice_child_die
.L__FUNCTION__.splice_child_die:
	.asciz	"splice_child_die"
	.size	.L__FUNCTION__.splice_child_die, 17

	.type	.L__FUNCTION__.add_data_member_location_attribute,@object # @__FUNCTION__.add_data_member_location_attribute
.L__FUNCTION__.add_data_member_location_attribute:
	.asciz	"add_data_member_location_attribute"
	.size	.L__FUNCTION__.add_data_member_location_attribute, 35

	.type	.L__FUNCTION__.field_byte_offset,@object # @__FUNCTION__.field_byte_offset
.L__FUNCTION__.field_byte_offset:
	.asciz	"field_byte_offset"
	.size	.L__FUNCTION__.field_byte_offset, 18

	.type	.L__FUNCTION__.add_byte_size_attribute,@object # @__FUNCTION__.add_byte_size_attribute
.L__FUNCTION__.add_byte_size_attribute:
	.asciz	"add_byte_size_attribute"
	.size	.L__FUNCTION__.add_byte_size_attribute, 24

	.type	.L__FUNCTION__.scope_die_for,@object # @__FUNCTION__.scope_die_for
.L__FUNCTION__.scope_die_for:
	.asciz	"scope_die_for"
	.size	.L__FUNCTION__.scope_die_for, 14

	.type	.L__FUNCTION__.loc_descriptor_from_tree,@object # @__FUNCTION__.loc_descriptor_from_tree
.L__FUNCTION__.loc_descriptor_from_tree:
	.asciz	"loc_descriptor_from_tree"
	.size	.L__FUNCTION__.loc_descriptor_from_tree, 25

	.type	used_rtx_varray,@object # @used_rtx_varray
	.local	used_rtx_varray
	.comm	used_rtx_varray,8,8
	.type	.L__FUNCTION__.mem_loc_descriptor,@object # @__FUNCTION__.mem_loc_descriptor
.L__FUNCTION__.mem_loc_descriptor:
	.asciz	"mem_loc_descriptor"
	.size	.L__FUNCTION__.mem_loc_descriptor, 19

	.type	.L__FUNCTION__.reg_number,@object # @__FUNCTION__.reg_number
.L__FUNCTION__.reg_number:
	.asciz	"reg_number"
	.size	.L__FUNCTION__.reg_number, 11

	.type	.L__FUNCTION__.rtl_for_decl_location,@object # @__FUNCTION__.rtl_for_decl_location
.L__FUNCTION__.rtl_for_decl_location:
	.asciz	"rtl_for_decl_location"
	.size	.L__FUNCTION__.rtl_for_decl_location, 22

	.type	.L__FUNCTION__.loc_descriptor,@object # @__FUNCTION__.loc_descriptor
.L__FUNCTION__.loc_descriptor:
	.asciz	"loc_descriptor"
	.size	.L__FUNCTION__.loc_descriptor, 15

	.type	.L__FUNCTION__.gen_formal_parameter_die,@object # @__FUNCTION__.gen_formal_parameter_die
.L__FUNCTION__.gen_formal_parameter_die:
	.asciz	"gen_formal_parameter_die"
	.size	.L__FUNCTION__.gen_formal_parameter_die, 25

	.type	.L__FUNCTION__.add_location_or_const_value_attribute,@object # @__FUNCTION__.add_location_or_const_value_attribute
.L__FUNCTION__.add_location_or_const_value_attribute:
	.asciz	"add_location_or_const_value_attribute"
	.size	.L__FUNCTION__.add_location_or_const_value_attribute, 38

	.type	.L__FUNCTION__.add_const_value_attribute,@object # @__FUNCTION__.add_const_value_attribute
.L__FUNCTION__.add_const_value_attribute:
	.asciz	"add_const_value_attribute"
	.size	.L__FUNCTION__.add_const_value_attribute, 26

	.type	decl_die_table_allocated,@object # @decl_die_table_allocated
	.local	decl_die_table_allocated
	.comm	decl_die_table_allocated,4,4
	.type	decl_die_table,@object  # @decl_die_table
	.local	decl_die_table
	.comm	decl_die_table,8,8
	.type	decl_die_table_in_use,@object # @decl_die_table_in_use
	.local	decl_die_table_in_use
	.comm	decl_die_table_in_use,4,4
	.type	.L__FUNCTION__.add_abstract_origin_attribute,@object # @__FUNCTION__.add_abstract_origin_attribute
.L__FUNCTION__.add_abstract_origin_attribute:
	.asciz	"add_abstract_origin_attribute"
	.size	.L__FUNCTION__.add_abstract_origin_attribute, 30

	.type	.L__FUNCTION__.add_child_die,@object # @__FUNCTION__.add_child_die
.L__FUNCTION__.add_child_die:
	.asciz	"add_child_die"
	.size	.L__FUNCTION__.add_child_die, 14

	.type	primary_filename,@object # @primary_filename
	.local	primary_filename
	.comm	primary_filename,8,8
	.type	.L.str391,@object       # @.str391
.L.str391:
	.asciz	"decl_scope_table"
	.size	.L.str391, 17

	.type	line_info_table,@object # @line_info_table
	.local	line_info_table
	.comm	line_info_table,8,8
	.type	line_info_table_in_use,@object # @line_info_table_in_use
	.local	line_info_table_in_use
	.comm	line_info_table_in_use,4,4
	.type	.L.str392,@object       # @.str392
.L.str392:
	.asciz	"incomplete_types"
	.size	.L.str392, 17

	.type	.L.str393,@object       # @.str393
.L.str393:
	.asciz	"used_rtx_varray"
	.size	.L.str393, 16

	.type	.L.str394,@object       # @.str394
.L.str394:
	.asciz	"Ldebug_abbrev"
	.size	.L.str394, 14

	.type	.L.str395,@object       # @.str395
.L.str395:
	.asciz	"Ltext"
	.size	.L.str395, 6

	.type	.L.str396,@object       # @.str396
.L.str396:
	.asciz	"Ldebug_info"
	.size	.L.str396, 12

	.type	.L.str397,@object       # @.str397
.L.str397:
	.asciz	"Ldebug_line"
	.size	.L.str397, 12

	.type	.L.str398,@object       # @.str398
.L.str398:
	.asciz	"Ldebug_ranges"
	.size	.L.str398, 14

	.type	.L.str399,@object       # @.str399
.L.str399:
	.asciz	".debug_line"
	.size	.L.str399, 12

	.type	.L.str400,@object       # @.str400
.L.str400:
	.asciz	"Ldebug_macinfo"
	.size	.L.str400, 15

	.type	file_table.0,@object    # @file_table.0
	.local	file_table.0
	.comm	file_table.0,8,8
	.type	file_table.1,@object    # @file_table.1
	.local	file_table.1
	.comm	file_table.1,4,8
	.type	file_table.2,@object    # @file_table.2
	.local	file_table.2
	.comm	file_table.2,4,4
	.type	file_table.3,@object    # @file_table.3
	.local	file_table.3
	.comm	file_table.3,4,8
	.type	.L.str401,@object       # @.str401
.L.str401:
	.asciz	"Undefine macro"
	.size	.L.str401, 15

	.type	.L.str402,@object       # @.str402
.L.str402:
	.asciz	"At line number %d"
	.size	.L.str402, 18

	.type	.L.str403,@object       # @.str403
.L.str403:
	.asciz	"The macro"
	.size	.L.str403, 10

	.type	.L.str404,@object       # @.str404
.L.str404:
	.asciz	"Define macro"
	.size	.L.str404, 13

	.type	.L.str405,@object       # @.str405
.L.str405:
	.asciz	"Start new file"
	.size	.L.str405, 15

	.type	.L.str406,@object       # @.str406
.L.str406:
	.asciz	"Included from line number %d"
	.size	.L.str406, 29

	.type	.L.str407,@object       # @.str407
.L.str407:
	.asciz	"Filename we just started"
	.size	.L.str407, 25

	.type	.L.str408,@object       # @.str408
.L.str408:
	.asciz	"\t%s %s:%d\n"
	.size	.L.str408, 11

	.type	.L.str409,@object       # @.str409
.L.str409:
	.asciz	"#"
	.size	.L.str409, 2

	.type	.L.str410,@object       # @.str410
.L.str410:
	.asciz	"\t.loc %d %d 0\n"
	.size	.L.str410, 15

	.type	.L.str411,@object       # @.str411
.L.str411:
	.asciz	"<internal>"
	.size	.L.str411, 11

	.type	.L.str412,@object       # @.str412
.L.str412:
	.asciz	"<built-in>"
	.size	.L.str412, 11

	.type	.L.str413,@object       # @.str413
.L.str413:
	.asciz	"\t.file %u \"%s\"\n"
	.size	.L.str413, 16

	.type	.L__FUNCTION__.gen_decl_die,@object # @__FUNCTION__.gen_decl_die
.L__FUNCTION__.gen_decl_die:
	.asciz	"gen_decl_die"
	.size	.L__FUNCTION__.gen_decl_die, 13

	.type	.L__FUNCTION__.add_bit_offset_attribute,@object # @__FUNCTION__.add_bit_offset_attribute
.L__FUNCTION__.add_bit_offset_attribute:
	.asciz	"add_bit_offset_attribute"
	.size	.L__FUNCTION__.add_bit_offset_attribute, 25

	.type	.L__FUNCTION__.add_bit_size_attribute,@object # @__FUNCTION__.add_bit_size_attribute
.L__FUNCTION__.add_bit_size_attribute:
	.asciz	"add_bit_size_attribute"
	.size	.L__FUNCTION__.add_bit_size_attribute, 23

	.type	.L__FUNCTION__.gen_label_die,@object # @__FUNCTION__.gen_label_die
.L__FUNCTION__.gen_label_die:
	.asciz	"gen_label_die"
	.size	.L__FUNCTION__.gen_label_die, 14

	.type	.L.str414,@object       # @.str414
.L.str414:
	.asciz	"L"
	.size	.L.str414, 2

	.type	.L__FUNCTION__.gen_typedef_die,@object # @__FUNCTION__.gen_typedef_die
.L__FUNCTION__.gen_typedef_die:
	.asciz	"gen_typedef_die"
	.size	.L__FUNCTION__.gen_typedef_die, 16

	.type	.L__FUNCTION__.gen_tagged_type_instantiation_die,@object # @__FUNCTION__.gen_tagged_type_instantiation_die
.L__FUNCTION__.gen_tagged_type_instantiation_die:
	.asciz	"gen_tagged_type_instantiation_die"
	.size	.L__FUNCTION__.gen_tagged_type_instantiation_die, 34

	.type	.L__FUNCTION__.gen_subprogram_die,@object # @__FUNCTION__.gen_subprogram_die
.L__FUNCTION__.gen_subprogram_die:
	.asciz	"gen_subprogram_die"
	.size	.L__FUNCTION__.gen_subprogram_die, 19

	.type	.L.str415,@object       # @.str415
.L.str415:
	.asciz	"__builtin_va_alist"
	.size	.L.str415, 19

	.type	ranges_table_allocated,@object # @ranges_table_allocated
	.local	ranges_table_allocated
	.comm	ranges_table_allocated,4,4
	.type	arange_table_allocated,@object # @arange_table_allocated
	.local	arange_table_allocated
	.comm	arange_table_allocated,4,4
	.type	.L__FUNCTION__.gen_type_die_for_member,@object # @__FUNCTION__.gen_type_die_for_member
.L__FUNCTION__.gen_type_die_for_member:
	.asciz	"gen_type_die_for_member"
	.size	.L__FUNCTION__.gen_type_die_for_member, 24

	.type	.L__FUNCTION__.modified_type_die,@object # @__FUNCTION__.modified_type_die
.L__FUNCTION__.modified_type_die:
	.asciz	"modified_type_die"
	.size	.L__FUNCTION__.modified_type_die, 18

	.type	.L.str416,@object       # @.str416
.L.str416:
	.asciz	"__unknown__"
	.size	.L.str416, 12

	.type	.L.str417,@object       # @.str417
.L.str417:
	.asciz	"signed char"
	.size	.L.str417, 12

	.type	.L.str418,@object       # @.str418
.L.str418:
	.asciz	"unsigned char"
	.size	.L.str418, 14

	.type	.L__FUNCTION__.base_type_die,@object # @__FUNCTION__.base_type_die
.L__FUNCTION__.base_type_die:
	.asciz	"base_type_die"
	.size	.L__FUNCTION__.base_type_die, 14

	.type	.L__FUNCTION__.is_base_type,@object # @__FUNCTION__.is_base_type
.L__FUNCTION__.is_base_type:
	.asciz	"is_base_type"
	.size	.L__FUNCTION__.is_base_type, 13

	.type	pubname_table_allocated,@object # @pubname_table_allocated
	.local	pubname_table_allocated
	.comm	pubname_table_allocated,4,4
	.type	.L.str419,@object       # @.str419
.L.str419:
	.asciz	"DIE %4lu: %s\n"
	.size	.L.str419, 14

	.type	.L.str420,@object       # @.str420
.L.str420:
	.asciz	"  abbrev id: %lu"
	.size	.L.str420, 17

	.type	.L.str421,@object       # @.str421
.L.str421:
	.asciz	" offset: %lu\n"
	.size	.L.str421, 14

	.type	.L.str422,@object       # @.str422
.L.str422:
	.asciz	"  %s: "
	.size	.L.str422, 7

	.type	.L.str423,@object       # @.str423
.L.str423:
	.asciz	"address"
	.size	.L.str423, 8

	.type	.L.str424,@object       # @.str424
.L.str424:
	.asciz	"offset"
	.size	.L.str424, 7

	.type	.L.str425,@object       # @.str425
.L.str425:
	.asciz	"location descriptor"
	.size	.L.str425, 20

	.type	.L.str426,@object       # @.str426
.L.str426:
	.asciz	"location list -> label:%s"
	.size	.L.str426, 26

	.type	.L.str427,@object       # @.str427
.L.str427:
	.asciz	"range list"
	.size	.L.str427, 11

	.type	.L.str428,@object       # @.str428
.L.str428:
	.asciz	"%ld"
	.size	.L.str428, 4

	.type	.L.str429,@object       # @.str429
.L.str429:
	.asciz	"%lu"
	.size	.L.str429, 4

	.type	.L.str430,@object       # @.str430
.L.str430:
	.asciz	"constant (%lu,%lu)"
	.size	.L.str430, 19

	.type	.L.str431,@object       # @.str431
.L.str431:
	.asciz	"floating-point constant"
	.size	.L.str431, 24

	.type	.L.str432,@object       # @.str432
.L.str432:
	.asciz	"%u"
	.size	.L.str432, 3

	.type	.L.str433,@object       # @.str433
.L.str433:
	.asciz	"die -> label: %s"
	.size	.L.str433, 17

	.type	.L.str434,@object       # @.str434
.L.str434:
	.asciz	"die -> %lu"
	.size	.L.str434, 11

	.type	.L.str435,@object       # @.str435
.L.str435:
	.asciz	"die -> <null>"
	.size	.L.str435, 14

	.type	.L.str436,@object       # @.str436
.L.str436:
	.asciz	"label: %s"
	.size	.L.str436, 10

	.type	.L.str437,@object       # @.str437
.L.str437:
	.asciz	"\"%s\""
	.size	.L.str437, 5

	.type	.L.str438,@object       # @.str438
.L.str438:
	.asciz	"<null>"
	.size	.L.str438, 7

	.type	.L.str439,@object       # @.str439
.L.str439:
	.asciz	"%*s"
	.size	.L.str439, 4

	.type	.L.str440,@object       # @.str440
.L.str440:
	.zero	1
	.size	.L.str440, 1

	.type	.L.str441,@object       # @.str441
.L.str441:
	.asciz	".debug_frame"
	.size	.L.str441, 13

	.type	.L.str442,@object       # @.str442
.L.str442:
	.asciz	"Lframe"
	.size	.L.str442, 7

	.type	.L.str443,@object       # @.str443
.L.str443:
	.asciz	"LSCIE"
	.size	.L.str443, 6

	.type	.L.str444,@object       # @.str444
.L.str444:
	.asciz	"LECIE"
	.size	.L.str444, 6

	.type	.L.str445,@object       # @.str445
.L.str445:
	.asciz	"Length of Common Information Entry"
	.size	.L.str445, 35

	.type	.L.str446,@object       # @.str446
.L.str446:
	.asciz	"CIE Identifier Tag"
	.size	.L.str446, 19

	.type	.L.str447,@object       # @.str447
.L.str447:
	.asciz	"CIE Version"
	.size	.L.str447, 12

	.type	.L.str448,@object       # @.str448
.L.str448:
	.asciz	"CIE Augmentation"
	.size	.L.str448, 17

	.type	.L.str449,@object       # @.str449
.L.str449:
	.asciz	"CIE Code Alignment Factor"
	.size	.L.str449, 26

	.type	.L.str450,@object       # @.str450
.L.str450:
	.asciz	"CIE Data Alignment Factor"
	.size	.L.str450, 26

	.type	.L.str451,@object       # @.str451
.L.str451:
	.asciz	"CIE RA Column"
	.size	.L.str451, 14

	.type	.L.str452,@object       # @.str452
.L.str452:
	.asciz	"Augmentation size"
	.size	.L.str452, 18

	.type	.L.str453,@object       # @.str453
.L.str453:
	.asciz	"Personality (%s)"
	.size	.L.str453, 17

	.type	.L.str454,@object       # @.str454
.L.str454:
	.asciz	"LSDA Encoding (%s)"
	.size	.L.str454, 19

	.type	.L.str455,@object       # @.str455
.L.str455:
	.asciz	"FDE Encoding (%s)"
	.size	.L.str455, 18

	.type	cie_cfi_head,@object    # @cie_cfi_head
	.local	cie_cfi_head
	.comm	cie_cfi_head,8,8
	.type	.L.str456,@object       # @.str456
.L.str456:
	.asciz	"\t.align %d\n"
	.size	.L.str456, 12

	.type	.L.str457,@object       # @.str457
.L.str457:
	.asciz	"LASFDE"
	.size	.L.str457, 7

	.type	.L.str458,@object       # @.str458
.L.str458:
	.asciz	"LEFDE"
	.size	.L.str458, 6

	.type	.L.str459,@object       # @.str459
.L.str459:
	.asciz	"FDE Length"
	.size	.L.str459, 11

	.type	.L.str460,@object       # @.str460
.L.str460:
	.asciz	"FDE CIE offset"
	.size	.L.str460, 15

	.type	.L.str461,@object       # @.str461
.L.str461:
	.asciz	"FDE initial location"
	.size	.L.str461, 21

	.type	.L.str462,@object       # @.str462
.L.str462:
	.asciz	"FDE address range"
	.size	.L.str462, 18

	.type	.L.str463,@object       # @.str463
.L.str463:
	.asciz	"LLSDA"
	.size	.L.str463, 6

	.type	.L.str464,@object       # @.str464
.L.str464:
	.asciz	"Language Specific Data Area"
	.size	.L.str464, 28

	.type	.L.str465,@object       # @.str465
.L.str465:
	.asciz	"Language Specific Data Area (none)"
	.size	.L.str465, 35

	.type	.L.str466,@object       # @.str466
.L.str466:
	.asciz	"DW_CFA_advance_loc 0x%lx"
	.size	.L.str466, 25

	.type	.L.str467,@object       # @.str467
.L.str467:
	.asciz	"DW_CFA_offset, column 0x%lx"
	.size	.L.str467, 28

	.type	.L.str468,@object       # @.str468
.L.str468:
	.asciz	"DW_CFA_restore, column 0x%lx"
	.size	.L.str468, 29

	.type	.L__FUNCTION__.output_cfi,@object # @__FUNCTION__.output_cfi
.L__FUNCTION__.output_cfi:
	.asciz	"output_cfi"
	.size	.L__FUNCTION__.output_cfi, 11

	.type	.L.str472,@object       # @.str472
.L.str472:
	.asciz	"DW_CFA_nop"
	.size	.L.str472, 11

	.type	.L.str473,@object       # @.str473
.L.str473:
	.asciz	"DW_CFA_set_loc"
	.size	.L.str473, 15

	.type	.L.str474,@object       # @.str474
.L.str474:
	.asciz	"DW_CFA_advance_loc1"
	.size	.L.str474, 20

	.type	.L.str475,@object       # @.str475
.L.str475:
	.asciz	"DW_CFA_advance_loc2"
	.size	.L.str475, 20

	.type	.L.str476,@object       # @.str476
.L.str476:
	.asciz	"DW_CFA_advance_loc4"
	.size	.L.str476, 20

	.type	.L.str477,@object       # @.str477
.L.str477:
	.asciz	"DW_CFA_offset_extended"
	.size	.L.str477, 23

	.type	.L.str478,@object       # @.str478
.L.str478:
	.asciz	"DW_CFA_restore_extended"
	.size	.L.str478, 24

	.type	.L.str479,@object       # @.str479
.L.str479:
	.asciz	"DW_CFA_undefined"
	.size	.L.str479, 17

	.type	.L.str480,@object       # @.str480
.L.str480:
	.asciz	"DW_CFA_same_value"
	.size	.L.str480, 18

	.type	.L.str481,@object       # @.str481
.L.str481:
	.asciz	"DW_CFA_register"
	.size	.L.str481, 16

	.type	.L.str482,@object       # @.str482
.L.str482:
	.asciz	"DW_CFA_remember_state"
	.size	.L.str482, 22

	.type	.L.str483,@object       # @.str483
.L.str483:
	.asciz	"DW_CFA_restore_state"
	.size	.L.str483, 21

	.type	.L.str484,@object       # @.str484
.L.str484:
	.asciz	"DW_CFA_def_cfa"
	.size	.L.str484, 15

	.type	.L.str485,@object       # @.str485
.L.str485:
	.asciz	"DW_CFA_def_cfa_register"
	.size	.L.str485, 24

	.type	.L.str486,@object       # @.str486
.L.str486:
	.asciz	"DW_CFA_def_cfa_offset"
	.size	.L.str486, 22

	.type	.L.str487,@object       # @.str487
.L.str487:
	.asciz	"DW_CFA_def_cfa_expression"
	.size	.L.str487, 26

	.type	.L.str488,@object       # @.str488
.L.str488:
	.asciz	"DW_CFA_expression"
	.size	.L.str488, 18

	.type	.L.str489,@object       # @.str489
.L.str489:
	.asciz	"DW_CFA_offset_extended_sf"
	.size	.L.str489, 26

	.type	.L.str490,@object       # @.str490
.L.str490:
	.asciz	"DW_CFA_def_cfa_sf"
	.size	.L.str490, 18

	.type	.L.str491,@object       # @.str491
.L.str491:
	.asciz	"DW_CFA_def_cfa_offset_sf"
	.size	.L.str491, 25

	.type	.L.str492,@object       # @.str492
.L.str492:
	.asciz	"DW_CFA_MIPS_advance_loc8"
	.size	.L.str492, 25

	.type	.L.str493,@object       # @.str493
.L.str493:
	.asciz	"DW_CFA_GNU_window_save"
	.size	.L.str493, 23

	.type	.L.str494,@object       # @.str494
.L.str494:
	.asciz	"DW_CFA_GNU_args_size"
	.size	.L.str494, 21

	.type	.L.str495,@object       # @.str495
.L.str495:
	.asciz	"DW_CFA_GNU_negative_offset_extended"
	.size	.L.str495, 36

	.type	.L.str496,@object       # @.str496
.L.str496:
	.asciz	"DW_CFA_<unknown>"
	.size	.L.str496, 17

	.type	.L__FUNCTION__.initial_return_save,@object # @__FUNCTION__.initial_return_save
.L__FUNCTION__.initial_return_save:
	.asciz	"initial_return_save"
	.size	.L__FUNCTION__.initial_return_save, 20

	.type	.L__FUNCTION__.dwarf2out_frame_debug_expr,@object # @__FUNCTION__.dwarf2out_frame_debug_expr
.L__FUNCTION__.dwarf2out_frame_debug_expr:
	.asciz	"dwarf2out_frame_debug_expr"
	.size	.L__FUNCTION__.dwarf2out_frame_debug_expr, 27

	.type	queued_reg_saves,@object # @queued_reg_saves
	.local	queued_reg_saves
	.comm	queued_reg_saves,8,8
	.type	last_reg_save_label,@object # @last_reg_save_label
	.local	last_reg_save_label
	.comm	last_reg_save_label,8,8
	.type	.L__FUNCTION__.dwarf2out_stack_adjust,@object # @__FUNCTION__.dwarf2out_stack_adjust
.L__FUNCTION__.dwarf2out_stack_adjust:
	.asciz	"dwarf2out_stack_adjust"
	.size	.L__FUNCTION__.dwarf2out_stack_adjust, 23

	.type	.L__FUNCTION__.stack_adjust_offset,@object # @__FUNCTION__.stack_adjust_offset
.L__FUNCTION__.stack_adjust_offset:
	.asciz	"stack_adjust_offset"
	.size	.L__FUNCTION__.stack_adjust_offset, 20

	.type	.L.str497,@object       # @.str497
.L.str497:
	.asciz	"DW_LOC_OP %s not implemented\n"
	.size	.L.str497, 30


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
