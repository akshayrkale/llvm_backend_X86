	.file	"graph.c.bc"
	.text
	.globl	print_rtl_graph_with_bb
	.align	16, 0x90
	.type	print_rtl_graph_with_bb,@function
print_rtl_graph_with_bb:                # @print_rtl_graph_with_bb
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp5:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
.Ltmp6:
	.cfi_offset %rbx, -56
.Ltmp7:
	.cfi_offset %r12, -48
.Ltmp8:
	.cfi_offset %r13, -40
.Ltmp9:
	.cfi_offset %r14, -32
.Ltmp10:
	.cfi_offset %r15, -24
	movq	%rdx, -56(%rbp)         # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rbx, -48(%rbp)         # 8-byte Spill
	movq	%rdi, -72(%rbp)         # 8-byte Spill
	callq	strlen
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	strlen
	movl	graph_dump_format(%rip), %ecx
	movq	graph_ext(,%rcx,8), %rdi
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rax, %rbx
	callq	strlen
	movq	%rax, %r13
	leaq	(%rbx,%r14), %r12
	leaq	16(%r13,%r12), %rax
	andq	$-16, %rax
	movq	%rsp, %r15
	subq	%rax, %r15
	movq	%r15, %rsp
	cmpq	$0, basic_block_info(%rip)
	je	.LBB0_98
# BB#1:                                 # %if.end
	incq	%r13
	movq	%r15, %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	callq	memcpy
	addq	%r15, %r14
	movq	%r14, %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	memcpy
	addq	%r15, %r12
	movq	%r12, %rdi
	movq	-64(%rbp), %rsi         # 8-byte Reload
	movq	%r13, %rdx
	callq	memcpy
	movabsq	$.L.str, %rsi
	movq	%r15, %rdi
	callq	fopen
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB0_98
# BB#2:                                 # %if.end12
	cmpq	$0, -56(%rbp)           # 8-byte Folded Reload
	je	.LBB0_3
# BB#4:                                 # %if.else
	movq	%rbx, -48(%rbp)         # 8-byte Spill
	callq	get_max_uid
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$2, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	%r12, -104(%rbp)        # 8-byte Spill
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %r13
	movq	%rbx, %rdi
	callq	xmalloc
	testq	%r15, %r15
	movq	%r12, %r15
	movq	%r13, %r12
	movq	%rax, %r13
	jle	.LBB0_6
# BB#5:                                 # %for.body.lr.ph
	decq	%r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	leaq	4(,%rax,4), %rbx
	movq	$255, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	memset
	movq	$255, %rsi
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rdx
	callq	memset
.LBB0_6:                                # %for.end
	movq	%r13, -96(%rbp)         # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	movq	%r12, %r8
	movq	%r8, -88(%rbp)          # 8-byte Spill
	jle	.LBB0_14
# BB#7:                                 # %for.body35.lr.ph
	movq	basic_block_info(%rip), %r9
	movq	-96(%rbp), %rdx         # 8-byte Reload
	.align	16, 0x90
.LBB0_9:                                # %for.body35
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_10 Depth 2
	movq	24(%r9,%rax,8), %rsi
	movq	(%rsi), %rcx
	movslq	8(%rcx), %rdi
	decq	%rax
	movl	%eax, (%r15,%rdi,4)
	movq	8(%rsi), %rsi
	movslq	8(%rsi), %rdi
	testq	%rcx, %rcx
	movl	%eax, (%r8,%rdi,4)
	je	.LBB0_8
	.align	16, 0x90
.LBB0_10:                               # %for.body52
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	8(%rcx), %rdi
	movl	(%rdx,%rdi,4), %ebx
	testq	%rbx, %rbx
	movq	$1, %rbx
	je	.LBB0_12
# BB#11:                                # %select.mid
                                        #   in Loop: Header=BB0_10 Depth=2
	movq	$2, %rbx
.LBB0_12:                               # %select.end
                                        #   in Loop: Header=BB0_10 Depth=2
	movl	%ebx, (%rdx,%rdi,4)
	cmpq	%rsi, %rcx
	je	.LBB0_8
# BB#13:                                # %for.inc70
                                        #   in Loop: Header=BB0_10 Depth=2
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_10
.LBB0_8:                                # %for.cond32.loopexit
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB0_9
.LBB0_14:                               # %for.end75
	movl	graph_dump_format(%rip), %eax
	movq	$1, %rcx
	movl	%ecx, dump_for_graph(%rip)
	cmpq	$1, %rax
	jne	.LBB0_16
# BB#15:                                # %sw.bb.i
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rdx, %rcx
	movq	%r8, %rbx
	callq	fprintf
	movq	%rbx, %r8
.LBB0_16:                               # %for.cond79.preheader
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB0_23
# BB#17:
	movabsq	$entry_exit_blocks+112, %r13
	.align	16, 0x90
.LBB0_18:                               # %for.body82
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_65 Depth 2
                                        #     Child Loop BB0_88 Depth 2
	movslq	8(%rbx), %rax
	movslq	(%r15,%rax,4), %r14
	testq	%r14, %r14
	jns	.LBB0_29
# BB#19:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	movslq	(%r8,%rax,4), %rax
	testq	%rax, %rax
	jns	.LBB0_27
# BB#20:                                # %if.then97
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	-56(%rbp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	je	.LBB0_22
# BB#21:                                # %if.then97
                                        #   in Loop: Header=BB0_18 Depth=1
	cmpq	$37, %rax
	je	.LBB0_22
	.align	16, 0x90
.LBB0_27:                               # %if.end108
                                        #   in Loop: Header=BB0_18 Depth=1
	testq	%r14, %r14
	movq	-56(%rbp), %rbx         # 8-byte Reload
	js	.LBB0_35
# BB#28:                                # %if.end108
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	graph_dump_format(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	jmp	.LBB0_30
	.align	16, 0x90
.LBB0_29:                               # %if.then116
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	graph_dump_format(%rip), %eax
.LBB0_30:                               # %if.then116
                                        #   in Loop: Header=BB0_18 Depth=1
	cmpq	$1, %rax
	jne	.LBB0_35
# BB#31:                                # %sw.epilog.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	xorq	%rax, %rax
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movabsq	$.L.str23, %rsi
	movq	%r14, %rcx
	movq	%r14, %r8
	callq	fprintf
	movl	graph_dump_format(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_35
# BB#32:                                # %start_bb.exit
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$3, %rsi
	movq	$1, %rdx
	movabsq	$.L.str24, %rdi
	movq	-48(%rbp), %rcx         # 8-byte Reload
	callq	fwrite
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB0_35
# BB#33:                                # %start_bb.exit
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	graph_dump_format(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB0_35
# BB#34:                                # %sw.bb.i324
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movl	8(%rbx), %r9d
	subq	$16, %rsp
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	$.L.str19, (%rsp)
	movq	-48(%rbp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movabsq	$.L.str21, %rsi
	movq	%rdx, %r8
	callq	fprintf
	addq	$16, %rsp
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	movq	%r14, %rcx
	callq	fwrite
	.align	16, 0x90
.LBB0_35:                               # %if.end124
                                        #   in Loop: Header=BB0_18 Depth=1
	cmpq	$0, 16(%rbx)
	movl	graph_dump_format(%rip), %eax
	jne	.LBB0_36
# BB#37:                                # %if.end124
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	movq	-48(%rbp), %r14         # 8-byte Reload
	jne	.LBB0_39
# BB#38:                                # %sw.bb.i331
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movl	8(%rbx), %r8d
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str8, %rsi
	movq	%rdx, %rcx
	callq	fprintf
	movl	graph_dump_format(%rip), %eax
	jmp	.LBB0_39
	.align	16, 0x90
.LBB0_36:                               #   in Loop: Header=BB0_18 Depth=1
	movq	-48(%rbp), %r14         # 8-byte Reload
.LBB0_39:                               # %if.end.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB0_46
# BB#40:                                # %sw.bb4.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movzbl	(%rbx), %eax
	movl	8(%rbx), %ecx
	cmpq	$37, %rax
	movabsq	$.L.str10, %r8
	je	.LBB0_45
# BB#41:                                # %cond.false.i
                                        #   in Loop: Header=BB0_18 Depth=1
	cmpq	$32, %rax
	movabsq	$.L.str11, %r8
	je	.LBB0_45
# BB#42:                                # %cond.false14.i
                                        #   in Loop: Header=BB0_18 Depth=1
	leaq	-33(%rax), %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	$2, %rsi
	movabsq	$.L.str12, %r8
	jb	.LBB0_45
# BB#43:                                # %cond.false24.i
                                        #   in Loop: Header=BB0_18 Depth=1
	cmpq	$36, %rax
	movabsq	$.L.str13, %r8
	je	.LBB0_45
# BB#44:                                # %cond.false24.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movabsq	$.L.str14, %r8
	.align	16, 0x90
.LBB0_45:                               # %cond.end33.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	rtx_name(,%rax,8), %r9
	subq	$16, %rsp
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	%r14, %rdi
	movabsq	$.L.str9, %rsi
	callq	fprintf
	addq	$16, %rsp
.LBB0_46:                               # %sw.epilog42.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB0_50
# BB#47:                                # %if.then46.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	movabsq	$.L.str15, %rdx
	jns	.LBB0_49
# BB#48:                                # %if.then52.i
                                        #   in Loop: Header=BB0_18 Depth=1
	addq	$100, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	note_insn_name(,%rax,8), %rdx
.LBB0_49:                               # %if.end58.i
                                        #   in Loop: Header=BB0_18 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str16, %rsi
	callq	fprintf
	jmp	.LBB0_54
	.align	16, 0x90
.LBB0_50:                               # %if.else.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_52
# BB#51:                                # %if.then66.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	jmp	.LBB0_53
	.align	16, 0x90
.LBB0_52:                               # %if.else71.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
.LBB0_53:                               # %if.end74.i
                                        #   in Loop: Header=BB0_18 Depth=1
	callq	print_rtl_single
.LBB0_54:                               # %if.end74.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	graph_dump_format(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_56
# BB#55:                                # %sw.bb75.i
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$4, %rsi
	movq	$1, %rdx
	movabsq	$.L.str17, %rdi
	movq	%r14, %rcx
	callq	fwrite
.LBB0_56:                               # %node_data.exit
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	%r14, -48(%rbp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	movslq	8(%rbx), %rcx
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	movq	-88(%rbp), %rdx         # 8-byte Reload
	movslq	(%rdx,%rcx,4), %rcx
	movq	%rax, %rdi
	movq	%rdi, -72(%rbp)         # 8-byte Spill
	testq	%rcx, %rcx
	js	.LBB0_85
# BB#57:                                # %if.then133
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	basic_block_info(%rip), %rax
	movl	graph_dump_format(%rip), %edx
	movq	32(%rax,%rcx,8), %rbx
	cmpq	$1, %rdx
	jne	.LBB0_59
# BB#58:                                # %sw.bb.i334
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rdi, %r14
	movabsq	$.L.str6, %rdi
	movq	-48(%rbp), %rcx         # 8-byte Reload
	callq	fwrite
	movq	%r14, %rdi
.LBB0_59:                               # %end_bb.exit
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	40(%rbx), %r12
	testq	%r12, %r12
	je	.LBB0_85
# BB#60:                                # %for.body139.lr.ph
                                        #   in Loop: Header=BB0_18 Depth=1
	testq	%rdi, %rdi
	movabsq	$.L.str19, %rax
	movq	%rax, -80(%rbp)         # 8-byte Spill
	jne	.LBB0_62
# BB#61:                                # %for.body139.lr.ph
                                        #   in Loop: Header=BB0_18 Depth=1
	movabsq	$.L.str15, %rax
	movq	%rax, -80(%rbp)         # 8-byte Spill
.LBB0_62:                               # %for.body139.lr.ph
                                        #   in Loop: Header=BB0_18 Depth=1
	movabsq	$0, %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movq	%r13, %r14
	jne	.LBB0_64
# BB#63:                                # %for.body139.lr.ph
                                        #   in Loop: Header=BB0_18 Depth=1
	movabsq	$1, %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
.LBB0_64:                               # %for.body139.lr.ph
                                        #   in Loop: Header=BB0_18 Depth=1
	xorq	%r15, %r15
	.align	16, 0x90
.LBB0_65:                               # %for.body139
                                        #   Parent Loop BB0_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r12), %rax
	cmpq	%r14, %rax
	jne	.LBB0_66
# BB#75:                                # %if.else159
                                        #   in Loop: Header=BB0_65 Depth=2
	movl	graph_dump_format(%rip), %eax
	cmpq	$1, %rax
	movq	-64(%rbp), %rax         # 8-byte Reload
	jne	.LBB0_81
# BB#76:                                # %sw.bb.i346
                                        #   in Loop: Header=BB0_65 Depth=2
	movl	48(%r12), %r13d
	andq	$2, %r13
	movq	%r13, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movq	-80(%rbp), %rsi         # 8-byte Reload
	je	.LBB0_78
# BB#77:                                # %sw.bb.i346
                                        #   in Loop: Header=BB0_65 Depth=2
	movabsq	$.L.str18, %rsi
.LBB0_78:                               # %sw.bb.i346
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movq	-56(%rbp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	subq	$16, %rsp
	movq	$999999, %r9            # imm = 0xF423F
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movq	-48(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str21, %rsi
	movq	%rdx, %r8
	callq	fprintf
	addq	$16, %rsp
	testq	%r14, %r14
	je	.LBB0_80
# BB#79:                                # %if.then9.i356
                                        #   in Loop: Header=BB0_65 Depth=2
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str22, %rsi
	movq	%r13, %rdx
	callq	fprintf
.LBB0_80:                               # %if.end11.i357
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movq	-64(%rbp), %rax         # 8-byte Reload
	movabsq	$entry_exit_blocks+112, %r14
	movq	-72(%rbp), %rdi         # 8-byte Reload
	jmp	.LBB0_81
	.align	16, 0x90
.LBB0_66:                               # %if.then142
                                        #   in Loop: Header=BB0_65 Depth=2
	movl	graph_dump_format(%rip), %ecx
	movq	(%rax), %r13
	cmpq	$1, %rcx
	jne	.LBB0_73
# BB#67:                                # %sw.bb.i338
                                        #   in Loop: Header=BB0_65 Depth=2
	movl	48(%r12), %ebx
	movq	-56(%rbp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	movl	8(%r13), %r9d
	andq	$2, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB0_71
# BB#68:                                # %if.else.i339
                                        #   in Loop: Header=BB0_65 Depth=2
	cmpq	%r13, %rdi
	movabsq	$.L.str19, %rsi
	jne	.LBB0_70
# BB#69:                                # %if.else.i339
                                        #   in Loop: Header=BB0_65 Depth=2
	movabsq	$.L.str15, %rsi
.LBB0_70:                               # %if.else.i339
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	subq	$16, %rsp
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movq	-48(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str21, %rsi
	movq	%rdx, %r8
	callq	fprintf
	addq	$16, %rsp
	jmp	.LBB0_72
.LBB0_71:                               # %if.then9.i.critedge
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	subq	$16, %rsp
	xorq	%rax, %rax
	movq	$.L.str18, (%rsp)
	movq	-48(%rbp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movabsq	$.L.str21, %rsi
	movq	%rdx, %r8
	callq	fprintf
	addq	$16, %rsp
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str22, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rbx
	movabsq	$entry_exit_blocks+112, %r14
	callq	fprintf
.LBB0_72:                               # %if.end11.i
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	%rbx, -48(%rbp)         # 8-byte Spill
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movq	-72(%rbp), %rdi         # 8-byte Reload
.LBB0_73:                               # %draw_edge.exit343
                                        #   in Loop: Header=BB0_65 Depth=2
	cmpq	%rdi, %r13
	movabsq	$0, %rax
	jne	.LBB0_81
# BB#74:                                # %draw_edge.exit343
                                        #   in Loop: Header=BB0_65 Depth=2
	movabsq	$1, %rax
.LBB0_81:                               # %for.inc174
                                        #   in Loop: Header=BB0_65 Depth=2
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB0_83
# BB#82:                                # %for.inc174
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	%r15, %rax
.LBB0_83:                               # %for.inc174
                                        #   in Loop: Header=BB0_65 Depth=2
	movq	8(%r12), %r12
	testq	%r12, %r12
	movq	%rax, %r15
	jne	.LBB0_65
# BB#84:                                # %if.end176
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	%r14, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	-104(%rbp), %r15        # 8-byte Reload
	jne	.LBB0_22
	.align	16, 0x90
.LBB0_85:                               # %if.then178
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$999999, %r9            # imm = 0xF423F
	testq	%rdi, %rdi
	je	.LBB0_92
# BB#86:                                # %lor.lhs.false
                                        #   in Loop: Header=BB0_18 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$35, %rax
	jne	.LBB0_91
# BB#87:                                # %do.body.preheader
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	24(%rdi), %rax
	testq	%rax, %rax
	movq	$999999, %r9            # imm = 0xF423F
	je	.LBB0_96
	.align	16, 0x90
.LBB0_88:                               # %land.rhs
                                        #   Parent Loop BB0_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	je	.LBB0_90
# BB#89:                                # %land.rhs
                                        #   in Loop: Header=BB0_88 Depth=2
	cmpq	$37, %rcx
	jne	.LBB0_95
.LBB0_90:                               # %do.body.backedge
                                        #   in Loop: Header=BB0_88 Depth=2
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB0_88
	jmp	.LBB0_96
.LBB0_91:                               # %cond.true
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	8(%rdi), %r9d
.LBB0_92:                               # %cond.end
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	graph_dump_format(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_22
# BB#93:                                # %sw.bb.i361
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movq	-56(%rbp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	subq	$16, %rsp
	xorq	%rax, %rax
	movq	$.L.str15, (%rsp)
	movq	-48(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str21, %rsi
	movq	%rdx, %r8
	callq	fprintf
	addq	$16, %rsp
	jmp	.LBB0_94
.LBB0_95:                               # %cond.true211
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	8(%rax), %r9d
.LBB0_96:                               # %cond.end216
                                        #   in Loop: Header=BB0_18 Depth=1
	movl	graph_dump_format(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_22
# BB#97:                                # %sw.bb.i372
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movq	-56(%rbp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	subq	$16, %rsp
	xorq	%rax, %rax
	movq	$.L.str20, (%rsp)
	movq	-48(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movabsq	$.L.str21, %rsi
	movq	%rdx, %r8
	callq	fprintf
	addq	$16, %rsp
	movq	$3, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str22, %rsi
	callq	fprintf
.LBB0_94:                               # %for.cond79.backedge
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	movq	%rbx, %rcx
	callq	fwrite
.LBB0_22:                               # %for.cond79.backedge
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	-56(%rbp), %rbx         # 8-byte Reload
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	movq	-88(%rbp), %r8          # 8-byte Reload
	jne	.LBB0_18
.LBB0_23:                               # %for.end224
	movq	%r8, %r14
	movl	graph_dump_format(%rip), %eax
	xorq	%rcx, %rcx
	movl	%ecx, dump_for_graph(%rip)
	cmpq	$1, %rax
	movq	-48(%rbp), %rbx         # 8-byte Reload
	jne	.LBB0_25
# BB#24:                                # %sw.bb.i387
	movq	cfun(%rip), %rax
	movq	40(%rax), %rdx
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
.LBB0_25:                               # %end_fct.exit
	movq	%r15, %rdi
	callq	free
	movq	%r14, %rdi
	callq	free
	movq	-96(%rbp), %rdi         # 8-byte Reload
	callq	free
	jmp	.LBB0_26
.LBB0_98:                               # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB0_3:                                # %if.then14
	movabsq	$.L.str1, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
.LBB0_26:                               # %if.end225
	movq	%rbx, %rdi
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	fclose  # TAILCALL
.Ltmp11:
	.size	print_rtl_graph_with_bb, .Ltmp11-print_rtl_graph_with_bb
	.cfi_endproc

	.globl	clean_graph_dump_file
	.align	16, 0x90
	.type	clean_graph_dump_file,@function
clean_graph_dump_file:                  # @clean_graph_dump_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp15:
	.cfi_def_cfa_offset 16
.Ltmp16:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp17:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp18:
	.cfi_offset %rbx, -56
.Ltmp19:
	.cfi_offset %r12, -48
.Ltmp20:
	.cfi_offset %r13, -40
.Ltmp21:
	.cfi_offset %r14, -32
.Ltmp22:
	.cfi_offset %r15, -24
	movq	%rsi, %rbx
	movq	%rbx, -56(%rbp)         # 8-byte Spill
	movq	%rdi, %r13
                                        # kill: RDI<def> R13<kill>
	callq	strlen
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	strlen
	movl	graph_dump_format(%rip), %ecx
	movq	graph_ext(,%rcx,8), %rdi
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	%rax, %r14
	callq	strlen
	leaq	(%r14,%r12), %r15
	leaq	16(%rax,%r15), %rcx
	leaq	1(%rax), %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	andq	$-16, %rcx
	movq	%rsp, %rbx
	subq	%rcx, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	memcpy
	addq	%rbx, %r12
	movq	%r12, %rdi
	movq	-56(%rbp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	callq	memcpy
	addq	%rbx, %r15
	movq	%r15, %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	movq	-64(%rbp), %rdx         # 8-byte Reload
	callq	memcpy
	movabsq	$.L.str2, %rsi
	movq	%rbx, %rdi
	callq	fopen
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB1_5
# BB#1:                                 # %if.end
	movl	graph_dump_format(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_6
# BB#2:                                 # %if.end
	cmpq	$1, %rax
	jne	.LBB1_4
# BB#3:                                 # %sw.bb
	movabsq	$.L.str4, %rdi
	movq	$26, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
.LBB1_4:                                # %sw.epilog
	movq	%r14, %rdi
	callq	fclose
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB1_5:                                # %if.then
	movabsq	$.L.str3, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	callq	fatal_io_error
.LBB1_6:                                # %sw.bb11
	movabsq	$.L.str5, %rax
	movabsq	$.L__FUNCTION__.clean_graph_dump_file, %rcx
	movq	$429, %rsi              # imm = 0x1AD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp23:
	.size	clean_graph_dump_file, .Ltmp23-clean_graph_dump_file
	.cfi_endproc

	.globl	finish_graph_dump_file
	.align	16, 0x90
	.type	finish_graph_dump_file,@function
finish_graph_dump_file:                 # @finish_graph_dump_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp27:
	.cfi_def_cfa_offset 16
.Ltmp28:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp29:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp30:
	.cfi_offset %rbx, -56
.Ltmp31:
	.cfi_offset %r12, -48
.Ltmp32:
	.cfi_offset %r13, -40
.Ltmp33:
	.cfi_offset %r14, -32
.Ltmp34:
	.cfi_offset %r15, -24
	movq	%rsi, %r14
	movq	%r14, -56(%rbp)         # 8-byte Spill
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	callq	strlen
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	strlen
	movl	graph_dump_format(%rip), %ecx
	movq	graph_ext(,%rcx,8), %rdi
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	%rax, %r13
	callq	strlen
	leaq	(%r13,%rbx), %r14
	leaq	16(%rax,%r14), %rcx
	leaq	1(%rax), %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	andq	$-16, %rcx
	movq	%rsp, %r15
	subq	%rcx, %r15
	movq	%r15, %rsp
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	addq	%r15, %rbx
	movq	%rbx, %rdi
	movq	-56(%rbp), %rsi         # 8-byte Reload
	movq	%r13, %rdx
	callq	memcpy
	addq	%r15, %r14
	movq	%r14, %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	movq	-64(%rbp), %rdx         # 8-byte Reload
	callq	memcpy
	movabsq	$.L.str, %rsi
	movq	%r15, %rdi
	callq	fopen
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB2_5
# BB#1:                                 # %if.then
	movl	graph_dump_format(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_6
# BB#2:                                 # %if.then
	cmpq	$1, %rax
	jne	.LBB2_4
# BB#3:                                 # %sw.bb
	movabsq	$.L.str6, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
.LBB2_4:                                # %sw.epilog
	movq	%rbx, %rdi
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	fclose  # TAILCALL
.LBB2_5:                                # %if.end
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB2_6:                                # %sw.bb11
	movabsq	$.L.str5, %rax
	movabsq	$.L__FUNCTION__.finish_graph_dump_file, %rcx
	movq	$461, %rsi              # imm = 0x1CD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp35:
	.size	finish_graph_dump_file, .Ltmp35-finish_graph_dump_file
	.cfi_endproc

	.type	graph_ext,@object       # @graph_ext
	.section	.rodata,"a",@progbits
	.align	16
graph_ext:
	.quad	.L.str15
	.quad	.L.str26
	.size	graph_ext, 16

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"a"
	.size	.L.str, 2

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"(nil)\n"
	.size	.L.str1, 7

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"w"
	.size	.L.str2, 2

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"can't open %s"
	.size	.L.str3, 14

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"graph: {\nport_sharing: no\n"
	.size	.L.str4, 27

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"graph.c"
	.size	.L.str5, 8

	.type	.L__FUNCTION__.clean_graph_dump_file,@object # @__FUNCTION__.clean_graph_dump_file
.L__FUNCTION__.clean_graph_dump_file:
	.asciz	"clean_graph_dump_file"
	.size	.L__FUNCTION__.clean_graph_dump_file, 22

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"}\n"
	.size	.L.str6, 3

	.type	.L__FUNCTION__.finish_graph_dump_file,@object # @__FUNCTION__.finish_graph_dump_file
.L__FUNCTION__.finish_graph_dump_file:
	.asciz	"finish_graph_dump_file"
	.size	.L__FUNCTION__.finish_graph_dump_file, 23

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"node: { title: \"%s.999999\" label: \"END\" }\n}\n"
	.size	.L.str7, 45

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"edge: { sourcename: \"%s.0\" targetname: \"%s.%d\" }\n"
	.size	.L.str8, 50

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"node: {\n  title: \"%s.%d\"\n  color: %s\n  label: \"%s %d\n"
	.size	.L.str9, 54

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"lightgrey"
	.size	.L.str10, 10

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"green"
	.size	.L.str11, 6

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"darkgreen"
	.size	.L.str12, 10

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"darkgrey\n  shape: ellipse"
	.size	.L.str13, 26

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"white"
	.size	.L.str14, 6

	.type	.L.str15,@object        # @.str15
.L.str15:
	.zero	1
	.size	.L.str15, 1

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	" %s"
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"\"\n}\n"
	.size	.L.str17, 5

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"color: red "
	.size	.L.str18, 12

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"color: blue "
	.size	.L.str19, 13

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"color: green "
	.size	.L.str20, 14

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"edge: { sourcename: \"%s.%d\" targetname: \"%s.%d\" %s"
	.size	.L.str21, 51

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"class: %d "
	.size	.L.str22, 11

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"graph: {\ntitle: \"%s.BB%d\"\nfolding: 1\ncolor: lightblue\nlabel: \"basic block %d"
	.size	.L.str23, 77

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"\"\n\n"
	.size	.L.str24, 4

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"graph: { title: \"%s\"\nfolding: 1\nhidden: 2\nnode: { title: \"%s.0\" }\n"
	.size	.L.str25, 67

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	".vcg"
	.size	.L.str26, 5


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
