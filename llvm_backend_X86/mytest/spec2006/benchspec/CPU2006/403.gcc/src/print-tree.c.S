	.file	"print-tree.c.bc"
	.text
	.globl	debug_tree
	.align	16, 0x90
	.type	debug_tree,@function
debug_tree:                             # @debug_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$296, %rdi              # imm = 0x128
	callq	permalloc
	movq	%rax, table(%rip)
	xorq	%rsi, %rsi
	movq	$296, %rdx              # imm = 0x128
	movq	%rax, %rdi
	callq	memset
	movq	stderr(%rip), %r14
	movabsq	$.L.str, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	print_node
	movq	$10, %rdi
	movq	$0, table(%rip)
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fputc  # TAILCALL
.Ltmp8:
	.size	debug_tree, .Ltmp8-debug_tree
	.cfi_endproc

	.globl	print_node
	.align	16, 0x90
	.type	print_node,@function
print_node:                             # @print_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp17:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp20:
	.cfi_def_cfa_offset 288
.Ltmp21:
	.cfi_offset %rbx, -48
.Ltmp22:
	.cfi_offset %r12, -40
.Ltmp23:
	.cfi_offset %r13, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%rbx, %rbx
	je	.LBB1_354
# BB#1:                                 # %if.end
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$25, %rax
	jge	.LBB1_2
# BB#3:                                 # %if.end3
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %esi
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$9, %rdx
	jl	.LBB1_5
# BB#4:                                 # %land.lhs.true
	movq	%rsi, %rcx
	orq	$16, %rcx
	andq	$255, %rcx
	cmpq	$116, %rcx
	je	.LBB1_2
.LBB1_5:                                # %if.end11
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB1_2
# BB#6:                                 # %if.end19
	movabsq	$-2492803253203993461, %rcx # imm = 0xDD67C8A60DD67C8B
	movq	%rbx, %rax
	mulq	%rcx
	movq	$5, %rcx
	movq	table(%rip), %rax
	shrq	%cl, %rdx
	imulq	$37, %rdx, %rcx
	movq	%rbx, %r15
	subq	%rcx, %r15
	movq	(%rax,%r15,8), %rax
	jmp	.LBB1_7
.LBB1_15:                               # %for.cond
                                        #   in Loop: Header=BB1_7 Depth=1
	movq	8(%rax), %rax
.LBB1_7:                                # %if.end19
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB1_8
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB1_7 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB1_15
.LBB1_2:                                # %if.then2
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	print_node_brief
	jmp	.LBB1_354
.LBB1_8:                                # %for.end
	movq	$16, %rdi
	callq	permalloc
	movq	%rbx, (%rax)
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	table(%rip), %rcx
	movq	(%rcx,%r15,8), %rcx
	movq	%rcx, 8(%rax)
	movq	table(%rip), %rcx
	movq	%rax, (%rcx,%r15,8)
	movq	$32, %rcx
	movq	%r13, %rax
	movq	%r13, 40(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r12, %r15
	jle	.LBB1_11
# BB#9:                                 # %for.body.lr.ph.i
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r13          # 8-byte Reload
	andq	%r12, %r13
	.align	16, 0x90
.LBB1_10:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r15, %rsi
	callq	fputc
	incq	%rbx
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	%r13, %rax
	jne	.LBB1_10
.LBB1_11:                               # %indent_to.exit
	movq	64(%rsp), %rbx          # 8-byte Reload
	movzbl	16(%rbx), %eax
	movq	tree_code_name(,%rax,8), %rcx
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	fprintf
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	$255, %rax
	cmpq	$116, %rax
	movq	%r15, %r13
	jne	.LBB1_12
# BB#16:                                # %if.then55
	movq	96(%rbx), %rax
	testq	%rax, %rax
	je	.LBB1_22
# BB#17:                                # %if.then58
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB1_18
# BB#19:                                # %land.lhs.true83
	movq	72(%rax), %rax
	jmp	.LBB1_20
.LBB1_12:                               # %indent_to.exit
	cmpq	$100, %rax
	jne	.LBB1_22
# BB#13:                                # %if.then45
	movq	72(%rbx), %rax
.LBB1_20:                               # %land.lhs.true83
	testq	%rax, %rax
	jne	.LBB1_21
	jmp	.LBB1_22
.LBB1_18:                               # %if.then58
	cmpq	$1, %rcx
	jne	.LBB1_22
.LBB1_21:                               # %if.then89
	movq	32(%rax), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
.LBB1_22:                               # %if.end102
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	jne	.LBB1_24
# BB#23:                                # %if.then109
	movq	32(%rbx), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
	movl	16(%rbx), %eax
.LBB1_24:                               # %if.end114
	andq	$255, %rax
	cmpq	$25, %rax
	jne	.LBB1_27
# BB#25:                                # %if.then121
	movq	%rbx, %r12
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jg	.LBB1_31
# BB#26:                                # %if.then124
	movq	8(%r12), %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	4(%rax), %rcx
	movabsq	$.L.str13, %rsi
	movq	%r13, %rdi
	callq	print_node_brief
	jmp	.LBB1_31
.LBB1_27:                               # %if.else128
	movq	8(%rbx), %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	4(%rax), %rcx
	movabsq	$.L.str13, %rsi
	movq	%r13, %rdi
	callq	print_node
	cmpq	$0, 8(%rbx)
	movq	%rbx, %r12
	je	.LBB1_31
# BB#28:                                # %if.then135
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	3(%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_31
# BB#29:                                # %for.body.lr.ph.i2118
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r14
	.align	16, 0x90
.LBB1_30:                               # %for.body.i2123
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%r14, %rax
	jne	.LBB1_30
.LBB1_31:                               # %if.end138
	movq	%r12, %rbx
	movl	16(%rbx), %eax
	testq	$256, %rax              # imm = 0x100
	je	.LBB1_33
# BB#32:                                # %if.then143
	movabsq	$.L.str14, %rdi
	movq	$13, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_33:                               # %if.end145
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB1_35
# BB#34:                                # %if.then151
	movabsq	$.L.str15, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_35:                               # %if.end153
	testq	$512, %rax              # imm = 0x200
	je	.LBB1_37
# BB#36:                                # %if.then159
	movabsq	$.L.str16, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_37:                               # %if.end161
	testq	$1024, %rax             # imm = 0x400
	je	.LBB1_39
# BB#38:                                # %if.then167
	movabsq	$.L.str17, %rdi
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_39:                               # %if.end169
	testq	$2048, %rax             # imm = 0x800
	je	.LBB1_41
# BB#40:                                # %if.then175
	movabsq	$.L.str18, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_41:                               # %if.end177
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB1_43
# BB#42:                                # %if.then183
	movabsq	$.L.str19, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_43:                               # %if.end185
	testq	$16384, %rax            # imm = 0x4000
	je	.LBB1_45
# BB#44:                                # %if.then191
	movabsq	$.L.str20, %rdi
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_45:                               # %if.end193
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB1_47
# BB#46:                                # %if.then199
	movabsq	$.L.str21, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_47:                               # %if.end201
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB1_49
# BB#48:                                # %if.then207
	movabsq	$.L.str22, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_49:                               # %if.end209
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB1_51
# BB#50:                                # %if.then215
	movabsq	$.L.str23, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_51:                               # %if.end217
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB1_53
# BB#52:                                # %if.then223
	movabsq	$.L.str24, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_53:                               # %if.end225
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB1_55
# BB#54:                                # %if.then231
	movabsq	$.L.str25, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_55:                               # %if.end233
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_57
# BB#56:                                # %if.then239
	movabsq	$.L.str26, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_57:                               # %if.end241
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB1_59
# BB#58:                                # %if.then247
	movabsq	$.L.str27, %rdi
	movq	$11, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_59:                               # %if.end249
	testq	$16777216, %rax         # imm = 0x1000000
	je	.LBB1_61
# BB#60:                                # %if.then255
	movabsq	$.L.str28, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_61:                               # %if.end257
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB1_63
# BB#62:                                # %if.then263
	movabsq	$.L.str29, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_63:                               # %if.end265
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB1_65
# BB#64:                                # %if.then271
	movabsq	$.L.str30, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_65:                               # %if.end273
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB1_67
# BB#66:                                # %if.then279
	movabsq	$.L.str31, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_67:                               # %if.end281
	testq	$268435456, %rax        # imm = 0x10000000
	je	.LBB1_69
# BB#68:                                # %if.then287
	movabsq	$.L.str32, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_69:                               # %if.end289
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB1_71
# BB#70:                                # %if.then295
	movabsq	$.L.str33, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_71:                               # %if.end297
	testq	$1073741824, %rax       # imm = 0x40000000
	je	.LBB1_73
# BB#72:                                # %if.then303
	movabsq	$.L.str34, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%rbx), %eax
.LBB1_73:                               # %if.end305
	movq	%rax, %rdi
	andq	$255, %rdi
	movsbq	tree_code_type(%rdi), %rcx
	cmpq	$59, %rcx
	jg	.LBB1_75
# BB#74:                                # %if.end305
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	leaq	-49(%rcx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB1_77
	jmp	.LBB1_353
.LBB1_75:                               # %if.end305
	cmpq	$97, %rcx
	jg	.LBB1_79
# BB#76:                                # %if.end305
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$60, %rax
	jne	.LBB1_353
	jmp	.LBB1_77
.LBB1_79:                               # %if.end305
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	cmpq	$113, %rcx
	jg	.LBB1_82
# BB#80:                                # %if.end305
	leaq	-98(%rcx), %rdx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rdx
	cmpq	$3, %rdx
	ja	.LBB1_353
# BB#81:                                # %if.end305
	jmpq	*.LJTI1_0(,%rdx,8)
.LBB1_285:                              # %sw.bb1316
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %rdx
	movq	40(%rsp), %rbx          # 8-byte Reload
	addq	$4, %rbx
	movabsq	$.L.str114, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	48(%r14), %rdx
	movabsq	$.L.str115, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	40(%r14), %rdx
	movabsq	$.L.str116, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	(%r14), %rdx
	movabsq	$.L.str87, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	56(%r14), %rdx
	movabsq	$.L.str79, %rsi
	jmp	.LBB1_316
.LBB1_82:                               # %if.end305
	leaq	-114(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jae	.LBB1_83
.LBB1_77:                               # %sw.bb1328
	cmpq	$52, %rdi
	jne	.LBB1_286
# BB#78:                                # %if.then1335
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %rdx
	movq	40(%rsp), %rbx          # 8-byte Reload
	addq	$4, %rbx
	movabsq	$.L.str114, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	40(%r14), %rdx
	movabsq	$.L.str117, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	48(%r14), %rdx
	movabsq	$.L.str118, %rsi
	jmp	.LBB1_316
.LBB1_286:                              # %if.end1346
	movslq	tree_code_length(,%rdi,4), %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	callq	first_rtl_op
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB1_306
# BB#287:                               # %for.body1361.lr.ph
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	4(%rax), %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	jle	.LBB1_298
# BB#288:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, 56(%rsp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB1_289:                              # %for.body1361.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_291 Depth 2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB1_290
# BB#296:                               # %if.else1381.us
                                        #   in Loop: Header=BB1_289 Depth=1
	xorq	%rax, %rax
	movq	%r13, %r12
	leaq	230(%rsp), %r13
	movq	%r13, %rdi
	movabsq	$.L.str121, %rsi
	movq	%r15, %rdx
	callq	sprintf
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	32(%rax,%r15,8), %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	callq	print_node
	jmp	.LBB1_297
	.align	16, 0x90
.LBB1_290:                              # %for.body.lr.ph.i2154.us
                                        #   in Loop: Header=BB1_289 Depth=1
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%r12, %r12
	.align	16, 0x90
.LBB1_291:                              # %for.body.i2159.us
                                        #   Parent Loop BB1_289 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%r12
	movq	%r12, %rax
	andq	%r14, %rax
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_291
# BB#292:                               # %indent_to.exit2160.us
                                        #   in Loop: Header=BB1_289 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str119, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	32(%rax,%r15,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_293
# BB#294:                               # %if.else1377.us
                                        #   in Loop: Header=BB1_289 Depth=1
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str120, %rdi
	movq	%r13, %rcx
	callq	fwrite
	jmp	.LBB1_295
.LBB1_293:                              # %if.then1372.us
                                        #   in Loop: Header=BB1_289 Depth=1
	movq	%r13, %rdi
	callq	print_rtl
.LBB1_295:                              # %if.end1379.us
                                        #   in Loop: Header=BB1_289 Depth=1
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	%r13, %r12
.LBB1_297:                              # %for.inc1390.us
                                        #   in Loop: Header=BB1_289 Depth=1
	incq	%r15
	movq	%r15, %rax
	andq	%r14, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%r12, %r13
	jne	.LBB1_289
	jmp	.LBB1_306
.LBB1_83:                               # %if.end305
	movq	%rcx, %rdx
	andq	%rsi, %rdx
	cmpq	$116, %rdx
	jne	.LBB1_84
# BB#213:                               # %sw.bb920
	leaq	-20(%rdi), %rax
	andq	%rsi, %rax
	cmpq	$2, %rax
	ja	.LBB1_216
# BB#214:                               # %land.lhs.true941
	movq	64(%rsp), %rbx          # 8-byte Reload
	movzbl	62(%rbx), %eax
	testq	$2, %rax
	je	.LBB1_220
# BB#215:                               # %if.then947
	leaq	60(%rbx), %r12
	movabsq	$.L.str88, %rdi
	movq	$13, %rsi
	jmp	.LBB1_223
.LBB1_298:                              # %for.body1361.preheader
	leaq	230(%rsp), %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, 56(%rsp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB1_299:                              # %for.body1361
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB1_300
# BB#304:                               # %if.else1381
                                        #   in Loop: Header=BB1_299 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str121, %rsi
	movq	%r15, %rdx
	callq	sprintf
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	32(%rax,%r15,8), %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	callq	print_node
	jmp	.LBB1_305
.LBB1_300:                              # %indent_to.exit2160
                                        #   in Loop: Header=BB1_299 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str119, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	32(%rax,%r15,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_301
# BB#302:                               # %if.else1377
                                        #   in Loop: Header=BB1_299 Depth=1
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str120, %rdi
	movq	%r13, %rcx
	callq	fwrite
	jmp	.LBB1_303
.LBB1_301:                              # %if.then1372
                                        #   in Loop: Header=BB1_299 Depth=1
	movq	%r13, %rdi
	callq	print_rtl
.LBB1_303:                              # %if.end1379
                                        #   in Loop: Header=BB1_299 Depth=1
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
.LBB1_305:                              # %for.inc1390
                                        #   in Loop: Header=BB1_299 Depth=1
	incq	%r15
	movq	%r15, %rax
	andq	%r12, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB1_299
.LBB1_306:                              # %for.end1391
	movq	64(%rsp), %r14          # 8-byte Reload
	movzbl	16(%r14), %eax
	movq	40(%rsp), %rbx          # 8-byte Reload
	addq	$4, %rbx
	cmpq	$144, %rax
	jne	.LBB1_314
# BB#307:                               # %if.then1398
	movq	%r14, %r12
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_310
# BB#308:                               # %for.body.lr.ph.i2145
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r15          # 8-byte Reload
	andq	%r14, %r15
	.align	16, 0x90
.LBB1_309:                              # %for.body.i2150
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	%r15, %rax
	jne	.LBB1_309
.LBB1_310:                              # %indent_to.exit2151
	movq	%r12, %r14
	movq	40(%r14), %rax
	testq	%rax, %rax
	je	.LBB1_311
# BB#312:                               # %cond.true1404
	movq	32(%rax), %rdx
	jmp	.LBB1_313
.LBB1_311:
	movabsq	$.L.str123, %rdx
.LBB1_313:                              # %cond.end1412
	movl	24(%r14), %r8d
	movq	$32, %rcx
	movq	%r8, %r9
	shlq	%cl, %r9
	movq	$44, %rcx
	sarq	%cl, %r9
	andq	$4095, %r8              # imm = 0xFFF
	movabsq	$.L.str122, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r9, %rcx
	callq	fprintf
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB1_314:                              # %if.end1418
	movq	(%r14), %rdx
.LBB1_315:                              # %sw.epilog1576
	movabsq	$.L.str87, %rsi
.LBB1_316:                              # %sw.epilog1576
	movq	%r13, %rdi
.LBB1_317:                              # %sw.epilog1576
	movq	%rbx, %rcx
	callq	print_node
.LBB1_353:                              # %sw.epilog1576
	movq	$62, %rdi
	movq	%r13, %rsi
	callq	fputc
.LBB1_354:                              # %return
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_84:                               # %if.end305
	cmpq	$120, %rdx
	jne	.LBB1_353
.LBB1_85:                               # %sw.bb1422
	cmpq	$24, %rdi
	jg	.LBB1_318
# BB#86:                                # %sw.bb1422
	cmpq	$1, %rdi
	movq	64(%rsp), %r15          # 8-byte Reload
	je	.LBB1_345
# BB#87:                                # %sw.bb1422
	cmpq	$2, %rdi
	jne	.LBB1_88
# BB#346:                               # %sw.bb1532
	movq	24(%r15), %rdx
	movq	40(%rsp), %rbx          # 8-byte Reload
	addq	$4, %rbx
	movabsq	$.L.str128, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	32(%r15), %rdx
	movabsq	$.L.str129, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node
	movq	(%r15), %rdx
	jmp	.LBB1_315
.LBB1_97:                               # %sw.bb
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	48(%rbx), %r14
	testq	$16384, %r14            # imm = 0x4000
	movq	%r14, %rax
	je	.LBB1_99
# BB#98:                                # %if.then323
	movabsq	$.L.str35, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_99:                               # %if.end325
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB1_101
# BB#100:                               # %if.then332
	movabsq	$.L.str36, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_101:                              # %if.end334
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB1_103
# BB#102:                               # %if.then341
	movabsq	$.L.str37, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_103:                              # %if.end343
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB1_105
# BB#104:                               # %if.then350
	movabsq	$.L.str38, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_105:                              # %if.end352
	testq	$256, %rax              # imm = 0x100
	je	.LBB1_107
# BB#106:                               # %if.then359
	movabsq	$.L.str39, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_107:                              # %if.end361
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB1_109
# BB#108:                               # %if.then368
	movabsq	$.L.str40, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_109:                              # %if.end370
	testq	$1024, %rax             # imm = 0x400
	je	.LBB1_113
# BB#110:                               # %land.lhs.true377
	movzbl	16(%rbx), %ecx
	cmpq	$37, %rcx
	ja	.LBB1_112
# BB#111:                               # %land.lhs.true377
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$140660178944, %rcx     # imm = 0x20C0000000
	testq	%rcx, %rdx
	jne	.LBB1_113
.LBB1_112:                              # %if.then398
	movabsq	$.L.str41, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%rbx), %rax
.LBB1_113:                              # %if.end400
	testq	$512, %rax              # imm = 0x200
	je	.LBB1_115
# BB#114:                               # %if.then407
	movabsq	$.L.str42, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
.LBB1_115:                              # %if.end409
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$33, %rcx
	jne	.LBB1_116
# BB#117:                               # %land.lhs.true416
	movzbl	49(%rbx), %ecx
	movq	%rbx, %r12
	testq	$1, %rcx
	je	.LBB1_119
# BB#118:                               # %if.then424
	movabsq	$.L.str43, %rdi
	movq	$15, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
	jmp	.LBB1_119
.LBB1_216:                              # %if.else949
	cmpq	$23, %rdi
	movq	64(%rsp), %rbx          # 8-byte Reload
	jne	.LBB1_217
# BB#221:                               # %land.lhs.true972
	movzbl	62(%rbx), %eax
	leaq	60(%rbx), %r12
	testq	$2, %rax
	je	.LBB1_224
# BB#222:                               # %if.then979
	movabsq	$.L.str90, %rdi
	movq	$24, %rsi
	jmp	.LBB1_223
.LBB1_318:                              # %sw.bb1422
	addq	$-25, %rdi
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rdi
	cmpq	$4, %rdi
	movq	64(%rsp), %r15          # 8-byte Reload
	ja	.LBB1_351
# BB#319:                               # %sw.bb1422
	jmpq	*.LJTI1_1(,%rdi,8)
.LBB1_320:                              # %sw.bb1427
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_322
# BB#321:                               # %if.then1434
	movabsq	$.L.str6, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
.LBB1_322:                              # %if.end1436
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	40(%r15), %rcx
	movq	%r15, %rbx
	cmpq	$-1, %rcx
	jne	.LBB1_323
# BB#328:                               # %land.lhs.true1451
	xorq	%r8, %r8
	cmpq	$0, 32(%rbx)
	je	.LBB1_325
# BB#329:                               # %if.then1457
	movq	$45, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	32(%rbx), %rcx
	negq	%rcx
	jmp	.LBB1_327
.LBB1_217:                              # %if.else949
	cmpq	$6, %rdi
	jne	.LBB1_220
# BB#218:                               # %land.lhs.true956
	movzbl	62(%rbx), %eax
	testq	$2, %rax
	je	.LBB1_220
# BB#219:                               # %if.then963
	leaq	60(%rbx), %r12
	movabsq	$.L.str89, %rdi
	movq	$9, %rsi
.LBB1_223:                              # %if.end983
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	jmp	.LBB1_224
.LBB1_220:                              # %if.else965.if.end983_crit_edge
	leaq	60(%rbx), %r12
.LBB1_224:                              # %if.end983
	movl	(%r12), %eax
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB1_226
# BB#225:                               # %if.then989
	movabsq	$.L.str91, %rdi
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_226:                              # %if.end991
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_228
# BB#227:                               # %if.then997
	movabsq	$.L.str92, %rdi
	movq	$19, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
.LBB1_228:                              # %if.end999
	movzbl	16(%rbx), %eax
	cmpq	$18, %rax
	je	.LBB1_233
# BB#229:                               # %if.end999
	cmpq	$23, %rax
	jne	.LBB1_230
# BB#235:                               # %land.lhs.true1037
	movzbl	2(%r12), %eax
	testq	$8, %rax
	je	.LBB1_238
# BB#236:                               # %if.then1044
	movabsq	$.L.str94, %rdi
	movq	$20, %rsi
	jmp	.LBB1_237
.LBB1_233:                              # %land.lhs.true1021
	movzbl	2(%r12), %eax
	testq	$8, %rax
	je	.LBB1_238
# BB#234:                               # %if.then1028
	movabsq	$.L.str93, %rdi
	movq	$21, %rsi
	jmp	.LBB1_237
.LBB1_230:                              # %if.end999
	cmpq	$21, %rax
	jne	.LBB1_238
# BB#231:                               # %land.lhs.true1006
	movzbl	2(%r12), %eax
	testq	$8, %rax
	je	.LBB1_238
# BB#232:                               # %if.then1012
	movabsq	$.L.str54, %rdi
	movq	$18, %rsi
.LBB1_237:                              # %if.end1048
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
.LBB1_238:                              # %if.end1048
	movl	(%r12), %eax
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB1_240
# BB#239:                               # %if.then1054
	movabsq	$.L.str48, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_240:                              # %if.end1056
	testq	$16777216, %rax         # imm = 0x1000000
	je	.LBB1_242
# BB#241:                               # %if.then1063
	movabsq	$.L.str95, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_242:                              # %if.end1065
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB1_244
# BB#243:                               # %if.then1072
	movabsq	$.L.str96, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_244:                              # %if.end1074
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB1_246
# BB#245:                               # %if.then1081
	movabsq	$.L.str97, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_246:                              # %if.end1083
	testq	$134217728, %rax        # imm = 0x8000000
	movq	%rbx, %r15
	je	.LBB1_248
# BB#247:                               # %if.then1090
	movabsq	$.L.str98, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_248:                              # %if.end1092
	testq	$268435456, %rax        # imm = 0x10000000
	je	.LBB1_250
# BB#249:                               # %if.then1099
	movabsq	$.L.str99, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_250:                              # %if.end1101
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB1_252
# BB#251:                               # %if.then1108
	movabsq	$.L.str100, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_252:                              # %if.end1110
	testq	$1073741824, %rax       # imm = 0x40000000
	je	.LBB1_254
# BB#253:                               # %if.then1117
	movabsq	$.L.str101, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	(%r12), %eax
.LBB1_254:                              # %if.end1119
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	$6, %rcx
	shrq	%cl, %rax
	andq	$1016, %rax             # imm = 0x3F8
	movq	mode_name(%rax), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	callq	fprintf
	movq	32(%r15), %rdx
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%r15, %r14
	leaq	4(%r12), %r15
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movabsq	$.L.str66, %rsi
	movq	%r13, %rdi
	movq	%r15, %rcx
	callq	print_node
	movq	40(%r14), %rdx
	movabsq	$.L.str67, %rsi
	movq	%r13, %rdi
	movq	%r15, %rcx
	callq	print_node
	leaq	3(%r12), %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r13, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jle	.LBB1_257
# BB#255:                               # %for.body.lr.ph.i2199
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	movq	%r12, %r13
	andq	%r14, %r13
.LBB1_256:                              # %for.body.i2204
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r15, %rsi
	callq	fputc
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	%r13, %rax
	jne	.LBB1_256
.LBB1_257:                              # %indent_to.exit2205
	movq	48(%rsp), %rax          # 8-byte Reload
	movslq	(%rax), %rax
	testq	%rax, %rax
	movq	%r15, %r13
	jns	.LBB1_259
# BB#258:                               # %if.then1140
	movabsq	$.L.str68, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
.LBB1_259:                              # %if.end1142
	movq	64(%rsp), %r15          # 8-byte Reload
	movl	64(%r15), %edx
	movabsq	$.L.str69, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
	movl	88(%r15), %edx
	movabsq	$.L.str102, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
	movabsq	$.L.str73, %rdi
	movq	$11, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%r13, %rcx
	callq	fwrite
	movq	152(%r15), %rdx
	movabsq	$.L.str74, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
	movq	48(%r15), %rdx
	movabsq	$.L.str78, %rsi
	movq	%r13, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	print_node
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$12, %rcx
	ja	.LBB1_262
# BB#260:                               # %if.end1142
	shlq	%cl, %rbx
	testq	$7360, %rbx             # imm = 0x1CC0
	je	.LBB1_262
# BB#261:                               # %if.then1187
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %edx
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$.L.str103, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
	movq	104(%r15), %rdx
	movabsq	$.L.str104, %rsi
	movq	%r13, %rdi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	112(%r15), %rdx
	movabsq	$.L.str105, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node_brief
	movl	16(%r15), %eax
.LBB1_262:                              # %if.end1196
	andq	$255, %rax
	addq	$-10, %rax
	andq	%r14, %rax
	cmpq	$13, %rax
	ja	.LBB1_272
# BB#263:                               # %if.end1196
	jmpq	*.LJTI1_2(,%rax,8)
.LBB1_267:                              # %if.then1245
	movq	24(%r15), %rdx
	movabsq	$.L.str108, %rsi
	jmp	.LBB1_265
.LBB1_345:                              # %sw.bb1531
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	*lang_hooks+136(%rip)
	jmp	.LBB1_353
.LBB1_88:                               # %sw.bb1422
	cmpq	$3, %rdi
	jne	.LBB1_351
# BB#89:                                # %sw.bb1541
	movslq	24(%r15), %r14
	movq	%r15, %rbx
	testq	%r14, %r14
	jle	.LBB1_353
# BB#90:                                # %for.body1545.lr.ph
	movq	40(%rsp), %rdx          # 8-byte Reload
	addq	$4, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	movq	%rdx, %r15
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_347
# BB#91:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movabsq	$.L.str130, %r12
.LBB1_92:                               # %for.body1545.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_94 Depth 2
	movq	56(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 32(%rbx,%rax,8)
	je	.LBB1_96
# BB#93:                                # %for.body.lr.ph.i2127.us
                                        #   in Loop: Header=BB1_92 Depth=1
	xorq	%rax, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	leaq	76(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%r12, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	sprintf
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
.LBB1_94:                               # %for.body.i2132.us
                                        #   Parent Loop BB1_92 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	40(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	%r15, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_94
# BB#95:                                # %indent_to.exit2133.us
                                        #   in Loop: Header=BB1_92 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	32(%rbx,%rax,8), %rax
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	leaq	76(%rsp), %rdx
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	print_node_brief
.LBB1_96:                               # %for.inc1562.us
                                        #   in Loop: Header=BB1_92 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r14, %rax
	jne	.LBB1_92
	jmp	.LBB1_353
.LBB1_351:                              # %sw.default
	movq	%r15, %rsi
	andq	$255, %rcx
	cmpq	$120, %rcx
	jne	.LBB1_353
# BB#352:                               # %if.then1574
	movq	%r13, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	*lang_hooks+112(%rip)
	jmp	.LBB1_353
.LBB1_116:
	movq	%rbx, %r12
.LBB1_119:                              # %if.end426
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB1_122
# BB#120:                               # %land.lhs.true433
	movzbl	49(%r12), %ecx
	testq	$8, %rcx
	je	.LBB1_122
# BB#121:                               # %if.then440
	movabsq	$.L.str44, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_122:                              # %if.end442
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB1_125
# BB#123:                               # %land.lhs.true449
	movzbl	51(%r12), %ecx
	testq	$96, %rcx
	je	.LBB1_125
# BB#124:                               # %if.then457
	movabsq	$.L.str45, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_125:                              # %if.end459
	movq	%rax, %rcx
	andq	$8447, %rcx             # imm = 0x20FF
	cmpq	$8222, %rcx             # imm = 0x201E
	jne	.LBB1_127
# BB#126:                               # %if.then473
	movabsq	$.L.str46, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_127:                              # %if.end475
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB1_130
# BB#128:                               # %land.lhs.true482
	movzbl	49(%r12), %ecx
	testq	$4, %rcx
	je	.LBB1_130
# BB#129:                               # %if.then490
	movabsq	$.L.str47, %rdi
	movq	$16, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_130:                              # %if.end492
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	jne	.LBB1_133
# BB#131:                               # %land.lhs.true499
	movzbl	49(%r12), %ecx
	testq	$4, %rcx
	je	.LBB1_133
# BB#132:                               # %if.then507
	movabsq	$.L.str48, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_133:                              # %if.end509
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	jne	.LBB1_136
# BB#134:                               # %land.lhs.true516
	movzbl	49(%r12), %ecx
	testq	$16, %rcx
	je	.LBB1_136
# BB#135:                               # %if.then523
	movabsq	$.L.str49, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_136:                              # %if.end525
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	jne	.LBB1_139
# BB#137:                               # %land.lhs.true532
	movzbl	52(%r12), %ecx
	testq	$4, %rcx
	je	.LBB1_139
# BB#138:                               # %if.then539
	movabsq	$.L.str50, %rax
	movq	$15, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_139:                              # %if.end541
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$31, %rcx
	jne	.LBB1_142
# BB#140:                               # %land.lhs.true548
	movzbl	49(%r12), %ecx
	testq	$16, %rcx
	je	.LBB1_142
# BB#141:                               # %if.then556
	movabsq	$.L.str51, %rax
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_142:                              # %if.end558
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$31, %rcx
	jne	.LBB1_145
# BB#143:                               # %land.lhs.true565
	movzbl	49(%r12), %ecx
	testq	$4, %rcx
	je	.LBB1_145
# BB#144:                               # %if.then573
	movabsq	$.L.str52, %rax
	movq	$13, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_145:                              # %if.end575
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	jne	.LBB1_148
# BB#146:                               # %land.lhs.true582
	movzbl	49(%r12), %ecx
	testq	$16, %rcx
	je	.LBB1_148
# BB#147:                               # %if.then590
	movabsq	$.L.str53, %rax
	movq	$16, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movl	16(%r12), %eax
.LBB1_148:                              # %if.end592
	andq	$255, %rax
	cmpq	$35, %rax
	jne	.LBB1_151
# BB#149:                               # %land.lhs.true599
	movzbl	50(%r12), %eax
	testq	$8, %rax
	je	.LBB1_151
# BB#150:                               # %if.then606
	movabsq	$.L.str54, %rax
	movq	$18, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
.LBB1_151:                              # %if.end608
	movq	48(%r12), %rax
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB1_153
# BB#152:                               # %if.then615
	movabsq	$.L.str55, %rax
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_153:                              # %if.end617
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_155
# BB#154:                               # %if.then624
	movabsq	$.L.str56, %rax
	movq	$13, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_155:                              # %if.end626
	movabsq	$137438953472, %rcx     # imm = 0x2000000000
	testq	%rcx, %rax
	je	.LBB1_157
# BB#156:                               # %if.then634
	movabsq	$.L.str57, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_157:                              # %if.end636
	movabsq	$274877906944, %rcx     # imm = 0x4000000000
	testq	%rcx, %rax
	je	.LBB1_159
# BB#158:                               # %if.then644
	movabsq	$.L.str58, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_159:                              # %if.end646
	movabsq	$549755813888, %rcx     # imm = 0x8000000000
	testq	%rcx, %rax
	je	.LBB1_161
# BB#160:                               # %if.then654
	movabsq	$.L.str59, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_161:                              # %if.end656
	movabsq	$1099511627776, %rcx    # imm = 0x10000000000
	testq	%rcx, %rax
	je	.LBB1_163
# BB#162:                               # %if.then664
	movabsq	$.L.str60, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_163:                              # %if.end666
	movabsq	$2199023255552, %rcx    # imm = 0x20000000000
	testq	%rcx, %rax
	je	.LBB1_165
# BB#164:                               # %if.then674
	movabsq	$.L.str61, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_165:                              # %if.end676
	movabsq	$4398046511104, %rcx    # imm = 0x40000000000
	testq	%rcx, %rax
	je	.LBB1_167
# BB#166:                               # %if.then684
	movabsq	$.L.str62, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_167:                              # %if.end686
	movabsq	$8796093022208, %rcx    # imm = 0x80000000000
	testq	%rcx, %rax
	je	.LBB1_169
# BB#168:                               # %if.then694
	movabsq	$.L.str63, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	movq	48(%r12), %rax
.LBB1_169:                              # %if.end696
	andq	$255, %r14
	movabsq	$17592186044416, %rcx   # imm = 0x100000000000
	testq	%rcx, %rax
	je	.LBB1_171
# BB#170:                               # %if.then703
	movabsq	$.L.str64, %rax
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
.LBB1_171:                              # %if.end705
	movq	mode_name(,%r14,8), %rcx
	movabsq	$.L.str5, %rdx
	xorq	%rax, %rax
	movq	%r13, %r14
	movq	%r14, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movq	24(%r12), %rcx
	movl	32(%r12), %r8d
	movabsq	$.L.str65, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	fprintf
	movq	40(%r12), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	leaq	4(%rcx), %rbx
	movabsq	$.L.str66, %rcx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movq	64(%r12), %rax
	movabsq	$.L.str67, %rcx
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB1_173
# BB#172:                               # %lor.lhs.false722
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	48(%rcx), %ecx
	testq	$1610614784, %rcx       # imm = 0x60000800
	je	.LBB1_177
.LBB1_173:                              # %if.then739
	movq	40(%rsp), %rcx          # 8-byte Reload
	leaq	3(%rcx), %r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB1_177
# BB#174:                               # %for.body.lr.ph.i2136
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%r12, %r12
	andq	%r15, %r14
.LBB1_175:                              # %for.body.i2141
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%r12
	movq	%r12, %rax
	andq	%r15, %rax
	cmpq	%r14, %rax
	jne	.LBB1_175
# BB#176:                               # %if.end741.loopexit
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
.LBB1_177:                              # %if.end741
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx), %rdx
	andq	$255, %rax
	cmpq	$30, %rax
	jne	.LBB1_178
# BB#182:                               # %if.else777
	movq	$29, %rcx
	shrq	%cl, %rdx
	andq	$3, %rdx
	je	.LBB1_186
# BB#183:                               # %if.else777
	andq	%r15, %rdx
	cmpq	$2, %rdx
	jne	.LBB1_185
# BB#184:                               # %if.then795
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	56(%rax), %ecx
	movabsq	$.L.str71, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	jmp	.LBB1_186
.LBB1_178:                              # %if.then748
	movabsq	$34359738368, %rax      # imm = 0x800000000
	testq	%rax, %rdx
	je	.LBB1_180
# BB#179:                               # %if.then755
	movabsq	$.L.str68, %rax
	movq	$5, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
.LBB1_180:                              # %if.end757
	movq	64(%rsp), %r14          # 8-byte Reload
	movl	56(%r14), %ecx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	movabsq	$.L.str69, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movzbl	16(%r14), %eax
	cmpq	$37, %rax
	jne	.LBB1_186
# BB#181:                               # %if.then768
	movabsq	$.L.str70, %rax
	movq	$14, %rsi
	movq	$1, %rdx
	movq	$1, %r14
	movq	%rax, %rdi
	movq	%r13, %r12
	movq	%r12, %rcx
	callq	fwrite
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	59(%rax), %eax
	movq	%rax, %rcx
	shlq	%cl, %r14
	movabsq	$.L.str7, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r12, %r13
	movq	%rcx, %rsi
	movq	%r14, %rdx
	callq	fprintf
	jmp	.LBB1_186
.LBB1_268:                              # %if.then1263
	movq	112(%r15), %rdx
	testq	%rdx, %rdx
	je	.LBB1_270
# BB#269:                               # %if.then1267
	movabsq	$.L.str109, %rsi
	movq	%r13, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	print_node_brief
.LBB1_270:                              # %if.end1271
	movq	24(%r15), %rdx
	movabsq	$.L.str110, %rsi
	jmp	.LBB1_265
.LBB1_266:                              # %if.then1220
	movq	24(%r15), %rdx
	movabsq	$.L.str107, %rsi
	jmp	.LBB1_265
.LBB1_330:                              # %sw.bb1473
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB1_332
# BB#331:                               # %if.then1480
	movabsq	$.L.str6, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
.LBB1_332:                              # %if.end1482
	movq	48(%r15), %rax
	movq	%rax, 216(%rsp)
	movq	32(%r15), %rax
	movq	40(%r15), %rcx
	movq	%rcx, 208(%rsp)
	movq	%rax, 200(%rsp)
	movq	48(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	32(%r15), %rax
	movq	40(%r15), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isinf
	testq	%r14, %rax
	je	.LBB1_335
# BB#333:                               # %if.then1486
	movabsq	$.L.str10, %rax
	jmp	.LBB1_334
.LBB1_341:                              # %sw.bb1513
	movq	32(%r15), %rdx
	movq	40(%rsp), %r14          # 8-byte Reload
	addq	$4, %r14
	movabsq	$.L.str125, %rsi
	movq	%r13, %rdi
	movq	%r14, %rcx
	callq	print_node
	movq	40(%r15), %rdx
	movabsq	$.L.str126, %rsi
	movq	%r13, %rdi
	movq	%r14, %rcx
	callq	print_node
	jmp	.LBB1_353
.LBB1_338:                              # %sw.bb1499
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB1_353
# BB#339:                               # %for.body1503.lr.ph
	addq	$4, 40(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	movabsq	$.L.str124, %r14
	leaq	86(%rsp), %r15
.LBB1_340:                              # %for.body1503
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	sprintf
	movq	32(%rbx), %rdx
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	print_node
	movq	(%rbx), %rbx
	incq	%r12
	testq	%rbx, %rbx
	jne	.LBB1_340
	jmp	.LBB1_353
.LBB1_342:                              # %sw.bb1517
	movq	40(%r15), %rdx
	movabsq	$.L.str127, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	fprintf
	movq	(%r15), %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %r14
	addq	$4, %rcx
	movabsq	$.L.str87, %rsi
	movq	%r13, %rdi
	cmpq	$4, %r14
	jne	.LBB1_344
# BB#343:                               # %if.then1522
	callq	print_node
	jmp	.LBB1_353
.LBB1_264:                              # %if.then1203
	movq	24(%r15), %rdx
	movabsq	$.L.str106, %rsi
.LBB1_265:                              # %if.end1290
	movq	%r13, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	print_node
.LBB1_272:                              # %if.end1290
	movq	144(%r15), %rdx
	testq	%rdx, %rdx
	je	.LBB1_274
# BB#273:                               # %if.then1294
	movabsq	$.L.str77, %rsi
	movq	%r13, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	print_node_brief
.LBB1_274:                              # %if.end1298
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	*lang_hooks+128(%rip)
	movq	72(%r15), %rdx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	je	.LBB1_275
# BB#280:                               # %if.then1305
	testq	%rax, %rax
	jg	.LBB1_281
	jmp	.LBB1_284
.LBB1_275:                              # %lor.lhs.false1301
	testq	%rax, %rax
	jg	.LBB1_276
# BB#277:                               # %lor.lhs.false1301
	movabsq	$1, %rax
	jmp	.LBB1_278
.LBB1_276:
	movabsq	$0, %rax
.LBB1_278:                              # %lor.lhs.false1301
	xorq	%rdx, %rdx
	cmpq	$0, (%r15)
	je	.LBB1_284
# BB#279:                               # %lor.lhs.false1301
	testq	%rax, %rax
	jne	.LBB1_284
.LBB1_281:                              # %for.body.lr.ph.i2190
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%rbx, %rbx
	andq	%r14, %r12
.LBB1_282:                              # %for.body.i2195
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	%r12, %rax
	jne	.LBB1_282
# BB#283:                               # %if.end1307.loopexit
	movq	72(%r15), %rdx
.LBB1_284:                              # %if.end1307
	movabsq	$.L.str112, %rsi
	movq	%r13, %rdi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	80(%r15), %rdx
	movabsq	$.L.str113, %rsi
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	(%r15), %rdx
	movabsq	$.L.str87, %rsi
	movq	%r13, %rdi
.LBB1_212:                              # %sw.epilog1576
	movq	%rbx, %rcx
	callq	print_node_brief
	jmp	.LBB1_353
.LBB1_271:                              # %if.then1282
	movq	112(%r15), %rdx
	movabsq	$.L.str111, %rsi
	movq	%r13, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	print_node_brief
	jmp	.LBB1_272
.LBB1_323:                              # %if.end1436
	testq	%rcx, %rcx
	jne	.LBB1_324
# BB#326:                               # %if.then1441
	movq	32(%rbx), %rcx
.LBB1_327:                              # %sw.epilog1576
	movabsq	$.L.str7, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	jmp	.LBB1_353
.LBB1_347:                              # %for.body1545.preheader
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r14
	movabsq	$.L.str130, %r12
.LBB1_348:                              # %for.body1545
                                        # =>This Inner Loop Header: Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 32(%rbx,%rax,8)
	je	.LBB1_350
# BB#349:                               # %indent_to.exit2133
                                        #   in Loop: Header=BB1_348 Depth=1
	xorq	%rax, %rax
	leaq	76(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%r12, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	sprintf
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	32(%rbx,%rax,8), %rax
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	leaq	76(%rsp), %rdx
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	print_node_brief
.LBB1_350:                              # %for.inc1562
                                        #   in Loop: Header=BB1_348 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	andq	%r15, %rax
	cmpq	%r14, %rax
	jne	.LBB1_348
	jmp	.LBB1_353
.LBB1_185:                              # %if.else799
	movq	64(%rsp), %rax          # 8-byte Reload
	movslq	56(%rax), %rax
	movq	built_in_class_names(,%rdx,8), %rcx
	movq	built_in_names(,%rax,8), %r8
	movabsq	$.L.str72, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	fprintf
.LBB1_186:                              # %if.end816
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	192(%rax), %rax
	cmpq	$-1, %rax
	je	.LBB1_188
# BB#187:                               # %if.then820
	movabsq	$.L.str73, %rax
	movq	$11, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %r14
	movq	%r14, %rcx
	callq	fwrite
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	192(%rax), %rcx
	movabsq	$.L.str74, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
.LBB1_188:                              # %if.end825
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$37, %rax
	jne	.LBB1_190
# BB#189:                               # %if.then832
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	88(%r12), %rax
	movabsq	$.L.str75, %rcx
	movq	%r13, %r14
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movq	160(%r12), %rax
	movabsq	$.L.str76, %rcx
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
.LBB1_190:                              # %if.end837
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	80(%r12), %rax
	movabsq	$.L.str77, %rcx
	movq	%r13, %r14
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	136(%r12), %rax
	movabsq	$.L.str78, %rcx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	112(%r12), %rax
	movabsq	$.L.str79, %rcx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	88(%r12), %rax
	movabsq	$.L.str80, %rcx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movq	96(%r12), %rax
	movabsq	$.L.str81, %rcx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movq	104(%r12), %rax
	movabsq	$.L.str82, %rcx
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node_brief
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%r12, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	callq	*lang_hooks+120(%rip)
	movq	144(%r12), %rax
	testq	%rax, %rax
	je	.LBB1_197
# BB#191:                               # %if.then854
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB1_196
# BB#192:                               # %for.body.lr.ph.i2163
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%r14, %r14
	movq	%rbx, %r12
	andq	%r15, %r12
.LBB1_193:                              # %for.body.i2168
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%r14
	movq	%r14, %rax
	andq	%r15, %rax
	cmpq	%r12, %rax
	jne	.LBB1_193
# BB#194:                               # %indent_to.exit2169
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	144(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_196
# BB#195:                               # %cond.false
	xorq	%rsi, %rsi
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB1_196:                              # %cond.end
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	print_rtl
.LBB1_197:                              # %if.end863
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$30, %rax
	jne	.LBB1_198
# BB#204:                               # %land.lhs.true895
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 160(%rax)
	je	.LBB1_209
# BB#205:                               # %if.then901
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_208
# BB#206:                               # %for.body.lr.ph.i2181
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%r14, %r14
	movq	%rbx, %r12
	andq	%r15, %r12
.LBB1_207:                              # %for.body.i2186
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%r14
	movq	%r14, %rax
	andq	%r15, %rax
	cmpq	%r12, %rax
	jne	.LBB1_207
.LBB1_208:                              # %indent_to.exit2187
	movabsq	$.L.str86, %rax
	movq	$12, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %r14
	movq	%r14, %rcx
	callq	fwrite
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	160(%rax), %rcx
	movabsq	$.L.str4, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	jmp	.LBB1_209
.LBB1_198:                              # %if.end863
	cmpq	$35, %rax
	jne	.LBB1_209
# BB#199:                               # %if.then870
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	104(%r12), %rax
	movabsq	$.L.str83, %rcx
	movq	%r13, %r14
	movq	%r14, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	movq	96(%r12), %rax
	movabsq	$.L.str84, %rcx
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	print_node
	cmpq	$0, 160(%r12)
	je	.LBB1_209
# BB#200:                               # %if.then881
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_203
# BB#201:                               # %for.body.lr.ph.i2172
	movq	$10, %rdi
	movq	%r13, %rsi
	callq	fputc
	xorq	%r14, %r14
	movq	%rbx, %r12
	andq	%r15, %r12
.LBB1_202:                              # %for.body.i2177
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r13, %rsi
	callq	fputc
	incq	%r14
	movq	%r14, %rax
	andq	%r15, %rax
	cmpq	%r12, %rax
	jne	.LBB1_202
.LBB1_203:                              # %indent_to.exit2178
	movabsq	$.L.str85, %rax
	movq	$13, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %r14
	movq	%r14, %rcx
	callq	fwrite
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	160(%rax), %rax
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rax, %rsi
	callq	print_rtl
.LBB1_209:                              # %if.end909
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	cmpq	$4, %rcx
	jne	.LBB1_211
# BB#210:                               # %if.then912
	movabsq	$.L.str87, %rcx
	movq	%r13, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	jmp	.LBB1_317
.LBB1_344:                              # %if.else1526
	callq	print_node_brief
	jmp	.LBB1_353
.LBB1_211:                              # %if.else915
	movabsq	$.L.str87, %rcx
	movq	%r13, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	jmp	.LBB1_212
.LBB1_335:                              # %if.else1488
	movq	216(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	200(%rsp), %rax
	movq	208(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%r14, %rax
	je	.LBB1_337
# BB#336:                               # %if.then1491
	movabsq	$.L.str11, %rax
.LBB1_334:                              # %sw.epilog1576
	movq	$4, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	callq	fwrite
	jmp	.LBB1_353
.LBB1_324:                              # %if.end1436.if.else1463_crit_edge
	movq	32(%rbx), %r8
.LBB1_325:                              # %if.else1463
	movabsq	$.L.str9, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	fprintf
	jmp	.LBB1_353
.LBB1_337:                              # %if.else1493
	movq	216(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	200(%rsp), %rax
	movq	208(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	96(%rsp), %rbx
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	callq	ereal_to_decimal
	movabsq	$.L.str5, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB1_353
.Ltmp26:
	.size	print_node, .Ltmp26-print_node
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_285
	.quad	.LBB1_85
	.quad	.LBB1_97
	.quad	.LBB1_77
.LJTI1_1:
	.quad	.LBB1_320
	.quad	.LBB1_330
	.quad	.LBB1_341
	.quad	.LBB1_338
	.quad	.LBB1_342
.LJTI1_2:
	.quad	.LBB1_264
	.quad	.LBB1_272
	.quad	.LBB1_272
	.quad	.LBB1_272
	.quad	.LBB1_271
	.quad	.LBB1_272
	.quad	.LBB1_268
	.quad	.LBB1_272
	.quad	.LBB1_266
	.quad	.LBB1_266
	.quad	.LBB1_267
	.quad	.LBB1_267
	.quad	.LBB1_267
	.quad	.LBB1_268

	.text
	.globl	print_node_brief
	.align	16, 0x90
	.type	print_node_brief,@function
print_node_brief:                       # @print_node_brief
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp32:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp33:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp34:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp35:
	.cfi_def_cfa_offset 40
	subq	$168, %rsp
.Ltmp36:
	.cfi_def_cfa_offset 208
.Ltmp37:
	.cfi_offset %rbx, -40
.Ltmp38:
	.cfi_offset %r12, -32
.Ltmp39:
	.cfi_offset %r14, -24
.Ltmp40:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r12, %r12
	je	.LBB2_35
# BB#1:                                 # %if.end
	movl	16(%r12), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ebx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_3
# BB#2:                                 # %if.then2
	movq	$32, %rdi
	movq	%r14, %rsi
	callq	fputc
	movl	16(%r12), %edx
.LBB2_3:                                # %if.end3
	andq	$255, %rdx
	movq	tree_code_name(,%rdx,8), %rcx
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	fprintf
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	fprintf
	andq	$255, %rbx
	cmpq	$116, %rbx
	jne	.LBB2_4
# BB#6:                                 # %if.then23
	movq	96(%r12), %rax
	testq	%rax, %rax
	je	.LBB2_12
# BB#7:                                 # %if.then26
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB2_8
# BB#9:                                 # %land.lhs.true
	movq	72(%rax), %rax
	jmp	.LBB2_10
.LBB2_4:                                # %if.end3
	cmpq	$100, %rbx
	jne	.LBB2_12
# BB#5:                                 # %if.then14
	movq	72(%r12), %rax
.LBB2_10:                               # %land.lhs.true
	testq	%rax, %rax
	jne	.LBB2_11
	jmp	.LBB2_12
.LBB2_8:                                # %if.then26
	cmpq	$1, %rcx
	jne	.LBB2_12
.LBB2_11:                               # %if.then56
	movq	32(%rax), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
.LBB2_12:                               # %if.end69
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	jne	.LBB2_14
# BB#13:                                # %if.then76
	movq	32(%r12), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movl	16(%r12), %eax
.LBB2_14:                               # %if.end81
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB2_21
# BB#15:                                # %if.then88
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB2_17
# BB#16:                                # %if.then93
	movabsq	$.L.str6, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
.LBB2_17:                               # %if.end95
	movq	$32, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	40(%r12), %rdx
	cmpq	$-1, %rdx
	jne	.LBB2_18
# BB#29:                                # %land.lhs.true110
	xorq	%rcx, %rcx
	cmpq	$0, 32(%r12)
	je	.LBB2_20
# BB#30:                                # %if.then116
	movq	$45, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	32(%r12), %rdx
	negq	%rdx
	jmp	.LBB2_28
.LBB2_18:                               # %if.end95
	testq	%rdx, %rdx
	jne	.LBB2_19
# BB#27:                                # %if.then100
	movq	32(%r12), %rdx
.LBB2_28:                               # %if.end132
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	jmp	.LBB2_21
.LBB2_19:                               # %if.end95.if.else122_crit_edge
	movq	32(%r12), %rcx
.LBB2_20:                               # %if.else122
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
.LBB2_21:                               # %if.end132
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jne	.LBB2_34
# BB#22:                                # %if.then139
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB2_24
# BB#23:                                # %if.then145
	movabsq	$.L.str6, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
.LBB2_24:                               # %if.end147
	movq	48(%r12), %rax
	movq	%rax, 160(%rsp)
	movq	32(%r12), %rax
	movq	40(%r12), %rcx
	movq	%rcx, 152(%rsp)
	movq	%rax, 144(%rsp)
	movq	48(%r12), %rax
	movq	%rax, 16(%rsp)
	movq	32(%r12), %rax
	movq	40(%r12), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isinf
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB2_31
# BB#25:                                # %if.then151
	movabsq	$.L.str10, %rdi
	jmp	.LBB2_26
.LBB2_31:                               # %if.else153
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%rbx, %rax
	je	.LBB2_33
# BB#32:                                # %if.then156
	movabsq	$.L.str11, %rdi
.LBB2_26:                               # %if.end163
	movq	$4, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
.LBB2_34:                               # %if.end163
	movq	$62, %rdi
	movq	%r14, %rsi
	callq	fputc
.LBB2_35:                               # %return
	addq	$168, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB2_33:                               # %if.else158
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rbx
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	callq	ereal_to_decimal
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB2_34
.Ltmp41:
	.size	print_node_brief, .Ltmp41-print_node_brief
	.cfi_endproc

	.globl	indent_to
	.align	16, 0x90
	.type	indent_to,@function
indent_to:                              # @indent_to
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp47:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp49:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp51:
	.cfi_def_cfa_offset 48
.Ltmp52:
	.cfi_offset %rbx, -40
.Ltmp53:
	.cfi_offset %r12, -32
.Ltmp54:
	.cfi_offset %r14, -24
.Ltmp55:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_3
# BB#1:                                 # %for.body.lr.ph
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r15
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rdi
	movq	%r14, %rsi
	callq	fputc
	incq	%rbx
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	%r15, %rax
	jne	.LBB3_2
.LBB3_3:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	indent_to, .Ltmp56-indent_to
	.cfi_endproc

	.type	table,@object           # @table
	.local	table
	.comm	table,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.zero	1
	.size	.L.str, 1

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"%s <%s "
	.size	.L.str3, 8

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%p"
	.size	.L.str4, 3

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" %s"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	" overflow"
	.size	.L.str6, 10

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%llu"
	.size	.L.str7, 5

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"0x%llx%016llx"
	.size	.L.str9, 14

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	" Inf"
	.size	.L.str10, 5

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	" Nan"
	.size	.L.str11, 5

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"type"
	.size	.L.str13, 5

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	" side-effects"
	.size	.L.str14, 14

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	" readonly"
	.size	.L.str15, 10

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	" constant"
	.size	.L.str16, 10

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	" addressable"
	.size	.L.str17, 13

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	" volatile"
	.size	.L.str18, 10

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	" unsigned"
	.size	.L.str19, 10

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	" asm_written"
	.size	.L.str20, 13

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	" used"
	.size	.L.str21, 6

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	" nothrow"
	.size	.L.str22, 9

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	" public"
	.size	.L.str23, 8

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	" private"
	.size	.L.str24, 9

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	" protected"
	.size	.L.str25, 11

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	" static"
	.size	.L.str26, 8

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	" deprecated"
	.size	.L.str27, 12

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	" tree_0"
	.size	.L.str28, 8

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	" tree_1"
	.size	.L.str29, 8

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	" tree_2"
	.size	.L.str30, 8

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	" tree_3"
	.size	.L.str31, 8

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	" tree_4"
	.size	.L.str32, 8

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	" tree_5"
	.size	.L.str33, 8

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	" tree_6"
	.size	.L.str34, 8

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	" ignored"
	.size	.L.str35, 9

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	" abstract"
	.size	.L.str36, 10

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	" in_system_header"
	.size	.L.str37, 18

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	" common"
	.size	.L.str38, 8

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	" external"
	.size	.L.str39, 10

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	" weak"
	.size	.L.str40, 6

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	" regdecl"
	.size	.L.str41, 9

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	" nonlocal"
	.size	.L.str42, 10

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	" suppress-debug"
	.size	.L.str43, 16

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	" inline"
	.size	.L.str44, 8

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	" built-in"
	.size	.L.str45, 10

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	" built-in-nonansi"
	.size	.L.str46, 18

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	" no-static-chain"
	.size	.L.str47, 17

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	" packed"
	.size	.L.str48, 8

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	" bit-field"
	.size	.L.str49, 11

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	" nonaddressable"
	.size	.L.str50, 16

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	" too-late"
	.size	.L.str51, 10

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	" error-issued"
	.size	.L.str52, 14

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	" in-text-section"
	.size	.L.str53, 17

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	" transparent-union"
	.size	.L.str54, 19

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	" virtual"
	.size	.L.str55, 9

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	" defer-output"
	.size	.L.str56, 14

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	" decl_0"
	.size	.L.str57, 8

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	" decl_1"
	.size	.L.str58, 8

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	" decl_2"
	.size	.L.str59, 8

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	" decl_3"
	.size	.L.str60, 8

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	" decl_4"
	.size	.L.str61, 8

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	" decl_5"
	.size	.L.str62, 8

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	" decl_6"
	.size	.L.str63, 8

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	" decl_7"
	.size	.L.str64, 8

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	" file %s line %d"
	.size	.L.str65, 17

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"size"
	.size	.L.str66, 5

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"unit size"
	.size	.L.str67, 10

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	" user"
	.size	.L.str68, 6

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	" align %d"
	.size	.L.str69, 10

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	" offset_align "
	.size	.L.str70, 15

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	" built-in BUILT_IN_MD %d"
	.size	.L.str71, 25

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	" built-in %s:%s"
	.size	.L.str72, 16

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	" alias set "
	.size	.L.str73, 12

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"%lld"
	.size	.L.str74, 5

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"offset"
	.size	.L.str75, 7

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"bit offset"
	.size	.L.str76, 11

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"context"
	.size	.L.str77, 8

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"attributes"
	.size	.L.str78, 11

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"abstract_origin"
	.size	.L.str79, 16

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"arguments"
	.size	.L.str80, 10

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"result"
	.size	.L.str81, 7

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"initial"
	.size	.L.str82, 8

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"arg-type"
	.size	.L.str83, 9

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"arg-type-as-written"
	.size	.L.str84, 20

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"incoming-rtl "
	.size	.L.str85, 14

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"saved-insns "
	.size	.L.str86, 13

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"chain"
	.size	.L.str87, 6

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	" no-force-blk"
	.size	.L.str88, 14

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	" sizetype"
	.size	.L.str89, 10

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	" returns-stack-depressed"
	.size	.L.str90, 25

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	" string-flag"
	.size	.L.str91, 13

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	" needs-constructing"
	.size	.L.str92, 20

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	" nonaliased-component"
	.size	.L.str93, 22

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	" ambient-boundedness"
	.size	.L.str94, 21

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	" type_0"
	.size	.L.str95, 8

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	" type_1"
	.size	.L.str96, 8

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	" type_2"
	.size	.L.str97, 8

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	" type_3"
	.size	.L.str98, 8

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	" type_4"
	.size	.L.str99, 8

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	" type_5"
	.size	.L.str100, 8

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	" type_6"
	.size	.L.str101, 8

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	" symtab %d"
	.size	.L.str102, 11

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	" precision %d"
	.size	.L.str103, 14

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"min"
	.size	.L.str104, 4

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"max"
	.size	.L.str105, 4

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"values"
	.size	.L.str106, 7

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"domain"
	.size	.L.str107, 7

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"fields"
	.size	.L.str108, 7

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"method basetype"
	.size	.L.str109, 16

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"arg-types"
	.size	.L.str110, 10

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"basetype"
	.size	.L.str111, 9

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"pointer_to_this"
	.size	.L.str112, 16

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"reference_to_this"
	.size	.L.str113, 18

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"vars"
	.size	.L.str114, 5

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"supercontext"
	.size	.L.str115, 13

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"subblocks"
	.size	.L.str116, 10

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"body"
	.size	.L.str117, 5

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"block"
	.size	.L.str118, 6

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"rtl %d "
	.size	.L.str119, 8

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"(nil)"
	.size	.L.str120, 6

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"arg %d"
	.size	.L.str121, 7

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"%s:%d:%d"
	.size	.L.str122, 9

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"(no file info)"
	.size	.L.str123, 15

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"elt%d: "
	.size	.L.str124, 8

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"real"
	.size	.L.str125, 5

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"imag"
	.size	.L.str126, 5

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	" \"%s\""
	.size	.L.str127, 6

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"purpose"
	.size	.L.str128, 8

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"value"
	.size	.L.str129, 6

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"elt %d"
	.size	.L.str130, 7


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
