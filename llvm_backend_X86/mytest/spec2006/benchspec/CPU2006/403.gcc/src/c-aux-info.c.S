	.file	"c-aux-info.c.bc"
	.text
	.globl	gen_aux_info_record
	.align	16, 0x90
	.type	gen_aux_info_record,@function
gen_aux_info_record:                    # @gen_aux_info_record
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	movl	flag_gen_aux_info(%rip), %eax
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %r15
	testq	%rax, %rax
	je	.LBB0_24
# BB#1:                                 # %if.then
	movl	gen_aux_info_record.compiled_from_record(%rip), %eax
	testq	%rax, %rax
	leaq	1(%rax), %rax
	movl	%eax, gen_aux_info_record.compiled_from_record(%rip)
	jne	.LBB0_3
# BB#2:                                 # %if.then2
	movq	aux_info_file(%rip), %rcx
	movq	%rdi, %r14
	movabsq	$.L.str, %rdi
	movq	$23, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	%r14, %rdi
.LBB0_3:                                # %if.end
	movq	aux_info_file(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	24(%rdi), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	32(%rdi), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$73, %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %r12
	jne	.LBB0_8
# BB#4:                                 # %cond.false
	testq	%r14, %rbx
	jne	.LBB0_5
# BB#6:                                 # %cond.false
	movabsq	$1, %r13
	jmp	.LBB0_7
.LBB0_24:                               # %if.end18
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_5:
	movabsq	$0, %r13
.LBB0_7:                                # %cond.false
	orq	$78, %r13
.LBB0_8:                                # %cond.end
	andq	%r15, %r14
	je	.LBB0_9
# BB#10:                                # %cond.end
	movabsq	$1, %rax
	jmp	.LBB0_11
.LBB0_9:
	movabsq	$0, %rax
.LBB0_11:                               # %cond.end
	leaq	67(%rax,%rax,2), %rbx
	xorq	%rdx, %rdx
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	movq	%r15, %rsi
	callq	gen_decl
	movq	%rax, %rcx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%r13, %r8
	movq	%rbx, %r9
	callq	fprintf
	testq	%r14, %r14
	je	.LBB0_23
# BB#12:                                # %if.then12
	movq	88(%r12), %rbx
	movq	%r12, %r14
	movq	aux_info_file(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$.L.str4, %r13
	testq	%rbx, %rbx
	movq	%r13, %r8
	je	.LBB0_19
# BB#13:
	movabsq	$.L.str4, %r15
	movabsq	$.L.str5, %r12
	movq	%r13, %r8
	.align	16, 0x90
.LBB0_14:                               # %while.body.us.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r8), %eax
	testq	%rax, %rax
	je	.LBB0_16
# BB#15:                                # %if.then.us.i
                                        #   in Loop: Header=BB0_14 Depth=1
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r8, %rdi
	movq	%r12, %rsi
	callq	concat
	movq	%rax, %r8
.LBB0_16:                               # %if.end11.us.i
                                        #   in Loop: Header=BB0_14 Depth=1
	movq	72(%rbx), %rax
	testq	%rax, %rax
	movq	%r15, %rsi
	je	.LBB0_18
# BB#17:                                # %if.then.i
                                        #   in Loop: Header=BB0_14 Depth=1
	movq	32(%rax), %rsi
.LBB0_18:                               # %gen_decl.exit
                                        #   in Loop: Header=BB0_14 Depth=1
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r8, %rdi
	callq	concat
	movq	(%rbx), %rbx
	movq	%rax, %r8
	testq	%rbx, %rbx
	jne	.LBB0_14
.LBB0_19:                               # %gen_formal_list_for_func_def.exit
	movabsq	$.L.str9, %rdi
	movabsq	$.L.str10, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r8, %rsi
	callq	concat
	movq	88(%r14), %rbx
	movq	%rax, %r15
	testq	%rbx, %rbx
	je	.LBB0_22
# BB#20:
	movabsq	$.L.str6, %r12
	.align	16, 0x90
.LBB0_21:                               # %if.end11.us69.i
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	gen_decl
	movq	%rax, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rsi
	movq	%r12, %rdx
	callq	concat
	movq	(%rbx), %rbx
	movq	%rax, %r13
	testq	%rbx, %rbx
	jne	.LBB0_21
.LBB0_22:                               # %gen_formal_list_for_func_def.exit30
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	fprintf
.LBB0_23:                               # %if.end16
	movq	aux_info_file(%rip), %rsi
	movq	$10, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp17:
	.size	gen_aux_info_record, .Ltmp17-gen_aux_info_record
	.cfi_endproc

	.align	16, 0x90
	.type	gen_decl,@function
gen_decl:                               # @gen_decl
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
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	72(%r14), %rax
	movq	%rdx, %r15
	movq	%rsi, %r12
	testq	%rax, %rax
	je	.LBB1_1
# BB#2:                                 # %if.then
	movq	32(%rax), %rbx
	jmp	.LBB1_3
.LBB1_1:
	movabsq	$.L.str4, %rbx
.LBB1_3:                                # %if.end
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	je	.LBB1_18
# BB#4:                                 # %if.end5
	movl	16(%r14), %ecx
	testq	$2048, %rcx             # imm = 0x800
	je	.LBB1_6
# BB#5:                                 # %if.then7
	movabsq	$.L.str11, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	concat
	movl	16(%r14), %ecx
	movq	%rax, %rbx
.LBB1_6:                                # %if.end8
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB1_8
# BB#7:                                 # %if.then14
	movabsq	$.L.str12, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	concat
	movq	%rax, %rbx
.LBB1_8:                                # %if.end16
	movq	$.L.str4, data_type(%rip)
	movzbl	16(%r14), %eax
	cmpq	$30, %rax
	jne	.LBB1_11
# BB#9:                                 # %if.end16
	testq	%r13, %r12
	je	.LBB1_11
# BB#10:                                # %if.then22
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	gen_formal_list_for_func_def
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	concat
	movq	8(%r14), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %rdi
	jmp	.LBB1_12
.LBB1_11:                               # %if.else29
	movq	8(%r14), %rsi
	movq	%rbx, %rdi
.LBB1_12:                               # %if.end33
	movq	%r15, %rdx
	callq	gen_type
	movq	%rax, %rdi
	callq	affix_data_type
	movl	16(%r14), %ecx
	movq	%rax, %rbx
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$30, %rax
	je	.LBB1_15
# BB#13:                                # %land.lhs.true40
	movzbl	49(%r14), %eax
	testq	$4, %rax
	je	.LBB1_15
# BB#14:                                # %if.then46
	movabsq	$.L.str13, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	concat
	movl	16(%r14), %ecx
	movq	%rax, %rbx
.LBB1_15:                               # %if.end48
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB1_17
# BB#16:                                # %if.then54
	movabsq	$.L.str14, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	concat
	movl	16(%r14), %ecx
	movq	%rax, %rbx
.LBB1_17:                               # %if.end56
	andq	$524543, %rcx           # imm = 0x800FF
	cmpq	$30, %rcx
	jne	.LBB1_18
# BB#19:                                # %if.then69
	movabsq	$.L.str15, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	concat  # TAILCALL
.LBB1_18:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	gen_decl, .Ltmp34-gen_decl
	.cfi_endproc

	.align	16, 0x90
	.type	gen_formal_list_for_func_def,@function
gen_formal_list_for_func_def:           # @gen_formal_list_for_func_def
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp41:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp43:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp44:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 48
.Ltmp46:
	.cfi_offset %rbx, -48
.Ltmp47:
	.cfi_offset %r12, -40
.Ltmp48:
	.cfi_offset %r13, -32
.Ltmp49:
	.cfi_offset %r14, -24
.Ltmp50:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	88(%r15), %rbx
	movq	%rsi, %r14
	testq	%rbx, %rbx
	je	.LBB2_1
# BB#2:                                 # %while.body.lr.ph
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	movabsq	$.L.str4, %r12
	cmpq	$2, %rax
	jae	.LBB2_7
# BB#3:
	movabsq	$.L.str5, %r13
	.align	16, 0x90
.LBB2_4:                                # %while.body.us
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_6
# BB#5:                                 # %if.then.us
                                        #   in Loop: Header=BB2_4 Depth=1
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	concat
	movq	%rax, %r12
.LBB2_6:                                # %if.end11.us
                                        #   in Loop: Header=BB2_4 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	gen_decl
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rsi
	callq	concat
	movq	(%rbx), %rbx
	movq	%rax, %r12
	testq	%rbx, %rbx
	jne	.LBB2_4
	jmp	.LBB2_11
.LBB2_1:
	movabsq	$.L.str4, %r12
	jmp	.LBB2_11
.LBB2_7:                                # %while.body.lr.ph.while.body.lr.ph.split_crit_edge
	jne	.LBB2_10
# BB#8:
	movabsq	$.L.str6, %r13
	.align	16, 0x90
.LBB2_9:                                # %if.end11.us69
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	gen_decl
	movq	%rax, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rdx, %rsi
	movq	%r13, %rdx
	callq	concat
	movq	(%rbx), %rbx
	movq	%rax, %r12
	testq	%rbx, %rbx
	jne	.LBB2_9
	jmp	.LBB2_11
	.align	16, 0x90
.LBB2_10:                               # %if.end11
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	gen_decl
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rsi
	callq	concat
	movq	(%rbx), %rbx
	movq	%rax, %r12
	testq	%rbx, %rbx
	jne	.LBB2_10
.LBB2_11:                               # %while.end
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r14
	jne	.LBB2_19
# BB#12:                                # %if.then14
	cmpq	$0, 88(%r15)
	jne	.LBB2_14
# BB#13:                                # %if.then18
	movabsq	$.L.str7, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	concat
	movq	%rax, %r12
.LBB2_14:                               # %if.end20
	movq	8(%r15), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB2_19
# BB#15:                                # %land.rhs.lr.ph.i
	movq	global_trees+216(%rip), %rcx
	.align	16, 0x90
.LBB2_16:                               # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rdx
	cmpq	%rcx, %rdx
	je	.LBB2_19
# BB#17:                                # %while.body.i
                                        #   in Loop: Header=BB2_16 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_16
# BB#18:                                # %if.then24
	movabsq	$.L.str8, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	concat
	movq	%rax, %r12
.LBB2_19:                               # %if.end27
	andq	%rbx, %r14
	cmpq	$1, %r14
	ja	.LBB2_20
# BB#21:                                # %if.then33
	movabsq	$.L.str9, %rdi
	movabsq	$.L.str10, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r12, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	concat  # TAILCALL
.LBB2_20:                               # %if.end35
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp51:
	.size	gen_formal_list_for_func_def, .Ltmp51-gen_formal_list_for_func_def
	.cfi_endproc

	.align	16, 0x90
	.type	gen_type,@function
gen_type:                               # @gen_type
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
	subq	$32, %rsp
.Ltmp63:
	.cfi_def_cfa_offset 80
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
	movq	96(%r14), %rax
	movq	%rdx, %r15
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB3_3
# BB#1:                                 # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB3_3
# BB#2:                                 # %if.then
	movq	72(%rax), %rax
	movq	32(%rax), %rax
	movq	%rax, data_type(%rip)
	leaq	16(%r14), %r12
	jmp	.LBB3_80
.LBB3_3:                                # %if.else
	movl	16(%r14), %ecx
	leaq	16(%r14), %r12
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$32, %rdx
	jg	.LBB3_63
# BB#4:                                 # %if.else
	cmpq	$23, %rdx
	ja	.LBB3_77
# BB#5:                                 # %if.else
	jmpq	*.LJTI3_0(,%rdx,8)
.LBB3_76:                               # %sw.bb228
	movq	$.L.str29, data_type(%rip)
	jmp	.LBB3_80
.LBB3_63:                               # %if.else
	cmpq	$33, %rdx
	jne	.LBB3_77
# BB#64:                                # %sw.bb167
	movq	72(%r14), %rax
	movq	32(%rax), %rax
	jmp	.LBB3_79
.LBB3_78:                               # %sw.bb88
	movq	32(%r14), %rax
	jmp	.LBB3_79
.LBB3_75:                               # %sw.bb227
	movq	$.L.str7, data_type(%rip)
	jmp	.LBB3_80
.LBB3_66:                               # %sw.bb173
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	movq	%rsi, data_type(%rip)
	movl	(%r12), %edx
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB3_80
# BB#67:                                # %land.lhs.true186
	movq	$12, %rcx
	movq	%rdx, %r8
	shrq	%cl, %r8
	movq	$10, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	movl	60(%r14), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$255, %rdx
	xorq	%rbx, %rbx
	cmpq	$20, %rdx
	jne	.LBB3_71
# BB#68:                                # %land.rhs
	cmpq	$0, 8(%r14)
	je	.LBB3_69
# BB#70:                                # %land.rhs
	movabsq	$1, %rbx
	jmp	.LBB3_71
.LBB3_74:                               # %sw.bb219
	movq	72(%rax), %rax
	movq	32(%rax), %rax
	jmp	.LBB3_79
.LBB3_54:                               # %sw.bb137
	testq	%rax, %rax
	je	.LBB3_57
# BB#55:                                # %if.then141
	movq	32(%rax), %rsi
	jmp	.LBB3_56
.LBB3_6:                                # %sw.bb
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB3_8
# BB#7:                                 # %if.then14
	movabsq	$.L.str12, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	concat
	movl	(%r12), %ecx
	movq	%rax, %r13
.LBB3_8:                                # %if.end
	testq	$2048, %rcx             # imm = 0x800
	je	.LBB3_10
# BB#9:                                 # %if.then20
	movabsq	$.L.str11, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	concat
	movq	%rax, %r13
.LBB3_10:                               # %if.end22
	movabsq	$.L.str17, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	concat
	movq	8(%r14), %rsi
	movzbl	16(%rsi), %ecx
	movq	%rax, %rbx
	cmpq	$18, %rcx
	je	.LBB3_12
# BB#11:                                # %if.end22
	cmpq	$23, %rcx
	jne	.LBB3_13
.LBB3_12:                               # %if.then38
	movabsq	$.L.str18, %rdi
	movabsq	$.L.str10, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	concat
	movq	8(%r14), %rsi
	movq	%rax, %rbx
.LBB3_13:                               # %if.end40
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	gen_type
	jmp	.LBB3_86
.LBB3_14:                               # %sw.bb44
	movq	32(%r14), %rax
	testq	%rax, %rax
	je	.LBB3_16
# BB#15:                                # %lor.lhs.false47
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB3_16
# BB#17:                                # %if.else60
	movq	%r14, %rdi
	callq	int_size_in_bytes
	testq	%rax, %rax
	je	.LBB3_18
# BB#19:                                # %if.else68
	movq	%r14, %rdi
	callq	int_size_in_bytes
	movq	8(%r14), %rdi
	movq	%rax, %rbx
	callq	int_size_in_bytes
	movq	%rax, %rcx
	movq	%rbx, %rax
	cqto
	idivq	%rcx
	movq	%rax, %rcx
	movabsq	$.L.str21, %rsi
	leaq	22(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	sprintf
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	jmp	.LBB3_39
.LBB3_40:                               # %sw.bb92
	testq	%rax, %rax
	je	.LBB3_42
# BB#41:                                # %if.then96
	movq	32(%rax), %rsi
	jmp	.LBB3_46
.LBB3_47:                               # %sw.bb112
	testq	%rax, %rax
	je	.LBB3_49
# BB#48:                                # %if.then116
	movq	32(%rax), %rsi
	jmp	.LBB3_53
.LBB3_20:                               # %sw.bb82
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB3_22
# BB#21:
	movabsq	$.L.str32, %rsi
	jmp	.LBB3_38
.LBB3_77:                               # %sw.default
	movabsq	$.L.str30, %rdi
	movabsq	$.L__FUNCTION__.gen_type, %rdx
	movq	$471, %rsi              # imm = 0x1D7
	callq	fancy_abort
.LBB3_16:                               # %if.then55
	movabsq	$.L.str19, %rsi
	jmp	.LBB3_38
.LBB3_57:                               # %if.else147
	movq	$.L.str4, data_type(%rip)
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB3_58
# BB#60:                                # %while.body152.lr.ph
	movabsq	$.L.str4, %rdi
	movabsq	$.L.str5, %r15
	jmp	.LBB3_61
	.align	16, 0x90
.LBB3_62:                               # %while.cond150.backedge
                                        #   in Loop: Header=BB3_61 Depth=1
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%r15, %rsi
	callq	concat
	movq	%rax, %rdi
	movq	%rdi, data_type(%rip)
.LBB3_61:                               # %while.body152
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	movq	32(%rax), %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	concat
	movq	%rax, %rsi
	movq	%rsi, data_type(%rip)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_62
	jmp	.LBB3_59
.LBB3_42:                               # %if.else102
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	$.L.str4, data_type(%rip)
	movq	24(%r14), %rbx
	movabsq	$.L.str4, %r15
	testq	%rbx, %rbx
	je	.LBB3_45
# BB#43:                                # %while.body.lr.ph
	movabsq	$.L.str6, %r12
	.align	16, 0x90
.LBB3_44:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	gen_decl
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	concat
	movq	%rax, %rcx
	movq	%rcx, data_type(%rip)
	movq	(%rbx), %rbx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r12, %rsi
	callq	concat
	movq	%rax, %r15
	testq	%rbx, %rbx
	movq	%r15, data_type(%rip)
	jne	.LBB3_44
.LBB3_45:                               # %while.end
	movabsq	$.L.str22, %rdi
	movabsq	$.L.str23, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	concat
	movq	%rax, %rsi
	movq	8(%rsp), %r12           # 8-byte Reload
.LBB3_46:                               # %if.end110
	movq	%rsi, data_type(%rip)
	movabsq	$.L.str24, %rdi
	jmp	.LBB3_73
.LBB3_49:                               # %if.else122
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	$.L.str4, data_type(%rip)
	movq	24(%r14), %rbx
	movabsq	$.L.str4, %r15
	testq	%rbx, %rbx
	je	.LBB3_52
# BB#50:                                # %while.body127.lr.ph
	movabsq	$.L.str6, %r12
	.align	16, 0x90
.LBB3_51:                               # %while.body127
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	gen_decl
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	concat
	movq	%rax, %rcx
	movq	%rcx, data_type(%rip)
	movq	(%rbx), %rbx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r12, %rsi
	callq	concat
	movq	%rax, %r15
	testq	%rbx, %rbx
	movq	%r15, data_type(%rip)
	jne	.LBB3_51
.LBB3_52:                               # %while.end133
	movabsq	$.L.str22, %rdi
	movabsq	$.L.str23, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	concat
	movq	%rax, %rsi
	movq	8(%rsp), %r12           # 8-byte Reload
.LBB3_53:                               # %if.end135
	movq	%rsi, data_type(%rip)
	movabsq	$.L.str25, %rdi
	jmp	.LBB3_73
.LBB3_22:                               # %if.end.i
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	24(%r14), %rbx
	movabsq	$.L.str4, %r12
	jmp	.LBB3_23
.LBB3_32:                               # %cond.end.i
                                        #   in Loop: Header=BB3_23 Depth=1
	callq	concat
	movq	%rax, %r12
	movq	(%rbx), %rbx
.LBB3_23:                               # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB3_24
# BB#26:                                # %land.end.i
                                        #   in Loop: Header=BB3_23 Depth=1
	movq	32(%rbx), %rsi
	movq	global_trees+216(%rip), %rcx
	movzbl	(%r12), %eax
	cmpq	%rcx, %rsi
	je	.LBB3_33
# BB#27:                                # %while.body.i
                                        #   in Loop: Header=BB3_23 Depth=1
	testq	$255, %rax
	je	.LBB3_29
# BB#28:                                # %if.then3.i
                                        #   in Loop: Header=BB3_23 Depth=1
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str5, %rsi
	callq	concat
	movq	32(%rbx), %rsi
	movq	%rax, %r12
.LBB3_29:                               # %if.end4.i
                                        #   in Loop: Header=BB3_23 Depth=1
	xorq	%rdx, %rdx
	movabsq	$.L.str4, %rdi
	callq	gen_type
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_30
# BB#31:                                # %cond.false.i
                                        #   in Loop: Header=BB3_23 Depth=1
	movq	data_type(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	jmp	.LBB3_32
.LBB3_30:                               # %cond.true.i
                                        #   in Loop: Header=BB3_23 Depth=1
	movq	%rax, %rdi
	callq	affix_data_type
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rsi
	jmp	.LBB3_32
.LBB3_24:                               # %land.end.i.thread
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB3_34
# BB#25:                                # %if.then22.i
	movabsq	$.L.str8, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	concat
	movq	%rax, %r12
	jmp	.LBB3_37
.LBB3_69:
	movabsq	$0, %rbx
.LBB3_71:                               # %land.end
	andq	$1, %r8
	andq	$2, %rdi
	orq	%r8, %rdi
	andq	$4, %rax
	orq	%rdi, %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	orq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB3_80
# BB#72:                                # %if.then216
	movabsq	$.L.str28, %rdi
	jmp	.LBB3_73
.LBB3_58:
	movabsq	$.L.str4, %rsi
.LBB3_59:                               # %while.end163
	movabsq	$.L.str22, %rdi
	movabsq	$.L.str26, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	callq	concat
	movq	%rax, %rsi
.LBB3_56:                               # %if.end165
	movq	%rsi, data_type(%rip)
	movabsq	$.L.str27, %rdi
.LBB3_73:                               # %if.end229
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	concat
.LBB3_79:                               # %if.end229
	movq	%rax, data_type(%rip)
.LBB3_80:                               # %if.end229
	movl	(%r12), %ecx
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB3_82
# BB#81:                                # %if.then236
	movabsq	$.L.str12, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	concat
	movl	(%r12), %ecx
	movq	%rax, %r13
.LBB3_82:                               # %if.end238
	testq	$2048, %rcx             # imm = 0x800
	je	.LBB3_84
# BB#83:                                # %if.then245
	movabsq	$.L.str11, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	concat
	movq	%rax, %r13
.LBB3_84:                               # %if.end247
	movzbl	62(%r14), %eax
	testq	$32, %rax
	je	.LBB3_87
# BB#85:                                # %if.then254
	movabsq	$.L.str31, %rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	concat
.LBB3_86:                               # %return
	movq	%rax, %r13
.LBB3_87:                               # %return
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_18:                               # %if.then63
	movabsq	$.L.str20, %rsi
	jmp	.LBB3_38
.LBB3_33:                               # %while.end.i
	testq	$255, %rax
	jne	.LBB3_37
.LBB3_34:                               # %if.then14.i
	cmpq	$0, 24(%r14)
	je	.LBB3_35
# BB#36:                                # %select.mid
	movabsq	$.L.str7, %r12
	jmp	.LBB3_37
.LBB3_35:
	movabsq	$.L.str33, %r12
.LBB3_37:                               # %if.end25.i
	movabsq	$.L.str9, %rdi
	movabsq	$.L.str10, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	concat
	movq	%rax, %rsi
	movq	8(%rsp), %r12           # 8-byte Reload
.LBB3_38:                               # %gen_formal_list_for_type.exit
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
.LBB3_39:                               # %if.end229
	callq	concat
	movq	8(%r14), %rsi
	movq	%rax, %rdi
	movq	%r15, %rdx
	callq	gen_type
	movq	%rax, %r13
	jmp	.LBB3_80
.Ltmp69:
	.size	gen_type, .Ltmp69-gen_type
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_76
	.quad	.LBB3_78
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_75
	.quad	.LBB3_66
	.quad	.LBB3_74
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_54
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_6
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_77
	.quad	.LBB3_14
	.quad	.LBB3_77
	.quad	.LBB3_40
	.quad	.LBB3_47
	.quad	.LBB3_77
	.quad	.LBB3_20

	.text
	.align	16, 0x90
	.type	affix_data_type,@function
affix_data_type:                        # @affix_data_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp73:
	.cfi_def_cfa_offset 16
.Ltmp74:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp75:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp76:
	.cfi_offset %rbx, -56
.Ltmp77:
	.cfi_offset %r12, -48
.Ltmp78:
	.cfi_offset %r13, -40
.Ltmp79:
	.cfi_offset %r14, -32
.Ltmp80:
	.cfi_offset %r15, -24
	movq	%rdi, %rbx
	movq	%rbx, libiberty_optr(%rip)
                                        # kill: RDI<def> RBX<kill>
	callq	strlen
	leaq	1(%rax), %rdx
	movq	%rdx, libiberty_len(%rip)
	movq	%rsp, %r14
	addq	$16, %rax
	andq	$-16, %rax
	subq	%rax, %r14
	movq	%r14, %rsp
	movq	%r14, libiberty_nptr(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	memcpy
	movabsq	$.L.str11, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$.L.str12, %r12
	movq	%r14, %rbx
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_2:                                # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	addq	$9, %rbx
.LBB4_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	$9, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strncmp
	testq	%r13, %rax
	je	.LBB4_2
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	$6, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	strncmp
	testq	%r13, %rax
	jne	.LBB4_5
# BB#4:                                 # %if.then4
                                        #   in Loop: Header=BB4_1 Depth=1
	addq	$6, %rbx
	jmp	.LBB4_1
.LBB4_5:                                # %for.end
	cmpq	%r14, %rbx
	je	.LBB4_6
# BB#7:                                 # %if.end9
	movzbl	(%rbx), %r15d
	movq	data_type(%rip), %rsi
	xorq	%rax, %rax
	movb	%al, (%rbx)
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	concat
	movq	%rax, %rcx
	movb	%r15b, (%rbx)
	movabsq	$.L.str16, %rdx
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rcx, %rsi
	movq	%rbx, %rcx
	callq	reconcat
	jmp	.LBB4_8
.LBB4_6:                                # %if.then7
	movq	data_type(%rip), %rdi
	movabsq	$.L.str16, %rsi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	concat
.LBB4_8:                                # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp81:
	.size	affix_data_type, .Ltmp81-affix_data_type
	.cfi_endproc

	.type	gen_aux_info_record.compiled_from_record,@object # @gen_aux_info_record.compiled_from_record
	.local	gen_aux_info_record.compiled_from_record
	.comm	gen_aux_info_record.compiled_from_record,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"/* compiled from: . */\n"
	.size	.L.str, 24

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"/* %s:%d:%c%c */ %s;"
	.size	.L.str1, 21

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	" /*%s %s*/"
	.size	.L.str2, 11

	.type	libiberty_optr,@object  # @libiberty_optr
	.comm	libiberty_optr,8,8
	.type	libiberty_nptr,@object  # @libiberty_nptr
	.comm	libiberty_nptr,8,8
	.type	libiberty_len,@object   # @libiberty_len
	.comm	libiberty_len,8,8
	.type	.L.str4,@object         # @.str4
.L.str4:
	.zero	1
	.size	.L.str4, 1

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	", "
	.size	.L.str5, 3

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"; "
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"void"
	.size	.L.str7, 5

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	", ..."
	.size	.L.str8, 6

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	" ("
	.size	.L.str9, 3

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	")"
	.size	.L.str10, 2

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"volatile "
	.size	.L.str11, 10

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"const "
	.size	.L.str12, 7

	.type	data_type,@object       # @data_type
	.local	data_type
	.comm	data_type,8,8
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"register "
	.size	.L.str13, 10

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"extern "
	.size	.L.str14, 8

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"static "
	.size	.L.str15, 8

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	" "
	.size	.L.str16, 2

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"*"
	.size	.L.str17, 2

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"("
	.size	.L.str18, 2

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"[]"
	.size	.L.str19, 3

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"[0]"
	.size	.L.str20, 4

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"[%d]"
	.size	.L.str21, 5

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"{ "
	.size	.L.str22, 3

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"}"
	.size	.L.str23, 2

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"struct "
	.size	.L.str24, 8

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"union "
	.size	.L.str25, 7

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	" }"
	.size	.L.str26, 3

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"enum "
	.size	.L.str27, 6

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"unsigned "
	.size	.L.str28, 10

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"[ERROR]"
	.size	.L.str29, 8

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"c-aux-info.c"
	.size	.L.str30, 13

	.type	.L__FUNCTION__.gen_type,@object # @__FUNCTION__.gen_type
.L__FUNCTION__.gen_type:
	.asciz	"gen_type"
	.size	.L__FUNCTION__.gen_type, 9

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"restrict "
	.size	.L.str31, 10

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"()"
	.size	.L.str32, 3

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"/* ??? */"
	.size	.L.str33, 10


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
