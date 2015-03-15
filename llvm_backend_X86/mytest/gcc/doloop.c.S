	.file	"doloop.c.bc"
	.text
	.globl	doloop_optimize
	.align	16, 0x90
	.type	doloop_optimize,@function
doloop_optimize:                        # @doloop_optimize
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
	subq	$112, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 160
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
	movq	%rdi, %r14
	movq	loop_dump_stream(%rip), %rdi
	movq	160(%r14), %r12
	testq	%rdi, %rdi
	je	.LBB0_2
# BB#1:                                 # %if.then
	movl	124(%r14), %ecx
	movl	(%r14), %edx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB0_2:                                # %if.end
	movq	200(%r14), %rdi
	callq	prev_nonnote_insn
	movq	160(%r14), %rbx
	movq	%rax, %r13
	movq	%r13, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB0_4
# BB#3:                                 # %lor.lhs.false.i
	movq	%r13, %rdi
	callq	onlyjump_p
	testq	%r15, %rax
	je	.LBB0_4
# BB#8:                                 # %if.end6.i
	movq	96(%rbx), %rax
	movl	104(%rbx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB0_11
# BB#9:                                 # %if.then8.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#10:                                # %if.then10.i
	movabsq	$.L.str14, %rdi
	jmp	.LBB0_6
.LBB0_4:                                # %if.then.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#5:                                 # %if.then4.i
	movabsq	$.L.str13, %rdi
.LBB0_6:                                # %return
	movq	$34, %rsi
	jmp	.LBB0_7
.LBB0_11:                               # %if.end13.i
	movl	20(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_13
# BB#12:                                # %lor.lhs.false15.i
	movl	240(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB0_15
.LBB0_13:                               # %if.then17.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#14:                                # %if.then19.i
	movabsq	$.L.str15, %rdi
	movq	$40, %rsi
.LBB0_7:                                # %return
	movq	$1, %rdx
	callq	fwrite
.LBB0_141:                              # %return
	movq	%rbx, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_15:                               # %if.end22.i
	movl	24(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_18
# BB#16:                                # %if.then24.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#17:                                # %if.then26.i
	movabsq	$.L.str16, %rdi
	movq	$35, %rsi
	jmp	.LBB0_7
.LBB0_18:                               # %if.end29.i
	movl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_21
# BB#19:                                # %if.then31.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#20:                                # %if.then33.i
	movabsq	$.L.str17, %rdi
	movq	$31, %rsi
	jmp	.LBB0_7
.LBB0_21:                               # %if.end36.i
	movl	16(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_24
# BB#22:                                # %if.then38.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#23:                                # %if.then40.i
	movabsq	$.L.str18, %rdi
	movq	$37, %rsi
	jmp	.LBB0_7
.LBB0_24:                               # %if.end43.i
	movq	80(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB0_25
# BB#27:                                # %if.end50.i
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB0_28
# BB#30:                                # %if.end59.i
	movl	88(%rbx), %edx
	cmpq	$102, %rdx
	jne	.LBB0_36
# BB#31:                                # %land.lhs.true.i
	movq	8(%rcx), %rsi
	cmpq	$-1, %rsi
	je	.LBB0_36
# BB#32:                                # %land.lhs.true.i
	cmpq	$1, %rsi
	jne	.LBB0_33
.LBB0_36:                               # %if.end77.i
	testq	%rax, %rax
	jne	.LBB0_45
# BB#37:                                # %land.lhs.true80.i
	addq	$-108, %rdx
	andq	%r15, %rdx
	cmpq	$3, %rdx
	ja	.LBB0_45
# BB#38:                                # %land.lhs.true80.i
	jmpq	*.LJTI0_0(,%rdx,8)
.LBB0_40:                               # %land.lhs.true95.i
	cmpq	$0, 8(%rcx)
	jns	.LBB0_45
	jmp	.LBB0_43
.LBB0_25:                               # %if.then45.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#26:                                # %if.then47.i
	movabsq	$.L.str19, %rdi
	movq	$44, %rsi
	jmp	.LBB0_7
.LBB0_28:                               # %if.then54.i
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#29:                                # %if.then56.i
	movabsq	$.L.str20, %rdi
	movq	$43, %rsi
	jmp	.LBB0_7
.LBB0_33:                               # %if.then72.i
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB0_141
# BB#34:                                # %if.then74.i
	movabsq	$.L.str21, %rcx
	movq	$42, %rsi
	jmp	.LBB0_35
.LBB0_42:                               # %land.lhs.true117.i
	movq	8(%rcx), %rax
	cmpq	$-2, %rax
	jle	.LBB0_43
	jmp	.LBB0_45
.LBB0_39:                               # %land.lhs.true84.i
	cmpq	$0, 8(%rcx)
	jg	.LBB0_43
	jmp	.LBB0_45
.LBB0_41:                               # %land.lhs.true106.i
	movq	8(%rcx), %rax
	cmpq	$1, %rax
	jle	.LBB0_45
.LBB0_43:                               # %if.then124.i
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	je	.LBB0_45
# BB#44:                                # %if.then126.i
	movabsq	$.L.str22, %rcx
	movq	$50, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
.LBB0_45:                               # %if.end5
	leaq	104(%rsp), %rsi
	leaq	96(%rsp), %rdx
	leaq	88(%rsp), %rcx
	leaq	84(%rsp), %r8
	movq	%r14, %rdi
	callq	precondition_loop_p
	testq	%r15, %rax
	je	.LBB0_46
# BB#48:                                # %if.end13
	movq	96(%r12), %rbx
	testq	%rbx, %rbx
	movq	%rbx, %rsi
	jne	.LBB0_76
# BB#49:                                # %if.else
	movq	$8, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	find_reg_note
	movq	80(%r12), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movslq	84(%rsp), %r13
	movq	$63, %rcx
	movq	%rsi, %rax
	sarq	%cl, %rax
	movl	88(%r12), %edx
	leaq	(%rsi,%rax), %rcx
	xorq	%rax, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	js	.LBB0_50
# BB#51:                                # %if.else.i
	leaq	56(%r12), %rcx
	leaq	64(%r12), %rax
	jmp	.LBB0_52
.LBB0_46:                               # %if.then8
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB0_141
# BB#47:                                # %if.then10
	movabsq	$.L.str1, %rcx
	movq	$34, %rsi
	jmp	.LBB0_35
.LBB0_50:                               # %if.then1.i
	movq	%rdx, %rdi
	callq	swap_condition
	movq	%rax, %rdx
	leaq	64(%r12), %rcx
	leaq	56(%r12), %rax
.LBB0_52:                               # %if.end5.i
	movq	(%rax), %rax
	movq	(%rcx), %rcx
	leaq	-106(%rdx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jb	.LBB0_61
# BB#53:                                # %if.end5.i
	movq	%rdx, %rsi
	andq	%r15, %rsi
	cmpq	$102, %rsi
	je	.LBB0_66
# BB#54:                                # %if.end5.i
	addq	$-110, %rdx
	andq	%r15, %rdx
	cmpq	$1, %rdx
	ja	.LBB0_74
# BB#55:                                # %sw.bb.i
	movzbl	(%rcx), %esi
	xorq	%rdx, %rdx
	cmpq	$54, %rsi
	jne	.LBB0_57
# BB#56:                                # %if.then7.i
	movq	8(%rcx), %rdx
.LBB0_57:                               # %if.end12.i
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	je	.LBB0_58
# BB#59:                                # %if.else20.i
	movzbl	mode_bitsize(%r13,%r13), %ecx
	decq	%rcx
	movq	$2, %rax
	jmp	.LBB0_60
.LBB0_61:                               # %sw.bb27.i
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB0_63
# BB#62:                                # %if.then32.i
	movq	8(%rcx), %rdx
	jmp	.LBB0_64
.LBB0_66:                               # %sw.bb63.i
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB0_69
# BB#67:                                # %land.lhs.true.i149
	movzbl	(%rax), %edx
	cmpq	$54, %rdx
	jne	.LBB0_69
# BB#68:                                # %if.then72.i150
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	subq	%rcx, %rax
	jmp	.LBB0_70
.LBB0_63:                               # %if.else36.i
	movzbl	mode_bitsize(%r13,%r13), %ecx
	decq	%rcx
	movq	$-1, %rdx
	shlq	%cl, %rdx
	andq	%r15, %rdx
.LBB0_64:                               # %if.end44.i
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB0_65
.LBB0_58:                               # %if.then16.i
	movq	8(%rax), %rax
	subq	%rdx, %rax
	jmp	.LBB0_70
.LBB0_69:                               # %if.else80.i
	movzbl	mode_bitsize(%r13,%r13), %ecx
	decq	%rcx
	movq	$2, %rax
	shlq	%cl, %rax
	decq	%rax
	andq	%r15, %rax
	jmp	.LBB0_70
.LBB0_65:                               # %if.else53.i
	movzbl	mode_bitsize(%r13,%r13), %ecx
	decq	%rcx
	movq	$1, %rax
.LBB0_60:                               # %sw.epilog.i
	shlq	%cl, %rax
	decq	%rax
	andq	%r15, %rax
	subq	%rdx, %rax
.LBB0_70:                               # %sw.epilog.i
	xorq	%rdx, %rdx
	divq	64(%rsp)                # 8-byte Folded Reload
	movq	%rax, %rsi
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB0_73
# BB#71:                                # %land.lhs.true90.i
	movzbl	mode_bitsize(%r13,%r13), %ecx
	decq	%rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rsi
	jbe	.LBB0_73
# BB#72:                                # %if.then99.i
	decq	%rax
	andq	%r15, %rax
	movq	%rax, %rsi
.LBB0_73:                               # %doloop_iterations_max.exit
	testq	%rsi, %rsi
	je	.LBB0_74
.LBB0_76:                               # %if.end26
	movl	104(%r12), %ecx
	xorq	%rdx, %rdx
	movq	%rbx, %rax
	divq	%rcx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_80
# BB#77:                                # %if.end26
	cmpq	$2, 64(%rsp)            # 8-byte Folded Reload
	ja	.LBB0_80
# BB#78:                                # %if.then34
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_141
# BB#79:                                # %if.then36
	movabsq	$.L.str3, %rdx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	fprintf
	jmp	.LBB0_141
.LBB0_74:                               # %if.then20
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB0_141
# BB#75:                                # %if.then22
	movabsq	$.L.str2, %rcx
	movq	$25, %rsi
	jmp	.LBB0_35
.LBB0_80:                               # %if.end39
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%rcx
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	movq	%r13, 56(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	callq	gen_label_rtx
	movl	84(%rsp), %ecx
	movq	%rax, %r12
	movq	%rcx, %rdi
	callq	gen_reg_rtx
	movslq	124(%r14), %rcx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	callq	gen_rtx_CONST_INT
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%r12, %r8
	callq	gen_doloop_end
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB0_85
# BB#81:                                # %land.lhs.true49
	movl	84(%rsp), %ecx
	movl	word_mode(%rip), %eax
	cmpq	%rax, %rcx
	je	.LBB0_83
# BB#82:                                # %if.end57
	movq	72(%rsp), %r13          # 8-byte Reload
	movb	%al, 2(%r13)
	movslq	124(%r14), %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r13, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%r12, %r8
	callq	gen_doloop_end
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB0_83
.LBB0_85:                               # %if.end64
	movzbl	(%r13), %eax
	cmpq	$24, %rax
	movq	%r13, %rax
	jne	.LBB0_88
# BB#86:                                # %if.then69
	movq	8(%r13), %rax
	movl	(%rax), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB0_103
# BB#87:                                # %if.end84
	movq	32(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_103
.LBB0_88:                               # %lor.lhs.false
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	jne	.LBB0_103
# BB#89:                                # %if.end.i
	movq	8(%rax), %rcx
	movq	16(%rcx), %rax
	movzbl	(%rax), %edx
	cmpq	$47, %rdx
	jne	.LBB0_103
# BB#90:                                # %lor.lhs.false.i157
	movq	8(%rax), %rax
	movzbl	(%rax), %edx
	cmpq	$61, %rdx
	jne	.LBB0_103
# BB#91:                                # %if.end17.i
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_103
# BB#92:                                # %lor.lhs.false24.i
	movq	8(%rdx), %rcx
	movq	global_rtl(%rip), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB0_103
# BB#93:                                # %lor.lhs.false29.i
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$72, %rsi
	jne	.LBB0_103
# BB#94:                                # %lor.lhs.false36.i
	movq	16(%rdx), %rsi
	movzbl	(%rsi), %esi
	cmpq	$67, %rsi
	jne	.LBB0_103
# BB#95:                                # %lor.lhs.false46.i
	movq	24(%rdx), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB0_103
# BB#96:                                # %if.end55.i
	movq	8(%rdx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movzbl	(%rcx), %ecx
	cmpq	$102, %rcx
	je	.LBB0_98
# BB#97:                                # %if.end55.i
	cmpq	$104, %rcx
	jne	.LBB0_103
.LBB0_98:                               # %lor.lhs.false68.i
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB0_103
# BB#99:                                # %if.end76.i
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	cmpq	%rax, %rcx
	je	.LBB0_102
# BB#100:                               # %if.end82.i
	movzbl	(%rcx), %edx
	cmpq	$75, %rdx
	jne	.LBB0_103
# BB#101:                               # %land.lhs.true89.i
	movq	8(%rcx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB0_103
.LBB0_102:                              # %doloop_condition_get.exit
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB0_103
# BB#105:                               # %if.end93
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB0_107
# BB#106:                               # %if.then96
	movq	%r14, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB0_140
.LBB0_83:                               # %if.then59
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB0_141
# BB#84:                                # %if.then61
	movabsq	$.L.str4, %rcx
	movq	$48, %rsi
	jmp	.LBB0_35
.LBB0_103:                              # %if.then88
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB0_141
# BB#104:                               # %if.then90
	movabsq	$.L.str5, %rcx
	movq	$39, %rsi
.LBB0_35:                               # %return
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	jmp	.LBB0_141
.LBB0_107:                              # %if.else98
	movq	160(%r14), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	80(%rax), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	32(%rax), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	48(%rax), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	8(%rdx), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movl	88(%rax), %edx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	$63, %rcx
	movq	%rsi, %rax
	sarq	%cl, %rax
	leaq	(%rsi,%rax), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	leaq	-102(%rdx), %rcx
	movq	%rcx, %rdx
	andq	%r15, %rdx
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	cmpq	$9, %rdx
	ja	.LBB0_109
# BB#108:                               # %switch.lookup.i
	andq	$1023, %rcx             # imm = 0x3FF
	movq	$961, %rdx              # imm = 0x3C1
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
.LBB0_109:                              # %lor.end.i
	xorq	%rax, 32(%rsp)          # 8-byte Folded Spill
	callq	start_sequence
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	js	.LBB0_111
# BB#110:                               # %lor.end.i
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB0_111:                              # %lor.end.i
	js	.LBB0_113
# BB#112:                               # %lor.end.i
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 40(%rsp)          # 8-byte Spill
.LBB0_113:                              # %lor.end.i
	movq	%rax, %rdi
	callq	copy_rtx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	$76, %rsi
	xorq	%r8, %r8
	movq	$3, (%rsp)
	movl	84(%rsp), %ecx
	movq	%rcx, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	movq	16(%rsp), %r9           # 8-byte Reload
	callq	expand_simple_binop
	movq	216(%r14), %rdx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB0_123
# BB#114:                               # %if.then20.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rax
	movq	160(%r14), %rcx
	movl	8(%rax), %esi
	movq	208(%rcx), %rax
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	(%rax,%rdi), %ecx
	cmpq	$1, %rcx
	je	.LBB0_117
# BB#115:                               # %if.then20.i
	cmpq	$3, %rcx
	jne	.LBB0_126
# BB#116:                               # %if.then43.i
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	8(%rax,%rsi), %rcx
	movq	16(%rcx), %rcx
	movl	8(%rcx), %esi
.LBB0_117:                              # %if.end61.i167
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	8(%rax,%rsi), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	movslq	8(%rax), %rax
	movslq	max_uid_for_loop(%rip), %rsi
	cmpq	%rsi, %rax
	jge	.LBB0_123
# BB#118:                               # %cond.true71.i
	movslq	8(%rdx), %rcx
	cmpq	%rsi, %rcx
	jge	.LBB0_128
# BB#119:                               # %cond.true87.i
	movq	uid_luid(%rip), %rdx
	movslq	(%rdx,%rax,4), %rax
	movslq	(%rdx,%rcx,4), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_123
# BB#120:                               # %if.then98.i
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	je	.LBB0_122
# BB#121:                               # %if.then100.i
	movabsq	$.L.str7, %rcx
	movq	$48, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
.LBB0_122:                              # %if.end102.i
	movq	$75, %rsi
	movq	$3, (%rsp)
	movl	84(%rsp), %eax
	movq	%rax, %rdi
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, %r8
	movq	16(%rsp), %r9           # 8-byte Reload
	callq	expand_simple_binop
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB0_123:                              # %if.end105.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	104(%rax), %eax
	movq	32(%rsp), %rcx          # 8-byte Reload
	imulq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB0_134
# BB#124:                               # %if.then108.i
	movq	%rax, %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB0_125
# BB#129:                               # %if.end116.i
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movzbl	2(%rdx), %edx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rsi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	%rax, 24(%rsp)          # 8-byte Spill
	cmpq	$1, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB0_131
# BB#130:
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jmp	.LBB0_134
.LBB0_126:                              # %if.else59.i
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.doloop_modify_runtime, %rcx
	movq	$652, %rsi              # imm = 0x28C
	jmp	.LBB0_127
.LBB0_128:                              # %cond.false94.i
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.doloop_modify_runtime, %rcx
	movq	$655, %rsi              # imm = 0x28F
	jmp	.LBB0_127
.LBB0_125:                              # %if.then115.i
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.doloop_modify_runtime, %rcx
	movq	$675, %rsi              # imm = 0x2A3
.LBB0_127:                              # %if.else59.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB0_131:                              # %if.then122.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	104(%rax), %eax
	movq	32(%rsp), %rcx          # 8-byte Reload
	imulq	%rcx, %rax
	decq	%rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	24(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %ecx
	movq	$83, %rsi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rcx, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	%rax, 40(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	104(%rcx), %ecx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rax, 16(%rsp)          # 8-byte Spill
	decq	8(%rsp)                 # 8-byte Folded Spill
	andq	%r15, 8(%rsp)           # 8-byte Folded Spill
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	imulq	%rax, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	gen_rtx_CONST_INT
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	movq	$103, %rdx
	je	.LBB0_133
# BB#132:                               # %if.then122.i
	movq	$110, %rdx
.LBB0_133:                              # %if.then122.i
	movq	40(%rsp), %rsi          # 8-byte Reload
	movzbl	2(%rsi), %r8d
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, (%rsp)
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	emit_cmp_and_jump_insns
	callq	get_last_insn
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 64(%rax)
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	32(%rax), %eax
	incq	%rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 32(%rcx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %r15d
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	expand_simple_binop
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
.LBB0_134:                              # %if.end158.i
	cmpq	$0, 168(%r14)
	jne	.LBB0_139
# BB#135:                               # %if.then160.i
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	je	.LBB0_137
# BB#136:                               # %if.then163.i
	movabsq	$.L.str8, %rcx
	movq	$23, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
.LBB0_137:                              # %if.end165.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movl	104(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB0_139
# BB#138:                               # %if.then169.i
	callq	gen_label_rtx
	movq	%rax, %r15
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	40(%rcx), %rcx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%rcx, %rdi
	callq	copy_rtx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r15, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movl	84(%rsp), %eax
	movq	%rax, %r8
	callq	emit_cmp_and_jump_insns
	callq	get_last_insn
	movq	%r15, 64(%rax)
	movl	32(%r15), %eax
	incq	%rax
	movl	%eax, 32(%r15)
	movq	const_int_rtx+520(%rip), %rax
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r15, %rdi
	callq	emit_label
.LBB0_139:                              # %doloop_modify_runtime.exit
	callq	gen_sequence
	movq	%rax, %r15
	callq	end_sequence
	movq	192(%r14), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_insn_before
	movq	%r14, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB0_140:                              # %return
	movq	%rbx, %rdx
	movq	%r13, %rcx
	movq	%r12, %r8
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	doloop_modify
	movq	$1, %rbx
	jmp	.LBB0_141
.Ltmp17:
	.size	doloop_optimize, .Ltmp17-doloop_optimize
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_40
	.quad	.LBB0_42
	.quad	.LBB0_39
	.quad	.LBB0_41

	.text
	.align	16, 0x90
	.type	doloop_modify,@function
doloop_modify:                          # @doloop_modify
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
	movq	%rdi, %r13
	movq	200(%r13), %rdi
	movq	%r9, %r15
	movq	%r8, %r14
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	callq	prev_nonnote_insn
	movq	loop_dump_stream(%rip), %rcx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	je	.LBB1_5
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	movq	$34, %rsi
	movq	$1, %rdx
	callq	fwrite
	movzbl	(%r12), %eax
	movq	loop_dump_stream(%rip), %rcx
	cmpq	$54, %rax
	jne	.LBB1_3
# BB#2:                                 # %if.then2
	movq	8(%r12), %rdx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	jmp	.LBB1_4
.LBB1_3:                                # %if.else
	movabsq	$.L.str11, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB1_4:                                # %if.end
	movq	loop_dump_stream(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$13, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB1_5:                                # %if.end6
	movq	208(%r13), %rsi
	testq	%rsi, %rsi
	jne	.LBB1_7
# BB#6:                                 # %cond.false
	movq	192(%r13), %rsi
.LBB1_7:                                # %cond.end
	movq	%r14, %rdi
	callq	emit_label_after
	movl	32(%r14), %eax
	incq	%rax
	movl	%eax, 32(%r14)
	movq	%rbx, %rdi
	callq	delete_related_insns
	movq	8(%r15), %rbx
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB1_9
# BB#8:                                 # %if.then18
	movq	8(%rbx), %rbx
.LBB1_9:                                # %if.end22
	callq	start_sequence
	movzbl	(%r15), %eax
	cmpq	$104, %rax
	jne	.LBB1_10
# BB#15:                                # %sw.bb38
	movq	16(%r15), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_16
# BB#17:                                # %if.end44
	movzbl	2(%rbx), %eax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	decq	%rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	ja	.LBB1_18
# BB#19:                                # %if.end44
	movabsq	$1, %r15
	jmp	.LBB1_20
.LBB1_10:                               # %if.end22
	cmpq	$102, %rax
	jne	.LBB1_27
# BB#11:                                # %sw.bb
	movq	16(%r15), %rax
	movq	const_int_rtx+512(%rip), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	je	.LBB1_20
# BB#12:                                # %if.else30
	movq	const_int_rtx+520(%rip), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	je	.LBB1_24
# BB#13:                                # %if.then35
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.doloop_modify, %rcx
	movq	$458, %rsi              # imm = 0x1CA
	jmp	.LBB1_14
.LBB1_18:
	movabsq	$0, %r15
.LBB1_20:                               # %if.then57
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	jne	.LBB1_22
# BB#21:                                # %if.then62
	movq	8(%r12), %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB1_23
.LBB1_22:                               # %if.else68
	movzbl	2(%rbx), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	gen_rtx_CONST_INT
	movq	$76, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	expand_simple_binop
.LBB1_23:                               # %if.end75
	movq	%rax, %r12
.LBB1_24:                               # %if.end75
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	convert_move
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	192(%r13), %rsi
	movq	%rbx, %rdi
	callq	emit_insn_before
	movq	200(%r13), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	emit_jump_insn_before
	movq	200(%r13), %rdi
	callq	prev_nonnote_insn
	movq	%rax, %rbx
	movq	%r14, 64(%rbx)
	testq	%r15, %r15
	je	.LBB1_26
# BB#25:                                # %if.then87
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$8, %rsi
	xorq	%rdx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB1_26:                               # %if.end95
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_16:                               # %if.then43
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.doloop_modify, %rcx
	movq	$464, %rsi              # imm = 0x1D0
	jmp	.LBB1_14
.LBB1_27:                               # %sw.default
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.doloop_modify, %rcx
	movq	$478, %rsi              # imm = 0x1DE
.LBB1_14:                               # %if.then35
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp35:
	.size	doloop_modify, .Ltmp35-doloop_modify
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Doloop: Processing loop %d, enclosed levels %d.\n"
	.size	.L.str, 49

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Doloop: Cannot precondition loop.\n"
	.size	.L.str1, 35

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Doloop: Not normal loop.\n"
	.size	.L.str2, 26

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Doloop: Too few iterations (%ld) to be profitable.\n"
	.size	.L.str3, 52

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Doloop: Target unwilling to use doloop pattern!\n"
	.size	.L.str4, 49

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Doloop: Unrecognizable doloop pattern!\n"
	.size	.L.str5, 40

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"doloop.c"
	.size	.L.str6, 9

	.type	.L__FUNCTION__.doloop_modify_runtime,@object # @__FUNCTION__.doloop_modify_runtime
.L__FUNCTION__.doloop_modify_runtime:
	.asciz	"doloop_modify_runtime"
	.size	.L__FUNCTION__.doloop_modify_runtime, 22

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Doloop: Basic induction var skips initial incr.\n"
	.size	.L.str7, 49

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Doloop: Do-while loop.\n"
	.size	.L.str8, 24

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Doloop: Inserting doloop pattern ("
	.size	.L.str9, 35

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%lld"
	.size	.L.str10, 5

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"runtime"
	.size	.L.str11, 8

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	" iterations)."
	.size	.L.str12, 14

	.type	.L__FUNCTION__.doloop_modify,@object # @__FUNCTION__.doloop_modify
.L__FUNCTION__.doloop_modify:
	.asciz	"doloop_modify"
	.size	.L__FUNCTION__.doloop_modify, 14

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Doloop: Invalid jump at loop end.\n"
	.size	.L.str13, 35

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Doloop: Loop completely unrolled.\n"
	.size	.L.str14, 35

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Doloop: Loop has multiple exit targets.\n"
	.size	.L.str15, 41

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Doloop: Indirect jump in function.\n"
	.size	.L.str16, 36

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Doloop: Function call in loop.\n"
	.size	.L.str17, 32

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Doloop: Computed branch in the loop.\n"
	.size	.L.str18, 38

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Doloop: Could not determine iteration info.\n"
	.size	.L.str19, 45

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"Doloop: Increment not an integer constant.\n"
	.size	.L.str20, 44

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Doloop: NE loop with non-unity increment.\n"
	.size	.L.str21, 43

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Doloop: Possible infinite iteration case ignored.\n"
	.size	.L.str22, 51


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
