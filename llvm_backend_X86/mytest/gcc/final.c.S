	.file	"final.c.bc"
	.text
	.globl	init_final
	.align	16, 0x90
	.type	init_final,@function
init_final:                             # @init_final
	.cfi_startproc
# BB#0:                                 # %entry
	movl	ix86_asm_dialect(%rip), %eax
	xorq	%rcx, %rcx
	movb	%cl, app_on(%rip)
	movq	$0, final_sequence(%rip)
	movl	%eax, dialect_number(%rip)
	retq
.Ltmp0:
	.size	init_final, .Ltmp0-init_final
	.cfi_endproc

	.globl	end_final
	.align	16, 0x90
	.type	end_final,@function
end_final:                              # @end_final
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp4:
	.cfi_def_cfa_offset 16
.Ltmp5:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp6:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
.Ltmp7:
	.cfi_offset %rbx, -56
.Ltmp8:
	.cfi_offset %r12, -48
.Ltmp9:
	.cfi_offset %r13, -40
.Ltmp10:
	.cfi_offset %r14, -32
.Ltmp11:
	.cfi_offset %r15, -24
	movl	profile_arc_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_9
# BB#1:                                 # %if.then
	movq	%rdi, -72(%rbp)         # 8-byte Spill
	movq	$16, %rdi
	callq	exact_log2_wide
	movq	%rax, %r14
	movl	target_flags(%rip), %ebx
	movq	$20, %rcx
	shrq	%cl, %rbx
	andq	$32, %rbx
	movl	count_instrumented_edges(%rip), %eax
	addq	$32, %rbx
	movq	$3, %rcx
	movq	%rbx, %r13
	sarq	%cl, %r13
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, -80(%rbp)         # 8-byte Spill
	callq	data_section
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB1_3
# BB#2:                                 # %if.then13
	movq	asm_out_file(%rip), %rdi
	movq	$1, %rdx
	movq	%r14, %rcx
	shlq	%cl, %rdx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB1_3:                                # %do.body
	movq	%r14, -88(%rbp)         # 8-byte Spill
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str3, %r14
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movabsq	$.L.str4, %r12
	leaq	-64(%rbp), %r15
	movq	$1, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	sprintf
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_s
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	sprintf
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$68, %rdi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_s
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movslq	count_instrumented_edges(%rip), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	imulq	$11, %rax, %rax
	addq	$44, %rax
	movabsq	$4294967292, %rsi       # imm = 0xFFFFFFFC
	andq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	const_int_rtx+512(%rip), %rdi
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	assemble_integer
	movl	target_flags(%rip), %esi
	movq	const_int_rtx+512(%rip), %rdi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	$1, %rcx
	movq	%rbx, %rdx
	callq	assemble_integer
	movq	asm_out_file(%rip), %rdi
	movq	$1, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str2, %rsi
	movq	%r14, %rdx
	callq	fprintf
	callq	getpwd
	movq	%rax, %r15
	movq	-72(%rbp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	strlen
	leaq	1(%rbx,%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movabsq	$17179869184, %rdx      # imm = 0x400000000
	addq	%rax, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$15, %rdx
	andq	$-16, %rdx
	movq	%rsp, %rbx
	subq	%rdx, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	%rbx, %rdi
	callq	strlen
	movq	$47, %rcx
	movw	%cx, (%rbx,%rax)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	strcat
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	strip_off_ending
	movq	%rbx, %rdi
	callq	strlen
	movq	$6382638, %rcx          # imm = 0x61642E
	movl	%ecx, (%rax,%rbx)
	movq	%rbx, %rdi
	callq	strlen
	leaq	1(%rax), %rsi
	movq	%rbx, %rdi
	callq	assemble_string
	movl	count_instrumented_edges(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rcx, %rax
	je	.LBB1_4
# BB#7:                                 # %do.body89
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str3, %rdx
	leaq	-64(%rbp), %rbx
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str8, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str10, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str11, %rsi
	movq	$16, %rcx
	xorq	%rax, %rax
	movq	-80(%rbp), %rdx         # 8-byte Reload
	jmp	.LBB1_8
.LBB1_4:                                # %if.then75
	movq	-88(%rbp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB1_6
# BB#5:                                 # %if.then78
	movq	asm_out_file(%rip), %rdi
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB1_6:                                # %do.body82
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str3, %rdx
	movq	$2, %rcx
	xorq	%rax, %rax
.LBB1_8:                                # %if.end104
	callq	fprintf
.LBB1_9:                                # %if.end104
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp12:
	.size	end_final, .Ltmp12-end_final
	.cfi_endproc

	.globl	default_function_pro_epilogue
	.align	16, 0x90
	.type	default_function_pro_epilogue,@function
default_function_pro_epilogue:          # @default_function_pro_epilogue
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp13:
	.size	default_function_pro_epilogue, .Ltmp13-default_function_pro_epilogue
	.cfi_endproc

	.globl	no_asm_to_stream
	.align	16, 0x90
	.type	no_asm_to_stream,@function
no_asm_to_stream:                       # @no_asm_to_stream
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp14:
	.size	no_asm_to_stream, .Ltmp14-no_asm_to_stream
	.cfi_endproc

	.globl	app_enable
	.align	16, 0x90
	.type	app_enable,@function
app_enable:                             # @app_enable
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 16
.Ltmp18:
	.cfi_offset %rbx, -16
	movzbl	app_on(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_2
# BB#1:                                 # %if.then
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	callq	fwrite
	movb	%bl, app_on(%rip)
.LBB4_2:                                # %if.end
	popq	%rbx
	retq
.Ltmp19:
	.size	app_enable, .Ltmp19-app_enable
	.cfi_endproc

	.globl	app_disable
	.align	16, 0x90
	.type	app_disable,@function
app_disable:                            # @app_disable
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp21:
	.cfi_def_cfa_offset 16
	movzbl	app_on(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB5_2
# BB#1:                                 # %if.then
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str13, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB5_2:                                # %if.end
	popq	%rax
	retq
.Ltmp22:
	.size	app_disable, .Ltmp22-app_disable
	.cfi_endproc

	.globl	init_insn_lengths
	.align	16, 0x90
	.type	init_insn_lengths,@function
init_insn_lengths:                      # @init_insn_lengths
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp24:
	.cfi_def_cfa_offset 16
	movq	uid_shuid(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB6_2
# BB#1:                                 # %if.then
	callq	free
	movq	$0, uid_shuid(%rip)
.LBB6_2:                                # %if.end
	movq	insn_lengths(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB6_4
# BB#3:                                 # %if.then2
	callq	free
	movq	$0, insn_lengths(%rip)
	xorq	%rax, %rax
	movl	%eax, insn_lengths_max_uid(%rip)
.LBB6_4:                                # %do.body
	movq	insn_addresses_(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB6_6
# BB#5:                                 # %if.then5
	callq	free
	movq	$0, insn_addresses_(%rip)
.LBB6_6:                                # %do.end
	movq	uid_align(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB6_8
# BB#7:                                 # %if.then8
	callq	free
	movq	$0, uid_align(%rip)
.LBB6_8:                                # %if.end9
	popq	%rax
	retq
.Ltmp25:
	.size	init_insn_lengths, .Ltmp25-init_insn_lengths
	.cfi_endproc

	.globl	get_attr_length
	.align	16, 0x90
	.type	get_attr_length,@function
get_attr_length:                        # @get_attr_length
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp30:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 32
.Ltmp33:
	.cfi_offset %rbx, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movslq	insn_lengths_max_uid(%rip), %rcx
	movslq	8(%r15), %rax
	cmpq	%rax, %rcx
	jle	.LBB7_2
# BB#1:                                 # %if.then
	movq	insn_lengths(%rip), %rcx
	movl	(%rcx,%rax,4), %ebx
	jmp	.LBB7_24
.LBB7_2:                                # %if.else
	movzbl	(%r15), %eax
	xorq	%rbx, %rbx
	cmpq	$32, %rax
	je	.LBB7_7
# BB#3:                                 # %if.else
	cmpq	$33, %rax
	jne	.LBB7_4
# BB#6:                                 # %sw.bb6
	movq	32(%r15), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB7_5
	jmp	.LBB7_24
.LBB7_7:                                # %sw.bb18
	movq	32(%r15), %r14
	movl	(%r14), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-48(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$2, %rsi
	jb	.LBB7_24
# BB#8:                                 # %sw.bb18
	cmpq	$40, %rax
	je	.LBB7_10
# BB#9:                                 # %lor.lhs.false34
	movq	%r14, %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movl	(%r14), %edx
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB7_19
.LBB7_10:                               # %if.then37
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$40, %rdx
	jne	.LBB7_12
# BB#11:                                # %if.then.i
	movq	8(%r14), %rax
	jmp	.LBB7_13
.LBB7_4:                                # %if.else
	cmpq	$34, %rax
	jne	.LBB7_24
	jmp	.LBB7_5
.LBB7_12:                               # %if.else.i
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	callq	decode_asm_operands
.LBB7_13:                               # %for.cond.i
	movq	$1, %r14
	jmp	.LBB7_14
	.align	16, 0x90
.LBB7_18:                               # %for.inc.i
                                        #   in Loop: Header=BB7_14 Depth=1
	incq	%rax
.LBB7_14:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$10, %rcx
	je	.LBB7_17
# BB#15:                                # %for.cond.i
                                        #   in Loop: Header=BB7_14 Depth=1
	testq	%rcx, %rcx
	je	.LBB7_23
# BB#16:                                # %for.cond.i
                                        #   in Loop: Header=BB7_14 Depth=1
	cmpq	$59, %rcx
	jne	.LBB7_18
.LBB7_17:                               # %if.then6.i
                                        #   in Loop: Header=BB7_14 Depth=1
	incq	%r14
	jmp	.LBB7_18
.LBB7_23:                               # %asm_insn_count.exit
	movq	%r15, %rdi
	callq	insn_default_length
	movq	%rax, %rbx
	imulq	%r14, %rbx
.LBB7_24:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB7_19:                               # %if.else40
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$24, %rdx
	jne	.LBB7_5
# BB#20:                                # %for.cond.preheader
	movq	8(%r14), %rdx
	movslq	(%rdx), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB7_24
# BB#21:
	movq	$1, %r15
.LBB7_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%r15,8), %rdi
	callq	get_attr_length
	movq	8(%r14), %rdx
	addq	%rax, %rbx
	movslq	(%rdx), %rax
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rax, %rsi
	jl	.LBB7_22
	jmp	.LBB7_24
.LBB7_5:                                # %sw.bb5
	movq	%r15, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	insn_default_length  # TAILCALL
.Ltmp36:
	.size	get_attr_length, .Ltmp36-get_attr_length
	.cfi_endproc

	.globl	label_to_alignment
	.align	16, 0x90
	.type	label_to_alignment,@function
label_to_alignment:                     # @label_to_alignment
	.cfi_startproc
# BB#0:                                 # %entry
	movl	48(%rdi), %eax
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	label_align(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movswq	(%rdx,%rax,4), %rax
	retq
.Ltmp37:
	.size	label_to_alignment, .Ltmp37-label_to_alignment
	.cfi_endproc

	.globl	insn_current_reference_address
	.align	16, 0x90
	.type	insn_current_reference_address,@function
insn_current_reference_address:         # @insn_current_reference_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp44:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp45:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp46:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp47:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 48
.Ltmp49:
	.cfi_offset %rbx, -48
.Ltmp50:
	.cfi_offset %r12, -40
.Ltmp51:
	.cfi_offset %r13, -32
.Ltmp52:
	.cfi_offset %r14, -24
.Ltmp53:
	.cfi_offset %r15, -16
	movq	insn_addresses_(%rip), %r8
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB9_20
# BB#1:                                 # %if.end
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB9_2
# BB#3:                                 # %if.end8
	movq	16(%rdi), %rax
	movq	64(%rdi), %rcx
	movq	24(%rax), %rax
	movslq	8(%rax), %rax
	movq	uid_shuid(%rip), %r9
	movslq	8(%rcx), %rcx
	movslq	(%r9,%rax,4), %r12
	movslq	(%r9,%rcx,4), %r13
	cmpq	%r13, %r12
	jge	.LBB9_12
# BB#4:                                 # %if.then22
	movq	uid_align(%rip), %r11
	movq	insn_lengths(%rip), %r10
	movq	(%r11,%rax,8), %rsi
	movl	(%r10,%rax,4), %eax
	xorq	%r12, %r12
	testq	%rsi, %rsi
	je	.LBB9_11
# BB#5:                                 # %for.body.lr.ph.i
	movl	length_unit_log(%rip), %ecx
	movq	label_align(%rip), %r15
	movq	$1, %r14
	shlq	%cl, %r14
	xorq	%r12, %r12
	.align	16, 0x90
.LBB9_6:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%rsi), %rbx
	movslq	(%r9,%rbx,4), %rcx
	cmpq	%r13, %rcx
	jg	.LBB9_11
# BB#7:                                 # %if.end.i
                                        #   in Loop: Header=BB9_6 Depth=1
	movl	48(%rsi), %edx
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	(%r15,%rdx,4), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jge	.LBB9_9
# BB#8:                                 #   in Loop: Header=BB9_6 Depth=1
	movq	%r14, %rdx
	jmp	.LBB9_10
	.align	16, 0x90
.LBB9_9:                                # %if.end26.i
                                        #   in Loop: Header=BB9_6 Depth=1
	movl	32(%r8,%rbx,4), %ecx
	movl	(%r10,%rbx,4), %esi
	subq	%rcx, %rsi
	notq	%rsi
	movq	%rdx, %rcx
	subq	%r14, %rcx
	andq	%rsi, %rcx
	addq	%rcx, %r12
.LBB9_10:                               # %for.inc.i
                                        #   in Loop: Header=BB9_6 Depth=1
	movq	(%r11,%rbx,8), %rsi
	testq	%rsi, %rsi
	movq	%rdx, %r14
	jne	.LBB9_6
.LBB9_11:                               # %align_fuzz.exit
	movl	insn_last_address(%rip), %ecx
	addq	%rcx, %rax
	subq	%r12, %rax
	jmp	.LBB9_20
.LBB9_2:                                # %if.then7
	movl	insn_current_address(%rip), %eax
	jmp	.LBB9_20
.LBB9_12:                               # %if.else
	movq	uid_align(%rip), %r14
	movq	(%r14,%rcx,8), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB9_19
# BB#13:                                # %for.body.lr.ph.i47
	movl	length_unit_log(%rip), %ecx
	movq	insn_lengths(%rip), %r10
	movl	min_labelno(%rip), %r11d
	movq	label_align(%rip), %r15
	movq	$1, %r13
	shlq	%cl, %r13
	xorq	%rax, %rax
	.align	16, 0x90
.LBB9_14:                               # %for.body.i58
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%rsi), %rdi
	movslq	(%r9,%rdi,4), %rcx
	cmpq	%r12, %rcx
	jg	.LBB9_19
# BB#15:                                # %if.end.i67
                                        #   in Loop: Header=BB9_14 Depth=1
	movl	48(%rsi), %esi
	subq	%r11, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movswq	(%r15,%rsi,4), %rcx
	movq	$1, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	jge	.LBB9_17
# BB#16:                                #   in Loop: Header=BB9_14 Depth=1
	movq	%r13, %rbx
	jmp	.LBB9_18
	.align	16, 0x90
.LBB9_17:                               # %if.end26.i73
                                        #   in Loop: Header=BB9_14 Depth=1
	movl	32(%r8,%rdi,4), %ecx
	movl	(%r10,%rdi,4), %edx
	subq	%rcx, %rdx
	notq	%rdx
	movq	%rbx, %rcx
	subq	%r13, %rcx
	andq	%rdx, %rcx
	addq	%rcx, %rax
.LBB9_18:                               # %for.inc.i79
                                        #   in Loop: Header=BB9_14 Depth=1
	movq	(%r14,%rdi,8), %rsi
	testq	%rsi, %rsi
	movq	%rbx, %r13
	jne	.LBB9_14
.LBB9_19:                               # %align_fuzz.exit81
	movl	insn_current_address(%rip), %ecx
	addq	%rcx, %rax
.LBB9_20:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp54:
	.size	insn_current_reference_address, .Ltmp54-insn_current_reference_address
	.cfi_endproc

	.globl	compute_alignments
	.align	16, 0x90
	.type	compute_alignments,@function
compute_alignments:                     # @compute_alignments
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp61:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp62:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp63:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp64:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp66:
	.cfi_def_cfa_offset 80
.Ltmp67:
	.cfi_offset %rbx, -48
.Ltmp68:
	.cfi_offset %r12, -40
.Ltmp69:
	.cfi_offset %r13, -32
.Ltmp70:
	.cfi_offset %r14, -24
.Ltmp71:
	.cfi_offset %r15, -16
	movq	label_align(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB10_2
# BB#1:                                 # %if.then
	callq	free
	movq	$0, label_align(%rip)
.LBB10_2:                               # %if.end
	callq	max_label_num
	movl	%eax, max_labelno(%rip)
	callq	get_first_label_num
	movl	%eax, min_labelno(%rip)
	movl	max_labelno(%rip), %ecx
	movq	$1, %rdi
	subq	%rax, %rdi
	addq	%rcx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rsi
	callq	xcalloc
	movl	optimize(%rip), %ecx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, label_align(%rip)
	testq	%rcx, %rcx
	je	.LBB10_36
# BB#3:                                 # %if.end
	movl	optimize_size(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB10_36
# BB#4:                                 # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	testq	%rdx, %rdx
	jle	.LBB10_36
# BB#5:                                 # %for.body.lr.ph
	movq	basic_block_info(%rip), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movabsq	$3777893186295716171, %r13 # imm = 0x346DC5D63886594B
	.align	16, 0x90
.LBB10_6:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_8 Depth 2
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rax,%rsi,8), %r8
	movq	(%r8), %r12
	movzbl	(%r12), %eax
	cmpq	$36, %rax
	jne	.LBB10_35
# BB#7:                                 # %if.end12
                                        #   in Loop: Header=BB10_6 Depth=1
	movl	align_labels_log(%rip), %r15d
	movl	align_labels_max_skip(%rip), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	32(%r8), %r10
	xorq	%r9, %r9
	testq	%r10, %r10
	movabsq	$0, %rbx
	movabsq	$0, %rdi
	je	.LBB10_15
	.align	16, 0x90
.LBB10_8:                               # %for.body15
                                        #   Parent Loop BB10_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	48(%r10), %r11d
	movq	$63, %rcx
	movq	%r11, %r14
	shlq	%cl, %r14
	movq	16(%r10), %rax
	movq	$63, %rcx
	sarq	%cl, %r14
	movl	104(%rax), %ecx
	movl	52(%r10), %eax
	movq	(%r10), %r10
	imulq	%rcx, %rax
	addq	$5000, %rax             # imm = 0x1388
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r13
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$11, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	andq	$1, %r11
	je	.LBB10_10
# BB#9:                                 # %for.body15
                                        #   in Loop: Header=BB10_8 Depth=2
	movq	$1, %r9
.LBB10_10:                              # %for.body15
                                        #   in Loop: Header=BB10_8 Depth=2
	andq	%rdx, %r14
	testq	%r11, %r11
	je	.LBB10_12
# BB#11:                                # %for.body15
                                        #   in Loop: Header=BB10_8 Depth=2
	xorq	%rdx, %rdx
.LBB10_12:                              # %for.body15
                                        #   in Loop: Header=BB10_8 Depth=2
	addq	%r14, %rdi
	addq	%rdx, %rbx
	testq	%r10, %r10
	jne	.LBB10_8
# BB#13:                                # %for.end
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	$1, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r9
	jne	.LBB10_23
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1000, %rax             # imm = 0x3E8
	jg	.LBB10_18
	.align	16, 0x90
.LBB10_15:                              # %lor.lhs.false31
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	24(%rax,%rsi,8), %rax
	movl	104(%rax), %edx
	movslq	104(%r8), %r8
	leaq	(%rdx,%rdx), %rcx
	leaq	(%rcx,%rcx,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r8
	jle	.LBB10_16
# BB#17:                                # %land.lhs.true42
                                        #   in Loop: Header=BB10_6 Depth=1
	movl	entry_exit_blocks+104(%rip), %r8d
	movq	$31, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	addq	%r8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$33, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rcx, %rcx
	cmpq	%rax, %rdx
	jg	.LBB10_23
.LBB10_18:                              # %if.then52
                                        #   in Loop: Header=BB10_6 Depth=1
	movslq	align_jumps_log(%rip), %rdx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jge	.LBB10_20
# BB#19:                                #   in Loop: Header=BB10_6 Depth=1
	movl	align_jumps_max_skip(%rip), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB10_20:                              # %if.then52
                                        #   in Loop: Header=BB10_6 Depth=1
	jl	.LBB10_22
# BB#21:                                # %if.then52
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%r15, %rdx
.LBB10_22:                              # %if.then52
                                        #   in Loop: Header=BB10_6 Depth=1
	xorq	%rcx, %rcx
	movq	%rdx, %r15
.LBB10_23:                              # %if.end57
                                        #   in Loop: Header=BB10_6 Depth=1
	testq	%rcx, %rcx
	je	.LBB10_24
# BB#25:                                # %if.end57
                                        #   in Loop: Header=BB10_6 Depth=1
	leaq	(%rbx,%rdi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1001, %rax             # imm = 0x3E9
	jl	.LBB10_26
# BB#27:                                # %if.end57
                                        #   in Loop: Header=BB10_6 Depth=1
	leaq	(%rdi,%rdi,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jle	.LBB10_28
# BB#29:                                # %if.then67
                                        #   in Loop: Header=BB10_6 Depth=1
	movslq	align_loops_log(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	16(%rsp), %rdx          # 8-byte Reload
	jge	.LBB10_31
# BB#30:                                #   in Loop: Header=BB10_6 Depth=1
	movl	align_loops_max_skip(%rip), %edx
.LBB10_31:                              # %if.then67
                                        #   in Loop: Header=BB10_6 Depth=1
	jl	.LBB10_33
# BB#32:                                # %if.then67
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%r15, %rax
.LBB10_33:                              # %if.then67
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%rax, %r15
	jmp	.LBB10_34
.LBB10_16:                              #   in Loop: Header=BB10_6 Depth=1
	movl	align_labels_max_skip(%rip), %edx
	jmp	.LBB10_34
.LBB10_28:                              #   in Loop: Header=BB10_6 Depth=1
	movq	16(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB10_34
.LBB10_24:                              #   in Loop: Header=BB10_6 Depth=1
	movq	16(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB10_34
.LBB10_26:                              #   in Loop: Header=BB10_6 Depth=1
	movq	16(%rsp), %rdx          # 8-byte Reload
	.align	16, 0x90
.LBB10_34:                              # %if.end72
                                        #   in Loop: Header=BB10_6 Depth=1
	movl	48(%r12), %eax
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movw	%r15w, (%rcx,%rax,4)
	movw	%dx, 2(%rcx,%rax,4)
	movq	(%rsp), %rdx            # 8-byte Reload
.LBB10_35:                              # %for.inc86
                                        #   in Loop: Header=BB10_6 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB10_6
.LBB10_36:                              # %for.end87
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	compute_alignments, .Ltmp72-compute_alignments
	.cfi_endproc

	.globl	shorten_branches
	.align	16, 0x90
	.type	shorten_branches,@function
shorten_branches:                       # @shorten_branches
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp81:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp84:
	.cfi_def_cfa_offset 208
.Ltmp85:
	.cfi_offset %rbx, -48
.Ltmp86:
	.cfi_offset %r12, -40
.Ltmp87:
	.cfi_offset %r13, -32
.Ltmp88:
	.cfi_offset %r14, -24
.Ltmp89:
	.cfi_offset %r15, -16
	movq	%rdi, (%rsp)            # 8-byte Spill
	callq	get_max_uid
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$2, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	callq	xmalloc
	movl	max_labelno(%rip), %ebx
	movq	%rax, uid_shuid(%rip)
	callq	max_label_num
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	je	.LBB11_3
# BB#1:                                 # %if.then
	movl	max_labelno(%rip), %r14d
	callq	max_label_num
	movq	%rax, %rbx
	movl	min_labelno(%rip), %eax
	movl	%ebx, max_labelno(%rip)
	subq	%rax, %rbx
	incq	%rbx
	subq	%rax, %r14
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	label_align(%rip), %rdi
	movq	$32, %rcx
	movq	%rbx, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$2, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	callq	xrealloc
	cmpq	%r12, %r13
	movq	%rax, label_align(%rip)
	jl	.LBB11_123
# BB#2:                                 # %if.end
	movq	$32, %rcx
	subq	%r14, %rbx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	(%rax,%r14,4), %rdi
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$2, %rcx
	shlq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
.LBB11_3:                               # %if.end16
	callq	get_insns
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB11_33
# BB#4:
	movq	$1, %r14
	.align	16, 0x90
.LBB11_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_9 Depth 2
	movslq	8(%rbx), %rax
	movq	uid_shuid(%rip), %rcx
	movl	%r14d, (%rcx,%rax,4)
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	je	.LBB11_32
# BB#6:                                 # %if.else
                                        #   in Loop: Header=BB11_5 Depth=1
	cmpq	$36, %rcx
	jne	.LBB11_7
# BB#13:                                # %if.then29
                                        #   in Loop: Header=BB11_5 Depth=1
	movl	48(%rbx), %esi
	movl	min_labelno(%rip), %r8d
	movq	%rsi, %rdx
	subq	%r8, %rdx
	movq	$32, %rcx
	movq	label_align(%rip), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	(%rdi,%rdx,4), %rcx
	xorq	%r12, %r12
	testq	%rcx, %rcx
	movabsq	$0, %rax
	jle	.LBB11_15
# BB#14:                                # %if.then39
                                        #   in Loop: Header=BB11_5 Depth=1
	movswq	2(%rdi,%rdx,4), %r12
	movq	%rcx, %rax
.LBB11_15:                              # %if.end48
                                        #   in Loop: Header=BB11_5 Depth=1
	movslq	align_labels_log(%rip), %r13
	cmpq	%r13, %rax
	jge	.LBB11_17
# BB#16:                                #   in Loop: Header=BB11_5 Depth=1
	movl	align_labels_max_skip(%rip), %r12d
.LBB11_17:                              # %if.end48
                                        #   in Loop: Header=BB11_5 Depth=1
	jl	.LBB11_19
# BB#18:                                # %if.end48
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	%rax, %r13
.LBB11_19:                              # %if.end48
                                        #   in Loop: Header=BB11_5 Depth=1
	movl	target_flags(%rip), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	jne	.LBB11_31
# BB#20:                                # %if.end48
                                        #   in Loop: Header=BB11_5 Depth=1
	movl	flag_pic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB11_31
# BB#21:                                # %if.end48
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB11_31
# BB#22:                                # %land.lhs.true59
                                        #   in Loop: Header=BB11_5 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB11_31
# BB#23:                                # %if.then64
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	32(%rax), %rax
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$44, %rcx
	jne	.LBB11_31
# BB#24:                                # %if.then76
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movq	$16, %rdi
	cmpq	$16, %rax
	ja	.LBB11_26
# BB#25:                                # %if.then76
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	%rax, %rdi
.LBB11_26:                              # %if.then76
                                        #   in Loop: Header=BB11_5 Depth=1
	callq	exact_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB11_28
# BB#27:                                #   in Loop: Header=BB11_5 Depth=1
	movl	align_labels_max_skip(%rip), %r12d
.LBB11_28:                              # %if.then76
                                        #   in Loop: Header=BB11_5 Depth=1
	jl	.LBB11_30
# BB#29:                                # %if.then76
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	%r13, %rax
.LBB11_30:                              # %if.then76
                                        #   in Loop: Header=BB11_5 Depth=1
	movl	48(%rbx), %esi
	movl	min_labelno(%rip), %r8d
	movq	label_align(%rip), %rdi
	movq	%rax, %r13
	.align	16, 0x90
.LBB11_31:                              # %if.end84
                                        #   in Loop: Header=BB11_5 Depth=1
	subq	%r8, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movw	%r13w, (%rdi,%rsi,4)
	movw	%r12w, 2(%rdi,%rsi,4)
	jmp	.LBB11_32
	.align	16, 0x90
.LBB11_7:                               # %if.else
                                        #   in Loop: Header=BB11_5 Depth=1
	cmpq	$35, %rcx
	jne	.LBB11_32
# BB#8:                                 # %for.cond107.preheader
                                        #   in Loop: Header=BB11_5 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %rcx
	je	.LBB11_32
	jmp	.LBB11_9
	.align	16, 0x90
.LBB11_12:                              # %for.inc.land.rhs_crit_edge
                                        #   in Loop: Header=BB11_9 Depth=2
	movl	(%rcx), %eax
.LBB11_9:                               # %land.rhs
                                        #   Parent Loop BB11_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %edx
	cmpq	$105, %rdx
	je	.LBB11_32
# BB#10:                                # %land.rhs
                                        #   in Loop: Header=BB11_9 Depth=2
	cmpq	$36, %rax
	je	.LBB11_32
# BB#11:                                # %for.inc
                                        #   in Loop: Header=BB11_9 Depth=2
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB11_12
	.align	16, 0x90
.LBB11_32:                              # %for.inc133
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	24(%rbx), %rbx
	incq	%r14
	testq	%rbx, %rbx
	jne	.LBB11_5
.LBB11_33:                              # %for.end137
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	xmalloc
	movq	%rax, insn_lengths(%rip)
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	%eax, insn_lengths_max_uid(%rip)
	movabsq	$.L.str14, %rdx
	movq	$4, %rsi
	movq	%r15, %rdi
	callq	varray_init
	movq	%rax, insn_addresses_(%rip)
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	xcalloc
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$8, %rsi
	movq	%r15, %rdi
	callq	xcalloc
	movq	%rax, uid_align(%rip)
	movq	$0, 152(%rsp)
	movq	$0, 144(%rsp)
	movq	$0, 136(%rsp)
	movq	$0, 128(%rsp)
	movq	$0, 120(%rsp)
	movq	$0, 112(%rsp)
	movq	$0, 104(%rsp)
	movq	$0, 96(%rsp)
	movq	$0, 88(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 72(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	callq	get_last_insn
	jmp	.LBB11_34
	.align	16, 0x90
.LBB11_36:                              # %cond.end.thread
                                        #   in Loop: Header=BB11_34 Depth=1
	movq	32(%rsp), %rcx
	movq	uid_align(%rip), %rdx
	movq	%rcx, (%rdx,%rsi,8)
	movq	16(%rax), %rax
.LBB11_34:                              # %for.end137
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_40 Depth 2
	testq	%rax, %rax
	je	.LBB11_42
# BB#35:                                # %for.body157
                                        #   in Loop: Header=BB11_34 Depth=1
	movzbl	(%rax), %ecx
	movslq	8(%rax), %rsi
	cmpq	$36, %rcx
	jne	.LBB11_36
# BB#37:                                # %cond.end
                                        #   in Loop: Header=BB11_34 Depth=1
	movl	48(%rax), %edx
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rdx
	movq	$32, %rcx
	movq	label_align(%rip), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rdi,%rdx,4), %edi
	movq	$48, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	32(%rsp), %r8
	movq	uid_align(%rip), %rbx
	movq	$48, %rcx
	sarq	%cl, %rdx
	testq	%rdi, %rdi
	movq	%r8, (%rbx,%rsi,8)
	je	.LBB11_41
# BB#38:                                # %if.then179
                                        #   in Loop: Header=BB11_34 Depth=1
	movq	32(%rsp,%rdx,8), %rcx
	movq	uid_align(%rip), %rbx
	movq	%rcx, (%rbx,%rsi,8)
	movq	$48, %rcx
	shlq	%cl, %rdi
	movq	$48, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB11_41
# BB#39:                                # %for.body188.preheader
                                        #   in Loop: Header=BB11_34 Depth=1
	decq	%rdx
	.align	16, 0x90
.LBB11_40:                              # %for.body188
                                        #   Parent Loop BB11_34 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, 32(%rsp,%rdx,8)
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rdx
	testq	%rsi, %rsi
	jg	.LBB11_40
.LBB11_41:                              # %for.inc195
                                        #   in Loop: Header=BB11_34 Depth=1
	movq	16(%rax), %rax
	jmp	.LBB11_34
.LBB11_42:                              # %for.end199
	xorq	%rbx, %rbx
	movl	%ebx, insn_current_address(%rip)
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%rax, %rax
	je	.LBB11_121
# BB#43:                                # %for.body203.lr.ph
	movq	insn_lengths(%rip), %rdx
	movq	%rax, %r15
	jmp	.LBB11_44
.LBB11_67:                              # %if.else308
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-48(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jb	.LBB11_86
# BB#68:                                # %if.else308
                                        #   in Loop: Header=BB11_44 Depth=1
	cmpq	$24, %rax
	jne	.LBB11_85
# BB#69:                                # %for.cond315.preheader
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	8(%r12), %rax
	movslq	(%rax), %rcx
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jle	.LBB11_86
	jmp	.LBB11_70
	.align	16, 0x90
.LBB11_84:                              # %if.end357.for.body322_crit_edge
                                        #   in Loop: Header=BB11_70 Depth=2
	movl	(%r12), %esi
.LBB11_70:                              # %for.body322
                                        #   Parent Loop BB11_44 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB11_76 Depth 3
	movq	8(%rax,%r14,8), %rcx
	movslq	8(%rcx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$40, %rsi
	je	.LBB11_72
# BB#71:                                # %lor.lhs.false335
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	32(%rcx), %rdi
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rax
	js	.LBB11_82
.LBB11_72:                              # %if.then348
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	32(%rcx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$40, %rax
	jne	.LBB11_74
# BB#73:                                # %if.then.i789
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	8(%rdi), %rax
	jmp	.LBB11_75
	.align	16, 0x90
.LBB11_74:                              # %if.else.i791
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	callq	decode_asm_operands
.LBB11_75:                              # %for.cond.i794
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	$1, %rbx
	jmp	.LBB11_76
	.align	16, 0x90
.LBB11_80:                              # %for.inc.i799
                                        #   in Loop: Header=BB11_76 Depth=3
	incq	%rax
.LBB11_76:                              # %for.cond.i794
                                        #   Parent Loop BB11_44 Depth=1
                                        #     Parent Loop BB11_70 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rax), %ecx
	cmpq	$10, %rcx
	je	.LBB11_79
# BB#77:                                # %for.cond.i794
                                        #   in Loop: Header=BB11_76 Depth=3
	testq	%rcx, %rcx
	je	.LBB11_81
# BB#78:                                # %for.cond.i794
                                        #   in Loop: Header=BB11_76 Depth=3
	cmpq	$59, %rcx
	jne	.LBB11_80
.LBB11_79:                              # %if.then6.i796
                                        #   in Loop: Header=BB11_76 Depth=3
	incq	%rbx
	jmp	.LBB11_80
	.align	16, 0x90
.LBB11_81:                              # %asm_insn_count.exit800
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	insn_default_length
	imulq	%rbx, %rax
	xorq	%rbx, %rbx
	jmp	.LBB11_83
	.align	16, 0x90
.LBB11_82:                              # %if.else355
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	%rcx, %rdi
	callq	insn_default_length
.LBB11_83:                              # %if.end357
                                        #   in Loop: Header=BB11_70 Depth=2
	movq	insn_lengths(%rip), %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movl	%eax, (%rcx,%rsi,4)
	movq	24(%rsp), %rdx          # 8-byte Reload
	movb	%bl, (%rdx,%rsi)
	movl	(%rcx,%r13,4), %edx
	addq	%rax, %rdx
	movl	%edx, (%rcx,%r13,4)
	movq	8(%r12), %rax
	incq	%r14
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB11_84
	jmp	.LBB11_86
.LBB11_85:                              # %if.then400
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	%r15, %rdi
	callq	insn_default_length
	movq	insn_lengths(%rip), %rcx
	movl	%eax, (%rcx,%r13,4)
	movq	%r15, %rdi
	callq	insn_variable_length_p
	movq	24(%rsp), %rcx          # 8-byte Reload
	movb	%al, (%rcx,%r13)
	jmp	.LBB11_86
	.align	16, 0x90
.LBB11_44:                              # %for.body203
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_70 Depth 2
                                        #       Child Loop BB11_76 Depth 3
                                        #     Child Loop BB11_61 Depth 2
	movslq	8(%r15), %r13
	movl	%ebx, (%rdx,%r13,4)
	movzbl	(%r15), %eax
	cmpq	$36, %rax
	jne	.LBB11_47
# BB#45:                                # %if.then213
                                        #   in Loop: Header=BB11_44 Depth=1
	movl	48(%r15), %eax
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	label_align(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rsi,%rax,4), %eax
	testq	%rax, %rax
	je	.LBB11_47
# BB#46:                                # %if.then224
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	movl	insn_current_address(%rip), %esi
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	leaq	-1(%rdi,%rsi), %rax
	negq	%rdi
	andq	%rax, %rdi
	subq	%rsi, %rdi
	movl	%edi, (%rdx,%r13,4)
.LBB11_47:                              # %if.end233
                                        #   in Loop: Header=BB11_44 Depth=1
	movl	insn_current_address(%rip), %eax
	movq	insn_addresses_(%rip), %rcx
	movl	%eax, 32(%rcx,%r13,4)
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-35, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$3, %rcx
	jb	.LBB11_86
# BB#48:                                # %if.end233
                                        #   in Loop: Header=BB11_44 Depth=1
	andq	$134217728, %rax        # imm = 0x8000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB11_86
# BB#49:                                # %if.end257
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	32(%r15), %r12
	movl	(%r12), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$40, %rax
	je	.LBB11_57
# BB#50:                                # %if.end257
                                        #   in Loop: Header=BB11_44 Depth=1
	leaq	-44(%rax), %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$1, %rcx
	ja	.LBB11_56
# BB#51:                                # %if.then270
                                        #   in Loop: Header=BB11_44 Depth=1
	movl	target_flags(%rip), %ecx
	testq	$33554432, %rcx         # imm = 0x2000000
	jne	.LBB11_86
# BB#52:                                # %if.then270
                                        #   in Loop: Header=BB11_44 Depth=1
	movl	flag_pic(%rip), %ecx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rcx
	je	.LBB11_86
# BB#53:                                # %if.then275
                                        #   in Loop: Header=BB11_44 Depth=1
	cmpq	$45, %rax
	movabsq	$0, %rax
	jne	.LBB11_55
# BB#54:                                # %if.then275
                                        #   in Loop: Header=BB11_44 Depth=1
	movabsq	$1, %rax
.LBB11_55:                              # %if.then275
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	8(%r12,%rax,8), %rax
	movl	(%rax), %eax
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	imulq	%rax, %rcx
	movl	%ecx, (%rdx,%r13,4)
	jmp	.LBB11_86
.LBB11_56:                              # %lor.lhs.false298
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	%r12, %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movl	(%r12), %esi
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB11_67
.LBB11_57:                              # %if.then302
                                        #   in Loop: Header=BB11_44 Depth=1
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$40, %rsi
	jne	.LBB11_59
# BB#58:                                # %if.then.i
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	8(%r12), %rax
	jmp	.LBB11_60
.LBB11_59:                              # %if.else.i
                                        #   in Loop: Header=BB11_44 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r12, %rdi
	callq	decode_asm_operands
.LBB11_60:                              # %for.cond.i
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	$1, %rbx
	jmp	.LBB11_61
	.align	16, 0x90
.LBB11_65:                              # %for.inc.i
                                        #   in Loop: Header=BB11_61 Depth=2
	incq	%rax
.LBB11_61:                              # %for.cond.i
                                        #   Parent Loop BB11_44 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$10, %rcx
	je	.LBB11_64
# BB#62:                                # %for.cond.i
                                        #   in Loop: Header=BB11_61 Depth=2
	testq	%rcx, %rcx
	je	.LBB11_66
# BB#63:                                # %for.cond.i
                                        #   in Loop: Header=BB11_61 Depth=2
	cmpq	$59, %rcx
	jne	.LBB11_65
.LBB11_64:                              # %if.then6.i
                                        #   in Loop: Header=BB11_61 Depth=2
	incq	%rbx
	jmp	.LBB11_65
	.align	16, 0x90
.LBB11_66:                              # %asm_insn_count.exit
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	%r15, %rdi
	callq	insn_default_length
	movq	insn_lengths(%rip), %rcx
	imulq	%rbx, %rax
	movl	%eax, (%rcx,%r13,4)
	xorq	%rbx, %rbx
.LBB11_86:                              # %for.inc412
                                        #   in Loop: Header=BB11_44 Depth=1
	movq	insn_lengths(%rip), %rdx
	movl	(%rdx,%r13,4), %eax
	movl	insn_current_address(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, insn_current_address(%rip)
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB11_44
	.align	16, 0x90
.LBB11_87:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_88 Depth 2
                                        #       Child Loop BB11_107 Depth 3
                                        #       Child Loop BB11_100 Depth 3
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%rax, %rax
	movq	$15, %rcx
	movl	%ecx, insn_current_align(%rip)
	movabsq	$0, %rcx
	movl	%ecx, insn_current_address(%rip)
	movq	%rax, %r15
	movabsq	$0, %r14
	jne	.LBB11_88
	jmp	.LBB11_122
.LBB11_105:                             #   in Loop: Header=BB11_88 Depth=2
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB11_116
	.align	16, 0x90
.LBB11_88:                              # %for.body424
                                        #   Parent Loop BB11_87 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB11_107 Depth 3
                                        #       Child Loop BB11_100 Depth 3
	movzbl	(%r15), %eax
	movslq	8(%r15), %r12
	cmpq	$36, %rax
	jne	.LBB11_93
# BB#89:                                # %if.then432
                                        #   in Loop: Header=BB11_88 Depth=2
	movl	48(%r15), %eax
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	label_align(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	movswq	(%rdx,%rax,4), %rcx
	movslq	insn_current_align(%rip), %rax
	cmpq	%rax, %rcx
	jle	.LBB11_91
# BB#90:                                # %if.then444
                                        #   in Loop: Header=BB11_88 Depth=2
	movl	insn_current_address(%rip), %edx
	movq	$1, %rax
	shlq	%cl, %rax
	leaq	-1(%rax,%rdx), %rsi
	negq	%rax
	andq	%rsi, %rax
	movq	%rax, %rsi
	movq	insn_lengths(%rip), %rdi
	subq	%rdx, %rsi
	movl	%esi, (%rdi,%r12,4)
	movl	%ecx, insn_current_align(%rip)
	movl	%eax, insn_current_address(%rip)
	jmp	.LBB11_92
	.align	16, 0x90
.LBB11_93:                              # %if.end463
                                        #   in Loop: Header=BB11_88 Depth=2
	movslq	length_unit_log(%rip), %rax
	movslq	insn_current_align(%rip), %rcx
	cmpq	%rcx, %rax
	jge	.LBB11_95
# BB#94:                                # %if.then466
                                        #   in Loop: Header=BB11_88 Depth=2
	movl	%eax, insn_current_align(%rip)
.LBB11_95:                              # %if.end467
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	insn_addresses_(%rip), %r9
	movl	32(%r9,%r12,4), %ecx
	movl	insn_current_address(%rip), %edx
	movl	%ecx, insn_last_address(%rip)
	movl	%edx, 32(%r9,%r12,4)
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax,%r12), %edx
	movzbl	(%r15), %ecx
	testq	%rdx, %rdx
	jne	.LBB11_102
# BB#96:                                # %if.then479
                                        #   in Loop: Header=BB11_88 Depth=2
	cmpq	$32, %rcx
	jne	.LBB11_101
# BB#97:                                # %land.lhs.true484
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	32(%r15), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB11_101
# BB#98:                                # %for.cond497.preheader
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	8(%rcx), %rdx
	movslq	(%rdx), %rcx
	testq	%rcx, %rcx
	jle	.LBB11_118
# BB#99:                                # %for.body504.lr.ph
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	insn_lengths(%rip), %r8
	movl	insn_current_address(%rip), %ebx
	movq	$1, %rax
	.align	16, 0x90
.LBB11_100:                             # %for.body504
                                        #   Parent Loop BB11_87 Depth=1
                                        #     Parent Loop BB11_88 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx,%rax,8), %rcx
	movslq	8(%rcx), %rcx
	movl	%ebx, 32(%r9,%rcx,4)
	movl	(%r8,%rcx,4), %ecx
	movl	insn_current_address(%rip), %ebx
	addq	%rcx, %rbx
	movl	%ebx, insn_current_address(%rip)
	movslq	(%rdx), %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rsi, %rdi
	jl	.LBB11_100
	jmp	.LBB11_118
	.align	16, 0x90
.LBB11_91:                              # %if.else455
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	insn_lengths(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax,%r12,4)
	movl	insn_current_address(%rip), %eax
.LBB11_92:                              # %if.end458
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	insn_addresses_(%rip), %rcx
	movl	%eax, 32(%rcx,%r12,4)
	jmp	.LBB11_118
	.align	16, 0x90
.LBB11_102:                             # %if.end531
                                        #   in Loop: Header=BB11_88 Depth=2
	cmpq	$32, %rcx
	jne	.LBB11_115
# BB#103:                               # %land.lhs.true536
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	32(%r15), %r8
	movzbl	(%r8), %eax
	cmpq	$24, %rax
	jne	.LBB11_115
# BB#104:                               # %for.cond549.preheader
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	8(%r8), %rsi
	movslq	(%rsi), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB11_105
# BB#106:                               # %for.body556.lr.ph
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movl	insn_current_address(%rip), %edx
	xorq	%rbx, %rbx
	movq	$1, %r12
	jmp	.LBB11_107
	.align	16, 0x90
.LBB11_114:                             # %if.end589.for.body556_crit_edge
                                        #   in Loop: Header=BB11_107 Depth=3
	movq	insn_addresses_(%rip), %r9
	incq	%r12
.LBB11_107:                             # %for.body556
                                        #   Parent Loop BB11_87 Depth=1
                                        #     Parent Loop BB11_88 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rsi,%r12,8), %rdi
	movslq	8(%rdi), %r14
	movl	%edx, 32(%r9,%r14,4)
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax,%r14), %eax
	testq	%rax, %rax
	jne	.LBB11_109
# BB#108:                               # %if.end581.thread
                                        #   in Loop: Header=BB11_107 Depth=3
	movq	insn_lengths(%rip), %rax
	movl	(%rax,%r14,4), %eax
	movq	%rax, %rcx
	jmp	.LBB11_112
	.align	16, 0x90
.LBB11_109:                             # %if.end581
                                        #   in Loop: Header=BB11_107 Depth=3
	movq	%r8, %r13
	callq	insn_current_length
	movq	insn_lengths(%rip), %rdx
	movl	(%rdx,%r14,4), %ecx
	movq	%rax, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	%rcx, %rsi
	je	.LBB11_111
# BB#110:                               # %if.then586
                                        #   in Loop: Header=BB11_107 Depth=3
	movl	%eax, (%rdx,%r14,4)
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rcx
.LBB11_111:                             # %if.end589
                                        #   in Loop: Header=BB11_107 Depth=3
	movq	%r13, %r8
.LBB11_112:                             # %if.end589
                                        #   in Loop: Header=BB11_107 Depth=3
	movl	insn_current_address(%rip), %edx
	addq	%rcx, %rdx
	movl	%edx, insn_current_address(%rip)
	movq	8(%r8), %rsi
	addq	%rax, %rbx
	movslq	(%rsi), %rax
	movq	$32, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jl	.LBB11_114
# BB#113:                               #   in Loop: Header=BB11_88 Depth=2
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB11_116
.LBB11_101:                             # %if.else526
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	insn_lengths(%rip), %rax
	movl	(%rax,%r12,4), %eax
	movl	insn_current_address(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, insn_current_address(%rip)
	jmp	.LBB11_118
.LBB11_115:                             # %if.else597
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	%r15, %rdi
	callq	insn_current_length
	movl	insn_current_address(%rip), %ecx
	movq	%rax, %rbx
	addq	%rbx, %rcx
	movl	%ecx, insn_current_address(%rip)
.LBB11_116:                             # %if.end600
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	insn_lengths(%rip), %rax
	movl	(%rax,%r12,4), %ecx
	movq	%rbx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rcx, %rdx
	je	.LBB11_118
# BB#117:                               # %if.then605
                                        #   in Loop: Header=BB11_88 Depth=2
	movl	%ebx, (%rax,%r12,4)
	movq	$1, %r14
	.align	16, 0x90
.LBB11_118:                             # %for.inc609
                                        #   in Loop: Header=BB11_88 Depth=2
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB11_88
# BB#119:                               # %for.end613
                                        #   in Loop: Header=BB11_87 Depth=1
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_122
# BB#120:                               # %for.end613
                                        #   in Loop: Header=BB11_87 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB11_87
	jmp	.LBB11_122
.LBB11_121:                             # %while.end.critedge
	movq	$15, %rax
	movl	%eax, insn_current_align(%rip)
	movl	%ebx, insn_current_address(%rip)
.LBB11_122:                             # %while.end
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	free
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_123:                             # %if.then12
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.shorten_branches, %rcx
	movq	$928, %rsi              # imm = 0x3A0
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp90:
	.size	shorten_branches, .Ltmp90-shorten_branches
	.cfi_endproc

	.globl	final_start_function
	.align	16, 0x90
	.type	final_start_function,@function
final_start_function:                   # @final_start_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp96:
	.cfi_def_cfa_offset 32
.Ltmp97:
	.cfi_offset %rbx, -24
.Ltmp98:
	.cfi_offset %r14, -16
	movq	%rdi, %rax
	xorq	%rcx, %rcx
	movl	%ecx, block_depth(%rip)
	movq	$0, this_is_asm_operands(%rip)
	movl	40(%rax), %edi
	movq	%rsi, %r14
	movabsq	$4294967197, %rcx       # imm = 0xFFFFFF9D
	cmpq	%rcx, %rdi
	jne	.LBB12_2
# BB#1:                                 # %entry.if.end_crit_edge
	movl	last_linenum(%rip), %edi
	movq	last_filename(%rip), %rsi
	jmp	.LBB12_7
.LBB12_2:                               # %if.then
	movq	32(%rax), %rsi
	movq	%rsi, last_filename(%rip)
	movl	%edi, last_linenum(%rip)
	movq	$32, %rcx
	movq	%rdi, %rax
	movslq	high_block_linenum(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	movq	%rdi, %rcx
	jge	.LBB12_4
# BB#3:                                 # %if.then
	movq	%rdx, %rcx
.LBB12_4:                               # %if.then
	movslq	high_function_linenum(%rip), %rdx
	movl	%ecx, high_block_linenum(%rip)
	cmpq	%rdx, %rax
	movq	%rdi, %rax
	jge	.LBB12_6
# BB#5:                                 # %if.then
	movq	%rdx, %rax
.LBB12_6:                               # %if.then
	movl	%eax, high_function_linenum(%rip)
.LBB12_7:                               # %if.end
	movq	debug_hooks(%rip), %rax
	movl	%edi, high_function_linenum(%rip)
	movl	%edi, high_block_linenum(%rip)
	callq	*80(%rax)
	movl	write_symbols(%rip), %eax
	cmpq	$4, %rax
	je	.LBB12_10
# BB#8:                                 # %if.end
	cmpq	$7, %rax
	je	.LBB12_10
# BB#9:                                 # %if.then3
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	dwarf2out_begin_prologue
.LBB12_10:                              # %if.end4
	callq	dwarf2out_do_frame
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_12
# BB#11:                                # %if.then5
	xorq	%rdi, %rdi
	callq	dwarf2out_frame_debug
.LBB12_12:                              # %if.end6
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_14
# BB#13:                                # %if.then8
	callq	remove_unnecessary_notes
	callq	reorder_blocks
	movq	current_function_decl(%rip), %rdi
	callq	number_blocks
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %rax
	movl	16(%rax), %ecx
	orq	$16384, %rcx            # imm = 0x4000
	movl	%ecx, 16(%rax)
.LBB12_14:                              # %if.end9
	movq	targetm+96(%rip), %rbx
	callq	get_frame_size
	movq	%r14, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmpq	*%rbx  # TAILCALL
.Ltmp99:
	.size	final_start_function, .Ltmp99-final_start_function
	.cfi_endproc

	.globl	final_end_function
	.align	16, 0x90
	.type	final_end_function,@function
final_end_function:                     # @final_end_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp103:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp104:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp105:
	.cfi_def_cfa_offset 32
.Ltmp106:
	.cfi_offset %rbx, -24
.Ltmp107:
	.cfi_offset %r14, -16
	movzbl	app_on(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB13_2
# BB#1:                                 # %if.then.i
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str13, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB13_2:                               # %app_disable.exit
	movq	debug_hooks(%rip), %rax
	movl	high_function_linenum(%rip), %edi
	callq	*112(%rax)
	movq	targetm+120(%rip), %r14
	movq	asm_out_file(%rip), %rbx
	callq	get_frame_size
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	*%r14
	movq	debug_hooks(%rip), %rax
	callq	*96(%rax)
	movl	write_symbols(%rip), %eax
	cmpq	$4, %rax
	je	.LBB13_5
# BB#3:                                 # %app_disable.exit
	cmpq	$7, %rax
	je	.LBB13_5
# BB#4:                                 # %land.lhs.true2
	callq	dwarf2out_do_frame
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_5
# BB#6:                                 # %if.then
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	dwarf2out_end_epilogue  # TAILCALL
.LBB13_5:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp108:
	.size	final_end_function, .Ltmp108-final_end_function
	.cfi_endproc

	.globl	final
	.align	16, 0x90
	.type	final,@function
final:                                  # @final
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp114:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp115:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp116:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp117:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp118:
	.cfi_def_cfa_offset 48
.Ltmp119:
	.cfi_offset %rbx, -40
.Ltmp120:
	.cfi_offset %r12, -32
.Ltmp121:
	.cfi_offset %r14, -24
.Ltmp122:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB14_13
# BB#1:
	xorq	%rdi, %rdi
	movq	%r15, %rax
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB14_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movslq	40(%rax), %rdx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB14_5
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdi, %rdx
.LBB14_5:                               # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rdi
.LBB14_6:                               # %for.inc
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB14_2
# BB#7:                                 # %for.end
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	movq	$1, %rbx
	callq	xcalloc
	testq	%r15, %r15
	movq	%rax, line_note_exists(%rip)
	je	.LBB14_14
# BB#8:
	movq	%r15, %rax
	.align	16, 0x90
.LBB14_9:                               # %for.body9
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB14_12
# BB#10:                                # %land.lhs.true24
                                        #   in Loop: Header=BB14_9 Depth=1
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB14_12
# BB#11:                                # %if.then30
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	line_note_exists(%rip), %rdx
	movb	%bl, (%rdx,%rcx)
.LBB14_12:                              # %for.inc36
                                        #   in Loop: Header=BB14_9 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB14_9
	jmp	.LBB14_14
.LBB14_13:                              # %for.end40.critedge
	movq	$1, %rdi
	movq	$1, %rsi
	callq	xcalloc
	movq	%rax, line_note_exists(%rip)
.LBB14_14:                              # %for.end40
	callq	init_recog
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB14_19
# BB#15:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_16:                              # %for.body46
                                        # =>This Inner Loop Header: Depth=1
	movq	insn_addresses_(%rip), %rdx
	movl	8(%rax), %edi
	movq	$32, %rcx
	movq	%rdi, %rsi
	movq	(%rdx), %rbx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rdi
	movq	%r15, %rcx
	jae	.LBB14_18
# BB#17:                                # %if.else
                                        #   in Loop: Header=BB14_16 Depth=1
	movl	32(%rdx,%rsi,4), %ecx
.LBB14_18:                              # %if.end59
                                        #   in Loop: Header=BB14_16 Depth=1
	movl	%ecx, insn_current_address(%rip)
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	final_scan_insn
	testq	%rax, %rax
	jne	.LBB14_16
.LBB14_19:                              # %for.end61
	movq	line_note_exists(%rip), %rdi
	callq	free
	movq	$0, line_note_exists(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp123:
	.size	final, .Ltmp123-final
	.cfi_endproc

	.globl	final_scan_insn
	.align	16, 0x90
	.type	final_scan_insn,@function
final_scan_insn:                        # @final_scan_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp127:
	.cfi_def_cfa_offset 16
.Ltmp128:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp129:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp130:
	.cfi_offset %rbx, -56
.Ltmp131:
	.cfi_offset %r12, -48
.Ltmp132:
	.cfi_offset %r13, -40
.Ltmp133:
	.cfi_offset %r14, -32
.Ltmp134:
	.cfi_offset %r15, -24
	movl	insn_counter(%rip), %eax
	movq	%rdi, %r15
	incq	%rax
	movl	%eax, insn_counter(%rip)
	movl	(%r15), %eax
	movq	%rcx, %r13
	movq	%rsi, %r12
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB15_173
# BB#1:                                 # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$35, %rax
	je	.LBB15_69
# BB#2:                                 # %if.end
	cmpq	$36, %rax
	jne	.LBB15_3
# BB#71:                                # %sw.bb166
	movslq	48(%r15), %rax
	movslq	max_labelno(%rip), %rcx
	cmpq	%rcx, %rax
	jg	.LBB15_77
# BB#72:                                # %if.then171
	movl	min_labelno(%rip), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	movq	label_align(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rsi,%rax,4), %edi
	movq	$48, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$48, %rcx
	sarq	%cl, %rdx
	testq	%rdi, %rdi
	je	.LBB15_77
# BB#73:                                # %land.lhs.true186
	cmpq	$0, 24(%r15)
	je	.LBB15_77
# BB#74:                                # %if.then195
	movswq	2(%rsi,%rax,4), %rcx
	testq	$65535, %rcx            # imm = 0xFFFF
	je	.LBB15_75
# BB#76:                                # %if.else200
	movabsq	$.L.str21, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	jmp	.LBB15_77
.LBB15_69:                              # %sw.bb161
	callq	dwarf2out_do_frame
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_173
# BB#70:                                # %if.then164
	movq	%r15, %rdi
	callq	dwarf2out_frame_debug
	jmp	.LBB15_173
.LBB15_3:                               # %if.end
	cmpq	$37, %rax
	jne	.LBB15_96
# BB#4:                                 # %sw.bb
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	jg	.LBB15_173
# BB#5:                                 # %if.end4
	movslq	40(%r15), %rdi
	testq	%rdi, %rdi
	jns	.LBB15_52
# BB#6:                                 # %if.end4
	leaq	99(%rdi), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$20, %rax
	ja	.LBB15_53
# BB#7:                                 # %if.end4
	jmpq	*.LJTI15_0(,%rax,8)
.LBB15_33:                              # %sw.bb29
	movl	debug_info_level(%rip), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	cmpq	$2, %rcx
	je	.LBB15_38
# BB#34:                                # %sw.bb29
	movl	write_symbols(%rip), %eax
	andq	%rbx, %rax
	cmpq	$3, %rax
	je	.LBB15_38
# BB#35:                                # %sw.bb29
	cmpq	$4, %rax
	je	.LBB15_38
# BB#36:                                # %sw.bb29
	cmpq	$7, %rax
	je	.LBB15_38
# BB#37:                                # %sw.bb29
	cmpq	$6, %rax
	jne	.LBB15_173
.LBB15_38:                              # %if.then40
	movq	32(%r15), %rax
	movl	24(%rax), %ebx
	movzbl	app_on(%rip), %eax
	movq	$2, %rcx
	shrq	%cl, %rbx
	cmpq	$1, %rax
	jne	.LBB15_40
# BB#39:                                # %if.then.i762
	movq	asm_out_file(%rip), %rax
	movabsq	$.L.str13, %rcx
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB15_40:                              # %app_disable.exit763
	movl	block_depth(%rip), %eax
	movl	last_linenum(%rip), %ecx
	movq	debug_hooks(%rip), %rdx
	incq	%rax
	movl	%eax, block_depth(%rip)
	movl	%ecx, high_block_linenum(%rip)
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	callq	*48(%rdx)
	movq	32(%r15), %rax
	movl	16(%rax), %ecx
	orq	$16384, %rcx            # imm = 0x4000
	movl	%ecx, 16(%rax)
	jmp	.LBB15_173
.LBB15_96:                              # %sw.default295
	movq	32(%r15), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$40, %rcx
	je	.LBB15_113
# BB#97:                                # %sw.default295
	leaq	-48(%rcx), %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	cmpq	$2, %rdx
	jb	.LBB15_173
# BB#98:                                # %sw.default295
	addq	$-44, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	ja	.LBB15_118
# BB#99:                                # %if.then320
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	jg	.LBB15_173
# BB#100:                               # %if.end324
	movzbl	app_on(%rip), %ecx
	cmpq	$1, %rcx
	jne	.LBB15_102
# BB#101:                               # %if.then326
	movq	%rsi, %rbx
	movabsq	$.L.str13, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	movq	%rbx, %rsi
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
	movl	(%rsi), %eax
.LBB15_102:                             # %if.end328
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$45, %rcx
	jne	.LBB15_103
# BB#104:                               # %if.end328
	movabsq	$1, %rcx
	jmp	.LBB15_105
.LBB15_113:                             # %if.then387
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	jg	.LBB15_173
# BB#114:                               # %if.end394
	movq	8(%rsi), %rbx
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB15_173
# BB#115:                               # %if.then397
	movzbl	app_on(%rip), %eax
	testq	%rax, %rax
	jne	.LBB15_117
# BB#116:                               # %if.then399
	movabsq	$.L.str12, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	$1, %r14
	movq	%r12, %rcx
	callq	fwrite
	movb	%r14b, app_on(%rip)
.LBB15_117:                             # %if.end401
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB15_173
.LBB15_52:                              # %if.end4
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB15_173
.LBB15_53:                              # %sw.default
	testq	%rdi, %rdi
	jle	.LBB15_175
# BB#54:                                # %for.cond.preheader
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB15_64
# BB#55:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB15_56:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rsi
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$36, %rsi
	jne	.LBB15_64
# BB#57:                                # %if.else
                                        #   in Loop: Header=BB15_56 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB15_176
# BB#58:                                # %land.lhs.true104
                                        #   in Loop: Header=BB15_56 Depth=1
	movl	40(%rax), %ecx
	addq	$98, %rcx
	andq	%rdx, %rcx
	cmpq	$11, %rcx
	jbe	.LBB15_59
.LBB15_60:                              # %land.lhs.true124
                                        #   in Loop: Header=BB15_56 Depth=1
	movslq	40(%rax), %rsi
	testq	%rsi, %rsi
	jg	.LBB15_61
.LBB15_176:                             # %for.cond.backedge
                                        #   in Loop: Header=BB15_56 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB15_56
	jmp	.LBB15_64
.LBB15_59:                              # %land.lhs.true104
                                        #   in Loop: Header=BB15_56 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$2051, %rsi             # imm = 0x803
	je	.LBB15_60
	jmp	.LBB15_64
.LBB15_75:                              # %if.then198
	movabsq	$.L.str20, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
.LBB15_77:                              # %if.end206
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	jg	.LBB15_173
# BB#78:                                # %if.end210
	cmpq	$0, 56(%r15)
	je	.LBB15_80
# BB#79:                                # %if.then214
	movq	debug_hooks(%rip), %rax
	movq	%r15, %rdi
	callq	*152(%rax)
.LBB15_80:                              # %if.end215
	movzbl	app_on(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB15_82
# BB#81:                                # %if.then217
	movabsq	$.L.str13, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB15_82:                              # %if.end219
	movq	24(%r15), %rax
	testq	%rax, %rax
	je	.LBB15_94
# BB#83:                                # %land.lhs.true225
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB15_94
# BB#84:                                # %if.then233
	movq	32(%rax), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB15_94
# BB#85:                                # %if.then249
	movl	target_flags(%rip), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	jne	.LBB15_87
# BB#86:                                # %if.then249
	movl	flag_pic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_87
# BB#91:                                # %if.else263
	movq	current_function_decl(%rip), %rdi
	callq	function_section
	jmp	.LBB15_92
.LBB15_94:                              # %if.end275
	movq	64(%r15), %rsi
	testq	%rsi, %rsi
	je	.LBB15_93
# BB#95:                                # %do.body281
	movq	%r12, %rdi
	callq	assemble_name
	movabsq	$.L.str22, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	jmp	.LBB15_173
.LBB15_118:                             # %if.end404
	movq	%rsi, %r14
	movq	%r14, %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_125
# BB#119:                               # %if.then408
	movq	%r14, %rdi
	callq	asm_noperands
	andq	%rax, %rbx
	leaq	15(,%rbx,8), %rcx
	andq	$-16, %rcx
	movq	%rsp, %rbx
	subq	%rcx, %rbx
	movq	%rbx, %rsp
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	movq	%r14, %rdi
	jg	.LBB15_173
# BB#120:                               # %if.end415
	movq	%rax, %r13
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	decode_asm_operands
	movq	%rax, %r14
	movl	%r13d, insn_noperands(%rip)
	movq	%r15, this_is_asm_operands(%rip)
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB15_124
# BB#121:                               # %if.then419
	movzbl	app_on(%rip), %eax
	testq	%rax, %rax
	jne	.LBB15_123
# BB#122:                               # %if.then421
	movabsq	$.L.str12, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	movq	$1, %r13
	movq	%r12, %rcx
	callq	fwrite
	movb	%r13b, app_on(%rip)
.LBB15_123:                             # %if.end423
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	output_asm_insn
.LBB15_124:                             # %if.end424
	movq	$0, this_is_asm_operands(%rip)
	jmp	.LBB15_173
.LBB15_87:                              # %if.then253
	callq	readonly_data_section
	movq	24(%r15), %rax
	movq	32(%rax), %rax
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %eax
	cmpq	$16, %rax
	movq	$16, %rdi
	ja	.LBB15_89
# BB#88:                                # %if.then253
	movq	%rax, %rdi
.LBB15_89:                              # %if.then253
	callq	exact_log2_wide
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_92
# BB#90:                                # %if.then260
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
.LBB15_92:                              # %do.body265
	movabsq	$.L.str1, %rsi
	movq	$4, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
.LBB15_93:                              # %do.body78
	movl	48(%r15), %ecx
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str19, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	jmp	.LBB15_173
.LBB15_103:
	movabsq	$0, %rcx
.LBB15_105:                             # %if.end328
	movq	8(%rsi,%rcx,8), %rcx
	movslq	(%rcx), %r13
	testq	%r13, %r13
	jle	.LBB15_112
# BB#106:                               # %for.body339.lr.ph
	movq	$1, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r13
	jmp	.LBB15_107
.LBB15_111:                             # %for.inc379.for.body339_crit_edge
                                        #   in Loop: Header=BB15_107 Depth=1
	movl	(%rsi), %eax
	incq	%rbx
.LBB15_107:                             # %for.body339
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$44, %rax
	jne	.LBB15_109
# BB#108:                               # %if.then344
                                        #   in Loop: Header=BB15_107 Depth=1
	movq	8(%rsi), %rax
	movq	%rsi, %r14
	movq	(%rax,%rbx,8), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %esi
	movq	%r12, %rdi
	callq	ix86_output_addr_vec_elt
	jmp	.LBB15_110
.LBB15_109:                             # %if.else356
                                        #   in Loop: Header=BB15_107 Depth=1
	movq	16(%rsi), %rax
	movq	8(%rsi), %rcx
	movq	%rsi, %r14
	movq	(%rax,%rbx,8), %rax
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	movl	48(%rax), %esi
	movl	48(%rcx), %edx
	movq	%r12, %rdi
	callq	ix86_output_addr_diff_elt
.LBB15_110:                             # %for.inc379
                                        #   in Loop: Header=BB15_107 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r13, %rax
	movq	%r14, %rsi
	jne	.LBB15_111
.LBB15_112:                             # %for.end381
	movq	current_function_decl(%rip), %rdi
	callq	function_section
	jmp	.LBB15_173
.LBB15_125:                             # %if.end425
	movq	%r14, %rbx
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB15_128
# BB#126:                               # %if.end425
	movzbl	app_on(%rip), %eax
	testq	$1, %rax
	je	.LBB15_128
# BB#127:                               # %if.then430
	movabsq	$.L.str13, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB15_128:                             # %if.end432
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB15_138
# BB#129:                               # %if.then437
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB15_173
# BB#130:                               # %if.end441
	movq	%rbx, final_sequence(%rip)
	movq	8(%rbx), %rax
	movq	8(%rax), %rdi
	movq	%r12, %rsi
	movq	%r13, %rcx
	callq	final_scan_insn
	movq	%rbx, %rsi
	movq	8(%rsi), %rcx
	movq	16(%rcx), %rdx
	movq	%rax, %rbx
	cmpq	%rdx, %rbx
	je	.LBB15_131
# BB#177:                               # %if.then455
	movq	$0, final_sequence(%rip)
	jmp	.LBB15_174
.LBB15_138:                             # %if.end495
	movslq	40(%r15), %r14
	movq	32(%r15), %rbx
	testq	%r14, %r14
	jns	.LBB15_140
# BB#139:                               # %cond.false
	movq	%r15, %rdi
	callq	recog_memoized_1
	movq	%rax, %r14
.LBB15_140:                             # %cond.end
	movq	%r15, %rdi
	callq	cleanup_subreg_operands
	movl	flag_dump_rtl_in_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_142
# BB#141:                               # %if.then509
	movq	asm_out_file(%rip), %rdi
	movq	$.L.str16, print_rtx_head(%rip)
	movq	%r15, %rsi
	callq	print_rtl_single
	movq	$.L.str24, print_rtx_head(%rip)
.LBB15_142:                             # %if.end511
	movq	$1, %rdi
	callq	constrain_operands_cached
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_178
# BB#143:                               # %if.end515
	movq	%r15, debug_insn(%rip)
	movq	%r15, current_output_insn(%rip)
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB15_146
# BB#144:                               # %land.lhs.true520
	callq	dwarf2out_do_frame
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_146
# BB#145:                               # %if.then523
	movq	%r15, %rdi
	callq	dwarf2out_frame_debug
.LBB15_146:                             # %if.end524
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	(%r14,%r14,4), %rcx
	movsbq	insn_data+35(,%rcx,8), %rax
	movq	insn_data+8(,%rcx,8), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB15_154
# BB#147:                               # %if.end524
	cmpq	$3, %rax
	jne	.LBB15_148
# BB#150:                               # %sw.bb7.i
	testq	%r15, %r15
	je	.LBB15_151
# BB#153:                               # %if.end.i
	movabsq	$recog_data, %rdi
	movq	%r15, %rsi
	callq	*%rdx
	movq	%rax, %rdx
	jmp	.LBB15_154
.LBB15_61:                              # %if.then129
	movq	line_note_exists(%rip), %rax
	leaq	1(%rdi), %rbx
.LBB15_62:                              # %for.cond133
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jge	.LBB15_173
# BB#63:                                # %for.body138
                                        #   in Loop: Header=BB15_62 Depth=1
	movzbl	(%rax,%rbx), %ecx
	incq	%rbx
	testq	%rcx, %rcx
	je	.LBB15_62
.LBB15_64:                              # %if.then159
	movq	32(%r15), %rsi
	movslq	high_block_linenum(%rip), %rcx
	movq	%rsi, last_filename(%rip)
	movl	%edi, last_linenum(%rip)
	cmpq	%rcx, %rdi
	movq	%rdi, %rax
	jge	.LBB15_66
# BB#65:                                # %if.then159
	movq	%rcx, %rax
.LBB15_66:                              # %if.then159
	movslq	high_function_linenum(%rip), %rcx
	movl	%eax, high_block_linenum(%rip)
	cmpq	%rcx, %rdi
	movq	%rdi, %rax
	jge	.LBB15_68
# BB#67:                                # %if.then159
	movq	%rcx, %rax
.LBB15_68:                              # %if.then159
	movq	debug_hooks(%rip), %rcx
	movl	%eax, high_function_linenum(%rip)
	callq	*72(%rcx)
.LBB15_173:                             # %sw.epilog604
	movq	24(%r15), %rbx
.LBB15_174:                             # %return
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB15_148:                             # %if.end524
	cmpq	$2, %rax
	jne	.LBB15_152
# BB#149:                               # %sw.bb4.i
	movslq	which_alternative(%rip), %rax
	movq	(%rdx,%rax,8), %rdx
.LBB15_154:                             # %get_insn_template.exit
	testq	%rdx, %rdx
	je	.LBB15_155
# BB#161:                               # %if.end550
	movzbl	(%rdx), %eax
	cmpq	$35, %rax
	jne	.LBB15_166
# BB#162:                               # %land.lhs.true555
	movzbl	1(%rdx), %eax
	testq	%rax, %rax
	je	.LBB15_163
.LBB15_166:                             # %if.end572
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jg	.LBB15_173
# BB#167:                               # %if.end576
	movabsq	$recog_data, %rsi
	movq	%rdx, %rdi
	callq	output_asm_insn
	movzbl	(%r15), %eax
	cmpq	$32, %rax
	jne	.LBB15_170
# BB#168:                               # %land.lhs.true581
	callq	dwarf2out_do_frame
	testq	%rbx, %rax
	je	.LBB15_170
# BB#169:                               # %if.then584
	movq	%r15, %rdi
	callq	dwarf2out_frame_debug
.LBB15_170:                             # %if.end585
	movq	$30, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB15_172
# BB#171:                               # %if.then588
	movq	8(%rax), %rax
	movq	16(%rax), %rcx
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	assemble_vtable_entry
.LBB15_172:                             # %if.end603
	movq	$0, debug_insn(%rip)
	movq	$0, current_output_insn(%rip)
	jmp	.LBB15_173
.LBB15_155:                             # %if.then528
	movq	%r15, %rdi
	callq	prev_nonnote_insn
	testq	%rax, %rax
	jne	.LBB15_179
# BB#156:                               # %for.cond534.preheader
	xorq	%rbx, %rbx
	jmp	.LBB15_157
.LBB15_160:                             # %for.inc545
                                        #   in Loop: Header=BB15_157 Depth=1
	movq	16(%r15), %r15
.LBB15_157:                             # %for.cond534.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB15_174
# BB#158:                               # %for.body537
                                        #   in Loop: Header=BB15_157 Depth=1
	movzbl	(%r15), %eax
	cmpq	$37, %rax
	jne	.LBB15_160
# BB#159:                               # %if.then542
                                        #   in Loop: Header=BB15_157 Depth=1
	movq	%r15, %rdi
	callq	delete_insn
	jmp	.LBB15_160
.LBB15_41:                              # %sw.bb52
	movl	debug_info_level(%rip), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	cmpq	$2, %rcx
	je	.LBB15_46
# BB#42:                                # %sw.bb52
	movl	write_symbols(%rip), %eax
	andq	%rbx, %rax
	cmpq	$3, %rax
	je	.LBB15_46
# BB#43:                                # %sw.bb52
	cmpq	$4, %rax
	je	.LBB15_46
# BB#44:                                # %sw.bb52
	cmpq	$7, %rax
	je	.LBB15_46
# BB#45:                                # %sw.bb52
	cmpq	$6, %rax
	jne	.LBB15_173
.LBB15_46:                              # %if.then64
	movq	32(%r15), %rax
	movzbl	app_on(%rip), %ecx
	movl	24(%rax), %ebx
	cmpq	$1, %rcx
	jne	.LBB15_48
# BB#47:                                # %if.then.i765
	movq	asm_out_file(%rip), %rax
	movabsq	$.L.str13, %rcx
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB15_48:                              # %app_disable.exit766
	movslq	block_depth(%rip), %rax
	testq	%rax, %rax
	leaq	-1(%rax), %rax
	movl	%eax, block_depth(%rip)
	jle	.LBB15_49
# BB#51:                                # %if.end75
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	movq	debug_hooks(%rip), %rdx
	movl	high_block_linenum(%rip), %esi
	andq	%rax, %rbx
	movq	$2, %rcx
	shrq	%cl, %rbx
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	callq	*56(%rdx)
	jmp	.LBB15_173
.LBB15_13:                              # %sw.bb26
	movq	%r12, %rdi
	callq	*targetm+104(%rip)
	movq	cfun(%rip), %rax
	movl	424(%rax), %r14d
	testq	$16384, %r14            # imm = 0x4000
	je	.LBB15_173
# BB#14:                                # %if.then.i
	movq	current_function_decl(%rip), %rdi
	callq	function_section
	andq	$8, %r14
	je	.LBB15_18
# BB#15:                                # %if.then.i.i
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	movq	$39, %rax
	jne	.LBB15_17
# BB#16:                                # %if.then.i.i
	movq	$2, %rax
.LBB15_17:                              # %if.then.i.i
	movq	reg_names(,%rax,8), %rcx
	movabsq	$.L.str50, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	asm_fprintf
.LBB15_18:                              # %if.end.i.i
	movl	target_flags(%rip), %ecx
	movl	flag_pic(%rip), %eax
	testq	$33554432, %rcx         # imm = 0x2000000
	je	.LBB15_21
# BB#19:                                # %if.end.i.i
	testq	%rbx, %rax
	je	.LBB15_21
# BB#20:                                # %if.then5.i.i
	movabsq	$.L.str51, %rax
	movq	$18, %rsi
	jmp	.LBB15_24
.LBB15_29:                              # %sw.bb27
	movq	%r12, %rdi
	callq	*targetm+112(%rip)
	jmp	.LBB15_173
.LBB15_30:                              # %sw.bb28
	movzbl	app_on(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB15_32
# BB#31:                                # %if.then.i760
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str13, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	fwrite
	xorq	%rax, %rax
	movb	%al, app_on(%rip)
.LBB15_32:                              # %app_disable.exit
	movq	debug_hooks(%rip), %rax
	movl	last_linenum(%rip), %edi
	callq	*88(%rax)
	jmp	.LBB15_173
.LBB15_10:                              # %do.body
	movq	asm_out_file(%rip), %rdi
	movl	32(%r15), %ecx
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str17, %rdx
	jmp	.LBB15_11
.LBB15_12:                              # %do.body20
	movq	asm_out_file(%rip), %rdi
	movl	32(%r15), %ecx
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str18, %rdx
	jmp	.LBB15_11
.LBB15_8:                               # %sw.bb8
	movl	flag_debug_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_173
# BB#9:                                 # %if.then10
	movq	32(%r15), %rax
	movq	asm_out_file(%rip), %rdi
	movl	88(%rax), %ecx
	movabsq	$.L.str15, %rsi
	movabsq	$.L.str16, %rdx
.LBB15_11:                              # %sw.epilog604
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB15_173
.LBB15_131:                             # %for.cond457.preheader
	movslq	(%rcx), %rax
	cmpq	$1, %rax
	jle	.LBB15_137
# BB#132:
	movq	%rsi, -56(%rbp)         # 8-byte Spill
	movq	$2, -48(%rbp)           # 8-byte Folded Spill
	jmp	.LBB15_133
.LBB15_136:                             # %for.inc481.for.body464_crit_edge
                                        #   in Loop: Header=BB15_133 Depth=1
	movq	8(%rax,%rdi,8), %rbx
	incq	%rdi
	movq	%rdi, -48(%rbp)         # 8-byte Spill
.LBB15_133:                             # %for.body464
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_134 Depth 2
	movq	24(%rbx), %r14
.LBB15_134:                             # %do.body476
                                        #   Parent Loop BB15_133 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rcx
	callq	final_scan_insn
	movq	%rax, %rbx
	cmpq	%r14, %rbx
	jne	.LBB15_134
# BB#135:                               # %for.inc481
                                        #   in Loop: Header=BB15_133 Depth=1
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB15_136
.LBB15_137:                             # %for.end483
	movq	$0, final_sequence(%rip)
	jmp	.LBB15_173
.LBB15_175:                             # %if.then88
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_scan_insn, %rcx
	movq	$1947, %rsi             # imm = 0x79B
	jmp	.LBB15_50
.LBB15_178:                             # %if.then514
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_scan_insn, %rcx
	movq	$2551, %rdx             # imm = 0x9F7
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn_not_found
.LBB15_151:                             # %if.then.i773
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.get_insn_template, %rcx
	movq	$1802, %rsi             # imm = 0x70A
	jmp	.LBB15_50
.LBB15_152:                             # %sw.default.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.get_insn_template, %rcx
	movq	$1806, %rsi             # imm = 0x70E
	jmp	.LBB15_50
.LBB15_163:                             # %if.then560
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	try_split
	cmpq	%r15, %rax
	jne	.LBB15_165
# BB#164:                               # %land.lhs.true564
	movq	32(%r15), %rax
	cmpq	%rbx, %rax
	jne	.LBB15_165
# BB#180:                               # %if.then570
	movabsq	$.L.str25, %rax
	movabsq	$.L.str, %rdx
	movabsq	$.L__FUNCTION__.final_scan_insn, %rbx
	movq	$2622, %rcx             # imm = 0xA3E
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%rbx, %r8
	callq	_fatal_insn
.LBB15_179:                             # %if.then532
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_scan_insn, %rcx
	movq	$2596, %rsi             # imm = 0xA24
	jmp	.LBB15_50
.LBB15_49:                              # %if.then74
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_scan_insn, %rcx
	movq	$1929, %rsi             # imm = 0x789
	jmp	.LBB15_50
.LBB15_21:                              # %if.else.i.i
	testq	%rbx, %rax
	je	.LBB15_23
# BB#22:                                # %if.then7.i.i
	movabsq	$.L.str52, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rsi
	callq	fprintf
	jmp	.LBB15_25
.LBB15_165:                             # %if.end571
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_scan_insn, %rcx
	movq	$2628, %rsi             # imm = 0xA44
.LBB15_50:                              # %if.then74
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB15_23:                              # %if.else9.i.i
	movabsq	$.L.str53, %rax
	movq	$13, %rsi
.LBB15_24:                              # %if.end12.i.i
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r12, %rcx
	callq	fwrite
.LBB15_25:                              # %if.end12.i.i
	testq	%rbx, %r14
	je	.LBB15_173
# BB#26:                                # %if.then14.i.i
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	movq	$39, %rax
	jne	.LBB15_28
# BB#27:                                # %if.then14.i.i
	movq	$2, %rax
.LBB15_28:                              # %if.then14.i.i
	movq	reg_names(,%rax,8), %rcx
	movabsq	$.L.str54, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	asm_fprintf
	jmp	.LBB15_173
.Ltmp135:
	.size	final_scan_insn, .Ltmp135-final_scan_insn
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI15_0:
	.quad	.LBB15_173
	.quad	.LBB15_33
	.quad	.LBB15_41
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_13
	.quad	.LBB15_29
	.quad	.LBB15_93
	.quad	.LBB15_30
	.quad	.LBB15_10
	.quad	.LBB15_12
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_173
	.quad	.LBB15_8
	.quad	.LBB15_173

	.text
	.globl	get_insn_template
	.align	16, 0x90
	.type	get_insn_template,@function
get_insn_template:                      # @get_insn_template
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp137:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	(%rdi,%rdi,4), %rax
	movsbq	insn_data+35(,%rax,8), %rdx
	movq	insn_data+8(,%rax,8), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	je	.LBB16_4
# BB#1:                                 # %entry
	cmpq	$3, %rcx
	jne	.LBB16_2
# BB#5:                                 # %sw.bb7
	testq	%rsi, %rsi
	je	.LBB16_6
# BB#8:                                 # %if.end
	movabsq	$recog_data, %rdi
	popq	%rdx
	jmpq	*%rax  # TAILCALL
.LBB16_2:                               # %entry
	cmpq	$2, %rcx
	jne	.LBB16_9
# BB#3:                                 # %sw.bb4
	movslq	which_alternative(%rip), %rcx
	movq	(%rax,%rcx,8), %rax
.LBB16_4:                               # %return
	popq	%rdx
	retq
.LBB16_6:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.get_insn_template, %rcx
	movq	$1802, %rsi             # imm = 0x70A
	jmp	.LBB16_7
.LBB16_9:                               # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.get_insn_template, %rcx
	movq	$1806, %rsi             # imm = 0x70E
.LBB16_7:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp138:
	.size	get_insn_template, .Ltmp138-get_insn_template
	.cfi_endproc

	.globl	output_asm_insn
	.align	16, 0x90
	.type	output_asm_insn,@function
output_asm_insn:                        # @output_asm_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp145:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp146:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp147:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp148:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp149:
	.cfi_def_cfa_offset 48
	subq	$464, %rsp              # imm = 0x1D0
.Ltmp150:
	.cfi_def_cfa_offset 512
.Ltmp151:
	.cfi_offset %rbx, -48
.Ltmp152:
	.cfi_offset %r12, -40
.Ltmp153:
	.cfi_offset %r13, -32
.Ltmp154:
	.cfi_offset %r14, -24
.Ltmp155:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	(%r12), %eax
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB17_64
# BB#1:                                 # %if.end
	xorq	%rax, %rax
	movw	%ax, 60(%rsp)
	movl	%eax, 56(%rsp)
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	asm_out_file(%rip), %rsi
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	movq	$9, %rdi
	callq	putc
	movzbl	(%r12), %edi
	testq	%rdi, %rdi
	je	.LBB17_49
# BB#2:                                 # %while.body.lr.ph
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB17_3:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_46 Depth 2
                                        #     Child Loop BB17_34 Depth 2
                                        #     Child Loop BB17_116 Depth 2
                                        #     Child Loop BB17_133 Depth 2
                                        #     Child Loop BB17_19 Depth 2
	leaq	1(%r12), %r13
	movq	$56, %rcx
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	movq	%rdi, %rax
	andq	%r14, %rax
	cmpq	$122, %rdi
	jg	.LBB17_11
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$10, %rax
	jne	.LBB17_5
# BB#16:                                # %sw.bb
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movl	flag_verbose_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_28
# BB#17:                                # %if.then4
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB17_28
# BB#18:                                #   in Loop: Header=BB17_3 Depth=1
	movq	$1, %rbx
	xorq	%r12, %r12
	.align	16, 0x90
.LBB17_19:                              # %for.body.i
                                        #   Parent Loop BB17_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	60(%rsp,%rbx,4), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	leaq	188(%rsp), %rsi
	callq	get_mem_expr_from_op
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB17_27
# BB#20:                                # %if.then.i
                                        #   in Loop: Header=BB17_19 Depth=2
	movq	asm_out_file(%rip), %rdi
	andq	%r14, %r12
	movq	$44, %rdx
	jne	.LBB17_22
# BB#21:                                # %if.then.i
                                        #   in Loop: Header=BB17_19 Depth=2
	movq	$9, %rdx
.LBB17_22:                              # %if.then.i
                                        #   in Loop: Header=BB17_19 Depth=2
	testq	%r12, %r12
	movabsq	$.L.str24, %rcx
	jne	.LBB17_24
# BB#23:                                # %if.then.i
                                        #   in Loop: Header=BB17_19 Depth=2
	movabsq	$.L.str16, %rcx
.LBB17_24:                              # %if.then.i
                                        #   in Loop: Header=BB17_19 Depth=2
	movl	188(%rsp), %eax
	testq	%rax, %rax
	movabsq	$.L.str49, %r8
	jne	.LBB17_26
# BB#25:                                # %select.mid
                                        #   in Loop: Header=BB17_19 Depth=2
	movabsq	$.L.str24, %r8
.LBB17_26:                              # %select.end
                                        #   in Loop: Header=BB17_19 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str48, %rsi
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	movq	%r15, %rsi
	callq	print_mem_expr
	movq	$1, %r12
.LBB17_27:                              # %for.inc.i
                                        #   in Loop: Header=BB17_19 Depth=2
	movq	%rbx, %rax
	andq	%r14, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%r14, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB17_19
.LBB17_28:                              # %if.end5
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	flag_print_asm_name(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_30
# BB#29:                                # %if.then7
                                        #   in Loop: Header=BB17_3 Depth=1
	callq	output_asm_name
.LBB17_30:                              # %if.end8
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movw	%ax, 60(%rsp)
	movl	%eax, 56(%rsp)
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	asm_out_file(%rip), %rsi
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	movq	$10, %rdi
	callq	putc
	movq	8(%rsp), %rbx           # 8-byte Reload
	jmp	.LBB17_48
	.align	16, 0x90
.LBB17_11:                              # %while.body
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$123, %rax
	je	.LBB17_31
# BB#12:                                # %while.body
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$124, %rax
	jne	.LBB17_13
# BB#45:                                # %sw.bb43
                                        #   in Loop: Header=BB17_3 Depth=1
	testq	%r14, %rbx
	jne	.LBB17_46
# BB#66:                                # %if.else55
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rsi
	movq	$124, %rdi
	jmp	.LBB17_67
	.align	16, 0x90
.LBB17_46:                              # %do.body
                                        #   Parent Loop BB17_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	testq	%rax, %rax
	je	.LBB17_47
# BB#65:                                # %do.cond
                                        #   in Loop: Header=BB17_46 Depth=2
	incq	%r13
	xorq	%rbx, %rbx
	cmpq	$125, %rax
	jne	.LBB17_46
	jmp	.LBB17_48
	.align	16, 0x90
.LBB17_5:                               # %while.body
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$37, %rax
	jne	.LBB17_138
# BB#6:                                 # %sw.bb63
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movzbl	(%r13), %ebx
	movq	$56, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$56, %rcx
	sarq	%cl, %r15
	cmpq	$61, %rbx
	jne	.LBB17_7
# BB#69:                                # %if.then74
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	movl	insn_counter(%rip), %edx
	addq	$2, %r12
	xorq	%rax, %rax
	movabsq	$.L.str31, %rsi
	callq	fprintf
	jmp	.LBB17_9
.LBB17_31:                              # %sw.bb10
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$1, %r15
	testq	%r14, %rbx
	je	.LBB17_33
# BB#32:                                # %if.then12
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str29, %rdi
	callq	output_operand_lossage
	movq	%rbx, %r15
.LBB17_33:                              # %if.end13
                                        #   in Loop: Header=BB17_3 Depth=1
	movslq	dialect_number(%rip), %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jle	.LBB17_42
	.align	16, 0x90
.LBB17_34:                              # %while.cond16
                                        #   Parent Loop BB17_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %ecx
	testq	%rcx, %rcx
	je	.LBB17_38
# BB#35:                                # %while.cond16
                                        #   in Loop: Header=BB17_34 Depth=2
	cmpq	$125, %rcx
	je	.LBB17_38
# BB#36:                                # %land.rhs
                                        #   in Loop: Header=BB17_34 Depth=2
	incq	%r13
	cmpq	$124, %rcx
	jne	.LBB17_34
# BB#37:                                # %land.rhs.while.end_crit_edge
                                        #   in Loop: Header=BB17_34 Depth=2
	movzbl	(%r13), %ecx
	.align	16, 0x90
.LBB17_38:                              # %while.end
                                        #   in Loop: Header=BB17_34 Depth=2
	andq	$255, %rcx
	cmpq	$125, %rcx
	je	.LBB17_42
# BB#39:                                # %while.end
                                        #   in Loop: Header=BB17_34 Depth=2
	cmpq	$124, %rcx
	jne	.LBB17_41
# BB#40:                                # %if.then35
                                        #   in Loop: Header=BB17_34 Depth=2
	incq	%r13
.LBB17_41:                              # %for.inc
                                        #   in Loop: Header=BB17_34 Depth=2
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB17_34
.LBB17_42:                              # %for.end
                                        #   in Loop: Header=BB17_3 Depth=1
	movzbl	(%r13), %eax
	testq	%rax, %rax
	jne	.LBB17_43
# BB#44:                                # %if.then41
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str30, %rdi
	callq	output_operand_lossage
	movq	%r15, %rbx
	jmp	.LBB17_48
.LBB17_13:                              # %while.body
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$125, %rax
	jne	.LBB17_138
# BB#14:                                # %sw.bb58
                                        #   in Loop: Header=BB17_3 Depth=1
	testq	%r14, %rbx
	jne	.LBB17_15
# BB#68:                                # %if.then60
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rsi
	movq	$125, %rdi
.LBB17_67:                              # %while.cond.backedge
                                        #   in Loop: Header=BB17_3 Depth=1
	callq	putc
	xorq	%rbx, %rbx
	jmp	.LBB17_48
	.align	16, 0x90
.LBB17_138:                             # %sw.default
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rsi
	callq	putc
	jmp	.LBB17_48
.LBB17_43:                              #   in Loop: Header=BB17_3 Depth=1
	movq	%r15, %rbx
	jmp	.LBB17_48
.LBB17_7:                               # %sw.bb63
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$37, %rbx
	jne	.LBB17_70
# BB#8:                                 # %if.then67
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rsi
	addq	$2, %r12
	movq	$37, %rdi
	callq	putc
.LBB17_9:                               # %while.cond.backedge
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r12, %r13
	movq	8(%rsp), %rbx           # 8-byte Reload
	jmp	.LBB17_48
.LBB17_47:                              # %if.then49
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movabsq	$.L.str30, %rdi
	callq	output_operand_lossage
	jmp	.LBB17_48
.LBB17_15:                              #   in Loop: Header=BB17_3 Depth=1
	xorq	%rbx, %rbx
	jmp	.LBB17_48
.LBB17_70:                              # %if.else77
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r15, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$136, %rax
	jne	.LBB17_71
# BB#117:                               # %if.else207
                                        #   in Loop: Header=BB17_3 Depth=1
	testq	$4, %rax
	jne	.LBB17_118
# BB#135:                               # %if.else250
                                        #   in Loop: Header=BB17_3 Depth=1
	andq	$254, %rbx
	cmpq	$42, %rbx
	jne	.LBB17_137
# BB#136:                               # %if.then258
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	addq	$2, %r12
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	print_operand
	jmp	.LBB17_9
.LBB17_71:                              # %if.then82
                                        #   in Loop: Header=BB17_3 Depth=1
	leaq	2(%r12), %rdi
	callq	atoi
	movzbl	2(%r12), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	movq	%rax, %r13
	testq	$4, %rcx
	jne	.LBB17_73
# BB#72:                                # %if.then93
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str32, %rdi
	callq	output_operand_lossage
	jmp	.LBB17_113
.LBB17_118:                             # %if.then215
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r13, %rdi
	callq	atoi
	cmpq	$0, this_is_asm_operands(%rip)
	movq	%rax, %rbx
	je	.LBB17_122
# BB#119:                               # %land.lhs.true218
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB17_121
# BB#120:                               # %land.lhs.true218
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	insn_noperands(%rip), %eax
	andq	%r14, %rax
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jae	.LBB17_121
.LBB17_122:                             # %if.else225
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r15,8), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	movq	%rax, 192(%rsp)
	je	.LBB17_129
# BB#123:                               # %land.lhs.true.i395
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	jne	.LBB17_126
# BB#124:                               # %if.end.i398
                                        #   in Loop: Header=BB17_3 Depth=1
	leaq	192(%rsp), %rdi
	callq	alter_subreg
	testq	%rax, %rax
	movq	%rax, 192(%rsp)
	je	.LBB17_129
# BB#125:                               # %if.end.i398.land.lhs.true2.i402_crit_edge
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	(%rax), %ecx
.LBB17_126:                             # %land.lhs.true2.i402
                                        #   in Loop: Header=BB17_3 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB17_127
# BB#128:                               # %land.lhs.true6.i406
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	movq	%rax, %r13
	jb	.LBB17_129
	jmp	.LBB17_139
.LBB17_73:                              # %if.else94
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$0, this_is_asm_operands(%rip)
	je	.LBB17_77
# BB#74:                                # %land.lhs.true96
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB17_76
# BB#75:                                # %land.lhs.true96
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	insn_noperands(%rip), %eax
	andq	%r14, %rax
	movq	%r13, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jae	.LBB17_76
.LBB17_77:                              # %if.else102
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	cmpq	$97, %rbx
	je	.LBB17_88
# BB#78:                                # %if.else102
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$99, %rbx
	jne	.LBB17_79
# BB#89:                                # %if.then117
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-54(%rdx), %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_92
# BB#90:                                # %if.then117
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24595, %rdi            # imm = 0x6013
	je	.LBB17_92
# BB#91:                                # %if.then150
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	callq	output_addr_const
	jmp	.LBB17_113
.LBB17_137:                             # %if.else261
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str35, %rdi
	callq	output_operand_lossage
	movq	8(%rsp), %rbx           # 8-byte Reload
	jmp	.LBB17_48
.LBB17_88:                              # %if.then111
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rax
	movq	%rax, 192(%rsp)
	leaq	192(%rsp), %rdi
	callq	walk_alter_subreg
	movq	asm_out_file(%rip), %rdi
	movq	192(%rsp), %rsi
	callq	print_operand_address
	jmp	.LBB17_113
.LBB17_79:                              # %if.else102
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$108, %rbx
	jne	.LBB17_100
# BB#80:                                # %if.then105
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$67, %rdx
	jne	.LBB17_82
# BB#81:                                # %if.then.i359
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	8(%rax), %rax
	movl	(%rax), %ecx
.LBB17_82:                              # %if.end.i
                                        #   in Loop: Header=BB17_3 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	je	.LBB17_85
# BB#83:                                # %if.end.i
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$37, %rcx
	jne	.LBB17_86
# BB#84:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	40(%rax), %ecx
	movabsq	$4294967208, %rdx       # imm = 0xFFFFFFA8
	cmpq	%rdx, %rcx
	jne	.LBB17_86
.LBB17_85:                              # %do.body.i
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	48(%rax), %ecx
	xorq	%rax, %rax
	leaq	192(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str19, %rdx
	callq	sprintf
	jmp	.LBB17_87
.LBB17_121:                             # %if.then224
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str33, %rdi
	callq	output_operand_lossage
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	jmp	.LBB17_130
.LBB17_76:                              # %if.then101
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str33, %rdi
	callq	output_operand_lossage
	jmp	.LBB17_113
.LBB17_92:                              # %if.else153
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%rsi, 192(%rsp)
	xorq	%rbx, %rbx
	testq	%rsi, %rsi
	je	.LBB17_99
# BB#93:                                # %land.lhs.true.i367
                                        #   in Loop: Header=BB17_3 Depth=1
	cmpq	$63, %rdx
	leaq	192(%rsp), %rdi
	jne	.LBB17_96
# BB#94:                                # %if.end.i369
                                        #   in Loop: Header=BB17_3 Depth=1
	callq	alter_subreg
	movq	%rax, %rsi
	testq	%rsi, %rsi
	movq	%rsi, 192(%rsp)
	je	.LBB17_99
# BB#95:                                # %if.end.i369.land.lhs.true2.i_crit_edge
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	(%rsi), %eax
.LBB17_96:                              # %land.lhs.true2.i
                                        #   in Loop: Header=BB17_3 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB17_97
# BB#98:                                # %land.lhs.true6.i
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	8(%rsi), %eax
	cmpq	$53, %rax
	movq	%rsi, %rbx
	jb	.LBB17_99
	jmp	.LBB17_139
.LBB17_127:                             #   in Loop: Header=BB17_3 Depth=1
	movq	%rax, %r13
.LBB17_129:                             # %output_operand.exit408
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	callq	print_operand
.LBB17_130:                             # %if.end228
                                        #   in Loop: Header=BB17_3 Depth=1
	movzbl	32(%rsp,%r15), %eax
	testq	%rax, %rax
	jne	.LBB17_132
# BB#131:                               # %if.then232
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	leaq	1(%rdx), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	%ebx, 64(%rsp,%rdx,4)
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB17_132:                             # %if.end236
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	$1, %rax
	movb	%al, 32(%rsp,%r15)
	.align	16, 0x90
.LBB17_133:                             # %while.cond239
                                        #   Parent Loop BB17_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r12), %eax
	movzbl	_sch_istable(%rax,%rax), %eax
	incq	%r12
	testq	$4, %rax
	jne	.LBB17_133
# BB#134:                               #   in Loop: Header=BB17_3 Depth=1
	movq	%r12, %r13
	jmp	.LBB17_48
.LBB17_100:                             # %if.else157
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rdx,8), %rax
	cmpq	$110, %rbx
	jne	.LBB17_104
# BB#101:                               # %if.then160
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%rdx, %rbx
	movzbl	(%rax), %ecx
	movq	asm_out_file(%rip), %rsi
	cmpq	$54, %rcx
	jne	.LBB17_103
# BB#102:                               # %if.then167
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	8(%rax), %rdx
	negq	%rdx
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movabsq	$.L.str34, %rsi
	callq	fprintf
	jmp	.LBB17_113
.LBB17_104:                             # %if.else177
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	%rax, 192(%rsp)
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB17_111
# BB#105:                               # %land.lhs.true.i376
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$63, %rdx
	jne	.LBB17_108
# BB#106:                               # %if.end.i379
                                        #   in Loop: Header=BB17_3 Depth=1
	leaq	192(%rsp), %rdi
	callq	alter_subreg
	testq	%rax, %rax
	movq	%rax, 192(%rsp)
	je	.LBB17_111
# BB#107:                               # %if.end.i379.land.lhs.true2.i383_crit_edge
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	(%rax), %ecx
.LBB17_108:                             # %land.lhs.true2.i383
                                        #   in Loop: Header=BB17_3 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB17_109
# BB#110:                               # %land.lhs.true6.i387
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	movq	%rax, %rbx
	jb	.LBB17_111
	jmp	.LBB17_139
.LBB17_86:                              # %if.else.i
                                        #   in Loop: Header=BB17_3 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str36, %rdi
	callq	output_operand_lossage
	leaq	192(%rsp), %rbx
.LBB17_87:                              # %output_asm_label.exit
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	jmp	.LBB17_113
.LBB17_103:                             # %if.else172
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$45, %rdi
	callq	putc
	movq	asm_out_file(%rip), %rdi
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %rsi
	callq	output_addr_const
	jmp	.LBB17_113
.LBB17_97:                              #   in Loop: Header=BB17_3 Depth=1
	movq	%rsi, %rbx
.LBB17_99:                              # %output_operand.exit
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	movq	$99, %rdx
	movq	%rbx, %rsi
	jmp	.LBB17_112
.LBB17_109:                             #   in Loop: Header=BB17_3 Depth=1
	movq	%rax, %rbx
.LBB17_111:                             # %output_operand.exit389
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
.LBB17_112:                             # %if.end185
                                        #   in Loop: Header=BB17_3 Depth=1
	callq	print_operand
.LBB17_113:                             # %if.end185
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	32(%rsp,%rax), %ecx
	testq	%rcx, %rcx
	movq	8(%rsp), %rbx           # 8-byte Reload
	jne	.LBB17_115
# BB#114:                               # %if.then189
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	leaq	1(%rsi), %rdx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	%r13d, 64(%rsp,%rsi,4)
	movq	%rdx, 24(%rsp)          # 8-byte Spill
.LBB17_115:                             # %if.end193
                                        #   in Loop: Header=BB17_3 Depth=1
	incq	%r12
	movq	$1, %rcx
	movb	%cl, 32(%rsp,%rax)
	movq	%r12, %r13
	.align	16, 0x90
.LBB17_116:                             # %while.cond196
                                        #   Parent Loop BB17_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r13), %eax
	movzbl	_sch_istable(%rax,%rax), %eax
	incq	%r13
	testq	$4, %rax
	jne	.LBB17_116
	.align	16, 0x90
.LBB17_48:                              # %while.cond.backedge
                                        #   in Loop: Header=BB17_3 Depth=1
	movzbl	(%r13), %edi
	testq	%rdi, %rdi
	movq	%r13, %r12
	jne	.LBB17_3
.LBB17_49:                              # %while.end268
	movl	flag_verbose_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_61
# BB#50:                                # %if.then270
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB17_61
# BB#51:
	xorq	%r15, %r15
	movq	$1, %rbx
	leaq	460(%rsp), %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, 24(%rsp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB17_52:                              # %for.body.i419
                                        # =>This Inner Loop Header: Depth=1
	movslq	60(%rsp,%rbx,4), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	%r12, %rsi
	callq	get_mem_expr_from_op
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB17_60
# BB#53:                                # %if.then.i426
                                        #   in Loop: Header=BB17_52 Depth=1
	movq	asm_out_file(%rip), %rdi
	andq	%r13, %r15
	movq	$44, %rdx
	jne	.LBB17_55
# BB#54:                                # %if.then.i426
                                        #   in Loop: Header=BB17_52 Depth=1
	movq	$9, %rdx
.LBB17_55:                              # %if.then.i426
                                        #   in Loop: Header=BB17_52 Depth=1
	testq	%r15, %r15
	movabsq	$.L.str24, %rcx
	jne	.LBB17_57
# BB#56:                                # %if.then.i426
                                        #   in Loop: Header=BB17_52 Depth=1
	movabsq	$.L.str16, %rcx
.LBB17_57:                              # %if.then.i426
                                        #   in Loop: Header=BB17_52 Depth=1
	movl	460(%rsp), %eax
	testq	%rax, %rax
	movabsq	$.L.str49, %r8
	jne	.LBB17_59
# BB#58:                                # %select.mid30
                                        #   in Loop: Header=BB17_52 Depth=1
	movabsq	$.L.str24, %r8
.LBB17_59:                              # %select.end29
                                        #   in Loop: Header=BB17_52 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str48, %rsi
	callq	fprintf
	movq	asm_out_file(%rip), %rdi
	movq	%r14, %rsi
	callq	print_mem_expr
	movq	$1, %r15
.LBB17_60:                              # %for.inc.i431
                                        #   in Loop: Header=BB17_52 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	incq	%rbx
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB17_52
.LBB17_61:                              # %if.end272
	movl	flag_print_asm_name(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_63
# BB#62:                                # %if.then274
	callq	output_asm_name
.LBB17_63:                              # %if.end275
	movq	asm_out_file(%rip), %rsi
	movq	$10, %rdi
	callq	putc
.LBB17_64:                              # %cleanup
	addq	$464, %rsp              # imm = 0x1D0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_139:                             # %if.then8.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.output_operand, %rcx
	movq	$3349, %rsi             # imm = 0xD15
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp156:
	.size	output_asm_insn, .Ltmp156-output_asm_insn
	.cfi_endproc

	.globl	cleanup_subreg_operands
	.align	16, 0x90
	.type	cleanup_subreg_operands,@function
cleanup_subreg_operands:                # @cleanup_subreg_operands
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp160:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp161:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp162:
	.cfi_def_cfa_offset 32
.Ltmp163:
	.cfi_offset %rbx, -24
.Ltmp164:
	.cfi_offset %r14, -16
	callq	extract_insn_cached
	movzbl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB18_4
# BB#1:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB18_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data+240(,%rbx,8), %rdi
	movq	(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$63, %rax
	jne	.LBB18_9
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB18_2 Depth=1
	callq	alter_subreg
	jmp	.LBB18_12
	.align	16, 0x90
.LBB18_9:                               # %if.else
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	recog_data(,%rbx,8), %rax
	movzbl	(%rax), %ecx
	addq	$-66, %rcx
	andq	%r14, %rcx
	cmpq	$12, %rcx
	ja	.LBB18_13
# BB#10:                                # %if.else
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$4609, %rax             # imm = 0x1201
	je	.LBB18_13
# BB#11:                                # %if.then27
                                        #   in Loop: Header=BB18_2 Depth=1
	callq	walk_alter_subreg
.LBB18_12:                              # %for.inc
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	%rax, recog_data(,%rbx,8)
.LBB18_13:                              # %for.inc
                                        #   in Loop: Header=BB18_2 Depth=1
	movzbl	recog_data+996(%rip), %eax
	incq	%rbx
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB18_2
.LBB18_4:                               # %for.cond34.preheader
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB18_19
# BB#5:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB18_6:                               # %for.body38
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data+960(,%rbx,8), %rdi
	movq	(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$74, %rax
	jg	.LBB18_14
# BB#7:                                 # %for.body38
                                        #   in Loop: Header=BB18_6 Depth=1
	cmpq	$63, %rax
	jne	.LBB18_8
# BB#20:                                # %if.then45
                                        #   in Loop: Header=BB18_6 Depth=1
	callq	alter_subreg
	jmp	.LBB18_17
	.align	16, 0x90
.LBB18_14:                              # %for.body38
                                        #   in Loop: Header=BB18_6 Depth=1
	cmpq	$75, %rax
	je	.LBB18_16
# BB#15:                                # %for.body38
                                        #   in Loop: Header=BB18_6 Depth=1
	cmpq	$78, %rax
	jne	.LBB18_18
	jmp	.LBB18_16
	.align	16, 0x90
.LBB18_8:                               # %for.body38
                                        #   in Loop: Header=BB18_6 Depth=1
	cmpq	$66, %rax
	jne	.LBB18_18
.LBB18_16:                              # %if.then72
                                        #   in Loop: Header=BB18_6 Depth=1
	callq	walk_alter_subreg
.LBB18_17:                              # %for.inc80
                                        #   in Loop: Header=BB18_6 Depth=1
	movq	recog_data+960(,%rbx,8), %rcx
	movq	%rax, (%rcx)
.LBB18_18:                              # %for.inc80
                                        #   in Loop: Header=BB18_6 Depth=1
	incq	%rbx
	movsbq	recog_data+997(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB18_6
.LBB18_19:                              # %for.end82
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp165:
	.size	cleanup_subreg_operands, .Ltmp165-cleanup_subreg_operands
	.cfi_endproc

	.globl	alter_subreg
	.align	16, 0x90
	.type	alter_subreg,@function
alter_subreg:                           # @alter_subreg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp170:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp171:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp172:
	.cfi_def_cfa_offset 32
.Ltmp173:
	.cfi_offset %rbx, -32
.Ltmp174:
	.cfi_offset %r14, -24
.Ltmp175:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rbx
	movq	8(%rbx), %r15
	movl	(%r15), %edx
	movzbl	2(%rbx), %eax
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB19_2
# BB#1:                                 # %if.then
	movl	16(%rbx), %edx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	movq	%rax, (%r14)
	jmp	.LBB19_6
.LBB19_2:                               # %if.else
	movl	16(%rbx), %r8d
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%r8, %rcx
	callq	simplify_subreg
	testq	%rax, %rax
	je	.LBB19_4
# BB#3:                                 # %if.then17
	movq	%rax, (%r14)
	jmp	.LBB19_6
.LBB19_4:                               # %if.else18
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB19_7
# BB#5:                                 # %if.then23
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	subreg_hard_regno
	movl	(%rbx), %ecx
	andq	$-65536, %rcx           # imm = 0xFFFFFFFFFFFF0000
	orq	$61, %rcx
	movl	%ecx, (%rbx)
	movl	%eax, 8(%rbx)
	movl	16(%r15), %eax
	movl	%eax, 16(%rbx)
	movabsq	$3758030909, %rax       # imm = 0xDFFF003D
	andq	%rcx, %rax
	movl	%eax, (%rbx)
	movq	(%r14), %rax
.LBB19_6:                               # %if.end40
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB19_7:                               # %if.else38
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.alter_subreg, %rcx
	movq	$2761, %rsi             # imm = 0xAC9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp176:
	.size	alter_subreg, .Ltmp176-alter_subreg
	.cfi_endproc

	.align	16, 0x90
	.type	walk_alter_subreg,@function
walk_alter_subreg:                      # @walk_alter_subreg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp180:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp181:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp182:
	.cfi_def_cfa_offset 32
.Ltmp183:
	.cfi_offset %rbx, -24
.Ltmp184:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	(%r14), %rbx
	movzbl	(%rbx), %eax
	cmpq	$74, %rax
	jg	.LBB20_4
# BB#1:                                 # %entry
	cmpq	$63, %rax
	jne	.LBB20_2
# BB#8:                                 # %sw.bb19
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	alter_subreg  # TAILCALL
.LBB20_4:                               # %entry
	cmpq	$75, %rax
	je	.LBB20_6
# BB#5:                                 # %entry
	cmpq	$78, %rax
	jne	.LBB20_7
.LBB20_6:                               # %sw.bb
	leaq	8(%rbx), %rdi
	callq	walk_alter_subreg
	movq	%rax, 8(%rbx)
	leaq	16(%rbx), %rdi
	callq	walk_alter_subreg
	movq	%rax, 16(%rbx)
	jmp	.LBB20_7
.LBB20_2:                               # %entry
	cmpq	$66, %rax
	jne	.LBB20_7
# BB#3:                                 # %sw.bb11
	leaq	8(%rbx), %rdi
	callq	walk_alter_subreg
	movq	%rax, 8(%rbx)
.LBB20_7:                               # %return
	movq	(%r14), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp185:
	.size	walk_alter_subreg, .Ltmp185-walk_alter_subreg
	.cfi_endproc

	.globl	output_operand_lossage
	.align	16, 0x90
	.type	output_operand_lossage,@function
output_operand_lossage:                 # @output_operand_lossage
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$88, %rsp
.Ltmp187:
	.cfi_def_cfa_offset 96
	movq	%rdi, %r10
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	cmpq	$0, this_is_asm_operands(%rip)
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	je	.LBB21_2
# BB#1:
	movabsq	$.L.str26, %rdx
	jmp	.LBB21_3
.LBB21_2:                               # %select.mid
	movabsq	$.L.str27, %rdx
.LBB21_3:                               # %select.end
	movabsq	$.L.str28, %rsi
	leaq	80(%rsp), %rdi
	xorq	%rax, %rax
	movq	%r10, %rcx
	callq	asprintf
	movq	80(%rsp), %rsi
	leaq	72(%rsp), %rdi
	leaq	48(%rsp), %rdx
	callq	vasprintf
	movq	this_is_asm_operands(%rip), %rdi
	movq	72(%rsp), %rdx
	testq	%rdi, %rdi
	je	.LBB21_5
# BB#4:                                 # %if.then
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	callq	error_for_asm
	movq	80(%rsp), %rdi
	callq	free
	movq	72(%rsp), %rdi
	callq	free
	addq	$88, %rsp
	retq
.LBB21_5:                               # %if.else
	movabsq	$.L.str7, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	internal_error
.Ltmp188:
	.size	output_operand_lossage, .Ltmp188-output_operand_lossage
	.cfi_endproc

	.align	16, 0x90
	.type	output_asm_name,@function
output_asm_name:                        # @output_asm_name
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp191:
	.cfi_def_cfa_offset 16
.Ltmp192:
	.cfi_offset %rbx, -16
	movq	debug_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB22_4
# BB#1:                                 # %if.then
	movslq	40(%rax), %rcx
	movq	asm_out_file(%rip), %rdi
	leaq	(%rcx,%rcx,4), %rbx
	movq	insn_data(,%rbx,8), %r8
	movl	8(%rax), %ecx
	movabsq	$.L.str45, %rsi
	movabsq	$.L.str16, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movsbq	insn_data+34(,%rbx,8), %rax
	cmpq	$2, %rax
	jl	.LBB22_3
# BB#2:                                 # %if.then8
	movl	which_alternative(%rip), %edx
	movq	asm_out_file(%rip), %rdi
	incq	%rdx
	movabsq	$.L.str46, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB22_3:                               # %if.end
	movq	asm_out_file(%rip), %rbx
	movq	debug_insn(%rip), %rdi
	callq	get_attr_length
	movq	%rax, %rcx
	movabsq	$.L.str47, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movq	$0, debug_insn(%rip)
.LBB22_4:                               # %if.end12
	popq	%rbx
	retq
.Ltmp193:
	.size	output_asm_name, .Ltmp193-output_asm_name
	.cfi_endproc

	.globl	output_asm_label
	.align	16, 0x90
	.type	output_asm_label,@function
output_asm_label:                       # @output_asm_label
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$264, %rsp              # imm = 0x108
.Ltmp195:
	.cfi_def_cfa_offset 272
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$67, %rcx
	jne	.LBB23_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movl	(%rdi), %eax
.LBB23_2:                               # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$36, %rax
	je	.LBB23_5
# BB#3:                                 # %if.end
	cmpq	$37, %rax
	jne	.LBB23_6
# BB#4:                                 # %land.lhs.true
	movl	40(%rdi), %eax
	movabsq	$4294967208, %rcx       # imm = 0xFFFFFFA8
	cmpq	%rcx, %rax
	jne	.LBB23_6
.LBB23_5:                               # %do.body
	movl	48(%rdi), %ecx
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str19, %rdx
	leaq	(%rsp), %rdi
	xorq	%rax, %rax
	callq	sprintf
	jmp	.LBB23_7
.LBB23_6:                               # %if.else
	movabsq	$.L.str36, %rdi
	xorq	%rax, %rax
	callq	output_operand_lossage
.LBB23_7:                               # %if.end14
	movq	asm_out_file(%rip), %rdi
	leaq	(%rsp), %rsi
	callq	assemble_name
	addq	$264, %rsp              # imm = 0x108
	retq
.Ltmp196:
	.size	output_asm_label, .Ltmp196-output_asm_label
	.cfi_endproc

	.globl	output_address
	.align	16, 0x90
	.type	output_address,@function
output_address:                         # @output_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp198:
	.cfi_def_cfa_offset 16
	movq	%rdi, (%rsp)
	leaq	(%rsp), %rdi
	callq	walk_alter_subreg
	movq	asm_out_file(%rip), %rdi
	movq	(%rsp), %rsi
	popq	%rax
	jmp	print_operand_address  # TAILCALL
.Ltmp199:
	.size	output_address, .Ltmp199-output_address
	.cfi_endproc

	.globl	output_addr_const
	.align	16, 0x90
	.type	output_addr_const,@function
output_addr_const:                      # @output_addr_const
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp204:
	.cfi_def_cfa_offset 24
	subq	$264, %rsp              # imm = 0x108
.Ltmp205:
	.cfi_def_cfa_offset 288
.Ltmp206:
	.cfi_offset %rbx, -24
.Ltmp207:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movl	(%rbx), %eax
	movq	%rdi, %r14
.LBB25_1:                               # %restart
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$119, %rcx
	jg	.LBB25_39
# BB#2:                                 # %restart
                                        #   in Loop: Header=BB25_1 Depth=1
	cmpq	$53, %rcx
	jle	.LBB25_3
# BB#6:                                 # %restart
                                        #   in Loop: Header=BB25_1 Depth=1
	cmpq	$74, %rcx
	jle	.LBB25_7
# BB#9:                                 # %restart
                                        #   in Loop: Header=BB25_1 Depth=1
	cmpq	$76, %rcx
	jne	.LBB25_10
# BB#32:                                # %sw.bb97
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	%rbx, %rdi
	callq	simplify_subtraction
	movq	%rax, %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$76, %rcx
	jne	.LBB25_1
# BB#33:                                # %if.end103
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	output_addr_const
	movq	$45, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	16(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$68, %rax
	je	.LBB25_15
# BB#34:                                # %if.end103
	cmpq	$59, %rax
	je	.LBB25_15
# BB#35:                                # %if.end103
	cmpq	$54, %rax
	jne	.LBB25_37
# BB#36:                                # %land.lhs.true
	cmpq	$0, 8(%rsi)
	jns	.LBB25_15
.LBB25_37:                              # %if.else139
	movq	targetm(%rip), %rdi
	movq	%r14, %rsi
	callq	fputs
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	output_addr_const
	movq	targetm+8(%rip), %rdi
	movq	%r14, %rsi
	callq	fputs
	jmp	.LBB25_38
.LBB25_39:                              # %restart
	addq	$-120, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jb	.LBB25_14
	jmp	.LBB25_4
.LBB25_3:                               # %restart
	cmpq	$36, %rcx
	je	.LBB25_19
	jmp	.LBB25_4
.LBB25_7:                               # %restart
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$14, %rdx
	ja	.LBB25_4
# BB#8:                                 # %restart
	jmpq	*.LJTI25_0(,%rdx,8)
.LBB25_20:                              # %sw.bb10
	movq	8(%rbx), %rdx
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	jmp	.LBB25_38
.LBB25_21:                              # %sw.bb18
	testq	$16711680, %rax         # imm = 0xFF0000
	je	.LBB25_22
# BB#28:                                # %if.else47
	movabsq	$.L.str39, %rdi
	jmp	.LBB25_5
.LBB25_16:                              # %sw.bb
	movq	$46, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB25_38
.LBB25_18:                              # %sw.bb2
	movq	8(%rbx), %rbx
.LBB25_19:                              # %do.body
	movl	48(%rbx), %ecx
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str19, %rdx
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	jmp	.LBB25_38
.LBB25_17:                              # %sw.bb1
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	assemble_name
	jmp	.LBB25_38
.LBB25_22:                              # %if.then
	movq	24(%rbx), %rdx
	movq	16(%rbx), %rcx
	testq	%rdx, %rdx
	je	.LBB25_24
# BB#23:                                # %if.then24
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	jmp	.LBB25_38
.LBB25_24:                              # %if.else
	testq	%rcx, %rcx
	js	.LBB25_25
# BB#27:                                # %if.else41
	movabsq	$.L.str34, %rsi
	jmp	.LBB25_26
.LBB25_10:                              # %restart
	cmpq	$75, %rcx
	jne	.LBB25_4
# BB#11:                                # %sw.bb49
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$54, %rax
	jne	.LBB25_29
# BB#12:                                # %if.then56
	movq	16(%rbx), %rsi
	movq	%r14, %rdi
	callq	output_addr_const
	movq	8(%rbx), %rsi
	cmpq	$0, 8(%rsi)
	js	.LBB25_15
# BB#13:                                # %if.then67
	movq	$43, %rdi
	movq	%r14, %rsi
	callq	fputc
.LBB25_14:                              # %if.end69
	movq	8(%rbx), %rsi
.LBB25_15:                              # %if.end69
	movq	%r14, %rdi
	callq	output_addr_const
	jmp	.LBB25_38
.LBB25_4:                               # %sw.default
	movabsq	$.L.str42, %rdi
.LBB25_5:                               # %sw.epilog
	xorq	%rax, %rax
	callq	output_operand_lossage
.LBB25_38:                              # %sw.epilog
	addq	$264, %rsp              # imm = 0x108
	popq	%rbx
	popq	%r14
	retq
.LBB25_25:                              # %if.then36
	movabsq	$.L.str38, %rsi
.LBB25_26:                              # %sw.epilog
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	jmp	.LBB25_38
.LBB25_29:                              # %if.else73
	movq	%r14, %rdi
	callq	output_addr_const
	movq	16(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$54, %rax
	jne	.LBB25_31
# BB#30:                                # %lor.lhs.false
	cmpq	$0, 8(%rsi)
	js	.LBB25_15
.LBB25_31:                              # %if.then90
	movq	$43, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	16(%rbx), %rsi
	jmp	.LBB25_15
.Ltmp208:
	.size	output_addr_const, .Ltmp208-output_addr_const
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI25_0:
	.quad	.LBB25_20
	.quad	.LBB25_21
	.quad	.LBB25_4
	.quad	.LBB25_4
	.quad	.LBB25_14
	.quad	.LBB25_16
	.quad	.LBB25_4
	.quad	.LBB25_4
	.quad	.LBB25_4
	.quad	.LBB25_14
	.quad	.LBB25_4
	.quad	.LBB25_4
	.quad	.LBB25_4
	.quad	.LBB25_18
	.quad	.LBB25_17

	.text
	.globl	asm_fprintf
	.align	16, 0x90
	.type	asm_fprintf,@function
asm_fprintf:                            # @asm_fprintf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp215:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp216:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp217:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp218:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp219:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp220:
	.cfi_def_cfa_offset 144
.Ltmp221:
	.cfi_offset %rbx, -48
.Ltmp222:
	.cfi_offset %r12, -40
.Ltmp223:
	.cfi_offset %r13, -32
.Ltmp224:
	.cfi_offset %r14, -24
.Ltmp225:
	.cfi_offset %r15, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	144(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movq	$37, %rax
	movb	%al, 86(%rsp)
	movzbl	(%rsi), %eax
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB26_29
# BB#1:                                 # %while.body.lr.ph
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$0, %r15
	movabsq	$1, %r14
	.align	16, 0x90
.LBB26_2:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB26_10 Depth 2
                                        #     Child Loop BB26_17 Depth 2
                                        #     Child Loop BB26_20 Depth 2
	leaq	1(%rsi), %r12
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$122, %rax
	jg	.LBB26_6
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$37, %rcx
	jne	.LBB26_57
# BB#4:                                 # %sw.bb27
                                        #   in Loop: Header=BB26_2 Depth=1
	movzbl	1(%rsi), %eax
	addq	$2, %rsi
	movq	$56, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	cmpq	$46, %rax
	je	.LBB26_5
# BB#19:                                # %sw.bb27
                                        #   in Loop: Header=BB26_2 Depth=1
	movzbl	_sch_istable(%rax,%rax), %ecx
	andq	$4, %rcx
	testq	$65535, %rcx            # imm = 0xFFFF
	leaq	87(%rsp), %rdx
	movq	%rsi, %r12
	je	.LBB26_22
	jmp	.LBB26_20
	.align	16, 0x90
.LBB26_6:                               # %while.body
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$125, %rcx
	je	.LBB26_28
# BB#7:                                 # %while.body
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$124, %rcx
	jne	.LBB26_8
	.align	16, 0x90
.LBB26_17:                              # %while.cond16
                                        #   Parent Loop BB26_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB26_28
# BB#18:                                # %land.rhs19
                                        #   in Loop: Header=BB26_17 Depth=2
	incq	%r12
	cmpq	$125, %rax
	jne	.LBB26_17
	jmp	.LBB26_28
.LBB26_8:                               # %while.body
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$123, %rcx
	jne	.LBB26_57
# BB#9:                                 # %for.cond.preheader
                                        #   in Loop: Header=BB26_2 Depth=1
	movslq	dialect_number(%rip), %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jle	.LBB26_28
	.align	16, 0x90
.LBB26_10:                              # %while.cond3
                                        #   Parent Loop BB26_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r12), %esi
	xorq	%rcx, %rcx
	testq	%rsi, %rsi
	je	.LBB26_14
# BB#11:                                # %land.rhs
                                        #   in Loop: Header=BB26_10 Depth=2
	incq	%r12
	cmpq	$124, %rsi
	jne	.LBB26_10
# BB#12:                                # %land.rhs.while.end_crit_edge
                                        #   in Loop: Header=BB26_10 Depth=2
	movzbl	(%r12), %ecx
	cmpq	$124, %rcx
	movq	%r15, %rcx
	jne	.LBB26_14
# BB#13:                                # %land.rhs.while.end_crit_edge
                                        #   in Loop: Header=BB26_10 Depth=2
	movq	%r14, %rcx
.LBB26_14:                              # %while.end
                                        #   in Loop: Header=BB26_10 Depth=2
	testq	%rcx, %rcx
	je	.LBB26_16
# BB#15:                                #   in Loop: Header=BB26_10 Depth=2
	incq	%r12
.LBB26_16:                              # %while.end
                                        #   in Loop: Header=BB26_10 Depth=2
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB26_10
	jmp	.LBB26_28
	.align	16, 0x90
.LBB26_57:                              # %sw.default120
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	%rax, %rdi
.LBB26_33:                              # %while.cond.backedge
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	%r13, %rsi
	callq	fputc
	jmp	.LBB26_28
.LBB26_5:                               #   in Loop: Header=BB26_2 Depth=1
	leaq	87(%rsp), %rdx
	movq	%rsi, %r12
	.align	16, 0x90
.LBB26_20:                              # %while.body39
                                        #   Parent Loop BB26_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	%al, (%rdx)
	movzbl	(%r12), %eax
	movq	$56, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	movzbl	_sch_istable(%rax,%rax), %esi
	sarq	%cl, %rdi
	incq	%rdx
	incq	%r12
	andq	$4, %rsi
	cmpq	$46, %rax
	je	.LBB26_20
# BB#21:                                # %while.body39
                                        #   in Loop: Header=BB26_20 Depth=2
	testq	$65535, %rsi            # imm = 0xFFFF
	jne	.LBB26_20
.LBB26_22:                              # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$72, %rdi
	jg	.LBB26_25
# BB#23:                                # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	andq	%rbx, %rdi
	cmpq	$37, %rdi
	jne	.LBB26_56
# BB#24:                                # %sw.bb44
                                        #   in Loop: Header=BB26_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str43, %rsi
	callq	fprintf
	jmp	.LBB26_28
.LBB26_25:                              # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$99, %rdi
	jg	.LBB26_36
# BB#26:                                # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$75, %rdi
	jg	.LBB26_30
# BB#27:                                # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	andq	%rbx, %rdi
	cmpq	$73, %rdi
	je	.LBB26_28
	jmp	.LBB26_56
.LBB26_36:                              # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	addq	$-100, %rdi
	andq	%rbx, %rdi
	cmpq	$20, %rdi
	ja	.LBB26_56
# BB#37:                                # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	jmpq	*.LJTI26_0(,%rdi,8)
.LBB26_49:                              # %sw.bb87
                                        #   in Loop: Header=BB26_2 Depth=1
	movl	52(%rsp), %esi
	movb	%al, (%rdx)
	xorq	%rax, %rax
	movb	%al, 1(%rdx)
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$160, %rsi
	ja	.LBB26_51
# BB#50:                                # %vaarg.in_reg91
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	64(%rsp), %rax
	addq	%rdx, %rax
	addq	$16, %rsi
	movl	%esi, 52(%rsp)
	movq	(%rax), %rdx
	jmp	.LBB26_42
.LBB26_30:                              # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$78, %rdi
	jg	.LBB26_34
# BB#31:                                # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	andq	%rbx, %rdi
	cmpq	$76, %rdi
	jne	.LBB26_56
# BB#32:                                # %sw.bb116
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	$46, %rdi
	jmp	.LBB26_33
.LBB26_34:                              # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	addq	$-79, %rdi
	andq	%rbx, %rdi
	cmpq	$9, %rdi
	ja	.LBB26_56
# BB#35:                                # %while.end42
                                        #   in Loop: Header=BB26_2 Depth=1
	jmpq	*.LJTI26_1(,%rdi,8)
.LBB26_55:                              # %sw.bb118
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	user_label_prefix(%rip), %rdi
	movq	%r13, %rsi
	callq	fputs
	jmp	.LBB26_28
.LBB26_38:                              # %sw.bb45
                                        #   in Loop: Header=BB26_2 Depth=1
	movl	48(%rsp), %esi
	movb	%al, (%rdx)
	xorq	%rax, %rax
	movb	%al, 1(%rdx)
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rsi
	ja	.LBB26_40
# BB#39:                                # %vaarg.in_reg
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	64(%rsp), %rax
	addq	%rdx, %rax
	addq	$8, %rsi
	movl	%esi, 48(%rsp)
	jmp	.LBB26_41
.LBB26_40:                              # %vaarg.in_mem
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	56(%rsp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 56(%rsp)
.LBB26_41:                              # %vaarg.end
                                        #   in Loop: Header=BB26_2 Depth=1
	movl	(%rax), %edx
	jmp	.LBB26_42
.LBB26_48:                              # %sw.bb69
                                        #   in Loop: Header=BB26_2 Depth=1
	movb	%al, (%rdx)
	movzbl	(%r12), %ecx
	incq	%r12
	movl	48(%rsp), %eax
	movb	%cl, 1(%rdx)
	xorq	%rcx, %rcx
	movb	%cl, 2(%rdx)
	jmp	.LBB26_44
.LBB26_53:                              # %sw.bb100
                                        #   in Loop: Header=BB26_2 Depth=1
	movl	48(%rsp), %esi
	movb	%al, (%rdx)
	xorq	%rax, %rax
	movb	%al, 1(%rdx)
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rsi
	ja	.LBB26_51
# BB#54:                                # %vaarg.in_reg107
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	64(%rsp), %rax
	addq	%rdx, %rax
	addq	$8, %rsi
	movl	%esi, 48(%rsp)
	movq	(%rax), %rdx
	jmp	.LBB26_42
.LBB26_51:                              # %vaarg.in_mem93
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	56(%rsp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 56(%rsp)
	movq	(%rax), %rdx
	jmp	.LBB26_42
.LBB26_43:                              # %sw.bb50
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	$27756, %rax            # imm = 0x6C6C
	movw	%ax, (%rdx)
	movzbl	(%r12), %ecx
	incq	%r12
	movl	48(%rsp), %eax
	movb	%cl, 2(%rdx)
	xorq	%rcx, %rcx
	movb	%cl, 3(%rdx)
.LBB26_44:                              # %sw.bb50
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB26_46
# BB#45:                                # %vaarg.in_reg60
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	64(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 48(%rsp)
	movq	(%rcx), %rdx
	jmp	.LBB26_42
.LBB26_46:                              # %vaarg.in_mem62
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
	movq	(%rcx), %rdx
.LBB26_42:                              # %while.cond.backedge
                                        #   in Loop: Header=BB26_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	leaq	86(%rsp), %rsi
	callq	fprintf
	.align	16, 0x90
.LBB26_28:                              # %while.cond.backedge
                                        #   in Loop: Header=BB26_2 Depth=1
	movzbl	(%r12), %eax
	testq	%rax, %rax
	movq	%r12, %rsi
	jne	.LBB26_2
.LBB26_29:                              # %while.end124
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_56:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.asm_fprintf, %rdx
	movq	$3648, %rsi             # imm = 0xE40
	callq	fancy_abort
.Ltmp226:
	.size	asm_fprintf, .Ltmp226-asm_fprintf
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI26_0:
	.quad	.LBB26_38
	.quad	.LBB26_49
	.quad	.LBB26_49
	.quad	.LBB26_49
	.quad	.LBB26_56
	.quad	.LBB26_38
	.quad	.LBB26_56
	.quad	.LBB26_56
	.quad	.LBB26_48
	.quad	.LBB26_56
	.quad	.LBB26_56
	.quad	.LBB26_38
	.quad	.LBB26_38
	.quad	.LBB26_56
	.quad	.LBB26_56
	.quad	.LBB26_53
	.quad	.LBB26_56
	.quad	.LBB26_38
	.quad	.LBB26_56
	.quad	.LBB26_43
	.quad	.LBB26_38
.LJTI26_1:
	.quad	.LBB26_28
	.quad	.LBB26_56
	.quad	.LBB26_56
	.quad	.LBB26_28
	.quad	.LBB26_56
	.quad	.LBB26_56
	.quad	.LBB26_55
	.quad	.LBB26_56
	.quad	.LBB26_56
	.quad	.LBB26_38

	.text
	.globl	split_double
	.align	16, 0x90
	.type	split_double,@function
split_double:                           # @split_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp231:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp232:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp233:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp234:
	.cfi_def_cfa_offset 80
.Ltmp235:
	.cfi_offset %rbx, -32
.Ltmp236:
	.cfi_offset %r14, -24
.Ltmp237:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$55, %rcx
	jne	.LBB27_1
# BB#13:                                # %if.else39
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	je	.LBB27_15
# BB#14:                                # %lor.lhs.false
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$1, %rax
	jne	.LBB27_16
.LBB27_15:                              # %if.then48
	movq	16(%rbx), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, (%r15)
	movq	24(%rbx), %rsi
	jmp	.LBB27_17
.LBB27_1:                               # %entry
	cmpq	$54, %rcx
	jne	.LBB27_12
# BB#2:                                 # %if.then
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	leaq	64(%rax,%rax), %rcx
	movabsq	$4294967232, %rdx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdx
	cmpq	$64, %rdx
	ja	.LBB27_8
# BB#3:                                 # %if.then2
	orq	$31, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	8(%rbx), %rdi
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	leaq	(%rbx,%rbx), %rax
	leaq	-1(%rbx,%rbx), %r8
	movq	%rdi, %rcx
	andq	%r8, %rcx
	negq	%rax
	movq	%rcx, %rsi
	andq	%rbx, %rsi
	je	.LBB27_5
# BB#4:                                 # %if.then2
	movq	%rax, %rsi
.LBB27_5:                               # %if.then2
	orq	%rcx, %rsi
	movq	%rdx, %rcx
	shrq	%cl, %rdi
	movq	$1, %rcx
	shrq	%cl, %rdi
	andq	%r8, %rdi
	andq	%rdi, %rbx
	je	.LBB27_7
# BB#6:                                 # %if.then2
	movq	%rax, %rbx
.LBB27_7:                               # %if.then2
	orq	%rdi, %rbx
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, (%r15)
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	jmp	.LBB27_18
.LBB27_12:                              # %if.then38
	movq	%rbx, (%r15)
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB27_19
.LBB27_16:                              # %do.body
	movq	32(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	16(%rbx), %rax
	movq	24(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etardouble
	movq	32(%rsp), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, (%r15)
	movq	40(%rsp), %rsi
.LBB27_17:                              # %if.end67
	xorq	%rdi, %rdi
.LBB27_18:                              # %if.end67
	callq	gen_rtx_CONST_INT
.LBB27_19:                              # %if.end67
	movq	%rax, (%r14)
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB27_8:                               # %if.else
	cmpq	$0, 8(%rbx)
	js	.LBB27_9
# BB#10:                                # %select.mid
	movq	const_int_rtx+512(%rip), %rax
	movq	%rbx, (%r15)
	jmp	.LBB27_19
.LBB27_9:
	movq	const_int_rtx+504(%rip), %rax
	movq	%rbx, (%r15)
	jmp	.LBB27_19
.Ltmp238:
	.size	split_double, .Ltmp238-split_double
	.cfi_endproc

	.globl	leaf_function_p
	.align	16, 0x90
	.type	leaf_function_p,@function
leaf_function_p:                        # @leaf_function_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 16
.Ltmp242:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
	movl	profile_arc_flag(%rip), %ecx
	andq	$16384, %rax            # imm = 0x4000
	orq	%rax, %rcx
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB28_16
# BB#1:                                 # %if.end
	callq	get_insns
	jmp	.LBB28_2
	.align	16, 0x90
.LBB28_7:                               # %for.inc
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	24(%rax), %rax
.LBB28_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB28_8
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB28_2 Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$16842751, %rdx         # imm = 0x100FFFF
	cmpq	$34, %rdx
	je	.LBB28_16
# BB#4:                                 # %if.end10
                                        #   in Loop: Header=BB28_2 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$32, %rcx
	jne	.LBB28_7
# BB#5:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	32(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB28_7
# BB#6:                                 # %land.lhs.true18
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	(%rcx), %ecx
	andq	$16842751, %rcx         # imm = 0x100FFFF
	cmpq	$34, %rcx
	jne	.LBB28_7
	jmp	.LBB28_16
.LBB28_8:                               # %for.end
	movq	cfun(%rip), %rax
	movq	416(%rax), %rax
	jmp	.LBB28_9
	.align	16, 0x90
.LBB28_14:                              # %for.inc99
                                        #   in Loop: Header=BB28_9 Depth=1
	movq	16(%rax), %rax
.LBB28_9:                               # %for.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB28_15
# BB#10:                                # %for.body48
                                        #   in Loop: Header=BB28_9 Depth=1
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$16842751, %rsi         # imm = 0x100FFFF
	cmpq	$34, %rsi
	je	.LBB28_16
# BB#11:                                # %if.end61
                                        #   in Loop: Header=BB28_9 Depth=1
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$32, %rdx
	jne	.LBB28_14
# BB#12:                                # %land.lhs.true65
                                        #   in Loop: Header=BB28_9 Depth=1
	movq	32(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB28_14
# BB#13:                                # %land.lhs.true72
                                        #   in Loop: Header=BB28_9 Depth=1
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	(%rcx), %ecx
	andq	$16842751, %rcx         # imm = 0x100FFFF
	cmpq	$34, %rcx
	jne	.LBB28_14
	jmp	.LBB28_16
.LBB28_15:
	movq	$1, %rbx
.LBB28_16:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp243:
	.size	leaf_function_p, .Ltmp243-leaf_function_p
	.cfi_endproc

	.globl	final_forward_branch_p
	.align	16, 0x90
	.type	final_forward_branch_p,@function
final_forward_branch_p:                 # @final_forward_branch_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp245:
	.cfi_def_cfa_offset 16
	movq	uid_shuid(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB29_1
# BB#3:                                 # %if.end
	movslq	8(%rdi), %rax
	movl	(%rcx,%rax,4), %eax
	testq	%rax, %rax
	je	.LBB29_5
# BB#4:                                 # %if.end
	movq	64(%rdi), %rdx
	movslq	8(%rdx), %rdx
	movl	(%rcx,%rdx,4), %edx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	je	.LBB29_5
# BB#6:                                 # %if.end12
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jge	.LBB29_7
# BB#8:                                 # %if.end12
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB29_7:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB29_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_forward_branch_p, %rcx
	movq	$3878, %rsi             # imm = 0xF26
	jmp	.LBB29_2
.LBB29_5:                               # %if.then11
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.final_forward_branch_p, %rcx
	movq	$3883, %rsi             # imm = 0xF2B
.LBB29_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp246:
	.size	final_forward_branch_p, .Ltmp246-final_forward_branch_p
	.cfi_endproc

	.align	16, 0x90
	.type	get_mem_expr_from_op,@function
get_mem_expr_from_op:                   # @get_mem_expr_from_op
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp249:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp250:
	.cfi_def_cfa_offset 32
.Ltmp251:
	.cfi_offset %rbx, -16
	xorq	%rax, %rax
	testq	%rdi, %rdi
	movl	%eax, (%rsi)
	je	.LBB30_16
# BB#1:                                 # %if.end
	movzbl	(%rdi), %ecx
	cmpq	$66, %rcx
	jne	.LBB30_2
# BB#5:                                 # %if.end13
	movq	16(%rdi), %rax
	testq	%rax, %rax
	je	.LBB30_7
# BB#6:                                 # %cond.false
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB30_16
.LBB30_7:                               # %if.end35
	movq	$1, %rax
	movl	%eax, (%rsi)
	movq	8(%rdi), %rbx
	leaq	12(%rsp), %rsi
	movq	%rbx, %rdi
	callq	get_mem_expr_from_op
	testq	%rax, %rax
	je	.LBB30_9
# BB#8:                                 # %if.end35
	movl	12(%rsp), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB30_16
.LBB30_9:                               # %if.else41
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB30_12
# BB#10:                                # %land.lhs.true45
	movq	16(%rbx), %rdi
	leaq	12(%rsp), %rsi
	callq	get_mem_expr_from_op
	testq	%rax, %rax
	jne	.LBB30_16
# BB#11:                                # %land.lhs.true45.while.cond.preheader_crit_edge
	movl	(%rbx), %eax
.LBB30_12:                              # %while.cond.preheader
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	addq	$-49, %rax
	andq	$255, %rax
	cmpq	$1, %rax
	ja	.LBB30_14
	.align	16, 0x90
.LBB30_13:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	addq	$-49, %rax
	andq	$255, %rax
	cmpq	$2, %rax
	jb	.LBB30_13
.LBB30_14:                              # %while.end
	leaq	12(%rsp), %rsi
	movq	%rbx, %rdi
	callq	get_mem_expr_from_op
	movl	12(%rsp), %edx
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jne	.LBB30_16
# BB#15:                                # %select.mid
	movq	%rcx, %rax
	jmp	.LBB30_16
.LBB30_2:                               # %if.end
	cmpq	$61, %rcx
	jne	.LBB30_16
# BB#3:                                 # %land.lhs.true
	movl	16(%rdi), %ecx
	cmpq	$53, %rcx
	jb	.LBB30_16
# BB#4:                                 # %if.then3
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	72(%rax), %rax
	movq	(%rax,%rcx,8), %rax
.LBB30_16:                              # %return
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp252:
	.size	get_mem_expr_from_op, .Ltmp252-get_mem_expr_from_op
	.cfi_endproc

	.type	app_on,@object          # @app_on
	.local	app_on
	.comm	app_on,1,1
	.type	final_sequence,@object  # @final_sequence
	.comm	final_sequence,8,8
	.type	dialect_number,@object  # @dialect_number
	.local	dialect_number
	.comm	dialect_number,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"final.c"
	.size	.L.str, 8

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\t.align %d\n"
	.size	.L.str1, 12

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	".%s%u:\n"
	.size	.L.str2, 8

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"LPBX"
	.size	.L.str3, 5

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"*.%s%u"
	.size	.L.str4, 7

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s"
	.size	.L.str7, 3

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\t.local\t"
	.size	.L.str8, 9

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"\t.comm\t"
	.size	.L.str10, 8

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	",%u,%u\n"
	.size	.L.str11, 8

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"/APP\n"
	.size	.L.str12, 6

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"/NO_APP\n"
	.size	.L.str13, 9

	.type	uid_shuid,@object       # @uid_shuid
	.local	uid_shuid
	.comm	uid_shuid,8,8
	.type	insn_lengths,@object    # @insn_lengths
	.local	insn_lengths
	.comm	insn_lengths,8,8
	.type	insn_lengths_max_uid,@object # @insn_lengths_max_uid
	.local	insn_lengths_max_uid
	.comm	insn_lengths_max_uid,4,4
	.type	insn_addresses_,@object # @insn_addresses_
	.comm	insn_addresses_,8,8
	.type	uid_align,@object       # @uid_align
	.local	uid_align
	.comm	uid_align,8,8
	.type	min_labelno,@object     # @min_labelno
	.local	min_labelno
	.comm	min_labelno,4,4
	.type	label_align,@object     # @label_align
	.local	label_align
	.comm	label_align,8,8
	.type	insn_current_address,@object # @insn_current_address
	.comm	insn_current_address,4,4
	.type	insn_last_address,@object # @insn_last_address
	.comm	insn_last_address,4,4
	.type	max_labelno,@object     # @max_labelno
	.local	max_labelno
	.comm	max_labelno,4,4
	.type	.L__FUNCTION__.shorten_branches,@object # @__FUNCTION__.shorten_branches
.L__FUNCTION__.shorten_branches:
	.asciz	"shorten_branches"
	.size	.L__FUNCTION__.shorten_branches, 17

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"insn_addresses"
	.size	.L.str14, 15

	.type	insn_current_align,@object # @insn_current_align
	.comm	insn_current_align,4,4
	.type	block_depth,@object     # @block_depth
	.local	block_depth
	.comm	block_depth,4,4
	.type	this_is_asm_operands,@object # @this_is_asm_operands
	.local	this_is_asm_operands
	.comm	this_is_asm_operands,8,8
	.type	last_linenum,@object    # @last_linenum
	.local	last_linenum
	.comm	last_linenum,4,4
	.type	high_function_linenum,@object # @high_function_linenum
	.local	high_function_linenum
	.comm	high_function_linenum,4,4
	.type	high_block_linenum,@object # @high_block_linenum
	.local	high_block_linenum
	.comm	high_block_linenum,4,4
	.type	last_filename,@object   # @last_filename
	.local	last_filename
	.comm	last_filename,8,8
	.type	line_note_exists,@object # @line_note_exists
	.local	line_note_exists
	.comm	line_note_exists,8,8
	.type	.L__FUNCTION__.get_insn_template,@object # @__FUNCTION__.get_insn_template
.L__FUNCTION__.get_insn_template:
	.asciz	"get_insn_template"
	.size	.L__FUNCTION__.get_insn_template, 18

	.type	insn_counter,@object    # @insn_counter
	.local	insn_counter
	.comm	insn_counter,4,4
	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"\t%s basic block %d\n"
	.size	.L.str15, 20

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"#"
	.size	.L.str16, 2

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"LEHB"
	.size	.L.str17, 5

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"LEHE"
	.size	.L.str18, 5

	.type	.L__FUNCTION__.final_scan_insn,@object # @__FUNCTION__.final_scan_insn
.L__FUNCTION__.final_scan_insn:
	.asciz	"final_scan_insn"
	.size	.L__FUNCTION__.final_scan_insn, 16

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"L"
	.size	.L.str19, 2

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"\t.p2align %d\n"
	.size	.L.str20, 14

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"\t.p2align %d,,%d\n"
	.size	.L.str21, 18

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	":\n"
	.size	.L.str22, 3

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"\t%s\n"
	.size	.L.str23, 5

	.type	insn_noperands,@object  # @insn_noperands
	.local	insn_noperands
	.comm	insn_noperands,4,4
	.type	.L.str24,@object        # @.str24
.L.str24:
	.zero	1
	.size	.L.str24, 1

	.type	debug_insn,@object      # @debug_insn
	.local	debug_insn
	.comm	debug_insn,8,8
	.type	current_output_insn,@object # @current_output_insn
	.comm	current_output_insn,8,8
	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"could not split insn"
	.size	.L.str25, 21

	.type	.L__FUNCTION__.alter_subreg,@object # @__FUNCTION__.alter_subreg
.L__FUNCTION__.alter_subreg:
	.asciz	"alter_subreg"
	.size	.L__FUNCTION__.alter_subreg, 13

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"invalid `asm': "
	.size	.L.str26, 16

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"output_operand: "
	.size	.L.str27, 17

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"%s%s"
	.size	.L.str28, 5

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"nested assembly dialect alternatives"
	.size	.L.str29, 37

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"unterminated assembly dialect alternative"
	.size	.L.str30, 42

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"%d"
	.size	.L.str31, 3

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"operand number missing after %%-letter"
	.size	.L.str32, 39

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"operand number out of range"
	.size	.L.str33, 28

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"%lld"
	.size	.L.str34, 5

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"invalid %%-code"
	.size	.L.str35, 16

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"`%%l' operand isn't a label"
	.size	.L.str36, 28

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"0x%llx%016llx"
	.size	.L.str37, 14

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"0x%llx"
	.size	.L.str38, 7

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"floating constant misused"
	.size	.L.str39, 26

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"invalid expression as operand"
	.size	.L.str42, 30

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"%%"
	.size	.L.str43, 3

	.type	.L__FUNCTION__.asm_fprintf,@object # @__FUNCTION__.asm_fprintf
.L__FUNCTION__.asm_fprintf:
	.asciz	"asm_fprintf"
	.size	.L__FUNCTION__.asm_fprintf, 12

	.type	.L__FUNCTION__.final_forward_branch_p,@object # @__FUNCTION__.final_forward_branch_p
.L__FUNCTION__.final_forward_branch_p:
	.asciz	"final_forward_branch_p"
	.size	.L__FUNCTION__.final_forward_branch_p, 23

	.type	regs_ever_live,@object  # @regs_ever_live
	.comm	regs_ever_live,53,16
	.type	frame_pointer_needed,@object # @frame_pointer_needed
	.comm	frame_pointer_needed,4,4
	.type	.L__FUNCTION__.output_operand,@object # @__FUNCTION__.output_operand
.L__FUNCTION__.output_operand:
	.asciz	"output_operand"
	.size	.L__FUNCTION__.output_operand, 15

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"\t%s %d\t%s"
	.size	.L.str45, 10

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"/%d"
	.size	.L.str46, 4

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"\t[length = %d]"
	.size	.L.str47, 15

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"%c%s %s"
	.size	.L.str48, 8

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"*"
	.size	.L.str49, 2

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"\tpush{l}\t%%e%s\n"
	.size	.L.str50, 16

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"\tcall\t*mcount@PLT\n"
	.size	.L.str51, 19

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"\tcall\t*mcount@GOT(%%ebx)\n"
	.size	.L.str52, 26

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"\tcall\tmcount\n"
	.size	.L.str53, 14

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"\tpop{l}\t%%e%s\n"
	.size	.L.str54, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
