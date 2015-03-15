	.file	"dbxout.c.bc"
	.text
	.align	16, 0x90
	.type	dbxout_init,@function
dbxout_init:                            # @dbxout_init
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
	movq	%rdi, %r15
	callq	getdecls
	movq	asm_out_file(%rip), %rcx
	movq	%rax, %r14
	movq	%rcx, asmfile(%rip)
	movq	$100, %rax
	movl	%eax, typevec_len(%rip)
	movq	$100, %rdi
	movq	$12, %rsi
	callq	xcalloc
	movq	%rax, typevec(%rip)
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str71, %rdx
	leaq	(%rsp), %rdi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	xorq	%r12, %r12
	callq	sprintf
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_8
# BB#1:                                 # %if.then
	cmpq	$0, cwd(%rip)
	jne	.LBB0_7
# BB#2:                                 # %land.lhs.true
	callq	getpwd
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, cwd(%rip)
	je	.LBB0_8
# BB#3:                                 # %land.lhs.true6
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_5
# BB#4:                                 # %lor.lhs.false
	movq	%rbx, %rdi
	callq	strlen
	movzbl	-1(%rax,%rbx), %eax
	cmpq	$47, %rax
	je	.LBB0_6
.LBB0_5:                                # %if.then11
	movabsq	$.L.str75, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	concat
	movq	%rax, %rbx
	movq	%rbx, cwd(%rip)
.LBB0_6:                                # %if.end
	testq	%rbx, %rbx
	je	.LBB0_8
.LBB0_7:                                # %if.then14
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str1, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asmfile(%rip), %rdi
	movq	cwd(%rip), %rsi
	callq	output_quoted_string
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str72, %rsi
	movq	$100, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asmfile(%rip), %rdi
	leaq	(%rsp), %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB0_8:                                # %if.end20
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str1, %r13
	movq	$8, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%r13, %rdi
	callq	fwrite
	movq	asmfile(%rip), %rdi
	movq	%r15, %rsi
	callq	output_quoted_string
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str72, %rsi
	movq	$100, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asmfile(%rip), %rdi
	leaq	(%rsp), %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	callq	text_section
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str61, %rsi
	movabsq	$.L.str71, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str76, %rsi
	movabsq	$.L.str77, %rcx
	movq	$60, %r8
	xorq	%rax, %rax
	movq	%r13, %rdx
	callq	fprintf
	movq	%r15, lastfile(%rip)
	movl	%ebx, next_type_number(%rip)
	movq	$16, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movl	%r12d, 8(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rax, current_file(%rip)
	movl	%ebx, 12(%rax)
	movl	%ebx, next_file_number(%rip)
	movq	96(%rcx), %rdi
	callq	dbxout_symbol
	movq	integer_types(%rip), %rax
	movq	96(%rax), %rdi
	callq	dbxout_symbol
	movq	%r14, %rdi
	callq	dbxout_typedefs
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	dbxout_init, .Ltmp17-dbxout_init
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_finish,@function
dbxout_finish:                          # @dbxout_finish
	.cfi_startproc
# BB#0:                                 # %entry
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str58, %rsi
	movq	$100, %rdx
	xorq	%rax, %rax
	jmp	asm_fprintf  # TAILCALL
.Ltmp18:
	.size	dbxout_finish, .Ltmp18-dbxout_finish
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_start_source_file,@function
dbxout_start_source_file:               # @dbxout_start_source_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 16
.Ltmp22:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	$16, %rdi
	callq	xmalloc
	movq	current_file(%rip), %rcx
	movl	next_file_number(%rip), %edx
	movq	%rcx, (%rax)
	leaq	1(%rdx), %rcx
	movl	%ecx, next_file_number(%rip)
	movl	%edx, 8(%rax)
	movq	asmfile(%rip), %rcx
	movq	$1, %rdx
	movl	%edx, 12(%rax)
	movq	%rax, current_file(%rip)
	movabsq	$.L.str1, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	output_quoted_string
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str74, %rsi
	movq	$130, %rdx
	xorq	%rax, %rax
	popq	%rbx
	jmp	fprintf  # TAILCALL
.Ltmp23:
	.size	dbxout_start_source_file, .Ltmp23-dbxout_start_source_file
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_end_source_file,@function
dbxout_end_source_file:                 # @dbxout_end_source_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 16
.Ltmp27:
	.cfi_offset %rbx, -16
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str73, %rsi
	movabsq	$.L.str67, %rdx
	movq	$162, %rcx
	xorq	%rax, %rax
	callq	fprintf
	movq	current_file(%rip), %rdi
	movq	(%rdi), %rbx
	callq	free
	movq	%rbx, current_file(%rip)
	popq	%rbx
	retq
.Ltmp28:
	.size	dbxout_end_source_file, .Ltmp28-dbxout_end_source_file
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_begin_block,@function
dbxout_begin_block:                     # @dbxout_begin_block
	.cfi_startproc
# BB#0:                                 # %entry
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str61, %r8
	movabsq	$.L.str64, %rdx
	xorq	%rax, %rax
	movq	%rsi, %rcx
	movq	%r8, %rsi
	jmp	fprintf  # TAILCALL
.Ltmp29:
	.size	dbxout_begin_block, .Ltmp29-dbxout_begin_block
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_end_block,@function
dbxout_end_block:                       # @dbxout_end_block
	.cfi_startproc
# BB#0:                                 # %entry
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str61, %r8
	movabsq	$.L.str68, %rdx
	xorq	%rax, %rax
	movq	%rsi, %rcx
	movq	%r8, %rsi
	jmp	fprintf  # TAILCALL
.Ltmp30:
	.size	dbxout_end_block, .Ltmp30-dbxout_end_block
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_source_line,@function
dbxout_source_line:                     # @dbxout_source_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 40
	subq	$264, %rsp              # imm = 0x108
.Ltmp40:
	.cfi_def_cfa_offset 304
.Ltmp41:
	.cfi_offset %rbx, -40
.Ltmp42:
	.cfi_offset %r12, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rbx, %rbx
	je	.LBB6_7
# BB#1:                                 # %land.lhs.true.i
	movq	lastfile(%rip), %rsi
	movq	asmfile(%rip), %r15
	testq	%rsi, %rsi
	je	.LBB6_3
# BB#2:                                 # %lor.lhs.false.i
	movq	%rbx, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_7
.LBB6_3:                                # %if.then.i
	movl	source_label_number(%rip), %ecx
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str71, %rdx
	leaq	(%rsp), %r12
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	sprintf
	movabsq	$.L.str1, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	output_quoted_string
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str72, %rsi
	movq	$132, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%r12, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	je	.LBB6_5
# BB#4:                                 # %land.lhs.true8.i
	cmpq	$0, 128(%rax)
	jne	.LBB6_6
.LBB6_5:                                # %if.else.i
	callq	text_section
.LBB6_6:                                # %do.body11.i
	movl	source_label_number(%rip), %ecx
	movabsq	$.L.str61, %rsi
	movabsq	$.L.str71, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	movl	source_label_number(%rip), %eax
	incq	%rax
	movl	%eax, source_label_number(%rip)
	movq	%rbx, lastfile(%rip)
.LBB6_7:                                # %dbxout_source_file.exit
	movl	dbxout_source_line.sym_lineno(%rip), %ecx
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str69, %rdx
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str70, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$45, %rdi
	callq	putc
	movq	current_function_decl(%rip), %rdi
	movq	144(%rdi), %rax
	movq	asmfile(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB6_9
# BB#8:                                 # %cond.false
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	144(%rax), %rax
.LBB6_9:                                # %cond.end
	movq	8(%rax), %rax
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	putc
	movq	asmfile(%rip), %rdi
	movl	dbxout_source_line.sym_lineno(%rip), %ecx
	movabsq	$.L.str61, %rsi
	movabsq	$.L.str69, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movl	dbxout_source_line.sym_lineno(%rip), %eax
	incq	%rax
	movl	%eax, dbxout_source_line.sym_lineno(%rip)
	addq	$264, %rsp              # imm = 0x108
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp45:
	.size	dbxout_source_line, .Ltmp45-dbxout_source_line
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_begin_function,@function
dbxout_begin_function:                  # @dbxout_begin_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 16
.Ltmp49:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	dbxout_symbol
	movq	88(%rbx), %rdi
	callq	dbxout_parms
	movq	96(%rbx), %rdi
	cmpq	$0, 72(%rdi)
	je	.LBB7_1
# BB#2:                                 # %if.then
	popq	%rbx
	jmp	dbxout_symbol  # TAILCALL
.LBB7_1:                                # %if.end
	popq	%rbx
	retq
.Ltmp50:
	.size	dbxout_begin_function, .Ltmp50-dbxout_begin_function
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_function_decl,@function
dbxout_function_decl:                   # @dbxout_function_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp55:
	.cfi_def_cfa_offset 24
	subq	$104, %rsp
.Ltmp56:
	.cfi_def_cfa_offset 128
.Ltmp57:
	.cfi_offset %rbx, -24
.Ltmp58:
	.cfi_offset %r14, -16
	movq	88(%rdi), %rdx
	movq	104(%rdi), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_block
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_5
# BB#1:                                 # %entry
	movzbl	targetm+328(%rip), %eax
	testq	$255, %rax
	je	.LBB8_5
# BB#2:                                 # %if.then
	movl	dbxout_function_end.scope_labelno(%rip), %ecx
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str60, %rbx
	leaq	(%rsp), %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	sprintf
	movq	asmfile(%rip), %rdi
	movl	dbxout_function_end.scope_labelno(%rip), %ecx
	movabsq	$.L.str61, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	dbxout_function_end.scope_labelno(%rip), %eax
	movq	asmfile(%rip), %rdi
	incq	%rax
	movl	%eax, dbxout_function_end.scope_labelno(%rip)
	movabsq	$.L.str62, %rsi
	movabsq	$.L.str1, %rdx
	movq	$36, %rcx
	xorq	%rax, %rax
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%r14, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$45, %rdi
	callq	putc
	movq	current_function_decl(%rip), %rdi
	movq	144(%rdi), %rax
	movq	asmfile(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB8_4
# BB#3:                                 # %cond.false.i
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	144(%rax), %rax
.LBB8_4:                                # %dbxout_function_end.exit
	movq	8(%rax), %rax
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
.LBB8_5:                                # %if.end
	addq	$104, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp59:
	.size	dbxout_function_decl, .Ltmp59-dbxout_function_decl
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_global_decl,@function
dbxout_global_decl:                     # @dbxout_global_decl
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	cmpq	$34, %rax
	jne	.LBB9_3
# BB#1:                                 # %land.lhs.true
	movzbl	49(%rdi), %eax
	testq	$1, %rax
	jne	.LBB9_3
# BB#2:                                 # %land.lhs.true4
	cmpq	$0, 144(%rdi)
	je	.LBB9_3
# BB#4:                                 # %if.then
	jmp	dbxout_symbol  # TAILCALL
.LBB9_3:                                # %if.end
	retq
.Ltmp60:
	.size	dbxout_global_decl, .Ltmp60-dbxout_global_decl
	.cfi_endproc

	.globl	dbxout_symbol
	.align	16, 0x90
	.type	dbxout_symbol,@function
dbxout_symbol:                          # @dbxout_symbol
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp66:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp67:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp68:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp69:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp70:
	.cfi_def_cfa_offset 48
.Ltmp71:
	.cfi_offset %rbx, -40
.Ltmp72:
	.cfi_offset %r12, -32
.Ltmp73:
	.cfi_offset %r14, -24
.Ltmp74:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %r15
	xorq	%rax, %rax
	movl	%eax, current_sym_code(%rip)
	movl	%eax, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	movq	72(%r14), %rcx
	testq	%rcx, %rcx
	jne	.LBB10_3
# BB#1:                                 # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB10_2
.LBB10_3:                               # %lor.lhs.false
	movq	48(%r14), %rax
	testq	$16384, %rax            # imm = 0x4000
	je	.LBB10_5
# BB#4:
	xorq	%rbx, %rbx
	jmp	.LBB10_105
.LBB10_5:                               # %if.end
	testq	%rcx, %rcx
	je	.LBB10_7
# BB#6:                                 # %if.then11
	movl	24(%rcx), %edx
	addq	$2, %rdx
	movl	%edx, current_sym_nchars(%rip)
.LBB10_7:                               # %if.end14
	movl	16(%r14), %edx
	movq	%rdx, %rsi
	andq	$255, %rsi
	addq	$-30, %rsi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rsi
	xorq	%rbx, %rbx
	cmpq	$6, %rsi
	ja	.LBB10_105
# BB#8:                                 # %if.end14
	jmpq	*.LJTI10_0(,%rsi,8)
.LBB10_85:                              # %sw.bb335
	cmpq	$0, 144(%r14)
	je	.LBB10_86
# BB#87:                                # %sw.bb335
	andq	$256, %rax              # imm = 0x100
	movabsq	$0, %rbx
	jne	.LBB10_105
# BB#88:                                # %if.end349
	andq	$266240, %rdx           # imm = 0x41000
	cmpq	$266240, %rdx           # imm = 0x41000
	jne	.LBB10_95
# BB#89:                                # %land.lhs.true361
	movq	104(%r14), %rax
	testq	%rax, %rax
	je	.LBB10_95
# BB#90:                                # %land.lhs.true364
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB10_95
# BB#91:                                # %land.lhs.true369
	movl	16(%r14), %eax
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB10_95
# BB#92:                                # %land.lhs.true376
	movq	80(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB10_100
# BB#93:                                # %lor.lhs.false380
	movzbl	16(%rcx), %ecx
	cmpq	$4, %rcx
	jne	.LBB10_95
# BB#94:                                # %lor.lhs.false380
	andq	$524288, %rax           # imm = 0x80000
	testq	%r12, %rax
	je	.LBB10_101
	jmp	.LBB10_95
.LBB10_2:
	xorq	%rbx, %rbx
.LBB10_105:                             # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB10_9:                               # %sw.bb
	cmpq	$0, 144(%r14)
	jne	.LBB10_12
# BB#10:                                # %cond.end
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	make_decl_rtl
	cmpq	$0, 144(%r14)
	je	.LBB10_105
# BB#11:                                # %cond.end.if.end27_crit_edge
	movq	48(%r14), %rax
.LBB10_12:                              # %if.end27
	testq	$256, %rax              # imm = 0x100
	je	.LBB10_14
# BB#13:
	xorq	%rbx, %rbx
	jmp	.LBB10_105
.LBB10_40:                              # %sw.bb127
	testq	$16384, %rdx            # imm = 0x4000
	jne	.LBB10_41
# BB#42:                                # %sw.bb127
	movq	%rax, %rdx
	andq	$256, %rdx              # imm = 0x100
	movabsq	$0, %rbx
	jne	.LBB10_105
# BB#43:                                # %if.end142
	testq	%rcx, %rcx
	je	.LBB10_70
# BB#44:                                # %if.then146
	movzbl	16(%r15), %ecx
	addq	$-20, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	ja	.LBB10_58
# BB#45:                                # %land.lhs.true164
	movq	96(%r15), %rcx
	cmpq	%r14, %rcx
	jne	.LBB10_58
# BB#46:                                # %land.lhs.true168
	movl	use_gnu_debug_info_extensions(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB10_47
# BB#48:                                # %land.lhs.true168
	movabsq	$1, %rcx
	jmp	.LBB10_49
.LBB10_84:                              # %sw.bb334
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.dbxout_symbol, %rcx
	movq	$2058, %rsi             # imm = 0x80A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB10_86:
	xorq	%rbx, %rbx
	jmp	.LBB10_105
.LBB10_100:                             # %if.then388
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB10_101
.LBB10_95:                              # %if.end436
	movq	144(%r14), %rax
	testq	%rax, %rax
	jne	.LBB10_97
# BB#96:                                # %cond.false443
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB10_97:                              # %cond.end446
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	eliminate_regs
	testq	%rax, %rax
	movq	%rax, 144(%r14)
	jne	.LBB10_99
# BB#98:                                # %cond.false457
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB10_99:                              # %cond.end460
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	dbxout_symbol_location  # TAILCALL
.LBB10_14:                              # %if.end35
	movq	%r14, %rdi
	callq	decl_function_context
	movq	current_function_decl(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	je	.LBB10_15
# BB#16:                                # %if.end38
	movq	144(%r14), %rax
	testq	%rax, %rax
	jne	.LBB10_18
# BB#17:                                # %cond.false45
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB10_18:                              # %cond.end48
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB10_19
# BB#20:                                # %lor.lhs.false53
	testq	%rax, %rax
	jne	.LBB10_22
# BB#21:                                # %cond.false60
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB10_22:                              # %cond.end63
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$68, %rax
	movabsq	$0, %rbx
	jne	.LBB10_105
# BB#23:                                # %if.end69
	movq	120(%r14), %rax
	movq	asmfile(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB10_25
# BB#24:                                # %cond.false73
	movq	%r14, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r14), %rax
.LBB10_25:                              # %cond.end74
	movl	16(%r14), %r8d
	movq	32(%rax), %r9
	movq	$14, %rcx
	shrq	%cl, %r8
	andq	$32, %r8
	xorq	$102, %r8
	movabsq	$.L.str, %rcx
	movabsq	$.L.str1, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	%r9, %rcx
	callq	fprintf
	movq	$36, %rax
	movl	%eax, current_sym_code(%rip)
	movq	144(%r14), %rax
	testq	%rax, %rax
	jne	.LBB10_27
# BB#26:                                # %cond.false92
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rax
.LBB10_27:                              # %cond.end95
	movq	8(%rax), %rax
	movq	%rax, current_sym_addr(%rip)
	movq	8(%r15), %rax
	testq	%rax, %rax
	jne	.LBB10_29
# BB#28:                                # %if.else
	movq	global_trees+216(%rip), %rax
.LBB10_29:                              # %if.end106
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	dbxout_type
	testq	%r12, %r12
	je	.LBB10_33
# BB#30:                                # %if.then108
	movq	120(%r14), %rax
	movq	asmfile(%rip), %rbx
	testq	%rax, %rax
	jne	.LBB10_32
# BB#31:                                # %cond.false113
	movq	%r14, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r14), %rax
.LBB10_32:                              # %cond.end114
	movq	72(%r12), %rcx
	movq	32(%rax), %rdx
	movq	32(%rcx), %rcx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
.LBB10_33:                              # %if.end126
	movl	use_gnu_debug_info_extensions(%rip), %eax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB10_36
# BB#34:                                # %if.end126
	testq	%r14, %r14
	je	.LBB10_36
# BB#35:                                # %if.then.i
	movl	32(%r14), %ecx
.LBB10_36:                              # %if.end.i
	movq	asmfile(%rip), %rdx
	movl	current_sym_code(%rip), %ebx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	movq	current_sym_addr(%rip), %rax
	movq	asmfile(%rip), %rcx
	testq	%rax, %rax
	je	.LBB10_38
# BB#37:                                # %if.then2.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	output_addr_const
	jmp	.LBB10_39
.LBB10_41:
	xorq	%rbx, %rbx
	jmp	.LBB10_105
.LBB10_70:                              # %land.lhs.true266
	movq	96(%r15), %rax
	testq	%rax, %rax
	je	.LBB10_78
# BB#71:                                # %land.lhs.true270
	movl	16(%rax), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	je	.LBB10_73
# BB#72:                                # %lor.lhs.false278
	cmpq	$0, 72(%rax)
	je	.LBB10_78
.LBB10_73:                              # %land.lhs.true284
	cmpq	$0, 32(%r15)
	je	.LBB10_78
# BB#74:                                # %land.lhs.true284
	andq	$16384, %rdx            # imm = 0x4000
	testq	%r12, %rdx
	jne	.LBB10_78
# BB#75:                                # %if.then297
	cmpq	$33, %rcx
	jne	.LBB10_77
# BB#76:                                # %if.then306
	movq	72(%rax), %rax
.LBB10_77:                              # %if.end309
	movq	$128, %rcx
	movl	%ecx, current_sym_code(%rip)
	xorq	%rcx, %rcx
	movl	%ecx, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	movl	24(%rax), %ecx
	addq	$2, %rcx
	movl	%ecx, current_sym_nchars(%rip)
	movq	asmfile(%rip), %rcx
	movq	32(%rax), %rbx
	movabsq	$.L.str3, %rdx
	movabsq	$.L.str1, %r8
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%r8, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	jmp	.LBB10_80
.LBB10_47:
	movabsq	$0, %rcx
.LBB10_49:                              # %land.lhs.true168
	movzbl	have_used_extensions(%rip), %edx
	testq	%rdx, %rcx
	jne	.LBB10_58
# BB#50:                                # %land.lhs.true168
	andq	$4194304, %rax          # imm = 0x400000
	je	.LBB10_58
# BB#51:                                # %land.lhs.true188
	movq	32(%r15), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB10_58
# BB#52:                                # %if.then192
	movq	96(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB10_54
# BB#53:                                # %if.then201
	movq	72(%rax), %rax
.LBB10_54:                              # %if.end204
	movq	$128, %rcx
	movl	%ecx, current_sym_code(%rip)
	xorq	%rcx, %rcx
	movl	%ecx, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	movl	24(%rax), %ecx
	addq	$2, %rcx
	movl	%ecx, current_sym_nchars(%rip)
	movq	asmfile(%rip), %rcx
	movq	32(%rax), %rbx
	movabsq	$.L.str3, %rdx
	movabsq	$.L.str1, %r8
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%r8, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	dbxout_type
	movq	asmfile(%rip), %rdx
	movl	current_sym_code(%rip), %ebx
	movabsq	$.L.str13, %rsi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	movq	current_sym_addr(%rip), %rax
	movq	asmfile(%rip), %rcx
	testq	%rax, %rax
	je	.LBB10_56
# BB#55:                                # %if.then2.i567
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	output_addr_const
	jmp	.LBB10_57
.LBB10_101:                             # %if.then395
	movq	8(%r14), %rax
	movq	72(%r14), %rcx
	movzbl	16(%rax), %eax
	movq	32(%rcx), %r15
	cmpq	$6, %rax
	je	.LBB10_103
# BB#102:                               # %if.then395
	xorq	%rbx, %rbx
	cmpq	$10, %rax
	jne	.LBB10_105
.LBB10_103:                             # %if.then417
	movq	104(%r14), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	asmfile(%rip), %rcx
	movq	%rax, %rbx
	movabsq	$.L.str7, %rdx
	movabsq	$.L.str1, %r8
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%r8, %rdx
	movq	%r15, %rcx
	callq	fprintf
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str8, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str9, %rsi
	movq	$128, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	jmp	.LBB10_104
.LBB10_15:
	xorq	%rbx, %rbx
	jmp	.LBB10_105
.LBB10_19:
	xorq	%rbx, %rbx
	jmp	.LBB10_105
.LBB10_38:                              # %if.else.i
	movl	current_sym_value(%rip), %edx
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
.LBB10_39:                              # %dbxout_finish_symbol.exit
	movq	asmfile(%rip), %rax
	movq	$10, %rdi
	movq	%rax, %rsi
	callq	putc
	jmp	.LBB10_104
.LBB10_78:                              # %land.lhs.true320
	movzbl	16(%r15), %eax
	cmpq	$10, %rax
	jne	.LBB10_83
# BB#79:                                # %if.then326
	movq	$128, %rax
	movl	%eax, current_sym_code(%rip)
	xorq	%rax, %rax
	movl	%eax, current_sym_value(%rip)
	movq	asmfile(%rip), %rcx
	movq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$0, current_sym_addr(%rip)
	movabsq	$.L.str5, %rdx
	movabsq	$.L.str1, %rbx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	fprintf
.LBB10_80:                              # %if.then326
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	dbxout_type
	movq	asmfile(%rip), %rdx
	movl	current_sym_code(%rip), %ebx
	movabsq	$.L.str13, %rsi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rbx, %rdx
	jmp	.LBB10_68
.LBB10_56:                              # %if.else.i569
	movl	current_sym_value(%rip), %edx
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
.LBB10_57:                              # %dbxout_finish_symbol.exit571
	movq	asmfile(%rip), %rax
	movq	$10, %rdi
	movq	%rax, %rsi
	callq	putc
.LBB10_58:                              # %if.end213
	movq	72(%r14), %rax
	movq	asmfile(%rip), %rcx
	movq	32(%rax), %r8
	movabsq	$.L.str4, %rdx
	movabsq	$.L.str1, %r9
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	callq	fprintf
	movzbl	16(%r15), %eax
	addq	$-20, %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	ja	.LBB10_64
# BB#59:                                # %land.lhs.true237
	movq	96(%r15), %rax
	cmpq	%r14, %rax
	jne	.LBB10_64
# BB#60:                                # %land.lhs.true241
	movzbl	50(%r14), %eax
	testq	$64, %rax
	je	.LBB10_64
# BB#61:                                # %if.then249
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_64
# BB#62:                                # %if.then249
	movzbl	have_used_extensions(%rip), %eax
	testq	$1, %rax
	je	.LBB10_64
# BB#63:                                # %if.then253
	movq	asmfile(%rip), %rax
	movq	$84, %rdi
	movq	%rax, %rsi
	callq	putc
	movq	96(%r15), %rax
	movl	16(%rax), %ecx
	orq	$16384, %rcx            # imm = 0x4000
	movl	%ecx, 16(%rax)
.LBB10_64:                              # %if.end262
	movq	asmfile(%rip), %rax
	movq	$116, %rdi
	movq	%rax, %rsi
	callq	putc
	movq	$128, %rax
	movl	%eax, current_sym_code(%rip)
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_67
# BB#65:                                # %if.end262
	testq	%r14, %r14
	je	.LBB10_67
# BB#66:                                # %if.then.i576
	movl	32(%r14), %ebx
.LBB10_67:                              # %if.end.i580
	movq	asmfile(%rip), %rcx
	movl	current_sym_code(%rip), %edx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rcx
.LBB10_68:                              # %if.end.i580
	callq	fprintf
	movq	current_sym_addr(%rip), %rax
	movq	asmfile(%rip), %rcx
	testq	%rax, %rax
	je	.LBB10_81
# BB#69:                                # %if.then2.i581
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	output_addr_const
	jmp	.LBB10_82
.LBB10_81:                              # %if.else.i603
	movl	current_sym_value(%rip), %edx
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
.LBB10_82:                              # %dbxout_finish_symbol.exit605
	movq	asmfile(%rip), %rax
	movq	$10, %rdi
	movq	%rax, %rsi
	callq	putc
.LBB10_83:                              # %if.end328
	movl	16(%r14), %eax
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%r14)
.LBB10_104:                             # %return
	movq	$1, %rbx
	jmp	.LBB10_105
.Ltmp75:
	.size	dbxout_symbol, .Ltmp75-dbxout_symbol
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI10_0:
	.quad	.LBB10_9
	.quad	.LBB10_105
	.quad	.LBB10_105
	.quad	.LBB10_40
	.quad	.LBB10_85
	.quad	.LBB10_84
	.quad	.LBB10_85

	.text
	.align	16, 0x90
	.type	dbxout_type,@function
dbxout_type:                            # @dbxout_type
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
	subq	$48, %rsp
.Ltmp87:
	.cfi_def_cfa_offset 96
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
	movzbl	16(%rdi), %eax
	movq	%rsi, %r14
	cmpq	$9, %rax
	jne	.LBB11_2
# BB#1:                                 # %if.then
	movq	24(%rdi), %rdi
.LBB11_2:                               # %if.end
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB11_7
# BB#3:                                 # %if.else
	movq	96(%rdi), %rax
	testq	%rax, %rax
	je	.LBB11_11
# BB#4:                                 # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB11_8
# BB#5:                                 # %land.lhs.true12
	movzbl	49(%rax), %ecx
	testq	$1, %rcx
	je	.LBB11_8
# BB#6:                                 # %land.lhs.true12
	xorq	%r14, %r14
	jmp	.LBB11_8
.LBB11_7:                               # %if.end20
	movq	integer_types+40(%rip), %rdi
	movq	96(%rdi), %rax
	testq	%rax, %rax
	je	.LBB11_11
.LBB11_8:                               # %land.lhs.true24
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB11_11
# BB#9:                                 # %land.lhs.true32
	cmpq	$0, 96(%rax)
	je	.LBB11_11
# BB#10:                                # %if.then37
	addq	$8, %rax
	jmp	.LBB11_12
.LBB11_11:                              # %if.else42
	leaq	128(%rdi), %rax
.LBB11_12:                              # %if.end45
	movq	(%rax), %rbx
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	movq	%rbx, %r12
	je	.LBB11_14
# BB#13:                                # %select.mid
	movq	%rdi, %r12
.LBB11_14:                              # %select.end
	movl	88(%r12), %edx
	testq	%rdx, %rdx
	je	.LBB11_16
# BB#15:                                # %if.end45.if.end72_crit_edge
	movq	typevec(%rip), %rax
	jmp	.LBB11_19
.LBB11_16:                              # %if.then51
	movl	next_type_number(%rip), %edx
	leaq	1(%rdx), %rax
	movl	%eax, next_type_number(%rip)
	movl	%edx, 88(%r12)
	movl	next_type_number(%rip), %eax
	movl	typevec_len(%rip), %ecx
	cmpq	%rcx, %rax
	jne	.LBB11_18
# BB#17:                                # %if.then56
	movq	typevec(%rip), %rdi
	movq	$33, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$2, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movslq	typevec_len(%rip), %rdx
	movq	%rax, typevec(%rip)
	movq	$2, %rcx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rdx
	leaq	(%rax,%rdx), %rdi
	xorq	%rsi, %rsi
	callq	memset
	movl	typevec_len(%rip), %eax
	addq	%rax, %rax
	movl	%eax, typevec_len(%rip)
	movl	88(%r12), %edx
.LBB11_18:                              # %if.end61
	movq	current_file(%rip), %rsi
	movl	8(%rsi), %edi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	typevec(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rcx
	movl	%edi, 4(%rax,%rcx,4)
	movl	12(%rsi), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, 12(%rsi)
	movslq	88(%r12), %rdx
	leaq	(%rdx,%rdx,2), %rdx
	movl	%ecx, 8(%rax,%rdx,4)
	movl	88(%r12), %edx
.LBB11_19:                              # %if.end72
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rdx,%rdx,2), %rdx
	movl	8(%rax,%rdx,4), %ecx
	movl	4(%rax,%rdx,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%r12), %rax
	movq	typevec(%rip), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	(%rcx,%rax,4), %eax
	cmpq	$2, %rax
	je	.LBB11_291
# BB#20:                                # %if.end72
	cmpq	$1, %rax
	jne	.LBB11_24
# BB#21:                                # %sw.bb
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r14
	je	.LBB11_291
# BB#22:                                # %lor.lhs.false
	movq	32(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB11_291
# BB#23:                                # %lor.lhs.false82
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB11_291
.LBB11_24:                              # %sw.epilog
	movq	asmfile(%rip), %rsi
	movq	$61, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%r12), %rcx
	movq	typevec(%rip), %r9
	leaq	(%rcx,%rcx,2), %rsi
	movq	$2, %rcx
	movl	%ecx, (%r9,%rsi,4)
	movl	16(%r12), %r8d
	movl	16(%rbx), %eax
	movq	$12, %rcx
	movq	%r8, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$12, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$1, %rdi
	cmpq	%rdi, %rdx
	jbe	.LBB11_27
# BB#25:                                # %if.then108
	movq	asmfile(%rip), %rsi
	movq	$107, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movl	16(%r12), %esi
	movq	$10, %rcx
	shrq	%cl, %rsi
	andq	$2, %rsi
	jmp	.LBB11_26
.LBB11_27:                              # %if.else117
	movq	$11, %rcx
	movq	%r8, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$11, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	%rax, %rdx
	jbe	.LBB11_29
# BB#28:                                # %if.then130
	movq	asmfile(%rip), %rsi
	movq	$66, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movl	16(%r12), %esi
	movq	$12, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
.LBB11_26:                              # %if.then108
	movq	%r12, %rdi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	jmp	.LBB11_118
.LBB11_29:                              # %if.else141
	movq	128(%r12), %rax
	cmpq	%rax, %rbx
	je	.LBB11_31
# BB#30:                                # %if.then146
	movq	96(%r12), %rax
	movq	96(%rax), %rdi
.LBB11_117:                             # %if.end490
	xorq	%rsi, %rsi
.LBB11_118:                             # %if.end490
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	dbxout_type  # TAILCALL
.LBB11_31:                              # %if.end153
	movq	%r8, %rcx
	andq	$255, %rcx
	addq	$-5, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$19, %rcx
	ja	.LBB11_288
# BB#32:                                # %if.end153
	jmpq	*.LJTI11_0(,%rcx,8)
.LBB11_119:                             # %sw.bb493
	movq	136(%r12), %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB11_123
# BB#120:                               # %land.lhs.true497
	movzbl	16(%rax), %ecx
	cmpq	$3, %rcx
	jne	.LBB11_123
# BB#121:                               # %land.lhs.true506
	movq	64(%rax), %rax
	testq	%rax, %rax
	je	.LBB11_123
# BB#122:                               # %if.then512
	movl	24(%rax), %r15d
.LBB11_123:                             # %if.end519
	movq	96(%r12), %rax
	testq	%rax, %rax
	je	.LBB11_127
# BB#124:                               # %land.lhs.true524
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB11_126
# BB#125:                               # %land.lhs.true533
	movzbl	49(%rax), %eax
	testq	$64, %rax
	jne	.LBB11_127
.LBB11_126:                             # %land.lhs.true542
	testq	%rbx, %r14
	je	.LBB11_129
.LBB11_127:                             # %lor.lhs.false544
	movq	32(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB11_129
# BB#128:                               # %lor.lhs.false549
	movq	$1, %rsi
	callq	host_integerp
	movl	16(%r12), %r8d
	testq	%rbx, %rax
	je	.LBB11_129
# BB#141:                               # %if.end582
	movq	%r15, 8(%rsp)           # 8-byte Spill
	andq	$255, %r8
	cmpq	$20, %r8
	jne	.LBB11_142
# BB#143:                               # %if.end582
	movabsq	$1, %rax
	jmp	.LBB11_144
.LBB11_289:                             # %sw.bb158
	movq	asmfile(%rip), %rdi
	movl	8(%r9,%rsi,4), %ecx
	movl	4(%r9,%rsi,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$9, %rax
	jmp	.LBB11_290
.LBB11_129:                             # %if.then554
	andq	$255, %r8
	cmpq	$20, %r8
	je	.LBB11_130
# BB#131:                               # %if.then554
	movabsq	$.L.str41, %rdi
	jmp	.LBB11_132
.LBB11_288:                             # %sw.default
	movabsq	$.L.str6, %rdi
	movabsq	$.L__FUNCTION__.dbxout_type, %rdx
	movq	$1720, %rsi             # imm = 0x6B8
	callq	fancy_abort
.LBB11_33:                              # %sw.bb159
	movq	integer_types(%rip), %rax
	cmpq	%rax, %r12
	jne	.LBB11_37
# BB#34:                                # %sw.bb159
	andq	$8192, %r8              # imm = 0x2000
	testq	%rbx, %r8
	jne	.LBB11_37
# BB#35:                                # %if.then168
	movq	asmfile(%rip), %rsi
	movq	$114, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%r12), %rax
	movq	typevec(%rip), %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movl	8(%rdx,%rax,4), %ecx
	movl	4(%rdx,%rax,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str22, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
	jmp	.LBB11_36
.LBB11_57:                              # %sw.bb322
	movq	asmfile(%rip), %rsi
	movq	$114, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	movq	integer_types+40(%rip), %rcx
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%rcx), %rax
	movq	typevec(%rip), %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movl	8(%rdx,%rax,4), %ecx
	movl	4(%rdx,%rax,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	%rax, %r14
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r14, %r15
	sarq	%cl, %r15
	leaq	(%r14,%r15), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	xorq	%r15, %rbx
	movq	$63, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_60
# BB#58:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_59:                              # %while.body.i1245
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_59
.LBB11_60:                              # %print_wide_int.exit1248
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	jmp	.LBB11_61
.LBB11_81:                              # %sw.bb370
	movq	8(%r12), %rax
	movzbl	16(%rax), %eax
	movq	asmfile(%rip), %rsi
	cmpq	$7, %rax
	jne	.LBB11_88
# BB#82:                                # %if.then379
	movq	$114, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%r12), %rax
	movq	typevec(%rip), %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movl	8(%rdx,%rax,4), %ecx
	movl	4(%rdx,%rax,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	8(%r12), %rdi
	callq	int_size_in_bytes
	movq	asmfile(%rip), %rdi
	movq	%rax, %r14
	leaq	(%r14,%r14), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r14, %r14
	jns	.LBB11_84
# BB#83:
	negq	%rbx
.LBB11_84:                              # %if.then379
	movq	$62, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_87
# BB#85:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_86:                              # %while.body.i1311
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_86
.LBB11_87:                              # %print_wide_int.exit1314
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	andq	$1, %r14
.LBB11_61:                              # %sw.epilog902
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str25, %rdi
	movq	$3, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$3, %rax
	jmp	.LBB11_290
.LBB11_243:                             # %sw.bb724
	movq	96(%r12), %rax
	testq	%rax, %rax
	je	.LBB11_247
# BB#244:                               # %land.lhs.true729
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB11_246
# BB#245:                               # %land.lhs.true738
	movzbl	49(%rax), %eax
	testq	$64, %rax
	jne	.LBB11_247
.LBB11_246:                             # %land.lhs.true748
	testq	%rbx, %r14
	je	.LBB11_248
.LBB11_247:                             # %lor.lhs.false750
	cmpq	$0, 32(%r12)
	je	.LBB11_248
# BB#253:                               # %if.end766
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_256
# BB#254:                               # %land.lhs.true768
	movq	integer_types+40(%rip), %rax
	movl	60(%r12), %edx
	movl	60(%rax), %eax
	andq	$511, %rdx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rdx
	je	.LBB11_256
# BB#255:                               # %if.then779
	movq	asmfile(%rip), %rdi
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_256:                             # %if.end786
	movq	asmfile(%rip), %rsi
	movq	$101, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	24(%r12), %r13
	movq	asmfile(%rip), %rcx
	testq	%r13, %r13
	je	.LBB11_269
# BB#257:                               # %for.body793.lr.ph
	movabsq	$.L.str46, %r15
	movabsq	$7378697629483820647, %r14 # imm = 0x6666666666666667
.LBB11_258:                             # %for.body793
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_261 Depth 2
                                        #     Child Loop BB11_265 Depth 2
	movq	24(%r13), %rax
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	24(%r13), %rax
	movl	24(%rax), %eax
	movl	current_sym_nchars(%rip), %ecx
	leaq	1(%rax,%rcx), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	32(%r13), %rdi
	movq	40(%rdi), %rax
	cmpq	$-1, %rax
	jne	.LBB11_259
# BB#263:                               # %land.lhs.true818
                                        #   in Loop: Header=BB11_258 Depth=1
	movq	32(%rdi), %rbx
	testq	%rbx, %rbx
	jns	.LBB11_267
# BB#264:                               # %while.body.i.preheader
                                        #   in Loop: Header=BB11_258 Depth=1
	movq	asmfile(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	$63, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	negq	%rbx
.LBB11_265:                             # %while.body.i
                                        #   Parent Loop BB11_258 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rax
	imulq	%r14
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_265
# BB#266:                               # %print_wide_int.exit
                                        #   in Loop: Header=BB11_258 Depth=1
	movl	current_sym_nchars(%rip), %eax
	leaq	1(%rsi,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	jmp	.LBB11_268
.LBB11_259:                             # %for.body793
                                        #   in Loop: Header=BB11_258 Depth=1
	testq	%rax, %rax
	jne	.LBB11_267
# BB#260:                               # %if.then805
                                        #   in Loop: Header=BB11_258 Depth=1
	movq	32(%rdi), %r15
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r15, %r12
	sarq	%cl, %r12
	leaq	(%r15,%r12), %rbx
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%r15, %rdx
	callq	fprintf
	xorq	%r12, %rbx
	movq	$63, %rcx
	shrq	%cl, %r15
	testq	%rbx, %rbx
	jle	.LBB11_262
.LBB11_261:                             # %while.body.i1098
                                        #   Parent Loop BB11_258 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rax
	imulq	%r14
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_261
.LBB11_262:                             # %print_wide_int.exit1101
                                        #   in Loop: Header=BB11_258 Depth=1
	movl	current_sym_nchars(%rip), %eax
	leaq	1(%r15,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str46, %r15
	jmp	.LBB11_268
.LBB11_267:                             # %if.else832
                                        #   in Loop: Header=BB11_258 Depth=1
	callq	print_int_cst_octal
.LBB11_268:                             # %if.end836
                                        #   in Loop: Header=BB11_258 Depth=1
	movq	asmfile(%rip), %rsi
	movq	$44, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	(%r13), %r13
	movq	asmfile(%rip), %rcx
	testq	%r13, %r13
	jne	.LBB11_258
.LBB11_269:                             # %for.end847
	movq	$59, %rdi
	movq	%rcx, %rsi
	jmp	.LBB11_276
.LBB11_71:                              # %sw.bb352
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_78
# BB#72:                                # %if.then354
	movq	asmfile(%rip), %rcx
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movabsq	$.L.str26, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	asmfile(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r14, %r14
	jns	.LBB11_74
# BB#73:
	negq	%rbx
.LBB11_74:                              # %if.then354
	movq	$60, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_77
# BB#75:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_76:                              # %while.body.i1278
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_76
.LBB11_77:                              # %print_wide_int.exit1281
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	andq	$1, %r14
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str29, %rdi
	jmp	.LBB11_69
.LBB11_62:                              # %sw.bb330
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_70
# BB#63:                                # %if.then332
	movq	asmfile(%rip), %rcx
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movabsq	$.L.str26, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	asmfile(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r14, %r14
	jns	.LBB11_65
# BB#64:
	negq	%rbx
.LBB11_65:                              # %if.then332
	movq	$60, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_68
# BB#66:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_67:                              # %while.body.i1258
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_67
.LBB11_68:                              # %print_wide_int.exit1261
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	andq	$1, %r14
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str27, %rdi
.LBB11_69:                              # %sw.epilog902
	movq	$5, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$4, %rax
	jmp	.LBB11_290
.LBB11_270:                             # %sw.bb850
	movq	asmfile(%rip), %rsi
	movq	$42, %rdi
	jmp	.LBB11_80
.LBB11_278:                             # %sw.bb874
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_280
# BB#279:                               # %if.then876
	movq	asmfile(%rip), %rsi
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movq	$64, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	112(%r12), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	asmfile(%rip), %rsi
	movq	$44, %rdi
	jmp	.LBB11_80
.LBB11_281:                             # %sw.bb887
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_283
# BB#282:                               # %if.then889
	movq	$1, %rcx
	movb	%cl, have_used_extensions(%rip)
.LBB11_283:                             # %if.end890
	testq	%rax, %rax
	je	.LBB11_284
# BB#285:                               # %if.end890
	movabsq	$1, %rax
	jmp	.LBB11_286
.LBB11_271:                             # %sw.bb855
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_116
# BB#272:                               # %if.then857
	movq	asmfile(%rip), %rsi
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movq	$35, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	112(%r12), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	asmfile(%rip), %rsi
	movq	$44, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	8(%r12), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	24(%r12), %rbx
	jmp	.LBB11_274
.LBB11_273:                             # %while.body.i1375
                                        #   in Loop: Header=BB11_274 Depth=1
	movq	$44, %rdi
	callq	putc
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	(%rbx), %rbx
.LBB11_274:                             # %while.body.i1375
                                        # =>This Inner Loop Header: Depth=1
	movq	asmfile(%rip), %rsi
	testq	%rbx, %rbx
	jne	.LBB11_273
# BB#275:                               # %dbxout_args.exit
	movq	$59, %rdi
	jmp	.LBB11_276
.LBB11_79:                              # %sw.bb365
	movq	asmfile(%rip), %rsi
	movq	$100, %rdi
	jmp	.LBB11_80
.LBB11_102:                             # %sw.bb448
	movl	60(%r12), %ecx
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	$1048576, %rcx          # imm = 0x100000
	je	.LBB11_110
# BB#103:                               # %sw.bb448
	testq	%rbx, %rax
	je	.LBB11_110
# BB#104:                               # %if.then456
	movq	asmfile(%rip), %rcx
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movabsq	$.L.str26, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	asmfile(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r14, %r14
	jns	.LBB11_106
# BB#105:
	negq	%rbx
.LBB11_106:                             # %if.then456
	movq	$60, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_109
# BB#107:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_108:                             # %while.body.i1291
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_108
.LBB11_109:                             # %print_wide_int.exit1294
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	andq	$1, %r14
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str36, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
	jmp	.LBB11_101
.LBB11_92:                              # %sw.bb425
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_100
# BB#93:                                # %if.then427
	movq	asmfile(%rip), %rcx
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movabsq	$.L.str26, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	asmfile(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r14, %r14
	jns	.LBB11_95
# BB#94:
	negq	%rbx
.LBB11_95:                              # %if.then427
	movq	$60, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_98
# BB#96:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_97:                              # %while.body.i1337
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_97
.LBB11_98:                              # %print_wide_int.exit1340
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	andq	$1, %r14
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movzbl	62(%r12), %eax
	testq	$1, %rax
	je	.LBB11_100
# BB#99:                                # %if.then439
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str35, %rdi
	movq	$3, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$3, %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_100:                             # %if.end443
	movq	asmfile(%rip), %rsi
	movq	$83, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
.LBB11_101:                             # %if.end443
	movl	%eax, current_sym_nchars(%rip)
	movq	24(%r12), %rdi
	jmp	.LBB11_117
.LBB11_287:                             # %sw.bb897
	movq	asmfile(%rip), %rsi
	movq	$102, %rdi
	jmp	.LBB11_80
.LBB11_130:
	movabsq	$.L.str40, %rdi
.LBB11_132:                             # %if.then554
	movq	asmfile(%rip), %rcx
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	96(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB11_139
# BB#133:                               # %if.end.i1205
	movzbl	16(%rbx), %eax
	cmpq	$1, %rax
	je	.LBB11_136
# BB#134:                               # %if.end.i1205
	cmpq	$33, %rax
	jne	.LBB11_137
# BB#135:                               # %if.then15.i1207
	movq	72(%rbx), %rbx
.LBB11_136:                             # %dbxout_type_name.exit1214
	movq	asmfile(%rip), %rsi
	movq	32(%rbx), %rdi
	callq	fputs
	movl	24(%rbx), %ecx
	movl	current_sym_nchars(%rip), %eax
	addq	%rcx, %rax
	jmp	.LBB11_140
.LBB11_139:                             # %if.else569
	movl	dbxout_type.anonymous_type_number(%rip), %edx
	movq	asmfile(%rip), %rdi
	leaq	1(%rdx), %rax
	movl	%eax, dbxout_type.anonymous_type_number(%rip)
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
.LBB11_140:                             # %if.end573
	movq	asmfile(%rip), %rsi
	movl	%eax, current_sym_nchars(%rip)
	movq	$58, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%r12), %rax
	movq	typevec(%rip), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	$1, %rdx
	movl	%edx, (%rcx,%rax,4)
	jmp	.LBB11_291
.LBB11_88:                              # %if.else390
	movq	$115, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	%rax, %r14
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r14, %r15
	sarq	%cl, %r15
	leaq	(%r14,%r15), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	xorq	%r15, %rbx
	movq	$63, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB11_91
# BB#89:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_90:                              # %while.body.i1324
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_90
.LBB11_91:                              # %print_wide_int.exit1327
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str31, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	8(%r12), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	8(%r12), %rax
	movl	60(%rax), %edx
	movq	asmfile(%rip), %rdi
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$.L.str32, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	addq	$7, %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str33, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	8(%r12), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	8(%r12), %rax
	movl	60(%rax), %edx
	movq	asmfile(%rip), %rdi
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rcx
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$10, %rax
	jmp	.LBB11_290
.LBB11_284:
	movabsq	$0, %rax
.LBB11_286:                             # %if.end890
	movq	asmfile(%rip), %rsi
	xorq	$1, %rax
	leaq	38(,%rax,4), %rdi
.LBB11_80:                              # %sw.bb365
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_116:                             # %if.end490
	movq	8(%r12), %rdi
	jmp	.LBB11_117
.LBB11_37:                              # %if.else173
	movq	8(%r12), %rax
	testq	%rax, %rax
	je	.LBB11_43
# BB#38:                                # %land.lhs.true178
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB11_43
# BB#39:                                # %if.then187
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_42
# BB#40:                                # %land.lhs.true189
	movq	integer_types+40(%rip), %rax
	movl	60(%r12), %ecx
	movl	60(%rax), %eax
	xorq	%rcx, %rax
	testq	$511, %rax              # imm = 0x1FF
	je	.LBB11_42
# BB#41:                                # %if.then199
	movq	asmfile(%rip), %rdx
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	andq	$511, %rcx              # imm = 0x1FF
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
	movl	%eax, current_sym_nchars(%rip)
	jmp	.LBB11_42
.LBB11_248:                             # %if.then755
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str45, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	96(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB11_167
# BB#249:                               # %if.end.i
	movzbl	16(%rbx), %eax
	cmpq	$1, %rax
	je	.LBB11_252
# BB#250:                               # %if.end.i
	cmpq	$33, %rax
	jne	.LBB11_137
# BB#251:                               # %if.then15.i
	movq	72(%rbx), %rbx
.LBB11_252:                             # %dbxout_type_name.exit
	movq	asmfile(%rip), %rsi
	movq	32(%rbx), %rdi
	callq	fputs
	movl	24(%rbx), %eax
	movl	current_sym_nchars(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, current_sym_nchars(%rip)
	movslq	88(%r12), %rax
	movq	typevec(%rip), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	$1, %rdx
	movl	%edx, (%rcx,%rax,4)
	movq	asmfile(%rip), %rsi
	movq	$58, %rdi
.LBB11_276:                             # %sw.epilog902
	callq	putc
.LBB11_277:                             # %sw.epilog902
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	jmp	.LBB11_290
.LBB11_110:                             # %if.end465
	testq	$65536, %rcx            # imm = 0x10000
	je	.LBB11_113
# BB#111:                               # %if.end465
	testq	%rbx, %rax
	je	.LBB11_113
# BB#112:                               # %if.then474
	movq	asmfile(%rip), %rcx
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movabsq	$.L.str35, %rdi
	movq	$3, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$3, %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_113:                             # %if.end477
	movq	24(%r12), %rbx
	movq	asmfile(%rip), %rax
	testq	%rbx, %rbx
	je	.LBB11_114
# BB#115:                               # %if.else487
	movq	$97, %rdi
	movq	%rax, %rsi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%rbx, %rdi
	callq	dbxout_range_type
	jmp	.LBB11_116
.LBB11_142:
	movabsq	$0, %rax
.LBB11_144:                             # %if.end582
	movq	asmfile(%rip), %rsi
	xorq	$1, %rax
	leaq	115(%rax,%rax), %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	%r12, %rdi
	callq	int_size_in_bytes
	movq	%rax, %r13
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r13, %r15
	sarq	%cl, %r15
	leaq	(%r13,%r15), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdx
	callq	fprintf
	xorq	%r15, %rbx
	movq	$63, %rcx
	shrq	%cl, %r13
	testq	%rbx, %rbx
	jle	.LBB11_147
# BB#145:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
.LBB11_146:                             # %while.body.i1182
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_146
.LBB11_147:                             # %print_wide_int.exit1185
	movl	current_sym_nchars(%rip), %eax
	movl	use_gnu_debug_info_extensions(%rip), %ecx
	leaq	1(%r13,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	testq	%rcx, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rbx           # 8-byte Reload
	je	.LBB11_150
# BB#148:                               # %print_wide_int.exit1185
	testq	%rax, %rbx
	je	.LBB11_150
# BB#149:                               # %if.then596
	movq	asmfile(%rip), %rdi
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movabsq	$.L.str44, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$8, %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_150:                             # %for.cond.preheader
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB11_182
# BB#151:                               # %for.body.lr.ph
	xorq	%rsi, %rsi
	movabsq	$7378697629483820647, %r15 # imm = 0x6666666666666667
.LBB11_152:                             # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_158 Depth 2
                                        #     Child Loop BB11_163 Depth 2
                                        #     Child Loop BB11_175 Depth 2
                                        #     Child Loop BB11_179 Depth 2
	movq	136(%r12), %rax
	movq	64(%rax), %rax
	movl	use_gnu_debug_info_extensions(%rip), %ecx
	movq	32(%rax,%rsi,8), %r13
	testq	%rcx, %rcx
	jne	.LBB11_153
# BB#166:                               # %if.else658
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	8(%r13), %rax
	movq	96(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB11_167
# BB#168:                               # %if.end.i1135
                                        #   in Loop: Header=BB11_152 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$1, %rax
	jne	.LBB11_170
# BB#169:                               #   in Loop: Header=BB11_152 Depth=1
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	jmp	.LBB11_172
.LBB11_153:                             # %if.then613
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movl	16(%r13), %edi
	movq	$18, %rcx
	movq	asmfile(%rip), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$48, %rdi
	callq	putc
	movl	16(%r13), %edi
	movq	$18, %rcx
	movq	asmfile(%rip), %rsi
	shrq	%cl, %rdi
	andq	$2, %rdi
	orq	$48, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB11_160
# BB#154:                               # %land.lhs.true635
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	lang_hooks(%rip), %rdi
	movabsq	$.L.str15, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB11_160
# BB#155:                               # %if.then639
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	72(%r13), %rdi
	movq	%r13, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	asmfile(%rip), %rdi
	movq	%rax, %rbx
	movq	$3, %rcx
	movq	%rbx, %r13
	shlq	%cl, %r13
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%r13, %rdx
	callq	fprintf
	testq	%rbx, %rbx
	jns	.LBB11_157
# BB#156:                               #   in Loop: Header=BB11_152 Depth=1
	negq	%r13
.LBB11_157:                             # %if.then639
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	$60, %rcx
	shrq	%cl, %rbx
	testq	%r13, %r13
	jle	.LBB11_159
.LBB11_158:                             # %while.body.i1169
                                        #   Parent Loop BB11_152 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rax
	imulq	%r15
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %r13
	movq	%rdx, %r13
	jg	.LBB11_158
.LBB11_159:                             # %print_wide_int.exit1172
                                        #   in Loop: Header=BB11_152 Depth=1
	movl	current_sym_nchars(%rip), %eax
	andq	$1, %rbx
	leaq	1(%rbx,%rax), %rax
	jmp	.LBB11_165
.LBB11_160:                             # %if.else645
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	40(%r13), %rdi
	movq	%r13, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	asmfile(%rip), %rdi
	movq	%rax, %r13
	movq	$3, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r13, %r13
	jns	.LBB11_162
# BB#161:                               #   in Loop: Header=BB11_152 Depth=1
	negq	%rbx
.LBB11_162:                             # %if.else645
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	$60, %rcx
	shrq	%cl, %r13
	testq	%rbx, %rbx
	jle	.LBB11_164
.LBB11_163:                             # %while.body.i1156
                                        #   Parent Loop BB11_152 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rax
	imulq	%r15
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_163
.LBB11_164:                             # %print_wide_int.exit1159
                                        #   in Loop: Header=BB11_152 Depth=1
	movl	current_sym_nchars(%rip), %eax
	andq	$1, %r13
	leaq	1(%r13,%rax), %rax
.LBB11_165:                             # %if.end651
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	asmfile(%rip), %rsi
	movl	%eax, current_sym_nchars(%rip)
	movq	$44, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	asmfile(%rip), %rsi
	jmp	.LBB11_181
.LBB11_170:                             # %if.end.i1135
                                        #   in Loop: Header=BB11_152 Depth=1
	cmpq	$33, %rax
	jne	.LBB11_137
# BB#171:                               # %if.then15.i1137
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	72(%rbx), %rbx
.LBB11_172:                             # %dbxout_type_name.exit1144
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	asmfile(%rip), %rsi
	movq	32(%rbx), %rdi
	callq	fputs
	movl	24(%rbx), %eax
	movl	current_sym_nchars(%rip), %ecx
	movq	asmfile(%rip), %rsi
	addq	%rax, %rcx
	movl	%ecx, current_sym_nchars(%rip)
	movq	$58, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	8(%r13), %rdi
	movq	%r14, %rsi
	callq	dbxout_type
	movq	asmfile(%rip), %rsi
	movq	$44, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	40(%r13), %rdi
	movq	%r13, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	asmfile(%rip), %rdi
	movq	%rax, %r13
	movq	$3, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r13, %r13
	jns	.LBB11_174
# BB#173:                               #   in Loop: Header=BB11_152 Depth=1
	negq	%rbx
.LBB11_174:                             # %dbxout_type_name.exit1144
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	$60, %rcx
	shrq	%cl, %r13
	testq	%rbx, %rbx
	jle	.LBB11_176
.LBB11_175:                             # %while.body.i1126
                                        #   Parent Loop BB11_152 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rax
	imulq	%r15
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_175
.LBB11_176:                             # %print_wide_int.exit1129
                                        #   in Loop: Header=BB11_152 Depth=1
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	andq	$1, %r13
	leaq	1(%r13,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$44, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	96(%rax), %rax
	movq	40(%rax), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	asmfile(%rip), %rdi
	movq	%rax, %r13
	movq	$3, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	testq	%r13, %r13
	jns	.LBB11_178
# BB#177:                               #   in Loop: Header=BB11_152 Depth=1
	negq	%rbx
.LBB11_178:                             # %print_wide_int.exit1129
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	$60, %rcx
	shrq	%cl, %r13
	testq	%rbx, %rbx
	jle	.LBB11_180
.LBB11_179:                             # %while.body.i1113
                                        #   Parent Loop BB11_152 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rax
	imulq	%r15
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_179
.LBB11_180:                             # %print_wide_int.exit1116
                                        #   in Loop: Header=BB11_152 Depth=1
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	andq	$1, %r13
	leaq	1(%r13,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_181:                             # %for.inc
                                        #   in Loop: Header=BB11_152 Depth=1
	movq	$59, %rdi
	callq	putc
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB11_152
.LBB11_182:                             # %for.end
	movq	24(%r12), %r14
	testq	%r14, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB11_214
.LBB11_183:                             # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_209 Depth 2
                                        #     Child Loop BB11_211 Depth 2
	movzbl	16(%r14), %eax
	cmpq	$33, %rax
	je	.LBB11_213
# BB#184:                               # %for.body.i
                                        #   in Loop: Header=BB11_183 Depth=1
	cmpq	$37, %rax
	jne	.LBB11_188
# BB#185:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	%r14, %rdi
	callq	bit_position
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%rbx, %rax
	je	.LBB11_213
# BB#186:                               # %lor.lhs.false9.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	40(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB11_213
# BB#187:                               # %lor.lhs.false11.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	$1, %rsi
	callq	host_integerp
	testq	%rbx, %rax
	je	.LBB11_213
.LBB11_188:                             # %lor.lhs.false16.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movzbl	49(%r14), %eax
	testq	$64, %rax
	jne	.LBB11_213
# BB#189:                               # %if.else.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$32, %rax
	je	.LBB11_213
# BB#190:                               # %if.then26.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	72(%r14), %rax
	movq	asmfile(%rip), %rcx
	testq	%rax, %rax
	jne	.LBB11_191
# BB#192:                               # %if.else42.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	$58, %rdi
	movq	%rcx, %rsi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	jmp	.LBB11_193
.LBB11_191:                             # %if.then33.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str46, %rsi
	callq	fprintf
	movq	72(%r14), %rax
	movl	24(%rax), %eax
	movl	current_sym_nchars(%rip), %ecx
	leaq	2(%rax,%rcx), %rax
.LBB11_193:                             # %if.end45.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movl	use_gnu_debug_info_extensions(%rip), %ecx
	movl	%eax, current_sym_nchars(%rip)
	testq	%rcx, %rcx
	je	.LBB11_198
# BB#194:                               # %land.lhs.true47.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movl	16(%r14), %eax
	andq	$3145983, %rax          # imm = 0x3000FF
	cmpq	$37, %rax
	je	.LBB11_198
# BB#195:                               # %if.then65.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	asmfile(%rip), %rsi
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movq	$47, %rdi
	callq	putc
	movl	16(%r14), %eax
	testq	$1048576, %rax          # imm = 0x100000
	movq	$48, %rdi
	jne	.LBB11_197
# BB#196:                               # %cond.false.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	$21, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$50, %rdi
	subq	%rax, %rdi
.LBB11_197:                             # %cond.end.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	asmfile(%rip), %rsi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_198:                             # %if.end82.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$37, %rax
	jne	.LBB11_200
# BB#199:                               # %land.lhs.true88.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	96(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB11_201
.LBB11_200:                             # %cond.false94.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	8(%r14), %rdi
.LBB11_201:                             # %cond.end97.i
                                        #   in Loop: Header=BB11_183 Depth=1
	xorq	%rsi, %rsi
	callq	dbxout_type
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	jne	.LBB11_208
# BB#202:                               # %if.then104.i
                                        #   in Loop: Header=BB11_183 Depth=1
	testq	$262144, %rax           # imm = 0x40000
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB11_207
# BB#203:                               # %if.then104.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rbx, %rax
	je	.LBB11_207
# BB#204:                               # %if.then112.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	120(%r14), %r15
	testq	%r15, %r15
	jne	.LBB11_206
# BB#205:                               # %cond.false117.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	%r14, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r14), %r15
.LBB11_206:                             # %cond.end118.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movq	asmfile(%rip), %rdi
	movq	32(%r15), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str49, %rsi
	callq	fprintf
	movl	24(%r15), %eax
	movl	current_sym_nchars(%rip), %ecx
	leaq	2(%rax,%rcx), %rax
	movl	%eax, current_sym_nchars(%rip)
	jmp	.LBB11_213
.LBB11_208:                             # %if.else134.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	asmfile(%rip), %rsi
	movq	$44, %rdi
	callq	putc
	movq	%r14, %rdi
	callq	int_bit_position
	movq	%rax, %r13
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r13, %rbx
	sarq	%cl, %rbx
	leaq	(%r13,%rbx), %r15
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	movq	%r13, %rdx
	callq	fprintf
	xorq	%rbx, %r15
	movq	$63, %rcx
	shrq	%cl, %r13
	testq	%r15, %r15
	movabsq	$7378697629483820647, %rbx # imm = 0x6666666666666667
	jle	.LBB11_210
.LBB11_209:                             # %while.body.i.i
                                        #   Parent Loop BB11_183 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rax
	imulq	%rbx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %r15
	movq	%rdx, %r15
	jg	.LBB11_209
.LBB11_210:                             # %print_wide_int.exit.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	leaq	1(%r13,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$44, %rdi
	callq	putc
	movq	40(%r14), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%rdx, %r13
	sarq	%cl, %r13
	movq	%rbx, %r15
	leaq	(%rdx,%r13), %rbx
	xorq	%rax, %rax
	movabsq	$.L.str8, %rsi
	callq	fprintf
	movq	24(%rsp), %rdi          # 8-byte Reload
	xorq	%r13, %rbx
	movq	$63, %rcx
	shrq	%cl, %rdi
	testq	%rbx, %rbx
	jle	.LBB11_212
.LBB11_211:                             # %while.body.i188.i
                                        #   Parent Loop BB11_183 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rax
	imulq	%r15
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB11_211
.LBB11_212:                             # %print_wide_int.exit191.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	leaq	1(%rdi,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	addq	$3, %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB11_213
.LBB11_207:                             # %if.else130.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	asmfile(%rip), %rcx
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str50, %rdi
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$5, %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_213:                             # %for.inc.i
                                        #   in Loop: Header=BB11_183 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB11_183
.LBB11_214:                             # %dbxout_type_fields.exit
	movq	%rbx, %r14
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_237
# BB#215:                               # %land.lhs.true687
	movq	112(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB11_237
# BB#216:                               # %if.end.i1355
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movq	96(%r12), %rax
	movq	72(%rax), %rax
	movl	24(%rax), %edx
	movabsq	$.L.str14, %rsi
	leaq	32(%rsp), %rdi
	xorq	%rax, %rax
	callq	sprintf
	movzbl	16(%rbx), %eax
	cmpq	$3, %rax
	jne	.LBB11_217
# BB#218:                               # %if.else.i1356
	movq	32(%rbx), %r15
	testq	%r15, %r15
	jne	.LBB11_221
# BB#219:                               # %while.cond.i.preheader
	movq	40(%rbx), %r15
	jmp	.LBB11_220
.LBB11_78:                              # %if.else361
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str30, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$17, %rax
	jmp	.LBB11_290
.LBB11_70:                              # %if.else339
	movq	asmfile(%rip), %rsi
	movq	$114, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	movq	integer_types(%rip), %rcx
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%rcx), %rax
	movq	typevec(%rip), %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movl	8(%rdx,%rax,4), %ecx
	movl	4(%rdx,%rax,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movl	16(%r12), %edx
	movq	asmfile(%rip), %rdi
	movq	$6, %rcx
	shrq	%cl, %rdx
	andq	$128, %rdx
	orq	$127, %rdx
	movabsq	$.L.str28, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB11_36:                              # %sw.epilog902
	movl	current_sym_nchars(%rip), %eax
	addq	$7, %rax
.LBB11_290:                             # %sw.epilog902
	movl	%eax, current_sym_nchars(%rip)
.LBB11_291:                             # %sw.epilog902
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_280:                             # %if.else885
	movq	integer_types+40(%rip), %rdi
	jmp	.LBB11_117
.LBB11_43:                              # %if.else207
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_42
# BB#44:                                # %land.lhs.true209
	movq	integer_types+40(%rip), %rax
	movl	60(%r12), %edx
	movl	60(%rax), %eax
	xorq	%rdx, %rax
	testq	$511, %rax              # imm = 0x1FF
	je	.LBB11_46
# BB#45:                                # %if.end227
	movq	asmfile(%rip), %rdi
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movl	use_gnu_debug_info_extensions(%rip), %ecx
	addq	$5, %rax
	movl	%eax, current_sym_nchars(%rip)
	testq	%rcx, %rcx
	je	.LBB11_42
.LBB11_46:                              # %land.lhs.true229
	movq	104(%r12), %rax
	testq	%rax, %rax
	je	.LBB11_42
# BB#47:                                # %land.lhs.true233
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB11_42
# BB#48:                                # %land.lhs.true242
	movq	112(%r12), %rax
	testq	%rax, %rax
	je	.LBB11_42
# BB#49:                                # %land.lhs.true246
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB11_42
# BB#50:                                # %land.lhs.true255
	movq	integer_types+40(%rip), %rcx
	movl	60(%r12), %eax
	movl	60(%rcx), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	ja	.LBB11_56
# BB#51:                                # %lor.lhs.false266
	andq	%rbx, %rcx
	andq	%rbx, %rax
	cmpq	%rcx, %rax
	jne	.LBB11_53
# BB#52:                                # %land.lhs.true277
	movzbl	17(%r12), %ecx
	testq	$32, %rcx
	jne	.LBB11_56
.LBB11_53:                              # %lor.lhs.false284
	cmpq	$64, %rax
	ja	.LBB11_56
# BB#54:                                # %lor.lhs.false291
	cmpq	$64, %rax
	jne	.LBB11_42
# BB#55:                                # %land.lhs.true298
	movzbl	17(%r12), %eax
	testq	$32, %rax
	je	.LBB11_42
.LBB11_56:                              # %if.then305
	movq	asmfile(%rip), %rax
	movq	$114, %rdi
	movq	%rax, %rsi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%r12), %rax
	movq	typevec(%rip), %rcx
	movq	asmfile(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movl	8(%rcx,%rax,4), %ebx
	movl	4(%rcx,%rax,4), %ecx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	movq	%rcx, %rsi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	104(%r12), %rax
	movq	%rax, %rdi
	callq	print_int_cst_octal
	movq	asmfile(%rip), %rax
	movq	$59, %rdi
	movq	%rax, %rsi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	112(%r12), %rax
	movq	%rax, %rdi
	callq	print_int_cst_octal
	movq	asmfile(%rip), %rax
	movq	$59, %rdi
	movq	%rax, %rsi
	callq	fputc
	jmp	.LBB11_277
.LBB11_114:                             # %if.then482
	movabsq	$.L.str37, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rax, %rcx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	movq	integer_types+40(%rip), %rcx
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movslq	88(%rcx), %rax
	movq	typevec(%rip), %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movl	8(%rdx,%rax,4), %ecx
	movl	4(%rdx,%rax,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rcx
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	movabsq	$.L.str38, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$6, %rax
	movl	%eax, current_sym_nchars(%rip)
	jmp	.LBB11_116
.LBB11_42:                              # %if.end206
	movq	%r12, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	dbxout_range_type  # TAILCALL
.LBB11_217:
	movq	%rbx, %r15
	jmp	.LBB11_221
.LBB11_167:                             # %if.then.i1131
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.dbxout_type_name, %rcx
	movq	$1815, %rsi             # imm = 0x717
	jmp	.LBB11_138
.LBB11_137:                             # %if.else19.i1208
	movabsq	$.L.str6, %rax
	movabsq	$.L__FUNCTION__.dbxout_type_name, %rcx
	movq	$1825, %rsi             # imm = 0x721
.LBB11_138:                             # %if.else19.i1208
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB11_220:                             # %while.cond.i.preheader
	testq	%r15, %r15
	je	.LBB11_237
.LBB11_221:                             # %for.cond.i1358.preheader
	testq	%r15, %r15
	je	.LBB11_237
# BB#222:
	movq	%r15, %rbx
	xorq	%rax, %rax
	movq	$1, %r13
.LBB11_223:                             # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB11_226
# BB#224:                               # %lor.rhs.i
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	72(%rbx), %rcx
	movq	72(%rax), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB11_225
.LBB11_226:                             # %for.body.i1359
                                        #   in Loop: Header=BB11_223 Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$30, %rcx
	jne	.LBB11_234
# BB#227:                               # %if.end30.i
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB11_229
# BB#228:                               # %cond.false.i1361
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB11_229:                             # %cond.end.i1366
                                        #   in Loop: Header=BB11_223 Depth=1
	movzbl	49(%rbx), %ecx
	testq	$192, %rcx
	jne	.LBB11_233
# BB#230:                               # %if.end48.i
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	32(%rax), %r15
	testq	%r14, %r13
	je	.LBB11_232
# BB#231:                               # %if.then50.i
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	72(%rbx), %r13
	movq	asmfile(%rip), %rdi
	movq	32(%r13), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str47, %rsi
	callq	fprintf
	movl	24(%r13), %eax
	movl	current_sym_nchars(%rip), %ecx
	leaq	2(%rax,%rcx), %rax
	movl	%eax, current_sym_nchars(%rip)
.LBB11_232:                             # %if.end62.i
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	8(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	callq	dbxout_type
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	dbxout_type_method_1
.LBB11_233:                             # %for.inc.i1369
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	%rbx, %rax
.LBB11_234:                             # %for.inc.i1369
                                        #   in Loop: Header=BB11_223 Depth=1
	movq	(%rbx), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	jne	.LBB11_223
	jmp	.LBB11_235
.LBB11_237:                             # %if.end693
	movq	asmfile(%rip), %rsi
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	movl	use_gnu_debug_info_extensions(%rip), %ecx
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	testq	%rcx, %rcx
	je	.LBB11_291
# BB#238:                               # %land.lhs.true697
	movzbl	16(%r12), %eax
	cmpq	$20, %rax
	jne	.LBB11_291
# BB#239:                               # %land.lhs.true704
	cmpq	$0, 104(%r12)
	je	.LBB11_291
# BB#240:                               # %if.then708
	movq	asmfile(%rip), %rsi
	movq	$1, %rax
	movb	%al, have_used_extensions(%rip)
	movq	$126, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	cmpq	$0, 104(%r12)
	je	.LBB11_242
# BB#241:                               # %if.then714
	movq	asmfile(%rip), %rax
	movq	$37, %rdi
	movq	%rax, %rsi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	104(%r12), %rax
	movq	184(%rax), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	dbxout_type
.LBB11_242:                             # %if.end720
	movq	asmfile(%rip), %rax
	movq	$59, %rdi
	movq	%rax, %rsi
	jmp	.LBB11_276
.LBB11_225:
	movq	%rbx, %r15
.LBB11_235:                             # %for.end.i
	testq	%r14, %r13
	jne	.LBB11_220
# BB#236:                               # %if.then67.i
	movq	asmfile(%rip), %rsi
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	jmp	.LBB11_220
.Ltmp93:
	.size	dbxout_type, .Ltmp93-dbxout_type
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI11_0:
	.quad	.LBB11_289
	.quad	.LBB11_33
	.quad	.LBB11_57
	.quad	.LBB11_81
	.quad	.LBB11_288
	.quad	.LBB11_243
	.quad	.LBB11_71
	.quad	.LBB11_62
	.quad	.LBB11_270
	.quad	.LBB11_278
	.quad	.LBB11_281
	.quad	.LBB11_271
	.quad	.LBB11_79
	.quad	.LBB11_102
	.quad	.LBB11_92
	.quad	.LBB11_119
	.quad	.LBB11_119
	.quad	.LBB11_119
	.quad	.LBB11_287
	.quad	.LBB11_289

	.text
	.align	16, 0x90
	.type	dbxout_symbol_location,@function
dbxout_symbol_location:                 # @dbxout_symbol_location
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
	subq	$16, %rsp
.Ltmp105:
	.cfi_def_cfa_offset 64
.Ltmp106:
	.cfi_offset %rbx, -48
.Ltmp107:
	.cfi_offset %r12, -40
.Ltmp108:
	.cfi_offset %r13, -32
.Ltmp109:
	.cfi_offset %r14, -24
.Ltmp110:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%r8, 8(%rsp)
	movl	(%r8), %ebx
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r13
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB12_6
	jmp	.LBB12_1
	.align	16, 0x90
.LBB12_3:                               # %while.body
                                        #   in Loop: Header=BB12_1 Depth=1
	movq	8(%r8), %r8
	movl	(%r8), %ebx
.LBB12_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$61, %rbx
	je	.LBB12_4
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB12_1 Depth=1
	cmpq	$63, %rbx
	je	.LBB12_3
	jmp	.LBB12_5
.LBB12_4:                               # %if.then7
	movl	8(%r8), %ecx
	xorq	%rax, %rax
	cmpq	$52, %rcx
	ja	.LBB12_77
.LBB12_5:                               # %if.end12
	leaq	8(%rsp), %rdi
	callq	alter_subreg
	movq	%rax, %r8
	movq	%r8, 8(%rsp)
	movl	(%r8), %ebx
.LBB12_6:                               # %if.end13
	andq	$65535, %rbx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rbx
	je	.LBB12_13
# BB#7:                                 # %if.end13
	cmpq	$65, %rbx
	je	.LBB12_60
# BB#8:                                 # %if.end13
	cmpq	$61, %rbx
	jne	.LBB12_77
# BB#9:                                 # %if.then17
	movslq	8(%r8), %rcx
	cmpq	$52, %rcx
	jg	.LBB12_77
# BB#10:                                # %if.else83
	testq	%rcx, %rcx
	js	.LBB12_59
# BB#11:                                # %if.then85
	movl	target_flags(%rip), %eax
	movq	$64, %rdx
	movl	%edx, current_sym_code(%rip)
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB12_12
# BB#29:                                # %if.then85
	movabsq	$dbx64_register_map, %rax
	jmp	.LBB12_30
.LBB12_13:                              # %land.lhs.true
	movq	8(%r8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$68, %rdx
	jne	.LBB12_31
# BB#14:                                # %if.then34
	movzbl	18(%r13), %eax
	testq	$8, %rax
	je	.LBB12_16
# BB#15:                                # %if.then37
	movq	$32, %rax
	movl	%eax, current_sym_code(%rip)
	movq	$71, %r12
	jmp	.LBB12_44
.LBB12_31:                              # %land.lhs.true95
	movq	8(%r8), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$66, %rcx
	jne	.LBB12_32
# BB#40:                                # %if.else172
	movq	$128, %rax
	movl	%eax, current_sym_code(%rip)
	movq	8(%rdx), %rax
	movzbl	(%rax), %ecx
	xorq	%r12, %r12
	cmpq	$75, %rcx
	jne	.LBB12_41
# BB#42:                                # %cond.true182
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	jmp	.LBB12_43
.LBB12_32:                              # %land.lhs.true95
	cmpq	$61, %rcx
	jne	.LBB12_48
# BB#33:                                # %land.lhs.true109
	movl	8(%rdx), %ecx
	cmpq	$16, %rcx
	ja	.LBB12_35
# BB#34:                                # %land.lhs.true109
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$65728, %rsi            # imm = 0x100C0
	je	.LBB12_35
.LBB12_48:                              # %land.lhs.true208
	movq	8(%r8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB12_52
# BB#49:                                # %if.then216
	movq	$128, %rax
	movl	%eax, current_sym_code(%rip)
	movzbl	(%rcx), %eax
	xorq	%r12, %r12
	cmpq	$75, %rax
	movabsq	$0, %rax
	jne	.LBB12_51
# BB#50:                                # %cond.true224
	movq	16(%rcx), %rax
	movq	8(%rax), %rax
.LBB12_51:                              # %cond.end235
	movl	%eax, current_sym_value(%rip)
	jmp	.LBB12_44
.LBB12_16:                              # %if.else
	movq	%rcx, current_sym_addr(%rip)
	movq	%r13, %rdi
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB12_17
# BB#18:                                # %if.else
	movabsq	$1, %rax
	jmp	.LBB12_19
.LBB12_41:
	xorq	%rax, %rax
	jmp	.LBB12_43
.LBB12_17:
	movabsq	$0, %rax
.LBB12_19:                              # %if.else
	movq	104(%r13), %rbx
	leaq	83(%rax,%rax,2), %r12
	testq	%rbx, %rbx
	je	.LBB12_22
# BB#20:                                # %lor.lhs.false
	movq	lang_hooks(%rip), %rdi
	movabsq	$.L.str15, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_23
# BB#21:                                # %lor.lhs.false
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB12_23
.LBB12_22:                              # %if.then51
	movq	$40, %rax
	movl	%eax, current_sym_code(%rip)
	jmp	.LBB12_44
.LBB12_52:                              # %land.lhs.true243
	movq	8(%r8), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$75, %rcx
	jne	.LBB12_57
# BB#53:                                # %land.lhs.true251
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$54, %rsi
	jne	.LBB12_57
# BB#54:                                # %if.then262
	movq	$128, %rax
	movl	%eax, current_sym_code(%rip)
	movzbl	(%rdx), %eax
	xorq	%r12, %r12
	cmpq	$75, %rax
	movabsq	$0, %rax
	jne	.LBB12_56
# BB#55:                                # %cond.true270
	movq	8(%rcx), %rax
.LBB12_56:                              # %cond.end281
	movl	%eax, current_sym_value(%rip)
	jmp	.LBB12_44
.LBB12_12:
	movabsq	$svr4_dbx_register_map, %rax
.LBB12_30:                              # %if.then85
	movl	(%rax,%rcx,4), %eax
	movl	%eax, current_sym_value(%rip)
	movq	$114, %r12
	jmp	.LBB12_44
.LBB12_35:                              # %if.then140
	movq	$64, %rcx
	movl	%ecx, current_sym_code(%rip)
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	ja	.LBB12_77
# BB#36:                                # %if.end149
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	je	.LBB12_37
# BB#38:                                # %if.end149
	movabsq	$dbx64_register_map, %rax
	jmp	.LBB12_39
.LBB12_57:                              # %land.lhs.true289
	movq	8(%r8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$58, %rdx
	jne	.LBB12_59
# BB#58:                                # %if.then297
	movq	$40, %rax
	movl	%eax, current_sym_code(%rip)
	movq	8(%rcx), %rax
	movq	%rax, current_sym_addr(%rip)
	movq	$86, %r12
	jmp	.LBB12_44
.LBB12_59:                              # %if.else304
	cmpq	$65, %rbx
	jne	.LBB12_77
.LBB12_60:                              # %if.then309
	movzbl	16(%r15), %ecx
	cmpq	$8, %rcx
	jne	.LBB12_77
# BB#61:                                # %if.end317
	movq	8(%r15), %r14
	movq	8(%r8), %rcx
	movabsq	$.L.str16, %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	dbxout_symbol_location
	xorq	%rax, %rax
	movq	8(%rsp), %rcx
	movl	%eax, current_sym_code(%rip)
	movl	%eax, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	movq	16(%rcx), %rcx
	movabsq	$.L.str17, %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	dbxout_symbol_location
	jmp	.LBB12_76
.LBB12_23:                              # %if.else52
	movq	%rcx, %rbx
	movzbl	49(%r13), %eax
	testq	$16, %rax
	je	.LBB12_25
# BB#24:                                # %if.then58
	movq	$36, %rax
	movl	%eax, current_sym_code(%rip)
	jmp	.LBB12_44
.LBB12_25:                              # %if.else59
	movq	current_sym_addr(%rip), %rdi
	movl	(%rdi), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	jne	.LBB12_28
# BB#26:                                # %if.then68
	callq	get_pool_constant
	movzbl	(%rax), %ecx
	addq	$-67, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	ja	.LBB12_28
# BB#27:                                # %if.then77
	movq	%rax, current_sym_addr(%rip)
.LBB12_28:                              # %if.end79
	movq	$38, %rax
	movl	%eax, current_sym_code(%rip)
	jmp	.LBB12_44
.LBB12_37:
	movabsq	$svr4_dbx_register_map, %rax
.LBB12_39:                              # %if.end149
	movl	(%rax,%rcx,4), %eax
	movq	$114, %r12
.LBB12_43:                              # %if.end197
	movl	%eax, current_sym_value(%rip)
	movq	$13, %rdi
	callq	make_node
	movq	8(%r13), %rcx
	movq	%rax, %r15
	movq	%rcx, 8(%r15)
.LBB12_44:                              # %if.end334
	movq	80(%r13), %rax
	testq	%rax, %rax
	je	.LBB12_62
# BB#45:                                # %land.lhs.true.i
	movzbl	16(%rax), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB12_62
# BB#46:                                # %if.then.i361
	cmpq	$0, 120(%r13)
	leaq	120(%r13), %rbx
	jne	.LBB12_63
# BB#47:                                # %cond.false.i
	movq	%r13, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	jmp	.LBB12_63
.LBB12_62:                              # %if.else.i362
	leaq	72(%r13), %rbx
.LBB12_63:                              # %if.end.i364
	movq	(%rbx), %rax
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB12_65
# BB#64:
	movabsq	$.L.str11, %rcx
.LBB12_65:                              # %if.end.i364
	movq	asmfile(%rip), %rdi
	testq	%r14, %r14
	jne	.LBB12_67
# BB#66:                                # %if.end.i364
	movabsq	$.L.str19, %r14
.LBB12_67:                              # %if.end.i364
	movabsq	$.L.str18, %rsi
	movabsq	$.L.str1, %rdx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %r8
	callq	fprintf
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB12_69
# BB#68:                                # %if.then24.i
	movq	asmfile(%rip), %rsi
	movq	%r12, %rdi
	callq	putc
.LBB12_69:                              # %dbxout_symbol_name.exit
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_72
# BB#70:                                # %dbxout_symbol_name.exit
	testq	%r13, %r13
	je	.LBB12_72
# BB#71:                                # %if.then.i
	movl	32(%r13), %ebx
.LBB12_72:                              # %if.end.i
	movq	asmfile(%rip), %rdi
	movl	current_sym_code(%rip), %edx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rcx
	callq	fprintf
	movq	current_sym_addr(%rip), %rsi
	movq	asmfile(%rip), %rdi
	testq	%rsi, %rsi
	je	.LBB12_74
# BB#73:                                # %if.then2.i
	callq	output_addr_const
	jmp	.LBB12_75
.LBB12_74:                              # %if.else.i
	movl	current_sym_value(%rip), %edx
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB12_75:                              # %dbxout_finish_symbol.exit
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	putc
.LBB12_76:                              # %return
	movq	$1, %rax
.LBB12_77:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp111:
	.size	dbxout_symbol_location, .Ltmp111-dbxout_symbol_location
	.cfi_endproc

	.globl	dbxout_syms
	.align	16, 0x90
	.type	dbxout_syms,@function
dbxout_syms:                            # @dbxout_syms
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp115:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp116:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp117:
	.cfi_def_cfa_offset 32
.Ltmp118:
	.cfi_offset %rbx, -24
.Ltmp119:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	xorq	%r14, %r14
	jmp	.LBB13_2
	.align	16, 0x90
.LBB13_1:                               # %while.body
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%rbx, %rdi
	callq	dbxout_symbol
	movq	(%rbx), %rbx
	addq	%rax, %r14
.LBB13_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB13_1
# BB#3:                                 # %while.end
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp120:
	.size	dbxout_syms, .Ltmp120-dbxout_syms
	.cfi_endproc

	.globl	dbxout_parms
	.align	16, 0x90
	.type	dbxout_parms,@function
dbxout_parms:                           # @dbxout_parms
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp127:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp128:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp129:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp130:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp131:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp132:
	.cfi_def_cfa_offset 64
.Ltmp133:
	.cfi_offset %rbx, -48
.Ltmp134:
	.cfi_offset %r12, -40
.Ltmp135:
	.cfi_offset %r13, -32
.Ltmp136:
	.cfi_offset %r14, -24
.Ltmp137:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB14_137
# BB#1:
	movq	$160, %r13
	movabsq	$.L.str1, %r15
	movabsq	$.L.str13, %r12
	jmp	.LBB14_2
.LBB14_133:                             # %if.then2.i
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	output_addr_const
	jmp	.LBB14_135
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 72(%rbx)
	je	.LBB14_136
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rbx), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB14_136
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	160(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	eliminate_regs
	movq	%rax, 160(%rbx)
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB14_6
# BB#5:                                 # %cond.false
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB14_6:                               # %cond.end
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	eliminate_regs
	movq	%rax, 144(%rbx)
	movq	160(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$66, %rdx
	jne	.LBB14_18
# BB#7:                                 # %if.then19
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edx
	xorq	%rax, %rax
	cmpq	$75, %rdx
	jne	.LBB14_10
# BB#8:                                 # %land.lhs.true26
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB14_10
# BB#9:                                 # %if.then33
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rcx), %rax
.LBB14_10:                              # %if.end
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	%eax, current_sym_value(%rip)
	movl	%r13d, current_sym_code(%rip)
	movq	$0, current_sym_addr(%rip)
	movq	72(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_11
# BB#12:                                # %if.else50
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movq	$8, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$112, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	movq	%r15, %rdx
	callq	fprintf
	jmp	.LBB14_13
.LBB14_18:                              # %if.else54
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_20
# BB#19:                                # %cond.false61
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_20:                              # %cond.end64
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	(%rax), %ecx
	leaq	160(%rbx), %r14
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB14_39
# BB#21:                                # %if.then70
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$64, %rax
	movl	%eax, current_sym_code(%rip)
	movq	$0, current_sym_addr(%rip)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_23
# BB#22:                                # %cond.false77
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_23:                              # %cond.end80
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	8(%rax), %ecx
	cmpq	$52, %rcx
	ja	.LBB14_26
# BB#24:                                # %if.then86
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_27
# BB#25:                                # %cond.false93
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	jmp	.LBB14_27
.LBB14_39:                              # %if.else142
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_41
# BB#40:                                # %cond.false149
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movl	(%rax), %ecx
.LBB14_41:                              # %cond.end152
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB14_70
# BB#42:                                # %land.lhs.true158
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_44
# BB#43:                                # %cond.false165
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_44:                              # %cond.end168
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB14_70
# BB#45:                                # %land.lhs.true177
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_47
# BB#46:                                # %cond.false184
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
.LBB14_47:                              # %cond.end187
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	8(%rcx), %edx
	cmpq	$6, %rdx
	je	.LBB14_70
# BB#48:                                # %land.lhs.true197
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_50
# BB#49:                                # %cond.false204
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	8(%rcx), %edx
.LBB14_50:                              # %cond.end207
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	$7, %rsi
	je	.LBB14_70
# BB#51:                                # %land.lhs.true217
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_53
# BB#52:                                # %cond.false224
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	8(%rcx), %edx
.LBB14_53:                              # %cond.end227
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$16, %rdx
	jne	.LBB14_54
.LBB14_70:                              # %if.else313
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_72
# BB#71:                                # %cond.false320
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_72:                              # %cond.end323
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB14_90
# BB#73:                                # %land.lhs.true329
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_75
# BB#74:                                # %cond.false336
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_75:                              # %cond.end339
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$66, %rdx
	jne	.LBB14_90
# BB#76:                                # %if.then348
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	72(%rbx), %rdx
	testq	%rdx, %rdx
	movabsq	$.L.str11, %r13
	je	.LBB14_78
# BB#77:                                # %cond.true352
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%rdx), %r13
.LBB14_78:                              # %cond.end359
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_80
# BB#79:                                # %cond.false367
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
.LBB14_80:                              # %cond.end370
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edx
	xorq	%r14, %r14
	cmpq	$61, %rdx
	movabsq	$0, %rdx
	je	.LBB14_84
# BB#81:                                # %if.else383
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_83
# BB#82:                                # %cond.false390
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rcx
.LBB14_83:                              # %cond.end393
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	16(%rcx), %rax
	movq	8(%rax), %rdx
.LBB14_84:                              # %if.end408
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movl	%edx, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	xorq	%rax, %rax
	movabsq	$.L.str12, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movq	8(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_85
# BB#86:                                # %if.end408
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	movq	$160, %r13
	je	.LBB14_88
# BB#87:                                # %if.then.i836
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	32(%rbx), %r14d
	jmp	.LBB14_88
.LBB14_11:                              # %if.then42
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	24(%rax), %ecx
	addq	$2, %rcx
	movl	%ecx, current_sym_nchars(%rip)
	movq	asmfile(%rip), %rdi
	movq	32(%rax), %rcx
	movq	$112, %r8
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	movq	%r15, %rdx
	callq	fprintf
.LBB14_13:                              # %if.end52
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	104(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_16
# BB#14:                                # %if.end52
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB14_16
# BB#15:                                # %if.then.i794
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	32(%rbx), %r14d
.LBB14_16:                              # %if.end.i798
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movl	current_sym_code(%rip), %edx
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	current_sym_addr(%rip), %rsi
	movq	asmfile(%rip), %rdi
	testq	%rsi, %rsi
	je	.LBB14_134
# BB#17:                                # %if.then2.i799
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	output_addr_const
	jmp	.LBB14_135
.LBB14_90:                              # %if.else412
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_92
# BB#91:                                # %cond.false419
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_92:                              # %cond.end422
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB14_136
# BB#93:                                # %land.lhs.true428
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_95
# BB#94:                                # %cond.false435
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_95:                              # %cond.end438
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rax), %rcx
	movq	const_int_rtx+512(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB14_136
# BB#96:                                # %land.lhs.true445
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_98
# BB#97:                                # %cond.false452
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
.LBB14_98:                              # %cond.end455
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$67, %rsi
	je	.LBB14_136
# BB#99:                                # %lor.lhs.false
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_101
# BB#100:                               # %cond.false470
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_101:                             # %cond.end473
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$68, %rsi
	je	.LBB14_136
# BB#102:                               # %lor.lhs.false482
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_104
# BB#103:                               # %cond.false489
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_104:                             # %cond.end492
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$54, %rsi
	je	.LBB14_136
# BB#105:                               # %lor.lhs.false501
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_107
# BB#106:                               # %cond.false508
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_107:                             # %cond.end511
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$55, %rsi
	je	.LBB14_136
# BB#108:                               # %lor.lhs.false520
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_110
# BB#109:                               # %cond.false527
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_110:                             # %cond.end530
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$58, %rsi
	je	.LBB14_136
# BB#111:                               # %lor.lhs.false539
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_113
# BB#112:                               # %cond.false546
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_113:                             # %cond.end549
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$134, %rsi
	je	.LBB14_136
# BB#114:                               # %lor.lhs.false558
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_116
# BB#115:                               # %cond.false565
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_116:                             # %cond.end568
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$56, %rsi
	je	.LBB14_136
# BB#117:                               # %lor.lhs.false577
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_119
# BB#118:                               # %cond.false584
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
	movl	(%rcx), %edx
.LBB14_119:                             # %cond.end587
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$140, %rdx
	je	.LBB14_136
# BB#120:                               # %if.then596
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	%r13d, current_sym_code(%rip)
	testq	%rax, %rax
	jne	.LBB14_122
# BB#121:                               # %cond.false603
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
.LBB14_122:                             # %cond.end606
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	(%rcx), %esi
	xorq	%rdx, %rdx
	cmpq	$61, %rsi
	je	.LBB14_126
# BB#123:                               # %if.else616
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_125
# BB#124:                               # %cond.false623
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
.LBB14_125:                             # %cond.end626
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	16(%rcx), %rax
	movq	8(%rax), %rdx
.LBB14_126:                             # %if.end638
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	%edx, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	movq	72(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_127
# BB#128:                               # %if.else657
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movq	$8, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$112, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	movq	%r15, %rdx
	callq	fprintf
	jmp	.LBB14_129
.LBB14_26:                              # %if.else100
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%r14), %rax
.LBB14_27:                              # %if.end106
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rbx), %r14
	movl	target_flags(%rip), %ecx
	movl	8(%rax), %eax
	testq	$33554432, %rcx         # imm = 0x2000000
	movabsq	$svr4_dbx_register_map, %rcx
	je	.LBB14_29
# BB#28:                                # %if.end106
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$dbx64_register_map, %rcx
.LBB14_29:                              # %if.end106
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	(%rcx,%rax,4), %eax
	movl	%eax, current_sym_value(%rip)
	movq	72(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_30
# BB#31:                                # %if.else138
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movq	$8, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$80, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	movq	%r15, %rdx
	callq	fprintf
	jmp	.LBB14_32
.LBB14_30:                              # %if.then124
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	24(%rax), %ecx
	addq	$2, %rcx
	movl	%ecx, current_sym_nchars(%rip)
	movq	asmfile(%rip), %rdi
	movq	32(%rax), %rcx
	movq	$80, %r8
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	movq	%r15, %rdx
	callq	fprintf
.LBB14_32:                              # %if.end141
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_35
# BB#33:                                # %if.end141
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB14_35
# BB#34:                                # %if.then.i808
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	32(%rbx), %r13d
.LBB14_35:                              # %if.end.i812
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movl	current_sym_code(%rip), %edx
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r13, %rcx
	callq	fprintf
	movq	current_sym_addr(%rip), %rsi
	movq	asmfile(%rip), %rdi
	testq	%rsi, %rsi
	jne	.LBB14_36
# BB#37:                                # %if.else.i815
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	current_sym_value(%rip), %edx
	xorq	%rax, %rax
	movabsq	$.L.str14, %rsi
	callq	fprintf
	movq	$160, %r13
	jmp	.LBB14_135
.LBB14_36:                              # %if.then2.i813
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	output_addr_const
	movq	$160, %r13
	jmp	.LBB14_135
.LBB14_85:                              #   in Loop: Header=BB14_2 Depth=1
	movq	$160, %r13
.LBB14_88:                              # %if.end.i840
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movl	current_sym_code(%rip), %edx
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	current_sym_addr(%rip), %rsi
	movq	asmfile(%rip), %rdi
	testq	%rsi, %rsi
	je	.LBB14_134
# BB#89:                                # %if.then2.i841
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	output_addr_const
	jmp	.LBB14_135
.LBB14_54:                              # %if.then237
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	use_gnu_debug_info_extensions(%rip), %edx
	movq	$64, %rsi
	movl	%esi, current_sym_code(%rip)
	testq	%rdx, %rdx
	movq	$80, %rdx
	je	.LBB14_56
# BB#55:                                # %select.mid
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$97, %rdx
.LBB14_56:                              # %select.end
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB14_58
# BB#57:                                # %cond.false249
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movq	8(%rax), %rcx
.LBB14_58:                              # %cond.end252
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	8(%rcx), %ecx
	cmpq	$52, %rcx
	ja	.LBB14_62
# BB#59:                                # %if.then262
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rax, %rax
	jne	.LBB14_61
# BB#60:                                # %cond.false269
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB14_61:                              # %cond.end272
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	$8, %rax
	movq	%rax, %r14
.LBB14_62:                              # %if.end287
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%r14), %rax
	movl	8(%rax), %eax
	movl	%eax, current_sym_value(%rip)
	movq	$0, current_sym_addr(%rip)
	movq	72(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB14_63
# BB#64:                                # %if.else307
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movq	$8, %rax
	movl	%eax, current_sym_nchars(%rip)
	xorq	%rax, %rax
	movabsq	$.L.str10, %rsi
	movq	%r15, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	fprintf
	jmp	.LBB14_65
.LBB14_63:                              # %if.then291
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%rax), %r14
	movq	%r14, %rdi
	callq	strlen
	movq	asmfile(%rip), %rdi
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	8(%rsp), %r8            # 8-byte Reload
	callq	fprintf
.LBB14_65:                              # %if.end310
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_68
# BB#66:                                # %if.end310
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB14_68
# BB#67:                                # %if.then.i822
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	32(%rbx), %r14d
.LBB14_68:                              # %if.end.i826
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movl	current_sym_code(%rip), %edx
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	current_sym_addr(%rip), %rsi
	movq	asmfile(%rip), %rdi
	testq	%rsi, %rsi
	je	.LBB14_134
# BB#69:                                # %if.then2.i827
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	output_addr_const
	jmp	.LBB14_135
.LBB14_127:                             # %if.then642
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%rax), %r14
	movq	%r14, %rdi
	callq	strlen
	movq	asmfile(%rip), %rdi
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$112, %r8
	xorq	%rax, %rax
	movabsq	$.L.str, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	fprintf
.LBB14_129:                             # %if.end659
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	dbxout_type
	movl	use_gnu_debug_info_extensions(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_132
# BB#130:                               # %if.end659
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB14_132
# BB#131:                               # %if.then.i
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	32(%rbx), %r14d
.LBB14_132:                             # %if.end.i
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rdi
	movl	current_sym_code(%rip), %edx
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	current_sym_addr(%rip), %rsi
	movq	asmfile(%rip), %rdi
	testq	%rsi, %rsi
	jne	.LBB14_133
.LBB14_134:                             # %if.else.i
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	current_sym_value(%rip), %edx
	xorq	%rax, %rax
	movabsq	$.L.str14, %rsi
	callq	fprintf
.LBB14_135:                             # %dbxout_finish_symbol.exit
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	putc
.LBB14_136:                             # %for.inc
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB14_2
.LBB14_137:                             # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp138:
	.size	dbxout_parms, .Ltmp138-dbxout_parms
	.cfi_endproc

	.globl	dbxout_reg_parms
	.align	16, 0x90
	.type	dbxout_reg_parms,@function
dbxout_reg_parms:                       # @dbxout_reg_parms
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp143:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp144:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp145:
	.cfi_def_cfa_offset 32
.Ltmp146:
	.cfi_offset %rbx, -32
.Ltmp147:
	.cfi_offset %r14, -24
.Ltmp148:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB15_24
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB15_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 72(%rbx)
	je	.LBB15_23
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	160(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	jne	.LBB15_23
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	144(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB15_6
# BB#5:                                 # %cond.false
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB15_6:                               # %cond.end
                                        #   in Loop: Header=BB15_2 Depth=1
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB15_10
# BB#7:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB15_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB15_9
# BB#8:                                 # %cond.false19
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB15_9:                               # %cond.end22
                                        #   in Loop: Header=BB15_2 Depth=1
	movl	8(%rcx), %eax
	cmpq	$52, %rax
	jbe	.LBB15_20
.LBB15_10:                              # %if.else
                                        #   in Loop: Header=BB15_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB15_12
# BB#11:                                # %cond.false43
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB15_12:                              # %cond.end46
                                        #   in Loop: Header=BB15_2 Depth=1
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$65, %rdx
	je	.LBB15_20
# BB#13:                                # %if.else66
                                        #   in Loop: Header=BB15_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB15_15
# BB#14:                                # %cond.false73
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	movl	(%rcx), %eax
.LBB15_15:                              # %cond.end76
                                        #   in Loop: Header=BB15_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB15_23
# BB#16:                                # %land.lhs.true81
                                        #   in Loop: Header=BB15_2 Depth=1
	testq	%rcx, %rcx
	jne	.LBB15_18
# BB#17:                                # %cond.false88
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB15_18:                              # %cond.end91
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	160(%rbx), %rsi
	movq	%rcx, %rdi
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB15_23
# BB#19:                                # %if.then98
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	144(%rbx), %rcx
.LBB15_20:                              # %if.then98
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	8(%rbx), %r14
	testq	%rcx, %rcx
	jne	.LBB15_22
# BB#21:                                # %cond.false107
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
.LBB15_22:                              # %cond.end110
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	dbxout_symbol_location
	.align	16, 0x90
.LBB15_23:                              # %for.inc
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_2
.LBB15_24:                              # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp149:
	.size	dbxout_reg_parms, .Ltmp149-dbxout_reg_parms
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_range_type,@function
dbxout_range_type:                      # @dbxout_range_type
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
	movq	asmfile(%rip), %rsi
	movq	%rdi, %r14
	movq	$114, %rdi
	callq	fputc
	movq	8(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB16_2
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	callq	dbxout_type
	jmp	.LBB16_7
.LBB16_2:                               # %if.else
	movzbl	16(%r14), %eax
	cmpq	$6, %rax
	jne	.LBB16_3
# BB#4:                                 # %if.else6
	movslq	88(%r14), %rax
	testq	%rax, %rax
	jne	.LBB16_6
# BB#5:                                 # %if.else10
	movq	integer_types+40(%rip), %rax
	movslq	88(%rax), %rax
.LBB16_6:                               # %if.end12
	movq	typevec(%rip), %rdx
	movq	asmfile(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movl	8(%rdx,%rax,4), %ecx
	movl	4(%rdx,%rax,4), %edx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$9, %rax
	movl	%eax, current_sym_nchars(%rip)
	jmp	.LBB16_7
.LBB16_3:                               # %if.then5
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	dbxout_type
.LBB16_7:                               # %if.end12
	movq	104(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB16_13
# BB#8:                                 # %land.lhs.true
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_13
# BB#9:                                 # %if.then19
	movq	asmfile(%rip), %rsi
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	104(%r14), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r15
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r15, %r12
	sarq	%cl, %r12
	leaq	(%r15,%r12), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	fprintf
	xorq	%r12, %rbx
	movq	$63, %rcx
	shrq	%cl, %r15
	testq	%rbx, %rbx
	jle	.LBB16_12
# BB#10:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
	.align	16, 0x90
.LBB16_11:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB16_11
.LBB16_12:                              # %print_wide_int.exit
	movl	current_sym_nchars(%rip), %eax
	leaq	1(%r15,%rax), %rax
	jmp	.LBB16_14
.LBB16_13:                              # %if.else24
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str55, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
.LBB16_14:                              # %if.end27
	movl	%eax, current_sym_nchars(%rip)
	movq	112(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB16_20
# BB#15:                                # %land.lhs.true30
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_20
# BB#16:                                # %if.then35
	movq	asmfile(%rip), %rsi
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	112(%r14), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r14
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r14, %r15
	sarq	%cl, %r15
	leaq	(%r14,%r15), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	xorq	%r15, %rbx
	movq	$63, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB16_19
# BB#17:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
	.align	16, 0x90
.LBB16_18:                              # %while.body.i74
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB16_18
.LBB16_19:                              # %print_wide_int.exit77
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	jmp	.LBB16_21
.LBB16_20:                              # %if.else43
	movq	asmfile(%rip), %rcx
	movabsq	$.L.str56, %rdi
	movq	$4, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	current_sym_nchars(%rip), %eax
	addq	$4, %rax
.LBB16_21:                              # %if.end46
	movl	%eax, current_sym_nchars(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp164:
	.size	dbxout_range_type, .Ltmp164-dbxout_range_type
	.cfi_endproc

	.align	16, 0x90
	.type	print_int_cst_octal,@function
print_int_cst_octal:                    # @print_int_cst_octal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp170:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp171:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp172:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp173:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp174:
	.cfi_def_cfa_offset 48
.Ltmp175:
	.cfi_offset %rbx, -40
.Ltmp176:
	.cfi_offset %r12, -32
.Ltmp177:
	.cfi_offset %r14, -24
.Ltmp178:
	.cfi_offset %r15, -16
	movq	8(%rdi), %rax
	movl	60(%rax), %ecx
	movq	40(%rdi), %r15
	movq	32(%rdi), %r14
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	$128, %rcx
	je	.LBB17_5
# BB#1:                                 # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$65, %rax
	jb	.LBB17_3
# BB#2:                                 # %if.then8
	addq	$-64, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	andq	%rax, %r15
	jmp	.LBB17_5
.LBB17_3:                               # %if.else10
	xorq	%r15, %r15
	cmpq	$64, %rax
	je	.LBB17_5
# BB#4:                                 # %if.else13
	movq	$1, %rax
	shlq	%cl, %rax
	decq	%rax
	andq	%rax, %r14
.LBB17_5:                               # %if.end19
	movq	asmfile(%rip), %rsi
	movq	$48, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$2, %rcx
	movq	%r15, %rdx
	shrq	%cl, %rdx
	addq	%r15, %r15
	andq	$6, %r15
	movq	$63, %rcx
	movq	%r14, %rbx
	shrq	%cl, %rbx
	movq	asmfile(%rip), %rdi
	orq	%r15, %rbx
	movabsq	$9223372036854775807, %r15 # imm = 0x7FFFFFFFFFFFFFFF
	andq	%r14, %r15
	movabsq	$.L.str53, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rcx
	callq	fprintf
	movl	current_sym_nchars(%rip), %eax
	addq	$2, %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$20, %r12
	movq	$60, %rbx
	movabsq	$.L.str54, %r14
	.align	16, 0x90
.LBB17_6:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	asmfile(%rip), %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	shrq	%cl, %rdx
	andq	$7, %rdx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r12
	addq	$-3, %rbx
	testq	%rax, %rax
	jg	.LBB17_6
# BB#7:                                 # %print_octal.exit
	movl	current_sym_nchars(%rip), %eax
	addq	$21, %rax
	movl	%eax, current_sym_nchars(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp179:
	.size	print_int_cst_octal, .Ltmp179-print_int_cst_octal
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_type_method_1,@function
dbxout_type_method_1:                   # @dbxout_type_method_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp185:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp187:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp188:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp189:
	.cfi_def_cfa_offset 48
.Ltmp190:
	.cfi_offset %rbx, -40
.Ltmp191:
	.cfi_offset %r12, -32
.Ltmp192:
	.cfi_offset %r14, -24
.Ltmp193:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	8(%r15), %rax
	movzbl	16(%rax), %ecx
	movq	%rsi, %r14
	cmpq	$23, %rcx
	jne	.LBB18_2
# BB#1:
	movq	$63, %r9
	movq	$65, %r8
	jmp	.LBB18_11
.LBB18_2:                               # %if.else
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %eax
	testq	$4096, %rax             # imm = 0x1000
	jne	.LBB18_3
# BB#4:                                 # %if.else
	movabsq	$1, %r8
	jmp	.LBB18_5
.LBB18_3:
	movabsq	$0, %r8
.LBB18_5:                               # %if.else
	xorq	$1, %r8
	testq	$2048, %rax             # imm = 0x800
	je	.LBB18_6
# BB#7:                                 # %if.else
	addq	$67, %r8
	jmp	.LBB18_8
.LBB18_6:
	addq	$65, %r8
.LBB18_8:                               # %if.else
	cmpq	$0, 184(%r15)
	movq	$46, %r9
	je	.LBB18_10
# BB#9:                                 # %select.mid
	movq	$42, %r9
.LBB18_10:                              # %select.end
	andq	$255, %r8
.LBB18_11:                              # %if.end29
	movl	16(%r15), %eax
	movq	asmfile(%rip), %rdi
	testq	$1048576, %rax          # imm = 0x100000
	movq	$48, %rcx
	jne	.LBB18_13
# BB#12:                                # %cond.false
	movq	$21, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$50, %rcx
	subq	%rax, %rcx
.LBB18_13:                              # %cond.end
	movabsq	$.L.str48, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movq	120(%r15), %rax
	testq	%rax, %rax
	je	.LBB18_15
# BB#14:                                # %cond.end48.thread
	movl	24(%rax), %ecx
	addq	$6, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	jmp	.LBB18_17
.LBB18_15:                              # %cond.end48
	movq	%r15, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r15), %rax
	movl	24(%rax), %ecx
	addq	$6, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	testq	%rax, %rax
	jne	.LBB18_17
# BB#16:                                # %cond.false58
	movq	%r15, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r15), %rax
.LBB18_17:                              # %cond.end59
	movq	32(%rax), %rax
	movl	current_sym_nchars(%rip), %ecx
	subq	%r14, %rbx
	addq	%rax, %rbx
	addq	%rcx, %rbx
	movl	%ebx, current_sym_nchars(%rip)
	movq	184(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB18_23
# BB#18:                                # %land.lhs.true
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_23
# BB#19:                                # %if.then74
	movq	184(%r15), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	%rax, %r14
	movq	asmfile(%rip), %rdi
	movq	$63, %rcx
	movq	%r14, %r12
	sarq	%cl, %r12
	leaq	(%r14,%r12), %rbx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	xorq	%r12, %rbx
	movq	$63, %rcx
	shrq	%cl, %r14
	testq	%rbx, %rbx
	jle	.LBB18_22
# BB#20:
	movabsq	$7378697629483820647, %rsi # imm = 0x6666666666666667
	.align	16, 0x90
.LBB18_21:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	cmpq	$9, %rbx
	movq	%rdx, %rbx
	jg	.LBB18_21
.LBB18_22:                              # %print_wide_int.exit
	movl	current_sym_nchars(%rip), %eax
	movq	asmfile(%rip), %rsi
	leaq	1(%r14,%rax), %rax
	movl	%eax, current_sym_nchars(%rip)
	movq	$59, %rdi
	callq	putc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
	movq	80(%r15), %rdi
	xorq	%rsi, %rsi
	callq	dbxout_type
	movq	asmfile(%rip), %rsi
	movq	$59, %rdi
	callq	fputc
	movl	current_sym_nchars(%rip), %eax
	incq	%rax
	movl	%eax, current_sym_nchars(%rip)
.LBB18_23:                              # %if.end83
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp194:
	.size	dbxout_type_method_1, .Ltmp194-dbxout_type_method_1
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_block,@function
dbxout_block:                           # @dbxout_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp201:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp202:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp203:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp204:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp205:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp206:
	.cfi_def_cfa_offset 176
.Ltmp207:
	.cfi_offset %rbx, -48
.Ltmp208:
	.cfi_offset %r12, -40
.Ltmp209:
	.cfi_offset %r13, -32
.Ltmp210:
	.cfi_offset %r14, -24
.Ltmp211:
	.cfi_offset %r15, -16
	movq	current_function_func_begin_label(%rip), %rax
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB19_2
# BB#1:                                 # %if.then
	addq	$32, %rax
	jmp	.LBB19_5
.LBB19_2:                               # %if.else
	movq	current_function_decl(%rip), %rdi
	movq	144(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB19_4
# BB#3:                                 # %cond.false
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	144(%rax), %rax
.LBB19_4:                               # %cond.end
	movq	8(%rax), %rax
	addq	$8, %rax
.LBB19_5:                               # %while.cond.preheader
	testq	%r13, %r13
	je	.LBB19_48
# BB#6:                                 # %while.body.lr.ph
	movq	(%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r14, 48(%rsp)          # 8-byte Spill
	leaq	1(%r14), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movabsq	$.L.str65, %r12
	movabsq	$.L.str1, %r15
	testq	%rax, %r14
	je	.LBB19_7
# BB#27:
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB19_28:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_32 Depth 2
                                        #     Child Loop BB19_44 Depth 2
	movl	16(%r13), %eax
	andq	$81920, %rax            # imm = 0x14000
	cmpq	$81920, %rax            # imm = 0x14000
	jne	.LBB19_47
# BB#29:                                # %if.then14
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	%r15, %r14
	movl	debug_info_level(%rip), %eax
	xorq	%r15, %r15
	cmpq	$1, %rax
	je	.LBB19_33
# BB#30:                                # %if.then17
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	32(%r13), %rbx
	xorq	%r15, %r15
	jmp	.LBB19_32
	.align	16, 0x90
.LBB19_31:                              # %while.body.i
                                        #   in Loop: Header=BB19_32 Depth=2
	movq	%rbx, %rdi
	callq	dbxout_symbol
	movq	(%rbx), %rbx
	addq	%rax, %r15
.LBB19_32:                              # %while.body.i
                                        #   Parent Loop BB19_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	jne	.LBB19_31
.LBB19_33:                              # %if.end19
                                        #   in Loop: Header=BB19_28 Depth=1
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB19_35
# BB#34:                                # %if.then21
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	dbxout_reg_parms
.LBB19_35:                              # %if.end22
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	$32, %rcx
	movq	48(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rbx
	jle	.LBB19_37
# BB#36:                                # %if.end22
                                        #   in Loop: Header=BB19_28 Depth=1
	movabsq	$1, %rbx
.LBB19_37:                              # %if.end22
                                        #   in Loop: Header=BB19_28 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	movabsq	$0, %rax
	je	.LBB19_39
# BB#38:                                # %if.end22
                                        #   in Loop: Header=BB19_28 Depth=1
	movabsq	$1, %rax
.LBB19_39:                              # %if.end22
                                        #   in Loop: Header=BB19_28 Depth=1
	andq	%rax, %rbx
	movq	%r14, %r15
	je	.LBB19_40
# BB#41:                                # %if.then26
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movl	24(%r13), %r8d
	movq	$2, %rcx
	shrq	%cl, %r8
	movq	%r8, 16(%rsp)           # 8-byte Spill
	xorq	%rax, %rax
	leaq	96(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str64, %rdx
	movq	%r8, %rcx
	callq	sprintf
	movzbl	24(%r13), %eax
	testq	$1, %rax
	je	.LBB19_45
# BB#42:                                # %if.then35
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	32(%r13), %r14
	jmp	.LBB19_44
	.align	16, 0x90
.LBB19_43:                              # %while.body41
                                        #   in Loop: Header=BB19_44 Depth=2
	movq	72(%r14), %rax
	movq	asmfile(%rip), %rdi
	movq	32(%rax), %rcx
	movq	$84, %r8
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	(%r14), %r14
.LBB19_44:                              # %while.body41
                                        #   Parent Loop BB19_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r14, %r14
	jne	.LBB19_43
.LBB19_45:                              # %if.end55
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	asmfile(%rip), %rdi
	movq	$192, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str66, %rsi
	movabsq	$.L.str67, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$45, %rdi
	callq	putc
	movq	asmfile(%rip), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	40(%r13), %rdi
	xorq	%rdx, %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	dbxout_block
	testq	$1, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB19_47
# BB#46:                                # %do.body62
                                        #   in Loop: Header=BB19_28 Depth=1
	xorq	%rax, %rax
	leaq	64(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str68, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	sprintf
	movq	asmfile(%rip), %rdi
	movq	$224, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str66, %rsi
	movabsq	$.L.str67, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$45, %rdi
	callq	putc
	movq	asmfile(%rip), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	jmp	.LBB19_47
	.align	16, 0x90
.LBB19_40:                              # %if.end55.thread
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	40(%r13), %rdi
	xorq	%rdx, %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	dbxout_block
.LBB19_47:                              # %if.end71
                                        #   in Loop: Header=BB19_28 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB19_28
	jmp	.LBB19_48
.LBB19_7:
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB19_8:                               # %while.body.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_11 Depth 2
                                        #     Child Loop BB19_23 Depth 2
	movl	16(%r13), %eax
	andq	$81920, %rax            # imm = 0x14000
	cmpq	$81920, %rax            # imm = 0x14000
	jne	.LBB19_26
# BB#9:                                 # %if.then17.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	%r15, %r14
	movq	32(%r13), %rbx
	xorq	%r15, %r15
	jmp	.LBB19_11
	.align	16, 0x90
.LBB19_10:                              # %while.body.i.us
                                        #   in Loop: Header=BB19_11 Depth=2
	movq	%rbx, %rdi
	callq	dbxout_symbol
	movq	(%rbx), %rbx
	addq	%rax, %r15
.LBB19_11:                              # %while.body.i.us
                                        #   Parent Loop BB19_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	jne	.LBB19_10
# BB#12:                                # %if.end19.us
                                        #   in Loop: Header=BB19_8 Depth=1
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB19_14
# BB#13:                                # %if.then21.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	dbxout_reg_parms
.LBB19_14:                              # %if.end22.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	$32, %rcx
	movq	48(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rbx
	jle	.LBB19_16
# BB#15:                                # %if.end22.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movabsq	$1, %rbx
.LBB19_16:                              # %if.end22.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	movabsq	$0, %rax
	je	.LBB19_18
# BB#17:                                # %if.end22.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movabsq	$1, %rax
.LBB19_18:                              # %if.end22.us
                                        #   in Loop: Header=BB19_8 Depth=1
	andq	%rax, %rbx
	movq	%r14, %r15
	je	.LBB19_19
# BB#20:                                # %if.then26.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movl	24(%r13), %r8d
	movq	$2, %rcx
	shrq	%cl, %r8
	movq	%r8, 16(%rsp)           # 8-byte Spill
	xorq	%rax, %rax
	leaq	96(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str64, %rdx
	movq	%r8, %rcx
	callq	sprintf
	movzbl	24(%r13), %eax
	testq	$1, %rax
	je	.LBB19_24
# BB#21:                                # %if.then35.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	32(%r13), %r14
	jmp	.LBB19_23
	.align	16, 0x90
.LBB19_22:                              # %while.body41.us
                                        #   in Loop: Header=BB19_23 Depth=2
	movq	72(%r14), %rax
	movq	asmfile(%rip), %rdi
	movq	32(%rax), %rcx
	movq	$84, %r8
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	(%r14), %r14
.LBB19_23:                              # %while.body41.us
                                        #   Parent Loop BB19_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r14, %r14
	jne	.LBB19_22
.LBB19_24:                              # %if.end55.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	asmfile(%rip), %rdi
	movq	$192, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str66, %rsi
	movabsq	$.L.str67, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$45, %rdi
	callq	putc
	movq	asmfile(%rip), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	40(%r13), %rdi
	xorq	%rdx, %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	dbxout_block
	testq	$1, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB19_26
# BB#25:                                # %do.body62.us
                                        #   in Loop: Header=BB19_8 Depth=1
	xorq	%rax, %rax
	leaq	64(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str59, %rsi
	movabsq	$.L.str68, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	sprintf
	movq	asmfile(%rip), %rdi
	movq	$224, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str66, %rsi
	movabsq	$.L.str67, %rdx
	callq	fprintf
	movq	asmfile(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$45, %rdi
	callq	putc
	movq	asmfile(%rip), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	assemble_name
	movq	asmfile(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	jmp	.LBB19_26
	.align	16, 0x90
.LBB19_19:                              # %if.end55.us.thread
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	40(%r13), %rdi
	xorq	%rdx, %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	dbxout_block
.LBB19_26:                              # %if.end71.us
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB19_8
.LBB19_48:                              # %while.end74
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp212:
	.size	dbxout_block, .Ltmp212-dbxout_block
	.cfi_endproc

	.align	16, 0x90
	.type	dbxout_typedefs,@function
dbxout_typedefs:                        # @dbxout_typedefs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp215:
	.cfi_def_cfa_offset 16
.Ltmp216:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB20_6
# BB#1:                                 # %if.then
	movq	(%rbx), %rdi
	callq	dbxout_typedefs
	movzbl	16(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB20_6
# BB#2:                                 # %if.then2
	movq	8(%rbx), %rcx
	movq	96(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB20_6
# BB#3:                                 # %land.lhs.true
	movl	16(%rdi), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$33, %rdx
	jne	.LBB20_6
# BB#4:                                 # %land.lhs.true14
	cmpq	$0, 32(%rcx)
	je	.LBB20_6
# BB#5:                                 # %land.lhs.true14
	andq	$16384, %rax            # imm = 0x4000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB20_6
# BB#7:                                 # %if.then24
	popq	%rbx
	jmp	dbxout_symbol  # TAILCALL
.LBB20_6:                               # %if.end28
	popq	%rbx
	retq
.Ltmp217:
	.size	dbxout_typedefs, .Ltmp217-dbxout_typedefs
	.cfi_endproc

	.type	dbx_debug_hooks,@object # @dbx_debug_hooks
	.data
	.globl	dbx_debug_hooks
	.align	8
dbx_debug_hooks:
	.quad	dbxout_init
	.quad	dbxout_finish
	.quad	debug_nothing_int_charstar
	.quad	debug_nothing_int_charstar
	.quad	dbxout_start_source_file
	.quad	dbxout_end_source_file
	.quad	dbxout_begin_block
	.quad	dbxout_end_block
	.quad	debug_true_tree
	.quad	dbxout_source_line
	.quad	dbxout_source_line
	.quad	debug_nothing_int
	.quad	debug_nothing_void
	.quad	dbxout_begin_function
	.quad	debug_nothing_int
	.quad	dbxout_function_decl
	.quad	dbxout_global_decl
	.quad	debug_nothing_tree
	.quad	debug_nothing_tree
	.quad	debug_nothing_rtx
	.size	dbx_debug_hooks, 160

	.type	current_sym_code,@object # @current_sym_code
	.local	current_sym_code
	.comm	current_sym_code,4,4
	.type	current_sym_value,@object # @current_sym_value
	.local	current_sym_value
	.comm	current_sym_value,4,4
	.type	current_sym_addr,@object # @current_sym_addr
	.local	current_sym_addr
	.comm	current_sym_addr,8,8
	.type	current_sym_nchars,@object # @current_sym_nchars
	.local	current_sym_nchars
	.comm	current_sym_nchars,4,4
	.type	asmfile,@object         # @asmfile
	.local	asmfile
	.comm	asmfile,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%s\"%s:%c"
	.size	.L.str, 9

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\t.stabs\t"
	.size	.L.str1, 9

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	",%s,%s"
	.size	.L.str2, 7

	.type	have_used_extensions,@object # @have_used_extensions
	.local	have_used_extensions
	.comm	have_used_extensions,1,1
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"%s\"%s:T"
	.size	.L.str3, 8

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%s\"%s:"
	.size	.L.str4, 7

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%s\" :T"
	.size	.L.str5, 7

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"dbxout.c"
	.size	.L.str6, 9

	.type	.L__FUNCTION__.dbxout_symbol,@object # @__FUNCTION__.dbxout_symbol
.L__FUNCTION__.dbxout_symbol:
	.asciz	"dbxout_symbol"
	.size	.L__FUNCTION__.dbxout_symbol, 14

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s\"%s:c=i"
	.size	.L.str7, 10

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"%lld"
	.size	.L.str8, 5

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\",0x%x,0,0,0\n"
	.size	.L.str9, 14

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%s\"(anon):%c"
	.size	.L.str10, 13

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"(anon)"
	.size	.L.str11, 7

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"%s\"%s:v"
	.size	.L.str12, 8

	.type	typevec,@object         # @typevec
	.comm	typevec,8,8
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"\",%d,0,%d,"
	.size	.L.str13, 11

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"%d"
	.size	.L.str14, 3

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"GNU C++"
	.size	.L.str15, 8

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"$real"
	.size	.L.str16, 6

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"$imag"
	.size	.L.str17, 6

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"%s\"%s%s:"
	.size	.L.str18, 9

	.type	.L.str19,@object        # @.str19
.L.str19:
	.zero	1
	.size	.L.str19, 1

	.type	dbxout_type.anonymous_type_number,@object # @dbxout_type.anonymous_type_number
	.local	dbxout_type.anonymous_type_number
	.comm	dbxout_type.anonymous_type_number,4,4
	.type	next_type_number,@object # @next_type_number
	.local	next_type_number
	.comm	next_type_number,4,4
	.type	typevec_len,@object     # @typevec_len
	.local	typevec_len
	.comm	typevec_len,4,4
	.type	current_file,@object    # @current_file
	.local	current_file
	.comm	current_file,8,8
	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	";0;127;"
	.size	.L.str22, 8

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"@s%d;"
	.size	.L.str23, 6

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	";0;"
	.size	.L.str25, 4

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"@s"
	.size	.L.str26, 3

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	";-20;"
	.size	.L.str27, 6

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	";0;%d;"
	.size	.L.str28, 7

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	";-16;"
	.size	.L.str29, 6

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"eFalse:0,True:1,;"
	.size	.L.str30, 18

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"real:"
	.size	.L.str31, 6

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	",0,%d;"
	.size	.L.str32, 7

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"imag:"
	.size	.L.str33, 6

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	",%d,%d;;"
	.size	.L.str34, 9

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"@S;"
	.size	.L.str35, 4

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	";@S;S"
	.size	.L.str36, 6

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"ar"
	.size	.L.str37, 3

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	";0;-1;"
	.size	.L.str38, 7

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"xs"
	.size	.L.str40, 3

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"xu"
	.size	.L.str41, 3

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"$$%d"
	.size	.L.str42, 5

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"!%d,"
	.size	.L.str44, 5

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"xe"
	.size	.L.str45, 3

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"%s:"
	.size	.L.str46, 4

	.type	.L__FUNCTION__.dbxout_type,@object # @__FUNCTION__.dbxout_type
.L__FUNCTION__.dbxout_type:
	.asciz	"dbxout_type"
	.size	.L__FUNCTION__.dbxout_type, 12

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"%s::"
	.size	.L.str47, 5

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	":%s;%c%c%c"
	.size	.L.str48, 11

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	":%s;"
	.size	.L.str49, 5

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	",0,0;"
	.size	.L.str50, 6

	.type	.L__FUNCTION__.dbxout_type_name,@object # @__FUNCTION__.dbxout_type_name
.L__FUNCTION__.dbxout_type_name:
	.asciz	"dbxout_type_name"
	.size	.L__FUNCTION__.dbxout_type_name, 17

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"%o%01o"
	.size	.L.str53, 7

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"%01o"
	.size	.L.str54, 5

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	";0"
	.size	.L.str55, 3

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	";-1;"
	.size	.L.str56, 5

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"(%d,%d)"
	.size	.L.str57, 8

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"\t.text\n\t.stabs \"\",%d,0,0,%LLetext\n%LLetext:\n"
	.size	.L.str58, 45

	.type	dbxout_function_end.scope_labelno,@object # @dbxout_function_end.scope_labelno
	.local	dbxout_function_end.scope_labelno
	.comm	dbxout_function_end.scope_labelno,4,4
	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"*.%s%u"
	.size	.L.str59, 7

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"Lscope"
	.size	.L.str60, 7

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	".%s%u:\n"
	.size	.L.str61, 8

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"%s\"\",%d,0,0,"
	.size	.L.str62, 13

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"LBB"
	.size	.L.str64, 4

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"%s\"%s:C1\",%d,0,0,"
	.size	.L.str65, 18

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"%s%d,0,0,"
	.size	.L.str66, 10

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"\t.stabn\t"
	.size	.L.str67, 9

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"LBE"
	.size	.L.str68, 4

	.type	dbxout_source_line.sym_lineno,@object # @dbxout_source_line.sym_lineno
	.data
	.align	4
dbxout_source_line.sym_lineno:
	.long	1                       # 0x1
	.size	dbxout_source_line.sym_lineno, 4

	.type	.L.str69,@object        # @.str69
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str69:
	.asciz	"LM"
	.size	.L.str69, 3

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"\t.stabn 68,0,%d,"
	.size	.L.str70, 17

	.type	lastfile,@object        # @lastfile
	.local	lastfile
	.comm	lastfile,8,8
	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"Ltext"
	.size	.L.str71, 6

	.type	source_label_number,@object # @source_label_number
	.data
	.align	4
source_label_number:
	.long	1                       # 0x1
	.size	source_label_number, 4

	.type	.L.str72,@object        # @.str72
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str72:
	.asciz	",%d,0,0,"
	.size	.L.str72, 9

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"%s%d,0,0,0\n"
	.size	.L.str73, 12

	.type	next_file_number,@object # @next_file_number
	.local	next_file_number
	.comm	next_file_number,4,4
	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	",%d,0,0,0\n"
	.size	.L.str74, 11

	.type	cwd,@object             # @cwd
	.local	cwd
	.comm	cwd,8,8
	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"/"
	.size	.L.str75, 2

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"%s\"%s\",%d,0,0,0\n"
	.size	.L.str76, 17

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"gcc2_compiled."
	.size	.L.str77, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
