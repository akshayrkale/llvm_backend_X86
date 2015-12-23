	.file	"c-lex.c.bc"
	.text
	.globl	init_c_lex
	.align	16, 0x90
	.type	init_c_lex,@function
init_c_lex:                             # @init_c_lex
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$strcmp, %rdi
	movabsq	$free, %rdx
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	splay_tree_new
	movq	%rax, file_info_tree(%rip)
	movabsq	$.L.str, %rsi
	movq	%rax, %rdi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB0_2
# BB#1:                                 # %if.then.i
	movq	8(%rax), %rbx
	jmp	.LBB0_3
.LBB0_2:                                # %if.end.i
	movq	$8, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	file_info_tree(%rip), %rdi
	movl	%r15d, (%rbx)
	movq	$65536, %rax            # imm = 0x10000
	movl	%eax, 4(%rbx)
	movabsq	$.L.str, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_insert
.LBB0_3:                                # %get_fileinfo.exit
	movl	flag_detailed_statistics(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_5
# BB#4:                                 # %if.then
	xorq	%rax, %rax
	movl	%eax, header_time(%rip)
	callq	get_run_time
	movl	%eax, body_time(%rip)
	movl	%eax, (%rbx)
.LBB0_5:                                # %if.end
	movq	parse_in(%rip), %rdi
	callq	cpp_get_callbacks
	movq	$cb_line_change, (%rax)
	movq	$cb_ident, 40(%rax)
	movq	$cb_file_change, 8(%rax)
	movq	$cb_def_pragma, 48(%rax)
	movl	debug_info_level(%rip), %ecx
	cmpq	$3, %rcx
	jne	.LBB0_9
# BB#6:                                 # %land.lhs.true
	movl	write_symbols(%rip), %ecx
	cmpq	$7, %rcx
	ja	.LBB0_9
# BB#7:                                 # %land.lhs.true
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$152, %rdx
	je	.LBB0_9
# BB#8:                                 # %if.then12
	movq	$cb_define, 24(%rax)
	movq	$cb_undef, 32(%rax)
.LBB0_9:                                # %if.end13
	xorq	%rax, %rax
	movl	%eax, lineno(%rip)
	testq	%r14, %r14
	je	.LBB0_11
# BB#10:                                # %lor.lhs.false16
	movabsq	$.L.str1, %rsi
	movq	%r14, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_12
.LBB0_11:                               # %if.then19
	movabsq	$.L.str2, %r14
.LBB0_12:                               # %if.end20
	movq	parse_in(%rip), %rdi
	movq	ident_hash(%rip), %rdx
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	cpp_read_main_file  # TAILCALL
.Ltmp10:
	.size	init_c_lex, .Ltmp10-init_c_lex
	.cfi_endproc

	.globl	get_fileinfo
	.align	16, 0x90
	.type	get_fileinfo,@function
get_fileinfo:                           # @get_fileinfo
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp14:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp15:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp16:
	.cfi_def_cfa_offset 32
.Ltmp17:
	.cfi_offset %rbx, -24
.Ltmp18:
	.cfi_offset %r14, -16
	movq	file_info_tree(%rip), %rax
	movq	%rdi, %r14
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB1_2
# BB#1:                                 # %if.then
	movq	8(%rax), %rbx
	jmp	.LBB1_3
.LBB1_2:                                # %if.end
	movq	$8, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	file_info_tree(%rip), %rdi
	movl	%eax, (%rbx)
	movq	$65536, %rax            # imm = 0x10000
	movl	%eax, 4(%rbx)
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_insert
.LBB1_3:                                # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp19:
	.size	get_fileinfo, .Ltmp19-get_fileinfo
	.cfi_endproc

	.align	16, 0x90
	.type	cb_line_change,@function
cb_line_change:                         # @cb_line_change
	.cfi_startproc
# BB#0:                                 # %entry
	movq	map(%rip), %rax
	movl	(%rsi), %ecx
	movl	8(%rax), %edx
	movl	12(%rax), %eax
	addq	%rcx, %rdx
	subq	%rax, %rdx
	movl	%edx, src_lineno(%rip)
	retq
.Ltmp20:
	.size	cb_line_change, .Ltmp20-cb_line_change
	.cfi_endproc

	.align	16, 0x90
	.type	cb_ident,@function
cb_ident:                               # @cb_ident
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp22:
	.cfi_def_cfa_offset 16
	movl	flag_no_ident(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB3_2:                                # %if.then
	movq	8(%rdx), %rdi
	movl	(%rdx), %esi
	xorq	%rdx, %rdx
	callq	lex_string
	movq	asm_out_file(%rip), %rdi
	movq	40(%rax), %rcx
	movabsq	$.L.str54, %rsi
	movabsq	$.L.str55, %rdx
	xorq	%rax, %rax
	popq	%r8
	jmp	fprintf  # TAILCALL
.Ltmp23:
	.size	cb_ident, .Ltmp23-cb_ident
	.cfi_endproc

	.align	16, 0x90
	.type	cb_file_change,@function
cb_file_change:                         # @cb_file_change
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp30:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp31:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp32:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp33:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp34:
	.cfi_def_cfa_offset 48
.Ltmp35:
	.cfi_offset %rbx, -48
.Ltmp36:
	.cfi_offset %r12, -40
.Ltmp37:
	.cfi_offset %r13, -32
.Ltmp38:
	.cfi_offset %r14, -24
.Ltmp39:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movl	8(%r13), %r14d
	movzbl	20(%r13), %eax
	movl	12(%r13), %ecx
	addq	%r14, %r14
	subq	%rcx, %r14
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$1, %rdx
	jne	.LBB4_1
# BB#8:                                 # %if.then28
	movl	c_header_level(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB4_13
# BB#9:                                 # %land.lhs.true
	decq	%rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	movl	%ecx, c_header_level(%rip)
	jne	.LBB4_13
# BB#10:                                # %if.then32
	andq	$65280, %rax            # imm = 0xFF00
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB4_12
# BB#11:                                # %if.then37
	movabsq	$.L.str53, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB4_12:                               # %if.end38
	movl	pending_lang_change(%rip), %eax
	decq	%rax
	movl	%eax, pending_lang_change(%rip)
.LBB4_13:                               # %if.end40
	callq	pop_srcloc
	movq	debug_hooks(%rip), %rax
	movq	%r14, %rdi
	callq	*40(%rax)
	jmp	.LBB4_14
.LBB4_1:                                # %entry
	testq	$255, %rax
	jne	.LBB4_14
# BB#2:                                 # %if.then
	cmpq	$0, map(%rip)
	je	.LBB4_3
# BB#4:                                 # %if.else
	movl	-16(%r13), %eax
	movl	-12(%r13), %edx
	leaq	-1(%rcx,%rax), %rbx
	subq	%rdx, %rbx
	movl	%ebx, lineno(%rip)
	movq	(%r13), %rdi
	movq	$1, %rsi
	callq	push_srcloc
	movl	indent_level(%rip), %eax
	movq	input_file_stack(%rip), %rcx
	movl	%eax, 20(%rcx)
	movq	debug_hooks(%rip), %rax
	movq	(%r13), %rsi
	movq	%rbx, %rdi
	callq	*32(%rax)
	movl	c_header_level(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_6
# BB#5:                                 # %if.then14
	incq	%rax
	movl	%eax, c_header_level(%rip)
	jmp	.LBB4_14
.LBB4_3:                                # %if.then4
	movq	(%r13), %rax
	movq	%rax, main_input_filename(%rip)
	jmp	.LBB4_14
.LBB4_6:                                # %if.else15
	movzbl	21(%r13), %eax
	cmpq	$2, %rax
	jne	.LBB4_14
# BB#7:                                 # %if.then18
	movl	pending_lang_change(%rip), %eax
	movq	$1, %rcx
	movl	%ecx, c_header_level(%rip)
	incq	%rax
	movl	%eax, pending_lang_change(%rip)
.LBB4_14:                               # %if.end42
	movl	flag_detailed_statistics(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_19
# BB#15:                                # %if.then.i
	movq	(%r13), %r12
	callq	get_run_time
	movq	file_info_tree(%rip), %rdi
	movq	%rax, %r15
	movq	%r12, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB4_17
# BB#16:                                # %if.then.i.i
	movq	8(%rax), %rbx
	jmp	.LBB4_18
.LBB4_17:                               # %if.end.i.i
	movq	$8, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	file_info_tree(%rip), %rdi
	movl	%eax, (%rbx)
	movq	$65536, %rax            # imm = 0x10000
	movl	%eax, 4(%rbx)
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_insert
.LBB4_18:                               # %get_fileinfo.exit.i
	movl	body_time(%rip), %eax
	movl	header_time(%rip), %ecx
	movq	%r15, %rdx
	subq	%rax, %rdx
	addq	%rdx, %rcx
	movl	%ecx, header_time(%rip)
	movl	(%rbx), %eax
	addq	%rdx, %rax
	movl	%eax, (%rbx)
	movl	%r15d, body_time(%rip)
.LBB4_19:                               # %update_header_times.exit
	movzbl	21(%r13), %eax
	testq	%rax, %rax
	je	.LBB4_20
# BB#21:                                # %update_header_times.exit
	movabsq	$1, %rax
	jmp	.LBB4_22
.LBB4_20:
	movabsq	$0, %rax
.LBB4_22:                               # %update_header_times.exit
	movl	%eax, in_system_header(%rip)
	movq	(%r13), %rax
	movq	%rax, input_filename(%rip)
	movl	%r14d, lineno(%rip)
	movq	%r13, map(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	extract_interface_info  # TAILCALL
.Ltmp40:
	.size	cb_file_change, .Ltmp40-cb_file_change
	.cfi_endproc

	.align	16, 0x90
	.type	cb_def_pragma,@function
cb_def_pragma:                          # @cb_def_pragma
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp46:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp48:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp50:
	.cfi_def_cfa_offset 48
.Ltmp51:
	.cfi_offset %rbx, -40
.Ltmp52:
	.cfi_offset %r12, -32
.Ltmp53:
	.cfi_offset %r14, -24
.Ltmp54:
	.cfi_offset %r15, -16
	movslq	warn_unknown_pragmas(%rip), %rax
	movslq	in_system_header(%rip), %rcx
	movq	%rsi, %r14
	movq	%rdi, %rbx
	cmpq	%rcx, %rax
	jle	.LBB5_6
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$67, %rcx
	movabsq	$.L.str2, %r12
	jne	.LBB5_3
# BB#2:
	movq	%r12, %r15
	jmp	.LBB5_5
.LBB5_6:                                # %if.end12
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB5_3:                                # %if.then2
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$56, %rcx
	jne	.LBB5_5
# BB#4:                                 # %if.then9
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %r12
.LBB5_5:                                # %if.end11
	movq	map(%rip), %rax
	movl	8(%rax), %ecx
	movl	12(%rax), %eax
	addq	%r14, %rcx
	subq	%rax, %rcx
	movabsq	$.L.str52, %rdi
	xorq	%rax, %rax
	movl	%ecx, lineno(%rip)
	movq	%r15, %rsi
	movq	%r12, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	warning  # TAILCALL
.Ltmp55:
	.size	cb_def_pragma, .Ltmp55-cb_def_pragma
	.cfi_endproc

	.align	16, 0x90
	.type	cb_define,@function
cb_define:                              # @cb_define
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp61:
	.cfi_def_cfa_offset 32
.Ltmp62:
	.cfi_offset %rbx, -24
.Ltmp63:
	.cfi_offset %r14, -16
	movq	map(%rip), %rax
	movq	debug_hooks(%rip), %rcx
	movl	8(%rax), %ebx
	movl	12(%rax), %eax
	movq	16(%rcx), %r14
	addq	%rsi, %rbx
	subq	%rax, %rbx
	movq	%rdx, %rsi
	callq	cpp_macro_definition
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmpq	*%r14  # TAILCALL
.Ltmp64:
	.size	cb_define, .Ltmp64-cb_define
	.cfi_endproc

	.align	16, 0x90
	.type	cb_undef,@function
cb_undef:                               # @cb_undef
	.cfi_startproc
# BB#0:                                 # %entry
	movq	map(%rip), %rax
	movq	debug_hooks(%rip), %rcx
	movq	24(%rcx), %rcx
	movl	8(%rax), %edi
	movl	12(%rax), %r8d
	movq	8(%rdx), %rax
	addq	%rsi, %rdi
	subq	%r8, %rdi
	movq	%rax, %rsi
	jmpq	*%rcx  # TAILCALL
.Ltmp65:
	.size	cb_undef, .Ltmp65-cb_undef
	.cfi_endproc

	.globl	yyparse
	.align	16, 0x90
	.type	yyparse,@function
yyparse:                                # @yyparse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp67:
	.cfi_def_cfa_offset 16
	movq	debug_hooks(%rip), %rax
	movq	input_filename(%rip), %rsi
	movl	lineno(%rip), %edi
	callq	*32(%rax)
	movq	parse_in(%rip), %rdi
	callq	cpp_finish_options
	popq	%rax
	jmp	yyparse_1  # TAILCALL
.Ltmp68:
	.size	yyparse, .Ltmp68-yyparse
	.cfi_endproc

	.globl	dump_time_statistics
	.align	16, 0x90
	.type	dump_time_statistics,@function
dump_time_statistics:                   # @dump_time_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp74:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp75:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp76:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp77:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp78:
	.cfi_def_cfa_offset 48
.Ltmp79:
	.cfi_offset %rbx, -40
.Ltmp80:
	.cfi_offset %r12, -32
.Ltmp81:
	.cfi_offset %r14, -24
.Ltmp82:
	.cfi_offset %r15, -16
	movq	input_filename(%rip), %rbx
	movq	file_info_tree(%rip), %rdi
	movq	%rbx, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB9_2
# BB#1:                                 # %if.then.i
	movq	8(%rax), %r14
	jmp	.LBB9_3
.LBB9_2:                                # %if.end.i
	movq	$8, %rdi
	callq	xmalloc
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	file_info_tree(%rip), %rdi
	movl	%eax, (%r14)
	movq	$65536, %rax            # imm = 0x10000
	movl	%eax, 4(%r14)
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	splay_tree_insert
.LBB9_3:                                # %get_fileinfo.exit
	callq	get_run_time
	movl	body_time(%rip), %ecx
	movl	(%r14), %edx
	movq	%rax, %rbx
                                        # kill: RAX<def> RBX<kill>
	movq	stderr(%rip), %r15
	subq	%rcx, %rax
	addq	%rax, %rdx
	movl	%edx, (%r14)
	movabsq	$.L.str3, %r14
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r15, %rcx
	callq	fwrite
	movslq	header_time(%rip), %rsi
	movabsq	$.L.str4, %rdi
	callq	print_time
	movl	body_time(%rip), %eax
	movq	%rbx, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movabsq	$.L.str5, %rdi
	callq	print_time
	movl	header_time(%rip), %edi
	callq	int32_to_float64
	movl	body_time(%rip), %ecx
	movq	%rax, %r12
	subq	%rcx, %rbx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	callq	int32_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movq	$8, %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r15, %rcx
	callq	fwrite
	movq	file_info_tree(%rip), %rdi
	movabsq	$dump_one_header, %rsi
	xorq	%rdx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	splay_tree_foreach  # TAILCALL
.Ltmp83:
	.size	dump_time_statistics, .Ltmp83-dump_time_statistics
	.cfi_endproc

	.align	16, 0x90
	.type	dump_one_header,@function
dump_one_header:                        # @dump_one_header
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp85:
	.cfi_def_cfa_offset 16
	movq	8(%rdi), %rax
	movq	(%rdi), %rdi
	movslq	(%rax), %rsi
	callq	print_time
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp86:
	.size	dump_one_header, .Ltmp86-dump_one_header
	.cfi_endproc

	.globl	c_lex
	.align	16, 0x90
	.type	c_lex,@function
c_lex:                                  # @c_lex
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
	subq	$16, %rsp
.Ltmp98:
	.cfi_def_cfa_offset 64
.Ltmp99:
	.cfi_offset %rbx, -48
.Ltmp100:
	.cfi_offset %r12, -40
.Ltmp101:
	.cfi_offset %r13, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$.L.str8, %r12
	movabsq	$.L.str7, %r13
	jmp	.LBB11_1
.LBB11_8:                               # %retry
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	%rcx, %rsi
	callq	error
	.align	16, 0x90
.LBB11_1:                               # %retry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_2 Depth 2
	movq	$7, %rdi
	callq	timevar_push
	.align	16, 0x90
.LBB11_2:                               # %do.body
                                        #   Parent Loop BB11_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	parse_in(%rip), %rdi
	callq	cpp_get_token
	movq	%rax, %rbx
	movzbl	6(%rbx), %eax
	cmpq	$66, %rax
	je	.LBB11_2
# BB#3:                                 # %do.end
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	$7, %rdi
	callq	timevar_pop
	movl	src_lineno(%rip), %eax
	movl	%eax, lineno(%rip)
	movq	$0, (%r14)
	movzbl	6(%rbx), %ecx
	leaq	-44(%rcx), %rax
	andq	%r15, %rax
	cmpq	$21, %rax
	ja	.LBB11_31
# BB#4:                                 # %do.end
                                        #   in Loop: Header=BB11_1 Depth=1
	jmpq	*.LJTI11_0(,%rax,8)
.LBB11_6:                               # %sw.bb5
                                        #   in Loop: Header=BB11_1 Depth=1
	movzbl	8(%rbx), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %eax
	testq	$172, %rax
	jne	.LBB11_7
# BB#9:                                 # %if.else
                                        #   in Loop: Header=BB11_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	jmp	.LBB11_8
.LBB11_7:                               # %if.then
                                        #   in Loop: Header=BB11_1 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	jmp	.LBB11_8
.LBB11_29:                              # %sw.bb
	movl	indent_level(%rip), %eax
	incq	%rax
	jmp	.LBB11_30
.LBB11_5:                               # %sw.bb4
	movl	indent_level(%rip), %eax
	decq	%rax
.LBB11_30:                              # %sw.epilog
	movl	%eax, indent_level(%rip)
	jmp	.LBB11_31
.LBB11_10:                              # %sw.bb14
	movq	8(%rbx), %rax
	addq	$-24, %rax
	movq	%rax, (%r14)
	jmp	.LBB11_31
.LBB11_11:                              # %sw.bb16
	movq	16(%rbx), %rax
	movl	8(%rbx), %ecx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	lex_number
	movq	%rax, (%r14)
	jmp	.LBB11_31
.LBB11_12:                              # %sw.bb21
	movq	parse_in(%rip), %rax
	movl	flag_traditional(%rip), %ecx
	movl	warn_multichar(%rip), %edx
	leaq	12(%rsp), %r8
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	cpp_interpret_charconst
	movzbl	6(%rbx), %ecx
	cmpq	$59, %rcx
	jne	.LBB11_14
# BB#13:                                # %if.then.i
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_int_2_wide
	movabsq	$c_global_trees, %rcx
	jmp	.LBB11_23
.LBB11_24:                              # %sw.bb23
	movq	16(%rbx), %rax
	cmpq	$62, %rcx
	jne	.LBB11_25
# BB#26:                                # %sw.bb23
	movabsq	$1, %rcx
	jmp	.LBB11_27
.LBB11_28:                              # %sw.bb36
	movabsq	$.L.str9, %rax
	movabsq	$.L__FUNCTION__.c_lex, %rcx
	movq	$822, %rsi              # imm = 0x336
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB11_14:                              # %if.else.i
	testq	%rax, %rax
	js	.LBB11_15
# BB#16:                                # %if.else6.i
	xorq	%rsi, %rsi
	jmp	.LBB11_17
.LBB11_25:
	movabsq	$0, %rcx
.LBB11_27:                              # %sw.bb23
	movl	8(%rbx), %edx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	lex_string
	movq	%rax, (%r14)
	jmp	.LBB11_31
.LBB11_15:                              # %if.then4.i
	movq	$-1, %rsi
.LBB11_17:                              # %if.end.i
	movq	%rax, %rdi
	callq	build_int_2_wide
	movl	c_language(%rip), %ecx
	cmpq	$1, %rcx
	jne	.LBB11_18
# BB#19:                                # %if.end.i
	movabsq	$1, %rcx
	jmp	.LBB11_20
.LBB11_18:
	movabsq	$0, %rcx
.LBB11_20:                              # %if.end.i
	movl	12(%rsp), %edx
	cmpq	$2, %rdx
	sbbq	%rdx, %rdx
	testq	%rcx, %rdx
	jne	.LBB11_21
# BB#22:                                # %if.end.i
	movabsq	$integer_types+40, %rcx
	jmp	.LBB11_23
.LBB11_21:
	movabsq	$integer_types, %rcx
.LBB11_23:                              # %lex_charconst.exit
	movq	c_global_trees+72(%rip), %rdx
	movq	(%rcx), %rcx
	movq	%rdx, 8(%rax)
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	convert
	movl	16(%rax), %ecx
	movabsq	$4294443007, %rdx       # imm = 0xFFF7FFFF
	andq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movq	%rax, (%r14)
.LBB11_31:                              # %sw.epilog
	movzbl	6(%rbx), %eax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	c_lex, .Ltmp104-c_lex
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI11_0:
	.quad	.LBB11_29
	.quad	.LBB11_5
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_31
	.quad	.LBB11_10
	.quad	.LBB11_11
	.quad	.LBB11_12
	.quad	.LBB11_12
	.quad	.LBB11_6
	.quad	.LBB11_24
	.quad	.LBB11_24
	.quad	.LBB11_28
	.quad	.LBB11_28
	.quad	.LBB11_28

	.text
	.align	16, 0x90
	.type	lex_number,@function
lex_number:                             # @lex_number
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp108:
	.cfi_def_cfa_offset 16
.Ltmp109:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp110:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$248, %rsp
.Ltmp111:
	.cfi_offset %rbx, -56
.Ltmp112:
	.cfi_offset %r12, -48
.Ltmp113:
	.cfi_offset %r13, -40
.Ltmp114:
	.cfi_offset %r14, -32
.Ltmp115:
	.cfi_offset %r15, -24
	movq	%rsi, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB12_1
# BB#5:                                 # %if.then
	movzbl	(%rdi), %eax
	movq	$56, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	cmpq	$49, %rax
	jne	.LBB12_6
# BB#295:                               # %if.then7
	movq	global_trees+96(%rip), %rbx
	jmp	.LBB12_294
.LBB12_1:                               # %for.cond.preheader
	movq	$0, -56(%rbp)
	movq	$0, -64(%rbp)
	movq	$0, -72(%rbp)
	movq	$0, -80(%rbp)
	movq	$0, -88(%rbp)
	movq	$0, -96(%rbp)
	movq	$0, -104(%rbp)
	movq	$0, -112(%rbp)
	movzbl	(%rdi), %eax
	andq	%r13, %rbx
	andq	$255, %rax
	movq	$10, %r14
	cmpq	$2, %rbx
	jbe	.LBB12_8
# BB#2:                                 # %land.lhs.true
	cmpq	$48, %rax
	movq	%rdi, %rdx
	jne	.LBB12_12
# BB#3:                                 # %land.lhs.true19
	movzbl	1(%rdi), %eax
	orq	$32, %rax
	andq	$255, %rax
	cmpq	$120, %rax
	jne	.LBB12_9
# BB#4:                                 # %if.then28
	leaq	2(%rdi), %rdx
	movq	$16, %r14
	jmp	.LBB12_12
.LBB12_6:                               # %if.then
	cmpq	$48, %rax
	je	.LBB12_110
# BB#7:                                 # %if.else8
	addq	$-48, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, %rbx
	jmp	.LBB12_294
.LBB12_8:                               # %if.else29
	cmpq	$48, %rax
	movq	%rdi, %rdx
	jne	.LBB12_12
.LBB12_9:                               # %land.lhs.true34
	movzbl	1(%rdi), %eax
	movzbl	_sch_istable(%rax,%rax), %r14d
	andq	$4, %r14
	movq	%rdi, %rdx
	je	.LBB12_11
# BB#10:                                # %land.lhs.true34
	leaq	1(%rdi), %rdx
.LBB12_11:                              # %land.lhs.true34
	movq	$1, %rcx
	shrq	%cl, %r14
	xorq	$10, %r14
.LBB12_12:                              # %do.body.preheader
	addq	%rdi, %rbx
	movq	%rdi, -216(%rbp)        # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r11, %r11
	movq	$0, -208(%rbp)          # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	$0, -200(%rbp)          # 8-byte Folded Spill
.LBB12_13:                              # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_32 Depth 2
	movq	%rdx, %rdi
	movsbq	(%rdi), %r15
	movq	%r15, %rcx
	andq	$255, %rcx
	cmpq	$95, %rcx
	je	.LBB12_23
# BB#14:                                # %do.body
                                        #   in Loop: Header=BB12_13 Depth=1
	leaq	1(%rdi), %rdx
	cmpq	$46, %rcx
	jne	.LBB12_24
# BB#15:                                # %if.then48
                                        #   in Loop: Header=BB12_13 Depth=1
	andq	%r13, %r12
	cmpq	$2, %r12
	je	.LBB12_19
# BB#16:                                # %if.then48
                                        #   in Loop: Header=BB12_13 Depth=1
	cmpq	$1, %r12
	je	.LBB12_17
# BB#20:                                # %if.else60
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	movq	$10, %rcx
	je	.LBB12_22
# BB#21:                                # %if.else60
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	%r14, %rcx
.LBB12_22:                              # %if.else60
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	$1, %r12
	movq	%rcx, %r14
	jmp	.LBB12_52
	.align	16, 0x90
.LBB12_24:                              # %if.else74
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	%rsi, %r10
	movq	%rdx, %r9
	movq	%rbx, %r8
	movq	%r15, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %ebx
	testq	$4, %rbx
	jne	.LBB12_29
# BB#25:                                # %lor.lhs.false81
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	%r14, %rsi
	andq	%r13, %rsi
	cmpq	$16, %rsi
	movabsq	$0, %rax
	je	.LBB12_27
# BB#26:                                # %lor.lhs.false81
                                        #   in Loop: Header=BB12_13 Depth=1
	movabsq	$1, %rax
.LBB12_27:                              # %lor.lhs.false81
                                        #   in Loop: Header=BB12_13 Depth=1
	testq	$256, %rbx              # imm = 0x100
	je	.LBB12_34
# BB#28:                                # %lor.lhs.false81
                                        #   in Loop: Header=BB12_13 Depth=1
	testq	%rax, %rax
	jne	.LBB12_34
.LBB12_29:                              # %if.then91
                                        #   in Loop: Header=BB12_13 Depth=1
	movsbq	_hex_value(%rcx), %rdi
	movq	$32, %rcx
	movq	-200(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	jl	.LBB12_31
# BB#30:                                # %if.then91
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	%rdi, -200(%rbp)        # 8-byte Spill
.LBB12_31:                              # %if.then91
                                        #   in Loop: Header=BB12_13 Depth=1
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB12_32:                              # %for.body127
                                        #   Parent Loop BB12_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	-112(%rbp,%rbx,4), %esi
	imulq	%r14, %rsi
	movl	%esi, -112(%rbp,%rbx,4)
	testq	%r13, %rbx
	jne	.LBB12_33
# BB#47:                                # %if.else141
                                        #   in Loop: Header=BB12_32 Depth=2
	movl	-112(%rbp), %eax
	addq	%rdi, %rax
	movl	%eax, -112(%rbp)
	jmp	.LBB12_48
	.align	16, 0x90
.LBB12_33:                              # %if.then131
                                        #   in Loop: Header=BB12_32 Depth=2
	movl	-116(%rbp,%rbx,4), %edx
	movq	$8, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	addq	%rsi, %rax
	movl	%eax, -112(%rbp,%rbx,4)
	andq	$255, %rdx
	movl	%edx, -116(%rbp,%rbx,4)
.LBB12_48:                              # %for.inc145
                                        #   in Loop: Header=BB12_32 Depth=2
	incq	%rbx
	cmpq	$16, %rbx
	jne	.LBB12_32
# BB#49:                                # %for.end147
                                        #   in Loop: Header=BB12_13 Depth=1
	movl	-52(%rbp), %esi
	incq	%r11
	cmpq	$256, %rsi              # imm = 0x100
	jb	.LBB12_51
# BB#50:                                # %if.then151
                                        #   in Loop: Header=BB12_13 Depth=1
	andq	$255, %rsi
	movl	%esi, -52(%rbp)
	movq	$1, -208(%rbp)          # 8-byte Folded Spill
.LBB12_51:                              # %do.cond157
                                        #   in Loop: Header=BB12_13 Depth=1
	movq	%r8, %rbx
	movq	%r9, %rdx
.LBB12_52:                              # %do.cond157
                                        #   in Loop: Header=BB12_13 Depth=1
	cmpq	%rbx, %rdx
	jb	.LBB12_13
	jmp	.LBB12_53
.LBB12_19:                              # %do.body57
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_23:                              # %do.body71
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
.LBB12_109:                             # %syntax_error
	callq	error
.LBB12_110:                             # %syntax_error
	movq	global_trees+88(%rip), %rbx
.LBB12_294:                             # %cleanup745
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB12_34:                              # %if.else96
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r15, %rcx
	andq	$223, %rcx
	cmpq	$10, %rax
	jg	.LBB12_37
# BB#35:                                # %if.else96
	cmpq	$69, %rcx
	jne	.LBB12_37
# BB#36:
	movq	$2, %r12
	movq	$10, %r14
	movq	%r8, %rbx
	jmp	.LBB12_46
.LBB12_17:                              # %do.body52
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_37:                              # %if.else106
	cmpq	$16, %rsi
	movabsq	$0, %rax
	movabsq	$1, %rbx
	movq	%rax, %rsi
	jne	.LBB12_39
# BB#38:                                # %if.else106
	movq	%rbx, %rsi
.LBB12_39:                              # %if.else106
	cmpq	$80, %rcx
	jne	.LBB12_41
# BB#40:                                # %if.else106
	movq	%rbx, %rax
.LBB12_41:                              # %if.else106
	andq	%rax, %rsi
	jne	.LBB12_43
# BB#42:                                # %if.else106
	movq	%rdi, %r9
.LBB12_43:                              # %if.else106
	testq	%rsi, %rsi
	movq	$2, %rax
	movq	%r8, %rbx
	jne	.LBB12_45
# BB#44:                                # %if.else106
	movq	%r12, %rax
.LBB12_45:                              # %if.else106
	movq	%rax, %r12
.LBB12_46:                              # %do.end161
	movq	%r9, %rdx
	movq	%r10, %rsi
.LBB12_53:                              # %do.end161
	testq	%r13, %r11
	je	.LBB12_54
# BB#55:                                # %if.end168
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	-200(%rbp), %rdi        # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jge	.LBB12_56
# BB#57:                                # %if.end175
	testq	%r13, %r12
	je	.LBB12_58
# BB#60:                                # %if.then178
	movq	%rdx, -200(%rbp)        # 8-byte Spill
	movq	%rbx, -224(%rbp)        # 8-byte Spill
	movq	%r14, %rbx
	andq	%r13, %rbx
	cmpq	$16, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB12_62
# BB#61:                                # %if.then178
	movq	%rdx, %rcx
.LBB12_62:                              # %if.then178
	movl	pedantic(%rip), %esi
	testq	%rsi, %rsi
	jne	.LBB12_64
# BB#63:                                # %if.then178
	movq	%rdx, %rax
.LBB12_64:                              # %if.then178
	movl	flag_isoc99(%rip), %edx
	testq	%rdx, %rdx
	jne	.LBB12_67
# BB#65:                                # %if.then178
	orq	%rax, %rcx
	jne	.LBB12_67
# BB#66:                                # %if.then185
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB12_67:                              # %if.end186
	cmpq	$16, %rbx
	jne	.LBB12_70
# BB#68:                                # %if.end186
	andq	%r13, %r12
	cmpq	$2, %r12
	je	.LBB12_70
# BB#69:                                # %do.body193
	movabsq	$.L.str16, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_54:                              # %do.body165
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_56:                              # %do.body172
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_58:                              # %while.cond373.preheader
	movq	%rsi, -264(%rbp)        # 8-byte Spill
	movq	$0, -256(%rbp)          # 8-byte Folded Spill
	cmpq	%rbx, %rdx
	jae	.LBB12_59
# BB#140:
	movabsq	$12884901891, %r15      # imm = 0x300000003
	movabsq	$0, %rax
	movq	%rax, -280(%rbp)        # 8-byte Spill
	movabsq	$0, %rsi
	movq	$0, -248(%rbp)          # 8-byte Folded Spill
	movq	$0, -232(%rbp)          # 8-byte Folded Spill
	movq	$0, -240(%rbp)          # 8-byte Folded Spill
.LBB12_141:                             # %while.body378
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%rdx), %r12
	leaq	-73(%r12), %rcx
	andq	%r13, %rcx
	cmpq	$44, %rcx
	ja	.LBB12_187
# BB#142:                               # %while.body378
                                        #   in Loop: Header=BB12_141 Depth=1
	incq	%rdx
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	je	.LBB12_143
# BB#183:                               # %sw.bb423
                                        #   in Loop: Header=BB12_141 Depth=1
	testq	%r13, %rsi
	jne	.LBB12_184
# BB#185:                               # %if.else426
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rsi
	je	.LBB12_161
# BB#186:                               # %if.then428
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str23, %rdi
	movq	%rdx, %r12
	callq	pedwarn
	movq	$1, %rsi
	movq	%r12, %rdx
	jmp	.LBB12_161
.LBB12_143:                             # %while.body378
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$34359738376, %rdi      # imm = 0x800000008
	testq	%rdi, %rax
	je	.LBB12_144
# BB#155:                               # %sw.bb397
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	-232(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_157
# BB#156:                               #   in Loop: Header=BB12_141 Depth=1
	movq	%r12, -232(%rbp)        # 8-byte Spill
	jmp	.LBB12_161
.LBB12_184:                             # %if.then425
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str33, %rdi
	movq	%rdx, %r12
	callq	error
	movq	%r12, %rdx
	movq	$1, %rsi
	jmp	.LBB12_161
.LBB12_144:                             # %while.body378
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$17592186048512, %rcx   # imm = 0x100000001000
	testq	%rcx, %rax
	je	.LBB12_187
# BB#145:                               # %sw.bb381
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	%rsi, %r15
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_146
# BB#147:                               # %if.else384
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_152
# BB#148:                               # %if.else384
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	in_system_header(%rip), %eax
	testq	%r13, %rax
	jne	.LBB12_152
# BB#149:                               # %land.lhs.true388
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	parse_in(%rip), %rdi
	movq	%rdx, %r12
	callq	cpp_sys_macro_p
	movq	%r12, %rdx
	testq	%r13, %rax
	jne	.LBB12_152
# BB#150:                               # %if.then391
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str28, %rdi
	movq	%rdx, %r12
	callq	warning
	jmp	.LBB12_151
.LBB12_157:                             # %if.then399
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	%rsi, -272(%rbp)        # 8-byte Spill
	movq	%rdx, -200(%rbp)        # 8-byte Spill
	movq	-248(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_158
# BB#170:                               # %if.else402
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	-280(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_171
# BB#172:                               # %if.else405
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	-232(%rbp), %rax        # 8-byte Reload
	andq	%r13, %rax
	movq	%r12, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	jne	.LBB12_173
# BB#174:                               # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rsi
	movq	%rsi, %rax
	je	.LBB12_176
# BB#175:                               # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movabsq	$1, %rax
.LBB12_176:                             # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	%rsi, %rcx
	jne	.LBB12_178
# BB#177:                               # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movabsq	$1, %rcx
.LBB12_178:                             # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	in_system_header(%rip), %edx
	testq	%rdx, %rdx
	movq	%rsi, %rdx
	jne	.LBB12_180
# BB#179:                               # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movabsq	$1, %rdx
.LBB12_180:                             # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	movl	warn_long_long(%rip), %esi
	movq	$0, -280(%rbp)          # 8-byte Folded Spill
	testq	%rsi, %rsi
	movq	$1, -248(%rbp)          # 8-byte Folded Spill
	je	.LBB12_160
# BB#181:                               # %if.else409
                                        #   in Loop: Header=BB12_141 Depth=1
	andq	%rcx, %rax
	andq	%rdx, %rax
	je	.LBB12_160
# BB#182:                               # %if.then417
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movq	$0, -280(%rbp)          # 8-byte Folded Spill
	movabsq	$.L.str32, %rcx
	movq	%rcx, %rdi
	callq	pedwarn
	jmp	.LBB12_160
.LBB12_146:                             # %if.then383
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str27, %rdi
	movq	%rdx, %r12
	callq	error
.LBB12_151:                             # %if.end393
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	%r12, %rdx
.LBB12_152:                             # %if.end393
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	-232(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	movq	$1, -240(%rbp)          # 8-byte Folded Spill
	je	.LBB12_154
# BB#153:                               # %if.end393
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	$1, -280(%rbp)          # 8-byte Folded Spill
.LBB12_154:                             # %if.end393
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	%r15, %rsi
	movabsq	$12884901891, %r15      # imm = 0x300000003
	jmp	.LBB12_161
.LBB12_158:                             # %if.then401
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str29, %rdi
	jmp	.LBB12_159
.LBB12_171:                             # %if.then404
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str30, %rdi
	jmp	.LBB12_159
.LBB12_173:                             # %if.then408
                                        #   in Loop: Header=BB12_141 Depth=1
	xorq	%rax, %rax
	movq	$0, -280(%rbp)          # 8-byte Folded Spill
	movabsq	$.L.str31, %rdi
.LBB12_159:                             # %while.cond373.backedge
                                        #   in Loop: Header=BB12_141 Depth=1
	callq	error
	movq	$1, -248(%rbp)          # 8-byte Folded Spill
	movq	%r12, -232(%rbp)        # 8-byte Spill
.LBB12_160:                             # %while.cond373.backedge
                                        #   in Loop: Header=BB12_141 Depth=1
	movq	-200(%rbp), %rdx        # 8-byte Reload
	movq	-272(%rbp), %rsi        # 8-byte Reload
.LBB12_161:                             # %while.cond373.backedge
                                        #   in Loop: Header=BB12_141 Depth=1
	cmpq	%rbx, %rdx
	jb	.LBB12_141
	jmp	.LBB12_162
.LBB12_70:                              # %if.end196
	cmpq	$16, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB12_72
# BB#71:                                # %if.end196
	movq	%rdx, %rcx
.LBB12_72:                              # %if.end196
	cmpq	$10, %rbx
	movq	%rax, %rsi
	jne	.LBB12_74
# BB#73:                                # %if.end196
	movq	%rdx, %rsi
.LBB12_74:                              # %if.end196
	movq	%r15, %rbx
	andq	$223, %rbx
	cmpq	$69, %rbx
	movq	%rax, %rdi
	jne	.LBB12_76
# BB#75:                                # %if.end196
	movq	%rdx, %rdi
.LBB12_76:                              # %if.end196
	cmpq	$80, %rbx
	jne	.LBB12_78
# BB#77:                                # %if.end196
	movq	%rdx, %rax
.LBB12_78:                              # %if.end196
	testq	%rdi, %rsi
	jne	.LBB12_81
# BB#79:                                # %if.end196
	andq	%rax, %rcx
	jne	.LBB12_81
# BB#80:
	movq	-216(%rbp), %rsi        # 8-byte Reload
	movq	-200(%rbp), %r15        # 8-byte Reload
	jmp	.LBB12_97
.LBB12_59:
	xorq	%rsi, %rsi
	movq	$0, -248(%rbp)          # 8-byte Folded Spill
	movq	$0, -232(%rbp)          # 8-byte Folded Spill
	movq	$0, -240(%rbp)          # 8-byte Folded Spill
.LBB12_162:                             # %while.end436
	movq	%rsi, -272(%rbp)        # 8-byte Spill
	movq	%rdx, -200(%rbp)        # 8-byte Spill
	movq	%rbx, -224(%rbp)        # 8-byte Spill
	movq	-208(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	je	.LBB12_164
# BB#163:                               # %if.then438
	movabsq	$.L.str35, %rdi
	movq	$128, %rsi
	xorq	%rax, %rax
	callq	pedwarn
	movq	$1, -256(%rbp)          # 8-byte Folded Spill
.LBB12_164:                             # %if.end439
	movl	-80(%rbp), %eax
	movl	-112(%rbp), %edx
	movl	-76(%rbp), %esi
	movq	$8, %rcx
	shlq	%cl, %rsi
	orq	%rax, %rsi
	movl	-108(%rbp), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movl	-72(%rbp), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	-104(%rbp), %esi
	movq	$16, %rcx
	shlq	%cl, %rsi
	orq	%rax, %rsi
	movl	-68(%rbp), %eax
	movq	$24, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movl	-100(%rbp), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	-64(%rbp), %esi
	movq	$32, %rcx
	shlq	%cl, %rsi
	orq	%rax, %rsi
	movl	-96(%rbp), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movl	-60(%rbp), %edx
	movq	$40, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movl	-92(%rbp), %esi
	movq	$40, %rcx
	shlq	%cl, %rsi
	orq	%rax, %rsi
	movl	-56(%rbp), %eax
	movq	$48, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movl	-88(%rbp), %edx
	movq	$48, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	movq	$56, %rcx
	movl	-84(%rbp), %edi
	movq	-264(%rbp), %rsi        # 8-byte Reload
	shlq	%cl, %rsi
	orq	%rax, %rsi
	movq	$56, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	callq	build_int_2_wide
	movq	integer_types+80(%rip), %rcx
	movq	%rax, %rbx
	movq	%rcx, 8(%rbx)
	movl	warn_traditional(%rip), %eax
	movl	flag_traditional(%rip), %ecx
	orq	%rax, %rcx
	xorq	%rdi, %rdi
	testq	%r13, %rcx
	movabsq	$0, %r15
	je	.LBB12_210
# BB#165:                               # %if.then464
	movq	-232(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_189
# BB#166:                               # %if.then464
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	$10, %rax
	je	.LBB12_189
# BB#167:                               # %land.lhs.true469
	movq	integer_types+48(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	xorq	%rdi, %rdi
	testq	%r13, %rax
	je	.LBB12_189
# BB#168:                               # %if.then472
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_169
# BB#188:                               # %if.then472
	movabsq	$integer_types+40, %rax
	jmp	.LBB12_209
.LBB12_187:                             # %do.body432
	movabsq	$.L.str34, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_81:                              # %if.then214
	movq	-224(%rbp), %rcx        # 8-byte Reload
	movq	-200(%rbp), %rdx        # 8-byte Reload
	cmpq	%rcx, %rdx
	jae	.LBB12_83
# BB#82:                                # %if.then219
	movsbq	(%rdx), %r15
	incq	%rdx
.LBB12_83:                              # %if.end222
	cmpq	%rcx, %rdx
	jae	.LBB12_87
# BB#84:                                # %land.lhs.true227
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$43, %rax
	je	.LBB12_86
# BB#85:                                # %land.lhs.true227
	cmpq	$45, %rax
	jne	.LBB12_87
.LBB12_86:                              # %if.then233
	movsbq	(%rdx), %r15
	incq	%rdx
.LBB12_87:                              # %if.end236
	movq	%r15, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB12_90
# BB#88:                                # %while.cond.preheader
	cmpq	%rcx, %rdx
	jae	.LBB12_89
.LBB12_91:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB12_92
# BB#93:                                # %while.body
                                        #   in Loop: Header=BB12_91 Depth=1
	movsbq	(%rdx), %r15
	leaq	1(%rdx), %rax
	cmpq	%rcx, %rax
	movq	%rax, %rdx
	jb	.LBB12_91
	jmp	.LBB12_94
.LBB12_189:                             # %if.else474
	movq	-232(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_195
# BB#190:                               # %if.else474
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	$10, %rax
	jne	.LBB12_195
# BB#191:                               # %land.lhs.true479
	movq	integer_types+40(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	xorq	%rdi, %rdi
	testq	%r13, %rax
	je	.LBB12_195
# BB#192:                               # %if.then482
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_193
# BB#194:                               # %if.then482
	movabsq	$integer_types+40, %rax
	jmp	.LBB12_209
.LBB12_90:                              # %do.body244
	movabsq	$.L.str17, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_195:                             # %if.else488
	movq	-248(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	je	.LBB12_196
# BB#199:                               # %if.else496
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_200
# BB#201:                               # %if.else496
	movabsq	$integer_types+72, %rax
	jmp	.LBB12_202
.LBB12_89:
	movq	%rdx, %rax
	jmp	.LBB12_94
.LBB12_92:
	movq	%rdx, %rax
.LBB12_94:                              # %while.end
	andq	$255, %r15
	movzbl	_sch_istable(%r15,%r15), %ecx
	testq	$4, %rcx
	jne	.LBB12_96
# BB#95:
	decq	%rax
.LBB12_96:                              # %while.end
	movq	-216(%rbp), %rsi        # 8-byte Reload
	movq	%rax, %r15
.LBB12_97:                              # %if.end269
	movq	%r15, %r12
	subq	%rsi, %r12
	leaq	16(%r12), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, -208(%rbp)        # 8-byte Spill
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	memcpy
	xorq	%rsi, %rsi
	movq	-224(%rbp), %rax        # 8-byte Reload
	cmpq	%rax, %r15
	movb	%sil, (%r12,%rbx)
	movq	%r15, %rdx
	jae	.LBB12_98
# BB#99:
	movabsq	$103079215128, %r12     # imm = 0x1800000018
	xorq	%r15, %r15
	xorq	%rbx, %rbx
.LBB12_100:                             # %while.body283
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%rdx), %rcx
	addq	$-70, %rcx
	andq	%r13, %rcx
	cmpq	$38, %rcx
	ja	.LBB12_136
# BB#101:                               # %while.body283
                                        #   in Loop: Header=BB12_100 Depth=1
	incq	%rdx
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB12_102
# BB#132:                               # %sw.bb317
                                        #   in Loop: Header=BB12_100 Depth=1
	testq	%r13, %rbx
	jne	.LBB12_133
# BB#134:                               # %if.else323
                                        #   in Loop: Header=BB12_100 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB12_116
# BB#135:                               # %if.then325
                                        #   in Loop: Header=BB12_100 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str23, %rdi
	movq	%r15, -200(%rbp)        # 8-byte Spill
	movq	%r12, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	callq	pedwarn
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%r15, %r12
	movq	-200(%rbp), %r15        # 8-byte Reload
	movq	$1, %rbx
	jmp	.LBB12_116
.LBB12_102:                             # %while.body283
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4294967297, %rdi       # imm = 0x100000001
	testq	%rdi, %rax
	je	.LBB12_103
# BB#106:                               # %sw.bb
                                        #   in Loop: Header=BB12_100 Depth=1
	testq	%r13, %r15
	jne	.LBB12_107
# BB#111:                               # %if.else291
                                        #   in Loop: Header=BB12_100 Depth=1
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	movq	$1, %r15
	je	.LBB12_116
# BB#112:                               # %if.else291
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	%rsi, -232(%rbp)        # 8-byte Spill
	movl	in_system_header(%rip), %eax
	testq	%r13, %rax
	jne	.LBB12_115
# BB#113:                               # %land.lhs.true295
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	%rdx, -200(%rbp)        # 8-byte Spill
	movq	parse_in(%rip), %rdi
	callq	cpp_sys_macro_p
	testq	%r13, %rax
	jne	.LBB12_114
# BB#126:                               # %if.then298
                                        #   in Loop: Header=BB12_100 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str19, %rdi
	callq	warning
.LBB12_114:                             #   in Loop: Header=BB12_100 Depth=1
	movq	-200(%rbp), %rdx        # 8-byte Reload
.LBB12_115:                             # %while.cond278.backedge
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	-232(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB12_116
.LBB12_103:                             # %while.body283
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$274877907008, %rcx     # imm = 0x4000000040
	testq	%rcx, %rax
	je	.LBB12_136
# BB#104:                               # %sw.bb301
                                        #   in Loop: Header=BB12_100 Depth=1
	testq	%r13, %rsi
	jne	.LBB12_105
# BB#127:                               # %if.else307
                                        #   in Loop: Header=BB12_100 Depth=1
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rsi
	je	.LBB12_116
# BB#128:                               # %if.else307
                                        #   in Loop: Header=BB12_100 Depth=1
	movl	in_system_header(%rip), %eax
	testq	%r13, %rax
	jne	.LBB12_116
# BB#129:                               # %land.lhs.true311
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	%rdx, -200(%rbp)        # 8-byte Spill
	movq	parse_in(%rip), %rdi
	callq	cpp_sys_macro_p
	testq	%r13, %rax
	jne	.LBB12_130
# BB#131:                               # %if.then314
                                        #   in Loop: Header=BB12_100 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str21, %rdi
	callq	warning
.LBB12_130:                             #   in Loop: Header=BB12_100 Depth=1
	movq	-200(%rbp), %rdx        # 8-byte Reload
	movq	$1, %rsi
.LBB12_116:                             # %while.cond278.backedge
                                        #   in Loop: Header=BB12_100 Depth=1
	movq	-224(%rbp), %rax        # 8-byte Reload
	cmpq	%rax, %rdx
	jb	.LBB12_100
	jmp	.LBB12_117
.LBB12_98:
	xorq	%r15, %r15
	xorq	%rbx, %rbx
.LBB12_117:                             # %while.end331
	movq	%rdx, -200(%rbp)        # 8-byte Spill
	movq	-208(%rbp), %rax        # 8-byte Reload
	movq	%rax, -192(%rbp)
	movl	%r15d, -184(%rbp)
	movl	%esi, -180(%rbp)
	movl	%r14d, -176(%rbp)
	movabsq	$parse_float, %rdi
	leaq	-192(%rbp), %rsi
	callq	do_float_handler
	testq	%r13, %rax
	je	.LBB12_137
# BB#118:                               # %if.then338
	movq	-152(%rbp), %rax
	movq	%rax, -120(%rbp)
	movq	-168(%rbp), %rax
	movq	-160(%rbp), %rcx
	movl	flag_traditional(%rip), %edx
	movq	-144(%rbp), %r14
	movq	%rcx, -128(%rbp)
	movq	%rax, -136(%rbp)
	testq	%rdx, %rdx
	jne	.LBB12_124
# BB#119:                               # %if.then338
	movl	-172(%rbp), %eax
	andq	%r13, %rax
	cmpq	$34, %rax
	jne	.LBB12_124
# BB#120:                               # %if.then338
	movl	pedantic(%rip), %eax
	testq	%r13, %rax
	je	.LBB12_124
# BB#121:                               # %land.lhs.true353
	subq	$48, %rsp
	movq	-120(%rbp), %rax
	movq	%rax, 40(%rsp)
	movq	-136(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	dconst1+16(%rip), %rax
	movq	%rax, 16(%rsp)
	movq	dconst1+8(%rip), %rax
	movq	%rax, 8(%rsp)
	movq	dconst1(%rip), %rax
	movq	%rax, (%rsp)
	callq	ereal_cmp
	addq	$48, %rsp
	andq	%r13, %rax
	cmpq	%r13, %rax
	je	.LBB12_123
# BB#122:                               # %lor.lhs.false357
	subq	$48, %rsp
	movq	dconstm1+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconstm1+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconstm1(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	-120(%rbp), %rax
	movq	%rax, 16(%rsp)
	movq	-136(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	addq	$48, %rsp
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB12_124
.LBB12_123:                             # %if.then361
	movabsq	$.L.str26, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB12_124:                             # %if.end362
	testq	%r13, %rbx
	je	.LBB12_138
# BB#125:                               # %if.then364
	movq	global_trees+88(%rip), %rsi
	movq	%r14, %rdi
	callq	convert
	movq	%rax, %rbx
	subq	$32, %rsp
	movq	-120(%rbp), %rax
	movq	%rax, 16(%rsp)
	movq	-136(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	build_real
	addq	$32, %rsp
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_complex
	jmp	.LBB12_139
.LBB12_136:                             # %do.body328
	movabsq	$.L.str24, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_137:                             # %do.body341
	movabsq	$.L.str25, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_138:                             # %if.else368
	subq	$32, %rsp
	movq	-120(%rbp), %rax
	movq	%rax, 16(%rsp)
	movq	-136(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	build_real
	addq	$32, %rsp
.LBB12_139:                             # %if.end734
	movq	%rax, %rbx
	movq	-216(%rbp), %r14        # 8-byte Reload
	movq	-200(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB12_292
.LBB12_169:
	movabsq	$integer_types+48, %rax
	jmp	.LBB12_209
.LBB12_196:                             # %if.then490
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_197
# BB#198:                               # %if.then490
	movabsq	$integer_types+56, %rax
	jmp	.LBB12_209
.LBB12_200:
	movabsq	$integer_types+80, %rax
.LBB12_202:                             # %if.else496
	movq	(%rax), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_206
# BB#203:                               # %if.then504
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	movabsq	$0, %rdi
	jne	.LBB12_204
# BB#205:                               # %if.then504
	movabsq	$integer_types+72, %rax
	jmp	.LBB12_209
.LBB12_193:
	movabsq	$integer_types+48, %rax
	jmp	.LBB12_209
.LBB12_197:
	movabsq	$integer_types+64, %rax
	jmp	.LBB12_209
.LBB12_206:                             # %if.else510
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	movabsq	$0, %rdi
	jne	.LBB12_207
# BB#208:                               # %if.else510
	movabsq	$c_global_trees+72, %rax
	jmp	.LBB12_209
.LBB12_133:                             # %do.body320
	movabsq	$.L.str22, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_204:
	movabsq	$integer_types+80, %rax
	jmp	.LBB12_209
.LBB12_207:
	movabsq	$c_global_trees+80, %rax
.LBB12_209:                             # %if.else510
	movq	(%rax), %r15
.LBB12_210:                             # %if.end520
	movl	warn_traditional(%rip), %eax
	testq	%rax, %rax
	jne	.LBB12_212
# BB#211:                               # %if.end520
	movl	flag_traditional(%rip), %eax
	testq	%r13, %rax
	jne	.LBB12_238
.LBB12_212:                             # %if.then524
	movq	-232(%rbp), %rax        # 8-byte Reload
	movq	-240(%rbp), %rcx        # 8-byte Reload
	orq	%rcx, %rax
	testq	%r13, %rax
	jne	.LBB12_215
# BB#213:                               # %land.lhs.true528
	movq	integer_types+40(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_215
# BB#214:                               # %if.then531
	movq	integer_types+40(%rip), %rdi
	jmp	.LBB12_238
.LBB12_215:                             # %if.else532
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB12_217
# BB#216:                               # %if.else532
	movq	%rdx, %rcx
.LBB12_217:                             # %if.else532
	movq	%r14, %rsi
	andq	%r13, %rsi
	cmpq	$10, %rsi
	je	.LBB12_219
# BB#218:                               # %if.else532
	movq	%rdx, %rax
.LBB12_219:                             # %if.else532
	movq	-232(%rbp), %rdx        # 8-byte Reload
	testq	%r13, %rdx
	jne	.LBB12_223
# BB#220:                               # %if.else532
	orq	%rcx, %rax
	je	.LBB12_223
# BB#221:                               # %land.lhs.true539
	movq	integer_types+48(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_223
# BB#222:                               # %if.then542
	movq	integer_types+48(%rip), %rdi
	jmp	.LBB12_238
.LBB12_223:                             # %if.else543
	movq	-248(%rbp), %rax        # 8-byte Reload
	movq	-240(%rbp), %rcx        # 8-byte Reload
	orq	%rcx, %rax
	testq	%r13, %rax
	jne	.LBB12_226
# BB#224:                               # %land.lhs.true547
	movq	integer_types+56(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_226
# BB#225:                               # %if.then550
	movq	integer_types+56(%rip), %rdi
	jmp	.LBB12_238
.LBB12_226:                             # %if.else551
	movq	-248(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_229
# BB#227:                               # %land.lhs.true553
	movq	integer_types+64(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_229
# BB#228:                               # %if.then556
	movq	integer_types+64(%rip), %rdi
	jmp	.LBB12_238
.LBB12_229:                             # %if.else557
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_232
# BB#230:                               # %land.lhs.true559
	movq	integer_types+72(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_232
# BB#231:                               # %if.then562
	movq	integer_types+72(%rip), %rdi
	jmp	.LBB12_238
.LBB12_232:                             # %if.else563
	movq	integer_types+80(%rip), %rsi
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_234
# BB#233:                               # %if.then566
	movq	integer_types+80(%rip), %rdi
	jmp	.LBB12_238
.LBB12_107:                             # %do.body288
	movabsq	$.L.str18, %rcx
	jmp	.LBB12_108
.LBB12_105:                             # %do.body304
	movabsq	$.L.str20, %rcx
.LBB12_108:                             # %syntax_error
	xorq	%rax, %rax
	movq	%rcx, %rdi
	jmp	.LBB12_109
.LBB12_234:                             # %if.else567
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_237
# BB#235:                               # %land.lhs.true569
	movq	c_global_trees+72(%rip), %rax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_237
# BB#236:                               # %if.then572
	movq	c_global_trees+72(%rip), %rdi
	jmp	.LBB12_238
.LBB12_237:                             # %if.else573
	movq	c_global_trees+80(%rip), %rdi
.LBB12_238:                             # %if.end581
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	movq	%r15, %r12
	jne	.LBB12_240
# BB#239:                               # %select.mid
	movq	%rdi, %r12
.LBB12_240:                             # %select.end
	movl	warn_traditional(%rip), %eax
	movl	in_system_header(%rip), %esi
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB12_242
# BB#241:                               # %select.end
	movq	%rdx, %rcx
.LBB12_242:                             # %select.end
	testq	%rsi, %rsi
	movq	%rax, %rsi
	je	.LBB12_244
# BB#243:                               # %select.end
	movq	%rdx, %rsi
.LBB12_244:                             # %select.end
	andq	%r13, %r14
	cmpq	$10, %r14
	je	.LBB12_246
# BB#245:                               # %select.end
	movq	%rdx, %rax
.LBB12_246:                             # %select.end
	cmpq	%rdi, %r15
	je	.LBB12_254
# BB#247:                               # %select.end
	orq	%rcx, %rsi
	orq	%rax, %rsi
	jne	.LBB12_254
# BB#248:                               # %if.then596
	movl	60(%r15), %eax
	movl	60(%rdi), %ecx
	xorq	%rax, %rcx
	testq	$511, %rcx              # imm = 0x1FF
	je	.LBB12_250
# BB#249:                               # %if.then604
	movabsq	$.L.str36, %rdi
	jmp	.LBB12_253
.LBB12_250:                             # %if.else605
	movl	16(%r15), %eax
	movl	16(%rdi), %ecx
	xorq	%rax, %rcx
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB12_252
# BB#251:                               # %if.then616
	movabsq	$.L.str37, %rdi
	jmp	.LBB12_253
.LBB12_252:                             # %if.else617
	movabsq	$.L.str38, %rdi
.LBB12_253:                             # %if.end620
	xorq	%rax, %rax
	callq	warning
.LBB12_254:                             # %if.end620
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_255
# BB#256:                               # %if.end620
	movl	flag_traditional(%rip), %eax
	testq	%r13, %rax
	jne	.LBB12_257
# BB#258:                               # %land.lhs.true624
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB12_260
# BB#259:                               # %land.lhs.true624
	movq	%rsi, %rdx
.LBB12_260:                             # %land.lhs.true624
	movq	-248(%rbp), %rdi        # 8-byte Reload
	testq	%r13, %rdi
	je	.LBB12_262
# BB#261:                               # %land.lhs.true624
	movq	%rsi, %rcx
.LBB12_262:                             # %land.lhs.true624
	testq	%rcx, %rdx
	jne	.LBB12_263
# BB#264:                               # %land.lhs.true624
	cmpq	$0, -256(%rbp)          # 8-byte Folded Reload
	movq	-272(%rbp), %rsi        # 8-byte Reload
	jne	.LBB12_275
# BB#265:                               # %land.lhs.true630
	testq	%rax, %rax
	je	.LBB12_266
# BB#267:                               # %land.lhs.true630
	movabsq	$integer_types+72, %rcx
	jmp	.LBB12_268
.LBB12_255:
	movq	-272(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB12_275
.LBB12_257:
	movq	-272(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB12_275
.LBB12_263:
	movq	-272(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB12_275
.LBB12_266:
	movabsq	$integer_types+56, %rcx
.LBB12_268:                             # %land.lhs.true630
	movq	(%rcx), %rcx
	movl	60(%rcx), %ecx
	movl	60(%r12), %edx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	movq	$0, -256(%rbp)          # 8-byte Folded Spill
	cmpq	%rdx, %rcx
	jae	.LBB12_275
# BB#269:                               # %if.then650
	movq	%rsi, %r15
	testq	%rax, %rax
	je	.LBB12_270
# BB#271:                               # %cond.true652
	movzbl	17(%r12), %eax
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB12_272
# BB#273:                               # %cond.true652
	movabsq	$.L.str41, %rcx
	jmp	.LBB12_274
.LBB12_270:
	movabsq	$.L.str42, %rcx
	jmp	.LBB12_274
.LBB12_272:
	movabsq	$.L.str40, %rcx
.LBB12_274:                             # %cond.end661
	movabsq	$.L.str39, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	pedwarn
	movq	$1, -256(%rbp)          # 8-byte Folded Spill
	movq	%r15, %rsi
.LBB12_275:                             # %if.end663
	cmpq	$10, %r14
	movq	-216(%rbp), %r14        # 8-byte Reload
	jne	.LBB12_279
# BB#276:                               # %if.end663
	movq	-240(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB12_279
# BB#277:                               # %land.lhs.true668
	movzbl	17(%r12), %eax
	testq	$32, %rax
	je	.LBB12_279
# BB#278:                               # %if.then675
	movabsq	$.L.str43, %rdi
	xorq	%rax, %rax
	movq	%rsi, %r15
	callq	warning
	movq	%r15, %rsi
.LBB12_279:                             # %if.end676
	testq	%r13, %rsi
	je	.LBB12_283
# BB#280:                               # %if.then678
	movq	integer_types+40(%rip), %rdi
	movl	60(%r12), %eax
	movl	60(%rdi), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	jbe	.LBB12_281
# BB#282:                               # %do.body693
	movabsq	$.L.str44, %rdi
	xorq	%rax, %rax
	jmp	.LBB12_109
.LBB12_283:                             # %if.else697
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_286
# BB#284:                               # %land.lhs.true699
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	int_fits_type_p
	testq	%r13, %rax
	je	.LBB12_285
.LBB12_286:                             # %if.else713
	movq	%r12, 8(%rbx)
	jmp	.LBB12_287
.LBB12_281:                             # %if.then689
	movq	global_trees+88(%rip), %r15
	movq	%rbx, %rsi
	callq	convert
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	build_complex
	movq	%rax, %rbx
	jmp	.LBB12_287
.LBB12_285:                             # %if.then702
	movq	%r12, %rdi
	callq	unsigned_type
	movq	%rax, 8(%rbx)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	movabsq	$4294180863, %rcx       # imm = 0xFFF3FFFF
	andq	%rax, %rcx
	movl	%ecx, 16(%rbx)
.LBB12_287:                             # %if.end717
	cmpq	$0, -256(%rbp)          # 8-byte Folded Reload
	movq	-200(%rbp), %rsi        # 8-byte Reload
	jne	.LBB12_292
# BB#288:                               # %land.lhs.true719
	movq	%rsi, %r15
	movq	8(%rbx), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$6, %rax
	jne	.LBB12_291
# BB#289:                               # %land.lhs.true727
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r13, %rax
	jne	.LBB12_291
# BB#290:                               # %if.then732
	movabsq	$.L.str45, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	pedwarn
.LBB12_291:                             # %if.end734
	movq	%r15, %rsi
.LBB12_292:                             # %if.end734
	movq	-224(%rbp), %rax        # 8-byte Reload
	cmpq	%rax, %rsi
	jae	.LBB12_294
# BB#293:                               # %if.then739
	subq	%r14, %rsi
	movabsq	$.L.str46, %rdi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	error
	jmp	.LBB12_294
.Ltmp116:
	.size	lex_number, .Ltmp116-lex_number
	.cfi_endproc

	.align	16, 0x90
	.type	lex_string,@function
lex_string:                             # @lex_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp120:
	.cfi_def_cfa_offset 16
.Ltmp121:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp122:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp123:
	.cfi_offset %rbx, -56
.Ltmp124:
	.cfi_offset %r12, -48
.Ltmp125:
	.cfi_offset %r13, -40
.Ltmp126:
	.cfi_offset %r14, -32
.Ltmp127:
	.cfi_offset %r15, -24
	movq	%rdx, %r14
	movq	%rsi, %rbx
	incq	%rbx
	movq	c_global_trees(%rip), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r14
	movq	$1, %rdx
	je	.LBB13_2
# BB#1:                                 # %cond.true
	movl	60(%rax), %edx
	movq	$3, %rcx
	shrq	%cl, %rdx
	andq	$63, %rdx
.LBB13_2:                               # %cond.end
	imulq	%rbx, %rdx
	andq	%r13, %rdx
	addq	$15, %rdx
	andq	$-16, %rdx
	movq	%rsp, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rsp
	movq	%rsi, %r12
	andq	%r13, %r12
	testq	%r13, %r14
	movq	%rdi, -48(%rbp)
	jne	.LBB13_4
# BB#3:                                 # %cond.end
	movq	integer_types(%rip), %rax
.LBB13_4:                               # %cond.end
	andq	%r13, %rsi
	movq	%rcx, -64(%rbp)         # 8-byte Spill
	movq	%rcx, %r15
	je	.LBB13_32
# BB#5:                                 # %while.body.lr.ph
	movl	60(%rax), %ecx
	addq	%rdi, %r12
	andq	$511, %rcx              # imm = 0x1FF
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	cmpq	$31, %rax
	ja	.LBB13_19
# BB#6:
	decq	%rdx
	andq	%r13, %rdx
	movq	%rdx, -56(%rbp)         # 8-byte Spill
	movq	-64(%rbp), %r15         # 8-byte Reload
	.align	16, 0x90
.LBB13_7:                               # %while.body.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_14 Depth 2
	leaq	1(%rdi), %rax
	movq	%rax, -48(%rbp)
	movzbl	(%rdi), %eax
	cmpq	$92, %rax
	jne	.LBB13_9
# BB#8:                                 # %if.end.us
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	parse_in(%rip), %rdi
	movl	flag_traditional(%rip), %r8d
	leaq	-48(%rbp), %rsi
	movq	%r12, %rdx
	movq	-56(%rbp), %rcx         # 8-byte Reload
	callq	cpp_parse_escape
.LBB13_9:                               # %if.end23.us
                                        #   in Loop: Header=BB13_7 Depth=1
	testq	%r13, %r14
	je	.LBB13_10
# BB#12:                                # %if.then25.us
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	integer_types(%rip), %rcx
	movq	c_global_trees(%rip), %rdx
	movl	60(%rcx), %r8d
	movzbl	60(%rdx), %esi
	movq	$1, %rdi
	movq	%r8, %rcx
	shlq	%cl, %rdi
	xorq	%rbx, %rbx
	testq	$504, %rsi              # imm = 0x1F8
	je	.LBB13_18
# BB#13:                                #   in Loop: Header=BB13_7 Depth=1
	andq	$511, %r8               # imm = 0x1FF
	addq	$255, %rdi
	.align	16, 0x90
.LBB13_14:                              # %for.body.us
                                        #   Parent Loop BB13_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$3, %rsi
	movabsq	$0, %rsi
	jg	.LBB13_16
# BB#15:                                # %if.else42.us
                                        #   in Loop: Header=BB13_14 Depth=2
	movq	%rbx, %rcx
	imulq	%r8, %rcx
	movq	%rax, %rsi
	andq	%r13, %rsi
	andq	%r13, %rcx
	shrq	%cl, %rsi
	andq	%rdi, %rsi
.LBB13_16:                              # %if.end44.us
                                        #   in Loop: Header=BB13_14 Depth=2
	movq	c_global_trees(%rip), %rcx
	movb	%sil, (%r15,%rbx)
	incq	%rbx
	movl	60(%rcx), %esi
	movq	$3, %rcx
	shrq	%cl, %rsi
	andq	$63, %rsi
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jl	.LBB13_14
# BB#17:                                # %for.cond.for.end_crit_edge.us
                                        #   in Loop: Header=BB13_7 Depth=1
	andq	%r13, %rsi
	movq	%rsi, %rbx
.LBB13_18:                              # %for.end.us
                                        #   in Loop: Header=BB13_7 Depth=1
	addq	%rbx, %r15
	jmp	.LBB13_11
	.align	16, 0x90
.LBB13_10:                              # %if.else53.us
                                        #   in Loop: Header=BB13_7 Depth=1
	movb	%al, (%r15)
	incq	%r15
.LBB13_11:                              # %while.cond.backedge.us
                                        #   in Loop: Header=BB13_7 Depth=1
	movq	-48(%rbp), %rdi
	cmpq	%r12, %rdi
	jb	.LBB13_7
	jmp	.LBB13_32
.LBB13_19:
	movq	-64(%rbp), %r15         # 8-byte Reload
	.align	16, 0x90
.LBB13_20:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_25 Depth 2
	leaq	1(%rdi), %rax
	movq	%rax, -48(%rbp)
	movzbl	(%rdi), %eax
	cmpq	$92, %rax
	jne	.LBB13_22
# BB#21:                                # %if.end
                                        #   in Loop: Header=BB13_20 Depth=1
	movq	parse_in(%rip), %rdi
	movl	flag_traditional(%rip), %r8d
	leaq	-48(%rbp), %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	cpp_parse_escape
.LBB13_22:                              # %if.end23
                                        #   in Loop: Header=BB13_20 Depth=1
	testq	%r13, %r14
	je	.LBB13_30
# BB#23:                                # %if.then25
                                        #   in Loop: Header=BB13_20 Depth=1
	movq	integer_types(%rip), %rcx
	movq	c_global_trees(%rip), %rdx
	movl	60(%rcx), %r8d
	movzbl	60(%rdx), %esi
	movq	$1, %rdi
	movq	%r8, %rcx
	shlq	%cl, %rdi
	xorq	%rbx, %rbx
	testq	$504, %rsi              # imm = 0x1F8
	je	.LBB13_29
# BB#24:                                #   in Loop: Header=BB13_20 Depth=1
	andq	$511, %r8               # imm = 0x1FF
	addq	$255, %rdi
	.align	16, 0x90
.LBB13_25:                              # %for.body
                                        #   Parent Loop BB13_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$3, %rsi
	movabsq	$0, %rsi
	jg	.LBB13_27
# BB#26:                                # %if.else42
                                        #   in Loop: Header=BB13_25 Depth=2
	movq	%rbx, %rcx
	imulq	%r8, %rcx
	movq	%rax, %rsi
	andq	%r13, %rsi
	andq	%r13, %rcx
	shrq	%cl, %rsi
	andq	%rdi, %rsi
.LBB13_27:                              # %if.end44
                                        #   in Loop: Header=BB13_25 Depth=2
	movq	c_global_trees(%rip), %rcx
	movb	%sil, (%r15,%rbx)
	incq	%rbx
	movl	60(%rcx), %esi
	movq	$3, %rcx
	shrq	%cl, %rsi
	andq	$63, %rsi
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jl	.LBB13_25
# BB#28:                                # %for.cond.for.end_crit_edge
                                        #   in Loop: Header=BB13_20 Depth=1
	andq	%r13, %rsi
	movq	%rsi, %rbx
.LBB13_29:                              # %for.end
                                        #   in Loop: Header=BB13_20 Depth=1
	addq	%rbx, %r15
	jmp	.LBB13_31
	.align	16, 0x90
.LBB13_30:                              # %if.else53
                                        #   in Loop: Header=BB13_20 Depth=1
	movb	%al, (%r15)
	incq	%r15
.LBB13_31:                              # %while.cond.backedge
                                        #   in Loop: Header=BB13_20 Depth=1
	movq	-48(%rbp), %rdi
	cmpq	%r12, %rdi
	jb	.LBB13_20
.LBB13_32:                              # %while.end
	testq	%r13, %r14
	je	.LBB13_34
# BB#33:                                # %if.then58
	movq	c_global_trees(%rip), %rax
	movl	60(%rax), %edx
	movq	$3, %rcx
	shrq	%cl, %rdx
	andq	$63, %rdx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memset
	movq	c_global_trees(%rip), %rax
	movl	60(%rax), %edi
	movq	$3, %rcx
	shrq	%cl, %rdi
	andq	$63, %rdi
	addq	%r15, %rdi
	jmp	.LBB13_35
.LBB13_34:                              # %if.else72
	xorq	%rax, %rax
	movb	%al, (%r15)
	incq	%r15
	movq	%r15, %rdi
.LBB13_35:                              # %if.end74
	movq	-64(%rbp), %rsi         # 8-byte Reload
	subq	%rsi, %rdi
	callq	build_string
	testq	%r13, %r14
	je	.LBB13_37
# BB#36:                                # %if.then78
	movq	c_global_trees+96(%rip), %rcx
	jmp	.LBB13_38
.LBB13_37:                              # %if.else80
	movq	c_global_trees+88(%rip), %rcx
.LBB13_38:                              # %if.end83
	movq	%rcx, 8(%rax)
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp128:
	.size	lex_string, .Ltmp128-lex_string
	.cfi_endproc

	.align	16, 0x90
	.type	parse_float,@function
parse_float:                            # @parse_float
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
	subq	$80, %rsp
.Ltmp140:
	.cfi_def_cfa_offset 128
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
	movq	%rdi, %r13
	xorq	%rax, %rax
	movl	%eax, 20(%r13)
	movq	global_trees+200(%rip), %r12
	movq	%r12, 48(%r13)
	movl	8(%r13), %ecx
	movl	12(%r13), %eax
	testq	%rcx, %rcx
	je	.LBB14_4
# BB#1:                                 # %if.then
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_3
# BB#2:                                 # %if.then2
	movabsq	$.L.str48, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB14_3
.LBB14_4:                               # %if.else
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_6
# BB#5:                                 # %if.then6
	movq	global_trees+208(%rip), %r12
	movq	%r12, 48(%r13)
	movabsq	$.L.str50, %r14
	jmp	.LBB14_8
.LBB14_6:                               # %if.else8
	movl	flag_single_precision_constant(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_7
.LBB14_3:                               # %if.then10
	movq	global_trees+192(%rip), %r12
	movq	%r12, 48(%r13)
	movabsq	$.L.str49, %r14
.LBB14_8:                               # %if.end14
	callq	__errno_location
	movq	%rax, %r15
	xorq	%rax, %rax
	movl	%eax, (%r15)
	movl	16(%r13), %eax
	movl	60(%r12), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	movq	(%r13), %rsi
	leaq	24(%r13), %rbx
	andq	$127, %rdx
	cmpq	$16, %rax
	jne	.LBB14_10
# BB#9:                                 # %if.then15
	leaq	56(%rsp), %rdi
	callq	ereal_atof
	movq	72(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	56(%rsp), %rax
	movq	64(%rsp), %rcx
	jmp	.LBB14_11
.LBB14_10:                              # %if.else18
	leaq	32(%rsp), %rdi
	callq	ereal_atof
	movq	48(%rsp), %rax
	movq	%rax, 16(%rbx)
	movq	32(%rsp), %rax
	movq	40(%rsp), %rcx
.LBB14_11:                              # %if.end28
	movq	%rcx, 8(%rbx)
	movq	%rax, (%rbx)
	movl	(%r15), %eax
	movl	%eax, 20(%r13)
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isinf
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_14
# BB#12:                                # %if.end28
	movl	pedantic(%rip), %eax
	testq	%rcx, %rax
	je	.LBB14_14
# BB#13:                                # %if.then35
	movabsq	$.L.str51, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	warning
.LBB14_14:                              # %if.end36
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB14_7:
	movabsq	$.L.str47, %r14
	jmp	.LBB14_8
.Ltmp146:
	.size	parse_float, .Ltmp146-parse_float
	.cfi_endproc

	.type	file_info_tree,@object  # @file_info_tree
	.local	file_info_tree
	.comm	file_info_tree,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"<top level>"
	.size	.L.str, 12

	.type	header_time,@object     # @header_time
	.local	header_time
	.comm	header_time,4,4
	.type	body_time,@object       # @body_time
	.local	body_time
	.comm	body_time,4,4
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"-"
	.size	.L.str1, 2

	.type	.L.str2,@object         # @.str2
.L.str2:
	.zero	1
	.size	.L.str2, 1

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"\n******\n"
	.size	.L.str3, 9

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"header files (total)"
	.size	.L.str4, 21

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"main file (total)"
	.size	.L.str5, 18

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"ratio = %g : 1\n"
	.size	.L.str6, 16

	.type	src_lineno,@object      # @src_lineno
	.local	src_lineno
	.comm	src_lineno,4,4
	.type	indent_level,@object    # @indent_level
	.comm	indent_level,4,4
	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"stray '%c' in program"
	.size	.L.str7, 22

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"stray '\\%o' in program"
	.size	.L.str8, 23

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"c-lex.c"
	.size	.L.str9, 8

	.type	.L__FUNCTION__.c_lex,@object # @__FUNCTION__.c_lex
.L__FUNCTION__.c_lex:
	.asciz	"c_lex"
	.size	.L__FUNCTION__.c_lex, 6

	.type	pending_lang_change,@object # @pending_lang_change
	.comm	pending_lang_change,4,4
	.type	c_header_level,@object  # @c_header_level
	.comm	c_header_level,4,4
	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"too many decimal points in floating constant"
	.size	.L.str10, 45

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"decimal point in exponent - impossible!"
	.size	.L.str11, 40

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"underscore in number"
	.size	.L.str12, 21

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"numeric constant with no digits"
	.size	.L.str13, 32

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"numeric constant contains digits beyond the radix"
	.size	.L.str14, 50

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"floating constant may not be in radix 16"
	.size	.L.str15, 41

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"hexadecimal floating constant has no exponent"
	.size	.L.str16, 46

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"floating constant exponent has no digits"
	.size	.L.str17, 41

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"more than one 'f' suffix on floating constant"
	.size	.L.str18, 46

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"traditional C rejects the 'f' suffix"
	.size	.L.str19, 37

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"more than one 'l' suffix on floating constant"
	.size	.L.str20, 46

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"traditional C rejects the 'l' suffix"
	.size	.L.str21, 37

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"more than one 'i' or 'j' suffix on floating constant"
	.size	.L.str22, 53

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"ISO C forbids imaginary numeric constants"
	.size	.L.str23, 42

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"invalid suffix on floating constant"
	.size	.L.str24, 36

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"floating constant out of range"
	.size	.L.str25, 31

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"floating point number exceeds range of 'double'"
	.size	.L.str26, 48

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"two 'u' suffixes on integer constant"
	.size	.L.str27, 37

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"traditional C rejects the 'u' suffix"
	.size	.L.str28, 37

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"three 'l' suffixes on integer constant"
	.size	.L.str29, 39

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"'lul' is not a valid integer suffix"
	.size	.L.str30, 36

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"'Ll' and 'lL' are not valid integer suffixes"
	.size	.L.str31, 45

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"ISO C89 forbids long long integer constants"
	.size	.L.str32, 44

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"more than one 'i' or 'j' suffix on integer constant"
	.size	.L.str33, 52

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"invalid suffix on integer constant"
	.size	.L.str34, 35

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"integer constant is too large for this configuration of the compiler - truncated to %d bits"
	.size	.L.str35, 92

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"width of integer constant changes with -traditional"
	.size	.L.str36, 52

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"integer constant is unsigned in ISO C, signed with -traditional"
	.size	.L.str37, 64

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"width of integer constant may change on other systems with -traditional"
	.size	.L.str38, 72

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"integer constant larger than the maximum value of %s"
	.size	.L.str39, 53

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"an unsigned long long int"
	.size	.L.str40, 26

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"a long long int"
	.size	.L.str41, 16

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"an unsigned long int"
	.size	.L.str42, 21

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"decimal constant is so large that it is unsigned"
	.size	.L.str43, 49

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"complex integer constant is too wide for 'complex int'"
	.size	.L.str44, 55

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"integer constant is larger than the maximum value for its type"
	.size	.L.str45, 63

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"missing white space after number '%.*s'"
	.size	.L.str46, 40

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"double"
	.size	.L.str47, 7

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"both 'f' and 'l' suffixes on floating constant"
	.size	.L.str48, 47

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"float"
	.size	.L.str49, 6

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"long double"
	.size	.L.str50, 12

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"floating point number exceeds range of '%s'"
	.size	.L.str51, 44

	.type	map,@object             # @map
	.local	map
	.comm	map,8,8
	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"ignoring #pragma %s %s"
	.size	.L.str52, 23

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"badly nested C headers from preprocessor"
	.size	.L.str53, 41

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"%s\"%s\"\n"
	.size	.L.str54, 8

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"\t.ident\t"
	.size	.L.str55, 9


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
