	.file	"c-objc-common.c.bc"
	.text
	.globl	c_missing_noreturn_ok_p
	.align	16, 0x90
	.type	c_missing_noreturn_ok_p,@function
c_missing_noreturn_ok_p:                # @c_missing_noreturn_ok_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movl	flag_hosted(%rip), %ecx
	movq	%rdi, %rbx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB0_6
# BB#1:                                 # %land.rhs
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB0_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB0_3:                                # %cond.end
	movq	global_trees+400(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_4
# BB#5:                                 # %cond.end
	movabsq	$1, %rax
.LBB0_6:                                # %land.end
	popq	%rbx
	retq
.LBB0_4:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp4:
	.size	c_missing_noreturn_ok_p, .Ltmp4-c_missing_noreturn_ok_p
	.cfi_endproc

	.globl	c_disregard_inline_limits
	.align	16, 0x90
	.type	c_disregard_inline_limits,@function
c_disregard_inline_limits:              # @c_disregard_inline_limits
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 16
.Ltmp8:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	136(%rbx), %rsi
	movabsq	$.L.str, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB1_3
# BB#1:                                 # %if.end
	movq	200(%rbx), %rax
	movzbl	(%rax), %ecx
	xorq	%rax, %rax
	testq	$1, %rcx
	je	.LBB1_3
# BB#2:                                 # %land.rhs
	movzbl	49(%rbx), %eax
	andq	$1, %rax
.LBB1_3:                                # %return
	popq	%rbx
	retq
.Ltmp9:
	.size	c_disregard_inline_limits, .Ltmp9-c_disregard_inline_limits
	.cfi_endproc

	.globl	c_cannot_inline_tree_fn
	.align	16, 0x90
	.type	c_cannot_inline_tree_fn,@function
c_cannot_inline_tree_fn:                # @c_cannot_inline_tree_fn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp14:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp15:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 32
.Ltmp17:
	.cfi_offset %rbx, -32
.Ltmp18:
	.cfi_offset %r14, -24
.Ltmp19:
	.cfi_offset %r15, -16
	movl	flag_really_no_inline(%rip), %eax
	movq	(%rdi), %rbx
	testq	%rax, %rax
	je	.LBB2_2
# BB#1:                                 # %land.lhs.true
	movq	136(%rbx), %rsi
	movabsq	$.L.str, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	movq	$1, %r14
	je	.LBB2_13
.LBB2_2:                                # %if.end
	movabsq	$68719476736, %r15      # imm = 0x1000000000
	movq	%rbx, %rdi
	callq	function_attribute_inlinable_p
	testq	%rax, %rax
	je	.LBB2_12
# BB#3:                                 # %if.end4
	movq	current_function_decl(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB2_5
# BB#4:                                 # %if.then6
	callq	get_pending_sizes
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	put_pending_sizes
	testq	%r14, %r14
	jne	.LBB2_12
.LBB2_5:                                # %if.end16
	cmpq	$0, 80(%rbx)
	je	.LBB2_6
# BB#10:                                # %if.then19
	movq	200(%rbx), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB2_12
	jmp	.LBB2_11
.LBB2_6:
	movabsq	$current_function_decl, %rcx
	.align	16, 0x90
.LBB2_7:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rax
	testq	%rax, %rax
	je	.LBB2_9
# BB#8:                                 # %while.cond
                                        #   in Loop: Header=BB2_7 Depth=1
	leaq	80(%rax), %rcx
	cmpq	%rbx, %rax
	jne	.LBB2_7
.LBB2_9:                                # %while.end
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB2_13
.LBB2_11:                               # %if.end36
	leaq	168(%rbx), %rdi
	movabsq	$inline_forbidden_p, %rsi
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	movq	%rbx, %rdx
	callq	walk_tree
	testq	%rax, %rax
	je	.LBB2_13
.LBB2_12:                               # %if.then40
	orq	%r15, 48(%rbx)
	movq	$1, %r14
.LBB2_13:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp20:
	.size	c_cannot_inline_tree_fn, .Ltmp20-c_cannot_inline_tree_fn
	.cfi_endproc

	.align	16, 0x90
	.type	inline_forbidden_p,@function
inline_forbidden_p:                     # @inline_forbidden_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp26:
	.cfi_def_cfa_offset 32
.Ltmp27:
	.cfi_offset %rbx, -24
.Ltmp28:
	.cfi_offset %r14, -16
	movq	(%rdi), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$137, %rax
	jg	.LBB3_6
# BB#1:                                 # %entry
	cmpq	$53, %rax
	jne	.LBB3_14
# BB#2:                                 # %sw.bb
	movq	%rbx, %rdi
	callq	get_callee_fndecl
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB3_14
# BB#3:                                 # %if.end
	movq	%r14, %rdi
	callq	setjmp_call_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_15
# BB#4:                                 # %if.end4
	movq	48(%r14), %rax
	andq	$1610612736, %rax       # imm = 0x60000000
	cmpq	$1610612736, %rax       # imm = 0x60000000
	jne	.LBB3_14
# BB#5:                                 # %if.then7
	movl	56(%r14), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	cmpq	$90, %rcx
	jne	.LBB3_14
	jmp	.LBB3_15
.LBB3_6:                                # %entry
	cmpq	$138, %rax
	je	.LBB3_9
# BB#7:                                 # %entry
	cmpq	$154, %rax
	jne	.LBB3_8
# BB#12:                                # %sw.bb11
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB3_14
# BB#13:                                # %land.lhs.true
	cmpq	$0, 104(%rax)
	jne	.LBB3_15
	jmp	.LBB3_14
.LBB3_8:                                # %entry
	cmpq	$163, %rax
	jne	.LBB3_14
.LBB3_9:                                # %sw.bb24
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$31, %rcx
	jne	.LBB3_15
# BB#10:                                # %land.lhs.true40
	movq	80(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_14
# BB#11:                                # %land.lhs.true40
	cmpq	%rdx, %rax
	jne	.LBB3_15
.LBB3_14:                               # %sw.epilog50
	xorq	%rbx, %rbx
.LBB3_15:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp29:
	.size	inline_forbidden_p, .Ltmp29-inline_forbidden_p
	.cfi_endproc

	.globl	c_objc_common_init
	.align	16, 0x90
	.type	c_objc_common_init,@function
c_objc_common_init:                     # @c_objc_common_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 16
.Ltmp33:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	c_init_decl_processing
	movq	%rbx, %rdi
	callq	c_common_init
	movq	%rax, %rbx
	callq	add_c_tree_codes
	movq	$push_c_function_context, save_lang_status(%rip)
	movq	$pop_c_function_context, restore_lang_status(%rip)
	movq	global_dc(%rip), %rax
	movq	$mark_c_function_context, mark_lang_status(%rip)
	movq	$c_expand_expr, lang_expand_expr(%rip)
	movq	$c_expand_decl_stmt, lang_expand_decl_stmt(%rip)
	movq	$c_tree_printer, 312(%rax)
	movslq	mesg_implicit_function_declaration(%rip), %rax
	movq	$c_missing_noreturn_ok_p, lang_missing_noreturn_ok_p(%rip)
	testq	%rax, %rax
	jns	.LBB4_6
# BB#1:                                 # %if.then
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_4
# BB#2:                                 # %if.then2
	movl	flag_pedantic_errors(%rip), %eax
	testq	%rax, %rax
	movq	$2, %rax
	jne	.LBB4_5
# BB#3:                                 # %select.mid
	movq	$1, %rax
	jmp	.LBB4_5
.LBB4_4:                                # %if.else
	xorq	%rax, %rax
.LBB4_5:                                # %if.end4
	movl	%eax, mesg_implicit_function_declaration(%rip)
.LBB4_6:                                # %if.end4
	movabsq	$.L.str1, %rdx
	movq	$32, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, deferred_fns(%rip)
	movabsq	$deferred_fns, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_varray_root
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp34:
	.size	c_objc_common_init, .Ltmp34-c_objc_common_init
	.cfi_endproc

	.align	16, 0x90
	.type	c_tree_printer,@function
c_tree_printer:                         # @c_tree_printer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp37:
	.cfi_def_cfa_offset 16
.Ltmp38:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	40(%rbx), %rax
	movl	(%rax), %esi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$40, %rsi
	ja	.LBB5_2
# BB#1:                                 # %vaarg.in_reg
	movq	16(%rax), %rdx
	addq	%rdi, %rdx
	addq	$8, %rsi
	movl	%esi, (%rax)
	jmp	.LBB5_3
.LBB5_2:                                # %vaarg.in_mem
	movq	8(%rax), %rdx
	leaq	8(%rdx), %rcx
	movq	%rcx, 8(%rax)
.LBB5_3:                                # %vaarg.end
	movq	32(%rbx), %rax
	movsbq	(%rax), %rsi
	xorq	%rax, %rax
	addq	$-68, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$16, %rcx
	ja	.LBB5_9
# BB#4:                                 # %vaarg.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$65541, %rsi            # imm = 0x10005
	je	.LBB5_9
# BB#5:                                 # %sw.bb
	movq	(%rdx), %rdi
	cmpq	$0, 72(%rdi)
	je	.LBB5_6
# BB#7:                                 # %cond.true
	movq	$2, %rsi
	callq	*decl_printable_name(%rip)
	movq	%rax, %rsi
	jmp	.LBB5_8
.LBB5_6:
	movabsq	$.L.str2, %rsi
.LBB5_8:                                # %cond.end
	movq	%rbx, %rdi
	callq	output_add_string
	movq	$1, %rax
.LBB5_9:                                # %return
	popq	%rbx
	retq
.Ltmp39:
	.size	c_tree_printer, .Ltmp39-c_tree_printer
	.cfi_endproc

	.globl	defer_fn
	.align	16, 0x90
	.type	defer_fn,@function
defer_fn:                               # @defer_fn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp42:
	.cfi_def_cfa_offset 16
.Ltmp43:
	.cfi_offset %rbx, -16
	movq	deferred_fns(%rip), %rcx
	movq	(%rcx), %rsi
	movq	8(%rcx), %rdx
	movq	%rdi, %rbx
	cmpq	%rsi, %rdx
	jb	.LBB6_2
# BB#1:                                 # %if.then
	addq	%rsi, %rsi
	movq	%rcx, %rdi
	callq	varray_grow
	movq	%rax, %rcx
	movq	%rcx, deferred_fns(%rip)
	movq	8(%rcx), %rdx
.LBB6_2:                                # %if.end
	leaq	1(%rdx), %rax
	movq	%rax, 8(%rcx)
	movq	$1, %rax
	movq	%rbx, 32(%rcx,%rdx,8)
	popq	%rbx
	retq
.Ltmp44:
	.size	defer_fn, .Ltmp44-defer_fn
	.cfi_endproc

	.globl	c_objc_common_finish_file
	.align	16, 0x90
	.type	c_objc_common_finish_file,@function
c_objc_common_finish_file:              # @c_objc_common_finish_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp50:
	.cfi_def_cfa_offset 32
.Ltmp51:
	.cfi_offset %rbx, -24
.Ltmp52:
	.cfi_offset %r14, -16
	movq	deferred_fns(%rip), %rdi
	cmpq	$0, 8(%rdi)
	je	.LBB7_7
# BB#1:
	xorq	%rax, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB7_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdi,%rax,8), %rax
	movl	16(%rax), %ecx
	testq	$16384, %rcx            # imm = 0x4000
	jne	.LBB7_6
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB7_2 Depth=1
	testq	$524288, %rcx           # imm = 0x80000
	jne	.LBB7_5
# BB#4:                                 # %if.then7.i
                                        #   in Loop: Header=BB7_2 Depth=1
	orq	$262144, 48(%rax)       # imm = 0x40000
.LBB7_5:                                # %if.end.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rax, %rdi
	callq	c_expand_deferred_function
	movq	deferred_fns(%rip), %rdi
.LBB7_6:                                # %for.inc.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	8(%rdi), %rcx
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	%rcx, %rax
	jb	.LBB7_2
.LBB7_7:                                # %do.body.i
	testq	%rdi, %rdi
	je	.LBB7_9
# BB#8:                                 # %if.then13.i
	callq	free
	movq	$0, deferred_fns(%rip)
.LBB7_9:                                # %expand_deferred_fns.exit
	cmpq	$0, static_ctors(%rip)
	je	.LBB7_13
# BB#10:                                # %if.then
	movq	$73, %rdi
	callq	get_file_function_name
	movq	global_trees+216(%rip), %rsi
	movq	%rax, %r14
	xorq	%rdi, %rdi
	callq	build_tree_list
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	build_nt
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	start_function
	callq	store_parm_decls
	movq	cfun(%rip), %rax
	movq	$.L.str3, 136(%rax)
	callq	c_begin_compound_stmt
	movq	%rax, %r14
	xorq	%rdi, %rdi
	callq	pushlevel
	callq	clear_last_expr
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	add_scope_stmt
	movq	static_ctors(%rip), %rax
	testq	%rax, %rax
	je	.LBB7_12
	.align	16, 0x90
.LBB7_11:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	build_function_call
	movq	%rax, %rdi
	callq	c_expand_expr_stmt
	movq	static_ctors(%rip), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%rax, static_ctors(%rip)
	jne	.LBB7_11
.LBB7_12:                               # %for.end
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	add_scope_stmt
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	poplevel
	movq	24(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movq	32(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movq	(%r14), %rax
	movq	%rax, 32(%r14)
	movq	$0, (%r14)
	callq	current_stmt_tree
	movq	%r14, (%rax)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	finish_function
.LBB7_13:                               # %if.end
	cmpq	$0, static_dtors(%rip)
	je	.LBB7_17
# BB#14:                                # %if.then5
	movq	$68, %rdi
	callq	get_file_function_name
	movq	global_trees+216(%rip), %rsi
	movq	%rax, %r14
	xorq	%rdi, %rdi
	callq	build_tree_list
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	build_nt
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	start_function
	callq	store_parm_decls
	movq	cfun(%rip), %rax
	movq	$.L.str3, 136(%rax)
	callq	c_begin_compound_stmt
	movq	%rax, %r14
	xorq	%rdi, %rdi
	callq	pushlevel
	callq	clear_last_expr
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	add_scope_stmt
	movq	static_dtors(%rip), %rax
	testq	%rax, %rax
	je	.LBB7_16
	.align	16, 0x90
.LBB7_15:                               # %for.body10
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	build_function_call
	movq	%rax, %rdi
	callq	c_expand_expr_stmt
	movq	static_dtors(%rip), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%rax, static_dtors(%rip)
	jne	.LBB7_15
.LBB7_16:                               # %for.end18
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	add_scope_stmt
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	poplevel
	movq	24(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movq	32(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movq	(%r14), %rax
	movq	%rax, 32(%r14)
	movq	$0, (%r14)
	callq	current_stmt_tree
	movq	%r14, (%rax)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	finish_function
.LBB7_17:                               # %if.end19
	leaq	4(%rsp), %rsi
	xorq	%rdi, %rdi
	callq	dump_begin
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB7_19
# BB#18:                                # %if.then22
	callq	getdecls
	movl	4(%rsp), %esi
	andq	$-3, %rsi
	movq	%rax, %rdi
	movq	%rbx, %rdx
	callq	dump_node
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	dump_end
.LBB7_19:                               # %if.end24
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp53:
	.size	c_objc_common_finish_file, .Ltmp53-c_objc_common_finish_file
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"always_inline"
	.size	.L.str, 14

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"deferred_fns"
	.size	.L.str1, 13

	.type	deferred_fns,@object    # @deferred_fns
	.local	deferred_fns
	.comm	deferred_fns,8,8
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"({anonymous})"
	.size	.L.str2, 14

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"static constructors and destructors cannot be inlined"
	.size	.L.str3, 54


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
