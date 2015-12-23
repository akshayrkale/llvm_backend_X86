	.file	"langhooks.c.bc"
	.text
	.globl	lhd_do_nothing
	.align	16, 0x90
	.type	lhd_do_nothing,@function
lhd_do_nothing:                         # @lhd_do_nothing
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp0:
	.size	lhd_do_nothing, .Ltmp0-lhd_do_nothing
	.cfi_endproc

	.globl	lhd_return_tree
	.align	16, 0x90
	.type	lhd_return_tree,@function
lhd_return_tree:                        # @lhd_return_tree
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	retq
.Ltmp1:
	.size	lhd_return_tree, .Ltmp1-lhd_return_tree
	.cfi_endproc

	.globl	lhd_decode_option
	.align	16, 0x90
	.type	lhd_decode_option,@function
lhd_decode_option:                      # @lhd_decode_option
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp2:
	.size	lhd_decode_option, .Ltmp2-lhd_decode_option
	.cfi_endproc

	.globl	lhd_print_tree_nothing
	.align	16, 0x90
	.type	lhd_print_tree_nothing,@function
lhd_print_tree_nothing:                 # @lhd_print_tree_nothing
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp3:
	.size	lhd_print_tree_nothing, .Ltmp3-lhd_print_tree_nothing
	.cfi_endproc

	.globl	lhd_safe_from_p
	.align	16, 0x90
	.type	lhd_safe_from_p,@function
lhd_safe_from_p:                        # @lhd_safe_from_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp4:
	.size	lhd_safe_from_p, .Ltmp4-lhd_safe_from_p
	.cfi_endproc

	.globl	lhd_staticp
	.align	16, 0x90
	.type	lhd_staticp,@function
lhd_staticp:                            # @lhd_staticp
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp5:
	.size	lhd_staticp, .Ltmp5-lhd_staticp
	.cfi_endproc

	.globl	lhd_set_yydebug
	.align	16, 0x90
	.type	lhd_set_yydebug,@function
lhd_set_yydebug:                        # @lhd_set_yydebug
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB6_1
# BB#2:                                 # %if.then
	movq	stderr(%rip), %rcx
	movabsq	$.L.str, %rdi
	movq	$50, %rsi
	movq	$1, %rdx
	jmp	fwrite  # TAILCALL
.LBB6_1:                                # %if.end
	retq
.Ltmp6:
	.size	lhd_set_yydebug, .Ltmp6-lhd_set_yydebug
	.cfi_endproc

	.globl	lhd_clear_binding_stack
	.align	16, 0x90
	.type	lhd_clear_binding_stack,@function
lhd_clear_binding_stack:                # @lhd_clear_binding_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp9:
	.cfi_def_cfa_offset 16
.Ltmp10:
	.cfi_offset %rbx, -16
	callq	global_bindings_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB7_2
	.align	16, 0x90
.LBB7_1:                                # %while.body
                                        #   in Loop: Header=BB7_2 Depth=1
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	poplevel
	callq	global_bindings_p
.LBB7_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rax
	je	.LBB7_1
# BB#3:                                 # %while.end
	popq	%rbx
	retq
.Ltmp11:
	.size	lhd_clear_binding_stack, .Ltmp11-lhd_clear_binding_stack
	.cfi_endproc

	.globl	lhd_get_alias_set
	.align	16, 0x90
	.type	lhd_get_alias_set,@function
lhd_get_alias_set:                      # @lhd_get_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$-1, %rax
	retq
.Ltmp12:
	.size	lhd_get_alias_set, .Ltmp12-lhd_get_alias_set
	.cfi_endproc

	.globl	hook_get_alias_set_0
	.align	16, 0x90
	.type	hook_get_alias_set_0,@function
hook_get_alias_set_0:                   # @hook_get_alias_set_0
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp13:
	.size	hook_get_alias_set_0, .Ltmp13-hook_get_alias_set_0
	.cfi_endproc

	.globl	lhd_tree_inlining_walk_subtrees
	.align	16, 0x90
	.type	lhd_tree_inlining_walk_subtrees,@function
lhd_tree_inlining_walk_subtrees:        # @lhd_tree_inlining_walk_subtrees
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp14:
	.size	lhd_tree_inlining_walk_subtrees, .Ltmp14-lhd_tree_inlining_walk_subtrees
	.cfi_endproc

	.globl	lhd_tree_inlining_cannot_inline_tree_fn
	.align	16, 0x90
	.type	lhd_tree_inlining_cannot_inline_tree_fn,@function
lhd_tree_inlining_cannot_inline_tree_fn: # @lhd_tree_inlining_cannot_inline_tree_fn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp16:
	.cfi_def_cfa_offset 16
	movl	flag_really_no_inline(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_2
# BB#1:                                 # %land.lhs.true
	movq	(%rdi), %rax
	movq	136(%rax), %rsi
	movabsq	$.L.str1, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB11_3
.LBB11_2:                               # %if.end
	xorq	%rax, %rax
.LBB11_3:                               # %return
	popq	%rdx
	retq
.Ltmp17:
	.size	lhd_tree_inlining_cannot_inline_tree_fn, .Ltmp17-lhd_tree_inlining_cannot_inline_tree_fn
	.cfi_endproc

	.globl	lhd_tree_inlining_disregard_inline_limits
	.align	16, 0x90
	.type	lhd_tree_inlining_disregard_inline_limits,@function
lhd_tree_inlining_disregard_inline_limits: # @lhd_tree_inlining_disregard_inline_limits
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp19:
	.cfi_def_cfa_offset 16
	movq	136(%rdi), %rsi
	movabsq	$.L.str1, %rdi
	callq	lookup_attribute
	testq	%rax, %rax
	je	.LBB12_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB12_1:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp20:
	.size	lhd_tree_inlining_disregard_inline_limits, .Ltmp20-lhd_tree_inlining_disregard_inline_limits
	.cfi_endproc

	.globl	lhd_tree_inlining_add_pending_fn_decls
	.align	16, 0x90
	.type	lhd_tree_inlining_add_pending_fn_decls,@function
lhd_tree_inlining_add_pending_fn_decls: # @lhd_tree_inlining_add_pending_fn_decls
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	retq
.Ltmp21:
	.size	lhd_tree_inlining_add_pending_fn_decls, .Ltmp21-lhd_tree_inlining_add_pending_fn_decls
	.cfi_endproc

	.globl	lhd_tree_inlining_tree_chain_matters_p
	.align	16, 0x90
	.type	lhd_tree_inlining_tree_chain_matters_p,@function
lhd_tree_inlining_tree_chain_matters_p: # @lhd_tree_inlining_tree_chain_matters_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp22:
	.size	lhd_tree_inlining_tree_chain_matters_p, .Ltmp22-lhd_tree_inlining_tree_chain_matters_p
	.cfi_endproc

	.globl	lhd_tree_inlining_auto_var_in_fn_p
	.align	16, 0x90
	.type	lhd_tree_inlining_auto_var_in_fn_p,@function
lhd_tree_inlining_auto_var_in_fn_p:     # @lhd_tree_inlining_auto_var_in_fn_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %r8d
	movq	%r8, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %edx
	xorq	%rax, %rax
	cmpq	$100, %rdx
	jne	.LBB15_8
# BB#1:                                 # %land.lhs.true
	movq	80(%rdi), %rdx
	cmpq	%rsi, %rdx
	jne	.LBB15_8
# BB#2:                                 # %land.rhs
	andq	$262398, %r8            # imm = 0x400FE
	cmpq	$34, %r8
	movq	$1, %rax
	je	.LBB15_8
# BB#3:                                 # %lor.lhs.false20
	cmpq	$31, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	movq	%rdx, %rax
	jne	.LBB15_5
# BB#4:                                 # %lor.lhs.false20
	movq	%rsi, %rax
.LBB15_5:                               # %lor.lhs.false20
	cmpq	$36, %rcx
	jne	.LBB15_7
# BB#6:                                 # %lor.lhs.false20
	movq	%rsi, %rdx
.LBB15_7:                               # %lor.lhs.false20
	orq	%rdx, %rax
.LBB15_8:                               # %land.end
	retq
.Ltmp23:
	.size	lhd_tree_inlining_auto_var_in_fn_p, .Ltmp23-lhd_tree_inlining_auto_var_in_fn_p
	.cfi_endproc

	.globl	lhd_tree_inlining_copy_res_decl_for_inlining
	.align	16, 0x90
	.type	lhd_tree_inlining_copy_res_decl_for_inlining,@function
lhd_tree_inlining_copy_res_decl_for_inlining: # @lhd_tree_inlining_copy_res_decl_for_inlining
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	copy_decl_for_inlining  # TAILCALL
.Ltmp24:
	.size	lhd_tree_inlining_copy_res_decl_for_inlining, .Ltmp24-lhd_tree_inlining_copy_res_decl_for_inlining
	.cfi_endproc

	.globl	lhd_tree_inlining_anon_aggr_type_p
	.align	16, 0x90
	.type	lhd_tree_inlining_anon_aggr_type_p,@function
lhd_tree_inlining_anon_aggr_type_p:     # @lhd_tree_inlining_anon_aggr_type_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp25:
	.size	lhd_tree_inlining_anon_aggr_type_p, .Ltmp25-lhd_tree_inlining_anon_aggr_type_p
	.cfi_endproc

	.globl	lhd_tree_inlining_start_inlining
	.align	16, 0x90
	.type	lhd_tree_inlining_start_inlining,@function
lhd_tree_inlining_start_inlining:       # @lhd_tree_inlining_start_inlining
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp26:
	.size	lhd_tree_inlining_start_inlining, .Ltmp26-lhd_tree_inlining_start_inlining
	.cfi_endproc

	.globl	lhd_tree_inlining_end_inlining
	.align	16, 0x90
	.type	lhd_tree_inlining_end_inlining,@function
lhd_tree_inlining_end_inlining:         # @lhd_tree_inlining_end_inlining
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp27:
	.size	lhd_tree_inlining_end_inlining, .Ltmp27-lhd_tree_inlining_end_inlining
	.cfi_endproc

	.globl	lhd_tree_inlining_convert_parm_for_inlining
	.align	16, 0x90
	.type	lhd_tree_inlining_convert_parm_for_inlining,@function
lhd_tree_inlining_convert_parm_for_inlining: # @lhd_tree_inlining_convert_parm_for_inlining
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rax
	retq
.Ltmp28:
	.size	lhd_tree_inlining_convert_parm_for_inlining, .Ltmp28-lhd_tree_inlining_convert_parm_for_inlining
	.cfi_endproc

	.globl	lhd_tree_dump_dump_tree
	.align	16, 0x90
	.type	lhd_tree_dump_dump_tree,@function
lhd_tree_dump_dump_tree:                # @lhd_tree_dump_dump_tree
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp29:
	.size	lhd_tree_dump_dump_tree, .Ltmp29-lhd_tree_dump_dump_tree
	.cfi_endproc

	.globl	lhd_tree_dump_type_quals
	.align	16, 0x90
	.type	lhd_tree_dump_type_quals,@function
lhd_tree_dump_type_quals:               # @lhd_tree_dump_type_quals
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %edx
	movq	$12, %rcx
	movq	%rdx, %r8
	shrq	%cl, %r8
	movq	$10, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movl	60(%rdi), %r9d
	movq	$19, %rcx
	shrq	%cl, %r9
	andq	$255, %rdx
	xorq	%rax, %rax
	cmpq	$20, %rdx
	jne	.LBB22_4
# BB#1:                                 # %land.rhs
	cmpq	$0, 8(%rdi)
	je	.LBB22_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB22_4
.LBB22_2:
	movabsq	$0, %rax
.LBB22_4:                               # %land.end
	andq	$1, %r8
	andq	$2, %rsi
	orq	%r8, %rsi
	andq	$4, %r9
	orq	%rsi, %r9
	movq	$3, %rcx
	shlq	%cl, %rax
	orq	%r9, %rax
	retq
.Ltmp30:
	.size	lhd_tree_dump_type_quals, .Ltmp30-lhd_tree_dump_type_quals
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"warning: no yacc/bison-generated output to debug!\n"
	.size	.L.str, 51

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"always_inline"
	.size	.L.str1, 14


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
