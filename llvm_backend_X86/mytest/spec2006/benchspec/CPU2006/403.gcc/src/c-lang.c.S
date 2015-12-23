	.file	"c-lang.c.bc"
	.text
	.align	16, 0x90
	.type	c_init_options,@function
c_init_options:                         # @c_init_options
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rdi, %rdi
	jmp	c_common_init_options  # TAILCALL
.Ltmp0:
	.size	c_init_options, .Ltmp0-c_init_options
	.cfi_endproc

	.align	16, 0x90
	.type	c_init,@function
c_init:                                 # @c_init
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	c_objc_common_init  # TAILCALL
.Ltmp1:
	.size	c_init, .Ltmp1-c_init
	.cfi_endproc

	.globl	lookup_interface
	.align	16, 0x90
	.type	lookup_interface,@function
lookup_interface:                       # @lookup_interface
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp2:
	.size	lookup_interface, .Ltmp2-lookup_interface
	.cfi_endproc

	.globl	is_class_name
	.align	16, 0x90
	.type	is_class_name,@function
is_class_name:                          # @is_class_name
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp3:
	.size	is_class_name, .Ltmp3-is_class_name
	.cfi_endproc

	.globl	maybe_objc_check_decl
	.align	16, 0x90
	.type	maybe_objc_check_decl,@function
maybe_objc_check_decl:                  # @maybe_objc_check_decl
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp4:
	.size	maybe_objc_check_decl, .Ltmp4-maybe_objc_check_decl
	.cfi_endproc

	.globl	maybe_objc_comptypes
	.align	16, 0x90
	.type	maybe_objc_comptypes,@function
maybe_objc_comptypes:                   # @maybe_objc_comptypes
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.Ltmp5:
	.size	maybe_objc_comptypes, .Ltmp5-maybe_objc_comptypes
	.cfi_endproc

	.globl	maybe_building_objc_message_expr
	.align	16, 0x90
	.type	maybe_building_objc_message_expr,@function
maybe_building_objc_message_expr:       # @maybe_building_objc_message_expr
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp6:
	.size	maybe_building_objc_message_expr, .Ltmp6-maybe_building_objc_message_expr
	.cfi_endproc

	.globl	recognize_objc_keyword
	.align	16, 0x90
	.type	recognize_objc_keyword,@function
recognize_objc_keyword:                 # @recognize_objc_keyword
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp7:
	.size	recognize_objc_keyword, .Ltmp7-recognize_objc_keyword
	.cfi_endproc

	.globl	lookup_objc_ivar
	.align	16, 0x90
	.type	lookup_objc_ivar,@function
lookup_objc_ivar:                       # @lookup_objc_ivar
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp8:
	.size	lookup_objc_ivar, .Ltmp8-lookup_objc_ivar
	.cfi_endproc

	.globl	finish_file
	.align	16, 0x90
	.type	finish_file,@function
finish_file:                            # @finish_file
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	c_objc_common_finish_file  # TAILCALL
.Ltmp9:
	.size	finish_file, .Ltmp9-finish_file
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"GNU C"
	.size	.L.str, 6

	.type	lang_hooks,@object      # @lang_hooks
	.section	.rodata,"a",@progbits
	.globl	lang_hooks
	.align	8
lang_hooks:
	.quad	.L.str
	.quad	104                     # 0x68
	.quad	c_init_options
	.quad	c_decode_option
	.quad	c_common_post_options
	.quad	c_init
	.quad	c_common_finish
	.quad	lhd_clear_binding_stack
	.quad	c_common_get_alias_set
	.quad	lhd_return_tree
	.quad	c_safe_from_p
	.quad	c_staticp
	.byte	0                       # 0x0
	.zero	7
	.quad	lhd_do_nothing
	.quad	lhd_print_tree_nothing
	.quad	lhd_print_tree_nothing
	.quad	lhd_print_tree_nothing
	.quad	c_print_identifier
	.quad	c_set_yydebug
	.quad	lhd_tree_inlining_walk_subtrees
	.quad	c_cannot_inline_tree_fn
	.quad	c_disregard_inline_limits
	.quad	lhd_tree_inlining_add_pending_fn_decls
	.quad	lhd_tree_inlining_tree_chain_matters_p
	.quad	lhd_tree_inlining_auto_var_in_fn_p
	.quad	lhd_tree_inlining_copy_res_decl_for_inlining
	.quad	anon_aggr_type_p
	.quad	lhd_tree_inlining_start_inlining
	.quad	lhd_tree_inlining_end_inlining
	.quad	c_convert_parm_for_inlining
	.quad	lhd_tree_dump_dump_tree
	.quad	lhd_tree_dump_type_quals
	.size	lang_hooks, 256


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
