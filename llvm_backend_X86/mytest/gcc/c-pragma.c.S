	.file	"c-pragma.c.bc"
	.text
	.globl	maybe_apply_pragma_weak
	.align	16, 0x90
	.type	maybe_apply_pragma_weak,@function
maybe_apply_pragma_weak:                # @maybe_apply_pragma_weak
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
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB0_5
# BB#1:                                 # %entry
	cmpq	$34, %rcx
	jne	.LBB0_12
# BB#2:                                 # %land.lhs.true
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB0_5
# BB#3:                                 # %lor.lhs.false9
	testq	$256, 48(%r14)          # imm = 0x100
	jne	.LBB0_5
# BB#4:                                 # %lor.lhs.false9
	andq	$524288, %rax           # imm = 0x80000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_12
.LBB0_5:                                # %if.then
	movq	120(%r14), %rax
	testq	%rax, %rax
	jne	.LBB0_7
# BB#6:                                 # %cond.false
	movq	%r14, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r14), %rax
.LBB0_7:                                # %cond.end
	movq	pending_weaks(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB0_12
# BB#8:
	movabsq	$pending_weaks, %r15
	.align	16, 0x90
.LBB0_10:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB0_11
# BB#9:                                 # %for.cond
                                        #   in Loop: Header=BB0_10 Depth=1
	movq	%rbx, %r15
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_10
	jmp	.LBB0_12
.LBB0_11:                               # %if.then27
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	callq	apply_pragma_weak
	movq	(%rbx), %rax
	movq	%rax, (%r15)
.LBB0_12:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	maybe_apply_pragma_weak, .Ltmp10-maybe_apply_pragma_weak
	.cfi_endproc

	.align	16, 0x90
	.type	apply_pragma_weak,@function
apply_pragma_weak:                      # @apply_pragma_weak
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
	testq	%rsi, %rsi
	movq	%rdi, (%rsp)
	je	.LBB1_2
# BB#1:                                 # %if.then
	movq	32(%rsi), %rax
	movl	24(%rsi), %edi
	movq	%rax, %rsi
	callq	build_string
	movq	%rax, %r14
	movabsq	$.L.str16, %rdi
	callq	get_identifier
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	build_tree_list
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	leaq	(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	decl_attributes
	movq	(%rsp), %rdi
.LBB1_2:                                # %if.end
	movzbl	49(%rdi), %eax
	testq	$1, %rax
	je	.LBB1_8
# BB#3:                                 # %land.lhs.true
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB1_8
# BB#4:                                 # %land.lhs.true13
	movq	120(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB1_6
# BB#5:                                 # %cond.false
	callq	*lang_set_decl_assembler_name(%rip)
	movq	(%rsp), %rdi
	movq	120(%rdi), %rax
.LBB1_6:                                # %cond.end
	movzbl	18(%rax), %eax
	testq	$4, %rax
	je	.LBB1_8
# BB#7:                                 # %if.then22
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	callq	warning_with_decl
	movq	(%rsp), %rdi
.LBB1_8:                                # %if.end23
	callq	declare_weak
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp19:
	.size	apply_pragma_weak, .Ltmp19-apply_pragma_weak
	.cfi_endproc

	.globl	maybe_apply_renaming_pragma
	.align	16, 0x90
	.type	maybe_apply_renaming_pragma,@function
maybe_apply_renaming_pragma:            # @maybe_apply_renaming_pragma
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp25:
	.cfi_def_cfa_offset 32
.Ltmp26:
	.cfi_offset %rbx, -24
.Ltmp27:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB2_5
# BB#1:                                 # %entry
	cmpq	$34, %rcx
	jne	.LBB2_12
# BB#2:                                 # %land.lhs.true
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB2_5
# BB#3:                                 # %lor.lhs.false9
	testq	$256, 48(%rbx)          # imm = 0x100
	jne	.LBB2_5
# BB#4:                                 # %lor.lhs.false9
	andq	$524288, %rax           # imm = 0x80000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_12
.LBB2_5:                                # %if.then
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB2_7
# BB#6:                                 # %cond.false
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB2_7:                                # %cond.end
	movq	32(%rax), %rbx
	movzbl	(%rbx), %eax
	cmpq	$42, %rax
	jne	.LBB2_12
# BB#8:                                 # %if.then27
	incq	%rbx
	testq	%r14, %r14
	je	.LBB2_11
# BB#9:                                 # %land.lhs.true32
	movq	40(%r14), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_11
# BB#10:                                # %if.then35
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB2_11:                               # %if.end36
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %rdi
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build_string  # TAILCALL
.LBB2_12:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp28:
	.size	maybe_apply_renaming_pragma, .Ltmp28-maybe_apply_renaming_pragma
	.cfi_endproc

	.globl	init_pragma
	.align	16, 0x90
	.type	init_pragma,@function
init_pragma:                            # @init_pragma
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp30:
	.cfi_def_cfa_offset 16
	movq	parse_in(%rip), %rdi
	movabsq	$.L.str1, %rdx
	movabsq	$handle_pragma_pack, %rcx
	xorq	%rsi, %rsi
	callq	cpp_register_pragma
	movq	parse_in(%rip), %rdi
	movabsq	$.L.str2, %rdx
	movabsq	$handle_pragma_weak, %rcx
	xorq	%rsi, %rsi
	callq	cpp_register_pragma
	movabsq	$pending_weaks, %rdi
	movq	$1, %rsi
	popq	%rax
	jmp	ggc_add_tree_root  # TAILCALL
.Ltmp31:
	.size	init_pragma, .Ltmp31-init_pragma
	.cfi_endproc

	.align	16, 0x90
	.type	handle_pragma_pack,@function
handle_pragma_pack:                     # @handle_pragma_pack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp37:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp38:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp39:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp41:
	.cfi_def_cfa_offset 48
.Ltmp42:
	.cfi_offset %rbx, -40
.Ltmp43:
	.cfi_offset %r12, -32
.Ltmp44:
	.cfi_offset %r14, -24
.Ltmp45:
	.cfi_offset %r15, -16
	leaq	(%rsp), %rdi
	callq	c_lex
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	$22, %rax
	jne	.LBB4_1
# BB#2:                                 # %if.end
	leaq	(%rsp), %rdi
	callq	c_lex
	movq	%rax, %rcx
	andq	%r15, %rcx
	xorq	%r14, %r14
	cmpq	$23, %rcx
	je	.LBB4_3
# BB#10:                                # %if.end
	andq	%r15, %rax
	cmpq	$56, %rax
	jne	.LBB4_11
# BB#14:                                # %if.then17
	movq	(%rsp), %rax
	movq	32(%rax), %rbx
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	movq	$1, %r14
	je	.LBB4_17
# BB#15:                                # %if.else21
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	movq	$2, %r14
	je	.LBB4_17
# BB#16:                                # %do.body26
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	warning
	jmp	.LBB4_38
.LBB4_1:                                # %do.body
	movabsq	$.L.str5, %rdi
	jmp	.LBB4_37
.LBB4_3:
	xorq	%r12, %r12
	jmp	.LBB4_4
.LBB4_11:                               # %if.end
	cmpq	$57, %rax
	jne	.LBB4_13
# BB#12:                                # %if.then5
	movq	(%rsp), %rax
	movq	32(%rax), %r12
	leaq	(%rsp), %rdi
	callq	c_lex
	andq	%r15, %rax
	cmpq	$23, %rax
	je	.LBB4_4
.LBB4_13:                               # %do.body11
	movabsq	$.L.str6, %rdi
	jmp	.LBB4_37
.LBB4_17:                               # %if.end29
	leaq	(%rsp), %rdi
	callq	c_lex
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$21, %rcx
	je	.LBB4_20
# BB#18:                                # %if.end29
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$1, %rcx
	je	.LBB4_19
.LBB4_20:                               # %if.end47
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$21, %rcx
	movq	%r15, %r12
	jne	.LBB4_28
# BB#21:                                # %if.then50
	leaq	(%rsp), %rdi
	callq	c_lex
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r12, %rcx
	cmpq	$56, %rcx
	jne	.LBB4_25
# BB#22:                                # %if.then54
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r14, %rax
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB4_24
# BB#23:                                # %land.lhs.true57
	leaq	(%rsp), %rdi
	callq	c_lex
	andq	%rbx, %rax
	cmpq	$21, %rax
	jne	.LBB4_19
.LBB4_24:                               # %if.end73
	leaq	(%rsp), %rdi
	callq	c_lex
.LBB4_25:                               # %if.end75
	movq	%r14, %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	jne	.LBB4_28
# BB#26:                                # %if.then78
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$57, %rax
	jne	.LBB4_19
# BB#27:                                # %if.then81
	movq	(%rsp), %rax
	movq	32(%rax), %r12
	leaq	(%rsp), %rdi
	callq	c_lex
.LBB4_28:                               # %if.end101
	andq	%r15, %rax
	cmpq	$23, %rax
	jne	.LBB4_29
.LBB4_4:                                # %if.end122
	leaq	(%rsp), %rdi
	callq	c_lex
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$67, %rax
	je	.LBB4_6
# BB#5:                                 # %if.then126
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB4_6:                                # %if.end127
	movq	%r14, %rax
	andq	%rbx, %rax
	cmpq	$2, %rax
	je	.LBB4_36
# BB#7:                                 # %if.then130
	movq	%r12, %rcx
	andq	%rbx, %rcx
	cmpq	$16, %rcx
	ja	.LBB4_9
# BB#8:                                 # %if.then130
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$65815, %rdx            # imm = 0x10117
	je	.LBB4_9
# BB#31:                                # %if.end133
	cmpq	$2, %rax
	je	.LBB4_36
# BB#32:                                # %if.end133
	cmpq	$1, %rax
	jne	.LBB4_33
# BB#35:                                # %do.body136
	movabsq	$.L.str14, %rdi
	jmp	.LBB4_37
.LBB4_36:                               # %do.body139
	movabsq	$.L.str15, %rdi
	jmp	.LBB4_37
.LBB4_9:                                # %do.body131
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	warning
	jmp	.LBB4_38
.LBB4_29:                               # %do.body105
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	cmpq	$1, %r14
	jne	.LBB4_30
.LBB4_19:                               # %do.body40
	movabsq	$.L.str10, %rdi
.LBB4_37:                               # %sw.epilog141
	xorq	%rax, %rax
	callq	warning
.LBB4_38:                               # %sw.epilog141
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB4_33:                               # %if.end133
	testq	%rbx, %r14
	jne	.LBB4_38
# BB#34:                                # %sw.bb134
	movq	$3, %rcx
	shlq	%cl, %r12
	movl	%r12d, maximum_field_alignment(%rip)
	jmp	.LBB4_38
.LBB4_30:                               # %do.body112
	movabsq	$.L.str11, %rdi
	jmp	.LBB4_37
.Ltmp46:
	.size	handle_pragma_pack, .Ltmp46-handle_pragma_pack
	.cfi_endproc

	.align	16, 0x90
	.type	handle_pragma_weak,@function
handle_pragma_weak:                     # @handle_pragma_weak
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 48
.Ltmp51:
	.cfi_offset %rbx, -16
	movq	$0, 16(%rsp)
	leaq	24(%rsp), %rdi
	callq	c_lex
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$56, %rax
	jne	.LBB5_1
# BB#2:                                 # %if.end
	leaq	8(%rsp), %rdi
	callq	c_lex
	testq	%rbx, %rax
	jne	.LBB5_5
# BB#3:                                 # %if.then3
	leaq	16(%rsp), %rdi
	callq	c_lex
	andq	%rbx, %rax
	cmpq	$56, %rax
	jne	.LBB5_1
# BB#4:                                 # %if.end9
	leaq	8(%rsp), %rdi
	callq	c_lex
.LBB5_5:                                # %if.end11
	andq	%rbx, %rax
	cmpq	$67, %rax
	je	.LBB5_7
# BB#6:                                 # %if.then13
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB5_7:                                # %if.end14
	movq	24(%rsp), %rdi
	callq	identifier_global_value
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB5_11
# BB#8:                                 # %land.lhs.true
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB5_11
# BB#9:                                 # %if.then18
	movq	16(%rsp), %rsi
	movq	%rbx, %rdi
	callq	apply_pragma_weak
	movq	16(%rsp), %rsi
	testq	%rsi, %rsi
	je	.LBB5_12
# BB#10:                                # %if.then20
	movq	%rbx, %rdi
	callq	assemble_alias
	jmp	.LBB5_12
.LBB5_1:                                # %do.body
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB5_12
.LBB5_11:                               # %if.else
	movq	24(%rsp), %rdi
	movq	16(%rsp), %rsi
	movq	pending_weaks(%rip), %rdx
	callq	tree_cons
	movq	%rax, pending_weaks(%rip)
.LBB5_12:                               # %if.end23
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp52:
	.size	handle_pragma_weak, .Ltmp52-handle_pragma_weak
	.cfi_endproc

	.type	pending_weaks,@object   # @pending_weaks
	.local	pending_weaks
	.comm	pending_weaks,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"asm declaration conficts with previous rename"
	.size	.L.str, 46

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"pack"
	.size	.L.str1, 5

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"weak"
	.size	.L.str2, 5

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"malformed #pragma weak, ignored"
	.size	.L.str3, 32

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"junk at end of #pragma weak"
	.size	.L.str4, 28

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"missing '(' after '#pragma pack' - ignored"
	.size	.L.str5, 43

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"malformed '#pragma pack' - ignored"
	.size	.L.str6, 35

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"push"
	.size	.L.str7, 5

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"pop"
	.size	.L.str8, 4

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"unknown action '%s' for '#pragma pack' - ignored"
	.size	.L.str9, 49

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"malformed '#pragma pack(push[, id], <n>)' - ignored"
	.size	.L.str10, 52

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"malformed '#pragma pack(pop[, id])' - ignored"
	.size	.L.str11, 46

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"junk at end of '#pragma pack'"
	.size	.L.str12, 30

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"alignment must be a small power of two, not %d"
	.size	.L.str13, 47

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"#pragma pack(push[, id], <n>) is not supported on this target"
	.size	.L.str14, 62

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"#pragma pack(pop[, id], <n>) is not supported on this target"
	.size	.L.str15, 61

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"alias"
	.size	.L.str16, 6

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"applying #pragma weak `%s' after first use results in unspecified behavior"
	.size	.L.str17, 75


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
