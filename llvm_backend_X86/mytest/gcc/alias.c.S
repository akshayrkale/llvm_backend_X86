	.file	"alias.c.bc"
	.text
	.globl	alias_sets_conflict_p
	.align	16, 0x90
	.type	alias_sets_conflict_p,@function
alias_sets_conflict_p:                  # @alias_sets_conflict_p
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
	movq	%rsi, %rbx
	movq	%rdi, %r15
	testq	%r15, %r15
	movq	$1, %r14
	je	.LBB0_12
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB0_12
# BB#2:                                 # %entry
	cmpq	%rbx, %r15
	je	.LBB0_12
# BB#3:                                 # %if.end
	movq	alias_sets(%rip), %rdi
	movq	%r15, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB0_7
# BB#4:                                 # %get_alias_set_entry.exit
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_7
# BB#5:                                 # %land.lhs.true
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_12
# BB#6:                                 # %lor.lhs.false5
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	jne	.LBB0_12
.LBB0_7:                                # %if.end9
	movq	alias_sets(%rip), %rdi
	movq	%rbx, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB0_11
# BB#8:                                 # %get_alias_set_entry.exit38
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB0_11
# BB#9:                                 # %land.lhs.true12
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_12
# BB#10:                                # %lor.lhs.false15
	movq	8(%rax), %rdi
	movq	%r15, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	jne	.LBB0_12
.LBB0_11:                               # %if.end20
	xorq	%r14, %r14
.LBB0_12:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	alias_sets_conflict_p, .Ltmp10-alias_sets_conflict_p
	.cfi_endproc

	.globl	readonly_fields_p
	.align	16, 0x90
	.type	readonly_fields_p,@function
readonly_fields_p:                      # @readonly_fields_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 32
.Ltmp18:
	.cfi_offset %rbx, -32
.Ltmp19:
	.cfi_offset %r14, -24
.Ltmp20:
	.cfi_offset %r15, -16
	movzbl	16(%rdi), %eax
	addq	$-20, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB1_7
# BB#1:                                 # %if.end
	movq	24(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB1_7
# BB#2:                                 # %for.body.lr.ph
	movq	$1, %r15
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	jne	.LBB1_6
# BB#4:                                 # %land.lhs.true19
                                        #   in Loop: Header=BB1_3 Depth=1
	testq	$4096, %rax             # imm = 0x1000
	jne	.LBB1_8
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	8(%rbx), %rdi
	callq	readonly_fields_p
	testq	%r14, %rax
	jne	.LBB1_8
.LBB1_6:                                # %for.inc
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_3
.LBB1_7:
	xorq	%r15, %r15
.LBB1_8:                                # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp21:
	.size	readonly_fields_p, .Ltmp21-readonly_fields_p
	.cfi_endproc

	.globl	objects_must_conflict_p
	.align	16, 0x90
	.type	objects_must_conflict_p,@function
objects_must_conflict_p:                # @objects_must_conflict_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp31:
	.cfi_def_cfa_offset 48
.Ltmp32:
	.cfi_offset %rbx, -40
.Ltmp33:
	.cfi_offset %r12, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%r15, %r15
	movq	%rbx, %rax
	orq	%r14, %rax
	je	.LBB2_17
# BB#1:                                 # %if.end
	testq	%rbx, %rbx
	je	.LBB2_3
# BB#2:                                 # %land.lhs.true3
	movq	%rbx, %rdi
	callq	readonly_fields_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_17
.LBB2_3:                                # %lor.lhs.false
	testq	%r14, %r14
	je	.LBB2_5
# BB#4:                                 # %land.lhs.true5
	movq	%r14, %rdi
	callq	readonly_fields_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_17
.LBB2_5:                                # %lor.lhs.false8
	testq	%rbx, %rbx
	je	.LBB2_7
# BB#6:                                 # %land.lhs.true10
	movzbl	17(%rbx), %eax
	testq	$16, %rax
	jne	.LBB2_17
.LBB2_7:                                # %lor.lhs.false12
	testq	%r14, %r14
	je	.LBB2_9
# BB#8:                                 # %land.lhs.true14
	movzbl	17(%r14), %eax
	testq	$16, %rax
	jne	.LBB2_17
.LBB2_9:                                # %if.end22
	movq	$1, %r15
	cmpq	%r14, %rbx
	je	.LBB2_17
# BB#10:                                # %lor.lhs.false24
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB2_15
# BB#11:                                # %land.lhs.true26
	movl	16(%rbx), %eax
	testq	$2048, %rax             # imm = 0x800
	je	.LBB2_14
# BB#12:                                # %land.lhs.true26
	testq	%r14, %r14
	je	.LBB2_14
# BB#13:                                # %land.lhs.true34
	movzbl	17(%r14), %ecx
	testq	$8, %rcx
	jne	.LBB2_17
.LBB2_14:                               # %land.rhs
	andq	$255, %rax
	addq	$-18, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB2_15:                               # %land.end
	testq	%r14, %r14
	je	.LBB2_16
# BB#18:                                # %land.end103
	movzbl	16(%r14), %ecx
	addq	$-18, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$5, %rdx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	xorq	%r15, %r15
	xorq	%rcx, %rax
	jne	.LBB2_17
	jmp	.LBB2_19
.LBB2_16:                               # %land.end.land.end103_crit_edge
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB2_19
.LBB2_17:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB2_19:                               # %if.end107
	xorq	%r15, %r15
	testq	%rbx, %rbx
	movabsq	$0, %r12
	je	.LBB2_21
# BB#20:                                # %cond.true
	movq	%rbx, %rdi
	callq	get_alias_set
	movq	%rax, %r12
.LBB2_21:                               # %cond.end
	testq	%r14, %r14
	je	.LBB2_23
# BB#22:                                # %cond.true111
	movq	%r14, %rdi
	callq	get_alias_set
	movq	%rax, %r15
.LBB2_23:                               # %cond.end114
	movq	%r12, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	alias_sets_conflict_p  # TAILCALL
.Ltmp36:
	.size	objects_must_conflict_p, .Ltmp36-objects_must_conflict_p
	.cfi_endproc

	.globl	get_alias_set
	.align	16, 0x90
	.type	get_alias_set,@function
get_alias_set:                          # @get_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp46:
	.cfi_def_cfa_offset 48
.Ltmp47:
	.cfi_offset %rbx, -40
.Ltmp48:
	.cfi_offset %r12, -32
.Ltmp49:
	.cfi_offset %r14, -24
.Ltmp50:
	.cfi_offset %r15, -16
	movl	flag_strict_aliasing(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB3_1
# BB#2:                                 # %lor.lhs.false
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB3_3
# BB#4:                                 # %lor.lhs.false1
	movl	16(%rbx), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %edx
	cmpq	$116, %rdx
	je	.LBB3_58
# BB#5:                                 # %land.lhs.true
	movq	8(%rbx), %rdx
	testq	%rdx, %rdx
	je	.LBB3_6
# BB#7:                                 # %land.lhs.true
	cmpq	%rax, %rdx
	je	.LBB3_8
# BB#9:                                 # %if.then21
	movq	$0, (%rsp)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rbx, %r14
	jmp	.LBB3_10
	.align	16, 0x90
.LBB3_13:                               # %land.rhs.while.cond_crit_edge
                                        #   in Loop: Header=BB3_10 Depth=1
	movl	16(%rdx), %ecx
	movq	%rdx, %r14
.LBB3_10:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	addq	$-114, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	ja	.LBB3_14
# BB#11:                                # %land.lhs.true42
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	32(%r14), %rdx
	cmpq	%rax, %rdx
	je	.LBB3_14
# BB#12:                                # %land.rhs
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	8(%r14), %rcx
	movq	8(%rdx), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %esi
	xorq	%rcx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB3_13
.LBB3_14:                               # %while.end
	movq	%r14, %rdi
	callq	*lang_hooks+64(%rip)
	movq	%rax, %r15
	cmpq	$-1, %r15
	jne	.LBB3_68
# BB#15:                                # %while.cond70.preheader
	movl	16(%rbx), %ecx
	leaq	(%rsp), %r15
	jmp	.LBB3_16
	.align	16, 0x90
.LBB3_23:                               #   in Loop: Header=BB3_16 Depth=1
	movq	%rax, %rbx
.LBB3_16:                               # %while.cond70
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_22 Depth 2
	andq	$255, %rcx
	cmpq	$57, %rcx
	je	.LBB3_19
# BB#17:                                # %lor.end
                                        #   in Loop: Header=BB3_16 Depth=1
	movq	%rbx, %rdi
	callq	handled_component_p
	movzbl	16(%rbx), %ecx
	testq	%r12, %rax
	je	.LBB3_27
# BB#18:                                # %while.body79
                                        #   in Loop: Header=BB3_16 Depth=1
	cmpq	$57, %rcx
	jne	.LBB3_20
.LBB3_19:                               # %if.then86
                                        #   in Loop: Header=BB3_16 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	find_placeholder
	jmp	.LBB3_21
	.align	16, 0x90
.LBB3_20:                               # %if.else
                                        #   in Loop: Header=BB3_16 Depth=1
	movq	32(%rbx), %rax
.LBB3_21:                               # %while.cond92.preheader
                                        #   in Loop: Header=BB3_16 Depth=1
	movq	global_trees(%rip), %rdx
	.align	16, 0x90
.LBB3_22:                               # %while.cond92
                                        #   Parent Loop BB3_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	16(%rax), %ecx
	movq	%rcx, %rsi
	andq	$255, %rsi
	addq	$-114, %rsi
	andq	%r12, %rsi
	cmpq	$2, %rsi
	ja	.LBB3_23
# BB#24:                                # %land.lhs.true113
                                        #   in Loop: Header=BB3_22 Depth=2
	movq	32(%rax), %rsi
	cmpq	%rdx, %rsi
	je	.LBB3_25
# BB#26:                                # %land.rhs119
                                        #   in Loop: Header=BB3_22 Depth=2
	movq	8(%rax), %rdi
	movq	8(%rsi), %rbx
	movl	60(%rdi), %edi
	movl	60(%rbx), %ebx
	xorq	%rdi, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	movq	%rax, %rbx
	movq	%rsi, %rax
	je	.LBB3_22
	jmp	.LBB3_16
	.align	16, 0x90
.LBB3_25:                               #   in Loop: Header=BB3_16 Depth=1
	movq	%rax, %rbx
	jmp	.LBB3_16
.LBB3_1:
	xorq	%r15, %r15
	jmp	.LBB3_68
.LBB3_3:
	xorq	%r15, %r15
	jmp	.LBB3_68
.LBB3_6:
	xorq	%r15, %r15
	jmp	.LBB3_68
.LBB3_8:
	xorq	%r15, %r15
	jmp	.LBB3_68
.LBB3_27:                               # %while.end145
	cmpq	$41, %rcx
	jne	.LBB3_38
# BB#28:                                # %if.then152
	movq	32(%rbx), %rdi
	callq	find_base_decl
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB3_36
# BB#29:                                # %land.lhs.true158
	movq	192(%r15), %rax
	cmpq	$-1, %rax
	jne	.LBB3_30
.LBB3_36:                               # %if.else186
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	jne	.LBB3_38
# BB#37:
	xorq	%r15, %r15
	jmp	.LBB3_68
.LBB3_38:                               # %if.end198
	movq	$0, (%rsp)
	movl	16(%r14), %ecx
	leaq	(%rsp), %r15
	jmp	.LBB3_39
	.align	16, 0x90
.LBB3_53:                               #   in Loop: Header=BB3_39 Depth=1
	movq	%rax, %r14
.LBB3_39:                               # %while.cond199
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_52 Depth 2
	andq	$255, %rcx
	cmpq	$57, %rcx
	je	.LBB3_49
# BB#40:                                # %lor.rhs206
                                        #   in Loop: Header=BB3_39 Depth=1
	movq	%r14, %rdi
	callq	handled_component_p
	testq	%r12, %rax
	je	.LBB3_41
# BB#47:                                # %lor.end213
                                        #   in Loop: Header=BB3_39 Depth=1
	movq	%r14, %rdi
	callq	can_address_p
	movzbl	16(%r14), %ecx
	testq	%r12, %rax
	jne	.LBB3_42
# BB#48:                                # %while.body214
                                        #   in Loop: Header=BB3_39 Depth=1
	cmpq	$57, %rcx
	jne	.LBB3_50
.LBB3_49:                               # %if.then221
                                        #   in Loop: Header=BB3_39 Depth=1
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	find_placeholder
	jmp	.LBB3_51
.LBB3_50:                               # %if.else223
                                        #   in Loop: Header=BB3_39 Depth=1
	movq	32(%r14), %rax
.LBB3_51:                               # %while.cond228.preheader
                                        #   in Loop: Header=BB3_39 Depth=1
	movq	global_trees(%rip), %rdx
	.align	16, 0x90
.LBB3_52:                               # %while.cond228
                                        #   Parent Loop BB3_39 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	16(%rax), %ecx
	movq	%rcx, %rsi
	andq	$255, %rsi
	addq	$-114, %rsi
	andq	%r12, %rsi
	cmpq	$2, %rsi
	ja	.LBB3_53
# BB#54:                                # %land.lhs.true249
                                        #   in Loop: Header=BB3_52 Depth=2
	movq	32(%rax), %rsi
	cmpq	%rdx, %rsi
	je	.LBB3_55
# BB#56:                                # %land.rhs255
                                        #   in Loop: Header=BB3_52 Depth=2
	movq	8(%rax), %rdi
	movq	8(%rsi), %rbx
	movl	60(%rdi), %edi
	movl	60(%rbx), %ebx
	xorq	%rdi, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	movq	%rax, %r14
	movq	%rsi, %rax
	je	.LBB3_52
	jmp	.LBB3_39
	.align	16, 0x90
.LBB3_55:                               #   in Loop: Header=BB3_39 Depth=1
	movq	%rax, %r14
	jmp	.LBB3_39
.LBB3_41:                               # %lor.end213.thread518
	movzbl	16(%r14), %ecx
.LBB3_42:                               # %while.end281
	cmpq	$34, %rcx
	jne	.LBB3_57
# BB#43:                                # %land.lhs.true288
	movq	144(%r14), %rax
	testq	%rax, %rax
	je	.LBB3_57
# BB#44:                                # %cond.end
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB3_57
# BB#45:                                # %cond.end314
	movq	16(%rax), %rax
	testq	%rax, %rax
	movabsq	$0, %r15
	je	.LBB3_68
# BB#46:                                # %cond.end330
	movq	(%rax), %r15
	jmp	.LBB3_68
.LBB3_57:                               # %if.end337
	movq	8(%r14), %rbx
.LBB3_58:                               # %if.end340
	movq	128(%rbx), %rbx
	movq	152(%rbx), %r15
	cmpq	$-1, %r15
	jne	.LBB3_68
# BB#59:                                # %if.end348
	movq	%rbx, %rdi
	callq	*lang_hooks+64(%rip)
	movq	%rax, %r15
	cmpq	$-1, %r15
	jne	.LBB3_68
# BB#60:                                # %if.else353
	movzbl	16(%rbx), %eax
	xorq	%r15, %r15
	cmpq	$23, %rax
	je	.LBB3_65
# BB#61:                                # %if.else353
	cmpq	$9, %rax
	jne	.LBB3_63
# BB#62:                                # %if.then368
	movq	8(%rbx), %rdi
	callq	get_alias_set
	movq	%rax, %r15
	jmp	.LBB3_65
.LBB3_63:                               # %if.else372
	movl	flag_strict_aliasing(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_65
# BB#64:                                # %if.then.i
	movq	new_alias_set.last_alias_set(%rip), %r15
	incq	%r15
	movq	%r15, new_alias_set.last_alias_set(%rip)
.LBB3_65:                               # %if.end376
	movq	%r15, 152(%rbx)
	movzbl	16(%rbx), %ecx
	cmpq	$22, %rcx
	ja	.LBB3_68
# BB#66:                                # %if.end376
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$8126720, %rax          # imm = 0x7C0100
	je	.LBB3_68
# BB#67:                                # %if.then420
	movq	%rbx, %rdi
	callq	record_component_aliases
	jmp	.LBB3_68
.LBB3_30:                               # %land.lhs.true158
	cmpq	$-2, %rax
	jne	.LBB3_35
# BB#31:                                # %if.then167
	movq	8(%r15), %rax
	movq	8(%rax), %rdi
	callq	get_alias_set
	testq	%rax, %rax
	je	.LBB3_35
# BB#32:                                # %if.else176
	movl	flag_strict_aliasing(%rip), %ecx
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB3_34
# BB#33:                                # %if.then.i509
	movq	new_alias_set.last_alias_set(%rip), %rsi
	incq	%rsi
	movq	%rsi, new_alias_set.last_alias_set(%rip)
.LBB3_34:                               # %new_alias_set.exit511
	movq	%rsi, 192(%r15)
	movq	%rax, %rdi
	callq	record_alias_subset
.LBB3_35:                               # %if.end183
	movq	192(%r15), %r15
.LBB3_68:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp51:
	.size	get_alias_set, .Ltmp51-get_alias_set
	.cfi_endproc

	.globl	can_address_p
	.align	16, 0x90
	.type	can_address_p,@function
can_address_p:                          # @can_address_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp58:
	.cfi_def_cfa_offset 32
.Ltmp59:
	.cfi_offset %rbx, -32
.Ltmp60:
	.cfi_offset %r14, -24
.Ltmp61:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	handled_component_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	movq	$1, %r14
	je	.LBB4_11
# BB#1:                                 # %if.else
	movzbl	16(%rbx), %eax
	cmpq	$40, %rax
	je	.LBB4_10
# BB#2:                                 # %if.else
	cmpq	$39, %rax
	jne	.LBB4_6
# BB#3:                                 # %land.lhs.true
	movq	40(%rbx), %rax
	movzbl	52(%rax), %eax
	testq	$4, %rax
	jne	.LBB4_6
# BB#4:                                 # %land.lhs.true11
	movq	32(%rbx), %rax
	movq	8(%rax), %rdi
	callq	get_alias_set
	testq	%rax, %rax
	je	.LBB4_6
# BB#5:                                 # %land.lhs.true18
	movq	32(%rbx), %rdi
	callq	can_address_p
	testq	%r15, %rax
	jne	.LBB4_11
.LBB4_6:                                # %if.else25
	movzbl	16(%rbx), %eax
	addq	$-43, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	ja	.LBB4_10
# BB#7:                                 # %land.lhs.true36
	movq	32(%rbx), %rax
	movq	8(%rax), %rdi
	movzbl	62(%rdi), %eax
	testq	$8, %rax
	jne	.LBB4_10
# BB#8:                                 # %land.lhs.true47
	callq	get_alias_set
	testq	%rax, %rax
	je	.LBB4_10
# BB#9:                                 # %land.lhs.true55
	movq	32(%rbx), %rdi
	callq	can_address_p
	testq	%r15, %rax
	jne	.LBB4_11
.LBB4_10:                               # %if.end64
	xorq	%r14, %r14
.LBB4_11:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp62:
	.size	can_address_p, .Ltmp62-can_address_p
	.cfi_endproc

	.align	16, 0x90
	.type	find_base_decl,@function
find_base_decl:                         # @find_base_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp67:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp68:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp69:
	.cfi_def_cfa_offset 32
.Ltmp70:
	.cfi_offset %rbx, -32
.Ltmp71:
	.cfi_offset %r14, -24
.Ltmp72:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB5_28
# BB#1:                                 # %entry
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB5_28
# BB#2:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_3:                                # %lor.lhs.false2
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rcx
	movl	16(%rcx), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB5_28
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB5_3 Depth=1
	movzbl	16(%rbx), %ecx
	movzbl	tree_code_type(%rcx), %edi
	cmpq	$100, %rdi
	je	.LBB5_5
# BB#6:                                 # %if.end20
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	$56, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$51, %rdi
	je	.LBB5_16
# BB#7:                                 # %if.end20
                                        #   in Loop: Header=BB5_3 Depth=1
	andq	%rdx, %rsi
	cmpq	$50, %rsi
	je	.LBB5_11
# BB#8:                                 # %if.end20
                                        #   in Loop: Header=BB5_3 Depth=1
	cmpq	$49, %rsi
	jne	.LBB5_28
# BB#9:                                 # %sw.bb
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB5_28
# BB#10:                                # %sw.bb
                                        #   in Loop: Header=BB5_3 Depth=1
	cmpq	%rbx, %rax
	jne	.LBB5_3
	jmp	.LBB5_28
.LBB5_5:
	movq	%rbx, %r14
.LBB5_28:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB5_16:                               # %sw.bb49
	movq	32(%rbx), %rdi
	callq	find_base_decl
	movq	40(%rbx), %rdi
	movq	%rax, %r15
	callq	find_base_decl
	movq	48(%rbx), %rdi
	movq	%rax, %rbx
	callq	find_base_decl
	testq	%rbx, %rbx
	movq	%rax, %rdx
	je	.LBB5_18
# BB#17:                                # %sw.bb49
	movq	%rbx, %rdx
.LBB5_18:                               # %sw.bb49
	testq	%r15, %r15
	movq	%rdx, %rcx
	je	.LBB5_20
# BB#19:                                # %sw.bb49
	movq	%r15, %rcx
.LBB5_20:                               # %sw.bb49
	testq	%rdx, %rdx
	movq	%rcx, %rsi
	je	.LBB5_22
# BB#21:                                # %sw.bb49
	movq	%rdx, %rsi
.LBB5_22:                               # %sw.bb49
	cmpq	%rsi, %rcx
	jne	.LBB5_28
# BB#23:                                # %land.lhs.true
	testq	%rax, %rax
	movq	%rcx, %rdx
	je	.LBB5_25
# BB#24:                                # %land.lhs.true
	movq	%rax, %rdx
.LBB5_25:                               # %land.lhs.true
	cmpq	%rdx, %rcx
	je	.LBB5_27
# BB#26:                                # %land.lhs.true
	xorq	%rcx, %rcx
.LBB5_27:                               # %land.lhs.true
	movq	%rcx, %r14
	jmp	.LBB5_28
.LBB5_11:                               # %sw.bb29
	movq	32(%rbx), %rdi
	callq	find_base_decl
	movq	40(%rbx), %rdi
	movq	%rax, %r14
	callq	find_base_decl
	cmpq	%rax, %r14
	je	.LBB5_28
# BB#12:                                # %if.else
	testq	%r14, %r14
	je	.LBB5_13
# BB#14:                                # %if.else44
	testq	%rax, %rax
	je	.LBB5_28
# BB#15:                                # %if.else44
	xorq	%r14, %r14
	jmp	.LBB5_28
.LBB5_13:
	movq	%rax, %r14
	jmp	.LBB5_28
.Ltmp73:
	.size	find_base_decl, .Ltmp73-find_base_decl
	.cfi_endproc

	.globl	new_alias_set
	.align	16, 0x90
	.type	new_alias_set,@function
new_alias_set:                          # @new_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	movl	flag_strict_aliasing(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB6_2
# BB#1:                                 # %if.then
	movq	new_alias_set.last_alias_set(%rip), %rax
	incq	%rax
	movq	%rax, new_alias_set.last_alias_set(%rip)
.LBB6_2:                                # %return
	retq
.Ltmp74:
	.size	new_alias_set, .Ltmp74-new_alias_set
	.cfi_endproc

	.globl	record_alias_subset
	.align	16, 0x90
	.type	record_alias_subset,@function
record_alias_subset:                    # @record_alias_subset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp80:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp81:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp84:
	.cfi_def_cfa_offset 48
.Ltmp85:
	.cfi_offset %rbx, -40
.Ltmp86:
	.cfi_offset %r12, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	cmpq	%r14, %rbx
	je	.LBB7_7
# BB#1:                                 # %if.end
	testq	%rbx, %rbx
	je	.LBB7_14
# BB#2:                                 # %if.end3
	movq	alias_sets(%rip), %rdi
	movq	%rbx, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB7_4
# BB#3:                                 # %get_alias_set_entry.exit
	movq	8(%rax), %r15
	testq	%r15, %r15
	jne	.LBB7_5
.LBB7_4:                                # %if.then5
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%rbx, (%r15)
	movabsq	$splay_tree_compare_ints, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	callq	splay_tree_new
	movq	alias_sets(%rip), %rdi
	movq	%rax, 8(%r15)
	movl	%r12d, 16(%r15)
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	splay_tree_insert
.LBB7_5:                                # %if.end9
	testq	%r14, %r14
	je	.LBB7_6
# BB#8:                                 # %if.else
	movq	alias_sets(%rip), %rdi
	movq	%r14, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB7_13
# BB#9:                                 # %get_alias_set_entry.exit49
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB7_13
# BB#10:                                # %if.then14
	movl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB7_12
# BB#11:                                # %if.then17
	movq	$1, %rcx
	movl	%ecx, 16(%r15)
.LBB7_12:                               # %if.end19
	movq	8(%rax), %rdi
	movq	8(%r15), %rdx
	movabsq	$insert_subset_children, %rsi
	callq	splay_tree_foreach
.LBB7_13:                               # %if.end23
	movq	8(%r15), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	splay_tree_insert  # TAILCALL
.LBB7_6:                                # %if.then11
	movq	$1, %rax
	movl	%eax, 16(%r15)
.LBB7_7:                                # %if.end26
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB7_14:                               # %if.then2
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.record_alias_subset, %rcx
	movq	$625, %rsi              # imm = 0x271
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp89:
	.size	record_alias_subset, .Ltmp89-record_alias_subset
	.cfi_endproc

	.globl	record_component_aliases
	.align	16, 0x90
	.type	record_component_aliases,@function
record_component_aliases:               # @record_component_aliases
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp95:
	.cfi_def_cfa_offset 32
.Ltmp96:
	.cfi_offset %rbx, -24
.Ltmp97:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	get_alias_set
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB8_12
# BB#1:                                 # %if.end
	movzbl	16(%rbx), %eax
	leaq	-20(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	jae	.LBB8_2
# BB#6:                                 # %sw.bb9
	movq	24(%rbx), %rbx
	jmp	.LBB8_7
	.align	16, 0x90
.LBB8_11:                               # %for.inc
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	(%rbx), %rbx
.LBB8_7:                                # %sw.bb9
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB8_12
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB8_7 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB8_11
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB8_7 Depth=1
	movzbl	52(%rbx), %eax
	testq	$4, %rax
	jne	.LBB8_11
# BB#10:                                # %if.then21
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	8(%rbx), %rdi
	callq	get_alias_set
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	record_alias_subset
	jmp	.LBB8_11
.LBB8_2:                                # %if.end
	cmpq	$8, %rax
	je	.LBB8_5
# BB#3:                                 # %if.end
	cmpq	$18, %rax
	jne	.LBB8_12
# BB#4:                                 # %sw.bb
	movzbl	62(%rbx), %eax
	testq	$8, %rax
	je	.LBB8_5
.LBB8_12:                               # %sw.epilog
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB8_5:                                # %if.then4
	movq	8(%rbx), %rdi
	callq	get_alias_set
	movq	%r14, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	record_alias_subset  # TAILCALL
.Ltmp98:
	.size	record_component_aliases, .Ltmp98-record_component_aliases
	.cfi_endproc

	.align	16, 0x90
	.type	insert_subset_children,@function
insert_subset_children:                 # @insert_subset_children
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp100:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rax
	movq	8(%rdi), %rdx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	splay_tree_insert
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp101:
	.size	insert_subset_children, .Ltmp101-insert_subset_children
	.cfi_endproc

	.globl	get_varargs_alias_set
	.align	16, 0x90
	.type	get_varargs_alias_set,@function
get_varargs_alias_set:                  # @get_varargs_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	movq	get_varargs_alias_set.set(%rip), %rax
	cmpq	$-1, %rax
	jne	.LBB10_4
# BB#1:                                 # %if.then
	movl	flag_strict_aliasing(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB10_3
# BB#2:                                 # %if.then.i
	movq	new_alias_set.last_alias_set(%rip), %rax
	incq	%rax
	movq	%rax, new_alias_set.last_alias_set(%rip)
.LBB10_3:                               # %new_alias_set.exit
	movq	%rax, get_varargs_alias_set.set(%rip)
.LBB10_4:                               # %if.end
	retq
.Ltmp102:
	.size	get_varargs_alias_set, .Ltmp102-get_varargs_alias_set
	.cfi_endproc

	.globl	get_frame_alias_set
	.align	16, 0x90
	.type	get_frame_alias_set,@function
get_frame_alias_set:                    # @get_frame_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	movq	get_frame_alias_set.set(%rip), %rax
	cmpq	$-1, %rax
	jne	.LBB11_4
# BB#1:                                 # %if.then
	movl	flag_strict_aliasing(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB11_3
# BB#2:                                 # %if.then.i
	movq	new_alias_set.last_alias_set(%rip), %rax
	incq	%rax
	movq	%rax, new_alias_set.last_alias_set(%rip)
.LBB11_3:                               # %new_alias_set.exit
	movq	%rax, get_frame_alias_set.set(%rip)
.LBB11_4:                               # %if.end
	retq
.Ltmp103:
	.size	get_frame_alias_set, .Ltmp103-get_frame_alias_set
	.cfi_endproc

	.globl	record_base_value
	.align	16, 0x90
	.type	record_base_value,@function
record_base_value:                      # @record_base_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp107:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp108:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp109:
	.cfi_def_cfa_offset 32
.Ltmp110:
	.cfi_offset %rbx, -24
.Ltmp111:
	.cfi_offset %r14, -16
	movl	reg_base_value_size(%rip), %eax
	movq	%rdi, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jbe	.LBB12_8
# BB#1:                                 # %if.end
	testq	%r14, %rdx
	je	.LBB12_4
# BB#2:                                 # %if.end
	movq	alias_invariant(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB12_4
# BB#3:                                 # %if.then2
	movq	%rbx, %rdx
	andq	%r14, %rdx
	movq	%rsi, (%rcx,%rdx,8)
.LBB12_4:                               # %if.end3
	movzbl	(%rsi), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_7
# BB#5:                                 # %if.then5
	movl	8(%rsi), %ecx
	cmpq	%rax, %rcx
	jae	.LBB12_8
# BB#6:                                 # %if.then8
	movq	reg_base_value(%rip), %rax
	movq	(%rax,%rcx,8), %rcx
	andq	%r14, %rbx
	movq	%rcx, (%rax,%rbx,8)
	jmp	.LBB12_8
.LBB12_7:                               # %if.end17
	movq	%rsi, %rdi
	callq	find_base_value
	movq	reg_base_value(%rip), %rcx
	andq	%r14, %rbx
	movq	%rax, (%rcx,%rbx,8)
.LBB12_8:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp112:
	.size	record_base_value, .Ltmp112-record_base_value
	.cfi_endproc

	.align	16, 0x90
	.type	find_base_value,@function
find_base_value:                        # @find_base_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp119:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp120:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp121:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp122:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp123:
	.cfi_def_cfa_offset 48
.Ltmp124:
	.cfi_offset %rbx, -48
.Ltmp125:
	.cfi_offset %r12, -40
.Ltmp126:
	.cfi_offset %r13, -32
.Ltmp127:
	.cfi_offset %r14, -24
.Ltmp128:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$2147549183, %r13       # imm = 0x8000FFFF
	movabsq	$2147483709, %rdi       # imm = 0x8000003D
	movabsq	$414331165718085632, %r8 # imm = 0x5C0000000000000
	jmp	.LBB13_1
	.align	16, 0x90
.LBB13_14:                              # %sw.bb300
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	8(%rbx), %rbx
.LBB13_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$133, %rcx
	jg	.LBB13_15
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$119, %rcx
	jg	.LBB13_11
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$95, %rcx
	jg	.LBB13_9
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$82, %rcx
	jg	.LBB13_72
# BB#5:                                 # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$74, %rcx
	jg	.LBB13_8
# BB#6:                                 # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	addq	$-58, %rcx
	andq	%r12, %rcx
	cmpq	$10, %rcx
	ja	.LBB13_10
# BB#7:                                 # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%rbx, %rdx
	jmpq	*.LJTI13_0(,%rcx,8)
.LBB13_35:                              # %sw.bb44
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %ecx
	addq	$-75, %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	jbe	.LBB13_36
	jmp	.LBB13_10
	.align	16, 0x90
.LBB13_15:                              # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB13_14
# BB#16:                                # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$135, %rcx
	jne	.LBB13_10
# BB#17:                                # %sw.bb253
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	16(%rbx), %rbx
	jmp	.LBB13_1
	.align	16, 0x90
.LBB13_11:                              # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	leaq	-120(%rcx), %rsi
	andq	%r12, %rsi
	cmpq	$2, %rsi
	jb	.LBB13_14
# BB#12:                                # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$122, %rcx
	jne	.LBB13_10
# BB#13:                                # %sw.bb280
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	movl	target_flags(%rip), %esi
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movzbl	mode_size+4(%rsi), %ecx
	cmpq	%rcx, %rdx
	jae	.LBB13_14
	jmp	.LBB13_10
.LBB13_9:                               # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	addq	$-96, %rcx
	andq	%r12, %rcx
	cmpq	$6, %rcx
	jb	.LBB13_14
	jmp	.LBB13_10
.LBB13_72:                              # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$83, %rcx
	jne	.LBB13_10
# BB#73:                                # %sw.bb258
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	xorq	%rdx, %rdx
	cmpq	$54, %rcx
	jne	.LBB13_69
# BB#74:                                # %land.lhs.true266
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$0, 8(%rax)
	je	.LBB13_69
# BB#75:                                # %if.then274
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	8(%rbx), %rbx
	jmp	.LBB13_1
.LBB13_8:                               # %tailrecurse
                                        #   in Loop: Header=BB13_1 Depth=1
	addq	$-75, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jae	.LBB13_10
.LBB13_36:                              # %sw.bb59
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	8(%rbx), %r15
	movl	(%r15), %eax
	movq	16(%rbx), %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	andq	%r13, %rax
	cmpq	%rdi, %rax
	movq	%r15, %rbx
	je	.LBB13_1
# BB#37:                                # %if.end75
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	(%r14), %eax
	movq	%rax, %rdx
	andq	%r13, %rdx
	cmpq	%rdi, %rdx
	movq	%r14, %rbx
	je	.LBB13_1
# BB#38:                                # %if.end86
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$61, %rcx
	jne	.LBB13_39
# BB#40:                                # %if.then91
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%r15, %rdi
	callq	find_base_value
	testq	%rax, %rax
	je	.LBB13_42
# BB#41:                                # %if.then91
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%rax, %r15
.LBB13_42:                              # %if.then91
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	(%r14), %eax
	movq	%r15, %rbx
	movabsq	$2147483709, %rdi       # imm = 0x8000003D
	movabsq	$414331165718085632, %r8 # imm = 0x5C0000000000000
	jmp	.LBB13_43
.LBB13_39:                              #   in Loop: Header=BB13_1 Depth=1
	movq	%r15, %rbx
.LBB13_43:                              # %if.end97
                                        #   in Loop: Header=BB13_1 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB13_47
# BB#44:                                # %if.then102
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%r13, %r15
	movq	%r14, %rdi
	callq	find_base_value
	testq	%rax, %rax
	je	.LBB13_46
# BB#45:                                # %if.then102
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%rax, %r14
.LBB13_46:                              # %if.then102
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%r15, %r13
	movabsq	$2147483709, %rdi       # imm = 0x8000003D
	movabsq	$414331165718085632, %r8 # imm = 0x5C0000000000000
.LBB13_47:                              # %if.end108
                                        #   in Loop: Header=BB13_1 Depth=1
	testq	%rbx, %rbx
	je	.LBB13_53
# BB#48:                                # %land.lhs.true111
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-67(%rcx), %rdx
	andq	%r12, %rdx
	cmpq	$2, %rdx
	jb	.LBB13_49
# BB#50:                                # %land.lhs.true111
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$25, %rcx
	jne	.LBB13_53
# BB#51:                                # %land.lhs.true126
                                        #   in Loop: Header=BB13_1 Depth=1
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB13_52
.LBB13_53:                              # %if.end133
                                        #   in Loop: Header=BB13_1 Depth=1
	xorq	%rax, %rax
	testq	%r14, %r14
	je	.LBB13_60
# BB#54:                                # %land.lhs.true136
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	(%r14), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-67(%rax), %rdx
	andq	%r12, %rdx
	cmpq	$2, %rdx
	jb	.LBB13_55
# BB#56:                                # %land.lhs.true136
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$25, %rax
	jne	.LBB13_59
# BB#57:                                # %land.lhs.true151
                                        #   in Loop: Header=BB13_1 Depth=1
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB13_58
.LBB13_59:                              # %if.end158
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$54, %rax
	je	.LBB13_1
.LBB13_60:                              # %lor.lhs.false163
                                        #   in Loop: Header=BB13_1 Depth=1
	movzbl	(%rbx), %ecx
	leaq	-67(%rcx), %rdx
	andq	%r12, %rdx
	cmpq	$2, %rdx
	jb	.LBB13_1
# BB#61:                                # %lor.lhs.false173
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$133, %rcx
	jg	.LBB13_64
# BB#62:                                # %lor.lhs.false173
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$58, %rcx
	ja	.LBB13_66
# BB#63:                                # %lor.lhs.false173
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r8, %rdx
	je	.LBB13_66
	jmp	.LBB13_1
.LBB13_64:                              # %lor.lhs.false173
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB13_1
# BB#65:                                # %lor.lhs.false173
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$140, %rcx
	je	.LBB13_1
.LBB13_66:                              # %switch.early.test
                                        #   in Loop: Header=BB13_1 Depth=1
	xorq	%rdx, %rdx
	cmpq	$133, %rax
	jg	.LBB13_70
# BB#67:                                # %switch.early.test
                                        #   in Loop: Header=BB13_1 Depth=1
	addq	$-55, %rax
	andq	%r12, %rax
	cmpq	$13, %rax
	ja	.LBB13_69
# BB#68:                                # %switch.early.test
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	$12299, %rsi            # imm = 0x300B
	movq	%r14, %rbx
	jne	.LBB13_1
	jmp	.LBB13_69
.LBB13_70:                              # %switch.early.test
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$134, %rax
	movq	%r14, %rbx
	je	.LBB13_1
# BB#71:                                # %switch.early.test
                                        #   in Loop: Header=BB13_1 Depth=1
	cmpq	$140, %rax
	movq	%r14, %rbx
	je	.LBB13_1
.LBB13_69:                              # %return.loopexit
	movq	%rdx, %rax
.LBB13_10:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_18:                              # %sw.bb1
	movl	8(%rbx), %eax
	cmpq	$52, %rax
	ja	.LBB13_21
# BB#19:                                # %sw.bb1
	movzbl	copying_arguments(%rip), %ecx
	testq	$1, %rcx
	je	.LBB13_21
# BB#20:                                # %if.then
	movq	new_reg_base_value(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	jmp	.LBB13_10
.LBB13_30:                              # %sw.bb18
	movzbl	copying_arguments(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB13_10
# BB#31:                                # %land.lhs.true20
	movq	8(%rbx), %rcx
	movq	global_rtl+40(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB13_34
# BB#32:                                # %lor.lhs.false25
	movzbl	(%rcx), %esi
	cmpq	$75, %rsi
	jne	.LBB13_10
# BB#33:                                # %land.lhs.true33
	movq	8(%rcx), %rcx
	cmpq	%rdx, %rcx
	jne	.LBB13_10
.LBB13_34:                              # %if.then42
	movq	$25, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_e  # TAILCALL
.LBB13_21:                              # %if.end
	cmpq	$52, %rax
	jbe	.LBB13_22
# BB#26:                                # %land.lhs.true7
	movl	reg_base_value_size(%rip), %ecx
	cmpq	%rcx, %rax
	jb	.LBB13_28
# BB#27:
	movq	%rbx, %rax
	jmp	.LBB13_10
.LBB13_49:
	movq	%rbx, %rdx
	movq	%rdx, %rax
	jmp	.LBB13_10
.LBB13_55:
	movq	%r14, %rdx
	movq	%rdx, %rax
	jmp	.LBB13_10
.LBB13_22:                              # %lor.lhs.false
	movzbl	fixed_regs(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB13_23
# BB#24:                                # %lor.lhs.false
	movl	reg_base_value_size(%rip), %ecx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jae	.LBB13_25
.LBB13_28:                              # %land.lhs.true10
	movq	reg_base_value(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	jne	.LBB13_10
# BB#29:                                # %if.end17
	movq	%rbx, %rax
	jmp	.LBB13_10
.LBB13_52:
	movq	%rbx, %rdx
	movq	%rdx, %rax
	jmp	.LBB13_10
.LBB13_58:
	movq	%r14, %rdx
	movq	%rdx, %rax
	jmp	.LBB13_10
.LBB13_23:
	movq	%rbx, %rax
	jmp	.LBB13_10
.LBB13_25:
	movq	%rbx, %rax
	jmp	.LBB13_10
.Ltmp129:
	.size	find_base_value, .Ltmp129-find_base_value
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_35
	.quad	.LBB13_10
	.quad	.LBB13_10
	.quad	.LBB13_18
	.quad	.LBB13_10
	.quad	.LBB13_10
	.quad	.LBB13_10
	.quad	.LBB13_10
	.quad	.LBB13_30
	.quad	.LBB13_69
	.quad	.LBB13_69

	.text
	.globl	clear_reg_alias_info
	.align	16, 0x90
	.type	clear_reg_alias_info,@function
clear_reg_alias_info:                   # @clear_reg_alias_info
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rdi), %eax
	movl	reg_known_value_size(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB14_3
# BB#1:                                 # %entry
	cmpq	$53, %rax
	jb	.LBB14_3
# BB#2:                                 # %if.then
	movq	reg_known_value(%rip), %rcx
	movq	%rdi, (%rcx,%rax,8)
.LBB14_3:                               # %if.end
	retq
.Ltmp130:
	.size	clear_reg_alias_info, .Ltmp130-clear_reg_alias_info
	.cfi_endproc

	.globl	canon_rtx
	.align	16, 0x90
	.type	canon_rtx,@function
canon_rtx:                              # @canon_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp134:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp135:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp136:
	.cfi_def_cfa_offset 32
.Ltmp137:
	.cfi_offset %rbx, -24
.Ltmp138:
	.cfi_offset %r14, -16
	movl	reg_known_value_size(%rip), %edx
	movq	reg_known_value(%rip), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	.align	16, 0x90
.LBB15_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	(%rbx), %edx
	cmpq	$75, %rdx
	je	.LBB15_8
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$66, %rdx
	je	.LBB15_16
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$61, %rdx
	jne	.LBB15_7
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_1 Depth=1
	movl	8(%rbx), %edx
	cmpq	$53, %rdx
	jb	.LBB15_7
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	%rcx, %rdx
	jae	.LBB15_7
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	(%rax,%rdx,8), %rdi
	cmpq	%rbx, %rdi
	jne	.LBB15_1
	jmp	.LBB15_7
.LBB15_8:                               # %if.then20
	movq	8(%rbx), %rdi
	callq	canon_rtx
	movq	16(%rbx), %rdi
	movq	%rax, %r14
	callq	canon_rtx
	movq	8(%rbx), %rcx
	cmpq	%rcx, %r14
	jne	.LBB15_10
# BB#9:                                 # %lor.lhs.false
	movq	16(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB15_7
.LBB15_10:                              # %if.then36
	movzbl	(%r14), %ecx
	cmpq	$54, %rcx
	jne	.LBB15_13
# BB#11:                                # %if.then40
	movq	8(%r14), %rsi
	movq	%rax, %rdi
	jmp	.LBB15_12
.LBB15_7:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB15_16:                              # %if.then62
	movq	8(%rbx), %rdi
	callq	canon_rtx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	replace_equiv_address_nv  # TAILCALL
.LBB15_13:                              # %if.else44
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB15_15
# BB#14:                                # %if.then48
	movq	8(%rax), %rsi
	movq	%r14, %rdi
.LBB15_12:                              # %if.then40
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	plus_constant_wide  # TAILCALL
.LBB15_15:                              # %if.end53
	movzbl	2(%rbx), %esi
	movq	$75, %rdi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp139:
	.size	canon_rtx, .Ltmp139-canon_rtx
	.cfi_endproc

	.globl	get_addr
	.align	16, 0x90
	.type	get_addr,@function
get_addr:                               # @get_addr
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$60, %rax
	jne	.LBB16_1
# BB#2:                                 # %if.end
	movq	8(%rdi), %rax
	movq	16(%rax), %rsi
	testq	%rsi, %rsi
	je	.LBB16_3
# BB#6:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rsi, %rdx
.LBB16_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB16_10
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB16_7 Depth=1
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB16_12
# BB#9:                                 # %for.body
                                        #   in Loop: Header=BB16_7 Depth=1
	movq	$1, %r9
	shlq	%cl, %r9
	testq	$24599, %r9             # imm = 0x6017
	je	.LBB16_12
	jmp	.LBB16_19
	.align	16, 0x90
.LBB16_10:                              # %for.body
                                        #   in Loop: Header=BB16_7 Depth=1
	cmpq	$134, %rcx
	je	.LBB16_19
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB16_7 Depth=1
	cmpq	$140, %rcx
	je	.LBB16_19
.LBB16_12:                              # %for.inc
                                        #   in Loop: Header=BB16_7 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB16_7
# BB#4:                                 # %for.cond42.preheader
	testq	%rsi, %rsi
	je	.LBB16_5
# BB#13:
	movq	%rsi, %rcx
	.align	16, 0x90
.LBB16_14:                              # %for.body44
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rax
	movzbl	(%rax), %edx
	cmpq	$61, %rdx
	je	.LBB16_16
# BB#15:                                # %for.body44
                                        #   in Loop: Header=BB16_14 Depth=1
	cmpq	$66, %rdx
	jne	.LBB16_19
.LBB16_16:                              # %for.inc56
                                        #   in Loop: Header=BB16_14 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB16_14
# BB#17:                                # %for.end58
	testq	%rsi, %rsi
	movq	%rdi, %rax
	je	.LBB16_19
# BB#18:                                # %if.then61
	movq	8(%rsi), %rax
.LBB16_19:                              # %return
	retq
.LBB16_1:
	movq	%rdi, %rax
	retq
.LBB16_3:
	movq	%rdi, %rax
	retq
.LBB16_5:
	movq	%rdi, %rax
	retq
.Ltmp140:
	.size	get_addr, .Ltmp140-get_addr
	.cfi_endproc

	.globl	addr_side_effect_eval
	.align	16, 0x90
	.type	addr_side_effect_eval,@function
addr_side_effect_eval:                  # @addr_side_effect_eval
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp144:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp145:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp146:
	.cfi_def_cfa_offset 32
.Ltmp147:
	.cfi_offset %rbx, -24
.Ltmp148:
	.cfi_offset %r14, -16
	movl	(%rdi), %ebx
	movq	%rbx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-96, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	ja	.LBB17_7
# BB#1:                                 # %entry
	jmpq	*.LJTI17_0(,%rcx,8)
.LBB17_3:                               # %sw.bb1
	notq	%rdx
	jmp	.LBB17_5
.LBB17_2:                               # %sw.bb
	incq	%rdx
	jmp	.LBB17_5
.LBB17_4:                               # %sw.bb6
	negq	%rdx
.LBB17_5:                               # %sw.epilog
	imulq	%rsi, %rdx
	testq	%rax, %rdx
	je	.LBB17_6
# BB#8:                                 # %if.then
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	8(%rdi), %r14
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rdi, %rdi
	movq	%rdx, %rsi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB17_6:                               # %if.else
	movq	8(%rdi), %rdi
.LBB17_7:                               # %return
	movq	%rdi, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp149:
	.size	addr_side_effect_eval, .Ltmp149-addr_side_effect_eval
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_3
	.quad	.LBB17_2
	.quad	.LBB17_4
	.quad	.LBB17_5

	.text
	.globl	read_dependence
	.align	16, 0x90
	.type	read_dependence,@function
read_dependence:                        # @read_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	3(%rsi), %ecx
	xorq	%rax, %rax
	testq	$8, %rcx
	je	.LBB18_2
# BB#1:                                 # %land.rhs
	movzbl	3(%rdi), %eax
	andq	$8, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
.LBB18_2:                               # %land.end
	retq
.Ltmp150:
	.size	read_dependence, .Ltmp150-read_dependence
	.cfi_endproc

	.globl	true_dependence
	.align	16, 0x90
	.type	true_dependence,@function
true_dependence:                        # @true_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp157:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp158:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp159:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp160:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp161:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp162:
	.cfi_def_cfa_offset 80
.Ltmp163:
	.cfi_offset %rbx, -48
.Ltmp164:
	.cfi_offset %r12, -40
.Ltmp165:
	.cfi_offset %r13, -32
.Ltmp166:
	.cfi_offset %r14, -24
.Ltmp167:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movl	(%r15), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r13
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB19_2
# BB#1:                                 # %land.lhs.true
	movzbl	3(%r13), %edx
	movq	$1, %r12
	testq	$8, %rdx
	jne	.LBB19_70
.LBB19_2:                               # %if.end
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB19_4
# BB#3:                                 # %land.lhs.true8
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %r12
	je	.LBB19_70
.LBB19_4:                               # %if.end13
	movl	(%r13), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB19_6
# BB#5:                                 # %land.lhs.true18
	movq	8(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %r12
	je	.LBB19_70
.LBB19_6:                               # %if.end26
	movq	16(%r15), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$0, %rdi
	je	.LBB19_8
# BB#7:                                 # %cond.false.i
	movq	(%rax), %rdi
.LBB19_8:                               # %cond.end.i
	movq	%rbx, %r14
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	16(%r13), %rax
	testq	%rax, %rax
	je	.LBB19_10
# BB#9:                                 # %cond.false9.i
	movq	(%rax), %rsi
.LBB19_10:                              # %mems_in_disjoint_alias_sets_p.exit
	callq	alias_sets_conflict_p
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB19_70
# BB#11:                                # %if.end29
	movzbl	3(%r15), %eax
	testq	$4, %rax
	je	.LBB19_13
# BB#12:                                # %land.lhs.true34
	movzbl	3(%r13), %eax
	testq	$4, %rax
	je	.LBB19_70
.LBB19_13:                              # %if.end40
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	nonoverlapping_memrefs_p
	testq	%rbx, %rax
	jne	.LBB19_70
# BB#14:                                # %if.end44
	testq	%rbx, %r14
	movq	%r14, %rbx
	jne	.LBB19_16
# BB#15:                                # %if.then46
	movzbl	2(%r13), %ebx
.LBB19_16:                              # %if.end50
	movq	8(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$60, %rax
	jne	.LBB19_30
# BB#17:                                # %if.end.i
	movq	8(%rdi), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB19_30
# BB#18:
	movq	%rax, %rdx
.LBB19_19:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jg	.LBB19_22
# BB#20:                                # %for.body.i
                                        #   in Loop: Header=BB19_19 Depth=1
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_24
# BB#21:                                # %for.body.i
                                        #   in Loop: Header=BB19_19 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	je	.LBB19_24
	jmp	.LBB19_30
.LBB19_22:                              # %for.body.i
                                        #   in Loop: Header=BB19_19 Depth=1
	cmpq	$134, %rcx
	je	.LBB19_30
# BB#23:                                # %for.body.i
                                        #   in Loop: Header=BB19_19 Depth=1
	cmpq	$140, %rcx
	je	.LBB19_30
.LBB19_24:                              # %for.inc.i
                                        #   in Loop: Header=BB19_19 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB19_19
# BB#25:
	movq	%rax, %rcx
.LBB19_26:                              # %for.body44.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rdi
	movzbl	(%rdi), %edx
	cmpq	$61, %rdx
	je	.LBB19_28
# BB#27:                                # %for.body44.i
                                        #   in Loop: Header=BB19_26 Depth=1
	cmpq	$66, %rdx
	jne	.LBB19_30
.LBB19_28:                              # %for.inc56.i
                                        #   in Loop: Header=BB19_26 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_26
# BB#29:                                # %if.then61.i
	movq	8(%rax), %rdi
.LBB19_30:                              # %get_addr.exit
	movq	8(%r13), %r14
	movzbl	(%r14), %eax
	cmpq	$60, %rax
	jne	.LBB19_31
# BB#32:                                # %if.end.i172
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB19_45
# BB#33:
	movq	%rax, %rdx
.LBB19_34:                              # %for.body.i178
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %r14
	movzbl	(%r14), %ecx
	cmpq	$133, %rcx
	jg	.LBB19_37
# BB#35:                                # %for.body.i178
                                        #   in Loop: Header=BB19_34 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_39
# BB#36:                                # %for.body.i178
                                        #   in Loop: Header=BB19_34 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	je	.LBB19_39
	jmp	.LBB19_45
.LBB19_37:                              # %for.body.i178
                                        #   in Loop: Header=BB19_34 Depth=1
	cmpq	$134, %rcx
	je	.LBB19_45
# BB#38:                                # %for.body.i178
                                        #   in Loop: Header=BB19_34 Depth=1
	cmpq	$140, %rcx
	je	.LBB19_45
.LBB19_39:                              # %for.inc.i182
                                        #   in Loop: Header=BB19_34 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB19_34
# BB#40:
	movq	%rax, %rcx
.LBB19_41:                              # %for.body44.i187
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %r14
	movzbl	(%r14), %edx
	cmpq	$61, %rdx
	je	.LBB19_43
# BB#42:                                # %for.body44.i187
                                        #   in Loop: Header=BB19_41 Depth=1
	cmpq	$66, %rdx
	jne	.LBB19_45
.LBB19_43:                              # %for.inc56.i191
                                        #   in Loop: Header=BB19_41 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_41
# BB#44:                                # %if.then61.i194
	movq	8(%rax), %r14
	jmp	.LBB19_45
.LBB19_31:
	movq	%rbx, 16(%rsp)          # 8-byte Spill
.LBB19_45:                              # %get_addr.exit196
	movq	%rdi, %rbx
	callq	find_base_term
	testq	%rax, %rax
	je	.LBB19_49
# BB#46:                                # %land.lhs.true61
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$67, %rcx
	je	.LBB19_70
# BB#47:                                # %land.lhs.true61
	cmpq	$68, %rcx
	jne	.LBB19_49
# BB#48:                                # %land.lhs.true68
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB19_70
.LBB19_49:                              # %if.end74
	movzbl	2(%r15), %edx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	base_alias_check
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_70
# BB#50:                                # %if.end81
	movq	%rbx, %rdi
	callq	canon_rtx
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	canon_rtx
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movzbl	2(%r15), %esi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %edi
	movzbl	mode_size(%rsi), %edx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r8, %r8
	xorq	%r12, %r12
	movq	%rax, %rsi
	movq	%rbx, %rcx
	movq	%rbx, %r14
	callq	memrefs_conflict_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rdx          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB19_70
# BB#51:                                # %if.end94
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$83, %rax
	movq	$1, %r12
	je	.LBB19_70
# BB#52:                                # %if.end94
	andq	%rsi, %rcx
	cmpq	$2, %rcx
	je	.LBB19_70
# BB#53:                                # %lor.lhs.false101
	movq	%r14, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$83, %rax
	je	.LBB19_70
# BB#54:                                # %lor.lhs.false101
	cmpq	$51, %rcx
	movq	%rbx, %rdi
	je	.LBB19_70
# BB#55:                                # %lor.lhs.false110
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB19_70
# BB#56:                                # %if.end117
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movl	flag_strict_aliasing(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB19_66
# BB#57:                                # %if.end.i160
	movq	%rdx, %r12
	movslq	(%r13), %rcx
	testq	%rcx, %rcx
	jns	.LBB19_62
# BB#58:                                # %if.end.i160
	andq	$268435456, %rax        # imm = 0x10000000
	testq	%rbx, %rax
	je	.LBB19_62
# BB#59:                                # %land.lhs.true5.i
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	*%r12
	testq	%rbx, %rax
	jne	.LBB19_62
# BB#60:                                # %land.lhs.true7.i
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	*%r12
	testq	%rbx, %rax
	je	.LBB19_62
# BB#61:
	movq	%r13, %r15
	jmp	.LBB19_67
.LBB19_62:                              # %if.end11.i
	movzbl	3(%r13), %eax
	testq	$16, %rax
	je	.LBB19_66
# BB#63:                                # %land.lhs.true16.i
	movslq	(%r15), %rax
	testq	%rax, %rax
	jns	.LBB19_66
# BB#64:                                # %land.lhs.true20.i
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	*%r12
	testq	%rbx, %rax
	je	.LBB19_66
# BB#65:                                # %land.lhs.true23.i
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	*%r12
	testq	%rbx, %rax
	je	.LBB19_67
.LBB19_66:                              # %if.end27.i
	xorq	%r15, %r15
.LBB19_67:                              # %fixed_scalar_and_varying_struct_p.exit
	testq	%r15, %r15
	jne	.LBB19_68
# BB#69:                                # %fixed_scalar_and_varying_struct_p.exit
	movabsq	$1, %r12
	jmp	.LBB19_70
.LBB19_68:
	movabsq	$0, %r12
.LBB19_70:                              # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp168:
	.size	true_dependence, .Ltmp168-true_dependence
	.cfi_endproc

	.align	16, 0x90
	.type	nonoverlapping_memrefs_p,@function
nonoverlapping_memrefs_p:               # @nonoverlapping_memrefs_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp175:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp176:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp177:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp178:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp179:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp180:
	.cfi_def_cfa_offset 96
.Ltmp181:
	.cfi_offset %rbx, -48
.Ltmp182:
	.cfi_offset %r12, -40
.Ltmp183:
	.cfi_offset %r13, -32
.Ltmp184:
	.cfi_offset %r14, -24
.Ltmp185:
	.cfi_offset %r15, -16
	movq	%rdi, %r11
	movq	16(%r11), %r9
	movq	%rsi, %r15
	xorq	%rax, %rax
	testq	%r9, %r9
	movabsq	$0, %r14
	je	.LBB20_2
# BB#1:                                 # %cond.false
	movq	8(%r9), %r14
.LBB20_2:                               # %cond.end
	movq	16(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB20_165
# BB#3:                                 # %cond.end14
	testq	%r14, %r14
	je	.LBB20_165
# BB#4:                                 # %cond.end14
	movq	8(%rcx), %r13
	testq	%r13, %r13
	je	.LBB20_165
# BB#5:                                 # %if.end
	movzbl	16(%r14), %r8d
	cmpq	$39, %r8
	jne	.LBB20_15
# BB#6:                                 # %land.lhs.true
	movzbl	16(%r13), %eax
	cmpq	$39, %rax
	jne	.LBB20_15
# BB#7:
	movq	%r13, %r10
	movq	%r14, %rsi
	.align	16, 0x90
.LBB20_8:                               # %do.body1.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_9 Depth 2
	movq	40(%rsi), %rbx
	movq	80(%rbx), %rdx
	movq	%r10, %rdi
	.align	16, 0x90
.LBB20_9:                               # %do.body2.i
                                        #   Parent Loop BB20_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rdi), %rcx
	movq	80(%rcx), %rax
	cmpq	%rax, %rdx
	je	.LBB20_19
# BB#10:                                # %if.end.i
                                        #   in Loop: Header=BB20_9 Depth=2
	movq	32(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB20_12
# BB#11:                                # %land.rhs.i
                                        #   in Loop: Header=BB20_9 Depth=2
	movzbl	16(%rdi), %eax
	cmpq	$39, %rax
	je	.LBB20_9
.LBB20_12:                              # %do.end.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movq	32(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB20_15
# BB#13:                                # %land.rhs17.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movzbl	16(%rsi), %eax
	jmp	.LBB20_14
	.align	16, 0x90
.LBB20_19:                              # %found.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movzbl	16(%rdx), %eax
	cmpq	$20, %rax
	jne	.LBB20_21
# BB#20:                                # %found.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movq	$1, %rax
	cmpq	%rcx, %rbx
	jne	.LBB20_165
.LBB20_21:                              # %if.end32.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movq	32(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB20_15
# BB#22:                                # %if.end32.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movq	32(%rdi), %r10
	testq	%r10, %r10
	je	.LBB20_15
# BB#23:                                # %land.lhs.true43.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movzbl	16(%rsi), %eax
	cmpq	$39, %rax
	jne	.LBB20_15
# BB#24:                                # %land.rhs49.i
                                        #   in Loop: Header=BB20_8 Depth=1
	movzbl	16(%r10), %eax
.LBB20_14:                              # %land.rhs17.i
                                        #   in Loop: Header=BB20_8 Depth=1
	cmpq	$39, %rax
	je	.LBB20_8
.LBB20_15:                              # %if.end26
	xorq	%rdx, %rdx
	testq	%r9, %r9
	je	.LBB20_17
# BB#16:                                # %cond.false32
	movq	16(%r9), %rdx
.LBB20_17:                              # %cond.end36
	cmpq	$39, %r8
	jne	.LBB20_18
# BB#25:
	movq	%r14, %rbx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB20_26:                              # %do.body.i915
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB20_165
# BB#27:                                # %land.rhs.i920
                                        #   in Loop: Header=BB20_26 Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$39, %rcx
	je	.LBB20_26
# BB#28:                                # %land.lhs.true.i
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB20_165
# BB#29:                                # %if.end46
	testq	%rdx, %rdx
	je	.LBB20_30
# BB#31:                                # %if.end.i924
	movq	%r11, 32(%rsp)          # 8-byte Spill
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	8(%rdx), %r15
	.align	16, 0x90
.LBB20_32:                              # %do.body.i932
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r14), %r12
	movq	88(%r12), %rdi
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB20_33
# BB#34:                                # %if.end4.i942
                                        #   in Loop: Header=BB20_32 Depth=1
	movq	88(%r12), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	160(%r12), %rdi
	movq	%rax, %r12
	movq	$1, %rsi
	callq	tree_low_cst
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$3, %rcx
	movq	32(%r14), %r14
	sarq	%cl, %rdx
	addq	%r12, %r15
	addq	%rdx, %r15
	testq	%r14, %r14
	je	.LBB20_36
# BB#35:                                # %land.rhs.i947
                                        #   in Loop: Header=BB20_32 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$39, %rax
	je	.LBB20_32
.LBB20_36:                              # %do.end.i949
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdx
	jmp	.LBB20_37
.LBB20_18:
	movq	%r14, %rbx
.LBB20_38:                              # %if.end48
	movq	16(%r15), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB20_40
# BB#39:                                # %cond.false54
	movq	16(%rax), %rsi
.LBB20_40:                              # %cond.end59
	movzbl	16(%r13), %eax
	cmpq	$39, %rax
	jne	.LBB20_41
# BB#42:
	movq	%r13, %r12
	xorq	%rax, %rax
	.align	16, 0x90
.LBB20_43:                              # %do.body.i956
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r12), %r12
	testq	%r12, %r12
	je	.LBB20_165
# BB#44:                                # %land.rhs.i961
                                        #   in Loop: Header=BB20_43 Depth=1
	movzbl	16(%r12), %ecx
	cmpq	$39, %rcx
	je	.LBB20_43
# BB#45:                                # %land.lhs.true.i966
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB20_165
# BB#46:                                # %if.end71
	testq	%rsi, %rsi
	je	.LBB20_47
# BB#48:                                # %if.end.i900
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r11, 32(%rsp)          # 8-byte Spill
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	8(%rsi), %r15
.LBB20_49:                              # %do.body.i903
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r13), %r14
	movq	88(%r14), %rdi
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB20_50
# BB#51:                                # %if.end4.i
                                        #   in Loop: Header=BB20_49 Depth=1
	movq	88(%r14), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	160(%r14), %rdi
	movq	%rax, %r14
	movq	$1, %rsi
	callq	tree_low_cst
	movq	$63, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$61, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movq	$3, %rcx
	movq	32(%r13), %r13
	sarq	%cl, %rdx
	addq	%r14, %r15
	addq	%rdx, %r15
	testq	%r13, %r13
	je	.LBB20_53
# BB#52:                                # %land.rhs.i908
                                        #   in Loop: Header=BB20_49 Depth=1
	movzbl	16(%r13), %eax
	cmpq	$39, %rax
	je	.LBB20_49
.LBB20_53:                              # %do.end.i909
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rsi
	jmp	.LBB20_54
.LBB20_41:
	movq	%r13, %r12
.LBB20_55:                              # %if.end73
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$100, %rcx
	jne	.LBB20_165
# BB#56:                                # %lor.lhs.false81
	movzbl	16(%r12), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB20_165
# BB#57:                                # %if.end92
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	144(%rbx), %r14
	testq	%r14, %r14
	jne	.LBB20_59
# BB#58:                                # %cond.false97
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r11, %r14
	callq	make_decl_rtl
	movq	%r14, %r11
	movq	144(%rbx), %r14
.LBB20_59:                              # %cond.end100
	movq	144(%r12), %rbx
	testq	%rbx, %rbx
	jne	.LBB20_61
# BB#60:                                # %cond.false108
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%r11, %rbx
	callq	make_decl_rtl
	movq	%rbx, %r11
	movq	144(%r12), %rbx
.LBB20_61:                              # %cond.end111
	movq	%r11, %r13
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB20_63
# BB#62:                                # %lor.lhs.false117
	movzbl	(%rbx), %ecx
	cmpq	$66, %rcx
	je	.LBB20_65
.LBB20_63:                              # %land.lhs.true122
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	je	.LBB20_165
# BB#64:                                # %land.lhs.true122.if.end126_crit_edge
	movl	(%r14), %eax
.LBB20_65:                              # %if.end126
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	movq	%r14, %rdi
	jne	.LBB20_67
# BB#66:                                # %cond.true131
	movq	8(%r14), %rdi
	movl	(%rdi), %eax
.LBB20_67:                              # %cond.end135
	movq	%r14, 8(%rsp)           # 8-byte Spill
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rdx, %rdx
	cmpq	$75, %rax
	jne	.LBB20_68
# BB#69:                                # %land.lhs.true141
	movq	16(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB20_70
# BB#71:                                # %if.then149
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	8(%rax), %rdx
	movq	8(%rdi), %rdi
	jmp	.LBB20_72
.LBB20_30:
	xorq	%rdx, %rdx
	jmp	.LBB20_38
.LBB20_68:
	movq	%r15, 40(%rsp)          # 8-byte Spill
	jmp	.LBB20_72
.LBB20_47:
	xorq	%rsi, %rsi
	jmp	.LBB20_55
.LBB20_33:
	xorq	%rdx, %rdx
.LBB20_37:                              # %if.end48
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%rsp), %r11          # 8-byte Reload
	jmp	.LBB20_38
.LBB20_70:
	movq	%r15, 40(%rsp)          # 8-byte Spill
.LBB20_72:                              # %if.end158
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	movq	%rbx, %r12
	jne	.LBB20_74
# BB#73:                                # %cond.true163
	movq	8(%rbx), %r12
	movl	(%r12), %eax
.LBB20_74:                              # %cond.end168
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$75, %rax
	jne	.LBB20_75
# BB#76:                                # %land.lhs.true174
	movq	16(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB20_75
# BB#77:                                # %if.then182
	movq	%rdx, %r15
	movq	8(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	8(%r12), %r12
	jmp	.LBB20_78
.LBB20_75:
	movq	$0, (%rsp)              # 8-byte Folded Spill
	movq	%rdx, %r15
.LBB20_78:                              # %if.end192
	movq	%r12, %rsi
	movq	%rdi, %r14
	callq	rtx_equal_p
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	je	.LBB20_79
# BB#108:                               # %if.end436
	movq	8(%rsp), %rdi           # 8-byte Reload
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	movq	%r13, %rdx
	movq	24(%rsp), %rsi          # 8-byte Reload
	jne	.LBB20_109
# BB#110:                               # %cond.false448
	movq	16(%rdi), %rcx
	testq	%rcx, %rcx
	movq	%r15, %r14
	je	.LBB20_112
# BB#111:                               # %cond.true454
	cmpq	$0, 24(%rcx)
	movq	$-1, %r12
	jne	.LBB20_116
	jmp	.LBB20_119
.LBB20_79:                              # %if.then195
	movzbl	(%r14), %edi
	cmpq	$133, %rdi
	movq	%r14, %r8
	jg	.LBB20_82
# BB#80:                                # %if.then195
	leaq	-54(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB20_98
# BB#81:                                # %if.then195
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB20_84
	jmp	.LBB20_98
.LBB20_109:                             # %cond.true441
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %r12d
	movq	%r15, %r14
	jmp	.LBB20_119
.LBB20_50:
	xorq	%rsi, %rsi
.LBB20_54:                              # %if.end73
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%rsp), %r11          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB20_55
.LBB20_82:                              # %if.then195
	cmpq	$134, %rdi
	je	.LBB20_84
# BB#83:                                # %if.then195
	cmpq	$140, %rdi
	jne	.LBB20_98
.LBB20_84:                              # %land.lhs.true235
	movzbl	(%r12), %ecx
	cmpq	$133, %rcx
	movq	$1, %rax
	jg	.LBB20_87
# BB#85:                                # %land.lhs.true235
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB20_89
# BB#86:                                # %land.lhs.true235
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB20_165
	jmp	.LBB20_89
.LBB20_112:                             # %cond.false459
	movq	%rdi, %r15
	movq	%rsi, %r13
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$51, %rax
	movq	$-1, %r12
	jne	.LBB20_114
# BB#113:
	movq	%r13, %rsi
	jmp	.LBB20_119
.LBB20_87:                              # %land.lhs.true235
	cmpq	$134, %rcx
	je	.LBB20_165
# BB#88:                                # %land.lhs.true235
	cmpq	$140, %rcx
	je	.LBB20_165
.LBB20_89:                              # %lor.lhs.false275
	cmpq	$133, %rdi
	jg	.LBB20_96
# BB#90:                                # %lor.lhs.false275
	leaq	-54(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB20_98
# BB#91:                                # %lor.lhs.false275
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB20_92
	jmp	.LBB20_98
.LBB20_114:                             # %cond.true465
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	callq	gen_rtx_CONST_INT
	movq	32(%rsp), %rdx          # 8-byte Reload
	testq	%rax, %rax
	movq	%r13, %rsi
	je	.LBB20_119
# BB#115:                               # %cond.true475
	movq	16(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB20_117
.LBB20_116:                             # %cond.true481
	movq	24(%rcx), %rax
.LBB20_118:                             # %cond.end503
	movq	8(%rax), %r12
.LBB20_119:                             # %cond.end511
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	(%rsp), %r15            # 8-byte Reload
	jne	.LBB20_120
# BB#121:                               # %cond.false525
	movq	16(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB20_123
# BB#122:                               # %cond.true531
	cmpq	$0, 24(%rcx)
	movq	$-1, %r13
	jne	.LBB20_126
	jmp	.LBB20_130
.LBB20_120:                             # %cond.true517
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %r13d
	jmp	.LBB20_130
.LBB20_123:                             # %cond.false537
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$51, %rax
	movq	$-1, %r13
	je	.LBB20_129
# BB#124:                               # %cond.true543
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	testq	%rax, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	je	.LBB20_130
# BB#125:                               # %cond.true553
	movq	16(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB20_127
.LBB20_126:                             # %cond.true559
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	24(%rcx), %rax
.LBB20_128:                             # %cond.end581
	movq	8(%rax), %r13
.LBB20_129:                             # %cond.end589
	movq	32(%rsp), %rdx          # 8-byte Reload
.LBB20_130:                             # %cond.end589
	testq	%rsi, %rsi
	je	.LBB20_132
# BB#131:                               # %if.then592
	movq	8(%rsi), %rax
	addq	%rax, %r14
	subq	%rax, %r12
.LBB20_132:                             # %if.end599
	testq	%rdi, %rdi
	je	.LBB20_134
# BB#133:                               # %if.then601
	movq	8(%rdi), %rax
	addq	%rax, %r15
	subq	%rax, %r13
.LBB20_134:                             # %if.end610
	movq	16(%rdx), %rax
	testq	%rax, %rax
	je	.LBB20_137
# BB#135:                               # %cond.true616
	cmpq	$0, 24(%rax)
	je	.LBB20_145
# BB#136:                               # %cond.true616
	testq	%rsi, %rsi
	jne	.LBB20_142
	jmp	.LBB20_145
.LBB20_137:                             # %cond.false622
	movq	%rsi, %rbx
	movzbl	2(%rdx), %eax
	cmpq	$51, %rax
	je	.LBB20_145
# BB#138:                               # %cond.true628
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	testq	%rax, %rax
	je	.LBB20_139
# BB#140:                               # %cond.true628
	testq	%rbx, %rbx
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	je	.LBB20_145
# BB#141:                               # %if.then640
	movq	16(%rcx), %rax
	testq	%rax, %rax
	je	.LBB20_143
.LBB20_142:                             # %cond.true646
	movq	%rdi, %rbx
	movq	24(%rax), %rax
.LBB20_144:                             # %cond.end668
	movq	8(%rax), %r12
	movq	%rbx, %rdi
.LBB20_145:                             # %if.end673
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rax
	testq	%rax, %rax
	je	.LBB20_148
# BB#146:                               # %cond.true679
	cmpq	$0, 24(%rax)
	je	.LBB20_155
# BB#147:                               # %cond.true679
	testq	%rdi, %rdi
	jne	.LBB20_152
	jmp	.LBB20_155
.LBB20_148:                             # %cond.false685
	movq	%rdi, %rbx
	movzbl	2(%rcx), %eax
	cmpq	$51, %rax
	je	.LBB20_155
# BB#149:                               # %cond.true691
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	testq	%rax, %rax
	je	.LBB20_155
# BB#150:                               # %cond.true691
	testq	%rbx, %rbx
	je	.LBB20_155
# BB#151:                               # %if.then703
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB20_153
.LBB20_152:                             # %cond.true709
	movq	24(%rax), %rax
.LBB20_154:                             # %cond.end731
	movq	8(%rax), %r13
.LBB20_155:                             # %if.end736
	cmpq	%r15, %r14
	jg	.LBB20_157
# BB#156:                               # %if.end736
	movq	%r12, %r13
.LBB20_157:                             # %if.end736
	xorq	%rax, %rax
	testq	%r13, %r13
	js	.LBB20_165
# BB#158:                               # %land.rhs743
	cmpq	%r15, %r14
	movq	%r14, %rax
	jge	.LBB20_160
# BB#159:                               # %land.rhs743
	movq	%r15, %rax
.LBB20_160:                             # %land.rhs743
	jg	.LBB20_162
# BB#161:                               # %land.rhs743
	movq	%r14, %r15
.LBB20_162:                             # %land.rhs743
	addq	%r15, %r13
	cmpq	%r13, %rax
	jle	.LBB20_163
# BB#164:                               # %land.rhs743
	movabsq	$1, %rax
	jmp	.LBB20_165
.LBB20_163:
	movabsq	$0, %rax
	jmp	.LBB20_165
.LBB20_96:                              # %lor.lhs.false275
	cmpq	$134, %rdi
	je	.LBB20_92
# BB#97:                                # %lor.lhs.false275
	cmpq	$140, %rdi
	jne	.LBB20_98
.LBB20_92:                              # %land.lhs.true315
	movl	(%r12), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB20_99
# BB#93:                                # %land.lhs.true320
	movl	8(%r12), %ecx
	cmpq	$20, %rcx
	ja	.LBB20_95
# BB#94:                                # %land.lhs.true320
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$1114304, %rbx          # imm = 0x1100C0
	jne	.LBB20_165
.LBB20_95:                              # %lor.lhs.false343
	addq	$-53, %rcx
	andq	%rsi, %rcx
	cmpq	$5, %rcx
	jae	.LBB20_99
	jmp	.LBB20_165
.LBB20_98:                              # %lor.lhs.false275.lor.rhs_crit_edge
	movl	(%r12), %edx
.LBB20_99:                              # %lor.rhs
	xorq	%rax, %rax
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$133, %rdx
	jg	.LBB20_102
# BB#100:                               # %lor.rhs
	addq	$-54, %rdx
	andq	%rsi, %rdx
	cmpq	$14, %rdx
	ja	.LBB20_165
# BB#101:                               # %lor.rhs
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	testq	$24599, %rbx            # imm = 0x6017
	jne	.LBB20_104
	jmp	.LBB20_165
.LBB20_102:                             # %lor.rhs
	cmpq	$134, %rdx
	je	.LBB20_104
# BB#103:                               # %lor.rhs
	cmpq	$140, %rdx
	jne	.LBB20_165
.LBB20_104:                             # %land.lhs.true394
	cmpq	$61, %rdi
	jne	.LBB20_165
# BB#105:                               # %land.rhs
	movl	8(%r8), %ecx
	cmpq	$20, %rcx
	ja	.LBB20_107
# BB#106:                               # %land.rhs
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$1114304, %rdx          # imm = 0x1100C0
	jne	.LBB20_165
.LBB20_107:                             # %lor.rhs422
	addq	$-53, %rcx
	andq	%rsi, %rcx
	cmpq	$5, %rcx
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB20_165:                             # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_139:
	movq	16(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB20_145
.LBB20_143:                             # %cond.false651
	movq	%rdi, %rbx
	movzbl	2(%rcx), %eax
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB20_144
.LBB20_153:                             # %cond.false714
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB20_154
.LBB20_117:                             # %cond.false486
	movzbl	2(%r15), %eax
	movq	%rsi, %r13
	movzbl	mode_size(%rax), %esi
	xorq	%rdi, %rdi
	movq	%rdx, %r12
	callq	gen_rtx_CONST_INT
	movq	%r13, %rsi
	movq	%r12, %rdx
	jmp	.LBB20_118
.LBB20_127:                             # %cond.false564
	movzbl	2(%rbx), %eax
	movq	%rsi, %r13
	movzbl	mode_size(%rax), %esi
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%r13, %rsi
	jmp	.LBB20_128
.Ltmp186:
	.size	nonoverlapping_memrefs_p, .Ltmp186-nonoverlapping_memrefs_p
	.cfi_endproc

	.align	16, 0x90
	.type	find_base_term,@function
find_base_term:                         # @find_base_term
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp193:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp194:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp195:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp196:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp197:
	.cfi_def_cfa_offset 48
.Ltmp198:
	.cfi_offset %rbx, -48
.Ltmp199:
	.cfi_offset %r12, -40
.Ltmp200:
	.cfi_offset %r13, -32
.Ltmp201:
	.cfi_offset %r14, -24
.Ltmp202:
	.cfi_offset %r15, -16
	movq	%rdi, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$2147549183, %r12       # imm = 0x8000FFFF
	movabsq	$2147483709, %r13       # imm = 0x8000003D
	jmp	.LBB21_1
	.align	16, 0x90
.LBB21_12:                              # %sw.bb20
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	8(%rax), %rax
.LBB21_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	callq	ix86_find_base_term
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB21_13
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$119, %rcx
	jg	.LBB21_9
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$95, %rcx
	jg	.LBB21_8
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$82, %rcx
	jg	.LBB21_43
# BB#5:                                 # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	addq	$-58, %rcx
	andq	%r15, %rcx
	cmpq	$18, %rcx
	ja	.LBB21_6
# BB#7:                                 # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	%rax, %rdx
	xorq	%rbx, %rbx
	jmpq	*.LJTI21_0(,%rcx,8)
.LBB21_24:                              # %sw.bb34
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	addq	$-75, %rcx
	andq	%r15, %rcx
	xorq	%rdx, %rdx
	cmpq	$1, %rcx
	jbe	.LBB21_25
	jmp	.LBB21_47
	.align	16, 0x90
.LBB21_13:                              # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB21_12
# BB#14:                                # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$135, %rcx
	jne	.LBB21_15
.LBB21_25:                              # %sw.bb48
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	8(%rax), %rdi
	movq	pic_offset_table_rtx(%rip), %rcx
	movq	16(%rax), %r14
	cmpq	%rcx, %rdi
	jne	.LBB21_31
# BB#26:                                # %land.lhs.true57
                                        #   in Loop: Header=BB21_1 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$133, %rcx
	jg	.LBB21_29
# BB#27:                                # %land.lhs.true57
                                        #   in Loop: Header=BB21_1 Depth=1
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB21_31
# BB#28:                                # %land.lhs.true57
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	movq	%r14, %rax
	je	.LBB21_31
	jmp	.LBB21_1
	.align	16, 0x90
.LBB21_9:                               # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	leaq	-120(%rcx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jb	.LBB21_12
# BB#10:                                # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$122, %rcx
	movabsq	$0, %rbx
	jne	.LBB21_48
# BB#11:                                # %sw.bb5
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	movl	target_flags(%rip), %esi
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %edi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movzbl	mode_size+4(%rsi), %ecx
	xorq	%rdx, %rdx
	cmpq	%rcx, %rdi
	jae	.LBB21_12
	jmp	.LBB21_47
.LBB21_8:                               # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	addq	$-96, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	movabsq	$0, %rbx
	jb	.LBB21_12
	jmp	.LBB21_48
.LBB21_29:                              # %land.lhs.true57
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$134, %rcx
	movq	%r14, %rax
	je	.LBB21_1
# BB#30:                                # %land.lhs.true57
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$140, %rcx
	movq	%r14, %rax
	je	.LBB21_1
	.align	16, 0x90
.LBB21_31:                              # %if.end98
                                        #   in Loop: Header=BB21_1 Depth=1
	movl	(%rdi), %eax
	andq	%r12, %rax
	cmpq	%r13, %rax
	movq	%rdi, %rax
	je	.LBB21_1
# BB#32:                                # %if.end109
                                        #   in Loop: Header=BB21_1 Depth=1
	movl	(%r14), %eax
	andq	%r12, %rax
	cmpq	%r13, %rax
	movq	%r14, %rax
	je	.LBB21_1
	jmp	.LBB21_33
.LBB21_43:                              # %tailrecurse
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$83, %rcx
	movabsq	$0, %rbx
	jne	.LBB21_48
# BB#44:                                # %sw.bb173
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %esi
	xorq	%rdx, %rdx
	cmpq	$54, %rsi
	jne	.LBB21_47
# BB#45:                                # %land.lhs.true181
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$0, 8(%rcx)
	je	.LBB21_47
# BB#46:                                # %if.then189
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	8(%rax), %rax
	jmp	.LBB21_1
.LBB21_15:
	xorq	%rbx, %rbx
	jmp	.LBB21_48
.LBB21_47:                              # %return.loopexit1
	movq	%rdx, %rbx
	jmp	.LBB21_48
.LBB21_33:                              # %if.end120
	callq	find_base_term
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	find_base_term
	testq	%rbx, %rbx
	je	.LBB21_37
# BB#34:                                # %land.lhs.true125
	movl	(%rbx), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-67(%rdx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jb	.LBB21_48
# BB#35:                                # %land.lhs.true125
	cmpq	$25, %rdx
	jne	.LBB21_37
# BB#36:                                # %land.lhs.true140
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB21_48
.LBB21_37:                              # %if.end147
	testq	%rax, %rax
	je	.LBB21_42
# BB#38:                                # %land.lhs.true150
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-67(%rdx), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	jae	.LBB21_40
# BB#39:
	movq	%rax, %rbx
	jmp	.LBB21_48
.LBB21_6:
	xorq	%rbx, %rbx
	jmp	.LBB21_48
.LBB21_19:                              # %sw.bb25
	movq	8(%rax), %rax
	movq	16(%rax), %r14
	testq	%r14, %r14
	je	.LBB21_20
	.align	16, 0x90
.LBB21_21:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rdi
	callq	find_base_term
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB21_48
# BB#22:                                # %for.cond
                                        #   in Loop: Header=BB21_21 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB21_21
# BB#23:
	xorq	%rbx, %rbx
	jmp	.LBB21_48
.LBB21_16:                              # %sw.bb196
	movq	global_rtl+24(%rip), %rax
.LBB21_17:                              # %sw.bb
	movl	8(%rax), %eax
	movl	reg_base_value_size(%rip), %ecx
	cmpq	%rcx, %rax
	movabsq	$0, %rbx
	jae	.LBB21_48
# BB#18:                                # %cond.true
	movq	reg_base_value(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
	jmp	.LBB21_48
.LBB21_40:                              # %land.lhs.true150
	cmpq	$25, %rdx
	jne	.LBB21_42
# BB#41:                                # %land.lhs.true165
	testq	$16711680, %rcx         # imm = 0xFF0000
	movq	%rax, %rbx
	jne	.LBB21_48
.LBB21_42:                              # %if.end172
	xorq	%rbx, %rbx
	jmp	.LBB21_48
.LBB21_20:
	xorq	%rbx, %rbx
.LBB21_48:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp203:
	.size	find_base_term, .Ltmp203-find_base_term
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI21_0:
	.quad	.LBB21_24
	.quad	.LBB21_48
	.quad	.LBB21_19
	.quad	.LBB21_17
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_47
	.quad	.LBB21_47
	.quad	.LBB21_48
	.quad	.LBB21_16
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_48
	.quad	.LBB21_25
	.quad	.LBB21_25

	.text
	.align	16, 0x90
	.type	base_alias_check,@function
base_alias_check:                       # @base_alias_check
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp210:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp211:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp212:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp213:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp214:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp215:
	.cfi_def_cfa_offset 64
.Ltmp216:
	.cfi_offset %rbx, -48
.Ltmp217:
	.cfi_offset %r12, -40
.Ltmp218:
	.cfi_offset %r13, -32
.Ltmp219:
	.cfi_offset %r14, -24
.Ltmp220:
	.cfi_offset %r15, -16
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r13
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	find_base_term
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	find_base_term
	movq	%rax, %r14
	testq	%r12, %r12
	jne	.LBB22_4
# BB#1:                                 # %if.then
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	movq	$1, %r15
	je	.LBB22_28
# BB#2:                                 # %lor.lhs.false
	movq	%rbx, %rdi
	callq	canon_rtx
	cmpq	%rbx, %rax
	je	.LBB22_28
# BB#3:                                 # %if.end
	movq	%rax, %rdi
	callq	find_base_term
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB22_28
.LBB22_4:                               # %if.end9
	testq	%r14, %r14
	jne	.LBB22_8
# BB#5:                                 # %if.then11
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	movq	$1, %r15
	je	.LBB22_28
# BB#6:                                 # %lor.lhs.false13
	movq	%r13, %rdi
	callq	canon_rtx
	cmpq	%r13, %rax
	je	.LBB22_28
# BB#7:                                 # %if.end17
	movq	%rax, %rdi
	callq	find_base_term
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB22_28
.LBB22_8:                               # %if.end22
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r15
	jne	.LBB22_28
# BB#9:                                 # %if.end26
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$25, %rcx
	jne	.LBB22_10
# BB#19:                                # %land.lhs.true93
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movl	target_flags(%rip), %esi
	andq	$255, %rdx
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	xorq	%r15, %r15
	cmpq	%rsi, %rdx
	je	.LBB22_28
	jmp	.LBB22_20
.LBB22_10:                              # %land.lhs.true
	movzbl	(%r14), %ecx
	cmpq	$25, %rcx
	jne	.LBB22_11
.LBB22_20:                              # %lor.lhs.false99
	movl	(%r14), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$25, %rcx
	jne	.LBB22_22
# BB#21:                                # %land.lhs.true104
	movq	$16, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movl	target_flags(%rip), %edi
	andq	$255, %rsi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%r15, %r15
	cmpq	%rdi, %rsi
	je	.LBB22_28
.LBB22_22:                              # %if.end114
	movl	flag_argument_noalias(%rip), %esi
	testq	%rsi, %rsi
	movq	$1, %r15
	je	.LBB22_28
# BB#23:                                # %if.end117
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%r15, %r15
	cmpq	$1, %rsi
	jg	.LBB22_28
# BB#24:                                # %if.end121
	testq	$16711680, %rax         # imm = 0xFF0000
	movq	$1, %r15
	jne	.LBB22_28
# BB#25:                                # %land.rhs
	testq	$16711680, %rdx         # imm = 0xFF0000
	je	.LBB22_26
# BB#27:                                # %land.rhs
	movabsq	$1, %r15
	jmp	.LBB22_28
.LBB22_11:                              # %if.then31
	movzbl	(%rbx), %eax
	cmpq	$83, %rax
	jne	.LBB22_15
# BB#12:                                # %land.lhs.true35
	movzbl	(%r13), %eax
	cmpq	$83, %rax
	je	.LBB22_28
# BB#13:                                # %land.lhs.true44
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB22_28
# BB#14:                                # %lor.lhs.false48
	movq	$32, %rcx
	movq	(%rsp), %rdx            # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax), %rax
	movzbl	mode_unit_size(%rdx), %ecx
	negq	%rax
	cmpq	%rax, %rcx
	jl	.LBB22_28
.LBB22_15:                              # %if.end59
	movzbl	(%r13), %eax
	cmpq	$83, %rax
	jne	.LBB22_18
# BB#16:                                # %land.lhs.true64
	movq	16(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB22_28
# BB#17:                                # %lor.lhs.false72
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax), %rax
	movzbl	mode_unit_size(%rdx), %ecx
	negq	%rax
	cmpq	%rax, %rcx
	jl	.LBB22_28
.LBB22_18:                              # %if.end87
	xorq	%r15, %r15
	jmp	.LBB22_28
.LBB22_26:
	movabsq	$0, %r15
.LBB22_28:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp221:
	.size	base_alias_check, .Ltmp221-base_alias_check
	.cfi_endproc

	.align	16, 0x90
	.type	memrefs_conflict_p,@function
memrefs_conflict_p:                     # @memrefs_conflict_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp228:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp229:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp230:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp231:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp233:
	.cfi_def_cfa_offset 96
.Ltmp234:
	.cfi_offset %rbx, -48
.Ltmp235:
	.cfi_offset %r12, -40
.Ltmp236:
	.cfi_offset %r13, -32
.Ltmp237:
	.cfi_offset %r14, -24
.Ltmp238:
	.cfi_offset %r15, -16
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_77:                              # %if.then162
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rax), %rax
	movq	8(%r14), %r14
	addq	%rax, 24(%rsp)          # 8-byte Folded Spill
.LBB23_1:                               # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_4 Depth 2
                                        #     Child Loop BB23_11 Depth 2
                                        #     Child Loop BB23_17 Depth 2
                                        #     Child Loop BB23_24 Depth 2
	movzbl	(%r12), %eax
	cmpq	$60, %rax
	jne	.LBB23_2
# BB#3:                                 # %if.end.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r12), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rax, %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	je	.LBB23_15
.LBB23_4:                               # %for.body.i
                                        #   Parent Loop BB23_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rdx), %r12
	movzbl	(%r12), %ecx
	cmpq	$133, %rcx
	jg	.LBB23_7
# BB#5:                                 # %for.body.i
                                        #   in Loop: Header=BB23_4 Depth=2
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_9
# BB#6:                                 # %for.body.i
                                        #   in Loop: Header=BB23_4 Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	je	.LBB23_9
	jmp	.LBB23_15
	.align	16, 0x90
.LBB23_7:                               # %for.body.i
                                        #   in Loop: Header=BB23_4 Depth=2
	cmpq	$134, %rcx
	je	.LBB23_15
# BB#8:                                 # %for.body.i
                                        #   in Loop: Header=BB23_4 Depth=2
	cmpq	$140, %rcx
	je	.LBB23_15
.LBB23_9:                               # %for.inc.i
                                        #   in Loop: Header=BB23_4 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB23_4
# BB#10:                                #   in Loop: Header=BB23_1 Depth=1
	movq	%rax, %rcx
	.align	16, 0x90
.LBB23_11:                              # %for.body44.i
                                        #   Parent Loop BB23_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %r12
	movzbl	(%r12), %edx
	cmpq	$61, %rdx
	je	.LBB23_13
# BB#12:                                # %for.body44.i
                                        #   in Loop: Header=BB23_11 Depth=2
	cmpq	$66, %rdx
	jne	.LBB23_15
.LBB23_13:                              # %for.inc56.i
                                        #   in Loop: Header=BB23_11 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_11
# BB#14:                                # %if.then61.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rax), %r12
	jmp	.LBB23_15
	.align	16, 0x90
.LBB23_2:                               #   in Loop: Header=BB23_1 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB23_15:                              # %if.end
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	(%r14), %eax
	cmpq	$60, %rax
	jne	.LBB23_28
# BB#16:                                # %if.end.i796
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r14), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rax, %rdx
	je	.LBB23_28
.LBB23_17:                              # %for.body.i802
                                        #   Parent Loop BB23_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rdx), %r14
	movzbl	(%r14), %ecx
	cmpq	$133, %rcx
	jg	.LBB23_20
# BB#18:                                # %for.body.i802
                                        #   in Loop: Header=BB23_17 Depth=2
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_22
# BB#19:                                # %for.body.i802
                                        #   in Loop: Header=BB23_17 Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	je	.LBB23_22
	jmp	.LBB23_28
	.align	16, 0x90
.LBB23_20:                              # %for.body.i802
                                        #   in Loop: Header=BB23_17 Depth=2
	cmpq	$134, %rcx
	je	.LBB23_28
# BB#21:                                # %for.body.i802
                                        #   in Loop: Header=BB23_17 Depth=2
	cmpq	$140, %rcx
	je	.LBB23_28
.LBB23_22:                              # %for.inc.i806
                                        #   in Loop: Header=BB23_17 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB23_17
# BB#23:                                #   in Loop: Header=BB23_1 Depth=1
	movq	%rax, %rcx
	.align	16, 0x90
.LBB23_24:                              # %for.body44.i811
                                        #   Parent Loop BB23_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %r14
	movzbl	(%r14), %edx
	cmpq	$61, %rdx
	je	.LBB23_26
# BB#25:                                # %for.body44.i811
                                        #   in Loop: Header=BB23_24 Depth=2
	cmpq	$66, %rdx
	jne	.LBB23_28
.LBB23_26:                              # %for.inc56.i815
                                        #   in Loop: Header=BB23_24 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB23_24
# BB#27:                                # %if.then61.i818
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rax), %r14
	.align	16, 0x90
.LBB23_28:                              # %if.end6
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	(%r12), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB23_33
# BB#29:                                # %if.end6
                                        #   in Loop: Header=BB23_1 Depth=1
	xorq	%rsi, %rsi
	leaq	-98(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB23_38
# BB#30:                                # %if.end6
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$96, %rax
	jne	.LBB23_31
# BB#37:                                # %sw.bb1.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r13, %rsi
	jmp	.LBB23_38
	.align	16, 0x90
.LBB23_33:                              # %if.end6
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$135, %rax
	jne	.LBB23_34
# BB#36:                                # %if.then14
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	16(%r12), %r12
	jmp	.LBB23_42
	.align	16, 0x90
.LBB23_34:                              # %if.end6
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$134, %rax
	jne	.LBB23_41
# BB#35:                                # %if.then10
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r12), %r12
	jmp	.LBB23_42
.LBB23_31:                              # %if.end6
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$97, %rax
	jne	.LBB23_41
# BB#32:                                # %sw.bb.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %rsi
	.align	16, 0x90
.LBB23_38:                              # %sw.epilog.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	imulq	%rax, %rsi
	testq	%r13, %rsi
	jne	.LBB23_39
# BB#40:                                # %if.else.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r12), %r12
	jmp	.LBB23_41
	.align	16, 0x90
.LBB23_39:                              # %if.then.i
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	8(%r12), %r15
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
.LBB23_41:                              # %addr_side_effect_eval.exit
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r12, %rdi
	callq	canon_rtx
	movq	%rax, %r12
.LBB23_42:                              # %if.end22
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	(%r14), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB23_47
# BB#43:                                # %if.end22
                                        #   in Loop: Header=BB23_1 Depth=1
	xorq	%rsi, %rsi
	leaq	-98(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB23_52
# BB#44:                                # %if.end22
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$96, %rax
	jne	.LBB23_45
# BB#51:                                # %sw.bb1.i829
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r13, %rsi
	jmp	.LBB23_52
	.align	16, 0x90
.LBB23_47:                              # %if.end22
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$135, %rax
	jne	.LBB23_48
# BB#50:                                # %if.then34
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	16(%r14), %r14
	jmp	.LBB23_56
	.align	16, 0x90
.LBB23_48:                              # %if.end22
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$134, %rax
	jne	.LBB23_55
# BB#49:                                # %if.then26
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r14), %r14
	jmp	.LBB23_56
.LBB23_45:                              # %if.end22
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$97, %rax
	jne	.LBB23_55
# BB#46:                                # %sw.bb.i828
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %rsi
	.align	16, 0x90
.LBB23_52:                              # %sw.epilog.i834
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	imulq	%rax, %rsi
	testq	%r13, %rsi
	jne	.LBB23_53
# BB#54:                                # %if.else.i845
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r14), %r14
	jmp	.LBB23_55
	.align	16, 0x90
.LBB23_53:                              # %if.then.i842
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	8(%r14), %r14
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
.LBB23_55:                              # %addr_side_effect_eval.exit847
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r14, %rdi
	callq	canon_rtx
	movq	%rax, %r14
.LBB23_56:                              # %if.end42
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	rtx_equal_for_memref_p
	testq	%r13, %rax
	jne	.LBB23_57
# BB#64:                                # %if.end62
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB23_75
# BB#65:                                # %if.then67
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	(%r14), %edx
	movq	8(%r12), %rdi
	movq	16(%r12), %r13
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$75, %rsi
	jne	.LBB23_73
# BB#66:                                # %if.then78
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	8(%r14), %r12
	movq	16(%r14), %r15
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rbx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	rtx_equal_for_memref_p
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rsi, %r12
	movq	%rdx, %r14
	jne	.LBB23_1
# BB#67:                                # %if.end89
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rsi, %r14
	movq	%r14, %rdi
	movq	%rdx, %rbx
	movq	%rbx, %rsi
	callq	rtx_equal_for_memref_p
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r13, %r12
	movq	%r15, %r14
	jne	.LBB23_1
# BB#68:                                # %if.end94
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	(%r13), %ecx
	movzbl	(%r15), %eax
	cmpq	$54, %rcx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %rsi
	jne	.LBB23_71
# BB#69:                                # %if.then99
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r13), %rcx
	subq	%rcx, 24(%rsp)          # 8-byte Folded Spill
	cmpq	$54, %rax
	movq	%rdi, %r12
	movq	8(%rsp), %r14           # 8-byte Reload
	jne	.LBB23_1
# BB#70:                                # %if.then104
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rsi), %rax
	addq	%rax, 24(%rsp)          # 8-byte Folded Spill
	movq	%rdi, %r12
	movq	%rdx, %r14
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_75:                              # %if.else144
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	(%r14), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$75, %rsi
	jne	.LBB23_78
# BB#76:                                # %if.then149
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movq	$1, %r15
	je	.LBB23_77
	jmp	.LBB23_173
	.align	16, 0x90
.LBB23_73:                              # %if.else131
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	(%r13), %esi
	cmpq	$54, %rsi
	jne	.LBB23_78
# BB#74:                                # %if.then136
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r13), %rax
	subq	%rax, 24(%rsp)          # 8-byte Folded Spill
	movq	%rdi, %r12
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_78:                              # %if.end170
                                        #   in Loop: Header=BB23_1 Depth=1
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	%rdx, %rcx
	jne	.LBB23_79
# BB#80:                                # %if.then177
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$61, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jne	.LBB23_81
# BB#94:                                # %sw.bb248
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	alias_invariant(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB23_106
# BB#95:                                # %if.then250
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	8(%r12), %r9d
	movl	reg_base_value_size(%rip), %edi
	movl	8(%r14), %r8d
	xorq	%rcx, %rcx
	cmpq	%rdi, %r9
	movabsq	$0, %rsi
	jae	.LBB23_97
# BB#96:                                # %cond.false
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	(%rdx,%r9,8), %rsi
.LBB23_97:                              # %cond.end
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	%rdi, %r8
	jae	.LBB23_99
# BB#98:                                # %cond.false262
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	(%rdx,%r8,8), %rcx
.LBB23_99:                              # %cond.end265
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rsi, %rdx
	orq	%rcx, %rdx
	je	.LBB23_106
# BB#100:                               # %if.end273
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r14, %r13
	movq	%rbx, %r14
	testq	%rsi, %rsi
	jne	.LBB23_102
# BB#101:                               # %if.end273
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r12, %rsi
.LBB23_102:                             # %if.end273
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rcx, %rcx
	jne	.LBB23_104
# BB#103:                               # %if.end273
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r13, %rcx
.LBB23_104:                             # %if.end273
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	memrefs_conflict_p
	xorq	%r15, %r15
	testq	%r14, %rax
	je	.LBB23_173
# BB#105:                               # %if.end273.if.end289_crit_edge
                                        #   in Loop: Header=BB23_1 Depth=1
	movl	(%r12), %eax
	movq	%r14, %rbx
	movq	%r13, %r14
	jmp	.LBB23_106
.LBB23_79:                              #   in Loop: Header=BB23_1 Depth=1
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB23_106
.LBB23_81:                              # %if.then177
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$78, %rcx
	jne	.LBB23_106
# BB#82:                                # %sw.bb
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	16(%r12), %rdi
	callq	canon_rtx
	movq	16(%r14), %rdi
	movq	%rax, %r13
	callq	canon_rtx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_for_memref_p
	testq	%rbx, %rax
	je	.LBB23_83
# BB#84:                                # %if.end195
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r12), %rdi
	callq	canon_rtx
	movq	8(%r14), %rdi
	movq	%rax, %r12
	callq	canon_rtx
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	rtx_equal_for_memref_p
	testq	%rbx, %rax
	jne	.LBB23_85
# BB#92:                                # %if.end225
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	movq	$1, %r15
	jne	.LBB23_173
# BB#93:                                # %if.end231
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r13), %rsi
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	$32, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %rax
	cqto
	idivq	%rsi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	cqto
	idivq	%rsi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_106:                             # %if.end289
                                        #   in Loop: Header=BB23_1 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$83, %rax
	jne	.LBB23_112
# BB#107:                               # %land.lhs.true294
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	16(%r12), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$54, %rcx
	jne	.LBB23_112
# BB#108:                               # %if.then302
                                        #   in Loop: Header=BB23_1 Depth=1
	movzbl	(%r14), %eax
	cmpq	$83, %rax
	je	.LBB23_110
# BB#109:                               # %lor.lhs.false307
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	8(%rdx), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	negq	%rdx
	cmpq	%rdx, %rax
	jge	.LBB23_111
.LBB23_110:                             # %if.then318
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
.LBB23_111:                             # %if.end319
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r12), %r12
	jmp	.LBB23_1
	.align	16, 0x90
.LBB23_112:                             # %if.end324
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%r14, %rsi
	movzbl	(%rsi), %r14d
	cmpq	$83, %r14
	jne	.LBB23_118
# BB#113:                               # %land.lhs.true329
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	16(%rsi), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$54, %rcx
	jne	.LBB23_118
# BB#114:                               # %if.then337
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$83, %rax
	je	.LBB23_116
# BB#115:                               # %lor.lhs.false342
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rdx), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	negq	%rdx
	cmpq	%rdx, %rax
	jge	.LBB23_117
.LBB23_116:                             # %if.then353
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rbx, 32(%rsp)          # 8-byte Spill
.LBB23_117:                             # %if.end354
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rsi), %r14
	jmp	.LBB23_1
.LBB23_118:                             # %if.end359
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	%rsi, %rbx
	movq	global_rtl+24(%rip), %rcx
	cmpq	$70, %rax
	jne	.LBB23_126
# BB#119:                               # %if.then364
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	%rcx, %rbx
	je	.LBB23_121
# BB#120:                               # %if.then364
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$70, %r14
	jne	.LBB23_133
	jmp	.LBB23_121
.LBB23_126:                             # %if.end381
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$70, %r14
	jne	.LBB23_133
# BB#127:                               # %if.end381
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	%rcx, %r12
	je	.LBB23_128
.LBB23_133:                             # %if.end398
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %r15
	cmpq	$133, %rax
	jg	.LBB23_142
# BB#134:                               # %if.end398
                                        #   in Loop: Header=BB23_1 Depth=1
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_173
# BB#135:                               # %if.end398
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24598, %rdx            # imm = 0x6016
	jne	.LBB23_144
# BB#136:                               # %if.end398
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%rcx, %rcx
	jne	.LBB23_173
# BB#137:                               # %land.lhs.true443
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$133, %r14
	jg	.LBB23_147
# BB#138:                               # %land.lhs.true443
                                        #   in Loop: Header=BB23_1 Depth=1
	addq	$-54, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	cmpq	$14, %r14
	ja	.LBB23_173
# BB#139:                               # %land.lhs.true443
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %rax
	movq	%r14, %rcx
	shlq	%cl, %rax
	testq	$24582, %rax            # imm = 0x6006
	jne	.LBB23_163
# BB#140:                               # %land.lhs.true443
                                        #   in Loop: Header=BB23_1 Depth=1
	testq	%r14, %r14
	je	.LBB23_148
# BB#141:                               # %land.lhs.true443
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$4, %r14
	je	.LBB23_146
	jmp	.LBB23_173
.LBB23_142:                             # %if.end398
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$134, %rax
	je	.LBB23_144
# BB#143:                               # %if.end398
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$140, %rax
	jne	.LBB23_173
.LBB23_144:                             # %if.end480
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$58, %rax
	jne	.LBB23_157
# BB#145:                               # %if.then485
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%r12), %rdi
	callq	canon_rtx
	movq	%rax, %r12
	cmpq	$58, %r14
	movq	%rbx, %r14
	jne	.LBB23_1
	jmp	.LBB23_146
.LBB23_157:                             # %if.end506
                                        #   in Loop: Header=BB23_1 Depth=1
	cmpq	$58, %r14
	jne	.LBB23_158
.LBB23_146:                             # %if.then490
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rbx), %rdi
	callq	canon_rtx
	movq	%rax, %r14
	jmp	.LBB23_1
.LBB23_71:                              # %if.else118
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	$1, %r15
	cmpq	$54, %rax
	jne	.LBB23_173
# BB#72:                                # %if.then123
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rsi), %rax
	addq	%rax, 24(%rsp)          # 8-byte Folded Spill
	movq	%rcx, %r12
	movq	%rdx, %r14
	jmp	.LBB23_1
.LBB23_57:                              # %if.then44
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %r15
	jle	.LBB23_173
# BB#58:                                # %if.then44
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB23_173
# BB#59:                                # %if.end48
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	js	.LBB23_61
# BB#60:                                # %if.end48
	cmpq	%rax, %rcx
	jg	.LBB23_173
.LBB23_61:                              # %if.end53
	testq	%rax, %rax
	movq	%rax, %rdx
	jns	.LBB23_63
# BB#62:                                # %land.lhs.true56
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %rdx
	testq	%rdx, %rdx
	jg	.LBB23_173
.LBB23_63:                              # %if.end61
	xorq	%r15, %r15
	jmp	.LBB23_173
.LBB23_147:                             # %land.lhs.true443
	cmpq	$134, %r14
	jne	.LBB23_162
	jmp	.LBB23_163
.LBB23_121:                             # %if.then372
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %r15
	jg	.LBB23_123
# BB#122:                               # %if.then372
	movq	%rdx, %r15
.LBB23_123:                             # %if.then372
	movq	$32, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB23_125
# BB#124:                               # %if.then372
	movq	%rdx, %rax
.LBB23_125:                             # %if.then372
	orq	%rax, %r15
	jmp	.LBB23_173
.LBB23_128:                             # %if.then389
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %r15
	jg	.LBB23_130
# BB#129:                               # %if.then389
	movq	%rdx, %r15
.LBB23_130:                             # %if.then389
	movq	$32, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB23_132
# BB#131:                               # %if.then389
	movq	%rdx, %rax
.LBB23_132:                             # %if.then389
	orq	%rax, %r15
	jmp	.LBB23_173
.LBB23_158:                             # %if.end517
	movq	%rbx, %rdx
	cmpq	$133, %r14
	jg	.LBB23_161
# BB#159:                               # %if.end517
	addq	$-54, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_173
# BB#160:                               # %if.end517
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24583, %rax            # imm = 0x6007
	movq	%rdx, %rbx
	jne	.LBB23_163
	jmp	.LBB23_173
.LBB23_83:
	movq	$1, %r15
	jmp	.LBB23_173
.LBB23_85:                              # %if.then206
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	$1, %r15
	je	.LBB23_173
# BB#86:                                # %if.then206
	movq	%rcx, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB23_173
# BB#87:                                # %lor.lhs.false212
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	js	.LBB23_89
# BB#88:                                # %lor.lhs.false212
	cmpq	%rdx, %rax
	jg	.LBB23_173
.LBB23_89:                              # %lor.rhs
	xorq	%r15, %r15
	testq	%rdx, %rdx
	jns	.LBB23_173
# BB#90:                                # %land.rhs
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %rdx
	testq	%rdx, %rdx
	jg	.LBB23_172
# BB#91:
	movabsq	$0, %r15
	jmp	.LBB23_173
.LBB23_161:                             # %if.end517
	cmpq	$134, %r14
	movq	%rdx, %rbx
	je	.LBB23_163
.LBB23_162:                             # %if.end517
	cmpq	$140, %r14
	jne	.LBB23_173
.LBB23_163:                             # %if.then557
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %r15
	jle	.LBB23_173
# BB#164:                               # %if.then557
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	jle	.LBB23_173
# BB#165:                               # %lor.rhs563
	movq	%r12, %rdi
	callq	rtx_equal_for_memref_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB23_166
# BB#167:                               # %land.rhs566
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	js	.LBB23_169
# BB#168:                               # %land.rhs566
	cmpq	%rdx, %rax
	jg	.LBB23_173
.LBB23_169:                             # %lor.rhs573
	testq	%rdx, %rdx
	movabsq	$0, %r15
	jns	.LBB23_173
# BB#170:                               # %land.rhs576
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %rdx
	testq	%rdx, %rdx
	jle	.LBB23_171
.LBB23_172:                             # %land.rhs576
	movabsq	$1, %r15
	jmp	.LBB23_173
.LBB23_166:
	xorq	%r15, %r15
.LBB23_173:                             # %return
	movq	%r15, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_171:
	movabsq	$0, %r15
	jmp	.LBB23_173
.LBB23_148:                             # %if.then448
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %r15
	jle	.LBB23_173
# BB#149:                               # %if.then448
	movq	$32, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB23_173
# BB#150:                               # %lor.lhs.false462
	movq	8(%rbx), %rax
	movq	8(%r12), %rcx
	subq	%rcx, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, %rcx
	testq	%rax, %rax
	js	.LBB23_152
# BB#151:                               # %lor.lhs.false462
	cmpq	%rax, %rcx
	jg	.LBB23_173
.LBB23_152:                             # %lor.rhs469
	xorq	%r15, %r15
	testq	%rax, %rax
	jns	.LBB23_173
# BB#153:                               # %land.rhs472
	movq	$32, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rdx, %rax
	testq	%rax, %rax
	jle	.LBB23_154
# BB#155:                               # %land.rhs472
	movabsq	$1, %rax
	movq	%rax, %r15
	jmp	.LBB23_173
.LBB23_154:
	movabsq	$0, %rax
	movq	%rax, %r15
	jmp	.LBB23_173
.Ltmp239:
	.size	memrefs_conflict_p, .Ltmp239-memrefs_conflict_p
	.cfi_endproc

	.globl	canon_true_dependence
	.align	16, 0x90
	.type	canon_true_dependence,@function
canon_true_dependence:                  # @canon_true_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp246:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp247:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp248:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp249:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp250:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp251:
	.cfi_def_cfa_offset 80
.Ltmp252:
	.cfi_offset %rbx, -48
.Ltmp253:
	.cfi_offset %r12, -40
.Ltmp254:
	.cfi_offset %r13, -32
.Ltmp255:
	.cfi_offset %r14, -24
.Ltmp256:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movl	(%r12), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r15
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB24_2
# BB#1:                                 # %land.lhs.true
	movzbl	3(%r15), %ecx
	movq	$1, %r14
	testq	$8, %rcx
	jne	.LBB24_50
.LBB24_2:                               # %if.end
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB24_4
# BB#3:                                 # %land.lhs.true8
	movq	8(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %r14
	je	.LBB24_50
.LBB24_4:                               # %if.end13
	movl	(%r15), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB24_6
# BB#5:                                 # %land.lhs.true18
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %r14
	je	.LBB24_50
.LBB24_6:                               # %if.end26
	movq	16(%r12), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$0, %rdi
	je	.LBB24_8
# BB#7:                                 # %cond.false.i
	movq	(%rax), %rdi
.LBB24_8:                               # %cond.end.i
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	16(%r15), %rax
	testq	%rax, %rax
	je	.LBB24_10
# BB#9:                                 # %cond.false9.i
	movq	(%rax), %rsi
.LBB24_10:                              # %mems_in_disjoint_alias_sets_p.exit
	callq	alias_sets_conflict_p
	xorq	%r14, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB24_50
# BB#11:                                # %if.end29
	movzbl	3(%r12), %eax
	testq	$4, %rax
	je	.LBB24_13
# BB#12:                                # %land.lhs.true34
	movzbl	3(%r15), %eax
	testq	$4, %rax
	je	.LBB24_50
.LBB24_13:                              # %if.end40
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	nonoverlapping_memrefs_p
	testq	%r13, %rax
	jne	.LBB24_50
# BB#14:                                # %if.end44
	movq	8(%r12), %rbx
	movzbl	(%rbx), %eax
	cmpq	$60, %rax
	jne	.LBB24_15
# BB#16:                                # %if.end.i
	movq	8(%rbx), %rax
	movq	16(%rax), %r8
	testq	%r8, %r8
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rax          # 8-byte Reload
	je	.LBB24_29
# BB#17:
	movq	%r8, %rdx
.LBB24_18:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$133, %rcx
	jg	.LBB24_21
# BB#19:                                # %for.body.i
                                        #   in Loop: Header=BB24_18 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB24_23
# BB#20:                                # %for.body.i
                                        #   in Loop: Header=BB24_18 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	je	.LBB24_23
	jmp	.LBB24_29
.LBB24_21:                              # %for.body.i
                                        #   in Loop: Header=BB24_18 Depth=1
	cmpq	$134, %rcx
	je	.LBB24_29
# BB#22:                                # %for.body.i
                                        #   in Loop: Header=BB24_18 Depth=1
	cmpq	$140, %rcx
	je	.LBB24_29
.LBB24_23:                              # %for.inc.i
                                        #   in Loop: Header=BB24_18 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB24_18
# BB#24:
	movq	%r8, %rcx
.LBB24_25:                              # %for.body44.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rbx
	movzbl	(%rbx), %edx
	cmpq	$61, %rdx
	je	.LBB24_27
# BB#26:                                # %for.body44.i
                                        #   in Loop: Header=BB24_25 Depth=1
	cmpq	$66, %rdx
	jne	.LBB24_29
.LBB24_27:                              # %for.inc56.i
                                        #   in Loop: Header=BB24_25 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB24_25
# BB#28:                                # %if.then61.i
	movq	8(%r8), %rbx
	jmp	.LBB24_29
.LBB24_15:
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rax          # 8-byte Reload
.LBB24_29:                              # %get_addr.exit
	movzbl	2(%r12), %edx
	movq	%rbx, %rdi
	movq	%rax, %rcx
	movq	%rsi, %r13
	callq	base_alias_check
	movq	%r13, %rcx
	movq	24(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB24_50
# BB#30:                                # %if.end55
	movq	%rbx, %rdi
	movq	%rcx, %rbx
	callq	canon_rtx
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movzbl	2(%r12), %esi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %edi
	movzbl	mode_size(%rsi), %edx
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	%rbx, %rsi
	movq	%rax, %rcx
	callq	memrefs_conflict_p
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rbx, %rdx
	movq	8(%rsp), %rbx           # 8-byte Reload
	testq	%rsi, %rax
	je	.LBB24_50
# BB#31:                                # %if.end67
	movq	8(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$83, %rax
	movq	$1, %r14
	je	.LBB24_50
# BB#32:                                # %if.end67
	andq	%rsi, %r13
	cmpq	$2, %r13
	je	.LBB24_50
# BB#33:                                # %lor.lhs.false
	movzbl	(%rdx), %eax
	cmpq	$83, %rax
	je	.LBB24_50
# BB#34:                                # %lor.lhs.false
	cmpq	$51, %r13
	je	.LBB24_50
# BB#35:                                # %lor.lhs.false82
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB24_50
# BB#36:                                # %if.end89
	movl	flag_strict_aliasing(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB24_46
# BB#37:                                # %if.end.i124
	movslq	(%r15), %rcx
	testq	%rcx, %rcx
	jns	.LBB24_42
# BB#38:                                # %if.end.i124
	andq	$268435456, %rax        # imm = 0x10000000
	testq	%rsi, %rax
	je	.LBB24_42
# BB#39:                                # %land.lhs.true5.i
	movq	%rsi, %r13
	movq	$1, %rsi
	movq	%rdx, %rdi
	movq	%rbx, %r14
	movq	%rdx, %rbx
	callq	*%r14
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rbx
	testq	%rsi, %rax
	jne	.LBB24_42
# BB#40:                                # %land.lhs.true7.i
	movq	%rsi, %r13
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %r14
	movq	%rdx, %rbx
	callq	*%r14
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rbx
	testq	%rsi, %rax
	je	.LBB24_42
# BB#41:
	movq	%r15, %r12
	jmp	.LBB24_47
.LBB24_42:                              # %if.end11.i
	movq	%rsi, %r14
	movzbl	3(%r15), %eax
	testq	$16, %rax
	je	.LBB24_46
# BB#43:                                # %land.lhs.true16.i
	movslq	(%r12), %rax
	testq	%rax, %rax
	jns	.LBB24_46
# BB#44:                                # %land.lhs.true20.i
	movq	$1, %rsi
	movq	%rdx, %rdi
	callq	*%rbx
	movq	%rbx, %rcx
	testq	%r14, %rax
	je	.LBB24_46
# BB#45:                                # %land.lhs.true23.i
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	*%rcx
	testq	%r14, %rax
	je	.LBB24_47
.LBB24_46:                              # %if.end27.i
	xorq	%r12, %r12
.LBB24_47:                              # %fixed_scalar_and_varying_struct_p.exit
	testq	%r12, %r12
	jne	.LBB24_48
# BB#49:                                # %fixed_scalar_and_varying_struct_p.exit
	movabsq	$1, %r14
	jmp	.LBB24_50
.LBB24_48:
	movabsq	$0, %r14
.LBB24_50:                              # %return
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp257:
	.size	canon_true_dependence, .Ltmp257-canon_true_dependence
	.cfi_endproc

	.globl	anti_dependence
	.align	16, 0x90
	.type	anti_dependence,@function
anti_dependence:                        # @anti_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rdx, %rdx
	jmp	write_dependence_p  # TAILCALL
.Ltmp258:
	.size	anti_dependence, .Ltmp258-anti_dependence
	.cfi_endproc

	.align	16, 0x90
	.type	write_dependence_p,@function
write_dependence_p:                     # @write_dependence_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp265:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp266:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp267:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp268:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp269:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp270:
	.cfi_def_cfa_offset 64
.Ltmp271:
	.cfi_offset %rbx, -48
.Ltmp272:
	.cfi_offset %r12, -40
.Ltmp273:
	.cfi_offset %r13, -32
.Ltmp274:
	.cfi_offset %r14, -24
.Ltmp275:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movl	(%r15), %eax
	movq	%rdx, %r12
	movq	%rdi, %r14
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB26_2
# BB#1:                                 # %land.lhs.true
	movzbl	3(%r14), %ecx
	movq	$1, %rbx
	testq	$8, %rcx
	jne	.LBB26_66
.LBB26_2:                               # %if.end
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB26_4
# BB#3:                                 # %land.lhs.true8
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %rbx
	je	.LBB26_66
.LBB26_4:                               # %if.end13
	movl	(%r14), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB26_6
# BB#5:                                 # %land.lhs.true18
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %rbx
	je	.LBB26_66
.LBB26_6:                               # %if.end26
	movq	16(%r15), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$0, %rdi
	je	.LBB26_8
# BB#7:                                 # %cond.false.i
	movq	(%rax), %rdi
.LBB26_8:                               # %cond.end.i
	movq	16(%r14), %rax
	testq	%rax, %rax
	je	.LBB26_10
# BB#9:                                 # %cond.false9.i
	movq	(%rax), %rsi
.LBB26_10:                              # %mems_in_disjoint_alias_sets_p.exit
	callq	alias_sets_conflict_p
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB26_66
# BB#11:                                # %if.end29
	movl	(%r15), %eax
	movq	$26, %rcx
	movl	(%r14), %edx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	$26, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	cmpq	%rdx, %rax
	jne	.LBB26_66
# BB#12:                                # %if.end38
	testq	%r13, %r12
	jne	.LBB26_14
# BB#13:                                # %if.end38
	testq	%rax, %rax
	jne	.LBB26_66
.LBB26_14:                              # %if.end46
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	nonoverlapping_memrefs_p
	testq	%r13, %rax
	jne	.LBB26_66
# BB#15:                                # %if.end50
	movq	8(%r15), %rdi
	movzbl	(%rdi), %eax
	cmpq	$60, %rax
	jne	.LBB26_29
# BB#16:                                # %if.end.i
	movq	8(%rdi), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB26_29
# BB#17:
	movq	%rax, %rdx
.LBB26_18:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB26_21
# BB#19:                                # %for.body.i
                                        #   in Loop: Header=BB26_18 Depth=1
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB26_23
# BB#20:                                # %for.body.i
                                        #   in Loop: Header=BB26_18 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	je	.LBB26_23
	jmp	.LBB26_29
.LBB26_21:                              # %for.body.i
                                        #   in Loop: Header=BB26_18 Depth=1
	cmpq	$134, %rcx
	je	.LBB26_29
# BB#22:                                # %for.body.i
                                        #   in Loop: Header=BB26_18 Depth=1
	cmpq	$140, %rcx
	je	.LBB26_29
.LBB26_23:                              # %for.inc.i
                                        #   in Loop: Header=BB26_18 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB26_18
# BB#24:
	movq	%rax, %rcx
.LBB26_25:                              # %for.body44.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rdi
	movzbl	(%rdi), %edx
	cmpq	$61, %rdx
	je	.LBB26_27
# BB#26:                                # %for.body44.i
                                        #   in Loop: Header=BB26_25 Depth=1
	cmpq	$66, %rdx
	jne	.LBB26_29
.LBB26_27:                              # %for.inc56.i
                                        #   in Loop: Header=BB26_25 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB26_25
# BB#28:                                # %if.then61.i
	movq	8(%rax), %rdi
.LBB26_29:                              # %get_addr.exit
	movq	8(%r14), %rsi
	movzbl	(%rsi), %eax
	cmpq	$60, %rax
	jne	.LBB26_43
# BB#30:                                # %if.end.i180
	movq	8(%rsi), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB26_43
# BB#31:
	movq	%rax, %rdx
.LBB26_32:                              # %for.body.i186
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$133, %rcx
	jg	.LBB26_35
# BB#33:                                # %for.body.i186
                                        #   in Loop: Header=BB26_32 Depth=1
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB26_37
# BB#34:                                # %for.body.i186
                                        #   in Loop: Header=BB26_32 Depth=1
	movq	$1, %r8
	shlq	%cl, %r8
	testq	$24599, %r8             # imm = 0x6017
	je	.LBB26_37
	jmp	.LBB26_43
.LBB26_35:                              # %for.body.i186
                                        #   in Loop: Header=BB26_32 Depth=1
	cmpq	$134, %rcx
	je	.LBB26_43
# BB#36:                                # %for.body.i186
                                        #   in Loop: Header=BB26_32 Depth=1
	cmpq	$140, %rcx
	je	.LBB26_43
.LBB26_37:                              # %for.inc.i190
                                        #   in Loop: Header=BB26_32 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB26_32
# BB#38:
	movq	%rax, %rcx
.LBB26_39:                              # %for.body44.i195
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rsi
	movzbl	(%rsi), %edx
	cmpq	$61, %rdx
	je	.LBB26_41
# BB#40:                                # %for.body44.i195
                                        #   in Loop: Header=BB26_39 Depth=1
	cmpq	$66, %rdx
	jne	.LBB26_43
.LBB26_41:                              # %for.inc56.i199
                                        #   in Loop: Header=BB26_39 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB26_39
# BB#42:                                # %if.then61.i202
	movq	8(%rax), %rsi
.LBB26_43:                              # %get_addr.exit204
	testq	%r13, %r12
	jne	.LBB26_48
# BB#44:                                # %if.then60
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	%rsi, %rdi
	movq	%rsi, %r12
	callq	find_base_term
	movq	%r12, %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	testq	%rax, %rax
	je	.LBB26_48
# BB#45:                                # %land.lhs.true63
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$67, %rcx
	je	.LBB26_66
# BB#46:                                # %land.lhs.true63
	cmpq	$68, %rcx
	jne	.LBB26_48
# BB#47:                                # %land.lhs.true70
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB26_66
.LBB26_48:                              # %if.end77
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movzbl	2(%r14), %ecx
	movzbl	2(%r15), %edx
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	callq	base_alias_check
	testq	%r13, %rax
	je	.LBB26_66
# BB#49:                                # %if.end87
	movq	%r12, %rdi
	callq	canon_rtx
	movq	%rax, %r12
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	canon_rtx
	movzbl	2(%r14), %ecx
	movzbl	2(%r15), %edx
	movzbl	mode_size(%rcx), %edi
	movzbl	mode_size(%rdx), %edx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%rax, %rsi
	movq	%r12, %rcx
	callq	memrefs_conflict_p
	testq	%r13, %rax
	je	.LBB26_66
# BB#50:                                # %if.end103
	movl	flag_strict_aliasing(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB26_60
# BB#51:                                # %if.end.i168
	movslq	(%r14), %rax
	testq	%rax, %rax
	jns	.LBB26_55
# BB#52:                                # %land.lhs.true.i
	movzbl	3(%r15), %eax
	testq	$16, %rax
	je	.LBB26_55
# BB#53:                                # %land.lhs.true5.i
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	rtx_addr_varies_p
	testq	%r13, %rax
	jne	.LBB26_55
# BB#54:                                # %land.lhs.true7.i
	movq	$1, %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	rtx_addr_varies_p
	testq	%r13, %rax
	jne	.LBB26_61
.LBB26_55:                              # %if.end11.i
	movzbl	3(%r14), %eax
	testq	$16, %rax
	je	.LBB26_59
# BB#56:                                # %land.lhs.true16.i
	movslq	(%r15), %rax
	testq	%rax, %rax
	jns	.LBB26_59
# BB#57:                                # %land.lhs.true20.i
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	rtx_addr_varies_p
	testq	%r13, %rax
	je	.LBB26_59
# BB#58:                                # %land.lhs.true23.i
	movq	$1, %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	rtx_addr_varies_p
	testq	%r13, %rax
	movq	%r15, %rax
	je	.LBB26_60
.LBB26_59:                              # %if.end27.i
	xorq	%rax, %rax
.LBB26_60:                              # %fixed_scalar_and_varying_struct_p.exit
	cmpq	%r14, %rax
	jne	.LBB26_62
.LBB26_61:                              # %land.lhs.true107
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	xorq	%rbx, %rbx
	cmpq	$83, %rax
	movq	%r14, %rax
	jne	.LBB26_66
.LBB26_62:                              # %land.rhs
	movq	$1, %rbx
	cmpq	%r15, %rax
	jne	.LBB26_66
# BB#63:                                # %land.rhs112
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$83, %rax
	jne	.LBB26_64
# BB#65:                                # %land.rhs112
	movabsq	$1, %rbx
	jmp	.LBB26_66
.LBB26_64:
	movabsq	$0, %rbx
.LBB26_66:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp276:
	.size	write_dependence_p, .Ltmp276-write_dependence_p
	.cfi_endproc

	.globl	output_dependence
	.align	16, 0x90
	.type	output_dependence,@function
output_dependence:                      # @output_dependence
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rdx
	jmp	write_dependence_p  # TAILCALL
.Ltmp277:
	.size	output_dependence, .Ltmp277-output_dependence
	.cfi_endproc

	.globl	mark_constant_function
	.align	16, 0x90
	.type	mark_constant_function,@function
mark_constant_function:                 # @mark_constant_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp281:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp282:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp283:
	.cfi_def_cfa_offset 32
.Ltmp284:
	.cfi_offset %rbx, -24
.Ltmp285:
	.cfi_offset %r14, -16
	movq	current_function_decl(%rip), %rax
	movl	16(%rax), %ecx
	testq	$528384, %rcx           # imm = 0x81000
	jne	.LBB28_19
# BB#1:                                 # %lor.lhs.false6
	movq	48(%rax), %rdx
	movabsq	$2147483648, %rsi       # imm = 0x80000000
	testq	%rsi, %rdx
	jne	.LBB28_19
# BB#2:                                 # %lor.lhs.false6
	andq	$2048, %rcx             # imm = 0x800
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rcx
	jne	.LBB28_19
# BB#3:                                 # %lor.lhs.false17
	movq	8(%rax), %rax
	movzbl	61(%rax), %eax
	testq	$254, %rax
	je	.LBB28_19
# BB#4:                                 # %if.end
	callq	mark_dfs_back_edges
	testq	%rax, %rax
	jne	.LBB28_19
# BB#5:                                 # %if.end24
	callq	init_alias_analysis
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB28_6
.LBB28_9:                               # %for.inc
                                        #   in Loop: Header=BB28_6 Depth=1
	movq	24(%rbx), %rbx
.LBB28_6:                               # %if.end24
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB28_10
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB28_6 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB28_9
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB28_6 Depth=1
	movq	%rbx, %rdi
	callq	nonlocal_mentioned_p
	testq	%r14, %rax
	je	.LBB28_9
# BB#15:                                # %for.end
	movq	reg_known_value(%rip), %rdi
	addq	$424, %rdi              # imm = 0x1A8
	callq	free
	movq	reg_known_equiv_p(%rip), %rdi
	movq	$0, reg_known_value(%rip)
	xorq	%rbx, %rbx
	movl	%ebx, reg_known_value_size(%rip)
	addq	$53, %rdi
	callq	free
	movq	reg_base_value(%rip), %rdi
	movq	$0, reg_known_equiv_p(%rip)
	testq	%rdi, %rdi
	je	.LBB28_17
# BB#16:                                # %if.then.i
	callq	ggc_del_root
	movq	reg_base_value(%rip), %rdi
	callq	free
	movq	$0, reg_base_value(%rip)
.LBB28_17:                              # %if.end.i
	movq	alias_invariant(%rip), %rdi
	movl	%ebx, reg_base_value_size(%rip)
	testq	%rdi, %rdi
	je	.LBB28_19
# BB#18:                                # %if.then3.i
	callq	free
	movq	$0, alias_invariant(%rip)
	jmp	.LBB28_19
.LBB28_10:                              # %if.then37.critedge
	movq	reg_known_value(%rip), %rdi
	addq	$424, %rdi              # imm = 0x1A8
	callq	free
	movq	reg_known_equiv_p(%rip), %rdi
	movq	$0, reg_known_value(%rip)
	xorq	%rbx, %rbx
	movl	%ebx, reg_known_value_size(%rip)
	addq	$53, %rdi
	callq	free
	movq	reg_base_value(%rip), %rdi
	movq	$0, reg_known_equiv_p(%rip)
	testq	%rdi, %rdi
	je	.LBB28_12
# BB#11:                                # %if.then.i51
	callq	ggc_del_root
	movq	reg_base_value(%rip), %rdi
	callq	free
	movq	$0, reg_base_value(%rip)
.LBB28_12:                              # %if.end.i53
	movq	alias_invariant(%rip), %rdi
	movl	%ebx, reg_base_value_size(%rip)
	testq	%rdi, %rdi
	je	.LBB28_14
# BB#13:                                # %if.then3.i54
	callq	free
	movq	$0, alias_invariant(%rip)
.LBB28_14:                              # %end_alias_analysis.exit55
	movq	current_function_decl(%rip), %rax
	movl	16(%rax), %ecx
	orq	$4096, %rcx             # imm = 0x1000
	movl	%ecx, 16(%rax)
.LBB28_19:                              # %if.end42
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp286:
	.size	mark_constant_function, .Ltmp286-mark_constant_function
	.cfi_endproc

	.globl	init_alias_analysis
	.align	16, 0x90
	.type	init_alias_analysis,@function
init_alias_analysis:                    # @init_alias_analysis
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp293:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp294:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp295:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp296:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp297:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp298:
	.cfi_def_cfa_offset 80
.Ltmp299:
	.cfi_offset %rbx, -48
.Ltmp300:
	.cfi_offset %r12, -40
.Ltmp301:
	.cfi_offset %r13, -32
.Ltmp302:
	.cfi_offset %r14, -24
.Ltmp303:
	.cfi_offset %r15, -16
	callq	max_reg_num
	movq	%rax, %r14
	movq	%r14, (%rsp)            # 8-byte Spill
	movl	%r14d, reg_known_value_size(%rip)
	leaq	-53(%r14), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	addq	$-424, %rax             # imm = 0xFFFFFFFFFFFFFE58
	movq	%rax, reg_known_value(%rip)
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	addq	$-53, %rax
	movq	%rax, reg_known_equiv_p(%rip)
	leaq	(%r14,%r14), %rax
	movl	%eax, reg_base_value_size(%rip)
	movabsq	$4294967294, %rdi       # imm = 0xFFFFFFFE
	andq	%rax, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movl	reg_base_value_size(%rip), %esi
	movq	%rax, reg_base_value(%rip)
	movq	%rax, %rdi
	callq	ggc_add_rtx_root
	movl	reg_base_value_size(%rip), %edi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movl	reg_base_value_size(%rip), %edi
	movq	%rax, new_reg_base_value(%rip)
	callq	xmalloc
	movl	reload_completed(%rip), %ecx
	movq	%rax, reg_seen(%rip)
	testq	%rcx, %rcx
	jne	.LBB29_3
# BB#1:                                 # %entry
	movl	flag_unroll_loops(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB29_3
# BB#2:                                 # %if.then
	movl	reg_base_value_size(%rip), %esi
	movq	alias_invariant(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	reg_base_value_size(%rip), %edx
	movq	%rax, alias_invariant(%rip)
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	memset
.LBB29_3:                               # %do.body.preheader
	xorq	%r12, %r12
	movl	reg_base_value_size(%rip), %edx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB29_4:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_5 Depth 2
                                        #     Child Loop BB29_9 Depth 2
                                        #     Child Loop BB29_55 Depth 2
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movl	%r12d, unique_id(%rip)
	movq	$1, %rax
	movb	%al, copying_arguments(%rip)
	movq	new_reg_base_value(%rip), %rdi
	andq	%r15, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	reg_seen(%rip), %rdi
	movl	reg_base_value_size(%rip), %edx
	xorq	%rsi, %rsi
	callq	memset
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB29_5:                               # %for.body
                                        #   Parent Loop BB29_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	argument_registers(%rip), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB29_7
# BB#6:                                 # %if.then24
                                        #   in Loop: Header=BB29_5 Depth=2
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	$25, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, (%rcx,%rbx,8)
.LBB29_7:                               # %for.inc
                                        #   in Loop: Header=BB29_5 Depth=2
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB29_5
# BB#8:                                 # %for.end
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl+16(%rip), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$25, %rdi
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, 56(%rcx)
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl+40(%rip), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$25, %rdi
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, 128(%rcx)
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl+24(%rip), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$25, %rdi
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, 160(%rcx)
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	global_rtl+32(%rip), %rdx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$25, %rdi
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, 48(%rcx)
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB29_9
	.align	16, 0x90
.LBB29_54:                              # %for.inc298
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	24(%rbx), %rbx
.LBB29_9:                               # %for.end
                                        #   Parent Loop BB29_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB29_10
# BB#12:                                # %for.body53
                                        #   in Loop: Header=BB29_9 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB29_51
# BB#13:                                # %if.then59
                                        #   in Loop: Header=BB29_9 Depth=2
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_15
# BB#14:                                # %land.lhs.true61
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	%rbx, %rdi
	callq	prologue_epilogue_contains
	testq	%r15, %rax
	jne	.LBB29_54
.LBB29_15:                              # %if.end65
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB29_23
# BB#16:                                # %land.lhs.true71
                                        #   in Loop: Header=BB29_9 Depth=2
	cmpq	$0, 56(%rbx)
	je	.LBB29_23
# BB#17:                                # %land.lhs.true77
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	$18, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	32(%rbx), %rdi
	testq	%rax, %rax
	jne	.LBB29_18
.LBB29_23:                              # %if.else
                                        #   in Loop: Header=BB29_9 Depth=2
	xorq	%rdx, %rdx
	movabsq	$record_set, %rsi
	callq	note_stores
.LBB29_24:                              # %if.end90
                                        #   in Loop: Header=BB29_9 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB29_54
# BB#25:                                # %cond.true
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	32(%rbx), %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	je	.LBB29_27
# BB#26:                                # %cond.false
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	single_set_2
	movq	%rax, %r13
.LBB29_27:                              # %cond.end115
                                        #   in Loop: Header=BB29_9 Depth=2
	testq	%r13, %r13
	je	.LBB29_54
# BB#28:                                # %land.lhs.true119
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB29_54
# BB#29:                                # %land.lhs.true127
                                        #   in Loop: Header=BB29_9 Depth=2
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB29_54
# BB#30:                                # %if.then135
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	16(%r13), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	cmpq	$0, 56(%rbx)
	je	.LBB29_40
# BB#31:                                # %land.lhs.true150
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB29_33
# BB#32:                                # %land.lhs.true154
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movl	12(%rax), %eax
	cmpq	$1, %rax
	je	.LBB29_34
.LBB29_33:                              # %lor.lhs.false
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB29_40
.LBB29_34:                              # %land.lhs.true162
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	cmpq	$3, %rax
	je	.LBB29_40
# BB#35:                                # %land.lhs.true170
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	$1, %rsi
	callq	rtx_varies_p
	testq	%r15, %rax
	jne	.LBB29_40
# BB#36:                                # %land.lhs.true176
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%r13), %rdi
	movq	8(%r14), %rsi
	callq	reg_overlap_mentioned_p
	testq	%r15, %rax
	jne	.LBB29_40
# BB#37:                                # %if.then185
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%r14), %rax
	movq	reg_known_value(%rip), %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rax, (%rcx,%rdx,8)
	movl	(%r14), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$196608, %rax           # imm = 0x30000
	movabsq	$0, %rax
	jne	.LBB29_39
# BB#38:                                # %if.then185
                                        #   in Loop: Header=BB29_9 Depth=2
	movabsq	$1, %rax
.LBB29_39:                              # %if.then185
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_known_equiv_p(%rip), %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movb	%al, (%rcx,%rdx)
	movq	24(%rbx), %rbx
	jmp	.LBB29_9
	.align	16, 0x90
.LBB29_51:                              # %if.else285
                                        #   in Loop: Header=BB29_9 Depth=2
	cmpq	$37, %rax
	jne	.LBB29_54
# BB#52:                                # %land.lhs.true290
                                        #   in Loop: Header=BB29_9 Depth=2
	movl	40(%rbx), %eax
	movabsq	$4294967209, %rcx       # imm = 0xFFFFFFA9
	cmpq	%rcx, %rax
	jne	.LBB29_54
# BB#53:                                # %if.then295
                                        #   in Loop: Header=BB29_9 Depth=2
	movb	%r12b, copying_arguments(%rip)
	jmp	.LBB29_54
.LBB29_18:                              # %if.then80
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB29_24
# BB#19:                                # %if.end.i
                                        #   in Loop: Header=BB29_9 Depth=2
	movl	8(%rax), %r13d
	movl	reg_base_value_size(%rip), %eax
	cmpq	%rax, %r13
	jae	.LBB29_78
# BB#20:                                # %if.end3.i
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_seen(%rip), %rax
	movzbl	(%rax,%r13), %ecx
	testq	%rcx, %rcx
	jne	.LBB29_21
# BB#22:                                # %if.end19.i
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	$1, %rcx
	movb	%cl, (%rax,%r13)
	movl	target_flags(%rip), %r14d
	movq	$25, %rcx
	movslq	unique_id(%rip), %rsi
	shrq	%cl, %r14
	andq	$1, %r14
	orq	$4, %r14
	leaq	1(%rsi), %rax
	movl	%eax, unique_id(%rip)
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$25, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, (%rcx,%r13,8)
	jmp	.LBB29_24
.LBB29_40:                              # %if.else198
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_n_info(%rip), %rax
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	32(%rax,%r14,8), %rax
	movl	12(%rax), %eax
	cmpq	$1, %rax
	movq	8(%rsp), %r13           # 8-byte Reload
	jne	.LBB29_54
# BB#41:                                # %land.lhs.true206
                                        #   in Loop: Header=BB29_9 Depth=2
	movzbl	(%r13), %ecx
	cmpq	$75, %rcx
	jne	.LBB29_48
# BB#42:                                # %land.lhs.true211
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%r13), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB29_47
# BB#43:                                # %land.lhs.true219
                                        #   in Loop: Header=BB29_9 Depth=2
	movl	8(%rcx), %ecx
	cmpq	$53, %rcx
	jb	.LBB29_47
# BB#44:                                # %land.lhs.true228
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_known_value(%rip), %rdx
	movq	(%rdx,%rcx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB29_47
# BB#45:                                # %land.lhs.true238
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	16(%r13), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB29_47
# BB#46:                                # %if.then246
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	8(%rcx), %rsi
	callq	plus_constant_wide
	movq	reg_known_value(%rip), %rcx
	movq	%rax, (%rcx,%r14,8)
	jmp	.LBB29_50
.LBB29_21:                              # %if.then16.i
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	new_reg_base_value(%rip), %rax
	movq	$0, (%rax,%r13,8)
	jmp	.LBB29_24
.LBB29_47:                              # %if.else265
                                        #   in Loop: Header=BB29_9 Depth=2
	cmpq	$1, %rax
	jne	.LBB29_54
.LBB29_48:                              # %land.lhs.true273
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	rtx_varies_p
	testq	%r15, %rax
	jne	.LBB29_54
# BB#49:                                # %if.then276
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_known_value(%rip), %rax
	movq	%r13, (%rax,%r14,8)
.LBB29_50:                              # %for.inc298
                                        #   in Loop: Header=BB29_9 Depth=2
	movq	reg_known_equiv_p(%rip), %rax
	movb	%r12b, (%rax,%r14)
	movq	24(%rbx), %rbx
	jmp	.LBB29_9
	.align	16, 0x90
.LBB29_10:                              # %for.cond303.preheader
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	reg_base_value_size(%rip), %eax
	xorq	%r14, %r14
	testq	%rax, %rax
	movabsq	$0, %r13
	movabsq	$0, %rbx
	je	.LBB29_11
	.align	16, 0x90
.LBB29_55:                              # %for.body306
                                        #   Parent Loop BB29_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	new_reg_base_value(%rip), %rax
	movq	%rbx, %r12
	andq	%r15, %r12
	movq	(%rax,%r12,8), %rdi
	testq	%rdi, %rdi
	je	.LBB29_59
# BB#56:                                # %land.lhs.true310
                                        #   in Loop: Header=BB29_55 Depth=2
	movq	reg_base_value(%rip), %rax
	movq	(%rax,%r12,8), %rsi
	cmpq	%rsi, %rdi
	je	.LBB29_59
# BB#57:                                # %land.lhs.true317
                                        #   in Loop: Header=BB29_55 Depth=2
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB29_59
# BB#58:                                # %if.then324
                                        #   in Loop: Header=BB29_55 Depth=2
	movq	new_reg_base_value(%rip), %rax
	movq	(%rax,%r12,8), %rax
	movq	reg_base_value(%rip), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	$1, %r13
	.align	16, 0x90
.LBB29_59:                              # %for.inc330
                                        #   in Loop: Header=BB29_55 Depth=2
	movl	reg_base_value_size(%rip), %edx
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%rdx, %rax
	jb	.LBB29_55
# BB#60:                                # %do.cond
                                        #   in Loop: Header=BB29_4 Depth=1
	testq	%r15, %r13
	movabsq	$0, %r12
	je	.LBB29_62
# BB#61:                                # %land.rhs
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$10, %rax
	jl	.LBB29_4
	jmp	.LBB29_62
.LBB29_11:
	xorq	%rdx, %rdx
.LBB29_62:                              # %for.cond337.preheader
	movq	$32, %rcx
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$54, %rax
	jl	.LBB29_67
# BB#63:
	movq	$54, %rax
	andq	%r15, %rdi
	.align	16, 0x90
.LBB29_64:                              # %for.body340
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_known_value(%rip), %rcx
	cmpq	$0, -8(%rcx,%rax,8)
	jne	.LBB29_66
# BB#65:                                # %if.then345
                                        #   in Loop: Header=BB29_64 Depth=1
	movq	cfun(%rip), %rsi
	movq	24(%rsi), %rsi
	movq	80(%rsi), %rsi
	movq	-8(%rsi,%rax,8), %rsi
	movq	%rsi, -8(%rcx,%rax,8)
.LBB29_66:                              # %for.inc351
                                        #   in Loop: Header=BB29_64 Depth=1
	movq	%rax, %rcx
	andq	%r15, %rcx
	incq	%rax
	cmpq	%rdi, %rcx
	jne	.LBB29_64
	.align	16, 0x90
.LBB29_67:                              # %do.body354
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_69 Depth 2
	testq	%rdx, %rdx
	je	.LBB29_77
# BB#68:                                #   in Loop: Header=BB29_67 Depth=1
	incq	%r14
	xorq	%r8, %r8
	xorq	%rax, %rax
	.align	16, 0x90
.LBB29_69:                              # %for.body359
                                        #   Parent Loop BB29_67 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	reg_base_value(%rip), %rsi
	movq	%rax, %rdi
	andq	%r15, %rdi
	movq	(%rsi,%rdi,8), %rbx
	testq	%rbx, %rbx
	je	.LBB29_74
# BB#70:                                # %land.lhs.true363
                                        #   in Loop: Header=BB29_69 Depth=2
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	jne	.LBB29_74
# BB#71:                                # %if.then368
                                        #   in Loop: Header=BB29_69 Depth=2
	movl	8(%rbx), %ebx
	xorq	%rcx, %rcx
	cmpq	%rdi, %rbx
	je	.LBB29_73
# BB#72:                                # %if.else377
                                        #   in Loop: Header=BB29_69 Depth=2
	movq	(%rsi,%rbx,8), %rcx
.LBB29_73:                              # %if.end382
                                        #   in Loop: Header=BB29_69 Depth=2
	movq	%rcx, (%rsi,%rdi,8)
	movq	$1, %r8
.LBB29_74:                              # %for.inc384
                                        #   in Loop: Header=BB29_69 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	%rdx, %rcx
	jb	.LBB29_69
# BB#75:                                # %do.cond387
                                        #   in Loop: Header=BB29_67 Depth=1
	testq	%r15, %r8
	je	.LBB29_77
# BB#76:                                # %do.cond387
                                        #   in Loop: Header=BB29_67 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$10, %rax
	jl	.LBB29_67
.LBB29_77:                              # %do.end393
	movq	new_reg_base_value(%rip), %rdi
	callq	free
	movq	reg_seen(%rip), %rdi
	movq	$0, new_reg_base_value(%rip)
	callq	free
	movq	$0, reg_seen(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB29_78:                              # %if.then2.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.record_set, %rcx
	movq	$911, %rsi              # imm = 0x38F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp304:
	.size	init_alias_analysis, .Ltmp304-init_alias_analysis
	.cfi_endproc

	.align	16, 0x90
	.type	nonlocal_mentioned_p,@function
nonlocal_mentioned_p:                   # @nonlocal_mentioned_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp311:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp312:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp313:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp314:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp315:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp316:
	.cfi_def_cfa_offset 64
.Ltmp317:
	.cfi_offset %rbx, -48
.Ltmp318:
	.cfi_offset %r12, -40
.Ltmp319:
	.cfi_offset %r13, -32
.Ltmp320:
	.cfi_offset %r14, -24
.Ltmp321:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB30_5
# BB#1:                                 # %if.then
	andq	$67174399, %rdx         # imm = 0x400FFFF
	cmpq	$67108898, %rdx         # imm = 0x4000022
	jne	.LBB30_3
# BB#2:                                 # %if.then6
	movq	64(%rbx), %rbx
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%rbx, %rbx
	jne	.LBB30_4
	jmp	.LBB30_46
.LBB30_3:                               # %if.else
	movq	32(%rbx), %rbx
.LBB30_4:                               # %if.end14
	movl	(%rbx), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
.LBB30_5:                               # %if.end17
	leaq	-40(%rax), %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$29, %rcx
	ja	.LBB30_34
# BB#6:                                 # %if.end17
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmpq	*.LJTI30_0(,%rcx,8)
.LBB30_33:                              # %sw.bb125
	testq	$134217728, %rdx        # imm = 0x8000000
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jne	.LBB30_46
	jmp	.LBB30_34
.LBB30_15:                              # %sw.bb62
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_11:                              # %sw.bb41
	movslq	8(%rbx), %rax
	cmpq	$52, %rax
	jg	.LBB30_13
# BB#12:                                # %land.lhs.true47
	movzbl	global_regs(%rax), %eax
	testq	%rax, %rax
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jne	.LBB30_46
.LBB30_13:                              # %if.end53
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_7:                               # %sw.bb
	movq	8(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB30_34
# BB#8:                                 # %if.then25
	movl	8(%rcx), %eax
	cmpq	$52, %rax
	ja	.LBB30_10
# BB#9:                                 # %land.lhs.true33
	movq	%rbx, %rdi
	callq	subreg_regno
	andq	%r14, %rax
	movzbl	global_regs(%rax), %eax
	testq	%rax, %rax
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jne	.LBB30_46
.LBB30_10:                              # %if.end39
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_34:                              # %sw.epilog
	movzbl	rtx_length(%rax), %r13d
	testq	%r13, %r13
	je	.LBB30_35
# BB#36:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB30_37:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB30_45 Depth 2
	movq	%r13, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r12), %eax
	leaq	-1(%r12), %r13
	cmpq	$69, %rax
	jne	.LBB30_38
# BB#43:                                # %for.cond166.preheader
                                        #   in Loop: Header=BB30_37 Depth=1
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	movq	$1, %r15
	jle	.LBB30_41
	.align	16, 0x90
.LBB30_45:                              # %for.body172
                                        #   Parent Loop BB30_37 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r15,8), %rdi
	callq	nonlocal_mentioned_p
	testq	%r14, %rax
	jne	.LBB30_46
# BB#44:                                # %for.cond166
                                        #   in Loop: Header=BB30_45 Depth=2
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB30_45
	jmp	.LBB30_41
	.align	16, 0x90
.LBB30_38:                              # %for.body
                                        #   in Loop: Header=BB30_37 Depth=1
	cmpq	$101, %rax
	jne	.LBB30_41
# BB#39:                                # %land.lhs.true144
                                        #   in Loop: Header=BB30_37 Depth=1
	movq	(%rbx,%r12,8), %rdi
	testq	%rdi, %rdi
	je	.LBB30_41
# BB#40:                                # %if.then150
                                        #   in Loop: Header=BB30_37 Depth=1
	callq	nonlocal_mentioned_p
	testq	%r14, %rax
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jne	.LBB30_46
	.align	16, 0x90
.LBB30_41:                              # %for.cond.backedge
                                        #   in Loop: Header=BB30_37 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB30_37
# BB#42:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_35:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_16:                              # %sw.bb63
	testq	$134217728, %rdx        # imm = 0x8000000
	je	.LBB30_18
# BB#17:
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_14:                              # %sw.bb55
	movq	$26, %rcx
	shrq	%cl, %rdx
	notq	%rdx
	andq	$1, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	jmp	.LBB30_46
.LBB30_18:                              # %if.end69
	movq	8(%rbx), %rdi
	callq	find_base_term
	testq	%rax, %rax
	je	.LBB30_32
# BB#19:                                # %if.then75
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$25, %rcx
	jne	.LBB30_30
# BB#20:                                # %land.lhs.true80
	movq	$16, %rcx
	movq	%rdx, %rsi
	movl	target_flags(%rip), %edi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	cmpq	%rdi, %rsi
	jne	.LBB30_30
# BB#21:                                # %land.lhs.true87
	movq	8(%rax), %rax
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_22
# BB#23:                                # %land.lhs.true87
	movq	global_rtl+40(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_24
# BB#25:                                # %land.lhs.true87
	movq	global_rtl+32(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_26
# BB#27:                                # %land.lhs.true87
	movq	global_rtl+24(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_28
# BB#29:                                # %land.lhs.true87
	andq	$67174399, %rdx         # imm = 0x400FFFF
	andq	%r14, %rdx
	jmp	.LBB30_31
.LBB30_30:                              # %if.end111
	andq	$67174399, %rdx         # imm = 0x400FFFF
.LBB30_31:                              # %if.end111
	cmpq	$67108932, %rdx         # imm = 0x4000044
	movabsq	$0, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	je	.LBB30_46
.LBB30_32:                              # %sw.bb124
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
.LBB30_46:                              # %return
	movq	8(%rsp), %rax           # 8-byte Reload
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_28:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_26:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_22:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.LBB30_24:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB30_46
.Ltmp322:
	.size	nonlocal_mentioned_p, .Ltmp322-nonlocal_mentioned_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI30_0:
	.quad	.LBB30_32
	.quad	.LBB30_33
	.quad	.LBB30_34
	.quad	.LBB30_32
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_15
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_46
	.quad	.LBB30_46
	.quad	.LBB30_46
	.quad	.LBB30_34
	.quad	.LBB30_46
	.quad	.LBB30_46
	.quad	.LBB30_34
	.quad	.LBB30_11
	.quad	.LBB30_46
	.quad	.LBB30_7
	.quad	.LBB30_34
	.quad	.LBB30_34
	.quad	.LBB30_16
	.quad	.LBB30_46
	.quad	.LBB30_14
	.quad	.LBB30_46

	.text
	.globl	end_alias_analysis
	.align	16, 0x90
	.type	end_alias_analysis,@function
end_alias_analysis:                     # @end_alias_analysis
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp325:
	.cfi_def_cfa_offset 16
.Ltmp326:
	.cfi_offset %rbx, -16
	movq	reg_known_value(%rip), %rdi
	addq	$424, %rdi              # imm = 0x1A8
	callq	free
	movq	reg_known_equiv_p(%rip), %rdi
	movq	$0, reg_known_value(%rip)
	xorq	%rbx, %rbx
	movl	%ebx, reg_known_value_size(%rip)
	addq	$53, %rdi
	callq	free
	movq	reg_base_value(%rip), %rdi
	movq	$0, reg_known_equiv_p(%rip)
	testq	%rdi, %rdi
	je	.LBB31_2
# BB#1:                                 # %if.then
	callq	ggc_del_root
	movq	reg_base_value(%rip), %rdi
	callq	free
	movq	$0, reg_base_value(%rip)
.LBB31_2:                               # %if.end
	movq	alias_invariant(%rip), %rdi
	movl	%ebx, reg_base_value_size(%rip)
	testq	%rdi, %rdi
	je	.LBB31_4
# BB#3:                                 # %if.then3
	callq	free
	movq	$0, alias_invariant(%rip)
.LBB31_4:                               # %if.end4
	popq	%rbx
	retq
.Ltmp327:
	.size	end_alias_analysis, .Ltmp327-end_alias_analysis
	.cfi_endproc

	.globl	init_alias_once
	.align	16, 0x90
	.type	init_alias_once,@function
init_alias_once:                        # @init_alias_once
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp331:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp332:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp333:
	.cfi_def_cfa_offset 32
.Ltmp334:
	.cfi_offset %rbx, -24
.Ltmp335:
	.cfi_offset %r14, -16
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB32_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	ix86_function_arg_regno_p
	testq	%rax, %rax
	je	.LBB32_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB32_1 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%rbx, %rdi
	callq	ix86_hard_regno_mode_ok
	testq	%r14, %rax
	je	.LBB32_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rax, argument_registers(%rip)
.LBB32_4:                               # %for.inc
                                        #   in Loop: Header=BB32_1 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB32_1
# BB#5:                                 # %for.end
	movabsq	$splay_tree_compare_ints, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	splay_tree_new
	movq	%rax, alias_sets(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp336:
	.size	init_alias_once, .Ltmp336-init_alias_once
	.cfi_endproc

	.align	16, 0x90
	.type	record_set,@function
record_set:                             # @record_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp340:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp341:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp342:
	.cfi_def_cfa_offset 32
.Ltmp343:
	.cfi_offset %rbx, -24
.Ltmp344:
	.cfi_offset %r14, -16
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB33_31
# BB#1:                                 # %if.end
	movl	8(%rdi), %r14d
	movl	reg_base_value_size(%rip), %eax
	cmpq	%rax, %r14
	jae	.LBB33_32
# BB#2:                                 # %if.end3
	testq	%rsi, %rsi
	je	.LBB33_5
# BB#3:                                 # %if.then4
	movzbl	(%rsi), %eax
	cmpq	$49, %rax
	je	.LBB33_4
# BB#25:                                # %if.end10
	movq	new_reg_base_value(%rip), %rdx
	movq	16(%rsi), %rax
	cmpq	$0, (%rdx,%r14,8)
	je	.LBB33_26
# BB#7:                                 # %if.then30
	movzbl	(%rax), %ecx
	cmpq	$134, %rcx
	jg	.LBB33_14
# BB#8:                                 # %if.then30
	cmpq	$75, %rcx
	je	.LBB33_18
# BB#9:                                 # %if.then30
	cmpq	$76, %rcx
	je	.LBB33_15
# BB#10:                                # %if.then30
	cmpq	$83, %rcx
	jne	.LBB33_13
# BB#11:                                # %sw.bb76
	movq	8(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB33_13
# BB#12:                                # %lor.lhs.false82
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	je	.LBB33_30
	jmp	.LBB33_13
.LBB33_5:                               # %if.else
	movq	reg_seen(%rip), %rax
	movzbl	(%rax,%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB33_6
.LBB33_4:                               # %if.then8
	movq	new_reg_base_value(%rip), %rax
	movq	$0, (%rax,%r14,8)
	jmp	.LBB33_31
.LBB33_6:                               # %if.end19
	movq	$1, %rcx
	movb	%cl, (%rax,%r14)
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movslq	unique_id(%rip), %rsi
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	leaq	1(%rsi), %rax
	movl	%eax, unique_id(%rip)
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$25, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, (%rcx,%r14,8)
	jmp	.LBB33_31
.LBB33_26:                              # %if.else96
	cmpq	$52, %r14
	ja	.LBB33_28
# BB#27:                                # %lor.lhs.false99
	movzbl	fixed_regs(%r14), %ecx
	testq	%rcx, %rcx
	jne	.LBB33_30
.LBB33_28:                              # %land.lhs.true103
	movq	reg_seen(%rip), %rcx
	movzbl	(%rcx,%r14), %ecx
	testq	%rcx, %rcx
	jne	.LBB33_30
# BB#29:                                # %if.then112
	movq	%rax, %rdi
	callq	find_base_value
	movq	new_reg_base_value(%rip), %rcx
	movq	%rax, (%rcx,%r14,8)
	jmp	.LBB33_30
.LBB33_14:                              # %if.then30
	cmpq	$135, %rcx
	jne	.LBB33_13
.LBB33_15:                              # %sw.bb
	movq	8(%rax), %rcx
	cmpq	%rdi, %rcx
	je	.LBB33_30
# BB#16:                                # %land.lhs.true
	movq	16(%rax), %rax
	cmpq	%rdi, %rax
	je	.LBB33_30
# BB#17:                                # %if.then43
	movq	$0, (%rdx,%r14,8)
	jmp	.LBB33_30
.LBB33_13:                              # %sw.default
	movq	$0, (%rdx,%r14,8)
	jmp	.LBB33_30
.LBB33_18:                              # %sw.bb47
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	cmpq	%rdi, %rcx
	je	.LBB33_19
# BB#20:                                # %if.else57
	cmpq	%rdi, %rax
	jne	.LBB33_24
	jmp	.LBB33_21
.LBB33_19:
	movq	%rax, %rcx
.LBB33_21:                              # %if.end68
	testq	%rcx, %rcx
	je	.LBB33_24
# BB#22:                                # %lor.lhs.false
	movq	%rcx, %rdi
	callq	find_base_value
	testq	%rax, %rax
	je	.LBB33_30
# BB#23:                                # %lor.lhs.false.if.then72_crit_edge
	movq	new_reg_base_value(%rip), %rdx
.LBB33_24:                              # %if.then72
	movq	$0, (%rdx,%r14,8)
.LBB33_30:                              # %if.end117
	movq	reg_seen(%rip), %rax
	movq	$1, %rcx
	movb	%cl, (%rax,%r14)
.LBB33_31:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB33_32:                              # %if.then2
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.record_set, %rcx
	movq	$911, %rsi              # imm = 0x38F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp345:
	.size	record_set, .Ltmp345-record_set
	.cfi_endproc

	.align	16, 0x90
	.type	rtx_equal_for_memref_p,@function
rtx_equal_for_memref_p:                 # @rtx_equal_for_memref_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp352:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp353:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp354:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp355:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp356:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp357:
	.cfi_def_cfa_offset 64
.Ltmp358:
	.cfi_offset %rbx, -48
.Ltmp359:
	.cfi_offset %r12, -40
.Ltmp360:
	.cfi_offset %r13, -32
.Ltmp361:
	.cfi_offset %r14, -24
.Ltmp362:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	testq	%rdi, %rdi
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB34_2
# BB#1:                                 # %entry
	movq	%rdx, %rax
.LBB34_2:                               # %entry
	testq	%rbx, %rbx
	jne	.LBB34_4
# BB#3:                                 # %entry
	movq	%rdx, %rcx
.LBB34_4:                               # %entry
	movq	%rdi, %rdx
	orq	%rbx, %rdx
	jne	.LBB34_6
# BB#5:
	movq	$1, %rsi
	jmp	.LBB34_73
.LBB34_6:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$0, %r13
.LBB34_7:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	testq	$1, %rax
	jne	.LBB34_73
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB34_7 Depth=1
	testq	$1, %rcx
	jne	.LBB34_73
# BB#9:                                 # %if.end5
                                        #   in Loop: Header=BB34_7 Depth=1
	callq	canon_rtx
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	canon_rtx
	movq	%rax, %r15
	cmpq	%r15, %r12
	je	.LBB34_10
# BB#11:                                # %if.end9
                                        #   in Loop: Header=BB34_7 Depth=1
	movl	(%r12), %ecx
	movl	(%r15), %edx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	%rsi, %rax
	jne	.LBB34_72
# BB#12:                                # %if.end14
                                        #   in Loop: Header=BB34_7 Depth=1
	xorq	%rcx, %rdx
	testq	$16711680, %rdx         # imm = 0xFF0000
	movabsq	$0, %rsi
	jne	.LBB34_73
# BB#13:                                # %if.end22
                                        #   in Loop: Header=BB34_7 Depth=1
	leaq	-54(%rax), %rdx
	andq	%r14, %rdx
	cmpq	$16, %rdx
	jbe	.LBB34_14
.LBB34_31:                              # %sw.epilog
                                        #   in Loop: Header=BB34_7 Depth=1
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$102, %rcx
	je	.LBB34_40
# BB#32:                                # %lor.lhs.false72
                                        #   in Loop: Header=BB34_7 Depth=1
	movsbq	rtx_class(%rax), %rcx
	cmpq	$98, %rcx
	jg	.LBB34_39
# BB#33:                                # %lor.lhs.false72
                                        #   in Loop: Header=BB34_7 Depth=1
	andq	$255, %rcx
	cmpq	$49, %rcx
	jne	.LBB34_34
# BB#46:                                # %if.then151
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	8(%r12), %rdi
	movq	8(%r15), %rbx
	testq	%rdi, %rdi
	movq	%r13, %rax
	jne	.LBB34_48
# BB#47:                                # %if.then151
                                        #   in Loop: Header=BB34_7 Depth=1
	movabsq	$1, %rax
.LBB34_48:                              # %if.then151
                                        #   in Loop: Header=BB34_7 Depth=1
	testq	%rbx, %rbx
	movq	%r13, %rcx
	jne	.LBB34_50
# BB#49:                                # %if.then151
                                        #   in Loop: Header=BB34_7 Depth=1
	movabsq	$1, %rcx
.LBB34_50:                              # %if.then151
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	%rdi, %rdx
	orq	%rbx, %rdx
	jne	.LBB34_7
	jmp	.LBB34_51
.LBB34_14:                              # %if.end22
                                        #   in Loop: Header=BB34_7 Depth=1
	jmpq	*.LJTI34_0(,%rdx,8)
.LBB34_15:                              # %sw.bb
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB34_16
# BB#17:                                # %sw.bb
	movabsq	$1, %rsi
	jmp	.LBB34_73
.LBB34_39:                              # %lor.lhs.false72
	andq	$255, %rcx
	cmpq	$99, %rcx
	jne	.LBB34_52
.LBB34_40:                              # %if.then77
	movq	8(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r14, %rax
	je	.LBB34_42
# BB#41:                                # %land.lhs.true86
	movq	16(%r12), %rdi
	movq	16(%r15), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r14, %rax
	movq	$1, %rsi
	jne	.LBB34_73
.LBB34_42:
	leaq	16(%r15), %rax
	movq	8(%r12), %rdi
	movq	(%rax), %rsi
	callq	rtx_equal_for_memref_p
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB34_73
# BB#43:                                # %land.rhs103
	movq	16(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r14, %rax
	jne	.LBB34_45
# BB#44:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_10:
	movq	$1, %rsi
	jmp	.LBB34_73
.LBB34_51:
	movq	$1, %rsi
	jmp	.LBB34_73
.LBB34_34:                              # %lor.lhs.false72
	cmpq	$50, %rcx
	je	.LBB34_36
# BB#35:                                # %lor.lhs.false72
	cmpq	$60, %rcx
	jne	.LBB34_52
.LBB34_36:                              # %if.then125
	movq	8(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_equal_for_memref_p
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB34_73
# BB#37:                                # %land.rhs134
	movq	16(%r12), %rdi
	movq	16(%r15), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r14, %rax
	je	.LBB34_38
.LBB34_45:                              # %land.rhs134
	movabsq	$1, %rsi
	jmp	.LBB34_73
.LBB34_52:                              # %if.end161
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB34_53
# BB#54:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB34_55:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB34_69 Depth 2
	movq	%rdx, %rbx
	movq	(%rsp), %rax            # 8-byte Reload
	movsbq	-1(%rax,%rbx), %rax
	leaq	-1(%rbx), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$68, %rax
	jg	.LBB34_59
# BB#56:                                # %for.body
                                        #   in Loop: Header=BB34_55 Depth=1
	andq	%r14, %rax
	movq	%r14, %r13
	cmpq	$48, %rax
	je	.LBB34_57
	jmp	.LBB34_74
	.align	16, 0x90
.LBB34_59:                              # %for.body
                                        #   in Loop: Header=BB34_55 Depth=1
	cmpq	$100, %rax
	jg	.LBB34_63
# BB#60:                                # %for.body
                                        #   in Loop: Header=BB34_55 Depth=1
	andq	%r14, %rax
	movq	%r14, %r13
	cmpq	$69, %rax
	jne	.LBB34_74
# BB#61:                                # %sw.bb185
                                        #   in Loop: Header=BB34_55 Depth=1
	movq	(%r12,%rbx,8), %rax
	movq	(%r15,%rbx,8), %rdx
	movl	(%rax), %esi
	movl	(%rdx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB34_72
# BB#62:                                # %for.cond198.preheader
                                        #   in Loop: Header=BB34_55 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %r14
	testq	%rsi, %rsi
	jle	.LBB34_57
	jmp	.LBB34_69
	.align	16, 0x90
.LBB34_68:                              # %for.cond198.for.body206_crit_edge
                                        #   in Loop: Header=BB34_69 Depth=2
	movq	(%r15,%rbx,8), %rdx
	incq	%r14
.LBB34_69:                              # %for.body206
                                        #   Parent Loop BB34_55 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	movq	(%rdx,%r14,8), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r13, %rax
	je	.LBB34_72
# BB#67:                                # %for.cond198
                                        #   in Loop: Header=BB34_69 Depth=2
	movq	(%r12,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB34_68
	jmp	.LBB34_57
	.align	16, 0x90
.LBB34_63:                              # %for.body
                                        #   in Loop: Header=BB34_55 Depth=1
	andq	%r14, %rax
	cmpq	$101, %rax
	je	.LBB34_70
# BB#64:                                # %for.body
                                        #   in Loop: Header=BB34_55 Depth=1
	cmpq	$115, %rax
	jne	.LBB34_65
# BB#71:                                # %sw.bb239
                                        #   in Loop: Header=BB34_55 Depth=1
	movq	(%r12,%rbx,8), %rdi
	movq	(%r15,%rbx,8), %rsi
	callq	strcmp
	testq	%r14, %rax
	movq	%r14, %r13
	je	.LBB34_57
	jmp	.LBB34_72
.LBB34_70:                              # %sw.bb225
                                        #   in Loop: Header=BB34_55 Depth=1
	movq	(%r12,%rbx,8), %rdi
	movq	(%r15,%rbx,8), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r14, %rax
	movq	%r14, %r13
	jne	.LBB34_57
	jmp	.LBB34_72
.LBB34_65:                              # %for.body
                                        #   in Loop: Header=BB34_55 Depth=1
	movq	%r14, %r13
	cmpq	$105, %rax
	jne	.LBB34_74
# BB#66:                                # %sw.bb172
                                        #   in Loop: Header=BB34_55 Depth=1
	movl	(%r12,%rbx,8), %eax
	movl	(%r15,%rbx,8), %ecx
	cmpq	%rcx, %rax
	jne	.LBB34_72
	.align	16, 0x90
.LBB34_57:                              # %for.cond.backedge
                                        #   in Loop: Header=BB34_55 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r13, %r14
	jg	.LBB34_55
# BB#58:
	movq	$1, %rsi
	jmp	.LBB34_73
.LBB34_72:
	xorq	%rsi, %rsi
	jmp	.LBB34_73
.LBB34_53:
	movq	$1, %rsi
.LBB34_73:                              # %return
	movq	%rsi, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB34_38:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_18:                              # %sw.bb27
	movl	8(%r12), %eax
	movl	8(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB34_19
# BB#20:                                # %sw.bb27
	movabsq	$1, %rsi
	jmp	.LBB34_73
.LBB34_21:                              # %sw.bb35
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB34_22
# BB#23:                                # %sw.bb35
	movabsq	$1, %rsi
	jmp	.LBB34_73
.LBB34_24:                              # %sw.bb43
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB34_25
# BB#26:                                # %sw.bb43
	movabsq	$1, %rsi
	jmp	.LBB34_73
.LBB34_27:                              # %sw.bb52
	movl	16(%r12), %eax
	movl	16(%r15), %ecx
	xorq	%rsi, %rsi
	cmpq	%rcx, %rax
	jne	.LBB34_73
# BB#28:                                # %land.rhs
	movq	8(%r12), %rdi
	movq	8(%r15), %rsi
	callq	rtx_equal_for_memref_p
	testq	%r14, %rax
	je	.LBB34_29
# BB#30:                                # %land.rhs
	movabsq	$1, %rsi
	jmp	.LBB34_73
.LBB34_16:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_19:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_22:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_25:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_29:
	movabsq	$0, %rsi
	jmp	.LBB34_73
.LBB34_74:                              # %sw.default252
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.rtx_equal_for_memref_p, %rcx
	movq	$1192, %rsi             # imm = 0x4A8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp363:
	.size	rtx_equal_for_memref_p, .Ltmp363-rtx_equal_for_memref_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI34_0:
	.quad	.LBB34_73
	.quad	.LBB34_73
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_15
	.quad	.LBB34_18
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_31
	.quad	.LBB34_21
	.quad	.LBB34_24
	.quad	.LBB34_31
	.quad	.LBB34_27

	.type	new_alias_set.last_alias_set,@object # @new_alias_set.last_alias_set
	.local	new_alias_set.last_alias_set
	.comm	new_alias_set.last_alias_set,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"alias.c"
	.size	.L.str, 8

	.type	.L__FUNCTION__.record_alias_subset,@object # @__FUNCTION__.record_alias_subset
.L__FUNCTION__.record_alias_subset:
	.asciz	"record_alias_subset"
	.size	.L__FUNCTION__.record_alias_subset, 20

	.type	alias_sets,@object      # @alias_sets
	.local	alias_sets
	.comm	alias_sets,8,8
	.type	get_varargs_alias_set.set,@object # @get_varargs_alias_set.set
	.data
	.align	8
get_varargs_alias_set.set:
	.quad	-1                      # 0xffffffffffffffff
	.size	get_varargs_alias_set.set, 8

	.type	get_frame_alias_set.set,@object # @get_frame_alias_set.set
	.align	8
get_frame_alias_set.set:
	.quad	-1                      # 0xffffffffffffffff
	.size	get_frame_alias_set.set, 8

	.type	reg_base_value_size,@object # @reg_base_value_size
	.local	reg_base_value_size
	.comm	reg_base_value_size,4,4
	.type	alias_invariant,@object # @alias_invariant
	.local	alias_invariant
	.comm	alias_invariant,8,8
	.type	reg_base_value,@object  # @reg_base_value
	.local	reg_base_value
	.comm	reg_base_value,8,8
	.type	reg_known_value_size,@object # @reg_known_value_size
	.local	reg_known_value_size
	.comm	reg_known_value_size,4,4
	.type	reg_known_value,@object # @reg_known_value
	.comm	reg_known_value,8,8
	.type	argument_registers,@object # @argument_registers
	.local	argument_registers
	.comm	argument_registers,8,8
	.type	reg_known_equiv_p,@object # @reg_known_equiv_p
	.comm	reg_known_equiv_p,8,8
	.type	new_reg_base_value,@object # @new_reg_base_value
	.local	new_reg_base_value
	.comm	new_reg_base_value,8,8
	.type	reg_seen,@object        # @reg_seen
	.local	reg_seen
	.comm	reg_seen,8,8
	.type	unique_id,@object       # @unique_id
	.local	unique_id
	.comm	unique_id,4,4
	.type	copying_arguments,@object # @copying_arguments
	.local	copying_arguments
	.comm	copying_arguments,1,1
	.type	.L__FUNCTION__.record_set,@object # @__FUNCTION__.record_set
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.record_set:
	.asciz	"record_set"
	.size	.L__FUNCTION__.record_set, 11

	.type	.L__FUNCTION__.rtx_equal_for_memref_p,@object # @__FUNCTION__.rtx_equal_for_memref_p
.L__FUNCTION__.rtx_equal_for_memref_p:
	.asciz	"rtx_equal_for_memref_p"
	.size	.L__FUNCTION__.rtx_equal_for_memref_p, 23


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
