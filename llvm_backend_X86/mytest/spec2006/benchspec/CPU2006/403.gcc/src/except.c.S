	.file	"except.c.bc"
	.text
	.globl	doing_eh
	.align	16, 0x90
	.type	doing_eh,@function
doing_eh:                               # @doing_eh
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rbx
	jne	.LBB0_4
# BB#1:                                 # %if.then
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB0_4
# BB#2:                                 # %if.then
	movzbl	doing_eh.warned(%rip), %eax
	xorq	$1, %rax
	testq	$1, %rax
	je	.LBB0_4
# BB#3:                                 # %if.then3
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
	movq	$1, %rax
	movb	%al, doing_eh.warned(%rip)
.LBB0_4:                                # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4:
	.size	doing_eh, .Ltmp4-doing_eh
	.cfi_endproc

	.globl	init_eh
	.align	16, 0x90
	.type	init_eh,@function
init_eh:                                # @init_eh
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp6:
	.cfi_def_cfa_offset 16
	movabsq	$exception_handler_label_map, %rdi
	movabsq	$mark_ehl_map, %rcx
	movq	$1, %rsi
	movq	$1, %rdx
	callq	ggc_add_root
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_1
# BB#2:                                 # %if.end
	movabsq	$t2r_hash, %rsi
	movabsq	$t2r_eq, %rdx
	movq	$31, %rdi
	xorq	%rcx, %rcx
	callq	htab_create
	movabsq	$type_to_runtime_map, %rdi
	movabsq	$t2r_mark, %rcx
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rax, type_to_runtime_map(%rip)
	popq	%rax
	jmp	ggc_add_root  # TAILCALL
.LBB1_1:                                # %return
	popq	%rax
	retq
.Ltmp7:
	.size	init_eh, .Ltmp7-init_eh
	.cfi_endproc

	.align	16, 0x90
	.type	mark_ehl_map,@function
mark_ehl_map:                           # @mark_ehl_map
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB2_1
# BB#2:                                 # %if.then
	movabsq	$mark_ehl_map_entry, %rsi
	xorq	%rdx, %rdx
	jmp	htab_traverse  # TAILCALL
.LBB2_1:                                # %if.end
	retq
.Ltmp8:
	.size	mark_ehl_map, .Ltmp8-mark_ehl_map
	.cfi_endproc

	.align	16, 0x90
	.type	t2r_hash,@function
t2r_hash:                               # @t2r_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdi), %rax
	andq	$262143, %rax           # imm = 0x3FFFF
	retq
.Ltmp9:
	.size	t2r_hash, .Ltmp9-t2r_hash
	.cfi_endproc

	.align	16, 0x90
	.type	t2r_eq,@function
t2r_eq:                                 # @t2r_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdi), %rax
	cmpq	%rsi, %rax
	jne	.LBB4_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB4_1:
	movabsq	$0, %rax
	retq
.Ltmp10:
	.size	t2r_eq, .Ltmp10-t2r_eq
	.cfi_endproc

	.align	16, 0x90
	.type	t2r_mark,@function
t2r_mark:                               # @t2r_mark
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	movabsq	$t2r_mark_1, %rsi
	xorq	%rdx, %rdx
	jmp	htab_traverse  # TAILCALL
.Ltmp11:
	.size	t2r_mark, .Ltmp11-t2r_mark
	.cfi_endproc

	.globl	init_eh_for_function
	.align	16, 0x90
	.type	init_eh_for_function,@function
init_eh_for_function:                   # @init_eh_for_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp13:
	.cfi_def_cfa_offset 16
	movq	$1, %rdi
	movq	$144, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	%rax, (%rcx)
	popq	%rax
	retq
.Ltmp14:
	.size	init_eh_for_function, .Ltmp14-init_eh_for_function
	.cfi_endproc

	.globl	mark_eh_status
	.align	16, 0x90
	.type	mark_eh_status,@function
mark_eh_status:                         # @mark_eh_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp23:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp24:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 48
.Ltmp26:
	.cfi_offset %rbx, -48
.Ltmp27:
	.cfi_offset %r12, -40
.Ltmp28:
	.cfi_offset %r13, -32
.Ltmp29:
	.cfi_offset %r14, -24
.Ltmp30:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB7_46
# BB#1:                                 # %if.end
	movq	8(%r15), %rax
	testq	%rax, %rax
	je	.LBB7_9
# BB#2:                                 # %if.then1
	movslq	60(%r15), %rbx
	testq	%rbx, %rbx
	jle	.LBB7_13
# BB#3:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB7_4
	.align	16, 0x90
.LBB7_8:                                # %for.inc.for.body_crit_edge
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	8(%r15), %rax
.LBB7_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB7_7
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_4 Depth=1
	movl	24(%rdi), %eax
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB7_7
# BB#6:                                 # %if.then6
                                        #   in Loop: Header=BB7_4 Depth=1
	callq	mark_eh_region
.LBB7_7:                                # %for.inc
                                        #   in Loop: Header=BB7_4 Depth=1
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_8
	jmp	.LBB7_13
.LBB7_9:                                # %if.else
	movq	(%r15), %rax
	testq	%rax, %rax
	je	.LBB7_13
	.align	16, 0x90
.LBB7_10:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_11 Depth 2
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	mark_eh_region
	movq	8(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB7_10
.LBB7_11:                               # %if.else15
                                        #   Parent Loop BB7_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB7_10
# BB#12:                                # %do.body
                                        #   in Loop: Header=BB7_11 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_11
.LBB7_13:                               # %do.body30
	movq	32(%r15), %r14
	testq	%r14, %r14
	je	.LBB7_18
# BB#14:                                # %land.lhs.true32
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_18
# BB#15:                                # %do.body35
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB7_17
# BB#16:                                # %if.then37
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB7_17:                               # %if.end40
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB7_18:                               # %do.body48
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_21
# BB#19:                                # %land.lhs.true50
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_21
# BB#20:                                # %if.then53
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_21:                               # %do.body57
	movq	48(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_24
# BB#22:                                # %land.lhs.true60
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_24
# BB#23:                                # %if.then63
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_24:                               # %do.end66
	movq	64(%r15), %rdi
	callq	ggc_mark_tree_varray
	movq	88(%r15), %rax
	testq	%rax, %rax
	je	.LBB7_32
# BB#25:                                # %if.then68
	movslq	96(%r15), %r14
	testq	%r14, %r14
	jle	.LBB7_32
# BB#26:                                # %do.body72.lr.ph
	movq	$4, %rcx
	leaq	-1(%r14), %r12
	shlq	%cl, %r14
	addq	$-16, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB7_27
	.align	16, 0x90
.LBB7_30:                               # %for.cond69.backedge.do.body72_crit_edge
                                        #   in Loop: Header=BB7_27 Depth=1
	movq	88(%r15), %rax
	decq	%r12
	addq	$-16, %r14
.LBB7_27:                               # %do.body72
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB7_29
# BB#28:                                # %land.lhs.true78
                                        #   in Loop: Header=BB7_27 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB7_29
# BB#31:                                # %if.then81
                                        #   in Loop: Header=BB7_27 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_29:                               # %for.cond69.backedge
                                        #   in Loop: Header=BB7_27 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_30
.LBB7_32:                               # %do.body89
	movq	104(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_35
# BB#33:                                # %land.lhs.true92
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_35
# BB#34:                                # %if.then95
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_35:                               # %do.body99
	movq	112(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_38
# BB#36:                                # %land.lhs.true102
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_38
# BB#37:                                # %if.then105
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_38:                               # %do.body109
	movq	120(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_41
# BB#39:                                # %land.lhs.true112
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_41
# BB#40:                                # %if.then115
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_41:                               # %do.body119
	movq	128(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_44
# BB#42:                                # %land.lhs.true122
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_44
# BB#43:                                # %if.then125
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB7_44:                               # %do.body129
	movq	136(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB7_46
# BB#45:                                # %land.lhs.true132
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB7_47
.LBB7_46:                               # %do.end138
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_47:                               # %if.then135
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	ggc_mark_rtx_children  # TAILCALL
.Ltmp31:
	.size	mark_eh_status, .Ltmp31-mark_eh_status
	.cfi_endproc

	.align	16, 0x90
	.type	mark_eh_region,@function
mark_eh_region:                         # @mark_eh_region
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 32
.Ltmp39:
	.cfi_offset %rbx, -32
.Ltmp40:
	.cfi_offset %r14, -24
.Ltmp41:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB8_31
# BB#1:                                 # %if.end
	movl	40(%r15), %eax
	cmpq	$7, %rax
	ja	.LBB8_30
# BB#2:                                 # %if.end
	jmpq	*.LJTI8_0(,%rax,8)
.LBB8_3:                                # %do.body
	movq	48(%r15), %r14
	jmp	.LBB8_4
.LBB8_9:                                # %do.body13
	movq	72(%r15), %r14
	testq	%r14, %r14
	je	.LBB8_12
# BB#10:                                # %land.lhs.true16
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_12
# BB#11:                                # %if.then19
	movq	%r14, %rdi
	callq	ggc_mark_rtx_children
	jmp	.LBB8_12
.LBB8_24:                               # %do.body23
	movq	64(%r15), %r14
	testq	%r14, %r14
	je	.LBB8_29
# BB#25:                                # %land.lhs.true27
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_29
# BB#26:                                # %do.body31
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB8_28
# BB#27:                                # %if.then35
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB8_28:                               # %if.end39
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB8_29:                               # %do.body48
	movq	72(%r15), %r14
.LBB8_4:                                # %do.body
	testq	%r14, %r14
	je	.LBB8_12
# BB#5:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_12
# BB#6:                                 # %do.body3
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB8_8
# BB#7:                                 # %if.then5
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB8_8:                                # %if.end8
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB8_12:                               # %do.body154
	movq	80(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB8_15
# BB#13:                                # %land.lhs.true157
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_15
# BB#14:                                # %if.then160
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB8_15:                               # %do.body163
	movq	104(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB8_18
# BB#16:                                # %land.lhs.true166
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_18
# BB#17:                                # %if.then169
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB8_18:                               # %do.body172
	movq	88(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB8_21
# BB#19:                                # %land.lhs.true175
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_21
# BB#20:                                # %if.then178
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB8_21:                               # %do.body181
	movq	96(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB8_31
# BB#22:                                # %land.lhs.true184
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_23
.LBB8_31:                               # %do.end189
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB8_23:                               # %if.then187
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	ggc_mark_rtx_children  # TAILCALL
.LBB8_30:                               # %sw.default
	movabsq	$.L.str26, %rdi
	movabsq	$.L__FUNCTION__.mark_eh_region, %rdx
	movq	$531, %rsi              # imm = 0x213
	callq	fancy_abort
.Ltmp42:
	.size	mark_eh_region, .Ltmp42-mark_eh_region
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_12
	.quad	.LBB8_3
	.quad	.LBB8_9
	.quad	.LBB8_24
	.quad	.LBB8_3
	.quad	.LBB8_12
	.quad	.LBB8_3
	.quad	.LBB8_3

	.text
	.globl	free_eh_status
	.align	16, 0x90
	.type	free_eh_status,@function
free_eh_status:                         # @free_eh_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp50:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp51:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp52:
	.cfi_def_cfa_offset 48
.Ltmp53:
	.cfi_offset %rbx, -40
.Ltmp54:
	.cfi_offset %r12, -32
.Ltmp55:
	.cfi_offset %r14, -24
.Ltmp56:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %r15
	movq	8(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_19
# BB#1:                                 # %if.then
	movslq	60(%r15), %rbx
	testq	%rbx, %rbx
	jle	.LBB9_7
# BB#2:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB9_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB9_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_3 Depth=1
	movl	24(%rdi), %eax
	movq	%rbx, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB9_6
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB9_3 Depth=1
	callq	free
.LBB9_6:                                # %for.inc
                                        #   in Loop: Header=BB9_3 Depth=1
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	8(%r15), %rdi
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_3
.LBB9_7:                                # %for.end
	callq	free
	jmp	.LBB9_8
.LBB9_19:                               # %if.else
	movq	(%r15), %r12
	testq	%r12, %r12
	jne	.LBB9_20
	jmp	.LBB9_8
	.align	16, 0x90
.LBB9_22:                               # %if.then16
                                        #   in Loop: Header=BB9_20 Depth=1
	callq	free
.LBB9_20:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_21 Depth 2
	movq	%r12, %rbx
	movq	8(%rbx), %r12
	testq	%r12, %r12
	jne	.LBB9_20
.LBB9_21:                               # %if.else14
                                        #   Parent Loop BB9_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rbx), %r12
	movq	%rbx, %rdi
	testq	%r12, %r12
	jne	.LBB9_22
# BB#23:                                # %do.body
                                        #   in Loop: Header=BB9_21 Depth=2
	movq	(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	jne	.LBB9_21
.LBB9_8:                                # %do.body29
	movq	64(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_10
# BB#9:                                 # %if.then31
	callq	free
	movq	$0, 64(%r15)
.LBB9_10:                               # %do.body37
	movq	72(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_12
# BB#11:                                # %if.then39
	callq	free
	movq	$0, 72(%r15)
.LBB9_12:                               # %do.body45
	movq	80(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_14
# BB#13:                                # %if.then47
	callq	free
	movq	$0, 80(%r15)
.LBB9_14:                               # %do.end52
	movq	88(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_16
# BB#15:                                # %if.then54
	callq	free
.LBB9_16:                               # %if.end56
	movq	%r15, %rdi
	callq	free
	movq	$0, (%r14)
	movq	exception_handler_label_map(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB9_18
# BB#17:                                # %if.then59
	callq	htab_delete
	movq	$0, exception_handler_label_map(%rip)
.LBB9_18:                               # %if.end60
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp57:
	.size	free_eh_status, .Ltmp57-free_eh_status
	.cfi_endproc

	.globl	expand_eh_region_start
	.align	16, 0x90
	.type	expand_eh_region_start,@function
expand_eh_region_start:                 # @expand_eh_region_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp60:
	.cfi_def_cfa_offset 16
.Ltmp61:
	.cfi_offset %rbx, -16
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_5
# BB#1:                                 # %if.end
	movq	$1, %rdi
	movq	$112, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rdx
	movq	%rax, %rbx
	testq	%rdx, %rdx
	movq	%rdx, (%rbx)
	je	.LBB10_3
# BB#2:                                 # %if.then4
	movq	8(%rdx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, 8(%rdx)
	jmp	.LBB10_4
.LBB10_3:                               # %if.else
	movq	(%rcx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%rcx)
.LBB10_4:                               # %if.end10
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	%rbx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	60(%rax), %ecx
	incq	%rcx
	movl	%ecx, 60(%rax)
	movl	%ecx, 24(%rbx)
	movabsq	$4294967210, %rsi       # imm = 0xFFFFFFAA
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
.LBB10_5:                               # %return
	popq	%rbx
	retq
.Ltmp62:
	.size	expand_eh_region_start, .Ltmp62-expand_eh_region_start
	.cfi_endproc

	.globl	expand_eh_region_end_cleanup
	.align	16, 0x90
	.type	expand_eh_region_end_cleanup,@function
expand_eh_region_end_cleanup:           # @expand_eh_region_end_cleanup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp69:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp70:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp71:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp72:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp74:
	.cfi_def_cfa_offset 80
.Ltmp75:
	.cfi_offset %rbx, -48
.Ltmp76:
	.cfi_offset %r12, -40
.Ltmp77:
	.cfi_offset %r13, -32
.Ltmp78:
	.cfi_offset %r14, -24
.Ltmp79:
	.cfi_offset %r15, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB11_16
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %r14
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	xorq	%r15, %r15
	callq	emit_note
	movl	24(%r14), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%r14), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	$1, %rax
	movl	%eax, 40(%r14)
	callq	gen_label_rtx
	movq	%rax, 80(%r14)
	movq	%rbx, 48(%r14)
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	emit_jump
	movq	80(%r14), %rdi
	callq	emit_label
	movq	lang_protect_cleanup_actions(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB11_9
# BB#2:                                 # %cond.end
	callq	*%rax
	movq	%rax, %r13
	xorq	%r15, %r15
	testq	%r13, %r13
	movabsq	$0, %rcx
	je	.LBB11_9
# BB#3:                                 # %if.then9
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	movq	$1, %r15
	je	.LBB11_8
# BB#4:                                 # %if.end.i
	movq	$1, %rdi
	movq	$112, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rdx
	movq	%rax, %rbx
	testq	%rdx, %rdx
	movq	%rdx, (%rbx)
	je	.LBB11_6
# BB#5:                                 # %if.then4.i
	movq	8(%rdx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, 8(%rdx)
	jmp	.LBB11_7
.LBB11_16:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_6:                               # %if.else.i
	movq	(%rcx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%rcx)
.LBB11_7:                               # %if.end10.i
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	%rbx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	60(%rax), %ecx
	incq	%rcx
	movl	%ecx, 60(%rax)
	movl	%ecx, 24(%rbx)
	movabsq	$4294967210, %rsi       # imm = 0xFFFFFFAA
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
	movq	$1, %r15
.LBB11_8:                               # %if.end10
	movq	%r13, %rcx
.LBB11_9:                               # %if.end10
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	cfun(%rip), %rbx
	movq	(%rbx), %rcx
	movq	48(%rcx), %rsi
	movq	%rax, %r13
	testq	%rsi, %rsi
	jne	.LBB11_11
# BB#10:                                # %if.then.i54
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	(%rbx), %rcx
	movq	%rax, %rsi
	movq	%rsi, 48(%rcx)
.LBB11_11:                              # %get_exception_pointer.exit
	movq	%r13, %rdi
	callq	emit_move_insn
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movq	cfun(%rip), %r12
	movq	(%r12), %rcx
	movq	40(%rcx), %rsi
	movq	%rax, %rbx
	testq	%rsi, %rsi
	jne	.LBB11_13
# BB#12:                                # %if.then.i61
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movq	(%r12), %rcx
	movq	%rax, %rsi
	movq	%rsi, 40(%rcx)
.LBB11_13:                              # %get_exception_filter.exit
	movq	%rbx, %rdi
	callq	emit_move_insn
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	expand_expr
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	48(%rax), %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	40(%rax), %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	testq	%r15, %r15
	je	.LBB11_15
# BB#14:                                # %if.then29
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	expand_eh_region_end_must_not_throw
.LBB11_15:                              # %if.end30
	callq	do_pending_stack_adjust
	movl	24(%r14), %edx
	movq	$53, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_i
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%rax, 104(%r14)
	callq	emit_barrier
	movq	16(%rsp), %rdi          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.Ltmp80:
	.size	expand_eh_region_end_cleanup, .Ltmp80-expand_eh_region_end_cleanup
	.cfi_endproc

	.globl	get_exception_pointer
	.align	16, 0x90
	.type	get_exception_pointer,@function
get_exception_pointer:                  # @get_exception_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 16
.Ltmp84:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	cfun(%rip), %rcx
	movq	48(%rax), %rax
	cmpq	%rbx, %rcx
	jne	.LBB12_3
# BB#1:                                 # %entry
	testq	%rax, %rax
	jne	.LBB12_3
# BB#2:                                 # %if.then
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	(%rbx), %rcx
	movq	%rax, 48(%rcx)
.LBB12_3:                               # %if.end
	popq	%rbx
	retq
.Ltmp85:
	.size	get_exception_pointer, .Ltmp85-get_exception_pointer
	.cfi_endproc

	.globl	expand_eh_region_end_must_not_throw
	.align	16, 0x90
	.type	expand_eh_region_end_must_not_throw,@function
expand_eh_region_end_must_not_throw:    # @expand_eh_region_end_must_not_throw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp90:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp91:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 32
.Ltmp93:
	.cfi_offset %rbx, -32
.Ltmp94:
	.cfi_offset %r14, -24
.Ltmp95:
	.cfi_offset %r15, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB13_1
# BB#2:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rbx
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%rbx), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	$5, %rax
	movl	%eax, 40(%rbx)
	callq	gen_label_rtx
	movq	%rax, 80(%rbx)
	callq	gen_label_rtx
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	emit_jump
	movq	80(%rbx), %rdi
	callq	emit_label
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movq	%r15, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB13_1:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp96:
	.size	expand_eh_region_end_must_not_throw, .Ltmp96-expand_eh_region_end_must_not_throw
	.cfi_endproc

	.globl	expand_start_all_catch
	.align	16, 0x90
	.type	expand_start_all_catch,@function
expand_start_all_catch:                 # @expand_start_all_catch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp99:
	.cfi_def_cfa_offset 16
.Ltmp100:
	.cfi_offset %rbx, -16
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB14_1
# BB#4:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rbx
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%rbx), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	$2, %rax
	movl	%eax, 40(%rbx)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	24(%rax), %rax
	movq	%rax, 64(%rbx)
	callq	gen_label_rtx
	movq	%rax, 72(%rbx)
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rbx, 24(%rcx)
	movq	%rax, %rdi
	popq	%rbx
	jmp	emit_jump  # TAILCALL
.LBB14_1:                               # %if.then.i
	movzbl	doing_eh.warned(%rip), %eax
	testq	%rax, %rax
	jne	.LBB14_3
# BB#2:                                 # %if.then3.i
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	error
	movq	$1, %rax
	movb	%al, doing_eh.warned(%rip)
.LBB14_3:                               # %return
	popq	%rbx
	retq
.Ltmp101:
	.size	expand_start_all_catch, .Ltmp101-expand_start_all_catch
	.cfi_endproc

	.globl	expand_start_catch
	.align	16, 0x90
	.type	expand_start_catch,@function
expand_start_catch:                     # @expand_start_catch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp107:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp108:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp109:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp110:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp111:
	.cfi_def_cfa_offset 48
.Ltmp112:
	.cfi_offset %rbx, -40
.Ltmp113:
	.cfi_offset %r12, -32
.Ltmp114:
	.cfi_offset %r14, -24
.Ltmp115:
	.cfi_offset %r15, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB15_18
# BB#1:                                 # %if.end
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB15_10
# BB#2:                                 # %if.then2
	movzbl	16(%r14), %eax
	cmpq	$2, %rax
	je	.LBB15_4
# BB#3:                                 # %if.end5
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	%r14, %rsi
	callq	tree_cons
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB15_9
.LBB15_4:                               # %for.body.lr.ph
	movq	%r14, %rbx
	.align	16, 0x90
.LBB15_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %r12
	movq	type_to_runtime_map(%rip), %rdi
	movq	%r12, %rdx
	andq	$262143, %rdx           # imm = 0x3FFFF
	movq	$1, %rcx
	movq	%r12, %rsi
	callq	htab_find_slot_with_hash
	movq	%rax, %r15
	cmpq	$0, (%r15)
	jne	.LBB15_7
# BB#6:                                 # %if.then.i44
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	%r12, %rdi
	callq	*lang_eh_runtime_type(%rip)
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, (%r15)
.LBB15_7:                               # %add_type_for_runtime.exit
                                        #   in Loop: Header=BB15_5 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_5
# BB#8:
	movq	%r14, %r15
.LBB15_9:                               # %if.end8
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_14
.LBB15_10:                              # %if.end.i
	movq	$1, %rdi
	movq	$112, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rdx
	movq	%rax, %rbx
	testq	%rdx, %rdx
	movq	%rdx, (%rbx)
	je	.LBB15_12
# BB#11:                                # %if.then4.i
	movq	8(%rdx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, 8(%rdx)
	jmp	.LBB15_13
.LBB15_18:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB15_12:                              # %if.else.i
	movq	(%rcx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%rcx)
.LBB15_13:                              # %if.end10.i
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	%rbx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	60(%rax), %ecx
	incq	%rcx
	movl	%ecx, 60(%rax)
	movl	%ecx, 24(%rbx)
	movabsq	$4294967210, %rsi       # imm = 0xFFFFFFAA
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
.LBB15_14:                              # %expand_eh_region_start.exit
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rbx
	movq	24(%rax), %r14
	movq	$3, %rax
	movl	%eax, 40(%rbx)
	movq	%r15, 64(%rbx)
	callq	gen_label_rtx
	movq	%rax, 80(%rbx)
	movq	56(%r14), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 56(%rbx)
	je	.LBB15_16
# BB#15:                                # %if.then16
	movq	%rbx, 48(%rcx)
	jmp	.LBB15_17
.LBB15_16:                              # %if.else
	movq	%rbx, 48(%r14)
.LBB15_17:                              # %if.end22
	movq	%rbx, 56(%r14)
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.Ltmp116:
	.size	expand_start_catch, .Ltmp116-expand_start_catch
	.cfi_endproc

	.globl	expand_end_catch
	.align	16, 0x90
	.type	expand_end_catch,@function
expand_end_catch:                       # @expand_end_catch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp119:
	.cfi_def_cfa_offset 16
.Ltmp120:
	.cfi_offset %rbx, -16
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_1
# BB#2:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rbx
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%rbx), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	24(%rax), %rax
	movq	72(%rax), %rdi
	popq	%rbx
	jmp	emit_jump  # TAILCALL
.LBB16_1:                               # %return
	popq	%rbx
	retq
.Ltmp121:
	.size	expand_end_catch, .Ltmp121-expand_end_catch
	.cfi_endproc

	.globl	expand_end_all_catch
	.align	16, 0x90
	.type	expand_end_all_catch,@function
expand_end_all_catch:                   # @expand_end_all_catch
	.cfi_startproc
# BB#0:                                 # %entry
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_1
# BB#2:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	24(%rax), %rcx
	movq	64(%rcx), %rdx
	movq	%rdx, 24(%rax)
	movq	72(%rcx), %rdi
	jmp	emit_label  # TAILCALL
.LBB17_1:                               # %return
	retq
.Ltmp122:
	.size	expand_end_all_catch, .Ltmp122-expand_end_all_catch
	.cfi_endproc

	.globl	expand_eh_region_end_allowed
	.align	16, 0x90
	.type	expand_eh_region_end_allowed,@function
expand_eh_region_end_allowed:           # @expand_eh_region_end_allowed
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp129:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp130:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp131:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp132:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp133:
	.cfi_def_cfa_offset 48
.Ltmp134:
	.cfi_offset %rbx, -48
.Ltmp135:
	.cfi_offset %r12, -40
.Ltmp136:
	.cfi_offset %r13, -32
.Ltmp137:
	.cfi_offset %r14, -24
.Ltmp138:
	.cfi_offset %r15, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB18_6
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %r13
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%r13), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%r13), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	$4, %rax
	movl	%eax, 40(%r13)
	movq	%rbx, 48(%r13)
	callq	gen_label_rtx
	testq	%rbx, %rbx
	movq	%rax, 80(%r13)
	je	.LBB18_5
	.align	16, 0x90
.LBB18_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %r15
	movq	type_to_runtime_map(%rip), %rdi
	movq	%r15, %rdx
	andq	$262143, %rdx           # imm = 0x3FFFF
	movq	$1, %rcx
	movq	%r15, %rsi
	callq	htab_find_slot_with_hash
	movq	%rax, %r12
	cmpq	$0, (%r12)
	jne	.LBB18_4
# BB#3:                                 # %if.then.i17
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	%r15, %rdi
	callq	*lang_eh_runtime_type(%rip)
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, (%r12)
.LBB18_4:                               # %add_type_for_runtime.exit
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB18_2
.LBB18_5:                               # %for.end
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	emit_jump
	movq	80(%r13), %rdi
	callq	emit_label
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB18_6:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp139:
	.size	expand_eh_region_end_allowed, .Ltmp139-expand_eh_region_end_allowed
	.cfi_endproc

	.globl	expand_eh_region_end_throw
	.align	16, 0x90
	.type	expand_eh_region_end_throw,@function
expand_eh_region_end_throw:             # @expand_eh_region_end_throw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp143:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp144:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp145:
	.cfi_def_cfa_offset 32
.Ltmp146:
	.cfi_offset %rbx, -24
.Ltmp147:
	.cfi_offset %r14, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB19_2
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rbx
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%rbx), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	$6, %rax
	movl	%eax, 40(%rbx)
	movq	%r14, 48(%rbx)
.LBB19_2:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp148:
	.size	expand_eh_region_end_throw, .Ltmp148-expand_eh_region_end_throw
	.cfi_endproc

	.globl	expand_eh_region_end_fixup
	.align	16, 0x90
	.type	expand_eh_region_end_fixup,@function
expand_eh_region_end_fixup:             # @expand_eh_region_end_fixup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp153:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp154:
	.cfi_def_cfa_offset 32
.Ltmp155:
	.cfi_offset %rbx, -24
.Ltmp156:
	.cfi_offset %r14, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB20_2
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	16(%rax), %rbx
	movabsq	$4294967211, %rsi       # imm = 0xFFFFFFAB
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	(%rbx), %rcx
	movq	(%rax), %rax
	movq	%rcx, 16(%rax)
	movq	$7, %rax
	movl	%eax, 40(%rbx)
	movq	%r14, 48(%rbx)
.LBB20_2:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp157:
	.size	expand_eh_region_end_fixup, .Ltmp157-expand_eh_region_end_fixup
	.cfi_endproc

	.globl	begin_protect_partials
	.align	16, 0x90
	.type	begin_protect_partials,@function
begin_protect_partials:                 # @begin_protect_partials
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp159:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	32(%rax), %rdx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	tree_cons
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 32(%rcx)
	popq	%rax
	retq
.Ltmp160:
	.size	begin_protect_partials, .Ltmp160-begin_protect_partials
	.cfi_endproc

	.globl	add_partial_entry
	.align	16, 0x90
	.type	add_partial_entry,@function
add_partial_entry:                      # @add_partial_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp164:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp165:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp166:
	.cfi_def_cfa_offset 32
.Ltmp167:
	.cfi_offset %rbx, -24
.Ltmp168:
	.cfi_offset %r14, -16
	movl	flag_exceptions(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB22_5
# BB#1:                                 # %if.end.i
	movq	$1, %rdi
	movq	$112, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	16(%rcx), %rdx
	movq	%rax, %rbx
	testq	%rdx, %rdx
	movq	%rdx, (%rbx)
	je	.LBB22_3
# BB#2:                                 # %if.then4.i
	movq	8(%rdx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, 8(%rdx)
	jmp	.LBB22_4
.LBB22_3:                               # %if.else.i
	movq	(%rcx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%rcx)
.LBB22_4:                               # %if.end10.i
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	%rbx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	60(%rax), %ecx
	incq	%rcx
	movl	%ecx, 60(%rax)
	movl	%ecx, 24(%rbx)
	movabsq	$4294967210, %rsi       # imm = 0xFFFFFFAA
	xorq	%rdi, %rdi
	callq	emit_note
	movl	24(%rbx), %ecx
	movl	%ecx, 32(%rax)
.LBB22_5:                               # %expand_eh_region_start.exit
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	32(%rax), %rax
	movq	32(%rax), %rdx
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%rax, 32(%rcx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp169:
	.size	add_partial_entry, .Ltmp169-add_partial_entry
	.cfi_endproc

	.globl	end_protect_partials
	.align	16, 0x90
	.type	end_protect_partials,@function
end_protect_partials:                   # @end_protect_partials
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp172:
	.cfi_def_cfa_offset 16
.Ltmp173:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	32(%rax), %rcx
	movq	(%rcx), %rdx
	movq	32(%rcx), %rbx
	testq	%rbx, %rbx
	movq	%rdx, 32(%rax)
	je	.LBB23_2
	.align	16, 0x90
.LBB23_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	callq	expand_eh_region_end_cleanup
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB23_1
.LBB23_2:                               # %for.end
	popq	%rbx
	retq
.Ltmp174:
	.size	end_protect_partials, .Ltmp174-end_protect_partials
	.cfi_endproc

	.globl	convert_from_eh_region_ranges
	.align	16, 0x90
	.type	convert_from_eh_region_ranges,@function
convert_from_eh_region_ranges:          # @convert_from_eh_region_ranges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp180:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp181:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp182:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp183:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp184:
	.cfi_def_cfa_offset 48
.Ltmp185:
	.cfi_offset %rbx, -40
.Ltmp186:
	.cfi_offset %r12, -32
.Ltmp187:
	.cfi_offset %r14, -24
.Ltmp188:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB24_6
# BB#1:                                 # %if.end.i
	movl	60(%rcx), %edi
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 8(%rcx)
	.align	16, 0x90
.LBB24_2:                               # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_3 Depth 2
	movq	%rbx, %rcx
	movslq	24(%rcx), %rdx
	movq	%rcx, (%rax,%rdx,8)
	movq	8(%rcx), %rbx
	testq	%rbx, %rbx
	jne	.LBB24_2
.LBB24_3:                               # %if.else.i
                                        #   Parent Loop BB24_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rcx), %rbx
	testq	%rbx, %rbx
	jne	.LBB24_2
# BB#4:                                 # %do.body.i
                                        #   in Loop: Header=BB24_3 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB24_3
# BB#5:                                 # %collect_eh_region_array.exit.loopexit
	movq	cfun(%rip), %rax
	movq	(%rax), %rcx
.LBB24_6:                               # %collect_eh_region_array.exit
	movslq	60(%rcx), %rdi
	testq	%rdi, %rdi
	jle	.LBB24_19
# BB#7:
	movq	$1, %r8
	jmp	.LBB24_8
	.align	16, 0x90
.LBB24_17:                              # %for.inc23.for.body_crit_edge.i
                                        #   in Loop: Header=BB24_8 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rcx
	incq	%r8
.LBB24_8:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_11 Depth 2
	movq	8(%rcx), %rsi
	movq	(%rsi,%r8,8), %r9
	testq	%r9, %r9
	je	.LBB24_16
# BB#9:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB24_8 Depth=1
	movl	40(%r9), %ecx
	cmpq	$7, %rcx
	jne	.LBB24_16
# BB#10:                                # %for.body5.lr.ph.i
                                        #   in Loop: Header=BB24_8 Depth=1
	movq	$1, %rbx
	.align	16, 0x90
.LBB24_11:                              # %for.body5.i
                                        #   Parent Loop BB24_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rsi,%rbx,8), %rcx
	movl	40(%rcx), %eax
	cmpq	$1, %rax
	jne	.LBB24_13
# BB#12:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB24_11 Depth=2
	movq	48(%rcx), %rax
	movq	48(%r9), %rdx
	cmpq	%rdx, %rax
	je	.LBB24_15
.LBB24_13:                              # %for.inc.i
                                        #   in Loop: Header=BB24_11 Depth=2
	movq	$32, %rcx
	leaq	1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdi, %rbx
	movq	%rax, %rbx
	jl	.LBB24_11
	jmp	.LBB24_14
	.align	16, 0x90
.LBB24_15:                              # %if.end20.i
                                        #   in Loop: Header=BB24_8 Depth=1
	movq	(%rcx), %rax
	movq	%rax, 56(%r9)
.LBB24_16:                              # %for.inc23.i
                                        #   in Loop: Header=BB24_8 Depth=1
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jl	.LBB24_17
# BB#18:                                # %resolve_fixup_regions.exit.loopexit
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	60(%rax), %edi
.LBB24_19:                              # %resolve_fixup_regions.exit
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	callq	get_insns
	movq	%rax, (%rsp)
	leaq	(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	convert_from_eh_region_ranges_1
	movq	%rbx, %rdi
	callq	free
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB24_20
	.align	16, 0x90
.LBB24_29:                              # %for.inc.i22
                                        #   in Loop: Header=BB24_20 Depth=1
	movq	24(%rbx), %rbx
.LBB24_20:                              # %resolve_fixup_regions.exit
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB24_30
# BB#21:                                # %for.body.i13
                                        #   in Loop: Header=BB24_20 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB24_29
# BB#22:                                # %land.lhs.true.i15
                                        #   in Loop: Header=BB24_20 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB24_29
# BB#23:                                # %land.lhs.true4.i
                                        #   in Loop: Header=BB24_20 Depth=1
	movq	8(%r14), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	jle	.LBB24_29
# BB#24:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB24_20 Depth=1
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB24_29
# BB#25:                                # %land.lhs.true19.i
                                        #   in Loop: Header=BB24_20 Depth=1
	movl	40(%rax), %ecx
	cmpq	$7, %rcx
	jne	.LBB24_29
# BB#26:                                # %if.then.i
                                        #   in Loop: Header=BB24_20 Depth=1
	movq	56(%rax), %rax
	testq	%rax, %rax
	jne	.LBB24_27
# BB#28:                                # %if.else.i20
                                        #   in Loop: Header=BB24_20 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	remove_note
	jmp	.LBB24_29
.LBB24_27:                              # %if.then24.i
                                        #   in Loop: Header=BB24_20 Depth=1
	movslq	24(%rax), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%r14)
	movq	24(%rbx), %rbx
	jmp	.LBB24_20
.LBB24_30:                              # %for.end.i
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	60(%rax), %rbx
	testq	%rbx, %rbx
	jg	.LBB24_31
	jmp	.LBB24_45
	.align	16, 0x90
.LBB24_44:                              # %for.inc83.for.body41_crit_edge.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
.LBB24_31:                              # %for.body41.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_37 Depth 2
	movq	8(%rax), %rax
	movq	(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB24_43
# BB#32:                                # %if.end48.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movl	40(%rdi), %eax
	cmpq	$7, %rax
	jne	.LBB24_33
# BB#35:                                # %if.end59.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movq	8(%rdi), %rax
	testq	%rax, %rax
	je	.LBB24_42
# BB#36:                                # %if.then61.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movq	56(%rdi), %rcx
	movq	%rax, %rsi
	.align	16, 0x90
.LBB24_37:                              # %for.cond66.i
                                        #   Parent Loop BB24_31 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rdx
	movq	%rcx, (%rdx)
	movq	16(%rdx), %rsi
	testq	%rsi, %rsi
	jne	.LBB24_37
# BB#38:                                # %for.end72.i
                                        #   in Loop: Header=BB24_31 Depth=1
	testq	%rcx, %rcx
	jne	.LBB24_39
# BB#40:                                # %if.else76.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	jmp	.LBB24_41
	.align	16, 0x90
.LBB24_33:                              # %if.end48.i
                                        #   in Loop: Header=BB24_31 Depth=1
	cmpq	$1, %rax
	jne	.LBB24_43
# BB#34:                                # %if.end54.thread.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movq	$0, 48(%rdi)
	jmp	.LBB24_43
.LBB24_39:                              # %if.then74.i
                                        #   in Loop: Header=BB24_31 Depth=1
	addq	$8, %rcx
.LBB24_41:                              # %if.end78.i
                                        #   in Loop: Header=BB24_31 Depth=1
	movq	(%rcx), %rsi
	movq	%rsi, 16(%rdx)
	movq	%rax, (%rcx)
	movq	$0, 8(%rdi)
.LBB24_42:                              # %if.end82.i
                                        #   in Loop: Header=BB24_31 Depth=1
	callq	remove_eh_handler
.LBB24_43:                              # %for.inc83.i
                                        #   in Loop: Header=BB24_31 Depth=1
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB24_44
.LBB24_45:                              # %remove_fixup_regions.exit
	movq	(%rsp), %rbx
	callq	get_max_uid
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rsi
	movq	%rax, %rdi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movl	60(%rcx), %edi
	movq	%rax, %r15
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rdx
	movslq	60(%rdx), %r12
	movq	%rax, %r14
	testq	%r12, %r12
	jle	.LBB24_53
# BB#46:                                # %for.body.lr.ph.i
	movq	8(%rdx), %r10
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r12, %rdi
	.align	16, 0x90
.LBB24_47:                              # %for.body.i39
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r10,%rdi,8), %rsi
	testq	%rsi, %rsi
	je	.LBB24_77
# BB#48:                                # %lor.lhs.false.i42
                                        #   in Loop: Header=BB24_47 Depth=1
	movl	24(%rsi), %r9d
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %r9
	jne	.LBB24_77
# BB#49:                                # %if.end.i43
                                        #   in Loop: Header=BB24_47 Depth=1
	movq	104(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB24_68
# BB#50:                                # %if.then11.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movslq	8(%rcx), %rcx
	movl	(%r15,%rcx,4), %eax
	testq	%rax, %rax
	jne	.LBB24_51
# BB#67:                                # %if.end18.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movl	%r9d, (%r15,%rcx,4)
.LBB24_68:                              # %if.end25.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movq	80(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB24_72
# BB#69:                                # %if.then27.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movslq	8(%rcx), %rcx
	movl	(%r15,%rcx,4), %eax
	testq	%rax, %rax
	jne	.LBB24_70
# BB#71:                                # %if.end36.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movl	%r9d, (%r15,%rcx,4)
.LBB24_72:                              # %if.end43.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movl	40(%rsi), %eax
	cmpq	$2, %rax
	jne	.LBB24_77
# BB#73:                                # %land.lhs.true.i46
                                        #   in Loop: Header=BB24_47 Depth=1
	movq	72(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB24_77
# BB#74:                                # %if.then47.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movslq	8(%rcx), %rcx
	movl	(%r15,%rcx,4), %eax
	testq	%rax, %rax
	jne	.LBB24_75
# BB#76:                                # %if.end58.i
                                        #   in Loop: Header=BB24_47 Depth=1
	movl	%r9d, (%r15,%rcx,4)
	.align	16, 0x90
.LBB24_77:                              # %for.inc.i48
                                        #   in Loop: Header=BB24_47 Depth=1
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB24_47
.LBB24_53:                              # %for.cond68.preheader.i
	testq	%rbx, %rbx
	je	.LBB24_56
# BB#54:
	movq	$1, %rax
	.align	16, 0x90
.LBB24_55:                              # %for.body70.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	8(%rbx), %rcx
	movslq	(%r15,%rcx,4), %rcx
	movb	%al, (%r14,%rcx)
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB24_55
.LBB24_56:                              # %for.end81.i
	testq	%r12, %r12
	jle	.LBB24_78
# BB#57:                                # %for.body87.lr.ph.i
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB24_58
	.align	16, 0x90
.LBB24_66:                              # %for.inc117.for.body87_crit_edge.i
                                        #   in Loop: Header=BB24_58 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rdx
.LBB24_58:                              # %for.body87.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rax
	movq	(%rax,%r12,8), %rdi
	testq	%rdi, %rdi
	je	.LBB24_65
# BB#59:                                # %land.lhs.true93.i
                                        #   in Loop: Header=BB24_58 Depth=1
	movl	24(%rdi), %eax
	movq	%r12, %rcx
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB24_65
# BB#60:                                # %land.lhs.true97.i
                                        #   in Loop: Header=BB24_58 Depth=1
	movzbl	(%r14,%r12), %eax
	testq	%rax, %rax
	jne	.LBB24_65
# BB#61:                                # %if.then101.i
                                        #   in Loop: Header=BB24_58 Depth=1
	movl	40(%rdi), %eax
	cmpq	$6, %rax
	jne	.LBB24_64
# BB#62:                                # %land.lhs.true105.i
                                        #   in Loop: Header=BB24_58 Depth=1
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB24_64
# BB#63:                                # %land.lhs.true107.i
                                        #   in Loop: Header=BB24_58 Depth=1
	movslq	24(%rax), %rax
	movzbl	(%r14,%rax), %eax
	testq	%rax, %rax
	jne	.LBB24_65
.LBB24_64:                              # %if.end115.i
                                        #   in Loop: Header=BB24_58 Depth=1
	callq	remove_eh_handler
	.align	16, 0x90
.LBB24_65:                              # %for.inc117.i
                                        #   in Loop: Header=BB24_58 Depth=1
	decq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB24_66
.LBB24_78:                              # %remove_unreachable_regions.exit
	movq	%r14, %rdi
	callq	free
	movq	%r15, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB24_14:                              # %if.then19.i
	movabsq	$.L.str26, %rdi
	movabsq	$.L__FUNCTION__.resolve_fixup_regions, %rdx
	movq	$1180, %rsi             # imm = 0x49C
	callq	fancy_abort
.LBB24_51:                              # %if.then17.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.remove_unreachable_regions, %rcx
	movq	$1280, %rsi             # imm = 0x500
	jmp	.LBB24_52
.LBB24_70:                              # %if.then35.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.remove_unreachable_regions, %rcx
	movq	$1286, %rsi             # imm = 0x506
	jmp	.LBB24_52
.LBB24_75:                              # %if.then57.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.remove_unreachable_regions, %rcx
	movq	$1292, %rsi             # imm = 0x50C
.LBB24_52:                              # %if.then17.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp189:
	.size	convert_from_eh_region_ranges, .Ltmp189-convert_from_eh_region_ranges
	.cfi_endproc

	.align	16, 0x90
	.type	convert_from_eh_region_ranges_1,@function
convert_from_eh_region_ranges_1:        # @convert_from_eh_region_ranges_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp196:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp197:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp198:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp199:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp201:
	.cfi_def_cfa_offset 80
.Ltmp202:
	.cfi_offset %rbx, -48
.Ltmp203:
	.cfi_offset %r12, -40
.Ltmp204:
	.cfi_offset %r13, -32
.Ltmp205:
	.cfi_offset %r14, -24
.Ltmp206:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	(%rdi), %r15
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	je	.LBB25_30
# BB#1:
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	movabsq	$4294967210, %r13       # imm = 0xFFFFFFAA
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rsi, %r14
.LBB25_2:                               # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_3 Depth 2
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB25_3:                               # %for.body
                                        #   Parent Loop BB25_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rbx
	movzbl	(%rbx), %eax
	movq	24(%rbx), %r15
	cmpq	$37, %rax
	jne	.LBB25_19
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB25_3 Depth=2
	movl	40(%rbx), %eax
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	%r13, %rcx
	jne	.LBB25_5
	jmp	.LBB25_6
	.align	16, 0x90
.LBB25_19:                              # %if.else32
                                        #   in Loop: Header=BB25_3 Depth=2
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB25_5
# BB#20:                                # %if.then39
                                        #   in Loop: Header=BB25_3 Depth=2
	movq	$32, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB25_27
# BB#21:                                # %land.lhs.true
                                        #   in Loop: Header=BB25_3 Depth=2
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r8, %r12
	callq	find_reg_note
	movq	%r12, %r8
	testq	%rax, %rax
	jne	.LBB25_27
# BB#22:                                # %land.lhs.true43
                                        #   in Loop: Header=BB25_3 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB25_26
# BB#23:                                # %lor.lhs.false48
                                        #   in Loop: Header=BB25_3 Depth=2
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB25_27
# BB#24:                                # %land.lhs.true50
                                        #   in Loop: Header=BB25_3 Depth=2
	movq	32(%rbx), %rdi
	movl	(%rdi), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	je	.LBB25_27
# BB#25:                                # %land.lhs.true66
                                        #   in Loop: Header=BB25_3 Depth=2
	movq	%r8, %r12
	callq	may_trap_p
	movq	%r12, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB25_27
.LBB25_26:                              # %if.then72
                                        #   in Loop: Header=BB25_3 Depth=2
	xorq	%rdi, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r12
	callq	gen_rtx_CONST_INT
	movq	56(%rbx), %rdx
	movq	$23, %rdi
	movq	%rax, %rsi
	callq	alloc_EXPR_LIST
	movq	%r12, %r8
	movq	%rax, 56(%rbx)
	.align	16, 0x90
.LBB25_27:                              # %if.end82
                                        #   in Loop: Header=BB25_3 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB25_5
# BB#28:                                # %land.lhs.true87
                                        #   in Loop: Header=BB25_3 Depth=2
	movq	32(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$141, %rax
	jne	.LBB25_5
# BB#29:                                # %if.then95
                                        #   in Loop: Header=BB25_3 Depth=2
	addq	$8, %rdi
	movq	%r14, %rsi
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdx
	movq	%r8, %r12
	callq	convert_from_eh_region_ranges_1
	movq	32(%rbx), %rdi
	addq	$16, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	convert_from_eh_region_ranges_1
	movq	32(%rbx), %rdi
	addq	$24, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	convert_from_eh_region_ranges_1
	movq	%r12, %r8
	movabsq	$4294967210, %r13       # imm = 0xFFFFFFAA
	.align	16, 0x90
.LBB25_5:                               # %for.cond.backedge
                                        #   in Loop: Header=BB25_3 Depth=2
	testq	%r15, %r15
	jne	.LBB25_3
	jmp	.LBB25_17
	.align	16, 0x90
.LBB25_6:                               # %if.then5
                                        #   in Loop: Header=BB25_2 Depth=1
	cmpq	%r13, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	jne	.LBB25_13
# BB#7:                                 # %if.then7
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	%eax, (%r14)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	32(%rbx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	8(%rax), %rax
	movq	(%rax,%rcx,8), %rax
	movl	40(%rax), %ecx
	addq	$4, %r14
	cmpq	$3, %rcx
	jne	.LBB25_8
# BB#12:                                # %if.then17
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	(%rax), %rax
	jmp	.LBB25_10
.LBB25_13:                              # %if.else25
                                        #   in Loop: Header=BB25_2 Depth=1
	movl	-4(%r14), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	addq	$-4, %r14
	jmp	.LBB25_14
.LBB25_8:                               # %if.then7
                                        #   in Loop: Header=BB25_2 Depth=1
	cmpq	$7, %rcx
	jne	.LBB25_14
# BB#9:                                 # %if.then13
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	56(%rax), %rax
.LBB25_10:                              # %if.then13
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB25_14
# BB#11:                                # %cond.true
                                        #   in Loop: Header=BB25_2 Depth=1
	movl	24(%rax), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB25_14:                              # %if.end27
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	%r8, %r12
	movq	(%rdx), %rax
	cmpq	%rax, %rbx
	jne	.LBB25_16
# BB#15:                                # %if.then29
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	%r15, (%rdx)
.LBB25_16:                              # %if.end30
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	%rbx, %rdi
	callq	remove_insn
	testq	%r15, %r15
	movq	%r12, %r8
	movabsq	$4294967210, %r9        # imm = 0xFFFFFFAA
	movq	%r9, %r13
	jne	.LBB25_2
.LBB25_17:                              # %for.end
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	%rax, %r14
	jne	.LBB25_18
.LBB25_30:                              # %if.end120
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_18:                              # %if.then119
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.convert_from_eh_region_ranges_1, %rcx
	movq	$1404, %rsi             # imm = 0x57C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp207:
	.size	convert_from_eh_region_ranges_1, .Ltmp207-convert_from_eh_region_ranges_1
	.cfi_endproc

	.globl	find_exception_handler_labels
	.align	16, 0x90
	.type	find_exception_handler_labels,@function
find_exception_handler_labels:          # @find_exception_handler_labels
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp214:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp215:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp216:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp217:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp218:
	.cfi_def_cfa_offset 48
.Ltmp219:
	.cfi_offset %rbx, -48
.Ltmp220:
	.cfi_offset %r12, -40
.Ltmp221:
	.cfi_offset %r13, -32
.Ltmp222:
	.cfi_offset %r14, -24
.Ltmp223:
	.cfi_offset %r15, -16
	movq	exception_handler_label_map(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB26_2
# BB#1:                                 # %if.then
	callq	htab_empty
	jmp	.LBB26_3
.LBB26_2:                               # %if.else
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	60(%rax), %eax
	leaq	(%rax,%rax,2), %rax
	movabsq	$2147483648, %rdi       # imm = 0x80000000
	andq	%rax, %rdi
	movq	$31, %rcx
	shrq	%cl, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$33, %rcx
	sarq	%cl, %rdi
	movabsq	$ehl_hash, %rsi
	movabsq	$ehl_eq, %rdx
	movabsq	$ehl_free, %rcx
	callq	htab_create
	movq	%rax, exception_handler_label_map(%rip)
.LBB26_3:                               # %if.end
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	cmpq	$0, (%rax)
	je	.LBB26_17
# BB#4:                                 # %if.end4
	movslq	60(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB26_17
# BB#5:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB26_6
	.align	16, 0x90
.LBB26_16:                              # %for.inc.for.body_crit_edge
                                        #   in Loop: Header=BB26_6 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
.LBB26_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movq	(%rcx,%rbx,8), %r12
	testq	%r12, %r12
	je	.LBB26_15
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB26_6 Depth=1
	movl	24(%r12), %ecx
	movq	%rbx, %rdx
	andq	%r15, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB26_15
# BB#8:                                 # %if.end14
                                        #   in Loop: Header=BB26_6 Depth=1
	movl	56(%rax), %eax
	testq	%rax, %rax
	jne	.LBB26_10
# BB#9:                                 #   in Loop: Header=BB26_6 Depth=1
	leaq	80(%r12), %rax
	jmp	.LBB26_11
.LBB26_10:                              # %select.mid
                                        #   in Loop: Header=BB26_6 Depth=1
	leaq	88(%r12), %rax
.LBB26_11:                              # %select.end
                                        #   in Loop: Header=BB26_6 Depth=1
	movq	(%rax), %r13
	testq	%r13, %r13
	je	.LBB26_15
# BB#12:                                # %if.then21
                                        #   in Loop: Header=BB26_6 Depth=1
	movl	(%r13), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r13)
	movq	$16, %rdi
	callq	xmalloc
	movq	exception_handler_label_map(%rip), %rdi
	movq	%rax, %r14
	movq	%r13, (%r14)
	movq	%r12, 8(%r14)
	movq	$1, %rdx
	movq	%r14, %rsi
	callq	htab_find_slot
	cmpq	$0, (%rax)
	je	.LBB26_14
# BB#13:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB26_6 Depth=1
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movl	56(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB26_18
.LBB26_14:                              # %add_ehl_entry.exit
                                        #   in Loop: Header=BB26_6 Depth=1
	movq	%r14, (%rax)
	.align	16, 0x90
.LBB26_15:                              # %for.inc
                                        #   in Loop: Header=BB26_6 Depth=1
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB26_16
.LBB26_17:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_18:                              # %if.then.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.add_ehl_entry, %rcx
	movq	$1446, %rsi             # imm = 0x5A6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp224:
	.size	find_exception_handler_labels, .Ltmp224-find_exception_handler_labels
	.cfi_endproc

	.align	16, 0x90
	.type	ehl_hash,@function
ehl_hash:                               # @ehl_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movl	48(%rax), %eax
	imulq	$-1640531527, %rax, %rax # imm = 0xFFFFFFFF9E3779B9
	retq
.Ltmp225:
	.size	ehl_hash, .Ltmp225-ehl_hash
	.cfi_endproc

	.align	16, 0x90
	.type	ehl_eq,@function
ehl_eq:                                 # @ehl_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movq	(%rsi), %rcx
	cmpq	%rcx, %rax
	jne	.LBB28_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB28_1:
	movabsq	$0, %rax
	retq
.Ltmp226:
	.size	ehl_eq, .Ltmp226-ehl_eq
	.cfi_endproc

	.align	16, 0x90
	.type	ehl_free,@function
ehl_free:                               # @ehl_free
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movl	(%rax), %ecx
	movabsq	$4026531839, %rdx       # imm = 0xEFFFFFFF
	andq	%rcx, %rdx
	movl	%edx, (%rax)
	jmp	free  # TAILCALL
.Ltmp227:
	.size	ehl_free, .Ltmp227-ehl_free
	.cfi_endproc

	.globl	current_function_has_exception_handlers
	.align	16, 0x90
	.type	current_function_has_exception_handlers,@function
current_function_has_exception_handlers: # @current_function_has_exception_handlers
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	60(%rax), %rdi
	testq	%rdi, %rdi
	jle	.LBB30_6
# BB#1:                                 # %for.body.lr.ph
	movq	8(%rax), %rsi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi,%rdi,8), %rax
	testq	%rax, %rax
	je	.LBB30_5
# BB#3:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB30_2 Depth=1
	movl	24(%rax), %ecx
	movq	%rdi, %rdx
	andq	%r8, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB30_5
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB30_2 Depth=1
	movl	40(%rax), %eax
	cmpq	$6, %rax
	movq	$1, %rax
	jne	.LBB30_7
.LBB30_5:                               # %for.inc
                                        #   in Loop: Header=BB30_2 Depth=1
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB30_2
.LBB30_6:
	xorq	%rax, %rax
.LBB30_7:                               # %return
	retq
.Ltmp228:
	.size	current_function_has_exception_handlers, .Ltmp228-current_function_has_exception_handlers
	.cfi_endproc

	.globl	duplicate_eh_regions
	.align	16, 0x90
	.type	duplicate_eh_regions,@function
duplicate_eh_regions:                   # @duplicate_eh_regions
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp235:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp236:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp237:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp238:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp239:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp240:
	.cfi_def_cfa_offset 80
.Ltmp241:
	.cfi_offset %rbx, -48
.Ltmp242:
	.cfi_offset %r12, -40
.Ltmp243:
	.cfi_offset %r13, -32
.Ltmp244:
	.cfi_offset %r14, -24
.Ltmp245:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	(%r12), %rax
	movl	60(%rax), %edx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB31_56
# BB#1:                                 # %if.end
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	1(%rdx), %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r13
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, %rdi
	testq	%rbx, %rbx
	jle	.LBB31_8
# BB#2:                                 # %for.body.lr.ph
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	$1, %r15
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%r13, %rsi
	.align	16, 0x90
.LBB31_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rax
	movq	8(%rax), %rax
	movq	(%rax,%r15,8), %r13
	testq	%r13, %r13
	je	.LBB31_20
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB31_3 Depth=1
	movl	24(%r13), %eax
	movq	%r15, %rcx
	andq	%r11, %rcx
	cmpq	%rcx, %rax
	movq	%r15, %rax
	jne	.LBB31_21
# BB#5:                                 # %if.end7
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	%rsi, %r14
	movq	$1, %rdi
	movq	$112, %rsi
	movq	%r11, %rbx
	callq	xcalloc
	movq	%rbx, %r11
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movl	24(%r13), %edx
	movl	60(%rcx), %ecx
	movq	%rax, %rbx
	addq	%rdx, %rcx
	movl	%ecx, 24(%rbx)
	movl	40(%r13), %eax
	movl	%eax, 40(%rbx)
	decq	%rax
	andq	%r11, %rax
	cmpq	$5, %rax
	ja	.LBB31_13
# BB#6:                                 # %if.end7
                                        #   in Loop: Header=BB31_3 Depth=1
	jmpq	*.LJTI31_0(,%rax,8)
.LBB31_9:                               # %sw.bb4.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	72(%r13), %rax
	testq	%rax, %rax
	je	.LBB31_15
# BB#10:                                # %if.then.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movl	48(%rax), %esi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	get_label_from_map
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%rax, 72(%rbx)
	jmp	.LBB31_15
.LBB31_14:                              # %sw.bb12.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	64(%r13), %rax
	movq	%rax, 64(%rbx)
	jmp	.LBB31_15
.LBB31_11:                              # %sw.bb17.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	48(%r13), %rax
	movq	%rax, 48(%rbx)
	.align	16, 0x90
.LBB31_15:                              # %sw.epilog.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	80(%r13), %rax
	testq	%rax, %rax
	je	.LBB31_17
# BB#16:                                # %if.then30.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movl	48(%rax), %esi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	get_label_from_map
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%rax, 80(%rbx)
.LBB31_17:                              # %if.end37.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	104(%r13), %rax
	testq	%rax, %rax
	movq	%r14, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	je	.LBB31_19
# BB#18:                                # %if.then39.i
                                        #   in Loop: Header=BB31_3 Depth=1
	movslq	8(%rax), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	48(%rcx), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	movq	%rax, 104(%rbx)
	je	.LBB31_57
.LBB31_19:                              # %duplicate_eh_region_1.exit
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	%rbx, (%rdx,%r15,8)
.LBB31_20:                              # %for.inc
                                        #   in Loop: Header=BB31_3 Depth=1
	movq	%r15, %rax
.LBB31_21:                              # %for.inc
                                        #   in Loop: Header=BB31_3 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB31_3
# BB#7:                                 # %for.cond11.preheader
	movq	$32, %rcx
	movq	%rsi, %rax
	movq	%rsi, %r13
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	jle	.LBB31_8
# BB#22:                                # %for.body14.lr.ph
	movq	(%r12), %r8
	movq	8(%r8), %r9
	movq	$1, %rsi
	movq	8(%rsp), %r10           # 8-byte Reload
	andq	%r11, %r10
	.align	16, 0x90
.LBB31_23:                              # %for.body14
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r9,%rsi,8), %rbx
	testq	%rbx, %rbx
	je	.LBB31_38
# BB#24:                                # %lor.lhs.false20
                                        #   in Loop: Header=BB31_23 Depth=1
	movl	24(%rbx), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rsi, %rcx
	andq	%r11, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB31_38
# BB#25:                                # %if.end25
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	(%rdi,%rax,8), %rcx
	movl	40(%rcx), %eax
	cmpq	$3, %rax
	jne	.LBB31_26
# BB#28:                                # %sw.bb15.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	48(%rbx), %rax
	testq	%rax, %rax
	je	.LBB31_30
# BB#29:                                # %if.then.i138
                                        #   in Loop: Header=BB31_23 Depth=1
	movslq	24(%rax), %rax
	movq	(%rdi,%rax,8), %rax
	movq	%rax, 48(%rcx)
.LBB31_30:                              # %if.end.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.LBB31_32
	jmp	.LBB31_31
.LBB31_26:                              # %if.end25
                                        #   in Loop: Header=BB31_23 Depth=1
	cmpq	$2, %rax
	jne	.LBB31_32
# BB#27:                                # %sw.bb.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	48(%rbx), %rax
	movslq	24(%rax), %rax
	movq	(%rdi,%rax,8), %rax
	movq	%rax, 48(%rcx)
	movq	56(%rbx), %rax
.LBB31_31:                              # %if.then30.i140
                                        #   in Loop: Header=BB31_23 Depth=1
	movslq	24(%rax), %rax
	movq	(%rdi,%rax,8), %rax
	movq	%rax, 56(%rcx)
.LBB31_32:                              # %sw.epilog.i141
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB31_34
# BB#33:                                # %if.then42.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movslq	24(%rax), %rax
	movq	(%rdi,%rax,8), %rax
	movq	%rax, (%rcx)
.LBB31_34:                              # %if.end48.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB31_36
# BB#35:                                # %if.then50.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movslq	24(%rax), %rax
	movq	(%rdi,%rax,8), %rax
	movq	%rax, 8(%rcx)
.LBB31_36:                              # %if.end56.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movq	16(%rbx), %rax
	testq	%rax, %rax
	je	.LBB31_38
# BB#37:                                # %if.then58.i
                                        #   in Loop: Header=BB31_23 Depth=1
	movslq	24(%rax), %rax
	movq	(%rdi,%rax,8), %rax
	movq	%rax, 16(%rcx)
	.align	16, 0x90
.LBB31_38:                              # %for.inc26
                                        #   in Loop: Header=BB31_23 Depth=1
	incq	%rsi
	movq	%rsi, %rax
	andq	%r11, %rax
	cmpq	%r10, %rax
	jne	.LBB31_23
	jmp	.LBB31_39
.LBB31_8:                               # %for.cond11.preheader.for.end28_crit_edge
	movq	(%r12), %r8
.LBB31_39:                              # %for.end28
	movq	cfun(%rip), %rax
	movq	(%r8), %rcx
	movq	(%rax), %rdx
	movslq	24(%rcx), %rcx
	movq	16(%rdx), %rax
	movq	(%rdi,%rcx,8), %rcx
	testq	%rax, %rax
	je	.LBB31_51
# BB#40:                                # %if.then35
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB31_44
# BB#41:
	movq	8(%rsp), %rbx           # 8-byte Reload
	.align	16, 0x90
.LBB31_42:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rsi
	movq	16(%rsi), %rdx
	testq	%rdx, %rdx
	jne	.LBB31_42
# BB#43:                                # %while.end
	movq	%rcx, 16(%rsi)
	jmp	.LBB31_45
.LBB31_51:                              # %if.else62
	movq	(%rdx), %rax
	testq	%rax, %rax
	je	.LBB31_54
	.align	16, 0x90
.LBB31_52:                              # %while.cond68
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movq	16(%rdx), %rax
	testq	%rax, %rax
	jne	.LBB31_52
# BB#53:                                # %while.end73
	movq	%rcx, 16(%rdx)
	jmp	.LBB31_55
.LBB31_44:                              # %if.else
	movq	%rcx, 8(%rax)
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB31_45:                              # %for.cond43.preheader
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB31_55
# BB#46:
	movq	$2, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rbx
	.align	16, 0x90
.LBB31_47:                              # %for.body46
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rdi,%rcx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB31_50
# BB#48:                                # %land.lhs.true
                                        #   in Loop: Header=BB31_47 Depth=1
	cmpq	$0, (%rsi)
	jne	.LBB31_50
# BB#49:                                # %if.then54
                                        #   in Loop: Header=BB31_47 Depth=1
	movq	%rax, (%rsi)
.LBB31_50:                              # %for.inc59
                                        #   in Loop: Header=BB31_47 Depth=1
	movq	%rcx, %rsi
	andq	%rdx, %rsi
	incq	%rcx
	cmpq	%rbx, %rsi
	jne	.LBB31_47
	jmp	.LBB31_55
.LBB31_54:                              # %if.else75
	movq	%rcx, (%rdx)
.LBB31_55:                              # %if.end79
	callq	free
	movq	cfun(%rip), %rax
	movq	(%rax), %rcx
	movl	60(%rcx), %eax
	addq	%rax, %r13
	movl	%r13d, 60(%rcx)
.LBB31_56:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB31_12:                              # %sw.bb23.i
	movq	48(%r13), %rax
	movq	%rax, 48(%rbx)
.LBB31_13:                              # %sw.default.i
	movabsq	$.L.str26, %rdi
	movabsq	$.L__FUNCTION__.duplicate_eh_region_1, %rdx
	movq	$1555, %rsi             # imm = 0x613
	callq	fancy_abort
.LBB31_57:                              # %if.then47.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.duplicate_eh_region_1, %rcx
	movq	$1564, %rsi             # imm = 0x61C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp246:
	.size	duplicate_eh_regions, .Ltmp246-duplicate_eh_regions
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI31_0:
	.quad	.LBB31_15
	.quad	.LBB31_9
	.quad	.LBB31_14
	.quad	.LBB31_11
	.quad	.LBB31_15
	.quad	.LBB31_12

	.text
	.globl	sjlj_emit_function_exit_after
	.align	16, 0x90
	.type	sjlj_emit_function_exit_after,@function
sjlj_emit_function_exit_after:          # @sjlj_emit_function_exit_after
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	%rdi, 136(%rax)
	retq
.Ltmp247:
	.size	sjlj_emit_function_exit_after, .Ltmp247-sjlj_emit_function_exit_after
	.cfi_endproc

	.globl	finish_eh_generation
	.align	16, 0x90
	.type	finish_eh_generation,@function
finish_eh_generation:                   # @finish_eh_generation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp254:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp255:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp256:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp257:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp258:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp259:
	.cfi_def_cfa_offset 112
.Ltmp260:
	.cfi_offset %rbx, -48
.Ltmp261:
	.cfi_offset %r12, -40
.Ltmp262:
	.cfi_offset %r13, -32
.Ltmp263:
	.cfi_offset %r14, -24
.Ltmp264:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	cmpq	$0, (%rax)
	je	.LBB33_78
# BB#1:                                 # %if.end
	callq	get_insns
	movq	%rax, %rdi
	callq	rebuild_jump_labels
	callq	get_insns
	movq	%rax, %rbx
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$16, %rdi
	callq	cleanup_cfg
	movq	cfun(%rip), %rbx
	movq	(%rbx), %rax
	cmpq	$0, 48(%rax)
	jne	.LBB33_3
# BB#2:                                 # %if.then.i
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	(%rbx), %rcx
	movq	%rax, 48(%rcx)
	movq	cfun(%rip), %rbx
	movq	(%rbx), %rax
.LBB33_3:                               # %get_exception_pointer.exit
	cmpq	$0, 40(%rax)
	jne	.LBB33_5
# BB#4:                                 # %if.then.i16
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movq	(%rbx), %rcx
	movq	%rax, 40(%rcx)
.LBB33_5:                               # %get_exception_filter.exit
	movabsq	$.L.str28, %rdx
	movq	$16, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 64(%rcx)
	movabsq	$.L.str29, %rdx
	movq	$64, %rdi
	movq	$1, %rsi
	callq	varray_init
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 72(%rcx)
	movabsq	$ttypes_filter_hash, %rsi
	movabsq	$ttypes_filter_eq, %rdx
	movabsq	$free, %rbx
	movq	$31, %rdi
	movq	%rbx, %rcx
	callq	htab_create
	movq	%rax, %r15
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movabsq	$ehspec_filter_hash, %rsi
	movabsq	$ehspec_filter_eq, %rdx
	movq	$31, %rdi
	movq	%rbx, %rcx
	callq	htab_create
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movslq	60(%rcx), %r13
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	jle	.LBB33_35
# BB#6:                                 # %for.body.lr.ph.i
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB33_7
	.align	16, 0x90
.LBB33_34:                              # %for.inc46.for.body_crit_edge.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rcx
.LBB33_7:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_12 Depth 2
                                        #     Child Loop BB33_20 Depth 2
                                        #       Child Loop BB33_22 Depth 3
	movq	8(%rcx), %rax
	movq	(%rax,%r13,8), %rdx
	testq	%rdx, %rdx
	je	.LBB33_33
# BB#8:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movl	24(%rdx), %eax
	movq	%r13, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jne	.LBB33_33
# BB#9:                                 # %if.end.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movl	40(%rdx), %eax
	cmpq	$4, %rax
	jne	.LBB33_10
# BB#18:                                # %sw.bb40.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	48(%rdx), %r13
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%r13, 48(%rsp)
	movq	$1, %rdx
	movq	16(%rsp), %rdi          # 8-byte Reload
	leaq	48(%rsp), %rsi
	callq	htab_find_slot
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB33_32
# BB#19:                                # %if.then.i74.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	$16, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r13, (%rax)
	movq	(%rcx), %rcx
	movq	72(%rcx), %rcx
	movq	8(%rcx), %rcx
	notq	%rcx
	movl	%ecx, 8(%rax)
	movq	%rax, (%rbx)
	movq	cfun(%rip), %rax
	movq	(%rax), %r15
	jmp	.LBB33_20
.LBB33_10:                              # %if.end.i
                                        #   in Loop: Header=BB33_7 Depth=1
	cmpq	$3, %rax
	jne	.LBB33_33
# BB#11:                                # %sw.bb.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	$0, 72(%rdx)
	movq	64(%rdx), %rbx
	testq	%rbx, %rbx
	jne	.LBB33_12
# BB#13:                                # %if.else.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	callq	htab_find_slot_with_hash
	movq	%rax, %r12
	movq	(%r12), %rbx
	testq	%rbx, %rbx
	jne	.LBB33_17
# BB#14:                                # %if.then.i.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	$16, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	%rax, %rbx
	movq	$0, (%rbx)
	movq	(%rcx), %rax
	movq	64(%rax), %rax
	movq	8(%rax), %rax
	incq	%rax
	movl	%eax, 8(%rbx)
	movq	%rbx, (%r12)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB33_16
# BB#15:                                # %if.then11.i.i
                                        #   in Loop: Header=BB33_7 Depth=1
	addq	%rsi, %rsi
	callq	varray_grow
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 64(%rcx)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	movq	8(%rdi), %rax
.LBB33_16:                              # %if.end.i.i
                                        #   in Loop: Header=BB33_7 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 8(%rdi)
	movq	$0, 32(%rdi,%rax,8)
.LBB33_17:                              # %add_ttypes_entry.exit.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movslq	8(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	72(%rbx), %rdx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, 72(%rbx)
	jmp	.LBB33_33
	.align	16, 0x90
.LBB33_12:                              # %for.body17.i
                                        #   Parent Loop BB33_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rbx), %rsi
	movq	%r15, %rdi
	movq	%rdx, %r12
	callq	add_ttypes_entry
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	72(%r12), %rdx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%r12, %rdx
	movq	%rax, 72(%rdx)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB33_12
	jmp	.LBB33_33
	.align	16, 0x90
.LBB33_28:                              # %push_uleb128.exit.i.i
                                        #   in Loop: Header=BB33_20 Depth=2
	movq	cfun(%rip), %rax
	movq	(%rax), %r15
	movq	(%r13), %r13
.LBB33_20:                              # %if.then.i74.i
                                        #   Parent Loop BB33_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB33_22 Depth 3
	addq	$72, %r15
	testq	%r13, %r13
	je	.LBB33_29
# BB#21:                                # %for.body.i.i
                                        #   in Loop: Header=BB33_20 Depth=2
	movq	32(%r13), %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	add_ttypes_entry
	movq	%rax, %r12
	.align	16, 0x90
.LBB33_22:                              # %do.body.i.i.i
                                        #   Parent Loop BB33_7 Depth=1
                                        #     Parent Loop BB33_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rbx
	movabsq	$4294967168, %rax       # imm = 0xFFFFFF80
	andq	%rax, %rbx
	movq	$7, %rcx
	shrq	%cl, %rbx
	testq	%rbx, %rbx
	jne	.LBB33_23
# BB#24:                                # %do.body.i.i.i
                                        #   in Loop: Header=BB33_22 Depth=3
	andq	$127, %r12
	jmp	.LBB33_25
	.align	16, 0x90
.LBB33_23:                              #   in Loop: Header=BB33_22 Depth=3
	orq	$-128, %r12
.LBB33_25:                              # %do.body.i.i.i
                                        #   in Loop: Header=BB33_22 Depth=3
	movq	(%r15), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB33_27
# BB#26:                                # %if.then5.i.i.i
                                        #   in Loop: Header=BB33_22 Depth=3
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%r15)
	movq	8(%rax), %rcx
.LBB33_27:                              # %if.end7.i.i.i
                                        #   in Loop: Header=BB33_22 Depth=3
	leaq	1(%rcx), %rdx
	testq	%r14, %rbx
	movq	%rdx, 8(%rax)
	movb	%r12b, 32(%rax,%rcx)
	movq	%rbx, %r12
	jne	.LBB33_22
	jmp	.LBB33_28
.LBB33_29:                              # %do.body.i.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	(%r15), %rdi
	movq	(%rdi), %rsi
	movq	8(%rdi), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB33_31
# BB#30:                                # %if.then14.i.i
                                        #   in Loop: Header=BB33_7 Depth=1
	addq	%rsi, %rsi
	callq	varray_grow
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 72(%rcx)
	movq	cfun(%rip), %rax
.LBB33_31:                              # %if.end.i81.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movq	(%rax), %rax
	movq	72(%rax), %rax
	movq	8(%rax), %rcx
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	xorq	%rdx, %rdx
	movb	%dl, 32(%rax,%rcx)
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB33_32:                              # %add_ehspec_entry.exit.i
                                        #   in Loop: Header=BB33_7 Depth=1
	movl	8(%rax), %eax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 56(%rcx)
	movq	24(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB33_33:                              # %for.inc46.i
                                        #   in Loop: Header=BB33_7 Depth=1
	decq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB33_34
.LBB33_35:                              # %assign_filter_values.exit
	movq	%r15, %rdi
	callq	htab_delete
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	htab_delete
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	60(%rax), %r14
	testq	%r14, %r14
	jle	.LBB33_77
# BB#36:                                # %for.body.lr.ph.i23
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB33_37:                              # %for.body.i28
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_42 Depth 2
                                        #       Child Loop BB33_46 Depth 3
	movq	8(%rax), %rax
	movq	(%rax,%r14,8), %rbx
	testq	%rbx, %rbx
	je	.LBB33_52
# BB#38:                                # %lor.lhs.false.i30
                                        #   in Loop: Header=BB33_37 Depth=1
	movl	24(%rbx), %eax
	movq	%r14, %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, %rax
	jne	.LBB33_52
# BB#39:                                # %if.end.i32
                                        #   in Loop: Header=BB33_37 Depth=1
	movl	40(%rbx), %eax
	decq	%rax
	andq	%rsi, %rax
	cmpq	$5, %rax
	ja	.LBB33_50
# BB#40:                                # %if.end.i32
                                        #   in Loop: Header=BB33_37 Depth=1
	jmpq	*.LJTI33_0(,%rax,8)
.LBB33_49:                              # %sw.bb59.i
                                        #   in Loop: Header=BB33_37 Depth=1
	movq	80(%rbx), %rax
	movq	%rax, 96(%rbx)
	jmp	.LBB33_52
.LBB33_41:                              # %sw.bb.i35
                                        #   in Loop: Header=BB33_37 Depth=1
	callq	gen_label_rtx
	movq	%rax, 96(%rbx)
	callq	start_sequence
	movq	96(%rbx), %rdi
	callq	emit_label
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	48(%rbx), %r13
	jmp	.LBB33_42
.LBB33_48:                              # %sw.bb40.i44
                                        #   in Loop: Header=BB33_37 Depth=1
	movq	%rsi, %r15
	callq	gen_label_rtx
	movq	%rax, 96(%rbx)
	callq	start_sequence
	movq	96(%rbx), %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	40(%rax), %r12
	movslq	56(%rbx), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	80(%rbx), %rsi
	movl	word_mode(%rip), %r8d
	movq	$103, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rsi, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_cmp_and_jump_insns
	movl	24(%rbx), %edx
	movq	$53, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_i
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%rax, 104(%rbx)
	callq	emit_barrier
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	movq	80(%rbx), %rsi
	movq	%r12, %rdi
	callq	emit_insns_before
	movq	%r15, %rsi
	jmp	.LBB33_52
.LBB33_47:                              # %for.inc.i
                                        #   in Loop: Header=BB33_42 Depth=2
	movq	48(%r13), %r13
.LBB33_42:                              # %sw.bb.i35
                                        #   Parent Loop BB33_37 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB33_46 Depth 3
	testq	%r13, %r13
	je	.LBB33_51
# BB#43:                                # %for.body7.i
                                        #   in Loop: Header=BB33_42 Depth=2
	movq	64(%r13), %r15
	testq	%r15, %r15
	jne	.LBB33_45
# BB#44:                                # %if.then11.i
                                        #   in Loop: Header=BB33_42 Depth=2
	movq	80(%r13), %rdi
	callq	emit_jump
	movq	48(%r13), %r13
	jmp	.LBB33_42
.LBB33_45:                              # %for.body19.lr.ph.i
                                        #   in Loop: Header=BB33_42 Depth=2
	movq	72(%r13), %r12
	.align	16, 0x90
.LBB33_46:                              # %for.body19.i
                                        #   Parent Loop BB33_37 Depth=1
                                        #     Parent Loop BB33_42 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	40(%rax), %rbx
	movq	32(%r12), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	80(%r13), %rsi
	movl	word_mode(%rip), %r8d
	movq	$103, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rsi, (%rsp)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_cmp_and_jump_insns
	movq	(%r15), %r15
	movq	(%r12), %r12
	testq	%r15, %r15
	jne	.LBB33_46
	jmp	.LBB33_47
.LBB33_51:                              # %for.end29.i
                                        #   in Loop: Header=BB33_37 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movl	24(%rbx), %edx
	movq	$53, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_i
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%rax, 104(%rbx)
	callq	emit_barrier
	callq	get_insns
	movq	%rax, %r15
	callq	end_sequence
	movq	48(%rbx), %rax
	movq	80(%rax), %rsi
	movq	%r15, %rdi
	callq	emit_insns_before
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB33_52:                              # %for.inc63.i
                                        #   in Loop: Header=BB33_37 Depth=1
	movq	cfun(%rip), %rax
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	(%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB33_37
# BB#53:                                # %build_post_landing_pads.exit
	movslq	60(%rax), %r15
	testq	%r15, %r15
	jle	.LBB33_77
	.align	16, 0x90
.LBB33_54:                              # %for.body.i55
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_58 Depth 2
	movq	8(%rax), %rax
	movq	(%rax,%r15,8), %r12
	testq	%r12, %r12
	je	.LBB33_63
# BB#55:                                # %lor.lhs.false.i58
                                        #   in Loop: Header=BB33_54 Depth=1
	movl	24(%r12), %eax
	movq	%r15, %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, %rax
	jne	.LBB33_63
# BB#56:                                # %if.end.i60
                                        #   in Loop: Header=BB33_54 Depth=1
	movq	104(%r12), %rax
	testq	%rax, %rax
	je	.LBB33_63
# BB#57:                                # %lor.lhs.false4.i
                                        #   in Loop: Header=BB33_54 Depth=1
	movzbl	3(%rax), %eax
	testq	$8, %rax
	movq	%r12, %rbx
	jne	.LBB33_63
	.align	16, 0x90
.LBB33_58:                              # %for.cond10.i
                                        #   Parent Loop BB33_54 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB33_61
# BB#59:                                # %for.body12.i
                                        #   in Loop: Header=BB33_58 Depth=2
	cmpq	$0, 96(%rbx)
	je	.LBB33_58
# BB#60:                                # %if.then18.i
                                        #   in Loop: Header=BB33_54 Depth=1
	movq	%rsi, %r13
	callq	start_sequence
	movq	96(%rbx), %rdi
	callq	emit_jump
	jmp	.LBB33_62
.LBB33_61:                              # %if.else.i65
                                        #   in Loop: Header=BB33_54 Depth=1
	movq	%rsi, %r13
	callq	start_sequence
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	target_flags(%rip), %r9d
	movq	libfunc_table+144(%rip), %rdi
	movq	48(%rax), %r8
	movq	$25, %rcx
	shrq	%cl, %r9
	andq	$1, %r9
	orq	$4, %r9
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%rax, %rax
	callq	emit_library_call
.LBB33_62:                              # %if.end22.i
                                        #   in Loop: Header=BB33_54 Depth=1
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	104(%r12), %rsi
	movq	%r14, %rdi
	callq	emit_insns_before
	movq	104(%r12), %rdi
	callq	delete_insn
	movq	%r13, %rsi
	.align	16, 0x90
.LBB33_63:                              # %for.inc27.i
                                        #   in Loop: Header=BB33_54 Depth=1
	movq	cfun(%rip), %rax
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	(%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB33_54
# BB#64:                                # %connect_post_landing_pads.exit
	movslq	60(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB33_77
# BB#65:                                # %for.body.lr.ph.i73
	movabsq	$.L.str27, %r14
	movq	%rsi, %r15
	.align	16, 0x90
.LBB33_66:                              # %for.body.i78
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rax
	movq	(%rax,%rbx,8), %r13
	testq	%r13, %r13
	je	.LBB33_76
# BB#67:                                # %lor.lhs.false.i81
                                        #   in Loop: Header=BB33_66 Depth=1
	movl	24(%r13), %eax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB33_76
# BB#68:                                # %if.end.i83
                                        #   in Loop: Header=BB33_66 Depth=1
	movl	40(%r13), %ecx
	cmpq	$4, %rcx
	ja	.LBB33_76
# BB#69:                                # %if.end.i83
                                        #   in Loop: Header=BB33_66 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$22, %rax
	je	.LBB33_76
# BB#70:                                # %if.end10.i
                                        #   in Loop: Header=BB33_66 Depth=1
	callq	start_sequence
	callq	gen_label_rtx
	movq	%rax, 88(%r13)
	movq	%rax, %rdi
	callq	emit_label
	movzbl	call_used_regs(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB33_72
# BB#71:                                # %if.then21.i
                                        #   in Loop: Header=BB33_66 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	$1, %rax
.LBB33_72:                              # %for.inc.i88
                                        #   in Loop: Header=BB33_66 Depth=1
	movzbl	call_used_regs+1(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB33_79
# BB#73:                                # %for.inc.1.thread.i
                                        #   in Loop: Header=BB33_66 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$1, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB33_74
.LBB33_79:                              # %for.inc.1.i
                                        #   in Loop: Header=BB33_66 Depth=1
	testq	%rax, %rax
	je	.LBB33_75
.LBB33_74:                              # %if.then29.i
                                        #   in Loop: Header=BB33_66 Depth=1
	movq	$40, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_s
	movq	%rax, %rdi
	callq	emit_insn
.LBB33_75:                              # %if.end32.i
                                        #   in Loop: Header=BB33_66 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	target_flags(%rip), %edi
	movq	48(%rax), %r12
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_REG
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	40(%rax), %r12
	movl	word_mode(%rip), %edi
	movq	$1, %rsi
	callq	gen_rtx_REG
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	movq	96(%r13), %rsi
	movq	%r12, %rdi
	callq	emit_insns_before
	.align	16, 0x90
.LBB33_76:                              # %for.inc44.i
                                        #   in Loop: Header=BB33_66 Depth=1
	movq	cfun(%rip), %rax
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	(%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB33_66
.LBB33_77:                              # %dw2_build_landing_pads.exit
	movq	$1, %rcx
	movl	%ecx, 56(%rax)
	callq	find_exception_handler_labels
	callq	get_insns
	movq	%rax, %rdi
	callq	rebuild_jump_labels
	callq	get_insns
	movq	%rax, %rbx
	callq	max_reg_num
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	find_basic_blocks
	movq	$16, %rdi
	callq	cleanup_cfg
.LBB33_78:                              # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB33_50:                              # %sw.default.i
	movabsq	$.L.str26, %rdi
	movabsq	$.L__FUNCTION__.build_post_landing_pads, %rdx
	movq	$2044, %rsi             # imm = 0x7FC
	callq	fancy_abort
.Ltmp265:
	.size	finish_eh_generation, .Ltmp265-finish_eh_generation
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI33_0:
	.quad	.LBB33_49
	.quad	.LBB33_41
	.quad	.LBB33_52
	.quad	.LBB33_48
	.quad	.LBB33_49
	.quad	.LBB33_52

	.text
	.globl	maybe_remove_eh_handler
	.align	16, 0x90
	.type	maybe_remove_eh_handler,@function
maybe_remove_eh_handler:                # @maybe_remove_eh_handler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp268:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp269:
	.cfi_def_cfa_offset 32
.Ltmp270:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	jne	.LBB34_6
# BB#1:                                 # %if.end
	movq	exception_handler_label_map(%rip), %rax
	movq	%rdi, (%rsp)
	leaq	(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	htab_find_slot
	testq	%rax, %rax
	je	.LBB34_6
# BB#2:                                 # %if.end5
	movq	(%rax), %rcx
	movq	8(%rcx), %rbx
	testq	%rbx, %rbx
	je	.LBB34_6
# BB#3:                                 # %if.end9
	movl	40(%rbx), %ecx
	cmpq	$5, %rcx
	jne	.LBB34_5
# BB#4:                                 # %if.then10
	movq	exception_handler_label_map(%rip), %rdi
	movq	%rax, %rsi
	callq	htab_clear_slot
	movq	$0, 80(%rbx)
	jmp	.LBB34_6
.LBB34_5:                               # %if.else
	movq	%rbx, %rdi
	callq	remove_eh_handler
.LBB34_6:                               # %if.end12
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp271:
	.size	maybe_remove_eh_handler, .Ltmp271-maybe_remove_eh_handler
	.cfi_endproc

	.align	16, 0x90
	.type	remove_eh_handler,@function
remove_eh_handler:                      # @remove_eh_handler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp276:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp277:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp278:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp279:
	.cfi_def_cfa_offset 48
.Ltmp280:
	.cfi_offset %rbx, -32
.Ltmp281:
	.cfi_offset %r14, -24
.Ltmp282:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	%rdi, %r14
	movq	(%r14), %r15
	movslq	24(%r14), %rcx
	movq	8(%rax), %rax
	movq	%r15, (%rax,%rcx,8)
	movq	32(%r14), %rax
	testq	%rax, %rax
	je	.LBB35_12
# BB#1:                                 # %do.body
	movq	(%rax), %r9
	testq	%r9, %r9
	je	.LBB35_12
# BB#2:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_3:                               # %for.cond9.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB35_4 Depth 2
                                        #     Child Loop BB35_8 Depth 2
	movq	24(%r9), %rdi
	testq	%rdi, %rdi
	movq	$1, %rbx
	je	.LBB35_7
	.align	16, 0x90
.LBB35_4:                               # %for.body18
                                        #   Parent Loop BB35_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rbx), %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB35_6
# BB#5:                                 # %if.then20
                                        #   in Loop: Header=BB35_4 Depth=2
	movl	16(%r9), %edx
	movq	cfun(%rip), %r10
	movq	$7, %rcx
	shlq	%cl, %rdx
	notq	%rsi
	movq	(%r10), %r10
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	8(%r10), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	andq	%rsi, %rdi
	movq	%r15, (%rdx,%rax,8)
	je	.LBB35_7
.LBB35_6:                               # %for.inc
                                        #   in Loop: Header=BB35_4 Depth=2
	movq	%rbx, %rax
	andq	%r8, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB35_4
.LBB35_7:                               # %if.end33
                                        #   in Loop: Header=BB35_3 Depth=1
	movq	32(%r9), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB35_11
	.align	16, 0x90
.LBB35_8:                               # %for.body18.1
                                        #   Parent Loop BB35_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB35_10
# BB#9:                                 # %if.then20.1
                                        #   in Loop: Header=BB35_8 Depth=2
	movl	16(%r9), %edx
	movq	cfun(%rip), %rbx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rdx
	movq	(%rbx), %rbx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	8(%rbx), %rbx
	sarq	%cl, %rdx
	notq	%rsi
	andq	%rsi, %rdi
	movq	%r15, (%rbx,%rdx,8)
	je	.LBB35_11
.LBB35_10:                              # %for.inc.1
                                        #   in Loop: Header=BB35_8 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	$64, %rcx
	jb	.LBB35_8
.LBB35_11:                              # %if.end33.1
                                        #   in Loop: Header=BB35_3 Depth=1
	movq	(%r9), %r9
	testq	%r9, %r9
	jne	.LBB35_3
.LBB35_12:                              # %if.end40
	testq	%r15, %r15
	je	.LBB35_18
# BB#13:                                # %if.then42
	movq	32(%r15), %rdi
	testq	%rdi, %rdi
	jne	.LBB35_15
# BB#14:                                # %if.then45
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, %rdi
	movq	%rdi, 32(%r15)
.LBB35_15:                              # %if.end48
	movq	32(%r14), %rdx
	testq	%rdx, %rdx
	je	.LBB35_17
# BB#16:                                # %if.then51
	movq	$2, %rcx
	movq	%rdi, %rsi
	callq	bitmap_operation
	movq	32(%r15), %rdi
.LBB35_17:                              # %if.end56
	movl	24(%r14), %esi
	callq	bitmap_set_bit
.LBB35_18:                              # %if.end59
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	je	.LBB35_19
# BB#20:                                # %select.mid
	leaq	88(%r14), %rax
	jmp	.LBB35_21
.LBB35_19:
	leaq	80(%r14), %rax
.LBB35_21:                              # %select.end
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB35_27
# BB#22:                                # %if.then65
	movq	exception_handler_label_map(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB35_27
# BB#23:                                # %if.end.i
	movq	%rax, (%rsp)
	leaq	(%rsp), %rsi
	xorq	%rdx, %rdx
	callq	htab_find_slot
	testq	%rax, %rax
	je	.LBB35_24
# BB#26:                                # %if.end3.i
	movq	exception_handler_label_map(%rip), %rdi
	movq	%rax, %rsi
	callq	htab_clear_slot
.LBB35_27:                              # %if.end66
	testq	%r15, %r15
	je	.LBB35_29
# BB#28:                                # %if.then68
	leaq	8(%r15), %rax
	jmp	.LBB35_30
.LBB35_29:                              # %if.else70
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
.LBB35_30:                              # %if.end72
	movq	%rax, %rdx
	.align	16, 0x90
.LBB35_31:                              # %for.cond73
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rcx
	movq	(%rcx), %rdx
	cmpq	%r14, %rdx
	leaq	16(%rdx), %rdx
	jne	.LBB35_31
# BB#32:                                # %for.end77
	movq	16(%r14), %rdx
	movq	%rdx, (%rcx)
	movq	8(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB35_38
# BB#33:                                # %for.cond82.preheader
	movq	16(%rcx), %rsi
	testq	%rsi, %rsi
	movq	%r15, (%rcx)
	je	.LBB35_34
	.align	16, 0x90
.LBB35_35:                              # %for.inc87
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	movq	16(%rdx), %rsi
	testq	%rsi, %rsi
	movq	%r15, (%rdx)
	jne	.LBB35_35
# BB#36:
	addq	$16, %rdx
	jmp	.LBB35_37
.LBB35_34:
	leaq	16(%rcx), %rdx
.LBB35_37:                              # %for.end89
	movq	(%rax), %rsi
	movq	%rsi, (%rdx)
	movq	%rcx, (%rax)
.LBB35_38:                              # %if.end92
	movl	40(%r14), %eax
	cmpq	$3, %rax
	jne	.LBB35_51
# BB#39:
	leaq	16(%r14), %rdi
	jmp	.LBB35_40
	.align	16, 0x90
.LBB35_42:                              # %for.inc101
                                        #   in Loop: Header=BB35_40 Depth=1
	addq	$16, %rdi
.LBB35_40:                              # %for.cond97
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rdi
	movl	40(%rdi), %eax
	cmpq	$2, %rax
	je	.LBB35_44
# BB#41:                                # %for.cond97
                                        #   in Loop: Header=BB35_40 Depth=1
	cmpq	$3, %rax
	je	.LBB35_42
# BB#43:                                # %if.then106
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.remove_eh_handler, %rcx
	movq	$2698, %rsi             # imm = 0xA8A
	jmp	.LBB35_25
.LBB35_44:                              # %if.end107
	movq	48(%r14), %rax
	movq	56(%r14), %rcx
	testq	%rax, %rax
	je	.LBB35_46
# BB#45:                                # %if.then111
	movq	%rcx, 56(%rax)
	jmp	.LBB35_47
.LBB35_46:                              # %if.else115
	movq	%rcx, 56(%rdi)
.LBB35_47:                              # %if.end118
	testq	%rcx, %rcx
	je	.LBB35_49
# BB#48:                                # %if.then120
	movq	%rax, 48(%rcx)
	jmp	.LBB35_51
.LBB35_49:                              # %if.else124
	testq	%rax, %rax
	movq	%rax, 48(%rdi)
	jne	.LBB35_51
# BB#50:                                # %if.then129
	callq	remove_eh_handler
.LBB35_51:                              # %if.end132
	movq	%r14, %rdi
	callq	free
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB35_24:                              # %if.then2.i
	movabsq	$.L.str26, %rax
	movabsq	$.L__FUNCTION__.remove_exception_handler_label, %rcx
	movq	$2625, %rsi             # imm = 0xA41
.LBB35_25:                              # %if.then2.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp283:
	.size	remove_eh_handler, .Ltmp283-remove_eh_handler
	.cfi_endproc

	.globl	for_each_eh_label
	.align	16, 0x90
	.type	for_each_eh_label,@function
for_each_eh_label:                      # @for_each_eh_label
	.cfi_startproc
# BB#0:                                 # %entry
	movq	exception_handler_label_map(%rip), %rax
	movabsq	$for_each_eh_label_1, %rsi
	movq	%rdi, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rdx
	jmp	htab_traverse  # TAILCALL
.Ltmp284:
	.size	for_each_eh_label, .Ltmp284-for_each_eh_label
	.cfi_endproc

	.align	16, 0x90
	.type	for_each_eh_label_1,@function
for_each_eh_label_1:                    # @for_each_eh_label_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp286:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rax
	movq	(%rax), %rdi
	callq	*%rsi
	movq	$1, %rax
	popq	%rdx
	retq
.Ltmp287:
	.size	for_each_eh_label_1, .Ltmp287-for_each_eh_label_1
	.cfi_endproc

	.globl	reachable_handlers
	.align	16, 0x90
	.type	reachable_handlers,@function
reachable_handlers:                     # @reachable_handlers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp293:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp294:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp295:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp296:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp297:
	.cfi_def_cfa_offset 64
.Ltmp298:
	.cfi_offset %rbx, -40
.Ltmp299:
	.cfi_offset %r12, -32
.Ltmp300:
	.cfi_offset %r14, -24
.Ltmp301:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$33, %rcx
	jne	.LBB38_3
# BB#1:                                 # %land.lhs.true
	movq	32(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$53, %rdx
	jne	.LBB38_3
# BB#2:                                 # %if.then
	movl	8(%rcx), %edx
	jmp	.LBB38_6
.LBB38_3:                               # %if.else
	movq	$23, %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB38_16
# BB#4:                                 # %lor.lhs.false
	movq	8(%rax), %rax
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	jle	.LBB38_16
# BB#5:                                 # %if.end
	movl	(%r14), %eax
.LBB38_6:                               # %if.end22
	movq	cfun(%rip), %rcx
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	movq	(%rcx), %rsi
	movq	$32, %rcx
	movq	8(%rsi), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	(%rsi,%rdx,8), %rbx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB38_9
# BB#7:                                 # %land.lhs.true28
	movq	32(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$53, %rax
	jne	.LBB38_9
# BB#8:                                 # %if.then36
	xorq	%r15, %r15
	testq	%rbx, %rbx
	jne	.LBB38_11
	jmp	.LBB38_16
.LBB38_9:                               # %if.else41
	movl	40(%rbx), %eax
	xorq	%r15, %r15
	cmpq	$6, %rax
	jne	.LBB38_12
# BB#10:                                # %if.then44
	movq	48(%rbx), %r15
.LBB38_11:                              # %for.cond.preheader
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB38_15
.LBB38_12:                              # %for.body.lr.ph
	leaq	(%rsp), %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB38_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	reachable_next_level
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB38_15
# BB#14:                                # %for.inc
                                        #   in Loop: Header=BB38_13 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB38_13
.LBB38_15:                              # %for.end
	movq	16(%rsp), %r15
.LBB38_16:                              # %return
	movq	%r15, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp302:
	.size	reachable_handlers, .Ltmp302-reachable_handlers
	.cfi_endproc

	.align	16, 0x90
	.type	reachable_next_level,@function
reachable_next_level:                   # @reachable_next_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp309:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp310:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp311:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp312:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp313:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp314:
	.cfi_def_cfa_offset 96
.Ltmp315:
	.cfi_offset %rbx, -48
.Ltmp316:
	.cfi_offset %r12, -40
.Ltmp317:
	.cfi_offset %r13, -32
.Ltmp318:
	.cfi_offset %r14, -24
.Ltmp319:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movl	40(%r12), %eax
	decq	%rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$4, %rax
	ja	.LBB39_78
# BB#1:                                 # %entry
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	jmpq	*.LJTI39_0(,%rax,8)
.LBB39_2:                               # %for.cond.preheader
	movq	48(%r12), %rax
	testq	%rax, %rax
	je	.LBB39_31
# BB#3:                                 # %for.body.lr.ph
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB39_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB39_19 Depth 2
                                        #     Child Loop BB39_35 Depth 2
                                        #       Child Loop BB39_44 Depth 3
                                        #       Child Loop BB39_40 Depth 3
	movq	64(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB39_5
# BB#17:                                # %if.end
                                        #   in Loop: Header=BB39_4 Depth=1
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB39_29
# BB#18:                                # %for.body11.preheader
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB39_19:                              # %for.body11
                                        #   Parent Loop BB39_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rbx), %rdi
	cmpq	%r14, %rdi
	je	.LBB39_22
# BB#20:                                # %lor.lhs.false
                                        #   in Loop: Header=BB39_19 Depth=2
	movq	lang_eh_type_covers(%rip), %rax
	testq	%rax, %rax
	je	.LBB39_27
# BB#21:                                # %land.lhs.true
                                        #   in Loop: Header=BB39_19 Depth=2
	movq	%r14, %rsi
	callq	*%rax
	testq	%r13, %rax
	jne	.LBB39_22
.LBB39_27:                              # %for.inc
                                        #   in Loop: Header=BB39_19 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB39_19
# BB#28:                                # %for.end
                                        #   in Loop: Header=BB39_4 Depth=1
	xorq	%r14, %r14
	cmpq	$0, lang_eh_type_covers(%rip)
	movq	%r15, %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	jne	.LBB39_31
.LBB39_29:                              # %if.end21
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	$1, %r14
	testq	%rdx, %rdx
	je	.LBB39_30
# BB#32:                                # %if.else
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	64(%rax), %r15
	testq	%r15, %r15
	jne	.LBB39_34
# BB#33:                                #   in Loop: Header=BB39_4 Depth=1
	movq	16(%rsp), %r14          # 8-byte Reload
	jmp	.LBB39_30
.LBB39_34:                              # %for.body30.lr.ph
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB39_35:                              # %for.body30
                                        #   Parent Loop BB39_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB39_44 Depth 3
                                        #       Child Loop BB39_40 Depth 3
	movq	32(%r15), %rbx
	movq	lang_eh_type_covers(%rip), %rax
	movq	(%rdx), %r14
	movq	%rdx, %r12
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jne	.LBB39_36
# BB#39:                                # %for.cond.preheader.i
                                        #   in Loop: Header=BB39_35 Depth=2
	testq	%r14, %r14
	movq	%r14, %rax
	je	.LBB39_48
	.align	16, 0x90
.LBB39_40:                              # %for.body.i
                                        #   Parent Loop BB39_4 Depth=1
                                        #     Parent Loop BB39_35 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	je	.LBB39_41
# BB#42:                                # %for.inc.i
                                        #   in Loop: Header=BB39_40 Depth=3
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB39_40
# BB#43:                                #   in Loop: Header=BB39_35 Depth=2
	movq	%r14, %rdx
	jmp	.LBB39_48
	.align	16, 0x90
.LBB39_36:                              # %for.cond3.preheader.i
                                        #   in Loop: Header=BB39_35 Depth=2
	testq	%r14, %r14
	je	.LBB39_48
# BB#37:                                # %for.body5.i.preheader
                                        #   in Loop: Header=BB39_35 Depth=2
	movq	32(%r14), %rdi
	movq	%rbx, %rsi
	callq	*%rax
	testq	%r13, %rax
	jne	.LBB39_38
	.align	16, 0x90
.LBB39_44:                              # %for.inc11.i
                                        #   Parent Loop BB39_4 Depth=1
                                        #     Parent Loop BB39_35 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%r14), %r14
	testq	%r14, %r14
	je	.LBB39_47
# BB#45:                                # %for.inc11.for.body5_crit_edge.i
                                        #   in Loop: Header=BB39_44 Depth=3
	movq	32(%r14), %rdi
	movq	%rbx, %rsi
	callq	*lang_eh_type_covers(%rip)
	testq	%r13, %rax
	je	.LBB39_44
# BB#46:                                #   in Loop: Header=BB39_35 Depth=2
	movq	%r12, %rdx
	jmp	.LBB39_49
.LBB39_41:                              #   in Loop: Header=BB39_35 Depth=2
	movq	%r12, %rdx
	jmp	.LBB39_49
.LBB39_38:                              #   in Loop: Header=BB39_35 Depth=2
	movq	%r12, %rdx
	jmp	.LBB39_49
.LBB39_47:                              # %if.then36.loopexit283
                                        #   in Loop: Header=BB39_35 Depth=2
	movq	(%r12), %rdx
	.align	16, 0x90
.LBB39_48:                              # %if.then36
                                        #   in Loop: Header=BB39_35 Depth=2
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%r12, %rdx
	movq	%rax, (%rdx)
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
.LBB39_49:                              # %for.inc41
                                        #   in Loop: Header=BB39_35 Depth=2
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB39_35
# BB#50:                                # %for.end44
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	$1, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	je	.LBB39_52
# BB#51:                                # %for.end44
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	$1, %rax
.LBB39_52:                              # %for.end44
                                        #   in Loop: Header=BB39_4 Depth=1
	testq	%rcx, %rcx
	movq	%rax, %r14
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	je	.LBB39_30
# BB#53:                                # %if.end.i175
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	%rax, %rbx
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	jne	.LBB39_54
# BB#57:                                # %if.else.i185
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	80(%rbx), %rdi
	movq	16(%rdx), %rsi
	movq	%rdx, %r14
	callq	alloc_INSN_LIST
	movq	%r14, %rdx
	movq	%rax, 16(%rdx)
	movq	%rbx, %rax
	movq	$1, %r14
	jmp	.LBB39_30
.LBB39_54:                              # %if.then2.i178
                                        #   in Loop: Header=BB39_4 Depth=1
	cmpq	$0, 16(%rdx)
	movq	$1, %r14
	jne	.LBB39_55
# BB#56:                                # %if.then4.i181
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	88(%r12), %rdi
	xorq	%rsi, %rsi
	movq	%rdx, %r15
	callq	alloc_INSN_LIST
	movq	%r15, %rdx
	movq	%rax, 16(%rdx)
	movq	%rbx, %rax
	jmp	.LBB39_30
.LBB39_55:                              #   in Loop: Header=BB39_4 Depth=1
	movq	%rbx, %rax
	.align	16, 0x90
.LBB39_30:                              # %for.cond.backedge
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	48(%rax), %rax
	testq	%rax, %rax
	movq	%r14, 16(%rsp)          # 8-byte Spill
	jne	.LBB39_4
	jmp	.LBB39_31
.LBB39_58:                              # %sw.bb53
	movq	48(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB39_59
# BB#60:                                # %if.end58
	testq	%rdx, %rdx
	je	.LBB39_62
# BB#61:                                # %if.then60
	movq	%rdx, %rbx
	movq	8(%rbx), %rdx
	xorq	%rdi, %rdi
	callq	tree_cons
	movq	%rbx, %rdx
	movq	%rax, 8(%rdx)
.LBB39_62:                              # %if.end66
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB39_8
# BB#63:                                # %if.end66
	movq	lang_eh_type_covers(%rip), %rax
	testq	%rax, %rax
	je	.LBB39_8
# BB#64:                                # %for.cond3.preheader.i205
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	48(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB39_69
# BB#65:                                # %for.body5.i218.preheader
	movq	32(%rbx), %rdi
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	*%rax
	jmp	.LBB39_68
	.align	16, 0x90
.LBB39_67:                              # %for.inc11.for.body5_crit_edge.i223
                                        #   in Loop: Header=BB39_68 Depth=1
	movq	32(%rbx), %rdi
	movq	%r15, %rsi
	callq	*lang_eh_type_covers(%rip)
.LBB39_68:                              # %for.inc11.for.body5_crit_edge.i223
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rax
	jne	.LBB39_31
# BB#66:                                # %for.inc11.i221
                                        #   in Loop: Header=BB39_68 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB39_67
.LBB39_69:                              # %if.else77
	movq	$2, %r14
	movq	32(%rsp), %r15          # 8-byte Reload
	testq	%r15, %r15
	je	.LBB39_31
# BB#70:                                # %if.end.i230
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	jne	.LBB39_24
# BB#71:                                # %if.else.i240
	movq	80(%r12), %rdi
	jmp	.LBB39_72
.LBB39_8:                               # %sw.bb
	movq	$1, %r14
	jmp	.LBB39_9
.LBB39_74:                              # %sw.bb80
	movq	$3, %r14
	testq	%rdx, %rdx
	je	.LBB39_31
# BB#75:                                # %land.lhs.true82
	movq	16(%rdx), %rsi
	testq	%rsi, %rsi
	je	.LBB39_31
# BB#76:                                # %if.end.i262
	movq	%rdx, %rbx
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	movq	$2, %r14
	jne	.LBB39_31
# BB#77:                                # %if.else.i272
	movq	80(%r12), %rdi
	jmp	.LBB39_16
.LBB39_22:                              # %if.then16
	movq	$2, %r14
	testq	%r15, %r15
	je	.LBB39_31
# BB#23:                                # %if.end.i156
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	je	.LBB39_26
.LBB39_24:                              # %if.then2.i159
	cmpq	$0, 16(%r15)
	jne	.LBB39_31
# BB#25:                                # %if.then4.i162
	movq	88(%r12), %rdi
	xorq	%rsi, %rsi
	jmp	.LBB39_73
.LBB39_59:                              # %if.then57
	movq	$2, %r14
.LBB39_9:                               # %sw.bb
	testq	%rdx, %rdx
	je	.LBB39_31
# BB#10:                                # %if.end.i
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	jne	.LBB39_11
# BB#13:                                # %if.else.i
	movq	80(%r12), %rdi
.LBB39_14:                              # %return
	movq	16(%rdx), %rsi
	jmp	.LBB39_15
.LBB39_78:                              # %sw.epilog
	movabsq	$.L.str26, %rdi
	movabsq	$.L__FUNCTION__.reachable_next_level, %rdx
	movq	$3010, %rsi             # imm = 0xBC2
	callq	fancy_abort
.LBB39_26:                              # %if.else.i166
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	80(%rax), %rdi
.LBB39_72:                              # %return
	movq	16(%r15), %rsi
.LBB39_73:                              # %return
	callq	alloc_INSN_LIST
	movq	%rax, 16(%r15)
	jmp	.LBB39_31
.LBB39_5:                               # %if.then
	movq	$2, %r14
	testq	%rdx, %rdx
	je	.LBB39_31
# BB#6:                                 # %if.end.i140
	movq	%rax, %rcx
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movl	56(%rax), %eax
	testq	%rax, %rax
	je	.LBB39_7
.LBB39_11:                              # %if.then2.i
	cmpq	$0, 16(%rdx)
	jne	.LBB39_31
# BB#12:                                # %if.then4.i
	movq	88(%r12), %rdi
	xorq	%rsi, %rsi
.LBB39_15:                              # %return
	movq	%rdx, %rbx
.LBB39_16:                              # %return
	callq	alloc_INSN_LIST
	movq	%rax, 16(%rbx)
.LBB39_31:                              # %return
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB39_7:                               # %if.else.i150
	movq	80(%rcx), %rdi
	jmp	.LBB39_14
.Ltmp320:
	.size	reachable_next_level, .Ltmp320-reachable_next_level
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI39_0:
	.quad	.LBB39_8
	.quad	.LBB39_2
	.quad	.LBB39_31
	.quad	.LBB39_58
	.quad	.LBB39_74

	.text
	.globl	can_throw_internal
	.align	16, 0x90
	.type	can_throw_internal,@function
can_throw_internal:                     # @can_throw_internal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp326:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp327:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp328:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp329:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp330:
	.cfi_def_cfa_offset 48
.Ltmp331:
	.cfi_offset %rbx, -40
.Ltmp332:
	.cfi_offset %r12, -32
.Ltmp333:
	.cfi_offset %r14, -24
.Ltmp334:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	xorq	%r12, %r12
	cmpq	$105, %rdx
	jne	.LBB40_24
# BB#1:                                 # %if.end
	cmpq	$32, %rcx
	jne	.LBB40_4
# BB#2:                                 # %land.lhs.true
	movq	32(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB40_4
# BB#3:                                 # %if.then11
	movq	8(%rcx), %rax
	movq	8(%rax), %r14
	movl	(%r14), %eax
.LBB40_4:                               # %if.end18
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB40_13
# BB#5:                                 # %land.lhs.true23
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$141, %rcx
	jne	.LBB40_13
# BB#6:
	xorq	%r15, %r15
	movq	8(%rax,%r15,8), %rbx
	jmp	.LBB40_9
	.align	16, 0x90
.LBB40_8:                               # %for.cond41
                                        #   in Loop: Header=BB40_9 Depth=1
	movq	24(%rbx), %rbx
.LBB40_9:                               # %for.cond41
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB40_11
# BB#10:                                # %for.body42
                                        #   in Loop: Header=BB40_9 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_internal
	movq	$1, %r12
	testq	%rax, %rax
	je	.LBB40_8
	jmp	.LBB40_24
.LBB40_11:                              # %for.inc48
                                        #   in Loop: Header=BB40_9 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r12, %r12
	cmpq	$2, %rax
	jg	.LBB40_24
# BB#12:                                # %for.inc48.for.body_crit_edge
                                        #   in Loop: Header=BB40_9 Depth=1
	movq	32(%r14), %rax
	movq	8(%rax,%r15,8), %rbx
	jmp	.LBB40_9
.LBB40_13:                              # %if.end50
	movq	$23, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB40_24
# BB#14:                                # %lor.lhs.false
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	jle	.LBB40_24
# BB#15:                                # %if.end61
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	(%rcx,%rax,8), %r14
	movl	40(%r14), %eax
	xorq	%r12, %r12
	cmpq	$6, %rax
	jne	.LBB40_16
# BB#17:                                # %if.then71
	movq	(%r14), %rax
	movq	48(%r14), %r15
	movq	%rax, %r14
	jmp	.LBB40_18
.LBB40_16:
	xorq	%r15, %r15
.LBB40_18:                              # %for.cond74.preheader
	testq	%r14, %r14
	je	.LBB40_24
# BB#19:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	$1, %r12
	.align	16, 0x90
.LBB40_20:                              # %for.body76
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	reachable_next_level
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$3, %rcx
	je	.LBB40_23
# BB#21:                                # %for.body76
                                        #   in Loop: Header=BB40_20 Depth=1
	testq	%rbx, %rax
	jne	.LBB40_24
# BB#22:                                # %for.inc86
                                        #   in Loop: Header=BB40_20 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB40_20
.LBB40_23:                              # %return.loopexit2
	xorq	%r12, %r12
.LBB40_24:                              # %return
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp335:
	.size	can_throw_internal, .Ltmp335-can_throw_internal
	.cfi_endproc

	.globl	can_throw_external
	.align	16, 0x90
	.type	can_throw_external,@function
can_throw_external:                     # @can_throw_external
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp341:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp342:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp343:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp344:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp345:
	.cfi_def_cfa_offset 48
.Ltmp346:
	.cfi_offset %rbx, -40
.Ltmp347:
	.cfi_offset %r12, -32
.Ltmp348:
	.cfi_offset %r14, -24
.Ltmp349:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	xorq	%r12, %r12
	cmpq	$105, %rdx
	jne	.LBB41_27
# BB#1:                                 # %if.end
	cmpq	$32, %rcx
	jne	.LBB41_4
# BB#2:                                 # %land.lhs.true
	movq	32(%r14), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB41_4
# BB#3:                                 # %if.then11
	movq	8(%rcx), %rax
	movq	8(%rax), %r14
	movl	(%r14), %eax
.LBB41_4:                               # %if.end18
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$34, %rax
	jne	.LBB41_13
# BB#5:                                 # %land.lhs.true23
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$141, %rcx
	jne	.LBB41_13
# BB#6:
	xorq	%r15, %r15
	movq	8(%rax,%r15,8), %rbx
	jmp	.LBB41_9
	.align	16, 0x90
.LBB41_8:                               # %for.cond41
                                        #   in Loop: Header=BB41_9 Depth=1
	movq	24(%rbx), %rbx
.LBB41_9:                               # %for.cond41
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB41_11
# BB#10:                                # %for.body42
                                        #   in Loop: Header=BB41_9 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_external
	movq	$1, %r12
	testq	%rax, %rax
	je	.LBB41_8
	jmp	.LBB41_27
.LBB41_11:                              # %for.inc48
                                        #   in Loop: Header=BB41_9 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r12, %r12
	cmpq	$2, %rax
	jg	.LBB41_27
# BB#12:                                # %for.inc48.for.body_crit_edge
                                        #   in Loop: Header=BB41_9 Depth=1
	movq	32(%r14), %rax
	movq	8(%rax,%r15,8), %rbx
	jmp	.LBB41_9
.LBB41_13:                              # %if.end50
	movq	$23, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB41_14
# BB#19:                                # %if.end64
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	jle	.LBB41_27
# BB#20:                                # %if.end73
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	(%rcx,%rax,8), %r14
	movl	40(%r14), %eax
	xorq	%r15, %r15
	cmpq	$6, %rax
	jne	.LBB41_22
# BB#21:                                # %if.then83
	movq	(%r14), %rax
	movq	48(%r14), %r15
	movq	%rax, %r14
.LBB41_22:                              # %for.cond86.preheader
	testq	%r14, %r14
	je	.LBB41_26
# BB#23:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	.align	16, 0x90
.LBB41_24:                              # %for.body88
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	reachable_next_level
	andq	%rbx, %rax
	cmpq	$1, %rax
	ja	.LBB41_27
# BB#25:                                # %for.inc94
                                        #   in Loop: Header=BB41_24 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB41_24
.LBB41_26:
	movq	$1, %r12
	jmp	.LBB41_27
.LBB41_14:                              # %if.then53
	movzbl	(%r14), %eax
	cmpq	$34, %rax
	movq	$1, %r12
	je	.LBB41_27
# BB#15:                                # %lor.rhs
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %r12
	je	.LBB41_27
# BB#16:                                # %land.rhs
	movq	32(%r14), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB41_17
# BB#18:                                # %land.rhs
	movabsq	$1, %r12
	jmp	.LBB41_27
.LBB41_17:
	movabsq	$0, %r12
.LBB41_27:                              # %return
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp350:
	.size	can_throw_external, .Ltmp350-can_throw_external
	.cfi_endproc

	.globl	nothrow_function_p
	.align	16, 0x90
	.type	nothrow_function_p,@function
nothrow_function_p:                     # @nothrow_function_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp353:
	.cfi_def_cfa_offset 16
.Ltmp354:
	.cfi_offset %rbx, -16
	movl	flag_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB42_9
# BB#1:                                 # %if.end
	callq	get_insns
	movq	%rax, %rbx
	jmp	.LBB42_2
	.align	16, 0x90
.LBB42_4:                               # %for.inc
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	24(%rbx), %rbx
.LBB42_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB42_5
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_external
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB42_4
	jmp	.LBB42_10
.LBB42_5:                               # %for.end
	movq	cfun(%rip), %rax
	movq	416(%rax), %rbx
	jmp	.LBB42_6
	.align	16, 0x90
.LBB42_8:                               # %for.inc11
                                        #   in Loop: Header=BB42_6 Depth=1
	movq	16(%rbx), %rbx
.LBB42_6:                               # %for.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB42_9
# BB#7:                                 # %for.body7
                                        #   in Loop: Header=BB42_6 Depth=1
	movq	%rbx, %rdi
	callq	can_throw_external
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB42_8
	jmp	.LBB42_10
.LBB42_9:
	movq	$1, %rax
.LBB42_10:                              # %return
	popq	%rbx
	retq
.Ltmp355:
	.size	nothrow_function_p, .Ltmp355-nothrow_function_p
	.cfi_endproc

	.globl	expand_builtin_unwind_init
	.align	16, 0x90
	.type	expand_builtin_unwind_init,@function
expand_builtin_unwind_init:             # @expand_builtin_unwind_init
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 424(%rax)
	jmp	ix86_setup_frame_addresses  # TAILCALL
.Ltmp356:
	.size	expand_builtin_unwind_init, .Ltmp356-expand_builtin_unwind_init
	.cfi_endproc

	.globl	expand_builtin_eh_return_data_regno
	.align	16, 0x90
	.type	expand_builtin_eh_return_data_regno,@function
expand_builtin_eh_return_data_regno:    # @expand_builtin_eh_return_data_regno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp358:
	.cfi_def_cfa_offset 16
	movq	32(%rdi), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB44_1
# BB#3:                                 # %if.end
	movq	$1, %rsi
	callq	tree_low_cst
	cmpq	$2, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jb	.LBB44_5
# BB#4:                                 # %if.end
	movq	%rcx, %rax
.LBB44_5:                               # %if.end
	cmpq	%rcx, %rax
	je	.LBB44_2
# BB#6:                                 # %if.end4
	movl	target_flags(%rip), %ecx
	testq	$33554432, %rcx         # imm = 0x2000000
	je	.LBB44_7
# BB#8:                                 # %if.end4
	movabsq	$dbx64_register_map, %rcx
	jmp	.LBB44_9
.LBB44_1:                               # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	error
.LBB44_2:                               # %return
	movq	const_int_rtx+504(%rip), %rax
	popq	%rdx
	retq
.LBB44_7:
	movabsq	$svr4_dbx_register_map, %rcx
.LBB44_9:                               # %if.end4
	movslq	(%rcx,%rax,4), %rsi
	xorq	%rdi, %rdi
	popq	%rax
	jmp	gen_rtx_CONST_INT  # TAILCALL
.Ltmp359:
	.size	expand_builtin_eh_return_data_regno, .Ltmp359-expand_builtin_eh_return_data_regno
	.cfi_endproc

	.globl	expand_builtin_extract_return_addr
	.align	16, 0x90
	.type	expand_builtin_extract_return_addr,@function
expand_builtin_extract_return_addr:     # @expand_builtin_extract_return_addr
	.cfi_startproc
# BB#0:                                 # %entry
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	jmp	expand_expr  # TAILCALL
.Ltmp360:
	.size	expand_builtin_extract_return_addr, .Ltmp360-expand_builtin_extract_return_addr
	.cfi_endproc

	.globl	expand_builtin_frob_return_addr
	.align	16, 0x90
	.type	expand_builtin_frob_return_addr,@function
expand_builtin_frob_return_addr:        # @expand_builtin_frob_return_addr
	.cfi_startproc
# BB#0:                                 # %entry
	movl	ptr_mode(%rip), %edx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	jmp	expand_expr  # TAILCALL
.Ltmp361:
	.size	expand_builtin_frob_return_addr, .Ltmp361-expand_builtin_frob_return_addr
	.cfi_endproc

	.globl	expand_builtin_eh_return
	.align	16, 0x90
	.type	expand_builtin_eh_return,@function
expand_builtin_eh_return:               # @expand_builtin_eh_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp365:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp366:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp367:
	.cfi_def_cfa_offset 32
.Ltmp368:
	.cfi_offset %rbx, -24
.Ltmp369:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	104(%rax), %rax
	movq	%rsi, %r14
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rsi
	callq	expand_expr
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	112(%rcx), %rsi
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	cmpq	$0, 120(%rcx)
	movq	%rax, %r14
	je	.LBB47_1
# BB#2:                                 # %if.else
	movq	104(%rcx), %rdi
	cmpq	%rdi, %rbx
	je	.LBB47_4
# BB#3:                                 # %if.then15
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	cfun(%rip), %rax
	movq	(%rax), %rcx
.LBB47_4:                               # %if.end
	movq	112(%rcx), %rdi
	cmpq	%rdi, %r14
	je	.LBB47_6
# BB#5:                                 # %if.then22
	movq	%r14, %rsi
	callq	emit_move_insn
	jmp	.LBB47_6
.LBB47_1:                               # %if.then
	movq	%rbx, %rdi
	callq	copy_to_reg
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 104(%rcx)
	movq	%r14, %rdi
	callq	copy_to_reg
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 112(%rcx)
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 120(%rcx)
.LBB47_6:                               # %if.end27
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	120(%rax), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_jump  # TAILCALL
.Ltmp370:
	.size	expand_builtin_eh_return, .Ltmp370-expand_builtin_eh_return
	.cfi_endproc

	.globl	expand_eh_return
	.align	16, 0x90
	.type	expand_eh_return,@function
expand_eh_return:                       # @expand_eh_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp374:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp375:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp376:
	.cfi_def_cfa_offset 32
.Ltmp377:
	.cfi_offset %rbx, -24
.Ltmp378:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	cmpq	$0, 120(%rax)
	je	.LBB48_3
# BB#1:                                 # %if.end
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$2, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB48_4
# BB#2:                                 # %if.end4
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$128, %rcx
	movl	%ecx, 424(%rax)
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	emit_move_insn
	movq	%r14, %rdi
	callq	emit_jump
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	120(%rax), %rdi
	callq	emit_label
	callq	clobber_return_register
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	104(%rax), %rdi
	movq	112(%rax), %rsi
	callq	gen_eh_return
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_label  # TAILCALL
.LBB48_3:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB48_4:                               # %if.then3
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	error  # TAILCALL
.Ltmp379:
	.size	expand_eh_return, .Ltmp379-expand_eh_return
	.cfi_endproc

	.globl	convert_to_eh_region_ranges
	.align	16, 0x90
	.type	convert_to_eh_region_ranges,@function
convert_to_eh_region_ranges:            # @convert_to_eh_region_ranges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp386:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp387:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp388:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp389:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp390:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp391:
	.cfi_def_cfa_offset 96
.Ltmp392:
	.cfi_offset %rbx, -48
.Ltmp393:
	.cfi_offset %r12, -40
.Ltmp394:
	.cfi_offset %r13, -32
.Ltmp395:
	.cfi_offset %r14, -24
.Ltmp396:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	cmpq	$0, (%rax)
	je	.LBB49_51
# BB#1:                                 # %if.end
	movabsq	$.L.str3, %rdx
	movq	$64, %rdi
	movq	$1, %rsi
	callq	varray_init
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 80(%rcx)
	movabsq	$action_record_hash, %rsi
	movabsq	$action_record_eq, %rdx
	movabsq	$free, %rcx
	movq	$31, %rdi
	callq	htab_create
	movq	%rax, 24(%rsp)          # 8-byte Spill
	callq	get_insns
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB49_50
# BB#2:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967293, %r13       # imm = 0xFFFFFFFD
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	xorq	%r15, %r15
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB49_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB49_24 Depth 2
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB49_4
# BB#5:                                 # %if.then6
                                        #   in Loop: Header=BB49_3 Depth=1
	cmpq	$32, %rax
	movq	%r14, %rbx
	jne	.LBB49_8
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	movq	%r14, %rbx
	jne	.LBB49_8
# BB#7:                                 # %if.then16
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	8(%rax), %rax
	movq	8(%rax), %rbx
.LBB49_8:                               # %if.end23
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB49_14
# BB#9:                                 # %if.then26
                                        #   in Loop: Header=BB49_3 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB49_17
# BB#10:                                # %lor.lhs.false
                                        #   in Loop: Header=BB49_3 Depth=1
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	jne	.LBB49_12
# BB#11:                                #   in Loop: Header=BB49_3 Depth=1
	movq	%r15, %rbx
	jmp	.LBB49_46
	.align	16, 0x90
.LBB49_4:                               #   in Loop: Header=BB49_3 Depth=1
	movq	%r15, %rbx
	jmp	.LBB49_46
	.align	16, 0x90
.LBB49_14:                              # %if.else
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	jg	.LBB49_16
# BB#15:                                #   in Loop: Header=BB49_3 Depth=1
	movq	%r15, %rbx
	jmp	.LBB49_46
.LBB49_16:                              # %if.end58
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	(%rcx,%rax,8), %r12
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	collect_one_action_chain
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rdx, %rcx
	je	.LBB49_17
# BB#22:                                # %if.end69
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	cfun(%rip), %rcx
	movl	424(%rcx), %edx
	orq	$4194304, %rdx          # imm = 0x400000
	movl	%edx, 424(%rcx)
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rbx, %rbx
	testq	%rdx, %rdx
	jns	.LBB49_24
	jmp	.LBB49_25
	.align	16, 0x90
.LBB49_23:                              # %for.inc
                                        #   in Loop: Header=BB49_24 Depth=2
	movq	(%r12), %r12
.LBB49_24:                              # %for.inc
                                        #   Parent Loop BB49_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	88(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB49_23
.LBB49_25:                              #   in Loop: Header=BB49_3 Depth=1
	movq	%r13, %rdx
	movq	%rax, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB49_26
.LBB49_12:                              # %land.lhs.true32
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	32(%rbx), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB49_17
# BB#13:                                #   in Loop: Header=BB49_3 Depth=1
	movq	%r15, %rbx
	jmp	.LBB49_46
	.align	16, 0x90
.LBB49_17:                              # %if.end69.thread
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	cmpq	%rdx, %rax
	movq	$-1, %rdx
	je	.LBB49_19
# BB#18:                                # %if.end69.thread
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%r13, %rdx
.LBB49_19:                              # %if.end69.thread
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%r14, %rax
	je	.LBB49_21
# BB#20:                                # %if.end69.thread
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB49_21:                              # %if.end69.thread
                                        #   in Loop: Header=BB49_3 Depth=1
	xorq	%rbx, %rbx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rcx, %r13
.LBB49_26:                              # %if.end78
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%r13, %rax
	andq	%rcx, %rax
	movq	%rcx, %rsi
	movq	%rdx, %rcx
	andq	%rsi, %rcx
	cmpq	%rax, %rcx
	jne	.LBB49_29
# BB#27:                                # %if.end78
                                        #   in Loop: Header=BB49_3 Depth=1
	cmpq	%rbx, %r15
	jne	.LBB49_29
# BB#28:                                #   in Loop: Header=BB49_3 Depth=1
	movq	%rdx, %r13
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rbx
	jmp	.LBB49_46
.LBB49_29:                              # %if.then84
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$-1, %rdx
	jl	.LBB49_37
# BB#30:                                # %if.then87
                                        #   in Loop: Header=BB49_3 Depth=1
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB49_36
# BB#31:                                # %if.then89
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	96(%rax), %r15
	movslq	100(%rax), %rcx
	movq	88(%rax), %rax
	cmpq	%rcx, %r15
	jl	.LBB49_35
# BB#32:                                # %if.then.i
                                        #   in Loop: Header=BB49_3 Depth=1
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	movq	$64, %r12
	je	.LBB49_34
# BB#33:                                # %if.then.i
                                        #   in Loop: Header=BB49_3 Depth=1
	addq	%rcx, %rcx
	movq	%rcx, %r12
.LBB49_34:                              # %if.then.i
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%rax, %rdi
	callq	xrealloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 88(%rcx)
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movl	%r12d, 100(%rcx)
.LBB49_35:                              # %add_call_site.exit
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$4, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$0, (%rax,%rdx)
	xorq	%rcx, %rcx
	movl	%ecx, 8(%rax,%rdx)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	leaq	1(%r15), %rcx
	movl	%ecx, 96(%rax)
	movl	call_site_base(%rip), %r12d
	addq	%r15, %r12
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967210, %rdi       # imm = 0xFFFFFFAA
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	emit_note_before
	movl	%r12d, 32(%rax)
.LBB49_36:                              # %if.end94
                                        #   in Loop: Header=BB49_3 Depth=1
	movabsq	$4294967211, %rdi       # imm = 0xFFFFFFAB
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	emit_note_after
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 32(%rax)
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
.LBB49_37:                              # %if.end99
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$-1, %rax
	jl	.LBB49_45
# BB#38:                                # %if.then102
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	js	.LBB49_40
# BB#39:                                # %if.then102
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%r13, %rsi
.LBB49_40:                              # %if.then102
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	96(%rax), %r12
	movslq	100(%rax), %rcx
	movq	88(%rax), %rax
	cmpq	%rcx, %r12
	jl	.LBB49_44
# BB#41:                                # %if.then.i182
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	movq	$64, %r15
	je	.LBB49_43
# BB#42:                                # %if.then.i182
                                        #   in Loop: Header=BB49_3 Depth=1
	addq	%rcx, %rcx
	movq	%rcx, %r15
.LBB49_43:                              # %if.then.i182
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%rax, %rdi
	callq	xrealloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 88(%rcx)
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movl	%r15d, 100(%rcx)
	movq	32(%rsp), %rsi          # 8-byte Reload
.LBB49_44:                              # %add_call_site.exit191
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	$4, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	%rbx, (%rax,%rdx)
	movl	%esi, 8(%rax,%rdx)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	leaq	1(%r12), %rcx
	movl	%ecx, 96(%rax)
	movl	call_site_base(%rip), %r15d
	addq	%r12, %r15
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967210, %rdi       # imm = 0xFFFFFFAA
	movq	%r14, %rsi
	callq	emit_note_before
	movl	%r15d, 32(%rax)
.LBB49_45:                              # %for.inc113
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	%r14, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB49_46:                              # %for.inc113
                                        #   in Loop: Header=BB49_3 Depth=1
	movq	24(%r14), %r14
	testq	%r14, %r14
	movq	%rbx, %r15
	jne	.LBB49_3
# BB#47:                                # %for.end117
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	$-1, %r13
	jl	.LBB49_50
# BB#48:                                # %for.end117
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB49_50
# BB#49:                                # %if.then122
	movabsq	$4294967211, %rdi       # imm = 0xFFFFFFAB
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	emit_note_after
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 32(%rax)
.LBB49_50:                              # %if.end127
	movq	24(%rsp), %rdi          # 8-byte Reload
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	htab_delete  # TAILCALL
.LBB49_51:                              # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp397:
	.size	convert_to_eh_region_ranges, .Ltmp397-convert_to_eh_region_ranges
	.cfi_endproc

	.align	16, 0x90
	.type	action_record_hash,@function
action_record_hash:                     # @action_record_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rdi), %ecx
	movl	4(%rdi), %eax
	imulq	$1009, %rcx, %rcx       # imm = 0x3F1
	addq	%rcx, %rax
	retq
.Ltmp398:
	.size	action_record_hash, .Ltmp398-action_record_hash
	.cfi_endproc

	.align	16, 0x90
	.type	action_record_eq,@function
action_record_eq:                       # @action_record_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rdi), %ecx
	movl	4(%rsi), %edx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB51_4
# BB#1:                                 # %land.rhs
	movl	8(%rdi), %eax
	movl	8(%rsi), %ecx
	cmpq	%rcx, %rax
	jne	.LBB51_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB51_4:                               # %land.end
	retq
.LBB51_2:
	movabsq	$0, %rax
	retq
.Ltmp399:
	.size	action_record_eq, .Ltmp399-action_record_eq
	.cfi_endproc

	.align	16, 0x90
	.type	collect_one_action_chain,@function
collect_one_action_chain:               # @collect_one_action_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp406:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp407:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp408:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp409:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp410:
	.cfi_def_cfa_offset 48
.Ltmp411:
	.cfi_offset %rbx, -48
.Ltmp412:
	.cfi_offset %r12, -40
.Ltmp413:
	.cfi_offset %r13, -32
.Ltmp414:
	.cfi_offset %r14, -24
.Ltmp415:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%r14, %r14
	je	.LBB52_1
# BB#2:
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
.LBB52_3:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	40(%r14), %edx
	decq	%rdx
	andq	%rcx, %rdx
	cmpq	$5, %rdx
	ja	.LBB52_31
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB52_3 Depth=1
	jmpq	*.LJTI52_0(,%rdx,8)
.LBB52_28:                              # %sw.bb68
                                        #   in Loop: Header=BB52_3 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB52_3
# BB#29:
	movq	%rcx, %rax
	jmp	.LBB52_30
.LBB52_1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB52_30
.LBB52_10:                              # %sw.bb
	movq	(%r14), %rsi
	movq	%r15, %rdi
	callq	collect_one_action_chain
	movq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	jle	.LBB52_30
# BB#11:                                # %for.cond.preheader
	movq	(%r14), %rax
	jmp	.LBB52_12
.LBB52_5:                               # %for.cond12.preheader
	movq	56(%r14), %rbx
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	testq	%rbx, %rbx
	je	.LBB52_30
# BB#6:                                 # %for.body14.lr.ph
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB52_7:                               # %for.body14
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_23 Depth 2
	cmpq	$0, 64(%rbx)
	jne	.LBB52_18
# BB#8:                                 # %if.then17
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	72(%rbx), %rax
	movq	32(%rax), %rax
	movq	32(%rax), %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	add_action_record
	jmp	.LBB52_9
	.align	16, 0x90
.LBB52_18:                              # %if.else
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	%rax, %rcx
	andq	%r13, %rcx
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	cmpq	%rdx, %rcx
	jne	.LBB52_22
# BB#19:                                # %if.then24
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	(%r14), %rsi
	movq	%r15, %rdi
	callq	collect_one_action_chain
	movq	%rax, %rdx
	movq	%rdx, %rcx
	andq	%r13, %rcx
	xorq	%rax, %rax
	cmpq	%r13, %rcx
	je	.LBB52_22
# BB#20:                                # %if.else30
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %rax
	jg	.LBB52_22
# BB#21:                                # %if.then33
                                        #   in Loop: Header=BB52_7 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	add_action_record
	.align	16, 0x90
.LBB52_22:                              # %if.end37
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	72(%rbx), %r12
	jmp	.LBB52_23
	.align	16, 0x90
.LBB52_24:                              # %for.body43
                                        #   in Loop: Header=BB52_23 Depth=2
	movq	32(%r12), %rcx
	movq	32(%rcx), %rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	add_action_record
	movq	(%r12), %r12
.LBB52_23:                              # %if.end37
                                        #   Parent Loop BB52_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r12, %r12
	jne	.LBB52_24
.LBB52_9:                               # %for.cond12.backedge
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	56(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB52_7
	jmp	.LBB52_30
.LBB52_25:                              # %sw.bb59
	movq	(%r14), %rsi
	movq	%r15, %rdi
	callq	collect_one_action_chain
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	testq	%rsi, %rsi
	js	.LBB52_27
# BB#26:                                # %sw.bb59
	movq	%rax, %rdx
.LBB52_27:                              # %sw.bb59
	movl	56(%r14), %esi
	jmp	.LBB52_14
.LBB52_15:                              # %for.cond
                                        #   in Loop: Header=BB52_12 Depth=1
	movq	(%rax), %rax
.LBB52_12:                              # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB52_13
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB52_12 Depth=1
	movl	40(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB52_15
# BB#17:
	movq	%rdx, %rax
.LBB52_30:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB52_13:                              # %for.end
	xorq	%rsi, %rsi
.LBB52_14:                              # %for.end
	movq	%r15, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_action_record  # TAILCALL
.LBB52_31:                              # %sw.default
	movabsq	$.L.str26, %rdi
	movabsq	$.L__FUNCTION__.collect_one_action_chain, %rdx
	movq	$3549, %rsi             # imm = 0xDDD
	callq	fancy_abort
.Ltmp416:
	.size	collect_one_action_chain, .Ltmp416-collect_one_action_chain
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI52_0:
	.quad	.LBB52_10
	.quad	.LBB52_5
	.quad	.LBB52_28
	.quad	.LBB52_25
	.quad	.LBB52_30
	.quad	.LBB52_28

	.text
	.globl	output_function_exception_table
	.align	16, 0x90
	.type	output_function_exception_table,@function
output_function_exception_table:        # @output_function_exception_table
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp423:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp424:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp425:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp426:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp427:
	.cfi_def_cfa_offset 48
	subq	$288, %rsp              # imm = 0x120
.Ltmp428:
	.cfi_def_cfa_offset 336
.Ltmp429:
	.cfi_offset %rbx, -48
.Ltmp430:
	.cfi_offset %r12, -40
.Ltmp431:
	.cfi_offset %r13, -32
.Ltmp432:
	.cfi_offset %r14, -24
.Ltmp433:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movzbl	426(%rax), %eax
	testq	$64, %rax
	je	.LBB53_47
# BB#1:                                 # %if.end
	movl	current_funcdef_number(%rip), %r13d
	callq	*targetm+136(%rip)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rcx
	cmpq	$0, 8(%rcx)
	jne	.LBB53_4
# BB#2:                                 # %lor.end
	movq	72(%rax), %rax
	cmpq	$0, 8(%rax)
	je	.LBB53_3
.LBB53_4:                               # %if.else
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	movq	$155, %r12
	jne	.LBB53_6
# BB#5:                                 # %select.mid
	xorq	%r12, %r12
.LBB53_6:                               # %select.end
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str5, %rdx
	leaq	160(%rsp), %rdi
	xorq	%rax, %rax
	movq	%r13, %rcx
	callq	sprintf
	movq	%r12, %rdi
	callq	size_of_encoded_value
	movq	%rax, %rbx
	movq	$3, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	callq	assemble_align
	movq	$1, %r14
.LBB53_7:                               # %do.body9
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	asm_out_file(%rip), %rdi
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str7, %rdx
	xorq	%rax, %rax
	movq	%r13, %rcx
	callq	fprintf
	movq	$255, %rdi
	callq	eh_data_format_name
	movq	%rax, %rcx
	movabsq	$.L.str8, %rdx
	movq	$1, %rdi
	movq	$255, %rsi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	movq	%r12, %rdi
	callq	eh_data_format_name
	movq	%rax, %rcx
	movabsq	$.L.str9, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	dw2_asm_output_data
	testq	%r14, %r14
	je	.LBB53_9
# BB#8:                                 # %do.body17
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str10, %rdx
	leaq	64(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rcx
	callq	sprintf
	movabsq	$.L.str11, %rdx
	leaq	160(%rsp), %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	dw2_asm_output_delta_uleb128
	movq	asm_out_file(%rip), %rdi
	movq	%rbx, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB53_9:                               # %if.end25
	movq	$1, %rdi
	callq	eh_data_format_name
	movq	%rax, %rcx
	movabsq	$.L.str13, %rdx
	movq	$1, %rdi
	movq	$1, %rsi
	xorq	%rax, %rax
	movq	%r13, %r14
	callq	dw2_asm_output_data
	movabsq	$.L.str4, %r15
	movabsq	$.L.str14, %rdx
	leaq	128(%rsp), %r12
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rcx
	callq	sprintf
	movabsq	$.L.str15, %rdx
	leaq	96(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rcx
	callq	sprintf
	movabsq	$.L.str16, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	dw2_asm_output_delta_uleb128
	movq	asm_out_file(%rip), %rdi
	movq	%r12, %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movslq	96(%rax), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	movq	$1, %rdx
	jle	.LBB53_18
# BB#10:
	movq	current_function_func_begin_label(%rip), %rcx
	movq	32(%rcx), %r15
	movq	$8, %rbx
	movabsq	$.L.str4, %r12
	jmp	.LBB53_11
	.align	16, 0x90
.LBB53_17:                              # %if.end20.for.body_crit_edge.i
                                        #   in Loop: Header=BB53_11 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	incq	%rdx
	addq	$16, %rbx
.LBB53_11:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movl	call_site_base(%rip), %ecx
	movq	88(%rax), %r14
	leaq	-1(%rdx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	leaq	-1(%rcx,%rdx), %rcx
	xorq	%rax, %rax
	leaq	256(%rsp), %rdi
	movq	%r12, %r13
	movq	%r13, %rsi
	movq	%rdx, %r12
	movabsq	$.L.str19, %rdx
	callq	sprintf
	movl	call_site_base(%rip), %eax
	leaq	-1(%rax,%r12), %rcx
	xorq	%rax, %rax
	leaq	224(%rsp), %rdi
	movq	%r13, %rsi
	movq	%r13, %r12
	movabsq	$.L.str20, %rdx
	callq	sprintf
	movq	-8(%r14,%rbx), %rax
	movq	%rbx, %r13
	testq	%rax, %rax
	je	.LBB53_13
# BB#12:                                # %do.body7.i
                                        #   in Loop: Header=BB53_11 Depth=1
	movl	48(%rax), %ecx
	xorq	%rax, %rax
	leaq	192(%rsp), %rdi
	movq	%r12, %rsi
	movabsq	$.L.str21, %rdx
	callq	sprintf
.LBB53_13:                              # %if.end.i
                                        #   in Loop: Header=BB53_11 Depth=1
	xorq	%rax, %rax
	leaq	256(%rsp), %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str22, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	callq	dw2_asm_output_delta_uleb128
	xorq	%rax, %rax
	leaq	224(%rsp), %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str23, %rdx
	callq	dw2_asm_output_delta_uleb128
	movq	%r13, %rbx
	cmpq	$0, -8(%r14,%rbx)
	jne	.LBB53_14
# BB#15:                                # %if.else.i
                                        #   in Loop: Header=BB53_11 Depth=1
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str24, %rsi
	callq	dw2_asm_output_data_uleb128
	jmp	.LBB53_16
	.align	16, 0x90
.LBB53_14:                              # %if.then18.i
                                        #   in Loop: Header=BB53_11 Depth=1
	xorq	%rax, %rax
	leaq	192(%rsp), %rdi
	movq	%r15, %rsi
	movabsq	$.L.str24, %rdx
	callq	dw2_asm_output_delta_uleb128
.LBB53_16:                              # %if.end20.i
                                        #   in Loop: Header=BB53_11 Depth=1
	movslq	(%r14,%rbx), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str25, %rsi
	callq	dw2_asm_output_data_uleb128
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	andq	%rsi, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%rsi, %rcx
	cmpq	%rcx, %rax
	jne	.LBB53_17
.LBB53_18:                              # %dw2_output_call_site_table.exit
	movl	call_site_base(%rip), %eax
	movq	asm_out_file(%rip), %rdi
	movq	40(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	movl	%eax, call_site_base(%rip)
	leaq	96(%rsp), %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rax
	movq	8(%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rbx
	jle	.LBB53_24
# BB#19:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r14
	movabsq	$.L.str17, %r15
	jmp	.LBB53_20
	.align	16, 0x90
.LBB53_23:                              # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB53_20 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rax
.LBB53_20:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	32(%rax,%rbx), %esi
	testq	%r12, %rbx
	movabsq	$0, %rdx
	jne	.LBB53_22
# BB#21:                                # %for.body
                                        #   in Loop: Header=BB53_20 Depth=1
	movq	%r15, %rdx
.LBB53_22:                              # %for.body
                                        #   in Loop: Header=BB53_20 Depth=1
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	incq	%rbx
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	%r14, %rax
	jne	.LBB53_23
.LBB53_24:                              # %for.end
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
	je	.LBB53_26
# BB#25:                                # %if.then53
	movq	$3, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	callq	assemble_align
.LBB53_26:                              # %if.end55
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rax
	movq	8(%rax), %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB53_38
# BB#27:                                # %while.body.lr.ph
	movq	$3, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	testq	%r15, %r15
	je	.LBB53_33
# BB#28:                                # %while.body.preheader
	movabsq	$global_trees+88, %r14
	jmp	.LBB53_29
	.align	16, 0x90
.LBB53_32:                              # %while.cond.backedge.while.body_crit_edge
                                        #   in Loop: Header=BB53_29 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rax
	decq	%rbx
.LBB53_29:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rbx,8), %rsi
	testq	%rsi, %rsi
	movq	%r14, %rax
	je	.LBB53_31
# BB#30:                                # %if.else70
                                        #   in Loop: Header=BB53_29 Depth=1
	movq	type_to_runtime_map(%rip), %rdi
	movq	%rsi, %rdx
	andq	$262143, %rdx           # imm = 0x3FFFF
	xorq	%rcx, %rcx
	callq	htab_find_slot_with_hash
	movq	(%rax), %rax
	addq	$32, %rax
.LBB53_31:                              # %while.cond.backedge
                                        #   in Loop: Header=BB53_29 Depth=1
	movq	(%rax), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$3, %rcx
	callq	expand_expr
	movq	%rax, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	dw2_asm_output_encoded_addr_rtx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB53_32
	jmp	.LBB53_38
.LBB53_33:                              # %while.body.us.preheader
	movabsq	$global_trees+88, %r15
	jmp	.LBB53_34
	.align	16, 0x90
.LBB53_37:                              # %while.cond.backedge.us.while.body.us_crit_edge
                                        #   in Loop: Header=BB53_34 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rax
	decq	%rbx
.LBB53_34:                              # %while.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rbx,8), %rsi
	testq	%rsi, %rsi
	movq	%r15, %rax
	je	.LBB53_36
# BB#35:                                # %if.else70.us
                                        #   in Loop: Header=BB53_34 Depth=1
	movq	type_to_runtime_map(%rip), %rdi
	movq	%rsi, %rdx
	andq	$262143, %rdx           # imm = 0x3FFFF
	xorq	%rcx, %rcx
	callq	htab_find_slot_with_hash
	movq	(%rax), %rax
	addq	$32, %rax
.LBB53_36:                              # %while.cond.backedge.us
                                        #   in Loop: Header=BB53_34 Depth=1
	movq	(%rax), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$3, %rcx
	callq	expand_expr
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	assemble_integer
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB53_37
.LBB53_38:                              # %while.end
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB53_40
# BB#39:                                # %if.then84
	movq	asm_out_file(%rip), %rdi
	leaq	160(%rsp), %rsi
	callq	assemble_name
	movq	asm_out_file(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB53_40:                              # %if.end87
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	72(%rax), %rax
	movq	8(%rax), %r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB53_46
# BB#41:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r14
	movabsq	$.L.str18, %r12
	jmp	.LBB53_42
	.align	16, 0x90
.LBB53_45:                              # %for.body95.for.body95_crit_edge
                                        #   in Loop: Header=BB53_42 Depth=1
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	72(%rax), %rax
.LBB53_42:                              # %for.body95
                                        # =>This Inner Loop Header: Depth=1
	movzbl	32(%rax,%rbx), %esi
	testq	%r15, %rbx
	movabsq	$0, %rdx
	jne	.LBB53_44
# BB#43:                                # %for.body95
                                        #   in Loop: Header=BB53_42 Depth=1
	movq	%r12, %rdx
.LBB53_44:                              # %for.body95
                                        #   in Loop: Header=BB53_42 Depth=1
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	dw2_asm_output_data
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%r14, %rax
	jne	.LBB53_45
.LBB53_46:                              # %for.end107
	movq	current_function_decl(%rip), %rdi
	callq	function_section
.LBB53_47:                              # %return
	addq	$288, %rsp              # imm = 0x120
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB53_3:
	xorq	%r14, %r14
	movq	$255, %r12
	xorq	%rbx, %rbx
	jmp	.LBB53_7
.Ltmp434:
	.size	output_function_exception_table, .Ltmp434-output_function_exception_table
	.cfi_endproc

	.align	16, 0x90
	.type	add_action_record,@function
add_action_record:                      # @add_action_record
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp441:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp442:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp443:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp444:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp445:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp446:
	.cfi_def_cfa_offset 80
.Ltmp447:
	.cfi_offset %rbx, -48
.Ltmp448:
	.cfi_offset %r12, -40
.Ltmp449:
	.cfi_offset %r13, -32
.Ltmp450:
	.cfi_offset %r14, -24
.Ltmp451:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movl	%r14d, 20(%rsp)
	movl	%ebx, 24(%rsp)
	leaq	16(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %r12
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB54_25
# BB#1:                                 # %if.then
	movq	$12, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	8(%rcx), %rcx
	incq	%rcx
	movl	%ecx, (%rax)
	movl	%r14d, 4(%rax)
	movl	%ebx, 8(%rax)
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	%rax, (%r12)
	movq	cfun(%rip), %rax
	movq	(%rax), %rdi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB54_2:                               # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	%r14, %r15
	andq	$127, %r15
	movq	$39, %rcx
	sarq	%cl, %rbx
	testq	%rdx, %rbx
	jne	.LBB54_4
# BB#3:                                 # %do.body.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	%r14, %rax
	andq	$64, %rax
	xorq	%r12, %r12
	testq	$255, %rax
	je	.LBB54_7
.LBB54_4:                               # %lor.rhs.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	$1, %r12
	cmpq	%rdx, %rax
	jne	.LBB54_7
# BB#5:                                 # %land.rhs.i
                                        #   in Loop: Header=BB54_2 Depth=1
	testq	$64, %r14
	movabsq	$0, %r12
	jne	.LBB54_7
# BB#6:                                 # %land.rhs.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movabsq	$1, %r12
	.align	16, 0x90
.LBB54_7:                               # %lor.end.i
                                        #   in Loop: Header=BB54_2 Depth=1
	testq	%r12, %r12
	je	.LBB54_9
# BB#8:                                 #   in Loop: Header=BB54_2 Depth=1
	orq	$-128, %r14
	movq	%r14, %r15
.LBB54_9:                               # %lor.end.i
                                        #   in Loop: Header=BB54_2 Depth=1
	movq	80(%rdi), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	movq	%rdx, %r13
	cmpq	%rsi, %rcx
	jb	.LBB54_11
# BB#10:                                # %if.then17.i
                                        #   in Loop: Header=BB54_2 Depth=1
	addq	%rsi, %rsi
	movq	%rdi, %r14
	movq	%rax, %rdi
	callq	varray_grow
	movq	%r14, %rdi
	movq	%rax, 80(%rdi)
	movq	8(%rax), %rcx
.LBB54_11:                              # %if.end19.i
                                        #   in Loop: Header=BB54_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movb	%r15b, 32(%rax,%rcx)
	testq	%r12, %r12
	movq	%rbx, %r14
	movq	%r13, %rdx
	jne	.LBB54_2
# BB#12:                                # %push_sleb128.exit
	movq	cfun(%rip), %rax
	movq	(%rax), %r14
	xorq	%rax, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	testq	%rdx, %rcx
	je	.LBB54_14
# BB#13:                                # %if.then8
	movq	80(%r14), %rax
	movq	8(%rax), %rax
	andq	%rdx, %rcx
	addq	%rdx, %rcx
	subq	%rax, %rcx
	movq	%rcx, %rax
	.align	16, 0x90
.LBB54_14:                              # %do.body.i39
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	%rax, %r13
	andq	$127, %r13
	movq	$39, %rcx
	sarq	%cl, %rbx
	testq	%rdx, %rbx
	jne	.LBB54_16
# BB#15:                                # %do.body.i39
                                        #   in Loop: Header=BB54_14 Depth=1
	movq	%rax, %rcx
	andq	$64, %rcx
	xorq	%r12, %r12
	testq	$255, %rcx
	je	.LBB54_19
.LBB54_16:                              # %lor.rhs.i41
                                        #   in Loop: Header=BB54_14 Depth=1
	movq	%rbx, %rcx
	andq	%rdx, %rcx
	movq	$1, %r12
	cmpq	%rdx, %rcx
	jne	.LBB54_19
# BB#17:                                # %land.rhs.i44
                                        #   in Loop: Header=BB54_14 Depth=1
	testq	$64, %rax
	movabsq	$0, %r12
	jne	.LBB54_19
# BB#18:                                # %land.rhs.i44
                                        #   in Loop: Header=BB54_14 Depth=1
	movabsq	$1, %r12
	.align	16, 0x90
.LBB54_19:                              # %lor.end.i50
                                        #   in Loop: Header=BB54_14 Depth=1
	testq	%r12, %r12
	je	.LBB54_21
# BB#20:                                #   in Loop: Header=BB54_14 Depth=1
	orq	$-128, %rax
	movq	%rax, %r13
.LBB54_21:                              # %lor.end.i50
                                        #   in Loop: Header=BB54_14 Depth=1
	movq	%rdx, %r15
	movq	80(%r14), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB54_23
# BB#22:                                # %if.then17.i55
                                        #   in Loop: Header=BB54_14 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, 80(%r14)
	movq	8(%rax), %rcx
.LBB54_23:                              # %if.end19.i61
                                        #   in Loop: Header=BB54_14 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movb	%r13b, 32(%rax,%rcx)
	testq	%r12, %r12
	movq	%rbx, %rax
	movq	%r15, %rdx
	jne	.LBB54_14
# BB#24:
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB54_25:                              # %if.end17
	movl	(%rax), %eax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp452:
	.size	add_action_record, .Ltmp452-add_action_record
	.cfi_endproc

	.align	16, 0x90
	.type	ttypes_filter_hash,@function
ttypes_filter_hash:                     # @ttypes_filter_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	andq	$262143, %rax           # imm = 0x3FFFF
	retq
.Ltmp453:
	.size	ttypes_filter_hash, .Ltmp453-ttypes_filter_hash
	.cfi_endproc

	.align	16, 0x90
	.type	ttypes_filter_eq,@function
ttypes_filter_eq:                       # @ttypes_filter_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	cmpq	%rsi, %rax
	jne	.LBB56_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB56_1:
	movabsq	$0, %rax
	retq
.Ltmp454:
	.size	ttypes_filter_eq, .Ltmp454-ttypes_filter_eq
	.cfi_endproc

	.align	16, 0x90
	.type	ehspec_filter_hash,@function
ehspec_filter_hash:                     # @ehspec_filter_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB57_3
# BB#1:
	movabsq	$4160749568, %r8        # imm = 0xF8000000
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB57_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$5, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	andq	%r8, %rax
	movq	$27, %rcx
	shrq	%cl, %rax
	movq	32(%rdx), %rcx
	orq	%rsi, %rax
	movq	(%rdx), %rdx
	andq	%rdi, %rax
	andq	$262143, %rcx           # imm = 0x3FFFF
	addq	%rcx, %rax
	testq	%rdx, %rdx
	jne	.LBB57_2
.LBB57_3:                               # %for.end
	retq
.Ltmp455:
	.size	ehspec_filter_hash, .Ltmp455-ehspec_filter_hash
	.cfi_endproc

	.align	16, 0x90
	.type	ehspec_filter_eq,@function
ehspec_filter_eq:                       # @ehspec_filter_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	movq	(%rsi), %rsi
	jmp	type_list_equal  # TAILCALL
.Ltmp456:
	.size	ehspec_filter_eq, .Ltmp456-ehspec_filter_eq
	.cfi_endproc

	.align	16, 0x90
	.type	add_ttypes_entry,@function
add_ttypes_entry:                       # @add_ttypes_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp461:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp462:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp463:
	.cfi_def_cfa_offset 32
.Ltmp464:
	.cfi_offset %rbx, -32
.Ltmp465:
	.cfi_offset %r14, -24
.Ltmp466:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%r14, %rdx
	andq	$262143, %rdx           # imm = 0x3FFFF
	movq	$1, %rcx
                                        # kill: RSI<def> R14<kill>
	callq	htab_find_slot_with_hash
	movq	%rax, %r15
	movq	(%r15), %rbx
	testq	%rbx, %rbx
	jne	.LBB59_4
# BB#1:                                 # %if.then
	movq	$16, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	%rax, %rbx
	movq	%r14, (%rbx)
	movq	(%rcx), %rax
	movq	64(%rax), %rax
	movq	8(%rax), %rax
	incq	%rax
	movl	%eax, 8(%rbx)
	movq	%rbx, (%r15)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	movq	(%rdi), %rsi
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	jb	.LBB59_3
# BB#2:                                 # %if.then11
	addq	%rsi, %rsi
	callq	varray_grow
	movq	cfun(%rip), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 64(%rcx)
	movq	cfun(%rip), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	movq	8(%rdi), %rax
.LBB59_3:                               # %if.end
	leaq	1(%rax), %rcx
	movq	%rcx, 8(%rdi)
	movq	%r14, 32(%rdi,%rax,8)
.LBB59_4:                               # %if.end25
	movl	8(%rbx), %eax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp467:
	.size	add_ttypes_entry, .Ltmp467-add_ttypes_entry
	.cfi_endproc

	.align	16, 0x90
	.type	t2r_mark_1,@function
t2r_mark_1:                             # @t2r_mark_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp470:
	.cfi_def_cfa_offset 16
.Ltmp471:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB60_5
# BB#1:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB60_5
# BB#2:                                 # %do.body1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB60_4
# BB#3:                                 # %if.then3
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB60_4:                               # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB60_5:                               # %do.end9
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp472:
	.size	t2r_mark_1, .Ltmp472-t2r_mark_1
	.cfi_endproc

	.align	16, 0x90
	.type	mark_ehl_map_entry,@function
mark_ehl_map_entry:                     # @mark_ehl_map_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp475:
	.cfi_def_cfa_offset 16
.Ltmp476:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB61_3
# BB#1:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB61_3
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB61_3:                               # %do.end
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp477:
	.size	mark_ehl_map_entry, .Ltmp477-mark_ehl_map_entry
	.cfi_endproc

	.type	doing_eh.warned,@object # @doing_eh.warned
	.local	doing_eh.warned
	.comm	doing_eh.warned,1,1
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"exception handling disabled, use -fexceptions to enable"
	.size	.L.str, 56

	.type	exception_handler_label_map,@object # @exception_handler_label_map
	.local	exception_handler_label_map
	.comm	exception_handler_label_map,8,8
	.type	type_to_runtime_map,@object # @type_to_runtime_map
	.local	type_to_runtime_map
	.comm	type_to_runtime_map,8,8
	.type	lang_protect_cleanup_actions,@object # @lang_protect_cleanup_actions
	.comm	lang_protect_cleanup_actions,8,8
	.type	flag_non_call_exceptions,@object # @flag_non_call_exceptions
	.comm	flag_non_call_exceptions,4,4
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"argument of `__builtin_eh_return_regno' must be constant"
	.size	.L.str1, 57

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"__builtin_eh_return not supported on this target"
	.size	.L.str2, 49

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"action_record_data"
	.size	.L.str3, 19

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"*.%s%u"
	.size	.L.str4, 7

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"LLSDATT"
	.size	.L.str5, 8

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	".%s%u:\n"
	.size	.L.str6, 8

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"LLSDA"
	.size	.L.str7, 6

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"@LPStart format (%s)"
	.size	.L.str8, 21

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"@TType format (%s)"
	.size	.L.str9, 19

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"LLSDATTD"
	.size	.L.str10, 9

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"@TType base offset"
	.size	.L.str11, 19

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	":\n"
	.size	.L.str12, 3

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"call-site format (%s)"
	.size	.L.str13, 22

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"LLSDACSB"
	.size	.L.str14, 9

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"LLSDACSE"
	.size	.L.str15, 9

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Call-site table length"
	.size	.L.str16, 23

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Action record table"
	.size	.L.str17, 20

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Exception specification table"
	.size	.L.str18, 30

	.type	lang_eh_type_covers,@object # @lang_eh_type_covers
	.comm	lang_eh_type_covers,8,8
	.type	lang_eh_runtime_type,@object # @lang_eh_runtime_type
	.comm	lang_eh_runtime_type,8,8
	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"LEHB"
	.size	.L.str19, 5

	.type	call_site_base,@object  # @call_site_base
	.local	call_site_base
	.comm	call_site_base,4,4
	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"LEHE"
	.size	.L.str20, 5

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"L"
	.size	.L.str21, 2

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"region %d start"
	.size	.L.str22, 16

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"length"
	.size	.L.str23, 7

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"landing pad"
	.size	.L.str24, 12

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"action"
	.size	.L.str25, 7

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"except.c"
	.size	.L.str26, 9

	.type	.L__FUNCTION__.collect_one_action_chain,@object # @__FUNCTION__.collect_one_action_chain
.L__FUNCTION__.collect_one_action_chain:
	.asciz	"collect_one_action_chain"
	.size	.L__FUNCTION__.collect_one_action_chain, 25

	.type	.L__FUNCTION__.reachable_next_level,@object # @__FUNCTION__.reachable_next_level
.L__FUNCTION__.reachable_next_level:
	.asciz	"reachable_next_level"
	.size	.L__FUNCTION__.reachable_next_level, 21

	.type	.L__FUNCTION__.remove_eh_handler,@object # @__FUNCTION__.remove_eh_handler
.L__FUNCTION__.remove_eh_handler:
	.asciz	"remove_eh_handler"
	.size	.L__FUNCTION__.remove_eh_handler, 18

	.type	.L__FUNCTION__.remove_exception_handler_label,@object # @__FUNCTION__.remove_exception_handler_label
.L__FUNCTION__.remove_exception_handler_label:
	.asciz	"remove_exception_handler_label"
	.size	.L__FUNCTION__.remove_exception_handler_label, 31

	.type	.L.str27,@object        # @.str27
.L.str27:
	.zero	1
	.size	.L.str27, 1

	.type	.L__FUNCTION__.build_post_landing_pads,@object # @__FUNCTION__.build_post_landing_pads
.L__FUNCTION__.build_post_landing_pads:
	.asciz	"build_post_landing_pads"
	.size	.L__FUNCTION__.build_post_landing_pads, 24

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"ttype_data"
	.size	.L.str28, 11

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"ehspec_data"
	.size	.L.str29, 12

	.type	.L__FUNCTION__.duplicate_eh_region_1,@object # @__FUNCTION__.duplicate_eh_region_1
.L__FUNCTION__.duplicate_eh_region_1:
	.asciz	"duplicate_eh_region_1"
	.size	.L__FUNCTION__.duplicate_eh_region_1, 22

	.type	.L__FUNCTION__.add_ehl_entry,@object # @__FUNCTION__.add_ehl_entry
.L__FUNCTION__.add_ehl_entry:
	.asciz	"add_ehl_entry"
	.size	.L__FUNCTION__.add_ehl_entry, 14

	.type	.L__FUNCTION__.remove_unreachable_regions,@object # @__FUNCTION__.remove_unreachable_regions
.L__FUNCTION__.remove_unreachable_regions:
	.asciz	"remove_unreachable_regions"
	.size	.L__FUNCTION__.remove_unreachable_regions, 27

	.type	.L__FUNCTION__.convert_from_eh_region_ranges_1,@object # @__FUNCTION__.convert_from_eh_region_ranges_1
.L__FUNCTION__.convert_from_eh_region_ranges_1:
	.asciz	"convert_from_eh_region_ranges_1"
	.size	.L__FUNCTION__.convert_from_eh_region_ranges_1, 32

	.type	.L__FUNCTION__.resolve_fixup_regions,@object # @__FUNCTION__.resolve_fixup_regions
.L__FUNCTION__.resolve_fixup_regions:
	.asciz	"resolve_fixup_regions"
	.size	.L__FUNCTION__.resolve_fixup_regions, 22

	.type	.L__FUNCTION__.mark_eh_region,@object # @__FUNCTION__.mark_eh_region
.L__FUNCTION__.mark_eh_region:
	.asciz	"mark_eh_region"
	.size	.L__FUNCTION__.mark_eh_region, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
