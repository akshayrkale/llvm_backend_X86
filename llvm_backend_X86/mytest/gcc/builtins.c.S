	.file	"builtins.c.bc"
	.text
	.globl	expand_builtin_return_addr
	.align	16, 0x90
	.type	expand_builtin_return_addr,@function
expand_builtin_return_addr:             # @expand_builtin_return_addr
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
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
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
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_1
# BB#2:                                 # %for.body.lr.ph
	callq	ix86_setup_frame_addresses
	xorq	%r12, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r14, 8(%rsp)           # 8-byte Spill
	andq	%r13, %r14
	.align	16, 0x90
.LBB0_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rbx, %rsi
	callq	memory_address
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	callq	get_frame_alias_set
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	%rbx, %rdi
	callq	copy_to_reg
	incq	%r12
	movq	%rax, %rbx
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	%r14, %rax
	jne	.LBB0_3
	jmp	.LBB0_4
.LBB0_1:
	movq	%r14, 8(%rsp)           # 8-byte Spill
.LBB0_4:                                # %for.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	cmpq	$52, %r15
	jne	.LBB0_5
# BB#9:                                 # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_5:                                # %if.end10
	movl	target_flags(%rip), %esi
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$25, %rcx
	movq	%rsi, %r15
	shrq	%cl, %r15
	orq	$4, %r15
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB0_6
# BB#7:                                 # %cond.false
	movabsq	$4286578688, %rcx       # imm = 0xFF800000
	andq	%rcx, %rsi
	movq	$23, %rcx
	shrq	%cl, %rsi
	addq	$4, %rsi
	andq	%rax, %rsi
	movq	%rbx, %rdi
	jmp	.LBB0_8
.LBB0_6:                                # %cond.true
	movq	global_rtl+40(%rip), %rdi
	movabsq	$4286578688, %rax       # imm = 0xFF800000
	andq	%rax, %rsi
	movq	$23, %rcx
	shrq	%cl, %rsi
	movq	$-4, %rax
	subq	%rsi, %rax
	movq	%rax, %rsi
.LBB0_8:                                # %cond.false
	callq	plus_constant_wide
	movq	%r15, %rdi
	movq	%rax, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_MEM  # TAILCALL
.Ltmp17:
	.size	expand_builtin_return_addr, .Ltmp17-expand_builtin_return_addr
	.cfi_endproc

	.globl	expand_builtin_setjmp_setup
	.align	16, 0x90
	.type	expand_builtin_setjmp_setup,@function
expand_builtin_setjmp_setup:            # @expand_builtin_setjmp_setup
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
	subq	$16, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 64
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
	movl	target_flags(%rip), %ebx
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$25, %rcx
	movq	%rbx, %r12
	movq	setjmp_alias_set(%rip), %rax
	shrq	%cl, %r12
	andq	$1, %r12
	orq	$4, %r12
	cmpq	$-1, %rax
	jne	.LBB1_2
# BB#1:                                 # %if.then
	callq	new_alias_set
	movl	target_flags(%rip), %ebx
	movq	%rax, setjmp_alias_set(%rip)
.LBB1_2:                                # %if.end
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	force_operand
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%rax, %r15
	callq	emit_queue
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movq	setjmp_alias_set(%rip), %rsi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	set_mem_alias_set
	movq	global_rtl+56(%rip), %rsi
	movq	%rbx, %rdi
	callq	emit_move_insn
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movzbl	mode_size+4(%rbx), %esi
	orq	$4, %rbx
	movq	%r15, %rdi
	callq	plus_constant_wide
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	setjmp_alias_set(%rip), %rsi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	set_mem_alias_set
	movq	%rbx, %rdi
	callq	validize_mem
	movl	target_flags(%rip), %ebx
	movq	%rax, %r13
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$67, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_u00
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movzbl	mode_size+4(%rax), %esi
	addq	%rsi, %rsi
	movq	%r15, %rdi
	callq	plus_constant_wide
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	setjmp_alias_set(%rip), %rsi
	movq	%rax, 8(%rsp)
	movq	%rax, %rdi
	callq	set_mem_alias_set
	leaq	8(%rsp), %rsi
	movq	$2, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_save
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$16, %rcx
	movl	%ecx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 424(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	expand_builtin_setjmp_setup, .Ltmp35-expand_builtin_setjmp_setup
	.cfi_endproc

	.globl	expand_builtin_setjmp_receiver
	.align	16, 0x90
	.type	expand_builtin_setjmp_receiver,@function
expand_builtin_setjmp_receiver:         # @expand_builtin_setjmp_receiver
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 16
.Ltmp39:
	.cfi_offset %rbx, -16
	movq	global_rtl+32(%rip), %rdx
	movq	%rdi, %rbx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	static_chain_rtx(%rip), %rdx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	global_rtl+56(%rip), %rdi
	movq	global_rtl+32(%rip), %rsi
	callq	emit_move_insn
	movzbl	fixed_regs+16(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_3
# BB#1:
	movq	$2, %rax
	.align	16, 0x90
.LBB2_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	decq	%rax
	jne	.LBB2_2
.LBB2_3:                                # %if.end15
	movl	target_flags(%rip), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	jne	.LBB2_6
# BB#4:                                 # %if.end15
	movl	flag_pic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_6
# BB#5:                                 # %if.then19
	movq	%rbx, %rdi
	callq	gen_builtin_setjmp_receiver
	movq	%rax, %rdi
	callq	emit_insn
.LBB2_6:                                # %if.end22
	movabsq	$.L.str99, %rdx
	movq	$40, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_s
	movq	%rax, %rdi
	popq	%rbx
	jmp	emit_insn  # TAILCALL
.Ltmp40:
	.size	expand_builtin_setjmp_receiver, .Ltmp40-expand_builtin_setjmp_receiver
	.cfi_endproc

	.globl	expand_builtin_longjmp
	.align	16, 0x90
	.type	expand_builtin_longjmp,@function
expand_builtin_longjmp:                 # @expand_builtin_longjmp
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
	movl	target_flags(%rip), %edx
	movq	setjmp_alias_set(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$25, %rcx
	movq	%rdx, %r12
	shrq	%cl, %r12
	cmpq	$-1, %rax
	jne	.LBB3_2
# BB#1:                                 # %if.then
	callq	new_alias_set
	movl	target_flags(%rip), %edx
	movq	%rax, setjmp_alias_set(%rip)
.LBB3_2:                                # %if.end
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	%rdx, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rax, %r15
	cmpq	%r14, %rcx
	jne	.LBB3_9
# BB#3:                                 # %if.end7
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$32, %rcx
	movl	%ecx, 424(%rax)
	movl	target_flags(%rip), %edi
	andq	$1, %r12
	orq	$4, %r12
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movzbl	mode_size+4(%rbx), %esi
	movq	%rax, %r14
	orq	$4, %rbx
	movq	%r15, %rdi
	callq	plus_constant_wide
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movzbl	mode_size+4(%rdx), %esi
	movq	%rax, %rbx
	addq	%rsi, %rsi
	movq	%r15, %rdi
	callq	plus_constant_wide
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	setjmp_alias_set(%rip), %rsi
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	set_mem_alias_set
	movq	setjmp_alias_set(%rip), %rsi
	movq	%rbx, %rdi
	callq	set_mem_alias_set
	movq	setjmp_alias_set(%rip), %rsi
	movq	%r15, %rdi
	callq	set_mem_alias_set
	movq	%rbx, %rdi
	callq	copy_to_reg
	movq	global_rtl+32(%rip), %rdi
	movq	%rax, %rbx
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	$2, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	emit_stack_restore
	movq	global_rtl+32(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	global_rtl+16(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rbx, %rdi
	callq	emit_indirect_jump
	callq	get_last_insn
	movq	%rax, %rbx
	jmp	.LBB3_4
	.align	16, 0x90
.LBB3_10:                               # %for.inc
                                        #   in Loop: Header=BB3_4 Depth=1
	movq	16(%rbx), %rbx
.LBB3_4:                                # %if.end7
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB3_8
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB3_4 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB3_8
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB3_4 Depth=1
	cmpq	$33, %rax
	jne	.LBB3_10
# BB#7:                                 # %if.then41
	movq	const_int_rtx+512(%rip), %rsi
	movq	56(%rbx), %rdx
	movq	$27, %rdi
	callq	alloc_EXPR_LIST
	movq	%rax, 56(%rbx)
.LBB3_8:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB3_9:                                # %if.then6
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin_longjmp, %rcx
	movq	$661, %rsi              # imm = 0x295
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp55:
	.size	expand_builtin_longjmp, .Ltmp55-expand_builtin_longjmp
	.cfi_endproc

	.globl	apply_args_register_offset
	.align	16, 0x90
	.type	apply_args_register_offset,@function
apply_args_register_offset:             # @apply_args_register_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp58:
	.cfi_def_cfa_offset 16
.Ltmp59:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	apply_args_size
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	apply_args_reg_offset(,%rbx,4), %eax
	popq	%rbx
	retq
.Ltmp60:
	.size	apply_args_register_offset, .Ltmp60-apply_args_register_offset
	.cfi_endproc

	.align	16, 0x90
	.type	apply_args_size,@function
apply_args_size:                        # @apply_args_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp67:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp68:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp69:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp70:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp71:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp72:
	.cfi_def_cfa_offset 80
.Ltmp73:
	.cfi_offset %rbx, -48
.Ltmp74:
	.cfi_offset %r12, -40
.Ltmp75:
	.cfi_offset %r13, -32
.Ltmp76:
	.cfi_offset %r14, -24
.Ltmp77:
	.cfi_offset %r15, -16
	movslq	apply_args_size.size(%rip), %rax
	testq	%rax, %rax
	jns	.LBB5_54
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movzbl	mode_size+4(%rax), %eax
	cmpq	$0, struct_value_rtx(%rip)
	movl	%eax, apply_args_size.size(%rip)
	je	.LBB5_3
# BB#2:                                 # %if.then2
	addq	%rax, %rax
	movl	%eax, apply_args_size.size(%rip)
.LBB5_3:                                # %for.cond.preheader
	xorq	%r12, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	.align	16, 0x90
.LBB5_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_19 Depth 2
                                        #     Child Loop BB5_13 Depth 2
                                        #     Child Loop BB5_31 Depth 2
                                        #     Child Loop BB5_37 Depth 2
                                        #     Child Loop BB5_43 Depth 2
	movq	%r15, %rdi
	callq	ix86_function_arg_regno_p
	testq	%rax, %rax
	je	.LBB5_51
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB5_4 Depth=1
	movl	class_narrowest_mode+4(%rip), %eax
	testq	%r13, %rax
	je	.LBB5_30
# BB#6:                                 # %for.body15.lr.ph
                                        #   in Loop: Header=BB5_4 Depth=1
	movq	%r15, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	xorq	%r14, %r14
	cmpq	$8, %rax
	je	.LBB5_7
# BB#8:                                 # %for.body15.lr.ph
                                        #   in Loop: Header=BB5_4 Depth=1
	leaq	-21(%r15), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB5_9
# BB#10:                                # %for.body15.lr.ph
                                        #   in Loop: Header=BB5_4 Depth=1
	leaq	-45(%r15), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB5_11
# BB#12:                                # %for.body15.lr.ph
                                        #   in Loop: Header=BB5_4 Depth=1
	leaq	-29(%r15), %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	movl	class_narrowest_mode+4(%rip), %eax
	movq	%rax, %rbx
	movq	%rax, %r12
	jbe	.LBB5_13
	.align	16, 0x90
.LBB5_19:                               # %for.body15
                                        #   Parent Loop BB5_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB5_28
# BB#20:                                # %cond.false
                                        #   in Loop: Header=BB5_19 Depth=2
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	$24, %rax
	jne	.LBB5_21
# BB#23:                                # %cond.true58
                                        #   in Loop: Header=BB5_19 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB5_25
	.align	16, 0x90
.LBB5_21:                               # %cond.false
                                        #   in Loop: Header=BB5_19 Depth=2
	cmpq	$18, %rax
	jne	.LBB5_24
# BB#22:                                # %cond.true51
                                        #   in Loop: Header=BB5_19 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB5_25
.LBB5_24:                               # %cond.false62
                                        #   in Loop: Header=BB5_19 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	target_flags(%rip), %esi
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB5_25:                               # %cond.end76
                                        #   in Loop: Header=BB5_19 Depth=2
	andq	%r13, %rax
	cmpq	$1, %rax
	movq	%r12, %rax
	je	.LBB5_27
# BB#26:                                # %cond.end76
                                        #   in Loop: Header=BB5_19 Depth=2
	movq	%r14, %rax
.LBB5_27:                               # %cond.end76
                                        #   in Loop: Header=BB5_19 Depth=2
	movq	%rax, %r14
.LBB5_28:                               # %for.inc
                                        #   in Loop: Header=BB5_19 Depth=2
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	mode_wider_mode(%r12), %r12d
	testq	%r12, %r12
	jne	.LBB5_19
	jmp	.LBB5_29
	.align	16, 0x90
.LBB5_51:                               # %if.else
                                        #   in Loop: Header=BB5_4 Depth=1
	movl	%r12d, apply_args_mode(,%r15,4)
	movl	%r12d, apply_args_reg_offset(,%r15,4)
	jmp	.LBB5_52
.LBB5_11:                               #   in Loop: Header=BB5_4 Depth=1
	movl	class_narrowest_mode+4(%rip), %ebx
	jmp	.LBB5_13
.LBB5_7:                                #   in Loop: Header=BB5_4 Depth=1
	movl	class_narrowest_mode+4(%rip), %ebx
	jmp	.LBB5_13
.LBB5_9:                                #   in Loop: Header=BB5_4 Depth=1
	movl	class_narrowest_mode+4(%rip), %ebx
	.align	16, 0x90
.LBB5_13:                               # %for.body15.us
                                        #   Parent Loop BB5_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB5_18
# BB#14:                                # %cond.true.us
                                        #   in Loop: Header=BB5_13 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	je	.LBB5_18
# BB#15:                                # %lor.rhs.us
                                        #   in Loop: Header=BB5_13 Depth=2
	cmpq	$6, %rax
	movq	%rbx, %rax
	jne	.LBB5_17
# BB#16:                                # %lor.rhs.us
                                        #   in Loop: Header=BB5_13 Depth=2
	movq	%r14, %rax
.LBB5_17:                               # %lor.rhs.us
                                        #   in Loop: Header=BB5_13 Depth=2
	movq	%rax, %r14
.LBB5_18:                               # %for.inc.us
                                        #   in Loop: Header=BB5_13 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB5_13
.LBB5_29:                               # %for.end
                                        #   in Loop: Header=BB5_4 Depth=1
	testq	%r13, %r14
	movabsq	$0, %r12
	jne	.LBB5_48
.LBB5_30:                               # %if.then87
                                        #   in Loop: Header=BB5_4 Depth=1
	xorq	%r14, %r14
	movl	class_narrowest_mode+8(%rip), %eax
	testq	%r13, %rax
	movq	%rax, %rbx
	je	.LBB5_36
	.align	16, 0x90
.LBB5_31:                               # %for.body91
                                        #   Parent Loop BB5_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB5_34
# BB#32:                                # %land.lhs.true94
                                        #   in Loop: Header=BB5_31 Depth=2
	movq	$47, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r13, %rax
	je	.LBB5_34
# BB#33:                                # %land.lhs.true94
                                        #   in Loop: Header=BB5_31 Depth=2
	movq	%rbx, %r14
.LBB5_34:                               # %for.inc99
                                        #   in Loop: Header=BB5_31 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB5_31
# BB#35:                                # %if.end104
                                        #   in Loop: Header=BB5_4 Depth=1
	testq	%r13, %r14
	jne	.LBB5_48
.LBB5_36:                               # %if.then107
                                        #   in Loop: Header=BB5_4 Depth=1
	xorq	%r14, %r14
	movl	class_narrowest_mode+32(%rip), %eax
	testq	%r13, %rax
	movq	%rax, %rbx
	je	.LBB5_42
	.align	16, 0x90
.LBB5_37:                               # %for.body111
                                        #   Parent Loop BB5_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB5_40
# BB#38:                                # %land.lhs.true114
                                        #   in Loop: Header=BB5_37 Depth=2
	movq	$47, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r13, %rax
	je	.LBB5_40
# BB#39:                                # %land.lhs.true114
                                        #   in Loop: Header=BB5_37 Depth=2
	movq	%rbx, %r14
.LBB5_40:                               # %for.inc119
                                        #   in Loop: Header=BB5_37 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB5_37
# BB#41:                                # %if.end124
                                        #   in Loop: Header=BB5_4 Depth=1
	testq	%r13, %r14
	jne	.LBB5_48
.LBB5_42:                               # %if.then127
                                        #   in Loop: Header=BB5_4 Depth=1
	xorq	%r14, %r14
	movl	class_narrowest_mode+28(%rip), %eax
	testq	%r13, %rax
	movq	%rax, %rbx
	je	.LBB5_55
	.align	16, 0x90
.LBB5_43:                               # %for.body131
                                        #   Parent Loop BB5_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB5_46
# BB#44:                                # %land.lhs.true134
                                        #   in Loop: Header=BB5_43 Depth=2
	movq	$47, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r13, %rax
	je	.LBB5_46
# BB#45:                                # %land.lhs.true134
                                        #   in Loop: Header=BB5_43 Depth=2
	movq	%rbx, %r14
.LBB5_46:                               # %for.inc139
                                        #   in Loop: Header=BB5_43 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB5_43
# BB#47:                                # %if.end144
                                        #   in Loop: Header=BB5_4 Depth=1
	testq	%r13, %r14
	je	.LBB5_55
.LBB5_48:                               # %if.end148
                                        #   in Loop: Header=BB5_4 Depth=1
	movq	%r14, %rdi
	callq	get_mode_alignment
	movslq	apply_args_size.size(%rip), %rsi
	movq	%rax, %rdi
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	movq	%rsi, %rax
	cqto
	idivq	%rdi
	testq	%r13, %rdx
	je	.LBB5_50
# BB#49:                                # %if.then153
                                        #   in Loop: Header=BB5_4 Depth=1
	leaq	-1(%rdi,%rsi), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rdi
	subq	%rdx, %rsi
	movl	%esi, apply_args_size.size(%rip)
.LBB5_50:                               # %if.end157
                                        #   in Loop: Header=BB5_4 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movl	%esi, apply_args_reg_offset(,%r15,4)
	addq	%rsi, %rax
	movl	%eax, apply_args_size.size(%rip)
	movl	%r14d, apply_args_mode(,%r15,4)
.LBB5_52:                               # %for.inc171
                                        #   in Loop: Header=BB5_4 Depth=1
	incq	%r15
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$53, %rax
	jb	.LBB5_4
# BB#53:                                # %if.end173.loopexit
	movl	apply_args_size.size(%rip), %eax
.LBB5_54:                               # %if.end173
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_55:                               # %if.then147
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.apply_args_size, %rcx
	movq	$944, %rsi              # imm = 0x3B0
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp78:
	.size	apply_args_size, .Ltmp78-apply_args_size
	.cfi_endproc

	.globl	expand_builtin_saveregs
	.align	16, 0x90
	.type	expand_builtin_saveregs,@function
expand_builtin_saveregs:                # @expand_builtin_saveregs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp82:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp84:
	.cfi_def_cfa_offset 32
.Ltmp85:
	.cfi_offset %rbx, -24
.Ltmp86:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_2
# BB#1:                                 # %if.end
	callq	start_sequence
	movabsq	$.L.str101, %rdi
	xorq	%rax, %rax
	callq	error
	movq	const_int_rtx+512(%rip), %rbx
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	%rbx, 16(%rax)
	callq	push_topmost_sequence
	callq	get_insns
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_insns_after
	callq	pop_topmost_sequence
.LBB6_2:                                # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp87:
	.size	expand_builtin_saveregs, .Ltmp87-expand_builtin_saveregs
	.cfi_endproc

	.globl	std_expand_builtin_va_start
	.align	16, 0x90
	.type	std_expand_builtin_va_start,@function
std_expand_builtin_va_start:            # @std_expand_builtin_va_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp93:
	.cfi_def_cfa_offset 32
.Ltmp94:
	.cfi_offset %rbx, -24
.Ltmp95:
	.cfi_offset %r14, -16
	movq	%rdx, %r8
	movq	%rsi, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	jne	.LBB7_4
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	movq	integer_types+40(%rip), %rdx
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movl	60(%rdx), %edx
	movq	$23, %rcx
	shrq	%cl, %rax
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movzbl	mode_size(%rdx), %ecx
	addq	$4, %rax
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rax, %rdx
	cmpq	%rcx, %rdx
	ja	.LBB7_3
# BB#2:                                 # %if.then
	movq	%rcx, %rax
.LBB7_3:                                # %if.then
	leaq	-1(%rsi,%rax), %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	subq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r8, %rdi
	movq	%rdx, %rsi
	callq	plus_constant_wide
	movq	%rax, %r8
.LBB7_4:                                # %if.end
	movq	8(%r14), %rbx
	movq	global_trees+224(%rip), %rdi
	movq	%r8, %rsi
	callq	make_tree
	movq	%rax, %rcx
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_expr  # TAILCALL
.Ltmp96:
	.size	std_expand_builtin_va_start, .Ltmp96-std_expand_builtin_va_start
	.cfi_endproc

	.globl	std_expand_builtin_va_arg
	.align	16, 0x90
	.type	std_expand_builtin_va_arg,@function
std_expand_builtin_va_arg:              # @std_expand_builtin_va_arg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp102:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp103:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp104:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp105:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp106:
	.cfi_def_cfa_offset 48
.Ltmp107:
	.cfi_offset %rbx, -40
.Ltmp108:
	.cfi_offset %r12, -32
.Ltmp109:
	.cfi_offset %r14, -24
.Ltmp110:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	movl	target_flags(%rip), %edi
	movq	%rax, %r14
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	decq	%rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	global_trees(%rip), %rdx
	movq	%rax, %rcx
	cmpq	%rbx, %rdx
	je	.LBB8_3
# BB#1:                                 # %lor.lhs.false
	movq	128(%rbx), %rax
	movq	40(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB8_3
# BB#2:                                 # %lor.lhs.false12
	movzbl	18(%rdx), %eax
	testq	$8, %rax
	je	.LBB8_4
.LBB8_3:                                # %if.then
	movq	global_trees+120(%rip), %rbx
.LBB8_5:                                # %if.end
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	expand_expr
	movq	%rax, %rdi
	callq	copy_to_reg
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_7
# BB#6:                                 # %if.then27
	movq	8(%r15), %r12
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rcx
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB8_7:                                # %if.end38
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB8_4:                                # %if.else
	movq	sizetype_tab(%rip), %rbx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$62, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rbx
	jmp	.LBB8_5
.Ltmp111:
	.size	std_expand_builtin_va_arg, .Ltmp111-std_expand_builtin_va_arg
	.cfi_endproc

	.globl	expand_builtin_va_arg
	.align	16, 0x90
	.type	expand_builtin_va_arg,@function
expand_builtin_va_arg:                  # @expand_builtin_va_arg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp121:
	.cfi_def_cfa_offset 48
.Ltmp122:
	.cfi_offset %rbx, -40
.Ltmp123:
	.cfi_offset %r12, -32
.Ltmp124:
	.cfi_offset %r14, -24
.Ltmp125:
	.cfi_offset %r15, -16
	movq	global_trees+248(%rip), %rax
	movq	%rdi, %rbx
	movzbl	16(%rax), %edx
	movq	8(%rbx), %rcx
	movq	%rsi, %r12
	cmpq	$18, %rdx
	jne	.LBB9_4
# BB#1:                                 # %if.then
	movzbl	16(%rcx), %edx
	cmpq	$13, %rdx
	je	.LBB9_3
# BB#2:                                 # %if.then
	cmpq	$18, %rdx
	jne	.LBB9_4
.LBB9_3:                                # %if.then13
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
.LBB9_4:                                # %if.end18
	movq	128(%rax), %rax
	movq	128(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB9_6
# BB#5:                                 # %if.then23
	movabsq	$.L.str102, %rdi
	xorq	%rax, %rax
	callq	error
	movq	const_int_rtx+512(%rip), %rsi
	jmp	.LBB9_21
.LBB9_6:                                # %if.else
	movq	%r12, %rdi
	callq	*lang_type_promotes_to(%rip)
	testq	%rax, %rax
	je	.LBB9_20
# BB#7:                                 # %if.then25
	movq	96(%r12), %rcx
	testq	%rcx, %rcx
	movabsq	$.L.str103, %r14
	je	.LBB9_12
# BB#8:                                 # %if.then28
	movzbl	16(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB9_9
# BB#10:                                # %land.lhs.true
	movq	72(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB9_11
	jmp	.LBB9_12
.LBB9_20:                               # %if.else107
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	stabilize_va_list
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	ix86_va_arg
	movq	%rax, %rsi
	jmp	.LBB9_21
.LBB9_9:                                # %if.then28
	cmpq	$1, %rdx
	jne	.LBB9_12
.LBB9_11:                               # %if.then51
	movq	32(%rcx), %r14
.LBB9_12:                               # %if.end61
	movq	96(%rax), %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB9_17
# BB#13:                                # %if.then65
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB9_14
# BB#15:                                # %land.lhs.true87
	movq	72(%rax), %rax
	testq	%rax, %rax
	jne	.LBB9_16
	jmp	.LBB9_17
.LBB9_14:                               # %if.then65
	cmpq	$1, %rcx
	jne	.LBB9_17
.LBB9_16:                               # %if.then93
	movq	32(%rax), %r15
.LBB9_17:                               # %if.end103
	movabsq	$.L.str104, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	warning
	movzbl	expand_builtin_va_arg.gave_help(%rip), %eax
	testq	%rax, %rax
	jne	.LBB9_19
# BB#18:                                # %if.then105
	movq	$1, %rax
	movb	%al, expand_builtin_va_arg.gave_help(%rip)
	movabsq	$.L.str105, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	warning
.LBB9_19:                               # %if.end106
	callq	gen_trap
	movq	%rax, %rdi
	callq	emit_insn
	callq	emit_barrier
	movq	const_int_rtx+512(%rip), %rsi
.LBB9_21:                               # %if.end111
	movl	60(%r12), %edi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	callq	get_varargs_alias_set
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp126:
	.size	expand_builtin_va_arg, .Ltmp126-expand_builtin_va_arg
	.cfi_endproc

	.globl	expand_builtin_trap
	.align	16, 0x90
	.type	expand_builtin_trap,@function
expand_builtin_trap:                    # @expand_builtin_trap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp128:
	.cfi_def_cfa_offset 16
	callq	gen_trap
	movq	%rax, %rdi
	callq	emit_insn
	popq	%rax
	jmp	emit_barrier  # TAILCALL
.Ltmp129:
	.size	expand_builtin_trap, .Ltmp129-expand_builtin_trap
	.cfi_endproc

	.align	16, 0x90
	.type	stabilize_va_list,@function
stabilize_va_list:                      # @stabilize_va_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp135:
	.cfi_def_cfa_offset 32
.Ltmp136:
	.cfi_offset %rbx, -24
.Ltmp137:
	.cfi_offset %r14, -16
	movq	global_trees+248(%rip), %rax
	movzbl	16(%rax), %ecx
	movq	%rdi, %rbx
	cmpq	$18, %rcx
	jne	.LBB11_5
# BB#1:                                 # %if.then
	movzbl	17(%rbx), %eax
	testq	$1, %rax
	je	.LBB11_3
# BB#2:                                 # %if.then4
	movq	%rbx, %rdi
	callq	save_expr
	movq	%rax, %rbx
.LBB11_3:                               # %if.end
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB11_12
# BB#4:                                 # %if.then11
	movq	global_trees+248(%rip), %rax
	movq	8(%rax), %rdi
	callq	build_pointer_type
	movq	global_trees+248(%rip), %rdi
	movq	%rax, %r14
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	$115, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	jmp	.LBB11_11
.LBB11_5:                               # %if.else
	movl	16(%rbx), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rsi
	jne	.LBB11_8
# BB#6:                                 # %if.then21
	testq	$256, %rcx              # imm = 0x100
	je	.LBB11_12
# BB#7:                                 # %if.end29
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rbx
	movl	16(%rbx), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rbx)
.LBB11_8:                               # %if.end37
	testq	$256, %rcx              # imm = 0x100
	je	.LBB11_10
# BB#9:                                 # %if.then44
	movq	%rbx, %rdi
	callq	save_expr
	movq	%rax, %rbx
.LBB11_10:                              # %if.end46
	movq	8(%rbx), %rax
	movq	8(%rax), %rsi
	movq	$41, %rdi
	movq	%rbx, %rdx
.LBB11_11:                              # %if.end46
	callq	build1
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fold  # TAILCALL
.LBB11_12:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp138:
	.size	stabilize_va_list, .Ltmp138-stabilize_va_list
	.cfi_endproc

	.globl	expand_builtin_expect_jump
	.align	16, 0x90
	.type	expand_builtin_expect_jump,@function
expand_builtin_expect_jump:             # @expand_builtin_expect_jump
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
	subq	$32, %rsp
.Ltmp150:
	.cfi_def_cfa_offset 80
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
	movq	40(%rdi), %rax
	movq	(%rax), %rcx
	movq	32(%rcx), %rbx
	movq	8(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rdx, %r14
	movq	%rsi, %r15
	cmpq	$6, %rcx
	jne	.LBB12_33
# BB#1:                                 # %land.lhs.true
	movq	32(%rax), %r13
	movq	%rbx, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB12_3
# BB#2:                                 # %lor.lhs.false
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB12_33
.LBB12_3:                               # %if.then
	movq	%r13, %rdi
	callq	unsafe_for_reeval
	andq	%r12, %rax
	cmpq	$2, %rax
	jne	.LBB12_4
.LBB12_33:                              # %return
	xorq	%rax, %rax
.LBB12_32:                              # %for.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_4:                               # %if.then
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	cmpq	$1, %rax
	jne	.LBB12_6
# BB#5:                                 # %sw.bb
	movq	%r13, %rdi
	callq	unsave_expr
	movq	%rax, %r13
.LBB12_6:                               # %sw.epilog
	callq	start_sequence
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	do_jump
	callq	gen_sequence
	movq	%rax, %r12
	callq	end_sequence
	movq	8(%r12), %rax
	movslq	(%rax), %rcx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	testq	%rcx, %rcx
	jle	.LBB12_7
# BB#8:
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r14, (%rsp)            # 8-byte Spill
	movq	$1, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB12_9
.LBB12_7:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB12_30
.LBB12_24:                              #   in Loop: Header=BB12_9 Depth=1
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB12_29
	.align	16, 0x90
.LBB12_9:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r13,8), %r14
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB12_29
# BB#10:                                # %land.lhs.true21
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%rbx, %rax
	je	.LBB12_29
# BB#11:                                # %land.lhs.true24
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	%r14, %rdi
	callq	pc_set
	testq	%rax, %rax
	je	.LBB12_29
# BB#12:                                # %if.then27
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$72, %rcx
	jne	.LBB12_29
# BB#13:                                # %if.end
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	16(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$67, %rcx
	jne	.LBB12_15
# BB#14:                                # %if.then40
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	8(%rdx), %rbx
	movq	$1, %r15
	jmp	.LBB12_17
.LBB12_15:                              # %if.else
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	24(%rax), %rdx
	movzbl	(%rdx), %eax
	cmpq	$67, %rax
	jne	.LBB12_21
# BB#16:                                # %if.then53
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	8(%rdx), %rbx
	xorq	%r15, %r15
	jmp	.LBB12_17
.LBB12_21:                              # %if.else60
                                        #   in Loop: Header=BB12_9 Depth=1
	xorq	%rbx, %rbx
	cmpq	$51, %rcx
	jne	.LBB12_23
# BB#22:                                #   in Loop: Header=BB12_9 Depth=1
	movq	$1, %r15
	jmp	.LBB12_17
.LBB12_23:                              # %if.else68
                                        #   in Loop: Header=BB12_9 Depth=1
	cmpq	$51, %rax
	movabsq	$0, %r15
	jne	.LBB12_24
.LBB12_17:                              # %if.end80
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rdx
	je	.LBB12_19
# BB#18:                                # %if.end80
                                        #   in Loop: Header=BB12_9 Depth=1
	movabsq	$1, %rdx
.LBB12_19:                              # %if.end80
                                        #   in Loop: Header=BB12_9 Depth=1
	xorq	%r15, %rdx
	testq	%rbx, %rbx
	jne	.LBB12_25
# BB#20:                                #   in Loop: Header=BB12_9 Depth=1
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB12_28
.LBB12_25:                              # %if.else87
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	%rax, %rbx
	jne	.LBB12_27
# BB#26:                                # %if.then89
                                        #   in Loop: Header=BB12_9 Depth=1
	xorq	$1, %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB12_28
.LBB12_27:                              # %if.else91
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	%rax, %rbx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jne	.LBB12_29
.LBB12_28:                              # %if.end96
                                        #   in Loop: Header=BB12_9 Depth=1
	incq	24(%rsp)                # 8-byte Folded Spill
	movq	$6, %rsi
	movq	%r14, %rdi
	callq	predict_insn_def
	.align	16, 0x90
.LBB12_29:                              # %for.inc
                                        #   in Loop: Header=BB12_9 Depth=1
	movq	8(%r12), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	cmpq	%rdx, %rsi
	jl	.LBB12_9
.LBB12_30:                              # %for.end
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	je	.LBB12_32
# BB#31:                                # %for.end
	movq	%r12, %rax
	jmp	.LBB12_32
.Ltmp156:
	.size	expand_builtin_expect_jump, .Ltmp156-expand_builtin_expect_jump
	.cfi_endproc

	.globl	expand_builtin
	.align	16, 0x90
	.type	expand_builtin,@function
expand_builtin:                         # @expand_builtin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp163:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp164:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp165:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp166:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp167:
	.cfi_def_cfa_offset 48
	subq	$640, %rsp              # imm = 0x280
.Ltmp168:
	.cfi_def_cfa_offset 688
.Ltmp169:
	.cfi_offset %rbx, -48
.Ltmp170:
	.cfi_offset %r12, -40
.Ltmp171:
	.cfi_offset %r13, -32
.Ltmp172:
	.cfi_offset %r14, -24
.Ltmp173:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %rax
	movq	32(%rax), %r14
	movq	48(%r14), %rax
	movq	%rdx, %r13
	movq	%rsi, %r15
	andq	$1610612736, %rax       # imm = 0x60000000
	cmpq	$1073741824, %rax       # imm = 0x40000000
	jne	.LBB13_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	*targetm+312(%rip)
	jmp	.LBB13_650
.LBB13_2:                               # %if.end
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	%r8, 88(%rsp)           # 8-byte Spill
	movq	40(%rbx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movl	optimize(%rip), %eax
	movl	56(%r14), %r12d
	testq	%rax, %rax
	jne	.LBB13_7
# BB#3:                                 # %land.lhs.true
	movq	72(%r14), %rax
	movq	32(%rax), %rdi
	movabsq	$.L.str106, %rsi
	movq	$10, %rdx
	callq	strncmp
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB13_7
# BB#4:                                 # %if.then12
	leaq	-17(%r12), %rcx
	andq	%rdx, %rcx
	cmpq	$57, %rcx
	ja	.LBB13_7
# BB#5:                                 # %if.then12
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$285943392133709823, %rcx # imm = 0x3F7E00009FFFFFF
	testq	%rcx, %rax
	je	.LBB13_7
# BB#6:                                 # %sw.bb
	movq	%rbx, %rdi
	movq	%r15, %rsi
	jmp	.LBB13_649
.LBB13_7:                               # %if.end14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$94, %r12
	jbe	.LBB13_8
.LBB13_647:                             # %sw.default314
	movq	72(%r14), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str108, %rdi
	xorq	%rax, %rax
	callq	error
	movq	%r15, %rsi
.LBB13_648:                             # %sw.epilog320
	movq	%rbx, %rdi
.LBB13_649:                             # %return
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	expand_call
.LBB13_650:                             # %return
	movq	%rax, %r15
.LBB13_651:                             # %return
	movq	%r15, %rax
	addq	$640, %rsp              # imm = 0x280
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_8:                               # %if.end14
	movq	%r15, %rsi
	jmpq	*.LJTI13_0(,%r12,8)
.LBB13_12:                              # %sw.bb17
	movq	%rax, %rcx
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	movq	%rcx, %rax
	movq	%r15, %rsi
	je	.LBB13_648
.LBB13_13:                              # %sw.bb21
	movq	$7, %rsi
	movq	$5, %rdx
	movq	%rbx, %r15
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%rbx, %rax
	movq	%r15, %rbx
	je	.LBB13_648
# BB#14:                                # %if.end.i
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movl	16(%rax), %ecx
	andq	$254, %rcx
	cmpq	$34, %rcx
	jne	.LBB13_16
# BB#15:
	movq	48(%rsp), %r12          # 8-byte Reload
	jmp	.LBB13_17
.LBB13_154:                             # %sw.bb150
	movq	$6, %rsi
	movq	$5, %rdx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#155:                               # %expand_builtin_alloca.exit
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	$8, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	allocate_dynamic_stack_space
	jmp	.LBB13_229
.LBB13_9:                               # %sw.bb15
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin, %rcx
	movq	$3665, %rsi             # imm = 0xE51
	jmp	.LBB13_10
.LBB13_11:                              # %sw.bb16
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin, %rcx
	movq	$3678, %rsi             # imm = 0xE5E
	jmp	.LBB13_10
.LBB13_429:                             # %sw.bb225
	movq	$13, %rsi
	movq	$6, %rdx
	movq	$5, %rcx
	movq	%rbx, %r14
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%rbx, %rax
	movq	%r14, %rbx
	je	.LBB13_648
# BB#430:                               # %expand_builtin_bzero.exit
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	(%r14), %rax
	movq	%rbx, %r15
	movq	32(%r14), %rbx
	movq	32(%rax), %rax
	movq	sizetype_tab(%rip), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	convert
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	global_trees+88(%rip), %rcx
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rbx
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rax, 40(%rbx)
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_builtin_memset
	xorq	%rsi, %rsi
	movq	%rax, %r15
	testq	%r15, %r15
	movq	%r14, 40(%rbx)
	jne	.LBB13_651
	jmp	.LBB13_648
.LBB13_532:                             # %sw.bb240
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_533
# BB#534:                               # %if.end.i1315
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rax
	movq	(%rax), %rcx
	movq	32(%rdx), %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	32(%rax), %r13
	movq	32(%rcx), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_537
# BB#535:                               # %land.lhs.true.i1318
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	testq	%rax, %rax
	je	.LBB13_536
.LBB13_537:                             # %if.end15.i1324
	leaq	144(%rsp), %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	testq	%r12, %r12
	movabsq	$0, %r14
	je	.LBB13_543
# BB#538:                               # %if.end.i.i1326
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_539
# BB#540:                               # %if.else.i.i1331
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %r14
	je	.LBB13_543
# BB#541:                               # %lor.lhs.false.i.i1336
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r14
	jg	.LBB13_543
# BB#542:                               # %if.end10.i.i1340
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
	jmp	.LBB13_543
.LBB13_156:                             # %sw.bb155
	movq	$6, %rsi
	movq	$5, %rdx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#157:                               # %if.end.i594
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	32(%r14), %rcx
	movq	8(%rcx), %rcx
	movl	60(%rcx), %edx
	movq	optab_table+296(%rip), %rsi
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	$1, %r8
	movq	%rdx, %rdi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	expand_unop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB13_651
# BB#158:                               # %if.then6.i
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin_ffs, %rcx
	movq	$3325, %rsi             # imm = 0xCFD
	jmp	.LBB13_10
.LBB13_366:                             # %sw.bb205
	movq	$13, %rsi
	movq	$6, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_367
# BB#368:                               # %if.else.i982
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	32(%rax), %r13
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB13_369
# BB#370:                               # %if.end.i985
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r14
	leaq	616(%rsp), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_371
# BB#372:                               # %if.end.i.i987
	movq	616(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_373
# BB#374:                               # %if.else.i.i992
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_375
# BB#376:                               # %lor.lhs.false.i.i997
	movl	32(%r12), %eax
	movq	616(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_378
# BB#377:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_387:                             # %sw.bb210
	movq	$13, %rsi
	movq	$6, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_388
# BB#389:                               # %if.else.i1031
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	32(%rax), %r13
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB13_390
# BB#391:                               # %if.end.i1034
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r14
	leaq	624(%rsp), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_403
# BB#392:                               # %if.end.i.i1036
	movq	624(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_393
# BB#394:                               # %if.else.i.i1041
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_403
# BB#395:                               # %lor.lhs.false.i.i1046
	movl	32(%r12), %eax
	movq	624(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_403
# BB#396:                               # %if.end10.i.i1050
	movq	40(%r12), %r12
	movq	624(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
	jmp	.LBB13_397
.LBB13_408:                             # %sw.bb215
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_409
# BB#410:                               # %if.else.i1096
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rax
	movq	(%rax), %rcx
	movq	32(%rdx), %r12
	movq	32(%rax), %r14
	movq	32(%rcx), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	get_pointer_alignment
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	get_pointer_alignment
	movq	%rax, %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB13_409
# BB#411:                               # %if.end.i1098
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_414
# BB#412:                               # %land.lhs.true.i1100
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	testq	%rax, %rax
	je	.LBB13_413
.LBB13_414:                             # %if.end19.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB13_409
# BB#415:                               # %if.end22.i1108
	movq	%r12, %rdi
	callq	get_memory_rtx
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	set_mem_align
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	expand_expr
	movq	%rax, %rbx
	leaq	632(%rsp), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB13_426
# BB#416:                               # %if.end.i.i1110
	movq	632(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_417
# BB#418:                               # %if.else.i.i1115
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_426
# BB#419:                               # %lor.lhs.false.i.i1120
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	32(%rax), %eax
	movq	632(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_426
# BB#420:                               # %if.end10.i.i1124
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %r15
	movq	632(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r15, %rax
	jmp	.LBB13_421
.LBB13_428:                             # %sw.bb220
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_builtin_memset
	jmp	.LBB13_251
.LBB13_213:                             # %sw.bb175
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	movq	%r14, %r12
	je	.LBB13_214
# BB#215:                               # %if.else.i690
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %r14
	movq	32(%rax), %rax
	leaq	576(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	string_constant
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB13_216
# BB#217:                               # %if.end.i.i692
	movq	576(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_218
# BB#219:                               # %if.else.i.i697
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB13_220
# BB#221:                               # %lor.lhs.false.i.i702
	movl	32(%r13), %eax
	movq	576(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_223
# BB#222:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_230:                             # %sw.bb180
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#231:                               # %if.else.i722
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rax
	movq	(%rax), %rcx
	movq	32(%rdx), %r14
	movq	32(%rax), %r13
	movq	32(%rcx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	leaq	144(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, 64(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB13_237
# BB#232:                               # %if.end.i.i724
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_233
# BB#234:                               # %if.else.i.i729
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_237
# BB#235:                               # %lor.lhs.false.i.i734
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	32(%rax), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_237
# BB#236:                               # %if.end10.i.i738
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
	movq	%rax, %r12
	jmp	.LBB13_237
.LBB13_183:                             # %sw.bb165
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_184
# BB#185:                               # %if.end.i628
	movq	built_in_decls+176(%rip), %r14
	testq	%r14, %r14
	je	.LBB13_186
# BB#187:                               # %if.end4.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	c_strlen
	movq	%rax, %r12
	testq	%r12, %r12
	movabsq	$0, %rsi
	je	.LBB13_648
# BB#188:                               # %expand_builtin_strcpy.exit
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	chainon
	movq	8(%r14), %rax
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	build1
	movq	8(%r14), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	%rax, %rdi
	callq	fold
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	jmp	.LBB13_228
.LBB13_189:                             # %sw.bb170
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_409
# BB#190:                               # %if.else.i651
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	(%r14), %rax
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	c_strlen
	movq	(%r14), %rcx
	movq	%r14, %r13
	movq	(%rcx), %rcx
	movq	32(%rcx), %r14
	movzbl	16(%r14), %ecx
	movq	%rax, %r12
	cmpq	$25, %rcx
	jne	.LBB13_409
# BB#191:                               # %if.end.i653
	movq	%r14, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_193
# BB#192:                               # %if.then12.i
	movq	%r13, %r14
	movq	(%r14), %rax
	movq	32(%rax), %rax
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	32(%r14), %rax
	jmp	.LBB13_209
.LBB13_431:                             # %sw.bb230
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#432:                               # %if.end.i1159
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	32(%rax), %r13
	leaq	144(%rsp), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB13_438
# BB#433:                               # %if.end.i.i1161
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_434
# BB#435:                               # %if.else.i.i1166
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_438
# BB#436:                               # %lor.lhs.false.i.i1171
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_438
# BB#437:                               # %if.end10.i.i1175
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
	jmp	.LBB13_438
.LBB13_485:                             # %sw.bb235
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_486
# BB#487:                               # %if.end.i1231
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rax
	movq	(%rax), %rcx
	movq	32(%rdx), %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	32(%rax), %r13
	movq	32(%rcx), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_490
# BB#488:                               # %land.lhs.true.i1234
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	testq	%rax, %rax
	je	.LBB13_489
.LBB13_490:                             # %if.end16.i1239
	leaq	144(%rsp), %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	testq	%r12, %r12
	movabsq	$0, %r14
	je	.LBB13_496
# BB#491:                               # %if.end.i.i1241
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_492
# BB#493:                               # %if.else.i.i1246
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %r14
	je	.LBB13_496
# BB#494:                               # %lor.lhs.false.i.i1251
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r14
	jg	.LBB13_496
# BB#495:                               # %if.end10.i.i1255
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
	jmp	.LBB13_496
.LBB13_159:                             # %sw.bb160
	movq	%rax, %r14
	movq	8(%rbx), %rax
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movl	60(%rax), %r12d
	movq	$9, %rcx
	shrq	%cl, %r12
	movq	$13, %rsi
	movq	$5, %rdx
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_409
# BB#160:                               # %if.else.i610
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r13
	movq	%r13, %rdi
	callq	get_pointer_alignment
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$60, %rcx
	shrq	%cl, %rdx
	andq	$7, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$35, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	addq	$7, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$15, %rax
	jb	.LBB13_409
# BB#161:                               # %if.else.i610
	andq	$127, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB13_409
# BB#162:                               # %while.body.lr.ph.i
	movq	optab_table+352(%rip), %rsi
	movq	%r12, %rax
.LBB13_163:                             # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movslq	8(%rsi,%rdi), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB13_165
# BB#164:                               # %if.end9.i
                                        #   in Loop: Header=BB13_163 Depth=1
	movzbl	mode_wider_mode(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB13_163
	jmp	.LBB13_409
.LBB13_306:                             # %sw.bb195
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_307
# BB#308:                               # %if.else.i862
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %r13
	movq	32(%rax), %rax
	leaq	584(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	string_constant
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB13_309
# BB#310:                               # %if.end.i.i864
	movq	584(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_311
# BB#312:                               # %if.else.i.i869
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_313
# BB#314:                               # %lor.lhs.false.i.i874
	movl	32(%r14), %eax
	movq	584(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_316
# BB#315:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_339:                             # %sw.bb200
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_340
# BB#341:                               # %if.else.i913
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %r13
	movq	32(%rax), %rax
	leaq	600(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	string_constant
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB13_342
# BB#343:                               # %if.end.i.i915
	movq	600(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_344
# BB#345:                               # %if.else.i.i920
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_346
# BB#347:                               # %lor.lhs.false.i.i925
	movl	32(%r14), %eax
	movq	600(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_349
# BB#348:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_254:                             # %sw.bb185
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#255:                               # %if.else.i774
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	32(%rax), %r13
	leaq	144(%rsp), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB13_261
# BB#256:                               # %if.end.i.i776
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_257
# BB#258:                               # %if.else.i.i781
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_261
# BB#259:                               # %lor.lhs.false.i.i786
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_261
# BB#260:                               # %if.end10.i.i790
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
	jmp	.LBB13_261
.LBB13_279:                             # %sw.bb190
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#280:                               # %if.else.i810
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %r13
	movq	32(%rax), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	leaq	144(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB13_286
# BB#281:                               # %if.end.i.i812
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_282
# BB#283:                               # %if.else.i.i817
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_286
# BB#284:                               # %lor.lhs.false.i.i822
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_286
# BB#285:                               # %if.end10.i.i826
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
	jmp	.LBB13_286
.LBB13_90:                              # %sw.bb50
	callq	expand_builtin_saveregs
	jmp	.LBB13_650
.LBB13_100:                             # %sw.bb56
	movq	%rax, %rsi
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_123
# BB#101:                               # %if.then.i538
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	8(%rax), %rcx
	movzbl	16(%rcx), %edx
	addq	$-5, %rdx
	andq	%rsi, %rdx
	cmpq	$19, %rdx
	ja	.LBB13_120
# BB#102:                               # %if.then.i538
	xorq	%rax, %rax
	jmpq	*.LJTI13_1(,%rdx,8)
.LBB13_115:                             # %sw.bb13.i.i
	movq	$13, %rax
	jmp	.LBB13_121
.LBB13_99:                              # %sw.bb54
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	expand_builtin_next_arg
	jmp	.LBB13_650
.LBB13_91:                              # %sw.bb52
	movq	%rax, %r14
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_98
# BB#92:                                # %if.then.i531
	movq	cfun(%rip), %rbx
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%r14, %rax
	je	.LBB13_93
# BB#95:                                # %if.else.i532
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	cmpq	$7, %rax
	jb	.LBB13_97
# BB#96:                                # %if.then9.i
	movabsq	$.L.str124, %rcx
	jmp	.LBB13_94
.LBB13_124:                             # %sw.bb58
	movq	%rbx, %rax
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_125
# BB#126:                               # %if.end.i555
	movq	8(%rax), %rax
	movl	60(%rax), %ebx
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	$140, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	jmp	.LBB13_650
.LBB13_127:                             # %sw.bb60
	movq	%rax, %r12
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_128
# BB#129:                               # %if.else.i567
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movl	56(%r14), %ebx
	testq	%r12, %rax
	je	.LBB13_130
# BB#133:                               # %if.else12.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	global_rtl+32(%rip), %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	callq	expand_builtin_return_addr
	movl	56(%r14), %ecx
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB13_134
# BB#138:                               # %if.end31.i
	andq	%r12, %rcx
	cmpq	$52, %rcx
	je	.LBB13_651
# BB#139:                               # %if.end38.i
	movzbl	(%r15), %eax
	cmpq	$133, %rax
	jg	.LBB13_142
# BB#140:                               # %if.end38.i
	addq	$-54, %rax
	andq	%r12, %rax
	cmpq	$14, %rax
	ja	.LBB13_144
# BB#141:                               # %if.end38.i
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24727, %rdx            # imm = 0x6097
	jne	.LBB13_651
	jmp	.LBB13_144
.LBB13_145:                             # %sw.bb62
	movq	%rax, %rdx
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	jne	.LBB13_150
# BB#146:                               # %lor.lhs.false
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %ecx
	addq	$-18, %rcx
	andq	%rdx, %rcx
	cmpq	$4, %rcx
	ja	.LBB13_150
# BB#147:                               # %lor.lhs.false111
	movq	96(%rax), %rax
	movq	144(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB13_149
# BB#148:                               # %cond.false
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rax
	movq	144(%rax), %rcx
.LBB13_149:                             # %cond.end
	movzbl	(%rcx), %edx
	cmpq	$66, %rdx
	jne	.LBB13_150
# BB#151:                               # %if.else129
	testq	%rcx, %rcx
	jne	.LBB13_153
# BB#152:                               # %cond.false140
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rax
	movq	144(%rax), %rcx
.LBB13_153:                             # %cond.end147
	movq	8(%rcx), %r15
	jmp	.LBB13_651
.LBB13_31:                              # %sw.bb26
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	24(%rax), %r15
	testq	%r15, %r15
	jne	.LBB13_651
# BB#32:                                # %if.end.i475
	callq	start_sequence
	callq	apply_args_size
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$51, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	assign_stack_local
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movzbl	mode_size+4(%rdx), %r15d
	cmpq	$0, struct_value_rtx(%rip)
	movq	%rax, %rbx
	je	.LBB13_33
# BB#34:                                # %if.end.i475
	movabsq	$1, %rax
	jmp	.LBB13_35
.LBB13_44:                              # %sw.bb28
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%rbx, %rax
	jne	.LBB13_45
# BB#51:                                # %land.lhs.true31
	movq	$15, %rsi
	movq	$13, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%rbx, %rax
	je	.LBB13_52
.LBB13_45:                              # %for.cond.preheader
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
                                        # implicit-def: RAX
	movq	%rax, 88(%rsp)          # 8-byte Spill
                                        # implicit-def: R12
                                        # implicit-def: R15
	je	.LBB13_49
# BB#46:
	leaq	96(%rsp), %rbx
.LBB13_47:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	32(%r14), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, (%rbx)
	movq	(%r14), %r14
	movq	%r14, 72(%rsp)          # 8-byte Spill
	addq	$8, %rbx
	testq	%r14, %r14
	jne	.LBB13_47
# BB#48:                                # %for.cond.for.end_crit_edge
	movq	96(%rsp), %r15
	movq	104(%rsp), %r12
	movq	112(%rsp), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB13_49:                              # %for.end
	movq	$0, 136(%rsp)
	movq	$0, 128(%rsp)
	callq	apply_result_size
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$51, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	assign_stack_local
	movl	target_flags(%rip), %edx
	movq	%rax, %rbx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	%rdx, %rdi
	callq	gen_reg_rtx
	movl	target_flags(%rip), %edx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	%rdx, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
	callq	emit_queue
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	leaq	136(%rsp), %rax
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	%rax, %rsi
	callq	emit_stack_save
	xorq	%rsi, %rsi
	movq	$8, %rdx
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	allocate_dynamic_stack_space
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movl	target_flags(%rip), %edx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rsi
	callq	set_mem_align
	movq	$51, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_MEM
	movl	target_flags(%rip), %edx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rsi
	callq	set_mem_align
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	emit_block_move
	callq	apply_args_size
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movl	target_flags(%rip), %edx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	set_mem_align
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movzbl	mode_size+4(%rax), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	cmpq	$0, struct_value_rtx(%rip)
	je	.LBB13_50
# BB#53:                                # %for.end
	movabsq	$1, %rax
	jmp	.LBB13_54
.LBB13_81:                              # %sw.bb42
	movq	$13, %rsi
	movq	$5, %rdx
	movq	%rax, %r14
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	je	.LBB13_89
# BB#82:                                # %if.then45
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %r14
	callq	apply_result_size
	movq	$51, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movabsq	$4294967288, %r15       # imm = 0xFFFFFFF8
	xorq	%r12, %r12
	xorq	%r13, %r13
.LBB13_83:                              # %for.body.i507
                                        # =>This Inner Loop Header: Depth=1
	movl	apply_result_mode(,%r12,4), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB13_87
# BB#84:                                # %if.then.i
                                        #   in Loop: Header=BB13_83 Depth=1
	movl	apply_result_mode(,%r12,4), %eax
	movq	%rax, %rdi
	callq	get_mode_alignment
	movq	%rax, %rsi
	andq	%r15, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_86
# BB#85:                                # %if.then5.i
                                        #   in Loop: Header=BB13_83 Depth=1
	leaq	-1(%rbx,%rsi), %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	subq	%rax, %rbx
.LBB13_86:                              # %if.end.i518
                                        #   in Loop: Header=BB13_83 Depth=1
	movl	apply_result_mode(,%r12,4), %eax
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_REG
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movl	apply_result_mode(,%r12,4), %edx
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	adjust_address_1
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r13, %rdi
	callq	push_to_sequence
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	callq	get_insns
	movq	%rax, %r13
	callq	end_sequence
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	addq	%rax, %rbx
.LBB13_87:                              # %for.inc.i522
                                        #   in Loop: Header=BB13_83 Depth=1
	incq	%r12
	cmpq	$53, %r12
	jne	.LBB13_83
# BB#88:                                # %expand_builtin_return.exit
	movq	%r13, %rdi
	callq	emit_insns
	callq	expand_null_return
.LBB13_89:                              # %if.end49
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_582:                             # %sw.bb245
	movq	$13, %rsi
	movq	$5, %rdx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	xorq	%rsi, %rsi
	testq	%r14, %rax
	je	.LBB13_648
# BB#583:                               # %if.end.i1393
	testq	%r15, %r15
	je	.LBB13_586
# BB#584:                               # %lor.lhs.false.i1396
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB13_586
# BB#585:                               # %lor.lhs.false2.i
	movl	8(%r15), %eax
	cmpq	$52, %rax
	ja	.LBB13_587
.LBB13_586:                             # %if.then4.i
	movq	integer_types+40(%rip), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
.LBB13_587:                             # %expand_builtin_setjmp.exit
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %r13
	callq	gen_label_rtx
	movq	%rax, %r14
	callq	gen_label_rtx
	movq	%rax, %r12
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	expand_builtin_setjmp_setup
	movq	const_int_rtx+512(%rip), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r12, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%r14, %rdi
	callq	emit_label
	movq	%r14, %rdi
	callq	expand_builtin_setjmp_receiver
	movq	const_int_rtx+520(%rip), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r12, %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 424(%rax)
	movq	cfun(%rip), %rax
	movq	176(%rax), %rax
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	cfun(%rip), %rcx
	testq	%r15, %r15
	movq	%rax, 176(%rcx)
	jne	.LBB13_651
	jmp	.LBB13_648
.LBB13_588:                             # %sw.bb250
	movq	$13, %rsi
	movq	$6, %rdx
	movq	$5, %rcx
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%r14, %rax
	movq	%r15, %rsi
	je	.LBB13_648
# BB#589:                               # %if.else254
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	(%rbx), %rcx
	movq	32(%rcx), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB13_591
# BB#590:                               # %if.then265
	movabsq	$.L.str107, %rcx
	jmp	.LBB13_94
.LBB13_592:                             # %sw.bb267
	callq	gen_trap
	movq	%rax, %rdi
	callq	emit_insn
	callq	emit_barrier
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_623:                             # %sw.bb313
	movq	$13, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	validate_arglist
	testq	%rbx, %rax
	movq	%rbx, %r12
	je	.LBB13_646
# BB#624:                               # %if.end.i1496
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %rbx
	testq	%rax, %rax
	je	.LBB13_627
# BB#625:                               # %if.then2.i1498
	movq	(%rax), %rcx
	movq	32(%rax), %r14
	testq	%rcx, %rcx
	je	.LBB13_628
# BB#626:                               # %if.then12.i1499
	movq	32(%rcx), %r15
	jmp	.LBB13_629
.LBB13_593:                             # %sw.bb269
	xorq	%rdx, %rdx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	expand_builtin_fputs
	jmp	.LBB13_229
.LBB13_594:                             # %sw.bb274
	movq	$1, %rdx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	expand_builtin_fputs
	jmp	.LBB13_251
.LBB13_595:                             # %sw.bb279
	callq	expand_builtin_unwind_init
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_596:                             # %sw.bb280
	movq	global_rtl+80(%rip), %r15
	jmp	.LBB13_651
.LBB13_597:                             # %sw.bb281
	callq	expand_builtin_dwarf_fp_regnum
	jmp	.LBB13_650
.LBB13_598:                             # %sw.bb283
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	expand_builtin_init_dwarf_reg_sizes
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_599:                             # %sw.bb286
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	expand_builtin_frob_return_addr
	jmp	.LBB13_650
.LBB13_600:                             # %sw.bb290
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	expand_builtin_extract_return_addr
	jmp	.LBB13_650
.LBB13_601:                             # %sw.bb294
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %rcx
	movq	32(%rax), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	expand_builtin_eh_return
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_602:                             # %sw.bb301
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	expand_builtin_eh_return_data_regno
	jmp	.LBB13_650
.LBB13_603:                             # %sw.bb303
	xorq	%rdi, %rdi
	callq	expand_builtin_next_arg
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	$0, (%rcx)
	movq	%rax, %rbx
	je	.LBB13_605
# BB#604:                               # %if.then6.i1423
	movabsq	$.L.str115, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	error
.LBB13_605:                             # %expand_builtin_va_start.exit
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	stabilize_va_list
	xorq	%rdi, %rdi
	jmp	.LBB13_606
.LBB13_607:                             # %sw.bb305
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rbx
	movq	%rbx, %rdi
	callq	expand_builtin_next_arg
	cmpq	$0, (%rbx)
	movq	%rax, %rbx
	je	.LBB13_609
# BB#608:                               # %if.then6.i1432
	movabsq	$.L.str115, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	error
.LBB13_609:                             # %expand_builtin_va_start.exit1436
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	stabilize_va_list
	movq	$1, %rdi
.LBB13_606:                             # %return
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	ix86_va_start
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_610:                             # %sw.bb307
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	17(%rax), %ecx
	testq	$1, %rcx
	je	.LBB13_612
# BB#611:                               # %if.then.i1443
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB13_612:                             # %expand_builtin_va_end.exit
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_613:                             # %sw.bb309
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	32(%rcx), %rcx
	movq	32(%rax), %rbx
	movq	$1, %rsi
	movq	%rcx, %rdi
	callq	stabilize_va_list
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	stabilize_va_list
	movq	global_trees+248(%rip), %rcx
	movzbl	16(%rcx), %edx
	movq	%rax, %rbx
	cmpq	$18, %rdx
	jne	.LBB13_614
# BB#615:                               # %if.else.i1468
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	expand_expr
	movl	target_flags(%rip), %edx
	movq	%rax, %r13
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	movq	global_trees+248(%rip), %rcx
	movq	40(%rcx), %rdi
	movq	%rax, %r12
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, %r15
	movq	$51, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	8(%r14), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %r14
	movq	%rcx, %rdi
	callq	get_alias_set
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	global_trees+248(%rip), %rax
	movl	64(%rax), %eax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	set_mem_align
	movq	$51, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_MEM
	movq	8(%rbx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %rbx
	movq	%rcx, %rdi
	callq	get_alias_set
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	global_trees+248(%rip), %rax
	movl	64(%rax), %eax
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_mem_align
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	emit_block_move
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_616:                             # %sw.bb311
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_618
# BB#617:                               # %lor.lhs.false.i1473
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_618
# BB#619:                               # %if.end.i1480
	movq	32(%rax), %r14
	movzbl	16(%r14), %eax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx), %rbx
	cmpq	$25, %rax
	je	.LBB13_621
# BB#620:                               # %if.then8.i1481
	movabsq	$.L.str114, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	error
	movq	global_trees+88(%rip), %r14
.LBB13_621:                             # %if.end9.i1483
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	expand_expr
	movq	%rax, %r15
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB13_651
# BB#622:                               # %if.then13.i1490
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	%rax, %r15
	movzbl	2(%r15), %eax
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	expand_expr
	movq	%rax, %r14
	movabsq	$4294967217, %rax       # imm = 0xFFFFFFB1
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	emit_note
	movq	%rax, %rbx
	movq	$103, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 32(%rbx)
	jmp	.LBB13_651
.LBB13_16:                              # %if.then15.i
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	copy_node
	movq	%rax, %r12
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, 40(%r12)
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	callq	save_expr
	movq	%rax, 32(%rbx)
	movq	%rbx, %rdi
	callq	copy_node
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	32(%rax), %rax
.LBB13_17:                              # %if.end26.i
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	expand_expr
	movq	8(%r12), %rcx
	movl	60(%rcx), %edx
	movq	%rax, %rbx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	%rdx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
	callq	emit_queue
	callq	start_sequence
	movl	56(%r14), %eax
	cmpq	$46, %rax
	ja	.LBB13_23
# BB#18:                                # %if.end26.i
	movq	$1, %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	movabsq	$20066087206912, %rcx   # imm = 0x124000000000
	testq	%rcx, %rsi
	jne	.LBB13_24
# BB#19:                                # %if.end26.i
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$40132174413824, %rcx   # imm = 0x248000000000
	testq	%rcx, %rdx
	je	.LBB13_21
# BB#20:
	movabsq	$optab_table+312, %rax
	jmp	.LBB13_25
.LBB13_533:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_536:                             # %if.then12.i1321
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %r14
	jmp	.LBB13_581
.LBB13_539:                             # %if.then2.i.i1328
	movq	40(%r12), %r14
.LBB13_543:                             # %c_getstr.exit.i1342
	leaq	144(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_550
# BB#544:                               # %if.end.i206.i
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_545
# BB#546:                               # %if.else.i211.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_550
# BB#547:                               # %lor.lhs.false.i216.i
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_550
# BB#548:                               # %if.end10.i220.i
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
	jmp	.LBB13_549
.LBB13_367:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_369:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_371:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_373:                             # %if.then2.i.i989
	movq	40(%r12), %rax
	jmp	.LBB13_379
.LBB13_375:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_378:                             # %if.end10.i.i1001
	movq	40(%r12), %r12
	movq	616(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
.LBB13_379:                             # %c_getstr.exit.i1004
	movq	%rax, 72(%rsp)          # 8-byte Spill
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_380
# BB#381:                               # %if.then7.i1005
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_382
# BB#383:                               # %if.end11.i1009
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	tree_low_cst
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB13_384
# BB#385:                               # %if.end16.i1019
	movq	8(%r14), %r12
	movq	72(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB13_386
.LBB13_388:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_390:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_393:                             # %if.then2.i.i1038
	movq	40(%r12), %rax
.LBB13_397:                             # %c_getstr.exit.i1053
	movq	%rax, 64(%rsp)          # 8-byte Spill
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB13_403
# BB#398:                               # %if.then7.i1054
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_399
# BB#400:                               # %if.end11.i1060
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	tree_low_cst
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	strrchr
	testq	%rax, %rax
	je	.LBB13_401
# BB#402:                               # %if.end16.i1070
	movq	8(%r14), %r12
	movq	64(%rsp), %rcx          # 8-byte Reload
.LBB13_386:                             # %expand_builtin_strpbrk.exit
	subq	%rcx, %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	build
	jmp	.LBB13_336
.LBB13_413:                             # %if.then16.i
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	jmp	.LBB13_210
.LBB13_417:                             # %if.then2.i.i1112
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
.LBB13_421:                             # %c_getstr.exit.i1126
	movq	%rax, 80(%rsp)          # 8-byte Spill
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB13_426
# BB#422:                               # %land.lhs.true27.i
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB13_426
# BB#423:                               # %land.lhs.true29.i
	movq	8(%rbx), %r15
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	strlen
	incq	%rax
	cmpq	%rax, %r15
	ja	.LBB13_426
# BB#424:                               # %land.lhs.true32.i
	movabsq	$builtin_memcpy_read_str, %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	callq	can_store_by_pieces
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_426
# BB#425:                               # %if.then38.i
	movq	8(%rbx), %rax
	movabsq	$builtin_memcpy_read_str, %rcx
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %r8           # 8-byte Reload
	callq	store_by_pieces
	jmp	.LBB13_427
.LBB13_214:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_216:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_218:                             # %if.then2.i.i694
	movq	40(%r13), %rax
	jmp	.LBB13_224
.LBB13_220:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_223:                             # %if.end10.i.i706
	movq	40(%r13), %r12
	movq	576(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
.LBB13_224:                             # %c_getstr.exit.i708
	testq	%rax, %rax
	je	.LBB13_225
# BB#226:                               # %land.lhs.true.i710
	movzbl	(%rax), %eax
	testq	%rax, %rax
	movabsq	$0, %rsi
	jne	.LBB13_648
# BB#227:                               # %expand_builtin_strcat.exit
	xorq	%rcx, %rcx
	movq	%r14, %rdi
.LBB13_228:                             # %expand_builtin_strcat.exit
	movq	%r15, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_expr
.LBB13_229:                             # %expand_builtin_strcat.exit
	xorq	%rsi, %rsi
	movq	%rax, %r15
	jmp	.LBB13_253
.LBB13_233:                             # %if.then2.i.i726
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %r12
.LBB13_237:                             # %c_getstr.exit.i742
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_241
# BB#238:                               # %lor.lhs.false.i743
	testq	%r12, %r12
	je	.LBB13_239
# BB#240:                               # %land.lhs.true.i745
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB13_241
# BB#242:                               # %if.end.i751
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB13_243
# BB#244:                               # %land.lhs.true23.i
	movq	%r12, %rdi
	callq	strlen
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rsi
	js	.LBB13_648
# BB#245:                               # %if.then28.i756
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	build_tree_list
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rsi, %rsi
	movq	built_in_decls+200(%rip), %r12
	jmp	.LBB13_246
.LBB13_184:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_186:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_193:                             # %if.end21.i
	testq	%r12, %r12
	je	.LBB13_409
# BB#194:                               # %lor.lhs.false.i
	movzbl	16(%r12), %eax
	cmpq	$25, %rax
	jne	.LBB13_409
# BB#195:                               # %if.end29.i
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_207
# BB#196:                               # %if.then34.i
	movq	%r13, %rbx
	movq	32(%rbx), %r12
	movq	%r12, %rdi
	callq	get_pointer_alignment
	movq	(%rbx), %rcx
	movq	32(%rcx), %rcx
	movq	%rax, %rbx
	leaq	568(%rsp), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB13_409
# BB#197:                               # %if.end.i.i658
	movq	568(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_198
# BB#199:                               # %if.else.i.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_409
# BB#200:                               # %lor.lhs.false.i.i
	movl	32(%r13), %eax
	movq	568(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_409
# BB#201:                               # %if.end10.i.i
	movq	40(%r13), %r15
	movq	568(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r13
	addq	%r15, %r13
	jmp	.LBB13_202
.LBB13_434:                             # %if.then2.i.i1163
	movq	40(%r12), %r14
.LBB13_438:                             # %c_getstr.exit.i1177
	leaq	144(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_460
# BB#439:                               # %if.end.i139.i
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_440
# BB#441:                               # %if.else.i144.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_460
# BB#442:                               # %lor.lhs.false.i149.i
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_443
.LBB13_460:                             # %c_getstr.exit155.thread.i
	xorq	%rax, %rax
	testq	%r14, %r14
	movabsq	$0, %rcx
	jne	.LBB13_456
	jmp	.LBB13_461
.LBB13_486:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_489:                             # %if.then13.i
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	expand_expr
	jmp	.LBB13_363
.LBB13_492:                             # %if.then2.i.i1243
	movq	40(%r12), %r14
.LBB13_496:                             # %c_getstr.exit.i1257
	leaq	144(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_503
# BB#497:                               # %if.end.i161.i
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_498
# BB#499:                               # %if.else.i166.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_503
# BB#500:                               # %lor.lhs.false.i171.i
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_503
# BB#501:                               # %if.end10.i175.i
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
	jmp	.LBB13_502
.LBB13_165:                             # %if.end15.i
	testq	%r15, %r15
	je	.LBB13_169
# BB#166:                               # %land.lhs.true.i
	movl	(%r15), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB13_169
# BB#167:                               # %land.lhs.true22.i
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	%rdx, %rsi
	jne	.LBB13_169
# BB#168:                               # %land.lhs.true28.i
	movl	8(%r15), %ecx
	cmpq	$52, %rcx
	movq	%r15, %r14
	ja	.LBB13_170
.LBB13_169:                             # %if.then32.i
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB13_170:                             # %if.end34.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	get_last_insn
	movq	72(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rcx,4), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rcx,8), %rcx
	movq	const_int_rtx+512(%rip), %rbx
	movzbl	64(%rcx), %edx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	callq	*48(%rcx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_172
# BB#171:                               # %if.then49.i
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
.LBB13_172:                             # %if.end51.i
	movq	$51, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_MEM
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	56(%rsp), %rbx          # 8-byte Reload
	callq	*insn_data+16(,%rbx,8)
	testq	%rax, %rax
	je	.LBB13_409
# BB#173:                               # %if.end60.i
	movq	%rax, %rdi
	callq	emit_insn
	callq	start_sequence
	movl	ptr_mode(%rip), %eax
	movq	$1, %rcx
	movq	%r13, %rdi
	movq	64(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	expand_expr
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	memory_address
	cmpq	%r13, %rax
	movq	%r13, %rcx
	je	.LBB13_175
# BB#174:                               # %if.then66.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB13_175:                             # %if.end68.i
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB13_177
# BB#176:                               # %if.then71.i
	movq	%r13, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	emit_insn_after
	jmp	.LBB13_178
.LBB13_307:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_309:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_311:                             # %if.then2.i.i866
	movq	40(%r14), %r14
	jmp	.LBB13_317
.LBB13_313:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_316:                             # %if.end10.i.i878
	movq	40(%r14), %r12
	movq	584(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
.LBB13_317:                             # %c_getstr.exit.i881
	testq	%r14, %r14
	je	.LBB13_318
# BB#319:                               # %if.end.i882
	leaq	592(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_328
# BB#320:                               # %if.end.i63.i
	movq	592(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_321
# BB#322:                               # %if.else.i68.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_328
# BB#323:                               # %lor.lhs.false.i73.i
	movl	32(%r12), %eax
	movq	592(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_328
# BB#324:                               # %if.end10.i77.i
	movq	40(%r12), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	592(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r12
	movq	72(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r12
	jmp	.LBB13_325
.LBB13_340:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_342:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_344:                             # %if.then2.i.i917
	movq	40(%r14), %r14
	jmp	.LBB13_350
.LBB13_346:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_349:                             # %if.end10.i.i929
	movq	40(%r14), %r12
	movq	600(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r14
	addq	%r12, %r14
.LBB13_350:                             # %c_getstr.exit.i932
	testq	%r14, %r14
	je	.LBB13_351
# BB#352:                               # %if.end.i933
	leaq	608(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_362
# BB#353:                               # %if.end.i61.i
	movq	608(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_354
# BB#355:                               # %if.else.i66.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_362
# BB#356:                               # %lor.lhs.false.i71.i
	movl	32(%r12), %eax
	movq	608(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_362
# BB#357:                               # %if.end10.i75.i
	movq	40(%r12), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	608(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r12
	movq	72(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r12
	jmp	.LBB13_358
.LBB13_257:                             # %if.then2.i.i778
	movq	40(%r12), %r14
.LBB13_261:                             # %c_getstr.exit.i792
	leaq	144(%rsp), %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_652
# BB#262:                               # %if.end.i36.i
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_263
# BB#264:                               # %if.else.i41.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_652
# BB#265:                               # %lor.lhs.false.i46.i
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_266
.LBB13_652:                             # %c_getstr.exit52.thread.i
	xorq	%rax, %rax
	testq	%r14, %r14
	movabsq	$0, %rcx
	movabsq	$0, %rsi
	jne	.LBB13_275
	jmp	.LBB13_648
.LBB13_282:                             # %if.then2.i.i814
	movq	40(%r12), %r14
.LBB13_286:                             # %c_getstr.exit.i828
	leaq	144(%rsp), %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	string_constant
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB13_653
# BB#287:                               # %if.end.i46.i
	movq	144(%rsp), %rax
	testq	%rax, %rax
	je	.LBB13_288
# BB#289:                               # %if.else.i51.i
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_653
# BB#290:                               # %lor.lhs.false.i56.i
	movl	32(%r12), %eax
	movq	144(%rsp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_291
.LBB13_653:                             # %c_getstr.exit62.thread.i
	xorq	%rax, %rax
	testq	%r14, %r14
	movabsq	$0, %rcx
	movabsq	$0, %rsi
	jne	.LBB13_299
	jmp	.LBB13_648
.LBB13_123:                             # %if.end.i541
	xorq	%rdi, %rdi
	movq	$-1, %rsi
	callq	gen_rtx_CONST_INT
	jmp	.LBB13_650
.LBB13_103:                             # %sw.bb1.i.i
	movq	$1, %rax
	jmp	.LBB13_121
.LBB13_110:                             # %sw.bb8.i.i
	movq	$8, %rax
	jmp	.LBB13_121
.LBB13_111:                             # %sw.bb9.i.i
	movq	$9, %rax
	jmp	.LBB13_121
.LBB13_120:                             # %sw.default.i.i
	movq	$-1, %rax
	jmp	.LBB13_121
.LBB13_105:                             # %sw.bb3.i.i
	movq	$3, %rax
	jmp	.LBB13_121
.LBB13_106:                             # %sw.bb4.i.i
	movq	$4, %rax
	jmp	.LBB13_121
.LBB13_104:                             # %sw.bb2.i.i
	movq	$2, %rax
	jmp	.LBB13_121
.LBB13_107:                             # %sw.bb5.i.i
	movq	$5, %rax
	jmp	.LBB13_121
.LBB13_109:                             # %sw.bb7.i.i
	movq	$7, %rax
	jmp	.LBB13_121
.LBB13_108:                             # %sw.bb6.i.i
	movq	$6, %rax
	jmp	.LBB13_121
.LBB13_113:                             # %sw.bb11.i.i
	movq	$11, %rax
	jmp	.LBB13_121
.LBB13_118:                             # %sw.bb19.i.i
	movq	$17, %rax
	jmp	.LBB13_121
.LBB13_116:                             # %sw.bb14.i.i
	movzbl	62(%rcx), %eax
	andq	$1, %rax
	orq	$14, %rax
	jmp	.LBB13_121
.LBB13_117:                             # %sw.bb18.i.i
	movq	$16, %rax
	jmp	.LBB13_121
.LBB13_114:                             # %sw.bb12.i.i
	movq	$12, %rax
	jmp	.LBB13_121
.LBB13_112:                             # %sw.bb10.i.i
	movq	$10, %rax
	jmp	.LBB13_121
.LBB13_119:                             # %sw.bb20.i.i
	movq	$18, %rax
	jmp	.LBB13_121
.LBB13_98:                              # %if.else15.i
	movabsq	$.L.str125, %rcx
	jmp	.LBB13_94
.LBB13_93:                              # %if.then2.i
	movabsq	$.L.str123, %rcx
	jmp	.LBB13_94
.LBB13_97:                              # %if.else10.i
	movslq	88(%rbx,%rax,4), %rax
.LBB13_121:                             # %type_to_class.exit.i
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	jmp	.LBB13_650
.LBB13_125:                             # %if.then.i546
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_128:                             # %if.then.i560
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_130:                             # %if.then8.i
	andq	%r12, %rbx
	cmpq	$52, %rbx
	jne	.LBB13_132
# BB#131:                               # %if.then10.i
	movabsq	$.L.str116, %rcx
	jmp	.LBB13_94
.LBB13_134:                             # %if.then22.i
	andq	%r12, %rcx
	cmpq	$52, %rcx
	jne	.LBB13_137
# BB#135:                               # %if.then28.i
	movabsq	$.L.str118, %rcx
	jmp	.LBB13_136
.LBB13_142:                             # %if.end38.i
	cmpq	$134, %rax
	je	.LBB13_651
# BB#143:                               # %if.end38.i
	cmpq	$140, %rax
	je	.LBB13_651
.LBB13_144:                             # %if.then79.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	copy_to_mode_reg
	jmp	.LBB13_650
.LBB13_150:                             # %if.then128
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_33:
	movabsq	$0, %rax
.LBB13_35:                              # %if.end.i475
	movq	%rax, %rcx
	shlq	%cl, %r15
	xorq	%r12, %r12
	movabsq	$4294967288, %r13       # imm = 0xFFFFFFF8
.LBB13_36:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movl	apply_args_mode(,%r12,4), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB13_40
# BB#37:                                # %if.then15.i.i
                                        #   in Loop: Header=BB13_36 Depth=1
	movl	apply_args_mode(,%r12,4), %eax
	movq	%rax, %rdi
	callq	get_mode_alignment
	movq	%rax, %rsi
	andq	%r13, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	testq	%r14, %rax
	je	.LBB13_39
# BB#38:                                # %if.then19.i.i
                                        #   in Loop: Header=BB13_36 Depth=1
	leaq	-1(%r15,%rsi), %r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	subq	%rax, %r15
.LBB13_39:                              # %if.end22.i.i
                                        #   in Loop: Header=BB13_36 Depth=1
	movl	apply_args_mode(,%r12,4), %eax
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_REG
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movl	apply_args_mode(,%r12,4), %edx
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	adjust_address_1
	movq	%rax, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	addq	%rax, %r15
.LBB13_40:                              # %for.inc.i.i
                                        #   in Loop: Header=BB13_36 Depth=1
	incq	%r12
	cmpq	$53, %r12
	jne	.LBB13_36
# BB#41:                                # %for.end.i.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	movq	global_rtl+48(%rip), %rcx
	movq	%rax, %r15
	movq	%rcx, %rdi
	callq	copy_to_reg
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	cmpq	$0, struct_value_incoming_rtx(%rip)
	je	.LBB13_43
# BB#42:                                # %if.then45.i.i
	andq	$1, %rax
	orq	$4, %rax
	andq	%rax, %r14
	movzbl	mode_size(%r14), %edx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	movq	struct_value_incoming_rtx(%rip), %rcx
	movq	%rax, %r14
	movq	%rcx, %rdi
	callq	copy_to_reg
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB13_43:                              # %expand_builtin_apply_args_1.exit.i
	movq	8(%rbx), %rax
	movq	%rax, %rdi
	callq	copy_addr_to_reg
	movq	%rax, %r15
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	%r15, 24(%rax)
	callq	push_topmost_sequence
	callq	get_insns
	movq	24(%rax), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_insns_before
	callq	pop_topmost_sequence
	jmp	.LBB13_651
.LBB13_52:                              # %if.then34
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_50:
	movabsq	$0, %rax
.LBB13_54:                              # %for.end
	movq	%rax, %rcx
	shlq	%cl, 80(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967288, %r12       # imm = 0xFFFFFFF8
.LBB13_55:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	apply_args_mode(,%r13,4), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB13_59
# BB#56:                                # %if.then40.i
                                        #   in Loop: Header=BB13_55 Depth=1
	movl	apply_args_mode(,%r13,4), %eax
	movq	%rax, %rdi
	callq	get_mode_alignment
	movq	%rax, %rsi
	andq	%r12, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$32, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	testq	%r14, %rax
	je	.LBB13_58
# BB#57:                                # %if.then44.i
                                        #   in Loop: Header=BB13_55 Depth=1
	movq	80(%rsp), %rdi          # 8-byte Reload
	leaq	-1(%rdi,%rsi), %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	subq	%rax, %rdi
	movq	%rdi, 80(%rsp)          # 8-byte Spill
.LBB13_58:                              # %if.end47.i
                                        #   in Loop: Header=BB13_55 Depth=1
	movl	apply_args_mode(,%r13,4), %eax
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_REG
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rcx
	movq	$1, %r8
	movq	88(%rsp), %rdi          # 8-byte Reload
	movl	apply_args_mode(,%r13,4), %edx
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	adjust_address_1
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
	leaq	128(%rsp), %rax
	movq	%rax, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	use_reg
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	addq	%rax, 80(%rsp)          # 8-byte Folded Spill
.LBB13_59:                              # %for.inc.i
                                        #   in Loop: Header=BB13_55 Depth=1
	incq	%r13
	cmpq	$53, %r13
	jne	.LBB13_55
# BB#60:                                # %for.end.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	cmpq	$0, struct_value_rtx(%rip)
	je	.LBB13_63
# BB#61:                                # %if.then64.i
	andq	$1, %rax
	orq	$4, %rax
	movq	%rax, %r13
	andq	%r14, %r13
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movl	target_flags(%rip), %edx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$25, %rcx
	movzbl	mode_size(%r13), %eax
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	adjust_address_1
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	struct_value_rtx(%rip), %rax
	movq	%rax, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
	movq	struct_value_rtx(%rip), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB13_63
# BB#62:                                # %if.then78.i
	leaq	128(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	use_reg
.LBB13_63:                              # %if.end87.i
	leaq	128(%rsp), %rax
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r13, %r13
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	prepare_call_address
	movzbl	(%rax), %ecx
	cmpq	$68, %rcx
	je	.LBB13_65
# BB#64:                                # %if.then93.i
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	memory_address
.LBB13_65:                              # %if.end95.i
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r15
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
.LBB13_66:                              # %for.body.i.i489
                                        # =>This Inner Loop Header: Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	apply_result_mode(,%rax,4), %edx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB13_70
# BB#67:                                # %if.then.i.i
                                        #   in Loop: Header=BB13_66 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	get_mode_alignment
	movq	%rax, %rsi
	andq	%r12, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	testq	%r14, %rax
	je	.LBB13_69
# BB#68:                                # %if.then3.i.i
                                        #   in Loop: Header=BB13_66 Depth=1
	leaq	-1(%r13,%rsi), %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rax
	subq	%rax, %r13
.LBB13_69:                              # %if.end.i.i
                                        #   in Loop: Header=BB13_66 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_REG
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	adjust_address_1
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	$32, %rcx
	movq	80(%rsp), %rdi          # 8-byte Reload
	leaq	1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	72(%rsp), %rcx          # 8-byte Reload
	movzbl	mode_size(%rcx), %esi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rax, 144(%rsp,%rdi,8)
	addq	%rsi, %r13
	movq	%rdx, 80(%rsp)          # 8-byte Spill
.LBB13_70:                              # %for.inc.i.i500
                                        #   in Loop: Header=BB13_66 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	cmpq	$53, %rax
	jne	.LBB13_66
# BB#71:                                # %result_vector.exit.i
	leaq	144(%rsp), %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	gen_rtvec_v
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_untyped_call
	movq	%rax, %rdi
	callq	emit_call_insn
	callq	get_last_insn
	jmp	.LBB13_72
.LBB13_591:                             # %if.end266
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_builtin_longjmp
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_627:                             # %if.else21.i
	movq	global_trees+88(%rip), %r14
.LBB13_628:                             # %if.end23.i
	movq	$3, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, %r15
.LBB13_629:                             # %if.end23.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	expand_expr
	movzbl	16(%r14), %ecx
	movq	%rax, %rbx
	cmpq	$25, %rcx
	je	.LBB13_631
# BB#630:                               # %if.then27.i
	movabsq	$.L.str110, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	error
	movq	global_trees+88(%rip), %r14
.LBB13_631:                             # %if.end28.i1512
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	expand_expr
	movq	%rax, %r14
	movq	8(%r14), %rax
	cmpq	$2, %rax
	jb	.LBB13_633
# BB#632:                               # %if.then35.i
	movabsq	$.L.str111, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	warning
	movq	const_int_rtx+512(%rip), %r14
.LBB13_633:                             # %if.end36.i
	movzbl	16(%r15), %eax
	cmpq	$25, %rax
	je	.LBB13_635
# BB#634:                               # %if.then42.i
	movabsq	$.L.str112, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	error
	movq	global_trees+88(%rip), %r15
.LBB13_635:                             # %if.end43.i
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	expand_expr
	movq	%rax, %r15
	movq	8(%r15), %rax
	cmpq	$4, %rax
	jb	.LBB13_637
# BB#636:                               # %if.then53.i1515
	movabsq	$.L.str113, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	warning
	movq	const_int_rtx+512(%rip), %r15
.LBB13_637:                             # %if.end54.i1516
	movl	x86_prefetch_sse(%rip), %eax
	testq	%rax, %rax
	jne	.LBB13_639
# BB#638:                               # %lor.lhs.false56.i
	movzbl	target_flags+2(%rip), %eax
	testq	$16, %rax
	je	.LBB13_642
.LBB13_639:                             # %if.then59.i
	movq	insn_data+52664(%rip), %rax
	movzbl	16(%rax), %ecx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	*(%rax)
	testq	%r12, %rax
	jne	.LBB13_641
# BB#640:                               # %if.then65.i
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB13_641:                             # %if.end70.i
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	gen_prefetch
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB13_643
.LBB13_614:                             # %if.then.i1458
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_618:                             # %if.then.i1474
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_24:                              # %sw.bb36.i
	movabsq	$optab_table+304, %rax
	jmp	.LBB13_25
.LBB13_21:                              # %if.end26.i
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$80264348827648, %rax   # imm = 0x490000000000
	testq	%rax, %rdx
	je	.LBB13_23
# BB#22:                                # %sw.bb35.i
	movabsq	$optab_table+320, %rax
.LBB13_25:                              # %sw.epilog.i
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	60(%rcx), %edx
	movq	(%rax), %rax
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	xorq	%r8, %r8
	movq	%rdx, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	callq	expand_unop
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB13_26
# BB#27:                                # %if.end49.i
	movl	flag_errno_math(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_30
# BB#28:                                # %if.end49.i
	movl	flag_unsafe_math_optimizations(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_30
# BB#29:                                # %if.then53.i
	callq	gen_label_rtx
	movzbl	2(%r15), %r8d
	movq	%rax, %r14
	movq	$103, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r14, (%rsp)
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	emit_cmp_and_jump_insns
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	expand_call
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movl	%ecx, 4(%rax)
	movq	%r14, %rdi
	callq	emit_label
.LBB13_30:                              # %expand_builtin_mathfn.exit
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	%r14, %rdi
	callq	emit_insns
	jmp	.LBB13_651
.LBB13_545:                             # %if.then2.i208.i
	movq	40(%r12), %rax
.LBB13_549:                             # %c_getstr.exit222.i
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB13_550:                             # %c_getstr.exit222.i
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_561
# BB#551:                               # %c_getstr.exit222.i
	testq	%r14, %r14
	je	.LBB13_561
# BB#552:                               # %c_getstr.exit222.i
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB13_561
# BB#553:                               # %land.lhs.true24.i
	movq	%r14, %rdi
	callq	strlen
	leaq	1(%rax), %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_561
# BB#554:                               # %land.lhs.true28.i1351
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	strlen
	leaq	1(%rax), %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_555
.LBB13_561:                             # %if.end42.i
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_564
# BB#562:                               # %land.lhs.true45.i
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	cmpq	$1, %rax
	jne	.LBB13_564
# BB#563:                               # %if.then48.i1371
	movq	integer_types+16(%rip), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	build_qualified_type
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	build_pointer_type
	movq	integer_types+40(%rip), %r12
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$115, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	$41, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	$114, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	movq	$115, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	build1
	movq	$41, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	$114, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	integer_types+40(%rip), %rcx
	movq	%rax, %r8
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r8, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_expr
	jmp	.LBB13_580
.LBB13_380:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_382:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_384:                             # %if.then15.i1010
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_338
.LBB13_403:                             # %if.end22.i1073
	movq	%r13, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_404
# BB#405:                               # %if.end26.i1074
	movq	built_in_decls+288(%rip), %r14
	testq	%r14, %r14
	je	.LBB13_406
# BB#407:                               # %if.end29.i1083
	movq	8(%r14), %rax
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	build1
	movq	8(%r14), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	72(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB13_335
.LBB13_399:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_401:                             # %if.then15.i1061
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_338
.LBB13_426:                             # %if.end45.i
	movq	%r14, %rdi
	callq	get_memory_rtx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	set_mem_align
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	emit_block_move
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB13_651
	jmp	.LBB13_427
.LBB13_225:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_239:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_241:                             # %if.then14.i
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	expand_expr
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	jmp	.LBB13_250
.LBB13_243:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_207:                             # %if.end58.i
	movq	built_in_decls+176(%rip), %r14
	testq	%r14, %r14
	je	.LBB13_409
# BB#208:                               # %if.end61.i
	movq	8(%r14), %rax
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	build1
	movq	8(%r14), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r13, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	%rax, %rdi
	callq	fold
.LBB13_209:                             # %expand_builtin_strncpy.exit
	xorq	%rcx, %rcx
	movq	%rax, %rdi
.LBB13_210:                             # %expand_builtin_strncpy.exit
	movq	%r15, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_expr
	jmp	.LBB13_211
.LBB13_198:                             # %if.then2.i.i
	movq	40(%r13), %r13
.LBB13_202:                             # %c_getstr.exit.i
	testq	%r13, %r13
	je	.LBB13_409
# BB#203:                               # %c_getstr.exit.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB13_409
# BB#204:                               # %lor.lhs.false46.i
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_409
# BB#205:                               # %lor.lhs.false49.i
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	tree_low_cst
	movabsq	$builtin_strncpy_read_str, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	can_store_by_pieces
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_409
# BB#206:                               # %if.end54.i
	movq	%r12, %rdi
	callq	get_memory_rtx
	movq	%rax, %r12
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	tree_low_cst
	movabsq	$builtin_strncpy_read_str, %rcx
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	store_by_pieces
.LBB13_427:                             # %if.then49.i1135
	movq	8(%r12), %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
.LBB13_211:                             # %expand_builtin_strncpy.exit
	movq	%rax, %r15
	testq	%r15, %r15
	jmp	.LBB13_212
.LBB13_440:                             # %if.then2.i141.i
	movq	40(%r12), %rax
	jmp	.LBB13_444
.LBB13_443:                             # %if.end10.i153.i
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
.LBB13_444:                             # %c_getstr.exit155.i
	testq	%rax, %rax
	je	.LBB13_445
# BB#446:                               # %c_getstr.exit155.i
	movabsq	$1, %rcx
	jmp	.LBB13_447
.LBB13_498:                             # %if.then2.i163.i
	movq	40(%r12), %rax
.LBB13_502:                             # %c_getstr.exit177.i
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB13_503:                             # %c_getstr.exit177.i
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_512
# BB#504:                               # %c_getstr.exit177.i
	testq	%r14, %r14
	je	.LBB13_512
# BB#505:                               # %c_getstr.exit177.i
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB13_512
# BB#506:                               # %if.then25.i1264
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	movq	%r14, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	strncmp
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB13_507
# BB#508:                               # %cond.false.i1267
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_509
# BB#510:                               # %cond.false.i1267
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %r15
	jmp	.LBB13_338
.LBB13_177:                             # %if.else73.i
	callq	get_insns
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_insn_before
.LBB13_178:                             # %if.end76.i
	movzbl	2(%r14), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	je	.LBB13_182
# BB#179:                               # %if.else83.i
	testq	%r15, %r15
	jne	.LBB13_180
# BB#181:                               # %if.else87.i
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	convert_to_mode
	movq	%rax, %r14
.LBB13_182:                             # %expand_builtin_strlen.exit
	testq	%r14, %r14
	movq	%r14, %r15
.LBB13_212:                             # %expand_builtin_strncpy.exit
	movabsq	$0, %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	jne	.LBB13_651
	jmp	.LBB13_648
.LBB13_318:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_321:                             # %if.then2.i65.i
	movq	40(%r12), %r12
.LBB13_325:                             # %c_getstr.exit79.i
	testq	%r12, %r12
	je	.LBB13_328
# BB#326:                               # %if.then7.i885
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	strstr
	testq	%rax, %rax
	jne	.LBB13_361
# BB#327:                               # %if.then10.i886
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_338
.LBB13_351:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_354:                             # %if.then2.i63.i
	movq	40(%r12), %r12
.LBB13_358:                             # %c_getstr.exit77.i
	testq	%r12, %r12
	je	.LBB13_362
# BB#359:                               # %if.then7.i937
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	strpbrk
	testq	%rax, %rax
	je	.LBB13_360
.LBB13_361:                             # %if.end11.i
	movq	8(%r13), %r14
	subq	%r12, %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	build
	jmp	.LBB13_336
.LBB13_263:                             # %if.then2.i38.i
	movq	40(%r12), %rax
	jmp	.LBB13_267
.LBB13_266:                             # %if.end10.i50.i
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
.LBB13_267:                             # %c_getstr.exit52.i
	testq	%rax, %rax
	je	.LBB13_268
# BB#269:                               # %c_getstr.exit52.i
	movabsq	$1, %rcx
	jmp	.LBB13_270
.LBB13_288:                             # %if.then2.i48.i
	movq	40(%r12), %rax
	jmp	.LBB13_292
.LBB13_291:                             # %if.end10.i60.i
	movq	40(%r12), %r12
	movq	144(%rsp), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	addq	%r12, %rax
.LBB13_292:                             # %c_getstr.exit62.i
	testq	%rax, %rax
	je	.LBB13_293
# BB#294:                               # %c_getstr.exit62.i
	movabsq	$1, %rcx
	jmp	.LBB13_295
.LBB13_132:                             # %if.else11.i
	movabsq	$.L.str117, %rcx
.LBB13_94:                              # %return
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	error
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_137:                             # %if.else29.i
	movabsq	$.L.str119, %rcx
.LBB13_136:                             # %return
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	warning
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_74:                              # %for.inc108.i
                                        #   in Loop: Header=BB13_72 Depth=1
	movq	16(%rax), %rax
.LBB13_72:                              # %result_vector.exit.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB13_75
# BB#73:                                # %land.rhs.i
                                        #   in Loop: Header=BB13_72 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	jne	.LBB13_74
# BB#76:                                # %if.end113.i
	movq	64(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB13_79
.LBB13_77:                              # %for.cond122.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB13_77
# BB#78:                                # %for.end133.i
	movq	128(%rsp), %rcx
	movq	%rcx, 16(%rax)
	jmp	.LBB13_80
.LBB13_642:                             # %if.else73.i1525
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movq	%rax, %rbx
.LBB13_643:                             # %if.end75.i1526
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	je	.LBB13_646
# BB#644:                               # %land.lhs.true79.i1529
	movq	%rbx, %rdi
	callq	side_effects_p
	testq	%r12, %rax
	je	.LBB13_646
# BB#645:                               # %if.then82.i
	movq	%rbx, %rdi
	callq	emit_insn
.LBB13_646:                             # %expand_builtin_prefetch.exit
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_651
.LBB13_23:                              # %sw.default.i
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin_mathfn, %rcx
	movq	$1516, %rsi             # imm = 0x5EC
	jmp	.LBB13_10
.LBB13_26:                              # %if.then48.i
	callq	end_sequence
.LBB13_409:
	xorq	%rsi, %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB13_648
.LBB13_555:                             # %if.then33.i
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	movq	%r14, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	memcmp
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB13_556
# BB#557:                               # %cond.false.i1356
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_558
# BB#559:                               # %cond.false.i1356
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %r14
	jmp	.LBB13_581
.LBB13_564:                             # %if.end62.i1377
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	get_pointer_alignment
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$60, %rcx
	shrq	%cl, %r12
	andq	$7, %r12
	addq	%r14, %r12
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$35, %rcx
	sarq	%cl, %r12
	movq	%r13, %rdi
	callq	get_pointer_alignment
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$60, %rcx
	shrq	%cl, %rdx
	andq	$7, %rdx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$35, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	addq	$7, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	cmpq	$15, %r14
	jb	.LBB13_565
# BB#566:                               # %if.end62.i1377
	addq	$7, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$15, %rax
	movabsq	$0, %rsi
	jb	.LBB13_648
# BB#567:                               # %if.end71.i
	movq	insn_data+48144(%rip), %rax
	movzbl	16(%rax), %eax
	testq	%r15, %r15
	je	.LBB13_571
# BB#568:                               # %land.lhs.true73.i
	movl	(%r15), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB13_571
# BB#569:                               # %land.lhs.true76.i
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rax, %rdx
	jne	.LBB13_571
# BB#570:                               # %land.lhs.true80.i
	movl	8(%r15), %ecx
	cmpq	$52, %rcx
	movq	%r15, %r14
	ja	.LBB13_572
.LBB13_571:                             # %if.then83.i
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB13_572:                             # %if.end85.i
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	get_memory_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	get_memory_rtx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r13
	movq	80(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jle	.LBB13_574
# BB#573:                               # %if.end85.i
	movq	80(%rsp), %r12          # 8-byte Reload
.LBB13_574:                             # %if.end85.i
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%rax, %r8
	callq	gen_cmpstrsi
	testq	%rax, %rax
	je	.LBB13_576
# BB#575:                               # %if.then97.i
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB13_577
.LBB13_404:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_406:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_445:
	movabsq	$0, %rcx
.LBB13_447:                             # %c_getstr.exit155.i
	testq	%r14, %r14
	je	.LBB13_455
# BB#448:                               # %c_getstr.exit155.i
	testq	%rax, %rax
	je	.LBB13_455
# BB#449:                               # %if.then8.i1182
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB13_450
# BB#451:                               # %cond.false.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB13_452
# BB#453:                               # %cond.false.i
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %r15
	jmp	.LBB13_252
.LBB13_512:                             # %if.end34.i1269
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_520
# BB#513:                               # %land.lhs.true37.i
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	cmpq	$1, %rax
	je	.LBB13_519
# BB#514:                               # %lor.lhs.false.i1272
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	tree_low_cst
	cmpq	$2, %rax
	jl	.LBB13_520
# BB#515:                               # %land.lhs.true42.i
	testq	%r14, %r14
	je	.LBB13_517
# BB#516:                               # %land.lhs.true44.i
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB13_519
.LBB13_517:                             # %lor.lhs.false47.i
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB13_520
# BB#518:                               # %land.lhs.true49.i
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB13_519
.LBB13_520:                             # %if.end67.i
	testq	%r14, %r14
	je	.LBB13_522
# BB#521:                               # %if.then69.i
	movq	72(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB13_524
.LBB13_507:                             # %cond.true.i1265
	movq	const_int_rtx+504(%rip), %r15
	jmp	.LBB13_338
.LBB13_509:
	movabsq	$const_int_rtx+520, %rax
	movq	(%rax), %r15
	jmp	.LBB13_338
.LBB13_328:                             # %if.end17.i
	movsbq	(%r14), %rax
	testq	%rax, %rax
	je	.LBB13_329
# BB#330:                               # %if.end22.i
	movzbl	1(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB13_332
# BB#331:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_362:                             # %if.end17.i949
	movsbq	(%r14), %rax
	testq	%rax, %rax
	je	.LBB13_363
# BB#364:                               # %if.end22.i954
	movzbl	1(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB13_332
# BB#365:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_360:                             # %if.then10.i938
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_338
.LBB13_268:
	movabsq	$0, %rcx
.LBB13_270:                             # %c_getstr.exit52.i
	testq	%r14, %r14
	je	.LBB13_274
# BB#271:                               # %c_getstr.exit52.i
	testq	%rax, %rax
	je	.LBB13_274
# BB#272:                               # %if.then7.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strspn
	jmp	.LBB13_273
.LBB13_293:
	movabsq	$0, %rcx
.LBB13_295:                             # %c_getstr.exit62.i
	testq	%r14, %r14
	je	.LBB13_298
# BB#296:                               # %c_getstr.exit62.i
	testq	%rax, %rax
	je	.LBB13_298
# BB#297:                               # %if.then7.i835
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcspn
.LBB13_273:                             # %expand_builtin_strcspn.exit
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	jmp	.LBB13_249
.LBB13_75:                              # %if.then112.i
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin_apply, %rcx
	movq	$1306, %rsi             # imm = 0x51A
.LBB13_10:                              # %sw.bb15
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB13_79:                              # %if.else.i
	movq	128(%rsp), %rcx
	movq	%rcx, 64(%rax)
.LBB13_80:                              # %expand_builtin_apply.exit
	movq	136(%rsp), %rax
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	emit_stack_restore
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movl	%ecx, 4(%rax)
	movq	8(%rbx), %rax
	movq	%rax, %rdi
	callq	copy_addr_to_reg
	jmp	.LBB13_650
.LBB13_556:                             # %cond.true.i1354
	movq	const_int_rtx+504(%rip), %r14
	jmp	.LBB13_581
.LBB13_558:
	movabsq	$const_int_rtx+520, %rax
	movq	(%rax), %r14
	jmp	.LBB13_581
.LBB13_565:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_576:                             # %if.else.i1388
	movq	integer_types+40(%rip), %rax
	movq	libfunc_table+112(%rip), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movl	60(%rax), %r12d
	movq	$9, %rcx
	shrq	%cl, %r12
	andq	$127, %r12
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movl	target_flags(%rip), %eax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$25, %rcx
	shrq	%cl, 56(%rsp)           # 8-byte Folded Spill
	andq	$1, 56(%rsp)            # 8-byte Folded Spill
	movq	sizetype_tab(%rip), %rax
	orq	$4, 56(%rsp)            # 8-byte Folded Spill
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movl	60(%rax), %edx
	movq	$9, %rcx
	movl	16(%rax), %eax
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%rdx, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	convert_to_mode
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rcx), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	%rdx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rdx
	movq	$3, %r8
	xorq	%rax, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r12, %rcx
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	emit_library_call_value
.LBB13_577:                             # %if.end129.i
	movq	8(%rbx), %rax
	movl	60(%rax), %eax
	movzbl	2(%r14), %edx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	cmpq	%rax, %rdx
	je	.LBB13_581
# BB#578:                               # %if.else143.i
	testq	%r15, %r15
	je	.LBB13_579
.LBB13_180:                             # %expand_builtin_strlen.exit.thread1537
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	convert_move
	jmp	.LBB13_651
.LBB13_455:                             # %if.end15.i1183
	testq	%r14, %r14
	je	.LBB13_457
.LBB13_456:                             # %land.lhs.true17.i
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB13_459
.LBB13_457:                             # %lor.lhs.false.i1185
	testq	%rcx, %rcx
	je	.LBB13_461
# BB#458:                               # %land.lhs.true21.i
	movzbl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB13_459
.LBB13_461:                             # %if.end39.i
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	c_strlen
	movq	%rax, %r12
	movq	%r13, %rdi
	callq	c_strlen
	movq	%rax, 64(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	testq	%r12, %r12
	movabsq	$0, %r13
	je	.LBB13_463
# BB#462:                               # %if.then43.i
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	%rax, %r13
.LBB13_463:                             # %if.end46.i
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB13_465
# BB#464:                               # %if.then48.i1200
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rax, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	size_binop
	movq	%rax, %r14
.LBB13_465:                             # %if.end51.i1202
	testq	%r13, %r13
	je	.LBB13_467
# BB#466:                               # %lor.lhs.false53.i
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB13_467
# BB#476:                               # %if.else70.i
	testq	%r14, %r14
	je	.LBB13_480
# BB#477:                               # %land.lhs.true72.i
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB13_480
# BB#478:                               # %land.lhs.true79.i
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_480
# BB#479:                               # %land.lhs.true79.i
	movq	%r14, %r13
.LBB13_480:                             # %land.lhs.true79.i
	movq	%r13, %r14
	jmp	.LBB13_481
.LBB13_450:                             # %cond.true.i
	movq	const_int_rtx+504(%rip), %r15
	jmp	.LBB13_252
.LBB13_452:
	movabsq	$const_int_rtx+520, %rax
	movq	(%rax), %r15
	jmp	.LBB13_252
.LBB13_519:                             # %if.then53.i1283
	movq	integer_types+16(%rip), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	build_qualified_type
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	build_pointer_type
	movq	integer_types+40(%rip), %r12
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$115, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	$41, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	$114, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	movq	$115, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	build1
	movq	$41, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	$114, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	integer_types+40(%rip), %rcx
	movq	%rax, %r8
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r8, %rcx
	callq	build
	jmp	.LBB13_336
.LBB13_522:                             # %if.else.i1284
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB13_525
# BB#523:                               # %if.then72.i
	movq	%r13, %rdi
.LBB13_524:                             # %if.end75.i
	callq	c_strlen
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB13_530
.LBB13_525:                             # %land.lhs.true77.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$1, %rax
	jne	.LBB13_527
# BB#526:                               # %if.end82.i
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	c_strlen
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB13_530
.LBB13_527:                             # %land.lhs.true84.i
	movzbl	17(%r13), %eax
	testq	$1, %rax
	je	.LBB13_529
# BB#528:
	xorq	%rsi, %rsi
	jmp	.LBB13_648
.LBB13_329:                             # %if.then20.i
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	jmp	.LBB13_337
.LBB13_363:                             # %if.then20.i951
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_338
.LBB13_332:                             # %if.end28.i
	movq	built_in_decls+288(%rip), %r14
	xorq	%rsi, %rsi
	testq	%r14, %r14
	je	.LBB13_648
# BB#333:                               # %if.end31.i903
	movq	%rax, %rdi
	callq	build_int_2_wide
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	jmp	.LBB13_334
.LBB13_274:                             # %if.end.i797
	testq	%r14, %r14
	je	.LBB13_276
.LBB13_275:                             # %land.lhs.true12.i
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB13_278
.LBB13_276:                             # %lor.lhs.false.i799
	movq	%rbx, %rdx
	testq	%rcx, %rcx
	je	.LBB13_303
# BB#277:                               # %land.lhs.true15.i
	movzbl	(%rax), %eax
	testq	%rax, %rax
	movabsq	$0, %rsi
	movq	%rdx, %rbx
	jne	.LBB13_648
.LBB13_278:                             # %if.then19.i
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	jmp	.LBB13_301
.LBB13_298:                             # %if.end.i836
	testq	%r14, %r14
	je	.LBB13_302
.LBB13_299:                             # %land.lhs.true12.i838
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB13_300
.LBB13_302:                             # %if.end16.i841
	movq	%rbx, %rdx
	testq	%rcx, %rcx
	je	.LBB13_303
# BB#304:                               # %land.lhs.true18.i
	movzbl	(%rax), %eax
	testq	%rax, %rax
	movabsq	$0, %rsi
	movq	%rdx, %rbx
	jne	.LBB13_648
# BB#305:                               # %if.then22.i843
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	build_tree_list
	xorq	%rsi, %rsi
	movq	built_in_decls+248(%rip), %r12
.LBB13_246:                             # %if.then28.i756
	movq	%rax, %r14
	testq	%r12, %r12
	jne	.LBB13_247
	jmp	.LBB13_648
.LBB13_579:                             # %if.else147.i
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	convert_to_mode
.LBB13_580:                             # %expand_builtin_memcmp.exit
	movq	%rax, %r14
.LBB13_581:                             # %expand_builtin_memcmp.exit
	testq	%r14, %r14
	movabsq	$0, %rsi
	movq	%r14, %r15
	jne	.LBB13_651
	jmp	.LBB13_648
.LBB13_459:                             # %if.then25.i
	movq	integer_types+16(%rip), %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	build_qualified_type
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	build_pointer_type
	movq	integer_types+40(%rip), %r12
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$115, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	$41, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	$114, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	movq	$115, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	build1
	movq	$41, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	$114, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	integer_types+40(%rip), %rcx
	movq	%rax, %r8
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r8, %rcx
	callq	build
	jmp	.LBB13_248
.LBB13_467:                             # %if.then57.i
	testq	%r14, %r14
	je	.LBB13_475
# BB#468:                               # %land.lhs.true59.i
	movzbl	17(%r14), %eax
	testq	%r13, %r13
	je	.LBB13_469
# BB#470:                               # %land.lhs.true59.i
	movabsq	$1, %rcx
	jmp	.LBB13_471
.LBB13_529:                             # %if.end93.i
	movq	%r13, %rdi
	callq	c_strlen
	movq	%rax, %r12
	testq	%r12, %r12
	movabsq	$0, %rsi
	je	.LBB13_648
.LBB13_530:                             # %if.end96.i
	movq	built_in_decls+184(%rip), %r14
	xorq	%rsi, %rsi
	testq	%r14, %r14
	je	.LBB13_648
# BB#531:                               # %if.end99.i
	movq	$1, %rdi
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	8(%rcx), %rdx
	movq	$78, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
.LBB13_334:                             # %expand_builtin_strstr.exit
	movq	%rax, %rdx
	callq	tree_cons
	movq	8(%r14), %rcx
	movq	%rax, %r12
	movq	%rcx, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	build1
	movq	8(%r14), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rcx
.LBB13_335:                             # %expand_builtin_strstr.exit
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
.LBB13_336:                             # %expand_builtin_strstr.exit
	movq	%rax, %rdi
	callq	fold
	xorq	%rcx, %rcx
	movq	%rax, %rdi
.LBB13_337:                             # %expand_builtin_strstr.exit
	movq	%r15, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r15
.LBB13_338:                             # %expand_builtin_strstr.exit
	testq	%r15, %r15
	movabsq	$0, %rsi
	jne	.LBB13_651
	jmp	.LBB13_648
.LBB13_300:                             # %if.then14.i840
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
.LBB13_301:                             # %expand_builtin_strncat.exit
	movq	%rax, %rsi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB13_252
.LBB13_303:
	xorq	%rsi, %rsi
	movq	%rdx, %rbx
	jmp	.LBB13_648
.LBB13_475:                             # %if.else.i1208
	xorq	%rsi, %rsi
	testq	%r13, %r13
	movq	%r13, %r14
	jne	.LBB13_481
	jmp	.LBB13_648
.LBB13_469:
	movabsq	$0, %rcx
.LBB13_471:                             # %land.lhs.true59.i
	andq	$1, %rax
	je	.LBB13_473
# BB#472:                               # %land.lhs.true59.i
	movq	%r13, %r14
.LBB13_473:                             # %land.lhs.true59.i
	testq	%rax, %rax
	je	.LBB13_481
# BB#474:                               # %land.lhs.true59.i
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB13_648
.LBB13_481:                             # %if.end84.i
	movq	%rbx, %rcx
	movzbl	17(%r14), %eax
	testq	$1, %rax
	je	.LBB13_483
# BB#482:
	xorq	%rsi, %rsi
	movq	%rcx, %rbx
	jmp	.LBB13_648
.LBB13_483:                             # %if.end92.i
	movq	built_in_decls+184(%rip), %r12
	testq	%r12, %r12
	movabsq	$0, %rsi
	movq	%rcx, %rbx
	je	.LBB13_648
# BB#484:                               # %if.end95.i1219
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	build_tree_list
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	chainon
.LBB13_247:                             # %if.end33.i
	movq	8(%r12), %rax
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	build1
	movq	8(%r12), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %rdx
	movq	$53, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r14, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
.LBB13_248:                             # %expand_builtin_strncat.exit
	movq	%rax, %rdi
	callq	fold
.LBB13_249:                             # %expand_builtin_strncat.exit
	xorq	%rcx, %rcx
	movq	%rax, %rdi
.LBB13_250:                             # %expand_builtin_strncat.exit
	movq	%r15, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_expr
.LBB13_251:                             # %expand_builtin_strncat.exit
	movq	%rax, %r15
.LBB13_252:                             # %expand_builtin_strncat.exit
	xorq	%rsi, %rsi
.LBB13_253:                             # %expand_builtin_strncat.exit
	testq	%r15, %r15
	je	.LBB13_648
	jmp	.LBB13_651
.Ltmp174:
	.size	expand_builtin, .Ltmp174-expand_builtin
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_154
	.quad	.LBB13_9
	.quad	.LBB13_9
	.quad	.LBB13_9
	.quad	.LBB13_9
	.quad	.LBB13_9
	.quad	.LBB13_9
	.quad	.LBB13_9
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_11
	.quad	.LBB13_429
	.quad	.LBB13_532
	.quad	.LBB13_156
	.quad	.LBB13_366
	.quad	.LBB13_387
	.quad	.LBB13_408
	.quad	.LBB13_532
	.quad	.LBB13_428
	.quad	.LBB13_213
	.quad	.LBB13_230
	.quad	.LBB13_183
	.quad	.LBB13_189
	.quad	.LBB13_431
	.quad	.LBB13_485
	.quad	.LBB13_159
	.quad	.LBB13_306
	.quad	.LBB13_339
	.quad	.LBB13_254
	.quad	.LBB13_279
	.quad	.LBB13_366
	.quad	.LBB13_387
	.quad	.LBB13_13
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_13
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_13
	.quad	.LBB13_12
	.quad	.LBB13_12
	.quad	.LBB13_90
	.quad	.LBB13_100
	.quad	.LBB13_99
	.quad	.LBB13_91
	.quad	.LBB13_124
	.quad	.LBB13_127
	.quad	.LBB13_127
	.quad	.LBB13_145
	.quad	.LBB13_31
	.quad	.LBB13_44
	.quad	.LBB13_81
	.quad	.LBB13_582
	.quad	.LBB13_588
	.quad	.LBB13_592
	.quad	.LBB13_623
	.quad	.LBB13_648
	.quad	.LBB13_648
	.quad	.LBB13_647
	.quad	.LBB13_648
	.quad	.LBB13_593
	.quad	.LBB13_648
	.quad	.LBB13_647
	.quad	.LBB13_648
	.quad	.LBB13_648
	.quad	.LBB13_647
	.quad	.LBB13_648
	.quad	.LBB13_594
	.quad	.LBB13_648
	.quad	.LBB13_647
	.quad	.LBB13_647
	.quad	.LBB13_647
	.quad	.LBB13_647
	.quad	.LBB13_647
	.quad	.LBB13_647
	.quad	.LBB13_647
	.quad	.LBB13_595
	.quad	.LBB13_596
	.quad	.LBB13_597
	.quad	.LBB13_598
	.quad	.LBB13_599
	.quad	.LBB13_600
	.quad	.LBB13_601
	.quad	.LBB13_602
	.quad	.LBB13_603
	.quad	.LBB13_607
	.quad	.LBB13_610
	.quad	.LBB13_613
	.quad	.LBB13_616
.LJTI13_1:
	.quad	.LBB13_121
	.quad	.LBB13_103
	.quad	.LBB13_110
	.quad	.LBB13_111
	.quad	.LBB13_120
	.quad	.LBB13_105
	.quad	.LBB13_106
	.quad	.LBB13_104
	.quad	.LBB13_107
	.quad	.LBB13_109
	.quad	.LBB13_108
	.quad	.LBB13_113
	.quad	.LBB13_118
	.quad	.LBB13_116
	.quad	.LBB13_117
	.quad	.LBB13_114
	.quad	.LBB13_115
	.quad	.LBB13_115
	.quad	.LBB13_112
	.quad	.LBB13_119

	.text
	.align	16, 0x90
	.type	validate_arglist,@function
validate_arglist:                       # @validate_arglist
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%r9, -32(%rsp)
	movq	%r8, -40(%rsp)
	movq	%rcx, -48(%rsp)
	movq	%rdx, -56(%rsp)
	movq	%rsi, -64(%rsp)
	leaq	-72(%rsp), %rax
	movq	%rax, -8(%rsp)
	leaq	8(%rsp), %rax
	movq	%rax, -16(%rsp)
	movq	$48, -20(%rsp)
	movq	$8, -24(%rsp)
	movl	-24(%rsp), %edx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB14_1
	.align	16, 0x90
.LBB14_12:                              # %sw.epilog
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	(%rdi), %rdi
.LBB14_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rax
	andq	%r8, %rax
	cmpq	$40, %rax
	ja	.LBB14_3
# BB#2:                                 # %vaarg.in_reg
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	-8(%rsp), %rsi
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rsi, %rax
	addq	$8, %rdx
	movl	%edx, -24(%rsp)
	jmp	.LBB14_4
	.align	16, 0x90
.LBB14_3:                               # %vaarg.in_mem
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	-16(%rsp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, -16(%rsp)
.LBB14_4:                               # %vaarg.end
                                        #   in Loop: Header=BB14_1 Depth=1
	movl	(%rax), %ecx
	movq	$1, %rax
	testq	%rcx, %rcx
	je	.LBB14_9
# BB#5:                                 # %vaarg.end
                                        #   in Loop: Header=BB14_1 Depth=1
	cmpq	$5, %rcx
	je	.LBB14_6
# BB#10:                                # %sw.default
                                        #   in Loop: Header=BB14_1 Depth=1
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB14_9
# BB#11:                                # %lor.lhs.false
                                        #   in Loop: Header=BB14_1 Depth=1
	movq	32(%rdi), %rsi
	movq	8(%rsi), %rsi
	movzbl	16(%rsi), %esi
	cmpq	%rsi, %rcx
	je	.LBB14_12
	jmp	.LBB14_9
.LBB14_6:                               # %sw.bb3
	testq	%rdi, %rdi
	jne	.LBB14_7
# BB#8:                                 # %sw.bb3
	movabsq	$1, %rax
.LBB14_9:                               # %end
	retq
.LBB14_7:
	movabsq	$0, %rax
	retq
.Ltmp175:
	.size	validate_arglist, .Ltmp175-validate_arglist
	.cfi_endproc

	.align	16, 0x90
	.type	expand_builtin_next_arg,@function
expand_builtin_next_arg:                # @expand_builtin_next_arg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp178:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp179:
	.cfi_def_cfa_offset 32
.Ltmp180:
	.cfi_offset %rbx, -16
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB15_2
# BB#1:                                 # %lor.lhs.false
	movq	%rax, %rdi
	callq	tree_last
	movq	32(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB15_3
.LBB15_2:                               # %land.lhs.true
	movq	cfun(%rip), %rax
	movzbl	426(%rax), %eax
	testq	$1, %rax
	je	.LBB15_13
.LBB15_3:                               # %if.end
	testq	%rbx, %rbx
	je	.LBB15_9
# BB#4:                                 # %if.then6
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rdi
	callq	tree_last
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB15_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rbx
	movzbl	16(%rbx), %edx
	leaq	-114(%rdx), %rsi
	andq	%rcx, %rsi
	cmpq	$3, %rsi
	jb	.LBB15_5
# BB#6:                                 # %while.cond
                                        #   in Loop: Header=BB15_5 Depth=1
	cmpq	$41, %rdx
	je	.LBB15_5
# BB#7:                                 # %lor.rhs
	cmpq	%rax, %rbx
	je	.LBB15_12
# BB#8:                                 # %if.then32
	movabsq	$.L.str121, %rdi
	jmp	.LBB15_11
.LBB15_9:                               # %if.else
	movq	cfun(%rip), %rax
	movzbl	426(%rax), %eax
	testq	$1, %rax
	jne	.LBB15_12
# BB#10:                                # %if.then39
	movabsq	$.L.str122, %rdi
.LBB15_11:                              # %if.end41
	xorq	%rax, %rax
	callq	warning
.LBB15_12:                              # %if.end41
	movl	target_flags(%rip), %edi
	movq	cfun(%rip), %rdx
	movq	$25, %rcx
	movq	optab_table(%rip), %rsi
	movq	80(%rdx), %rax
	movq	128(%rdx), %rdx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rcx
	callq	expand_binop
	jmp	.LBB15_14
.LBB15_13:                              # %return
	movabsq	$.L.str120, %rdi
	xorq	%rax, %rax
	callq	error
	movq	const_int_rtx+512(%rip), %rax
.LBB15_14:                              # %return
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp181:
	.size	expand_builtin_next_arg, .Ltmp181-expand_builtin_next_arg
	.cfi_endproc

	.align	16, 0x90
	.type	expand_builtin_memset,@function
expand_builtin_memset:                  # @expand_builtin_memset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp188:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp189:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp190:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp191:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp192:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp193:
	.cfi_def_cfa_offset 80
.Ltmp194:
	.cfi_offset %rbx, -48
.Ltmp195:
	.cfi_offset %r12, -40
.Ltmp196:
	.cfi_offset %r13, -32
.Ltmp197:
	.cfi_offset %r14, -24
.Ltmp198:
	.cfi_offset %r15, -16
	movq	40(%rdi), %rbx
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	$13, %rsi
	movq	$6, %rdx
	movq	$6, %rcx
	movq	$5, %r8
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%rbx, %rdi
	callq	validate_arglist
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB16_15
# BB#1:                                 # %if.else
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	(%rbx), %rax
	movq	(%rax), %rcx
	movq	32(%rbx), %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	32(%rax), %r15
	movq	32(%rcx), %rbx
	callq	get_pointer_alignment
	testq	%r13, %rax
	je	.LBB16_15
# BB#2:                                 # %if.end
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB16_5
# BB#3:                                 # %land.lhs.true
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	testq	%rax, %rax
	je	.LBB16_4
.LBB16_5:                               # %if.end19
	movzbl	16(%r15), %eax
	cmpq	$25, %rax
	jne	.LBB16_15
# BB#6:                                 # %if.end23
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB16_15
# BB#7:                                 # %if.end27
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	tree_low_cst
	testq	$255, %rax
	movb	%al, 31(%rsp)
	je	.LBB16_11
# BB#8:                                 # %if.then29
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	testq	%r13, %rax
	je	.LBB16_15
# BB#9:                                 # %if.end33
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	movabsq	$builtin_memset_read_str, %rsi
	leaq	31(%rsp), %rdx
	movq	%rax, %rdi
	movq	(%rsp), %r15            # 8-byte Reload
	movq	%r15, %rcx
	callq	can_store_by_pieces
	testq	%r13, %rax
	je	.LBB16_15
# BB#10:                                # %if.end38
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	get_memory_rtx
	movq	%rax, %r14
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	movabsq	$builtin_memset_read_str, %rdx
	leaq	31(%rsp), %rcx
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r15, %r8
	callq	store_by_pieces
	movq	8(%r14), %rdi
	jmp	.LBB16_13
.LBB16_4:                               # %if.then16
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	expand_expr
	xorq	%rcx, %rcx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	expand_expr
	jmp	.LBB16_14
.LBB16_11:                              # %if.end43
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	movq	%rax, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	get_memory_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	set_mem_align
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	clear_storage
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB16_15
# BB#12:                                # %if.then48
	movq	8(%rbx), %rdi
.LBB16_13:                              # %return
	xorq	%rsi, %rsi
	callq	force_operand
.LBB16_14:                              # %return
	movq	%rax, %r12
.LBB16_15:                              # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp199:
	.size	expand_builtin_memset, .Ltmp199-expand_builtin_memset
	.cfi_endproc

	.align	16, 0x90
	.type	expand_builtin_fputs,@function
expand_builtin_fputs:                   # @expand_builtin_fputs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp206:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp207:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp208:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp209:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp210:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp211:
	.cfi_def_cfa_offset 64
.Ltmp212:
	.cfi_offset %rbx, -48
.Ltmp213:
	.cfi_offset %r12, -40
.Ltmp214:
	.cfi_offset %r13, -32
.Ltmp215:
	.cfi_offset %r14, -24
.Ltmp216:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rdx
	jne	.LBB17_1
# BB#2:                                 # %entry
	movabsq	$built_in_decls+536, %rax
	jmp	.LBB17_3
.LBB17_1:
	movabsq	$built_in_decls+592, %rax
.LBB17_3:                               # %entry
	testq	%rdx, %rdx
	jne	.LBB17_4
# BB#5:                                 # %entry
	movabsq	$built_in_decls+520, %rcx
	jmp	.LBB17_6
.LBB17_4:
	movabsq	$built_in_decls+576, %rcx
.LBB17_6:                               # %entry
	xorq	%rbx, %rbx
	testq	%r15, %rsi
	je	.LBB17_27
# BB#7:                                 # %entry
	movq	(%rcx), %r13
	testq	%r13, %r13
	je	.LBB17_27
# BB#8:                                 # %entry
	movq	(%rax), %r14
	testq	%r14, %r14
	je	.LBB17_27
# BB#9:                                 # %if.end
	movq	$13, %rsi
	movq	$13, %rdx
	movq	$5, %rcx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	callq	validate_arglist
	testq	%r15, %rax
	je	.LBB17_27
# BB#10:                                # %if.end12
	movq	32(%r12), %rdi
	callq	c_strlen
	testq	%rax, %rax
	je	.LBB17_27
# BB#11:                                # %lor.lhs.false15
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB17_27
# BB#12:                                # %if.end17
	movq	$1, %rsi
	movq	%rax, %rdi
	movq	%rax, (%rsp)            # 8-byte Spill
	callq	compare_tree_int
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$1, %rcx
	je	.LBB17_25
# BB#13:                                # %if.end17
	testq	%r15, %rax
	jne	.LBB17_14
# BB#16:                                # %sw.bb23
	movq	32(%r12), %rdi
	leaq	8(%rsp), %rsi
	callq	string_constant
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB17_25
# BB#17:                                # %if.end.i
	movq	8(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB17_18
# BB#19:                                # %if.else.i
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB17_25
# BB#20:                                # %lor.lhs.false.i
	movl	32(%rbx), %esi
	movq	8(%rsp), %rdi
	decq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB17_25
# BB#21:                                # %if.end10.i
	movq	40(%rbx), %r15
	movq	8(%rsp), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rbx
	addq	%r15, %rbx
	jmp	.LBB17_22
.LBB17_14:                              # %if.end17
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB17_24
# BB#15:                                # %sw.bb
	movq	(%r12), %rax
	movq	32(%rax), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rbx
	jmp	.LBB17_27
.LBB17_18:                              # %if.then2.i
	movq	40(%rbx), %rbx
.LBB17_22:                              # %c_getstr.exit
	testq	%rbx, %rbx
	je	.LBB17_25
# BB#23:                                # %if.then28
	movq	(%r12), %rax
	movq	32(%rax), %rsi
	xorq	%rdi, %rdi
	callq	build_tree_list
	movsbq	(%rbx), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rbx
	movq	%r13, %r14
	jmp	.LBB17_26
.LBB17_25:                              # %sw.bb37
	movq	(%r12), %rax
	movq	32(%r12), %rbx
	movq	32(%rax), %rsi
	xorq	%rdi, %rdi
	callq	build_tree_list
	xorq	%rdi, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	movq	global_trees+128(%rip), %rsi
	xorq	%rdi, %rdi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rax, %rbx
.LBB17_26:                              # %sw.epilog
	movq	8(%r14), %rdi
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	build1
	movq	8(%r14), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %rcx
	movq	$53, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	%rax, %rdi
	callq	fold
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	%rax, %rbx
.LBB17_27:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_24:                              # %sw.default
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.expand_builtin_fputs, %rcx
	movq	$3400, %rsi             # imm = 0xD48
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp217:
	.size	expand_builtin_fputs, .Ltmp217-expand_builtin_fputs
	.cfi_endproc

	.globl	fold_builtin
	.align	16, 0x90
	.type	fold_builtin,@function
fold_builtin:                           # @fold_builtin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp220:
	.cfi_def_cfa_offset 16
.Ltmp221:
	.cfi_offset %rbx, -16
	movq	32(%rdi), %rax
	movq	32(%rax), %rcx
	movq	48(%rcx), %rdx
	andq	$1610612736, %rdx       # imm = 0x60000000
	xorq	%rax, %rax
	cmpq	$1073741824, %rdx       # imm = 0x40000000
	je	.LBB18_45
# BB#1:                                 # %if.end
	movl	56(%rcx), %ecx
	movq	40(%rdi), %rbx
	cmpq	$31, %rcx
	je	.LBB18_42
# BB#2:                                 # %if.end
	cmpq	$48, %rcx
	jne	.LBB18_3
# BB#20:                                # %sw.bb9
	testq	%rbx, %rbx
	je	.LBB18_40
# BB#21:                                # %if.end.i
	movq	32(%rbx), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %edx
	addq	$-5, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$19, %rcx
	ja	.LBB18_40
# BB#22:                                # %if.end.i
	xorq	%rdi, %rdi
	jmpq	*.LJTI18_0(,%rcx,8)
.LBB18_35:                              # %sw.bb13.i.i
	movq	$13, %rdi
	jmp	.LBB18_41
.LBB18_42:                              # %sw.bb11
	movq	$13, %rsi
	movq	$5, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	validate_arglist
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_44
# BB#43:                                # %if.then13
	movq	32(%rbx), %rdi
	callq	c_strlen
	testq	%rax, %rax
	jne	.LBB18_45
	jmp	.LBB18_44
.LBB18_3:                               # %if.end
	cmpq	$51, %rcx
	jne	.LBB18_44
# BB#4:                                 # %sw.bb
	testq	%rbx, %rbx
	je	.LBB18_45
# BB#5:                                 # %while.cond.preheader.i
	movq	global_trees(%rip), %r9
	movq	32(%rbx), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB18_6:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movl	16(%rbx), %r10d
	movq	%r10, %rdi
	andq	$255, %rdi
	leaq	-114(%rdi), %rcx
	andq	%r8, %rcx
	cmpq	$2, %rcx
	ja	.LBB18_9
# BB#7:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB18_6 Depth=1
	movq	32(%rbx), %rcx
	cmpq	%r9, %rcx
	je	.LBB18_9
# BB#8:                                 # %land.rhs.i
                                        #   in Loop: Header=BB18_6 Depth=1
	movq	8(%rbx), %rdx
	movq	8(%rcx), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB18_6
.LBB18_9:                               # %while.end.i
	movzbl	tree_code_type(%rdi), %ecx
	cmpq	$99, %rcx
	je	.LBB18_13
# BB#10:                                # %while.end.i
	movq	%r10, %rcx
	andq	$767, %rcx              # imm = 0x2FF
	andq	%r8, %rcx
	cmpq	$558, %rcx              # imm = 0x22E
	je	.LBB18_13
# BB#11:                                # %lor.lhs.false51.i
	cmpq	$121, %rdi
	jne	.LBB18_14
# BB#12:                                # %land.lhs.true58.i
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$29, %rcx
	jne	.LBB18_14
.LBB18_13:                              # %if.then68.i
	movq	global_trees+96(%rip), %rax
	popq	%rbx
	retq
.LBB18_44:                              # %sw.epilog
	xorq	%rax, %rax
.LBB18_45:                              # %return
	popq	%rbx
	retq
.LBB18_40:                              # %sw.default.i.i
	movq	$-1, %rdi
.LBB18_41:                              # %type_to_class.exit.i
	xorq	%rsi, %rsi
	popq	%rbx
	jmp	build_int_2_wide  # TAILCALL
.LBB18_14:                              # %if.end69.i
	movl	cse_not_expected(%rip), %ecx
	andq	$256, %r10              # imm = 0x100
	orq	%r10, %rcx
	testq	%r8, %rcx
	jne	.LBB18_19
# BB#15:                                # %lor.lhs.false77.i
	movq	8(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$22, %rcx
	ja	.LBB18_17
# BB#16:                                # %lor.lhs.false77.i
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$8134656, %rdx          # imm = 0x7C2000
	jne	.LBB18_19
.LBB18_17:                              # %lor.lhs.false131.i
	cmpq	$15, %rcx
	je	.LBB18_19
# BB#18:                                # %lor.lhs.false131.i
	movq	cfun(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB18_45
.LBB18_19:                              # %if.then143.i
	movq	global_trees+88(%rip), %rax
	popq	%rbx
	retq
.LBB18_23:                              # %sw.bb1.i.i
	movq	$1, %rdi
	jmp	.LBB18_41
.LBB18_30:                              # %sw.bb8.i.i
	movq	$8, %rdi
	jmp	.LBB18_41
.LBB18_31:                              # %sw.bb9.i.i
	movq	$9, %rdi
	jmp	.LBB18_41
.LBB18_25:                              # %sw.bb3.i.i
	movq	$3, %rdi
	jmp	.LBB18_41
.LBB18_26:                              # %sw.bb4.i.i
	movq	$4, %rdi
	jmp	.LBB18_41
.LBB18_24:                              # %sw.bb2.i.i
	movq	$2, %rdi
	jmp	.LBB18_41
.LBB18_27:                              # %sw.bb5.i.i
	movq	$5, %rdi
	jmp	.LBB18_41
.LBB18_29:                              # %sw.bb7.i.i
	movq	$7, %rdi
	jmp	.LBB18_41
.LBB18_28:                              # %sw.bb6.i.i
	movq	$6, %rdi
	jmp	.LBB18_41
.LBB18_33:                              # %sw.bb11.i.i
	movq	$11, %rdi
	jmp	.LBB18_41
.LBB18_38:                              # %sw.bb19.i.i
	movq	$17, %rdi
	jmp	.LBB18_41
.LBB18_36:                              # %sw.bb14.i.i
	movzbl	62(%rax), %edi
	andq	$1, %rdi
	orq	$14, %rdi
	jmp	.LBB18_41
.LBB18_37:                              # %sw.bb18.i.i
	movq	$16, %rdi
	jmp	.LBB18_41
.LBB18_34:                              # %sw.bb12.i.i
	movq	$12, %rdi
	jmp	.LBB18_41
.LBB18_32:                              # %sw.bb10.i.i
	movq	$10, %rdi
	jmp	.LBB18_41
.LBB18_39:                              # %sw.bb20.i.i
	movq	$18, %rdi
	jmp	.LBB18_41
.Ltmp222:
	.size	fold_builtin, .Ltmp222-fold_builtin
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_41
	.quad	.LBB18_23
	.quad	.LBB18_30
	.quad	.LBB18_31
	.quad	.LBB18_40
	.quad	.LBB18_25
	.quad	.LBB18_26
	.quad	.LBB18_24
	.quad	.LBB18_27
	.quad	.LBB18_29
	.quad	.LBB18_28
	.quad	.LBB18_33
	.quad	.LBB18_38
	.quad	.LBB18_36
	.quad	.LBB18_37
	.quad	.LBB18_34
	.quad	.LBB18_35
	.quad	.LBB18_35
	.quad	.LBB18_32
	.quad	.LBB18_39

	.text
	.align	16, 0x90
	.type	c_strlen,@function
c_strlen:                               # @c_strlen
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp227:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp228:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp229:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp230:
	.cfi_def_cfa_offset 48
.Ltmp231:
	.cfi_offset %rbx, -32
.Ltmp232:
	.cfi_offset %r14, -24
.Ltmp233:
	.cfi_offset %r15, -16
	leaq	8(%rsp), %rsi
	callq	string_constant
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB19_15
# BB#1:                                 # %if.end
	movl	32(%rax), %r15d
	movq	8(%rsp), %rdi
	movq	40(%rax), %r14
	decq	%r15
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB19_10
# BB#2:                                 # %land.lhs.true
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB19_3
# BB#8:                                 # %if.else
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_12
# BB#9:                                 # %if.else19
	movq	8(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
.LBB19_10:                              # %if.end22
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	testq	%rax, %rax
	js	.LBB19_12
# BB#11:                                # %if.end22
	cmpq	%r15, %rax
	jg	.LBB19_12
# BB#13:                                # %if.end29
	addq	%rax, %r14
	movq	%r14, %rdi
	callq	strlen
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	size_int_wide
	jmp	.LBB19_14
.LBB19_12:                              # %if.then28
	movabsq	$.L.str109, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	warning
	jmp	.LBB19_15
.LBB19_3:                               # %for.cond.preheader
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_7
# BB#4:
	movq	$1, %rax
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB19_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%r14,%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB19_15
# BB#5:                                 # %for.cond
                                        #   in Loop: Header=BB19_6 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rax
	cmpq	%rsi, %rdx
	jl	.LBB19_6
.LBB19_7:                               # %for.end
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	size_int_wide
	movq	8(%rsp), %rsi
	movq	%rax, %rdi
	callq	size_diffop
.LBB19_14:                              # %return
	movq	%rax, %rbx
.LBB19_15:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp234:
	.size	c_strlen, .Ltmp234-c_strlen
	.cfi_endproc

	.globl	default_init_builtins
	.align	16, 0x90
	.type	default_init_builtins,@function
default_init_builtins:                  # @default_init_builtins
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp235:
	.size	default_init_builtins, .Ltmp235-default_init_builtins
	.cfi_endproc

	.globl	default_expand_builtin
	.align	16, 0x90
	.type	default_expand_builtin,@function
default_expand_builtin:                 # @default_expand_builtin
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp236:
	.size	default_expand_builtin, .Ltmp236-default_expand_builtin
	.cfi_endproc

	.align	16, 0x90
	.type	get_pointer_alignment,@function
get_pointer_alignment:                  # @get_pointer_alignment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp243:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp244:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp245:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp246:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp247:
	.cfi_def_cfa_offset 48
.Ltmp248:
	.cfi_offset %rbx, -48
.Ltmp249:
	.cfi_offset %r12, -40
.Ltmp250:
	.cfi_offset %r13, -32
.Ltmp251:
	.cfi_offset %r14, -24
.Ltmp252:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	xorq	%r14, %r14
	cmpq	$13, %rcx
	jne	.LBB22_21
# BB#1:                                 # %if.end
	movq	8(%rax), %rax
	movl	64(%rax), %r14d
	cmpq	$128, %r14
	movq	$128, %r15
	jb	.LBB22_3
# BB#2:                                 # %if.end
	movq	$128, %r14
.LBB22_3:                               # %if.end
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB22_4
	.align	16, 0x90
.LBB22_9:                               # %if.end22
                                        #   in Loop: Header=BB22_4 Depth=1
	movq	%r15, %r14
.LBB22_4:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB22_11 Depth 2
	movzbl	16(%rbx), %eax
	cmpq	$121, %rax
	je	.LBB22_13
# BB#5:                                 # %while.body
                                        #   in Loop: Header=BB22_4 Depth=1
	cmpq	$59, %rax
	je	.LBB22_10
# BB#6:                                 # %while.body
                                        #   in Loop: Header=BB22_4 Depth=1
	addq	$-114, %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	ja	.LBB22_21
# BB#7:                                 # %sw.bb
                                        #   in Loop: Header=BB22_4 Depth=1
	movq	32(%rbx), %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB22_21
# BB#8:                                 # %if.end22
                                        #   in Loop: Header=BB22_4 Depth=1
	movq	8(%rax), %rax
	movl	64(%rax), %r14d
	movq	%r15, %rax
	andq	%r12, %rax
	cmpq	%rax, %r14
	jb	.LBB22_4
	jmp	.LBB22_9
.LBB22_10:                              # %sw.bb34
                                        #   in Loop: Header=BB22_4 Depth=1
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB22_21
	.align	16, 0x90
.LBB22_11:                              # %while.cond40
                                        #   Parent Loop BB22_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rbx), %rdi
	movq	%r15, %r13
	movq	$1, %rsi
	callq	tree_low_cst
	andq	%r12, %r15
	movq	$3, %rcx
	movq	%r15, %rdx
	shrq	%cl, %rdx
	decq	%rdx
	andq	%rax, %rdx
	movq	$1, %rcx
	shrq	%cl, %r15
	testq	%r12, %rdx
	jne	.LBB22_11
# BB#12:                                # %while.end
                                        #   in Loop: Header=BB22_4 Depth=1
	movq	32(%rbx), %rbx
	movq	%r13, %r15
	jmp	.LBB22_4
.LBB22_13:                              # %sw.bb51
	movq	32(%rbx), %rdi
	movzbl	16(%rdi), %ecx
	cmpq	$30, %rcx
	movq	$16, %rax
	je	.LBB22_18
# BB#14:                                # %if.else
	movzbl	tree_code_type(%rcx), %eax
	cmpq	$99, %rax
	jne	.LBB22_15
# BB#17:                                # %if.then83
	movq	%r14, %rsi
	callq	ix86_constant_alignment
	jmp	.LBB22_18
.LBB22_15:                              # %if.else
	cmpq	$100, %rax
	movq	%r14, %rax
	jne	.LBB22_18
# BB#16:                                # %if.then70
	movl	56(%rdi), %eax
	andq	$16777215, %rax         # imm = 0xFFFFFF
.LBB22_18:                              # %if.end87
	movq	%r15, %rcx
	andq	%r12, %rcx
	andq	%rax, %r12
	cmpq	%rcx, %r12
	jb	.LBB22_20
# BB#19:                                # %if.end87
	movq	%r15, %rax
.LBB22_20:                              # %if.end87
	movq	%rax, %r14
.LBB22_21:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp253:
	.size	get_pointer_alignment, .Ltmp253-get_pointer_alignment
	.cfi_endproc

	.align	16, 0x90
	.type	get_memory_rtx,@function
get_memory_rtx:                         # @get_memory_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp257:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp258:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp259:
	.cfi_def_cfa_offset 32
.Ltmp260:
	.cfi_offset %rbx, -24
.Ltmp261:
	.cfi_offset %r14, -16
	movl	ptr_mode(%rip), %edx
	movq	%rdi, %rbx
	xorq	%rsi, %rsi
	movq	$1, %rcx
                                        # kill: RDI<def> RBX<kill>
	callq	expand_expr
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB23_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdx
	movzbl	16(%rdx), %ecx
	leaq	-114(%rcx), %rsi
	andq	%rax, %rsi
	cmpq	$2, %rsi
	ja	.LBB23_5
# BB#2:                                 # %land.rhs
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	32(%rdx), %rbx
	movq	8(%rbx), %rcx
	movl	16(%rcx), %ecx
	andq	$253, %rcx
	cmpq	$13, %rcx
	je	.LBB23_1
	jmp	.LBB23_3
.LBB23_5:                               # %while.end
	cmpq	$121, %rcx
	jne	.LBB23_3
# BB#6:                                 # %if.then
	movq	32(%rdx), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	set_mem_attributes
	jmp	.LBB23_7
.LBB23_3:                               # %if.else
	movq	8(%rdx), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB23_7
# BB#4:                                 # %if.then57
	movq	8(%rax), %rsi
	movq	$41, %rdi
	callq	build1
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	set_mem_alias_set
.LBB23_7:                               # %if.end63
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp262:
	.size	get_memory_rtx, .Ltmp262-get_memory_rtx
	.cfi_endproc

	.align	16, 0x90
	.type	builtin_memset_read_str,@function
builtin_memset_read_str:                # @builtin_memset_read_str
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp266:
	.cfi_def_cfa_offset 16
.Ltmp267:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp268:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
.Ltmp269:
	.cfi_offset %rbx, -32
.Ltmp270:
	.cfi_offset %r14, -24
	movq	%rdx, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %edx
	leaq	15(%rdx), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movzbl	(%rdi), %esi
	movq	%rbx, %rdi
	callq	memset
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	c_readstr
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Ltmp271:
	.size	builtin_memset_read_str, .Ltmp271-builtin_memset_read_str
	.cfi_endproc

	.align	16, 0x90
	.type	c_readstr,@function
c_readstr:                              # @c_readstr
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp273:
	.cfi_def_cfa_offset 32
	movq	%rsi, %r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB25_1
# BB#3:                                 # %if.end
	movzbl	mode_size(%rax), %r9d
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	xorq	%rsi, %rsi
	testq	%r9, %r9
	je	.LBB25_4
# BB#5:
	movq	$1, %r11
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB25_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rcx
	andq	%r10, %rcx
	cmpq	$129, %rcx
	jae	.LBB25_7
# BB#8:                                 # %if.end10
                                        #   in Loop: Header=BB25_6 Depth=1
	testq	%r11, %r11
	movabsq	$0, %r11
	je	.LBB25_10
# BB#9:                                 # %if.then11
                                        #   in Loop: Header=BB25_6 Depth=1
	movzbl	(%rdi,%rax), %r11d
.LBB25_10:                              # %if.end15
                                        #   in Loop: Header=BB25_6 Depth=1
	movq	%rsi, %rcx
	andq	$56, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	%rax, %rcx
	andq	$536870904, %rcx        # imm = 0x1FFFFFF8
	orq	%rdx, (%rsp,%rcx)
	incq	%rax
	movq	%rax, %rcx
	andq	%r10, %rcx
	addq	$8, %rsi
	cmpq	%r9, %rcx
	jb	.LBB25_6
# BB#11:                                # %for.cond.for.end_crit_edge
	movq	(%rsp), %rdi
	movq	8(%rsp), %rsi
	jmp	.LBB25_12
.LBB25_4:
	xorq	%rdi, %rdi
.LBB25_12:                              # %for.end
	movq	%r8, %rdx
	addq	$24, %rsp
	jmp	immed_double_const  # TAILCALL
.LBB25_1:                               # %if.then
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.c_readstr, %rcx
	movq	$330, %rsi              # imm = 0x14A
	jmp	.LBB25_2
.LBB25_7:                               # %if.then9
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.c_readstr, %rcx
	movq	$344, %rsi              # imm = 0x158
.LBB25_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp274:
	.size	c_readstr, .Ltmp274-c_readstr
	.cfi_endproc

	.align	16, 0x90
	.type	builtin_memcpy_read_str,@function
builtin_memcpy_read_str:                # @builtin_memcpy_read_str
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp280:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp281:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp282:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp283:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp284:
	.cfi_def_cfa_offset 48
.Ltmp285:
	.cfi_offset %rbx, -40
.Ltmp286:
	.cfi_offset %r12, -32
.Ltmp287:
	.cfi_offset %r14, -24
.Ltmp288:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	testq	%r15, %r15
	js	.LBB26_2
# BB#1:                                 # %lor.lhs.false
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ebx
	addq	%r15, %rbx
	movq	%r12, %rdi
	callq	strlen
	incq	%rax
	cmpq	%rax, %rbx
	ja	.LBB26_2
# BB#3:                                 # %if.end
	addq	%r15, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	c_readstr  # TAILCALL
.LBB26_2:                               # %if.then
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.builtin_memcpy_read_str, %rcx
	movq	$1912, %rsi             # imm = 0x778
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp289:
	.size	builtin_memcpy_read_str, .Ltmp289-builtin_memcpy_read_str
	.cfi_endproc

	.align	16, 0x90
	.type	builtin_strncpy_read_str,@function
builtin_strncpy_read_str:               # @builtin_strncpy_read_str
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp294:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp295:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp296:
	.cfi_def_cfa_offset 32
.Ltmp297:
	.cfi_offset %rbx, -32
.Ltmp298:
	.cfi_offset %r14, -24
.Ltmp299:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	strlen
	cmpq	%r15, %rax
	jae	.LBB27_2
# BB#1:                                 # %return
	movq	const_int_rtx+512(%rip), %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB27_2:                               # %if.end
	addq	%r15, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	c_readstr  # TAILCALL
.Ltmp300:
	.size	builtin_strncpy_read_str, .Ltmp300-builtin_strncpy_read_str
	.cfi_endproc

	.align	16, 0x90
	.type	apply_result_size,@function
apply_result_size:                      # @apply_result_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp307:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp308:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp309:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp310:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp311:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp312:
	.cfi_def_cfa_offset 80
.Ltmp313:
	.cfi_offset %rbx, -48
.Ltmp314:
	.cfi_offset %r12, -40
.Ltmp315:
	.cfi_offset %r13, -32
.Ltmp316:
	.cfi_offset %r14, -24
.Ltmp317:
	.cfi_offset %r15, -16
	movslq	apply_result_size.size(%rip), %rax
	testq	%rax, %rax
	jns	.LBB28_32
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	movl	%eax, apply_result_size.size(%rip)
	movl	class_narrowest_mode+4(%rip), %eax
	movl	class_narrowest_mode+8(%rip), %r12d
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB28_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB28_4 Depth 2
                                        #     Child Loop BB28_9 Depth 2
                                        #     Child Loop BB28_15 Depth 2
                                        #     Child Loop BB28_21 Depth 2
	movq	%r15, %rdi
	callq	ix86_function_value_regno_p
	testq	%rax, %rax
	je	.LBB28_29
# BB#3:                                 # %if.then2
                                        #   in Loop: Header=BB28_2 Depth=1
	xorq	%r14, %r14
	cmpq	$6, 24(%rsp)            # 8-byte Folded Reload
	movl	class_narrowest_mode+4(%rip), %ebx
	je	.LBB28_8
	.align	16, 0x90
.LBB28_4:                               # %for.body5
                                        #   Parent Loop BB28_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB28_6
# BB#5:                                 # %for.body5
                                        #   in Loop: Header=BB28_4 Depth=2
	movq	%rbx, %r14
.LBB28_6:                               # %for.body5
                                        #   in Loop: Header=BB28_4 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	cmpq	$6, %rbx
	jne	.LBB28_4
# BB#7:                                 # %for.end
                                        #   in Loop: Header=BB28_2 Depth=1
	testq	%r13, %r14
	jne	.LBB28_26
.LBB28_8:                               # %if.then10
                                        #   in Loop: Header=BB28_2 Depth=1
	xorq	%r14, %r14
	testq	%r13, %r12
	movq	%r12, %rbx
	je	.LBB28_14
	.align	16, 0x90
.LBB28_9:                               # %for.body14
                                        #   Parent Loop BB28_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB28_12
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB28_9 Depth=2
	movq	$47, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r13, %rax
	je	.LBB28_12
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB28_9 Depth=2
	movq	%rbx, %r14
.LBB28_12:                              # %for.inc21
                                        #   in Loop: Header=BB28_9 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB28_9
# BB#13:                                # %if.end26
                                        #   in Loop: Header=BB28_2 Depth=1
	testq	%r13, %r14
	jne	.LBB28_26
.LBB28_14:                              # %if.then29
                                        #   in Loop: Header=BB28_2 Depth=1
	xorq	%r14, %r14
	movl	class_narrowest_mode+32(%rip), %eax
	testq	%r13, %rax
	movq	%rax, %rbx
	je	.LBB28_20
	.align	16, 0x90
.LBB28_15:                              # %for.body33
                                        #   Parent Loop BB28_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB28_18
# BB#16:                                # %land.lhs.true36
                                        #   in Loop: Header=BB28_15 Depth=2
	movq	$47, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r13, %rax
	je	.LBB28_18
# BB#17:                                # %land.lhs.true36
                                        #   in Loop: Header=BB28_15 Depth=2
	movq	%rbx, %r14
.LBB28_18:                              # %for.inc41
                                        #   in Loop: Header=BB28_15 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB28_15
# BB#19:                                # %if.end46
                                        #   in Loop: Header=BB28_2 Depth=1
	testq	%r13, %r14
	jne	.LBB28_26
.LBB28_20:                              # %if.then49
                                        #   in Loop: Header=BB28_2 Depth=1
	xorq	%r14, %r14
	movl	class_narrowest_mode+28(%rip), %eax
	testq	%r13, %rax
	movq	%rax, %rbx
	je	.LBB28_33
	.align	16, 0x90
.LBB28_21:                              # %for.body53
                                        #   Parent Loop BB28_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB28_24
# BB#22:                                # %land.lhs.true56
                                        #   in Loop: Header=BB28_21 Depth=2
	movq	$47, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r13, %rax
	je	.LBB28_24
# BB#23:                                # %land.lhs.true56
                                        #   in Loop: Header=BB28_21 Depth=2
	movq	%rbx, %r14
.LBB28_24:                              # %for.inc61
                                        #   in Loop: Header=BB28_21 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB28_21
# BB#25:                                # %if.end66
                                        #   in Loop: Header=BB28_2 Depth=1
	testq	%r13, %r14
	je	.LBB28_33
.LBB28_26:                              # %if.end70
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	%r14, %rdi
	callq	get_mode_alignment
	movslq	apply_result_size.size(%rip), %rsi
	movq	%rax, %rdi
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	movq	%rsi, %rax
	cqto
	idivq	%rdi
	testq	%r13, %rdx
	je	.LBB28_28
# BB#27:                                # %if.then74
                                        #   in Loop: Header=BB28_2 Depth=1
	leaq	-1(%rdi,%rsi), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rdi
	subq	%rdx, %rsi
	movl	%esi, apply_result_size.size(%rip)
.LBB28_28:                              # %if.end76
                                        #   in Loop: Header=BB28_2 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	addq	%rsi, %rax
	movl	%eax, apply_result_size.size(%rip)
	movl	%r14d, apply_result_mode(,%r15,4)
	jmp	.LBB28_30
	.align	16, 0x90
.LBB28_29:                              # %if.else
                                        #   in Loop: Header=BB28_2 Depth=1
	xorq	%rax, %rax
	movl	%eax, apply_result_mode(,%r15,4)
.LBB28_30:                              # %for.inc86
                                        #   in Loop: Header=BB28_2 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB28_2
# BB#31:                                # %for.end87
	movq	$116, %rax
	movl	%eax, apply_result_size.size(%rip)
.LBB28_32:                              # %if.end88
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_33:                              # %if.then69
	movabsq	$.L.str100, %rax
	movabsq	$.L__FUNCTION__.apply_result_size, %rcx
	movq	$1016, %rsi             # imm = 0x3F8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp318:
	.size	apply_result_size, .Ltmp318-apply_result_size
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"NOT_BUILT_IN"
	.size	.L.str, 13

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"BUILT_IN_FRONTEND"
	.size	.L.str1, 18

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"BUILT_IN_MD"
	.size	.L.str2, 12

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"BUILT_IN_NORMAL"
	.size	.L.str3, 16

	.type	built_in_class_names,@object # @built_in_class_names
	.section	.rodata,"a",@progbits
	.globl	built_in_class_names
	.align	16
built_in_class_names:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.size	built_in_class_names, 32

	.type	.L.str4,@object         # @.str4
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str4:
	.asciz	"BUILT_IN_ALLOCA"
	.size	.L.str4, 16

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"BUILT_IN_ABS"
	.size	.L.str5, 13

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"BUILT_IN_LABS"
	.size	.L.str6, 14

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"BUILT_IN_FABS"
	.size	.L.str7, 14

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"BUILT_IN_FABSF"
	.size	.L.str8, 15

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"BUILT_IN_FABSL"
	.size	.L.str9, 15

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"BUILT_IN_LLABS"
	.size	.L.str10, 15

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"BUILT_IN_IMAXABS"
	.size	.L.str11, 17

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"BUILT_IN_CONJ"
	.size	.L.str12, 14

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"BUILT_IN_CONJF"
	.size	.L.str13, 15

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"BUILT_IN_CONJL"
	.size	.L.str14, 15

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"BUILT_IN_CREAL"
	.size	.L.str15, 15

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"BUILT_IN_CREALF"
	.size	.L.str16, 16

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"BUILT_IN_CREALL"
	.size	.L.str17, 16

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"BUILT_IN_CIMAG"
	.size	.L.str18, 15

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"BUILT_IN_CIMAGF"
	.size	.L.str19, 16

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"BUILT_IN_CIMAGL"
	.size	.L.str20, 16

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"BUILT_IN_BZERO"
	.size	.L.str21, 15

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"BUILT_IN_BCMP"
	.size	.L.str22, 14

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"BUILT_IN_FFS"
	.size	.L.str23, 13

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"BUILT_IN_INDEX"
	.size	.L.str24, 15

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"BUILT_IN_RINDEX"
	.size	.L.str25, 16

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"BUILT_IN_MEMCPY"
	.size	.L.str26, 16

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"BUILT_IN_MEMCMP"
	.size	.L.str27, 16

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"BUILT_IN_MEMSET"
	.size	.L.str28, 16

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"BUILT_IN_STRCAT"
	.size	.L.str29, 16

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"BUILT_IN_STRNCAT"
	.size	.L.str30, 17

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"BUILT_IN_STRCPY"
	.size	.L.str31, 16

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"BUILT_IN_STRNCPY"
	.size	.L.str32, 17

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"BUILT_IN_STRCMP"
	.size	.L.str33, 16

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"BUILT_IN_STRNCMP"
	.size	.L.str34, 17

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"BUILT_IN_STRLEN"
	.size	.L.str35, 16

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"BUILT_IN_STRSTR"
	.size	.L.str36, 16

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"BUILT_IN_STRPBRK"
	.size	.L.str37, 17

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"BUILT_IN_STRSPN"
	.size	.L.str38, 16

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"BUILT_IN_STRCSPN"
	.size	.L.str39, 17

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"BUILT_IN_STRCHR"
	.size	.L.str40, 16

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"BUILT_IN_STRRCHR"
	.size	.L.str41, 17

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"BUILT_IN_SQRT"
	.size	.L.str42, 14

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"BUILT_IN_SIN"
	.size	.L.str43, 13

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"BUILT_IN_COS"
	.size	.L.str44, 13

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"BUILT_IN_SQRTF"
	.size	.L.str45, 15

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"BUILT_IN_SINF"
	.size	.L.str46, 14

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"BUILT_IN_COSF"
	.size	.L.str47, 14

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"BUILT_IN_SQRTL"
	.size	.L.str48, 15

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"BUILT_IN_SINL"
	.size	.L.str49, 14

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"BUILT_IN_COSL"
	.size	.L.str50, 14

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"BUILT_IN_SAVEREGS"
	.size	.L.str51, 18

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"BUILT_IN_CLASSIFY_TYPE"
	.size	.L.str52, 23

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"BUILT_IN_NEXT_ARG"
	.size	.L.str53, 18

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"BUILT_IN_ARGS_INFO"
	.size	.L.str54, 19

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"BUILT_IN_CONSTANT_P"
	.size	.L.str55, 20

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"BUILT_IN_FRAME_ADDRESS"
	.size	.L.str56, 23

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"BUILT_IN_RETURN_ADDRESS"
	.size	.L.str57, 24

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"BUILT_IN_AGGREGATE_INCOMING_ADDRESS"
	.size	.L.str58, 36

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"BUILT_IN_APPLY_ARGS"
	.size	.L.str59, 20

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"BUILT_IN_APPLY"
	.size	.L.str60, 15

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"BUILT_IN_RETURN"
	.size	.L.str61, 16

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"BUILT_IN_SETJMP"
	.size	.L.str62, 16

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"BUILT_IN_LONGJMP"
	.size	.L.str63, 17

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"BUILT_IN_TRAP"
	.size	.L.str64, 14

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"BUILT_IN_PREFETCH"
	.size	.L.str65, 18

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"BUILT_IN_PUTCHAR"
	.size	.L.str66, 17

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"BUILT_IN_PUTS"
	.size	.L.str67, 14

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"BUILT_IN_PRINTF"
	.size	.L.str68, 16

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"BUILT_IN_FPUTC"
	.size	.L.str69, 15

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"BUILT_IN_FPUTS"
	.size	.L.str70, 15

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"BUILT_IN_FWRITE"
	.size	.L.str71, 16

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"BUILT_IN_FPRINTF"
	.size	.L.str72, 17

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"BUILT_IN_PUTCHAR_UNLOCKED"
	.size	.L.str73, 26

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"BUILT_IN_PUTS_UNLOCKED"
	.size	.L.str74, 23

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"BUILT_IN_PRINTF_UNLOCKED"
	.size	.L.str75, 25

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"BUILT_IN_FPUTC_UNLOCKED"
	.size	.L.str76, 24

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"BUILT_IN_FPUTS_UNLOCKED"
	.size	.L.str77, 24

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"BUILT_IN_FWRITE_UNLOCKED"
	.size	.L.str78, 25

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"BUILT_IN_FPRINTF_UNLOCKED"
	.size	.L.str79, 26

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"BUILT_IN_ISGREATER"
	.size	.L.str80, 19

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"BUILT_IN_ISGREATEREQUAL"
	.size	.L.str81, 24

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"BUILT_IN_ISLESS"
	.size	.L.str82, 16

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"BUILT_IN_ISLESSEQUAL"
	.size	.L.str83, 21

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"BUILT_IN_ISLESSGREATER"
	.size	.L.str84, 23

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"BUILT_IN_ISUNORDERED"
	.size	.L.str85, 21

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"BUILT_IN_UNWIND_INIT"
	.size	.L.str86, 21

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"BUILT_IN_DWARF_CFA"
	.size	.L.str87, 19

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"BUILT_IN_DWARF_FP_REGNUM"
	.size	.L.str88, 25

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"BUILT_IN_INIT_DWARF_REG_SIZES"
	.size	.L.str89, 30

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"BUILT_IN_FROB_RETURN_ADDR"
	.size	.L.str90, 26

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"BUILT_IN_EXTRACT_RETURN_ADDR"
	.size	.L.str91, 29

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"BUILT_IN_EH_RETURN"
	.size	.L.str92, 19

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"BUILT_IN_EH_RETURN_DATA_REGNO"
	.size	.L.str93, 30

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"BUILT_IN_VARARGS_START"
	.size	.L.str94, 23

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"BUILT_IN_STDARG_START"
	.size	.L.str95, 22

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"BUILT_IN_VA_END"
	.size	.L.str96, 16

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"BUILT_IN_VA_COPY"
	.size	.L.str97, 17

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"BUILT_IN_EXPECT"
	.size	.L.str98, 16

	.type	built_in_names,@object  # @built_in_names
	.section	.rodata,"a",@progbits
	.globl	built_in_names
	.align	16
built_in_names:
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str6
	.quad	.L.str7
	.quad	.L.str8
	.quad	.L.str9
	.quad	.L.str10
	.quad	.L.str11
	.quad	.L.str12
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	.L.str36
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.quad	.L.str40
	.quad	.L.str41
	.quad	.L.str42
	.quad	.L.str43
	.quad	.L.str44
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.quad	.L.str49
	.quad	.L.str50
	.quad	.L.str51
	.quad	.L.str52
	.quad	.L.str53
	.quad	.L.str54
	.quad	.L.str55
	.quad	.L.str56
	.quad	.L.str57
	.quad	.L.str58
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.quad	.L.str62
	.quad	.L.str63
	.quad	.L.str64
	.quad	.L.str65
	.quad	.L.str66
	.quad	.L.str67
	.quad	.L.str68
	.quad	.L.str69
	.quad	.L.str70
	.quad	.L.str71
	.quad	.L.str72
	.quad	.L.str73
	.quad	.L.str74
	.quad	.L.str75
	.quad	.L.str76
	.quad	.L.str77
	.quad	.L.str78
	.quad	.L.str79
	.quad	.L.str80
	.quad	.L.str81
	.quad	.L.str82
	.quad	.L.str83
	.quad	.L.str84
	.quad	.L.str85
	.quad	.L.str86
	.quad	.L.str87
	.quad	.L.str88
	.quad	.L.str89
	.quad	.L.str90
	.quad	.L.str91
	.quad	.L.str92
	.quad	.L.str93
	.quad	.L.str94
	.quad	.L.str95
	.quad	.L.str96
	.quad	.L.str97
	.quad	.L.str98
	.size	built_in_names, 760

	.type	setjmp_alias_set,@object # @setjmp_alias_set
	.data
	.align	8
setjmp_alias_set:
	.quad	-1                      # 0xffffffffffffffff
	.size	setjmp_alias_set, 8

	.type	.L.str99,@object        # @.str99
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str99:
	.zero	1
	.size	.L.str99, 1

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"builtins.c"
	.size	.L.str100, 11

	.type	.L__FUNCTION__.expand_builtin_longjmp,@object # @__FUNCTION__.expand_builtin_longjmp
.L__FUNCTION__.expand_builtin_longjmp:
	.asciz	"expand_builtin_longjmp"
	.size	.L__FUNCTION__.expand_builtin_longjmp, 23

	.type	apply_args_reg_offset,@object # @apply_args_reg_offset
	.local	apply_args_reg_offset
	.comm	apply_args_reg_offset,212,16
	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"__builtin_saveregs not supported by this target"
	.size	.L.str101, 48

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"first argument to `va_arg' not of type `va_list'"
	.size	.L.str102, 49

	.type	lang_type_promotes_to,@object # @lang_type_promotes_to
	.comm	lang_type_promotes_to,8,8
	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"<anonymous type>"
	.size	.L.str103, 17

	.type	expand_builtin_va_arg.gave_help,@object # @expand_builtin_va_arg.gave_help
	.local	expand_builtin_va_arg.gave_help
	.comm	expand_builtin_va_arg.gave_help,1,1
	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"`%s' is promoted to `%s' when passed through `...'"
	.size	.L.str104, 51

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"(so you should pass `%s' not `%s' to `va_arg')"
	.size	.L.str105, 47

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"__builtin_"
	.size	.L.str106, 11

	.type	.L__FUNCTION__.expand_builtin,@object # @__FUNCTION__.expand_builtin
.L__FUNCTION__.expand_builtin:
	.asciz	"expand_builtin"
	.size	.L__FUNCTION__.expand_builtin, 15

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"__builtin_longjmp second argument must be 1"
	.size	.L.str107, 44

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"built-in function `%s' not currently supported"
	.size	.L.str108, 47

	.type	built_in_decls,@object  # @built_in_decls
	.comm	built_in_decls,760,16
	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"offset outside bounds of constant string"
	.size	.L.str109, 41

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"second arg to `__builtin_prefetch' must be a constant"
	.size	.L.str110, 54

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"invalid second arg to __builtin_prefetch; using zero"
	.size	.L.str111, 53

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"third arg to `__builtin_prefetch' must be a constant"
	.size	.L.str112, 53

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"invalid third arg to __builtin_prefetch; using zero"
	.size	.L.str113, 52

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"second arg to `__builtin_expect' must be a constant"
	.size	.L.str114, 52

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"too many arguments to function `va_start'"
	.size	.L.str115, 42

	.type	.L__FUNCTION__.expand_builtin_fputs,@object # @__FUNCTION__.expand_builtin_fputs
.L__FUNCTION__.expand_builtin_fputs:
	.asciz	"expand_builtin_fputs"
	.size	.L__FUNCTION__.expand_builtin_fputs, 21

	.type	.L__FUNCTION__.c_readstr,@object # @__FUNCTION__.c_readstr
.L__FUNCTION__.c_readstr:
	.asciz	"c_readstr"
	.size	.L__FUNCTION__.c_readstr, 10

	.type	.L__FUNCTION__.builtin_memcpy_read_str,@object # @__FUNCTION__.builtin_memcpy_read_str
.L__FUNCTION__.builtin_memcpy_read_str:
	.asciz	"builtin_memcpy_read_str"
	.size	.L__FUNCTION__.builtin_memcpy_read_str, 24

	.type	.L__FUNCTION__.expand_builtin_ffs,@object # @__FUNCTION__.expand_builtin_ffs
.L__FUNCTION__.expand_builtin_ffs:
	.asciz	"expand_builtin_ffs"
	.size	.L__FUNCTION__.expand_builtin_ffs, 19

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"invalid arg to `__builtin_frame_address'"
	.size	.L.str116, 41

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"invalid arg to `__builtin_return_address'"
	.size	.L.str117, 42

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"unsupported arg to `__builtin_frame_address'"
	.size	.L.str118, 45

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"unsupported arg to `__builtin_return_address'"
	.size	.L.str119, 46

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"`va_start' used in function with fixed args"
	.size	.L.str120, 44

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"second parameter of `va_start' not last named argument"
	.size	.L.str121, 55

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"`__builtin_next_arg' called without an argument"
	.size	.L.str122, 48

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"argument of `__builtin_args_info' must be constant"
	.size	.L.str123, 51

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"argument of `__builtin_args_info' out of range"
	.size	.L.str124, 47

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"missing argument in `__builtin_args_info'"
	.size	.L.str125, 42

	.type	apply_result_mode,@object # @apply_result_mode
	.local	apply_result_mode
	.comm	apply_result_mode,212,16
	.type	apply_result_size.size,@object # @apply_result_size.size
	.data
	.align	4
apply_result_size.size:
	.long	4294967295              # 0xffffffff
	.size	apply_result_size.size, 4

	.type	.L__FUNCTION__.apply_result_size,@object # @__FUNCTION__.apply_result_size
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.apply_result_size:
	.asciz	"apply_result_size"
	.size	.L__FUNCTION__.apply_result_size, 18

	.type	apply_args_mode,@object # @apply_args_mode
	.local	apply_args_mode
	.comm	apply_args_mode,212,16
	.type	.L__FUNCTION__.expand_builtin_apply,@object # @__FUNCTION__.expand_builtin_apply
.L__FUNCTION__.expand_builtin_apply:
	.asciz	"expand_builtin_apply"
	.size	.L__FUNCTION__.expand_builtin_apply, 21

	.type	.L__FUNCTION__.expand_builtin_mathfn,@object # @__FUNCTION__.expand_builtin_mathfn
.L__FUNCTION__.expand_builtin_mathfn:
	.asciz	"expand_builtin_mathfn"
	.size	.L__FUNCTION__.expand_builtin_mathfn, 22

	.type	apply_args_size.size,@object # @apply_args_size.size
	.data
	.align	4
apply_args_size.size:
	.long	4294967295              # 0xffffffff
	.size	apply_args_size.size, 4

	.type	.L__FUNCTION__.apply_args_size,@object # @__FUNCTION__.apply_args_size
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.apply_args_size:
	.asciz	"apply_args_size"
	.size	.L__FUNCTION__.apply_args_size, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
