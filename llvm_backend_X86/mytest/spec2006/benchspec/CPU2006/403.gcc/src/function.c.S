	.file	"function.c.bc"
	.text
	.globl	find_function_data
	.align	16, 0x90
	.type	find_function_data,@function
find_function_data:                     # @find_function_data
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movq	outer_function_chain(%rip), %rax
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_3:                                # %for.cond
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	56(%rax), %rax
.LBB0_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB0_2
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	48(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB0_3
# BB#5:                                 # %if.then
	popq	%rdx
	retq
.LBB0_2:                                # %for.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_function_data, %rcx
	movq	$329, %rsi              # imm = 0x149
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp2:
	.size	find_function_data, .Ltmp2-find_function_data
	.cfi_endproc

	.globl	push_function_context_to
	.align	16, 0x90
	.type	push_function_context_to,@function
push_function_context_to:               # @push_function_context_to
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	testq	%rdi, %rdi
	je	.LBB1_9
# BB#1:                                 # %if.then
	movq	current_function_decl(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB1_2
# BB#3:                                 # %if.else
	movq	outer_function_chain(%rip), %rax
	jmp	.LBB1_4
	.align	16, 0x90
.LBB1_6:                                # %for.cond.i
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	56(%rax), %rax
.LBB1_4:                                # %if.else
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB1_5
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB1_4 Depth=1
	movq	48(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB1_6
	jmp	.LBB1_8
.LBB1_2:                                # %if.then1
	movq	cfun(%rip), %rax
.LBB1_8:                                # %find_function_data.exit
	movl	424(%rax), %ecx
	orq	$1024, %rcx             # imm = 0x400
	movl	%ecx, 424(%rax)
.LBB1_9:                                # %if.end6
	movq	cfun(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_11
# BB#10:                                # %if.then8
	callq	prepare_function_start
	movq	cfun(%rip), %rdi
.LBB1_11:                               # %if.end9
	movq	outer_function_chain(%rip), %rax
	movq	%rax, 56(%rdi)
	movq	%rdi, outer_function_chain(%rip)
	movq	$0, 344(%rdi)
	movq	save_lang_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_13
# BB#12:                                # %if.then11
	callq	*%rax
.LBB1_13:                               # %if.end12
	movq	$0, cfun(%rip)
	popq	%rax
	retq
.LBB1_5:                                # %for.end.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_function_data, %rcx
	movq	$329, %rsi              # imm = 0x149
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp5:
	.size	push_function_context_to, .Ltmp5-push_function_context_to
	.cfi_endproc

	.globl	init_dummy_function_start
	.align	16, 0x90
	.type	init_dummy_function_start,@function
init_dummy_function_start:              # @init_dummy_function_start
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	prepare_function_start  # TAILCALL
.Ltmp6:
	.size	init_dummy_function_start, .Ltmp6-init_dummy_function_start
	.cfi_endproc

	.globl	push_function_context
	.align	16, 0x90
	.type	push_function_context,@function
push_function_context:                  # @push_function_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp8:
	.cfi_def_cfa_offset 16
	cmpq	$0, current_function_decl(%rip)
	je	.LBB3_2
# BB#1:                                 # %if.then1.i
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$1024, %rcx             # imm = 0x400
	movl	%ecx, 424(%rax)
.LBB3_2:                                # %if.end6.i
	movq	cfun(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB3_4
# BB#3:                                 # %if.then8.i
	callq	prepare_function_start
	movq	cfun(%rip), %rdi
.LBB3_4:                                # %if.end9.i
	movq	outer_function_chain(%rip), %rax
	movq	%rax, 56(%rdi)
	movq	%rdi, outer_function_chain(%rip)
	movq	$0, 344(%rdi)
	movq	save_lang_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB3_6
# BB#5:                                 # %if.then11.i
	callq	*%rax
.LBB3_6:                                # %push_function_context_to.exit
	movq	$0, cfun(%rip)
	popq	%rax
	retq
.Ltmp9:
	.size	push_function_context, .Ltmp9-push_function_context
	.cfi_endproc

	.globl	pop_function_context_from
	.align	16, 0x90
	.type	pop_function_context_from,@function
pop_function_context_from:              # @pop_function_context_from
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
	movq	outer_function_chain(%rip), %r14
	movq	%r14, cfun(%rip)
	movq	56(%r14), %rax
	movq	%rax, outer_function_chain(%rip)
	movq	48(%r14), %rax
	movq	%rax, current_function_decl(%rip)
	movq	$0, reg_renumber(%rip)
	movq	%r14, %rdi
	callq	restore_emit_status
	movq	restore_lang_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB4_2
# BB#1:                                 # %if.then
	movq	%r14, %rdi
	callq	*%rax
.LBB4_2:                                # %if.end
	movq	344(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB4_9
# BB#3:                                 # %if.else
	cmpq	$0, 16(%rbx)
	je	.LBB4_10
# BB#4:
	xorq	%r15, %r15
	.align	16, 0x90
.LBB4_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	16(%rbx), %rbx
	movq	%rax, %r15
	testq	%rbx, %rbx
	jne	.LBB4_5
# BB#6:                                 # %for.cond16.preheader
	movq	344(%r14), %rbx
	jmp	.LBB4_8
	.align	16, 0x90
.LBB4_7:                                # %for.body18
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	(%rbx), %rdi
	movl	12(%rbx), %edx
	movl	8(%rbx), %esi
	xorq	%r8, %r8
	movq	%r15, %rcx
	callq	fixup_var_refs
	movq	16(%rbx), %rbx
.LBB4_8:                                # %for.body18
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB4_7
	jmp	.LBB4_9
.LBB4_10:                               # %if.then4
	movq	(%rbx), %rdi
	movl	12(%rbx), %edx
	movl	8(%rbx), %esi
	xorq	%r8, %r8
	movq	%rdi, %rcx
	callq	fixup_var_refs
.LBB4_9:                                # %if.end26
	movq	$0, 344(%r14)
	movq	$1, %rax
	movl	%eax, rtx_equal_function_value_matters(%rip)
	xorq	%rcx, %rcx
	movl	%ecx, virtuals_instantiated(%rip)
	movl	%eax, generating_concat_p(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp20:
	.size	pop_function_context_from, .Ltmp20-pop_function_context_from
	.cfi_endproc

	.align	16, 0x90
	.type	fixup_var_refs,@function
fixup_var_refs:                         # @fixup_var_refs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp32:
	.cfi_def_cfa_offset 64
.Ltmp33:
	.cfi_offset %rbx, -48
.Ltmp34:
	.cfi_offset %r12, -40
.Ltmp35:
	.cfi_offset %r13, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r13
	callq	get_insns
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rdx
	movq	32(%rdx), %rbx
	testq	%r14, %r14
	je	.LBB5_8
# BB#1:                                 # %if.then
	testq	%rbx, %rbx
	jne	.LBB5_21
# BB#2:                                 # %if.end
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	hash_lookup
	movq	24(%rax), %rbx
	jmp	.LBB5_3
	.align	16, 0x90
.LBB5_6:                                # %for.inc.i
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	16(%rbx), %rbx
.LBB5_3:                                # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB5_7
# BB#4:                                 # %for.body.i
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB5_6
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	$1, %r8
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	8(%rsp), %r9            # 8-byte Reload
	callq	fixup_var_refs_insn
	jmp	.LBB5_6
.LBB5_8:                                # %if.end2
	movq	224(%rcx), %r14
	testq	%rbx, %rbx
	jne	.LBB5_9
# BB#10:                                # %if.end2
	movabsq	$1, %r8
	jmp	.LBB5_11
.LBB5_9:
	movabsq	$0, %r8
.LBB5_11:                               # %if.end2
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	8(%rsp), %r9            # 8-byte Reload
	callq	fixup_var_refs_insns
	jmp	.LBB5_12
	.align	16, 0x90
.LBB5_20:                               # %for.body
                                        #   in Loop: Header=BB5_12 Depth=1
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	8(%rsp), %r9            # 8-byte Reload
	callq	fixup_var_refs_insns
	callq	get_last_insn
	movq	%rax, 8(%rbx)
	callq	end_sequence
	movq	24(%rbx), %rbx
.LBB5_12:                               # %if.end2
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB5_13
# BB#18:                                # %for.body
                                        #   in Loop: Header=BB5_12 Depth=1
	movq	(%rbx), %rdi
	movq	8(%rbx), %rsi
	callq	push_to_full_sequence
	movq	(%rbx), %rdi
	cmpq	$0, 24(%rbx)
	movabsq	$0, %r8
	je	.LBB5_20
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB5_12 Depth=1
	movabsq	$1, %r8
	jmp	.LBB5_20
	.align	16, 0x90
.LBB5_17:                               # %for.inc20
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	(%r14), %r14
.LBB5_13:                               # %for.cond11.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB5_7
# BB#14:                                # %for.body13
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	32(%r14), %rax
	movq	32(%rax), %rbx
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB5_17
# BB#15:                                # %for.body13
                                        #   in Loop: Header=BB5_13 Depth=1
	testq	%rbx, %rbx
	je	.LBB5_17
# BB#16:                                # %if.then18
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %rdi
	callq	push_to_sequence
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	8(%rsp), %r9            # 8-byte Reload
	callq	fixup_var_refs_insns
	callq	end_sequence
	jmp	.LBB5_17
.LBB5_7:                                # %for.end21
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_21:                               # %if.then1
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fixup_var_refs, %rcx
	movq	$1593, %rsi             # imm = 0x639
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp38:
	.size	fixup_var_refs, .Ltmp38-fixup_var_refs
	.cfi_endproc

	.globl	pop_function_context
	.align	16, 0x90
	.type	pop_function_context,@function
pop_function_context:                   # @pop_function_context
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	pop_function_context_from  # TAILCALL
.Ltmp39:
	.size	pop_function_context, .Ltmp39-pop_function_context
	.cfi_endproc

	.globl	free_after_parsing
	.align	16, 0x90
	.type	free_after_parsing,@function
free_after_parsing:                     # @free_after_parsing
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp42:
	.cfi_def_cfa_offset 16
.Ltmp43:
	.cfi_offset %rbx, -16
	movq	free_lang_status(%rip), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB7_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	*%rax
.LBB7_2:                                # %if.end
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free_stmt_status  # TAILCALL
.Ltmp44:
	.size	free_after_parsing, .Ltmp44-free_after_parsing
	.cfi_endproc

	.globl	free_after_compilation
	.align	16, 0x90
	.type	free_after_compilation,@function
free_after_compilation:                 # @free_after_compilation
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
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	free_eh_status
	movq	%rbx, %rdi
	callq	free_expr_status
	movq	%rbx, %rdi
	callq	free_emit_status
	movq	%rbx, %rdi
	callq	free_varasm_status
	movq	free_machine_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB8_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	*%rax
.LBB8_2:                                # %if.end
	movq	312(%rbx), %rdi
	leaq	312(%rbx), %r14
	testq	%rdi, %rdi
	je	.LBB8_4
# BB#3:                                 # %if.then2
	callq	free
.LBB8_4:                                # %if.end4
	movq	$0, 80(%rbx)
	movq	$0, 344(%rbx)
	movq	$0, 416(%rbx)
	movq	$0, 128(%rbx)
	movq	$0, 120(%rbx)
	movq	$0, 256(%rbx)
	movq	$0, 248(%rbx)
	movq	$0, 240(%rbx)
	movq	$0, 232(%rbx)
	movq	$0, 224(%rbx)
	movq	$0, 216(%rbx)
	movq	$0, 208(%rbx)
	movq	$0, 200(%rbx)
	movq	$0, 192(%rbx)
	movq	$0, 184(%rbx)
	movq	$0, 176(%rbx)
	movq	$0, 168(%rbx)
	movq	$0, 160(%rbx)
	movq	$0, 296(%rbx)
	movq	$0, 288(%rbx)
	movq	$0, 280(%rbx)
	movq	$0, 272(%rbx)
	movq	$0, 8(%r14)
	movq	$0, (%r14)
	movq	$0, 376(%rbx)
	movq	$0, 368(%rbx)
	movq	$0, 360(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp53:
	.size	free_after_compilation, .Ltmp53-free_after_compilation
	.cfi_endproc

	.globl	get_func_frame_size
	.align	16, 0x90
	.type	get_func_frame_size,@function
get_func_frame_size:                    # @get_func_frame_size
	.cfi_startproc
# BB#0:                                 # %entry
	movq	264(%rdi), %rax
	negq	%rax
	retq
.Ltmp54:
	.size	get_func_frame_size, .Ltmp54-get_func_frame_size
	.cfi_endproc

	.globl	get_frame_size
	.align	16, 0x90
	.type	get_frame_size,@function
get_frame_size:                         # @get_frame_size
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	264(%rax), %rax
	negq	%rax
	retq
.Ltmp55:
	.size	get_frame_size, .Ltmp55-get_frame_size
	.cfi_endproc

	.globl	assign_stack_local
	.align	16, 0x90
	.type	assign_stack_local,@function
assign_stack_local:                     # @assign_stack_local
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rcx
	jmp	assign_stack_local_1  # TAILCALL
.Ltmp56:
	.size	assign_stack_local, .Ltmp56-assign_stack_local
	.cfi_endproc

	.align	16, 0x90
	.type	assign_stack_local_1,@function
assign_stack_local_1:                   # @assign_stack_local_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp65:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp66:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp67:
	.cfi_def_cfa_offset 48
.Ltmp68:
	.cfi_offset %rbx, -48
.Ltmp69:
	.cfi_offset %r12, -40
.Ltmp70:
	.cfi_offset %r13, -32
.Ltmp71:
	.cfi_offset %r14, -24
.Ltmp72:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rsi, %r15
	movq	%rdi, %r14
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rdx, %rax
	andq	%r8, %rax
	cmpq	%r8, %rax
	jne	.LBB12_1
# BB#7:                                 # %if.then9
	addq	$15, %r15
	andq	$-16, %r15
	movq	$16, %rbx
	jmp	.LBB12_9
.LBB12_1:                               # %entry
	testq	%r8, %rdx
	jne	.LBB12_8
# BB#2:                                 # %if.then
	movq	%r14, %rax
	andq	%r8, %rax
	movq	$128, %r12
	cmpq	$51, %rax
	je	.LBB12_4
# BB#3:                                 # %if.else
	movq	%r14, %rdi
	callq	get_mode_alignment
	movq	%rax, %r12
.LBB12_4:                               # %if.end
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	type_for_mode
	testq	%rax, %rax
	je	.LBB12_6
# BB#5:                                 # %if.then4
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	ix86_local_alignment
	movq	%rax, %r12
.LBB12_6:                               # %if.end6
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$60, %rcx
	shrq	%cl, %rbx
	andq	$7, %rbx
	addq	%r12, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$35, %rcx
	sarq	%cl, %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB12_9
.LBB12_8:                               # %if.else12
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$60, %rcx
	shrq	%cl, %rbx
	andq	$7, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$35, %rcx
	sarq	%cl, %rbx
.LBB12_9:                               # %if.end15
	movq	264(%r13), %rsi
	subq	%r15, %rsi
	movq	%rsi, 264(%r13)
	movq	$35, %rcx
	movq	%rbx, %rdi
	movslq	ix86_preferred_stack_boundary(%rip), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdx, %rdi
	jle	.LBB12_11
# BB#10:                                # %if.then19
	movq	$60, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	andq	$7, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$35, %rcx
	sarq	%cl, %rbx
.LBB12_11:                              # %if.end21
	movslq	400(%r13), %rdi
	movq	$3, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$35, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	jge	.LBB12_13
# BB#12:                                # %if.then25
	movl	%edx, 400(%r13)
.LBB12_13:                              # %cond.end
	negq	%rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	%rbx, %rsi
	movq	%rsi, 264(%r13)
	movq	cfun(%rip), %rax
	cmpq	%r13, %rax
	jne	.LBB12_16
# BB#14:                                # %cond.end
	movl	virtuals_instantiated(%rip), %eax
	testq	%r8, %rax
	je	.LBB12_16
# BB#15:                                # %if.then45
	movq	global_rtl+24(%rip), %rdi
	jmp	.LBB12_17
.LBB12_16:                              # %if.else51
	movq	global_rtl+56(%rip), %rdi
.LBB12_17:                              # %if.end56
	callq	plus_constant_wide
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	216(%r13), %rcx
	movq	%rax, %rbx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 216(%r13)
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp73:
	.size	assign_stack_local_1, .Ltmp73-assign_stack_local_1
	.cfi_endproc

	.globl	assign_stack_temp_for_type
	.align	16, 0x90
	.type	assign_stack_temp_for_type,@function
assign_stack_temp_for_type:             # @assign_stack_temp_for_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp80:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp81:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp82:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp83:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp84:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp85:
	.cfi_def_cfa_offset 96
.Ltmp86:
	.cfi_offset %rbx, -48
.Ltmp87:
	.cfi_offset %r12, -40
.Ltmp88:
	.cfi_offset %r13, -32
.Ltmp89:
	.cfi_offset %r14, -24
.Ltmp90:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %rbx
	cmpq	$-1, %r12
	je	.LBB13_1
# BB#3:                                 # %if.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rbx, %rdx
	andq	%rax, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	$128, %r15
	cmpq	$51, %rdx
	je	.LBB13_5
# BB#4:                                 # %if.else
	movq	%rbx, %rdi
	movq	%rcx, %r14
	callq	get_mode_alignment
	movq	%r14, %rcx
	movq	%rax, %r15
.LBB13_5:                               # %if.end3
	testq	%rcx, %rcx
	jne	.LBB13_8
# BB#6:                                 # %if.end6
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%rbx, %rdi
	callq	type_for_mode
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB13_7
.LBB13_8:                               # %if.then8
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %rdi
	movq	%rcx, %r14
	movq	%r15, %rsi
	callq	ix86_local_alignment
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$1, %r13
.LBB13_9:                               # %if.end10
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	cfun(%rip), %rax
	movq	320(%rax), %rbx
	testq	%rbx, %rbx
	movq	%r14, %rcx
	je	.LBB13_30
# BB#10:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	movq	40(%rsp), %r15          # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	movq	24(%rsp), %rdx          # 8-byte Reload
	.align	16, 0x90
.LBB13_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	24(%rbx), %eax
	cmpq	%r15, %rax
	jb	.LBB13_17
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	32(%rbx), %rax
	cmpq	%r12, %rax
	jl	.LBB13_17
# BB#13:                                # %land.lhs.true16
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	cmpq	%rdx, %rax
	jne	.LBB13_17
# BB#14:                                # %land.lhs.true19
                                        #   in Loop: Header=BB13_11 Depth=1
	movzbl	56(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB13_17
# BB#15:                                # %land.lhs.true21
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	%rdx, %r14
	movq	40(%rbx), %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	objects_must_conflict_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_22
# BB#16:                                #   in Loop: Header=BB13_11 Depth=1
	movq	%r14, %rdx
	jmp	.LBB13_17
.LBB13_22:                              # %land.lhs.true25
                                        #   in Loop: Header=BB13_11 Depth=1
	testq	%r13, %r13
	movq	%r14, %rdx
	je	.LBB13_26
# BB#23:                                # %lor.lhs.false
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	32(%r13), %rax
	movq	32(%rbx), %rcx
	cmpq	%rcx, %rax
	jg	.LBB13_26
# BB#24:                                # %lor.lhs.false30
                                        #   in Loop: Header=BB13_11 Depth=1
	jne	.LBB13_17
# BB#25:                                # %land.lhs.true34
                                        #   in Loop: Header=BB13_11 Depth=1
	movl	24(%r13), %eax
	movl	24(%rbx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB13_17
.LBB13_26:                              # %if.then38
                                        #   in Loop: Header=BB13_11 Depth=1
	movl	24(%rbx), %eax
	cmpq	%r15, %rax
	movq	%rbx, %r13
	jne	.LBB13_17
# BB#27:                                # %land.lhs.true41
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	32(%rbx), %rax
	cmpq	%r12, %rax
	movq	%rbx, %r13
	je	.LBB13_28
	.align	16, 0x90
.LBB13_17:                              # %for.inc
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_11
# BB#18:                                # %for.end
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	movq	32(%rsp), %rcx          # 8-byte Reload
	je	.LBB13_30
# BB#19:                                # %if.then48
	movq	8(%r13), %rax
	movl	(%rax), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB13_38
# BB#20:                                # %if.then54
	movl	24(%r13), %eax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	32(%r13), %rcx
	leaq	-1(%r12,%rax), %rdx
	movq	%rax, %r15
	negq	%r15
	andq	%rdx, %r15
	subq	%r15, %rcx
	cmpq	%rax, %rcx
	jge	.LBB13_50
# BB#21:
	movq	%r13, %rbx
.LBB13_29:                              # %if.end91
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rbx, %rbx
	movq	%rbx, %r13
	jne	.LBB13_38
.LBB13_30:                              # %if.then94
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	cfun(%rip), %rax
	movq	264(%rax), %r15
	movq	$88, %rdi
	callq	ggc_alloc
	movq	%rax, %r13
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	$51, %rdx
	jne	.LBB13_33
# BB#31:                                # %if.then94
	movq	40(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$127, %rax
	jbe	.LBB13_32
.LBB13_33:                              # %if.end102
	cmpq	$51, %rdx
	jne	.LBB13_34
# BB#35:                                # %cond.true
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	movq	40(%rsp), %rbx          # 8-byte Reload
	andq	%rbx, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	addq	%rax, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	addq	%rcx, %r12
	negq	%rax
	andq	%r12, %rax
	andq	%rcx, %rax
	movq	%rax, %r12
	jmp	.LBB13_36
.LBB13_34:
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB13_36:                              # %cond.end
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	cfun(%rip), %rcx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	assign_stack_local_1
	movq	%rax, 8(%r13)
	movl	%ebx, 24(%r13)
	movq	cfun(%rip), %rax
	movq	264(%rax), %rax
	subq	%rax, %r15
	movq	%r15, 32(%r13)
	movq	%rax, 72(%r13)
	movq	%r15, 80(%r13)
	movq	$0, 16(%r13)
	movq	cfun(%rip), %rax
	movq	320(%rax), %rax
	movq	%rax, (%r13)
	movq	cfun(%rip), %rax
	movq	%r13, 320(%rax)
.LBB13_37:                              # %if.end129
	movq	32(%rsp), %rcx          # 8-byte Reload
.LBB13_38:                              # %if.end129
	movq	$1, %rax
	movw	%ax, 56(%r13)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	24(%rax), %rax
	movq	%rax, 48(%r13)
	movq	%rcx, 40(%r13)
	movq	%rcx, %r14
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB13_40
# BB#39:                                # %if.then136
	movq	cfun(%rip), %rax
	movl	336(%rax), %eax
	jmp	.LBB13_42
.LBB13_40:                              # %if.else138
	movq	cfun(%rip), %rcx
	cmpq	$3, %rax
	jne	.LBB13_43
# BB#41:                                # %if.then141
	movl	332(%rcx), %eax
.LBB13_42:                              # %if.end148
	movl	%eax, 60(%r13)
	xorq	%rax, %rax
	movl	%eax, 64(%r13)
	jmp	.LBB13_44
.LBB13_43:                              # %if.else144
	movl	328(%rcx), %eax
	movl	%eax, 60(%r13)
	movl	%edx, 64(%r13)
.LBB13_44:                              # %if.end148
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	8(%r13), %rax
	movq	8(%rax), %rsi
	callq	gen_rtx_MEM
	movq	cfun(%rip), %rcx
	movq	216(%rcx), %rcx
	movq	%rax, %rbx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	cfun(%rip), %rcx
	movq	%rax, 216(%rcx)
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	je	.LBB13_48
# BB#45:                                # %cond.true158
	movq	%r14, %rdi
	callq	get_alias_set
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	set_mem_align
	movl	16(%r14), %edx
	movl	(%rbx), %eax
	movq	$14, %rcx
	shlq	%cl, %rdx
	andq	$67108864, %rdx         # imm = 0x4000000
	andq	$-67108865, %rax        # imm = 0xFFFFFFFFFBFFFFFF
	orq	%rdx, %rax
	movl	%eax, (%rbx)
	movl	16(%r14), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	andq	$134217728, %rdx        # imm = 0x8000000
	andq	$-134217729, %rax       # imm = 0xFFFFFFFFF7FFFFFF
	orq	%rdx, %rax
	movl	%eax, (%rbx)
	movzbl	16(%r14), %ecx
	addq	$-18, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	$1879048191, %rax       # imm = 0x6FFFFFFF
	cmpq	$4, %rcx
	ja	.LBB13_47
# BB#46:                                # %if.then213
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%rbx)
	jmp	.LBB13_49
.LBB13_48:                              # %if.end226.critedge
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_mem_alias_set
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	set_mem_align
	jmp	.LBB13_49
.LBB13_47:                              # %if.else219
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	orq	%rcx, %rax
	movl	%eax, (%rbx)
.LBB13_49:                              # %if.end226
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_7:
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%r15, 40(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	jmp	.LBB13_9
.LBB13_50:                              # %if.then63
	movq	$88, %rdi
	callq	ggc_alloc
	movq	%rax, %r12
	xorq	%rax, %rax
	movw	%ax, 56(%r12)
	movq	32(%r13), %rax
	subq	%r15, %rax
	movq	%rax, 32(%r12)
	movq	72(%r13), %rax
	addq	%r15, %rax
	movq	%rax, 72(%r12)
	movq	80(%r13), %rax
	subq	%r15, %rax
	movq	%rax, 80(%r12)
	movq	8(%r13), %rax
	movq	8(%rax), %rdi
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, 8(%r12)
	movl	24(%r13), %ecx
	movl	%ecx, 24(%r12)
	movq	$0, 16(%r12)
	movq	$0, 48(%r12)
	movq	40(%r13), %rcx
	movq	%rcx, 40(%r12)
	movq	cfun(%rip), %rcx
	movq	320(%rcx), %rcx
	movq	%rcx, (%r12)
	movq	cfun(%rip), %rcx
	movq	%r12, 320(%rcx)
	movq	cfun(%rip), %rcx
	movq	216(%rcx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	cfun(%rip), %rcx
	movq	%rax, 216(%rcx)
	movq	%r15, 32(%r13)
	movq	%r15, 80(%r13)
	jmp	.LBB13_37
.LBB13_28:
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB13_29
.LBB13_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.assign_stack_temp_for_type, %rcx
	movq	$674, %rsi              # imm = 0x2A2
	jmp	.LBB13_2
.LBB13_32:                              # %if.then101
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.assign_stack_temp_for_type, %rcx
	movq	$759, %rsi              # imm = 0x2F7
.LBB13_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp91:
	.size	assign_stack_temp_for_type, .Ltmp91-assign_stack_temp_for_type
	.cfi_endproc

	.globl	assign_stack_temp
	.align	16, 0x90
	.type	assign_stack_temp,@function
assign_stack_temp:                      # @assign_stack_temp
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rcx, %rcx
	jmp	assign_stack_temp_for_type  # TAILCALL
.Ltmp92:
	.size	assign_stack_temp, .Ltmp92-assign_stack_temp
	.cfi_endproc

	.globl	assign_temp
	.align	16, 0x90
	.type	assign_temp,@function
assign_temp:                            # @assign_temp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp99:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp100:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp101:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp102:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp103:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp104:
	.cfi_def_cfa_offset 64
.Ltmp105:
	.cfi_offset %rbx, -48
.Ltmp106:
	.cfi_offset %r12, -40
.Ltmp107:
	.cfi_offset %r13, -32
.Ltmp108:
	.cfi_offset %r14, -24
.Ltmp109:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	16(%r12), %edi
	movq	%rdi, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %ebx
	movq	%rcx, %rax
	movq	%rsi, %r15
	cmpq	$100, %rbx
	jne	.LBB15_1
# BB#2:                                 # %if.then
	movq	8(%r12), %r14
	movl	16(%r14), %edi
	jmp	.LBB15_3
.LBB15_1:
	movq	%r12, %r14
	xorq	%r12, %r12
.LBB15_3:                               # %if.end
	movl	60(%r14), %ebx
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	movq	$13, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	cmpq	$51, %rbx
	movl	%edi, 12(%rsp)
	je	.LBB15_5
# BB#4:                                 # %if.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	jne	.LBB15_5
# BB#17:                                # %if.end56
	testq	%rcx, %rax
	jne	.LBB15_19
# BB#18:                                # %if.then58
	leaq	12(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	promote_mode
	movq	%rax, %rbx
.LBB15_19:                              # %if.end60
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	jmp	.LBB15_20
.LBB15_5:                               # %if.then14
	movq	%r14, %rdi
	callq	int_size_in_bytes
	testq	%rax, %rax
	movq	$1, %r13
	je	.LBB15_7
# BB#6:                                 # %if.then14
	movq	%rax, %r13
.LBB15_7:                               # %if.then14
	cmpq	$-1, %r13
	jne	.LBB15_16
# BB#8:                                 # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	$18, %rax
	movq	$-1, %r13
	jne	.LBB15_12
# BB#9:                                 # %land.lhs.true27
	movq	112(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB15_12
# BB#10:                                # %land.lhs.true31
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_12
# BB#11:                                # %if.then36
	movq	112(%r14), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %r13
.LBB15_12:                              # %if.end40
	testq	%r12, %r12
	je	.LBB15_16
# BB#13:                                # %if.end40
	cmpq	$-1, %r13
	jne	.LBB15_16
# BB#14:                                # %land.lhs.true45
	movq	40(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	movq	$-1, %r13
	jne	.LBB15_16
# BB#15:                                # %if.then53
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	error_with_decl
	movq	$1, %r13
.LBB15_16:                              # %if.end54
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	assign_stack_temp_for_type
.LBB15_20:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp110:
	.size	assign_temp, .Ltmp110-assign_temp
	.cfi_endproc

	.globl	combine_temp_slots
	.align	16, 0x90
	.type	combine_temp_slots,@function
combine_temp_slots:                     # @combine_temp_slots
	.cfi_startproc
# BB#0:                                 # %entry
	movl	flag_strict_aliasing(%rip), %eax
	testq	%rax, %rax
	jne	.LBB16_28
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movl	flag_expensive_optimizations(%rip), %ecx
	movq	320(%rax), %r10
	testq	%rcx, %rcx
	jne	.LBB16_8
# BB#2:                                 # %if.then2
	testq	%r10, %r10
	je	.LBB16_28
# BB#3:                                 # %for.body.lr.ph
	movl	optimize(%rip), %ecx
	xorq	%rdx, %rdx
	movq	%r10, %rsi
	testq	%rcx, %rcx
	je	.LBB16_4
	.align	16, 0x90
.LBB16_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$100, %rdi
	jg	.LBB16_28
# BB#7:                                 # %for.inc
                                        #   in Loop: Header=BB16_6 Depth=1
	movq	(%rsi), %rsi
	incq	%rdx
	testq	%rsi, %rsi
	jne	.LBB16_6
	jmp	.LBB16_8
	.align	16, 0x90
.LBB16_4:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$10, %rdi
	jg	.LBB16_28
# BB#5:                                 # %for.inc.us
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	(%rsi), %rsi
	incq	%rdx
	testq	%rsi, %rsi
	jne	.LBB16_4
.LBB16_8:                               # %if.end8
	testq	%r10, %r10
	je	.LBB16_28
# BB#9:
	xorq	%r8, %r8
	.align	16, 0x90
.LBB16_10:                              # %for.body12
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_14 Depth 2
                                        #       Child Loop BB16_15 Depth 3
	movzbl	56(%r10), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_24
# BB#11:                                # %land.lhs.true14
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	8(%r10), %rcx
	movl	(%rcx), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	jne	.LBB16_24
# BB#12:                                # %for.cond18.preheader
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	(%r10), %rdi
	testq	%rdi, %rdi
	je	.LBB16_24
# BB#13:                                # %for.body20.lr.ph.lr.ph
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	%r10, %r9
.LBB16_14:                              # %for.body20.lr.ph
                                        #   Parent Loop BB16_10 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_15 Depth 3
	movq	%rdi, %rdx
	.align	16, 0x90
.LBB16_15:                              # %for.body20
                                        #   Parent Loop BB16_10 Depth=1
                                        #     Parent Loop BB16_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	56(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_20
# BB#16:                                # %land.lhs.true23
                                        #   in Loop: Header=BB16_15 Depth=3
	movq	8(%rdx), %rcx
	movl	(%rcx), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	jne	.LBB16_20
# BB#17:                                # %if.then29
                                        #   in Loop: Header=BB16_15 Depth=3
	movq	72(%r10), %rcx
	movq	80(%r10), %rdi
	movq	72(%rdx), %rsi
	leaq	(%rdi,%rcx), %rax
	cmpq	%rsi, %rax
	jne	.LBB16_19
# BB#18:                                # %if.then32
                                        #   in Loop: Header=BB16_15 Depth=3
	movq	32(%rdx), %rax
	addq	%rax, 32(%r10)
	movq	80(%rdx), %rax
	addq	%rax, %rdi
	movq	%rdi, 80(%r10)
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, (%r9)
	jne	.LBB16_15
	jmp	.LBB16_21
	.align	16, 0x90
.LBB16_19:                              # %if.else
                                        #   in Loop: Header=BB16_14 Depth=2
	movq	80(%rdx), %r9
	addq	%r9, %rsi
	cmpq	%rcx, %rsi
	je	.LBB16_22
.LBB16_20:                              # %for.cond18.outer.backedge
                                        #   in Loop: Header=BB16_14 Depth=2
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	movq	%rdx, %r9
	jne	.LBB16_14
.LBB16_21:                              # %for.inc75
                                        #   in Loop: Header=BB16_10 Depth=1
	testq	%r10, %r10
	jne	.LBB16_24
	jmp	.LBB16_26
.LBB16_22:                              # %if.then43
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	32(%r10), %rax
	addq	%rax, 32(%rdx)
	addq	%rdi, %r9
	movq	%r9, 80(%rdx)
	movq	(%r10), %rax
	testq	%r8, %r8
	jne	.LBB16_23
# BB#25:                                # %for.inc75.thread112
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	cfun(%rip), %rcx
	movq	%rax, 320(%rcx)
.LBB16_26:                              # %cond.false
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	cfun(%rip), %r10
	addq	$320, %r10              # imm = 0x140
	xorq	%r8, %r8
	jmp	.LBB16_27
.LBB16_23:                              # %if.then66
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	%rax, (%r8)
	movq	%r8, %r10
	.align	16, 0x90
.LBB16_24:                              # %cond.true
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	%r10, %r8
.LBB16_27:                              # %for.cond10.backedge
                                        #   in Loop: Header=BB16_10 Depth=1
	movq	(%r10), %r10
	testq	%r10, %r10
	jne	.LBB16_10
.LBB16_28:                              # %for.end79
	retq
.Ltmp111:
	.size	combine_temp_slots, .Ltmp111-combine_temp_slots
	.cfi_endproc

	.globl	update_temp_slot_address
	.align	16, 0x90
	.type	update_temp_slot_address,@function
update_temp_slot_address:               # @update_temp_slot_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp118:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp119:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp120:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp121:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp122:
	.cfi_def_cfa_offset 48
.Ltmp123:
	.cfi_offset %rbx, -48
.Ltmp124:
	.cfi_offset %r12, -40
.Ltmp125:
	.cfi_offset %r13, -32
.Ltmp126:
	.cfi_offset %r14, -24
.Ltmp127:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> R14<kill>
	callq	rtx_equal_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB17_20
.LBB17_1:                               # %if.end.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_2 Depth 2
	movq	%r15, %rbx
	.align	16, 0x90
.LBB17_2:                               # %if.end
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	find_temp_slot_from_address
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB17_15
# BB#3:                                 # %if.then2
                                        #   in Loop: Header=BB17_2 Depth=2
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB17_20
# BB#4:                                 # %if.end5
                                        #   in Loop: Header=BB17_2 Depth=2
	movzbl	(%r14), %eax
	cmpq	$75, %rax
	je	.LBB17_7
# BB#5:                                 # %if.end5
                                        #   in Loop: Header=BB17_2 Depth=2
	cmpq	$61, %rax
	jne	.LBB17_20
# BB#6:                                 # %if.then9
                                        #   in Loop: Header=BB17_2 Depth=2
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	update_temp_slot_address
	movq	16(%rbx), %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB17_2
	jmp	.LBB17_20
.LBB17_7:                               # %if.end18
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%rbx), %rdi
	movq	8(%r14), %rsi
	callq	rtx_equal_p
	movq	16(%rbx), %r15
	testq	%r13, %rax
	jne	.LBB17_8
# BB#9:                                 # %if.else34
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%r14), %rsi
	movq	%r15, %rdi
	callq	rtx_equal_p
	movq	8(%rbx), %r15
	movq	16(%r14), %r12
	testq	%r13, %rax
	jne	.LBB17_14
# BB#10:                                # %if.else50
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	movq	16(%rbx), %r15
	testq	%r13, %rax
	jne	.LBB17_13
# BB#11:                                # %if.else66
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	16(%r14), %rsi
	movq	%r15, %rdi
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB17_20
# BB#12:                                # %if.then75
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%rbx), %r15
.LBB17_13:                              # %tailrecurse.outer.backedge
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%r14), %r12
	jmp	.LBB17_14
.LBB17_8:                               # %if.then27
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	16(%r14), %r12
.LBB17_14:                              # %tailrecurse.outer.backedge
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	movq	%r12, %r14
	je	.LBB17_1
	jmp	.LBB17_20
.LBB17_15:                              # %if.else86
	movq	16(%r15), %rax
	testq	%rax, %rax
	je	.LBB17_16
# BB#17:                                # %if.else90
	movzbl	(%rax), %ecx
	cmpq	$3, %rcx
	je	.LBB17_19
# BB#18:                                # %if.then95
	movq	$3, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%r15)
.LBB17_19:                              # %if.end99
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 16(%r15)
	jmp	.LBB17_20
.LBB17_16:                              # %if.then88
	movq	%r14, 16(%r15)
.LBB17_20:                              # %if.end104
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp128:
	.size	update_temp_slot_address, .Ltmp128-update_temp_slot_address
	.cfi_endproc

	.align	16, 0x90
	.type	find_temp_slot_from_address,@function
find_temp_slot_from_address:            # @find_temp_slot_from_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp134:
	.cfi_def_cfa_offset 32
.Ltmp135:
	.cfi_offset %rbx, -24
.Ltmp136:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	320(%rax), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB18_15
# BB#1:                                 # %for.body.lr.ph
	movq	global_rtl+56(%rip), %r8
	.align	16, 0x90
.LBB18_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_12 Depth 2
	movzbl	56(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB18_14
# BB#3:                                 # %if.else
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	8(%rax), %rdx
	movq	8(%rdx), %rdx
	cmpq	%rbx, %rdx
	je	.LBB18_22
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	16(%rax), %rdx
	cmpq	%rbx, %rdx
	je	.LBB18_22
# BB#5:                                 # %lor.lhs.false3
                                        #   in Loop: Header=BB18_2 Depth=1
	movzbl	(%rbx), %esi
	cmpq	$75, %rsi
	jne	.LBB18_10
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	8(%rbx), %rsi
	cmpq	%r8, %rsi
	jne	.LBB18_10
# BB#7:                                 # %land.lhs.true9
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	16(%rbx), %rsi
	movzbl	(%rsi), %edi
	cmpq	$54, %rdi
	jne	.LBB18_10
# BB#8:                                 # %land.lhs.true16
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	8(%rsi), %rsi
	movq	72(%rax), %rdi
	cmpq	%rdi, %rsi
	jl	.LBB18_10
# BB#9:                                 # %land.lhs.true23
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	80(%rax), %rcx
	addq	%rdi, %rcx
	cmpq	%rcx, %rsi
	jl	.LBB18_22
	.align	16, 0x90
.LBB18_10:                              # %if.else33
                                        #   in Loop: Header=BB18_2 Depth=1
	testq	%rdx, %rdx
	je	.LBB18_14
# BB#11:                                # %land.lhs.true36
                                        #   in Loop: Header=BB18_2 Depth=1
	movzbl	(%rdx), %ecx
	cmpq	$3, %rcx
	jne	.LBB18_14
	.align	16, 0x90
.LBB18_12:                              # %for.body45
                                        #   Parent Loop BB18_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rdx), %rcx
	cmpq	%rbx, %rcx
	je	.LBB18_22
# BB#13:                                # %for.inc
                                        #   in Loop: Header=BB18_12 Depth=2
	movq	16(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB18_12
	.align	16, 0x90
.LBB18_14:                              # %for.inc57
                                        #   in Loop: Header=BB18_2 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB18_2
.LBB18_15:                              # %for.end59
	movzbl	(%rbx), %r14d
	cmpq	$75, %r14
	jne	.LBB18_21
# BB#16:                                # %land.lhs.true63
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB18_19
# BB#17:                                # %land.lhs.true70
	callq	find_temp_slot_from_address
	testq	%rax, %rax
	jne	.LBB18_22
# BB#18:                                # %if.else76
	cmpq	$75, %r14
	jne	.LBB18_21
.LBB18_19:                              # %land.lhs.true80
	movq	16(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB18_21
# BB#20:                                # %land.lhs.true87
	callq	find_temp_slot_from_address
	testq	%rax, %rax
	jne	.LBB18_22
.LBB18_21:                              # %if.end95
	xorq	%rax, %rax
.LBB18_22:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp137:
	.size	find_temp_slot_from_address, .Ltmp137-find_temp_slot_from_address
	.cfi_endproc

	.globl	mark_temp_addr_taken
	.align	16, 0x90
	.type	mark_temp_addr_taken,@function
mark_temp_addr_taken:                   # @mark_temp_addr_taken
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp139:
	.cfi_def_cfa_offset 16
	testq	%rdi, %rdi
	je	.LBB19_9
# BB#1:                                 # %if.end
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB19_9
# BB#2:                                 # %lor.lhs.false
	movq	8(%rdi), %rdi
	movzbl	(%rdi), %eax
	cmpq	$133, %rax
	jg	.LBB19_5
# BB#3:                                 # %lor.lhs.false
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_7
# BB#4:                                 # %lor.lhs.false
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB19_9
	jmp	.LBB19_7
.LBB19_5:                               # %lor.lhs.false
	cmpq	$134, %rax
	je	.LBB19_9
# BB#6:                                 # %lor.lhs.false
	cmpq	$140, %rax
	je	.LBB19_9
.LBB19_7:                               # %if.end55
	callq	find_temp_slot_from_address
	testq	%rax, %rax
	je	.LBB19_9
# BB#8:                                 # %if.then60
	movq	$1, %rcx
	movb	%cl, 57(%rax)
.LBB19_9:                               # %if.end61
	popq	%rax
	retq
.Ltmp140:
	.size	mark_temp_addr_taken, .Ltmp140-mark_temp_addr_taken
	.cfi_endproc

	.globl	preserve_temp_slots
	.align	16, 0x90
	.type	preserve_temp_slots,@function
preserve_temp_slots:                    # @preserve_temp_slots
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
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB20_1
# BB#8:                                 # %if.end9
	movl	(%rbx), %r14d
	movabsq	$2147549183, %rax       # imm = 0x8000FFFF
	andq	%r14, %rax
	movabsq	$2147483709, %rcx       # imm = 0x8000003D
	cmpq	%rcx, %rax
	jne	.LBB20_10
# BB#9:                                 # %if.end16
	movq	%rbx, %rdi
	callq	find_temp_slot_from_address
	testq	%rax, %rax
	jne	.LBB20_15
.LBB20_10:                              # %land.lhs.true19
	andq	$65535, %r14            # imm = 0xFFFF
	cmpq	$66, %r14
	jne	.LBB20_27
# BB#11:                                # %lor.lhs.false
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$133, %rax
	jg	.LBB20_25
# BB#12:                                # %lor.lhs.false
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB20_14
# BB#13:                                # %lor.lhs.false
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB20_27
	jmp	.LBB20_14
.LBB20_1:                               # %if.then
	movq	cfun(%rip), %rax
	movq	320(%rax), %rcx
	jmp	.LBB20_2
	.align	16, 0x90
.LBB20_7:                               # %for.inc
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	(%rcx), %rcx
.LBB20_2:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB20_24
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB20_2 Depth=1
	movzbl	56(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB20_7
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB20_2 Depth=1
	movl	60(%rcx), %edx
	movl	328(%rax), %esi
	cmpq	%rsi, %rdx
	jne	.LBB20_7
# BB#5:                                 # %land.lhs.true4
                                        #   in Loop: Header=BB20_2 Depth=1
	movzbl	57(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB20_7
# BB#6:                                 # %if.then7
                                        #   in Loop: Header=BB20_2 Depth=1
	decq	%rdx
	movl	%edx, 60(%rcx)
	jmp	.LBB20_7
.LBB20_25:                              # %lor.lhs.false
	cmpq	$134, %rax
	je	.LBB20_27
# BB#26:                                # %lor.lhs.false
	cmpq	$140, %rax
	jne	.LBB20_14
.LBB20_27:                              # %if.then84
	movq	cfun(%rip), %rax
	movq	320(%rax), %rcx
	jmp	.LBB20_28
	.align	16, 0x90
.LBB20_33:                              # %for.inc105
                                        #   in Loop: Header=BB20_28 Depth=1
	movq	(%rcx), %rcx
.LBB20_28:                              # %if.then84
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB20_24
# BB#29:                                # %for.body88
                                        #   in Loop: Header=BB20_28 Depth=1
	movzbl	56(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB20_33
# BB#30:                                # %land.lhs.true92
                                        #   in Loop: Header=BB20_28 Depth=1
	movl	60(%rcx), %edx
	movl	328(%rax), %esi
	cmpq	%rsi, %rdx
	jne	.LBB20_33
# BB#31:                                # %land.lhs.true97
                                        #   in Loop: Header=BB20_28 Depth=1
	movzbl	57(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB20_33
# BB#32:                                # %if.then101
                                        #   in Loop: Header=BB20_28 Depth=1
	decq	%rdx
	movl	%edx, 60(%rcx)
	jmp	.LBB20_33
.LBB20_14:                              # %if.then111
	callq	find_temp_slot_from_address
	testq	%rax, %rax
	je	.LBB20_34
.LBB20_15:                              # %if.then119
	movq	cfun(%rip), %rcx
	movl	60(%rax), %edx
	movl	328(%rcx), %esi
	cmpq	%rsi, %rdx
	jne	.LBB20_24
# BB#16:                                # %if.then124
	movq	320(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB20_23
	.align	16, 0x90
.LBB20_17:                              # %for.body128
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rax, %rcx
	je	.LBB20_21
# BB#18:                                # %land.lhs.true131
                                        #   in Loop: Header=BB20_17 Depth=1
	movzbl	57(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB20_21
# BB#19:                                # %land.lhs.true135
                                        #   in Loop: Header=BB20_17 Depth=1
	movl	60(%rcx), %edx
	movl	60(%rax), %esi
	cmpq	%rsi, %rdx
	jne	.LBB20_21
# BB#20:                                # %if.then140
                                        #   in Loop: Header=BB20_17 Depth=1
	decq	%rdx
	movl	%edx, 60(%rcx)
	.align	16, 0x90
.LBB20_21:                              # %for.inc144
                                        #   in Loop: Header=BB20_17 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB20_17
# BB#22:                                # %for.cond126.for.end146_crit_edge
	movl	60(%rax), %edx
.LBB20_23:                              # %for.end146
	decq	%rdx
	movl	%edx, 60(%rax)
	xorq	%rcx, %rcx
	movb	%cl, 57(%rax)
	jmp	.LBB20_24
.LBB20_34:                              # %if.end151
	movq	cfun(%rip), %rax
	movq	320(%rax), %rcx
	jmp	.LBB20_35
.LBB20_40:                              # %for.inc170
                                        #   in Loop: Header=BB20_35 Depth=1
	movq	(%rcx), %rcx
.LBB20_35:                              # %if.end151
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB20_24
# BB#36:                                # %for.body155
                                        #   in Loop: Header=BB20_35 Depth=1
	movzbl	56(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB20_40
# BB#37:                                # %land.lhs.true159
                                        #   in Loop: Header=BB20_35 Depth=1
	movl	60(%rcx), %edx
	movl	328(%rax), %esi
	cmpq	%rsi, %rdx
	jne	.LBB20_40
# BB#38:                                # %land.lhs.true164
                                        #   in Loop: Header=BB20_35 Depth=1
	movl	64(%rcx), %esi
	testq	%rsi, %rsi
	jne	.LBB20_40
# BB#39:                                # %if.then166
                                        #   in Loop: Header=BB20_35 Depth=1
	decq	%rdx
	movl	%edx, 60(%rcx)
	jmp	.LBB20_40
.LBB20_24:                              # %for.end172
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp149:
	.size	preserve_temp_slots, .Ltmp149-preserve_temp_slots
	.cfi_endproc

	.globl	preserve_rtl_expr_result
	.align	16, 0x90
	.type	preserve_rtl_expr_result,@function
preserve_rtl_expr_result:               # @preserve_rtl_expr_result
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp151:
	.cfi_def_cfa_offset 16
	testq	%rdi, %rdi
	je	.LBB21_11
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB21_11
# BB#2:                                 # %lor.lhs.false2
	movq	8(%rdi), %rdi
	movzbl	(%rdi), %eax
	cmpq	$133, %rax
	jg	.LBB21_5
# BB#3:                                 # %lor.lhs.false2
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB21_7
# BB#4:                                 # %lor.lhs.false2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB21_11
	jmp	.LBB21_7
.LBB21_5:                               # %lor.lhs.false2
	cmpq	$134, %rax
	je	.LBB21_11
# BB#6:                                 # %lor.lhs.false2
	cmpq	$140, %rax
	je	.LBB21_11
.LBB21_7:                               # %if.end
	callq	find_temp_slot_from_address
	testq	%rax, %rax
	je	.LBB21_11
# BB#8:                                 # %if.then59
	movq	cfun(%rip), %rdx
	movslq	60(%rax), %rcx
	movslq	328(%rdx), %rdx
	cmpq	%rdx, %rcx
	jle	.LBB21_10
# BB#9:                                 # %if.then59
	movq	%rdx, %rcx
.LBB21_10:                              # %if.then59
	movl	%ecx, 60(%rax)
	movq	$0, 48(%rax)
.LBB21_11:                              # %return
	popq	%rax
	retq
.Ltmp152:
	.size	preserve_rtl_expr_result, .Ltmp152-preserve_rtl_expr_result
	.cfi_endproc

	.globl	free_temp_slots
	.align	16, 0x90
	.type	free_temp_slots,@function
free_temp_slots:                        # @free_temp_slots
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	320(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB22_8
# BB#1:                                 # %for.body.lr.ph
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB22_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	56(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB22_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB22_2 Depth=1
	movl	60(%rcx), %esi
	movl	328(%rax), %edi
	cmpq	%rdi, %rsi
	jne	.LBB22_7
# BB#4:                                 # %land.lhs.true3
                                        #   in Loop: Header=BB22_2 Depth=1
	movl	64(%rcx), %esi
	testq	%rsi, %rsi
	jne	.LBB22_7
# BB#5:                                 # %land.lhs.true5
                                        #   in Loop: Header=BB22_2 Depth=1
	cmpq	$0, 48(%rcx)
	jne	.LBB22_7
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB22_2 Depth=1
	movb	%dl, 56(%rcx)
	.align	16, 0x90
.LBB22_7:                               # %for.inc
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB22_2
.LBB22_8:                               # %for.end
	jmp	combine_temp_slots  # TAILCALL
.Ltmp153:
	.size	free_temp_slots, .Ltmp153-free_temp_slots
	.cfi_endproc

	.globl	free_temps_for_rtl_expr
	.align	16, 0x90
	.type	free_temps_for_rtl_expr,@function
free_temps_for_rtl_expr:                # @free_temps_for_rtl_expr
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	320(%rax), %rax
	testq	%rax, %rax
	je	.LBB23_7
# BB#1:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB23_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	48(%rax), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB23_6
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB23_2 Depth=1
	movq	cfun(%rip), %rdx
	movslq	328(%rdx), %rdx
	movslq	60(%rax), %rsi
	cmpq	%rsi, %rdx
	jle	.LBB23_4
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB23_2 Depth=1
	movq	$0, 48(%rax)
	jmp	.LBB23_6
	.align	16, 0x90
.LBB23_4:                               # %if.then2
                                        #   in Loop: Header=BB23_2 Depth=1
	movb	%cl, 56(%rax)
.LBB23_6:                               # %for.inc
                                        #   in Loop: Header=BB23_2 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB23_2
.LBB23_7:                               # %for.end
	jmp	combine_temp_slots  # TAILCALL
.Ltmp154:
	.size	free_temps_for_rtl_expr, .Ltmp154-free_temps_for_rtl_expr
	.cfi_endproc

	.globl	mark_all_temps_used
	.align	16, 0x90
	.type	mark_all_temps_used,@function
mark_all_temps_used:                    # @mark_all_temps_used
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rcx
	movq	320(%rcx), %rax
	testq	%rax, %rax
	je	.LBB24_5
# BB#1:                                 # %for.body.lr.ph
	movl	328(%rcx), %r8d
	movq	$1, %rsi
	.align	16, 0x90
.LBB24_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	%esi, 64(%rax)
	movb	%sil, 56(%rax)
	movslq	60(%rax), %rdi
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB24_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	%r8, %rdi
.LBB24_4:                               # %for.body
                                        #   in Loop: Header=BB24_2 Depth=1
	movl	%edi, 60(%rax)
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB24_2
.LBB24_5:                               # %for.end
	retq
.Ltmp155:
	.size	mark_all_temps_used, .Ltmp155-mark_all_temps_used
	.cfi_endproc

	.globl	push_temp_slots
	.align	16, 0x90
	.type	push_temp_slots,@function
push_temp_slots:                        # @push_temp_slots
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movl	328(%rax), %ecx
	incq	%rcx
	movl	%ecx, 328(%rax)
	retq
.Ltmp156:
	.size	push_temp_slots, .Ltmp156-push_temp_slots
	.cfi_endproc

	.globl	pop_temp_slots
	.align	16, 0x90
	.type	pop_temp_slots,@function
pop_temp_slots:                         # @pop_temp_slots
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp158:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	320(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB26_7
# BB#1:                                 # %for.body.lr.ph
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB26_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	56(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB26_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB26_2 Depth=1
	movl	60(%rcx), %esi
	movl	328(%rax), %edi
	cmpq	%rdi, %rsi
	jne	.LBB26_6
# BB#4:                                 # %land.lhs.true3
                                        #   in Loop: Header=BB26_2 Depth=1
	cmpq	$0, 48(%rcx)
	jne	.LBB26_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB26_2 Depth=1
	movb	%dl, 56(%rcx)
	.align	16, 0x90
.LBB26_6:                               # %for.inc
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB26_2
.LBB26_7:                               # %for.end
	callq	combine_temp_slots
	movq	cfun(%rip), %rax
	movl	328(%rax), %ecx
	decq	%rcx
	movl	%ecx, 328(%rax)
	popq	%rax
	retq
.Ltmp159:
	.size	pop_temp_slots, .Ltmp159-pop_temp_slots
	.cfi_endproc

	.globl	init_temp_slots
	.align	16, 0x90
	.type	init_temp_slots,@function
init_temp_slots:                        # @init_temp_slots
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	$0, 320(%rax)
	movq	cfun(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 328(%rax)
	movl	%ecx, 332(%rax)
	movl	%ecx, 336(%rax)
	retq
.Ltmp160:
	.size	init_temp_slots, .Ltmp160-init_temp_slots
	.cfi_endproc

	.globl	put_var_into_stack
	.align	16, 0x90
	.type	put_var_into_stack,@function
put_var_into_stack:                     # @put_var_into_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp167:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp168:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp169:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp170:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp171:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp172:
	.cfi_def_cfa_offset 144
.Ltmp173:
	.cfi_offset %rbx, -48
.Ltmp174:
	.cfi_offset %r12, -40
.Ltmp175:
	.cfi_offset %r13, -32
.Ltmp176:
	.cfi_offset %r14, -24
.Ltmp177:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %r14d
	movq	%r14, %rax
	andq	$255, %rax
	xorq	%rdx, %rdx
	cmpq	$118, %rax
	je	.LBB28_2
# BB#1:                                 # %land.rhs
	movq	%r14, %rdx
	andq	$2048, %rdx             # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %rdx
.LBB28_2:                               # %land.end
	andq	$65536, %r14            # imm = 0x10000
	movq	$16, %rcx
	shrq	%cl, %r14
	testq	%r14, %r14
	jne	.LBB28_3
# BB#4:                                 # %land.end
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	cmpq	$118, %rax
	je	.LBB28_8
# BB#5:                                 # %land.rhs14
	cmpq	$0, 104(%rbx)
	je	.LBB28_6
# BB#7:                                 # %land.rhs14
	movabsq	$1, %r14
	jmp	.LBB28_8
.LBB28_3:
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	jmp	.LBB28_8
.LBB28_6:
	movabsq	$0, %r14
.LBB28_8:                               # %lor.end
	movq	%rbx, %rdi
	callq	decl_function_context
	movzbl	16(%rbx), %edx
	cmpq	$118, %rdx
	jne	.LBB28_9
# BB#11:                                # %cond.end39
	movq	48(%rbx), %r10
	testq	%r10, %r10
	je	.LBB28_61
# BB#12:                                # %cond.true47
	movq	8(%rbx), %rcx
	movl	60(%rcx), %r8d
	movq	$9, %rcx
	shrq	%cl, %r8
	andq	$127, %r8
	jmp	.LBB28_13
.LBB28_9:                               # %cond.false
	movq	144(%rbx), %r10
	testq	%r10, %r10
	je	.LBB28_61
# BB#10:                                # %cond.false53
	movzbl	48(%rbx), %r8d
.LBB28_13:                              # %cond.end58
	movl	(%r10), %esi
	movq	current_function_decl(%rip), %rdi
	movq	$16, %rcx
	movq	%rsi, %r11
	shrq	%cl, %r11
	xorq	%r9, %r9
	cmpq	%rdi, %rax
	je	.LBB28_20
# BB#14:                                # %cond.end58
	movq	inline_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB28_20
# BB#15:                                # %for.cond.preheader
	movq	outer_function_chain(%rip), %rcx
	jmp	.LBB28_17
	.align	16, 0x90
.LBB28_16:                              # %for.cond
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	56(%rcx), %rcx
.LBB28_17:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB28_20
# BB#18:                                # %for.body
                                        #   in Loop: Header=BB28_17 Depth=1
	movq	48(%rcx), %rdi
	cmpq	%rax, %rdi
	jne	.LBB28_16
# BB#19:
	movq	%rcx, %r9
.LBB28_20:                              # %if.end71
	andq	$255, %r11
	cmpq	$118, %rdx
	je	.LBB28_26
# BB#21:                                # %land.lhs.true77
	testq	$512, 48(%rbx)          # imm = 0x200
	je	.LBB28_26
# BB#22:                                # %land.lhs.true77
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$66, %rsi
	jne	.LBB28_26
# BB#23:                                # %land.lhs.true88
	movq	8(%r10), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB28_26
# BB#24:                                # %land.lhs.true93
	movl	8(%rax), %ecx
	cmpq	$58, %rcx
	jb	.LBB28_26
# BB#25:                                # %if.then100
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rdx, %r8
	movq	%rdx, %r11
	movq	%rax, %r10
.LBB28_26:                              # %if.end107
	testq	%r9, %r9
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB28_28
# BB#27:                                # %if.end107
	movq	%rdx, %rax
.LBB28_28:                              # %if.end107
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movslq	optimize(%rip), %rsi
	testq	%rsi, %rsi
	movq	%rcx, %rsi
	jle	.LBB28_30
# BB#29:                                # %if.end107
	movq	%rdx, %rsi
.LBB28_30:                              # %if.end107
	andq	%rsi, %rax
	cmpq	%r11, %r8
	jne	.LBB28_32
# BB#31:                                # %if.end107
	movq	%rdx, %rcx
.LBB28_32:                              # %if.end107
	movq	%r9, %r12
	movq	%r10, %r9
	andq	%rcx, %rax
	jne	.LBB28_36
# BB#33:                                # %land.lhs.true116
	movzbl	(%r9), %ecx
	cmpq	$66, %rcx
	jne	.LBB28_36
# BB#34:                                # %land.lhs.true120
	movq	8(%r9), %rcx
	movzbl	(%rcx), %edx
	cmpq	$70, %rdx
	jne	.LBB28_36
# BB#35:                                # %if.then127
	movq	8(%rcx), %r9
.LBB28_36:                              # %if.end134
	movzbl	(%r9), %ecx
	cmpq	$65, %rcx
	jne	.LBB28_37
# BB#40:                                # %if.then149
	movq	%r11, 56(%rsp)          # 8-byte Spill
	movq	8(%r9), %rax
	movzbl	2(%rax), %r15d
	movq	%r15, 48(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%r9, 88(%rsp)           # 8-byte Spill
	callq	type_for_mode
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	movq	%r15, %r8
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	%r13, %r9
	callq	put_reg_into_stack
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	movq	%r12, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %rcx
	movq	%r15, %r8
	movq	%r13, %r9
	callq	put_reg_into_stack
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	$66, %rax
	movw	%ax, (%rdi)
	movq	$0, 16(%rdi)
	movq	72(%rsp), %rsi          # 8-byte Reload
	movzbl	16(%rsi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB28_42
# BB#41:                                # %if.then174
	movq	$0, 144(%rsi)
.LBB28_42:                              # %if.end177
	movq	$1, %rdx
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	set_mem_attributes
	movq	%r15, %rcx
	movzbl	16(%rbx), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB28_44
# BB#43:                                # %if.then187
	movq	%rcx, 144(%rbx)
.LBB28_44:                              # %if.end190
	movq	8(%rcx), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 8(%rcx)
	movzbl	(%rdi), %eax
	cmpq	$75, %rax
	movq	56(%rsp), %r13          # 8-byte Reload
	jne	.LBB28_46
# BB#45:                                # %if.then207
	movq	%rcx, %r15
	callq	copy_rtx
	movq	%r15, %rcx
	movq	%rax, 8(%rcx)
.LBB28_46:                              # %if.end215
	testq	%r14, %r14
	je	.LBB28_61
# BB#47:                                # %if.then217
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	8(%rbx), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB28_49
# BB#48:                                # %cond.true.i
	movl	16(%rax), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
.LBB28_49:                              # %cond.end.i
	testq	%r12, %r12
	movq	%r12, %r14
	je	.LBB28_51
# BB#50:                                # %if.then.i
	movq	$24, %rdi
	callq	ggc_alloc
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax)
	movl	%r13d, 8(%rax)
	movl	%ebx, 12(%rax)
	movq	344(%r14), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, 344(%r14)
	jmp	.LBB28_52
.LBB28_37:                              # %if.end134
	cmpq	$61, %rcx
	movq	72(%rsp), %rsi          # 8-byte Reload
	jne	.LBB28_61
# BB#38:                                # %if.then138
	testq	$1, %rax
	movq	%r12, %rdi
	je	.LBB28_39
# BB#62:                                # %if.then140
	movq	%r9, %rdi
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_mem_addressof  # TAILCALL
.LBB28_39:                              # %if.else
	movq	8(%rsi), %rdx
	movq	%r14, 8(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, (%rsp)
	movq	%r9, %rsi
	movq	%r11, %rcx
	movq	80(%rsp), %r9           # 8-byte Reload
	callq	put_reg_into_stack
	jmp	.LBB28_61
.LBB28_51:                              # %if.else.i
	xorq	%r8, %r8
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%rdi, %rcx
	callq	fixup_var_refs
.LBB28_52:                              # %schedule_fixup_var_refs.exit
	movq	40(%rsp), %r12          # 8-byte Reload
	xorq	%rbx, %rbx
	testq	%r12, %r12
	movq	48(%rsp), %r15          # 8-byte Reload
	je	.LBB28_54
# BB#53:                                # %cond.true.i309
	movl	16(%r12), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
.LBB28_54:                              # %cond.end.i312
	testq	%r14, %r14
	je	.LBB28_56
# BB#55:                                # %if.then.i319
	movq	$24, %rdi
	callq	ggc_alloc
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax)
	movl	%r15d, 8(%rax)
	movl	%ebx, 12(%rax)
	movq	344(%r14), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, 344(%r14)
	jmp	.LBB28_57
.LBB28_56:                              # %if.else.i320
	xorq	%r8, %r8
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rdi, %rcx
	callq	fixup_var_refs
.LBB28_57:                              # %schedule_fixup_var_refs.exit321
	movq	%r14, %rax
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB28_59
# BB#58:                                # %cond.true.i327
	movl	16(%r12), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
.LBB28_59:                              # %cond.end.i330
	testq	%rax, %rax
	je	.LBB28_63
# BB#60:                                # %if.then.i337
	movq	$24, %rdi
	movq	%rax, %r14
	callq	ggc_alloc
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax)
	movl	%r15d, 8(%rax)
	movl	%ebx, 12(%rax)
	movq	344(%r14), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, 344(%r14)
.LBB28_61:                              # %if.end223
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_63:                              # %if.else.i338
	xorq	%r8, %r8
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rdi, %rcx
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fixup_var_refs  # TAILCALL
.Ltmp178:
	.size	put_var_into_stack, .Ltmp178-put_var_into_stack
	.cfi_endproc

	.globl	gen_mem_addressof
	.align	16, 0x90
	.type	gen_mem_addressof,@function
gen_mem_addressof:                      # @gen_mem_addressof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp185:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp187:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp188:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp189:
	.cfi_def_cfa_offset 48
.Ltmp190:
	.cfi_offset %rbx, -48
.Ltmp191:
	.cfi_offset %r12, -40
.Ltmp192:
	.cfi_offset %r13, -32
.Ltmp193:
	.cfi_offset %r14, -24
.Ltmp194:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %ebx
	movq	%rsi, %r14
	movq	%rdi, %r13
	movq	$25, %rcx
	movzbl	2(%r13), %edi
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	callq	gen_reg_rtx
	movl	8(%r13), %ecx
	movq	$70, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eit
	movq	%rax, %r12
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB29_2
# BB#1:                                 # %cond.true
	movq	%r14, %rdi
	callq	get_alias_set
	movq	%rax, %r15
.LBB29_2:                               # %cond.end
	movq	8(%r12), %rax
	movl	(%r13), %ecx
	movl	(%rax), %edx
	andq	$134217728, %rcx        # imm = 0x8000000
	movabsq	$4160749567, %rsi       # imm = 0xF7FFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movq	8(%r12), %rax
	movl	(%r13), %ecx
	movl	(%rax), %edx
	andq	$67108864, %rcx         # imm = 0x4000000
	movabsq	$4227858431, %rsi       # imm = 0xFBFFFFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, (%rax)
	movl	(%r13), %esi
	movq	%rsi, %rax
	andq	$-65536, %rax           # imm = 0xFFFFFFFFFFFF0000
	orq	$66, %rax
	testq	%r14, %r14
	movl	%eax, (%r13)
	movq	$0, 16(%r13)
	movq	%r12, 8(%r13)
	je	.LBB29_19
# BB#3:                                 # %if.then
	movzbl	16(%r14), %esi
	movzbl	tree_code_type(%rsi), %ecx
	movq	8(%r14), %r12
	cmpq	$100, %rcx
	jne	.LBB29_5
# BB#4:                                 # %cond.true38
	movzbl	48(%r14), %edx
	jmp	.LBB29_6
.LBB29_19:                              # %if.else
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB29_20
.LBB29_5:                               # %cond.false42
	movl	60(%r12), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
.LBB29_6:                               # %cond.end50
	cmpq	$118, %rsi
	jne	.LBB29_8
# BB#7:                                 # %cond.true58
	leaq	48(%r14), %rcx
	jmp	.LBB29_9
.LBB29_8:                               # %cond.false60
	leaq	144(%r14), %rcx
.LBB29_9:                               # %cond.end79
	movq	(%rcx), %rbx
	movq	$16, %rcx
	shlq	%cl, %rdx
	movabsq	$4278190146, %rcx       # imm = 0xFF000042
	andq	%rcx, %rax
	orq	%rdx, %rax
	movl	%eax, (%r13)
	movzbl	16(%r14), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB29_12
# BB#10:                                # %cond.end79
	cmpq	%r13, %rbx
	jne	.LBB29_12
# BB#11:                                # %if.then97
	movq	$0, 144(%r14)
.LBB29_12:                              # %if.end
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	set_mem_attributes
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	set_mem_alias_set
	movl	16(%r14), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %edx
	cmpq	$100, %rdx
	jne	.LBB29_15
# BB#13:                                # %if.end
	cmpq	%r13, %rbx
	jne	.LBB29_15
# BB#14:                                # %if.then112
	movq	%r13, 144(%r14)
.LBB29_15:                              # %if.end115
	testq	$65536, %rcx            # imm = 0x10000
	jne	.LBB29_18
# BB#16:                                # %lor.lhs.false
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB29_21
# BB#17:                                # %land.lhs.true130
	cmpq	$0, 104(%r14)
	je	.LBB29_21
.LBB29_18:                              # %if.then134
	movl	16(%r12), %edx
	movzbl	2(%r13), %esi
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
.LBB29_20:                              # %if.end146
	xorq	%r8, %r8
	movq	%r13, %rdi
	movq	%r13, %rcx
	callq	fixup_var_refs
.LBB29_21:                              # %if.end146
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp195:
	.size	gen_mem_addressof, .Ltmp195-gen_mem_addressof
	.cfi_endproc

	.align	16, 0x90
	.type	put_reg_into_stack,@function
put_reg_into_stack:                     # @put_reg_into_stack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp202:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp203:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp204:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp205:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp206:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp207:
	.cfi_def_cfa_offset 80
.Ltmp208:
	.cfi_offset %rbx, -48
.Ltmp209:
	.cfi_offset %r12, -40
.Ltmp210:
	.cfi_offset %r13, -32
.Ltmp211:
	.cfi_offset %r14, -24
.Ltmp212:
	.cfi_offset %r15, -16
	movq	80(%rsp), %rax
	movq	%r9, %r15
	movq	%r8, %r12
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	jne	.LBB30_2
# BB#1:                                 # %entry
	movq	cfun(%rip), %rbx
.LBB30_2:                               # %entry
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB30_4
# BB#3:                                 # %if.then
	movl	8(%r14), %eax
.LBB30_4:                               # %if.end
	movl	304(%rbx), %ecx
	andq	%rsi, %rax
	cmpq	%rcx, %rax
	jae	.LBB30_6
# BB#5:                                 # %if.end4
	movq	312(%rbx), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	jne	.LBB30_7
.LBB30_6:                               # %if.then6
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rsi, %r13
	movzbl	mode_size(%rax), %esi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rcx
	callq	assign_stack_local_1
	movq	%r13, %rsi
.LBB30_7:                               # %if.end9
	movl	(%r14), %edx
	andq	$-16777216, %rdx        # imm = 0xFFFFFFFFFF000000
	movq	$16, %rcx
	shlq	%cl, %r12
	andq	$16711680, %r12         # imm = 0xFF0000
	orq	%rdx, %r12
	orq	$66, %r12
	movl	%r12d, (%r14)
	movq	8(%rax), %rcx
	movq	%rcx, 8(%r14)
	movq	$0, 16(%r14)
	andq	$1, %r15
	shlq	$27, %r15
	andq	$-134283198, %r12       # imm = 0xFFFFFFFFF7FF0042
	orq	%r15, %r12
	movl	%r12d, (%r14)
	movq	24(%rsp), %r15          # 8-byte Reload
	testq	%r15, %r15
	je	.LBB30_13
# BB#8:                                 # %do.body
	movzbl	16(%r15), %ecx
	addq	$-18, %rcx
	andq	%rsi, %rcx
	cmpq	$5, %rcx
	jb	.LBB30_10
# BB#9:                                 # %lor.lhs.false58
	movzbl	3(%rax), %eax
	testq	$16, %rax
	je	.LBB30_11
.LBB30_10:                              # %if.then62
	movq	%rsi, %rbx
	andq	$1878982722, %r12       # imm = 0x6FFF0042
	orq	$268435456, %r12        # imm = 0x10000000
	jmp	.LBB30_12
.LBB30_11:                              # %if.else
	movq	%rsi, %rbx
	andq	$1878982722, %r12       # imm = 0x6FFF0042
	orq	$-2147483648, %r12      # imm = 0xFFFFFFFF80000000
.LBB30_12:                              # %do.end
	movl	%r12d, (%r14)
	movq	%r15, %rdi
	callq	get_alias_set
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	%rbx, %rsi
.LBB30_13:                              # %if.end75
	movq	88(%rsp), %rax
	testq	%rsi, %rax
	je	.LBB30_18
# BB#14:                                # %if.then77
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB30_16
# BB#15:                                # %cond.true.i
	movl	16(%r15), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
.LBB30_16:                              # %cond.end.i
	movq	16(%rsp), %r15          # 8-byte Reload
	testq	%r15, %r15
	je	.LBB30_19
# BB#17:                                # %if.then.i
	movq	$24, %rdi
	callq	ggc_alloc
	movq	%r14, (%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%ecx, 8(%rax)
	movl	%ebx, 12(%rax)
	movq	344(%r15), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, 344(%r15)
.LBB30_18:                              # %if.end78
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_19:                              # %if.else.i
	movq	%r14, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	96(%rsp), %r8
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fixup_var_refs  # TAILCALL
.Ltmp213:
	.size	put_reg_into_stack, .Ltmp213-put_reg_into_stack
	.cfi_endproc

	.globl	flush_addressof
	.align	16, 0x90
	.type	flush_addressof,@function
flush_addressof:                        # @flush_addressof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp216:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp217:
	.cfi_def_cfa_offset 48
.Ltmp218:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	andq	$254, %rax
	cmpq	$34, %rax
	jne	.LBB31_17
# BB#1:                                 # %land.lhs.true
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB31_3
# BB#2:                                 # %cond.end
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	testq	%rax, %rax
	je	.LBB31_17
.LBB31_3:                               # %cond.end22
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB31_17
# BB#4:                                 # %cond.end37
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$70, %rax
	jne	.LBB31_17
# BB#5:                                 # %cond.end52
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB31_17
# BB#6:                                 # %cond.end72
	movq	8(%rdi), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB31_18
# BB#7:                                 # %if.end.i
	movq	24(%rdi), %r10
	testq	%r10, %r10
	je	.LBB31_8
# BB#9:                                 # %if.then3.i
	movl	16(%r10), %r8d
	movq	8(%r10), %rdx
	movq	%r8, %rbx
	andq	$255, %rbx
	xorq	%r9, %r9
	cmpq	$118, %rbx
	je	.LBB31_11
# BB#10:                                # %land.rhs.i
	movq	%r8, %r9
	andq	$2048, %r9              # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %r9
.LBB31_11:                              # %land.end.i
	testq	$65536, %r8             # imm = 0x10000
	movq	$1, %r8
	jne	.LBB31_16
# BB#12:                                # %lor.rhs.i
	movzbl	tree_code_type(%rbx), %ecx
	xorq	%r8, %r8
	cmpq	$100, %rcx
	jne	.LBB31_16
# BB#13:                                # %land.rhs25.i
	cmpq	$0, 104(%r10)
	je	.LBB31_14
# BB#15:                                # %land.rhs25.i
	movabsq	$1, %r8
	jmp	.LBB31_16
.LBB31_8:
	xorq	%r9, %r9
	movq	$1, %r8
	xorq	%rdx, %rdx
.LBB31_16:                              # %put_addressof_into_stack.exit
	movl	16(%rdi), %edi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r8, 8(%rsp)
	movq	%rdi, (%rsp)
	xorq	%rdi, %rdi
	movq	$0, 16(%rsp)
	movq	%rax, %rcx
	movq	%rax, %r8
	callq	put_reg_into_stack
.LBB31_17:                              # %if.end
	addq	$32, %rsp
	popq	%rbx
	retq
.LBB31_14:
	movabsq	$0, %r8
	jmp	.LBB31_16
.LBB31_18:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.put_addressof_into_stack, %rcx
	movq	$2997, %rsi             # imm = 0xBB5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp219:
	.size	flush_addressof, .Ltmp219-flush_addressof
	.cfi_endproc

	.globl	purge_addressof
	.align	16, 0x90
	.type	purge_addressof,@function
purge_addressof:                        # @purge_addressof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp226:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp227:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp228:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp229:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp230:
	.cfi_def_cfa_offset 48
	subq	$176, %rsp
.Ltmp231:
	.cfi_def_cfa_offset 224
.Ltmp232:
	.cfi_offset %rbx, -48
.Ltmp233:
	.cfi_offset %r12, -40
.Ltmp234:
	.cfi_offset %r13, -32
.Ltmp235:
	.cfi_offset %r14, -24
.Ltmp236:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movabsq	$insns_for_mem_newfunc, %rsi
	movabsq	$insns_for_mem_hash, %rdx
	movabsq	$insns_for_mem_comp, %rcx
	leaq	16(%rsp), %rbx
	movq	%rbx, %rdi
	callq	hash_table_init
	xorq	%rax, %rax
	testq	%r13, %r13
	movq	%rbx, 144(%rsp)
	movl	%eax, 160(%rsp)
	je	.LBB32_25
# BB#1:
	movabsq	$insns_for_mem_walk, %r14
	leaq	168(%rsp), %r15
	leaq	144(%rsp), %rbx
	.align	16, 0x90
.LBB32_2:                               # %for.body5.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_3 Depth 2
	movq	%r13, 168(%rsp)
	movq	%r13, %rax
	.align	16, 0x90
.LBB32_3:                               # %for.body5.i
                                        #   Parent Loop BB32_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB32_5
# BB#4:                                 # %if.then.i
                                        #   in Loop: Header=BB32_3 Depth=2
	movq	%rax, 152(%rsp)
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
	movq	168(%rsp), %rax
.LBB32_5:                               # %for.inc.i
                                        #   in Loop: Header=BB32_3 Depth=2
	movq	24(%rax), %rax
	testq	%rax, %rax
	movq	%rax, 168(%rsp)
	jne	.LBB32_3
# BB#6:                                 # %for.inc10.i
                                        #   in Loop: Header=BB32_2 Depth=1
	movl	160(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	movl	%eax, 160(%rsp)
	jl	.LBB32_2
# BB#7:                                 # %for.cond.preheader
	testq	%r13, %r13
	je	.LBB32_24
# BB#8:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	leaq	16(%rsp), %r15
	leaq	8(%rsp), %r12
	.align	16, 0x90
.LBB32_9:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_17 Depth 2
	movzbl	(%r13), %eax
	addq	$-32, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB32_23
# BB#10:                                # %if.then
                                        #   in Loop: Header=BB32_9 Depth=1
	movq	32(%r13), %rdi
	leaq	32(%r13), %rbx
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rdx
	jle	.LBB32_12
# BB#11:                                # %if.then
                                        #   in Loop: Header=BB32_9 Depth=1
	movabsq	$1, %rdx
.LBB32_12:                              # %if.then
                                        #   in Loop: Header=BB32_9 Depth=1
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r15, %r8
	callq	purge_addressof_1
	testq	%rax, %rax
	je	.LBB32_13
# BB#15:                                # %if.end
                                        #   in Loop: Header=BB32_9 Depth=1
	leaq	56(%r13), %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r15, %r8
	callq	purge_addressof_1
	testq	%rax, %rax
	jne	.LBB32_23
# BB#16:                                # %for.cond22.preheader
                                        #   in Loop: Header=BB32_9 Depth=1
	movq	(%rbx), %rax
	jmp	.LBB32_17
	.align	16, 0x90
.LBB32_22:                              # %for.inc
                                        #   in Loop: Header=BB32_17 Depth=2
	movq	8(%rsp), %rax
	movq	16(%rax), %rax
.LBB32_17:                              # %for.cond22.preheader
                                        #   Parent Loop BB32_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	movq	%rax, 8(%rsp)
	je	.LBB32_23
# BB#18:                                # %for.body24
                                        #   in Loop: Header=BB32_17 Depth=2
	movl	(%rax), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$393216, %rax           # imm = 0x60000
	je	.LBB32_19
# BB#20:                                # %if.end30
                                        #   in Loop: Header=BB32_17 Depth=2
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movabsq	$is_addressof, %rsi
	callq	for_each_rtx
	testq	%r14, %rax
	je	.LBB32_22
# BB#21:                                # %if.then33
                                        #   in Loop: Header=BB32_17 Depth=2
	movq	8(%rsp), %rsi
	movq	%r13, %rdi
	callq	remove_note
	jmp	.LBB32_22
	.align	16, 0x90
.LBB32_23:                              # %for.inc40
                                        #   in Loop: Header=BB32_9 Depth=1
	movq	24(%r13), %r13
	testq	%r13, %r13
	jne	.LBB32_9
	jmp	.LBB32_24
.LBB32_25:                              # %for.cond.preheader.thread
	movq	$0, 168(%rsp)
	movq	$2, %rax
	movl	%eax, 160(%rsp)
.LBB32_24:                              # %for.end44
	leaq	16(%rsp), %rdi
	callq	hash_table_free
	movq	$0, purge_bitfield_addressof_replacements(%rip)
	movq	$0, purge_addressof_replacements(%rip)
	callq	get_insns
	movq	%rax, %rdi
	callq	unshare_all_rtl_again
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB32_13:                              # %if.then13
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.purge_addressof, %rcx
	movq	$3455, %rsi             # imm = 0xD7F
	jmp	.LBB32_14
.LBB32_19:                              # %if.then29
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.purge_addressof, %rcx
	movq	$3470, %rsi             # imm = 0xD8E
.LBB32_14:                              # %if.then13
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp237:
	.size	purge_addressof, .Ltmp237-purge_addressof
	.cfi_endproc

	.align	16, 0x90
	.type	insns_for_mem_newfunc,@function
insns_for_mem_newfunc:                  # @insns_for_mem_newfunc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp239:
	.cfi_def_cfa_offset 16
	movq	%rsi, %rcx
	movq	%rdi, %rax
	testq	%rax, %rax
	jne	.LBB33_2
# BB#1:                                 # %if.end
	movq	$32, %rsi
	movq	%rcx, %rdi
	callq	hash_allocate
	movq	$0, 24(%rax)
.LBB33_2:                               # %return
	popq	%rdx
	retq
.Ltmp240:
	.size	insns_for_mem_newfunc, .Ltmp240-insns_for_mem_newfunc
	.cfi_endproc

	.align	16, 0x90
	.type	insns_for_mem_hash,@function
insns_for_mem_hash:                     # @insns_for_mem_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	retq
.Ltmp241:
	.size	insns_for_mem_hash, .Ltmp241-insns_for_mem_hash
	.cfi_endproc

	.align	16, 0x90
	.type	insns_for_mem_comp,@function
insns_for_mem_comp:                     # @insns_for_mem_comp
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rsi, %rdi
	jne	.LBB35_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB35_1:
	movabsq	$0, %rax
	retq
.Ltmp242:
	.size	insns_for_mem_comp, .Ltmp242-insns_for_mem_comp
	.cfi_endproc

	.align	16, 0x90
	.type	purge_addressof_1,@function
purge_addressof_1:                      # @purge_addressof_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp249:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp250:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp251:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp252:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp253:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp254:
	.cfi_def_cfa_offset 240
.Ltmp255:
	.cfi_offset %rbx, -48
.Ltmp256:
	.cfi_offset %r12, -40
.Ltmp257:
	.cfi_offset %r13, -32
.Ltmp258:
	.cfi_offset %r14, -24
.Ltmp259:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rdx
	je	.LBB36_1
# BB#7:                                 # %restart.us
	movq	(%rbx), %r10
	testq	%r10, %r10
	movq	$1, %r12
	je	.LBB36_122
# BB#8:                                 # %if.end.us
	movzbl	(%r10), %r12d
	cmpq	$47, %r12
	je	.LBB36_9
# BB#10:                                # %if.end.us
	cmpq	$70, %r12
	jne	.LBB36_16
# BB#11:
	movq	%r8, 64(%rsp)           # 8-byte Spill
.LBB36_12:                              # %if.then13
	movq	8(%r10), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB36_13
# BB#23:                                # %if.end.i
	movq	24(%r10), %rdi
	xorq	%r9, %r9
	testq	%rdi, %rdi
	movq	$1, %r8
	movabsq	$0, %rdx
	je	.LBB36_32
# BB#24:                                # %if.then3.i
	movl	16(%rdi), %r8d
	movq	8(%rdi), %rdx
	movq	%r8, %rbx
	andq	$255, %rbx
	xorq	%r9, %r9
	cmpq	$118, %rbx
	je	.LBB36_26
# BB#25:                                # %land.rhs.i
	movq	%r8, %r9
	andq	$2048, %r9              # imm = 0x800
	movq	$11, %rcx
	shrq	%cl, %r9
.LBB36_26:                              # %land.end.i
	testq	$65536, %r8             # imm = 0x10000
	movq	$1, %r8
	jne	.LBB36_32
# BB#27:                                # %lor.rhs.i
	movzbl	tree_code_type(%rbx), %ecx
	xorq	%r8, %r8
	cmpq	$100, %rcx
	jne	.LBB36_32
# BB#28:                                # %land.rhs25.i
	cmpq	$0, 104(%rdi)
	je	.LBB36_29
# BB#30:                                # %land.rhs25.i
	movq	%r10, %r13
	movabsq	$1, %r8
	jmp	.LBB36_31
.LBB36_1:
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%r14, %r13
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB36_2:                               # %restart
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %r10
	testq	%r10, %r10
	movq	$1, %r12
	je	.LBB36_122
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB36_2 Depth=1
	movl	(%r10), %esi
	movq	%rsi, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	cmpq	$66, %r12
	movq	%r13, %r14
	jne	.LBB36_4
# BB#39:                                # %land.lhs.true48
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	8(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$70, %rcx
	movq	$66, %r12
	jne	.LBB36_18
# BB#40:                                # %if.then58
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	8(%rax), %r9
	movl	(%r9), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB36_41
# BB#43:                                # %land.lhs.true79
                                        #   in Loop: Header=BB36_2 Depth=1
	testq	$134217728, %rsi        # imm = 0x8000000
	jne	.LBB36_18
# BB#44:                                # %land.lhs.true79
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	%rsi, %rax
	andq	$16711680, %rax         # imm = 0xFF0000
	andq	%r15, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB36_18
# BB#45:                                # %land.lhs.true96
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	%r10, 88(%rsp)          # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rsi
	jne	.LBB36_46
.LBB36_107:                             # %if.else351
                                        #   in Loop: Header=BB36_2 Depth=1
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r14, %r13
	movq	%rbx, %rsi
	movq	%r9, %rdx
	movq	%r9, %r14
	callq	validate_change
	testq	%r15, %rax
	je	.LBB36_108
# BB#109:                               # %if.then354
                                        #   in Loop: Header=BB36_2 Depth=1
	movl	(%r14), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	movq	%r14, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	jne	.LBB36_2
	jmp	.LBB36_110
.LBB36_41:                              # %if.then58
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	%r10, 88(%rsp)          # 8-byte Spill
	cmpq	$66, %rax
	jne	.LBB36_107
# BB#42:                                # %if.then70
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r9, %rdi
	callq	adjust_address_1
	movq	88(%rsp), %r10          # 8-byte Reload
	jmp	.LBB36_18
.LBB36_9:
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
.LBB36_6:                               # %if.then2
	leaq	8(%r10), %rdi
	movq	$1, %rcx
	movq	%r14, %rsi
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r8
	movq	%r10, %rbx
	callq	purge_addressof_1
	movq	%rax, %r13
	addq	$16, %rbx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %r8
	callq	purge_addressof_1
	movq	%rax, %r12
	andq	%r13, %r12
	jmp	.LBB36_122
.LBB36_16:                              # %if.end.us
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
	cmpq	$66, %r12
	jne	.LBB36_18
# BB#17:                                # %land.lhs.true48.us
	movq	$66, %r12
	jmp	.LBB36_18
.LBB36_13:                              # %if.then13
	movq	%r10, 88(%rsp)          # 8-byte Spill
	cmpq	$66, %rcx
	jne	.LBB36_14
# BB#33:                                # %if.end22
	movq	8(%rsi), %rdi
	callq	copy_rtx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%rax, %r13
	callq	validate_change
	testq	%r15, %rax
	movq	$1, %r12
	jne	.LBB36_122
# BB#34:                                # %lor.lhs.false
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	validate_replace_rtx
	movq	%r15, %rcx
	testq	%rcx, %rax
	jne	.LBB36_122
# BB#35:                                # %if.end35
	movq	%rcx, %r15
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	force_operand
	movq	%rax, %rbx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	validate_change
	testq	%r15, %rax
	jne	.LBB36_38
# BB#36:                                # %land.lhs.true
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	validate_replace_rtx
	testq	%r15, %rax
	je	.LBB36_37
.LBB36_38:                              # %if.end42
	callq	gen_sequence
	movq	%rax, %r15
	callq	end_sequence
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	jmp	.LBB36_122
.LBB36_108:
	movq	%r13, %r14
	movq	88(%rsp), %r10          # 8-byte Reload
	jmp	.LBB36_18
.LBB36_46:                              # %if.then105
	testq	%r14, %r14
	movq	88(%rsp), %r10          # 8-byte Reload
	je	.LBB36_47
# BB#67:                                # %if.end238
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	movzbl	mode_bitsize(%rdi,%rdi), %eax
	cmpq	%rax, %rcx
	ja	.LBB36_18
# BB#68:                                # %land.lhs.true253
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%r10, %r13
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	callq	int_mode_for_mode
	movq	%r15, %rcx
	andq	%rcx, %rax
	cmpq	$51, %rax
	jne	.LBB36_70
# BB#69:
	movq	%r13, %r10
	jmp	.LBB36_18
.LBB36_110:                             # %for.cond366.preheader
	movq	purge_addressof_replacements(%rip), %rbx
	movq	8(%rax), %r14
	xorq	%rcx, %rcx
	testq	%rbx, %rbx
	je	.LBB36_116
# BB#111:
	movq	%rdx, %r12
	.align	16, 0x90
.LBB36_112:                             # %for.body369
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r13
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	rtx_equal_p
	movq	16(%rbx), %rcx
	testq	%r15, %rax
	jne	.LBB36_113
# BB#114:                               # %for.inc386
                                        #   in Loop: Header=BB36_112 Depth=1
	movq	16(%rcx), %rbx
	movq	%r13, %rax
	movq	8(%rax), %r14
	testq	%rbx, %rbx
	jne	.LBB36_112
# BB#115:                               # %for.cond366.for.end393_crit_edge
	movq	purge_addressof_replacements(%rip), %rcx
	movq	%r12, %rdx
.LBB36_116:                             # %for.end393
	movq	$3, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	gen_rtx
	movq	%rax, purge_addressof_replacements(%rip)
	movq	$1, %r12
	jmp	.LBB36_122
.LBB36_4:                               # %if.end
	cmpq	$70, %r12
	je	.LBB36_12
# BB#5:                                 # %if.end
	cmpq	$47, %r12
	jne	.LBB36_18
	jmp	.LBB36_6
.LBB36_47:                              # %for.cond.preheader
	movq	%rbx, %r14
	movq	purge_bitfield_addressof_replacements(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB36_48
.LBB36_51:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movq	%r10, %rdi
	movq	%r10, %r12
	callq	rtx_equal_p
	movq	16(%rbx), %rcx
	testq	%r15, %rax
	jne	.LBB36_52
# BB#50:                                # %for.inc
                                        #   in Loop: Header=BB36_51 Depth=1
	movq	16(%rcx), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r10
	jne	.LBB36_51
.LBB36_48:                              # %for.cond129.preheader
	movq	purge_addressof_replacements(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB36_49
.LBB36_53:                              # %for.body132
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r10), %rdi
	movq	%r10, %r12
	movq	8(%rbx), %rsi
	callq	rtx_equal_p
	movq	16(%rbx), %rcx
	testq	%r15, %rax
	jne	.LBB36_54
# BB#65:                                # %for.inc230
                                        #   in Loop: Header=BB36_53 Depth=1
	movq	16(%rcx), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r10
	jne	.LBB36_53
# BB#66:
	xorq	%r12, %r12
	jmp	.LBB36_122
.LBB36_113:                             # %if.then378
	movq	%r12, 8(%rcx)
	movq	$1, %r12
	jmp	.LBB36_122
.LBB36_70:                              # %if.then260
	movq	16(%r14), %r15
	movq	%rcx, %rbx
	callq	start_sequence
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB36_95
# BB#71:                                # %if.then262
	movzbl	2(%r13), %edi
	callq	gen_reg_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	testq	%rbx, %rax
	je	.LBB36_72
# BB#73:                                # %if.end273
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	testq	%r15, %r15
	je	.LBB36_75
# BB#74:                                # %cond.true
	movq	24(%r15), %r12
	jmp	.LBB36_76
.LBB36_52:                              # %if.then115
	movq	8(%rcx), %rax
	movq	%rax, (%r14)
	movq	$1, %r12
	jmp	.LBB36_122
.LBB36_49:
	xorq	%r12, %r12
	jmp	.LBB36_122
.LBB36_29:
	movq	%r10, %r13
	movabsq	$0, %r8
.LBB36_31:                              # %land.rhs25.i
	movq	%r13, %r10
.LBB36_32:                              # %put_addressof_into_stack.exit
	movl	16(%r10), %ebx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 16(%rsp)
	movq	%r8, 8(%rsp)
	xorq	%rdi, %rdi
	movq	%rbx, (%rsp)
	movq	%rax, %rcx
	movq	%rax, %r8
	callq	put_reg_into_stack
	movq	$1, %r12
	jmp	.LBB36_122
.LBB36_54:                              # %if.then141
	movq	8(%rcx), %rsi
	movl	(%rsi), %eax
	movl	(%r12), %edi
	movq	%rax, %rcx
	xorq	%rdi, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	movq	$1, %r12
	je	.LBB36_57
# BB#55:                                # %lor.lhs.false156
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB36_60
# BB#56:                                # %lor.lhs.false156
	cmpq	$63, %rax
	jne	.LBB36_57
# BB#58:                                # %land.lhs.true184
	movl	16(%rsi), %eax
	testq	%rax, %rax
	jne	.LBB36_60
# BB#59:                                # %if.then189
	movq	8(%rsi), %rsi
.LBB36_60:                              # %if.end193
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rdi), %eax
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jbe	.LBB36_63
# BB#61:                                # %land.lhs.true203
	movzbl	2(%rsi), %ecx
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB36_63
# BB#62:                                # %if.then218
	xorq	%rdx, %rdx
	callq	gen_rtx_SUBREG
	movq	%rax, (%r14)
	jmp	.LBB36_122
.LBB36_95:                              # %if.else313
	movq	40(%rsp), %rdi          # 8-byte Reload
	movzbl	2(%rdi), %eax
	movzbl	mode_size(%rax), %eax
	movzbl	2(%r13), %r9d
	movq	%rax, 8(%rsp)
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%r9, (%rsp)
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	extract_bit_field
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	testq	%rbx, %rax
	je	.LBB36_72
# BB#96:                                # %if.end335
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	testq	%r15, %r15
	je	.LBB36_98
# BB#97:                                # %cond.true339
	movq	24(%r15), %r15
	jmp	.LBB36_99
.LBB36_63:                              # %if.else223
	callq	gen_lowpart
	movq	%rax, (%r14)
	jmp	.LBB36_122
.LBB36_72:                              # %if.then272
	callq	end_sequence
	movq	%r13, %r10
.LBB36_18:                              # %give_up
	movq	%r10, 88(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%r12), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	je	.LBB36_121
# BB#19:                                # %for.body415.lr.ph
	movq	rtx_format(,%r12,8), %rdx
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB36_20:                              # %for.body415
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_119 Depth 2
	movzbl	(%rdx), %eax
	cmpq	$101, %rax
	jne	.LBB36_21
# BB#126:                               # %if.then419
                                        #   in Loop: Header=BB36_20 Depth=1
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	8(%r10,%r15,8), %rdi
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	callq	purge_addressof_1
	movq	56(%rsp), %r12          # 8-byte Reload
	andq	%rax, %r12
	jmp	.LBB36_120
	.align	16, 0x90
.LBB36_21:                              # %for.body415
                                        #   in Loop: Header=BB36_20 Depth=1
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB36_22
# BB#117:                               # %for.cond436.preheader
                                        #   in Loop: Header=BB36_20 Depth=1
	movq	8(%r10,%r15,8), %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB36_22
# BB#118:                               #   in Loop: Header=BB36_20 Depth=1
	movq	$8, %r14
	movq	$1, %r13
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	64(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB36_119:                             # %for.body442
                                        #   Parent Loop BB36_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r14, %rdi
	xorq	%rcx, %rcx
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %r8
	callq	purge_addressof_1
	andq	%rax, %r12
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax,%r15,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r13
	addq	$8, %r14
	cmpq	%rax, %rdx
	jl	.LBB36_119
	jmp	.LBB36_120
	.align	16, 0x90
.LBB36_22:                              #   in Loop: Header=BB36_20 Depth=1
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB36_120:                             # %for.inc460
                                        #   in Loop: Header=BB36_20 Depth=1
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r12, 56(%rsp)          # 8-byte Spill
	incq	%r15
	movq	48(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	88(%rsp), %r10          # 8-byte Reload
	movq	80(%rsp), %r14          # 8-byte Reload
	jl	.LBB36_20
.LBB36_121:                             # %for.end462
	movq	56(%rsp), %r12          # 8-byte Reload
	andq	$1, %r12
.LBB36_122:                             # %return
	movq	%r12, %rax
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB36_75:                              # %cond.false
	callq	get_insns
	movq	%rax, %r12
.LBB36_76:                              # %cond.end
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 160(%rsp)
	movq	%rax, %r15
	xorq	%rax, %rax
	movl	%eax, 176(%rsp)
	cmpq	%r14, %r12
	je	.LBB36_123
# BB#77:
	movabsq	$insns_for_mem_walk, %rbx
	leaq	160(%rsp), %r13
.LBB36_78:                              # %for.body5.lr.ph.i647
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_79 Depth 2
	movq	%r12, 184(%rsp)
	movq	%r12, %rax
	.align	16, 0x90
.LBB36_79:                              # %for.body5.i654
                                        #   Parent Loop BB36_78 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB36_81
# BB#80:                                # %if.then.i657
                                        #   in Loop: Header=BB36_79 Depth=2
	movq	%rax, 168(%rsp)
	leaq	184(%rsp), %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	for_each_rtx
	movq	184(%rsp), %rax
.LBB36_81:                              # %for.inc.i661
                                        #   in Loop: Header=BB36_79 Depth=2
	movq	24(%rax), %rax
	cmpq	%r14, %rax
	movq	%rax, 184(%rsp)
	jne	.LBB36_79
# BB#82:                                # %for.inc10.i665
                                        #   in Loop: Header=BB36_78 Depth=1
	movl	176(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	movl	%eax, 176(%rsp)
	jl	.LBB36_78
	jmp	.LBB36_83
.LBB36_123:                             # %for.inc10.us.i646.preheader
	movq	%r14, 184(%rsp)
	movq	$2, %rax
	movl	%eax, 176(%rsp)
.LBB36_83:                              # %compute_insns_for_mem.exit666
	callq	start_sequence
	movq	40(%rsp), %rdi          # 8-byte Reload
	movzbl	2(%rdi), %eax
	movzbl	mode_size(%rax), %r9d
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %ecx
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	store_bit_field
	callq	get_insns
	movq	%rax, %rdi
	callq	unshare_all_rtl_again
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	emit_insn_after
	movq	24(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB36_84
# BB#85:                                # %if.then300
	testq	%rax, %rax
	je	.LBB36_87
# BB#86:                                # %cond.true305
	movq	24(%rax), %rbx
.LBB36_87:                              # %cond.end310
	movq	%r15, 128(%rsp)
	xorq	%rax, %rax
	movl	%eax, 144(%rsp)
	cmpq	%rbx, %rcx
	je	.LBB36_124
# BB#88:
	movabsq	$insns_for_mem_walk, %r14
	leaq	152(%rsp), %r12
	leaq	128(%rsp), %r13
.LBB36_89:                              # %for.body5.lr.ph.i613
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_90 Depth 2
	movq	%rcx, 152(%rsp)
	movq	%rcx, %rax
	movq	%rcx, %r15
	.align	16, 0x90
.LBB36_90:                              # %for.body5.i620
                                        #   Parent Loop BB36_89 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB36_92
# BB#91:                                # %if.then.i623
                                        #   in Loop: Header=BB36_90 Depth=2
	movq	%rax, 136(%rsp)
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	for_each_rtx
	movq	152(%rsp), %rax
.LBB36_92:                              # %for.inc.i627
                                        #   in Loop: Header=BB36_90 Depth=2
	movq	24(%rax), %rax
	cmpq	%rbx, %rax
	movq	%rax, 152(%rsp)
	jne	.LBB36_90
# BB#93:                                # %for.inc10.i631
                                        #   in Loop: Header=BB36_89 Depth=1
	movl	144(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	movl	%eax, 144(%rsp)
	movq	%r15, %rcx
	jl	.LBB36_89
# BB#94:
	movq	$1, %r12
	jmp	.LBB36_106
.LBB36_84:
	movq	$1, %r12
	jmp	.LBB36_106
.LBB36_98:                              # %cond.false343
	callq	get_insns
	movq	%rax, %r15
.LBB36_99:                              # %cond.end345
	movq	$1, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 96(%rsp)
	xorq	%rax, %rax
	movl	%eax, 112(%rsp)
	cmpq	%r14, %r15
	je	.LBB36_125
# BB#100:
	leaq	120(%rsp), %r13
	leaq	96(%rsp), %rbx
.LBB36_101:                             # %for.body5.lr.ph.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_102 Depth 2
	movq	%r15, 120(%rsp)
	movq	%r15, %rax
	.align	16, 0x90
.LBB36_102:                             # %for.body5.i
                                        #   Parent Loop BB36_101 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB36_104
# BB#103:                               # %if.then.i
                                        #   in Loop: Header=BB36_102 Depth=2
	movq	%rax, 104(%rsp)
	movq	%r13, %rdi
	movabsq	$insns_for_mem_walk, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
	movq	120(%rsp), %rax
.LBB36_104:                             # %for.inc.i
                                        #   in Loop: Header=BB36_102 Depth=2
	movq	24(%rax), %rax
	cmpq	%r14, %rax
	movq	%rax, 120(%rsp)
	jne	.LBB36_102
# BB#105:                               # %for.inc10.i
                                        #   in Loop: Header=BB36_101 Depth=1
	movl	112(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	movl	%eax, 112(%rsp)
	jl	.LBB36_101
	jmp	.LBB36_106
.LBB36_125:                             # %for.inc10.us.i.preheader
	movq	%r14, 120(%rsp)
	movq	$2, %rax
	movl	%eax, 112(%rsp)
	jmp	.LBB36_106
.LBB36_124:                             # %for.inc10.us.i612.preheader
	movq	%rcx, 152(%rsp)
	movq	$2, %rax
	movl	%eax, 144(%rsp)
	movq	$1, %r12
.LBB36_106:                             # %if.end347
	movq	purge_bitfield_addressof_replacements(%rip), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, purge_bitfield_addressof_replacements(%rip)
	jmp	.LBB36_122
.LBB36_14:                              # %if.then.i637
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.put_addressof_into_stack, %rcx
	movq	$2997, %rsi             # imm = 0xBB5
	jmp	.LBB36_15
.LBB36_37:                              # %if.then41
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.purge_addressof_1, %rcx
	movq	$3088, %rsi             # imm = 0xC10
	jmp	.LBB36_15
.LBB36_57:                              # %if.then178
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.purge_addressof_1, %rcx
	movq	$3136, %rsi             # imm = 0xC40
.LBB36_15:                              # %if.then.i637
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp260:
	.size	purge_addressof_1, .Ltmp260-purge_addressof_1
	.cfi_endproc

	.align	16, 0x90
	.type	is_addressof,@function
is_addressof:                           # @is_addressof
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$70, %rax
	jne	.LBB37_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB37_1:
	movabsq	$0, %rax
	retq
.Ltmp261:
	.size	is_addressof, .Ltmp261-is_addressof
	.cfi_endproc

	.globl	purge_hard_subreg_sets
	.align	16, 0x90
	.type	purge_hard_subreg_sets,@function
purge_hard_subreg_sets:                 # @purge_hard_subreg_sets
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp268:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp269:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp270:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp271:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp272:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp273:
	.cfi_def_cfa_offset 64
.Ltmp274:
	.cfi_offset %rbx, -48
.Ltmp275:
	.cfi_offset %r12, -40
.Ltmp276:
	.cfi_offset %r13, -32
.Ltmp277:
	.cfi_offset %r14, -24
.Ltmp278:
	.cfi_offset %r15, -16
	jmp	.LBB38_1
	.align	16, 0x90
.LBB38_12:                              # %for.inc39
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	24(%rdi), %rdi
.LBB38_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB38_15 Depth 2
	testq	%rdi, %rdi
	je	.LBB38_13
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB38_12
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	32(%rax), %r15
	movzbl	(%r15), %eax
	cmpq	$39, %rax
	jne	.LBB38_4
# BB#14:                                # %sw.bb13
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%r15), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB38_12
	jmp	.LBB38_15
	.align	16, 0x90
.LBB38_17:                              # %for.cond16.backedge.for.body19_crit_edge
                                        #   in Loop: Header=BB38_15 Depth=2
	movq	8(%r15), %rax
.LBB38_15:                              # %for.body19.lr.ph
                                        #   Parent Loop BB38_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	jne	.LBB38_16
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB38_15 Depth=2
	movq	8(%r13), %r12
	movl	(%r12), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB38_16
# BB#19:                                # %land.lhs.true.i68
                                        #   in Loop: Header=BB38_15 Depth=2
	movq	8(%r12), %rdi
	movl	(%rdi), %esi
	movq	$16, %rcx
	movq	%rdx, %r14
	shrq	%cl, %r14
	andq	$255, %r14
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$61, %rcx
	jne	.LBB38_22
# BB#20:                                # %land.lhs.true12.i72
                                        #   in Loop: Header=BB38_15 Depth=2
	movl	8(%rdi), %edi
	cmpq	$52, %rdi
	ja	.LBB38_22
# BB#21:                                # %if.then.i80
                                        #   in Loop: Header=BB38_15 Depth=2
	movl	16(%r12), %edx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r14, %rcx
	callq	subreg_regno_offset
	movq	8(%r12), %r12
	movl	(%r12), %edx
.LBB38_22:                              # %if.end.i86
                                        #   in Loop: Header=BB38_15 Depth=2
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB38_16
# BB#23:                                # %land.lhs.true43.i90
                                        #   in Loop: Header=BB38_15 Depth=2
	movl	8(%r12), %esi
	cmpq	$52, %rsi
	ja	.LBB38_16
# BB#24:                                # %if.then48.i94
                                        #   in Loop: Header=BB38_15 Depth=2
	addq	%rax, %rsi
	movq	%r14, %rdi
	callq	gen_rtx_REG
	movq	%rax, 8(%r13)
	.align	16, 0x90
.LBB38_16:                              # %for.cond16.backedge
                                        #   in Loop: Header=BB38_15 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB38_17
	jmp	.LBB38_12
.LBB38_4:                               # %if.then
                                        #   in Loop: Header=BB38_1 Depth=1
	cmpq	$47, %rax
	jne	.LBB38_12
# BB#5:                                 # %sw.bb
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%r15), %rbx
	movl	(%rbx), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB38_12
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	8(%rbx), %rdi
	movl	(%rdi), %esi
	movq	$16, %rcx
	movq	%rdx, %r14
	shrq	%cl, %r14
	andq	$255, %r14
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$61, %rcx
	jne	.LBB38_9
# BB#7:                                 # %land.lhs.true12.i
                                        #   in Loop: Header=BB38_1 Depth=1
	movl	8(%rdi), %edi
	cmpq	$52, %rdi
	ja	.LBB38_9
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB38_1 Depth=1
	movl	16(%rbx), %edx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r14, %rcx
	callq	subreg_regno_offset
	movq	8(%rbx), %rbx
	movl	(%rbx), %edx
.LBB38_9:                               # %if.end.i
                                        #   in Loop: Header=BB38_1 Depth=1
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB38_12
# BB#10:                                # %land.lhs.true43.i
                                        #   in Loop: Header=BB38_1 Depth=1
	movl	8(%rbx), %esi
	cmpq	$52, %rsi
	ja	.LBB38_12
# BB#11:                                # %if.then48.i
                                        #   in Loop: Header=BB38_1 Depth=1
	addq	%rax, %rsi
	movq	%r14, %rdi
	callq	gen_rtx_REG
	movq	%rax, 8(%r15)
	jmp	.LBB38_12
.LBB38_13:                              # %for.end43
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp279:
	.size	purge_hard_subreg_sets, .Ltmp279-purge_hard_subreg_sets
	.cfi_endproc

	.globl	instantiate_virtual_regs
	.align	16, 0x90
	.type	instantiate_virtual_regs,@function
instantiate_virtual_regs:               # @instantiate_virtual_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp284:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp285:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp286:
	.cfi_def_cfa_offset 32
.Ltmp287:
	.cfi_offset %rbx, -32
.Ltmp288:
	.cfi_offset %r14, -24
.Ltmp289:
	.cfi_offset %r15, -16
	movzbl	target_flags+1(%rip), %ecx
	movq	%rsi, %rbx
	movq	%rdi, %r14
	xorq	%rax, %rax
	testq	$16, %rcx
	je	.LBB39_2
# BB#1:                                 # %cond.true
	movq	cfun(%rip), %rax
	movl	76(%rax), %eax
.LBB39_2:                               # %cond.end
	movl	%eax, dynamic_offset(%rip)
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	instantiate_decls
	callq	init_recog
	testq	%rbx, %rbx
	je	.LBB39_8
# BB#3:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB39_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	addq	$-32, %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	ja	.LBB39_7
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB39_4 Depth=1
	leaq	32(%rbx), %rdi
	movq	$1, %rdx
	movq	%rbx, %rsi
	callq	instantiate_virtual_regs_1
	leaq	56(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB39_7
# BB#6:                                 # %if.then17
                                        #   in Loop: Header=BB39_4 Depth=1
	leaq	64(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
.LBB39_7:                               # %for.inc
                                        #   in Loop: Header=BB39_4 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB39_4
.LBB39_8:                               # %for.cond26.preheader
	movq	cfun(%rip), %rax
	movl	304(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB39_13
# BB#9:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB39_10:                              # %for.body28
                                        # =>This Inner Loop Header: Depth=1
	movq	312(%rax), %rcx
	movq	%rbx, %rdx
	andq	%r15, %rdx
	cmpq	$0, (%rcx,%rdx,8)
	je	.LBB39_12
# BB#11:                                # %if.then31
                                        #   in Loop: Header=BB39_10 Depth=1
	leaq	(%rcx,%rdx,8), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	movq	cfun(%rip), %rax
.LBB39_12:                              # %for.inc37
                                        #   in Loop: Header=BB39_10 Depth=1
	movl	304(%rax), %ecx
	incq	%rbx
	movq	%rbx, %rdx
	andq	%r15, %rdx
	cmpq	%rcx, %rdx
	jb	.LBB39_10
.LBB39_13:                              # %for.end38
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	instantiate_decls
	movq	$1, %rax
	movl	%eax, virtuals_instantiated(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp290:
	.size	instantiate_virtual_regs, .Ltmp290-instantiate_virtual_regs
	.cfi_endproc

	.align	16, 0x90
	.type	instantiate_decls,@function
instantiate_decls:                      # @instantiate_decls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp296:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp297:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp298:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp299:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp300:
	.cfi_def_cfa_offset 48
.Ltmp301:
	.cfi_offset %rbx, -40
.Ltmp302:
	.cfi_offset %r12, -32
.Ltmp303:
	.cfi_offset %r14, -24
.Ltmp304:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	88(%r14), %rbx
	movq	%rsi, %r15
	jmp	.LBB40_1
	.align	16, 0x90
.LBB40_6:                               # %cond.end
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	%r15, %rdx
	callq	instantiate_decl
	movq	(%rbx), %rbx
.LBB40_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB40_7
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	8(%rbx), %rdi
	callq	int_size_in_bytes
	movq	144(%rbx), %rdi
	movq	%rax, %r12
	testq	%rdi, %rdi
	jne	.LBB40_4
# BB#3:                                 # %cond.false
                                        #   in Loop: Header=BB40_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB40_4:                               # %cond.end
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	instantiate_decl
	movq	160(%rbx), %rdi
	movzbl	2(%rdi), %eax
	movzbl	mode_size(%rax), %esi
	cmpq	%r12, %rsi
	jge	.LBB40_6
# BB#5:                                 # %cond.end
                                        #   in Loop: Header=BB40_1 Depth=1
	movq	%r12, %rsi
	jmp	.LBB40_6
.LBB40_7:                               # %for.end
	movq	104(%r14), %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	instantiate_decls_1  # TAILCALL
.Ltmp305:
	.size	instantiate_decls, .Ltmp305-instantiate_decls
	.cfi_endproc

	.align	16, 0x90
	.type	instantiate_virtual_regs_1,@function
instantiate_virtual_regs_1:             # @instantiate_virtual_regs_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp312:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp313:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp314:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp315:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp316:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp317:
	.cfi_def_cfa_offset 112
.Ltmp318:
	.cfi_offset %rbx, -48
.Ltmp319:
	.cfi_offset %r12, -40
.Ltmp320:
	.cfi_offset %r13, -32
.Ltmp321:
	.cfi_offset %r14, -24
.Ltmp322:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	(%r12), %r11
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	testq	%r11, %r11
	je	.LBB41_132
# BB#1:                                 # %if.end.lr.ph.lr.ph
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	jmp	.LBB41_2
	.align	16, 0x90
.LBB41_3:                               # %if.end
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	$1, %rbx
	jmpq	*.LJTI41_0(,%rcx,8)
.LBB41_65:                              # %if.end.sw.bb227_crit_edge1025
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	16(%r11), %rdi
.LBB41_66:                              # %sw.bb227
                                        #   in Loop: Header=BB41_2 Depth=1
	testq	%rdi, %rdi
	je	.LBB41_73
# BB#67:                                # %land.lhs.true232
                                        #   in Loop: Header=BB41_2 Depth=1
	movzbl	(%rdi), %ecx
	leaq	16(%r11), %rdi
	cmpq	$133, %rcx
	jg	.LBB41_70
# BB#68:                                # %land.lhs.true232
                                        #   in Loop: Header=BB41_2 Depth=1
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	ja	.LBB41_72
# BB#69:                                # %land.lhs.true232
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB41_73
	jmp	.LBB41_72
.LBB41_70:                              # %land.lhs.true232
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$134, %rcx
	je	.LBB41_73
# BB#71:                                # %land.lhs.true232
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$140, %rcx
	je	.LBB41_73
.LBB41_72:                              # %if.then288
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r10, %r14
	movq	%r11, %rbx
	callq	instantiate_virtual_regs_1
	movq	%rbx, %r11
	movq	%r14, %r10
	.align	16, 0x90
.LBB41_73:                              # %if.end293
                                        #   in Loop: Header=BB41_2 Depth=1
	addq	$8, %r11
	movq	%r11, %rbx
.LBB41_74:                              # %restart.backedge
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	(%rbx), %r11
	testq	%r11, %r11
.LBB41_25:                              # %restart.outer.backedge
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%rbx, %r12
	jne	.LBB41_2
	jmp	.LBB41_132
.LBB41_97:                              # %sw.bb453
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r10, %r14
	movq	8(%r11), %rax
	movl	(%rax), %ecx
	addq	$8, %r11
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	jne	.LBB41_100
# BB#98:                                # %land.lhs.true460
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r11, %rbx
	leaq	8(%rax), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	instantiate_virtual_regs_1
	testq	%r14, %rax
	jne	.LBB41_132
# BB#99:                                # %land.lhs.true460.lor.lhs.false472_crit_edge
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%rbx, %r11
	movq	(%r11), %rax
	movl	(%rax), %ecx
.LBB41_100:                             # %lor.lhs.false472
                                        #   in Loop: Header=BB41_2 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB41_103
# BB#101:                               # %land.lhs.true479
                                        #   in Loop: Header=BB41_2 Depth=1
	xorq	%rdx, %rdx
	movq	%r11, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%r11, %rbx
	callq	instantiate_virtual_regs_1
	testq	%r14, %rax
	jne	.LBB41_132
# BB#102:                               # %land.lhs.true479.if.end486_crit_edge
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	(%rbx), %rax
	movq	%rbx, %r11
.LBB41_103:                             # %if.end486
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%rax, %rdi
	movq	%r11, %rbx
	callq	copy_rtx
	movq	%rax, (%rbx)
	movq	%r14, %r10
	jmp	.LBB41_74
.LBB41_4:                               # %sw.bb1
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%r11), %rcx
	movq	global_rtl+48(%rip), %rax
	leaq	8(%r11), %r14
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	movabsq	$global_rtl+40, %rax
	jne	.LBB41_6
# BB#5:                                 #   in Loop: Header=BB41_2 Depth=1
	movq	%r11, %r15
	movq	%r10, %r13
	jmp	.LBB41_13
.LBB41_75:                              # %sw.bb297
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%r11), %rax
	movzbl	(%rax), %r8d
	leaq	-54(%r8), %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	jbe	.LBB41_76
.LBB41_77:                              # %lor.lhs.false320
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+40(%rip), %rsi
	cmpq	%rsi, %rax
	je	.LBB41_132
# BB#78:                                # %lor.lhs.false320
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+32(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB41_132
# BB#79:                                # %lor.lhs.false320
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+24(%rip), %rdx
	cmpq	%rdx, %rax
	je	.LBB41_132
# BB#80:                                # %if.end327
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$75, %r8
	jne	.LBB41_86
# BB#81:                                # %land.lhs.true331
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	ja	.LBB41_86
# BB#82:                                # %land.lhs.true331
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	$1, %rbx
	movq	$1, %r9
	shlq	%cl, %r9
	testq	$24595, %r9             # imm = 0x6013
	je	.LBB41_86
# BB#83:                                # %land.lhs.true366
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.LBB41_133
# BB#84:                                # %land.lhs.true366
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	%rdi, %rcx
	je	.LBB41_133
# BB#85:                                # %land.lhs.true366
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	%rsi, %rcx
	je	.LBB41_133
.LBB41_86:                              # %if.end382
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+56(%rip), %rsi
	cmpq	%rsi, %rax
	je	.LBB41_93
# BB#87:                                # %if.end382
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+48(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB41_93
# BB#88:                                # %lor.lhs.false386
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$75, %r8
	jne	.LBB41_73
# BB#89:                                # %land.lhs.true390
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	ja	.LBB41_73
# BB#90:                                # %land.lhs.true390
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24595, %rdx            # imm = 0x6013
	je	.LBB41_73
# BB#91:                                # %land.lhs.true425
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%rax), %rax
	cmpq	%rsi, %rax
	je	.LBB41_93
# BB#92:                                # %land.lhs.true425
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	%rdi, %rax
	jne	.LBB41_73
.LBB41_93:                              # %if.then435
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r10, %rbx
	leaq	8(%r11), %rdi
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %rsi
	jne	.LBB41_95
# BB#94:                                # %if.then435
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r11, %rsi
.LBB41_95:                              # %if.then435
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r11, %r14
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	testq	%rbx, %rax
	jne	.LBB41_132
# BB#96:                                # %if.end446
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rbx, %r10
	movq	%rax, %r11
	movq	%r11, (%r12)
	jmp	.LBB41_73
.LBB41_26:                              # %sw.bb44
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	16(%r11), %rdi
	movzbl	(%rdi), %ecx
	leaq	16(%r11), %r14
	cmpq	$133, %rcx
	jg	.LBB41_29
# BB#27:                                # %sw.bb44
                                        #   in Loop: Header=BB41_2 Depth=1
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	ja	.LBB41_66
# BB#28:                                # %sw.bb44
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB41_31
	jmp	.LBB41_66
.LBB41_6:                               # %if.else.i
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+56(%rip), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB41_8
# BB#7:                                 #   in Loop: Header=BB41_2 Depth=1
	movq	%r11, %r15
	movq	%r10, %r13
	movabsq	$global_rtl+24, %rax
	jmp	.LBB41_13
.LBB41_29:                              # %sw.bb44
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$134, %rcx
	je	.LBB41_31
# BB#30:                                # %sw.bb44
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$140, %rcx
	jne	.LBB41_66
.LBB41_31:                              # %if.then100
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%r11), %rcx
	movzbl	(%rcx), %r8d
	leaq	8(%r11), %rbx
	cmpq	$75, %r8
	jne	.LBB41_46
# BB#32:                                # %if.then107
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%rcx), %rdx
	movq	global_rtl+48(%rip), %rax
	xorq	%r8, %r8
	cmpq	%rdx, %rax
	movabsq	$global_rtl+40, %rax
	je	.LBB41_40
# BB#33:                                # %if.else.i789
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+56(%rip), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB41_35
# BB#34:                                #   in Loop: Header=BB41_2 Depth=1
	movabsq	$global_rtl+24, %rax
	jmp	.LBB41_40
.LBB41_46:                              # %if.else137
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+48(%rip), %rdx
	xorq	%r15, %r15
	cmpq	%rcx, %rdx
	movabsq	$global_rtl+40, %rdx
	je	.LBB41_54
# BB#47:                                # %if.else.i772
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+56(%rip), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB41_49
# BB#48:                                #   in Loop: Header=BB41_2 Depth=1
	movabsq	$global_rtl+24, %rdx
	jmp	.LBB41_54
.LBB41_8:                               # %if.else5.i
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+64(%rip), %rdx
	movq	%r11, %r15
	movq	%r10, %r13
	cmpq	%rcx, %rdx
	jne	.LBB41_10
# BB#9:                                 # %if.then8.i
                                        #   in Loop: Header=BB41_2 Depth=1
	movslq	dynamic_offset(%rip), %r12
	movabsq	$global_rtl+16, %rax
	jmp	.LBB41_13
.LBB41_10:                              # %if.else10.i
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+72(%rip), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB41_12
# BB#11:                                #   in Loop: Header=BB41_2 Depth=1
	movabsq	$global_rtl+16, %rax
	jmp	.LBB41_13
.LBB41_76:                              # %sw.bb297
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24595, %rdx            # imm = 0x6013
	je	.LBB41_77
	jmp	.LBB41_132
.LBB41_35:                              # %if.else5.i791
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+64(%rip), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB41_37
# BB#36:                                # %if.then8.i793
                                        #   in Loop: Header=BB41_2 Depth=1
	movslq	dynamic_offset(%rip), %r8
	movabsq	$global_rtl+16, %rax
	jmp	.LBB41_40
.LBB41_49:                              # %if.else5.i774
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+64(%rip), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB41_51
# BB#50:                                # %if.then8.i776
                                        #   in Loop: Header=BB41_2 Depth=1
	movslq	dynamic_offset(%rip), %r15
	movabsq	$global_rtl+16, %rdx
	jmp	.LBB41_54
.LBB41_12:                              # %if.else15.i
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+80(%rip), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB41_23
.LBB41_13:                              # %instantiate_new_reg.exit
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB41_14
.LBB41_23:                              # %if.end36
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	%r14, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	instantiate_virtual_regs_1
	movq	16(%r15), %rcx
	addq	$16, %r15
	movq	%r15, %rbx
	movq	%r13, %r10
	jmp	.LBB41_24
.LBB41_37:                              # %if.else10.i795
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+72(%rip), %rsi
	cmpq	%rdx, %rsi
	movabsq	$0, %r8
	jne	.LBB41_39
# BB#38:                                #   in Loop: Header=BB41_2 Depth=1
	movabsq	$global_rtl+16, %rax
	jmp	.LBB41_40
.LBB41_51:                              # %if.else10.i778
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+72(%rip), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB41_53
# BB#52:                                #   in Loop: Header=BB41_2 Depth=1
	movabsq	$global_rtl+16, %rdx
	jmp	.LBB41_54
.LBB41_39:                              # %if.else15.i797
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+80(%rip), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB41_74
.LBB41_40:                              # %instantiate_new_reg.exit803
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	(%rax), %r13
	testq	%r13, %r13
	je	.LBB41_24
	jmp	.LBB41_41
.LBB41_53:                              # %if.else15.i780
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	global_rtl+80(%rip), %rsi
	cmpq	%rcx, %rsi
	jne	.LBB41_55
.LBB41_54:                              # %instantiate_new_reg.exit786
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	(%rdx), %rax
	testq	%rax, %rax
	jne	.LBB41_42
.LBB41_55:                              # %if.then143
                                        #   in Loop: Header=BB41_2 Depth=1
	cmpq	$61, %r8
	je	.LBB41_132
.LBB41_24:                              # %restart.outer.backedge
                                        #   in Loop: Header=BB41_2 Depth=1
	testq	%rcx, %rcx
	movq	%rcx, %r11
	jmp	.LBB41_25
	.align	16, 0x90
.LBB41_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r11), %eax
	leaq	-3(%rax), %rcx
	andq	%r10, %rcx
	cmpq	$128, %rcx
	jbe	.LBB41_3
	jmp	.LBB41_122
.LBB41_104:                             # %sw.bb497
	movq	global_rtl+48(%rip), %rax
	xorq	%rsi, %rsi
	cmpq	%r11, %rax
	movabsq	$global_rtl+40, %rax
	je	.LBB41_105
# BB#106:                               # %if.else.i755
	movq	global_rtl+56(%rip), %rcx
	movq	%r10, %r15
	cmpq	%r11, %rcx
	je	.LBB41_107
# BB#108:                               # %if.else5.i757
	movq	global_rtl+64(%rip), %rcx
	cmpq	%r11, %rcx
	je	.LBB41_109
# BB#110:                               # %if.else10.i761
	movq	global_rtl+72(%rip), %rcx
	cmpq	%r11, %rcx
	je	.LBB41_111
# BB#112:                               # %if.else15.i763
	movq	global_rtl+80(%rip), %rcx
	cmpq	%r11, %rcx
	jne	.LBB41_132
	jmp	.LBB41_113
.LBB41_119:                             # %sw.bb520
	movq	8(%r11), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$61, %rcx
	je	.LBB41_132
# BB#120:                               # %sw.bb520
	movq	$70, %rax
	cmpq	$66, %rcx
	jne	.LBB41_122
# BB#121:                               # %if.then535
	addq	$8, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	jmp	.LBB41_132
.LBB41_122:                             # %sw.epilog
	movzbl	rtx_length(%rax), %ecx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB41_132
# BB#123:                               # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rdx
	xorq	%r15, %r15
	.align	16, 0x90
.LBB41_124:                             # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB41_129 Depth 2
	movq	%r10, %r12
	movzbl	(%rdx), %eax
	cmpq	$101, %rax
	jne	.LBB41_125
# BB#130:                               # %if.then553
                                        #   in Loop: Header=BB41_124 Depth=1
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	leaq	8(%r11,%r15,8), %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r11, %rbx
	callq	instantiate_virtual_regs_1
	movq	%rbx, %r11
	xorq	%rbx, %rbx
	movq	%r12, %r10
	testq	%r10, %rax
	jne	.LBB41_131
	jmp	.LBB41_133
	.align	16, 0x90
.LBB41_125:                             # %for.body
                                        #   in Loop: Header=BB41_124 Depth=1
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	movq	%r12, %r10
	jne	.LBB41_131
# BB#126:                               # %for.cond567.preheader
                                        #   in Loop: Header=BB41_124 Depth=1
	movq	8(%r11,%r15,8), %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB41_131
# BB#127:                               #   in Loop: Header=BB41_124 Depth=1
	movq	$8, %r12
	movq	$1, %r13
	.align	16, 0x90
.LBB41_129:                             # %for.body573
                                        #   Parent Loop BB41_124 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r10, %rbx
	movq	%r11, %r14
	callq	instantiate_virtual_regs_1
	movq	%r14, %r11
	movq	%rbx, %r10
	xorq	%rbx, %rbx
	testq	%r10, %rax
	je	.LBB41_133
# BB#128:                               # %for.cond567
                                        #   in Loop: Header=BB41_129 Depth=2
	movq	8(%r11,%r15,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r13
	addq	$8, %r12
	cmpq	%rax, %rdx
	jl	.LBB41_129
	.align	16, 0x90
.LBB41_131:                             # %for.inc586
                                        #   in Loop: Header=BB41_124 Depth=1
	incq	%r15
	movq	32(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB41_124
	jmp	.LBB41_132
.LBB41_14:                              # %if.then3
	movq	16(%r15), %rax
	movq	%rax, 56(%rsp)
	leaq	56(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	movq	56(%rsp), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB41_18
# BB#15:                                # %if.then3
	cmpq	$75, %rax
	jne	.LBB41_16
.LBB41_18:                              # %if.end15
	addq	$16, %r15
	negq	%r12
	callq	start_sequence
	movq	56(%rsp), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	je	.LBB41_20
# BB#19:                                # %if.then19
	xorq	%rsi, %rsi
	callq	force_operand
	movq	%rax, %rdi
.LBB41_20:                              # %if.end21
	movq	%r12, %rsi
	callq	plus_constant_wide
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	movq	%r12, %rdi
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	emit_insns_before
	movq	%rbx, (%r14)
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	validate_change
	testq	%r13, %rax
	je	.LBB41_22
# BB#21:                                # %if.end21
	movq	48(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	movq	$1, %rbx
	jne	.LBB41_133
.LBB41_22:                              # %if.then34
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.instantiate_virtual_regs_1, %rcx
	movq	$3862, %rsi             # imm = 0xF16
	jmp	.LBB41_17
.LBB41_105:
	movq	%r10, %r15
	jmp	.LBB41_113
.LBB41_107:
	movabsq	$global_rtl+24, %rax
	jmp	.LBB41_113
.LBB41_41:                              # %if.then116
	addq	$16, %rcx
	movq	%rcx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%r11, %r15
	callq	instantiate_virtual_regs_1
	movq	(%rbx), %rax
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	movq	16(%rax), %rax
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r15, %r11
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	(%r14), %rdi
.LBB41_42:                              # %if.end156
	movq	%rax, %r13
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB41_44
# BB#43:                                # %land.lhs.true162
	xorq	%rcx, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	32(%rsp), %rax          # 8-byte Reload
	jne	.LBB41_132
.LBB41_44:                              # %if.end166
	movq	(%rbx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	je	.LBB41_45
# BB#56:                                # %cond.false
	movq	%r13, (%rbx)
	xorq	%rcx, %rcx
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB41_57
.LBB41_109:                             # %if.then8.i759
	movslq	dynamic_offset(%rip), %rsi
	movabsq	$global_rtl+16, %rax
	jmp	.LBB41_113
.LBB41_111:
	movabsq	$global_rtl+16, %rax
.LBB41_113:                             # %instantiate_new_reg.exit769
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB41_132
# BB#114:                               # %if.then500
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	plus_constant_wide
	movq	%rax, %r13
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	validate_change
	testq	%r15, %rax
	jne	.LBB41_132
# BB#115:                               # %if.then504
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB41_133
# BB#116:                               # %if.end507
	movq	%r15, %r13
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	force_operand
	movq	%rax, %r15
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	callq	emit_insns_before
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	validate_change
	testq	%r13, %rax
	jne	.LBB41_132
# BB#117:                               # %land.lhs.true513
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	validate_replace_rtx
	testq	%r13, %rax
	movq	$1, %rbx
	jne	.LBB41_133
# BB#118:                               # %if.then516
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.instantiate_virtual_regs_1, %rcx
	movq	$4126, %rsi             # imm = 0x101E
	jmp	.LBB41_17
.LBB41_45:                              # %cond.true
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
.LBB41_57:                              # %cond.false
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB41_132
# BB#58:                                # %if.then184
	movq	48(%rsp), %rax          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB41_59
# BB#60:                                # %if.end190
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	%r13, (%rbx)
	xorq	%rcx, %rcx
	movq	%r15, %r13
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rax, %r14
	movq	%r14, %rdx
	callq	validate_change
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	je	.LBB41_62
# BB#61:                                # %if.then203
	movq	%r14, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	jmp	.LBB41_132
.LBB41_59:                              # %if.then186
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rbx)
	xorq	%rbx, %rbx
	jmp	.LBB41_133
.LBB41_62:                              # %if.else206
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rbx)
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %r14
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	callq	start_sequence
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	force_operand
	movq	%rax, %r15
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	emit_insns_before
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	validate_change
	testq	%r14, %rax
	movq	%r14, %rbx
	je	.LBB41_63
.LBB41_132:
	movq	$1, %rbx
.LBB41_133:                             # %return
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB41_16:                              # %if.then14
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.instantiate_virtual_regs_1, %rcx
	movq	$3846, %rsi             # imm = 0xF06
	jmp	.LBB41_17
.LBB41_63:                              # %land.lhs.true219
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	validate_replace_rtx
	testq	%rbx, %rax
	movq	$1, %rbx
	jne	.LBB41_133
# BB#64:                                # %if.then222
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.instantiate_virtual_regs_1, %rcx
	movq	$3972, %rsi             # imm = 0xF84
.LBB41_17:                              # %if.then14
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp323:
	.size	instantiate_virtual_regs_1, .Ltmp323-instantiate_virtual_regs_1
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI41_0:
	.quad	.LBB41_65
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_133
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_133
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_133
	.quad	.LBB41_133
	.quad	.LBB41_73
	.quad	.LBB41_4
	.quad	.LBB41_97
	.quad	.LBB41_97
	.quad	.LBB41_65
	.quad	.LBB41_133
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_133
	.quad	.LBB41_133
	.quad	.LBB41_133
	.quad	.LBB41_122
	.quad	.LBB41_133
	.quad	.LBB41_133
	.quad	.LBB41_122
	.quad	.LBB41_104
	.quad	.LBB41_122
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_122
	.quad	.LBB41_75
	.quad	.LBB41_122
	.quad	.LBB41_133
	.quad	.LBB41_133
	.quad	.LBB41_119
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_65
	.quad	.LBB41_26
	.quad	.LBB41_65
	.quad	.LBB41_73
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_73
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_65
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_122
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73
	.quad	.LBB41_73

	.text
	.globl	max_parm_reg_num
	.align	16, 0x90
	.type	max_parm_reg_num,@function
max_parm_reg_num:                       # @max_parm_reg_num
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movl	304(%rax), %eax
	retq
.Ltmp324:
	.size	max_parm_reg_num, .Ltmp324-max_parm_reg_num
	.cfi_endproc

	.globl	get_first_nonparm_insn
	.align	16, 0x90
	.type	get_first_nonparm_insn,@function
get_first_nonparm_insn:                 # @get_first_nonparm_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	296(%rax), %rax
	testq	%rax, %rax
	je	.LBB43_2
# BB#1:                                 # %return
	movq	24(%rax), %rax
	retq
.LBB43_2:                               # %if.end
	jmp	get_insns  # TAILCALL
.Ltmp325:
	.size	get_first_nonparm_insn, .Ltmp325-get_first_nonparm_insn
	.cfi_endproc

	.globl	get_first_block_beg
	.align	16, 0x90
	.type	get_first_block_beg,@function
get_first_block_beg:                    # @get_first_block_beg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp327:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	296(%rax), %rax
	testq	%rax, %rax
	je	.LBB44_2
# BB#1:                                 # %if.then.i
	movq	24(%rax), %rax
	jmp	.LBB44_3
.LBB44_2:                               # %if.end.i
	callq	get_insns
.LBB44_3:                               # %for.cond.preheader
	testq	%rax, %rax
	je	.LBB44_8
# BB#4:
	movabsq	$4294967198, %rcx       # imm = 0xFFFFFF9E
	.align	16, 0x90
.LBB44_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %edx
	cmpq	$37, %rdx
	jne	.LBB44_7
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB44_5 Depth=1
	movl	40(%rax), %edx
	cmpq	%rcx, %rdx
	je	.LBB44_9
.LBB44_7:                               # %for.inc
                                        #   in Loop: Header=BB44_5 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB44_5
.LBB44_8:                               # %for.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.get_first_block_beg, %rcx
	movq	$4250, %rsi             # imm = 0x109A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB44_9:                               # %if.then
	popq	%rdx
	retq
.Ltmp328:
	.size	get_first_block_beg, .Ltmp328-get_first_block_beg
	.cfi_endproc

	.globl	aggregate_value_p
	.align	16, 0x90
	.type	aggregate_value_p,@function
aggregate_value_p:                      # @aggregate_value_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp333:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp334:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp335:
	.cfi_def_cfa_offset 32
.Ltmp336:
	.cfi_offset %rbx, -32
.Ltmp337:
	.cfi_offset %r14, -24
.Ltmp338:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$116, %rcx
	je	.LBB45_2
# BB#1:                                 # %cond.false
	movq	8(%rbx), %rbx
	movl	16(%rbx), %eax
.LBB45_2:                               # %cond.end
	andq	$255, %rax
	xorq	%r14, %r14
	cmpq	$5, %rax
	je	.LBB45_26
# BB#3:                                 # %if.end
	movq	%rbx, %rdi
	callq	ix86_return_in_memory
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	movq	$1, %r14
	jne	.LBB45_26
# BB#4:                                 # %if.end11
	movl	16(%rbx), %eax
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB45_26
# BB#5:                                 # %if.end17
	movl	flag_pcc_struct_return(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB45_7
# BB#6:                                 # %land.lhs.true
	andq	$255, %rax
	addq	$-18, %rax
	andq	%r15, %rax
	cmpq	$5, %rax
	jb	.LBB45_26
.LBB45_7:                               # %if.end53
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	hard_function_value
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB45_26
# BB#8:                                 # %if.end60
	movl	8(%rax), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	60(%rbx), %eax
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%rdx, %rdi
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	cmpq	$8, %rdi
	je	.LBB45_12
# BB#9:                                 # %if.end60
	leaq	-21(%rdx), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB45_12
# BB#10:                                # %if.end60
	leaq	-45(%rdx), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB45_12
# BB#11:                                # %if.end60
	addq	$-29, %rdx
	andq	%r15, %rdx
	cmpq	$7, %rdx
	jbe	.LBB45_12
# BB#17:                                # %cond.false104
	andq	%r15, %rax
	cmpq	$24, %rax
	jne	.LBB45_18
# BB#21:                                # %cond.true123
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB45_22
.LBB45_12:                              # %cond.true85
	andq	%r15, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB45_22
# BB#13:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB45_14
# BB#15:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	movq	$1, %rdx
	jmp	.LBB45_24
.LBB45_18:                              # %cond.false104
	cmpq	$18, %rax
	jne	.LBB45_20
# BB#19:                                # %cond.true112
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	movq	$1, %rdx
	jmp	.LBB45_24
.LBB45_20:                              # %cond.end146
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB45_26
.LBB45_22:                              # %for.body.lr.ph
	movq	$1, %rdx
	jmp	.LBB45_24
.LBB45_14:
	movabsq	$0, %rax
	incq	%rax
	movq	$1, %rdx
	.align	16, 0x90
.LBB45_24:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	call_used_regs-1(%rsi,%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB45_25
# BB#23:                                # %for.cond
                                        #   in Loop: Header=BB45_24 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rdx
	cmpq	%rbx, %rdi
	jl	.LBB45_24
	jmp	.LBB45_26
.LBB45_25:
	movq	$1, %r14
.LBB45_26:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp339:
	.size	aggregate_value_p, .Ltmp339-aggregate_value_p
	.cfi_endproc

	.globl	assign_parms
	.align	16, 0x90
	.type	assign_parms,@function
assign_parms:                           # @assign_parms
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp346:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp347:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp348:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp349:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp350:
	.cfi_def_cfa_offset 48
	subq	$256, %rsp              # imm = 0x100
.Ltmp351:
	.cfi_def_cfa_offset 304
.Ltmp352:
	.cfi_offset %rbx, -48
.Ltmp353:
	.cfi_offset %r12, -40
.Ltmp354:
	.cfi_offset %r13, -32
.Ltmp355:
	.cfi_offset %r14, -24
.Ltmp356:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	8(%rdi), %r14
	movl	424(%rax), %eax
	movq	88(%rdi), %r12
	xorq	%r13, %r13
	testq	$65536, %rax            # imm = 0x10000
	movabsq	$0, %r15
	je	.LBB46_7
# BB#1:                                 # %entry
	testq	%r12, %r12
	movabsq	$0, %r15
	je	.LBB46_7
# BB#2:                                 # %land.lhs.true2
	movq	%r12, %rdi
	callq	tree_last
	testq	%rax, %rax
	movabsq	$0, %r15
	je	.LBB46_7
# BB#3:                                 # %land.lhs.true3
	movq	72(%rax), %rax
	testq	%rax, %rax
	movabsq	$0, %r15
	je	.LBB46_7
# BB#4:                                 # %land.rhs
	movq	32(%rax), %rdi
	movabsq	$.L.str2, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_5
# BB#6:                                 # %land.rhs
	movabsq	$1, %r15
	jmp	.LBB46_7
.LBB46_5:
	movabsq	$0, %r15
.LBB46_7:                               # %land.end
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movq	24(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB46_11
# BB#8:                                 # %land.rhs12
	callq	tree_last
	movq	32(%rax), %rax
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB46_9
# BB#10:                                # %land.rhs12
	movabsq	$1, %r13
	jmp	.LBB46_11
.LBB46_9:
	movabsq	$0, %r13
.LBB46_11:                              # %land.end17
	movq	cfun(%rip), %rax
	movl	424(%rax), %edx
	movq	$17, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movabsq	$4294836223, %rcx       # imm = 0xFFFDFFFF
	andq	%rdx, %rcx
	orq	%rsi, %rcx
	movl	%ecx, 424(%rax)
	movzbl	fixed_regs+16(%rip), %ecx
	movq	global_rtl+48(%rip), %rax
	testq	%rcx, %rcx
	jne	.LBB46_13
# BB#12:                                # %if.then
	movq	%rax, %rdi
	callq	copy_to_reg
.LBB46_13:                              # %if.end
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	cfun(%rip), %rcx
	movq	%rax, 128(%rcx)
	movq	$0, 208(%rsp)
	movq	$0, 200(%rsp)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	96(%rax), %rdi
	callq	aggregate_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB46_14
# BB#15:                                # %land.lhs.true28
	movq	cfun(%rip), %rax
	movl	424(%rax), %eax
	testq	$2, %rax
	jne	.LBB46_16
# BB#17:                                # %land.lhs.true28
	movq	struct_value_incoming_rtx(%rip), %rax
	testq	%rax, %rax
	jne	.LBB46_18
# BB#19:                                # %if.then36
	movq	8(%r14), %rdi
	callq	build_pointer_type
	movq	%r15, %rbx
	movq	%rax, %r15
	movq	$35, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	build_decl
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r15, 104(%rax)
	movq	%rbx, %r15
	movq	%r12, (%rax)
	movq	%rax, %r12
	jmp	.LBB46_20
.LBB46_14:
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB46_20
.LBB46_16:
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB46_20
.LBB46_18:
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
.LBB46_20:                              # %if.end44
	movq	cfun(%rip), %rax
	movq	$58, %rcx
	movl	%ecx, 304(%rax)
	movq	$58, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	cfun(%rip), %rcx
	movq	%rax, 312(%rcx)
	leaq	224(%rsp), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	init_cumulative_args
	movq	cfun(%rip), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 72(%rax)
	xorq	%rdi, %rdi
	testq	%r12, %r12
	je	.LBB46_21
# BB#22:                                # %for.body.lr.ph
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%r13, %r14
	movq	%r14, 40(%rsp)          # 8-byte Spill
	jmp	.LBB46_23
.LBB46_39:                              # %if.end141
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	50(%r12), %r8d
	movq	%rbx, %rcx
	movq	%r13, %rbx
	xorq	$1, %rbx
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	%rcx, %rbx
	andq	$127, %rdx
	testq	$8, %r8
	movabsq	$0, %r14
	movq	%rdx, %r15
	jne	.LBB46_43
# BB#40:                                # %lor.lhs.false147
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$21, %rcx
	jne	.LBB46_41
# BB#42:                                # %lor.lhs.false147
                                        #   in Loop: Header=BB46_23 Depth=1
	andq	$524288, %rax           # imm = 0x80000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB46_41
.LBB46_43:                              # %if.then160
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rdi, 136(%rsp)         # 8-byte Spill
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	movq	24(%rbx), %rax
	movq	8(%rax), %rbx
	jmp	.LBB46_44
.LBB46_41:                              #   in Loop: Header=BB46_23 Depth=1
	movq	%rdi, 136(%rsp)         # 8-byte Spill
	movq	%rsi, 88(%rsp)          # 8-byte Spill
.LBB46_44:                              # %if.end165
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	32(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	je	.LBB46_46
# BB#45:                                # %land.lhs.true173
                                        #   in Loop: Header=BB46_23 Depth=1
	callq	contains_placeholder_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_47
.LBB46_46:                              # %lor.lhs.false178
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	17(%rbx), %eax
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	testq	$4, %rax
	je	.LBB46_48
.LBB46_47:                              # %if.then184
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, %rdi
	callq	build_pointer_type
	movl	target_flags(%rip), %r15d
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	orq	$4, %r15
	movq	%r15, 136(%rsp)         # 8-byte Spill
	movq	$1, 112(%rsp)           # 8-byte Folded Spill
	movq	%rax, %rbx
.LBB46_48:                              # %if.end188
                                        #   in Loop: Header=BB46_23 Depth=1
	leaq	224(%rsp), %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	128(%rsp), %rcx         # 8-byte Reload
	callq	function_arg
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	je	.LBB46_51
# BB#49:                                # %if.end188
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB46_51
# BB#50:                                # %if.then197
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rcx
	addq	$72, %rcx
	xorq	%r8, %r8
	leaq	224(%rsp), %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	ix86_setup_incoming_varargs
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
.LBB46_51:                              # %if.end199
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	$1, %rcx
	leaq	224(%rsp), %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	function_arg
	testq	%rax, %rax
	movq	%r14, %rdx
	je	.LBB46_53
# BB#52:                                # %if.end199
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rdx
.LBB46_53:                              # %if.end199
                                        #   in Loop: Header=BB46_23 Depth=1
	leaq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	152(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rsi
	leaq	200(%rsp), %r8
	leaq	168(%rsp), %r9
	callq	locate_and_pad_parm
	movq	176(%rsp), %rsi
	testq	%rsi, %rsi
	jne	.LBB46_55
# BB#54:                                # %cond.true207
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	168(%rsp), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB46_56
.LBB46_55:                              # %cond.false217
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	168(%rsp), %rdi
	movq	%rax, %r14
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB46_56:                              # %cond.end226
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	movq	56(%rsp), %rdx          # 8-byte Reload
	jne	.LBB46_58
# BB#57:                                # %if.then230
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, %rdi
	movq	%r15, 144(%rsp)         # 8-byte Spill
	movq	%rdx, %rsi
	jmp	.LBB46_59
.LBB46_58:                              # %if.else232
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r15, %rdi
	movq	%r15, 144(%rsp)         # 8-byte Spill
	movq	%rax, %rsi
.LBB46_59:                              # %if.end238
                                        #   in Loop: Header=BB46_23 Depth=1
	callq	gen_rtx_MEM
	movq	%rax, %r15
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	set_mem_attributes
	testq	%rbx, %rbx
	je	.LBB46_65
# BB#60:                                # %land.lhs.true241
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$25, %rcx
	jne	.LBB46_66
# BB#61:                                # %lor.lhs.false250
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	17(%rbx), %ecx
	testq	$4, %rcx
	jne	.LBB46_66
# BB#62:                                # %lor.lhs.false250
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	144(%rsp), %rcx         # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	je	.LBB46_66
# BB#63:                                # %lor.lhs.false260
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$51, %rcx
	jne	.LBB46_65
# BB#64:                                # %land.lhs.true275
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, %rdi
	callq	int_size_in_bytes
.LBB46_65:                              # %if.end285
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
.LBB46_66:                              # %if.end285
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB46_68
# BB#67:                                # %if.end285
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rax, %rcx
.LBB46_68:                              # %if.end285
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	%r15, %rcx
	movq	%rcx, 160(%r12)
	je	.LBB46_71
# BB#69:                                # %lor.lhs.false337
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	(%rcx), %eax
	xorq	%r14, %r14
	cmpq	$39, %rax
	jne	.LBB46_79
# BB#70:                                # %land.lhs.true342
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%rcx), %rax
	movq	8(%rax), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB46_79
.LBB46_71:                              # %if.then350
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	152(%rsp), %rax
	movq	200(%rsp), %rbx
	movq	160(%rsp), %r14
	addq	%rax, %rbx
	movq	%rbx, 200(%rsp)
	testq	%r14, %r14
	jne	.LBB46_73
# BB#72:                                #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, %r14
	jmp	.LBB46_79
.LBB46_73:                              # %do.body
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_74
# BB#75:                                # %if.else364
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	208(%rsp), %rbx
	movq	sizetype_tab+8(%rip), %rdi
	movq	%r14, %rsi
	callq	convert
	testq	%rbx, %rbx
	je	.LBB46_77
# BB#76:                                # %if.else371
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
.LBB46_77:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rax, 208(%rsp)
	jmp	.LBB46_78
.LBB46_74:                              # %if.then360
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	tree_low_cst
	addq	%rax, %rbx
	movq	%rbx, 200(%rsp)
.LBB46_78:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, %r14
	movq	80(%rsp), %rcx          # 8-byte Reload
.LBB46_79:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rcx, %r13
	leaq	224(%rsp), %rdi
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	104(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdx
	movq	128(%rsp), %rcx         # 8-byte Reload
	callq	function_arg_advance
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	ix86_function_arg_boundary
	movq	%rax, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rbx
	andq	%rbx, %r15
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	get_mode_alignment
	andq	%rbx, %rax
	cmpq	%r15, %rax
	movabsq	$0, %r10
	ja	.LBB46_81
# BB#80:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r14, %r10
.LBB46_81:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r8
	movq	%r13, %rsi
	testq	%rsi, %rsi
	movabsq	$0, %r14
	movq	%r14, %rax
	jne	.LBB46_83
# BB#82:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rax
.LBB46_83:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	144(%rsp), %rdx         # 8-byte Reload
	andq	%rcx, %rdx
	cmpq	$51, %r8
	movq	%r14, %rcx
	jne	.LBB46_85
# BB#84:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rcx
.LBB46_85:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	orq	%rcx, %rax
	cmpq	%rdx, %r8
	movq	%r14, %rcx
	jne	.LBB46_87
# BB#86:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rcx
.LBB46_87:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	orq	%rax, %rcx
	movq	%r10, %rbx
	jne	.LBB46_89
# BB#88:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rbx, %rbx
.LBB46_89:                              # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	(%rsi), %eax
	cmpq	$39, %rax
	jne	.LBB46_90
# BB#91:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$51, %r8
	je	.LBB46_90
# BB#92:                                # %if.end380
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$51, %rdx
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	je	.LBB46_93
# BB#94:                                # %if.then407
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, %r9
	movq	8(%rsi), %rax
	movq	%rsi, %r11
	movslq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	jle	.LBB46_101
	.align	16, 0x90
.LBB46_95:                              # %for.body414
                                        #   Parent Loop BB46_23 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rsi,8), %rdi
	movq	8(%rdi), %r15
	testq	%r15, %r15
	je	.LBB46_100
# BB#96:                                # %land.lhs.true425
                                        #   in Loop: Header=BB46_95 Depth=2
	movl	(%r15), %ebx
	movq	%rbx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB46_100
# BB#97:                                # %land.lhs.true439
                                        #   in Loop: Header=BB46_95 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	144(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rbx
	jne	.LBB46_100
# BB#98:                                # %land.lhs.true454
                                        #   in Loop: Header=BB46_95 Depth=2
	movq	16(%rdi), %rcx
	cmpq	$0, 8(%rcx)
	je	.LBB46_99
	.align	16, 0x90
.LBB46_100:                             # %for.inc482
                                        #   in Loop: Header=BB46_95 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB46_95
.LBB46_101:                             #   in Loop: Header=BB46_23 Depth=1
	movq	%r11, %r15
.LBB46_102:                             # %if.end485
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r9, %rbx
	jmp	.LBB46_103
.LBB46_93:                              #   in Loop: Header=BB46_23 Depth=1
	movq	%rsi, %r15
	jmp	.LBB46_103
.LBB46_90:                              #   in Loop: Header=BB46_23 Depth=1
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	movq	%rsi, %r15
.LBB46_103:                             # %if.end485
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	(%r15), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$51, %r8
	je	.LBB46_105
# BB#104:                               # %if.end485
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$39, %rax
	je	.LBB46_105
# BB#112:                               # %if.else568
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB46_114
# BB#113:                               # %land.lhs.true570
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	49(%r12), %eax
	testq	$4, %rax
	je	.LBB46_117
.LBB46_114:                             # %lor.lhs.false577
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	17(%r12), %eax
	testq	$1, %rax
	jne	.LBB46_117
# BB#115:                               # %lor.lhs.false583
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	flag_float_store(%rip), %eax
	testq	%rax, %rax
	je	.LBB46_119
# BB#116:                               # %land.lhs.true585
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r12), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB46_119
.LBB46_117:                             # %lor.lhs.false594
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB46_119
# BB#118:                               # %lor.lhs.false594
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r12
	je	.LBB46_119
# BB#209:                               # %if.else1188
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, %r13
	cmpq	%r8, 144(%rsp)          # 8-byte Folded Reload
	je	.LBB46_210
# BB#211:                               # %if.then1191
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	validize_mem
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	push_to_sequence
	movq	8(%r12), %rax
	movl	16(%rax), %edx
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert_to_mode
	movq	%rax, %r15
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	testq	%rdi, %rdi
	je	.LBB46_213
# BB#212:                               # %if.then1208
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r14, %rsi
	callq	adjust_address_1
	movq	%rax, %rbx
.LBB46_213:                             # %if.end1210
                                        #   in Loop: Header=BB46_23 Depth=1
	callq	get_insns
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	end_sequence
	jmp	.LBB46_214
.LBB46_105:                             # %if.then493
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r10, %r13
	cmpq	$39, %rax
	je	.LBB46_107
# BB#106:                               # %if.then493
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$61, %rax
	jne	.LBB46_220
.LBB46_107:                             # %if.then503
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r12), %rdi
	callq	int_size_in_bytes
	movl	target_flags(%rip), %r14d
	movq	$23, %rcx
	shrq	%cl, %r14
	movq	%r14, %rcx
	andq	$4, %rcx
	addq	%rax, %rcx
	notq	%r14
	movabsq	$4294967299, %rax       # imm = 0x100000003
	addq	%rax, %rcx
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	orq	%rax, %r14
	andq	%rcx, %r14
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rax, %r14
	testq	%rbx, %rbx
	jne	.LBB46_109
# BB#108:                               # %if.then522
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	$32, %rcx
	movq	%r14, %rsi
	movq	cfun(%rip), %rax
	movzbl	2(%r15), %edi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rcx
	callq	assign_stack_local_1
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%rax, %r13
	movq	%r12, %rsi
	callq	set_mem_attributes
.LBB46_109:                             # %if.end540
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	(%r15), %eax
	cmpq	$39, %rax
	jne	.LBB46_111
# BB#110:                               # %if.then545
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r13, %rbx
	movq	%rbx, %rdi
	callq	validize_mem
	movq	8(%r12), %rdi
	movq	%rax, %r14
	callq	int_size_in_bytes
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	emit_group_store
	jmp	.LBB46_220
.LBB46_119:                             # %if.then599
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	8(%r12), %rdi
	movl	16(%rdi), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 220(%rsp)
	xorq	%rcx, %rcx
	movq	136(%rsp), %rsi         # 8-byte Reload
	leaq	220(%rsp), %rdx
	callq	promote_mode
	movq	%rax, %rdi
	movq	%rax, %r13
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	mark_user_reg
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB46_121
# BB#120:                               # %if.then611
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movl	60(%rax), %edi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	set_mem_attributes
	movq	%rbx, 144(%r12)
	jmp	.LBB46_124
.LBB46_111:                             # %if.else551
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	$32, %rcx
	movl	8(%r15), %eax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r13, %r15
	movq	%r15, %rdi
	callq	validize_mem
	movq	%rax, %r13
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	%r14, %rax
	cqto
	idivq	%rsi
	movq	%rax, %rbx
	movq	8(%r12), %rdi
	callq	int_size_in_bytes
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	move_block_from_reg
	movq	%r15, %rbx
	jmp	.LBB46_220
.LBB46_121:                             # %if.else622
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r14, 144(%r12)
	testq	%r14, %r14
	movq	%r14, %rdi
	jne	.LBB46_123
# BB#122:                               # %cond.false631
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rdi
.LBB46_123:                             # %cond.end634
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r12, %rsi
	callq	maybe_set_unchanging
.LBB46_124:                             # %if.end636
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rax, %rcx
	movabsq	$0, %rax
	jne	.LBB46_126
# BB#125:                               # %if.end636
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rax
.LBB46_126:                             # %if.end636
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r13
	movq	144(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %r13
	jne	.LBB46_128
# BB#127:                               # %if.end636
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rax, %rax
	je	.LBB46_128
# BB#136:                               # %if.else723
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, %rdi
	callq	validize_mem
	movq	%r14, %r13
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	xorq	%r15, %r15
	jmp	.LBB46_137
.LBB46_128:                             # %if.then642
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	2(%r15), %edi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	validize_mem
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	push_to_sequence
	movl	220(%rsp), %edx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	convert_to_mode
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB46_135
# BB#129:                               # %land.lhs.true654
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	$16, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rsi
	jne	.LBB46_135
# BB#130:                               # %land.lhs.true660
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%rax), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB46_135
# BB#131:                               # %land.lhs.true660
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	144(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, 80(%rsp)          # 8-byte Folded Reload
	jne	.LBB46_135
# BB#132:                               # %land.lhs.true671
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	2(%r15), %edi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	%rdi, %rsi
	jne	.LBB46_135
# BB#133:                               # %land.lhs.true683
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	80(%rsp), %rcx          # 8-byte Reload
	movzbl	mode_size(%rcx), %ecx
	movzbl	mode_size(%rsi), %esi
	cmpq	%rsi, %rcx
	jae	.LBB46_135
# BB#134:                               # %if.then698
                                        #   in Loop: Header=BB46_23 Depth=1
	orq	$268435456, %rdx        # imm = 0x10000000
	movl	%edx, (%rax)
	movl	220(%rsp), %ecx
	andq	$1, %rcx
	shlq	$26, %rcx
	movabsq	$4227858431, %rsi       # imm = 0xFBFFFFFF
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%rax)
.LBB46_135:                             # %if.end707
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	16(%r12), %ebx
	andq	$65536, %rbx            # imm = 0x10000
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	make_tree
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	expand_assignment
	movl	16(%r12), %eax
	movabsq	$4294901759, %rcx       # imm = 0xFFFEFFFF
	andq	%rcx, %rax
	orq	%rbx, %rax
	movl	%eax, 16(%r12)
	callq	get_insns
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	end_sequence
	movq	$1, %r15
	movq	%r14, %r13
.LBB46_137:                             # %if.end726
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB46_138
# BB#139:                               # %land.lhs.true728
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r12), %rax
	movl	60(%rax), %ebx
	movq	$9, %rcx
	shrq	%cl, %rbx
	andq	$127, %rbx
	cmpq	$51, %rbx
	jne	.LBB46_141
# BB#140:                               #   in Loop: Header=BB46_23 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB46_166
.LBB46_138:                             #   in Loop: Header=BB46_23 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB46_166
.LBB46_210:                             #   in Loop: Header=BB46_23 Depth=1
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	%r13, %rbx
.LBB46_214:                             # %if.end1212
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	%rbx, %r15
	je	.LBB46_220
# BB#215:                               # %if.then1215
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rbx, %rbx
	jne	.LBB46_217
# BB#216:                               # %if.then1218
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	2(%r15), %edi
	movzbl	mode_size(%rdi), %esi
	movq	cfun(%rip), %rcx
	xorq	%rdx, %rdx
	callq	assign_stack_local_1
	movq	%rax, %rbx
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	set_mem_attributes
.LBB46_217:                             # %if.end1229
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 144(%rsp)         # 8-byte Folded Reload
	je	.LBB46_219
# BB#218:                               # %if.then1232
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	push_to_sequence
	movq	%rbx, %rdi
	callq	validize_mem
	movq	%rbx, %r14
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	validize_mem
	movq	%rbx, %rdi
	movq	%r14, %rbx
	movq	%rax, %rsi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	end_sequence
	jmp	.LBB46_220
.LBB46_141:                             # %land.lhs.true738
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	144(%r12), %rax
	testq	%rax, %rax
	jne	.LBB46_143
# BB#142:                               # %cond.false752
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_143:                             # %cond.end755
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	2(%rax), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rax, %rbx
	jne	.LBB46_152
# BB#144:                               # %lor.lhs.false762
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB46_147
# BB#145:                               # %land.lhs.true764
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	49(%r12), %eax
	testq	$4, %rax
	jne	.LBB46_147
# BB#146:                               #   in Loop: Header=BB46_23 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB46_166
.LBB46_219:                             # %if.else1237
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, %rdi
	callq	validize_mem
	movq	%rbx, %r14
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	validize_mem
	movq	%rbx, %rdi
	movq	%r14, %rbx
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB46_220:                             # %if.end1242
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rbx, 144(%r12)
	movq	48(%rsp), %r15          # 8-byte Reload
	jmp	.LBB46_221
.LBB46_147:                             # %lor.lhs.false772
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	17(%r12), %eax
	testq	$1, %rax
	jne	.LBB46_148
# BB#149:                               # %lor.lhs.false779
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	flag_float_store(%rip), %eax
	testq	%rax, %rax
	je	.LBB46_152
# BB#150:                               # %land.lhs.true781
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r12), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB46_152
# BB#151:                               #   in Loop: Header=BB46_23 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB46_166
.LBB46_152:                             # %if.then790
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r12), %rax
	movl	60(%rax), %edi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
	movq	%r13, %rdi
	callq	mark_user_reg
	movq	144(%r12), %rsi
	movl	(%r13), %ebx
	testq	%rsi, %rsi
	jne	.LBB46_154
# BB#153:                               # %cond.false808
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rsi
.LBB46_154:                             # %cond.end811
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	(%rsi), %edi
	xorq	%rdi, %rbx
	testq	$16711680, %rbx         # imm = 0xFF0000
	jne	.LBB46_155
# BB#162:                               # %if.else873
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rsi, %rsi
	jne	.LBB46_164
# BB#163:                               # %cond.false880
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rsi
.LBB46_164:                             # %cond.end883
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r13, %rdi
	callq	emit_move_insn
	jmp	.LBB46_165
.LBB46_99:                              # %if.then468
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, 160(%r12)
	jmp	.LBB46_102
.LBB46_155:                             # %if.then818
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rsi, %rsi
	jne	.LBB46_157
# BB#156:                               # %cond.false826
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
	movl	(%rax), %edi
.LBB46_157:                             # %cond.end829
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	8(%r12), %rcx
	movl	16(%rcx), %r14d
	movq	%rax, %r15
	movq	$13, %rcx
	shrq	%cl, %r14
	andq	$1, %r14
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	push_to_sequence
	movq	144(%r12), %rsi
	testq	%rsi, %rsi
	jne	.LBB46_159
# BB#158:                               # %cond.false848
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rsi
.LBB46_159:                             # %cond.end851
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, %rdi
	callq	emit_move_insn
	movzbl	2(%r13), %edi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	convert_to_mode
	testq	%rax, %rax
	movq	%rax, 144(%r12)
	jne	.LBB46_161
# BB#160:                               # %cond.false866
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_161:                             # %cond.end869
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	end_sequence
	movq	$1, %r15
.LBB46_165:                             # %if.end886
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r13, 144(%r12)
	xorq	%rdi, %rdi
	jmp	.LBB46_166
.LBB46_148:                             #   in Loop: Header=BB46_23 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
.LBB46_166:                             # %if.end889
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$65, %rcx
	jne	.LBB46_170
# BB#167:                               # %if.then894
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r13), %rcx
	movq	16(%r13), %rdx
	movl	8(%rcx), %esi
	movl	8(%rdx), %ecx
	cmpq	%rcx, %rsi
	ja	.LBB46_169
# BB#168:                               # %if.then894
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rcx, %rsi
.LBB46_169:                             # %if.then894
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	jmp	.LBB46_171
.LBB46_170:                             # %if.else925
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movl	8(%r13), %esi
.LBB46_171:                             # %if.end929
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	cfun(%rip), %rdx
	movl	304(%rdx), %r14d
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rsi, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	%r14, %rcx
	jb	.LBB46_173
# BB#172:                               # %if.then933
                                        #   in Loop: Header=BB46_23 Depth=1
	incq	%rsi
	movl	%esi, 304(%rdx)
	movq	312(%rdx), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %r15
	andq	%r15, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	cfun(%rip), %rcx
	movl	304(%rcx), %edx
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	%rax, %r13
	leaq	(%r13,%rbx,8), %rdi
	subq	%r14, %rdx
	andq	%r15, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	cfun(%rip), %rax
	movq	%r13, 312(%rax)
	movq	136(%rsp), %r13         # 8-byte Reload
	movl	(%r13), %eax
.LBB46_173:                             # %if.end947
                                        #   in Loop: Header=BB46_23 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$65, %rax
	movq	144(%rsp), %r15         # 8-byte Reload
	jne	.LBB46_177
# BB#174:                               # %if.then952
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r13), %rax
	movzbl	2(%rax), %r14d
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	gen_realpart
	movl	8(%rax), %eax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%r13, 136(%rsp)         # 8-byte Spill
	callq	gen_imagpart
	movl	8(%rax), %r13d
	movq	128(%rsp), %rbx         # 8-byte Reload
	testq	%rbx, %rbx
	jne	.LBB46_175
# BB#176:                               # %if.end993.thread
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rax
	movq	312(%rax), %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	$0, (%rax,%rcx,8)
	movq	cfun(%rip), %rax
	movq	312(%rax), %rax
	movq	$0, (%rax,%r13,8)
	movq	48(%rsp), %r15          # 8-byte Reload
	jmp	.LBB46_205
.LBB46_177:                             # %if.else986
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rax
	movl	8(%r13), %ecx
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	312(%rax), %rax
	movq	128(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, (%rax,%rcx,8)
	xorq	%r14, %r14
	xorq	%rdx, %rdx
	movq	80(%rsp), %rax          # 8-byte Reload
	jmp	.LBB46_178
.LBB46_175:                             # %if.then969
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_realpart
	movq	cfun(%rip), %rcx
	movq	312(%rcx), %rcx
	movq	%r13, 88(%rsp)          # 8-byte Spill
	movq	112(%rsp), %r13         # 8-byte Reload
	movq	%rax, (%rcx,%r13,8)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_imagpart
	movq	cfun(%rip), %rcx
	movq	312(%rcx), %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, (%rcx,%rdx,8)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%r13, %rdx
	movq	88(%rsp), %r14          # 8-byte Reload
.LBB46_178:                             # %if.end993
                                        #   in Loop: Header=BB46_23 Depth=1
	cmpq	%r15, %rax
	movabsq	$0, %rcx
	movq	%rcx, %rax
	jne	.LBB46_180
# BB#179:                               # %if.end993
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rax
.LBB46_180:                             # %if.end993
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	jne	.LBB46_182
# BB#181:                               # %if.end993
                                        #   in Loop: Header=BB46_23 Depth=1
	movabsq	$1, %rcx
.LBB46_182:                             # %if.end993
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rbx, %rbx
	movq	48(%rsp), %r15          # 8-byte Reload
	je	.LBB46_205
# BB#183:                               # %if.end993
                                        #   in Loop: Header=BB46_23 Depth=1
	andq	%rcx, %rax
	je	.LBB46_205
# BB#184:                               # %land.lhs.true1001
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB46_205
# BB#185:                               # %land.lhs.true1001
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	176(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB46_205
# BB#186:                               # %land.lhs.true1010
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	global_rtl+48(%rip), %rdi
	movq	8(%rbx), %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB46_205
# BB#187:                               # %if.then1016
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r14, %r13
	callq	get_last_insn
	movq	136(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %ecx
	movq	%rax, %r14
	cmpq	$65, %rcx
	jne	.LBB46_199
# BB#188:                               # %for.cond1023.preheader
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%r14, %r14
	movq	%r13, %rbx
	je	.LBB46_205
	.align	16, 0x90
.LBB46_189:                             # %for.body1026
                                        #   Parent Loop BB46_23 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB46_198
# BB#190:                               # %cond.true1034
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB46_192
# BB#191:                               # %cond.false1046
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB46_192:                             # %cond.end1054
                                        #   in Loop: Header=BB46_189 Depth=2
	testq	%rax, %rax
	je	.LBB46_198
# BB#193:                               # %land.lhs.true1058
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rdx
	movq	80(%rdx), %rdx
	movq	8(%rax), %rax
	movq	(%rdx,%rbx,8), %rsi
	cmpq	%rsi, %rax
	jne	.LBB46_195
# BB#194:                               # %if.then1066
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	312(%rcx), %rax
	movq	(%rax,%rbx,8), %rdx
	jmp	.LBB46_197
.LBB46_195:                             # %land.lhs.true1080
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	(%rdx,%rsi,8), %rdx
	cmpq	%rdx, %rax
	jne	.LBB46_198
# BB#196:                               # %if.then1090
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	312(%rcx), %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rdx
.LBB46_197:                             # %for.inc1103
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$3, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
	.align	16, 0x90
.LBB46_198:                             # %for.inc1103
                                        #   in Loop: Header=BB46_189 Depth=2
	movq	%r14, %rdi
	callq	prev_nonnote_insn
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB46_189
	jmp	.LBB46_205
.LBB46_199:                             # %if.else1106
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB46_205
# BB#200:                               # %cond.true1114
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB46_202
# BB#201:                               # %cond.false1126
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB46_202:                             # %cond.end1134
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rax, %rax
	je	.LBB46_205
# BB#203:                               # %land.lhs.true1138
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%rax), %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB46_205
# BB#204:                               # %if.then1144
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$3, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
.LBB46_205:                             # %if.end1154
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%r12), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB46_207
# BB#206:                               # %if.then1172
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	8(%rax), %rax
	movl	64(%rax), %esi
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	mark_reg_pointer
.LBB46_207:                             # %if.end1178
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	17(%r12), %eax
	testq	$4, %rax
	je	.LBB46_221
# BB#208:                               # %if.then1185
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	push_to_sequence
	movq	%r12, %rdi
	callq	put_var_into_stack
	callq	get_insns
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	end_sequence
.LBB46_221:                             # %if.end1246
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r12
	jne	.LBB46_225
# BB#222:                               # %if.then1249
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	144(%rax), %rsi
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	96(%rax), %r14
	testq	%rsi, %rsi
	jne	.LBB46_224
# BB#223:                               # %cond.false1259
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rsi
.LBB46_224:                             # %cond.end1262
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	48(%r14), %edi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	set_mem_attributes
	movq	%rbx, 144(%r14)
.LBB46_225:                             # %if.end1273
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	144(%r12), %rax
	testq	%rax, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	jne	.LBB46_227
# BB#226:                               # %cond.false1280
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_227:                             # %cond.end1283
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB46_231
# BB#228:                               # %if.then1289
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rax, %rax
	jne	.LBB46_230
# BB#229:                               # %cond.false1296
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_230:                             # %cond.end1299
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rcx
	jmp	.LBB46_239
.LBB46_231:                             # %if.else1307
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rax, %rax
	jne	.LBB46_233
# BB#232:                               # %cond.false1314
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
	movl	(%rax), %ecx
.LBB46_233:                             # %cond.end1317
                                        #   in Loop: Header=BB46_23 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$65, %rcx
	jne	.LBB46_240
# BB#234:                               # %if.then1323
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rax, %rax
	jne	.LBB46_236
# BB#235:                               # %cond.false1330
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_236:                             # %cond.end1333
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rax
	movq	24(%rcx), %rcx
	movl	8(%rax), %eax
	movq	72(%rcx), %rcx
	movq	%r12, (%rcx,%rax,8)
	movq	144(%r12), %rax
	testq	%rax, %rax
	jne	.LBB46_238
# BB#237:                               # %cond.false1351
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_238:                             # %cond.end1354
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rcx
	movq	16(%rax), %rax
.LBB46_239:                             # %for.inc1368
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	24(%rcx), %rcx
	movl	8(%rax), %eax
	movq	72(%rcx), %rcx
	movq	%r12, (%rcx,%rax,8)
	jmp	.LBB46_240
	.align	16, 0x90
.LBB46_23:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB46_25 Depth 2
                                        #     Child Loop BB46_95 Depth 2
                                        #     Child Loop BB46_189 Depth 2
	movq	8(%r12), %rsi
	movq	104(%r12), %rbx
	testq	%r14, %r14
	movq	%r12, %rax
	jne	.LBB46_25
# BB#24:                                # %lor.lhs.false
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rax
	movzbl	426(%rax), %eax
	xorq	%r13, %r13
	testq	$1, %rax
	movq	%r12, %rax
	je	.LBB46_27
	.align	16, 0x90
.LBB46_25:                              # %for.cond62
                                        #   Parent Loop BB46_23 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	$1, %r13
	je	.LBB46_27
# BB#26:                                # %for.body64
                                        #   in Loop: Header=BB46_25 Depth=2
	xorq	%r13, %r13
	cmpq	$0, 72(%rax)
	je	.LBB46_25
.LBB46_27:                              # %if.end76
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rsi
	je	.LBB46_30
# BB#28:                                # %lor.lhs.false83
                                        #   in Loop: Header=BB46_23 Depth=1
	movzbl	16(%r12), %eax
	cmpq	$35, %rax
	jne	.LBB46_30
# BB#29:                                # %lor.lhs.false83
                                        #   in Loop: Header=BB46_23 Depth=1
	testq	%rbx, %rbx
	je	.LBB46_30
# BB#33:                                # %if.end107
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB46_35
# BB#34:                                # %if.then111
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	cfun(%rip), %rax
	movl	248(%rsp), %ecx
	movl	%ecx, 112(%rax)
	movq	240(%rsp), %rcx
	movq	%rcx, 104(%rax)
	movq	224(%rsp), %rcx
	movq	232(%rsp), %rdx
	movq	%rdx, 96(%rax)
	movq	%rcx, 88(%rax)
.LBB46_35:                              # %if.end112
                                        #   in Loop: Header=BB46_23 Depth=1
	movl	60(%rbx), %eax
	movl	60(%rsi), %edi
	movq	$9, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	jne	.LBB46_39
# BB#36:                                # %if.then124
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	testq	%rax, %rax
	movq	%rax, 144(%r12)
	jne	.LBB46_38
# BB#37:                                # %cond.false133
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_38:                              # %cond.end136
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rax, 160(%r12)
	jmp	.LBB46_240
	.align	16, 0x90
.LBB46_30:                              # %if.then92
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	const_int_rtx+512(%rip), %rsi
	movq	$51, %rdi
	callq	gen_rtx_MEM
	testq	%rax, %rax
	movq	%rax, 144(%r12)
	jne	.LBB46_32
# BB#31:                                # %cond.false
                                        #   in Loop: Header=BB46_23 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB46_32:                              # %cond.end
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	%rax, 160(%r12)
	movl	16(%r12), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r12)
.LBB46_240:                             # %for.inc1368
                                        #   in Loop: Header=BB46_23 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB46_23
# BB#241:                               # %for.cond.for.end1371_crit_edge
	movq	%r15, %r12
	movq	200(%rsp), %r15
	movq	208(%rsp), %r14
	movq	64(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB46_242
.LBB46_21:
	movq	%r15, %r12
	xorq	%r14, %r14
	xorq	%r15, %r15
.LBB46_242:                             # %for.end1371
	callq	emit_insns
	callq	get_last_insn
	movq	cfun(%rip), %rcx
	movq	%rax, 296(%rcx)
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	cfun(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r15, %rax
	jns	.LBB46_244
# BB#243:                               # %for.end1371
	xorq	%rax, %rax
.LBB46_244:                             # %for.end1371
	movl	%eax, 68(%rsi)
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	leaq	-1(%rax,%rdi), %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rdi
	subq	%rdx, %rbx
	movl	%ebx, 68(%rsi)
	testq	%r14, %r14
	je	.LBB46_245
# BB#246:                               # %cond.false1416
	movq	sizetype_tab+8(%rip), %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %r14
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB46_247
.LBB46_245:                             # %cond.true1406
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
.LBB46_247:                             # %cond.end1425
	movq	cfun(%rip), %rcx
	movq	%rax, 80(%rcx)
	movq	cfun(%rip), %rax
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	8(%rdi), %rsi
	movl	68(%rax), %edx
	movq	%rdi, %rbx
	callq	ix86_return_pops_args
	movq	cfun(%rip), %rcx
	movl	%eax, 64(%rcx)
	testq	%r12, %r12
	jne	.LBB46_249
# BB#248:                               # %if.then1432
	movl	248(%rsp), %eax
	movl	%eax, 112(%rcx)
	movq	240(%rsp), %rax
	movq	%rax, 104(%rcx)
	movq	224(%rsp), %rax
	movq	232(%rsp), %rdx
	movq	%rdx, 96(%rcx)
	movq	%rax, 88(%rcx)
.LBB46_249:                             # %if.end1434
	movq	96(%rbx), %rax
	movq	144(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rdx, 120(%rcx)
	je	.LBB46_254
# BB#250:                               # %cond.end1482
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB46_252
# BB#251:                               # %cond.true1488
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	ja	.LBB46_253
	jmp	.LBB46_254
.LBB46_252:                             # %cond.false1494
	movzbl	49(%rax), %ecx
	testq	$4, %rcx
	je	.LBB46_254
.LBB46_253:                             # %if.then1502
	movq	8(%rax), %rdi
	callq	ix86_function_value
	movl	(%rax), %ecx
	orq	$1073741824, %rcx       # imm = 0x40000000
	movl	%ecx, (%rax)
	movq	cfun(%rip), %rcx
	movq	%rax, 120(%rcx)
.LBB46_254:                             # %if.end1511
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp357:
	.size	assign_parms, .Ltmp357-assign_parms
	.cfi_endproc

	.globl	locate_and_pad_parm
	.align	16, 0x90
	.type	locate_and_pad_parm,@function
locate_and_pad_parm:                    # @locate_and_pad_parm
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp364:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp365:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp366:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp367:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp368:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp369:
	.cfi_def_cfa_offset 80
.Ltmp370:
	.cfi_offset %rbx, -48
.Ltmp371:
	.cfi_offset %r12, -40
.Ltmp372:
	.cfi_offset %r13, -32
.Ltmp373:
	.cfi_offset %r14, -24
.Ltmp374:
	.cfi_offset %r15, -16
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%r8, %r13
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	testq	%rbx, %rbx
	je	.LBB47_2
# BB#1:                                 # %cond.true
	movq	%rbx, %rdi
	callq	size_in_bytes
	jmp	.LBB47_3
.LBB47_2:                               # %cond.false
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %edi
	xorq	%rsi, %rsi
	callq	size_int_wide
.LBB47_3:                               # %cond.end
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	ix86_function_arg_boundary
	movq	%rax, %rbx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %r15
	movq	80(%rsp), %rax
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	88(%rsp), %rax
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, %rdx
	jne	.LBB47_12
# BB#4:                                 # %if.then41
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$60, %rcx
	movq	%rsi, %r15
	shrq	%cl, %r15
	andq	$7, %r15
	addq	%rbx, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$35, %rcx
	sarq	%cl, %r15
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	xorq	%r8, %r8
	cmpq	%rsi, %rax
	movabsq	$0, %r9
	jge	.LBB47_6
# BB#5:                                 # %if.then.i
	movq	(%r13), %r8
	movq	8(%r13), %r9
.LBB47_6:                               # %if.end.i
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$9, %rsi
	movq	$0, 8(%rdx)
	movq	$0, (%rdx)
	jl	.LBB47_12
# BB#7:                                 # %if.then8.i
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	8(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB47_10
# BB#8:                                 # %cond.false.i
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	(%r13), %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	round_up
	movq	%rax, 8(%r13)
	movq	$0, (%r13)
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rdx          # 8-byte Reload
	jge	.LBB47_12
# BB#9:                                 # %if.then34.i
	movq	%rdi, %rbx
	movq	$60, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	%rbx, %rdi
	movq	88(%rsp), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB47_12
.LBB47_10:                              # %if.else.i
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%r13), %rcx
	leaq	-1(%r15,%rcx), %rsi
	negq	%r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	%rsi, %r15
	cmpq	%rbx, %rax
	movq	%r15, (%r13)
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jge	.LBB47_12
# BB#11:                                # %if.then55.i
	subq	%r8, %r15
	movq	88(%rsp), %rax
	movq	%r15, (%rax)
.LBB47_12:                              # %if.end42
	movq	(%r13), %rax
	movq	8(%r13), %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, 8(%rdx)
	movq	%rax, (%rdx)
	andq	%rdi, %r12
	movq	%rdi, %rbx
	cmpq	$51, %r12
	je	.LBB47_17
# BB#13:                                # %if.then45
	movzbl	target_flags+3(%rip), %eax
	movq	32(%r14), %rdi
	testq	$2, %rax
	je	.LBB47_15
# BB#14:                                # %cond.true47
	addq	$7, %rdi
	andq	$-8, %rdi
	jmp	.LBB47_16
.LBB47_15:                              # %cond.false50
	incq	%rdi
	andq	$-2, %rdi
.LBB47_16:                              # %cond.end56
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, %r14
.LBB47_17:                              # %land.lhs.true70
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	host_integerp
	movq	%rbx, %r15
	testq	%rbx, %rax
	je	.LBB47_18
# BB#19:                                # %lor.lhs.false
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	tree_low_cst
	movq	$3, %rcx
	movl	target_flags(%rip), %esi
	shlq	%cl, %rax
	movq	$20, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdx, %rcx
	cqto
	idivq	%rcx
	testq	%rdx, %rdx
	jne	.LBB47_20
	jmp	.LBB47_21
.LBB47_18:                              # %land.lhs.true70.if.then79_crit_edge
	movl	target_flags(%rip), %esi
.LBB47_20:                              # %if.then79
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movq	%r14, %rdi
	callq	round_up
	movq	%rax, %r14
.LBB47_21:                              # %do.body
	movq	80(%rsp), %rbx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB47_23
# BB#22:                                # %if.then87
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	tree_low_cst
	addq	%rax, (%rbx)
	jmp	.LBB47_26
.LBB47_23:                              # %if.else91
	movq	8(%rbx), %r15
	movq	sizetype_tab+8(%rip), %rdi
	movq	%r14, %rsi
	callq	convert
	testq	%r15, %r15
	je	.LBB47_25
# BB#24:                                # %if.else98
	movq	$59, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
.LBB47_25:                              # %do.end
	movq	%rax, 8(%rbx)
.LBB47_26:                              # %do.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp375:
	.size	locate_and_pad_parm, .Ltmp375-locate_and_pad_parm
	.cfi_endproc

	.globl	uninitialized_vars_warning
	.align	16, 0x90
	.type	uninitialized_vars_warning,@function
uninitialized_vars_warning:             # @uninitialized_vars_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp382:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp383:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp384:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp385:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp386:
	.cfi_def_cfa_offset 48
.Ltmp387:
	.cfi_offset %rbx, -48
.Ltmp388:
	.cfi_offset %r12, -40
.Ltmp389:
	.cfi_offset %r13, -32
.Ltmp390:
	.cfi_offset %r14, -24
.Ltmp391:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB48_21
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$.L.str4, %r15
	movabsq	$.L.str3, %r12
	.align	16, 0x90
.LBB48_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	warn_uninitialized(%rip), %eax
	testq	%rax, %rax
	je	.LBB48_13
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB48_2 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB48_13
# BB#4:                                 # %land.lhs.true3
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	addq	$-18, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	jb	.LBB48_13
# BB#5:                                 # %land.lhs.true41
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB48_7
# BB#6:                                 # %cond.end
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	testq	%rax, %rax
	je	.LBB48_13
.LBB48_7:                               # %cond.end59
                                        #   in Loop: Header=BB48_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB48_13
# BB#8:                                 # %land.lhs.true64
                                        #   in Loop: Header=BB48_2 Depth=1
	cmpq	$0, 104(%rbx)
	jne	.LBB48_13
# BB#9:                                 # %land.lhs.true67
                                        #   in Loop: Header=BB48_2 Depth=1
	testq	%rax, %rax
	jne	.LBB48_11
# BB#10:                                # %cond.false74
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB48_11:                              # %cond.end77
                                        #   in Loop: Header=BB48_2 Depth=1
	movl	8(%rax), %edi
	callq	regno_uninitialized
	testq	%r13, %rax
	je	.LBB48_13
# BB#12:                                # %if.then
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB48_13:                              # %if.end
                                        #   in Loop: Header=BB48_2 Depth=1
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	je	.LBB48_20
# BB#14:                                # %land.lhs.true81
                                        #   in Loop: Header=BB48_2 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB48_20
# BB#15:                                # %land.lhs.true87
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB48_17
# BB#16:                                # %cond.end97
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	testq	%rax, %rax
	je	.LBB48_20
.LBB48_17:                              # %cond.end110
                                        #   in Loop: Header=BB48_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB48_20
# BB#18:                                # %cond.end125
                                        #   in Loop: Header=BB48_2 Depth=1
	movl	8(%rax), %edi
	callq	regno_clobbered_at_setjmp
	testq	%r13, %rax
	je	.LBB48_20
# BB#19:                                # %if.then132
                                        #   in Loop: Header=BB48_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB48_20:                              # %for.inc
                                        #   in Loop: Header=BB48_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB48_2
.LBB48_21:                              # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB48_23
	.align	16, 0x90
.LBB48_22:                              # %for.body138
                                        #   in Loop: Header=BB48_23 Depth=1
	movq	%rbx, %rdi
	callq	uninitialized_vars_warning
	movq	(%rbx), %rbx
.LBB48_23:                              # %for.body138
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB48_22
# BB#24:                                # %for.end142
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp392:
	.size	uninitialized_vars_warning, .Ltmp392-uninitialized_vars_warning
	.cfi_endproc

	.globl	setjmp_args_warning
	.align	16, 0x90
	.type	setjmp_args_warning,@function
setjmp_args_warning:                    # @setjmp_args_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp397:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp398:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp399:
	.cfi_def_cfa_offset 32
.Ltmp400:
	.cfi_offset %rbx, -32
.Ltmp401:
	.cfi_offset %r14, -24
.Ltmp402:
	.cfi_offset %r15, -16
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB49_8
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$.L.str5, %r14
	.align	16, 0x90
.LBB49_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB49_4
# BB#3:                                 # %cond.end
                                        #   in Loop: Header=BB49_2 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	testq	%rax, %rax
	je	.LBB49_7
.LBB49_4:                               # %cond.end17
                                        #   in Loop: Header=BB49_2 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB49_7
# BB#5:                                 # %cond.end30
                                        #   in Loop: Header=BB49_2 Depth=1
	movl	8(%rax), %edi
	callq	regno_clobbered_at_setjmp
	testq	%r15, %rax
	je	.LBB49_7
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB49_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB49_7:                               # %for.inc
                                        #   in Loop: Header=BB49_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB49_2
.LBB49_8:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp403:
	.size	setjmp_args_warning, .Ltmp403-setjmp_args_warning
	.cfi_endproc

	.globl	setjmp_protect
	.align	16, 0x90
	.type	setjmp_protect,@function
setjmp_protect:                         # @setjmp_protect
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp407:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp408:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp409:
	.cfi_def_cfa_offset 32
.Ltmp410:
	.cfi_offset %rbx, -24
.Ltmp411:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	32(%r14), %rbx
	jmp	.LBB50_1
	.align	16, 0x90
.LBB50_12:                              # %for.inc
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	(%rbx), %rbx
.LBB50_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB50_13
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB50_1 Depth=1
	movl	16(%rbx), %eax
	andq	$254, %rax
	cmpq	$34, %rax
	jne	.LBB50_12
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB50_5
# BB#4:                                 # %cond.end
                                        #   in Loop: Header=BB50_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	testq	%rax, %rax
	je	.LBB50_12
.LBB50_5:                               # %cond.end24
                                        #   in Loop: Header=BB50_1 Depth=1
	movl	(%rax), %ecx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB50_8
# BB#6:                                 # %cond.end39
                                        #   in Loop: Header=BB50_1 Depth=1
	cmpq	$66, %rcx
	jne	.LBB50_12
# BB#7:                                 # %cond.end54
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$70, %rax
	jne	.LBB50_12
.LBB50_8:                               # %land.lhs.true59
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	112(%rbx), %rax
	testq	%rax, %rax
	je	.LBB50_10
# BB#9:                                 # %land.lhs.true59
                                        #   in Loop: Header=BB50_1 Depth=1
	cmpq	%rbx, %rax
	jne	.LBB50_12
.LBB50_10:                              # %land.lhs.true66
                                        #   in Loop: Header=BB50_1 Depth=1
	movzbl	49(%rbx), %eax
	testq	$4, %rax
	jne	.LBB50_12
# BB#11:                                # %if.then
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	%rbx, %rdi
	callq	put_var_into_stack
	jmp	.LBB50_12
.LBB50_13:                              # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB50_15
	.align	16, 0x90
.LBB50_14:                              # %for.body75
                                        #   in Loop: Header=BB50_15 Depth=1
	movq	%rbx, %rdi
	callq	setjmp_protect
	movq	(%rbx), %rbx
.LBB50_15:                              # %for.body75
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB50_14
# BB#16:                                # %for.end79
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp412:
	.size	setjmp_protect, .Ltmp412-setjmp_protect
	.cfi_endproc

	.globl	setjmp_protect_args
	.align	16, 0x90
	.type	setjmp_protect_args,@function
setjmp_protect_args:                    # @setjmp_protect_args
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp415:
	.cfi_def_cfa_offset 16
.Ltmp416:
	.cfi_offset %rbx, -16
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rbx
	jmp	.LBB51_1
	.align	16, 0x90
.LBB51_10:                              # %for.inc
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	(%rbx), %rbx
.LBB51_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB51_11
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB51_1 Depth=1
	movl	16(%rbx), %eax
	andq	$254, %rax
	cmpq	$34, %rax
	jne	.LBB51_10
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB51_5
# BB#4:                                 # %cond.end
                                        #   in Loop: Header=BB51_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	testq	%rax, %rax
	je	.LBB51_10
.LBB51_5:                               # %cond.end24
                                        #   in Loop: Header=BB51_1 Depth=1
	movl	(%rax), %ecx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB51_8
# BB#6:                                 # %cond.end39
                                        #   in Loop: Header=BB51_1 Depth=1
	cmpq	$66, %rcx
	jne	.LBB51_10
# BB#7:                                 # %cond.end54
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$70, %rax
	jne	.LBB51_10
.LBB51_8:                               # %land.lhs.true59
                                        #   in Loop: Header=BB51_1 Depth=1
	movzbl	49(%rbx), %eax
	testq	$4, %rax
	jne	.LBB51_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB51_1 Depth=1
	movq	%rbx, %rdi
	callq	put_var_into_stack
	jmp	.LBB51_10
.LBB51_11:                              # %for.end
	popq	%rbx
	retq
.Ltmp417:
	.size	setjmp_protect_args, .Ltmp417-setjmp_protect_args
	.cfi_endproc

	.globl	lookup_static_chain
	.align	16, 0x90
	.type	lookup_static_chain,@function
lookup_static_chain:                    # @lookup_static_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp420:
	.cfi_def_cfa_offset 16
.Ltmp421:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	decl_function_context
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB52_12
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rbx), %edx
	cmpq	$30, %rdx
	jne	.LBB52_3
# BB#2:                                 # %land.lhs.true
	movzbl	49(%rbx), %edx
	testq	$4, %rdx
	jne	.LBB52_12
.LBB52_3:                               # %if.end
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB52_5
# BB#4:                                 # %if.end
	movq	inline_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB52_5
# BB#6:                                 # %if.end9
	movq	cfun(%rip), %rcx
	movq	272(%rcx), %rcx
	jmp	.LBB52_7
	.align	16, 0x90
.LBB52_9:                               # %for.cond
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	(%rcx), %rcx
.LBB52_7:                               # %if.end9
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB52_8
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	24(%rcx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB52_9
# BB#11:                                # %if.then12
	movq	32(%rcx), %rax
	movq	40(%rax), %rcx
	jmp	.LBB52_12
.LBB52_5:                               # %if.then8
	movq	global_rtl+56(%rip), %rcx
.LBB52_12:                              # %return
	movq	%rcx, %rax
	popq	%rbx
	retq
.LBB52_8:                               # %for.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.lookup_static_chain, %rcx
	movq	$5612, %rsi             # imm = 0x15EC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp422:
	.size	lookup_static_chain, .Ltmp422-lookup_static_chain
	.cfi_endproc

	.globl	fix_lexical_addr
	.align	16, 0x90
	.type	fix_lexical_addr,@function
fix_lexical_addr:                       # @fix_lexical_addr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp427:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp428:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp429:
	.cfi_def_cfa_offset 32
.Ltmp430:
	.cfi_offset %rbx, -32
.Ltmp431:
	.cfi_offset %r14, -24
.Ltmp432:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	decl_function_context
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB53_26
# BB#1:                                 # %entry
	movq	inline_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB53_26
# BB#2:                                 # %if.end
	movq	outer_function_chain(%rip), %rdi
	jmp	.LBB53_3
	.align	16, 0x90
.LBB53_6:                               # %for.cond.i
                                        #   in Loop: Header=BB53_3 Depth=1
	movq	56(%rdi), %rdi
.LBB53_3:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB53_4
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB53_3 Depth=1
	movq	48(%rdi), %rcx
	cmpq	%rax, %rcx
	jne	.LBB53_6
# BB#8:                                 # %find_function_data.exit
	movl	(%rbx), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$70, %rdx
	jne	.LBB53_11
# BB#9:                                 # %land.lhs.true
	movq	8(%rbx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$66, %rsi
	jne	.LBB53_11
# BB#10:                                # %if.then7
	movq	8(%rdx), %rbx
	movl	(%rbx), %ecx
.LBB53_11:                              # %if.end14
	xorq	%r14, %r14
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB53_15
# BB#12:                                # %if.end14
	cmpq	$75, %rcx
	jne	.LBB53_27
# BB#13:                                # %land.lhs.true22
	movq	16(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB53_27
# BB#14:                                # %if.then29
	movq	8(%rbx), %rbx
	movq	8(%rcx), %r14
.LBB53_15:                              # %if.end40
	movq	128(%rdi), %rcx
	cmpq	%rcx, %rbx
	je	.LBB53_16
# BB#17:                                # %if.else56
	movq	global_rtl+56(%rip), %rcx
	cmpq	%rcx, %rbx
	jne	.LBB53_24
# BB#18:                                # %if.then58
	movq	cfun(%rip), %rcx
	movq	272(%rcx), %rcx
	jmp	.LBB53_20
	.align	16, 0x90
.LBB53_19:                              # %for.cond
                                        #   in Loop: Header=BB53_20 Depth=1
	movq	(%rcx), %rcx
.LBB53_20:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB53_24
# BB#21:                                # %for.body
                                        #   in Loop: Header=BB53_20 Depth=1
	movq	24(%rcx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB53_19
# BB#22:                                # %if.then61
	movq	32(%rcx), %rax
	movq	40(%rax), %rdi
	jmp	.LBB53_23
.LBB53_26:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB53_16:                              # %if.then42
	callq	get_arg_pointer_save_area
	movq	8(%rax), %rdi
	movq	%r15, %rsi
	callq	fix_lexical_addr
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, %rsi
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
	movq	%rax, %rdi
.LBB53_23:                              # %if.end66
	testq	%rdi, %rdi
	je	.LBB53_24
# BB#25:                                # %if.end69
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	plus_constant_wide  # TAILCALL
.LBB53_4:                               # %for.end.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_function_data, %rcx
	movq	$329, %rsi              # imm = 0x149
	jmp	.LBB53_5
.LBB53_27:                              # %if.else38
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fix_lexical_addr, %rcx
	movq	$5645, %rsi             # imm = 0x160D
	jmp	.LBB53_5
.LBB53_24:                              # %if.then68
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fix_lexical_addr, %rcx
	movq	$5691, %rsi             # imm = 0x163B
.LBB53_5:                               # %for.end.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp433:
	.size	fix_lexical_addr, .Ltmp433-fix_lexical_addr
	.cfi_endproc

	.globl	get_arg_pointer_save_area
	.align	16, 0x90
	.type	get_arg_pointer_save_area,@function
get_arg_pointer_save_area:              # @get_arg_pointer_save_area
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp437:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp438:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp439:
	.cfi_def_cfa_offset 32
.Ltmp440:
	.cfi_offset %rbx, -24
.Ltmp441:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	248(%rbx), %r14
	testq	%r14, %r14
	jne	.LBB54_2
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	movzbl	mode_size+4(%rdi), %esi
	orq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rcx
	callq	assign_stack_local_1
	movq	%rax, %r14
	movq	%r14, 248(%rbx)
.LBB54_2:                               # %if.end
	movq	cfun(%rip), %rax
	cmpq	%rbx, %rax
	jne	.LBB54_5
# BB#3:                                 # %land.lhs.true
	movzbl	426(%rbx), %eax
	testq	$128, %rax
	jne	.LBB54_5
# BB#4:                                 # %if.then8
	callq	start_sequence
	movq	%r14, %rdi
	callq	validize_mem
	movq	global_rtl+48(%rip), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	callq	push_topmost_sequence
	callq	get_insns
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_insn_after
	callq	pop_topmost_sequence
.LBB54_5:                               # %if.end14
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp442:
	.size	get_arg_pointer_save_area, .Ltmp442-get_arg_pointer_save_area
	.cfi_endproc

	.globl	trampoline_address
	.align	16, 0x90
	.type	trampoline_address,@function
trampoline_address:                     # @trampoline_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp448:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp449:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp450:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp451:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp452:
	.cfi_def_cfa_offset 48
.Ltmp453:
	.cfi_offset %rbx, -40
.Ltmp454:
	.cfi_offset %r12, -32
.Ltmp455:
	.cfi_offset %r14, -24
.Ltmp456:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	280(%rax), %rax
	movq	%rdi, %r15
	jmp	.LBB55_1
	.align	16, 0x90
.LBB55_3:                               # %for.cond
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	(%rax), %rax
.LBB55_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB55_4
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	24(%rax), %rcx
	cmpq	%r15, %rcx
	jne	.LBB55_3
# BB#7:                                 # %if.then
	movq	32(%rax), %rax
	movq	40(%rax), %rax
	movl	target_flags(%rip), %edi
	movq	8(%rax), %r14
	jmp	.LBB55_8
.LBB55_4:                               # %for.cond3.preheader
	movq	outer_function_chain(%rip), %rax
	jmp	.LBB55_5
	.align	16, 0x90
.LBB55_13:                              # %for.inc29
                                        #   in Loop: Header=BB55_5 Depth=1
	movq	56(%rax), %rax
.LBB55_5:                               # %for.cond3.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB55_10 Depth 2
	testq	%rax, %rax
	je	.LBB55_14
# BB#6:                                 # %for.body5
                                        #   in Loop: Header=BB55_5 Depth=1
	movq	280(%rax), %rcx
	jmp	.LBB55_10
	.align	16, 0x90
.LBB55_9:                               # %for.cond7
                                        #   in Loop: Header=BB55_10 Depth=2
	movq	(%rcx), %rcx
.LBB55_10:                              # %for.cond7
                                        #   Parent Loop BB55_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	je	.LBB55_13
# BB#11:                                # %for.body9
                                        #   in Loop: Header=BB55_10 Depth=2
	movq	24(%rcx), %rdx
	cmpq	%r15, %rdx
	jne	.LBB55_9
# BB#12:                                # %if.then13
	movq	32(%rcx), %rax
	movq	40(%rax), %rax
	movq	8(%rax), %rdi
	jmp	.LBB55_29
.LBB55_14:                              # %for.end30
	movq	%r15, %rdi
	callq	decl_function_context
	movq	current_function_decl(%rip), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	je	.LBB55_21
# BB#15:                                # %for.end30
	movq	inline_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB55_21
# BB#16:                                # %if.then34
	movq	outer_function_chain(%rip), %rbx
	jmp	.LBB55_17
	.align	16, 0x90
.LBB55_19:                              # %for.cond.i
                                        #   in Loop: Header=BB55_17 Depth=1
	movq	56(%rbx), %rbx
.LBB55_17:                              # %if.then34
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB55_18
# BB#20:                                # %for.body.i
                                        #   in Loop: Header=BB55_17 Depth=1
	movq	48(%rbx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB55_19
.LBB55_21:                              # %if.end36
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	movq	$24, %rsi
	jne	.LBB55_23
# BB#22:                                # %if.end36
	movq	$11, %rsi
.LBB55_23:                              # %if.end36
	testq	%rbx, %rbx
	movq	%rbx, %rcx
	jne	.LBB55_25
# BB#24:                                # %if.end36
	movq	cfun(%rip), %rcx
.LBB55_25:                              # %if.end36
	movq	$51, %rdi
	xorq	%rdx, %rdx
	callq	assign_stack_local_1
	movq	%rax, %r14
	movq	$120, %rdi
	callq	make_node
	testq	%rbx, %rbx
	movq	%r14, 40(%rax)
	je	.LBB55_27
# BB#26:                                # %if.then43
	movq	280(%rbx), %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, 280(%rbx)
	jmp	.LBB55_28
.LBB55_27:                              # %if.else
	movq	cfun(%rip), %rcx
	movq	280(%rcx), %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	cfun(%rip), %rcx
	movq	%rax, 280(%rcx)
.LBB55_28:                              # %if.end58
	movq	8(%r14), %rdi
.LBB55_29:                              # %if.end58
	movq	%r15, %rsi
	callq	fix_lexical_addr
	movl	target_flags(%rip), %edi
	movq	%rax, %r14
.LBB55_8:                               # %if.then
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$-2, %rsi
	callq	gen_rtx_CONST_INT
	movl	target_flags(%rip), %edi
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	expand_simple_binop
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$83, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	expand_simple_binop
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB55_18:                              # %for.end.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_function_data, %rcx
	movq	$329, %rsi              # imm = 0x149
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp457:
	.size	trampoline_address, .Ltmp457-trampoline_address
	.cfi_endproc

	.globl	identify_blocks
	.align	16, 0x90
	.type	identify_blocks,@function
identify_blocks:                        # @identify_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp463:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp464:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp465:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp466:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp467:
	.cfi_def_cfa_offset 48
.Ltmp468:
	.cfi_offset %rbx, -40
.Ltmp469:
	.cfi_offset %r12, -32
.Ltmp470:
	.cfi_offset %r14, -24
.Ltmp471:
	.cfi_offset %r15, -16
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %r14
	testq	%r14, %r14
	je	.LBB56_1
# BB#2:                                 # %if.end
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	all_blocks
	movq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$3, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	all_blocks
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	callq	get_insns
	leaq	8(%r12), %rsi
	leaq	(%r12,%r15,8), %rdx
	movq	%rax, %rdi
	movq	%rbx, %rcx
	callq	identify_blocks_1
	movq	%r12, %rdi
	callq	free
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB56_1:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp472:
	.size	identify_blocks, .Ltmp472-identify_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	identify_blocks_1,@function
identify_blocks_1:                      # @identify_blocks_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp479:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp480:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp481:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp482:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp483:
	.cfi_def_cfa_offset 48
.Ltmp484:
	.cfi_offset %rbx, -48
.Ltmp485:
	.cfi_offset %r12, -40
.Ltmp486:
	.cfi_offset %r13, -32
.Ltmp487:
	.cfi_offset %r14, -24
.Ltmp488:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB57_21
# BB#1:
	movq	%r14, %r12
	.align	16, 0x90
.LBB57_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB57_3
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	32(%rbx), %r13
	movzbl	(%r13), %eax
	cmpq	$141, %rax
	jne	.LBB57_18
# BB#14:                                # %if.then31
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	8(%r13), %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	identify_blocks_1
	movq	16(%r13), %rdi
	movq	%rax, %rsi
	testq	%rdi, %rdi
	je	.LBB57_16
# BB#15:                                # %if.then42
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	identify_blocks_1
	movq	%rax, %rsi
.LBB57_16:                              # %if.end47
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	24(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB57_18
# BB#17:                                # %if.then52
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	identify_blocks_1
	movq	%rax, %rsi
	jmp	.LBB57_18
	.align	16, 0x90
.LBB57_3:                               # %for.body
                                        #   in Loop: Header=BB57_2 Depth=1
	cmpq	$37, %rax
	jne	.LBB57_18
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB57_2 Depth=1
	movl	40(%rbx), %eax
	movabsq	$4294967199, %rcx       # imm = 0xFFFFFF9F
	cmpq	%rcx, %rax
	jne	.LBB57_5
# BB#10:                                # %if.then12
                                        #   in Loop: Header=BB57_2 Depth=1
	cmpq	%r14, %r12
	je	.LBB57_11
# BB#12:                                # %if.end15
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	-8(%r12), %rax
	addq	$-8, %r12
	movq	%rax, 32(%rbx)
	jmp	.LBB57_18
.LBB57_5:                               # %if.then
                                        #   in Loop: Header=BB57_2 Depth=1
	movabsq	$4294967198, %rcx       # imm = 0xFFFFFF9E
	cmpq	%rcx, %rax
	jne	.LBB57_18
# BB#6:                                 # %if.then2
                                        #   in Loop: Header=BB57_2 Depth=1
	cmpq	%r15, %rsi
	je	.LBB57_7
# BB#9:                                 # %if.end
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	(%rsi), %rax
	addq	$8, %rsi
	movq	%rax, 32(%rbx)
	movq	%rax, (%r12)
	addq	$8, %r12
	.align	16, 0x90
.LBB57_18:                              # %for.inc
                                        #   in Loop: Header=BB57_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB57_2
# BB#19:                                # %for.end
	cmpq	%r14, %r12
	jne	.LBB57_20
.LBB57_21:                              # %if.end65
	movq	%rsi, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB57_11:                              # %if.then14
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.identify_blocks_1, %rcx
	movq	$5887, %rsi             # imm = 0x16FF
	jmp	.LBB57_8
.LBB57_7:                               # %if.then4
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.identify_blocks_1, %rcx
	movq	$5876, %rsi             # imm = 0x16F4
	jmp	.LBB57_8
.LBB57_20:                              # %if.then64
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.identify_blocks_1, %rcx
	movq	$5911, %rsi             # imm = 0x1717
.LBB57_8:                               # %if.then4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp489:
	.size	identify_blocks_1, .Ltmp489-identify_blocks_1
	.cfi_endproc

	.globl	reorder_blocks
	.align	16, 0x90
	.type	reorder_blocks,@function
reorder_blocks:                         # @reorder_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp492:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp493:
	.cfi_def_cfa_offset 32
.Ltmp494:
	.cfi_offset %rbx, -16
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB58_6
# BB#1:                                 # %if.end
	movabsq	$.L.str6, %rdx
	movq	$10, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, 8(%rsp)
	movq	%rbx, %rdi
	callq	reorder_blocks_0
	movq	$0, 40(%rbx)
	movq	$0, (%rbx)
	callq	get_insns
	leaq	8(%rsp), %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	reorder_blocks_1
	movq	40(%rbx), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB58_2
	.align	16, 0x90
.LBB58_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rax, (%rdx)
	movq	%rdx, %rax
	jne	.LBB58_3
	jmp	.LBB58_4
.LBB58_2:
	xorq	%rdx, %rdx
.LBB58_4:                               # %blocks_nreverse.exit
	movq	%rdx, 40(%rbx)
	movq	%rbx, %rdi
	callq	reorder_fix_fragments
	movq	8(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB58_6
# BB#5:                                 # %if.then8
	callq	free
	movq	$0, 8(%rsp)
.LBB58_6:                               # %do.end
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp495:
	.size	reorder_blocks, .Ltmp495-reorder_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	reorder_blocks_0,@function
reorder_blocks_0:                       # @reorder_blocks_0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp499:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp500:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp501:
	.cfi_def_cfa_offset 32
.Ltmp502:
	.cfi_offset %rbx, -24
.Ltmp503:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB59_3
# BB#1:
	movabsq	$4294950911, %r14       # imm = 0xFFFFBFFF
	.align	16, 0x90
.LBB59_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	andq	%r14, %rax
	movl	%eax, 16(%rbx)
	movq	40(%rbx), %rdi
	callq	reorder_blocks_0
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB59_2
.LBB59_3:                               # %while.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp504:
	.size	reorder_blocks_0, .Ltmp504-reorder_blocks_0
	.cfi_endproc

	.align	16, 0x90
	.type	reorder_blocks_1,@function
reorder_blocks_1:                       # @reorder_blocks_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp511:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp512:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp513:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp514:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp515:
	.cfi_def_cfa_offset 48
.Ltmp516:
	.cfi_offset %rbx, -48
.Ltmp517:
	.cfi_offset %r12, -40
.Ltmp518:
	.cfi_offset %r13, -32
.Ltmp519:
	.cfi_offset %r14, -24
.Ltmp520:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB60_23
# BB#1:
	movabsq	$4294967199, %r13       # imm = 0xFFFFFF9F
	.align	16, 0x90
.LBB60_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_14 Depth 2
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB60_3
# BB#16:                                # %land.lhs.true
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	32(%rbx), %r12
	movzbl	(%r12), %eax
	cmpq	$141, %rax
	jne	.LBB60_21
# BB#17:                                # %if.then72
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	8(%r12), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	reorder_blocks_1
	movq	16(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB60_19
# BB#18:                                # %if.then83
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	reorder_blocks_1
.LBB60_19:                              # %if.end87
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	24(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB60_21
# BB#20:                                # %if.then92
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	reorder_blocks_1
	jmp	.LBB60_21
	.align	16, 0x90
.LBB60_3:                               # %for.body
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$37, %rax
	jne	.LBB60_21
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB60_2 Depth=1
	movl	40(%rbx), %eax
	cmpq	%r13, %rax
	jne	.LBB60_5
# BB#13:                                # %if.then45
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rax
	movq	%rax, 32(%rbx)
	movq	(%r14), %rax
	decq	8(%rax)
	movq	40(%r15), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB60_15
	.align	16, 0x90
.LBB60_14:                              # %for.body.i
                                        #   Parent Loop BB60_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rax, (%rcx)
	movq	%rcx, %rax
	movq	%rdx, %rcx
	jne	.LBB60_14
.LBB60_15:                              # %blocks_nreverse.exit
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%rax, 40(%r15)
	movq	48(%r15), %r12
	jmp	.LBB60_22
.LBB60_5:                               # %if.then
                                        #   in Loop: Header=BB60_2 Depth=1
	movabsq	$4294967198, %rcx       # imm = 0xFFFFFF9E
	cmpq	%rcx, %rax
	jne	.LBB60_21
# BB#6:                                 # %if.then2
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	32(%rbx), %r12
	movl	16(%r12), %ecx
	testq	$16384, %rcx            # imm = 0x4000
	je	.LBB60_10
# BB#7:                                 # %if.then8
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%r12, %rdi
	callq	copy_node
	movq	64(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB60_9
# BB#8:                                 # %if.then8
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%rcx, %r12
.LBB60_9:                               # %if.then8
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%r12, 64(%rax)
	movq	72(%r12), %rcx
	movq	%rcx, 72(%rax)
	movq	%rax, 72(%r12)
	movq	%rax, 32(%rbx)
	movl	16(%rax), %ecx
	movq	%rax, %r12
.LBB60_10:                              # %if.end
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	$0, 40(%r12)
	orq	$16384, %rcx            # imm = 0x4000
	movl	%ecx, 16(%r12)
	movq	%r15, 48(%r12)
	movq	40(%r15), %rax
	movq	%rax, (%r12)
	movq	%r12, 40(%r15)
	movq	(%r14), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB60_12
# BB#11:                                # %if.then35
                                        #   in Loop: Header=BB60_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%r14)
	movq	8(%rax), %rcx
.LBB60_12:                              # %if.end38
                                        #   in Loop: Header=BB60_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
	jmp	.LBB60_22
	.align	16, 0x90
.LBB60_21:                              # %for.inc
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%r15, %r12
.LBB60_22:                              # %for.inc
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r15
	jne	.LBB60_2
.LBB60_23:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp521:
	.size	reorder_blocks_1, .Ltmp521-reorder_blocks_1
	.cfi_endproc

	.align	16, 0x90
	.type	reorder_fix_fragments,@function
reorder_fix_fragments:                  # @reorder_fix_fragments
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp524:
	.cfi_def_cfa_offset 16
.Ltmp525:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	jmp	.LBB61_1
	.align	16, 0x90
.LBB61_13:                              # %if.end44
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	40(%rbx), %rdi
	callq	reorder_fix_fragments
	movq	(%rbx), %rbx
.LBB61_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB61_4 Depth 2
                                        #     Child Loop BB61_7 Depth 2
	testq	%rbx, %rbx
	je	.LBB61_14
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	64(%rbx), %rcx
	testq	%rcx, %rcx
	movq	%rbx, %rax
	je	.LBB61_6
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB61_1 Depth=1
	movzbl	17(%rcx), %eax
	testq	$64, %rax
	jne	.LBB61_13
	.align	16, 0x90
.LBB61_4:                               # %while.cond6
                                        #   Parent Loop BB61_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	72(%rcx), %rcx
	movzbl	17(%rcx), %eax
	testq	$64, %rax
	je	.LBB61_4
# BB#5:                                 # %if.end21
                                        #   in Loop: Header=BB61_1 Depth=1
	testq	%rcx, %rcx
	movq	$0, 64(%rcx)
	movq	%rcx, %rax
	je	.LBB61_13
	.align	16, 0x90
.LBB61_6:                               # %if.then23
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	72(%rax), %rcx
	leaq	72(%rax), %rdx
	jmp	.LBB61_7
	.align	16, 0x90
.LBB61_9:                               # %while.body28.if.end40_crit_edge
                                        #   in Loop: Header=BB61_7 Depth=2
	addq	$72, %rcx
	movq	(%rcx), %rcx
.LBB61_7:                               # %if.then23
                                        #   Parent Loop BB61_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	je	.LBB61_12
# BB#8:                                 # %while.body28
                                        #   in Loop: Header=BB61_7 Depth=2
	movzbl	17(%rcx), %esi
	testq	$64, %rsi
	je	.LBB61_9
# BB#10:                                # %if.then35
                                        #   in Loop: Header=BB61_7 Depth=2
	movq	%rax, 64(%rcx)
	movq	%rcx, (%rdx)
	addq	$72, %rcx
	movq	%rcx, %rdx
	movq	(%rcx), %rcx
	jmp	.LBB61_7
	.align	16, 0x90
.LBB61_12:                              # %while.end43
                                        #   in Loop: Header=BB61_1 Depth=1
	movq	$0, (%rdx)
	jmp	.LBB61_13
.LBB61_14:                              # %while.end48
	popq	%rbx
	retq
.Ltmp526:
	.size	reorder_fix_fragments, .Ltmp526-reorder_fix_fragments
	.cfi_endproc

	.globl	number_blocks
	.align	16, 0x90
	.type	number_blocks,@function
number_blocks:                          # @number_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp532:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp533:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp534:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp535:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp536:
	.cfi_def_cfa_offset 48
.Ltmp537:
	.cfi_offset %rbx, -40
.Ltmp538:
	.cfi_offset %r12, -32
.Ltmp539:
	.cfi_offset %r14, -24
.Ltmp540:
	.cfi_offset %r15, -16
	movq	104(%rdi), %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	all_blocks
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$3, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	all_blocks
	cmpq	$2, %r12
	jl	.LBB62_3
# BB#1:
	movq	$2, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %r14
	.align	16, 0x90
.LBB62_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	next_block_index(%rip), %esi
	leaq	1(%rsi), %rcx
	movq	-8(%rbx,%rax,8), %rdi
	movl	%ecx, next_block_index(%rip)
	movl	24(%rdi), %edx
	movq	$2, %rcx
	shlq	%cl, %rsi
	andq	$3, %rdx
	orq	%rsi, %rdx
	movl	%edx, 24(%rdi)
	movq	%rax, %rcx
	andq	%r8, %rcx
	incq	%rax
	cmpq	%r14, %rcx
	jne	.LBB62_2
.LBB62_3:                               # %for.end
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp541:
	.size	number_blocks, .Ltmp541-number_blocks
	.cfi_endproc

	.globl	debug_find_var_in_block_tree
	.align	16, 0x90
	.type	debug_find_var_in_block_tree,@function
debug_find_var_in_block_tree:           # @debug_find_var_in_block_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp545:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp546:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp547:
	.cfi_def_cfa_offset 32
.Ltmp548:
	.cfi_offset %rbx, -24
.Ltmp549:
	.cfi_offset %r14, -16
	movq	%rsi, %rax
	movq	32(%rax), %rcx
	movq	%rdi, %r14
	jmp	.LBB63_1
	.align	16, 0x90
.LBB63_5:                               # %for.cond
                                        #   in Loop: Header=BB63_1 Depth=1
	movq	(%rcx), %rcx
.LBB63_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB63_2
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB63_1 Depth=1
	cmpq	%r14, %rcx
	jne	.LBB63_5
	jmp	.LBB63_9
.LBB63_2:                               # %for.end
	movq	40(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB63_3
	.align	16, 0x90
.LBB63_8:                               # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	debug_find_var_in_block_tree
	testq	%rax, %rax
	jne	.LBB63_9
# BB#6:                                 # %for.cond3
                                        #   in Loop: Header=BB63_8 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB63_8
# BB#7:
	xorq	%rax, %rax
	jmp	.LBB63_9
.LBB63_3:
	xorq	%rax, %rax
.LBB63_9:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp550:
	.size	debug_find_var_in_block_tree, .Ltmp550-debug_find_var_in_block_tree
	.cfi_endproc

	.align	16, 0x90
	.type	prepare_function_start,@function
prepare_function_start:                 # @prepare_function_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp553:
	.cfi_def_cfa_offset 16
.Ltmp554:
	.cfi_offset %rbx, -16
	movq	$432, %rdi              # imm = 0x1B0
	callq	ggc_alloc_cleared
	movq	%rax, cfun(%rip)
	callq	init_stmt_for_function
	callq	init_eh_for_function
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	jne	.LBB64_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB64_3
.LBB64_1:
	movabsq	$0, %rax
.LBB64_3:                               # %entry
	movq	cfun(%rip), %rcx
	movl	%eax, cse_not_expected(%rip)
	xorq	%rbx, %rbx
	movl	%ebx, caller_save_needed(%rip)
	movq	$0, 216(%rcx)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294967039, %rdx       # imm = 0xFFFFFEFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294966783, %rdx       # imm = 0xFFFFFDFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 168(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 184(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 160(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 176(%rax)
	movq	cfun(%rip), %rax
	movl	%ebx, 152(%rax)
	movq	$58, %rcx
	movl	%ecx, 304(%rax)
	callq	init_emit
	callq	init_expr
	movq	cfun(%rip), %rdi
	movq	$0, reg_renumber(%rip)
	callq	init_varasm_status
	movq	cfun(%rip), %rax
	movl	%ebx, 352(%rax)
	movq	$0, 368(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 360(%rax)
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	movq	cfun(%rip), %rdx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movl	%eax, 400(%rdx)
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movl	%eax, 404(%rdx)
	movl	424(%rdx), %eax
	movabsq	$4294967279, %rcx       # imm = 0xFFFFFFEF
	andq	%rax, %rcx
	movl	%ecx, 424(%rdx)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294967263, %rdx       # imm = 0xFFFFFFDF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294967231, %rdx       # imm = 0xFFFFFFBF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294966271, %rdx       # imm = 0xFFFFFBFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movl	%ebx, current_function_is_leaf(%rip)
	movq	cfun(%rip), %rax
	movl	%ebx, current_function_nothrow(%rip)
	movl	%ebx, current_function_sp_is_unchanging(%rip)
	movl	%ebx, current_function_uses_only_leaf_regs(%rip)
	movl	424(%rax), %ecx
	movabsq	$4294965247, %rdx       # imm = 0xFFFFF7FF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294963199, %rdx       # imm = 0xFFFFEFFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294967293, %rdx       # imm = 0xFFFFFFFD
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 416(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4293918719, %rdx       # imm = 0xFFEFFFFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4292870143, %rdx       # imm = 0xFFDFFFFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 136(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 232(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 248(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 264(%rax)
	movq	$0, 208(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 224(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 320(%rax)
	movq	cfun(%rip), %rax
	movl	%ebx, 328(%rax)
	movl	%ebx, 332(%rax)
	movl	%ebx, 336(%rax)
	movq	$1, %rcx
	movl	%ecx, rtx_equal_function_value_matters(%rip)
	movl	%ebx, virtuals_instantiated(%rip)
	movl	%ecx, generating_concat_p(%rip)
	movl	%ebx, frame_pointer_needed(%rip)
	movl	424(%rax), %ecx
	movabsq	$4294901759, %rdx       # imm = 0xFFFEFFFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	movabsq	$4294836223, %rdx       # imm = 0xFFFDFFFF
	andq	%rcx, %rdx
	movl	%edx, 424(%rax)
	movq	cfun(%rip), %rax
	movq	$0, 280(%rax)
	callq	init_pending_stack_adjust
	movq	cfun(%rip), %rdi
	movq	16(%rdi), %rax
	movl	%ebx, 4(%rax)
	movl	%ebx, 76(%rdi)
	movq	init_lang_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB64_5
# BB#4:                                 # %if.then
	callq	*%rax
.LBB64_5:                               # %if.end
	movq	init_machine_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB64_6
# BB#7:                                 # %if.then33
	movq	cfun(%rip), %rdi
	popq	%rbx
	jmpq	*%rax  # TAILCALL
.LBB64_6:                               # %if.end34
	popq	%rbx
	retq
.Ltmp555:
	.size	prepare_function_start, .Ltmp555-prepare_function_start
	.cfi_endproc

	.globl	init_function_start
	.align	16, 0x90
	.type	init_function_start,@function
init_function_start:                    # @init_function_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp560:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp561:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp562:
	.cfi_def_cfa_offset 32
.Ltmp563:
	.cfi_offset %rbx, -32
.Ltmp564:
	.cfi_offset %r14, -24
.Ltmp565:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	prepare_function_start
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	*decl_printable_name(%rip)
	movq	cfun(%rip), %rcx
	movq	%rax, 40(%rcx)
	movq	cfun(%rip), %rax
	movq	%rbx, 48(%rax)
	movq	current_function_decl(%rip), %rdi
	callq	decl_function_context
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB65_4
# BB#1:                                 # %land.rhs
	movq	current_function_decl(%rip), %rax
	movzbl	49(%rax), %eax
	testq	$4, %rax
	jne	.LBB65_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB65_4
.LBB65_2:
	movabsq	$0, %rdx
.LBB65_4:                               # %land.end
	movq	cfun(%rip), %rax
	movl	424(%rax), %esi
	movq	$3, %rcx
	shlq	%cl, %rdx
	movabsq	$4294967287, %rcx       # imm = 0xFFFFFFF7
	andq	%rsi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 424(%rax)
	movl	immediate_size_expand(%rip), %eax
	incq	%rax
	movl	%eax, immediate_size_expand(%rip)
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB65_6
# BB#5:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_line_note
.LBB65_6:                               # %if.end
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	96(%rbx), %rdi
	callq	aggregate_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB65_8
# BB#7:                                 # %if.then11
	movq	cfun(%rip), %rax
	movl	424(%rax), %edx
	orq	$1, %rdx
	movl	%edx, 424(%rax)
.LBB65_8:                               # %if.end15
	movl	warn_aggregate_return(%rip), %eax
	testq	%rax, %rax
	je	.LBB65_11
# BB#9:                                 # %land.lhs.true
	movq	96(%rbx), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	addq	$-18, %rax
	andq	%rcx, %rax
	cmpq	$4, %rax
	ja	.LBB65_11
# BB#10:                                # %if.then62
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB65_11:                              # %if.end63
	movq	96(%rbx), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %eax
	andq	$253, %rax
	cmpq	$13, %rax
	jne	.LBB65_12
# BB#13:                                # %if.end63
	movabsq	$1, %rax
	jmp	.LBB65_14
.LBB65_12:
	movabsq	$0, %rax
.LBB65_14:                              # %if.end63
	movq	cfun(%rip), %rdx
	movl	424(%rdx), %esi
	movq	$2, %rcx
	shlq	%cl, %rax
	movabsq	$4294967291, %rcx       # imm = 0xFFFFFFFB
	andq	%rsi, %rcx
	orq	%rax, %rcx
	movl	%ecx, 424(%rdx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp566:
	.size	init_function_start, .Ltmp566-init_function_start
	.cfi_endproc

	.globl	init_function_for_compilation
	.align	16, 0x90
	.type	init_function_for_compilation,@function
init_function_for_compilation:          # @init_function_for_compilation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp568:
	.cfi_def_cfa_offset 16
	movq	prologue(%rip), %rdi
	movq	$0, reg_renumber(%rip)
	xorq	%rsi, %rsi
	callq	varray_grow
	movq	epilogue(%rip), %rdi
	movq	%rax, prologue(%rip)
	xorq	%rsi, %rsi
	callq	varray_grow
	movq	sibcall_epilogue(%rip), %rdi
	movq	%rax, epilogue(%rip)
	xorq	%rsi, %rsi
	callq	varray_grow
	movq	%rax, sibcall_epilogue(%rip)
	popq	%rax
	retq
.Ltmp569:
	.size	init_function_for_compilation, .Ltmp569-init_function_for_compilation
	.cfi_endproc

	.globl	mark_varargs
	.align	16, 0x90
	.type	mark_varargs,@function
mark_varargs:                           # @mark_varargs
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 424(%rax)
	retq
.Ltmp570:
	.size	mark_varargs, .Ltmp570-mark_varargs
	.cfi_endproc

	.globl	expand_main_function
	.align	16, 0x90
	.type	expand_main_function,@function
expand_main_function:                   # @expand_main_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp575:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp576:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp577:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp578:
	.cfi_def_cfa_offset 48
.Ltmp579:
	.cfi_offset %rbx, -32
.Ltmp580:
	.cfi_offset %r14, -24
.Ltmp581:
	.cfi_offset %r15, -16
	movl	target_flags(%rip), %eax
	movslq	ix86_preferred_stack_boundary(%rip), %r15
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	addq	$32, %rdx
	cmpq	%rdx, %r15
	jle	.LBB68_10
# BB#1:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB68_10
# BB#2:                                 # %if.then
	callq	start_sequence
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	global_rtl+16(%rip), %r14
	movq	$60, %rcx
	movq	%r15, %rsi
	shrq	%cl, %rsi
	andq	$7, %rsi
	addq	%r15, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$35, %rcx
	sarq	%cl, %rsi
	negq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	global_rtl+16(%rip), %r8
	movq	$83, %rsi
	movq	$1, %r9
	movq	$2, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	global_rtl+16(%rip), %rdi
	cmpq	%rdi, %rax
	je	.LBB68_4
# BB#3:                                 # %if.then10
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB68_4:                               # %if.end
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	const_int_rtx+512(%rip), %rsi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	force_reg
	xorq	%rsi, %rsi
	movq	$128, %rdx
	movq	%rax, %rdi
	callq	allocate_dynamic_stack_space
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	callq	get_last_insn
	testq	%rax, %rax
	je	.LBB68_9
# BB#5:
	movabsq	$4294967209, %rcx       # imm = 0xFFFFFFA9
	.align	16, 0x90
.LBB68_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %edx
	cmpq	$37, %rdx
	jne	.LBB68_8
# BB#7:                                 # %land.lhs.true22
                                        #   in Loop: Header=BB68_6 Depth=1
	movl	40(%rax), %edx
	cmpq	%rcx, %rdx
	je	.LBB68_11
.LBB68_8:                               # %for.inc
                                        #   in Loop: Header=BB68_6 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB68_6
.LBB68_9:                               # %if.else
	movq	%rbx, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.LBB68_10:                              # %if.end34
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB68_11:                              # %if.then30
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_insn_before  # TAILCALL
.Ltmp582:
	.size	expand_main_function, .Ltmp582-expand_main_function
	.cfi_endproc

	.globl	expand_pending_sizes
	.align	16, 0x90
	.type	expand_pending_sizes,@function
expand_pending_sizes:                   # @expand_pending_sizes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp585:
	.cfi_def_cfa_offset 16
.Ltmp586:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	jmp	.LBB69_2
	.align	16, 0x90
.LBB69_1:                               # %for.body
                                        #   in Loop: Header=BB69_2 Depth=1
	movq	32(%rbx), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	callq	emit_queue
	movq	(%rbx), %rbx
.LBB69_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB69_1
# BB#3:                                 # %for.end
	popq	%rbx
	retq
.Ltmp587:
	.size	expand_pending_sizes, .Ltmp587-expand_pending_sizes
	.cfi_endproc

	.globl	expand_function_start
	.align	16, 0x90
	.type	expand_function_start,@function
expand_function_start:                  # @expand_function_start
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp593:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp594:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp595:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp596:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp597:
	.cfi_def_cfa_offset 64
.Ltmp598:
	.cfi_offset %rbx, -40
.Ltmp599:
	.cfi_offset %r12, -32
.Ltmp600:
	.cfi_offset %r14, -24
.Ltmp601:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r12
	callq	init_recog_no_volatile
	movl	flag_instrument_function_entry_exit(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB70_4
# BB#1:                                 # %land.rhs
	movzbl	51(%r12), %ecx
	testq	$2, %rcx
	jne	.LBB70_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB70_4
.LBB70_2:
	movabsq	$0, %rdx
.LBB70_4:                               # %land.end
	movq	cfun(%rip), %rsi
	movl	424(%rsi), %edi
	movq	$13, %rcx
	shlq	%cl, %rdx
	movabsq	$4294959103, %rcx       # imm = 0xFFFFDFFF
	andq	%rdi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 424(%rsi)
	movl	profile_flag(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB70_8
# BB#5:                                 # %land.rhs5
	movzbl	51(%r12), %eax
	testq	$2, %rax
	jne	.LBB70_6
# BB#7:                                 # %land.rhs5
	movabsq	$1, %rax
	jmp	.LBB70_8
.LBB70_6:
	movabsq	$0, %rax
.LBB70_8:                               # %land.end14
	movq	cfun(%rip), %rdx
	movl	424(%rdx), %esi
	movq	$14, %rcx
	shlq	%cl, %rax
	movabsq	$4294950911, %rcx       # imm = 0xFFFFBFFF
	andq	%rsi, %rcx
	orq	%rax, %rcx
	movl	%ecx, 424(%rdx)
	xorq	%r14, %r14
	cmpq	$0, stack_limit_rtx(%rip)
	movabsq	$0, %rax
	je	.LBB70_12
# BB#9:                                 # %land.rhs21
	movzbl	51(%r12), %eax
	testq	$16, %rax
	jne	.LBB70_10
# BB#11:                                # %land.rhs21
	movabsq	$1, %rax
	jmp	.LBB70_12
.LBB70_10:
	movabsq	$0, %rax
.LBB70_12:                              # %land.end29
	movq	cfun(%rip), %rdx
	movl	424(%rdx), %esi
	movq	$15, %rcx
	shlq	%cl, %rax
	movabsq	$4294934527, %rcx       # imm = 0xFFFF7FFF
	andq	%rsi, %rcx
	orq	%rax, %rcx
	movl	%ecx, 424(%rdx)
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %ecx
	testq	$8, %rcx
	je	.LBB70_15
# BB#13:                                # %if.then
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	movzbl	mode_size+4(%rdi), %esi
	orq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rcx
	callq	assign_stack_local_1
	movq	static_chain_incoming_rtx(%rip), %rsi
	movzbl	(%rsi), %ecx
	movq	%rax, %r14
	cmpq	$61, %rcx
	jne	.LBB70_15
# BB#14:                                # %if.then48
	movq	%r14, %rdi
	callq	emit_move_insn
.LBB70_15:                              # %if.end50
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r15
	je	.LBB70_17
# BB#16:                                # %if.then52
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, 192(%rcx)
	jmp	.LBB70_18
.LBB70_17:                              # %if.else
	movq	cfun(%rip), %rax
	movq	$0, 192(%rax)
.LBB70_18:                              # %if.end55
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, 200(%rcx)
	movq	96(%r12), %rdi
	callq	aggregate_value_p
	testq	%rbx, %rax
	je	.LBB70_22
# BB#19:                                # %if.then60
	cmpq	$0, struct_value_incoming_rtx(%rip)
	je	.LBB70_27
# BB#20:                                # %if.end68
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	struct_value_incoming_rtx(%rip), %rsi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	emit_move_insn
	testq	%rbx, %rbx
	je	.LBB70_27
# BB#21:                                # %if.then70
	movq	96(%r12), %rax
	movzbl	48(%rax), %edi
	movq	%rbx, %rsi
	callq	gen_rtx_MEM
	movq	96(%r12), %rsi
	movq	%rax, %rbx
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	set_mem_attributes
	movq	96(%r12), %rax
	movq	%rbx, 144(%rax)
	jmp	.LBB70_27
.LBB70_22:                              # %if.else84
	movq	96(%r12), %rax
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB70_23
# BB#24:                                # %if.else99
	movq	8(%rax), %rdi
	movq	$1, %rdx
	movq	%r12, %rsi
	callq	hard_function_value
	movl	(%rax), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB70_25
# BB#26:                                # %if.else112
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	96(%r12), %rcx
	movq	%rax, 144(%rcx)
	orq	$1024, 48(%rcx)         # imm = 0x400
	jmp	.LBB70_27
.LBB70_23:                              # %if.then94
	movq	$0, 144(%rax)
	jmp	.LBB70_27
.LBB70_25:                              # %if.then107
	movq	96(%r12), %rcx
	movq	%rax, 144(%rcx)
.LBB70_27:                              # %if.end129
	movq	%r12, %rdi
	callq	assign_parms
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$8, %rax
	je	.LBB70_30
# BB#28:                                # %if.then135
	movq	static_chain_incoming_rtx(%rip), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	je	.LBB70_30
# BB#29:                                # %if.then140
	movq	%r14, %rdi
	callq	emit_move_insn
.LBB70_30:                              # %if.end143
	movabsq	$4294967209, %rsi       # imm = 0xFFFFFFA9
	xorq	%rdi, %rdi
	callq	emit_note
	callq	get_last_insn
	movzbl	(%rax), %eax
	cmpq	$37, %rax
	je	.LBB70_32
# BB#31:                                # %if.then150
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
.LBB70_32:                              # %if.end152
	callq	get_last_insn
	movq	cfun(%rip), %rcx
	movq	%rax, 288(%rcx)
	movq	cfun(%rip), %rax
	movq	$0, 272(%rax)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	testq	$8, %rcx
	je	.LBB70_41
# BB#33:                                # %if.then159
	movq	current_function_decl(%rip), %rdi
	callq	decl_function_context
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB70_40
# BB#34:                                # %if.then162
	movq	static_chain_incoming_rtx(%rip), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB70_36
# BB#35:                                # %if.then167
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	static_chain_incoming_rtx(%rip), %rdi
.LBB70_36:                              # %while.body.lr.ph
	callq	copy_to_reg
	movq	%rax, %r14
	jmp	.LBB70_37
	.align	16, 0x90
.LBB70_39:                              # %if.then201
                                        #   in Loop: Header=BB70_37 Depth=1
	movq	cfun(%rip), %rax
	movq	208(%rax), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	cfun(%rip), %rcx
	movq	%rax, 208(%rcx)
.LBB70_37:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$120, %rdi
	callq	make_node
	movq	%r14, 40(%rax)
	movq	cfun(%rip), %rcx
	movq	272(%rcx), %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	cfun(%rip), %rcx
	movq	%rax, 272(%rcx)
	movq	%r15, %rdi
	callq	decl_function_context
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB70_40
# BB#38:                                # %if.end182
                                        #   in Loop: Header=BB70_37 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movzbl	mode_size+4(%rax), %esi
	negq	%rsi
	movq	%r14, %rdi
	callq	plus_constant_wide
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	callq	get_frame_alias_set
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
	movq	%rbx, %rdi
	callq	copy_to_reg
	movl	optimize(%rip), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	jne	.LBB70_37
	jmp	.LBB70_39
.LBB70_40:                              # %if.end205.loopexit
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
.LBB70_41:                              # %if.end205
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB70_46
# BB#42:                                # %if.then211
	movq	current_function_decl(%rip), %rdi
	movq	144(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB70_44
# BB#43:                                # %cond.false
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	144(%rax), %rax
.LBB70_44:                              # %cond.end
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB70_52
# BB#45:                                # %if.then224
	movq	8(%rax), %r14
	movl	target_flags(%rip), %ebx
	movq	libfunc_table+760(%rip), %r15
	movq	$25, %rcx
	movq	global_rtl+32(%rip), %rdx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$53, %rdi
	xorq	%rsi, %rsi
	callq	expand_builtin_return_addr
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, (%rsp)
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	%rdi, 8(%rsp)
	movq	%r15, %rdi
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	emit_library_call
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
.LBB70_46:                              # %if.end235
	testq	$16384, %rcx            # imm = 0x4000
	je	.LBB70_48
# BB#47:                                # %if.then241
	movl	profile_label_no(%rip), %ecx
	leaq	1(%rcx), %rdx
	movl	%edx, profile_label_no(%rip)
	movl	%ecx, 388(%rax)
.LBB70_48:                              # %if.end242
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	cfun(%rip), %rcx
	movq	%rax, 240(%rcx)
	callq	get_pending_sizes
	movq	%rax, %rdi
	callq	nreverse
	movq	%rax, %rbx
	jmp	.LBB70_50
	.align	16, 0x90
.LBB70_49:                              # %for.body.i
                                        #   in Loop: Header=BB70_50 Depth=1
	movq	32(%rbx), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	callq	emit_queue
	movq	(%rbx), %rbx
.LBB70_50:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB70_49
# BB#51:                                # %expand_pending_sizes.exit
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	force_next_line_note  # TAILCALL
.LBB70_52:                              # %if.else226
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_function_start, %rcx
	movq	$6689, %rsi             # imm = 0x1A21
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp602:
	.size	expand_function_start, .Ltmp602-expand_function_start
	.cfi_endproc

	.globl	expand_dummy_function_end
	.align	16, 0x90
	.type	expand_dummy_function_end,@function
expand_dummy_function_end:              # @expand_dummy_function_end
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp606:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp607:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp608:
	.cfi_def_cfa_offset 32
.Ltmp609:
	.cfi_offset %rbx, -24
.Ltmp610:
	.cfi_offset %r14, -16
	callq	in_sequence_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB71_2
	.align	16, 0x90
.LBB71_1:                               # %while.body
                                        #   in Loop: Header=BB71_2 Depth=1
	callq	end_sequence
	callq	in_sequence_p
.LBB71_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rax
	jne	.LBB71_1
# BB#3:                                 # %while.end
	movq	free_lang_status(%rip), %rax
	movq	cfun(%rip), %rbx
	testq	%rax, %rax
	je	.LBB71_5
# BB#4:                                 # %if.then.i
	movq	%rbx, %rdi
	callq	*%rax
.LBB71_5:                               # %free_after_parsing.exit
	movq	%rbx, %rdi
	callq	free_stmt_status
	movq	cfun(%rip), %rbx
	movq	%rbx, %rdi
	callq	free_eh_status
	movq	%rbx, %rdi
	callq	free_expr_status
	movq	%rbx, %rdi
	callq	free_emit_status
	movq	%rbx, %rdi
	callq	free_varasm_status
	movq	free_machine_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB71_7
# BB#6:                                 # %if.then.i2
	movq	%rbx, %rdi
	callq	*%rax
.LBB71_7:                               # %if.end.i
	movq	312(%rbx), %rdi
	leaq	312(%rbx), %r14
	testq	%rdi, %rdi
	je	.LBB71_9
# BB#8:                                 # %if.then2.i
	callq	free
.LBB71_9:                               # %free_after_compilation.exit
	movq	$0, 80(%rbx)
	movq	$0, 344(%rbx)
	movq	$0, 416(%rbx)
	movq	$0, 128(%rbx)
	movq	$0, 120(%rbx)
	movq	$0, 256(%rbx)
	movq	$0, 248(%rbx)
	movq	$0, 240(%rbx)
	movq	$0, 232(%rbx)
	movq	$0, 224(%rbx)
	movq	$0, 216(%rbx)
	movq	$0, 208(%rbx)
	movq	$0, 200(%rbx)
	movq	$0, 192(%rbx)
	movq	$0, 184(%rbx)
	movq	$0, 176(%rbx)
	movq	$0, 168(%rbx)
	movq	$0, 160(%rbx)
	movq	$0, 296(%rbx)
	movq	$0, 288(%rbx)
	movq	$0, 280(%rbx)
	movq	$0, 272(%rbx)
	movq	$0, 8(%r14)
	movq	$0, (%r14)
	movq	$0, 376(%rbx)
	movq	$0, 368(%rbx)
	movq	$0, 360(%rbx)
	movq	$0, cfun(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp611:
	.size	expand_dummy_function_end, .Ltmp611-expand_dummy_function_end
	.cfi_endproc

	.globl	diddle_return_value
	.align	16, 0x90
	.type	diddle_return_value,@function
diddle_return_value:                    # @diddle_return_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp617:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp618:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp619:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp620:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp621:
	.cfi_def_cfa_offset 48
.Ltmp622:
	.cfi_offset %rbx, -40
.Ltmp623:
	.cfi_offset %r12, -32
.Ltmp624:
	.cfi_offset %r14, -24
.Ltmp625:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	120(%rax), %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r12, %r12
	je	.LBB72_9
# BB#1:                                 # %if.end
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB72_2
# BB#10:                                # %if.then1
	movq	%r12, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmpq	*%r14  # TAILCALL
.LBB72_2:                               # %if.end
	cmpq	$39, %rax
	jne	.LBB72_9
# BB#3:                                 # %for.cond.preheader
	movq	8(%r12), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB72_9
# BB#4:
	movq	$1, %rbx
	.align	16, 0x90
.LBB72_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rcx
	movq	8(%rcx), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$61, %rcx
	jne	.LBB72_8
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB72_5 Depth=1
	movl	8(%rdi), %ecx
	cmpq	$52, %rcx
	ja	.LBB72_8
# BB#7:                                 # %if.then19
                                        #   in Loop: Header=BB72_5 Depth=1
	movq	%r15, %rsi
	callq	*%r14
	movq	8(%r12), %rax
.LBB72_8:                               # %for.inc
                                        #   in Loop: Header=BB72_5 Depth=1
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB72_5
.LBB72_9:                               # %if.end22
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp626:
	.size	diddle_return_value, .Ltmp626-diddle_return_value
	.cfi_endproc

	.globl	clobber_return_register
	.align	16, 0x90
	.type	clobber_return_register,@function
clobber_return_register:                # @clobber_return_register
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp630:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp631:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp632:
	.cfi_def_cfa_offset 32
.Ltmp633:
	.cfi_offset %rbx, -24
.Ltmp634:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	120(%rax), %r14
	testq	%r14, %r14
	je	.LBB73_9
# BB#1:                                 # %if.end.i
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB73_2
# BB#13:                                # %if.then1.i
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB73_9
.LBB73_2:                               # %if.end.i
	cmpq	$39, %rax
	jne	.LBB73_9
# BB#3:                                 # %for.cond.preheader.i
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB73_9
# BB#4:
	movq	$1, %rbx
	.align	16, 0x90
.LBB73_5:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rcx
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB73_8
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB73_5 Depth=1
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	ja	.LBB73_8
# BB#7:                                 # %if.then19.i
                                        #   in Loop: Header=BB73_5 Depth=1
	movq	$49, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	8(%r14), %rax
.LBB73_8:                               # %for.inc.i
                                        #   in Loop: Header=BB73_5 Depth=1
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB73_5
.LBB73_9:                               # %diddle_return_value.exit
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rax
	movq	144(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB73_12
# BB#10:                                # %cond.end
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	jne	.LBB73_12
# BB#11:                                # %land.lhs.true
	movl	8(%rdx), %eax
	cmpq	$53, %rax
	jb	.LBB73_12
# BB#14:                                # %if.then12
	movq	$49, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_insn  # TAILCALL
.LBB73_12:                              # %if.end13
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp635:
	.size	clobber_return_register, .Ltmp635-clobber_return_register
	.cfi_endproc

	.globl	use_return_register
	.align	16, 0x90
	.type	use_return_register,@function
use_return_register:                    # @use_return_register
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp639:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp640:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp641:
	.cfi_def_cfa_offset 32
.Ltmp642:
	.cfi_offset %rbx, -24
.Ltmp643:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	120(%rax), %r14
	testq	%r14, %r14
	je	.LBB74_9
# BB#1:                                 # %if.end.i
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB74_2
# BB#10:                                # %if.then1.i
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	emit_insn  # TAILCALL
.LBB74_2:                               # %if.end.i
	cmpq	$39, %rax
	jne	.LBB74_9
# BB#3:                                 # %for.cond.preheader.i
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB74_9
# BB#4:
	movq	$1, %rbx
	.align	16, 0x90
.LBB74_5:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rcx
	movq	8(%rcx), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB74_8
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB74_5 Depth=1
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	ja	.LBB74_8
# BB#7:                                 # %if.then19.i
                                        #   in Loop: Header=BB74_5 Depth=1
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	8(%r14), %rax
.LBB74_8:                               # %for.inc.i
                                        #   in Loop: Header=BB74_5 Depth=1
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB74_5
.LBB74_9:                               # %diddle_return_value.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp644:
	.size	use_return_register, .Ltmp644-use_return_register
	.cfi_endproc

	.globl	expand_function_end
	.align	16, 0x90
	.type	expand_function_end,@function
expand_function_end:                    # @expand_function_end
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp651:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp652:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp653:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp654:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp655:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp656:
	.cfi_def_cfa_offset 112
.Ltmp657:
	.cfi_offset %rbx, -48
.Ltmp658:
	.cfi_offset %r12, -40
.Ltmp659:
	.cfi_offset %r13, -32
.Ltmp660:
	.cfi_offset %r14, -24
.Ltmp661:
	.cfi_offset %r15, -16
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	callq	finish_expr_for_function
	movq	cfun(%rip), %rdi
	cmpq	$0, 248(%rdi)
	je	.LBB75_3
# BB#1:                                 # %land.lhs.true
	movzbl	426(%rdi), %eax
	testq	$128, %rax
	jne	.LBB75_3
# BB#2:                                 # %if.then
	callq	get_arg_pointer_save_area
	movq	cfun(%rip), %rdi
.LBB75_3:                               # %if.end
	movq	280(%rdi), %r12
	jmp	.LBB75_4
	.align	16, 0x90
.LBB75_18:                              # %cond.end
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	8(%rcx), %rsi
	movq	%r15, %rdi
	movq	48(%rsp), %rdx          # 8-byte Reload
	callq	x86_initialize_trampoline
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	cfun(%rip), %rax
	movq	240(%rax), %rsi
	movq	%rbx, %rdi
	callq	emit_insns_before
	movq	(%r12), %r12
.LBB75_4:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB75_13 Depth 2
	testq	%r12, %r12
	je	.LBB75_19
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	24(%r12), %r14
	movq	%r14, %rdi
	callq	decl_function_context
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB75_16
# BB#6:                                 # %lor.lhs.false.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movzbl	16(%r14), %ecx
	cmpq	$30, %rcx
	jne	.LBB75_8
# BB#7:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movzbl	49(%r14), %ecx
	testq	$4, %rcx
	movabsq	$0, %rcx
	jne	.LBB75_16
.LBB75_8:                               # %if.end.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB75_10
# BB#9:                                 # %if.end.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	inline_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB75_10
# BB#11:                                # %if.end9.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	cfun(%rip), %rcx
	movq	272(%rcx), %rcx
	jmp	.LBB75_13
	.align	16, 0x90
.LBB75_12:                              # %for.cond.i
                                        #   in Loop: Header=BB75_13 Depth=2
	movq	(%rcx), %rcx
.LBB75_13:                              # %for.cond.i
                                        #   Parent Loop BB75_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	je	.LBB75_92
# BB#14:                                # %for.body.i
                                        #   in Loop: Header=BB75_13 Depth=2
	movq	24(%rcx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB75_12
# BB#15:                                # %if.then12.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	32(%rcx), %rax
	movq	40(%rax), %rcx
	jmp	.LBB75_16
.LBB75_10:                              # %if.then8.i
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	global_rtl+56(%rip), %rcx
	.align	16, 0x90
.LBB75_16:                              # %lookup_static_chain.exit
                                        #   in Loop: Header=BB75_4 Depth=1
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	32(%r12), %rax
	movq	40(%rax), %rbx
	callq	start_sequence
	movl	target_flags(%rip), %edi
	movq	8(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	$-2, %rsi
	callq	gen_rtx_CONST_INT
	movl	target_flags(%rip), %edi
	movq	%rax, %r15
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	expand_simple_binop
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	$83, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	expand_simple_binop
	movq	144(%r14), %rcx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB75_18
# BB#17:                                # %cond.false
                                        #   in Loop: Header=BB75_4 Depth=1
	xorq	%rsi, %rsi
	movq	%r14, %rbx
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rcx
	jmp	.LBB75_18
.LBB75_19:                              # %for.end
	movl	flag_stack_check(%rip), %eax
	testq	%rax, %rax
	je	.LBB75_24
# BB#20:                                # %if.then18
	callq	get_insns
	jmp	.LBB75_21
	.align	16, 0x90
.LBB75_93:                              # %for.inc38
                                        #   in Loop: Header=BB75_21 Depth=1
	movq	24(%rax), %rax
.LBB75_21:                              # %if.then18
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB75_24
# BB#22:                                # %for.body23
                                        #   in Loop: Header=BB75_21 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	jne	.LBB75_93
# BB#23:                                # %if.then26
	callq	start_sequence
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	imulq	$75, %rax, %rcx
	addq	$300, %rcx              # imm = 0x12C
	movabsq	$4294967292, %rbx       # imm = 0xFFFFFFFC
	andq	%rcx, %rbx
	movq	$4092, %rsi             # imm = 0xFFC
	subq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	probe_stack_range
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	cfun(%rip), %rax
	movq	240(%rax), %rsi
	movq	%rbx, %rdi
	callq	emit_insns_before
.LBB75_24:                              # %if.end43
	movslq	warn_unused_parameter(%rip), %rax
	testq	%rax, %rax
	jg	.LBB75_27
# BB#25:                                # %lor.lhs.false
	jns	.LBB75_34
# BB#26:                                # %lor.lhs.false
	movl	extra_warnings(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB75_34
.LBB75_27:                              # %if.then50
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB75_34
# BB#28:
	movabsq	$.L.str8, %r14
	.align	16, 0x90
.LBB75_29:                              # %for.body55
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	andq	$65791, %rax            # imm = 0x100FF
	cmpq	$35, %rax
	jne	.LBB75_33
# BB#30:                                # %land.lhs.true67
                                        #   in Loop: Header=BB75_29 Depth=1
	cmpq	$0, 72(%rbx)
	je	.LBB75_33
# BB#31:                                # %land.lhs.true70
                                        #   in Loop: Header=BB75_29 Depth=1
	movzbl	50(%rbx), %eax
	testq	$64, %rax
	jne	.LBB75_33
# BB#32:                                # %if.then76
                                        #   in Loop: Header=BB75_29 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB75_33:                              # %for.inc78
                                        #   in Loop: Header=BB75_29 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB75_29
.LBB75_34:                              # %if.end82
	movq	cfun(%rip), %rax
	cmpq	$0, 168(%rax)
	je	.LBB75_56
# BB#35:                                # %land.lhs.true85
	movzbl	425(%rax), %eax
	testq	$1, %rax
	jne	.LBB75_56
# BB#36:                                # %if.then90
	callq	get_insns
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB75_56
# BB#37:
	movabsq	$4026531839, %r15       # imm = 0xEFFFFFFF
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB75_38:                              # %for.body.i330
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB75_41 Depth 2
                                        #     Child Loop BB75_49 Depth 2
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	jne	.LBB75_47
# BB#39:                                # %if.then.i
                                        #   in Loop: Header=BB75_38 Depth=1
	andq	%r15, %rax
	movl	%eax, (%r14)
	movq	cfun(%rip), %rax
	movq	160(%rax), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	je	.LBB75_47
	.align	16, 0x90
.LBB75_41:                              # %for.body5.i
                                        #   Parent Loop BB75_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rbx
	movq	32(%rbx), %rdi
	movq	144(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB75_43
# BB#42:                                # %cond.false.i
                                        #   in Loop: Header=BB75_41 Depth=2
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	32(%rbx), %rax
	movq	144(%rax), %rax
.LBB75_43:                              # %cond.end.i
                                        #   in Loop: Header=BB75_41 Depth=2
	cmpq	%r14, %rax
	je	.LBB75_44
# BB#40:                                # %for.cond3.i
                                        #   in Loop: Header=BB75_41 Depth=2
	movq	(%rbx), %rax
	testq	%rax, %rax
	movq	%rbx, %r13
	jne	.LBB75_41
	jmp	.LBB75_47
.LBB75_44:                              # %if.then20.i
                                        #   in Loop: Header=BB75_38 Depth=1
	testq	%r13, %r13
	jne	.LBB75_46
# BB#45:                                # %if.then22.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	cfun(%rip), %rax
	movq	160(%rax), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, 160(%rax)
	jmp	.LBB75_47
.LBB75_46:                              # %if.else.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	(%rbx), %rax
	movq	%rax, (%r13)
	.align	16, 0x90
.LBB75_47:                              # %if.end33.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movzbl	(%r14), %eax
	cmpq	$32, %rax
	jne	.LBB75_55
# BB#48:                                # %if.then37.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	cfun(%rip), %rax
	movq	168(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB75_52
	.align	16, 0x90
.LBB75_49:                              # %for.body41.i
                                        #   Parent Loop BB75_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%r14), %rsi
	movq	%rbx, %rdi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB75_54
# BB#50:                                # %for.inc46.i
                                        #   in Loop: Header=BB75_49 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB75_49
# BB#51:                                # %for.cond39.lor.lhs.false.critedge_crit_edge.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	cfun(%rip), %rax
.LBB75_52:                              # %lor.lhs.false.critedge.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	184(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB75_55
# BB#53:                                # %land.lhs.true.i334
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	32(%r14), %rsi
	callq	reg_mentioned_p
	testq	%r12, %rax
	je	.LBB75_55
.LBB75_54:                              # %if.then59.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	%r14, %rdi
	callq	delete_related_insns
.LBB75_55:                              # %for.inc63.i
                                        #   in Loop: Header=BB75_38 Depth=1
	movq	24(%r14), %r14
	testq	%r14, %r14
	jne	.LBB75_38
.LBB75_56:                              # %while.cond.preheader
	callq	in_sequence_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB75_58
	.align	16, 0x90
.LBB75_57:                              # %while.body
                                        #   in Loop: Header=BB75_58 Depth=1
	callq	end_sequence
	callq	in_sequence_p
.LBB75_58:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rax
	jne	.LBB75_57
# BB#59:                                # %while.end
	movl	immediate_size_expand(%rip), %eax
	decq	%rax
	movl	%eax, immediate_size_expand(%rip)
	callq	clear_pending_stack_adjust
	callq	do_pending_stack_adjust
	movabsq	$4294967205, %rsi       # imm = 0xFFFFFFA5
	xorq	%rdi, %rdi
	callq	emit_note
	movl	flag_test_coverage(%rip), %eax
	testq	%rax, %rax
	je	.LBB75_61
# BB#60:                                # %if.then96
	movabsq	$4294967212, %rsi       # imm = 0xFFFFFFAC
	xorq	%rdi, %rdi
	callq	emit_note
.LBB75_61:                              # %if.end98
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	emit_line_note_force
	callq	get_last_insn
	movq	cfun(%rip), %rcx
	movq	200(%rcx), %rdi
	movq	%rax, %r15
	testq	%rdi, %rdi
	je	.LBB75_63
# BB#62:                                # %if.then102
	callq	emit_label
.LBB75_63:                              # %if.end105
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB75_65
# BB#64:                                # %if.then107
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	expand_end_bindings
.LBB75_65:                              # %if.end108
	movq	cfun(%rip), %rax
	movzbl	425(%rax), %eax
	testq	$32, %rax
	je	.LBB75_70
# BB#66:                                # %if.then113
	movq	current_function_decl(%rip), %rdi
	movq	144(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB75_68
# BB#67:                                # %cond.false120
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	144(%rax), %rax
.LBB75_68:                              # %cond.end123
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB75_73
# BB#69:                                # %if.then129
	movq	8(%rax), %r14
	movl	target_flags(%rip), %ebx
	movq	libfunc_table+768(%rip), %r12
	movq	$25, %rcx
	movq	global_rtl+32(%rip), %rdx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	movq	$53, %rdi
	xorq	%rsi, %rsi
	callq	expand_builtin_return_addr
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rax, (%rsp)
	movabsq	$0, %rsi
	movabsq	$0, %rdx
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	%rdi, 8(%rsp)
	movq	%r12, %rdi
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	emit_library_call
.LBB75_70:                              # %if.end141
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rax
	movq	144(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB75_85
# BB#71:                                # %cond.end164
	movl	(%rbx), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB75_75
# BB#72:                                # %cond.true170
	movl	8(%rbx), %edx
	cmpq	$52, %rdx
	ja	.LBB75_76
	jmp	.LBB75_85
.LBB75_75:                              # %cond.false175
	movzbl	49(%rax), %edx
	testq	$4, %rdx
	je	.LBB75_85
.LBB75_76:                              # %if.then182
	movq	cfun(%rip), %rdx
	movq	120(%rdx), %r14
	movl	(%r14), %edx
	testq	$1073741824, %rdx       # imm = 0x40000000
	je	.LBB75_77
# BB#78:                                # %if.end188
	movq	%rdx, %rsi
	andq	$16711680, %rsi         # imm = 0xFF0000
	cmpq	$3342336, %rsi          # imm = 0x330000
	jne	.LBB75_80
# BB#79:                                # %if.then194
	andq	$16711680, %rcx         # imm = 0xFF0000
	movabsq	$4278255615, %rsi       # imm = 0xFF00FFFF
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%r14)
	movl	(%rbx), %ecx
.LBB75_80:                              # %if.end200
	xorq	%rdx, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB75_82
# BB#81:                                # %if.then209
	movq	8(%rax), %rax
	movl	16(%rax), %edx
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert_move
	jmp	.LBB75_85
.LBB75_82:                              # %if.else215
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$39, %rdx
	jne	.LBB75_84
# BB#83:                                # %if.then220
	movq	8(%rax), %rdi
	callq	int_size_in_bytes
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	emit_group_load
	jmp	.LBB75_85
.LBB75_84:                              # %if.else225
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB75_85:                              # %if.end230
	movq	cfun(%rip), %rax
	movzbl	424(%rax), %eax
	testq	$3, %rax
	je	.LBB75_89
# BB#86:                                # %if.then239
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rdi
	movq	144(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB75_88
# BB#87:                                # %cond.false250
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rdi
	movq	144(%rdi), %rax
.LBB75_88:                              # %cond.end257
	movq	8(%rax), %r14
	movq	8(%rdi), %rdi
	callq	build_pointer_type
	movq	%rax, %rdi
	callq	ix86_function_value
	movq	%rax, %rbx
	movl	(%rbx), %eax
	orq	$1073741824, %rax       # imm = 0x40000000
	movl	%eax, (%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	cfun(%rip), %rax
	movq	%rbx, 120(%rax)
.LBB75_89:                              # %if.end274
	callq	expand_eh_return
	callq	start_sequence
	callq	clobber_return_register
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_insn_after
	cmpq	%rax, %r15
	je	.LBB75_91
# BB#90:                                # %if.then280
	movq	cfun(%rip), %rcx
	movq	%rax, 256(%rcx)
.LBB75_91:                              # %if.end281
	callq	use_return_register
	callq	get_insns
	movq	%rax, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_fixups  # TAILCALL
.LBB75_92:                              # %for.end.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.lookup_static_chain, %rdx
	movq	$5612, %rsi             # imm = 0x15EC
	callq	fancy_abort
.LBB75_73:                              # %if.else
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_function_end, %rcx
	movq	$6977, %rsi             # imm = 0x1B41
	jmp	.LBB75_74
.LBB75_77:                              # %if.then187
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_function_end, %rcx
	movq	$7021, %rsi             # imm = 0x1B6D
.LBB75_74:                              # %if.else
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp662:
	.size	expand_function_end, .Ltmp662-expand_function_end
	.cfi_endproc

	.globl	prologue_epilogue_contains
	.align	16, 0x90
	.type	prologue_epilogue_contains,@function
prologue_epilogue_contains:             # @prologue_epilogue_contains
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp669:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp670:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp671:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp672:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp673:
	.cfi_def_cfa_offset 48
.Ltmp674:
	.cfi_offset %rbx, -48
.Ltmp675:
	.cfi_offset %r12, -40
.Ltmp676:
	.cfi_offset %r13, -32
.Ltmp677:
	.cfi_offset %r14, -24
.Ltmp678:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %r8d
	movq	prologue(%rip), %rsi
	cmpq	$32, %r8
	jne	.LBB76_11
# BB#1:                                 # %land.lhs.true.i
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB76_11
# BB#2:                                 # %if.then.i
	movq	8(%rax), %r9
	movslq	(%r9), %r12
	testq	%r12, %r12
	jle	.LBB76_3
# BB#4:                                 # %for.body.lr.ph.i
	movq	%r8, -8(%rsp)           # 8-byte Spill
	movq	(%rsi), %r10
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	addq	%r15, %r10
	movq	$32, %rcx
	movq	%r10, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	xorq	%rdx, %rdx
	movabsq	$0, %r13
	movabsq	$1, %r14
	.align	16, 0x90
.LBB76_6:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB76_8 Depth 2
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB76_5
# BB#7:                                 # %for.body14.lr.ph.i
                                        #   in Loop: Header=BB76_6 Depth=1
	movq	(%r9,%r12,8), %rax
	movl	8(%rax), %r8d
	movq	%r11, %rbx
	movq	%rdx, %rcx
	.align	16, 0x90
.LBB76_8:                               # %for.body14.i
                                        #   Parent Loop BB76_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	32(%rsi,%rbx,4), %edx
	movq	%r8, %rax
	andq	%r15, %rax
	cmpq	%rdx, %rax
	movq	%r13, %rdx
	jne	.LBB76_10
# BB#9:                                 # %for.body14.i
                                        #   in Loop: Header=BB76_8 Depth=2
	movq	%r14, %rdx
.LBB76_10:                              # %for.body14.i
                                        #   in Loop: Header=BB76_8 Depth=2
	addq	%rcx, %rdx
	movq	$32, %rcx
	leaq	-1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rax, %rbx
	movq	%rdx, %rcx
	jg	.LBB76_8
.LBB76_5:                               # %for.cond.loopexit.i
                                        #   in Loop: Header=BB76_6 Depth=1
	decq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB76_6
# BB#15:                                # %contains.exit
	movq	$1, %rax
	testq	%r15, %rdx
	movq	-8(%rsp), %r8           # 8-byte Reload
	jne	.LBB76_36
	jmp	.LBB76_16
.LBB76_11:                              # %if.else.i
	movq	(%rsi), %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	js	.LBB76_16
# BB#12:                                # %for.body39.lr.ph.i
	movl	8(%rdi), %ebx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	andq	%rax, %rbx
	.align	16, 0x90
.LBB76_14:                              # %for.body39.i
                                        # =>This Inner Loop Header: Depth=1
	movl	32(%rsi,%rdx,4), %ecx
	movq	$1, %rax
	cmpq	%rcx, %rbx
	je	.LBB76_36
# BB#13:                                # %for.cond36.i
                                        #   in Loop: Header=BB76_14 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdx), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rax, %rdx
	jg	.LBB76_14
.LBB76_16:                              # %if.end
	movq	epilogue(%rip), %rax
	cmpq	$32, %r8
	jne	.LBB76_27
	jmp	.LBB76_17
.LBB76_3:                               # %if.end.thread
	movq	epilogue(%rip), %rax
.LBB76_17:                              # %land.lhs.true.i14
	movq	32(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB76_27
# BB#18:                                # %if.then.i19
	movq	8(%rcx), %r8
	movslq	(%r8), %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	jle	.LBB76_33
# BB#19:                                # %for.body.lr.ph.i27
	movq	(%rax), %r10
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	addq	%r15, %r10
	movq	$32, %rcx
	movq	%r10, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	xorq	%rsi, %rsi
	movabsq	$0, %r12
	movabsq	$1, %r11
	.align	16, 0x90
.LBB76_21:                              # %for.body.i34
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB76_24 Depth 2
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB76_22
# BB#23:                                # %for.body14.lr.ph.i38
                                        #   in Loop: Header=BB76_21 Depth=1
	movq	(%r8,%r14,8), %rcx
	movl	8(%rcx), %edx
	movq	%r9, %rdi
	.align	16, 0x90
.LBB76_24:                              # %for.body14.i47
                                        #   Parent Loop BB76_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	32(%rax,%rdi,4), %ecx
	movq	%rdx, %rbx
	andq	%r15, %rbx
	cmpq	%rcx, %rbx
	movq	%r12, %rbx
	jne	.LBB76_26
# BB#25:                                # %for.body14.i47
                                        #   in Loop: Header=BB76_24 Depth=2
	movq	%r11, %rbx
.LBB76_26:                              # %for.body14.i47
                                        #   in Loop: Header=BB76_24 Depth=2
	addq	%rsi, %rbx
	movq	$32, %rcx
	leaq	-1(%rdi), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	jg	.LBB76_24
	jmp	.LBB76_20
	.align	16, 0x90
.LBB76_22:                              #   in Loop: Header=BB76_21 Depth=1
	movq	%rsi, %rbx
.LBB76_20:                              # %for.cond.loopexit.i30
                                        #   in Loop: Header=BB76_21 Depth=1
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rbx, %rsi
	jg	.LBB76_21
	jmp	.LBB76_33
.LBB76_27:                              # %if.else.i52
	movq	(%rax), %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	addq	%rbx, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB76_28
# BB#31:                                # %for.body39.lr.ph.i58
	movl	8(%rdi), %esi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	andq	%rbx, %rsi
	movq	$1, %rbx
	.align	16, 0x90
.LBB76_32:                              # %for.body39.i65
                                        # =>This Inner Loop Header: Depth=1
	movl	32(%rax,%rdx,4), %ecx
	cmpq	%rcx, %rsi
	je	.LBB76_33
# BB#29:                                # %for.cond36.i61
                                        #   in Loop: Header=BB76_32 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdx), %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rdi, %rdx
	jg	.LBB76_32
# BB#30:
	xorq	%rbx, %rbx
	jmp	.LBB76_33
.LBB76_28:
	xorq	%rbx, %rbx
.LBB76_33:                              # %contains.exit67
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB76_34
# BB#35:                                # %contains.exit67
	movabsq	$1, %rax
	jmp	.LBB76_36
.LBB76_34:
	movabsq	$0, %rax
.LBB76_36:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp679:
	.size	prologue_epilogue_contains, .Ltmp679-prologue_epilogue_contains
	.cfi_endproc

	.globl	sibcall_epilogue_contains
	.align	16, 0x90
	.type	sibcall_epilogue_contains,@function
sibcall_epilogue_contains:              # @sibcall_epilogue_contains
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp685:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp686:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp687:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp688:
	.cfi_def_cfa_offset 40
.Ltmp689:
	.cfi_offset %rbx, -40
.Ltmp690:
	.cfi_offset %r12, -32
.Ltmp691:
	.cfi_offset %r14, -24
.Ltmp692:
	.cfi_offset %r15, -16
	movq	sibcall_epilogue(%rip), %rdx
	testq	%rdx, %rdx
	je	.LBB77_1
# BB#2:                                 # %if.then
	movzbl	(%rdi), %eax
	cmpq	$32, %rax
	jne	.LBB77_13
# BB#3:                                 # %land.lhs.true.i
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB77_13
# BB#4:                                 # %if.then.i
	movq	8(%rax), %r8
	movslq	(%r8), %r11
	testq	%r11, %r11
	movabsq	$0, %rax
	jle	.LBB77_19
# BB#5:                                 # %for.body.lr.ph.i
	movq	(%rdx), %r9
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	addq	%r15, %r9
	movq	$32, %rcx
	movq	%r9, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	xorq	%rbx, %rbx
	movabsq	$0, %r12
	movabsq	$1, %r14
	.align	16, 0x90
.LBB77_7:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB77_10 Depth 2
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB77_8
# BB#9:                                 # %for.body14.lr.ph.i
                                        #   in Loop: Header=BB77_7 Depth=1
	movq	(%r8,%r11,8), %rax
	movl	8(%rax), %esi
	movq	%r10, %rdi
	.align	16, 0x90
.LBB77_10:                              # %for.body14.i
                                        #   Parent Loop BB77_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	32(%rdx,%rdi,4), %eax
	movq	%rsi, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	movq	%r12, %rax
	jne	.LBB77_12
# BB#11:                                # %for.body14.i
                                        #   in Loop: Header=BB77_10 Depth=2
	movq	%r14, %rax
.LBB77_12:                              # %for.body14.i
                                        #   in Loop: Header=BB77_10 Depth=2
	addq	%rbx, %rax
	movq	$32, %rcx
	leaq	-1(%rdi), %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rbx, %rdi
	movq	%rax, %rbx
	jg	.LBB77_10
	jmp	.LBB77_6
	.align	16, 0x90
.LBB77_8:                               #   in Loop: Header=BB77_7 Depth=1
	movq	%rbx, %rax
.LBB77_6:                               # %for.cond.loopexit.i
                                        #   in Loop: Header=BB77_7 Depth=1
	decq	%r11
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rax, %rbx
	jg	.LBB77_7
	jmp	.LBB77_19
.LBB77_13:                              # %if.else.i
	movq	(%rdx), %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	addq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	js	.LBB77_14
# BB#17:                                # %for.body39.lr.ph.i
	movl	8(%rdi), %edi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	andq	%rax, %rdi
	.align	16, 0x90
.LBB77_18:                              # %for.body39.i
                                        # =>This Inner Loop Header: Depth=1
	movl	32(%rdx,%rsi,4), %ecx
	movq	$1, %rax
	cmpq	%rcx, %rdi
	je	.LBB77_19
# BB#15:                                # %for.cond36.i
                                        #   in Loop: Header=BB77_18 Depth=1
	movq	$32, %rcx
	leaq	-1(%rsi), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rax, %rsi
	jg	.LBB77_18
# BB#16:
	xorq	%rax, %rax
	jmp	.LBB77_19
.LBB77_1:
	xorq	%rax, %rax
	jmp	.LBB77_19
.LBB77_14:
	xorq	%rax, %rax
.LBB77_19:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp693:
	.size	sibcall_epilogue_contains, .Ltmp693-sibcall_epilogue_contains
	.cfi_endproc

	.globl	thread_prologue_and_epilogue_insns
	.align	16, 0x90
	.type	thread_prologue_and_epilogue_insns,@function
thread_prologue_and_epilogue_insns:     # @thread_prologue_and_epilogue_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp700:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp701:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp702:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp703:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp704:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp705:
	.cfi_def_cfa_offset 160
.Ltmp706:
	.cfi_offset %rbx, -48
.Ltmp707:
	.cfi_offset %r12, -40
.Ltmp708:
	.cfi_offset %r13, -32
.Ltmp709:
	.cfi_offset %r14, -24
.Ltmp710:
	.cfi_offset %r15, -16
	callq	start_sequence
	callq	gen_prologue
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	emit_insn
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$24, %rcx
	je	.LBB78_2
# BB#1:                                 # %if.then
	callq	get_insns
	movq	%rax, %r14
	movl	(%r14), %eax
.LBB78_2:                               # %if.end
	movabsq	$4294967296, %rdx       # imm = 0x100000000
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$24, %rax
	jne	.LBB78_6
# BB#3:                                 # %if.then.i
	movq	8(%r14), %rax
	movq	prologue(%rip), %rdi
	movslq	(%rax), %rbx
	movq	(%rdi), %r15
	leaq	(%r15,%rbx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	testq	%rbx, %rbx
	movq	%rax, prologue(%rip)
	jle	.LBB78_7
# BB#4:                                 # %while.body.lr.ph.i
	movq	8(%r14), %rdx
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	32(%rax,%r15,4), %rax
	decq	%rbx
	.align	16, 0x90
.LBB78_5:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx,%rbx,8), %rcx
	movl	8(%rcx), %ecx
	movl	%ecx, (%rax)
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$4, %rax
	decq	%rbx
	testq	%rsi, %rsi
	jg	.LBB78_5
	jmp	.LBB78_7
.LBB78_6:                               # %if.else.i
	movq	prologue(%rip), %rdi
	movq	(%rdi), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	leaq	(%rbx,%rdx), %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movq	%rax, prologue(%rip)
	movl	8(%r14), %edx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	%edx, 32(%rax,%rbx,4)
.LBB78_7:                               # %record_insns.exit
	movabsq	$4294967206, %rsi       # imm = 0xFFFFFFA6
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, %r15
	movq	%r15, 56(%rsp)          # 8-byte Spill
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	entry_exit_blocks+40(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB78_9
# BB#8:                                 # %lor.lhs.false
	cmpq	$0, 8(%rsi)
	jne	.LBB78_9
# BB#11:                                # %if.end7
	movq	%rbx, %rdi
	callq	insert_insn_on_edge
	movq	entry_exit_blocks+144(%rip), %rax
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	jmp	.LBB78_13
	.align	16, 0x90
.LBB78_12:                              # %for.cond
                                        #   in Loop: Header=BB78_13 Depth=1
	movq	(%rax), %rax
.LBB78_13:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB78_150
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB78_13 Depth=1
	movzbl	48(%rax), %ecx
	testq	$16, %rcx
	jne	.LBB78_12
# BB#15:                                # %if.end14
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB78_28
# BB#16:                                # %land.lhs.true
	callq	ix86_can_use_return_insn_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB78_28
# BB#17:                                # %for.cond19.preheader
	movq	entry_exit_blocks+144(%rip), %rax
	jmp	.LBB78_19
	.align	16, 0x90
.LBB78_18:                              # %for.cond19
                                        #   in Loop: Header=BB78_19 Depth=1
	movq	(%rax), %rax
.LBB78_19:                              # %for.cond19
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB78_150
# BB#20:                                # %for.body21
                                        #   in Loop: Header=BB78_19 Depth=1
	movzbl	48(%rax), %ecx
	testq	$1, %rcx
	je	.LBB78_18
# BB#21:                                # %if.end32
	movq	16(%rax), %r14
	movq	8(%r14), %rbx
	xorq	%r13, %r13
	jmp	.LBB78_22
.LBB78_195:                             # %if.end40
                                        #   in Loop: Header=BB78_22 Depth=1
	movq	16(%rbx), %rbx
.LBB78_22:                              # %if.end32
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB78_26
# BB#23:                                # %land.rhs
                                        #   in Loop: Header=BB78_22 Depth=1
	movq	%rdx, %r12
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB78_25
# BB#24:                                # %while.body
                                        #   in Loop: Header=BB78_22 Depth=1
	movq	%rbx, %rdi
	callq	active_insn_p
	movq	%r12, %rdx
	testq	%rdx, %rax
	je	.LBB78_195
.LBB78_25:
	movq	%rbx, %r13
.LBB78_26:                              # %while.end
	movq	(%r14), %rax
	cmpq	%r13, %rax
	jne	.LBB78_28
# BB#27:                                # %land.lhs.true42
	movzbl	(%r13), %eax
	cmpq	$36, %rax
	jne	.LBB78_28
# BB#29:                                # %if.then46
	movq	%r14, 40(%rsp)          # 8-byte Spill
	callq	get_last_insn
	movq	%rax, %rbx
	jmp	.LBB78_30
.LBB78_28:                              # %for.cond150.preheader
	movq	entry_exit_blocks+144(%rip), %r14
	jmp	.LBB78_60
	.align	16, 0x90
.LBB78_59:                              # %for.cond150
                                        #   in Loop: Header=BB78_60 Depth=1
	movq	(%r14), %r14
.LBB78_60:                              # %for.cond150
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB78_150
# BB#61:                                # %for.body152
                                        #   in Loop: Header=BB78_60 Depth=1
	movzbl	48(%r14), %eax
	testq	$1, %rax
	je	.LBB78_59
# BB#62:                                # %if.end163
	callq	start_sequence
	movabsq	$4294967207, %rsi       # imm = 0xFFFFFFA7
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	gen_epilogue
	movq	current_function_decl(%rip), %rcx
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %edx
	movq	%rax, %r13
	movq	%r13, 32(%rsp)          # 8-byte Spill
	cmpq	$23, %rdx
	jne	.LBB78_142
# BB#63:                                # %land.lhs.true170
	movzbl	62(%rcx), %eax
	testq	$2, %rax
	je	.LBB78_142
# BB#64:                                # %if.then177
	movzbl	(%r13), %eax
	cmpq	$24, %rax
	jne	.LBB78_142
# BB#65:                                # %if.end.i
	callq	start_sequence
	movq	global_rtl+16(%rip), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rax, 72(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 104(%rsp)
	movq	8(%r13), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB78_141
# BB#66:                                # %for.body.lr.ph.i
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	jmp	.LBB78_67
.LBB78_35:                              # %for.inc64
                                        #   in Loop: Header=BB78_30 Depth=1
	movq	16(%rbx), %rbx
.LBB78_30:                              # %if.then46
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB78_36
# BB#31:                                # %land.rhs50
                                        #   in Loop: Header=BB78_30 Depth=1
	movq	%rbx, %rdi
	callq	active_insn_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_36
# BB#32:                                # %for.body54
                                        #   in Loop: Header=BB78_30 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB78_35
# BB#33:                                # %land.lhs.true58
                                        #   in Loop: Header=BB78_30 Depth=1
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jle	.LBB78_35
# BB#34:
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB78_37
.LBB78_133:                             # %if.else344.i
                                        #   in Loop: Header=BB78_67 Depth=1
	cmpq	$39, %rax
	movq	32(%rsp), %r13          # 8-byte Reload
	jne	.LBB78_138
# BB#134:                               # %for.cond365.preheader.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	8(%rdi), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB78_139
.LBB78_135:                             # %for.body375.i
                                        #   Parent Loop BB78_67 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB78_137
# BB#136:                               # %if.then389.i
                                        #   in Loop: Header=BB78_135 Depth=2
	movq	%rax, %rdi
	leaq	72(%rsp), %rsi
	callq	handle_epilogue_set
	movq	32(%r12), %rdi
.LBB78_137:                             # %for.inc400.i
                                        #   in Loop: Header=BB78_135 Depth=2
	movq	8(%rdi), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB78_135
	jmp	.LBB78_139
.LBB78_138:                             # %if.else403.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r12, %rdi
	callq	add_insn
	jmp	.LBB78_139
	.align	16, 0x90
.LBB78_67:                              # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_135 Depth 2
                                        #     Child Loop BB78_97 Depth 2
	movq	8(%rax,%rbx,8), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB78_68
# BB#69:                                # %if.end13.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	104(%rsp), %r13
	xorq	%rcx, %rcx
	testq	%r13, %r13
	jne	.LBB78_71
# BB#70:                                #   in Loop: Header=BB78_67 Depth=1
	movq	48(%rsp), %r13          # 8-byte Reload
	jmp	.LBB78_75
	.align	16, 0x90
.LBB78_68:                              # %if.then12.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r12, %rdi
	callq	add_insn
	jmp	.LBB78_140
.LBB78_71:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	32(%r12), %rsi
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	callq	reg_referenced_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_73
# BB#72:                                #   in Loop: Header=BB78_67 Depth=1
	movq	%r13, %rcx
	movq	%r12, %r13
	jmp	.LBB78_74
.LBB78_73:                              # %emit_equiv_load.exit.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
	movq	$0, 104(%rsp)
	movq	$0, 72(%rsp)
	xorq	%r13, %r13
	xorq	%rcx, %rcx
.LBB78_74:                              # %if.end22.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	24(%rsp), %r12          # 8-byte Reload
.LBB78_75:                              # %if.end22.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r13, 88(%rsp)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 96(%rsp)
	movq	32(%r12), %rsi
	movzbl	(%rsi), %eax
	cmpq	$51, %rax
	je	.LBB78_78
# BB#76:                                # %if.end22.i
                                        #   in Loop: Header=BB78_67 Depth=1
	cmpq	$39, %rax
	jne	.LBB78_122
# BB#77:                                # %land.lhs.true39.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	8(%rsi), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	jne	.LBB78_122
.LBB78_78:                              # %if.then52.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%r13, 48(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_REG
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r13
	movzbl	(%r13), %eax
	cmpq	$66, %rax
	jne	.LBB78_79
# BB#83:                                # %land.lhs.true64.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB78_84
# BB#86:                                # %land.lhs.true93.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	8(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB78_193
# BB#87:                                # %land.lhs.true104.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB78_193
# BB#88:                                # %if.then115.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movl	8(%rdx), %esi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_rtx_REG
	movq	8(%r13), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %r12
	jmp	.LBB78_89
.LBB78_122:                             # %if.else310.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	global_rtl+16(%rip), %rdi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_126
# BB#123:                               # %land.lhs.true316.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	global_rtl+16(%rip), %rax
	cmpq	%rax, %r13
	je	.LBB78_125
# BB#124:                               # %lor.lhs.false320.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r13, %rdi
	movq	%rcx, %rbx
	movq	%r12, %rsi
	callq	reg_set_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB78_125
.LBB78_126:                             # %if.else325.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	global_rtl+16(%rip), %rdi
	movq	%rcx, %rbx
	movq	%r12, %rsi
	callq	reg_set_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	jne	.LBB78_132
# BB#127:                               # %land.lhs.true328.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	global_rtl+16(%rip), %rax
	cmpq	%rax, %r13
	movq	%r13, %rbx
                                        # kill: R13<def> RBX<kill>
	je	.LBB78_130
# BB#128:                               # %lor.lhs.false332.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	reg_set_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_132
# BB#129:                               # %lor.lhs.false332.if.then336_crit_edge.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	global_rtl+16(%rip), %r13
.LBB78_130:                             # %if.then336.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	plus_constant_wide
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	validate_replace_rtx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB78_131
.LBB78_125:                             # %if.then324.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r12, %rdi
	callq	add_insn
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB78_139
.LBB78_79:                              # %if.then52.i
                                        #   in Loop: Header=BB78_67 Depth=1
	cmpq	$61, %rax
	jne	.LBB78_193
# BB#80:                                # %if.then59.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB78_82
# BB#81:                                # %if.then.i500.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	emit_move_insn
.LBB78_82:                              # %emit_equiv_load.exit501.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	$0, 104(%rsp)
	movq	%r12, %rdi
	callq	add_insn
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB78_140
.LBB78_84:                              # %land.lhs.true64.i
                                        #   in Loop: Header=BB78_67 Depth=1
	cmpq	$61, %rcx
	jne	.LBB78_193
# BB#85:                                # %if.then72.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movl	8(%rax), %esi
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, %r12
	xorq	%rsi, %rsi
.LBB78_89:                              # %if.end140.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%r12, %rdi
	callq	plus_constant_wide
	movq	global_rtl+16(%rip), %rcx
	movq	%rax, %r13
	cmpq	%rcx, %r12
	jne	.LBB78_90
# BB#91:                                # %if.then144.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	plus_constant_wide
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	simplify_replace_rtx
	movq	%rax, %r13
	jmp	.LBB78_92
.LBB78_90:                              #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
.LBB78_92:                              # %if.end149.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	jne	.LBB78_94
# BB#93:                                #   in Loop: Header=BB78_67 Depth=1
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB78_116
.LBB78_94:                              # %land.lhs.true156.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, %rsi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB78_96
# BB#95:                                #   in Loop: Header=BB78_67 Depth=1
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB78_115
.LBB78_132:                             # %if.else344.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	32(%r12), %rdi
	movzbl	(%rdi), %eax
	cmpq	$47, %rax
	jne	.LBB78_133
# BB#194:                               # %if.then352.i
                                        #   in Loop: Header=BB78_67 Depth=1
	leaq	72(%rsp), %rsi
	callq	handle_epilogue_set
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB78_139
.LBB78_96:                              #   in Loop: Header=BB78_67 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$1, %r13
	xorq	%r12, %r12
	.align	16, 0x90
.LBB78_97:                              # %for.body164.i
                                        #   Parent Loop BB78_67 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rbx
	movl	target_flags(%rip), %esi
	leaq	-1(%r13), %r15
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%r15, %rdi
	callq	ix86_hard_regno_mode_ok
	movq	%r15, %rcx
	movq	%rbx, %rsi
	testq	%rsi, %rax
	je	.LBB78_111
# BB#98:                                # %land.lhs.true170.i
                                        #   in Loop: Header=BB78_97 Depth=2
	movzbl	fixed_regs-1(%r13), %eax
	testq	%rax, %rax
	jne	.LBB78_111
# BB#99:                                # %land.lhs.true174.i
                                        #   in Loop: Header=BB78_97 Depth=2
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB78_111
# BB#100:                               # %land.lhs.true177.i
                                        #   in Loop: Header=BB78_97 Depth=2
	movq	entry_exit_blocks+176(%rip), %rdi
	movq	%rsi, %r15
	movq	%rcx, %rsi
	movq	%rcx, %rbx
	callq	bitmap_bit_p
	movq	%rbx, %rdi
	movq	%r15, %rsi
	testq	%rsi, %rax
	jne	.LBB78_111
# BB#101:                               # %land.lhs.true180.i
                                        #   in Loop: Header=BB78_97 Depth=2
	movq	%rsi, %rcx
	movl	target_flags(%rip), %esi
	movq	%rdi, %rax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rax
	cmpq	$8, %rax
	je	.LBB78_105
# BB#102:                               # %land.lhs.true180.i
                                        #   in Loop: Header=BB78_97 Depth=2
	leaq	-21(%rdi), %rax
	andq	%rcx, %rax
	cmpq	$8, %rax
	jb	.LBB78_105
# BB#103:                               # %land.lhs.true180.i
                                        #   in Loop: Header=BB78_97 Depth=2
	leaq	-45(%rdi), %rax
	andq	%rcx, %rax
	cmpq	$8, %rax
	jb	.LBB78_105
# BB#104:                               # %land.lhs.true180.i
                                        #   in Loop: Header=BB78_97 Depth=2
	leaq	-29(%rdi), %rax
	andq	%rcx, %rax
	cmpq	$7, %rax
	jbe	.LBB78_105
# BB#109:                               # %cond.false.i
                                        #   in Loop: Header=BB78_97 Depth=2
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$25, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movzbl	mode_size+4(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
	jmp	.LBB78_110
.LBB78_105:                             # %cond.true.i
                                        #   in Loop: Header=BB78_97 Depth=2
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movl	mode_class+16(,%rsi,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB78_110
# BB#106:                               # %lor.rhs.i
                                        #   in Loop: Header=BB78_97 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %rsi
	jne	.LBB78_108
# BB#107:                               # %lor.rhs.i
                                        #   in Loop: Header=BB78_97 Depth=2
	movabsq	$1, %rsi
.LBB78_108:                             # %lor.rhs.i
                                        #   in Loop: Header=BB78_97 Depth=2
	incq	%rsi
.LBB78_110:                             # %cond.end254.i
                                        #   in Loop: Header=BB78_97 Depth=2
	addq	%rdi, %rsi
	xorq	%rcx, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rdi, %rbx
	callq	refers_to_regno_p
	movq	%rbx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	je	.LBB78_112
	.align	16, 0x90
.LBB78_111:                             # %for.inc.i
                                        #   in Loop: Header=BB78_97 Depth=2
	incq	%r12
	movq	%r13, %rax
	andq	%rsi, %rax
	incq	%r13
	cmpq	$53, %rax
	movq	%r12, %rdx
	jb	.LBB78_97
.LBB78_112:                             # %for.end.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rdx, %rax
	andq	%rsi, %rax
	cmpq	$53, %rax
	movq	32(%rsp), %r13          # 8-byte Reload
	je	.LBB78_113
# BB#114:                               # %if.end265.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rdx, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
	movq	%r12, %rbx
	movq	56(%rsp), %r15          # 8-byte Reload
.LBB78_115:                             # %if.then.i506.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	emit_move_insn
.LBB78_116:                             # %emit_equiv_load.exit507.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	$0, 104(%rsp)
	movq	%rbx, %rdi
	callq	gen_indirect_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	%rax, %rcx
	movzbl	(%rcx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB78_120
# BB#117:                               # %cond.true281.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	32(%rcx), %rax
	movzbl	(%rax), %edx
	cmpq	$47, %rdx
	je	.LBB78_119
# BB#118:                               # %cond.false293.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB78_119:                             # %cond.end301.i
                                        #   in Loop: Header=BB78_67 Depth=1
	testq	%rax, %rax
	je	.LBB78_120
# BB#121:                               # %if.else306.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movl	(%rax), %ecx
	orq	$16777216, %rcx         # imm = 0x1000000
	movl	%ecx, (%rax)
.LBB78_139:                             # %if.end408.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	88(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rax, 72(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rax, 80(%rsp)
	movq	16(%rsp), %rbx          # 8-byte Reload
.LBB78_140:                             # %for.inc413.i
                                        #   in Loop: Header=BB78_67 Depth=1
	movq	8(%r13), %rax
	incq	%rbx
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB78_67
.LBB78_141:                             # %for.end415.i
	callq	gen_sequence
	movq	%rax, %r13
	callq	end_sequence
.LBB78_142:                             # %if.end179
	movq	%r13, %rdi
	callq	emit_jump_insn
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$24, %rcx
	je	.LBB78_144
# BB#143:                               # %if.then184
	callq	get_insns
	movq	%rax, %r13
	movl	(%r13), %eax
.LBB78_144:                             # %if.end186
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$24, %rax
	jne	.LBB78_148
# BB#145:                               # %if.then.i463
	movq	8(%r13), %rax
	movq	epilogue(%rip), %rdi
	movslq	(%rax), %rbx
	movq	(%rdi), %r12
	leaq	(%r12,%rbx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	testq	%rbx, %rbx
	movq	%rax, epilogue(%rip)
	jle	.LBB78_149
# BB#146:                               # %while.body.lr.ph.i467
	movq	8(%r13), %rdx
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	leaq	32(%rax,%r12,4), %rax
	decq	%rbx
	.align	16, 0x90
.LBB78_147:                             # %while.body.i477
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx,%rbx,8), %rcx
	movl	8(%rcx), %ecx
	movl	%ecx, (%rax)
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$4, %rax
	decq	%rbx
	testq	%rsi, %rsi
	jg	.LBB78_147
	jmp	.LBB78_149
.LBB78_148:                             # %if.else.i489
	movq	epilogue(%rip), %rdi
	movq	(%rdi), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$4294967296, %rax       # imm = 0x100000000
	leaq	(%rbx,%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movq	%rax, epilogue(%rip)
	movl	8(%r13), %edx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	%edx, 32(%rax,%rbx,4)
.LBB78_149:                             # %record_insns.exit491
	callq	gen_sequence
	movq	%rax, %r12
	callq	end_sequence
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	insert_insn_on_edge
.LBB78_150:                             # %if.then189
	callq	commit_edge_insertions
	movq	entry_exit_blocks+144(%rip), %r14
	jmp	.LBB78_151
	.align	16, 0x90
.LBB78_160:                             # %for.inc215
                                        #   in Loop: Header=BB78_151 Depth=1
	movq	(%r14), %r14
.LBB78_151:                             # %if.then189
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB78_158 Depth 2
	testq	%r14, %r14
	je	.LBB78_161
# BB#152:                               # %for.body193
                                        #   in Loop: Header=BB78_151 Depth=1
	movq	16(%r14), %rax
	movq	8(%rax), %rbx
	movl	(%rbx), %eax
	andq	$16842751, %rax         # imm = 0x100FFFF
	cmpq	$16777250, %rax         # imm = 0x1000022
	jne	.LBB78_160
# BB#153:                               # %if.end206
                                        #   in Loop: Header=BB78_151 Depth=1
	callq	start_sequence
	callq	gen_sibcall_epilogue
	movq	%rax, %r12
	callq	end_sequence
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_before
	movzbl	(%r12), %ecx
	cmpq	$24, %rcx
	je	.LBB78_155
# BB#154:                               # %if.end206
                                        #   in Loop: Header=BB78_151 Depth=1
	movq	%rax, %r12
.LBB78_155:                             # %if.end206
                                        #   in Loop: Header=BB78_151 Depth=1
	movzbl	(%r12), %eax
	cmpq	$24, %rax
	jne	.LBB78_159
# BB#156:                               # %if.then.i504
                                        #   in Loop: Header=BB78_151 Depth=1
	movq	8(%r12), %rax
	movq	sibcall_epilogue(%rip), %rdi
	movslq	(%rax), %rbx
	movq	(%rdi), %r13
	leaq	(%r13,%rbx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	testq	%rbx, %rbx
	movq	%rax, sibcall_epilogue(%rip)
	jle	.LBB78_160
# BB#157:                               # %while.body.lr.ph.i508
                                        #   in Loop: Header=BB78_151 Depth=1
	movq	8(%r12), %rdx
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	32(%rax,%r13,4), %rax
	decq	%rbx
	.align	16, 0x90
.LBB78_158:                             # %while.body.i518
                                        #   Parent Loop BB78_151 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rdx,%rbx,8), %rcx
	movl	8(%rcx), %ecx
	movl	%ecx, (%rax)
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$4, %rax
	decq	%rbx
	testq	%rsi, %rsi
	jg	.LBB78_158
	jmp	.LBB78_160
.LBB78_159:                             # %if.else.i530
                                        #   in Loop: Header=BB78_151 Depth=1
	movq	sibcall_epilogue(%rip), %rdi
	movq	(%rdi), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$4294967296, %rax       # imm = 0x100000000
	leaq	(%rbx,%rax), %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	varray_grow
	movq	%rax, sibcall_epilogue(%rip)
	movl	8(%r12), %edx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	%edx, 32(%rax,%rbx,4)
	jmp	.LBB78_160
.LBB78_161:                             # %for.end217
	testq	%r15, %r15
	je	.LBB78_186
# BB#162:
	movq	%r15, %rdi
	jmp	.LBB78_163
	.align	16, 0x90
.LBB78_169:                             # %for.cond221.backedge.thread
                                        #   in Loop: Header=BB78_163 Depth=1
	movq	%rdi, %rsi
	movq	%r15, %rdx
	callq	reorder_insns
	movq	%rbx, %rdi
.LBB78_163:                             # %for.body223
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %eax
	movq	16(%rdi), %rbx
	cmpq	$37, %rax
	jne	.LBB78_164
# BB#167:                               # %land.lhs.true230
                                        #   in Loop: Header=BB78_163 Depth=1
	movslq	40(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB78_164
# BB#168:                               # %if.then235
                                        #   in Loop: Header=BB78_163 Depth=1
	testq	%rbx, %rbx
	jne	.LBB78_169
	jmp	.LBB78_165
	.align	16, 0x90
.LBB78_164:                             # %for.cond221.backedge
                                        #   in Loop: Header=BB78_163 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB78_163
.LBB78_165:                             # %for.end241
	movq	basic_block_info(%rip), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB78_166
# BB#170:                               # %for.end241
	movabsq	$1, %rcx
	jmp	.LBB78_171
.LBB78_166:
	movabsq	$0, %rcx
.LBB78_171:                             # %for.end241
	cmpq	%r15, %rax
	je	.LBB78_180
# BB#172:                               # %for.end241
	testq	%rax, %rax
	je	.LBB78_180
# BB#173:
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	.align	16, 0x90
.LBB78_174:                             # %for.body250
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB78_176
# BB#175:                               # %land.lhs.true254
                                        #   in Loop: Header=BB78_174 Depth=1
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jg	.LBB78_186
.LBB78_176:                             # %for.inc261
                                        #   in Loop: Header=BB78_174 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rdx, %rcx
	je	.LBB78_178
# BB#177:                               # %for.inc261
                                        #   in Loop: Header=BB78_174 Depth=1
	movq	%rsi, %rcx
.LBB78_178:                             # %for.inc261
                                        #   in Loop: Header=BB78_174 Depth=1
	cmpq	%r15, %rax
	je	.LBB78_180
# BB#179:                               # %for.inc261
                                        #   in Loop: Header=BB78_174 Depth=1
	testq	%rax, %rax
	jne	.LBB78_174
.LBB78_180:                             # %for.end265
	testq	%rcx, %rcx
	jne	.LBB78_186
# BB#181:                               # %if.then267
	movq	%r15, %rdi
	callq	next_active_insn
	jmp	.LBB78_182
	.align	16, 0x90
.LBB78_196:                             # %for.inc288
                                        #   in Loop: Header=BB78_182 Depth=1
	movq	16(%rax), %rax
.LBB78_182:                             # %if.then267
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB78_186
# BB#183:                               # %for.body271
                                        #   in Loop: Header=BB78_182 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB78_196
# BB#184:                               # %land.lhs.true275
                                        #   in Loop: Header=BB78_182 Depth=1
	movslq	40(%rax), %rsi
	testq	%rsi, %rsi
	jle	.LBB78_196
# BB#185:                               # %if.then280
	movq	32(%rax), %rdi
	movq	%r15, %rdx
	callq	emit_line_note_after
.LBB78_186:                             # %if.end294
	movq	64(%rsp), %r14          # 8-byte Reload
	testq	%r14, %r14
	je	.LBB78_190
# BB#187:                               # %for.body300.lr.ph
	movq	%r14, %rbx
	.align	16, 0x90
.LBB78_188:                             # %for.body300
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movzbl	(%rdi), %eax
	movq	24(%rdi), %rbx
	cmpq	$37, %rax
	jne	.LBB78_189
# BB#191:                               # %land.lhs.true307
                                        #   in Loop: Header=BB78_188 Depth=1
	movslq	40(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB78_189
# BB#192:                               # %if.then312
                                        #   in Loop: Header=BB78_188 Depth=1
	movq	16(%r14), %rdx
	movq	%rdi, %rsi
	callq	reorder_insns
.LBB78_189:                             # %for.cond298.backedge
                                        #   in Loop: Header=BB78_188 Depth=1
	testq	%rbx, %rbx
	jne	.LBB78_188
.LBB78_190:                             # %if.end319
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB78_36:
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB78_37:                              # %for.end68
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %r14
	testq	%r14, %r14
	je	.LBB78_52
# BB#38:                                # %for.body71.lr.ph
	movabsq	$entry_exit_blocks, %rdx
	.align	16, 0x90
.LBB78_39:                              # %for.body71
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rbx
	movq	16(%rbx), %r13
	movq	(%rbx), %r14
	cmpq	%rdx, %r13
	je	.LBB78_51
# BB#40:                                # %if.end76
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	8(%r13), %r12
	movzbl	(%r12), %eax
	cmpq	$33, %rax
	jne	.LBB78_51
# BB#41:                                # %lor.lhs.false81
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	64(%r12), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB78_51
# BB#42:                                # %if.end87
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	%r12, %rdi
	callq	simplejump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB78_43
# BB#46:                                # %if.else
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	%r12, %rdi
	callq	condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$entry_exit_blocks, %rdx
	je	.LBB78_51
# BB#47:                                # %if.then94
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	32(%r12), %rax
	movq	16(%rax), %rcx
	movq	16(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB78_49
# BB#48:                                #   in Loop: Header=BB78_39 Depth=1
	addq	$16, %rcx
	jmp	.LBB78_50
.LBB78_43:                              # %if.then90
                                        #   in Loop: Header=BB78_39 Depth=1
	callq	gen_return
	movq	8(%r13), %rsi
	movq	%rax, %rdi
	callq	emit_jump_insn_after
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB78_45
# BB#44:                                # %if.then.i425
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	8(%r13), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx), %rdi
	movq	16(%rax), %rdx
	movl	40(%rcx), %esi
	callq	emit_line_note_after
.LBB78_45:                              # %emit_return_into_block.exit
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	%r12, %rdi
	callq	delete_insn
	movabsq	$entry_exit_blocks, %rdx
.LBB78_58:                              # %if.end139
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	%rbx, %rdi
	movabsq	$entry_exit_blocks+112, %rsi
	movq	%rdx, %rbx
	callq	redirect_edge_succ
	movq	%rbx, %rdx
	jmp	.LBB78_51
.LBB78_49:                              # %if.else111
                                        #   in Loop: Header=BB78_39 Depth=1
	addq	$24, %rcx
.LBB78_50:                              # %if.end115
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	$51, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$entry_exit_blocks, %rdx
	je	.LBB78_51
# BB#55:                                # %if.end120
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB78_57
# BB#56:                                # %if.then125
                                        #   in Loop: Header=BB78_39 Depth=1
	movl	32(%rax), %ecx
	decq	%rcx
	movl	%ecx, 32(%rax)
.LBB78_57:                              # %if.end132
                                        #   in Loop: Header=BB78_39 Depth=1
	movq	40(%r13), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB78_58
	.align	16, 0x90
.LBB78_51:                              # %for.cond69.backedge
                                        #   in Loop: Header=BB78_39 Depth=1
	testq	%r14, %r14
	jne	.LBB78_39
.LBB78_52:                              # %for.end141
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %rdi
	callq	emit_barrier_after
	callq	gen_return
	movq	8(%rbx), %rsi
	movq	%rax, %rdi
	callq	emit_jump_insn_after
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB78_54
# BB#53:                                # %if.then.i437
	movq	8(%rbx), %rax
	movq	32(%rcx), %rdi
	movq	16(%rax), %rdx
	movl	40(%rcx), %esi
	callq	emit_line_note_after
.LBB78_54:                              # %emit_return_into_block.exit438
	movq	40(%rbx), %rax
	movl	48(%rax), %ecx
	movq	8(%rbx), %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rcx, %rdx
	movl	%edx, 48(%rax)
	jmp	.LBB78_150
.LBB78_9:                               # %if.then6
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.thread_prologue_and_epilogue_insns, %rcx
	movq	$7594, %rsi             # imm = 0x1DAA
	jmp	.LBB78_10
.LBB78_131:                             # %if.then342.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.keep_stack_depressed, %rcx
	movq	$7446, %rsi             # imm = 0x1D16
	jmp	.LBB78_10
.LBB78_193:                             # %if.else137.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.keep_stack_depressed, %rcx
	movq	$7377, %rsi             # imm = 0x1CD1
	jmp	.LBB78_10
.LBB78_113:                             # %if.then264.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.keep_stack_depressed, %rcx
	movq	$7412, %rsi             # imm = 0x1CF4
	jmp	.LBB78_10
.LBB78_120:                             # %if.then305.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.keep_stack_depressed, %rcx
	movq	$7425, %rsi             # imm = 0x1D01
.LBB78_10:                              # %if.then6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp711:
	.size	thread_prologue_and_epilogue_insns, .Ltmp711-thread_prologue_and_epilogue_insns
	.cfi_endproc

	.globl	reposition_prologue_and_epilogue_notes
	.align	16, 0x90
	.type	reposition_prologue_and_epilogue_notes,@function
reposition_prologue_and_epilogue_notes: # @reposition_prologue_and_epilogue_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp718:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp719:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp720:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp721:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp722:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp723:
	.cfi_def_cfa_offset 80
.Ltmp724:
	.cfi_offset %rbx, -48
.Ltmp725:
	.cfi_offset %r12, -40
.Ltmp726:
	.cfi_offset %r13, -32
.Ltmp727:
	.cfi_offset %r14, -24
.Ltmp728:
	.cfi_offset %r15, -16
	movq	prologue(%rip), %rbx
	movq	(%rbx), %r13
	movq	%rdi, %r11
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB79_35
# BB#1:                                 # %entry
	testq	%r11, %r11
	je	.LBB79_35
# BB#2:                                 # %for.body.lr.ph
	xorq	%rdi, %rdi
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	addq	%r10, %r13
	movabsq	$0, %r12
	movabsq	$1, %r14
	movabsq	$4294967206, %r9        # imm = 0xFFFFFFA6
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB79_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB79_21 Depth 2
                                        #     Child Loop BB79_13 Depth 2
                                        #       Child Loop BB79_15 Depth 3
	movzbl	(%r11), %eax
	cmpq	$32, %rax
	jne	.LBB79_4
# BB#9:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	32(%r11), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB79_18
# BB#10:                                # %if.then.i
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	8(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movslq	(%rax), %rax
	testq	%rax, %rax
	jle	.LBB79_8
# BB#11:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rdx
                                        # kill: R13<def> RDX<kill>
	movq	%rdx, %r15
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%r8, %r8
	.align	16, 0x90
.LBB79_13:                              # %for.body.i
                                        #   Parent Loop BB79_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB79_15 Depth 3
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	js	.LBB79_12
# BB#14:                                # %for.body14.lr.ph.i
                                        #   in Loop: Header=BB79_13 Depth=2
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rcx
	movl	8(%rcx), %r9d
	movq	%r13, %rdi
	movq	%r8, %rcx
	.align	16, 0x90
.LBB79_15:                              # %for.body14.i
                                        #   Parent Loop BB79_3 Depth=1
                                        #     Parent Loop BB79_13 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	32(%rbx,%rdi,4), %edx
	movq	%r9, %rsi
	andq	%r10, %rsi
	cmpq	%rdx, %rsi
	movq	%r12, %r8
	jne	.LBB79_17
# BB#16:                                # %for.body14.i
                                        #   in Loop: Header=BB79_15 Depth=3
	movq	%r14, %r8
.LBB79_17:                              # %for.body14.i
                                        #   in Loop: Header=BB79_15 Depth=3
	addq	%rcx, %r8
	movq	$32, %rcx
	leaq	-1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rdx, %rdi
	movq	%r8, %rcx
	jg	.LBB79_15
.LBB79_12:                              # %for.cond.loopexit.i
                                        #   in Loop: Header=BB79_13 Depth=2
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jg	.LBB79_13
# BB#22:                                # %contains.exit
                                        #   in Loop: Header=BB79_3 Depth=1
	testq	%r10, %r8
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	(%rsp), %rdx            # 8-byte Reload
	movabsq	$4294967206, %r9        # imm = 0xFFFFFFA6
	movq	%r15, %r13
	je	.LBB79_8
	jmp	.LBB79_23
	.align	16, 0x90
.LBB79_4:                               # %for.body
                                        #   in Loop: Header=BB79_3 Depth=1
	cmpq	$37, %rax
	jne	.LBB79_18
# BB#5:                                 # %if.then4
                                        #   in Loop: Header=BB79_3 Depth=1
	movl	40(%r11), %eax
	cmpq	%r9, %rax
	movq	%r11, %rax
	je	.LBB79_7
# BB#6:                                 # %select.mid
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	%rdi, %rax
.LBB79_7:                               # %select.end
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	%rax, %rdi
	jmp	.LBB79_8
	.align	16, 0x90
.LBB79_18:                              # %if.else.i
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB79_8
# BB#19:                                # %for.body39.lr.ph.i
                                        #   in Loop: Header=BB79_3 Depth=1
	movl	8(%r11), %r8d
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	.align	16, 0x90
.LBB79_21:                              # %for.body39.i
                                        #   Parent Loop BB79_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	32(%rbx,%rsi,4), %ecx
	movq	%r8, %rax
	andq	%r10, %rax
	cmpq	%rcx, %rax
	je	.LBB79_23
# BB#20:                                # %for.cond36.i
                                        #   in Loop: Header=BB79_21 Depth=2
	movq	$32, %rcx
	leaq	-1(%rsi), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	movq	%rax, %rsi
	jg	.LBB79_21
	jmp	.LBB79_8
.LBB79_23:                              # %if.then9
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	decq	%rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%r10, %rax
	movq	%r11, %rdx
	je	.LBB79_24
	.align	16, 0x90
.LBB79_8:                               # %for.inc
                                        #   in Loop: Header=BB79_3 Depth=1
	movq	24(%r11), %r11
	testq	%r11, %r11
	jne	.LBB79_3
	jmp	.LBB79_25
.LBB79_24:
	movq	%r11, %rdx
.LBB79_25:                              # %for.end
	testq	%rdx, %rdx
	je	.LBB79_35
# BB#26:                                # %if.then19
	testq	%rdi, %rdi
	jne	.LBB79_32
# BB#27:                                # %for.cond23.preheader
	movq	24(%rdx), %rax
	xorq	%rdi, %rdi
	jmp	.LBB79_28
.LBB79_69:                              # %for.cond23.backedge
                                        #   in Loop: Header=BB79_28 Depth=1
	movq	24(%rax), %rax
.LBB79_28:                              # %for.cond23.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB79_32
# BB#29:                                # %for.body28
                                        #   in Loop: Header=BB79_28 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB79_69
# BB#30:                                # %land.lhs.true
                                        #   in Loop: Header=BB79_28 Depth=1
	movl	40(%rax), %ecx
	cmpq	%r9, %rcx
	jne	.LBB79_69
# BB#31:
	movq	%rax, %rdi
.LBB79_32:                              # %if.end41
	movzbl	(%rdx), %eax
	cmpq	$36, %rax
	jne	.LBB79_34
# BB#33:                                # %if.then49
	movq	24(%rdx), %rdx
.LBB79_34:                              # %if.end53
	movq	%rdi, %rsi
	callq	reorder_insns
.LBB79_35:                              # %if.end55
	movq	epilogue(%rip), %rax
	movq	(%rax), %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB79_71
# BB#36:                                # %if.then60
	callq	get_last_insn
	testq	%rax, %rax
	je	.LBB79_71
# BB#37:                                # %for.body64.lr.ph
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	epilogue(%rip), %r11
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$0, %r15
	movabsq	$1, %r10
	movabsq	$4294967207, %r8        # imm = 0xFFFFFFA7
	xorq	%r9, %r9
	.align	16, 0x90
.LBB79_38:                              # %for.body64
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB79_56 Depth 2
                                        #     Child Loop BB79_48 Depth 2
                                        #       Child Loop BB79_50 Depth 3
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB79_39
# BB#44:                                # %land.lhs.true.i171
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	32(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$24, %rdx
	jne	.LBB79_53
# BB#45:                                # %if.then.i176
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	8(%rcx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movslq	(%rcx), %rbx
	testq	%rbx, %rbx
	jle	.LBB79_43
# BB#46:                                # %for.body.lr.ph.i184
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	(%r11), %r12
	addq	%r14, %r12
	movq	$32, %rcx
	movq	%r12, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%r8, %r8
	.align	16, 0x90
.LBB79_48:                              # %for.body.i191
                                        #   Parent Loop BB79_38 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB79_50 Depth 3
	movq	$32, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	js	.LBB79_47
# BB#49:                                # %for.body14.lr.ph.i195
                                        #   in Loop: Header=BB79_48 Depth=2
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rbx,8), %rcx
	movl	8(%rcx), %r9d
	movq	%r13, %rdi
	movq	%r8, %rcx
	.align	16, 0x90
.LBB79_50:                              # %for.body14.i204
                                        #   Parent Loop BB79_38 Depth=1
                                        #     Parent Loop BB79_48 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	32(%r11,%rdi,4), %esi
	movq	%r9, %rdx
	andq	%r14, %rdx
	cmpq	%rsi, %rdx
	movq	%r15, %r8
	jne	.LBB79_52
# BB#51:                                # %for.body14.i204
                                        #   in Loop: Header=BB79_50 Depth=3
	movq	%r10, %r8
.LBB79_52:                              # %for.body14.i204
                                        #   in Loop: Header=BB79_50 Depth=3
	addq	%rcx, %r8
	movq	$32, %rcx
	leaq	-1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rdx, %rdi
	movq	%r8, %rcx
	jg	.LBB79_50
.LBB79_47:                              # %for.cond.loopexit.i187
                                        #   in Loop: Header=BB79_48 Depth=2
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jg	.LBB79_48
# BB#57:                                # %contains.exit224
                                        #   in Loop: Header=BB79_38 Depth=1
	testq	%r14, %r8
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$4294967207, %r8        # imm = 0xFFFFFFA7
	movq	(%rsp), %r9             # 8-byte Reload
	je	.LBB79_43
	jmp	.LBB79_58
	.align	16, 0x90
.LBB79_39:                              # %for.body64
                                        #   in Loop: Header=BB79_38 Depth=1
	cmpq	$37, %rcx
	jne	.LBB79_53
# BB#40:                                # %if.then69
                                        #   in Loop: Header=BB79_38 Depth=1
	movl	40(%rax), %ecx
	cmpq	%r8, %rcx
	movq	%rax, %rcx
	je	.LBB79_42
# BB#41:                                # %select.mid30
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	%rdi, %rcx
.LBB79_42:                              # %select.end29
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	%rcx, %rdi
	jmp	.LBB79_43
	.align	16, 0x90
.LBB79_53:                              # %if.else.i209
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	(%r11), %rbx
	addq	%r14, %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB79_43
# BB#54:                                # %for.body39.lr.ph.i215
                                        #   in Loop: Header=BB79_38 Depth=1
	movl	8(%rax), %esi
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB79_56:                              # %for.body39.i222
                                        #   Parent Loop BB79_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	32(%r11,%rbx,4), %ecx
	movq	%rsi, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	je	.LBB79_58
# BB#55:                                # %for.cond36.i218
                                        #   in Loop: Header=BB79_56 Depth=2
	movq	$32, %rcx
	leaq	-1(%rbx), %rdx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rdx, %rbx
	jg	.LBB79_56
	jmp	.LBB79_43
.LBB79_58:                              # %if.then80
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	decq	%rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	testq	%r14, %rcx
	movq	%rax, %r9
	je	.LBB79_59
	.align	16, 0x90
.LBB79_43:                              # %for.inc88
                                        #   in Loop: Header=BB79_38 Depth=1
	movq	16(%rax), %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	jne	.LBB79_38
	jmp	.LBB79_60
.LBB79_59:
	movq	%rax, %rcx
	movq	%rax, %r9
.LBB79_60:                              # %for.end92
	testq	%r9, %r9
	je	.LBB79_71
# BB#61:                                # %if.then94
	testq	%rdi, %rdi
	jne	.LBB79_67
# BB#62:                                # %for.cond98.preheader
	movq	16(%rcx), %rax
	xorq	%rdi, %rdi
	jmp	.LBB79_63
.LBB79_70:                              # %for.cond98.backedge
                                        #   in Loop: Header=BB79_63 Depth=1
	movq	16(%rax), %rax
.LBB79_63:                              # %for.cond98.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB79_67
# BB#64:                                # %for.body103
                                        #   in Loop: Header=BB79_63 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB79_70
# BB#65:                                # %land.lhs.true108
                                        #   in Loop: Header=BB79_63 Depth=1
	movl	40(%rax), %ecx
	cmpq	%r8, %rcx
	jne	.LBB79_70
# BB#66:
	movq	%rax, %rdi
.LBB79_67:                              # %if.end117
	movq	16(%r9), %rdx
	cmpq	%rdi, %rdx
	je	.LBB79_71
# BB#68:                                # %if.then123
	movq	%rdi, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	reorder_insns  # TAILCALL
.LBB79_71:                              # %if.end129
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp729:
	.size	reposition_prologue_and_epilogue_notes, .Ltmp729-reposition_prologue_and_epilogue_notes
	.cfi_endproc

	.globl	ggc_mark_struct_function
	.align	16, 0x90
	.type	ggc_mark_struct_function,@function
ggc_mark_struct_function:               # @ggc_mark_struct_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp736:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp737:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp738:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp739:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp740:
	.cfi_def_cfa_offset 48
.Ltmp741:
	.cfi_offset %rbx, -48
.Ltmp742:
	.cfi_offset %r12, -40
.Ltmp743:
	.cfi_offset %r13, -32
.Ltmp744:
	.cfi_offset %r14, -24
.Ltmp745:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB80_1:                               # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB80_14 Depth 2
                                        #     Child Loop BB80_73 Depth 2
                                        #     Child Loop BB80_92 Depth 2
	testq	%r14, %r14
	je	.LBB80_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
.LBB80_3:                               # %do.body1
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	48(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_8
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_8
# BB#5:                                 # %do.body5
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_7
# BB#6:                                 # %if.then7
                                        #   in Loop: Header=BB80_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_7:                               # %if.end10
                                        #   in Loop: Header=BB80_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB80_8:                               # %do.end14
                                        #   in Loop: Header=BB80_1 Depth=1
	testq	%r14, %r14
	je	.LBB80_112
# BB#9:                                 # %do.body.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	80(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_12
# BB#10:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_12
# BB#11:                                # %if.then2.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_12:                              # %do.end.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	312(%r14), %r13
	testq	%r13, %r13
	je	.LBB80_18
# BB#13:                                # %if.then5.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movslq	304(%r14), %rbx
	testq	%rbx, %rbx
	jle	.LBB80_18
	.align	16, 0x90
.LBB80_14:                              # %do.body8.i
                                        #   Parent Loop BB80_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13), %r15
	testq	%r15, %r15
	je	.LBB80_17
# BB#15:                                # %land.lhs.true11.i
                                        #   in Loop: Header=BB80_14 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_17
# BB#16:                                # %if.then14.i
                                        #   in Loop: Header=BB80_14 Depth=2
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB80_17:                              # %for.inc.i
                                        #   in Loop: Header=BB80_14 Depth=2
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$8, %r13
	testq	%rax, %rax
	jg	.LBB80_14
.LBB80_18:                              # %do.body18.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	120(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_21
# BB#19:                                # %land.lhs.true21.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_21
# BB#20:                                # %if.then24.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_21:                              # %do.body27.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	192(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_24
# BB#22:                                # %land.lhs.true30.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_24
# BB#23:                                # %if.then33.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_24:                              # %do.body36.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	200(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_27
# BB#25:                                # %land.lhs.true39.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_27
# BB#26:                                # %if.then42.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_27:                              # %do.body45.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	208(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_30
# BB#28:                                # %land.lhs.true48.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_30
# BB#29:                                # %if.then51.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_30:                              # %do.body54.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	216(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_33
# BB#31:                                # %land.lhs.true57.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_33
# BB#32:                                # %if.then60.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_33:                              # %do.body63.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	288(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_36
# BB#34:                                # %land.lhs.true66.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_36
# BB#35:                                # %if.then69.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_36:                              # %do.body72.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	232(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_39
# BB#37:                                # %land.lhs.true75.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_39
# BB#38:                                # %if.then78.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_39:                              # %do.body81.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	240(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_42
# BB#40:                                # %land.lhs.true84.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_42
# BB#41:                                # %if.then87.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_42:                              # %do.body90.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	128(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_45
# BB#43:                                # %land.lhs.true93.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_45
# BB#44:                                # %if.then96.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_45:                              # %do.body99.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	248(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_48
# BB#46:                                # %land.lhs.true102.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_48
# BB#47:                                # %if.then105.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_48:                              # %do.body108.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	224(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_53
# BB#49:                                # %land.lhs.true110.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_53
# BB#50:                                # %do.body114.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_52
# BB#51:                                # %if.then116.i
                                        #   in Loop: Header=BB80_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_52:                              # %if.end119.i
                                        #   in Loop: Header=BB80_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB80_53:                              # %do.body124.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	296(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_56
# BB#54:                                # %land.lhs.true127.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_56
# BB#55:                                # %if.then130.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_56:                              # %do.body133.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	272(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_61
# BB#57:                                # %land.lhs.true136.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_61
# BB#58:                                # %do.body140.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_60
# BB#59:                                # %if.then144.i
                                        #   in Loop: Header=BB80_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_60:                              # %if.end148.i
                                        #   in Loop: Header=BB80_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB80_61:                              # %do.body157.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	280(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_66
# BB#62:                                # %land.lhs.true160.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_66
# BB#63:                                # %do.body164.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_65
# BB#64:                                # %if.then168.i
                                        #   in Loop: Header=BB80_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_65:                              # %if.end172.i
                                        #   in Loop: Header=BB80_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB80_66:                              # %do.body181.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	416(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_69
# BB#67:                                # %land.lhs.true184.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_69
# BB#68:                                # %if.then187.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_69:                              # %do.body190.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	256(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_72
# BB#70:                                # %land.lhs.true193.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_72
# BB#71:                                # %if.then196.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_72:                              # %do.end198.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	320(%r14), %rbx
	jmp	.LBB80_73
	.align	16, 0x90
.LBB80_90:                              # %for.inc274.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	(%rbx), %rbx
.LBB80_73:                              # %do.end198.i
                                        #   Parent Loop BB80_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB80_91
# BB#74:                                # %if.then204.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movq	8(%rbx), %r15
	testq	%r15, %r15
	je	.LBB80_77
# BB#75:                                # %land.lhs.true211.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_77
# BB#76:                                # %if.then214.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB80_77:                              # %do.body217.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	16(%rbx), %r15
	testq	%r15, %r15
	je	.LBB80_80
# BB#78:                                # %land.lhs.true220.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_80
# BB#79:                                # %if.then223.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB80_80:                              # %do.body226.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	48(%rbx), %r15
	testq	%r15, %r15
	je	.LBB80_85
# BB#81:                                # %land.lhs.true229.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_85
# BB#82:                                # %do.body233.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_84
# BB#83:                                # %if.then237.i
                                        #   in Loop: Header=BB80_73 Depth=2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_84:                              # %if.end241.i
                                        #   in Loop: Header=BB80_73 Depth=2
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r15, 32(%rax,%rcx,8)
.LBB80_85:                              # %do.body250.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	40(%rbx), %r15
	testq	%r15, %r15
	je	.LBB80_90
# BB#86:                                # %land.lhs.true253.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_90
# BB#87:                                # %do.body257.i
                                        #   in Loop: Header=BB80_73 Depth=2
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_89
# BB#88:                                # %if.then261.i
                                        #   in Loop: Header=BB80_73 Depth=2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_89:                              # %if.end265.i
                                        #   in Loop: Header=BB80_73 Depth=2
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r15, 32(%rax,%rcx,8)
	jmp	.LBB80_90
	.align	16, 0x90
.LBB80_91:                              # %for.end275.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	344(%r14), %rbx
	jmp	.LBB80_92
	.align	16, 0x90
.LBB80_96:                              # %for.inc295.i
                                        #   in Loop: Header=BB80_92 Depth=2
	movq	16(%rbx), %rbx
.LBB80_92:                              # %for.end275.i
                                        #   Parent Loop BB80_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB80_97
# BB#93:                                # %if.then282.i
                                        #   in Loop: Header=BB80_92 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movq	(%rbx), %r15
	testq	%r15, %r15
	je	.LBB80_96
# BB#94:                                # %land.lhs.true289.i
                                        #   in Loop: Header=BB80_92 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_96
# BB#95:                                # %if.then292.i
                                        #   in Loop: Header=BB80_92 Depth=2
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
	jmp	.LBB80_96
	.align	16, 0x90
.LBB80_97:                              # %do.body298.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	168(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_100
# BB#98:                                # %land.lhs.true301.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_100
# BB#99:                                # %if.then304.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_100:                             # %do.body307.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	176(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_103
# BB#101:                               # %land.lhs.true310.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_103
# BB#102:                               # %if.then313.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_103:                             # %do.body316.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	184(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_106
# BB#104:                               # %land.lhs.true319.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_106
# BB#105:                               # %if.then322.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB80_106:                             # %do.body325.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	160(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_111
# BB#107:                               # %land.lhs.true328.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_111
# BB#108:                               # %do.body332.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_110
# BB#109:                               # %if.then336.i
                                        #   in Loop: Header=BB80_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_110:                             # %if.end340.i
                                        #   in Loop: Header=BB80_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB80_111:                             # %do.end348.i
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%r14, %rdi
	callq	mark_hard_reg_initial_vals
.LBB80_112:                             # %mark_function_status.exit
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	(%r14), %rdi
	callq	mark_eh_status
	movq	8(%r14), %rdi
	callq	mark_stmt_status
	movq	16(%r14), %rdi
	callq	mark_expr_status
	movq	24(%r14), %rdi
	callq	mark_emit_status
	movq	32(%r14), %rdi
	callq	mark_varasm_status
	movq	mark_machine_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB80_114
# BB#113:                               # %if.then16
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%r14, %rdi
	callq	*%rax
.LBB80_114:                             # %if.end17
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	mark_lang_status(%rip), %rax
	testq	%rax, %rax
	je	.LBB80_116
# BB#115:                               # %if.then19
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%r14, %rdi
	callq	*%rax
.LBB80_116:                             # %if.end20
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	360(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_119
# BB#117:                               # %do.body23
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_119
# BB#118:                               # %if.then29
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtvec_children
.LBB80_119:                             # %if.end32
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	368(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB80_124
# BB#120:                               # %land.lhs.true39
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB80_124
# BB#121:                               # %do.body43
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB80_123
# BB#122:                               # %if.then47
                                        #   in Loop: Header=BB80_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB80_123:                             # %if.end51
                                        #   in Loop: Header=BB80_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB80_124:                             # %if.end60
                                        #   in Loop: Header=BB80_1 Depth=1
	movq	56(%r14), %r14
	testq	%r14, %r14
	jne	.LBB80_1
# BB#125:                               # %if.end64
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp746:
	.size	ggc_mark_struct_function, .Ltmp746-ggc_mark_struct_function
	.cfi_endproc

	.globl	init_function_once
	.align	16, 0x90
	.type	init_function_once,@function
init_function_once:                     # @init_function_once
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp749:
	.cfi_def_cfa_offset 16
.Ltmp750:
	.cfi_offset %rbx, -16
	movabsq	$cfun, %rdi
	movabsq	$maybe_mark_struct_function, %rbx
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rbx, %rcx
	callq	ggc_add_root
	movabsq	$outer_function_chain, %rdi
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rbx, %rcx
	callq	ggc_add_root
	movabsq	$.L.str9, %rdx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	varray_init
	movq	%rax, prologue(%rip)
	movabsq	$.L.str10, %rdx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	varray_init
	movq	%rax, epilogue(%rip)
	movabsq	$.L.str11, %rdx
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	varray_init
	movq	%rax, sibcall_epilogue(%rip)
	popq	%rbx
	retq
.Ltmp751:
	.size	init_function_once, .Ltmp751-init_function_once
	.cfi_endproc

	.align	16, 0x90
	.type	maybe_mark_struct_function,@function
maybe_mark_struct_function:             # @maybe_mark_struct_function
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB82_1
# BB#2:                                 # %if.end
	jmp	ggc_mark_struct_function  # TAILCALL
.LBB82_1:                               # %return
	retq
.Ltmp752:
	.size	maybe_mark_struct_function, .Ltmp752-maybe_mark_struct_function
	.cfi_endproc

	.align	16, 0x90
	.type	handle_epilogue_set,@function
handle_epilogue_set:                    # @handle_epilogue_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp758:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp759:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp760:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp761:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp762:
	.cfi_def_cfa_offset 48
.Ltmp763:
	.cfi_offset %rbx, -40
.Ltmp764:
	.cfi_offset %r12, -32
.Ltmp765:
	.cfi_offset %r14, -24
.Ltmp766:
	.cfi_offset %r15, -16
	movq	global_rtl+16(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	reg_set_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB83_13
# BB#1:                                 # %if.then
	movq	8(%rbx), %rax
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB83_2
# BB#4:                                 # %if.end
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB83_7
# BB#5:                                 # %land.lhs.true
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB83_7
# BB#6:                                 # %if.then15
	movq	8(%rax), %rax
	movq	%rax, 16(%r14)
	movq	16(%rbx), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 24(%r14)
	jmp	.LBB83_8
.LBB83_7:                               # %if.else
	movq	%rax, 16(%r14)
	movq	$0, 24(%r14)
	xorq	%rcx, %rcx
.LBB83_8:                               # %if.end35
	movq	global_rtl+16(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB83_10
# BB#9:                                 # %if.then38
	movq	(%r14), %rax
	movq	%rax, 16(%r14)
	movq	8(%r14), %rdx
	addq	%rdx, %rcx
	movq	%rcx, 24(%r14)
.LBB83_10:                              # %if.end41
	testq	%rax, %rax
	je	.LBB83_12
# BB#11:                                # %lor.lhs.false
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB83_20
.LBB83_12:                              # %if.then48
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.handle_epilogue_set, %rcx
	movq	$7504, %rsi             # imm = 0x1D50
	jmp	.LBB83_3
.LBB83_13:                              # %if.else50
	movq	16(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB83_18
# BB#14:                                # %land.lhs.true53
	movq	%rbx, %rsi
	callq	reg_set_p
	testq	%r15, %rax
	je	.LBB83_18
# BB#15:                                # %if.then57
	movq	16(%r14), %rdi
	movq	8(%rbx), %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB83_17
# BB#16:                                # %lor.lhs.false64
	cmpq	$0, 32(%r14)
	jne	.LBB83_17
# BB#19:                                # %if.else67
	movq	16(%rbx), %rbx
	movq	global_rtl+16(%rip), %r15
	movq	(%r14), %rdi
	movq	8(%r14), %rsi
	callq	plus_constant_wide
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	simplify_replace_rtx
	movq	%rax, 32(%r14)
.LBB83_20:                              # %if.end100
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB83_18:                              # %if.else77
	movq	16(%rbx), %r15
	movq	global_rtl+16(%rip), %r12
	movq	(%r14), %rdi
	movq	8(%r14), %rsi
	callq	plus_constant_wide
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	simplify_replace_rtx
	movq	%rax, 16(%rbx)
	movq	8(%rbx), %r15
	movq	global_rtl+16(%rip), %r12
	movq	(%r14), %rdi
	movq	8(%r14), %rsi
	callq	plus_constant_wide
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	simplify_replace_rtx
	movq	%rax, 8(%rbx)
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	emit_insn  # TAILCALL
.LBB83_2:                               # %if.then1
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.handle_epilogue_set, %rcx
	movq	$7485, %rsi             # imm = 0x1D3D
	jmp	.LBB83_3
.LBB83_17:                              # %if.then66
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.handle_epilogue_set, %rcx
	movq	$7519, %rsi             # imm = 0x1D5F
.LBB83_3:                               # %if.then1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp767:
	.size	handle_epilogue_set, .Ltmp767-handle_epilogue_set
	.cfi_endproc

	.align	16, 0x90
	.type	all_blocks,@function
all_blocks:                             # @all_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp773:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp774:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp775:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp776:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp777:
	.cfi_def_cfa_offset 48
.Ltmp778:
	.cfi_offset %rbx, -40
.Ltmp779:
	.cfi_offset %r12, -32
.Ltmp780:
	.cfi_offset %r14, -24
.Ltmp781:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB84_6
# BB#1:                                 # %while.body.lr.ph
	xorq	%r14, %r14
	testq	%r15, %r15
	je	.LBB84_2
# BB#4:
	movabsq	$4294950911, %r12       # imm = 0xFFFFBFFF
	.align	16, 0x90
.LBB84_5:                               # %cond.end
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	andq	%r12, %rax
	movl	%eax, 16(%rbx)
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rbx, (%r15,%rax,8)
	leaq	1(%r14), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	40(%rbx), %rdi
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r15,%rax,8), %rsi
	callq	all_blocks
	movq	(%rbx), %rbx
	leaq	1(%rax,%r14), %r14
	testq	%rbx, %rbx
	jne	.LBB84_5
	jmp	.LBB84_6
.LBB84_2:
	movabsq	$4294950911, %r15       # imm = 0xFFFFBFFF
	.align	16, 0x90
.LBB84_3:                               # %cond.end.us
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	andq	%r15, %rax
	movl	%eax, 16(%rbx)
	movq	40(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	all_blocks
	movq	(%rbx), %rbx
	leaq	1(%r14,%rax), %r14
	testq	%rbx, %rbx
	jne	.LBB84_3
.LBB84_6:                               # %while.end
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp782:
	.size	all_blocks, .Ltmp782-all_blocks
	.cfi_endproc

	.align	16, 0x90
	.type	instantiate_decl,@function
instantiate_decl:                       # @instantiate_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp788:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp789:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp790:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp791:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp792:
	.cfi_def_cfa_offset 48
.Ltmp793:
	.cfi_offset %rbx, -40
.Ltmp794:
	.cfi_offset %r12, -32
.Ltmp795:
	.cfi_offset %r14, -24
.Ltmp796:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%r12, %r12
	je	.LBB85_10
# BB#1:                                 # %lor.lhs.false
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	jne	.LBB85_10
# BB#2:                                 # %if.end
	movq	8(%r12), %rdi
	movq	%rdi, (%rsp)
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB85_8
# BB#3:                                 # %if.end
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$16, %rcx
	ja	.LBB85_12
# BB#4:                                 # %if.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB85_10
# BB#5:                                 # %if.end
	cmpq	$7, %rcx
	jne	.LBB85_6
# BB#11:                                # %land.lhs.true47
	movl	8(%rdi), %ecx
	addq	$-53, %rcx
	andq	%rax, %rcx
	cmpq	$4, %rcx
	ja	.LBB85_10
	jmp	.LBB85_12
.LBB85_8:                               # %if.end
	cmpq	$134, %rcx
	je	.LBB85_10
# BB#9:                                 # %if.end
	cmpq	$140, %rcx
	jne	.LBB85_12
	jmp	.LBB85_10
.LBB85_6:                               # %if.end
	cmpq	$16, %rcx
	jne	.LBB85_12
# BB#7:                                 # %land.lhs.true
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB85_10
.LBB85_12:                              # %if.end57
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rdx
	je	.LBB85_13
# BB#15:                                # %if.end59
	callq	copy_rtx
	movq	%rax, (%rsp)
	leaq	(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	testq	%r14, %r14
	js	.LBB85_14
# BB#16:                                # %if.then64
	movl	class_narrowest_mode+4(%rip), %edi
	jmp	.LBB85_17
.LBB85_20:                              # %for.inc
                                        #   in Loop: Header=BB85_17 Depth=1
	movzbl	mode_wider_mode(%rbx), %edi
.LBB85_17:                              # %if.then64
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB85_21
# BB#18:                                # %land.rhs
                                        #   in Loop: Header=BB85_17 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_size(%rbx), %eax
	cmpq	%r14, %rax
	ja	.LBB85_21
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB85_17 Depth=1
	movq	(%rsp), %rsi
	callq	memory_address_p
	testq	%r15, %rax
	jne	.LBB85_20
	jmp	.LBB85_10
.LBB85_13:                              # %if.end59.thread
	leaq	(%rsp), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	instantiate_virtual_regs_1
	jmp	.LBB85_14
.LBB85_21:                              # %for.end
	movl	class_narrowest_mode+8(%rip), %edi
	jmp	.LBB85_22
.LBB85_25:                              # %for.inc91
                                        #   in Loop: Header=BB85_22 Depth=1
	movzbl	mode_wider_mode(%rbx), %edi
.LBB85_22:                              # %for.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB85_14
# BB#23:                                # %land.rhs79
                                        #   in Loop: Header=BB85_22 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_size(%rbx), %eax
	cmpq	%r14, %rax
	ja	.LBB85_14
# BB#24:                                # %for.body86
                                        #   in Loop: Header=BB85_22 Depth=1
	movq	(%rsp), %rsi
	callq	memory_address_p
	testq	%r15, %rax
	jne	.LBB85_25
	jmp	.LBB85_10
.LBB85_14:                              # %if.end96
	movq	(%rsp), %rax
	movq	%rax, 8(%r12)
.LBB85_10:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp797:
	.size	instantiate_decl, .Ltmp797-instantiate_decl
	.cfi_endproc

	.align	16, 0x90
	.type	instantiate_decls_1,@function
instantiate_decls_1:                    # @instantiate_decls_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp803:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp804:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp805:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp806:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp807:
	.cfi_def_cfa_offset 48
.Ltmp808:
	.cfi_offset %rbx, -40
.Ltmp809:
	.cfi_offset %r12, -32
.Ltmp810:
	.cfi_offset %r14, -24
.Ltmp811:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	32(%r14), %rbx
	movq	%rsi, %r15
	jmp	.LBB86_1
	.align	16, 0x90
.LBB86_4:                               # %for.inc
                                        #   in Loop: Header=BB86_1 Depth=1
	movq	(%rbx), %rbx
.LBB86_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB86_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB86_1 Depth=1
	movq	144(%rbx), %r12
	testq	%r12, %r12
	je	.LBB86_4
# BB#3:                                 # %cond.end
                                        #   in Loop: Header=BB86_1 Depth=1
	movq	8(%rbx), %rdi
	callq	int_size_in_bytes
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	instantiate_decl
	jmp	.LBB86_4
.LBB86_5:                               # %for.end
	movq	40(%r14), %rbx
	jmp	.LBB86_7
	.align	16, 0x90
.LBB86_6:                               # %for.body12
                                        #   in Loop: Header=BB86_7 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	instantiate_decls_1
	movq	(%rbx), %rbx
.LBB86_7:                               # %for.body12
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB86_6
# BB#8:                                 # %for.end16
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp812:
	.size	instantiate_decls_1, .Ltmp812-instantiate_decls_1
	.cfi_endproc

	.align	16, 0x90
	.type	insns_for_mem_walk,@function
insns_for_mem_walk:                     # @insns_for_mem_walk
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp816:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp817:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp818:
	.cfi_def_cfa_offset 32
.Ltmp819:
	.cfi_offset %rbx, -24
.Ltmp820:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movl	16(%rbx), %eax
	cmpq	$1, %rax
	je	.LBB87_7
# BB#1:                                 # %entry
	testq	%rax, %rax
	jne	.LBB87_14
# BB#2:                                 # %land.lhs.true
	movq	(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB87_14
# BB#3:                                 # %land.lhs.true1
	movzbl	(%rcx), %edx
	cmpq	$70, %rdx
	jne	.LBB87_14
# BB#4:                                 # %land.lhs.true3
	movq	8(%rcx), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$61, %rcx
	jne	.LBB87_6
# BB#5:                                 # %if.then
	movq	(%rbx), %rdi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	callq	hash_lookup
	jmp	.LBB87_14
.LBB87_6:                               # %if.else
	cmpq	$1, %rax
	jne	.LBB87_14
.LBB87_7:                               # %land.lhs.true12
	movq	(%rdi), %rsi
	testq	%rsi, %rsi
	je	.LBB87_14
# BB#8:                                 # %land.lhs.true14
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB87_14
# BB#9:                                 # %if.then18
	movq	(%rbx), %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	hash_lookup
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB87_14
# BB#10:                                # %land.lhs.true22
	movq	24(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB87_11
# BB#12:                                # %lor.lhs.false
	movq	8(%rcx), %rax
	movq	8(%rbx), %rdx
	cmpq	%rdx, %rax
	jne	.LBB87_13
	jmp	.LBB87_14
.LBB87_11:                              # %land.lhs.true22.if.then29_crit_edge
	movq	8(%rbx), %rdx
.LBB87_13:                              # %if.then29
	movq	$3, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%r14)
.LBB87_14:                              # %if.end35
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp821:
	.size	insns_for_mem_walk, .Ltmp821-insns_for_mem_walk
	.cfi_endproc

	.align	16, 0x90
	.type	fixup_var_refs_insns,@function
fixup_var_refs_insns:                   # @fixup_var_refs_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp828:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp829:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp830:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp831:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp832:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp833:
	.cfi_def_cfa_offset 80
.Ltmp834:
	.cfi_offset %rbx, -48
.Ltmp835:
	.cfi_offset %r12, -40
.Ltmp836:
	.cfi_offset %r13, -32
.Ltmp837:
	.cfi_offset %r14, -24
.Ltmp838:
	.cfi_offset %r15, -16
	movq	%r9, %r12
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, %r13
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	jmp	.LBB88_1
	.align	16, 0x90
.LBB88_11:                              # %if.then29
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	%r15, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	8(%rsp), %r8            # 8-byte Reload
	movq	%r12, %r9
	callq	fixup_var_refs_insn
.LBB88_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB88_12
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	%r14, %r15
	movzbl	(%r15), %ecx
	movq	24(%r15), %r14
	cmpq	$34, %rcx
	jne	.LBB88_10
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	32(%r15), %rax
	movzbl	(%rax), %edx
	cmpq	$141, %rdx
	jne	.LBB88_10
# BB#4:                                 # %for.cond.preheader
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	%r12, %r13
	movq	8(%rax), %r12
	testq	%r12, %r12
	je	.LBB88_6
# BB#5:                                 # %if.then15
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	%r12, %rdi
	callq	push_to_sequence
	xorq	%r8, %r8
	movq	%r12, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	%r13, %r9
	callq	fixup_var_refs_insns
	callq	get_insns
	movq	32(%r15), %rcx
	movq	%rax, 8(%rcx)
	callq	end_sequence
	movq	32(%r15), %rax
.LBB88_6:                               # %for.inc
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	movq	%r13, %r12
	movq	(%rsp), %r13            # 8-byte Reload
	je	.LBB88_8
# BB#7:                                 # %if.then15.1
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	%rbx, %rdi
	callq	push_to_sequence
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r12, %r9
	callq	fixup_var_refs_insns
	callq	get_insns
	movq	32(%r15), %rcx
	movq	%rax, 16(%rcx)
	callq	end_sequence
	movq	32(%r15), %rax
.LBB88_8:                               # %for.inc.1
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB88_1
# BB#9:                                 # %if.then15.2
                                        #   in Loop: Header=BB88_1 Depth=1
	movq	%rbx, %rdi
	callq	push_to_sequence
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r12, %r9
	callq	fixup_var_refs_insns
	callq	get_insns
	movq	32(%r15), %rcx
	movq	%rax, 24(%rcx)
	callq	end_sequence
	jmp	.LBB88_1
	.align	16, 0x90
.LBB88_10:                              # %if.else
                                        #   in Loop: Header=BB88_1 Depth=1
	movzbl	rtx_class(%rcx), %eax
	cmpq	$105, %rax
	jne	.LBB88_1
	jmp	.LBB88_11
.LBB88_12:                              # %while.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp839:
	.size	fixup_var_refs_insns, .Ltmp839-fixup_var_refs_insns
	.cfi_endproc

	.align	16, 0x90
	.type	fixup_var_refs_insn,@function
fixup_var_refs_insn:                    # @fixup_var_refs_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp846:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp847:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp848:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp849:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp850:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp851:
	.cfi_def_cfa_offset 80
.Ltmp852:
	.cfi_offset %rbx, -48
.Ltmp853:
	.cfi_offset %r12, -40
.Ltmp854:
	.cfi_offset %r13, -32
.Ltmp855:
	.cfi_offset %r14, -24
.Ltmp856:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	32(%r14), %r13
	movzbl	(%r13), %eax
	movq	56(%r14), %r15
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rsi, %r12
	cmpq	$49, %rax
	jne	.LBB89_13
# BB#1:                                 # %land.lhs.true
	movq	8(%r13), %rcx
	cmpq	%r12, %rcx
	je	.LBB89_5
# BB#2:                                 # %lor.lhs.false
	movzbl	(%rcx), %edx
	cmpq	$65, %rdx
	jne	.LBB89_13
# BB#3:                                 # %land.lhs.true20
	movq	8(%rcx), %rdx
	cmpq	%r12, %rdx
	je	.LBB89_5
# BB#4:                                 # %lor.lhs.false31
	movq	16(%rcx), %rcx
	cmpq	%r12, %rcx
	je	.LBB89_5
.LBB89_13:                              # %if.else
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r8
	je	.LBB89_29
# BB#14:                                # %land.lhs.true52
	movzbl	(%r14), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB89_29
# BB#15:                                # %cond.true
	cmpq	$47, %rax
	je	.LBB89_17
# BB#16:                                # %cond.false
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%r9, %r13
	callq	single_set_2
	movq	%r13, %r9
	movq	%rax, %r13
.LBB89_17:                              # %cond.end74
	testq	%r13, %r13
	je	.LBB89_29
# BB#18:                                # %land.lhs.true78
	movq	8(%r13), %rax
	cmpq	%r12, %rax
	jne	.LBB89_29
# BB#19:                                # %land.lhs.true84
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r9, 8(%rsp)            # 8-byte Spill
	callq	find_reg_note
	movq	8(%rsp), %r9            # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB89_29
# BB#20:                                # %land.lhs.true88
	movq	16(%r13), %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	movq	8(%rsp), %r9            # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB89_7
# BB#21:                                # %lor.lhs.false94
	movq	16(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB89_29
# BB#22:                                # %land.lhs.true102
	movq	%r14, %rdi
	callq	prev_nonnote_insn
	movq	8(%rsp), %r9            # 8-byte Reload
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB89_29
# BB#23:                                # %land.lhs.true106
	movzbl	(%rcx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB89_29
# BB#24:                                # %cond.true114
	movq	32(%rcx), %rax
	movzbl	(%rax), %edx
	cmpq	$47, %rdx
	je	.LBB89_26
# BB#25:                                # %cond.false126
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB89_26:                              # %cond.end134
	testq	%rax, %rax
	movq	8(%rsp), %r9            # 8-byte Reload
	je	.LBB89_29
# BB#27:                                # %land.lhs.true138
	movq	8(%rax), %rcx
	movq	16(%r13), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB89_29
# BB#28:                                # %land.lhs.true147
	movq	16(%rax), %rdi
	movq	%r12, %rsi
	movq	%r9, %r13
	callq	rtx_equal_p
	movq	%r13, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB89_7
.LBB89_29:                              # %if.else155
	movq	$0, 24(%rsp)
	movq	24(%r14), %r13
	leaq	32(%r14), %rdx
	leaq	24(%rsp), %r8
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rcx
	callq	fixup_var_refs_1
	movq	cfun(%rip), %rax
	movq	296(%rax), %rcx
	cmpq	%r14, %rcx
	jne	.LBB89_31
# BB#30:                                # %if.then255
	movq	16(%r13), %rcx
	movq	%rcx, 296(%rax)
.LBB89_31:                              # %while.condthread-pre-split
	movq	24(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB89_8
# BB#32:
	movq	16(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB89_33:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB89_41
# BB#34:                                # %if.then266
                                        #   in Loop: Header=BB89_33 Depth=1
	movq	(%rdi), %rdi
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	jne	.LBB89_36
# BB#35:                                # %if.then271
                                        #   in Loop: Header=BB89_33 Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	fixup_memory_subreg
	jmp	.LBB89_37
	.align	16, 0x90
.LBB89_36:                              # %if.else275
                                        #   in Loop: Header=BB89_33 Depth=1
	movq	%r14, %rsi
	callq	fixup_stack_1
.LBB89_37:                              # %if.end279
                                        #   in Loop: Header=BB89_33 Depth=1
	movq	24(%rsp), %rcx
	movq	%rax, (%rcx)
	movq	24(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdi
	movl	(%rdi), %eax
	movl	(%rsi), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB89_38
# BB#39:                                # %if.else294
                                        #   in Loop: Header=BB89_33 Depth=1
	callq	gen_move_insn
	movq	%rax, %r12
	jmp	.LBB89_40
	.align	16, 0x90
.LBB89_38:                              # %if.then290
                                        #   in Loop: Header=BB89_33 Depth=1
	callq	start_sequence
	movq	24(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdi
	movq	%r13, %rdx
	callq	convert_move
	callq	gen_sequence
	movq	%rax, %r12
	callq	end_sequence
.LBB89_40:                              # %if.end298
                                        #   in Loop: Header=BB89_33 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	24(%rsp), %rdi
.LBB89_41:                              # %if.end300
                                        #   in Loop: Header=BB89_33 Depth=1
	movq	16(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, 24(%rsp)
	movq	%rbx, %rdi
	jne	.LBB89_33
	jmp	.LBB89_8
.LBB89_5:                               # %if.then
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB89_7
# BB#6:                                 # %if.then43
	movq	8(%r15), %rbx
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB89_7:                               # %if.end
	movq	%r14, %rdi
	callq	delete_insn
	jmp	.LBB89_8
	.align	16, 0x90
.LBB89_11:                              # %if.end319
                                        #   in Loop: Header=BB89_8 Depth=1
	movq	16(%r15), %r15
.LBB89_8:                               # %while.cond304.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB89_12
# BB#9:                                 # %while.body306
                                        #   in Loop: Header=BB89_8 Depth=1
	movzbl	(%r15), %eax
	cmpq	$4, %rax
	je	.LBB89_11
# BB#10:                                # %if.then311
                                        #   in Loop: Header=BB89_8 Depth=1
	movq	8(%r15), %rdi
	movq	%r14, %rsi
	callq	walk_fixup_memory_subreg
	movq	%rax, 8(%r15)
	jmp	.LBB89_11
.LBB89_12:                              # %while.end323
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp857:
	.size	fixup_var_refs_insn, .Ltmp857-fixup_var_refs_insn
	.cfi_endproc

	.align	16, 0x90
	.type	fixup_var_refs_1,@function
fixup_var_refs_1:                       # @fixup_var_refs_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp864:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp865:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp866:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp867:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp868:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp869:
	.cfi_def_cfa_offset 128
.Ltmp870:
	.cfi_offset %rbx, -48
.Ltmp871:
	.cfi_offset %r12, -40
.Ltmp872:
	.cfi_offset %r13, -32
.Ltmp873:
	.cfi_offset %r14, -24
.Ltmp874:
	.cfi_offset %r15, -16
	movq	(%rdx), %r10
	movl	(%r10), %eax
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	%rdi, %r8
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	jmp	.LBB90_1
	.align	16, 0x90
.LBB90_73:                              # %if.then283
                                        #   in Loop: Header=BB90_1 Depth=1
	movq	%r13, %rdx
	movq	%r8, (%rdx)
	movl	(%r8), %eax
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r8, %r10
.LBB90_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r12
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$131, %rsi
	jg	.LBB90_4
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB90_1 Depth=1
	leaq	-47(%rsi), %rcx
	andq	%r11, %rcx
	cmpq	$23, %rcx
	ja	.LBB90_209
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB90_1 Depth=1
	movq	%rdx, %r13
	jmpq	*.LJTI90_0(,%rcx,8)
.LBB90_71:                              # %sw.bb272
                                        #   in Loop: Header=BB90_1 Depth=1
	movq	8(%r10), %rcx
	cmpq	%r8, %rcx
	movq	$63, %rsi
	jne	.LBB90_209
# BB#72:                                # %if.then278
                                        #   in Loop: Header=BB90_1 Depth=1
	testq	$268435456, %rax        # imm = 0x10000000
	jne	.LBB90_73
# BB#74:                                # %if.end287
	movq	$16, %rcx
	movzbl	2(%r8), %edx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rdx), %ecx
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	(%r14), %r14
	cmpq	%rcx, %rax
	jbe	.LBB90_82
# BB#75:                                # %if.then302
	movq	%r10, %r15
	testq	%r14, %r14
	je	.LBB90_76
.LBB90_78:                              # %land.rhs.i1662
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rdi
	movq	%r8, %rsi
	movq	%r8, %rbx
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB90_79
# BB#77:                                # %for.cond.i1657
                                        #   in Loop: Header=BB90_78 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	movq	%rbx, %r8
	jne	.LBB90_78
.LBB90_76:                              # %if.then.i1667
	movq	$24, %rdi
	movq	%r8, %rbx
	callq	xmalloc
	movq	%rax, %r14
	movq	%rbx, (%r14)
	movq	$0, 8(%r14)
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	(%r13), %rax
	movq	%rax, 16(%r14)
	movq	%r14, (%r13)
.LBB90_79:                              # %find_fixup_replacement.exit1669
	movq	8(%r14), %rax
	testq	%rax, %rax
	movq	%r15, %rbx
	jne	.LBB90_81
# BB#80:                                # %if.then307
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, 8(%r14)
.LBB90_81:                              # %if.end310
	movq	%rax, 8(%rbx)
	jmp	.LBB90_218
.LBB90_4:                               # %tailrecurse
	leaq	-132(%rsi), %rax
	andq	%r11, %rax
	cmpq	$2, %rax
	jae	.LBB90_209
# BB#5:                                 # %sw.bb87
	movq	8(%r10), %rdi
	cmpq	%r8, %rdi
	je	.LBB90_6
# BB#41:                                # %lor.lhs.false
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB90_209
# BB#42:                                # %land.lhs.true98
	movq	8(%rdi), %rcx
	cmpq	%r8, %rcx
	jne	.LBB90_209
	jmp	.LBB90_43
.LBB90_6:                               # %sw.bb87.if.then106_crit_edge
	movl	(%r8), %eax
.LBB90_43:                              # %if.then106
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB90_54
# BB#44:                                # %if.then113
	movq	$15, %rcx
	movzbl	2(%r8), %edx
	shrq	%cl, %rax
	andq	$510, %rax              # imm = 0x1FE
	movzbl	mode_bitsize(%rax), %eax
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB90_53
# BB#45:                                # %if.then126
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%r11, %r13
	movq	%r10, %r15
	movq	%r14, %rax
	movq	(%rax), %r14
	movq	%rax, %r12
	testq	%r14, %r14
	je	.LBB90_46
	.align	16, 0x90
.LBB90_48:                              # %land.rhs.i1644
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rdi
	movq	%r8, %rsi
	movq	%r8, %rbx
	callq	rtx_equal_p
	testq	%r13, %rax
	jne	.LBB90_49
# BB#47:                                # %for.cond.i1639
                                        #   in Loop: Header=BB90_48 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	movq	%rbx, %r8
	jne	.LBB90_48
.LBB90_46:                              # %if.then.i1649
	movq	$24, %rdi
	movq	%r8, %rbx
	callq	xmalloc
	movq	%rbx, %rcx
	movq	%rax, %r14
	movq	%rcx, (%r14)
	movq	$0, 8(%r14)
	movq	%r12, %rbx
	movq	(%rbx), %rax
	movq	%rax, 16(%r14)
	movq	%r14, (%rbx)
	jmp	.LBB90_50
.LBB90_54:                              # %if.else147
	movq	%r11, %r14
	movq	%r10, %r13
	movq	%rbx, %rsi
	movq	%rbx, %r12
	callq	fixup_stack_1
	jmp	.LBB90_55
.LBB90_53:                              # %if.else
	movq	%r11, %r14
	movq	%r10, %r13
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	movq	%rbx, %r12
	callq	fixup_memory_subreg
.LBB90_55:                              # %if.end149
	movq	%rax, %r15
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	jne	.LBB90_70
# BB#56:                                # %land.lhs.true151
	movq	16(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB90_70
# BB#57:                                # %land.lhs.true159
	movq	24(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB90_70
# BB#58:                                # %land.lhs.true167
	movq	8(%r15), %rdi
	callq	mode_dependent_address_p
	testq	%r14, %rax
	jne	.LBB90_70
# BB#59:                                # %land.lhs.true173
	movl	(%r15), %ebx
	testq	$134217728, %rbx        # imm = 0x8000000
	jne	.LBB90_70
# BB#60:                                # %if.then178
	movq	24(%r13), %rdx
	movq	$16, %rcx
	movzbl	(%r13), %eax
	movq	8(%rdx), %rdx
	shrq	%cl, %rbx
	andq	$255, %rbx
	cmpq	$132, %rax
	jne	.LBB90_61
# BB#63:                                # %if.then202
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	$1, %rdi
	jmp	.LBB90_64
.LBB90_49:
	movq	%rbx, %rcx
.LBB90_50:                              # %find_fixup_replacement.exit1651
	movq	8(%r14), %rax
	testq	%rax, %rax
	movq	%r15, %rbx
	jne	.LBB90_52
# BB#51:                                # %if.then131
	movzbl	2(%rcx), %edi
	callq	gen_reg_rtx
	movq	%rax, 8(%r14)
.LBB90_52:                              # %if.end137
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx)
	movq	%rcx, 8(%rbx)
	jmp	.LBB90_218
.LBB90_82:                              # %if.end315
	testq	%r14, %r14
	je	.LBB90_83
.LBB90_85:                              # %land.rhs.i1680
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rdi
	movq	%r10, %rsi
	movq	%r10, %r15
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB90_86
# BB#84:                                # %for.cond.i1675
                                        #   in Loop: Header=BB90_85 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	movq	%r15, %r10
	jne	.LBB90_85
.LBB90_83:                              # %if.then.i1685
	movq	$24, %rdi
	movq	%r10, %r14
	callq	xmalloc
	movq	%r14, %rdi
	movq	%rax, %r14
	movq	%rdi, (%r14)
	movq	$0, 8(%r14)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	%rax, 16(%r14)
	movq	%r14, (%rcx)
	jmp	.LBB90_87
.LBB90_92:                              # %sw.bb352
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	8(%r10), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$132, %rax
	jne	.LBB90_94
# BB#93:                                # %if.then368
	movq	%r10, %rdi
	movq	%rbx, %rsi
	movq	%r10, %r14
	movq	%r8, %r15
	movq	%r11, %r13
	callq	optimize_bit_field
	movq	%r13, %r11
	movq	%r15, %r8
	movq	%r14, %r10
.LBB90_94:                              # %if.end369
	movq	%r11, %r13
	movq	16(%r10), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$132, %rax
	jne	.LBB90_96
# BB#95:                                # %if.then385
	movq	%r10, %rdi
	movq	%rbx, %rsi
	movq	%r10, %r14
	movq	%r8, %r15
	callq	optimize_bit_field
	movq	%r15, %r8
	movq	%r14, %r10
.LBB90_96:                              # %if.end386
	movq	8(%r10), %r9
	movl	(%r9), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	movq	%r13, %r15
	jne	.LBB90_108
# BB#97:                                # %land.lhs.true394
	movq	8(%r9), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$63, %rsi
	jne	.LBB90_108
# BB#98:                                # %land.lhs.true405
	movq	8(%rcx), %rcx
	cmpq	%r8, %rcx
	jne	.LBB90_108
# BB#99:                                # %land.lhs.true417
	movq	$16, %rcx
	movzbl	2(%r8), %esi
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	movzbl	mode_size(%rsi), %edx
	cmpq	%rdx, %rcx
	jbe	.LBB90_108
# BB#100:                               # %if.then438
	movq	%r10, 64(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB90_101
.LBB90_103:                             # %land.rhs.i1698
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdi
	movq	%r8, %rsi
	movq	%r8, %r13
	movq	%rax, %r14
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB90_104
# BB#102:                               # %for.cond.i1693
                                        #   in Loop: Header=BB90_103 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	movq	%r13, %r8
	movq	%r14, %rax
	jne	.LBB90_103
.LBB90_101:                             # %if.then.i1703
	movq	$24, %rdi
	movq	%r8, %r13
	callq	xmalloc
	movq	%rax, %rdx
	movq	%r13, (%rdx)
	movq	$0, 8(%rdx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	%rax, 16(%rdx)
	movq	%rdx, (%rcx)
	jmp	.LBB90_105
.LBB90_18:                              # %sw.bb35
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	cmpq	%r8, %r10
	je	.LBB90_19
# BB#31:                                # %if.end71
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%r8, %rdi
	movq	%r10, %rsi
	movq	%r10, %r14
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$66, %rsi
	je	.LBB90_32
# BB#33:                                # %if.then74
	movq	%rcx, %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB90_34
.LBB90_36:                              # %land.rhs.i1626
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdi
	movq	%r14, %rsi
	movq	%rax, %rbx
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB90_37
# BB#35:                                # %for.cond.i1621
                                        #   in Loop: Header=BB90_36 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rbx, %rax
	jne	.LBB90_36
.LBB90_34:                              # %if.then.i1631
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rcx
	movq	%r14, %rdi
	movq	%rdi, (%rcx)
	movq	$0, 8(%rcx)
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	(%r14), %rax
	movq	%rax, 16(%rcx)
	movq	%rcx, (%r14)
	jmp	.LBB90_38
.LBB90_7:                               # %sw.bb
	movq	8(%r10), %rax
	cmpq	%r8, %rax
	jne	.LBB90_218
# BB#8:                                 # %if.then
	movq	8(%r8), %rdi
	movq	%r11, %r14
	callq	copy_rtx
	movq	%rax, %r15
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	validate_change
	testq	%r14, %rax
	jne	.LBB90_218
# BB#9:                                 # %if.then5
	movzbl	2(%r15), %edi
	movq	%r14, %r12
	callq	gen_reg_rtx
	movq	(%r13), %rdi
	movq	%rax, %r14
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	validate_replace_rtx
	testq	%r12, %rax
	je	.LBB90_10
# BB#12:                                # %if.end
	callq	start_sequence
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r12
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB90_17
# BB#13:                                # %cond.end
	movq	%r12, %rdi
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_17
# BB#14:                                # %if.then23
	callq	start_sequence
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	force_operand
	cmpq	%r14, %rax
	je	.LBB90_16
# BB#15:                                # %if.then26
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	callq	emit_insn
.LBB90_16:                              # %if.end29
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
.LBB90_17:                              # %if.end31
	movq	%rbx, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn_before  # TAILCALL
.LBB90_86:
	movq	%r15, %rdi
.LBB90_87:                              # %find_fixup_replacement.exit1687
	movq	8(%r14), %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	je	.LBB90_89
# BB#88:                                # %if.then319
	movq	%rax, (%r13)
	jmp	.LBB90_218
.LBB90_89:                              # %if.end321
	xorq	%rdx, %rdx
	movq	%rsi, %rbx
	movq	%rdi, %r15
	callq	fixup_memory_subreg
	movq	%r15, %rdx
	movq	%rbx, %rdi
	movq	%rax, (%r13)
	movq	%r13, %rbx
	movq	%rax, 8(%r14)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%rdi)
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	jne	.LBB90_91
# BB#90:                                # %cond.false338
	movq	%rdx, %r15
	callq	recog_memoized_1
	movq	%r15, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_218
.LBB90_91:                              # %if.end345
	movzbl	2(%rdx), %edi
	jmp	.LBB90_30
.LBB90_19:                              # %if.then37
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	(%r14), %r14
	testq	%r14, %r14
	je	.LBB90_20
.LBB90_22:                              # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rdi
	movq	%r8, %rsi
	movq	%r8, %r15
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB90_23
# BB#21:                                # %for.cond.i
                                        #   in Loop: Header=BB90_22 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	movq	%r15, %r8
	jne	.LBB90_22
.LBB90_20:                              # %if.then.i
	movq	$24, %rdi
	movq	%r8, %r14
	callq	xmalloc
	movq	%r14, %rdi
	movq	%rax, %r14
	movq	%rdi, (%r14)
	movq	$0, 8(%r14)
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	(%r15), %rax
	movq	%rax, 16(%r14)
	movq	%r14, (%r15)
	jmp	.LBB90_24
.LBB90_61:                              # %if.then178
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	cmpq	$133, %rax
	jne	.LBB90_70
# BB#62:                                # %if.then191
	movq	$2, %rdi
.LBB90_64:                              # %if.then202
	movq	$1, %rsi
	callq	mode_for_extraction
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	andq	%r14, %rax
	cmpq	$59, %rax
	je	.LBB90_70
# BB#65:                                # %if.end210
	testq	%r14, %rsi
	je	.LBB90_70
# BB#66:                                # %land.lhs.true213
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	mode_size(%rdi), %ecx
	movzbl	mode_size(%rbx), %edx
	cmpq	%rdx, %rcx
	jae	.LBB90_70
# BB#67:                                # %if.then222
	movq	$63, %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r9
	sarq	%cl, %r9
	movq	$61, %rcx
	shrq	%cl, %r9
	addq	%rax, %r9
	movzbl	mode_bitsize(%rdi,%rdi), %ecx
	movq	%r13, %rbx
	movq	24(%rbx), %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	cqto
	idivq	%rcx
	movq	%rdx, %r13
	movq	$3, %rcx
	sarq	%cl, %r9
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r9, %rdx
	callq	adjust_address_1
	movl	%r14d, 40(%r12)
	movq	%rax, 8(%rbx)
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rcx
	movq	%r12, %rdi
	movq	%rax, 24(%rcx)
	movslq	40(%rdi), %rax
	testq	%rax, %rax
	jns	.LBB90_218
# BB#68:                                # %cond.end251
	movq	%rdi, %r14
	movq	%rcx, %rbx
	callq	recog_memoized_1
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_218
# BB#69:                                # %if.end256
	movq	%rdx, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsi)
	movq	%rsi, %r13
.LBB90_70:                              # %if.end261
	movzbl	2(%r15), %edi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	emit_insn_before
	movq	%rbx, 8(%r13)
	jmp	.LBB90_218
.LBB90_32:
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %r10
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	jmp	.LBB90_209
.LBB90_37:
	movq	%r14, %rdi
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%rbx, %rcx
.LBB90_38:                              # %find_fixup_replacement.exit1633
	movq	8(%rcx), %rax
	testq	%rax, %rax
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	jne	.LBB90_40
# BB#39:                                # %if.then78
	movq	%r9, %rsi
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	movq	%r9, %r13
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	callq	copy_most_rtx
	movq	%r13, %r9
	movq	%r15, %r8
	movq	64(%rsp), %r13          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx)
.LBB90_40:                              # %if.end81
	movq	%rax, (%r13)
	movzbl	(%rax), %esi
	movq	%rax, %r10
	jmp	.LBB90_209
.LBB90_23:
	movq	%r15, %rdi
.LBB90_24:                              # %find_fixup_replacement.exit
	movq	8(%r14), %rax
	testq	%rax, %rax
	movq	56(%rsp), %rsi          # 8-byte Reload
	je	.LBB90_26
# BB#25:                                # %if.then40
	movq	%rax, (%r13)
	jmp	.LBB90_218
.LBB90_26:                              # %if.end42
	movq	%rsi, %rbx
	callq	fixup_stack_1
	movq	%rbx, %rdi
	movq	%rax, 8(%r14)
	movq	%rax, (%r13)
	movq	%r13, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movl	%edx, 40(%rdi)
	movl	flag_force_mem(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB90_29
# BB#27:                                # %land.lhs.true
	movzbl	2(%rax), %eax
	andq	%r12, %rdx
	cmpq	%rdx, %rax
	jne	.LBB90_29
# BB#28:                                # %cond.false62
	callq	recog_memoized_1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_218
.LBB90_29:                              # %if.end68
	movq	%r12, %rdi
.LBB90_30:                              # %for.end1225
	callq	gen_reg_rtx
	movq	%rax, 8(%r14)
	movq	%rax, (%rbx)
	jmp	.LBB90_218
.LBB90_104:
	movq	%r14, %rdx
.LBB90_105:                             # %find_fixup_replacement.exit1705
	movq	8(%rdx), %rax
	testq	%rax, %rax
	jne	.LBB90_107
# BB#106:                               # %if.then443
	movzbl	2(%r13), %eax
	movq	%rax, %rdi
	movq	%rdx, %r14
	callq	gen_reg_rtx
	movq	%r14, %rdx
	movq	%rax, 8(%rdx)
.LBB90_107:                             # %if.end449
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	8(%r14), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 8(%rcx)
	movq	8(%rdx), %rsi
	movq	%r13, %rdi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_after
	movq	%r13, %r8
	movq	%r14, %r10
	movq	8(%r10), %r9
	movl	(%r9), %eax
.LBB90_108:                             # %if.end463
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB90_112
# BB#109:                               # %land.lhs.true471
	movq	8(%r9), %rcx
	cmpq	%r8, %rcx
	jne	.LBB90_112
# BB#110:                               # %land.lhs.true480
	movq	$16, %rcx
	movq	%rax, %rdi
	movzbl	2(%r8), %edx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	movzbl	mode_size(%rdx), %edx
	cmpq	%rdx, %rcx
	jbe	.LBB90_112
# BB#111:                               # %if.then498
	movq	%r10, %r14
	movq	%r8, %r15
	callq	gen_reg_rtx
	movq	%rax, 8(%r14)
	movzbl	2(%r15), %edi
	movq	%rax, %rsi
	callq	gen_lowpart
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_after
	movq	%r15, %r8
	movq	%r14, %r10
	movq	$47, %rsi
	jmp	.LBB90_120
.LBB90_112:                             # %if.end515
	leaq	16(%r10), %rsi
	movq	(%rsi), %rcx
	movq	%r9, %rdi
	jmp	.LBB90_113
.LBB90_115:                             # %while.body
                                        #   in Loop: Header=BB90_113 Depth=1
	movq	8(%rdi), %rdi
	movl	(%rdi), %eax
.LBB90_113:                             # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rdx
	andq	%r15, %rdx
	cmpq	$2, %rdx
	jb	.LBB90_115
# BB#114:                               # %while.cond
                                        #   in Loop: Header=BB90_113 Depth=1
	addq	$-132, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	jbe	.LBB90_115
# BB#116:                               # %while.end
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	jne	.LBB90_118
# BB#117:                               # %if.then547
	movq	8(%rcx), %rcx
.LBB90_118:                             # %if.end551
	movq	%r9, %rdx
	cmpq	%r8, %rcx
	movq	$47, %rsi
	je	.LBB90_121
# BB#119:                               # %if.end551
	cmpq	%r8, %rdi
	jne	.LBB90_120
.LBB90_121:                             # %if.end558
	movl	%r15d, 40(%rbx)
	movzbl	(%rdx), %eax
	cmpq	$133, %rax
	jne	.LBB90_137
# BB#122:                               # %if.end558
	cmpq	%r8, %rdi
	jne	.LBB90_137
# BB#123:                               # %land.lhs.true569
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	movq	%r10, %r14
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%rdx, %r13
	callq	mode_for_extraction
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	$47, %rsi
	movq	%r13, %r9
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r10
	andq	%r15, %rax
	cmpq	$59, %rax
	jne	.LBB90_124
.LBB90_137:                             # %if.end705
	cmpq	%r8, %rdi
	jne	.LBB90_140
# BB#138:                               # %land.lhs.true708
	movq	8(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$64, %rcx
	jne	.LBB90_140
# BB#139:                               # %if.then716
	movq	8(%rax), %rax
	movq	%rax, 8(%r10)
.LBB90_140:                             # %if.end726
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rax
	cmpq	%r8, %rax
	je	.LBB90_143
# BB#141:                               # %lor.lhs.false732
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB90_181
# BB#142:                               # %land.lhs.true740
	movq	8(%rax), %rax
	cmpq	%r8, %rax
	jne	.LBB90_181
.LBB90_143:                             # %land.lhs.true749
	movq	8(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	je	.LBB90_146
# BB#144:                               # %land.lhs.true749
	cmpq	$63, %rcx
	jne	.LBB90_181
# BB#145:                               # %land.lhs.true765
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB90_181
.LBB90_146:                             # %land.lhs.true776
	movzbl	2(%r8), %eax
	movq	%r12, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB90_181
# BB#147:                               # %land.lhs.true782
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB90_151
# BB#148:                               # %cond.true790
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	%r8, %r13
	movq	%r10, %r14
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB90_150
# BB#149:                               # %cond.false802
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB90_150:                             # %cond.end810
	movq	%r14, %r10
	movq	%r13, %r8
	movq	$47, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rdi          # 8-byte Reload
.LBB90_151:                             # %cond.end810
	cmpq	%rax, %r10
	je	.LBB90_152
.LBB90_181:                             # %if.end965
	movq	8(%r10), %rax
	cmpq	%r8, %rax
	je	.LBB90_184
# BB#182:                               # %lor.lhs.false971
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB90_202
# BB#183:                               # %land.lhs.true979
	movq	8(%rax), %rax
	cmpq	%r8, %rax
	jne	.LBB90_202
.LBB90_184:                             # %land.lhs.true988
	movq	(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	je	.LBB90_187
# BB#185:                               # %land.lhs.true988
	cmpq	$63, %rcx
	jne	.LBB90_202
# BB#186:                               # %land.lhs.true1004
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB90_202
.LBB90_187:                             # %land.lhs.true1015
	movzbl	2(%r8), %eax
	andq	%r12, %r15
	cmpq	%r15, %rax
	jne	.LBB90_202
# BB#188:                               # %land.lhs.true1021
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB90_192
# BB#189:                               # %cond.true1029
	movq	%rdi, %r13
	movq	%r8, %r15
	movq	%r10, %r14
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB90_191
# BB#190:                               # %cond.false1041
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB90_191:                             # %cond.end1049
	movq	%r14, %r10
	movq	%r15, %r8
	movq	$47, %rsi
	movq	%r13, %rdi
.LBB90_192:                             # %cond.end1049
	cmpq	%rax, %r10
	je	.LBB90_193
.LBB90_202:                             # %if.end1145
	cmpq	%r8, %rdi
	je	.LBB90_203
.LBB90_120:
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
.LBB90_209:                             # %sw.epilog
	movzbl	rtx_length(%rsi), %edx
	testq	%rdx, %rdx
	je	.LBB90_218
# BB#210:                               # %for.body.lr.ph
	movq	rtx_format(,%rsi,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB90_211:                             # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB90_216 Depth 2
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%rdx), %eax
	leaq	-1(%rdx), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB90_212
# BB#214:                               # %for.cond1209.preheader
                                        #   in Loop: Header=BB90_211 Depth=1
	movq	(%r10,%rdx,8), %rdx
	movslq	(%rdx), %rax
	testq	%rax, %rax
	movq	%r12, %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	jle	.LBB90_213
# BB#215:                               #   in Loop: Header=BB90_211 Depth=1
	movq	$8, %r15
	movq	$1, %r13
	.align	16, 0x90
.LBB90_216:                             # %for.body1215
                                        #   Parent Loop BB90_211 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r15, %rdx
	movq	%r8, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r14, %r8
	movq	%r9, %rbx
                                        # kill: R9<def> RBX<kill>
	movq	%r14, %r12
	movq	%r10, %r14
	callq	fixup_var_refs_1
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rbx, %r9
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r10
	movq	%r12, %r14
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	(%r10,%rax,8), %rdx
	movslq	(%rdx), %rax
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	addq	$8, %r15
	cmpq	%rax, %rsi
	movq	72(%rsp), %rcx          # 8-byte Reload
	jl	.LBB90_216
	jmp	.LBB90_213
	.align	16, 0x90
.LBB90_212:                             # %for.body
                                        #   in Loop: Header=BB90_211 Depth=1
	cmpq	$101, %rax
	movq	%r12, %rcx
	jne	.LBB90_213
# BB#219:                               # %if.then1197
                                        #   in Loop: Header=BB90_211 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	8(%r10,%rax,8), %rdx
	movq	%r8, %rdi
	movq	%rcx, %r12
	movq	%r12, %rsi
	movq	%rbx, %rcx
	movq	%r8, %rbx
	movq	%r14, %r8
	movq	%r9, %r13
                                        # kill: R9<def> R13<kill>
	movq	%r10, %r15
	callq	fixup_var_refs_1
	movq	%r12, %rcx
	movq	%r13, %r9
	movq	%rbx, %r8
	movq	%r15, %r10
	movq	56(%rsp), %rbx          # 8-byte Reload
.LBB90_213:                             # %for.cond.backedge
                                        #   in Loop: Header=BB90_211 Depth=1
	movq	%rcx, %r12
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB90_211
	jmp	.LBB90_218
.LBB90_124:                             # %if.then573
	leaq	16(%r9), %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%r8, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rcx
	movq	%rbx, %r13
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r8, %rbx
	movq	%r14, %r8
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movq	%r15, %r9
	callq	fixup_var_refs_1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	24(%rax), %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rcx
	movq	%r14, %r8
	movq	%r15, %r9
	callq	fixup_var_refs_1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r13, %r12
	movq	%r14, %r8
	movq	%r15, %r9
	callq	fixup_var_refs_1
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	8(%r13), %rdi
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	jne	.LBB90_127
# BB#125:                               # %land.lhs.true590
	movq	8(%rdi), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB90_126
.LBB90_127:                             # %if.else598
	movq	%r12, %rsi
	callq	fixup_stack_1
	jmp	.LBB90_128
.LBB90_203:                             # %if.then1148
	movq	8(%r10), %rdi
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$64, %rcx
	jne	.LBB90_205
# BB#204:                               # %if.then1156
	movq	8(%rdi), %rdi
	movl	(%rdi), %eax
.LBB90_205:                             # %if.end1160
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%r10, %r14
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB90_207
# BB#206:                               # %if.then1165
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	movq	%rbx, %r13
	callq	fixup_memory_subreg
	movzbl	2(%rax), %edi
	movq	%rax, %r15
	jmp	.LBB90_208
.LBB90_207:                             # %if.else1170
	movq	%rbx, %rsi
	movq	%rbx, %r13
	callq	fixup_stack_1
	movq	%rax, %r15
	movq	%r12, %rdi
.LBB90_208:                             # %if.end1172
	callq	gen_reg_rtx
	movq	%r15, %rbx
	movzbl	2(%rbx), %edi
	movq	%rax, %r15
	movq	%r15, %rsi
	callq	gen_lowpart
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_after
	movq	%r14, %r10
	movq	%r15, 8(%r10)
	movq	%r13, %rbx
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	16(%rsp), %r9           # 8-byte Reload
	movq	$47, %rsi
	jmp	.LBB90_209
.LBB90_126:                             # %if.then596
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	fixup_memory_subreg
.LBB90_128:                             # %if.end600
	movq	%rax, %r14
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB90_136
# BB#129:                               # %land.lhs.true608
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB90_136
# BB#130:                               # %land.lhs.true616
	movq	8(%r14), %rdi
	callq	mode_dependent_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB90_136
# BB#131:                               # %land.lhs.true622
	movl	(%r14), %ebx
	testq	$134217728, %rbx        # imm = 0x8000000
	jne	.LBB90_136
# BB#132:                               # %if.then627
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
	movq	8(%rax), %r15
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	mode_for_extraction
	movq	%rax, %rsi
	movq	$32, %rcx
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	movzbl	mode_size(%rbx), %edx
	cmpq	%rdx, %rcx
	jae	.LBB90_136
# BB#133:                               # %if.then649
	movq	$63, %rcx
	movq	%r15, %r9
	sarq	%cl, %r9
	movq	$61, %rcx
	shrq	%cl, %r9
	addq	%r15, %r9
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movq	24(%r13), %rbx
	movq	%r15, %rax
	cqto
	idivq	%rcx
	movq	%rdx, %r15
	movq	$3, %rcx
	sarq	%cl, %r9
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%r9, %rdx
	callq	adjust_address_1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 40(%r12)
	movq	%rax, 8(%r13)
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r12, %rcx
	movq	%rax, 24(%r13)
	movslq	40(%rcx), %rax
	testq	%rax, %rax
	jns	.LBB90_218
# BB#134:                               # %cond.end683
	movq	%rcx, %rdi
	movq	%rcx, %r15
	callq	recog_memoized_1
	movq	%r15, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_218
# BB#135:                               # %if.end688
	movq	%rdx, %r12
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rbx, (%rax)
.LBB90_136:                             # %if.end693
	movzbl	2(%r14), %edi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r12, %r15
	movq	%r15, %rsi
	callq	emit_insn_before
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_move_insn
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_insn_after
	movq	%rbx, 8(%r13)
	jmp	.LBB90_218
.LBB90_10:                              # %if.then11
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fixup_var_refs_1, %rcx
	movq	$1964, %rsi             # imm = 0x7AC
	jmp	.LBB90_11
.LBB90_152:                             # %if.then814
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%r10, %r12
	movq	%rbx, %r15
	movq	(%rdx), %r14
	movq	%rdx, %r13
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB90_162
# BB#153:                               # %land.lhs.true822
	movq	$16, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	movzbl	2(%rdx), %edx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB90_162
# BB#154:                               # %if.then840
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rbx
	jmp	.LBB90_155
.LBB90_193:                             # %if.then1053
	movq	8(%r10), %rax
	movq	%r10, %r14
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB90_195
# BB#194:                               # %if.then1063
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	fixup_memory_subreg
	jmp	.LBB90_196
.LBB90_162:                             # %if.else859
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rbx
	jmp	.LBB90_163
.LBB90_157:                             # %for.cond.i1711
                                        #   in Loop: Header=BB90_155 Depth=1
	movq	16(%rbx), %rbx
.LBB90_155:                             # %if.then840
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB90_156
# BB#158:                               # %land.rhs.i1716
                                        #   in Loop: Header=BB90_155 Depth=1
	movq	(%rbx), %rax
	movq	%rax, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB90_157
	jmp	.LBB90_159
.LBB90_195:                             # %if.else1071
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	fixup_stack_1
.LBB90_196:                             # %if.end1079
	movq	%rax, 8(%r14)
	movq	%rbx, %rcx
	movslq	40(%rcx), %rax
	testq	%rax, %rax
	movq	%r14, %rax
	jns	.LBB90_218
# BB#197:                               # %cond.end1091
	movq	%rcx, %rdi
	movq	%rcx, %rbx
	movq	%rax, %r14
	callq	recog_memoized_1
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_218
# BB#198:                               # %if.end1096
	movq	%rdx, %rbx
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	gen_move_insn
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB90_217
# BB#199:                               # %if.then1108
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_before
	movq	56(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB90_180
# BB#200:                               # %land.lhs.true1114
	cmpq	$0, 56(%rbx)
	je	.LBB90_179
# BB#201:                               # %if.then1119
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fixup_var_refs_1, %rcx
	movq	$2468, %rsi             # imm = 0x9A4
	jmp	.LBB90_11
.LBB90_165:                             # %for.cond.i1729
                                        #   in Loop: Header=BB90_163 Depth=1
	movq	16(%rbx), %rbx
.LBB90_163:                             # %if.else859
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB90_164
# BB#166:                               # %land.rhs.i1734
                                        #   in Loop: Header=BB90_163 Depth=1
	movq	(%rbx), %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB90_165
	jmp	.LBB90_167
.LBB90_156:                             # %if.then.i1721
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rbx)
	movq	$0, 8(%rbx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%rcx)
.LBB90_159:                             # %find_fixup_replacement.exit1723
	movq	8(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB90_161
# BB#160:                               # %if.then845
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, 8(%rbx)
.LBB90_161:                             # %if.end851
	movq	(%r13), %rcx
	movq	%rax, 8(%rcx)
	jmp	.LBB90_173
.LBB90_164:                             # %if.then.i1739
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movq	%r14, (%rbx)
	movq	$0, 8(%rbx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%rcx)
.LBB90_167:                             # %find_fixup_replacement.exit1741
	movq	8(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB90_172
# BB#168:                               # %if.else871
	movq	(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB90_170
# BB#169:                               # %if.then879
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	fixup_memory_subreg
	jmp	.LBB90_171
.LBB90_170:                             # %if.else888
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	fixup_stack_1
.LBB90_171:                             # %if.end899
	movq	%rax, 8(%rbx)
.LBB90_172:                             # %if.end899
	movq	%rax, (%r13)
.LBB90_173:                             # %if.end899
	movq	%r15, %rcx
	movslq	40(%rcx), %rax
	testq	%rax, %rax
	movq	%r12, %rax
	jns	.LBB90_218
# BB#174:                               # %cond.end911
	movq	%rcx, %rdi
	movq	%rcx, %rbx
	movq	%rax, %r14
	callq	recog_memoized_1
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB90_218
# BB#175:                               # %if.end916
	movq	%rdx, %rbx
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	gen_move_insn
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB90_217
# BB#176:                               # %if.then928
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_before
	movq	56(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB90_180
# BB#177:                               # %land.lhs.true934
	cmpq	$0, 56(%rbx)
	jne	.LBB90_178
.LBB90_179:                             # %if.then945
	movq	%rcx, 56(%rbx)
.LBB90_180:                             # %if.end952
	movq	32(%rax), %rcx
	movq	%rcx, 32(%rbx)
	movq	%rax, %rdi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_insn  # TAILCALL
.LBB90_217:                             # %if.else960
	movq	%rax, 32(%rbx)
.LBB90_218:                             # %for.end1225
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB90_178:                             # %if.then939
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fixup_var_refs_1, %rcx
	movq	$2424, %rsi             # imm = 0x978
.LBB90_11:                              # %if.then11
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp875:
	.size	fixup_var_refs_1, .Ltmp875-fixup_var_refs_1
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI90_0:
	.quad	.LBB90_92
	.quad	.LBB90_209
	.quad	.LBB90_209
	.quad	.LBB90_209
	.quad	.LBB90_209
	.quad	.LBB90_209
	.quad	.LBB90_209
	.quad	.LBB90_218
	.quad	.LBB90_218
	.quad	.LBB90_218
	.quad	.LBB90_209
	.quad	.LBB90_218
	.quad	.LBB90_218
	.quad	.LBB90_209
	.quad	.LBB90_218
	.quad	.LBB90_209
	.quad	.LBB90_71
	.quad	.LBB90_209
	.quad	.LBB90_209
	.quad	.LBB90_18
	.quad	.LBB90_218
	.quad	.LBB90_218
	.quad	.LBB90_218
	.quad	.LBB90_7

	.text
	.align	16, 0x90
	.type	fixup_memory_subreg,@function
fixup_memory_subreg:                    # @fixup_memory_subreg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp881:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp882:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp883:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp884:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp885:
	.cfi_def_cfa_offset 48
.Ltmp886:
	.cfi_offset %rbx, -40
.Ltmp887:
	.cfi_offset %r12, -32
.Ltmp888:
	.cfi_offset %r14, -24
.Ltmp889:
	.cfi_offset %r15, -16
	movq	8(%rdi), %rbx
	movzbl	2(%rdi), %r14d
	movzbl	2(%rbx), %eax
	movzbl	mode_size(%r14), %ecx
	movzbl	mode_size(%rax), %eax
	movq	8(%rbx), %r8
	movq	%rsi, %r15
	cmpq	%rax, %rcx
	jbe	.LBB91_2
# BB#1:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB91_5
.LBB91_2:                               # %if.end
	movl	flag_force_addr(%rip), %eax
	movslq	16(%rdi), %r12
	testq	%rax, %rax
	jne	.LBB91_4
# BB#3:                                 # %land.lhs.true15
	movq	%r8, %rdi
	movq	%r12, %rsi
	callq	plus_constant_wide
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB91_4
# BB#6:                                 # %if.then19
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	adjust_address_1  # TAILCALL
.LBB91_4:                               # %if.end22
	callq	start_sequence
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	adjust_address_1
	movq	%rax, %rbx
	callq	gen_sequence
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_insn_before
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB91_5:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fixup_memory_subreg, %rcx
	movq	$2571, %rsi             # imm = 0xA0B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp890:
	.size	fixup_memory_subreg, .Ltmp890-fixup_memory_subreg
	.cfi_endproc

	.align	16, 0x90
	.type	fixup_stack_1,@function
fixup_stack_1:                          # @fixup_stack_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp897:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp898:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp899:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp900:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp901:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp902:
	.cfi_def_cfa_offset 64
.Ltmp903:
	.cfi_offset %rbx, -48
.Ltmp904:
	.cfi_offset %r12, -40
.Ltmp905:
	.cfi_offset %r13, -32
.Ltmp906:
	.cfi_offset %r14, -24
.Ltmp907:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movl	(%r13), %edi
	movq	%rsi, %r14
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB92_10
# BB#1:                                 # %if.then
	movq	8(%r13), %r15
	movzbl	(%r15), %eax
	cmpq	$75, %rax
	jne	.LBB92_15
# BB#2:                                 # %land.lhs.true
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB92_15
# BB#3:                                 # %land.lhs.true10
	movl	8(%rax), %ecx
	leaq	-53(%rcx), %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	cmpq	$5, %rdx
	jb	.LBB92_7
# BB#4:                                 # %lor.lhs.false
	andq	%rbx, %rcx
	cmpq	$20, %rcx
	ja	.LBB92_6
# BB#5:                                 # %lor.lhs.false
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$1114304, %rdx          # imm = 0x1100C0
	jne	.LBB92_7
.LBB92_6:                               # %lor.lhs.false56
	movq	cfun(%rip), %rcx
	movq	128(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB92_15
.LBB92_7:                               # %land.lhs.true61
	movq	16(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB92_15
# BB#8:                                 # %if.then68
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%r15, %rsi
	callq	memory_address_p
	testq	%rbx, %rax
	jne	.LBB92_15
# BB#9:                                 # %if.end
	callq	start_sequence
	movq	%r15, %rdi
	callq	copy_to_reg
	movq	%rax, %r15
	callq	gen_sequence
	movq	%rax, %r12
	callq	end_sequence
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	%r13, %rdi
	movq	%r15, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	replace_equiv_address  # TAILCALL
.LBB92_10:                              # %if.end77
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB92_15
# BB#11:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB92_12:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB92_18 Depth 2
	movq	%r15, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r15
	cmpq	$69, %rax
	jne	.LBB92_13
# BB#17:                                # %for.cond104.preheader
                                        #   in Loop: Header=BB92_12 Depth=1
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r12
	jle	.LBB92_14
	.align	16, 0x90
.LBB92_18:                              # %for.body110
                                        #   Parent Loop BB92_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r12,8), %rdi
	movq	%r14, %rsi
	callq	fixup_stack_1
	movq	(%r13,%rbx,8), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB92_18
	jmp	.LBB92_14
	.align	16, 0x90
.LBB92_13:                              # %for.body
                                        #   in Loop: Header=BB92_12 Depth=1
	cmpq	$101, %rax
	jne	.LBB92_14
# BB#16:                                # %if.then88
                                        #   in Loop: Header=BB92_12 Depth=1
	movq	(%r13,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	fixup_stack_1
	movq	%rax, (%r13,%rbx,8)
.LBB92_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB92_12 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB92_12
.LBB92_15:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp908:
	.size	fixup_stack_1, .Ltmp908-fixup_stack_1
	.cfi_endproc

	.align	16, 0x90
	.type	walk_fixup_memory_subreg,@function
walk_fixup_memory_subreg:               # @walk_fixup_memory_subreg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp915:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp916:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp917:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp918:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp919:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp920:
	.cfi_def_cfa_offset 64
.Ltmp921:
	.cfi_offset %rbx, -48
.Ltmp922:
	.cfi_offset %r12, -40
.Ltmp923:
	.cfi_offset %r13, -32
.Ltmp924:
	.cfi_offset %r14, -24
.Ltmp925:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r13
	xorq	%rax, %rax
	testq	%r13, %r13
	je	.LBB93_5
# BB#1:                                 # %if.end
	movzbl	(%r13), %eax
	cmpq	$63, %rax
	jne	.LBB93_3
# BB#2:                                 # %land.lhs.true
	movq	8(%r13), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$66, %rcx
	jne	.LBB93_3
# BB#13:                                # %if.then5
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fixup_memory_subreg  # TAILCALL
.LBB93_3:                               # %if.end6
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB93_4
# BB#6:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB93_7:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB93_12 Depth 2
	movq	%r15, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r15
	cmpq	$69, %rax
	jne	.LBB93_8
# BB#11:                                # %for.cond33.preheader
                                        #   in Loop: Header=BB93_7 Depth=1
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r12
	jle	.LBB93_9
	.align	16, 0x90
.LBB93_12:                              # %for.body39
                                        #   Parent Loop BB93_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r12,8), %rdi
	movq	%r14, %rsi
	callq	walk_fixup_memory_subreg
	movq	(%r13,%rbx,8), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB93_12
	jmp	.LBB93_9
	.align	16, 0x90
.LBB93_8:                               # %for.body
                                        #   in Loop: Header=BB93_7 Depth=1
	cmpq	$101, %rax
	jne	.LBB93_9
# BB#10:                                # %if.then17
                                        #   in Loop: Header=BB93_7 Depth=1
	movq	(%r13,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	walk_fixup_memory_subreg
	movq	%rax, (%r13,%rbx,8)
.LBB93_9:                               # %for.cond.backedge
                                        #   in Loop: Header=BB93_7 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB93_7
.LBB93_4:
	movq	%r13, %rax
.LBB93_5:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp926:
	.size	walk_fixup_memory_subreg, .Ltmp926-walk_fixup_memory_subreg
	.cfi_endproc

	.align	16, 0x90
	.type	optimize_bit_field,@function
optimize_bit_field:                     # @optimize_bit_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp933:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp934:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp935:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp936:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp937:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp938:
	.cfi_def_cfa_offset 80
.Ltmp939:
	.cfi_offset %rbx, -48
.Ltmp940:
	.cfi_offset %r12, -40
.Ltmp941:
	.cfi_offset %r13, -32
.Ltmp942:
	.cfi_offset %r14, -24
.Ltmp943:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %r13
	movl	(%r13), %eax
	movq	%rsi, %r14
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$132, %rax
	movq	$1, %r12
	je	.LBB94_2
# BB#1:                                 # %if.else
	movq	16(%rbx), %r13
	xorq	%r12, %r12
.LBB94_2:                               # %if.end
	movq	16(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB94_42
# BB#3:                                 # %land.lhs.true
	movq	24(%r13), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB94_42
# BB#4:                                 # %land.lhs.true25
	movq	8(%rax), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	callq	mode_for_size
	movq	%rax, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
                                        # kill: RAX<def> R15<kill>
	andq	%rcx, %rax
	cmpq	$51, %rax
	je	.LBB94_42
# BB#5:                                 # %land.lhs.true33
	movq	24(%r13), %rax
	movq	8(%rax), %rax
	movq	16(%r13), %rcx
	cqto
	idivq	8(%rcx)
	testq	%rdx, %rdx
	jne	.LBB94_42
# BB#6:                                 # %if.then48
	movq	8(%r13), %rcx
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	je	.LBB94_9
# BB#7:                                 # %if.then48
	cmpq	$63, %rax
	jne	.LBB94_42
# BB#8:                                 # %land.lhs.true86
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	jne	.LBB94_42
.LBB94_9:                               # %if.end141
	testq	%rcx, %rcx
	je	.LBB94_42
# BB#10:                                # %land.lhs.true142
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	callq	mode_dependent_address_p
	movq	24(%rsp), %rdx          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB94_42
# BB#11:                                # %land.lhs.true148
	movzbl	3(%rdx), %eax
	testq	$8, %rax
	jne	.LBB94_42
# BB#12:                                # %if.then152
	movq	24(%r13), %rax
	movq	8(%rax), %rdx
	movq	8(%r13), %rax
	movq	$63, %rcx
	movq	%rdx, %r13
	sarq	%cl, %r13
	movq	$61, %rcx
	shrq	%cl, %r13
	movzbl	(%rax), %esi
	addq	%rdx, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	cmpq	$63, %rsi
	jne	.LBB94_14
# BB#13:                                # %if.then166
	movl	16(%rax), %eax
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	movq	$-4, %rcx
	subq	%rdx, %rcx
	andq	%rax, %rcx
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	andq	%rcx, %rax
	addq	%rax, %r13
.LBB94_14:                              # %if.end179
	leaq	8(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	callq	start_sequence
	movq	$1, %rcx
	movq	$1, %r8
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	adjust_address_1
	movq	%rax, %r15
	callq	get_insns
	movq	%rax, %r13
	callq	end_sequence
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	emit_insns_before
	testq	%r12, %r12
	je	.LBB94_29
# BB#15:                                # %if.then184
	movq	$1, %rcx
	movq	$1, %r12
	movq	%r14, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	callq	validate_change
	movq	16(%rbx), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-54, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	jbe	.LBB94_17
# BB#16:
	movq	%rsi, %rdx
.LBB94_21:                              # %while.end
	movq	%rdi, %r12
	addq	$16, %rbx
	movzbl	2(%r15), %edi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	%rdi, %rax
	je	.LBB94_23
# BB#22:                                # %if.then252
	callq	gen_lowpart
	movq	%rax, %rdx
.LBB94_23:                              # %if.end260
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	jmp	.LBB94_24
.LBB94_29:                              # %if.else289
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r10
	movl	target_flags(%rip), %r8d
	movq	$23, %rcx
	shrq	%cl, %r8
	movl	(%r10), %edx
	andq	$4, %r8
	addq	$4, %r8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %r8
	movq	%rax, %rsi
.LBB94_30:                              # %while.cond293
                                        # =>This Inner Loop Header: Depth=1
	movq	%r10, %r13
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB94_34
# BB#31:                                # %land.lhs.true298
                                        #   in Loop: Header=BB94_30 Depth=1
	movl	16(%r13), %ecx
	testq	%rcx, %rcx
	jne	.LBB94_34
# BB#32:                                # %land.lhs.true304
                                        #   in Loop: Header=BB94_30 Depth=1
	movq	$14, %rcx
	movq	8(%r13), %r10
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %eax
	movl	(%r10), %edx
	movq	$16, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	mode_class(,%rdi,4), %ecx
	cmpq	%rcx, %rax
	jne	.LBB94_34
# BB#33:                                # %land.rhs320
                                        #   in Loop: Header=BB94_30 Depth=1
	movzbl	mode_size(%rdi), %eax
	cmpq	%r8, %rax
	jbe	.LBB94_30
.LBB94_34:                              # %while.end340
	movq	%r9, %r12
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	validate_change
	movzbl	2(%r13), %edi
	movzbl	2(%r15), %eax
	cmpq	%rax, %rdi
	jne	.LBB94_36
# BB#35:                                # %if.then353
	addq	$16, %rbx
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
.LBB94_24:                              # %if.end376
	callq	validate_change
	xorq	%r13, %r13
	jmp	.LBB94_40
.LBB94_17:                              # %if.then184
	shlq	%cl, %r12
	testq	$24595, %r12            # imm = 0x6013
	je	.LBB94_18
# BB#25:                                # %if.else265
	movq	$16, %rcx
	shrq	%cl, %rax
	xorq	%r13, %r13
	andq	$255, %rax
	je	.LBB94_26
# BB#27:                                # %land.lhs.true274
	movzbl	2(%r15), %ecx
	andq	%rdi, %rax
	movq	%rdi, %r12
	cmpq	%rcx, %rax
	je	.LBB94_40
# BB#28:                                # %if.then286
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.optimize_bit_field, %rcx
	movq	$2815, %rsi             # imm = 0xAFF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB94_36:                              # %if.else358
	callq	gen_reg_rtx
	movq	%rax, %r13
	callq	start_sequence
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	addq	$16, %rbx
	cmpq	$133, %rax
	jne	.LBB94_37
# BB#38:                                # %if.else358
	movabsq	$1, %rdx
	jmp	.LBB94_39
.LBB94_26:
	movq	%rdi, %r12
	jmp	.LBB94_40
.LBB94_37:
	movabsq	$0, %rdx
.LBB94_39:                              # %if.else358
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	convert_move
	callq	get_insns
	movq	%r13, %r15
	movq	%rax, %r13
	callq	end_sequence
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	validate_change
.LBB94_40:                              # %if.end376
	callq	apply_change_group
	testq	%r12, %rax
	je	.LBB94_42
# BB#41:                                # %if.end376
	testq	%r13, %r13
	je	.LBB94_42
# BB#43:                                # %if.then381
	movq	%r13, %rdi
	movq	%r14, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insns_before  # TAILCALL
.LBB94_42:                              # %if.end385
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB94_18:                              # %if.then184
	movq	%rsi, %rdx
	cmpq	$9, %rcx
	jne	.LBB94_21
.LBB94_19:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB94_21
# BB#20:                                # %while.body
                                        #   in Loop: Header=BB94_19 Depth=1
	movq	8(%rdx), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	je	.LBB94_19
	jmp	.LBB94_21
.Ltmp944:
	.size	optimize_bit_field, .Ltmp944-optimize_bit_field
	.cfi_endproc

	.type	cfun,@object            # @cfun
	.bss
	.globl	cfun
	.align	8
cfun:
	.quad	0
	.size	cfun, 8

	.type	outer_function_chain,@object # @outer_function_chain
	.local	outer_function_chain
	.comm	outer_function_chain,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"function.c"
	.size	.L.str, 11

	.type	.L__FUNCTION__.find_function_data,@object # @__FUNCTION__.find_function_data
.L__FUNCTION__.find_function_data:
	.asciz	"find_function_data"
	.size	.L__FUNCTION__.find_function_data, 19

	.type	save_lang_status,@object # @save_lang_status
	.comm	save_lang_status,8,8
	.type	restore_lang_status,@object # @restore_lang_status
	.comm	restore_lang_status,8,8
	.type	virtuals_instantiated,@object # @virtuals_instantiated
	.comm	virtuals_instantiated,4,4
	.type	free_lang_status,@object # @free_lang_status
	.comm	free_lang_status,8,8
	.type	free_machine_status,@object # @free_machine_status
	.comm	free_machine_status,8,8
	.type	.L__FUNCTION__.assign_stack_temp_for_type,@object # @__FUNCTION__.assign_stack_temp_for_type
.L__FUNCTION__.assign_stack_temp_for_type:
	.asciz	"assign_stack_temp_for_type"
	.size	.L__FUNCTION__.assign_stack_temp_for_type, 27

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"size of variable `%s' is too large"
	.size	.L.str1, 35

	.type	inline_function_decl,@object # @inline_function_decl
	.comm	inline_function_decl,8,8
	.type	.L__FUNCTION__.purge_addressof,@object # @__FUNCTION__.purge_addressof
.L__FUNCTION__.purge_addressof:
	.asciz	"purge_addressof"
	.size	.L__FUNCTION__.purge_addressof, 16

	.type	purge_bitfield_addressof_replacements,@object # @purge_bitfield_addressof_replacements
	.local	purge_bitfield_addressof_replacements
	.comm	purge_bitfield_addressof_replacements,8,8
	.type	purge_addressof_replacements,@object # @purge_addressof_replacements
	.local	purge_addressof_replacements
	.comm	purge_addressof_replacements,8,8
	.type	dynamic_offset,@object  # @dynamic_offset
	.local	dynamic_offset
	.comm	dynamic_offset,4,4
	.type	.L__FUNCTION__.get_first_block_beg,@object # @__FUNCTION__.get_first_block_beg
.L__FUNCTION__.get_first_block_beg:
	.asciz	"get_first_block_beg"
	.size	.L__FUNCTION__.get_first_block_beg, 20

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"__builtin_va_alist"
	.size	.L.str2, 19

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"`%s' might be used uninitialized in this function"
	.size	.L.str3, 50

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"variable `%s' might be clobbered by `longjmp' or `vfork'"
	.size	.L.str4, 57

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"argument `%s' might be clobbered by `longjmp' or `vfork'"
	.size	.L.str5, 57

	.type	.L__FUNCTION__.lookup_static_chain,@object # @__FUNCTION__.lookup_static_chain
.L__FUNCTION__.lookup_static_chain:
	.asciz	"lookup_static_chain"
	.size	.L__FUNCTION__.lookup_static_chain, 20

	.type	.L__FUNCTION__.fix_lexical_addr,@object # @__FUNCTION__.fix_lexical_addr
.L__FUNCTION__.fix_lexical_addr:
	.asciz	"fix_lexical_addr"
	.size	.L__FUNCTION__.fix_lexical_addr, 17

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"block_stack"
	.size	.L.str6, 12

	.type	next_block_index,@object # @next_block_index
	.data
	.align	4
next_block_index:
	.long	2                       # 0x2
	.size	next_block_index, 4

	.type	.L.str7,@object         # @.str7
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str7:
	.asciz	"function returns an aggregate"
	.size	.L.str7, 30

	.type	prologue,@object        # @prologue
	.local	prologue
	.comm	prologue,8,8
	.type	epilogue,@object        # @epilogue
	.local	epilogue
	.comm	epilogue,8,8
	.type	sibcall_epilogue,@object # @sibcall_epilogue
	.local	sibcall_epilogue
	.comm	sibcall_epilogue,8,8
	.type	.L__FUNCTION__.expand_function_start,@object # @__FUNCTION__.expand_function_start
.L__FUNCTION__.expand_function_start:
	.asciz	"expand_function_start"
	.size	.L__FUNCTION__.expand_function_start, 22

	.type	profile_label_no,@object # @profile_label_no
	.local	profile_label_no
	.comm	profile_label_no,4,4
	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"unused parameter `%s'"
	.size	.L.str8, 22

	.type	.L__FUNCTION__.expand_function_end,@object # @__FUNCTION__.expand_function_end
.L__FUNCTION__.expand_function_end:
	.asciz	"expand_function_end"
	.size	.L__FUNCTION__.expand_function_end, 20

	.type	.L__FUNCTION__.thread_prologue_and_epilogue_insns,@object # @__FUNCTION__.thread_prologue_and_epilogue_insns
.L__FUNCTION__.thread_prologue_and_epilogue_insns:
	.asciz	"thread_prologue_and_epilogue_insns"
	.size	.L__FUNCTION__.thread_prologue_and_epilogue_insns, 35

	.type	mark_machine_status,@object # @mark_machine_status
	.comm	mark_machine_status,8,8
	.type	mark_lang_status,@object # @mark_lang_status
	.comm	mark_lang_status,8,8
	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"prologue"
	.size	.L.str9, 9

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"epilogue"
	.size	.L.str10, 9

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"sibcall_epilogue"
	.size	.L.str11, 17

	.type	current_function_is_leaf,@object # @current_function_is_leaf
	.comm	current_function_is_leaf,4,4
	.type	current_function_nothrow,@object # @current_function_nothrow
	.comm	current_function_nothrow,4,4
	.type	current_function_sp_is_unchanging,@object # @current_function_sp_is_unchanging
	.comm	current_function_sp_is_unchanging,4,4
	.type	current_function_uses_only_leaf_regs,@object # @current_function_uses_only_leaf_regs
	.comm	current_function_uses_only_leaf_regs,4,4
	.type	init_machine_status,@object # @init_machine_status
	.comm	init_machine_status,8,8
	.type	init_lang_status,@object # @init_lang_status
	.comm	init_lang_status,8,8
	.type	.L__FUNCTION__.keep_stack_depressed,@object # @__FUNCTION__.keep_stack_depressed
.L__FUNCTION__.keep_stack_depressed:
	.asciz	"keep_stack_depressed"
	.size	.L__FUNCTION__.keep_stack_depressed, 21

	.type	.L__FUNCTION__.handle_epilogue_set,@object # @__FUNCTION__.handle_epilogue_set
.L__FUNCTION__.handle_epilogue_set:
	.asciz	"handle_epilogue_set"
	.size	.L__FUNCTION__.handle_epilogue_set, 20

	.type	.L__FUNCTION__.identify_blocks_1,@object # @__FUNCTION__.identify_blocks_1
.L__FUNCTION__.identify_blocks_1:
	.asciz	"identify_blocks_1"
	.size	.L__FUNCTION__.identify_blocks_1, 18

	.type	.L__FUNCTION__.instantiate_virtual_regs_1,@object # @__FUNCTION__.instantiate_virtual_regs_1
.L__FUNCTION__.instantiate_virtual_regs_1:
	.asciz	"instantiate_virtual_regs_1"
	.size	.L__FUNCTION__.instantiate_virtual_regs_1, 27

	.type	.L__FUNCTION__.purge_addressof_1,@object # @__FUNCTION__.purge_addressof_1
.L__FUNCTION__.purge_addressof_1:
	.asciz	"purge_addressof_1"
	.size	.L__FUNCTION__.purge_addressof_1, 18

	.type	.L__FUNCTION__.put_addressof_into_stack,@object # @__FUNCTION__.put_addressof_into_stack
.L__FUNCTION__.put_addressof_into_stack:
	.asciz	"put_addressof_into_stack"
	.size	.L__FUNCTION__.put_addressof_into_stack, 25

	.type	.L__FUNCTION__.fixup_var_refs,@object # @__FUNCTION__.fixup_var_refs
.L__FUNCTION__.fixup_var_refs:
	.asciz	"fixup_var_refs"
	.size	.L__FUNCTION__.fixup_var_refs, 15

	.type	.L__FUNCTION__.fixup_memory_subreg,@object # @__FUNCTION__.fixup_memory_subreg
.L__FUNCTION__.fixup_memory_subreg:
	.asciz	"fixup_memory_subreg"
	.size	.L__FUNCTION__.fixup_memory_subreg, 20

	.type	.L__FUNCTION__.fixup_var_refs_1,@object # @__FUNCTION__.fixup_var_refs_1
.L__FUNCTION__.fixup_var_refs_1:
	.asciz	"fixup_var_refs_1"
	.size	.L__FUNCTION__.fixup_var_refs_1, 17

	.type	.L__FUNCTION__.optimize_bit_field,@object # @__FUNCTION__.optimize_bit_field
.L__FUNCTION__.optimize_bit_field:
	.asciz	"optimize_bit_field"
	.size	.L__FUNCTION__.optimize_bit_field, 19


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
