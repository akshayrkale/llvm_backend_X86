	.file	"haifa-sched.c.bc"
	.text
	.globl	fix_sched_param
	.align	16, 0x90
	.type	fix_sched_param,@function
fix_sched_param:                        # @fix_sched_param
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movabsq	$.L.str, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_1
# BB#2:                                 # %if.else
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	warning  # TAILCALL
.LBB0_1:                                # %if.end
	movq	%r14, %rdi
	callq	atoi
	movl	%eax, sched_verbose_param(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	fix_sched_param, .Ltmp8-fix_sched_param
	.cfi_endproc

	.globl	insn_unit
	.align	16, 0x90
	.type	insn_unit,@function
insn_unit:                              # @insn_unit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 16
.Ltmp12:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	42(%rdx,%rax), %edx
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB1_6
# BB#1:                                 # %if.then
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB1_4
# BB#2:                                 # %cond.end
	movq	%rbx, %rdi
	callq	recog_memoized_1
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	js	.LBB1_3
.LBB1_4:                                # %if.end21
	movq	%rbx, %rdi
	callq	function_units_used
	movq	%rax, %rdx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rdx, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	xorq	$1, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB1_6
# BB#5:                                 # %if.then26
	movslq	8(%rbx), %rcx
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movw	%ax, 42(%rdx,%rsi)
	jmp	.LBB1_6
.LBB1_3:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB1_6:                                # %if.end35
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB1_7
# BB#8:                                 # %if.end35
	movabsq	$1, %rcx
	jmp	.LBB1_9
.LBB1_7:
	movabsq	$0, %rcx
.LBB1_9:                                # %if.end35
	subq	%rcx, %rax
	popq	%rbx
	retq
.Ltmp13:
	.size	insn_unit, .Ltmp13-insn_unit
	.cfi_endproc

	.globl	get_unit_last_insn
	.align	16, 0x90
	.type	get_unit_last_insn,@function
get_unit_last_insn:                     # @get_unit_last_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	unit_last_insn(,%rdi,8), %rax
	retq
.Ltmp14:
	.size	get_unit_last_insn, .Ltmp14-get_unit_last_insn
	.cfi_endproc

	.globl	insn_issue_delay
	.align	16, 0x90
	.type	insn_issue_delay,@function
insn_issue_delay:                       # @insn_issue_delay
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
	subq	$16, %rsp
.Ltmp26:
	.cfi_def_cfa_offset 64
.Ltmp27:
	.cfi_offset %rbx, -48
.Ltmp28:
	.cfi_offset %r12, -40
.Ltmp29:
	.cfi_offset %r13, -32
.Ltmp30:
	.cfi_offset %r14, -24
.Ltmp31:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	42(%rdx,%rax), %eax
	movq	$48, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	testq	%rax, %rax
	jne	.LBB3_6
# BB#1:                                 # %if.then.i
	movslq	40(%r14), %rax
	testq	%rax, %rax
	jns	.LBB3_4
# BB#2:                                 # %cond.end.i
	movq	%r14, %rdi
	callq	recog_memoized_1
	movslq	40(%r14), %rax
	testq	%rax, %rax
	js	.LBB3_3
.LBB3_4:                                # %if.end21.i
	movq	%r14, %rdi
	callq	function_units_used
	movabsq	$2147483648, %rbx       # imm = 0x80000000
	andq	%rax, %rbx
	movq	$31, %rcx
	shrq	%cl, %rbx
	xorq	$1, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_6
# BB#5:                                 # %if.then26.i
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movw	%bx, 42(%rdx,%rax)
	jmp	.LBB3_6
.LBB3_3:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB3_6:                                # %insn_unit.exit
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_7
# BB#8:                                 # %insn_unit.exit
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB3_9
.LBB3_7:
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movabsq	$0, %rax
.LBB3_9:                                # %insn_unit.exit
	subq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_13
# BB#10:                                # %if.then
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	(%rbx,%rbx,8), %rax
	xorq	%r15, %r15
	cmpq	$0, function_units+56(,%rax,8)
	je	.LBB3_21
# BB#11:                                # %land.lhs.true
	movq	function_units+64(,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB3_21
# BB#12:                                # %if.then4
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*%rax  # TAILCALL
.LBB3_13:                               # %if.else
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	andq	%r12, %rax
	xorq	%r15, %r15
	cmpq	%r12, %rax
	je	.LBB3_21
# BB#14:                                # %for.body.lr.ph
	notq	%rbx
	xorq	%r15, %r15
	movabsq	$function_units+64, %r13
	.align	16, 0x90
.LBB3_15:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rbx
	je	.LBB3_20
# BB#16:                                # %land.lhs.true11
                                        #   in Loop: Header=BB3_15 Depth=1
	cmpq	$0, -8(%r13)
	je	.LBB3_20
# BB#17:                                # %land.lhs.true16
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	(%r13), %r14
	testq	%r14, %r14
	je	.LBB3_20
# BB#18:                                # %if.then21
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, %rsi
	callq	*%r14
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB3_20
# BB#19:                                # %cond.false
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, %rsi
	callq	*%r14
	movq	%rax, %r15
	.align	16, 0x90
.LBB3_20:                               # %for.inc
                                        #   in Loop: Header=BB3_15 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$33, %rcx
	sarq	%cl, %rbx
	addq	$72, %r13
	testq	%r12, %rbx
	jne	.LBB3_15
.LBB3_21:                               # %if.end32
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp32:
	.size	insn_issue_delay, .Ltmp32-insn_issue_delay
	.cfi_endproc

	.globl	actual_hazard_this_instance
	.align	16, 0x90
	.type	actual_hazard_this_instance,@function
actual_hazard_this_instance:            # @actual_hazard_this_instance
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp39:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp40:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp41:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp42:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp44:
	.cfi_def_cfa_offset 64
.Ltmp45:
	.cfi_offset %rbx, -48
.Ltmp46:
	.cfi_offset %r12, -40
.Ltmp47:
	.cfi_offset %r13, -32
.Ltmp48:
	.cfi_offset %r14, -24
.Ltmp49:
	.cfi_offset %r15, -16
	movq	%r8, %r15
	movq	%rcx, %r14
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	unit_tick(,%rsi,4), %ebx
	movq	%rdi, %r13
	movq	%rbx, %rdi
	subq	%r14, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jle	.LBB4_12
# BB#1:                                 # %if.then
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,8), %r12
	movq	function_units+56(,%r12,8), %r8
	testq	%r8, %r8
	je	.LBB4_8
# BB#2:                                 # %if.then3
	movq	function_units+64(,%r12,8), %rax
	testq	%rax, %rax
	je	.LBB4_4
# BB#3:                                 # %if.then7
	movq	unit_last_insn(,%rsi,8), %rdi
	movq	%rdx, %rsi
	callq	*%rax
	movl	function_units+48(,%r12,8), %ecx
	addq	%rbx, %rax
	subq	%rcx, %rax
	movq	%rax, %rbx
	jmp	.LBB4_8
.LBB4_12:                               # %if.end28
	movq	%r15, %rax
	jmp	.LBB4_11
.LBB4_4:                                # %if.else
	movslq	8(%rdx), %rax
	movq	h_i_d(%rip), %rsi
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	28(%rsi,%rax), %eax
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	incq	%r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r13, %rcx
	cmpq	%rcx, %rsi
	jne	.LBB4_5
# BB#6:                                 # %if.else.i
	movq	$8, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	andq	$255, %rax
	orq	%rdx, %rax
	jmp	.LBB4_7
.LBB4_5:                                # %if.then.i
	movq	%rdx, %rdi
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	callq	*%r8
	movabsq	$4294901760, %rdx       # imm = 0xFFFF0000
	andq	%rax, %rdx
	movq	$8, %rcx
	shlq	%cl, %r13
	movq	$16, %rcx
	shrq	%cl, %rdx
	orq	%r13, %rdx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	movslq	8(%rcx), %rcx
	orq	%rdx, %rsi
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%esi, 28(%rdx,%rdi)
.LBB4_7:                                # %blockage_range.exit
	movl	function_units+48(,%r12,8), %ecx
	andq	$65535, %rax            # imm = 0xFFFF
	subq	%rcx, %rbx
	addq	%rax, %rbx
.LBB4_8:                                # %if.end22
	subq	%r14, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB4_10
# BB#9:                                 # %if.end22
	movq	%r15, %rbx
.LBB4_10:                               # %if.end22
	movq	%rbx, %rax
.LBB4_11:                               # %if.end22
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp50:
	.size	actual_hazard_this_instance, .Ltmp50-actual_hazard_this_instance
	.cfi_endproc

	.globl	insn_cost
	.align	16, 0x90
	.type	insn_cost,@function
insn_cost:                              # @insn_cost
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp58:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp59:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp60:
	.cfi_def_cfa_offset 48
.Ltmp61:
	.cfi_offset %rbx, -40
.Ltmp62:
	.cfi_offset %r12, -32
.Ltmp63:
	.cfi_offset %r14, -24
.Ltmp64:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movslq	8(%r15), %rax
	movq	h_i_d(%rip), %rdi
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	40(%rdi,%rax), %eax
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	$48, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	testq	%rax, %rax
	jne	.LBB5_7
# BB#1:                                 # %if.then
	movslq	40(%r15), %rax
	testq	%rax, %rax
	jns	.LBB5_4
# BB#2:                                 # %cond.end
	movq	%r15, %rdi
	callq	recog_memoized_1
	movslq	40(%r15), %rax
	testq	%rax, %rax
	js	.LBB5_3
.LBB5_4:                                # %if.else
	movq	%r15, %rdi
	callq	result_ready_cost
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	$1, %rbx
	jle	.LBB5_6
# BB#5:                                 # %if.else
	movq	%rax, %rbx
.LBB5_6:                                # %if.else
	movslq	8(%r15), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movw	%bx, 40(%rdx,%rax)
.LBB5_7:                                # %if.end36
	movq	%r14, %rax
	orq	%r12, %rax
	je	.LBB5_18
# BB#8:                                 # %if.end42
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB5_11
# BB#9:                                 # %cond.end54
	movq	%r12, %rdi
	callq	recog_memoized_1
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB5_11
# BB#10:                                # %if.then61
	movl	(%r14), %eax
	orq	$33554432, %rax         # imm = 0x2000000
	movl	%eax, (%r14)
.LBB5_11:                               # %if.end62
	movl	(%r14), %ecx
	xorq	%rax, %rax
	testq	$33554432, %rcx         # imm = 0x2000000
	jne	.LBB5_19
# BB#12:                                # %if.else66
	testq	$16777216, %rcx         # imm = 0x1000000
	jne	.LBB5_18
# BB#13:                                # %if.else66
	movq	targetm+168(%rip), %r8
	testq	%r8, %r8
	je	.LBB5_18
# BB#14:                                # %if.then73
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	*%r8
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB5_16
# BB#15:                                # %if.then77
	movl	(%r14), %eax
	orq	$33554432, %rax         # imm = 0x2000000
	movl	%eax, (%r14)
	xorq	%rax, %rax
.LBB5_16:                               # %if.end81
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	andq	%rbx, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB5_19
# BB#17:                                # %if.then84
	movl	(%r14), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, (%r14)
.LBB5_18:                               # %return
	movq	%rbx, %rax
.LBB5_19:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB5_3:                                # %if.then17
	movslq	8(%r15), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	$1, %rax
	movw	%ax, 40(%rdx,%rsi)
	jmp	.LBB5_19
.Ltmp65:
	.size	insn_cost, .Ltmp65-insn_cost
	.cfi_endproc

	.globl	ready_add
	.align	16, 0x90
	.type	ready_add,@function
ready_add:                              # @ready_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp71:
	.cfi_def_cfa_offset 32
.Ltmp72:
	.cfi_offset %rbx, -24
.Ltmp73:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	12(%rbx), %eax
	movq	%rsi, %r14
	movq	$32, %rcx
	movq	%rax, %rdx
	movl	16(%rbx), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rax
	jne	.LBB6_3
# BB#1:                                 # %if.then
	testq	%rax, %rax
	je	.LBB6_4
# BB#2:                                 # %ready_lastpos.exit
	movq	(%rbx), %rsi
	movslq	8(%rbx), %rax
	subq	%rdx, %rax
	leaq	(%rsi,%rax,8), %rdi
	addq	$8, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memmove
	movl	8(%rbx), %eax
	decq	%rax
	movl	%eax, 12(%rbx)
	movl	16(%rbx), %esi
.LBB6_3:                                # %if.end
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	(%rbx), %rdx
	sarq	%cl, %rax
	movq	%r14, (%rdx,%rax,8)
	incq	%rsi
	movl	%esi, 16(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB6_4:                                # %if.then.i
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.ready_lastpos, %rcx
	movq	$893, %rsi              # imm = 0x37D
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp74:
	.size	ready_add, .Ltmp74-ready_add
	.cfi_endproc

	.globl	get_block_head_tail
	.align	16, 0x90
	.type	get_block_head_tail,@function
get_block_head_tail:                    # @get_block_head_tail
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	32(%rax,%rdi,8), %rax
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB7_1
.LBB7_2:                                # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_4 Depth 2
	movzbl	(%rcx), %r8d
	cmpq	$37, %r8
	je	.LBB7_3
	.align	16, 0x90
.LBB7_4:                                # %if.else
                                        #   Parent Loop BB7_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %edi
	cmpq	$37, %rdi
	jne	.LBB7_6
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB7_4 Depth=2
	movq	16(%rax), %rax
	cmpq	%rax, %rcx
	jne	.LBB7_4
	jmp	.LBB7_1
	.align	16, 0x90
.LBB7_6:                                # %if.else15
                                        #   in Loop: Header=BB7_2 Depth=1
	cmpq	$36, %r8
	jne	.LBB7_7
.LBB7_3:                                # %while.cond.outer.backedge
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rcx), %rcx
	cmpq	%rax, %rcx
	movq	%rax, %rdi
	jne	.LBB7_2
	jmp	.LBB7_8
.LBB7_1:
	movq	%rcx, %rdi
	movq	%rcx, %rax
.LBB7_8:                                # %while.end
	movq	%rdi, (%rsi)
	movq	%rax, (%rdx)
	retq
.LBB7_7:
	movq	%rcx, %rdi
	jmp	.LBB7_8
.Ltmp75:
	.size	get_block_head_tail, .Ltmp75-get_block_head_tail
	.cfi_endproc

	.globl	no_real_insns_p
	.align	16, 0x90
	.type	no_real_insns_p,@function
no_real_insns_p:                        # @no_real_insns_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rsi), %rcx
	cmpq	%rdi, %rcx
	je	.LBB8_4
# BB#1:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB8_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdi), %edx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$36, %rdx
	jne	.LBB8_5
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	24(%rdi), %rdi
	cmpq	%rcx, %rdi
	jne	.LBB8_2
.LBB8_4:
	movq	$1, %rax
.LBB8_5:                                # %return
	retq
.Ltmp76:
	.size	no_real_insns_p, .Ltmp76-no_real_insns_p
	.cfi_endproc

	.globl	rm_line_notes
	.align	16, 0x90
	.type	rm_line_notes,@function
rm_line_notes:                          # @rm_line_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp79:
	.cfi_def_cfa_offset 16
.Ltmp80:
	.cfi_offset %rbx, -16
	movq	24(%rsi), %r9
	cmpq	%rdi, %r9
	je	.LBB9_5
# BB#1:
	movq	%rdi, %r8
	.align	16, 0x90
.LBB9_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_8 Depth 2
                                        #       Child Loop BB9_15 Depth 3
                                        #       Child Loop BB9_9 Depth 3
	movzbl	(%r8), %eax
	cmpq	$37, %rax
	jne	.LBB9_3
# BB#7:                                 # %land.rhs.lr.ph.lr.ph.i
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	16(%r8), %r10
	movq	%r8, %rdx
.LBB9_8:                                # %land.rhs.lr.ph.i
                                        #   Parent Loop BB9_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_15 Depth 3
                                        #       Child Loop BB9_9 Depth 3
	testq	%r10, %r10
	movq	%rdx, %rax
	jne	.LBB9_15
	.align	16, 0x90
.LBB9_9:                                # %land.rhs.us.i
                                        #   Parent Loop BB9_2 Depth=1
                                        #     Parent Loop BB9_8 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB9_22
# BB#10:                                # %while.body.us.i
                                        #   in Loop: Header=BB9_9 Depth=3
	movl	write_symbols(%rip), %ecx
	movq	24(%rax), %rdx
	testq	%rcx, %rcx
	je	.LBB9_6
# BB#11:                                # %land.lhs.true.us.i
                                        #   in Loop: Header=BB9_9 Depth=3
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB9_6
# BB#12:                                # %if.end.us.i
                                        #   in Loop: Header=BB9_9 Depth=3
	testq	%rdx, %rdx
	je	.LBB9_14
# BB#13:                                # %if.then14.us.i
                                        #   in Loop: Header=BB9_9 Depth=3
	movq	%r10, 16(%rdx)
.LBB9_14:                               # %if.end18.us.i
                                        #   in Loop: Header=BB9_9 Depth=3
	movslq	8(%rax), %rcx
	movq	h_i_d(%rip), %r11
	leaq	(%rcx,%rcx,2), %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	%rax, 8(%r11,%rbx)
	cmpq	%r9, %rdx
	movq	%rdx, %rax
	jne	.LBB9_9
	jmp	.LBB9_21
	.align	16, 0x90
.LBB9_15:                               # %land.rhs.i
                                        #   Parent Loop BB9_2 Depth=1
                                        #     Parent Loop BB9_8 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB9_22
# BB#16:                                # %while.body.i
                                        #   in Loop: Header=BB9_15 Depth=3
	movl	write_symbols(%rip), %ecx
	movq	24(%rax), %rdx
	testq	%rcx, %rcx
	je	.LBB9_6
# BB#17:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB9_15 Depth=3
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB9_6
# BB#18:                                # %if.end.i
                                        #   in Loop: Header=BB9_15 Depth=3
	testq	%rdx, %rdx
	movq	%rdx, 24(%r10)
	je	.LBB9_20
# BB#19:                                # %if.then14.i
                                        #   in Loop: Header=BB9_15 Depth=3
	movq	%r10, 16(%rdx)
.LBB9_20:                               # %if.end18.i
                                        #   in Loop: Header=BB9_15 Depth=3
	movslq	8(%rax), %rcx
	movq	h_i_d(%rip), %r11
	leaq	(%rcx,%rcx,2), %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	%rax, 8(%r11,%rbx)
	cmpq	%r9, %rdx
	movq	%rdx, %rax
	jne	.LBB9_15
	jmp	.LBB9_21
	.align	16, 0x90
.LBB9_6:                                # %while.cond.outer.loopexit.i
                                        #   in Loop: Header=BB9_8 Depth=2
	cmpq	%r9, %rdx
	movq	%rax, %r10
	jne	.LBB9_8
	.align	16, 0x90
.LBB9_21:                               #   in Loop: Header=BB9_2 Depth=1
	movq	%r9, %rax
.LBB9_22:                               # %unlink_line_notes.exit
                                        #   in Loop: Header=BB9_2 Depth=1
	cmpq	%rsi, %r8
	je	.LBB9_23
# BB#25:                                # %if.end
                                        #   in Loop: Header=BB9_2 Depth=1
	cmpq	%rdi, %r8
	je	.LBB9_26
# BB#27:                                # %if.end6
                                        #   in Loop: Header=BB9_2 Depth=1
	cmpq	%r9, %rax
	jne	.LBB9_4
	jmp	.LBB9_28
	.align	16, 0x90
.LBB9_3:                                #   in Loop: Header=BB9_2 Depth=1
	movq	%r8, %rax
.LBB9_4:                                # %for.inc
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	24(%rax), %r8
	cmpq	%r9, %r8
	jne	.LBB9_2
.LBB9_5:                                # %for.end
	popq	%rbx
	retq
.LBB9_23:                               # %if.then3
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.rm_line_notes, %rcx
	movq	$1191, %rsi             # imm = 0x4A7
	jmp	.LBB9_24
.LBB9_26:                               # %if.then5
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.rm_line_notes, %rcx
	movq	$1193, %rsi             # imm = 0x4A9
	jmp	.LBB9_24
.LBB9_28:                               # %if.then8
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.rm_line_notes, %rcx
	movq	$1195, %rsi             # imm = 0x4AB
.LBB9_24:                               # %if.then3
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp81:
	.size	rm_line_notes, .Ltmp81-rm_line_notes
	.cfi_endproc

	.globl	save_line_notes
	.align	16, 0x90
	.type	save_line_notes,@function
save_line_notes:                        # @save_line_notes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdx), %r8
	cmpq	%rsi, %r8
	je	.LBB10_6
# BB#1:                                 # %for.body.lr.ph
	movq	line_note_head(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	(%rdx,%rdi,8), %rdx
	.align	16, 0x90
.LBB10_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rsi), %ecx
	cmpq	$37, %rcx
	jne	.LBB10_4
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB10_2 Depth=1
	movslq	40(%rsi), %rcx
	testq	%rcx, %rcx
	movq	%rsi, %rcx
	jg	.LBB10_5
.LBB10_4:                               # %if.else
                                        #   in Loop: Header=BB10_2 Depth=1
	movslq	8(%rsi), %rcx
	movq	h_i_d(%rip), %rdi
	leaq	(%rcx,%rcx,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rdx, 8(%rdi,%rax)
	movq	%rdx, %rcx
.LBB10_5:                               # %for.inc
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	24(%rsi), %rsi
	cmpq	%r8, %rsi
	movq	%rcx, %rdx
	jne	.LBB10_2
.LBB10_6:                               # %for.end
	retq
.Ltmp82:
	.size	save_line_notes, .Ltmp82-save_line_notes
	.cfi_endproc

	.globl	restore_line_notes
	.align	16, 0x90
	.type	restore_line_notes,@function
restore_line_notes:                     # @restore_line_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp89:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp90:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp91:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp92:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp93:
	.cfi_def_cfa_offset 48
.Ltmp94:
	.cfi_offset %rbx, -48
.Ltmp95:
	.cfi_offset %r12, -40
.Ltmp96:
	.cfi_offset %r13, -32
.Ltmp97:
	.cfi_offset %r14, -24
.Ltmp98:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	24(%rsi), %r15
	xorq	%r14, %r14
	testq	%rbx, %rbx
	movabsq	$0, %rax
	je	.LBB11_4
# BB#1:
	movq	%rbx, %rax
	.align	16, 0x90
.LBB11_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB11_9
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB11_2 Depth=1
	movslq	40(%rax), %rcx
	testq	%rcx, %rcx
	jg	.LBB11_4
.LBB11_9:                               # %for.inc
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB11_2
# BB#10:
	xorq	%rax, %rax
.LBB11_4:                               # %for.cond7.preheader
	cmpq	%rbx, %r15
	je	.LBB11_27
# BB#5:
	movabsq	$3221225471, %r12       # imm = 0xBFFFFFFF
	.align	16, 0x90
.LBB11_6:                               # %for.body9
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$37, %rcx
	jne	.LBB11_11
# BB#7:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB11_6 Depth=1
	movslq	40(%rbx), %rcx
	testq	%rcx, %rcx
	movq	%rbx, %r13
	jg	.LBB11_24
# BB#8:                                 # %select.mid
                                        #   in Loop: Header=BB11_6 Depth=1
	movq	%rax, %r13
	jmp	.LBB11_24
	.align	16, 0x90
.LBB11_11:                              # %land.lhs.true22
                                        #   in Loop: Header=BB11_6 Depth=1
	movslq	8(%rbx), %rcx
	movslq	old_max_uid(%rip), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB11_12
# BB#13:                                # %land.lhs.true27
                                        #   in Loop: Header=BB11_6 Depth=1
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	8(%rdx,%rsi), %r13
	testq	%r13, %r13
	je	.LBB11_14
# BB#15:                                # %land.lhs.true27
                                        #   in Loop: Header=BB11_6 Depth=1
	cmpq	%rax, %r13
	je	.LBB11_16
# BB#17:                                # %land.lhs.true35
                                        #   in Loop: Header=BB11_6 Depth=1
	testq	%rax, %rax
	je	.LBB11_21
# BB#18:                                # %lor.lhs.false
                                        #   in Loop: Header=BB11_6 Depth=1
	movl	40(%r13), %ecx
	movl	40(%rax), %esi
	cmpq	%rsi, %rcx
	jne	.LBB11_21
# BB#19:                                # %lor.lhs.false44
                                        #   in Loop: Header=BB11_6 Depth=1
	movq	32(%r13), %rcx
	movq	32(%rax), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB11_21
# BB#20:                                #   in Loop: Header=BB11_6 Depth=1
	movq	%rax, %r13
	jmp	.LBB11_24
	.align	16, 0x90
.LBB11_12:                              #   in Loop: Header=BB11_6 Depth=1
	movq	%rax, %r13
	jmp	.LBB11_24
.LBB11_21:                              # %if.then51
                                        #   in Loop: Header=BB11_6 Depth=1
	movslq	8(%r13), %rax
	movq	16(%rbx), %rsi
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	cmpq	$0, 8(%rdx,%rax)
	jne	.LBB11_22
# BB#23:                                # %if.else81
                                        #   in Loop: Header=BB11_6 Depth=1
	movl	40(%r13), %edi
	incq	%r14
	callq	emit_note_after
	movq	32(%r13), %rcx
	movq	%rcx, 32(%rax)
	movl	(%r13), %ecx
	movl	(%rax), %edx
	andq	$1073741824, %rcx       # imm = 0x40000000
	andq	%r12, %rdx
	orq	%rcx, %rdx
	movl	%edx, (%rax)
	jmp	.LBB11_24
.LBB11_14:                              #   in Loop: Header=BB11_6 Depth=1
	movq	%rax, %r13
	jmp	.LBB11_24
.LBB11_16:                              #   in Loop: Header=BB11_6 Depth=1
	movq	%rax, %r13
	jmp	.LBB11_24
.LBB11_22:                              # %if.then62
                                        #   in Loop: Header=BB11_6 Depth=1
	leaq	8(%rdx,%rax), %rax
	movq	$0, (%rax)
	movq	%rsi, 16(%r13)
	movq	%r13, 24(%rsi)
	movq	%r13, 16(%rbx)
	movq	%rbx, 24(%r13)
	.align	16, 0x90
.LBB11_24:                              # %for.inc98
                                        #   in Loop: Header=BB11_6 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r15, %rbx
	movq	%r13, %rax
	jne	.LBB11_6
# BB#25:                                # %for.end102
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_27
# BB#26:                                # %for.end102
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB11_27
# BB#28:                                # %if.then106
	movq	sched_dump(%rip), %rdi
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB11_27:                              # %if.end108
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp99:
	.size	restore_line_notes, .Ltmp99-restore_line_notes
	.cfi_endproc

	.globl	rm_redundant_line_notes
	.align	16, 0x90
	.type	rm_redundant_line_notes,@function
rm_redundant_line_notes:                # @rm_redundant_line_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp101:
	.cfi_def_cfa_offset 16
	callq	get_insns
	callq	get_last_insn
	testq	%rax, %rax
	je	.LBB12_20
# BB#1:
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movabsq	$4294967197, %r9        # imm = 0xFFFFFF9D
	xorq	%rcx, %rcx
	xorq	%rdi, %rdi
	jmp	.LBB12_2
.LBB12_7:                               # %if.else
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	jne	.LBB12_9
# BB#8:                                 #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %rdi
	jmp	.LBB12_17
.LBB12_9:                               # %land.lhs.true11
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	40(%rdi), %r10d
	andq	%r8, %rsi
	cmpq	%rsi, %r10
	jne	.LBB12_10
# BB#11:                                # %land.lhs.true19
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%rdi), %r10
	movq	32(%rax), %rsi
	cmpq	%rsi, %r10
	jne	.LBB12_12
# BB#13:                                # %if.then27
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%rdx
	movq	$0, 32(%rdi)
	movl	%r9d, 40(%rdi)
	movq	%rax, %rdi
	jmp	.LBB12_17
.LBB12_10:                              #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %rdi
	jmp	.LBB12_17
.LBB12_12:                              #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %rdi
	jmp	.LBB12_17
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %esi
	cmpq	$32, %rsi
	jne	.LBB12_3
# BB#15:                                # %land.lhs.true49
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	32(%rax), %rsi
	movl	(%rsi), %esi
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$48, %rsi
	je	.LBB12_17
	jmp	.LBB12_16
	.align	16, 0x90
.LBB12_3:                               # %for.body
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$37, %rsi
	jne	.LBB12_16
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB12_2 Depth=1
	movslq	40(%rax), %rsi
	testq	%rsi, %rsi
	jle	.LBB12_14
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	testq	%r8, %rcx
	jne	.LBB12_7
# BB#6:                                 # %if.then4
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%rdx
	movq	$0, 32(%rax)
	movl	%r9d, 40(%rax)
	xorq	%rcx, %rcx
	jmp	.LBB12_17
.LBB12_14:                              # %land.lhs.true41
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	40(%rax), %esi
	cmpq	%r9, %rsi
	je	.LBB12_17
	.align	16, 0x90
.LBB12_16:                              # %if.then62
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%rcx
.LBB12_17:                              # %for.inc
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB12_2
# BB#18:                                # %for.end
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_20
# BB#19:                                # %for.end
	testq	%r8, %rdx
	je	.LBB12_20
# BB#21:                                # %if.then72
	movq	sched_dump(%rip), %rdi
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	popq	%rcx
	jmp	fprintf  # TAILCALL
.LBB12_20:                              # %if.end74
	popq	%rax
	retq
.Ltmp102:
	.size	rm_redundant_line_notes, .Ltmp102-rm_redundant_line_notes
	.cfi_endproc

	.globl	rm_other_notes
	.align	16, 0x90
	.type	rm_other_notes,@function
rm_other_notes:                         # @rm_other_notes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp105:
	.cfi_def_cfa_offset 16
.Ltmp106:
	.cfi_offset %rbx, -16
	cmpq	%rsi, %rdi
	movq	$0, note_list(%rip)
	jne	.LBB13_2
# BB#1:                                 # %land.lhs.true
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_7
.LBB13_2:                               # %if.end
	movq	24(%rsi), %r10
	cmpq	%rdi, %r10
	je	.LBB13_7
# BB#3:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rdi, %r9
	.align	16, 0x90
.LBB13_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_20 Depth 2
                                        #     Child Loop BB13_9 Depth 2
	movl	(%r9), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$37, %rax
	jne	.LBB13_5
# BB#8:                                 # %land.rhs.lr.ph.i
                                        #   in Loop: Header=BB13_4 Depth=1
	movq	16(%r9), %r11
	testq	%r11, %r11
	movq	%r9, %rax
	jne	.LBB13_20
	jmp	.LBB13_9
	.align	16, 0x90
.LBB13_18:                              # %while.cond.backedge.us.i.land.rhs.us.i_crit_edge
                                        #   in Loop: Header=BB13_9 Depth=2
	movl	(%rdx), %ecx
	movq	%rdx, %rax
.LBB13_9:                               # %land.rhs.us.i
                                        #   Parent Loop BB13_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB13_27
# BB#10:                                # %if.end.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	24(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB13_12
# BB#11:                                # %if.then9.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	%r11, 16(%rdx)
.LBB13_12:                              # %if.end13.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movl	40(%rax), %ecx
	addq	$96, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB13_14
# BB#13:                                # %if.end13.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$27651, %rbx            # imm = 0x6C03
	jne	.LBB13_17
	.align	16, 0x90
.LBB13_14:                              # %if.then41.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	note_list(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	note_list(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB13_16
# BB#15:                                # %if.then46.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	%rax, 24(%rcx)
.LBB13_16:                              # %if.end50.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	%rax, note_list(%rip)
.LBB13_17:                              # %while.cond.backedge.us.i
                                        #   in Loop: Header=BB13_9 Depth=2
	cmpq	%r10, %rdx
	jne	.LBB13_18
	jmp	.LBB13_26
	.align	16, 0x90
.LBB13_19:                              # %while.cond.backedge.i.land.rhs.i_crit_edge
                                        #   in Loop: Header=BB13_20 Depth=2
	movl	(%rdx), %ecx
	movq	%rdx, %rax
.LBB13_20:                              # %land.rhs.i
                                        #   Parent Loop BB13_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	jne	.LBB13_27
# BB#21:                                # %if.end.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movq	24(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rdx, 24(%r11)
	je	.LBB13_23
# BB#22:                                # %if.then9.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movq	%r11, 16(%rdx)
.LBB13_23:                              # %if.end13.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movl	40(%rax), %ecx
	addq	$96, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB13_30
# BB#24:                                # %if.end13.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$27651, %rbx            # imm = 0x6C03
	jne	.LBB13_25
	.align	16, 0x90
.LBB13_30:                              # %if.then41.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movq	note_list(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	note_list(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB13_32
# BB#31:                                # %if.then46.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movq	%rax, 24(%rcx)
.LBB13_32:                              # %if.end50.i
                                        #   in Loop: Header=BB13_20 Depth=2
	movq	%rax, note_list(%rip)
.LBB13_25:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB13_20 Depth=2
	cmpq	%r10, %rdx
	jne	.LBB13_19
.LBB13_26:                              #   in Loop: Header=BB13_4 Depth=1
	movq	%r10, %rax
.LBB13_27:                              # %unlink_other_notes.exit
                                        #   in Loop: Header=BB13_4 Depth=1
	cmpq	%rsi, %r9
	je	.LBB13_28
# BB#33:                                # %if.end14
                                        #   in Loop: Header=BB13_4 Depth=1
	cmpq	%rdi, %r9
	je	.LBB13_34
# BB#35:                                # %if.end18
                                        #   in Loop: Header=BB13_4 Depth=1
	cmpq	%r10, %rax
	jne	.LBB13_6
	jmp	.LBB13_36
	.align	16, 0x90
.LBB13_5:                               #   in Loop: Header=BB13_4 Depth=1
	movq	%r9, %rax
.LBB13_6:                               # %for.inc
                                        #   in Loop: Header=BB13_4 Depth=1
	movq	24(%rax), %r9
	cmpq	%r10, %r9
	jne	.LBB13_4
.LBB13_7:                               # %for.end
	popq	%rbx
	retq
.LBB13_28:                              # %if.then13
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.rm_other_notes, %rcx
	movq	$1371, %rsi             # imm = 0x55B
	jmp	.LBB13_29
.LBB13_34:                              # %if.then17
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.rm_other_notes, %rcx
	movq	$1373, %rsi             # imm = 0x55D
	jmp	.LBB13_29
.LBB13_36:                              # %if.then21
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.rm_other_notes, %rcx
	movq	$1375, %rsi             # imm = 0x55F
.LBB13_29:                              # %if.then13
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp107:
	.size	rm_other_notes, .Ltmp107-rm_other_notes
	.cfi_endproc

	.globl	sched_emit_insn
	.align	16, 0x90
	.type	sched_emit_insn,@function
sched_emit_insn:                        # @sched_emit_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp109:
	.cfi_def_cfa_offset 16
	movq	last_scheduled_insn(%rip), %rsi
	callq	emit_insn_after
	movq	%rax, last_scheduled_insn(%rip)
	popq	%rdx
	retq
.Ltmp110:
	.size	sched_emit_insn, .Ltmp110-sched_emit_insn
	.cfi_endproc

	.globl	schedule_block
	.align	16, 0x90
	.type	schedule_block,@function
schedule_block:                         # @schedule_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp121:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp122:
	.cfi_def_cfa_offset 144
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
	movq	current_sched_info(%rip), %rax
	movq	64(%rax), %r13
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	72(%rax), %rax
	movq	24(%r13), %rbx
	movq	16(%rax), %r12
	movq	%rsi, %r15
	movq	%rdi, %r14
	cmpq	%r12, %rbx
	jne	.LBB15_4
# BB#1:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB15_2
.LBB15_4:                               # %if.end
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_9
# BB#5:                                 # %if.then9
	movq	sched_dump(%rip), %rcx
	movabsq	$.L.str5, %rdi
	movq	$60, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	sched_dump(%rip), %rdi
	movl	8(%rbx), %ecx
	movl	reload_completed(%rip), %eax
	movl	8(%r12), %r8d
	testq	%rax, %rax
	je	.LBB15_7
# BB#6:
	movabsq	$.L.str7, %r9
	jmp	.LBB15_8
.LBB15_7:                               # %select.mid
	movabsq	$.L.str8, %r9
.LBB15_8:                               # %select.end
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movq	sched_dump(%rip), %rcx
	movabsq	$.L.str5, %rdi
	movq	$60, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	sched_dump(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	callq	visualize_alloc
	callq	init_block_visualization
.LBB15_9:                               # %if.end19
	movabsq	$unit_last_insn, %rdi
	xorq	%rsi, %rsi
	movq	$624, %rdx              # imm = 0x270
	xorq	%rbx, %rbx
	callq	memset
	movabsq	$unit_tick, %rdi
	xorq	%rsi, %rsi
	movq	$312, %rdx              # imm = 0x138
	callq	memset
	movl	issue_rate(%rip), %eax
	leaq	1(%rax,%r15), %rdi
	movl	%edi, 72(%rsp)
	addq	%r15, %rax
	movl	%eax, 76(%rsp)
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	current_sched_info(%rip), %rcx
	movq	%rax, 64(%rsp)
	movl	%ebx, 80(%rsp)
	leaq	64(%rsp), %rdi
	callq	*(%rcx)
	movq	targetm+200(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_11
# BB#10:                                # %if.then26
	movq	sched_dump(%rip), %rdi
	movl	72(%rsp), %edx
	movl	sched_verbose(%rip), %esi
	callq	*%rax
.LBB15_11:                              # %if.end28
	movq	%r13, last_scheduled_insn(%rip)
	movl	%ebx, q_ptr(%rip)
	movl	%ebx, q_size(%rip)
	movl	%ebx, last_clock_var(%rip)
	movabsq	$insn_queue, %rdi
	xorq	%rsi, %rsi
	movq	$1024, %rdx             # imm = 0x400
	callq	memset
	movq	current_sched_info(%rip), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movl	%ebx, clock_var(%rip)
	callq	*16(%rax)
	testq	%rbx, %rax
	je	.LBB15_85
	.align	16, 0x90
.LBB15_12:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_13 Depth 2
                                        #     Child Loop BB15_23 Depth 2
                                        #       Child Loop BB15_24 Depth 3
                                        #     Child Loop BB15_43 Depth 2
                                        #     Child Loop BB15_64 Depth 2
                                        #     Child Loop BB15_66 Depth 2
                                        #       Child Loop BB15_68 Depth 3
                                        #       Child Loop BB15_100 Depth 3
                                        #         Child Loop BB15_101 Depth 4
                                        #       Child Loop BB15_112 Depth 3
                                        #       Child Loop BB15_138 Depth 3
	movl	q_ptr(%rip), %eax
	movl	clock_var(%rip), %ecx
	incq	%rax
	andq	$127, %rax
	movq	insn_queue(,%rax,8), %rbx
	incq	%rcx
	movl	%ecx, clock_var(%rip)
	movl	%eax, q_ptr(%rip)
	testq	%rbx, %rbx
	je	.LBB15_22
	.align	16, 0x90
.LBB15_13:                              # %for.body.i206
                                        #   Parent Loop BB15_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	q_size(%rip), %eax
	movslq	sched_verbose(%rip), %rcx
	movq	8(%rbx), %r14
	decq	%rax
	movl	%eax, q_size(%rip)
	cmpq	$2, %rcx
	jl	.LBB15_15
# BB#14:                                # %if.then.i
                                        #   in Loop: Header=BB15_13 Depth=2
	movq	sched_dump(%rip), %r15
	movq	current_sched_info(%rip), %rax
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str23, %rsi
	movq	%rcx, %rdx
	callq	fprintf
.LBB15_15:                              # %if.end.i
                                        #   in Loop: Header=BB15_13 Depth=2
	movl	76(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movl	80(%rsp), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rax
	jne	.LBB15_18
# BB#16:                                # %if.then.i.i
                                        #   in Loop: Header=BB15_13 Depth=2
	testq	%rax, %rax
	je	.LBB15_183
# BB#17:                                # %ready_lastpos.exit.i.i
                                        #   in Loop: Header=BB15_13 Depth=2
	movq	64(%rsp), %rsi
	movslq	72(%rsp), %rax
	subq	%rdx, %rax
	leaq	(%rsi,%rax,8), %rdi
	addq	$8, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memmove
	movl	72(%rsp), %eax
	decq	%rax
	movl	%eax, 76(%rsp)
	movl	80(%rsp), %esi
.LBB15_18:                              # %ready_add.exit.i
                                        #   in Loop: Header=BB15_13 Depth=2
	subq	%rsi, %rax
	movq	$32, %rcx
	movq	64(%rsp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r14, (%rdx,%rax,8)
	movslq	sched_verbose(%rip), %rax
	incq	%rsi
	movl	%esi, 80(%rsp)
	cmpq	$2, %rax
	jl	.LBB15_20
# BB#19:                                # %if.then4.i
                                        #   in Loop: Header=BB15_13 Depth=2
	movq	sched_dump(%rip), %rcx
	movq	$31, %rsi
	movq	$1, %rdx
	movabsq	$.L.str24, %rdi
	callq	fwrite
.LBB15_20:                              # %for.inc.i
                                        #   in Loop: Header=BB15_13 Depth=2
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_13
# BB#21:                                # %for.cond.for.end_crit_edge.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movl	q_ptr(%rip), %eax
.LBB15_22:                              # %for.end.i211
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	80(%rsp), %esi
	sarq	%cl, %rdx
	movq	$0, insn_queue(,%rdx,8)
	testq	%rsi, %rsi
	movq	$1, %r14
	jne	.LBB15_39
	.align	16, 0x90
.LBB15_23:                              # %for.body16.i
                                        #   Parent Loop BB15_12 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB15_24 Depth 3
	leaq	(%r14,%rax), %rcx
	andq	$127, %rcx
	movq	insn_queue(,%rcx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB15_33
	.align	16, 0x90
.LBB15_24:                              # %for.body25.i
                                        #   Parent Loop BB15_12 Depth=1
                                        #     Parent Loop BB15_23 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	q_size(%rip), %eax
	movslq	sched_verbose(%rip), %rcx
	movq	8(%rbx), %r15
	decq	%rax
	movl	%eax, q_size(%rip)
	cmpq	$2, %rcx
	jl	.LBB15_26
# BB#25:                                # %if.then31.i
                                        #   in Loop: Header=BB15_24 Depth=3
	movq	sched_dump(%rip), %r12
	movq	current_sched_info(%rip), %rax
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str23, %rsi
	movq	%rcx, %rdx
	callq	fprintf
.LBB15_26:                              # %if.end35.i
                                        #   in Loop: Header=BB15_24 Depth=3
	movl	76(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movl	80(%rsp), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rax
	jne	.LBB15_29
# BB#27:                                # %if.then.i93.i
                                        #   in Loop: Header=BB15_24 Depth=3
	testq	%rax, %rax
	je	.LBB15_183
# BB#28:                                # %ready_lastpos.exit.i102.i
                                        #   in Loop: Header=BB15_24 Depth=3
	movq	64(%rsp), %rsi
	movslq	72(%rsp), %rax
	subq	%rdx, %rax
	leaq	(%rsi,%rax,8), %rdi
	addq	$8, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memmove
	movl	72(%rsp), %eax
	decq	%rax
	movl	%eax, 76(%rsp)
	movl	80(%rsp), %esi
.LBB15_29:                              # %ready_add.exit107.i
                                        #   in Loop: Header=BB15_24 Depth=3
	subq	%rsi, %rax
	movq	$32, %rcx
	movq	64(%rsp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r15, (%rdx,%rax,8)
	movslq	sched_verbose(%rip), %rax
	incq	%rsi
	movl	%esi, 80(%rsp)
	cmpq	$2, %rax
	jl	.LBB15_31
# BB#30:                                # %if.then37.i
                                        #   in Loop: Header=BB15_24 Depth=3
	movq	sched_dump(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str25, %rsi
	movq	%r14, %rdx
	callq	fprintf
.LBB15_31:                              # %for.inc40.i
                                        #   in Loop: Header=BB15_24 Depth=3
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_24
# BB#32:                                # %for.end44.i
                                        #   in Loop: Header=BB15_23 Depth=2
	movl	q_ptr(%rip), %eax
	movl	80(%rsp), %esi
	leaq	(%rax,%r14), %rcx
	andq	$127, %rcx
	testq	%rsi, %rsi
	movq	$0, insn_queue(,%rcx,8)
	jne	.LBB15_34
.LBB15_33:                              # %for.inc54.i
                                        #   in Loop: Header=BB15_23 Depth=2
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rsi, %rsi
	cmpq	$128, %rdx
	jl	.LBB15_23
.LBB15_34:                              # %for.end55.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movl	sched_verbose(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB15_37
# BB#35:                                # %for.end55.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB15_37
# BB#36:                                # %if.then58.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	%r14, %rdi
	callq	visualize_stall_cycles
	movl	q_ptr(%rip), %eax
	movl	80(%rsp), %esi
.LBB15_37:                              # %queue_to_ready.exit
                                        #   in Loop: Header=BB15_12 Depth=1
	addq	%r14, %rax
	movl	clock_var(%rip), %ecx
	andq	$127, %rax
	movl	%eax, q_ptr(%rip)
	addq	%r14, %rcx
	movl	%ecx, clock_var(%rip)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB15_38
.LBB15_39:                              # %if.end35
                                        #   in Loop: Header=BB15_12 Depth=1
	movslq	sched_verbose(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB15_45
# BB#40:                                # %if.then38
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	sched_dump(%rip), %rcx
	movq	$38, %rsi
	movq	$1, %rdx
	movabsq	$.L.str10, %rdi
	callq	fwrite
	movl	80(%rsp), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	je	.LBB15_46
# BB#41:                                # %ready_lastpos.exit.i221
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	sched_dump(%rip), %r15
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB15_44
# BB#42:                                # %for.body.i230.preheader
                                        #   in Loop: Header=BB15_12 Depth=1
	movslq	76(%rsp), %rcx
	movq	64(%rsp), %rdx
	subq	%rax, %rcx
	leaq	(%rdx,%rcx,8), %r14
	movq	$1, %rbx
	movabsq	$.L.str22, %r12
	.align	16, 0x90
.LBB15_43:                              # %for.body.i230
                                        #   Parent Loop BB15_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	current_sched_info(%rip), %rax
	movq	(%r14,%rbx,8), %rdi
	xorq	%rsi, %rsi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movslq	80(%rsp), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	sched_dump(%rip), %r15
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB15_43
.LBB15_44:                              # %for.end.i233
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	movl	80(%rsp), %esi
.LBB15_45:                              # %if.end40
                                        #   in Loop: Header=BB15_12 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB15_46
# BB#47:                                # %ready_lastpos.exit.i242
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	64(%rsp), %r14
	movslq	76(%rsp), %rax
	movq	%rsi, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rbx
	subq	%rdi, %rbx
	addq	%rax, %rbx
	cmpq	$2, %rdx
	leaq	(%r14,%rbx,8), %rdi
	jne	.LBB15_51
# BB#48:                                # %land.rhs.i.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	8(%r14,%rbx,8), %r15
	movq	%r15, 88(%rsp)
	leaq	88(%rsp), %rsi
	callq	rank_for_schedule
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rax
	js	.LBB15_50
# BB#49:                                # %swap_sort.exit.loopexit.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	(%r14,%rbx,8), %rax
	movq	%rax, 8(%r14,%rbx,8)
	xorq	%rax, %rax
.LBB15_50:                              # %swap_sort.exit.i
                                        #   in Loop: Header=BB15_12 Depth=1
	addq	%rbx, %rax
	movq	%r15, (%r14,%rax,8)
	jmp	.LBB15_53
	.align	16, 0x90
.LBB15_51:                              # %if.else.i
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB15_53
# BB#52:                                # %if.then4.i246
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$8, %rdx
	movabsq	$rank_for_schedule, %rcx
	callq	specqsort
.LBB15_53:                              # %ready_sort.exit
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	targetm+216(%rip), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_54
# BB#56:                                # %if.else
                                        #   in Loop: Header=BB15_12 Depth=1
	movl	issue_rate(%rip), %eax
	jmp	.LBB15_57
	.align	16, 0x90
.LBB15_54:                              # %if.then42
                                        #   in Loop: Header=BB15_12 Depth=1
	movslq	80(%rsp), %rax
	testq	%rax, %rax
	je	.LBB15_46
# BB#55:                                # %ready_lastpos.exit
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	sched_dump(%rip), %rdi
	movq	64(%rsp), %r9
	movslq	76(%rsp), %rdx
	movq	$1, %rcx
	movl	clock_var(%rip), %r8d
	movl	sched_verbose(%rip), %esi
	subq	%rax, %rcx
	addq	%rdx, %rcx
	leaq	(%r9,%rcx,8), %rdx
	leaq	80(%rsp), %rcx
	callq	*%rbx
.LBB15_57:                              # %if.end46
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movl	sched_verbose(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB15_60
# BB#58:                                # %if.end46
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	targetm+232(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_60
# BB#59:                                # %if.then50
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	last_scheduled_insn(%rip), %rsi
	movl	clock_var(%rip), %edi
	callq	*%rax
	movl	sched_verbose(%rip), %ecx
	movq	%rax, last_scheduled_insn(%rip)
.LBB15_60:                              # %if.end52
                                        #   in Loop: Header=BB15_12 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB15_66
# BB#61:                                # %if.then54
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	sched_dump(%rip), %rdi
	movl	clock_var(%rip), %edx
	xorq	%rax, %rax
	movabsq	$.L.str11, %rsi
	callq	fprintf
	movl	80(%rsp), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	je	.LBB15_66
# BB#62:                                # %ready_lastpos.exit.i260
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	sched_dump(%rip), %r15
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB15_65
# BB#63:                                # %for.body.i269.preheader
                                        #   in Loop: Header=BB15_12 Depth=1
	movslq	76(%rsp), %rcx
	movq	64(%rsp), %rdx
	subq	%rax, %rcx
	leaq	(%rdx,%rcx,8), %r14
	movq	$1, %rbx
	movabsq	$.L.str22, %r12
	.align	16, 0x90
.LBB15_64:                              # %for.body.i269
                                        #   Parent Loop BB15_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	current_sched_info(%rip), %rax
	movq	(%r14,%rbx,8), %rdi
	xorq	%rsi, %rsi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movslq	80(%rsp), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	sched_dump(%rip), %r15
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB15_64
.LBB15_65:                              # %for.end.i272
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	fputc
	jmp	.LBB15_66
.LBB15_175:                             # %cond.end
                                        #   in Loop: Header=BB15_66 Depth=2
	movl	clock_var(%rip), %r8d
	movq	%r9, %rsi
	leaq	80(%rsp), %rcx
	callq	*%rbx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB15_66:                              # %while.cond57.preheader
                                        #   Parent Loop BB15_12 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB15_68 Depth 3
                                        #       Child Loop BB15_100 Depth 3
                                        #         Child Loop BB15_101 Depth 4
                                        #       Child Loop BB15_112 Depth 3
                                        #       Child Loop BB15_138 Depth 3
	movl	80(%rsp), %eax
	testq	%rax, %rax
	je	.LBB15_82
# BB#67:                                # %while.cond57.preheader
                                        #   in Loop: Header=BB15_66 Depth=2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB15_82
	.align	16, 0x90
.LBB15_68:                              # %land.rhs
                                        #   Parent Loop BB15_12 Depth=1
                                        #     Parent Loop BB15_66 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	current_sched_info(%rip), %rax
	callq	*16(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_82
# BB#69:                                # %while.body66
                                        #   in Loop: Header=BB15_68 Depth=3
	movl	80(%rsp), %eax
	testq	%rax, %rax
	je	.LBB15_184
# BB#70:                                # %if.end.i280
                                        #   in Loop: Header=BB15_68 Depth=3
	movslq	76(%rsp), %rcx
	leaq	-1(%rcx), %rdx
	movl	%edx, 76(%rsp)
	movq	64(%rsp), %rdx
	movq	(%rdx,%rcx,8), %r12
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movl	%eax, 80(%rsp)
	jne	.LBB15_72
# BB#71:                                # %if.then5.i
                                        #   in Loop: Header=BB15_68 Depth=3
	movl	72(%rsp), %eax
	decq	%rax
	movl	%eax, 76(%rsp)
.LBB15_72:                              # %ready_remove_first.exit
                                        #   in Loop: Header=BB15_68 Depth=3
	movslq	8(%r12), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	42(%rdx,%rax), %eax
	movq	$48, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$48, %rcx
	sarq	%cl, %rdi
	testq	%rax, %rax
	jne	.LBB15_77
# BB#73:                                # %if.then.i285
                                        #   in Loop: Header=BB15_68 Depth=3
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB15_75
# BB#74:                                # %cond.end.i
                                        #   in Loop: Header=BB15_68 Depth=3
	movq	%r12, %rdi
	callq	recog_memoized_1
	movslq	40(%r12), %rax
	testq	%rax, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	js	.LBB15_77
.LBB15_75:                              # %if.end21.i
                                        #   in Loop: Header=BB15_68 Depth=3
	movq	%r12, %rdi
	callq	function_units_used
	movq	%rax, %rdi
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	andq	%rcx, %rdi
	movq	$31, %rcx
	shrq	%cl, %rdi
	xorq	$1, %rdi
	addq	%rax, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_77
# BB#76:                                # %if.then26.i
                                        #   in Loop: Header=BB15_68 Depth=3
	movslq	8(%r12), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movw	%di, 42(%rdx,%rax)
	.align	16, 0x90
.LBB15_77:                              # %insn_unit.exit
                                        #   in Loop: Header=BB15_68 Depth=3
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jle	.LBB15_79
# BB#78:                                # %insn_unit.exit
                                        #   in Loop: Header=BB15_68 Depth=3
	movabsq	$1, %rax
.LBB15_79:                              # %insn_unit.exit
                                        #   in Loop: Header=BB15_68 Depth=3
	movl	clock_var(%rip), %edx
	subq	%rax, %rdi
	xorq	%rcx, %rcx
	movq	%r12, %rsi
	callq	actual_hazard
	movq	%rax, %r14
	movq	$32, %rcx
                                        # kill: RAX<def> R14<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_98
# BB#80:                                # %if.then72
                                        #   in Loop: Header=BB15_68 Depth=3
	movl	q_ptr(%rip), %ebx
	addq	%r14, %rbx
	andq	$127, %rbx
	movq	insn_queue(,%rbx,8), %rsi
	movq	%r12, %rdi
	callq	alloc_INSN_LIST
	movl	q_size(%rip), %ecx
	movslq	sched_verbose(%rip), %rdx
	movq	%rax, insn_queue(,%rbx,8)
	incq	%rcx
	movl	%ecx, q_size(%rip)
	cmpq	$1, %rdx
	jle	.LBB15_81
# BB#97:                                # %if.then.i298
                                        #   in Loop: Header=BB15_68 Depth=3
	movq	sched_dump(%rip), %r15
	movq	current_sched_info(%rip), %rax
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str20, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movq	sched_dump(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str21, %rsi
	movq	%r14, %rdx
	callq	fprintf
.LBB15_81:                              # %while.cond57.backedge
                                        #   in Loop: Header=BB15_68 Depth=3
	movl	80(%rsp), %eax
	testq	%rax, %rax
	jne	.LBB15_68
	jmp	.LBB15_82
.LBB15_98:                              # %if.end73
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	current_sched_info(%rip), %rax
	movq	%r12, %rdi
	callq	*8(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_164
# BB#99:                                # %if.end77
                                        #   in Loop: Header=BB15_66 Depth=2
	movzbl	3(%r12), %eax
	movq	last_scheduled_insn(%rip), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	testq	$16, %rax
	movq	%r12, 40(%rsp)          # 8-byte Spill
	je	.LBB15_111
	.align	16, 0x90
.LBB15_100:                             # %while.body.i
                                        #   Parent Loop BB15_12 Depth=1
                                        #     Parent Loop BB15_66 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB15_101 Depth 4
	movq	16(%r12), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	24(%r12), %rax
	movq	%rax, 24(%rcx)
	movq	16(%r12), %rax
	movq	24(%r12), %rcx
	movq	%rax, 16(%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	24(%rcx), %rax
	movq	%rax, 24(%r12)
	movq	24(%rcx), %rax
	movq	%r12, 16(%rax)
	movq	%r12, 24(%rcx)
	movq	%rcx, 16(%r12)
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r13
	je	.LBB15_108
	.align	16, 0x90
.LBB15_101:                             # %for.body.i31.i
                                        #   Parent Loop BB15_12 Depth=1
                                        #     Parent Loop BB15_66 Depth=2
                                        #       Parent Loop BB15_100 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$1572864, %rax          # imm = 0x180000
	jne	.LBB15_107
# BB#102:                               # %if.then.i40.i
                                        #   in Loop: Header=BB15_101 Depth=4
	movq	8(%rbx), %rax
	movq	8(%rax), %r15
	leaq	83(%r15), %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	emit_note_before
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	remove_note
	movq	16(%rbx), %rbx
	cmpq	$1, %r14
	ja	.LBB15_104
# BB#103:                               # %if.then11.i44.i
                                        #   in Loop: Header=BB15_101 Depth=4
	movq	8(%rbx), %rax
	movq	%rax, 32(%r13)
	jmp	.LBB15_106
	.align	16, 0x90
.LBB15_104:                             # %if.else.i45.i
                                        #   in Loop: Header=BB15_101 Depth=4
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %r15
	movabsq	$4294967210, %rax       # imm = 0xFFFFFFAA
	cmpq	%rax, %r15
	jne	.LBB15_106
# BB#105:                               # %if.then30.i52.i
                                        #   in Loop: Header=BB15_101 Depth=4
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movl	%eax, 32(%r13)
.LBB15_106:                             # %if.end40.i53.i
                                        #   in Loop: Header=BB15_101 Depth=4
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	remove_note
.LBB15_107:                             # %for.inc.i60.i
                                        #   in Loop: Header=BB15_101 Depth=4
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_101
.LBB15_108:                             # %reemit_notes.exit61.i
                                        #   in Loop: Header=BB15_100 Depth=3
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB15_110
# BB#109:                               # %reemit_notes.exit61.i
                                        #   in Loop: Header=BB15_100 Depth=3
	movq	%rax, %r12
.LBB15_110:                             # %reemit_notes.exit61.i
                                        #   in Loop: Header=BB15_100 Depth=3
	movq	48(%rsp), %rcx          # 8-byte Reload
	movzbl	3(%rcx), %eax
	movq	%r12, 56(%rsp)          # 8-byte Spill
	testq	$16, %rax
	movq	%rcx, %r12
	jne	.LBB15_100
.LBB15_111:                             # %while.end.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	16(%r12), %rax
	movq	24(%r12), %rcx
	movq	%rcx, 24(%rax)
	movq	16(%r12), %rax
	movq	24(%r12), %rcx
	movq	%rax, 16(%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	24(%rcx), %rax
	movq	%rax, 24(%r12)
	movq	24(%rcx), %rax
	movq	%r12, 16(%rax)
	movq	%r12, 24(%rcx)
	movq	%rcx, 16(%r12)
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r13
	je	.LBB15_119
	.align	16, 0x90
.LBB15_112:                             # %for.body.i.i
                                        #   Parent Loop BB15_12 Depth=1
                                        #     Parent Loop BB15_66 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$1572864, %rax          # imm = 0x180000
	jne	.LBB15_118
# BB#113:                               # %if.then.i.i308
                                        #   in Loop: Header=BB15_112 Depth=3
	movq	8(%rbx), %rax
	movq	8(%rax), %r15
	leaq	83(%r15), %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	emit_note_before
	movq	%rax, %r13
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	remove_note
	movq	16(%rbx), %rbx
	cmpq	$1, %r14
	ja	.LBB15_115
# BB#114:                               # %if.then11.i.i
                                        #   in Loop: Header=BB15_112 Depth=3
	movq	8(%rbx), %rax
	movq	%rax, 32(%r13)
	jmp	.LBB15_117
	.align	16, 0x90
.LBB15_115:                             # %if.else.i.i
                                        #   in Loop: Header=BB15_112 Depth=3
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %r15
	movabsq	$4294967210, %rax       # imm = 0xFFFFFFAA
	cmpq	%rax, %r15
	jne	.LBB15_117
# BB#116:                               # %if.then30.i.i
                                        #   in Loop: Header=BB15_112 Depth=3
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movl	%eax, 32(%r13)
.LBB15_117:                             # %if.end40.i.i
                                        #   in Loop: Header=BB15_112 Depth=3
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	remove_note
.LBB15_118:                             # %for.inc.i.i
                                        #   in Loop: Header=BB15_112 Depth=3
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_112
.LBB15_119:                             # %move_insn.exit
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	je	.LBB15_121
# BB#120:                               # %move_insn.exit
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	%rax, %r12
.LBB15_121:                             # %move_insn.exit
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	targetm+192(%rip), %rax
	movq	%r12, last_scheduled_insn(%rip)
	testq	%rax, %rax
	jne	.LBB15_122
# BB#123:                               # %if.else82
                                        #   in Loop: Header=BB15_66 Depth=2
	decq	32(%rsp)                # 8-byte Folded Spill
	movq	40(%rsp), %r15          # 8-byte Reload
	jmp	.LBB15_124
.LBB15_122:                             # %if.then80
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	sched_dump(%rip), %rdi
	movl	sched_verbose(%rip), %esi
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	callq	*%rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB15_124:                             # %if.end83
                                        #   in Loop: Header=BB15_66 Depth=2
	movslq	8(%r15), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movzbl	42(%rdx,%rax), %eax
	movl	clock_var(%rip), %r12d
	movq	$48, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	testq	%rax, %rax
	jne	.LBB15_129
# BB#125:                               # %if.then.i.i313
                                        #   in Loop: Header=BB15_66 Depth=2
	movslq	40(%r15), %rax
	testq	%rax, %rax
	jns	.LBB15_127
# BB#126:                               # %cond.end.i.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	%r15, %rdi
	callq	recog_memoized_1
	movslq	40(%r15), %rax
	testq	%rax, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	js	.LBB15_129
.LBB15_127:                             # %if.end21.i.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	%r15, %rdi
	callq	function_units_used
	movq	%rax, %rbx
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	andq	%rcx, %rbx
	movq	$31, %rcx
	shrq	%cl, %rbx
	xorq	$1, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_129
# BB#128:                               # %if.then26.i.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movslq	8(%r15), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movw	%bx, 42(%rdx,%rax)
.LBB15_129:                             # %insn_unit.exit.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r14
	jle	.LBB15_131
# BB#130:                               # %insn_unit.exit.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movabsq	$1, %r14
.LBB15_131:                             # %insn_unit.exit.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movslq	sched_verbose(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB15_133
# BB#132:                               # %if.then.i321
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	sched_dump(%rip), %rdi
	movl	8(%r15), %edx
	xorq	%rax, %rax
	movabsq	$.L.str16, %rsi
	callq	fprintf
	movq	%r15, %rdi
	callq	insn_print_units
	movq	sched_dump(%rip), %rsi
	movq	$10, %rdi
	callq	fputc
	movl	sched_verbose(%rip), %eax
.LBB15_133:                             # %if.end.i325
                                        #   in Loop: Header=BB15_66 Depth=2
	subq	%r14, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_136
# BB#134:                               # %if.end.i325
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB15_136
# BB#135:                               # %if.then4.i326
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	%r15, %rdi
	callq	visualize_no_unit
.LBB15_136:                             # %if.end5.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rbx
	movq	%rbx, %rdx
	callq	schedule_unit
	movslq	8(%r15), %rax
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%rbx, %r15
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %r12
	testq	%r12, %r12
	jne	.LBB15_138
# BB#137:                               #   in Loop: Header=BB15_66 Depth=2
	movq	16(%rsp), %r13          # 8-byte Reload
	jmp	.LBB15_164
	.align	16, 0x90
.LBB15_138:                             # %for.body.i336
                                        #   Parent Loop BB15_12 Depth=1
                                        #     Parent Loop BB15_66 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r12), %r13
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	insn_cost
	movslq	8(%r13), %rcx
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movslq	36(%rdx,%rsi), %rdi
	addq	%r15, %rax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdi
	jge	.LBB15_140
# BB#139:                               # %for.body.i336
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	%rax, %rdi
.LBB15_140:                             # %for.body.i336
                                        #   in Loop: Header=BB15_138 Depth=3
	movl	%edi, 36(%rdx,%rsi)
	movslq	8(%r13), %rax
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	24(%rdx,%rax), %ecx
	decq	%rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rcx
	movl	%ecx, 24(%rdx,%rax)
	jne	.LBB15_156
# BB#141:                               # %if.then48.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movslq	8(%r13), %rax
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	36(%rdx,%rax), %r14d
	movq	current_sched_info(%rip), %rax
	movq	%r13, %rdi
	callq	*24(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_156
# BB#142:                               # %if.end59.i338
                                        #   in Loop: Header=BB15_138 Depth=3
	movslq	sched_verbose(%rip), %rax
	subq	%r15, %r14
	cmpq	$2, %rax
	jl	.LBB15_147
# BB#143:                               # %if.then61.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	%r15, %rbx
	movq	sched_dump(%rip), %r15
	movq	current_sched_info(%rip), %rax
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str17, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	sched_dump(%rip), %r8
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB15_145
# BB#144:                               # %if.then65.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	$11, %rsi
	movq	$1, %rdx
	movabsq	$.L.str18, %rdi
	movq	%r8, %rcx
	callq	fwrite
	jmp	.LBB15_146
.LBB15_145:                             # %if.else.i340
                                        #   in Loop: Header=BB15_138 Depth=3
	xorq	%rax, %rax
	movq	%r8, %rdi
	movabsq	$.L.str19, %rsi
	movq	%r14, %rdx
	callq	fprintf
.LBB15_146:                             # %if.end69.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	%rbx, %r15
.LBB15_147:                             # %if.end69.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	targetm+176(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_149
# BB#148:                               # %if.then.i120.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movslq	8(%r13), %rcx
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	20(%rdx,%rsi), %esi
	movq	%r13, %rdi
	callq	*%rax
	movslq	8(%r13), %rcx
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	%eax, 20(%rdx,%rsi)
.LBB15_149:                             # %adjust_priority.exit.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB15_154
# BB#150:                               # %if.then71.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movl	76(%rsp), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	movl	80(%rsp), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rax
	jne	.LBB15_153
# BB#151:                               # %if.then.i122.i
                                        #   in Loop: Header=BB15_138 Depth=3
	testq	%rax, %rax
	je	.LBB15_46
# BB#152:                               # %ready_lastpos.exit.i.i351
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	64(%rsp), %rsi
	movslq	72(%rsp), %rax
	subq	%rdx, %rax
	leaq	(%rsi,%rax,8), %rdi
	addq	$8, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memmove
	movl	72(%rsp), %eax
	decq	%rax
	movl	%eax, 76(%rsp)
	movl	80(%rsp), %esi
.LBB15_153:                             # %ready_add.exit.i354
                                        #   in Loop: Header=BB15_138 Depth=3
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	64(%rsp), %rdx
	sarq	%cl, %rax
	movq	%r13, (%rdx,%rax,8)
	incq	%rsi
	movl	%esi, 80(%rsp)
	jmp	.LBB15_156
.LBB15_154:                             # %if.else72.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movl	q_ptr(%rip), %ebx
	addq	%r14, %rbx
	andq	$127, %rbx
	movq	insn_queue(,%rbx,8), %rsi
	movq	%r13, %rdi
	callq	alloc_INSN_LIST
	movl	q_size(%rip), %ecx
	movslq	sched_verbose(%rip), %rdx
	movq	%rax, insn_queue(,%rbx,8)
	incq	%rcx
	movl	%ecx, q_size(%rip)
	cmpq	$2, %rdx
	jl	.LBB15_156
# BB#155:                               # %if.then.i129.i
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	%r15, %rbx
	movq	sched_dump(%rip), %r15
	movq	current_sched_info(%rip), %rax
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %r15
	movabsq	$.L.str20, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movq	sched_dump(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str21, %rsi
	movq	%r14, %rdx
	callq	fprintf
	.align	16, 0x90
.LBB15_156:                             # %for.inc.i355
                                        #   in Loop: Header=BB15_138 Depth=3
	movq	16(%r12), %r12
	testq	%r12, %r12
	jne	.LBB15_138
# BB#157:                               # %for.end.i356
                                        #   in Loop: Header=BB15_66 Depth=2
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_158
# BB#159:                               # %for.end.i356
                                        #   in Loop: Header=BB15_66 Depth=2
	movl	issue_rate(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	40(%rsp), %rsi          # 8-byte Reload
	jl	.LBB15_160
# BB#161:                               # %if.then81.i
                                        #   in Loop: Header=BB15_66 Depth=2
	movslq	last_clock_var(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movq	$393216, %rax           # imm = 0x60000
	movq	16(%rsp), %r13          # 8-byte Reload
	jl	.LBB15_163
# BB#162:                               # %select.mid169
                                        #   in Loop: Header=BB15_66 Depth=2
	xorq	%rax, %rax
.LBB15_163:                             # %select.end168
                                        #   in Loop: Header=BB15_66 Depth=2
	movl	(%rsi), %ecx
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, (%rsi)
	movl	%r15d, last_clock_var(%rip)
	jmp	.LBB15_164
.LBB15_158:                             #   in Loop: Header=BB15_66 Depth=2
	movq	16(%rsp), %r13          # 8-byte Reload
	jmp	.LBB15_164
.LBB15_160:                             #   in Loop: Header=BB15_66 Depth=2
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB15_164:                             # %next
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	targetm+224(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB15_66
# BB#165:                               # %if.then85
                                        #   in Loop: Header=BB15_66 Depth=2
	movslq	80(%rsp), %rsi
	testq	%rsi, %rsi
	jle	.LBB15_173
# BB#166:                               # %ready_lastpos.exit.i368
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	64(%rsp), %r14
	movslq	76(%rsp), %rax
	movq	$1, %rbx
	subq	%rsi, %rbx
	addq	%rax, %rbx
	leaq	(%r14,%rbx,8), %rdi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB15_170
# BB#167:                               # %land.rhs.i.i375
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	8(%r14,%rbx,8), %r15
	movq	%r15, 88(%rsp)
	leaq	88(%rsp), %rsi
	callq	rank_for_schedule
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rax
	js	.LBB15_169
# BB#168:                               # %swap_sort.exit.loopexit.i378
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	(%r14,%rbx,8), %rax
	movq	%rax, 8(%r14,%rbx,8)
	xorq	%rax, %rax
.LBB15_169:                             # %swap_sort.exit.i382
                                        #   in Loop: Header=BB15_66 Depth=2
	addq	%rbx, %rax
	movq	%r15, (%r14,%rax,8)
	jmp	.LBB15_172
.LBB15_170:                             # %if.else.i384
                                        #   in Loop: Header=BB15_66 Depth=2
	cmpq	$3, %rsi
	jl	.LBB15_172
# BB#171:                               # %if.then4.i385
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	$8, %rdx
	movabsq	$rank_for_schedule, %rcx
	callq	specqsort
.LBB15_172:                             # %if.end90thread-pre-split
                                        #   in Loop: Header=BB15_66 Depth=2
	movl	80(%rsp), %esi
	movq	targetm+224(%rip), %rbx
.LBB15_173:                             # %if.end90
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	sched_dump(%rip), %rdi
	movl	sched_verbose(%rip), %r9d
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB15_175
# BB#174:                               # %ready_lastpos.exit398
                                        #   in Loop: Header=BB15_66 Depth=2
	movq	64(%rsp), %rdx
	movslq	76(%rsp), %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rcx
	subq	%rsi, %rcx
	addq	%rax, %rcx
	leaq	(%rdx,%rcx,8), %rdx
	jmp	.LBB15_175
	.align	16, 0x90
.LBB15_82:                              # %while.end
                                        #   in Loop: Header=BB15_12 Depth=1
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_84
# BB#83:                                # %if.then99
                                        #   in Loop: Header=BB15_12 Depth=1
	movl	clock_var(%rip), %edi
	callq	visualize_scheduled_insns
.LBB15_84:                              # %while.cond.backedge
                                        #   in Loop: Header=BB15_12 Depth=1
	movq	current_sched_info(%rip), %rax
	callq	*16(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_12
.LBB15_85:                              # %while.end101
	movq	targetm+208(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_87
# BB#86:                                # %if.then103
	movq	sched_dump(%rip), %rdi
	movl	sched_verbose(%rip), %esi
	callq	*%rax
.LBB15_87:                              # %if.end104
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_94
# BB#88:                                # %if.then106
	movq	sched_dump(%rip), %rcx
	movabsq	$.L.str12, %rdi
	movq	$24, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	80(%rsp), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	je	.LBB15_93
# BB#89:                                # %ready_lastpos.exit.i
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	sched_dump(%rip), %r12
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB15_92
# BB#90:                                # %for.body.i.preheader
	movslq	76(%rsp), %rcx
	movq	64(%rsp), %rdx
	subq	%rax, %rcx
	leaq	(%rdx,%rcx,8), %r15
	movq	$1, %rbx
	movabsq	$.L.str22, %r14
	.align	16, 0x90
.LBB15_91:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	current_sched_info(%rip), %rax
	movq	(%r15,%rbx,8), %rdi
	xorq	%rsi, %rsi
	callq	*40(%rax)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	fprintf
	movslq	80(%rsp), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	sched_dump(%rip), %r12
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB15_91
.LBB15_92:                              # %for.end.i
	movq	$10, %rdi
	movq	%r12, %rsi
	callq	fputc
.LBB15_93:                              # %debug_ready_list.exit
	movabsq	$.L.str13, %rdi
	callq	print_block_visualization
.LBB15_94:                              # %if.end108
	movq	current_sched_info(%rip), %rax
	movzbl	96(%rax), %eax
	testq	$1, %rax
	je	.LBB15_176
# BB#95:                                # %if.end108
	movl	q_size(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_96
.LBB15_176:                             # %if.end116
	movq	24(%r13), %rax
	movq	note_list(%rip), %rcx
	movq	last_scheduled_insn(%rip), %r14
	testq	%rcx, %rcx
	je	.LBB15_177
	.align	16, 0x90
.LBB15_178:                             # %while.cond123
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rbx
	movq	16(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB15_178
# BB#179:                               # %while.end132
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rbx)
	movq	16(%rax), %rcx
	movq	%rbx, 24(%rcx)
	movq	note_list(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	note_list(%rip), %rcx
	movq	%rax, 24(%rcx)
	jmp	.LBB15_180
.LBB15_177:
	movq	%rax, %rbx
.LBB15_180:                             # %if.end151
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_182
# BB#181:                               # %if.then153
	movq	sched_dump(%rip), %rdi
	movl	8(%rbx), %ecx
	movl	clock_var(%rip), %edx
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	sched_dump(%rip), %rdi
	movl	8(%r14), %edx
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	callq	fprintf
	callq	visualize_free
.LBB15_182:                             # %if.end162
	movq	current_sched_info(%rip), %rax
	movq	%rbx, 80(%rax)
	movq	current_sched_info(%rip), %rax
	movq	%r14, 88(%rax)
	movq	64(%rsp), %rdi
	callq	free
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_183:                             # %if.then.i.i.i
	movabsq	$.L.str2, %rdi
	movabsq	$.L__FUNCTION__.ready_lastpos, %rdx
	movq	$893, %rsi              # imm = 0x37D
	callq	fancy_abort
.LBB15_184:                             # %if.then.i276
	movabsq	$.L.str2, %rdi
	movabsq	$.L__FUNCTION__.ready_remove_first, %rdx
	movq	$925, %rsi              # imm = 0x39D
	callq	fancy_abort
.LBB15_2:                               # %if.then
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.schedule_block, %rcx
	movq	$1658, %rsi             # imm = 0x67A
	jmp	.LBB15_3
.LBB15_38:                              # %if.then34
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.schedule_block, %rcx
	movq	$1713, %rsi             # imm = 0x6B1
	jmp	.LBB15_3
.LBB15_46:                              # %if.then.i.i236
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.ready_lastpos, %rcx
	movq	$893, %rsi              # imm = 0x37D
	jmp	.LBB15_3
.LBB15_96:                              # %if.then115
	movabsq	$.L.str2, %rax
	movabsq	$.L__FUNCTION__.schedule_block, %rcx
	movq	$1806, %rsi             # imm = 0x70E
.LBB15_3:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp128:
	.size	schedule_block, .Ltmp128-schedule_block
	.cfi_endproc

	.align	16, 0x90
	.type	actual_hazard,@function
actual_hazard:                          # @actual_hazard
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
	subq	$64, %rsp
.Ltmp140:
	.cfi_def_cfa_offset 112
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
	movq	%rcx, %r8
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rdi, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB16_32
# BB#1:                                 # %if.then
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	unit_tick(,%rax,4), %r10d
	movq	%r10, %rbx
	subq	%rdx, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rbx
	jle	.LBB16_2
# BB#3:                                 # %if.then.i
	leaq	(%rax,%rax,8), %rbx
	movq	function_units+56(,%rbx,8), %r15
	testq	%r15, %r15
	je	.LBB16_4
# BB#5:                                 # %if.then3.i
	movq	%r10, %r14
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	function_units+64(,%rbx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB16_7
# BB#6:                                 # %if.then7.i
	movq	unit_last_insn(,%rax,8), %rdi
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	callq	*%rcx
	movl	function_units+48(,%rbx,8), %ecx
	addq	%r14, %rax
	subq	%rcx, %rax
	movq	%rax, %r10
	movq	%r12, %rdx
	movq	56(%rsp), %r8           # 8-byte Reload
	jmp	.LBB16_11
.LBB16_32:                              # %if.else
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r13, %rax
	andq	%r14, %rax
	cmpq	%r14, %rax
	je	.LBB16_33
# BB#34:                                # %for.body13.lr.ph
	movq	%rdx, %r12
	movq	%rsi, %r15
	notq	%r13
	xorq	%rbx, %rbx
	movq	%r8, %rax
	.align	16, 0x90
.LBB16_35:                              # %for.body13
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %r13
	je	.LBB16_37
# BB#36:                                # %if.then15
                                        #   in Loop: Header=BB16_35 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	actual_hazard
.LBB16_37:                              # %for.inc18
                                        #   in Loop: Header=BB16_35 Depth=1
	incq	%rbx
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$33, %rcx
	sarq	%cl, %r13
	testq	%r14, %r13
	jne	.LBB16_35
	jmp	.LBB16_38
.LBB16_2:
	movq	%r8, %rax
	jmp	.LBB16_38
.LBB16_33:
	movq	%r8, %rax
	jmp	.LBB16_38
.LBB16_4:
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	jmp	.LBB16_11
.LBB16_7:                               # %if.else.i
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movslq	8(%rsi), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	28(%rdx,%rax), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	leaq	1(%r13), %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB16_8
# BB#9:                                 # %if.else.i.i
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	$8, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	andq	$255, %rax
	orq	%rdx, %rax
	jmp	.LBB16_10
.LBB16_8:                               # %if.then.i.i
	movq	%rsi, %rdi
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	callq	*%r15
	movabsq	$4294901760, %rdx       # imm = 0xFFFF0000
	andq	%rax, %rdx
	movq	$8, %rcx
	shlq	%cl, %rbx
	movq	$16, %rcx
	shrq	%cl, %rdx
	orq	%rbx, %rdx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	48(%rsp), %rcx          # 8-byte Reload
	movslq	8(%rcx), %rcx
	orq	%rdx, %rsi
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%esi, 28(%rdx,%rdi)
.LBB16_10:                              # %blockage_range.exit.i
	movq	%r12, %rdx
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r10
	movq	40(%rsp), %rbx          # 8-byte Reload
	movl	function_units+48(,%rbx,8), %ecx
	andq	$65535, %rax            # imm = 0xFFFF
	subq	%rcx, %r10
	addq	%rax, %r10
.LBB16_11:                              # %actual_hazard_this_instance.exit
	subq	%rdx, %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rax
	jge	.LBB16_13
# BB#12:                                # %actual_hazard_this_instance.exit
	movq	%r8, %r10
.LBB16_13:                              # %actual_hazard_this_instance.exit
	jle	.LBB16_14
# BB#15:                                # %if.then2
	movq	%rdx, %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movl	function_units+12(,%rbx,8), %r14d
	leaq	function_units+64(,%rbx,8), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	leaq	1(%r13), %rax
	leaq	function_units+48(,%rbx,8), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	$8, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	shlq	%cl, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r15, %r9
.LBB16_16:                              # %for.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_17 Depth 2
	movq	%r10, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	.align	16, 0x90
.LBB16_17:                              # %for.cond
                                        #   Parent Loop BB16_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB16_18
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB16_17 Depth=2
	movl	unit_tick+104(,%r13,4), %ebx
	movq	%rbx, %rax
	subq	%rdi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movq	%r8, %rax
	jle	.LBB16_30
# BB#20:                                # %if.then.i58
                                        #   in Loop: Header=BB16_17 Depth=2
	testq	%r9, %r9
	je	.LBB16_27
# BB#21:                                # %if.then3.i61
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	jne	.LBB16_22
# BB#23:                                # %if.else.i75
                                        #   in Loop: Header=BB16_17 Depth=2
	movslq	8(%rsi), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	28(%rdx,%rax), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rdx
	jne	.LBB16_24
# BB#25:                                # %if.else.i.i89
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	$8, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	andq	$255, %rax
	orq	%rdx, %rax
	jmp	.LBB16_26
.LBB16_22:                              # %if.then7.i67
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	unit_last_insn+208(,%r13,8), %rdi
	movq	%r9, %r15
	movq	%r10, %r12
	callq	*%rax
	movq	%r12, %r10
	movq	%r15, %r9
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %ecx
	addq	%rbx, %rax
	subq	%rcx, %rax
	movq	%rax, %rbx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	jmp	.LBB16_27
.LBB16_24:                              # %if.then.i.i85
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	%rsi, %rdi
	movq	%rsi, %r12
	movq	%r9, %r15
	callq	*%r15
	movq	%r15, %r9
	movq	%rax, %rdx
	movabsq	$4294901760, %rcx       # imm = 0xFFFF0000
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movq	(%rsp), %rcx            # 8-byte Reload
	orq	%rcx, %rdx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movslq	8(%r12), %rcx
	orq	%rdx, %rsi
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%esi, 28(%rdx,%rdi)
.LBB16_26:                              # %blockage_range.exit.i95
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %ecx
	andq	$65535, %rax            # imm = 0xFFFF
	subq	%rcx, %rbx
	addq	%rax, %rbx
	movq	16(%rsp), %r10          # 8-byte Reload
.LBB16_27:                              # %if.end22.i100
                                        #   in Loop: Header=BB16_17 Depth=2
	subq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB16_29
# BB#28:                                # %if.end22.i100
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	%r8, %rbx
.LBB16_29:                              # %if.end22.i100
                                        #   in Loop: Header=BB16_17 Depth=2
	movq	%rbx, %rax
.LBB16_30:                              # %actual_hazard_this_instance.exit102
                                        #   in Loop: Header=BB16_17 Depth=2
	addq	$26, %r13
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB16_17
# BB#31:                                # %if.then6
                                        #   in Loop: Header=BB16_16 Depth=1
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	%rax, %r10
	jg	.LBB16_16
	jmp	.LBB16_38
.LBB16_14:
	movq	%r10, %rax
	jmp	.LBB16_38
.LBB16_18:
	movq	%r10, %rax
.LBB16_38:                              # %if.end20
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp146:
	.size	actual_hazard, .Ltmp146-actual_hazard
	.cfi_endproc

	.globl	set_priorities
	.align	16, 0x90
	.type	set_priorities,@function
set_priorities:                         # @set_priorities
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp153:
	.cfi_def_cfa_offset 32
.Ltmp154:
	.cfi_offset %rbx, -32
.Ltmp155:
	.cfi_offset %r14, -24
.Ltmp156:
	.cfi_offset %r15, -16
	movq	16(%rdi), %r15
	movq	%rsi, %rbx
	cmpq	%rbx, %rdi
	je	.LBB17_1
# BB#2:                                 # %for.cond.preheader
	xorq	%r14, %r14
	jmp	.LBB17_3
.LBB17_1:                               # %land.lhs.true
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r14, %r14
	cmpq	$105, %rax
	jne	.LBB17_7
.LBB17_3:                               # %for.cond.preheader
	cmpq	%rbx, %r15
	je	.LBB17_7
	.align	16, 0x90
.LBB17_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$37, %rcx
	je	.LBB17_6
# BB#5:                                 # %if.end11
                                        #   in Loop: Header=BB17_4 Depth=1
	movq	$28, %rcx
	shrq	%cl, %rax
	notq	%rax
	andq	$1, %rax
	addq	%rax, %r14
	movq	%rbx, %rdi
	callq	priority
.LBB17_6:                               # %for.inc
                                        #   in Loop: Header=BB17_4 Depth=1
	movq	16(%rbx), %rbx
	cmpq	%r15, %rbx
	jne	.LBB17_4
.LBB17_7:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp157:
	.size	set_priorities, .Ltmp157-set_priorities
	.cfi_endproc

	.align	16, 0x90
	.type	priority,@function
priority:                               # @priority
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp164:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp165:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp166:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp167:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp168:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp169:
	.cfi_def_cfa_offset 64
.Ltmp170:
	.cfi_offset %rbx, -48
.Ltmp171:
	.cfi_offset %r12, -40
.Ltmp172:
	.cfi_offset %r13, -32
.Ltmp173:
	.cfi_offset %r14, -24
.Ltmp174:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB18_21
# BB#1:                                 # %if.end
	movslq	8(%r14), %rdx
	movq	h_i_d(%rip), %rax
	leaq	(%rdx,%rdx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movzbl	46(%rax,%rsi), %ecx
	testq	$8, %rcx
	jne	.LBB18_20
# BB#2:                                 # %if.then7
	leaq	(%rdx,%rdx,2), %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	(%rax,%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB18_11
# BB#3:
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB18_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rbx), %eax
	testq	$64, %rax
	jne	.LBB18_9
# BB#5:                                 # %if.end28
                                        #   in Loop: Header=BB18_4 Depth=1
	movq	8(%rbx), %r15
	movq	current_sched_info(%rip), %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	*48(%rax)
	testq	%r12, %rax
	je	.LBB18_9
# BB#6:                                 # %if.end34
                                        #   in Loop: Header=BB18_4 Depth=1
	movq	%r12, %r13
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	insn_cost
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	priority
	addq	%r12, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB18_8
# BB#7:                                 # %if.end34
                                        #   in Loop: Header=BB18_4 Depth=1
	movq	%rdi, %rax
.LBB18_8:                               # %if.end34
                                        #   in Loop: Header=BB18_4 Depth=1
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r13, %r12
.LBB18_9:                               # %for.inc
                                        #   in Loop: Header=BB18_4 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB18_4
# BB#10:                                # %if.end44.loopexit
	movl	8(%r14), %edx
	movq	h_i_d(%rip), %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	jmp	.LBB18_19
.LBB18_11:                              # %if.then15
	movq	$4, %rcx
	shlq	%cl, %rsi
	movzbl	40(%rax,%rsi), %esi
	movq	$48, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$48, %rcx
	sarq	%cl, %rdi
	testq	%rsi, %rsi
	jne	.LBB18_19
# BB#12:                                # %if.then.i
	movslq	40(%r14), %rax
	testq	%rax, %rax
	jns	.LBB18_15
# BB#13:                                # %cond.end.i
	movq	%r14, %rdi
	callq	recog_memoized_1
	movslq	40(%r14), %rax
	testq	%rax, %rax
	js	.LBB18_14
.LBB18_15:                              # %if.else.i
	movq	%r14, %rdi
	callq	result_ready_cost
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	$1, %rdi
	jle	.LBB18_17
# BB#16:                                # %if.else.i
	movq	%rax, %rdi
.LBB18_17:                              # %if.else.i
	movslq	8(%r14), %rdx
	movq	h_i_d(%rip), %rax
	leaq	(%rdx,%rdx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
.LBB18_18:                              # %if.end44
	movw	%di, 40(%rax,%rsi)
.LBB18_19:                              # %if.end44
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%edi, 20(%rax,%rdx)
	movslq	8(%r14), %rcx
	leaq	(%rcx,%rcx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movzbl	46(%rax,%rdx), %ecx
	orq	$8, %rcx
	movb	%cl, 46(%rax,%rdx)
	movl	8(%r14), %edx
	movq	h_i_d(%rip), %rax
.LBB18_20:                              # %if.end58
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	20(%rax,%rdx), %eax
.LBB18_21:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_14:                              # %if.then17.i
	movslq	8(%r14), %rdx
	movq	h_i_d(%rip), %rax
	leaq	(%rdx,%rdx,2), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	$1, %rdi
	jmp	.LBB18_18
.Ltmp175:
	.size	priority, .Ltmp175-priority
	.cfi_endproc

	.globl	sched_init
	.align	16, 0x90
	.type	sched_init,@function
sched_init:                             # @sched_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp182:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp183:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp184:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp185:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp186:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp187:
	.cfi_def_cfa_offset 64
.Ltmp188:
	.cfi_offset %rbx, -48
.Ltmp189:
	.cfi_offset %r12, -40
.Ltmp190:
	.cfi_offset %r13, -32
.Ltmp191:
	.cfi_offset %r14, -24
.Ltmp192:
	.cfi_offset %r15, -16
	movslq	sched_verbose_param(%rip), %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rax
	jne	.LBB19_2
# BB#1:                                 # %entry
	movq	%rsi, %rax
.LBB19_2:                               # %entry
	testq	%rdi, %rdi
	je	.LBB19_4
# BB#3:                                 # %entry
	movq	%rsi, %rcx
.LBB19_4:                               # %entry
	testq	%rcx, %rax
	movq	$1, %rax
	movq	$1, %rsi
	jne	.LBB19_6
# BB#5:                                 # %entry
	movq	%rdx, %rsi
.LBB19_6:                               # %entry
	cmpq	$10, %rdx
	movl	%esi, sched_verbose(%rip)
	jge	.LBB19_7
# BB#8:                                 # %entry
	movabsq	$1, %rdx
	jmp	.LBB19_9
.LBB19_7:
	movabsq	$0, %rdx
.LBB19_9:                               # %entry
	testq	%rcx, %rdx
	jne	.LBB19_11
# BB#10:                                # %entry
	movq	stderr(%rip), %rdi
.LBB19_11:                              # %entry
	movq	targetm+184(%rip), %rcx
	movq	%rdi, sched_dump(%rip)
	testq	%rcx, %rcx
	je	.LBB19_13
# BB#12:                                # %if.then4
	callq	*%rcx
.LBB19_13:                              # %if.end5
	movl	%eax, issue_rate(%rip)
	callq	get_max_uid
	incq	%rax
	movl	%eax, old_max_uid(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$48, %rsi
	movq	%rax, %rdi
	callq	xcalloc
	movq	%rax, h_i_d(%rip)
	movslq	n_basic_blocks(%rip), %rcx
	xorq	%r10, %r10
	movl	%r10d, 16(%rax)
	testq	%rcx, %rcx
	movq	$1, %rdi
	jle	.LBB19_23
# BB#14:                                # %for.body.lr.ph
	movq	basic_block_info(%rip), %r8
	movq	$1, %rdx
	movabsq	$0, %r11
	movabsq	$1, %r9
	.align	16, 0x90
.LBB19_15:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_18 Depth 2
	movq	32(%r8,%r10,8), %rbx
	movq	(%rbx), %rsi
	movslq	8(%rsi), %rcx
	leaq	(%rcx,%rcx,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%edx, 16(%rax,%rdi)
	movzbl	(%rsi), %ecx
	cmpq	$37, %rcx
	movq	%r11, %rdi
	je	.LBB19_17
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB19_15 Depth=1
	movq	%r9, %rdi
.LBB19_17:                              # %for.body
                                        #   in Loop: Header=BB19_15 Depth=1
	movq	8(%rbx), %rbx
	addq	%rdx, %rdi
	jmp	.LBB19_18
	.align	16, 0x90
.LBB19_21:                              # %for.inc
                                        #   in Loop: Header=BB19_18 Depth=2
	addq	%rcx, %rdi
.LBB19_18:                              # %for.body
                                        #   Parent Loop BB19_15 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rbx, %rsi
	je	.LBB19_22
# BB#19:                                # %for.inc
                                        #   in Loop: Header=BB19_18 Depth=2
	movq	24(%rsi), %rsi
	movslq	8(%rsi), %rcx
	leaq	(%rcx,%rcx,2), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	%edi, 16(%rax,%rdx)
	movzbl	(%rsi), %ecx
	cmpq	$37, %rcx
	movq	%r11, %rcx
	je	.LBB19_21
# BB#20:                                # %for.inc
                                        #   in Loop: Header=BB19_18 Depth=2
	movq	%r9, %rcx
	jmp	.LBB19_21
	.align	16, 0x90
.LBB19_22:                              # %for.inc31
                                        #   in Loop: Header=BB19_15 Depth=1
	incq	%r10
	movslq	n_basic_blocks(%rip), %rdx
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	%rdi, %rdx
	jl	.LBB19_15
.LBB19_23:                              # %for.end33
	callq	init_dependency_caches
	movl	old_max_uid(%rip), %edi
	callq	compute_bb_for_insn
	callq	init_alias_analysis
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_38
# BB#24:                                # %if.then36
	movslq	n_basic_blocks(%rip), %rdi
	movq	$8, %rsi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, line_note_head(%rip)
	testq	%rdx, %rdx
	jle	.LBB19_38
# BB#25:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB19_26:                              # %for.body42
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_27 Depth 2
                                        #     Child Loop BB19_31 Depth 2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, %rsi
	je	.LBB19_37
	.align	16, 0x90
.LBB19_27:                              # %for.body50
                                        #   Parent Loop BB19_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rsi), %edi
	cmpq	$37, %rdi
	jne	.LBB19_30
# BB#28:                                # %land.lhs.true55
                                        #   in Loop: Header=BB19_27 Depth=2
	movslq	40(%rsi), %rdi
	testq	%rdi, %rdi
	jg	.LBB19_29
.LBB19_30:                              # %for.inc65
                                        #   in Loop: Header=BB19_27 Depth=2
	movq	16(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB19_27
	jmp	.LBB19_31
.LBB19_29:                              # %if.then61
                                        #   in Loop: Header=BB19_26 Depth=1
	movq	line_note_head(%rip), %rcx
	movq	%rsi, (%rcx,%rax,8)
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	(%rcx), %rcx
	jmp	.LBB19_31
	.align	16, 0x90
.LBB19_36:                              # %for.inc101
                                        #   in Loop: Header=BB19_31 Depth=2
	movq	24(%rcx), %rcx
.LBB19_31:                              # %for.end69
                                        #   Parent Loop BB19_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	je	.LBB19_37
# BB#32:                                # %for.body77
                                        #   in Loop: Header=BB19_31 Depth=2
	movzbl	(%rcx), %esi
	movzbl	rtx_class(%rsi), %edi
	cmpq	$105, %rdi
	je	.LBB19_37
# BB#33:                                # %if.end86
                                        #   in Loop: Header=BB19_31 Depth=2
	cmpq	$37, %rsi
	jne	.LBB19_36
# BB#34:                                # %land.lhs.true91
                                        #   in Loop: Header=BB19_31 Depth=2
	movslq	40(%rcx), %rsi
	testq	%rsi, %rsi
	jle	.LBB19_36
# BB#35:                                # %if.then97
                                        #   in Loop: Header=BB19_31 Depth=2
	movq	line_note_head(%rip), %rsi
	movq	%rcx, (%rsi,%rax,8)
	jmp	.LBB19_36
	.align	16, 0x90
.LBB19_37:                              # %for.inc106
                                        #   in Loop: Header=BB19_26 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB19_26
.LBB19_38:                              # %if.end109
	movl	sched_verbose(%rip), %eax
	testq	%rax, %rax
	je	.LBB19_40
# BB#39:                                # %if.then111
	callq	init_target_units
.LBB19_40:                              # %if.end112
	movl	n_basic_blocks(%rip), %edi
	leaq	-1(%rdi), %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movq	8(%rax), %rsi
	movq	24(%rsi), %rax
	testq	%rax, %rax
	je	.LBB19_43
# BB#41:                                # %lor.lhs.false123
	movl	(%rsi), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$36, %rcx
	je	.LBB19_44
# BB#42:                                # %land.lhs.true133
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	je	.LBB19_44
.LBB19_43:                              # %if.then141
	movabsq	$4294967197, %rdi       # imm = 0xFFFFFF9D
	callq	emit_note_after
	movl	n_basic_blocks(%rip), %edi
	leaq	-1(%rdi), %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rax
	movq	8(%rax), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, 8(%rax)
.LBB19_44:                              # %if.end164
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_57
# BB#45:                                # %for.body169.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$0, %r13
	jmp	.LBB19_46
.LBB19_53:                              #   in Loop: Header=BB19_46 Depth=1
	movq	%rax, %rbx
	movq	%rcx, %rdx
	jmp	.LBB19_54
	.align	16, 0x90
.LBB19_46:                              # %for.body169
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_47 Depth 2
                                        #       Child Loop BB19_49 Depth 3
                                        #     Child Loop BB19_55 Depth 2
                                        #       Child Loop BB19_65 Depth 3
                                        #         Child Loop BB19_66 Depth 4
                                        #       Child Loop BB19_73 Depth 3
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdi,8), %rcx
	movq	(%rcx), %rax
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rax
	movq	%rax, %rbx
	movq	%rax, %rdx
	je	.LBB19_54
.LBB19_47:                              # %while.body.lr.ph.i.i
                                        #   Parent Loop BB19_46 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_49 Depth 3
	movzbl	(%rax), %edx
	cmpq	$37, %rdx
	je	.LBB19_48
	.align	16, 0x90
.LBB19_49:                              # %if.else.i.i
                                        #   Parent Loop BB19_46 Depth=1
                                        #     Parent Loop BB19_47 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rcx), %esi
	cmpq	$37, %rsi
	jne	.LBB19_52
# BB#50:                                # %if.then11.i.i
                                        #   in Loop: Header=BB19_49 Depth=3
	movq	16(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_49
	jmp	.LBB19_51
	.align	16, 0x90
.LBB19_52:                              # %if.else15.i.i
                                        #   in Loop: Header=BB19_47 Depth=2
	cmpq	$36, %rdx
	jne	.LBB19_53
.LBB19_48:                              # %while.cond.outer.backedge.i.i
                                        #   in Loop: Header=BB19_47 Depth=2
	movq	24(%rax), %rax
	cmpq	%rcx, %rax
	movq	%rcx, %rbx
	movq	%rcx, %rdx
	jne	.LBB19_47
	jmp	.LBB19_54
	.align	16, 0x90
.LBB19_51:                              #   in Loop: Header=BB19_46 Depth=1
	movq	%rax, %rbx
	movq	%rax, %rdx
.LBB19_54:                              # %get_block_head_tail.exit.i
                                        #   in Loop: Header=BB19_46 Depth=1
	movq	24(%rdx), %r14
	jmp	.LBB19_55
	.align	16, 0x90
.LBB19_79:                              # %for.inc88.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movq	24(%rbx), %rbx
.LBB19_55:                              # %get_block_head_tail.exit.i
                                        #   Parent Loop BB19_46 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_65 Depth 3
                                        #         Child Loop BB19_66 Depth 4
                                        #       Child Loop BB19_73 Depth 3
	cmpq	%r14, %rbx
	je	.LBB19_56
# BB#58:                                # %for.body.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB19_79
# BB#59:                                # %if.end.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movq	32(%rbx), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	je	.LBB19_61
# BB#60:                                # %if.end.i
                                        #   in Loop: Header=BB19_55 Depth=2
	cmpq	$49, %rcx
	jne	.LBB19_63
.LBB19_61:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movq	8(%r12), %rdi
	xorq	%rsi, %rsi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r15
	jne	.LBB19_72
# BB#62:                                # %land.lhs.true.if.else_crit_edge.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movl	(%r12), %eax
.LBB19_63:                              # %if.else.i
                                        #   in Loop: Header=BB19_55 Depth=2
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%r15, %r15
	cmpq	$39, %rax
	jne	.LBB19_72
# BB#64:                                # %if.then23.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movq	8(%r12), %rax
	movl	(%rax), %r12d
	xorq	%r15, %r15
	jmp	.LBB19_65
	.align	16, 0x90
.LBB19_71:                              # %land.lhs.true47.i
                                        #   in Loop: Header=BB19_65 Depth=3
	addq	%rax, %r15
.LBB19_65:                              # %for.cond26.outer.i
                                        #   Parent Loop BB19_46 Depth=1
                                        #     Parent Loop BB19_55 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB19_66 Depth 4
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
.LBB19_66:                              # %for.cond26.i
                                        #   Parent Loop BB19_46 Depth=1
                                        #     Parent Loop BB19_55 Depth=2
                                        #       Parent Loop BB19_65 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_72
# BB#67:                                # %for.body29.i
                                        #   in Loop: Header=BB19_66 Depth=4
	movq	32(%rbx), %rax
	movq	8(%rax), %rax
	movq	(%rax,%r12,8), %rax
	movzbl	(%rax), %ecx
	decq	%r12
	cmpq	$47, %rcx
	je	.LBB19_69
# BB#68:                                # %for.body29.i
                                        #   in Loop: Header=BB19_66 Depth=4
	cmpq	$49, %rcx
	jne	.LBB19_66
	.align	16, 0x90
.LBB19_69:                              # %land.lhs.true47.i
                                        #   in Loop: Header=BB19_65 Depth=3
	movq	8(%rax), %rdi
	xorq	%rsi, %rsi
	callq	register_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r13, %rax
	je	.LBB19_71
# BB#70:                                # %land.lhs.true47.i
                                        #   in Loop: Header=BB19_65 Depth=3
	movabsq	$1, %rax
	jmp	.LBB19_71
	.align	16, 0x90
.LBB19_72:                              # %if.end57.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movq	56(%rbx), %rax
	jmp	.LBB19_73
	.align	16, 0x90
.LBB19_77:                              # %for.inc77.i
                                        #   in Loop: Header=BB19_73 Depth=3
	movq	16(%rax), %rax
.LBB19_73:                              # %if.end57.i
                                        #   Parent Loop BB19_46 Depth=1
                                        #     Parent Loop BB19_55 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	je	.LBB19_78
# BB#74:                                # %for.body63.i
                                        #   in Loop: Header=BB19_73 Depth=3
	movzbl	2(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB19_76
# BB#75:                                # %for.body63.i
                                        #   in Loop: Header=BB19_73 Depth=3
	cmpq	$10, %rcx
	jne	.LBB19_77
.LBB19_76:                              # %if.then74.i
                                        #   in Loop: Header=BB19_73 Depth=3
	decq	%r15
	jmp	.LBB19_77
	.align	16, 0x90
.LBB19_78:                              # %for.end81.i
                                        #   in Loop: Header=BB19_55 Depth=2
	movslq	8(%rbx), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movw	%r15w, 44(%rdx,%rax)
	jmp	.LBB19_79
	.align	16, 0x90
.LBB19_56:                              # %for.cond166.backedge
                                        #   in Loop: Header=BB19_46 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB19_46
.LBB19_57:                              # %for.end171
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp193:
	.size	sched_init, .Ltmp193-sched_init
	.cfi_endproc

	.globl	sched_finish
	.align	16, 0x90
	.type	sched_finish,@function
sched_finish:                           # @sched_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp195:
	.cfi_def_cfa_offset 16
	movq	h_i_d(%rip), %rdi
	callq	free
	callq	free_dependency_caches
	callq	end_alias_analysis
	movl	write_symbols(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_1
# BB#2:                                 # %if.then
	movq	line_note_head(%rip), %rdi
	popq	%rax
	jmp	free  # TAILCALL
.LBB20_1:                               # %if.end
	popq	%rax
	retq
.Ltmp196:
	.size	sched_finish, .Ltmp196-sched_finish
	.cfi_endproc

	.align	16, 0x90
	.type	schedule_unit,@function
schedule_unit:                          # @schedule_unit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp204:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp205:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp206:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp207:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp208:
	.cfi_def_cfa_offset 96
.Ltmp209:
	.cfi_offset %rbx, -48
.Ltmp210:
	.cfi_offset %r12, -40
.Ltmp211:
	.cfi_offset %r13, -32
.Ltmp212:
	.cfi_offset %r14, -24
.Ltmp213:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB21_16
# BB#1:                                 # %if.then
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	(%rbx,%rbx,8), %rax
	movl	function_units+12(,%rax,8), %r13d
	decq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB21_2
# BB#3:                                 # %for.body.lr.ph
	leaq	function_units+56(,%rax,8), %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	leaq	function_units+64(,%rax,8), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	incq	%r12
	leaq	function_units+48(,%rax,8), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$8, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB21_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	unit_tick(,%rbx,4), %r12d
	movq	%r12, %rax
	subq	%r15, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB21_14
# BB#6:                                 # %if.then.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movq	(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB21_13
# BB#7:                                 # %if.then3.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB21_8
# BB#9:                                 # %if.else.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movslq	8(%r14), %rax
	movq	h_i_d(%rip), %rsi
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	28(%rsi,%rax), %eax
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rsi
	jne	.LBB21_10
# BB#11:                                # %if.else.i.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movq	$8, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	andq	$16711680, %rdx         # imm = 0xFF0000
	andq	$255, %rax
	orq	%rdx, %rax
	jmp	.LBB21_12
	.align	16, 0x90
.LBB21_8:                               # %if.then7.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movq	%rdi, %r15
	movq	unit_last_insn(,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	*%rax
	movq	%r15, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movl	(%rsi), %ecx
	addq	%r12, %rax
	subq	%rcx, %rax
	movq	%rax, %r12
	movq	40(%rsp), %r15          # 8-byte Reload
	jmp	.LBB21_13
.LBB21_10:                              # %if.then.i.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movq	%r14, %rdi
	callq	*%rdx
	movq	%rax, %rdx
	movabsq	$4294901760, %rcx       # imm = 0xFFFF0000
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdx
	movq	(%rsp), %rcx            # 8-byte Reload
	orq	%rcx, %rdx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movslq	8(%r14), %rcx
	orq	%rdx, %rsi
	movq	h_i_d(%rip), %rdx
	leaq	(%rcx,%rcx,2), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movl	%esi, 28(%rdx,%rdi)
.LBB21_12:                              # %blockage_range.exit.i
                                        #   in Loop: Header=BB21_5 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	(%rsi), %ecx
	andq	$65535, %rax            # imm = 0xFFFF
	subq	%rcx, %r12
	addq	%rax, %r12
	movq	8(%rsp), %rdi           # 8-byte Reload
.LBB21_13:                              # %if.end22.i
                                        #   in Loop: Header=BB21_5 Depth=1
	subq	%r15, %r12
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	testq	%r12, %r12
	jle	.LBB21_14
# BB#4:                                 # %for.cond
                                        #   in Loop: Header=BB21_5 Depth=1
	addq	$26, %rbx
	decq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB21_5
.LBB21_14:                              # %for.cond.for.end_crit_edge
	movq	%rbx, %r12
	jmp	.LBB21_15
.LBB21_16:                              # %if.else
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	%r13, %rax
	je	.LBB21_21
# BB#17:                                # %for.body12.lr.ph
	notq	%r12
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB21_18:                              # %for.body12
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %r12
	je	.LBB21_20
# BB#19:                                # %if.then14
                                        #   in Loop: Header=BB21_18 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	schedule_unit
.LBB21_20:                              # %for.inc16
                                        #   in Loop: Header=BB21_18 Depth=1
	incq	%rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$33, %rcx
	sarq	%cl, %r12
	testq	%r13, %r12
	jne	.LBB21_18
	jmp	.LBB21_21
.LBB21_2:                               # %if.then.for.end_crit_edge
	leaq	function_units+48(,%rax,8), %rsi
.LBB21_15:                              # %for.end
	movq	$32, %rcx
	shlq	%cl, %r12
	movl	(%rsi), %eax
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r14, unit_last_insn(,%r12,8)
	addq	%r15, %rax
	movl	%eax, unit_tick(,%r12,4)
.LBB21_21:                              # %if.end18
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp214:
	.size	schedule_unit, .Ltmp214-schedule_unit
	.cfi_endproc

	.align	16, 0x90
	.type	rank_for_schedule,@function
rank_for_schedule:                      # @rank_for_schedule
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp221:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp222:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp223:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp224:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp225:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp226:
	.cfi_def_cfa_offset 64
.Ltmp227:
	.cfi_offset %rbx, -48
.Ltmp228:
	.cfi_offset %r12, -40
.Ltmp229:
	.cfi_offset %r13, -32
.Ltmp230:
	.cfi_offset %r14, -24
.Ltmp231:
	.cfi_offset %r15, -16
	movq	(%rdi), %r14
	movq	(%rsi), %r15
	movslq	8(%r14), %rcx
	movq	h_i_d(%rip), %rax
	leaq	(%rcx,%rcx,2), %rdx
	movslq	8(%r15), %rsi
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	20(%rax,%rdi), %ebx
	leaq	(%rsi,%rsi,2), %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	20(%rax,%rdi), %ecx
	subq	%rcx, %rbx
	jne	.LBB22_31
# BB#1:                                 # %if.end
	movl	reload_completed(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB22_4
# BB#2:                                 # %land.lhs.true
	movq	$4, %rcx
	shlq	%cl, %rsi
	movzbl	44(%rax,%rsi), %ebx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movzbl	44(%rax,%rdx), %eax
	cmpq	%rax, %rbx
	jne	.LBB22_3
.LBB22_4:                               # %if.end35
	movq	current_sched_info(%rip), %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	*32(%rax)
	movq	%rax, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rbx
	jne	.LBB22_31
# BB#5:                                 # %if.end38
	movq	last_scheduled_insn(%rip), %rax
	testq	%rax, %rax
	je	.LBB22_21
# BB#6:                                 # %if.then40
	movslq	8(%rax), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rsi
	movq	%r15, %rdi
	callq	find_insn_list
	movq	%rax, %r12
	testq	%r12, %r12
	movq	$3, %rbx
	movq	$3, %rax
	je	.LBB22_12
# BB#7:                                 # %lor.lhs.false
	movq	last_scheduled_insn(%rip), %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	insn_cost
	andq	%r13, %rax
	cmpq	$1, %rax
	movq	$3, %rax
	je	.LBB22_12
# BB#8:                                 # %if.else
	movzbl	2(%r12), %eax
	testq	%rax, %rax
	jne	.LBB22_9
# BB#10:                                # %if.else
	movabsq	$1, %rax
	jmp	.LBB22_11
.LBB22_3:                               # %if.then34
	movq	$48, %rcx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	movq	$48, %rcx
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	jmp	.LBB22_30
.LBB22_9:
	movabsq	$0, %rax
.LBB22_11:                              # %if.else
	xorq	$1, %rax
	incq	%rax
.LBB22_12:                              # %if.end57
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	last_scheduled_insn(%rip), %rax
	movslq	8(%rax), %rax
	movq	h_i_d(%rip), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rsi
	movq	%r14, %rdi
	callq	find_insn_list
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB22_13
# BB#14:                                # %lor.lhs.false67
	movq	last_scheduled_insn(%rip), %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	insn_cost
	andq	%r13, %rax
	cmpq	$1, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB22_19
# BB#15:                                # %if.else72
	movzbl	2(%r12), %eax
	testq	%rax, %rax
	jne	.LBB22_16
# BB#17:                                # %if.else72
	movabsq	$1, %rbx
	jmp	.LBB22_18
.LBB22_13:
	movq	8(%rsp), %rdx           # 8-byte Reload
	jmp	.LBB22_19
.LBB22_16:
	movabsq	$0, %rbx
.LBB22_18:                              # %if.else72
	xorq	$1, %rbx
	incq	%rbx
.LBB22_19:                              # %if.end81
	movq	%rdx, %rax
	andq	%r13, %rax
	movq	%rbx, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	jne	.LBB22_20
.LBB22_21:                              # %if.end86
	movslq	8(%r15), %rcx
	movq	h_i_d(%rip), %rax
	leaq	(%rcx,%rcx,2), %r8
	movq	$4, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	(%rax,%rsi), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	movabsq	$0, %rsi
	je	.LBB22_24
# BB#22:
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB22_23:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rcx), %rcx
	incq	%rsi
	testq	%rcx, %rcx
	jne	.LBB22_23
.LBB22_24:                              # %for.end
	movslq	8(%r14), %rcx
	leaq	(%rcx,%rcx,2), %rdx
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	(%rax,%rdi), %rcx
	jmp	.LBB22_26
	.align	16, 0x90
.LBB22_25:                              # %for.body104
                                        #   in Loop: Header=BB22_26 Depth=1
	movq	16(%rcx), %rcx
	incq	%rbx
.LBB22_26:                              # %for.body104
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	jne	.LBB22_25
# BB#27:                                # %for.end110
	movq	%rsi, %rcx
	andq	%r13, %rcx
	andq	%rbx, %r13
	cmpq	%rcx, %r13
	jne	.LBB22_28
# BB#29:                                # %if.end114
	movq	$4, %rcx
	shlq	%cl, %r8
	movl	16(%rax,%r8), %ebx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	16(%rax,%rdx), %eax
.LBB22_30:                              # %return
	subq	%rax, %rbx
	jmp	.LBB22_31
.LBB22_28:                              # %if.then113
	subq	%rsi, %rbx
	jmp	.LBB22_31
.LBB22_20:                              # %if.then84
	subq	%rdx, %rbx
.LBB22_31:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp232:
	.size	rank_for_schedule, .Ltmp232-rank_for_schedule
	.cfi_endproc

	.type	sched_verbose,@object   # @sched_verbose
	.bss
	.globl	sched_verbose
	.align	4
sched_verbose:
	.long	0                       # 0x0
	.size	sched_verbose, 4

	.type	sched_dump,@object      # @sched_dump
	.globl	sched_dump
	.align	8
sched_dump:
	.quad	0
	.size	sched_dump, 8

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"verbose"
	.size	.L.str, 8

	.type	sched_verbose_param,@object # @sched_verbose_param
	.local	sched_verbose_param
	.comm	sched_verbose_param,4,4
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"fix_sched_param: unknown param: %s"
	.size	.L.str1, 35

	.type	h_i_d,@object           # @h_i_d
	.comm	h_i_d,8,8
	.type	unit_last_insn,@object  # @unit_last_insn
	.local	unit_last_insn
	.comm	unit_last_insn,624,16
	.type	unit_tick,@object       # @unit_tick
	.local	unit_tick
	.comm	unit_tick,312,16
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"haifa-sched.c"
	.size	.L.str2, 14

	.type	.L__FUNCTION__.rm_line_notes,@object # @__FUNCTION__.rm_line_notes
.L__FUNCTION__.rm_line_notes:
	.asciz	"rm_line_notes"
	.size	.L__FUNCTION__.rm_line_notes, 14

	.type	line_note_head,@object  # @line_note_head
	.local	line_note_head
	.comm	line_note_head,8,8
	.type	old_max_uid,@object     # @old_max_uid
	.local	old_max_uid
	.comm	old_max_uid,4,4
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	";; added %d line-number notes\n"
	.size	.L.str3, 31

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	";; deleted %d line-number notes\n"
	.size	.L.str4, 33

	.type	note_list,@object       # @note_list
	.local	note_list
	.comm	note_list,8,8
	.type	.L__FUNCTION__.rm_other_notes,@object # @__FUNCTION__.rm_other_notes
.L__FUNCTION__.rm_other_notes:
	.asciz	"rm_other_notes"
	.size	.L__FUNCTION__.rm_other_notes, 15

	.type	last_scheduled_insn,@object # @last_scheduled_insn
	.local	last_scheduled_insn
	.comm	last_scheduled_insn,8,8
	.type	current_sched_info,@object # @current_sched_info
	.comm	current_sched_info,8,8
	.type	.L__FUNCTION__.schedule_block,@object # @__FUNCTION__.schedule_block
.L__FUNCTION__.schedule_block:
	.asciz	"schedule_block"
	.size	.L__FUNCTION__.schedule_block, 15

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	";;   ======================================================\n"
	.size	.L.str5, 61

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	";;   -- basic block %d from %d to %d -- %s reload\n"
	.size	.L.str6, 51

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"after"
	.size	.L.str7, 6

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"before"
	.size	.L.str8, 7

	.type	issue_rate,@object      # @issue_rate
	.local	issue_rate
	.comm	issue_rate,4,4
	.type	q_ptr,@object           # @q_ptr
	.local	q_ptr
	.comm	q_ptr,4,4
	.type	q_size,@object          # @q_size
	.local	q_size
	.comm	q_size,4,4
	.type	last_clock_var,@object  # @last_clock_var
	.local	last_clock_var
	.comm	last_clock_var,4,4
	.type	insn_queue,@object      # @insn_queue
	.local	insn_queue
	.comm	insn_queue,1024,16
	.type	clock_var,@object       # @clock_var
	.local	clock_var
	.comm	clock_var,4,4
	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	";;\t\tReady list after queue_to_ready:  "
	.size	.L.str10, 39

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"\n;;\tReady list (t =%3d):  "
	.size	.L.str11, 27

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	";;\tReady list (final):  "
	.size	.L.str12, 25

	.type	.L.str13,@object        # @.str13
.L.str13:
	.zero	1
	.size	.L.str13, 1

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	";;   total time = %d\n;;   new head = %d\n"
	.size	.L.str14, 41

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	";;   new tail = %d\n\n"
	.size	.L.str15, 21

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	";;\t\t--> scheduling insn <<<%d>>> on unit "
	.size	.L.str16, 42

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	";;\t\tdependences resolved: insn %s "
	.size	.L.str17, 35

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"into ready\n"
	.size	.L.str18, 12

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"into queue with cost=%d\n"
	.size	.L.str19, 25

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	";;\t\tReady-->Q: insn %s: "
	.size	.L.str20, 25

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"queued for %d cycles.\n"
	.size	.L.str21, 23

	.type	.L__FUNCTION__.ready_remove_first,@object # @__FUNCTION__.ready_remove_first
.L__FUNCTION__.ready_remove_first:
	.asciz	"ready_remove_first"
	.size	.L__FUNCTION__.ready_remove_first, 19

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"  %s"
	.size	.L.str22, 5

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	";;\t\tQ-->Ready: insn %s: "
	.size	.L.str23, 25

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"moving to ready without stalls\n"
	.size	.L.str24, 32

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"moving to ready with %d stalls\n"
	.size	.L.str25, 32

	.type	.L__FUNCTION__.ready_lastpos,@object # @__FUNCTION__.ready_lastpos
.L__FUNCTION__.ready_lastpos:
	.asciz	"ready_lastpos"
	.size	.L__FUNCTION__.ready_lastpos, 14


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
