	.file	"sched-vis.c.bc"
	.text
	.globl	insn_print_units
	.align	16, 0x90
	.type	insn_print_units,@function
insn_print_units:                       # @insn_print_units
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
	callq	insn_unit
	movq	%rax, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
                                        # kill: RAX<def> RBX<kill>
	andq	%r14, %rax
	cmpq	%r14, %rax
	je	.LBB0_8
# BB#1:                                 # %if.else
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	sched_dump(%rip), %rsi
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_2
# BB#9:                                 # %if.then3
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	(%rbx,%rbx,8), %rax
	movq	function_units(,%rax,8), %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fputs  # TAILCALL
.LBB0_8:                                # %if.then
	movq	sched_dump(%rip), %rcx
	movabsq	$.L.str, %rdi
	movq	$4, %rsi
	movq	$1, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.LBB0_2:                                # %for.body.lr.ph
	movq	$91, %rdi
	callq	fputc
	notq	%rbx
	movabsq	$function_units, %r15
	.align	16, 0x90
.LBB0_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rbx
	je	.LBB0_6
# BB#4:                                 # %if.then8
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	sched_dump(%rip), %rsi
	movq	(%r15), %rdi
	callq	fputs
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	je	.LBB0_7
# BB#5:                                 # %if.then14
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	sched_dump(%rip), %rsi
	movq	$32, %rdi
	callq	fputc
.LBB0_6:                                # %for.inc
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$33, %rcx
	sarq	%cl, %rbx
	addq	$72, %r15
	testq	%r14, %rbx
	jne	.LBB0_3
.LBB0_7:                                # %for.end
	movq	sched_dump(%rip), %rsi
	movq	$93, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp10:
	.size	insn_print_units, .Ltmp10-insn_print_units
	.cfi_endproc

	.globl	init_target_units
	.align	16, 0x90
	.type	init_target_units,@function
init_target_units:                      # @init_target_units
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 16
.Ltmp14:
	.cfi_offset %rbx, -16
	callq	get_last_insn
	movq	%rax, %rbx
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_6:                                # %for.inc
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rbx), %rbx
.LBB1_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB1_7
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB1_6
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rbx, %rdi
	callq	insn_unit
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB1_5
# BB#4:                                 # %if.then5
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	target_units(%rip), %ecx
	notq	%rax
	orq	%rax, %rcx
	movl	%ecx, target_units(%rip)
	movq	16(%rbx), %rbx
	jmp	.LBB1_1
.LBB1_5:                                # %if.else
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	target_units(%rip), %edx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %rdx
	movl	%edx, target_units(%rip)
	jmp	.LBB1_6
.LBB1_7:                                # %for.end
	popq	%rbx
	retq
.Ltmp15:
	.size	init_target_units, .Ltmp15-init_target_units
	.cfi_endproc

	.globl	init_block_visualization
	.align	16, 0x90
	.type	init_block_visualization,@function
init_block_visualization:               # @init_block_visualization
	.cfi_startproc
# BB#0:                                 # %entry
	movq	visual_tbl(%rip), %rax
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movl	%ecx, n_visual_lines(%rip)
	movl	%ecx, n_vis_no_unit(%rip)
	retq
.Ltmp16:
	.size	init_block_visualization, .Ltmp16-init_block_visualization
	.cfi_endproc

	.globl	print_block_visualization
	.align	16, 0x90
	.type	print_block_visualization,@function
print_block_visualization:              # @print_block_visualization
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 48
.Ltmp28:
	.cfi_offset %rbx, -48
.Ltmp29:
	.cfi_offset %r12, -40
.Ltmp30:
	.cfi_offset %r13, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	sched_dump(%rip), %rcx
	movq	%rdi, %rdx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%rcx, %rdi
	callq	fprintf
	movq	sched_dump(%rip), %rdi
	movabsq	$.L.str7, %rsi
	movabsq	$.L.str8, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movabsq	$.L.str9, %r14
	.align	16, 0x90
.LBB3_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_4 Depth 2
	leaq	(%r12,%r12,8), %rax
	movl	function_units+8(,%rax,8), %ecx
	movl	target_units(%rip), %edx
	andq	%rcx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	je	.LBB3_5
# BB#2:                                 # %for.cond2.preheader
                                        #   in Loop: Header=BB3_1 Depth=1
	movslq	function_units+12(,%rax,8), %r13
	testq	%r13, %r13
	jle	.LBB3_5
# BB#3:                                 # %for.body6.lr.ph
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	function_units(,%rax,8), %r15
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB3_4:                                # %for.body6
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	sched_dump(%rip), %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	fprintf
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r13, %rax
	jl	.LBB3_4
.LBB3_5:                                # %for.inc10
                                        #   in Loop: Header=BB3_1 Depth=1
	incq	%r12
	cmpq	$26, %r12
	jne	.LBB3_1
# BB#6:                                 # %for.end12
	movq	sched_dump(%rip), %rdi
	movabsq	$.L.str10, %rsi
	movabsq	$.L.str11, %rdx
	xorq	%rax, %rax
	xorq	%r13, %r13
	callq	fprintf
	movq	sched_dump(%rip), %rdi
	movabsq	$.L.str7, %rsi
	movabsq	$.L.str12, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movabsq	$.L.str9, %r14
	movabsq	$.L.str13, %r12
	.align	16, 0x90
.LBB3_7:                                # %for.body17
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_9 Depth 2
	leaq	(%r13,%r13,8), %rax
	movl	function_units+8(,%rax,8), %ecx
	movl	target_units(%rip), %edx
	andq	%rcx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	je	.LBB3_10
# BB#8:                                 # %for.cond24.preheader
                                        #   in Loop: Header=BB3_7 Depth=1
	movslq	function_units+12(,%rax,8), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	jle	.LBB3_10
	.align	16, 0x90
.LBB3_9:                                # %for.body29
                                        #   Parent Loop BB3_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	sched_dump(%rip), %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	fprintf
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jl	.LBB3_9
.LBB3_10:                               # %for.inc35
                                        #   in Loop: Header=BB3_7 Depth=1
	incq	%r13
	cmpq	$26, %r13
	jne	.LBB3_7
# BB#11:                                # %for.end37
	movq	sched_dump(%rip), %rdi
	movabsq	$.L.str10, %rsi
	movabsq	$.L.str14, %rdx
	xorq	%rax, %rax
	callq	fprintf
	movq	sched_dump(%rip), %rdi
	movq	visual_tbl(%rip), %rdx
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.Ltmp33:
	.size	print_block_visualization, .Ltmp33-print_block_visualization
	.cfi_endproc

	.globl	visualize_no_unit
	.align	16, 0x90
	.type	visualize_no_unit,@function
visualize_no_unit:                      # @visualize_no_unit
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	n_vis_no_unit(%rip), %rax
	cmpq	$19, %rax
	jg	.LBB4_2
# BB#1:                                 # %if.then
	movq	%rdi, vis_no_unit(,%rax,8)
	incq	%rax
	movl	%eax, n_vis_no_unit(%rip)
.LBB4_2:                                # %if.end
	retq
.Ltmp34:
	.size	visualize_no_unit, .Ltmp34-visualize_no_unit
	.cfi_endproc

	.globl	visualize_scheduled_insns
	.align	16, 0x90
	.type	visualize_scheduled_insns,@function
visualize_scheduled_insns:              # @visualize_scheduled_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp41:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp43:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp44:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 48
	subq	$2080, %rsp             # imm = 0x820
.Ltmp46:
	.cfi_def_cfa_offset 2128
.Ltmp47:
	.cfi_offset %rbx, -48
.Ltmp48:
	.cfi_offset %r12, -40
.Ltmp49:
	.cfi_offset %r13, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movslq	n_visual_lines(%rip), %rax
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	cmpq	$100, %rax
	jl	.LBB5_2
# BB#1:                                 # %if.then
	movabsq	$.L.str16, %rdi
	callq	print_block_visualization
	movq	visual_tbl(%rip), %rcx
	xorq	%rax, %rax
	movb	%al, (%rcx)
	movl	%eax, n_visual_lines(%rip)
	movl	%eax, n_vis_no_unit(%rip)
.LBB5_2:                                # %if.end
	movq	visual_tbl(%rip), %rbx
	incq	%rax
	movl	%eax, n_visual_lines(%rip)
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	sprintf
	xorq	%r14, %r14
	.align	16, 0x90
.LBB5_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_5 Depth 2
	leaq	(%r14,%r14,8), %rax
	movl	function_units+8(,%rax,8), %ecx
	movl	target_units(%rip), %edx
	andq	%rcx, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	je	.LBB5_14
# BB#4:                                 # %for.cond4.preheader
                                        #   in Loop: Header=BB5_3 Depth=1
	movslq	function_units+12(,%rax,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	testq	%rax, %rax
	jle	.LBB5_14
	.align	16, 0x90
.LBB5_5:                                # %for.body8
                                        #   Parent Loop BB5_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	imulq	$26, %r13, %r15
	addq	%r14, %r15
	movq	%r15, %rdi
	callq	get_unit_last_insn
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB5_12
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_5 Depth=2
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	actual_hazard_this_instance
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_7
.LBB5_12:                               # %if.else
                                        #   in Loop: Header=BB5_5 Depth=2
	movq	visual_tbl(%rip), %rbx
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str9, %rsi
	movabsq	$.L.str18, %rdx
	jmp	.LBB5_13
	.align	16, 0x90
.LBB5_7:                                # %if.then13
                                        #   in Loop: Header=BB5_5 Depth=2
	leaq	32(%rsp), %r15
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	print_insn
	movq	visual_tbl(%rip), %rbx
	xorq	%rax, %rax
	movb	%al, 62(%rsp)
	movq	%rbx, %rdi
	callq	strlen
	addq	%rax, %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str9, %rsi
	movq	%r15, %rdx
.LBB5_13:                               # %for.inc
                                        #   in Loop: Header=BB5_5 Depth=2
	callq	sprintf
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB5_5
.LBB5_14:                               # %for.inc25
                                        #   in Loop: Header=BB5_3 Depth=1
	incq	%r14
	cmpq	$26, %r14
	jne	.LBB5_3
# BB#8:                                 # %for.cond28.preheader
	movslq	n_vis_no_unit(%rip), %rax
	testq	%rax, %rax
	jle	.LBB5_11
# BB#9:
	movq	$1, %rbx
	movabsq	$.L.str19, %r14
	.align	16, 0x90
.LBB5_10:                               # %for.body30
                                        # =>This Inner Loop Header: Depth=1
	movq	visual_tbl(%rip), %r15
	movq	%r15, %rdi
	callq	strlen
	movq	vis_no_unit-8(,%rbx,8), %rcx
	movl	8(%rcx), %edx
	addq	%rax, %r15
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	sprintf
	movslq	n_vis_no_unit(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB5_10
.LBB5_11:                               # %for.end39
	movq	visual_tbl(%rip), %rbx
	xorq	%rax, %rax
	movl	%eax, n_vis_no_unit(%rip)
	movq	%rbx, %rdi
	callq	strlen
	movq	$10, %rcx
	movw	%cx, (%rbx,%rax)
	addq	$2080, %rsp             # imm = 0x820
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	visualize_scheduled_insns, .Ltmp52-visualize_scheduled_insns
	.cfi_endproc

	.align	16, 0x90
	.type	print_insn,@function
print_insn:                             # @print_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp59:
	.cfi_def_cfa_offset 32
	subq	$2048, %rsp             # imm = 0x800
.Ltmp60:
	.cfi_def_cfa_offset 2080
.Ltmp61:
	.cfi_offset %rbx, -32
.Ltmp62:
	.cfi_offset %r14, -24
.Ltmp63:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movzbl	(%rbx), %ecx
	movq	%rdi, %r14
	addq	$-32, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	jbe	.LBB6_1
# BB#13:                                # %if.else91
	movl	8(%rbx), %edx
	movabsq	$.L.str34, %rsi
.LBB6_14:                               # %sw.epilog
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	jmp	.LBB6_15
.LBB6_1:                                # %entry
	jmpq	*.LJTI6_0(,%rax,8)
.LBB6_2:                                # %sw.bb7
	movq	32(%rbx), %rsi
	leaq	(%rsp), %r15
	movq	%r15, %rdi
	callq	print_pattern
	movl	8(%rbx), %edx
	movabsq	$.L.str26, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rcx
	callq	sprintf
	jmp	.LBB6_15
.LBB6_3:                                # %sw.bb25
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	jne	.LBB6_5
# BB#4:                                 # %if.then31
	movq	8(%rax), %rax
	movq	8(%rax), %rsi
	leaq	(%rsp), %rdi
	callq	print_pattern
	jmp	.LBB6_6
.LBB6_9:                                # %sw.bb58
	movl	8(%rbx), %edx
	movabsq	$.L.str30, %rsi
	jmp	.LBB6_14
.LBB6_8:                                # %sw.bb53
	movl	8(%rbx), %edx
	movabsq	$.L.str29, %rsi
	jmp	.LBB6_14
.LBB6_10:                               # %sw.bb63
	movslq	40(%rbx), %r8
	movl	8(%rbx), %edx
	testq	%r8, %r8
	jle	.LBB6_12
# BB#11:                                # %if.then68
	movq	32(%rbx), %rcx
	movabsq	$.L.str31, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	jmp	.LBB6_15
.LBB6_5:                                # %if.else36
	xorq	%rax, %rax
	movb	%al, 10(%rsp)
	movq	$15918, %rax            # imm = 0x3E2E
	movw	%ax, 8(%rsp)
	movabsq	$3327663284629037411, %rax # imm = 0x2E2E3C206C6C6163
	movq	%rax, (%rsp)
.LBB6_6:                                # %if.else46
	movl	8(%rbx), %edx
	movabsq	$.L.str26, %rsi
	leaq	(%rsp), %rcx
	jmp	.LBB6_7
.LBB6_12:                               # %if.else78
	addq	$100, %r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	note_insn_name(,%r8,8), %rcx
	movabsq	$.L.str32, %rsi
.LBB6_7:                                # %sw.epilog
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
.LBB6_15:                               # %sw.epilog
	addq	$2048, %rsp             # imm = 0x800
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp64:
	.size	print_insn, .Ltmp64-print_insn
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI6_0:
	.quad	.LBB6_2
	.quad	.LBB6_2
	.quad	.LBB6_3
	.quad	.LBB6_9
	.quad	.LBB6_8
	.quad	.LBB6_10

	.text
	.globl	visualize_stall_cycles
	.align	16, 0x90
	.type	visualize_stall_cycles,@function
visualize_stall_cycles:                 # @visualize_stall_cycles
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp72:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp74:
	.cfi_def_cfa_offset 48
.Ltmp75:
	.cfi_offset %rbx, -40
.Ltmp76:
	.cfi_offset %r12, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movslq	n_visual_lines(%rip), %rax
	movq	%rdi, %r15
	cmpq	$100, %rax
	jl	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$.L.str16, %rdi
	callq	print_block_visualization
	movq	visual_tbl(%rip), %rcx
	xorq	%rax, %rax
	movb	%al, (%rcx)
	movl	%eax, n_visual_lines(%rip)
	movl	%eax, n_vis_no_unit(%rip)
.LBB7_2:                                # %if.end
	movq	visual_tbl(%rip), %r12
	incq	%rax
	movl	%eax, n_visual_lines(%rip)
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r14
	movq	$32, %rax
	movw	%ax, 8(%r12,%r14)
	movabsq	$2314885530818460475, %rax # imm = 0x2020202020203B3B
	movq	%rax, (%r12,%r14)
	movl	visual_tbl_line_length(%rip), %ebx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	leaq	-10(%rbx), %rcx
	addq	$-15, %rbx
	cmpq	%rcx, %rax
	ja	.LBB7_4
# BB#3:                                 # %if.end
	movq	%r15, %rbx
.LBB7_4:                                # %if.end
	leaq	9(%r12,%r14), %rdi
	ja	.LBB7_5
# BB#6:                                 # %if.end
	movabsq	$.L.str20, %r15
	jmp	.LBB7_7
.LBB7_5:
	movabsq	$.L.str22, %r15
.LBB7_7:                                # %if.end
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$46, %rsi
	movq	%rbx, %rdx
	callq	memset
	leaq	9(%rbx,%r14), %rax
	addq	%rax, %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	strcpy  # TAILCALL
.Ltmp79:
	.size	visualize_stall_cycles, .Ltmp79-visualize_stall_cycles
	.cfi_endproc

	.globl	visualize_alloc
	.align	16, 0x90
	.type	visualize_alloc,@function
visualize_alloc:                        # @visualize_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 16
	subq	$48, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 64
.Ltmp84:
	.cfi_offset %rbx, -16
	movabsq	$.L.str23, %rsi
	movabsq	$.L.str24, %rdx
	leaq	12(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	movl	target_units(%rip), %esi
	leaq	3(%rax), %r8
	movq	$-1872, %rdi            # imm = 0xFFFFFFFFFFFFF8B0
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r8, %rdx
	.align	16, 0x90
.LBB8_1:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	function_units+1880(%rdi), %ecx
	andq	%rsi, %rcx
	testq	%rbx, %rcx
	je	.LBB8_6
# BB#2:                                 # %for.cond3.preheader.i
                                        #   in Loop: Header=BB8_1 Depth=1
	movslq	function_units+1884(%rdi), %rcx
	testq	%rcx, %rcx
	jle	.LBB8_6
# BB#3:                                 # %for.body8.lr.ph.i
                                        #   in Loop: Header=BB8_1 Depth=1
	jg	.LBB8_5
# BB#4:                                 # %for.body8.lr.ph.i
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	$1, %rcx
.LBB8_5:                                # %for.body8.lr.ph.i
                                        #   in Loop: Header=BB8_1 Depth=1
	imulq	%rax, %rcx
	addq	%rcx, %rdx
.LBB8_6:                                # %for.inc10.i
                                        #   in Loop: Header=BB8_1 Depth=1
	addq	$72, %rdi
	jne	.LBB8_1
# BB#7:                                 # %get_visual_tbl_length.exit
	addq	%r8, %rdx
	movl	%edx, visual_tbl_line_length(%rip)
	imulq	$100, %rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	xmalloc
	movq	%rax, visual_tbl(%rip)
	addq	$48, %rsp
	popq	%rbx
	retq
.Ltmp85:
	.size	visualize_alloc, .Ltmp85-visualize_alloc
	.cfi_endproc

	.globl	visualize_free
	.align	16, 0x90
	.type	visualize_free,@function
visualize_free:                         # @visualize_free
	.cfi_startproc
# BB#0:                                 # %entry
	movq	visual_tbl(%rip), %rdi
	jmp	free  # TAILCALL
.Ltmp86:
	.size	visualize_free, .Ltmp86-visualize_free
	.cfi_endproc

	.align	16, 0x90
	.type	print_pattern,@function
print_pattern:                          # @print_pattern
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
	subq	$6160, %rsp             # imm = 0x1810
.Ltmp98:
	.cfi_def_cfa_offset 6208
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
	movq	%rsi, %rbx
	movzbl	(%rbx), %ecx
	movq	%rdi, %r15
	addq	$-24, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$28, %rax
	ja	.LBB10_35
# BB#1:                                 # %entry
	jmpq	*.LJTI10_0(,%rax,8)
.LBB10_22:                              # %sw.bb100
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str43, %rsi
	leaq	4112(%rsp), %rdi
	xorq	%rax, %rax
	callq	sprintf
	movq	8(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB10_25
# BB#23:
	movq	$1, %r12
	leaq	2064(%rsp), %r15
	leaq	16(%rsp), %r13
	leaq	4112(%rsp), %r14
	.align	16, 0x90
.LBB10_24:                              # %for.body110
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rsi
	movq	%r15, %rdi
	callq	print_insn
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str41, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	sprintf
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	strcpy
	movq	8(%rbx), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB10_24
.LBB10_25:                              # %for.end127
	movabsq	$.L.str44, %rsi
	jmp	.LBB10_21
.LBB10_35:                              # %sw.default
	movq	%r15, %rdi
	movq	%rbx, %rsi
.LBB10_36:                              # %sw.epilog
	callq	print_value
	jmp	.LBB10_37
.LBB10_8:                               # %sw.bb24
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$103, %rax
	jne	.LBB10_9
# BB#13:                                # %land.lhs.true50
	movq	16(%rsi), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB10_12
# BB#14:                                # %if.then58
	movq	$33, %rax
	movb	%al, 4112(%rsp)
	movq	8(%rsi), %rsi
	leaq	4113(%rsp), %rdi
	jmp	.LBB10_15
.LBB10_17:                              # %sw.bb80
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	$123, %rax
	movw	%ax, 4112(%rsp)
	movq	8(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB10_20
# BB#18:
	movq	$1, %r12
	leaq	2064(%rsp), %r15
	leaq	16(%rsp), %r13
	leaq	4112(%rsp), %r14
	.align	16, 0x90
.LBB10_19:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rsi
	movq	%r15, %rdi
	callq	print_pattern
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str41, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	sprintf
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	strcpy
	movq	8(%rbx), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB10_19
	jmp	.LBB10_20
.LBB10_26:                              # %sw.bb130
	movq	8(%rbx), %rdx
	movabsq	$.L.str45, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	sprintf
	jmp	.LBB10_37
.LBB10_29:                              # %sw.bb145
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movabsq	$34730709464870517, %rax # imm = 0x7B636570736E75
	movq	%rax, 4112(%rsp)
	movq	8(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB10_20
# BB#30:
	movq	$1, %r12
	leaq	2064(%rsp), %r15
	leaq	16(%rsp), %r13
	leaq	4112(%rsp), %r14
	.align	16, 0x90
.LBB10_31:                              # %for.body155
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rsi
	movq	%r15, %rdi
	callq	print_pattern
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str41, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	sprintf
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	strcpy
	movq	8(%rbx), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB10_31
	jmp	.LBB10_20
.LBB10_32:                              # %sw.bb175
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	$123, %rax
	movw	%ax, 4120(%rsp)
	movabsq	$8516134707710357109, %rax # imm = 0x762F636570736E75
	movq	%rax, 4112(%rsp)
	movq	8(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB10_20
# BB#33:
	movq	$1, %r12
	leaq	2064(%rsp), %r15
	leaq	16(%rsp), %r13
	leaq	4112(%rsp), %r14
	.align	16, 0x90
.LBB10_34:                              # %for.body185
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rsi
	movq	%r15, %rdi
	callq	print_pattern
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str41, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	sprintf
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	strcpy
	movq	8(%rbx), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB10_34
.LBB10_20:                              # %for.end
	movabsq	$.L.str42, %rsi
.LBB10_21:                              # %sw.epilog
	leaq	4112(%rsp), %rdx
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	sprintf
	jmp	.LBB10_37
.LBB10_27:                              # %sw.bb134
	movq	8(%rbx), %rsi
	movq	%r15, %rdi
	jmp	.LBB10_36
.LBB10_2:                               # %sw.bb
	movq	8(%rbx), %rsi
	leaq	4112(%rsp), %r14
	movq	%r14, %rdi
	callq	print_value
	movq	16(%rbx), %rsi
	leaq	2064(%rsp), %rbx
	movq	%rbx, %rdi
	callq	print_value
	movabsq	$.L.str35, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rdx
	jmp	.LBB10_16
.LBB10_7:                               # %sw.bb17
	movq	8(%rbx), %rsi
	leaq	4112(%rsp), %rbx
	movq	%rbx, %rdi
	callq	print_value
	movabsq	$.L.str38, %rsi
	jmp	.LBB10_6
.LBB10_5:                               # %sw.bb10
	movq	8(%rbx), %rsi
	leaq	4112(%rsp), %rbx
	movq	%rbx, %rdi
	callq	print_value
	movabsq	$.L.str37, %rsi
	jmp	.LBB10_6
.LBB10_4:                               # %sw.bb9
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	print_exp
	jmp	.LBB10_37
.LBB10_3:                               # %sw.bb7
	xorq	%rax, %rax
	movb	%al, 6(%r15)
	movq	$28274, %rax            # imm = 0x6E72
	movw	%ax, 4(%r15)
	movq	$1970562418, %rax       # imm = 0x75746572
	movl	%eax, (%r15)
	jmp	.LBB10_37
.LBB10_28:                              # %sw.bb138
	movq	8(%rbx), %rsi
	leaq	4112(%rsp), %rbx
	movq	%rbx, %rdi
	callq	print_value
	movabsq	$.L.str46, %rsi
.LBB10_6:                               # %sw.epilog
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	sprintf
	jmp	.LBB10_37
.LBB10_9:                               # %sw.bb24
	cmpq	$102, %rax
	jne	.LBB10_12
# BB#10:                                # %land.lhs.true
	movq	16(%rsi), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB10_12
# BB#11:                                # %if.then
	movq	8(%rsi), %rsi
.LBB10_12:                              # %if.else67
	leaq	4112(%rsp), %rdi
.LBB10_15:                              # %if.end72
	callq	print_value
	movq	16(%rbx), %rsi
	leaq	2064(%rsp), %rbx
	movq	%rbx, %rdi
	callq	print_pattern
	movabsq	$.L.str39, %rsi
	leaq	4112(%rsp), %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
.LBB10_16:                              # %sw.epilog
	movq	%rbx, %rcx
	callq	sprintf
.LBB10_37:                              # %sw.epilog
	addq	$6160, %rsp             # imm = 0x1810
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	print_pattern, .Ltmp104-print_pattern
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI10_0:
	.quad	.LBB10_22
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_35
	.quad	.LBB10_8
	.quad	.LBB10_17
	.quad	.LBB10_26
	.quad	.LBB10_35
	.quad	.LBB10_29
	.quad	.LBB10_32
	.quad	.LBB10_37
	.quad	.LBB10_27
	.quad	.LBB10_35
	.quad	.LBB10_2
	.quad	.LBB10_7
	.quad	.LBB10_5
	.quad	.LBB10_4
	.quad	.LBB10_3
	.quad	.LBB10_28

	.text
	.align	16, 0x90
	.type	print_value,@function
print_value:                            # @print_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp113:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 48
	subq	$2048, %rsp             # imm = 0x800
.Ltmp116:
	.cfi_def_cfa_offset 2096
.Ltmp117:
	.cfi_offset %rbx, -48
.Ltmp118:
	.cfi_offset %r12, -40
.Ltmp119:
	.cfi_offset %r13, -32
.Ltmp120:
	.cfi_offset %r14, -24
.Ltmp121:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	(%r14), %eax
	movq	%rdi, %r15
	cmpq	$133, %rax
	jg	.LBB11_68
# BB#1:                                 # %entry
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$15, %rcx
	ja	.LBB11_143
# BB#2:                                 # %entry
	jmpq	*.LJTI11_0(,%rcx,8)
.LBB11_3:                               # %sw.bb
	movq	8(%r14), %rdx
	leaq	(%rsp), %r14
	movabsq	$.L.str122, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	leaq	2046(%r15), %rax
	.align	16, 0x90
.LBB11_4:                               # %land.rhs.i18
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_5
# BB#6:                                 # %while.body.i22
                                        #   in Loop: Header=BB11_4 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_4
	jmp	.LBB11_147
.LBB11_68:                              # %entry
	cmpq	$134, %rax
	jne	.LBB11_143
# BB#69:                                # %sw.bb45
	movq	8(%r14), %rsi
	leaq	(%rsp), %r14
	movq	%r14, %rdi
	callq	print_value
	leaq	2046(%r15), %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB11_70:                              # %land.rhs.i235
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str129(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB11_72
# BB#71:                                # %while.body.i239
                                        #   in Loop: Header=BB11_70 Depth=1
	movb	%dl, (%r15,%rcx)
	leaq	1(%r15,%rcx), %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jb	.LBB11_70
.LBB11_72:                              # %land.rhs.i235.safe_concat.exit243_crit_edge
	addq	%rcx, %r15
	xorq	%rcx, %rcx
	cmpq	%r15, %rax
	movb	%cl, (%r15)
	jae	.LBB11_73
# BB#75:                                # %if.then.i248
	movb	%cl, (%rax)
	movq	%rax, %rcx
	jmp	.LBB11_80
.LBB11_143:                             # %sw.default
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	print_exp
	leaq	2046(%r15), %rax
	.align	16, 0x90
.LBB11_144:                             # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %edx
	testq	%rdx, %rdx
	je	.LBB11_145
# BB#146:                               # %while.body.i
                                        #   in Loop: Header=BB11_144 Depth=1
	incq	%rbx
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_144
	jmp	.LBB11_147
.LBB11_145:
	movq	%r15, %rcx
	jmp	.LBB11_147
.LBB11_73:                              # %while.cond.preheader.i247
	jbe	.LBB11_74
	.align	16, 0x90
.LBB11_76:                              # %land.rhs.i252
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_77
# BB#78:                                # %while.body.i256
                                        #   in Loop: Header=BB11_76 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_76
	jmp	.LBB11_79
.LBB11_74:
	movq	%r15, %rcx
	jmp	.LBB11_79
.LBB11_77:
	movq	%r15, %rcx
.LBB11_79:                              # %while.end.i258
	xorq	%rdx, %rdx
	movb	%dl, (%rcx)
.LBB11_80:                              # %safe_concat.exit260
	cmpq	%rcx, %rax
	jb	.LBB11_113
# BB#81:                                # %while.cond.preheader.i264
	cmpq	%rcx, %rax
	jbe	.LBB11_147
# BB#82:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB11_83:                              # %land.rhs.i269
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str52(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_85
# BB#84:                                # %while.body.i273
                                        #   in Loop: Header=BB11_83 Depth=1
	movb	%sil, (%rcx,%rdx)
	leaq	1(%rcx,%rdx), %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jb	.LBB11_83
.LBB11_85:                              # %land.rhs.i269.while.end.i275.loopexit_crit_edge
	addq	%rdx, %rcx
.LBB11_147:                             # %safe_concat.exit
	xorq	%rax, %rax
	movb	%al, (%rcx)
	jmp	.LBB11_148
.LBB11_7:                               # %sw.bb3
	movq	24(%r14), %rdx
	movq	32(%r14), %rcx
	leaq	(%rsp), %r14
	movabsq	$.L.str123, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	leaq	2046(%r15), %rax
	.align	16, 0x90
.LBB11_8:                               # %land.rhs.i32
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_9
# BB#10:                                # %while.body.i36
                                        #   in Loop: Header=BB11_8 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_8
	jmp	.LBB11_147
.LBB11_11:                              # %sw.bb14
	leaq	2046(%r15), %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB11_12:                              # %land.rhs.i46
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str124(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB11_14
# BB#13:                                # %while.body.i50
                                        #   in Loop: Header=BB11_12 Depth=1
	movb	%dl, (%r15,%rcx)
	leaq	1(%r15,%rcx), %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jb	.LBB11_12
.LBB11_14:                              # %land.rhs.i46.safe_concat.exit54_crit_edge
	addq	%rcx, %r15
	xorq	%rcx, %rcx
	cmpq	%r15, %rax
	movb	%cl, (%r15)
	jae	.LBB11_15
# BB#17:                                # %if.then.i59
	movb	%cl, (%rax)
	movq	%rax, %rcx
	jmp	.LBB11_23
.LBB11_51:                              # %sw.bb36
	movq	8(%r14), %rsi
	leaq	(%rsp), %r14
	movq	%r14, %rdi
	callq	print_value
	leaq	2046(%r15), %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB11_52:                              # %land.rhs.i173
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str128(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB11_54
# BB#53:                                # %while.body.i177
                                        #   in Loop: Header=BB11_52 Depth=1
	movb	%dl, (%r15,%rcx)
	leaq	1(%r15,%rcx), %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jb	.LBB11_52
.LBB11_54:                              # %land.rhs.i173.safe_concat.exit181_crit_edge
	addq	%rcx, %r15
	xorq	%rcx, %rcx
	cmpq	%r15, %rax
	movb	%cl, (%r15)
	jae	.LBB11_55
# BB#57:                                # %if.then.i186
	movb	%cl, (%rax)
	movq	%rax, %rcx
	jmp	.LBB11_62
.LBB11_122:                             # %sw.bb100
	leaq	2046(%r15), %rcx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB11_123:                             # %land.rhs.i328
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str134(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB11_125
# BB#124:                               # %while.body.i332
                                        #   in Loop: Header=BB11_123 Depth=1
	movb	%dl, (%r15,%rax)
	leaq	1(%r15,%rax), %rdx
	incq	%rax
	cmpq	%rcx, %rdx
	jb	.LBB11_123
.LBB11_125:                             # %land.rhs.i328.safe_concat.exit336_crit_edge
	addq	%rax, %r15
	jmp	.LBB11_97
.LBB11_86:                              # %sw.bb54
	movl	8(%r14), %edx
	cmpq	$52, %rdx
	ja	.LBB11_98
# BB#87:                                # %if.then
	movq	reg_names(,%rdx,8), %rax
	movzbl	(%rax), %eax
	movzbl	_sch_istable(%rax,%rax), %ecx
	leaq	2046(%r15), %rax
	testq	$4, %rcx
	je	.LBB11_93
# BB#88:
	xorq	%rcx, %rcx
.LBB11_89:                              # %land.rhs.i283
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str57(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB11_91
# BB#90:                                # %while.body.i287
                                        #   in Loop: Header=BB11_89 Depth=1
	movb	%dl, (%r15,%rcx)
	leaq	1(%r15,%rcx), %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jb	.LBB11_89
.LBB11_91:                              # %land.rhs.i283.if.end_crit_edge
	addq	%rcx, %r15
	xorq	%rcx, %rcx
	cmpq	%r15, %rax
	movb	%cl, (%r15)
	jae	.LBB11_92
# BB#149:                               # %if.then.i296
	movb	%cl, (%rax)
	jmp	.LBB11_148
.LBB11_114:                             # %sw.bb96
	leaq	2046(%r15), %rcx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB11_115:                             # %land.rhs.i373
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str132(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB11_117
# BB#116:                               # %while.body.i377
                                        #   in Loop: Header=BB11_115 Depth=1
	movb	%dl, (%r15,%rax)
	leaq	1(%r15,%rax), %rdx
	incq	%rax
	cmpq	%rcx, %rdx
	jb	.LBB11_115
.LBB11_117:                             # %land.rhs.i373.safe_concat.exit381_crit_edge
	addq	%rax, %r15
	jmp	.LBB11_97
.LBB11_102:                             # %sw.bb82
	movq	8(%r14), %rsi
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	callq	print_value
	leaq	2046(%r15), %r12
	.align	16, 0x90
.LBB11_103:                             # %land.rhs.i342
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB11_104
# BB#105:                               # %while.body.i346
                                        #   in Loop: Header=BB11_103 Depth=1
	incq	%rbx
	leaq	1(%r15), %r13
	cmpq	%r12, %r13
	movb	%al, (%r15)
	movq	%r13, %r15
	jb	.LBB11_103
	jmp	.LBB11_106
.LBB11_126:                             # %sw.bb102
	movq	8(%r14), %rsi
	leaq	(%rsp), %r14
	movq	%r14, %rdi
	callq	print_value
	leaq	2046(%r15), %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB11_127:                             # %land.rhs.i221
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str2(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB11_129
# BB#128:                               # %while.body.i225
                                        #   in Loop: Header=BB11_127 Depth=1
	movb	%dl, (%r15,%rcx)
	leaq	1(%r15,%rcx), %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jb	.LBB11_127
.LBB11_129:                             # %land.rhs.i221.safe_concat.exit229_crit_edge
	addq	%rcx, %r15
	xorq	%rcx, %rcx
	cmpq	%r15, %rax
	movb	%cl, (%r15)
	jae	.LBB11_130
# BB#132:                               # %if.then.i155
	movb	%cl, (%rax)
	movq	%rax, %rcx
	jmp	.LBB11_137
.LBB11_47:                              # %sw.bb27
	movq	8(%r14), %rax
	movl	8(%rax), %edx
	leaq	(%rsp), %r14
	movabsq	$.L.str127, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	leaq	2046(%r15), %rax
	.align	16, 0x90
.LBB11_48:                              # %land.rhs.i142
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_49
# BB#50:                                # %while.body.i146
                                        #   in Loop: Header=BB11_48 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_48
	jmp	.LBB11_147
.LBB11_29:                              # %sw.bb20
	leaq	2046(%r15), %rax
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB11_30:                              # %land.rhs.i94
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str125(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB11_32
# BB#31:                                # %while.body.i98
                                        #   in Loop: Header=BB11_30 Depth=1
	movb	%dl, (%r15,%rcx)
	leaq	1(%r15,%rcx), %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jb	.LBB11_30
.LBB11_32:                              # %land.rhs.i94.safe_concat.exit102_crit_edge
	addq	%rcx, %r15
	xorq	%rcx, %rcx
	cmpq	%r15, %rax
	movb	%cl, (%r15)
	jae	.LBB11_33
# BB#35:                                # %if.then.i107
	movb	%cl, (%rax)
	movq	%rax, %rcx
	jmp	.LBB11_41
.LBB11_118:                             # %sw.bb98
	leaq	2046(%r15), %rcx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB11_119:                             # %land.rhs.i387
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str133(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB11_121
# BB#120:                               # %while.body.i391
                                        #   in Loop: Header=BB11_119 Depth=1
	movb	%dl, (%r15,%rax)
	leaq	1(%r15,%rax), %rdx
	incq	%rax
	cmpq	%rcx, %rdx
	jb	.LBB11_119
.LBB11_121:                             # %land.rhs.i387.safe_concat.exit395_crit_edge
	addq	%rax, %r15
.LBB11_97:                              # %while.end.i306
	xorq	%rax, %rax
	movb	%al, (%r15)
	jmp	.LBB11_148
.LBB11_5:
	movq	%r15, %rcx
	jmp	.LBB11_147
.LBB11_9:
	movq	%r15, %rcx
	jmp	.LBB11_147
.LBB11_104:
	movq	%r15, %r13
.LBB11_106:                             # %safe_concat.exit350
	xorq	%r15, %r15
	movb	%r15b, (%r13)
	movl	16(%r14), %edx
	movabsq	$.L.str131, %rsi
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	sprintf
	cmpq	%r13, %r12
	jae	.LBB11_107
# BB#109:                               # %if.then.i355
	movb	%r15b, (%r12)
	jmp	.LBB11_148
.LBB11_49:
	movq	%r15, %rcx
	jmp	.LBB11_147
.LBB11_15:                              # %while.cond.preheader.i58
	jbe	.LBB11_16
# BB#18:                                # %land.rhs.i63.preheader
	movq	8(%r14), %rdx
.LBB11_19:                              # %land.rhs.i63
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_20
# BB#21:                                # %while.body.i67
                                        #   in Loop: Header=BB11_19 Depth=1
	incq	%rdx
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%sil, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_19
	jmp	.LBB11_22
.LBB11_55:                              # %while.cond.preheader.i185
	jbe	.LBB11_56
.LBB11_58:                              # %land.rhs.i190
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_59
# BB#60:                                # %while.body.i194
                                        #   in Loop: Header=BB11_58 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_58
	jmp	.LBB11_61
.LBB11_98:                              # %if.else
	leaq	(%rsp), %r14
	movabsq	$.L.str130, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	sprintf
	leaq	2046(%r15), %rax
.LBB11_99:                              # %land.rhs.i314
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_100
# BB#101:                               # %while.body.i318
                                        #   in Loop: Header=BB11_99 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_99
	jmp	.LBB11_147
.LBB11_130:                             # %while.cond.preheader.i154
	jbe	.LBB11_131
.LBB11_133:                             # %land.rhs.i159
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB11_134
# BB#135:                               # %while.body.i163
                                        #   in Loop: Header=BB11_133 Depth=1
	incq	%r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%dl, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_133
	jmp	.LBB11_136
.LBB11_33:                              # %while.cond.preheader.i106
	jbe	.LBB11_34
# BB#36:                                # %land.rhs.i111.preheader
	movq	8(%r14), %rdx
.LBB11_37:                              # %land.rhs.i111
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_38
# BB#39:                                # %while.body.i115
                                        #   in Loop: Header=BB11_37 Depth=1
	incq	%rdx
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movb	%sil, (%r15)
	movq	%rcx, %r15
	jb	.LBB11_37
	jmp	.LBB11_40
.LBB11_107:                             # %while.cond.preheader.i354
	jbe	.LBB11_108
.LBB11_110:                             # %land.rhs.i359
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB11_111
# BB#112:                               # %while.body.i363
                                        #   in Loop: Header=BB11_110 Depth=1
	incq	%rbx
	leaq	1(%r13), %rax
	cmpq	%r12, %rax
	movb	%cl, (%r13)
	movq	%rax, %r13
	jb	.LBB11_110
	jmp	.LBB11_113
.LBB11_100:
	movq	%r15, %rcx
	jmp	.LBB11_147
.LBB11_16:
	movq	%r15, %rcx
	jmp	.LBB11_22
.LBB11_56:
	movq	%r15, %rcx
	jmp	.LBB11_61
.LBB11_131:
	movq	%r15, %rcx
	jmp	.LBB11_136
.LBB11_34:
	movq	%r15, %rcx
	jmp	.LBB11_40
.LBB11_108:
	movq	%r13, %rax
	jmp	.LBB11_113
.LBB11_92:                              # %while.cond.preheader.i295
	jbe	.LBB11_97
.LBB11_93:                              # %land.rhs.i300.preheader
	movl	8(%r14), %ecx
	movq	reg_names(,%rcx,8), %rcx
	movq	%r15, %rdx
.LBB11_94:                              # %land.rhs.i300
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx), %esi
	testq	%rsi, %rsi
	je	.LBB11_95
# BB#96:                                # %while.body.i304
                                        #   in Loop: Header=BB11_94 Depth=1
	incq	%rcx
	leaq	1(%rdx), %r15
	cmpq	%rax, %r15
	movb	%sil, (%rdx)
	movq	%r15, %rdx
	jb	.LBB11_94
	jmp	.LBB11_97
.LBB11_95:
	movq	%rdx, %r15
	jmp	.LBB11_97
.LBB11_20:
	movq	%r15, %rcx
.LBB11_22:                              # %while.end.i69
	xorq	%rdx, %rdx
	movb	%dl, (%rcx)
.LBB11_23:                              # %safe_concat.exit71
	cmpq	%rcx, %rax
	jb	.LBB11_113
# BB#24:                                # %while.cond.preheader.i75
	cmpq	%rcx, %rax
	jbe	.LBB11_147
# BB#25:
	xorq	%rdx, %rdx
.LBB11_26:                              # %land.rhs.i80
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str124(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_28
# BB#27:                                # %while.body.i84
                                        #   in Loop: Header=BB11_26 Depth=1
	movb	%sil, (%rcx,%rdx)
	leaq	1(%rcx,%rdx), %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jb	.LBB11_26
.LBB11_28:                              # %land.rhs.i80.while.end.i86.loopexit_crit_edge
	addq	%rdx, %rcx
	jmp	.LBB11_147
.LBB11_59:
	movq	%r15, %rcx
.LBB11_61:                              # %while.end.i196
	xorq	%rdx, %rdx
	movb	%dl, (%rcx)
.LBB11_62:                              # %safe_concat.exit198
	cmpq	%rcx, %rax
	jb	.LBB11_113
# BB#63:                                # %while.cond.preheader.i202
	cmpq	%rcx, %rax
	jbe	.LBB11_147
# BB#64:
	xorq	%rdx, %rdx
.LBB11_65:                              # %land.rhs.i207
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str52(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_67
# BB#66:                                # %while.body.i211
                                        #   in Loop: Header=BB11_65 Depth=1
	movb	%sil, (%rcx,%rdx)
	leaq	1(%rcx,%rdx), %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jb	.LBB11_65
.LBB11_67:                              # %land.rhs.i207.while.end.i213.loopexit_crit_edge
	addq	%rdx, %rcx
	jmp	.LBB11_147
.LBB11_134:
	movq	%r15, %rcx
.LBB11_136:                             # %while.end.i165
	xorq	%rdx, %rdx
	movb	%dl, (%rcx)
.LBB11_137:                             # %safe_concat.exit167
	cmpq	%rcx, %rax
	jb	.LBB11_113
# BB#138:                               # %while.cond.preheader.i
	cmpq	%rcx, %rax
	jbe	.LBB11_147
# BB#139:
	xorq	%rdx, %rdx
.LBB11_140:                             # %land.rhs.i6
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str4(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_142
# BB#141:                               # %while.body.i10
                                        #   in Loop: Header=BB11_140 Depth=1
	movb	%sil, (%rcx,%rdx)
	leaq	1(%rcx,%rdx), %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jb	.LBB11_140
.LBB11_142:                             # %land.rhs.i6.while.end.i.loopexit_crit_edge
	addq	%rdx, %rcx
	jmp	.LBB11_147
.LBB11_38:
	movq	%r15, %rcx
.LBB11_40:                              # %while.end.i117
	xorq	%rdx, %rdx
	movb	%dl, (%rcx)
.LBB11_41:                              # %safe_concat.exit119
	cmpq	%rcx, %rax
	jb	.LBB11_113
# BB#42:                                # %while.cond.preheader.i123
	cmpq	%rcx, %rax
	jbe	.LBB11_147
# BB#43:
	xorq	%rdx, %rdx
.LBB11_44:                              # %land.rhs.i128
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str126(%rdx), %esi
	testq	%rsi, %rsi
	je	.LBB11_46
# BB#45:                                # %while.body.i132
                                        #   in Loop: Header=BB11_44 Depth=1
	movb	%sil, (%rcx,%rdx)
	leaq	1(%rcx,%rdx), %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jb	.LBB11_44
.LBB11_46:                              # %land.rhs.i128.while.end.i134.loopexit_crit_edge
	addq	%rdx, %rcx
	jmp	.LBB11_147
.LBB11_111:
	movq	%r13, %rax
.LBB11_113:                             # %if.then.i76
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
.LBB11_148:                             # %sw.epilog
	addq	$2048, %rsp             # imm = 0x800
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp122:
	.size	print_value, .Ltmp122-print_value
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI11_0:
	.quad	.LBB11_3
	.quad	.LBB11_7
	.quad	.LBB11_143
	.quad	.LBB11_11
	.quad	.LBB11_51
	.quad	.LBB11_122
	.quad	.LBB11_143
	.quad	.LBB11_86
	.quad	.LBB11_114
	.quad	.LBB11_102
	.quad	.LBB11_143
	.quad	.LBB11_143
	.quad	.LBB11_126
	.quad	.LBB11_47
	.quad	.LBB11_29
	.quad	.LBB11_118

	.text
	.align	16, 0x90
	.type	print_exp,@function
print_exp:                              # @print_exp
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
	subq	$2128, %rsp             # imm = 0x850
.Ltmp134:
	.cfi_def_cfa_offset 2176
.Ltmp135:
	.cfi_offset %rbx, -48
.Ltmp136:
	.cfi_offset %r12, -40
.Ltmp137:
	.cfi_offset %r13, -32
.Ltmp138:
	.cfi_offset %r14, -24
.Ltmp139:
	.cfi_offset %r15, -16
	movq	$0, 72(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	movq	$0, 24(%rsp)
	movq	$0, 16(%rsp)
	movzbl	(%rsi), %eax
	movq	%rdi, %r12
	leaq	-42(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$93, %rcx
	ja	.LBB12_110
# BB#1:                                 # %entry
	jmpq	*.LJTI12_0(,%rcx,8)
.LBB12_62:                              # %sw.bb515
	leaq	2046(%r12), %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB12_63:                              # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str116(%rbx), %eax
	testq	%rax, %rax
	je	.LBB12_65
# BB#64:                                # %while.body.i
                                        #   in Loop: Header=BB12_63 Depth=1
	movb	%al, (%r12,%rbx)
	leaq	1(%r12,%rbx), %rax
	incq	%rbx
	cmpq	%r13, %rax
	jb	.LBB12_63
.LBB12_65:                              # %land.rhs.i.safe_concat.exit_crit_edge
	addq	%r12, %rbx
	xorq	%rax, %rax
	movb	%al, (%rbx)
	movzbl	(%rsi), %eax
	cmpq	$43, %rax
	jne	.LBB12_73
# BB#66:                                # %if.then520
	cmpq	%rbx, %r13
	jae	.LBB12_67
# BB#177:                               # %if.then.i
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_73
.LBB12_110:                             # %sw.default
	movq	rtx_name(,%rax,8), %rax
	movq	%rax, 48(%rsp)
	jmp	.LBB12_123
.LBB12_61:                              # %sw.bb502
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	24(%rsi), %rax
	movq	%rax, 32(%rsp)
	movabsq	$.L.str115, %rax
	jmp	.LBB12_112
.LBB12_58:                              # %sw.bb466
	movq	$.L.str108, 48(%rsp)
	jmp	.LBB12_10
.LBB12_60:                              # %sw.bb497
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str114, %rax
	jmp	.LBB12_112
.LBB12_59:                              # %sw.bb480
	movq	$.L.str110, 48(%rsp)
	movq	$.L.str111, 56(%rsp)
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	$.L.str112, 64(%rsp)
	movq	24(%rsi), %rax
	movq	%rax, 32(%rsp)
	movq	$.L.str113, 72(%rsp)
	jmp	.LBB12_123
.LBB12_111:                             # %sw.bb57
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str53, %rax
	jmp	.LBB12_112
.LBB12_2:                               # %sw.bb
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movq	16(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB12_5
# BB#3:                                 # %land.lhs.true
	movq	8(%rax), %rsi
	testq	%rsi, %rsi
	js	.LBB12_4
.LBB12_5:                               # %if.else
	movq	$.L.str50, 56(%rsp)
	movq	%rax, 24(%rsp)
	jmp	.LBB12_123
.LBB12_7:                               # %sw.bb47
	movq	$.L.str49, 56(%rsp)
	jmp	.LBB12_8
.LBB12_9:                               # %sw.bb66
	movq	$.L.str49, 48(%rsp)
	jmp	.LBB12_10
.LBB12_12:                              # %sw.bb72
	movq	$.L.str54, 56(%rsp)
	jmp	.LBB12_8
.LBB12_13:                              # %sw.bb82
	movq	$.L.str55, 56(%rsp)
	jmp	.LBB12_8
.LBB12_15:                              # %sw.bb101
	movq	$.L.str57, 56(%rsp)
	jmp	.LBB12_8
.LBB12_14:                              # %sw.bb92
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str56, %rax
	jmp	.LBB12_112
.LBB12_16:                              # %sw.bb111
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str58, %rax
	jmp	.LBB12_112
.LBB12_22:                              # %sw.bb162
	movq	$.L.str64, 56(%rsp)
	jmp	.LBB12_8
.LBB12_23:                              # %sw.bb172
	movq	$.L.str65, 56(%rsp)
	jmp	.LBB12_8
.LBB12_24:                              # %sw.bb182
	movq	$.L.str66, 56(%rsp)
	jmp	.LBB12_8
.LBB12_21:                              # %sw.bb156
	movq	$.L.str63, 48(%rsp)
	jmp	.LBB12_10
.LBB12_25:                              # %sw.bb192
	movq	$.L.str67, 56(%rsp)
	jmp	.LBB12_8
.LBB12_28:                              # %sw.bb222
	movq	$.L.str70, 56(%rsp)
	jmp	.LBB12_8
.LBB12_27:                              # %sw.bb212
	movq	$.L.str69, 56(%rsp)
	jmp	.LBB12_8
.LBB12_26:                              # %sw.bb202
	movq	$.L.str68, 56(%rsp)
	jmp	.LBB12_8
.LBB12_29:                              # %sw.bb232
	movq	$.L.str71, 56(%rsp)
	jmp	.LBB12_8
.LBB12_17:                              # %sw.bb120
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str59, %rax
	jmp	.LBB12_112
.LBB12_18:                              # %sw.bb129
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str60, %rax
	jmp	.LBB12_112
.LBB12_19:                              # %sw.bb138
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str61, %rax
	jmp	.LBB12_112
.LBB12_20:                              # %sw.bb147
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str62, %rax
	jmp	.LBB12_112
.LBB12_54:                              # %sw.bb442
	movq	$.L.str106, 48(%rsp)
	jmp	.LBB12_10
.LBB12_55:                              # %sw.bb448
	movq	$.L.str107, 48(%rsp)
.LBB12_10:                              # %for.cond557.preheader.for.cond557.preheader.split_crit_edge
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	jmp	.LBB12_123
.LBB12_56:                              # %sw.bb454
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movq	$.L.str106, 56(%rsp)
	jmp	.LBB12_123
.LBB12_57:                              # %sw.bb460
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movq	$.L.str107, 56(%rsp)
	jmp	.LBB12_123
.LBB12_34:                              # %sw.bb267
	movq	$.L.str76, 56(%rsp)
	jmp	.LBB12_8
.LBB12_33:                              # %sw.bb257
	movq	$.L.str75, 56(%rsp)
	jmp	.LBB12_8
.LBB12_39:                              # %sw.bb315
	movq	$.L.str81, 56(%rsp)
	jmp	.LBB12_8
.LBB12_35:                              # %sw.bb277
	movq	$.L.str77, 56(%rsp)
	jmp	.LBB12_8
.LBB12_41:                              # %sw.bb334
	movq	$.L.str83, 56(%rsp)
	jmp	.LBB12_8
.LBB12_37:                              # %sw.bb296
	movq	$.L.str79, 56(%rsp)
.LBB12_8:                               # %for.cond557.preheader.for.cond557.preheader.split_crit_edge
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
.LBB12_123:                             # %for.cond557.preheader.for.cond557.preheader.split_crit_edge
	movq	%r12, %r13
.LBB12_124:                             # %for.cond557.preheader.for.cond557.preheader.split_crit_edge
	addq	$2046, %r12             # imm = 0x7FE
	xorq	%r15, %r15
	leaq	80(%rsp), %r14
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB12_125:                             # %for.body559
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_128 Depth 2
                                        #     Child Loop BB12_170 Depth 2
	movq	48(%rsp,%rbx,8), %rax
	testq	%rax, %rax
	je	.LBB12_167
# BB#126:                               # %if.then563
                                        #   in Loop: Header=BB12_125 Depth=1
	cmpq	%r13, %r12
	jae	.LBB12_127
# BB#164:                               # %if.then.i136
                                        #   in Loop: Header=BB12_125 Depth=1
	movb	%r15b, (%r12)
	movq	%r12, %r13
	jmp	.LBB12_167
	.align	16, 0x90
.LBB12_127:                             # %while.cond.preheader.i135
                                        #   in Loop: Header=BB12_125 Depth=1
	movq	%r13, %rcx
	jbe	.LBB12_166
	.align	16, 0x90
.LBB12_128:                             # %land.rhs.i140
                                        #   Parent Loop BB12_125 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB12_129
# BB#165:                               # %while.body.i144
                                        #   in Loop: Header=BB12_128 Depth=2
	incq	%rax
	leaq	1(%r13), %rcx
	cmpq	%r12, %rcx
	movb	%dl, (%r13)
	movq	%rcx, %r13
	jb	.LBB12_128
	jmp	.LBB12_166
.LBB12_129:                             #   in Loop: Header=BB12_125 Depth=1
	movq	%r13, %rcx
.LBB12_166:                             # %while.end.i146
                                        #   in Loop: Header=BB12_125 Depth=1
	movb	%r15b, (%rcx)
	movq	%rcx, %r13
.LBB12_167:                             # %if.end567
                                        #   in Loop: Header=BB12_125 Depth=1
	movq	16(%rsp,%rbx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB12_174
# BB#168:                               # %if.end577
                                        #   in Loop: Header=BB12_125 Depth=1
	movq	%r14, %rdi
	callq	print_value
	cmpq	%r13, %r12
	jae	.LBB12_169
# BB#182:                               # %if.then.i170
                                        #   in Loop: Header=BB12_125 Depth=1
	movb	%r15b, (%r12)
	movq	%r12, %r13
	jmp	.LBB12_174
	.align	16, 0x90
.LBB12_169:                             # %while.cond.preheader.i169
                                        #   in Loop: Header=BB12_125 Depth=1
	movq	%r14, %rax
	movq	%r13, %rcx
	jbe	.LBB12_173
	.align	16, 0x90
.LBB12_170:                             # %land.rhs.i174
                                        #   Parent Loop BB12_125 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB12_171
# BB#172:                               # %while.body.i178
                                        #   in Loop: Header=BB12_170 Depth=2
	incq	%rax
	leaq	1(%r13), %rcx
	cmpq	%r12, %rcx
	movb	%dl, (%r13)
	movq	%rcx, %r13
	jb	.LBB12_170
	jmp	.LBB12_173
.LBB12_171:                             #   in Loop: Header=BB12_125 Depth=1
	movq	%r13, %rcx
.LBB12_173:                             # %while.end.i180
                                        #   in Loop: Header=BB12_125 Depth=1
	movb	%r15b, (%rcx)
	movq	%rcx, %r13
.LBB12_174:                             # %for.inc584
                                        #   in Loop: Header=BB12_125 Depth=1
	incq	%rbx
	cmpq	$4, %rbx
	jne	.LBB12_125
	jmp	.LBB12_176
.LBB12_40:                              # %sw.bb325
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str82, %rax
	jmp	.LBB12_112
.LBB12_36:                              # %sw.bb287
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str78, %rax
	jmp	.LBB12_112
.LBB12_42:                              # %sw.bb344
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str84, %rax
	jmp	.LBB12_112
.LBB12_38:                              # %sw.bb306
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movabsq	$.L.str80, %rax
	jmp	.LBB12_112
.LBB12_45:                              # %sw.bb381
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str90, %rax
	jmp	.LBB12_112
.LBB12_46:                              # %sw.bb388
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str92, %rax
	jmp	.LBB12_112
.LBB12_48:                              # %sw.bb402
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str96, %rax
	jmp	.LBB12_112
.LBB12_47:                              # %sw.bb395
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str94, %rax
	jmp	.LBB12_112
.LBB12_49:                              # %sw.bb409
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str98, %rax
	jmp	.LBB12_112
.LBB12_50:                              # %sw.bb416
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str100, %rax
	jmp	.LBB12_112
.LBB12_52:                              # %sw.bb430
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str103, %rax
	jmp	.LBB12_112
.LBB12_51:                              # %sw.bb423
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str102, %rax
	jmp	.LBB12_112
.LBB12_53:                              # %sw.bb435
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str105, %rax
	jmp	.LBB12_112
.LBB12_30:                              # %sw.bb242
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str72, %rax
	jmp	.LBB12_112
.LBB12_31:                              # %sw.bb247
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str73, %rax
	jmp	.LBB12_112
.LBB12_32:                              # %sw.bb252
	movq	8(%rsi), %rax
	movq	%rax, 16(%rsp)
	movabsq	$.L.str74, %rax
	jmp	.LBB12_112
.LBB12_43:                              # %sw.bb353
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	24(%rsi), %rax
	movq	%rax, 32(%rsp)
	movabsq	$.L.str86, %rax
	jmp	.LBB12_112
.LBB12_44:                              # %sw.bb366
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	24(%rsi), %rax
	movq	%rax, 32(%rsp)
	movabsq	$.L.str88, %rax
.LBB12_112:                             # %if.then553
	leaq	2046(%r12), %r13
	.align	16, 0x90
.LBB12_113:                             # %land.rhs.i106
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_114
# BB#115:                               # %while.body.i110
                                        #   in Loop: Header=BB12_113 Depth=1
	incq	%rax
	leaq	1(%r12), %rbx
	cmpq	%r13, %rbx
	movb	%cl, (%r12)
	movq	%rbx, %r12
	jb	.LBB12_113
	jmp	.LBB12_116
.LBB12_114:
	movq	%r12, %rbx
.LBB12_116:                             # %while.end.i112
	xorq	%rax, %rax
	cmpq	%rbx, %r13
	movb	%al, (%rbx)
	jae	.LBB12_117
# BB#130:                               # %if.then.i119
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_131
.LBB12_117:                             # %while.cond.preheader.i118
	jbe	.LBB12_121
	.align	16, 0x90
.LBB12_118:                             # %land.rhs.i123
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str121(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_120
# BB#119:                               # %while.body.i127
                                        #   in Loop: Header=BB12_118 Depth=1
	movb	%cl, (%rbx,%rax)
	leaq	1(%rbx,%rax), %rcx
	incq	%rax
	cmpq	%r13, %rcx
	jb	.LBB12_118
.LBB12_120:                             # %land.rhs.i123.while.end.i129.loopexit_crit_edge
	addq	%rax, %rbx
.LBB12_121:                             # %while.end.i129
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB12_131:                             # %for.cond557.preheader.split.us
	xorq	%r12, %r12
	leaq	80(%rsp), %r14
	xorq	%r15, %r15
	.align	16, 0x90
.LBB12_132:                             # %for.body559.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_135 Depth 2
                                        #     Child Loop BB12_144 Depth 2
                                        #     Child Loop BB12_151 Depth 2
	movq	48(%rsp,%r15,8), %rax
	testq	%rax, %rax
	je	.LBB12_140
# BB#133:                               # %if.then563.us
                                        #   in Loop: Header=BB12_132 Depth=1
	cmpq	%rbx, %r13
	jae	.LBB12_134
# BB#139:                               # %if.then.i136.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movb	%r12b, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_140
	.align	16, 0x90
.LBB12_134:                             # %while.cond.preheader.i135.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movq	%rbx, %rcx
	jbe	.LBB12_138
	.align	16, 0x90
.LBB12_135:                             # %land.rhs.i140.us
                                        #   Parent Loop BB12_132 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB12_136
# BB#137:                               # %while.body.i144.us
                                        #   in Loop: Header=BB12_135 Depth=2
	incq	%rax
	leaq	1(%rbx), %rcx
	cmpq	%r13, %rcx
	movb	%dl, (%rbx)
	movq	%rcx, %rbx
	jb	.LBB12_135
	jmp	.LBB12_138
.LBB12_136:                             #   in Loop: Header=BB12_132 Depth=1
	movq	%rbx, %rcx
.LBB12_138:                             # %while.end.i146.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movb	%r12b, (%rcx)
	movq	%rcx, %rbx
.LBB12_140:                             # %if.end567.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movq	16(%rsp,%r15,8), %rsi
	testq	%rsi, %rsi
	je	.LBB12_156
# BB#141:                               # %if.then571.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB12_149
# BB#142:                               # %if.then575.us
                                        #   in Loop: Header=BB12_132 Depth=1
	cmpq	%rbx, %r13
	jae	.LBB12_143
# BB#148:                               # %if.then.i153.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movb	%r12b, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_149
.LBB12_143:                             # %while.cond.preheader.i152.us
                                        #   in Loop: Header=BB12_132 Depth=1
	xorq	%rax, %rax
	cmpq	%rbx, %r13
	jbe	.LBB12_147
	.align	16, 0x90
.LBB12_144:                             # %land.rhs.i157.us
                                        #   Parent Loop BB12_132 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	.L.str118(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_146
# BB#145:                               # %while.body.i161.us
                                        #   in Loop: Header=BB12_144 Depth=2
	movb	%cl, (%rbx,%rax)
	leaq	1(%rbx,%rax), %rcx
	incq	%rax
	cmpq	%r13, %rcx
	jb	.LBB12_144
.LBB12_146:                             # %land.rhs.i157.us.while.end.i163.us.loopexit_crit_edge
                                        #   in Loop: Header=BB12_132 Depth=1
	addq	%rax, %rbx
.LBB12_147:                             # %while.end.i163.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movb	%r12b, (%rbx)
	movq	16(%rsp,%r15,8), %rsi
.LBB12_149:                             # %if.end577.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movq	%r14, %rdi
	callq	print_value
	cmpq	%rbx, %r13
	jae	.LBB12_150
# BB#155:                               # %if.then.i170.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movb	%r12b, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_156
	.align	16, 0x90
.LBB12_150:                             # %while.cond.preheader.i169.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movq	%r14, %rax
	movq	%rbx, %rcx
	jbe	.LBB12_154
	.align	16, 0x90
.LBB12_151:                             # %land.rhs.i174.us
                                        #   Parent Loop BB12_132 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB12_152
# BB#153:                               # %while.body.i178.us
                                        #   in Loop: Header=BB12_151 Depth=2
	incq	%rax
	leaq	1(%rbx), %rcx
	cmpq	%r13, %rcx
	movb	%dl, (%rbx)
	movq	%rcx, %rbx
	jb	.LBB12_151
	jmp	.LBB12_154
.LBB12_152:                             #   in Loop: Header=BB12_132 Depth=1
	movq	%rbx, %rcx
.LBB12_154:                             # %while.end.i180.us
                                        #   in Loop: Header=BB12_132 Depth=1
	movb	%r12b, (%rcx)
	movq	%rcx, %rbx
.LBB12_156:                             # %for.inc584.us
                                        #   in Loop: Header=BB12_132 Depth=1
	incq	%r15
	cmpq	$4, %r15
	jne	.LBB12_132
# BB#157:                               # %if.then588
	cmpq	%rbx, %r13
	jae	.LBB12_158
# BB#175:                               # %if.then.i187
	xorq	%rax, %rax
	movb	%al, (%r13)
	jmp	.LBB12_176
.LBB12_158:                             # %while.cond.preheader.i186
	cmpq	%rbx, %r13
	jbe	.LBB12_163
# BB#159:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB12_160:                             # %land.rhs.i191
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str52(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_162
# BB#161:                               # %while.body.i195
                                        #   in Loop: Header=BB12_160 Depth=1
	movb	%cl, (%rbx,%rax)
	leaq	1(%rbx,%rax), %rcx
	incq	%rax
	cmpq	%r13, %rcx
	jb	.LBB12_160
.LBB12_162:                             # %land.rhs.i191.while.end.i197.loopexit_crit_edge
	addq	%rax, %rbx
.LBB12_163:                             # %while.end.i197
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB12_176:                             # %if.end590
	addq	$2128, %rsp             # imm = 0x850
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_122:                             # %sw.bb36
	movq	$.L.str51, 56(%rsp)
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 16(%rsp)
	movq	$.L.str52, 64(%rsp)
	jmp	.LBB12_123
.LBB12_67:                              # %while.cond.preheader.i
	cmpq	%rbx, %r13
	jbe	.LBB12_72
# BB#68:
	xorq	%rax, %rax
.LBB12_69:                              # %land.rhs.i6
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str117(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_71
# BB#70:                                # %while.body.i10
                                        #   in Loop: Header=BB12_69 Depth=1
	movb	%cl, (%rbx,%rax)
	leaq	1(%rbx,%rax), %rcx
	incq	%rax
	cmpq	%r13, %rcx
	jb	.LBB12_69
.LBB12_71:                              # %land.rhs.i6.while.end.i.loopexit_crit_edge
	addq	%rax, %rbx
.LBB12_72:                              # %while.end.i
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB12_73:                              # %if.end522
	cmpq	%rbx, %r13
	jae	.LBB12_74
# BB#178:                               # %if.then.i17
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_80
.LBB12_74:                              # %while.cond.preheader.i16
	cmpq	%rbx, %r13
	jbe	.LBB12_79
# BB#75:
	xorq	%rax, %rax
.LBB12_76:                              # %land.rhs.i21
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str2(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_78
# BB#77:                                # %while.body.i25
                                        #   in Loop: Header=BB12_76 Depth=1
	movb	%cl, (%rbx,%rax)
	leaq	1(%rbx,%rax), %rcx
	incq	%rax
	cmpq	%r13, %rcx
	jb	.LBB12_76
.LBB12_78:                              # %land.rhs.i21.while.end.i27.loopexit_crit_edge
	addq	%rax, %rbx
.LBB12_79:                              # %while.end.i27
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB12_80:                              # %for.cond524.preheader
	movq	8(%rsi), %rax
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB12_95
# BB#81:
	xorq	%r15, %r15
	movabsq	$.L.str5, %r14
	.align	16, 0x90
.LBB12_82:                              # %for.body528
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_84 Depth 2
                                        #     Child Loop BB12_90 Depth 2
	movq	8(%rax,%r15,8), %rsi
	leaq	80(%rsp), %rdi
	callq	print_pattern
	cmpq	%rbx, %r13
	jae	.LBB12_83
# BB#179:                               # %if.then.i34
                                        #   in Loop: Header=BB12_82 Depth=1
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	%r13, %rax
	jmp	.LBB12_88
.LBB12_83:                              # %while.cond.preheader.i33
                                        #   in Loop: Header=BB12_82 Depth=1
	movq	%rbx, %rax
	jbe	.LBB12_87
	.align	16, 0x90
.LBB12_84:                              # %land.rhs.i38
                                        #   Parent Loop BB12_82 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB12_85
# BB#86:                                # %while.body.i42
                                        #   in Loop: Header=BB12_84 Depth=2
	incq	%r14
	leaq	1(%rbx), %rax
	cmpq	%r13, %rax
	movb	%cl, (%rbx)
	movq	%rax, %rbx
	jb	.LBB12_84
	jmp	.LBB12_87
.LBB12_85:                              #   in Loop: Header=BB12_82 Depth=1
	movq	%rbx, %rax
.LBB12_87:                              # %while.end.i44
                                        #   in Loop: Header=BB12_82 Depth=1
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
.LBB12_88:                              # %safe_concat.exit46
                                        #   in Loop: Header=BB12_82 Depth=1
	cmpq	%rax, %r13
	jae	.LBB12_89
# BB#180:                               # %if.then.i51
                                        #   in Loop: Header=BB12_82 Depth=1
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	%r13, %rbx
	jmp	.LBB12_94
.LBB12_89:                              # %while.cond.preheader.i50
                                        #   in Loop: Header=BB12_82 Depth=1
	leaq	80(%rsp), %rcx
	movq	%rax, %rbx
	jbe	.LBB12_93
	.align	16, 0x90
.LBB12_90:                              # %land.rhs.i55
                                        #   Parent Loop BB12_82 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB12_91
# BB#92:                                # %while.body.i59
                                        #   in Loop: Header=BB12_90 Depth=2
	incq	%rcx
	leaq	1(%rax), %rbx
	cmpq	%r13, %rbx
	movb	%dl, (%rax)
	movq	%rbx, %rax
	jb	.LBB12_90
	jmp	.LBB12_93
.LBB12_91:                              #   in Loop: Header=BB12_82 Depth=1
	movq	%rax, %rbx
.LBB12_93:                              # %while.end.i61
                                        #   in Loop: Header=BB12_82 Depth=1
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB12_94:                              # %safe_concat.exit63
                                        #   in Loop: Header=BB12_82 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rax
	incq	%r15
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movabsq	$.L.str118, %r14
	jl	.LBB12_82
.LBB12_95:                              # %for.end539
	cmpq	%rbx, %r13
	jae	.LBB12_96
# BB#181:                               # %if.then.i68
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	%r13, %rbx
	movq	8(%rsp), %rcx           # 8-byte Reload
	jmp	.LBB12_102
.LBB12_96:                              # %while.cond.preheader.i67
	cmpq	%rbx, %r13
	jbe	.LBB12_101
# BB#97:
	xorq	%rax, %rax
.LBB12_98:                              # %land.rhs.i72
                                        # =>This Inner Loop Header: Depth=1
	movzbl	.L.str119(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB12_100
# BB#99:                                # %while.body.i76
                                        #   in Loop: Header=BB12_98 Depth=1
	movb	%cl, (%rbx,%rax)
	leaq	1(%rbx,%rax), %rcx
	incq	%rax
	cmpq	%r13, %rcx
	jb	.LBB12_98
.LBB12_100:                             # %land.rhs.i72.while.end.i78.loopexit_crit_edge
	addq	%rax, %rbx
.LBB12_101:                             # %while.end.i78
	movq	8(%rsp), %rcx           # 8-byte Reload
	xorq	%rax, %rax
	movb	%al, (%rbx)
.LBB12_102:                             # %safe_concat.exit80
	movl	16(%rcx), %edx
	movabsq	$.L.str120, %rsi
	leaq	80(%rsp), %r14
	xorq	%rax, %rax
	xorq	%r15, %r15
	movq	%r14, %rdi
	callq	sprintf
	cmpq	%rbx, %r13
	jae	.LBB12_103
# BB#105:                               # %if.then.i85
	movb	%r15b, (%r13)
	jmp	.LBB12_124
.LBB12_103:                             # %while.cond.preheader.i84
	jbe	.LBB12_104
.LBB12_106:                             # %land.rhs.i89
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB12_107
# BB#108:                               # %while.body.i93
                                        #   in Loop: Header=BB12_106 Depth=1
	incq	%r14
	leaq	1(%rbx), %rax
	cmpq	%r13, %rax
	movb	%cl, (%rbx)
	movq	%rax, %rbx
	jb	.LBB12_106
	jmp	.LBB12_109
.LBB12_104:
	movq	%rbx, %rax
	jmp	.LBB12_109
.LBB12_107:
	movq	%rbx, %rax
.LBB12_109:                             # %while.end.i95
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	%rax, %r13
	jmp	.LBB12_124
.LBB12_4:                               # %if.then
	movq	$.L.str49, 56(%rsp)
	negq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 24(%rsp)
	jmp	.LBB12_123
.Ltmp140:
	.size	print_exp, .Ltmp140-print_exp
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_62
	.quad	.LBB12_62
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_61
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_58
	.quad	.LBB12_110
	.quad	.LBB12_60
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_59
	.quad	.LBB12_110
	.quad	.LBB12_111
	.quad	.LBB12_2
	.quad	.LBB12_7
	.quad	.LBB12_9
	.quad	.LBB12_12
	.quad	.LBB12_13
	.quad	.LBB12_15
	.quad	.LBB12_14
	.quad	.LBB12_16
	.quad	.LBB12_22
	.quad	.LBB12_23
	.quad	.LBB12_24
	.quad	.LBB12_21
	.quad	.LBB12_25
	.quad	.LBB12_28
	.quad	.LBB12_27
	.quad	.LBB12_26
	.quad	.LBB12_29
	.quad	.LBB12_17
	.quad	.LBB12_18
	.quad	.LBB12_19
	.quad	.LBB12_20
	.quad	.LBB12_54
	.quad	.LBB12_55
	.quad	.LBB12_56
	.quad	.LBB12_57
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_34
	.quad	.LBB12_33
	.quad	.LBB12_39
	.quad	.LBB12_35
	.quad	.LBB12_41
	.quad	.LBB12_37
	.quad	.LBB12_40
	.quad	.LBB12_36
	.quad	.LBB12_42
	.quad	.LBB12_38
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_110
	.quad	.LBB12_45
	.quad	.LBB12_46
	.quad	.LBB12_48
	.quad	.LBB12_47
	.quad	.LBB12_49
	.quad	.LBB12_50
	.quad	.LBB12_52
	.quad	.LBB12_51
	.quad	.LBB12_53
	.quad	.LBB12_30
	.quad	.LBB12_31
	.quad	.LBB12_32
	.quad	.LBB12_43
	.quad	.LBB12_44
	.quad	.LBB12_110
	.quad	.LBB12_122

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"none"
	.size	.L.str, 5

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"["
	.size	.L.str2, 2

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"]"
	.size	.L.str4, 2

	.type	target_units,@object    # @target_units
	.local	target_units
	.comm	target_units,4,4
	.type	visual_tbl,@object      # @visual_tbl
	.comm	visual_tbl,8,8
	.type	.L.str5,@object         # @.str5
.L.str5:
	.zero	1
	.size	.L.str5, 1

	.type	n_visual_lines,@object  # @n_visual_lines
	.comm	n_visual_lines,4,4
	.type	n_vis_no_unit,@object   # @n_vis_no_unit
	.comm	n_vis_no_unit,4,4
	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\n;;   ==================== scheduling visualization %s \n"
	.size	.L.str6, 57

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	";;   %-8s"
	.size	.L.str7, 10

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"clock"
	.size	.L.str8, 6

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"  %-33s"
	.size	.L.str9, 8

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"  %-8s\n"
	.size	.L.str10, 8

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"no-unit"
	.size	.L.str11, 8

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"====="
	.size	.L.str12, 6

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"=============================="
	.size	.L.str13, 31

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"======="
	.size	.L.str14, 8

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"%s\n"
	.size	.L.str15, 4

	.type	vis_no_unit,@object     # @vis_no_unit
	.comm	vis_no_unit,160,16
	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"(incomplete)"
	.size	.L.str16, 13

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	";;   %-8d"
	.size	.L.str17, 10

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"------------------------------"
	.size	.L.str18, 31

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"  %-8d"
	.size	.L.str19, 7

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"\n"
	.size	.L.str20, 2

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	";;       "
	.size	.L.str21, 10

	.type	visual_tbl_line_length,@object # @visual_tbl_line_length
	.local	visual_tbl_line_length
	.comm	visual_tbl_line_length,4,4
	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"[...]\n"
	.size	.L.str22, 7

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"  %33s"
	.size	.L.str23, 7

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"uname"
	.size	.L.str24, 6

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"%-4d %s"
	.size	.L.str26, 8

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"call <...>"
	.size	.L.str28, 11

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"L%d:"
	.size	.L.str29, 5

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"i% 4d: barrier"
	.size	.L.str30, 15

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"%4d note \"%s\" %d"
	.size	.L.str31, 17

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"%4d %s"
	.size	.L.str32, 7

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"i%-4d  <What?>"
	.size	.L.str34, 15

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"%s=%s"
	.size	.L.str35, 6

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"return"
	.size	.L.str36, 7

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"clobber %s"
	.size	.L.str37, 11

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"use %s"
	.size	.L.str38, 7

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"(%s) %s"
	.size	.L.str39, 8

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"%s%s;"
	.size	.L.str41, 6

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"%s}"
	.size	.L.str42, 4

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"%%{"
	.size	.L.str43, 4

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"%s%%}"
	.size	.L.str44, 6

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"asm {%s}"
	.size	.L.str45, 9

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"trap_if %s"
	.size	.L.str46, 11

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"unspec/v{"
	.size	.L.str48, 10

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"-"
	.size	.L.str49, 2

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"+"
	.size	.L.str50, 2

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"+low("
	.size	.L.str51, 6

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	")"
	.size	.L.str52, 2

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"cmp"
	.size	.L.str53, 4

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"*"
	.size	.L.str54, 2

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"/"
	.size	.L.str55, 2

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"udiv"
	.size	.L.str56, 5

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"%"
	.size	.L.str57, 2

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"umod"
	.size	.L.str58, 5

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"smin"
	.size	.L.str59, 5

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"smax"
	.size	.L.str60, 5

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"umin"
	.size	.L.str61, 5

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"umax"
	.size	.L.str62, 5

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"!"
	.size	.L.str63, 2

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"&"
	.size	.L.str64, 2

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"|"
	.size	.L.str65, 2

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"^"
	.size	.L.str66, 2

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"<<"
	.size	.L.str67, 3

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	" 0>>"
	.size	.L.str68, 5

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	">>"
	.size	.L.str69, 3

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"<-<"
	.size	.L.str70, 4

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	">->"
	.size	.L.str71, 4

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"abs"
	.size	.L.str72, 4

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"sqrt"
	.size	.L.str73, 5

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"ffs"
	.size	.L.str74, 4

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"=="
	.size	.L.str75, 3

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"!="
	.size	.L.str76, 3

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	">"
	.size	.L.str77, 2

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"gtu"
	.size	.L.str78, 4

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"<"
	.size	.L.str79, 2

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"ltu"
	.size	.L.str80, 4

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	">="
	.size	.L.str81, 3

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"geu"
	.size	.L.str82, 4

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"<="
	.size	.L.str83, 3

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"leu"
	.size	.L.str84, 4

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"sxt"
	.size	.L.str86, 4

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"zxt"
	.size	.L.str88, 4

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"sxn"
	.size	.L.str90, 4

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"zxn"
	.size	.L.str92, 4

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"fxn"
	.size	.L.str94, 4

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"trn"
	.size	.L.str96, 4

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"ftr"
	.size	.L.str98, 4

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"flt"
	.size	.L.str100, 4

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"ufl"
	.size	.L.str102, 4

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"fix"
	.size	.L.str103, 4

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"ufx"
	.size	.L.str105, 4

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"--"
	.size	.L.str106, 3

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"++"
	.size	.L.str107, 3

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"call "
	.size	.L.str108, 6

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"{("
	.size	.L.str110, 3

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	")?"
	.size	.L.str111, 3

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	":"
	.size	.L.str112, 2

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"}"
	.size	.L.str113, 2

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"trap_if"
	.size	.L.str114, 8

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"prefetch"
	.size	.L.str115, 9

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"unspec"
	.size	.L.str116, 7

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"/v"
	.size	.L.str117, 3

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	","
	.size	.L.str118, 2

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"] "
	.size	.L.str119, 3

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"%d"
	.size	.L.str120, 3

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"("
	.size	.L.str121, 2

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"0x%llx"
	.size	.L.str122, 7

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"<0x%lx,0x%lx>"
	.size	.L.str123, 14

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"\""
	.size	.L.str124, 2

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"`"
	.size	.L.str125, 2

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"'"
	.size	.L.str126, 2

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"L%d"
	.size	.L.str127, 4

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"const("
	.size	.L.str128, 7

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"high("
	.size	.L.str129, 6

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"r%d"
	.size	.L.str130, 4

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"#%d"
	.size	.L.str131, 4

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"scratch"
	.size	.L.str132, 8

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"cc0"
	.size	.L.str133, 4

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"pc"
	.size	.L.str134, 3


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
