	.file	"predict.c.bc"
	.text
	.globl	predict_insn
	.align	16, 0x90
	.type	predict_insn,@function
predict_insn:                           # @predict_insn
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
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_2
# BB#1:                                 # %if.end
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB0_2:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.predict_insn, %rcx
	movq	$101, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp10:
	.size	predict_insn, .Ltmp10-predict_insn
	.cfi_endproc

	.globl	predict_insn_def
	.align	16, 0x90
	.type	predict_insn_def,@function
predict_insn_def:                       # @predict_insn_def
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdx, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	predictor_info+8(%rdx), %edx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	je	.LBB1_2
# BB#1:                                 # %entry
	movq	$10000, %rax            # imm = 0x2710
	subq	%rdx, %rax
	movq	%rax, %rdx
.LBB1_2:                                # %entry
	jmp	predict_insn  # TAILCALL
.Ltmp11:
	.size	predict_insn_def, .Ltmp11-predict_insn_def
	.cfi_endproc

	.globl	predict_edge
	.align	16, 0x90
	.type	predict_edge,@function
predict_edge:                           # @predict_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp21:
	.cfi_def_cfa_offset 48
.Ltmp22:
	.cfi_offset %rbx, -40
.Ltmp23:
	.cfi_offset %r12, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rax
	movq	8(%rax), %r12
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%r12, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_4
# BB#1:                                 # %if.end
	movl	48(%rbx), %eax
	testq	$1, %rax
	je	.LBB2_3
# BB#2:                                 # %if.end
	movq	$10000, %rax            # imm = 0x2710
	subq	%r15, %rax
	movq	%rax, %r15
.LBB2_3:                                # %if.end
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	predict_insn  # TAILCALL
.LBB2_4:                                # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp26:
	.size	predict_edge, .Ltmp26-predict_edge
	.cfi_endproc

	.globl	predict_edge_def
	.align	16, 0x90
	.type	predict_edge_def,@function
predict_edge_def:                       # @predict_edge_def
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp33:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp34:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp35:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp36:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp37:
	.cfi_def_cfa_offset 48
.Ltmp38:
	.cfi_offset %rbx, -48
.Ltmp39:
	.cfi_offset %r12, -40
.Ltmp40:
	.cfi_offset %r13, -32
.Ltmp41:
	.cfi_offset %r14, -24
.Ltmp42:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	predictor_info+8(%rax), %r12d
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rdx
	cmpq	$1, %rdx
	je	.LBB3_2
# BB#1:                                 # %entry
	movq	$10000, %rax            # imm = 0x2710
	subq	%r12, %rax
	movq	%rax, %r12
.LBB3_2:                                # %entry
	movq	16(%r15), %rax
	movq	8(%rax), %rbx
	movq	%rbx, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB3_6
# BB#3:                                 # %if.end.i
	movl	48(%r15), %eax
	testq	$1, %rax
	je	.LBB3_5
# BB#4:                                 # %if.end.i
	movq	$10000, %rax            # imm = 0x2710
	subq	%r12, %rax
	movq	%rax, %r12
.LBB3_5:                                # %if.end.i
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	predict_insn  # TAILCALL
.LBB3_6:                                # %predict_edge.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp43:
	.size	predict_edge_def, .Ltmp43-predict_edge_def
	.cfi_endproc

	.globl	invert_br_probabilities
	.align	16, 0x90
	.type	invert_br_probabilities,@function
invert_br_probabilities:                # @invert_br_probabilities
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 16
.Ltmp47:
	.cfi_offset %rbx, -16
	movq	56(%rdi), %rbx
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_6:                                # %for.inc
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	16(%rbx), %rbx
.LBB4_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB4_7
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB4_1 Depth=1
	movzbl	2(%rbx), %eax
	cmpq	$20, %rax
	jne	.LBB4_3
# BB#5:                                 # %if.then13
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rbx), %rax
	movq	16(%rax), %rax
	movq	8(%rax), %rax
	movq	$10000, %rsi            # imm = 0x2710
	subq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	8(%rbx), %rcx
	movq	%rax, 16(%rcx)
	jmp	.LBB4_6
	.align	16, 0x90
.LBB4_3:                                # %for.body
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$16, %rax
	jne	.LBB4_6
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movq	$10000, %rsi            # imm = 0x2710
	subq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rbx)
	movq	16(%rbx), %rbx
	jmp	.LBB4_1
.LBB4_7:                                # %for.end
	popq	%rbx
	retq
.Ltmp48:
	.size	invert_br_probabilities, .Ltmp48-invert_br_probabilities
	.cfi_endproc

	.globl	estimate_probability
	.align	16, 0x90
	.type	estimate_probability,@function
estimate_probability:                   # @estimate_probability
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp55:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp57:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp58:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp59:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp60:
	.cfi_def_cfa_offset 160
.Ltmp61:
	.cfi_offset %rbx, -48
.Ltmp62:
	.cfi_offset %r12, -40
.Ltmp63:
	.cfi_offset %r13, -32
.Ltmp64:
	.cfi_offset %r14, -24
.Ltmp65:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%rdi, %r14
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%rdi, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r15
	movq	%r15, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	calculate_dominance_info
	xorq	%rdi, %rdi
	movq	$1, %rdx
	movq	%r15, %rsi
	callq	calculate_dominance_info
	movq	%r14, %rdi
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB5_11
# BB#1:                                 # %for.body.lr.ph
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_4 Depth 2
                                        #       Child Loop BB5_6 Depth 3
                                        #       Child Loop BB5_46 Depth 3
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	8(%rdi), %r14
	imulq	$248, %rsi, %rbx
	leaq	(%r14,%rbx), %rsi
	movq	$8, %rdx
	callq	flow_loop_scan
	movq	48(%r14,%rbx), %rax
	movq	56(%r14,%rbx), %rcx
	movslq	88(%rax), %rdx
	movslq	88(%rcx), %rax
	movq	%rdx, %rcx
	cmpq	%rax, %rcx
	jg	.LBB5_56
# BB#3:                                 # %for.body6.lr.ph
                                        #   in Loop: Header=BB5_2 Depth=1
	movl	104(%r14,%rbx), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	leaq	56(%r14,%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	leaq	64(%r14,%rbx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	leaq	8(%r14,%rbx), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	leaq	16(%r14,%rbx), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB5_4:                                # %for.body6
                                        #   Parent Loop BB5_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_6 Depth 3
                                        #       Child Loop BB5_46 Depth 3
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rcx, %rsi
	movq	$3, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	andq	$536870904, %rdx        # imm = 0x1FFFFFF8
	movq	16(%rax,%rdx), %rax
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB5_55
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB5_4 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movq	40(%rax), %rbx
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	je	.LBB5_45
	.align	16, 0x90
.LBB5_6:                                # %for.body13
                                        #   Parent Loop BB5_2 Depth=1
                                        #     Parent Loop BB5_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_43
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_6 Depth=3
	movq	16(%rbx), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_43
# BB#8:                                 # %if.then16
                                        #   in Loop: Header=BB5_6 Depth=3
	movq	8(%rax), %r14
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	movq	$1, %rdx
	je	.LBB5_43
# BB#9:                                 # %if.end.i.i
                                        #   in Loop: Header=BB5_6 Depth=3
	movl	48(%rbx), %r13d
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB5_10
# BB#40:                                # %predict_insn.exit771
                                        #   in Loop: Header=BB5_6 Depth=3
	movq	%r15, %r12
	xorq	%rdi, %rdi
	movq	$8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	testq	$1, %r13
	movq	$8900, %rsi             # imm = 0x22C4
	je	.LBB5_42
# BB#41:                                # %predict_insn.exit771
                                        #   in Loop: Header=BB5_6 Depth=3
	movq	$1100, %rsi             # imm = 0x44C
.LBB5_42:                               # %predict_insn.exit771
                                        #   in Loop: Header=BB5_6 Depth=3
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
	movq	%r12, %r15
	movq	$1, %rdx
	.align	16, 0x90
.LBB5_43:                               # %for.inc
                                        #   in Loop: Header=BB5_6 Depth=3
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB5_6
# BB#44:                                # %for.end
                                        #   in Loop: Header=BB5_4 Depth=2
	testq	%r15, %rdx
	jne	.LBB5_55
.LBB5_45:                               # %if.then18
                                        #   in Loop: Header=BB5_4 Depth=2
	movq	basic_block_info(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movq	40(%rax), %r14
	jmp	.LBB5_46
	.align	16, 0x90
.LBB5_54:                               # %for.inc47
                                        #   in Loop: Header=BB5_46 Depth=3
	movq	8(%r14), %r14
.LBB5_46:                               # %if.then18
                                        #   Parent Loop BB5_2 Depth=1
                                        #     Parent Loop BB5_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r14, %r14
	je	.LBB5_55
# BB#47:                                # %for.body26
                                        #   in Loop: Header=BB5_46 Depth=3
	movq	24(%r14), %rax
	movslq	88(%rax), %rax
	testq	%rax, %rax
	js	.LBB5_49
# BB#48:                                # %lor.lhs.false
                                        #   in Loop: Header=BB5_46 Depth=3
	movq	%rax, %rdx
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rsi
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	16(%rsi,%rdx), %rdx
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	jne	.LBB5_54
.LBB5_49:                               # %if.then44
                                        #   in Loop: Header=BB5_46 Depth=3
	movq	$32, %rcx
	movq	80(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1000, %rax             # imm = 0x3E8
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %rbx
	movq	16(%r14), %rax
	movq	8(%rax), %r13
	movq	%r13, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB5_54
# BB#50:                                # %if.end.i
                                        #   in Loop: Header=BB5_46 Depth=3
	movq	%r15, %r12
	movl	48(%r14), %r15d
	movq	%r13, %rdi
	callq	any_condjump_p
	testq	%r12, %rax
	je	.LBB5_10
# BB#51:                                # %predict_insn.exit
                                        #   in Loop: Header=BB5_46 Depth=3
	testq	$1, %r15
	je	.LBB5_53
# BB#52:                                # %predict_insn.exit
                                        #   in Loop: Header=BB5_46 Depth=3
	movq	$10000, %rax            # imm = 0x2710
	subq	%rbx, %rax
	movq	%rax, %rbx
.LBB5_53:                               # %predict_insn.exit
                                        #   in Loop: Header=BB5_46 Depth=3
	xorq	%rdi, %rdi
	movq	$9, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%r13), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r13)
	movq	%r12, %r15
	jmp	.LBB5_54
	.align	16, 0x90
.LBB5_55:                               # %for.inc52
                                        #   in Loop: Header=BB5_4 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	$32, %rcx
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movslq	88(%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rsi, %rcx
	incq	%rcx
	cmpq	%rax, %rdx
	jl	.LBB5_4
.LBB5_56:                               # %for.inc54
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	32(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_2
.LBB5_11:                               # %for.cond57.preheader
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	movq	24(%rsp), %r12          # 8-byte Reload
	jle	.LBB5_160
# BB#12:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB5_13:                               # %for.body59
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_17 Depth 2
                                        #       Child Loop BB5_19 Depth 3
                                        #     Child Loop BB5_33 Depth 2
                                        #       Child Loop BB5_79 Depth 3
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r14,8), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	8(%rcx), %rbx
	cmpq	$0, 40(%rcx)
	jne	.LBB5_30
# BB#14:                                # %for.body59
                                        #   in Loop: Header=BB5_13 Depth=1
	testq	%r13, %rsi
	jne	.LBB5_30
# BB#15:                                # %for.cond71.preheader
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rsi
	testq	%rdx, %rdx
	jle	.LBB5_30
# BB#16:                                # %for.body73.lr.ph
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	$6, %rcx
	movq	%r14, %r15
	shrq	%cl, %r15
	andq	$67108863, %r15         # imm = 0x3FFFFFF
	movq	$1, %rdx
	movq	%r14, %rcx
	movq	%r14, 56(%rsp)          # 8-byte Spill
	shlq	%cl, %rdx
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB5_17:                               # %for.body73
                                        #   Parent Loop BB5_13 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_19 Depth 3
	movq	(%r12,%r13,8), %rcx
	movq	16(%rcx,%r15,8), %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rcx
	jne	.LBB5_28
# BB#18:                                # %if.then85
                                        #   in Loop: Header=BB5_17 Depth=2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r13,8), %rcx
	movq	40(%rcx), %r14
	testq	%r14, %r14
	je	.LBB5_28
	.align	16, 0x90
.LBB5_19:                               # %for.body93
                                        #   Parent Loop BB5_13 Depth=1
                                        #     Parent Loop BB5_17 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%r14), %rax
	movslq	88(%rax), %rax
	testq	%rax, %rax
	js	.LBB5_26
# BB#20:                                # %land.lhs.true97
                                        #   in Loop: Header=BB5_19 Depth=3
	movq	(%r12,%rax,8), %rax
	movq	16(%rax,%r15,8), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB5_26
# BB#21:                                # %if.then111
                                        #   in Loop: Header=BB5_19 Depth=3
	movq	16(%r14), %rax
	movq	8(%rax), %rbx
	movq	%rbx, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_26
# BB#22:                                # %if.end.i.i599
                                        #   in Loop: Header=BB5_19 Depth=3
	movq	%rbx, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_10
# BB#23:                                # %predict_insn.exit784
                                        #   in Loop: Header=BB5_19 Depth=3
	xorq	%rdi, %rdi
	movq	$7, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movl	48(%r14), %eax
	testq	$1, %rax
	movq	$100, %rsi
	je	.LBB5_25
# BB#24:                                # %predict_insn.exit784
                                        #   in Loop: Header=BB5_19 Depth=3
	movq	$9900, %rsi             # imm = 0x26AC
.LBB5_25:                               # %predict_insn.exit784
                                        #   in Loop: Header=BB5_19 Depth=3
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	.align	16, 0x90
.LBB5_26:                               # %for.inc113
                                        #   in Loop: Header=BB5_19 Depth=3
	movq	8(%r14), %r14
	testq	%r14, %r14
	jne	.LBB5_19
# BB#27:                                # %for.cond91.for.inc117.loopexit_crit_edge
                                        #   in Loop: Header=BB5_17 Depth=2
	movl	n_basic_blocks(%rip), %eax
.LBB5_28:                               # %for.inc117
                                        #   in Loop: Header=BB5_17 Depth=2
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB5_17
# BB#29:                                #   in Loop: Header=BB5_13 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$1, %rsi
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB5_30:                               # %if.end120
                                        #   in Loop: Header=BB5_13 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB5_134
# BB#31:                                # %lor.lhs.false122
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %rdi
	movq	%rsi, %r15
	callq	any_condjump_p
	movq	%r15, %rsi
	testq	%r13, %rax
	je	.LBB5_134
# BB#32:                                # %for.cond128.preheader
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r13, %r15
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	%r12, %r13
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %r12
	testq	%r12, %r12
	movabsq	$entry_exit_blocks+112, %rbx
	je	.LBB5_87
	.align	16, 0x90
.LBB5_33:                               # %for.body130
                                        #   Parent Loop BB5_13 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_79 Depth 3
	movq	24(%r12), %rax
	cmpq	%rbx, %rax
	je	.LBB5_37
# BB#34:                                # %lor.lhs.false133
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	40(%rax), %rax
	testq	%rax, %rax
	je	.LBB5_74
# BB#35:                                # %land.lhs.true137
                                        #   in Loop: Header=BB5_33 Depth=2
	cmpq	$0, 8(%rax)
	jne	.LBB5_74
# BB#36:                                # %land.lhs.true142
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	24(%rax), %rax
	cmpq	%rbx, %rax
	jne	.LBB5_74
	.align	16, 0x90
.LBB5_37:                               # %if.then147
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	16(%r12), %rax
	movq	8(%rax), %r14
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB5_74
# BB#38:                                # %if.end.i.i609
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB5_39
# BB#71:                                # %predict_insn.exit797
                                        #   in Loop: Header=BB5_33 Depth=2
	xorq	%rdi, %rdi
	movq	$18, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movl	48(%r12), %eax
	testq	$1, %rax
	movq	$4800, %rsi             # imm = 0x12C0
	je	.LBB5_73
# BB#72:                                # %predict_insn.exit797
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	$5200, %rsi             # imm = 0x1450
.LBB5_73:                               # %predict_insn.exit797
                                        #   in Loop: Header=BB5_33 Depth=2
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
.LBB5_74:                               # %if.end148
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	24(%r12), %rdi
	cmpq	%rbx, %rdi
	je	.LBB5_86
# BB#75:                                # %if.end148
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB5_86
# BB#76:                                # %land.lhs.true154
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	16(%r12), %r8
	movslq	88(%rdi), %r10
	movl	88(%r8), %r9d
	movq	$32, %rcx
	movq	%r9, %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r10,8), %rsi
	shlq	%cl, %rdx
	movq	%r9, %rcx
	shrq	$6, %rcx
	movq	16(%rsi,%rcx,8), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	movq	%r9, %rcx
	shlq	%cl, %rax
	testq	%rax, %rsi
	je	.LBB5_86
# BB#77:                                # %land.lhs.true172
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	%r10, %rax
	movq	(%r13,%rdx,8), %rdx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	16(%rdx,%rax), %rax
	movq	$1, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB5_86
# BB#78:                                # %if.then190
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	8(%rdi), %rcx
	movq	(%rdi), %rax
	movq	24(%rcx), %rcx
	jmp	.LBB5_79
	.align	16, 0x90
.LBB5_222:                              # %for.inc207
                                        #   in Loop: Header=BB5_79 Depth=3
	movq	24(%rax), %rax
.LBB5_79:                               # %if.then190
                                        #   Parent Loop BB5_13 Depth=1
                                        #     Parent Loop BB5_33 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%rcx, %rax
	je	.LBB5_86
# BB#80:                                # %for.body197
                                        #   in Loop: Header=BB5_79 Depth=3
	movl	(%rax), %edx
	andq	$67174399, %rdx         # imm = 0x400FFFF
	cmpq	$34, %rdx
	jne	.LBB5_222
# BB#81:                                # %if.then205
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	8(%r8), %r14
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB5_86
# BB#82:                                # %if.end.i.i619
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	%r14, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB5_39
# BB#83:                                # %predict_insn.exit810
                                        #   in Loop: Header=BB5_33 Depth=2
	xorq	%rdi, %rdi
	movq	$17, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movl	48(%r12), %eax
	testq	$1, %rax
	movq	$3000, %rsi             # imm = 0xBB8
	je	.LBB5_85
# BB#84:                                # %predict_insn.exit810
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	$7000, %rsi             # imm = 0x1B58
.LBB5_85:                               # %predict_insn.exit810
                                        #   in Loop: Header=BB5_33 Depth=2
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%r14), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r14)
	.align	16, 0x90
.LBB5_86:                               # %for.inc213
                                        #   in Loop: Header=BB5_33 Depth=2
	movq	8(%r12), %r12
	testq	%r12, %r12
	jne	.LBB5_33
.LBB5_87:                               # %for.end215
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	leaq	96(%rsp), %rsi
	callq	get_condition
	testq	%rax, %rax
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %r12
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r15, %r13
	je	.LBB5_134
# BB#88:                                # %if.end219
                                        #   in Loop: Header=BB5_13 Depth=1
	movzbl	(%rax), %edi
	movzbl	rtx_class(%rdi), %ecx
	cmpq	$60, %rcx
	jne	.LBB5_98
# BB#89:                                # %land.lhs.true226
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	8(%rax), %rcx
	movl	(%rcx), %ecx
	movabsq	$2147549183, %rdx       # imm = 0x8000FFFF
	andq	%rdx, %rcx
	movabsq	$2147483709, %rdx       # imm = 0x8000003D
	cmpq	%rdx, %rcx
	je	.LBB5_91
# BB#90:                                # %lor.lhs.false241
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	16(%rax), %rcx
	movl	(%rcx), %ecx
	movabsq	$2147549183, %rdx       # imm = 0x8000FFFF
	andq	%rdx, %rcx
	movabsq	$2147483709, %rdx       # imm = 0x8000003D
	cmpq	%rdx, %rcx
	jne	.LBB5_98
.LBB5_91:                               # %if.then256
                                        #   in Loop: Header=BB5_13 Depth=1
	cmpq	$102, %rdi
	jne	.LBB5_92
# BB#96:                                # %if.then266
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rsi, %r15
	movq	%rbx, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#97:                                # %predict_insn.exit644
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$13, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$8100, %rsi             # imm = 0x1FA4
	jmp	.LBB5_95
.LBB5_98:                               # %if.else269
                                        #   in Loop: Header=BB5_13 Depth=1
	cmpq	$101, %rdi
	jg	.LBB5_104
# BB#99:                                # %if.else269
                                        #   in Loop: Header=BB5_13 Depth=1
	cmpq	$54, %rdi
	jne	.LBB5_134
# BB#100:                               # %sw.bb
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r13, %rbx
	movq	const_int_rtx+512(%rip), %r13
	callq	any_condjump_p
	testq	%rbx, %rax
	je	.LBB5_39
# BB#101:                               # %predict_insn.exit657
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$4, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	cmpq	%r13, %r15
	movq	$10000, %rsi            # imm = 0x2710
	jne	.LBB5_103
# BB#102:                               # %select.mid
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rsi, %rsi
.LBB5_103:                              # %select.end
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	80(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB5_134
.LBB5_104:                              # %if.else269
                                        #   in Loop: Header=BB5_13 Depth=1
	addq	$-102, %rdi
	andq	%r13, %rdi
	cmpq	$17, %rdi
	ja	.LBB5_134
# BB#105:                               # %if.else269
                                        #   in Loop: Header=BB5_13 Depth=1
	jmpq	*.LJTI5_0(,%rdi,8)
.LBB5_113:                              # %sw.bb325
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	8(%rax), %rdi
	movzbl	2(%rdi), %ecx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$8, %rcx
	ja	.LBB5_115
# BB#114:                               # %sw.bb325
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$324, %rdx              # imm = 0x144
	jne	.LBB5_134
.LBB5_115:                              # %if.else359
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	16(%rax), %rdx
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rdx
	je	.LBB5_134
# BB#116:                               # %if.else359
                                        #   in Loop: Header=BB5_13 Depth=1
	cmpq	%rcx, %rdi
	je	.LBB5_134
# BB#117:                               # %if.else372
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %r15
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#118:                               # %predict_insn.exit681
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$15, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$7100, %rsi             # imm = 0x1BBC
	jmp	.LBB5_112
.LBB5_92:                               # %if.then256
                                        #   in Loop: Header=BB5_13 Depth=1
	cmpq	$103, %rdi
	jne	.LBB5_134
# BB#93:                                # %if.then261
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rsi, %r15
	movq	%rbx, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#94:                                # %predict_insn.exit632
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$13, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$1900, %rsi             # imm = 0x76C
.LBB5_95:                               # %for.inc418
                                        #   in Loop: Header=BB5_13 Depth=1
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	%r15, %rsi
	jmp	.LBB5_134
.LBB5_106:                              # %sw.bb275
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	8(%rax), %rdi
	movzbl	2(%rdi), %ecx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$8, %rcx
	ja	.LBB5_108
# BB#107:                               # %sw.bb275
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$324, %rdx              # imm = 0x144
	jne	.LBB5_134
.LBB5_108:                              # %if.else309
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	16(%rax), %rdx
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rdx
	je	.LBB5_134
# BB#109:                               # %if.else309
                                        #   in Loop: Header=BB5_13 Depth=1
	cmpq	%rcx, %rdi
	je	.LBB5_134
# BB#110:                               # %if.else322
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %r15
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#111:                               # %predict_insn.exit669
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$15, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$2900, %rsi             # imm = 0xB54
	jmp	.LBB5_112
.LBB5_128:                              # %sw.bb397
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	16(%rax), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB5_131
# BB#129:                               # %sw.bb397
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB5_131
# BB#130:                               # %sw.bb397
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	const_int_rtx+504(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_134
.LBB5_131:                              # %if.then415
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%r14, %r15
	movq	%rbx, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#132:                               # %predict_insn.exit729
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$14, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$7900, %rsi             # imm = 0x1EDC
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	%r15, %r14
	jmp	.LBB5_133
.LBB5_123:                              # %sw.bb377
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	16(%rax), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB5_126
# BB#124:                               # %sw.bb377
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB5_126
# BB#125:                               # %sw.bb377
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	const_int_rtx+504(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_134
.LBB5_126:                              # %if.then395
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %r15
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#127:                               # %predict_insn.exit717
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$14, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$2100, %rsi             # imm = 0x834
	jmp	.LBB5_112
.LBB5_121:                              # %sw.bb376
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %r15
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#122:                               # %predict_insn.exit705
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$1000, %rsi             # imm = 0x3E8
	jmp	.LBB5_112
.LBB5_119:                              # %sw.bb375
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	%rbx, %r15
	movq	%r15, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_39
# BB#120:                               # %predict_insn.exit693
                                        #   in Loop: Header=BB5_13 Depth=1
	xorq	%rdi, %rdi
	movq	$16, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	$9000, %rsi             # imm = 0x2328
.LBB5_112:                              # %for.inc418
                                        #   in Loop: Header=BB5_13 Depth=1
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%r15), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r15)
.LBB5_133:                              # %for.inc418
                                        #   in Loop: Header=BB5_13 Depth=1
	movq	80(%rsp), %rsi          # 8-byte Reload
	.align	16, 0x90
.LBB5_134:                              # %for.inc418
                                        #   in Loop: Header=BB5_13 Depth=1
	incq	%r14
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_13
# BB#57:                                # %for.cond421.preheader
	testq	%rax, %rax
	jle	.LBB5_160
# BB#58:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB5_59:                               # %for.body424
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_64 Depth 2
                                        #     Child Loop BB5_147 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	8(%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB5_159
# BB#60:                                # %land.lhs.true434
                                        #   in Loop: Header=BB5_59 Depth=1
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB5_159
# BB#61:                                # %if.then442
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	8(%rax), %rbx
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	rtl_dump_file(%rip), %rdi
	movq	%rax, %r14
	leaq	56(%rbx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB5_63
# BB#62:                                # %if.then.i735
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	88(%rax), %ecx
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movabsq	$.L.str2, %rsi
	callq	fprintf
.LBB5_63:                               # %for.cond.preheader.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r14
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	testq	%r14, %r14
	movq	$19, %rdx
	movq	$5000, %rbx             # imm = 0x1388
	movq	$5000, %rax             # imm = 0x1388
	movabsq	$0, %r15
	je	.LBB5_139
	.align	16, 0x90
.LBB5_64:                               # %for.body.i
                                        #   Parent Loop BB5_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%r14), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$1310720, %rcx          # imm = 0x140000
	jne	.LBB5_137
# BB#65:                                # %if.then8.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	8(%r14), %rsi
	movq	8(%rsi), %rcx
	movq	16(%rsi), %rsi
	movslq	8(%rcx), %r12
	movq	8(%rsi), %r15
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r12, %rsi
	jg	.LBB5_67
# BB#66:                                # %if.then8.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	%rdx, %r12
.LBB5_67:                               # %if.then8.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	%r15, %rcx
	jg	.LBB5_69
# BB#68:                                # %if.then8.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	%rbx, %rcx
.LBB5_69:                               # %if.then8.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	%r15, %rdx
	imulq	%rax, %rdx
	movq	$10000, %rcx            # imm = 0x2710
	subq	%rax, %rcx
	movq	$10000, %rbx            # imm = 0x2710
	subq	%r15, %rbx
	imulq	%rcx, %rbx
	addq	%rdx, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rbx
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	jne	.LBB5_135
# BB#70:                                #   in Loop: Header=BB5_64 Depth=2
	movq	$5000, %rax             # imm = 0x1388
	jmp	.LBB5_136
	.align	16, 0x90
.LBB5_135:                              # %if.else.i
                                        #   in Loop: Header=BB5_64 Depth=2
	andq	%r13, %rax
	movq	%rax, %rdi
	callq	int32_to_float64
	movq	%rax, 64(%rsp)          # 8-byte Spill
	andq	%r13, %r15
	movq	%r15, %rdi
	callq	int32_to_float64
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movabsq	$4666723172467343360, %rsi # imm = 0x40C3880000000000
	callq	float64_mul
	movq	%rax, %r15
	andq	%r13, %rbx
	movq	%rbx, %rdi
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movabsq	$4602678819172646912, %rsi # imm = 0x3FE0000000000000
	callq	float64_add
	movq	%rax, %rdi
	callq	float64_to_int64
.LBB5_136:                              # %for.inc.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	%r12, %rdx
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	24(%rsp), %r12          # 8-byte Reload
.LBB5_137:                              # %for.inc.i
                                        #   in Loop: Header=BB5_64 Depth=2
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB5_64
# BB#138:                               # %for.end.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	$-4, %rsi
	cmpq	$6, %rsi
	sbbq	%r15, %r15
	andq	$1, %r15
	testq	$1, 72(%rsp)            # 8-byte Folded Reload
	jne	.LBB5_140
.LBB5_139:                              # %if.then53.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	$3, %rdi
	movq	$1, %rcx
	movq	%rax, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB5_143
.LBB5_140:                              # %if.else54.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	cmpq	$6, %rsi
	sbbq	%r14, %r14
	andq	$1, %r14
	cmpq	$5, %rsi
	movabsq	$0, %rcx
	jbe	.LBB5_142
# BB#141:                               # %if.else54.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movabsq	$1, %rcx
.LBB5_142:                              # %if.else54.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	$1, %rdi
	movq	%rax, %rsi
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdx
	callq	dump_prediction
	movq	$2, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
.LBB5_143:                              # %if.end58.i
                                        #   in Loop: Header=BB5_59 Depth=1
	callq	dump_prediction
	testq	%r15, %r15
	movabsq	$0, %r14
	jne	.LBB5_145
# BB#144:                               # %if.end58.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB5_145:                              # %if.end58.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	dump_prediction
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	je	.LBB5_152
# BB#146:                               # %while.body.lr.ph.lr.ph.i
                                        #   in Loop: Header=BB5_59 Depth=1
	xorq	$1, %r15
	movq	56(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB5_147:                              # %while.body.i
                                        #   Parent Loop BB5_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$1310720, %rcx          # imm = 0x140000
	jne	.LBB5_151
# BB#148:                               # %if.then68.i
                                        #   in Loop: Header=BB5_147 Depth=2
	movq	8(%rax), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %rax
	movq	8(%rcx), %rdi
	movq	%rdi, %rcx
	movq	8(%rax), %rsi
	andq	%r13, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	%rcx, %rax
	movq	%r14, %rcx
	jne	.LBB5_150
# BB#149:                               # %if.then68.i
                                        #   in Loop: Header=BB5_147 Depth=2
	movabsq	$1, %rcx
.LBB5_150:                              # %if.then68.i
                                        #   in Loop: Header=BB5_147 Depth=2
	orq	%r15, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	dump_prediction
	movq	(%rbx), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rax, (%rbx)
	jne	.LBB5_147
	jmp	.LBB5_152
.LBB5_151:                              # %if.else97.i
                                        #   in Loop: Header=BB5_147 Depth=2
	movq	16(%rax), %rcx
	movq	%rax, %rbx
	addq	$16, %rbx
	testq	%rcx, %rcx
	movq	%rcx, %rax
	jne	.LBB5_147
	.align	16, 0x90
.LBB5_152:                              # %while.end.i
                                        #   in Loop: Header=BB5_59 Depth=1
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
	jne	.LBB5_159
# BB#153:                               # %if.then103.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	$32, %rcx
	movq	72(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	(%r14), %rcx
	movq	$3, %rdi
	movq	$16, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, (%r14)
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rcx
	movq	8(%rcx), %rax
	testq	%rax, %rax
	je	.LBB5_159
# BB#154:                               # %if.then114.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movl	48(%rcx), %edx
	andq	$1, %rdx
	movq	%rcx, %r8
	je	.LBB5_156
# BB#155:                               # %if.then114.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	%rax, %r8
.LBB5_156:                              # %if.then114.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	$10000, %rsi            # imm = 0x2710
	movq	72(%rsp), %rdi          # 8-byte Reload
	subq	%rdi, %rsi
	testq	%rdx, %rdx
	movl	%edi, 52(%r8)
	je	.LBB5_158
# BB#157:                               # %if.then114.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movq	%rcx, %rax
.LBB5_158:                              # %if.then114.i
                                        #   in Loop: Header=BB5_59 Depth=1
	movl	%esi, 52(%rax)
	.align	16, 0x90
.LBB5_159:                              # %for.inc453
                                        #   in Loop: Header=BB5_59 Depth=1
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_59
.LBB5_160:                              # %for.end455
	movq	%r12, %rdi
	callq	free
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	free
	callq	mark_dfs_back_edges
	movl	flag_branch_probabilities(%rip), %eax
	movl	n_basic_blocks(%rip), %r8d
	movq	$32, %rcx
	testq	%rax, %rax
	je	.LBB5_161
# BB#171:                               # %if.then.i740
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rbx
	testq	%rax, %rax
	jle	.LBB5_176
# BB#172:                               # %for.body.lr.ph.i.i
	movq	$1, %rsi
	movq	basic_block_info(%rip), %rcx
	movq	$4, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%r8, %rax
	andq	%rdi, %rax
	.align	16, 0x90
.LBB5_173:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx,%rdx,8), %rbx
	movq	96(%rbx), %rbx
	cmpq	%rsi, %rbx
	jge	.LBB5_175
# BB#174:                               # %for.body.i.i
                                        #   in Loop: Header=BB5_173 Depth=1
	movq	%rsi, %rbx
.LBB5_175:                              # %for.body.i.i
                                        #   in Loop: Header=BB5_173 Depth=1
	leaq	-3(%rdx), %rsi
	andq	%rdi, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	movq	%rbx, %rsi
	jne	.LBB5_173
.LBB5_176:                              # %for.cond7.preheader.i.i
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	$-1, %r8
	jl	.LBB5_221
# BB#177:                               # %for.body9.lr.ph.i.i
	movq	basic_block_info(%rip), %r8
	movq	$63, %rcx
	movq	%rbx, %rsi
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movq	$1, %rcx
	sarq	%cl, %rsi
	movq	$-2, %rdi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movabsq	$entry_exit_blocks, %r10
	movabsq	$4294967294, %r11       # imm = 0xFFFFFFFE
	movabsq	$entry_exit_blocks+112, %r14
	.align	16, 0x90
.LBB5_178:                              # %for.body9.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	andq	%r9, %rax
	cmpq	%r11, %rax
	movq	%r10, %rcx
	je	.LBB5_182
# BB#179:                               # %for.body9.i.i
                                        #   in Loop: Header=BB5_178 Depth=1
	cmpq	%r9, %rax
	jne	.LBB5_181
# BB#180:                               # %if.then13.i.i
                                        #   in Loop: Header=BB5_178 Depth=1
	movq	%r14, %rcx
	jmp	.LBB5_182
	.align	16, 0x90
.LBB5_181:                              # %if.else14.i.i
                                        #   in Loop: Header=BB5_178 Depth=1
	movq	32(%r8,%rdi,8), %rcx
.LBB5_182:                              # %if.end19.i.i
                                        #   in Loop: Header=BB5_178 Depth=1
	movq	96(%rcx), %rax
	imulq	$10000, %rax, %rax      # imm = 0x2710
	addq	%rsi, %rax
	cqto
	idivq	%rbx
	movl	%eax, 104(%rcx)
	incq	%rdi
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_178
	jmp	.LBB5_221
.LBB5_161:                              # %for.cond.preheader.i739
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	testq	%r8, %r8
	jle	.LBB5_190
# BB#162:                               # %for.body.lr.ph.i
	movq	basic_block_info(%rip), %rsi
	xorq	%r14, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$2147483648, %r12       # imm = 0x80000000
	.align	16, 0x90
.LBB5_163:                              # %for.body.i745
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_167 Depth 2
                                        #     Child Loop BB5_170 Depth 2
	movq	32(%rsi,%r14,8), %rcx
	movq	8(%rcx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB5_166
# BB#164:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	%rbx, %rdi
	callq	any_condjump_p
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi,%r14,8), %rcx
	testq	%r15, %rax
	je	.LBB5_166
# BB#165:                               # %lor.lhs.false4.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	40(%rcx), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB5_183
	.align	16, 0x90
.LBB5_166:                              # %if.then10.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	40(%rcx), %rdi
	xorq	%rdx, %rdx
	testq	%rdi, %rdi
	movq	%rdi, %rax
	je	.LBB5_189
	.align	16, 0x90
.LBB5_167:                              # %for.body18.i
                                        #   Parent Loop BB5_163 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	52(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB5_189
# BB#168:                               # %for.inc.i748
                                        #   in Loop: Header=BB5_167 Depth=2
	movq	8(%rax), %rax
	incq	%rdx
	testq	%rax, %rax
	jne	.LBB5_167
# BB#169:                               # %for.body33.lr.ph.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rdx, %rax
	andq	%r12, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$33, %rcx
	sarq	%cl, %rax
	addq	$10000, %rax            # imm = 0x2710
	cqto
	idivq	%rbx
	.align	16, 0x90
.LBB5_170:                              # %for.body33.i
                                        #   Parent Loop BB5_163 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%eax, 52(%rdi)
	movq	8(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB5_170
	jmp	.LBB5_189
.LBB5_183:                              # %if.else.i755
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	$16, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	basic_block_info(%rip), %rsi
	movq	32(%rsi,%r14,8), %rcx
	movq	40(%rcx), %rcx
	movq	8(%rax), %rax
	movl	48(%rcx), %edx
	movq	8(%rax), %rax
	testq	%rdx, %rdx
	jne	.LBB5_185
# BB#184:                               # %if.end53.thread.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	8(%rcx), %rdx
	jmp	.LBB5_188
.LBB5_185:                              # %if.end53.i
                                        #   in Loop: Header=BB5_163 Depth=1
	testq	$1, %rdx
	jne	.LBB5_187
# BB#186:                               #   in Loop: Header=BB5_163 Depth=1
	movq	%rcx, %rdx
	jmp	.LBB5_188
.LBB5_187:                              # %if.then62.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movq	8(%rcx), %rdi
	movq	%rcx, %rdx
	movq	%rdi, %rcx
.LBB5_188:                              # %if.end64.i
                                        #   in Loop: Header=BB5_163 Depth=1
	movl	%eax, 52(%rcx)
	movq	$10000, %rcx            # imm = 0x2710
	subq	%rax, %rcx
	movl	%ecx, 52(%rdx)
	.align	16, 0x90
.LBB5_189:                              # %for.inc68.i
                                        #   in Loop: Header=BB5_163 Depth=1
	incq	%r14
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_163
.LBB5_190:                              # %for.end70.i
	movq	entry_exit_blocks+40(%rip), %rax
	movq	$10000, %rcx            # imm = 0x2710
	movl	%ecx, 52(%rax)
	movq	$24, %rdi
	callq	alloc_aux_for_blocks
	movq	$16, %rdi
	callq	alloc_aux_for_edges
	movslq	n_basic_blocks(%rip), %rax
	cmpq	$-1, %rax
	jl	.LBB5_200
# BB#191:
	movq	$-2, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$4294967294, %r13       # imm = 0xFFFFFFFE
	movabsq	$4666723172467343360, %rbx # imm = 0x40C3880000000000
	.align	16, 0x90
.LBB5_192:                              # %for.body75.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_198 Depth 2
	movq	%r14, %rcx
	andq	%r15, %rcx
	cmpq	%r13, %rcx
	movabsq	$entry_exit_blocks, %rax
	je	.LBB5_196
# BB#193:                               # %for.body75.i
                                        #   in Loop: Header=BB5_192 Depth=1
	cmpq	%r15, %rcx
	jne	.LBB5_195
# BB#194:                               # %if.then84.i
                                        #   in Loop: Header=BB5_192 Depth=1
	movabsq	$entry_exit_blocks+112, %rax
	jmp	.LBB5_196
	.align	16, 0x90
.LBB5_195:                              # %if.else85.i
                                        #   in Loop: Header=BB5_192 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%r14,8), %rax
.LBB5_196:                              # %if.end91.i
                                        #   in Loop: Header=BB5_192 Depth=1
	movq	80(%rax), %rcx
	movzbl	16(%rcx), %edx
	andq	$254, %rdx
	movb	%dl, 16(%rcx)
	movq	40(%rax), %r12
	jmp	.LBB5_198
	.align	16, 0x90
.LBB5_197:                              # %for.body97.i
                                        #   in Loop: Header=BB5_198 Depth=2
	movl	52(%r12), %edi
	callq	int32_to_float64
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movq	40(%r12), %rcx
	movq	%rax, (%rcx)
	movq	8(%r12), %r12
.LBB5_198:                              # %for.body97.i
                                        #   Parent Loop BB5_192 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r12, %r12
	jne	.LBB5_197
# BB#199:                               # %for.inc105.i
                                        #   in Loop: Header=BB5_192 Depth=1
	incq	%r14
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_192
.LBB5_200:                              # %for.end107.i
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rdi
	callq	estimate_loops_at_level
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB5_203
# BB#201:
	movq	$4, %rax
	.align	16, 0x90
.LBB5_202:                              # %for.body111.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%rax,8), %rcx
	movq	80(%rcx), %rcx
	movzbl	16(%rcx), %edx
	orq	$1, %rdx
	movb	%dl, 16(%rcx)
	movslq	n_basic_blocks(%rip), %rdx
	leaq	-3(%rax), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rax
	cmpq	%rdx, %rsi
	jl	.LBB5_202
.LBB5_203:                              # %for.end122.i
	movq	entry_exit_blocks+80(%rip), %rax
	movzbl	16(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 16(%rax)
	movq	entry_exit_blocks+192(%rip), %rax
	movzbl	16(%rax), %ecx
	movabsq	$entry_exit_blocks, %rdi
	orq	$1, %rcx
	movb	%cl, 16(%rax)
	callq	propagate_freq
	movslq	n_basic_blocks(%rip), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	testq	%rax, %rax
	jle	.LBB5_213
# BB#204:                               # %for.body134.lr.ph.i
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	$4, %rbx
	movq	basic_block_info(%rip), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_205:                              # %for.body134.i
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %rax
	movq	80(%rax), %r12
	movq	(%r12), %r14
	movq	%r14, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	je	.LBB5_207
# BB#206:                               # %for.body134.i
                                        #   in Loop: Header=BB5_205 Depth=1
	movabsq	$1, %r15
.LBB5_207:                              # %for.body134.i
                                        #   in Loop: Header=BB5_205 Depth=1
	movq	%r14, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jg	.LBB5_209
# BB#208:                               # %for.body134.i
                                        #   in Loop: Header=BB5_205 Depth=1
	movabsq	$1, %rax
.LBB5_209:                              # %for.body134.i
                                        #   in Loop: Header=BB5_205 Depth=1
	orq	%rax, %r15
	movq	80(%rsp), %rcx          # 8-byte Reload
	jne	.LBB5_211
# BB#210:                               # %if.then142.i
                                        #   in Loop: Header=BB5_205 Depth=1
	movq	(%r12), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB5_211:                              # %for.inc150.i
                                        #   in Loop: Header=BB5_205 Depth=1
	leaq	-3(%rbx), %rax
	andq	%r13, %rax
	andq	%r13, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB5_205
# BB#212:                               # %for.cond131.for.cond153.preheader_crit_edge.i
	movl	n_basic_blocks(%rip), %eax
	movq	88(%rsp), %r13          # 8-byte Reload
.LBB5_213:                              # %for.cond153.preheader.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$-1, %rax
	jl	.LBB5_220
# BB#214:                               # %for.body156.lr.ph.i
	movq	$-2, %rbx
	movq	basic_block_info(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$4602678819172646912, %r12 # imm = 0x3FE0000000000000
	.align	16, 0x90
.LBB5_215:                              # %for.body156.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	andq	%r14, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	movabsq	$entry_exit_blocks, %r15
	je	.LBB5_219
# BB#216:                               # %for.body156.i
                                        #   in Loop: Header=BB5_215 Depth=1
	cmpq	%r14, %rax
	jne	.LBB5_218
# BB#217:                               # %if.then164.i
                                        #   in Loop: Header=BB5_215 Depth=1
	movabsq	$entry_exit_blocks+112, %r15
	jmp	.LBB5_219
	.align	16, 0x90
.LBB5_218:                              # %if.else165.i
                                        #   in Loop: Header=BB5_215 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	32(%rax,%rbx,8), %r15
.LBB5_219:                              # %if.end171.i
                                        #   in Loop: Header=BB5_215 Depth=1
	movq	80(%r15), %rax
	movq	(%rax), %rdi
	movabsq	$4666723172467343360, %rsi # imm = 0x40C3880000000000
	callq	float64_mul
	movq	%rax, 104(%rsp)
	movq	104(%rsp), %rdi
	movq	%r13, %rsi
	callq	float64_div
	movq	%rax, 104(%rsp)
	movq	104(%rsp), %rdi
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, 104(%rsp)
	movq	104(%rsp), %rdi
	callq	float64_to_int64
	movl	%eax, 104(%r15)
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_215
.LBB5_220:                              # %for.end180.i
	callq	free_aux_for_blocks
	callq	free_aux_for_edges
.LBB5_221:                              # %estimate_bb_frequencies.exit
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_10:                               # %if.then.i761
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.predict_insn, %rdx
	movq	$101, %rsi
	callq	fancy_abort
.LBB5_39:                               # %if.then.i787
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.predict_insn, %rcx
	movq	$101, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp66:
	.size	estimate_probability, .Ltmp66-estimate_probability
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_113
	.quad	.LBB5_106
	.quad	.LBB5_128
	.quad	.LBB5_128
	.quad	.LBB5_123
	.quad	.LBB5_123
	.quad	.LBB5_134
	.quad	.LBB5_134
	.quad	.LBB5_134
	.quad	.LBB5_134
	.quad	.LBB5_121
	.quad	.LBB5_119
	.quad	.LBB5_106
	.quad	.LBB5_134
	.quad	.LBB5_134
	.quad	.LBB5_134
	.quad	.LBB5_134
	.quad	.LBB5_113

	.text
	.globl	expected_value_to_br_prob
	.align	16, 0x90
	.type	expected_value_to_br_prob,@function
expected_value_to_br_prob:              # @expected_value_to_br_prob
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp73:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp74:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp75:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp76:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp77:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp78:
	.cfi_def_cfa_offset 64
.Ltmp79:
	.cfi_offset %rbx, -48
.Ltmp80:
	.cfi_offset %r12, -40
.Ltmp81:
	.cfi_offset %r13, -32
.Ltmp82:
	.cfi_offset %r14, -24
.Ltmp83:
	.cfi_offset %r15, -16
	callq	get_insns
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB6_29
# BB#1:
	xorq	%r14, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	.align	16, 0x90
.LBB6_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	je	.LBB6_8
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB6_2 Depth=1
	cmpq	$36, %rax
	jne	.LBB6_5
# BB#4:                                 #   in Loop: Header=BB6_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB6_28
	.align	16, 0x90
.LBB6_8:                                # %sw.bb8
                                        #   in Loop: Header=BB6_2 Depth=1
	testq	%r12, %r12
	jne	.LBB6_10
# BB#9:                                 #   in Loop: Header=BB6_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB6_28
	.align	16, 0x90
.LBB6_5:                                # %for.body
                                        #   in Loop: Header=BB6_2 Depth=1
	cmpq	$37, %rax
	jne	.LBB6_18
# BB#6:                                 # %sw.bb
                                        #   in Loop: Header=BB6_2 Depth=1
	movl	40(%rbx), %eax
	movabsq	$4294967217, %rcx       # imm = 0xFFFFFFB1
	cmpq	%rcx, %rax
	jne	.LBB6_28
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	32(%rbx), %r12
	movq	8(%r12), %r14
	movq	%rbx, %rdi
	callq	delete_insn
	jmp	.LBB6_28
.LBB6_10:                               # %lor.lhs.false13
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	%rbx, %rdi
	callq	any_condjump_p
	testq	%r15, %rax
	je	.LBB6_28
# BB#11:                                # %sw.epilog
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	%rbx, %rdi
	callq	pc_set
	movq	16(%rax), %rax
	movq	8(%rax), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %r8
	callq	canonicalize_condition
	testq	%rax, %rax
	je	.LBB6_28
# BB#12:                                # %lor.lhs.false32
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	8(%rax), %rcx
	cmpq	%r14, %rcx
	jne	.LBB6_28
# BB#13:                                # %lor.lhs.false37
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	16(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB6_27
# BB#14:                                # %if.end45
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	16(%r12), %rdx
	movzbl	(%rax), %edi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	simplify_rtx
	movq	const_true_rtx(%rip), %r13
	movq	%rax, %r15
	cmpq	%r13, %r15
	je	.LBB6_22
# BB#15:                                # %if.end45
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB6_16
.LBB6_22:                               # %if.end60
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	%rbx, %rdi
	callq	any_condjump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_23
# BB#24:                                # %predict_insn.exit
                                        #   in Loop: Header=BB6_2 Depth=1
	xorq	%rdi, %rdi
	movq	$6, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rsp)           # 8-byte Spill
	cmpq	%r13, %r15
	movq	$9001, %rsi             # imm = 0x2329
	je	.LBB6_26
# BB#25:                                # %predict_insn.exit
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	$999, %rsi              # imm = 0x3E7
.LBB6_26:                               # %predict_insn.exit
                                        #   in Loop: Header=BB6_2 Depth=1
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$65, %rdi
	xorq	%rsi, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$20, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB6_27:                               # %for.inc
                                        #   in Loop: Header=BB6_2 Depth=1
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB6_28
.LBB6_18:                               # %sw.default
                                        #   in Loop: Header=BB6_2 Depth=1
	testq	%r12, %r12
	jne	.LBB6_20
# BB#19:                                #   in Loop: Header=BB6_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB6_28
.LBB6_20:                               # %land.lhs.true
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	reg_set_p
	testq	%r15, %rax
	je	.LBB6_28
# BB#21:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_2 Depth=1
	xorq	%r12, %r12
	.align	16, 0x90
.LBB6_28:                               # %for.inc
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_2
.LBB6_29:                               # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_16:                               # %if.then59
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expected_value_to_br_prob, %rcx
	movq	$608, %rsi              # imm = 0x260
	jmp	.LBB6_17
.LBB6_23:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.predict_insn, %rcx
	movq	$101, %rsi
.LBB6_17:                               # %if.then59
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp84:
	.size	expected_value_to_br_prob, .Ltmp84-expected_value_to_br_prob
	.cfi_endproc

	.globl	expensive_function_p
	.align	16, 0x90
	.type	expensive_function_p,@function
expensive_function_p:                   # @expensive_function_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp93:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp94:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp96:
	.cfi_def_cfa_offset 64
.Ltmp97:
	.cfi_offset %rbx, -48
.Ltmp98:
	.cfi_offset %r12, -40
.Ltmp99:
	.cfi_offset %r13, -32
.Ltmp100:
	.cfi_offset %r14, -24
.Ltmp101:
	.cfi_offset %r15, -16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$10001, %rax            # imm = 0x2711
	jge	.LBB7_13
# BB#1:                                 # %if.end
	movl	entry_exit_blocks+104(%rip), %ecx
	testq	%rcx, %rcx
	movq	$1, %rax
	je	.LBB7_12
# BB#2:                                 # %if.end3
	movslq	n_basic_blocks(%rip), %rax
	xorq	%r12, %r12
	testq	%rax, %rax
	jle	.LBB7_11
# BB#3:
	imulq	%rdi, %rcx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB7_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_5 Depth 2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r12,8), %r15
	movq	8(%r15), %rcx
	movq	(%r15), %rbx
	movq	24(%rcx), %rcx
	cmpq	%rcx, %rbx
	je	.LBB7_10
	.align	16, 0x90
.LBB7_5:                                # %for.body9
                                        #   Parent Loop BB7_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	active_insn_p
	testq	%r13, %rax
	je	.LBB7_8
# BB#6:                                 # %if.then10
                                        #   in Loop: Header=BB7_5 Depth=2
	movl	104(%r15), %eax
	addq	%rax, %r14
	movq	%r14, %rax
	andq	%r13, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	ja	.LBB7_7
.LBB7_8:                                # %for.inc
                                        #   in Loop: Header=BB7_5 Depth=2
	movq	8(%r15), %rax
	movq	24(%rbx), %rbx
	movq	24(%rax), %rax
	cmpq	%rax, %rbx
	jne	.LBB7_5
# BB#9:                                 # %for.cond6.for.inc18_crit_edge
                                        #   in Loop: Header=BB7_4 Depth=1
	movl	n_basic_blocks(%rip), %eax
.LBB7_10:                               # %for.inc18
                                        #   in Loop: Header=BB7_4 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB7_4
.LBB7_11:
	xorq	%rax, %rax
	jmp	.LBB7_12
.LBB7_7:
	movq	$1, %rax
.LBB7_12:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_13:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expensive_function_p, %rcx
	movq	$828, %rsi              # imm = 0x33C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp102:
	.size	expensive_function_p, .Ltmp102-expensive_function_p
	.cfi_endproc

	.align	16, 0x90
	.type	estimate_loops_at_level,@function
estimate_loops_at_level:                # @estimate_loops_at_level
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp109:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp110:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp111:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp112:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp113:
	.cfi_def_cfa_offset 48
.Ltmp114:
	.cfi_offset %rbx, -48
.Ltmp115:
	.cfi_offset %r12, -40
.Ltmp116:
	.cfi_offset %r13, -32
.Ltmp117:
	.cfi_offset %r14, -24
.Ltmp118:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB8_22
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r14, %r15
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_3 Depth 2
                                        #     Child Loop BB8_5 Depth 2
                                        #     Child Loop BB8_10 Depth 2
                                        #       Child Loop BB8_13 Depth 3
                                        #         Child Loop BB8_15 Depth 4
	movq	136(%r15), %rdi
	callq	estimate_loops_at_level
	movq	16(%r15), %rcx
	movq	8(%r15), %rax
	addq	$40, %rcx
	.align	16, 0x90
.LBB8_3:                                # %for.cond1
                                        #   Parent Loop BB8_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rcx), %rdx
	movq	24(%rdx), %rsi
	leaq	8(%rdx), %rcx
	cmpq	%rax, %rsi
	jne	.LBB8_3
# BB#4:                                 # %for.end
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	40(%rdx), %rax
	movzbl	8(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 8(%rax)
	movl	152(%r15), %eax
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB8_8
	.align	16, 0x90
.LBB8_5:                                # %for.cond4
                                        #   Parent Loop BB8_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	144(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB8_8
# BB#6:                                 # %for.body6
                                        #   in Loop: Header=BB8_5 Depth=2
	movq	8(%rcx), %rdx
	movq	8(%r15), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB8_5
# BB#7:                                 # %for.inc61.loopexit
                                        #   in Loop: Header=BB8_2 Depth=1
	addq	$144, %r15
	jmp	.LBB8_21
	.align	16, 0x90
.LBB8_8:                                # %if.end17
                                        #   in Loop: Header=BB8_2 Depth=1
	testq	%rax, %rax
	movq	%r14, %r9
	jne	.LBB8_10
# BB#9:                                 # %if.end17
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%r15, %r9
	jmp	.LBB8_10
	.align	16, 0x90
.LBB8_19:                               # %for.inc57
                                        #   in Loop: Header=BB8_10 Depth=2
	movq	144(%r9), %r9
.LBB8_10:                               # %if.end17
                                        #   Parent Loop BB8_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB8_13 Depth 3
                                        #         Child Loop BB8_15 Depth 4
	movq	144(%r15), %rax
	movq	8(%r15), %rdi
	cmpq	%rax, %r9
	je	.LBB8_20
# BB#11:                                # %for.body23
                                        #   in Loop: Header=BB8_10 Depth=2
	movq	8(%r9), %rax
	cmpq	%rax, %rdi
	jne	.LBB8_19
# BB#12:                                # %do.body
                                        #   in Loop: Header=BB8_10 Depth=2
	movq	64(%r9), %r10
	movl	4(%r10), %r8d
	xorq	%r11, %r11
	testq	%r8, %r8
	je	.LBB8_19
	.align	16, 0x90
.LBB8_13:                               # %for.body31
                                        #   Parent Loop BB8_2 Depth=1
                                        #     Parent Loop BB8_10 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB8_15 Depth 4
	movq	16(%r10,%r11,8), %rdx
	testq	%rdx, %rdx
	je	.LBB8_18
# BB#14:                                # %for.body36.preheader
                                        #   in Loop: Header=BB8_13 Depth=3
	movq	$6, %rcx
	movq	%r11, %r13
	shlq	%cl, %r13
	movq	$1, %rbx
	.align	16, 0x90
.LBB8_15:                               # %for.body36
                                        #   Parent Loop BB8_2 Depth=1
                                        #     Parent Loop BB8_10 Depth=2
                                        #       Parent Loop BB8_13 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-1(%rbx), %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rdx, %rsi
	je	.LBB8_17
# BB#16:                                # %if.then38
                                        #   in Loop: Header=BB8_15 Depth=4
	addq	%r13, %rax
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdi,%rax,8), %rax
	movq	80(%rax), %rax
	movzbl	16(%rax), %ecx
	notq	%rsi
	orq	$1, %rcx
	andq	%rsi, %rdx
	movb	%cl, 16(%rax)
	je	.LBB8_18
.LBB8_17:                               # %for.inc50
                                        #   in Loop: Header=BB8_15 Depth=4
	movq	%rbx, %rax
	andq	%r12, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB8_15
.LBB8_18:                               # %for.inc53
                                        #   in Loop: Header=BB8_13 Depth=3
	incq	%r11
	movq	%r11, %rax
	andq	%r12, %rax
	cmpq	%r8, %rax
	jne	.LBB8_13
	jmp	.LBB8_19
	.align	16, 0x90
.LBB8_20:                               # %for.end59
                                        #   in Loop: Header=BB8_2 Depth=1
	addq	$144, %r15
	callq	propagate_freq
.LBB8_21:                               # %for.inc61
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB8_2
.LBB8_22:                               # %for.end63
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp119:
	.size	estimate_loops_at_level, .Ltmp119-estimate_loops_at_level
	.cfi_endproc

	.align	16, 0x90
	.type	propagate_freq,@function
propagate_freq:                         # @propagate_freq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp126:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp127:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp128:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp129:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp130:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp131:
	.cfi_def_cfa_offset 80
.Ltmp132:
	.cfi_offset %rbx, -48
.Ltmp133:
	.cfi_offset %r12, -40
.Ltmp134:
	.cfi_offset %r13, -32
.Ltmp135:
	.cfi_offset %r14, -24
.Ltmp136:
	.cfi_offset %r15, -16
	movslq	n_basic_blocks(%rip), %rax
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB9_16
# BB#1:
	xorq	%r13, %r13
	movabsq	$.L.str1, %r14
	.align	16, 0x90
.LBB9_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_5 Depth 2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r13,8), %r15
	movq	80(%r15), %rcx
	movzbl	16(%rcx), %edx
	testq	$1, %rdx
	je	.LBB9_15
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	32(%r15), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB9_14
# BB#4:                                 # %for.body5.lr.ph
                                        #   in Loop: Header=BB9_2 Depth=1
	xorq	%r12, %r12
	.align	16, 0x90
.LBB9_5:                                # %for.body5
                                        #   Parent Loop BB9_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rbx), %rax
	movq	80(%rax), %rcx
	movzbl	16(%rcx), %ecx
	andq	$1, %rcx
	je	.LBB9_12
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_5 Depth=2
	movzbl	48(%rbx), %edx
	testq	$32, %rdx
	jne	.LBB9_8
# BB#7:                                 # %if.then14
                                        #   in Loop: Header=BB9_5 Depth=2
	incq	%r12
	jmp	.LBB9_12
	.align	16, 0x90
.LBB9_8:                                # %if.else
                                        #   in Loop: Header=BB9_5 Depth=2
	testq	$255, %rcx
	je	.LBB9_12
# BB#9:                                 # %if.else
                                        #   in Loop: Header=BB9_5 Depth=2
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB9_12
# BB#10:                                # %land.lhs.true25
                                        #   in Loop: Header=BB9_5 Depth=2
	movq	40(%rbx), %rcx
	movzbl	8(%rcx), %ecx
	testq	$1, %rcx
	jne	.LBB9_12
# BB#11:                                # %if.then32
                                        #   in Loop: Header=BB9_5 Depth=2
	movl	88(%r15), %ecx
	movl	88(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
	.align	16, 0x90
.LBB9_12:                               # %for.inc
                                        #   in Loop: Header=BB9_5 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB9_5
# BB#13:                                # %for.cond3.for.end_crit_edge
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	80(%r15), %rcx
.LBB9_14:                               # %for.end
                                        #   in Loop: Header=BB9_2 Depth=1
	movl	%r12d, 20(%rcx)
	movl	n_basic_blocks(%rip), %eax
.LBB9_15:                               # %for.inc38
                                        #   in Loop: Header=BB9_2 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB9_2
.LBB9_16:                               # %for.end40
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	80(%r15), %rax
	testq	%r15, %r15
	movabsq	$4607182418800017408, %rcx # imm = 0x3FF0000000000000
	movq	%rcx, (%rax)
	je	.LBB9_46
# BB#17:
	movabsq	$4666723172467343360, %r12 # imm = 0x40C3880000000000
	movq	%r15, %r13
	movq	%r15, %r14
	.align	16, 0x90
.LBB9_19:                               # %for.body44
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_21 Depth 2
                                        #     Child Loop BB9_32 Depth 2
                                        #     Child Loop BB9_36 Depth 2
	movq	80(%r13), %rcx
	movq	8(%rcx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	cmpq	%r15, %r13
	movq	$0, 8(%rcx)
	je	.LBB9_30
# BB#20:                                # %if.then50
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	32(%r13), %rbx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	testq	%rbx, %rbx
	movabsq	$0, %r15
	je	.LBB9_27
	.align	16, 0x90
.LBB9_21:                               # %for.body54
                                        #   Parent Loop BB9_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rbx), %rax
	movzbl	8(%rax), %ecx
	testq	$1, %rcx
	jne	.LBB9_22
# BB#23:                                # %if.else64
                                        #   in Loop: Header=BB9_21 Depth=2
	movzbl	48(%rbx), %eax
	testq	$32, %rax
	jne	.LBB9_25
# BB#24:                                # %if.then68
                                        #   in Loop: Header=BB9_21 Depth=2
	movl	52(%rbx), %edi
	callq	int32_to_float64
	movq	16(%rbx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx), %rsi
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_div
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB9_25
	.align	16, 0x90
.LBB9_22:                               # %if.then62
                                        #   in Loop: Header=BB9_21 Depth=2
	movq	(%rax), %rsi
	movq	%r15, %rdi
	callq	float64_add
	movq	%rax, %r15
.LBB9_25:                               # %for.inc75
                                        #   in Loop: Header=BB9_21 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB9_21
# BB#26:                                # %for.cond52.for.end77_crit_edge
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	80(%r13), %rcx
.LBB9_27:                               # %for.end77
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	%rcx, %rbx
	movq	%r15, %rdi
	movabsq	$4607181518080091934, %rsi # imm = 0x3FEFFF2E48E8A71E
	callq	__gtdf2
	movq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	movq	%r15, %rsi
	callq	float64_sub
	testq	%r12, %r12
	movabsq	$4547007122018942976, %rsi # imm = 0x3F1A36E2EB1C4000
	jg	.LBB9_29
# BB#28:                                # %for.end77
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	%rax, %rsi
.LBB9_29:                               # %for.end77
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	float64_div
	movq	%rax, (%rbx)
	movq	80(%r13), %rcx
	movq	8(%rsp), %r15           # 8-byte Reload
	movabsq	$4666723172467343360, %r12 # imm = 0x40C3880000000000
.LBB9_30:                               # %if.end85
                                        #   in Loop: Header=BB9_19 Depth=1
	movzbl	16(%rcx), %eax
	andq	$254, %rax
	movb	%al, 16(%rcx)
	movq	40(%r13), %rbx
	testq	%rbx, %rbx
	jne	.LBB9_32
# BB#31:                                #   in Loop: Header=BB9_19 Depth=1
	movq	16(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB9_18
	.align	16, 0x90
.LBB9_32:                               # %for.body91
                                        #   Parent Loop BB9_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rax
	cmpq	%r15, %rax
	jne	.LBB9_34
# BB#33:                                # %if.then94
                                        #   in Loop: Header=BB9_32 Depth=2
	movl	52(%rbx), %edi
	callq	int32_to_float64
	movq	80(%r13), %rcx
	movq	(%rcx), %rsi
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_div
	movq	40(%rbx), %rcx
	movq	%rax, (%rcx)
.LBB9_34:                               # %for.inc104
                                        #   in Loop: Header=BB9_32 Depth=2
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB9_32
# BB#35:                                # %for.cond107.preheader
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	40(%r13), %rax
	testq	%rax, %rax
	movq	%r14, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rbx          # 8-byte Reload
	je	.LBB9_18
	.align	16, 0x90
.LBB9_36:                               # %for.body109
                                        #   Parent Loop BB9_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rax), %edx
	testq	$32, %rdx
	jne	.LBB9_37
# BB#38:                                # %land.lhs.true113
                                        #   in Loop: Header=BB9_36 Depth=2
	movq	24(%rax), %r14
	movq	80(%r14), %rdx
	movl	20(%rdx), %esi
	testq	%rsi, %rsi
	jne	.LBB9_40
# BB#39:                                #   in Loop: Header=BB9_36 Depth=2
	movq	%rcx, %r14
	jmp	.LBB9_45
	.align	16, 0x90
.LBB9_37:                               #   in Loop: Header=BB9_36 Depth=2
	movq	%rcx, %r14
	jmp	.LBB9_45
	.align	16, 0x90
.LBB9_40:                               # %if.then118
                                        #   in Loop: Header=BB9_36 Depth=2
	decq	%rsi
	testq	%rdi, %rsi
	movl	%esi, 20(%rdx)
	jne	.LBB9_41
# BB#42:                                # %if.then126
                                        #   in Loop: Header=BB9_36 Depth=2
	testq	%rbx, %rbx
	jne	.LBB9_44
# BB#43:                                #   in Loop: Header=BB9_36 Depth=2
	movq	%r14, %rbx
	jmp	.LBB9_45
.LBB9_41:                               #   in Loop: Header=BB9_36 Depth=2
	movq	%rcx, %r14
	jmp	.LBB9_45
.LBB9_44:                               # %if.else130
                                        #   in Loop: Header=BB9_36 Depth=2
	movq	80(%rcx), %rcx
	movq	%r14, 8(%rcx)
	.align	16, 0x90
.LBB9_45:                               # %for.inc138
                                        #   in Loop: Header=BB9_36 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	movq	%r14, %rcx
	jne	.LBB9_36
.LBB9_18:                               # %for.cond42.loopexit
                                        #   in Loop: Header=BB9_19 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %r13
	jne	.LBB9_19
.LBB9_46:                               # %for.end142
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp137:
	.size	propagate_freq, .Ltmp137-propagate_freq
	.cfi_endproc

	.align	16, 0x90
	.type	dump_prediction,@function
dump_prediction:                        # @dump_prediction
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp144:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp145:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp146:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp147:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp149:
	.cfi_def_cfa_offset 64
.Ltmp150:
	.cfi_offset %rbx, -48
.Ltmp151:
	.cfi_offset %r12, -40
.Ltmp152:
	.cfi_offset %r13, -32
.Ltmp153:
	.cfi_offset %r14, -24
.Ltmp154:
	.cfi_offset %r15, -16
	movq	rtl_dump_file(%rip), %r13
	movq	%rcx, %rax
	movq	%rdx, %r14
	testq	%r13, %r13
	je	.LBB10_9
# BB#1:                                 # %while.cond.preheader
	leaq	40(%r14), %rcx
	.align	16, 0x90
.LBB10_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %r15
	movzbl	48(%r15), %edx
	leaq	8(%r15), %rcx
	testq	$1, %rdx
	jne	.LBB10_2
# BB#3:                                 # %while.end
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movq	predictor_info(%rdi), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	%rdx, %rax
	jne	.LBB10_4
# BB#5:                                 # %while.end
	movabsq	$.L.str5, %r12
	jmp	.LBB10_6
.LBB10_9:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_4:
	movabsq	$.L.str4, %r12
.LBB10_6:                               # %while.end
	andq	%rdx, %rsi
	movq	%rsi, %rdi
	callq	int32_to_float64
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movabsq	$4666723172467343360, %rsi # imm = 0x40C3880000000000
	movq	%rax, %rdi
	callq	float64_div
	movq	%rax, %rbx
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	fprintf
	cmpq	$0, 96(%r14)
	je	.LBB10_8
# BB#7:                                 # %if.then4
	movq	rtl_dump_file(%rip), %rcx
	movabsq	$.L.str6, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	rtl_dump_file(%rip), %rdi
	movq	96(%r14), %rdx
	movabsq	$.L.str7, %rbx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	fprintf
	movq	rtl_dump_file(%rip), %rcx
	movabsq	$.L.str8, %rdi
	movq	$5, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	rtl_dump_file(%rip), %rdi
	movq	56(%r15), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	fprintf
	movq	rtl_dump_file(%rip), %r12
	movq	56(%r15), %rdi
	callq	int64_to_float64
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	96(%r14), %rdi
	movq	%rax, %rbx
	callq	int64_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rdx
	callq	fprintf
.LBB10_8:                               # %if.end18
	movq	rtl_dump_file(%rip), %rsi
	movq	$10, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp155:
	.size	dump_prediction, .Ltmp155-dump_prediction
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"predict.c"
	.size	.L.str, 10

	.type	.L__FUNCTION__.predict_insn,@object # @__FUNCTION__.predict_insn
.L__FUNCTION__.predict_insn:
	.asciz	"predict_insn"
	.size	.L__FUNCTION__.predict_insn, 13

	.type	predictor_info,@object  # @predictor_info
	.section	.rodata,"a",@progbits
	.align	16
predictor_info:
	.quad	.L.str11
	.long	10000                   # 0x2710
	.long	0                       # 0x0
	.quad	.L.str12
	.long	10000                   # 0x2710
	.long	0                       # 0x0
	.quad	.L.str13
	.long	10000                   # 0x2710
	.long	0                       # 0x0
	.quad	.L.str14
	.long	10000                   # 0x2710
	.long	0                       # 0x0
	.quad	.L.str15
	.long	10000                   # 0x2710
	.long	1                       # 0x1
	.quad	.L.str16
	.long	10000                   # 0x2710
	.long	1                       # 0x1
	.quad	.L.str17
	.long	9001                    # 0x2329
	.long	1                       # 0x1
	.quad	.L.str18
	.long	9900                    # 0x26ac
	.long	1                       # 0x1
	.quad	.L.str19
	.long	8900                    # 0x22c4
	.long	1                       # 0x1
	.quad	.L.str20
	.long	9000                    # 0x2328
	.long	1                       # 0x1
	.quad	.L.str21
	.long	9001                    # 0x2329
	.long	0                       # 0x0
	.quad	.L.str22
	.long	9001                    # 0x2329
	.long	0                       # 0x0
	.quad	.L.str23
	.long	6400                    # 0x1900
	.long	0                       # 0x0
	.quad	.L.str24
	.long	8100                    # 0x1fa4
	.long	0                       # 0x0
	.quad	.L.str25
	.long	7900                    # 0x1edc
	.long	0                       # 0x0
	.quad	.L.str26
	.long	7100                    # 0x1bbc
	.long	0                       # 0x0
	.quad	.L.str27
	.long	9000                    # 0x2328
	.long	0                       # 0x0
	.quad	.L.str28
	.long	7000                    # 0x1b58
	.long	0                       # 0x0
	.quad	.L.str29
	.long	5200                    # 0x1450
	.long	0                       # 0x0
	.zero	16
	.size	predictor_info, 320

	.type	.L__FUNCTION__.expected_value_to_br_prob,@object # @__FUNCTION__.expected_value_to_br_prob
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.expected_value_to_br_prob:
	.asciz	"expected_value_to_br_prob"
	.size	.L__FUNCTION__.expected_value_to_br_prob, 26

	.type	.L__FUNCTION__.expensive_function_p,@object # @__FUNCTION__.expensive_function_p
.L__FUNCTION__.expensive_function_p:
	.asciz	"expensive_function_p"
	.size	.L__FUNCTION__.expensive_function_p, 21

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Irreducible region hit, ignoring edge to %i->%i\n"
	.size	.L.str1, 49

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Predictions for insn %i bb %i\n"
	.size	.L.str2, 31

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"  %s heuristics%s: %.1f%%"
	.size	.L.str3, 26

	.type	.L.str4,@object         # @.str4
.L.str4:
	.zero	1
	.size	.L.str4, 1

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" (ignored)"
	.size	.L.str5, 11

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"  exec "
	.size	.L.str6, 8

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%lld"
	.size	.L.str7, 5

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	" hit "
	.size	.L.str8, 6

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	" (%.1f%%)"
	.size	.L.str9, 10

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"combined"
	.size	.L.str11, 9

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"DS theory"
	.size	.L.str12, 10

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"first match"
	.size	.L.str13, 12

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"no prediction"
	.size	.L.str14, 14

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"unconditional jump"
	.size	.L.str15, 19

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"loop iterations"
	.size	.L.str16, 16

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"__builtin_expect"
	.size	.L.str17, 17

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"noreturn call"
	.size	.L.str18, 14

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"loop branch"
	.size	.L.str19, 12

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"loop exit"
	.size	.L.str20, 10

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"loop condition"
	.size	.L.str21, 15

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"loop preconditioning"
	.size	.L.str22, 21

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"loop header"
	.size	.L.str23, 12

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"pointer"
	.size	.L.str24, 8

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"opcode values positive"
	.size	.L.str25, 23

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"opcode values nonequal"
	.size	.L.str26, 23

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"fp_opcode"
	.size	.L.str27, 10

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"call"
	.size	.L.str28, 5

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"error return"
	.size	.L.str29, 13


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
