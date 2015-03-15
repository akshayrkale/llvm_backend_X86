	.file	"ssa.c.bc"
	.text
	.globl	ssa_rename_from_print
	.align	16, 0x90
	.type	ssa_rename_from_print,@function
ssa_rename_from_print:                  # @ssa_rename_from_print
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movabsq	$.Lstr, %rdi
	callq	puts
	movq	ssa_rename_from_ht(%rip), %rdi
	movabsq	$ssa_rename_from_print_1, %rsi
	xorq	%rdx, %rdx
	popq	%rax
	jmp	htab_traverse  # TAILCALL
.Ltmp2:
	.size	ssa_rename_from_print, .Ltmp2-ssa_rename_from_print
	.cfi_endproc

	.align	16, 0x90
	.type	ssa_rename_from_print_1,@function
ssa_rename_from_print_1:                # @ssa_rename_from_print_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movl	(%rax), %esi
	movl	8(%rcx), %edx
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	$1, %rax
	popq	%rdx
	retq
.Ltmp5:
	.size	ssa_rename_from_print_1, .Ltmp5-ssa_rename_from_print_1
	.cfi_endproc

	.globl	remove_phi_alternative
	.align	16, 0x90
	.type	remove_phi_alternative,@function
remove_phi_alternative:                 # @remove_phi_alternative
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 16
.Ltmp9:
	.cfi_offset %rbx, -16
	movq	16(%rdi), %rax
	movq	8(%rax), %r11
	movl	(%r11), %r8d
	leaq	-2(%r8), %r9
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	js	.LBB2_7
# BB#1:                                 # %for.body.lr.ph
	movslq	88(%rsi), %rdi
	movq	$32, %rcx
	movq	%r9, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	%r10, %rsi
	movq	%r9, %rdx
	.align	16, 0x90
.LBB2_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r11,%rsi,8), %rcx
	movq	8(%rcx), %rcx
	cmpq	%rdi, %rcx
	je	.LBB2_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB2_3 Depth=1
	addq	$-2, %rdx
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	$-2, %rsi
	testq	%rbx, %rbx
	jns	.LBB2_3
	jmp	.LBB2_7
.LBB2_4:                                # %if.then
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB2_6
# BB#5:                                 # %if.then12
	movq	8(%r11,%r10,8), %rax
	movq	%rax, 8(%r11,%rsi,8)
	decq	%r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	8(%r11,%r8,8), %rax
	movq	%rax, 16(%r11,%rsi,8)
.LBB2_6:                                # %if.end
	movl	%r9d, (%r11)
	movq	$1, %rax
.LBB2_7:                                # %return
	popq	%rbx
	retq
.Ltmp10:
	.size	remove_phi_alternative, .Ltmp10-remove_phi_alternative
	.cfi_endproc

	.globl	compute_dominance_frontiers
	.align	16, 0x90
	.type	compute_dominance_frontiers,@function
compute_dominance_frontiers:            # @compute_dominance_frontiers
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
	movl	n_basic_blocks(%rip), %eax
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%rax, %rdi
	callq	sbitmap_alloc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	sbitmap_zero
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	callq	compute_dominance_frontiers_1
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp21:
	.size	compute_dominance_frontiers, .Ltmp21-compute_dominance_frontiers
	.cfi_endproc

	.align	16, 0x90
	.type	compute_dominance_frontiers_1,@function
compute_dominance_frontiers_1:          # @compute_dominance_frontiers_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp28:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp29:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp30:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp31:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp33:
	.cfi_def_cfa_offset 80
.Ltmp34:
	.cfi_offset %rbx, -48
.Ltmp35:
	.cfi_offset %r12, -40
.Ltmp36:
	.cfi_offset %r13, -32
.Ltmp37:
	.cfi_offset %r14, -24
.Ltmp38:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdx, %r15
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	movq	%r15, %rax
	andq	%rcx, %rax
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rbx,8), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	$1, %rdx
	movq	%r15, %rcx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%r12,%rax)
	movq	(%rdi,%rbx,8), %rdi
	movq	%rsi, %r14
	callq	sbitmap_zero
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB4_6
# BB#1:
	xorq	%r13, %r13
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%r15, %rsi
	.align	16, 0x90
.LBB4_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r14,%r13,4), %eax
	cmpq	%rsi, %rax
	jne	.LBB4_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$3, %rcx
	movq	%r13, %rax
	shrq	%cl, %rax
	andq	$536870904, %rax        # imm = 0x1FFFFFF8
	movq	16(%r12,%rax), %rax
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB4_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rsi, %rbx
	movq	%r14, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	compute_dominance_frontiers_1
	movq	%rbx, %rsi
	movl	n_basic_blocks(%rip), %ebx
.LBB4_5:                                # %for.inc
                                        #   in Loop: Header=BB4_2 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB4_2
.LBB4_6:                                # %for.end
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	40(%rax), %rax
	testq	%rax, %rax
	je	.LBB4_12
# BB#7:
	movabsq	$entry_exit_blocks+112, %r8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r15, %r9
	movq	16(%rsp), %rsi          # 8-byte Reload
	.align	16, 0x90
.LBB4_8:                                # %for.body17
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	cmpq	%r8, %rcx
	je	.LBB4_11
# BB#9:                                 # %if.end20
                                        #   in Loop: Header=BB4_8 Depth=1
	movslq	88(%rcx), %rcx
	movl	(%r14,%rcx,4), %edi
	cmpq	%r9, %rdi
	je	.LBB4_11
# BB#10:                                # %if.then25
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	%rcx, %rdi
	movabsq	$4294967232, %rdx       # imm = 0xFFFFFFC0
	andq	%rdx, %rdi
	movq	$1, %r10
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx,%rsi,8), %rdx
	shlq	%cl, %r10
	movq	$3, %rcx
	shrq	%cl, %rdi
	orq	%r10, 16(%rdx,%rdi)
.LBB4_11:                               # %for.inc41
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_8
.LBB4_12:                               # %for.cond43.preheader
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_25
# BB#13:
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r15
	.align	16, 0x90
.LBB4_14:                               # %for.body45
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_16 Depth 2
                                        #       Child Loop BB4_18 Depth 3
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movl	(%r14,%rsi,4), %eax
	cmpq	%r15, %rax
	jne	.LBB4_24
# BB#15:                                # %do.body
                                        #   in Loop: Header=BB4_14 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rax,%rcx,8), %r11
	movl	4(%r11), %eax
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB4_24
	.align	16, 0x90
.LBB4_16:                               # %for.body57
                                        #   Parent Loop BB4_14 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_18 Depth 3
	movq	16(%r11,%r12,8), %rdx
	testq	%rdx, %rdx
	je	.LBB4_23
# BB#17:                                # %for.body64.preheader
                                        #   in Loop: Header=BB4_16 Depth=2
	movq	$6, %rcx
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$1, %rdi
	.align	16, 0x90
.LBB4_18:                               # %for.body64
                                        #   Parent Loop BB4_14 Depth=1
                                        #     Parent Loop BB4_16 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdi), %r9
	movq	$1, %r8
	movq	%r9, %rcx
	shlq	%cl, %r8
	testq	%rdx, %r8
	je	.LBB4_22
# BB#19:                                # %if.then69
                                        #   in Loop: Header=BB4_18 Depth=3
	addq	%r10, %r9
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%r14,%rax,4), %eax
	notq	%r8
	andq	%r8, %rdx
	cmpq	%r15, %rax
	je	.LBB4_21
# BB#20:                                # %if.then74
                                        #   in Loop: Header=BB4_18 Depth=3
	movq	%r9, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$1, %rbx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	(%rcx,%rsi,8), %rsi
	movq	%r9, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rbx, 16(%rsi,%rax)
.LBB4_21:                               # %if.end85
                                        #   in Loop: Header=BB4_18 Depth=3
	testq	%rdx, %rdx
	je	.LBB4_23
.LBB4_22:                               # %for.inc90
                                        #   in Loop: Header=BB4_18 Depth=3
	movq	%rdi, %rax
	andq	%r13, %rax
	incq	%rdi
	cmpq	$64, %rax
	jb	.LBB4_18
.LBB4_23:                               # %for.inc94
                                        #   in Loop: Header=BB4_16 Depth=2
	incq	%r12
	movq	%r12, %rax
	andq	%r13, %rax
	movl	4(%r11), %ecx
	cmpq	%rcx, %rax
	jne	.LBB4_16
.LBB4_24:                               # %for.inc98
                                        #   in Loop: Header=BB4_14 Depth=1
	movq	8(%rsp), %rsi           # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	(%rsp), %rdx            # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB4_14
.LBB4_25:                               # %for.end100
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp39:
	.size	compute_dominance_frontiers_1, .Ltmp39-compute_dominance_frontiers_1
	.cfi_endproc

	.globl	convert_to_ssa
	.align	16, 0x90
	.type	convert_to_ssa,@function
convert_to_ssa:                         # @convert_to_ssa
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp43:
	.cfi_def_cfa_offset 16
.Ltmp44:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp45:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$120, %rsp
.Ltmp46:
	.cfi_offset %rbx, -56
.Ltmp47:
	.cfi_offset %r12, -48
.Ltmp48:
	.cfi_offset %r13, -40
.Ltmp49:
	.cfi_offset %r14, -32
.Ltmp50:
	.cfi_offset %r15, -24
	movl	in_ssa_form(%rip), %eax
	testq	%rax, %rax
	jne	.LBB5_60
# BB#1:                                 # %if.end
	callq	get_insns
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	life_analysis
	movslq	n_basic_blocks(%rip), %rdx
	movq	$2, %rcx
	movq	%rsp, %r13
	leaq	15(,%rdx,4), %rax
	shlq	%cl, %rdx
	andq	$-16, %rax
	subq	%rax, %r13
	movq	%r13, %rsp
	movq	$255, %rsi
	movq	%r13, %rdi
	callq	memset
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	calculate_dominance_info
	movq	rtl_dump_file(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB5_6
# BB#2:                                 # %if.then4
	movabsq	$.L.str2, %rdi
	movq	$25, %rsi
	movq	$1, %rdx
	callq	fwrite
	movslq	n_basic_blocks(%rip), %rax
	movq	rtl_dump_file(%rip), %rdi
	testq	%rax, %rax
	jle	.LBB5_5
# BB#3:
	movabsq	$.L.str3, %r14
	.align	16, 0x90
.LBB5_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13,%rbx,4), %ecx
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	incq	%rbx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	rtl_dump_file(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_4
.LBB5_5:                                # %for.end
	callq	fflush
.LBB5_6:                                # %if.end9
	movl	n_basic_blocks(%rip), %edi
	movq	%rdi, %rsi
	callq	sbitmap_vector_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r14
	callq	sbitmap_alloc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	sbitmap_zero
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rcx
	callq	compute_dominance_frontiers_1
	movq	%rbx, %rdi
	callq	free
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_8
# BB#7:                                 # %if.then12
	movl	n_basic_blocks(%rip), %r8d
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r14, %rcx
	callq	dump_sbitmap_vector
	movq	rtl_dump_file(%rip), %rdi
	callq	fflush
.LBB5_8:                                # %if.end14
	movq	%r14, -80(%rbp)         # 8-byte Spill
	callq	max_reg_num
	movl	n_basic_blocks(%rip), %esi
	movq	%rax, %rbx
	movq	%rbx, -128(%rbp)        # 8-byte Spill
	movl	%ebx, ssa_max_reg_num(%rip)
	movq	%rbx, %rdi
	callq	sbitmap_vector_alloc
	movq	%rax, %r15
	movq	%r15, -144(%rbp)        # 8-byte Spill
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_zero
	movslq	n_basic_blocks(%rip), %r14
	movq	%r15, fe_evals(%rip)
	testq	%r14, %r14
	jle	.LBB5_9
# BB#10:
	movq	%rbx, -128(%rbp)        # 8-byte Spill
	movabsq	$find_evaluations_1, %r15
	.align	16, 0x90
.LBB5_12:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_13 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	%r14, %rcx
	leaq	-1(%rcx), %r14
	movl	%r14d, fe_current_bb(%rip)
	movq	24(%rax,%rcx,8), %rbx
	movq	8(%rbx), %r12
	jmp	.LBB5_13
	.align	16, 0x90
.LBB5_16:                               # %if.end14.i
                                        #   in Loop: Header=BB5_13 Depth=2
	addq	$24, %rbx
.LBB5_13:                               # %while.body.i
                                        #   Parent Loop BB5_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB5_15
# BB#14:                                # %if.then.i
                                        #   in Loop: Header=BB5_13 Depth=2
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	note_stores
.LBB5_15:                               # %if.end.i
                                        #   in Loop: Header=BB5_13 Depth=2
	cmpq	%r12, %rbx
	jne	.LBB5_16
# BB#11:                                # %for.cond.loopexit.i
                                        #   in Loop: Header=BB5_12 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_12
# BB#17:                                # %find_evaluations.exit.loopexit
	movq	%r13, -152(%rbp)        # 8-byte Spill
	movl	n_basic_blocks(%rip), %r14d
	movq	-128(%rbp), %rbx        # 8-byte Reload
	jmp	.LBB5_18
.LBB5_9:
	movq	%r13, -152(%rbp)        # 8-byte Spill
.LBB5_18:                               # %find_evaluations.exit
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, -136(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %r14
	movl	n_basic_blocks(%rip), %edi
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	callq	sbitmap_alloc
	movq	%rax, -56(%rbp)         # 8-byte Spill
	movq	$0, -104(%rbp)          # 8-byte Folded Spill
	testq	%r14, %r14
	jle	.LBB5_31
# BB#19:                                # %for.body.lr.ph.i47
	xorq	%rdx, %rdx
	movq	$0, -104(%rbp)          # 8-byte Folded Spill
	movq	-80(%rbp), %r13         # 8-byte Reload
	.align	16, 0x90
.LBB5_20:                               # %for.body.i49
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_21 Depth 2
                                        #       Child Loop BB5_22 Depth 3
                                        #         Child Loop BB5_25 Depth 4
	movq	%rdx, -112(%rbp)        # 8-byte Spill
	movq	-136(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%rdx,8), %rbx
	movq	-144(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%rdx,8), %rsi
	movq	-56(%rbp), %rdi         # 8-byte Reload
	callq	sbitmap_copy
	movq	%rbx, %rdi
	movq	%rbx, %r15
	callq	sbitmap_zero
	.align	16, 0x90
.LBB5_21:                               # %do.body.i
                                        #   Parent Loop BB5_20 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_22 Depth 3
                                        #         Child Loop BB5_25 Depth 4
	movq	-56(%rbp), %rax         # 8-byte Reload
	movl	4(%rax), %eax
	movq	%rax, -96(%rbp)         # 8-byte Spill
	incq	-104(%rbp)              # 8-byte Folded Spill
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, -72(%rbp)         # 8-byte Spill
	je	.LBB5_30
	.align	16, 0x90
.LBB5_22:                               # %for.body6.i
                                        #   Parent Loop BB5_20 Depth=1
                                        #     Parent Loop BB5_21 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB5_25 Depth 4
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	16(%rax,%rdx,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB5_24
# BB#23:                                #   in Loop: Header=BB5_22 Depth=3
	movq	%rdx, -88(%rbp)         # 8-byte Spill
	jmp	.LBB5_28
	.align	16, 0x90
.LBB5_24:                               # %for.body12.preheader.i
                                        #   in Loop: Header=BB5_22 Depth=3
	movq	$6, %rcx
	movq	%rdx, %rax
	movq	%rdx, -88(%rbp)         # 8-byte Spill
	shlq	%cl, %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movq	$1, %r14
	.align	16, 0x90
.LBB5_25:                               # %for.body12.i
                                        #   Parent Loop BB5_20 Depth=1
                                        #     Parent Loop BB5_21 Depth=2
                                        #       Parent Loop BB5_22 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-1(%r14), %rbx
	movq	$1, %r12
	movq	%rbx, %rcx
	shlq	%cl, %r12
	testq	%r12, %rsi
	je	.LBB5_27
# BB#26:                                # %if.then14.i
                                        #   in Loop: Header=BB5_25 Depth=4
	movq	-64(%rbp), %rax         # 8-byte Reload
	addq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	-56(%rbp), %rdi         # 8-byte Reload
	andq	%rdx, 16(%rdi,%rax)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%r13,%rbx,8), %rdx
	notq	%r12
	movq	$1, -72(%rbp)           # 8-byte Folded Spill
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	%rdi, %rsi
	movq	%r15, %rcx
	callq	sbitmap_union_of_diff
	movq	(%r13,%rbx,8), %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	sbitmap_a_or_b
	movq	-48(%rbp), %rsi         # 8-byte Reload
	andq	%r12, %rsi
	je	.LBB5_28
.LBB5_27:                               # %for.inc.i
                                        #   in Loop: Header=BB5_25 Depth=4
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB5_25
.LBB5_28:                               # %for.inc34.i
                                        #   in Loop: Header=BB5_22 Depth=3
	movq	-88(%rbp), %rdx         # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	-96(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB5_22
# BB#29:                                # %do.cond.i
                                        #   in Loop: Header=BB5_21 Depth=2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	-72(%rbp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB5_21
.LBB5_30:                               # %for.inc38.i
                                        #   in Loop: Header=BB5_20 Depth=1
	movq	-112(%rbp), %rdx        # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	andq	%rsi, %rax
	movq	-128(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_20
.LBB5_31:                               # %for.end40.i
	movq	-56(%rbp), %rdi         # 8-byte Reload
	callq	free
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_34
# BB#32:                                # %compute_iterated_dominance_frontiers.exit
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	-104(%rbp), %rdx        # 8-byte Reload
	movq	%rbx, %rcx
	callq	fprintf
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB5_34
# BB#33:                                # %if.then19
	movabsq	$.L.str6, %rsi
	movabsq	$.L.str7, %rdx
	movq	-136(%rbp), %rcx        # 8-byte Reload
	movq	%rbx, %r8
	callq	dump_sbitmap_vector
	movq	rtl_dump_file(%rip), %rdi
	callq	fflush
.LBB5_34:                               # %if.end21
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_59
# BB#35:
	xorq	%rdx, %rdx
	movabsq	$0, %r13
	movabsq	$1, %r15
	.align	16, 0x90
.LBB5_36:                               # %for.body.i52
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_39 Depth 2
                                        #       Child Loop BB5_41 Depth 3
                                        #         Child Loop BB5_44 Depth 4
                                        #         Child Loop BB5_50 Depth 4
	movq	%rdx, -56(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB5_38
# BB#37:                                # %for.body.i52
                                        #   in Loop: Header=BB5_36 Depth=1
	movq	%rdx, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	cmpq	$16, %rax
	jne	.LBB5_58
.LBB5_38:                               # %do.body.i55
                                        #   in Loop: Header=BB5_36 Depth=1
	movq	-136(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%rdx,8), %rax
	movq	%rax, -112(%rbp)        # 8-byte Spill
	movl	4(%rax), %eax
	movq	%rax, -120(%rbp)        # 8-byte Spill
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB5_58
	.align	16, 0x90
.LBB5_39:                               # %for.body9.i
                                        #   Parent Loop BB5_36 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_41 Depth 3
                                        #         Child Loop BB5_44 Depth 4
                                        #         Child Loop BB5_50 Depth 4
	movq	%rsi, -88(%rbp)         # 8-byte Spill
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	16(%rax,%rsi,8), %rsi
	testq	%rsi, %rsi
	je	.LBB5_57
# BB#40:                                # %for.body16.preheader.i
                                        #   in Loop: Header=BB5_39 Depth=2
	movq	$6, %rcx
	movq	-88(%rbp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB5_41:                               # %for.body16.i
                                        #   Parent Loop BB5_36 Depth=1
                                        #     Parent Loop BB5_39 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB5_44 Depth 4
                                        #         Child Loop BB5_50 Depth 4
	movq	$1, %r12
	movq	%r14, %rcx
	shlq	%cl, %r12
	testq	%rsi, %r12
	je	.LBB5_56
# BB#42:                                # %if.then18.i
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	-64(%rbp), %rax         # 8-byte Reload
	leaq	(%r14,%rax), %rbx
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	32(%rax,%rbx,8), %rax
	movq	64(%rax), %rdi
	movq	%rdx, %rsi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$entry_exit_blocks, %rdx
	movq	%r13, %rsi
	movq	%r15, %r8
	je	.LBB5_55
# BB#43:                                # %if.then22.i
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rax
	movq	%rax, -72(%rbp)         # 8-byte Spill
	movq	32(%rax), %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	je	.LBB5_55
	.align	16, 0x90
.LBB5_44:                               # %for.body.i.i
                                        #   Parent Loop BB5_36 Depth=1
                                        #     Parent Loop BB5_39 Depth=2
                                        #       Parent Loop BB5_41 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	16(%rax), %rcx
	movq	(%rax), %rax
	cmpq	%rdx, %rcx
	movq	%rsi, %rcx
	je	.LBB5_46
# BB#45:                                # %for.body.i.i
                                        #   in Loop: Header=BB5_44 Depth=4
	movq	%r8, %rcx
.LBB5_46:                               # %for.body.i.i
                                        #   in Loop: Header=BB5_44 Depth=4
	addq	%rcx, %rdi
	testq	%rax, %rax
	jne	.LBB5_44
# BB#47:                                # %for.end.i.i
                                        #   in Loop: Header=BB5_41 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB5_55
# BB#48:                                # %if.end4.i.i
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	-56(%rbp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rax
	movq	%rax, -104(%rbp)        # 8-byte Spill
	addq	%rdi, %rdi
	movq	%rdx, %rbx
	callq	rtvec_alloc
	movq	%rbx, %rcx
	movq	-72(%rbp), %rdx         # 8-byte Reload
	movq	32(%rdx), %rbx
	movq	%rax, -96(%rbp)         # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB5_53
# BB#49:                                # %for.body10.i.i.preheader
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	-96(%rbp), %rax         # 8-byte Reload
	leaq	16(%rax), %r15
	.align	16, 0x90
.LBB5_50:                               # %for.body10.i.i
                                        #   Parent Loop BB5_36 Depth=1
                                        #     Parent Loop BB5_39 Depth=2
                                        #       Parent Loop BB5_41 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	16(%rbx), %rax
	cmpq	%rcx, %rax
	je	.LBB5_52
# BB#51:                                # %if.then13.i.i
                                        #   in Loop: Header=BB5_50 Depth=4
	movq	global_rtl(%rip), %rax
	movq	%rax, -8(%r15)
	movq	16(%rbx), %rax
	movslq	88(%rax), %rsi
	xorq	%rdi, %rdi
	movq	%rcx, %r13
	callq	gen_rtx_CONST_INT
	movq	%r13, %rcx
	movq	%rax, (%r15)
.LBB5_52:                               # %for.inc23.i.i
                                        #   in Loop: Header=BB5_50 Depth=4
	movq	(%rbx), %rbx
	addq	$16, %r15
	testq	%rbx, %rbx
	jne	.LBB5_50
.LBB5_53:                               # %for.end26.i.i
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	$152, %rdi
	xorq	%rsi, %rsi
	movq	-96(%rbp), %rdx         # 8-byte Reload
	callq	gen_rtx_fmt_E
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	-104(%rbp), %rdx        # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, -96(%rbp)         # 8-byte Spill
	movq	-72(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	first_insn_after_basic_block_note
	movq	16(%rax), %r15
	movq	8(%rbx), %r13
	movq	-96(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%rax, %rbx
	callq	emit_insn_before
	cmpq	%r13, %r15
	movabsq	$0, %rsi
	movabsq	$1, %r8
	jne	.LBB5_55
# BB#54:                                # %if.then35.i.i
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	16(%rbx), %rax
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx)
	.align	16, 0x90
.LBB5_55:                               # %if.end.i67
                                        #   in Loop: Header=BB5_41 Depth=3
	movq	%r8, %r15
	movq	%rsi, %r13
	notq	%r12
	movq	-48(%rbp), %rsi         # 8-byte Reload
	andq	%r12, %rsi
	movq	-56(%rbp), %rdx         # 8-byte Reload
	je	.LBB5_57
.LBB5_56:                               # %for.inc.i70
                                        #   in Loop: Header=BB5_41 Depth=3
	incq	%r14
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB5_41
.LBB5_57:                               # %for.inc28.i
                                        #   in Loop: Header=BB5_39 Depth=2
	movq	-88(%rbp), %rsi         # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	-120(%rbp), %rcx        # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB5_39
.LBB5_58:                               # %for.inc32.i
                                        #   in Loop: Header=BB5_36 Depth=1
	incq	%rdx
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	andq	%rsi, %rax
	movq	-128(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_36
.LBB5_59:                               # %insert_phi_nodes.exit
	movq	%rbx, %rdi
	movq	-152(%rbp), %rsi        # 8-byte Reload
	callq	rename_registers
	movq	-80(%rbp), %rdi         # 8-byte Reload
	callq	free
	movq	-144(%rbp), %rdi        # 8-byte Reload
	callq	free
	movq	-136(%rbp), %rdi        # 8-byte Reload
	callq	free
	movq	$1, %rax
	movl	%eax, in_ssa_form(%rip)
	callq	get_insns
	movq	%rax, %rbx
	callq	max_reg_num
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	reg_scan
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB5_60:                               # %if.then
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.convert_to_ssa, %rcx
	movq	$1145, %rsi             # imm = 0x479
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp51:
	.size	convert_to_ssa, .Ltmp51-convert_to_ssa
	.cfi_endproc

	.align	16, 0x90
	.type	rename_registers,@function
rename_registers:                       # @rename_registers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp55:
	.cfi_def_cfa_offset 16
.Ltmp56:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp57:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
.Ltmp58:
	.cfi_offset %rbx, -32
.Ltmp59:
	.cfi_offset %r14, -24
	movq	%rsi, %r14
	movq	%rdi, %rbx
	leaq	(%rbx,%rbx,2), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$.L.str9, %rdx
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, ssa_definition(%rip)
	movabsq	$ssa_rename_from_hash_function, %rsi
	movabsq	$ssa_rename_from_equal, %rdx
	movabsq	$ssa_rename_from_delete, %rcx
	movq	$64, %rdi
	callq	htab_create
	movq	%rax, ssa_rename_from_ht(%rip)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	leaq	15(,%rbx,8), %rax
	shlq	%cl, %rbx
	andq	$-16, %rax
	movq	%rsp, %rdi
	subq	%rax, %rdi
	movq	%rdi, %rsp
	movq	%rdi, ssa_rename_to_pseudo(%rip)
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
	movabsq	$ssa_rename_to_hard, %rdi
	xorq	%rsi, %rsi
	movq	$25016, %rdx            # imm = 0x61B8
	callq	memset
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	rename_block
	movq	$0, ssa_rename_to_pseudo(%rip)
	leaq	-16(%rbp), %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Ltmp60:
	.size	rename_registers, .Ltmp60-rename_registers
	.cfi_endproc

	.globl	convert_from_ssa
	.align	16, 0x90
	.type	convert_from_ssa,@function
convert_from_ssa:                       # @convert_from_ssa
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
	subq	$12656, %rsp            # imm = 0x3170
.Ltmp72:
	.cfi_def_cfa_offset 12704
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
	callq	get_insns
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	life_analysis
	movq	ssa_definition(%rip), %rax
	movq	(%rax), %rdi
	callq	partition_new
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	jle	.LBB7_5
# BB#1:                                 # %for.body.lr.ph.i
	movabsq	$-8589934592, %r12      # imm = 0xFFFFFFFE00000000
	.align	16, 0x90
.LBB7_2:                                # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_3 Depth 2
                                        #       Child Loop BB7_26 Depth 3
                                        #         Child Loop BB7_30 Depth 4
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdx,8), %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	callq	first_insn_after_basic_block_note
	jmp	.LBB7_3
	.align	16, 0x90
.LBB7_58:                               # %for.inc95.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	next_nonnote_insn
.LBB7_3:                                # %for.body.i
                                        #   Parent Loop BB7_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_26 Depth 3
                                        #         Child Loop BB7_30 Depth 4
	testq	%rax, %rax
	je	.LBB7_4
# BB#16:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB7_4
# BB#17:                                # %land.lhs.true2.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movq	32(%rax), %r14
	movzbl	(%r14), %ecx
	cmpq	$47, %rcx
	jne	.LBB7_4
# BB#18:                                # %land.rhs.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movq	16(%r14), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$152, %rcx
	jne	.LBB7_4
# BB#19:                                # %for.body.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB7_22
# BB#20:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movl	8(%rax), %r15d
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %r15
	ja	.LBB7_24
# BB#21:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movq	%r15, %rcx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rdx, %rcx
	cmpq	$16, %rcx
	jne	.LBB7_22
.LBB7_24:                               # %if.end.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx), %r13
	testq	%r13, %r13
	je	.LBB7_58
# BB#25:                                # %for.body43.lr.ph.i.i
                                        #   in Loop: Header=BB7_3 Depth=2
	leaq	(%rax,%rax,2), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	leaq	8(%rcx,%rax,8), %r8
	movq	%r8, 32(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB7_26:                               # %for.body43.i.i
                                        #   Parent Loop BB7_2 Depth=1
                                        #     Parent Loop BB7_3 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_30 Depth 4
	movzbl	48(%r13), %eax
	testq	$2, %rax
	je	.LBB7_57
# BB#27:                                # %land.lhs.true45.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	16(%r13), %rcx
	movq	40(%rcx), %rax
	cmpq	$0, 8(%rax)
	je	.LBB7_57
# BB#28:                                # %land.lhs.true47.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	24(%r13), %rax
	movq	32(%rax), %rax
	cmpq	$0, (%rax)
	je	.LBB7_57
# BB#29:                                # %if.then50.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	movslq	(%rax), %rdx
	movslq	88(%rcx), %rsi
	movq	$32, %rcx
	leaq	-2(%rdx), %rbx
	shlq	%cl, %rdx
	.align	16, 0x90
.LBB7_30:                               # %for.cond.i.i.i
                                        #   Parent Loop BB7_2 Depth=1
                                        #     Parent Loop BB7_3 Depth=2
                                        #       Parent Loop BB7_26 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	js	.LBB7_57
# BB#31:                                # %for.body.i.i.i
                                        #   in Loop: Header=BB7_30 Depth=4
	movq	16(%rax,%rbx,8), %rcx
	movq	8(%rcx), %rcx
	addq	$-2, %rbx
	addq	%r12, %rdx
	cmpq	%rsi, %rcx
	jne	.LBB7_30
# BB#32:                                # %if.end55.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB7_35
# BB#33:                                # %lor.lhs.false59.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movl	8(%rax), %ebx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rbx
	ja	.LBB7_36
# BB#34:                                # %lor.lhs.false59.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	%rbx, %rcx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rdx, %rcx
	cmpq	$16, %rcx
	jne	.LBB7_35
.LBB7_36:                               # %if.end75.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movl	(%r8), %ecx
	leaq	(%rax,%rax,2), %rax
	movq	64(%rsp), %rdx          # 8-byte Reload
	movl	8(%rdx,%rax,8), %eax
	cmpq	%rax, %rcx
	je	.LBB7_57
# BB#37:                                # %if.then86.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	ssa_rename_from_ht(%rip), %rdi
	movl	%r15d, 96(%rsp)
	movq	$0, 104(%rsp)
	leaq	96(%rsp), %rsi
	movq	%r15, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB7_40
# BB#38:                                # %ssa_rename_from_lookup.exit.i.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	8(%rax), %rax
	testq	%rax, %rax
	movq	48(%rsp), %r15          # 8-byte Reload
	je	.LBB7_40
# BB#39:                                # %cond.true.i.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movl	8(%rax), %r15d
.LBB7_40:                               # %original_register.exit.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	ssa_rename_from_ht(%rip), %rdi
	movl	%ebx, 112(%rsp)
	movq	$0, 120(%rsp)
	leaq	112(%rsp), %rsi
	movq	%rbx, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rbx, %rdx
	je	.LBB7_43
# BB#41:                                # %ssa_rename_from_lookup.exit.i31.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	8(%rax), %rax
	testq	%rax, %rax
	movq	%rdx, %rsi
	je	.LBB7_43
# BB#42:                                # %cond.true.i34.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movl	8(%rax), %esi
.LBB7_43:                               # %original_register.exit36.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	movabsq	$0, %r8
	movq	%r8, %r10
	movabsq	$1, %r9
	jle	.LBB7_45
# BB#44:                                # %original_register.exit36.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	%r9, %r10
.LBB7_45:                               # %original_register.exit36.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	%rsi, %rdi
	andq	%rcx, %rdi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$52, %rbx
	movq	%r8, %rcx
	jle	.LBB7_47
# BB#46:                                # %original_register.exit36.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	%r9, %rcx
.LBB7_47:                               # %original_register.exit36.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	cmpq	%rdi, %rax
	je	.LBB7_49
# BB#48:                                # %original_register.exit36.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	orq	%rcx, %r10
	je	.LBB7_55
.LBB7_49:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$53, %rsi
	movq	%r8, %rax
	jge	.LBB7_51
# BB#50:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	%r9, %rax
.LBB7_51:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$52, %r15
	movq	%r8, %rcx
	jle	.LBB7_53
# BB#52:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	%r9, %rcx
.LBB7_53:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	testq	%rax, %rcx
	jne	.LBB7_55
# BB#54:                                # %if.end.i.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	orq	%rax, %rcx
	xorq	$1, %rcx
	jne	.LBB7_55
# BB#56:                                # %if.end90.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	partition_union
	movq	32(%rsp), %r8           # 8-byte Reload
	.align	16, 0x90
.LBB7_57:                               # %for.inc.i.i
                                        #   in Loop: Header=BB7_26 Depth=3
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB7_26
	jmp	.LBB7_58
	.align	16, 0x90
.LBB7_4:                                # %for.cond.backedge.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rsp), %rdx          # 8-byte Reload
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_2
.LBB7_5:                                # %for.end.i
	leaq	72(%rsp), %r15
	movq	%r15, %rdi
	callq	bitmap_initialize
	movabsq	$4294967294, %r12       # imm = 0xFFFFFFFE
	movabsq	$4294967197, %r13       # imm = 0xFFFFFF9D
	.align	16, 0x90
.LBB7_6:                                # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_7 Depth 2
                                        #     Child Loop BB7_65 Depth 2
                                        #       Child Loop BB7_66 Depth 3
	movq	%r15, %rdi
	callq	bitmap_clear
	movq	ssa_definition(%rip), %rax
	xorq	%rcx, %rcx
	cmpq	$0, (%rax)
	movabsq	$0, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB7_62
	.align	16, 0x90
.LBB7_7:                                # %for.body.i40.i
                                        #   Parent Loop BB7_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdx
	andq	%rsi, %rdx
	cmpq	$52, %rdx
	ja	.LBB7_9
# BB#8:                                 # %for.body.i40.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	%rbx, %rdx
	andq	%r12, %rdx
	cmpq	$16, %rdx
	jne	.LBB7_61
.LBB7_9:                                # %if.then.i44.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	32(%rax,%rcx,8), %rax
	testq	%rax, %rax
	je	.LBB7_61
# BB#10:                                # %lor.lhs.false11.i.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB7_12
# BB#11:                                # %land.lhs.true.i48.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movl	40(%rax), %ecx
	cmpq	%r13, %rcx
	je	.LBB7_61
.LBB7_12:                               # %if.end.i49.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB7_61
# BB#13:                                # %if.end26.i.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	16(%rax), %rax
	movq	%rax, 128(%rsp)
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	jne	.LBB7_14
# BB#59:                                # %if.then41.i.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	%r15, %rdi
	movq	%rsi, %r14
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	leaq	128(%rsp), %rdi
	movabsq	$mark_reg_in_phi, %rsi
	movq	%r15, %rdx
	callq	for_each_rtx
	jmp	.LBB7_60
.LBB7_14:                               # %if.end26.i.i
                                        #   in Loop: Header=BB7_7 Depth=2
	cmpq	$61, %rax
	jne	.LBB7_61
# BB#15:                                # %if.then34.i.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	%r15, %rdi
	movq	%rsi, %r14
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movq	128(%rsp), %rax
	movl	8(%rax), %esi
	movq	%r15, %rdi
	callq	bitmap_set_bit
.LBB7_60:                               # %for.inc.i53.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	%r14, %rsi
	.align	16, 0x90
.LBB7_61:                               # %for.inc.i53.i
                                        #   in Loop: Header=BB7_7 Depth=2
	movq	ssa_definition(%rip), %rax
	movq	(%rax), %rdx
	incq	%rbx
	movq	%rbx, %rcx
	andq	%rsi, %rcx
	cmpq	%rdx, %rcx
	jb	.LBB7_7
.LBB7_62:                               # %mark_phi_and_copy_regs.exit.i
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	%r15, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	conflict_graph_compute
	movslq	n_basic_blocks(%rip), %r15
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	jle	.LBB7_63
# BB#64:                                # %for.body9.lr.ph.i
                                        #   in Loop: Header=BB7_6 Depth=1
	xorq	%r12, %r12
	.align	16, 0x90
.LBB7_65:                               # %for.body9.i
                                        #   Parent Loop BB7_6 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_66 Depth 3
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r15,8), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	(%rax), %r13
	movq	8(%rax), %rbx
	cmpq	%rbx, %r13
	movabsq	$0, %r14
	je	.LBB7_73
	.align	16, 0x90
.LBB7_66:                               # %for.body.i57.i
                                        #   Parent Loop BB7_6 Depth=1
                                        #     Parent Loop BB7_65 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r13), %eax
	cmpq	$32, %rax
	jne	.LBB7_72
# BB#67:                                # %if.end.i60.i
                                        #   in Loop: Header=BB7_66 Depth=3
	movq	32(%r13), %rdx
	movzbl	(%rdx), %eax
	cmpq	$47, %rax
	jne	.LBB7_72
# BB#68:                                # %if.end7.i.i
                                        #   in Loop: Header=BB7_66 Depth=3
	movq	16(%rdx), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB7_72
# BB#69:                                # %lor.lhs.false.i61.i
                                        #   in Loop: Header=BB7_66 Depth=3
	movq	8(%rdx), %rdx
	movl	(%rdx), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB7_72
# BB#70:                                # %if.end21.i.i
                                        #   in Loop: Header=BB7_66 Depth=3
	xorq	%rcx, %rsi
	testq	$16711680, %rsi         # imm = 0xFF0000
	jne	.LBB7_72
# BB#71:                                # %if.end29.i.i
                                        #   in Loop: Header=BB7_66 Depth=3
	movl	8(%rdx), %ecx
	movl	8(%rax), %edx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	coalesce_if_unconflicting
	addq	%rax, %r14
	.align	16, 0x90
.LBB7_72:                               # %for.inc.i67.i
                                        #   in Loop: Header=BB7_66 Depth=3
	movq	24(%r13), %r13
	cmpq	%rbx, %r13
	jne	.LBB7_66
.LBB7_73:                               # %coalesce_regs_in_copies.exit.i
                                        #   in Loop: Header=BB7_65 Depth=2
	decq	%r15
	addq	%r12, %r14
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 128(%rsp)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 136(%rsp)
	xorq	%rax, %rax
	movl	%eax, 144(%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movabsq	$coalesce_reg_in_phi, %rsi
	leaq	128(%rsp), %rdx
	callq	for_each_successor_phi
	movl	144(%rsp), %r12d
	addq	%r14, %r12
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_65
# BB#74:                                # %for.end14.i
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	conflict_graph_delete
	testq	%r12, %r12
	leaq	72(%rsp), %r15
	movabsq	$4294967294, %r12       # imm = 0xFFFFFFFE
	movabsq	$4294967197, %r13       # imm = 0xFFFFFF9D
	jg	.LBB7_6
	jmp	.LBB7_75
.LBB7_63:                               # %for.end14.thread.i
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	conflict_graph_delete
.LBB7_75:                               # %compute_coalesced_reg_partition.exit
	leaq	72(%rsp), %rdi
	callq	bitmap_clear
	callq	max_reg_num
	movq	%rax, %rdi
	callq	sbitmap_alloc
	movq	%rax, %r14
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	sbitmap_zero
	movq	ssa_rename_from_ht(%rip), %rdi
	movq	%r14, 72(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 80(%rsp)
	movabsq	$record_canonical_element_1, %rsi
	leaq	72(%rsp), %rdx
	callq	htab_traverse
	leaq	128(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$12508, %rdx            # imm = 0x30DC
	xorq	%rbx, %rbx
	callq	memset
	movl	4(%r14), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB7_86
	.align	16, 0x90
.LBB7_76:                               # %for.body12.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_78 Depth 2
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	16(%rax,%rbx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB7_85
# BB#77:                                # %for.body18.preheader.i
                                        #   in Loop: Header=BB7_76 Depth=1
	movq	$6, %rcx
	movq	56(%rsp), %r14          # 8-byte Reload
	shlq	%cl, %r14
	movq	$1, %r15
	.align	16, 0x90
.LBB7_78:                               # %for.body18.i
                                        #   Parent Loop BB7_76 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r15), %rcx
	movq	$1, %r12
	shlq	%cl, %r12
	testq	%rbx, %r12
	je	.LBB7_84
# BB#79:                                # %if.then20.i
                                        #   in Loop: Header=BB7_78 Depth=2
	movq	%rdx, %r13
	movq	ssa_rename_from_ht(%rip), %rdi
	addq	%r14, %rcx
	movl	%ecx, 12640(%rsp)
	movq	$0, 12648(%rsp)
	leaq	12640(%rsp), %rsi
	movq	%rcx, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB7_83
# BB#80:                                # %ssa_rename_from_lookup.exit.i
                                        #   in Loop: Header=BB7_78 Depth=2
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB7_83
# BB#81:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB7_78 Depth=2
	movl	8(%rax), %ecx
	cmpq	$52, %rcx
	ja	.LBB7_83
# BB#82:                                # %land.lhs.true26.i
                                        #   in Loop: Header=BB7_78 Depth=2
	movzbl	2(%rax), %eax
	imulq	$236, %rcx, %rcx
	leaq	128(%rsp,%rcx), %rcx
	movl	(%rcx,%rax,4), %eax
	testq	%rax, %rax
	jne	.LBB7_119
	.align	16, 0x90
.LBB7_83:                               # %if.end.i
                                        #   in Loop: Header=BB7_78 Depth=2
	notq	%r12
	andq	%r12, %rbx
	movq	%r13, %rdx
	je	.LBB7_85
.LBB7_84:                               # %for.inc40.i
                                        #   in Loop: Header=BB7_78 Depth=2
	movq	%r15, %rax
	andq	%rdx, %rax
	incq	%r15
	cmpq	$64, %rax
	jb	.LBB7_78
.LBB7_85:                               # %for.inc44.i
                                        #   in Loop: Header=BB7_76 Depth=1
	movq	56(%rsp), %rbx          # 8-byte Reload
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jb	.LBB7_76
.LBB7_86:                               # %if.end6
	movq	%rdx, %r12
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	free
	movslq	n_basic_blocks(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB7_87
	.align	16, 0x90
.LBB7_92:                               # %for.body.i120
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_93 Depth 2
                                        #       Child Loop BB7_97 Depth 3
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdx,8), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	decq	%rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB7_93:                               # %do.body.i123
                                        #   Parent Loop BB7_92 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_97 Depth 3
	movq	(%rax), %r14
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_98
# BB#94:                                # %if.then.i
                                        #   in Loop: Header=BB7_93 Depth=2
	leaq	32(%r14), %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movabsq	$rename_equivalent_regs_in_insn, %r15
	movq	%r15, %rsi
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	callq	for_each_rtx
	leaq	56(%r14), %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
	movq	32(%r14), %r13
	movzbl	(%r13), %eax
	cmpq	$24, %rax
	jne	.LBB7_98
# BB#95:                                # %if.then18.i
                                        #   in Loop: Header=BB7_93 Depth=2
	movq	8(%r13), %rax
	movslq	(%rax), %r15
	cmpq	$1, %r15
	jle	.LBB7_120
# BB#96:                                # %if.end.i126
                                        #   in Loop: Header=BB7_93 Depth=2
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rax
	testq	%rdx, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	$1, %rbx
	jle	.LBB7_98
	.align	16, 0x90
.LBB7_97:                               # %for.body39.i
                                        #   Parent Loop BB7_92 Depth=1
                                        #     Parent Loop BB7_93 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r13), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	%rbx, %rax
	andq	%r12, %rax
	movq	%r15, %rcx
	andq	%r12, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB7_97
	.align	16, 0x90
.LBB7_98:                               # %if.end48.i
                                        #   in Loop: Header=BB7_93 Depth=2
	movq	56(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	leaq	24(%r14), %rax
	jne	.LBB7_93
# BB#91:                                # %for.cond.loopexit.i
                                        #   in Loop: Header=BB7_92 Depth=1
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_92
# BB#99:                                # %rename_equivalent_regs.exit
	movslq	n_basic_blocks(%rip), %r14
	testq	%r14, %r14
	jle	.LBB7_87
# BB#100:
	movabsq	$entry_exit_blocks, %r15
	.align	16, 0x90
.LBB7_102:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_106 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%r14,8), %rax
	movq	32(%rax), %rbx
	jmp	.LBB7_106
	.align	16, 0x90
.LBB7_105:                              # %for.inc
                                        #   in Loop: Header=BB7_106 Depth=2
	movq	(%rbx), %rbx
.LBB7_106:                              # %for.inc
                                        #   Parent Loop BB7_102 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB7_101
# BB#103:                               # %for.body10
                                        #   in Loop: Header=BB7_106 Depth=2
	movq	16(%rbx), %rax
	cmpq	%r15, %rax
	je	.LBB7_105
# BB#104:                               # %if.then12
                                        #   in Loop: Header=BB7_106 Depth=2
	movq	%rbx, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	eliminate_phi
	jmp	.LBB7_105
	.align	16, 0x90
.LBB7_101:                              # %for.cond.loopexit
                                        #   in Loop: Header=BB7_102 Depth=1
	decq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_102
.LBB7_87:                               # %for.end14
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	partition_delete
	movslq	n_basic_blocks(%rip), %r14
	testq	%r14, %r14
	jle	.LBB7_88
	.align	16, 0x90
.LBB7_108:                              # %for.body18
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_109 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	%r14, %rbx
	movq	24(%rax,%rbx,8), %rax
	movq	(%rax), %rax
	leaq	-1(%rbx), %r14
	jmp	.LBB7_109
	.align	16, 0x90
.LBB7_118:                              # %if.else74
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	24(%rax), %rax
.LBB7_109:                              # %while.body
                                        #   Parent Loop BB7_108 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB7_116
# BB#110:                               # %land.lhs.true
                                        #   in Loop: Header=BB7_109 Depth=2
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$32, %rdx
	jne	.LBB7_116
# BB#111:                               # %land.lhs.true25
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	32(%rax), %rdx
	movzbl	(%rdx), %esi
	cmpq	$47, %rsi
	jne	.LBB7_116
# BB#112:                               # %land.lhs.true30
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	16(%rdx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$152, %rdx
	jne	.LBB7_116
# BB#113:                               # %if.then40
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	basic_block_info(%rip), %rcx
	movq	24(%rcx,%rbx,8), %rcx
	movq	8(%rcx), %rdx
	cmpq	%rdx, %rax
	jne	.LBB7_115
# BB#114:                               # %if.then46
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	16(%rax), %rdx
	movq	%rdx, 8(%rcx)
.LBB7_115:                              # %if.end55
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	%rax, %rdi
	callq	delete_insn
	jmp	.LBB7_109
	.align	16, 0x90
.LBB7_116:                              # %if.else57
                                        #   in Loop: Header=BB7_109 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	je	.LBB7_107
# BB#117:                               # %if.else65
                                        #   in Loop: Header=BB7_109 Depth=2
	movq	basic_block_info(%rip), %rcx
	movq	24(%rcx,%rbx,8), %rcx
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB7_118
.LBB7_107:                              # %for.cond15.loopexit
                                        #   in Loop: Header=BB7_108 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_108
.LBB7_88:                               # %for.end81
	callq	commit_edge_insertions
	xorq	%rax, %rax
	movl	%eax, in_ssa_form(%rip)
	xorq	%rdi, %rdi
	movq	$1, %rsi
	callq	count_or_remove_death_notes
	movq	ssa_definition(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB7_90
# BB#89:                                # %if.then84
	callq	free
	movq	$0, ssa_definition(%rip)
.LBB7_90:                               # %do.end
	movq	ssa_rename_from_ht(%rip), %rdi
	callq	htab_delete
	addq	$12656, %rsp            # imm = 0x3170
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_120:                              # %if.then26.i
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.rename_equivalent_regs, %rdx
	movq	$2125, %rsi             # imm = 0x84D
	callq	fancy_abort
.LBB7_22:                               # %if.then.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.make_regs_equivalent_over_bad_edges, %rcx
	movq	$1496, %rsi             # imm = 0x5D8
	jmp	.LBB7_23
.LBB7_35:                               # %if.then74.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.make_regs_equivalent_over_bad_edges, %rcx
	movq	$1514, %rsi             # imm = 0x5EA
	jmp	.LBB7_23
.LBB7_55:                               # %if.then89.i.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.make_regs_equivalent_over_bad_edges, %rcx
	movq	$1526, %rsi             # imm = 0x5F6
	jmp	.LBB7_23
.LBB7_119:                              # %if.then5
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.convert_from_ssa, %rcx
	movq	$2165, %rsi             # imm = 0x875
.LBB7_23:                               # %if.then.i.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp78:
	.size	convert_from_ssa, .Ltmp78-convert_from_ssa
	.cfi_endproc

	.align	16, 0x90
	.type	eliminate_phi,@function
eliminate_phi:                          # @eliminate_phi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp82:
	.cfi_def_cfa_offset 16
.Ltmp83:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp84:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$104, %rsp
.Ltmp85:
	.cfi_offset %rbx, -56
.Ltmp86:
	.cfi_offset %r12, -48
.Ltmp87:
	.cfi_offset %r13, -40
.Ltmp88:
	.cfi_offset %r14, -32
.Ltmp89:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movq	24(%r14), %rdi
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	callq	first_insn_after_basic_block_note
	testq	%rax, %rax
	je	.LBB8_43
# BB#1:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB8_2:                                # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB8_6
# BB#3:                                 # %land.lhs.true1
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	32(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$47, %rdx
	jne	.LBB8_6
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$152, %rcx
	jne	.LBB8_6
# BB#5:                                 # %while.body
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%rax, %rdi
	callq	next_nonnote_insn
	addq	$2, %rbx
	testq	%rax, %rax
	jne	.LBB8_2
.LBB8_6:                                # %while.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB8_43
# BB#7:                                 # %if.end
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,8), %rax
	andq	$-16, %rax
	movq	%rsp, %rcx
	subq	%rax, %rcx
	movq	%rcx, -56(%rbp)         # 8-byte Spill
	movq	%rcx, %rsp
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r12
	movq	%r12, -88(%rbp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_alloc
	movq	%rax, %r13
	movq	%r13, -112(%rbp)        # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_zero
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	sbitmap_vector_zero
	movq	24(%r14), %rdi
	callq	first_insn_after_basic_block_note
	testq	%rax, %rax
	je	.LBB8_42
# BB#8:                                 # %land.lhs.true21.lr.ph
	movq	%r14, %r12
	xorq	%r15, %r15
	movabsq	$-8589934592, %r14      # imm = 0xFFFFFFFE00000000
	movabsq	$4294967232, %r13       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB8_9:                                # %land.lhs.true21
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_13 Depth 2
                                        #     Child Loop BB8_19 Depth 2
                                        #     Child Loop BB8_23 Depth 2
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	jne	.LBB8_28
# BB#10:                                # %land.lhs.true26
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	32(%rax), %r8
	movzbl	(%r8), %ecx
	cmpq	$47, %rcx
	jne	.LBB8_28
# BB#11:                                # %land.rhs34
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	16(%r8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$152, %rdx
	jne	.LBB8_28
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	8(%rcx), %r9
	movq	16(%r12), %rcx
	movslq	(%r9), %rdi
	movslq	88(%rcx), %rbx
	movq	$32, %rcx
	leaq	-2(%rdi), %rsi
	shlq	%cl, %rdi
	.align	16, 0x90
.LBB8_13:                               # %for.cond.i
                                        #   Parent Loop BB8_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB8_27
# BB#14:                                # %for.body.i
                                        #   in Loop: Header=BB8_13 Depth=2
	movq	16(%r9,%rsi,8), %rcx
	movq	8(%rcx), %rcx
	addq	$-2, %rsi
	addq	%r14, %rdi
	cmpq	%rbx, %rcx
	jne	.LBB8_13
# BB#15:                                # %phi_alternative.exit
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	8(%r9,%rdi,8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB8_74
# BB#16:                                # %lor.lhs.false
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	8(%r8), %rdx
	movzbl	(%rdx), %esi
	cmpq	$61, %rsi
	jne	.LBB8_74
# BB#17:                                # %if.end69
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	cfun(%rip), %rsi
	movl	8(%rcx), %ecx
	leaq	(%rcx,%rcx,2), %rcx
	movl	8(%rdx), %edx
	movq	24(%rsi), %rsi
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movslq	8(%rdi,%rcx,8), %rcx
	movq	80(%rsi), %rsi
	leaq	(%rdx,%rdx,2), %rdx
	movslq	8(%rdi,%rdx,8), %rdx
	movq	(%rsi,%rcx,8), %rdi
	movq	(%rsi,%rdx,8), %rsi
	cmpq	%rsi, %rdi
	je	.LBB8_27
# BB#18:                                # %if.then88
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	$32, %rcx
	movq	%r15, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%r9, %rbx
	.align	16, 0x90
.LBB8_19:                               # %for.cond.i222
                                        #   Parent Loop BB8_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB8_75
# BB#20:                                # %for.body.i225
                                        #   in Loop: Header=BB8_19 Depth=2
	movq	-56(%rbp), %rcx         # 8-byte Reload
	movq	-8(%rcx,%rbx,8), %rcx
	movl	8(%rdi), %edx
	movl	8(%rcx), %ecx
	decq	%rbx
	cmpq	%rcx, %rdx
	jne	.LBB8_19
# BB#21:                                # %return.loopexit.i
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	%r15, %r8
	movq	%rbx, %r15
	jmp	.LBB8_22
.LBB8_75:                               # %for.end.i
                                        #   in Loop: Header=BB8_9 Depth=1
	leaq	1(%r15), %r8
	movq	-56(%rbp), %rcx         # 8-byte Reload
	movq	%rdi, (%rcx,%r9,8)
.LBB8_22:                               # %ephi_add_node.exit
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, %rbx
	.align	16, 0x90
.LBB8_23:                               # %for.cond.i232
                                        #   Parent Loop BB8_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB8_76
# BB#24:                                # %for.body.i237
                                        #   in Loop: Header=BB8_23 Depth=2
	movq	-56(%rbp), %rcx         # 8-byte Reload
	movq	-8(%rcx,%rbx,8), %rcx
	movl	8(%rsi), %edi
	movl	8(%rcx), %ecx
	decq	%rbx
	cmpq	%rcx, %rdi
	jne	.LBB8_23
# BB#25:                                # %return.loopexit.i241
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	%r8, %r9
	movq	%rbx, %r8
	jmp	.LBB8_26
.LBB8_76:                               # %for.end.i240
                                        #   in Loop: Header=BB8_9 Depth=1
	leaq	1(%r8), %r9
	movq	-56(%rbp), %rcx         # 8-byte Reload
	movq	%rsi, (%rcx,%rdx,8)
.LBB8_26:                               # %ephi_add_node.exit243
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	%r8, %rdx
	andq	%r13, %rdx
	movq	$1, %r10
	movq	%r8, %rcx
	shlq	%cl, %r10
	movq	%r15, %rsi
	movq	%rsi, %rbx
	movq	%rsi, %rdi
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	-88(%rbp), %rcx         # 8-byte Reload
	movq	(%rcx,%rbx,8), %rbx
	movq	$3, %rcx
	shrq	%cl, %rdx
	orq	%r10, 16(%rbx,%rdx)
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	-112(%rbp), %rcx        # 8-byte Reload
	movq	(%rcx,%r8,8), %rsi
	andq	%r13, %rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	orq	%rdx, 16(%rsi,%rdi)
	movq	%r9, %r15
.LBB8_27:                               # %for.inc
                                        #   in Loop: Header=BB8_9 Depth=1
	movq	%rax, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	jne	.LBB8_9
.LBB8_28:                               # %for.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rbx
	testq	%rax, %rbx
	je	.LBB8_42
# BB#29:                                # %if.end110
	movq	%r12, -136(%rbp)        # 8-byte Spill
	movq	%rbx, %rdi
	callq	sbitmap_alloc
	movq	%rax, %rdi
	movq	%rax, %r12
	callq	sbitmap_zero
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,4), %rcx
	andq	$-16, %rcx
	movq	%rsp, %rdx
	subq	%rcx, %rdx
	movq	%rdx, -120(%rbp)        # 8-byte Spill
	movq	%rdx, %rsp
	testq	%rax, %rax
	movq	%rdx, %r15
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r13, %r14
	movq	%rbx, %r13
	jle	.LBB8_35
# BB#30:
	xorq	%rbx, %rbx
	movq	-120(%rbp), %r15        # 8-byte Reload
	.align	16, 0x90
.LBB8_31:                               # %for.body117
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	andq	%r14, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	%r12, %rsi
	movq	16(%rsi,%rax), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB8_32
# BB#33:                                # %if.then125
                                        #   in Loop: Header=BB8_31 Depth=1
	movq	%rbx, %rdi
	movq	%rsi, %r12
	movq	-112(%rbp), %rdx        # 8-byte Reload
	movq	%r15, %rcx
	movq	%r14, %r15
	movq	%r8, %r14
	callq	ephi_forward
	movq	%r14, %r8
	movq	%r15, %r14
	movq	%rax, %r15
	jmp	.LBB8_34
	.align	16, 0x90
.LBB8_32:                               #   in Loop: Header=BB8_31 Depth=1
	movq	%rsi, %r12
.LBB8_34:                               # %for.inc128
                                        #   in Loop: Header=BB8_31 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r8, %rax
	movq	%r13, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	jne	.LBB8_31
.LBB8_35:                               # %for.end129
	movq	%r8, %r13
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	sbitmap_zero
	callq	start_sequence
	movq	%rbx, %r9
	movq	-120(%rbp), %rax        # 8-byte Reload
	movq	%r15, %rcx
	jmp	.LBB8_36
	.align	16, 0x90
.LBB8_38:                               # %while.cond130.backedge
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	-104(%rbp), %rcx        # 8-byte Reload
	addq	$-4, %rcx
	movq	-120(%rbp), %rax        # 8-byte Reload
.LBB8_36:                               # %for.end129
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_46 Depth 2
                                        #       Child Loop BB8_48 Depth 3
                                        #     Child Loop BB8_60 Depth 2
                                        #       Child Loop BB8_62 Depth 3
                                        #     Child Loop BB8_69 Depth 2
                                        #       Child Loop BB8_70 Depth 3
	cmpq	%rax, %rcx
	je	.LBB8_39
# BB#37:                                # %while.body133
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	%rcx, -104(%rbp)        # 8-byte Spill
	movl	-4(%rcx), %eax
	movq	$32, %rcx
	movq	%rax, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$6, %rcx
	movq	%rax, %r15
	shrq	%cl, %r15
	movq	16(%r9,%r15,8), %r11
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%r11, %rsi
	jne	.LBB8_38
# BB#44:                                # %if.then143
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	(%rax,%r12,8), %rax
	movq	%rax, -72(%rbp)         # 8-byte Spill
	movl	4(%rax), %eax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB8_68
# BB#45:                                #   in Loop: Header=BB8_36 Depth=1
	movq	%rsi, -128(%rbp)        # 8-byte Spill
	movq	%r11, -96(%rbp)         # 8-byte Spill
	movq	%r15, -80(%rbp)         # 8-byte Spill
	movq	%r9, %r10
	xorq	%r15, %r15
	movq	$0, -48(%rbp)           # 8-byte Folded Spill
	xorq	%r11, %r11
	movq	%r13, %r8
	.align	16, 0x90
.LBB8_46:                               # %for.body.i215
                                        #   Parent Loop BB8_36 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB8_48 Depth 3
	movq	-72(%rbp), %rax         # 8-byte Reload
	movq	16(%rax,%r15,8), %rdi
	testq	%rdi, %rdi
	je	.LBB8_55
# BB#47:                                # %for.body8.preheader.i
                                        #   in Loop: Header=BB8_46 Depth=2
	movq	$6, %rcx
	movq	%r15, %r13
	shlq	%cl, %r13
	movq	$1, %rdx
	.align	16, 0x90
.LBB8_48:                               # %for.body8.i
                                        #   Parent Loop BB8_36 Depth=1
                                        #     Parent Loop BB8_46 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rdx), %rsi
	movq	$1, %rbx
	movq	%rsi, %rcx
	shlq	%cl, %rbx
	testq	%rdi, %rbx
	je	.LBB8_54
# BB#49:                                # %if.then10.i
                                        #   in Loop: Header=BB8_48 Depth=3
	movq	%r8, %r9
	addq	%r13, %rsi
	movq	%rsi, %rax
	andq	%r14, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	16(%r10,%rax), %rax
	movq	$1, %r8
	movq	%rsi, %rcx
	shlq	%cl, %r8
	testq	%r8, %rax
	jne	.LBB8_51
# BB#50:                                #   in Loop: Header=BB8_48 Depth=3
	movq	$1, %r11
	jmp	.LBB8_53
	.align	16, 0x90
.LBB8_51:                               # %if.else.i
                                        #   in Loop: Header=BB8_48 Depth=3
	cmpq	$0, -48(%rbp)           # 8-byte Folded Reload
	jne	.LBB8_53
# BB#52:                                # %if.then19.i
                                        #   in Loop: Header=BB8_48 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	(%rax,%rsi,8), %rax
	movq	%rax, -48(%rbp)         # 8-byte Spill
.LBB8_53:                               # %if.end22.i
                                        #   in Loop: Header=BB8_48 Depth=3
	movq	%r9, %r8
	notq	%rbx
	andq	%rbx, %rdi
	je	.LBB8_55
.LBB8_54:                               # %for.inc.i
                                        #   in Loop: Header=BB8_48 Depth=3
	movq	%rdx, %rax
	andq	%r8, %rax
	incq	%rdx
	cmpq	$64, %rax
	jb	.LBB8_48
.LBB8_55:                               # %for.inc28.i
                                        #   in Loop: Header=BB8_46 Depth=2
	incq	%r15
	movq	%r15, %rax
	andq	%r8, %rax
	movq	-64(%rbp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB8_46
# BB#56:                                # %do.end.i
                                        #   in Loop: Header=BB8_36 Depth=1
	testq	%r8, %r11
	movq	%r8, %r13
	movq	%r10, %r9
	movq	-80(%rbp), %r15         # 8-byte Reload
	movq	-96(%rbp), %r11         # 8-byte Reload
	movq	-128(%rbp), %rsi        # 8-byte Reload
	jne	.LBB8_57
.LBB8_68:                               # %do.body101.i
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%r12,8), %r10
	movl	4(%r10), %r8d
	xorq	%rdx, %rdx
	testq	%r8, %r8
	je	.LBB8_38
.LBB8_69:                               # %for.body115.i
                                        #   Parent Loop BB8_36 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB8_70 Depth 3
	movq	16(%r10,%rdx,8), %rbx
	testq	%rbx, %rbx
	movabsq	$0, %rax
	je	.LBB8_73
	.align	16, 0x90
.LBB8_70:                               # %for.body123.i
                                        #   Parent Loop BB8_36 Depth=1
                                        #     Parent Loop BB8_69 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rbx, %rdi
	jne	.LBB8_71
# BB#72:                                # %for.inc147.i
                                        #   in Loop: Header=BB8_70 Depth=3
	incq	%rax
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	$64, %rcx
	jb	.LBB8_70
.LBB8_73:                               # %for.inc151.i
                                        #   in Loop: Header=BB8_69 Depth=2
	incq	%rdx
	movq	%rdx, %rax
	andq	%r13, %rax
	cmpq	%r8, %rax
	jb	.LBB8_69
	jmp	.LBB8_38
.LBB8_71:                               # %if.then129.i
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	$6, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	orq	%rsi, %r11
	movq	%r11, 16(%r9,%r15,8)
	movq	-56(%rbp), %rdx         # 8-byte Reload
	movq	(%rdx,%r12,8), %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rsi
	movq	%r9, %rbx
	callq	emit_move_insn
	movq	%rbx, %r9
	jmp	.LBB8_38
.LBB8_57:                               # %if.then32.i
                                        #   in Loop: Header=BB8_36 Depth=1
	cmpq	$0, -48(%rbp)           # 8-byte Folded Reload
	jne	.LBB8_59
# BB#58:                                # %if.then34.i
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	(%rax,%r12,8), %rbx
	movzbl	2(%rbx), %edi
	movq	%r9, %r15
	callq	gen_reg_rtx
	movq	%rax, -48(%rbp)         # 8-byte Spill
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	%r15, %r9
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	(%rax,%r12,8), %rax
	movq	%rax, -72(%rbp)         # 8-byte Spill
	movl	4(%rax), %eax
	movq	%rax, -64(%rbp)         # 8-byte Spill
.LBB8_59:                               # %do.body41.i
                                        #   in Loop: Header=BB8_36 Depth=1
	xorq	%rdx, %rdx
	movq	-64(%rbp), %rax         # 8-byte Reload
	testq	%r13, %rax
	je	.LBB8_38
	.align	16, 0x90
.LBB8_60:                               # %for.body55.i
                                        #   Parent Loop BB8_36 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB8_62 Depth 3
	movq	%rdx, -96(%rbp)         # 8-byte Spill
	movq	-72(%rbp), %rax         # 8-byte Reload
	movq	16(%rax,%rdx,8), %r14
	testq	%r14, %r14
	movq	%r13, %rsi
	je	.LBB8_67
# BB#61:                                # %for.body63.preheader.i
                                        #   in Loop: Header=BB8_60 Depth=2
	movq	$6, %rcx
	movq	-96(%rbp), %rdi         # 8-byte Reload
	shlq	%cl, %rdi
	movq	%rdi, -80(%rbp)         # 8-byte Spill
	movq	$1, %r15
	.align	16, 0x90
.LBB8_62:                               # %for.body63.i
                                        #   Parent Loop BB8_36 Depth=1
                                        #     Parent Loop BB8_60 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%r15), %r12
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%r14, %rax
	je	.LBB8_66
# BB#63:                                # %if.then69.i
                                        #   in Loop: Header=BB8_62 Depth=3
	notq	%rax
	addq	%rdi, %r12
	movq	%r12, %rdx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	16(%r9,%rdx), %rdx
	andq	%rax, %r14
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rax, %rdx
	jne	.LBB8_65
# BB#64:                                # %if.then83.i
                                        #   in Loop: Header=BB8_62 Depth=3
	movq	%r12, %rdi
	movq	%r9, %rsi
	movq	-88(%rbp), %rdx         # 8-byte Reload
	movq	-56(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r9, %r13
	callq	ephi_backward
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	(%rbx,%r12,8), %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	callq	emit_move_insn
	movq	-80(%rbp), %rdi         # 8-byte Reload
	movq	%r13, %r9
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB8_65:                               # %if.end87.i
                                        #   in Loop: Header=BB8_62 Depth=3
	testq	%r14, %r14
	je	.LBB8_67
.LBB8_66:                               # %for.inc92.i
                                        #   in Loop: Header=BB8_62 Depth=3
	movq	%r15, %rax
	andq	%rsi, %rax
	incq	%r15
	cmpq	$64, %rax
	jb	.LBB8_62
.LBB8_67:                               # %for.inc96.i
                                        #   in Loop: Header=BB8_60 Depth=2
	movq	-96(%rbp), %rdx         # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	andq	%rsi, %rax
	movq	-64(%rbp), %rcx         # 8-byte Reload
	andq	%rsi, %rcx
	movq	%rsi, %r13
	cmpq	%rcx, %rax
	movabsq	$4294967232, %r14       # imm = 0xFFFFFFC0
	jne	.LBB8_60
	jmp	.LBB8_38
.LBB8_39:                               # %while.end145
	movq	%r9, %r14
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	-136(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rsi
	callq	insert_insn_on_edge
	movq	rtl_dump_file(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB8_41
# BB#40:                                # %if.then148
	movq	16(%rbx), %rax
	movq	24(%rbx), %rcx
	movl	88(%rcx), %ecx
	movl	88(%rax), %edx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	callq	fprintf
.LBB8_41:                               # %if.end154
	movq	%r14, %rdi
	callq	free
.LBB8_42:                               # %out
	movq	-88(%rbp), %rdi         # 8-byte Reload
	callq	free
	movq	-112(%rbp), %rdi        # 8-byte Reload
	callq	free
.LBB8_43:                               # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB8_74:                               # %if.then68
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.eliminate_phi, %rcx
	movq	$1398, %rsi             # imm = 0x576
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp90:
	.size	eliminate_phi, .Ltmp90-eliminate_phi
	.cfi_endproc

	.globl	for_each_successor_phi
	.align	16, 0x90
	.type	for_each_successor_phi,@function
for_each_successor_phi:                 # @for_each_successor_phi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp97:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp98:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp99:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp100:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp102:
	.cfi_def_cfa_offset 64
.Ltmp103:
	.cfi_offset %rbx, -48
.Ltmp104:
	.cfi_offset %r12, -40
.Ltmp105:
	.cfi_offset %r13, -32
.Ltmp106:
	.cfi_offset %r14, -24
.Ltmp107:
	.cfi_offset %r15, -16
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, (%rsp)            # 8-byte Spill
	movq	%rdi, %r12
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %r12
	je	.LBB9_16
# BB#1:                                 # %if.end
	movq	40(%r12), %r13
	testq	%r13, %r13
	je	.LBB9_16
# BB#2:                                 # %for.body.lr.ph
	movabsq	$-8589934592, %r14      # imm = 0xFFFFFFFE00000000
	.align	16, 0x90
.LBB9_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_6 Depth 2
                                        #       Child Loop BB9_11 Depth 3
	movq	24(%r13), %rdi
	movabsq	$entry_exit_blocks, %rax
	cmpq	%rax, %rdi
	je	.LBB9_15
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB9_3 Depth=1
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdi
	je	.LBB9_15
# BB#5:                                 # %if.end5
                                        #   in Loop: Header=BB9_3 Depth=1
	callq	first_insn_after_basic_block_note
	movq	%rax, %r15
	jmp	.LBB9_6
	.align	16, 0x90
.LBB9_14:                               # %for.inc
                                        #   in Loop: Header=BB9_6 Depth=2
	movq	24(%r15), %r15
.LBB9_6:                                # %if.end5
                                        #   Parent Loop BB9_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_11 Depth 3
	testq	%r15, %r15
	je	.LBB9_15
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_6 Depth=2
	movzbl	(%r15), %eax
	cmpq	$32, %rax
	jne	.LBB9_15
# BB#8:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB9_6 Depth=2
	movq	32(%r15), %r8
	movzbl	(%r8), %eax
	cmpq	$47, %rax
	jne	.LBB9_15
# BB#9:                                 # %land.rhs
                                        #   in Loop: Header=BB9_6 Depth=2
	movq	16(%r8), %rax
	movzbl	(%rax), %ecx
	cmpq	$152, %rcx
	jne	.LBB9_15
# BB#10:                                # %for.body24
                                        #   in Loop: Header=BB9_6 Depth=2
	movq	8(%rax), %rdx
	movslq	(%rdx), %rsi
	movslq	88(%r12), %rdi
	movq	$32, %rcx
	leaq	-2(%rsi), %rbx
	shlq	%cl, %rsi
	.align	16, 0x90
.LBB9_11:                               # %for.cond.i
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB9_14
# BB#12:                                # %for.body.i
                                        #   in Loop: Header=BB9_11 Depth=3
	movq	16(%rdx,%rbx,8), %rax
	movq	8(%rax), %rax
	addq	$-2, %rbx
	addq	%r14, %rsi
	cmpq	%rdi, %rax
	jne	.LBB9_11
# BB#13:                                # %if.end31
                                        #   in Loop: Header=BB9_6 Depth=2
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	8(%rdx,%rsi,8), %rax
	movq	8(%r8), %rcx
	movl	8(%rax), %edx
	movl	8(%rcx), %esi
	movq	%r15, %rdi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	*(%rsp)                 # 8-byte Folded Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_14
	jmp	.LBB9_17
	.align	16, 0x90
.LBB9_15:                               # %for.inc47
                                        #   in Loop: Header=BB9_3 Depth=1
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB9_3
.LBB9_16:
	xorq	%rax, %rax
.LBB9_17:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp108:
	.size	for_each_successor_phi, .Ltmp108-for_each_successor_phi
	.cfi_endproc

	.align	16, 0x90
	.type	ephi_forward,@function
ephi_forward:                           # @ephi_forward
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp115:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp117:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp118:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp119:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp120:
	.cfi_def_cfa_offset 96
.Ltmp121:
	.cfi_offset %rbx, -48
.Ltmp122:
	.cfi_offset %r12, -40
.Ltmp123:
	.cfi_offset %r13, -32
.Ltmp124:
	.cfi_offset %r14, -24
.Ltmp125:
	.cfi_offset %r15, -16
	movq	%rcx, %r9
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	movq	%rdi, %rax
	andq	%rcx, %rax
	movq	$1, %r8
	movq	%rdi, %rcx
	shlq	%cl, %r8
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%r8, 16(%rsi,%rax)
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movl	4(%rcx), %ecx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB10_12
# BB#1:
	xorq	%rax, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_5 Depth 2
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx,%rax,8), %r12
	testq	%r12, %r12
	jne	.LBB10_4
# BB#3:                                 #   in Loop: Header=BB10_2 Depth=1
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB10_11
	.align	16, 0x90
.LBB10_4:                               # %for.body11.preheader
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$6, %rcx
	movq	%rax, %r14
	movq	%rax, 32(%rsp)          # 8-byte Spill
	shlq	%cl, %r14
	movq	$1, %r13
	.align	16, 0x90
.LBB10_5:                               # %for.body11
                                        #   Parent Loop BB10_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r13), %rdx
	movq	$1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB10_10
# BB#6:                                 # %if.then15
                                        #   in Loop: Header=BB10_5 Depth=2
	movq	%rdi, %rbx
	notq	%rax
	addq	%r14, %rdx
	movq	%rdx, %rdi
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	movq	16(%rsi,%rdi), %rdi
	andq	%rax, %r12
	movq	$1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	testq	%rax, %rdi
	jne	.LBB10_7
# BB#8:                                 # %if.then24
                                        #   in Loop: Header=BB10_5 Depth=2
	movq	%rdx, %rdi
	movq	%rsi, %r15
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r9, %rcx
	callq	ephi_forward
	movq	%rax, %r9
	jmp	.LBB10_9
	.align	16, 0x90
.LBB10_7:                               #   in Loop: Header=BB10_5 Depth=2
	movq	%rsi, %r15
.LBB10_9:                               # %if.end
                                        #   in Loop: Header=BB10_5 Depth=2
	testq	%r12, %r12
	movq	%rbx, %rdi
	movq	%r15, %rsi
	je	.LBB10_11
.LBB10_10:                              # %for.inc
                                        #   in Loop: Header=BB10_5 Depth=2
	movq	%r13, %rcx
	andq	%rdi, %rcx
	incq	%r13
	cmpq	$64, %rcx
	jb	.LBB10_5
.LBB10_11:                              # %for.inc30
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, %rcx
	andq	%rdi, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB10_2
.LBB10_12:                              # %do.end
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%ecx, (%r9)
	addq	$4, %r9
	movq	%r9, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp126:
	.size	ephi_forward, .Ltmp126-ephi_forward
	.cfi_endproc

	.align	16, 0x90
	.type	ephi_backward,@function
ephi_backward:                          # @ephi_backward
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp135:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp136:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp138:
	.cfi_def_cfa_offset 112
.Ltmp139:
	.cfi_offset %rbx, -48
.Ltmp140:
	.cfi_offset %r12, -40
.Ltmp141:
	.cfi_offset %r13, -32
.Ltmp142:
	.cfi_offset %r14, -24
.Ltmp143:
	.cfi_offset %r15, -16
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	movq	%rdi, %rax
	andq	%rcx, %rax
	movq	$1, %rbx
	movq	%rdi, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rbx, 16(%rsi,%rax)
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	(%rdx,%rdi,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	4(%rax), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rax, %rax
	je	.LBB11_10
# BB#1:                                 # %for.body.lr.ph
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	48(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB11_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_4 Depth 2
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rax,%rdx,8), %r12
	testq	%r12, %r12
	je	.LBB11_9
# BB#3:                                 # %for.body11.preheader
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	$6, %rcx
	movq	16(%rsp), %r8           # 8-byte Reload
	shlq	%cl, %r8
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	$1, %r13
	.align	16, 0x90
.LBB11_4:                               # %for.body11
                                        #   Parent Loop BB11_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r13), %rbx
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB11_8
# BB#5:                                 # %if.then15
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	%rdi, %r14
	notq	%rax
	addq	%r8, %rbx
	movq	%rbx, %rdx
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	16(%rsi,%rdx), %rdx
	andq	%rax, %r12
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%rax, %rdx
	jne	.LBB11_7
# BB#6:                                 # %if.then24
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	%rbx, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	callq	ephi_backward
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%r15,%rbx,8), %rdi
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%r15,%rax,8), %rsi
	callq	emit_move_insn
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
.LBB11_7:                               # %if.end
                                        #   in Loop: Header=BB11_4 Depth=2
	testq	%r12, %r12
	movq	%r14, %rdi
	je	.LBB11_9
.LBB11_8:                               # %for.inc
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	%r13, %rax
	andq	%rdi, %rax
	incq	%r13
	cmpq	$64, %rax
	jb	.LBB11_4
.LBB11_9:                               # %for.inc34
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	16(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	andq	%rdi, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB11_2
.LBB11_10:                              # %do.end
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp144:
	.size	ephi_backward, .Ltmp144-ephi_backward
	.cfi_endproc

	.align	16, 0x90
	.type	rename_equivalent_regs_in_insn,@function
rename_equivalent_regs_in_insn:         # @rename_equivalent_regs_in_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp153:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp154:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp155:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp156:
	.cfi_def_cfa_offset 64
.Ltmp157:
	.cfi_offset %rbx, -48
.Ltmp158:
	.cfi_offset %r12, -40
.Ltmp159:
	.cfi_offset %r13, -32
.Ltmp160:
	.cfi_offset %r14, -24
.Ltmp161:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB12_14
# BB#1:                                 # %if.end
	movzbl	(%rbx), %eax
	cmpq	$152, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB12_14
# BB#2:                                 # %if.end
	cmpq	$61, %rax
	jne	.LBB12_13
# BB#3:                                 # %sw.bb
	movl	8(%rbx), %r13d
	cmpq	$52, %r13
	ja	.LBB12_5
# BB#4:                                 # %sw.bb
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%r13, %rax
	cmpq	$16, %rax
	jne	.LBB12_14
.LBB12_5:                               # %if.then11
	leaq	(%r13,%r13,2), %rax
	movl	8(%rsi,%rax,8), %r15d
	movq	ssa_rename_from_ht(%rip), %rdi
	movl	%r15d, (%rsp)
	movq	$0, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	%r15, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB12_10
# BB#6:                                 # %ssa_rename_from_lookup.exit
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB12_10
# BB#7:                                 # %land.lhs.true
	movl	8(%rax), %ecx
	cmpq	$52, %rcx
	ja	.LBB12_10
# BB#8:                                 # %if.then21
	cmpq	%r13, %rcx
	je	.LBB12_14
# BB#9:                                 # %if.then26
	movq	%rax, (%r14)
	jmp	.LBB12_14
.LBB12_13:                              # %sw.default
	xorq	%r12, %r12
	jmp	.LBB12_14
.LBB12_10:                              # %if.else
	cmpq	%r15, %r13
	je	.LBB12_14
# BB#11:                                # %if.then29
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r15,8), %rax
	movl	(%rbx), %ecx
	movl	(%rax), %edx
	xorq	%rcx, %rdx
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB12_15
# BB#12:                                # %if.end39
	movq	%rax, (%r14)
.LBB12_14:                              # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_15:                              # %if.then38
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rename_equivalent_regs_in_insn, %rcx
	movq	$2008, %rsi             # imm = 0x7D8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp162:
	.size	rename_equivalent_regs_in_insn, .Ltmp162-rename_equivalent_regs_in_insn
	.cfi_endproc

	.align	16, 0x90
	.type	record_canonical_element_1,@function
record_canonical_element_1:             # @record_canonical_element_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	movl	(%rax), %eax
	movq	8(%rsi), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	8(%rcx,%rax,8), %ecx
	movq	(%rsi), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rax,%rcx,8)
	movq	$1, %rax
	retq
.Ltmp163:
	.size	record_canonical_element_1, .Ltmp163-record_canonical_element_1
	.cfi_endproc

	.align	16, 0x90
	.type	coalesce_reg_in_phi,@function
coalesce_reg_in_phi:                    # @coalesce_reg_in_phi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp166:
	.cfi_def_cfa_offset 16
.Ltmp167:
	.cfi_offset %rbx, -16
	movq	%rcx, %rbx
	movq	(%rbx), %rdi
	movq	8(%rbx), %rax
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	movq	%rax, %rsi
	callq	coalesce_if_unconflicting
	movl	16(%rbx), %ecx
	addq	%rax, %rcx
	xorq	%rax, %rax
	movl	%ecx, 16(%rbx)
	popq	%rbx
	retq
.Ltmp168:
	.size	coalesce_reg_in_phi, .Ltmp168-coalesce_reg_in_phi
	.cfi_endproc

	.align	16, 0x90
	.type	coalesce_if_unconflicting,@function
coalesce_if_unconflicting:              # @coalesce_if_unconflicting
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
	subq	$64, %rsp
.Ltmp180:
	.cfi_def_cfa_offset 112
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
	movq	%rcx, %rax
	movq	%rdi, %r15
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$4294967294, %rbx       # imm = 0xFFFFFFFE
	xorq	%r14, %r14
	cmpq	$52, %rdi
	jg	.LBB15_2
# BB#1:                                 # %entry
	movq	%rdx, %rcx
	andq	%rbx, %rcx
	cmpq	$16, %rcx
	jne	.LBB15_25
.LBB15_2:                               # %lor.lhs.false4
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$52, %rdi
	jg	.LBB15_4
# BB#3:                                 # %lor.lhs.false4
	andq	%rax, %rbx
	cmpq	$16, %rbx
	jne	.LBB15_25
.LBB15_4:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rcx
	movl	8(%r15,%rcx,8), %ebx
	movq	$32, %rcx
	movq	%rbx, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	movl	8(%r15,%rax,8), %eax
	cmpq	%rax, %rbx
	je	.LBB15_25
# BB#5:                                 # %if.end16
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	ssa_rename_from_ht(%rip), %rdi
	movl	%ebx, 32(%rsp)
	movq	$0, 40(%rsp)
	leaq	32(%rsp), %rsi
	movq	%rbx, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	movq	%rbx, %r13
	je	.LBB15_8
# BB#6:                                 # %ssa_rename_from_lookup.exit.i.i
	movq	8(%rax), %rax
	testq	%rax, %rax
	movq	%rbx, %r13
	je	.LBB15_8
# BB#7:                                 # %cond.true.i.i
	movl	8(%rax), %r13d
.LBB15_8:                               # %original_register.exit.i
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	ssa_rename_from_ht(%rip), %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movl	%ebx, 48(%rsp)
	movq	$0, 56(%rsp)
	leaq	48(%rsp), %rsi
	movq	%rbx, %rdx
	callq	htab_find_with_hash
	movq	%rbx, %rcx
	testq	%rax, %rax
                                        # kill: RBX<def> RCX<kill>
	je	.LBB15_11
# BB#9:                                 # %ssa_rename_from_lookup.exit.i31.i
	movq	8(%rax), %rax
	testq	%rax, %rax
	movq	%rcx, %rbx
	je	.LBB15_11
# BB#10:                                # %cond.true.i34.i
	movl	8(%rax), %ebx
.LBB15_11:                              # %original_register.exit36.i
	movq	%rcx, %r10
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	movabsq	$0, %rax
	movabsq	$1, %r8
	movq	%rax, %r9
	jle	.LBB15_13
# BB#12:                                # %original_register.exit36.i
	movq	%r8, %r9
.LBB15_13:                              # %original_register.exit36.i
	movq	%r13, %rdi
	andq	%rsi, %rdi
	movq	%rbx, %rdx
	andq	%rsi, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$52, %rsi
	jle	.LBB15_15
# BB#14:                                # %original_register.exit36.i
	movq	%r8, %rax
.LBB15_15:                              # %original_register.exit36.i
	cmpq	%rdx, %rdi
	je	.LBB15_17
# BB#16:                                # %original_register.exit36.i
	orq	%rax, %r9
	je	.LBB15_25
.LBB15_17:                              # %if.end.i
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$53, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rdx
	jge	.LBB15_19
# BB#18:                                # %if.end.i
	movq	%rsi, %rdx
.LBB15_19:                              # %if.end.i
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	$52, %r13
	movq	16(%rsp), %r13          # 8-byte Reload
	jle	.LBB15_21
# BB#20:                                # %if.end.i
	movq	%rsi, %rax
.LBB15_21:                              # %if.end.i
	testq	%rdx, %rax
	jne	.LBB15_25
# BB#22:                                # %if.end.i
	orq	%rdx, %rax
	xorq	$1, %rax
	jne	.LBB15_25
# BB#23:                                # %lor.lhs.false17
	movq	%r13, %rdi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r10, %rdx
	callq	conflict_graph_conflict_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_25
# BB#24:                                # %if.end21
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	callq	partition_union
	leaq	(%r12,%r12,2), %rax
	movq	%r13, %r12
	movl	8(%r15,%rax,8), %r15d
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	conflict_graph_merge_regs
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	conflict_graph_merge_regs
	movq	$1, %r14
.LBB15_25:                              # %return
	movq	%r14, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp186:
	.size	coalesce_if_unconflicting, .Ltmp186-coalesce_if_unconflicting
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reg_in_phi,@function
mark_reg_in_phi:                        # @mark_reg_in_phi
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp188:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	je	.LBB16_4
# BB#1:                                 # %entry
	cmpq	$152, %rcx
	je	.LBB16_4
# BB#2:                                 # %entry
	cmpq	$61, %rcx
	jne	.LBB16_5
# BB#3:                                 # %sw.bb
	movl	8(%rax), %eax
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	bitmap_set_bit
.LBB16_4:                               # %sw.bb1
	xorq	%rax, %rax
	popq	%rdx
	retq
.LBB16_5:                               # %sw.default
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.mark_reg_in_phi, %rcx
	movq	$1924, %rsi             # imm = 0x784
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp189:
	.size	mark_reg_in_phi, .Ltmp189-mark_reg_in_phi
	.cfi_endproc

	.align	16, 0x90
	.type	rename_block,@function
rename_block:                           # @rename_block
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
	subq	$80, %rsp
.Ltmp201:
	.cfi_def_cfa_offset 128
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
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %r15
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	32(%rax,%r15,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	8(%rax), %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movabsq	$rename_insn_1, %r13
	.align	16, 0x90
.LBB17_1:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_4 Depth 2
	movq	(%rax), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB17_7
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%rcx, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	%r12, 56(%rsp)
	callq	start_sequence
	leaq	32(%r12), %r14
	movq	%r14, %rdi
	movq	%r13, %rsi
	leaq	40(%rsp), %rbx
	movq	%rbx, %rdx
	callq	for_each_rtx
	leaq	56(%r12), %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	for_each_rtx
	callq	get_insns
	testq	%rax, %rax
	je	.LBB17_6
# BB#3:                                 # %if.then13
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	(%r14), %rdi
	callq	emit
	callq	gen_sequence
	movq	8(%rax), %rdx
	movslq	(%rdx), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB17_5
	.align	16, 0x90
.LBB17_4:                               # %for.body
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx,%rbx,8), %rcx
	movq	32(%rcx), %rcx
	movq	%rcx, (%rdx,%rbx,8)
	movq	8(%rax), %rdx
	movslq	(%rdx), %rdi
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdi, %rsi
	jl	.LBB17_4
.LBB17_5:                               # %for.end
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%rax, (%r14)
	movabsq	$rename_insn_1, %rbx
	movq	%rbx, %r13
.LBB17_6:                               # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	callq	end_sequence
	leaq	40(%rsp), %rdi
	callq	apply_delayed_renames
	movq	48(%rsp), %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
.LBB17_7:                               # %if.end41
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	%rdx, %r12
	leaq	24(%r12), %rax
	jne	.LBB17_1
# BB#8:                                 # %do.end
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	testq	%rax, %rax
	je	.LBB17_23
# BB#9:
	movabsq	$-8589934592, %r12      # imm = 0xFFFFFFFE00000000
	.align	16, 0x90
.LBB17_10:                              # %for.body48
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_12 Depth 2
                                        #       Child Loop BB17_48 Depth 3
                                        #       Child Loop BB17_40 Depth 3
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%rax), %rdi
	movabsq	$entry_exit_blocks+112, %rax
	cmpq	%rax, %rdi
	je	.LBB17_22
# BB#11:                                # %if.end52
                                        #   in Loop: Header=BB17_10 Depth=1
	callq	first_insn_after_basic_block_note
	movq	%rax, %r14
	jmp	.LBB17_12
	.align	16, 0x90
.LBB17_49:                              # %if.then.i262
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rax, 8(%rdx,%rsi,8)
	movq	24(%r14), %r14
.LBB17_12:                              # %if.end52
                                        #   Parent Loop BB17_10 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB17_48 Depth 3
                                        #       Child Loop BB17_40 Depth 3
	testq	%r14, %r14
	je	.LBB17_22
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB17_12 Depth=2
	movzbl	(%r14), %eax
	cmpq	$32, %rax
	jne	.LBB17_22
# BB#14:                                # %land.lhs.true60
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	32(%r14), %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	jne	.LBB17_22
# BB#15:                                # %land.rhs
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	16(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$152, %rax
	jne	.LBB17_22
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	8(%r13), %rax
	movl	8(%rax), %edx
	movl	ssa_max_reg_num(%rip), %ecx
	cmpq	%rcx, %rdx
	jb	.LBB17_19
# BB#17:                                # %if.then88
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	ssa_rename_from_ht(%rip), %rdi
	movl	%edx, 64(%rsp)
	movq	$0, 72(%rsp)
	leaq	64(%rsp), %rsi
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB17_20
# BB#18:                                # %cond.false.i
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	8(%rax), %rax
.LBB17_19:                              # %if.end93
                                        #   in Loop: Header=BB17_12 Depth=2
	testq	%rax, %rax
	je	.LBB17_20
# BB#32:                                # %if.end97
                                        #   in Loop: Header=BB17_12 Depth=2
	movl	8(%rax), %ecx
	cmpq	$53, %rcx
	jae	.LBB17_33
# BB#34:                                # %if.else.i
                                        #   in Loop: Header=BB17_12 Depth=2
	movzbl	2(%rax), %eax
	imulq	$472, %rcx, %rcx        # imm = 0x1D8
	leaq	ssa_rename_to_hard(%rcx,%rax,8), %rax
	jmp	.LBB17_35
	.align	16, 0x90
.LBB17_33:                              # %if.then.i
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	ssa_rename_to_pseudo(%rip), %rax
	addq	$-53, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	leaq	(%rax,%rcx,8), %rax
.LBB17_35:                              # %ssa_rename_to_lookup.exit
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB17_37
# BB#36:                                # %ssa_rename_to_lookup.exit
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB17_37
# BB#45:                                # %if.else
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	8(%r13), %rsi
	movl	(%rsi), %ebx
	movl	(%rax), %edx
	movq	$16, %rcx
	movq	%rbx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	jne	.LBB17_51
# BB#46:                                # %if.then115
                                        #   in Loop: Header=BB17_12 Depth=2
	andq	$16711680, %rdx         # imm = 0xFF0000
	movabsq	$4278255615, %rcx       # imm = 0xFF00FFFF
	andq	%rcx, %rbx
	orq	%rdx, %rbx
	movl	%ebx, (%rsi)
	jmp	.LBB17_47
	.align	16, 0x90
.LBB17_37:                              # %if.then103
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	16(%r13), %rax
	movq	8(%rax), %rax
	movl	(%rax), %r8d
	leaq	-2(%r8), %r9
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB17_44
# BB#38:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	24(%rsp), %rcx          # 8-byte Reload
	movslq	88(%rcx), %rsi
	movq	$32, %rcx
	movq	%r9, %r10
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	%r10, %rdi
	movq	%r9, %rdx
	.align	16, 0x90
.LBB17_40:                              # %for.body.i
                                        #   Parent Loop BB17_10 Depth=1
                                        #     Parent Loop BB17_12 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	16(%rax,%rdi,8), %rcx
	movq	8(%rcx), %rcx
	cmpq	%rsi, %rcx
	je	.LBB17_41
# BB#39:                                # %for.cond.i
                                        #   in Loop: Header=BB17_40 Depth=3
	addq	$-2, %rdx
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	$-2, %rdi
	testq	%rbx, %rbx
	jns	.LBB17_40
	jmp	.LBB17_44
	.align	16, 0x90
.LBB17_41:                              # %if.then.i233
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB17_43
# BB#42:                                # %if.then12.i
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	8(%rax,%r10,8), %rcx
	movq	%rcx, 8(%rax,%rdi,8)
	decq	%r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	8(%rax,%r8,8), %rcx
	movq	%rcx, 16(%rax,%rdi,8)
.LBB17_43:                              # %remove_phi_alternative.exit
                                        #   in Loop: Header=BB17_12 Depth=2
	movl	%r9d, (%rax)
	movq	24(%r14), %r14
	jmp	.LBB17_12
.LBB17_51:                              # %if.else124
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	%rdx, %rdi
	jne	.LBB17_52
.LBB17_47:                              # %if.end138
                                        #   in Loop: Header=BB17_12 Depth=2
	movq	16(%r13), %rcx
	movq	8(%rcx), %rdx
	movslq	(%rdx), %rsi
	leaq	(%rdx,%rsi,8), %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	.align	16, 0x90
.LBB17_48:                              # %for.cond.i258
                                        #   Parent Loop BB17_10 Depth=1
                                        #     Parent Loop BB17_12 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdi), %rcx
	movq	8(%rcx), %rcx
	addq	$-16, %rdi
	addq	%r12, %rsi
	cmpq	%r15, %rcx
	jne	.LBB17_48
	jmp	.LBB17_49
	.align	16, 0x90
.LBB17_22:                              # %for.inc144
                                        #   in Loop: Header=BB17_10 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB17_10
.LBB17_23:                              # %for.cond146.preheader
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	(%rsp), %rdi            # 8-byte Reload
	jle	.LBB17_28
# BB#24:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	.align	16, 0x90
.LBB17_25:                              # %for.body149
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r15,%rbx,4), %ecx
	cmpq	%r14, %rcx
	jne	.LBB17_27
# BB#26:                                # %if.then154
                                        #   in Loop: Header=BB17_25 Depth=1
	movq	%rdi, %r12
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	rename_block
	movq	%r12, %rdi
	movl	n_basic_blocks(%rip), %eax
.LBB17_27:                              # %for.inc156
                                        #   in Loop: Header=BB17_25 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB17_25
.LBB17_28:                              # %while.cond159.preheader
	testq	%rdi, %rdi
	je	.LBB17_57
# BB#29:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB17_30:                              # %while.body161
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rcx
	movq	16(%rdi), %rdx
	movq	(%rcx), %rax
	cmpq	%rdx, %rax
	jne	.LBB17_31
# BB#53:                                # %if.end168
                                        #   in Loop: Header=BB17_30 Depth=1
	movq	24(%rdi), %rdx
	movq	%rdx, (%rcx)
	movl	8(%rax), %edx
	movq	32(%rdi), %rcx
	cmpq	$53, %rdx
	jae	.LBB17_54
# BB#55:                                # %if.else.i248
                                        #   in Loop: Header=BB17_30 Depth=1
	movzbl	2(%rax), %eax
	imulq	$472, %rdx, %rdx        # imm = 0x1D8
	movq	%rcx, ssa_rename_to_hard(%rdx,%rax,8)
	jmp	.LBB17_56
	.align	16, 0x90
.LBB17_54:                              # %if.then.i241
                                        #   in Loop: Header=BB17_30 Depth=1
	movq	ssa_rename_to_pseudo(%rip), %rax
	addq	$-53, %rdx
	andq	%r14, %rdx
	movq	%rcx, (%rax,%rdx,8)
.LBB17_56:                              # %ssa_rename_to_insert.exit
                                        #   in Loop: Header=BB17_30 Depth=1
	movq	(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB17_30
.LBB17_57:                              # %while.end171
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_44:                              # %if.then106
	movabsq	$.L.str1, %rdi
	movabsq	$.L__FUNCTION__.rename_block, %rdx
	movq	$1059, %rsi             # imm = 0x423
	callq	fancy_abort
.LBB17_20:                              # %if.then96
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rename_block, %rcx
	movq	$1050, %rsi             # imm = 0x41A
	jmp	.LBB17_21
.LBB17_52:                              # %if.then136
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rename_block, %rcx
	movq	$1069, %rsi             # imm = 0x42D
	jmp	.LBB17_21
.LBB17_31:                              # %if.then167
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rename_block, %rcx
	movq	$1094, %rsi             # imm = 0x446
.LBB17_21:                              # %if.then96
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp207:
	.size	rename_block, .Ltmp207-rename_block
	.cfi_endproc

	.align	16, 0x90
	.type	rename_insn_1,@function
rename_insn_1:                          # @rename_insn_1
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
	subq	$16, %rsp
.Ltmp219:
	.cfi_def_cfa_offset 64
.Ltmp220:
	.cfi_offset %rbx, -48
.Ltmp221:
	.cfi_offset %r12, -40
.Ltmp222:
	.cfi_offset %r13, -32
.Ltmp223:
	.cfi_offset %r14, -24
.Ltmp224:
	.cfi_offset %r15, -16
	movq	(%rdi), %r15
	movq	%rsi, %r14
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB18_78
# BB#1:                                 # %if.end
	movl	(%r15), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	cmpq	$151, %rcx
	jg	.LBB18_76
# BB#2:                                 # %if.end
	cmpq	$47, %rcx
	je	.LBB18_11
# BB#3:                                 # %if.end
	cmpq	$49, %rcx
	jne	.LBB18_4
# BB#60:                                # %sw.bb210
	movq	8(%r15), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	movabsq	$0, %rbx
	jne	.LBB18_78
# BB#61:                                # %if.then219
	movl	8(%rcx), %esi
	cmpq	$52, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rdi
	jbe	.LBB18_63
# BB#62:                                # %if.then219
	movq	%r8, %rdi
.LBB18_63:                              # %if.then219
	movabsq	$4294967294, %rbx       # imm = 0xFFFFFFFE
	andq	%rsi, %rbx
	cmpq	$16, %rbx
	jne	.LBB18_65
# BB#64:                                # %if.then219
	movq	%r8, %rcx
.LBB18_65:                              # %if.then219
	orq	%rcx, %rdi
	je	.LBB18_66
# BB#67:                                # %if.then219
	movl	ssa_max_reg_num(%rip), %ecx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	%rcx, %rsi
	movq	%rax, %rbx
	jae	.LBB18_78
# BB#68:                                # %if.then243
	cmpq	$53, %rsi
	jae	.LBB18_69
# BB#70:                                # %if.else.i366
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$2040, %rdx             # imm = 0x7F8
	imulq	$472, %rsi, %rax        # imm = 0x1D8
	leaq	ssa_rename_to_hard(%rax,%rdx), %rax
	jmp	.LBB18_71
.LBB18_76:                              # %if.end
	cmpq	$152, %rcx
	movq	%rax, %rbx
	je	.LBB18_78
	jmp	.LBB18_77
.LBB18_11:                              # %sw.bb
	movq	8(%r15), %r9
	movl	(%r9), %eax
	leaq	8(%r15), %r12
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB18_12
# BB#13:                                # %land.lhs.true
	movq	8(%r9), %rdx
	movq	$16, %rcx
	movq	%rax, %rsi
	movl	(%rdx), %r8d
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %esi
	movq	$16, %rcx
	movq	%r8, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rsi
	jbe	.LBB18_14
# BB#15:                                # %land.lhs.true
	andq	$65535, %r8             # imm = 0xFFFF
	cmpq	$61, %r8
	jne	.LBB18_16
# BB#17:                                # %land.lhs.true29
	movl	8(%rdx), %edi
	cmpq	$52, %rdi
	movabsq	$0, %rax
	movabsq	$1, %r8
	movq	%rax, %rcx
	jbe	.LBB18_19
# BB#18:                                # %land.lhs.true29
	movq	%r8, %rcx
.LBB18_19:                              # %land.lhs.true29
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	andq	%rsi, %rdi
	cmpq	$16, %rdi
	jne	.LBB18_21
# BB#20:                                # %land.lhs.true29
	movq	%r8, %rax
.LBB18_21:                              # %land.lhs.true29
	orq	%rax, %rcx
	leaq	8(%r9), %rcx
	jne	.LBB18_23
# BB#22:                                # %land.lhs.true29
	movq	%r9, %rdx
.LBB18_23:                              # %land.lhs.true29
	jne	.LBB18_25
# BB#24:                                # %land.lhs.true29
	movq	%r12, %rcx
.LBB18_25:                              # %land.lhs.true29
	movl	(%rdx), %eax
	movq	%rcx, %r12
	jmp	.LBB18_26
.LBB18_4:                               # %if.end
	cmpq	$61, %rcx
	jne	.LBB18_77
# BB#5:                                 # %sw.bb166
	movl	8(%r15), %r9d
	cmpq	$52, %r9
	movabsq	$0, %rbx
	movabsq	$1, %r8
	movq	%rbx, %rcx
	jbe	.LBB18_7
# BB#6:                                 # %sw.bb166
	movq	%r8, %rcx
.LBB18_7:                               # %sw.bb166
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	andq	%r9, %rsi
	cmpq	$16, %rsi
	jne	.LBB18_9
# BB#8:                                 # %sw.bb166
	movq	%r8, %rbx
.LBB18_9:                               # %sw.bb166
	orq	%rbx, %rcx
	je	.LBB18_10
# BB#49:                                # %sw.bb166
	movl	ssa_max_reg_num(%rip), %ecx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	%rcx, %r9
	movq	%rax, %rbx
	jae	.LBB18_78
# BB#50:                                # %if.then190
	cmpq	$53, %r9
	jae	.LBB18_51
# BB#52:                                # %if.else.i
	movq	$13, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$2040, %rax             # imm = 0x7F8
	imulq	$472, %r9, %rcx         # imm = 0x1D8
	leaq	ssa_rename_to_hard(%rcx,%rax), %rax
	jmp	.LBB18_53
.LBB18_77:                              # %sw.default
	xorq	%rbx, %rbx
.LBB18_78:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_12:
	movq	%r9, %rdx
	jmp	.LBB18_26
.LBB18_66:
	movq	%rax, %rbx
	jmp	.LBB18_78
.LBB18_69:                              # %if.then.i359
	movq	ssa_rename_to_pseudo(%rip), %rax
	leaq	-53(%rsi), %rcx
	andq	%rdi, %rcx
	leaq	(%rax,%rcx,8), %rax
.LBB18_71:                              # %ssa_rename_to_lookup.exit369
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB18_72
# BB#73:                                # %ssa_rename_to_lookup.exit369
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB18_74
# BB#75:                                # %if.then251
	movq	%rax, 8(%r15)
	movq	%rdi, %rbx
	jmp	.LBB18_78
.LBB18_14:
	movq	%r9, %rdx
	jmp	.LBB18_26
.LBB18_16:
	movq	%r9, %rdx
.LBB18_26:                              # %if.end61
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-63(%rcx), %rsi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rsi
	cmpq	$2, %rsi
	jb	.LBB18_29
# BB#27:                                # %if.end61
	cmpq	$61, %rcx
	je	.LBB18_36
# BB#28:                                # %if.end61
	addq	$-132, %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	jbe	.LBB18_29
	jmp	.LBB18_78
	.align	16, 0x90
.LBB18_32:                              # %while.body
                                        #   in Loop: Header=BB18_29 Depth=1
	movq	8(%rdx), %rdx
	movl	(%rdx), %eax
.LBB18_29:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$2, %rsi
	jb	.LBB18_32
# BB#30:                                # %while.cond
                                        #   in Loop: Header=BB18_29 Depth=1
	cmpq	$61, %rax
	je	.LBB18_33
# BB#31:                                # %while.cond
                                        #   in Loop: Header=BB18_29 Depth=1
	addq	$-132, %rax
	andq	%rcx, %rax
	cmpq	$1, %rax
	jbe	.LBB18_32
	jmp	.LBB18_78
.LBB18_33:                              # %land.lhs.true107
	movl	8(%rdx), %eax
	cmpq	$52, %rax
	ja	.LBB18_35
# BB#34:                                # %land.lhs.true107
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	cmpq	$16, %rax
	movabsq	$0, %rbx
	jne	.LBB18_78
.LBB18_35:                              # %if.then125
	movq	(%r14), %r15
	movq	$0, (%r14)
	movq	$47, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%rdx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, 8(%rsp)
	movabsq	$rename_insn_1, %rsi
	leaq	8(%rsp), %rdi
	movq	%r14, %rdx
	callq	for_each_rtx
	movq	%r14, %rdi
	callq	apply_delayed_renames
	movq	%r15, (%r14)
	jmp	.LBB18_78
.LBB18_36:                              # %land.lhs.true135
	movl	8(%rdx), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	$52, %rax
	ja	.LBB18_38
# BB#37:                                # %land.lhs.true135
	andq	%rcx, %rax
	cmpq	$16, %rax
	movabsq	$0, %rbx
	jne	.LBB18_78
.LBB18_38:                              # %if.then153
	movq	%rcx, %rbx
	movq	$48, %rdi
	callq	xmalloc
	movq	(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB18_41
# BB#39:                                # %lor.lhs.false.i
	movl	8(%rcx), %edx
	cmpq	$52, %rdx
	ja	.LBB18_43
# BB#40:                                # %lor.lhs.false.i
	andq	%rdx, %rbx
	cmpq	$16, %rbx
	jne	.LBB18_41
.LBB18_43:                              # %if.end.i
	movq	%r12, 8(%rax)
	movq	%rcx, 16(%rax)
	cmpq	$53, %rdx
	jae	.LBB18_44
# BB#45:                                # %if.else.i.i
	movzbl	2(%rcx), %ecx
	imulq	$472, %rdx, %rdx        # imm = 0x1D8
	leaq	ssa_rename_to_hard(%rdx,%rcx,8), %rcx
	jmp	.LBB18_46
.LBB18_10:
	movq	%rax, %rbx
	jmp	.LBB18_78
.LBB18_51:                              # %if.then.i350
	movq	ssa_rename_to_pseudo(%rip), %rax
	leaq	-53(%r9), %rcx
	andq	%r8, %rcx
	leaq	(%rax,%rcx,8), %rax
.LBB18_53:                              # %ssa_rename_to_lookup.exit
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB18_54
# BB#55:                                # %ssa_rename_to_lookup.exit
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB18_56
# BB#57:                                # %if.then197
	movl	(%rax), %ecx
	xorq	%rdx, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB18_58
# BB#59:                                # %if.end207
	movq	%rax, (%rdi)
	movq	%r8, %rbx
	jmp	.LBB18_78
.LBB18_72:
	movq	%rdi, %rbx
	jmp	.LBB18_78
.LBB18_74:
	movq	%rdi, %rbx
	jmp	.LBB18_78
.LBB18_54:
	movq	%r8, %rbx
	jmp	.LBB18_78
.LBB18_56:
	movq	%r8, %rbx
	jmp	.LBB18_78
.LBB18_44:                              # %if.then.i.i
	movq	ssa_rename_to_pseudo(%rip), %rcx
	leaq	-53(%rdx), %rdx
	andq	%r13, %rdx
	leaq	(%rcx,%rdx,8), %rcx
.LBB18_46:                              # %create_delayed_rename.exit
	movq	(%rcx), %rcx
	movq	%rcx, 32(%rax)
	movq	16(%r14), %rcx
	movq	%rcx, 40(%rax)
	movq	(%r14), %rcx
	movq	%rcx, (%rax)
	movq	%rax, (%r14)
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	jne	.LBB18_47
# BB#48:                                # %if.then158
	addq	$16, %r15
	movabsq	$rename_insn_1, %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	for_each_rtx
	movq	%r13, %rbx
	jmp	.LBB18_78
.LBB18_47:
	movq	%r13, %rbx
	jmp	.LBB18_78
.LBB18_41:                              # %if.then.i
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.create_delayed_rename, %rcx
	movq	$746, %rsi              # imm = 0x2EA
	jmp	.LBB18_42
.LBB18_58:                              # %if.then206
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.rename_insn_1, %rcx
	movq	$927, %rsi              # imm = 0x39F
.LBB18_42:                              # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp225:
	.size	rename_insn_1, .Ltmp225-rename_insn_1
	.cfi_endproc

	.align	16, 0x90
	.type	apply_delayed_renames,@function
apply_delayed_renames:                  # @apply_delayed_renames
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp232:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp233:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp234:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp235:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp236:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp237:
	.cfi_def_cfa_offset 64
.Ltmp238:
	.cfi_offset %rbx, -48
.Ltmp239:
	.cfi_offset %r12, -40
.Ltmp240:
	.cfi_offset %r13, -32
.Ltmp241:
	.cfi_offset %r14, -24
.Ltmp242:
	.cfi_offset %r15, -16
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB19_20
# BB#1:
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r13
	movq	16(%r13), %rdx
	movl	8(%rdx), %ecx
	cmpq	$53, %rcx
	jae	.LBB19_3
# BB#4:                                 # %if.else.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movzbl	2(%rdx), %eax
	imulq	$472, %rcx, %rsi        # imm = 0x1D8
	leaq	ssa_rename_to_hard(%rsi,%rax,8), %rax
	jmp	.LBB19_5
	.align	16, 0x90
.LBB19_3:                               # %if.then.i
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	ssa_rename_to_pseudo(%rip), %rax
	leaq	-53(%rcx), %rsi
	andq	%r12, %rsi
	leaq	(%rax,%rsi,8), %rax
.LBB19_5:                               # %ssa_rename_to_lookup.exit
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	(%rax), %rax
	movq	32(%r13), %rsi
	cmpq	%rsi, %rax
	jne	.LBB19_21
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB19_2 Depth=1
	testq	%rax, %rax
	jne	.LBB19_9
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB19_2 Depth=1
	cmpq	$53, %rcx
	jb	.LBB19_9
# BB#8:                                 # %if.then6
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%rdx, 24(%r13)
	movq	global_rtl(%rip), %rax
	movq	%rax, 32(%r13)
	movq	%rcx, %r15
	movq	%rdx, %rax
	jmp	.LBB19_10
	.align	16, 0x90
.LBB19_9:                               # %if.else
                                        #   in Loop: Header=BB19_2 Depth=1
	movzbl	2(%rdx), %edi
	callq	gen_reg_rtx
	movq	%rax, 24(%r13)
	movq	16(%r13), %rdx
	movl	8(%rax), %r15d
	movl	8(%rdx), %ecx
.LBB19_10:                              # %if.end12
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%rcx, %rsi
	andq	%r12, %rsi
	cmpq	$53, %rsi
	jae	.LBB19_11
# BB#12:                                # %if.else.i74
                                        #   in Loop: Header=BB19_2 Depth=1
	movzbl	2(%rdx), %ecx
	imulq	$472, %rsi, %rdx        # imm = 0x1D8
	movq	%rax, ssa_rename_to_hard(%rdx,%rcx,8)
	jmp	.LBB19_13
	.align	16, 0x90
.LBB19_11:                              # %if.then.i67
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	ssa_rename_to_pseudo(%rip), %rdx
	addq	$-53, %rcx
	andq	%r12, %rcx
	movq	%rax, (%rdx,%rcx,8)
.LBB19_13:                              # %ssa_rename_to_insert.exit
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	ssa_definition(%rip), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB19_15
# BB#14:                                # %if.then21
                                        #   in Loop: Header=BB19_2 Depth=1
	leaq	(%r15,%r15,4), %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	andq	$3, %rsi
	addq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$34, %rcx
	sarq	%cl, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ssa_definition(%rip)
.LBB19_15:                              # %if.end24
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	40(%r13), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rdx, 32(%rax,%rsi,8)
	movq	16(%r13), %r14
	movq	$16, %rdi
	callq	xmalloc
	movq	ssa_rename_from_ht(%rip), %rdi
	movq	%rax, %rbx
	movl	%r15d, (%rbx)
	movq	%r14, 8(%rbx)
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %r15
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB19_17
# BB#16:                                # %if.then.i60
                                        #   in Loop: Header=BB19_2 Depth=1
	callq	free
.LBB19_17:                              # %ssa_rename_from_insert.exit
                                        #   in Loop: Header=BB19_2 Depth=1
	movq	%rbx, (%r15)
	movq	(%r13), %rax
	testq	%rax, %rax
	jne	.LBB19_2
# BB#18:                                # %for.end
	testq	%r13, %r13
	movq	8(%rsp), %rcx           # 8-byte Reload
	je	.LBB19_20
# BB#19:                                # %if.then29
	movq	8(%rcx), %rax
	movq	%rax, (%r13)
	movq	(%rcx), %rax
	movq	%rax, 8(%rcx)
	movq	$0, (%rcx)
.LBB19_20:                              # %if.end34
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_21:                              # %if.then
	movabsq	$.L.str1, %rax
	movabsq	$.L__FUNCTION__.apply_delayed_renames, %rcx
	movq	$782, %rsi              # imm = 0x30E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp243:
	.size	apply_delayed_renames, .Ltmp243-apply_delayed_renames
	.cfi_endproc

	.align	16, 0x90
	.type	ssa_rename_from_hash_function,@function
ssa_rename_from_hash_function:          # @ssa_rename_from_hash_function
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	retq
.Ltmp244:
	.size	ssa_rename_from_hash_function, .Ltmp244-ssa_rename_from_hash_function
	.cfi_endproc

	.align	16, 0x90
	.type	ssa_rename_from_equal,@function
ssa_rename_from_equal:                  # @ssa_rename_from_equal
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	movl	(%rsi), %ecx
	cmpq	%rcx, %rax
	jne	.LBB21_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB21_1:
	movabsq	$0, %rax
	retq
.Ltmp245:
	.size	ssa_rename_from_equal, .Ltmp245-ssa_rename_from_equal
	.cfi_endproc

	.align	16, 0x90
	.type	ssa_rename_from_delete,@function
ssa_rename_from_delete:                 # @ssa_rename_from_delete
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	free  # TAILCALL
.Ltmp246:
	.size	ssa_rename_from_delete, .Ltmp246-ssa_rename_from_delete
	.cfi_endproc

	.align	16, 0x90
	.type	find_evaluations_1,@function
find_evaluations_1:                     # @find_evaluations_1
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB23_4
# BB#1:                                 # %land.lhs.true
	movl	8(%rdi), %eax
	cmpq	$52, %rax
	ja	.LBB23_3
# BB#2:                                 # %land.lhs.true
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	cmpq	$16, %rcx
	jne	.LBB23_4
.LBB23_3:                               # %if.then
	movq	fe_evals(%rip), %rdx
	movl	fe_current_bb(%rip), %ecx
	movq	(%rdx,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	shrq	$6, %rcx
	orq	%rdx, 16(%rax,%rcx,8)
.LBB23_4:                               # %if.end
	retq
.Ltmp247:
	.size	find_evaluations_1, .Ltmp247-find_evaluations_1
	.cfi_endproc

	.type	in_ssa_form,@object     # @in_ssa_form
	.bss
	.globl	in_ssa_form
	.align	4
in_ssa_form:
	.long	0                       # 0x0
	.size	in_ssa_form, 4

	.type	ssa_rename_from_ht,@object # @ssa_rename_from_ht
	.comm	ssa_rename_from_ht,8,8
	.type	.L.str1,@object         # @.str1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str1:
	.asciz	"ssa.c"
	.size	.L.str1, 6

	.type	.L__FUNCTION__.convert_to_ssa,@object # @__FUNCTION__.convert_to_ssa
.L__FUNCTION__.convert_to_ssa:
	.asciz	"convert_to_ssa"
	.size	.L__FUNCTION__.convert_to_ssa, 15

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	";; Immediate Dominators:\n"
	.size	.L.str2, 26

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	";\t%3d = %3d\n"
	.size	.L.str3, 13

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	";; Dominance Frontiers:"
	.size	.L.str4, 24

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"; Basic Block"
	.size	.L.str5, 14

	.type	ssa_max_reg_num,@object # @ssa_max_reg_num
	.local	ssa_max_reg_num
	.comm	ssa_max_reg_num,4,4
	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	";; Iterated Dominance Frontiers:"
	.size	.L.str6, 33

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"; Register"
	.size	.L.str7, 11

	.type	.L__FUNCTION__.convert_from_ssa,@object # @__FUNCTION__.convert_from_ssa
.L__FUNCTION__.convert_from_ssa:
	.asciz	"convert_from_ssa"
	.size	.L__FUNCTION__.convert_from_ssa, 17

	.type	ssa_definition,@object  # @ssa_definition
	.comm	ssa_definition,8,8
	.type	ssa_rename_from,@object # @ssa_rename_from
	.comm	ssa_rename_from,8,8
	.type	.L__FUNCTION__.eliminate_phi,@object # @__FUNCTION__.eliminate_phi
.L__FUNCTION__.eliminate_phi:
	.asciz	"eliminate_phi"
	.size	.L__FUNCTION__.eliminate_phi, 14

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Emitting copy on edge (%d,%d)\n"
	.size	.L.str8, 31

	.type	.L__FUNCTION__.rename_equivalent_regs,@object # @__FUNCTION__.rename_equivalent_regs
.L__FUNCTION__.rename_equivalent_regs:
	.asciz	"rename_equivalent_regs"
	.size	.L__FUNCTION__.rename_equivalent_regs, 23

	.type	.L__FUNCTION__.rename_equivalent_regs_in_insn,@object # @__FUNCTION__.rename_equivalent_regs_in_insn
.L__FUNCTION__.rename_equivalent_regs_in_insn:
	.asciz	"rename_equivalent_regs_in_insn"
	.size	.L__FUNCTION__.rename_equivalent_regs_in_insn, 31

	.type	.L__FUNCTION__.mark_reg_in_phi,@object # @__FUNCTION__.mark_reg_in_phi
.L__FUNCTION__.mark_reg_in_phi:
	.asciz	"mark_reg_in_phi"
	.size	.L__FUNCTION__.mark_reg_in_phi, 16

	.type	.L__FUNCTION__.make_regs_equivalent_over_bad_edges,@object # @__FUNCTION__.make_regs_equivalent_over_bad_edges
.L__FUNCTION__.make_regs_equivalent_over_bad_edges:
	.asciz	"make_regs_equivalent_over_bad_edges"
	.size	.L__FUNCTION__.make_regs_equivalent_over_bad_edges, 36

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"ssa_definition"
	.size	.L.str9, 15

	.type	ssa_rename_to_pseudo,@object # @ssa_rename_to_pseudo
	.local	ssa_rename_to_pseudo
	.comm	ssa_rename_to_pseudo,8,8
	.type	ssa_rename_to_hard,@object # @ssa_rename_to_hard
	.local	ssa_rename_to_hard
	.comm	ssa_rename_to_hard,25016,16
	.type	.L__FUNCTION__.rename_block,@object # @__FUNCTION__.rename_block
.L__FUNCTION__.rename_block:
	.asciz	"rename_block"
	.size	.L__FUNCTION__.rename_block, 13

	.type	.L__FUNCTION__.apply_delayed_renames,@object # @__FUNCTION__.apply_delayed_renames
.L__FUNCTION__.apply_delayed_renames:
	.asciz	"apply_delayed_renames"
	.size	.L__FUNCTION__.apply_delayed_renames, 22

	.type	.L__FUNCTION__.rename_insn_1,@object # @__FUNCTION__.rename_insn_1
.L__FUNCTION__.rename_insn_1:
	.asciz	"rename_insn_1"
	.size	.L__FUNCTION__.rename_insn_1, 14

	.type	.L__FUNCTION__.create_delayed_rename,@object # @__FUNCTION__.create_delayed_rename
.L__FUNCTION__.create_delayed_rename:
	.asciz	"create_delayed_rename"
	.size	.L__FUNCTION__.create_delayed_rename, 22

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Iterated dominance frontier: %d passes on %d regs.\n"
	.size	.L.str10, 52

	.type	fe_evals,@object        # @fe_evals
	.local	fe_evals
	.comm	fe_evals,8,8
	.type	fe_current_bb,@object   # @fe_current_bb
	.local	fe_current_bb
	.comm	fe_current_bb,4,4
	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"ssa_rename_from maps pseudo %i to original %i.\n"
	.size	.L.str11, 48

	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"ssa_rename_from's hash table contents:"
	.size	.Lstr, 39


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
