	.file	"convert.c.bc"
	.text
	.globl	convert_to_pointer
	.align	16, 0x90
	.type	convert_to_pointer,@function
convert_to_pointer:                     # @convert_to_pointer
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB0_11
# BB#1:
	movabsq	$.L.str, %r15
	movabsq	$4294967264, %r13       # imm = 0xFFFFFFE0
	.align	16, 0x90
.LBB0_2:                                # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$15, %rcx
	ja	.LBB0_12
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB0_4
# BB#7:                                 # %sw.bb7
                                        #   in Loop: Header=BB0_2 Depth=1
	movl	target_flags(%rip), %edi
	movl	60(%rax), %eax
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	andq	$511, %rax              # imm = 0x1FF
	movq	%rdi, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB0_8
# BB#9:                                 # %if.end16
                                        #   in Loop: Header=BB0_2 Depth=1
	xorq	%rsi, %rsi
	callq	type_for_size
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
	jmp	.LBB0_10
.LBB0_4:                                # %if.end
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$40960, %rax            # imm = 0xA000
	jne	.LBB0_5
	.align	16, 0x90
.LBB0_12:                               # %sw.default
                                        #   in Loop: Header=BB0_2 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	error
	movq	global_trees+88(%rip), %rbx
.LBB0_10:                               # %tailrecurse.backedge
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB0_2
.LBB0_11:                               # %return
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%r14, 8(%rax)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_8:                                # %if.then14
	movq	$114, %rdi
.LBB0_6:                                # %sw.bb
	movq	%r14, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB0_5:                                # %sw.bb
	movq	$115, %rdi
	jmp	.LBB0_6
.Ltmp16:
	.size	convert_to_pointer, .Ltmp16-convert_to_pointer
	.cfi_endproc

	.globl	convert_to_real
	.align	16, 0x90
	.type	convert_to_real,@function
convert_to_real:                        # @convert_to_real
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp24:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp26:
	.cfi_def_cfa_offset 48
.Ltmp27:
	.cfi_offset %rbx, -40
.Ltmp28:
	.cfi_offset %r12, -32
.Ltmp29:
	.cfi_offset %r14, -24
.Ltmp30:
	.cfi_offset %r15, -16
	movq	%rsi, %rax
	movq	%rdi, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$.L.str2, %r12
	movabsq	$.L.str1, %r15
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_9:                                # %tailrecurse
                                        #   in Loop: Header=BB1_1 Depth=1
	callq	error
	movq	global_trees+88(%rip), %rax
.LBB1_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movzbl	16(%rcx), %edx
	addq	$-6, %rdx
	andq	%rbx, %rdx
	cmpq	$9, %rdx
	ja	.LBB1_10
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB1_1 Depth=1
	jmpq	*.LJTI1_0(,%rdx,8)
.LBB1_8:                                # %sw.bb13
                                        #   in Loop: Header=BB1_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	jmp	.LBB1_9
	.align	16, 0x90
.LBB1_10:                               # %sw.default
                                        #   in Loop: Header=BB1_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	jmp	.LBB1_9
.LBB1_6:                                # %sw.bb3
	movq	$76, %rdi
.LBB1_5:                                # %select.end
	movq	%r14, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB1_3:                                # %sw.bb
	movl	flag_float_store(%rip), %ecx
	testq	%rcx, %rcx
	movq	$114, %rdi
	jne	.LBB1_5
# BB#4:                                 # %select.mid
	movq	$115, %rdi
	jmp	.LBB1_5
.LBB1_7:                                # %sw.bb5
	movq	8(%rcx), %rsi
	movq	$127, %rdi
	movq	%rax, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%r14, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.Ltmp31:
	.size	convert_to_real, .Ltmp31-convert_to_real
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_6
	.quad	.LBB1_3
	.quad	.LBB1_7
	.quad	.LBB1_10
	.quad	.LBB1_6
	.quad	.LBB1_6
	.quad	.LBB1_6
	.quad	.LBB1_8
	.quad	.LBB1_10
	.quad	.LBB1_8

	.text
	.globl	convert_to_integer
	.align	16, 0x90
	.type	convert_to_integer,@function
convert_to_integer:                     # @convert_to_integer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp38:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp39:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp40:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp41:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp42:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp43:
	.cfi_def_cfa_offset 80
.Ltmp44:
	.cfi_offset %rbx, -48
.Ltmp45:
	.cfi_offset %r12, -40
.Ltmp46:
	.cfi_offset %r13, -32
.Ltmp47:
	.cfi_offset %r14, -24
.Ltmp48:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	cmpq	$0, 32(%r14)
	movq	%rsi, %rbx
	je	.LBB2_4
# BB#1:
	movq	8(%rbx), %rcx
	leaq	8(%rbx), %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_2:                                # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rcx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-6, %rdx
	andq	%r13, %rdx
	cmpq	$9, %rdx
	ja	.LBB2_67
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB2_2 Depth=1
	movl	60(%rcx), %r12d
	movl	60(%r14), %edi
	jmpq	*.LJTI2_0(,%rdx,8)
.LBB2_6:                                # %sw.bb
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r13, %rax
	jne	.LBB2_7
# BB#8:                                 # %if.else
                                        #   in Loop: Header=BB2_2 Depth=1
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	xorq	%rsi, %rsi
	callq	type_for_size
	movq	$114, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rbx
	jmp	.LBB2_9
	.align	16, 0x90
.LBB2_7:                                # %if.then15
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	global_trees+88(%rip), %rbx
.LBB2_9:                                # %tailrecurse.backedge
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	8(%rbx), %rcx
	cmpq	$0, 32(%r14)
	leaq	8(%rbx), %r15
	jne	.LBB2_2
.LBB2_4:                                # %if.then
	movabsq	$.L.str3, %rdi
.LBB2_5:                                # %return
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rbx
	jmp	.LBB2_70
.LBB2_67:                               # %sw.default434
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees+88(%rip), %rsi
	movq	%r14, %rdi
.LBB2_64:                               # %sw.bb402
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB2_10:                               # %sw.bb22
	movzbl	16(%rbx), %r8d
	movzbl	tree_code_type(%r8), %ecx
	cmpq	$60, %rcx
	je	.LBB2_69
# BB#11:                                # %if.else28
	cmpq	$96, %r8
	je	.LBB2_68
# BB#12:                                # %if.else28
	leaq	-91(%r8), %rcx
	andq	%r13, %rcx
	cmpq	$4, %rcx
	ja	.LBB2_14
# BB#13:                                # %if.then42
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	callq	convert
	movq	%rax, 32(%rbx)
	movq	40(%rbx), %rsi
	movq	%r14, %rdi
	callq	convert
	movq	%rax, 40(%rbx)
	jmp	.LBB2_69
.LBB2_59:                               # %sw.bb400
	movq	$72, %rdi
	jmp	.LBB2_16
.LBB2_60:                               # %sw.bb402
	movq	8(%rcx), %rsi
	movq	$127, %rdi
	movq	%rbx, %rdx
.LBB2_61:                               # %sw.bb402
	callq	build1
.LBB2_62:                               # %sw.bb402
	movq	%rax, %rdi
	callq	fold
.LBB2_63:                               # %sw.bb402
	movq	%r14, %rdi
	movq	%rax, %rsi
	jmp	.LBB2_64
.LBB2_65:                               # %sw.bb410
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movzbl	mode_size(%rdi), %eax
	movq	$9, %rcx
	shrq	%cl, %r12
	andq	$127, %r12
	movzbl	mode_size(%r12), %ecx
	cmpq	%rcx, %rax
	je	.LBB2_15
# BB#66:                                # %if.then431
	movabsq	$.L.str4, %rdi
	jmp	.LBB2_5
.LBB2_68:                               # %if.then60
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	callq	convert
	movq	%rax, 32(%rbx)
.LBB2_69:                               # %return
	movq	%r14, (%r15)
.LBB2_70:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_14:                               # %if.else70
	andq	$511, %r12              # imm = 0x1FF
	movq	%rdi, %rdx
	andq	$511, %rdx              # imm = 0x1FF
	andq	%r13, %r12
	andq	%r13, %rdx
	cmpq	%r12, %rdx
	jae	.LBB2_15
# BB#18:                                # %if.else75
	movl	16(%r14), %esi
	movq	%rsi, %r9
	andq	$255, %r9
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	cmpq	$10, %r9
	je	.LBB2_20
# BB#19:                                # %lor.lhs.false82
	movq	%rdi, %rcx
	andq	%r13, %rcx
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB2_20
# BB#21:                                # %if.end108
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	cmpq	$114, %r8
	jg	.LBB2_56
# BB#22:                                # %if.end108
	cmpq	$76, %r8
	jg	.LBB2_37
# BB#23:                                # %if.end108
	leaq	-59(%r8), %rax
	andq	%r13, %rax
	cmpq	$2, %rax
	jb	.LBB2_29
# BB#24:                                # %if.end108
	cmpq	$51, %r8
	jne	.LBB2_25
# BB#58:                                # %sw.bb385
	movq	32(%rbx), %r15
	movq	40(%rbx), %rsi
	movq	%r14, %rdi
	callq	convert
	movq	48(%rbx), %rsi
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	convert
	movq	%rax, %rbx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	build
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB2_20:                               # %if.then91
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_mode
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	$115, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	jmp	.LBB2_17
.LBB2_56:                               # %if.end108
	cmpq	$115, %r8
	jne	.LBB2_15
	jmp	.LBB2_57
.LBB2_37:                               # %if.end108
	leaq	-77(%r8), %rcx
	andq	%r13, %rcx
	cmpq	$13, %rcx
	ja	.LBB2_15
# BB#38:                                # %if.end108
	jmpq	*.LJTI2_1(,%rcx,8)
.LBB2_51:                               # %if.end349
	cmpq	%r12, 24(%rsp)          # 8-byte Folded Reload
	jne	.LBB2_52
.LBB2_57:                               # %sw.bb379
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	get_unwidened
	jmp	.LBB2_63
.LBB2_25:                               # %if.end108
	cmpq	$61, %r8
	jne	.LBB2_15
.LBB2_26:                               # %sw.bb185
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	movq	%r8, 8(%rsp)            # 8-byte Spill
	callq	get_unwidened
	movq	40(%rbx), %rcx
	movq	%rax, %r13
	movq	%rcx, %rdi
	movq	%r14, %rsi
	callq	get_unwidened
	movq	8(%rsp), %r8            # 8-byte Reload
	movq	8(%r13), %rcx
	movl	60(%rcx), %edx
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, 24(%rsp)          # 8-byte Folded Reload
	jb	.LBB2_15
# BB#27:                                # %land.lhs.true202
	movq	8(%rax), %rax
	movl	60(%rax), %edx
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, 24(%rsp)          # 8-byte Folded Reload
	jb	.LBB2_15
# BB#28:                                # %land.lhs.true211
	movl	16(%rcx), %ecx
	movl	16(%rax), %eax
	xorq	%rcx, %rax
	testq	$8192, %rax             # imm = 0x2000
	jne	.LBB2_15
	jmp	.LBB2_29
.LBB2_41:                               # %sw.bb132
	movq	40(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB2_15
# BB#42:                                # %land.lhs.true142
	movq	%rax, %rdi
	movq	%r8, 8(%rsp)            # 8-byte Spill
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_15
# BB#43:                                # %land.lhs.true149
	movzbl	17(%r14), %eax
	testq	$32, %rax
	je	.LBB2_15
# BB#44:                                # %land.lhs.true156
	movq	32(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB2_15
# BB#45:                                # %if.then165
	movq	40(%rbx), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	movq	8(%rsp), %r8            # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB2_29
# BB#46:                                # %if.else174
	movq	global_trees+88(%rip), %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert_to_integer
	movzbl	17(%rbx), %edx
	movq	%rax, %rcx
	testq	$1, %rdx
	je	.LBB2_47
# BB#48:                                # %if.then181
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB2_39:                               # %sw.bb109
	movq	40(%rbx), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$25, %rax
	jne	.LBB2_15
# BB#40:                                # %land.lhs.true
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	global_trees+96(%rip), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r8, 8(%rsp)            # 8-byte Spill
	callq	convert
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	movq	8(%rsp), %r8            # 8-byte Reload
	testq	%r13, %rax
	je	.LBB2_15
.LBB2_29:                               # %trunc1
	movq	%r8, %r13
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	get_unwidened
	movq	40(%rbx), %rdi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r14, %rsi
	callq	get_unwidened
	movl	16(%r14), %esi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %rax
	andq	$255, %rax
	cmpq	$10, %rax
	movq	%r14, %rax
	jne	.LBB2_31
# BB#30:                                # %if.then272
	movl	60(%r14), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_size
.LBB2_31:                               # %if.end283
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%r12, %rcx
	jne	.LBB2_32
.LBB2_15:                               # %if.then73
	movq	$115, %rdi
.LBB2_16:                               # %if.then73
	movq	%r14, %rsi
	movq	%rbx, %rdx
.LBB2_17:                               # %if.then73
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB2_32:                               # %if.then290
	movq	%r13, %rbx
	movq	(%r15), %rcx
	movzbl	17(%rcx), %ecx
	testq	$32, %rcx
	jne	.LBB2_36
# BB#33:                                # %lor.lhs.false299
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	17(%rcx), %ecx
	testq	$32, %rcx
	je	.LBB2_35
# BB#34:                                # %land.lhs.true308
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	17(%rcx), %ecx
	testq	$32, %rcx
	jne	.LBB2_36
.LBB2_35:                               # %lor.lhs.false317
	cmpq	$82, %rbx
	jne	.LBB2_49
.LBB2_36:                               # %cond.true
	movq	%rax, %rdi
	callq	unsigned_type
	jmp	.LBB2_50
.LBB2_52:                               # %if.then356
	movq	%r8, %r12
	movq	%r14, %rdi
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB2_54
# BB#53:                                # %cond.true365
	callq	unsigned_type
	jmp	.LBB2_55
.LBB2_47:
	movq	%rcx, %rbx
	jmp	.LBB2_70
.LBB2_49:                               # %cond.false
	movq	%rax, %rdi
	callq	signed_type
.LBB2_50:                               # %cond.end
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %r15
	movq	%r12, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	build
	jmp	.LBB2_62
.LBB2_54:                               # %cond.false367
	callq	signed_type
.LBB2_55:                               # %cond.end369
	movq	%rax, %r15
	movq	32(%rbx), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	jmp	.LBB2_61
.Ltmp49:
	.size	convert_to_integer, .Ltmp49-convert_to_integer
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI2_0:
	.quad	.LBB2_10
	.quad	.LBB2_59
	.quad	.LBB2_60
	.quad	.LBB2_65
	.quad	.LBB2_10
	.quad	.LBB2_10
	.quad	.LBB2_10
	.quad	.LBB2_6
	.quad	.LBB2_67
	.quad	.LBB2_6
.LJTI2_1:
	.quad	.LBB2_51
	.quad	.LBB2_26
	.quad	.LBB2_26
	.quad	.LBB2_15
	.quad	.LBB2_15
	.quad	.LBB2_41
	.quad	.LBB2_39
	.quad	.LBB2_15
	.quad	.LBB2_15
	.quad	.LBB2_29
	.quad	.LBB2_29
	.quad	.LBB2_29
	.quad	.LBB2_29
	.quad	.LBB2_51

	.text
	.globl	convert_to_complex
	.align	16, 0x90
	.type	convert_to_complex,@function
convert_to_complex:                     # @convert_to_complex
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp58:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp59:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp60:
	.cfi_def_cfa_offset 48
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
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movabsq	$.L.str7, %r12
	movabsq	$.L.str6, %r13
	jmp	.LBB3_1
	.align	16, 0x90
.LBB3_11:                               # %tailrecurse
                                        #   in Loop: Header=BB3_1 Depth=1
	callq	error
	movq	global_trees+88(%rip), %rbx
.LBB3_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$15, %rcx
	ja	.LBB3_12
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	8(%r14), %r15
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7360, %rdx             # imm = 0x1CC0
	jne	.LBB3_14
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$40960, %rdx            # imm = 0xA000
	je	.LBB3_4
# BB#10:                                # %sw.bb46
                                        #   in Loop: Header=BB3_1 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	jmp	.LBB3_11
.LBB3_4:                                # %tailrecurse
                                        #   in Loop: Header=BB3_1 Depth=1
	cmpq	$8, %rcx
	je	.LBB3_5
	.align	16, 0x90
.LBB3_12:                               # %sw.default
                                        #   in Loop: Header=BB3_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	jmp	.LBB3_11
.LBB3_14:                               # %sw.bb
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	global_trees+88(%rip), %rsi
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$125, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB3_5:                                # %sw.bb7
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	128(%r15), %rcx
	cmpq	%rcx, %rax
	je	.LBB3_13
# BB#6:                                 # %if.else
	movzbl	16(%rbx), %eax
	cmpq	$125, %rax
	jne	.LBB3_9
# BB#7:                                 # %if.then20
	movq	32(%rbx), %rsi
	movq	%r15, %rdi
	callq	convert
	movq	40(%rbx), %rsi
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$125, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	jmp	.LBB3_8
.LBB3_13:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_9:                                # %if.else28
	movq	%rbx, %rdi
	callq	save_expr
	movq	%rax, %rbx
	movq	8(%rbx), %rax
	movq	8(%rax), %rsi
	movq	$127, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	8(%rbx), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %r12
	movq	$128, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$125, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
.LBB3_8:                                # %if.then20
	callq	build
	movq	%rax, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.Ltmp66:
	.size	convert_to_complex, .Ltmp66-convert_to_complex
	.cfi_endproc

	.globl	convert_to_vector
	.align	16, 0x90
	.type	convert_to_vector,@function
convert_to_vector:                      # @convert_to_vector
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp72:
	.cfi_def_cfa_offset 32
.Ltmp73:
	.cfi_offset %rbx, -24
.Ltmp74:
	.cfi_offset %r14, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movabsq	$.L.str8, %r14
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_5:                                # %sw.default
                                        #   in Loop: Header=BB4_1 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	error
	movq	global_trees+88(%rip), %rax
.LBB4_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$6, %rcx
	je	.LBB4_3
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$9, %rcx
	jne	.LBB4_5
.LBB4_3:                                # %sw.bb
	movl	60(%rbx), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	movl	60(%rdx), %edx
	andq	$127, %rsi
	movzbl	mode_size(%rsi), %esi
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB4_6
# BB#4:                                 # %if.end
	movq	$115, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build1  # TAILCALL
.LBB4_6:                                # %return
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp75:
	.size	convert_to_vector, .Ltmp75-convert_to_vector
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"cannot convert to a pointer type"
	.size	.L.str, 33

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"pointer value used where a floating point value was expected"
	.size	.L.str1, 61

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"aggregate value used where a float was expected"
	.size	.L.str2, 48

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"conversion to incomplete type"
	.size	.L.str3, 30

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"can't convert between vector values of different size"
	.size	.L.str4, 54

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"aggregate value used where an integer was expected"
	.size	.L.str5, 51

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"pointer value used where a complex was expected"
	.size	.L.str6, 48

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"aggregate value used where a complex was expected"
	.size	.L.str7, 50

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"can't convert value to a vector"
	.size	.L.str8, 32


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
