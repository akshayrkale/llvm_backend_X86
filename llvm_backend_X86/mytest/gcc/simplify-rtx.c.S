	.file	"simplify-rtx.c.bc"
	.text
	.globl	simplify_gen_binary
	.align	16, 0x90
	.type	simplify_gen_binary,@function
simplify_gen_binary:                    # @simplify_gen_binary
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
	movq	%rcx, %r14
	movq	%rdi, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	rtx_class(%rax), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r15
	cmpq	$99, %rax
	jne	.LBB0_1
# BB#2:                                 # %land.lhs.true
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rbx, %r12
	je	.LBB0_4
# BB#3:                                 # %land.lhs.true
	movq	%r14, %r12
.LBB0_4:                                # %land.lhs.true
	testq	%rcx, %rcx
	je	.LBB0_6
# BB#5:                                 # %land.lhs.true
	movq	%rbx, %r14
	jmp	.LBB0_6
.LBB0_1:
	movq	%rbx, %r12
.LBB0_6:                                # %if.end
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB0_9
# BB#7:                                 # %if.end5
	leaq	-75(%r13), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB0_10
# BB#8:                                 # %if.then10
	movq	$1, %r8
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	simplify_plus_minus
	testq	%rax, %rax
	je	.LBB0_10
.LBB0_9:                                # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_10:                               # %if.end15
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp16:
	.size	simplify_gen_binary, .Ltmp16-simplify_gen_binary
	.cfi_endproc

	.globl	simplify_binary_operation
	.align	16, 0x90
	.type	simplify_binary_operation,@function
simplify_binary_operation:              # @simplify_binary_operation
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
	subq	$176, %rsp
.Ltmp28:
	.cfi_def_cfa_offset 224
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%rdx, %rbx
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %r12
	movzbl	(%rbx), %eax
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	$66, %rax
	movq	%rbx, %r9
	jne	.LBB1_8
# BB#1:                                 # %if.end.i
	movq	8(%rbx), %r13
	movl	(%r13), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	%rbx, %r9
	jne	.LBB1_8
# BB#2:                                 # %if.end7.i
	movq	%rdi, %r15
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movq	%r13, %rdi
	callq	get_pool_constant
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	get_pool_mode
	movzbl	2(%rbx), %edi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rdi, %rcx
	jne	.LBB1_4
# BB#3:
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	jmp	.LBB1_7
.LBB1_4:                                # %if.then13.i
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	simplify_subreg
	movq	%rax, %r9
	testq	%r9, %r9
	jne	.LBB1_6
# BB#5:                                 # %if.then13.i
	movq	%rbx, %r9
.LBB1_6:                                # %if.then13.i
	movq	72(%rsp), %r8           # 8-byte Reload
.LBB1_7:                                # %avoid_constant_pool_reference.exit
	movq	%r15, %rdi
.LBB1_8:                                # %avoid_constant_pool_reference.exit
	movzbl	(%r8), %eax
	cmpq	$66, %rax
	movq	%r8, %rsi
	jne	.LBB1_15
# BB#9:                                 # %if.end.i2498
	movq	8(%r8), %r13
	movl	(%r13), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	%r8, %rsi
	jne	.LBB1_15
# BB#10:                                # %if.end7.i2505
	movq	%rdi, %r15
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movq	%r13, %rdi
	movq	%r8, %r14
	callq	get_pool_constant
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	get_pool_mode
	movq	%r14, %r8
	movzbl	2(%r8), %edi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rdi, %rcx
	jne	.LBB1_12
# BB#11:
	movq	72(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB1_14
.LBB1_12:                               # %if.then13.i2509
	xorq	%rcx, %rcx
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r8, %r14
	callq	simplify_subreg
	movq	%r14, %r8
	movq	%rax, %rsi
	testq	%rsi, %rsi
	jne	.LBB1_14
# BB#13:                                # %if.then13.i2509
	movq	%r8, %rsi
.LBB1_14:                               # %if.then13.i2509
	movq	48(%rsp), %r9           # 8-byte Reload
	movq	%r15, %rdi
.LBB1_15:                               # %avoid_constant_pool_reference.exit2511
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	%r8, %r13
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	rtx_class(%rax), %eax
	movzbl	mode_bitsize(%r12,%r12), %ecx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%r12, %r15
	cmpq	$99, %rax
	jne	.LBB1_16
# BB#18:                                # %land.lhs.true
	movq	%r9, %rdi
	movq	%r9, %r12
	movq	%rsi, %r14
	callq	swap_commutative_operands_p
	movq	%r14, %rsi
	movq	%r12, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rdx, %rax
	je	.LBB1_20
# BB#19:                                # %land.lhs.true
	movq	%rsi, %rax
.LBB1_20:                               # %land.lhs.true
	movq	%rbx, %rdi
	je	.LBB1_22
# BB#21:                                # %land.lhs.true
	movq	%rdx, %rsi
.LBB1_22:                               # %land.lhs.true
	movq	%r13, %rdx
	je	.LBB1_24
# BB#23:                                # %land.lhs.true
	movq	%rdi, %rdx
.LBB1_24:                               # %land.lhs.true
	testq	%rcx, %rcx
	movq	%r15, %r12
	je	.LBB1_26
# BB#25:                                # %land.lhs.true
	movq	%r13, %rdi
.LBB1_26:                               # %land.lhs.true
	movq	%rax, %r9
	movq	%rdx, %rbx
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	80(%rsp), %r11          # 8-byte Reload
.LBB1_27:                               # %if.end13
	movl	mode_class(,%r12,4), %r13d
	cmpq	$2, %r13
	jne	.LBB1_34
# BB#28:                                # %land.lhs.true18
	movzbl	(%r9), %eax
	cmpq	$55, %rax
	jne	.LBB1_77
# BB#29:                                # %land.lhs.true21
	movzbl	(%rsi), %eax
	cmpq	$55, %rax
	jne	.LBB1_34
# BB#30:                                # %land.lhs.true26
	movzbl	2(%rdi), %ecx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	%r14, %rax
	cmpq	%rax, %rcx
	jne	.LBB1_34
# BB#31:                                # %land.lhs.true31
	movzbl	2(%rbx), %ecx
	cmpq	%rax, %rcx
	jne	.LBB1_34
# BB#32:                                # %if.then37
	movq	%r9, 144(%rsp)
	movq	%rsi, 152(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	%eax, 172(%rsp)
	movl	%r11d, 168(%rsp)
	movabsq	$simplify_binary_real, %rdi
	leaq	144(%rsp), %rsi
	callq	do_float_handler
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	je	.LBB1_481
# BB#33:                                # %if.then44
	movq	160(%rsp), %rax
	jmp	.LBB1_481
.LBB1_34:                               # %if.end46
	cmpq	$1, %r13
	jne	.LBB1_77
# BB#35:                                # %if.end46
	cmpq	$128, %r15
	jne	.LBB1_77
# BB#36:                                # %land.lhs.true54
	movl	(%r9), %ecx
	movq	%rcx, %rax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$54, %rax
	jne	.LBB1_77
# BB#37:                                # %land.lhs.true63
	movl	(%rsi), %eax
	movq	%rax, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$54, %rdx
	jne	.LBB1_77
# BB#38:                                # %if.then73
	movq	%rsi, %rdx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$55, %rcx
	jne	.LBB1_40
# BB#39:                                # %if.then78
	movq	16(%r9), %rbx
	movq	24(%r9), %r14
	jmp	.LBB1_41
.LBB1_77:                               # %if.end212
	movl	(%rdi), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB1_81
# BB#78:                                # %lor.lhs.false217
	movzbl	(%rbx), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_81
# BB#79:                                # %lor.lhs.false217
	cmpq	$64, %r15
	ja	.LBB1_81
# BB#80:                                # %lor.lhs.false217
	testq	%r15, %r15
	je	.LBB1_81
# BB#424:                               # %if.end1668
	movq	8(%r9), %rdi
	movq	8(%rsi), %r8
	cmpq	$63, %r15
	ja	.LBB1_425
# BB#426:                               # %if.then1677
	movq	$1, %rax
	movq	$1, %rdx
	movq	%r15, %rcx
	shlq	%cl, %rdx
	decq	%rdx
	andq	%rdx, %rdi
	andq	%rdx, %r8
	movq	%r15, %rcx
	decq	%rcx
	shlq	%cl, %rax
	testq	%rax, %rdi
	movq	%rdi, %rdx
	je	.LBB1_428
# BB#427:                               # %if.then1691
	movq	$-1, %rdx
	movq	%r15, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
.LBB1_428:                              # %if.end1695
	testq	%rax, %r8
	je	.LBB1_429
# BB#430:                               # %if.then1701
	movq	$-1, %rbx
	movq	%r15, %rcx
	shlq	%cl, %rbx
	orq	%r8, %rbx
	jmp	.LBB1_431
.LBB1_81:                               # %if.then228
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$145, %rdx
	jg	.LBB1_422
# BB#82:                                # %if.then228
	movq	%rsi, %r14
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	addq	$-74, %r11
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %r11
	cmpq	$21, %r11
	ja	.LBB1_423
# BB#83:                                # %if.then228
	jmpq	*.LJTI1_1(,%r11,8)
.LBB1_400:                              # %sw.bb1542
	movzbl	(%r9), %eax
	cmpq	$54, %rax
	jne	.LBB1_405
# BB#401:                               # %sw.bb1542
	cmpq	$64, %r15
	ja	.LBB1_405
# BB#402:                               # %land.lhs.true1550
	movq	8(%r9), %rax
	movq	mode_mask_array(,%r12,8), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_405
# BB#403:                               # %land.lhs.true1558
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %r15
	callq	side_effects_p
	movq	%r15, %r9
	testq	%rbx, %rax
	je	.LBB1_404
.LBB1_405:                              # %sw.bb1563
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB1_406
# BB#407:                               # %if.end1567
	cmpq	%rax, %r9
	jne	.LBB1_415
# BB#408:                               # %land.lhs.true1570
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	side_effects_p
	testq	%rbx, %rax
	movq	56(%rsp), %rax          # 8-byte Reload
	jne	.LBB1_415
	jmp	.LBB1_481
.LBB1_422:                              # %if.then228
	addq	$-146, %r11
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r11, %rax
	cmpq	$4, %rax
	movabsq	$0, %rax
	jb	.LBB1_481
.LBB1_423:                              # %sw.default1666
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.simplify_binary_operation, %rdx
	movq	$1540, %rsi             # imm = 0x604
	callq	fancy_abort
.LBB1_425:
	movq	%rdi, %rdx
	movq	%r8, %rbx
	jmp	.LBB1_431
.LBB1_40:                               # %if.else
	movq	8(%r9), %rbx
	movq	$63, %rcx
	movq	%rbx, %r14
	sarq	%cl, %r14
.LBB1_41:                               # %if.end88
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$55, %rax
	jne	.LBB1_43
# BB#42:                                # %if.then93
	movq	%rdx, %rax
	movq	16(%rax), %rdx
	movq	24(%rax), %r10
	jmp	.LBB1_44
.LBB1_43:                               # %if.else100
	movq	8(%rdx), %rdx
	movq	$63, %rcx
	movq	%rdx, %r10
	sarq	%cl, %r10
.LBB1_44:                               # %if.end107
	xorq	%rax, %rax
	leaq	-75(%r11), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$20, %rdi
	ja	.LBB1_481
# BB#45:                                # %if.end107
	jmpq	*.LJTI1_2(,%rdi,8)
.LBB1_64:                               # %sw.bb168
	testq	%r10, %r10
	jne	.LBB1_481
# BB#65:                                # %sw.bb168
	cmpq	$127, %rdx
	ja	.LBB1_481
# BB#66:                                # %if.end178
	leaq	-89(%r11), %rax
	andq	%rcx, %rax
	andq	%rcx, %r11
	cmpq	$1, %rax
	ja	.LBB1_71
# BB#67:                                # %if.then184
	cmpq	$89, %r11
	jne	.LBB1_68
# BB#69:                                # %if.then184
	movabsq	$1, %rax
	jmp	.LBB1_70
.LBB1_429:
	movq	%r8, %rbx
.LBB1_431:                              # %if.end1707
	addq	$-74, %r11
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r11, %rcx
	cmpq	$21, %rcx
	ja	.LBB1_478
# BB#432:                               # %if.end1707
	movabsq	$-9223372036854775808, %r9 # imm = 0x8000000000000000
	xorq	%rax, %rax
	jmpq	*.LJTI1_0(,%rcx,8)
.LBB1_433:                              # %sw.bb1708
	addq	%rdx, %rbx
	movq	%rbx, %rdi
	jmp	.LBB1_480
.LBB1_406:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_177:                              # %sw.bb671
	cmpq	$109, %rax
	jne	.LBB1_178
# BB#180:                               # %land.lhs.true686
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$111, %rax
	jne	.LBB1_415
	jmp	.LBB1_181
.LBB1_84:                               # %sw.bb229
	movq	%r13, %rcx
	orq	$4, %rcx
	cmpq	$6, %rcx
	jne	.LBB1_85
# BB#87:                                # %land.lhs.true244
	movq	%r13, %rdx
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_88
	jmp	.LBB1_415
.LBB1_188:                              # %sw.bb755
	movq	%r13, %r15
	orq	$4, %r15
	cmpq	$6, %r15
	jne	.LBB1_189
# BB#191:                               # %land.lhs.true770
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_192
	jmp	.LBB1_415
.LBB1_293:                              # %sw.bb1132
	movq	const_int_rtx+504(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB1_294
# BB#297:                               # %if.end1143
	movq	%r13, %rax
	orq	$4, %rax
	cmpq	$6, %rax
	jne	.LBB1_298
# BB#300:                               # %lor.lhs.false1158
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_301
	jmp	.LBB1_303
.LBB1_369:                              # %sw.bb1406
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_372
# BB#370:                               # %land.lhs.true1411
	movq	8(%r14), %rdi
	movq	%r9, %r15
	callq	exact_log2_wide
	movq	%r15, %r9
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB1_372
# BB#371:                               # %if.then1419
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	jmp	.LBB1_314
.LBB1_372:                              # %sw.bb1423
	movq	const_tiny_rtx+472(,%r12,8), %rax
	cmpq	%rax, %r14
	je	.LBB1_373
# BB#379:                               # %if.end1447
	movq	%r13, %rax
	orq	$4, %rax
	cmpq	$6, %rax
	jne	.LBB1_380
# BB#388:                               # %lor.lhs.false1462
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	jne	.LBB1_389
	jmp	.LBB1_382
.LBB1_392:                              # %sw.bb1508
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_395
# BB#393:                               # %land.lhs.true1513
	movq	8(%r14), %rdi
	movq	%r9, %r15
	callq	exact_log2_wide
	movq	%r15, %r9
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_395
# BB#394:                               # %if.then1520
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$83, %rdi
.LBB1_314:                              # %return
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB1_315:                              # %return
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB1_316:                              # %return
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	jmp	.LBB1_481
.LBB1_395:                              # %sw.bb1528
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_397
# BB#396:                               # %sw.bb1528
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, %r14
	jne	.LBB1_415
.LBB1_397:                              # %land.lhs.true1534
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	side_effects_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	jne	.LBB1_415
# BB#398:                               # %land.lhs.true1537
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rbx
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB1_415
# BB#399:                               # %if.then1540
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB1_481
.LBB1_352:                              # %sw.bb1338
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r14
	jne	.LBB1_355
# BB#353:                               # %land.lhs.true1341
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %r15
	callq	side_effects_p
	movq	%r15, %r9
	testq	%rbx, %rax
	je	.LBB1_354
.LBB1_355:                              # %if.end1345
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_358
# BB#356:                               # %land.lhs.true1350
	movq	8(%r14), %rax
	movq	mode_mask_array(,%r12,8), %rcx
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB1_358
# BB#357:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_328:                              # %sw.bb1243
	movq	const_int_rtx+512(%rip), %rax
	movq	%r14, %rsi
	cmpq	%rax, %rsi
	je	.LBB1_329
# BB#330:                               # %if.end1247
	movzbl	(%rsi), %eax
	cmpq	$54, %rax
	jne	.LBB1_333
# BB#331:                               # %land.lhs.true1252
	movq	8(%rsi), %rax
	movq	mode_mask_array(,%r12,8), %rcx
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB1_333
# BB#332:
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_343:                              # %sw.bb1303
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB1_344
# BB#345:                               # %if.end1307
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_348
# BB#346:                               # %land.lhs.true1312
	movq	8(%r14), %rax
	movq	mode_mask_array(,%r12,8), %rcx
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB1_348
# BB#347:                               # %if.then1323
	movq	$86, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB1_296
.LBB1_409:                              # %sw.bb1575
	cmpq	$64, %r15
	ja	.LBB1_413
# BB#410:                               # %land.lhs.true1578
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_413
# BB#411:                               # %land.lhs.true1583
	movq	8(%r14), %rax
	decq	%r15
	movq	$1, %rdx
	movq	%r15, %rcx
	shlq	%cl, %rdx
	cmpq	%rdx, %rax
	jne	.LBB1_413
	jmp	.LBB1_412
.LBB1_416:                              # %sw.bb1604
	cmpq	$64, %r15
	ja	.LBB1_413
# BB#417:                               # %land.lhs.true1607
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_413
# BB#418:                               # %land.lhs.true1612
	movq	mode_mask_array(,%r12,8), %rax
	movq	8(%r14), %rdx
	movq	$1, %rcx
	shrq	%cl, %rax
	cmpq	%rax, %rdx
	je	.LBB1_412
	jmp	.LBB1_413
.LBB1_419:                              # %sw.bb1633
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB1_420
.LBB1_421:                              # %sw.bb1649
	movq	const_int_rtx+504(%rip), %rax
.LBB1_420:                              # %sw.bb1633
	cmpq	%rax, %r14
	jne	.LBB1_413
.LBB1_412:                              # %land.lhs.true1591
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %r15
	callq	side_effects_p
	movq	%r15, %r9
	testq	%rbx, %rax
	movq	72(%rsp), %rax          # 8-byte Reload
	je	.LBB1_481
.LBB1_413:                              # %if.else1595
	movq	%r9, %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB1_415
# BB#414:                               # %land.lhs.true1598
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rcx, %r14
	callq	side_effects_p
	testq	%r14, %rax
	movq	%rbx, %rax
	jne	.LBB1_415
	jmp	.LBB1_481
.LBB1_478:                              # %sw.default1846
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.simplify_binary_operation, %rdx
	movq	$1716, %rsi             # imm = 0x6B4
	callq	fancy_abort
.LBB1_435:                              # %sw.bb1710
	subq	%rbx, %rdx
	movq	%rdx, %rdi
	jmp	.LBB1_480
.LBB1_436:                              # %sw.bb1712
	imulq	%rdx, %rbx
	movq	%rbx, %rdi
	jmp	.LBB1_480
.LBB1_437:                              # %sw.bb1713
	testq	%rbx, %rbx
	je	.LBB1_481
# BB#438:                               # %lor.lhs.false1716
	cmpq	%r9, %rdx
	jne	.LBB1_440
# BB#439:                               # %lor.lhs.false1716
	cmpq	$-1, %rbx
	je	.LBB1_481
.LBB1_440:                              # %if.end1723
	movq	%rdx, %rax
	cqto
	idivq	%rbx
	movq	%rax, %rdi
	jmp	.LBB1_480
.LBB1_442:                              # %sw.bb1724
	testq	%rbx, %rbx
	je	.LBB1_481
# BB#443:                               # %lor.lhs.false1727
	cmpq	%r9, %rdx
	jne	.LBB1_445
# BB#444:                               # %lor.lhs.false1727
	cmpq	$-1, %rbx
	je	.LBB1_481
.LBB1_445:                              # %if.end1734
	movq	%rdx, %rax
	cqto
	idivq	%rbx
	movq	%rdx, %rdi
	jmp	.LBB1_480
.LBB1_446:                              # %sw.bb1735
	testq	%r8, %r8
	je	.LBB1_481
# BB#447:                               # %lor.lhs.false1738
	cmpq	%r9, %rdx
	jne	.LBB1_449
# BB#448:                               # %lor.lhs.false1738
	cmpq	$-1, %rbx
	je	.LBB1_481
.LBB1_449:                              # %if.end1745
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	%r8
	movq	%rax, %rdi
	jmp	.LBB1_480
.LBB1_450:                              # %sw.bb1747
	testq	%r8, %r8
	je	.LBB1_481
# BB#451:                               # %lor.lhs.false1750
	cmpq	%r9, %rdx
	jne	.LBB1_453
# BB#452:                               # %lor.lhs.false1750
	cmpq	$-1, %rbx
	je	.LBB1_481
.LBB1_453:                              # %if.end1757
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	%r8
	movq	%rdx, %rdi
	jmp	.LBB1_480
.LBB1_479:                              # %sw.bb1759
	andq	%r8, %rdi
	jmp	.LBB1_480
.LBB1_454:                              # %sw.bb1761
	orq	%r8, %rdi
	jmp	.LBB1_480
.LBB1_455:                              # %sw.bb1763
	xorq	%r8, %rdi
	jmp	.LBB1_480
.LBB1_458:                              # %sw.bb1771
	testq	%r8, %r8
	js	.LBB1_481
# BB#459:                               # %if.end1775
	movq	%r8, %rcx
	shlq	%cl, %rdi
	jmp	.LBB1_480
.LBB1_468:                              # %sw.bb1805
	testq	%r8, %r8
	js	.LBB1_481
# BB#469:                               # %if.end1809
	movq	%r8, %rax
	cqto
	idivq	%r15
	movq	%rdi, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	subq	%rdx, %r15
	movq	%r15, %rcx
	jmp	.LBB1_466
.LBB1_460:                              # %sw.bb1777
	testq	%r8, %r8
	js	.LBB1_481
# BB#461:                               # %if.end1781
	movq	%rdx, %rdi
	movq	%r8, %rcx
	sarq	%cl, %rdi
	testq	%rdx, %rdx
	jns	.LBB1_480
# BB#462:                               # %if.end1781
	testq	%r8, %r8
	jle	.LBB1_480
# BB#463:                               # %if.then1788
	movq	$64, %rcx
	subq	%r8, %rcx
	movq	$-1, %rax
	shlq	%cl, %rax
	orq	%rax, %rdi
	jmp	.LBB1_480
.LBB1_456:                              # %sw.bb1765
	testq	%r8, %r8
	js	.LBB1_481
# BB#457:                               # %if.end1769
	movq	%r8, %rcx
	shrq	%cl, %rdi
	jmp	.LBB1_480
.LBB1_464:                              # %sw.bb1793
	testq	%r8, %r8
	js	.LBB1_481
# BB#465:                               # %if.end1797
	movq	%r8, %rax
	cqto
	idivq	%r15
	subq	%rdx, %r15
	movq	%rdi, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	movq	%rdx, %rcx
.LBB1_466:                              # %sw.epilog1847
	shrq	%cl, %rdi
	orq	%rax, %rdi
	jmp	.LBB1_480
.LBB1_470:                              # %sw.bb1818
	cmpq	%rbx, %rdx
	jle	.LBB1_472
	jmp	.LBB1_471
.LBB1_475:                              # %sw.bb1832
	cmpq	%rbx, %rdx
	jge	.LBB1_472
.LBB1_471:                              # %sw.bb1818
	movq	%rbx, %rdx
.LBB1_472:                              # %sw.bb1818
	movq	%rdx, %rdi
	jmp	.LBB1_480
.LBB1_473:                              # %sw.bb1825
	cmpq	%r8, %rdi
	jbe	.LBB1_480
# BB#474:                               # %sw.bb1825
	movq	%r8, %rdi
	jmp	.LBB1_480
.LBB1_476:                              # %sw.bb1839
	cmpq	%r8, %rdi
	ja	.LBB1_480
# BB#477:                               # %sw.bb1839
	movq	%r8, %rdi
.LBB1_480:                              # %sw.epilog1847
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	jmp	.LBB1_481
.LBB1_358:                              # %if.end1362
	cmpq	%r14, %r9
	movq	56(%rsp), %rdi          # 8-byte Reload
	jne	.LBB1_362
# BB#359:                               # %land.lhs.true1365
	movq	%rdi, %r14
	callq	side_effects_p
	movq	%r14, %rdi
	testq	%rbx, %rax
	jne	.LBB1_362
# BB#360:                               # %land.lhs.true1365
	cmpq	$4, %r13
	je	.LBB1_362
# BB#361:
	movq	%rdi, %rax
	jmp	.LBB1_481
.LBB1_373:                              # %if.then1428
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	gen_lowpart_common
	movq	%rbx, %rcx
	testq	%rax, %rax
	jne	.LBB1_481
# BB#374:                               # %if.else1432
	movq	56(%rsp), %rsi          # 8-byte Reload
	movzbl	2(%rsi), %eax
	movq	64(%rsp), %rdi          # 8-byte Reload
	andq	%rdi, %rcx
	cmpq	%rcx, %rax
	je	.LBB1_375
# BB#376:                               # %if.else1432
	testq	%rax, %rax
	je	.LBB1_377
# BB#378:                               # %if.then1444
	callq	gen_lowpart_SUBREG
	jmp	.LBB1_481
.LBB1_362:                              # %if.end1374
	movzbl	(%rdi), %eax
	movq	%rdi, %r14
	cmpq	$86, %rax
	jne	.LBB1_364
# BB#363:                               # %land.lhs.true1379
	movq	8(%r14), %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%rbx, %rax
	jne	.LBB1_366
.LBB1_364:                              # %lor.lhs.false1385
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$86, %rax
	jne	.LBB1_415
# BB#365:                               # %land.lhs.true1390
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB1_415
.LBB1_366:                              # %land.lhs.true1396
	movq	%r14, %rdi
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB1_415
# BB#367:                               # %land.lhs.true1396
	cmpq	$4, %r13
	je	.LBB1_415
# BB#368:                               # %if.then1404
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB1_481
.LBB1_178:                              # %sw.bb671
	cmpq	$105, %rax
	jne	.LBB1_415
# BB#179:                               # %land.lhs.true676
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$107, %rax
	jne	.LBB1_415
.LBB1_181:                              # %land.lhs.true691
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_415
# BB#182:                               # %land.lhs.true697
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB1_415
# BB#183:                               # %if.then703
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB1_415
# BB#184:                               # %land.lhs.true714
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rsi
	movl	(%rsi), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB1_415
# BB#185:                               # %land.lhs.true719
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rdx
	jne	.LBB1_415
# BB#186:                               # %land.lhs.true728
	movl	8(%rax), %ecx
	movl	8(%rsi), %esi
	cmpq	%rsi, %rcx
	jne	.LBB1_415
# BB#187:                               # %land.lhs.true736
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$4, %rcx
	jne	.LBB1_415
	jmp	.LBB1_481
.LBB1_333:                              # %if.end1264
	movq	%r9, %rdi
	callq	rtx_equal_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	movq	56(%rsp), %rbx          # 8-byte Reload
	je	.LBB1_336
# BB#334:                               # %land.lhs.true1267
	movq	%rbx, %rdi
	movq	%rcx, %r14
	callq	side_effects_p
	movq	%r14, %rcx
	testq	%rcx, %rax
	je	.LBB1_335
.LBB1_336:                              # %if.end1271
	movzbl	(%rbx), %eax
	cmpq	$86, %rax
	jne	.LBB1_338
# BB#337:                               # %land.lhs.true1276
	movq	8(%rbx), %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %r14
	callq	rtx_equal_p
	movq	%r14, %rcx
	testq	%rcx, %rax
	jne	.LBB1_340
.LBB1_338:                              # %lor.lhs.false1282
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$86, %rax
	jne	.LBB1_415
# BB#339:                               # %land.lhs.true1287
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	movq	%rcx, %r14
	callq	rtx_equal_p
	movq	%r14, %rcx
	testq	%rcx, %rax
	je	.LBB1_415
.LBB1_340:                              # %land.lhs.true1293
	movq	%rbx, %rdi
	movq	%rcx, %rbx
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB1_415
# BB#341:                               # %land.lhs.true1293
	cmpq	$4, %r13
	je	.LBB1_415
# BB#342:                               # %if.then1301
	movq	const_int_rtx+504(%rip), %rax
	jmp	.LBB1_481
.LBB1_348:                              # %if.end1325
	cmpq	%r14, %r9
	jne	.LBB1_415
# BB#349:                               # %land.lhs.true1328
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB1_415
# BB#350:                               # %land.lhs.true1328
	cmpq	$4, %r13
	je	.LBB1_415
# BB#351:                               # %if.then1336
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB1_481
.LBB1_294:                              # %if.then1135
	movq	$77, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rsi, %rcx
	movq	%rsi, %rbx
	callq	simplify_unary_operation
	testq	%rax, %rax
	jne	.LBB1_481
# BB#295:                               # %cond.false1139
	movq	$77, %rdi
	movq	%rbx, %rsi
.LBB1_296:                              # %return
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	gen_rtx_fmt_e
	jmp	.LBB1_481
.LBB1_329:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_344:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_380:                              # %lor.lhs.false1457
	cmpq	$8, %r13
	movq	72(%rsp), %rdi          # 8-byte Reload
	jne	.LBB1_389
# BB#381:                               # %lor.lhs.false1457
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rbx, %rax
	je	.LBB1_382
.LBB1_389:                              # %land.lhs.true1464
	movq	const_tiny_rtx(,%r12,8), %rax
	cmpq	%rax, %r9
	jne	.LBB1_382
# BB#390:                               # %land.lhs.true1469
	callq	side_effects_p
	testq	%rbx, %rax
	je	.LBB1_391
.LBB1_382:                              # %if.else1473
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$55, %rcx
	jne	.LBB1_415
# BB#383:                               # %land.lhs.true1478
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB1_415
# BB#384:                               # %land.lhs.true1486
	movq	const_tiny_rtx(,%r12,8), %rax
	cmpq	%rax, %r14
	je	.LBB1_415
# BB#385:                               # %land.lhs.true1486
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rbx, %rax
	je	.LBB1_415
# BB#386:                               # %do.body
	movq	16(%r14), %rax
	movq	24(%r14), %rcx
	movq	32(%r14), %rdx
	movq	%rdx, 104(%rsp)
	movq	%rcx, 96(%rsp)
	movq	%rax, 88(%rsp)
	movq	dconst0+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst0+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst0(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	104(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	88(%rsp), %rax
	movq	96(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%rbx, %rax
	je	.LBB1_415
# BB#387:                               # %if.then1501
	movq	$79, %rdi
	callq	rtx_to_tree_code
	movabsq	$dconst1, %rdx
	leaq	88(%rsp), %rdi
	movq	%rax, %rsi
	movq	%rdi, %rcx
	callq	earith
	movq	104(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	88(%rsp), %rax
	movq	96(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	immed_real_const_1
	movq	$78, %rdi
	movq	%rbx, %rsi
	jmp	.LBB1_315
.LBB1_85:                               # %lor.lhs.false239
	cmpq	$8, %r13
	movq	%r13, %rdx
	jne	.LBB1_88
# BB#86:                                # %lor.lhs.false239
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rbx, %rcx
	je	.LBB1_415
.LBB1_88:                               # %if.end247
	movq	const_tiny_rtx(,%r12,8), %rcx
	cmpq	%rcx, %r14
	je	.LBB1_89
# BB#90:                                # %if.end253
	cmpq	$77, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	jne	.LBB1_102
# BB#91:                                # %if.then258
	movzbl	rtx_class+76(%rip), %eax
	movq	8(%rdi), %r15
	cmpq	$99, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	je	.LBB1_93
	jmp	.LBB1_92
.LBB1_189:                              # %lor.lhs.false765
	cmpq	$8, %r13
	jne	.LBB1_192
# BB#190:                               # %lor.lhs.false765
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rbx, %rax
	je	.LBB1_415
.LBB1_192:                              # %if.end773
	movq	%r9, %rdi
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movq	%r14, %rsi
	callq	rtx_equal_p
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB1_193
# BB#194:                               # %land.lhs.true776
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %rbx
	callq	side_effects_p
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB1_195
.LBB1_193:
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
.LBB1_198:                              # %if.end799
	movq	const_tiny_rtx(,%r12,8), %rax
	cmpq	%rax, %rdx
	je	.LBB1_199
# BB#203:                               # %if.end806
	movq	const_int_rtx+504(%rip), %rcx
	cmpq	%rcx, %rdx
	je	.LBB1_204
# BB#205:                               # %if.end811
	cmpq	%rax, %r14
	je	.LBB1_206
# BB#207:                               # %if.end817
	cmpq	$8, %rsi
	movq	56(%rsp), %rbx          # 8-byte Reload
	ja	.LBB1_209
# BB#208:                               # %if.end817
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	$324, %rax              # imm = 0x144
	jne	.LBB1_247
.LBB1_209:                              # %if.then832
	movzbl	(%rbx), %eax
	xorq	%rdx, %rdx
	cmpq	$87, %rax
	movq	$1, %r15
	je	.LBB1_216
# BB#210:                               # %if.then832
	cmpq	$78, %rax
	jne	.LBB1_211
# BB#214:                               # %land.lhs.true851
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movq	%rbx, %rdi
	jne	.LBB1_221
# BB#215:                               # %if.then859
	movq	8(%rax), %r15
	movq	8(%rbx), %rdi
	movq	$1, %rdx
	jmp	.LBB1_221
.LBB1_71:                               # %if.else190
	leaq	136(%rsp), %r8
	leaq	128(%rsp), %r9
	cmpq	$88, %r11
	jne	.LBB1_72
# BB#74:                                # %if.then200
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rcx
	callq	lrotate_double
	jmp	.LBB1_76
.LBB1_89:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_298:                              # %lor.lhs.false1153
	cmpq	$8, %r13
	jne	.LBB1_301
# BB#299:                               # %lor.lhs.false1153
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rbx, %rax
	je	.LBB1_303
.LBB1_301:                              # %land.lhs.true1160
	movq	const_tiny_rtx(,%r12,8), %rax
	cmpq	%rax, %r14
	jne	.LBB1_303
# BB#302:                               # %land.lhs.true1165
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	side_effects_p
	testq	%rbx, %rax
	movq	72(%rsp), %rax          # 8-byte Reload
	je	.LBB1_481
.LBB1_303:                              # %if.end1169
	movq	const_tiny_rtx+472(,%r12,8), %rax
	cmpq	%rax, %r14
	je	.LBB1_304
# BB#305:                               # %if.end1175
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB1_317
# BB#306:                               # %land.lhs.true1180
	movq	8(%r14), %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB1_317
# BB#307:                               # %land.lhs.true1188
	cmpq	$64, %r15
	movabsq	$0, %rcx
	movabsq	$1, %rdi
	movq	%rcx, %rdx
	jbe	.LBB1_309
# BB#308:                               # %land.lhs.true1188
	movq	%rdi, %rdx
.LBB1_309:                              # %land.lhs.true1188
	andq	%rbx, %rax
	cmpq	$63, %rax
	jne	.LBB1_311
# BB#310:                               # %land.lhs.true1188
	movq	%rdi, %rcx
.LBB1_311:                              # %land.lhs.true1188
	testq	%rdx, %rcx
	jne	.LBB1_317
# BB#312:                               # %land.lhs.true1188
	movl	rtx_equal_function_value_matters(%rip), %eax
	testq	%rbx, %rax
	jne	.LBB1_317
# BB#313:                               # %if.then1196
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$87, %rdi
	jmp	.LBB1_314
.LBB1_317:                              # %if.end1199
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$55, %rcx
	jne	.LBB1_415
# BB#318:                               # %land.lhs.true1204
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB1_415
# BB#319:                               # %if.then1212
	movq	%r14, 112(%rsp)
	movabsq	$simplify_binary_is2orm1, %rdi
	leaq	112(%rsp), %rsi
	callq	do_float_handler
	movq	%rbx, %rsi
	testq	%rsi, %rax
	je	.LBB1_320
# BB#321:                               # %if.end1217
	movzbl	120(%rsp), %eax
	testq	$255, %rax
	je	.LBB1_324
# BB#322:                               # %land.lhs.true1220
	movq	56(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %ecx
	movq	64(%rsp), %rdx          # 8-byte Reload
	andq	%rsi, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB1_324
# BB#323:                               # %if.then1226
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	$75, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB1_316
.LBB1_304:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_404:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_199:                              # %if.then804
	movq	$77, %rdi
	jmp	.LBB1_200
.LBB1_46:                               # %sw.bb
	leaq	136(%rsp), %rax
	leaq	128(%rsp), %rcx
	movq	%rdx, %rdi
	movq	%r10, %rsi
	movq	%rax, %rdx
	callq	neg_double
	movq	136(%rsp), %rdx
	movq	128(%rsp), %r10
.LBB1_47:                               # %sw.bb109
	leaq	136(%rsp), %r8
	leaq	128(%rsp), %r9
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r10, %rcx
	callq	add_double
	jmp	.LBB1_76
.LBB1_48:                               # %sw.bb111
	leaq	136(%rsp), %r8
	leaq	128(%rsp), %r9
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r10, %rcx
	callq	mul_double
	jmp	.LBB1_76
.LBB1_49:                               # %sw.bb114
	andq	%rbx, %rdx
	movq	%rdx, 136(%rsp)
	andq	%r14, %r10
	movq	%r10, 128(%rsp)
	jmp	.LBB1_76
.LBB1_50:                               # %sw.bb116
	orq	%rbx, %rdx
	movq	%rdx, 136(%rsp)
	orq	%r14, %r10
	movq	%r10, 128(%rsp)
	jmp	.LBB1_76
.LBB1_51:                               # %sw.bb118
	xorq	%rbx, %rdx
	movq	%rdx, 136(%rsp)
	xorq	%r14, %r10
	movq	%r10, 128(%rsp)
	jmp	.LBB1_76
.LBB1_52:                               # %sw.bb120
	cmpq	%r10, %r14
	jl	.LBB1_55
# BB#53:                                # %lor.lhs.false123
	je	.LBB1_54
	jmp	.LBB1_63
.LBB1_56:                               # %sw.bb132
	cmpq	%r10, %r14
	jg	.LBB1_55
# BB#57:                                # %lor.lhs.false135
	je	.LBB1_58
	jmp	.LBB1_63
.LBB1_59:                               # %sw.bb144
	cmpq	%r10, %r14
	jb	.LBB1_55
# BB#60:                                # %lor.lhs.false147
	jne	.LBB1_63
.LBB1_54:                               # %lor.lhs.false123
	cmpq	%rdx, %rbx
	jb	.LBB1_55
	jmp	.LBB1_63
.LBB1_61:                               # %sw.bb156
	cmpq	%r10, %r14
	ja	.LBB1_55
# BB#62:                                # %lor.lhs.false159
	jne	.LBB1_63
.LBB1_58:                               # %lor.lhs.false135
	cmpq	%rdx, %rbx
	jbe	.LBB1_63
.LBB1_55:                               # %if.then129
	movq	%rbx, 136(%rsp)
	movq	%r14, 128(%rsp)
	jmp	.LBB1_76
.LBB1_102:                              # %if.else262
	movq	72(%rsp), %r10          # 8-byte Reload
	movl	(%r10), %esi
	movq	%rsi, %r9
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$77, %r9
	jne	.LBB1_104
# BB#103:                               # %if.then267
	movzbl	rtx_class+76(%rip), %eax
	movq	8(%r10), %r15
	cmpq	$99, %rax
	jne	.LBB1_92
.LBB1_93:                               # %land.lhs.true.i2620
	movq	%r15, %rsi
	movq	%rdi, %r14
	callq	swap_commutative_operands_p
	movq	%r14, %rcx
	andq	%rbx, %rax
                                        # kill: R14<def> RCX<kill>
	je	.LBB1_95
# BB#94:                                # %land.lhs.true.i2620
	movq	%r15, %r14
.LBB1_95:                               # %land.lhs.true.i2620
	testq	%rax, %rax
	movq	64(%rsp), %rsi          # 8-byte Reload
	je	.LBB1_97
# BB#96:                                # %land.lhs.true.i2620
	movq	%rcx, %r15
	jmp	.LBB1_97
.LBB1_63:                               # %if.else130
	movq	%rdx, 136(%rsp)
	movq	%r10, 128(%rsp)
	jmp	.LBB1_76
.LBB1_354:                              # %if.then1344
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB1_481
.LBB1_195:                              # %land.lhs.true779
	cmpq	$6, %r15
	movq	72(%rsp), %r10          # 8-byte Reload
	movq	%r13, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	jne	.LBB1_196
# BB#201:                               # %lor.lhs.false794
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_202
	jmp	.LBB1_198
.LBB1_92:
	movq	%rdi, %r14
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB1_97:                               # %if.end.i2625
	movq	$76, %rdi
	movq	%rsi, %rbx
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB1_481
# BB#98:                                # %if.end5.i2626
	movq	$76, %rdi
	movq	$1, %r8
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_plus_minus
	testq	%rax, %rax
	jne	.LBB1_481
# BB#99:                                # %if.end15.i2630
	movq	$76, %rdi
.LBB1_100:                              # %return
	movq	%rbx, %rsi
.LBB1_101:                              # %return
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	jmp	.LBB1_481
.LBB1_68:
	movabsq	$0, %rax
.LBB1_70:                               # %if.then184
	leaq	136(%rsp), %r8
	leaq	128(%rsp), %r9
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rcx
	callq	rshift_double
	jmp	.LBB1_76
.LBB1_204:                              # %if.then809
	movq	$86, %rdi
.LBB1_200:                              # %return
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r10, %rdx
	callq	gen_rtx_fmt_e
	jmp	.LBB1_481
.LBB1_72:                               # %if.else190
	cmpq	$87, %r11
	jne	.LBB1_75
# BB#73:                                # %if.then193
	movq	$1, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rcx
	callq	lshift_double
	jmp	.LBB1_76
.LBB1_335:
	movq	%rbx, %rax
	jmp	.LBB1_481
.LBB1_104:                              # %if.end273
	movq	%rdx, %r11
	cmpq	$7, %r11
	ja	.LBB1_109
# BB#105:                               # %if.end273
	movq	$1, %rdx
	movq	%r11, %rcx
	shlq	%cl, %rdx
	testq	$170, %rdx
	je	.LBB1_109
# BB#106:                               # %land.lhs.true293
	cmpq	$86, %rax
	jne	.LBB1_109
# BB#107:                               # %land.lhs.true293
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %r14
	jne	.LBB1_109
# BB#108:                               # %if.then301
	movq	8(%rdi), %rdx
	movq	$77, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_fmt_e
	jmp	.LBB1_481
.LBB1_391:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_206:
	movq	56(%rsp), %rax          # 8-byte Reload
	jmp	.LBB1_481
.LBB1_109:                              # %if.end306
	cmpq	$133, %rax
	jg	.LBB1_121
# BB#110:                               # %if.end306
	leaq	-54(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB1_112
# BB#111:                               # %if.end306
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB1_123
	jmp	.LBB1_112
.LBB1_375:
	movq	%rsi, %rax
	jmp	.LBB1_481
.LBB1_377:
	movq	%rsi, %rax
	jmp	.LBB1_481
.LBB1_75:                               # %if.else204
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rcx
	callq	rrotate_double
.LBB1_76:                               # %sw.epilog
	movq	136(%rsp), %rdi
	movq	128(%rsp), %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	immed_double_const
	jmp	.LBB1_481
.LBB1_196:                              # %lor.lhs.false789
	cmpq	$8, %rsi
	jne	.LBB1_202
# BB#197:                               # %lor.lhs.false789
	movl	flag_unsafe_math_optimizations(%rip), %eax
	movq	$8, %rsi
	testq	%r9, %rax
	je	.LBB1_198
.LBB1_202:                              # %if.then796
	movq	const_tiny_rtx(,%r12,8), %rax
	jmp	.LBB1_481
.LBB1_121:                              # %if.end306
	cmpq	$134, %rax
	je	.LBB1_123
# BB#122:                               # %if.end306
	cmpq	$140, %rax
	jne	.LBB1_112
.LBB1_123:                              # %land.lhs.true346
	testq	$16711680, %r8          # imm = 0xFF0000
	je	.LBB1_112
# BB#124:                               # %land.lhs.true346
	cmpq	$54, %r9
	jne	.LBB1_112
# BB#125:                               # %if.then357
	movq	8(%r10), %rsi
	callq	plus_constant_wide
	jmp	.LBB1_481
.LBB1_112:                              # %if.else362
	movq	%r10, %r12
	movq	%rdi, %r14
	cmpq	$133, %r9
	jg	.LBB1_126
# BB#113:                               # %if.else362
	leaq	-54(%r9), %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB1_115
# BB#114:                               # %if.else362
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB1_128
	jmp	.LBB1_115
.LBB1_324:                              # %if.else1229
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB1_415
# BB#325:                               # %land.lhs.true1232
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	64(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rsi
	cmpq	%rsi, %rax
	jne	.LBB1_415
# BB#326:                               # %if.then1238
	movq	$77, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB1_296
.LBB1_216:                              # %land.lhs.true874
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_217
# BB#218:                               # %land.lhs.true882
	movq	8(%rax), %rcx
	cmpq	$63, %rcx
	ja	.LBB1_219
# BB#220:                               # %if.then900
	movq	8(%rbx), %rdi
	movq	$1, %r15
	shlq	%cl, %r15
	jmp	.LBB1_221
.LBB1_211:                              # %if.then832
	cmpq	$77, %rax
	jne	.LBB1_212
# BB#213:                               # %if.then842
	movq	8(%rbx), %rdi
	movq	$-1, %r15
	jmp	.LBB1_221
.LBB1_320:
	xorq	%rax, %rax
	jmp	.LBB1_481
.LBB1_126:                              # %if.else362
	cmpq	$134, %r9
	je	.LBB1_128
# BB#127:                               # %if.else362
	cmpq	$140, %r9
	jne	.LBB1_115
.LBB1_128:                              # %land.lhs.true402
	cmpq	$54, %rax
	je	.LBB1_129
# BB#130:                               # %land.lhs.true402
	movabsq	$1, %rcx
	jmp	.LBB1_131
.LBB1_217:
	movq	%rbx, %rdi
	jmp	.LBB1_221
.LBB1_212:
	movq	%rbx, %rdi
	jmp	.LBB1_221
.LBB1_129:
	movabsq	$0, %rcx
.LBB1_131:                              # %land.lhs.true402
	testq	$16711680, %rsi         # imm = 0xFF0000
	je	.LBB1_115
# BB#132:                               # %land.lhs.true402
	testq	%rcx, %rcx
	jne	.LBB1_115
# BB#133:                               # %if.then413
	movq	8(%r14), %rsi
	movq	%r12, %rdi
	callq	plus_constant_wide
	jmp	.LBB1_481
.LBB1_115:                              # %if.end419
	cmpq	$8, %r11
	movq	%r11, %r15
	ja	.LBB1_117
# BB#116:                               # %if.end419
	movq	$1, %rsi
	movq	%r15, %rcx
	shlq	%cl, %rsi
	testq	$324, %rsi              # imm = 0x144
	jne	.LBB1_415
.LBB1_117:                              # %if.then434
	movq	$1, %r13
	cmpq	$87, %rax
	je	.LBB1_137
# BB#118:                               # %if.then434
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	cmpq	$78, %rax
	jne	.LBB1_119
# BB#135:                               # %land.lhs.true448
	movq	%r14, %rsi
	movq	16(%rsi), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movq	%rsi, 48(%rsp)          # 8-byte Spill
                                        # kill: R14<def> RSI<kill>
	jne	.LBB1_142
# BB#136:                               # %if.then456
	movq	8(%rax), %r13
	movq	8(%r14), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_142
.LBB1_219:
	movq	%rbx, %rdi
.LBB1_221:                              # %if.end913
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movzbl	(%r10), %eax
	cmpq	$87, %rax
	movq	$1, %r12
	je	.LBB1_228
# BB#222:                               # %if.end913
	cmpq	$78, %rax
	jne	.LBB1_223
# BB#226:                               # %land.lhs.true927
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	16(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movq	%r10, %rsi
	jne	.LBB1_233
# BB#227:                               # %if.then935
	movq	8(%rax), %r12
	movq	8(%r10), %rsi
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_233
.LBB1_228:                              # %land.lhs.true950
	movq	16(%r10), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_229
# BB#230:                               # %land.lhs.true958
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	8(%rax), %rcx
	cmpq	$63, %rcx
	ja	.LBB1_231
# BB#232:                               # %if.then976
	movq	8(%r10), %rsi
	movq	$1, %r12
	shlq	%cl, %r12
	jmp	.LBB1_233
.LBB1_223:                              # %if.end913
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	cmpq	$77, %rax
	jne	.LBB1_224
# BB#225:                               # %if.then918
	movq	8(%r10), %rsi
	movq	$-1, %r12
	jmp	.LBB1_233
.LBB1_229:
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%r10, %rsi
	jmp	.LBB1_233
.LBB1_224:
	movq	%r10, %rsi
	jmp	.LBB1_233
.LBB1_231:
	movq	%r10, %rsi
.LBB1_233:                              # %if.end989
	movq	%rdi, %r13
	movq	%r10, %r14
	callq	rtx_equal_p
	movq	%r14, %r10
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	movq	80(%rsp), %rsi          # 8-byte Reload
	je	.LBB1_247
# BB#234:                               # %if.then992
	movq	%r9, %r14
	subq	%r12, %r15
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	movzbl	rtx_class+78(%rip), %ecx
	movq	%rax, %r12
	cmpq	$99, %rcx
	jne	.LBB1_235
# BB#236:                               # %land.lhs.true.i2522
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	swap_commutative_operands_p
	andq	%r14, %rax
	movq	%rbx, %r15
	je	.LBB1_238
# BB#237:                               # %land.lhs.true.i2522
	movq	%r12, %r15
.LBB1_238:                              # %land.lhs.true.i2522
	testq	%rax, %rax
	movq	64(%rsp), %rsi          # 8-byte Reload
	je	.LBB1_240
# BB#239:                               # %land.lhs.true.i2522
	movq	%rbx, %r12
	jmp	.LBB1_240
.LBB1_247:                              # %if.end1006
	movl	(%r10), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$77, %rdx
	jne	.LBB1_257
# BB#248:                               # %if.then1011
	movq	%r9, %r14
	movzbl	rtx_class+75(%rip), %eax
	movq	8(%r10), %r15
	cmpq	$99, %rax
	jne	.LBB1_249
# BB#250:                               # %land.lhs.true.i2537
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	swap_commutative_operands_p
	movq	%rbx, %rcx
	andq	%r14, %rax
	movq	%rcx, %r14
	je	.LBB1_252
# BB#251:                               # %land.lhs.true.i2537
	movq	%r15, %r14
.LBB1_252:                              # %land.lhs.true.i2537
	testq	%rax, %rax
	movq	64(%rsp), %rsi          # 8-byte Reload
	je	.LBB1_254
# BB#253:                               # %land.lhs.true.i2537
	movq	%rcx, %r15
	jmp	.LBB1_254
.LBB1_257:                              # %if.end1016
	cmpq	$7, %rsi
	ja	.LBB1_268
# BB#258:                               # %if.end1016
	movq	$1, %rdi
	movq	%rsi, %rcx
	shlq	%cl, %rdi
	testq	$170, %rdi
	je	.LBB1_268
# BB#259:                               # %land.lhs.true1036
	movzbl	(%rbx), %ecx
	leaq	-75(%rcx), %rsi
	andq	%r9, %rsi
	cmpq	$2, %rsi
	jb	.LBB1_266
# BB#260:                               # %land.lhs.true1036
	leaq	-75(%rdx), %rsi
	andq	%r9, %rsi
	cmpq	$2, %rsi
	jb	.LBB1_266
# BB#261:                               # %lor.lhs.false1056
	cmpq	$58, %rcx
	jne	.LBB1_263
# BB#262:                               # %land.lhs.true1061
	movq	8(%rbx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$75, %rcx
	je	.LBB1_266
.LBB1_263:                              # %lor.lhs.false1069
	movq	%r10, %rcx
	movq	%r9, %rdi
	movq	%rbx, %rsi
	cmpq	$58, %rdx
	jne	.LBB1_264
# BB#265:                               # %land.lhs.true1074
	movq	%rcx, %r10
	movq	8(%r10), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$75, %rcx
	movq	%rsi, %rbx
	movq	%rdi, %r9
	jne	.LBB1_268
.LBB1_266:                              # %land.lhs.true1082
	movq	$76, %rdi
	movq	%rbx, %rax
	xorq	%r8, %r8
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
                                        # kill: RBX<def> RAX<kill>
	movq	%r10, %rcx
	movq	%r10, %r15
	movq	%r9, %r14
	callq	simplify_plus_minus
	movq	%r14, %r9
	testq	%rax, %rax
	jne	.LBB1_481
# BB#267:                               # %land.lhs.true1082.if.end1087_crit_edge
	movq	%r15, %r10
	movl	(%r10), %eax
	jmp	.LBB1_268
.LBB1_235:
	movq	%r13, %r15
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB1_240:                              # %if.end.i2527
	movq	$78, %rdi
	movq	%r15, %rdx
	movq	%rsi, %rbx
	movq	%r12, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB1_242
# BB#241:                               # %if.end5.i2528
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
.LBB1_242:                              # %simplify_gen_binary.exit2531
	movzbl	(%rax), %ecx
	cmpq	$78, %rcx
	je	.LBB1_243
# BB#244:                               # %simplify_gen_binary.exit2531
	movabsq	$1, %rcx
	jmp	.LBB1_245
.LBB1_249:
	movq	%rbx, %r14
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB1_254:                              # %if.end.i2542
	movq	$75, %rdi
	movq	%rsi, %rbx
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB1_481
# BB#255:                               # %if.end5.i2543
	movq	$75, %rdi
	movq	$1, %r8
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_plus_minus
	testq	%rax, %rax
	jne	.LBB1_481
# BB#256:                               # %if.end15.i
	movq	$75, %rdi
	jmp	.LBB1_100
.LBB1_243:
	movabsq	$0, %rcx
.LBB1_245:                              # %simplify_gen_binary.exit2531
	movq	72(%rsp), %rdx          # 8-byte Reload
	orq	%rdx, %rcx
	jne	.LBB1_481
# BB#246:                               # %simplify_gen_binary.exit2531
	xorq	%rax, %rax
	jmp	.LBB1_481
.LBB1_16:                               # %avoid_constant_pool_reference.exit2511
	cmpq	$60, %rax
	movq	%rbx, %rdi
	movq	%r13, %rbx
	movq	%r15, %r12
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	80(%rsp), %r11          # 8-byte Reload
	jne	.LBB1_27
# BB#17:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_binary_operation, %rcx
	movq	$891, %rsi              # imm = 0x37B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB1_137:                              # %land.lhs.true471
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_138
# BB#139:                               # %land.lhs.true479
	movq	8(%rax), %rax
	cmpq	$63, %rax
	ja	.LBB1_140
# BB#141:                               # %if.then497
	movq	8(%r14), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	$1, %r13
	movq	%rax, %rcx
	shlq	%cl, %r13
	jmp	.LBB1_142
.LBB1_119:                              # %if.then434
	cmpq	$77, %rax
	jne	.LBB1_120
# BB#134:                               # %if.then439
	movq	8(%r14), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$-1, %r13
	jmp	.LBB1_142
.LBB1_264:
	movq	%rsi, %rbx
	movq	%rdi, %r9
	movq	%rcx, %r10
.LBB1_268:                              # %if.end1087
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$83, %rax
	jne	.LBB1_269
# BB#281:                               # %if.then1106
	movq	8(%r10), %rsi
	movq	%rbx, %rdi
	movq	%r9, %r14
	movq	%r10, %r15
	callq	rtx_equal_p
	movq	16(%r15), %rcx
	testq	%r14, %rax
	je	.LBB1_290
# BB#282:                               # %if.then1112
	movq	$86, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	callq	gen_rtx_fmt_e
	movzbl	rtx_class+83(%rip), %ecx
	movq	%rax, %r15
	cmpq	$99, %rcx
	je	.LBB1_284
# BB#283:
	movq	%rbx, %r14
	jmp	.LBB1_288
.LBB1_269:                              # %if.end1087
	cmpq	$54, %rax
	jne	.LBB1_415
# BB#270:                               # %land.lhs.true1092
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	je	.LBB1_415
# BB#271:                               # %if.then1098
	movq	%r9, %r14
	movq	8(%r10), %rax
	negq	%rax
	movq	%rax, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movzbl	rtx_class+75(%rip), %ecx
	movq	%rax, %r15
	cmpq	$99, %rcx
	jne	.LBB1_272
# BB#273:                               # %land.lhs.true.i2570
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	swap_commutative_operands_p
	andq	%r14, %rax
	movq	%rbx, %r14
	movq	%rbx, %rcx
	je	.LBB1_275
# BB#274:                               # %land.lhs.true.i2570
	movq	%r15, %r14
.LBB1_275:                              # %land.lhs.true.i2570
	testq	%rax, %rax
	je	.LBB1_277
# BB#276:                               # %land.lhs.true.i2570
	movq	%rcx, %r15
	jmp	.LBB1_277
.LBB1_138:
	movq	%r14, 48(%rsp)          # 8-byte Spill
	jmp	.LBB1_142
.LBB1_140:
	movq	%r14, 48(%rsp)          # 8-byte Spill
	jmp	.LBB1_142
.LBB1_120:
	movq	%r14, 48(%rsp)          # 8-byte Spill
.LBB1_142:                              # %if.end509
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	cmpq	$87, %r9
	je	.LBB1_149
# BB#143:                               # %if.end509
	cmpq	$78, %r9
	jne	.LBB1_144
# BB#147:                               # %land.lhs.true523
	movq	%r12, %rdx
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %eax
	cmpq	$54, %rax
	movq	%rdx, %rax
                                        # kill: R12<def> RDX<kill>
	jne	.LBB1_154
# BB#148:                               # %if.then531
	movq	8(%rcx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	8(%r12), %rax
	movq	$1, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_154
.LBB1_290:                              # %if.end1118
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	rtx_equal_p
	movq	%r14, %rcx
	testq	%rcx, %rax
	je	.LBB1_415
# BB#291:                               # %if.then1124
	movq	%rcx, %r14
	movq	8(%r15), %rax
	movq	$86, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movzbl	rtx_class+83(%rip), %ecx
	movq	%rax, %r15
	cmpq	$99, %rcx
	jne	.LBB1_292
.LBB1_284:                              # %land.lhs.true.i2588
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	swap_commutative_operands_p
	andq	%r14, %rax
	movq	%rbx, %r14
	movq	%rbx, %rcx
	je	.LBB1_286
# BB#285:                               # %land.lhs.true.i2588
	movq	%r15, %r14
.LBB1_286:                              # %land.lhs.true.i2588
	testq	%rax, %rax
	je	.LBB1_288
# BB#287:                               # %land.lhs.true.i2588
	movq	%rcx, %r15
	jmp	.LBB1_288
.LBB1_272:
	movq	%rbx, %r14
.LBB1_277:                              # %if.end.i2575
	movq	$75, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB1_481
# BB#278:                               # %if.end5.i2576
	movq	$75, %rdi
	movq	$1, %r8
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_plus_minus
	testq	%rax, %rax
	jne	.LBB1_481
# BB#279:                               # %if.end15.i2580
	movq	$75, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB1_101
.LBB1_149:                              # %land.lhs.true546
	movq	16(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_150
# BB#151:                               # %land.lhs.true554
	movq	8(%rax), %rcx
	cmpq	$63, %rcx
	ja	.LBB1_152
# BB#153:                               # %if.then572
	movq	8(%r12), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	jmp	.LBB1_154
.LBB1_144:                              # %if.end509
	cmpq	$77, %r9
	jne	.LBB1_145
# BB#146:                               # %if.then514
	movq	8(%r12), %rax
	movq	$-1, 56(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_154
.LBB1_292:
	movq	%rbx, %r14
.LBB1_288:                              # %if.end.i2593
	movq	$83, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB1_481
# BB#289:                               # %if.end5.i2594
	movq	$83, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB1_101
.LBB1_150:
	movq	%r12, %rax
	jmp	.LBB1_154
.LBB1_152:
	movq	%r12, %rax
	jmp	.LBB1_154
.LBB1_145:
	movq	%r12, %rax
.LBB1_154:                              # %if.end585
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB1_168
# BB#155:                               # %if.then588
	movq	56(%rsp), %rax          # 8-byte Reload
	addq	%r13, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movzbl	rtx_class+78(%rip), %ecx
	movq	%rax, %r14
	cmpq	$99, %rcx
	jne	.LBB1_156
# BB#157:                               # %land.lhs.true.i
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	swap_commutative_operands_p
	andq	%rbx, %rax
	movq	%r15, %r12
	movq	%r15, %rcx
	je	.LBB1_159
# BB#158:                               # %land.lhs.true.i
	movq	%r14, %r12
.LBB1_159:                              # %land.lhs.true.i
	testq	%rax, %rax
	je	.LBB1_161
# BB#160:                               # %land.lhs.true.i
	movq	%rcx, %r14
	jmp	.LBB1_161
.LBB1_168:                              # %if.end599
	cmpq	$7, %r15
	movq	%rbx, %rsi
	ja	.LBB1_415
# BB#169:                               # %if.end599
	movq	$1, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	testq	$170, %rax
	je	.LBB1_415
# BB#170:                               # %land.lhs.true619
	movzbl	(%r14), %ecx
	leaq	-75(%rcx), %rax
	andq	%rsi, %rax
	cmpq	$2, %rax
	jb	.LBB1_176
# BB#171:                               # %lor.lhs.false629
	movzbl	(%r12), %eax
	leaq	-75(%rax), %rdx
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jb	.LBB1_176
# BB#172:                               # %lor.lhs.false639
	cmpq	$58, %rcx
	jne	.LBB1_174
# BB#173:                               # %land.lhs.true644
	movq	8(%r14), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$75, %rcx
	je	.LBB1_176
.LBB1_174:                              # %lor.lhs.false652
	cmpq	$58, %rax
	jne	.LBB1_415
# BB#175:                               # %land.lhs.true657
	movq	8(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$75, %rax
	jne	.LBB1_415
.LBB1_176:                              # %land.lhs.true665
	movq	$75, %rdi
	xorq	%r8, %r8
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	simplify_plus_minus
	testq	%rax, %rax
	jne	.LBB1_481
.LBB1_415:                              # %sw.epilog1667
	xorq	%rax, %rax
.LBB1_481:                              # %return
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_156:
	movq	48(%rsp), %r12          # 8-byte Reload
.LBB1_161:                              # %if.end.i2515
	movq	$78, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB1_163
# BB#162:                               # %if.end5.i
	movq	$78, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
.LBB1_163:                              # %simplify_gen_binary.exit
	movzbl	(%rax), %ecx
	cmpq	$78, %rcx
	je	.LBB1_164
# BB#165:                               # %simplify_gen_binary.exit
	movabsq	$1, %rcx
	jmp	.LBB1_166
.LBB1_164:
	movabsq	$0, %rcx
.LBB1_166:                              # %simplify_gen_binary.exit
	movq	80(%rsp), %rdx          # 8-byte Reload
	orq	%rdx, %rcx
	jne	.LBB1_481
# BB#167:                               # %simplify_gen_binary.exit
	xorq	%rax, %rax
	jmp	.LBB1_481
.Ltmp34:
	.size	simplify_binary_operation, .Ltmp34-simplify_binary_operation
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_481
	.quad	.LBB1_433
	.quad	.LBB1_435
	.quad	.LBB1_478
	.quad	.LBB1_436
	.quad	.LBB1_437
	.quad	.LBB1_442
	.quad	.LBB1_446
	.quad	.LBB1_450
	.quad	.LBB1_479
	.quad	.LBB1_454
	.quad	.LBB1_455
	.quad	.LBB1_478
	.quad	.LBB1_458
	.quad	.LBB1_468
	.quad	.LBB1_460
	.quad	.LBB1_456
	.quad	.LBB1_464
	.quad	.LBB1_470
	.quad	.LBB1_475
	.quad	.LBB1_473
	.quad	.LBB1_476
.LJTI1_1:
	.quad	.LBB1_177
	.quad	.LBB1_84
	.quad	.LBB1_188
	.quad	.LBB1_423
	.quad	.LBB1_293
	.quad	.LBB1_372
	.quad	.LBB1_395
	.quad	.LBB1_369
	.quad	.LBB1_392
	.quad	.LBB1_352
	.quad	.LBB1_328
	.quad	.LBB1_343
	.quad	.LBB1_423
	.quad	.LBB1_405
	.quad	.LBB1_400
	.quad	.LBB1_405
	.quad	.LBB1_405
	.quad	.LBB1_400
	.quad	.LBB1_409
	.quad	.LBB1_416
	.quad	.LBB1_419
	.quad	.LBB1_421
.LJTI1_2:
	.quad	.LBB1_47
	.quad	.LBB1_46
	.quad	.LBB1_481
	.quad	.LBB1_48
	.quad	.LBB1_481
	.quad	.LBB1_481
	.quad	.LBB1_481
	.quad	.LBB1_481
	.quad	.LBB1_49
	.quad	.LBB1_50
	.quad	.LBB1_51
	.quad	.LBB1_481
	.quad	.LBB1_64
	.quad	.LBB1_64
	.quad	.LBB1_64
	.quad	.LBB1_64
	.quad	.LBB1_64
	.quad	.LBB1_52
	.quad	.LBB1_56
	.quad	.LBB1_59
	.quad	.LBB1_61

	.text
	.align	16, 0x90
	.type	simplify_plus_minus,@function
simplify_plus_minus:                    # @simplify_plus_minus
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
	subq	$256, %rsp              # imm = 0x100
.Ltmp46:
	.cfi_def_cfa_offset 304
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
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %rdi
	xorq	%r13, %r13
	cmpq	$76, %rdi
	movq	$0, 248(%rsp)
	movq	$0, 240(%rsp)
	movq	$0, 232(%rsp)
	movq	$0, 224(%rsp)
	movq	$0, 216(%rsp)
	movq	$0, 208(%rsp)
	movq	$0, 200(%rsp)
	movq	$0, 192(%rsp)
	movq	$0, 184(%rsp)
	movq	$0, 176(%rsp)
	movq	$0, 168(%rsp)
	movq	$0, 160(%rsp)
	movq	$0, 152(%rsp)
	movq	$0, 144(%rsp)
	movq	$0, 136(%rsp)
	movq	$0, 128(%rsp)
	movq	%rdx, 128(%rsp)
	movl	%r13d, 136(%rsp)
	movq	%rcx, 144(%rsp)
	movabsq	$0, %rax
	movabsq	$1, %rcx
	jne	.LBB2_2
# BB#1:                                 # %entry
	movq	%rcx, %rax
.LBB2_2:                                # %entry
	movl	%eax, 152(%rsp)
	movq	$2, 16(%rsp)            # 8-byte Folded Spill
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	$2, %r11
	jmp	.LBB2_3
	.align	16, 0x90
.LBB2_46:                               # %for.inc
                                        #   in Loop: Header=BB2_3 Depth=1
	incq	%rbx
.LBB2_3:                                # %for.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_4 Depth 2
	movq	%r11, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB2_4:                                # %for.cond
                                        #   Parent Loop BB2_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB2_5
# BB#47:                                # %do.cond
                                        #   in Loop: Header=BB2_4 Depth=2
	xorq	%rbx, %rbx
	testq	%r10, %rax
	movabsq	$0, %rax
	jne	.LBB2_4
	jmp	.LBB2_48
	.align	16, 0x90
.LBB2_5:                                # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	128(%rsp,%rdx), %rsi
	movzbl	(%rsi), %edi
	movl	136(%rsp,%rdx), %r9d
	leaq	128(%rsp,%rdx), %r12
	leaq	136(%rsp,%rdx), %r15
	cmpq	$85, %rdi
	jg	.LBB2_18
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$74, %rdi
	jg	.LBB2_14
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$54, %rdi
	jne	.LBB2_8
# BB#42:                                # %sw.bb275
                                        #   in Loop: Header=BB2_3 Depth=1
	testq	%r10, %r9
	je	.LBB2_46
# BB#43:                                # %if.then277
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	8(%rsi), %rdi
	negq	%rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%r11, %r14
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r14, %r11
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	%rax, (%r12)
	xorq	%rax, %rax
	jmp	.LBB2_44
	.align	16, 0x90
.LBB2_18:                               # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$86, %rdi
	jne	.LBB2_46
# BB#19:                                # %sw.bb251
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%r11, %rcx
	andq	%r10, %rcx
	cmpq	$7, %rcx
	jne	.LBB2_39
# BB#20:                                #   in Loop: Header=BB2_3 Depth=1
	movq	$7, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_14:                               # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	leaq	-75(%rdi), %rcx
	andq	%r10, %rcx
	cmpq	$2, %rcx
	jae	.LBB2_15
# BB#21:                                # %sw.bb
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%r11, %rcx
	andq	%r10, %rcx
	xorq	%rax, %rax
	cmpq	$7, %rcx
	je	.LBB2_187
# BB#22:                                # %if.end
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	16(%rsi), %rdx
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rdx, 128(%rsp,%rax)
	cmpq	$76, %rdi
	movabsq	$0, %rcx
	jne	.LBB2_24
# BB#23:                                # %if.end
                                        #   in Loop: Header=BB2_3 Depth=1
	movabsq	$1, %rcx
.LBB2_24:                               # %if.end
                                        #   in Loop: Header=BB2_3 Depth=1
	xorq	%r9, %rcx
	movl	%ecx, 136(%rsp,%rax)
	movq	8(%rsi), %rax
	incq	%r11
	movq	%rax, (%r12)
	incq	16(%rsp)                # 8-byte Folded Spill
	jmp	.LBB2_45
.LBB2_8:                                # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$58, %rdi
	jne	.LBB2_46
# BB#9:                                 # %sw.bb41
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$6, %rdx
	jg	.LBB2_46
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	8(%rsi), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$75, %rcx
	jne	.LBB2_46
# BB#11:                                # %land.lhs.true51
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%r11, %r14
	movq	8(%rdi), %r11
	movzbl	(%r11), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_27
# BB#12:                                # %land.lhs.true51
                                        #   in Loop: Header=BB2_3 Depth=1
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	jbe	.LBB2_25
# BB#13:                                #   in Loop: Header=BB2_3 Depth=1
	movq	%r14, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_39:                               # %if.then254
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	const_int_rtx+504(%rip), %rax
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%rax, 128(%rsp,%rdx)
	movl	%r9d, 136(%rsp,%rdx)
	movq	8(%rsi), %rax
	testq	%r10, %r9
	movq	%rax, (%r12)
	movabsq	$0, %rax
	jne	.LBB2_41
# BB#40:                                # %if.then254
                                        #   in Loop: Header=BB2_3 Depth=1
	movabsq	$1, %rax
.LBB2_41:                               # %if.then254
                                        #   in Loop: Header=BB2_3 Depth=1
	incq	%r11
.LBB2_44:                               # %for.inc
                                        #   in Loop: Header=BB2_3 Depth=1
	movl	%eax, (%r15)
.LBB2_45:                               # %for.inc
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	$1, %rax
	jmp	.LBB2_46
.LBB2_15:                               # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$77, %rdi
	jne	.LBB2_46
# BB#16:                                # %sw.bb31
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	8(%rsi), %rax
	movq	%rax, (%r12)
	testq	%r10, %r9
	movabsq	$0, %rax
	jne	.LBB2_44
# BB#17:                                # %sw.bb31
                                        #   in Loop: Header=BB2_3 Depth=1
	movabsq	$1, %rax
	jmp	.LBB2_44
.LBB2_27:                               # %land.lhs.true51
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$134, %rcx
	je	.LBB2_30
# BB#28:                                # %land.lhs.true51
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$140, %rcx
	je	.LBB2_30
# BB#29:                                #   in Loop: Header=BB2_3 Depth=1
	movq	%r14, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_25:                               # %land.lhs.true51
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	je	.LBB2_26
.LBB2_30:                               # %land.lhs.true138
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_35
# BB#31:                                # %land.lhs.true138
                                        #   in Loop: Header=BB2_3 Depth=1
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	jbe	.LBB2_33
# BB#32:                                #   in Loop: Header=BB2_3 Depth=1
	movq	%r14, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_35:                               # %land.lhs.true138
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$134, %rcx
	je	.LBB2_38
# BB#36:                                # %land.lhs.true138
                                        #   in Loop: Header=BB2_3 Depth=1
	cmpq	$140, %rcx
	je	.LBB2_38
# BB#37:                                #   in Loop: Header=BB2_3 Depth=1
	movq	%r14, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_33:                               # %land.lhs.true138
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	je	.LBB2_34
.LBB2_38:                               # %if.then226
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%r11, (%r12)
	movq	8(%rsi), %rax
	movq	16(%rax), %rax
	movq	$32, %rcx
	movq	%r14, %r11
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	%rax, 128(%rsp,%rdx)
	movl	%r9d, 136(%rsp,%rdx)
	incq	%r11
	incq	%r13
	jmp	.LBB2_45
.LBB2_26:                               #   in Loop: Header=BB2_3 Depth=1
	movq	%r14, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_34:                               #   in Loop: Header=BB2_3 Depth=1
	movq	%r14, %r11
	incq	%rbx
	jmp	.LBB2_3
.LBB2_48:                               # %do.end
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jg	.LBB2_50
# BB#49:                                # %do.end
	xorq	%rax, %rax
	testq	%r10, %r8
	je	.LBB2_187
.LBB2_50:                               # %for.cond293.preheader
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_51
# BB#66:
	movq	$1, %rsi
	leaq	128(%rsp), %rcx
	movabsq	$0, %rdx
	movabsq	$1, %r9
	movq	%r11, %rdi
	andq	%r10, %rdi
	.align	16, 0x90
.LBB2_67:                               # %for.body296
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rbx
	movzbl	(%rbx), %ebx
	cmpq	$58, %rbx
	movq	%rdx, %rbx
	jne	.LBB2_69
# BB#68:                                # %for.body296
                                        #   in Loop: Header=BB2_67 Depth=1
	movq	%r9, %rbx
.LBB2_69:                               # %for.body296
                                        #   in Loop: Header=BB2_67 Depth=1
	addq	%r13, %rbx
	movq	%rsi, %rax
	andq	%r10, %rax
	incq	%rsi
	addq	$16, %rcx
	cmpq	%rdi, %rax
	movq	%rbx, %r13
	jne	.LBB2_67
	jmp	.LBB2_52
.LBB2_51:
	movq	%r13, %rbx
.LBB2_52:                               # %do.body310.preheader
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r8, 24(%rsp)           # 8-byte Spill
	leaq	-1(%r11), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$1, 32(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_53:                               # %do.body310
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_72 Depth 2
                                        #       Child Loop BB2_75 Depth 3
                                        #       Child Loop BB2_119 Depth 3
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_54
# BB#70:                                # %for.body314.lr.ph
                                        #   in Loop: Header=BB2_53 Depth=1
	xorq	%rdx, %rdx
	movq	$2, %rdi
	leaq	152(%rsp), %rax
	movq	%rax, %r8
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB2_72:                               # %for.body314
                                        #   Parent Loop BB2_53 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_75 Depth 3
                                        #       Child Loop BB2_119 Depth 3
	movq	%r8, 96(%rsp)           # 8-byte Spill
	movq	%rdx, %rax
	leaq	1(%rax), %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB2_73
# BB#74:                                # %for.body318.lr.ph
                                        #   in Loop: Header=BB2_72 Depth=2
	movq	$4, %rcx
	shlq	%cl, %rax
	xorq	%r14, %r14
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%r10, %rcx
	leaq	128(%rsp,%rax), %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	leaq	136(%rsp,%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%r8, %r13
	movq	%rdi, %r15
	movq	%rdi, %r12
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	jne	.LBB2_75
	.align	16, 0x90
.LBB2_119:                              # %for.body318
                                        #   Parent Loop BB2_53 Depth=1
                                        #     Parent Loop BB2_72 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB2_149
# BB#120:                               # %for.body318
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	-8(%r8,%r14), %r15
	testq	%r15, %r15
	je	.LBB2_149
# BB#121:                               # %if.then418
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movl	(%r8,%r14), %edx
	movq	%rdx, %rax
	andq	%r10, %rax
	movq	%rsi, %rcx
	andq	%r10, %rcx
	cmpq	%rax, %rcx
	jne	.LBB2_122
# BB#127:                               # %if.else
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r10, %r13
	callq	swap_commutative_operands_p
	andq	%r13, %rax
	movabsq	$0, %rcx
	jne	.LBB2_129
# BB#128:                               # %if.else
                                        #   in Loop: Header=BB2_119 Depth=3
	movabsq	$1, %rcx
.LBB2_129:                              # %if.else
                                        #   in Loop: Header=BB2_119 Depth=3
	testq	%rax, %rax
	movq	%rbx, %rax
	je	.LBB2_131
# BB#130:                               # %if.else
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%r15, %rax
.LBB2_131:                              # %if.else
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	$75, %r13
	jmp	.LBB2_132
	.align	16, 0x90
.LBB2_122:                              # %if.then421
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%rsi, %rax
	andq	%r10, %rax
	movabsq	$0, %rcx
	jne	.LBB2_124
# BB#123:                               # %if.then421
                                        #   in Loop: Header=BB2_119 Depth=3
	movabsq	$1, %rcx
.LBB2_124:                              # %if.then421
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%rbx, %rax
	je	.LBB2_126
# BB#125:                               # %if.then421
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%r15, %rax
.LBB2_126:                              # %if.then421
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	$76, %r13
.LBB2_132:                              # %if.end429
                                        #   in Loop: Header=BB2_119 Depth=3
	testq	%rcx, %rcx
	jne	.LBB2_134
# BB#133:                               # %if.end429
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%rbx, %r15
.LBB2_134:                              # %if.end429
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%r13, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	movq	%rdx, %rbx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB2_136
# BB#135:                               #   in Loop: Header=BB2_119 Depth=3
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	jmp	.LBB2_149
	.align	16, 0x90
.LBB2_136:                              # %land.lhs.true432
                                        #   in Loop: Header=BB2_119 Depth=3
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$58, %rdx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	jne	.LBB2_141
# BB#137:                               # %land.lhs.true437
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	8(%rax), %rdx
	movzbl	(%rdx), %esi
	andq	%r10, %r13
	cmpq	%r13, %rsi
	jne	.LBB2_140
# BB#138:                               # %land.lhs.true445
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	8(%rdx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB2_140
# BB#139:                               # %land.lhs.true454
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	16(%rdx), %rdx
	cmpq	%r15, %rdx
	je	.LBB2_149
.LBB2_140:                              # %if.then476.thread
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	andq	%rdx, %rsi
	jmp	.LBB2_145
.LBB2_141:                              # %if.then476
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdi          # 8-byte Reload
	andq	%rdi, %rsi
	cmpq	$77, %rdx
	jne	.LBB2_145
# BB#142:                               # %if.then481
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	8(%rax), %rax
	testq	%r10, %rsi
	movabsq	$0, %rcx
	movq	%rcx, %rsi
	jne	.LBB2_144
# BB#143:                               # %if.then481
                                        #   in Loop: Header=BB2_119 Depth=3
	movabsq	$1, %rcx
	movq	%rcx, %rsi
.LBB2_144:                              # %if.then481
                                        #   in Loop: Header=BB2_119 Depth=3
	movl	(%rax), %ecx
.LBB2_145:                              # %if.end488
                                        #   in Loop: Header=BB2_119 Depth=3
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB2_148
# BB#146:                               # %if.end488
                                        #   in Loop: Header=BB2_119 Depth=3
	testq	%r10, %rsi
	je	.LBB2_148
# BB#147:                               # %if.then495
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	8(%rax), %rdi
	negq	%rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%r10, %r15
	movq	%r11, %r13
	movq	%r8, %rbx
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rsi, %rsi
	movq	%rbx, %r8
	movq	%r13, %r11
	movq	%r15, %r10
.LBB2_148:                              # %if.end497
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	%esi, (%rax)
	movq	$0, -8(%r8,%r14)
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
.LBB2_149:                              # %for.inc509
                                        #   in Loop: Header=BB2_119 Depth=3
	movq	%r12, %rax
	andq	%r10, %rax
	movq	%r11, %rcx
	andq	%r10, %rcx
	incq	%r12
	addq	$16, %r14
	cmpq	%rcx, %rax
	jne	.LBB2_119
	jmp	.LBB2_71
	.align	16, 0x90
.LBB2_75:                               # %for.body318.us
                                        #   Parent Loop BB2_53 Depth=1
                                        #     Parent Loop BB2_72 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB2_118
# BB#76:                                # %for.body318.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	-8(%r13), %r12
	testq	%r12, %r12
	je	.LBB2_118
# BB#77:                                # %lor.lhs.false338.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movzbl	(%rbx), %ecx
	movl	(%r13), %edx
	cmpq	$133, %rcx
	jg	.LBB2_80
# BB#78:                                # %lor.lhs.false338.us
                                        #   in Loop: Header=BB2_75 Depth=3
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_118
# BB#79:                                # %lor.lhs.false338.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB2_82
	jmp	.LBB2_118
	.align	16, 0x90
.LBB2_80:                               # %lor.lhs.false338.us
                                        #   in Loop: Header=BB2_75 Depth=3
	cmpq	$134, %rcx
	je	.LBB2_82
# BB#81:                                # %lor.lhs.false338.us
                                        #   in Loop: Header=BB2_75 Depth=3
	cmpq	$140, %rcx
	jne	.LBB2_118
.LBB2_82:                               # %land.lhs.true378.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movzbl	(%r12), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_85
# BB#83:                                # %land.lhs.true378.us
                                        #   in Loop: Header=BB2_75 Depth=3
	addq	$-54, %rcx
	andq	%r10, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_118
# BB#84:                                # %land.lhs.true378.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB2_87
	jmp	.LBB2_118
.LBB2_85:                               # %land.lhs.true378.us
                                        #   in Loop: Header=BB2_75 Depth=3
	cmpq	$134, %rcx
	je	.LBB2_87
# BB#86:                                # %land.lhs.true378.us
                                        #   in Loop: Header=BB2_75 Depth=3
	cmpq	$140, %rcx
	jne	.LBB2_118
.LBB2_87:                               # %if.then418.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%rdx, %rax
	andq	%r10, %rax
	movq	%rsi, %rcx
	andq	%r10, %rcx
	cmpq	%rax, %rcx
	jne	.LBB2_88
# BB#93:                                # %if.else.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r10, %r14
	callq	swap_commutative_operands_p
	movq	%r14, %r10
	andq	%r10, %rax
	movabsq	$0, %rcx
	jne	.LBB2_95
# BB#94:                                # %if.else.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movabsq	$1, %rcx
.LBB2_95:                               # %if.else.us
                                        #   in Loop: Header=BB2_75 Depth=3
	testq	%rax, %rax
	movq	%rbx, %rax
	je	.LBB2_97
# BB#96:                                # %if.else.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%r12, %rax
.LBB2_97:                               # %if.else.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	$75, %rdi
	jmp	.LBB2_98
.LBB2_88:                               # %if.then421.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%rsi, %rax
	andq	%r10, %rax
	movabsq	$0, %rcx
	jne	.LBB2_90
# BB#89:                                # %if.then421.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movabsq	$1, %rcx
.LBB2_90:                               # %if.then421.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%rbx, %rax
	je	.LBB2_92
# BB#91:                                # %if.then421.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%r12, %rax
.LBB2_92:                               # %if.then421.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	$76, %rdi
.LBB2_98:                               # %if.end429.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%r10, %r14
	testq	%rcx, %rcx
	jne	.LBB2_100
# BB#99:                                # %if.end429.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%rbx, %r12
.LBB2_100:                              # %if.end429.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%rdi, %rbx
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	%r12, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB2_102
# BB#101:                               #   in Loop: Header=BB2_75 Depth=3
	movq	%r14, %r10
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	jmp	.LBB2_118
.LBB2_102:                              # %land.lhs.true432.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$86, %rdx
	movq	%r14, %r10
	movq	72(%rsp), %r11          # 8-byte Reload
	movq	96(%rsp), %r8           # 8-byte Reload
	jne	.LBB2_103
# BB#107:                               # %land.lhs.true470.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	8(%rax), %rdx
.LBB2_108:                              # %land.lhs.true470.us
                                        #   in Loop: Header=BB2_75 Depth=3
	cmpq	%r12, %rdx
	je	.LBB2_118
	jmp	.LBB2_109
.LBB2_103:                              # %land.lhs.true432.us
                                        #   in Loop: Header=BB2_75 Depth=3
	cmpq	$58, %rdx
	jne	.LBB2_110
# BB#104:                               # %land.lhs.true437.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	8(%rax), %rdx
	movzbl	(%rdx), %esi
	andq	%r10, %rbx
	cmpq	%rbx, %rsi
	jne	.LBB2_109
# BB#105:                               # %land.lhs.true445.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	8(%rdx), %rsi
	movq	80(%rsp), %rdi          # 8-byte Reload
	cmpq	%rdi, %rsi
	jne	.LBB2_109
# BB#106:                               # %land.lhs.true454.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	16(%rdx), %rdx
	jmp	.LBB2_108
.LBB2_109:                              # %if.then476.us.thread1038
                                        #   in Loop: Header=BB2_75 Depth=3
	movl	(%r13), %esi
	movq	88(%rsp), %rdx          # 8-byte Reload
	andq	%rdx, %rsi
	jmp	.LBB2_114
.LBB2_110:                              # %if.then476.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movl	(%r13), %esi
	movq	88(%rsp), %rdi          # 8-byte Reload
	andq	%rdi, %rsi
	cmpq	$77, %rdx
	jne	.LBB2_114
# BB#111:                               # %if.then481.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	8(%rax), %rax
	testq	%r10, %rsi
	movabsq	$0, %rcx
	movq	%rcx, %rsi
	jne	.LBB2_113
# BB#112:                               # %if.then481.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movabsq	$1, %rcx
	movq	%rcx, %rsi
.LBB2_113:                              # %if.then481.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movl	(%rax), %ecx
.LBB2_114:                              # %if.end488.us
                                        #   in Loop: Header=BB2_75 Depth=3
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB2_117
# BB#115:                               # %if.end488.us
                                        #   in Loop: Header=BB2_75 Depth=3
	testq	%r10, %rsi
	je	.LBB2_117
# BB#116:                               # %if.then495.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	8(%rax), %rdi
	negq	%rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%r10, %r14
	movq	%r11, %r12
	movq	%r8, %rbx
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rsi, %rsi
	movq	%rbx, %r8
	movq	%r12, %r11
	movq	%r14, %r10
.LBB2_117:                              # %if.end497.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx)
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	%esi, (%rax)
	movq	$0, -8(%r13)
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB2_118:                              # %for.inc509.us
                                        #   in Loop: Header=BB2_75 Depth=3
	movq	%r15, %rax
	andq	%r10, %rax
	movq	%r11, %rcx
	andq	%r10, %rcx
	incq	%r15
	addq	$16, %r13
	cmpq	%rcx, %rax
	jne	.LBB2_75
	jmp	.LBB2_71
	.align	16, 0x90
.LBB2_73:                               #   in Loop: Header=BB2_72 Depth=2
	movq	%rdi, 56(%rsp)          # 8-byte Spill
.LBB2_71:                               # %for.cond311.loopexit
                                        #   in Loop: Header=BB2_72 Depth=2
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	andq	%r10, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%r10, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	incq	%rdi
	addq	$16, %r8
	cmpq	%rcx, %rax
	jne	.LBB2_72
	jmp	.LBB2_55
	.align	16, 0x90
.LBB2_54:                               #   in Loop: Header=BB2_53 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB2_55:                               # %for.end514
                                        #   in Loop: Header=BB2_53 Depth=1
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%r10, %rax
	jne	.LBB2_53
# BB#56:                                # %for.cond518.preheader
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_150
# BB#57:
	xorq	%r15, %r15
	movq	$1, %rax
	leaq	128(%rsp), %rdx
	andq	%r10, %r11
	.align	16, 0x90
.LBB2_58:                               # %for.body521
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rdx)
	je	.LBB2_60
# BB#59:                                # %if.then526
                                        #   in Loop: Header=BB2_58 Depth=1
	movq	$32, %rcx
	leaq	1(%r15), %rsi
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	(%rdx), %rdi
	movq	8(%rdx), %rbx
	movq	$4, %rcx
	shlq	%cl, %r15
	movq	%rbx, 136(%rsp,%r15)
	movq	%rdi, 128(%rsp,%r15)
	movq	%rsi, %r15
.LBB2_60:                               # %for.inc533
                                        #   in Loop: Header=BB2_58 Depth=1
	movq	%rax, %rcx
	andq	%r10, %rcx
	incq	%rax
	addq	$16, %rdx
	cmpq	%r11, %rcx
	jne	.LBB2_58
# BB#61:                                # %for.end535
	movq	%r10, %rbx
	movabsq	$simplify_plus_minus_op_data_cmp, %rcx
	leaq	128(%rsp), %r12
	movq	$16, %rdx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	specqsort
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB2_156
# BB#62:                                # %land.lhs.true539
	leaq	-1(%r15), %r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	128(%rsp,%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movq	%rbx, %r8
	jne	.LBB2_157
# BB#63:                                # %land.lhs.true548
	leaq	-2(%r15), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$4, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	128(%rsp,%rsi), %rdi
	movzbl	(%rdi), %ecx
	leaq	128(%rsp,%rsi), %r14
	cmpq	$133, %rcx
	jg	.LBB2_151
# BB#64:                                # %land.lhs.true548
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_157
# BB#65:                                # %land.lhs.true548
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB2_153
	jmp	.LBB2_157
.LBB2_150:                              # %if.end649.thread
	movabsq	$simplify_plus_minus_op_data_cmp, %rcx
	leaq	128(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$16, %rdx
	xorq	%r14, %r14
	movq	%r10, %rbx
	callq	specqsort
	movq	%rbx, %r8
	xorq	%r15, %r15
	xorq	%rdx, %rdx
	jmp	.LBB2_166
.LBB2_151:                              # %land.lhs.true548
	cmpq	$134, %rcx
	je	.LBB2_153
# BB#152:                               # %land.lhs.true548
	cmpq	$140, %rcx
	jne	.LBB2_157
.LBB2_153:                              # %if.then620
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	136(%rsp,%rdx), %edx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	136(%rsp,%rbx), %ecx
	movq	%r8, %rbx
	cmpq	%rcx, %rdx
	je	.LBB2_155
# BB#154:                               # %if.then635
	movq	8(%rax), %rdi
	negq	%rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	(%r14), %rdi
.LBB2_155:                              # %if.end637
	movq	8(%rax), %rsi
	callq	plus_constant_wide
	movq	%rax, (%r14)
	movq	%r13, %r15
.LBB2_156:                              # %if.end649
	movq	%rbx, %r8
.LBB2_157:                              # %if.end649
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jle	.LBB2_158
# BB#159:                               # %if.end649
	movabsq	$1, %r14
	jmp	.LBB2_160
.LBB2_158:
	movabsq	$0, %r14
.LBB2_160:                              # %if.end649
	jle	.LBB2_161
# BB#162:
	movq	$1, %rax
	movabsq	$0, %rbx
	movabsq	$1, %rsi
	movq	%r15, %rdi
	andq	%r8, %rdi
	.align	16, 0x90
.LBB2_163:                              # %for.body653
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$58, %rcx
	movq	%rbx, %rcx
	jne	.LBB2_165
# BB#164:                               # %for.body653
                                        #   in Loop: Header=BB2_163 Depth=1
	movq	%rsi, %rcx
.LBB2_165:                              # %for.body653
                                        #   in Loop: Header=BB2_163 Depth=1
	addq	%rcx, %rdx
	movq	%rax, %rcx
	andq	%r8, %rcx
	incq	%rax
	addq	$16, %r12
	cmpq	%rdi, %rcx
	jne	.LBB2_163
	jmp	.LBB2_166
.LBB2_161:
	xorq	%r14, %r14
.LBB2_166:                              # %for.end666
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB2_170
# BB#167:                               # %land.lhs.true668
	leaq	(%rdx,%r15), %r9
	movq	$32, %rcx
	movq	%r9, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rax, %rax
	cmpq	%rbx, %rdi
	jg	.LBB2_187
# BB#168:                               # %lor.lhs.false672
	andq	%r8, %rsi
	andq	%r8, %r9
	cmpq	%rsi, %r9
	jne	.LBB2_170
# BB#169:                               # %lor.lhs.false672
	movq	$32, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jle	.LBB2_187
.LBB2_170:                              # %for.cond681.preheader
	xorq	%rax, %rax
	testq	%r14, %r14
	je	.LBB2_174
# BB#171:                               # %land.rhs.preheader
	leaq	136(%rsp), %rdx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB2_173:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB2_174
# BB#172:                               # %for.cond681
                                        #   in Loop: Header=BB2_173 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$16, %rdx
	cmpq	%rdi, %rsi
	jl	.LBB2_173
.LBB2_174:                              # %for.end691
	movq	%r15, %r13
	andq	%r8, %r13
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	%r13, %rcx
	jne	.LBB2_178
# BB#175:                               # %for.cond695.preheader
	movq	$1, %r12
	testq	%r14, %r14
	je	.LBB2_180
# BB#176:                               # %for.body698.preheader
	leaq	136(%rsp), %rax
	movq	$1, %r12
	xorq	%rcx, %rcx
	movq	$1, %rdx
	.align	16, 0x90
.LBB2_177:                              # %for.body698
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, (%rax)
	movq	%rdx, %rsi
	andq	%r8, %rsi
	incq	%rdx
	addq	$16, %rax
	cmpq	%r13, %rsi
	jne	.LBB2_177
	jmp	.LBB2_180
.LBB2_178:                              # %if.else705
	xorq	%r12, %r12
	testq	%r8, %rax
	je	.LBB2_180
# BB#179:                               # %if.then708
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	128(%rsp,%rax), %rcx
	movq	128(%rsp), %rdx
	movq	136(%rsp,%rax), %rsi
	movq	%rsi, 136(%rsp)
	movq	%rcx, 128(%rsp)
	movq	%rdx, 128(%rsp,%rax)
	movq	$1, %rcx
	movl	%ecx, 136(%rsp,%rax)
.LBB2_180:                              # %if.end721
	movq	128(%rsp), %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$2, %r15
	leaq	152(%rsp), %rbx
	jl	.LBB2_185
# BB#181:                               # %for.body727.preheader
	movq	$2, %r15
	.align	16, 0x90
.LBB2_182:                              # %for.body727
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %r14
	movl	(%rbx), %ecx
	testq	%rcx, %rcx
	movq	$76, %rdi
	jne	.LBB2_184
# BB#183:                               # %select.mid
                                        #   in Loop: Header=BB2_182 Depth=1
	movq	$75, %rdi
.LBB2_184:                              # %select.end
                                        #   in Loop: Header=BB2_182 Depth=1
	movq	-8(%rbx), %rcx
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%r15, %rcx
	movq	%r14, %r8
	andq	%r8, %rcx
	incq	%r15
	addq	$16, %rbx
	cmpq	%r13, %rcx
	jne	.LBB2_182
.LBB2_185:                              # %for.end738
	testq	%r12, %r12
	je	.LBB2_187
# BB#186:                               # %cond.true
	movq	$77, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
.LBB2_187:                              # %cleanup
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	simplify_plus_minus, .Ltmp52-simplify_plus_minus
	.cfi_endproc

	.globl	avoid_constant_pool_reference
	.align	16, 0x90
	.type	avoid_constant_pool_reference,@function
avoid_constant_pool_reference:          # @avoid_constant_pool_reference
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
.Ltmp60:
	.cfi_offset %rbx, -32
.Ltmp61:
	.cfi_offset %r14, -24
.Ltmp62:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB3_7
# BB#1:                                 # %if.end
	movq	8(%rbx), %r15
	movl	(%r15), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	jne	.LBB3_7
# BB#2:                                 # %if.end7
	movq	%r15, %rdi
	callq	get_pool_constant
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	get_pool_mode
	movzbl	2(%rbx), %edi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rdi, %rcx
	jne	.LBB3_4
# BB#3:
	movq	%r14, %rbx
	jmp	.LBB3_7
.LBB3_4:                                # %if.then13
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	simplify_subreg
	testq	%rax, %rax
	jne	.LBB3_6
# BB#5:                                 # %if.then13
	movq	%rbx, %rax
.LBB3_6:                                # %if.then13
	movq	%rax, %rbx
.LBB3_7:                                # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp63:
	.size	avoid_constant_pool_reference, .Ltmp63-avoid_constant_pool_reference
	.cfi_endproc

	.globl	simplify_subreg
	.align	16, 0x90
	.type	simplify_subreg,@function
simplify_subreg:                        # @simplify_subreg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp72:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp73:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp74:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp75:
	.cfi_def_cfa_offset 112
.Ltmp76:
	.cfi_offset %rbx, -48
.Ltmp77:
	.cfi_offset %r12, -40
.Ltmp78:
	.cfi_offset %r13, -32
.Ltmp79:
	.cfi_offset %r14, -24
.Ltmp80:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r11
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %r11
	je	.LBB4_4
# BB#1:                                 # %entry
	testq	%r9, %rdi
	je	.LBB4_4
# BB#2:                                 # %entry
	movq	%r11, %r12
	andq	%r9, %r12
	cmpq	$51, %r12
	je	.LBB4_4
# BB#3:                                 # %entry
	movq	%rdi, %r14
	andq	%r9, %r14
	cmpq	$51, %r14
	je	.LBB4_4
# BB#6:                                 # %if.end
	movl	(%rsi), %r8d
	movq	$16, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	%r12, %rax
	je	.LBB4_9
# BB#7:                                 # %if.end
	testq	%rax, %rax
	jne	.LBB4_8
.LBB4_9:                                # %if.end12
	movq	$32, %rcx
	movq	%rdi, %r13
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movzbl	mode_size(%r13), %r10d
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	andq	%r9, %rbx
	xorq	%rdx, %rdx
	movq	%rbx, %rax
	divq	%r10
	testq	%rdx, %rdx
	jne	.LBB4_11
# BB#10:                                # %lor.lhs.false13
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %r15d
	cmpq	%rbx, %r15
	jbe	.LBB4_11
# BB#12:                                # %if.end20
	cmpq	%r12, %r14
	jne	.LBB4_15
# BB#13:                                # %if.end20
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r9, %rax
	jne	.LBB4_15
# BB#14:
	movq	%rsi, %rbx
.LBB4_81:                               # %return
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_15:                               # %if.end26
	movq	%rdx, %r12
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%r8, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB4_24
# BB#16:                                # %if.end26
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	addq	$-54, %rcx
	andq	%r9, %rcx
	cmpq	$14, %rcx
	ja	.LBB4_18
# BB#17:                                # %if.end26
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB4_26
	jmp	.LBB4_18
.LBB4_24:                               # %if.end26
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	cmpq	$134, %rcx
	je	.LBB4_26
# BB#25:                                # %if.end26
	cmpq	$140, %rcx
	jne	.LBB4_18
.LBB4_26:                               # %if.then66
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r11, %rsi
	movq	%r11, 16(%rsp)          # 8-byte Spill
	callq	subreg_lowpart_offset
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	40(%rsp), %r9           # 8-byte Reload
	movq	24(%rsp), %r10          # 8-byte Reload
	jne	.LBB4_28
# BB#27:                                # %if.then69
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %rsi
	movq	%r10, %rbx
	callq	gen_lowpart_if_possible
	movq	%rbx, %r10
	movq	40(%rsp), %r9           # 8-byte Reload
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB4_81
.LBB4_28:                               # %if.end74
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	cmpq	%rcx, %r10
	jne	.LBB4_32
# BB#29:                                # %if.end74
	cmpq	%r10, %r15
	jbe	.LBB4_32
# BB#30:                                # %land.lhs.true90
	movl	mode_class(,%r13,4), %eax
	cmpq	$1, %rax
	jne	.LBB4_32
# BB#31:                                # %if.then95
	xorq	%rdx, %rdx
	movq	32(%rsp), %rax          # 8-byte Reload
	divq	%r10
	movq	%r9, %rdi
	movq	%rax, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r9, %r15
	movq	%r10, %rbx
	callq	constant_subword
	movq	%rbx, %r10
	movq	%r15, %r9
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB4_81
.LBB4_32:                               # %if.end104
	movl	(%r9), %r8d
	movq	$3, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r15
	shlq	%cl, %r15
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB4_34
# BB#33:
	xorq	%r14, %r14
.LBB4_42:                               # %sw.bb147
	andq	$65535, %r8             # imm = 0xFFFF
	cmpq	$54, %r8
	jne	.LBB4_44
# BB#43:                                # %if.then152
	movq	8(%r9), %r14
.LBB4_44:                               # %if.end156
	movl	mode_class(,%r13,4), %eax
	xorq	%rbx, %rbx
	cmpq	$1, %rax
	jne	.LBB4_81
# BB#45:                                # %if.end162
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$64, %rax
	movq	%r14, %rsi
	jl	.LBB4_50
# BB#46:                                # %if.then165
	testq	%rsi, %rsi
	js	.LBB4_47
# BB#48:                                # %if.then165
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB4_81
.LBB4_34:                               # %if.end104
	movq	%r9, 40(%rsp)           # 8-byte Spill
	cmpq	$55, %rax
	jne	.LBB4_35
# BB#36:                                # %sw.bb
	movq	%r10, 24(%rsp)          # 8-byte Spill
	testq	$16711680, %r8          # imm = 0xFF0000
	movq	16(%rsp), %r11          # 8-byte Reload
	jne	.LBB4_18
# BB#37:                                # %if.end113
	movzbl	mode_bitsize(%r13,%r13), %eax
	xorq	%rbx, %rbx
	cmpq	$63, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	ja	.LBB4_81
# BB#38:                                # %if.end120
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$63, %rax
	jg	.LBB4_39
# BB#40:                                # %if.end120
	leaq	16(%rdx), %rcx
	jmp	.LBB4_41
.LBB4_35:
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %r11          # 8-byte Reload
.LBB4_18:                               # %if.end181
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_19
# BB#60:                                # %land.lhs.true318
	testq	$1073741824, %r8        # imm = 0x40000000
	movq	40(%rsp), %r9           # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB4_62
# BB#61:                                # %land.lhs.true318
	movl	rtx_equal_function_value_matters(%rip), %eax
	testq	%rdx, %rax
	jne	.LBB4_67
.LBB4_62:                               # %land.lhs.true325
	movl	8(%r9), %ecx
	cmpq	$52, %rcx
	movq	56(%rsp), %rsi          # 8-byte Reload
	ja	.LBB4_67
# BB#63:                                # %land.lhs.true331
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_65
# BB#64:                                # %land.lhs.true331
	movl	frame_pointer_needed(%rip), %eax
	testq	%rdx, %rax
	jne	.LBB4_65
# BB#74:                                # %land.lhs.true347
	cmpq	$7, %rcx
	je	.LBB4_67
# BB#75:                                # %land.lhs.true347
	movq	%rsi, %rdx
	cmpq	$16, %rcx
	je	.LBB4_67
	jmp	.LBB4_76
.LBB4_19:                               # %if.end181
	cmpq	$63, %rax
	movq	40(%rsp), %r9           # 8-byte Reload
	jne	.LBB4_67
# BB#20:                                # %if.then186
	movq	8(%r9), %rsi
	movzbl	2(%rsi), %r8d
	movl	16(%r9), %r12d
	cmpq	%r14, %r8
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rax          # 8-byte Reload
	jne	.LBB4_53
# BB#21:                                # %if.then186
	testq	%rdi, %rax
	jne	.LBB4_53
# BB#22:                                # %if.then186
	testq	%rdi, %r12
	jne	.LBB4_53
# BB#23:
	movq	%rsi, %rbx
	jmp	.LBB4_81
.LBB4_53:                               # %if.end210
	movq	48(%rsp), %r15          # 8-byte Reload
	movzbl	mode_size(%r8), %r10d
	addq	%rax, %r12
	movq	24(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r10
	jbe	.LBB4_57
# BB#54:                                # %if.then262
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	js	.LBB4_81
# BB#55:                                # %if.end266
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	24(%rsp)                # 8-byte Folded Reload
	testq	%rdi, %rdx
	jne	.LBB4_81
# BB#56:                                # %if.end266
	andq	%r12, %rdi
	cmpq	%r10, %rdi
	jb	.LBB4_58
	jmp	.LBB4_81
.LBB4_50:                               # %if.else
	movzbl	mode_bitsize(%r13,%r13), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r15
	movq	%r15, %rcx
	shrq	%cl, %rsi
	cmpq	$63, %rax
	ja	.LBB4_52
# BB#51:                                # %if.then177
	movq	%rsi, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	trunc_int_for_mode
	movq	%rax, %rsi
.LBB4_52:                               # %if.end179
	xorq	%rdi, %rdi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_CONST_INT  # TAILCALL
.LBB4_65:                               # %lor.lhs.false335
	movq	%rsi, %rdx
	cmpq	$20, %rcx
	ja	.LBB4_76
# BB#66:                                # %lor.lhs.false335
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$1114304, %rax          # imm = 0x1100C0
	jne	.LBB4_67
.LBB4_76:                               # %if.then359
	movq	%r11, 16(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r9, %rsi
	movq	%r9, %r13
	callq	gen_rtx_SUBREG
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	subreg_hard_regno
	movq	%rbx, %rsi
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%rsi, %r15
	callq	ix86_hard_regno_mode_ok
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_79
# BB#77:                                # %lor.lhs.false364
	movl	8(%r13), %edi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	ix86_hard_regno_mode_ok
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_79
# BB#78:                                # %lor.lhs.false364.if.end384_crit_edge
	movl	(%r13), %r8d
	movq	%r13, %r9
	movq	16(%rsp), %r11          # 8-byte Reload
.LBB4_67:                               # %if.end384
	movq	%r11, %r14
	andq	$65535, %r8             # imm = 0xFFFF
	cmpq	$66, %r8
	jne	.LBB4_82
# BB#68:                                # %land.lhs.true389
	movq	8(%r9), %rdi
	movq	%r9, %rbx
	callq	mode_dependent_address_p
	movq	%rbx, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_82
# BB#69:                                # %land.lhs.true395
	movl	(%r9), %eax
	testq	$134217728, %rax        # imm = 0x8000000
	je	.LBB4_72
# BB#70:                                # %lor.lhs.false400
	movq	$47, %rdi
	movq	%r14, %rsi
	movq	%r9, %rbx
	callq	have_insn_for
	movq	%rbx, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_82
# BB#71:                                # %lor.lhs.false400.land.lhs.true403_crit_edge
	movl	(%r9), %eax
.LBB4_72:                               # %land.lhs.true403
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, 24(%rsp)          # 8-byte Folded Reload
	jbe	.LBB4_73
.LBB4_82:                               # %if.end418
	movq	48(%rsp), %rdx          # 8-byte Reload
	movzbl	(%r9), %eax
	xorq	%rbx, %rbx
	cmpq	$65, %rax
	jne	.LBB4_81
# BB#83:                                # %if.then423
	movzbl	mode_unit_size(%r12), %ecx
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rcx
	ja	.LBB4_84
# BB#85:                                # %if.then423
	addq	$16, %r9
	jmp	.LBB4_86
.LBB4_84:
	addq	$8, %r9
.LBB4_86:                               # %if.then423
	movq	%rdx, %rdi
	movq	(%r9), %r12
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rdx, %r14
	movzbl	2(%r12), %edx
	movq	%rdi, %r15
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	simplify_subreg
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB4_81
# BB#87:                                # %if.end452
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
.LBB4_88:                               # %if.end452
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_SUBREG  # TAILCALL
.LBB4_57:                               # %if.else280
	xorq	%rbx, %rbx
	testq	%rdi, %r12
	movabsq	$0, %r12
	jne	.LBB4_81
.LBB4_58:                               # %if.end295
	movq	%r15, %rdi
	movq	%r8, %rdx
	movq	%r12, %rcx
	movq	%r9, %rbx
	callq	simplify_subreg
	movq	%rbx, %rcx
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB4_81
# BB#59:                                # %if.end308
	movq	8(%rcx), %rsi
	movq	%r15, %rdi
	movq	%r12, %rdx
	jmp	.LBB4_88
.LBB4_47:
	movabsq	$const_int_rtx+504, %rax
	movq	(%rax), %rbx
	jmp	.LBB4_81
.LBB4_79:                               # %if.then370
	movq	%r13, %r12
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rcx, %r14
	callq	gen_rtx_REG
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	subreg_lowpart_offset
	andq	%r14, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB4_81
# BB#80:                                # %if.then375
	movl	16(%r12), %eax
	movl	%eax, 16(%rbx)
	jmp	.LBB4_81
.LBB4_73:                               # %if.then415
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r9, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	adjust_address_1  # TAILCALL
.LBB4_39:
	leaq	24(%rdx), %rcx
.LBB4_41:                               # %if.end120
	movq	%rdx, %rbx
	movq	(%rcx), %r14
	movq	$57, %rcx
	shrq	%cl, %rax
	andq	$63, %rax
	addq	%r15, %rax
	andq	$-64, %rax
	subq	%rax, %r15
	movq	$64, %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movl	(%rbx), %r8d
	movq	%rbx, %r9
	jmp	.LBB4_42
.LBB4_4:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_subreg, %rcx
	movq	$2488, %rsi             # imm = 0x9B8
	jmp	.LBB4_5
.LBB4_8:                                # %if.then11
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_subreg, %rcx
	movq	$2492, %rsi             # imm = 0x9BC
	jmp	.LBB4_5
.LBB4_11:                               # %if.then19
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_subreg, %rcx
	movq	$2496, %rsi             # imm = 0x9C0
.LBB4_5:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp81:
	.size	simplify_subreg, .Ltmp81-simplify_subreg
	.cfi_endproc

	.globl	simplify_gen_unary
	.align	16, 0x90
	.type	simplify_gen_unary,@function
simplify_gen_unary:                     # @simplify_gen_unary
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp88:
	.cfi_def_cfa_offset 32
.Ltmp89:
	.cfi_offset %rbx, -32
.Ltmp90:
	.cfi_offset %r14, -24
.Ltmp91:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R15<kill>
                                        # kill: RDX<def> R14<kill>
	callq	simplify_unary_operation
	testq	%rax, %rax
	je	.LBB5_2
# BB#1:                                 # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB5_2:                                # %if.end
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_e  # TAILCALL
.Ltmp92:
	.size	simplify_gen_unary, .Ltmp92-simplify_gen_unary
	.cfi_endproc

	.globl	simplify_unary_operation
	.align	16, 0x90
	.type	simplify_unary_operation,@function
simplify_unary_operation:               # @simplify_unary_operation
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
	subq	$224, %rsp
.Ltmp104:
	.cfi_def_cfa_offset 272
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
	movq	%rcx, %r9
	movq	%rdx, %r13
	movq	%rsi, %r8
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%r8, %r12
	movzbl	(%r13), %eax
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	$66, %rax
	movq	%r13, %r15
	jne	.LBB6_6
# BB#1:                                 # %if.end.i
	movq	8(%r13), %rbx
	movl	(%rbx), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	%r13, %r15
	jne	.LBB6_6
# BB#2:                                 # %if.end7.i
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	callq	get_pool_constant
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	get_pool_mode
	movzbl	2(%r13), %edi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rdi, %rcx
	je	.LBB6_5
# BB#3:                                 # %if.then13.i
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	simplify_subreg
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB6_5
# BB#4:                                 # %if.then13.i
	movq	%r13, %r15
.LBB6_5:                                # %if.then13.i
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
.LBB6_6:                                # %avoid_constant_pool_reference.exit
	movzbl	mode_bitsize(%r12,%r12), %edx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r14, %rax
	andq	%rbx, %rax
	cmpq	$125, %rax
	jne	.LBB6_7
# BB#17:                                # %land.lhs.true
	movl	(%r15), %edi
	movq	%rdi, %rcx
	andq	$16777214, %rcx         # imm = 0xFFFFFE
	cmpq	$54, %rcx
	jne	.LBB6_9
# BB#18:                                # %if.then
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$54, %rdi
	jne	.LBB6_20
# BB#19:                                # %if.then17
	movq	8(%r15), %rsi
	movq	$63, %rcx
	movq	%rsi, %rdx
	sarq	%cl, %rdx
	jmp	.LBB6_21
.LBB6_7:                                # %avoid_constant_pool_reference.exit
	cmpq	$127, %rax
	jne	.LBB6_8
# BB#23:                                # %land.lhs.true31
	movl	(%r15), %edi
	movq	%rdi, %rcx
	andq	$16777214, %rcx         # imm = 0xFFFFFE
	cmpq	$54, %rcx
	jne	.LBB6_9
# BB#24:                                # %if.then47
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$54, %rdi
	jne	.LBB6_26
# BB#25:                                # %if.then58
	movq	8(%r15), %rsi
	movq	$63, %rcx
	movq	%rsi, %rdx
	sarq	%cl, %rdx
	jmp	.LBB6_27
.LBB6_8:                                # %avoid_constant_pool_reference.exit.if.end95_crit_edge
	movl	(%r15), %edi
.LBB6_9:                                # %if.end95
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB6_56
# BB#10:                                # %if.end95
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$64, %rsi
	ja	.LBB6_56
# BB#11:                                # %if.end95
	testq	$65535, %rdx            # imm = 0xFFFF
	je	.LBB6_56
# BB#12:                                # %if.then106
	xorq	%r13, %r13
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$149, %rdx
	jg	.LBB6_52
# BB#13:                                # %if.then106
	movq	8(%r15), %rdi
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB6_32
# BB#14:                                # %if.then106
	cmpq	$77, %rax
	jne	.LBB6_15
# BB#54:                                # %sw.bb112
	negq	%rdi
	jmp	.LBB6_55
.LBB6_56:                               # %if.else213
	testq	$16711680, %rdi         # imm = 0xFF0000
	jne	.LBB6_87
# BB#57:                                # %if.else213
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$128, %rsi
	ja	.LBB6_87
# BB#58:                                # %if.else213
	movq	%rdi, %rsi
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$54, %rsi
	jne	.LBB6_87
# BB#59:                                # %if.then232
	cmpq	$55, %rcx
	jne	.LBB6_61
# BB#60:                                # %if.then243
	movq	16(%r15), %rdi
	movq	24(%r15), %rsi
	jmp	.LBB6_62
.LBB6_52:                               # %if.then106
	addq	$-150, %r14
	andq	%rbx, %r14
	cmpq	$2, %r14
	jb	.LBB6_114
	jmp	.LBB6_53
.LBB6_87:                               # %if.else348
	cmpq	$55, %rcx
	jne	.LBB6_97
# BB#88:                                # %land.lhs.true353
	movl	mode_class(,%r12,4), %esi
	cmpq	$2, %rsi
	jne	.LBB6_91
# BB#89:                                # %if.then358
	movq	%r15, 80(%rsp)
	movl	%r8d, 96(%rsp)
	movl	%r14d, 100(%rsp)
	xorq	%r13, %r13
	movb	%r13b, 104(%rsp)
	movabsq	$simplify_unary_real, %rdi
	leaq	80(%rsp), %rsi
	callq	do_float_handler
	testq	%rbx, %rax
	je	.LBB6_114
# BB#90:                                # %if.then364
	movq	88(%rsp), %r13
	jmp	.LBB6_114
.LBB6_20:                               # %if.else
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
.LBB6_21:                               # %if.end
	leaq	200(%rsp), %rdi
	movq	%r8, %rcx
	movq	%r8, %rbx
	callq	ereal_from_int
	movq	216(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	200(%rsp), %rax
	movq	208(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	176(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	%rbx, %rsi
	callq	real_value_truncate
	movq	192(%rsp), %rax
	movq	176(%rsp), %rcx
	movq	184(%rsp), %rdx
	movq	%rax, 216(%rsp)
	movq	%rdx, 208(%rsp)
	movq	%rcx, 200(%rsp)
	movq	192(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	176(%rsp), %rax
	movq	184(%rsp), %rcx
	jmp	.LBB6_22
.LBB6_32:                               # %if.then106
	addq	$-120, %r14
	andq	%rbx, %r14
	cmpq	$11, %r14
	ja	.LBB6_53
# BB#33:                                # %if.then106
	jmpq	*.LJTI6_1(,%r14,8)
.LBB6_44:                               # %sw.bb160
	testq	%rbx, %r9
	movq	%r8, %rax
	je	.LBB6_46
# BB#45:                                # %sw.bb160
	movq	%r9, %rax
.LBB6_46:                               # %sw.bb160
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	$64, %rax
	jne	.LBB6_49
# BB#47:                                # %if.then170
	cmpq	$64, %rsi
	je	.LBB6_55
# BB#48:                                # %if.then176
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_unary_operation, %rcx
	movq	$557, %rsi              # imm = 0x22D
	jmp	.LBB6_38
.LBB6_61:                               # %if.else250
	movq	8(%r15), %rdi
	movq	$63, %rcx
	movq	%rdi, %rsi
	sarq	%cl, %rsi
.LBB6_62:                               # %if.end257
	xorq	%r13, %r13
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$85, %rdx
	jg	.LBB6_65
# BB#63:                                # %if.end257
	cmpq	$77, %rax
	jne	.LBB6_114
	jmp	.LBB6_64
.LBB6_65:                               # %if.end257
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB6_68
# BB#66:                                # %if.end257
	cmpq	$86, %rax
	jne	.LBB6_114
# BB#67:                                # %sw.bb258
	notq	%rdi
	movq	%rdi, 120(%rsp)
	notq	%rsi
	movq	%rsi, 112(%rsp)
	jmp	.LBB6_86
.LBB6_26:                               # %if.else65
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
.LBB6_27:                               # %if.end72
	testq	%rbx, %r9
	je	.LBB6_28
# BB#29:                                # %if.else80
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movzbl	mode_bitsize(%r9,%r9), %eax
	cmpq	$127, %rax
	ja	.LBB6_31
# BB#30:                                # %if.else87
	movq	mode_mask_array(,%r9,8), %rax
	andq	%rax, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB6_31
.LBB6_28:                               # %if.then75
	xorq	%r13, %r13
	testq	%rdx, %rdx
	js	.LBB6_114
.LBB6_31:                               # %if.end91
	leaq	152(%rsp), %rdi
	movq	%r8, %rcx
	movq	%r8, %rbx
	callq	ereal_from_uint
	movq	168(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	152(%rsp), %rax
	movq	160(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	128(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	%rbx, %rsi
	callq	real_value_truncate
	movq	144(%rsp), %rax
	movq	128(%rsp), %rcx
	movq	136(%rsp), %rdx
	movq	%rax, 168(%rsp)
	movq	%rdx, 160(%rsp)
	movq	%rcx, 152(%rsp)
	movq	144(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	128(%rsp), %rax
	movq	136(%rsp), %rcx
.LBB6_22:                               # %return
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	callq	immed_real_const_1
	movq	%rax, %r13
	jmp	.LBB6_114
.LBB6_15:                               # %if.then106
	cmpq	$86, %rax
	jne	.LBB6_53
# BB#16:                                # %sw.bb
	notq	%rdi
	jmp	.LBB6_55
.LBB6_91:                               # %land.lhs.true371
	movq	$14, %rcx
	shrq	%cl, %rdi
	andq	$1020, %rdi             # imm = 0x3FC
	movl	mode_class(%rdi), %ecx
	cmpq	$2, %rcx
	jne	.LBB6_97
# BB#92:                                # %land.lhs.true379
	cmpq	$1, %rsi
	jne	.LBB6_97
# BB#93:                                # %land.lhs.true379
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$64, %rcx
	ja	.LBB6_97
# BB#94:                                # %land.lhs.true379
	testq	$65535, %rdx            # imm = 0xFFFF
	je	.LBB6_97
# BB#95:                                # %if.then390
	movq	%r15, 48(%rsp)
	movl	%r8d, 64(%rsp)
	movl	%r14d, 68(%rsp)
	movq	$1, %rax
	movb	%al, 72(%rsp)
	movabsq	$simplify_unary_real, %rdi
	leaq	48(%rsp), %rsi
	callq	do_float_handler
	xorq	%r13, %r13
	testq	%rbx, %rax
	je	.LBB6_114
# BB#96:                                # %if.then399
	movq	56(%rsp), %r13
	jmp	.LBB6_114
.LBB6_97:                               # %if.else402
	cmpq	$77, %rax
	je	.LBB6_106
# BB#98:                                # %if.else402
	cmpq	$120, %rax
	jne	.LBB6_99
# BB#108:                               # %sw.bb446
	movzbl	(%r13), %eax
	cmpq	$122, %rax
	jne	.LBB6_113
# BB#109:                               # %land.lhs.true451
	movq	8(%r13), %r13
	movl	(%r13), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	andq	%rbx, %r8
	cmpq	%r8, %rdx
	jne	.LBB6_113
# BB#110:                               # %land.lhs.true451
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$76, %rax
	jne	.LBB6_113
# BB#111:                               # %land.lhs.true468
	movq	8(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB6_113
# BB#112:                               # %land.lhs.true479
	movq	16(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$67, %rax
	je	.LBB6_114
	jmp	.LBB6_113
.LBB6_68:                               # %if.end257
	addq	$-120, %r14
	andq	%rbx, %r14
	cmpq	$11, %r14
	ja	.LBB6_114
# BB#69:                                # %if.end257
	jmpq	*.LJTI6_0(,%r14,8)
.LBB6_80:                               # %sw.bb303
	testq	%rbx, %r9
	je	.LBB6_114
# BB#81:                                # %lor.lhs.false306
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movzbl	mode_bitsize(%r9,%r9), %eax
	cmpq	$64, %rax
	ja	.LBB6_114
# BB#82:                                # %if.else313
	movq	mode_mask_array(,%r9,8), %rdx
	andq	%rdi, %rdx
	movq	%rdx, 120(%rsp)
	cmpq	$63, %rax
	ja	.LBB6_85
# BB#83:                                # %land.lhs.true322
	addq	%rax, %rbx
	movq	$1, %rsi
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	testq	%rdx, %rdi
	je	.LBB6_85
# BB#84:                                # %if.then332
	movq	%rax, %rcx
	shlq	%cl, %rsi
	subq	%rsi, %rdx
	movq	%rdx, 120(%rsp)
.LBB6_85:                               # %if.end339
	movq	$63, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 112(%rsp)
	jmp	.LBB6_86
.LBB6_106:                              # %sw.bb436
	movzbl	(%r13), %eax
	cmpq	$77, %rax
	jne	.LBB6_113
# BB#107:                               # %if.then441
	movq	8(%r13), %r13
	jmp	.LBB6_114
.LBB6_99:                               # %if.else402
	cmpq	$86, %rax
	jne	.LBB6_113
# BB#100:                               # %sw.bb404
	movzbl	(%r13), %eax
	cmpq	$86, %rax
	jne	.LBB6_102
# BB#101:                               # %if.then409
	movq	8(%r13), %r13
	jmp	.LBB6_114
.LBB6_36:                               # %sw.bb127
	testq	%rbx, %r9
	je	.LBB6_37
# BB#39:                                # %if.end131
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movzbl	mode_bitsize(%r9,%r9), %ecx
	cmpq	$64, %rcx
	jne	.LBB6_42
# BB#40:                                # %if.then137
	cmpq	$64, %rsi
	je	.LBB6_55
# BB#41:                                # %if.then143
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_unary_operation, %rcx
	movq	$539, %rsi              # imm = 0x21B
	jmp	.LBB6_38
.LBB6_34:                               # %sw.bb113
	movq	$63, %rcx
	movq	%rdi, %rax
	sarq	%cl, %rax
	addq	%rax, %rdi
	xorq	%rax, %rdi
	jmp	.LBB6_55
.LBB6_35:                               # %sw.bb118
	movq	mode_mask_array(,%r12,8), %rax
	andq	%rdi, %rax
	movq	%rax, %rdi
	negq	%rdi
	andq	%rax, %rdi
	movq	%r8, %rbx
	callq	exact_log2_wide
	movq	%rbx, %r8
	movq	%rax, %rdi
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	jmp	.LBB6_55
.LBB6_102:                              # %if.end412
	andq	%rbx, %r8
	cmpq	$1, %r8
	jne	.LBB6_113
# BB#103:                               # %land.lhs.true415
	movzbl	rtx_class(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB6_113
# BB#104:                               # %land.lhs.true423
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%r9, %r14
	callq	reversed_comparison_code
	movq	%r14, %rsi
	testq	%rbx, %rax
	je	.LBB6_113
# BB#105:                               # %if.then427
	movq	8(%r13), %rdx
	movq	16(%r13), %rcx
	movq	%rax, %rdi
	callq	gen_rtx_fmt_ee
	movq	%rax, %r13
	jmp	.LBB6_114
.LBB6_113:                              # %sw.epilog496
	xorq	%r13, %r13
	jmp	.LBB6_114
.LBB6_49:                               # %if.else178
	cmpq	$63, %rax
	ja	.LBB6_114
# BB#50:                                # %if.then184
	movq	$-1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, %rdi
	addq	%rax, %rbx
	movq	$1, %rdx
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB6_55
# BB#51:                                # %if.then199
	movq	%rax, %rcx
	shlq	%cl, %rdx
	subq	%rdx, %rdi
	jmp	.LBB6_55
.LBB6_53:                               # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.simplify_unary_operation, %rdx
	movq	$580, %rsi              # imm = 0x244
	callq	fancy_abort
.LBB6_42:                               # %if.else145
	cmpq	$63, %rcx
	ja	.LBB6_114
# BB#43:                                # %if.then151
	movq	$-1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdi
.LBB6_55:                               # %sw.epilog
	movq	%r8, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r13
	jmp	.LBB6_114
.LBB6_76:                               # %sw.bb288
	testq	%rbx, %r9
	je	.LBB6_77
# BB#78:                                # %if.end292
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movzbl	mode_bitsize(%r9,%r9), %eax
	cmpq	$64, %rax
	ja	.LBB6_114
# BB#79:                                # %if.end299
	movq	mode_mask_array(,%r9,8), %rax
	movq	$0, 112(%rsp)
	andq	%rdi, %rax
	movq	%rax, 120(%rsp)
	jmp	.LBB6_86
.LBB6_70:                               # %sw.bb263
	testq	%rsi, %rsi
	js	.LBB6_64
.LBB6_71:                               # %if.else268
	movq	%rdi, 120(%rsp)
	movq	%rsi, 112(%rsp)
	jmp	.LBB6_86
.LBB6_64:                               # %sw.bb261
	leaq	120(%rsp), %rdx
	leaq	112(%rsp), %rcx
	movq	%r8, %rbx
	callq	neg_double
	movq	%rbx, %r8
.LBB6_86:                               # %sw.epilog346
	movq	120(%rsp), %rdi
	movq	112(%rsp), %rsi
	movq	%r8, %rdx
	callq	immed_double_const
	movq	%rax, %r13
.LBB6_114:                              # %return
	movq	%r13, %rax
	addq	$224, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_72:                               # %sw.bb270
	movq	%r8, %rbx
	movq	$0, 112(%rsp)
	testq	%rdi, %rdi
	je	.LBB6_73
# BB#75:                                # %if.else280
	movq	%rdi, %rax
	negq	%rax
	andq	%rdi, %rax
	movq	%rax, %rdi
	callq	exact_log2_wide
	incq	%rax
	jmp	.LBB6_74
.LBB6_73:                               # %if.then273
	movq	%rsi, %rdi
	negq	%rdi
	andq	%rsi, %rdi
	callq	exact_log2_wide
	addq	$65, %rax
.LBB6_74:                               # %sw.epilog346
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 120(%rsp)
	movq	%rbx, %r8
	jmp	.LBB6_86
.LBB6_37:                               # %if.then130
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_unary_operation, %rcx
	movq	$532, %rsi              # imm = 0x214
	jmp	.LBB6_38
.LBB6_77:                               # %if.then291
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_unary_operation, %rcx
	movq	$636, %rsi              # imm = 0x27C
.LBB6_38:                               # %if.then130
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp110:
	.size	simplify_unary_operation, .Ltmp110-simplify_unary_operation
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI6_0:
	.quad	.LBB6_80
	.quad	.LBB6_76
	.quad	.LBB6_71
	.quad	.LBB6_114
	.quad	.LBB6_114
	.quad	.LBB6_114
	.quad	.LBB6_114
	.quad	.LBB6_114
	.quad	.LBB6_114
	.quad	.LBB6_70
	.quad	.LBB6_114
	.quad	.LBB6_72
.LJTI6_1:
	.quad	.LBB6_44
	.quad	.LBB6_36
	.quad	.LBB6_55
	.quad	.LBB6_114
	.quad	.LBB6_114
	.quad	.LBB6_53
	.quad	.LBB6_53
	.quad	.LBB6_53
	.quad	.LBB6_53
	.quad	.LBB6_34
	.quad	.LBB6_114
	.quad	.LBB6_35

	.text
	.globl	simplify_gen_ternary
	.align	16, 0x90
	.type	simplify_gen_ternary,@function
simplify_gen_ternary:                   # @simplify_gen_ternary
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
.Ltmp122:
	.cfi_offset %rbx, -48
.Ltmp123:
	.cfi_offset %r12, -40
.Ltmp124:
	.cfi_offset %r13, -32
.Ltmp125:
	.cfi_offset %r14, -24
.Ltmp126:
	.cfi_offset %r15, -16
	movq	%r9, %r14
	movq	%r8, %r15
	movq	%rcx, %r12
	movq	%rsi, %r13
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R13<kill>
                                        # kill: RCX<def> R12<kill>
                                        # kill: R8<def> R15<kill>
                                        # kill: R9<def> R14<kill>
	callq	simplify_ternary_operation
	testq	%rax, %rax
	je	.LBB7_2
# BB#1:                                 # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_2:                                # %if.end
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%r14, %r8
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_eee  # TAILCALL
.Ltmp127:
	.size	simplify_gen_ternary, .Ltmp127-simplify_gen_ternary
	.cfi_endproc

	.globl	simplify_ternary_operation
	.align	16, 0x90
	.type	simplify_ternary_operation,@function
simplify_ternary_operation:             # @simplify_ternary_operation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp134:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp135:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp136:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp137:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp138:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp139:
	.cfi_def_cfa_offset 64
.Ltmp140:
	.cfi_offset %rbx, -48
.Ltmp141:
	.cfi_offset %r12, -40
.Ltmp142:
	.cfi_offset %r13, -32
.Ltmp143:
	.cfi_offset %r14, -24
.Ltmp144:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movq	%rcx, %r13
	movq	%rsi, %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_bitsize(%rbx,%rbx), %esi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rdi, %rcx
	andq	%r15, %rcx
	testq	%rsi, %rsi
	movq	$64, %rax
	je	.LBB8_2
# BB#1:                                 # %entry
	movq	%rsi, %rax
.LBB8_2:                                # %entry
	cmpq	$72, %rcx
	je	.LBB8_17
# BB#3:                                 # %entry
	leaq	-132(%rdi), %rcx
	andq	%r15, %rcx
	movq	%r15, %rbx
	cmpq	$1, %rcx
	ja	.LBB8_59
# BB#4:                                 # %sw.bb
	movzbl	(%r13), %ecx
	xorq	%r15, %r15
	cmpq	$54, %rcx
	jne	.LBB8_22
# BB#5:                                 # %land.lhs.true
	movzbl	(%r12), %ecx
	cmpq	$54, %rcx
	jne	.LBB8_22
# BB#6:                                 # %land.lhs.true8
	movzbl	(%r9), %ecx
	cmpq	$54, %rcx
	jne	.LBB8_22
# BB#7:                                 # %land.lhs.true13
	movq	8(%r12), %r11
	movq	8(%r9), %rcx
	leaq	(%rcx,%r11), %rsi
	andq	%rbx, %rsi
	movq	%rax, %rdx
	andq	%rbx, %rdx
	movq	%rbx, %r10
	cmpq	%rdx, %rsi
	ja	.LBB8_22
# BB#8:                                 # %land.lhs.true13
	cmpq	$64, %rdx
	ja	.LBB8_22
# BB#9:                                 # %if.then25
	movq	8(%r13), %rsi
	sarq	%cl, %rsi
	cmpq	$64, %r11
	je	.LBB8_13
# BB#10:                                # %if.then37
	movq	$1, %r9
	movq	$1, %r8
	movq	%r11, %rcx
	shlq	%cl, %r8
	leaq	-1(%r8), %rcx
	andq	%rcx, %rsi
	andq	%r10, %rdi
	cmpq	$132, %rdi
	jne	.LBB8_13
# BB#11:                                # %land.lhs.true43
	decq	%r11
	movq	%r11, %rcx
	shlq	%cl, %r9
	testq	%r9, %rsi
	je	.LBB8_13
# BB#12:                                # %if.then50
	negq	%r8
	orq	%r8, %rsi
.LBB8_13:                               # %if.end57
	cmpq	$63, %rdx
	ja	.LBB8_16
# BB#14:                                # %land.lhs.true60
	leaq	-1(%rax), %rcx
	movq	$-1, %rdx
	shlq	%cl, %rdx
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	cmpq	%rdx, %rcx
	je	.LBB8_16
# BB#15:                                # %if.then69
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	decq	%rdx
	andq	%rdx, %rsi
.LBB8_16:                               # %if.end74
	xorq	%rdi, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_CONST_INT  # TAILCALL
.LBB8_17:                               # %sw.bb76
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movzbl	(%r13), %eax
	cmpq	$102, %rax
	jne	.LBB8_18
# BB#23:                                # %land.lhs.true89
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%r13, %rdi
	callq	side_effects_p
	testq	%r15, %rax
	jne	.LBB8_27
# BB#24:                                # %land.lhs.true92
	movl	mode_class(,%rbx,4), %eax
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_25
# BB#40:                                # %lor.lhs.false106
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB8_41
	jmp	.LBB8_27
.LBB8_18:                               # %sw.bb76
	movq	%rdx, (%rsp)            # 8-byte Spill
	cmpq	$54, %rax
	jne	.LBB8_27
# BB#19:                                # %if.then81
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%r13, %rax
	jne	.LBB8_21
# BB#20:                                # %select.mid
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB8_21
.LBB8_25:                               # %lor.lhs.false101
	andq	%r15, %rax
	cmpq	$8, %rax
	jne	.LBB8_41
# BB#26:                                # %lor.lhs.false101
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%r15, %rax
	je	.LBB8_27
.LBB8_41:                               # %land.lhs.true108
	movq	8(%r13), %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB8_27
# BB#42:                                # %land.lhs.true113
	movq	16(%r13), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB8_21
.LBB8_27:                               # %if.else
	movzbl	(%r13), %eax
	cmpq	$103, %rax
	jne	.LBB8_32
# BB#28:                                # %land.lhs.true124
	movq	%r13, %rdi
	callq	side_effects_p
	testq	%r15, %rax
	jne	.LBB8_32
# BB#29:                                # %land.lhs.true127
	movl	mode_class(,%rbx,4), %eax
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%r15, %rcx
	cmpq	$6, %rcx
	jne	.LBB8_30
# BB#43:                                # %lor.lhs.false142
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB8_44
	jmp	.LBB8_32
.LBB8_30:                               # %lor.lhs.false137
	andq	%r15, %rax
	cmpq	$8, %rax
	jne	.LBB8_44
# BB#31:                                # %lor.lhs.false137
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%r15, %rax
	je	.LBB8_32
.LBB8_44:                               # %land.lhs.true144
	movq	16(%r13), %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB8_32
# BB#45:                                # %land.lhs.true150
	movq	8(%r13), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB8_32
# BB#46:
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB8_22
.LBB8_32:                               # %if.else157
	movq	%r15, %rbx
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r15, %r15
	cmpq	$60, %rax
	jne	.LBB8_22
# BB#33:                                # %land.lhs.true165
	movq	%r13, %rdi
	callq	side_effects_p
	testq	%rbx, %rax
	jne	.LBB8_22
# BB#34:                                # %if.then168
	movq	8(%r13), %rdx
	movl	(%rdx), %edi
	movq	16(%r13), %rax
	testq	$16711680, %rdi         # imm = 0xFF0000
	jne	.LBB8_36
# BB#35:                                # %cond.true176
	movl	(%rax), %edi
.LBB8_36:                               # %cond.end190
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	je	.LBB8_38
# BB#37:                                # %cond.end190
	movq	%rdi, %rsi
.LBB8_38:                               # %cond.end190
	movzbl	(%r13), %edi
	movq	%rax, %rcx
	callq	simplify_relational_operation
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB8_39
# BB#47:                                # %if.else208
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB8_21
# BB#48:                                # %if.else212
	testq	%rax, %rax
	je	.LBB8_50
# BB#49:                                # %if.else212
	movq	%rax, %r13
.LBB8_50:                               # %if.else212
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	jne	.LBB8_22
# BB#51:                                # %land.lhs.true222
	movzbl	(%rdx), %eax
	cmpq	$54, %rax
	jne	.LBB8_22
# BB#52:                                # %if.then227
	movq	8(%r12), %rcx
	movq	8(%rdx), %rax
	cmpq	$1, %rcx
	jne	.LBB8_55
# BB#53:                                # %if.then227
	testq	%rax, %rax
	jne	.LBB8_55
# BB#54:                                # %if.then239
	movzbl	(%r13), %edi
	jmp	.LBB8_58
.LBB8_21:                               # %select.end
	movq	%r12, %r15
.LBB8_22:                               # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_39:
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB8_22
.LBB8_55:                               # %if.else242
	testq	%rcx, %rcx
	jne	.LBB8_22
# BB#56:                                # %if.else242
	cmpq	$1, %rax
	jne	.LBB8_22
# BB#57:                                # %if.then248
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%r13, %rdi
	callq	reversed_comparison_code
	movq	%rax, %rdi
	testq	%rbx, %rdi
	je	.LBB8_22
.LBB8_58:                               # %if.end256
	movq	8(%r13), %rdx
	movq	16(%r13), %rcx
	movq	%r14, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB8_59:                               # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_ternary_operation, %rcx
	movq	$2471, %rsi             # imm = 0x9A7
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp145:
	.size	simplify_ternary_operation, .Ltmp145-simplify_ternary_operation
	.cfi_endproc

	.globl	simplify_gen_relational
	.align	16, 0x90
	.type	simplify_gen_relational,@function
simplify_gen_relational:                # @simplify_gen_relational
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp153:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp155:
	.cfi_def_cfa_offset 48
.Ltmp156:
	.cfi_offset %rbx, -40
.Ltmp157:
	.cfi_offset %r12, -32
.Ltmp158:
	.cfi_offset %r14, -24
.Ltmp159:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movq	%rcx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%rdx, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	simplify_relational_operation
	testq	%rax, %rax
	je	.LBB9_1
# BB#5:                                 # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB9_1:                                # %if.end
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_2
# BB#3:                                 # %if.then2
	movq	%rbx, %rdi
	callq	swap_condition
	movq	%rax, %rbx
	movq	%r12, %rdx
	jmp	.LBB9_4
.LBB9_2:
	movq	%r15, %rdx
	movq	%r12, %r15
.LBB9_4:                                # %if.end4
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp160:
	.size	simplify_gen_relational, .Ltmp160-simplify_gen_relational
	.cfi_endproc

	.globl	simplify_relational_operation
	.align	16, 0x90
	.type	simplify_relational_operation,@function
simplify_relational_operation:          # @simplify_relational_operation
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
	subq	$64, %rsp
.Ltmp172:
	.cfi_def_cfa_offset 112
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
	movq	%rdx, %r15
	movl	(%r15), %eax
	movq	%rcx, %r12
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	jne	.LBB10_3
# BB#1:                                 # %land.lhs.true
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB10_222
# BB#2:                                 # %lor.lhs.false
	movzbl	2(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB10_222
.LBB10_3:                               # %if.end
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$74, %rcx
	jne	.LBB10_4
# BB#5:                                 # %if.end
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%r12, %rcx
	jne	.LBB10_6
# BB#7:                                 # %if.then11
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	8(%r15), %rcx
	movq	16(%r15), %r12
	movl	(%rcx), %eax
	movq	%rcx, %r15
	jmp	.LBB10_8
.LBB10_4:
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	jmp	.LBB10_8
.LBB10_6:
	movq	%rdi, 24(%rsp)          # 8-byte Spill
.LBB10_8:                               # %if.end15
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	%r15, %rdi
	jne	.LBB10_14
# BB#9:                                 # %if.end.i
	movq	8(%r15), %rbx
	movl	(%rbx), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	%r15, %rdi
	jne	.LBB10_14
# BB#10:                                # %if.end7.i
	movq	%rbx, %rdi
	callq	get_pool_constant
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	get_pool_mode
	movzbl	2(%r15), %edi
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rdi, %rcx
	jne	.LBB10_12
# BB#11:
	movq	%r14, %rdi
	jmp	.LBB10_14
.LBB10_12:                              # %if.then13.i
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	simplify_subreg
	movq	%rax, %rdi
	testq	%rdi, %rdi
	jne	.LBB10_14
# BB#13:                                # %if.then13.i
	movq	%r15, %rdi
.LBB10_14:                              # %avoid_constant_pool_reference.exit
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	movq	%r12, %r14
	jne	.LBB10_20
# BB#15:                                # %if.end.i905
	movq	8(%r12), %rbx
	movl	(%rbx), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	%r12, %r14
	jne	.LBB10_20
# BB#16:                                # %if.end7.i912
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	callq	get_pool_constant
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	get_pool_mode
	movzbl	2(%r12), %edi
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rdi, %rcx
	je	.LBB10_19
# BB#17:                                # %if.then13.i916
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	simplify_subreg
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB10_19
# BB#18:                                # %if.then13.i916
	movq	%r12, %r14
.LBB10_19:                              # %if.then13.i916
	movq	8(%rsp), %rdi           # 8-byte Reload
.LBB10_20:                              # %avoid_constant_pool_reference.exit918
	movzbl	2(%r15), %eax
	movl	mode_class(,%rax,4), %eax
	cmpq	$4, %rax
	jne	.LBB10_22
# BB#21:
	xorq	%rbx, %rbx
	jmp	.LBB10_38
.LBB10_22:                              # %if.end23
	movq	%r14, %rsi
	movq	%rdi, %r13
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB10_23
# BB#24:                                # %if.then25
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	swap_condition
	movq	%rax, %rsi
	movq	%r12, %rdx
	movq	%r14, %r9
	movq	%r13, %r8
	jmp	.LBB10_25
.LBB10_23:
	movq	%r15, %rdx
	movq	%r12, %r15
	movq	%r13, %r9
	movq	%r14, %r8
	movq	24(%rsp), %rsi          # 8-byte Reload
.LBB10_25:                              # %if.end27
	movq	$32, %rcx
	movq	16(%rsp), %r13          # 8-byte Reload
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movl	mode_class(,%r13,4), %r12d
	cmpq	$7, %r12
	ja	.LBB10_39
# BB#26:                                # %if.end27
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	$170, %rax
	je	.LBB10_39
# BB#27:                                # %land.lhs.true43
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r8
	je	.LBB10_39
# BB#28:                                # %land.lhs.true45
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	je	.LBB10_30
# BB#29:                                # %lor.lhs.false49
	movzbl	(%r9), %eax
	cmpq	$54, %rax
	jne	.LBB10_32
.LBB10_30:                              # %land.lhs.true53
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	je	.LBB10_39
# BB#31:                                # %lor.lhs.false57
	movzbl	(%r8), %eax
	cmpq	$54, %rax
	je	.LBB10_39
.LBB10_32:                              # %land.lhs.true61
	movq	$76, %rdi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r8, %r15
	movq	%rdx, %r12
	movq	%r9, %r14
	callq	simplify_binary_operation
	movq	%r14, %r9
	movq	%r12, %rdx
	movl	mode_class(,%r13,4), %r12d
	movq	%r15, %r8
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB10_39
# BB#33:                                # %land.lhs.true61
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$109, %rax
	je	.LBB10_39
# BB#34:                                # %land.lhs.true61
	cmpq	$108, %rax
	je	.LBB10_39
# BB#35:                                # %land.lhs.true61
	cmpq	$111, %rax
	je	.LBB10_39
# BB#36:                                # %land.lhs.true61
	cmpq	$110, %rax
	je	.LBB10_39
# BB#37:                                # %if.then72
	movq	%rsi, %rdi
	callq	signed_condition
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	simplify_relational_operation
	movq	%rax, %rbx
	jmp	.LBB10_38
.LBB10_39:                              # %if.end75
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_41
# BB#40:                                # %if.end75
	movq	%rsi, %rcx
	andq	%rbx, %rcx
	cmpq	$113, %rcx
	je	.LBB10_163
.LBB10_41:                              # %if.end80
	testq	%rax, %rax
	je	.LBB10_44
# BB#42:                                # %if.end80
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$112, %rax
	jne	.LBB10_44
# BB#43:                                # %if.then84
	movq	const_int_rtx+512(%rip), %rbx
	jmp	.LBB10_38
.LBB10_44:                              # %if.end85
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%r9, %rdi
	movq	%r8, %rsi
	movq	%r8, %r14
	movq	%r9, %r15
	callq	rtx_equal_p
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movl	(%rdi), %esi
	testq	%rdx, %rax
	je	.LBB10_50
# BB#45:                                # %land.lhs.true88
	movq	$14, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	jne	.LBB10_46
# BB#48:                                # %lor.lhs.false109
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB10_50
# BB#49:
	movq	$1, %r9
	xorq	%r8, %r8
	jmp	.LBB10_47
.LBB10_46:                              # %lor.lhs.false102
	andq	%rdx, %rax
	xorq	%r8, %r8
	cmpq	$8, %rax
	movq	$1, %r9
	jne	.LBB10_47
# BB#177:                               # %lor.lhs.false102
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rdx, %rax
	movabsq	$0, %rax
	movabsq	$0, %r10
	movabsq	$0, %rbx
	jne	.LBB10_178
.LBB10_50:                              # %if.else
	movq	%rsi, %r10
	andq	$65535, %r10            # imm = 0xFFFF
	cmpq	$55, %r10
	movq	%r14, %rbx
	jne	.LBB10_62
# BB#51:                                # %land.lhs.true115
	movzbl	(%rbx), %eax
	cmpq	$55, %rax
	jne	.LBB10_62
# BB#52:                                # %land.lhs.true119
	movq	$14, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB10_62
# BB#53:                                # %if.then126
	movq	%rdi, 32(%rsp)
	movq	%rbx, 40(%rsp)
	movabsq	$check_fold_consts, %rdi
	leaq	32(%rsp), %rsi
	movq	%rdx, %rbx
	callq	do_float_handler
	movq	%rbx, %rdx
	testq	%rdx, %rax
	je	.LBB10_54
# BB#55:                                # %if.end132
	movl	60(%rsp), %eax
	testq	%rax, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	jne	.LBB10_56
# BB#61:                                # %if.end137
	movq	48(%rsp), %r9
	movl	56(%rsp), %r10d
	movq	$32, %rcx
	movq	%r9, %r8
	shrq	%cl, %r8
	movq	%r8, %rax
	movq	%r10, %rbx
	jmp	.LBB10_178
.LBB10_62:                              # %if.else141
	movq	%rdi, %r8
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rdx, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdi
	movq	%rax, %rcx
	jne	.LBB10_64
# BB#63:                                # %if.else141
	movq	%rdi, %rcx
.LBB10_64:                              # %if.else141
	cmpq	$1, %r12
	movq	8(%rsp), %r11           # 8-byte Reload
	jne	.LBB10_66
# BB#65:                                # %if.else141
	movq	%rdi, %rax
.LBB10_66:                              # %if.else141
	orq	%rcx, %rax
	je	.LBB10_111
# BB#67:                                # %if.else141
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$54, %rsi
	jne	.LBB10_111
# BB#68:                                # %land.lhs.true155
	movl	(%rbx), %esi
	movq	%rsi, %rax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$54, %rax
	jne	.LBB10_111
# BB#69:                                # %if.then163
	movzbl	mode_bitsize(%r13,%r13), %eax
	cmpq	$55, %r10
	jne	.LBB10_71
# BB#70:                                # %if.then170
	movq	16(%r8), %r14
	movq	24(%r8), %r15
	jmp	.LBB10_72
.LBB10_111:                             # %if.else294
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$-102, %rax
	andq	%rdx, %rax
	cmpq	$9, %rax
	ja	.LBB10_176
# BB#112:                               # %if.else294
	jmpq	*.LJTI10_0(,%rax,8)
.LBB10_139:                             # %sw.bb408
	movq	global_rtl+24(%rip), %rdi
	cmpq	%rdi, %r11
	je	.LBB10_160
# BB#140:                               # %lor.lhs.false411
	movq	global_rtl+32(%rip), %rdx
	cmpq	%rdx, %r11
	je	.LBB10_160
# BB#141:                               # %lor.lhs.false411
	movq	global_rtl+56(%rip), %rcx
	cmpq	%rcx, %r11
	je	.LBB10_160
# BB#142:                               # %lor.lhs.false411
	movq	global_rtl+48(%rip), %rsi
	cmpq	%rsi, %r11
	je	.LBB10_160
# BB#143:                               # %lor.lhs.false420
	movzbl	(%r11), %r9d
	cmpq	$75, %r9
	jne	.LBB10_151
# BB#144:                               # %land.lhs.true425
	movq	16(%r11), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB10_151
# BB#145:                               # %land.lhs.true433
	movq	8(%r11), %r8
	cmpq	%rdi, %r8
	je	.LBB10_160
# BB#146:                               # %land.lhs.true433
	cmpq	%rdx, %r8
	je	.LBB10_160
# BB#147:                               # %lor.lhs.false445
	movq	global_rtl+40(%rip), %rax
	cmpq	%rax, %r11
	jne	.LBB10_149
# BB#148:                               # %lor.lhs.false445
	movzbl	fixed_regs+16(%rip), %eax
	testq	$255, %rax
	jne	.LBB10_160
.LBB10_149:                             # %lor.lhs.false451
	cmpq	%rcx, %r8
	je	.LBB10_160
# BB#150:                               # %lor.lhs.false451
	cmpq	%rsi, %r8
	je	.LBB10_160
.LBB10_151:                             # %lor.lhs.false463
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %r11
	je	.LBB10_160
# BB#152:                               # %lor.lhs.false466
	movq	global_rtl+64(%rip), %rsi
	cmpq	%rsi, %r11
	je	.LBB10_160
# BB#153:                               # %lor.lhs.false469
	movq	global_rtl+72(%rip), %rdi
	cmpq	%rdi, %r11
	je	.LBB10_160
# BB#154:                               # %lor.lhs.false472
	cmpq	$75, %r9
	jne	.LBB10_159
# BB#155:                               # %land.lhs.true477
	movq	16(%r11), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB10_161
# BB#156:                               # %land.lhs.true485
	movq	8(%r11), %rax
	cmpq	%rcx, %rax
	je	.LBB10_160
# BB#157:                               # %land.lhs.true485
	cmpq	%rsi, %rax
	je	.LBB10_160
# BB#158:                               # %land.lhs.true485
	cmpq	%rdi, %rax
	je	.LBB10_160
.LBB10_159:                             # %lor.lhs.false503
	cmpq	$70, %r9
	jne	.LBB10_161
.LBB10_160:                             # %land.lhs.true508
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB10_162
.LBB10_161:                             # %lor.lhs.false511
	cmpq	$67, %r10
	jne	.LBB10_176
.LBB10_162:                             # %land.lhs.true516
	movq	global_rtl+40(%rip), %rax
	cmpq	%rax, %r11
	jne	.LBB10_163
	jmp	.LBB10_176
.LBB10_47:
	xorq	%rax, %rax
	xorq	%r10, %r10
	xorq	%rbx, %rbx
	jmp	.LBB10_178
.LBB10_71:                              # %if.else176
	movq	8(%r8), %r14
	movq	$63, %rcx
	movq	%r14, %r15
	sarq	%cl, %r15
.LBB10_72:                              # %if.end182
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$55, %rsi
	jne	.LBB10_74
# BB#73:                                # %if.then187
	movq	16(%rbx), %rsi
	movq	24(%rbx), %r13
	jmp	.LBB10_75
.LBB10_74:                              # %if.else194
	movq	8(%rbx), %rsi
	movq	$63, %rcx
	movq	%rsi, %r13
	sarq	%cl, %r13
.LBB10_75:                              # %if.end201
	testq	%rax, %rax
	je	.LBB10_76
# BB#77:                                # %if.end201
	cmpq	$63, %rax
	ja	.LBB10_76
# BB#78:                                # %if.then207
	movq	$1, %rbx
	movq	$1, %r12
	movq	%rax, %rcx
	shlq	%cl, %r12
	decq	%r12
	leaq	(%rax,%rdx), %rcx
	shlq	%cl, %rbx
	testq	%rbx, %r14
	movq	%r14, %rdi
	je	.LBB10_80
# BB#79:                                # %if.then217
	movq	$-1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	orq	%r14, %rdi
.LBB10_80:                              # %if.end220
	andq	%r12, %r14
	andq	%rsi, %r12
	testq	%rbx, %rsi
	je	.LBB10_82
# BB#81:                                # %if.then226
	movq	$-1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, %rsi
	jmp	.LBB10_82
.LBB10_76:
	movq	%r14, %rdi
	movq	%rsi, %r12
.LBB10_82:                              # %if.end231
	testq	%rax, %rax
	je	.LBB10_83
# BB#84:                                # %if.end231
	cmpq	$64, %rax
	movq	%r15, %rcx
	movq	%r13, %r11
	ja	.LBB10_86
# BB#85:                                # %if.then237
	movq	$63, %rcx
	sarq	%cl, %rdi
	movq	$63, %rcx
	sarq	%cl, %rsi
	xorq	%rcx, %rcx
	movq	%rdi, %r15
	movq	%rsi, %r13
	xorq	%r11, %r11
	jmp	.LBB10_86
.LBB10_83:
	movq	%r15, %rcx
	movq	%r13, %r11
.LBB10_86:                              # %if.end244
	cmpq	%r11, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rax
	movq	%rdx, %r9
	jne	.LBB10_88
# BB#87:                                # %if.end244
	movq	%rax, %r9
.LBB10_88:                              # %if.end244
	cmpq	%r12, %r14
	jne	.LBB10_90
# BB#89:                                # %if.end244
	movq	%rax, %rdx
.LBB10_90:                              # %if.end244
	movq	$1, %r10
	cmpq	%r13, %r15
	movq	$1, %r8
	jl	.LBB10_95
# BB#91:                                # %lor.rhs
	jne	.LBB10_92
# BB#93:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB10_94
.LBB10_92:
	movabsq	$0, %rax
.LBB10_94:                              # %lor.rhs
	cmpq	%r12, %r14
	sbbq	%r8, %r8
	andq	%rax, %r8
.LBB10_95:                              # %lor.end
	cmpq	%r15, %r13
	jl	.LBB10_100
# BB#96:                                # %lor.rhs260
	jne	.LBB10_97
# BB#98:                                # %lor.rhs260
	movabsq	$1, %rax
	jmp	.LBB10_99
.LBB10_97:
	movabsq	$0, %rax
.LBB10_99:                              # %lor.rhs260
	cmpq	%r14, %r12
	sbbq	%r10, %r10
	andq	%rax, %r10
.LBB10_100:                             # %lor.end268
	movq	$1, %rbx
	cmpq	%r11, %rcx
	movq	$1, %rax
	jb	.LBB10_105
# BB#101:                               # %lor.rhs272
	jne	.LBB10_102
# BB#103:                               # %lor.rhs272
	movabsq	$1, %rsi
	jmp	.LBB10_104
.LBB10_102:
	movabsq	$0, %rsi
.LBB10_104:                             # %lor.rhs272
	cmpq	%r12, %r14
	sbbq	%rax, %rax
	andq	%rsi, %rax
.LBB10_105:                             # %lor.end280
	cmpq	%rcx, %r11
	jb	.LBB10_110
# BB#106:                               # %lor.rhs284
	cmpq	%r11, %rcx
	jne	.LBB10_107
# BB#108:                               # %lor.rhs284
	movabsq	$1, %rcx
	jmp	.LBB10_109
.LBB10_107:
	movabsq	$0, %rcx
.LBB10_109:                             # %lor.rhs284
	cmpq	%r14, %r12
	sbbq	%rbx, %rbx
	andq	%rcx, %rbx
.LBB10_110:                             # %lor.end292
	andq	%rdx, %r9
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB10_178:                             # %if.end604
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	$-102, %rcx
	andq	%rdx, %rcx
	cmpq	$17, %rcx
	ja	.LBB10_221
# BB#179:                               # %if.end604
	jmpq	*.LJTI10_1(,%rcx,8)
.LBB10_184:                             # %sw.bb608
	testq	%rdx, %r9
	jne	.LBB10_185
# BB#186:                               # %sw.bb608
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_180:                             # %sw.bb605
	testq	%rdx, %r9
	jne	.LBB10_181
# BB#182:                               # %sw.bb605
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_208:                             # %sw.bb646
	orq	%r9, %r10
	testq	%rdx, %r10
	jne	.LBB10_209
# BB#210:                               # %sw.bb646
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_192:                             # %sw.bb620
	testq	%rdx, %r10
	jne	.LBB10_193
# BB#194:                               # %sw.bb620
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_204:                             # %sw.bb638
	orq	%r9, %r8
	testq	%rdx, %r8
	jne	.LBB10_205
# BB#206:                               # %sw.bb638
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_188:                             # %sw.bb614
	testq	%r8, %r8
	jne	.LBB10_189
# BB#190:                               # %sw.bb614
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_221:                             # %sw.default672
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.simplify_relational_operation, %rdx
	movq	$2350, %rsi             # imm = 0x92E
	callq	fancy_abort
.LBB10_54:                              # %if.end132.thread
	movq	$1, %rax
	movl	%eax, 60(%rsp)
	movq	24(%rsp), %rcx          # 8-byte Reload
.LBB10_56:                              # %if.then135
	addq	$-102, %rcx
	andq	%rdx, %rcx
	cmpq	$17, %rcx
	jbe	.LBB10_58
# BB#57:
	xorq	%rbx, %rbx
	jmp	.LBB10_38
.LBB10_58:                              # %if.then135
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$128001, %rdx           # imm = 0x1F401
	jne	.LBB10_163
# BB#59:                                # %if.then135
	shlq	%cl, %rax
	testq	$133182, %rax           # imm = 0x2083E
	movabsq	$0, %rbx
	je	.LBB10_38
# BB#60:                                # %sw.bb136
	movq	const_int_rtx+512(%rip), %rbx
	jmp	.LBB10_38
.LBB10_113:                             # %sw.bb295
	movq	global_rtl+24(%rip), %rdi
	cmpq	%rdi, %r11
	je	.LBB10_135
# BB#114:                               # %lor.lhs.false298
	movq	global_rtl+32(%rip), %rdx
	cmpq	%rdx, %r11
	je	.LBB10_135
# BB#115:                               # %lor.lhs.false298
	movq	global_rtl+56(%rip), %rcx
	cmpq	%rcx, %r11
	je	.LBB10_135
# BB#116:                               # %lor.lhs.false298
	movq	global_rtl+48(%rip), %rsi
	cmpq	%rsi, %r11
	je	.LBB10_135
# BB#117:                               # %lor.lhs.false307
	movzbl	(%r11), %r9d
	cmpq	$75, %r9
	jne	.LBB10_125
# BB#118:                               # %land.lhs.true312
	movq	16(%r11), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB10_125
# BB#119:                               # %land.lhs.true320
	movq	8(%r11), %r8
	cmpq	%rdi, %r8
	je	.LBB10_135
# BB#120:                               # %land.lhs.true320
	cmpq	%rdx, %r8
	je	.LBB10_135
# BB#121:                               # %lor.lhs.false332
	movq	global_rtl+40(%rip), %rax
	cmpq	%rax, %r11
	jne	.LBB10_123
# BB#122:                               # %lor.lhs.false332
	movzbl	fixed_regs+16(%rip), %eax
	testq	$255, %rax
	jne	.LBB10_135
.LBB10_123:                             # %lor.lhs.false338
	cmpq	%rcx, %r8
	je	.LBB10_135
# BB#124:                               # %lor.lhs.false338
	cmpq	%rsi, %r8
	je	.LBB10_135
.LBB10_125:                             # %lor.lhs.false350
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %r11
	je	.LBB10_135
# BB#126:                               # %lor.lhs.false353
	movq	global_rtl+64(%rip), %rsi
	cmpq	%rsi, %r11
	je	.LBB10_135
# BB#127:                               # %lor.lhs.false356
	movq	global_rtl+72(%rip), %rdi
	cmpq	%rdi, %r11
	je	.LBB10_135
# BB#128:                               # %lor.lhs.false359
	cmpq	$75, %r9
	jne	.LBB10_134
# BB#129:                               # %land.lhs.true364
	movq	16(%r11), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB10_130
# BB#131:                               # %land.lhs.true372
	movq	8(%r11), %rax
	cmpq	%rcx, %rax
	je	.LBB10_135
# BB#132:                               # %land.lhs.true372
	cmpq	%rsi, %rax
	je	.LBB10_135
# BB#133:                               # %land.lhs.true372
	cmpq	%rdi, %rax
	je	.LBB10_135
.LBB10_134:                             # %lor.lhs.false390
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	$70, %r9
	je	.LBB10_136
	jmp	.LBB10_137
.LBB10_164:                             # %sw.bb521
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB10_176
	jmp	.LBB10_163
.LBB10_170:                             # %sw.bb566
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB10_176
# BB#171:                               # %land.lhs.true571
	movq	8(%rbx), %rax
	movq	mode_mask_array(,%r13,8), %rcx
	cmpq	%rcx, %rax
	jne	.LBB10_176
# BB#172:                               # %land.lhs.true579
	cmpq	$7, %r12
	ja	.LBB10_176
# BB#173:                               # %land.lhs.true579
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	$170, %rax
	je	.LBB10_176
# BB#174:                               # %if.then599
	movq	const_int_rtx+512(%rip), %rbx
	jmp	.LBB10_38
.LBB10_166:                             # %sw.bb531
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB10_176
# BB#167:                               # %land.lhs.true536
	movq	8(%rbx), %rax
	movq	mode_mask_array(,%r13,8), %rcx
	cmpq	%rcx, %rax
	jne	.LBB10_176
# BB#168:                               # %land.lhs.true544
	cmpq	$7, %r12
	ja	.LBB10_176
# BB#169:                               # %land.lhs.true544
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	$170, %rax
	je	.LBB10_176
.LBB10_163:                             # %sw.bb
	movq	const_true_rtx(%rip), %rbx
.LBB10_38:                              # %return
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_165:                             # %sw.bb526
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB10_176
	jmp	.LBB10_38
.LBB10_216:                             # %sw.bb662
	orq	%r9, %rbx
	testq	%rdx, %rbx
	jne	.LBB10_217
# BB#218:                               # %sw.bb662
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_200:                             # %sw.bb632
	testq	%rdx, %rbx
	jne	.LBB10_201
# BB#202:                               # %sw.bb632
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_212:                             # %sw.bb654
	orq	%r9, %rax
	testq	%rdx, %rax
	jne	.LBB10_213
# BB#214:                               # %sw.bb654
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_196:                             # %sw.bb626
	testq	%rax, %rax
	jne	.LBB10_197
# BB#198:                               # %sw.bb626
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_220:                             # %sw.bb671
	movq	const_int_rtx+512(%rip), %rbx
	jmp	.LBB10_38
.LBB10_185:
	movabsq	$const_int_rtx+512, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_181:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_209:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_193:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_205:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_189:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_135:                             # %land.lhs.true395
	movq	const_int_rtx+512(%rip), %rcx
.LBB10_136:                             # %land.lhs.true395
	cmpq	%rcx, %rbx
	jne	.LBB10_137
# BB#175:                               # %land.lhs.true403
	movq	global_rtl+40(%rip), %rax
	cmpq	%rax, %r11
	jne	.LBB10_38
	jmp	.LBB10_176
.LBB10_217:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_201:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_213:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_197:
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %rbx
	jmp	.LBB10_38
.LBB10_222:                             # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_relational_operation, %rcx
	movq	$2091, %rsi             # imm = 0x82B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB10_130:                             # %lor.lhs.false390.thread
	movq	const_int_rtx+512(%rip), %rcx
.LBB10_137:                             # %lor.lhs.false398
	cmpq	$67, %r10
	jne	.LBB10_176
# BB#138:                               # %lor.lhs.false398
	movq	global_rtl+40(%rip), %rax
	cmpq	%rax, %r11
	movq	%rcx, %rbx
	jne	.LBB10_38
.LBB10_176:                             # %sw.epilog
	xorq	%rbx, %rbx
	jmp	.LBB10_38
.Ltmp178:
	.size	simplify_relational_operation, .Ltmp178-simplify_relational_operation
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI10_0:
	.quad	.LBB10_139
	.quad	.LBB10_113
	.quad	.LBB10_176
	.quad	.LBB10_176
	.quad	.LBB10_176
	.quad	.LBB10_176
	.quad	.LBB10_164
	.quad	.LBB10_170
	.quad	.LBB10_166
	.quad	.LBB10_165
.LJTI10_1:
	.quad	.LBB10_184
	.quad	.LBB10_180
	.quad	.LBB10_208
	.quad	.LBB10_192
	.quad	.LBB10_204
	.quad	.LBB10_188
	.quad	.LBB10_216
	.quad	.LBB10_200
	.quad	.LBB10_212
	.quad	.LBB10_196
	.quad	.LBB10_220
	.quad	.LBB10_163
	.quad	.LBB10_180
	.quad	.LBB10_208
	.quad	.LBB10_192
	.quad	.LBB10_204
	.quad	.LBB10_188
	.quad	.LBB10_184

	.text
	.globl	simplify_replace_rtx
	.align	16, 0x90
	.type	simplify_replace_rtx,@function
simplify_replace_rtx:                   # @simplify_replace_rtx
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
	subq	$32, %rsp
.Ltmp190:
	.cfi_def_cfa_offset 80
.Ltmp191:
	.cfi_offset %rbx, -48
.Ltmp192:
	.cfi_offset %r12, -40
.Ltmp193:
	.cfi_offset %r13, -32
.Ltmp194:
	.cfi_offset %r14, -24
.Ltmp195:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %r15d
	movq	%rsi, %r13
	movq	$16, %rcx
	movq	%r15, %r14
	shrq	%cl, %r14
	cmpq	%r13, %rbx
	je	.LBB11_1
# BB#2:                                 # %if.end
	andq	$65535, %r15            # imm = 0xFFFF
	movsbq	rtx_class(%r15), %r12
	andq	$255, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	cmpq	$119, %r12
	jg	.LBB11_41
# BB#3:                                 # %if.end
	cmpq	$97, %r12
	jg	.LBB11_12
# BB#4:                                 # %if.end
	cmpq	$59, %r12
	jg	.LBB11_28
# BB#5:                                 # %if.end
	andq	%r12, %rax
	cmpq	$49, %rax
	je	.LBB11_16
# BB#6:                                 # %if.end
	cmpq	$50, %rax
	je	.LBB11_14
# BB#7:                                 # %if.end
	cmpq	$51, %rax
	jne	.LBB11_46
	jmp	.LBB11_8
.LBB11_1:
	movq	%rdx, %rbx
	jmp	.LBB11_45
.LBB11_41:                              # %if.end
	andq	%r12, %rax
	cmpq	$120, %rax
	jne	.LBB11_46
# BB#42:                                # %sw.bb113
	cmpq	$63, %r15
	jne	.LBB11_45
# BB#43:                                # %if.then116
	movq	8(%rbx), %rdi
	movq	%r13, %rsi
	callq	simplify_replace_rtx
	movq	8(%rbx), %rdx
	movl	16(%rbx), %ecx
	movzbl	2(%rdx), %edx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	simplify_gen_subreg
	testq	%rax, %rax
	je	.LBB11_45
# BB#44:                                # %if.then116
	movq	%rax, %rbx
	jmp	.LBB11_45
.LBB11_12:                              # %if.end
	andq	%r12, %rax
	cmpq	$98, %rax
	jne	.LBB11_13
.LBB11_8:                               # %sw.bb82
	movq	8(%rbx), %rdi
	movzbl	2(%rdi), %r12d
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rsi
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	callq	simplify_replace_rtx
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	jne	.LBB11_10
# BB#9:                                 # %cond.false98
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB11_10:                              # %cond.end102
	movq	16(%rbx), %rdi
	movq	%r13, %rsi
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdx
	callq	simplify_replace_rtx
	movq	24(%rbx), %rdi
	movq	%r13, %rsi
	movq	%rax, %r13
	movq	%r12, %rdx
	callq	simplify_replace_rtx
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r13, %r8
	movq	%r12, %r9
	callq	simplify_ternary_operation
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB11_45
# BB#11:                                # %if.end.i211
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r12, %r8
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_eee  # TAILCALL
.LBB11_28:                              # %if.end
	andq	%rax, %r12
	cmpq	$60, %r12
	jne	.LBB11_46
# BB#29:                                # %sw.bb26
	movq	8(%rbx), %rdi
	movl	(%rdi), %eax
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB11_31
# BB#30:                                # %cond.false43
	movq	16(%rbx), %rax
	movl	(%rax), %eax
.LBB11_31:                              # %cond.end50
	movq	$16, %rcx
	shrq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rsi
	movq	%rdx, %r12
                                        # kill: RDX<def> R12<kill>
	callq	simplify_replace_rtx
	movq	16(%rbx), %rdi
	movq	%rax, %rbx
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	simplify_replace_rtx
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %r12
	andq	$255, %rsi
	jne	.LBB11_35
# BB#32:                                # %cond.false63
	movl	(%rbx), %esi
	testq	$16711680, %rsi         # imm = 0xFF0000
	jne	.LBB11_34
# BB#33:                                # %cond.false73
	movl	(%r12), %esi
.LBB11_34:                              # %cond.end77
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
.LBB11_35:                              # %cond.end79
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	simplify_relational_operation
	movq	%rbx, %r13
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB11_45
# BB#36:                                # %if.end.i216
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r13, %rbx
	je	.LBB11_37
# BB#38:                                # %if.then2.i
	movq	%r15, %rdi
	callq	swap_condition
	movq	%rax, %r15
	movq	%r12, %rdx
	jmp	.LBB11_39
.LBB11_13:                              # %if.end
	cmpq	$99, %rax
	jne	.LBB11_46
.LBB11_14:                              # %sw.bb16
	movq	8(%rbx), %rdi
	movq	%r13, %rsi
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	callq	simplify_replace_rtx
	movq	16(%rbx), %rdi
	movq	%rax, %rbx
	movq	%r13, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	simplify_replace_rtx
	movq	%rax, %r13
	andq	$255, %r12
	cmpq	$99, %r12
	jne	.LBB11_15
# BB#20:                                # %land.lhs.true.i
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rbx, %r12
	je	.LBB11_22
# BB#21:                                # %land.lhs.true.i
	movq	%r13, %r12
.LBB11_22:                              # %land.lhs.true.i
	testq	%rcx, %rcx
	je	.LBB11_24
# BB#23:                                # %land.lhs.true.i
	movq	%rbx, %r13
	jmp	.LBB11_24
.LBB11_46:                              # %sw.default
	cmpq	$66, %r15
	jne	.LBB11_45
# BB#47:                                # %if.then140
	movq	8(%rbx), %rdi
	movq	%r13, %rsi
	callq	simplify_replace_rtx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	replace_equiv_address_nv  # TAILCALL
.LBB11_15:
	movq	%rbx, %r12
.LBB11_24:                              # %if.end.i223
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	simplify_binary_operation
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB11_45
# BB#25:                                # %if.end5.i
	leaq	-75(%r15), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB11_27
# BB#26:                                # %if.then10.i
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	simplify_plus_minus
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB11_45
.LBB11_27:                              # %if.end15.i
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	jmp	.LBB11_40
.LBB11_16:                              # %sw.bb
	movq	8(%rbx), %rdi
	movzbl	2(%rdi), %ebx
	cmpq	%r13, %rdi
	je	.LBB11_18
# BB#17:                                # %cond.false
	movq	%r13, %rsi
	callq	simplify_replace_rtx
	movq	%rax, %rdx
.LBB11_18:                              # %cond.end
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rdx, %r12
                                        # kill: RDX<def> R12<kill>
	movq	%rbx, %rcx
	callq	simplify_unary_operation
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB11_19
.LBB11_45:                              # %if.then116
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_19:                              # %if.end.i
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_e  # TAILCALL
.LBB11_37:
	movq	%rbx, %rdx
	movq	%r12, %rbx
.LBB11_39:                              # %if.end4.i
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
.LBB11_40:                              # %if.end4.i
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp196:
	.size	simplify_replace_rtx, .Ltmp196-simplify_replace_rtx
	.cfi_endproc

	.globl	simplify_gen_subreg
	.align	16, 0x90
	.type	simplify_gen_subreg,@function
simplify_gen_subreg:                    # @simplify_gen_subreg
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
	movq	%rcx, %r14
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rbx
	je	.LBB12_4
# BB#1:                                 # %entry
	testq	%r13, %r12
	je	.LBB12_4
# BB#2:                                 # %entry
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$51, %rax
	je	.LBB12_4
# BB#3:                                 # %entry
	movq	%r12, %rcx
	andq	%r13, %rcx
	cmpq	$51, %rcx
	je	.LBB12_4
# BB#6:                                 # %if.end
	movl	(%r15), %edi
	movq	$16, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rax, %rdx
	je	.LBB12_9
# BB#7:                                 # %if.end
	testq	%rdx, %rdx
	jne	.LBB12_8
.LBB12_9:                               # %if.end12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	movq	%r14, %rsi
	andq	%r13, %rsi
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%rcx
	testq	%rdx, %rdx
	jne	.LBB12_11
# BB#10:                                # %lor.lhs.false13
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rsi, %rax
	jbe	.LBB12_11
# BB#12:                                # %if.end20
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$71, %rdi
	jne	.LBB12_14
# BB#13:
	xorq	%rax, %rax
	jmp	.LBB12_18
.LBB12_14:                              # %if.end26
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	simplify_subreg
	testq	%rax, %rax
	jne	.LBB12_18
# BB#15:                                # %if.end29
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	je	.LBB12_16
# BB#17:                                # %if.end29
	andq	$16711680, %rax         # imm = 0xFF0000
	testq	%r13, %rax
	movabsq	$0, %rax
	je	.LBB12_18
# BB#19:                                # %if.end41
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_SUBREG  # TAILCALL
.LBB12_16:
	xorq	%rax, %rax
.LBB12_18:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_4:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_gen_subreg, %rcx
	movq	$2755, %rsi             # imm = 0xAC3
	jmp	.LBB12_5
.LBB12_8:                               # %if.then11
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_gen_subreg, %rcx
	movq	$2759, %rsi             # imm = 0xAC7
	jmp	.LBB12_5
.LBB12_11:                              # %if.then19
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_gen_subreg, %rcx
	movq	$2763, %rsi             # imm = 0xACB
.LBB12_5:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp213:
	.size	simplify_gen_subreg, .Ltmp213-simplify_gen_subreg
	.cfi_endproc

	.align	16, 0x90
	.type	simplify_unary_real,@function
simplify_unary_real:                    # @simplify_unary_real
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp217:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp218:
	.cfi_def_cfa_offset 24
	subq	$168, %rsp
.Ltmp219:
	.cfi_def_cfa_offset 192
.Ltmp220:
	.cfi_offset %rbx, -24
.Ltmp221:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	32(%rax), %rax
	movq	%rax, 160(%rsp)
	movq	%rdx, 152(%rsp)
	movq	%rcx, 144(%rsp)
	movzbl	24(%rbx), %ecx
	movl	20(%rbx), %eax
	testq	%rcx, %rcx
	je	.LBB13_7
# BB#1:                                 # %if.then
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$128, %rax
	jne	.LBB13_2
# BB#5:                                 # %sw.bb2
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	efixui
	jmp	.LBB13_6
.LBB13_7:                               # %if.else
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$122, %rdx
	jg	.LBB13_10
# BB#8:                                 # %if.else
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$77, %rax
	jne	.LBB13_22
# BB#9:                                 # %sw.bb12
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	96(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_negate
	movq	112(%rsp), %rax
	movq	96(%rsp), %rcx
	movq	104(%rsp), %rdx
.LBB13_16:                              # %sw.epilog22
	movq	%rax, 160(%rsp)
	movq	%rdx, 152(%rsp)
	movq	%rcx, 144(%rsp)
.LBB13_17:                              # %sw.epilog22
	movl	16(%rbx), %edi
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	immed_real_const_1
	movq	%rax, 8(%rbx)
	jmp	.LBB13_13
.LBB13_2:                               # %if.then
	cmpq	$126, %rax
	jne	.LBB13_4
# BB#3:                                 # %sw.bb
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	efixi
.LBB13_6:                               # %sw.epilog
	movl	16(%rbx), %esi
	movq	%rax, %rdi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 8(%rbx)
.LBB13_13:                              # %if.end
	addq	$168, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB13_10:                              # %if.else
	addq	$-123, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	$7, %rax
	ja	.LBB13_22
# BB#11:                                # %if.else
	jmpq	*.LJTI13_0(,%rax,8)
.LBB13_19:                              # %sw.bb14
	movl	16(%rbx), %esi
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	72(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	real_value_truncate
	movq	88(%rsp), %rax
	movq	72(%rsp), %rcx
	movq	80(%rsp), %rdx
	jmp	.LBB13_16
.LBB13_20:                              # %sw.bb17
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	48(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etrunci
	movq	64(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	56(%rsp), %rdx
	jmp	.LBB13_16
.LBB13_21:                              # %sw.bb19
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	24(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	etruncui
	movq	40(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	32(%rsp), %rdx
	jmp	.LBB13_16
.LBB13_14:                              # %sw.bb9
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_negative
	testq	%r14, %rax
	je	.LBB13_17
# BB#15:                                # %cond.true
	movq	160(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	144(%rsp), %rax
	movq	152(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	120(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_negate
	movq	136(%rsp), %rax
	movq	120(%rsp), %rcx
	movq	128(%rsp), %rdx
	jmp	.LBB13_16
.LBB13_12:                              # %sw.bb7
	movq	$0, 8(%rbx)
	jmp	.LBB13_13
.LBB13_22:                              # %sw.default21
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.simplify_unary_real, %rdx
	movq	$400, %rsi              # imm = 0x190
	callq	fancy_abort
.LBB13_4:                               # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.simplify_unary_real, %rcx
	movq	$380, %rsi              # imm = 0x17C
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp222:
	.size	simplify_unary_real, .Ltmp222-simplify_unary_real
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_17
	.quad	.LBB13_19
	.quad	.LBB13_22
	.quad	.LBB13_20
	.quad	.LBB13_22
	.quad	.LBB13_21
	.quad	.LBB13_14
	.quad	.LBB13_12

	.text
	.align	16, 0x90
	.type	simplify_binary_real,@function
simplify_binary_real:                   # @simplify_binary_real
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp225:
	.cfi_def_cfa_offset 16
	subq	$176, %rsp
.Ltmp226:
	.cfi_def_cfa_offset 192
.Ltmp227:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	32(%rax), %rax
	movq	%rax, 168(%rsp)
	movq	%rdx, 160(%rsp)
	movq	%rcx, 152(%rsp)
	movq	8(%rbx), %rax
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	32(%rax), %rax
	movq	%rax, 144(%rsp)
	movq	%rdx, 136(%rsp)
	movq	%rcx, 128(%rsp)
	movl	28(%rbx), %esi
	movq	168(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	152(%rsp), %rax
	movq	160(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	80(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	real_value_truncate
	movq	96(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	88(%rsp), %rdx
	movq	%rax, 168(%rsp)
	movq	%rdx, 160(%rsp)
	movq	%rcx, 152(%rsp)
	movl	28(%rbx), %esi
	movq	144(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	128(%rsp), %rax
	movq	136(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	56(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	real_value_truncate
	movq	72(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	64(%rsp), %rdx
	movq	%rax, 144(%rsp)
	movq	%rdx, 136(%rsp)
	movq	%rcx, 128(%rsp)
	movl	24(%rbx), %edi
	callq	rtx_to_tree_code
	leaq	104(%rsp), %rdi
	leaq	152(%rsp), %rdx
	leaq	128(%rsp), %rcx
	movq	%rax, %rsi
	callq	earith
	movl	28(%rbx), %esi
	movq	120(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	104(%rsp), %rax
	movq	112(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	real_value_truncate
	movq	48(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	40(%rsp), %rdx
	movq	%rax, 120(%rsp)
	movq	%rdx, 112(%rsp)
	movq	%rcx, 104(%rsp)
	movl	28(%rbx), %edi
	movq	48(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rsp), %rax
	movq	40(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	immed_real_const_1
	movq	%rax, 16(%rbx)
	addq	$176, %rsp
	popq	%rbx
	retq
.Ltmp228:
	.size	simplify_binary_real, .Ltmp228-simplify_binary_real
	.cfi_endproc

	.align	16, 0x90
	.type	simplify_binary_is2orm1,@function
simplify_binary_is2orm1:                # @simplify_binary_is2orm1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp234:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp235:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp236:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp237:
	.cfi_def_cfa_offset 40
	subq	$72, %rsp
.Ltmp238:
	.cfi_def_cfa_offset 112
.Ltmp239:
	.cfi_offset %rbx, -40
.Ltmp240:
	.cfi_offset %r12, -32
.Ltmp241:
	.cfi_offset %r14, -24
.Ltmp242:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	32(%rax), %rax
	movq	%rax, 64(%rsp)
	movq	%rdx, 56(%rsp)
	movq	%rcx, 48(%rsp)
	movq	dconst2+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst2+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst2(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	48(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	movabsq	$0, %rbx
	movabsq	$1, %r15
	movq	%rbx, %rax
	jne	.LBB15_2
# BB#1:                                 # %entry
	movq	%r15, %rax
.LBB15_2:                               # %entry
	movb	%al, 8(%r14)
	movq	dconstm1+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconstm1+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconstm1(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	64(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	48(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r12, %rax
	jne	.LBB15_4
# BB#3:                                 # %entry
	movq	%r15, %rbx
.LBB15_4:                               # %entry
	movb	%bl, 9(%r14)
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp243:
	.size	simplify_binary_is2orm1, .Ltmp243-simplify_binary_is2orm1
	.cfi_endproc

	.align	16, 0x90
	.type	check_fold_consts,@function
check_fold_consts:                      # @check_fold_consts
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp249:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp250:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp251:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp252:
	.cfi_def_cfa_offset 40
	subq	$104, %rsp
.Ltmp253:
	.cfi_def_cfa_offset 144
.Ltmp254:
	.cfi_offset %rbx, -40
.Ltmp255:
	.cfi_offset %r12, -32
.Ltmp256:
	.cfi_offset %r14, -24
.Ltmp257:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$1, %rax
	movl	%eax, 28(%rbx)
	movq	(%rbx), %rax
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	32(%rax), %rax
	movq	%rax, 96(%rsp)
	movq	%rdx, 88(%rsp)
	movq	%rcx, 80(%rsp)
	movq	8(%rbx), %rax
	movq	16(%rax), %rcx
	movq	24(%rax), %rdx
	movq	32(%rax), %rax
	movq	%rax, 72(%rsp)
	movq	%rdx, 64(%rsp)
	movq	%rcx, 56(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	80(%rsp), %rax
	movq	88(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB16_9
# BB#1:                                 # %lor.lhs.false
	movq	72(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	56(%rsp), %rax
	movq	64(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%r14, %rax
	jne	.LBB16_9
# BB#2:                                 # %if.end
	movq	72(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	56(%rsp), %rax
	movq	64(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	80(%rsp), %rax
	movq	88(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r14, %rax
	movabsq	$0, %r15
	movabsq	$1, %r12
	movq	%r15, %rax
	jne	.LBB16_4
# BB#3:                                 # %if.end
	movq	%r12, %rax
.LBB16_4:                               # %if.end
	movl	%eax, 16(%rbx)
	movq	72(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	56(%rsp), %rax
	movq	64(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	96(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	80(%rsp), %rax
	movq	88(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	andq	%r14, %rax
	cmpq	%r14, %rax
	movq	%r15, %rax
	jne	.LBB16_6
# BB#5:                                 # %if.end
	movq	%r12, %rax
.LBB16_6:                               # %if.end
	movl	%eax, 20(%rbx)
	movq	96(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	80(%rsp), %rax
	movq	88(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	72(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	56(%rsp), %rax
	movq	64(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	andq	%r14, %rax
	cmpq	%r14, %rax
	jne	.LBB16_8
# BB#7:                                 # %if.end
	movq	%r12, %r15
.LBB16_8:                               # %if.end
	movl	%r15d, 24(%rbx)
	xorq	%rax, %rax
	movl	%eax, 28(%rbx)
.LBB16_9:                               # %return
	addq	$104, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp258:
	.size	check_fold_consts, .Ltmp258-check_fold_consts
	.cfi_endproc

	.globl	simplify_rtx
	.align	16, 0x90
	.type	simplify_rtx,@function
simplify_rtx:                           # @simplify_rtx
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
.Ltmp270:
	.cfi_offset %rbx, -48
.Ltmp271:
	.cfi_offset %r12, -40
.Ltmp272:
	.cfi_offset %r13, -32
.Ltmp273:
	.cfi_offset %r14, -24
.Ltmp274:
	.cfi_offset %r15, -16
	movl	(%rdi), %ebx
	movq	%rbx, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	movsbq	rtx_class(%r14), %rax
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	cmpq	$119, %rax
	jg	.LBB17_18
# BB#1:                                 # %entry
	cmpq	$97, %rax
	jg	.LBB17_7
# BB#2:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	$59, %rax
	jg	.LBB17_14
# BB#3:                                 # %entry
	andq	%rcx, %rax
	cmpq	$49, %rax
	je	.LBB17_21
# BB#4:                                 # %entry
	cmpq	$50, %rax
	jne	.LBB17_5
# BB#11:                                # %entry.sw.bb37_crit_edge
	leaq	8(%rdi), %r15
	addq	$16, %rdi
	movq	%rdi, %r12
	jmp	.LBB17_12
.LBB17_18:                              # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$120, %rax
	jne	.LBB17_20
# BB#19:                                # %sw.bb89
	cmpq	$63, %r14
	jne	.LBB17_20
# BB#22:                                # %if.then92
	movq	8(%rdi), %rsi
	movl	16(%rdi), %ecx
	movzbl	2(%rsi), %edx
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_gen_subreg  # TAILCALL
.LBB17_7:                               # %entry
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$98, %rax
	je	.LBB17_6
# BB#8:                                 # %entry
	cmpq	$99, %rax
	jne	.LBB17_20
# BB#9:                                 # %sw.bb10
	movq	8(%rdi), %rax
	movq	16(%rdi), %rsi
	leaq	8(%rdi), %r15
	leaq	16(%rdi), %r12
	movq	%rax, %rdi
	callq	swap_commutative_operands_p
	testq	%r13, %rax
	je	.LBB17_12
# BB#10:                                # %if.then
	movq	(%r15), %rcx
	movq	(%r12), %rdx
	movq	%rdx, (%r15)
	movq	%rcx, (%r12)
	jmp	.LBB17_13
.LBB17_14:                              # %entry
	andq	%rcx, %rax
	cmpq	$60, %rax
	jne	.LBB17_20
# BB#15:                                # %sw.bb62
	movq	8(%rdi), %rdx
	movl	(%rdx), %esi
	movq	16(%rdi), %rax
	testq	$16711680, %rsi         # imm = 0xFF0000
	jne	.LBB17_17
# BB#16:                                # %cond.false
	movl	(%rax), %esi
.LBB17_17:                              # %cond.end
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r14, %rdi
	movq	%rax, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_relational_operation  # TAILCALL
.LBB17_12:                              # %sw.bb37
	movq	(%r15), %rdx
	movq	(%r12), %rcx
.LBB17_13:                              # %sw.bb37
	movq	%r14, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_binary_operation  # TAILCALL
.LBB17_21:                              # %sw.bb
	movq	8(%rdi), %rdx
	movzbl	2(%rdx), %ecx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_unary_operation  # TAILCALL
.LBB17_5:                               # %entry
	cmpq	$51, %rax
	jne	.LBB17_20
.LBB17_6:                               # %sw.bb45
	movq	8(%rdi), %rcx
	movq	16(%rdi), %r8
	movq	24(%rdi), %r9
	movzbl	2(%rcx), %edx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_ternary_operation  # TAILCALL
.LBB17_20:                              # %return
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp275:
	.size	simplify_rtx, .Ltmp275-simplify_rtx
	.cfi_endproc

	.align	16, 0x90
	.type	simplify_plus_minus_op_data_cmp,@function
simplify_plus_minus_op_data_cmp:        # @simplify_plus_minus_op_data_cmp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp278:
	.cfi_def_cfa_offset 16
.Ltmp279:
	.cfi_offset %rbx, -16
	movq	(%rsi), %rax
	movq	%rdi, %rbx
	movq	%rax, %rdi
	callq	commutative_operand_precedence
	movq	(%rbx), %rdi
	movq	%rax, %rbx
	callq	commutative_operand_precedence
	subq	%rax, %rbx
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp280:
	.size	simplify_plus_minus_op_data_cmp, .Ltmp280-simplify_plus_minus_op_data_cmp
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"simplify-rtx.c"
	.size	.L.str, 15

	.type	.L__FUNCTION__.simplify_unary_operation,@object # @__FUNCTION__.simplify_unary_operation
.L__FUNCTION__.simplify_unary_operation:
	.asciz	"simplify_unary_operation"
	.size	.L__FUNCTION__.simplify_unary_operation, 25

	.type	.L__FUNCTION__.simplify_binary_operation,@object # @__FUNCTION__.simplify_binary_operation
.L__FUNCTION__.simplify_binary_operation:
	.asciz	"simplify_binary_operation"
	.size	.L__FUNCTION__.simplify_binary_operation, 26

	.type	.L__FUNCTION__.simplify_relational_operation,@object # @__FUNCTION__.simplify_relational_operation
.L__FUNCTION__.simplify_relational_operation:
	.asciz	"simplify_relational_operation"
	.size	.L__FUNCTION__.simplify_relational_operation, 30

	.type	.L__FUNCTION__.simplify_ternary_operation,@object # @__FUNCTION__.simplify_ternary_operation
.L__FUNCTION__.simplify_ternary_operation:
	.asciz	"simplify_ternary_operation"
	.size	.L__FUNCTION__.simplify_ternary_operation, 27

	.type	.L__FUNCTION__.simplify_subreg,@object # @__FUNCTION__.simplify_subreg
.L__FUNCTION__.simplify_subreg:
	.asciz	"simplify_subreg"
	.size	.L__FUNCTION__.simplify_subreg, 16

	.type	.L__FUNCTION__.simplify_gen_subreg,@object # @__FUNCTION__.simplify_gen_subreg
.L__FUNCTION__.simplify_gen_subreg:
	.asciz	"simplify_gen_subreg"
	.size	.L__FUNCTION__.simplify_gen_subreg, 20

	.type	.L__FUNCTION__.simplify_unary_real,@object # @__FUNCTION__.simplify_unary_real
.L__FUNCTION__.simplify_unary_real:
	.asciz	"simplify_unary_real"
	.size	.L__FUNCTION__.simplify_unary_real, 20


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
