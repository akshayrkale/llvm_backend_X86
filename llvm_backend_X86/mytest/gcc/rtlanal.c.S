	.file	"rtlanal.c.bc"
	.text
	.globl	rtx_unstable_p
	.align	16, 0x90
	.type	rtx_unstable_p,@function
rtx_unstable_p:                         # @rtx_unstable_p
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
	movl	(%rdi), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	$1, %rax
	cmpq	$53, %rcx
	jg	.LBB0_5
# BB#1:                                 # %entry
	cmpq	$41, %rcx
	jne	.LBB0_3
# BB#2:                                 # %sw.bb20
	testq	$134217728, %rsi        # imm = 0x8000000
	jne	.LBB0_33
	jmp	.LBB0_3
.LBB0_5:                                # %entry
	leaq	-54(%rcx), %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	cmpq	$17, %rdx
	ja	.LBB0_3
# BB#6:                                 # %entry
	jmpq	*.LJTI0_0(,%rdx,8)
.LBB0_11:                               # %sw.bb5
	xorq	%rax, %rax
	jmp	.LBB0_33
.LBB0_3:                                # %sw.epilog
	movzbl	rtx_length(%rcx), %ebx
	testq	%rbx, %rbx
	je	.LBB0_4
# BB#23:                                # %for.body.lr.ph
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_24:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_31 Depth 2
	movq	%rbx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB0_25
# BB#28:                                # %for.cond53.preheader
                                        #   in Loop: Header=BB0_24 Depth=1
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	(%rcx,%rax,8), %r14
	movslq	(%r14), %r15
	testq	%r15, %r15
	movq	$1, %r12
	jle	.LBB0_26
	.align	16, 0x90
.LBB0_31:                               # %for.body59
                                        #   Parent Loop BB0_24 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r14,%r12,8), %rdi
	callq	rtx_unstable_p
	testq	%r13, %rax
	jne	.LBB0_32
# BB#30:                                # %for.cond53
                                        #   in Loop: Header=BB0_31 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r12
	cmpq	%r15, %rax
	jl	.LBB0_31
	jmp	.LBB0_26
	.align	16, 0x90
.LBB0_25:                               # %for.body
                                        #   in Loop: Header=BB0_24 Depth=1
	cmpq	$101, %rcx
	jne	.LBB0_26
# BB#29:                                # %if.then38
                                        #   in Loop: Header=BB0_24 Depth=1
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	callq	rtx_unstable_p
	testq	%r13, %rax
	jne	.LBB0_32
.LBB0_26:                               # %for.cond.backedge
                                        #   in Loop: Header=BB0_24 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_24
# BB#27:
	xorq	%rax, %rax
	jmp	.LBB0_33
.LBB0_32:
	movq	$1, %rax
	jmp	.LBB0_33
.LBB0_4:
	xorq	%rax, %rax
.LBB0_33:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_12:                               # %sw.bb6
	movq	global_rtl+24(%rip), %rcx
	xorq	%rax, %rax
	cmpq	%rdi, %rcx
	je	.LBB0_33
# BB#13:                                # %sw.bb6
	movq	global_rtl+32(%rip), %rcx
	cmpq	%rdi, %rcx
	je	.LBB0_33
# BB#14:                                # %lor.lhs.false8
	movq	global_rtl+40(%rip), %rcx
	cmpq	%rdi, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rdx
	je	.LBB0_16
# BB#15:                                # %lor.lhs.false8
	movq	%r8, %rdx
.LBB0_16:                               # %lor.lhs.false8
	movzbl	fixed_regs+16(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_18
# BB#17:                                # %lor.lhs.false8
	movq	%r8, %rcx
.LBB0_18:                               # %lor.lhs.false8
	orq	%rcx, %rdx
	movabsq	$0, %rax
	je	.LBB0_33
# BB#19:                                # %lor.lhs.false8
	andq	$67108864, %rsi         # imm = 0x4000000
	testq	%rbx, %rsi
	jne	.LBB0_33
# BB#20:                                # %if.end
	movq	pic_offset_table_rtx(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB0_21
# BB#22:                                # %if.end
	movabsq	$1, %rax
	jmp	.LBB0_33
.LBB0_7:                                # %sw.bb
	testq	$67108864, %rsi         # imm = 0x4000000
	movq	$1, %rax
	je	.LBB0_33
# BB#8:                                 # %lor.rhs
	movq	8(%rdi), %rdi
	callq	rtx_unstable_p
	testq	%rbx, %rax
	je	.LBB0_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB0_33
.LBB0_9:
	movabsq	$0, %rax
	jmp	.LBB0_33
.LBB0_21:
	movabsq	$0, %rax
	jmp	.LBB0_33
.Ltmp17:
	.size	rtx_unstable_p, .Ltmp17-rtx_unstable_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_11
	.quad	.LBB0_11
	.quad	.LBB0_11
	.quad	.LBB0_3
	.quad	.LBB0_11
	.quad	.LBB0_3
	.quad	.LBB0_3
	.quad	.LBB0_12
	.quad	.LBB0_3
	.quad	.LBB0_3
	.quad	.LBB0_3
	.quad	.LBB0_3
	.quad	.LBB0_7
	.quad	.LBB0_11
	.quad	.LBB0_11
	.quad	.LBB0_3
	.quad	.LBB0_11
	.quad	.LBB0_33

	.text
	.globl	rtx_varies_p
	.align	16, 0x90
	.type	rtx_varies_p,@function
rtx_varies_p:                           # @rtx_varies_p
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
	movq	%rdi, %r14
	movl	(%r14), %ecx
	movq	%rsi, %r12
	movq	%rcx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	$1, %rax
	cmpq	$134, %rsi
	jg	.LBB1_20
# BB#1:                                 # %entry
	cmpq	$53, %rsi
	jg	.LBB1_6
# BB#2:                                 # %entry
	cmpq	$41, %rsi
	jne	.LBB1_4
# BB#3:                                 # %sw.bb31
	testq	$134217728, %rcx        # imm = 0x8000000
	jne	.LBB1_36
	jmp	.LBB1_4
.LBB1_20:                               # %entry
	cmpq	$135, %rsi
	jne	.LBB1_4
# BB#21:                                # %sw.bb15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r12
	jne	.LBB1_23
# BB#22:                                # %land.lhs.true17
	movq	8(%r14), %rdi
	xorq	%rsi, %rsi
	callq	rtx_varies_p
	testq	%rbx, %rax
	movq	$1, %rax
	jne	.LBB1_36
.LBB1_23:                               # %lor.rhs23
	movq	16(%r14), %rdi
	movq	%r12, %rsi
	callq	rtx_varies_p
	testq	%rbx, %rax
	je	.LBB1_24
# BB#25:                                # %lor.rhs23
	movabsq	$1, %rax
	jmp	.LBB1_36
.LBB1_6:                                # %entry
	leaq	-54(%rsi), %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	cmpq	$17, %rdx
	ja	.LBB1_4
# BB#7:                                 # %entry
	jmpq	*.LJTI1_0(,%rdx,8)
.LBB1_12:                               # %sw.bb5
	xorq	%rax, %rax
	jmp	.LBB1_36
.LBB1_4:                                # %sw.epilog
	movzbl	rtx_length(%rsi), %ebx
	testq	%rbx, %rbx
	je	.LBB1_5
# BB#26:                                # %for.body.lr.ph
	movq	%r14, (%rsp)            # 8-byte Spill
	movq	rtx_format(,%rsi,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB1_27:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_34 Depth 2
	movq	%rbx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB1_28
# BB#31:                                # %for.cond64.preheader
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	(%rcx,%rax,8), %r15
	movslq	(%r15), %r13
	testq	%r13, %r13
	movq	$1, %r14
	jle	.LBB1_29
	.align	16, 0x90
.LBB1_34:                               # %for.body70
                                        #   Parent Loop BB1_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r15,%r14,8), %rdi
	movq	%r12, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_35
# BB#33:                                # %for.cond64
                                        #   in Loop: Header=BB1_34 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r14
	cmpq	%r13, %rax
	jl	.LBB1_34
	jmp	.LBB1_29
	.align	16, 0x90
.LBB1_28:                               # %for.body
                                        #   in Loop: Header=BB1_27 Depth=1
	cmpq	$101, %rcx
	jne	.LBB1_29
# BB#32:                                # %if.then49
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	%r12, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_35
.LBB1_29:                               # %for.cond.backedge
                                        #   in Loop: Header=BB1_27 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_27
# BB#30:
	xorq	%rax, %rax
	jmp	.LBB1_36
.LBB1_35:
	movq	$1, %rax
	jmp	.LBB1_36
.LBB1_5:
	xorq	%rax, %rax
	jmp	.LBB1_36
.LBB1_24:
	movabsq	$0, %rax
.LBB1_36:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_13:                               # %sw.bb6
	movq	global_rtl+24(%rip), %rcx
	xorq	%rax, %rax
	cmpq	%r14, %rcx
	je	.LBB1_36
# BB#14:                                # %sw.bb6
	movq	global_rtl+32(%rip), %rcx
	cmpq	%r14, %rcx
	je	.LBB1_36
# BB#15:                                # %lor.lhs.false8
	movq	global_rtl+40(%rip), %rcx
	cmpq	%r14, %rcx
	jne	.LBB1_17
# BB#16:                                # %lor.lhs.false8
	movzbl	fixed_regs+16(%rip), %ecx
	testq	$255, %rcx
	jne	.LBB1_36
.LBB1_17:                               # %if.end
	movq	pic_offset_table_rtx(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB1_18
# BB#19:                                # %if.end
	movabsq	$1, %rax
	jmp	.LBB1_36
.LBB1_8:                                # %sw.bb
	testq	$67108864, %rcx         # imm = 0x4000000
	movq	$1, %rax
	je	.LBB1_36
# BB#9:                                 # %lor.rhs
	movq	8(%r14), %rdi
	movq	%r12, %rsi
	callq	rtx_varies_p
	testq	%rbx, %rax
	je	.LBB1_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB1_36
.LBB1_10:
	movabsq	$0, %rax
	jmp	.LBB1_36
.LBB1_18:
	movabsq	$0, %rax
	jmp	.LBB1_36
.Ltmp35:
	.size	rtx_varies_p, .Ltmp35-rtx_varies_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_12
	.quad	.LBB1_12
	.quad	.LBB1_12
	.quad	.LBB1_4
	.quad	.LBB1_12
	.quad	.LBB1_4
	.quad	.LBB1_4
	.quad	.LBB1_13
	.quad	.LBB1_4
	.quad	.LBB1_4
	.quad	.LBB1_4
	.quad	.LBB1_4
	.quad	.LBB1_8
	.quad	.LBB1_12
	.quad	.LBB1_12
	.quad	.LBB1_4
	.quad	.LBB1_4
	.quad	.LBB1_36

	.text
	.globl	rtx_addr_can_trap_p
	.align	16, 0x90
	.type	rtx_addr_can_trap_p,@function
rtx_addr_can_trap_p:                    # @rtx_addr_can_trap_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp40:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp41:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp42:
	.cfi_def_cfa_offset 32
.Ltmp43:
	.cfi_offset %rbx, -32
.Ltmp44:
	.cfi_offset %r14, -24
.Ltmp45:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB2_1
	.align	16, 0x90
.LBB2_36:                               # %sw.bb106
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$16, %rbx
	movq	(%rbx), %rbx
.LBB2_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$134, %rax
	jg	.LBB2_35
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$95, %rax
	jg	.LBB2_19
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$74, %rax
	jg	.LBB2_17
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$66, %rax
	jg	.LBB2_14
# BB#5:                                 # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$58, %rax
	je	.LBB2_22
	jmp	.LBB2_6
	.align	16, 0x90
.LBB2_35:                               # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$135, %rax
	movq	$1, %rax
	je	.LBB2_36
	jmp	.LBB2_24
	.align	16, 0x90
.LBB2_19:                               # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	leaq	-96(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$4, %rcx
	jb	.LBB2_22
# BB#20:                                # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$100, %rax
	je	.LBB2_36
# BB#21:                                # %tailrecurse
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$101, %rax
	movq	$1, %rax
	jne	.LBB2_24
	.align	16, 0x90
.LBB2_22:                               # %sw.bb20
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$8, %rbx
	movq	(%rbx), %rbx
	jmp	.LBB2_1
.LBB2_17:                               # %tailrecurse
	cmpq	$75, %rax
	jne	.LBB2_18
# BB#25:                                # %sw.bb23
	movq	8(%rbx), %r14
	movq	%r14, %rdi
	callq	rtx_addr_can_trap_p
	testq	%r15, %rax
	jne	.LBB2_27
# BB#26:                                # %land.lhs.true29
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$54, %rcx
	je	.LBB2_24
.LBB2_27:                               # %lor.rhs
	movq	pic_offset_table_rtx(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB2_29
# BB#28:
	movq	$1, %rax
	jmp	.LBB2_24
.LBB2_14:                               # %tailrecurse
	cmpq	$67, %rax
	jne	.LBB2_15
# BB#37:                                # %return.loopexit
	xorq	%rax, %rax
	jmp	.LBB2_24
.LBB2_18:
	movq	$1, %rax
	jmp	.LBB2_24
.LBB2_15:                               # %tailrecurse
	cmpq	$68, %rax
	movq	$1, %rax
	jne	.LBB2_24
# BB#16:                                # %sw.bb
	movq	$30, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%rdx, %rax
	jmp	.LBB2_24
.LBB2_29:                               # %land.rhs
	movq	16(%rbx), %rax
	movzbl	(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$133, %rcx
	jg	.LBB2_32
# BB#30:                                # %land.rhs
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_34
# BB#31:                                # %land.rhs
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB2_24
	jmp	.LBB2_34
.LBB2_32:                               # %land.rhs
	cmpq	$134, %rcx
	je	.LBB2_24
# BB#33:                                # %land.rhs
	cmpq	$140, %rcx
	je	.LBB2_24
.LBB2_34:                               # %lor.rhs97
	movq	$1, %rax
	jmp	.LBB2_24
.LBB2_6:                                # %tailrecurse
	cmpq	$61, %rax
	movq	$1, %rax
	jne	.LBB2_24
# BB#7:                                 # %sw.bb4
	movq	global_rtl+24(%rip), %rcx
	xorq	%rax, %rax
	cmpq	%rbx, %rcx
	je	.LBB2_24
# BB#8:                                 # %sw.bb4
	movq	global_rtl+32(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.LBB2_24
# BB#9:                                 # %sw.bb4
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.LBB2_24
# BB#10:                                # %lor.lhs.false8
	movq	global_rtl+40(%rip), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB2_12
# BB#11:                                # %lor.lhs.false8
	movzbl	fixed_regs+16(%rip), %ecx
	testq	$255, %rcx
	jne	.LBB2_24
.LBB2_12:                               # %if.end
	movl	8(%rbx), %eax
	addq	$-53, %rax
	andq	%r15, %rax
	cmpq	$4, %rax
	jbe	.LBB2_13
# BB#23:                                # %if.end
	movabsq	$1, %rax
	jmp	.LBB2_24
.LBB2_13:
	movabsq	$0, %rax
.LBB2_24:                               # %if.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp46:
	.size	rtx_addr_can_trap_p, .Ltmp46-rtx_addr_can_trap_p
	.cfi_endproc

	.globl	rtx_addr_varies_p
	.align	16, 0x90
	.type	rtx_addr_varies_p,@function
rtx_addr_varies_p:                      # @rtx_addr_varies_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp53:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp55:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp56:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp58:
	.cfi_def_cfa_offset 80
.Ltmp59:
	.cfi_offset %rbx, -48
.Ltmp60:
	.cfi_offset %r12, -40
.Ltmp61:
	.cfi_offset %r13, -32
.Ltmp62:
	.cfi_offset %r14, -24
.Ltmp63:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	testq	%rdi, %rdi
	je	.LBB3_1
# BB#2:                                 # %if.end
	movl	(%rdi), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB3_7
# BB#3:                                 # %if.then2
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	movq	$1, %r12
	je	.LBB3_18
# BB#4:                                 # %lor.rhs
	movq	8(%rdi), %rdi
	movq	%rbx, %rsi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_5
# BB#6:                                 # %lor.rhs
	movabsq	$1, %r12
	jmp	.LBB3_18
.LBB3_1:
	xorq	%r12, %r12
	jmp	.LBB3_18
.LBB3_7:                                # %if.end6
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB3_8
# BB#9:                                 # %for.body.lr.ph
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_10:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_17 Depth 2
	movq	%rdx, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	cmpq	$69, %rcx
	jne	.LBB3_11
# BB#14:                                # %for.cond32.preheader
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %r13
	movslq	(%r13), %r14
	testq	%r14, %r14
	movq	$1, %r12
	movq	$1, %r15
	jle	.LBB3_12
	.align	16, 0x90
.LBB3_17:                               # %for.body38
                                        #   Parent Loop BB3_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13,%r15,8), %rdi
	movq	%rbx, %rsi
	callq	rtx_addr_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_18
# BB#16:                                # %for.cond32
                                        #   in Loop: Header=BB3_17 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%r14, %rax
	jl	.LBB3_17
	jmp	.LBB3_12
	.align	16, 0x90
.LBB3_11:                               # %for.body
                                        #   in Loop: Header=BB3_10 Depth=1
	cmpq	$101, %rcx
	jne	.LBB3_12
# BB#15:                                # %if.then17
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	%rbx, %rsi
	callq	rtx_addr_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	jne	.LBB3_18
.LBB3_12:                               # %for.cond.backedge
                                        #   in Loop: Header=BB3_10 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB3_10
# BB#13:
	xorq	%r12, %r12
	jmp	.LBB3_18
.LBB3_8:
	xorq	%r12, %r12
	jmp	.LBB3_18
.LBB3_5:
	movabsq	$0, %r12
.LBB3_18:                               # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp64:
	.size	rtx_addr_varies_p, .Ltmp64-rtx_addr_varies_p
	.cfi_endproc

	.globl	get_integer_term
	.align	16, 0x90
	.type	get_integer_term,@function
get_integer_term:                       # @get_integer_term
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$58, %rax
	jne	.LBB4_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movl	(%rdi), %ecx
.LBB4_2:                                # %if.end
	xorq	%rax, %rax
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB4_3
# BB#6:                                 # %land.lhs.true20
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB4_8
# BB#7:                                 # %if.then27
	movq	8(%rcx), %rax
	jmp	.LBB4_8
.LBB4_3:                                # %if.end
	cmpq	$76, %rcx
	jne	.LBB4_8
# BB#4:                                 # %land.lhs.true
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB4_8
# BB#5:                                 # %if.then10
	movq	8(%rcx), %rax
	negq	%rax
	retq
.LBB4_8:                                # %return
	retq
.Ltmp65:
	.size	get_integer_term, .Ltmp65-get_integer_term
	.cfi_endproc

	.globl	get_related_value
	.align	16, 0x90
	.type	get_related_value,@function
get_related_value:                      # @get_related_value
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$58, %rcx
	jne	.LBB5_5
# BB#1:                                 # %if.end
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$76, %rdx
	je	.LBB5_3
# BB#2:                                 # %if.end
	cmpq	$75, %rdx
	jne	.LBB5_5
.LBB5_3:                                # %land.lhs.true17
	movq	16(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$54, %rdx
	jne	.LBB5_5
# BB#4:                                 # %if.then24
	movq	8(%rcx), %rax
.LBB5_5:                                # %return
	retq
.Ltmp66:
	.size	get_related_value, .Ltmp66-get_related_value
	.cfi_endproc

	.globl	get_jump_table_offset
	.align	16, 0x90
	.type	get_jump_table_offset,@function
get_jump_table_offset:                  # @get_jump_table_offset
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
	subq	$32, %rsp
.Ltmp78:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, 24(%rsp)
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB6_1
# BB#2:                                 # %lor.lhs.false
	movq	64(%rdi), %r15
	testq	%r15, %r15
	je	.LBB6_3
# BB#4:                                 # %lor.lhs.false1
	movq	24(%r15), %r14
	testq	%r14, %r14
	je	.LBB6_5
# BB#6:                                 # %lor.lhs.false6
	movq	%rsi, (%rsp)            # 8-byte Spill
	movzbl	(%r14), %eax
	cmpq	$33, %rax
	jne	.LBB6_7
# BB#8:                                 # %lor.lhs.false10
	movq	32(%r14), %rax
	movl	(%rax), %r12d
	movq	%r12, %rax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$44, %rax
	jne	.LBB6_9
# BB#10:                                # %lor.lhs.false10
	movzbl	rtx_class+33(%rip), %eax
	andq	$255, %rax
	cmpq	$105, %rax
	jne	.LBB6_11
# BB#12:                                # %cond.true
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB6_14
# BB#13:                                # %cond.false
	movq	%rax, %rsi
	callq	single_set_2
.LBB6_14:                               # %cond.end44
	testq	%rax, %rax
	je	.LBB6_15
# BB#16:                                # %if.end
	movq	16(%rax), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$72, %rcx
	jne	.LBB6_19
# BB#17:                                # %land.lhs.true54
	movq	24(%rbx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$67, %rcx
	jne	.LBB6_19
# BB#18:                                # %if.then62
	movq	16(%rbx), %rbx
	movl	(%rbx), %eax
.LBB6_19:                               # %for.cond.preheader
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB6_20
# BB#21:                                # %for.cond.preheader
	testq	%rbx, %rbx
	je	.LBB6_22
# BB#23:
	leaq	24(%rsp), %r12
	.align	16, 0x90
.LBB6_24:                               # %for.inc
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	find_last_value
	movq	%rax, %r13
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	jne	.LBB6_26
# BB#25:                                # %for.inc
                                        #   in Loop: Header=BB6_24 Depth=1
	cmpq	%rbx, %r13
	movq	%r13, %rbx
	jne	.LBB6_24
.LBB6_26:                               # %for.cond.for.end_crit_edge
	movq	32(%r14), %rcx
	movl	(%rcx), %r12d
	jmp	.LBB6_27
.LBB6_1:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_3:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_5:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_7:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_9:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_11:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_15:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_20:
	movq	%rbx, %r13
	jmp	.LBB6_27
.LBB6_22:
	movq	%rbx, %r13
.LBB6_27:                               # %for.end
	andq	$65535, %r12            # imm = 0xFFFF
	cmpq	$45, %r12
	jne	.LBB6_48
# BB#28:                                # %for.end
	addq	$-75, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB6_48
# BB#29:                                # %for.cond92.preheader
	movq	24(%rsp), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r14, %r14
	leaq	16(%rsp), %r12
	.align	16, 0x90
.LBB6_30:                               # %for.body95
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_35 Depth 2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	8(%r13,%r14,8), %rbx
	movq	global_rtl(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB6_40
# BB#31:                                # %for.body95
                                        #   in Loop: Header=BB6_30 Depth=1
	movq	pic_offset_table_rtx(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB6_40
# BB#32:                                # %for.cond107.preheader
                                        #   in Loop: Header=BB6_30 Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_33
# BB#34:                                # %for.cond107.preheader
                                        #   in Loop: Header=BB6_30 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %rax
	je	.LBB6_37
	.align	16, 0x90
.LBB6_35:                               # %for.inc117
                                        #   Parent Loop BB6_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	find_last_value
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_37
# BB#36:                                # %for.inc117
                                        #   in Loop: Header=BB6_35 Depth=2
	cmpq	%rbx, %rax
	movq	%rax, %rbx
	jne	.LBB6_35
	jmp	.LBB6_37
.LBB6_33:                               #   in Loop: Header=BB6_30 Depth=1
	movq	%rbx, %rax
.LBB6_37:                               # %for.end119
                                        #   in Loop: Header=BB6_30 Depth=1
	cmpq	$67, %rcx
	jne	.LBB6_39
# BB#38:                                # %land.lhs.true124
                                        #   in Loop: Header=BB6_30 Depth=1
	movq	8(%rax), %rax
	cmpq	%r15, %rax
	je	.LBB6_40
.LBB6_39:                               # %for.inc132
                                        #   in Loop: Header=BB6_30 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$2, %rdx
	jl	.LBB6_30
	jmp	.LBB6_84
.LBB6_40:                               # %for.end133
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$1, %rdx
	jg	.LBB6_84
# BB#41:                                # %if.end137
	movq	$1, %rax
	subq	%r14, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%r13,%rax,8), %rbx
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB6_42
# BB#43:                                # %if.end137
	testq	%rbx, %rbx
	je	.LBB6_44
# BB#45:
	leaq	24(%rsp), %r12
.LBB6_46:                               # %for.inc152
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	find_last_value
	movq	%rax, %r13
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	jne	.LBB6_48
# BB#47:                                # %for.inc152
                                        #   in Loop: Header=BB6_46 Depth=1
	cmpq	%rbx, %r13
	movq	%r13, %rbx
	jne	.LBB6_46
	jmp	.LBB6_48
.LBB6_42:
	movq	%rbx, %r13
	jmp	.LBB6_48
.LBB6_44:
	movq	%rbx, %r13
.LBB6_48:                               # %if.end155
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$120, %rcx
	jne	.LBB6_56
# BB#49:                                # %if.then165
	movq	8(%r13), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB6_50
# BB#51:                                # %if.then165
	testq	%rbx, %rbx
	je	.LBB6_52
# BB#53:
	leaq	24(%rsp), %r12
.LBB6_54:                               # %for.inc179
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	find_last_value
	movq	%rax, %r13
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB6_56
# BB#55:                                # %for.inc179
                                        #   in Loop: Header=BB6_54 Depth=1
	cmpq	%rbx, %r13
	movq	%r13, %rbx
	jne	.LBB6_54
	jmp	.LBB6_56
.LBB6_50:
	movq	%rbx, %r13
	jmp	.LBB6_56
.LBB6_52:
	movq	%rbx, %r13
.LBB6_56:                               # %if.end182
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB6_57
# BB#58:                                # %if.end188
	movq	8(%r13), %rbx
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB6_59
# BB#60:                                # %if.end188
	testq	%rbx, %rbx
	je	.LBB6_61
# BB#62:
	leaq	24(%rsp), %r13
.LBB6_63:                               # %for.inc202
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	find_last_value
	movq	%rax, %r12
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB6_65
# BB#64:                                # %for.inc202
                                        #   in Loop: Header=BB6_63 Depth=1
	cmpq	%rbx, %r12
	movq	%r12, %rbx
	jne	.LBB6_63
	jmp	.LBB6_65
.LBB6_57:
	xorq	%rax, %rax
	jmp	.LBB6_84
.LBB6_59:
	movq	%rbx, %r12
	jmp	.LBB6_65
.LBB6_61:
	movq	%rbx, %r12
.LBB6_65:                               # %for.end204
	cmpq	$75, %rax
	movabsq	$0, %rax
	jne	.LBB6_84
# BB#66:                                # %for.cond211.preheader
	movq	24(%rsp), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%r14, %r14
	leaq	16(%rsp), %r13
.LBB6_67:                               # %for.body214
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_70 Depth 2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	8(%r12,%r14,8), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_68
# BB#69:                                # %for.body214
                                        #   in Loop: Header=BB6_67 Depth=1
	testq	%rbx, %rbx
	movq	%rbx, %rax
	je	.LBB6_72
	.align	16, 0x90
.LBB6_70:                               # %for.inc229
                                        #   Parent Loop BB6_67 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	find_last_value
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_72
# BB#71:                                # %for.inc229
                                        #   in Loop: Header=BB6_70 Depth=2
	cmpq	%rbx, %rax
	movq	%rax, %rbx
	jne	.LBB6_70
	jmp	.LBB6_72
.LBB6_68:                               #   in Loop: Header=BB6_67 Depth=1
	movq	%rbx, %rax
.LBB6_72:                               # %for.end231
                                        #   in Loop: Header=BB6_67 Depth=1
	cmpq	$58, %rcx
	je	.LBB6_74
# BB#73:                                # %for.end231
                                        #   in Loop: Header=BB6_67 Depth=1
	cmpq	$67, %rcx
	jne	.LBB6_75
.LBB6_74:                               # %land.lhs.true241
                                        #   in Loop: Header=BB6_67 Depth=1
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_76
.LBB6_75:                               # %for.inc246
                                        #   in Loop: Header=BB6_67 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$2, %rdx
	jl	.LBB6_67
	jmp	.LBB6_84
.LBB6_76:                               # %for.end248
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$1, %rdx
	jg	.LBB6_84
# BB#77:                                # %if.end252
	movq	$1, %rax
	subq	%r14, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%r12,%rax,8), %rax
	movzbl	(%rax), %ecx
	movq	$1, %rbx
	addq	$-75, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	ja	.LBB6_82
# BB#78:                                # %for.cond268.preheader
	movq	pic_offset_table_rtx(%rip), %rdi
	movq	$16, %rsi
.LBB6_80:                               # %for.body271
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rcx
	cmpq	%rdi, %rcx
	je	.LBB6_81
# BB#79:                                # %for.cond268
                                        #   in Loop: Header=BB6_80 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$-8, %rsi
	incq	%rbx
	cmpq	$1, %rdx
	jle	.LBB6_80
	jmp	.LBB6_82
.LBB6_81:                               # %if.then278
	movq	(%rax,%rsi), %rax
.LBB6_82:                               # %if.end288
	movq	(%rsp), %rcx            # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB6_84
# BB#83:                                # %if.then290
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, (%rcx)
.LBB6_84:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp84:
	.size	get_jump_table_offset, .Ltmp84-get_jump_table_offset
	.cfi_endproc

	.globl	single_set_2
	.align	16, 0x90
	.type	single_set_2,@function
single_set_2:                           # @single_set_2
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
	subq	$32, %rsp
.Ltmp96:
	.cfi_def_cfa_offset 80
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
	movzbl	(%rsi), %eax
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	cmpq	$39, %rax
	jne	.LBB7_34
# BB#1:                                 # %for.cond.preheader
	movq	8(%rsi), %rsi
	movslq	(%rsi), %r13
	movq	%r13, 16(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	jle	.LBB7_34
# BB#2:                                 # %for.body.lr.ph
	xorq	%r14, %r14
	movq	$1, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	.align	16, 0x90
.LBB7_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_11 Depth 2
                                        #     Child Loop BB7_9 Depth 2
                                        #     Child Loop BB7_28 Depth 2
                                        #     Child Loop BB7_21 Depth 2
	movq	8(%rsi,%r14,8), %r12
	movzbl	(%r12), %eax
	leaq	-48(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jb	.LBB7_27
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB7_3 Depth=1
	cmpq	$47, %rax
	jne	.LBB7_33
# BB#5:                                 # %sw.bb8
                                        #   in Loop: Header=BB7_3 Depth=1
	testq	%rdx, %rbx
	jne	.LBB7_16
# BB#6:                                 # %if.then9
                                        #   in Loop: Header=BB7_3 Depth=1
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	movq	$1, %rbx
	jne	.LBB7_18
# BB#7:                                 # %if.end.i
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	56(%rdi), %rax
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB7_16
# BB#8:                                 # %for.body.lr.ph.i
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	8(%r15), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_11
	.align	16, 0x90
.LBB7_9:                                # %for.body.us.i
                                        #   Parent Loop BB7_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %rbx
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$655360, %rcx           # imm = 0xA0000
	je	.LBB7_25
# BB#10:                                # %for.inc.us.i
                                        #   in Loop: Header=BB7_9 Depth=2
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	jne	.LBB7_9
	jmp	.LBB7_14
	.align	16, 0x90
.LBB7_11:                               # %for.body.i
                                        #   Parent Loop BB7_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	andq	$16711680, %rdx         # imm = 0xFF0000
	cmpq	$655360, %rdx           # imm = 0xA0000
	jne	.LBB7_13
# BB#12:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB7_11 Depth=2
	movq	8(%rax), %rdx
	cmpq	%rcx, %rdx
	je	.LBB7_23
.LBB7_13:                               # %for.inc.i
                                        #   in Loop: Header=BB7_11 Depth=2
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB7_11
.LBB7_14:                               #   in Loop: Header=BB7_3 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB7_15:                               # %if.end16
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	$1, %rbx
	jmp	.LBB7_16
.LBB7_23:                               # %find_reg_note.exit
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%rsi, %rbx
	testq	%rax, %rax
	jne	.LBB7_25
# BB#24:                                #   in Loop: Header=BB7_3 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rbx, %rsi
	jmp	.LBB7_15
.LBB7_25:                               # %land.lhs.true
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%r15, %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	$1, %rbx
	je	.LBB7_26
	.align	16, 0x90
.LBB7_16:                               # %if.end16
                                        #   in Loop: Header=BB7_3 Depth=1
	testq	%r15, %r15
	jne	.LBB7_17
.LBB7_26:                               #   in Loop: Header=BB7_3 Depth=1
	movq	%r12, %r15
	xorq	%rbx, %rbx
	jmp	.LBB7_27
.LBB7_17:                               # %if.end16.if.else19_crit_edge
                                        #   in Loop: Header=BB7_3 Depth=1
	movl	(%rdi), %eax
.LBB7_18:                               # %if.else19
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%r15, 8(%rsp)           # 8-byte Spill
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_33
# BB#19:                                # %if.end.i51
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%rsi, %r15
	movq	%rdx, %r13
	movq	56(%rdi), %rax
	testq	%rax, %rax
	je	.LBB7_33
# BB#20:                                # %for.body.lr.ph.i53
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	8(%r12), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_28
	.align	16, 0x90
.LBB7_21:                               # %for.body.us.i59
                                        #   Parent Loop BB7_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$655360, %rcx           # imm = 0xA0000
	je	.LBB7_32
# BB#22:                                # %for.inc.us.i64
                                        #   in Loop: Header=BB7_21 Depth=2
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB7_21
	jmp	.LBB7_33
	.align	16, 0x90
.LBB7_28:                               # %for.body.i70
                                        #   Parent Loop BB7_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rax), %edx
	andq	$16711680, %rdx         # imm = 0xFF0000
	cmpq	$655360, %rdx           # imm = 0xA0000
	jne	.LBB7_30
# BB#29:                                # %lor.lhs.false.i74
                                        #   in Loop: Header=BB7_28 Depth=2
	movq	8(%rax), %rdx
	cmpq	%rcx, %rdx
	je	.LBB7_31
.LBB7_30:                               # %for.inc.i79
                                        #   in Loop: Header=BB7_28 Depth=2
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB7_28
	jmp	.LBB7_33
.LBB7_31:                               # %find_reg_note.exit81
                                        #   in Loop: Header=BB7_3 Depth=1
	testq	%rax, %rax
	je	.LBB7_33
.LBB7_32:                               # %lor.lhs.false
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%r12, %rdi
	callq	side_effects_p
	movq	%r13, %rdx
	testq	%rdx, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %r13          # 8-byte Reload
	movq	%r15, %rsi
	movq	8(%rsp), %r15           # 8-byte Reload
	jne	.LBB7_33
	.align	16, 0x90
.LBB7_27:                               # %for.inc
                                        #   in Loop: Header=BB7_3 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r13, %rax
	jl	.LBB7_3
	jmp	.LBB7_34
.LBB7_33:
	xorq	%r15, %r15
.LBB7_34:                               # %return
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp102:
	.size	single_set_2, .Ltmp102-single_set_2
	.cfi_endproc

	.globl	find_last_value
	.align	16, 0x90
	.type	find_last_value,@function
find_last_value:                        # @find_last_value
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
	subq	$48, %rsp
.Ltmp114:
	.cfi_def_cfa_offset 96
.Ltmp115:
	.cfi_offset %rbx, -48
.Ltmp116:
	.cfi_offset %r12, -40
.Ltmp117:
	.cfi_offset %r13, -32
.Ltmp118:
	.cfi_offset %r14, -24
.Ltmp119:
	.cfi_offset %r15, -16
	movq	(%rsi), %rax
	movq	16(%rax), %rbx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB8_32
# BB#1:                                 # %land.rhs.lr.ph
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB8_2:                                # %land.rhs
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_8 Depth 2
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB8_32
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB8_2 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB8_4
# BB#5:                                 # %cond.true
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	32(%rbx), %r12
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB8_7
# BB#6:                                 # %cond.false
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	single_set_2
	movq	%rax, %r12
.LBB8_7:                                # %if.end.i
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	56(%rbx), %rax
	xorq	%r13, %r13
	jmp	.LBB8_8
	.align	16, 0x90
.LBB8_34:                               # %for.inc.us.i
                                        #   in Loop: Header=BB8_8 Depth=2
	movq	16(%rax), %rax
.LBB8_8:                                # %if.end.i
                                        #   Parent Loop BB8_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB8_11
# BB#9:                                 # %for.body.us.i
                                        #   in Loop: Header=BB8_8 Depth=2
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$262144, %rcx           # imm = 0x40000
	jne	.LBB8_34
# BB#10:                                #   in Loop: Header=BB8_2 Depth=1
	movq	%rax, %r13
	.align	16, 0x90
.LBB8_11:                               # %find_reg_note.exit
                                        #   in Loop: Header=BB8_2 Depth=1
	testq	%r12, %r12
	je	.LBB8_22
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	8(%r12), %rsi
	movq	%r15, %rdi
	callq	rtx_equal_p
	testq	%r14, %rax
	je	.LBB8_22
# BB#13:                                # %if.then37
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	16(%r12), %r12
	testq	%r13, %r13
	je	.LBB8_16
# BB#14:                                # %land.lhs.true42
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	8(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$3, %rcx
	je	.LBB8_16
# BB#15:                                # %land.lhs.true42
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%rax, %r12
.LBB8_16:                               # %if.end
                                        #   in Loop: Header=BB8_2 Depth=1
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB8_18
# BB#17:                                # %lor.lhs.false
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	16(%rbx), %rsi
	movq	%r12, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	modified_between_p
	testq	%r14, %rax
	jne	.LBB8_22
.LBB8_18:                               # %land.lhs.true61
                                        #   in Loop: Header=BB8_2 Depth=1
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB8_21
# BB#19:                                # %land.lhs.true66
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	8(%r12), %eax
	cmpq	$52, %rax
	ja	.LBB8_21
# BB#20:                                # %land.lhs.true66
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB8_21
	.align	16, 0x90
.LBB8_22:                               # %if.end75
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	jne	.LBB8_29
# BB#23:                                # %if.end75
                                        #   in Loop: Header=BB8_2 Depth=1
	cmpq	$34, %rcx
	jne	.LBB8_29
# BB#24:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB8_2 Depth=1
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	je	.LBB8_32
# BB#25:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB8_2 Depth=1
	cmpq	$61, %rax
	jne	.LBB8_27
# BB#26:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	8(%r15), %eax
	cmpq	$53, %rax
	jb	.LBB8_32
.LBB8_27:                               # %lor.lhs.false18.i
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	find_reg_fusage
	testq	%r14, %rax
	jne	.LBB8_32
# BB#28:                                # %lor.lhs.false18.if.end22_crit_edge.i
                                        #   in Loop: Header=BB8_2 Depth=1
	movl	(%rbx), %eax
.LBB8_29:                               # %if.end22.i
                                        #   in Loop: Header=BB8_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, 32(%rsp)
	movq	%r15, 40(%rsp)
	cmpq	$105, %rax
	movq	%rbx, %rdi
	jne	.LBB8_31
# BB#30:                                # %cond.true.i.i
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	32(%rbx), %rdi
.LBB8_31:                               # %reg_set_p.exit
                                        #   in Loop: Header=BB8_2 Depth=1
	movabsq	$set_of_1, %rsi
	leaq	32(%rsp), %rdx
	callq	note_stores
	cmpq	$0, 32(%rsp)
	jne	.LBB8_32
.LBB8_4:                                # %for.cond.backedge
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB8_2
.LBB8_32:
	movq	%r15, %r12
.LBB8_33:                               # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_21:                               # %if.then73
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rbx, (%rax)
	jmp	.LBB8_33
.Ltmp120:
	.size	find_last_value, .Ltmp120-find_last_value
	.cfi_endproc

	.globl	reg_mentioned_p
	.align	16, 0x90
	.type	reg_mentioned_p,@function
reg_mentioned_p:                        # @reg_mentioned_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp127:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp128:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp129:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp130:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp131:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp132:
	.cfi_def_cfa_offset 64
.Ltmp133:
	.cfi_offset %rbx, -48
.Ltmp134:
	.cfi_offset %r12, -40
.Ltmp135:
	.cfi_offset %r13, -32
.Ltmp136:
	.cfi_offset %r14, -24
.Ltmp137:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r15
	xorq	%rcx, %rcx
	testq	%rbx, %rbx
	je	.LBB9_28
# BB#1:                                 # %if.end
	cmpq	%rbx, %r15
	je	.LBB9_27
# BB#2:                                 # %if.end3
	movzbl	(%rbx), %r13d
	leaq	-54(%r13), %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	cmpq	$15, %rax
	ja	.LBB9_15
# BB#3:                                 # %if.end3
	jmpq	*.LJTI9_0(,%rax,8)
.LBB9_11:                               # %sw.bb22
	movzbl	(%r15), %eax
	xorq	%rcx, %rcx
	cmpq	$54, %rax
	jne	.LBB9_28
# BB#12:                                # %land.rhs27
	movq	8(%rbx), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB9_13
# BB#14:                                # %land.rhs27
	movabsq	$1, %rcx
	jmp	.LBB9_28
.LBB9_15:                               # %sw.epilog
	movzbl	(%r15), %eax
	cmpq	%r13, %rax
	jne	.LBB9_17
# BB#16:                                # %land.lhs.true
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	rtx_equal_p
	xorq	%rcx, %rcx
	testq	%r12, %rax
	jne	.LBB9_27
.LBB9_17:                               # %if.end43
	movzbl	rtx_length(%r13), %edx
	testq	%rdx, %rdx
	je	.LBB9_28
# BB#18:                                # %for.body.lr.ph
	movq	rtx_format(,%r13,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB9_19:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_24 Depth 2
	movq	%rdx, %r13
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	cmpq	$69, %rax
	jne	.LBB9_20
# BB#23:                                # %if.then55
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	(%rbx,%r13,8), %rax
	movslq	(%rax), %r14
	.align	16, 0x90
.LBB9_24:                               # %for.cond60
                                        #   Parent Loop BB9_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB9_21
# BB#25:                                # %for.body63
                                        #   in Loop: Header=BB9_24 Depth=2
	movq	(%rbx,%r13,8), %rax
	movq	(%rax,%r14,8), %rsi
	decq	%r14
	movq	%r15, %rdi
	callq	reg_mentioned_p
	testq	%r12, %rax
	je	.LBB9_24
	jmp	.LBB9_27
.LBB9_20:                               # %for.body
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$101, %rax
	jne	.LBB9_21
# BB#26:                                # %land.lhs.true79
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	(%rbx,%r13,8), %rsi
	movq	%r15, %rdi
	callq	reg_mentioned_p
	testq	%r12, %rax
	jne	.LBB9_27
.LBB9_21:                               # %for.cond.backedge
                                        #   in Loop: Header=BB9_19 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_19
# BB#22:
	xorq	%rcx, %rcx
	jmp	.LBB9_28
.LBB9_27:
	movq	$1, %rcx
.LBB9_28:                               # %return
	movq	%rcx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_7:                                # %sw.bb
	movzbl	(%r15), %eax
	xorq	%rcx, %rcx
	cmpq	$61, %rax
	jne	.LBB9_28
# BB#8:                                 # %land.rhs
	movl	8(%rbx), %eax
	movl	8(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB9_9
# BB#10:                                # %land.rhs
	movabsq	$1, %rcx
	jmp	.LBB9_28
.LBB9_4:                                # %if.then5
	movq	8(%rbx), %rax
	cmpq	%r15, %rax
	jne	.LBB9_5
# BB#6:                                 # %if.then5
	movabsq	$1, %rcx
	jmp	.LBB9_28
.LBB9_5:
	movabsq	$0, %rcx
	jmp	.LBB9_28
.LBB9_13:
	movabsq	$0, %rcx
	jmp	.LBB9_28
.LBB9_9:
	movabsq	$0, %rcx
	jmp	.LBB9_28
.Ltmp138:
	.size	reg_mentioned_p, .Ltmp138-reg_mentioned_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_11
	.quad	.LBB9_28
	.quad	.LBB9_28
	.quad	.LBB9_15
	.quad	.LBB9_15
	.quad	.LBB9_28
	.quad	.LBB9_15
	.quad	.LBB9_7
	.quad	.LBB9_28
	.quad	.LBB9_15
	.quad	.LBB9_15
	.quad	.LBB9_15
	.quad	.LBB9_15
	.quad	.LBB9_4
	.quad	.LBB9_15
	.quad	.LBB9_28

	.text
	.globl	count_occurrences
	.align	16, 0x90
	.type	count_occurrences,@function
count_occurrences:                      # @count_occurrences
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
	movq	%rdi, %r15
	cmpq	%rsi, %r15
	movq	$1, %rbx
	je	.LBB10_24
# BB#1:                                 # %if.end.lr.ph
	movq	%rsi, %r12
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rdx, %r13
	testq	%rsi, %rdx
	je	.LBB10_2
# BB#9:                                 # %if.end.us
	movzbl	(%r15), %r14d
	leaq	-36(%r14), %rcx
	andq	%rsi, %rcx
	cmpq	$33, %rcx
	ja	.LBB10_17
# BB#10:                                # %if.end.us
	xorq	%rbx, %rbx
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$12928679937, %rdx      # imm = 0x3029C0001
	testq	%rdx, %rax
	jne	.LBB10_24
# BB#11:                                # %if.end.us
	cmpq	$11, %rcx
	jne	.LBB10_6
# BB#12:                                # %sw.bb7.us
	movq	$47, %r14
	jmp	.LBB10_17
.LBB10_2:
	movabsq	$12928679937, %rax      # imm = 0x3029C0001
	.align	16, 0x90
.LBB10_3:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %r14d
	leaq	-36(%r14), %rcx
	andq	%rsi, %rcx
	cmpq	$33, %rcx
	ja	.LBB10_17
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB10_3 Depth=1
	xorq	%rbx, %rbx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB10_24
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB10_3 Depth=1
	cmpq	$11, %rcx
	jne	.LBB10_6
# BB#13:                                # %sw.bb7
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	8(%r15), %rcx
	cmpq	%r12, %rcx
	movq	%r12, %rcx
	movq	$47, %r14
	jne	.LBB10_14
# BB#15:                                # %if.then11
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	16(%r15), %r15
	cmpq	%rcx, %r15
	movq	%rcx, %r12
	movq	$1, %rbx
	jne	.LBB10_3
	jmp	.LBB10_24
.LBB10_14:
	movq	%rcx, %r12
	jmp	.LBB10_17
.LBB10_6:                               # %if.end
	cmpq	$30, %rcx
	jne	.LBB10_17
# BB#7:                                 # %sw.bb1
	movq	%rsi, %rbx
	movq	%r12, %rsi
	movzbl	(%rsi), %eax
	cmpq	$66, %rax
	movq	$66, %r14
	jne	.LBB10_8
# BB#16:                                # %land.lhs.true
	movq	%r15, %rdi
	movq	%rsi, %r12
	callq	rtx_equal_p
	movq	%rbx, %rsi
	testq	%rsi, %rax
	movq	$1, %rbx
	jne	.LBB10_24
	jmp	.LBB10_17
.LBB10_8:
	movq	%rsi, %r12
	movq	%rbx, %rsi
.LBB10_17:                              # %sw.epilog
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%r14), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB10_24
# BB#18:                                # %for.body.lr.ph
	movq	rtx_format(,%r14,8), %rcx
	xorq	%r14, %r14
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB10_19:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_22 Depth 2
	movsbq	(%rcx), %rax
	incq	%rcx
	andq	%rsi, %rax
	cmpq	$101, %rax
	jne	.LBB10_20
# BB#25:                                # %sw.bb23
                                        #   in Loop: Header=BB10_19 Depth=1
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	8(%r15,%r14,8), %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	count_occurrences
	addq	%rax, %rbx
	jmp	.LBB10_23
	.align	16, 0x90
.LBB10_20:                              # %for.body
                                        #   in Loop: Header=BB10_19 Depth=1
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB10_23
# BB#21:                                # %for.cond30.preheader
                                        #   in Loop: Header=BB10_19 Depth=1
	movq	8(%r15,%r14,8), %rdx
	movslq	(%rdx), %rax
	testq	%rax, %rax
	movq	$1, %r15
	jle	.LBB10_23
	.align	16, 0x90
.LBB10_22:                              # %for.body36
                                        #   Parent Loop BB10_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx,%r15,8), %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	count_occurrences
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx,%r14,8), %rdx
	addq	%rax, %rbx
	movslq	(%rdx), %rax
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rax, %rsi
	jl	.LBB10_22
	.align	16, 0x90
.LBB10_23:                              # %for.inc46
                                        #   in Loop: Header=BB10_19 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	24(%rsp), %r15          # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rcx          # 8-byte Reload
	jl	.LBB10_19
.LBB10_24:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp156:
	.size	count_occurrences, .Ltmp156-count_occurrences
	.cfi_endproc

	.globl	no_labels_between_p
	.align	16, 0x90
	.type	no_labels_between_p,@function
no_labels_between_p:                    # @no_labels_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rsi, %rdi
	je	.LBB11_3
	.align	16, 0x90
.LBB11_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdi
	cmpq	%rsi, %rdi
	movq	$1, %rax
	je	.LBB11_4
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB11_1 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$36, %rax
	jne	.LBB11_1
.LBB11_3:
	xorq	%rax, %rax
.LBB11_4:                               # %return
	retq
.Ltmp157:
	.size	no_labels_between_p, .Ltmp157-no_labels_between_p
	.cfi_endproc

	.globl	no_jumps_between_p
	.align	16, 0x90
	.type	no_jumps_between_p,@function
no_jumps_between_p:                     # @no_jumps_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	.align	16, 0x90
.LBB12_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdi
	cmpq	%rsi, %rdi
	movq	$1, %rax
	je	.LBB12_3
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB12_1 Depth=1
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$33, %rcx
	jne	.LBB12_1
.LBB12_3:                               # %return
	retq
.Ltmp158:
	.size	no_jumps_between_p, .Ltmp158-no_jumps_between_p
	.cfi_endproc

	.globl	reg_used_between_p
	.align	16, 0x90
	.type	reg_used_between_p,@function
reg_used_between_p:                     # @reg_used_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp165:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp166:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp167:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp168:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp169:
	.cfi_def_cfa_offset 48
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
	movq	%rdx, %r15
	movq	%rdi, %r14
	cmpq	%r15, %rsi
	je	.LBB13_9
# BB#1:                                 # %for.cond.preheader
	movq	24(%rsi), %rbx
	cmpq	%r15, %rbx
	je	.LBB13_9
# BB#2:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$1, %r12
	.align	16, 0x90
.LBB13_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_8
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r13, %rax
	jne	.LBB13_10
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB13_3 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB13_8
# BB#6:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	$48, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_fusage
	testq	%r13, %rax
	jne	.LBB13_10
# BB#7:                                 # %lor.lhs.false15
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_fusage
	testq	%r13, %rax
	jne	.LBB13_10
.LBB13_8:                               # %for.cond.backedge
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r15, %rbx
	jne	.LBB13_3
.LBB13_9:
	xorq	%r12, %r12
.LBB13_10:                              # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp175:
	.size	reg_used_between_p, .Ltmp175-reg_used_between_p
	.cfi_endproc

	.globl	reg_overlap_mentioned_p
	.align	16, 0x90
	.type	reg_overlap_mentioned_p,@function
reg_overlap_mentioned_p:                # @reg_overlap_mentioned_p
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
.Ltmp187:
	.cfi_offset %rbx, -48
.Ltmp188:
	.cfi_offset %r12, -40
.Ltmp189:
	.cfi_offset %r13, -32
.Ltmp190:
	.cfi_offset %r14, -24
.Ltmp191:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	(%r12), %eax
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$64, %rcx
	jne	.LBB14_2
# BB#1:                                 # %if.then
	movq	8(%r12), %r12
	movl	(%r12), %eax
.LBB14_2:                               # %if.end
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$133, %rdx
	jg	.LBB14_7
# BB#3:                                 # %if.end
	leaq	-54(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB14_9
# BB#4:                                 # %if.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB14_5
	jmp	.LBB14_9
.LBB14_7:                               # %if.end
	cmpq	$134, %rdx
	je	.LBB14_5
# BB#8:                                 # %if.end
	cmpq	$140, %rdx
	je	.LBB14_5
.LBB14_9:                               # %lor.lhs.false31
	movzbl	(%r14), %esi
	cmpq	$133, %rsi
	jg	.LBB14_12
# BB#10:                                # %lor.lhs.false31
	leaq	-54(%rsi), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB14_14
# BB#11:                                # %lor.lhs.false31
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB14_5
	jmp	.LBB14_14
.LBB14_12:                              # %lor.lhs.false31
	cmpq	$134, %rsi
	je	.LBB14_5
# BB#13:                                # %lor.lhs.false31
	cmpq	$140, %rsi
	je	.LBB14_5
.LBB14_14:                              # %if.end64
	cmpq	$58, %rdx
	jg	.LBB14_22
# BB#15:                                # %if.end64
	cmpq	$39, %rdx
	jne	.LBB14_50
# BB#16:                                # %sw.bb177
	movq	8(%r12), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB14_5
# BB#17:                                # %for.body185.lr.ph
	decq	%rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$1, %r15
	jmp	.LBB14_18
	.align	16, 0x90
.LBB14_21:                              # %for.cond182.backedge.for.body185_crit_edge
                                        #   in Loop: Header=BB14_18 Depth=1
	movq	8(%r12), %rax
	decq	%rbx
.LBB14_18:                              # %for.body185
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%rbx,8), %rax
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB14_20
# BB#19:                                # %land.lhs.true196
                                        #   in Loop: Header=BB14_18 Depth=1
	movq	%r14, %rsi
	callq	reg_overlap_mentioned_p
	testq	%r13, %rax
	jne	.LBB14_6
.LBB14_20:                              # %for.cond182.backedge
                                        #   in Loop: Header=BB14_18 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB14_21
	jmp	.LBB14_5
.LBB14_22:                              # %if.end64
	addq	$-59, %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	cmpq	$10, %rdx
	ja	.LBB14_50
# BB#23:                                # %if.end64
	jmpq	*.LJTI14_0(,%rdx,8)
.LBB14_49:                              # %sw.bb175
	movq	%r12, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	reg_mentioned_p  # TAILCALL
.LBB14_26:                              # %sw.bb75
	movl	8(%r12), %r15d
	jmp	.LBB14_27
.LBB14_24:                              # %sw.bb
	movq	8(%r12), %rcx
	movl	8(%rcx), %r15d
	cmpq	$52, %r15
	movq	$1, %rsi
	ja	.LBB14_42
# BB#25:                                # %if.then73
	movl	16(%r12), %edx
	movzbl	2(%rcx), %esi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r15, %rdi
	movq	%rax, %rcx
	callq	subreg_regno_offset
	addq	%rax, %r15
.LBB14_27:                              # %do_reg
	movq	%r15, %rax
	andq	%rbx, %rax
	movq	$1, %rsi
	cmpq	$52, %rax
	ja	.LBB14_42
# BB#28:                                # %cond.true
	movzbl	2(%r12), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r15, %rcx
	cmpq	$8, %rcx
	je	.LBB14_32
# BB#29:                                # %cond.true
	leaq	-21(%r15), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB14_32
# BB#30:                                # %cond.true
	leaq	-45(%r15), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB14_32
# BB#31:                                # %cond.true
	leaq	-29(%r15), %rcx
	andq	%rbx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB14_32
# BB#37:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB14_38
# BB#40:                                # %cond.true116
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB14_42
.LBB14_43:                              # %sw.bb142
	movq	$1, %r15
	cmpq	$66, %rsi
	je	.LBB14_6
# BB#44:                                # %if.end148
	movzbl	rtx_length(%rsi), %ebx
	testq	%rbx, %rbx
	je	.LBB14_5
# BB#45:                                # %for.body.lr.ph
	movq	rtx_format(,%rsi,8), %r13
	decq	%rbx
.LBB14_46:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13,%rbx), %eax
	cmpq	$101, %rax
	jne	.LBB14_48
# BB#47:                                # %land.lhs.true166
                                        #   in Loop: Header=BB14_46 Depth=1
	movq	8(%r14,%rbx,8), %rsi
	movq	%r12, %rdi
	callq	reg_overlap_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_6
.LBB14_48:                              # %for.cond.backedge
                                        #   in Loop: Header=BB14_46 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB14_46
.LBB14_5:
	xorq	%r15, %r15
.LBB14_6:                               # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB14_50:                              # %sw.epilog
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.reg_overlap_mentioned_p, %rdx
	movq	$1459, %rsi             # imm = 0x5B3
	callq	fancy_abort
.LBB14_32:                              # %cond.true94
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB14_42
# BB#33:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB14_34
# BB#35:                                # %lor.rhs
	movabsq	$1, %rsi
	incq	%rsi
	jmp	.LBB14_42
.LBB14_38:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB14_41
# BB#39:                                # %cond.true109
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB14_42
.LBB14_34:
	movabsq	$0, %rsi
	incq	%rsi
	jmp	.LBB14_42
.LBB14_41:                              # %cond.false120
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
.LBB14_42:                              # %cond.end138
	addq	%r15, %rsi
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	refers_to_regno_p  # TAILCALL
.Ltmp192:
	.size	reg_overlap_mentioned_p, .Ltmp192-reg_overlap_mentioned_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_49
	.quad	.LBB14_50
	.quad	.LBB14_26
	.quad	.LBB14_49
	.quad	.LBB14_24
	.quad	.LBB14_50
	.quad	.LBB14_50
	.quad	.LBB14_43
	.quad	.LBB14_50
	.quad	.LBB14_50
	.quad	.LBB14_49

	.text
	.globl	find_reg_fusage
	.align	16, 0x90
	.type	find_reg_fusage,@function
find_reg_fusage:                        # @find_reg_fusage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp199:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp200:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp201:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp202:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp203:
	.cfi_def_cfa_offset 48
.Ltmp204:
	.cfi_offset %rbx, -48
.Ltmp205:
	.cfi_offset %r12, -40
.Ltmp206:
	.cfi_offset %r13, -32
.Ltmp207:
	.cfi_offset %r14, -24
.Ltmp208:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	movq	%rdx, %r12
	movq	%rsi, %r15
	cmpq	$34, %rax
	jne	.LBB15_1
# BB#2:                                 # %if.end
	testq	%r12, %r12
	je	.LBB15_33
# BB#3:                                 # %if.end2
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB15_4
# BB#11:                                # %if.else
	movl	8(%r12), %ebx
	cmpq	$52, %rbx
	ja	.LBB15_12
# BB#13:                                # %if.then29
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rbx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB15_17
# BB#14:                                # %if.then29
	leaq	-21(%rbx), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB15_17
# BB#15:                                # %if.then29
	leaq	-45(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB15_17
# BB#16:                                # %if.then29
	leaq	-29(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB15_17
# BB#22:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB15_23
# BB#25:                                # %cond.true66
	movzbl	target_flags+3(%rip), %r12d
	andq	$2, %r12
	xorq	$6, %r12
	jmp	.LBB15_27
.LBB15_1:
	xorq	%rax, %rax
	jmp	.LBB15_32
.LBB15_4:                               # %if.then6
	movq	64(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB15_5
# BB#6:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %r15
	.align	16, 0x90
.LBB15_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	%r15, %rcx
	jne	.LBB15_9
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_7 Depth=1
	movq	8(%rax), %rsi
	movq	%r12, %rdi
	callq	rtx_equal_p
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB15_32
.LBB15_9:                               # %for.inc
                                        #   in Loop: Header=BB15_7 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB15_7
# BB#10:
	xorq	%rax, %rax
	jmp	.LBB15_32
.LBB15_12:
	xorq	%rax, %rax
	jmp	.LBB15_32
.LBB15_5:
	xorq	%rax, %rax
	jmp	.LBB15_32
.LBB15_17:                              # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r12
	je	.LBB15_27
# BB#18:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB15_19
# BB#20:                                # %lor.rhs
	movabsq	$1, %r12
	incq	%r12
	jmp	.LBB15_27
.LBB15_23:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB15_26
# BB#24:                                # %cond.true58
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	xorq	$3, %r12
	jmp	.LBB15_27
.LBB15_19:
	movabsq	$0, %r12
	incq	%r12
	jmp	.LBB15_27
.LBB15_26:                              # %cond.false70
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r12
.LBB15_27:                              # %cond.end85
	addq	%rbx, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r12
	cmpq	%r12, %rbx
	jae	.LBB15_28
	.align	16, 0x90
.LBB15_31:                              # %for.body91
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	find_regno_fusage
	testq	%r13, %rax
	movq	$1, %rax
	jne	.LBB15_32
# BB#29:                                # %for.cond88
                                        #   in Loop: Header=BB15_31 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	%r12, %rax
	jb	.LBB15_31
# BB#30:
	xorq	%rax, %rax
	jmp	.LBB15_32
.LBB15_28:
	xorq	%rax, %rax
.LBB15_32:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_33:                              # %if.then1
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reg_fusage, %rcx
	movq	$1862, %rsi             # imm = 0x746
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp209:
	.size	find_reg_fusage, .Ltmp209-find_reg_fusage
	.cfi_endproc

	.globl	reg_referenced_p
	.align	16, 0x90
	.type	reg_referenced_p,@function
reg_referenced_p:                       # @reg_referenced_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp216:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp217:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp218:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp219:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp220:
	.cfi_def_cfa_offset 48
.Ltmp221:
	.cfi_offset %rbx, -48
.Ltmp222:
	.cfi_offset %r12, -40
.Ltmp223:
	.cfi_offset %r13, -32
.Ltmp224:
	.cfi_offset %r14, -24
.Ltmp225:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB16_1
	.align	16, 0x90
.LBB16_32:                              # %if.end178
                                        #   in Loop: Header=BB16_1 Depth=1
	movq	16(%r15), %r15
.LBB16_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %eax
	cmpq	$71, %rax
	jg	.LBB16_16
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB16_1 Depth=1
	addq	$-38, %rax
	andq	%r12, %rax
	xorq	%rbx, %rbx
	cmpq	$14, %rax
	ja	.LBB16_34
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB16_1 Depth=1
	jmpq	*.LJTI16_0(,%rax,8)
.LBB16_31:                              # %sw.bb171
                                        #   in Loop: Header=BB16_1 Depth=1
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	movq	$1, %rbx
	je	.LBB16_32
	jmp	.LBB16_34
.LBB16_16:                              # %tailrecurse
	cmpq	$72, %rax
	movabsq	$0, %rbx
	jne	.LBB16_34
.LBB16_17:                              # %sw.bb93
	movq	%r14, %rdi
	movq	%r15, %rsi
.LBB16_18:                              # %sw.bb93
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	reg_overlap_mentioned_p  # TAILCALL
.LBB16_20:                              # %sw.bb105
	movq	8(%r15), %rax
	movslq	(%rax), %r13
	movq	$1, %rbx
.LBB16_21:                              # %for.cond111
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB16_22
# BB#23:                                # %for.body114
                                        #   in Loop: Header=BB16_21 Depth=1
	movq	8(%r15), %rax
	movq	(%rax,%r13,8), %rsi
	decq	%r13
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	je	.LBB16_21
	jmp	.LBB16_34
.LBB16_19:                              # %sw.bb100
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	jmp	.LBB16_18
.LBB16_22:
	xorq	%rbx, %rbx
	jmp	.LBB16_34
.LBB16_24:                              # %sw.bb128
	movq	8(%r15), %rax
	movslq	(%rax), %r13
	movq	$1, %rbx
.LBB16_25:                              # %for.cond134
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB16_26
# BB#27:                                # %for.body137
                                        #   in Loop: Header=BB16_25 Depth=1
	movq	8(%r15), %rax
	movq	(%rax,%r13,8), %rsi
	decq	%r13
	movq	%r14, %rdi
	callq	reg_referenced_p
	testq	%r12, %rax
	je	.LBB16_25
	jmp	.LBB16_34
.LBB16_13:                              # %sw.bb78
	movq	32(%r15), %rax
	movslq	(%rax), %r13
	movq	$1, %rbx
.LBB16_14:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB16_15
# BB#33:                                # %for.body
                                        #   in Loop: Header=BB16_14 Depth=1
	movq	32(%r15), %rax
	movq	(%rax,%r13,8), %rsi
	decq	%r13
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	je	.LBB16_14
	jmp	.LBB16_34
.LBB16_4:                               # %sw.bb
	movq	16(%r15), %rsi
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	movq	$1, %rbx
	jne	.LBB16_34
# BB#5:                                 # %if.end
	movq	8(%r15), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$68, %rcx
	jg	.LBB16_35
# BB#6:                                 # %if.end
	cmpq	$59, %rcx
	je	.LBB16_12
# BB#7:                                 # %if.end
	cmpq	$61, %rcx
	je	.LBB16_12
# BB#8:                                 # %if.end
	cmpq	$63, %rcx
	jne	.LBB16_11
# BB#9:                                 # %land.lhs.true26
	movq	8(%rsi), %rcx
	movl	(%rcx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB16_11
# BB#10:                                # %land.lhs.true36
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %edx
	movl	target_flags(%rip), %edi
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	4(%rdi), %r8
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %r9d
	leaq	3(%rdi,%rdx), %rax
	cqto
	idivq	%r8
	movq	%rax, %rcx
	leaq	3(%r9,%rdi), %rax
	cqto
	idivq	%r8
	andq	%r12, %rcx
	andq	%r12, %rax
	cmpq	%rax, %rcx
	je	.LBB16_12
	jmp	.LBB16_11
.LBB16_28:                              # %sw.bb151
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB16_30
# BB#29:                                # %if.then159
	movq	8(%rax), %rsi
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	movq	$1, %rbx
	jne	.LBB16_34
.LBB16_30:                              # %if.end170
	xorq	%rbx, %rbx
	jmp	.LBB16_34
.LBB16_26:
	xorq	%rbx, %rbx
	jmp	.LBB16_34
.LBB16_15:
	xorq	%rbx, %rbx
	jmp	.LBB16_34
.LBB16_35:                              # %if.end
	cmpq	$69, %rcx
	je	.LBB16_12
.LBB16_11:                              # %land.lhs.true70
	movq	%r14, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	jne	.LBB16_34
.LBB16_12:                              # %if.end77
	xorq	%rbx, %rbx
.LBB16_34:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp226:
	.size	reg_referenced_p, .Ltmp226-reg_referenced_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI16_0:
	.quad	.LBB16_31
	.quad	.LBB16_24
	.quad	.LBB16_34
	.quad	.LBB16_13
	.quad	.LBB16_20
	.quad	.LBB16_20
	.quad	.LBB16_34
	.quad	.LBB16_34
	.quad	.LBB16_19
	.quad	.LBB16_4
	.quad	.LBB16_17
	.quad	.LBB16_28
	.quad	.LBB16_17
	.quad	.LBB16_34
	.quad	.LBB16_19

	.text
	.globl	reg_referenced_between_p
	.align	16, 0x90
	.type	reg_referenced_between_p,@function
reg_referenced_between_p:               # @reg_referenced_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp233:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp234:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp235:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp236:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp237:
	.cfi_def_cfa_offset 48
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
	movq	%rdx, %r15
	movq	%rdi, %r14
	cmpq	%r15, %rsi
	je	.LBB17_8
# BB#1:                                 # %for.cond.preheader
	movq	24(%rsi), %rbx
	cmpq	%r15, %rbx
	je	.LBB17_8
# BB#2:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$1, %r12
	.align	16, 0x90
.LBB17_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB17_7
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	callq	reg_referenced_p
	testq	%r13, %rax
	jne	.LBB17_9
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB17_3 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB17_7
# BB#6:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	$48, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_fusage
	testq	%r13, %rax
	jne	.LBB17_9
.LBB17_7:                               # %for.cond.backedge
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r15, %rbx
	jne	.LBB17_3
.LBB17_8:
	xorq	%r12, %r12
.LBB17_9:                               # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp243:
	.size	reg_referenced_between_p, .Ltmp243-reg_referenced_between_p
	.cfi_endproc

	.globl	reg_set_between_p
	.align	16, 0x90
	.type	reg_set_between_p,@function
reg_set_between_p:                      # @reg_set_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp250:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp251:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp252:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp253:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp254:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp255:
	.cfi_def_cfa_offset 64
.Ltmp256:
	.cfi_offset %rbx, -48
.Ltmp257:
	.cfi_offset %r12, -40
.Ltmp258:
	.cfi_offset %r13, -32
.Ltmp259:
	.cfi_offset %r14, -24
.Ltmp260:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rdi, %r14
	cmpq	%r15, %rsi
	je	.LBB18_1
# BB#2:                                 # %for.cond.preheader
	movq	24(%rsi), %rbx
	cmpq	%r15, %rbx
	je	.LBB18_3
# BB#4:                                 # %for.body.lr.ph
	leaq	(%rsp), %r13
	.align	16, 0x90
.LBB18_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	jne	.LBB18_6
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_5 Depth=1
	cmpq	$34, %rcx
	jne	.LBB18_14
# BB#9:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB18_5 Depth=1
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	movq	$1, %r12
	je	.LBB18_17
# BB#10:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB18_5 Depth=1
	cmpq	$61, %rax
	jne	.LBB18_12
# BB#11:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB18_5 Depth=1
	movl	8(%r14), %eax
	cmpq	$53, %rax
	jb	.LBB18_17
.LBB18_12:                              # %lor.lhs.false18.i
                                        #   in Loop: Header=BB18_5 Depth=1
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_fusage
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_17
# BB#13:                                # %lor.lhs.false18.if.end22_crit_edge.i
                                        #   in Loop: Header=BB18_5 Depth=1
	movl	(%rbx), %eax
.LBB18_14:                              # %if.end22.i
                                        #   in Loop: Header=BB18_5 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, (%rsp)
	movq	%r14, 8(%rsp)
	cmpq	$105, %rax
	movq	%rbx, %rdi
	jne	.LBB18_16
# BB#15:                                # %cond.true.i.i
                                        #   in Loop: Header=BB18_5 Depth=1
	movq	32(%rbx), %rdi
.LBB18_16:                              # %reg_set_p.exit
                                        #   in Loop: Header=BB18_5 Depth=1
	movabsq	$set_of_1, %rsi
	movq	%r13, %rdx
	callq	note_stores
	cmpq	$0, (%rsp)
	movq	$1, %r12
	jne	.LBB18_17
.LBB18_6:                               # %for.cond.backedge
                                        #   in Loop: Header=BB18_5 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r15, %rbx
	jne	.LBB18_5
# BB#7:
	xorq	%r12, %r12
	jmp	.LBB18_17
.LBB18_1:
	xorq	%r12, %r12
	jmp	.LBB18_17
.LBB18_3:
	xorq	%r12, %r12
.LBB18_17:                              # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp261:
	.size	reg_set_between_p, .Ltmp261-reg_set_between_p
	.cfi_endproc

	.globl	reg_set_p
	.align	16, 0x90
	.type	reg_set_p,@function
reg_set_p:                              # @reg_set_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp266:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp267:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp268:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp269:
	.cfi_def_cfa_offset 48
.Ltmp270:
	.cfi_offset %rbx, -32
.Ltmp271:
	.cfi_offset %r14, -24
.Ltmp272:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	movq	%rdi, %r14
	cmpq	$105, %rdx
	jne	.LBB19_7
# BB#1:                                 # %entry
	cmpq	$34, %rcx
	jne	.LBB19_7
# BB#2:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	movq	$1, %r15
	je	.LBB19_12
# BB#3:                                 # %land.lhs.true
	cmpq	$61, %rax
	jne	.LBB19_5
# BB#4:                                 # %land.lhs.true10
	movl	8(%r14), %eax
	cmpq	$53, %rax
	jb	.LBB19_12
.LBB19_5:                               # %lor.lhs.false18
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_reg_fusage
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_12
# BB#6:                                 # %lor.lhs.false18.if.end22_crit_edge
	movl	(%rbx), %eax
.LBB19_7:                               # %if.end22
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, (%rsp)
	movq	%r14, 8(%rsp)
	cmpq	$105, %rax
	jne	.LBB19_9
# BB#8:                                 # %cond.true.i
	movq	32(%rbx), %rbx
.LBB19_9:                               # %set_of.exit
	movabsq	$set_of_1, %rsi
	leaq	(%rsp), %rdx
	movq	%rbx, %rdi
	callq	note_stores
	cmpq	$0, (%rsp)
	je	.LBB19_10
# BB#11:                                # %set_of.exit
	movabsq	$1, %r15
	jmp	.LBB19_12
.LBB19_10:
	movabsq	$0, %r15
.LBB19_12:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp273:
	.size	reg_set_p, .Ltmp273-reg_set_p
	.cfi_endproc

	.globl	set_of
	.align	16, 0x90
	.type	set_of,@function
set_of:                                 # @set_of
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp275:
	.cfi_def_cfa_offset 32
	movq	$0, 8(%rsp)
	movq	%rdi, 16(%rsp)
	movzbl	(%rsi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB20_2
# BB#1:                                 # %cond.true
	movq	32(%rsi), %rsi
.LBB20_2:                               # %cond.end
	movabsq	$set_of_1, %rax
	leaq	8(%rsp), %rdx
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	note_stores
	movq	8(%rsp), %rax
	addq	$24, %rsp
	retq
.Ltmp276:
	.size	set_of, .Ltmp276-set_of
	.cfi_endproc

	.globl	regs_set_between_p
	.align	16, 0x90
	.type	regs_set_between_p,@function
regs_set_between_p:                     # @regs_set_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp283:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp284:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp285:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp286:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp287:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp288:
	.cfi_def_cfa_offset 96
.Ltmp289:
	.cfi_offset %rbx, -48
.Ltmp290:
	.cfi_offset %r12, -40
.Ltmp291:
	.cfi_offset %r13, -32
.Ltmp292:
	.cfi_offset %r14, -24
.Ltmp293:
	.cfi_offset %r15, -16
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movzbl	(%rdi), %eax
	movq	%rsi, %rbx
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$15, %rcx
	ja	.LBB21_21
# BB#1:                                 # %entry
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$57399, %rsi            # imm = 0xE037
	je	.LBB21_3
# BB#2:
	xorq	%r12, %r12
	jmp	.LBB21_33
.LBB21_3:                               # %entry
	cmpq	$7, %rcx
	jne	.LBB21_21
# BB#4:                                 # %sw.bb1
	cmpq	%rdx, %rbx
	je	.LBB21_5
# BB#6:                                 # %for.cond.preheader.i
	movq	24(%rbx), %rbx
	cmpq	%rdx, %rbx
	je	.LBB21_7
# BB#8:                                 # %for.body.lr.ph.i
	movabsq	$set_of_1, %r15
	leaq	32(%rsp), %r13
.LBB21_9:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r14
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	jne	.LBB21_10
# BB#12:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB21_9 Depth=1
	cmpq	$34, %rcx
	jne	.LBB21_18
# BB#13:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	movq	$1, %r12
	je	.LBB21_33
# BB#14:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	cmpq	$61, %rax
	jne	.LBB21_16
# BB#15:                                # %land.lhs.true10.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB21_33
.LBB21_16:                              # %lor.lhs.false18.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	find_reg_fusage
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB21_33
# BB#17:                                # %lor.lhs.false18.if.end22_crit_edge.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movl	(%rbx), %eax
.LBB21_18:                              # %if.end22.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, 32(%rsp)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 40(%rsp)
	cmpq	$105, %rax
	movq	%rbx, %rdi
	jne	.LBB21_20
# BB#19:                                # %cond.true.i.i.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movq	32(%rbx), %rdi
.LBB21_20:                              # %reg_set_p.exit.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	note_stores
	cmpq	$0, 32(%rsp)
	movq	$1, %r12
	jne	.LBB21_33
.LBB21_10:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB21_9 Depth=1
	movq	24(%rbx), %rbx
	movq	%r14, %rdx
	cmpq	%rdx, %rbx
	jne	.LBB21_9
# BB#11:
	xorq	%r12, %r12
	jmp	.LBB21_33
.LBB21_21:                              # %sw.epilog
	movzbl	rtx_length(%rax), %r13d
	testq	%r13, %r13
	je	.LBB21_22
# BB#23:                                # %for.body.lr.ph
	movq	%rdx, %r15
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB21_24:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_31 Depth 2
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$101, %rax
	jne	.LBB21_27
# BB#25:                                # %land.lhs.true
                                        #   in Loop: Header=BB21_24 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	regs_set_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	jne	.LBB21_33
# BB#26:                                # %if.elsethread-pre-split
                                        #   in Loop: Header=BB21_24 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rax,%rcx), %eax
.LBB21_27:                              # %if.else
                                        #   in Loop: Header=BB21_24 Depth=1
	andq	$255, %rax
	cmpq	$69, %rax
	jne	.LBB21_28
# BB#30:                                # %if.then18
                                        #   in Loop: Header=BB21_24 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rax
	movslq	(%rax), %r14
	.align	16, 0x90
.LBB21_31:                              # %for.cond23
                                        #   Parent Loop BB21_24 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB21_28
# BB#32:                                # %for.body26
                                        #   in Loop: Header=BB21_31 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rax
	movq	(%rax,%r14,8), %rdi
	decq	%r14
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	regs_set_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	je	.LBB21_31
	jmp	.LBB21_33
	.align	16, 0x90
.LBB21_28:                              # %for.cond.backedge
                                        #   in Loop: Header=BB21_24 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %r13
	jg	.LBB21_24
# BB#29:
	xorq	%r12, %r12
	jmp	.LBB21_33
.LBB21_22:
	xorq	%r12, %r12
.LBB21_33:                              # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB21_5:
	xorq	%r12, %r12
	jmp	.LBB21_33
.LBB21_7:
	xorq	%r12, %r12
	jmp	.LBB21_33
.Ltmp294:
	.size	regs_set_between_p, .Ltmp294-regs_set_between_p
	.cfi_endproc

	.globl	modified_between_p
	.align	16, 0x90
	.type	modified_between_p,@function
modified_between_p:                     # @modified_between_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp301:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp302:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp303:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp304:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp305:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp306:
	.cfi_def_cfa_offset 96
.Ltmp307:
	.cfi_offset %rbx, -48
.Ltmp308:
	.cfi_offset %r12, -40
.Ltmp309:
	.cfi_offset %r13, -32
.Ltmp310:
	.cfi_offset %r14, -24
.Ltmp311:
	.cfi_offset %r15, -16
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movl	(%rdi), %ecx
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-54(%rax), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$15, %rdx
	ja	.LBB22_21
# BB#1:                                 # %entry
	xorq	%r12, %r12
	jmpq	*.LJTI22_0(,%rdx,8)
.LBB22_2:                               # %sw.bb1
	movq	$1, %r12
	jmp	.LBB22_33
.LBB22_3:                               # %sw.bb5
	movq	%r15, %r14
	cmpq	%r14, %rbx
	je	.LBB22_4
# BB#5:                                 # %for.cond.preheader.i
	movq	24(%rbx), %rbx
	cmpq	%r14, %rbx
	je	.LBB22_6
# BB#7:                                 # %for.body.lr.ph.i
	movabsq	$set_of_1, %r15
	leaq	32(%rsp), %r13
	.align	16, 0x90
.LBB22_8:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	jne	.LBB22_9
# BB#11:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB22_8 Depth=1
	cmpq	$34, %rcx
	jne	.LBB22_17
# BB#12:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	movq	$1, %r12
	je	.LBB22_33
# BB#13:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	cmpq	$61, %rax
	jne	.LBB22_15
# BB#14:                                # %land.lhs.true10.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB22_33
.LBB22_15:                              # %lor.lhs.false18.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	find_reg_fusage
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB22_33
# BB#16:                                # %lor.lhs.false18.if.end22_crit_edge.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movl	(%rbx), %eax
.LBB22_17:                              # %if.end22.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, 32(%rsp)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 40(%rsp)
	cmpq	$105, %rax
	movq	%rbx, %rdi
	jne	.LBB22_19
# BB#18:                                # %cond.true.i.i.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	32(%rbx), %rdi
.LBB22_19:                              # %reg_set_p.exit.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	note_stores
	cmpq	$0, 32(%rsp)
	movq	$1, %r12
	jne	.LBB22_33
.LBB22_9:                               # %for.cond.backedge.i
                                        #   in Loop: Header=BB22_8 Depth=1
	movq	24(%rbx), %rbx
	cmpq	%r14, %rbx
	jne	.LBB22_8
# BB#10:
	xorq	%r12, %r12
	jmp	.LBB22_33
.LBB22_20:                              # %sw.bb2
	testq	$67108864, %rcx         # imm = 0x4000000
	movq	$1, %r12
	je	.LBB22_33
.LBB22_21:                              # %sw.epilog
	movzbl	rtx_length(%rax), %r13d
	testq	%r13, %r13
	je	.LBB22_22
# BB#23:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB22_24:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB22_31 Depth 2
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$101, %rax
	jne	.LBB22_27
# BB#25:                                # %land.lhs.true
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	modified_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	jne	.LBB22_33
# BB#26:                                # %if.elsethread-pre-split
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rax,%rcx), %eax
.LBB22_27:                              # %if.else
                                        #   in Loop: Header=BB22_24 Depth=1
	andq	$255, %rax
	cmpq	$69, %rax
	jne	.LBB22_28
# BB#30:                                # %if.then24
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rax
	movslq	(%rax), %r14
	.align	16, 0x90
.LBB22_31:                              # %for.cond29
                                        #   Parent Loop BB22_24 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB22_28
# BB#32:                                # %for.body32
                                        #   in Loop: Header=BB22_31 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r13,8), %rax
	movq	(%rax,%r14,8), %rdi
	decq	%r14
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	modified_between_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r12
	je	.LBB22_31
	jmp	.LBB22_33
	.align	16, 0x90
.LBB22_28:                              # %for.cond.backedge
                                        #   in Loop: Header=BB22_24 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %r13
	jg	.LBB22_24
# BB#29:
	xorq	%r12, %r12
	jmp	.LBB22_33
.LBB22_22:
	xorq	%r12, %r12
.LBB22_33:                              # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB22_4:
	xorq	%r12, %r12
	jmp	.LBB22_33
.LBB22_6:
	xorq	%r12, %r12
	jmp	.LBB22_33
.Ltmp312:
	.size	modified_between_p, .Ltmp312-modified_between_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI22_0:
	.quad	.LBB22_33
	.quad	.LBB22_33
	.quad	.LBB22_33
	.quad	.LBB22_21
	.quad	.LBB22_33
	.quad	.LBB22_2
	.quad	.LBB22_21
	.quad	.LBB22_3
	.quad	.LBB22_21
	.quad	.LBB22_21
	.quad	.LBB22_21
	.quad	.LBB22_21
	.quad	.LBB22_20
	.quad	.LBB22_33
	.quad	.LBB22_33
	.quad	.LBB22_2

	.text
	.globl	modified_in_p
	.align	16, 0x90
	.type	modified_in_p,@function
modified_in_p:                          # @modified_in_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp319:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp320:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp321:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp322:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp323:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp324:
	.cfi_def_cfa_offset 80
.Ltmp325:
	.cfi_offset %rbx, -48
.Ltmp326:
	.cfi_offset %r12, -40
.Ltmp327:
	.cfi_offset %r13, -32
.Ltmp328:
	.cfi_offset %r14, -24
.Ltmp329:
	.cfi_offset %r15, -16
	movl	(%rdi), %ecx
	movq	%rdi, %r14
	movq	%rsi, %r15
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-54(%rax), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$15, %rdx
	ja	.LBB23_15
# BB#1:                                 # %entry
	xorq	%rbx, %rbx
	jmpq	*.LJTI23_0(,%rdx,8)
.LBB23_2:                               # %sw.bb1
	movq	$1, %rbx
	jmp	.LBB23_27
.LBB23_3:                               # %sw.bb5
	movq	%r15, %rdi
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	jne	.LBB23_8
# BB#4:                                 # %sw.bb5
	cmpq	$34, %rcx
	movq	%r14, %rdx
	jne	.LBB23_9
# BB#5:                                 # %land.lhs.true10.i
	movl	8(%rdx), %eax
	cmpq	$53, %rax
	movq	$1, %rbx
	jb	.LBB23_27
# BB#6:                                 # %lor.lhs.false18.i
	movq	$49, %rsi
	movq	%rdi, %r15
	movq	%rdx, %r14
	callq	find_reg_fusage
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB23_27
# BB#7:                                 # %lor.lhs.false18.if.end22_crit_edge.i
	movq	%r15, %rdi
	movl	(%rdi), %eax
.LBB23_8:                               # %if.end22.i
	movq	%r14, %rdx
.LBB23_9:                               # %if.end22.i
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, 16(%rsp)
	movq	%rdx, 24(%rsp)
	cmpq	$105, %rax
	jne	.LBB23_11
# BB#10:                                # %cond.true.i.i
	movq	32(%rdi), %rdi
.LBB23_11:                              # %set_of.exit.i
	movabsq	$set_of_1, %rsi
	leaq	16(%rsp), %rdx
	callq	note_stores
	cmpq	$0, 16(%rsp)
	je	.LBB23_12
# BB#13:                                # %set_of.exit.i
	movabsq	$1, %rbx
	jmp	.LBB23_27
.LBB23_14:                              # %sw.bb2
	testq	$67108864, %rcx         # imm = 0x4000000
	movq	$1, %rbx
	je	.LBB23_27
.LBB23_15:                              # %sw.epilog
	movzbl	rtx_length(%rax), %r13d
	testq	%r13, %r13
	je	.LBB23_16
# BB#17:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB23_18:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_25 Depth 2
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$101, %rax
	jne	.LBB23_21
# BB#19:                                # %land.lhs.true
                                        #   in Loop: Header=BB23_18 Depth=1
	movq	(%r14,%r13,8), %rdi
	movq	%r15, %rsi
	callq	modified_in_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rbx
	jne	.LBB23_27
# BB#20:                                # %if.elsethread-pre-split
                                        #   in Loop: Header=BB23_18 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	(%rax,%rcx), %eax
.LBB23_21:                              # %if.else
                                        #   in Loop: Header=BB23_18 Depth=1
	andq	$255, %rax
	cmpq	$69, %rax
	jne	.LBB23_22
# BB#24:                                # %if.then24
                                        #   in Loop: Header=BB23_18 Depth=1
	movq	(%r14,%r13,8), %rax
	movslq	(%rax), %r12
	.align	16, 0x90
.LBB23_25:                              # %for.cond29
                                        #   Parent Loop BB23_18 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB23_22
# BB#26:                                # %for.body32
                                        #   in Loop: Header=BB23_25 Depth=2
	movq	(%r14,%r13,8), %rax
	movq	(%rax,%r12,8), %rdi
	decq	%r12
	movq	%r15, %rsi
	callq	modified_in_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rbx
	je	.LBB23_25
	jmp	.LBB23_27
	.align	16, 0x90
.LBB23_22:                              # %for.cond.backedge
                                        #   in Loop: Header=BB23_18 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %r13
	jg	.LBB23_18
# BB#23:
	xorq	%rbx, %rbx
	jmp	.LBB23_27
.LBB23_16:
	xorq	%rbx, %rbx
.LBB23_27:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_12:
	movabsq	$0, %rbx
	jmp	.LBB23_27
.Ltmp330:
	.size	modified_in_p, .Ltmp330-modified_in_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI23_0:
	.quad	.LBB23_27
	.quad	.LBB23_27
	.quad	.LBB23_27
	.quad	.LBB23_15
	.quad	.LBB23_27
	.quad	.LBB23_2
	.quad	.LBB23_15
	.quad	.LBB23_3
	.quad	.LBB23_15
	.quad	.LBB23_15
	.quad	.LBB23_15
	.quad	.LBB23_15
	.quad	.LBB23_14
	.quad	.LBB23_27
	.quad	.LBB23_27
	.quad	.LBB23_2

	.text
	.globl	insn_dependent_p
	.align	16, 0x90
	.type	insn_dependent_p,@function
insn_dependent_p:                       # @insn_dependent_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp334:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp335:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp336:
	.cfi_def_cfa_offset 32
.Ltmp337:
	.cfi_offset %rbx, -24
.Ltmp338:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	movq	%rsi, %rbx
	cmpq	$105, %rax
	jne	.LBB24_7
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB24_7
# BB#2:                                 # %if.end
	movq	32(%rbx), %rax
	movq	%rax, (%rsp)
	movq	32(%r14), %rdi
	movabsq	$insn_dependent_p_1, %rsi
	leaq	(%rsp), %rdx
	callq	note_stores
	cmpq	$0, (%rsp)
	movq	$1, %rax
	je	.LBB24_6
# BB#3:                                 # %if.end16
	movq	32(%r14), %rax
	movq	%rax, (%rsp)
	movq	32(%rbx), %rdi
	movabsq	$insn_dependent_p_1, %rsi
	leaq	(%rsp), %rdx
	callq	note_stores
	cmpq	$0, (%rsp)
	jne	.LBB24_4
# BB#5:                                 # %if.end16
	movabsq	$1, %rax
	jmp	.LBB24_6
.LBB24_4:
	movabsq	$0, %rax
.LBB24_6:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB24_7:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.insn_dependent_p, %rcx
	movq	$988, %rsi              # imm = 0x3DC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp339:
	.size	insn_dependent_p, .Ltmp339-insn_dependent_p
	.cfi_endproc

	.globl	note_stores
	.align	16, 0x90
	.type	note_stores,@function
note_stores:                            # @note_stores
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
.Ltmp351:
	.cfi_offset %rbx, -48
.Ltmp352:
	.cfi_offset %r12, -40
.Ltmp353:
	.cfi_offset %r13, -32
.Ltmp354:
	.cfi_offset %r14, -24
.Ltmp355:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	(%r12), %eax
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$38, %rcx
	jne	.LBB25_2
# BB#1:                                 # %if.then
	movq	16(%r12), %r12
	movl	(%r12), %eax
.LBB25_2:                               # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	je	.LBB25_19
# BB#3:                                 # %if.end
	cmpq	$47, %rax
	movq	%r12, %rbx
	je	.LBB25_5
# BB#4:                                 # %if.end
	cmpq	$49, %rax
	movq	%r12, %rbx
	jne	.LBB25_22
	.align	16, 0x90
.LBB25_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$131, %rax
	jg	.LBB25_17
# BB#6:                                 # %while.cond
                                        #   in Loop: Header=BB25_5 Depth=1
	cmpq	$63, %rax
	jne	.LBB25_7
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB25_5 Depth=1
	movq	8(%rbx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB25_5
# BB#14:                                # %lor.lhs.false20
                                        #   in Loop: Header=BB25_5 Depth=1
	movl	8(%rcx), %ecx
	cmpq	$52, %rcx
	ja	.LBB25_5
# BB#15:                                # %lor.lhs.false27
                                        #   in Loop: Header=BB25_5 Depth=1
	leaq	-132(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB25_5
.LBB25_7:                               # %while.cond
                                        #   in Loop: Header=BB25_5 Depth=1
	cmpq	$64, %rax
	je	.LBB25_5
	jmp	.LBB25_8
	.align	16, 0x90
.LBB25_17:                              # %while.cond
                                        #   in Loop: Header=BB25_5 Depth=1
	addq	$-132, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB25_5
	jmp	.LBB25_18
.LBB25_19:                              # %if.then72
	movq	8(%r12), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jg	.LBB25_20
	jmp	.LBB25_22
	.align	16, 0x90
.LBB25_21:                              # %for.body80.for.body80_crit_edge
                                        #   in Loop: Header=BB25_20 Depth=1
	movq	8(%r12), %rax
.LBB25_20:                              # %for.body80.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	note_stores
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB25_21
	jmp	.LBB25_22
.LBB25_8:                               # %lor.lhs.false27
	cmpq	$39, %rax
	jne	.LBB25_18
# BB#9:                                 # %if.then44
	movq	8(%rbx), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jg	.LBB25_10
	jmp	.LBB25_22
	.align	16, 0x90
.LBB25_12:                              # %for.cond.backedge.for.body_crit_edge
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	8(%rbx), %rax
.LBB25_10:                              # %for.body.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%r13
	movq	8(%rax,%r13,8), %rax
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB25_11
# BB#16:                                # %if.then56
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	*%r14
.LBB25_11:                              # %for.cond.backedge
                                        #   in Loop: Header=BB25_10 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB25_12
.LBB25_22:                              # %if.end91
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_18:                              # %if.else
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*%r14  # TAILCALL
.Ltmp356:
	.size	note_stores, .Ltmp356-note_stores
	.cfi_endproc

	.align	16, 0x90
	.type	insn_dependent_p_1,@function
insn_dependent_p_1:                     # @insn_dependent_p_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp359:
	.cfi_def_cfa_offset 16
.Ltmp360:
	.cfi_offset %rbx, -16
	movq	%rdx, %rbx
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB26_3
# BB#1:                                 # %land.lhs.true
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB26_3
# BB#2:                                 # %if.then
	movq	$0, (%rbx)
.LBB26_3:                               # %if.end
	popq	%rbx
	retq
.Ltmp361:
	.size	insn_dependent_p_1, .Ltmp361-insn_dependent_p_1
	.cfi_endproc

	.align	16, 0x90
	.type	set_of_1,@function
set_of_1:                               # @set_of_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp367:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp368:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp369:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp370:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp371:
	.cfi_def_cfa_offset 48
.Ltmp372:
	.cfi_offset %rbx, -40
.Ltmp373:
	.cfi_offset %r12, -32
.Ltmp374:
	.cfi_offset %r14, -24
.Ltmp375:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	8(%r15), %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB27_3
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	je	.LBB27_4
# BB#2:                                 # %land.lhs.true
	movq	8(%r15), %rdi
	movq	%rbx, %rsi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	je	.LBB27_4
.LBB27_3:                               # %if.then
	movq	%r14, (%r15)
.LBB27_4:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp376:
	.size	set_of_1, .Ltmp376-set_of_1
	.cfi_endproc

	.globl	find_reg_note
	.align	16, 0x90
	.type	find_reg_note,@function
find_reg_note:                          # @find_reg_note
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB28_12
# BB#1:                                 # %if.end
	movq	56(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB28_12
# BB#2:                                 # %for.body.lr.ph
	testq	%rdx, %rdx
	je	.LBB28_3
# BB#7:
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	.align	16, 0x90
.LBB28_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2(%rcx), %edi
	cmpq	%rsi, %rdi
	jne	.LBB28_11
# BB#9:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB28_8 Depth=1
	movq	8(%rcx), %rdi
	cmpq	%rdx, %rdi
	je	.LBB28_10
.LBB28_11:                              # %for.inc
                                        #   in Loop: Header=BB28_8 Depth=1
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB28_8
	jmp	.LBB28_12
.LBB28_3:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	.align	16, 0x90
.LBB28_4:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2(%rcx), %edx
	cmpq	%rsi, %rdx
	je	.LBB28_5
# BB#6:                                 # %for.inc.us
                                        #   in Loop: Header=BB28_4 Depth=1
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB28_4
.LBB28_12:                              # %return
	retq
.LBB28_10:
	movq	%rcx, %rax
	retq
.LBB28_5:
	movq	%rcx, %rax
	retq
.Ltmp377:
	.size	find_reg_note, .Ltmp377-find_reg_note
	.cfi_endproc

	.globl	side_effects_p
	.align	16, 0x90
	.type	side_effects_p,@function
side_effects_p:                         # @side_effects_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp384:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp385:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp386:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp387:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp388:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp389:
	.cfi_def_cfa_offset 64
.Ltmp390:
	.cfi_offset %rbx, -48
.Ltmp391:
	.cfi_offset %r12, -40
.Ltmp392:
	.cfi_offset %r13, -32
.Ltmp393:
	.cfi_offset %r14, -24
.Ltmp394:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movl	(%rdi), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$95, %rax
	jg	.LBB29_6
# BB#1:                                 # %entry
	leaq	-40(%rax), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$29, %rdx
	ja	.LBB29_9
# BB#2:                                 # %entry
	xorq	%r15, %r15
	jmpq	*.LJTI29_0(,%rdx,8)
.LBB29_8:                               # %sw.bb5
	testq	$134217728, %rcx        # imm = 0x8000000
	movq	$1, %r15
	jne	.LBB29_20
	jmp	.LBB29_9
.LBB29_6:                               # %entry
	leaq	-96(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$6, %rdx
	jae	.LBB29_9
.LBB29_7:                               # %sw.bb4
	movq	$1, %r15
	jmp	.LBB29_20
.LBB29_9:                               # %sw.epilog
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB29_10
# BB#11:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB29_12:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB29_19 Depth 2
	movq	%rbx, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB29_13
# BB#16:                                # %for.cond31.preheader
                                        #   in Loop: Header=BB29_12 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %r12
	movslq	(%r12), %r14
	testq	%r14, %r14
	movq	$1, %r15
	movq	$1, %r13
	jle	.LBB29_14
	.align	16, 0x90
.LBB29_19:                              # %for.body37
                                        #   Parent Loop BB29_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r12,%r13,8), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB29_20
# BB#18:                                # %for.cond31
                                        #   in Loop: Header=BB29_19 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r13
	cmpq	%r14, %rax
	jl	.LBB29_19
	jmp	.LBB29_14
	.align	16, 0x90
.LBB29_13:                              # %for.body
                                        #   in Loop: Header=BB29_12 Depth=1
	cmpq	$101, %rcx
	jne	.LBB29_14
# BB#17:                                # %if.then19
                                        #   in Loop: Header=BB29_12 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	callq	side_effects_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r15
	jne	.LBB29_20
.LBB29_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB29_12 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB29_12
# BB#15:
	xorq	%r15, %r15
	jmp	.LBB29_20
.LBB29_10:
	xorq	%r15, %r15
.LBB29_20:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB29_3:                               # %sw.bb1
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB29_4
# BB#5:                                 # %sw.bb1
	movabsq	$1, %r15
	jmp	.LBB29_20
.LBB29_4:
	movabsq	$0, %r15
	jmp	.LBB29_20
.Ltmp395:
	.size	side_effects_p, .Ltmp395-side_effects_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_20
	.quad	.LBB29_8
	.quad	.LBB29_9
	.quad	.LBB29_7
	.quad	.LBB29_20
	.quad	.LBB29_20
	.quad	.LBB29_9
	.quad	.LBB29_9
	.quad	.LBB29_9
	.quad	.LBB29_3
	.quad	.LBB29_7
	.quad	.LBB29_9
	.quad	.LBB29_9
	.quad	.LBB29_9
	.quad	.LBB29_20
	.quad	.LBB29_20
	.quad	.LBB29_20
	.quad	.LBB29_9
	.quad	.LBB29_20
	.quad	.LBB29_20
	.quad	.LBB29_9
	.quad	.LBB29_20
	.quad	.LBB29_20
	.quad	.LBB29_9
	.quad	.LBB29_9
	.quad	.LBB29_9
	.quad	.LBB29_8
	.quad	.LBB29_20
	.quad	.LBB29_20
	.quad	.LBB29_20

	.text
	.globl	multiple_sets
	.align	16, 0x90
	.type	multiple_sets,@function
multiple_sets:                          # @multiple_sets
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$105, %rcx
	jne	.LBB30_8
# BB#1:                                 # %if.end
	movq	32(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$39, %rdx
	jne	.LBB30_8
# BB#2:                                 # %for.cond.preheader
	movq	8(%rcx), %r9
	movslq	(%r9), %rsi
	testq	%rsi, %rsi
	jle	.LBB30_7
# BB#3:
	movq	$1, %rdi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB30_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r9,%rdi,8), %rax
	movzbl	(%rax), %eax
	cmpq	$47, %rax
	jne	.LBB30_6
# BB#5:                                 # %if.then27
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	$1, %rax
	testq	%r8, %rdx
	movq	$1, %rdx
	jne	.LBB30_8
.LBB30_6:                               # %for.inc
                                        #   in Loop: Header=BB30_4 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdi
	cmpq	%rsi, %rax
	jl	.LBB30_4
.LBB30_7:
	xorq	%rax, %rax
.LBB30_8:                               # %return
	retq
.Ltmp396:
	.size	multiple_sets, .Ltmp396-multiple_sets
	.cfi_endproc

	.globl	set_noop_p
	.align	16, 0x90
	.type	set_noop_p,@function
set_noop_p:                             # @set_noop_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp402:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp403:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp404:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp405:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp406:
	.cfi_def_cfa_offset 48
.Ltmp407:
	.cfi_offset %rbx, -40
.Ltmp408:
	.cfi_offset %r12, -32
.Ltmp409:
	.cfi_offset %r14, -24
.Ltmp410:
	.cfi_offset %r15, -16
	movq	8(%rdi), %r14
	movq	16(%rdi), %r15
	movq	%r15, %rdi
	callq	side_effects_p
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB31_21
# BB#1:                                 # %lor.lhs.false
	movq	%r14, %rdi
	callq	side_effects_p
	testq	%r12, %rax
	jne	.LBB31_21
# BB#2:                                 # %if.end
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB31_4
# BB#3:                                 # %land.lhs.true
	movzbl	(%r15), %ecx
	cmpq	$66, %rcx
	jne	.LBB31_4
# BB#22:                                # %if.then9
	movq	%r14, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	rtx_equal_p  # TAILCALL
.LBB31_4:                               # %if.end11
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %r14
	jne	.LBB31_6
# BB#5:                                 # %if.end11
	movq	$1, %rbx
	cmpq	%rcx, %r15
	je	.LBB31_21
.LBB31_6:                               # %if.end16
	cmpq	$64, %rax
	je	.LBB31_11
# BB#7:                                 # %if.end16
	addq	$-132, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB31_12
# BB#8:                                 # %if.then24
	movq	8(%r14), %rdi
	movq	%r15, %rsi
	callq	rtx_equal_p
	xorq	%rbx, %rbx
	testq	%r12, %rax
	je	.LBB31_21
# BB#9:                                 # %land.rhs
	movq	24(%r14), %rax
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB31_20
# BB#10:
	movabsq	$0, %rbx
	jmp	.LBB31_21
.LBB31_11:                              # %if.then38
	movq	8(%r14), %r14
.LBB31_12:                              # %if.end42
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB31_16
# BB#13:                                # %land.lhs.true46
	movzbl	(%r14), %ecx
	cmpq	$63, %rcx
	jne	.LBB31_16
# BB#14:                                # %if.then50
	movl	16(%r15), %eax
	movl	16(%r14), %ecx
	cmpq	%rcx, %rax
	movabsq	$0, %rbx
	jne	.LBB31_21
# BB#15:                                # %if.end58
	movq	8(%r15), %r15
	movq	8(%r14), %r14
	movl	(%r15), %eax
.LBB31_16:                              # %if.end65
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rbx, %rbx
	cmpq	$61, %rax
	jne	.LBB31_21
# BB#17:                                # %land.lhs.true69
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB31_21
# BB#18:                                # %land.rhs73
	movl	8(%r15), %eax
	movl	8(%r14), %ecx
	cmpq	%rcx, %rax
	jne	.LBB31_19
.LBB31_20:                              # %land.rhs73
	movabsq	$1, %rbx
	jmp	.LBB31_21
.LBB31_19:
	movabsq	$0, %rbx
.LBB31_21:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp411:
	.size	set_noop_p, .Ltmp411-set_noop_p
	.cfi_endproc

	.globl	noop_move_p
	.align	16, 0x90
	.type	noop_move_p,@function
noop_move_p:                            # @noop_move_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp417:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp418:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp419:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp420:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp421:
	.cfi_def_cfa_offset 48
.Ltmp422:
	.cfi_offset %rbx, -40
.Ltmp423:
	.cfi_offset %r12, -32
.Ltmp424:
	.cfi_offset %r14, -24
.Ltmp425:
	.cfi_offset %r15, -16
	movl	40(%rdi), %eax
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	movq	$1, %rbx
	je	.LBB32_23
# BB#1:                                 # %if.end
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	movq	32(%rdi), %r14
	cmpq	$105, %rax
	jne	.LBB32_11
# BB#2:                                 # %if.end.i
	movq	56(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB32_7
# BB#3:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB32_4:                               # %for.body.us.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rcx), %edx
	andq	$16711680, %rdx         # imm = 0xFF0000
	cmpq	$262144, %rdx           # imm = 0x40000
	je	.LBB32_23
# BB#5:                                 # %for.inc.us.i
                                        #   in Loop: Header=BB32_4 Depth=1
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB32_4
# BB#6:                                 # %if.end4
	cmpq	$105, %rax
	jne	.LBB32_11
.LBB32_7:                               # %if.end.i63
	movq	56(%rdi), %rax
	testq	%rax, %rax
	je	.LBB32_11
# BB#8:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB32_9:                               # %for.body.us.i70
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$393216, %rcx           # imm = 0x60000
	je	.LBB32_23
# BB#10:                                # %for.inc.us.i75
                                        #   in Loop: Header=BB32_9 Depth=1
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB32_9
.LBB32_11:                              # %if.end8
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB32_14
# BB#12:                                # %land.lhs.true
	movq	%r14, %rdi
	callq	set_noop_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rbx
	jne	.LBB32_23
# BB#13:                                # %land.lhs.true.if.end13_crit_edge
	movl	(%r14), %eax
.LBB32_14:                              # %if.end13
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rbx, %rbx
	cmpq	$39, %rax
	jne	.LBB32_23
# BB#15:                                # %for.cond.preheader
	movq	8(%r14), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	jle	.LBB32_22
# BB#16:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB32_17:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r15,8), %rdi
	movzbl	(%rdi), %ecx
	leaq	-48(%rcx), %rdx
	andq	%r12, %rdx
	cmpq	$2, %rdx
	jb	.LBB32_21
# BB#18:                                # %for.body
                                        #   in Loop: Header=BB32_17 Depth=1
	cmpq	$47, %rcx
	jne	.LBB32_23
# BB#19:                                # %lor.lhs.false36
                                        #   in Loop: Header=BB32_17 Depth=1
	callq	set_noop_p
	testq	%r12, %rax
	je	.LBB32_23
# BB#20:                                # %lor.lhs.false36.for.inc_crit_edge
                                        #   in Loop: Header=BB32_17 Depth=1
	movq	8(%r14), %rax
.LBB32_21:                              # %for.inc
                                        #   in Loop: Header=BB32_17 Depth=1
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB32_17
.LBB32_22:
	movq	$1, %rbx
.LBB32_23:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp426:
	.size	noop_move_p, .Ltmp426-noop_move_p
	.cfi_endproc

	.globl	refers_to_regno_p
	.align	16, 0x90
	.type	refers_to_regno_p,@function
refers_to_regno_p:                      # @refers_to_regno_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp433:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp434:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp435:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp436:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp437:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp438:
	.cfi_def_cfa_offset 96
.Ltmp439:
	.cfi_offset %rbx, -48
.Ltmp440:
	.cfi_offset %r12, -40
.Ltmp441:
	.cfi_offset %r13, -32
.Ltmp442:
	.cfi_offset %r14, -24
.Ltmp443:
	.cfi_offset %r15, -16
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB33_1
# BB#3:
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	jmp	.LBB33_4
	.align	16, 0x90
.LBB33_68:                              # %for.body.lr.ph
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	rtx_format(,%r13,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB33_69:                              # %for.body
                                        #   Parent Loop BB33_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB33_78 Depth 3
	movq	%r15, %r14
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%r14), %eax
	leaq	-1(%r14), %r15
	cmpq	$69, %rax
	jne	.LBB33_70
# BB#76:                                # %if.then317
                                        #   in Loop: Header=BB33_69 Depth=2
	movq	(%rbx,%r14,8), %rax
	movslq	(%rax), %rdx
	testq	%rdx, %rdx
	jle	.LBB33_75
# BB#77:                                # %for.body325.lr.ph
                                        #   in Loop: Header=BB33_69 Depth=2
	movq	$3, %rcx
	leaq	-1(%rdx), %r13
	shlq	%cl, %rdx
	movq	40(%rsp), %r12          # 8-byte Reload
	subq	%rdx, %r12
	jmp	.LBB33_78
	.align	16, 0x90
.LBB33_81:                              # %for.cond322.backedge.for.body325_crit_edge
                                        #   in Loop: Header=BB33_78 Depth=3
	movq	(%rbx,%r14,8), %rax
	decq	%r13
	addq	$8, %r12
.LBB33_78:                              # %for.body325
                                        #   Parent Loop BB33_4 Depth=1
                                        #     Parent Loop BB33_69 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r12, %rax
	je	.LBB33_80
# BB#79:                                # %land.lhs.true334
                                        #   in Loop: Header=BB33_78 Depth=3
	movq	8(%rax,%r13,8), %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB33_2
.LBB33_80:                              # %for.cond322.backedge
                                        #   in Loop: Header=BB33_78 Depth=3
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB33_81
	jmp	.LBB33_75
	.align	16, 0x90
.LBB33_70:                              # %for.body
                                        #   in Loop: Header=BB33_69 Depth=2
	cmpq	$101, %rax
	jne	.LBB33_75
# BB#71:                                # %land.lhs.true288
                                        #   in Loop: Header=BB33_69 Depth=2
	leaq	8(%rbx,%r15,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB33_75
# BB#72:                                # %if.then295
                                        #   in Loop: Header=BB33_69 Depth=2
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %r15
	je	.LBB33_73
# BB#74:                                # %if.else
                                        #   in Loop: Header=BB33_69 Depth=2
	movq	(%rax), %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_p
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB33_2
	.align	16, 0x90
.LBB33_75:                              # %for.cond.backedge
                                        #   in Loop: Header=BB33_69 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB33_69
	jmp	.LBB33_1
.LBB33_73:                              # %if.then298
                                        #   in Loop: Header=BB33_4 Depth=1
	addq	$8, %rbx
	jmp	.LBB33_18
	.align	16, 0x90
.LBB33_4:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_69 Depth 2
                                        #       Child Loop BB33_78 Depth 3
	movl	(%rbx), %r8d
	movq	%r8, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$60, %r13
	jg	.LBB33_19
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB33_4 Depth=1
	cmpq	$47, %r13
	je	.LBB33_7
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB33_4 Depth=1
	cmpq	$49, %r13
	jne	.LBB33_67
.LBB33_7:                               # %sw.bb191
                                        #   in Loop: Header=BB33_4 Depth=1
	leaq	8(%rbx), %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB33_16
# BB#8:                                 # %land.lhs.true197
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	(%r14), %rdx
	movl	(%rdx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB33_14
# BB#9:                                 # %land.lhs.true205
                                        #   in Loop: Header=BB33_4 Depth=1
	leaq	8(%rdx), %rax
	movq	40(%rsp), %rsi          # 8-byte Reload
	cmpq	%rsi, %rax
	je	.LBB33_14
# BB#10:                                # %land.lhs.true214
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	(%rax), %rax
	movzbl	(%rax), %esi
	cmpq	$61, %rsi
	jne	.LBB33_14
# BB#11:                                # %land.lhs.true225
                                        #   in Loop: Header=BB33_4 Depth=1
	movl	8(%rax), %esi
	cmpq	$53, %rsi
	jb	.LBB33_14
# BB#12:                                # %land.lhs.true237
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB33_2
# BB#13:                                # %land.lhs.true237.lor.lhs.false246_crit_edge
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	(%r14), %rdx
	movl	(%rdx), %ecx
.LBB33_14:                              # %lor.lhs.false246
                                        #   in Loop: Header=BB33_4 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB33_16
# BB#15:                                # %land.lhs.true254
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB33_2
.LBB33_16:                              # %if.end261
                                        #   in Loop: Header=BB33_4 Depth=1
	cmpq	$49, %r13
	je	.LBB33_1
# BB#17:                                # %lor.lhs.false264
                                        #   in Loop: Header=BB33_4 Depth=1
	addq	$16, %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	je	.LBB33_1
.LBB33_18:                              # %repeat.backedge
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB33_4
	jmp	.LBB33_1
	.align	16, 0x90
.LBB33_19:                              # %if.end
                                        #   in Loop: Header=BB33_4 Depth=1
	cmpq	$63, %r13
	jne	.LBB33_20
# BB#47:                                # %sw.bb76
                                        #   in Loop: Header=BB33_4 Depth=1
	movq	8(%rbx), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB33_67
# BB#48:                                # %land.lhs.true83
                                        #   in Loop: Header=BB33_4 Depth=1
	movl	8(%rax), %r14d
	cmpq	$52, %r14
	ja	.LBB33_67
	jmp	.LBB33_49
.LBB33_20:                              # %if.end
                                        #   in Loop: Header=BB33_4 Depth=1
	cmpq	$61, %r13
	je	.LBB33_21
	.align	16, 0x90
.LBB33_67:                              # %sw.epilog
                                        #   in Loop: Header=BB33_4 Depth=1
	movzbl	rtx_length(%r13), %r15d
	testq	%r15, %r15
	jne	.LBB33_68
.LBB33_1:
	xorq	%rax, %rax
.LBB33_2:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB33_49:                              # %if.then92
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%rbx), %edx
	shrq	%cl, %r8
	andq	$255, %rsi
	andq	$255, %r8
	movq	%r14, %rdi
	movq	%r8, %rcx
	callq	subreg_regno_offset
	movq	%rax, %r9
	addq	%r14, %r9
	movq	%r9, %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdi
	cmpq	$52, %rdi
	movq	$1, %rdx
	movq	24(%rsp), %r8           # 8-byte Reload
	ja	.LBB33_64
# BB#50:                                # %cond.true95
	movzbl	2(%rbx), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r8, %rcx
	cmpq	$8, %rcx
	je	.LBB33_54
# BB#51:                                # %cond.true95
	leaq	-21(%r8), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB33_54
# BB#52:                                # %cond.true95
	leaq	-45(%r8), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB33_54
# BB#53:                                # %cond.true95
	leaq	-29(%r8), %rcx
	andq	%rsi, %rcx
	cmpq	$7, %rcx
	jbe	.LBB33_54
# BB#59:                                # %cond.false137
	cmpq	$24, %rax
	jne	.LBB33_60
# BB#62:                                # %cond.true153
	movzbl	target_flags+3(%rip), %edx
	andq	$2, %rdx
	xorq	$6, %rdx
	jmp	.LBB33_64
.LBB33_21:                              # %sw.bb
	movl	8(%rbx), %esi
	cmpq	$7, %rsi
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB33_23
# BB#22:                                # %sw.bb
	movq	%rdx, %rcx
.LBB33_23:                              # %sw.bb
	cmpq	$16, %rsi
	movq	%rax, %rdi
	jne	.LBB33_25
# BB#24:                                # %sw.bb
	movq	%rdx, %rdi
.LBB33_25:                              # %sw.bb
	orq	%rdi, %rcx
	cmpq	$20, %rsi
	jne	.LBB33_27
# BB#26:                                # %sw.bb
	movq	%rdx, %rax
.LBB33_27:                              # %sw.bb
	orq	%rax, %rcx
	je	.LBB33_30
# BB#28:                                # %sw.bb
	movq	24(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$53, %rcx
	jb	.LBB33_30
# BB#29:                                # %sw.bb
	movq	$1, %rax
	cmpq	$58, %rcx
	jb	.LBB33_2
.LBB33_30:                              # %if.end9
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	%rbx, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rsi
	jae	.LBB33_2
# BB#31:                                # %land.rhs
	movq	$1, %rax
	cmpq	$52, %rsi
	ja	.LBB33_46
# BB#32:                                # %cond.true
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rsi, %rax
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	cmpq	$8, %rax
	je	.LBB33_36
# BB#33:                                # %cond.true
	leaq	-21(%rsi), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB33_36
# BB#34:                                # %cond.true
	leaq	-45(%rsi), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB33_36
# BB#35:                                # %cond.true
	leaq	-29(%rsi), %rax
	andq	%rbx, %rax
	cmpq	$7, %rax
	jbe	.LBB33_36
# BB#41:                                # %cond.false
	cmpq	$24, %r8
	jne	.LBB33_42
# BB#44:                                # %cond.true49
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB33_46
.LBB33_54:                              # %cond.true119
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rdx
	je	.LBB33_64
# BB#55:                                # %lor.rhs127
	cmpq	$6, %rax
	jne	.LBB33_56
# BB#57:                                # %lor.rhs127
	movabsq	$1, %rdx
	incq	%rdx
	jmp	.LBB33_64
.LBB33_60:                              # %cond.false137
	cmpq	$18, %rax
	jne	.LBB33_63
# BB#61:                                # %cond.true143
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	xorq	$3, %rdx
	jmp	.LBB33_64
.LBB33_56:
	movabsq	$0, %rdx
	incq	%rdx
	jmp	.LBB33_64
.LBB33_63:                              # %cond.false157
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %rdx
.LBB33_64:                              # %cond.end180
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	%rsi, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rdi
	jae	.LBB33_2
# BB#65:                                # %land.rhs185
	addq	%r9, %rdx
	andq	%rsi, %rdx
	andq	%rsi, %r8
	cmpq	%rdx, %r8
	jmp	.LBB33_66
.LBB33_36:                              # %cond.true27
	movl	mode_class(,%r8,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB33_46
# BB#37:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB33_38
# BB#39:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB33_46
.LBB33_42:                              # %cond.false
	cmpq	$18, %r8
	jne	.LBB33_45
# BB#43:                                # %cond.true42
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB33_46
.LBB33_45:                              # %cond.false53
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	jmp	.LBB33_46
.LBB33_38:
	movabsq	$0, %rax
	incq	%rax
.LBB33_46:                              # %cond.end71
	addq	%rsi, %rax
	andq	%rbx, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
.LBB33_66:                              # %return
	sbbq	%rax, %rax
	andq	$1, %rax
	jmp	.LBB33_2
.Ltmp444:
	.size	refers_to_regno_p, .Ltmp444-refers_to_regno_p
	.cfi_endproc

	.globl	subreg_regno
	.align	16, 0x90
	.type	subreg_regno,@function
subreg_regno:                           # @subreg_regno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp447:
	.cfi_def_cfa_offset 16
.Ltmp448:
	.cfi_offset %rbx, -16
	movq	8(%rdi), %rax
	movl	8(%rax), %ebx
	movzbl	2(%rdi), %ecx
	movl	16(%rdi), %edx
	movzbl	2(%rax), %esi
	movq	%rbx, %rdi
	callq	subreg_regno_offset
	addq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp449:
	.size	subreg_regno, .Ltmp449-subreg_regno
	.cfi_endproc

	.globl	reg_set_last
	.align	16, 0x90
	.type	reg_set_last,@function
reg_set_last:                           # @reg_set_last
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp456:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp457:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp458:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp459:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp460:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp461:
	.cfi_def_cfa_offset 80
.Ltmp462:
	.cfi_offset %rbx, -48
.Ltmp463:
	.cfi_offset %r12, -40
.Ltmp464:
	.cfi_offset %r13, -32
.Ltmp465:
	.cfi_offset %r14, -24
.Ltmp466:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%r12, %r12
	je	.LBB35_33
# BB#1:                                 # %land.lhs.true.lr.ph
	movabsq	$set_of_1, %r15
	leaq	16(%rsp), %r13
	movq	%r12, %r14
	.align	16, 0x90
.LBB35_2:                               # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	je	.LBB35_33
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB35_2 Depth=1
	cmpq	$34, %rax
	jne	.LBB35_5
# BB#4:                                 # %land.rhs4
                                        #   in Loop: Header=BB35_2 Depth=1
	movl	8(%rbx), %ecx
	cmpq	$54, %rcx
	jb	.LBB35_33
.LBB35_5:                               # %for.body
                                        #   in Loop: Header=BB35_2 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB35_32
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	$0, 16(%rsp)
	movq	%rbx, 24(%rsp)
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movq	%r14, %rdi
	jne	.LBB35_8
# BB#7:                                 # %cond.true.i
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	32(%r14), %rdi
.LBB35_8:                               # %set_of.exit
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	note_stores
	movq	16(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB35_9
.LBB35_32:                              # %for.inc
                                        #   in Loop: Header=BB35_2 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB35_2
	jmp	.LBB35_33
.LBB35_9:                               # %if.then13
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB35_33
# BB#10:                                # %lor.lhs.false
	movq	8(%rax), %rax
	cmpq	%rbx, %rax
	jne	.LBB35_33
# BB#11:                                # %if.end
	movq	16(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB35_20
# BB#12:                                # %if.end
	addq	$-54, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	ja	.LBB35_31
# BB#13:                                # %if.end
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB35_33
# BB#14:                                # %if.end
	shlq	%cl, %rax
	testq	$640, %rax              # imm = 0x280
	je	.LBB35_31
# BB#15:                                # %land.lhs.true75
	cmpq	%r12, %r14
	je	.LBB35_33
# BB#16:                                # %for.cond.preheader.i
	movq	24(%r14), %r15
	cmpq	%r12, %r15
	je	.LBB35_33
# BB#17:                                # %for.body.lr.ph.i
	movabsq	$set_of_1, %r14
	leaq	16(%rsp), %r13
.LBB35_18:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movzbl	rtx_class(%rcx), %edx
	cmpq	$105, %rdx
	jne	.LBB35_19
# BB#22:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB35_18 Depth=1
	cmpq	$34, %rcx
	jne	.LBB35_28
# BB#23:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$66, %rax
	je	.LBB35_31
# BB#24:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	cmpq	$61, %rax
	jne	.LBB35_26
# BB#25:                                # %land.lhs.true10.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB35_31
.LBB35_26:                              # %lor.lhs.false18.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movq	$49, %rsi
	movq	%r15, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	find_reg_fusage
	testq	%rbx, %rax
	jne	.LBB35_31
# BB#27:                                # %lor.lhs.false18.if.end22_crit_edge.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movl	(%r15), %eax
.LBB35_28:                              # %if.end22.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	movq	$0, 16(%rsp)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 24(%rsp)
	cmpq	$105, %rax
	movq	%r15, %rdi
	jne	.LBB35_30
# BB#29:                                # %cond.true.i.i.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movq	32(%r15), %rdi
.LBB35_30:                              # %reg_set_p.exit.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	note_stores
	cmpq	$0, 16(%rsp)
	jne	.LBB35_31
.LBB35_19:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB35_18 Depth=1
	movq	24(%r15), %r15
	cmpq	%r12, %r15
	jne	.LBB35_18
	jmp	.LBB35_33
.LBB35_20:                              # %if.end
	cmpq	$134, %rcx
	je	.LBB35_33
# BB#21:                                # %if.end
	cmpq	$140, %rcx
	je	.LBB35_33
.LBB35_31:                              # %if.else
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
.LBB35_33:                              # %return
	movq	8(%rsp), %rax           # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp467:
	.size	reg_set_last, .Ltmp467-reg_set_last
	.cfi_endproc

	.globl	note_uses
	.align	16, 0x90
	.type	note_uses,@function
note_uses:                              # @note_uses
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp474:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp475:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp476:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp477:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp478:
	.cfi_def_cfa_offset 48
.Ltmp479:
	.cfi_offset %rbx, -48
.Ltmp480:
	.cfi_offset %r12, -40
.Ltmp481:
	.cfi_offset %r13, -32
.Ltmp482:
	.cfi_offset %r14, -24
.Ltmp483:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB36_1
	.align	16, 0x90
.LBB36_3:                               # %sw.bb
                                        #   in Loop: Header=BB36_1 Depth=1
	leaq	8(%rbx), %rdi
	movq	%r15, %rsi
	callq	*%r14
	addq	$16, %rbx
	movq	%rbx, %rdi
.LBB36_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rbx
	movzbl	(%rbx), %eax
	addq	$-38, %rax
	andq	%r13, %rax
	cmpq	$14, %rax
	ja	.LBB36_25
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB36_1 Depth=1
	jmpq	*.LJTI36_0(,%rax,8)
.LBB36_12:                              # %sw.bb11
	addq	$8, %rbx
	movq	%rbx, %rdi
.LBB36_25:                              # %sw.default
	movq	%r15, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*%r14  # TAILCALL
.LBB36_13:                              # %sw.bb41
	movq	8(%rbx), %rdi
	movslq	(%rdi), %r12
	testq	%r12, %r12
	jle	.LBB36_26
# BB#14:                                # %for.body49.lr.ph
	movq	$3, %rcx
	leaq	-1(%r12), %r13
	shlq	%cl, %r12
	jmp	.LBB36_15
	.align	16, 0x90
.LBB36_16:                              # %for.body49.for.body49_crit_edge
                                        #   in Loop: Header=BB36_15 Depth=1
	movq	8(%rbx), %rdi
	decq	%r13
	addq	$-8, %r12
.LBB36_15:                              # %for.body49
                                        # =>This Inner Loop Header: Depth=1
	addq	%r12, %rdi
	movq	%r15, %rsi
	callq	*%r14
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB36_16
	jmp	.LBB36_26
.LBB36_4:                               # %sw.bb4
	movq	8(%rbx), %rdi
	movslq	(%rdi), %r12
	testq	%r12, %r12
	jle	.LBB36_26
# BB#5:                                 # %for.body.lr.ph
	movq	$3, %rcx
	leaq	-1(%r12), %r13
	shlq	%cl, %r12
	jmp	.LBB36_6
	.align	16, 0x90
.LBB36_7:                               # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB36_6 Depth=1
	movq	8(%rbx), %rdi
	decq	%r13
	addq	$-8, %r12
.LBB36_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	addq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	note_uses
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB36_7
	jmp	.LBB36_26
.LBB36_8:                               # %sw.bb15
	movq	32(%rbx), %rdi
	movslq	(%rdi), %r12
	testq	%r12, %r12
	jle	.LBB36_26
# BB#9:                                 # %for.body23.lr.ph
	movq	$3, %rcx
	leaq	-1(%r12), %r13
	shlq	%cl, %r12
	jmp	.LBB36_10
	.align	16, 0x90
.LBB36_11:                              # %for.body23.for.body23_crit_edge
                                        #   in Loop: Header=BB36_10 Depth=1
	movq	32(%rbx), %rdi
	decq	%r13
	addq	$-8, %r12
.LBB36_10:                              # %for.body23
                                        # =>This Inner Loop Header: Depth=1
	addq	%r12, %rdi
	movq	%r15, %rsi
	callq	*%r14
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB36_11
	jmp	.LBB36_26
.LBB36_19:                              # %sw.bb72
	movq	8(%rbx), %r12
	addq	$16, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*%r14
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jne	.LBB36_21
# BB#20:                                # %if.then82
	leaq	16(%r12), %rdi
	movq	%r15, %rsi
	callq	*%r14
	leaq	24(%r12), %rdi
	movq	%r15, %rsi
	callq	*%r14
	movl	(%r12), %eax
.LBB36_21:                              # %while.cond.preheader
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	ja	.LBB36_23
	.align	16, 0x90
.LBB36_22:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %r12
	movzbl	(%r12), %eax
	leaq	-63(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jb	.LBB36_22
.LBB36_23:                              # %while.end
	cmpq	$66, %rax
	jne	.LBB36_26
# BB#24:                                # %if.then102
	addq	$8, %r12
	movq	%r12, %rdi
	jmp	.LBB36_25
.LBB36_17:                              # %sw.bb59
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB36_26
# BB#18:                                # %if.then
	addq	$8, %rdi
	jmp	.LBB36_25
.LBB36_26:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp484:
	.size	note_uses, .Ltmp484-note_uses
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI36_0:
	.quad	.LBB36_3
	.quad	.LBB36_4
	.quad	.LBB36_25
	.quad	.LBB36_8
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_25
	.quad	.LBB36_25
	.quad	.LBB36_12
	.quad	.LBB36_19
	.quad	.LBB36_12
	.quad	.LBB36_17
	.quad	.LBB36_25
	.quad	.LBB36_25
	.quad	.LBB36_12

	.text
	.globl	dead_or_set_p
	.align	16, 0x90
	.type	dead_or_set_p,@function
dead_or_set_p:                          # @dead_or_set_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp490:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp491:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp492:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp493:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp494:
	.cfi_def_cfa_offset 48
.Ltmp495:
	.cfi_offset %rbx, -40
.Ltmp496:
	.cfi_offset %r12, -32
.Ltmp497:
	.cfi_offset %r14, -24
.Ltmp498:
	.cfi_offset %r15, -16
	movl	(%rsi), %eax
	movq	%rdi, %r14
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB37_1
# BB#3:                                 # %if.end5
	movl	8(%rsi), %ebx
	cmpq	$52, %rbx
	movq	%rbx, %r15
	ja	.LBB37_19
# BB#4:                                 # %cond.false
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rbx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB37_8
# BB#5:                                 # %cond.false
	leaq	-21(%rbx), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB37_8
# BB#6:                                 # %cond.false
	leaq	-45(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB37_8
# BB#7:                                 # %cond.false
	leaq	-29(%rbx), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB37_8
# BB#13:                                # %cond.false31
	cmpq	$24, %rax
	jne	.LBB37_14
# BB#16:                                # %cond.true43
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB37_18
.LBB37_1:                               # %entry
	cmpq	$69, %rcx
	movq	$1, %rax
	je	.LBB37_24
# BB#2:                                 # %if.then4
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.dead_or_set_p, %rcx
	movq	$1678, %rsi             # imm = 0x68E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB37_8:                               # %cond.true20
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB37_18
# BB#9:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB37_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB37_18
.LBB37_14:                              # %cond.false31
	cmpq	$18, %rax
	jne	.LBB37_17
# BB#15:                                # %cond.true36
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB37_18
.LBB37_10:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB37_18
.LBB37_17:                              # %cond.false47
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB37_18:                              # %cond.end62
	leaq	-1(%rbx,%rax), %r15
.LBB37_19:                              # %cond.end66
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r15
	cmpq	%r15, %rbx
	jbe	.LBB37_23
# BB#20:
	movq	$1, %rax
	jmp	.LBB37_24
	.align	16, 0x90
.LBB37_23:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	dead_or_set_regno_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r12, %rcx
	je	.LBB37_24
# BB#21:                                # %for.cond
                                        #   in Loop: Header=BB37_23 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	%r15, %rax
	jbe	.LBB37_23
# BB#22:
	movq	$1, %rax
.LBB37_24:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp499:
	.size	dead_or_set_p, .Ltmp499-dead_or_set_p
	.cfi_endproc

	.globl	dead_or_set_regno_p
	.align	16, 0x90
	.type	dead_or_set_regno_p,@function
dead_or_set_regno_p:                    # @dead_or_set_regno_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp506:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp507:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp508:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp509:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp510:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp511:
	.cfi_def_cfa_offset 64
.Ltmp512:
	.cfi_offset %rbx, -48
.Ltmp513:
	.cfi_offset %r12, -40
.Ltmp514:
	.cfi_offset %r13, -32
.Ltmp515:
	.cfi_offset %r14, -24
.Ltmp516:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$1, %rsi
                                        # kill: RDI<def> RBX<kill>
	movq	%r14, %rdx
	callq	find_regno_note
	testq	%rax, %rax
	je	.LBB38_2
# BB#1:
	movq	$1, %r10
.LBB38_64:                              # %return
	movq	%r10, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB38_2:                               # %if.end
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB38_5
# BB#3:                                 # %land.lhs.true
	movq	$49, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	find_regno_fusage
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB38_5
# BB#4:
	movq	$1, %r10
	jmp	.LBB38_64
.LBB38_5:                               # %if.end4
	movq	32(%rbx), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$38, %rdx
	movq	%rax, %rdx
	jne	.LBB38_7
# BB#6:                                 # %if.then8
	movq	16(%rax), %rdx
	movl	(%rdx), %ecx
.LBB38_7:                               # %if.end12
	xorq	%r10, %r10
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$39, %rcx
	jne	.LBB38_8
# BB#35:                                # %if.then160
	movq	8(%rdx), %r12
	movslq	(%r12), %rbx
	testq	%rbx, %rbx
	jle	.LBB38_64
# BB#36:                                # %for.body.lr.ph
	movl	target_flags(%rip), %r8d
	movq	$23, %rcx
	movq	%r8, %r9
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	4(%r9), %r11
	orq	$3, %r9
	movq	$25, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$24, %rcx
	shrq	%cl, %r8
	andq	$2, %r8
	xorq	$6, %r8
	decq	%rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB38_37:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12,%rbx,8), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$38, %rdx
	jne	.LBB38_39
# BB#38:                                # %if.then175
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	16(%rax), %rax
	movl	(%rax), %ecx
.LBB38_39:                              # %if.end179
                                        #   in Loop: Header=BB38_37 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	je	.LBB38_41
# BB#40:                                # %if.end179
                                        #   in Loop: Header=BB38_37 Depth=1
	cmpq	$49, %rcx
	jne	.LBB38_63
.LBB38_41:                              # %if.then189
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	8(%rax), %rsi
	movl	(%rsi), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB38_45
# BB#42:                                # %land.lhs.true198
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %eax
	leaq	-1(%rax,%r11), %rax
	cqto
	idivq	%r11
	movq	%rax, %rcx
	movq	8(%rsi), %rdi
	movzbl	2(%rdi), %eax
	movzbl	mode_size(%rax), %eax
	addq	%r9, %rax
	cqto
	idivq	%r11
	andq	%r15, %rcx
	andq	%r15, %rax
	cmpq	%rax, %rcx
	je	.LBB38_44
# BB#43:                                # %land.lhs.true198
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	%rsi, %rdi
.LBB38_44:                              # %land.lhs.true198
                                        #   in Loop: Header=BB38_37 Depth=1
	movl	(%rdi), %edx
	movq	%rdi, %rsi
.LBB38_45:                              # %if.end238
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB38_63
# BB#46:                                # %if.end244
                                        #   in Loop: Header=BB38_37 Depth=1
	movl	8(%rsi), %r13d
	cmpq	$53, %r13
	jb	.LBB38_48
# BB#47:                                # %cond.true250
                                        #   in Loop: Header=BB38_37 Depth=1
	leaq	1(%r13), %rax
	jmp	.LBB38_60
.LBB38_48:                              # %cond.false252
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	%r13, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	$8, %rax
	je	.LBB38_52
# BB#49:                                # %cond.false252
                                        #   in Loop: Header=BB38_37 Depth=1
	leaq	-21(%r13), %rax
	andq	%r15, %rax
	cmpq	$8, %rax
	jb	.LBB38_52
# BB#50:                                # %cond.false252
                                        #   in Loop: Header=BB38_37 Depth=1
	leaq	-45(%r13), %rax
	andq	%r15, %rax
	cmpq	$8, %rax
	jb	.LBB38_52
# BB#51:                                # %cond.false252
                                        #   in Loop: Header=BB38_37 Depth=1
	leaq	-29(%r13), %rax
	andq	%r15, %rax
	cmpq	$7, %rax
	jbe	.LBB38_52
# BB#56:                                # %cond.false294
                                        #   in Loop: Header=BB38_37 Depth=1
	cmpq	$18, %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	je	.LBB38_59
# BB#57:                                # %cond.false294
                                        #   in Loop: Header=BB38_37 Depth=1
	cmpq	$24, %rdx
	movq	%r8, %rax
	je	.LBB38_59
# BB#58:                                # %cond.false314
                                        #   in Loop: Header=BB38_37 Depth=1
	movzbl	mode_size(%rdx), %eax
	leaq	-1(%rax,%r11), %rax
	cqto
	idivq	%r11
	jmp	.LBB38_59
.LBB38_52:                              # %cond.true276
                                        #   in Loop: Header=BB38_37 Depth=1
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB38_59
# BB#53:                                # %lor.rhs284
                                        #   in Loop: Header=BB38_37 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB38_55
# BB#54:                                # %lor.rhs284
                                        #   in Loop: Header=BB38_37 Depth=1
	movabsq	$1, %rax
.LBB38_55:                              # %lor.rhs284
                                        #   in Loop: Header=BB38_37 Depth=1
	incq	%rax
.LBB38_59:                              # %cond.end334
                                        #   in Loop: Header=BB38_37 Depth=1
	addq	%r13, %rax
.LBB38_60:                              # %cond.end337
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	%r14, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %r13
	ja	.LBB38_63
# BB#61:                                # %cond.end337
                                        #   in Loop: Header=BB38_37 Depth=1
	andq	%r15, %rax
	cmpq	%rcx, %rax
	ja	.LBB38_62
	.align	16, 0x90
.LBB38_63:                              # %for.cond.backedge
                                        #   in Loop: Header=BB38_37 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB38_37
	jmp	.LBB38_64
.LBB38_8:                               # %if.end12
	cmpq	$47, %rcx
	jne	.LBB38_64
# BB#9:                                 # %if.then16
	movq	8(%rax), %rsi
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB38_13
# BB#10:                                # %land.lhs.true26
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	4(%rbx), %r8
	leaq	3(%rax,%rbx), %rax
	cqto
	idivq	%r8
	movq	%rax, %rcx
	movq	8(%rsi), %rdi
	movzbl	2(%rdi), %eax
	movzbl	mode_size(%rax), %eax
	leaq	3(%rbx,%rax), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	cqto
	idivq	%r8
	andq	%rbx, %rcx
	andq	%rbx, %rax
	cmpq	%rax, %rcx
	je	.LBB38_12
# BB#11:                                # %land.lhs.true26
	movq	%rsi, %rdi
.LBB38_12:                              # %land.lhs.true26
	movl	(%rdi), %eax
	movq	%rdi, %rsi
.LBB38_13:                              # %if.end58
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB38_64
# BB#14:                                # %if.end64
	movl	8(%rsi), %esi
	cmpq	$53, %rsi
	jb	.LBB38_16
# BB#15:                                # %cond.true
	leaq	1(%rsi), %rax
	jmp	.LBB38_31
.LBB38_62:
	movq	$1, %r10
	jmp	.LBB38_64
.LBB38_16:                              # %cond.false
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rsi, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB38_20
# BB#17:                                # %cond.false
	leaq	-21(%rsi), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB38_20
# BB#18:                                # %cond.false
	leaq	-45(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB38_20
# BB#19:                                # %cond.false
	leaq	-29(%rsi), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB38_20
# BB#25:                                # %cond.false108
	cmpq	$24, %rax
	jne	.LBB38_26
# BB#28:                                # %cond.true124
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB38_30
.LBB38_20:                              # %cond.true92
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB38_30
# BB#21:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB38_22
# BB#23:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB38_30
.LBB38_26:                              # %cond.false108
	cmpq	$18, %rax
	jne	.LBB38_29
# BB#27:                                # %cond.true114
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB38_30
.LBB38_22:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB38_30
.LBB38_29:                              # %cond.false128
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB38_30:                              # %cond.end147
	addq	%rsi, %rax
.LBB38_31:                              # %cond.end150
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %r14
	cmpq	%rax, %r14
	sbbq	%r10, %r10
	cmpq	%r14, %rsi
	ja	.LBB38_32
# BB#33:                                # %cond.end150
	movabsq	$1, %rax
	andq	%rax, %r10
	jmp	.LBB38_64
.LBB38_32:
	movabsq	$0, %rax
	andq	%rax, %r10
	jmp	.LBB38_64
.Ltmp517:
	.size	dead_or_set_regno_p, .Ltmp517-dead_or_set_regno_p
	.cfi_endproc

	.globl	find_regno_note
	.align	16, 0x90
	.type	find_regno_note,@function
find_regno_note:                        # @find_regno_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp524:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp525:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp526:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp527:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp528:
	.cfi_def_cfa_offset 48
.Ltmp529:
	.cfi_offset %rbx, -48
.Ltmp530:
	.cfi_offset %r12, -40
.Ltmp531:
	.cfi_offset %r13, -32
.Ltmp532:
	.cfi_offset %r14, -24
.Ltmp533:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	movq	%rdx, %r13
	xorq	%r8, %r8
	cmpq	$105, %rax
	jne	.LBB39_20
# BB#1:                                 # %if.end
	movq	56(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB39_20
# BB#2:                                 # %for.body.lr.ph
	movl	target_flags(%rip), %r10d
	movq	$23, %rcx
	movq	%r10, %r11
	shrq	%cl, %r11
	andq	$4, %r11
	addq	$4, %r11
	movq	$25, %rcx
	movq	%r10, %rbx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	movq	$24, %rcx
	shrq	%cl, %r10
	andq	$2, %r10
	xorq	$6, %r10
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rsi
	movabsq	$4294967288, %r15       # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB39_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2(%rdi), %eax
	cmpq	%rsi, %rax
	jne	.LBB39_19
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB39_3 Depth=1
	movq	8(%rdi), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB39_19
# BB#5:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB39_3 Depth=1
	movl	8(%rax), %r12d
	movq	%r13, %r9
	andq	%r14, %r9
	cmpq	%r9, %r12
	ja	.LBB39_19
# BB#6:                                 # %land.lhs.true22
                                        #   in Loop: Header=BB39_3 Depth=1
	movq	$1, %rax
	cmpq	$52, %r12
	ja	.LBB39_17
# BB#7:                                 # %cond.false
                                        #   in Loop: Header=BB39_3 Depth=1
	movq	%r12, %rax
	andq	%r15, %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	$8, %rax
	je	.LBB39_10
# BB#8:                                 # %cond.false
                                        #   in Loop: Header=BB39_3 Depth=1
	cmpq	$44, %r12
	ja	.LBB39_10
# BB#9:                                 # %cond.false
                                        #   in Loop: Header=BB39_3 Depth=1
	leaq	-21(%r12), %rax
	andq	%r14, %rax
	cmpq	$15, %rax
	jbe	.LBB39_10
# BB#14:                                # %cond.false128
                                        #   in Loop: Header=BB39_3 Depth=1
	cmpq	$18, %rdx
	movq	%rbx, %rax
	je	.LBB39_17
# BB#15:                                # %cond.false128
                                        #   in Loop: Header=BB39_3 Depth=1
	cmpq	$24, %rdx
	movq	%r10, %rax
	je	.LBB39_17
# BB#16:                                # %cond.false153
                                        #   in Loop: Header=BB39_3 Depth=1
	movzbl	mode_size(%rdx), %eax
	leaq	-1(%rax,%r11), %rax
	cqto
	idivq	%r11
	jmp	.LBB39_17
.LBB39_10:                              # %cond.true107
                                        #   in Loop: Header=BB39_3 Depth=1
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB39_17
# BB#11:                                # %lor.rhs
                                        #   in Loop: Header=BB39_3 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB39_13
# BB#12:                                # %lor.rhs
                                        #   in Loop: Header=BB39_3 Depth=1
	movabsq	$1, %rax
.LBB39_13:                              # %lor.rhs
                                        #   in Loop: Header=BB39_3 Depth=1
	incq	%rax
.LBB39_17:                              # %cond.end174
                                        #   in Loop: Header=BB39_3 Depth=1
	addq	%r12, %rax
	andq	%r14, %rax
	cmpq	%r9, %rax
	ja	.LBB39_18
	.align	16, 0x90
.LBB39_19:                              # %for.inc
                                        #   in Loop: Header=BB39_3 Depth=1
	movq	16(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB39_3
	jmp	.LBB39_20
.LBB39_18:
	movq	%rdi, %r8
.LBB39_20:                              # %return
	movq	%r8, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp534:
	.size	find_regno_note, .Ltmp534-find_regno_note
	.cfi_endproc

	.globl	find_regno_fusage
	.align	16, 0x90
	.type	find_regno_fusage,@function
find_regno_fusage:                      # @find_regno_fusage
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp541:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp542:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp543:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp544:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp545:
	.cfi_def_cfa_offset 48
.Ltmp546:
	.cfi_offset %rbx, -48
.Ltmp547:
	.cfi_offset %r12, -40
.Ltmp548:
	.cfi_offset %r13, -32
.Ltmp549:
	.cfi_offset %r14, -24
.Ltmp550:
	.cfi_offset %r15, -16
	movq	%rdx, %r8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r8, %rax
	andq	%r9, %rax
	cmpq	$52, %rax
	ja	.LBB40_20
# BB#1:                                 # %lor.lhs.false
	movzbl	(%rdi), %eax
	cmpq	$34, %rax
	jne	.LBB40_20
# BB#2:                                 # %if.end
	movq	64(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB40_20
# BB#3:                                 # %for.body.lr.ph
	movl	target_flags(%rip), %r10d
	movq	$23, %rcx
	movq	%r10, %r11
	shrq	%cl, %r11
	andq	$4, %r11
	addq	$4, %r11
	movq	$25, %rcx
	movq	%r10, %r13
	shrq	%cl, %r13
	andq	$1, %r13
	xorq	$3, %r13
	movq	$24, %rcx
	shrq	%cl, %r10
	andq	$2, %r10
	xorq	$6, %r10
	andq	%r9, %rsi
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	movabsq	$0, %r15
	.align	16, 0x90
.LBB40_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	%rsi, %rcx
	jne	.LBB40_19
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB40_4 Depth=1
	movq	8(%rax), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB40_19
# BB#6:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB40_4 Depth=1
	movl	8(%rax), %r12d
	movq	%r8, %rbx
	andq	%r9, %rbx
	cmpq	%rbx, %r12
	ja	.LBB40_19
# BB#7:                                 # %land.lhs.true18
                                        #   in Loop: Header=BB40_4 Depth=1
	movq	%r12, %rax
	andq	%r14, %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	$8, %rax
	je	.LBB40_11
# BB#8:                                 # %land.lhs.true18
                                        #   in Loop: Header=BB40_4 Depth=1
	leaq	-21(%r12), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB40_11
# BB#9:                                 # %land.lhs.true18
                                        #   in Loop: Header=BB40_4 Depth=1
	leaq	-45(%r12), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB40_11
# BB#10:                                # %land.lhs.true18
                                        #   in Loop: Header=BB40_4 Depth=1
	leaq	-29(%r12), %rax
	andq	%r9, %rax
	cmpq	$7, %rax
	jbe	.LBB40_11
# BB#15:                                # %cond.false
                                        #   in Loop: Header=BB40_4 Depth=1
	cmpq	$18, %rdx
	movq	%r13, %rax
	je	.LBB40_18
# BB#16:                                # %cond.false
                                        #   in Loop: Header=BB40_4 Depth=1
	cmpq	$24, %rdx
	movq	%r10, %rax
	je	.LBB40_18
# BB#17:                                # %cond.false60
                                        #   in Loop: Header=BB40_4 Depth=1
	movzbl	mode_size(%rdx), %eax
	leaq	-1(%rax,%r11), %rax
	cqto
	idivq	%r11
	jmp	.LBB40_18
.LBB40_11:                              # %cond.true
                                        #   in Loop: Header=BB40_4 Depth=1
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB40_18
# BB#12:                                # %lor.rhs
                                        #   in Loop: Header=BB40_4 Depth=1
	cmpq	$6, %rcx
	movq	%r15, %rax
	jne	.LBB40_14
# BB#13:                                # %lor.rhs
                                        #   in Loop: Header=BB40_4 Depth=1
	movabsq	$1, %rax
.LBB40_14:                              # %lor.rhs
                                        #   in Loop: Header=BB40_4 Depth=1
	incq	%rax
.LBB40_18:                              # %cond.end75
                                        #   in Loop: Header=BB40_4 Depth=1
	addq	%r12, %rax
	andq	%r9, %rax
	cmpq	%rbx, %rax
	movq	$1, %rax
	ja	.LBB40_21
	.align	16, 0x90
.LBB40_19:                              # %for.inc
                                        #   in Loop: Header=BB40_4 Depth=1
	movq	16(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB40_4
.LBB40_20:
	xorq	%rax, %rax
.LBB40_21:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp551:
	.size	find_regno_fusage, .Ltmp551-find_regno_fusage
	.cfi_endproc

	.globl	find_reg_equal_equiv_note
	.align	16, 0x90
	.type	find_reg_equal_equiv_note,@function
find_reg_equal_equiv_note:              # @find_reg_equal_equiv_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp555:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp556:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp557:
	.cfi_def_cfa_offset 32
.Ltmp558:
	.cfi_offset %rbx, -24
.Ltmp559:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r14, %r14
	cmpq	$105, %rax
	jne	.LBB41_12
# BB#1:                                 # %cond.true
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB41_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB41_3:                               # %cond.end15
	testq	%rax, %rax
	je	.LBB41_12
# BB#4:                                 # %if.end.i
	movq	56(%rbx), %rax
	jmp	.LBB41_5
	.align	16, 0x90
.LBB41_7:                               # %for.inc.us.i
                                        #   in Loop: Header=BB41_5 Depth=1
	movq	16(%rax), %rax
.LBB41_5:                               # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB41_8
# BB#6:                                 # %for.body.us.i
                                        #   in Loop: Header=BB41_5 Depth=1
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$196608, %rcx           # imm = 0x30000
	jne	.LBB41_7
	jmp	.LBB41_11
.LBB41_8:                               # %if.end.i40
	movq	56(%rbx), %rax
	jmp	.LBB41_9
	.align	16, 0x90
.LBB41_13:                              # %for.inc.us.i52
                                        #   in Loop: Header=BB41_9 Depth=1
	movq	16(%rax), %rax
.LBB41_9:                               # %if.end.i40
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB41_12
# BB#10:                                # %for.body.us.i47
                                        #   in Loop: Header=BB41_9 Depth=1
	movl	(%rax), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$262144, %rcx           # imm = 0x40000
	jne	.LBB41_13
.LBB41_11:
	movq	%rax, %r14
.LBB41_12:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp560:
	.size	find_reg_equal_equiv_note, .Ltmp560-find_reg_equal_equiv_note
	.cfi_endproc

	.globl	pure_call_p
	.align	16, 0x90
	.type	pure_call_p,@function
pure_call_p:                            # @pure_call_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108898, %rax         # imm = 0x4000022
	jne	.LBB42_7
# BB#1:                                 # %if.end
	movq	64(%rdi), %rcx
	jmp	.LBB42_2
	.align	16, 0x90
.LBB42_6:                               # %for.inc
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	16(%rcx), %rcx
.LBB42_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB42_7
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	8(%rcx), %rax
	movzbl	(%rax), %edx
	cmpq	$48, %rdx
	jne	.LBB42_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	8(%rax), %rax
	movl	(%rax), %edx
	andq	$16777215, %rdx         # imm = 0xFFFFFF
	cmpq	$3342402, %rdx          # imm = 0x330042
	jne	.LBB42_6
# BB#5:                                 # %land.lhs.true21
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$62, %rax
	movq	$1, %rax
	jne	.LBB42_6
	jmp	.LBB42_8
.LBB42_7:
	xorq	%rax, %rax
.LBB42_8:                               # %return
	retq
.Ltmp561:
	.size	pure_call_p, .Ltmp561-pure_call_p
	.cfi_endproc

	.globl	remove_note
	.align	16, 0x90
	.type	remove_note,@function
remove_note:                            # @remove_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp563:
	.cfi_def_cfa_offset 16
	testq	%rsi, %rsi
	je	.LBB43_6
# BB#1:                                 # %if.end
	movq	56(%rdi), %rcx
	cmpq	%rsi, %rcx
	je	.LBB43_2
	.align	16, 0x90
.LBB43_3:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	testq	%rax, %rax
	je	.LBB43_7
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB43_3 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB43_3
# BB#5:                                 # %if.then17
	movq	16(%rsi), %rcx
	movq	%rcx, 16(%rax)
.LBB43_6:                               # %return
	popq	%rax
	retq
.LBB43_2:                               # %if.then2
	movq	16(%rsi), %rax
	movq	%rax, 56(%rdi)
	popq	%rax
	retq
.LBB43_7:                               # %for.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_note, %rcx
	movq	$1980, %rsi             # imm = 0x7BC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp564:
	.size	remove_note, .Ltmp564-remove_note
	.cfi_endproc

	.globl	in_expr_list_p
	.align	16, 0x90
	.type	in_expr_list_p,@function
in_expr_list_p:                         # @in_expr_list_p
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB44_1
	.align	16, 0x90
.LBB44_3:                               # %for.inc
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	16(%rdi), %rdi
.LBB44_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	je	.LBB44_4
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	8(%rdi), %rax
	cmpq	%rsi, %rax
	movq	$1, %rax
	jne	.LBB44_3
	jmp	.LBB44_5
.LBB44_4:
	xorq	%rax, %rax
.LBB44_5:                               # %return
	retq
.Ltmp565:
	.size	in_expr_list_p, .Ltmp565-in_expr_list_p
	.cfi_endproc

	.globl	remove_node_from_expr_list
	.align	16, 0x90
	.type	remove_node_from_expr_list,@function
remove_node_from_expr_list:             # @remove_node_from_expr_list
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB45_7
# BB#1:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB45_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	8(%rdx), %rcx
	cmpq	%rdi, %rcx
	je	.LBB45_3
# BB#6:                                 # %if.end12
                                        #   in Loop: Header=BB45_2 Depth=1
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rdx, %rax
	jne	.LBB45_2
.LBB45_7:                               # %while.end
	retq
.LBB45_3:                               # %if.then
	movq	16(%rdx), %rcx
	testq	%rax, %rax
	je	.LBB45_5
# BB#4:                                 # %if.then2
	movq	%rcx, 16(%rax)
	retq
.LBB45_5:                               # %if.else
	movq	%rcx, (%rsi)
	retq
.Ltmp566:
	.size	remove_node_from_expr_list, .Ltmp566-remove_node_from_expr_list
	.cfi_endproc

	.globl	volatile_insn_p
	.align	16, 0x90
	.type	volatile_insn_p,@function
volatile_insn_p:                        # @volatile_insn_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp573:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp574:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp575:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp576:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp577:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp578:
	.cfi_def_cfa_offset 80
.Ltmp579:
	.cfi_offset %rbx, -48
.Ltmp580:
	.cfi_offset %r12, -40
.Ltmp581:
	.cfi_offset %r13, -32
.Ltmp582:
	.cfi_offset %r14, -24
.Ltmp583:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movl	(%rdi), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-40(%rax), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$29, %rcx
	ja	.LBB46_7
# BB#1:                                 # %entry
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$1013827121, %rsi       # imm = 0x3C6DC631
	je	.LBB46_3
# BB#2:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB46_18
.LBB46_3:                               # %entry
	cmpq	$1, %rcx
	jne	.LBB46_4
# BB#6:                                 # %sw.bb2
	testq	$134217728, %rdx        # imm = 0x8000000
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB46_18
	jmp	.LBB46_7
.LBB46_4:                               # %entry
	cmpq	$3, %rcx
	jne	.LBB46_7
# BB#5:                                 # %sw.bb1
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB46_18
.LBB46_7:                               # %sw.epilog
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB46_8
# BB#9:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB46_10:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB46_17 Depth 2
	movq	%rbx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB46_11
# BB#14:                                # %for.cond25.preheader
                                        #   in Loop: Header=BB46_10 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %r13
	movslq	(%r13), %r14
	testq	%r14, %r14
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movq	$1, %r15
	jle	.LBB46_12
	.align	16, 0x90
.LBB46_17:                              # %for.body31
                                        #   Parent Loop BB46_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13,%r15,8), %rdi
	callq	volatile_insn_p
	testq	%r12, %rax
	jne	.LBB46_18
# BB#16:                                # %for.cond25
                                        #   in Loop: Header=BB46_17 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%r14, %rax
	jl	.LBB46_17
	jmp	.LBB46_12
	.align	16, 0x90
.LBB46_11:                              # %for.body
                                        #   in Loop: Header=BB46_10 Depth=1
	cmpq	$101, %rcx
	jne	.LBB46_12
# BB#15:                                # %if.then13
                                        #   in Loop: Header=BB46_10 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	callq	volatile_insn_p
	testq	%r12, %rax
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB46_18
.LBB46_12:                              # %for.cond.backedge
                                        #   in Loop: Header=BB46_10 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB46_10
# BB#13:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB46_18
.LBB46_8:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB46_18:                              # %return
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp584:
	.size	volatile_insn_p, .Ltmp584-volatile_insn_p
	.cfi_endproc

	.globl	volatile_refs_p
	.align	16, 0x90
	.type	volatile_refs_p,@function
volatile_refs_p:                        # @volatile_refs_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp591:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp592:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp593:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp594:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp595:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp596:
	.cfi_def_cfa_offset 80
.Ltmp597:
	.cfi_offset %rbx, -48
.Ltmp598:
	.cfi_offset %r12, -40
.Ltmp599:
	.cfi_offset %r13, -32
.Ltmp600:
	.cfi_offset %r14, -24
.Ltmp601:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movl	(%rdi), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-40(%rax), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$29, %rcx
	ja	.LBB47_6
# BB#1:                                 # %entry
	movq	$1, %rsi
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$946717233, %rdi        # imm = 0x386DC231
	je	.LBB47_3
# BB#2:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB47_17
.LBB47_3:                               # %entry
	shlq	%cl, %rsi
	testq	$67108866, %rsi         # imm = 0x4000002
	je	.LBB47_4
# BB#5:                                 # %sw.bb2
	testq	$134217728, %rdx        # imm = 0x8000000
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB47_17
	jmp	.LBB47_6
.LBB47_4:                               # %entry
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$1032, %rdx             # imm = 0x408
	jne	.LBB47_17
.LBB47_6:                               # %sw.epilog
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB47_7
# BB#8:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB47_9:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB47_16 Depth 2
	movq	%rbx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB47_10
# BB#13:                                # %for.cond25.preheader
                                        #   in Loop: Header=BB47_9 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %r13
	movslq	(%r13), %r14
	testq	%r14, %r14
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movq	$1, %r15
	jle	.LBB47_11
	.align	16, 0x90
.LBB47_16:                              # %for.body31
                                        #   Parent Loop BB47_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13,%r15,8), %rdi
	callq	volatile_refs_p
	testq	%r12, %rax
	jne	.LBB47_17
# BB#15:                                # %for.cond25
                                        #   in Loop: Header=BB47_16 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%r14, %rax
	jl	.LBB47_16
	jmp	.LBB47_11
	.align	16, 0x90
.LBB47_10:                              # %for.body
                                        #   in Loop: Header=BB47_9 Depth=1
	cmpq	$101, %rcx
	jne	.LBB47_11
# BB#14:                                # %if.then13
                                        #   in Loop: Header=BB47_9 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	callq	volatile_refs_p
	testq	%r12, %rax
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB47_17
.LBB47_11:                              # %for.cond.backedge
                                        #   in Loop: Header=BB47_9 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB47_9
# BB#12:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB47_17
.LBB47_7:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB47_17:                              # %return
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp602:
	.size	volatile_refs_p, .Ltmp602-volatile_refs_p
	.cfi_endproc

	.globl	may_trap_p
	.align	16, 0x90
	.type	may_trap_p,@function
may_trap_p:                             # @may_trap_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp609:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp610:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp611:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp612:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp613:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp614:
	.cfi_def_cfa_offset 64
.Ltmp615:
	.cfi_offset %rbx, -48
.Ltmp616:
	.cfi_offset %r12, -40
.Ltmp617:
	.cfi_offset %r13, -32
.Ltmp618:
	.cfi_offset %r14, -24
.Ltmp619:
	.cfi_offset %r15, -16
	testq	%rdi, %rdi
	je	.LBB48_1
# BB#2:                                 # %if.end
	movl	(%rdi), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	jg	.LBB48_5
# BB#3:                                 # %if.end
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	cmpq	$3, %rax
	jne	.LBB48_28
# BB#4:                                 # %sw.bb83
	movq	$1, %r13
	jmp	.LBB48_40
.LBB48_1:
	xorq	%r13, %r13
	jmp	.LBB48_40
.LBB48_5:                               # %if.end
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	cmpq	$128, %rax
	jg	.LBB48_27
# BB#6:                                 # %if.end
	cmpq	$103, %rax
	jg	.LBB48_23
# BB#7:                                 # %if.end
	leaq	-40(%rax), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$42, %rcx
	ja	.LBB48_28
# BB#8:                                 # %if.end
	xorq	%r13, %r13
	jmpq	*.LJTI48_0(,%rcx,8)
.LBB48_12:                              # %sw.bb6
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	16(%rcx), %rsi
	movzbl	(%rsi), %edi
	cmpq	$133, %rdi
	jg	.LBB48_17
# BB#13:                                # %sw.bb6
	leaq	-54(%rdi), %rcx
	andq	%rbx, %rcx
	cmpq	$14, %rcx
	jbe	.LBB48_15
# BB#14:
	movq	$1, %r13
	jmp	.LBB48_40
.LBB48_27:                              # %if.end
	cmpq	$129, %rax
	je	.LBB48_29
	jmp	.LBB48_28
.LBB48_23:                              # %if.end
	leaq	-104(%rax), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$4, %rsi
	jae	.LBB48_28
.LBB48_24:                              # %sw.bb84
	movq	$14, %rcx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	movq	$1, %r13
	cmpq	$2, %rcx
	je	.LBB48_40
# BB#25:                                # %if.end92
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	8(%rdx), %rcx
	movzbl	2(%rcx), %ecx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$2, %rcx
	je	.LBB48_40
# BB#26:                                # %lor.lhs.false102
	movq	16(%rdx), %rcx
	movzbl	2(%rcx), %ecx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB48_29
	jmp	.LBB48_40
.LBB48_28:                              # %sw.default
	movq	$14, %rcx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	cmpq	$2, %rcx
	movq	$1, %r13
	je	.LBB48_40
	jmp	.LBB48_29
.LBB48_9:                               # %sw.bb1
	movq	$1, %r13
	jmp	.LBB48_40
.LBB48_17:                              # %sw.bb6
	cmpq	$134, %rdi
	je	.LBB48_20
# BB#18:                                # %sw.bb6
	cmpq	$140, %rdi
	je	.LBB48_20
# BB#19:
	movq	$1, %r13
	jmp	.LBB48_40
.LBB48_10:                              # %sw.bb2
	movq	$27, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%rdx, %r13
	jmp	.LBB48_40
.LBB48_11:                              # %sw.bb5
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	rtx_addr_can_trap_p  # TAILCALL
.LBB48_15:                              # %sw.bb6
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$24599, %rbx            # imm = 0x6017
	je	.LBB48_16
.LBB48_20:                              # %lor.lhs.false61
	movq	$1, %r13
	movq	$14, %rcx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	cmpq	$2, %rcx
	je	.LBB48_40
# BB#21:                                # %if.end68
	cmpq	$54, %rdi
	jne	.LBB48_29
# BB#22:                                # %land.lhs.true
	cmpq	$0, 8(%rsi)
	je	.LBB48_40
.LBB48_29:                              # %sw.epilog
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB48_30
# BB#31:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB48_32:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB48_39 Depth 2
	movq	%rbx, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB48_33
# BB#36:                                # %for.cond148.preheader
                                        #   in Loop: Header=BB48_32 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %r12
	movslq	(%r12), %r14
	testq	%r14, %r14
	movq	$1, %r13
	movq	$1, %r15
	jle	.LBB48_34
	.align	16, 0x90
.LBB48_39:                              # %for.body154
                                        #   Parent Loop BB48_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r12,%r15,8), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB48_40
# BB#38:                                # %for.cond148
                                        #   in Loop: Header=BB48_39 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%r14, %rax
	jl	.LBB48_39
	jmp	.LBB48_34
	.align	16, 0x90
.LBB48_33:                              # %for.body
                                        #   in Loop: Header=BB48_32 Depth=1
	cmpq	$101, %rcx
	jne	.LBB48_34
# BB#37:                                # %if.then134
                                        #   in Loop: Header=BB48_32 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r13
	jne	.LBB48_40
.LBB48_34:                              # %for.cond.backedge
                                        #   in Loop: Header=BB48_32 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB48_32
# BB#35:
	xorq	%r13, %r13
	jmp	.LBB48_40
.LBB48_30:
	xorq	%r13, %r13
.LBB48_40:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB48_16:
	movq	$1, %r13
	jmp	.LBB48_40
.Ltmp620:
	.size	may_trap_p, .Ltmp620-may_trap_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI48_0:
	.quad	.LBB48_9
	.quad	.LBB48_10
	.quad	.LBB48_28
	.quad	.LBB48_9
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_9
	.quad	.LBB48_28
	.quad	.LBB48_40
	.quad	.LBB48_40
	.quad	.LBB48_40
	.quad	.LBB48_28
	.quad	.LBB48_40
	.quad	.LBB48_40
	.quad	.LBB48_28
	.quad	.LBB48_40
	.quad	.LBB48_40
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_11
	.quad	.LBB48_40
	.quad	.LBB48_40
	.quad	.LBB48_40
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_24
	.quad	.LBB48_28
	.quad	.LBB48_28
	.quad	.LBB48_29
	.quad	.LBB48_28
	.quad	.LBB48_12
	.quad	.LBB48_12
	.quad	.LBB48_12
	.quad	.LBB48_12

	.text
	.globl	inequality_comparisons_p
	.align	16, 0x90
	.type	inequality_comparisons_p,@function
inequality_comparisons_p:               # @inequality_comparisons_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp627:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp628:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp629:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp630:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp631:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp632:
	.cfi_def_cfa_offset 64
.Ltmp633:
	.cfi_offset %rbx, -48
.Ltmp634:
	.cfi_offset %r12, -40
.Ltmp635:
	.cfi_offset %r13, -32
.Ltmp636:
	.cfi_offset %r14, -24
.Ltmp637:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	(%r14), %edx
	leaq	-54(%rdx), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$57, %rcx
	ja	.LBB49_4
# BB#1:                                 # %entry
	xorq	%rax, %rax
	movq	$1, %rsi
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$57783, %rdi            # imm = 0xE1B7
	jne	.LBB49_13
# BB#2:                                 # %entry
	shlq	%cl, %rsi
	movabsq	$287104476244869120, %rax # imm = 0x3FC000000000000
	testq	%rax, %rsi
	je	.LBB49_4
# BB#3:                                 # %sw.bb1
	movq	$1, %rax
	jmp	.LBB49_13
.LBB49_4:                               # %sw.epilog
	movzbl	rtx_length(%rdx), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB49_13
# BB#5:
	movq	rtx_format(,%rdx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB49_6:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB49_11 Depth 2
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	(%rax,%rbx), %eax
	cmpq	$69, %rax
	jne	.LBB49_7
# BB#10:                                # %if.then18
                                        #   in Loop: Header=BB49_6 Depth=1
	movq	8(%r14,%rbx,8), %r13
	movslq	(%r13), %r15
	.align	16, 0x90
.LBB49_11:                              # %for.cond22
                                        #   Parent Loop BB49_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB49_9
# BB#12:                                # %for.body25
                                        #   in Loop: Header=BB49_11 Depth=2
	movq	(%r13,%r15,8), %rdi
	decq	%r15
	callq	inequality_comparisons_p
	testq	%r12, %rax
	movq	$1, %rax
	je	.LBB49_11
	jmp	.LBB49_13
	.align	16, 0x90
.LBB49_7:                               # %for.body
                                        #   in Loop: Header=BB49_6 Depth=1
	cmpq	$101, %rax
	jne	.LBB49_9
# BB#8:                                 # %if.then
                                        #   in Loop: Header=BB49_6 Depth=1
	movq	8(%r14,%rbx,8), %rdi
	callq	inequality_comparisons_p
	testq	%r12, %rax
	movq	$1, %rax
	jne	.LBB49_13
.LBB49_9:                               # %for.inc38
                                        #   in Loop: Header=BB49_6 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rdx
	jl	.LBB49_6
.LBB49_13:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp638:
	.size	inequality_comparisons_p, .Ltmp638-inequality_comparisons_p
	.cfi_endproc

	.globl	replace_rtx
	.align	16, 0x90
	.type	replace_rtx,@function
replace_rtx:                            # @replace_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp645:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp646:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp647:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp648:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp649:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp650:
	.cfi_def_cfa_offset 64
.Ltmp651:
	.cfi_offset %rbx, -48
.Ltmp652:
	.cfi_offset %r12, -40
.Ltmp653:
	.cfi_offset %r13, -32
.Ltmp654:
	.cfi_offset %r14, -24
.Ltmp655:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB50_1
# BB#4:                                 # %land.lhs.true
	movzbl	(%rbx), %eax
	cmpq	$55, %rax
	je	.LBB50_28
# BB#5:                                 # %if.end
	cmpq	%r15, %rbx
	movq	%r14, %rax
	je	.LBB50_7
# BB#6:                                 # %if.end
	movabsq	$0, %rax
.LBB50_7:                               # %if.end
	je	.LBB50_29
# BB#8:                                 # %if.end
	testq	%rbx, %rbx
	je	.LBB50_29
# BB#9:                                 # %if.end7
	movzbl	(%rbx), %eax
	cmpq	$121, %rax
	jne	.LBB50_10
# BB#15:                                # %if.then37
	movq	8(%rbx), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	replace_rtx
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB50_27
# BB#16:                                # %if.then46
	movq	8(%rbx), %rcx
	movzbl	2(%rbx), %esi
	movzbl	2(%rcx), %ecx
	movq	$121, %rdi
	movq	%rax, %rdx
	callq	simplify_unary_operation
	testq	%rax, %rax
	jne	.LBB50_29
# BB#17:                                # %if.then58
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.replace_rtx, %rcx
	movq	$2466, %rsi             # imm = 0x9A2
	jmp	.LBB50_14
.LBB50_1:                               # %if.end.thread
	cmpq	%r15, %rbx
	je	.LBB50_3
# BB#2:                                 # %if.end.thread
	xorq	%r14, %r14
.LBB50_3:                               # %if.end.thread
	movq	%r14, %rax
	jmp	.LBB50_29
.LBB50_10:                              # %if.end7
	cmpq	$63, %rax
	jne	.LBB50_18
# BB#11:                                # %if.then11
	movq	8(%rbx), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	replace_rtx
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB50_27
# BB#12:                                # %if.then15
	movq	8(%rbx), %rdx
	movl	16(%rbx), %ecx
	movzbl	2(%rbx), %edi
	movzbl	2(%rdx), %edx
	movq	%rax, %rsi
	callq	simplify_subreg
	testq	%rax, %rax
	jne	.LBB50_29
# BB#13:                                # %if.then27
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.replace_rtx, %rcx
	movq	$2450, %rsi             # imm = 0x992
.LBB50_14:                              # %if.then27
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB50_27:                              # %if.else
	movq	%rax, 8(%rbx)
	jmp	.LBB50_28
.LBB50_18:                              # %if.end66
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB50_28
# BB#19:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB50_20:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB50_25 Depth 2
	movq	%rdx, %r12
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%r12), %eax
	leaq	-1(%r12), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB50_21
# BB#24:                                # %if.then97
                                        #   in Loop: Header=BB50_20 Depth=1
	movq	(%rbx,%r12,8), %rax
	movslq	(%rax), %r13
	testq	%r13, %r13
	jle	.LBB50_22
	jmp	.LBB50_25
	.align	16, 0x90
.LBB50_26:                              # %for.body105.for.body105_crit_edge
                                        #   in Loop: Header=BB50_25 Depth=2
	movq	(%rbx,%r12,8), %rax
.LBB50_25:                              # %for.body105.lr.ph
                                        #   Parent Loop BB50_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%r13
	movq	8(%rax,%r13,8), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	replace_rtx
	movq	(%rbx,%r12,8), %rcx
	movq	%rax, 8(%rcx,%r13,8)
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB50_26
	jmp	.LBB50_22
	.align	16, 0x90
.LBB50_21:                              # %for.body
                                        #   in Loop: Header=BB50_20 Depth=1
	cmpq	$101, %rax
	jne	.LBB50_22
# BB#23:                                # %if.then81
                                        #   in Loop: Header=BB50_20 Depth=1
	movq	(%rbx,%r12,8), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	replace_rtx
	movq	%rax, (%rbx,%r12,8)
.LBB50_22:                              # %for.cond.backedge
                                        #   in Loop: Header=BB50_20 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB50_20
.LBB50_28:                              # %return
	movq	%rbx, %rax
.LBB50_29:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp656:
	.size	replace_rtx, .Ltmp656-replace_rtx
	.cfi_endproc

	.globl	replace_regs
	.align	16, 0x90
	.type	replace_regs,@function
replace_regs:                           # @replace_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp663:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp664:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp665:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp666:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp667:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp668:
	.cfi_def_cfa_offset 80
.Ltmp669:
	.cfi_offset %rbx, -48
.Ltmp670:
	.cfi_offset %r12, -40
.Ltmp671:
	.cfi_offset %r13, -32
.Ltmp672:
	.cfi_offset %r14, -24
.Ltmp673:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r10
	xorq	%rax, %rax
	testq	%r10, %r10
	je	.LBB51_5
# BB#1:                                 # %if.end
	movl	(%r10), %edi
	movq	%rdi, %r9
	andq	$65535, %r9             # imm = 0xFFFF
	leaq	-47(%r9), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$22, %rdx
	ja	.LBB51_22
# BB#2:                                 # %if.end
	movq	%r10, %rax
	jmpq	*.LJTI51_0(,%rdx,8)
.LBB51_15:                              # %sw.bb88
	movq	8(%r10), %rdi
	testq	%rsi, %rcx
	je	.LBB51_17
# BB#16:                                # %if.then89
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r10, %rbx
	callq	replace_regs
	movq	%rbx, %r10
	movq	%rax, 8(%r10)
	jmp	.LBB51_21
.LBB51_3:                               # %sw.bb1
	movl	8(%r10), %eax
	andq	%rsi, %r15
	cmpq	%r15, %rax
	jae	.LBB51_4
# BB#6:                                 # %land.lhs.true
	movq	(%r14,%rax,8), %rdi
	testq	%rdi, %rdi
	movq	%r10, %rax
	je	.LBB51_5
# BB#7:                                 # %if.then8
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	jne	.LBB51_8
# BB#9:                                 # %if.then17
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	copy_rtx  # TAILCALL
.LBB51_10:                              # %sw.bb30
	movq	8(%r10), %r8
	movl	(%r8), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB51_22
# BB#11:                                # %land.lhs.true36
	movl	8(%r8), %eax
	andq	%r15, %rsi
	cmpq	%rsi, %rax
	jae	.LBB51_22
# BB#12:                                # %land.lhs.true44
	movq	(%r14,%rax,8), %rsi
	testq	%rsi, %rsi
	je	.LBB51_22
# BB#13:                                # %land.lhs.true54
	movzbl	(%rsi), %eax
	cmpq	$63, %rax
	jne	.LBB51_22
# BB#14:                                # %if.then66
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	16(%r10), %eax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rax, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	simplify_gen_subreg  # TAILCALL
.LBB51_17:                              # %if.else
	movzbl	(%rdi), %eax
	cmpq	$64, %rax
	je	.LBB51_20
# BB#18:                                # %if.else
	cmpq	$133, %rax
	jne	.LBB51_19
.LBB51_22:                              # %sw.epilog
	movq	%r15, %r12
	movq	%r14, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%r9), %edx
	testq	%rdx, %rdx
	je	.LBB51_4
# BB#23:                                # %for.body.lr.ph
	movq	%rcx, %rbx
	movq	rtx_format(,%r9,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB51_24:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB51_29 Depth 2
	movq	%rdx, %r13
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB51_25
# BB#28:                                # %for.cond169.preheader
                                        #   in Loop: Header=BB51_24 Depth=1
	movq	(%r10,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r14
	jle	.LBB51_26
	.align	16, 0x90
.LBB51_29:                              # %for.body175
                                        #   Parent Loop BB51_24 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r14,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%r10, %r15
	callq	replace_regs
	movq	%r15, %r10
	movq	(%r10,%r13,8), %rcx
	movq	%rax, (%rcx,%r14,8)
	movq	(%r10,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB51_29
	jmp	.LBB51_26
	.align	16, 0x90
.LBB51_25:                              # %for.body
                                        #   in Loop: Header=BB51_24 Depth=1
	cmpq	$101, %rax
	jne	.LBB51_26
# BB#27:                                # %if.then152
                                        #   in Loop: Header=BB51_24 Depth=1
	movq	(%r10,%r13,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	%rbx, %rcx
	movq	%r10, %r14
	callq	replace_regs
	movq	%r14, %r10
	movq	%rax, (%r10,%r13,8)
.LBB51_26:                              # %for.cond.backedge
                                        #   in Loop: Header=BB51_24 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB51_24
.LBB51_4:
	movq	%r10, %rax
.LBB51_5:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB51_8:
	movq	%rdi, %rax
	jmp	.LBB51_5
.LBB51_19:                              # %if.else
	cmpq	$66, %rax
	jne	.LBB51_21
.LBB51_20:                              # %if.then109
	movq	8(%rdi), %rdi
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r10, %rbx
	callq	replace_regs
	movq	%rbx, %r10
	movq	8(%r10), %rcx
	movq	%rax, 8(%rcx)
.LBB51_21:                              # %if.end133
	movq	16(%r10), %rdi
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r10, %rbx
	callq	replace_regs
	movq	%rax, 16(%rbx)
	movq	%rbx, %rax
	jmp	.LBB51_5
.Ltmp674:
	.size	replace_regs, .Ltmp674-replace_regs
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI51_0:
	.quad	.LBB51_15
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_5
	.quad	.LBB51_5
	.quad	.LBB51_5
	.quad	.LBB51_22
	.quad	.LBB51_5
	.quad	.LBB51_5
	.quad	.LBB51_22
	.quad	.LBB51_3
	.quad	.LBB51_5
	.quad	.LBB51_10
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_22
	.quad	.LBB51_5
	.quad	.LBB51_5
	.quad	.LBB51_5

	.text
	.globl	computed_jump_p
	.align	16, 0x90
	.type	computed_jump_p,@function
computed_jump_p:                        # @computed_jump_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp680:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp681:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp682:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp683:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp684:
	.cfi_def_cfa_offset 48
.Ltmp685:
	.cfi_offset %rbx, -40
.Ltmp686:
	.cfi_offset %r12, -32
.Ltmp687:
	.cfi_offset %r14, -24
.Ltmp688:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB52_27
# BB#1:                                 # %if.then
	movzbl	rtx_class+33(%rip), %eax
	movq	32(%rdi), %rcx
	cmpq	$105, %rax
	jne	.LBB52_6
# BB#2:                                 # %if.end.i
	movq	56(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB52_6
# BB#3:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB52_4:                               # %for.body.us.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx), %esi
	andq	$16711680, %rsi         # imm = 0xFF0000
	cmpq	$851968, %rsi           # imm = 0xD0000
	je	.LBB52_28
# BB#5:                                 # %for.inc.us.i
                                        #   in Loop: Header=BB52_4 Depth=1
	movq	16(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB52_4
.LBB52_6:                               # %if.else
	movzbl	(%rcx), %eax
	cmpq	$47, %rax
	jne	.LBB52_7
# BB#25:                                # %land.lhs.true77
	movq	8(%rcx), %rax
	movq	global_rtl(%rip), %rdx
	cmpq	%rdx, %rax
	jne	.LBB52_27
# BB#26:                                # %land.lhs.true82
	movq	16(%rcx), %rdi
	callq	computed_jump_p_1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB52_28
	jmp	.LBB52_27
.LBB52_7:                               # %if.else
	cmpq	$39, %rax
	jne	.LBB52_27
# BB#8:                                 # %if.then5
	movq	8(%rcx), %r14
	movslq	(%r14), %rbx
	testq	%rbx, %rbx
	jle	.LBB52_27
# BB#9:                                 # %for.body.lr.ph
	leaq	-1(%rbx), %rdx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB52_10:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14,%rdx,8), %rcx
	movzbl	(%rcx), %esi
	cmpq	$48, %rsi
	jne	.LBB52_14
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB52_10 Depth=1
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$67, %rcx
	movq	$1, %rcx
	je	.LBB52_13
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB52_10 Depth=1
	movq	%rax, %rcx
.LBB52_13:                              # %land.lhs.true
                                        #   in Loop: Header=BB52_10 Depth=1
	movq	%rcx, %rax
.LBB52_14:                              # %for.inc
                                        #   in Loop: Header=BB52_10 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rdx
	testq	%rsi, %rsi
	jg	.LBB52_10
# BB#15:                                # %for.end
	testq	%rbx, %rbx
	jg	.LBB52_16
# BB#17:                                # %for.end
	movabsq	$1, %rcx
	jmp	.LBB52_18
.LBB52_16:
	movabsq	$0, %rcx
.LBB52_18:                              # %for.end
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB52_27
# BB#19:                                # %for.end
	testq	%rcx, %rcx
	jne	.LBB52_27
# BB#20:                                # %for.body34.lr.ph
	movq	global_rtl(%rip), %r12
	decq	%rbx
	.align	16, 0x90
.LBB52_21:                              # %for.body34
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14,%rbx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB52_24
# BB#22:                                # %land.lhs.true44
                                        #   in Loop: Header=BB52_21 Depth=1
	movq	8(%rax), %rcx
	cmpq	%r12, %rcx
	jne	.LBB52_24
# BB#23:                                # %land.lhs.true55
                                        #   in Loop: Header=BB52_21 Depth=1
	movq	16(%rax), %rdi
	callq	computed_jump_p_1
	testq	%r15, %rax
	movq	$1, %rax
	jne	.LBB52_28
.LBB52_24:                              # %for.inc69
                                        #   in Loop: Header=BB52_21 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB52_21
.LBB52_27:                              # %if.end92
	xorq	%rax, %rax
.LBB52_28:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp689:
	.size	computed_jump_p, .Ltmp689-computed_jump_p
	.cfi_endproc

	.align	16, 0x90
	.type	computed_jump_p_1,@function
computed_jump_p_1:                      # @computed_jump_p_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp696:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp697:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp698:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp699:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp700:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp701:
	.cfi_def_cfa_offset 80
.Ltmp702:
	.cfi_offset %rbx, -48
.Ltmp703:
	.cfi_offset %r12, -40
.Ltmp704:
	.cfi_offset %r13, -32
.Ltmp705:
	.cfi_offset %r14, -24
.Ltmp706:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movzbl	(%rdi), %eax
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$18, %rcx
	ja	.LBB53_13
# BB#1:                                 # %entry
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmpq	*.LJTI53_0(,%rcx,8)
.LBB53_2:                               # %sw.bb1
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB53_24
.LBB53_13:                              # %sw.epilog
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB53_14
# BB#15:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB53_16:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB53_23 Depth 2
	movq	%rbx, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %rbx
	cmpq	$69, %rcx
	jne	.LBB53_17
# BB#21:                                # %for.cond42.preheader
                                        #   in Loop: Header=BB53_16 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %r13
	movslq	(%r13), %r14
	testq	%r14, %r14
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movq	$1, %r15
	jle	.LBB53_19
	.align	16, 0x90
.LBB53_23:                              # %for.body48
                                        #   Parent Loop BB53_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13,%r15,8), %rdi
	callq	computed_jump_p_1
	testq	%r12, %rax
	movq	%r12, %rax
	jne	.LBB53_24
# BB#22:                                # %for.cond42
                                        #   in Loop: Header=BB53_23 Depth=2
	movq	%rax, %r12
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%r14, %rax
	jl	.LBB53_23
	jmp	.LBB53_19
	.align	16, 0x90
.LBB53_17:                              # %for.body
                                        #   in Loop: Header=BB53_16 Depth=1
	cmpq	$101, %rcx
	jne	.LBB53_19
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB53_16 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	callq	computed_jump_p_1
	testq	%r12, %rax
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB53_24
.LBB53_19:                              # %for.cond.backedge
                                        #   in Loop: Header=BB53_16 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB53_16
# BB#20:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB53_24
.LBB53_14:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB53_24:                              # %return
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB53_3:                               # %sw.bb2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$68, %rcx
	movq	$1, %rcx
	jne	.LBB53_7
# BB#4:                                 # %land.rhs
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB53_5
# BB#6:                                 # %land.rhs
	movabsq	$1, %rcx
.LBB53_7:                               # %land.end
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB53_24
.LBB53_8:                               # %sw.bb10
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rdi
	callq	computed_jump_p_1
	testq	%r12, %rax
	movq	%r12, %rax
	movq	$1, %rcx
	jne	.LBB53_12
# BB#9:                                 # %lor.rhs
	movq	24(%rbx), %rdi
	movq	%rax, %rbx
	callq	computed_jump_p_1
	testq	%rbx, %rax
	je	.LBB53_10
# BB#11:                                # %lor.rhs
	movabsq	$1, %rcx
.LBB53_12:                              # %lor.end
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB53_24
.LBB53_5:
	movabsq	$0, %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB53_24
.LBB53_10:
	movabsq	$0, %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB53_24
.Ltmp707:
	.size	computed_jump_p_1, .Ltmp707-computed_jump_p_1
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI53_0:
	.quad	.LBB53_2
	.quad	.LBB53_2
	.quad	.LBB53_2
	.quad	.LBB53_13
	.quad	.LBB53_2
	.quad	.LBB53_24
	.quad	.LBB53_13
	.quad	.LBB53_2
	.quad	.LBB53_13
	.quad	.LBB53_13
	.quad	.LBB53_13
	.quad	.LBB53_13
	.quad	.LBB53_3
	.quad	.LBB53_24
	.quad	.LBB53_2
	.quad	.LBB53_13
	.quad	.LBB53_13
	.quad	.LBB53_13
	.quad	.LBB53_8

	.text
	.globl	for_each_rtx
	.align	16, 0x90
	.type	for_each_rtx,@function
for_each_rtx:                           # @for_each_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp714:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp715:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp716:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp717:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp718:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp719:
	.cfi_def_cfa_offset 80
.Ltmp720:
	.cfi_offset %rbx, -48
.Ltmp721:
	.cfi_offset %r12, -40
.Ltmp722:
	.cfi_offset %r13, -32
.Ltmp723:
	.cfi_offset %r14, -24
.Ltmp724:
	.cfi_offset %r15, -16
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%rdx, %rsi
	callq	*%rax
	movq	%rax, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rsi, %rdx
	xorq	%rax, %rax
	cmpq	%rsi, %rdx
	je	.LBB54_15
# BB#1:                                 # %entry
	testq	%rsi, %rcx
	movq	%rcx, %rax
	jne	.LBB54_15
# BB#2:                                 # %if.end3
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	movabsq	$0, %rax
	je	.LBB54_15
# BB#3:                                 # %if.end6
	movzbl	(%rcx), %ecx
	movzbl	rtx_length(%rcx), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%r13, %r13
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB54_15
# BB#4:
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB54_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_14 Depth 2
	movq	8(%rsp), %rax           # 8-byte Reload
	movsbq	(%rax,%r13), %rax
	andq	%rsi, %rax
	cmpq	$69, %rax
	je	.LBB54_10
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB54_5 Depth=1
	cmpq	$86, %rax
	jne	.LBB54_7
.LBB54_10:                              # %sw.bb23
                                        #   in Loop: Header=BB54_5 Depth=1
	movq	(%rbx), %rax
	movq	8(%rax,%r13,8), %rdi
	testq	%rdi, %rdi
	je	.LBB54_9
# BB#11:                                # %for.cond30.preheader
                                        #   in Loop: Header=BB54_5 Depth=1
	movslq	(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB54_9
# BB#12:                                #   in Loop: Header=BB54_5 Depth=1
	movq	$8, %r12
	movq	$1, %r14
	.align	16, 0x90
.LBB54_14:                              # %for.body37
                                        #   Parent Loop BB54_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%r12, %rdi
	movq	%rsi, %r15
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	for_each_rtx
	movq	%r15, %rsi
	testq	%rsi, %rax
	jne	.LBB54_15
# BB#13:                                # %for.cond30
                                        #   in Loop: Header=BB54_14 Depth=2
	movq	(%rbx), %rax
	movq	8(%rax,%r13,8), %rdi
	movslq	(%rdi), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$8, %r12
	cmpq	%rax, %rdx
	jl	.LBB54_14
	jmp	.LBB54_9
	.align	16, 0x90
.LBB54_7:                               # %for.body
                                        #   in Loop: Header=BB54_5 Depth=1
	cmpq	$101, %rax
	jne	.LBB54_9
# BB#8:                                 # %sw.bb
                                        #   in Loop: Header=BB54_5 Depth=1
	movq	(%rbx), %rax
	leaq	8(%rax,%r13,8), %rdi
	movq	%rsi, %r14
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	for_each_rtx
	movq	%r14, %rsi
	testq	%rsi, %rax
	jne	.LBB54_15
	.align	16, 0x90
.LBB54_9:                               # %for.inc50
                                        #   in Loop: Header=BB54_5 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rdx
	jl	.LBB54_5
.LBB54_15:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp725:
	.size	for_each_rtx, .Ltmp725-for_each_rtx
	.cfi_endproc

	.globl	regno_use_in
	.align	16, 0x90
	.type	regno_use_in,@function
regno_use_in:                           # @regno_use_in
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp732:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp733:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp734:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp735:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp736:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp737:
	.cfi_def_cfa_offset 64
.Ltmp738:
	.cfi_offset %rbx, -48
.Ltmp739:
	.cfi_offset %r12, -40
.Ltmp740:
	.cfi_offset %r13, -32
.Ltmp741:
	.cfi_offset %r14, -24
.Ltmp742:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	(%r14), %eax
	movq	%rdi, %r13
	cmpq	$61, %rax
	jne	.LBB55_3
# BB#1:                                 # %land.lhs.true
	movl	8(%r14), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r13, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB55_3
# BB#2:
	movq	%r14, %rax
	jmp	.LBB55_14
.LBB55_3:                               # %if.end
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB55_4
# BB#5:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB55_6:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB55_12 Depth 2
	movq	%r15, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %r15
	cmpq	$69, %rcx
	jne	.LBB55_7
# BB#11:                                # %if.then27
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	(%r14,%rax,8), %rbx
	movslq	(%rbx), %r12
	.align	16, 0x90
.LBB55_12:                              # %for.cond32
                                        #   Parent Loop BB55_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB55_8
# BB#13:                                # %for.body35
                                        #   in Loop: Header=BB55_12 Depth=2
	movq	(%rbx,%r12,8), %rsi
	decq	%r12
	movq	%r13, %rdi
	callq	regno_use_in
	testq	%rax, %rax
	je	.LBB55_12
	jmp	.LBB55_14
	.align	16, 0x90
.LBB55_7:                               # %for.body
                                        #   in Loop: Header=BB55_6 Depth=1
	cmpq	$101, %rcx
	jne	.LBB55_8
# BB#10:                                # %if.then16
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	(%r14,%rax,8), %rsi
	movq	%r13, %rdi
	callq	regno_use_in
	testq	%rax, %rax
	jne	.LBB55_14
.LBB55_8:                               # %for.cond.backedge
                                        #   in Loop: Header=BB55_6 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB55_6
# BB#9:
	xorq	%rax, %rax
	jmp	.LBB55_14
.LBB55_4:
	xorq	%rax, %rax
.LBB55_14:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp743:
	.size	regno_use_in, .Ltmp743-regno_use_in
	.cfi_endproc

	.globl	commutative_operand_precedence
	.align	16, 0x90
	.type	commutative_operand_precedence,@function
commutative_operand_precedence:         # @commutative_operand_precedence
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %edx
	cmpq	$54, %rdx
	jne	.LBB56_2
# BB#1:
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	retq
.LBB56_2:                               # %if.end
	cmpq	$55, %rdx
	jne	.LBB56_4
# BB#3:
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	retq
.LBB56_4:                               # %if.end5
	leaq	-67(%rdx), %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$2, %rax
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	jb	.LBB56_17
# BB#5:                                 # %lor.lhs.false12
	cmpq	$133, %rdx
	jg	.LBB56_11
# BB#6:                                 # %lor.lhs.false12
	cmpq	$56, %rdx
	je	.LBB56_17
# BB#7:                                 # %lor.lhs.false12
	cmpq	$58, %rdx
	je	.LBB56_17
# BB#8:                                 # %lor.lhs.false12
	cmpq	$63, %rdx
	jne	.LBB56_13
# BB#9:                                 # %land.lhs.true
	movq	8(%rdi), %rax
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$111, %rax
	jne	.LBB56_15
# BB#10:
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	retq
.LBB56_11:                              # %lor.lhs.false12
	cmpq	$134, %rdx
	je	.LBB56_17
# BB#12:                                # %lor.lhs.false12
	cmpq	$140, %rdx
	je	.LBB56_17
.LBB56_13:                              # %if.end47
	leaq	-75(%rdx), %rcx
	andq	%rsi, %rcx
	cmpq	$11, %rcx
	ja	.LBB56_15
# BB#14:                                # %if.end47
	movq	$2, %rax
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$2063, %rsi             # imm = 0x80F
	jne	.LBB56_17
.LBB56_15:                              # %if.end73
	movzbl	rtx_class(%rdx), %eax
	cmpq	$111, %rax
	movq	$-1, %rax
	je	.LBB56_17
# BB#16:                                # %if.end73
	xorq	%rax, %rax
.LBB56_17:                              # %return
	retq
.Ltmp744:
	.size	commutative_operand_precedence, .Ltmp744-commutative_operand_precedence
	.cfi_endproc

	.globl	swap_commutative_operands_p
	.align	16, 0x90
	.type	swap_commutative_operands_p,@function
swap_commutative_operands_p:            # @swap_commutative_operands_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %edx
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	cmpq	$54, %rdx
	movq	%rax, %r8
	je	.LBB57_16
# BB#1:                                 # %if.end.i
	cmpq	$55, %rdx
	jne	.LBB57_3
# BB#2:
	movabsq	$4294967292, %r8        # imm = 0xFFFFFFFC
	jmp	.LBB57_16
.LBB57_3:                               # %if.end5.i
	leaq	-67(%rdx), %rcx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rcx
	cmpq	$2, %rcx
	movabsq	$4294967293, %r8        # imm = 0xFFFFFFFD
	jb	.LBB57_16
# BB#4:                                 # %lor.lhs.false12.i
	cmpq	$133, %rdx
	jg	.LBB57_10
# BB#5:                                 # %lor.lhs.false12.i
	cmpq	$56, %rdx
	je	.LBB57_16
# BB#6:                                 # %lor.lhs.false12.i
	cmpq	$58, %rdx
	je	.LBB57_16
# BB#7:                                 # %lor.lhs.false12.i
	cmpq	$63, %rdx
	jne	.LBB57_12
# BB#8:                                 # %land.lhs.true.i
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$111, %rcx
	jne	.LBB57_14
# BB#9:
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	jmp	.LBB57_16
.LBB57_10:                              # %lor.lhs.false12.i
	cmpq	$134, %rdx
	je	.LBB57_16
# BB#11:                                # %lor.lhs.false12.i
	cmpq	$140, %rdx
	je	.LBB57_16
.LBB57_12:                              # %if.end47.i
	leaq	-75(%rdx), %rcx
	andq	%r9, %rcx
	cmpq	$11, %rcx
	ja	.LBB57_14
# BB#13:                                # %if.end47.i
	movq	$2, %r8
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$2063, %rdi             # imm = 0x80F
	jne	.LBB57_16
.LBB57_14:                              # %if.end73.i
	movzbl	rtx_class(%rdx), %ecx
	cmpq	$111, %rcx
	movq	$-1, %r8
	je	.LBB57_16
# BB#15:                                # %if.end73.i
	xorq	%r8, %r8
.LBB57_16:                              # %commutative_operand_precedence.exit
	movzbl	(%rsi), %edx
	cmpq	$54, %rdx
	je	.LBB57_32
# BB#17:                                # %if.end.i6
	cmpq	$55, %rdx
	jne	.LBB57_19
# BB#18:
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	jmp	.LBB57_32
.LBB57_19:                              # %if.end5.i9
	leaq	-67(%rdx), %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$2, %rax
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	jb	.LBB57_32
# BB#20:                                # %lor.lhs.false12.i10
	cmpq	$133, %rdx
	jg	.LBB57_26
# BB#21:                                # %lor.lhs.false12.i10
	cmpq	$56, %rdx
	je	.LBB57_32
# BB#22:                                # %lor.lhs.false12.i10
	cmpq	$58, %rdx
	je	.LBB57_32
# BB#23:                                # %lor.lhs.false12.i10
	cmpq	$63, %rdx
	jne	.LBB57_28
# BB#24:                                # %land.lhs.true.i18
	movq	8(%rsi), %rax
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$111, %rax
	jne	.LBB57_30
# BB#25:
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	jmp	.LBB57_32
.LBB57_26:                              # %lor.lhs.false12.i10
	cmpq	$134, %rdx
	je	.LBB57_32
# BB#27:                                # %lor.lhs.false12.i10
	cmpq	$140, %rdx
	je	.LBB57_32
.LBB57_28:                              # %if.end47.i19
	leaq	-75(%rdx), %rcx
	andq	%rdi, %rcx
	cmpq	$11, %rcx
	ja	.LBB57_30
# BB#29:                                # %if.end47.i19
	movq	$2, %rax
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$2063, %rsi             # imm = 0x80F
	jne	.LBB57_32
.LBB57_30:                              # %if.end73.i24
	movzbl	rtx_class(%rdx), %eax
	cmpq	$111, %rax
	movq	$-1, %rax
	je	.LBB57_32
# BB#31:                                # %if.end73.i24
	xorq	%rax, %rax
.LBB57_32:                              # %commutative_operand_precedence.exit26
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	%rax, %r8
	jge	.LBB57_33
# BB#34:                                # %commutative_operand_precedence.exit26
	movabsq	$1, %rax
	retq
.LBB57_33:
	movabsq	$0, %rax
	retq
.Ltmp745:
	.size	swap_commutative_operands_p, .Ltmp745-swap_commutative_operands_p
	.cfi_endproc

	.globl	auto_inc_p
	.align	16, 0x90
	.type	auto_inc_p,@function
auto_inc_p:                             # @auto_inc_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	addq	$-96, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	ja	.LBB58_2
# BB#1:                                 # %sw.bb
	movq	8(%rdi), %rax
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB58_3
.LBB58_2:                               # %sw.epilog
	xorq	%rax, %rax
.LBB58_3:                               # %return
	retq
.Ltmp746:
	.size	auto_inc_p, .Ltmp746-auto_inc_p
	.cfi_endproc

	.globl	insns_safe_to_move_p
	.align	16, 0x90
	.type	insns_safe_to_move_p,@function
insns_safe_to_move_p:                   # @insns_safe_to_move_p
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdx, %rdx
	je	.LBB59_2
# BB#1:                                 # %if.then
	movq	%rsi, (%rdx)
.LBB59_2:                               # %while.cond.preheader
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB59_35
# BB#3:                                 # %while.body.lr.ph
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	je	.LBB59_17
# BB#4:
	movabsq	$4294967210, %r8        # imm = 0xFFFFFFAA
	movabsq	$4294967211, %r9        # imm = 0xFFFFFFAB
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	xorq	%r11, %r11
	.align	16, 0x90
.LBB59_5:                               # %while.body.us
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB59_6
# BB#7:                                 # %if.then2.us
                                        #   in Loop: Header=BB59_5 Depth=1
	movl	40(%rdi), %eax
	cmpq	%r8, %rax
	jne	.LBB59_8
# BB#11:                                # %sw.bb.us
                                        #   in Loop: Header=BB59_5 Depth=1
	incq	%r11
	jmp	.LBB59_12
	.align	16, 0x90
.LBB59_6:                               # %if.else.us
                                        #   in Loop: Header=BB59_5 Depth=1
	xorq	%rax, %rax
	testq	%r10, %rcx
	je	.LBB59_12
	jmp	.LBB59_35
	.align	16, 0x90
.LBB59_8:                               # %if.then2.us
                                        #   in Loop: Header=BB59_5 Depth=1
	cmpq	%r9, %rax
	jne	.LBB59_12
# BB#9:                                 # %sw.bb3.us
                                        #   in Loop: Header=BB59_5 Depth=1
	xorq	%rax, %rax
	testq	%r10, %r11
	je	.LBB59_35
# BB#10:                                # %if.end6.us
                                        #   in Loop: Header=BB59_5 Depth=1
	decq	%r11
	.align	16, 0x90
.LBB59_12:                              # %if.end17.us
                                        #   in Loop: Header=BB59_5 Depth=1
	cmpq	%rsi, %rdi
	jne	.LBB59_14
# BB#13:                                # %if.end17.us
                                        #   in Loop: Header=BB59_5 Depth=1
	movq	$1, %rcx
.LBB59_14:                              # %if.end17.us
                                        #   in Loop: Header=BB59_5 Depth=1
	testq	%r10, %rcx
	je	.LBB59_16
# BB#15:                                # %if.end17.us
                                        #   in Loop: Header=BB59_5 Depth=1
	testq	%r10, %r11
	je	.LBB59_33
.LBB59_16:                              # %if.end22.us
                                        #   in Loop: Header=BB59_5 Depth=1
	movq	24(%rdi), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	jne	.LBB59_5
	jmp	.LBB59_35
.LBB59_17:
	movabsq	$4294967211, %r9        # imm = 0xFFFFFFAB
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movabsq	$4294967210, %r10       # imm = 0xFFFFFFAA
	xorq	%r11, %r11
	.align	16, 0x90
.LBB59_18:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %eax
	cmpq	$37, %rax
	jne	.LBB59_24
# BB#19:                                # %if.then2
                                        #   in Loop: Header=BB59_18 Depth=1
	movl	40(%rdi), %eax
	cmpq	%r9, %rax
	jne	.LBB59_20
# BB#22:                                # %sw.bb3
                                        #   in Loop: Header=BB59_18 Depth=1
	xorq	%rax, %rax
	testq	%r8, %r11
	je	.LBB59_35
# BB#23:                                # %if.end6
                                        #   in Loop: Header=BB59_18 Depth=1
	decq	%r11
	jmp	.LBB59_25
	.align	16, 0x90
.LBB59_24:                              # %if.else
                                        #   in Loop: Header=BB59_18 Depth=1
	xorq	%rax, %rax
	testq	%r8, %rcx
	je	.LBB59_25
	jmp	.LBB59_35
	.align	16, 0x90
.LBB59_20:                              # %if.then2
                                        #   in Loop: Header=BB59_18 Depth=1
	cmpq	%r10, %rax
	jne	.LBB59_25
# BB#21:                                # %sw.bb
                                        #   in Loop: Header=BB59_18 Depth=1
	incq	%r11
	.align	16, 0x90
.LBB59_25:                              # %if.end10
                                        #   in Loop: Header=BB59_18 Depth=1
	cmpq	%rsi, %rdi
	jne	.LBB59_27
# BB#26:                                # %if.end10
                                        #   in Loop: Header=BB59_18 Depth=1
	movq	$1, %rcx
.LBB59_27:                              # %if.end10
                                        #   in Loop: Header=BB59_18 Depth=1
	je	.LBB59_28
# BB#31:                                # %if.end17
                                        #   in Loop: Header=BB59_18 Depth=1
	testq	%r8, %rcx
	je	.LBB59_34
# BB#32:                                # %if.end17
                                        #   in Loop: Header=BB59_18 Depth=1
	testq	%r8, %r11
	je	.LBB59_33
.LBB59_34:                              # %if.end22
                                        #   in Loop: Header=BB59_18 Depth=1
	movq	24(%rdi), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	jne	.LBB59_18
.LBB59_35:                              # %return
	retq
.LBB59_33:                              # %if.then21
	movq	%rdi, (%rdx)
	movq	$1, %rax
	retq
.LBB59_28:                              # %if.then14
	testq	%r8, %r11
	jne	.LBB59_29
# BB#30:                                # %if.then14
	movabsq	$1, %rax
	retq
.LBB59_29:
	movabsq	$0, %rax
	retq
.Ltmp747:
	.size	insns_safe_to_move_p, .Ltmp747-insns_safe_to_move_p
	.cfi_endproc

	.globl	loc_mentioned_in_p
	.align	16, 0x90
	.type	loc_mentioned_in_p,@function
loc_mentioned_in_p:                     # @loc_mentioned_in_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp754:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp755:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp756:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp757:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp758:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp759:
	.cfi_def_cfa_offset 80
.Ltmp760:
	.cfi_offset %rbx, -48
.Ltmp761:
	.cfi_offset %r12, -40
.Ltmp762:
	.cfi_offset %r13, -32
.Ltmp763:
	.cfi_offset %r14, -24
.Ltmp764:
	.cfi_offset %r15, -16
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movzbl	(%rsi), %eax
	movzbl	rtx_length(%rax), %r12d
	movq	%rdi, %rbx
	testq	%r12, %r12
	je	.LBB60_1
# BB#2:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB60_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB60_11 Depth 2
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r12,8), %rax
	cmpq	%rbx, %rax
	je	.LBB60_4
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	decq	%r12
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	(%rcx,%r12), %ecx
	cmpq	$69, %rcx
	jne	.LBB60_6
# BB#10:                                # %if.then25
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	(%rax), %r14
	movslq	(%r14), %r15
	.align	16, 0x90
.LBB60_11:                              # %for.cond30
                                        #   Parent Loop BB60_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB60_7
# BB#12:                                # %for.body33
                                        #   in Loop: Header=BB60_11 Depth=2
	movq	(%r14,%r15,8), %rsi
	decq	%r15
	movq	%rbx, %rdi
	callq	loc_mentioned_in_p
	testq	%r13, %rax
	je	.LBB60_11
	jmp	.LBB60_13
	.align	16, 0x90
.LBB60_6:                               # %if.end
                                        #   in Loop: Header=BB60_3 Depth=1
	cmpq	$101, %rcx
	jne	.LBB60_7
# BB#9:                                 # %if.then13
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	(%rax), %rsi
	movq	%rbx, %rdi
	callq	loc_mentioned_in_p
	testq	%r13, %rax
	jne	.LBB60_13
.LBB60_7:                               # %for.cond.backedge
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB60_3
# BB#8:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB60_13
.LBB60_1:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB60_13:                              # %return
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB60_4:
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB60_13
.Ltmp765:
	.size	loc_mentioned_in_p, .Ltmp765-loc_mentioned_in_p
	.cfi_endproc

	.globl	subreg_lsb
	.align	16, 0x90
	.type	subreg_lsb,@function
subreg_lsb:                             # @subreg_lsb
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdi), %rax
	movzbl	2(%rdi), %ecx
	movzbl	2(%rax), %eax
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	movzbl	mode_bitsize(%rax,%rax), %edx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	ja	.LBB61_2
# BB#1:                                 # %if.end
	movl	16(%rdi), %r8d
	movl	target_flags(%rip), %edi
	andq	$33554432, %rdi         # imm = 0x2000000
	movq	$23, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	leaq	4(%rsi), %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	xorq	%rdx, %rdx
	movq	%r8, %rax
	divq	%rcx
	movq	$20, %rcx
	shrq	%cl, %rdi
	addq	$32, %rdi
	imulq	%rax, %rdi
	orq	$3, %rsi
	andq	%r8, %rsi
	leaq	(%rdi,%rsi,8), %rax
.LBB61_2:                               # %return
	retq
.Ltmp766:
	.size	subreg_lsb, .Ltmp766-subreg_lsb
	.cfi_endproc

	.globl	subreg_regno_offset
	.align	16, 0x90
	.type	subreg_regno_offset,@function
subreg_regno_offset:                    # @subreg_regno_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp769:
	.cfi_def_cfa_offset 16
.Ltmp770:
	.cfi_offset %rbx, -16
	movq	%rcx, %r9
	movq	%rdx, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rdi, %rax
	andq	%rbx, %rax
	cmpq	$53, %rax
	jae	.LBB62_1
# BB#3:                                 # %if.end
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	je	.LBB62_7
# BB#4:                                 # %if.end
	leaq	-21(%rdi), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB62_7
# BB#5:                                 # %if.end
	leaq	-45(%rdi), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB62_7
# BB#6:                                 # %if.end
	addq	$-29, %rdi
	andq	%rbx, %rdi
	cmpq	$7, %rdi
	jbe	.LBB62_7
# BB#15:                                # %cond.false
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$24, %rax
	jne	.LBB62_16
# BB#21:                                # %cond.true23
	movl	target_flags(%rip), %r10d
	movq	$24, %rcx
	movq	%r10, %r11
	shrq	%cl, %r11
	andq	$2, %r11
	xorq	$6, %r11
	jmp	.LBB62_22
.LBB62_7:                               # %cond.true
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r10
	movq	$2, %r11
	je	.LBB62_12
# BB#8:                                 # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB62_9
# BB#10:                                # %lor.rhs
	movabsq	$1, %r11
	jmp	.LBB62_11
.LBB62_16:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB62_18
# BB#17:                                # %cond.end39.thread198
	movl	target_flags(%rip), %r10d
	movq	$25, %rcx
	movq	%r10, %r11
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	jmp	.LBB62_22
.LBB62_9:
	movabsq	$0, %r11
.LBB62_11:                              # %lor.rhs
	incq	%r11
.LBB62_12:                              # %cond.true64
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	je	.LBB62_27
# BB#13:                                # %lor.rhs69
	cmpq	$6, %rax
	jne	.LBB62_14
# BB#19:                                # %lor.rhs69
	movabsq	$1, %r10
	incq	%r10
	jmp	.LBB62_27
.LBB62_18:                              # %cond.false27
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	target_flags(%rip), %r10d
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	movq	%r10, %rdi
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %r11
.LBB62_22:                              # %cond.false76
	movq	%r9, %rax
	andq	%rbx, %rax
	cmpq	$24, %rax
	jne	.LBB62_23
# BB#25:                                # %cond.true86
	movq	$24, %rcx
	shrq	%cl, %r10
	andq	$2, %r10
	xorq	$6, %r10
	jmp	.LBB62_27
.LBB62_23:                              # %cond.false76
	cmpq	$18, %rax
	jne	.LBB62_26
# BB#24:                                # %cond.true79
	movq	$25, %rcx
	shrq	%cl, %r10
	andq	$1, %r10
	xorq	$3, %r10
	jmp	.LBB62_27
.LBB62_14:
	movabsq	$0, %r10
	incq	%r10
	jmp	.LBB62_27
.LBB62_26:                              # %cond.false90
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r10
	andq	$4, %r10
	leaq	3(%rax,%r10), %rax
	addq	$4, %r10
	cqto
	idivq	%r10
	movq	%rax, %r10
.LBB62_27:                              # %cond.end107
	xorq	%rax, %rax
	testq	%rbx, %r8
	je	.LBB62_32
# BB#28:                                # %cond.end107
	movq	%r10, %rcx
	andq	%rbx, %rcx
	movq	%r11, %rdx
	andq	%rbx, %rdx
	cmpq	%rcx, %rdx
	je	.LBB62_32
# BB#29:                                # %if.end115
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movzbl	mode_size(%r9), %ecx
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %r9
	testq	%r9, %r9
	je	.LBB62_30
# BB#31:                                # %if.end126
	andq	$255, %r9
	andq	%rbx, %r8
	xorq	%rdx, %rdx
	movq	%r8, %rax
	divq	%rcx
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r11, %rax
	cqto
	idivq	%rsi
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	movq	%r9, %rax
	idivq	%rsi
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rdi, %rax
	cqto
	idivq	%rsi
	imulq	%r10, %rax
.LBB62_32:                              # %return
	popq	%rbx
	retq
.LBB62_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_regno_offset, %rcx
	movq	$3031, %rsi             # imm = 0xBD7
	jmp	.LBB62_2
.LBB62_30:                              # %if.then125
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subreg_regno_offset, %rcx
	movq	$3041, %rsi             # imm = 0xBE1
.LBB62_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp771:
	.size	subreg_regno_offset, .Ltmp771-subreg_regno_offset
	.cfi_endproc

	.globl	find_first_parameter_load
	.align	16, 0x90
	.type	find_first_parameter_load,@function
find_first_parameter_load:              # @find_first_parameter_load
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp778:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp779:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp780:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp781:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp782:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp783:
	.cfi_def_cfa_offset 64
.Ltmp784:
	.cfi_offset %rbx, -48
.Ltmp785:
	.cfi_offset %r12, -40
.Ltmp786:
	.cfi_offset %r13, -32
.Ltmp787:
	.cfi_offset %r14, -24
.Ltmp788:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	$0, 8(%rsp)
	xorq	%rax, %rax
	movl	%eax, (%rsp)
	movq	64(%r13), %rbx
	movq	%rsi, %r14
	testq	%rbx, %rbx
	je	.LBB63_20
	.align	16, 0x90
.LBB63_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB63_15
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB63_1 Depth=1
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB63_15
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB63_1 Depth=1
	movl	8(%rax), %edi
	cmpq	$53, %rdi
	jae	.LBB63_4
# BB#13:                                # %if.end
                                        #   in Loop: Header=BB63_1 Depth=1
	callq	ix86_function_arg_regno_p
	testq	%rax, %rax
	je	.LBB63_15
# BB#14:                                # %if.end33
                                        #   in Loop: Header=BB63_1 Depth=1
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movl	8(%rax), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, 8(%rsp)
	movl	(%rsp), %eax
	incq	%rax
	movl	%eax, (%rsp)
	.align	16, 0x90
.LBB63_15:                              # %for.inc
                                        #   in Loop: Header=BB63_1 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB63_1
# BB#6:                                 # %while.cond.preheader
	movl	(%rsp), %eax
	testq	%rax, %rax
	je	.LBB63_20
# BB#7:                                 # %while.cond.preheader
	cmpq	%r14, %r13
	je	.LBB63_20
# BB#8:
	movabsq	$parms_set, %r15
	leaq	(%rsp), %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB63_9:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r13), %r13
	movzbl	(%r13), %ecx
	cmpq	$34, %rcx
	je	.LBB63_20
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB63_9 Depth=1
	cmpq	$36, %rcx
	je	.LBB63_11
# BB#16:                                # %if.end67
                                        #   in Loop: Header=BB63_9 Depth=1
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$105, %rcx
	jne	.LBB63_18
# BB#17:                                # %if.then73
                                        #   in Loop: Header=BB63_9 Depth=1
	movq	32(%r13), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	note_stores
	movl	(%rsp), %eax
.LBB63_18:                              # %while.cond.backedge
                                        #   in Loop: Header=BB63_9 Depth=1
	testq	%rbx, %rax
	je	.LBB63_20
# BB#19:                                # %while.cond.backedge
                                        #   in Loop: Header=BB63_9 Depth=1
	cmpq	%r14, %r13
	jne	.LBB63_9
	jmp	.LBB63_20
.LBB63_11:                              # %if.then63
	cmpq	%r14, %r13
	movq	%r14, %r13
	jne	.LBB63_12
.LBB63_20:                              # %while.end
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB63_4:                               # %if.then22
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_first_parameter_load, %rcx
	movq	$3104, %rsi             # imm = 0xC20
	jmp	.LBB63_5
.LBB63_12:                              # %if.then65
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_first_parameter_load, %rcx
	movq	$3133, %rsi             # imm = 0xC3D
.LBB63_5:                               # %if.then22
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp789:
	.size	find_first_parameter_load, .Ltmp789-find_first_parameter_load
	.cfi_endproc

	.align	16, 0x90
	.type	parms_set,@function
parms_set:                              # @parms_set
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB64_4
# BB#1:                                 # %land.lhs.true
	movl	8(%rdi), %ecx
	cmpq	$52, %rcx
	ja	.LBB64_4
# BB#2:                                 # %land.lhs.true2
	movq	8(%rdx), %rax
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %rax
	je	.LBB64_4
# BB#3:                                 # %if.then
	notq	%rsi
	andq	%rsi, %rax
	movq	%rax, 8(%rdx)
	movl	(%rdx), %eax
	decq	%rax
	movl	%eax, (%rdx)
.LBB64_4:                               # %if.end
	retq
.Ltmp790:
	.size	parms_set, .Ltmp790-parms_set
	.cfi_endproc

	.type	target_flags,@object    # @target_flags
	.comm	target_flags,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"rtlanal.c"
	.size	.L.str, 10

	.type	.L__FUNCTION__.insn_dependent_p,@object # @__FUNCTION__.insn_dependent_p
.L__FUNCTION__.insn_dependent_p:
	.asciz	"insn_dependent_p"
	.size	.L__FUNCTION__.insn_dependent_p, 17

	.type	.L__FUNCTION__.reg_overlap_mentioned_p,@object # @__FUNCTION__.reg_overlap_mentioned_p
.L__FUNCTION__.reg_overlap_mentioned_p:
	.asciz	"reg_overlap_mentioned_p"
	.size	.L__FUNCTION__.reg_overlap_mentioned_p, 24

	.type	.L__FUNCTION__.dead_or_set_p,@object # @__FUNCTION__.dead_or_set_p
.L__FUNCTION__.dead_or_set_p:
	.asciz	"dead_or_set_p"
	.size	.L__FUNCTION__.dead_or_set_p, 14

	.type	.L__FUNCTION__.find_reg_fusage,@object # @__FUNCTION__.find_reg_fusage
.L__FUNCTION__.find_reg_fusage:
	.asciz	"find_reg_fusage"
	.size	.L__FUNCTION__.find_reg_fusage, 16

	.type	.L__FUNCTION__.remove_note,@object # @__FUNCTION__.remove_note
.L__FUNCTION__.remove_note:
	.asciz	"remove_note"
	.size	.L__FUNCTION__.remove_note, 12

	.type	.L__FUNCTION__.replace_rtx,@object # @__FUNCTION__.replace_rtx
.L__FUNCTION__.replace_rtx:
	.asciz	"replace_rtx"
	.size	.L__FUNCTION__.replace_rtx, 12

	.type	.L__FUNCTION__.subreg_regno_offset,@object # @__FUNCTION__.subreg_regno_offset
.L__FUNCTION__.subreg_regno_offset:
	.asciz	"subreg_regno_offset"
	.size	.L__FUNCTION__.subreg_regno_offset, 20

	.type	.L__FUNCTION__.find_first_parameter_load,@object # @__FUNCTION__.find_first_parameter_load
.L__FUNCTION__.find_first_parameter_load:
	.asciz	"find_first_parameter_load"
	.size	.L__FUNCTION__.find_first_parameter_load, 26


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
