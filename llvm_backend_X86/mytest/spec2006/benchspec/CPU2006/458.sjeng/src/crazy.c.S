	.file	"crazy.c.bc"
	.text
	.globl	ProcessHoldings
	.align	16, 0x90
	.type	ProcessHoldings,@function
ProcessHoldings:                        # @ProcessHoldings
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
	subq	$64, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 112
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
	movq	%rdi, %rbx
	movq	$0, holding+120(%rip)
	movq	$0, holding+112(%rip)
	movq	$0, holding+104(%rip)
	movq	$0, holding+96(%rip)
	movq	$0, holding+88(%rip)
	movq	$0, holding+80(%rip)
	movq	$0, holding+72(%rip)
	movq	$0, holding+64(%rip)
	movq	$0, holding+56(%rip)
	movq	$0, holding+48(%rip)
	movq	$0, holding+40(%rip)
	movq	$0, holding+32(%rip)
	movq	$0, holding+24(%rip)
	movq	$0, holding+16(%rip)
	movq	$0, holding+8(%rip)
	movq	$0, holding(%rip)
	movabsq	$3237998080, %rax       # imm = 0xC0FFEE00
	movl	%eax, hold_hash(%rip)
	xorq	%r14, %r14
	movl	%r14d, white_hand_eval(%rip)
	movl	%r14d, black_hand_eval(%rip)
	callq	reset_ecache
	movq	$0, num_holding(%rip)
	movabsq	$4294967296, %rdx       # imm = 0x100000000
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	xorq	%r10, %r10
.LBB0_1:                                # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_2 Depth 2
                                        #     Child Loop BB0_7 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movzbl	(%rbx,%r10), %ecx
	leaq	1(%r10), %rax
	imulq	%rdx, %r10
	addq	%rdx, %r10
	movq	%r10, %rdi
	movq	%rax, %r10
	.align	16, 0x90
.LBB0_2:                                # %while.cond
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	cmpq	$91, %rcx
	je	.LBB0_3
# BB#49:                                # %while.body
                                        #   in Loop: Header=BB0_2 Depth=2
	movzbl	(%rbx,%r10), %ecx
	incq	%r10
	addq	%rdx, %rdi
	testq	%rcx, %rcx
	jne	.LBB0_2
	jmp	.LBB0_48
	.align	16, 0x90
.LBB0_3:                                # %while.cond8.preheader
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	(%rbx,%rdi), %r9d
	cmpq	$93, %r9
	je	.LBB0_23
# BB#4:                                 # %while.body14.lr.ph
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	%r11, %r14
	movabsq	$0, %rax
	je	.LBB0_6
# BB#5:                                 # %while.body14.lr.ph
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$1, %rax
.LBB0_6:                                # %while.body14.lr.ph
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$6, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	leaq	holding+4(%rsi,%rax,4), %r12
	leaq	holding+36(%rsi,%rax,4), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	leaq	holding+28(%rsi,%rax,4), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	leaq	holding+44(%rsi,%rax,4), %r15
	leaq	holding+12(%rsi,%rax,4), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	leaq	1(%rax), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	leaq	9(%rax), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	leaq	7(%rax), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	leaq	11(%rax), %r8
	leaq	3(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r10
.LBB0_7:                                # %while.body14
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r10, %r13
	leaq	1(%r13), %r10
	movq	$56, %rcx
	shlq	%cl, %r9
	movq	$56, %rcx
	sarq	%cl, %r9
	cmpq	$77, %r9
	jg	.LBB0_9
# BB#8:                                 # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	andq	%r11, %r9
	cmpq	$66, %r9
	je	.LBB0_14
	jmp	.LBB0_48
	.align	16, 0x90
.LBB0_9:                                # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	cmpq	$97, %r9
	jg	.LBB0_12
# BB#10:                                # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	addq	$-78, %r9
	andq	%r11, %r9
	cmpq	$4, %r9
	ja	.LBB0_48
# BB#11:                                # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	jmpq	*.LJTI0_1(,%r9,8)
	.align	16, 0x90
.LBB0_12:                               # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	cmpq	$109, %r9
	jg	.LBB0_15
# BB#13:                                # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	andq	%r11, %r9
	cmpq	$98, %r9
	jne	.LBB0_48
	.align	16, 0x90
.LBB0_14:                               # %sw.bb97
                                        #   in Loop: Header=BB0_7 Depth=2
	movl	(%r15), %eax
	incq	%rax
	movl	%eax, (%r15)
	movl	num_holding(,%r14,4), %ecx
	incq	%rcx
	movl	%ecx, num_holding(,%r14,4)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r8,%r8,8), %rsi
	jmp	.LBB0_22
.LBB0_15:                               # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	addq	$-110, %r9
	andq	%r11, %r9
	cmpq	$4, %r9
	ja	.LBB0_48
# BB#16:                                # %while.body14
                                        #   in Loop: Header=BB0_7 Depth=2
	jmpq	*.LJTI0_0(,%r9,8)
.LBB0_20:                               # %sw.bb124
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %eax
	incq	%rax
	movl	%eax, (%rcx)
	movl	num_holding(,%r14,4), %ecx
	incq	%rcx
	movl	%ecx, num_holding(,%r14,4)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	jmp	.LBB0_21
.LBB0_17:                               # %sw.bb
                                        #   in Loop: Header=BB0_7 Depth=2
	movl	(%r12), %eax
	incq	%rax
	movl	%eax, (%r12)
	movl	num_holding(,%r14,4), %ecx
	incq	%rcx
	movl	%ecx, num_holding(,%r14,4)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB0_21
.LBB0_18:                               # %sw.bb43
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	56(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %eax
	incq	%rax
	movl	%eax, (%rcx)
	movl	num_holding(,%r14,4), %ecx
	incq	%rcx
	movl	%ecx, num_holding(,%r14,4)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB0_21
.LBB0_19:                               # %sw.bb70
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	48(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %eax
	incq	%rax
	movl	%eax, (%rcx)
	movl	num_holding(,%r14,4), %ecx
	incq	%rcx
	movl	%ecx, num_holding(,%r14,4)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
.LBB0_21:                               # %while.cond8.backedge
                                        #   in Loop: Header=BB0_7 Depth=2
	leaq	(%rcx,%rcx,8), %rsi
.LBB0_22:                               # %while.cond8.backedge
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	$6, %rcx
	shlq	%cl, %rsi
	movl	zobrist(%rsi,%rax,4), %eax
	movl	hold_hash(%rip), %ecx
	xorq	%rax, %rcx
	movl	%ecx, hold_hash(%rip)
	movzbl	1(%rbx,%r13), %r9d
	cmpq	$93, %r9
	jne	.LBB0_7
	.align	16, 0x90
.LBB0_23:                               # %for.inc
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_1
# BB#24:                                # %for.end
	movl	Variant(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_48
# BB#25:                                # %for.end
	movl	userealholdings(%rip), %eax
	testq	%r11, %rax
	jne	.LBB0_48
# BB#26:                                # %if.then155
	movabsq	$realholdings, %rdi
	movq	$200, %rdx
	movq	%rbx, %rsi
	callq	strncpy
	movl	comp_color(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB0_37
# BB#27:                                # %if.then158
	movl	holding+72(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_29
# BB#28:                                # %if.then161
	movq	$1, %rax
	movl	num_holding+4(%rip), %ecx
	movl	zobrist+1156(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+72(%rip)
	incq	%rcx
	movl	%ecx, num_holding+4(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_29:                               # %if.end167
	movl	holding+112(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_31
# BB#30:                                # %if.then170
	movq	$1, %rax
	movl	num_holding+4(%rip), %ecx
	movl	zobrist+1156(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+112(%rip)
	incq	%rcx
	movl	%ecx, num_holding+4(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_31:                               # %if.end176
	movl	holding+80(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_33
# BB#32:                                # %if.then179
	movq	$1, %rax
	movl	num_holding+4(%rip), %ecx
	movl	zobrist+2308(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+80(%rip)
	incq	%rcx
	movl	%ecx, num_holding+4(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_33:                               # %if.end185
	movl	holding+96(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_35
# BB#34:                                # %if.then188
	movq	$1, %rax
	movl	num_holding+4(%rip), %ecx
	movl	zobrist+2308(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+96(%rip)
	incq	%rcx
	movl	%ecx, num_holding+4(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_35:                               # %if.end194
	movl	holding+104(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_48
# BB#36:                                # %if.then197
	movq	$1, %rax
	movl	num_holding+4(%rip), %ecx
	movl	zobrist+2308(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+104(%rip)
	incq	%rcx
	movl	%ecx, num_holding+4(%rip)
	jmp	.LBB0_47
.LBB0_37:                               # %if.else
	movl	holding+36(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_39
# BB#38:                                # %if.then206
	movq	$1, %rax
	movl	num_holding(%rip), %ecx
	movl	zobrist+5188(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+36(%rip)
	incq	%rcx
	movl	%ecx, num_holding(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_39:                               # %if.end212
	movl	holding+28(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_41
# BB#40:                                # %if.then215
	movq	$1, %rax
	movl	num_holding(%rip), %ecx
	movl	zobrist+5188(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+28(%rip)
	incq	%rcx
	movl	%ecx, num_holding(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_41:                               # %if.end221
	movl	holding+44(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_43
# BB#42:                                # %if.then224
	movq	$1, %rax
	movl	num_holding(%rip), %ecx
	movl	zobrist+5188(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+44(%rip)
	incq	%rcx
	movl	%ecx, num_holding(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_43:                               # %if.end230
	movl	holding+12(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_45
# BB#44:                                # %if.then233
	movq	$1, %rax
	movl	num_holding(%rip), %ecx
	movl	zobrist+5188(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+12(%rip)
	incq	%rcx
	movl	%ecx, num_holding(%rip)
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_45:                               # %if.end239
	movl	holding+4(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_48
# BB#46:                                # %if.then242
	movq	$1, %rax
	movl	num_holding(%rip), %ecx
	movl	zobrist+5188(%rip), %edx
	movl	hold_hash(%rip), %esi
	movl	%eax, holding+4(%rip)
	incq	%rcx
	movl	%ecx, num_holding(%rip)
.LBB0_47:                               # %if.end250
	xorq	%rdx, %rsi
	movl	%esi, hold_hash(%rip)
.LBB0_48:                               # %if.end250
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	ProcessHoldings, .Ltmp17-ProcessHoldings
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_20
	.quad	.LBB0_48
	.quad	.LBB0_17
	.quad	.LBB0_18
	.quad	.LBB0_19
.LJTI0_1:
	.quad	.LBB0_20
	.quad	.LBB0_48
	.quad	.LBB0_17
	.quad	.LBB0_18
	.quad	.LBB0_19

	.text
	.globl	text_to_piece
	.align	16, 0x90
	.type	text_to_piece,@function
text_to_piece:                          # @text_to_piece
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	$77, %rdi
	movq	$13, %rax
	jg	.LBB1_2
# BB#1:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$66, %rdi
	je	.LBB1_7
	jmp	.LBB1_29
.LBB1_2:                                # %entry
	cmpq	$97, %rdi
	jg	.LBB1_5
# BB#3:                                 # %entry
	addq	$-78, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$4, %rcx
	ja	.LBB1_29
# BB#4:                                 # %entry
	jmpq	*.LJTI1_1(,%rcx,8)
.LBB1_5:                                # %entry
	cmpq	$109, %rdi
	jg	.LBB1_9
# BB#6:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$98, %rdi
	jne	.LBB1_29
.LBB1_7:                                # %sw.bb2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	jne	.LBB1_8
# BB#15:                                # %sw.bb2
	movabsq	$1, %rax
	jmp	.LBB1_16
.LBB1_8:
	movabsq	$0, %rax
.LBB1_16:                               # %sw.bb2
	xorq	$1, %rax
	addq	$11, %rax
	retq
.LBB1_9:                                # %entry
	addq	$-110, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$4, %rcx
	ja	.LBB1_29
# BB#10:                                # %entry
	jmpq	*.LJTI1_0(,%rcx,8)
.LBB1_17:                               # %sw.bb6
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	jne	.LBB1_18
# BB#19:                                # %sw.bb6
	movabsq	$1, %rax
	jmp	.LBB1_20
.LBB1_11:                               # %sw.bb
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	jne	.LBB1_12
# BB#13:                                # %sw.bb
	movabsq	$1, %rax
	jmp	.LBB1_14
.LBB1_25:                               # %sw.bb14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	jne	.LBB1_26
# BB#27:                                # %sw.bb14
	movabsq	$1, %rax
	jmp	.LBB1_28
.LBB1_21:                               # %sw.bb10
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	jne	.LBB1_22
# BB#23:                                # %sw.bb10
	movabsq	$1, %rax
	jmp	.LBB1_24
.LBB1_18:
	movabsq	$0, %rax
.LBB1_20:                               # %sw.bb6
	xorq	$1, %rax
	addq	$3, %rax
	retq
.LBB1_12:
	movabsq	$0, %rax
.LBB1_14:                               # %sw.bb
	xorq	$1, %rax
	incq	%rax
	retq
.LBB1_26:
	movabsq	$0, %rax
.LBB1_28:                               # %sw.bb14
	xorq	$1, %rax
	addq	$9, %rax
.LBB1_29:                               # %return
	retq
.LBB1_22:
	movabsq	$0, %rax
.LBB1_24:                               # %sw.bb10
	xorq	$1, %rax
	addq	$7, %rax
	retq
.Ltmp18:
	.size	text_to_piece, .Ltmp18-text_to_piece
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_17
	.quad	.LBB1_29
	.quad	.LBB1_11
	.quad	.LBB1_25
	.quad	.LBB1_21
.LJTI1_1:
	.quad	.LBB1_17
	.quad	.LBB1_29
	.quad	.LBB1_11
	.quad	.LBB1_25
	.quad	.LBB1_21

	.text
	.globl	SwitchColor
	.align	16, 0x90
	.type	SwitchColor,@function
SwitchColor:                            # @SwitchColor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 16
.Ltmp22:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	leaq	-1(%rbx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$12, %rcx
	jb	.LBB2_2
# BB#1:                                 # %lor.rhs
	movabsq	$.L.str, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.SwitchColor, %rcx
	movq	$200, %rdx
	callq	__assert_fail
.LBB2_2:                                # %lor.end
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	.LSwitchColor.t(,%rbx,4), %eax
	popq	%rbx
	retq
.Ltmp23:
	.size	SwitchColor, .Ltmp23-SwitchColor
	.cfi_endproc

	.globl	SwitchPromoted
	.align	16, 0x90
	.type	SwitchPromoted,@function
SwitchPromoted:                         # @SwitchPromoted
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 16
.Ltmp27:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	leaq	-1(%rbx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$12, %rcx
	jb	.LBB3_2
# BB#1:                                 # %lor.rhs
	movabsq	$.L.str, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.SwitchPromoted, %rcx
	movq	$209, %rdx
	callq	__assert_fail
.LBB3_2:                                # %lor.end
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	.LSwitchPromoted.t(,%rbx,4), %eax
	popq	%rbx
	retq
.Ltmp28:
	.size	SwitchPromoted, .Ltmp28-SwitchPromoted
	.cfi_endproc

	.globl	addHolding
	.align	16, 0x90
	.type	addHolding,@function
addHolding:                             # @addHolding
	.cfi_startproc
# BB#0:                                 # %entry
	movl	Variant(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rax, %rax
	jne	.LBB4_2
# BB#1:                                 # %if.then
	movq	$32, %rcx
	movq	%rsi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	$6, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movl	holding(%rdx,%rdi,4), %eax
	incq	%rax
	movl	%eax, holding(%rdx,%rdi,4)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rdi,%rdi,8), %rdx
	movq	$6, %rcx
	shlq	%cl, %rdx
	movl	num_holding(,%r8,4), %ecx
	movl	zobrist(%rdx,%rax,4), %eax
	movl	hold_hash(%rip), %edx
	incq	%rcx
	movl	%ecx, num_holding(,%r8,4)
	xorq	%rax, %rdx
	movl	%edx, hold_hash(%rip)
.LBB4_2:                                # %if.end
	movl	hand_value(,%rdi,4), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	je	.LBB4_3
# BB#4:                                 # %if.else
	movl	black_hand_eval(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, black_hand_eval(%rip)
	jmp	.LBB4_5
.LBB4_3:                                # %if.then15
	movl	white_hand_eval(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, white_hand_eval(%rip)
.LBB4_5:                                # %if.end21
	movl	material(,%rdi,4), %eax
	movl	Material(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, Material(%rip)
	retq
.Ltmp29:
	.size	addHolding, .Ltmp29-addHolding
	.cfi_endproc

	.globl	removeHolding
	.align	16, 0x90
	.type	removeHolding,@function
removeHolding:                          # @removeHolding
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 48
.Ltmp40:
	.cfi_offset %rbx, -40
.Ltmp41:
	.cfi_offset %r12, -32
.Ltmp42:
	.cfi_offset %r14, -24
.Ltmp43:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movl	Variant(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rax, %rax
	jne	.LBB5_6
# BB#1:                                 # %if.then
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movslq	holding(%rdx,%rbx,4), %rax
	leaq	holding(%rdx,%rbx,4), %r12
	testq	%rax, %rax
	jg	.LBB5_3
# BB#2:                                 # %lor.rhs
	movabsq	$.L.str2, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.removeHolding, %rcx
	movq	$245, %rdx
	callq	__assert_fail
	movl	(%r12), %eax
.LBB5_3:                                # %lor.end
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$20, %rdx
	jl	.LBB5_5
# BB#4:                                 # %lor.rhs9
	movabsq	$.L.str3, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.removeHolding, %rcx
	movq	$246, %rdx
	callq	__assert_fail
	movl	(%r12), %eax
.LBB5_5:                                # %lor.end10
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rbx,%rbx,8), %rsi
	movq	$6, %rcx
	shlq	%cl, %rsi
	movl	zobrist(%rsi,%rdx,4), %ecx
	movl	hold_hash(%rip), %edx
	xorq	%rcx, %rdx
	movl	num_holding(,%r15,4), %ecx
	movl	%edx, hold_hash(%rip)
	decq	%rax
	movl	%eax, (%r12)
	decq	%rcx
	movl	%ecx, num_holding(,%r15,4)
.LBB5_6:                                # %if.end
	movl	hand_value(,%rbx,4), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB5_7
# BB#8:                                 # %if.else
	movl	black_hand_eval(%rip), %ecx
	subq	%rax, %rcx
	movl	%ecx, black_hand_eval(%rip)
	jmp	.LBB5_9
.LBB5_7:                                # %if.then28
	movl	white_hand_eval(%rip), %ecx
	subq	%rax, %rcx
	movl	%ecx, white_hand_eval(%rip)
.LBB5_9:                                # %if.end34
	movl	material(,%rbx,4), %eax
	movl	Material(%rip), %ecx
	subq	%rax, %rcx
	movl	%ecx, Material(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp44:
	.size	removeHolding, .Ltmp44-removeHolding
	.cfi_endproc

	.globl	DropaddHolding
	.align	16, 0x90
	.type	DropaddHolding,@function
DropaddHolding:                         # @DropaddHolding
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$6, %rcx
	movq	%rax, %r8
	shlq	%cl, %r8
	movl	holding(%r8,%rdi,4), %edx
	incq	%rdx
	movl	%edx, holding(%r8,%rdi,4)
	movl	num_holding(,%rax,4), %ecx
	incq	%rcx
	movl	%ecx, num_holding(,%rax,4)
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdi,%rdi,8), %rax
	movq	$6, %rcx
	shlq	%cl, %rax
	movl	zobrist(%rax,%rdx,4), %eax
	movl	hold_hash(%rip), %ecx
	xorq	%rax, %rcx
	movl	hand_value(,%rdi,4), %eax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rsi
	movl	%ecx, hold_hash(%rip)
	je	.LBB6_1
# BB#2:                                 # %if.else
	movl	black_hand_eval(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, black_hand_eval(%rip)
	jmp	.LBB6_3
.LBB6_1:                                # %if.then
	movl	white_hand_eval(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, white_hand_eval(%rip)
.LBB6_3:                                # %if.end
	movl	material(,%rdi,4), %eax
	movl	Material(%rip), %ecx
	addq	%rax, %rcx
	movl	%ecx, Material(%rip)
	retq
.Ltmp45:
	.size	DropaddHolding, .Ltmp45-DropaddHolding
	.cfi_endproc

	.globl	DropremoveHolding
	.align	16, 0x90
	.type	DropremoveHolding,@function
DropremoveHolding:                      # @DropremoveHolding
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp51:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp52:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp53:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp55:
	.cfi_def_cfa_offset 48
.Ltmp56:
	.cfi_offset %rbx, -40
.Ltmp57:
	.cfi_offset %r12, -32
.Ltmp58:
	.cfi_offset %r14, -24
.Ltmp59:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$6, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movslq	holding(%rdx,%rbx,4), %rax
	leaq	holding(%rdx,%rbx,4), %r12
	testq	%rax, %rax
	jg	.LBB7_2
# BB#1:                                 # %lor.rhs
	movabsq	$.L.str2, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.DropremoveHolding, %rcx
	movq	$287, %rdx              # imm = 0x11F
	callq	__assert_fail
	movl	(%r12), %eax
.LBB7_2:                                # %lor.end
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$20, %rdx
	jl	.LBB7_4
# BB#3:                                 # %lor.rhs8
	movabsq	$.L.str3, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.DropremoveHolding, %rcx
	movq	$289, %rdx              # imm = 0x121
	callq	__assert_fail
	movl	(%r12), %eax
.LBB7_4:                                # %lor.end9
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rbx,%rbx,8), %rsi
	movq	$6, %rcx
	shlq	%cl, %rsi
	movl	zobrist(%rsi,%rdx,4), %ecx
	movl	hold_hash(%rip), %edx
	xorq	%rcx, %rdx
	movl	%edx, hold_hash(%rip)
	decq	%rax
	movl	%eax, (%r12)
	movl	num_holding(,%r15,4), %ecx
	decq	%rcx
	movl	hand_value(,%rbx,4), %eax
	movl	%ecx, num_holding(,%r15,4)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB7_5
# BB#6:                                 # %if.else
	movl	black_hand_eval(%rip), %ecx
	subq	%rax, %rcx
	movl	%ecx, black_hand_eval(%rip)
	jmp	.LBB7_7
.LBB7_5:                                # %if.then
	movl	white_hand_eval(%rip), %ecx
	subq	%rax, %rcx
	movl	%ecx, white_hand_eval(%rip)
.LBB7_7:                                # %if.end
	movl	material(,%rbx,4), %eax
	movl	Material(%rip), %ecx
	subq	%rax, %rcx
	movl	%ecx, Material(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp60:
	.size	DropremoveHolding, .Ltmp60-DropremoveHolding
	.cfi_endproc

	.globl	printHolding
	.align	16, 0x90
	.type	printHolding,@function
printHolding:                           # @printHolding
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp62:
	.cfi_def_cfa_offset 16
	movl	holding+36(%rip), %r9d
	movl	holding+12(%rip), %r8d
	movl	holding+44(%rip), %ecx
	movl	holding+28(%rip), %edx
	movl	holding+4(%rip), %esi
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	printf
	movl	holding+104(%rip), %r9d
	movl	holding+80(%rip), %r8d
	movl	holding+112(%rip), %ecx
	movl	holding+96(%rip), %edx
	movl	holding+72(%rip), %esi
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	popq	%r10
	jmp	printf  # TAILCALL
.Ltmp63:
	.size	printHolding, .Ltmp63-printHolding
	.cfi_endproc

	.type	holding,@object         # @holding
	.comm	holding,128,16
	.type	hold_hash,@object       # @hold_hash
	.comm	hold_hash,4,4
	.type	white_hand_eval,@object # @white_hand_eval
	.comm	white_hand_eval,4,4
	.type	black_hand_eval,@object # @black_hand_eval
	.comm	black_hand_eval,4,4
	.type	num_holding,@object     # @num_holding
	.comm	num_holding,8,4
	.type	userealholdings,@object # @userealholdings
	.comm	userealholdings,4,4
	.type	realholdings,@object    # @realholdings
	.comm	realholdings,255,16
	.type	.LSwitchColor.t,@object # @SwitchColor.t
	.section	.rodata,"a",@progbits
	.align	16
.LSwitchColor.t:
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	4                       # 0x4
	.long	3                       # 0x3
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	8                       # 0x8
	.long	7                       # 0x7
	.long	10                      # 0xa
	.long	9                       # 0x9
	.long	12                      # 0xc
	.long	11                      # 0xb
	.size	.LSwitchColor.t, 52

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"piece > frame && piece < npiece"
	.size	.L.str, 32

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"crazy.c"
	.size	.L.str1, 8

	.type	.L__func__.SwitchColor,@object # @__func__.SwitchColor
.L__func__.SwitchColor:
	.asciz	"SwitchColor"
	.size	.L__func__.SwitchColor, 12

	.type	.LSwitchPromoted.t,@object # @SwitchPromoted.t
	.section	.rodata,"a",@progbits
	.align	16
.LSwitchPromoted.t:
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	1                       # 0x1
	.size	.LSwitchPromoted.t, 52

	.type	.L__func__.SwitchPromoted,@object # @__func__.SwitchPromoted
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__func__.SwitchPromoted:
	.asciz	"SwitchPromoted"
	.size	.L__func__.SwitchPromoted, 15

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"holding[who][what] > 0"
	.size	.L.str2, 23

	.type	.L__func__.removeHolding,@object # @__func__.removeHolding
.L__func__.removeHolding:
	.asciz	"removeHolding"
	.size	.L__func__.removeHolding, 14

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"holding[who][what] < 20"
	.size	.L.str3, 24

	.type	.L__func__.DropremoveHolding,@object # @__func__.DropremoveHolding
.L__func__.DropremoveHolding:
	.asciz	"DropremoveHolding"
	.size	.L__func__.DropremoveHolding, 18

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"WP: %d WR: %d WB: %d WN: %d WQ: %d\n"
	.size	.L.str4, 36

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"BP: %d BR: %d BB: %d BN: %d BQ: %d\n"
	.size	.L.str5, 36

	.type	drop_piece,@object      # @drop_piece
	.comm	drop_piece,4,4

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
