	.file	"cselib.c.bc"
	.text
	.globl	references_value_p
	.align	16, 0x90
	.type	references_value_p,@function
references_value_p:                     # @references_value_p
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movzbl	(%rdi), %eax
	movq	%rsi, %rbx
	cmpq	$60, %rax
	jne	.LBB0_3
# BB#1:                                 # %land.lhs.true
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	je	.LBB0_14
# BB#2:                                 # %lor.lhs.false
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	cmpq	$0, 16(%rcx)
	je	.LBB0_14
.LBB0_3:                                # %if.end
	movzbl	rtx_length(%rax), %r12d
	testq	%r12, %r12
	je	.LBB0_4
# BB#5:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB0_6:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_13 Depth 2
	movq	%r12, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %r12
	cmpq	$69, %rcx
	jne	.LBB0_7
# BB#11:                                # %for.cond26.preheader
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %r14
	movslq	(%r14), %r13
	testq	%r13, %r13
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movq	$1, %r15
	jle	.LBB0_9
	.align	16, 0x90
.LBB0_13:                               # %for.body32
                                        #   Parent Loop BB0_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r14,%r15,8), %rdi
	movq	%rbx, %rsi
	callq	references_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_14
# BB#12:                                # %for.cond26
                                        #   in Loop: Header=BB0_13 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	cmpq	%r13, %rax
	jl	.LBB0_13
	jmp	.LBB0_9
	.align	16, 0x90
.LBB0_7:                                # %for.body
                                        #   in Loop: Header=BB0_6 Depth=1
	cmpq	$101, %rcx
	jne	.LBB0_9
# BB#8:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	%rbx, %rsi
	callq	references_value_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jne	.LBB0_14
.LBB0_9:                                # %for.cond.backedge
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_6
# BB#10:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB0_14
.LBB0_4:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB0_14:                               # %return
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	references_value_p, .Ltmp17-references_value_p
	.cfi_endproc

	.globl	rtx_equal_for_cselib_p
	.align	16, 0x90
	.type	rtx_equal_for_cselib_p,@function
rtx_equal_for_cselib_p:                 # @rtx_equal_for_cselib_p
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
	movq	%rdi, %r12
	movl	(%r12), %eax
	movq	%rsi, %r15
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB1_2
# BB#1:                                 # %entry
	cmpq	$66, %rcx
	jne	.LBB1_4
.LBB1_2:                                # %if.then
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cselib_lookup
	testq	%rax, %rax
	je	.LBB1_4
# BB#3:                                 # %if.then6
	movq	8(%rax), %r12
.LBB1_4:                                # %if.end7
	movl	(%r15), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB1_6
# BB#5:                                 # %if.end7
	cmpq	$66, %rax
	jne	.LBB1_8
.LBB1_6:                                # %if.then15
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	cselib_lookup
	testq	%rax, %rax
	je	.LBB1_8
# BB#7:                                 # %if.then22
	movq	8(%rax), %r15
.LBB1_8:                                # %if.end26
	movq	$1, %r14
	cmpq	%r15, %r12
	je	.LBB1_59
# BB#9:                                 # %if.end29
	movl	(%r12), %ecx
	movl	(%r15), %edx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$60, %rax
	jne	.LBB1_20
# BB#10:                                # %land.lhs.true
	movq	8(%r12), %rax
	cmpq	$60, %rsi
	jne	.LBB1_14
# BB#11:                                # %if.then36
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_12
# BB#13:                                # %if.then36
	movabsq	$1, %r14
	jmp	.LBB1_59
.LBB1_20:                               # %if.end67
	cmpq	$60, %rsi
	jne	.LBB1_27
# BB#21:                                # %if.then72
	movq	8(%r15), %rax
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB1_58
# BB#22:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_23:                               # %for.body81
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	je	.LBB1_26
# BB#24:                                # %for.body81
                                        #   in Loop: Header=BB1_23 Depth=1
	cmpq	$66, %rax
	je	.LBB1_26
# BB#25:                                # %if.else94
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	%r12, %rdi
	callq	rtx_equal_for_cselib_p
	testq	%r15, %rax
	jne	.LBB1_59
.LBB1_26:                               # %for.inc100
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_23
	jmp	.LBB1_58
.LBB1_14:                               # %if.then46
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB1_58
# BB#15:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_16:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	je	.LBB1_19
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB1_16 Depth=1
	cmpq	$66, %rax
	je	.LBB1_19
# BB#18:                                # %if.else
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	%r15, %rsi
	callq	rtx_equal_for_cselib_p
	testq	%r12, %rax
	jne	.LBB1_59
.LBB1_19:                               # %for.inc
                                        #   in Loop: Header=BB1_16 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_16
	jmp	.LBB1_58
.LBB1_27:                               # %if.end103
	cmpq	%rsi, %rax
	jne	.LBB1_58
# BB#28:                                # %lor.lhs.false110
	xorq	%rcx, %rdx
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB1_58
# BB#29:                                # %if.end120
	cmpq	$67, %rax
	jne	.LBB1_33
# BB#30:                                # %if.then125
	movq	8(%r12), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_31
# BB#32:                                # %if.then125
	movabsq	$1, %r14
	jmp	.LBB1_59
.LBB1_12:
	movabsq	$0, %r14
	jmp	.LBB1_59
.LBB1_33:                               # %if.end133
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB1_59
# BB#34:                                # %for.body143.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB1_35:                               # %for.body143
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_56 Depth 2
	movq	%rdx, %rbx
	movq	(%rsp), %rax            # 8-byte Reload
	movsbq	-1(%rax,%rbx), %rax
	leaq	-1(%rbx), %rcx
	cmpq	$68, %rax
	jg	.LBB1_38
# BB#36:                                # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$48, %rax
	je	.LBB1_37
	jmp	.LBB1_40
.LBB1_38:                               # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$82, %rax
	jg	.LBB1_41
# BB#39:                                # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$69, %rax
	je	.LBB1_44
	jmp	.LBB1_40
.LBB1_41:                               # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	$100, %rax
	jg	.LBB1_46
# BB#42:                                # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$83, %rax
	je	.LBB1_57
# BB#43:                                # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	$86, %rax
	jne	.LBB1_40
.LBB1_44:                               # %sw.bb170
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	(%r12,%rbx,8), %rax
	movq	(%r15,%rbx,8), %rdx
	movl	(%rax), %esi
	movl	(%rdx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB1_58
# BB#45:                                # %for.cond183.preheader
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %r13
	testq	%rsi, %rsi
	jle	.LBB1_37
	jmp	.LBB1_56
.LBB1_55:                               # %for.cond183.for.body191_crit_edge
                                        #   in Loop: Header=BB1_56 Depth=2
	movq	(%r15,%rbx,8), %rdx
	incq	%r13
.LBB1_56:                               # %for.body191
                                        #   Parent Loop BB1_35 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r13,8), %rdi
	movq	(%rdx,%r13,8), %rsi
	callq	rtx_equal_for_cselib_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_58
# BB#54:                                # %for.cond183
                                        #   in Loop: Header=BB1_56 Depth=2
	movq	(%r12,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB1_55
	jmp	.LBB1_37
.LBB1_46:                               # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	$104, %rax
	jg	.LBB1_49
# BB#47:                                # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$101, %rax
	jne	.LBB1_40
# BB#48:                                # %sw.bb211
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	(%r12,%rbx,8), %rdi
	movq	(%r15,%rbx,8), %rsi
	callq	rtx_equal_for_cselib_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_37
	jmp	.LBB1_58
.LBB1_49:                               # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	addq	$-105, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB1_40
# BB#50:                                # %for.body143
                                        #   in Loop: Header=BB1_35 Depth=1
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_53:                               # %sw.bb158
                                        #   in Loop: Header=BB1_35 Depth=1
	movl	(%r12,%rbx,8), %eax
	movl	(%r15,%rbx,8), %ecx
	jmp	.LBB1_52
.LBB1_57:                               # %sw.bb224
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	(%r12,%rbx,8), %rdi
	movq	(%r15,%rbx,8), %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_37
	jmp	.LBB1_58
.LBB1_51:                               # %sw.bb
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	(%r12,%rbx,8), %rax
	movq	(%r15,%rbx,8), %rcx
.LBB1_52:                               # %sw.bb
                                        #   in Loop: Header=BB1_35 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB1_58
.LBB1_37:                               # %for.cond140.backedge
                                        #   in Loop: Header=BB1_35 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_35
	jmp	.LBB1_59
.LBB1_58:
	xorq	%r14, %r14
.LBB1_59:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_31:
	movabsq	$0, %r14
	jmp	.LBB1_59
.LBB1_40:                               # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.rtx_equal_for_cselib_p, %rdx
	movq	$530, %rsi              # imm = 0x212
	callq	fancy_abort
.Ltmp35:
	.size	rtx_equal_for_cselib_p, .Ltmp35-rtx_equal_for_cselib_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_53
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_53
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_40
	.quad	.LBB1_57
	.quad	.LBB1_37
	.quad	.LBB1_37
	.quad	.LBB1_40
	.quad	.LBB1_51

	.text
	.globl	cselib_lookup
	.align	16, 0x90
	.type	cselib_lookup,@function
cselib_lookup:                          # @cselib_lookup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 64
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movq	%rdi, %r8
	movl	(%r8), %eax
	movq	%rdx, %r13
	movq	%rsi, %r12
	movq	$16, %rcx
	movq	%rax, %r14
	shrq	%cl, %r14
	andq	$255, %r14
	je	.LBB2_2
# BB#1:                                 # %entry
	movq	%r14, %r12
.LBB2_2:                                # %entry
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	je	.LBB2_51
# BB#3:                                 # %entry
	cmpq	$61, %rcx
	jne	.LBB2_4
# BB#6:                                 # %if.then12
	movl	8(%r8), %r15d
	movq	reg_values(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	testq	%rax, %rax
	je	.LBB2_10
# BB#7:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	.align	16, 0x90
.LBB2_9:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rbx
	movq	8(%rbx), %rdx
	movzbl	2(%rdx), %edx
	cmpq	%rdx, %rcx
	je	.LBB2_114
# BB#8:                                 # %for.cond
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_9
.LBB2_10:                               # %for.end
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r13
	je	.LBB2_114
# BB#11:                                # %if.end25
	cmpq	$52, %r15
	ja	.LBB2_28
# BB#12:                                # %if.then27
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r15, %rax
	cmpq	$8, %rax
	je	.LBB2_16
# BB#13:                                # %if.then27
	leaq	-21(%r15), %rax
	andq	%rsi, %rax
	cmpq	$8, %rax
	jb	.LBB2_16
# BB#14:                                # %if.then27
	leaq	-45(%r15), %rax
	andq	%rsi, %rax
	cmpq	$8, %rax
	jb	.LBB2_16
# BB#15:                                # %if.then27
	leaq	-29(%r15), %rax
	andq	%rsi, %rax
	cmpq	$7, %rax
	jbe	.LBB2_16
# BB#21:                                # %cond.false
	movq	%r12, %rax
	andq	%rsi, %rax
	cmpq	$24, %rax
	jne	.LBB2_22
# BB#24:                                # %cond.true53
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_26
.LBB2_51:                               # %if.then115
	xorq	%rbx, %rbx
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB2_114
# BB#52:                                # %if.then115
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r14, %r15
	andq	%rdx, %r15
	cmpq	$51, %r15
	je	.LBB2_114
# BB#53:                                # %lor.lhs.false4.i
	movl	mode_class(,%r15,4), %eax
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	jne	.LBB2_54
# BB#56:                                # %land.lhs.true.i
	movl	flag_float_store(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_114
	jmp	.LBB2_57
.LBB2_4:                                # %entry
	cmpq	$60, %rcx
	jne	.LBB2_93
# BB#5:                                 # %if.then7
	movq	8(%r8), %rbx
	jmp	.LBB2_114
.LBB2_93:                               # %if.end117
	movq	%r8, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r8, %r14
	callq	hash_rtx
	movq	%rax, %r15
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB2_114
# BB#94:                                # %if.end121
	movl	(%r14), %r8d
	movq	hash_table(%rip), %rdi
	movq	%r8, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	movq	%r14, %rdx
	je	.LBB2_97
# BB#95:                                # %if.end121
	cmpq	$55, %rcx
	movq	%rdx, %rsi
	jne	.LBB2_99
# BB#96:                                # %lor.lhs.false.i
	testq	$16711680, %r8          # imm = 0xFF0000
	movq	%rdx, %rsi
	jne	.LBB2_99
.LBB2_97:                               # %if.end.i202
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %r12
	je	.LBB2_115
# BB#98:                                # %if.end9.i
	movq	$58, %rdi
	movq	%r12, %rsi
	movq	%rdx, %r14
	callq	gen_rtx_fmt_e
	movq	%r14, %rdx
	movq	%rax, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB2_99:                               # %wrap_constant.exit
	movq	%rdx, %r14
	testq	%rax, %r13
	je	.LBB2_100
# BB#101:                               # %wrap_constant.exit
	movabsq	$1, %rcx
	jmp	.LBB2_102
.LBB2_54:                               # %lor.lhs.false10.i
	andq	%rdx, %rax
	cmpq	$8, %rax
	jne	.LBB2_57
# BB#55:                                # %lor.lhs.false10.i
	movl	flag_float_store(%rip), %eax
	testq	%rdx, %rax
	jne	.LBB2_114
.LBB2_57:                               # %if.end.i279
	movq	8(%r8), %rdi
	movq	%r8, %r12
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	cselib_lookup
	testq	%rax, %rax
	je	.LBB2_114
# BB#58:                                # %if.end18.i
	movq	24(%rax), %rdi
	testq	%rdi, %rdi
	movq	%r12, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB2_59
	.align	16, 0x90
.LBB2_68:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rbx
	movq	8(%rbx), %rcx
	movzbl	2(%rcx), %ecx
	cmpq	%r15, %rcx
	je	.LBB2_114
# BB#67:                                # %for.cond.i
                                        #   in Loop: Header=BB2_68 Depth=1
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB2_68
.LBB2_59:                               # %for.end.i
	testq	%rsi, %r13
	movabsq	$0, %rbx
	je	.LBB2_114
# BB#60:                                # %if.end29.i
	movq	%rax, %r15
	movl	next_unknown_value(%rip), %edi
	incq	%rdi
	movl	%edi, next_unknown_value(%rip)
	movq	%r14, %rsi
	movq	%rdx, %rbx
	callq	new_cselib_val
	movq	%r15, %rsi
	movq	%rbx, %r9
	movq	%rax, %rbx
	movq	16(%rbx), %rax
	jmp	.LBB2_61
.LBB2_100:
	movabsq	$0, %rcx
.LBB2_102:                              # %wrap_constant.exit
	movq	%r15, %rdx
	callq	htab_find_slot_with_hash
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB2_114
# BB#103:                               # %if.end129
	movq	(%r13), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_114
# BB#104:                               # %if.end132
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	new_cselib_val
	movq	%rax, %rbx
	movq	%rbx, (%r13)
	movq	16(%rbx), %r15
	movq	%r14, %rdi
	callq	cselib_subst_to_values
	movq	empty_elt_loc_lists(%rip), %rcx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB2_106
# BB#105:                               # %if.then.i
	movq	(%rcx), %rax
	movq	%rax, empty_elt_loc_lists(%rip)
	jmp	.LBB2_113
.LBB2_16:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movl	mode_class(,%r12,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_26
# BB#17:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB2_18
# BB#19:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB2_26
.LBB2_64:                               # %for.inc.i.i
                                        #   in Loop: Header=BB2_61 Depth=1
	movq	(%rax), %rax
.LBB2_61:                               # %if.end29.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB2_65
# BB#62:                                # %for.body.i.i
                                        #   in Loop: Header=BB2_61 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$66, %rdx
	jne	.LBB2_64
# BB#63:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB2_61 Depth=1
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB2_64
	jmp	.LBB2_86
.LBB2_65:                               # %for.end.i.i
	movq	empty_elt_lists(%rip), %rax
	movq	24(%rsi), %r13
	testq	%rax, %rax
	je	.LBB2_69
# BB#66:                                # %if.then.i320
	movq	(%rax), %rcx
	movq	%rcx, empty_elt_lists(%rip)
	jmp	.LBB2_76
.LBB2_22:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB2_25
# BB#23:                                # %cond.true48
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_26
.LBB2_106:                              # %if.else.i
	movq	cselib_obstack+32(%rip), %rcx
	movq	cselib_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB2_108
# BB#107:                               # %if.then3.i
	movabsq	$cselib_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	cselib_obstack+24(%rip), %rax
.LBB2_108:                              # %if.end.i
	addq	$24, %rax
	movq	%rax, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_110
# BB#109:                               # %if.then10.i
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB2_110:                              # %if.end11.i
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rax
	notq	%rdx
	andq	%rax, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB2_112
# BB#111:                               # %if.then31.i
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_112:                              # %if.end34.i
	movq	%rdx, cselib_obstack+16(%rip)
.LBB2_113:                              # %new_elt_loc_list.exit
	movq	%r15, (%rcx)
	movq	%r14, 8(%rcx)
	movq	cselib_current_insn(%rip), %rax
	movq	%rax, 16(%rcx)
	movq	%rcx, 16(%rbx)
	jmp	.LBB2_114
.LBB2_25:                               # %cond.false57
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	movl	target_flags(%rip), %edi
	sarq	%cl, %r12
	movzbl	mode_size(%r12), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	jmp	.LBB2_26
.LBB2_18:
	movabsq	$0, %rax
	incq	%rax
.LBB2_26:                               # %cond.end69
	movl	max_value_regs(%rip), %ecx
	andq	%rax, %rsi
	cmpq	%rcx, %rsi
	jbe	.LBB2_28
# BB#27:                                # %if.then73
	movl	%eax, max_value_regs(%rip)
.LBB2_28:                               # %if.end75
	movl	next_unknown_value(%rip), %edi
	incq	%rdi
	movl	%edi, next_unknown_value(%rip)
	movzbl	2(%r8), %esi
	movq	%r8, %r14
	callq	new_cselib_val
	movq	%rax, %rbx
	movq	empty_elt_loc_lists(%rip), %rax
	movq	16(%rbx), %r12
	testq	%rax, %rax
	je	.LBB2_30
# BB#29:                                # %if.then.i239
	movq	(%rax), %rcx
	movq	%rcx, empty_elt_loc_lists(%rip)
	movq	%r14, %r9
	jmp	.LBB2_37
.LBB2_30:                               # %if.else.i244
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB2_32
# BB#31:                                # %if.then3.i246
	movabsq	$cselib_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	cselib_obstack+24(%rip), %rcx
.LBB2_32:                               # %if.end.i249
	addq	$24, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_34
# BB#33:                                # %if.then10.i252
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB2_34:                               # %if.end11.i266
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	movq	%r14, %r9
	jle	.LBB2_36
# BB#35:                                # %if.then31.i267
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_36:                               # %if.end34.i268
	movq	%rdx, cselib_obstack+16(%rip)
.LBB2_37:                               # %new_elt_loc_list.exit273
	movq	%r12, (%rax)
	movq	%r9, 8(%rax)
	movq	cselib_current_insn(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, 16(%rbx)
	movq	reg_values(%rip), %rax
	movq	32(%rax,%r15,8), %r12
	testq	%r12, %r12
	jne	.LBB2_41
# BB#38:                                # %do.body
	movq	used_regs(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	xorq	%r12, %r12
	cmpq	%rsi, %rcx
	jb	.LBB2_40
# BB#39:                                # %if.then90
	addq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r9, %r14
	callq	varray_grow
	movq	%r14, %r9
	movq	reg_values(%rip), %rdx
	movq	%rax, used_regs(%rip)
	movq	8(%rax), %rcx
	movq	32(%rdx,%r15,8), %r12
.LBB2_40:                               # %if.end93
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movl	%r15d, 32(%rax,%rcx,4)
.LBB2_41:                               # %if.end99
	movq	empty_elt_lists(%rip), %rax
	testq	%rax, %rax
	je	.LBB2_43
# BB#42:                                # %if.then.i205
	movq	(%rax), %rcx
	movq	%rcx, empty_elt_lists(%rip)
	jmp	.LBB2_50
.LBB2_43:                               # %if.else.i210
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB2_45
# BB#44:                                # %if.then3.i212
	movabsq	$cselib_obstack, %rdi
	movq	$16, %rsi
	movq	%r9, %r14
	callq	_obstack_newchunk
	movq	%r14, %r9
	movq	cselib_obstack+24(%rip), %rcx
.LBB2_45:                               # %if.end.i215
	addq	$16, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_47
# BB#46:                                # %if.then10.i218
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB2_47:                               # %if.end11.i232
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB2_49
# BB#48:                                # %if.then31.i233
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_49:                               # %if.end34.i234
	movq	%rdx, cselib_obstack+16(%rip)
.LBB2_50:                               # %new_elt_list.exit
	movq	%r12, (%rax)
	movq	%rbx, 8(%rax)
	movq	reg_values(%rip), %rcx
	movq	%rax, 32(%rcx,%r15,8)
	movq	hash_table(%rip), %rdi
	movl	(%rbx), %edx
	movq	$1, %rcx
.LBB2_92:                               # %return
	movq	%r9, %rsi
	callq	htab_find_slot_with_hash
	movq	%rbx, (%rax)
.LBB2_114:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_69:                               # %if.else.i325
	movq	%rsi, %r12
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB2_71
# BB#70:                                # %if.then3.i327
	movabsq	$cselib_obstack, %rdi
	movq	$16, %rsi
	movq	%r9, %r15
	callq	_obstack_newchunk
	movq	%r15, %r9
	movq	cselib_obstack+24(%rip), %rcx
.LBB2_71:                               # %if.end.i330
	addq	$16, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_73
# BB#72:                                # %if.then10.i333
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB2_73:                               # %if.end11.i347
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB2_75
# BB#74:                                # %if.then31.i348
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_75:                               # %if.end34.i349
	movq	%rdx, cselib_obstack+16(%rip)
	movq	%r12, %rsi
.LBB2_76:                               # %new_elt_list.exit353
	movq	%r13, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rax, 24(%rsi)
	movq	16(%rbx), %r13
	movq	8(%rsi), %rsi
	movq	%r9, %rdi
	movq	%r9, %r15
	callq	replace_equiv_address_nv
	movq	empty_elt_loc_lists(%rip), %rcx
	movq	%rax, %r12
	testq	%rcx, %rcx
	je	.LBB2_78
# BB#77:                                # %if.then.i283
	movq	(%rcx), %rax
	movq	%rax, empty_elt_loc_lists(%rip)
	movq	%r15, %r9
	jmp	.LBB2_85
.LBB2_78:                               # %if.else.i288
	movq	cselib_obstack+32(%rip), %rcx
	movq	cselib_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB2_80
# BB#79:                                # %if.then3.i290
	movabsq	$cselib_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	cselib_obstack+24(%rip), %rax
.LBB2_80:                               # %if.end.i293
	addq	$24, %rax
	movq	%rax, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rcx
	cmpq	%rcx, %rax
	movq	%r15, %r9
	jne	.LBB2_82
# BB#81:                                # %if.then10.i296
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB2_82:                               # %if.end11.i310
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rax
	notq	%rdx
	andq	%rax, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB2_84
# BB#83:                                # %if.then31.i311
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_84:                               # %if.end34.i312
	movq	%rdx, cselib_obstack+16(%rip)
.LBB2_85:                               # %new_elt_loc_list.exit317
	movq	%r13, (%rcx)
	movq	%r12, 8(%rcx)
	movq	cselib_current_insn(%rip), %rax
	movq	%rax, 16(%rcx)
	movq	%rcx, 16(%rbx)
.LBB2_86:                               # %add_mem_for_addr.exit.i
	movl	(%r9), %eax
	movq	hash_table(%rip), %r12
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB2_89
# BB#87:                                # %add_mem_for_addr.exit.i
	cmpq	$55, %rcx
	jne	.LBB2_91
# BB#88:                                # %lor.lhs.false.i.i
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB2_91
.LBB2_89:                               # %if.end.i.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB2_115
# BB#90:                                # %if.end9.i.i
	movq	$58, %rdi
	movq	%r14, %rsi
	movq	%r9, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r9
.LBB2_91:                               # %wrap_constant.exit.i
	movl	(%rbx), %edx
	movq	$1, %rcx
	movq	%r12, %rdi
	jmp	.LBB2_92
.LBB2_115:                              # %if.then8.i.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.wrap_constant, %rcx
	movq	$548, %rsi              # imm = 0x224
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp53:
	.size	cselib_lookup, .Ltmp53-cselib_lookup
	.cfi_endproc

	.globl	cselib_subst_to_values
	.align	16, 0x90
	.type	cselib_subst_to_values,@function
cselib_subst_to_values:                 # @cselib_subst_to_values
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 128
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%rdi, %r8
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movl	(%r8), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	rtx_format(,%rcx,8), %rdx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	cmpq	$95, %rcx
	jg	.LBB3_23
# BB#1:                                 # %entry
	leaq	-54(%rcx), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$3, %rsi
	jae	.LBB3_3
# BB#2:
	movq	%r8, %rax
	jmp	.LBB3_22
.LBB3_23:                               # %entry
	leaq	-96(%rcx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$6, %rdx
	jae	.LBB3_24
.LBB3_21:                               # %sw.bb23
	movl	next_unknown_value(%rip), %edi
	incq	%rdi
	movl	%edi, next_unknown_value(%rip)
	movzbl	2(%r8), %esi
	callq	new_cselib_val
	movq	8(%rax), %rax
	jmp	.LBB3_22
.LBB3_3:                                # %entry
	cmpq	$61, %rcx
	jne	.LBB3_4
# BB#10:                                # %sw.bb
	movl	8(%r8), %ecx
	movq	reg_values(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
	jmp	.LBB3_11
	.align	16, 0x90
.LBB3_13:                               # %for.cond
                                        #   in Loop: Header=BB3_11 Depth=1
	movq	(%rcx), %rcx
.LBB3_11:                               # %sw.bb
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB3_52
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB3_11 Depth=1
	movq	8(%rcx), %rdx
	movq	8(%rdx), %rax
	movl	(%rax), %edx
	xorq	%rdi, %rdx
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB3_13
	jmp	.LBB3_22
.LBB3_4:                                # %entry
	cmpq	$66, %rcx
	jne	.LBB3_24
# BB#5:                                 # %sw.bb12
	movq	$16, %rcx
	movq	%rdi, %r15
	shrq	%cl, %r15
	testq	$134217728, %rdi        # imm = 0x8000000
	jne	.LBB3_21
# BB#6:                                 # %sw.bb12
	andq	$255, %r15
	cmpq	$51, %r15
	je	.LBB3_21
# BB#7:                                 # %lor.lhs.false4.i
	movl	mode_class(,%r15,4), %eax
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	jne	.LBB3_8
# BB#14:                                # %land.lhs.true.i
	movl	flag_float_store(%rip), %eax
	testq	%rax, %rax
	jne	.LBB3_21
	jmp	.LBB3_15
.LBB3_24:                               # %sw.epilog
	movzbl	rtx_length(%rcx), %edx
	decq	56(%rsp)                # 8-byte Folded Spill
	movq	%r8, 72(%rsp)           # 8-byte Spill
	jmp	.LBB3_25
	.align	16, 0x90
.LBB3_39:                               # %for.cond33.outer.outer.backedge
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%r12, %rbx
	movq	%rbx, %rdx
.LBB3_25:                               # %for.cond33.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_26 Depth 2
                                        #     Child Loop BB3_32 Depth 2
                                        #       Child Loop BB3_47 Depth 3
	movq	%rdx, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rax), %rdx
	xorq	%r13, %r13
	xorq	%r12, %r12
.LBB3_26:                               # %for.cond33
                                        #   Parent Loop BB3_25 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %r14
	leaq	(%rax,%r14), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB3_27
# BB#28:                                # %for.body36
                                        #   in Loop: Header=BB3_26 Depth=2
	movzbl	(%rdx,%r14), %ecx
	leaq	-1(%r14), %r12
	addq	$-8, %r13
	cmpq	$101, %rcx
	je	.LBB3_34
# BB#29:                                # %for.body36
                                        #   in Loop: Header=BB3_26 Depth=2
	cmpq	$69, %rcx
	jne	.LBB3_26
# BB#30:                                # %for.cond68.preheader
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%r8,%rdx,8), %rax
	movq	(%rax,%r14,8), %rax
	movslq	(%rax), %rcx
	addq	%r12, %rdx
	testq	%rcx, %rcx
	jle	.LBB3_25
# BB#31:                                # %for.body74.preheader
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$3, %rcx
	leaq	(%r8,%rsi,8), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	shlq	%cl, %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	xorq	%rdx, %rdx
.LBB3_32:                               # %for.body74
                                        #   Parent Loop BB3_25 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_47 Depth 3
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	8(%rax,%r15,8), %rdi
	callq	cselib_subst_to_values
	movq	24(%rsp), %r9           # 8-byte Reload
	movq	(%r9,%r14,8), %rcx
	movq	8(%rcx,%r15,8), %rsi
	movq	%rax, %rdi
	cmpq	%rsi, %rdi
	jne	.LBB3_40
# BB#33:                                #   in Loop: Header=BB3_32 Depth=2
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB3_50
.LBB3_40:                               # %land.lhs.true92
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	32(%rsp), %rax          # 8-byte Reload
	leaq	(%rbx,%rax), %rax
	movq	8(%r13,%rax), %rax
	cmpq	%rax, %rcx
	movq	48(%rsp), %r8           # 8-byte Reload
	jne	.LBB3_41
# BB#42:                                # %if.then103
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	cmpq	%r8, %rbx
	jne	.LBB3_44
# BB#43:                                # %if.then106
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	shallow_copy_rtx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%r14,8), %rcx
	movq	%rax, %rbx
.LBB3_44:                               # %if.end108
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	(%rcx), %edi
	callq	rtvec_alloc
	movq	32(%rsp), %rcx          # 8-byte Reload
	leaq	(%rbx,%rcx), %rbx
	movq	%rax, 8(%r13,%rbx)
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB3_45
# BB#46:                                #   in Loop: Header=BB3_32 Depth=2
	movq	$1, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	24(%rsp), %r9           # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB3_47
.LBB3_51:                               # %for.body122.for.body122_crit_edge
                                        #   in Loop: Header=BB3_47 Depth=3
	movq	8(%rbx,%r13), %rax
	incq	%rcx
.LBB3_47:                               # %for.body122
                                        #   Parent Loop BB3_25 Depth=1
                                        #     Parent Loop BB3_32 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%r9,%r14,8), %rsi
	movq	(%rsi,%rcx,8), %rsi
	movq	%rsi, (%rax,%rcx,8)
	movq	%rcx, %rax
	andq	%rdi, %rax
	movq	%rdx, %rsi
	andq	%rdi, %rsi
	cmpq	%rsi, %rax
	jne	.LBB3_51
# BB#48:                                #   in Loop: Header=BB3_32 Depth=2
	movq	48(%rsp), %r8           # 8-byte Reload
	jmp	.LBB3_49
.LBB3_41:                               #   in Loop: Header=BB3_32 Depth=2
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB3_50
.LBB3_45:                               #   in Loop: Header=BB3_32 Depth=2
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %r9           # 8-byte Reload
	movq	40(%rsp), %rdx          # 8-byte Reload
.LBB3_49:                               # %if.end140
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	16(%rsp), %rdi          # 8-byte Reload
.LBB3_50:                               # %if.end140
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx), %rax
	movq	8(%r13,%rax), %rax
	movq	%rdi, 8(%rax,%r15,8)
	movq	(%r9,%r14,8), %rax
	incq	%r15
	incq	%rdx
	movslq	(%rax), %rdi
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	movq	64(%rsp), %rbx          # 8-byte Reload
	jl	.LBB3_32
	jmp	.LBB3_39
	.align	16, 0x90
.LBB3_34:                               # %if.then42
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r8,%rax,8), %rbx
	movq	(%rbx,%r14,8), %rdi
	movq	%r8, %r15
	callq	cselib_subst_to_values
	movq	(%rbx,%r14,8), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB3_35
# BB#36:                                # %if.then42
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%r15, %rax
	movq	%r15, %r8
	jne	.LBB3_38
# BB#37:                                # %if.then55
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	%r8, %rdi
	movq	%r8, %rbx
	callq	shallow_copy_rtx
	movq	%rbx, %r8
	jmp	.LBB3_38
	.align	16, 0x90
.LBB3_35:                               #   in Loop: Header=BB3_25 Depth=1
	movq	%r15, %r8
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB3_38:                               # %if.end57
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rax,%rdx,8), %rax
	movq	%r14, 8(%r13,%rax)
	jmp	.LBB3_39
.LBB3_27:
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB3_22:                               # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_8:                                # %lor.lhs.false10.i
	andq	%rdx, %rax
	cmpq	$8, %rax
	jne	.LBB3_15
# BB#9:                                 # %lor.lhs.false10.i
	movl	flag_float_store(%rip), %eax
	testq	%rdx, %rax
	jne	.LBB3_21
.LBB3_15:                               # %if.end.i
	movq	8(%r8), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	movq	%r8, %rbx
	callq	cselib_lookup
	movq	%rbx, %r8
	testq	%rax, %rax
	je	.LBB3_21
# BB#16:                                # %if.end18.i
	movq	24(%rax), %rsi
	jmp	.LBB3_18
.LBB3_17:                               # %for.cond.i
                                        #   in Loop: Header=BB3_18 Depth=1
	movq	(%rsi), %rsi
.LBB3_18:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB3_21
# BB#19:                                # %for.body.i
                                        #   in Loop: Header=BB3_18 Depth=1
	movq	8(%rsi), %rcx
	movq	8(%rcx), %rax
	movzbl	2(%rax), %edx
	cmpq	%r15, %rdx
	jne	.LBB3_17
# BB#20:                                # %cselib_lookup_mem.exit
	testq	%rcx, %rcx
	jne	.LBB3_22
	jmp	.LBB3_21
.LBB3_52:                               # %for.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.cselib_subst_to_values, %rcx
	movq	$806, %rsi              # imm = 0x326
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp71:
	.size	cselib_subst_to_values, .Ltmp71-cselib_subst_to_values
	.cfi_endproc

	.align	16, 0x90
	.type	new_cselib_val,@function
new_cselib_val:                         # @new_cselib_val
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp76:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp77:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp78:
	.cfi_def_cfa_offset 32
.Ltmp79:
	.cfi_offset %rbx, -32
.Ltmp80:
	.cfi_offset %r14, -24
.Ltmp81:
	.cfi_offset %r15, -16
	movq	empty_vals(%rip), %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB4_2
# BB#1:                                 # %if.then
	movq	8(%rbx), %rax
	movq	%rax, empty_vals(%rip)
	jmp	.LBB4_9
.LBB4_2:                                # %if.else
	movq	cselib_obstack+32(%rip), %rcx
	movq	cselib_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$31, %rcx
	jg	.LBB4_4
# BB#3:                                 # %if.then3
	movabsq	$cselib_obstack, %rdi
	movq	$32, %rsi
	callq	_obstack_newchunk
	movq	cselib_obstack+24(%rip), %rax
.LBB4_4:                                # %if.end
	addq	$32, %rax
	movq	%rax, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB4_6
# BB#5:                                 # %if.then11
	movzbl	cselib_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, cselib_obstack+80(%rip)
.LBB4_6:                                # %if.end12
	movslq	cselib_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB4_8
# BB#7:                                 # %if.then32
	movq	%rax, cselib_obstack+24(%rip)
	movq	%rax, %rcx
.LBB4_8:                                # %if.end35
	movq	%rcx, cselib_obstack+16(%rip)
.LBB4_9:                                # %if.end39
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB4_11
# BB#10:                                # %if.end43
	movl	%r15d, (%rbx)
	movq	$60, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_fmt_0
	movq	%rax, 8(%rbx)
	movq	%rbx, 8(%rax)
	movq	$0, 24(%rbx)
	movq	$0, 16(%rbx)
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB4_11:                               # %if.then42
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.new_cselib_val, %rcx
	movq	$710, %rsi              # imm = 0x2C6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp82:
	.size	new_cselib_val, .Ltmp82-new_cselib_val
	.cfi_endproc

	.align	16, 0x90
	.type	hash_rtx,@function
hash_rtx:                               # @hash_rtx
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
	subq	$16, %rsp
.Ltmp94:
	.cfi_def_cfa_offset 64
.Ltmp95:
	.cfi_offset %rbx, -48
.Ltmp96:
	.cfi_offset %r12, -40
.Ltmp97:
	.cfi_offset %r13, -32
.Ltmp98:
	.cfi_offset %r14, -24
.Ltmp99:
	.cfi_offset %r15, -16
	movq	%rdi, %r8
	movl	(%r8), %edi
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	$16, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	leaq	(%rax,%rdx), %r14
	xorq	%r12, %r12
	cmpq	$95, %rdx
	jg	.LBB5_3
# BB#1:                                 # %entry
	leaq	-41(%rdx), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$28, %rcx
	ja	.LBB5_28
# BB#2:                                 # %entry
	jmpq	*.LJTI5_0(,%rcx,8)
.LBB5_5:                                # %sw.bb
	movq	%r8, %rdi
	movq	%rax, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	cselib_lookup
	testq	%rax, %rax
	je	.LBB5_4
# BB#6:                                 # %if.end
	movl	(%rax), %r12d
	jmp	.LBB5_4
.LBB5_3:                                # %entry
	leaq	-96(%rdx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$6, %rcx
	jb	.LBB5_4
	jmp	.LBB5_28
.LBB5_27:                               # %sw.bb95
	testq	$134217728, %rdi        # imm = 0x8000000
	jne	.LBB5_4
.LBB5_28:                               # %sw.epilog
	movzbl	rtx_length(%rdx), %r13d
	testq	%r13, %r13
	je	.LBB5_49
# BB#29:                                # %for.body110.lr.ph.lr.ph
	movq	rtx_format(,%rdx,8), %rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	jmp	.LBB5_30
.LBB5_7:                                # %sw.bb7
	movq	8(%r8), %rax
	addq	%r14, %rsi
	leaq	6912(%rax,%rsi), %r12
	testq	%rbx, %r12
	jne	.LBB5_4
# BB#8:                                 # %sw.bb7
	movq	$54, %r12
	jmp	.LBB5_4
.LBB5_9:                                # %sw.bb14
	addq	%r14, %r14
	testq	%rax, %rax
	je	.LBB5_13
# BB#10:                                # %for.cond.preheader
	movzbl	rtx_length+55(%rip), %eax
	cmpq	$3, %rax
	jb	.LBB5_14
# BB#11:                                # %for.body.lr.ph
	movq	$3, %rdx
.LBB5_12:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r8,%rdx,8), %rcx
	andq	%rbx, %r14
	addq	%rcx, %r14
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB5_12
	jmp	.LBB5_14
.LBB5_17:                               # %sw.bb50
	movq	8(%r8), %rax
	movslq	(%rax), %r15
	testq	%r15, %r15
	jle	.LBB5_18
# BB#19:
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	$1, %r12
	andq	%rbx, %r15
	movq	%rbx, %r13
	jmp	.LBB5_20
	.align	16, 0x90
.LBB5_22:                               # %for.body56.for.body56_crit_edge
                                        #   in Loop: Header=BB5_20 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rax
	incq	%r12
.LBB5_20:                               # %for.body56
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r12,8), %rdi
	movzbl	2(%rdi), %esi
	movq	%r14, %rbx
	xorq	%rdx, %rdx
	callq	hash_rtx
	movq	%rax, %r14
	addq	%rbx, %r14
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	%r15, %rax
	jne	.LBB5_22
# BB#21:
	movq	%r14, %r12
	jmp	.LBB5_4
.LBB5_23:                               # %sw.bb70
	movq	8(%r8), %rax
	leaq	8576(%r14,%rax), %r12
	testq	%rbx, %r12
	jne	.LBB5_4
# BB#24:                                # %sw.bb70
	movq	$67, %r12
	jmp	.LBB5_4
.LBB5_25:                               # %sw.bb82
	movq	8(%r8), %rax
	leaq	8704(%r14,%rax), %r12
	testq	%rbx, %r12
	jne	.LBB5_4
# BB#26:                                # %sw.bb82
	movq	$68, %r12
	jmp	.LBB5_4
.LBB5_13:                               # %if.else
	movl	16(%r8), %eax
	movl	24(%r8), %ecx
	addq	%r14, %rax
	addq	%rcx, %rax
	movq	%rax, %r14
.LBB5_14:                               # %if.end44
	testq	%rbx, %r14
	jne	.LBB5_16
# BB#15:                                # %if.end44
	movq	$55, %r14
.LBB5_16:                               # %if.end44
	movq	%r14, %r12
	jmp	.LBB5_4
.LBB5_18:
	movq	%r14, %r12
	jmp	.LBB5_4
.LBB5_52:                               # %while.cond.preheader
                                        #   in Loop: Header=BB5_30 Depth=1
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB5_48
# BB#53:                                # %while.body.preheader
                                        #   in Loop: Header=BB5_30 Depth=1
	incq	%rax
	movq	%r14, %rdx
.LBB5_54:                               # %while.body
                                        #   Parent Loop BB5_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	movq	%rcx, %r14
	movzbl	(%rax), %ecx
	addq	%rdx, %r14
	incq	%rax
	testq	%rcx, %rcx
	movq	%r14, %rdx
	jne	.LBB5_54
	jmp	.LBB5_48
	.align	16, 0x90
.LBB5_30:                               # %for.body110.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_31 Depth 2
                                        #     Child Loop BB5_54 Depth 2
                                        #     Child Loop BB5_37 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	decq	%r13
.LBB5_31:                               # %for.body110
                                        #   Parent Loop BB5_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movsbq	(%rdx,%r13), %rax
	cmpq	$68, %rax
	jg	.LBB5_34
# BB#32:                                # %for.body110
                                        #   in Loop: Header=BB5_31 Depth=2
	andq	$255, %rax
	cmpq	$48, %rax
	jne	.LBB5_33
.LBB5_45:                               # %for.cond107.backedge
                                        #   in Loop: Header=BB5_31 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r13
	testq	%rax, %rax
	jg	.LBB5_31
	jmp	.LBB5_49
	.align	16, 0x90
.LBB5_34:                               # %for.body110
                                        #   in Loop: Header=BB5_31 Depth=2
	cmpq	$100, %rax
	jle	.LBB5_35
# BB#39:                                # %for.body110
                                        #   in Loop: Header=BB5_31 Depth=2
	cmpq	$114, %rax
	jle	.LBB5_40
# BB#43:                                # %for.body110
                                        #   in Loop: Header=BB5_31 Depth=2
	andq	$255, %rax
	cmpq	$115, %rax
	jne	.LBB5_44
# BB#51:                                # %if.then166
                                        #   in Loop: Header=BB5_31 Depth=2
	movq	8(%r8,%r13,8), %rax
	testq	%rax, %rax
	je	.LBB5_45
	jmp	.LBB5_52
.LBB5_44:                               # %for.body110
                                        #   in Loop: Header=BB5_31 Depth=2
	cmpq	$116, %rax
	je	.LBB5_45
	jmp	.LBB5_33
	.align	16, 0x90
.LBB5_35:                               # %for.body110
                                        #   in Loop: Header=BB5_30 Depth=1
	andq	$255, %rax
	cmpq	$69, %rax
	jne	.LBB5_33
# BB#36:                                # %for.cond134.preheader
                                        #   in Loop: Header=BB5_30 Depth=1
	movq	8(%r8,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	movq	%r14, %rbx
	jle	.LBB5_48
	.align	16, 0x90
.LBB5_37:                               # %for.body142
                                        #   Parent Loop BB5_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r15,8), %rdi
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r8, %r14
	callq	hash_rtx
	movq	%r14, %r8
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB5_4
# BB#38:                                # %if.end155
                                        #   in Loop: Header=BB5_37 Depth=2
	movq	8(%r8,%r13,8), %rax
	addq	%rbx, %r14
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	movq	%r14, %rbx
	jl	.LBB5_37
	jmp	.LBB5_48
	.align	16, 0x90
.LBB5_40:                               # %for.body110
                                        #   in Loop: Header=BB5_30 Depth=1
	andq	$255, %rax
	cmpq	$101, %rax
	jne	.LBB5_41
# BB#46:                                # %if.then116
                                        #   in Loop: Header=BB5_30 Depth=1
	movq	8(%r8,%r13,8), %rdi
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r8, %rbx
	callq	hash_rtx
	movq	%rbx, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_47
	jmp	.LBB5_4
.LBB5_41:                               # %for.body110
                                        #   in Loop: Header=BB5_30 Depth=1
	cmpq	$105, %rax
	jne	.LBB5_33
# BB#42:                                # %if.then183
                                        #   in Loop: Header=BB5_30 Depth=1
	movl	8(%r8,%r13,8), %eax
.LBB5_47:                               # %if.end125
                                        #   in Loop: Header=BB5_30 Depth=1
	addq	%rax, %r14
.LBB5_48:                               # %for.cond107.outer.backedge
                                        #   in Loop: Header=BB5_30 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	(%rsp), %rdx            # 8-byte Reload
	jg	.LBB5_30
.LBB5_49:                               # %for.end207
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movq	%r14, %r12
	jne	.LBB5_4
# BB#50:                                # %cond.false210
	movzbl	(%r8), %r12d
	incq	%r12
.LBB5_4:                                # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_33:                               # %if.else200
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.hash_rtx, %rcx
	movq	$688, %rsi              # imm = 0x2B0
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp100:
	.size	hash_rtx, .Ltmp100-hash_rtx
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_27
	.quad	.LBB5_28
	.quad	.LBB5_4
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_4
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_7
	.quad	.LBB5_9
	.quad	.LBB5_17
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_4
	.quad	.LBB5_28
	.quad	.LBB5_5
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_28
	.quad	.LBB5_5
	.quad	.LBB5_23
	.quad	.LBB5_25
	.quad	.LBB5_4

	.text
	.globl	cselib_process_insn
	.align	16, 0x90
	.type	cselib_process_insn,@function
cselib_process_insn:                    # @cselib_process_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp107:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp108:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp109:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp110:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp111:
	.cfi_def_cfa_offset 48
	subq	$3440, %rsp             # imm = 0xD70
.Ltmp112:
	.cfi_def_cfa_offset 3488
.Ltmp113:
	.cfi_offset %rbx, -48
.Ltmp114:
	.cfi_offset %r12, -40
.Ltmp115:
	.cfi_offset %r13, -32
.Ltmp116:
	.cfi_offset %r14, -24
.Ltmp117:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%r14, cselib_current_insn(%rip)
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$36, %rcx
	je	.LBB6_6
# BB#1:                                 # %entry
	cmpq	$34, %rcx
	jne	.LBB6_4
# BB#2:                                 # %land.lhs.true
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB6_6
# BB#3:                                 # %land.lhs.true.lor.lhs.false4_crit_edge
	movl	(%r14), %eax
.LBB6_4:                                # %lor.lhs.false4
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$32, %rax
	jne	.LBB6_14
# BB#5:                                 # %land.lhs.true8
	movq	32(%r14), %rcx
	movl	(%rcx), %ecx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$134217769, %rcx        # imm = 0x8000029
	jne	.LBB6_14
.LBB6_6:                                # %if.then
	movq	used_regs(%rip), %rax
	cmpq	$0, 8(%rax)
	je	.LBB6_9
# BB#7:
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB6_8:                                # %for.body4.i
                                        # =>This Inner Loop Header: Depth=1
	movl	32(%rax,%rsi,4), %eax
	movq	reg_values(%rip), %rsi
	movq	$0, 32(%rsi,%rax,8)
	movq	used_regs(%rip), %rax
	movq	8(%rax), %rdi
	incq	%rdx
	movq	%rdx, %rsi
	andq	%rcx, %rsi
	cmpq	%rdi, %rsi
	jb	.LBB6_8
.LBB6_9:                                # %if.end.i
	xorq	%rcx, %rcx
	movl	%ecx, max_value_regs(%rip)
	movq	$0, 8(%rax)
	movq	hash_table(%rip), %rdi
	callq	htab_empty
	movq	cselib_startobj(%rip), %rsi
	movq	cselib_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB6_12
# BB#10:                                # %if.end.i
	movq	cselib_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB6_12
# BB#11:                                # %if.then20.i
	movq	%rsi, cselib_obstack+16(%rip)
	movq	%rsi, cselib_obstack+24(%rip)
	jmp	.LBB6_13
.LBB6_12:                               # %if.else21.i
	movabsq	$cselib_obstack, %rdi
	callq	obstack_free
.LBB6_13:                               # %clear_table.exit
	movq	$0, empty_vals(%rip)
	movq	$0, empty_elt_lists(%rip)
	movq	$0, empty_elt_loc_lists(%rip)
	xorq	%rax, %rax
	movl	%eax, n_useless_values(%rip)
	movl	%eax, next_unknown_value(%rip)
.LBB6_141:                              # %if.end80
	addq	$3440, %rsp             # imm = 0xD70
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_14:                               # %if.end
	movzbl	rtx_class(%rax), %ecx
	cmpq	$105, %rcx
	jne	.LBB6_15
# BB#16:                                # %if.end25
	cmpq	$34, %rax
	jne	.LBB6_23
# BB#17:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB6_18:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	call_used_regs(%rbx), %eax
	testq	%rax, %rax
	je	.LBB6_20
# BB#19:                                # %if.then36
                                        #   in Loop: Header=BB6_18 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	cselib_invalidate_regno
.LBB6_20:                               # %for.inc
                                        #   in Loop: Header=BB6_18 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB6_18
# BB#21:                                # %for.end
	movzbl	3(%r14), %eax
	testq	$4, %rax
	jne	.LBB6_23
# BB#22:                                # %if.then42
	movq	callmem(%rip), %rdx
	movq	hash_table(%rip), %rdi
	movabsq	$cselib_invalidate_mem_1, %rsi
	callq	htab_traverse
.LBB6_23:                               # %if.end44
	movq	32(%r14), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%r8, %r8
	cmpq	$38, %rcx
	jne	.LBB6_25
# BB#24:                                # %if.then.i
	movq	8(%rdx), %r8
	movq	16(%rdx), %rdx
	movl	(%rdx), %eax
.LBB6_25:                               # %if.end.i100
	movq	%r14, 16(%rsp)          # 8-byte Spill
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	jne	.LBB6_26
# BB#28:                                # %if.then25.i
	movq	8(%rdx), %rax
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movl	(%rax), %edx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_29
.LBB6_15:                               # %if.then24
	movq	$0, cselib_current_insn(%rip)
	jmp	.LBB6_141
.LBB6_26:                               # %if.end.i100
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	cmpq	$47, %rax
	movq	%r8, %rdx
	jne	.LBB6_142
# BB#27:                                # %if.end51.thread.i
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rax
	movq	%rax, 48(%rsp)
	movq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_34
.LBB6_32:                               # %if.then36.i
                                        #   in Loop: Header=BB6_29 Depth=1
	movq	16(%rsi), %r9
	movq	$5, %rcx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	%r9, 48(%rsp,%rbx)
	movq	8(%rsi), %rcx
	movq	%rcx, 56(%rsp,%rbx)
	incq	%rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
.LBB6_29:                               # %for.cond.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_30 Depth 2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.align	16, 0x90
.LBB6_30:                               # %for.cond.i
                                        #   Parent Loop BB6_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB6_33
# BB#31:                                # %for.body.i
                                        #   in Loop: Header=BB6_30 Depth=2
	movq	(%rax,%rdx,8), %rsi
	movzbl	(%rsi), %ecx
	decq	%rdx
	cmpq	$47, %rcx
	jne	.LBB6_30
	jmp	.LBB6_32
.LBB6_33:                               # %if.end51.i
	movq	$32, %rcx
	movq	40(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r8, %rdx
	jle	.LBB6_142
.LBB6_34:                               # %for.body54.lr.ph.i
	leaq	72(%rsp), %rbx
	movq	$1, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_35:                               # %for.body54.i
                                        # =>This Inner Loop Header: Depth=1
	movq	-16(%rbx), %r14
	movl	(%r14), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$64, %rax
	jne	.LBB6_37
# BB#36:                                # %if.then65.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	8(%r14), %r14
	movq	%r14, -16(%rbx)
	movl	(%r14), %esi
.LBB6_37:                               # %if.end72.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB6_39
# BB#38:                                # %if.end72.i
                                        #   in Loop: Header=BB6_35 Depth=1
	cmpq	$66, %rax
	jne	.LBB6_46
.LBB6_39:                               # %if.then79.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	-24(%rbx), %rax
	testq	%rdx, %rdx
	jne	.LBB6_41
# BB#40:                                #   in Loop: Header=BB6_35 Depth=1
	movq	%rdx, %r15
	movq	%rcx, %r12
	jmp	.LBB6_42
	.align	16, 0x90
.LBB6_41:                               # %if.then84.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	%rcx, %r12
	movzbl	2(%rax), %esi
	movq	$72, %rdi
	movq	%rdx, %r15
	movq	%rax, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_eee
	movl	(%r14), %esi
.LBB6_42:                               # %if.end87.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	cselib_lookup
	movq	%rax, -8(%rbx)
	movzbl	(%r14), %eax
	cmpq	$66, %rax
	jne	.LBB6_44
# BB#43:                                # %if.then97.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movl	target_flags(%rip), %esi
	movq	8(%r14), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$1, %rdx
	callq	cselib_lookup
	movq	%rax, (%rbx)
	jmp	.LBB6_45
	.align	16, 0x90
.LBB6_44:                               # %if.else106.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	$0, (%rbx)
.LBB6_45:                               # %for.inc112.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	%r12, %rcx
	movq	%r15, %rdx
.LBB6_46:                               # %for.inc112.i
                                        #   in Loop: Header=BB6_35 Depth=1
	movq	%r13, %rax
	andq	%rcx, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	andq	%rcx, %r14
	incq	%r13
	addq	$32, %rbx
	cmpq	%r14, %rax
	jne	.LBB6_35
# BB#47:                                # %for.end114.i
	movabsq	$cselib_invalidate_rtx, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rbx
	callq	note_stores
	.align	16, 0x90
.LBB6_48:                               # %for.body117.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_100 Depth 2
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	56(%rsp,%rax), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	je	.LBB6_50
# BB#49:                                # %for.body117.i
                                        #   in Loop: Header=BB6_48 Depth=1
	cmpq	$66, %rax
	jne	.LBB6_122
.LBB6_50:                               # %if.then129.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$5, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	64(%rsp,%rdx), %rcx
	movq	72(%rsp,%rdx), %rdx
	cmpq	$61, %rax
	movq	%rbx, %rax
	jne	.LBB6_52
# BB#51:                                # %cond.true.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	8(%rdi), %eax
.LBB6_52:                               # %cond.end.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	testq	%rcx, %rcx
	je	.LBB6_122
# BB#53:                                # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rdi, %r15
	movq	%rcx, %r13
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %r12
	movq	%rax, %r14
	callq	side_effects_p
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	32(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	%r13, %r9
	movq	%r15, %r13
	testq	%rbx, %rax
	jne	.LBB6_122
# BB#54:                                # %if.end.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB6_96
# BB#55:                                # %if.then3.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$32, %rcx
	movq	%rdi, %r10
	movq	reg_values(%rip), %rax
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	cmpq	$0, 32(%rax,%r10,8)
	jne	.LBB6_59
# BB#56:                                # %do.body.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rdi, %r15
	movq	used_regs(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB6_58
# BB#57:                                # %if.then8.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	%r9, %r13
	movq	%r10, %rbx
	callq	varray_grow
	movq	%rbx, %r10
	movq	%r13, %r9
	movq	24(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rax, used_regs(%rip)
	movq	8(%rax), %rcx
.LBB6_58:                               # %if.end11.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r15, %rdi
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movl	%edi, 32(%rax,%rcx,4)
.LBB6_59:                               # %if.end15.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB6_75
# BB#60:                                # %if.then17.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	2(%r13), %eax
	movq	%rdi, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	je	.LBB6_64
# BB#61:                                # %if.then17.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	leaq	-21(%rdi), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_64
# BB#62:                                # %if.then17.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	leaq	-45(%rdi), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_64
# BB#63:                                # %if.then17.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	addq	$-29, %rdi
	andq	%rbx, %rdi
	cmpq	$7, %rdi
	jbe	.LBB6_64
# BB#68:                                # %cond.false45.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	cmpq	$24, %rax
	jne	.LBB6_69
# BB#71:                                # %cond.true58.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB6_73
.LBB6_96:                               # %if.else.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	(%r13), %eax
	cmpq	$66, %rax
	jne	.LBB6_122
# BB#97:                                # %if.else.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	testq	%rsi, %rsi
	je	.LBB6_122
# BB#98:                                # %if.then108.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	16(%r9), %rax
	testq	%rax, %rax
	jne	.LBB6_100
# BB#99:                                # %if.end114.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	n_useless_values(%rip), %eax
	decq	%rax
	movl	%eax, n_useless_values(%rip)
	jmp	.LBB6_103
	.align	16, 0x90
.LBB6_100:                              # %for.body.i.i.i
                                        #   Parent Loop BB6_48 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$66, %rdx
	jne	.LBB6_102
# BB#101:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB6_100 Depth=2
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	cmpq	%rsi, %rcx
	je	.LBB6_122
.LBB6_102:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB6_100 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB6_100
.LBB6_103:                              # %for.end.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	empty_elt_lists(%rip), %rax
	movq	24(%rsi), %r15
	testq	%rax, %rax
	jne	.LBB6_104
# BB#105:                               # %if.else.i242.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB6_107
# BB#106:                               # %if.then3.i244.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$16, %rsi
	movabsq	$cselib_obstack, %rdi
	movq	%r9, %rbx
	callq	_obstack_newchunk
	movq	%rbx, %r9
	movq	cselib_obstack+24(%rip), %rcx
.LBB6_107:                              # %if.end.i247.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	addq	$16, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB6_109
# BB#108:                               # %if.then10.i250.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB6_109:                              # %if.end11.i264.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB6_111
# BB#110:                               # %if.then31.i265.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB6_111:                              # %if.end34.i266.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rdx, cselib_obstack+16(%rip)
	movq	24(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB6_112
.LBB6_64:                               # %cond.true32.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB6_73
# BB#65:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB6_67
# BB#66:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movabsq	$1, %rax
.LBB6_67:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	incq	%rax
	jmp	.LBB6_73
.LBB6_104:                              # %if.then.i237.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	(%rax), %rcx
	movq	%rcx, empty_elt_lists(%rip)
.LBB6_112:                              # %new_elt_list.exit270.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r15, (%rax)
	movq	%r9, 8(%rax)
	movq	%rax, 24(%rsi)
	movq	16(%r9), %rbx
	movq	%r9, %r15
	movq	8(%rsi), %rsi
	movq	%r13, %rdi
	callq	replace_equiv_address_nv
	movq	empty_elt_loc_lists(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB6_113
# BB#114:                               # %if.else.i205.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %r13
	movq	cselib_obstack+32(%rip), %rcx
	movq	cselib_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB6_116
# BB#115:                               # %if.then3.i207.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$24, %rsi
	movabsq	$cselib_obstack, %rdi
	callq	_obstack_newchunk
	movq	cselib_obstack+24(%rip), %rax
.LBB6_116:                              # %if.end.i210.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	addq	$24, %rax
	movq	%rax, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rcx
	cmpq	%rcx, %rax
	movq	%r15, %r9
	jne	.LBB6_118
# BB#117:                               # %if.then10.i213.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB6_118:                              # %if.end11.i227.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rax
	notq	%rdx
	andq	%rax, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jle	.LBB6_120
# BB#119:                               # %if.then31.i228.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB6_120:                              # %if.end34.i229.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rdx, cselib_obstack+16(%rip)
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB6_121
.LBB6_113:                              # %if.then.i200.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rbx, %r13
	movq	(%rcx), %rdx
	movq	%rdx, empty_elt_loc_lists(%rip)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r15, %r9
.LBB6_121:                              # %new_elt_loc_list.exit234.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r13, (%rcx)
	movq	%rax, 8(%rcx)
	movq	cselib_current_insn(%rip), %rax
	movq	%rax, 16(%rcx)
	movq	%rcx, 16(%r9)
	jmp	.LBB6_122
.LBB6_69:                               # %cond.false45.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	cmpq	$18, %rax
	jne	.LBB6_72
# BB#70:                                # %cond.true50.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB6_73
.LBB6_72:                               # %cond.false62.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB6_73:                               # %cond.end78.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	max_value_regs(%rip), %ecx
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB6_75
# BB#74:                                # %if.then82.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	%eax, max_value_regs(%rip)
.LBB6_75:                               # %if.end84.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	reg_values(%rip), %rcx
	movq	empty_elt_lists(%rip), %rax
	movq	32(%rcx,%r10,8), %r15
	testq	%rax, %rax
	jne	.LBB6_76
# BB#77:                                # %if.else.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB6_79
# BB#78:                                # %if.then3.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$16, %rsi
	movabsq	$cselib_obstack, %rdi
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movq	%r10, %r13
	callq	_obstack_newchunk
	movq	%r13, %r10
	movq	40(%rsp), %r9           # 8-byte Reload
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	cselib_obstack+24(%rip), %rcx
.LBB6_79:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	addq	$16, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB6_81
# BB#80:                                # %if.then10.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB6_81:                               # %if.end11.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB6_83
# BB#82:                                # %if.then31.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB6_83:                               # %if.end34.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rdx, cselib_obstack+16(%rip)
	jmp	.LBB6_84
.LBB6_76:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	(%rax), %rcx
	movq	%rcx, empty_elt_lists(%rip)
.LBB6_84:                               # %new_elt_list.exit.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r15, (%rax)
	movq	%r9, 8(%rax)
	movq	reg_values(%rip), %rcx
	movq	%rax, 32(%rcx,%r10,8)
	movq	16(%r9), %r15
	testq	%r15, %r15
	jne	.LBB6_86
# BB#85:                                # %if.then96.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movl	n_useless_values(%rip), %eax
	decq	%rax
	movl	%eax, n_useless_values(%rip)
.LBB6_86:                               # %if.end97.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	empty_elt_loc_lists(%rip), %rax
	testq	%rax, %rax
	jne	.LBB6_87
# BB#88:                                # %if.else.i168.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB6_90
# BB#89:                                # %if.then3.i170.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	$24, %rsi
	movabsq	$cselib_obstack, %rdi
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	%r9, %r13
	callq	_obstack_newchunk
	movq	%r13, %r9
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	cselib_obstack+24(%rip), %rcx
.LBB6_90:                               # %if.end.i173.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	addq	$24, %rcx
	movq	%rcx, cselib_obstack+24(%rip)
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB6_92
# BB#91:                                # %if.then10.i176.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB6_92:                               # %if.end11.i190.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %r8
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB6_94
# BB#93:                                # %if.then31.i191.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r8, cselib_obstack+24(%rip)
	movq	%r8, %rdx
.LBB6_94:                               # %if.end34.i192.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%rdx, cselib_obstack+16(%rip)
	jmp	.LBB6_95
.LBB6_87:                               # %if.then.i163.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	(%rax), %rcx
	movq	%rcx, empty_elt_loc_lists(%rip)
.LBB6_95:                               # %new_elt_loc_list.exit.i.i
                                        #   in Loop: Header=BB6_48 Depth=1
	movq	%r15, (%rax)
	movq	%r13, 8(%rax)
	movq	cselib_current_insn(%rip), %rcx
	movq	%rcx, 16(%rax)
	movq	%rax, 16(%r9)
	.align	16, 0x90
.LBB6_122:                              # %for.inc137.i
                                        #   in Loop: Header=BB6_48 Depth=1
	incq	%r12
	movq	%r12, %rax
	andq	%rbx, %rax
	cmpq	%r14, %rax
	jne	.LBB6_48
	jmp	.LBB6_123
.LBB6_142:                              # %for.end114.thread.i
	movabsq	$cselib_invalidate_rtx, %rsi
	xorq	%rdx, %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	note_stores
.LBB6_123:                              # %cselib_record_sets.exit
	movq	16(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$34, %rax
	jne	.LBB6_137
# BB#124:                               # %if.then49
	movq	64(%rcx), %r15
	testq	%r15, %r15
	je	.LBB6_137
# BB#125:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$cselib_invalidate_mem_1, %r14
	movabsq	$global_rtl+16, %r13
	.align	16, 0x90
.LBB6_126:                              # %for.body55
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_127 Depth 2
                                        #       Child Loop BB6_128 Depth 3
	movq	8(%r15), %rbx
	movzbl	(%rbx), %eax
	cmpq	$49, %rax
	jne	.LBB6_136
	.align	16, 0x90
.LBB6_127:                              # %if.then63
                                        #   Parent Loop BB6_126 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_128 Depth 3
	addq	$8, %rbx
.LBB6_128:                              # %while.cond.i
                                        #   Parent Loop BB6_126 Depth=1
                                        #     Parent Loop BB6_127 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rbx), %rbx
	movl	(%rbx), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$131, %rax
	jg	.LBB6_133
# BB#129:                               # %while.cond.i
                                        #   in Loop: Header=BB6_128 Depth=3
	leaq	-63(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jb	.LBB6_127
# BB#130:                               # %while.cond.i
                                        #   in Loop: Header=BB6_128 Depth=3
	cmpq	$61, %rax
	jne	.LBB6_131
# BB#134:                               # %if.then.i107
                                        #   in Loop: Header=BB6_128 Depth=3
	movl	8(%rbx), %edi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	cselib_invalidate_regno
	jmp	.LBB6_135
	.align	16, 0x90
.LBB6_133:                              # %while.cond.i
                                        #   in Loop: Header=BB6_128 Depth=3
	addq	$-132, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	jbe	.LBB6_127
	jmp	.LBB6_135
.LBB6_131:                              # %while.cond.i
                                        #   in Loop: Header=BB6_128 Depth=3
	cmpq	$66, %rax
	jne	.LBB6_135
# BB#132:                               # %if.then21.i
                                        #   in Loop: Header=BB6_128 Depth=3
	movq	hash_table(%rip), %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	htab_traverse
	.align	16, 0x90
.LBB6_135:                              # %if.end22.i
                                        #   in Loop: Header=BB6_128 Depth=3
	movzbl	2(%rbx), %esi
	movq	%rbx, %rdi
	callq	push_operand
	testq	%r12, %rax
	movq	%r13, %rbx
	jne	.LBB6_128
.LBB6_136:                              # %for.inc71
                                        #   in Loop: Header=BB6_126 Depth=1
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB6_126
.LBB6_137:                              # %if.end76
	movslq	n_useless_values(%rip), %rax
	movq	$0, cselib_current_insn(%rip)
	cmpq	$33, %rax
	jl	.LBB6_141
# BB#138:
	xorq	%r14, %r14
	movabsq	$discard_useless_locs, %rbx
	.align	16, 0x90
.LBB6_139:                              # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	hash_table(%rip), %rdi
	movb	%r14b, values_became_useless(%rip)
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	htab_traverse
	movzbl	values_became_useless(%rip), %eax
	testq	%rax, %rax
	jne	.LBB6_139
# BB#140:                               # %do.end.i
	movq	hash_table(%rip), %rdi
	movabsq	$discard_useless_values, %rsi
	xorq	%rdx, %rdx
	callq	htab_traverse
	movl	n_useless_values(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_141
# BB#143:                               # %if.then.i111
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_useless_values, %rcx
	movq	$394, %rsi              # imm = 0x18A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp118:
	.size	cselib_process_insn, .Ltmp118-cselib_process_insn
	.cfi_endproc

	.align	16, 0x90
	.type	cselib_invalidate_regno,@function
cselib_invalidate_regno:                # @cselib_invalidate_regno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp125:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp126:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp127:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp128:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp129:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp130:
	.cfi_def_cfa_offset 64
.Ltmp131:
	.cfi_offset %rbx, -48
.Ltmp132:
	.cfi_offset %r12, -40
.Ltmp133:
	.cfi_offset %r13, -32
.Ltmp134:
	.cfi_offset %r14, -24
.Ltmp135:
	.cfi_offset %r15, -16
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB7_3
# BB#1:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$53, %rax
	jb	.LBB7_3
# BB#2:                                 # %land.lhs.true1
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rax
	testq	%rax, %rax
	jns	.LBB7_58
.LBB7_3:                                # %if.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rdi, %r9
	andq	%rcx, %r9
	cmpq	$52, %r9
	ja	.LBB7_23
# BB#4:                                 # %if.end
	testq	%rcx, %rsi
	je	.LBB7_23
# BB#5:                                 # %if.then9
	movl	max_value_regs(%rip), %eax
	xorq	%r10, %r10
	cmpq	%r9, %rax
	ja	.LBB7_7
# BB#6:                                 # %if.then9
	movq	%rdi, %r10
	subq	%rax, %r10
.LBB7_7:                                # %if.then9
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	movq	%rcx, %rdx
	je	.LBB7_11
# BB#8:                                 # %if.then9
	leaq	-21(%rdi), %rax
	andq	%rdx, %rax
	cmpq	$8, %rax
	jb	.LBB7_11
# BB#9:                                 # %if.then9
	leaq	-45(%rdi), %rax
	andq	%rdx, %rax
	cmpq	$8, %rax
	jb	.LBB7_11
# BB#10:                                # %if.then9
	leaq	-29(%rdi), %rax
	andq	%rdx, %rax
	cmpq	$7, %rax
	jbe	.LBB7_11
# BB#16:                                # %cond.false
	movq	%rsi, %rax
	andq	%rdx, %rax
	movq	%rdx, %r8
	cmpq	$24, %rax
	jne	.LBB7_17
# BB#19:                                # %cond.true52
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB7_21
.LBB7_23:                               # %if.else73
	leaq	1(%rdi), %rax
	movq	%rdi, %r10
	movq	%rcx, %rsi
	jmp	.LBB7_24
.LBB7_11:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	%rdx, %rsi
	je	.LBB7_22
# BB#12:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB7_13
# BB#14:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	addq	%rdi, %rax
	jmp	.LBB7_24
.LBB7_17:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB7_20
# BB#18:                                # %cond.true46
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB7_21
.LBB7_13:
	movabsq	$0, %rax
	incq	%rax
	addq	%rdi, %rax
	jmp	.LBB7_24
.LBB7_20:                               # %cond.false56
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movl	target_flags(%rip), %ebx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB7_21:                               # %cond.end70
	movq	%r8, %rsi
.LBB7_22:                               # %cond.end70
	addq	%rdi, %rax
.LBB7_24:                               # %for.cond.preheader
	andq	%rsi, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r10, %r11
	andq	%rsi, %r11
	cmpq	%rax, %r11
	jae	.LBB7_57
	.align	16, 0x90
.LBB7_25:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_30 Depth 2
                                        #       Child Loop BB7_49 Depth 3
                                        #         Child Loop BB7_51 Depth 4
                                        #       Child Loop BB7_31 Depth 3
                                        #         Child Loop BB7_43 Depth 4
	movq	reg_values(%rip), %rcx
	movq	32(%rcx,%r11,8), %rax
	testq	%rax, %rax
	je	.LBB7_56
# BB#26:                                # %while.body.lr.ph.lr.ph
                                        #   in Loop: Header=BB7_25 Depth=1
	movq	%r11, %rdx
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%rdi, %rdx
	cmpq	$8, %rdx
	movabsq	$0, %rdx
	jne	.LBB7_28
# BB#27:                                # %while.body.lr.ph.lr.ph
                                        #   in Loop: Header=BB7_25 Depth=1
	movabsq	$1, %rdx
.LBB7_28:                               # %while.body.lr.ph.lr.ph
                                        #   in Loop: Header=BB7_25 Depth=1
	leaq	32(%rcx,%r11,8), %r8
	leaq	-21(%r11), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rdx, %rcx
	leaq	-45(%r11), %rdx
	andq	%rsi, %rdx
	cmpq	$8, %rdx
	sbbq	%rdx, %rdx
	andq	$1, %rdx
	orq	%rcx, %rdx
	leaq	-29(%r11), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	sbbq	%r12, %r12
	andq	$1, %r12
	orq	%rdx, %r12
	leaq	-1(%r11), %r13
.LBB7_30:                               # %while.body.lr.ph
                                        #   Parent Loop BB7_25 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_49 Depth 3
                                        #         Child Loop BB7_51 Depth 4
                                        #       Child Loop BB7_31 Depth 3
                                        #         Child Loop BB7_43 Depth 4
	movq	%r11, %rdi
	andq	%rsi, %rdi
	cmpq	$52, %rdi
	movq	%rax, %r14
	ja	.LBB7_49
	.align	16, 0x90
.LBB7_31:                               # %if.then83.us
                                        #   Parent Loop BB7_25 Depth=1
                                        #     Parent Loop BB7_30 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_43 Depth 4
	movq	8(%r14), %r15
	movq	8(%r15), %rax
	movzbl	2(%rax), %eax
	testq	%r12, %r12
	je	.LBB7_32
# BB#37:                                # %cond.true107.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB7_41
# BB#38:                                # %lor.rhs112.us
                                        #   in Loop: Header=BB7_31 Depth=3
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB7_40
# BB#39:                                # %lor.rhs112.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movabsq	$1, %rax
.LBB7_40:                               # %lor.rhs112.us
                                        #   in Loop: Header=BB7_31 Depth=3
	incq	%rax
	jmp	.LBB7_41
	.align	16, 0x90
.LBB7_32:                               # %cond.false124.us
                                        #   in Loop: Header=BB7_31 Depth=3
	cmpq	$18, %rax
	jne	.LBB7_33
# BB#35:                                # %cond.true132.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB7_41
	.align	16, 0x90
.LBB7_33:                               # %cond.false124.us
                                        #   in Loop: Header=BB7_31 Depth=3
	cmpq	$24, %rax
	jne	.LBB7_36
# BB#34:                                # %cond.true144.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB7_41
.LBB7_36:                               # %cond.false148.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	.align	16, 0x90
.LBB7_41:                               # %if.end174.us
                                        #   in Loop: Header=BB7_31 Depth=3
	addq	%r13, %rax
	andq	%rsi, %rax
	cmpq	%r9, %rax
	jb	.LBB7_29
# BB#42:                                # %if.end178.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movq	(%r14), %rax
	movq	%rax, (%r8)
	movq	empty_elt_lists(%rip), %rax
	movq	%rax, (%r14)
	addq	$16, %r15
	movq	%r14, empty_elt_lists(%rip)
	movq	%r15, %rcx
	jmp	.LBB7_43
	.align	16, 0x90
.LBB7_45:                               # %for.inc.us
                                        #   in Loop: Header=BB7_43 Depth=4
	movq	%rax, %rcx
.LBB7_43:                               # %for.cond179.us
                                        #   Parent Loop BB7_25 Depth=1
                                        #     Parent Loop BB7_30 Depth=2
                                        #       Parent Loop BB7_31 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%rcx), %rax
	movq	8(%rax), %rdx
	movzbl	(%rdx), %ebx
	cmpq	$61, %rbx
	jne	.LBB7_45
# BB#44:                                # %land.lhs.true184.us
                                        #   in Loop: Header=BB7_43 Depth=4
	movl	8(%rdx), %edx
	cmpq	%rdi, %rdx
	jne	.LBB7_45
# BB#46:                                # %if.then188.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movq	(%rax), %rdx
	movq	%rdx, (%rcx)
	movq	empty_elt_loc_lists(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rax, empty_elt_loc_lists(%rip)
	cmpq	$0, (%r15)
	jne	.LBB7_48
# BB#47:                                # %if.then194.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movl	n_useless_values(%rip), %eax
	incq	%rax
	movl	%eax, n_useless_values(%rip)
.LBB7_48:                               # %while.cond.backedge.us
                                        #   in Loop: Header=BB7_31 Depth=3
	movq	(%r8), %r14
	testq	%r14, %r14
	jne	.LBB7_31
	jmp	.LBB7_56
	.align	16, 0x90
.LBB7_49:                               # %if.end174
                                        #   Parent Loop BB7_25 Depth=1
                                        #     Parent Loop BB7_30 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_51 Depth 4
	cmpq	%r9, %rdi
	jb	.LBB7_29
# BB#50:                                # %if.end178
                                        #   in Loop: Header=BB7_49 Depth=3
	movq	%rsi, %r15
	movq	(%r14), %rcx
	movq	8(%r14), %rax
	movq	%rcx, (%r8)
	movq	empty_elt_lists(%rip), %rcx
	movq	%rcx, (%r14)
	addq	$16, %rax
	movq	%r14, empty_elt_lists(%rip)
	movq	%rax, %rdx
	jmp	.LBB7_51
	.align	16, 0x90
.LBB7_59:                               # %for.inc
                                        #   in Loop: Header=BB7_51 Depth=4
	movq	%rcx, %rdx
.LBB7_51:                               # %for.cond179
                                        #   Parent Loop BB7_25 Depth=1
                                        #     Parent Loop BB7_30 Depth=2
                                        #       Parent Loop BB7_49 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%rdx), %rcx
	movq	8(%rcx), %rbx
	movzbl	(%rbx), %esi
	cmpq	$61, %rsi
	jne	.LBB7_59
# BB#52:                                # %land.lhs.true184
                                        #   in Loop: Header=BB7_51 Depth=4
	movl	8(%rbx), %esi
	cmpq	%rdi, %rsi
	jne	.LBB7_59
# BB#53:                                # %if.then188
                                        #   in Loop: Header=BB7_49 Depth=3
	movq	(%rcx), %rsi
	movq	%rsi, (%rdx)
	movq	empty_elt_loc_lists(%rip), %rdx
	movq	%rdx, (%rcx)
	movq	%rcx, empty_elt_loc_lists(%rip)
	cmpq	$0, (%rax)
	jne	.LBB7_55
# BB#54:                                # %if.then194
                                        #   in Loop: Header=BB7_49 Depth=3
	movl	n_useless_values(%rip), %eax
	incq	%rax
	movl	%eax, n_useless_values(%rip)
.LBB7_55:                               # %while.cond.backedge
                                        #   in Loop: Header=BB7_49 Depth=3
	movq	(%r8), %r14
	testq	%r14, %r14
	movq	%r15, %rsi
	jne	.LBB7_49
	jmp	.LBB7_56
	.align	16, 0x90
.LBB7_29:                               # %while.cond.outer.loopexit
                                        #   in Loop: Header=BB7_30 Depth=2
	movq	(%r14), %rax
	testq	%rax, %rax
	movq	%r14, %r8
	jne	.LBB7_30
	.align	16, 0x90
.LBB7_56:                               # %for.inc196
                                        #   in Loop: Header=BB7_25 Depth=1
	incq	%r10
	movq	%r10, %rcx
	andq	%rsi, %rcx
	incq	%r11
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB7_25
.LBB7_57:                               # %for.end198
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_58:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.cselib_invalidate_regno, %rcx
	movq	$968, %rsi              # imm = 0x3C8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp136:
	.size	cselib_invalidate_regno, .Ltmp136-cselib_invalidate_regno
	.cfi_endproc

	.align	16, 0x90
	.type	cselib_invalidate_rtx,@function
cselib_invalidate_rtx:                  # @cselib_invalidate_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp142:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp143:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp144:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp145:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp146:
	.cfi_def_cfa_offset 48
.Ltmp147:
	.cfi_offset %rbx, -40
.Ltmp148:
	.cfi_offset %r12, -32
.Ltmp149:
	.cfi_offset %r14, -24
.Ltmp150:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$cselib_invalidate_mem_1, %r14
	movabsq	$global_rtl+16, %r12
	jmp	.LBB8_1
	.align	16, 0x90
.LBB8_8:                                # %while.cond.backedge
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	(%rbx), %rbx
.LBB8_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$131, %rax
	jg	.LBB8_6
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB8_1 Depth=1
	leaq	-63(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB8_7
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$61, %rax
	jne	.LBB8_4
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB8_1 Depth=1
	movl	8(%rbx), %edi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	callq	cselib_invalidate_regno
	jmp	.LBB8_10
	.align	16, 0x90
.LBB8_6:                                # %while.cond
                                        #   in Loop: Header=BB8_1 Depth=1
	addq	$-132, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	ja	.LBB8_10
.LBB8_7:                                # %while.body
                                        #   in Loop: Header=BB8_1 Depth=1
	addq	$8, %rbx
	jmp	.LBB8_8
.LBB8_4:                                # %while.cond
                                        #   in Loop: Header=BB8_1 Depth=1
	cmpq	$66, %rax
	jne	.LBB8_10
# BB#5:                                 # %if.then21
                                        #   in Loop: Header=BB8_1 Depth=1
	movq	hash_table(%rip), %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	htab_traverse
	.align	16, 0x90
.LBB8_10:                               # %if.end22
                                        #   in Loop: Header=BB8_1 Depth=1
	movzbl	2(%rbx), %esi
	movq	%rbx, %rdi
	callq	push_operand
	testq	%r15, %rax
	movq	%r12, %rbx
	jne	.LBB8_8
# BB#11:                                # %if.end27
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp151:
	.size	cselib_invalidate_rtx, .Ltmp151-cselib_invalidate_rtx
	.cfi_endproc

	.globl	cselib_update_varray_sizes
	.align	16, 0x90
	.type	cselib_update_varray_sizes,@function
cselib_update_varray_sizes:             # @cselib_update_varray_sizes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 16
.Ltmp155:
	.cfi_offset %rbx, -16
	callq	max_reg_num
	movl	cselib_nregs(%rip), %ecx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	%rcx, %rdx
	je	.LBB9_2
# BB#1:                                 # %if.end
	movq	reg_values(%rip), %rdi
	movl	%eax, cselib_nregs(%rip)
	andq	%rax, %rbx
	movq	%rbx, %rsi
	callq	varray_grow
	movq	used_regs(%rip), %rdi
	movq	%rax, reg_values(%rip)
	movq	%rbx, %rsi
	callq	varray_grow
	movq	%rax, used_regs(%rip)
.LBB9_2:                                # %return
	popq	%rbx
	retq
.Ltmp156:
	.size	cselib_update_varray_sizes, .Ltmp156-cselib_update_varray_sizes
	.cfi_endproc

	.globl	cselib_init
	.align	16, 0x90
	.type	cselib_init,@function
cselib_init:                            # @cselib_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp161:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp162:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp163:
	.cfi_def_cfa_offset 32
.Ltmp164:
	.cfi_offset %rbx, -32
.Ltmp165:
	.cfi_offset %r14, -24
.Ltmp166:
	.cfi_offset %r15, -16
	cmpq	$0, callmem(%rip)
	jne	.LBB10_8
# BB#1:                                 # %if.then
	movabsq	$cselib_obstack, %rdi
	callq	gcc_obstack_init
	movq	cselib_obstack+32(%rip), %rax
	movq	cselib_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB10_3
# BB#2:                                 # %if.then2
	movabsq	$cselib_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	cselib_obstack+24(%rip), %rcx
.LBB10_3:                               # %if.end
	movq	cselib_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB10_5
# BB#4:                                 # %if.then9
	movzbl	cselib_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, cselib_obstack+80(%rip)
.LBB10_5:                               # %if.end10
	movslq	cselib_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, cselib_obstack+24(%rip)
	movq	cselib_obstack+32(%rip), %rbx
	movq	cselib_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB10_7
# BB#6:                                 # %if.then30
	movq	%rbx, cselib_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB10_7:                               # %if.end33
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rdx, cselib_obstack+16(%rip)
	movq	%rax, cselib_startobj(%rip)
	movq	$51, %rdi
	callq	gen_rtx_MEM
	movq	%rax, callmem(%rip)
	movabsq	$callmem, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
.LBB10_8:                               # %if.end37
	callq	max_reg_num
	movl	%eax, cselib_nregs(%rip)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	movabsq	$.L.str1, %rdx
	movq	$8, %rsi
	movq	%rax, %rdi
	callq	varray_init
	movl	cselib_nregs(%rip), %edi
	movq	%rax, reg_values(%rip)
	movabsq	$.L.str2, %rdx
	movq	$4, %rsi
	movq	$4, %rbx
	callq	varray_init
	movq	%rax, used_regs(%rip)
	movabsq	$get_value_hash, %rsi
	movabsq	$entry_and_rtx_equal_p, %rdx
	movq	$31, %rdi
	xorq	%rcx, %rcx
	xorq	%r15, %r15
	callq	htab_create
	movl	cselib_nregs(%rip), %ecx
	movq	%rax, hash_table(%rip)
	testq	%rcx, %rcx
	je	.LBB10_11
	.align	16, 0x90
.LBB10_9:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_values(%rip), %rax
	movq	$0, (%rax,%rbx,8)
	leaq	-3(%rbx), %rax
	andq	%r14, %rax
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB10_9
# BB#10:                                # %if.end.loopexit35.i.loopexit
	movq	hash_table(%rip), %rax
.LBB10_11:                              # %if.end.loopexit35.i
	movq	used_regs(%rip), %rcx
	movl	%r15d, max_value_regs(%rip)
	movq	$0, 8(%rcx)
	movq	%rax, %rdi
	callq	htab_empty
	movq	cselib_startobj(%rip), %rsi
	movq	cselib_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB10_14
# BB#12:                                # %if.end.loopexit35.i
	movq	cselib_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB10_14
# BB#13:                                # %if.then20.i
	movq	%rsi, cselib_obstack+16(%rip)
	movq	%rsi, cselib_obstack+24(%rip)
	jmp	.LBB10_15
.LBB10_14:                              # %if.else21.i
	movabsq	$cselib_obstack, %rdi
	callq	obstack_free
.LBB10_15:                              # %clear_table.exit
	movq	$0, empty_vals(%rip)
	movq	$0, empty_elt_lists(%rip)
	movq	$0, empty_elt_loc_lists(%rip)
	xorq	%rax, %rax
	movl	%eax, n_useless_values(%rip)
	movl	%eax, next_unknown_value(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp167:
	.size	cselib_init, .Ltmp167-cselib_init
	.cfi_endproc

	.align	16, 0x90
	.type	get_value_hash,@function
get_value_hash:                         # @get_value_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	retq
.Ltmp168:
	.size	get_value_hash, .Ltmp168-get_value_hash
	.cfi_endproc

	.align	16, 0x90
	.type	entry_and_rtx_equal_p,@function
entry_and_rtx_equal_p:                  # @entry_and_rtx_equal_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp175:
	.cfi_def_cfa_offset 32
.Ltmp176:
	.cfi_offset %rbx, -32
.Ltmp177:
	.cfi_offset %r14, -24
.Ltmp178:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movl	(%r14), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	je	.LBB12_16
# BB#1:                                 # %lor.lhs.false
	andq	$255, %rdx
	jne	.LBB12_3
# BB#2:                                 # %lor.lhs.false
	cmpq	$55, %rax
	je	.LBB12_16
.LBB12_3:                               # %if.end
	movq	8(%rdi), %rcx
	movzbl	2(%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB12_4
# BB#5:                                 # %if.end13
	cmpq	$58, %rax
	jne	.LBB12_9
# BB#6:                                 # %land.lhs.true17
	movq	8(%r14), %rax
	movl	(%rax), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$54, %rcx
	je	.LBB12_8
# BB#7:                                 # %land.lhs.true17
	movq	%r14, %rax
.LBB12_8:                               # %land.lhs.true17
	movq	%rax, %r14
.LBB12_9:                               # %if.end32
	movq	16(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB12_10
# BB#13:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB12_14:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	rtx_equal_for_cselib_p
	testq	%r15, %rax
	movq	$1, %rax
	jne	.LBB12_15
# BB#11:                                # %for.cond
                                        #   in Loop: Header=BB12_14 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB12_14
# BB#12:
	xorq	%rax, %rax
	jmp	.LBB12_15
.LBB12_4:
	xorq	%rax, %rax
	jmp	.LBB12_15
.LBB12_10:
	xorq	%rax, %rax
.LBB12_15:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB12_16:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.entry_and_rtx_equal_p, %rcx
	movq	$265, %rsi              # imm = 0x109
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp179:
	.size	entry_and_rtx_equal_p, .Ltmp179-entry_and_rtx_equal_p
	.cfi_endproc

	.globl	cselib_finish
	.align	16, 0x90
	.type	cselib_finish,@function
cselib_finish:                          # @cselib_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp181:
	.cfi_def_cfa_offset 16
	movq	used_regs(%rip), %rax
	cmpq	$0, 8(%rax)
	je	.LBB13_3
# BB#1:
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB13_2:                               # %for.body4.i
                                        # =>This Inner Loop Header: Depth=1
	movl	32(%rax,%rsi,4), %eax
	movq	reg_values(%rip), %rsi
	movq	$0, 32(%rsi,%rax,8)
	movq	used_regs(%rip), %rax
	movq	8(%rax), %rdi
	incq	%rdx
	movq	%rdx, %rsi
	andq	%rcx, %rsi
	cmpq	%rdi, %rsi
	jb	.LBB13_2
.LBB13_3:                               # %if.end.i
	xorq	%rcx, %rcx
	movl	%ecx, max_value_regs(%rip)
	movq	$0, 8(%rax)
	movq	hash_table(%rip), %rdi
	callq	htab_empty
	movq	cselib_startobj(%rip), %rsi
	movq	cselib_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB13_6
# BB#4:                                 # %if.end.i
	movq	cselib_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB13_6
# BB#5:                                 # %if.then20.i
	movq	%rsi, cselib_obstack+16(%rip)
	movq	%rsi, cselib_obstack+24(%rip)
	jmp	.LBB13_7
.LBB13_6:                               # %if.else21.i
	movabsq	$cselib_obstack, %rdi
	callq	obstack_free
.LBB13_7:                               # %clear_table.exit
	xorq	%rax, %rax
	movl	%eax, n_useless_values(%rip)
	movl	%eax, next_unknown_value(%rip)
	movq	reg_values(%rip), %rdi
	movq	$0, empty_vals(%rip)
	movq	$0, empty_elt_lists(%rip)
	movq	$0, empty_elt_loc_lists(%rip)
	testq	%rdi, %rdi
	je	.LBB13_9
# BB#8:                                 # %if.then
	callq	free
	movq	$0, reg_values(%rip)
.LBB13_9:                               # %do.body1
	movq	used_regs(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB13_11
# BB#10:                                # %if.then3
	callq	free
	movq	$0, used_regs(%rip)
.LBB13_11:                              # %do.end5
	movq	hash_table(%rip), %rdi
	popq	%rax
	jmp	htab_delete  # TAILCALL
.Ltmp182:
	.size	cselib_finish, .Ltmp182-cselib_finish
	.cfi_endproc

	.align	16, 0x90
	.type	discard_useless_locs,@function
discard_useless_locs:                   # @discard_useless_locs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp189:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp190:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp191:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp192:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp193:
	.cfi_def_cfa_offset 48
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
	movq	(%rdi), %r15
	movq	16(%r15), %r14
	testq	%r14, %r14
	je	.LBB14_9
# BB#1:
	addq	$16, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	movq	%r15, %r13
.LBB14_3:                               # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_4 Depth 2
	movq	%rcx, %rbx
	.align	16, 0x90
.LBB14_4:                               # %while.body
                                        #   Parent Loop BB14_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rbx), %rdi
	movq	$1, %rsi
	callq	references_value_p
	movq	(%rbx), %rcx
	testq	%r12, %rax
	je	.LBB14_2
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB14_4 Depth=2
	movq	%rcx, (%r13)
	movq	empty_elt_loc_lists(%rip), %rax
	movq	%rax, (%rbx)
	movq	%rbx, empty_elt_loc_lists(%rip)
	movq	(%r13), %rbx
	testq	%rbx, %rbx
	jne	.LBB14_4
	jmp	.LBB14_6
	.align	16, 0x90
.LBB14_2:                               # %while.cond.outer.loopexit
                                        #   in Loop: Header=BB14_3 Depth=1
	testq	%rcx, %rcx
	movq	%rbx, %r13
	jne	.LBB14_3
.LBB14_6:                               # %while.end
	testq	%r14, %r14
	je	.LBB14_9
# BB#7:                                 # %land.lhs.true
	cmpq	$0, (%r15)
	jne	.LBB14_9
# BB#8:                                 # %if.then7
	movl	n_useless_values(%rip), %eax
	incq	%rax
	movl	%eax, n_useless_values(%rip)
	movq	$1, %rax
	movb	%al, values_became_useless(%rip)
.LBB14_9:                               # %if.end8
	movq	$1, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp199:
	.size	discard_useless_locs, .Ltmp199-discard_useless_locs
	.cfi_endproc

	.align	16, 0x90
	.type	discard_useless_values,@function
discard_useless_values:                 # @discard_useless_values
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp202:
	.cfi_def_cfa_offset 16
.Ltmp203:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	movq	(%rax), %rbx
	cmpq	$0, 16(%rbx)
	jne	.LBB15_5
# BB#1:                                 # %if.then
	movq	hash_table(%rip), %rdi
	movq	%rax, %rsi
	callq	htab_clear_slot
	jmp	.LBB15_3
	.align	16, 0x90
.LBB15_2:                               # %while.body.i
                                        #   in Loop: Header=BB15_3 Depth=1
	movq	(%rax), %rcx
	movq	%rcx, 24(%rbx)
	movq	empty_elt_lists(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rax, empty_elt_lists(%rip)
.LBB15_3:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB15_2
# BB#4:                                 # %unchain_one_value.exit
	movq	empty_vals(%rip), %rax
	movq	%rax, 8(%rbx)
	movl	n_useless_values(%rip), %eax
	movq	%rbx, empty_vals(%rip)
	decq	%rax
	movl	%eax, n_useless_values(%rip)
.LBB15_5:                               # %if.end
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp204:
	.size	discard_useless_values, .Ltmp204-discard_useless_values
	.cfi_endproc

	.align	16, 0x90
	.type	cselib_invalidate_mem_1,@function
cselib_invalidate_mem_1:                # @cselib_invalidate_mem_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp211:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp212:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp213:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp214:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp215:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp216:
	.cfi_def_cfa_offset 64
.Ltmp217:
	.cfi_offset %rbx, -48
.Ltmp218:
	.cfi_offset %r12, -40
.Ltmp219:
	.cfi_offset %r13, -32
.Ltmp220:
	.cfi_offset %r14, -24
.Ltmp221:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	movq	16(%rbx), %rax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB16_13
# BB#1:
	leaq	16(%rbx), %r12
	movq	%r12, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rcx
.LBB16_2:                               # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_3 Depth 2
                                        #       Child Loop BB16_8 Depth 3
	movq	%rcx, %rax
	.align	16, 0x90
.LBB16_3:                               # %while.body
                                        #   Parent Loop BB16_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_8 Depth 3
	movq	8(%rax), %r13
	movzbl	(%r13), %ecx
	cmpq	$66, %rcx
	jne	.LBB16_6
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB16_3 Depth=2
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	cselib_mem_conflict_p
	testq	%r15, %rax
	je	.LBB16_5
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB16_3 Depth=2
	movq	8(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	cselib_lookup
	addq	$24, %rax
	.align	16, 0x90
.LBB16_8:                               # %for.cond
                                        #   Parent Loop BB16_2 Depth=1
                                        #     Parent Loop BB16_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rax, %rcx
	movq	(%rcx), %rax
	movq	8(%rax), %rdx
	cmpq	%rbx, %rdx
	jne	.LBB16_8
# BB#9:                                 # %if.then8
                                        #   in Loop: Header=BB16_3 Depth=2
	movq	(%rax), %rdx
	movq	%rdx, (%rcx)
	movq	empty_elt_lists(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rax, empty_elt_lists(%rip)
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movq	%rcx, (%r12)
	movq	empty_elt_loc_lists(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rax, empty_elt_loc_lists(%rip)
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB16_3
	jmp	.LBB16_10
	.align	16, 0x90
.LBB16_5:                               # %lor.lhs.false.if.then_crit_edge
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	(%r12), %rax
.LBB16_6:                               # %if.then
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	%rax, %r12
	jne	.LBB16_2
.LBB16_10:                              # %while.end
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB16_13
# BB#11:                                # %land.lhs.true
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB16_13
# BB#12:                                # %if.then15
	movl	n_useless_values(%rip), %eax
	incq	%rax
	movl	%eax, n_useless_values(%rip)
.LBB16_13:                              # %if.end16
	movq	$1, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp222:
	.size	cselib_invalidate_mem_1, .Ltmp222-cselib_invalidate_mem_1
	.cfi_endproc

	.align	16, 0x90
	.type	cselib_mem_conflict_p,@function
cselib_mem_conflict_p:                  # @cselib_mem_conflict_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp229:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp230:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp231:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp232:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp233:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp234:
	.cfi_def_cfa_offset 80
.Ltmp235:
	.cfi_offset %rbx, -48
.Ltmp236:
	.cfi_offset %r12, -40
.Ltmp237:
	.cfi_offset %r13, -32
.Ltmp238:
	.cfi_offset %r14, -24
.Ltmp239:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movl	(%r15), %eax
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	leaq	-54(%r12), %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$15, %rcx
	ja	.LBB17_7
# BB#1:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$57783, %rdx            # imm = 0xE1B7
	je	.LBB17_3
# BB#2:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB17_18
.LBB17_3:                               # %entry
	cmpq	$12, %rcx
	jne	.LBB17_7
# BB#4:                                 # %sw.bb1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	je	.LBB17_18
# BB#5:                                 # %sw.bb1
	andq	$16711680, %rax         # imm = 0xFF0000
	andq	%r14, %rax
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB17_18
# BB#6:                                 # %lor.lhs.false8
	movq	%r15, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	anti_dependence
	testq	%r14, %rax
	jne	.LBB17_18
.LBB17_7:                               # %sw.epilog
	movzbl	rtx_length(%r12), %r13d
	testq	%r13, %r13
	je	.LBB17_8
# BB#9:                                 # %for.body.lr.ph
	movq	rtx_format(,%r12,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB17_10:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_17 Depth 2
	movq	%r13, %r12
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r12), %eax
	leaq	-1(%r12), %r13
	cmpq	$69, %rax
	jne	.LBB17_11
# BB#14:                                # %for.cond31.preheader
                                        #   in Loop: Header=BB17_10 Depth=1
	movq	(%r15,%r12,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	movq	$1, %rbx
	jle	.LBB17_12
	.align	16, 0x90
.LBB17_17:                              # %for.body37
                                        #   Parent Loop BB17_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cselib_mem_conflict_p
	testq	%r14, %rax
	jne	.LBB17_18
# BB#16:                                # %for.cond31
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	(%r15,%r12,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB17_17
	jmp	.LBB17_12
	.align	16, 0x90
.LBB17_11:                              # %for.body
                                        #   in Loop: Header=BB17_10 Depth=1
	cmpq	$101, %rax
	jne	.LBB17_12
# BB#15:                                # %if.then18
                                        #   in Loop: Header=BB17_10 Depth=1
	movq	(%r15,%r12,8), %rsi
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cselib_mem_conflict_p
	testq	%r14, %rax
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	jne	.LBB17_18
.LBB17_12:                              # %for.cond.backedge
                                        #   in Loop: Header=BB17_10 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB17_10
# BB#13:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB17_18
.LBB17_8:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB17_18:                              # %return
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp240:
	.size	cselib_mem_conflict_p, .Ltmp240-cselib_mem_conflict_p
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"cselib.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.rtx_equal_for_cselib_p,@object # @__FUNCTION__.rtx_equal_for_cselib_p
.L__FUNCTION__.rtx_equal_for_cselib_p:
	.asciz	"rtx_equal_for_cselib_p"
	.size	.L__FUNCTION__.rtx_equal_for_cselib_p, 23

	.type	reg_values,@object      # @reg_values
	.local	reg_values
	.comm	reg_values,8,8
	.type	.L__FUNCTION__.cselib_subst_to_values,@object # @__FUNCTION__.cselib_subst_to_values
.L__FUNCTION__.cselib_subst_to_values:
	.asciz	"cselib_subst_to_values"
	.size	.L__FUNCTION__.cselib_subst_to_values, 23

	.type	next_unknown_value,@object # @next_unknown_value
	.local	next_unknown_value
	.comm	next_unknown_value,4,4
	.type	max_value_regs,@object  # @max_value_regs
	.local	max_value_regs
	.comm	max_value_regs,4,4
	.type	used_regs,@object       # @used_regs
	.local	used_regs
	.comm	used_regs,8,8
	.type	hash_table,@object      # @hash_table
	.local	hash_table
	.comm	hash_table,8,8
	.type	cselib_current_insn,@object # @cselib_current_insn
	.local	cselib_current_insn
	.comm	cselib_current_insn,8,8
	.type	callmem,@object         # @callmem
	.local	callmem
	.comm	callmem,8,8
	.type	n_useless_values,@object # @n_useless_values
	.local	n_useless_values
	.comm	n_useless_values,4,4
	.type	cselib_nregs,@object    # @cselib_nregs
	.local	cselib_nregs
	.comm	cselib_nregs,4,4
	.type	cselib_obstack,@object  # @cselib_obstack
	.local	cselib_obstack
	.comm	cselib_obstack,88,8
	.type	cselib_startobj,@object # @cselib_startobj
	.local	cselib_startobj
	.comm	cselib_startobj,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"reg_values"
	.size	.L.str1, 11

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"used_regs"
	.size	.L.str2, 10

	.type	.L__FUNCTION__.entry_and_rtx_equal_p,@object # @__FUNCTION__.entry_and_rtx_equal_p
.L__FUNCTION__.entry_and_rtx_equal_p:
	.asciz	"entry_and_rtx_equal_p"
	.size	.L__FUNCTION__.entry_and_rtx_equal_p, 22

	.type	values_became_useless,@object # @values_became_useless
	.local	values_became_useless
	.comm	values_became_useless,1,1
	.type	.L__FUNCTION__.remove_useless_values,@object # @__FUNCTION__.remove_useless_values
.L__FUNCTION__.remove_useless_values:
	.asciz	"remove_useless_values"
	.size	.L__FUNCTION__.remove_useless_values, 22

	.type	empty_vals,@object      # @empty_vals
	.local	empty_vals
	.comm	empty_vals,8,8
	.type	empty_elt_lists,@object # @empty_elt_lists
	.local	empty_elt_lists
	.comm	empty_elt_lists,8,8
	.type	empty_elt_loc_lists,@object # @empty_elt_loc_lists
	.local	empty_elt_loc_lists
	.comm	empty_elt_loc_lists,8,8
	.type	.L__FUNCTION__.cselib_invalidate_regno,@object # @__FUNCTION__.cselib_invalidate_regno
.L__FUNCTION__.cselib_invalidate_regno:
	.asciz	"cselib_invalidate_regno"
	.size	.L__FUNCTION__.cselib_invalidate_regno, 24

	.type	.L__FUNCTION__.wrap_constant,@object # @__FUNCTION__.wrap_constant
.L__FUNCTION__.wrap_constant:
	.asciz	"wrap_constant"
	.size	.L__FUNCTION__.wrap_constant, 14

	.type	.L__FUNCTION__.hash_rtx,@object # @__FUNCTION__.hash_rtx
.L__FUNCTION__.hash_rtx:
	.asciz	"hash_rtx"
	.size	.L__FUNCTION__.hash_rtx, 9

	.type	.L__FUNCTION__.new_cselib_val,@object # @__FUNCTION__.new_cselib_val
.L__FUNCTION__.new_cselib_val:
	.asciz	"new_cselib_val"
	.size	.L__FUNCTION__.new_cselib_val, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
