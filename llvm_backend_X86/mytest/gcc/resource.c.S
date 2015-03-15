	.file	"resource.c.bc"
	.text
	.globl	mark_referenced_resources
	.align	16, 0x90
	.type	mark_referenced_resources,@function
mark_referenced_resources:              # @mark_referenced_resources
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
	subq	$48, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 96
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
	movq	%rdx, %rbx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB0_1
.LBB0_60:                               # %sw.bb217
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	8(%r14), %rax
	movq	8(%rax), %rdi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	mark_referenced_resources
	movq	16(%r14), %rdi
	.align	16, 0x90
.LBB0_1:                                # %tailrecurse.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_2 Depth 2
                                        #       Child Loop BB0_74 Depth 3
                                        #       Child Loop BB0_87 Depth 3
                                        #       Child Loop BB0_81 Depth 3
                                        #         Child Loop BB0_82 Depth 4
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %rax
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_90:                               # %sw.bb396
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
.LBB0_2:                                # %tailrecurse
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_74 Depth 3
                                        #       Child Loop BB0_87 Depth 3
                                        #       Child Loop BB0_81 Depth 3
                                        #         Child Loop BB0_82 Depth 4
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	leaq	-32(%rcx), %rdx
	andq	%r15, %rdx
	cmpq	$37, %rdx
	ja	.LBB0_51
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB0_2 Depth=2
	jmpq	*.LJTI0_0(,%rdx,8)
.LBB0_68:                               # %sw.bb270
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB0_90
# BB#69:                                # %if.then272
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	16(%r14), %rax
	movq	24(%rax), %rax
	xorq	%rcx, %rcx
	cmpq	%r14, %rax
	movabsq	$0, %r13
	movabsq	$0, %r12
	movq	32(%rsp), %rbx          # 8-byte Reload
	je	.LBB0_72
# BB#70:                                # %if.then282
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	32(%rax), %r13
	movzbl	(%r13), %eax
	cmpq	$24, %rax
	jne	.LBB0_95
# BB#71:                                #   in Loop: Header=BB0_2 Depth=2
	movq	8(%r13), %rax
	movl	(%rax), %r12d
.LBB0_72:                               # %if.end299
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	$1, %rax
	movb	%al, (%rbx)
	movq	8(%rbx), %rdx
	movq	%rdx, %rax
	orq	$128, %rax
	movq	%rax, 8(%rbx)
	movl	frame_pointer_needed(%rip), %esi
	testq	%rsi, %rsi
	je	.LBB0_74
# BB#73:                                # %if.then304
                                        #   in Loop: Header=BB0_2 Depth=2
	orq	$1048768, %rdx          # imm = 0x1000C0
	movq	%rdx, 8(%rbx)
	movq	%rdx, %rax
	.align	16, 0x90
.LBB0_74:                               # %for.body313
                                        #   Parent Loop BB0_1 Depth=1
                                        #     Parent Loop BB0_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	global_regs(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB0_76
# BB#75:                                # %if.then317
                                        #   in Loop: Header=BB0_74 Depth=3
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdx, %rax
	movq	%rax, 8(%rbx)
.LBB0_76:                               # %for.inc323
                                        #   in Loop: Header=BB0_74 Depth=3
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB0_74
# BB#77:                                # %for.end325
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_79
# BB#78:                                # %if.then328
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	$-1, 8(%rbx)
.LBB0_79:                               # %if.end330
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	64(%r14), %r14
	testq	%r14, %r14
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r9, %r15
	je	.LBB0_90
# BB#80:                                # %for.body336.lr.ph
                                        #   in Loop: Header=BB0_2 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_87
	.align	16, 0x90
.LBB0_81:                               # %for.body336.us
                                        #   Parent Loop BB0_1 Depth=1
                                        #     Parent Loop BB0_2 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_82 Depth 4
	movq	8(%r14), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	movq	$2, %rbx
	jne	.LBB0_86
	.align	16, 0x90
.LBB0_82:                               # %for.body348.us
                                        #   Parent Loop BB0_1 Depth=1
                                        #     Parent Loop BB0_2 Depth=2
                                        #       Parent Loop BB0_81 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%r13), %rax
	movq	(%rax,%rbx,8), %rax
	movq	32(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB0_84
# BB#83:                                # %land.lhs.true362.us
                                        #   in Loop: Header=BB0_82 Depth=4
	movq	8(%r14), %rcx
	movq	8(%rax), %rdi
	movq	8(%rcx), %rsi
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB0_86
.LBB0_84:                               # %for.inc376.us
                                        #   in Loop: Header=BB0_82 Depth=4
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB0_82
# BB#85:                                # %if.then381.us
                                        #   in Loop: Header=BB0_81 Depth=3
	movq	8(%r14), %rax
	movq	8(%rax), %rdi
	xorq	%rdx, %rdx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rbx
	callq	mark_referenced_resources
	movq	%rbx, %r15
.LBB0_86:                               # %for.inc390.us
                                        #   in Loop: Header=BB0_81 Depth=3
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB0_81
	jmp	.LBB0_90
	.align	16, 0x90
.LBB0_87:                               # %for.body336
                                        #   Parent Loop BB0_1 Depth=1
                                        #     Parent Loop BB0_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB0_89
# BB#88:                                # %if.then381
                                        #   in Loop: Header=BB0_87 Depth=3
	movq	8(%rax), %rdi
	xorq	%rdx, %rdx
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rbx
	callq	mark_referenced_resources
	movq	%rbx, %r15
.LBB0_89:                               # %for.inc390
                                        #   in Loop: Header=BB0_87 Depth=3
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB0_87
	jmp	.LBB0_90
.LBB0_61:                               # %sw.bb227
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	16(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	mark_referenced_resources
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %eax
	leaq	-132(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB0_64
# BB#62:                                # %sw.bb227
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$63, %rax
	jne	.LBB0_63
# BB#65:                                # %if.then254
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%rbx), %rbx
	jmp	.LBB0_66
.LBB0_4:                                # %sw.bb1
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	xorq	%rbx, %rbx
	cmpq	$61, %rax
	jne	.LBB0_1
	jmp	.LBB0_5
.LBB0_44:                               # %sw.bb173
                                        #   in Loop: Header=BB0_1 Depth=1
	testq	$67108864, %rax         # imm = 0x4000000
	jne	.LBB0_45
# BB#46:                                # %if.else179
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	$1, %rax
	movb	%al, (%rsi)
	jmp	.LBB0_47
.LBB0_45:                               # %if.then178
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	$1, %rax
	movb	%al, 1(%rsi)
.LBB0_47:                               # %if.end180
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movl	(%rdi), %eax
	movzbl	2(%rsi), %edx
	movq	$27, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rax, %rdx
	movb	%dl, 2(%rsi)
	movq	8(%rdi), %rdi
	xorq	%rbx, %rbx
	jmp	.LBB0_1
.LBB0_63:                               # %sw.bb227
                                        #   in Loop: Header=BB0_1 Depth=1
	cmpq	$64, %rax
	jne	.LBB0_66
.LBB0_64:                               # %if.then248
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %r14
	callq	mark_referenced_resources
	movq	%r14, %r15
.LBB0_66:                               # %if.end259
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB0_93
# BB#67:                                # %if.then264
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%rbx), %rdi
	xorq	%rbx, %rbx
	jmp	.LBB0_1
.LBB0_49:                               # %sw.bb191
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 2(%rcx)
	jmp	.LBB0_93
.LBB0_57:                               # %sw.bb195
	movq	32(%rsp), %rsi          # 8-byte Reload
	movzbl	2(%rsi), %edx
	movq	$27, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rax, %rdx
	movb	%dl, 2(%rsi)
	movq	%rsi, %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB0_93
# BB#58:
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_59:                               # %for.body208
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%rbx,8), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	mark_referenced_resources
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB0_59
	jmp	.LBB0_93
.LBB0_50:                               # %sw.bb193
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 2(%rcx)
	movq	$52, %rcx
.LBB0_51:                               # %sw.epilog
	movzbl	rtx_length(%rcx), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_93
# BB#52:                                # %for.body408.lr.ph
	movq	rtx_format(,%rcx,8), %rcx
	xorq	%r13, %r13
	.align	16, 0x90
.LBB0_53:                               # %for.body408
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_56 Depth 2
	movsbq	(%rcx), %rax
	incq	%rcx
	andq	%r15, %rax
	cmpq	$101, %rax
	jne	.LBB0_54
# BB#91:                                # %sw.bb410
                                        #   in Loop: Header=BB0_53 Depth=1
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	8(%r15,%r13,8), %rdi
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdx
	callq	mark_referenced_resources
	jmp	.LBB0_92
	.align	16, 0x90
.LBB0_54:                               # %for.body408
                                        #   in Loop: Header=BB0_53 Depth=1
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	24(%rsp), %r12          # 8-byte Reload
	jne	.LBB0_92
# BB#55:                                # %for.cond416.preheader
                                        #   in Loop: Header=BB0_53 Depth=1
	movq	8(%r15,%r13,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	jle	.LBB0_92
	.align	16, 0x90
.LBB0_56:                               # %for.body424
                                        #   Parent Loop BB0_53 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rbx,8), %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	mark_referenced_resources
	movq	8(%r15,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB0_56
	.align	16, 0x90
.LBB0_92:                               # %for.inc436
                                        #   in Loop: Header=BB0_53 Depth=1
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%r14, 32(%rsp)          # 8-byte Spill
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rcx          # 8-byte Reload
	jl	.LBB0_53
	jmp	.LBB0_93
.LBB0_25:                               # %sw.bb69
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	8(%rcx), %esi
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rsi, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB0_29
# BB#26:                                # %sw.bb69
	leaq	-21(%rsi), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_29
# BB#27:                                # %sw.bb69
	leaq	-45(%rsi), %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_29
# BB#28:                                # %sw.bb69
	leaq	-29(%rsi), %rcx
	andq	%r15, %rcx
	cmpq	$7, %rcx
	jbe	.LBB0_29
# BB#34:                                # %cond.false115
	cmpq	$24, %rax
	jne	.LBB0_35
# BB#37:                                # %cond.true131
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB0_39
.LBB0_48:                               # %sw.bb190
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 3(%rcx)
	jmp	.LBB0_93
.LBB0_29:                               # %cond.true97
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB0_39
# BB#30:                                # %lor.rhs105
	cmpq	$6, %rcx
	jne	.LBB0_31
# BB#32:                                # %lor.rhs105
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB0_39
.LBB0_35:                               # %cond.false115
	cmpq	$18, %rax
	jne	.LBB0_38
# BB#36:                                # %cond.true121
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB0_39
.LBB0_38:                               # %cond.false135
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	jmp	.LBB0_39
.LBB0_31:
	movabsq	$0, %rax
	incq	%rax
.LBB0_39:                               # %cond.end155
	addq	%rsi, %rax
	andq	%r15, %rax
	cmpq	$53, %rax
	ja	.LBB0_94
# BB#40:                                # %for.cond162.preheader
	cmpq	%rax, %rsi
	jae	.LBB0_93
# BB#41:                                # %for.body165.lr.ph
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	8(%r8), %rdx
	movq	%rsi, %rdi
	.align	16, 0x90
.LBB0_42:                               # %for.body165
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rbx
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	orq	%rbx, %rdx
	incq	%rdi
	movq	%rdi, %rcx
	andq	%r15, %rcx
	incq	%rsi
	cmpq	%rax, %rcx
	jne	.LBB0_42
	jmp	.LBB0_43
.LBB0_5:                                # %if.else
	movq	%r14, %rdi
	movq	%r15, %rbx
	callq	subreg_regno
	movq	%rax, %rsi
	movzbl	2(%r14), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	je	.LBB0_9
# BB#6:                                 # %if.else
	leaq	-21(%rsi), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_9
# BB#7:                                 # %if.else
	leaq	-45(%rsi), %rcx
	andq	%rbx, %rcx
	cmpq	$8, %rcx
	jb	.LBB0_9
# BB#8:                                 # %if.else
	movq	%rsi, %rcx
	addq	$-29, %rcx
	andq	%rbx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB0_9
# BB#14:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB0_15
# BB#17:                                # %cond.true41
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB0_19
.LBB0_9:                                # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB0_19
# BB#10:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB0_11
# BB#12:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB0_19
.LBB0_15:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB0_18
# BB#16:                                # %cond.true34
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB0_19
.LBB0_11:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB0_19
.LBB0_18:                               # %cond.false45
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB0_19:                               # %cond.end60
	addq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$53, %rax
	ja	.LBB0_23
# BB#20:                                # %for.cond.preheader
	movq	%rsi, %rcx
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	jae	.LBB0_93
# BB#21:                                # %for.body.lr.ph
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	8(%r8), %rdx
.LBB0_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	incq	%rsi
	movq	%rsi, %rdi
	andq	%rbx, %rdi
	incq	%rcx
	cmpq	%rax, %rdi
	jne	.LBB0_22
.LBB0_43:                               # %for.cond162.for.end438.loopexit589_crit_edge
	movq	%rdx, 8(%r8)
.LBB0_93:                               # %for.end438
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_95:                               # %if.then297
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_referenced_resources, %rcx
	movq	$338, %rsi              # imm = 0x152
	jmp	.LBB0_24
.LBB0_94:                               # %if.then160
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_referenced_resources, %rcx
	movq	$244, %rsi
	jmp	.LBB0_24
.LBB0_23:                               # %if.then65
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_referenced_resources, %rcx
	movq	$231, %rsi
.LBB0_24:                               # %if.then65
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	mark_referenced_resources, .Ltmp17-mark_referenced_resources
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_90
	.quad	.LBB0_90
	.quad	.LBB0_68
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_49
	.quad	.LBB0_57
	.quad	.LBB0_51
	.quad	.LBB0_49
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_61
	.quad	.LBB0_51
	.quad	.LBB0_93
	.quad	.LBB0_60
	.quad	.LBB0_51
	.quad	.LBB0_50
	.quad	.LBB0_51
	.quad	.LBB0_93
	.quad	.LBB0_93
	.quad	.LBB0_93
	.quad	.LBB0_51
	.quad	.LBB0_93
	.quad	.LBB0_93
	.quad	.LBB0_51
	.quad	.LBB0_25
	.quad	.LBB0_51
	.quad	.LBB0_4
	.quad	.LBB0_51
	.quad	.LBB0_51
	.quad	.LBB0_44
	.quad	.LBB0_93
	.quad	.LBB0_93
	.quad	.LBB0_48

	.text
	.globl	mark_set_resources
	.align	16, 0x90
	.type	mark_set_resources,@function
mark_set_resources:                     # @mark_set_resources
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
	subq	$32, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 80
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
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, %r13
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_43:                               # %if.then141
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rbx), %rdi
	xorq	%rcx, %rcx
	movq	%r13, %r15
	movq	%r15, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%r8, %r14
	callq	mark_set_resources
	movq	16(%rbx), %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%r15, %r13
	callq	mark_set_resources
	movq	%r14, %r8
	xorq	%rcx, %rcx
	movq	24(%rbx), %rdi
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB1_1:                                # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_19 Depth 2
                                        #     Child Loop BB1_2 Depth 2
                                        #       Child Loop BB1_14 Depth 3
                                        #       Child Loop BB1_6 Depth 3
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movl	(%rdi), %eax
	movq	%rcx, %r12
	andq	%r8, %r12
	cmpq	$1, %r12
	movq	%rdi, %rbx
	jne	.LBB1_19
.LBB1_2:                                # %restart.us
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_14 Depth 3
                                        #       Child Loop BB1_6 Depth 3
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	$1, %r15
	cmpq	$131, %rsi
	jg	.LBB1_40
# BB#3:                                 # %restart.us
                                        #   in Loop: Header=BB1_2 Depth=2
	leaq	-24(%rsi), %rdx
	andq	%r8, %rdx
	cmpq	$77, %rdx
	ja	.LBB1_98
# BB#4:                                 # %restart.us
                                        #   in Loop: Header=BB1_2 Depth=2
	jmpq	*.LJTI1_1(,%rdx,8)
.LBB1_13:                               # %if.then3.us
                                        #   in Loop: Header=BB1_2 Depth=2
	movq	%r13, %r14
	movq	$1, %rax
	movb	%al, (%r14)
	movb	%al, 3(%r14)
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB1_14:                               # %for.body.us
                                        #   Parent Loop BB1_1 Depth=1
                                        #     Parent Loop BB1_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	call_used_regs(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB1_16
# BB#15:                                # %lor.lhs.false.us
                                        #   in Loop: Header=BB1_14 Depth=3
	movzbl	global_regs(%rcx), %eax
	testq	%rax, %rax
	je	.LBB1_17
.LBB1_16:                               # %if.then11.us
                                        #   in Loop: Header=BB1_14 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, 8(%r14)
.LBB1_17:                               # %for.inc.us
                                        #   in Loop: Header=BB1_14 Depth=3
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB1_14
# BB#5:                                 # %for.end.us
                                        #   in Loop: Header=BB1_2 Depth=2
	movq	64(%rbx), %r15
	jmp	.LBB1_6
	.align	16, 0x90
.LBB1_9:                                # %for.inc32.us
                                        #   in Loop: Header=BB1_6 Depth=3
	movq	16(%r15), %r15
.LBB1_6:                                # %for.end.us
                                        #   Parent Loop BB1_1 Depth=1
                                        #     Parent Loop BB1_2 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r15, %r15
	je	.LBB1_10
# BB#7:                                 # %for.body16.us
                                        #   in Loop: Header=BB1_6 Depth=3
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	jne	.LBB1_9
# BB#8:                                 # %if.then24.us
                                        #   in Loop: Header=BB1_6 Depth=3
	movq	8(%rax), %rdi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	callq	mark_set_resources
	jmp	.LBB1_9
.LBB1_10:                               # %for.end36.us
                                        #   in Loop: Header=BB1_2 Depth=2
	movq	%r14, %r13
	movq	$28, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	je	.LBB1_12
# BB#11:                                # %if.then38.us
                                        #   in Loop: Header=BB1_2 Depth=2
	movq	$-1, 8(%r13)
	.align	16, 0x90
.LBB1_12:                               # %sw.bb42.us
                                        #   in Loop: Header=BB1_2 Depth=2
	movq	32(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$48, %rdx
	jne	.LBB1_2
	jmp	.LBB1_105
	.align	16, 0x90
.LBB1_19:                               # %restart
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$131, %rsi
	jg	.LBB1_97
# BB#20:                                # %restart
                                        #   in Loop: Header=BB1_19 Depth=2
	leaq	-24(%rsi), %rdx
	andq	%r8, %rdx
	cmpq	$77, %rdx
	ja	.LBB1_21
# BB#22:                                # %restart
                                        #   in Loop: Header=BB1_19 Depth=2
	movq	%rcx, %r15
	jmpq	*.LJTI1_0(,%rdx,8)
.LBB1_18:                               # %sw.bb42
                                        #   in Loop: Header=BB1_19 Depth=2
	movq	32(%rbx), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$48, %rdx
	jne	.LBB1_19
	jmp	.LBB1_105
	.align	16, 0x90
.LBB1_40:                               # %restart.us
                                        #   in Loop: Header=BB1_1 Depth=1
	leaq	-132(%rsi), %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	jb	.LBB1_41
	jmp	.LBB1_98
	.align	16, 0x90
.LBB1_97:                               # %restart
                                        #   in Loop: Header=BB1_1 Depth=1
	leaq	-132(%rsi), %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	movq	%rcx, %r15
	jae	.LBB1_98
.LBB1_41:                               # %sw.bb136
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$2, %r12
	jne	.LBB1_43
# BB#42:                                # %sw.bb136
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB1_43
	jmp	.LBB1_105
.LBB1_38:                               # %sw.bb76
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rbx), %rdi
	xorq	%rcx, %rcx
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_1
.LBB1_39:                               # %sw.bb120
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rbx), %rdi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	%r13, %r15
	movq	%r15, %rsi
	movq	%r8, %r14
	callq	mark_set_resources
	movq	16(%rbx), %rax
	movq	8(%rax), %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%r15, %r13
	callq	mark_set_resources
	movq	%r14, %r8
	movq	16(%rbx), %rax
	movq	16(%rax), %rdi
	xorq	%rcx, %rcx
	jmp	.LBB1_1
.LBB1_31:                               # %sw.bb56
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rbx), %rdi
	cmpq	$1, %r12
	jne	.LBB1_33
# BB#32:                                # %lor.end.thread
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%r13, %rsi
	movq	%r8, %r14
	callq	mark_set_resources
	movq	%r14, %r8
	jmp	.LBB1_36
.LBB1_47:                               # %sw.bb172
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB1_105
# BB#48:                                # %if.then174
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB1_1
	jmp	.LBB1_49
.LBB1_90:                               # %sw.bb389
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$2, %r12
	jne	.LBB1_46
# BB#91:                                # %sw.bb389
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB1_46
	jmp	.LBB1_92
.LBB1_44:                               # %sw.bb152
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB1_46
# BB#45:                                # %if.then154
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %r15
	movq	$1, %rax
	movb	%al, (%r15)
	movl	(%rbx), %eax
	movzbl	1(%r15), %edx
	movq	$26, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rax, %rdx
	movb	%dl, 1(%r15)
	movl	(%rbx), %eax
	movzbl	2(%r15), %edx
	movq	$27, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rax, %rdx
	movb	%dl, 2(%r15)
                                        # kill: R13<def> R15<kill>
.LBB1_46:                               # %if.end168
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	8(%rbx), %rdi
	jmp	.LBB1_37
.LBB1_33:                               # %lor.end
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r8, %r14
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$50, %rax
	movabsq	$0, %rdx
	movq	%r13, %rsi
	je	.LBB1_35
# BB#34:                                # %lor.end
                                        #   in Loop: Header=BB1_1 Depth=1
	movabsq	$1, %rdx
.LBB1_35:                               # %lor.end
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rsi, %r13
	callq	mark_set_resources
	cmpq	$2, %r12
	movq	%r14, %r8
	je	.LBB1_105
.LBB1_36:                               #   in Loop: Header=BB1_1 Depth=1
	addq	$16, %rbx
	movq	(%rbx), %rdi
.LBB1_37:                               # %tailrecurse
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	xorq	%rcx, %rcx
	jmp	.LBB1_1
.LBB1_21:
	movq	%rcx, %r15
	jmp	.LBB1_98
.LBB1_92:                               # %sw.bb399
	movq	$1, %rax
	movb	%al, 2(%r13)
	jmp	.LBB1_105
.LBB1_23:                               # %for.cond81.preheader
	movq	8(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB1_105
# BB#24:
	movq	$1, %r14
	.align	16, 0x90
.LBB1_25:                               # %for.body86
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movzbl	3(%rcx), %ecx
	movq	(%rax,%r14,8), %rdi
	testq	$4, %rcx
	je	.LBB1_27
# BB#26:                                # %land.lhs.true94
                                        #   in Loop: Header=BB1_25 Depth=1
	movzbl	3(%rdi), %ecx
	testq	$16, %rcx
	jne	.LBB1_28
.LBB1_27:                               # %if.then105
                                        #   in Loop: Header=BB1_25 Depth=1
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	movq	%r15, %rcx
	callq	mark_set_resources
	movq	8(%rbx), %rax
.LBB1_28:                               # %for.inc113
                                        #   in Loop: Header=BB1_25 Depth=1
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r14
	cmpq	%rdx, %rsi
	jl	.LBB1_25
	jmp	.LBB1_105
.LBB1_94:                               # %sw.bb403
	movq	%r13, %r15
	movzbl	2(%r15), %edx
	movq	$27, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rax, %rdx
	movb	%dl, 2(%r15)
	movq	%r15, %r14
	movq	32(%rbx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	jle	.LBB1_105
# BB#95:
	movq	$1, %r15
	.align	16, 0x90
.LBB1_96:                               # %for.body418
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax,%r15,8), %rdi
	xorq	%rcx, %rcx
	movq	%r14, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	mark_set_resources
	movq	32(%rbx), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB1_96
	jmp	.LBB1_105
.LBB1_93:                               # %sw.bb401
	movq	$1, %rax
	movb	%al, 2(%r13)
	movq	$52, %rsi
.LBB1_98:                               # %sw.epilog
	movzbl	rtx_length(%rsi), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rax, %rax
	je	.LBB1_105
# BB#99:                                # %for.body436.lr.ph
	movq	rtx_format(,%rsi,8), %rcx
	xorq	%r12, %r12
	.align	16, 0x90
.LBB1_100:                              # %for.body436
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_103 Depth 2
	movsbq	(%rcx), %rax
	incq	%rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	andq	%r8, %rax
	cmpq	$101, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	jne	.LBB1_101
# BB#107:                               # %sw.bb438
                                        #   in Loop: Header=BB1_100 Depth=1
	movq	8(%rbx,%r12,8), %rdi
	movq	%r13, %r14
	movq	%r14, %rsi
	movq	%rcx, %r13
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	mark_set_resources
	movq	%r15, %r8
	movq	%r14, %rdx
	jmp	.LBB1_104
	.align	16, 0x90
.LBB1_101:                              # %for.body436
                                        #   in Loop: Header=BB1_100 Depth=1
	cmpq	$69, %rax
	movq	%r13, %rdx
	movq	%rcx, %r13
	movq	%r15, %r8
	jne	.LBB1_104
# BB#102:                               # %for.cond444.preheader
                                        #   in Loop: Header=BB1_100 Depth=1
	movq	8(%rbx,%r12,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r15
	jle	.LBB1_104
	.align	16, 0x90
.LBB1_103:                              # %for.body452
                                        #   Parent Loop BB1_100 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r15,8), %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	movq	%r8, %rcx
	movq	%r12, %r14
	movq	%r8, %r12
	callq	mark_set_resources
	movq	%r12, %r8
	movq	%r14, %r12
	movq	8(%rbx,%r12,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	jl	.LBB1_103
	.align	16, 0x90
.LBB1_104:                              # %for.inc464
                                        #   in Loop: Header=BB1_100 Depth=1
	movq	%r8, %r15
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	jl	.LBB1_100
	jmp	.LBB1_105
.LBB1_70:                               # %sw.bb281
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%r8, %rcx
	je	.LBB1_105
# BB#71:                                # %if.then283
	movl	8(%rbx), %esi
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rsi, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB1_75
# BB#72:                                # %if.then283
	leaq	-21(%rsi), %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jb	.LBB1_75
# BB#73:                                # %if.then283
	leaq	-45(%rsi), %rcx
	andq	%r8, %rcx
	cmpq	$8, %rcx
	jb	.LBB1_75
# BB#74:                                # %if.then283
	leaq	-29(%rsi), %rcx
	andq	%r8, %rcx
	cmpq	$7, %rcx
	jbe	.LBB1_75
# BB#80:                                # %cond.false330
	cmpq	$24, %rax
	jne	.LBB1_81
# BB#83:                                # %cond.true346
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB1_85
.LBB1_29:                               # %sw.bb1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB1_105
# BB#30:                                # %if.then
	movq	$1, %rax
	movb	%al, 3(%r13)
	jmp	.LBB1_105
.LBB1_75:                               # %cond.true311
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB1_85
# BB#76:                                # %lor.rhs319
	cmpq	$6, %rcx
	jne	.LBB1_77
# BB#78:                                # %lor.rhs319
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB1_85
.LBB1_81:                               # %cond.false330
	cmpq	$18, %rax
	jne	.LBB1_84
# BB#82:                                # %cond.true336
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB1_85
.LBB1_77:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB1_85
.LBB1_84:                               # %cond.false350
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB1_85:                               # %cond.end370
	addq	%rsi, %rax
	andq	%r8, %rax
	cmpq	$53, %rax
	ja	.LBB1_106
# BB#86:                                # %for.cond377.preheader
	cmpq	%rax, %rsi
	jae	.LBB1_105
# BB#87:                                # %for.body380.lr.ph
	movq	%r13, %r15
	movq	8(%r15), %rdx
	movq	%rsi, %rdi
.LBB1_88:                               # %for.body380
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rbx
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	orq	%rbx, %rdx
	incq	%rdi
	movq	%rdi, %rcx
	andq	%r8, %rcx
	incq	%rsi
	cmpq	%rax, %rcx
	jne	.LBB1_88
# BB#89:                                # %for.cond377.for.end466.loopexit645_crit_edge
	movq	%rdx, 8(%r15)
.LBB1_105:                              # %for.end466
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_49:                               # %if.else
	movq	%rbx, %rdi
	movq	%r8, %r14
	callq	subreg_regno
	movq	%r14, %rdx
	movq	%rax, %rsi
	movzbl	2(%rbx), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	je	.LBB1_53
# BB#50:                                # %if.else
	leaq	-21(%rsi), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB1_53
# BB#51:                                # %if.else
	leaq	-45(%rsi), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB1_53
# BB#52:                                # %if.else
	movq	%rsi, %rcx
	addq	$-29, %rcx
	andq	%rdx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB1_53
# BB#58:                                # %cond.false
	movq	%rdx, %rbx
	cmpq	$24, %rax
	jne	.LBB1_59
# BB#61:                                # %cond.true241
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB1_63
.LBB1_106:                              # %if.then375
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_set_resources, %rcx
	movq	$794, %rsi              # imm = 0x31A
	jmp	.LBB1_69
.LBB1_53:                               # %cond.true
	movq	%rdx, %rbx
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB1_63
# BB#54:                                # %lor.rhs217
	cmpq	$6, %rcx
	jne	.LBB1_55
# BB#56:                                # %lor.rhs217
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB1_63
.LBB1_59:                               # %cond.false
	cmpq	$18, %rax
	jne	.LBB1_62
# BB#60:                                # %cond.true232
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB1_63
.LBB1_55:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB1_63
.LBB1_62:                               # %cond.false245
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB1_63:                               # %cond.end261
	addq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$53, %rax
	ja	.LBB1_68
# BB#64:                                # %for.cond268.preheader
	movq	%rsi, %rcx
	movq	%rbx, %rdx
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jae	.LBB1_105
# BB#65:                                # %for.body271.lr.ph
	movq	%rdx, %rbx
	movq	8(%r13), %rdx
.LBB1_66:                               # %for.body271
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	incq	%rsi
	movq	%rsi, %rdi
	andq	%rbx, %rdi
	incq	%rcx
	cmpq	%rax, %rdi
	jne	.LBB1_66
# BB#67:                                # %for.cond268.for.end466.loopexit644_crit_edge
	movq	%rdx, 8(%r13)
	jmp	.LBB1_105
.LBB1_68:                               # %if.then266
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mark_set_resources, %rcx
	movq	$779, %rsi              # imm = 0x30B
.LBB1_69:                               # %if.then266
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp35:
	.size	mark_set_resources, .Ltmp35-mark_set_resources
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_23
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_18
	.quad	.LBB1_18
	.quad	.LBB1_18
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_92
	.quad	.LBB1_94
	.quad	.LBB1_98
	.quad	.LBB1_92
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_31
	.quad	.LBB1_105
	.quad	.LBB1_38
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_93
	.quad	.LBB1_98
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_98
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_98
	.quad	.LBB1_70
	.quad	.LBB1_98
	.quad	.LBB1_47
	.quad	.LBB1_90
	.quad	.LBB1_98
	.quad	.LBB1_44
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_29
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_38
	.quad	.LBB1_38
	.quad	.LBB1_38
	.quad	.LBB1_38
	.quad	.LBB1_39
	.quad	.LBB1_39
.LJTI1_1:
	.quad	.LBB1_23
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_12
	.quad	.LBB1_12
	.quad	.LBB1_13
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_92
	.quad	.LBB1_94
	.quad	.LBB1_98
	.quad	.LBB1_92
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_31
	.quad	.LBB1_105
	.quad	.LBB1_38
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_93
	.quad	.LBB1_98
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_98
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_98
	.quad	.LBB1_70
	.quad	.LBB1_98
	.quad	.LBB1_47
	.quad	.LBB1_90
	.quad	.LBB1_98
	.quad	.LBB1_44
	.quad	.LBB1_105
	.quad	.LBB1_105
	.quad	.LBB1_29
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_98
	.quad	.LBB1_38
	.quad	.LBB1_38
	.quad	.LBB1_38
	.quad	.LBB1_38
	.quad	.LBB1_39
	.quad	.LBB1_39

	.text
	.globl	mark_target_live_regs
	.align	16, 0x90
	.type	mark_target_live_regs,@function
mark_target_live_regs:                  # @mark_target_live_regs
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
	subq	$144, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 192
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
	movq	%rdx, %r9
	movq	%rdi, %r14
	testq	%rsi, %rsi
	je	.LBB2_1
# BB#2:                                 # %if.end
	movq	$1, %rax
	movl	%eax, (%r9)
	movq	target_hash_table(%rip), %r8
	testq	%r8, %r8
	je	.LBB2_3
# BB#4:                                 # %if.then2
	movslq	8(%rsi), %rdi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movabsq	$9187483429707480961, %rcx # imm = 0x7F807F807F807F81
	movq	%rdi, %rax
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$7, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	imulq	$257, %rdx, %rax        # imm = 0x101
	movq	%rdi, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	(%r8,%rdx,8), %rbx
	movq	%r9, %r12
	testq	%rbx, %rbx
	je	.LBB2_5
# BB#8:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	.align	16, 0x90
.LBB2_9:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rbx), %ecx
	cmpq	%rdi, %rcx
	je	.LBB2_10
# BB#6:                                 # %for.cond
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_9
# BB#7:
	xorq	%rbx, %rbx
	jmp	.LBB2_12
.LBB2_1:                                # %if.then
	movq	end_of_function_needs+8(%rip), %rax
	movq	%rax, 8(%r9)
	movq	end_of_function_needs(%rip), %rax
	movq	%rax, (%r9)
	jmp	.LBB2_146
.LBB2_3:
	movq	%r9, %r12
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	jmp	.LBB2_12
.LBB2_5:
	xorq	%rbx, %rbx
	jmp	.LBB2_12
.LBB2_10:                               # %land.lhs.true
	movl	24(%rbx), %r15d
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %r15
	je	.LBB2_12
# BB#11:                                # %land.lhs.true12
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rdx,8), %rax
	movq	(%rax), %rax
	movzbl	3(%rax), %eax
	testq	$8, %rax
	je	.LBB2_13
.LBB2_12:                               # %if.then22
	movq	compiler_params(%rip), %rax
	movl	56(%rax), %esi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	find_basic_block
	movq	target_hash_table(%rip), %r8
	movq	%rax, %r15
.LBB2_13:                               # %if.end24
	testq	%r8, %r8
	je	.LBB2_20
# BB#14:                                # %if.then26
	testq	%rbx, %rbx
	je	.LBB2_19
# BB#15:                                # %if.then28
	movl	24(%rbx), %edx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB2_20
# BB#16:                                # %if.then28
	cmpq	%rax, %rcx
	je	.LBB2_20
# BB#17:                                # %land.lhs.true33
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	bb_ticks(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	28(%rbx), %ecx
	movl	(%rdx,%rax,4), %eax
	cmpq	%rax, %rcx
	jne	.LBB2_20
# BB#18:                                # %if.then37
	movq	16(%rbx), %rax
	movq	%rax, 8(%r12)
	jmp	.LBB2_146
.LBB2_19:                               # %if.else
	movq	$32, %rdi
	callq	xmalloc
	movq	56(%rsp), %rcx          # 8-byte Reload
	movslq	8(%rcx), %rsi
	movq	%rax, %rbx
	movl	%esi, (%rbx)
	movl	%r15d, 24(%rbx)
	movabsq	$9187483429707480961, %rcx # imm = 0x7F807F807F807F81
	movq	%rsi, %rax
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$7, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	imulq	$257, %rdx, %rax        # imm = 0x101
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	target_hash_table(%rip), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	(%rax,%rsi,8), %rcx
	movq	%rcx, 8(%rbx)
	movq	%rbx, (%rax,%rsi,8)
.LBB2_20:                               # %if.end59
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	$0, pending_dead_regs(%rip)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r15, %rax
	andq	%r14, %rax
	cmpq	%r14, %rax
	je	.LBB2_138
# BB#21:                                # %if.then61
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rdx
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	32(%rax,%rdx,8), %rax
	movq	64(%rax), %rbx
	movq	$0, current_live_regs(%rip)
	movabsq	$current_live_regs, %rdi
	movq	%rbx, %rsi
	callq	reg_set_to_hard_reg_set
	movq	(%rbx), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB2_49
# BB#22:                                # %for.cond79.preheader.lr.ph
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	testq	%rax, %rax
	movq	$53, %rbx
	je	.LBB2_24
# BB#23:                                # %select.mid
	xorq	%rbx, %rbx
.LBB2_24:                               # %select.end
	movabsq	$0, %r9
	.align	16, 0x90
.LBB2_25:                               # %for.cond79.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_26 Depth 2
                                        #       Child Loop BB2_28 Depth 3
                                        #         Child Loop BB2_33 Depth 4
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB2_26:                               # %for.body81
                                        #   Parent Loop BB2_25 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_28 Depth 3
                                        #         Child Loop BB2_33 Depth 4
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	24(%rax,%rcx,8), %r12
	testq	%r12, %r12
	je	.LBB2_47
# BB#27:                                # %for.body88.preheader
                                        #   in Loop: Header=BB2_26 Depth=2
	movq	$6, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %rsi
	andq	%r14, %rsi
	.align	16, 0x90
.LBB2_28:                               # %for.body88
                                        #   Parent Loop BB2_25 Depth=1
                                        #     Parent Loop BB2_26 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB2_33 Depth 4
	movq	$1, %r13
	movq	%rsi, %rcx
	shlq	%cl, %r13
	testq	%r12, %r13
	je	.LBB2_46
# BB#29:                                # %if.then90
                                        #   in Loop: Header=BB2_28 Depth=3
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	movq	reg_renumber(%rip), %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	leaq	(%rsi,%rax), %rax
	addq	%rdx, %rax
	andq	%r14, %rax
	movswq	(%rcx,%rax,2), %r10
	testq	%r10, %r10
	js	.LBB2_45
# BB#30:                                # %for.cond103.preheader
                                        #   in Loop: Header=BB2_28 Depth=3
	movq	%r10, %rcx
	andq	$65528, %rcx            # imm = 0xFFF8
	cmpq	$8, %rcx
	movq	%r9, %rcx
	jne	.LBB2_32
# BB#31:                                # %for.cond103.preheader
                                        #   in Loop: Header=BB2_28 Depth=3
	movabsq	$1, %rcx
.LBB2_32:                               # %for.cond103.preheader
                                        #   in Loop: Header=BB2_28 Depth=3
	leaq	-21(%r10), %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$8, %rdx
	sbbq	%rdx, %rdx
	andq	$1, %rdx
	orq	%rcx, %rdx
	movq	cfun(%rip), %rcx
	leaq	-45(%r10), %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$8, %rdi
	movq	24(%rcx), %rcx
	sbbq	%rdi, %rdi
	andq	$1, %rdi
	orq	%rdx, %rdi
	movq	%r10, %rdx
	movq	80(%rcx), %rcx
	addq	$-29, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$8, %rdx
	movq	(%rcx,%rax,8), %r8
	sbbq	%r15, %r15
	andq	$1, %r15
	orq	%rdi, %r15
	movq	%r10, %rdi
	jmp	.LBB2_33
	.align	16, 0x90
.LBB2_44:                               # %for.body199
                                        #   in Loop: Header=BB2_33 Depth=4
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	orq	%rax, current_live_regs(%rip)
	incq	%rdi
.LBB2_33:                               # %for.cond103
                                        #   Parent Loop BB2_25 Depth=1
                                        #     Parent Loop BB2_26 Depth=2
                                        #       Parent Loop BB2_28 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	2(%r8), %eax
	testq	%r15, %r15
	je	.LBB2_38
# BB#34:                                # %cond.true
                                        #   in Loop: Header=BB2_33 Depth=4
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_43
# BB#35:                                # %lor.rhs
                                        #   in Loop: Header=BB2_33 Depth=4
	cmpq	$6, %rcx
	movq	%r9, %rax
	jne	.LBB2_37
# BB#36:                                # %lor.rhs
                                        #   in Loop: Header=BB2_33 Depth=4
	movabsq	$1, %rax
.LBB2_37:                               # %lor.rhs
                                        #   in Loop: Header=BB2_33 Depth=4
	incq	%rax
	jmp	.LBB2_43
	.align	16, 0x90
.LBB2_38:                               # %cond.false
                                        #   in Loop: Header=BB2_33 Depth=4
	cmpq	$24, %rax
	jne	.LBB2_39
# BB#41:                                # %cond.true169
                                        #   in Loop: Header=BB2_33 Depth=4
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_43
	.align	16, 0x90
.LBB2_39:                               # %cond.false
                                        #   in Loop: Header=BB2_33 Depth=4
	cmpq	$18, %rax
	jne	.LBB2_42
# BB#40:                                # %cond.true155
                                        #   in Loop: Header=BB2_33 Depth=4
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_43
.LBB2_42:                               # %cond.false173
                                        #   in Loop: Header=BB2_33 Depth=4
	movl	target_flags(%rip), %r11d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r11
	andq	$4, %r11
	leaq	3(%rax,%r11), %rax
	addq	$4, %r11
	cqto
	idivq	%r11
	.align	16, 0x90
.LBB2_43:                               # %cond.end194
                                        #   in Loop: Header=BB2_33 Depth=4
	addq	%r10, %rax
	andq	%r14, %rax
	movq	%rdi, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB2_44
.LBB2_45:                               # %if.end204
                                        #   in Loop: Header=BB2_28 Depth=3
	notq	%r13
	andq	%r13, %r12
	je	.LBB2_47
.LBB2_46:                               # %for.inc210
                                        #   in Loop: Header=BB2_28 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	incq	%rsi
	cmpq	$64, %rax
	jb	.LBB2_28
.LBB2_47:                               # %if.end213
                                        #   in Loop: Header=BB2_26 Depth=2
	movq	64(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	xorq	%rbx, %rbx
	cmpq	$2, %rcx
	jne	.LBB2_26
# BB#48:                                # %for.end216
                                        #   in Loop: Header=BB2_25 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB2_25
.LBB2_49:                               # %do.end220
	movq	16(%rsp), %r13          # 8-byte Reload
	testq	%r14, %r13
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r15
	je	.LBB2_51
# BB#50:                                # %cond.false224
	movq	basic_block_info(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rax
	movq	(%rax), %r15
.LBB2_51:                               # %cond.end230
	movzbl	(%r15), %eax
	cmpq	$32, %rax
	jne	.LBB2_52
# BB#53:                                # %land.lhs.true236
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	movq	56(%rsp), %rcx          # 8-byte Reload
	jne	.LBB2_55
# BB#54:                                # %if.then243
	movq	8(%rax), %rax
	movq	8(%rax), %r15
	jmp	.LBB2_55
.LBB2_138:                              # %if.else708
	movq	%r12, %rsi
	movq	$-1, 8(%rsi)
	jmp	.LBB2_139
.LBB2_52:
	movq	56(%rsp), %rcx          # 8-byte Reload
.LBB2_55:                               # %if.end250
	movzbl	(%rcx), %eax
	cmpq	$32, %rax
	movq	%rcx, %rbx
	jne	.LBB2_58
# BB#56:                                # %land.lhs.true255
	movq	32(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$24, %rax
	movq	%rcx, %rbx
	jne	.LBB2_58
# BB#57:                                # %if.then263
	movq	16(%rcx), %rdi
	movq	%rcx, %rbx
	callq	next_insn
	movq	%rbx, %rcx
	movq	%rax, %rbx
.LBB2_58:                               # %if.end268
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	cmpq	%rbx, %r15
	je	.LBB2_127
	.align	16, 0x90
.LBB2_59:                               # %for.body272
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_68 Depth 2
                                        #     Child Loop BB2_76 Depth 2
                                        #       Child Loop BB2_96 Depth 3
                                        #     Child Loop BB2_100 Depth 2
                                        #       Child Loop BB2_119 Depth 3
                                        #     Child Loop BB2_132 Depth 2
	movl	(%r15), %eax
	testq	$268435456, %rax        # imm = 0x10000000
	jne	.LBB2_125
# BB#60:                                # %if.end278
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$32, %rcx
	jne	.LBB2_61
# BB#62:                                # %land.lhs.true283
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	32(%r15), %rcx
	movzbl	(%rcx), %edx
	cmpq	$48, %rdx
	movq	%r15, %r12
	jne	.LBB2_66
# BB#63:                                # %land.lhs.true291
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	8(%rcx), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	je	.LBB2_65
# BB#64:                                # %select.mid10
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	%r15, %r12
.LBB2_65:                               # %select.end9
                                        #   in Loop: Header=BB2_59 Depth=1
	movl	(%r12), %eax
	jmp	.LBB2_66
	.align	16, 0x90
.LBB2_61:                               #   in Loop: Header=BB2_59 Depth=1
	movq	%r15, %r12
.LBB2_66:                               # %if.end312
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$34, %rcx
	jne	.LBB2_72
# BB#67:                                # %if.then317
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	regs_invalidated_by_call(%rip), %rcx
	movq	current_live_regs(%rip), %rax
	notq	%rcx
	andq	%rcx, %rax
	movq	%rax, current_live_regs(%rip)
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB2_68:                               # %for.body323
                                        #   Parent Loop BB2_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	global_regs(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB2_70
# BB#69:                                # %if.then327
                                        #   in Loop: Header=BB2_68 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdx, %rax
	movq	%rax, current_live_regs(%rip)
.LBB2_70:                               # %for.inc332
                                        #   in Loop: Header=BB2_68 Depth=2
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB2_68
# BB#71:                                # %if.end335.loopexit
                                        #   in Loop: Header=BB2_59 Depth=1
	movl	(%r12), %eax
.LBB2_72:                               # %if.end335
                                        #   in Loop: Header=BB2_59 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	addq	$-32, %rax
	andq	%r14, %rax
	cmpq	$5, %rax
	ja	.LBB2_125
# BB#73:                                # %if.end335
                                        #   in Loop: Header=BB2_59 Depth=1
	jmpq	*.LJTI2_0(,%rax,8)
.LBB2_74:                               # %land.lhs.true340
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	32(%r12), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$48, %rax
	je	.LBB2_125
.LBB2_75:                               # %if.then366
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	56(%r12), %r8
	jmp	.LBB2_76
	.align	16, 0x90
.LBB2_98:                               # %for.inc511
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	16(%r8), %r8
.LBB2_76:                               # %if.then366
                                        #   Parent Loop BB2_59 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_96 Depth 3
	testq	%r8, %r8
	je	.LBB2_99
# BB#77:                                # %for.body372
                                        #   in Loop: Header=BB2_76 Depth=2
	movl	(%r8), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB2_98
# BB#78:                                # %land.lhs.true378
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	8(%r8), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB2_98
# BB#79:                                # %land.lhs.true386
                                        #   in Loop: Header=BB2_76 Depth=2
	movl	8(%rcx), %esi
	cmpq	$52, %rsi
	ja	.LBB2_98
# BB#80:                                # %if.then394
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	%rsi, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB2_84
# BB#81:                                # %if.then394
                                        #   in Loop: Header=BB2_76 Depth=2
	leaq	-21(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_84
# BB#82:                                # %if.then394
                                        #   in Loop: Header=BB2_76 Depth=2
	leaq	-45(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_84
# BB#83:                                # %if.then394
                                        #   in Loop: Header=BB2_76 Depth=2
	leaq	-29(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$7, %rcx
	jbe	.LBB2_84
# BB#88:                                # %cond.false448
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	%r13, %rdi
	cmpq	$24, %rax
	jne	.LBB2_89
# BB#91:                                # %cond.true470
                                        #   in Loop: Header=BB2_76 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_93
.LBB2_84:                               # %cond.true424
                                        #   in Loop: Header=BB2_76 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_94
# BB#85:                                # %lor.rhs435
                                        #   in Loop: Header=BB2_76 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_87
# BB#86:                                # %lor.rhs435
                                        #   in Loop: Header=BB2_76 Depth=2
	movabsq	$1, %rax
.LBB2_87:                               # %lor.rhs435
                                        #   in Loop: Header=BB2_76 Depth=2
	incq	%rax
	jmp	.LBB2_94
.LBB2_89:                               # %cond.false448
                                        #   in Loop: Header=BB2_76 Depth=2
	cmpq	$18, %rax
	jne	.LBB2_92
# BB#90:                                # %cond.true457
                                        #   in Loop: Header=BB2_76 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_93
.LBB2_92:                               # %cond.false474
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	%rbx, %r9
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%r9, %rbx
.LBB2_93:                               # %cond.end497
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	%rdi, %r13
.LBB2_94:                               # %cond.end497
                                        #   in Loop: Header=BB2_76 Depth=2
	addq	%rsi, %rax
	andq	%r14, %rax
	cmpq	%rax, %rsi
	jae	.LBB2_98
# BB#95:                                # %for.body503.lr.ph
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	%rbx, %r9
	movq	pending_dead_regs(%rip), %rdx
	movq	%rsi, %rbx
	.align	16, 0x90
.LBB2_96:                               # %for.body503
                                        #   Parent Loop BB2_59 Depth=1
                                        #     Parent Loop BB2_76 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %rdi
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rsi
	cmpq	%rax, %rcx
	jne	.LBB2_96
# BB#97:                                # %for.cond500.for.inc511.loopexit_crit_edge
                                        #   in Loop: Header=BB2_76 Depth=2
	movq	%rdx, pending_dead_regs(%rip)
	movq	%r9, %rbx
	jmp	.LBB2_98
.LBB2_99:                               # %for.end515
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	32(%r12), %rdi
	xorq	%rdx, %rdx
	movabsq	$update_live_status, %rsi
	callq	note_stores
	movq	56(%r12), %r8
	jmp	.LBB2_100
	.align	16, 0x90
.LBB2_121:                              # %for.inc667
                                        #   in Loop: Header=BB2_100 Depth=2
	movq	16(%r8), %r8
.LBB2_100:                              # %for.end515
                                        #   Parent Loop BB2_59 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_119 Depth 3
	testq	%r8, %r8
	je	.LBB2_125
# BB#101:                               # %for.body524
                                        #   in Loop: Header=BB2_100 Depth=2
	movl	(%r8), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$655360, %rax           # imm = 0xA0000
	jne	.LBB2_121
# BB#102:                               # %land.lhs.true530
                                        #   in Loop: Header=BB2_100 Depth=2
	movq	8(%r8), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB2_121
# BB#103:                               # %land.lhs.true538
                                        #   in Loop: Header=BB2_100 Depth=2
	movl	8(%rcx), %esi
	cmpq	$52, %rsi
	ja	.LBB2_121
# BB#104:                               # %if.then547
                                        #   in Loop: Header=BB2_100 Depth=2
	movq	%rsi, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB2_108
# BB#105:                               # %if.then547
                                        #   in Loop: Header=BB2_100 Depth=2
	leaq	-21(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_108
# BB#106:                               # %if.then547
                                        #   in Loop: Header=BB2_100 Depth=2
	leaq	-45(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_108
# BB#107:                               # %if.then547
                                        #   in Loop: Header=BB2_100 Depth=2
	leaq	-29(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$7, %rcx
	jbe	.LBB2_108
# BB#112:                               # %cond.false603
                                        #   in Loop: Header=BB2_100 Depth=2
	cmpq	$24, %rax
	jne	.LBB2_113
# BB#115:                               # %cond.true625
                                        #   in Loop: Header=BB2_100 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB2_117
.LBB2_108:                              # %cond.true579
                                        #   in Loop: Header=BB2_100 Depth=2
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_117
# BB#109:                               # %lor.rhs590
                                        #   in Loop: Header=BB2_100 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB2_111
# BB#110:                               # %lor.rhs590
                                        #   in Loop: Header=BB2_100 Depth=2
	movabsq	$1, %rax
.LBB2_111:                              # %lor.rhs590
                                        #   in Loop: Header=BB2_100 Depth=2
	incq	%rax
	jmp	.LBB2_117
.LBB2_113:                              # %cond.false603
                                        #   in Loop: Header=BB2_100 Depth=2
	cmpq	$18, %rax
	jne	.LBB2_116
# BB#114:                               # %cond.true612
                                        #   in Loop: Header=BB2_100 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB2_117
.LBB2_116:                              # %cond.false629
                                        #   in Loop: Header=BB2_100 Depth=2
	movq	%rbx, %rdi
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rdi, %rbx
.LBB2_117:                              # %cond.end652
                                        #   in Loop: Header=BB2_100 Depth=2
	addq	%rsi, %rax
	andq	%r14, %rax
	cmpq	%rax, %rsi
	jae	.LBB2_121
# BB#118:                               # %for.body658.lr.ph
                                        #   in Loop: Header=BB2_100 Depth=2
	movq	%rbx, %r9
	movq	current_live_regs(%rip), %rdx
	movq	%rsi, %rbx
	.align	16, 0x90
.LBB2_119:                              # %for.body658
                                        #   Parent Loop BB2_59 Depth=1
                                        #     Parent Loop BB2_100 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %rdi
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdi, %rdx
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	incq	%rsi
	cmpq	%rax, %rcx
	jne	.LBB2_119
# BB#120:                               # %for.cond655.for.inc667.loopexit_crit_edge
                                        #   in Loop: Header=BB2_100 Depth=2
	movq	%rdx, current_live_regs(%rip)
	movq	%r9, %rbx
	jmp	.LBB2_121
.LBB2_122:                              # %if.then677
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	pending_dead_regs(%rip), %rax
	notq	%rax
	andq	%rax, current_live_regs(%rip)
	movq	$0, pending_dead_regs(%rip)
	jmp	.LBB2_125
.LBB2_123:                              # %land.lhs.true685
                                        #   in Loop: Header=BB2_59 Depth=1
	movl	40(%r12), %eax
	movabsq	$4294967207, %rcx       # imm = 0xFFFFFFA7
	cmpq	%rcx, %rax
	jne	.LBB2_125
# BB#124:                               # %if.then691
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	start_of_epilogue_needs+8(%rip), %rax
	orq	%rax, current_live_regs(%rip)
	.align	16, 0x90
.LBB2_125:                              # %for.inc696
                                        #   in Loop: Header=BB2_59 Depth=1
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB2_126
# BB#129:                               # %if.then.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movzbl	3(%r15), %ecx
	testq	$4, %rcx
	jne	.LBB2_131
# BB#130:                               #   in Loop: Header=BB2_59 Depth=1
	movq	%r15, %rcx
	jmp	.LBB2_133
	.align	16, 0x90
.LBB2_131:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	16(%r15), %rcx
	movq	24(%rcx), %rcx
	cmpq	%r15, %rcx
	movq	%r15, %rcx
	je	.LBB2_133
	.align	16, 0x90
.LBB2_132:                              # %while.cond.i
                                        #   Parent Loop BB2_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rcx
	movq	24(%rcx), %r15
	movzbl	3(%r15), %edx
	testq	$16, %rdx
	jne	.LBB2_132
.LBB2_133:                              # %if.end.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB2_126
# BB#134:                               # %land.lhs.true20.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movzbl	(%rcx), %eax
	cmpq	$32, %rax
	jne	.LBB2_135
# BB#136:                               # %land.lhs.true24.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	32(%rcx), %rdx
	movzbl	(%rdx), %eax
	cmpq	$24, %rax
	movq	%rcx, %rax
	jne	.LBB2_126
# BB#137:                               # %if.then31.i
                                        #   in Loop: Header=BB2_59 Depth=1
	movq	8(%rdx), %rax
	movq	8(%rax), %rax
	jmp	.LBB2_126
.LBB2_135:                              #   in Loop: Header=BB2_59 Depth=1
	movq	%rcx, %rax
	.align	16, 0x90
.LBB2_126:                              # %for.cond269.backedge
                                        #   in Loop: Header=BB2_59 Depth=1
	cmpq	%rbx, %rax
	movq	%rax, %r15
	jne	.LBB2_59
.LBB2_127:                              # %for.end698
	movq	current_live_regs(%rip), %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, 8(%rsi)
	movq	40(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB2_139
# BB#128:                               # %if.then702
	movl	%r13d, 24(%rbx)
	movq	bb_ticks(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rax,%rcx,4), %eax
	movl	%eax, 28(%rbx)
.LBB2_139:                              # %do.body711
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$0, 128(%rsp)
	xorq	%r14, %r14
	movl	%r14d, 120(%rsp)
	movq	120(%rsp), %r8
	movq	$0, 112(%rsp)
	movl	%r14d, 104(%rsp)
	movq	104(%rsp), %rax
	movq	112(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	136(%rsp), %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	find_dead_or_set_registers
	testq	%rax, %rax
	je	.LBB2_144
# BB#140:                               # %if.then727
	movq	%rax, %rdi
	callq	next_active_insn
	movq	136(%rsp), %rdi
	movq	%rax, %r13
	callq	next_active_insn
	leaq	88(%rsp), %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	mark_target_live_regs
	cmpq	%r15, %r13
	movq	$0, 128(%rsp)
	movl	%r14d, 120(%rsp)
	movq	$0, 112(%rsp)
	movl	%r14d, 104(%rsp)
	movq	%r15, %rax
	je	.LBB2_143
# BB#141:
	leaq	104(%rsp), %r12
	leaq	120(%rsp), %r14
	.align	16, 0x90
.LBB2_142:                              # %for.body748
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	%rax, %rbx
	callq	mark_referenced_resources
	movq	128(%rsp), %rax
	movq	112(%rsp), %rcx
	notq	%rax
	andq	%rcx, %rax
	orq	%rax, 96(%rsp)
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	mark_set_resources
	movq	%rbx, %rdi
	callq	next_active_insn
	cmpq	%r13, %rax
	jne	.LBB2_142
.LBB2_143:                              # %for.end757
	movq	96(%rsp), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	orq	%rax, 8(%rcx)
.LBB2_144:                              # %if.end761
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB2_146
# BB#145:                               # %if.then764
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, 16(%rcx)
.LBB2_146:                              # %if.end767
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	mark_target_live_regs, .Ltmp53-mark_target_live_regs
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI2_0:
	.quad	.LBB2_74
	.quad	.LBB2_75
	.quad	.LBB2_75
	.quad	.LBB2_125
	.quad	.LBB2_122
	.quad	.LBB2_123

	.text
	.align	16, 0x90
	.type	find_basic_block,@function
find_basic_block:                       # @find_basic_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp59:
	.cfi_def_cfa_offset 32
.Ltmp60:
	.cfi_offset %rbx, -24
.Ltmp61:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	callq	prev_nonnote_insn
	testq	%rax, %rax
	je	.LBB3_5
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_2:                                # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	je	.LBB3_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_2 Depth=1
	testq	%r14, %rbx
	je	.LBB3_6
# BB#4:                                 # %for.inc
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%rax, %rdi
	callq	prev_nonnote_insn
	decq	%rbx
	testq	%rax, %rax
	jne	.LBB3_2
.LBB3_5:                                # %for.end.thread
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$1, %rax
	sbbq	%rdi, %rdi
	jmp	.LBB3_16
.LBB3_6:                                # %for.end
	andq	%r14, %rbx
	movq	$-1, %rdi
	je	.LBB3_8
# BB#7:                                 # %for.end
	xorq	%rdi, %rdi
.LBB3_8:                                # %for.end
	testq	%rbx, %rbx
	jne	.LBB3_9
	jmp	.LBB3_16
	.align	16, 0x90
.LBB3_10:                               # %land.rhs10
                                        #   in Loop: Header=BB3_9 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB3_15
# BB#11:                                # %for.cond16.preheader
                                        #   in Loop: Header=BB3_9 Depth=1
	movslq	n_basic_blocks(%rip), %rsi
	testq	%rsi, %rsi
	jle	.LBB3_9
# BB#12:                                # %for.body18.lr.ph
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	basic_block_info(%rip), %rbx
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB3_14:                               # %for.body18
                                        #   Parent Loop BB3_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rbx,%rdi,8), %rcx
	movq	(%rcx), %rcx
	cmpq	%rcx, %rax
	je	.LBB3_16
# BB#13:                                # %for.cond16
                                        #   in Loop: Header=BB3_14 Depth=2
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jl	.LBB3_14
	.align	16, 0x90
.LBB3_9:                                # %if.end6
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_14 Depth 2
	movq	%rax, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	jne	.LBB3_10
.LBB3_15:
	movq	%r14, %rdi
.LBB3_16:                               # %return
	movq	%rdi, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp62:
	.size	find_basic_block, .Ltmp62-find_basic_block
	.cfi_endproc

	.align	16, 0x90
	.type	update_live_status,@function
update_live_status:                     # @update_live_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp66:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp67:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp68:
	.cfi_def_cfa_offset 32
.Ltmp69:
	.cfi_offset %rbx, -24
.Ltmp70:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edx
	movq	%rsi, %r14
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_1
# BB#4:                                 # %if.else
	movl	8(%rbx), %esi
	jmp	.LBB4_5
.LBB4_1:                                # %entry
	cmpq	$63, %rax
	jne	.LBB4_28
# BB#2:                                 # %lor.lhs.false
	movq	8(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB4_28
# BB#3:                                 # %if.then10
	movq	%rbx, %rdi
	callq	subreg_regno
	movl	(%rbx), %edx
	movq	%rax, %rsi
.LBB4_5:                                # %if.end13
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rsi, %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	$8, %rax
	je	.LBB4_9
# BB#6:                                 # %if.end13
	leaq	-21(%rsi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_9
# BB#7:                                 # %if.end13
	leaq	-45(%rsi), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_9
# BB#8:                                 # %if.end13
	leaq	-29(%rsi), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB4_9
# BB#14:                                # %cond.false
	cmpq	$24, %rdx
	jne	.LBB4_15
# BB#17:                                # %cond.true50
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB4_19
.LBB4_9:                                # %cond.true
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB4_19
# BB#10:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB4_11
# BB#12:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB4_19
.LBB4_15:                               # %cond.false
	cmpq	$18, %rdx
	jne	.LBB4_18
# BB#16:                                # %cond.true43
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB4_19
.LBB4_11:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB4_19
.LBB4_18:                               # %cond.false54
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rdx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB4_19:                               # %cond.end69
	movzbl	(%r14), %ecx
	leaq	(%rax,%rsi), %r8
	cmpq	$49, %rcx
	jne	.LBB4_20
# BB#24:                                # %for.cond.preheader
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_28
# BB#25:                                # %for.body.lr.ph
	movq	current_live_regs(%rip), %rax
	.align	16, 0x90
.LBB4_26:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rdi, %rax
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rdi
	jl	.LBB4_26
# BB#27:                                # %for.cond.if.end94.loopexit_crit_edge
	movq	%rax, current_live_regs(%rip)
	jmp	.LBB4_28
.LBB4_20:                               # %for.cond81.preheader
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_28
# BB#21:                                # %for.body84.lr.ph
	movq	current_live_regs(%rip), %rax
	movq	pending_dead_regs(%rip), %rdi
	.align	16, 0x90
.LBB4_22:                               # %for.body84
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rdx
	movq	$1, %rdi
	movq	%rsi, %rcx
	shlq	%cl, %rdi
	orq	%rdi, %rax
	notq	%rdi
	andq	%rdx, %rdi
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	jl	.LBB4_22
# BB#23:                                # %for.cond81.if.end94.loopexit132_crit_edge
	movq	%rax, current_live_regs(%rip)
	movq	%rdi, pending_dead_regs(%rip)
.LBB4_28:                               # %if.end94
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp71:
	.size	update_live_status, .Ltmp71-update_live_status
	.cfi_endproc

	.align	16, 0x90
	.type	find_dead_or_set_registers,@function
find_dead_or_set_registers:             # @find_dead_or_set_registers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 160
.Ltmp84:
	.cfi_offset %rbx, -48
.Ltmp85:
	.cfi_offset %r12, -40
.Ltmp86:
	.cfi_offset %r13, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	xorq	%rax, %rax
	testq	%r13, %r13
	movq	%r8, 96(%rsp)
	movq	%r9, 104(%rsp)
	je	.LBB5_40
# BB#1:                                 # %for.body.lr.ph.lr.ph
	leaq	160(%rsp), %rbx
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_14 Depth 2
	movq	%r13, %r15
	movq	24(%r15), %r13
	movq	%r15, %rdi
	callq	can_throw_internal
	testq	%rax, %rax
	jne	.LBB5_39
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB5_2 Depth=1
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-32, %rcx
	andq	%r14, %rcx
	movq	%r15, %r12
	cmpq	$5, %rcx
	ja	.LBB5_16
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB5_2 Depth=1
	jmpq	*.LJTI5_0(,%rcx,8)
.LBB5_5:                                # %sw.bb5
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	32(%r15), %rcx
	movzbl	(%rcx), %edx
	cmpq	$48, %rdx
	je	.LBB5_11
# BB#6:                                 # %sw.bb5
                                        #   in Loop: Header=BB5_2 Depth=1
	cmpq	$24, %rdx
	je	.LBB5_13
# BB#7:                                 # %sw.bb5
                                        #   in Loop: Header=BB5_2 Depth=1
	cmpq	$49, %rdx
	je	.LBB5_10
# BB#8:                                 #   in Loop: Header=BB5_2 Depth=1
	movq	%r15, %r12
	jmp	.LBB5_16
.LBB5_9:                                # %sw.bb
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	8(%rbx), %rax
	movq	%rax, %rcx
	movq	pending_dead_regs(%rip), %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, pending_dead_regs(%rip)
	notq	%rdx
	orq	%rax, %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	%rdx, 8(%rax)
	movq	$0, pending_dead_regs(%rip)
	jmp	.LBB5_10
.LBB5_11:                               # %if.then11
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	8(%rcx), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB5_10
# BB#12:                                # %if.then23
                                        #   in Loop: Header=BB5_2 Depth=1
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	mark_set_resources
	.align	16, 0x90
.LBB5_10:                               # %for.cond.backedge
                                        #   in Loop: Header=BB5_2 Depth=1
	testq	%r13, %r13
	jne	.LBB5_2
	jmp	.LBB5_39
.LBB5_13:                               # %for.cond48.preheader
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	8(%rcx), %r8
	movslq	(%r8), %rsi
	testq	%rsi, %rsi
	movq	$1, %rdi
	movq	%r15, %r12
	jle	.LBB5_16
	.align	16, 0x90
.LBB5_14:                               # %for.body56
                                        #   Parent Loop BB5_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r8,%rdi,8), %r12
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$33, %rcx
	je	.LBB5_16
# BB#15:                                # %for.body56
                                        #   in Loop: Header=BB5_14 Depth=2
	movq	$32, %rcx
	leaq	1(%rdi), %rdx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	movq	%rdx, %rdi
	jl	.LBB5_14
	.align	16, 0x90
.LBB5_16:                               # %sw.epilog
                                        #   in Loop: Header=BB5_2 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$33, %rax
	jne	.LBB5_25
# BB#17:                                # %if.then78
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$9, %rdx
	jg	.LBB5_40
# BB#18:                                # %if.then82
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%r12, %rdi
	callq	any_uncondjump_p
	testq	%r14, %rax
	jne	.LBB5_20
# BB#19:                                # %lor.lhs.false
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	32(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	jne	.LBB5_26
.LBB5_20:                               # %if.then92
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	64(%r12), %r13
	incq	24(%rsp)                # 8-byte Folded Spill
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	movq	16(%rsp), %rax          # 8-byte Reload
	jne	.LBB5_22
# BB#21:                                # %if.then92
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%r15, 32(%rsp)          # 8-byte Spill
.LBB5_22:                               # %if.then92
                                        #   in Loop: Header=BB5_2 Depth=1
	jne	.LBB5_25
# BB#23:                                # %if.then92
                                        #   in Loop: Header=BB5_2 Depth=1
	testq	%rax, %rax
	je	.LBB5_25
# BB#24:                                # %if.then100
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%r13, (%rax)
	movq	%r15, 32(%rsp)          # 8-byte Spill
.LBB5_25:                               # %if.end236
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	mark_referenced_resources
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	leaq	96(%rsp), %rsi
	callq	mark_set_resources
	movq	104(%rsp), %rax
	movq	8(%rbx), %rcx
	notq	%rax
	orq	%rcx, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%rax, 8(%rcx)
	testq	%r13, %r13
	movq	32(%rsp), %rax          # 8-byte Reload
	jne	.LBB5_2
	jmp	.LBB5_40
.LBB5_26:                               # %if.else106
	movq	%r12, %rdi
	callq	any_condjump_p
	testq	%r14, %rax
	je	.LBB5_39
# BB#27:                                # %if.then109
	movq	24(%rsp), %r14          # 8-byte Reload
	addq	$5, %r14
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$9, %rax
	jg	.LBB5_39
# BB#28:                                # %if.end113
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	mark_referenced_resources
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$24, %rcx
	jne	.LBB5_37
# BB#29:                                # %land.lhs.true
	movzbl	3(%r12), %ecx
	testq	$4, %rcx
	je	.LBB5_37
# BB#30:                                # %for.cond125.preheader
	movq	8(%rax), %rdx
	movslq	(%rdx), %rax
	cmpq	$2, %rax
	jl	.LBB5_33
# BB#31:
	movq	$2, %rax
.LBB5_32:                               # %for.body135
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rax,8), %rcx
	movl	(%rcx), %edx
	xorq	$268435456, %rdx        # imm = 0x10000000
	movl	%edx, (%rcx)
	movq	32(%r15), %rcx
	movq	8(%rcx), %rdx
	movslq	(%rdx), %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rsi, %rdi
	jl	.LBB5_32
.LBB5_33:                               # %for.end162
	movq	96(%rsp), %rax
	movq	104(%rsp), %rcx
	movq	%rcx, 88(%rsp)
	movq	%rax, 80(%rsp)
	leaq	80(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	callq	mark_set_resources
	movq	32(%r15), %rax
	movq	8(%rax), %rdx
	movslq	(%rdx), %rax
	cmpq	$2, %rax
	jl	.LBB5_36
# BB#34:
	movq	$2, %rax
.LBB5_35:                               # %for.body173
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx,%rax,8), %rcx
	movl	(%rcx), %edx
	xorq	$268435456, %rdx        # imm = 0x10000000
	movl	%edx, (%rcx)
	movq	32(%r15), %rcx
	movq	8(%rcx), %rdx
	movslq	(%rdx), %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	cmpq	%rsi, %rdi
	jl	.LBB5_35
.LBB5_36:                               # %for.end205
	leaq	96(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	callq	mark_set_resources
	jmp	.LBB5_38
.LBB5_37:                               # %if.else206
	leaq	96(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	callq	mark_set_resources
	movq	96(%rsp), %rax
	movq	104(%rsp), %rcx
	movq	%rcx, 88(%rsp)
	movq	%rax, 80(%rsp)
.LBB5_38:                               # %if.end207
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	(%r14), %rax
	movq	88(%rsp), %r9
	movq	8(%r14), %rcx
	movq	8(%rbx), %rdx
	movq	%rcx, 72(%rsp)
	movq	%rax, 64(%rsp)
	movq	%r9, %rax
	notq	%rax
	orq	%rdx, %rax
	andq	%rax, 72(%rsp)
	movq	(%r14), %rax
	movq	8(%r14), %rcx
	movq	104(%rsp), %rsi
	movq	%rcx, 56(%rsp)
	movq	%rax, 48(%rsp)
	notq	%rsi
	orq	%rdx, %rsi
	andq	%rsi, 56(%rsp)
	movq	64(%r12), %rdi
	movq	80(%rsp), %r8
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	leaq	64(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	%rax, (%rsp)
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	callq	find_dead_or_set_registers
	movq	96(%rsp), %r8
	movq	104(%rsp), %r9
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	leaq	48(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	movq	%r15, %rcx
	callq	find_dead_or_set_registers
	movq	72(%rsp), %rax
	movq	56(%rsp), %rcx
	orq	%rax, %rcx
	movq	%rcx, 56(%rsp)
	andq	%rcx, 8(%r14)
.LBB5_39:                               # %for.end245
	movq	32(%rsp), %rax          # 8-byte Reload
.LBB5_40:                               # %for.end245
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	find_dead_or_set_registers, .Ltmp89-find_dead_or_set_registers
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_5
	.quad	.LBB5_16
	.quad	.LBB5_16
	.quad	.LBB5_10
	.quad	.LBB5_9
	.quad	.LBB5_10

	.text
	.globl	init_resource_info
	.align	16, 0x90
	.type	init_resource_info,@function
init_resource_info:                     # @init_resource_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp95:
	.cfi_def_cfa_offset 32
.Ltmp96:
	.cfi_offset %rbx, -24
.Ltmp97:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	movb	%bl, end_of_function_needs+3(%rip)
	movq	$1, %rax
	movb	%al, end_of_function_needs(%rip)
	movl	frame_pointer_needed(%rip), %eax
	movb	%bl, end_of_function_needs+1(%rip)
	movq	$0, end_of_function_needs+8(%rip)
	testq	%rax, %rax
	movq	$128, %rax
	je	.LBB6_3
# BB#1:                                 # %if.then
	movl	current_function_sp_is_unchanging(%rip), %eax
	movq	$1048640, end_of_function_needs+8(%rip) # imm = 0x100040
	testq	%rax, %rax
	movq	$1048640, %rax          # imm = 0x100040
	je	.LBB6_3
# BB#2:                                 # %select.mid
	movq	$1048768, %rax          # imm = 0x1000C0
.LBB6_3:                                # %if.end6
	movq	cfun(%rip), %rcx
	movq	%rax, end_of_function_needs+8(%rip)
	movq	120(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_5
# BB#4:                                 # %if.then7
	movabsq	$end_of_function_needs, %rsi
	movq	$1, %rdx
	callq	mark_referenced_resources
	.align	16, 0x90
.LBB6_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	global_regs(%rbx), %eax
	testq	%rax, %rax
	je	.LBB6_7
# BB#6:                                 # %if.then12
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	orq	%rax, end_of_function_needs+8(%rip)
.LBB6_7:                                # %for.inc
                                        #   in Loop: Header=BB6_5 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB6_5
# BB#8:                                 # %for.end
	movq	end_of_function_needs+8(%rip), %rax
	movq	end_of_function_needs(%rip), %rcx
	movq	%rax, start_of_epilogue_needs+8(%rip)
	movq	%rcx, start_of_epilogue_needs(%rip)
	movq	%r14, %rdi
	callq	next_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB6_11
# BB#9:
	movabsq	$end_of_function_needs, %r14
	.align	16, 0x90
.LBB6_10:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	mark_set_resources
	movq	%rbx, %rdi
	callq	next_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB6_10
.LBB6_11:                               # %while.end
	movq	$257, %rdi              # imm = 0x101
	movq	$8, %rsi
	callq	xcalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, target_hash_table(%rip)
	movq	$4, %rsi
	callq	xcalloc
	movq	%rax, bb_ticks(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp98:
	.size	init_resource_info, .Ltmp98-init_resource_info
	.cfi_endproc

	.globl	free_resource_info
	.align	16, 0x90
	.type	free_resource_info,@function
free_resource_info:                     # @free_resource_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp102:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp103:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp104:
	.cfi_def_cfa_offset 32
.Ltmp105:
	.cfi_offset %rbx, -24
.Ltmp106:
	.cfi_offset %r14, -16
	movq	target_hash_table(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB7_6
# BB#1:
	xorq	%r14, %r14
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_3 Depth 2
	movq	(%rdi,%r14,8), %rdi
	testq	%rdi, %rdi
	je	.LBB7_4
	.align	16, 0x90
.LBB7_3:                                # %while.body
                                        #   Parent Loop BB7_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB7_3
.LBB7_4:                                # %for.inc
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	target_hash_table(%rip), %rdi
	incq	%r14
	cmpq	$257, %r14              # imm = 0x101
	jne	.LBB7_2
# BB#5:                                 # %for.end
	callq	free
	movq	$0, target_hash_table(%rip)
.LBB7_6:                                # %if.end
	movq	bb_ticks(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB7_8
# BB#7:                                 # %if.then4
	callq	free
	movq	$0, bb_ticks(%rip)
.LBB7_8:                                # %if.end5
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp107:
	.size	free_resource_info, .Ltmp107-free_resource_info
	.cfi_endproc

	.globl	clear_hashed_info_for_insn
	.align	16, 0x90
	.type	clear_hashed_info_for_insn,@function
clear_hashed_info_for_insn:             # @clear_hashed_info_for_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	target_hash_table(%rip), %r8
	testq	%r8, %r8
	je	.LBB8_6
# BB#1:                                 # %if.then
	movslq	8(%rdi), %rsi
	movabsq	$9187483429707480961, %rcx # imm = 0x7F807F807F807F81
	movq	%rsi, %rax
	imulq	%rcx
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$7, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	imulq	$257, %rdx, %rax        # imm = 0x101
	movq	%rsi, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	(%r8,%rdx,8), %rax
	testq	%rax, %rax
	je	.LBB8_6
# BB#2:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	.align	16, 0x90
.LBB8_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %edx
	cmpq	%rsi, %rdx
	je	.LBB8_5
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB8_4 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB8_4
	jmp	.LBB8_6
.LBB8_5:                                # %if.then8
	movl	%ecx, 24(%rax)
.LBB8_6:                                # %if.end10
	retq
.Ltmp108:
	.size	clear_hashed_info_for_insn, .Ltmp108-clear_hashed_info_for_insn
	.cfi_endproc

	.globl	incr_ticks_for_insn
	.align	16, 0x90
	.type	incr_ticks_for_insn,@function
incr_ticks_for_insn:                    # @incr_ticks_for_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp110:
	.cfi_def_cfa_offset 16
	movq	compiler_params(%rip), %rax
	movl	56(%rax), %esi
	callq	find_basic_block
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	%rcx, %rdx
	je	.LBB9_2
# BB#1:                                 # %if.then
	movq	$32, %rcx
	movq	bb_ticks(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%rdx,%rax,4), %ecx
	incq	%rcx
	movl	%ecx, (%rdx,%rax,4)
.LBB9_2:                                # %if.end
	popq	%rax
	retq
.Ltmp111:
	.size	incr_ticks_for_insn, .Ltmp111-incr_ticks_for_insn
	.cfi_endproc

	.globl	mark_end_of_function_resources
	.align	16, 0x90
	.type	mark_end_of_function_resources,@function
mark_end_of_function_resources:         # @mark_end_of_function_resources
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$end_of_function_needs, %rax
	movq	%rsi, %rcx
	movq	%rax, %rsi
	movq	%rcx, %rdx
	jmp	mark_referenced_resources  # TAILCALL
.Ltmp112:
	.size	mark_end_of_function_resources, .Ltmp112-mark_end_of_function_resources
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"resource.c"
	.size	.L.str, 11

	.type	.L__FUNCTION__.mark_referenced_resources,@object # @__FUNCTION__.mark_referenced_resources
.L__FUNCTION__.mark_referenced_resources:
	.asciz	"mark_referenced_resources"
	.size	.L__FUNCTION__.mark_referenced_resources, 26

	.type	.L__FUNCTION__.mark_set_resources,@object # @__FUNCTION__.mark_set_resources
.L__FUNCTION__.mark_set_resources:
	.asciz	"mark_set_resources"
	.size	.L__FUNCTION__.mark_set_resources, 19

	.type	end_of_function_needs,@object # @end_of_function_needs
	.local	end_of_function_needs
	.comm	end_of_function_needs,16,8
	.type	target_hash_table,@object # @target_hash_table
	.local	target_hash_table
	.comm	target_hash_table,8,8
	.type	bb_ticks,@object        # @bb_ticks
	.local	bb_ticks
	.comm	bb_ticks,8,8
	.type	pending_dead_regs,@object # @pending_dead_regs
	.local	pending_dead_regs
	.comm	pending_dead_regs,8,8
	.type	current_live_regs,@object # @current_live_regs
	.local	current_live_regs
	.comm	current_live_regs,8,8
	.type	start_of_epilogue_needs,@object # @start_of_epilogue_needs
	.local	start_of_epilogue_needs
	.comm	start_of_epilogue_needs,16,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
