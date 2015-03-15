	.file	"mytest/spec2006/benchspec/CPU2006/458.sjeng/src/epd.c.bc"
	.text
	.globl	setup_epd_line
	.align	16, 0x90
	.type	setup_epd_line,@function
setup_epd_line:                         # @setup_epd_line
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
	movq	%rdi, %r14
	movabsq	$board, %rdi
	xorq	%rsi, %rsi
	movq	$576, %rdx              # imm = 0x240
	xorq	%rbx, %rbx
	callq	memset
	movl	%ebx, white_castled(%rip)
	movl	%ebx, black_castled(%rip)
	movq	$50, %rax
	movl	%eax, book_ply(%rip)
	movq	$-1, %rbx
	.align	16, 0x90
.LBB0_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%r14,%rbx), %eax
	incq	%rbx
	cmpq	$32, %rax
	je	.LBB0_1
# BB#2:                                 # %while.cond2.preheader
	movq	$110, 40(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	xorq	%rdx, %rdx
	xorq	%r9, %r9
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	jmp	.LBB0_3
	.align	16, 0x90
.LBB0_72:                               # %if.end218
                                        #   in Loop: Header=BB0_3 Depth=1
	incq	%rbx
	movq	%rax, %rdx
.LBB0_3:                                # %while.cond2
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_50 Depth 2
                                        #     Child Loop BB0_54 Depth 2
                                        #     Child Loop BB0_56 Depth 2
                                        #     Child Loop BB0_41 Depth 2
                                        #     Child Loop BB0_9 Depth 2
	movq	$32, %rcx
	movq	%rbx, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	(%r14,%r12), %esi
	movq	$56, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rsi, %rsi
	je	.LBB0_73
# BB#4:                                 # %while.cond2
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$10, %rsi
	je	.LBB0_73
# BB#5:                                 # %while.body13
                                        #   in Loop: Header=BB0_3 Depth=1
	testq	%rdi, %rdx
	jne	.LBB0_11
# BB#6:                                 # %while.body13
                                        #   in Loop: Header=BB0_3 Depth=1
	leaq	-48(%rax), %rcx
	andq	%rdi, %rcx
	cmpq	$9, %rcx
	ja	.LBB0_11
# BB#7:                                 # %for.cond.preheader
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%r9, 24(%rsp)           # 8-byte Spill
	addq	%r14, %r12
	movq	%r12, %rdi
	callq	atoi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_10
# BB#8:                                 # %for.body.lr.ph
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx), %r15
	movq	$1, %r13
	.align	16, 0x90
.LBB0_9:                                # %for.body
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r13,%r15), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$13, %rcx
	movl	%ecx, board(,%rax,4)
	movq	%r12, %rdi
	callq	atoi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r13
	cmpq	%rdx, %rsi
	jl	.LBB0_9
.LBB0_10:                               # %for.end
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	24(%rsp), %r9           # 8-byte Reload
	addq	%rax, %r9
	xorq	%rax, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB0_72
	.align	16, 0x90
.LBB0_11:                               # %if.else
                                        #   in Loop: Header=BB0_3 Depth=1
	testq	%rdi, %rdx
	jne	.LBB0_38
# BB#12:                                # %land.lhs.true35
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$47, %rsi
	jne	.LBB0_14
# BB#13:                                # %if.then41
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	setup_epd_line.rankoffsets(,%rax,4), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	xorq	%r9, %r9
	jmp	.LBB0_72
.LBB0_14:                               # %land.lhs.true48
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%rax, %rcx
	orq	$32, %rcx
	addq	$-97, %rcx
	andq	%rdi, %rcx
	cmpq	$25, %rcx
	ja	.LBB0_38
# BB#15:                                # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$74, %rax
	jg	.LBB0_18
# BB#16:                                # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	andq	%rdi, %rax
	cmpq	$66, %rax
	jne	.LBB0_37
# BB#17:                                # %sw.bb78
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$11, %rcx
	jmp	.LBB0_36
.LBB0_38:                               # %if.else107
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$32, %rsi
	jne	.LBB0_39
# BB#40:                                # %if.then113
                                        #   in Loop: Header=BB0_3 Depth=1
	leaq	1(%rdx), %rax
	testq	%rdi, %rdx
	movq	$32, %rcx
	je	.LBB0_41
# BB#47:                                # %if.else134
                                        #   in Loop: Header=BB0_3 Depth=1
	andq	%rdi, %rdx
	cmpq	$1, %rdx
	jne	.LBB0_48
# BB#53:                                # %if.then137
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$1, %rdx
	movl	%edx, moved+120(%rip)
	movl	%edx, moved+132(%rip)
	movl	%edx, moved+104(%rip)
	movl	%edx, moved+468(%rip)
	movl	%edx, moved+456(%rip)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-1(%rbx), %r8
	movabsq	$4294967296, %rsi       # imm = 0x100000000
	imulq	%rsi, %rbx
	movabsq	$-4294967296, %rcx      # imm = 0xFFFFFFFF00000000
	addq	%rcx, %rbx
	movl	%edx, moved+440(%rip)
	movq	%rbx, %rdx
	movq	%r8, %rbx
	.align	16, 0x90
.LBB0_54:                               # %while.cond138
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r14,%rbx), %ecx
	incq	%rbx
	addq	%rsi, %rdx
	cmpq	$32, %rcx
	je	.LBB0_54
# BB#55:                                # %while.cond147.preheader
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%r14,%rdx), %esi
	cmpq	$32, %rsi
	je	.LBB0_67
	.align	16, 0x90
.LBB0_56:                               # %while.body153
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$56, %rcx
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$106, %rsi
	jg	.LBB0_60
# BB#57:                                # %while.body153
                                        #   in Loop: Header=BB0_56 Depth=2
	andq	%rdi, %rsi
	cmpq	$75, %rsi
	jne	.LBB0_58
# BB#63:                                # %sw.bb157
                                        #   in Loop: Header=BB0_56 Depth=2
	xorq	%rcx, %rcx
	movl	%ecx, moved+132(%rip)
	movl	%ecx, moved+120(%rip)
	jmp	.LBB0_66
	.align	16, 0x90
.LBB0_60:                               # %while.body153
                                        #   in Loop: Header=BB0_56 Depth=2
	andq	%rdi, %rsi
	cmpq	$107, %rsi
	jne	.LBB0_61
# BB#64:                                # %sw.bb159
                                        #   in Loop: Header=BB0_56 Depth=2
	xorq	%rcx, %rcx
	movl	%ecx, moved+468(%rip)
	jmp	.LBB0_65
.LBB0_58:                               # %while.body153
                                        #   in Loop: Header=BB0_56 Depth=2
	cmpq	$81, %rsi
	jne	.LBB0_66
# BB#59:                                # %sw.bb158
                                        #   in Loop: Header=BB0_56 Depth=2
	xorq	%rcx, %rcx
	movl	%ecx, moved+104(%rip)
	movl	%ecx, moved+120(%rip)
	jmp	.LBB0_66
.LBB0_61:                               # %while.body153
                                        #   in Loop: Header=BB0_56 Depth=2
	cmpq	$113, %rsi
	jne	.LBB0_66
# BB#62:                                # %sw.bb160
                                        #   in Loop: Header=BB0_56 Depth=2
	xorq	%rcx, %rcx
	movl	%ecx, moved+440(%rip)
.LBB0_65:                               # %sw.epilog161
                                        #   in Loop: Header=BB0_56 Depth=2
	movl	%ecx, moved+456(%rip)
.LBB0_66:                               # %sw.epilog161
                                        #   in Loop: Header=BB0_56 Depth=2
	movzbl	1(%r14,%rdx), %esi
	incq	%rdx
	cmpq	$32, %rsi
	movq	%rdx, %rbx
	jne	.LBB0_56
.LBB0_67:                               # %while.end163
                                        #   in Loop: Header=BB0_3 Depth=1
	decq	%rbx
	jmp	.LBB0_72
	.align	16, 0x90
.LBB0_43:                               # %while.body124
                                        #   in Loop: Header=BB0_41 Depth=2
	movzbl	1(%r14,%r12), %ecx
	incq	%r12
.LBB0_41:                               # %while.cond118
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	cmpq	$119, %rcx
	je	.LBB0_44
# BB#42:                                # %while.cond118
                                        #   in Loop: Header=BB0_41 Depth=2
	cmpq	$32, %rcx
	je	.LBB0_43
# BB#46:                                # %if.else133
                                        #   in Loop: Header=BB0_3 Depth=1
	xorq	%rcx, %rcx
	movl	%ecx, white_to_move(%rip)
	jmp	.LBB0_71
.LBB0_39:                               #   in Loop: Header=BB0_3 Depth=1
	movq	%rdx, %rax
	jmp	.LBB0_72
.LBB0_44:                               # %if.then132
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$1, %rcx
	movl	%ecx, white_to_move(%rip)
	jmp	.LBB0_71
.LBB0_48:                               # %if.else134
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$2, %rdx
	jne	.LBB0_72
# BB#49:                                # %while.cond168.preheader
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$32, %rdx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movabsq	$4294967296, %rsi       # imm = 0x100000000
	imulq	%rsi, %rbx
	jmp	.LBB0_50
	.align	16, 0x90
.LBB0_52:                               # %while.body174
                                        #   in Loop: Header=BB0_50 Depth=2
	movzbl	1(%r14,%r12), %edx
	incq	%r12
.LBB0_50:                               # %while.cond168
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%rsi, %rbx
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$45, %rcx
	je	.LBB0_68
# BB#51:                                # %while.cond168
                                        #   in Loop: Header=BB0_50 Depth=2
	cmpq	$32, %rcx
	je	.LBB0_52
# BB#69:                                # %if.else183
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movsbq	(%r14,%rbx), %rcx
	addq	$2, %r12
	leaq	(%rcx,%rcx,2), %rcx
	leaq	-659(%rdx,%rcx,4), %rcx
	jmp	.LBB0_70
.LBB0_18:                               # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$97, %rax
	jg	.LBB0_21
# BB#19:                                # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	addq	$-75, %rax
	andq	%rdi, %rax
	cmpq	$7, %rax
	ja	.LBB0_37
# BB#20:                                # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	jmpq	*.LJTI0_1(,%rax,8)
.LBB0_35:                               # %sw.bb102
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movl	%eax, wking_loc(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	jmp	.LBB0_36
.LBB0_21:                               # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	$106, %rax
	jg	.LBB0_24
# BB#22:                                # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	andq	%rdi, %rax
	cmpq	$98, %rax
	jne	.LBB0_37
# BB#23:                                # %sw.bb74
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$12, %rcx
	jmp	.LBB0_36
.LBB0_68:                               # %if.then182
                                        #   in Loop: Header=BB0_3 Depth=1
	xorq	%rcx, %rcx
.LBB0_70:                               # %if.end218
                                        #   in Loop: Header=BB0_3 Depth=1
	movl	%ecx, ep_square(%rip)
.LBB0_71:                               # %if.end218
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%r12, %rbx
	jmp	.LBB0_72
.LBB0_24:                               # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	addq	$-107, %rax
	andq	%rdi, %rax
	cmpq	$7, %rax
	ja	.LBB0_37
# BB#25:                                # %if.then55
                                        #   in Loop: Header=BB0_3 Depth=1
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_34:                               # %sw.bb98
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movl	%eax, bking_loc(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$6, %rcx
	jmp	.LBB0_36
.LBB0_29:                               # %sw.bb70
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	jmp	.LBB0_36
.LBB0_27:                               # %sw.bb62
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %rcx
	jmp	.LBB0_36
.LBB0_33:                               # %sw.bb94
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$9, %rcx
	jmp	.LBB0_36
.LBB0_31:                               # %sw.bb86
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$7, %rcx
	jmp	.LBB0_36
.LBB0_28:                               # %sw.bb66
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	jmp	.LBB0_36
.LBB0_26:                               # %sw.bb
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$2, %rcx
	jmp	.LBB0_36
.LBB0_32:                               # %sw.bb90
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$10, %rcx
	jmp	.LBB0_36
.LBB0_30:                               # %sw.bb82
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$8, %rcx
.LBB0_36:                               # %sw.epilog
                                        #   in Loop: Header=BB0_3 Depth=1
	movl	%ecx, board(,%rax,4)
.LBB0_37:                               # %sw.epilog
                                        #   in Loop: Header=BB0_3 Depth=1
	incq	%r9
	xorq	%rax, %rax
	jmp	.LBB0_72
.LBB0_73:                               # %while.end220
	callq	reset_piece_square
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	initialize_hash  # TAILCALL
.Ltmp17:
	.size	setup_epd_line, .Ltmp17-setup_epd_line
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_34
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_28
	.quad	.LBB0_37
	.quad	.LBB0_26
	.quad	.LBB0_32
	.quad	.LBB0_30
.LJTI0_1:
	.quad	.LBB0_35
	.quad	.LBB0_37
	.quad	.LBB0_37
	.quad	.LBB0_29
	.quad	.LBB0_37
	.quad	.LBB0_27
	.quad	.LBB0_33
	.quad	.LBB0_31

	.text
	.globl	check_solution
	.align	16, 0x90
	.type	check_solution,@function
check_solution:                         # @check_solution
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 16
	subq	$288, %rsp              # imm = 0x120
.Ltmp21:
	.cfi_def_cfa_offset 304
.Ltmp22:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	320(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	304(%rsp), %rax
	movq	312(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	comp_to_san
	movabsq	$.L.str, %rsi
	movq	%rbx, %rdi
	callq	strstr
	testq	%rax, %rax
	je	.LBB1_4
# BB#1:                                 # %if.then
	leaq	32(%rsp), %rsi
	movq	%rbx, %rdi
	callq	strstr
	testq	%rax, %rax
	je	.LBB1_2
# BB#3:                                 # %if.then
	movabsq	$1, %rbx
	jmp	.LBB1_9
.LBB1_4:                                # %if.else5
	movabsq	$.L.str1, %rsi
	movq	%rbx, %rdi
	callq	strstr
	testq	%rax, %rax
	je	.LBB1_8
# BB#5:                                 # %if.then8
	leaq	32(%rsp), %rsi
	movq	%rbx, %rdi
	callq	strstr
	testq	%rax, %rax
	jne	.LBB1_6
# BB#7:                                 # %if.then8
	movabsq	$1, %rbx
	jmp	.LBB1_9
.LBB1_2:
	movabsq	$0, %rbx
	jmp	.LBB1_9
.LBB1_8:                                # %if.else14
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	pr