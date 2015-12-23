	.file	"attacks.c.bc"
	.text
	.globl	calc_attackers
	.align	16, 0x90
	.type	calc_attackers,@function
calc_attackers:                         # @calc_attackers
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB0_32
# BB#1:                                 # %if.end
	xorq	%r10, %r10
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	xorq	%r8, %r8
	testq	$1, %rsi
	je	.LBB0_2
	.align	16, 0x90
.LBB0_4:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_7 Depth 2
	movl	nk_attacked.rook_o(,%r10,4), %esi
	leaq	(%rsi,%rdi), %rax
	movq	$32, %rcx
	movq	%rax, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movl	board(,%r9,4), %ecx
	cmpq	$5, %rcx
	jne	.LBB0_7
	jmp	.LBB0_5
	.align	16, 0x90
.LBB0_6:                                # %if.end26
                                        #   in Loop: Header=BB0_7 Depth=2
	addq	%rsi, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
.LBB0_7:                                # %while.cond
                                        #   Parent Loop BB0_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r11, %rcx
	cmpq	$13, %rcx
	je	.LBB0_6
# BB#8:                                 # %while.cond
                                        #   in Loop: Header=BB0_4 Depth=1
	cmpq	$7, %rcx
	je	.LBB0_10
# BB#9:                                 # %while.cond
                                        #   in Loop: Header=BB0_4 Depth=1
	cmpq	$9, %rcx
	jne	.LBB0_11
.LBB0_10:                               # %if.then18
                                        #   in Loop: Header=BB0_4 Depth=1
	incq	%r8
.LBB0_11:                               # %for.inc
                                        #   in Loop: Header=BB0_4 Depth=1
	incq	%r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r9, %r9
	cmpq	$4, %rax
	jl	.LBB0_4
	jmp	.LBB0_12
	.align	16, 0x90
.LBB0_2:                                # %for.body100
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_41 Depth 2
	movl	nk_attacked.rook_o(,%r10,4), %esi
	leaq	(%rsi,%rdi), %rax
	movq	$32, %rcx
	movq	%rax, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movl	board(,%r9,4), %ecx
	cmpq	$6, %rcx
	jne	.LBB0_41
	jmp	.LBB0_3
	.align	16, 0x90
.LBB0_40:                               # %if.end129
                                        #   in Loop: Header=BB0_41 Depth=2
	addq	%rsi, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
.LBB0_41:                               # %while.cond110
                                        #   Parent Loop BB0_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r11, %rcx
	cmpq	$13, %rcx
	je	.LBB0_40
# BB#42:                                # %while.cond110
                                        #   in Loop: Header=BB0_2 Depth=1
	cmpq	$8, %rcx
	je	.LBB0_44
# BB#43:                                # %while.cond110
                                        #   in Loop: Header=BB0_2 Depth=1
	cmpq	$10, %rcx
	jne	.LBB0_45
.LBB0_44:                               # %if.then122
                                        #   in Loop: Header=BB0_2 Depth=1
	incq	%r8
.LBB0_45:                               # %for.inc135
                                        #   in Loop: Header=BB0_2 Depth=1
	incq	%r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r9, %r9
	cmpq	$4, %rax
	jl	.LBB0_2
	jmp	.LBB0_46
.LBB0_5:                                # %if.then8
	incq	%r8
	xorq	%r9, %r9
	.align	16, 0x90
.LBB0_12:                               # %for.body34
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_35 Depth 2
	movl	nk_attacked.bishop_o(,%r9,4), %edx
	leaq	(%rdx,%rdi), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_33
# BB#13:                                # %for.body34
                                        #   in Loop: Header=BB0_12 Depth=1
	movq	%r9, %rax
	andq	$1, %rax
	jne	.LBB0_14
.LBB0_33:                               # %if.else45
                                        #   in Loop: Header=BB0_12 Depth=1
	cmpq	$5, %rcx
	jne	.LBB0_35
	jmp	.LBB0_14
	.align	16, 0x90
.LBB0_34:                               # %if.end72
                                        #   in Loop: Header=BB0_35 Depth=2
	addq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
.LBB0_35:                               # %while.cond52
                                        #   Parent Loop BB0_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r11, %rcx
	cmpq	$13, %rcx
	je	.LBB0_34
# BB#36:                                # %while.cond52
                                        #   in Loop: Header=BB0_12 Depth=1
	cmpq	$9, %rcx
	je	.LBB0_38
# BB#37:                                # %while.cond52
                                        #   in Loop: Header=BB0_12 Depth=1
	cmpq	$11, %rcx
	jne	.LBB0_39
.LBB0_38:                               # %if.then64
                                        #   in Loop: Header=BB0_12 Depth=1
	incq	%r8
.LBB0_39:                               # %for.inc79
                                        #   in Loop: Header=BB0_12 Depth=1
	incq	%r9
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB0_12
	jmp	.LBB0_15
.LBB0_14:                               # %if.then43
	incq	%r8
.LBB0_15:                               # %for.cond82.preheader
	leaq	10(%rdi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %eax
	cmpq	$3, %rax
	movabsq	$0, %rax
	movabsq	$1, %r9
	movq	%rax, %rsi
	jne	.LBB0_17
# BB#16:                                # %for.cond82.preheader
	movq	%r9, %rsi
.LBB0_17:                               # %for.cond82.preheader
	leaq	-10(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%r8, %rsi
	cmpq	$3, %rcx
	movq	%rax, %r8
	jne	.LBB0_19
# BB#18:                                # %for.cond82.preheader
	movq	%r9, %r8
.LBB0_19:                               # %for.cond82.preheader
	leaq	14(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%rsi, %r8
	cmpq	$3, %rcx
	movq	%rax, %rsi
	jne	.LBB0_21
# BB#20:                                # %for.cond82.preheader
	movq	%r9, %rsi
.LBB0_21:                               # %for.cond82.preheader
	leaq	-14(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%r8, %rsi
	cmpq	$3, %rcx
	movq	%rax, %r8
	jne	.LBB0_23
# BB#22:                                # %for.cond82.preheader
	movq	%r9, %r8
.LBB0_23:                               # %for.cond82.preheader
	leaq	23(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%rsi, %r8
	cmpq	$3, %rcx
	movq	%rax, %rsi
	jne	.LBB0_25
# BB#24:                                # %for.cond82.preheader
	movq	%r9, %rsi
.LBB0_25:                               # %for.cond82.preheader
	leaq	-23(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%r8, %rsi
	cmpq	$3, %rcx
	movq	%rax, %r8
	jne	.LBB0_27
# BB#26:                                # %for.cond82.preheader
	movq	%r9, %r8
.LBB0_27:                               # %for.cond82.preheader
	leaq	25(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%rsi, %r8
	cmpq	$3, %rcx
	movq	%rax, %rsi
	jne	.LBB0_29
# BB#28:                                # %for.cond82.preheader
	movq	%r9, %rsi
.LBB0_29:                               # %for.cond82.preheader
	addq	$-25, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	board(,%rdi,4), %ecx
	addq	%r8, %rsi
	cmpq	$3, %rcx
	jne	.LBB0_31
	jmp	.LBB0_30
.LBB0_3:                                # %if.then107
	incq	%r8
	xorq	%r9, %r9
	.align	16, 0x90
.LBB0_46:                               # %for.body140
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_66 Depth 2
	movl	nk_attacked.bishop_o(,%r9,4), %edx
	leaq	(%rdx,%rdi), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB0_64
# BB#47:                                # %for.body140
                                        #   in Loop: Header=BB0_46 Depth=1
	movq	%r9, %rax
	andq	$1, %rax
	je	.LBB0_48
.LBB0_64:                               # %if.else152
                                        #   in Loop: Header=BB0_46 Depth=1
	cmpq	$6, %rcx
	jne	.LBB0_66
	jmp	.LBB0_48
	.align	16, 0x90
.LBB0_65:                               # %if.end179
                                        #   in Loop: Header=BB0_66 Depth=2
	addq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
.LBB0_66:                               # %while.cond159
                                        #   Parent Loop BB0_46 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r11, %rcx
	cmpq	$13, %rcx
	je	.LBB0_65
# BB#67:                                # %while.cond159
                                        #   in Loop: Header=BB0_46 Depth=1
	cmpq	$10, %rcx
	je	.LBB0_69
# BB#68:                                # %while.cond159
                                        #   in Loop: Header=BB0_46 Depth=1
	cmpq	$12, %rcx
	jne	.LBB0_70
.LBB0_69:                               # %if.then171
                                        #   in Loop: Header=BB0_46 Depth=1
	incq	%r8
.LBB0_70:                               # %for.inc186
                                        #   in Loop: Header=BB0_46 Depth=1
	incq	%r9
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB0_46
	jmp	.LBB0_49
.LBB0_48:                               # %if.then150
	incq	%r8
.LBB0_49:                               # %for.cond189.preheader
	leaq	10(%rdi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %eax
	cmpq	$4, %rax
	movabsq	$0, %rax
	movabsq	$1, %r9
	movq	%rax, %rsi
	jne	.LBB0_51
# BB#50:                                # %for.cond189.preheader
	movq	%r9, %rsi
.LBB0_51:                               # %for.cond189.preheader
	leaq	-10(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%r8, %rsi
	cmpq	$4, %rcx
	movq	%rax, %r8
	jne	.LBB0_53
# BB#52:                                # %for.cond189.preheader
	movq	%r9, %r8
.LBB0_53:                               # %for.cond189.preheader
	leaq	14(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%rsi, %r8
	cmpq	$4, %rcx
	movq	%rax, %rsi
	jne	.LBB0_55
# BB#54:                                # %for.cond189.preheader
	movq	%r9, %rsi
.LBB0_55:                               # %for.cond189.preheader
	leaq	-14(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%r8, %rsi
	cmpq	$4, %rcx
	movq	%rax, %r8
	jne	.LBB0_57
# BB#56:                                # %for.cond189.preheader
	movq	%r9, %r8
.LBB0_57:                               # %for.cond189.preheader
	leaq	23(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%rsi, %r8
	cmpq	$4, %rcx
	movq	%rax, %rsi
	jne	.LBB0_59
# BB#58:                                # %for.cond189.preheader
	movq	%r9, %rsi
.LBB0_59:                               # %for.cond189.preheader
	leaq	-23(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%r8, %rsi
	cmpq	$4, %rcx
	movq	%rax, %r8
	jne	.LBB0_61
# BB#60:                                # %for.cond189.preheader
	movq	%r9, %r8
.LBB0_61:                               # %for.cond189.preheader
	leaq	25(%rdi), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	addq	%rsi, %r8
	cmpq	$4, %rcx
	movq	%rax, %rsi
	jne	.LBB0_63
# BB#62:                                # %for.cond189.preheader
	movq	%r9, %rsi
.LBB0_63:                               # %for.cond189.preheader
	addq	$-25, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	board(,%rdi,4), %ecx
	addq	%r8, %rsi
	cmpq	$4, %rcx
	jne	.LBB0_31
.LBB0_30:                               # %for.cond82.preheader
	movq	%r9, %rax
.LBB0_31:                               # %for.cond82.preheader
	addq	%rsi, %rax
.LBB0_32:                               # %return
	retq
.Ltmp0:
	.size	calc_attackers, .Ltmp0-calc_attackers
	.cfi_endproc

	.globl	is_attacked
	.align	16, 0x90
	.type	is_attacked,@function
is_attacked:                            # @is_attacked
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5:
	.cfi_def_cfa_offset 24
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	xorq	%r11, %r11
	movabsq	$0, %r8
	movabsq	$1, %r9
	testq	$1, %rsi
	je	.LBB1_1
# BB#12:
	movabsq	$4294967293, %r14       # imm = 0xFFFFFFFD
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_13:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_17 Depth 2
	movl	nk_attacked.bishop_o(,%r11,4), %edx
	leaq	(%rdx,%rdi), %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %esi
	cmpq	$1, %rsi
	movq	%r8, %rax
	jne	.LBB1_15
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB1_13 Depth=1
	movq	%r9, %rax
.LBB1_15:                               # %for.body
                                        #   in Loop: Header=BB1_13 Depth=1
	testq	%rax, %r11
	movq	$1, %rax
	jne	.LBB1_48
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB1_13 Depth=1
	cmpq	$5, %rsi
	jne	.LBB1_17
	jmp	.LBB1_48
	.align	16, 0x90
.LBB1_20:                               # %if.end17
                                        #   in Loop: Header=BB1_17 Depth=2
	addq	%rdx, %rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	board(,%rsi,4), %esi
.LBB1_17:                               # %while.cond.preheader
                                        #   Parent Loop BB1_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rsi, %rsi
	je	.LBB1_21
# BB#18:                                # %while.body
                                        #   in Loop: Header=BB1_17 Depth=2
	movq	%rsi, %rcx
	andq	%r14, %rcx
	cmpq	$9, %rcx
	je	.LBB1_48
# BB#19:                                # %if.end14
                                        #   in Loop: Header=BB1_17 Depth=2
	andq	%r10, %rsi
	cmpq	$13, %rsi
	je	.LBB1_20
	.align	16, 0x90
.LBB1_21:                               # %for.inc
                                        #   in Loop: Header=BB1_13 Depth=1
	incq	%r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB1_13
# BB#22:
	movq	$1, %rax
	.align	16, 0x90
.LBB1_23:                               # %for.body23
                                        # =>This Inner Loop Header: Depth=1
	movl	nk_attacked.knight_o-4(,%rax,4), %edx
	addq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	cmpq	$3, %rcx
	je	.LBB1_49
# BB#24:                                # %for.cond21
                                        #   in Loop: Header=BB1_23 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rax
	cmpq	$7, %rdx
	jle	.LBB1_23
# BB#25:
	xorq	%r8, %r8
.LBB1_26:                               # %for.body37
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_28 Depth 2
	movl	nk_attacked.rook_o(,%r8,4), %eax
	leaq	(%rax,%rdi), %rdx
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	board(,%rbx,4), %ecx
	cmpq	$5, %rcx
	jne	.LBB1_28
	jmp	.LBB1_27
	.align	16, 0x90
.LBB1_33:                               # %if.end56
                                        #   in Loop: Header=BB1_28 Depth=2
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	board(,%rsi,4), %ecx
.LBB1_28:                               # %while.cond46
                                        #   Parent Loop BB1_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r10, %rcx
	cmpq	$9, %rcx
	je	.LBB1_29
# BB#30:                                # %while.cond46
                                        #   in Loop: Header=BB1_28 Depth=2
	cmpq	$7, %rcx
	je	.LBB1_31
# BB#32:                                # %while.cond46
                                        #   in Loop: Header=BB1_28 Depth=2
	cmpq	$13, %rcx
	je	.LBB1_33
# BB#34:                                # %for.inc61
                                        #   in Loop: Header=BB1_26 Depth=1
	incq	%r8
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$4, %rdx
	jl	.LBB1_26
	jmp	.LBB1_48
.LBB1_1:
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
.LBB1_2:                                # %for.body66
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_8 Depth 2
	movl	nk_attacked.bishop_o(,%r11,4), %esi
	leaq	(%rsi,%rdi), %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	cmpq	$2, %rcx
	movq	%r8, %rax
	jne	.LBB1_4
# BB#3:                                 # %for.body66
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%r9, %rax
.LBB1_4:                                # %for.body66
                                        #   in Loop: Header=BB1_2 Depth=1
	testq	$1, %r11
	movq	%r8, %rbx
	jne	.LBB1_6
# BB#5:                                 # %for.body66
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%r9, %rbx
.LBB1_6:                                # %for.body66
                                        #   in Loop: Header=BB1_2 Depth=1
	testq	%rbx, %rax
	movq	$1, %rax
	jne	.LBB1_48
# BB#7:                                 # %for.body66
                                        #   in Loop: Header=BB1_2 Depth=1
	cmpq	$6, %rcx
	jne	.LBB1_8
	jmp	.LBB1_48
	.align	16, 0x90
.LBB1_11:                               # %if.end91
                                        #   in Loop: Header=BB1_8 Depth=2
	addq	%rsi, %rdx
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	board(,%rbx,4), %ecx
.LBB1_8:                                # %while.cond81
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r10, %rcx
	cmpq	$12, %rcx
	je	.LBB1_48
# BB#9:                                 # %while.cond81
                                        #   in Loop: Header=BB1_8 Depth=2
	cmpq	$10, %rcx
	je	.LBB1_48
# BB#10:                                # %while.cond81
                                        #   in Loop: Header=BB1_8 Depth=2
	cmpq	$13, %rcx
	je	.LBB1_11
# BB#35:                                # %for.inc96
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB1_2
# BB#36:
	movq	$1, %rax
.LBB1_37:                               # %for.body101
                                        # =>This Inner Loop Header: Depth=1
	movl	nk_attacked.knight_o-4(,%rax,4), %edx
	addq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	cmpq	$4, %rcx
	je	.LBB1_49
# BB#38:                                # %for.cond99
                                        #   in Loop: Header=BB1_37 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rax
	cmpq	$7, %rdx
	jle	.LBB1_37
# BB#39:
	xorq	%rdx, %rdx
	movabsq	$4294967293, %r8        # imm = 0xFFFFFFFD
	jmp	.LBB1_40
.LBB1_49:
	movq	$1, %rax
	jmp	.LBB1_48
.LBB1_29:
	movq	$1, %rax
	jmp	.LBB1_48
.LBB1_31:
	movq	$1, %rax
.LBB1_48:                               # %return
	popq	%rbx
	popq	%r14
	retq
.LBB1_27:
	movq	$1, %rax
	jmp	.LBB1_48
.LBB1_41:                               # %for.body115
                                        #   in Loop: Header=BB1_40 Depth=1
	cmpq	$6, %rcx
	je	.LBB1_42
.LBB1_43:                               # %while.body128
                                        #   Parent Loop BB1_40 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rsi
	andq	%r8, %rsi
	cmpq	$8, %rsi
	je	.LBB1_44
# BB#45:                                # %if.end133
                                        #   in Loop: Header=BB1_43 Depth=2
	andq	%r10, %rcx
	cmpq	$13, %rcx
	jne	.LBB1_47
# BB#46:                                # %if.end136
                                        #   in Loop: Header=BB1_43 Depth=2
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	board(,%rsi,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_43
	jmp	.LBB1_47
.LBB1_40:                               # %for.body115
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_43 Depth 2
	movl	nk_attacked.rook_o(,%rdx,4), %eax
	leaq	(%rax,%rdi), %rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	board(,%rsi,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_41
.LBB1_47:                               # %for.inc141
                                        #   in Loop: Header=BB1_40 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	cmpq	$4, %rsi
	jl	.LBB1_40
	jmp	.LBB1_48
.LBB1_44:
	movq	$1, %rax
	jmp	.LBB1_48
.LBB1_42:
	movq	$1, %rax
	jmp	.LBB1_48
.Ltmp8:
	.size	is_attacked, .Ltmp8-is_attacked
	.cfi_endproc

	.globl	nk_attacked
	.align	16, 0x90
	.type	nk_attacked,@function
nk_attacked:                            # @nk_attacked
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%r11, %r11
	testq	$1, %rsi
	je	.LBB2_1
# BB#8:
	movabsq	$4294967293, %r9        # imm = 0xFFFFFFFD
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_9:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_11 Depth 2
	movl	nk_attacked.bishop_o(,%r11,4), %r10d
	leaq	(%r10,%rdi), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	cmpq	$1, %rcx
	movq	$1, %rax
	jne	.LBB2_11
# BB#10:                                # %for.body
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%r11, %rdx
	andq	$1, %rdx
	je	.LBB2_11
	jmp	.LBB2_40
	.align	16, 0x90
.LBB2_14:                               # %if.end14
                                        #   in Loop: Header=BB2_11 Depth=2
	addq	%r10, %rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
.LBB2_11:                               # %while.cond.preheader
                                        #   Parent Loop BB2_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	je	.LBB2_15
# BB#12:                                # %while.body
                                        #   in Loop: Header=BB2_11 Depth=2
	movq	%rcx, %rdx
	andq	%r9, %rdx
	cmpq	$9, %rdx
	je	.LBB2_40
# BB#13:                                # %if.end11
                                        #   in Loop: Header=BB2_11 Depth=2
	andq	%r8, %rcx
	cmpq	$13, %rcx
	je	.LBB2_14
	.align	16, 0x90
.LBB2_15:                               # %for.inc
                                        #   in Loop: Header=BB2_9 Depth=1
	incq	%r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB2_9
# BB#16:
	movq	$1, %rax
	.align	16, 0x90
.LBB2_17:                               # %for.body20
                                        # =>This Inner Loop Header: Depth=1
	movl	nk_attacked.knight_o-4(,%rax,4), %edx
	addq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	cmpq	$3, %rcx
	je	.LBB2_41
# BB#18:                                # %for.cond18
                                        #   in Loop: Header=BB2_17 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rax
	cmpq	$7, %rdx
	jle	.LBB2_17
# BB#19:
	xorq	%rdx, %rdx
.LBB2_20:                               # %for.body34
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_21 Depth 2
	movslq	nk_attacked.rook_o(,%rdx,4), %rax
	leaq	(%rdi,%rax), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	board(,%rsi,4), %rsi
	movq	$2, %rcx
	shlq	%cl, %rax
	jmp	.LBB2_21
	.align	16, 0x90
.LBB2_26:                               # %if.end50
                                        #   in Loop: Header=BB2_21 Depth=2
	addq	%rax, %rsi
.LBB2_21:                               # %while.cond40
                                        #   Parent Loop BB2_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rsi), %ecx
	cmpq	$9, %rcx
	je	.LBB2_22
# BB#23:                                # %while.cond40
                                        #   in Loop: Header=BB2_21 Depth=2
	cmpq	$7, %rcx
	je	.LBB2_24
# BB#25:                                # %while.cond40
                                        #   in Loop: Header=BB2_21 Depth=2
	cmpq	$13, %rcx
	je	.LBB2_26
# BB#27:                                # %for.inc55
                                        #   in Loop: Header=BB2_20 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	cmpq	$4, %rsi
	jl	.LBB2_20
	jmp	.LBB2_40
.LBB2_1:
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
.LBB2_2:                                # %for.body60
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_4 Depth 2
	movl	nk_attacked.bishop_o(,%r11,4), %r8d
	leaq	(%r8,%rdi), %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	cmpq	$2, %rcx
	movq	$1, %rax
	jne	.LBB2_4
# BB#3:                                 # %for.body60
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%r11, %rsi
	andq	$1, %rsi
	jne	.LBB2_4
	jmp	.LBB2_40
	.align	16, 0x90
.LBB2_7:                                # %if.end82
                                        #   in Loop: Header=BB2_4 Depth=2
	addq	%r8, %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	board(,%rsi,4), %ecx
.LBB2_4:                                # %while.cond72
                                        #   Parent Loop BB2_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r9, %rcx
	cmpq	$12, %rcx
	je	.LBB2_40
# BB#5:                                 # %while.cond72
                                        #   in Loop: Header=BB2_4 Depth=2
	cmpq	$10, %rcx
	je	.LBB2_40
# BB#6:                                 # %while.cond72
                                        #   in Loop: Header=BB2_4 Depth=2
	cmpq	$13, %rcx
	je	.LBB2_7
# BB#28:                                # %for.inc87
                                        #   in Loop: Header=BB2_2 Depth=1
	incq	%r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jl	.LBB2_2
# BB#29:
	movq	$1, %rax
.LBB2_30:                               # %for.body92
                                        # =>This Inner Loop Header: Depth=1
	movl	nk_attacked.knight_o-4(,%rax,4), %edx
	addq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	board(,%rdx,4), %ecx
	cmpq	$4, %rcx
	je	.LBB2_41
# BB#31:                                # %for.cond90
                                        #   in Loop: Header=BB2_30 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rax
	cmpq	$7, %rdx
	jle	.LBB2_30
# BB#32:
	xorq	%rdx, %rdx
	movabsq	$4294967293, %r8        # imm = 0xFFFFFFFD
.LBB2_33:                               # %for.body106
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_35 Depth 2
	movl	nk_attacked.rook_o(,%rdx,4), %r10d
	leaq	(%r10,%rdi), %rsi
	jmp	.LBB2_35
.LBB2_34:                               # %while.cond114
                                        #   in Loop: Header=BB2_35 Depth=2
	addq	%r10, %rsi
.LBB2_35:                               # %while.cond114
                                        #   Parent Loop BB2_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	board(,%rax,4), %ecx
	testq	%rcx, %rcx
	je	.LBB2_39
# BB#36:                                # %while.body116
                                        #   in Loop: Header=BB2_35 Depth=2
	movq	%rcx, %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	je	.LBB2_37
# BB#38:                                # %if.end121
                                        #   in Loop: Header=BB2_35 Depth=2
	andq	%r9, %rcx
	cmpq	$13, %rcx
	je	.LBB2_34
.LBB2_39:                               # %for.inc129
                                        #   in Loop: Header=BB2_33 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	cmpq	$4, %rsi
	jl	.LBB2_33
.LBB2_40:                               # %return
	retq
.LBB2_41:
	movq	$1, %rax
	retq
.LBB2_22:
	movq	$1, %rax
	retq
.LBB2_24:
	movq	$1, %rax
	retq
.LBB2_37:
	movq	$1, %rax
	retq
.Ltmp9:
	.size	nk_attacked, .Ltmp9-nk_attacked
	.cfi_endproc

	.type	nk_attacked.rook_o,@object # @nk_attacked.rook_o
	.section	.rodata.cst16,"aM",@progbits,16
	.align	16
nk_attacked.rook_o:
	.long	12                      # 0xc
	.long	4294967284              # 0xfffffff4
	.long	1                       # 0x1
	.long	4294967295              # 0xffffffff
	.size	nk_attacked.rook_o, 16

	.type	nk_attacked.bishop_o,@object # @nk_attacked.bishop_o
	.align	16
nk_attacked.bishop_o:
	.long	11                      # 0xb
	.long	4294967285              # 0xfffffff5
	.long	13                      # 0xd
	.long	4294967283              # 0xfffffff3
	.size	nk_attacked.bishop_o, 16

	.type	nk_attacked.knight_o,@object # @nk_attacked.knight_o
	.section	.rodata,"a",@progbits
	.align	16
nk_attacked.knight_o:
	.long	10                      # 0xa
	.long	4294967286              # 0xfffffff6
	.long	14                      # 0xe
	.long	4294967282              # 0xfffffff2
	.long	23                      # 0x17
	.long	4294967273              # 0xffffffe9
	.long	25                      # 0x19
	.long	4294967271              # 0xffffffe7
	.size	nk_attacked.knight_o, 32


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
