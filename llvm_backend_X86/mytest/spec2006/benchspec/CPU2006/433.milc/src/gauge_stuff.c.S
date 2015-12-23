	.file	"gauge_stuff.c.bc"
	.text
	.globl	make_loop_table
	.align	16, 0x90
	.type	make_loop_table,@function
make_loop_table:                        # @make_loop_table
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
	subq	$176, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 224
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
	xorq	%rax, %rax
	movl	%eax, loop_num+8(%rip)
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	$0, loop_num(%rip)
	movl	make_loop_table.loop_length_in+8(%rip), %eax
	movl	%eax, loop_length+8(%rip)
	movq	make_loop_table.loop_length_in(%rip), %rax
	movq	%rax, loop_length(%rip)
	movq	$0, loop_coeff+16(%rip)
	movq	$0, loop_coeff+8(%rip)
	movq	u0(%rip), %rbx
	movq	$0, loop_coeff(%rip)
	movabsq	$4607182418800017408, %r14 # imm = 0x3FF0000000000000
	movq	%r14, loop_coeff(%rip)
	movabsq	$4626322717216342016, %rsi # imm = 0x4034000000000000
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movabsq	$-4616189618054758400, %rdi # imm = 0xBFF0000000000000
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	log
	movabsq	$-4619554707696329635, %rsi # imm = 0xBFE40B780346DC5D
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_add
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	u0(%rip), %rbx
	movq	%rax, loop_coeff+8(%rip)
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movabsq	$4586408214438882784, %rsi # imm = 0x3FA631F8A0902DE0
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	log
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, loop_coeff+16(%rip)
	movq	$2256495, %rax          # imm = 0x226E6F
	movl	%eax, gauge_action_description+32(%rip)
	movabsq	$7598807740083959857, %rax # imm = 0x6974636120317831
	movq	%rax, gauge_action_description+24(%rip)
	movabsq	$8660738928006035505, %rax # imm = 0x7831202B20327831
	movq	%rax, gauge_action_description+16(%rip)
	movabsq	$2317981830054223979, %rax # imm = 0x202B20317831206B
	movl	this_node(%rip), %ecx
	movq	%rax, gauge_action_description+8(%rip)
	movabsq	$7600508685868684066, %rax # imm = 0x697A6E616D795322
	movq	%rax, gauge_action_description(%rip)
	testq	%rcx, %rcx
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.Lstr15, %rdi
	callq	puts
.LBB0_2:                                # %for.cond26.preheader
	movabsq	$make_loop_table.loop_ind, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB0_3:                                # %for.body28
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_4 Depth 2
                                        #       Child Loop BB0_5 Depth 3
                                        #         Child Loop BB0_6 Depth 4
                                        #           Child Loop BB0_7 Depth 5
                                        #             Child Loop BB0_20 Depth 6
                                        #               Child Loop BB0_21 Depth 7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	loop_length(,%rax,4), %ebx
	xorq	%rdx, %rdx
	movl	%edx, 144(%rsp)
	leaq	-1(%rbx), %rax
	andq	%r12, %rax
	leaq	4(,%rax,4), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%rax, %rax
	xorq	%r14, %r14
.LBB0_4:                                # %for.body35
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_5 Depth 3
                                        #         Child Loop BB0_6 Depth 4
                                        #           Child Loop BB0_7 Depth 5
                                        #             Child Loop BB0_20 Depth 6
                                        #               Child Loop BB0_21 Depth 7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	%edx, 148(%rsp)
	xorq	%rsi, %rsi
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB0_5:                                # %for.body40
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_6 Depth 4
                                        #           Child Loop BB0_7 Depth 5
                                        #             Child Loop BB0_20 Depth 6
                                        #               Child Loop BB0_21 Depth 7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	xorq	%rax, %rax
	movl	%eax, 152(%rsp)
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
.LBB0_6:                                # %for.body45
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB0_7 Depth 5
                                        #             Child Loop BB0_20 Depth 6
                                        #               Child Loop BB0_21 Depth 7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movl	%eax, 156(%rsp)
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
.LBB0_7:                                # %for.body50
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB0_20 Depth 6
                                        #               Child Loop BB0_21 Depth 7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movq	%rsi, %rdx
	andq	%r12, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	andq	%r12, %rdi
	cmpq	%rdx, %rdi
	jne	.LBB0_9
# BB#8:                                 #   in Loop: Header=BB0_7 Depth=5
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	jmp	.LBB0_74
	.align	16, 0x90
.LBB0_9:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_7 Depth=5
	movq	152(%rsp), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rax, %r9
	andq	%r12, %r9
	movq	$32, %rcx
	movq	%rax, %r8
	shrq	%cl, %r8
	cmpq	%r9, %rdi
	je	.LBB0_10
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_7 Depth=5
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rcx, %rdi
	je	.LBB0_12
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_7 Depth=5
	cmpq	%r9, %rdx
	je	.LBB0_14
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_7 Depth=5
	cmpq	%rcx, %rdx
	je	.LBB0_16
# BB#17:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_7 Depth=5
	cmpq	%rcx, %r9
	je	.LBB0_18
# BB#19:                                #   in Loop: Header=BB0_7 Depth=5
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
.LBB0_20:                               # %for.body78
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB0_21 Depth 7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_21:                               # %for.body83
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        # =>            This Loop Header: Depth=7
                                        #                 Child Loop BB0_22 Depth 8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB0_22:                               # %for.body88
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        #               Parent Loop BB0_21 Depth=7
                                        # =>              This Loop Header: Depth=8
                                        #                   Child Loop BB0_46 Depth 9
                                        #                     Child Loop BB0_60 Depth 10
                                        #                   Child Loop BB0_23 Depth 9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	jle	.LBB0_46
	.align	16, 0x90
.LBB0_23:                               # %for.cond94.preheader.us
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        #               Parent Loop BB0_21 Depth=7
                                        #                 Parent Loop BB0_22 Depth=8
                                        # =>                This Loop Header: Depth=9
                                        #                     Child Loop BB0_36 Depth 10
                                        #                     Child Loop BB0_39 Depth 10
	movq	32(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_24
# BB#25:                                # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_26
	.align	16, 0x90
.LBB0_24:                               #   in Loop: Header=BB0_23 Depth=9
	movq	$7, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
.LBB0_26:                               # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movl	%eax, 112(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 140(%rsp)
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_27
# BB#28:                                # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	%rsi, %rax
	jmp	.LBB0_29
	.align	16, 0x90
.LBB0_27:                               #   in Loop: Header=BB0_23 Depth=9
	movq	$7, %rax
	subq	%rsi, %rax
.LBB0_29:                               # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movl	%eax, 116(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 136(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_30
# BB#31:                                # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	40(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_32
	.align	16, 0x90
.LBB0_30:                               #   in Loop: Header=BB0_23 Depth=9
	movq	$7, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
.LBB0_32:                               # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movl	%eax, 120(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 132(%rsp)
	movq	%r15, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_33
# BB#34:                                # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	48(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_35
	.align	16, 0x90
.LBB0_33:                               #   in Loop: Header=BB0_23 Depth=9
	movq	$7, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
.LBB0_35:                               # %for.cond94.preheader.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	%rsi, %r13
	movl	%eax, 124(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 128(%rsp)
	movq	$1, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	.align	16, 0x90
.LBB0_36:                               # %for.body122.us
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        #               Parent Loop BB0_21 Depth=7
                                        #                 Parent Loop BB0_22 Depth=8
                                        #                   Parent Loop BB0_23 Depth=9
                                        # =>                  This Inner Loop Header: Depth=10
	movslq	(%rcx), %rdx
	movl	112(%rsp,%rdx,4), %edx
	movl	%edx, 76(%rsp,%rax,4)
	movq	%rax, %rdx
	andq	%r12, %rdx
	movq	%rbx, %rsi
	andq	%r12, %rsi
	incq	%rax
	addq	$4, %rcx
	cmpq	%rsi, %rdx
	jne	.LBB0_36
# BB#37:                                # %for.end133.us
                                        #   in Loop: Header=BB0_23 Depth=9
	leaq	80(%rsp), %rdi
	leaq	108(%rsp), %rsi
	movq	%rbx, %rdx
	callq	char_num
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_43
# BB#38:                                # %for.body136.lr.ph.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movl	108(%rsp), %eax
	xorq	%rsi, %rsi
	movq	$1, %rcx
	.align	16, 0x90
.LBB0_39:                               # %for.body136.us
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        #               Parent Loop BB0_21 Depth=7
                                        #                 Parent Loop BB0_22 Depth=8
                                        #                   Parent Loop BB0_23 Depth=9
                                        # =>                  This Inner Loop Header: Depth=10
	movl	loop_char-4(,%rcx,4), %edx
	movq	%rax, %rdi
	andq	%r12, %rdi
	cmpq	%rdx, %rdi
	movq	$1, %rdx
	je	.LBB0_41
# BB#40:                                # %select.mid
                                        #   in Loop: Header=BB0_39 Depth=10
	movq	%rsi, %rdx
.LBB0_41:                               # %select.end
                                        #   in Loop: Header=BB0_39 Depth=10
	movq	%rcx, %rsi
	andq	%r12, %rsi
	movq	%r14, %rdi
	andq	%r12, %rdi
	incq	%rcx
	cmpq	%rdi, %rsi
	movq	%rdx, %rsi
	jne	.LBB0_39
# BB#42:                                # %for.end144.us
                                        #   in Loop: Header=BB0_23 Depth=9
	testq	%r12, %rdx
	jne	.LBB0_44
.LBB0_43:                               # %for.end162.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movl	108(%rsp), %eax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	%eax, loop_char(,%rdx,4)
	addq	%rdx, %rdx
	leaq	(%rdx,%rdx,2), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rcx,2), %rdx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	loop_table(%rdx,%rax,4), %rdi
	leaq	80(%rsp), %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	memcpy
	incq	%r14
.LBB0_44:                               # %if.end164.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	%r13, %rsi
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$16, %rax
	jg	.LBB0_66
# BB#45:                                # %if.end171.us
                                        #   in Loop: Header=BB0_23 Depth=9
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	%r14d, loop_num(,%rax,4)
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_23
	jmp	.LBB0_71
	.align	16, 0x90
.LBB0_46:                               # %for.cond94.preheader
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        #               Parent Loop BB0_21 Depth=7
                                        #                 Parent Loop BB0_22 Depth=8
                                        # =>                This Loop Header: Depth=9
                                        #                     Child Loop BB0_60 Depth 10
	movq	32(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_47
# BB#48:                                # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movq	16(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_49
	.align	16, 0x90
.LBB0_47:                               #   in Loop: Header=BB0_46 Depth=9
	movq	$7, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
.LBB0_49:                               # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movl	%eax, 112(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 140(%rsp)
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_50
# BB#51:                                # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movq	%rsi, %rax
	jmp	.LBB0_52
	.align	16, 0x90
.LBB0_50:                               #   in Loop: Header=BB0_46 Depth=9
	movq	$7, %rax
	subq	%rsi, %rax
.LBB0_52:                               # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movl	%eax, 116(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 136(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB0_53
# BB#54:                                # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movq	40(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_55
	.align	16, 0x90
.LBB0_53:                               #   in Loop: Header=BB0_46 Depth=9
	movq	$7, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
.LBB0_55:                               # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movl	%eax, 120(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 132(%rsp)
	movq	%r15, %rax
	andq	%r12, %rax
	movq	%rsi, %r13
	cmpq	$1, %rax
	je	.LBB0_56
# BB#57:                                # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movq	48(%rsp), %rax          # 8-byte Reload
	jmp	.LBB0_58
	.align	16, 0x90
.LBB0_56:                               #   in Loop: Header=BB0_46 Depth=9
	movq	$7, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
.LBB0_58:                               # %for.cond94.preheader
                                        #   in Loop: Header=BB0_46 Depth=9
	movl	%eax, 124(%rsp)
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 128(%rsp)
	leaq	80(%rsp), %rdi
	leaq	108(%rsp), %rsi
	movq	%rbx, %rdx
	callq	char_num
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_64
# BB#59:                                # %for.body136.lr.ph
                                        #   in Loop: Header=BB0_46 Depth=9
	movl	108(%rsp), %eax
	xorq	%rsi, %rsi
	movq	$1, %rcx
	.align	16, 0x90
.LBB0_60:                               # %for.body136
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        #       Parent Loop BB0_5 Depth=3
                                        #         Parent Loop BB0_6 Depth=4
                                        #           Parent Loop BB0_7 Depth=5
                                        #             Parent Loop BB0_20 Depth=6
                                        #               Parent Loop BB0_21 Depth=7
                                        #                 Parent Loop BB0_22 Depth=8
                                        #                   Parent Loop BB0_46 Depth=9
                                        # =>                  This Inner Loop Header: Depth=10
	movl	loop_char-4(,%rcx,4), %edx
	movq	%rax, %rdi
	andq	%r12, %rdi
	cmpq	%rdx, %rdi
	movq	$1, %rdx
	je	.LBB0_62
# BB#61:                                # %select.mid38
                                        #   in Loop: Header=BB0_60 Depth=10
	movq	%rsi, %rdx
.LBB0_62:                               # %select.end37
                                        #   in Loop: Header=BB0_60 Depth=10
	movq	%rcx, %rsi
	andq	%r12, %rsi
	movq	%r14, %rdi
	andq	%r12, %rdi
	incq	%rcx
	cmpq	%rdi, %rsi
	movq	%rdx, %rsi
	jne	.LBB0_60
# BB#63:                                # %for.end144
                                        #   in Loop: Header=BB0_46 Depth=9
	testq	%r12, %rdx
	jne	.LBB0_65
.LBB0_64:                               # %for.end162
                                        #   in Loop: Header=BB0_46 Depth=9
	movl	108(%rsp), %eax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	%eax, loop_char(,%rdx,4)
	incq	%r14
.LBB0_65:                               # %if.end164
                                        #   in Loop: Header=BB0_46 Depth=9
	movq	%r13, %rsi
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$17, %rax
	jge	.LBB0_66
# BB#69:                                # %if.end171
                                        #   in Loop: Header=BB0_46 Depth=9
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	%r14d, loop_num(,%rax,4)
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_46
# BB#70:                                #   in Loop: Header=BB0_22 Depth=8
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB0_71:                               # %for.inc178
                                        #   in Loop: Header=BB0_22 Depth=8
	movq	64(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_22
# BB#72:                                # %for.inc182
                                        #   in Loop: Header=BB0_21 Depth=7
	movq	56(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_21
# BB#73:                                # %for.inc186
                                        #   in Loop: Header=BB0_20 Depth=6
	movq	32(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB0_20
	jmp	.LBB0_74
.LBB0_18:                               #   in Loop: Header=BB0_7 Depth=5
	movq	%r8, 48(%rsp)           # 8-byte Spill
	jmp	.LBB0_74
.LBB0_16:                               #   in Loop: Header=BB0_7 Depth=5
	movq	%r8, 48(%rsp)           # 8-byte Spill
	jmp	.LBB0_74
.LBB0_14:                               #   in Loop: Header=BB0_7 Depth=5
	movq	%r8, 48(%rsp)           # 8-byte Spill
	jmp	.LBB0_74
.LBB0_10:                               #   in Loop: Header=BB0_7 Depth=5
	movq	%r8, 48(%rsp)           # 8-byte Spill
	jmp	.LBB0_74
.LBB0_12:                               #   in Loop: Header=BB0_7 Depth=5
	movq	%r8, 48(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB0_74:                               # %for.inc191
                                        #   in Loop: Header=BB0_7 Depth=5
	movq	48(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movl	%edx, 156(%rsp)
	jl	.LBB0_7
# BB#75:                                # %for.inc195
                                        #   in Loop: Header=BB0_6 Depth=4
	movq	40(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movl	%edx, 152(%rsp)
	movabsq	$0, %rax
	jl	.LBB0_6
# BB#76:                                # %for.inc199
                                        #   in Loop: Header=BB0_5 Depth=3
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movl	%esi, 148(%rsp)
	movabsq	$0, %rdx
	jl	.LBB0_5
# BB#77:                                # %for.inc203
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	24(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movl	%esi, 144(%rsp)
	movq	%rsi, %rax
	jl	.LBB0_4
# BB#78:                                # %for.inc207
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$24, 8(%rsp)            # 8-byte Folded Spill
	cmpq	$3, %rax
	jl	.LBB0_3
# BB#79:                                # %for.end209
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_83
# BB#80:                                # %for.cond214.preheader
	movabsq	$.Lstr, %rax
	movq	%rax, %rdi
	callq	puts
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_83
# BB#81:                                # %for.inc230
	movl	loop_num(%rip), %ebx
	movq	loop_coeff(%rip), %rcx
	movabsq	$.L.str4, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %r8
	callq	printf
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_83
# BB#82:                                # %for.inc230.1
	movl	loop_num+4(%rip), %ebx
	movq	loop_coeff+8(%rip), %rcx
	movabsq	$.L.str4, %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %r8
	callq	printf
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	je	.LBB0_84
.LBB0_83:                               # %for.inc230.2
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_66:                               # %if.then166
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_68
# BB#67:                                # %if.then168
	movabsq	$.Lstr14, %rdi
	callq	puts
.LBB0_68:                               # %if.end170
	xorq	%rdi, %rdi
	callq	exit
.LBB0_84:                               # %if.then221.2
	movl	loop_num+8(%rip), %ebx
	movq	loop_coeff+16(%rip), %rcx
	movabsq	$.L.str4, %rdi
	movq	$2, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rbx, %r8
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	printf  # TAILCALL
.Ltmp17:
	.size	make_loop_table, .Ltmp17-make_loop_table
	.cfi_endproc

	.globl	char_num
	.align	16, 0x90
	.type	char_num,@function
char_num:                               # @char_num
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
	leaq	-1(%rdx), %r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %r14
	jle	.LBB1_3
# BB#1:
	xorq	%rcx, %rcx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r8, %rbx
	andq	%r9, %rbx
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	addq	%r14, %r14
	incq	%rcx
	leaq	(%r14,%r14,4), %r14
	movq	%rcx, %rax
	andq	%r9, %rax
	cmpq	%rbx, %rax
	jne	.LBB1_2
.LBB1_3:                                # %for.end
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, -40(%rsp)         # 8-byte Spill
	movl	(%rdi,%rax,4), %r9d
	movl	%r9d, (%rsi)
	leaq	-2(%rdx), %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_6
# BB#4:                                 # %for.body5.lr.ph
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	.align	16, 0x90
.LBB1_5:                                # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%r9,%r9,4), %rcx
	movl	(%rdi,%rax,4), %ebx
	leaq	(%rbx,%rcx,2), %r9
	movl	%r9d, (%rsi)
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rax
	testq	%rbx, %rbx
	jg	.LBB1_5
.LBB1_6:                                # %for.cond12.preheader
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_11
# BB#7:                                 # %for.body14.preheader
	leaq	-1(%rdx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rdi,%rax,4), %r13
	movq	%r9, %r15
	movq	%r8, %r11
	.align	16, 0x90
.LBB1_8:                                # %for.body14
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %r12d
	movq	%r12, %rcx
	imulq	%r14, %rcx
	subq	%rcx, %r15
	leaq	(%r15,%r15,4), %rcx
	leaq	(%r12,%rcx,2), %r15
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB1_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB1_8 Depth=1
	movl	%r15d, (%rsi)
	movq	%r15, %r9
.LBB1_10:                               # %if.end
                                        #   in Loop: Header=BB1_8 Depth=1
	decq	%r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$-4, %r13
	testq	%rax, %rax
	jg	.LBB1_8
.LBB1_11:                               # %for.cond27.preheader
	movq	$32, %rcx
	movq	%rdx, %r11
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
                                        # implicit-def: RBX
	jle	.LBB1_15
# BB#12:
	xorq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r11, %rbx
	andq	%r15, %rbx
	.align	16, 0x90
.LBB1_13:                               # %for.body29
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rdi,%rdx,4), %ecx
	movq	$7, %rdx
	subq	%rcx, %rdx
	movl	%edx, -32(%rsp,%rax,4)
	incq	%rax
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	%rbx, %rcx
	jne	.LBB1_13
# BB#14:                                # %for.cond27.for.end39_crit_edge
	movq	-40(%rsp), %rax         # 8-byte Reload
	movl	-32(%rsp,%rax,4), %ebx
.LBB1_15:                               # %for.end39
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %rdi
	js	.LBB1_18
# BB#16:                                # %for.body46.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	%rbx, %rdi
	.align	16, 0x90
.LBB1_17:                               # %for.body46
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%rdi,%rdi,4), %rax
	movl	-32(%rsp,%r10,4), %ecx
	leaq	(%rcx,%rax,2), %rdi
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r10
	testq	%rax, %rax
	jg	.LBB1_17
.LBB1_18:                               # %for.end53
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB1_20
# BB#19:                                # %if.then55
	movl	%edi, (%rsi)
	movq	%rdi, %r9
.LBB1_20:                               # %for.cond58.preheader
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_26
# BB#21:                                # %for.body60.preheader
	decq	%r11
	movq	$32, %rcx
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	leaq	-36(%rsp,%r11,4), %rdx
	jmp	.LBB1_22
	.align	16, 0x90
.LBB1_25:                               # %if.end71.for.body60_crit_edge
                                        #   in Loop: Header=BB1_22 Depth=1
	movl	(%rdx), %ebx
	addq	$-4, %rdx
.LBB1_22:                               # %for.body60
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%r14, %rax
	subq	%rax, %rdi
	leaq	(%rdi,%rdi,4), %rax
	leaq	(%rbx,%rax,2), %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r9, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB1_24
# BB#23:                                # %if.then70
                                        #   in Loop: Header=BB1_22 Depth=1
	movl	%edi, (%rsi)
	movq	%rdi, %r9
.LBB1_24:                               # %if.end71
                                        #   in Loop: Header=BB1_22 Depth=1
	decq	%r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_25
.LBB1_26:                               # %for.end74
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	char_num, .Ltmp34-char_num
	.cfi_endproc

	.globl	imp_gauge_action
	.align	16, 0x90
	.type	imp_gauge_action,@function
imp_gauge_action:                       # @imp_gauge_action
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
	subq	$16, %rsp
.Ltmp46:
	.cfi_def_cfa_offset 64
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
	movslq	loop_num(%rip), %rax
	movq	$0, 8(%rsp)
	testq	%rax, %rax
	jle	.LBB2_6
# BB#1:
	movl	loop_length(%rip), %r14d
	xorq	%r12, %r12
	movabsq	$4613937818241073152, %r15 # imm = 0x4008000000000000
	.align	16, 0x90
.LBB2_2:                                # %for.body5
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_4 Depth 2
	leaq	(%r12,%r12,2), %rax
	leaq	loop_table(,%rax,8), %rdi
	movq	%r14, %rsi
	callq	path_product
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_5
# BB#3:                                 # %for.body12.lr.ph
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	lattice(%rip), %r13
	addq	$2912, %r13             # imm = 0xB60
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_4:                                # %for.body12
                                        #   Parent Loop BB2_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rdi
	callq	trace_su3
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	loop_coeff(%rip), %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB2_4
.LBB2_5:                                # %for.inc29
                                        #   in Loop: Header=BB2_2 Depth=1
	incq	%r12
	movslq	loop_num(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB2_2
.LBB2_6:                                # %for.inc32
	movslq	loop_num+4(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_12
# BB#7:
	movl	loop_length+4(%rip), %r14d
	xorq	%r12, %r12
	movabsq	$4613937818241073152, %r15 # imm = 0x4008000000000000
	.align	16, 0x90
.LBB2_8:                                # %for.body5.1
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_10 Depth 2
	leaq	(%r12,%r12,2), %rax
	leaq	loop_table+384(,%rax,8), %rdi
	movq	%r14, %rsi
	callq	path_product
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_11
# BB#9:                                 # %for.body12.lr.ph.1
                                        #   in Loop: Header=BB2_8 Depth=1
	movq	lattice(%rip), %r13
	addq	$2912, %r13             # imm = 0xB60
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_10:                               # %for.body12.1
                                        #   Parent Loop BB2_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rdi
	callq	trace_su3
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	loop_coeff+8(%rip), %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB2_10
.LBB2_11:                               # %for.inc29.1
                                        #   in Loop: Header=BB2_8 Depth=1
	incq	%r12
	movslq	loop_num+4(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB2_8
.LBB2_12:                               # %for.inc32.1
	movslq	loop_num+8(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_18
# BB#13:
	movl	loop_length+8(%rip), %r14d
	xorq	%r12, %r12
	movabsq	$4613937818241073152, %r15 # imm = 0x4008000000000000
	.align	16, 0x90
.LBB2_14:                               # %for.body5.2
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_16 Depth 2
	leaq	(%r12,%r12,2), %rax
	leaq	loop_table+768(,%rax,8), %rdi
	movq	%r14, %rsi
	callq	path_product
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB2_17
# BB#15:                                # %for.body12.lr.ph.2
                                        #   in Loop: Header=BB2_14 Depth=1
	movq	lattice(%rip), %r13
	addq	$2912, %r13             # imm = 0xB60
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB2_16:                               # %for.body12.2
                                        #   Parent Loop BB2_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %rdi
	callq	trace_su3
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	loop_coeff+16(%rip), %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	8(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB2_16
.LBB2_17:                               # %for.inc29.2
                                        #   in Loop: Header=BB2_14 Depth=1
	incq	%r12
	movslq	loop_num+8(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB2_14
.LBB2_18:                               # %for.inc32.2
	leaq	8(%rsp), %rdi
	callq	g_doublesum
	movq	8(%rsp), %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	imp_gauge_action, .Ltmp52-imp_gauge_action
	.cfi_endproc

	.globl	imp_gauge_force
	.align	16, 0x90
	.type	imp_gauge_force,@function
imp_gauge_force:                        # @imp_gauge_force
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp61:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp62:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp63:
	.cfi_def_cfa_offset 48
	subq	$464, %rsp              # imm = 0x1D0
.Ltmp64:
	.cfi_def_cfa_offset 512
.Ltmp65:
	.cfi_offset %rbx, -48
.Ltmp66:
	.cfi_offset %r12, -40
.Ltmp67:
	.cfi_offset %r13, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	beta(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	float64_mul
	movabsq	$4613937818241073152, %rsi # imm = 0x4008000000000000
	movq	%rax, %rdi
	callq	float64_div
	movq	%rax, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movl	sites_on_node(%rip), %r8d
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	leaq	320(%rsp), %rdx
	.align	16, 0x90
.LBB3_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_3 Depth 2
                                        #     Child Loop BB3_5 Depth 2
                                        #       Child Loop BB3_8 Depth 3
                                        #         Child Loop BB3_9 Depth 4
                                        #         Child Loop BB3_14 Depth 4
                                        #           Child Loop BB3_19 Depth 5
                                        #           Child Loop BB3_26 Depth 5
                                        #           Child Loop BB3_29 Depth 5
                                        #     Child Loop BB3_35 Depth 2
	movq	%rdx, %r15
	movq	%r13, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB3_4
# BB#2:                                 # %for.cond4.preheader.lr.ph
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	lattice(%rip), %rbx
	addq	$3192, %rbx             # imm = 0xC78
	xorq	%r14, %r14
	.align	16, 0x90
.LBB3_3:                                # %for.cond4.preheader
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -136(%rbx)
	movq	%rdx, -128(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -120(%rbx)
	movq	%rdx, -112(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -104(%rbx)
	movq	%rdx, -96(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -88(%rbx)
	movq	%rdx, -80(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -72(%rbx)
	movq	%rdx, -64(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -56(%rbx)
	movq	%rdx, -48(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -40(%rbx)
	movq	%rdx, -32(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -24(%rbx)
	movq	%rdx, -16(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -8(%rbx)
	movq	%rdx, (%rbx)
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movslq	sites_on_node(%rip), %r8
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%r8, %rdx
	jl	.LBB3_3
.LBB3_4:                                # %for.cond18.preheader
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$7, %rax
	subq	%r13, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	leaq	7(%r13), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%r11, %r11
	movabsq	$loop_table, %rax
	movabsq	$4294967292, %r9        # imm = 0xFFFFFFFC
	.align	16, 0x90
.LBB3_5:                                # %for.body20
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_8 Depth 3
                                        #         Child Loop BB3_9 Depth 4
                                        #         Child Loop BB3_14 Depth 4
                                        #           Child Loop BB3_19 Depth 5
                                        #           Child Loop BB3_26 Depth 5
                                        #           Child Loop BB3_29 Depth 5
	movslq	loop_num(,%r11,4), %rcx
	testq	%rcx, %rcx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	jle	.LBB3_6
# BB#7:                                 # %for.cond28.preheader.lr.ph
                                        #   in Loop: Header=BB3_5 Depth=2
	movl	loop_length(,%r11,4), %r14d
	movq	%r14, 56(%rsp)          # 8-byte Spill
	leaq	-1(%r14), %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	leaq	-2(%r14), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_8:                                # %for.cond28.preheader
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_5 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_9 Depth 4
                                        #         Child Loop BB3_14 Depth 4
                                        #           Child Loop BB3_19 Depth 5
                                        #           Child Loop BB3_26 Depth 5
                                        #           Child Loop BB3_29 Depth 5
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rdx
	testq	%rsi, %rsi
	movq	%rdi, %rsi
	jle	.LBB3_31
	.align	16, 0x90
.LBB3_9:                                # %for.body30
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_5 Depth=2
                                        #       Parent Loop BB3_8 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movslq	(%rsi), %rbx
	cmpq	$3, %rbx
	jg	.LBB3_11
# BB#10:                                # %if.then
                                        #   in Loop: Header=BB3_9 Depth=4
	addq	%r13, %rbx
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$61, %rcx
	shrq	%cl, %rdi
	andq	$3, %rdi
	addq	%rbx, %rdi
	andq	%r9, %rdi
	subq	%rdi, %rbx
	movl	%ebx, 140(%rsp,%rdx,4)
	jmp	.LBB3_12
	.align	16, 0x90
.LBB3_11:                               # %if.else
                                        #   in Loop: Header=BB3_9 Depth=4
	movq	72(%rsp), %rax          # 8-byte Reload
	subq	%rbx, %rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$61, %rcx
	shrq	%cl, %rdi
	andq	$3, %rdi
	addq	%rax, %rdi
	andq	%r9, %rdi
	subq	%rdi, %rax
	movq	$7, %rcx
	subq	%rax, %rcx
	movl	%ecx, 140(%rsp,%rdx,4)
.LBB3_12:                               # %for.inc57
                                        #   in Loop: Header=BB3_9 Depth=4
	movq	%rdx, %rax
	andq	%r10, %rax
	movq	%r14, %r12
	andq	%r10, %r12
	incq	%rdx
	addq	$4, %rsi
	cmpq	%r12, %rax
	jne	.LBB3_9
# BB#13:                                # %for.end59
                                        #   in Loop: Header=BB3_8 Depth=3
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	jle	.LBB3_31
	.align	16, 0x90
.LBB3_14:                               # %for.body63
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_5 Depth=2
                                        #       Parent Loop BB3_8 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB3_19 Depth 5
                                        #           Child Loop BB3_26 Depth 5
                                        #           Child Loop BB3_29 Depth 5
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movl	144(%rsp,%rcx,4), %r9d
	movq	%r13, %rax
	andq	%r10, %rax
	cmpq	%rax, %r9
	movq	%r11, %r14
	je	.LBB3_16
# BB#15:                                # %for.body63
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	64(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %r9
	jne	.LBB3_30
.LBB3_16:                               # %if.then71
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	%r14, %r13
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	cmpq	$3, %rax
	jg	.LBB3_24
# BB#17:                                # %for.cond76.preheader
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r11          # 8-byte Reload
	jle	.LBB3_20
# BB#18:                                # %for.body78.lr.ph
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %r8
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB3_19:                               # %for.body78
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_5 Depth=2
                                        #       Parent Loop BB3_8 Depth=3
                                        #         Parent Loop BB3_14 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	leaq	(%r8,%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	144(%rsp,%rdx,4), %eax
	movl	%eax, 112(%rsp,%rbx,4)
	incq	%rbx
	movq	%rbx, %rax
	andq	%r10, %rax
	movq	%rsi, %rcx
	andq	%r10, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_19
.LBB3_20:                               # %if.end89
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	88(%rsp), %r14          # 8-byte Reload
	jg	.LBB3_22
# BB#21:                                # %if.end89
                                        #   in Loop: Header=BB3_14 Depth=4
	movabsq	$1, %rax
.LBB3_22:                               # %if.end89
                                        #   in Loop: Header=BB3_14 Depth=4
	cmpq	$4, %r9
	jl	.LBB3_27
# BB#23:                                # %if.end89
                                        #   in Loop: Header=BB3_14 Depth=4
	testq	%rax, %rax
	je	.LBB3_25
	jmp	.LBB3_27
	.align	16, 0x90
.LBB3_24:                               # %for.cond94.preheader
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r11          # 8-byte Reload
	movq	88(%rsp), %r14          # 8-byte Reload
	jle	.LBB3_27
.LBB3_25:                               # %for.body96.lr.ph
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %r8
	movq	$1, %rdi
	.align	16, 0x90
.LBB3_26:                               # %for.body96
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_5 Depth=2
                                        #       Parent Loop BB3_8 Depth=3
                                        #         Parent Loop BB3_14 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	leaq	-1(%rdi), %r9
	leaq	-1(%r8,%rdi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cqto
	idivq	%rbx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	144(%rsp,%rdx,4), %eax
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	%r14, %rax
	subq	%r9, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%edx, 112(%rsp,%rax,4)
	movq	%rdi, %rax
	andq	%r10, %rax
	movq	%rsi, %rcx
	andq	%r10, %rcx
	incq	%rdi
	cmpq	%rcx, %rax
	jne	.LBB3_26
.LBB3_27:                               # %if.end110
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	%r14, 88(%rsp)          # 8-byte Spill
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	leaq	112(%rsp), %rdi
	callq	path_product
	movslq	sites_on_node(%rip), %r8
	testq	%r8, %r8
	movq	%r13, %r14
	jle	.LBB3_30
# BB#28:                                # %for.body113.lr.ph
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	lattice(%rip), %rbx
	addq	$3056, %rbx             # imm = 0xBF0
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_29:                               # %for.body113
                                        #   Parent Loop BB3_1 Depth=1
                                        #     Parent Loop BB3_5 Depth=2
                                        #       Parent Loop BB3_8 Depth=3
                                        #         Parent Loop BB3_14 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	leaq	-144(%rbx), %rdi
	movq	%r15, %rsi
	callq	su3_adjoint
	movq	loop_coeff(,%r14,8), %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rcx
	callq	scalar_mult_add_su3_matrix
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %r8
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%r8, %rax
	jl	.LBB3_29
.LBB3_30:                               # %for.inc125
                                        #   in Loop: Header=BB3_14 Depth=4
	movq	%r14, %r11
	movq	104(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	movq	%rcx, %rax
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %rax
	cmpq	%r12, %rax
	movq	80(%rsp), %r13          # 8-byte Reload
	jne	.LBB3_14
.LBB3_31:                               # %for.inc128
                                        #   in Loop: Header=BB3_8 Depth=3
	movq	48(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movslq	loop_num(,%r11,4), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	40(%rsp), %rdi          # 8-byte Reload
	addq	$24, %rdi
	cmpq	%rax, %rdx
	movabsq	$4294967292, %r9        # imm = 0xFFFFFFFC
	movq	56(%rsp), %r14          # 8-byte Reload
	jl	.LBB3_8
	jmp	.LBB3_32
	.align	16, 0x90
.LBB3_6:                                #   in Loop: Header=BB3_5 Depth=2
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB3_32:                               # %for.inc131
                                        #   in Loop: Header=BB3_5 Depth=2
	incq	%r11
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	$384, %rax              # imm = 0x180
	cmpq	$3, %r11
	jne	.LBB3_5
# BB#33:                                # %for.end133
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r15, %rdx
	leaq	176(%rsp), %rax
	movq	%rax, %r12
	jle	.LBB3_36
# BB#34:                                # %for.body136.lr.ph
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	lattice(%rip), %rax
	leaq	(%r13,%r13,8), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	leaq	112(%rax,%rsi), %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx), %rbx
	leaq	3056(%rax), %r14
	xorq	%r15, %r15
	.align	16, 0x90
.LBB3_35:                               # %for.body136
                                        #   Parent Loop BB3_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	mult_su3_na
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	uncompress_anti_hermitian
	movq	%r12, %rdi
	leaq	320(%rsp), %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	callq	scalar_mult_sub_su3_matrix
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	make_anti_hermitian
	movq	104(%rsp), %rsi         # 8-byte Reload
	leaq	320(%rsp), %rdx
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movslq	sites_on_node(%rip), %r8
	addq	$3200, %rbx             # imm = 0xC80
	addq	$3200, %r14             # imm = 0xC80
	addq	$3200, %rsi             # imm = 0xC80
	cmpq	%r8, %rax
	jl	.LBB3_35
.LBB3_36:                               # %for.inc150
                                        #   in Loop: Header=BB3_1 Depth=1
	incq	%r13
	addq	$80, 16(%rsp)           # 8-byte Folded Spill
	cmpq	$4, %r13
	jne	.LBB3_1
# BB#37:                                # %for.end152
	addq	$464, %rsp              # imm = 0x1D0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp70:
	.size	imp_gauge_force, .Ltmp70-imp_gauge_force
	.cfi_endproc

	.globl	g_measure
	.align	16, 0x90
	.type	g_measure,@function
g_measure:                              # @g_measure
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp79:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp80:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp82:
	.cfi_def_cfa_offset 112
.Ltmp83:
	.cfi_offset %rbx, -48
.Ltmp84:
	.cfi_offset %r12, -40
.Ltmp85:
	.cfi_offset %r13, -32
.Ltmp86:
	.cfi_offset %r14, -24
.Ltmp87:
	.cfi_offset %r15, -16
	leaq	56(%rsp), %rdi
	leaq	48(%rsp), %rsi
	callq	d_plaquette
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_2
# BB#1:                                 # %if.then
	movq	48(%rsp), %rdx
	movq	56(%rsp), %rsi
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	printf
.LBB4_2:                                # %if.end
	callq	ploop
	movl	this_node(%rip), %esi
	movq	%rax, %rcx
	testq	%rsi, %rsi
	jne	.LBB4_4
# BB#3:                                 # %if.then3
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	printf
.LBB4_4:                                # %if.end5
	movq	$0, 32(%rsp)
	xorq	%r15, %r15
	movabsq	$loop_table, %rcx
	movabsq	$4613937818241073152, %rbx # imm = 0x4008000000000000
.LBB4_5:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_7 Depth 2
                                        #       Child Loop BB4_9 Depth 3
                                        #       Child Loop BB4_21 Depth 3
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movslq	loop_num(,%r15,4), %rax
	testq	%rax, %rax
	jle	.LBB4_29
# BB#6:                                 # %for.body11.lr.ph
                                        #   in Loop: Header=BB4_5 Depth=1
	movl	loop_length(,%r15,4), %r8d
	xorq	%rsi, %rsi
	movq	%rcx, 8(%rsp)           # 8-byte Spill
.LBB4_7:                                # %for.body11
                                        #   Parent Loop BB4_5 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_9 Depth 3
                                        #       Child Loop BB4_21 Depth 3
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	leaq	(%r15,%r15,2), %rax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	(%rsi,%rsi,2), %rcx
	leaq	loop_table(%rax,%rcx,8), %rdi
	movq	%r8, %rsi
	callq	path_product
	movslq	sites_on_node(%rip), %rax
	movq	$0, 40(%rsp)
	testq	%rax, %rax
	jle	.LBB4_10
# BB#8:                                 # %for.body23.lr.ph
                                        #   in Loop: Header=BB4_7 Depth=2
	movq	lattice(%rip), %r13
	addq	$2912, %r13             # imm = 0xB60
	xorq	%r14, %r14
	.align	16, 0x90
.LBB4_9:                                # %for.body23
                                        #   Parent Loop BB4_5 Depth=1
                                        #     Parent Loop BB4_7 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r13, %rdi
	callq	trace_su3
	movq	40(%rsp), %rdi
	movq	%rax, %r12
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, 40(%rsp)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	loop_coeff(,%r15,8), %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	32(%rsp), %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 32(%rsp)
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB4_9
.LBB4_10:                               # %for.end51
                                        #   in Loop: Header=BB4_7 Depth=2
	movq	$1, %rsi
	leaq	40(%rsp), %rdi
	callq	g_vecdoublesum
	movl	this_node(%rip), %eax
	xorq	%r13, %r13
	testq	%rax, %rax
	jne	.LBB4_15
# BB#11:                                # %if.then54
                                        #   in Loop: Header=BB4_7 Depth=2
	xorq	%rax, %rax
	xorq	%r13, %r13
	movabsq	$.L.str7, %rdi
	movq	%r15, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movl	loop_length(,%r15,4), %ecx
	callq	printf
.LBB4_12:                               # %for.cond57
                                        #   in Loop: Header=BB4_7 Depth=2
	movl	this_node(%rip), %eax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB4_16
# BB#13:                                # %for.body59
                                        #   in Loop: Header=BB4_7 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_15
# BB#14:                                # %if.then61
                                        #   in Loop: Header=BB4_7 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	40(%rsp,%rax,8), %r12
	movl	volume(%rip), %edi
	callq	int32_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str8, %rdi
	movq	%rcx, %rsi
	callq	printf
.LBB4_15:                               # %for.inc66
                                        #   in Loop: Header=BB4_7 Depth=2
	incq	%r13
	jmp	.LBB4_12
	.align	16, 0x90
.LBB4_16:                               # %for.end68
                                        #   in Loop: Header=BB4_7 Depth=2
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB4_18
# BB#17:                                # %if.then71
                                        #   in Loop: Header=BB4_7 Depth=2
	xorq	%rax, %rax
	movabsq	$.L.str9, %rdi
	callq	printf
	movl	this_node(%rip), %eax
.LBB4_18:                               # %for.cond74.preheader
                                        #   in Loop: Header=BB4_7 Depth=2
	movq	$32, %rcx
	movl	loop_length(,%r15,4), %r8d
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%r13, %rax
	movabsq	$0, %rcx
	jne	.LBB4_20
# BB#19:                                # %for.cond74.preheader
                                        #   in Loop: Header=BB4_7 Depth=2
	movabsq	$1, %rcx
.LBB4_20:                               # %for.cond74.preheader
                                        #   in Loop: Header=BB4_7 Depth=2
	testq	%rdx, %rdx
	movq	$1, %rbx
	movq	8(%rsp), %r14           # 8-byte Reload
	jle	.LBB4_26
	.align	16, 0x90
.LBB4_21:                               # %for.body77
                                        #   Parent Loop BB4_5 Depth=1
                                        #     Parent Loop BB4_7 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	$1, %rcx
	je	.LBB4_23
# BB#22:                                # %if.then80
                                        #   in Loop: Header=BB4_21 Depth=3
	movl	(%r14), %esi
	xorq	%rax, %rax
	movabsq	$.L.str10, %rdi
	movq	%r8, %r12
	callq	printf
	movq	%r12, %r8
	movl	this_node(%rip), %eax
.LBB4_23:                               # %for.inc89
                                        #   in Loop: Header=BB4_21 Depth=3
	testq	%r13, %rax
	movabsq	$0, %rcx
	jne	.LBB4_25
# BB#24:                                # %for.inc89
                                        #   in Loop: Header=BB4_21 Depth=3
	movabsq	$1, %rcx
.LBB4_25:                               # %for.inc89
                                        #   in Loop: Header=BB4_21 Depth=3
	movq	%rbx, %rdx
	andq	%r13, %rdx
	movq	%r8, %rsi
	andq	%r13, %rsi
	incq	%rbx
	addq	$4, %r14
	cmpq	%rsi, %rdx
	jne	.LBB4_21
.LBB4_26:                               # %for.end91
                                        #   in Loop: Header=BB4_7 Depth=2
	testq	%rcx, %rcx
	je	.LBB4_28
# BB#27:                                # %if.then94
                                        #   in Loop: Header=BB4_7 Depth=2
	movabsq	$.Lstr16, %rdi
	movq	%r8, %rbx
	callq	puts
	movq	%rbx, %r8
.LBB4_28:                               # %for.inc97
                                        #   in Loop: Header=BB4_7 Depth=2
	movq	24(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movslq	loop_num(,%r15,4), %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$24, 8(%rsp)            # 8-byte Folded Spill
	cmpq	%rax, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movabsq	$4613937818241073152, %rbx # imm = 0x4008000000000000
	jl	.LBB4_7
	.align	16, 0x90
.LBB4_29:                               # %for.inc100
                                        #   in Loop: Header=BB4_5 Depth=1
	incq	%r15
	addq	$384, %rcx              # imm = 0x180
	cmpq	$3, %r15
	jne	.LBB4_5
# BB#30:                                # %for.end102
	leaq	32(%rsp), %rdi
	callq	g_doublesum
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_33
# BB#31:                                # %if.end109
	movq	32(%rsp), %rbx
	movl	volume(%rip), %edi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	printf
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_33
# BB#32:                                # %if.then112
	movq	stdout(%rip), %rdi
	callq	fflush
.LBB4_33:                               # %if.end114
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp88:
	.size	g_measure, .Ltmp88-g_measure
	.cfi_endproc

	.globl	printpath
	.align	16, 0x90
	.type	printpath,@function
printpath:                              # @printpath
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 32
.Ltmp96:
	.cfi_offset %rbx, -32
.Ltmp97:
	.cfi_offset %r14, -24
.Ltmp98:
	.cfi_offset %r15, -16
	movl	this_node(%rip), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB5_3
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	printf
.LBB5_2:                                # %for.condthread-pre-split
	movl	this_node(%rip), %eax
.LBB5_3:                                # %for.cond
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	%rdx, %rsi
	jge	.LBB5_7
# BB#4:                                 # %for.body
	testq	%rcx, %rax
	jne	.LBB5_6
# BB#5:                                 # %if.then3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%r14,%rax,4), %esi
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	printf
.LBB5_6:                                # %for.inc
	incq	%rbx
	jmp	.LBB5_2
.LBB5_7:                                # %for.end
	testq	%rcx, %rax
	je	.LBB5_9
# BB#8:                                 # %if.end9
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB5_9:                                # %if.then7
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	printf  # TAILCALL
.Ltmp99:
	.size	printpath, .Ltmp99-printpath
	.cfi_endproc

	.type	gauge_action_nloops,@object # @gauge_action_nloops
	.data
	.globl	gauge_action_nloops
	.align	4
gauge_action_nloops:
	.long	3                       # 0x3
	.size	gauge_action_nloops, 4

	.type	gauge_action_nreps,@object # @gauge_action_nreps
	.globl	gauge_action_nreps
	.align	4
gauge_action_nreps:
	.long	1                       # 0x1
	.size	gauge_action_nreps, 4

	.type	make_loop_table.loop_ind,@object # @make_loop_table.loop_ind
	.section	.rodata,"a",@progbits
	.align	16
make_loop_table.loop_ind:
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	7                       # 0x7
	.long	6                       # 0x6
	.long	4294967295              # 0xffffffff
	.long	4294967295              # 0xffffffff
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	6                       # 0x6
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	7                       # 0x7
	.long	6                       # 0x6
	.long	5                       # 0x5
	.size	make_loop_table.loop_ind, 72

	.type	make_loop_table.loop_length_in,@object # @make_loop_table.loop_length_in
	.align	4
make_loop_table.loop_length_in:
	.long	4                       # 0x4
	.long	6                       # 0x6
	.long	6                       # 0x6
	.size	make_loop_table.loop_length_in, 12

	.type	loop_num,@object        # @loop_num
	.comm	loop_num,12,4
	.type	loop_length,@object     # @loop_length
	.comm	loop_length,12,4
	.type	loop_coeff,@object      # @loop_coeff
	.comm	loop_coeff,24,16
	.type	gauge_action_description,@object # @gauge_action_description
	.comm	gauge_action_description,128,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\"Symanzik 1x1 + 1x2 + 1x1x1 action\""
	.size	.L.str, 36

	.type	loop_char,@object       # @loop_char
	.comm	loop_char,64,16
	.type	loop_table,@object      # @loop_table
	.comm	loop_table,1152,16
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"                    %d %d      %e     %d\n"
	.size	.L.str4, 42

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"PLAQ:\t%f\t%f\n"
	.size	.L.str5, 13

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"P_LOOP:\t%e\t%e\n"
	.size	.L.str6, 15

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"G_LOOP:  %d  %d  %d   "
	.size	.L.str7, 23

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\t%e"
	.size	.L.str8, 4

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\t( "
	.size	.L.str9, 4

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%d "
	.size	.L.str10, 4

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"GACTION: %e\n"
	.size	.L.str12, 13

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	",  L = %d )\n"
	.size	.L.str13, 13

	.type	loop_ind,@object        # @loop_ind
	.comm	loop_ind,72,16
	.type	loop_expect,@object     # @loop_expect
	.comm	loop_expect,384,16
	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"loop coefficients: nloop rep loop_coeff  multiplicity"
	.size	.Lstr, 54

	.type	.Lstr14,@object         # @str14
	.align	16
.Lstr14:
	.asciz	"OOPS: MAX_NUM too small"
	.size	.Lstr14, 24

	.type	.Lstr15,@object         # @str15
	.align	16
.Lstr15:
	.asciz	"Symanzik 1x1 + 1x2 + 1x1x1 action"
	.size	.Lstr15, 34

	.type	.Lstr16,@object         # @str16
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr16:
	.asciz	" )"
	.size	.Lstr16, 3


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
