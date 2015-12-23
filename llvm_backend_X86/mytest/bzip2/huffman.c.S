	.file	"huffman.c.bc"
	.section	.debug_info,"",@progbits
.Lsection_info:
	.section	.debug_abbrev,"",@progbits
.Lsection_abbrev:
	.section	.debug_line,"",@progbits
.Lsection_line:
	.section	.debug_loc,"",@progbits
	.section	.debug_pubnames,"",@progbits
	.section	.debug_pubtypes,"",@progbits
	.section	.debug_str,"MS",@progbits,1
.Linfo_string:
	.section	.debug_ranges,"",@progbits
.Ldebug_range:
	.section	.debug_loc,"",@progbits
.Lsection_debug_loc:
	.file	1 "huffman.c"
	.text
	.globl	BZ2_hbMakeCodeLengths
	.align	16, 0x90
	.type	BZ2_hbMakeCodeLengths,@function
BZ2_hbMakeCodeLengths:                  # @BZ2_hbMakeCodeLengths
.Lfunc_begin0:
	.loc	1 107 0                 # huffman.c:107:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp5:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$5272, %rsp             # imm = 0x1498
.Ltmp6:
	.cfi_offset %rbx, -56
.Ltmp7:
	.cfi_offset %r12, -48
.Ltmp8:
	.cfi_offset %r13, -40
.Ltmp9:
	.cfi_offset %r14, -32
.Ltmp10:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	movq	%rsi, -56(%rbp)
	movl	%edx, -60(%rbp)
	movl	%ecx, -64(%rbp)
	xorq	%rax, %rax
	.loc	1 119 0 prologue_end    # huffman.c:119:0
.Ltmp11:
	movl	%eax, -84(%rbp)
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_10:                               # %for.inc
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$8, %rcx
	.loc	1 120 0                 # huffman.c:120:0
	shlq	%cl, %rax
	movl	-84(%rbp), %edx
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	.loc	1 119 0                 # huffman.c:119:0
	movl	-84(%rbp), %esi
	movq	$32, %rcx
	.loc	1 120 0                 # huffman.c:120:0
	sarq	%cl, %rdx
	movl	%eax, -3200(%rbp,%rdx,4)
	.loc	1 119 0                 # huffman.c:119:0
	incq	%rsi
	movl	%esi, -84(%rbp)
.LBB0_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-84(%rbp), %rax
	movslq	-60(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_2
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB0_1 Depth=1
	.loc	1 120 0                 # huffman.c:120:0
	movslq	-84(%rbp), %rax
	movq	-56(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	testq	%rax, %rax
	movq	$1, %rax
	je	.LBB0_10
# BB#9:                                 # %cond.false
                                        #   in Loop: Header=BB0_1 Depth=1
	movslq	-84(%rbp), %rax
	movq	-56(%rbp), %rcx
	movl	(%rcx,%rax,4), %eax
	jmp	.LBB0_10
.Ltmp12:
.LBB0_2:
	xorq	%r15, %r15
	movq	$1, %r13
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$9223372036854775806, %rbx # imm = 0x7FFFFFFFFFFFFFFE
	movabsq	$4294967040, %r14       # imm = 0xFFFFFF00
	jmp	.LBB0_3
	.align	16, 0x90
.LBB0_47:                               # %if.end227
                                        #   in Loop: Header=BB0_3 Depth=1
	.loc	1 182 0                 # huffman.c:182:0
.Ltmp13:
	movl	%r13d, -84(%rbp)
	jmp	.LBB0_48
	.align	16, 0x90
.LBB0_49:                               # %for.inc239
                                        #   in Loop: Header=BB0_48 Depth=2
	.loc	1 183 0                 # huffman.c:183:0
.Ltmp14:
	movslq	-84(%rbp), %rax
	movl	-3200(%rbp,%rax,4), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$40, %rcx
	sarq	%cl, %rdx
	movl	%edx, -88(%rbp)
	movq	$31, %rcx
	.loc	1 184 0                 # huffman.c:184:0
	shrq	%cl, %rax
	addq	%rdx, %rax
	movq	$1, %rcx
	sarq	%cl, %rax
	incq	%rax
	movl	%eax, -88(%rbp)
	movq	$8, %rcx
.Ltmp15:
	.loc	1 182 0                 # huffman.c:182:0
	movl	-84(%rbp), %edx
	.loc	1 185 0                 # huffman.c:185:0
.Ltmp16:
	movslq	-84(%rbp), %rsi
	shlq	%cl, %rax
	movl	%eax, -3200(%rbp,%rsi,4)
.Ltmp17:
	.loc	1 182 0                 # huffman.c:182:0
	incq	%rdx
	movl	%edx, -84(%rbp)
.LBB0_48:                               # %for.cond228
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-84(%rbp), %rax
	movslq	-60(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB0_49
.Ltmp18:
.LBB0_3:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_4 Depth 2
                                        #       Child Loop BB0_6 Depth 3
                                        #     Child Loop BB0_14 Depth 2
                                        #       Child Loop BB0_16 Depth 3
                                        #       Child Loop BB0_23 Depth 3
                                        #       Child Loop BB0_33 Depth 3
                                        #     Child Loop BB0_39 Depth 2
                                        #       Child Loop BB0_41 Depth 3
                                        #     Child Loop BB0_48 Depth 2
	.loc	1 124 0                 # huffman.c:124:0
	movl	-60(%rbp), %eax
	movl	%eax, -68(%rbp)
	.loc	1 125 0                 # huffman.c:125:0
	movl	%r15d, -72(%rbp)
	.loc	1 127 0                 # huffman.c:127:0
	movl	%r15d, -1136(%rbp)
	.loc	1 128 0                 # huffman.c:128:0
	movl	%r15d, -3200(%rbp)
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	.loc	1 129 0                 # huffman.c:129:0
	movl	%eax, -5264(%rbp)
	.loc	1 131 0                 # huffman.c:131:0
.Ltmp19:
	movl	%r13d, -84(%rbp)
	jmp	.LBB0_4
	.align	16, 0x90
.LBB0_11:                               # %for.inc36
                                        #   in Loop: Header=BB0_4 Depth=2
.Ltmp20:
	#DEBUG_VALUE: zz <- [RBP+-5268]
	#DEBUG_VALUE: tmp <- [RBP+-5272]
	.loc	1 135 0                 # huffman.c:135:0
	movl	-5272(%rbp), %eax
.Ltmp21:
	.loc	1 131 0                 # huffman.c:131:0
	movl	-84(%rbp), %ecx
	.loc	1 135 0                 # huffman.c:135:0
.Ltmp22:
	movslq	-5268(%rbp), %rdx
	movl	%eax, -1136(%rbp,%rdx,4)
.Ltmp23:
	.loc	1 131 0                 # huffman.c:131:0
	incq	%rcx
	movl	%ecx, -84(%rbp)
.Ltmp24:
.LBB0_4:                                # %for.cond9
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_6 Depth 3
	movslq	-84(%rbp), %rax
	movslq	-60(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB0_12
# BB#5:                                 # %for.body11
                                        #   in Loop: Header=BB0_4 Depth=2
	.loc	1 133 0                 # huffman.c:133:0
.Ltmp25:
	movl	-72(%rbp), %eax
	incq	%rax
	movl	%eax, -72(%rbp)
	.loc	1 135 0                 # huffman.c:135:0
.Ltmp26:
	movl	-72(%rbp), %eax
.Ltmp27:
	.loc	1 134 0                 # huffman.c:134:0
	movl	-84(%rbp), %ecx
	movslq	-72(%rbp), %rdx
	.loc	1 135 0                 # huffman.c:135:0
.Ltmp28:
	movl	%eax, -5268(%rbp)
	movslq	-5268(%rbp), %rax
.Ltmp29:
	.loc	1 134 0                 # huffman.c:134:0
	movl	%ecx, -1136(%rbp,%rdx,4)
.Ltmp30:
	#DEBUG_VALUE: tmp <- [RBP+-5272]
	#DEBUG_VALUE: zz <- [RBP+-5268]
	.loc	1 132 0                 # huffman.c:132:0
	movslq	-84(%rbp), %rcx
	.loc	1 135 0                 # huffman.c:135:0
.Ltmp31:
	movl	-1136(%rbp,%rax,4), %eax
.Ltmp32:
	.loc	1 132 0                 # huffman.c:132:0
	movl	%r12d, -5264(%rbp,%rcx,4)
	.loc	1 135 0                 # huffman.c:135:0
.Ltmp33:
	movl	%eax, -5272(%rbp)
	jmp	.LBB0_6
	.align	16, 0x90
.LBB0_7:                                # %while.body27
                                        #   in Loop: Header=BB0_6 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5268]
	#DEBUG_VALUE: tmp <- [RBP+-5272]
	.loc	1 135 0                 # huffman.c:135:0
.Ltmp34:
	movslq	-5268(%rbp), %rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	movslq	-5268(%rbp), %rdx
	movl	-1136(%rbp,%rcx,2), %ecx
	movl	%ecx, -1136(%rbp,%rax,4)
	movq	$1, %rcx
	shrq	%cl, %rdx
	movl	%edx, -5268(%rbp)
.Ltmp35:
.LBB0_6:                                # %while.cond19
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5268]
	#DEBUG_VALUE: tmp <- [RBP+-5272]
	.loc	1 135 0                 # huffman.c:135:0
	movslq	-5268(%rbp), %rax
	movslq	-5272(%rbp), %rcx
	andq	%rbx, %rax
	movslq	-1136(%rbp,%rax,2), %rax
	movslq	-3200(%rbp,%rcx,4), %rcx
	movslq	-3200(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jl	.LBB0_7
	jmp	.LBB0_11
.Ltmp36:
	.align	16, 0x90
.LBB0_12:                               # %for.end38
                                        #   in Loop: Header=BB0_3 Depth=1
	.loc	1 138 0                 # huffman.c:138:0
	movslq	-72(%rbp), %rax
	cmpq	$260, %rax              # imm = 0x104
	jl	.LBB0_14
# BB#13:                                # %if.then
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$2001, %rdi             # imm = 0x7D1
	callq	BZ2_bz__AssertH__fail
	jmp	.LBB0_14
.Ltmp37:
	.align	16, 0x90
.LBB0_35:                               # %while.end198
                                        #   in Loop: Header=BB0_14 Depth=2
	#DEBUG_VALUE: zz <- [RBP+-5300]
	#DEBUG_VALUE: tmp <- [RBP+-5304]
	.loc	1 149 0                 # huffman.c:149:0
	movl	-5304(%rbp), %eax
	movslq	-5300(%rbp), %rcx
	movl	%eax, -1136(%rbp,%rcx,4)
.Ltmp38:
.LBB0_14:                               # %while.cond40
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_16 Depth 3
                                        #       Child Loop BB0_23 Depth 3
                                        #       Child Loop BB0_33 Depth 3
	.loc	1 140 0                 # huffman.c:140:0
	movslq	-72(%rbp), %rax
	cmpq	$2, %rax
	jl	.LBB0_36
# BB#15:                                # %while.body42
                                        #   in Loop: Header=BB0_14 Depth=2
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp39:
	movslq	-72(%rbp), %rax
	movl	-1136(%rbp,%rax,4), %eax
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp40:
	movl	%r13d, -5276(%rbp)
	movslq	-5276(%rbp), %rcx
.Ltmp41:
	.loc	1 141 0                 # huffman.c:141:0
	movl	-1132(%rbp), %edx
	movl	%eax, -1132(%rbp)
	movl	-72(%rbp), %eax
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp42:
	movl	-1136(%rbp,%rcx,4), %ecx
.Ltmp43:
	.loc	1 141 0                 # huffman.c:141:0
	movl	%edx, -76(%rbp)
	decq	%rax
	movl	%eax, -72(%rbp)
.Ltmp44:
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: zz <- [RBP+-5276]
	.loc	1 141 0                 # huffman.c:141:0
	movl	%ecx, -5284(%rbp)
	jmp	.LBB0_16
	.align	16, 0x90
.LBB0_21:                               # %if.end82
                                        #   in Loop: Header=BB0_16 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp45:
	movslq	-5280(%rbp), %rax
	movl	-1136(%rbp,%rax,4), %eax
	movslq	-5276(%rbp), %rcx
	movl	-5280(%rbp), %edx
	movl	%eax, -1136(%rbp,%rcx,4)
	movl	%edx, -5276(%rbp)
.LBB0_16:                               # %while.body55
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	movl	-5276(%rbp), %eax
	movq	$33, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp46:
	movslq	-72(%rbp), %rsi
.Ltmp47:
	.loc	1 141 0                 # huffman.c:141:0
	sarq	%cl, %rdx
	addq	%rax, %rax
	movl	%eax, -5280(%rbp)
	cmpq	%rsi, %rdx
	jg	.LBB0_22
# BB#17:                                # %if.end59
                                        #   in Loop: Header=BB0_16 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp48:
	movslq	-5280(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_20
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB0_16 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	movslq	-5280(%rbp), %rax
	leaq	1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movslq	-1136(%rbp,%rdx,4), %rcx
	movslq	-1136(%rbp,%rax,4), %rax
	movslq	-3200(%rbp,%rcx,4), %rcx
	movslq	-3200(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jge	.LBB0_20
# BB#19:                                # %if.then71
                                        #   in Loop: Header=BB0_16 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	movl	-5280(%rbp), %eax
	incq	%rax
	movl	%eax, -5280(%rbp)
.Ltmp49:
.LBB0_20:                               # %if.end73
                                        #   in Loop: Header=BB0_16 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	.loc	1 141 0                 # huffman.c:141:0
	movslq	-5280(%rbp), %rax
	movslq	-5284(%rbp), %rcx
	movslq	-1136(%rbp,%rax,4), %rax
	movslq	-3200(%rbp,%rcx,4), %rcx
	movslq	-3200(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jge	.LBB0_21
.Ltmp50:
.LBB0_22:                               # %while.end87
                                        #   in Loop: Header=BB0_14 Depth=2
	#DEBUG_VALUE: zz <- [RBP+-5276]
	#DEBUG_VALUE: yy <- [RBP+-5280]
	#DEBUG_VALUE: tmp <- [RBP+-5284]
	.loc	1 141 0                 # huffman.c:141:0
	movl	-5284(%rbp), %eax
	movslq	-5276(%rbp), %rcx
.Ltmp51:
	.loc	1 142 0                 # huffman.c:142:0
	movslq	-72(%rbp), %rdx
	.loc	1 141 0                 # huffman.c:141:0
.Ltmp52:
	movl	%eax, -1136(%rbp,%rcx,4)
.Ltmp53:
	.loc	1 142 0                 # huffman.c:142:0
	movl	-1136(%rbp,%rdx,4), %eax
	.loc	1 142 0                 # huffman.c:142:0
.Ltmp54:
	movl	%r13d, -5288(%rbp)
	movslq	-5288(%rbp), %rcx
.Ltmp55:
	.loc	1 142 0                 # huffman.c:142:0
	movl	-1132(%rbp), %edx
	movl	%eax, -1132(%rbp)
	movl	-72(%rbp), %eax
	.loc	1 142 0                 # huffman.c:142:0
.Ltmp56:
	movl	-1136(%rbp,%rcx,4), %ecx
.Ltmp57:
	.loc	1 142 0                 # huffman.c:142:0
	movl	%edx, -80(%rbp)
	decq	%rax
	movl	%eax, -72(%rbp)
.Ltmp58:
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: zz <- [RBP+-5288]
	.loc	1 142 0                 # huffman.c:142:0
	movl	%ecx, -5296(%rbp)
	jmp	.LBB0_23
.Ltmp59:
	.align	16, 0x90
.LBB0_28:                               # %if.end132
                                        #   in Loop: Header=BB0_23 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	.loc	1 142 0                 # huffman.c:142:0
	movslq	-5292(%rbp), %rax
	movl	-1136(%rbp,%rax,4), %eax
	movslq	-5288(%rbp), %rcx
	movl	-5292(%rbp), %edx
	movl	%eax, -1136(%rbp,%rcx,4)
	movl	%edx, -5288(%rbp)
.LBB0_23:                               # %while.body104
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	movl	-5288(%rbp), %eax
	movq	$33, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	.loc	1 142 0                 # huffman.c:142:0
.Ltmp60:
	movslq	-72(%rbp), %rsi
.Ltmp61:
	.loc	1 142 0                 # huffman.c:142:0
	sarq	%cl, %rdx
	addq	%rax, %rax
	movl	%eax, -5292(%rbp)
	cmpq	%rsi, %rdx
	jg	.LBB0_29
# BB#24:                                # %if.end108
                                        #   in Loop: Header=BB0_23 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	.loc	1 142 0                 # huffman.c:142:0
.Ltmp62:
	movslq	-5292(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB0_27
# BB#25:                                # %land.lhs.true110
                                        #   in Loop: Header=BB0_23 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	movslq	-5292(%rbp), %rax
	leaq	1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movslq	-1136(%rbp,%rdx,4), %rcx
	movslq	-1136(%rbp,%rax,4), %rax
	movslq	-3200(%rbp,%rcx,4), %rcx
	movslq	-3200(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jge	.LBB0_27
# BB#26:                                # %if.then121
                                        #   in Loop: Header=BB0_23 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	movl	-5292(%rbp), %eax
	incq	%rax
	movl	%eax, -5292(%rbp)
.Ltmp63:
.LBB0_27:                               # %if.end123
                                        #   in Loop: Header=BB0_23 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	.loc	1 142 0                 # huffman.c:142:0
	movslq	-5292(%rbp), %rax
	movslq	-5296(%rbp), %rcx
	movslq	-1136(%rbp,%rax,4), %rax
	movslq	-3200(%rbp,%rcx,4), %rcx
	movslq	-3200(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jge	.LBB0_28
.Ltmp64:
.LBB0_29:                               # %while.end137
                                        #   in Loop: Header=BB0_14 Depth=2
	#DEBUG_VALUE: zz <- [RBP+-5288]
	#DEBUG_VALUE: yy <- [RBP+-5292]
	#DEBUG_VALUE: tmp <- [RBP+-5296]
	.loc	1 142 0                 # huffman.c:142:0
	movl	-5296(%rbp), %eax
	movslq	-5288(%rbp), %rcx
	movl	%eax, -1136(%rbp,%rcx,4)
.Ltmp65:
	.loc	1 143 0                 # huffman.c:143:0
	movl	-68(%rbp), %eax
	incq	%rax
	movl	%eax, -68(%rbp)
	.loc	1 144 0                 # huffman.c:144:0
	movslq	-80(%rbp), %rcx
	.loc	1 145 0                 # huffman.c:145:0
	movslq	-76(%rbp), %rdx
	movslq	-80(%rbp), %rsi
	.loc	1 144 0                 # huffman.c:144:0
	movl	%eax, -5264(%rbp,%rcx,4)
	movslq	-76(%rbp), %rcx
	.loc	1 145 0                 # huffman.c:145:0
	movl	-3200(%rbp,%rdx,4), %edx
	movl	-3200(%rbp,%rsi,4), %esi
	.loc	1 144 0                 # huffman.c:144:0
	movl	%eax, -5264(%rbp,%rcx,4)
	.loc	1 145 0                 # huffman.c:145:0
	movq	%rdx, %rcx
	andq	%r14, %rcx
	movq	%rsi, %rax
	andq	%r14, %rax
	addq	%rcx, %rax
	andq	$255, %rdx
	andq	$255, %rsi
	cmpq	%rsi, %rdx
	jle	.LBB0_31
.Ltmp66:
# BB#30:                                # %cond.true158
                                        #   in Loop: Header=BB0_14 Depth=2
	movslq	-76(%rbp), %rcx
	jmp	.LBB0_32
	.align	16, 0x90
.LBB0_31:                               # %cond.false162
                                        #   in Loop: Header=BB0_14 Depth=2
	movslq	-80(%rbp), %rcx
.LBB0_32:                               # %cond.end166
                                        #   in Loop: Header=BB0_14 Depth=2
	movzbl	-3200(%rbp,%rcx,4), %ecx
	.loc	1 147 0                 # huffman.c:147:0
	movl	-72(%rbp), %edx
	incq	%rdx
	movl	%edx, -72(%rbp)
	.loc	1 149 0                 # huffman.c:149:0
.Ltmp67:
	movl	-72(%rbp), %edx
.Ltmp68:
	.loc	1 145 0                 # huffman.c:145:0
	incq	%rcx
	.loc	1 148 0                 # huffman.c:148:0
	movl	-68(%rbp), %esi
	movslq	-72(%rbp), %rdi
	.loc	1 149 0                 # huffman.c:149:0
.Ltmp69:
	movl	%edx, -5300(%rbp)
	movslq	-5300(%rbp), %rdx
.Ltmp70:
	.loc	1 148 0                 # huffman.c:148:0
	movl	%esi, -1136(%rbp,%rdi,4)
.Ltmp71:
	#DEBUG_VALUE: tmp <- [RBP+-5304]
	#DEBUG_VALUE: zz <- [RBP+-5300]
	.loc	1 145 0                 # huffman.c:145:0
	movslq	-68(%rbp), %rsi
	.loc	1 146 0                 # huffman.c:146:0
	movslq	-68(%rbp), %rdi
	.loc	1 149 0                 # huffman.c:149:0
.Ltmp72:
	movl	-1136(%rbp,%rdx,4), %edx
.Ltmp73:
	.loc	1 145 0                 # huffman.c:145:0
	orq	%rcx, %rax
	movl	%eax, -3200(%rbp,%rsi,4)
	.loc	1 146 0                 # huffman.c:146:0
	movl	%r12d, -5264(%rbp,%rdi,4)
	.loc	1 149 0                 # huffman.c:149:0
.Ltmp74:
	movl	%edx, -5304(%rbp)
	jmp	.LBB0_33
	.align	16, 0x90
.LBB0_34:                               # %while.body191
                                        #   in Loop: Header=BB0_33 Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5300]
	#DEBUG_VALUE: tmp <- [RBP+-5304]
	.loc	1 149 0                 # huffman.c:149:0
.Ltmp75:
	movslq	-5300(%rbp), %rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	movslq	-5300(%rbp), %rdx
	movl	-1136(%rbp,%rcx,2), %ecx
	movl	%ecx, -1136(%rbp,%rax,4)
	movq	$1, %rcx
	shrq	%cl, %rdx
	movl	%edx, -5300(%rbp)
.Ltmp76:
.LBB0_33:                               # %while.cond182
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	#DEBUG_VALUE: zz <- [RBP+-5300]
	#DEBUG_VALUE: tmp <- [RBP+-5304]
	.loc	1 149 0                 # huffman.c:149:0
	movslq	-5300(%rbp), %rax
	movslq	-5304(%rbp), %rcx
	andq	%rbx, %rax
	movslq	-1136(%rbp,%rax,2), %rax
	movslq	-3200(%rbp,%rcx,4), %rcx
	movslq	-3200(%rbp,%rax,4), %rax
	cmpq	%rax, %rcx
	jl	.LBB0_34
	jmp	.LBB0_35
.Ltmp77:
	.align	16, 0x90
.LBB0_36:                               # %while.end201
                                        #   in Loop: Header=BB0_3 Depth=1
	.loc	1 152 0                 # huffman.c:152:0
	movslq	-68(%rbp), %rax
	cmpq	$516, %rax              # imm = 0x204
	jl	.LBB0_38
# BB#37:                                # %if.then203
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	$2002, %rdi             # imm = 0x7D2
	callq	BZ2_bz__AssertH__fail
.Ltmp78:
.LBB0_38:                               # %if.end204
                                        #   in Loop: Header=BB0_3 Depth=1
	.loc	1 154 0                 # huffman.c:154:0
	movb	%r15b, -93(%rbp)
	.loc	1 155 0                 # huffman.c:155:0
.Ltmp79:
	movl	%r13d, -84(%rbp)
	jmp	.LBB0_39
	.align	16, 0x90
.LBB0_45:                               # %for.inc223
                                        #   in Loop: Header=BB0_39 Depth=2
	movl	-84(%rbp), %eax
	incq	%rax
	movl	%eax, -84(%rbp)
.LBB0_39:                               # %for.cond205
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_41 Depth 3
	movslq	-84(%rbp), %rax
	movslq	-60(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB0_46
# BB#40:                                # %for.body207
                                        #   in Loop: Header=BB0_39 Depth=2
	.loc	1 157 0                 # huffman.c:157:0
.Ltmp80:
	movl	-84(%rbp), %eax
	.loc	1 156 0                 # huffman.c:156:0
	movl	%r15d, -88(%rbp)
	.loc	1 157 0                 # huffman.c:157:0
	movl	%eax, -92(%rbp)
	jmp	.LBB0_41
	.align	16, 0x90
.LBB0_42:                               # %while.body212
                                        #   in Loop: Header=BB0_41 Depth=3
	.loc	1 158 0                 # huffman.c:158:0
.Ltmp81:
	movslq	-92(%rbp), %rax
	movl	-88(%rbp), %ecx
	movl	-5264(%rbp,%rax,4), %eax
	movl	%eax, -92(%rbp)
	incq	%rcx
	movl	%ecx, -88(%rbp)
.Ltmp82:
.LBB0_41:                               # %while.cond208
                                        #   Parent Loop BB0_3 Depth=1
                                        #     Parent Loop BB0_39 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	.loc	1 158 0                 # huffman.c:158:0
	movslq	-92(%rbp), %rax
	movslq	-5264(%rbp,%rax,4), %rax
	testq	%rax, %rax
	jns	.LBB0_42
# BB#43:                                # %while.end216
                                        #   in Loop: Header=BB0_39 Depth=2
	.loc	1 159 0                 # huffman.c:159:0
	movzbl	-88(%rbp), %eax
	movslq	-84(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movb	%al, -1(%rcx,%rdx)
	.loc	1 160 0                 # huffman.c:160:0
.Ltmp83:
	movslq	-88(%rbp), %rax
	movslq	-64(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB0_45
# BB#44:                                # %if.then221
                                        #   in Loop: Header=BB0_39 Depth=2
	movb	%r13b, -93(%rbp)
	jmp	.LBB0_45
.Ltmp84:
	.align	16, 0x90
.LBB0_46:                               # %for.end225
                                        #   in Loop: Header=BB0_3 Depth=1
	.loc	1 163 0                 # huffman.c:163:0
	movzbl	-93(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB0_47
.Ltmp85:
# BB#50:                                # %while.end242
	.loc	1 188 0                 # huffman.c:188:0
	addq	$5272, %rsp             # imm = 0x1498
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp86:
.Ltmp87:
	.size	BZ2_hbMakeCodeLengths, .Ltmp87-BZ2_hbMakeCodeLengths
.Lfunc_end0:
	.file	2 "./bzlib_private.h"
	.cfi_endproc

	.globl	BZ2_hbAssignCodes
	.align	16, 0x90
	.type	BZ2_hbAssignCodes,@function
BZ2_hbAssignCodes:                      # @BZ2_hbAssignCodes
.Lfunc_begin1:
	.loc	1 197 0                 # huffman.c:197:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp90:
	.cfi_def_cfa_offset 16
.Ltmp91:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp92:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	%edx, -20(%rbp)
	movl	%ecx, -24(%rbp)
	.loc	1 201 0 prologue_end    # huffman.c:201:0
.Ltmp93:
	movl	-20(%rbp), %ecx
	movl	%r8d, -28(%rbp)
	xorq	%rax, %rax
.Ltmp94:
	.loc	1 200 0                 # huffman.c:200:0
	movl	%eax, -36(%rbp)
	.loc	1 201 0                 # huffman.c:201:0
.Ltmp95:
	movl	%ecx, -32(%rbp)
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_7:                                # %for.inc9
                                        #   in Loop: Header=BB1_1 Depth=1
	.loc	1 204 0                 # huffman.c:204:0
.Ltmp96:
	movl	-36(%rbp), %ecx
.Ltmp97:
	.loc	1 201 0                 # huffman.c:201:0
	movl	-32(%rbp), %edx
	.loc	1 204 0                 # huffman.c:204:0
.Ltmp98:
	addq	%rcx, %rcx
	movl	%ecx, -36(%rbp)
.Ltmp99:
	.loc	1 201 0                 # huffman.c:201:0
	incq	%rdx
	movl	%edx, -32(%rbp)
.LBB1_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_3 Depth 2
	movslq	-32(%rbp), %rcx
	movslq	-24(%rbp), %rdx
	cmpq	%rdx, %rcx
	jg	.LBB1_8
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB1_1 Depth=1
	.loc	1 202 0                 # huffman.c:202:0
.Ltmp100:
	movl	%eax, -40(%rbp)
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_6:                                # %for.inc
                                        #   in Loop: Header=BB1_3 Depth=2
	movl	-40(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -40(%rbp)
.LBB1_3:                                # %for.cond1
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-40(%rbp), %rcx
	movslq	-28(%rbp), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB1_7
# BB#4:                                 # %for.body3
                                        #   in Loop: Header=BB1_3 Depth=2
	.loc	1 203 0                 # huffman.c:203:0
.Ltmp101:
	movslq	-40(%rbp), %rcx
	movq	-16(%rbp), %rdx
	movzbl	(%rdx,%rcx), %ecx
	movl	-32(%rbp), %edx
	cmpq	%rdx, %rcx
	jne	.LBB1_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB1_3 Depth=2
	.loc	1 203 0                 # huffman.c:203:0
.Ltmp102:
	movl	-36(%rbp), %ecx
	movslq	-40(%rbp), %rdx
	movq	-8(%rbp), %rsi
	movl	%ecx, (%rsi,%rdx,4)
	movl	-36(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -36(%rbp)
	jmp	.LBB1_6
.Ltmp103:
.LBB1_8:                                # %for.end11
	.loc	1 206 0                 # huffman.c:206:0
	popq	%rbp
	retq
.Ltmp104:
.Ltmp105:
	.size	BZ2_hbAssignCodes, .Ltmp105-BZ2_hbAssignCodes
.Lfunc_end1:
	.cfi_endproc

	.globl	BZ2_hbCreateDecodeTables
	.align	16, 0x90
	.type	BZ2_hbCreateDecodeTables,@function
BZ2_hbCreateDecodeTables:               # @BZ2_hbCreateDecodeTables
.Lfunc_begin2:
	.loc	1 217 0                 # huffman.c:217:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp108:
	.cfi_def_cfa_offset 16
.Ltmp109:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp110:
	.cfi_def_cfa_register %rbp
	movq	16(%rbp), %rax
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movl	%r8d, -36(%rbp)
	movl	%r9d, -40(%rbp)
	.loc	1 221 0 prologue_end    # huffman.c:221:0
.Ltmp111:
	movl	-36(%rbp), %ecx
	movl	%eax, -44(%rbp)
	xorq	%rax, %rax
.Ltmp112:
	.loc	1 220 0                 # huffman.c:220:0
	movl	%eax, -48(%rbp)
	jmp	.LBB2_1
	.align	16, 0x90
.LBB2_7:                                # %for.inc9
                                        #   in Loop: Header=BB2_1 Depth=1
	.loc	1 221 0                 # huffman.c:221:0
.Ltmp113:
	movl	-52(%rbp), %ecx
	incq	%rcx
.LBB2_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_3 Depth 2
	movl	%ecx, -52(%rbp)
	movslq	-52(%rbp), %rcx
	movslq	-40(%rbp), %rdx
	cmpq	%rdx, %rcx
	jg	.LBB2_8
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB2_1 Depth=1
	.loc	1 222 0                 # huffman.c:222:0
.Ltmp114:
	movl	%eax, -56(%rbp)
	jmp	.LBB2_3
	.align	16, 0x90
.LBB2_6:                                # %for.inc
                                        #   in Loop: Header=BB2_3 Depth=2
	movl	-56(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -56(%rbp)
.LBB2_3:                                # %for.cond1
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-56(%rbp), %rcx
	movslq	-44(%rbp), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB2_7
# BB#4:                                 # %for.body3
                                        #   in Loop: Header=BB2_3 Depth=2
	.loc	1 223 0                 # huffman.c:223:0
.Ltmp115:
	movslq	-56(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movzbl	(%rdx,%rcx), %ecx
	movl	-52(%rbp), %edx
	cmpq	%rdx, %rcx
	jne	.LBB2_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB2_3 Depth=2
	.loc	1 223 0                 # huffman.c:223:0
.Ltmp116:
	movl	-56(%rbp), %ecx
	movslq	-48(%rbp), %rdx
	movq	-24(%rbp), %rsi
	movl	%ecx, (%rsi,%rdx,4)
	movl	-48(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -48(%rbp)
	jmp	.LBB2_6
.Ltmp117:
.LBB2_8:                                # %for.end11
	xorq	%rax, %rax
	.loc	1 225 0                 # huffman.c:225:0
.Ltmp118:
	movl	%eax, -52(%rbp)
	jmp	.LBB2_9
	.align	16, 0x90
.LBB2_10:                               # %for.inc18
                                        #   in Loop: Header=BB2_9 Depth=1
	movslq	-52(%rbp), %rcx
	movq	-16(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	movl	-52(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -52(%rbp)
.LBB2_9:                                # %for.cond12
                                        # =>This Inner Loop Header: Depth=1
	movslq	-52(%rbp), %rcx
	cmpq	$22, %rcx
	jle	.LBB2_10
.Ltmp119:
# BB#11:                                # %for.end20
	xorq	%rax, %rax
	jmp	.LBB2_12
	.align	16, 0x90
.LBB2_13:                               # %for.inc31
                                        #   in Loop: Header=BB2_12 Depth=1
	.loc	1 226 0                 # huffman.c:226:0
.Ltmp120:
	movslq	-52(%rbp), %rax
	movq	-32(%rbp), %rcx
	movzbl	(%rcx,%rax), %eax
	movq	-16(%rbp), %rcx
	movl	4(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 4(%rcx,%rax,4)
	movl	-52(%rbp), %eax
	incq	%rax
.LBB2_12:                               # %for.cond21
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -52(%rbp)
	movslq	-52(%rbp), %rax
	movslq	-44(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB2_13
.Ltmp121:
# BB#14:                                # %for.end33
	movq	$1, %rax
	jmp	.LBB2_15
	.align	16, 0x90
.LBB2_16:                               # %for.inc43
                                        #   in Loop: Header=BB2_15 Depth=1
	.loc	1 228 0                 # huffman.c:228:0
.Ltmp122:
	movslq	-52(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-16(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	(%rsi,%rax,4), %edx
	addq	%rcx, %rdx
	movl	%edx, (%rsi,%rax,4)
	movl	-52(%rbp), %eax
	incq	%rax
.LBB2_15:                               # %for.cond34
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -52(%rbp)
	movslq	-52(%rbp), %rax
	cmpq	$22, %rax
	jle	.LBB2_16
.Ltmp123:
# BB#17:                                # %for.end45
	xorq	%rax, %rax
	.loc	1 230 0                 # huffman.c:230:0
.Ltmp124:
	movl	%eax, -52(%rbp)
	jmp	.LBB2_18
	.align	16, 0x90
.LBB2_19:                               # %for.inc52
                                        #   in Loop: Header=BB2_18 Depth=1
	movslq	-52(%rbp), %rcx
	movq	-8(%rbp), %rdx
	movl	%eax, (%rdx,%rcx,4)
	movl	-52(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -52(%rbp)
.LBB2_18:                               # %for.cond46
                                        # =>This Inner Loop Header: Depth=1
	movslq	-52(%rbp), %rcx
	cmpq	$22, %rcx
	jle	.LBB2_19
.Ltmp125:
# BB#20:                                # %for.end54
	.loc	1 233 0                 # huffman.c:233:0
	movl	-36(%rbp), %eax
	xorq	%rcx, %rcx
.Ltmp126:
	.loc	1 231 0                 # huffman.c:231:0
	movl	%ecx, -60(%rbp)
	.loc	1 233 0                 # huffman.c:233:0
.Ltmp127:
	movl	%eax, -52(%rbp)
	jmp	.LBB2_21
	.align	16, 0x90
.LBB2_22:                               # %for.inc69
                                        #   in Loop: Header=BB2_21 Depth=1
	.loc	1 234 0                 # huffman.c:234:0
.Ltmp128:
	movslq	-52(%rbp), %rax
	leaq	1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-16(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	(%rsi,%rax,4), %eax
	subq	%rax, %rcx
	movl	-60(%rbp), %eax
	leaq	(%rax,%rcx), %rdx
	.loc	1 235 0                 # huffman.c:235:0
	movslq	-52(%rbp), %rsi
	movq	-8(%rbp), %rdi
	.loc	1 234 0                 # huffman.c:234:0
	movl	%edx, -60(%rbp)
	.loc	1 235 0                 # huffman.c:235:0
	leaq	-1(%rax,%rcx), %rax
	movl	%eax, (%rdi,%rsi,4)
	.loc	1 236 0                 # huffman.c:236:0
	movl	-60(%rbp), %eax
.Ltmp129:
	.loc	1 233 0                 # huffman.c:233:0
	movl	-52(%rbp), %ecx
	.loc	1 236 0                 # huffman.c:236:0
.Ltmp130:
	addq	%rax, %rax
	movl	%eax, -60(%rbp)
.Ltmp131:
	.loc	1 233 0                 # huffman.c:233:0
	incq	%rcx
	movl	%ecx, -52(%rbp)
.LBB2_21:                               # %for.cond55
                                        # =>This Inner Loop Header: Depth=1
	movslq	-52(%rbp), %rax
	movslq	-40(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB2_22
.Ltmp132:
# BB#23:                                # %for.end71
	.loc	1 238 0                 # huffman.c:238:0
	movl	-36(%rbp), %eax
	jmp	.LBB2_24
	.align	16, 0x90
.LBB2_25:                               # %for.inc87
                                        #   in Loop: Header=BB2_24 Depth=1
	.loc	1 239 0                 # huffman.c:239:0
	movslq	-52(%rbp), %rax
	leaq	-1(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-8(%rbp), %rsi
	movq	-16(%rbp), %rdi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	(%rsi,%rdx,4), %ecx
	movl	(%rdi,%rax,4), %edx
	leaq	2(%rcx,%rcx), %rcx
	subq	%rdx, %rcx
	movl	%ecx, (%rdi,%rax,4)
	.loc	1 238 0                 # huffman.c:238:0
	movl	-52(%rbp), %eax
.LBB2_24:                               # %for.cond73
                                        # =>This Inner Loop Header: Depth=1
	incq	%rax
	movl	%eax, -52(%rbp)
	movslq	-52(%rbp), %rax
	movslq	-40(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB2_25
.Ltmp133:
# BB#26:                                # %for.end89
	.loc	1 240 0                 # huffman.c:240:0
	popq	%rbp
	retq
.Ltmp134:
.Ltmp135:
	.size	BZ2_hbCreateDecodeTables, .Ltmp135-BZ2_hbCreateDecodeTables
.Lfunc_end2:
	.cfi_endproc

.Ldebug_end0:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"huffman.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"BZ2_hbMakeCodeLengths"
.Linfo_string4:
	.asciz	"BZ2_hbAssignCodes"
.Linfo_string5:
	.asciz	"BZ2_hbCreateDecodeTables"
.Linfo_string6:
	.asciz	"len"
.Linfo_string7:
	.asciz	"unsigned char"
.Linfo_string8:
	.asciz	"UChar"
.Linfo_string9:
	.asciz	"freq"
.Linfo_string10:
	.asciz	"int"
.Linfo_string11:
	.asciz	"Int32"
.Linfo_string12:
	.asciz	"alphaSize"
.Linfo_string13:
	.asciz	"maxLen"
.Linfo_string14:
	.asciz	"nNodes"
.Linfo_string15:
	.asciz	"nHeap"
.Linfo_string16:
	.asciz	"n1"
.Linfo_string17:
	.asciz	"n2"
.Linfo_string18:
	.asciz	"i"
.Linfo_string19:
	.asciz	"j"
.Linfo_string20:
	.asciz	"k"
.Linfo_string21:
	.asciz	"tooLong"
.Linfo_string22:
	.asciz	"Bool"
.Linfo_string23:
	.asciz	"heap"
.Linfo_string24:
	.asciz	"weight"
.Linfo_string25:
	.asciz	"parent"
.Linfo_string26:
	.asciz	"zz"
.Linfo_string27:
	.asciz	"tmp"
.Linfo_string28:
	.asciz	"yy"
.Linfo_string29:
	.asciz	"code"
.Linfo_string30:
	.asciz	"length"
.Linfo_string31:
	.asciz	"minLen"
.Linfo_string32:
	.asciz	"n"
.Linfo_string33:
	.asciz	"vec"
.Linfo_string34:
	.asciz	"limit"
.Linfo_string35:
	.asciz	"base"
.Linfo_string36:
	.asciz	"perm"
.Linfo_string37:
	.asciz	"pp"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	915                     # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x38c DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0x1d6 DW_TAG_subprogram
	.long	.Linfo_string3          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	103                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x40:0xe DW_TAG_formal_parameter
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	103                     # DW_AT_decl_line
	.long	828                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	3                       # Abbrev [3] 0x4e:0xe DW_TAG_formal_parameter
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	104                     # DW_AT_decl_line
	.long	851                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	3                       # Abbrev [3] 0x5c:0xe DW_TAG_formal_parameter
	.long	.Linfo_string12         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	105                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	3                       # Abbrev [3] 0x6a:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	106                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	4                       # Abbrev [4] 0x78:0xf DW_TAG_variable
	.long	.Linfo_string14         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	4                       # Abbrev [4] 0x87:0xf DW_TAG_variable
	.long	.Linfo_string15         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	4                       # Abbrev [4] 0x96:0xf DW_TAG_variable
	.long	.Linfo_string16         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	4                       # Abbrev [4] 0xa5:0xf DW_TAG_variable
	.long	.Linfo_string17         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	4                       # Abbrev [4] 0xb4:0xf DW_TAG_variable
	.long	.Linfo_string18         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	4                       # Abbrev [4] 0xc3:0xf DW_TAG_variable
	.long	.Linfo_string19         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	4                       # Abbrev [4] 0xd2:0xf DW_TAG_variable
	.long	.Linfo_string20         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	112                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	4                       # Abbrev [4] 0xe1:0xf DW_TAG_variable
	.long	.Linfo_string21         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	113                     # DW_AT_decl_line
	.long	874                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\243\177"
	.byte	4                       # Abbrev [4] 0xf0:0xf DW_TAG_variable
	.long	.Linfo_string23         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	115                     # DW_AT_decl_line
	.long	885                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220w"
	.byte	4                       # Abbrev [4] 0xff:0xf DW_TAG_variable
	.long	.Linfo_string24         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	116                     # DW_AT_decl_line
	.long	905                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200g"
	.byte	4                       # Abbrev [4] 0x10e:0xf DW_TAG_variable
	.long	.Linfo_string25         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	117                     # DW_AT_decl_line
	.long	905                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\360V"
	.byte	5                       # Abbrev [5] 0x11d:0xde DW_TAG_lexical_block
	.quad	.Ltmp13                 # DW_AT_low_pc
	.quad	.Ltmp85                 # DW_AT_high_pc
	.byte	5                       # Abbrev [5] 0x12e:0x3c DW_TAG_lexical_block
	.quad	.Ltmp19                 # DW_AT_low_pc
	.quad	.Ltmp36                 # DW_AT_high_pc
	.byte	6                       # Abbrev [6] 0x13f:0x2a DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x144:0x24 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	4                       # Abbrev [4] 0x149:0xf DW_TAG_variable
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	135                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\354V"
	.byte	4                       # Abbrev [4] 0x158:0xf DW_TAG_variable
	.long	.Linfo_string27         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	135                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\350V"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x16a:0x90 DW_TAG_lexical_block
	.long	.Ldebug_ranges5         # DW_AT_ranges
	.byte	6                       # Abbrev [6] 0x16f:0x24 DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	4                       # Abbrev [4] 0x174:0xf DW_TAG_variable
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	149                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\314V"
	.byte	4                       # Abbrev [4] 0x183:0xf DW_TAG_variable
	.long	.Linfo_string27         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	149                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310V"
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x193:0x33 DW_TAG_lexical_block
	.long	.Ldebug_ranges3         # DW_AT_ranges
	.byte	4                       # Abbrev [4] 0x198:0xf DW_TAG_variable
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	141                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\344V"
	.byte	4                       # Abbrev [4] 0x1a7:0xf DW_TAG_variable
	.long	.Linfo_string28         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	141                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340V"
	.byte	4                       # Abbrev [4] 0x1b6:0xf DW_TAG_variable
	.long	.Linfo_string27         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	141                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\334V"
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x1c6:0x33 DW_TAG_lexical_block
	.long	.Ldebug_ranges4         # DW_AT_ranges
	.byte	4                       # Abbrev [4] 0x1cb:0xf DW_TAG_variable
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330V"
	.byte	4                       # Abbrev [4] 0x1da:0xf DW_TAG_variable
	.long	.Linfo_string28         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\324V"
	.byte	4                       # Abbrev [4] 0x1e9:0xf DW_TAG_variable
	.long	.Linfo_string27         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	142                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\320V"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	2                       # Abbrev [2] 0x1fc:0x8b DW_TAG_subprogram
	.long	.Linfo_string4          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	192                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x216:0xe DW_TAG_formal_parameter
	.long	.Linfo_string29         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	192                     # DW_AT_decl_line
	.long	851                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	3                       # Abbrev [3] 0x224:0xe DW_TAG_formal_parameter
	.long	.Linfo_string30         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	193                     # DW_AT_decl_line
	.long	828                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	3                       # Abbrev [3] 0x232:0xe DW_TAG_formal_parameter
	.long	.Linfo_string31         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	194                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	3                       # Abbrev [3] 0x240:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	195                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	3                       # Abbrev [3] 0x24e:0xe DW_TAG_formal_parameter
	.long	.Linfo_string12         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	196                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	4                       # Abbrev [4] 0x25c:0xe DW_TAG_variable
	.long	.Linfo_string32         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	198                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	4                       # Abbrev [4] 0x26a:0xe DW_TAG_variable
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	198                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	4                       # Abbrev [4] 0x278:0xe DW_TAG_variable
	.long	.Linfo_string18         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	198                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	2                       # Abbrev [2] 0x287:0xb5 DW_TAG_subprogram
	.long	.Linfo_string5          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	210                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.quad	.Lfunc_end2             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x2a1:0xe DW_TAG_formal_parameter
	.long	.Linfo_string34         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	210                     # DW_AT_decl_line
	.long	851                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	3                       # Abbrev [3] 0x2af:0xe DW_TAG_formal_parameter
	.long	.Linfo_string35         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	211                     # DW_AT_decl_line
	.long	851                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	3                       # Abbrev [3] 0x2bd:0xe DW_TAG_formal_parameter
	.long	.Linfo_string36         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.long	851                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	3                       # Abbrev [3] 0x2cb:0xe DW_TAG_formal_parameter
	.long	.Linfo_string30         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.long	828                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	3                       # Abbrev [3] 0x2d9:0xe DW_TAG_formal_parameter
	.long	.Linfo_string31         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	214                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	3                       # Abbrev [3] 0x2e7:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	3                       # Abbrev [3] 0x2f5:0xe DW_TAG_formal_parameter
	.long	.Linfo_string12         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	216                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	4                       # Abbrev [4] 0x303:0xe DW_TAG_variable
	.long	.Linfo_string37         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	4                       # Abbrev [4] 0x311:0xe DW_TAG_variable
	.long	.Linfo_string18         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	4                       # Abbrev [4] 0x31f:0xe DW_TAG_variable
	.long	.Linfo_string19         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	4                       # Abbrev [4] 0x32d:0xe DW_TAG_variable
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.long	856                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0x33c:0x5 DW_TAG_pointer_type
	.long	833                     # DW_AT_type
	.byte	8                       # Abbrev [8] 0x341:0xb DW_TAG_typedef
	.long	844                     # DW_AT_type
	.long	.Linfo_string8          # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0x34c:0x7 DW_TAG_base_type
	.long	.Linfo_string7          # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	7                       # Abbrev [7] 0x353:0x5 DW_TAG_pointer_type
	.long	856                     # DW_AT_type
	.byte	8                       # Abbrev [8] 0x358:0xb DW_TAG_typedef
	.long	867                     # DW_AT_type
	.long	.Linfo_string11         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.byte	9                       # Abbrev [9] 0x363:0x7 DW_TAG_base_type
	.long	.Linfo_string10         # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	8                       # Abbrev [8] 0x36a:0xb DW_TAG_typedef
	.long	844                     # DW_AT_type
	.long	.Linfo_string22         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	10                      # Abbrev [10] 0x375:0xd DW_TAG_array_type
	.long	856                     # DW_AT_type
	.byte	11                      # Abbrev [11] 0x37a:0x7 DW_TAG_subrange_type
	.long	898                     # DW_AT_type
	.short	259                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x382:0x7 DW_TAG_base_type
	.long	.Linfo_string10         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	10                      # Abbrev [10] 0x389:0xd DW_TAG_array_type
	.long	856                     # DW_AT_type
	.byte	11                      # Abbrev [11] 0x38e:0x7 DW_TAG_subrange_type
	.long	898                     # DW_AT_type
	.short	515                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
.L.debug_info_end0:
	.section	.debug_abbrev,"",@progbits
	.byte	1                       # Abbreviation Code
	.byte	17                      # DW_TAG_compile_unit
	.byte	1                       # DW_CHILDREN_yes
	.byte	37                      # DW_AT_producer
	.byte	14                      # DW_FORM_strp
	.byte	19                      # DW_AT_language
	.byte	5                       # DW_FORM_data2
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	16                      # DW_AT_stmt_list
	.byte	23                      # DW_FORM_sec_offset
	.byte	27                      # DW_AT_comp_dir
	.byte	14                      # DW_FORM_strp
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	2                       # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	8                       # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	10                      # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	11                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	12                      # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_ranges,"",@progbits
.Lgnu_ranges0:
.Ldebug_ranges0:
	.quad	.Ltmp20
	.quad	.Ltmp21
	.quad	.Ltmp22
	.quad	.Ltmp23
	.quad	.Ltmp26
	.quad	.Ltmp27
	.quad	.Ltmp28
	.quad	.Ltmp29
	.quad	.Ltmp31
	.quad	.Ltmp32
	.quad	.Ltmp33
	.quad	.Ltmp36
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp20
	.quad	.Ltmp21
	.quad	.Ltmp22
	.quad	.Ltmp23
	.quad	.Ltmp25
	.quad	.Ltmp36
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp37
	.quad	.Ltmp38
	.quad	.Ltmp67
	.quad	.Ltmp68
	.quad	.Ltmp69
	.quad	.Ltmp70
	.quad	.Ltmp72
	.quad	.Ltmp73
	.quad	.Ltmp74
	.quad	.Ltmp77
	.quad	0
	.quad	0
.Ldebug_ranges3:
	.quad	.Ltmp40
	.quad	.Ltmp41
	.quad	.Ltmp42
	.quad	.Ltmp43
	.quad	.Ltmp44
	.quad	.Ltmp51
	.quad	.Ltmp52
	.quad	.Ltmp53
	.quad	0
	.quad	0
.Ldebug_ranges4:
	.quad	.Ltmp54
	.quad	.Ltmp55
	.quad	.Ltmp56
	.quad	.Ltmp57
	.quad	.Ltmp58
	.quad	.Ltmp65
	.quad	0
	.quad	0
.Ldebug_ranges5:
	.quad	.Ltmp37
	.quad	.Ltmp38
	.quad	.Ltmp39
	.quad	.Ltmp77
	.quad	0
	.quad	0
	.section	.debug_pubnames,"",@progbits
.Lset0 = .Lpubnames_end0-.Lpubnames_begin0 # Length of Public Names Info
	.long	.Lset0
.Lpubnames_begin0:
	.short	2                       # DWARF Version
	.long	.L.debug_info_begin0    # Offset of Compilation Unit Info
.Lset1 = .L.debug_info_end0-.L.debug_info_begin0 # Compilation Unit Length
	.long	.Lset1
	.long	38                      # DIE offset
	.asciz	"BZ2_hbMakeCodeLengths" # External Name
	.long	508                     # DIE offset
	.asciz	"BZ2_hbAssignCodes"     # External Name
	.long	647                     # DIE offset
	.asciz	"BZ2_hbCreateDecodeTables" # External Name
	.long	0                       # End Mark
.Lpubnames_end0:
	.section	.debug_pubtypes,"",@progbits
.Lset2 = .Lpubtypes_end0-.Lpubtypes_begin0 # Length of Public Types Info
	.long	.Lset2
.Lpubtypes_begin0:
	.short	2                       # DWARF Version
	.long	.L.debug_info_begin0    # Offset of Compilation Unit Info
.Lset3 = .L.debug_info_end0-.L.debug_info_begin0 # Compilation Unit Length
	.long	.Lset3
	.long	856                     # DIE offset
	.asciz	"Int32"                 # External Name
	.long	833                     # DIE offset
	.asciz	"UChar"                 # External Name
	.long	844                     # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	867                     # DIE offset
	.asciz	"int"                   # External Name
	.long	874                     # DIE offset
	.asciz	"Bool"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
