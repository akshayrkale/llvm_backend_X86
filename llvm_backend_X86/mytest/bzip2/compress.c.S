	.file	"compress.c.bc"
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
	.file	1 "compress.c"
	.text
	.globl	BZ2_bsInitWrite
	.align	16, 0x90
	.type	BZ2_bsInitWrite,@function
BZ2_bsInitWrite:                        # @BZ2_bsInitWrite
.Lfunc_begin0:
	.loc	1 82 0                  # compress.c:82:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp4:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	xorq	%rax, %rax
	.loc	1 83 0 prologue_end     # compress.c:83:0
.Ltmp5:
	movl	%eax, 644(%rdi)
	.loc	1 84 0                  # compress.c:84:0
	movq	-8(%rbp), %rcx
	movl	%eax, 640(%rcx)
	.loc	1 85 0                  # compress.c:85:0
	popq	%rbp
	retq
.Ltmp6:
.Ltmp7:
	.size	BZ2_bsInitWrite, .Ltmp7-BZ2_bsInitWrite
.Lfunc_end0:
	.file	2 "./bzlib.h"
	.file	3 "./bzlib_private.h"
	.cfi_endproc

	.globl	BZ2_compressBlock
	.align	16, 0x90
	.type	BZ2_compressBlock,@function
BZ2_compressBlock:                      # @BZ2_compressBlock
.Lfunc_begin1:
	.loc	1 653 0                 # compress.c:653:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp10:
	.cfi_def_cfa_offset 16
.Ltmp11:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp12:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 654 0 prologue_end    # compress.c:654:0
.Ltmp13:
	movq	-8(%rbp), %rax
	movb	%sil, -9(%rbp)
	movslq	108(%rax), %rax
	testq	%rax, %rax
	jle	.LBB1_6
# BB#1:                                 # %if.then
	.loc	1 656 0                 # compress.c:656:0
.Ltmp14:
	movq	-8(%rbp), %rax
	movl	648(%rax), %ecx
	notq	%rcx
	movl	%ecx, 648(%rax)
.Ltmp15:
	.loc	1 657 0                 # compress.c:657:0
	movq	-8(%rbp), %rax
	movl	652(%rax), %edx
	movq	$31, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	leaq	(%rdx,%rdx), %rcx
	orq	%rsi, %rcx
	movl	%ecx, 652(%rax)
	.loc	1 658 0                 # compress.c:658:0
	movq	-8(%rbp), %rax
	movl	648(%rax), %ecx
	movl	652(%rax), %edx
	xorq	%rcx, %rdx
	movl	%edx, 652(%rax)
	.loc	1 659 0                 # compress.c:659:0
.Ltmp16:
	movq	-8(%rbp), %rax
	movslq	660(%rax), %rax
	cmpq	$2, %rax
	jl	.LBB1_3
# BB#2:                                 # %if.then7
	movq	-8(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 116(%rax)
.Ltmp17:
.LBB1_3:                                # %if.end
	.loc	1 661 0                 # compress.c:661:0
	movq	-8(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$2, %rax
	jl	.LBB1_5
# BB#4:                                 # %if.then9
	.loc	1 662 0                 # compress.c:662:0
	movq	-8(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	108(%rax), %r9d
	movl	652(%rax), %r8d
	movl	648(%rax), %ecx
	movl	660(%rax), %edx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp18:
.LBB1_5:                                # %if.end14
	.loc	1 666 0                 # compress.c:666:0
	movq	-8(%rbp), %rdi
	callq	BZ2_blockSort
.Ltmp19:
.LBB1_6:                                # %if.end15
	.loc	1 669 0                 # compress.c:669:0
	movq	-8(%rbp), %rax
	movslq	108(%rax), %rcx
	movq	32(%rax), %rdx
	addq	%rcx, %rdx
	movq	%rdx, 80(%rax)
	.loc	1 672 0                 # compress.c:672:0
.Ltmp20:
	movq	-8(%rbp), %rax
	movl	660(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB1_8
# BB#7:                                 # %if.then19
	.loc	1 673 0                 # compress.c:673:0
.Ltmp21:
	movq	-8(%rbp), %rdi
	callq	BZ2_bsInitWrite
	.loc	1 674 0                 # compress.c:674:0
	movq	-8(%rbp), %rdi
	movq	$66, %rsi
	callq	bsPutUChar
	.loc	1 675 0                 # compress.c:675:0
	movq	-8(%rbp), %rdi
	movq	$90, %rsi
	callq	bsPutUChar
	.loc	1 676 0                 # compress.c:676:0
	movq	-8(%rbp), %rdi
	movq	$104, %rsi
	callq	bsPutUChar
	.loc	1 677 0                 # compress.c:677:0
	movq	-8(%rbp), %rdi
	movl	664(%rdi), %esi
	addq	$48, %rsi
	andq	$255, %rsi
	callq	bsPutUChar
.Ltmp22:
.LBB1_8:                                # %if.end20
	.loc	1 680 0                 # compress.c:680:0
	movq	-8(%rbp), %rax
	movslq	108(%rax), %rax
	testq	%rax, %rax
	jle	.LBB1_10
# BB#9:                                 # %if.then24
	.loc	1 682 0                 # compress.c:682:0
.Ltmp23:
	movq	-8(%rbp), %rdi
	movq	$49, %rsi
	callq	bsPutUChar
	movq	-8(%rbp), %rdi
	movq	$65, %rsi
	callq	bsPutUChar
	.loc	1 683 0                 # compress.c:683:0
	movq	-8(%rbp), %rdi
	movq	$89, %rsi
	callq	bsPutUChar
	movq	-8(%rbp), %rdi
	movq	$38, %rsi
	callq	bsPutUChar
	.loc	1 684 0                 # compress.c:684:0
	movq	-8(%rbp), %rdi
	movq	$83, %rsi
	callq	bsPutUChar
	movq	-8(%rbp), %rdi
	movq	$89, %rsi
	callq	bsPutUChar
	.loc	1 687 0                 # compress.c:687:0
	movq	-8(%rbp), %rdi
	movl	648(%rdi), %esi
	callq	bsPutUInt32
	.loc	1 698 0                 # compress.c:698:0
	movq	-8(%rbp), %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	bsW
	.loc	1 700 0                 # compress.c:700:0
	movq	-8(%rbp), %rdi
	movl	48(%rdi), %edx
	movq	$24, %rsi
	callq	bsW
	.loc	1 701 0                 # compress.c:701:0
	movq	-8(%rbp), %rdi
	callq	generateMTFValues
	.loc	1 702 0                 # compress.c:702:0
	movq	-8(%rbp), %rdi
	callq	sendMTFValues
.Ltmp24:
.LBB1_10:                               # %if.end26
	.loc	1 707 0                 # compress.c:707:0
	movzbl	-9(%rbp), %eax
	testq	%rax, %rax
	je	.LBB1_14
# BB#11:                                # %if.then27
	.loc	1 709 0                 # compress.c:709:0
.Ltmp25:
	movq	-8(%rbp), %rdi
	movq	$23, %rsi
	callq	bsPutUChar
	movq	-8(%rbp), %rdi
	movq	$114, %rsi
	callq	bsPutUChar
	.loc	1 710 0                 # compress.c:710:0
	movq	-8(%rbp), %rdi
	movq	$69, %rsi
	callq	bsPutUChar
	movq	-8(%rbp), %rdi
	movq	$56, %rsi
	callq	bsPutUChar
	.loc	1 711 0                 # compress.c:711:0
	movq	-8(%rbp), %rdi
	movq	$80, %rsi
	callq	bsPutUChar
	movq	-8(%rbp), %rdi
	movq	$144, %rsi
	callq	bsPutUChar
	.loc	1 712 0                 # compress.c:712:0
	movq	-8(%rbp), %rdi
	movl	652(%rdi), %esi
	callq	bsPutUInt32
	.loc	1 713 0                 # compress.c:713:0
.Ltmp26:
	movq	-8(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$2, %rax
	jl	.LBB1_13
# BB#12:                                # %if.then32
	.loc	1 714 0                 # compress.c:714:0
	movq	-8(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	652(%rax), %edx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp27:
.LBB1_13:                               # %if.end35
	.loc	1 715 0                 # compress.c:715:0
	movq	-8(%rbp), %rdi
	callq	bsFinishWrite
.Ltmp28:
.LBB1_14:                               # %if.end36
	.loc	1 717 0                 # compress.c:717:0
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp29:
.Ltmp30:
	.size	BZ2_compressBlock, .Ltmp30-BZ2_compressBlock
.Lfunc_end1:
	.cfi_endproc

	.align	16, 0x90
	.type	bsPutUChar,@function
bsPutUChar:                             # @bsPutUChar
.Lfunc_begin2:
	.loc	1 139 0                 # compress.c:139:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp33:
	.cfi_def_cfa_offset 16
.Ltmp34:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp35:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 140 0 prologue_end    # compress.c:140:0
.Ltmp36:
	movq	-8(%rbp), %rdi
	movq	%rsi, %rax
	movb	%al, -9(%rbp)
	movq	$8, %rsi
	movq	%rax, %rdx
	callq	bsW
	.loc	1 141 0                 # compress.c:141:0
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp37:
.Ltmp38:
	.size	bsPutUChar, .Ltmp38-bsPutUChar
.Lfunc_end2:
	.cfi_endproc

	.align	16, 0x90
	.type	bsPutUInt32,@function
bsPutUInt32:                            # @bsPutUInt32
.Lfunc_begin3:
	.loc	1 128 0                 # compress.c:128:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp41:
	.cfi_def_cfa_offset 16
.Ltmp42:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp43:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	.loc	1 129 0 prologue_end    # compress.c:129:0
.Ltmp44:
	movq	-8(%rbp), %rdi
	movzbl	-9(%rbp), %edx
	movq	$8, %rsi
	callq	bsW
	.loc	1 130 0                 # compress.c:130:0
	movq	-8(%rbp), %rdi
	movzbl	-10(%rbp), %edx
	movq	$8, %rsi
	callq	bsW
	.loc	1 131 0                 # compress.c:131:0
	movl	-12(%rbp), %edx
	movq	-8(%rbp), %rdi
	movq	$8, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	$8, %rsi
	callq	bsW
	.loc	1 132 0                 # compress.c:132:0
	movq	-8(%rbp), %rdi
	movzbl	-12(%rbp), %edx
	movq	$8, %rsi
	callq	bsW
	.loc	1 133 0                 # compress.c:133:0
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp45:
.Ltmp46:
	.size	bsPutUInt32, .Ltmp46-bsPutUInt32
.Lfunc_end3:
	.cfi_endproc

	.align	16, 0x90
	.type	bsW,@function
bsW:                                    # @bsW
.Lfunc_begin4:
	.loc	1 118 0                 # compress.c:118:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp49:
	.cfi_def_cfa_offset 16
.Ltmp50:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp51:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	%edx, -16(%rbp)
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_2:                                # %while.body
                                        #   in Loop: Header=BB4_1 Depth=1
	.loc	1 119 0 prologue_end    # compress.c:119:0
.Ltmp52:
	movq	-8(%rbp), %rax
	movzbl	643(%rax), %ecx
	movslq	116(%rax), %rdx
	movq	80(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	116(%rax), %ecx
	incq	%rcx
	movl	%ecx, 116(%rax)
	movq	-8(%rbp), %rax
	movl	640(%rax), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movl	%edx, 640(%rax)
	movq	-8(%rbp), %rax
	movl	644(%rax), %ecx
	addq	$-8, %rcx
	movl	%ecx, 644(%rax)
.Ltmp53:
.LBB4_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 119 0                 # compress.c:119:0
	movq	-8(%rbp), %rax
	movslq	644(%rax), %rax
	cmpq	$8, %rax
	jge	.LBB4_2
.Ltmp54:
# BB#3:                                 # %while.end
	.loc	1 120 0                 # compress.c:120:0
	movq	-8(%rbp), %rax
	movl	-16(%rbp), %edx
	movl	644(%rax), %r8d
	movl	-12(%rbp), %edi
	movq	$32, %rcx
	movl	640(%rax), %esi
	subq	%r8, %rcx
	subq	%rdi, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rsi
	movl	%esi, 640(%rax)
	.loc	1 121 0                 # compress.c:121:0
	movq	-8(%rbp), %rax
	movl	-12(%rbp), %ecx
	movl	644(%rax), %edx
	addq	%rcx, %rdx
	movl	%edx, 644(%rax)
	.loc	1 122 0                 # compress.c:122:0
	popq	%rbp
	retq
.Ltmp55:
.Ltmp56:
	.size	bsW, .Ltmp56-bsW
.Lfunc_end4:
	.cfi_endproc

	.align	16, 0x90
	.type	generateMTFValues,@function
generateMTFValues:                      # @generateMTFValues
.Lfunc_begin5:
	.loc	1 249 0                 # compress.c:249:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp59:
	.cfi_def_cfa_offset 16
.Ltmp60:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp61:
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 273 0 prologue_end    # compress.c:273:0
.Ltmp62:
	movq	56(%rdi), %rax
	.loc	1 274 0                 # compress.c:274:0
	movq	-8(%rbp), %rcx
	.loc	1 273 0                 # compress.c:273:0
	movq	%rax, -24(%rbp)
	.loc	1 274 0                 # compress.c:274:0
	movq	64(%rcx), %rax
	.loc	1 275 0                 # compress.c:275:0
	movq	-8(%rbp), %rcx
	.loc	1 274 0                 # compress.c:274:0
	movq	%rax, -32(%rbp)
	.loc	1 275 0                 # compress.c:275:0
	movq	72(%rcx), %rax
	.loc	1 277 0                 # compress.c:277:0
	movq	-8(%rbp), %rdi
	.loc	1 275 0                 # compress.c:275:0
	movq	%rax, -40(%rbp)
	.loc	1 277 0                 # compress.c:277:0
	callq	makeMaps_e
	.loc	1 278 0                 # compress.c:278:0
	movq	-8(%rbp), %rax
	movl	124(%rax), %eax
	incq	%rax
	movl	%eax, -12(%rbp)
	.loc	1 280 0                 # compress.c:280:0
	movq	-8(%rbp), %rdi
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rcx
	movq	-40(%rbp), %r8
	movl	-12(%rbp), %esi
	callq	generateMTFValues1
	.loc	1 281 0                 # compress.c:281:0
	addq	$48, %rsp
	popq	%rbp
	retq
.Ltmp63:
.Ltmp64:
	.size	generateMTFValues, .Ltmp64-generateMTFValues
.Lfunc_end5:
	.cfi_endproc

	.align	16, 0x90
	.type	sendMTFValues,@function
sendMTFValues:                          # @sendMTFValues
.Lfunc_begin6:
	.loc	1 290 0                 # compress.c:290:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp68:
	.cfi_def_cfa_offset 16
.Ltmp69:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp70:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$232, %rsp
.Ltmp71:
	.cfi_offset %rbx, -56
.Ltmp72:
	.cfi_offset %r12, -48
.Ltmp73:
	.cfi_offset %r13, -40
.Ltmp74:
	.cfi_offset %r14, -32
.Ltmp75:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)
	.loc	1 309 0 prologue_end    # compress.c:309:0
.Ltmp76:
	movq	72(%rdi), %rax
	.loc	1 311 0                 # compress.c:311:0
.Ltmp77:
	movq	-48(%rbp), %rcx
.Ltmp78:
	.loc	1 309 0                 # compress.c:309:0
	movq	%rax, -168(%rbp)
	.loc	1 311 0                 # compress.c:311:0
.Ltmp79:
	movslq	656(%rcx), %rax
	cmpq	$3, %rax
	jl	.LBB6_2
# BB#1:                                 # %if.then
	.loc	1 312 0                 # compress.c:312:0
	movq	-48(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	124(%rax), %r8d
	movl	668(%rax), %ecx
	movl	108(%rax), %edx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp80:
.LBB6_2:                                # %if.end
	.loc	1 316 0                 # compress.c:316:0
	movq	-48(%rbp), %rax
	movl	124(%rax), %eax
	addq	$2, %rax
	movl	%eax, -96(%rbp)
	xorq	%rax, %rax
	.loc	1 317 0                 # compress.c:317:0
.Ltmp81:
	movl	%eax, -56(%rbp)
	movq	$15, %rcx
	jmp	.LBB6_3
	.align	16, 0x90
.LBB6_7:                                # %for.inc9
                                        #   in Loop: Header=BB6_3 Depth=1
	movl	-56(%rbp), %edx
	incq	%rdx
	movl	%edx, -56(%rbp)
.LBB6_3:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_5 Depth 2
	movslq	-56(%rbp), %rdx
	cmpq	$5, %rdx
	jg	.LBB6_8
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB6_3 Depth=1
	.loc	1 318 0                 # compress.c:318:0
.Ltmp82:
	movl	%eax, -52(%rbp)
	jmp	.LBB6_5
	.align	16, 0x90
.LBB6_6:                                # %for.inc
                                        #   in Loop: Header=BB6_5 Depth=2
	.loc	1 319 0                 # compress.c:319:0
	movslq	-56(%rbp), %rdx
	movq	-48(%rbp), %rsi
	movslq	-52(%rbp), %rdi
	imulq	$258, %rdx, %rdx        # imm = 0x102
	addq	%rsi, %rdx
	movb	%cl, 37708(%rdi,%rdx)
	.loc	1 318 0                 # compress.c:318:0
	movl	-52(%rbp), %edx
	incq	%rdx
	movl	%edx, -52(%rbp)
.LBB6_5:                                # %for.cond4
                                        #   Parent Loop BB6_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-52(%rbp), %rdx
	movslq	-96(%rbp), %rsi
	cmpq	%rsi, %rdx
	jl	.LBB6_6
	jmp	.LBB6_7
.Ltmp83:
.LBB6_8:                                # %for.end11
	.loc	1 322 0                 # compress.c:322:0
	movq	-48(%rbp), %rax
	movslq	668(%rax), %rax
	testq	%rax, %rax
	jg	.LBB6_10
# BB#9:                                 # %if.then14
	movq	$3001, %rdi             # imm = 0xBB9
	callq	BZ2_bz__AssertH__fail
.Ltmp84:
.LBB6_10:                               # %if.end15
	.loc	1 323 0                 # compress.c:323:0
	movq	-48(%rbp), %rax
	movslq	668(%rax), %rax
	cmpq	$199, %rax
	jg	.LBB6_12
# BB#11:                                # %if.then18
	movq	$2, %rax
	jmp	.LBB6_19
.LBB6_12:                               # %if.else
	.loc	1 324 0                 # compress.c:324:0
.Ltmp85:
	movq	-48(%rbp), %rax
	movslq	668(%rax), %rax
	cmpq	$599, %rax              # imm = 0x257
	jg	.LBB6_14
# BB#13:                                # %if.then21
	movq	$3, %rax
	jmp	.LBB6_19
.LBB6_14:                               # %if.else22
	.loc	1 325 0                 # compress.c:325:0
.Ltmp86:
	movq	-48(%rbp), %rax
	movslq	668(%rax), %rax
	cmpq	$1199, %rax             # imm = 0x4AF
	jg	.LBB6_16
# BB#15:                                # %if.then25
	movq	$4, %rax
	jmp	.LBB6_19
.LBB6_16:                               # %if.else26
	.loc	1 326 0                 # compress.c:326:0
.Ltmp87:
	movq	-48(%rbp), %rax
	movslq	668(%rax), %rax
	cmpq	$2399, %rax             # imm = 0x95F
	jg	.LBB6_18
# BB#17:                                # %if.then29
	movq	$5, %rax
	jmp	.LBB6_19
.LBB6_18:                               # %if.else30
	movq	$6, %rax
.LBB6_19:                               # %if.end34
	.loc	1 327 0                 # compress.c:327:0
	movl	%eax, -112(%rbp)
.Ltmp88:
	#DEBUG_VALUE: nPart <- [RBP+-172]
	#DEBUG_VALUE: remF <- [RBP+-176]
	#DEBUG_VALUE: tFreq <- [RBP+-180]
	#DEBUG_VALUE: aFreq <- [RBP+-184]
	.loc	1 333 0                 # compress.c:333:0
	movl	-112(%rbp), %eax
	movl	%eax, -172(%rbp)
	.loc	1 334 0                 # compress.c:334:0
	movq	-48(%rbp), %rax
	movl	668(%rax), %eax
	movl	%eax, -176(%rbp)
	xorq	%r15, %r15
	.loc	1 335 0                 # compress.c:335:0
	movl	%r15d, -68(%rbp)
	.loc	1 340 0                 # compress.c:340:0
.Ltmp89:
	movabsq	$0, %r14
	movabsq	$1, %rdi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	$15, %r13
	jmp	.LBB6_20
.Ltmp90:
	.align	16, 0x90
.LBB6_41:                               # %for.end93
                                        #   in Loop: Header=BB6_20 Depth=1
	.loc	1 363 0                 # compress.c:363:0
	movl	-172(%rbp), %eax
	decq	%rax
	.loc	1 364 0                 # compress.c:364:0
	movl	-72(%rbp), %ecx
	.loc	1 365 0                 # compress.c:365:0
	movl	-184(%rbp), %edx
	movl	-176(%rbp), %esi
	.loc	1 363 0                 # compress.c:363:0
	movl	%eax, -172(%rbp)
	.loc	1 364 0                 # compress.c:364:0
	incq	%rcx
	movl	%ecx, -68(%rbp)
	.loc	1 365 0                 # compress.c:365:0
	subq	%rdx, %rsi
	movl	%esi, -176(%rbp)
.Ltmp91:
.LBB6_20:                               # %while.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_22 Depth 2
                                        #     Child Loop BB6_35 Depth 2
	#DEBUG_VALUE: nPart <- [RBP+-172]
	#DEBUG_VALUE: remF <- [RBP+-176]
	#DEBUG_VALUE: tFreq <- [RBP+-180]
	#DEBUG_VALUE: aFreq <- [RBP+-184]
	.loc	1 336 0                 # compress.c:336:0
	movslq	-172(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB6_42
# BB#21:                                # %while.body
                                        #   in Loop: Header=BB6_20 Depth=1
	#DEBUG_VALUE: nPart <- [RBP+-172]
	#DEBUG_VALUE: remF <- [RBP+-176]
	#DEBUG_VALUE: tFreq <- [RBP+-180]
	#DEBUG_VALUE: aFreq <- [RBP+-184]
	.loc	1 337 0                 # compress.c:337:0
.Ltmp92:
	movslq	-176(%rbp), %rax
	movslq	-172(%rbp), %rcx
	cqto
	idivq	%rcx
	.loc	1 338 0                 # compress.c:338:0
	movl	-68(%rbp), %ecx
	.loc	1 337 0                 # compress.c:337:0
	movl	%eax, -180(%rbp)
	.loc	1 338 0                 # compress.c:338:0
	decq	%rcx
	movl	%ecx, -72(%rbp)
	.loc	1 339 0                 # compress.c:339:0
	movl	%r15d, -184(%rbp)
	jmp	.LBB6_22
.Ltmp93:
	.align	16, 0x90
.LBB6_26:                               # %while.body41
                                        #   in Loop: Header=BB6_22 Depth=2
	.loc	1 341 0                 # compress.c:341:0
	movl	-72(%rbp), %eax
	incq	%rax
	movl	%eax, -72(%rbp)
	.loc	1 342 0                 # compress.c:342:0
	movslq	-72(%rbp), %rax
	movq	-48(%rbp), %rcx
	movl	672(%rcx,%rax,4), %eax
	movl	-184(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -184(%rbp)
.Ltmp94:
.LBB6_22:                               # %while.cond37
                                        #   Parent Loop BB6_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: nPart <- [RBP+-172]
	#DEBUG_VALUE: remF <- [RBP+-176]
	#DEBUG_VALUE: tFreq <- [RBP+-180]
	#DEBUG_VALUE: aFreq <- [RBP+-184]
	.loc	1 340 0                 # compress.c:340:0
	movslq	-184(%rbp), %rcx
	movslq	-180(%rbp), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jge	.LBB6_25
# BB#23:                                # %land.rhs
                                        #   in Loop: Header=BB6_22 Depth=2
	#DEBUG_VALUE: nPart <- [RBP+-172]
	#DEBUG_VALUE: remF <- [RBP+-176]
	#DEBUG_VALUE: tFreq <- [RBP+-180]
	#DEBUG_VALUE: aFreq <- [RBP+-184]
	movl	-96(%rbp), %eax
	movslq	-72(%rbp), %rdx
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	movq	%r14, %rax
	jge	.LBB6_25
# BB#24:                                # %land.rhs
                                        #   in Loop: Header=BB6_22 Depth=2
.Ltmp95:
	#DEBUG_VALUE: nPart <- [RBP+-172]
	#DEBUG_VALUE: remF <- [RBP+-176]
	#DEBUG_VALUE: tFreq <- [RBP+-180]
	#DEBUG_VALUE: aFreq <- [RBP+-184]
	movq	%rdi, %rax
.Ltmp96:
.LBB6_25:                               # %land.end
                                        #   in Loop: Header=BB6_22 Depth=2
	testq	%rax, %rax
	jne	.LBB6_26
# BB#27:                                # %while.end
                                        #   in Loop: Header=BB6_20 Depth=1
	movq	%rdi, %r12
	.loc	1 345 0                 # compress.c:345:0
.Ltmp97:
	movslq	-72(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB6_32
# BB#28:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_20 Depth=1
	movl	-172(%rbp), %eax
	movl	-112(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_32
# BB#29:                                # %land.lhs.true48
                                        #   in Loop: Header=BB6_20 Depth=1
	movl	-172(%rbp), %eax
	cmpq	$1, %rax
	je	.LBB6_32
# BB#30:                                # %land.lhs.true50
                                        #   in Loop: Header=BB6_20 Depth=1
	movl	-112(%rbp), %eax
	movl	-172(%rbp), %ecx
	subq	%rcx, %rax
	movq	%rax, %rdx
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	andq	%rcx, %rdx
	movq	$31, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rdx
	subq	%rdx, %rax
	andq	%r9, %rax
	cmpq	$1, %rax
	jne	.LBB6_32
# BB#31:                                # %if.then53
                                        #   in Loop: Header=BB6_20 Depth=1
	.loc	1 348 0                 # compress.c:348:0
.Ltmp98:
	movslq	-72(%rbp), %rax
	movq	-48(%rbp), %rcx
	movl	672(%rcx,%rax,4), %eax
	movl	-184(%rbp), %ecx
	.loc	1 349 0                 # compress.c:349:0
	movl	-72(%rbp), %edx
	.loc	1 348 0                 # compress.c:348:0
	subq	%rax, %rcx
	movl	%ecx, -184(%rbp)
	.loc	1 349 0                 # compress.c:349:0
	decq	%rdx
	movl	%edx, -72(%rbp)
.Ltmp99:
	.align	16, 0x90
.LBB6_32:                               # %if.end58
                                        #   in Loop: Header=BB6_20 Depth=1
	.loc	1 352 0                 # compress.c:352:0
	movq	-48(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB6_34
# BB#33:                                # %if.then61
                                        #   in Loop: Header=BB6_20 Depth=1
	.loc	1 353 0                 # compress.c:353:0
	movl	-184(%rbp), %edi
	callq	int32_to_float64
	movq	%rax, %rdi
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	callq	float64_mul
	movq	-48(%rbp), %rcx
	movl	668(%rcx), %edi
	movq	%rax, %rbx
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movl	-72(%rbp), %r8d
	movl	-68(%rbp), %ecx
	movl	-172(%rbp), %edx
	movq	%rax, %rsi
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movq	stderr(%rip), %rdi
	movabsq	$.L.str3, %rsi
	movl	-184(%rbp), %r9d
	callq	fprintf
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
.Ltmp100:
.LBB6_34:                               # %if.end66
                                        #   in Loop: Header=BB6_20 Depth=1
	.loc	1 358 0                 # compress.c:358:0
	movl	%r15d, -52(%rbp)
	movq	%r12, %rdi
	jmp	.LBB6_35
	.align	16, 0x90
.LBB6_40:                               # %for.inc91
                                        #   in Loop: Header=BB6_35 Depth=2
	movl	-52(%rbp), %eax
	incq	%rax
	movl	%eax, -52(%rbp)
.LBB6_35:                               # %for.cond67
                                        #   Parent Loop BB6_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-52(%rbp), %rax
	movslq	-96(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_41
# BB#36:                                # %for.body70
                                        #   in Loop: Header=BB6_35 Depth=2
	.loc	1 359 0                 # compress.c:359:0
.Ltmp101:
	movslq	-52(%rbp), %rax
	movslq	-68(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_39
# BB#37:                                # %land.lhs.true73
                                        #   in Loop: Header=BB6_35 Depth=2
	movslq	-52(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB6_39
# BB#38:                                # %if.then76
                                        #   in Loop: Header=BB6_35 Depth=2
	.loc	1 360 0                 # compress.c:360:0
	movslq	-52(%rbp), %rax
	movl	-172(%rbp), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-48(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$258, %rdx, %rcx        # imm = 0x102
	addq	%rsi, %rcx
	movb	%r15b, 37708(%rax,%rcx)
	jmp	.LBB6_40
	.align	16, 0x90
.LBB6_39:                               # %if.else83
                                        #   in Loop: Header=BB6_35 Depth=2
	.loc	1 361 0                 # compress.c:361:0
	movslq	-52(%rbp), %rax
	movl	-172(%rbp), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	-48(%rbp), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$258, %rdx, %rcx        # imm = 0x102
	addq	%rsi, %rcx
	movb	%r13b, 37708(%rax,%rcx)
	jmp	.LBB6_40
.Ltmp102:
.LBB6_42:                               # %while.end97
	xorq	%r13, %r13
	.loc	1 372 0                 # compress.c:372:0
.Ltmp103:
	movl	%r13d, -88(%rbp)
	.loc	1 490 0                 # compress.c:490:0
.Ltmp104:
	movabsq	$.L.str5, %r12
	movq	$999999999, %r15        # imm = 0x3B9AC9FF
	jmp	.LBB6_43
.Ltmp105:
	.align	16, 0x90
.LBB6_93:                               # %for.inc1700
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 372 0                 # compress.c:372:0
	movl	-88(%rbp), %eax
	incq	%rax
	movl	%eax, -88(%rbp)
	movq	%r14, %r9
.LBB6_43:                               # %for.cond98
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_45 Depth 2
                                        #     Child Loop BB6_48 Depth 2
                                        #       Child Loop BB6_50 Depth 3
                                        #     Child Loop BB6_55 Depth 2
                                        #     Child Loop BB6_58 Depth 2
                                        #       Child Loop BB6_62 Depth 3
                                        #       Child Loop BB6_73 Depth 3
                                        #         Child Loop BB6_75 Depth 4
                                        #       Child Loop BB6_68 Depth 3
                                        #       Child Loop BB6_83 Depth 3
                                        #     Child Loop BB6_87 Depth 2
                                        #     Child Loop BB6_91 Depth 2
	movslq	-88(%rbp), %rax
	cmpq	$3, %rax
	jg	.LBB6_94
# BB#44:                                # %for.body101
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 374 0                 # compress.c:374:0
.Ltmp106:
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_45
	.align	16, 0x90
.LBB6_46:                               # %for.inc108
                                        #   in Loop: Header=BB6_45 Depth=2
	movl	-56(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movl	%r13d, -160(%rbp,%rcx,4)
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_45:                               # %for.cond102
                                        #   Parent Loop BB6_43 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_46
.Ltmp107:
# BB#47:                                # %for.end110
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 376 0                 # compress.c:376:0
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_48
	.align	16, 0x90
.LBB6_52:                               # %for.inc126
                                        #   in Loop: Header=BB6_48 Depth=2
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_48:                               # %for.cond111
                                        #   Parent Loop BB6_43 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_50 Depth 3
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_53
# BB#49:                                # %for.body114
                                        #   in Loop: Header=BB6_48 Depth=2
	.loc	1 377 0                 # compress.c:377:0
.Ltmp108:
	movl	%r13d, -52(%rbp)
	jmp	.LBB6_50
	.align	16, 0x90
.LBB6_51:                               # %for.inc123
                                        #   in Loop: Header=BB6_50 Depth=3
	.loc	1 378 0                 # compress.c:378:0
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-52(%rbp), %rdx
	imulq	$1032, %rax, %rax       # imm = 0x408
	addq	%rcx, %rax
	movl	%r13d, 45448(%rax,%rdx,4)
	.loc	1 377 0                 # compress.c:377:0
	movl	-52(%rbp), %eax
	incq	%rax
	movl	%eax, -52(%rbp)
.LBB6_50:                               # %for.cond115
                                        #   Parent Loop BB6_43 Depth=1
                                        #     Parent Loop BB6_48 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-52(%rbp), %rax
	movslq	-96(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_51
	jmp	.LBB6_52
.Ltmp109:
	.align	16, 0x90
.LBB6_53:                               # %for.end128
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 384 0                 # compress.c:384:0
	movl	-112(%rbp), %eax
	cmpq	$6, %rax
	jne	.LBB6_57
# BB#54:                                # %if.then131
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 385 0                 # compress.c:385:0
.Ltmp110:
	movl	%r13d, -52(%rbp)
	jmp	.LBB6_55
	.align	16, 0x90
.LBB6_56:                               # %for.inc181
                                        #   in Loop: Header=BB6_55 Depth=2
	.loc	1 386 0                 # compress.c:386:0
.Ltmp111:
	movslq	-52(%rbp), %rax
	movq	-48(%rbp), %rdx
	movzbl	37966(%rdx,%rax), %esi
	movq	$16, %rcx
	shlq	%cl, %rsi
	movzbl	37708(%rdx,%rax), %edi
	orq	%rsi, %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	%edi, 51640(%rdx,%rax)
	.loc	1 387 0                 # compress.c:387:0
	movslq	-52(%rbp), %rax
	movq	-48(%rbp), %rdx
	movzbl	38482(%rdx,%rax), %esi
	movq	$16, %rcx
	shlq	%cl, %rsi
	movzbl	38224(%rdx,%rax), %edi
	orq	%rsi, %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	%edi, 51644(%rdx,%rax)
	.loc	1 388 0                 # compress.c:388:0
	movslq	-52(%rbp), %rax
	movq	-48(%rbp), %rdx
	movzbl	38998(%rdx,%rax), %esi
	movq	$16, %rcx
	movzbl	38740(%rdx,%rax), %edi
	shlq	%cl, %rsi
	orq	%rsi, %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	%edi, 51648(%rdx,%rax)
.Ltmp112:
	.loc	1 385 0                 # compress.c:385:0
	movl	-52(%rbp), %eax
	incq	%rax
	movl	%eax, -52(%rbp)
.LBB6_55:                               # %for.cond132
                                        #   Parent Loop BB6_43 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-52(%rbp), %rax
	movslq	-96(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_56
.Ltmp113:
.LBB6_57:                               # %if.end184
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 392 0                 # compress.c:392:0
	movl	%r13d, -92(%rbp)
	.loc	1 393 0                 # compress.c:393:0
	movl	%r13d, -76(%rbp)
	.loc	1 394 0                 # compress.c:394:0
	movl	%r13d, -68(%rbp)
	jmp	.LBB6_58
	.align	16, 0x90
.LBB6_81:                               # %if.end1663
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 484 0                 # compress.c:484:0
.Ltmp114:
	movl	-72(%rbp), %eax
	incq	%rax
	movl	%eax, -68(%rbp)
.LBB6_58:                               # %while.body186
                                        #   Parent Loop BB6_43 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_62 Depth 3
                                        #       Child Loop BB6_73 Depth 3
                                        #         Child Loop BB6_75 Depth 4
                                        #       Child Loop BB6_68 Depth 3
                                        #       Child Loop BB6_83 Depth 3
	.loc	1 398 0                 # compress.c:398:0
.Ltmp115:
	movq	-48(%rbp), %rax
	movslq	-68(%rbp), %rcx
	movslq	668(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB6_85
.Ltmp116:
# BB#59:                                # %if.end191
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 399 0                 # compress.c:399:0
	movl	-68(%rbp), %eax
	.loc	1 400 0                 # compress.c:400:0
.Ltmp117:
	movq	-48(%rbp), %rdx
.Ltmp118:
	.loc	1 399 0                 # compress.c:399:0
	addq	$49, %rax
	movq	$32, %rcx
	movl	%eax, -72(%rbp)
	.loc	1 400 0                 # compress.c:400:0
.Ltmp119:
	movslq	668(%rdx), %rdx
.Ltmp120:
	.loc	1 399 0                 # compress.c:399:0
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB6_61
# BB#60:                                # %if.then197
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 400 0                 # compress.c:400:0
.Ltmp121:
	movq	-48(%rbp), %rax
	movl	668(%rax), %eax
	decq	%rax
	movl	%eax, -72(%rbp)
.Ltmp122:
.LBB6_61:                               # %if.end200
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 406 0                 # compress.c:406:0
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_62
	.align	16, 0x90
.LBB6_63:                               # %for.inc207
                                        #   in Loop: Header=BB6_62 Depth=3
	movl	-56(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movw	%r13w, -128(%rbp,%rcx,2)
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_62:                               # %for.cond201
                                        #   Parent Loop BB6_43 Depth=1
                                        #     Parent Loop BB6_58 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_63
.Ltmp123:
# BB#64:                                # %for.end209
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 408 0                 # compress.c:408:0
	movl	-112(%rbp), %eax
	cmpq	$6, %rax
	jne	.LBB6_72
# BB#65:                                # %land.lhs.true212
                                        #   in Loop: Header=BB6_58 Depth=2
	movl	-72(%rbp), %eax
	movl	-68(%rbp), %ecx
	subq	%rcx, %rax
	andq	%r9, %rax
	cmpq	$49, %rax
	jne	.LBB6_72
# BB#66:                                # %if.then217
                                        #   in Loop: Header=BB6_58 Depth=2
.Ltmp124:
	#DEBUG_VALUE: cost01 <- [RBP+-188]
	#DEBUG_VALUE: cost23 <- [RBP+-192]
	#DEBUG_VALUE: cost45 <- [RBP+-196]
	#DEBUG_VALUE: icv <- [RBP+-198]
	.loc	1 420 0                 # compress.c:420:0
	movslq	-68(%rbp), %rax
	movq	-168(%rbp), %rcx
	.loc	1 412 0                 # compress.c:412:0
	movl	%r13d, -196(%rbp)
	movl	%r13d, -192(%rbp)
	movl	%r13d, -188(%rbp)
	.loc	1 420 0                 # compress.c:420:0
	movzbl	(%rcx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$2, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$3, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$4, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 421 0                 # compress.c:421:0
	movl	-68(%rbp), %eax
	addq	$5, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$6, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$7, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$8, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$9, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 422 0                 # compress.c:422:0
	movl	-68(%rbp), %eax
	addq	$10, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$11, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$12, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$13, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$14, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 423 0                 # compress.c:423:0
	movl	-68(%rbp), %eax
	addq	$15, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$16, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$17, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$18, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$19, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 424 0                 # compress.c:424:0
	movl	-68(%rbp), %eax
	addq	$20, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$21, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$22, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$23, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$24, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 425 0                 # compress.c:425:0
	movl	-68(%rbp), %eax
	addq	$25, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$26, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$27, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$28, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$29, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 426 0                 # compress.c:426:0
	movl	-68(%rbp), %eax
	addq	$30, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$31, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$32, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$33, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$34, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 427 0                 # compress.c:427:0
	movl	-68(%rbp), %eax
	addq	$35, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$36, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$37, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$38, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$39, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 428 0                 # compress.c:428:0
	movl	-68(%rbp), %eax
	addq	$40, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$41, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$42, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$43, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$44, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 429 0                 # compress.c:429:0
	movl	-68(%rbp), %eax
	addq	$45, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$46, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$47, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$48, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	movl	-68(%rbp), %eax
	addq	$49, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -198(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51640(%rdx,%rax), %eax
	movl	-188(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -188(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51644(%rdx,%rax), %eax
	movl	-192(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -192(%rbp)
	movzbl	-198(%rbp), %eax
	movq	-48(%rbp), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	51648(%rdx,%rax), %eax
	movl	-196(%rbp), %ecx
	addq	%rax, %rcx
	movl	%ecx, -196(%rbp)
	.loc	1 433 0                 # compress.c:433:0
	movzbl	-188(%rbp), %eax
	movw	%ax, -128(%rbp)
	movzbl	-186(%rbp), %eax
	movw	%ax, -126(%rbp)
	.loc	1 434 0                 # compress.c:434:0
	movzbl	-192(%rbp), %eax
	movzbl	-190(%rbp), %ecx
	.loc	1 435 0                 # compress.c:435:0
	movzbl	-196(%rbp), %edx
	movzbl	-194(%rbp), %esi
	.loc	1 434 0                 # compress.c:434:0
	movw	%ax, -124(%rbp)
	movw	%cx, -122(%rbp)
	.loc	1 435 0                 # compress.c:435:0
	movw	%dx, -120(%rbp)
	movw	%si, -118(%rbp)
	jmp	.LBB6_67
.Ltmp125:
	.align	16, 0x90
.LBB6_72:                               # %if.else1134
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 439 0                 # compress.c:439:0
	movl	-68(%rbp), %eax
	jmp	.LBB6_73
	.align	16, 0x90
.LBB6_77:                               # %for.inc1160
                                        #   in Loop: Header=BB6_73 Depth=3
	movl	-60(%rbp), %eax
	incq	%rax
.LBB6_73:                               # %for.cond1135
                                        #   Parent Loop BB6_43 Depth=1
                                        #     Parent Loop BB6_58 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB6_75 Depth 4
	movl	%eax, -60(%rbp)
	movslq	-60(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jg	.LBB6_67
# BB#74:                                # %for.body1138
                                        #   in Loop: Header=BB6_73 Depth=3
.Ltmp126:
	#DEBUG_VALUE: icv <- [RBP+-200]
	.loc	1 440 0                 # compress.c:440:0
	movslq	-60(%rbp), %rax
	movq	-168(%rbp), %rcx
	movzbl	(%rcx,%rax,2), %eax
	movw	%ax, -200(%rbp)
	.loc	1 441 0                 # compress.c:441:0
.Ltmp127:
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_75
	.align	16, 0x90
.LBB6_76:                               # %for.inc1157
                                        #   in Loop: Header=BB6_75 Depth=4
	#DEBUG_VALUE: icv <- [RBP+-200]
	movzbl	-200(%rbp), %eax
	movslq	-56(%rbp), %rcx
	movq	-48(%rbp), %rdx
	imulq	$258, %rcx, %rsi        # imm = 0x102
	addq	%rdx, %rsi
	movzbl	37708(%rax,%rsi), %eax
	movzbl	-128(%rbp,%rcx,2), %edx
	movl	-56(%rbp), %esi
	addq	%rax, %rdx
	movw	%dx, -128(%rbp,%rcx,2)
	incq	%rsi
	movl	%esi, -56(%rbp)
.LBB6_75:                               # %for.cond1142
                                        #   Parent Loop BB6_43 Depth=1
                                        #     Parent Loop BB6_58 Depth=2
                                        #       Parent Loop BB6_73 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
.Ltmp128:
	#DEBUG_VALUE: icv <- [RBP+-200]
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_76
	jmp	.LBB6_77
.Ltmp129:
	.align	16, 0x90
.LBB6_67:                               # %if.end1163
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 449 0                 # compress.c:449:0
	movl	%r15d, -84(%rbp)
	movl	%r9d, -80(%rbp)
	.loc	1 450 0                 # compress.c:450:0
.Ltmp130:
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_68
	.align	16, 0x90
.LBB6_71:                               # %for.inc1178
                                        #   in Loop: Header=BB6_68 Depth=3
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_68:                               # %for.cond1164
                                        #   Parent Loop BB6_43 Depth=1
                                        #     Parent Loop BB6_58 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_78
# BB#69:                                # %for.body1167
                                        #   in Loop: Header=BB6_68 Depth=3
	.loc	1 451 0                 # compress.c:451:0
.Ltmp131:
	movslq	-56(%rbp), %rax
	movzbl	-128(%rbp,%rax,2), %eax
	movslq	-84(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_71
# BB#70:                                # %if.then1173
                                        #   in Loop: Header=BB6_68 Depth=3
	.loc	1 451 0                 # compress.c:451:0
.Ltmp132:
	movslq	-56(%rbp), %rax
	movzbl	-128(%rbp,%rax,2), %eax
	movl	-56(%rbp), %ecx
	movl	%eax, -84(%rbp)
	movl	%ecx, -80(%rbp)
	jmp	.LBB6_71
.Ltmp133:
	.align	16, 0x90
.LBB6_78:                               # %for.end1180
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 452 0                 # compress.c:452:0
	movl	-84(%rbp), %r8d
	movl	-76(%rbp), %ecx
	.loc	1 453 0                 # compress.c:453:0
	movslq	-80(%rbp), %rdx
	movl	-160(%rbp,%rdx,4), %esi
	incq	%rsi
	.loc	1 454 0                 # compress.c:454:0
	movzbl	-80(%rbp), %edi
	movslq	-92(%rbp), %rbx
	movq	-48(%rbp), %rax
	.loc	1 453 0                 # compress.c:453:0
	movl	%esi, -160(%rbp,%rdx,4)
	.loc	1 452 0                 # compress.c:452:0
	addq	%r8, %rcx
	movl	%ecx, -76(%rbp)
	.loc	1 454 0                 # compress.c:454:0
	movb	%dil, 1704(%rax,%rbx)
	.loc	1 455 0                 # compress.c:455:0
	movl	-92(%rbp), %eax
	.loc	1 460 0                 # compress.c:460:0
.Ltmp134:
	movl	-112(%rbp), %ecx
.Ltmp135:
	.loc	1 455 0                 # compress.c:455:0
	incq	%rax
	movl	%eax, -92(%rbp)
	.loc	1 460 0                 # compress.c:460:0
.Ltmp136:
	cmpq	$6, %rcx
	jne	.LBB6_82
# BB#79:                                # %land.lhs.true1191
                                        #   in Loop: Header=BB6_58 Depth=2
	movl	-72(%rbp), %eax
	movl	-68(%rbp), %ecx
	subq	%rcx, %rax
	andq	%r9, %rax
	cmpq	$49, %rax
	jne	.LBB6_82
# BB#80:                                # %if.then1196
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 465 0                 # compress.c:465:0
.Ltmp137:
	movslq	-68(%rbp), %rax
	movq	-168(%rbp), %rcx
	movslq	-80(%rbp), %rdx
	movq	-48(%rbp), %rsi
	movzbl	(%rcx,%rax,2), %eax
	imulq	$1032, %rdx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$2, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$3, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$4, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 466 0                 # compress.c:466:0
	movl	-68(%rbp), %eax
	addq	$5, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$6, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$7, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$9, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 467 0                 # compress.c:467:0
	movl	-68(%rbp), %eax
	addq	$10, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$11, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$12, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$13, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$14, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 468 0                 # compress.c:468:0
	movl	-68(%rbp), %eax
	addq	$15, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$16, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$17, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$18, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$19, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 469 0                 # compress.c:469:0
	movl	-68(%rbp), %eax
	addq	$20, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$21, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$22, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$23, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$24, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 470 0                 # compress.c:470:0
	movl	-68(%rbp), %eax
	addq	$25, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$26, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$27, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$28, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$29, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 471 0                 # compress.c:471:0
	movl	-68(%rbp), %eax
	addq	$30, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$31, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$32, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$33, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$34, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 472 0                 # compress.c:472:0
	movl	-68(%rbp), %eax
	addq	$35, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$36, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$37, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$38, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$39, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 473 0                 # compress.c:473:0
	movl	-68(%rbp), %eax
	addq	$40, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$41, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$42, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$43, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$44, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 474 0                 # compress.c:474:0
	movl	-68(%rbp), %eax
	addq	$45, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$46, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$47, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$48, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	movl	-68(%rbp), %eax
	addq	$49, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	sarq	%cl, %rax
	movslq	-80(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movzbl	(%rdx,%rax,2), %eax
	imulq	$1032, %rcx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	jmp	.LBB6_81
.Ltmp138:
	.align	16, 0x90
.LBB6_82:                               # %if.else1647
                                        #   in Loop: Header=BB6_58 Depth=2
	.loc	1 480 0                 # compress.c:480:0
	movl	-68(%rbp), %eax
	jmp	.LBB6_83
	.align	16, 0x90
.LBB6_84:                               # %for.inc1660
                                        #   in Loop: Header=BB6_83 Depth=3
	.loc	1 481 0                 # compress.c:481:0
	movslq	-60(%rbp), %rax
	movq	-168(%rbp), %rcx
	movslq	-80(%rbp), %rdx
	movq	-48(%rbp), %rsi
	movzbl	(%rcx,%rax,2), %eax
	imulq	$1032, %rdx, %rcx       # imm = 0x408
	addq	%rsi, %rcx
	movl	45448(%rcx,%rax,4), %edx
	incq	%rdx
	movl	%edx, 45448(%rcx,%rax,4)
	.loc	1 480 0                 # compress.c:480:0
	movl	-60(%rbp), %eax
	incq	%rax
.LBB6_83:                               # %for.cond1648
                                        #   Parent Loop BB6_43 Depth=1
                                        #     Parent Loop BB6_58 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	%eax, -60(%rbp)
	movslq	-60(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB6_84
	jmp	.LBB6_81
.Ltmp139:
	.align	16, 0x90
.LBB6_85:                               # %while.end1665
                                        #   in Loop: Header=BB6_43 Depth=1
	movq	%r9, %r14
	.loc	1 486 0                 # compress.c:486:0
.Ltmp140:
	movq	-48(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB6_90
# BB#86:                                # %if.then1669
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 487 0                 # compress.c:487:0
.Ltmp141:
	movq	stderr(%rip), %rdi
	movl	-88(%rbp), %edx
	movslq	-76(%rbp), %rax
	incq	%rdx
	movq	$60, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	andq	$7, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$35, %rcx
	sarq	%cl, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movq	%rbx, %rcx
	callq	fprintf
	.loc	1 489 0                 # compress.c:489:0
.Ltmp142:
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_87
	.align	16, 0x90
.LBB6_88:                               # %for.inc1680
                                        #   in Loop: Header=BB6_87 Depth=2
	.loc	1 490 0                 # compress.c:490:0
	movslq	-56(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	-160(%rbp,%rax,4), %edx
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	fprintf
	.loc	1 489 0                 # compress.c:489:0
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_87:                               # %for.cond1673
                                        #   Parent Loop BB6_43 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_88
.Ltmp143:
# BB#89:                                # %for.end1682
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 491 0                 # compress.c:491:0
	movq	stderr(%rip), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str6, %rsi
	callq	fprintf
.Ltmp144:
.LBB6_90:                               # %if.end1684
                                        #   in Loop: Header=BB6_43 Depth=1
	.loc	1 499 0                 # compress.c:499:0
	movl	%r13d, -56(%rbp)
	jmp	.LBB6_91
	.align	16, 0x90
.LBB6_92:                               # %for.inc1697
                                        #   in Loop: Header=BB6_91 Depth=2
	.loc	1 500 0                 # compress.c:500:0
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	imulq	$258, %rax, %rsi        # imm = 0x102
	movl	-96(%rbp), %edx
	leaq	37708(%rcx,%rsi), %rdi
	imulq	$1032, %rax, %rax       # imm = 0x408
	leaq	45448(%rcx,%rax), %rsi
	movq	$17, %rcx
	callq	BZ2_hbMakeCodeLengths
	.loc	1 499 0                 # compress.c:499:0
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_91:                               # %for.cond1685
                                        #   Parent Loop BB6_43 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_92
	jmp	.LBB6_93
.Ltmp145:
.LBB6_94:                               # %for.end1702
	movq	%r9, %r15
	.loc	1 505 0                 # compress.c:505:0
.Ltmp146:
	movslq	-112(%rbp), %rax
	cmpq	$8, %rax
	jl	.LBB6_96
# BB#95:                                # %if.then1705
	movq	$3002, %rdi             # imm = 0xBBA
	callq	BZ2_bz__AssertH__fail
.Ltmp147:
.LBB6_96:                               # %if.end1706
	.loc	1 506 0                 # compress.c:506:0
	movslq	-92(%rbp), %rax
	cmpq	$32767, %rax            # imm = 0x7FFF
	jg	.LBB6_98
# BB#97:                                # %land.lhs.true1709
	movslq	-92(%rbp), %rax
	cmpq	$18003, %rax            # imm = 0x4653
	jl	.LBB6_99
.LBB6_98:                               # %if.then1712
	movq	$3003, %rdi             # imm = 0xBBB
	callq	BZ2_bz__AssertH__fail
.Ltmp148:
.LBB6_99:                               # %if.end1713
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	xorq	%rax, %rax
	jmp	.LBB6_100
	.align	16, 0x90
.LBB6_101:                              # %for.inc1721
                                        #   in Loop: Header=BB6_100 Depth=1
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	.loc	1 514 0                 # compress.c:514:0
.Ltmp149:
	movl	-60(%rbp), %eax
	movslq	-60(%rbp), %rcx
	movb	%cl, -206(%rbp,%rcx)
	incq	%rax
.Ltmp150:
.LBB6_100:                              # %for.cond1714
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -60(%rbp)
.Ltmp151:
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	movslq	-60(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_101
.Ltmp152:
# BB#102:                               # %for.end1723
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	xorq	%rax, %rax
	.loc	1 515 0                 # compress.c:515:0
.Ltmp153:
	movl	%eax, -60(%rbp)
	jmp	.LBB6_103
	.align	16, 0x90
.LBB6_107:                              # %for.inc1749
                                        #   in Loop: Header=BB6_103 Depth=1
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	.loc	1 525 0                 # compress.c:525:0
.Ltmp154:
	movzbl	-209(%rbp), %ecx
	.loc	1 526 0                 # compress.c:526:0
	movzbl	-64(%rbp), %edx
	movslq	-60(%rbp), %rsi
	movq	-48(%rbp), %rdi
	.loc	1 525 0                 # compress.c:525:0
	movb	%cl, -206(%rbp)
	.loc	1 526 0                 # compress.c:526:0
	movb	%dl, 19706(%rdi,%rsi)
.Ltmp155:
	.loc	1 515 0                 # compress.c:515:0
	movl	-60(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -60(%rbp)
.LBB6_103:                              # %for.cond1724
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_105 Depth 2
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	movslq	-60(%rbp), %rcx
	movslq	-92(%rbp), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB6_108
# BB#104:                               # %for.body1727
                                        #   in Loop: Header=BB6_103 Depth=1
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	.loc	1 516 0                 # compress.c:516:0
.Ltmp156:
	movslq	-60(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movzbl	1704(%rdx,%rcx), %ecx
	.loc	1 517 0                 # compress.c:517:0
	movl	%eax, -64(%rbp)
	.loc	1 518 0                 # compress.c:518:0
	movslq	-64(%rbp), %rdx
	movzbl	-206(%rbp,%rdx), %edx
	.loc	1 516 0                 # compress.c:516:0
	movb	%cl, -207(%rbp)
	.loc	1 518 0                 # compress.c:518:0
	movb	%dl, -209(%rbp)
	jmp	.LBB6_105
	.align	16, 0x90
.LBB6_106:                              # %while.body1738
                                        #   in Loop: Header=BB6_105 Depth=2
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	.loc	1 520 0                 # compress.c:520:0
.Ltmp157:
	movl	-64(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -64(%rbp)
	.loc	1 521 0                 # compress.c:521:0
	movzbl	-209(%rbp), %ecx
	.loc	1 522 0                 # compress.c:522:0
	movslq	-64(%rbp), %rdx
	.loc	1 521 0                 # compress.c:521:0
	movb	%cl, -208(%rbp)
	.loc	1 522 0                 # compress.c:522:0
	movzbl	-206(%rbp,%rdx), %ecx
	.loc	1 523 0                 # compress.c:523:0
	movzbl	-208(%rbp), %edx
	movslq	-64(%rbp), %rsi
	.loc	1 522 0                 # compress.c:522:0
	movb	%cl, -209(%rbp)
	.loc	1 523 0                 # compress.c:523:0
	movb	%dl, -206(%rbp,%rsi)
.Ltmp158:
.LBB6_105:                              # %while.cond1733
                                        #   Parent Loop BB6_103 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: pos <- [RBP+-206]
	#DEBUG_VALUE: ll_i <- [RBP+-207]
	#DEBUG_VALUE: tmp2 <- [RBP+-208]
	#DEBUG_VALUE: tmp <- [RBP+-209]
	.loc	1 519 0                 # compress.c:519:0
	movzbl	-207(%rbp), %ecx
	movzbl	-209(%rbp), %edx
	cmpq	%rdx, %rcx
	jne	.LBB6_106
	jmp	.LBB6_107
.Ltmp159:
.LBB6_108:                              # %for.end1751
	xorq	%rbx, %rbx
	.loc	1 531 0                 # compress.c:531:0
.Ltmp160:
	movl	%ebx, -56(%rbp)
	movq	$32, %r14
	jmp	.LBB6_109
	.align	16, 0x90
.LBB6_121:                              # %for.inc1810
                                        #   in Loop: Header=BB6_109 Depth=1
	.loc	1 540 0                 # compress.c:540:0
.Ltmp161:
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rsi
	imulq	$1032, %rax, %rdi       # imm = 0x408
	movl	-96(%rbp), %r8d
	movl	-104(%rbp), %ecx
	movl	-100(%rbp), %edx
	leaq	39256(%rsi,%rdi), %rdi
	imulq	$258, %rax, %rax        # imm = 0x102
	leaq	37708(%rsi,%rax), %rsi
	callq	BZ2_hbAssignCodes
.Ltmp162:
	.loc	1 531 0                 # compress.c:531:0
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_109:                              # %for.cond1752
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_111 Depth 2
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_122
# BB#110:                               # %for.body1755
                                        #   in Loop: Header=BB6_109 Depth=1
	.loc	1 532 0                 # compress.c:532:0
.Ltmp163:
	movl	%r14d, -100(%rbp)
	.loc	1 533 0                 # compress.c:533:0
	movl	%ebx, -104(%rbp)
	.loc	1 534 0                 # compress.c:534:0
.Ltmp164:
	movl	%ebx, -60(%rbp)
	jmp	.LBB6_111
	.align	16, 0x90
.LBB6_116:                              # %for.inc1792
                                        #   in Loop: Header=BB6_111 Depth=2
	movl	-60(%rbp), %eax
	incq	%rax
	movl	%eax, -60(%rbp)
.LBB6_111:                              # %for.cond1756
                                        #   Parent Loop BB6_109 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-60(%rbp), %rax
	movslq	-96(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_117
# BB#112:                               # %for.body1759
                                        #   in Loop: Header=BB6_111 Depth=2
	.loc	1 535 0                 # compress.c:535:0
.Ltmp165:
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-60(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	37708(%rdx,%rax), %eax
	movslq	-104(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB6_114
# BB#113:                               # %if.then1768
                                        #   in Loop: Header=BB6_111 Depth=2
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-60(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	37708(%rdx,%rax), %eax
	movl	%eax, -104(%rbp)
.Ltmp166:
.LBB6_114:                              # %if.end1775
                                        #   in Loop: Header=BB6_111 Depth=2
	.loc	1 536 0                 # compress.c:536:0
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-60(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	37708(%rdx,%rax), %eax
	movslq	-100(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_116
# BB#115:                               # %if.then1784
                                        #   in Loop: Header=BB6_111 Depth=2
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-60(%rbp), %rdx
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	37708(%rdx,%rax), %eax
	movl	%eax, -100(%rbp)
	jmp	.LBB6_116
.Ltmp167:
	.align	16, 0x90
.LBB6_117:                              # %for.end1794
                                        #   in Loop: Header=BB6_109 Depth=1
	.loc	1 538 0                 # compress.c:538:0
	movslq	-104(%rbp), %rax
	cmpq	$18, %rax
	jl	.LBB6_119
# BB#118:                               # %if.then1797
                                        #   in Loop: Header=BB6_109 Depth=1
	movq	$3004, %rdi             # imm = 0xBBC
	callq	BZ2_bz__AssertH__fail
.Ltmp168:
.LBB6_119:                              # %if.end1798
                                        #   in Loop: Header=BB6_109 Depth=1
	.loc	1 539 0                 # compress.c:539:0
	movslq	-100(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB6_121
# BB#120:                               # %if.then1801
                                        #   in Loop: Header=BB6_109 Depth=1
	movq	$3005, %rdi             # imm = 0xBBD
	callq	BZ2_bz__AssertH__fail
	jmp	.LBB6_121
.Ltmp169:
.LBB6_122:                              # %for.end1812
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	xorq	%rax, %rax
	.loc	1 547 0                 # compress.c:547:0
.Ltmp170:
	movl	%eax, -60(%rbp)
	movq	$1, %rdx
	jmp	.LBB6_123
	.align	16, 0x90
.LBB6_129:                              # %for.inc1835
                                        #   in Loop: Header=BB6_123 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movl	-60(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -60(%rbp)
.LBB6_123:                              # %for.cond1814
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_125 Depth 2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movslq	-60(%rbp), %rcx
	cmpq	$15, %rcx
	jg	.LBB6_130
# BB#124:                               # %for.body1817
                                        #   in Loop: Header=BB6_123 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 548 0                 # compress.c:548:0
.Ltmp171:
	movslq	-60(%rbp), %rcx
	movb	%al, -240(%rbp,%rcx)
	.loc	1 549 0                 # compress.c:549:0
.Ltmp172:
	movl	%eax, -64(%rbp)
	jmp	.LBB6_125
	.align	16, 0x90
.LBB6_128:                              # %for.inc1832
                                        #   in Loop: Header=BB6_125 Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movl	-64(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -64(%rbp)
.LBB6_125:                              # %for.cond1820
                                        #   Parent Loop BB6_123 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movslq	-64(%rbp), %rcx
	cmpq	$15, %rcx
	jg	.LBB6_129
# BB#126:                               # %for.body1823
                                        #   in Loop: Header=BB6_125 Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 550 0                 # compress.c:550:0
.Ltmp173:
	movl	-60(%rbp), %esi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	-64(%rbp), %edi
	addq	%rsi, %rdi
	movq	$32, %rcx
	movq	-48(%rbp), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	128(%rsi,%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB6_128
# BB#127:                               # %if.then1828
                                        #   in Loop: Header=BB6_125 Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movslq	-60(%rbp), %rcx
	movb	%dl, -240(%rbp,%rcx)
	jmp	.LBB6_128
.Ltmp174:
.LBB6_130:                              # %for.end1837
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 553 0                 # compress.c:553:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %eax
	movl	%eax, -116(%rbp)
	xorq	%rax, %rax
	jmp	.LBB6_131
.Ltmp175:
	.align	16, 0x90
.LBB6_135:                              # %for.inc1848
                                        #   in Loop: Header=BB6_131 Depth=1
	.loc	1 555 0                 # compress.c:555:0
	callq	bsW
.Ltmp176:
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 554 0                 # compress.c:554:0
	movl	-60(%rbp), %eax
	incq	%rax
.Ltmp177:
.LBB6_131:                              # %for.cond1838
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -60(%rbp)
.Ltmp178:
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movslq	-60(%rbp), %rax
	cmpq	$15, %rax
	jg	.LBB6_136
# BB#132:                               # %for.body1841
                                        #   in Loop: Header=BB6_131 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 555 0                 # compress.c:555:0
.Ltmp179:
	movslq	-60(%rbp), %rax
	movzbl	-240(%rbp,%rax), %eax
	testq	%rax, %rax
	je	.LBB6_134
# BB#133:                               # %if.then1845
                                        #   in Loop: Header=BB6_131 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	jmp	.LBB6_135
	.align	16, 0x90
.LBB6_134:                              # %if.else1846
                                        #   in Loop: Header=BB6_131 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB6_135
.Ltmp180:
.LBB6_136:                              # %for.end1850
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	xorq	%rbx, %rbx
	.loc	1 557 0                 # compress.c:557:0
.Ltmp181:
	movl	%ebx, -60(%rbp)
	jmp	.LBB6_137
	.align	16, 0x90
.LBB6_145:                              # %for.inc1876
                                        #   in Loop: Header=BB6_137 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movl	-60(%rbp), %eax
	incq	%rax
	movl	%eax, -60(%rbp)
.LBB6_137:                              # %for.cond1851
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_140 Depth 2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movslq	-60(%rbp), %rax
	cmpq	$15, %rax
	jg	.LBB6_146
# BB#138:                               # %for.body1854
                                        #   in Loop: Header=BB6_137 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 558 0                 # compress.c:558:0
.Ltmp182:
	movslq	-60(%rbp), %rax
	movzbl	-240(%rbp,%rax), %eax
	testq	%rax, %rax
	je	.LBB6_145
# BB#139:                               # %if.then1858
                                        #   in Loop: Header=BB6_137 Depth=1
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 559 0                 # compress.c:559:0
.Ltmp183:
	movl	%ebx, -64(%rbp)
	jmp	.LBB6_140
.Ltmp184:
	.align	16, 0x90
.LBB6_144:                              # %for.inc1872
                                        #   in Loop: Header=BB6_140 Depth=2
	.loc	1 560 0                 # compress.c:560:0
	callq	bsW
.Ltmp185:
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 559 0                 # compress.c:559:0
	movl	-64(%rbp), %eax
	incq	%rax
	movl	%eax, -64(%rbp)
.LBB6_140:                              # %for.cond1859
                                        #   Parent Loop BB6_137 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movslq	-64(%rbp), %rax
	cmpq	$15, %rax
	jg	.LBB6_145
# BB#141:                               # %for.body1862
                                        #   in Loop: Header=BB6_140 Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 560 0                 # compress.c:560:0
.Ltmp186:
	movl	-60(%rbp), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	-64(%rbp), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	-48(%rbp), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	128(%rax,%rdx), %eax
	testq	%rax, %rax
	je	.LBB6_143
# BB#142:                               # %if.then1869
                                        #   in Loop: Header=BB6_140 Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	jmp	.LBB6_144
	.align	16, 0x90
.LBB6_143:                              # %if.else1870
                                        #   in Loop: Header=BB6_140 Depth=2
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB6_144
.Ltmp187:
.LBB6_146:                              # %for.end1878
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 563 0                 # compress.c:563:0
	movq	-48(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB6_148
# BB#147:                               # %if.then1882
.Ltmp188:
	#DEBUG_VALUE: inUse16 <- [RBP+-240]
	.loc	1 564 0                 # compress.c:564:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %edx
	movl	-116(%rbp), %eax
	movq	stderr(%rip), %rdi
	subq	%rax, %rdx
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp189:
.LBB6_148:                              # %if.end1886
	.loc	1 568 0                 # compress.c:568:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %eax
	.loc	1 569 0                 # compress.c:569:0
	movq	-48(%rbp), %rdi
	movl	-112(%rbp), %edx
	.loc	1 568 0                 # compress.c:568:0
	movl	%eax, -116(%rbp)
	movq	$3, %rsi
	.loc	1 569 0                 # compress.c:569:0
	callq	bsW
	.loc	1 570 0                 # compress.c:570:0
	movq	-48(%rbp), %rdi
	movl	-92(%rbp), %edx
	movq	$15, %rsi
	callq	bsW
	xorq	%rbx, %rbx
	.loc	1 571 0                 # compress.c:571:0
.Ltmp190:
	movl	%ebx, -60(%rbp)
	jmp	.LBB6_149
	.align	16, 0x90
.LBB6_153:                              # %for.inc1903
                                        #   in Loop: Header=BB6_149 Depth=1
	.loc	1 573 0                 # compress.c:573:0
.Ltmp191:
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	bsW
.Ltmp192:
	.loc	1 571 0                 # compress.c:571:0
	movl	-60(%rbp), %eax
	incq	%rax
	movl	%eax, -60(%rbp)
.LBB6_149:                              # %for.cond1888
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_151 Depth 2
	movslq	-60(%rbp), %rax
	movslq	-92(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_154
# BB#150:                               # %for.body1891
                                        #   in Loop: Header=BB6_149 Depth=1
	.loc	1 572 0                 # compress.c:572:0
.Ltmp193:
	movl	%ebx, -64(%rbp)
	jmp	.LBB6_151
	.align	16, 0x90
.LBB6_152:                              # %for.inc1900
                                        #   in Loop: Header=BB6_151 Depth=2
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	callq	bsW
	movl	-64(%rbp), %eax
	incq	%rax
	movl	%eax, -64(%rbp)
.LBB6_151:                              # %for.cond1892
                                        #   Parent Loop BB6_149 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-60(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-64(%rbp), %rdx
	movzbl	19706(%rcx,%rax), %eax
	cmpq	%rax, %rdx
	jl	.LBB6_152
	jmp	.LBB6_153
.Ltmp194:
.LBB6_154:                              # %for.end1905
	.loc	1 575 0                 # compress.c:575:0
	movq	-48(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB6_156
# BB#155:                               # %if.then1909
	.loc	1 576 0                 # compress.c:576:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %edx
	movl	-116(%rbp), %eax
	movq	stderr(%rip), %rdi
	subq	%rax, %rdx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp195:
.LBB6_156:                              # %if.end1913
	.loc	1 579 0                 # compress.c:579:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %eax
	movl	%eax, -116(%rbp)
	xorq	%rbx, %rbx
	.loc	1 581 0                 # compress.c:581:0
.Ltmp196:
	movl	%ebx, -56(%rbp)
	jmp	.LBB6_157
	.align	16, 0x90
.LBB6_165:                              # %for.inc1956
                                        #   in Loop: Header=BB6_157 Depth=1
	movl	-56(%rbp), %eax
	incq	%rax
	movl	%eax, -56(%rbp)
.LBB6_157:                              # %for.cond1915
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_159 Depth 2
                                        #       Child Loop BB6_160 Depth 3
                                        #       Child Loop BB6_162 Depth 3
	movslq	-56(%rbp), %rax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB6_166
# BB#158:                               # %for.body1918
                                        #   in Loop: Header=BB6_157 Depth=1
.Ltmp197:
	#DEBUG_VALUE: curr <- [RBP+-244]
	.loc	1 582 0                 # compress.c:582:0
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	imulq	$258, %rax, %rax        # imm = 0x102
	movzbl	37708(%rcx,%rax), %eax
	movl	%eax, -244(%rbp)
	.loc	1 583 0                 # compress.c:583:0
	movq	-48(%rbp), %rdi
	movl	-244(%rbp), %edx
	movq	$5, %rsi
	callq	bsW
	.loc	1 584 0                 # compress.c:584:0
.Ltmp198:
	movl	%ebx, -60(%rbp)
	jmp	.LBB6_159
	.align	16, 0x90
.LBB6_164:                              # %for.inc1953
                                        #   in Loop: Header=BB6_159 Depth=2
	#DEBUG_VALUE: curr <- [RBP+-244]
	.loc	1 587 0                 # compress.c:587:0
.Ltmp199:
	movq	-48(%rbp), %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	bsW
.Ltmp200:
	.loc	1 584 0                 # compress.c:584:0
	movl	-60(%rbp), %eax
	incq	%rax
	movl	%eax, -60(%rbp)
.LBB6_159:                              # %for.cond1925
                                        #   Parent Loop BB6_157 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_160 Depth 3
                                        #       Child Loop BB6_162 Depth 3
	#DEBUG_VALUE: curr <- [RBP+-244]
	movslq	-60(%rbp), %rax
	movslq	-96(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_160
	jmp	.LBB6_165
	.align	16, 0x90
.LBB6_161:                              # %while.body1938
                                        #   in Loop: Header=BB6_160 Depth=3
	#DEBUG_VALUE: curr <- [RBP+-244]
	.loc	1 585 0                 # compress.c:585:0
.Ltmp201:
	movq	-48(%rbp), %rdi
	movq	$2, %rsi
	movq	$2, %rdx
	callq	bsW
	movl	-244(%rbp), %eax
	incq	%rax
	movl	%eax, -244(%rbp)
.Ltmp202:
.LBB6_160:                              # %while.cond1929
                                        #   Parent Loop BB6_157 Depth=1
                                        #     Parent Loop BB6_159 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	#DEBUG_VALUE: curr <- [RBP+-244]
	.loc	1 585 0                 # compress.c:585:0
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-60(%rbp), %rdx
	movslq	-244(%rbp), %rsi
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	37708(%rdx,%rax), %eax
	cmpq	%rax, %rsi
	jl	.LBB6_161
	jmp	.LBB6_162
	.align	16, 0x90
.LBB6_163:                              # %while.body1950
                                        #   in Loop: Header=BB6_162 Depth=3
	#DEBUG_VALUE: curr <- [RBP+-244]
	.loc	1 586 0                 # compress.c:586:0
.Ltmp203:
	movq	-48(%rbp), %rdi
	movq	$2, %rsi
	movq	$3, %rdx
	callq	bsW
	movl	-244(%rbp), %eax
	decq	%rax
	movl	%eax, -244(%rbp)
.Ltmp204:
.LBB6_162:                              # %while.cond1941
                                        #   Parent Loop BB6_157 Depth=1
                                        #     Parent Loop BB6_159 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	#DEBUG_VALUE: curr <- [RBP+-244]
	.loc	1 586 0                 # compress.c:586:0
	movslq	-56(%rbp), %rax
	movq	-48(%rbp), %rcx
	movslq	-60(%rbp), %rdx
	movslq	-244(%rbp), %rsi
	imulq	$258, %rax, %rax        # imm = 0x102
	addq	%rcx, %rax
	movzbl	37708(%rdx,%rax), %eax
	cmpq	%rax, %rsi
	jg	.LBB6_163
	jmp	.LBB6_164
.Ltmp205:
.LBB6_166:                              # %for.end1958
	.loc	1 591 0                 # compress.c:591:0
	movq	-48(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB6_168
# BB#167:                               # %if.then1962
	.loc	1 592 0                 # compress.c:592:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %edx
	movl	-116(%rbp), %eax
	movq	stderr(%rip), %rdi
	subq	%rax, %rdx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp206:
.LBB6_168:                              # %if.end1966
	.loc	1 595 0                 # compress.c:595:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %eax
	movl	%eax, -116(%rbp)
	xorq	%rax, %rax
	.loc	1 596 0                 # compress.c:596:0
	movl	%eax, -108(%rbp)
	.loc	1 597 0                 # compress.c:597:0
	movl	%eax, -68(%rbp)
	jmp	.LBB6_169
	.align	16, 0x90
.LBB6_177:                              # %if.end2446
                                        #   in Loop: Header=BB6_169 Depth=1
	.loc	1 641 0                 # compress.c:641:0
.Ltmp207:
	movl	-72(%rbp), %eax
	.loc	1 642 0                 # compress.c:642:0
	movl	-108(%rbp), %ecx
	.loc	1 641 0                 # compress.c:641:0
	incq	%rax
	movl	%eax, -68(%rbp)
	.loc	1 642 0                 # compress.c:642:0
	incq	%rcx
	movl	%ecx, -108(%rbp)
.LBB6_169:                              # %while.body1969
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_179 Depth 2
	.loc	1 599 0                 # compress.c:599:0
.Ltmp208:
	movq	-48(%rbp), %rax
	movslq	-68(%rbp), %rcx
	movslq	668(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB6_181
.Ltmp209:
# BB#170:                               # %if.end1974
                                        #   in Loop: Header=BB6_169 Depth=1
	.loc	1 600 0                 # compress.c:600:0
	movl	-68(%rbp), %eax
	.loc	1 601 0                 # compress.c:601:0
.Ltmp210:
	movq	-48(%rbp), %rdx
.Ltmp211:
	.loc	1 600 0                 # compress.c:600:0
	addq	$49, %rax
	movq	$32, %rcx
	movl	%eax, -72(%rbp)
	.loc	1 601 0                 # compress.c:601:0
.Ltmp212:
	movslq	668(%rdx), %rdx
.Ltmp213:
	.loc	1 600 0                 # compress.c:600:0
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB6_172
# BB#171:                               # %if.then1980
                                        #   in Loop: Header=BB6_169 Depth=1
	.loc	1 601 0                 # compress.c:601:0
.Ltmp214:
	movq	-48(%rbp), %rax
	movl	668(%rax), %eax
	decq	%rax
	movl	%eax, -72(%rbp)
.Ltmp215:
.LBB6_172:                              # %if.end1983
                                        #   in Loop: Header=BB6_169 Depth=1
	.loc	1 602 0                 # compress.c:602:0
	movslq	-108(%rbp), %rax
	movq	-48(%rbp), %rcx
	movzbl	1704(%rcx,%rax), %eax
	movslq	-112(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB6_174
# BB#173:                               # %if.then1990
                                        #   in Loop: Header=BB6_169 Depth=1
	movq	$3006, %rdi             # imm = 0xBBE
	callq	BZ2_bz__AssertH__fail
.Ltmp216:
.LBB6_174:                              # %if.end1991
                                        #   in Loop: Header=BB6_169 Depth=1
	.loc	1 604 0                 # compress.c:604:0
	movl	-112(%rbp), %eax
	cmpq	$6, %rax
	jne	.LBB6_178
# BB#175:                               # %land.lhs.true1994
                                        #   in Loop: Header=BB6_169 Depth=1
	movl	-72(%rbp), %eax
	movl	-68(%rbp), %ecx
	subq	%rcx, %rax
	andq	%r15, %rax
	cmpq	$49, %rax
	jne	.LBB6_178
# BB#176:                               # %if.then1999
                                        #   in Loop: Header=BB6_169 Depth=1
.Ltmp217:
	#DEBUG_VALUE: mtfv_i <- [RBP+-246]
	#DEBUG_VALUE: s_len_sel_selCtr <- [RBP+-256]
	.loc	1 607 0                 # compress.c:607:0
	movslq	-108(%rbp), %rax
	movq	-48(%rbp), %rcx
	movzbl	1704(%rcx,%rax), %eax
	.loc	1 609 0                 # compress.c:609:0
	movslq	-108(%rbp), %rdx
	movq	-48(%rbp), %rsi
	.loc	1 607 0                 # compress.c:607:0
	imulq	$258, %rax, %rax        # imm = 0x102
	leaq	37708(%rcx,%rax), %rax
	movq	%rax, -256(%rbp)
.Ltmp218:
	#DEBUG_VALUE: s_code_sel_selCtr <- [RBP+-264]
	.loc	1 609 0                 # compress.c:609:0
	movzbl	1704(%rsi,%rdx), %eax
	.loc	1 618 0                 # compress.c:618:0
	movslq	-68(%rbp), %rcx
	movq	-168(%rbp), %rdx
	.loc	1 609 0                 # compress.c:609:0
	imulq	$1032, %rax, %rax       # imm = 0x408
	leaq	39256(%rsi,%rax), %rax
	movq	%rax, -264(%rbp)
	.loc	1 618 0                 # compress.c:618:0
	movzbl	(%rdx,%rcx,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$2, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$3, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$4, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 619 0                 # compress.c:619:0
	movl	-68(%rbp), %eax
	addq	$5, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$6, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$7, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$8, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$9, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 620 0                 # compress.c:620:0
	movl	-68(%rbp), %eax
	addq	$10, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$11, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$12, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$13, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$14, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 621 0                 # compress.c:621:0
	movl	-68(%rbp), %eax
	addq	$15, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$16, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$17, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$18, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$19, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 622 0                 # compress.c:622:0
	movl	-68(%rbp), %eax
	addq	$20, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$21, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$22, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$23, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$24, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 623 0                 # compress.c:623:0
	movl	-68(%rbp), %eax
	addq	$25, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$26, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$27, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$28, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$29, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 624 0                 # compress.c:624:0
	movl	-68(%rbp), %eax
	addq	$30, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$31, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$32, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$33, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$34, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 625 0                 # compress.c:625:0
	movl	-68(%rbp), %eax
	addq	$35, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$36, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$37, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$38, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$39, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 626 0                 # compress.c:626:0
	movl	-68(%rbp), %eax
	addq	$40, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$41, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$42, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$43, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$44, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	.loc	1 627 0                 # compress.c:627:0
	movl	-68(%rbp), %eax
	addq	$45, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$46, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$47, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$48, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	movl	-68(%rbp), %eax
	addq	$49, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rdx,%rax,2), %eax
	movw	%ax, -246(%rbp)
	movzbl	-246(%rbp), %eax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	-48(%rbp), %rdi
	movzbl	(%rcx,%rax), %esi
	movl	(%rdx,%rax,4), %edx
	callq	bsW
	jmp	.LBB6_177
.Ltmp219:
	.align	16, 0x90
.LBB6_178:                              # %if.else2417
                                        #   in Loop: Header=BB6_169 Depth=1
	.loc	1 633 0                 # compress.c:633:0
	movl	-68(%rbp), %eax
	jmp	.LBB6_179
	.align	16, 0x90
.LBB6_180:                              # %for.inc2443
                                        #   in Loop: Header=BB6_179 Depth=2
	.loc	1 634 0                 # compress.c:634:0
.Ltmp220:
	movq	-48(%rbp), %rdi
	movslq	-108(%rbp), %rax
	movslq	-60(%rbp), %rcx
	movq	-168(%rbp), %rdx
	movzbl	(%rdx,%rcx,2), %ecx
	movzbl	1704(%rdi,%rax), %eax
	imulq	$258, %rax, %rdx        # imm = 0x102
	addq	%rdi, %rdx
	movzbl	37708(%rcx,%rdx), %esi
	imulq	$1032, %rax, %rax       # imm = 0x408
	addq	%rdi, %rax
	movl	39256(%rax,%rcx,4), %edx
	callq	bsW
.Ltmp221:
	.loc	1 633 0                 # compress.c:633:0
	movl	-60(%rbp), %eax
	incq	%rax
.LBB6_179:                              # %for.cond2418
                                        #   Parent Loop BB6_169 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%eax, -60(%rbp)
	movslq	-60(%rbp), %rax
	movslq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	jle	.LBB6_180
	jmp	.LBB6_177
.Ltmp222:
.LBB6_181:                              # %while.end2449
	.loc	1 644 0                 # compress.c:644:0
	movl	-108(%rbp), %eax
	movl	-92(%rbp), %ecx
	cmpq	%rcx, %rax
	je	.LBB6_183
# BB#182:                               # %if.then2452
	movq	$3007, %rdi             # imm = 0xBBF
	callq	BZ2_bz__AssertH__fail
.Ltmp223:
.LBB6_183:                              # %if.end2453
	.loc	1 646 0                 # compress.c:646:0
	movq	-48(%rbp), %rax
	movslq	656(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB6_185
# BB#184:                               # %if.then2457
	.loc	1 647 0                 # compress.c:647:0
	movq	-48(%rbp), %rax
	movl	116(%rax), %edx
	movl	-116(%rbp), %eax
	movq	stderr(%rip), %rdi
	subq	%rax, %rdx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp224:
.LBB6_185:                              # %if.end2461
	.loc	1 648 0                 # compress.c:648:0
	addq	$232, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp225:
.Ltmp226:
	.size	sendMTFValues, .Ltmp226-sendMTFValues
.Lfunc_end6:
	.cfi_endproc

	.align	16, 0x90
	.type	bsFinishWrite,@function
bsFinishWrite:                          # @bsFinishWrite
.Lfunc_begin7:
	.loc	1 91 0                  # compress.c:91:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp229:
	.cfi_def_cfa_offset 16
.Ltmp230:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp231:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	jmp	.LBB7_1
	.align	16, 0x90
.LBB7_2:                                # %while.body
                                        #   in Loop: Header=BB7_1 Depth=1
	.loc	1 93 0 prologue_end     # compress.c:93:0
.Ltmp232:
	movq	-8(%rbp), %rax
	movzbl	643(%rax), %ecx
	movslq	116(%rax), %rdx
	movq	80(%rax), %rax
	movb	%cl, (%rax,%rdx)
	.loc	1 94 0                  # compress.c:94:0
	movq	-8(%rbp), %rax
	movl	116(%rax), %ecx
	incq	%rcx
	movl	%ecx, 116(%rax)
	.loc	1 95 0                  # compress.c:95:0
	movq	-8(%rbp), %rax
	movl	640(%rax), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movl	%edx, 640(%rax)
	.loc	1 96 0                  # compress.c:96:0
	movq	-8(%rbp), %rax
	movl	644(%rax), %ecx
	addq	$-8, %rcx
	movl	%ecx, 644(%rax)
.Ltmp233:
.LBB7_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 92 0                  # compress.c:92:0
	movq	-8(%rbp), %rax
	movslq	644(%rax), %rax
	testq	%rax, %rax
	jg	.LBB7_2
# BB#3:                                 # %while.end
	.loc	1 98 0                  # compress.c:98:0
	popq	%rbp
	retq
.Ltmp234:
.Ltmp235:
	.size	bsFinishWrite, .Ltmp235-bsFinishWrite
.Lfunc_end7:
	.cfi_endproc

	.align	16, 0x90
	.type	makeMaps_e,@function
makeMaps_e:                             # @makeMaps_e
.Lfunc_begin8:
	.loc	1 151 0                 # compress.c:151:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp238:
	.cfi_def_cfa_offset 16
.Ltmp239:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp240:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	xorq	%rax, %rax
	.loc	1 153 0 prologue_end    # compress.c:153:0
.Ltmp241:
	movl	%eax, 124(%rdi)
	jmp	.LBB8_1
	.align	16, 0x90
.LBB8_4:                                # %for.inc
                                        #   in Loop: Header=BB8_1 Depth=1
	.loc	1 154 0                 # compress.c:154:0
.Ltmp242:
	movl	-12(%rbp), %eax
	incq	%rax
.LBB8_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -12(%rbp)
	movslq	-12(%rbp), %rax
	cmpq	$255, %rax
	jg	.LBB8_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB8_1 Depth=1
	.loc	1 155 0                 # compress.c:155:0
.Ltmp243:
	movslq	-12(%rbp), %rax
	movq	-8(%rbp), %rcx
	movzbl	128(%rcx,%rax), %eax
	testq	%rax, %rax
	je	.LBB8_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB8_1 Depth=1
	.loc	1 156 0                 # compress.c:156:0
.Ltmp244:
	movq	-8(%rbp), %rax
	movzbl	124(%rax), %ecx
	movslq	-12(%rbp), %rdx
	movb	%cl, 384(%rax,%rdx)
	.loc	1 157 0                 # compress.c:157:0
	movq	-8(%rbp), %rax
	movl	124(%rax), %ecx
	incq	%rcx
	movl	%ecx, 124(%rax)
	jmp	.LBB8_4
.Ltmp245:
.LBB8_5:                                # %for.end
	.loc	1 159 0                 # compress.c:159:0
	popq	%rbp
	retq
.Ltmp246:
.Ltmp247:
	.size	makeMaps_e, .Ltmp247-makeMaps_e
.Lfunc_end8:
	.cfi_endproc

	.align	16, 0x90
	.type	generateMTFValues1,@function
generateMTFValues1:                     # @generateMTFValues1
.Lfunc_begin9:
	.loc	1 165 0                 # compress.c:165:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp250:
	.cfi_def_cfa_offset 16
.Ltmp251:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp252:
	.cfi_def_cfa_register %rbp
	subq	$224, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8, -40(%rbp)
	xorq	%rax, %rax
	.loc	1 171 0 prologue_end    # compress.c:171:0
.Ltmp253:
	movl	%eax, -308(%rbp)
	jmp	.LBB9_1
	.align	16, 0x90
.LBB9_2:                                # %for.inc
                                        #   in Loop: Header=BB9_1 Depth=1
	movslq	-308(%rbp), %rcx
	movq	-8(%rbp), %rdx
	movl	%eax, 672(%rdx,%rcx,4)
	movl	-308(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -308(%rbp)
.LBB9_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-308(%rbp), %rcx
	movslq	-12(%rbp), %rdx
	cmpq	%rdx, %rcx
	jle	.LBB9_2
.Ltmp254:
# BB#3:                                 # %for.end
	xorq	%rax, %rax
	.loc	1 173 0                 # compress.c:173:0
	movl	%eax, -320(%rbp)
	.loc	1 174 0                 # compress.c:174:0
	movl	%eax, -316(%rbp)
	jmp	.LBB9_4
	.align	16, 0x90
.LBB9_5:                                # %for.inc6
                                        #   in Loop: Header=BB9_4 Depth=1
	.loc	1 175 0                 # compress.c:175:0
.Ltmp255:
	movl	-308(%rbp), %eax
	movslq	-308(%rbp), %rcx
	movb	%cl, -304(%rbp,%rcx)
	incq	%rax
.LBB9_4:                                # %for.cond1
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -308(%rbp)
	movq	-8(%rbp), %rax
	movslq	-308(%rbp), %rcx
	movslq	124(%rax), %rax
	cmpq	%rax, %rcx
	jl	.LBB9_5
.Ltmp256:
# BB#6:                                 # %for.end8
	xorq	%rdx, %rdx
	.loc	1 177 0                 # compress.c:177:0
.Ltmp257:
	movl	%edx, -308(%rbp)
	movq	$1, %r10
	movabsq	$2147483648, %rax       # imm = 0x80000000
	.loc	1 207 0                 # compress.c:207:0
.Ltmp258:
	leaq	-303(%rbp), %r8
	leaq	-304(%rbp), %r9
	jmp	.LBB9_7
.Ltmp259:
	.align	16, 0x90
.LBB9_24:                               # %for.inc77
                                        #   in Loop: Header=BB9_7 Depth=1
	.loc	1 177 0                 # compress.c:177:0
	movl	-308(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -308(%rbp)
.LBB9_7:                                # %for.cond9
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_14 Depth 2
                                        #     Child Loop BB9_21 Depth 2
	movq	-8(%rbp), %rcx
	movslq	-308(%rbp), %rdi
	movslq	108(%rcx), %rcx
	cmpq	%rcx, %rdi
	jge	.LBB9_25
# BB#8:                                 # %for.body12
                                        #   in Loop: Header=BB9_7 Depth=1
.Ltmp260:
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 180 0                 # compress.c:180:0
	movslq	-308(%rbp), %rcx
	movq	-24(%rbp), %rdi
	movl	(%rdi,%rcx,4), %esi
	decq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movl	%esi, -312(%rbp)
	jns	.LBB9_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 180 0                 # compress.c:180:0
.Ltmp261:
	movq	-8(%rbp), %rcx
	movl	108(%rcx), %ecx
	movl	-312(%rbp), %esi
	addq	%rcx, %rsi
	movl	%esi, -312(%rbp)
.Ltmp262:
.LBB9_10:                               # %if.end
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 181 0                 # compress.c:181:0
	movslq	-312(%rbp), %rcx
	movq	-32(%rbp), %rsi
	movzbl	(%rsi,%rcx), %ecx
	movq	-8(%rbp), %rsi
	movzbl	384(%rsi,%rcx), %ecx
	movb	%cl, -321(%rbp)
	.loc	1 184 0                 # compress.c:184:0
.Ltmp263:
	movzbl	-304(%rbp), %ecx
	movzbl	-321(%rbp), %esi
	cmpq	%rsi, %rcx
	jne	.LBB9_12
# BB#11:                                # %if.then27
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 185 0                 # compress.c:185:0
.Ltmp264:
	movl	-316(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -316(%rbp)
	jmp	.LBB9_24
.Ltmp265:
	.align	16, 0x90
.LBB9_12:                               # %if.else
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 188 0                 # compress.c:188:0
	movslq	-316(%rbp), %rcx
	testq	%rcx, %rcx
	jle	.LBB9_20
# BB#13:                                # %if.then31
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 189 0                 # compress.c:189:0
.Ltmp266:
	movl	-316(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -316(%rbp)
	jmp	.LBB9_14
	.align	16, 0x90
.LBB9_18:                               # %if.end50
                                        #   in Loop: Header=BB9_14 Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 199 0                 # compress.c:199:0
.Ltmp267:
	movl	-316(%rbp), %esi
	leaq	-2(%rsi), %rdi
	andq	%rax, %rdi
	movq	$31, %rcx
	shrq	%cl, %rdi
	leaq	-2(%rsi,%rdi), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rcx
	shrq	%cl, %rsi
	movl	%esi, -316(%rbp)
.LBB9_14:                               # %while.body
                                        #   Parent Loop BB9_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 191 0                 # compress.c:191:0
.Ltmp268:
	movzbl	-316(%rbp), %ecx
	testq	$1, %rcx
	je	.LBB9_16
# BB#15:                                # %if.then32
                                        #   in Loop: Header=BB9_14 Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 192 0                 # compress.c:192:0
.Ltmp269:
	movslq	-320(%rbp), %rcx
	movq	-40(%rbp), %rsi
	movw	%r10w, (%rsi,%rcx,2)
	movl	-320(%rbp), %ecx
	.loc	1 193 0                 # compress.c:193:0
	movq	-8(%rbp), %rsi
	.loc	1 192 0                 # compress.c:192:0
	incq	%rcx
	movl	%ecx, -320(%rbp)
	.loc	1 193 0                 # compress.c:193:0
	movl	676(%rsi), %ecx
	incq	%rcx
	movl	%ecx, 676(%rsi)
	jmp	.LBB9_17
.Ltmp270:
	.align	16, 0x90
.LBB9_16:                               # %if.else39
                                        #   in Loop: Header=BB9_14 Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 195 0                 # compress.c:195:0
	movslq	-320(%rbp), %rcx
	movq	-40(%rbp), %rsi
	movw	%dx, (%rsi,%rcx,2)
	movl	-320(%rbp), %ecx
	.loc	1 196 0                 # compress.c:196:0
	movq	-8(%rbp), %rsi
	.loc	1 195 0                 # compress.c:195:0
	incq	%rcx
	movl	%ecx, -320(%rbp)
	.loc	1 196 0                 # compress.c:196:0
	movl	672(%rsi), %ecx
	incq	%rcx
	movl	%ecx, 672(%rsi)
.Ltmp271:
.LBB9_17:                               # %if.end46
                                        #   in Loop: Header=BB9_14 Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 198 0                 # compress.c:198:0
	movslq	-316(%rbp), %rcx
	cmpq	$2, %rcx
	jge	.LBB9_18
.Ltmp272:
# BB#19:                                # %while.end
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	.loc	1 201 0                 # compress.c:201:0
	movl	%edx, -316(%rbp)
.Ltmp273:
.LBB9_20:                               # %if.end52
                                        #   in Loop: Header=BB9_7 Depth=1
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	#DEBUG_VALUE: rtmp <- [RBP+-322]
	#DEBUG_VALUE: ryy_j <- [RBP+-336]
	#DEBUG_VALUE: rll_i <- [RBP+-337]
	.loc	1 207 0                 # compress.c:207:0
	movzbl	-303(%rbp), %ecx
	.loc	1 208 0                 # compress.c:208:0
	movzbl	-304(%rbp), %esi
	.loc	1 210 0                 # compress.c:210:0
	movzbl	-321(%rbp), %edi
	.loc	1 207 0                 # compress.c:207:0
	movb	%cl, -322(%rbp)
	.loc	1 208 0                 # compress.c:208:0
	movb	%sil, -303(%rbp)
	.loc	1 209 0                 # compress.c:209:0
	movq	%r8, -336(%rbp)
	.loc	1 210 0                 # compress.c:210:0
	movb	%dil, -337(%rbp)
	jmp	.LBB9_21
	.align	16, 0x90
.LBB9_22:                               # %while.body61
                                        #   in Loop: Header=BB9_21 Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	#DEBUG_VALUE: rtmp <- [RBP+-322]
	#DEBUG_VALUE: ryy_j <- [RBP+-336]
	#DEBUG_VALUE: rll_i <- [RBP+-337]
.Ltmp274:
	#DEBUG_VALUE: rtmp2 <- [RBP+-338]
	.loc	1 213 0                 # compress.c:213:0
	incq	-336(%rbp)
	.loc	1 214 0                 # compress.c:214:0
	movzbl	-322(%rbp), %ecx
	.loc	1 215 0                 # compress.c:215:0
	movq	-336(%rbp), %rsi
	.loc	1 214 0                 # compress.c:214:0
	movb	%cl, -338(%rbp)
	.loc	1 215 0                 # compress.c:215:0
	movzbl	(%rsi), %ecx
	.loc	1 216 0                 # compress.c:216:0
	movzbl	-338(%rbp), %esi
	movq	-336(%rbp), %rdi
	.loc	1 215 0                 # compress.c:215:0
	movb	%cl, -322(%rbp)
	.loc	1 216 0                 # compress.c:216:0
	movb	%sil, (%rdi)
.Ltmp275:
.LBB9_21:                               # %while.cond
                                        #   Parent Loop BB9_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	#DEBUG_VALUE: rtmp <- [RBP+-322]
	#DEBUG_VALUE: ryy_j <- [RBP+-336]
	#DEBUG_VALUE: rll_i <- [RBP+-337]
	.loc	1 211 0                 # compress.c:211:0
	movzbl	-337(%rbp), %ecx
	movzbl	-322(%rbp), %esi
	cmpq	%rsi, %rcx
	jne	.LBB9_22
# BB#23:                                # %while.end62
                                        #   in Loop: Header=BB9_7 Depth=1
.Ltmp276:
	#DEBUG_VALUE: ll_i <- [RBP+-321]
	#DEBUG_VALUE: rtmp <- [RBP+-322]
	#DEBUG_VALUE: ryy_j <- [RBP+-336]
	#DEBUG_VALUE: rll_i <- [RBP+-337]
	.loc	1 218 0                 # compress.c:218:0
	movzbl	-322(%rbp), %ecx
	movb	%cl, -304(%rbp)
	.loc	1 219 0                 # compress.c:219:0
	movq	-336(%rbp), %rcx
	subq	%r9, %rcx
	movl	%ecx, -312(%rbp)
	.loc	1 220 0                 # compress.c:220:0
	movslq	-320(%rbp), %rsi
	movq	-40(%rbp), %rdi
	incq	%rcx
	movw	%cx, (%rdi,%rsi,2)
	movl	-320(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -320(%rbp)
	movl	-312(%rbp), %esi
	incq	%rsi
	movq	$32, %rcx
	movq	-8(%rbp), %rdi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	672(%rdi,%rsi,4), %ecx
	incq	%rcx
	movl	%ecx, 672(%rdi,%rsi,4)
	jmp	.LBB9_24
.Ltmp277:
.LBB9_25:                               # %for.end79
	.loc	1 226 0                 # compress.c:226:0
	movslq	-316(%rbp), %rcx
	testq	%rcx, %rcx
	jle	.LBB9_33
# BB#26:                                # %if.then82
	.loc	1 227 0                 # compress.c:227:0
.Ltmp278:
	movl	-316(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -316(%rbp)
	movq	$1, %r8
	xorq	%rsi, %rsi
	jmp	.LBB9_27
	.align	16, 0x90
.LBB9_31:                               # %if.end106
                                        #   in Loop: Header=BB9_27 Depth=1
	.loc	1 237 0                 # compress.c:237:0
.Ltmp279:
	movl	-316(%rbp), %edi
	leaq	-2(%rdi), %rdx
	andq	%rax, %rdx
	movq	$31, %rcx
	shrq	%cl, %rdx
	leaq	-2(%rdi,%rdx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	shrq	%cl, %rdx
	movl	%edx, -316(%rbp)
.LBB9_27:                               # %while.body85
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 229 0                 # compress.c:229:0
.Ltmp280:
	movzbl	-316(%rbp), %ecx
	testq	$1, %rcx
	je	.LBB9_29
# BB#28:                                # %if.then88
                                        #   in Loop: Header=BB9_27 Depth=1
	.loc	1 230 0                 # compress.c:230:0
.Ltmp281:
	movslq	-320(%rbp), %rcx
	movq	-40(%rbp), %rdi
	movw	%r8w, (%rdi,%rcx,2)
	movl	-320(%rbp), %ecx
	.loc	1 231 0                 # compress.c:231:0
	movq	-8(%rbp), %rdi
	.loc	1 230 0                 # compress.c:230:0
	incq	%rcx
	movl	%ecx, -320(%rbp)
	.loc	1 231 0                 # compress.c:231:0
	movl	676(%rdi), %ecx
	incq	%rcx
	movl	%ecx, 676(%rdi)
	jmp	.LBB9_30
.Ltmp282:
	.align	16, 0x90
.LBB9_29:                               # %if.else95
                                        #   in Loop: Header=BB9_27 Depth=1
	.loc	1 233 0                 # compress.c:233:0
	movslq	-320(%rbp), %rcx
	movq	-40(%rbp), %rdi
	movw	%si, (%rdi,%rcx,2)
	movl	-320(%rbp), %ecx
	.loc	1 234 0                 # compress.c:234:0
	movq	-8(%rbp), %rdi
	.loc	1 233 0                 # compress.c:233:0
	incq	%rcx
	movl	%ecx, -320(%rbp)
	.loc	1 234 0                 # compress.c:234:0
	movl	672(%rdi), %ecx
	incq	%rcx
	movl	%ecx, 672(%rdi)
.Ltmp283:
.LBB9_30:                               # %if.end102
                                        #   in Loop: Header=BB9_27 Depth=1
	.loc	1 236 0                 # compress.c:236:0
	movslq	-316(%rbp), %rcx
	cmpq	$2, %rcx
	jge	.LBB9_31
.Ltmp284:
# BB#32:                                # %while.end109
	xorq	%rax, %rax
	.loc	1 239 0                 # compress.c:239:0
	movl	%eax, -316(%rbp)
.Ltmp285:
.LBB9_33:                               # %if.end110
	.loc	1 242 0                 # compress.c:242:0
	movzbl	-12(%rbp), %eax
	movslq	-320(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movw	%ax, (%rdx,%rcx,2)
	movl	-320(%rbp), %eax
	movslq	-12(%rbp), %rcx
	movq	-8(%rbp), %rdx
	incq	%rax
	movl	%eax, -320(%rbp)
	movl	672(%rdx,%rcx,4), %eax
	incq	%rax
	movl	%eax, 672(%rdx,%rcx,4)
	.loc	1 244 0                 # compress.c:244:0
	movl	-320(%rbp), %eax
	movq	-8(%rbp), %rcx
	movl	%eax, 668(%rcx)
	.loc	1 245 0                 # compress.c:245:0
	addq	$224, %rsp
	popq	%rbp
	retq
.Ltmp286:
.Ltmp287:
	.size	generateMTFValues1, .Ltmp287-generateMTFValues1
.Lfunc_end9:
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"    block %d: crc = 0x%08x, combined CRC = 0x%08x, size = %d\n"
	.size	.L.str, 62

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"    final combined CRC = 0x%08x\n   "
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"      %d in block, %d after MTF & 1-2 coding, %d+2 syms in use\n"
	.size	.L.str2, 64

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"      initial group %d, [%d .. %d], has %d syms (%4.1f%%)\n"
	.size	.L.str3, 59

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"      pass %d: size is %d, grp uses are "
	.size	.L.str4, 41

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%d "
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\n"
	.size	.L.str6, 2

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"      bytes: mapping %d, "
	.size	.L.str7, 26

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"selectors %d, "
	.size	.L.str8, 15

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"code lengths %d, "
	.size	.L.str9, 18

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"codes %d\n"
	.size	.L.str10, 10

	.text
.Ldebug_end0:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"compress.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"BZ2_bsInitWrite"
.Linfo_string4:
	.asciz	"BZ2_compressBlock"
.Linfo_string5:
	.asciz	"bsFinishWrite"
.Linfo_string6:
	.asciz	"sendMTFValues"
.Linfo_string7:
	.asciz	"generateMTFValues"
.Linfo_string8:
	.asciz	"generateMTFValues1"
.Linfo_string9:
	.asciz	"makeMaps_e"
.Linfo_string10:
	.asciz	"bsW"
.Linfo_string11:
	.asciz	"bsPutUInt32"
.Linfo_string12:
	.asciz	"bsPutUChar"
.Linfo_string13:
	.asciz	"s"
.Linfo_string14:
	.asciz	"strm"
.Linfo_string15:
	.asciz	"next_in"
.Linfo_string16:
	.asciz	"char"
.Linfo_string17:
	.asciz	"avail_in"
.Linfo_string18:
	.asciz	"unsigned int"
.Linfo_string19:
	.asciz	"total_in_lo32"
.Linfo_string20:
	.asciz	"total_in_hi32"
.Linfo_string21:
	.asciz	"next_out"
.Linfo_string22:
	.asciz	"avail_out"
.Linfo_string23:
	.asciz	"total_out_lo32"
.Linfo_string24:
	.asciz	"total_out_hi32"
.Linfo_string25:
	.asciz	"state"
.Linfo_string26:
	.asciz	"bzalloc"
.Linfo_string27:
	.asciz	"int"
.Linfo_string28:
	.asciz	"bzfree"
.Linfo_string29:
	.asciz	"opaque"
.Linfo_string30:
	.asciz	"bz_stream"
.Linfo_string31:
	.asciz	"mode"
.Linfo_string32:
	.asciz	"Int32"
.Linfo_string33:
	.asciz	"avail_in_expect"
.Linfo_string34:
	.asciz	"UInt32"
.Linfo_string35:
	.asciz	"arr1"
.Linfo_string36:
	.asciz	"arr2"
.Linfo_string37:
	.asciz	"ftab"
.Linfo_string38:
	.asciz	"origPtr"
.Linfo_string39:
	.asciz	"ptr"
.Linfo_string40:
	.asciz	"block"
.Linfo_string41:
	.asciz	"unsigned char"
.Linfo_string42:
	.asciz	"UChar"
.Linfo_string43:
	.asciz	"mtfv"
.Linfo_string44:
	.asciz	"unsigned short"
.Linfo_string45:
	.asciz	"UInt16"
.Linfo_string46:
	.asciz	"zbits"
.Linfo_string47:
	.asciz	"workFactor"
.Linfo_string48:
	.asciz	"state_in_ch"
.Linfo_string49:
	.asciz	"state_in_len"
.Linfo_string50:
	.asciz	"rNToGo"
.Linfo_string51:
	.asciz	"rTPos"
.Linfo_string52:
	.asciz	"nblock"
.Linfo_string53:
	.asciz	"nblockMAX"
.Linfo_string54:
	.asciz	"numZ"
.Linfo_string55:
	.asciz	"state_out_pos"
.Linfo_string56:
	.asciz	"nInUse"
.Linfo_string57:
	.asciz	"inUse"
.Linfo_string58:
	.asciz	"Bool"
.Linfo_string59:
	.asciz	"unseqToSeq"
.Linfo_string60:
	.asciz	"bsBuff"
.Linfo_string61:
	.asciz	"bsLive"
.Linfo_string62:
	.asciz	"blockCRC"
.Linfo_string63:
	.asciz	"combinedCRC"
.Linfo_string64:
	.asciz	"verbosity"
.Linfo_string65:
	.asciz	"blockNo"
.Linfo_string66:
	.asciz	"blockSize100k"
.Linfo_string67:
	.asciz	"nMTF"
.Linfo_string68:
	.asciz	"mtfFreq"
.Linfo_string69:
	.asciz	"selector"
.Linfo_string70:
	.asciz	"selectorMtf"
.Linfo_string71:
	.asciz	"len"
.Linfo_string72:
	.asciz	"code"
.Linfo_string73:
	.asciz	"rfreq"
.Linfo_string74:
	.asciz	"len_pack"
.Linfo_string75:
	.asciz	"EState"
.Linfo_string76:
	.asciz	"is_last_block"
.Linfo_string77:
	.asciz	"c"
.Linfo_string78:
	.asciz	"u"
.Linfo_string79:
	.asciz	"n"
.Linfo_string80:
	.asciz	"v"
.Linfo_string81:
	.asciz	"EOB"
.Linfo_string82:
	.asciz	"t"
.Linfo_string83:
	.asciz	"i"
.Linfo_string84:
	.asciz	"j"
.Linfo_string85:
	.asciz	"gs"
.Linfo_string86:
	.asciz	"ge"
.Linfo_string87:
	.asciz	"totc"
.Linfo_string88:
	.asciz	"bt"
.Linfo_string89:
	.asciz	"bc"
.Linfo_string90:
	.asciz	"iter"
.Linfo_string91:
	.asciz	"nSelectors"
.Linfo_string92:
	.asciz	"alphaSize"
.Linfo_string93:
	.asciz	"minLen"
.Linfo_string94:
	.asciz	"maxLen"
.Linfo_string95:
	.asciz	"selCtr"
.Linfo_string96:
	.asciz	"nGroups"
.Linfo_string97:
	.asciz	"nBytes"
.Linfo_string98:
	.asciz	"cost"
.Linfo_string99:
	.asciz	"fave"
.Linfo_string100:
	.asciz	"nPart"
.Linfo_string101:
	.asciz	"remF"
.Linfo_string102:
	.asciz	"tFreq"
.Linfo_string103:
	.asciz	"aFreq"
.Linfo_string104:
	.asciz	"cost01"
.Linfo_string105:
	.asciz	"cost23"
.Linfo_string106:
	.asciz	"cost45"
.Linfo_string107:
	.asciz	"icv"
.Linfo_string108:
	.asciz	"pos"
.Linfo_string109:
	.asciz	"ll_i"
.Linfo_string110:
	.asciz	"tmp2"
.Linfo_string111:
	.asciz	"tmp"
.Linfo_string112:
	.asciz	"inUse16"
.Linfo_string113:
	.asciz	"curr"
.Linfo_string114:
	.asciz	"mtfv_i"
.Linfo_string115:
	.asciz	"s_len_sel_selCtr"
.Linfo_string116:
	.asciz	"s_code_sel_selCtr"
.Linfo_string117:
	.asciz	"yy"
.Linfo_string118:
	.asciz	"zPend"
.Linfo_string119:
	.asciz	"wr"
.Linfo_string120:
	.asciz	"rtmp"
.Linfo_string121:
	.asciz	"ryy_j"
.Linfo_string122:
	.asciz	"rll_i"
.Linfo_string123:
	.asciz	"rtmp2"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	2810                    # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0xaf3 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0x29 DW_TAG_subprogram
	.long	.Linfo_string3          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x40:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	4                       # Abbrev [4] 0x4f:0x3a DW_TAG_subprogram
	.long	.Linfo_string4          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	652                     # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	5                       # Abbrev [5] 0x6a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	652                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	5                       # Abbrev [5] 0x79:0xf DW_TAG_formal_parameter
	.long	.Linfo_string76         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	652                     # DW_AT_decl_line
	.long	2647                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	119
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x89:0x29 DW_TAG_subprogram
	.long	.Linfo_string5          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	90                      # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.quad	.Lfunc_end7             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0xa3:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	90                      # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	7                       # Abbrev [7] 0xb2:0x39b DW_TAG_subprogram
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	289                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.quad	.Lfunc_end6             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	5                       # Abbrev [5] 0xcd:0xf DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	289                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	8                       # Abbrev [8] 0xdc:0xf DW_TAG_variable
	.long	.Linfo_string80         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	8                       # Abbrev [8] 0xeb:0xf DW_TAG_variable
	.long	.Linfo_string82         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	8                       # Abbrev [8] 0xfa:0xf DW_TAG_variable
	.long	.Linfo_string83         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	8                       # Abbrev [8] 0x109:0xf DW_TAG_variable
	.long	.Linfo_string84         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	8                       # Abbrev [8] 0x118:0x10 DW_TAG_variable
	.long	.Linfo_string85         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	8                       # Abbrev [8] 0x128:0x10 DW_TAG_variable
	.long	.Linfo_string86         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	8                       # Abbrev [8] 0x138:0x10 DW_TAG_variable
	.long	.Linfo_string87         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	8                       # Abbrev [8] 0x148:0x10 DW_TAG_variable
	.long	.Linfo_string88         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	8                       # Abbrev [8] 0x158:0x10 DW_TAG_variable
	.long	.Linfo_string89         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	8                       # Abbrev [8] 0x168:0x10 DW_TAG_variable
	.long	.Linfo_string90         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	8                       # Abbrev [8] 0x178:0x10 DW_TAG_variable
	.long	.Linfo_string91         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	292                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\244\177"
	.byte	8                       # Abbrev [8] 0x188:0x10 DW_TAG_variable
	.long	.Linfo_string92         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	292                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	8                       # Abbrev [8] 0x198:0x10 DW_TAG_variable
	.long	.Linfo_string93         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	292                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\234\177"
	.byte	8                       # Abbrev [8] 0x1a8:0x10 DW_TAG_variable
	.long	.Linfo_string94         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	292                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230\177"
	.byte	8                       # Abbrev [8] 0x1b8:0x10 DW_TAG_variable
	.long	.Linfo_string95         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	292                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224\177"
	.byte	8                       # Abbrev [8] 0x1c8:0x10 DW_TAG_variable
	.long	.Linfo_string96         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	293                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220\177"
	.byte	8                       # Abbrev [8] 0x1d8:0x10 DW_TAG_variable
	.long	.Linfo_string97         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	293                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214\177"
	.byte	8                       # Abbrev [8] 0x1e8:0x10 DW_TAG_variable
	.long	.Linfo_string98         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	306                     # DW_AT_decl_line
	.long	2760                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200\177"
	.byte	8                       # Abbrev [8] 0x1f8:0x10 DW_TAG_variable
	.long	.Linfo_string99         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	307                     # DW_AT_decl_line
	.long	2772                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\340~"
	.byte	8                       # Abbrev [8] 0x208:0x10 DW_TAG_variable
	.long	.Linfo_string43         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	309                     # DW_AT_decl_line
	.long	2612                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\330~"
	.byte	9                       # Abbrev [9] 0x218:0x52 DW_TAG_lexical_block
	.quad	.Ltmp88                 # DW_AT_low_pc
	.quad	.Ltmp102                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x229:0x10 DW_TAG_variable
	.long	.Linfo_string100        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	331                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\324~"
	.byte	8                       # Abbrev [8] 0x239:0x10 DW_TAG_variable
	.long	.Linfo_string101        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	331                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\320~"
	.byte	8                       # Abbrev [8] 0x249:0x10 DW_TAG_variable
	.long	.Linfo_string102        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	331                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\314~"
	.byte	8                       # Abbrev [8] 0x259:0x10 DW_TAG_variable
	.long	.Linfo_string103        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	331                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310~"
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x26a:0xd4 DW_TAG_lexical_block
	.quad	.Ltmp103                # DW_AT_low_pc
	.quad	.Ltmp145                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x27b:0xc2 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	9                       # Abbrev [9] 0x280:0xbc DW_TAG_lexical_block
	.quad	.Ltmp114                # DW_AT_low_pc
	.quad	.Ltmp139                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x291:0xaa DW_TAG_lexical_block
	.quad	.Ltmp123                # DW_AT_low_pc
	.quad	.Ltmp129                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x2a2:0x52 DW_TAG_lexical_block
	.quad	.Ltmp124                # DW_AT_low_pc
	.quad	.Ltmp125                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x2b3:0x10 DW_TAG_variable
	.long	.Linfo_string104        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	410                     # DW_AT_decl_line
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\304~"
	.byte	8                       # Abbrev [8] 0x2c3:0x10 DW_TAG_variable
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	410                     # DW_AT_decl_line
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300~"
	.byte	8                       # Abbrev [8] 0x2d3:0x10 DW_TAG_variable
	.long	.Linfo_string106        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	410                     # DW_AT_decl_line
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274~"
	.byte	8                       # Abbrev [8] 0x2e3:0x10 DW_TAG_variable
	.long	.Linfo_string107        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	411                     # DW_AT_decl_line
	.long	2617                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\272~"
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x2f4:0x46 DW_TAG_lexical_block
	.quad	.Ltmp125                # DW_AT_low_pc
	.quad	.Ltmp129                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x305:0x34 DW_TAG_lexical_block
	.quad	.Ltmp125                # DW_AT_low_pc
	.quad	.Ltmp129                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x316:0x22 DW_TAG_lexical_block
	.quad	.Ltmp126                # DW_AT_low_pc
	.quad	.Ltmp129                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x327:0x10 DW_TAG_variable
	.long	.Linfo_string107        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	440                     # DW_AT_decl_line
	.long	2617                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x33e:0x52 DW_TAG_lexical_block
	.quad	.Ltmp149                # DW_AT_low_pc
	.quad	.Ltmp159                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x34f:0x10 DW_TAG_variable
	.long	.Linfo_string108        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	513                     # DW_AT_decl_line
	.long	2784                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\262~"
	.byte	8                       # Abbrev [8] 0x35f:0x10 DW_TAG_variable
	.long	.Linfo_string109        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	513                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\261~"
	.byte	8                       # Abbrev [8] 0x36f:0x10 DW_TAG_variable
	.long	.Linfo_string110        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	513                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260~"
	.byte	8                       # Abbrev [8] 0x37f:0x10 DW_TAG_variable
	.long	.Linfo_string111        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	513                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\257~"
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x390:0x22 DW_TAG_lexical_block
	.quad	.Ltmp170                # DW_AT_low_pc
	.quad	.Ltmp189                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x3a1:0x10 DW_TAG_variable
	.long	.Linfo_string112        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	546                     # DW_AT_decl_line
	.long	2796                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220~"
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x3b2:0x34 DW_TAG_lexical_block
	.quad	.Ltmp196                # DW_AT_low_pc
	.quad	.Ltmp205                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x3c3:0x22 DW_TAG_lexical_block
	.quad	.Ltmp197                # DW_AT_low_pc
	.quad	.Ltmp205                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x3d4:0x10 DW_TAG_variable
	.long	.Linfo_string113        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	582                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214~"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	9                       # Abbrev [9] 0x3e6:0x66 DW_TAG_lexical_block
	.quad	.Ltmp207                # DW_AT_low_pc
	.quad	.Ltmp222                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x3f7:0x54 DW_TAG_lexical_block
	.quad	.Ltmp216                # DW_AT_low_pc
	.quad	.Ltmp222                # DW_AT_high_pc
	.byte	9                       # Abbrev [9] 0x408:0x42 DW_TAG_lexical_block
	.quad	.Ltmp217                # DW_AT_low_pc
	.quad	.Ltmp219                # DW_AT_high_pc
	.byte	8                       # Abbrev [8] 0x419:0x10 DW_TAG_variable
	.long	.Linfo_string114        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	606                     # DW_AT_decl_line
	.long	2617                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\212~"
	.byte	8                       # Abbrev [8] 0x429:0x10 DW_TAG_variable
	.long	.Linfo_string115        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	607                     # DW_AT_decl_line
	.long	2589                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\200~"
	.byte	8                       # Abbrev [8] 0x439:0x10 DW_TAG_variable
	.long	.Linfo_string116        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	609                     # DW_AT_decl_line
	.long	2808                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x44d:0x64 DW_TAG_subprogram
	.long	.Linfo_string7          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.quad	.Lfunc_end5             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x467:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	248                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	11                      # Abbrev [11] 0x475:0xe DW_TAG_variable
	.long	.Linfo_string81         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	250                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	8                       # Abbrev [8] 0x483:0xf DW_TAG_variable
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	273                     # DW_AT_decl_line
	.long	2584                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	8                       # Abbrev [8] 0x492:0xf DW_TAG_variable
	.long	.Linfo_string40         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	274                     # DW_AT_decl_line
	.long	2589                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	8                       # Abbrev [8] 0x4a1:0xf DW_TAG_variable
	.long	.Linfo_string43         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.long	2612                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x4b1:0x133 DW_TAG_subprogram
	.long	.Linfo_string8          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.quad	.Lfunc_end9             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x4cb:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	3                       # Abbrev [3] 0x4d9:0xe DW_TAG_formal_parameter
	.long	.Linfo_string81         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	3                       # Abbrev [3] 0x4e7:0xe DW_TAG_formal_parameter
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	2584                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	3                       # Abbrev [3] 0x4f5:0xe DW_TAG_formal_parameter
	.long	.Linfo_string40         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	2589                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	3                       # Abbrev [3] 0x503:0xe DW_TAG_formal_parameter
	.long	.Linfo_string43         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	2612                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	11                      # Abbrev [11] 0x511:0xf DW_TAG_variable
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	166                     # DW_AT_decl_line
	.long	2665                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\320}"
	.byte	11                      # Abbrev [11] 0x520:0xf DW_TAG_variable
	.long	.Linfo_string83         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	167                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\314}"
	.byte	11                      # Abbrev [11] 0x52f:0xf DW_TAG_variable
	.long	.Linfo_string84         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	167                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\310}"
	.byte	11                      # Abbrev [11] 0x53e:0xf DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	168                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\304}"
	.byte	11                      # Abbrev [11] 0x54d:0xf DW_TAG_variable
	.long	.Linfo_string119        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	169                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300}"
	.byte	9                       # Abbrev [9] 0x55c:0x87 DW_TAG_lexical_block
	.quad	.Ltmp257                # DW_AT_low_pc
	.quad	.Ltmp277                # DW_AT_high_pc
	.byte	10                      # Abbrev [10] 0x56d:0x75 DW_TAG_lexical_block
	.long	.Ldebug_ranges4         # DW_AT_ranges
	.byte	11                      # Abbrev [11] 0x572:0xf DW_TAG_variable
	.long	.Linfo_string109        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	178                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\277}"
	.byte	10                      # Abbrev [10] 0x581:0x60 DW_TAG_lexical_block
	.long	.Ldebug_ranges3         # DW_AT_ranges
	.byte	10                      # Abbrev [10] 0x586:0x5a DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	10                      # Abbrev [10] 0x58b:0x54 DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	11                      # Abbrev [11] 0x590:0xf DW_TAG_variable
	.long	.Linfo_string120        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	204                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\276}"
	.byte	11                      # Abbrev [11] 0x59f:0xf DW_TAG_variable
	.long	.Linfo_string121        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	205                     # DW_AT_decl_line
	.long	2589                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260}"
	.byte	11                      # Abbrev [11] 0x5ae:0xf DW_TAG_variable
	.long	.Linfo_string122        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\257}"
	.byte	9                       # Abbrev [9] 0x5bd:0x21 DW_TAG_lexical_block
	.quad	.Ltmp274                # DW_AT_low_pc
	.quad	.Ltmp275                # DW_AT_high_pc
	.byte	11                      # Abbrev [11] 0x5ce:0xf DW_TAG_variable
	.long	.Linfo_string123        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\256}"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x5e4:0x37 DW_TAG_subprogram
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	150                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.quad	.Lfunc_end8             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x5fe:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	150                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	11                      # Abbrev [11] 0x60c:0xe DW_TAG_variable
	.long	.Linfo_string83         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	152                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x61b:0x45 DW_TAG_subprogram
	.long	.Linfo_string10         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	117                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.quad	.Lfunc_end4             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x635:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	117                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	3                       # Abbrev [3] 0x643:0xe DW_TAG_formal_parameter
	.long	.Linfo_string79         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	117                     # DW_AT_decl_line
	.long	2562                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	3                       # Abbrev [3] 0x651:0xe DW_TAG_formal_parameter
	.long	.Linfo_string80         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	117                     # DW_AT_decl_line
	.long	2573                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x660:0x37 DW_TAG_subprogram
	.long	.Linfo_string11         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	127                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.quad	.Lfunc_end3             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x67a:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	127                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	3                       # Abbrev [3] 0x688:0xe DW_TAG_formal_parameter
	.long	.Linfo_string78         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	127                     # DW_AT_decl_line
	.long	2573                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x697:0x37 DW_TAG_subprogram
	.long	.Linfo_string12         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	138                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.quad	.Lfunc_end2             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	3                       # Abbrev [3] 0x6b1:0xe DW_TAG_formal_parameter
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	138                     # DW_AT_decl_line
	.long	1742                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	3                       # Abbrev [3] 0x6bf:0xe DW_TAG_formal_parameter
	.long	.Linfo_string77         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	138                     # DW_AT_decl_line
	.long	2594                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	119
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x6ce:0x5 DW_TAG_pointer_type
	.long	1747                    # DW_AT_type
	.byte	13                      # Abbrev [13] 0x6d3:0xc DW_TAG_typedef
	.long	1759                    # DW_AT_type
	.long	.Linfo_string75         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.short	300                     # DW_AT_decl_line
	.byte	14                      # Abbrev [14] 0x6df:0x22c DW_TAG_structure_type
	.short	55768                   # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	231                     # DW_AT_decl_line
	.byte	15                      # Abbrev [15] 0x6e4:0xd DW_TAG_member
	.long	.Linfo_string14         # DW_AT_name
	.long	2315                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	233                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x6f1:0xd DW_TAG_member
	.long	.Linfo_string31         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	237                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x6fe:0xd DW_TAG_member
	.long	.Linfo_string25         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	238                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x70b:0xd DW_TAG_member
	.long	.Linfo_string33         # DW_AT_name
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	241                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x718:0xd DW_TAG_member
	.long	.Linfo_string35         # DW_AT_name
	.long	2584                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	244                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x725:0xd DW_TAG_member
	.long	.Linfo_string36         # DW_AT_name
	.long	2584                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	245                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x732:0xd DW_TAG_member
	.long	.Linfo_string37         # DW_AT_name
	.long	2584                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	246                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x73f:0xd DW_TAG_member
	.long	.Linfo_string38         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	247                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x74c:0xd DW_TAG_member
	.long	.Linfo_string39         # DW_AT_name
	.long	2584                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	250                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x759:0xd DW_TAG_member
	.long	.Linfo_string40         # DW_AT_name
	.long	2589                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	251                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x766:0xd DW_TAG_member
	.long	.Linfo_string43         # DW_AT_name
	.long	2612                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	252                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x773:0xd DW_TAG_member
	.long	.Linfo_string46         # DW_AT_name
	.long	2589                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	253                     # DW_AT_decl_line
	.byte	80                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x780:0xe DW_TAG_member
	.long	.Linfo_string47         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	256                     # DW_AT_decl_line
	.byte	88                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x78e:0xe DW_TAG_member
	.long	.Linfo_string48         # DW_AT_name
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	259                     # DW_AT_decl_line
	.byte	92                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x79c:0xe DW_TAG_member
	.long	.Linfo_string49         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.byte	96                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7aa:0xe DW_TAG_member
	.long	.Linfo_string50         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	100                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7b8:0xe DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	104                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7c6:0xe DW_TAG_member
	.long	.Linfo_string52         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	264                     # DW_AT_decl_line
	.byte	108                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7d4:0xe DW_TAG_member
	.long	.Linfo_string53         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	265                     # DW_AT_decl_line
	.byte	112                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7e2:0xe DW_TAG_member
	.long	.Linfo_string54         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	266                     # DW_AT_decl_line
	.byte	116                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7f0:0xe DW_TAG_member
	.long	.Linfo_string55         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.byte	120                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x7fe:0xe DW_TAG_member
	.long	.Linfo_string56         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	270                     # DW_AT_decl_line
	.byte	124                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	16                      # Abbrev [16] 0x80c:0xe DW_TAG_member
	.long	.Linfo_string57         # DW_AT_name
	.long	2635                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	271                     # DW_AT_decl_line
	.byte	128                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x81a:0xf DW_TAG_member
	.long	.Linfo_string59         # DW_AT_name
	.long	2665                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	272                     # DW_AT_decl_line
	.short	384                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x829:0xf DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.short	640                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x838:0xf DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
	.short	644                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x847:0xf DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	279                     # DW_AT_decl_line
	.short	648                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x856:0xf DW_TAG_member
	.long	.Linfo_string63         # DW_AT_name
	.long	2573                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	280                     # DW_AT_decl_line
	.short	652                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x865:0xf DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	283                     # DW_AT_decl_line
	.short	656                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x874:0xf DW_TAG_member
	.long	.Linfo_string65         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.short	660                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x883:0xf DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	285                     # DW_AT_decl_line
	.short	664                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x892:0xf DW_TAG_member
	.long	.Linfo_string67         # DW_AT_name
	.long	2562                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	288                     # DW_AT_decl_line
	.short	668                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8a1:0xf DW_TAG_member
	.long	.Linfo_string68         # DW_AT_name
	.long	2677                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	289                     # DW_AT_decl_line
	.short	672                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8b0:0xf DW_TAG_member
	.long	.Linfo_string69         # DW_AT_name
	.long	2690                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	290                     # DW_AT_decl_line
	.short	1704                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8bf:0xf DW_TAG_member
	.long	.Linfo_string70         # DW_AT_name
	.long	2690                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.short	19706                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8ce:0xf DW_TAG_member
	.long	.Linfo_string71         # DW_AT_name
	.long	2703                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	293                     # DW_AT_decl_line
	.short	37708                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8dd:0xf DW_TAG_member
	.long	.Linfo_string72         # DW_AT_name
	.long	2722                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	294                     # DW_AT_decl_line
	.short	39256                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8ec:0xf DW_TAG_member
	.long	.Linfo_string73         # DW_AT_name
	.long	2722                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	295                     # DW_AT_decl_line
	.short	45448                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	17                      # Abbrev [17] 0x8fb:0xf DW_TAG_member
	.long	.Linfo_string74         # DW_AT_name
	.long	2741                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.short	297                     # DW_AT_decl_line
	.short	51640                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x90b:0x5 DW_TAG_pointer_type
	.long	2320                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0x910:0xb DW_TAG_typedef
	.long	2331                    # DW_AT_type
	.long	.Linfo_string30         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
	.byte	19                      # Abbrev [19] 0x91b:0xa1 DW_TAG_structure_type
	.byte	80                      # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	204                     # DW_AT_decl_line
	.byte	15                      # Abbrev [15] 0x91f:0xd DW_TAG_member
	.long	.Linfo_string15         # DW_AT_name
	.long	2492                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	205                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x92c:0xd DW_TAG_member
	.long	.Linfo_string17         # DW_AT_name
	.long	2504                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x939:0xd DW_TAG_member
	.long	.Linfo_string19         # DW_AT_name
	.long	2504                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x946:0xd DW_TAG_member
	.long	.Linfo_string20         # DW_AT_name
	.long	2504                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	208                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x953:0xd DW_TAG_member
	.long	.Linfo_string21         # DW_AT_name
	.long	2492                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	210                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x960:0xd DW_TAG_member
	.long	.Linfo_string22         # DW_AT_name
	.long	2504                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	211                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x96d:0xd DW_TAG_member
	.long	.Linfo_string23         # DW_AT_name
	.long	2504                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x97a:0xd DW_TAG_member
	.long	.Linfo_string24         # DW_AT_name
	.long	2504                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x987:0xd DW_TAG_member
	.long	.Linfo_string25         # DW_AT_name
	.long	2511                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x994:0xd DW_TAG_member
	.long	.Linfo_string26         # DW_AT_name
	.long	2512                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	217                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x9a1:0xd DW_TAG_member
	.long	.Linfo_string28         # DW_AT_name
	.long	2545                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	15                      # Abbrev [15] 0x9ae:0xd DW_TAG_member
	.long	.Linfo_string29         # DW_AT_name
	.long	2511                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	219                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x9bc:0x5 DW_TAG_pointer_type
	.long	2497                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x9c1:0x7 DW_TAG_base_type
	.long	.Linfo_string16         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	20                      # Abbrev [20] 0x9c8:0x7 DW_TAG_base_type
	.long	.Linfo_string18         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	21                      # Abbrev [21] 0x9cf:0x1 DW_TAG_pointer_type
	.byte	12                      # Abbrev [12] 0x9d0:0x5 DW_TAG_pointer_type
	.long	2517                    # DW_AT_type
	.byte	22                      # Abbrev [22] 0x9d5:0x15 DW_TAG_subroutine_type
	.long	2511                    # DW_AT_type
                                        # DW_AT_prototyped
	.byte	23                      # Abbrev [23] 0x9da:0x5 DW_TAG_formal_parameter
	.long	2511                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9df:0x5 DW_TAG_formal_parameter
	.long	2538                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9e4:0x5 DW_TAG_formal_parameter
	.long	2538                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	20                      # Abbrev [20] 0x9ea:0x7 DW_TAG_base_type
	.long	.Linfo_string27         # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	12                      # Abbrev [12] 0x9f1:0x5 DW_TAG_pointer_type
	.long	2550                    # DW_AT_type
	.byte	24                      # Abbrev [24] 0x9f6:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	23                      # Abbrev [23] 0x9f7:0x5 DW_TAG_formal_parameter
	.long	2511                    # DW_AT_type
	.byte	23                      # Abbrev [23] 0x9fc:0x5 DW_TAG_formal_parameter
	.long	2511                    # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0xa02:0xb DW_TAG_typedef
	.long	2538                    # DW_AT_type
	.long	.Linfo_string32         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.byte	18                      # Abbrev [18] 0xa0d:0xb DW_TAG_typedef
	.long	2504                    # DW_AT_type
	.long	.Linfo_string34         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	85                      # DW_AT_decl_line
	.byte	12                      # Abbrev [12] 0xa18:0x5 DW_TAG_pointer_type
	.long	2573                    # DW_AT_type
	.byte	12                      # Abbrev [12] 0xa1d:0x5 DW_TAG_pointer_type
	.long	2594                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0xa22:0xb DW_TAG_typedef
	.long	2605                    # DW_AT_type
	.long	.Linfo_string42         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	20                      # Abbrev [20] 0xa2d:0x7 DW_TAG_base_type
	.long	.Linfo_string41         # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	12                      # Abbrev [12] 0xa34:0x5 DW_TAG_pointer_type
	.long	2617                    # DW_AT_type
	.byte	18                      # Abbrev [18] 0xa39:0xb DW_TAG_typedef
	.long	2628                    # DW_AT_type
	.long	.Linfo_string45         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
	.byte	20                      # Abbrev [20] 0xa44:0x7 DW_TAG_base_type
	.long	.Linfo_string44         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	2                       # DW_AT_byte_size
	.byte	25                      # Abbrev [25] 0xa4b:0xc DW_TAG_array_type
	.long	2647                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xa50:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0xa57:0xb DW_TAG_typedef
	.long	2605                    # DW_AT_type
	.long	.Linfo_string58         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	27                      # Abbrev [27] 0xa62:0x7 DW_TAG_base_type
	.long	.Linfo_string27         # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	25                      # Abbrev [25] 0xa69:0xc DW_TAG_array_type
	.long	2594                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xa6e:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xa75:0xd DW_TAG_array_type
	.long	2562                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0xa7a:0x7 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xa82:0xd DW_TAG_array_type
	.long	2594                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0xa87:0x7 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.short	18001                   # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xa8f:0x13 DW_TAG_array_type
	.long	2594                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xa94:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	28                      # Abbrev [28] 0xa9a:0x7 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xaa2:0x13 DW_TAG_array_type
	.long	2562                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xaa7:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	28                      # Abbrev [28] 0xaad:0x7 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xab5:0x13 DW_TAG_array_type
	.long	2573                    # DW_AT_type
	.byte	28                      # Abbrev [28] 0xaba:0x7 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	26                      # Abbrev [26] 0xac1:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	3                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xac8:0xc DW_TAG_array_type
	.long	2617                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xacd:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xad4:0xc DW_TAG_array_type
	.long	2562                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xad9:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xae0:0xc DW_TAG_array_type
	.long	2594                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xae5:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xaec:0xc DW_TAG_array_type
	.long	2647                    # DW_AT_type
	.byte	26                      # Abbrev [26] 0xaf1:0x6 DW_TAG_subrange_type
	.long	2658                    # DW_AT_type
	.byte	15                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xaf8:0x5 DW_TAG_pointer_type
	.long	2562                    # DW_AT_type
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
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
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
	.byte	5                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
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
	.byte	7                       # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
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
	.byte	8                       # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	10                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	11                      # Abbreviation Code
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
	.byte	12                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	13                      # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	14                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	5                       # DW_FORM_data2
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	15                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	16                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	17                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	56                      # DW_AT_data_member_location
	.byte	5                       # DW_FORM_data2
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	18                      # Abbreviation Code
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
	.byte	19                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	20                      # Abbreviation Code
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
	.byte	21                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	22                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	23                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	24                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	25                      # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	26                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	27                      # Abbreviation Code
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
	.byte	28                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_ranges,"",@progbits
.Lgnu_ranges0:
.Ldebug_ranges0:
	.quad	.Ltmp104
	.quad	.Ltmp105
	.quad	.Ltmp106
	.quad	.Ltmp145
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp258
	.quad	.Ltmp259
	.quad	.Ltmp273
	.quad	.Ltmp277
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp258
	.quad	.Ltmp259
	.quad	.Ltmp265
	.quad	.Ltmp277
	.quad	0
	.quad	0
.Ldebug_ranges3:
	.quad	.Ltmp258
	.quad	.Ltmp259
	.quad	.Ltmp263
	.quad	.Ltmp277
	.quad	0
	.quad	0
.Ldebug_ranges4:
	.quad	.Ltmp258
	.quad	.Ltmp259
	.quad	.Ltmp260
	.quad	.Ltmp277
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
	.asciz	"BZ2_bsInitWrite"       # External Name
	.long	137                     # DIE offset
	.asciz	"bsFinishWrite"         # External Name
	.long	1101                    # DIE offset
	.asciz	"generateMTFValues"     # External Name
	.long	79                      # DIE offset
	.asciz	"BZ2_compressBlock"     # External Name
	.long	1201                    # DIE offset
	.asciz	"generateMTFValues1"    # External Name
	.long	178                     # DIE offset
	.asciz	"sendMTFValues"         # External Name
	.long	1508                    # DIE offset
	.asciz	"makeMaps_e"            # External Name
	.long	1687                    # DIE offset
	.asciz	"bsPutUChar"            # External Name
	.long	1563                    # DIE offset
	.asciz	"bsW"                   # External Name
	.long	1632                    # DIE offset
	.asciz	"bsPutUInt32"           # External Name
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
	.long	2562                    # DIE offset
	.asciz	"Int32"                 # External Name
	.long	2594                    # DIE offset
	.asciz	"UChar"                 # External Name
	.long	2573                    # DIE offset
	.asciz	"UInt32"                # External Name
	.long	1747                    # DIE offset
	.asciz	"EState"                # External Name
	.long	2320                    # DIE offset
	.asciz	"bz_stream"             # External Name
	.long	2504                    # DIE offset
	.asciz	"unsigned int"          # External Name
	.long	2617                    # DIE offset
	.asciz	"UInt16"                # External Name
	.long	2538                    # DIE offset
	.asciz	"int"                   # External Name
	.long	2605                    # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	2628                    # DIE offset
	.asciz	"unsigned short"        # External Name
	.long	2497                    # DIE offset
	.asciz	"char"                  # External Name
	.long	2647                    # DIE offset
	.asciz	"Bool"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
