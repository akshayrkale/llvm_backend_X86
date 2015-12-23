	.file	"recog.c.bc"
	.text
	.globl	init_recog_no_volatile
	.align	16, 0x90
	.type	init_recog_no_volatile,@function
init_recog_no_volatile:                 # @init_recog_no_volatile
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movl	%eax, volatile_ok(%rip)
	retq
.Ltmp0:
	.size	init_recog_no_volatile, .Ltmp0-init_recog_no_volatile
	.cfi_endproc

	.globl	init_recog
	.align	16, 0x90
	.type	init_recog,@function
init_recog:                             # @init_recog
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	movl	%eax, volatile_ok(%rip)
	retq
.Ltmp1:
	.size	init_recog, .Ltmp1-init_recog
	.cfi_endproc

	.globl	recog_memoized_1
	.align	16, 0x90
	.type	recog_memoized_1,@function
recog_memoized_1:                       # @recog_memoized_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 16
.Ltmp5:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB2_2
# BB#1:                                 # %if.then
	movq	32(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	recog
	movl	%eax, 40(%rbx)
.LBB2_2:                                # %if.end
	popq	%rbx
	retq
.Ltmp6:
	.size	recog_memoized_1, .Ltmp6-recog_memoized_1
	.cfi_endproc

	.globl	check_asm_operands
	.align	16, 0x90
	.type	check_asm_operands,@function
check_asm_operands:                     # @check_asm_operands
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
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp13:
	.cfi_offset %rbx, -56
.Ltmp14:
	.cfi_offset %r12, -48
.Ltmp15:
	.cfi_offset %r13, -40
.Ltmp16:
	.cfi_offset %r14, -32
.Ltmp17:
	.cfi_offset %r15, -24
	movl	reload_completed(%rip), %eax
	movq	%rdi, %r12
	testq	%rax, %rax
	je	.LBB3_2
# BB#1:                                 # %if.then
	movq	%r12, %rdi
	callq	make_insn_raw
	movq	%rax, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movl	which_alternative(%rip), %eax
	movq	$31, %rcx
	shrq	%cl, %rax
	xorq	$1, %rax
	jmp	.LBB3_12
.LBB3_2:                                # %if.end
	movq	%r12, %rdi
	callq	asm_noperands
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	js	.LBB3_12
# BB#3:                                 # %if.end6
	movq	$1, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB3_12
# BB#4:                                 # %for.body.lr.ph
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,8), %rax
	andq	$-16, %rax
	movq	%rsp, %r15
	subq	%rax, %r15
	movq	%r15, %rsp
	movq	%rsp, %r13
	subq	%rax, %r13
	movq	%r13, %rsp
	xorq	%rdx, %rdx
	xorq	%r8, %r8
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rcx
	callq	decode_asm_operands
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r13,%rbx,8), %rsi
	movzbl	(%rsi), %eax
	cmpq	$37, %rax
	jne	.LBB3_8
# BB#7:                                 #   in Loop: Header=BB3_6 Depth=1
	incq	%rsi
.LBB3_8:                                # %select.end
                                        #   in Loop: Header=BB3_6 Depth=1
	movzbl	(%rsi), %eax
	movq	$56, %rcx
	movzbl	_sch_istable(%rax,%rax), %edx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	$4, %rdx
	je	.LBB3_11
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_6 Depth=1
	movzbl	1(%rsi), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_11
# BB#10:                                # %if.then34
                                        #   in Loop: Header=BB3_6 Depth=1
	movq	-384(%r13,%rax,8), %rsi
.LBB3_11:                               # %if.end39
                                        #   in Loop: Header=BB3_6 Depth=1
	movq	-8(%r15,%rbx,8), %rdi
	callq	asm_operand_ok
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB3_12
# BB#5:                                 # %for.cond
                                        #   in Loop: Header=BB3_6 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	movq	$1, %rax
	jl	.LBB3_6
.LBB3_12:                               # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp18:
	.size	check_asm_operands, .Ltmp18-check_asm_operands
	.cfi_endproc

	.globl	extract_insn
	.align	16, 0x90
	.type	extract_insn,@function
extract_insn:                           # @extract_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp28:
	.cfi_def_cfa_offset 48
.Ltmp29:
	.cfi_offset %rbx, -40
.Ltmp30:
	.cfi_offset %r12, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	32(%r14), %r15
	movq	$0, recog_data+1000(%rip)
	xorq	%rax, %rax
	movb	%al, recog_data+996(%rip)
	movb	%al, recog_data+998(%rip)
	movb	%al, recog_data+997(%rip)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movl	%r12d, which_alternative(%rip)
	movzbl	(%r15), %eax
	addq	$-39, %rax
	andq	%r12, %rax
	cmpq	$10, %rax
	ja	.LBB4_19
# BB#1:                                 # %entry
	jmpq	*.LJTI4_0(,%rax,8)
.LBB4_3:                                # %sw.bb7
	movq	8(%r15), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$41, %rcx
	je	.LBB4_7
# BB#4:                                 # %sw.bb7
	cmpq	$47, %rcx
	jne	.LBB4_19
# BB#5:                                 # %land.lhs.true
	movq	16(%rax), %rax
	jmp	.LBB4_6
.LBB4_2:                                # %sw.bb1
	movq	16(%r15), %rax
.LBB4_6:                                # %land.lhs.true
	movzbl	(%rax), %eax
	cmpq	$41, %rax
	jne	.LBB4_19
.LBB4_7:                                # %asm_insn
	movq	%r15, %rdi
	callq	asm_noperands
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movb	%bl, recog_data+996(%rip)
	js	.LBB4_17
# BB#8:                                 # %if.then38
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$31, %rax
	jge	.LBB4_9
# BB#11:                                # %if.end
	movabsq	$recog_data, %rsi
	movabsq	$recog_data+240, %rdx
	movabsq	$recog_data+480, %rcx
	movabsq	$recog_data+720, %r8
	movq	%r15, %rdi
	callq	decode_asm_operands
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_34
# BB#12:                                # %if.then45
	movq	recog_data+480(%rip), %rax
	movq	$1, %rcx
	movb	%cl, recog_data+998(%rip)
	movzbl	(%rax), %edi
	testq	%rdi, %rdi
	je	.LBB4_27
# BB#13:                                # %while.body.preheader
	incq	%rax
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	.align	16, 0x90
.LBB4_14:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdi
	cmpq	$44, %rdi
	movq	%rdx, %rdi
	jne	.LBB4_16
# BB#15:                                # %while.body
                                        #   in Loop: Header=BB4_14 Depth=1
	movq	%rsi, %rdi
.LBB4_16:                               # %while.body
                                        #   in Loop: Header=BB4_14 Depth=1
	addq	%rdi, %rcx
	movb	%cl, recog_data+998(%rip)
	movzbl	(%rax), %edi
	incq	%rax
	testq	%rdi, %rdi
	jne	.LBB4_14
	jmp	.LBB4_27
.LBB4_19:                               # %normal_insn
	movslq	40(%r14), %rax
	testq	%rax, %rax
	jns	.LBB4_22
# BB#20:                                # %cond.end
	movq	32(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	recog
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, 40(%r14)
	js	.LBB4_21
.LBB4_22:                               # %if.end64
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,4), %r15
	movsbq	insn_data+32(,%r15,8), %rbx
	movzbl	insn_data+34(,%r15,8), %eax
	movzbl	insn_data+33(,%r15,8), %ecx
	movb	%bl, recog_data+996(%rip)
	movb	%al, recog_data+998(%rip)
	movb	%cl, recog_data+997(%rip)
	movq	%r14, %rdi
	callq	insn_extract
	testq	%rbx, %rbx
	jle	.LBB4_27
# BB#23:                                # %for.body.lr.ph
	movq	insn_data+24(,%r15,8), %rax
	addq	$16, %rax
	xorq	%rcx, %rcx
	movq	%rbx, %rdx
	andq	%r12, %rdx
	.align	16, 0x90
.LBB4_24:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rax), %rsi
	movq	%rsi, recog_data+480(,%rcx,8)
	movzbl	(%rax), %esi
	testq	%rsi, %rsi
	movl	%esi, recog_data+720(,%rcx,4)
	jne	.LBB4_26
# BB#25:                                # %if.then92
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	recog_data(,%rcx,8), %rsi
	movzbl	2(%rsi), %esi
	movl	%esi, recog_data+720(,%rcx,4)
.LBB4_26:                               # %for.inc
                                        #   in Loop: Header=BB4_24 Depth=1
	incq	%rcx
	movq	%rcx, %rsi
	andq	%r12, %rsi
	addq	$24, %rax
	cmpq	%rdx, %rsi
	jne	.LBB4_24
.LBB4_27:                               # %sw.epilog
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB4_34
# BB#28:
	xorq	%rax, %rax
	andq	%r12, %rbx
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	.align	16, 0x90
.LBB4_29:                               # %for.body103
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data+480(,%rax,8), %rsi
	movzbl	(%rsi), %edi
	cmpq	$61, %rdi
	movq	$1, %rsi
	je	.LBB4_33
# BB#30:                                # %cond.false111
                                        #   in Loop: Header=BB4_29 Depth=1
	cmpq	$43, %rdi
	movq	%rcx, %rsi
	jne	.LBB4_32
# BB#31:                                # %cond.false111
                                        #   in Loop: Header=BB4_29 Depth=1
	movq	%rdx, %rsi
.LBB4_32:                               # %cond.false111
                                        #   in Loop: Header=BB4_29 Depth=1
	addq	%rsi, %rsi
.LBB4_33:                               # %cond.end119
                                        #   in Loop: Header=BB4_29 Depth=1
	movl	%esi, recog_data+840(,%rax,4)
	incq	%rax
	movq	%rax, %rsi
	andq	%r12, %rsi
	cmpq	%rbx, %rsi
	jne	.LBB4_29
.LBB4_34:                               # %for.end125
	movsbq	recog_data+998(%rip), %rax
	cmpq	$31, %rax
	jge	.LBB4_35
.LBB4_36:                               # %if.end130
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB4_17:                               # %if.end52
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_insn, %rcx
	movq	$2139, %rdx             # imm = 0x85B
	jmp	.LBB4_18
.LBB4_9:                                # %if.then41
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_insn, %rcx
	movq	$2123, %rsi             # imm = 0x84B
	jmp	.LBB4_10
.LBB4_21:                               # %if.then63
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_insn, %rcx
	movq	$2148, %rdx             # imm = 0x864
.LBB4_18:                               # %if.end52
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn_not_found
.LBB4_35:                               # %if.then129
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_insn, %rcx
	movq	$2172, %rsi             # imm = 0x87C
.LBB4_10:                               # %if.then41
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp33:
	.size	extract_insn, .Ltmp33-extract_insn
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI4_0:
	.quad	.LBB4_3
	.quad	.LBB4_36
	.quad	.LBB4_7
	.quad	.LBB4_19
	.quad	.LBB4_19
	.quad	.LBB4_36
	.quad	.LBB4_36
	.quad	.LBB4_19
	.quad	.LBB4_2
	.quad	.LBB4_36
	.quad	.LBB4_36

	.text
	.globl	constrain_operands
	.align	16, 0x90
	.type	constrain_operands,@function
constrain_operands:                     # @constrain_operands
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp40:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp41:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp42:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp43:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp44:
	.cfi_def_cfa_offset 48
	subq	$816, %rsp              # imm = 0x330
.Ltmp45:
	.cfi_def_cfa_offset 864
.Ltmp46:
	.cfi_offset %rbx, -48
.Ltmp47:
	.cfi_offset %r12, -40
.Ltmp48:
	.cfi_offset %r13, -32
.Ltmp49:
	.cfi_offset %r14, -24
.Ltmp50:
	.cfi_offset %r15, -16
	movl	recog_data+996(%rip), %r12d
	xorq	%rax, %rax
	movl	%eax, which_alternative(%rip)
	movq	$16, %rcx
	movq	%r12, %r14
	shrq	%cl, %r14
	testq	$255, %r12
	movq	$1, %rax
	je	.LBB5_201
# BB#1:                                 # %entry
	testq	$255, %r14
	je	.LBB5_201
# BB#2:                                 # %for.cond.preheader
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_7
# BB#3:                                 # %for.body.lr.ph
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$16777216, %rdx         # imm = 0x1000000
	jg	.LBB5_4
# BB#5:                                 # %for.body.lr.ph
	xorq	%rbx, %rbx
	jmp	.LBB5_6
.LBB5_4:
	decq	%rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
.LBB5_6:                                # %for.body.lr.ph
	leaq	8(,%rbx,8), %rdx
	leaq	576(%rsp), %rdi
	movabsq	$recog_data+480, %rsi
	callq	memcpy
	leaq	4(,%rbx,4), %rdx
	leaq	448(%rsp), %rdi
	movq	$255, %rsi
	callq	memset
.LBB5_7:                                # %for.cond12.preheader
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	xorq	%rsi, %rsi
	xorq	%rdi, %rdi
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB5_8
	.align	16, 0x90
.LBB5_170:                              # %for.cond12.backedge
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%r12, %r14
	movabsq	$4294901760, %rcx       # imm = 0xFFFF0000
	andq	%rcx, %r14
	movq	$16, %rcx
	shrq	%cl, %r14
	movq	%rax, %rdi
.LBB5_8:                                # %for.cond12
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_176 Depth 2
                                        #       Child Loop BB5_179 Depth 3
                                        #     Child Loop BB5_23 Depth 2
                                        #       Child Loop BB5_26 Depth 3
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB5_173
# BB#9:                                 # %for.body16
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	recog_data(,%rdi,8), %r12
	movl	(%r12), %eax
	movq	$16, %rcx
	movq	%rax, %rbx
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	movzbl	rtx_class(%rdx), %edx
	movq	576(%rsp,%rdi,8), %r14
	shrq	%cl, %rbx
	xorq	%rcx, %rcx
	movl	%ecx, 320(%rsp,%rdi,4)
	cmpq	$49, %rdx
	jne	.LBB5_11
# BB#10:                                # %if.then30
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	8(%r12), %r12
	movl	(%r12), %eax
.LBB5_11:                               # %if.end32
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%r9, %r9
	cmpq	$63, %rcx
	jne	.LBB5_12
# BB#13:                                # %if.then37
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	8(%r12), %r10
	movl	(%r10), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB5_14
# BB#15:                                # %land.lhs.true
                                        #   in Loop: Header=BB5_8 Depth=1
	movl	8(%r10), %edi
	cmpq	$52, %rdi
	movabsq	$0, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	ja	.LBB5_17
# BB#16:                                # %if.then52
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r12), %edx
	shrq	%cl, %rax
	andq	$255, %rsi
	andq	$255, %rax
	movq	%rax, %rcx
	movq	%r8, %r15
	callq	subreg_regno_offset
	xorq	%r9, %r9
	movq	%r15, %r8
	movq	8(%r12), %r10
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB5_17
	.align	16, 0x90
.LBB5_173:                              # %for.end853
                                        #   in Loop: Header=BB5_8 Depth=1
	testq	%r8, %rdi
	jne	.LBB5_198
# BB#174:                               # %if.then855
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_195
# BB#175:                               # %if.then855
                                        #   in Loop: Header=BB5_8 Depth=1
	xorq	%r13, %r13
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jle	.LBB5_195
	.align	16, 0x90
.LBB5_176:                              # %for.body864
                                        #   Parent Loop BB5_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_179 Depth 3
	movl	320(%rsp,%r13,4), %eax
	testq	%rax, %rax
	je	.LBB5_193
# BB#177:                               # %land.lhs.true868
                                        #   in Loop: Header=BB5_176 Depth=2
	movq	recog_data(,%r13,8), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB5_193
# BB#178:                               # %for.cond876.preheader
                                        #   in Loop: Header=BB5_176 Depth=2
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	xorq	%r15, %r15
	testq	%rax, %rax
	jle	.LBB5_193
	.align	16, 0x90
.LBB5_179:                              # %for.body880
                                        #   Parent Loop BB5_8 Depth=1
                                        #     Parent Loop BB5_176 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	recog_data(,%r15,8), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	jne	.LBB5_180
# BB#182:                               # %land.lhs.true892
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	%r15, %rax
	andq	%r8, %rax
	movq	%r13, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	je	.LBB5_191
	jmp	.LBB5_183
	.align	16, 0x90
.LBB5_180:                              # %lor.lhs.false887
                                        #   in Loop: Header=BB5_179 Depth=3
	movl	recog_data+840(,%r15,4), %eax
	cmpq	$1, %rax
	je	.LBB5_191
# BB#181:                               # %lor.lhs.false887
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	%r13, %rax
	andq	%r8, %rax
	movq	%r15, %rcx
	andq	%r8, %rcx
	cmpq	%rax, %rcx
	je	.LBB5_191
.LBB5_183:                              # %land.lhs.true895
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	recog_data+480(,%r15,8), %rax
	movzbl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB5_191
# BB#184:                               # %land.lhs.true901
                                        #   in Loop: Header=BB5_179 Depth=3
	movl	448(%rsp,%r15,4), %eax
	movq	%r13, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	movq	%r8, %rbx
	jne	.LBB5_185
# BB#186:                               # %land.lhs.true906
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	recog_data(,%r13,8), %rsi
	callq	operands_match_p
	testq	%rbx, %rax
	movq	%rbx, %r8
	jne	.LBB5_191
# BB#187:                               # %land.lhs.true906.land.lhs.true913_crit_edge
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	%r8, %r14
	movq	recog_data(,%r15,8), %rdi
	jmp	.LBB5_188
.LBB5_185:                              #   in Loop: Header=BB5_179 Depth=3
	movq	%rbx, %r14
.LBB5_188:                              # %land.lhs.true913
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	recog_data(,%r13,8), %rsi
	callq	safe_from_earlyclobber
	movq	%r14, %r8
	testq	%r8, %rax
	movq	$1, %rax
	je	.LBB5_190
# BB#189:                               # %land.lhs.true913
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB5_190:                              # %land.lhs.true913
                                        #   in Loop: Header=BB5_179 Depth=3
	movq	%rax, 64(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB5_191:                              # %for.inc922
                                        #   in Loop: Header=BB5_179 Depth=3
	movl	recog_data+996(%rip), %r12d
	incq	%r15
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_179
# BB#192:                               # %for.cond876.for.inc926.loopexit_crit_edge
                                        #   in Loop: Header=BB5_176 Depth=2
	movq	%r12, %r14
	movabsq	$4294901760, %rax       # imm = 0xFFFF0000
	andq	%rax, %r14
	movq	$16, %rcx
	shrq	%cl, %r14
	.align	16, 0x90
.LBB5_193:                              # %for.inc926
                                        #   in Loop: Header=BB5_176 Depth=2
	incq	%r13
	movq	$56, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB5_176
# BB#194:                               # %if.end929
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%r8, %rax
	je	.LBB5_195
.LBB5_198:                              # %if.end948
                                        #   in Loop: Header=BB5_8 Depth=1
	movl	which_alternative(%rip), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$56, %rcx
	shlq	%cl, %r14
	movq	$56, %rcx
	sarq	%cl, %r14
	xorq	%rsi, %rsi
	cmpq	%r14, %rdx
	movl	%eax, which_alternative(%rip)
	movabsq	$0, %rax
	movabsq	$0, %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	jl	.LBB5_170
	jmp	.LBB5_199
	.align	16, 0x90
.LBB5_12:                               #   in Loop: Header=BB5_8 Depth=1
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%r12, %r10
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	jmp	.LBB5_17
.LBB5_14:                               #   in Loop: Header=BB5_8 Depth=1
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
.LBB5_17:                               # %if.end75
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%r10, 32(%rsp)          # 8-byte Spill
	movzbl	(%r14), %edx
	andq	$255, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	movq	56(%rsp), %r15          # 8-byte Reload
	je	.LBB5_19
# BB#18:                                # %if.end75
                                        #   in Loop: Header=BB5_8 Depth=1
	cmpq	$44, %rdx
	jne	.LBB5_20
.LBB5_19:                               # %if.then83
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	$1, %r9
.LBB5_20:                               # %while.cond.preheader
                                        #   in Loop: Header=BB5_8 Depth=1
	testq	%rdx, %rdx
	jne	.LBB5_22
# BB#21:                                #   in Loop: Header=BB5_8 Depth=1
	movq	%r14, %r12
	jmp	.LBB5_167
	.align	16, 0x90
.LBB5_22:                               # %land.rhs.lr.ph
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	jmp	.LBB5_23
.LBB5_162:                              #   in Loop: Header=BB5_23 Depth=2
	movq	%rbx, %r8
	jmp	.LBB5_166
.LBB5_142:                              # %cond.false755
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$101, %rax
	jne	.LBB5_143
# BB#171:                               # %cond.true832
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	x86_64_sign_extended_value
	jmp	.LBB5_74
.LBB5_143:                              #   in Loop: Header=BB5_23 Depth=2
	movq	%r14, %r8
	jmp	.LBB5_166
	.align	16, 0x90
.LBB5_23:                               # %land.rhs
                                        #   Parent Loop BB5_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB5_26 Depth 3
	leaq	1(%r14), %r12
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$44, %rax
	je	.LBB5_167
# BB#24:                                # %while.body
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	addq	$-33, %rsi
	andq	%r8, %rsi
	cmpq	$82, %rsi
	ja	.LBB5_126
# BB#25:                                # %while.body
                                        #   in Loop: Header=BB5_23 Depth=2
	jmpq	*.LJTI5_0(,%rsi,8)
.LBB5_30:                               # %sw.bb103
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r8, %r13
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$10, %rdx
	movq	%r14, %rdi
	leaq	72(%rsp), %rsi
	callq	strtoul
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	movq	72(%rsp), %r12
	sarq	%cl, %r15
	testq	%rbx, %rbx
	movq	$1, %rax
	js	.LBB5_36
# BB#31:                                # %if.else
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	recog_data(,%r15,8), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	recog_data(,%rcx,8), %rsi
	cmpq	$49, %rax
	jne	.LBB5_33
# BB#32:                                # %if.then120
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%rdi), %rdi
.LBB5_33:                               # %if.end124
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%rsi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$49, %rax
	jne	.LBB5_35
# BB#34:                                # %if.then132
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%rsi), %rsi
.LBB5_35:                               # %if.end136
                                        #   in Loop: Header=BB5_23 Depth=2
	callq	operands_match_p
.LBB5_36:                               # %if.end138
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	%r14d, 448(%rsp,%rcx,4)
	movq	48(%rsp), %rsi          # 8-byte Reload
	movl	%esi, 448(%rsp,%r15,4)
	movq	%r13, %r8
	andq	%r8, %rax
	movq	16(%rsp), %r9           # 8-byte Reload
	movq	32(%rsp), %r10          # 8-byte Reload
	je	.LBB5_38
# BB#37:                                # %if.end138
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %r9
.LBB5_38:                               # %if.end138
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$2, %rax
	movq	56(%rsp), %r15          # 8-byte Reload
	jne	.LBB5_166
# BB#39:                                # %if.end138
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_166
# BB#40:                                # %if.then152
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%esi, 80(%rsp,%rax,8)
	incq	%rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movl	%r14d, 84(%rsp,%rax,8)
	jmp	.LBB5_166
	.align	16, 0x90
.LBB5_126:                              # %sw.default
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$12, %rsi
	cmpq	$114, %rax
	je	.LBB5_156
# BB#127:                               # %cond.false663
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$11, %rsi
	cmpq	$82, %rax
	je	.LBB5_156
# BB#128:                               # %cond.false663
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$113, %rax
	jne	.LBB5_130
# BB#129:                               # %cond.true670
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	orq	$8, %rsi
	jmp	.LBB5_156
.LBB5_90:                               # %sw.bb410
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$54, %rax
	jne	.LBB5_166
# BB#91:                                # %land.lhs.true415
                                        #   in Loop: Header=BB5_23 Depth=2
	addq	$-73, %rdx
	andq	$255, %rdx
	cmpq	$5, %rdx
	ja	.LBB5_166
# BB#92:                                # %land.lhs.true415
                                        #   in Loop: Header=BB5_23 Depth=2
	jmpq	*.LJTI5_1(,%rdx,8)
.LBB5_93:                               # %cond.true418
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	cmpq	$32, %rax
	jb	.LBB5_165
	jmp	.LBB5_166
.LBB5_130:                              # %cond.false672
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$101, %rsi
	jg	.LBB5_144
# BB#131:                               # %cond.false672
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$8, %rsi
	cmpq	$81, %rax
	je	.LBB5_156
	jmp	.LBB5_132
.LBB5_144:                              # %cond.false672
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$117, %rax
	je	.LBB5_150
# BB#145:                               # %cond.false672
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$116, %rax
	jne	.LBB5_146
# BB#149:                               # %cond.true688
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	target_flags(%rip), %eax
	movq	$13, %rsi
	jmp	.LBB5_148
.LBB5_69:                               # %sw.bb314
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %ecx
	cmpq	$55, %rcx
	jne	.LBB5_166
# BB#70:                                # %land.lhs.true319
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r8, %r14
	cmpq	$72, %rax
	jne	.LBB5_71
# BB#76:                                # %cond.true326
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	standard_sse_constant_p
	jmp	.LBB5_74
.LBB5_29:                               # %sw.bb100
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 320(%rsp,%rcx,4)
	jmp	.LBB5_166
.LBB5_59:                               # %sw.bb254
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	jne	.LBB5_166
# BB#60:                                # %land.lhs.true259
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	movl	(%rax), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$98, %rax
	je	.LBB5_165
	jmp	.LBB5_166
.LBB5_61:                               # %sw.bb277
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	jne	.LBB5_166
# BB#62:                                # %land.lhs.true282
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	movl	(%rax), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$99, %rax
	je	.LBB5_165
	jmp	.LBB5_166
.LBB5_63:                               # %sw.bb300
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$55, %rax
	movq	$1, %rax
	je	.LBB5_65
# BB#64:                                # %sw.bb300
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, %rax
.LBB5_65:                               # %sw.bb300
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%rax, %r9
	jmp	.LBB5_166
.LBB5_66:                               # %sw.bb307
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$55, %rax
	movq	$1, %rax
	je	.LBB5_68
# BB#67:                                # %sw.bb307
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, %rax
.LBB5_68:                               # %sw.bb307
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%rax, %r9
	jmp	.LBB5_166
.LBB5_100:                              # %sw.bb507
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB5_166
# BB#101:                               # %land.lhs.true512
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_103
# BB#102:                               # %offsettable_memref_p.exit
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$1, %rdi
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	offsettable_address_p
	movq	32(%rsp), %r10          # 8-byte Reload
	movq	%rbx, %r9
	movq	%r14, %r8
	testq	%r8, %rax
	je	.LBB5_165
.LBB5_103:                              # %lor.lhs.false518
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB5_109
# BB#104:                               # %land.lhs.true521
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %ecx
	cmpq	$133, %rcx
	jg	.LBB5_107
# BB#105:                               # %land.lhs.true521
                                        #   in Loop: Header=BB5_23 Depth=2
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB5_165
# BB#106:                               # %land.lhs.true521
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$28695, %rax            # imm = 0x7017
	jne	.LBB5_109
	jmp	.LBB5_165
.LBB5_44:                               # %sw.bb169
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB5_165
# BB#45:                                # %lor.lhs.false172
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$61, %rax
	jne	.LBB5_165
# BB#46:                                # %lor.lhs.false177
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	reload_in_progress(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_48
# BB#47:                                # %land.lhs.true179
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	8(%r10), %eax
	cmpq	$52, %rax
	ja	.LBB5_165
.LBB5_48:                               # %lor.lhs.false185
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$12, %rsi
	movq	%r10, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	reg_fits_class_p
	jmp	.LBB5_43
.LBB5_81:                               # %while.body.sw.bb350_crit_edge
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %ecx
	jmp	.LBB5_82
.LBB5_49:                               # %sw.bb191
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	je	.LBB5_165
# BB#50:                                # %lor.lhs.false196
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB5_56
# BB#51:                                # %land.lhs.true199
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$133, %rax
	jg	.LBB5_54
# BB#52:                                # %land.lhs.true199
                                        #   in Loop: Header=BB5_23 Depth=2
	leaq	-54(%rax), %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB5_56
# BB#53:                                # %land.lhs.true199
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB5_165
	jmp	.LBB5_56
.LBB5_87:                               # %sw.bb392
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB5_165
# BB#88:                                # %sw.bb392
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$55, %rcx
	jne	.LBB5_166
# BB#89:                                # %land.lhs.true402
                                        #   in Loop: Header=BB5_23 Depth=2
	testq	$16711680, %rax         # imm = 0xFF0000
	je	.LBB5_165
	jmp	.LBB5_166
.LBB5_112:                              # %sw.bb581
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_115
# BB#113:                               # %land.lhs.true584
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB5_115
# BB#114:                               # %land.rhs.i1178
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$1, %rdi
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	offsettable_address_p
	movq	32(%rsp), %r10          # 8-byte Reload
	movq	%rbx, %r9
	movq	%r14, %r8
	testq	%r8, %rax
	jne	.LBB5_165
.LBB5_115:                              # %lor.lhs.false587
                                        #   in Loop: Header=BB5_23 Depth=2
	testq	%r8, %r15
	jne	.LBB5_118
# BB#116:                               # %land.lhs.true590
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB5_118
# BB#117:                               # %land.rhs.i1190
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdi, %rdi
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	offsettable_address_p
	movq	32(%rsp), %r10          # 8-byte Reload
	movq	%rbx, %r9
	movq	%r14, %r8
	testq	%r8, %rax
	jne	.LBB5_165
.LBB5_118:                              # %lor.lhs.false593
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB5_124
# BB#119:                               # %land.lhs.true596
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %ecx
	cmpq	$133, %rcx
	jg	.LBB5_122
# BB#120:                               # %land.lhs.true596
                                        #   in Loop: Header=BB5_23 Depth=2
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB5_124
# BB#121:                               # %land.lhs.true596
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$28695, %rax            # imm = 0x7017
	jne	.LBB5_165
	jmp	.LBB5_124
.LBB5_41:                               # %sw.bb159
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_165
# BB#42:                                # %lor.lhs.false162
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	recog_data+720(,%rax,4), %edi
	movq	%r10, %rsi
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	strict_memory_address_p
.LBB5_43:                               # %lor.lhs.false162
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	32(%rsp), %r10          # 8-byte Reload
	movq	%rbx, %r9
	movq	%r14, %r8
	jmp	.LBB5_164
.LBB5_78:                               # %sw.bb332
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	je	.LBB5_166
# BB#79:                                # %sw.bb332
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$55, %rax
	jne	.LBB5_82
# BB#80:                                # %land.lhs.true342
                                        #   in Loop: Header=BB5_23 Depth=2
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB5_166
.LBB5_82:                               # %sw.bb350
                                        #   in Loop: Header=BB5_23 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB5_85
# BB#83:                                # %sw.bb350
                                        #   in Loop: Header=BB5_23 Depth=2
	addq	$-54, %rcx
	andq	%r8, %rcx
	cmpq	$14, %rcx
	ja	.LBB5_166
# BB#84:                                # %sw.bb350
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB5_165
	jmp	.LBB5_166
	.align	16, 0x90
.LBB5_28:                               # %while.body98
                                        #   in Loop: Header=BB5_26 Depth=3
	incq	%r12
.LBB5_26:                               # %while.cond90
                                        #   Parent Loop BB5_8 Depth=1
                                        #     Parent Loop BB5_23 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB5_166
# BB#27:                                # %while.cond90
                                        #   in Loop: Header=BB5_26 Depth=3
	cmpq	$44, %rax
	jne	.LBB5_28
	jmp	.LBB5_166
.LBB5_150:                              # %cond.true699
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	target_flags(%rip), %eax
	movq	$14, %rsi
	jmp	.LBB5_148
.LBB5_146:                              # %cond.false672
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$102, %rax
	jne	.LBB5_132
# BB#147:                               # %cond.true679
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	target_flags(%rip), %eax
	movq	$15, %rsi
.LBB5_148:                              # %cond.true679
                                        #   in Loop: Header=BB5_23 Depth=2
	testq	$33, %rax
	jne	.LBB5_156
	jmp	.LBB5_166
.LBB5_132:                              # %cond.false707
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %rsi
	cmpq	$97, %rax
	je	.LBB5_156
# BB#133:                               # %cond.false711
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$4, %rsi
	cmpq	$98, %rax
	je	.LBB5_156
# BB#134:                               # %cond.false715
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$3, %rsi
	cmpq	$99, %rax
	je	.LBB5_156
# BB#135:                               # %cond.false719
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB5_151
# BB#136:                               # %cond.false719
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$89, %rax
	jne	.LBB5_137
# BB#155:                               # %cond.end790
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	target_flags(%rip), %esi
	movq	$14, %rcx
	shrq	%cl, %rsi
	andq	$16, %rsi
	je	.LBB5_166
	jmp	.LBB5_156
.LBB5_85:                               # %sw.bb350
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$134, %rcx
	je	.LBB5_165
# BB#86:                                # %sw.bb350
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$140, %rcx
	je	.LBB5_165
	jmp	.LBB5_166
.LBB5_71:                               # %land.lhs.true319
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$71, %rax
	jne	.LBB5_72
# BB#73:                                # %cond.true
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	standard_80387_constant_p
.LBB5_74:                               # %cond.true
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r14, %r8
	testq	%r8, %rax
	jne	.LBB5_75
# BB#77:                                #   in Loop: Header=BB5_23 Depth=2
	movq	16(%rsp), %r9           # 8-byte Reload
	movq	%rbx, %r10
	jmp	.LBB5_166
.LBB5_75:                               # %if.then330
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %r9
	movq	%rbx, %r10
	jmp	.LBB5_166
.LBB5_94:                               # %cond.true432
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	cmpq	$64, %rax
	jb	.LBB5_165
	jmp	.LBB5_166
.LBB5_95:                               # %cond.true447
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	addq	$128, %rax
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB5_165
	jmp	.LBB5_166
.LBB5_96:                               # %cond.true462
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	cmpq	$255, %rax
	je	.LBB5_165
# BB#97:                                # %cond.true462
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$65535, %rax            # imm = 0xFFFF
	je	.LBB5_165
	jmp	.LBB5_166
.LBB5_98:                               # %cond.true477
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	cmpq	$4, %rax
	jb	.LBB5_165
	jmp	.LBB5_166
.LBB5_99:                               # %cond.true492
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	8(%r10), %rax
	cmpq	$255, %rax
	jbe	.LBB5_165
	jmp	.LBB5_166
.LBB5_72:                               #   in Loop: Header=BB5_23 Depth=2
	movq	%r14, %r8
	jmp	.LBB5_166
.LBB5_122:                              # %land.lhs.true596
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$134, %rcx
	je	.LBB5_165
# BB#123:                               # %land.lhs.true596
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$140, %rcx
	je	.LBB5_165
.LBB5_124:                              # %lor.lhs.false641
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	reload_in_progress(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_166
# BB#125:                               # %land.lhs.true643
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	jmp	.LBB5_57
.LBB5_54:                               # %land.lhs.true199
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$134, %rax
	je	.LBB5_165
# BB#55:                                # %land.lhs.true199
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$140, %rax
	je	.LBB5_165
.LBB5_56:                               # %lor.lhs.false239
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	reload_in_progress(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB5_166
.LBB5_57:                               # %lor.lhs.false239
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$61, %rax
	jne	.LBB5_166
# BB#58:                                # %land.lhs.true246
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	8(%r10), %eax
	cmpq	$53, %rax
	jae	.LBB5_165
	jmp	.LBB5_166
.LBB5_151:                              # %cond.false719
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$121, %rax
	jne	.LBB5_152
# BB#154:                               # %cond.true742
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	target_flags+1(%rip), %eax
	movq	$17, %rsi
	testq	$64, %rax
	jne	.LBB5_156
	jmp	.LBB5_166
.LBB5_107:                              # %land.lhs.true521
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$134, %rcx
	je	.LBB5_109
# BB#108:                               # %land.lhs.true521
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$140, %rcx
	jne	.LBB5_165
.LBB5_109:                              # %lor.lhs.false566
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	reload_in_progress(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_166
# BB#110:                               # %land.lhs.true568
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r10), %eax
	cmpq	$61, %rax
	jne	.LBB5_165
# BB#111:                               # %land.lhs.true573
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	8(%r10), %eax
	cmpq	$52, %rax
	jbe	.LBB5_165
	jmp	.LBB5_166
.LBB5_137:                              # %cond.false719
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$2, %rsi
	cmpq	$100, %rax
	je	.LBB5_156
	jmp	.LBB5_138
.LBB5_152:                              # %cond.false719
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$120, %rax
	jne	.LBB5_138
# BB#153:                               # %cond.true726
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	target_flags+2(%rip), %eax
	movq	$16, %rsi
	testq	$5, %rax
	jne	.LBB5_156
	jmp	.LBB5_166
.LBB5_138:                              # %cond.false747
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$7, %rsi
	cmpq	$65, %rax
	je	.LBB5_156
# BB#139:                               # %cond.false751
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$6, %rsi
	cmpq	$68, %rax
	je	.LBB5_156
# BB#140:                               # %cond.false755
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$5, %rsi
	cmpq	$83, %rax
	je	.LBB5_156
# BB#141:                               # %cond.false755
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r8, %r14
	cmpq	$90, %rax
	jne	.LBB5_142
# BB#172:                               # %cond.true838
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	x86_64_zero_extended_value
	jmp	.LBB5_74
	.align	16, 0x90
.LBB5_156:                              # %if.then794
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB5_165
# BB#157:                               # %lor.lhs.false797
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	(%r10), %eax
	testq	%r8, %r15
	jne	.LBB5_161
# BB#158:                               # %land.lhs.true800
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$62, %rcx
	je	.LBB5_165
# BB#159:                               # %land.lhs.true800
                                        #   in Loop: Header=BB5_23 Depth=2
	cmpq	$61, %rcx
	jne	.LBB5_161
# BB#160:                               # %land.lhs.true805
                                        #   in Loop: Header=BB5_23 Depth=2
	movl	8(%r10), %ecx
	cmpq	$52, %rcx
	ja	.LBB5_165
.LBB5_161:                              # %lor.lhs.false819
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r8, %rbx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB5_162
# BB#163:                               # %land.lhs.true824
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	%r10, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%r9, %r14
	movq	%r10, %r13
	callq	reg_fits_class_p
	movq	%r13, %r10
	movq	%r14, %r9
	movq	%rbx, %r8
.LBB5_164:                              # %land.lhs.true824
                                        #   in Loop: Header=BB5_23 Depth=2
	testq	%r8, %rax
	je	.LBB5_166
	.align	16, 0x90
.LBB5_165:                              # %if.then827
                                        #   in Loop: Header=BB5_23 Depth=2
	movq	$1, %r9
.LBB5_166:                              # %while.cond.backedge
                                        #   in Loop: Header=BB5_23 Depth=2
	movzbl	(%r12), %edx
	testq	%rdx, %rdx
	movq	%r12, %r14
	jne	.LBB5_23
.LBB5_167:                              # %while.end845
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%r12, 576(%rsp,%rax,8)
	testq	%r8, %r9
	movq	$1, %rax
	je	.LBB5_169
# BB#168:                               # %while.end845
                                        #   in Loop: Header=BB5_8 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
.LBB5_169:                              # %while.end845
                                        #   in Loop: Header=BB5_8 Depth=1
	movl	recog_data+996(%rip), %r12d
	movq	48(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	jmp	.LBB5_170
.LBB5_195:                              # %while.cond932.preheader
	movq	$32, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	jle	.LBB5_201
# BB#196:                               # %while.body935.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	decq	%rsi
	.align	16, 0x90
.LBB5_197:                              # %while.body935
                                        # =>This Inner Loop Header: Depth=1
	movslq	80(%rsp,%rsi,8), %rcx
	movq	recog_data(,%rcx,8), %rcx
	movslq	84(%rsp,%rsi,8), %rdx
	movq	%rcx, recog_data(,%rdx,8)
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rsi
	testq	%rdx, %rdx
	jg	.LBB5_197
	jmp	.LBB5_201
.LBB5_199:                              # %do.end
	xorq	%rax, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%r8, %rcx
	movl	%r8d, which_alternative(%rip)
	jne	.LBB5_201
# BB#200:                               # %if.then955
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	callq	constrain_operands
.LBB5_201:                              # %cleanup
	addq	$816, %rsp              # imm = 0x330
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp51:
	.size	constrain_operands, .Ltmp51-constrain_operands
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_166
	.quad	.LBB5_126
	.quad	.LBB5_26
	.quad	.LBB5_126
	.quad	.LBB5_166
	.quad	.LBB5_29
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_166
	.quad	.LBB5_166
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_30
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_59
	.quad	.LBB5_166
	.quad	.LBB5_61
	.quad	.LBB5_166
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_63
	.quad	.LBB5_66
	.quad	.LBB5_69
	.quad	.LBB5_69
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_90
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_100
	.quad	.LBB5_126
	.quad	.LBB5_165
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_44
	.quad	.LBB5_126
	.quad	.LBB5_81
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_49
	.quad	.LBB5_87
	.quad	.LBB5_112
	.quad	.LBB5_41
	.quad	.LBB5_126
	.quad	.LBB5_126
	.quad	.LBB5_78
.LJTI5_1:
	.quad	.LBB5_93
	.quad	.LBB5_94
	.quad	.LBB5_95
	.quad	.LBB5_96
	.quad	.LBB5_98
	.quad	.LBB5_99

	.text
	.globl	asm_noperands
	.align	16, 0x90
	.type	asm_noperands,@function
asm_noperands:                          # @asm_noperands
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	cmpq	$39, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	je	.LBB6_6
# BB#1:                                 # %entry
	cmpq	$47, %rcx
	jne	.LBB6_2
# BB#4:                                 # %sw.bb1
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$41, %rdx
	jne	.LBB6_26
# BB#5:                                 # %if.then
	movq	32(%rcx), %rax
	movl	(%rax), %eax
	incq	%rax
	retq
.LBB6_6:                                # %sw.bb13
	movq	8(%rdi), %rdx
	movq	8(%rdx), %r9
	movl	(%r9), %r11d
	movq	%r11, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$41, %rcx
	jne	.LBB6_7
# BB#21:                                # %if.then125
	movslq	(%rdx), %rsi
	decq	%rsi
	.align	16, 0x90
.LBB6_22:                               # %for.cond132
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB6_24
# BB#23:                                # %for.body134
                                        #   in Loop: Header=BB6_22 Depth=1
	movq	8(%rdx,%rsi,8), %rcx
	movzbl	(%rcx), %ecx
	decq	%rsi
	cmpq	$49, %rcx
	je	.LBB6_22
	jmp	.LBB6_26
.LBB6_2:                                # %entry
	cmpq	$41, %rcx
	jne	.LBB6_26
# BB#3:                                 # %sw.bb
	movq	32(%rdi), %rax
	jmp	.LBB6_25
.LBB6_7:                                # %sw.bb13
	cmpq	$47, %rcx
	jne	.LBB6_26
# BB#8:                                 # %land.lhs.true
	movq	16(%r9), %r8
	movzbl	(%r8), %ecx
	cmpq	$41, %rcx
	jne	.LBB6_26
# BB#9:                                 # %if.then32
	movslq	(%rdx), %rcx
	.align	16, 0x90
.LBB6_10:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB6_19
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB6_10 Depth=1
	movq	(%rdx,%rdi,8), %rcx
	movzbl	(%rcx), %esi
	leaq	-1(%rdi), %rcx
	cmpq	$49, %rsi
	je	.LBB6_10
# BB#12:                                # %for.body
	cmpq	$47, %rsi
	jne	.LBB6_26
# BB#13:                                # %for.body61.preheader
	movq	$1, %r10
	jmp	.LBB6_16
.LBB6_24:                               # %for.end148
	movq	32(%r9), %rax
.LBB6_25:                               # %return
	movl	(%rax), %eax
.LBB6_26:                               # %return
	retq
.LBB6_15:                               # %for.cond59.for.body61_crit_edge
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	8(%rdx,%r10,8), %r9
	movl	(%r9), %r11d
	incq	%r10
.LBB6_16:                               # %for.body61
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %r11            # imm = 0xFFFF
	cmpq	$47, %r11
	jne	.LBB6_26
# BB#17:                                # %if.end72
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	16(%r9), %rcx
	movzbl	(%rcx), %esi
	cmpq	$41, %rsi
	jne	.LBB6_26
# BB#18:                                # %if.end80
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	32(%rcx), %r9
	movq	32(%r8), %rcx
	cmpq	%rcx, %r9
	jne	.LBB6_26
# BB#14:                                # %for.cond59
                                        #   in Loop: Header=BB6_16 Depth=1
	movq	$32, %rcx
	movq	%rdi, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r11, %rsi
	jl	.LBB6_15
	jmp	.LBB6_20
.LBB6_19:                               # %for.cond59.preheader.for.end102_crit_edge
	movq	32(%r8), %r9
.LBB6_20:                               # %for.end102
	movl	(%r9), %eax
	addq	%rdi, %rax
	retq
.Ltmp52:
	.size	asm_noperands, .Ltmp52-asm_noperands
	.cfi_endproc

	.globl	decode_asm_operands
	.align	16, 0x90
	.type	decode_asm_operands,@function
decode_asm_operands:                    # @decode_asm_operands
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
.Ltmp64:
	.cfi_offset %rbx, -48
.Ltmp65:
	.cfi_offset %r12, -40
.Ltmp66:
	.cfi_offset %r13, -32
.Ltmp67:
	.cfi_offset %r14, -24
.Ltmp68:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %ebx
	movq	%rcx, %r9
	xorq	%rax, %rax
	cmpq	$39, %rbx
	je	.LBB7_48
# BB#1:                                 # %entry
	cmpq	$41, %rbx
	jne	.LBB7_2
# BB#36:                                # %if.then94
	movq	32(%rdi), %rax
	movslq	(%rax), %r11
	testq	%r11, %r11
	jle	.LBB7_47
# BB#37:                                # %for.body102.lr.ph
	xorq	%rcx, %rcx
	movq	$8, %rbx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %r11
	.align	16, 0x90
.LBB7_38:                               # %for.body102
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rdx
	je	.LBB7_40
# BB#39:                                # %if.then104
                                        #   in Loop: Header=BB7_38 Depth=1
	movq	32(%rdi), %rax
	addq	%rbx, %rax
	movq	%rax, (%rdx,%rcx,8)
.LBB7_40:                               # %if.end113
                                        #   in Loop: Header=BB7_38 Depth=1
	testq	%rsi, %rsi
	je	.LBB7_42
# BB#41:                                # %if.then115
                                        #   in Loop: Header=BB7_38 Depth=1
	movq	32(%rdi), %rax
	movq	8(%rax,%rcx,8), %rax
	movq	%rax, (%rsi,%rcx,8)
.LBB7_42:                               # %if.end124
                                        #   in Loop: Header=BB7_38 Depth=1
	testq	%r9, %r9
	je	.LBB7_44
# BB#43:                                # %if.then126
                                        #   in Loop: Header=BB7_38 Depth=1
	movq	40(%rdi), %rax
	movq	8(%rax,%rcx,8), %rax
	movq	8(%rax), %rax
	movq	%rax, (%r9,%rcx,8)
.LBB7_44:                               # %if.end138
                                        #   in Loop: Header=BB7_38 Depth=1
	testq	%r8, %r8
	je	.LBB7_46
# BB#45:                                # %if.then140
                                        #   in Loop: Header=BB7_38 Depth=1
	movq	40(%rdi), %rax
	movq	8(%rax,%rcx,8), %rax
	movzbl	2(%rax), %eax
	movl	%eax, (%r8,%rcx,4)
.LBB7_46:                               # %for.inc153
                                        #   in Loop: Header=BB7_38 Depth=1
	incq	%rcx
	movq	%rcx, %rax
	andq	%r10, %rax
	addq	$8, %rbx
	cmpq	%r11, %rax
	jne	.LBB7_38
.LBB7_47:                               # %for.end155
	movq	8(%rdi), %rax
	jmp	.LBB7_89
.LBB7_48:                               # %land.lhs.true163
	movq	8(%rdi), %r11
	movq	8(%r11), %r15
	movl	(%r15), %r12d
	movq	%r12, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$47, %rcx
	jne	.LBB7_76
# BB#49:                                # %land.lhs.true172
	movq	16(%r15), %r10
	movzbl	(%r10), %ecx
	cmpq	$41, %rcx
	jne	.LBB7_75
# BB#50:                                # %if.then184
	movq	32(%r10), %rax
	movslq	(%r11), %r14
	movl	(%rax), %eax
	movq	%rax, -8(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	testq	%r14, %r14
	jle	.LBB7_51
# BB#63:                                # %for.body204.lr.ph
	movq	$1, %rax
	jmp	.LBB7_64
	.align	16, 0x90
.LBB7_74:                               # %if.end277.for.body204_crit_edge
                                        #   in Loop: Header=BB7_64 Depth=1
	movq	8(%rdi), %rcx
	movq	8(%rcx,%rax,8), %r15
	movl	(%r15), %r12d
	incq	%rax
.LBB7_64:                               # %for.body204
                                        # =>This Inner Loop Header: Depth=1
	andq	$65535, %r12            # imm = 0xFFFF
	cmpq	$49, %r12
	je	.LBB7_51
# BB#65:                                # %if.end215
                                        #   in Loop: Header=BB7_64 Depth=1
	testq	%rsi, %rsi
	je	.LBB7_67
# BB#66:                                # %if.then217
                                        #   in Loop: Header=BB7_64 Depth=1
	movq	8(%r15), %rcx
	movq	%rcx, -8(%rsi,%rax,8)
.LBB7_67:                               # %if.end229
                                        #   in Loop: Header=BB7_64 Depth=1
	testq	%rdx, %rdx
	je	.LBB7_69
# BB#68:                                # %if.then231
                                        #   in Loop: Header=BB7_64 Depth=1
	movq	8(%rdi), %rcx
	movq	(%rcx,%rax,8), %rcx
	addq	$8, %rcx
	movq	%rcx, -8(%rdx,%rax,8)
.LBB7_69:                               # %if.end243
                                        #   in Loop: Header=BB7_64 Depth=1
	testq	%r9, %r9
	je	.LBB7_71
# BB#70:                                # %if.then245
                                        #   in Loop: Header=BB7_64 Depth=1
	movq	8(%rdi), %rcx
	movq	(%rcx,%rax,8), %rcx
	movq	16(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rcx, -8(%r9,%rax,8)
.LBB7_71:                               # %if.end260
                                        #   in Loop: Header=BB7_64 Depth=1
	testq	%r8, %r8
	je	.LBB7_73
# BB#72:                                # %if.then262
                                        #   in Loop: Header=BB7_64 Depth=1
	movq	8(%rdi), %rcx
	movq	(%rcx,%rax,8), %rcx
	movq	8(%rcx), %rcx
	movzbl	2(%rcx), %ecx
	movl	%ecx, -4(%r8,%rax,4)
.LBB7_73:                               # %if.end277
                                        #   in Loop: Header=BB7_64 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%r14, %rbx
	jl	.LBB7_74
.LBB7_51:                               # %for.cond282.preheader
	movq	$32, %rcx
	movq	-8(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB7_62
# BB#52:                                # %for.body284.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%r8,%r13,4), %r14
	leaq	(%r9,%r13,8), %r15
	leaq	(%rsi,%r13,8), %r12
	leaq	(%rdx,%r13,8), %r13
	xorq	%rcx, %rcx
	movq	$8, %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB7_53:                               # %for.body284
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rdx
	je	.LBB7_55
# BB#54:                                # %if.then286
                                        #   in Loop: Header=BB7_53 Depth=1
	movq	32(%r10), %rax
	addq	%rdi, %rax
	movq	%rax, (%r13,%rcx,8)
.LBB7_55:                               # %if.end296
                                        #   in Loop: Header=BB7_53 Depth=1
	testq	%rsi, %rsi
	je	.LBB7_57
# BB#56:                                # %if.then298
                                        #   in Loop: Header=BB7_53 Depth=1
	movq	32(%r10), %rax
	movq	8(%rax,%rcx,8), %rax
	movq	%rax, (%r12,%rcx,8)
.LBB7_57:                               # %if.end308
                                        #   in Loop: Header=BB7_53 Depth=1
	testq	%r9, %r9
	je	.LBB7_59
# BB#58:                                # %if.then310
                                        #   in Loop: Header=BB7_53 Depth=1
	movq	40(%r10), %rax
	movq	8(%rax,%rcx,8), %rax
	movq	8(%rax), %rax
	movq	%rax, (%r15,%rcx,8)
.LBB7_59:                               # %if.end323
                                        #   in Loop: Header=BB7_53 Depth=1
	testq	%r8, %r8
	je	.LBB7_61
# BB#60:                                # %if.then325
                                        #   in Loop: Header=BB7_53 Depth=1
	movq	40(%r10), %rax
	movq	8(%rax,%rcx,8), %rax
	movzbl	2(%rax), %eax
	movl	%eax, (%r14,%rcx,4)
.LBB7_61:                               # %for.inc339
                                        #   in Loop: Header=BB7_53 Depth=1
	incq	%rcx
	movq	%rcx, %rax
	andq	%rbx, %rax
	movq	-8(%rsp), %r11          # 8-byte Reload
	andq	%rbx, %r11
	addq	$8, %rdi
	cmpq	%r11, %rax
	jne	.LBB7_53
.LBB7_62:                               # %for.end341
	movq	8(%r10), %rax
	jmp	.LBB7_89
.LBB7_2:                                # %entry
	cmpq	$47, %rbx
	jne	.LBB7_89
# BB#3:                                 # %land.lhs.true
	movq	16(%rdi), %r10
	movzbl	(%r10), %ecx
	cmpq	$41, %rcx
	jne	.LBB7_89
# BB#4:                                 # %if.then
	movq	32(%r10), %rax
	movslq	(%rax), %r11
	testq	%r11, %r11
	jle	.LBB7_27
# BB#5:                                 # %for.body.lr.ph
	incq	%r11
	testq	%rdx, %rdx
	je	.LBB7_6
# BB#14:                                # %for.body.lr.ph.for.body.lr.ph.split_crit_edge
	testq	%rsi, %rsi
	je	.LBB7_21
# BB#15:                                # %if.end28.preheader
	xorq	%rbx, %rbx
	movq	$8, %r14
	.align	16, 0x90
.LBB7_16:                               # %if.end28
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r10), %rcx
	addq	%r14, %rcx
	movq	%rcx, 8(%rdx,%rbx,8)
	movq	32(%r10), %rcx
	movq	8(%rcx,%rbx,8), %rcx
	testq	%r9, %r9
	movq	%rcx, 8(%rsi,%rbx,8)
	je	.LBB7_18
# BB#17:                                # %if.then30
                                        #   in Loop: Header=BB7_16 Depth=1
	movq	40(%r10), %rcx
	movq	8(%rcx,%rbx,8), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%r9,%rbx,8)
.LBB7_18:                               # %if.end42
                                        #   in Loop: Header=BB7_16 Depth=1
	testq	%r8, %r8
	je	.LBB7_20
# BB#19:                                # %if.then44
                                        #   in Loop: Header=BB7_16 Depth=1
	movq	40(%r10), %rcx
	movq	8(%rcx,%rbx,8), %rcx
	movzbl	2(%rcx), %ecx
	movl	%ecx, 4(%r8,%rbx,4)
.LBB7_20:                               # %for.inc
                                        #   in Loop: Header=BB7_16 Depth=1
	leaq	2(%rbx), %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	addq	$8, %r14
	cmpq	%rax, %r15
	jl	.LBB7_16
	jmp	.LBB7_27
.LBB7_75:                               # %if.else345
	cmpq	$39, %rbx
	jne	.LBB7_89
.LBB7_76:                               # %land.lhs.true349
	movq	8(%rdi), %rcx
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edi
	cmpq	$41, %rdi
	jne	.LBB7_89
# BB#77:                                # %if.then358
	movq	32(%rcx), %rax
	movslq	(%rax), %r11
	testq	%r11, %r11
	jle	.LBB7_88
# BB#78:                                # %for.body372.lr.ph
	xorq	%rdi, %rdi
	movq	$8, %rbx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %r11
	.align	16, 0x90
.LBB7_79:                               # %for.body372
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdx, %rdx
	je	.LBB7_81
# BB#80:                                # %if.then374
                                        #   in Loop: Header=BB7_79 Depth=1
	movq	32(%rcx), %rax
	addq	%rbx, %rax
	movq	%rax, (%rdx,%rdi,8)
.LBB7_81:                               # %if.end383
                                        #   in Loop: Header=BB7_79 Depth=1
	testq	%rsi, %rsi
	je	.LBB7_83
# BB#82:                                # %if.then385
                                        #   in Loop: Header=BB7_79 Depth=1
	movq	32(%rcx), %rax
	movq	8(%rax,%rdi,8), %rax
	movq	%rax, (%rsi,%rdi,8)
.LBB7_83:                               # %if.end394
                                        #   in Loop: Header=BB7_79 Depth=1
	testq	%r9, %r9
	je	.LBB7_85
# BB#84:                                # %if.then396
                                        #   in Loop: Header=BB7_79 Depth=1
	movq	40(%rcx), %rax
	movq	8(%rax,%rdi,8), %rax
	movq	8(%rax), %rax
	movq	%rax, (%r9,%rdi,8)
.LBB7_85:                               # %if.end408
                                        #   in Loop: Header=BB7_79 Depth=1
	testq	%r8, %r8
	je	.LBB7_87
# BB#86:                                # %if.then410
                                        #   in Loop: Header=BB7_79 Depth=1
	movq	40(%rcx), %rax
	movq	8(%rax,%rdi,8), %rax
	movzbl	2(%rax), %eax
	movl	%eax, (%r8,%rdi,4)
.LBB7_87:                               # %for.inc423
                                        #   in Loop: Header=BB7_79 Depth=1
	incq	%rdi
	movq	%rdi, %rax
	andq	%r10, %rax
	addq	$8, %rbx
	cmpq	%r11, %rax
	jne	.LBB7_79
.LBB7_88:                               # %for.end425
	movq	8(%rcx), %rax
	jmp	.LBB7_89
.LBB7_6:                                # %if.end.us.preheader
	xorq	%r14, %r14
	.align	16, 0x90
.LBB7_7:                                # %if.end.us
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB7_9
# BB#8:                                 # %if.then18.us
                                        #   in Loop: Header=BB7_7 Depth=1
	movq	32(%r10), %rcx
	movq	8(%rcx,%r14,8), %rcx
	movq	%rcx, 8(%rsi,%r14,8)
.LBB7_9:                                # %if.end28.us
                                        #   in Loop: Header=BB7_7 Depth=1
	testq	%r9, %r9
	je	.LBB7_11
# BB#10:                                # %if.then30.us
                                        #   in Loop: Header=BB7_7 Depth=1
	movq	40(%r10), %rcx
	movq	8(%rcx,%r14,8), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%r9,%r14,8)
.LBB7_11:                               # %if.end42.us
                                        #   in Loop: Header=BB7_7 Depth=1
	testq	%r8, %r8
	je	.LBB7_13
# BB#12:                                # %if.then44.us
                                        #   in Loop: Header=BB7_7 Depth=1
	movq	40(%r10), %rcx
	movq	8(%rcx,%r14,8), %rcx
	movzbl	2(%rcx), %ecx
	movl	%ecx, 4(%r8,%r14,4)
.LBB7_13:                               # %for.inc.us
                                        #   in Loop: Header=BB7_7 Depth=1
	leaq	2(%r14), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r14
	cmpq	%rax, %rbx
	jl	.LBB7_7
	jmp	.LBB7_27
.LBB7_21:                               # %if.end28.us589.preheader
	xorq	%r15, %r15
	movq	$8, %r14
	.align	16, 0x90
.LBB7_22:                               # %if.end28.us589
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r10), %rcx
	addq	%r14, %rcx
	testq	%r9, %r9
	movq	%rcx, 8(%rdx,%r15,8)
	je	.LBB7_24
# BB#23:                                # %if.then30.us590
                                        #   in Loop: Header=BB7_22 Depth=1
	movq	40(%r10), %rcx
	movq	8(%rcx,%r15,8), %rcx
	movq	8(%rcx), %rcx
	movq	%rcx, 8(%r9,%r15,8)
.LBB7_24:                               # %if.end42.us598
                                        #   in Loop: Header=BB7_22 Depth=1
	testq	%r8, %r8
	je	.LBB7_26
# BB#25:                                # %if.then44.us599
                                        #   in Loop: Header=BB7_22 Depth=1
	movq	40(%r10), %rcx
	movq	8(%rcx,%r15,8), %rcx
	movzbl	2(%rcx), %ecx
	movl	%ecx, 4(%r8,%r15,4)
.LBB7_26:                               # %for.inc.us608
                                        #   in Loop: Header=BB7_22 Depth=1
	leaq	2(%r15), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	addq	$8, %r14
	cmpq	%rax, %rbx
	jl	.LBB7_22
.LBB7_27:                               # %for.end
	addq	$8, %rdi
	testq	%rsi, %rsi
	je	.LBB7_29
# BB#28:                                # %if.then58
	movq	(%rdi), %rax
	movq	%rax, (%rsi)
.LBB7_29:                               # %if.end63
	testq	%rdx, %rdx
	je	.LBB7_31
# BB#30:                                # %if.then65
	movq	%rdi, (%rdx)
.LBB7_31:                               # %if.end70
	testq	%r9, %r9
	je	.LBB7_33
# BB#32:                                # %if.then72
	movq	16(%r10), %rax
	movq	%rax, (%r9)
.LBB7_33:                               # %if.end77
	testq	%r8, %r8
	je	.LBB7_35
# BB#34:                                # %if.then79
	movq	(%rdi), %rax
	movzbl	2(%rax), %eax
	movl	%eax, (%r8)
.LBB7_35:                               # %if.end87
	movq	8(%r10), %rax
.LBB7_89:                               # %if.end432
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp69:
	.size	decode_asm_operands, .Ltmp69-decode_asm_operands
	.cfi_endproc

	.globl	asm_operand_ok
	.align	16, 0x90
	.type	asm_operand_ok,@function
asm_operand_ok:                         # @asm_operand_ok
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp76:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp77:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp78:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp79:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp80:
	.cfi_def_cfa_offset 48
.Ltmp81:
	.cfi_offset %rbx, -48
.Ltmp82:
	.cfi_offset %r12, -40
.Ltmp83:
	.cfi_offset %r13, -32
.Ltmp84:
	.cfi_offset %r14, -24
.Ltmp85:
	.cfi_offset %r15, -16
	movl	reload_completed(%rip), %eax
	movq	%rdi, %r13
	testq	%rax, %rax
	jne	.LBB8_91
# BB#1:                                 # %while.cond.preheader
	movzbl	(%rsi), %r15d
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB8_90
# BB#2:                                 # %while.body.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB8_3
.LBB8_30:                               # %sw.bb87
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$134, %rax
	je	.LBB8_32
# BB#31:                                # %sw.bb87
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$140, %rax
	jne	.LBB8_71
.LBB8_32:                               # %land.lhs.true127
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB8_89
# BB#33:                                # %lor.lhs.false129
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	-67(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jb	.LBB8_36
# BB#34:                                # %lor.lhs.false129
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$58, %rax
	jne	.LBB8_89
# BB#35:                                # %land.lhs.true144
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r13, %rdi
	callq	symbolic_reference_mentioned_p
	testq	%r14, %rax
	je	.LBB8_89
.LBB8_36:                               # %lor.lhs.false147
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r13, %rdi
	callq	legitimate_pic_address_disp_p
	jmp	.LBB8_20
	.align	16, 0x90
.LBB8_3:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_5 Depth 2
	leaq	1(%rsi), %r12
	movq	$56, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	addq	$-33, %rdx
	andq	%r14, %rdx
	cmpq	$82, %rdx
	ja	.LBB8_54
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$1, %rax
	jmpq	*.LJTI8_0(,%rdx,8)
	.align	16, 0x90
.LBB8_5:                                # %while.cond3
                                        #   Parent Loop BB8_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rsi), %eax
	movzbl	_sch_istable(%rax,%rax), %eax
	incq	%rsi
	testq	$4, %rax
	jne	.LBB8_5
# BB#6:                                 #   in Loop: Header=BB8_3 Depth=1
	movq	%rsi, %r12
	movq	%r14, %rbx
	jmp	.LBB8_71
	.align	16, 0x90
.LBB8_54:                               # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$112, %rax
	jg	.LBB8_65
# BB#55:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$56, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$96, %rax
	jg	.LBB8_61
# BB#56:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	-65(%r15), %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	ja	.LBB8_71
# BB#57:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$458761, %rax           # imm = 0x70009
	jne	.LBB8_70
# BB#58:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$24, %rcx
	jne	.LBB8_59
# BB#69:                                # %cond.end1008
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	target_flags+2(%rip), %eax
	testq	$4, %rax
	je	.LBB8_71
	jmp	.LBB8_70
	.align	16, 0x90
.LBB8_65:                               # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	-113(%r15), %rax
	andq	$255, %rax
	cmpq	$8, %rax
	ja	.LBB8_71
# BB#66:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	jmpq	*.LJTI8_1(,%rax,8)
.LBB8_67:                               # %cond.true941
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	jne	.LBB8_70
	jmp	.LBB8_71
.LBB8_61:                               # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	-97(%r15), %rax
	andq	$255, %rax
	cmpq	$4, %rax
	jb	.LBB8_70
# BB#62:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r15, %rax
	andq	$255, %rax
	cmpq	$101, %rax
	je	.LBB8_87
# BB#63:                                # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$102, %rax
	jne	.LBB8_71
.LBB8_64:                               # %cond.true887
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	target_flags(%rip), %eax
	testq	$33, %rax
	jne	.LBB8_70
	jmp	.LBB8_71
.LBB8_88:                               # %sw.bb28
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$66, %rax
	jne	.LBB8_71
	jmp	.LBB8_89
.LBB8_17:                               # %sw.bb35
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$55, %rax
	jne	.LBB8_71
	jmp	.LBB8_89
.LBB8_9:                                # %sw.bb14
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%r13), %eax
	movl	reload_completed(%rip), %edx
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	testq	%rdx, %rdx
	jne	.LBB8_11
# BB#10:                                # %if.then.i
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$66, %rcx
	je	.LBB8_14
	jmp	.LBB8_71
.LBB8_68:                               # %cond.true959
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	target_flags+1(%rip), %eax
	testq	$64, %rax
	je	.LBB8_71
	.align	16, 0x90
.LBB8_70:                               # %sw.bb1013
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%r13), %esi
	movq	%rsi, %rax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	je	.LBB8_71
# BB#72:                                # %if.end1020
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$16, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	movq	%r13, %rdx
	jne	.LBB8_79
# BB#73:                                # %if.then5.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	reload_completed(%rip), %ecx
	movq	8(%r13), %rdx
	testq	%rcx, %rcx
	jne	.LBB8_76
# BB#74:                                # %land.lhs.true6.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%rdx), %ecx
	cmpq	$66, %rcx
	jne	.LBB8_76
# BB#75:                                # %if.then10.i
                                        #   in Loop: Header=BB8_3 Depth=1
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	general_operand
	jmp	.LBB8_84
.LBB8_76:                               # %if.end11.i
                                        #   in Loop: Header=BB8_3 Depth=1
	andq	$255, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB8_77
# BB#78:                                # %land.lhs.true17.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%rdx), %esi
	movzbl	mode_size(%rax), %eax
	movq	$16, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rax
	ja	.LBB8_85
	jmp	.LBB8_79
.LBB8_77:                               # %if.end11.if.end33_crit_edge.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%rdx), %esi
.LBB8_79:                               # %if.end33.i
                                        #   in Loop: Header=BB8_3 Depth=1
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$70, %rsi
	je	.LBB8_89
# BB#80:                                # %if.end33.i
                                        #   in Loop: Header=BB8_3 Depth=1
	xorq	%rax, %rax
	cmpq	$61, %rsi
	jne	.LBB8_84
# BB#81:                                # %land.rhs.i1145
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	8(%rdx), %ecx
	cmpq	$52, %rcx
	movq	$1, %rax
	ja	.LBB8_84
# BB#82:                                # %lor.rhs.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	regclass_map(,%rcx,4), %eax
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB8_84
# BB#83:                                # %lor.rhs.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movabsq	$1, %rax
.LBB8_84:                               # %register_operand.exit
                                        #   in Loop: Header=BB8_3 Depth=1
	testq	%r14, %rax
	jne	.LBB8_89
.LBB8_85:                               # %if.end1025
                                        #   in Loop: Header=BB8_3 Depth=1
	andq	$255, %r15
	cmpq	$90, %r15
	je	.LBB8_60
# BB#86:                                # %if.end1025
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$101, %r15
	jne	.LBB8_71
.LBB8_87:                               # %cond.true1029
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r13, %rdi
	callq	x86_64_sign_extended_value
	jmp	.LBB8_20
.LBB8_18:                               # %sw.bb42
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$55, %rax
	jne	.LBB8_71
# BB#19:                                # %cond.true
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r13, %rdi
	callq	standard_80387_constant_p
	jmp	.LBB8_20
.LBB8_21:                               # %sw.bb55
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$55, %rax
	jne	.LBB8_71
# BB#22:                                # %cond.true65
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r13, %rdi
	callq	standard_sse_constant_p
	jmp	.LBB8_20
.LBB8_40:                               # %sw.bb170
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB8_71
# BB#41:                                # %cond.true176
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
	cmpq	$32, %rax
	jae	.LBB8_71
	jmp	.LBB8_89
.LBB8_42:                               # %sw.bb254
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB8_71
# BB#43:                                # %cond.true273
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
	cmpq	$64, %rax
	jae	.LBB8_71
	jmp	.LBB8_89
.LBB8_44:                               # %sw.bb340
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB8_71
# BB#45:                                # %cond.true372
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
	addq	$128, %rax
	jmp	.LBB8_53
.LBB8_46:                               # %sw.bb426
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB8_71
# BB#47:                                # %cond.true471
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
	cmpq	$255, %rax
	je	.LBB8_89
# BB#48:                                # %cond.true471
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$65535, %rax            # imm = 0xFFFF
	jne	.LBB8_71
	jmp	.LBB8_89
.LBB8_49:                               # %sw.bb512
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB8_71
# BB#50:                                # %cond.true570
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
	cmpq	$4, %rax
	jae	.LBB8_71
	jmp	.LBB8_89
.LBB8_51:                               # %sw.bb598
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB8_71
# BB#52:                                # %cond.true669
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
.LBB8_53:                               # %cond.true669
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$256, %rax              # imm = 0x100
	jae	.LBB8_71
	jmp	.LBB8_89
.LBB8_26:                               # %while.body.sw.bb87_crit_edge
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%r13), %eax
	jmp	.LBB8_27
.LBB8_37:                               # %sw.bb152
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB8_89
# BB#38:                                # %sw.bb152
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$55, %rcx
	jne	.LBB8_71
# BB#39:                                # %land.lhs.true162
                                        #   in Loop: Header=BB8_3 Depth=1
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB8_71
	jmp	.LBB8_89
.LBB8_15:                               # %sw.bb19
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%r13), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB8_71
# BB#16:                                # %land.rhs.i1132
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdi, %rdi
	callq	offsettable_address_p
	jmp	.LBB8_20
.LBB8_7:                                # %sw.bb10
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r13), %eax
	cmpq	$70, %rax
	je	.LBB8_89
# BB#8:                                 # %address_operand.exit
                                        #   in Loop: Header=BB8_3 Depth=1
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	callq	legitimate_address_p
	jmp	.LBB8_20
.LBB8_23:                               # %sw.bb71
                                        #   in Loop: Header=BB8_3 Depth=1
	movl	(%r13), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB8_71
# BB#24:                                # %sw.bb71
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$55, %rcx
	jne	.LBB8_27
# BB#25:                                # %land.lhs.true80
                                        #   in Loop: Header=BB8_3 Depth=1
	testq	$16711680, %rax         # imm = 0xFF0000
	je	.LBB8_71
.LBB8_27:                               # %sw.bb87
                                        #   in Loop: Header=BB8_3 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB8_30
# BB#28:                                # %sw.bb87
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	-54(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB8_71
# BB#29:                                # %sw.bb87
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB8_32
	jmp	.LBB8_71
.LBB8_11:                               # %if.end.i
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$63, %rcx
	jne	.LBB8_13
# BB#12:                                # %if.then11.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	8(%r13), %rax
	movl	(%rax), %eax
.LBB8_13:                               # %if.end12.i
                                        #   in Loop: Header=BB8_3 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB8_71
.LBB8_14:                               # %land.rhs16.i
                                        #   in Loop: Header=BB8_3 Depth=1
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	general_operand
	jmp	.LBB8_20
.LBB8_59:                               # %sw.default
                                        #   in Loop: Header=BB8_3 Depth=1
	cmpq	$25, %rcx
	jne	.LBB8_71
.LBB8_60:                               # %cond.true1036
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r13, %rdi
	callq	x86_64_zero_extended_value
.LBB8_20:                               # %cond.true
                                        #   in Loop: Header=BB8_3 Depth=1
	testq	%r14, %rax
	jne	.LBB8_89
	.align	16, 0x90
.LBB8_71:                               # %while.cond.backedge
                                        #   in Loop: Header=BB8_3 Depth=1
	movzbl	(%r12), %r15d
	testq	%r15, %r15
	movq	%r12, %rsi
	movq	%rbx, %rax
	jne	.LBB8_3
	jmp	.LBB8_90
.LBB8_89:
	movq	$1, %rax
.LBB8_90:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_91:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.asm_operand_ok, %rcx
	movq	$1655, %rsi             # imm = 0x677
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp86:
	.size	asm_operand_ok, .Ltmp86-asm_operand_ok
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_71
	.quad	.LBB8_54
	.quad	.LBB8_71
	.quad	.LBB8_54
	.quad	.LBB8_71
	.quad	.LBB8_71
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_71
	.quad	.LBB8_71
	.quad	.LBB8_71
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_5
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_88
	.quad	.LBB8_71
	.quad	.LBB8_88
	.quad	.LBB8_71
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_17
	.quad	.LBB8_17
	.quad	.LBB8_18
	.quad	.LBB8_21
	.quad	.LBB8_40
	.quad	.LBB8_42
	.quad	.LBB8_44
	.quad	.LBB8_46
	.quad	.LBB8_49
	.quad	.LBB8_51
	.quad	.LBB8_71
	.quad	.LBB8_71
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_9
	.quad	.LBB8_54
	.quad	.LBB8_90
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_14
	.quad	.LBB8_54
	.quad	.LBB8_26
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_54
	.quad	.LBB8_9
	.quad	.LBB8_37
	.quad	.LBB8_15
	.quad	.LBB8_7
	.quad	.LBB8_54
	.quad	.LBB8_70
	.quad	.LBB8_23
.LJTI8_1:
	.quad	.LBB8_70
	.quad	.LBB8_70
	.quad	.LBB8_71
	.quad	.LBB8_64
	.quad	.LBB8_64
	.quad	.LBB8_71
	.quad	.LBB8_71
	.quad	.LBB8_67
	.quad	.LBB8_68

	.text
	.globl	validate_change
	.align	16, 0x90
	.type	validate_change,@function
validate_change:                        # @validate_change
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp93:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp94:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp95:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp96:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp97:
	.cfi_def_cfa_offset 48
.Ltmp98:
	.cfi_offset %rbx, -48
.Ltmp99:
	.cfi_offset %r12, -40
.Ltmp100:
	.cfi_offset %r13, -32
.Ltmp101:
	.cfi_offset %r14, -24
.Ltmp102:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	(%r12), %r13
	movq	%rcx, %r14
	movq	%rdx, %rbx
	movq	%rdi, %r15
	cmpq	%rbx, %r13
	je	.LBB9_13
# BB#1:                                 # %lor.lhs.false
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rax
	jne	.LBB9_13
# BB#2:                                 # %if.end
	movl	num_changes(%rip), %edx
	testq	%rdi, %r14
	jne	.LBB9_4
# BB#3:                                 # %if.end
	testq	%rdi, %rdx
	jne	.LBB9_14
.LBB9_4:                                # %if.end4
	movq	%rbx, (%r12)
	movslq	changes_allocated(%rip), %rax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jge	.LBB9_6
# BB#5:                                 # %if.end4.if.end12_crit_edge
	movq	%rdi, %rbx
	movq	changes(%rip), %rax
	jmp	.LBB9_9
.LBB9_6:                                # %if.then6
	testq	%rdi, %rax
	movq	%rdi, %rbx
	movq	$150, %rsi
	je	.LBB9_8
# BB#7:                                 # %if.then6
	addq	%rax, %rax
	movq	%rax, %rsi
.LBB9_8:                                # %if.then6
	movl	%esi, changes_allocated(%rip)
	movq	changes(%rip), %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$5, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movl	num_changes(%rip), %edx
	movq	%rax, changes(%rip)
.LBB9_9:                                # %if.end12
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$5, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	%r15, (%rax,%rdi)
	movq	changes(%rip), %rax
	movq	%r12, 16(%rax,%rdi)
	movq	changes(%rip), %rax
	testq	%r15, %r15
	movq	%r13, 24(%rax,%rdi)
	je	.LBB9_12
# BB#10:                                # %land.lhs.true21
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	je	.LBB9_12
# BB#11:                                # %if.then24
	movl	40(%r15), %eax
	movq	changes(%rip), %rdx
	movq	$5, %rcx
	shlq	%cl, %rsi
	movl	%eax, 8(%rdx,%rsi)
	movl	%ebx, 40(%r15)
	movl	num_changes(%rip), %edx
.LBB9_12:                               # %if.end31
	incq	%rdx
	testq	%rbx, %r14
	movl	%edx, num_changes(%rip)
	je	.LBB9_15
.LBB9_13:                               # %return
	movq	$1, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_15:                               # %if.else34
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	apply_change_group  # TAILCALL
.LBB9_14:                               # %if.then3
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.validate_change, %rcx
	movq	$216, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp103:
	.size	validate_change, .Ltmp103-validate_change
	.cfi_endproc

	.globl	apply_change_group
	.align	16, 0x90
	.type	apply_change_group,@function
apply_change_group:                     # @apply_change_group
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp111:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp112:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp113:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp114:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp115:
	.cfi_def_cfa_offset 80
.Ltmp116:
	.cfi_offset %rbx, -48
.Ltmp117:
	.cfi_offset %r12, -40
.Ltmp118:
	.cfi_offset %r13, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
	.cfi_offset %r15, -16
	movslq	num_changes(%rip), %r8
	xorq	%rbx, %rbx
	testq	%r8, %r8
	jle	.LBB10_22
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	jmp	.LBB10_2
.LBB10_14:                              # %if.else48
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	callq	rtvec_alloc
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rdx
	movq	8(%rdx), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rdi
	jle	.LBB10_16
	.align	16, 0x90
.LBB10_15:                              # %for.body62
                                        #   Parent Loop BB10_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r14), %rcx
	movq	(%rcx,%rdi,8), %rcx
	movq	%rcx, (%rax,%rdi,8)
	movq	8(%rdx), %rax
	movslq	(%rax), %r8
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdi
	cmpq	%r8, %rsi
	jl	.LBB10_15
.LBB10_16:                              #   in Loop: Header=BB10_2 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
.LBB10_17:                              # %if.end75
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$1, %rcx
	movq	%r13, %rdi
	callq	validate_change
	jmp	.LBB10_19
	.align	16, 0x90
.LBB10_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_15 Depth 2
	movq	changes(%rip), %rax
	movq	$5, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	(%rax,%rdx), %r13
	testq	%r13, %r13
	je	.LBB10_19
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB10_2 Depth=1
	cmpq	%r15, %r13
	je	.LBB10_19
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB10_2 Depth=1
	movl	(%r13), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB10_7
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%r13), %rsi
	movzbl	(%rsi), %eax
	cmpq	$70, %rax
	jne	.LBB10_20
# BB#6:                                 #   in Loop: Header=BB10_2 Depth=1
	movq	%r13, %r15
	jmp	.LBB10_19
	.align	16, 0x90
.LBB10_7:                               # %if.else
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%r13, %rdi
	callq	insn_invalid_p
	testq	%r12, %rax
	jne	.LBB10_9
# BB#8:                                 #   in Loop: Header=BB10_2 Depth=1
	movq	%r13, %r15
	jmp	.LBB10_19
.LBB10_20:                              # %memory_address_p.exit
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	xorq	%rdx, %rdx
	callq	legitimate_address_p
	testq	%r12, %rax
	movq	%r13, %r15
	jne	.LBB10_19
	jmp	.LBB10_21
.LBB10_9:                               # %if.then13
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	32(%r13), %r14
	movl	(%r14), %r12d
	movq	%r12, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	jne	.LBB10_18
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%r14), %rdx
	movl	(%rdx), %r8d
	leaq	-1(%r8), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	8(%rdx,%rax,8), %rax
	movzbl	(%rax), %eax
	cmpq	$49, %rax
	jne	.LBB10_18
# BB#11:                                # %land.lhs.true30
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	callq	asm_noperands
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB10_18
# BB#12:                                # %if.then36
                                        #   in Loop: Header=BB10_2 Depth=1
	leaq	32(%r13), %rsi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rdx
	cmpq	$2, %rdx
	jne	.LBB10_14
# BB#13:                                # %if.then42
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	8(%r8), %rdx
	jmp	.LBB10_17
.LBB10_18:                              # %if.else80
                                        #   in Loop: Header=BB10_2 Depth=1
	andq	$65534, %r12            # imm = 0xFFFE
	cmpq	$48, %r12
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jne	.LBB10_21
	.align	16, 0x90
.LBB10_19:                              # %for.inc92
                                        #   in Loop: Header=BB10_2 Depth=1
	incq	%rbx
	movslq	num_changes(%rip), %r8
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jl	.LBB10_2
	jmp	.LBB10_22
.LBB10_21:                              # %for.end94thread-pre-split
	movl	num_changes(%rip), %r8d
.LBB10_22:                              # %for.end94
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r8, %rcx
	andq	%rax, %rcx
	andq	%rax, %rbx
	movq	$1, %rax
	cmpq	%rcx, %rbx
	je	.LBB10_28
# BB#23:                                # %if.else97
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	jle	.LBB10_28
# BB#24:                                # %for.body.lr.ph.i
	movq	changes(%rip), %rsi
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	$5, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	decq	%r8
	.align	16, 0x90
.LBB10_25:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	-16(%rsi,%rdi), %rcx
	movq	-8(%rsi,%rdi), %rsi
	movq	%rsi, (%rcx)
	movq	changes(%rip), %rsi
	movq	-32(%rsi,%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB10_27
# BB#26:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB10_25 Depth=1
	movzbl	(%rcx), %ebx
	cmpq	$66, %rbx
	je	.LBB10_27
# BB#29:                                # %if.then.i
                                        #   in Loop: Header=BB10_25 Depth=1
	movl	-24(%rsi,%rdi), %edx
	movl	%edx, 40(%rcx)
.LBB10_27:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB10_25 Depth=1
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$-32, %rdi
	decq	%r8
	testq	%rdx, %rdx
	jg	.LBB10_25
.LBB10_28:                              # %return
	xorq	%rcx, %rcx
	movl	%ecx, num_changes(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	apply_change_group, .Ltmp121-apply_change_group
	.cfi_endproc

	.globl	insn_invalid_p
	.align	16, 0x90
	.type	insn_invalid_p,@function
insn_invalid_p:                         # @insn_invalid_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp128:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp129:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp130:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp131:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp132:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp133:
	.cfi_def_cfa_offset 64
.Ltmp134:
	.cfi_offset %rbx, -48
.Ltmp135:
	.cfi_offset %r12, -40
.Ltmp136:
	.cfi_offset %r13, -32
.Ltmp137:
	.cfi_offset %r14, -24
.Ltmp138:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	32(%r12), %r14
	xorq	%rdx, %rdx
	movl	%edx, 12(%rsp)
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rcx
	je	.LBB11_2
# BB#1:                                 # %entry
	movq	%rsi, %rcx
.LBB11_2:                               # %entry
	movl	reload_completed(%rip), %edi
	testq	%rdi, %rdi
	movq	%rax, %rdi
	je	.LBB11_4
# BB#3:                                 # %entry
	movq	%rsi, %rdi
.LBB11_4:                               # %entry
	movl	reload_in_progress(%rip), %ebx
	orq	%rdi, %rcx
	testq	%rbx, %rbx
	je	.LBB11_6
# BB#5:                                 # %entry
	movq	%rsi, %rax
.LBB11_6:                               # %entry
	orq	%rax, %rcx
	jne	.LBB11_8
# BB#7:                                 # %entry
	leaq	12(%rsp), %rdx
.LBB11_8:                               # %entry
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	recog
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB11_11
# BB#9:                                 # %land.end
	movq	32(%r12), %r13
	movq	%r13, %rdi
	callq	asm_noperands
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rbx
	js	.LBB11_17
# BB#10:                                # %land.lhs.true10
	movq	%r13, %rdi
	callq	check_asm_operands
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB11_17
.LBB11_11:                              # %if.end
	movslq	12(%rsp), %rax
	testq	%rax, %rax
	jle	.LBB11_14
# BB#12:                                # %if.then20
	movq	%r15, %rdi
	callq	added_clobbers_hard_reg_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rbx
	jne	.LBB11_17
# BB#13:                                # %if.end24
	movl	12(%rsp), %edi
	incq	%rdi
	callq	rtvec_alloc
	movq	$39, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %rbx
	movq	8(%rbx), %rax
	movq	%r14, 8(%rax)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	add_clobbers
	movq	%rbx, 32(%r12)
.LBB11_14:                              # %if.end33
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB11_16
# BB#15:                                # %if.then35
	movq	%r12, %rdi
	callq	extract_insn
	movq	$1, %rdi
	movq	$1, %rbx
	callq	constrain_operands
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB11_17
.LBB11_16:                              # %if.end40
	movl	%r15d, 40(%r12)
	xorq	%rbx, %rbx
.LBB11_17:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp139:
	.size	insn_invalid_p, .Ltmp139-insn_invalid_p
	.cfi_endproc

	.globl	memory_address_p
	.align	16, 0x90
	.type	memory_address_p,@function
memory_address_p:                       # @memory_address_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp141:
	.cfi_def_cfa_offset 16
	movzbl	(%rsi), %eax
	cmpq	$70, %rax
	movq	$1, %rax
	je	.LBB12_4
# BB#1:                                 # %do.body
	xorq	%rdx, %rdx
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_2
# BB#3:                                 # %do.body
	movabsq	$1, %rax
.LBB12_4:                               # %return
	popq	%rdx
	retq
.LBB12_2:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp142:
	.size	memory_address_p, .Ltmp142-memory_address_p
	.cfi_endproc

	.globl	cancel_changes
	.align	16, 0x90
	.type	cancel_changes,@function
cancel_changes:                         # @cancel_changes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rdx
	movslq	num_changes(%rip), %r9
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %r9
	jle	.LBB13_5
# BB#1:                                 # %for.body.lr.ph
	movq	changes(%rip), %r8
	movq	$5, %rcx
	movq	%r9, %rsi
	shlq	%cl, %rsi
	decq	%r9
	.align	16, 0x90
.LBB13_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-16(%r8,%rsi), %rcx
	movq	-8(%r8,%rsi), %rdx
	movq	%rdx, (%rcx)
	movq	changes(%rip), %r8
	movq	-32(%r8,%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB13_4
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB13_2 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$66, %rdx
	je	.LBB13_4
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB13_2 Depth=1
	movl	-24(%r8,%rsi), %eax
	movl	%eax, 40(%rcx)
.LBB13_4:                               # %for.cond.backedge
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$-32, %rsi
	decq	%r9
	cmpq	%rax, %rdx
	jg	.LBB13_2
.LBB13_5:                               # %for.end
	movl	%edi, num_changes(%rip)
	retq
.Ltmp143:
	.size	cancel_changes, .Ltmp143-cancel_changes
	.cfi_endproc

	.globl	num_validated_changes
	.align	16, 0x90
	.type	num_validated_changes,@function
num_validated_changes:                  # @num_validated_changes
	.cfi_startproc
# BB#0:                                 # %entry
	movl	num_changes(%rip), %eax
	retq
.Ltmp144:
	.size	num_validated_changes, .Ltmp144-num_validated_changes
	.cfi_endproc

	.globl	validate_replace_rtx_subexp
	.align	16, 0x90
	.type	validate_replace_rtx_subexp,@function
validate_replace_rtx_subexp:            # @validate_replace_rtx_subexp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp146:
	.cfi_def_cfa_offset 16
	movq	%rdx, %rax
	movq	%rsi, %rdx
	movq	%rdi, %rsi
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	validate_replace_rtx_1
	popq	%rax
	jmp	apply_change_group  # TAILCALL
.Ltmp147:
	.size	validate_replace_rtx_subexp, .Ltmp147-validate_replace_rtx_subexp
	.cfi_endproc

	.align	16, 0x90
	.type	validate_replace_rtx_1,@function
validate_replace_rtx_1:                 # @validate_replace_rtx_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp154:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp155:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp156:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp157:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp158:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp159:
	.cfi_def_cfa_offset 112
.Ltmp160:
	.cfi_offset %rbx, -48
.Ltmp161:
	.cfi_offset %r12, -40
.Ltmp162:
	.cfi_offset %r13, -32
.Ltmp163:
	.cfi_offset %r14, -24
.Ltmp164:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	(%r13), %r8
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	testq	%r8, %r8
	je	.LBB16_36
# BB#1:                                 # %if.end
	movl	(%r8), %eax
	movq	%rax, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	movq	rtx_format(,%rbx,8), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movzbl	(%rcx), %ecx
	xorq	%rdi, %rdi
	cmpq	$101, %rcx
	jne	.LBB16_3
# BB#2:                                 # %if.then3
	movq	8(%r8), %rcx
	movzbl	2(%rcx), %edi
.LBB16_3:                               # %if.end7
	cmpq	%r14, %r8
	je	.LBB16_10
# BB#4:                                 # %lor.lhs.false
	movl	(%r14), %ecx
	cmpq	$61, %rbx
	jne	.LBB16_8
# BB#5:                                 # %lor.lhs.false
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB16_8
# BB#6:                                 # %land.lhs.true18
	movq	%rcx, %rdx
	xorq	%rax, %rdx
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB16_8
# BB#7:                                 # %land.lhs.true27
	movl	8(%r8), %edx
	movl	8(%r14), %esi
	cmpq	%rsi, %rdx
	je	.LBB16_10
.LBB16_8:                               # %lor.lhs.false35
	xorq	%rax, %rcx
	testq	$16777215, %rcx         # imm = 0xFFFFFF
	jne	.LBB16_11
# BB#9:                                 # %land.lhs.true51
	movq	%rdi, %r12
	movq	%r8, %rdi
	movq	%r14, %rsi
	movq	%r8, %r15
	callq	rtx_equal_p
	movq	%r12, %rdi
	movq	%r15, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_11
.LBB16_10:                              # %if.then53
	movq	$1, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
.LBB16_59:                              # %if.end241
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	validate_change  # TAILCALL
.LBB16_11:                              # %if.end55
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%rbx), %edx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB16_15
	.align	16, 0x90
.LBB16_12:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_26 Depth 2
	movq	%rdx, %r13
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	leaq	-1(%r13), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB16_13
# BB#24:                                # %if.then76
                                        #   in Loop: Header=BB16_12 Depth=1
	movq	(%r8,%r13,8), %rdi
	movslq	(%rdi), %rbx
	testq	%rbx, %rbx
	jle	.LBB16_14
# BB#25:                                # %for.body84.lr.ph
                                        #   in Loop: Header=BB16_12 Depth=1
	movq	%r8, %r12
	movq	$3, %rcx
	leaq	-1(%rbx), %r15
	shlq	%cl, %rbx
	jmp	.LBB16_26
	.align	16, 0x90
.LBB16_27:                              # %for.body84.for.body84_crit_edge
                                        #   in Loop: Header=BB16_26 Depth=2
	movq	(%r8,%r13,8), %rdi
	movq	%r8, %r12
	decq	%r15
	addq	$-8, %rbx
.LBB16_26:                              # %for.body84
                                        #   Parent Loop BB16_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%rbx, %rdi
	movq	%r14, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	validate_replace_rtx_1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r12, %r8
	jg	.LBB16_27
	jmp	.LBB16_14
	.align	16, 0x90
.LBB16_13:                              # %for.body
                                        #   in Loop: Header=BB16_12 Depth=1
	cmpq	$101, %rax
	jne	.LBB16_14
# BB#23:                                # %if.then66
                                        #   in Loop: Header=BB16_12 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	8(%r8,%rax,8), %rdi
	movq	%r14, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	callq	validate_replace_rtx_1
	movq	%rbx, %r8
.LBB16_14:                              # %for.cond.backedge
                                        #   in Loop: Header=BB16_12 Depth=1
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB16_12
.LBB16_15:                              # %for.end95
	movl	num_changes(%rip), %eax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movl	num_changes(%rip), %ecx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	movq	24(%rsp), %r15          # 8-byte Reload
	je	.LBB16_36
# BB#16:                                # %if.end99
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$101, %rax
	jne	.LBB16_19
# BB#17:                                # %land.lhs.true104
	movq	8(%r8), %rax
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	je	.LBB16_19
# BB#18:                                # %land.lhs.true104
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB16_19:                              # %land.lhs.true104
	movq	16(%rsp), %r13          # 8-byte Reload
	movzbl	rtx_class(%r13), %ebx
	cmpq	$60, %rbx
	je	.LBB16_21
# BB#20:                                # %if.end120
	cmpq	$99, %rbx
	jne	.LBB16_31
.LBB16_21:                              # %land.lhs.true132
	movq	8(%r8), %rdi
	movq	16(%r8), %rsi
	movq	%r8, %r12
	callq	swap_commutative_operands_p
	testq	%r14, %rax
	je	.LBB16_22
# BB#28:                                # %if.then141
	cmpq	$99, %rbx
	je	.LBB16_30
# BB#29:                                # %cond.false
	movq	%r13, %rdi
	callq	swap_condition
	movq	%rax, %r13
.LBB16_30:                              # %cond.end
	movq	8(%r12), %rcx
	movq	16(%r12), %rdx
	movzbl	2(%r12), %esi
	movq	%r13, %rdi
	callq	gen_rtx_fmt_ee
	movq	$1, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	validate_change
	movq	(%r15), %r8
	movzbl	(%r8), %r13d
	jmp	.LBB16_31
.LBB16_22:
	movq	%r12, %r8
.LBB16_31:                              # %if.end161
	cmpq	$74, %r13
	jg	.LBB16_37
# BB#32:                                # %if.end161
	cmpq	$63, %r13
	jne	.LBB16_36
# BB#33:                                # %sw.bb244
	movq	8(%r8), %rsi
	movl	16(%r8), %ecx
	movzbl	2(%r8), %edi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r8, %rbx
	callq	simplify_subreg
	movq	%rbx, %rcx
	testq	%rax, %rax
	jne	.LBB16_58
# BB#34:                                # %land.lhs.true256
	movq	8(%rcx), %rax
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	jne	.LBB16_36
# BB#35:                                # %if.end270
	movq	const_int_rtx+512(%rip), %rdx
	movzbl	2(%rcx), %esi
	movq	$49, %rdi
	callq	gen_rtx_fmt_e
	testq	%rax, %rax
	jne	.LBB16_58
	jmp	.LBB16_36
.LBB16_37:                              # %if.end161
	cmpq	$119, %r13
	jg	.LBB16_42
# BB#38:                                # %if.end161
	cmpq	$75, %r13
	movq	%r8, %rsi
	jne	.LBB16_39
# BB#52:                                # %sw.bb
	movq	16(%rsi), %rcx
	movzbl	(%rcx), %eax
	cmpq	$54, %rax
	jne	.LBB16_36
# BB#53:                                # %if.then169
	movq	8(%rsi), %rdx
	movzbl	2(%rsi), %esi
	movq	$75, %rdi
	callq	simplify_gen_binary
	jmp	.LBB16_58
.LBB16_42:                              # %if.end161
	leaq	-120(%r13), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	movq	%r8, %rbx
	jae	.LBB16_43
# BB#55:                                # %sw.bb218
	movq	%r13, %rdi
	movq	8(%rbx), %rdx
	movzbl	2(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB16_36
# BB#56:                                # %if.then227
	movzbl	2(%rbx), %esi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	simplify_gen_unary
	testq	%rax, %rax
	jne	.LBB16_58
# BB#57:                                # %if.then236
	movq	const_int_rtx+512(%rip), %rdx
	movzbl	2(%rbx), %esi
	movq	$49, %rdi
	callq	gen_rtx_fmt_e
	jmp	.LBB16_58
.LBB16_39:                              # %if.end161
	cmpq	$76, %r13
	jne	.LBB16_36
# BB#40:                                # %sw.bb182
	movq	16(%rsi), %rdx
	movl	(%rdx), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$54, %rax
	jne	.LBB16_36
# BB#41:                                # %if.then198
	movq	8(%rsi), %r14
	movzbl	2(%rsi), %ebx
	movq	$77, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %rcx
	callq	simplify_gen_unary
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	simplify_gen_binary
.LBB16_58:                              # %if.end241
	movq	$1, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rax, %rdx
	jmp	.LBB16_59
.LBB16_43:                              # %if.end161
	addq	$-132, %r13
	andq	%r14, %r13
	cmpq	$2, %r13
	jae	.LBB16_36
# BB#44:                                # %sw.bb275
	movq	8(%rbx), %rcx
	movl	(%rcx), %r13d
	movq	%r13, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB16_36
# BB#45:                                # %land.lhs.true283
	movq	16(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB16_36
# BB#46:                                # %land.lhs.true291
	movq	24(%rbx), %rax
	movzbl	(%rax), %edx
	cmpq	$54, %rdx
	jne	.LBB16_36
# BB#47:                                # %land.lhs.true299
	testq	$134217728, %r13        # imm = 0x8000000
	jne	.LBB16_36
# BB#48:                                # %land.lhs.true299
	movq	8(%rcx), %rcx
	movl	(%rcx), %ecx
	andq	$65534, %rcx            # imm = 0xFFFE
	cmpq	$98, %rcx
	je	.LBB16_36
# BB#49:                                # %if.then316
	leaq	24(%rbx), %r15
	movq	$16, %rcx
	shrq	%cl, %r13
	movzbl	(%rbx), %ecx
	movq	8(%rax), %r12
	andq	$255, %r13
	leaq	8(%rbx), %rbx
	cmpq	$132, %rcx
	jne	.LBB16_50
# BB#60:                                # %if.then344
	movq	$1, %rdi
	jmp	.LBB16_61
.LBB16_50:                              # %if.then316
	cmpq	$133, %rcx
	jne	.LBB16_36
# BB#51:                                # %if.then333
	movq	$2, %rdi
.LBB16_61:                              # %if.then344
	movq	$1, %rsi
	callq	mode_for_extraction
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	andq	%r14, %rax
	cmpq	$59, %rax
	je	.LBB16_36
# BB#62:                                # %if.end352
	testq	%r14, %rsi
	je	.LBB16_36
# BB#63:                                # %land.lhs.true355
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	movzbl	mode_size(%r13), %edx
	cmpq	%rdx, %rcx
	jae	.LBB16_36
# BB#64:                                # %if.then364
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$60, %rcx
	movq	%rdx, %r9
	shrq	%cl, %r9
	andq	$7, %r9
	addq	%r12, %r9
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movq	%rdx, %rax
	cqto
	idivq	%rcx
	movq	%rdx, %r14
	movq	$32, %rcx
	movq	(%rbx), %rax
	shlq	%cl, %r9
	movq	$35, %rcx
	sarq	%cl, %r9
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rax, %rdi
	movq	%r9, %rdx
	callq	adjust_address_1
	movq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_CONST_INT
	movq	$1, %rcx
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	validate_change
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	jmp	.LBB16_59
.LBB16_36:                              # %sw.epilog
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp165:
	.size	validate_replace_rtx_1, .Ltmp165-validate_replace_rtx_1
	.cfi_endproc

	.globl	validate_replace_rtx
	.align	16, 0x90
	.type	validate_replace_rtx,@function
validate_replace_rtx:                   # @validate_replace_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp167:
	.cfi_def_cfa_offset 16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rdx
	leaq	32(%rax), %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	validate_replace_rtx_1
	popq	%rax
	jmp	apply_change_group  # TAILCALL
.Ltmp168:
	.size	validate_replace_rtx, .Ltmp168-validate_replace_rtx
	.cfi_endproc

	.globl	validate_replace_rtx_group
	.align	16, 0x90
	.type	validate_replace_rtx_group,@function
validate_replace_rtx_group:             # @validate_replace_rtx_group
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdx, %rax
	leaq	32(%rax), %rcx
	movq	%rsi, %rdx
	movq	%rdi, %rsi
	movq	%rcx, %rdi
	movq	%rax, %rcx
	jmp	validate_replace_rtx_1  # TAILCALL
.Ltmp169:
	.size	validate_replace_rtx_group, .Ltmp169-validate_replace_rtx_group
	.cfi_endproc

	.globl	validate_replace_src
	.align	16, 0x90
	.type	validate_replace_src,@function
validate_replace_src:                   # @validate_replace_src
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp171:
	.cfi_def_cfa_offset 32
	movq	%rdi, (%rsp)
	movq	%rsi, 8(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	32(%rdx), %rdi
	movabsq	$validate_replace_src_1, %rsi
	leaq	(%rsp), %rdx
	callq	note_uses
	callq	apply_change_group
	addq	$24, %rsp
	retq
.Ltmp172:
	.size	validate_replace_src, .Ltmp172-validate_replace_src
	.cfi_endproc

	.align	16, 0x90
	.type	validate_replace_src_1,@function
validate_replace_src_1:                 # @validate_replace_src_1
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rsi), %rax
	movq	8(%rsi), %rdx
	movq	16(%rsi), %rcx
	movq	%rax, %rsi
	jmp	validate_replace_rtx_1  # TAILCALL
.Ltmp173:
	.size	validate_replace_src_1, .Ltmp173-validate_replace_src_1
	.cfi_endproc

	.globl	find_single_use
	.align	16, 0x90
	.type	find_single_use,@function
find_single_use:                        # @find_single_use
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp180:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp181:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp182:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp183:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp184:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp185:
	.cfi_def_cfa_offset 64
.Ltmp186:
	.cfi_offset %rbx, -48
.Ltmp187:
	.cfi_offset %r12, -40
.Ltmp188:
	.cfi_offset %r13, -32
.Ltmp189:
	.cfi_offset %r14, -24
.Ltmp190:
	.cfi_offset %r15, -16
	movl	reload_completed(%rip), %eax
	movl	reload_in_progress(%rip), %ecx
	movq	%rsi, %r13
	movq	%rdi, %r12
	orq	%rax, %rcx
	xorq	%r15, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rcx
	jne	.LBB21_13
# BB#1:                                 # %lor.lhs.false2
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB21_13
# BB#2:                                 # %if.end
	movq	%r13, %rdi
	jmp	.LBB21_3
	.align	16, 0x90
.LBB21_14:                              # %for.inc39
                                        #   in Loop: Header=BB21_3 Depth=1
	movq	%rbx, %rdi
.LBB21_3:                               # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_9 Depth 2
	callq	next_nonnote_insn
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB21_13
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB21_3 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB21_13
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB21_3 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB21_14
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB21_3 Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	dead_or_set_p
	testq	%r14, %rax
	je	.LBB21_14
# BB#7:                                 # %if.then13
                                        #   in Loop: Header=BB21_3 Depth=1
	movq	48(%rbx), %rax
	jmp	.LBB21_9
	.align	16, 0x90
.LBB21_8:                               # %for.cond15
                                        #   in Loop: Header=BB21_9 Depth=2
	movq	16(%rax), %rax
.LBB21_9:                               # %for.cond15
                                        #   Parent Loop BB21_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB21_14
# BB#10:                                # %for.body17
                                        #   in Loop: Header=BB21_9 Depth=2
	movq	8(%rax), %rcx
	cmpq	%r13, %rcx
	jne	.LBB21_8
# BB#11:                                # %if.then29
	leaq	32(%rbx), %rsi
	movq	%r12, %rdi
	callq	find_single_use_1
	movq	%rax, %r15
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%rax, %rax
	je	.LBB21_13
# BB#12:                                # %if.then35
	movq	%rbx, (%rax)
.LBB21_13:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp191:
	.size	find_single_use, .Ltmp191-find_single_use
	.cfi_endproc

	.align	16, 0x90
	.type	find_single_use_1,@function
find_single_use_1:                      # @find_single_use_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp198:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp199:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp200:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp201:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp202:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp203:
	.cfi_def_cfa_offset 80
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
	movl	target_flags(%rip), %r11d
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$23, %rcx
	shrq	%cl, %r11
	andq	$4, %r11
	leaq	4(%r11), %r8
	orq	$3, %r11
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	xorq	%rsi, %rsi
	jmp	.LBB22_1
	.align	16, 0x90
.LBB22_12:                              # %sw.bb70
                                        #   in Loop: Header=BB22_1 Depth=1
	addq	$8, %rbx
	movq	%rbx, %r15
.LBB22_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rbx
	movzbl	(%rbx), %edi
	leaq	-47(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$21, %rcx
	ja	.LBB22_14
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB22_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3148676, %rax          # imm = 0x300B84
	jne	.LBB22_41
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB22_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$589824, %rax           # imm = 0x90000
	jne	.LBB22_12
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB22_1 Depth=1
	testq	%rcx, %rcx
	jne	.LBB22_14
# BB#5:                                 # %sw.bb1
                                        #   in Loop: Header=BB22_1 Depth=1
	movq	8(%rbx), %rax
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	$47, %rdi
	cmpq	$68, %rcx
	jg	.LBB22_13
# BB#6:                                 # %sw.bb1
                                        #   in Loop: Header=BB22_1 Depth=1
	cmpq	$59, %rcx
	je	.LBB22_11
# BB#7:                                 # %sw.bb1
                                        #   in Loop: Header=BB22_1 Depth=1
	cmpq	$61, %rcx
	je	.LBB22_11
# BB#8:                                 # %sw.bb1
                                        #   in Loop: Header=BB22_1 Depth=1
	cmpq	$63, %rcx
	jne	.LBB22_14
# BB#9:                                 # %land.lhs.true24
                                        #   in Loop: Header=BB22_1 Depth=1
	movq	8(%rax), %rax
	movl	(%rax), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB22_14
# BB#10:                                # %land.lhs.true34
                                        #   in Loop: Header=BB22_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	addq	%r11, %rax
	cqto
	idivq	%r8
	movq	%rax, %r10
	addq	%r11, %rcx
	movq	%rcx, %rax
	cqto
	idivq	%r8
	andq	%r9, %r10
	andq	%r9, %rax
	cmpq	%rax, %r10
	je	.LBB22_11
	jmp	.LBB22_14
.LBB22_13:                              # %sw.bb1
                                        #   in Loop: Header=BB22_1 Depth=1
	cmpq	$69, %rcx
	jne	.LBB22_14
.LBB22_11:                              # %if.end
                                        #   in Loop: Header=BB22_1 Depth=1
	addq	$16, %rbx
	movq	%rbx, %r15
	jmp	.LBB22_1
.LBB22_14:                              # %sw.epilog
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%rdi), %r13d
	testq	%r13, %r13
	je	.LBB22_41
# BB#15:                                # %for.body.lr.ph.lr.ph
	movq	rtx_format(,%rdi,8), %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
.LBB22_17:                              # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB22_18 Depth 2
                                        #     Child Loop BB22_32 Depth 2
                                        #       Child Loop BB22_33 Depth 3
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r13,8), %rbx
	decq	%r13
	.align	16, 0x90
.LBB22_18:                              # %for.body
                                        #   Parent Loop BB22_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx,%r13), %eax
	cmpq	$101, %rax
	jne	.LBB22_19
# BB#22:                                # %if.then88
                                        #   in Loop: Header=BB22_18 Depth=2
	movq	(%rbx), %rcx
	cmpq	%r14, %rcx
	movq	%r15, %rax
	je	.LBB22_27
# BB#23:                                # %lor.lhs.false
                                        #   in Loop: Header=BB22_18 Depth=2
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB22_26
# BB#24:                                # %land.lhs.true99
                                        #   in Loop: Header=BB22_18 Depth=2
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB22_26
# BB#25:                                # %land.lhs.true108
                                        #   in Loop: Header=BB22_18 Depth=2
	movl	8(%r14), %eax
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	movq	%r15, %rax
	je	.LBB22_27
	.align	16, 0x90
.LBB22_26:                              # %if.else
                                        #   in Loop: Header=BB22_18 Depth=2
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %r12
	movq	%r14, %r15
	movq	%rdx, %r14
	callq	find_single_use_1
	movq	%r14, %rdx
	movq	%r15, %r14
	movq	%r12, %r15
	movq	24(%rsp), %r12          # 8-byte Reload
.LBB22_27:                              # %if.end126
                                        #   in Loop: Header=BB22_18 Depth=2
	testq	%r12, %r12
	je	.LBB22_28
# BB#29:                                # %if.else130
                                        #   in Loop: Header=BB22_18 Depth=2
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jne	.LBB22_41
.LBB22_20:                              # %for.cond.backedge
                                        #   in Loop: Header=BB22_18 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$-8, %rbx
	decq	%r13
	testq	%rax, %rax
	jg	.LBB22_18
	jmp	.LBB22_21
.LBB22_19:                              # %for.body
                                        #   in Loop: Header=BB22_18 Depth=2
	cmpq	$69, %rax
	jne	.LBB22_20
# BB#30:                                # %if.then141
                                        #   in Loop: Header=BB22_17 Depth=1
	movq	(%rbx), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movl	(%rax), %edi
	jmp	.LBB22_32
.LBB22_28:                              #   in Loop: Header=BB22_17 Depth=1
	movq	%rax, %r12
.LBB22_16:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB22_17 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r12, %rsi
	jg	.LBB22_17
	jmp	.LBB22_41
.LBB22_31:                              # %for.cond146.outer.loopexit
                                        #   in Loop: Header=BB22_32 Depth=2
	decq	%rdi
	movq	%rax, %r12
	movq	(%rsp), %rax            # 8-byte Reload
.LBB22_32:                              # %for.cond146.outer
                                        #   Parent Loop BB22_17 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB22_33 Depth 3
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	(%rax,%rdi,8), %rbx
	.align	16, 0x90
.LBB22_33:                              # %for.cond146
                                        #   Parent Loop BB22_17 Depth=1
                                        #     Parent Loop BB22_32 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB22_16
# BB#34:                                # %for.body149
                                        #   in Loop: Header=BB22_33 Depth=3
	movq	(%rbx), %rcx
	cmpq	%r14, %rcx
	movq	%r15, %rax
	je	.LBB22_39
# BB#35:                                # %lor.lhs.false158
                                        #   in Loop: Header=BB22_33 Depth=3
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB22_38
# BB#36:                                # %land.lhs.true163
                                        #   in Loop: Header=BB22_33 Depth=3
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB22_38
# BB#37:                                # %land.lhs.true175
                                        #   in Loop: Header=BB22_33 Depth=3
	movl	8(%rcx), %eax
	movl	8(%r14), %ecx
	cmpq	%rcx, %rax
	movq	%r15, %rax
	je	.LBB22_39
.LBB22_38:                              # %if.else192
                                        #   in Loop: Header=BB22_33 Depth=3
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	find_single_use_1
	movq	24(%rsp), %rdi          # 8-byte Reload
.LBB22_39:                              # %if.end201
                                        #   in Loop: Header=BB22_33 Depth=3
	testq	%r12, %r12
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB22_31
# BB#40:                                # %if.else205
                                        #   in Loop: Header=BB22_33 Depth=3
	decq	%rdi
	addq	$-8, %rbx
	xorq	%rsi, %rsi
	testq	%rax, %rax
	je	.LBB22_33
	jmp	.LBB22_41
.LBB22_21:
	movq	%r12, %rsi
.LBB22_41:                              # %return
	movq	%rsi, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp209:
	.size	find_single_use_1, .Ltmp209-find_single_use_1
	.cfi_endproc

	.globl	general_operand
	.align	16, 0x90
	.type	general_operand,@function
general_operand:                        # @general_operand
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
	movq	%rdi, %r14
	movl	(%r14), %edi
	movq	%rsi, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rbx
	jne	.LBB23_2
# BB#1:                                 # %if.then
	movq	$16, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	andq	$255, %rbx
.LBB23_2:                               # %if.end
	testq	$16711680, %rdi         # imm = 0xFF0000
	jne	.LBB23_5
# BB#3:                                 # %if.end
	testq	%r12, %rbx
	je	.LBB23_5
# BB#4:                                 # %land.lhs.true8
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	xorq	%r15, %r15
	orq	$2, %rax
	andq	%r12, %rax
	cmpq	$3, %rax
	jne	.LBB23_47
.LBB23_5:                               # %if.end15
	movq	%rdi, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$54, %r13
	jne	.LBB23_8
# BB#6:                                 # %land.lhs.true19
	movq	8(%r14), %rdi
	movq	%rbx, %rsi
	callq	trunc_int_for_mode
	movq	8(%r14), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jne	.LBB23_47
# BB#7:                                 # %land.lhs.true19.if.end26_crit_edge
	movl	(%r14), %edi
.LBB23_8:                               # %if.end26
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB23_14
# BB#9:                                 # %if.end26
	leaq	-54(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_11
# BB#10:                                # %if.end26
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB23_16
	jmp	.LBB23_11
.LBB23_14:                              # %if.end26
	cmpq	$134, %rax
	je	.LBB23_16
# BB#15:                                # %if.end26
	cmpq	$140, %rax
	jne	.LBB23_11
.LBB23_16:                              # %if.then57
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	je	.LBB23_19
# BB#17:                                # %lor.lhs.false62
	movq	%rbx, %rcx
	andq	%r12, %rcx
	andq	%r12, %rdi
	cmpq	%rcx, %rdi
	je	.LBB23_19
# BB#18:                                # %lor.lhs.false62
	xorq	%r15, %r15
	testq	%r12, %rbx
	jne	.LBB23_47
.LBB23_19:                              # %land.lhs.true69
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB23_24
# BB#20:                                # %lor.lhs.false70
	leaq	-67(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jb	.LBB23_23
# BB#21:                                # %lor.lhs.false70
	cmpq	$58, %rax
	jne	.LBB23_24
# BB#22:                                # %land.lhs.true82
	movq	%r14, %rdi
	callq	symbolic_reference_mentioned_p
	testq	%r12, %rax
	je	.LBB23_24
.LBB23_23:                              # %lor.lhs.false85
	movq	%r14, %rdi
	callq	legitimate_pic_address_disp_p
	xorq	%r15, %r15
	testq	%r12, %rax
	je	.LBB23_47
.LBB23_24:                              # %land.rhs
	movq	$1, %r15
	jmp	.LBB23_47
.LBB23_11:                              # %if.end88
	movq	$16, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rbx, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rax
	jne	.LBB23_46
# BB#12:                                # %if.end94
	cmpq	$63, %r13
	jne	.LBB23_13
# BB#25:                                # %if.then96
	movq	8(%r14), %rax
	movl	(%rax), %edi
	movq	%rdi, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$66, %r13
	jne	.LBB23_27
# BB#26:                                # %land.lhs.true102
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_size(%rbx), %esi
	movq	$16, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movzbl	mode_size(%rbx), %ecx
	cmpq	%rcx, %rsi
	ja	.LBB23_46
.LBB23_27:                              # %if.end114
	movl	reload_completed(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB23_33
# BB#28:                                # %land.lhs.true116
	movl	16(%r14), %ecx
	cmpq	$66, %r13
	je	.LBB23_29
# BB#30:                                # %land.lhs.true116
	movabsq	$1, %rsi
	jmp	.LBB23_31
.LBB23_13:
	movq	%r14, %rax
	jmp	.LBB23_35
.LBB23_29:
	movabsq	$0, %rsi
.LBB23_31:                              # %land.lhs.true116
	testq	%rcx, %rcx
	je	.LBB23_33
# BB#32:                                # %land.lhs.true116
	testq	%rsi, %rsi
	je	.LBB23_46
.LBB23_33:                              # %if.end127
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB23_35
# BB#34:                                # %land.lhs.true135
	movzbl	mode_size(%rdx), %edx
	movq	$16, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rdx
	ja	.LBB23_46
.LBB23_35:                              # %if.end154
	movq	$1, %r15
	cmpq	$70, %r13
	je	.LBB23_47
# BB#36:                                # %if.end154
	cmpq	$66, %r13
	jne	.LBB23_37
# BB#42:                                # %if.then173
	movl	volatile_ok(%rip), %ecx
	movq	8(%rax), %rsi
	testq	%rcx, %rcx
	jne	.LBB23_44
# BB#43:                                # %if.then173
	movq	%rdi, %rax
	andq	$134217728, %rax        # imm = 0x8000000
	testq	%r12, %rax
	jne	.LBB23_46
.LBB23_44:                              # %if.end184
	movzbl	(%rsi), %eax
	cmpq	$70, %rax
	je	.LBB23_47
# BB#45:                                # %if.end190
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	xorq	%rdx, %rdx
	callq	legitimate_address_p
	testq	%r12, %rax
	jne	.LBB23_47
	jmp	.LBB23_46
.LBB23_37:                              # %if.end154
	cmpq	$61, %r13
	jne	.LBB23_46
# BB#38:                                # %if.then157
	movl	8(%rax), %eax
	cmpq	$52, %rax
	movq	$1, %r15
	ja	.LBB23_47
# BB#39:                                # %lor.rhs
	movl	regclass_map(,%rax,4), %eax
	testq	%rax, %rax
	je	.LBB23_40
# BB#41:                                # %lor.rhs
	movabsq	$1, %r15
	jmp	.LBB23_47
.LBB23_46:                              # %if.end202
	xorq	%r15, %r15
.LBB23_47:                              # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_40:
	movabsq	$0, %r15
	jmp	.LBB23_47
.Ltmp226:
	.size	general_operand, .Ltmp226-general_operand
	.cfi_endproc

	.globl	address_operand
	.align	16, 0x90
	.type	address_operand,@function
address_operand:                        # @address_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp228:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movzbl	(%rcx), %eax
	cmpq	$70, %rax
	movq	$1, %rax
	je	.LBB24_4
# BB#1:                                 # %do.body.i
	xorq	%rdx, %rdx
	movq	%rsi, %rdi
	movq	%rcx, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB24_2
# BB#3:                                 # %do.body.i
	movabsq	$1, %rax
.LBB24_4:                               # %memory_address_p.exit
	popq	%rdx
	retq
.LBB24_2:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp229:
	.size	address_operand, .Ltmp229-address_operand
	.cfi_endproc

	.globl	register_operand
	.align	16, 0x90
	.type	register_operand,@function
register_operand:                       # @register_operand
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %r9d
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rsi, %rdx
	andq	%r8, %rdx
	movq	$16, %rcx
	movq	%r9, %r10
	shrq	%cl, %r10
	andq	$255, %r10
	xorq	%rax, %rax
	cmpq	%rdx, %r10
	je	.LBB25_2
# BB#1:                                 # %entry
	testq	%r8, %rsi
	jne	.LBB25_15
.LBB25_2:                               # %if.end
	movq	%r9, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB25_3
# BB#4:                                 # %if.then5
	movl	reload_completed(%rip), %ecx
	movq	8(%rdi), %r8
	testq	%rcx, %rcx
	jne	.LBB25_6
# BB#5:                                 # %land.lhs.true6
	movzbl	(%r8), %ecx
	cmpq	$66, %rcx
	jne	.LBB25_6
# BB#16:                                # %if.then10
	jmp	general_operand  # TAILCALL
.LBB25_3:
	movq	%rdi, %r8
	jmp	.LBB25_9
.LBB25_6:                               # %if.end11
	movl	mode_class(,%r10,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB25_7
# BB#8:                                 # %land.lhs.true17
	movl	(%r8), %r9d
	movzbl	mode_size(%r10), %edx
	movq	$16, %rcx
	movq	%r9, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rdx
	ja	.LBB25_15
	jmp	.LBB25_9
.LBB25_7:                               # %if.end11.if.end33_crit_edge
	movl	(%r8), %r9d
.LBB25_9:                               # %if.end33
	movq	$1, %rax
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$70, %r9
	je	.LBB25_15
# BB#10:                                # %if.end33
	xorq	%rax, %rax
	cmpq	$61, %r9
	jne	.LBB25_15
# BB#11:                                # %land.rhs
	movl	8(%r8), %ecx
	cmpq	$52, %rcx
	movq	$1, %rax
	ja	.LBB25_15
# BB#12:                                # %lor.rhs
	movl	regclass_map(,%rcx,4), %eax
	testq	%rax, %rax
	je	.LBB25_13
# BB#14:                                # %lor.rhs
	movabsq	$1, %rax
.LBB25_15:                              # %return
	retq
.LBB25_13:
	movabsq	$0, %rax
	retq
.Ltmp230:
	.size	register_operand, .Ltmp230-register_operand
	.cfi_endproc

	.globl	pmode_register_operand
	.align	16, 0x90
	.type	pmode_register_operand,@function
pmode_register_operand:                 # @pmode_register_operand
	.cfi_startproc
# BB#0:                                 # %entry
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	movl	(%rdi), %r9d
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$16, %rcx
	movq	%r9, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	xorq	%rax, %rax
	cmpq	%rsi, %rdx
	jne	.LBB26_14
# BB#1:                                 # %if.end.i
	movq	%r9, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB26_2
# BB#3:                                 # %if.then5.i
	movl	reload_completed(%rip), %ecx
	movq	8(%rdi), %r8
	testq	%rcx, %rcx
	jne	.LBB26_5
# BB#4:                                 # %land.lhs.true6.i
	movzbl	(%r8), %ecx
	cmpq	$66, %rcx
	jne	.LBB26_5
# BB#15:                                # %if.then10.i
	jmp	general_operand  # TAILCALL
.LBB26_2:
	movq	%rdi, %r8
	jmp	.LBB26_8
.LBB26_5:                               # %if.end11.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB26_6
# BB#7:                                 # %land.lhs.true17.i
	movl	(%r8), %r9d
	movzbl	mode_size(%rsi), %edx
	movq	$16, %rcx
	movq	%r9, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rdx
	ja	.LBB26_14
	jmp	.LBB26_8
.LBB26_6:                               # %if.end11.if.end33_crit_edge.i
	movl	(%r8), %r9d
.LBB26_8:                               # %if.end33.i
	movq	$1, %rax
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$70, %r9
	je	.LBB26_14
# BB#9:                                 # %if.end33.i
	xorq	%rax, %rax
	cmpq	$61, %r9
	jne	.LBB26_14
# BB#10:                                # %land.rhs.i
	movl	8(%r8), %ecx
	cmpq	$52, %rcx
	movq	$1, %rax
	ja	.LBB26_14
# BB#11:                                # %lor.rhs.i
	movl	regclass_map(,%rcx,4), %eax
	testq	%rax, %rax
	je	.LBB26_12
# BB#13:                                # %lor.rhs.i
	movabsq	$1, %rax
.LBB26_14:                              # %register_operand.exit
	retq
.LBB26_12:
	movabsq	$0, %rax
	retq
.Ltmp231:
	.size	pmode_register_operand, .Ltmp231-pmode_register_operand
	.cfi_endproc

	.globl	scratch_operand
	.align	16, 0x90
	.type	scratch_operand,@function
scratch_operand:                        # @scratch_operand
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %r9d
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rsi, %rax
	andq	%r8, %rax
	movq	$16, %rcx
	movq	%r9, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rax, %rdx
	je	.LBB27_2
# BB#1:                                 # %entry
	xorq	%rax, %rax
	testq	%r8, %rsi
	jne	.LBB27_5
.LBB27_2:                               # %if.end
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$62, %r9
	movq	$1, %rax
	je	.LBB27_5
# BB#3:                                 # %lor.rhs
	xorq	%rax, %rax
	cmpq	$61, %r9
	jne	.LBB27_5
# BB#4:                                 # %land.rhs
	movl	8(%rdi), %eax
	cmpq	$53, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB27_5:                               # %return
	retq
.Ltmp232:
	.size	scratch_operand, .Ltmp232-scratch_operand
	.cfi_endproc

	.globl	immediate_operand
	.align	16, 0x90
	.type	immediate_operand,@function
immediate_operand:                      # @immediate_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp238:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp239:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp240:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp242:
	.cfi_def_cfa_offset 48
.Ltmp243:
	.cfi_offset %rbx, -40
.Ltmp244:
	.cfi_offset %r12, -32
.Ltmp245:
	.cfi_offset %r14, -24
.Ltmp246:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	(%r14), %eax
	movq	%rsi, %rbx
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB28_3
# BB#1:                                 # %entry
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	je	.LBB28_3
# BB#2:                                 # %land.lhs.true2
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	mode_class(,%rsi,4), %ecx
	xorq	%r15, %r15
	orq	$2, %rcx
	andq	%rdx, %rcx
	cmpq	$3, %rcx
	jne	.LBB28_20
.LBB28_3:                               # %if.end
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	jne	.LBB28_6
# BB#4:                                 # %land.lhs.true11
	movq	8(%r14), %rdi
	movq	%rbx, %rsi
	callq	trunc_int_for_mode
	movq	8(%r14), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jne	.LBB28_20
# BB#5:                                 # %land.lhs.true11.if.end18_crit_edge
	movl	(%r14), %eax
.LBB28_6:                               # %if.end18
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$140, %rdx
	movq	$1, %r15
	je	.LBB28_20
# BB#7:                                 # %if.end23
	xorq	%r15, %r15
	cmpq	$133, %rdx
	jg	.LBB28_10
# BB#8:                                 # %if.end23
	leaq	-54(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB28_20
# BB#9:                                 # %if.end23
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB28_11
	jmp	.LBB28_20
.LBB28_10:                              # %if.end23
	cmpq	$134, %rdx
	jne	.LBB28_20
.LBB28_11:                              # %land.lhs.true54
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rbx, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	je	.LBB28_14
# BB#12:                                # %land.lhs.true54
	testq	%r12, %rbx
	je	.LBB28_14
# BB#13:                                # %land.lhs.true54
	testq	%rax, %rax
	jne	.LBB28_20
.LBB28_14:                              # %land.lhs.true66
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB28_19
# BB#15:                                # %lor.lhs.false67
	leaq	-67(%rdx), %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	jb	.LBB28_18
# BB#16:                                # %lor.lhs.false67
	cmpq	$58, %rdx
	jne	.LBB28_19
# BB#17:                                # %land.lhs.true79
	movq	%r14, %rdi
	callq	symbolic_reference_mentioned_p
	testq	%r12, %rax
	je	.LBB28_19
.LBB28_18:                              # %lor.lhs.false82
	movq	%r14, %rdi
	callq	legitimate_pic_address_disp_p
	testq	%r12, %rax
	je	.LBB28_20
.LBB28_19:                              # %land.rhs
	movq	$1, %r15
.LBB28_20:                              # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp247:
	.size	immediate_operand, .Ltmp247-immediate_operand
	.cfi_endproc

	.globl	const_int_operand
	.align	16, 0x90
	.type	const_int_operand,@function
const_int_operand:                      # @const_int_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp251:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp252:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp253:
	.cfi_def_cfa_offset 32
.Ltmp254:
	.cfi_offset %rbx, -24
.Ltmp255:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	xorq	%r14, %r14
	cmpq	$54, %rax
	jne	.LBB29_4
# BB#1:                                 # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB29_3
# BB#2:                                 # %land.lhs.true
	movq	8(%rbx), %rdi
	callq	trunc_int_for_mode
	movq	8(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB29_4
.LBB29_3:                               # %if.end7
	movq	$1, %r14
.LBB29_4:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp256:
	.size	const_int_operand, .Ltmp256-const_int_operand
	.cfi_endproc

	.globl	const_double_operand
	.align	16, 0x90
	.type	const_double_operand,@function
const_double_operand:                   # @const_double_operand
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %edx
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB30_3
# BB#1:                                 # %entry
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rsi
	je	.LBB30_3
# BB#2:                                 # %land.lhs.true2
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %ecx
	xorq	%rax, %rax
	orq	$2, %rcx
	andq	%rdi, %rcx
	cmpq	$3, %rcx
	jne	.LBB30_10
.LBB30_3:                               # %if.end
	movq	%rdx, %rcx
	andq	$65534, %rcx            # imm = 0xFFFE
	xorq	%rax, %rax
	cmpq	$54, %rcx
	jne	.LBB30_10
# BB#4:                                 # %land.rhs
	movq	$1, %rax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rsi
	je	.LBB30_10
# BB#5:                                 # %lor.lhs.false15
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	%rdi, %rsi
	andq	$255, %rdx
	movabsq	$0, %rax
	movabsq	$1, %rdi
	movq	%rax, %rcx
	jne	.LBB30_7
# BB#6:                                 # %lor.lhs.false15
	movq	%rdi, %rcx
.LBB30_7:                               # %lor.lhs.false15
	cmpq	%rsi, %rdx
	jne	.LBB30_9
# BB#8:                                 # %lor.lhs.false15
	movq	%rdi, %rax
.LBB30_9:                               # %lor.lhs.false15
	orq	%rcx, %rax
.LBB30_10:                              # %return
	retq
.Ltmp257:
	.size	const_double_operand, .Ltmp257-const_double_operand
	.cfi_endproc

	.globl	nonimmediate_operand
	.align	16, 0x90
	.type	nonimmediate_operand,@function
nonimmediate_operand:                   # @nonimmediate_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp260:
	.cfi_def_cfa_offset 16
.Ltmp261:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	general_operand
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB31_7
# BB#1:                                 # %land.rhs
	movzbl	(%rbx), %ecx
	cmpq	$133, %rcx
	jg	.LBB31_4
# BB#2:                                 # %land.rhs
	addq	$-54, %rcx
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB31_6
# BB#3:                                 # %land.rhs
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB31_7
	jmp	.LBB31_6
.LBB31_4:                               # %land.rhs
	cmpq	$134, %rcx
	je	.LBB31_7
# BB#5:                                 # %land.rhs
	cmpq	$140, %rcx
	je	.LBB31_7
.LBB31_6:                               # %lor.rhs
	movq	$1, %rax
.LBB31_7:                               # %land.end
	popq	%rbx
	retq
.Ltmp262:
	.size	nonimmediate_operand, .Ltmp262-nonimmediate_operand
	.cfi_endproc

	.globl	nonmemory_operand
	.align	16, 0x90
	.type	nonmemory_operand,@function
nonmemory_operand:                      # @nonmemory_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp266:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp267:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp268:
	.cfi_def_cfa_offset 32
.Ltmp269:
	.cfi_offset %rbx, -24
.Ltmp270:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movl	(%r14), %edx
	movq	%rsi, %rbx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$133, %rsi
	jg	.LBB32_7
# BB#1:                                 # %entry
	leaq	-54(%rsi), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB32_3
# BB#2:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB32_9
	jmp	.LBB32_3
.LBB32_7:                               # %entry
	cmpq	$134, %rsi
	je	.LBB32_9
# BB#8:                                 # %entry
	cmpq	$140, %rsi
	jne	.LBB32_3
.LBB32_9:                               # %if.then
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB32_12
# BB#10:                                # %if.then
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rbx
	je	.LBB32_12
# BB#11:                                # %land.lhs.true32
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %ecx
	xorq	%rax, %rax
	orq	$2, %rcx
	andq	%rdi, %rcx
	cmpq	$3, %rcx
	jne	.LBB32_31
.LBB32_12:                              # %if.end
	cmpq	$54, %rsi
	jne	.LBB32_15
# BB#13:                                # %land.lhs.true42
	movq	8(%r14), %rdi
	movq	%rbx, %rsi
	callq	trunc_int_for_mode
	movq	8(%r14), %rdx
	movq	%rax, %rcx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB32_31
# BB#14:                                # %land.lhs.true42.if.end49_crit_edge
	movl	(%r14), %edx
.LBB32_15:                              # %if.end49
	movq	$16, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	je	.LBB32_18
# BB#16:                                # %lor.lhs.false54
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rbx, %rsi
	andq	%rcx, %rsi
	andq	%rcx, %rax
	cmpq	%rsi, %rax
	je	.LBB32_18
# BB#17:                                # %lor.lhs.false54
	xorq	%rax, %rax
	testq	%rcx, %rbx
	jne	.LBB32_31
.LBB32_18:                              # %land.lhs.true61
	movl	flag_pic(%rip), %eax
	testq	%rax, %rax
	je	.LBB32_23
# BB#19:                                # %lor.lhs.false62
	andq	$65535, %rdx            # imm = 0xFFFF
	leaq	-67(%rdx), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$2, %rax
	jb	.LBB32_22
# BB#20:                                # %lor.lhs.false62
	cmpq	$58, %rdx
	jne	.LBB32_23
# BB#21:                                # %land.lhs.true74
	movq	%r14, %rdi
	callq	symbolic_reference_mentioned_p
	testq	%rbx, %rax
	je	.LBB32_23
.LBB32_22:                              # %lor.lhs.false77
	movq	%r14, %rdi
	callq	legitimate_pic_address_disp_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rbx, %rcx
	je	.LBB32_31
.LBB32_23:                              # %land.rhs
	movq	$1, %rax
	jmp	.LBB32_31
.LBB32_3:                               # %if.end80
	movq	$16, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rbx, %rdi
	andq	%rcx, %rdi
	cmpq	%rdi, %rax
	je	.LBB32_5
# BB#4:                                 # %if.end80
	xorq	%rax, %rax
	testq	%rcx, %rbx
	jne	.LBB32_31
.LBB32_5:                               # %if.end88
	cmpq	$63, %rsi
	jne	.LBB32_6
# BB#24:                                # %if.then92
	movq	8(%r14), %rcx
	movl	reload_completed(%rip), %eax
	movl	(%rcx), %edx
	testq	%rax, %rax
	jne	.LBB32_26
# BB#25:                                # %if.then92
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB32_26
# BB#32:                                # %if.then100
	movq	%r14, %rdi
	movq	%rbx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	general_operand  # TAILCALL
.LBB32_6:
	movq	%r14, %rcx
.LBB32_26:                              # %if.end106
	andq	$65535, %rdx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$61, %rdx
	jne	.LBB32_31
# BB#27:                                # %land.rhs110
	movl	8(%rcx), %ecx
	cmpq	$52, %rcx
	movq	$1, %rax
	ja	.LBB32_31
# BB#28:                                # %lor.rhs
	movl	regclass_map(,%rcx,4), %eax
	testq	%rax, %rax
	je	.LBB32_29
# BB#30:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB32_31
.LBB32_29:
	movabsq	$0, %rax
.LBB32_31:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp271:
	.size	nonmemory_operand, .Ltmp271-nonmemory_operand
	.cfi_endproc

	.globl	push_operand
	.align	16, 0x90
	.type	push_operand,@function
push_operand:                           # @push_operand
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	target_flags+3(%rip), %ecx
	movzbl	mode_size(%rax), %r8d
	testq	$2, %rcx
	je	.LBB33_2
# BB#1:                                 # %cond.true
	leaq	7(%r8), %r9
	andq	$504, %r9               # imm = 0x1F8
	jmp	.LBB33_3
.LBB33_2:                               # %cond.false
	leaq	1(%r8), %r9
	andq	$510, %r9               # imm = 0x1FE
.LBB33_3:                               # %cond.end
	movl	(%rdi), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rcx
	jne	.LBB33_17
# BB#4:                                 # %if.end
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	testq	%r10, %rsi
	je	.LBB33_6
# BB#5:                                 # %land.lhs.true
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	andq	%r10, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB33_17
.LBB33_6:                               # %if.end12
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %edx
	andq	%r9, %r10
	cmpq	%r8, %r10
	jne	.LBB33_9
# BB#7:                                 # %if.then19
	cmpq	$96, %rdx
	jne	.LBB33_17
# BB#8:                                 # %if.then19.if.end74_crit_edge
	movq	8(%rcx), %rsi
.LBB33_14:                              # %if.end74
	movq	global_rtl+16(%rip), %rax
	cmpq	%rax, %rsi
	jne	.LBB33_15
# BB#16:                                # %if.end74
	movabsq	$1, %rax
	jmp	.LBB33_17
.LBB33_9:                               # %if.else
	cmpq	$100, %rdx
	jne	.LBB33_17
# BB#10:                                # %lor.lhs.false
	movq	16(%rcx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$75, %rsi
	jne	.LBB33_17
# BB#11:                                # %lor.lhs.false37
	movq	8(%rdx), %rsi
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB33_17
# BB#12:                                # %lor.lhs.false49
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB33_17
# BB#13:                                # %lor.lhs.false60
	movq	8(%rcx), %rcx
	addq	%r9, %rcx
	je	.LBB33_14
.LBB33_17:                              # %return
	retq
.LBB33_15:
	movabsq	$0, %rax
	retq
.Ltmp272:
	.size	push_operand, .Ltmp272-push_operand
	.cfi_endproc

	.globl	pop_operand
	.align	16, 0x90
	.type	pop_operand,@function
pop_operand:                            # @pop_operand
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rcx
	jne	.LBB34_7
# BB#1:                                 # %if.end
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rsi
	je	.LBB34_3
# BB#2:                                 # %land.lhs.true
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	andq	%r8, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB34_7
.LBB34_3:                               # %if.end6
	movq	8(%rdi), %rcx
	movzbl	(%rcx), %edx
	cmpq	$99, %rdx
	jne	.LBB34_7
# BB#4:                                 # %if.end11
	movq	8(%rcx), %rax
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB34_5
# BB#6:                                 # %if.end11
	movabsq	$1, %rax
.LBB34_7:                               # %return
	retq
.LBB34_5:
	movabsq	$0, %rax
	retq
.Ltmp273:
	.size	pop_operand, .Ltmp273-pop_operand
	.cfi_endproc

	.globl	memory_operand
	.align	16, 0x90
	.type	memory_operand,@function
memory_operand:                         # @memory_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp277:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp278:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp279:
	.cfi_def_cfa_offset 32
.Ltmp280:
	.cfi_offset %rbx, -24
.Ltmp281:
	.cfi_offset %r14, -16
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB35_1
# BB#4:                                 # %if.end
	movl	(%rdi), %ebx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rsi
	je	.LBB35_6
# BB#5:                                 # %land.lhs.true
	movq	$16, %rcx
	movq	%rbx, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rsi, %rcx
	andq	%r14, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	jne	.LBB35_12
.LBB35_6:                               # %if.end7
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB35_8
# BB#7:                                 # %if.then11
	movq	8(%rdi), %rax
	movl	(%rax), %ebx
.LBB35_8:                               # %if.end12
	andq	$65535, %rbx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rbx
	jne	.LBB35_12
# BB#9:                                 # %land.rhs16
	callq	general_operand
	testq	%r14, %rax
	jne	.LBB35_11
# BB#10:
	movabsq	$0, %rax
	jmp	.LBB35_12
.LBB35_1:                               # %if.then
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$66, %rcx
	jne	.LBB35_12
# BB#2:                                 # %land.rhs
	callq	general_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB35_3
.LBB35_11:                              # %land.rhs16
	movabsq	$1, %rax
	jmp	.LBB35_12
.LBB35_3:
	movabsq	$0, %rax
.LBB35_12:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp282:
	.size	memory_operand, .Ltmp282-memory_operand
	.cfi_endproc

	.globl	indirect_operand
	.align	16, 0x90
	.type	indirect_operand,@function
indirect_operand:                       # @indirect_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp288:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp289:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp290:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp291:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp292:
	.cfi_def_cfa_offset 48
.Ltmp293:
	.cfi_offset %rbx, -40
.Ltmp294:
	.cfi_offset %r12, -32
.Ltmp295:
	.cfi_offset %r14, -24
.Ltmp296:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %eax
	movl	reload_completed(%rip), %ecx
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	testq	%rcx, %rcx
	jne	.LBB36_12
# BB#1:                                 # %entry
	cmpq	$63, %rdx
	jne	.LBB36_12
# BB#2:                                 # %land.lhs.true1
	movq	8(%rbx), %r12
	movzbl	(%r12), %edi
	cmpq	$66, %rdi
	jne	.LBB36_12
# BB#3:                                 # %if.then
	movl	16(%rbx), %ebx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rsi
	je	.LBB36_5
# BB#4:                                 # %land.lhs.true11
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	andq	%r15, %rsi
	xorq	%r14, %r14
	cmpq	%rsi, %rax
	jne	.LBB36_20
.LBB36_5:                               # %if.end
	testq	%r15, %rbx
	jne	.LBB36_7
# BB#6:                                 # %land.lhs.true17
	movl	target_flags(%rip), %esi
	movq	8(%r12), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	general_operand
	testq	%r15, %rax
	movq	$1, %r14
	jne	.LBB36_20
.LBB36_7:                               # %lor.rhs
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	xorq	%r14, %r14
	cmpq	$75, %rcx
	jne	.LBB36_20
# BB#8:                                 # %land.lhs.true29
	movq	16(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB36_20
# BB#9:                                 # %land.lhs.true39
	movq	8(%rcx), %rdx
	negq	%rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	jne	.LBB36_20
# BB#10:                                # %land.rhs
	movl	target_flags(%rip), %esi
	movq	8(%rax), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	general_operand
	testq	%r15, %rax
	jne	.LBB36_19
# BB#11:
	movabsq	$0, %r14
	jmp	.LBB36_20
.LBB36_12:                              # %if.end61
	xorq	%r14, %r14
	cmpq	$66, %rdx
	jne	.LBB36_20
# BB#13:                                # %land.lhs.true66
	testq	%rcx, %rcx
	je	.LBB36_16
# BB#14:                                # %if.end.i
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rsi
	je	.LBB36_16
# BB#15:                                # %land.lhs.true.i
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	andq	%rsi, %rdx
	cmpq	%rdx, %rax
	jne	.LBB36_20
.LBB36_16:                              # %land.rhs16.i
	movq	%rbx, %rdi
	callq	general_operand
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB36_20
# BB#17:                                # %land.rhs69
	movl	target_flags(%rip), %esi
	movq	8(%rbx), %rdi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	callq	general_operand
	testq	%r15, %rax
	je	.LBB36_18
.LBB36_19:                              # %land.rhs69
	movabsq	$1, %r14
	jmp	.LBB36_20
.LBB36_18:
	movabsq	$0, %r14
.LBB36_20:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp297:
	.size	indirect_operand, .Ltmp297-indirect_operand
	.cfi_endproc

	.globl	comparison_operator
	.align	16, 0x90
	.type	comparison_operator,@function
comparison_operator:                    # @comparison_operator
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %edx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB37_2
# BB#1:                                 # %lor.lhs.false
	movq	$16, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	andq	%rax, %rsi
	xorq	%rax, %rax
	cmpq	%rsi, %rdi
	jne	.LBB37_5
.LBB37_2:                               # %land.rhs
	andq	$65535, %rdx            # imm = 0xFFFF
	movzbl	rtx_class(%rdx), %eax
	cmpq	$60, %rax
	jne	.LBB37_3
# BB#4:                                 # %land.rhs
	movabsq	$1, %rax
.LBB37_5:                               # %land.end
	retq
.LBB37_3:
	movabsq	$0, %rax
	retq
.Ltmp298:
	.size	comparison_operator, .Ltmp298-comparison_operator
	.cfi_endproc

	.globl	offsettable_nonstrict_memref_p
	.align	16, 0x90
	.type	offsettable_nonstrict_memref_p,@function
offsettable_nonstrict_memref_p:         # @offsettable_nonstrict_memref_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp300:
	.cfi_def_cfa_offset 16
	movl	(%rdi), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rcx
	jne	.LBB38_4
# BB#1:                                 # %land.rhs
	movq	8(%rdi), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	xorq	%rdi, %rdi
	callq	offsettable_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB38_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB38_4:                               # %land.end
	popq	%rdx
	retq
.LBB38_2:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp301:
	.size	offsettable_nonstrict_memref_p, .Ltmp301-offsettable_nonstrict_memref_p
	.cfi_endproc

	.globl	find_constant_term_loc
	.align	16, 0x90
	.type	find_constant_term_loc,@function
find_constant_term_loc:                 # @find_constant_term_loc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp304:
	.cfi_def_cfa_offset 16
.Ltmp305:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	movq	(%rax), %rbx
	movzbl	(%rbx), %ecx
	addq	$-54, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$21, %rcx
	ja	.LBB39_19
# BB#1:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24593, %rdx            # imm = 0x6011
	jne	.LBB39_20
# BB#2:                                 # %entry
	cmpq	$21, %rcx
	jne	.LBB39_19
# BB#3:                                 # %if.end10
	movq	8(%rbx), %rdx
	testq	%rdx, %rdx
	je	.LBB39_17
# BB#4:                                 # %land.lhs.true
	movzbl	(%rdx), %ecx
	leaq	8(%rbx), %rdi
	cmpq	$133, %rcx
	jg	.LBB39_7
# BB#5:                                 # %land.lhs.true
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB39_16
# BB#6:                                 # %land.lhs.true
	movq	$1, %r8
	shlq	%cl, %r8
	testq	$24599, %r8             # imm = 0x6017
	jne	.LBB39_9
	jmp	.LBB39_16
.LBB39_7:                               # %land.lhs.true
	cmpq	$134, %rcx
	je	.LBB39_9
# BB#8:                                 # %land.lhs.true
	cmpq	$140, %rcx
	jne	.LBB39_16
.LBB39_9:                               # %land.lhs.true66
	movq	16(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB39_15
# BB#10:                                # %land.lhs.true71
	movzbl	(%rcx), %ecx
	cmpq	$133, %rcx
	jg	.LBB39_13
# BB#11:                                # %land.lhs.true71
	addq	$-54, %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB39_15
# BB#12:                                # %land.lhs.true71
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB39_20
	jmp	.LBB39_15
.LBB39_13:                              # %land.lhs.true71
	cmpq	$134, %rcx
	je	.LBB39_20
# BB#14:                                # %land.lhs.true71
	cmpq	$140, %rcx
	je	.LBB39_20
.LBB39_15:                              # %if.end128
	testq	%rdx, %rdx
	je	.LBB39_17
.LBB39_16:                              # %if.then133
	callq	find_constant_term_loc
	testq	%rax, %rax
	jne	.LBB39_20
.LBB39_17:                              # %if.end140
	cmpq	$0, 16(%rbx)
	je	.LBB39_19
# BB#18:                                # %if.then145
	addq	$16, %rbx
	movq	%rbx, %rdi
	callq	find_constant_term_loc
	testq	%rax, %rax
	jne	.LBB39_20
.LBB39_19:                              # %if.end153
	xorq	%rax, %rax
.LBB39_20:                              # %return
	popq	%rbx
	retq
.Ltmp306:
	.size	find_constant_term_loc, .Ltmp306-find_constant_term_loc
	.cfi_endproc

	.globl	offsettable_memref_p
	.align	16, 0x90
	.type	offsettable_memref_p,@function
offsettable_memref_p:                   # @offsettable_memref_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp308:
	.cfi_def_cfa_offset 16
	movl	(%rdi), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$66, %rcx
	jne	.LBB40_4
# BB#1:                                 # %land.rhs
	movq	8(%rdi), %rdx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$1, %rdi
	callq	offsettable_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB40_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB40_4:                               # %land.end
	popq	%rdx
	retq
.LBB40_2:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp309:
	.size	offsettable_memref_p, .Ltmp309-offsettable_memref_p
	.cfi_endproc

	.globl	offsettable_address_p
	.align	16, 0x90
	.type	offsettable_address_p,@function
offsettable_address_p:                  # @offsettable_address_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp316:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp317:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp318:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp319:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp320:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp321:
	.cfi_def_cfa_offset 64
.Ltmp322:
	.cfi_offset %rbx, -48
.Ltmp323:
	.cfi_offset %r12, -40
.Ltmp324:
	.cfi_offset %r13, -32
.Ltmp325:
	.cfi_offset %r14, -24
.Ltmp326:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movl	(%r12), %eax
	movq	%rsi, %r14
	movq	%rax, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rdi
	movq	%r12, 8(%rsp)
	jne	.LBB41_1
# BB#2:                                 # %entry
	movabsq	$memory_address_p, %rcx
	jmp	.LBB41_3
.LBB41_1:
	movabsq	$strict_memory_address_p, %rcx
.LBB41_3:                               # %entry
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	-54(%r13), %rcx
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB41_5
# BB#4:                                 # %entry
	movq	$1, %rbx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24595, %rsi            # imm = 0x6013
	jne	.LBB41_18
.LBB41_5:                               # %if.end
	andq	$65534, %rax            # imm = 0xFFFE
	xorq	%rbx, %rbx
	cmpq	$98, %rax
	je	.LBB41_18
# BB#6:                                 # %if.end25
	movzbl	mode_size(%rdx), %eax
	testq	%rax, %rax
	movq	$16, %r15
	je	.LBB41_8
# BB#7:                                 # %if.end25
	movq	%rax, %r15
.LBB41_8:                               # %if.end25
	cmpq	$75, %r13
	jne	.LBB41_11
# BB#9:                                 # %land.lhs.true
	leaq	8(%rsp), %rdi
	callq	find_constant_term_loc
	testq	%rax, %rax
	je	.LBB41_11
# BB#10:                                # %if.then34
	movq	%rax, %r14
	movq	(%r14), %rax
	movq	%rax, 8(%rsp)
	movq	(%r14), %rdi
	decq	%r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	%rax, (%r14)
	movq	$2, %rdi
	movq	%r12, %rsi
	callq	*(%rsp)                 # 8-byte Folded Reload
	movq	8(%rsp), %rcx
	movq	%rax, %rbx
	movq	%rcx, (%r14)
	jmp	.LBB41_18
.LBB41_11:                              # %if.end38
	movzbl	rtx_class(%r13), %eax
	cmpq	$97, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB41_18
# BB#12:                                # %if.end45
	cmpq	$135, %r13
	jne	.LBB41_16
# BB#13:                                # %if.end45
	movq	%r14, %rax
	andq	%rsi, %rax
	cmpq	$51, %rax
	je	.LBB41_16
# BB#14:                                # %land.lhs.true53
	movq	%r14, %rdi
	movq	%rsi, %rbx
	callq	get_mode_alignment
	movq	%rbx, %rsi
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	%r15, %rax
	andq	%rsi, %rax
	cmpq	%rdx, %rax
	jbe	.LBB41_15
.LBB41_16:                              # %if.else
	decq	%r15
	andq	%rsi, %r15
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	plus_constant_wide
	jmp	.LBB41_17
.LBB41_15:                              # %if.then57
	movq	8(%r12), %r14
	movq	16(%r12), %rdi
	movzbl	2(%r12), %ebx
	decq	%r15
	andq	%rsi, %r15
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	$135, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
.LBB41_17:                              # %if.end71
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	*(%rsp)                 # 8-byte Folded Reload
	movq	%rax, %rbx
.LBB41_18:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp327:
	.size	offsettable_address_p, .Ltmp327-offsettable_address_p
	.cfi_endproc

	.globl	mode_dependent_address_p
	.align	16, 0x90
	.type	mode_dependent_address_p,@function
mode_dependent_address_p:               # @mode_dependent_address_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$98, %rax
	jne	.LBB42_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB42_1:
	movabsq	$0, %rax
	retq
.Ltmp328:
	.size	mode_dependent_address_p, .Ltmp328-mode_dependent_address_p
	.cfi_endproc

	.globl	mode_independent_operand
	.align	16, 0x90
	.type	mode_independent_operand,@function
mode_independent_operand:               # @mode_independent_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp331:
	.cfi_def_cfa_offset 16
.Ltmp332:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	general_operand
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB43_5
# BB#1:                                 # %if.end
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	movq	$1, %rax
	jne	.LBB43_5
# BB#2:                                 # %if.end2
	movq	8(%rbx), %rax
	movl	(%rax), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$98, %rax
	je	.LBB43_3
# BB#4:                                 # %if.end2
	movabsq	$1, %rax
.LBB43_5:                               # %return
	popq	%rbx
	retq
.LBB43_3:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp333:
	.size	mode_independent_operand, .Ltmp333-mode_independent_operand
	.cfi_endproc

	.globl	extract_insn_cached
	.align	16, 0x90
	.type	extract_insn_cached,@function
extract_insn_cached:                    # @extract_insn_cached
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp336:
	.cfi_def_cfa_offset 16
.Ltmp337:
	.cfi_offset %rbx, -16
	movq	recog_data+1000(%rip), %rax
	movq	%rdi, %rbx
	cmpq	%rbx, %rax
	jne	.LBB44_2
# BB#1:                                 # %land.lhs.true
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB44_3
.LBB44_2:                               # %if.end
	movq	%rbx, %rdi
	callq	extract_insn
	movq	%rbx, recog_data+1000(%rip)
.LBB44_3:                               # %return
	popq	%rbx
	retq
.Ltmp338:
	.size	extract_insn_cached, .Ltmp338-extract_insn_cached
	.cfi_endproc

	.globl	extract_constrain_insn_cached
	.align	16, 0x90
	.type	extract_constrain_insn_cached,@function
extract_constrain_insn_cached:          # @extract_constrain_insn_cached
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp342:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp343:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp344:
	.cfi_def_cfa_offset 32
.Ltmp345:
	.cfi_offset %rbx, -24
.Ltmp346:
	.cfi_offset %r14, -16
	movq	recog_data+1000(%rip), %rax
	movq	%rdi, %rbx
	cmpq	%rbx, %rax
	jne	.LBB45_2
# BB#1:                                 # %land.lhs.true.i
	movslq	40(%rbx), %rax
	testq	%rax, %rax
	jns	.LBB45_3
.LBB45_2:                               # %if.end.i
	movq	%rbx, %rdi
	callq	extract_insn
	movq	%rbx, recog_data+1000(%rip)
.LBB45_3:                               # %extract_insn_cached.exit
	movl	which_alternative(%rip), %eax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	cmpq	%r14, %rax
	jne	.LBB45_5
# BB#4:                                 # %land.lhs.true
	movl	reload_completed(%rip), %edi
	callq	constrain_operands
	testq	%r14, %rax
	je	.LBB45_6
.LBB45_5:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB45_6:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_constrain_insn_cached, %rcx
	movq	$2063, %rdx             # imm = 0x80F
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	_fatal_insn_not_found
.Ltmp347:
	.size	extract_constrain_insn_cached, .Ltmp347-extract_constrain_insn_cached
	.cfi_endproc

	.globl	constrain_operands_cached
	.align	16, 0x90
	.type	constrain_operands_cached,@function
constrain_operands_cached:              # @constrain_operands_cached
	.cfi_startproc
# BB#0:                                 # %entry
	movl	which_alternative(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	%rcx, %rax
	je	.LBB46_2
# BB#1:                                 # %return
	movq	$1, %rax
	retq
.LBB46_2:                               # %if.then
	jmp	constrain_operands  # TAILCALL
.Ltmp348:
	.size	constrain_operands_cached, .Ltmp348-constrain_operands_cached
	.cfi_endproc

	.globl	preprocess_constraints
	.align	16, 0x90
	.type	preprocess_constraints,@function
preprocess_constraints:                 # @preprocess_constraints
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp355:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp356:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp357:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp358:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp359:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp360:
	.cfi_def_cfa_offset 80
.Ltmp361:
	.cfi_offset %rbx, -48
.Ltmp362:
	.cfi_offset %r12, -40
.Ltmp363:
	.cfi_offset %r13, -32
.Ltmp364:
	.cfi_offset %r14, -24
.Ltmp365:
	.cfi_offset %r15, -16
	movabsq	$recog_op_alt, %rdi
	xorq	%rsi, %rsi
	movq	$28800, %rdx            # imm = 0x7080
	xorq	%r14, %r14
	callq	memset
	movl	recog_data+996(%rip), %eax
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB47_71
# BB#1:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB47_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB47_4 Depth 2
                                        #       Child Loop BB47_7 Depth 3
                                        #         Child Loop BB47_8 Depth 4
	movq	$40, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB47_70
# BB#3:                                 # %for.body8.lr.ph
                                        #   in Loop: Header=BB47_2 Depth=1
	movq	recog_data+480(,%r14,8), %rdi
	xorq	%r13, %r13
	.align	16, 0x90
.LBB47_4:                               # %for.body8
                                        #   Parent Loop BB47_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB47_7 Depth 3
                                        #         Child Loop BB47_8 Depth 4
	movq	$5, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	imulq	$960, %r14, %rdx        # imm = 0x3C0
	movl	%ebx, recog_op_alt+8(%rdx,%rsi)
	movq	%rdi, recog_op_alt(%rdx,%rsi)
	movl	%r12d, recog_op_alt+16(%rdx,%rsi)
	movl	%r12d, recog_op_alt+20(%rdx,%rsi)
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB47_68
# BB#5:                                 # %for.body8
                                        #   in Loop: Header=BB47_4 Depth=2
	cmpq	$44, %rax
	jne	.LBB47_6
.LBB47_68:                              # %if.then
                                        #   in Loop: Header=BB47_4 Depth=2
	movq	$5, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movzbl	recog_op_alt+24(%rdx,%rax), %ecx
	orq	$128, %rcx
	movb	%cl, recog_op_alt+24(%rdx,%rax)
	jmp	.LBB47_69
	.align	16, 0x90
.LBB47_6:                               # %for.cond25.preheader
                                        #   in Loop: Header=BB47_4 Depth=2
	leaq	recog_op_alt+8(%rdx,%rsi), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	leaq	recog_op_alt+16(%rdx,%rsi), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	$5, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	leaq	recog_op_alt+12(%rdx,%rsi), %r8
	leaq	recog_op_alt+24(%rdx,%rsi), %r9
	jmp	.LBB47_7
	.align	16, 0x90
.LBB47_67:                              # %cond.end279
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	16(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx), %rax
	imulq	$100, %rax, %rax
	movl	reg_class_subunion(%rax,%rsi,4), %eax
	movl	%eax, (%rcx)
	xorq	%rbx, %rbx
	movzbl	(%rdi), %eax
.LBB47_7:                               # %for.cond25
                                        #   Parent Loop BB47_2 Depth=1
                                        #     Parent Loop BB47_4 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB47_8 Depth 4
	incq	%rdi
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$35, %rcx
	jne	.LBB47_10
.LBB47_8:                               # %do.body
                                        #   Parent Loop BB47_2 Depth=1
                                        #     Parent Loop BB47_4 Depth=2
                                        #       Parent Loop BB47_7 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%rdi), %eax
	incq	%rdi
	testq	%rax, %rax
	je	.LBB47_10
# BB#9:                                 # %do.body
                                        #   in Loop: Header=BB47_8 Depth=4
	cmpq	$44, %rax
	jne	.LBB47_8
	.align	16, 0x90
.LBB47_10:                              # %if.end37
                                        #   in Loop: Header=BB47_7 Depth=3
	testq	$255, %rax
	je	.LBB47_69
# BB#11:                                # %if.end37
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$44, %rdx
	je	.LBB47_69
# BB#12:                                # %if.end46
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	addq	$-33, %rsi
	andq	%r12, %rsi
	cmpq	$82, %rsi
	ja	.LBB47_27
# BB#13:                                # %if.end46
                                        #   in Loop: Header=BB47_7 Depth=3
	jmpq	*.LJTI47_0(,%rsi,8)
.LBB47_17:                              # %sw.bb62
                                        #   in Loop: Header=BB47_7 Depth=3
	decq	%rdi
	movq	$10, %rdx
	leaq	24(%rsp), %rsi
	movq	%r8, %r15
	movq	%r9, %rbx
	callq	strtoul
	movq	%rbx, %r9
	xorq	%rbx, %rbx
	movq	%r15, %r8
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%eax, (%rcx)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	movq	%r13, %rdx
	movq	24(%rsp), %rdi
	shlq	%cl, %rdx
	imulq	$960, %rax, %rax        # imm = 0x3C0
	movl	%r14d, recog_op_alt+20(%rax,%rdx)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
	.align	16, 0x90
.LBB47_27:                              # %sw.default
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$12, %rsi
	cmpq	$114, %rdx
	je	.LBB47_67
# BB#28:                                # %cond.false
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$11, %rsi
	cmpq	$82, %rdx
	je	.LBB47_67
# BB#29:                                # %cond.false
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$113, %rdx
	jne	.LBB47_31
# BB#30:                                # %cond.true148
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	target_flags(%rip), %esi
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$23, %rcx
	shrq	%cl, %rsi
	orq	$8, %rsi
	jmp	.LBB47_67
.LBB47_31:                              # %cond.false149
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$101, %rsi
	jg	.LBB47_44
# BB#32:                                # %cond.false149
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$8, %rsi
	cmpq	$81, %rdx
	je	.LBB47_67
	jmp	.LBB47_33
.LBB47_44:                              # %cond.false149
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$117, %rdx
	je	.LBB47_55
# BB#45:                                # %cond.false149
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$116, %rdx
	jne	.LBB47_46
# BB#51:                                # %cond.true168
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	target_flags(%rip), %eax
	testq	$1, %rax
	movq	$13, %rsi
	jne	.LBB47_67
# BB#52:                                # %lor.rhs171
                                        #   in Loop: Header=BB47_7 Depth=3
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB47_54
# BB#53:                                # %lor.rhs171
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$13, %rax
.LBB47_54:                              # %lor.rhs171
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	%rax, %rsi
	jmp	.LBB47_67
.LBB47_15:                              # %sw.bb51
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	(%r8), %eax
	addq	$600, %rax              # imm = 0x258
	movl	%eax, (%r8)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_16:                              # %sw.bb56
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$1, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_19:                              # %sw.bb81
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$16, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_20:                              # %sw.bb87
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$32, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_14:                              # %sw.bb48
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	(%r8), %eax
	addq	$6, %rax
	movl	%eax, (%r8)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_21:                              # %sw.bb93
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$8, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_23:                              # %sw.bb105
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$128, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_18:                              # %sw.bb75
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$2, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_22:                              # %sw.bb99
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$4, %rax
	movb	%al, (%r9)
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_24:                              # %sw.bb112
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%r9), %eax
	orq	$64, %rax
	movb	%al, (%r9)
.LBB47_25:                              # %sw.bb127
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	16(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx), %rax
	imulq	$100, %rax, %rax
	movl	reg_class_subunion+48(%rax), %eax
	movl	%eax, (%rcx)
.LBB47_26:                              # %for.cond25.backedge
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	(%rdi), %eax
	jmp	.LBB47_7
.LBB47_55:                              # %cond.true180
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	target_flags(%rip), %eax
	testq	$1, %rax
	movq	$14, %rsi
	jne	.LBB47_67
# BB#56:                                # %lor.rhs183
                                        #   in Loop: Header=BB47_7 Depth=3
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB47_58
# BB#57:                                # %lor.rhs183
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$14, %rax
.LBB47_58:                              # %lor.rhs183
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	%rax, %rsi
	jmp	.LBB47_67
.LBB47_46:                              # %cond.false149
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$102, %rdx
	jne	.LBB47_33
# BB#47:                                # %cond.true158
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	target_flags(%rip), %eax
	testq	$1, %rax
	movq	$15, %rsi
	jne	.LBB47_67
# BB#48:                                # %lor.rhs
                                        #   in Loop: Header=BB47_7 Depth=3
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB47_50
# BB#49:                                # %lor.rhs
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$15, %rax
.LBB47_50:                              # %lor.rhs
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	%rax, %rsi
	jmp	.LBB47_67
.LBB47_33:                              # %cond.false188
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$1, %rsi
	cmpq	$97, %rdx
	je	.LBB47_67
# BB#34:                                # %cond.false193
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$4, %rsi
	cmpq	$98, %rdx
	je	.LBB47_67
# BB#35:                                # %cond.false198
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$3, %rsi
	cmpq	$99, %rdx
	je	.LBB47_67
# BB#36:                                # %cond.false203
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$119, %rax
	jg	.LBB47_59
# BB#37:                                # %cond.false203
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$89, %rdx
	jne	.LBB47_38
# BB#64:                                # %cond.true221
                                        #   in Loop: Header=BB47_7 Depth=3
	movl	target_flags(%rip), %esi
	andq	$262144, %rsi           # imm = 0x40000
	movq	$14, %rcx
	shrq	%cl, %rsi
	jmp	.LBB47_67
.LBB47_59:                              # %cond.false203
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$121, %rdx
	jne	.LBB47_60
# BB#65:                                # %cond.true230
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	target_flags+1(%rip), %esi
	andq	$64, %rsi
	movq	$6, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB47_67
# BB#66:                                # %cond.true230
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$17, %rsi
	jmp	.LBB47_67
.LBB47_38:                              # %cond.false203
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$2, %rsi
	cmpq	$100, %rdx
	je	.LBB47_67
	jmp	.LBB47_39
.LBB47_60:                              # %cond.false203
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$120, %rdx
	jne	.LBB47_39
# BB#61:                                # %cond.true212
                                        #   in Loop: Header=BB47_7 Depth=3
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	movabsq	$0, %rsi
	je	.LBB47_63
# BB#62:                                # %cond.true212
                                        #   in Loop: Header=BB47_7 Depth=3
	movabsq	$1, %rsi
.LBB47_63:                              # %cond.true212
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$4, %rcx
	shlq	%cl, %rsi
	jmp	.LBB47_67
.LBB47_39:                              # %cond.false235
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$7, %rsi
	cmpq	$65, %rdx
	je	.LBB47_67
# BB#40:                                # %cond.false240
                                        #   in Loop: Header=BB47_7 Depth=3
	movq	$6, %rsi
	cmpq	$68, %rdx
	je	.LBB47_67
# BB#41:                                # %cond.false245
                                        #   in Loop: Header=BB47_7 Depth=3
	cmpq	$83, %rdx
	movabsq	$0, %rax
	jne	.LBB47_43
# BB#42:                                # %cond.false245
                                        #   in Loop: Header=BB47_7 Depth=3
	movabsq	$1, %rax
.LBB47_43:                              # %cond.false245
                                        #   in Loop: Header=BB47_7 Depth=3
	leaq	(%rax,%rax,4), %rsi
	jmp	.LBB47_67
	.align	16, 0x90
.LBB47_69:                              # %for.inc
                                        #   in Loop: Header=BB47_4 Depth=2
	incq	%r13
	movsbq	recog_data+998(%rip), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB47_4
.LBB47_70:                              # %for.inc292
                                        #   in Loop: Header=BB47_2 Depth=1
	movl	recog_data+996(%rip), %eax
	incq	%r14
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB47_2
.LBB47_71:                              # %for.end294
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp366:
	.size	preprocess_constraints, .Ltmp366-preprocess_constraints
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI47_0:
	.quad	.LBB47_15
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_26
	.quad	.LBB47_16
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_17
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_19
	.quad	.LBB47_26
	.quad	.LBB47_20
	.quad	.LBB47_14
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_26
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_21
	.quad	.LBB47_27
	.quad	.LBB47_23
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_25
	.quad	.LBB47_27
	.quad	.LBB47_26
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_27
	.quad	.LBB47_18
	.quad	.LBB47_26
	.quad	.LBB47_22
	.quad	.LBB47_24
	.quad	.LBB47_27
	.quad	.LBB47_25
	.quad	.LBB47_26

	.text
	.globl	reg_fits_class_p
	.align	16, 0x90
	.type	reg_fits_class_p,@function
reg_fits_class_p:                       # @reg_fits_class_p
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	8(%rdi), %rdi
	movq	%rcx, %r8
	xorq	%rax, %rax
	cmpq	$52, %rdi
	jg	.LBB48_21
# BB#1:                                 # %land.lhs.true
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	reg_class_contents(,%rsi,8), %rsi
	addq	%rdx, %rdi
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rsi
	je	.LBB48_21
# BB#2:                                 # %if.then
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	je	.LBB48_6
# BB#3:                                 # %if.then
	leaq	-21(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB48_6
# BB#4:                                 # %if.then
	leaq	-45(%rdi), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB48_6
# BB#5:                                 # %if.then
	leaq	-29(%rdi), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB48_6
# BB#11:                                # %cond.false
	andq	%r8, %rax
	cmpq	$24, %rax
	jne	.LBB48_12
# BB#14:                                # %cond.true30
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB48_16
.LBB48_6:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movl	mode_class(,%r8,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB48_16
# BB#7:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB48_8
# BB#9:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB48_16
.LBB48_12:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB48_15
# BB#13:                                # %cond.true24
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB48_16
.LBB48_8:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB48_16
.LBB48_15:                              # %cond.false34
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	movl	target_flags(%rip), %r9d
	sarq	%cl, %r8
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
	.align	16, 0x90
.LBB48_16:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB48_18
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB48_16 Depth=1
	leaq	(%rax,%rdi), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rsi
	jne	.LBB48_16
.LBB48_18:                              # %for.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB48_19
# BB#20:                                # %for.end
	movabsq	$1, %rax
.LBB48_21:                              # %return
	retq
.LBB48_19:
	movabsq	$0, %rax
	retq
.Ltmp367:
	.size	reg_fits_class_p, .Ltmp367-reg_fits_class_p
	.cfi_endproc

	.globl	split_all_insns
	.align	16, 0x90
	.type	split_all_insns,@function
split_all_insns:                        # @split_all_insns
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp374:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp375:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp376:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp377:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp378:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp379:
	.cfi_def_cfa_offset 80
.Ltmp380:
	.cfi_offset %rbx, -48
.Ltmp381:
	.cfi_offset %r12, -40
.Ltmp382:
	.cfi_offset %r13, -32
.Ltmp383:
	.cfi_offset %r14, -24
.Ltmp384:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	%rax, %rdi
	callq	sbitmap_alloc
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	sbitmap_zero
	movslq	n_basic_blocks(%rip), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB49_1:                               # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB49_3 Depth 2
                                        #       Child Loop BB49_6 Depth 3
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB49_11
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB49_1 Depth=1
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdx,8), %r14
	decq	%rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	(%r14), %rbx
	movq	$1, %r15
	movq	%rdx, %rcx
	shlq	%cl, %r15
	movq	$6, %rcx
	movq	%rdx, %r12
	shrq	%cl, %r12
	andq	$67108863, %r12         # imm = 0x3FFFFFF
	.align	16, 0x90
.LBB49_3:                               # %for.cond2
                                        #   Parent Loop BB49_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB49_6 Depth 3
	testq	%rbx, %rbx
	je	.LBB49_10
# BB#4:                                 # %for.body3
                                        #   in Loop: Header=BB49_3 Depth=2
	movq	24(%rbx), %r13
	movq	%rbx, %rdi
	callq	split_insn
	testq	%rax, %rax
	movq	%rbx, %rcx
	jne	.LBB49_6
	jmp	.LBB49_8
	.align	16, 0x90
.LBB49_5:                               # %while.body
                                        #   in Loop: Header=BB49_6 Depth=3
	movq	16(%rax), %rax
.LBB49_6:                               # %while.body
                                        #   Parent Loop BB49_1 Depth=1
                                        #     Parent Loop BB49_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	je	.LBB49_5
# BB#7:                                 # %while.end
                                        #   in Loop: Header=BB49_3 Depth=2
	movq	24(%rsp), %rcx          # 8-byte Reload
	orq	%r15, 16(%rcx,%r12,8)
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rcx
.LBB49_8:                               # %if.end
                                        #   in Loop: Header=BB49_3 Depth=2
	movq	8(%r14), %rax
	cmpq	%rax, %rcx
	movq	%r13, %rbx
	jne	.LBB49_3
# BB#9:                                 # %for.end
                                        #   in Loop: Header=BB49_1 Depth=1
	testq	%rcx, %rcx
	jne	.LBB49_1
.LBB49_10:                              # %if.then17
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.split_all_insns, %rdx
	movq	$2798, %rsi             # imm = 0xAEE
	callq	fancy_abort
.LBB49_11:                              # %for.end20
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	movq	24(%rsp), %rbx          # 8-byte Reload
	je	.LBB49_14
# BB#12:                                # %if.end23
	movq	%rbx, %rdi
	callq	find_many_sub_basic_blocks
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%r14, %rax
	je	.LBB49_14
# BB#13:                                # %if.then26
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	count_or_remove_death_notes
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	update_life_info
.LBB49_14:                              # %if.end28
	movq	%rbx, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp385:
	.size	split_all_insns, .Ltmp385-split_all_insns
	.cfi_endproc

	.align	16, 0x90
	.type	split_insn,@function
split_insn:                             # @split_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp391:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp392:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp393:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp394:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp395:
	.cfi_def_cfa_offset 48
.Ltmp396:
	.cfi_offset %rbx, -40
.Ltmp397:
	.cfi_offset %r12, -32
.Ltmp398:
	.cfi_offset %r14, -24
.Ltmp399:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r14, %r14
	cmpq	$105, %rax
	jne	.LBB50_13
# BB#1:                                 # %cond.true
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB50_3
# BB#2:                                 # %cond.false
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB50_3:                               # %cond.end22
	testq	%rax, %rax
	je	.LBB50_7
# BB#4:                                 # %land.lhs.true
	movq	%rax, %rdi
	callq	set_noop_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB50_7
# BB#5:                                 # %if.then27
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB50_13
# BB#6:                                 # %if.then29
	movq	$37, %rax
	movw	%ax, (%r15)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%r15)
	movq	$0, 32(%r15)
	jmp	.LBB50_13
.LBB50_7:                               # %if.else36
	movq	16(%r15), %rbx
	movq	32(%r15), %rdi
	movq	$1, %rdx
	movq	%r15, %rsi
	callq	try_split
	movq	%rax, %r12
	cmpq	%r15, %r12
	je	.LBB50_13
# BB#8:                                 # %if.then46
	movq	$37, %rax
	movw	%ax, (%r15)
	movq	$0, 32(%r15)
	movabsq	$4294967197, %rax       # imm = 0xFFFFFF9D
	movl	%eax, 40(%r15)
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	jne	.LBB50_9
	jmp	.LBB50_12
.LBB50_11:                              # %if.then71
                                        #   in Loop: Header=BB50_9 Depth=1
	movq	%rbx, %rdi
	callq	cleanup_subreg_operands
.LBB50_9:                               # %if.then46
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r12, %rbx
	je	.LBB50_12
# BB#10:                                # %while.body
                                        #   in Loop: Header=BB50_9 Depth=1
	movq	24(%rbx), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB50_9
	jmp	.LBB50_11
.LBB50_12:
	movq	%r12, %r14
.LBB50_13:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp400:
	.size	split_insn, .Ltmp400-split_insn
	.cfi_endproc

	.globl	split_all_insns_noflow
	.align	16, 0x90
	.type	split_all_insns_noflow,@function
split_all_insns_noflow:                 # @split_all_insns_noflow
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp403:
	.cfi_def_cfa_offset 16
.Ltmp404:
	.cfi_offset %rbx, -16
	callq	get_insns
	movq	%rax, %rdi
	testq	%rdi, %rdi
	je	.LBB51_2
	.align	16, 0x90
.LBB51_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rbx
	callq	split_insn
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB51_1
.LBB51_2:                               # %for.end
	popq	%rbx
	retq
.Ltmp405:
	.size	split_all_insns_noflow, .Ltmp405-split_all_insns_noflow
	.cfi_endproc

	.globl	peep2_next_insn
	.align	16, 0x90
	.type	peep2_next_insn,@function
peep2_next_insn:                        # @peep2_next_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp407:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jge	.LBB52_6
# BB#1:                                 # %if.end
	movl	peep2_current(%rip), %edx
	leaq	(%rdx,%rdi), %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$4, %rsi
	jle	.LBB52_3
# BB#2:
	leaq	-5(%rdx,%rdi), %rax
.LBB52_3:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	peep2_insn_data(%rax), %rcx
	movq	global_rtl(%rip), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	je	.LBB52_5
# BB#4:                                 # %select.mid
	movq	%rcx, %rax
.LBB52_5:                               # %select.end
	popq	%rdx
	retq
.LBB52_6:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_next_insn, %rcx
	movq	$2858, %rsi             # imm = 0xB2A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp408:
	.size	peep2_next_insn, .Ltmp408-peep2_next_insn
	.cfi_endproc

	.globl	peep2_regno_dead_p
	.align	16, 0x90
	.type	peep2_regno_dead_p,@function
peep2_regno_dead_p:                     # @peep2_regno_dead_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp410:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jge	.LBB53_1
# BB#3:                                 # %if.end
	movl	peep2_current(%rip), %r8d
	leaq	(%r8,%rdi), %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jle	.LBB53_5
# BB#4:
	leaq	-5(%r8,%rdi), %rdx
.LBB53_5:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	cmpq	$0, peep2_insn_data(%rax)
	je	.LBB53_6
# BB#7:                                 # %if.end6
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	peep2_insn_data+8(%rdx), %rdi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB53_8
# BB#9:                                 # %if.end6
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB53_8:
	movabsq	$0, %rax
	popq	%rdx
	retq
.LBB53_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_regno_dead_p, %rcx
	movq	$2878, %rsi             # imm = 0xB3E
	jmp	.LBB53_2
.LBB53_6:                               # %if.then5
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_regno_dead_p, %rcx
	movq	$2885, %rsi             # imm = 0xB45
.LBB53_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp411:
	.size	peep2_regno_dead_p, .Ltmp411-peep2_regno_dead_p
	.cfi_endproc

	.globl	peep2_reg_dead_p
	.align	16, 0x90
	.type	peep2_reg_dead_p,@function
peep2_reg_dead_p:                       # @peep2_reg_dead_p
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
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jge	.LBB54_1
# BB#3:                                 # %if.end
	movl	peep2_current(%rip), %eax
	leaq	(%rax,%rdi), %r8
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$4, %rdx
	jle	.LBB54_5
# BB#4:
	leaq	-5(%rax,%rdi), %r8
.LBB54_5:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	$4, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	cmpq	$0, peep2_insn_data(%rax)
	je	.LBB54_6
# BB#7:                                 # %if.end6
	movl	8(%rsi), %r14d
	movzbl	2(%rsi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r14, %rcx
	cmpq	$8, %rcx
	je	.LBB54_11
# BB#8:                                 # %if.end6
	leaq	-21(%r14), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB54_11
# BB#9:                                 # %if.end6
	leaq	-45(%r14), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB54_11
# BB#10:                                # %if.end6
	leaq	-29(%r14), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB54_11
# BB#16:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB54_17
# BB#19:                                # %cond.true41
	movzbl	target_flags+3(%rip), %ebx
	andq	$2, %rbx
	xorq	$6, %rbx
	jmp	.LBB54_21
.LBB54_11:                              # %cond.true
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rbx
	je	.LBB54_21
# BB#12:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB54_13
# BB#14:                                # %lor.rhs
	movabsq	$1, %rbx
	incq	%rbx
	jmp	.LBB54_21
.LBB54_17:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB54_20
# BB#18:                                # %cond.true34
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	xorq	$3, %rbx
	jmp	.LBB54_21
.LBB54_13:
	movabsq	$0, %rbx
	incq	%rbx
	jmp	.LBB54_21
.LBB54_20:                              # %cond.false45
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rbx
.LBB54_21:                              # %while.cond.preheader
	movq	$4, %rcx
	shlq	%cl, %r8
	leaq	peep2_insn_data+8(%r8), %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB54_22:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rax
	jle	.LBB54_24
# BB#23:                                # %while.body
                                        #   in Loop: Header=BB54_22 Depth=1
	movq	(%r15), %rdi
	decq	%rbx
	leaq	(%rbx,%r14), %rsi
	callq	bitmap_bit_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r12, %rcx
	je	.LBB54_22
.LBB54_24:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB54_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_reg_dead_p, %rcx
	movq	$2900, %rsi             # imm = 0xB54
	jmp	.LBB54_2
.LBB54_6:                               # %if.then5
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_reg_dead_p, %rcx
	movq	$2907, %rsi             # imm = 0xB5B
.LBB54_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp426:
	.size	peep2_reg_dead_p, .Ltmp426-peep2_reg_dead_p
	.cfi_endproc

	.globl	peep2_find_free_register
	.align	16, 0x90
	.type	peep2_find_free_register,@function
peep2_find_free_register:               # @peep2_find_free_register
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
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	jg	.LBB55_2
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jge	.LBB55_2
# BB#4:                                 # %if.end
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movl	peep2_current(%rip), %eax
	leaq	(%rax,%rdi), %r12
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$4, %rdx
	jle	.LBB55_6
# BB#5:
	leaq	-5(%rax,%rdi), %r12
.LBB55_6:                               # %if.end
	movq	%r8, 8(%rsp)            # 8-byte Spill
	leaq	(%rax,%rsi), %r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$4, %rdx
	jle	.LBB55_8
# BB#7:
	leaq	-5(%rax,%rsi), %r15
.LBB55_8:                               # %if.end
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	cmpq	$0, peep2_insn_data(%rdx)
	je	.LBB55_9
# BB#10:                                # %do.body
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	peep2_insn_data+8(%rax), %rsi
	movq	$0, 40(%rsp)
	leaq	40(%rsp), %rdi
	callq	reg_set_to_hard_reg_set
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r15
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	%r15, %rax
	je	.LBB55_17
# BB#11:
	leaq	32(%rsp), %r14
	.align	16, 0x90
.LBB55_12:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	cmpq	$3, %rax
	jg	.LBB55_14
# BB#13:                                # %while.body
                                        #   in Loop: Header=BB55_12 Depth=1
	incq	%r12
	movq	%r12, %rbx
.LBB55_14:                              # %while.body
                                        #   in Loop: Header=BB55_12 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	cmpq	$0, peep2_insn_data(%rdx)
	je	.LBB55_15
# BB#16:                                # %do.body25
                                        #   in Loop: Header=BB55_12 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	peep2_insn_data+8(%rax), %rsi
	movq	$0, 32(%rsp)
	movq	%r14, %rdi
	callq	reg_set_to_hard_reg_set
	movq	32(%rsp), %rax
	orq	%rax, 40(%rsp)
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	%r15, %rax
	movq	%rbx, %r12
	jne	.LBB55_12
.LBB55_17:                              # %while.end
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$114, %rax
	movq	$12, %rdx
	movq	8(%rsp), %r8            # 8-byte Reload
	je	.LBB55_59
# BB#18:                                # %cond.false38
	movq	$11, %rdx
	cmpq	$82, %rax
	je	.LBB55_59
# BB#19:                                # %cond.false38
	cmpq	$113, %rax
	jne	.LBB55_21
# BB#20:                                # %cond.true49
	movl	target_flags(%rip), %edx
	andq	$33554432, %rdx         # imm = 0x2000000
	movq	$23, %rcx
	shrq	%cl, %rdx
	orq	$8, %rdx
	jmp	.LBB55_59
.LBB55_21:                              # %cond.false50
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$101, %rdx
	jg	.LBB55_33
# BB#22:                                # %cond.false50
	movq	$8, %rdx
	cmpq	$81, %rax
	je	.LBB55_59
	jmp	.LBB55_23
.LBB55_33:                              # %cond.false50
	cmpq	$117, %rax
	je	.LBB55_44
# BB#34:                                # %cond.false50
	cmpq	$116, %rax
	jne	.LBB55_35
# BB#40:                                # %cond.true72
	movl	target_flags(%rip), %eax
	testq	$1, %rax
	movq	$13, %rdx
	jne	.LBB55_59
# BB#41:                                # %lor.rhs75
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB55_43
# BB#42:                                # %lor.rhs75
	movq	$13, %rax
.LBB55_43:                              # %lor.rhs75
	movq	%rax, %rdx
	jmp	.LBB55_59
.LBB55_44:                              # %cond.true85
	movl	target_flags(%rip), %eax
	testq	$1, %rax
	movq	$14, %rdx
	jne	.LBB55_59
# BB#45:                                # %lor.rhs88
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB55_47
# BB#46:                                # %lor.rhs88
	movq	$14, %rax
.LBB55_47:                              # %lor.rhs88
	movq	%rax, %rdx
	jmp	.LBB55_59
.LBB55_35:                              # %cond.false50
	cmpq	$102, %rax
	jne	.LBB55_23
# BB#36:                                # %cond.true61
	movl	target_flags(%rip), %eax
	testq	$1, %rax
	movq	$15, %rdx
	jne	.LBB55_59
# BB#37:                                # %lor.rhs
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	je	.LBB55_39
# BB#38:                                # %lor.rhs
	movq	$15, %rax
.LBB55_39:                              # %lor.rhs
	movq	%rax, %rdx
	jmp	.LBB55_59
.LBB55_23:                              # %cond.false93
	movq	$1, %rdx
	cmpq	$97, %rax
	je	.LBB55_59
# BB#24:                                # %cond.false99
	movq	$4, %rdx
	cmpq	$98, %rax
	je	.LBB55_59
# BB#25:                                # %cond.false105
	movq	$3, %rdx
	cmpq	$99, %rax
	je	.LBB55_59
# BB#26:                                # %cond.false111
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB55_48
# BB#27:                                # %cond.false111
	cmpq	$89, %rax
	jne	.LBB55_28
# BB#54:                                # %cond.true132
	movl	target_flags(%rip), %edx
	andq	$262144, %rdx           # imm = 0x40000
	movq	$14, %rcx
	shrq	%cl, %rdx
	jmp	.LBB55_59
.LBB55_48:                              # %cond.false111
	cmpq	$121, %rax
	jne	.LBB55_49
# BB#55:                                # %cond.true142
	movzbl	target_flags+1(%rip), %edx
	andq	$64, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB55_59
# BB#56:                                # %cond.true142
	movq	$17, %rdx
	jmp	.LBB55_59
.LBB55_28:                              # %cond.false111
	movq	$2, %rdx
	cmpq	$100, %rax
	je	.LBB55_59
	jmp	.LBB55_29
.LBB55_49:                              # %cond.false111
	cmpq	$120, %rax
	jne	.LBB55_29
# BB#50:                                # %cond.true122
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	je	.LBB55_51
# BB#52:                                # %cond.true122
	movabsq	$1, %rdx
	jmp	.LBB55_53
.LBB55_29:                              # %cond.false147
	movq	$7, %rdx
	cmpq	$65, %rax
	je	.LBB55_59
# BB#30:                                # %cond.false153
	movq	$6, %rdx
	cmpq	$68, %rax
	je	.LBB55_59
# BB#31:                                # %cond.false159
	cmpq	$83, %rax
	jne	.LBB55_32
# BB#57:                                # %cond.false159
	movabsq	$1, %rax
	jmp	.LBB55_58
.LBB55_51:
	movabsq	$0, %rdx
.LBB55_53:                              # %cond.true122
	movq	$4, %rcx
	shlq	%cl, %rdx
	jmp	.LBB55_59
.LBB55_32:
	movabsq	$0, %rax
.LBB55_58:                              # %cond.false159
	leaq	(%rax,%rax,4), %rdx
.LBB55_59:                              # %cond.end196
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB55_60:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB55_84 Depth 2
	movl	peep2_find_free_register.search_ofs(%rip), %eax
	leaq	(%rax,%r14), %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$52, %rdx
	jle	.LBB55_62
# BB#61:                                #   in Loop: Header=BB55_60 Depth=1
	leaq	-53(%rax,%r14), %rbx
.LBB55_62:                              # %for.body
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movslq	reg_alloc_order(,%rax,4), %rdi
	movzbl	fixed_regs(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB55_86
# BB#63:                                # %if.end212
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	%rdi, %r12
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rax
	andq	%r13, %r12
	movq	$1, %rdx
	movq	%r12, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB55_86
# BB#64:                                # %if.end218
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r13
	movq	%rdi, %r15
	callq	ix86_hard_regno_mode_ok
	movq	%r15, %rsi
	movq	%rbx, %r9
	movq	%r13, %r8
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB55_86
# BB#65:                                # %if.end221
                                        #   in Loop: Header=BB55_60 Depth=1
	movzbl	call_used_regs(%rsi), %eax
	testq	%rax, %rax
	jne	.LBB55_67
# BB#66:                                # %land.lhs.true
                                        #   in Loop: Header=BB55_60 Depth=1
	movzbl	regs_ever_live(%rsi), %eax
	testq	%rax, %rax
	je	.LBB55_86
.LBB55_67:                              # %if.end229
                                        #   in Loop: Header=BB55_60 Depth=1
	cmpq	$6, %r12
	je	.LBB55_69
# BB#68:                                # %if.end229
                                        #   in Loop: Header=BB55_60 Depth=1
	cmpq	$20, %r12
	jne	.LBB55_71
.LBB55_69:                              # %land.lhs.true235
                                        #   in Loop: Header=BB55_60 Depth=1
	movl	reload_completed(%rip), %eax
	testq	%rax, %rax
	je	.LBB55_86
# BB#70:                                # %land.lhs.true235
                                        #   in Loop: Header=BB55_60 Depth=1
	movl	frame_pointer_needed(%rip), %eax
	testq	%r13, %rax
	jne	.LBB55_86
.LBB55_71:                              # %if.end240
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	%rsi, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB55_73
# BB#72:                                # %if.end240
                                        #   in Loop: Header=BB55_60 Depth=1
	movabsq	$1, %rax
.LBB55_73:                              # %if.end240
                                        #   in Loop: Header=BB55_60 Depth=1
	leaq	-21(%rsi), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%rsi), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%rsi), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	sbbq	%r10, %r10
	andq	$1, %r10
	orq	%rax, %r10
	je	.LBB55_78
# BB#74:                                # %cond.true264
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB55_83
# BB#75:                                # %lor.rhs269
                                        #   in Loop: Header=BB55_60 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB55_77
# BB#76:                                # %lor.rhs269
                                        #   in Loop: Header=BB55_60 Depth=1
	movabsq	$1, %rax
.LBB55_77:                              # %lor.rhs269
                                        #   in Loop: Header=BB55_60 Depth=1
	incq	%rax
	jmp	.LBB55_83
.LBB55_78:                              # %cond.false276
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	$24, %rax
	jne	.LBB55_79
# BB#81:                                # %cond.true286
                                        #   in Loop: Header=BB55_60 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB55_83
.LBB55_79:                              # %cond.false276
                                        #   in Loop: Header=BB55_60 Depth=1
	cmpq	$18, %rax
	jne	.LBB55_82
# BB#80:                                # %cond.true279
                                        #   in Loop: Header=BB55_60 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB55_83
.LBB55_82:                              # %cond.false290
                                        #   in Loop: Header=BB55_60 Depth=1
	movl	target_flags(%rip), %edi
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB55_83:                              # %for.cond309.preheader
                                        #   in Loop: Header=BB55_60 Depth=1
	movq	40(%rsp), %rdx
	.align	16, 0x90
.LBB55_84:                              # %for.cond309
                                        #   Parent Loop BB55_60 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB55_88
# BB#85:                                # %for.body312
                                        #   in Loop: Header=BB55_84 Depth=2
	decq	%rax
	movq	(%r8), %rdi
	leaq	(%rax,%rsi), %rcx
	movq	$1, %rbx
	shlq	%cl, %rbx
	orq	%rdx, %rdi
	testq	%rdi, %rbx
	je	.LBB55_84
	.align	16, 0x90
.LBB55_86:                              # %for.inc415
                                        #   in Loop: Header=BB55_60 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB55_60
# BB#87:                                # %for.end417
	xorq	%rax, %rax
	movl	%eax, peep2_find_free_register.search_ofs(%rip)
	jmp	.LBB55_105
.LBB55_88:                              # %if.then327.critedge
	testq	$1, %r10
	je	.LBB55_94
# BB#89:                                # %cond.true351
	movq	(%rsp), %rax            # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB55_99
# BB#90:                                # %lor.rhs356
	cmpq	$6, %rcx
	jne	.LBB55_91
# BB#92:                                # %lor.rhs356
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB55_99
.LBB55_94:                              # %cond.false363
	movq	16(%rsp), %rax          # 8-byte Reload
	andq	%rax, %r13
	cmpq	$24, %r13
	jne	.LBB55_95
# BB#97:                                # %cond.true373
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB55_99
.LBB55_95:                              # %cond.false363
	cmpq	$18, %r13
	jne	.LBB55_98
# BB#96:                                # %cond.true366
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB55_99
.LBB55_91:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB55_99
.LBB55_98:                              # %for.cond397.preheader
	movl	target_flags(%rip), %edi
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB55_102
.LBB55_99:                              # %for.body400.lr.ph
	movq	(%r8), %rdi
	.align	16, 0x90
.LBB55_100:                             # %for.body400
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r10
	leaq	-1(%rax,%rsi), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rdi
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	orq	%r10, %rdi
	testq	%rbx, %rbx
	jg	.LBB55_100
# BB#101:                               # %for.cond397.for.end407_crit_edge
	movq	%rdi, (%r8)
.LBB55_102:                             # %for.end407
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	$51, %rdx
	jg	.LBB55_104
# BB#103:                               # %for.end407
	incq	%r9
	movq	%r9, %rax
.LBB55_104:                             # %for.end407
	movl	%eax, peep2_find_free_register.search_ofs(%rip)
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	gen_rtx_REG
.LBB55_105:                             # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB55_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_find_free_register, %rcx
	movq	$2941, %rsi             # imm = 0xB7D
	jmp	.LBB55_3
.LBB55_9:                               # %if.then11
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_find_free_register, %rcx
	movq	$2951, %rsi             # imm = 0xB87
	jmp	.LBB55_3
.LBB55_15:                              # %if.then23
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.peep2_find_free_register, %rcx
	movq	$2961, %rsi             # imm = 0xB91
.LBB55_3:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp444:
	.size	peep2_find_free_register, .Ltmp444-peep2_find_free_register
	.cfi_endproc

	.globl	peephole2_optimize
	.align	16, 0x90
	.type	peephole2_optimize,@function
peephole2_optimize:                     # @peephole2_optimize
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp451:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp452:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp453:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp454:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp455:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp456:
	.cfi_def_cfa_offset 288
.Ltmp457:
	.cfi_offset %rbx, -48
.Ltmp458:
	.cfi_offset %r12, -40
.Ltmp459:
	.cfi_offset %r13, -32
.Ltmp460:
	.cfi_offset %r14, -24
.Ltmp461:
	.cfi_offset %r15, -16
	leaq	96(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, peep2_insn_data+8(%rip)
	leaq	120(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, peep2_insn_data+24(%rip)
	leaq	144(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, peep2_insn_data+40(%rip)
	leaq	168(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, peep2_insn_data+56(%rip)
	leaq	192(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, peep2_insn_data+72(%rip)
	leaq	216(%rsp), %rdi
	callq	bitmap_initialize
	movq	%rax, %r15
	movq	%r15, 72(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	$1, %rsi
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	callq	count_or_remove_death_notes
	movslq	n_basic_blocks(%rip), %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jle	.LBB56_65
# BB#1:                                 # %for.body9.lr.ph
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	.align	16, 0x90
.LBB56_2:                               # %for.body9
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB56_3 Depth 2
                                        #       Child Loop BB56_13 Depth 3
                                        #       Child Loop BB56_19 Depth 3
                                        #       Child Loop BB56_23 Depth 3
                                        #       Child Loop BB56_28 Depth 3
                                        #       Child Loop BB56_34 Depth 3
                                        #       Child Loop BB56_39 Depth 3
                                        #       Child Loop BB56_53 Depth 3
                                        #       Child Loop BB56_59 Depth 3
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdx,8), %rbx
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	$0, peep2_insn_data(%rip)
	movq	$0, peep2_insn_data+16(%rip)
	movq	global_rtl(%rip), %rax
	movq	$0, peep2_insn_data+32(%rip)
	movq	$0, peep2_insn_data+48(%rip)
	movq	%rax, peep2_insn_data+64(%rip)
	movq	$4, %rax
	movl	%eax, peep2_current(%rip)
	movq	72(%rbx), %rsi
	decq	%rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%r15, %rdi
	callq	bitmap_copy
	movq	peep2_insn_data+72(%rip), %rdi
	movq	%r15, %rsi
	callq	bitmap_copy
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	init_propagate_block_info
	movq	8(%rbx), %rbx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB56_3:                               # %for.cond23
                                        #   Parent Loop BB56_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB56_13 Depth 3
                                        #       Child Loop BB56_19 Depth 3
                                        #       Child Loop BB56_23 Depth 3
                                        #       Child Loop BB56_28 Depth 3
                                        #       Child Loop BB56_34 Depth 3
                                        #       Child Loop BB56_39 Depth 3
                                        #       Child Loop BB56_53 Depth 3
                                        #       Child Loop BB56_59 Depth 3
	movq	%rbx, %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	movq	16(%r12), %rbx
	cmpq	$105, %rax
	jne	.LBB56_63
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB56_3 Depth=2
	movslq	peep2_current(%rip), %rcx
	testq	%rcx, %rcx
	movq	$4, %rax
	jle	.LBB56_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB56_3 Depth=2
	decq	%rcx
	movq	%rcx, %rax
.LBB56_6:                               # %if.then
                                        #   in Loop: Header=BB56_3 Depth=2
	movl	%eax, peep2_current(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r12, peep2_insn_data(%rax)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	propagate_one_insn
	movslq	peep2_current(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	peep2_insn_data+8(%rax), %rdi
	movq	%r15, %rsi
	callq	bitmap_copy
	movq	32(%r12), %rdi
	movq	%r12, %rsi
	leaq	92(%rsp), %rdx
	callq	peephole2_insns
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB56_63
# BB#7:                                 # %for.cond46.preheader
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	%r12, %r13
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movslq	92(%rsp), %rax
	movl	peep2_current(%rip), %edx
	xorq	%r12, %r12
	testq	%rax, %rax
	js	.LBB56_8
	.align	16, 0x90
.LBB56_13:                              # %for.body49
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	(%rdx,%r12), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$4, %rdi
	jle	.LBB56_15
# BB#14:                                #   in Loop: Header=BB56_13 Depth=3
	leaq	-5(%rdx,%r12), %rsi
.LBB56_15:                              # %for.body49
                                        #   in Loop: Header=BB56_13 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	peep2_insn_data(%rsi), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$34, %rcx
	je	.LBB56_16
# BB#12:                                # %for.cond46
                                        #   in Loop: Header=BB56_13 Depth=3
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rax, %rsi
	jl	.LBB56_13
	jmp	.LBB56_8
.LBB56_16:                              # %if.end64
                                        #   in Loop: Header=BB56_3 Depth=2
	movzbl	(%r14), %ecx
	cmpq	$34, %rcx
	movq	%r14, %r15
	je	.LBB56_21
# BB#17:                                # %if.end64
                                        #   in Loop: Header=BB56_3 Depth=2
	cmpq	$24, %rcx
	jne	.LBB56_82
# BB#18:                                # %if.then69
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	8(%r14), %r8
	movslq	(%r8), %rdi
	.align	16, 0x90
.LBB56_19:                              # %for.cond73
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	jle	.LBB56_82
# BB#20:                                # %for.body76
                                        #   in Loop: Header=BB56_19 Depth=3
	movq	(%r8,%rdi,8), %r15
	movzbl	(%r15), %ecx
	decq	%rdi
	cmpq	$34, %rcx
	jne	.LBB56_19
.LBB56_21:                              # %if.end98
                                        #   in Loop: Header=BB56_3 Depth=2
	testq	%r15, %r15
	je	.LBB56_82
# BB#22:                                # %if.end100
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	64(%rsi), %rcx
	movq	%rcx, 64(%r15)
	movq	56(%rsi), %rbx
	testq	%rbx, %rbx
	je	.LBB56_28
	.align	16, 0x90
.LBB56_23:                              # %for.body112
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	2(%rbx), %ecx
	cmpq	$29, %rcx
	ja	.LBB56_26
# BB#24:                                # %for.body112
                                        #   in Loop: Header=BB56_23 Depth=3
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$872415232, %rax        # imm = 0x34000000
	je	.LBB56_26
# BB#25:                                # %sw.bb
                                        #   in Loop: Header=BB56_23 Depth=3
	movq	56(%r15), %rax
	movq	8(%rbx), %rdx
	movq	$3, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r15)
.LBB56_26:                              # %for.inc128
                                        #   in Loop: Header=BB56_23 Depth=3
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB56_23
# BB#27:                                # %for.cond110.while.cond.preheader_crit_edge
                                        #   in Loop: Header=BB56_3 Depth=2
	movl	92(%rsp), %eax
	movl	peep2_current(%rip), %edx
	.align	16, 0x90
.LBB56_28:                              # %while.cond
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jge	.LBB56_8
# BB#29:                                # %while.body
                                        #   in Loop: Header=BB56_28 Depth=3
	incq	%r12
	leaq	(%rdx,%r12), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$4, %rdi
	jle	.LBB56_31
# BB#30:                                #   in Loop: Header=BB56_28 Depth=3
	leaq	-5(%rdx,%r12), %rsi
.LBB56_31:                              # %while.body
                                        #   in Loop: Header=BB56_28 Depth=3
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	peep2_insn_data(%rsi), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$34, %rcx
	jne	.LBB56_28
	jmp	.LBB56_32
	.align	16, 0x90
.LBB56_8:                               # %for.end153
                                        #   in Loop: Header=BB56_3 Depth=2
	leaq	(%rdx,%rax), %r12
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$4, %rsi
	jle	.LBB56_10
# BB#9:                                 #   in Loop: Header=BB56_3 Depth=2
	leaq	-5(%rdx,%rax), %r12
.LBB56_10:                              # %for.end153
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	peep2_insn_data(%rbx), %rdi
	movq	$23, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	movq	peep2_insn_data(%rbx), %rsi
	movq	%rax, %r15
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	emit_insn_after
	movq	%r13, %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	16(%rdi), %r14
	movq	peep2_insn_data(%rbx), %rsi
	movq	%rax, %r13
	callq	delete_insn_chain
	testq	%r15, %r15
	jne	.LBB56_33
# BB#11:                                #   in Loop: Header=BB56_3 Depth=2
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB56_50
.LBB56_33:                              # %if.then175
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	40(%rbx), %rax
	xorq	%r12, %r12
	jmp	.LBB56_34
	.align	16, 0x90
.LBB56_36:                              # %for.cond176
                                        #   in Loop: Header=BB56_34 Depth=3
	movq	8(%rax), %rax
.LBB56_34:                              # %if.then175
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	je	.LBB56_38
# BB#35:                                # %for.body178
                                        #   in Loop: Header=BB56_34 Depth=3
	movzbl	48(%rax), %ecx
	testq	$8, %rcx
	je	.LBB56_36
# BB#37:                                #   in Loop: Header=BB56_3 Depth=2
	movq	%rax, %r12
.LBB56_38:                              # %for.cond184.preheader
                                        #   in Loop: Header=BB56_3 Depth=2
	cmpq	%r14, %r13
	movq	%r13, %r15
	je	.LBB56_49
	.align	16, 0x90
.LBB56_39:                              # %for.body187
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	je	.LBB56_43
# BB#40:                                # %lor.lhs.false
                                        #   in Loop: Header=BB56_39 Depth=3
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB56_48
# BB#41:                                # %land.lhs.true
                                        #   in Loop: Header=BB56_39 Depth=3
	movq	32(%r15), %rdi
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB56_48
# BB#42:                                # %land.lhs.true198
                                        #   in Loop: Header=BB56_39 Depth=3
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB56_48
	.align	16, 0x90
.LBB56_43:                              # %if.then201
                                        #   in Loop: Header=BB56_39 Depth=3
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	movq	56(%r15), %rcx
	movq	$3, %rdi
	movq	$23, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%r15)
	movq	8(%rbx), %rax
	cmpq	%rax, %r15
	je	.LBB56_48
# BB#44:                                # %if.then201
                                        #   in Loop: Header=BB56_39 Depth=3
	testq	%r12, %r12
	je	.LBB56_48
# BB#45:                                # %if.then217
                                        #   in Loop: Header=BB56_39 Depth=3
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	split_block
	movzbl	(%r15), %ecx
	movq	%rax, %rbx
	cmpq	$34, %rcx
	movabsq	$0, %rax
	jne	.LBB56_47
# BB#46:                                # %if.then217
                                        #   in Loop: Header=BB56_39 Depth=3
	movabsq	$1, %rax
.LBB56_47:                              # %if.then217
                                        #   in Loop: Header=BB56_39 Depth=3
	movq	16(%rbx), %rdi
	movq	24(%r12), %rsi
	leaq	10(,%rax,4), %rdx
	callq	make_edge
	movl	52(%r12), %ecx
	movq	%rax, %r12
	movl	%ecx, 52(%r12)
	movq	$10000, %rax            # imm = 0x2710
	subq	%rcx, %rax
	movl	%eax, 52(%rbx)
	movq	24(%rbx), %rdi
	callq	purge_dead_edges
	movq	16(%rbx), %rbx
	movq	64(%rsp), %rcx          # 8-byte Reload
	andq	$1, %rcx
	orq	%rax, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
.LBB56_48:                              # %for.inc241
                                        #   in Loop: Header=BB56_39 Depth=3
	movq	16(%r15), %r15
	cmpq	%r14, %r15
	jne	.LBB56_39
.LBB56_49:                              # %for.end245
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	purge_dead_edges
	movq	64(%rsp), %rcx          # 8-byte Reload
	andq	$1, %rcx
	orq	%rax, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB56_50:                              # %if.end253
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r15, %r15
	cmpq	$3, %rax
	jg	.LBB56_52
# BB#51:                                # %if.end253
                                        #   in Loop: Header=BB56_3 Depth=2
	incq	%r12
	movq	%r12, %r15
.LBB56_52:                              # %if.end253
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	peep2_insn_data+8(%rax), %rsi
	callq	bitmap_copy
	movq	%r13, 24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB56_53:                              # %do.body
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB56_57
# BB#54:                                # %if.then269
                                        #   in Loop: Header=BB56_53 Depth=3
	movq	%rbx, %r14
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$4, %r12
	jle	.LBB56_56
# BB#55:                                # %if.then269
                                        #   in Loop: Header=BB56_53 Depth=3
	decq	%r15
	movq	%r15, %r12
.LBB56_56:                              # %if.then269
                                        #   in Loop: Header=BB56_53 Depth=3
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	%r13, peep2_insn_data(%rbx)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	propagate_one_insn
	movq	peep2_insn_data+8(%rbx), %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	bitmap_copy
	movq	%r12, %r15
	movq	%r14, %rbx
.LBB56_57:                              # %if.end282
                                        #   in Loop: Header=BB56_53 Depth=3
	movq	16(%r13), %r13
	cmpq	%rbx, %r13
	jne	.LBB56_53
# BB#58:                                # %do.end
                                        #   in Loop: Header=BB56_3 Depth=2
	movl	%r15d, peep2_current(%rip)
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	%rcx, %rdx
	movq	32(%rsp), %r12          # 8-byte Reload
	je	.LBB56_62
	.align	16, 0x90
.LBB56_59:                              # %for.body291
                                        #   Parent Loop BB56_2 Depth=1
                                        #     Parent Loop BB56_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rdx), %eax
	cmpq	$33, %rax
	je	.LBB56_60
# BB#61:                                # %for.inc298
                                        #   in Loop: Header=BB56_59 Depth=3
	movq	16(%rdx), %rdx
	cmpq	%rcx, %rdx
	jne	.LBB56_59
	jmp	.LBB56_62
.LBB56_60:                              #   in Loop: Header=BB56_3 Depth=2
	movq	$1, 8(%rsp)             # 8-byte Folded Spill
.LBB56_62:                              #   in Loop: Header=BB56_3 Depth=2
	movq	72(%rsp), %r15          # 8-byte Reload
.LBB56_63:                              # %if.end304
                                        #   in Loop: Header=BB56_3 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	cmpq	%rax, %r12
	jne	.LBB56_3
# BB#64:                                # %for.end310
                                        #   in Loop: Header=BB56_2 Depth=1
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	free_propagate_block_info
	movq	$32, %rcx
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB56_2
.LBB56_65:                              # %for.cond314.preheader
	movq	peep2_insn_data+8(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB56_67
# BB#66:                                # %if.then323
	callq	bitmap_clear
	movq	$0, peep2_insn_data+8(%rip)
.LBB56_67:                              # %for.inc333
	movq	peep2_insn_data+24(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB56_69
# BB#68:                                # %if.then323.1
	callq	bitmap_clear
	movq	$0, peep2_insn_data+24(%rip)
.LBB56_69:                              # %for.inc333.1
	movq	peep2_insn_data+40(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB56_71
# BB#70:                                # %if.then323.2
	callq	bitmap_clear
	movq	$0, peep2_insn_data+40(%rip)
.LBB56_71:                              # %for.inc333.2
	movq	peep2_insn_data+56(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB56_73
# BB#72:                                # %if.then323.3
	callq	bitmap_clear
	movq	$0, peep2_insn_data+56(%rip)
.LBB56_73:                              # %for.inc333.3
	movq	peep2_insn_data+72(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB56_75
# BB#74:                                # %if.then323.4
	callq	bitmap_clear
	movq	$0, peep2_insn_data+72(%rip)
.LBB56_75:                              # %for.inc333.4
	testq	%r15, %r15
	je	.LBB56_77
# BB#76:                                # %if.then338
	movq	%r15, %rdi
	callq	bitmap_clear
.LBB56_77:                              # %do.end341
	testq	$1, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB56_79
# BB#78:                                # %if.then343
	callq	get_insns
	movq	%rax, %rdi
	callq	rebuild_jump_labels
.LBB56_79:                              # %if.end345
	testq	$1, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB56_81
# BB#80:                                # %if.then347
	xorq	%rdi, %rdi
	callq	cleanup_cfg
	xorq	%rdi, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	callq	update_life_info
.LBB56_81:                              # %if.end349
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB56_32:                              # %if.then149
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.peephole2_optimize, %rdx
	movq	$3160, %rsi             # imm = 0xC58
	callq	fancy_abort
.LBB56_82:                              # %if.then99
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.peephole2_optimize, %rdx
	movq	$3130, %rsi             # imm = 0xC3A
	callq	fancy_abort
.Ltmp462:
	.size	peephole2_optimize, .Ltmp462-peephole2_optimize
	.cfi_endproc

	.type	volatile_ok,@object     # @volatile_ok
	.comm	volatile_ok,4,4
	.type	reload_completed,@object # @reload_completed
	.comm	reload_completed,4,4
	.type	which_alternative,@object # @which_alternative
	.comm	which_alternative,4,4
	.type	num_changes,@object     # @num_changes
	.local	num_changes
	.comm	num_changes,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"recog.c"
	.size	.L.str, 8

	.type	.L__FUNCTION__.validate_change,@object # @__FUNCTION__.validate_change
.L__FUNCTION__.validate_change:
	.asciz	"validate_change"
	.size	.L__FUNCTION__.validate_change, 16

	.type	changes_allocated,@object # @changes_allocated
	.local	changes_allocated
	.comm	changes_allocated,4,4
	.type	changes,@object         # @changes
	.local	changes
	.comm	changes,8,8
	.type	.L__FUNCTION__.asm_operand_ok,@object # @__FUNCTION__.asm_operand_ok
.L__FUNCTION__.asm_operand_ok:
	.asciz	"asm_operand_ok"
	.size	.L__FUNCTION__.asm_operand_ok, 15

	.type	recog_data,@object      # @recog_data
	.comm	recog_data,1008,8
	.type	.L__FUNCTION__.extract_constrain_insn_cached,@object # @__FUNCTION__.extract_constrain_insn_cached
.L__FUNCTION__.extract_constrain_insn_cached:
	.asciz	"extract_constrain_insn_cached"
	.size	.L__FUNCTION__.extract_constrain_insn_cached, 30

	.type	.L__FUNCTION__.extract_insn,@object # @__FUNCTION__.extract_insn
.L__FUNCTION__.extract_insn:
	.asciz	"extract_insn"
	.size	.L__FUNCTION__.extract_insn, 13

	.type	recog_op_alt,@object    # @recog_op_alt
	.comm	recog_op_alt,28800,16
	.type	.L__FUNCTION__.split_all_insns,@object # @__FUNCTION__.split_all_insns
.L__FUNCTION__.split_all_insns:
	.asciz	"split_all_insns"
	.size	.L__FUNCTION__.split_all_insns, 16

	.type	.L__FUNCTION__.peep2_next_insn,@object # @__FUNCTION__.peep2_next_insn
.L__FUNCTION__.peep2_next_insn:
	.asciz	"peep2_next_insn"
	.size	.L__FUNCTION__.peep2_next_insn, 16

	.type	peep2_current,@object   # @peep2_current
	.local	peep2_current
	.comm	peep2_current,4,4
	.type	peep2_insn_data,@object # @peep2_insn_data
	.local	peep2_insn_data
	.comm	peep2_insn_data,80,16
	.type	.L__FUNCTION__.peep2_regno_dead_p,@object # @__FUNCTION__.peep2_regno_dead_p
.L__FUNCTION__.peep2_regno_dead_p:
	.asciz	"peep2_regno_dead_p"
	.size	.L__FUNCTION__.peep2_regno_dead_p, 19

	.type	.L__FUNCTION__.peep2_reg_dead_p,@object # @__FUNCTION__.peep2_reg_dead_p
.L__FUNCTION__.peep2_reg_dead_p:
	.asciz	"peep2_reg_dead_p"
	.size	.L__FUNCTION__.peep2_reg_dead_p, 17

	.type	peep2_find_free_register.search_ofs,@object # @peep2_find_free_register.search_ofs
	.local	peep2_find_free_register.search_ofs
	.comm	peep2_find_free_register.search_ofs,4,4
	.type	.L__FUNCTION__.peep2_find_free_register,@object # @__FUNCTION__.peep2_find_free_register
.L__FUNCTION__.peep2_find_free_register:
	.asciz	"peep2_find_free_register"
	.size	.L__FUNCTION__.peep2_find_free_register, 25

	.type	.L__FUNCTION__.peephole2_optimize,@object # @__FUNCTION__.peephole2_optimize
.L__FUNCTION__.peephole2_optimize:
	.asciz	"peephole2_optimize"
	.size	.L__FUNCTION__.peephole2_optimize, 19


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
